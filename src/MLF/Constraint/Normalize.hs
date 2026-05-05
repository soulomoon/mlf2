{-# LANGUAGE DataKinds #-}
{- |
Module      : MLF.Constraint.Normalize
Description : Phase 2 - Local constraint transformations
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the normalization phase of MLF type inference,
which applies semantics-preserving local transformations to put constraints
into "locally-solved form."

Primary references:
  * Rémy & Yakobowski, "Graphic Type Constraints and Efficient Type
    Inference: from ML to MLF" (ICFP 2008) - §4 "Solving constraints locally"
  * Rémy & Yakobowski, "A graphical representation of MLF types with a
    linear-time incremental unification algorithm" (TLDI 2007) - §3

Related work (for later phases):
  * Rémy & Yakobowski, "A Church-Style Intermediate Language for MLF"
    (FLOPS 2010) - describes xMLF elaboration (Phase 6)
  * Yakobowski, PhD thesis (2008) - comprehensive treatment
-}
module MLF.Constraint.Normalize (
    normalize,
    NormalizeState (..),
    -- * Individual transformation rules (exported for testing)
    dropReflexiveInstEdges,
    dropReflexiveUnifyEdges,
    graftInstEdges,
    mergeUnifyEdges
) where

import Control.Monad (when)
import Control.Monad.State.Strict (execState, gets, modify')
import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Normalize.Graft (graftInstEdges)
import MLF.Constraint.Normalize.Internal (NormalizeM, NormalizeState (..))
import MLF.Constraint.Normalize.Merge
    ( applyUnionFindToConstraint
    , enforcePaperShapedInstEdges
    , mergeUnifyEdges
    )
import MLF.Constraint.Types.Graph
    ( Constraint (..)
    , InstEdge (..)
    , UnifyEdge (..)
    , maxNodeIdKeyOr0
    , toNormalizedConstraint
    )
import MLF.Constraint.Types.Phase (Phase(Raw, Normalized))
import MLF.Constraint.Types.SynthesizedExpVar (initSynthExpVarSupply)

{- Note [Normalization / Local Transformations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Phase 2 applies semantics-preserving rewrite rules to the constraint graph
until a fixed point is reached.

Paper reference: Rémy & Yakobowski, "Graphic Type Constraints and Efficient
Type Inference: from ML to MLF" (ICFP 2008), §4 "Solving constraints locally".

The paper describes several "local transformations" (§4, p.8):

  1. Drop reflexive edges: T ≤ T and T = T contribute no information.
     (Implicit in the paper, stated explicitly here.)

  2. Grafting: when an instantiation edge has a variable on the left and
     structure on the right, copy that structure onto the variable.
     (§4, "grafting" - copies the structure of τ onto α when α ≤ τ.)

  3. Merging: process unification edges by merging nodes via union-find.
     (§4, implicit in unification handling; explicit in TLDI'07 paper.)

  4. Collapse identity expansions: when s · τ must match τ exactly.
     (§4, "collapsing identity expansions" - requires presolution.)

  5. Binding-tree hygiene: the paper’s “push/pull binding nodes” is implicit
     in our representation (binding edges live outside the term DAG). We only
     maintain a valid binding tree when nodes are merged (paper Raise(n)).

We iterate these rules until no changes occur. The paper calls this
"putting constraints in locally-solved form" which is a prerequisite
for the global solving phases (acyclicity checking and presolution).

See also:
  - Yakobowski's PhD thesis (2008), Chapter 4 for extended treatment
  - TLDI'07 paper for the graphical unification algorithm
-}

-- | Apply all normalization rules until a fixed point is reached.
normalize :: Constraint 'Raw -> Constraint 'Normalized
normalize c = toNormalizedConstraint (nsConstraint finalState)
  where
    initialState = NormalizeState { nsNextNodeId = maxNodeIdKeyOr0 c + 1
        , nsSynthExpVarSupply = initSynthExpVarSupply c
        , nsUnionFind = IntMap.empty
        , nsConstraint = c
        }
    finalState = execState (normalizeLoop >> enforcePaperShapedInstEdges) initialState

-- | Main normalization loop: apply transformations until fixed point.
normalizeLoop :: NormalizeM ()
normalizeLoop = do
    before <- gets nsConstraint
    modify' $ \s -> s { nsConstraint = dropReflexiveInstEdges (nsConstraint s) }
    modify' $ \s -> s { nsConstraint = dropReflexiveUnifyEdges (nsConstraint s) }
    graftInstEdges
    mergeUnifyEdges
    applyUnionFindToConstraint
    after <- gets nsConstraint
    when (before /= after) normalizeLoop

-- | Remove instantiation edges where left == right (T ≤ T).
dropReflexiveInstEdges :: Constraint p -> Constraint p
dropReflexiveInstEdges c = c { cInstEdges = filter (not . isReflexive) (cInstEdges c) }
  where
    isReflexive edge = instLeft edge == instRight edge

-- | Remove unification edges where left == right (T = T).
dropReflexiveUnifyEdges :: Constraint p -> Constraint p
dropReflexiveUnifyEdges c = c { cUnifyEdges = filter (not . isReflexive) (cUnifyEdges c) }
  where
    isReflexive edge = uniLeft edge == uniRight edge
