{- |
Module      : MLF.Constraint.Normalize.Graft
Description : Grafting transformations for normalization
Copyright   : (c) 2024
License     : BSD-3-Clause
-}
module MLF.Constraint.Normalize.Graft (
    graftInstEdges
) where

import Control.Monad.State.Strict (gets, modify')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Constraint.Normalize.Internal
    ( NormalizeM
    , NormalizeState (..)
    , findRoot
    , freshVar
    , insertNode
    , setBindParentNorm
    , setBindParentRefNorm
    )
import MLF.Constraint.Types.Graph
    ( BindFlag (..)
    , Constraint (..)
    , InstEdge (..)
    , NodeId (..)
    , GenNode (..)
    , NodeMap
    , TyNode (..)
    , UnifyEdge (..)
    , lookupNodeIn
    , nodeRefKey
    , typeRef
    )

{- Note [Grafting]
~~~~~~~~~~~~~~~~~~
Grafting is one of the core semantic-preserving transformations described in
Rémy & Yakobowski (ICFP 2008) §4, under "Local transformations."

Paper terminology: "We say that τ is grafted onto α in the constraint
C[α ≤ τ] when C[α = τ] is formed by replacing α ≤ τ with α = τ."

More precisely (from the paper, §4 p.8):
  "When the type τ is rigid, i.e. when it is known from the constraint
   that τ begins with a type constructor κ, then the variable α must
   also begin with κ. This constraint can be propagated as follows:
   we graft a copy of τ's structure onto α."

The key insight is that an instantiation constraint α ≤ τ where τ has
constructor structure (arrow, base type) forces α to have that same structure.
Rather than keeping this as an instantiation edge, we "graft" the structure
onto α by creating a copy with fresh variables, then unifying.

Example:   α ≤ (Int → β)

Step-by-step grafting (paper §4):
  1. Create fresh variable nodes α₁, α₂ at α's binding level
  2. Create a fresh arrow node (α₁ → α₂)
  3. Emit unification edge: α = (α₁ → α₂)
  4. Emit unification edges: α₁ = Int, α₂ = β
  5. Remove the original instantiation edge

Constraint transformation:

    Before:  α ≤ (Int → β)               -- instantiation edge

    After:   α = (α₁ → α₂)               -- α must be an arrow
             α₁ = Int                    -- domain matches Int
             α₂ = β                      -- codomain matches β

After merging (union-find) resolves these unification edges:

    α  ↦  (Int → β)                      -- α is now (Int → β)

Why fresh variables? We create α₁, α₂ rather than directly unifying α with
(Int → β) because:
  1. The right-hand side nodes may be shared with other constraints
  2. Fresh nodes at α's level preserve correct scoping for generalization
  3. It keeps the graph structure clean (α points to its own arrow node)

The paper notes that grafting must respect binding levels: fresh nodes are
created at the same level as α to preserve the scoping discipline.

See also:
  - Note [Grafting Cases] for the complete case analysis
  - Yakobowski PhD thesis §4.4 for extended discussion of grafting
  - TLDI'07 paper §3.2 for the underlying graphical unification
-}

-- | Process instantiation edges by grafting structure onto variables.
graftInstEdges :: NormalizeM ()
graftInstEdges = do
    c <- gets nsConstraint
    let edges = cInstEdges c
        nodes = cNodes c

    -- Partition edges into those we can graft and those we keep
    (toGraft, toKeep) <- partitionGraftable edges nodes

    -- Process graftable edges, collecting unify edges and type errors
    results <- mapM graftEdge toGraft
    let (errors, successes) = partitionResults (zip toGraft results)
        newUnifyEdges = concat successes

    -- Update constraint: keep non-graftable edges + error edges
    modify' $ \s ->
        let c' = nsConstraint s
        in s
            { nsConstraint =
                c'
                    { cInstEdges = toKeep ++ errors
                    , cUnifyEdges = cUnifyEdges c' ++ newUnifyEdges
                    }
            }
  where
    partitionResults :: [(InstEdge, Maybe [UnifyEdge])] -> ([InstEdge], [[UnifyEdge]])
    partitionResults = foldr go ([], [])
      where
        go (edge, Nothing) (errs, succs) = (edge : errs, succs)
        go (_, Just unifyEdges) (errs, succs) = (errs, unifyEdges : succs)

-- | Partition edges into (graftable, non-graftable).
-- An edge is graftable if we can structurally transform it without needing
-- presolution decisions. See Note [Grafting Cases].
partitionGraftable :: [InstEdge] -> NodeMap TyNode -> NormalizeM ([InstEdge], [InstEdge])
partitionGraftable edges nodes = do
    polySyms <- gets (cPolySyms . nsConstraint)
    uf <- gets nsUnionFind
    let isGraftable edge =
            let leftId = findRoot uf (instLeft edge)
                rightId = findRoot uf (instRight edge)
                leftNode = lookupNodeIn nodes leftId
                rightNode = lookupNodeIn nodes rightId
            in case (leftNode, rightNode) of
                (Just TyVar {}, Just TyArrow {}) -> True
                (Just TyVar {}, Just TyBase { tnBase = base }) ->
                    not (Set.member base polySyms)
                (Just TyVar {}, Just TyCon { tnCon = con }) ->
                    not (Set.member con polySyms)
                (Just TyArrow {}, Just TyArrow {}) -> True
                (Just TyBase {}, Just TyBase {}) -> True
                (Just TyCon {}, Just TyCon {}) -> True
                (Just TyBottom {}, Just TyBottom {}) -> True
                (Just TyArrow {}, Just TyBase {}) -> True
                (Just TyBase {}, Just TyArrow {}) -> True
                (Just TyArrow {}, Just TyBottom {}) -> True
                (Just TyBottom {}, Just TyArrow {}) -> True
                (Just TyBase {}, Just TyBottom {}) -> True
                (Just TyBottom {}, Just TyBase {}) -> True
                (Just TyArrow {}, Just TyCon {}) -> True
                (Just TyCon {}, Just TyArrow {}) -> True
                (Just TyBase {}, Just TyCon {}) -> True
                (Just TyCon {}, Just TyBase {}) -> True
                (Just TyCon {}, Just TyBottom {}) -> True
                (Just TyBottom {}, Just TyCon {}) -> True
                _ -> False
    pure (filter isGraftable edges, filter (not . isGraftable) edges)

{- Note [Grafting Cases]
~~~~~~~~~~~~~~~~~~~~~~~~
Grafting transforms instantiation edges (≤) into unification edges (=) by
copying constructor structure. This is part of the normalization phase
(ICFP'08 §4) that puts constraints into "locally-solved form."

Paper reference (§4, p.8): "The type τ is rigid, i.e. [...] begins with
a type constructor κ, then the variable α must also begin with κ."

Cases handled during normalization (Phase 2):
──────────────────────────────────────────────
  Var ≤ Arrow    → Graft: rewrite α into (α₁ → α₂), then unify α₁/α₂ with
                   the arrow's components (core grafting operation).

  Var ≤ Base     → Graft if the base is rigid; keep for presolution only when
                   the base symbol is polymorphic (paper Poly set).

  Arrow ≤ Arrow  → Decompose: emit (dom₁ = dom₂) and (cod₁ = cod₂)
                   (Standard unification of constructors with same head.)

  Base ≤ Base    → If same, satisfied; if different, type error
                   (Nullary constructors unify only with themselves.)

  Arrow ≤ Base   → Type error (incompatible constructors)
  Base ≤ Arrow   → Type error (incompatible constructors)
                   (Paper §5: "type error detection" during constraint
                    solving identifies these clashes.)

Cases requiring Phase 4 (presolution):
──────────────────────────────────────
The paper (§4.3) discusses these cases under "expansion variables":

  Var ≤ Forall   → The ∀ introduces a polymorphic type. We must decide
                   whether to instantiate it (choose s := inst) or keep
                   the polymorphism (s := ∀). This is the "presolution"
                   phase described in §4.3 "Computing the presolution."

  Var ≤ Exp      → Expansion nodes (s · τ) are placeholders for
                   "expansion variables" (§4.1). Their meaning is
                   determined by presolution. We can't graft until we
                   know what expansion s represents.

  Forall ≤ *     → A polymorphic type on the left must be instantiated.
                   The presolution decides how deeply to instantiate.
                   (Paper: "each use may require a different expansion".)

  Exp ≤ *        → Expansion on the left: presolution must resolve s.
                   (The expansion variable s is constrained by both
                    the left and right context.)

  Var ≤ Var      → Neither side has structure yet. The instantiation
                   relationship is kept for Phase 4, which builds the
                   "instance graph" (§4.3) to compute minimal expansions.

These unresolved edges are kept as InstEdges for Phase 4 to process
after building the dependency graph and computing minimal expansions.

Implementation note: Type errors are kept as InstEdges rather than
raising an exception, allowing us to collect multiple errors or defer
error reporting to a later phase with better diagnostics.
-}

-- | Graft a single edge, returning new unification edges.
-- Returns Nothing if the edge represents a type error that should be kept.
graftEdge :: InstEdge -> NormalizeM (Maybe [UnifyEdge])
graftEdge edge = do
    c <- gets nsConstraint
    uf <- gets nsUnionFind
    let nodes = cNodes c
        schemeRoots =
            IntSet.fromList
                [ getNodeId root
                | gen <- NodeAccess.allGenNodes c
                , root <- gnSchemes gen
                ]
        leftId = findRoot uf (instLeft edge)
        rightId = findRoot uf (instRight edge)
        leftNode = lookupNodeIn nodes leftId
        rightNode = lookupNodeIn nodes rightId

    case (leftNode, rightNode) of
        (Just TyVar { tnBound = mbBound }, Just (TyArrow { tnDom = rDom, tnCod = rCod }))
            | occursIn nodes uf leftId rightId -> pure Nothing
            | otherwise -> do
                domVar <- freshVar
                codVar <- freshVar
                insertNode TyArrow { tnId = leftId, tnDom = domVar, tnCod = codVar }
                setBindParentNorm domVar leftId BindFlex
                setBindParentNorm codVar leftId BindFlex
                case mbBound of
                    Nothing -> pure ()
                    Just bnd -> do
                        let bp = cBindParents c
                        case IntMap.lookup (nodeRefKey (typeRef leftId)) bp of
                            Nothing -> pure ()
                            Just (parentRef, flag) ->
                                setBindParentRefNorm (typeRef bnd) parentRef flag
                let boundEdges =
                        case mbBound of
                            Just bnd
                                | bnd /= leftId
                                , not (IntSet.member (getNodeId leftId) schemeRoots) ->
                                    [UnifyEdge bnd leftId]
                            _ -> []
                pure $
                    Just
                        ( boundEdges
                            ++ [ UnifyEdge domVar (findRoot uf rDom)
                               , UnifyEdge codVar (findRoot uf rCod)
                               ]
                        )

        (Just TyVar { tnBound = mbBound }, Just TyBase {}) ->
            let boundEdges =
                    case mbBound of
                        Just bnd
                            | bnd /= rightId
                            , not (IntSet.member (getNodeId leftId) schemeRoots) ->
                                [UnifyEdge bnd rightId]
                        _ -> []
            in pure $ Just (UnifyEdge leftId rightId : boundEdges)

        (Just TyVar { tnBound = mbBound }, Just TyBottom {}) ->
            let boundEdges =
                    case mbBound of
                        Just bnd
                            | bnd /= rightId
                            , not (IntSet.member (getNodeId leftId) schemeRoots) ->
                                [UnifyEdge bnd rightId]
                        _ -> []
            in pure $ Just (UnifyEdge leftId rightId : boundEdges)

        (Just TyVar { tnBound = mbBound }, Just (TyCon { tnCon = con, tnArgs = rArgs }))
            | occursIn nodes uf leftId rightId -> pure Nothing
            | otherwise -> do
                argVars <- mapM (const freshVar) (NE.toList rArgs)
                let argVarsNE = NE.fromList argVars
                insertNode TyCon { tnId = leftId, tnCon = con, tnArgs = argVarsNE }
                mapM_ (\v -> setBindParentNorm v leftId BindFlex) argVars
                case mbBound of
                    Nothing -> pure ()
                    Just bnd -> do
                        let bp = cBindParents c
                        case IntMap.lookup (nodeRefKey (typeRef leftId)) bp of
                            Nothing -> pure ()
                            Just (parentRef, flag) ->
                                setBindParentRefNorm (typeRef bnd) parentRef flag
                let boundEdges =
                        case mbBound of
                            Just bnd
                                | bnd /= leftId
                                , not (IntSet.member (getNodeId leftId) schemeRoots) ->
                                    [UnifyEdge bnd leftId]
                            _ -> []
                    argUnifyEdges =
                        zipWith (\v r -> UnifyEdge v (findRoot uf r)) argVars (NE.toList rArgs)
                pure $ Just (boundEdges ++ argUnifyEdges)

        (Just (TyArrow { tnDom = lDom, tnCod = lCod }), Just (TyArrow { tnDom = rDom, tnCod = rCod })) ->
            pure $
                Just
                    [ UnifyEdge (findRoot uf lDom) (findRoot uf rDom)
                    , UnifyEdge (findRoot uf lCod) (findRoot uf rCod)
                    ]

        (Just (TyBase { tnBase = lBase }), Just (TyBase { tnBase = rBase }))
            | lBase == rBase -> pure $ Just []
            | otherwise -> pure Nothing

        (Just (TyCon { tnCon = lCon, tnArgs = lArgs }), Just (TyCon { tnCon = rCon, tnArgs = rArgs }))
            | lCon == rCon
            , NE.length lArgs == NE.length rArgs ->
                let argEdges =
                        zipWith
                            (\l r -> UnifyEdge (findRoot uf l) (findRoot uf r))
                            (NE.toList lArgs)
                            (NE.toList rArgs)
                in pure $ Just argEdges
            | otherwise -> pure Nothing

        (Just TyBottom {}, Just TyBottom {}) ->
            pure $ Just []

        (Just TyArrow {}, Just TyBase {}) -> pure Nothing
        (Just TyBase {}, Just TyArrow {}) -> pure Nothing
        (Just TyArrow {}, Just TyBottom {}) -> pure Nothing
        (Just TyBottom {}, Just TyArrow {}) -> pure Nothing
        (Just TyBase {}, Just TyBottom {}) -> pure Nothing
        (Just TyBottom {}, Just TyBase {}) -> pure Nothing
        (Just TyArrow {}, Just TyCon {}) -> pure Nothing
        (Just TyCon {}, Just TyArrow {}) -> pure Nothing
        (Just TyBase {}, Just TyCon {}) -> pure Nothing
        (Just TyCon {}, Just TyBase {}) -> pure Nothing
        (Just TyCon {}, Just TyBottom {}) -> pure Nothing
        (Just TyBottom {}, Just TyCon {}) -> pure Nothing

        _ -> pure $ Just []

-- | Check whether target (under UF reps) contains the variable.
occursIn :: NodeMap TyNode -> IntMap NodeId -> NodeId -> NodeId -> Bool
occursIn nodes uf var target =
    case Traversal.occursInUnder (findRoot uf) (lookupNodeIn nodes) var target of
        Left _ -> False
        Right ok -> ok
