{- |
Module      : MLF.Constraint.VarStore
Description : Variable bound and elimination state management
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides operations for managing type variable state in the
constraint graph, including:

* Instance bounds - the upper bound on a type variable's instantiation
* Elimination tracking - marking variables that have been unified away

= Instance Bounds

In MLF, type variables can have /instance bounds/ that constrain their
instantiation. A variable @α ⩾ τ@ can only be instantiated to types that
are instances of @τ@. Missing bounds are treated as @⊥@ (bottom), meaning
the variable is unconstrained.

= Variable Elimination

During unification, variables may be eliminated (unified with another type).
Eliminated variables are tracked to avoid re-processing them during
generalization and reification.
-}
module MLF.Constraint.VarStore (
    lookupVarBound,
    setVarBound,
    isEliminatedVar,
    markEliminatedVar,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import qualified MLF.Constraint.NodeAccess as NodeAccess

-- | Look up the instance bound of a variable.
--
-- Missing entries are treated as ⊥ (`Nothing`).
lookupVarBound :: Constraint -> NodeId -> Maybe NodeId
lookupVarBound = NodeAccess.lookupVarBound

-- | Set (or clear) the instance bound of a variable.
--
-- Storing @Nothing@ deletes the key so missing entries still represent ⊥.
setVarBound :: NodeId -> Maybe NodeId -> Constraint -> Constraint
setVarBound v mb c =
    let nodes0 = cNodes c
    in case lookupNodeIn nodes0 v of
        Just tv@TyVar{} ->
            c { cNodes = IntMap.insert (getNodeId v) tv{ tnBound = mb } nodes0 }
        _ -> c

-- | True iff the variable has been eliminated by presolution/ω execution.
isEliminatedVar :: Constraint -> NodeId -> Bool
isEliminatedVar c v =
    IntSet.member (getNodeId v) (cEliminatedVars c)

-- | Mark a variable as eliminated.
markEliminatedVar :: NodeId -> Constraint -> Constraint
markEliminatedVar v c =
    c { cEliminatedVars = IntSet.insert (getNodeId v) (cEliminatedVars c) }
