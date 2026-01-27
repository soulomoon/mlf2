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
