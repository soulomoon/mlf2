module MLF.VarStore (
    lookupVarBound,
    setVarBound,
    isEliminatedVar,
    markEliminatedVar,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Types

-- | Look up the instance bound of a variable.
--
-- Missing entries are treated as ⊥ (`Nothing`).
lookupVarBound :: Constraint -> NodeId -> Maybe NodeId
lookupVarBound c v =
    case IntMap.lookup (getNodeId v) (cVarBounds c) of
        Nothing -> Nothing
        Just mb -> mb

-- | Set (or clear) the instance bound of a variable.
--
-- Storing @Nothing@ deletes the key so missing entries still represent ⊥.
setVarBound :: NodeId -> Maybe NodeId -> Constraint -> Constraint
setVarBound v mb c =
    let vb0 = cVarBounds c
        vb1 = case mb of
            Nothing -> IntMap.delete (getNodeId v) vb0
            Just _ -> IntMap.insert (getNodeId v) mb vb0
    in c { cVarBounds = vb1 }

-- | True iff the variable has been eliminated by presolution/ω execution.
isEliminatedVar :: Constraint -> NodeId -> Bool
isEliminatedVar c v =
    IntSet.member (getNodeId v) (cEliminatedVars c)

-- | Mark a variable as eliminated.
markEliminatedVar :: NodeId -> Constraint -> Constraint
markEliminatedVar v c =
    c { cEliminatedVars = IntSet.insert (getNodeId v) (cEliminatedVars c) }

