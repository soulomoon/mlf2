{- |
Module      : MLF.Constraint.Presolution.Plan.BinderPlan.Predicate
Description : Binder predicate helpers
Copyright   : (c) 2024
License     : BSD-3-Clause
-}
module MLF.Constraint.Presolution.Plan.BinderPlan.Predicate (
    isTargetSchemeBinderFor
) where

import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore

-- | Check if a variable is a target scheme binder.
isTargetSchemeBinderFor
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Bool
    -> NodeId
    -> Bool
isTargetSchemeBinderFor canonical constraint target0 targetIsBaseLike v
    | targetIsBaseLike = False
    | canonical v == canonical target0 = True
    | otherwise =
        case VarStore.lookupVarBound constraint (canonical v) of
            Just bnd -> canonical bnd == canonical target0
            Nothing -> False
