{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.Materialization
Description : Expansion materialization for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module handles the materialization of expansions during presolution.
After the edge processing loop decides minimal expansions for each expansion
variable, this module applies those expansions to TyExp nodes and records
their replacements.

= Paper References

* Rémy & Yakobowski, "Graphic Type Constraints" (ICFP 2008) - §5 "Presolution"
-}
module MLF.Constraint.Presolution.Materialization (
    materializeExpansions,
    frWith
) where

import Control.Monad (forM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Util.UnionFind as UnionFind
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical, findSchemeIntroducerM)
import MLF.Constraint.Presolution.Expansion (
    applyExpansion,
    getExpansion
    )

-- | Apply final expansions to all TyExp nodes and record their replacements.
materializeExpansions :: PresolutionM (IntMap NodeId)
materializeExpansions = do
    (c0, canonical) <- getConstraintAndCanonical
    let exps = [ n | (_, n@TyExp{}) <- toListNode (cNodes c0) ]
    fmap IntMap.fromList $ forM exps $ \expNode -> do
        let eid = tnId expNode
        expn <- getExpansion (tnExpVar expNode)
        (c0', canonical') <- getConstraintAndCanonical
        gid <- findSchemeIntroducerM canonical' c0' (tnBody expNode)
        nid' <- case expn of
            -- Identity expansions are erased by rewriting the wrapper to its body.
            ExpIdentity -> applyExpansion gid expn expNode
            -- For non-identity expansions, `processInstEdge` should already have
            -- materialized and unified the `TyExp` with its expansion result. Reuse
            -- that representative to avoid duplicating fresh nodes here.
            _ ->
                let root = canonical eid
                in if root /= eid
                    then pure root
                    else applyExpansion gid expn expNode
        pure (getNodeId eid, nid')

-- | Read-only chase like Solve.frWith
frWith :: IntMap NodeId -> NodeId -> NodeId
frWith = UnionFind.frWith
