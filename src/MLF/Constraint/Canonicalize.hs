{- |
Module      : MLF.Constraint.Canonicalize
Description : Shared pure canonicalization helpers

Several phases maintain a union-find over `NodeId`s and then need to rewrite
edges (and sometimes other structures) through a canonicalization function.
This module centralizes the purely-mechanical rewriting helpers so phases can
share the same behavior without coupling higher-level responsibilities
(grafting, presolution trace handling, etc.).
-}
module MLF.Constraint.Canonicalize (
    chooseRepNode,
    rewriteInstEdges,
    rewriteUnifyEdges,
    rewriteBindParentsLenient
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types (NodeId(..), TyNode(..), InstEdge(..), UnifyEdge(..), BindParents, BindFlag)

-- | Prefer a structured node over a variable when collapsing duplicates.
--
-- When both candidates are vars or both are structured, keep the old value to
-- preserve deterministic left-to-right behavior when used with
-- 'IntMap.fromListWith'.
chooseRepNode :: TyNode -> TyNode -> TyNode
chooseRepNode new old = case (isVar old, isVar new) of
    (True, False) -> new
    (False, True) -> old
    _ -> old
  where
    isVar TyVar{} = True
    isVar _ = False

rewriteInstEdges :: (NodeId -> NodeId) -> [InstEdge] -> [InstEdge]
rewriteInstEdges canonical = map rewrite
  where
    rewrite (InstEdge eid l r) = InstEdge eid (canonical l) (canonical r)

rewriteUnifyEdges :: (NodeId -> NodeId) -> [UnifyEdge] -> [UnifyEdge]
rewriteUnifyEdges canonical = map rewrite
  where
    rewrite (UnifyEdge l r) = UnifyEdge (canonical l) (canonical r)

-- | Rewrite binding parents under a canonicalization function, dropping self
-- edges and merging duplicates by keeping the first parent and taking the max
-- flag.
--
-- This is intentionally lenient: it does not reject conflicting parents on the
-- same canonical child id, because some phases historically resolve such
-- conflicts deterministically.
rewriteBindParentsLenient
    :: (NodeId -> NodeId)
    -> (NodeId -> Bool)
    -> BindParents
    -> BindParents
rewriteBindParentsLenient canonical keepChild bindParents0 =
    IntMap.fromListWith combine
        [ (getNodeId childC, (parentC, flag))
        | (childId, (parent0, flag)) <- IntMap.toList bindParents0
        , let childC = canonical (NodeId childId)
        , let parentC = canonical parent0
        , childC /= parentC
        , keepChild childC
        ]
  where
    combine :: (NodeId, BindFlag) -> (NodeId, BindFlag) -> (NodeId, BindFlag)
    combine (_pNew, fNew) (pOld, fOld) = (pOld, max fNew fOld)
