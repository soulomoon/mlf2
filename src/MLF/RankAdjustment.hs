{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.RankAdjustment
Description : Rank adjustment ("raising") for TyVar/TyVar unification

This module centralizes the graph-side effect of the paper's raising operation:
before merging two variables, we adjust their binding levels to their lowest
common ancestor in the `GNode` tree and move any `gBinds` entries accordingly.

Paper anchors:
  * ICFP'08 (§1.2): "raising and adjusting ranks"
  * `papers/xmlf.txt` (§3.4): `Raise(n)` as a binding-edge raising operation
-}
module MLF.RankAdjustment (
    harmonizeVarLevels,
    harmonizeVarLevelsWithCounts
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Types (
    Constraint (..),
    GNode (..),
    GNodeId (..),
    NodeId (..),
    TyNode (..)
 )

-- | Apply rank adjustment to two nodes inside a constraint.
--
-- If both nodes are variables and a binding tree exists, their `tnVarLevel`s are
-- adjusted to their lowest common ancestor (LCA) and any `gBinds` entries are
-- moved to the LCA level.
harmonizeVarLevels :: NodeId -> NodeId -> Constraint -> Constraint
harmonizeVarLevels a b c = fst (harmonizeVarLevelsWithCounts a b c)

-- | Like 'harmonizeVarLevels', but also returns how many parent-steps each side
-- was raised by (i.e. the number of paper-style `Raise` steps).
harmonizeVarLevelsWithCounts :: NodeId -> NodeId -> Constraint -> (Constraint, (Int, Int))
harmonizeVarLevelsWithCounts a b c0 =
    let nodes = cNodes c0
        gnodes = cGNodes c0
    in if IntMap.null gnodes
        then (c0, (0, 0))
        else case (IntMap.lookup (getNodeId a) nodes, IntMap.lookup (getNodeId b) nodes) of
            (Just TyVar{ tnVarLevel = la }, Just TyVar{ tnVarLevel = lb }) ->
                case lowestCommonAncestorLevel gnodes la lb of
                    Nothing -> (c0, (0, 0))
                    Just lca ->
                        let (c1, ka) = raiseVarTo a lca c0
                            (c2, kb) = raiseVarTo b lca c1
                        in (c2, (ka, kb))
            _ -> (c0, (0, 0))

raiseVarTo :: NodeId -> GNodeId -> Constraint -> (Constraint, Int)
raiseVarTo nid targetLvl c0 = go 0 c0
  where
    go k c =
        case IntMap.lookup (getNodeId nid) (cNodes c) of
            Just TyVar{ tnVarLevel = lvl }
                | lvl == targetLvl -> (c, k)
                | otherwise ->
                    let (c', raised) = raiseVarOnce nid c
                    in if raised
                        then go (k + 1) c'
                        else (c, k)
            _ -> (c, k)

raiseVarOnce :: NodeId -> Constraint -> (Constraint, Bool)
raiseVarOnce nid c0 =
    let nodes = cNodes c0
        gnodes = cGNodes c0
    in case IntMap.lookup (getNodeId nid) nodes of
        Just TyVar{ tnVarLevel = oldLvl } ->
            case IntMap.lookup (getGNodeId oldLvl) gnodes >>= gParent of
                Nothing -> (c0, False)
                Just parentLvl ->
                    if IntMap.notMember (getGNodeId parentLvl) gnodes
                        then (c0, False)
                        else
                            let c1 = moveVarBindIfPresent nid oldLvl parentLvl c0
                                c2 = setVarLevel nid parentLvl c1
                            in (c2, True)
        _ -> (c0, False)

setVarLevel :: NodeId -> GNodeId -> Constraint -> Constraint
setVarLevel nid newLvl c =
    c
        { cNodes =
            IntMap.adjust
                (\case
                    TyVar{} -> TyVar nid newLvl
                    other -> other
                )
                (getNodeId nid)
                (cNodes c)
        }

moveVarBindIfPresent :: NodeId -> GNodeId -> GNodeId -> Constraint -> Constraint
moveVarBindIfPresent nid oldLvl newLvl c
    | oldLvl == newLvl = c
    | otherwise =
        let gnodes = cGNodes c
            oldKey = getGNodeId oldLvl
            newKey = getGNodeId newLvl
        in case (IntMap.lookup oldKey gnodes, IntMap.lookup newKey gnodes) of
            (Just gOld, Just gNew) ->
                let (mbBound, bindsOld') = extractBind nid (gBinds gOld)
                in case mbBound of
                    Nothing -> c
                    Just bound ->
                        let gnodes1 = IntMap.insert oldKey (gOld { gBinds = bindsOld' }) gnodes
                            gnodes2 =
                                IntMap.insert
                                    newKey
                                    ( gNew
                                        { gBinds =
                                            if any (\(v, _) -> v == nid) (gBinds gNew)
                                                then gBinds gNew
                                                else (nid, bound) : gBinds gNew
                                        }
                                    )
                                    gnodes1
                        in c { cGNodes = gnodes2 }
            _ -> c
  where
    extractBind :: NodeId -> [(NodeId, Maybe NodeId)] -> (Maybe (Maybe NodeId), [(NodeId, Maybe NodeId)])
    extractBind target = go
      where
        go [] = (Nothing, [])
        go ((v, bnd) : rest)
            | v == target = (Just bnd, rest)
            | otherwise =
                let (found, rest') = go rest
                in (found, (v, bnd) : rest')

lowestCommonAncestorLevel :: IntMap GNode -> GNodeId -> GNodeId -> Maybe GNodeId
lowestCommonAncestorLevel gnodes a b =
    let aset = IntSet.fromList (map getGNodeId (ancestors a))
    in pick (ancestors b) aset
  where
    pick [] _ = Nothing
    pick (x : xs) aset
        | IntSet.member (getGNodeId x) aset = Just x
        | otherwise = pick xs aset

    ancestors :: GNodeId -> [GNodeId]
    ancestors lvl =
        lvl : case IntMap.lookup (getGNodeId lvl) gnodes >>= gParent of
            Nothing -> []
            Just p -> ancestors p
