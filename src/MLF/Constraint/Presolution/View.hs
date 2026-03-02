module MLF.Constraint.Presolution.View (
    PresolutionView(..),
    fromPresolutionResult,
    fromSolved
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Solve (rewriteConstraintWithUF)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph
    ( BindFlag
    , BindParents
    , Constraint(..)
    , NodeId(..)
    , NodeRef
    , TyNode(..)
    )
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))

-- | Read-only presolution queries built directly from presolution snapshot data.
data PresolutionView = PresolutionView
    { pvConstraint :: Constraint
    , pvCanonicalMap :: IntMap NodeId
    , pvCanonical :: NodeId -> NodeId
    , pvLookupNode :: NodeId -> Maybe TyNode
    , pvLookupVarBound :: NodeId -> Maybe NodeId
    , pvLookupBindParent :: NodeRef -> Maybe (NodeRef, BindFlag)
    , pvBindParents :: BindParents
    , pvCanonicalConstraint :: Constraint
    }

fromPresolutionResult :: PresolutionSnapshot a => a -> PresolutionView
fromPresolutionResult pres =
    let constraint = snapshotConstraint pres
        uf = sanitizeSnapshotUf constraint (snapshotUnionFind pres)
        canonMap = buildCanonicalMap uf constraint
        canonical = equivCanonical canonMap
    in PresolutionView
        { pvConstraint = constraint
        , pvCanonicalMap = canonMap
        , pvCanonical = canonical
        , pvLookupNode = \nid -> NodeAccess.lookupNode constraint (canonical nid)
        , pvLookupVarBound = \nid -> NodeAccess.lookupVarBound constraint (canonical nid)
        , pvLookupBindParent = NodeAccess.lookupBindParent constraint
        , pvBindParents = cBindParents constraint
        , pvCanonicalConstraint = rewriteConstraintWithUF uf constraint
        }

fromSolved :: Solved.Solved -> PresolutionView
fromSolved solved =
    PresolutionView
        { pvConstraint = Solved.originalConstraint solved
        , pvCanonicalMap = Solved.canonicalMap solved
        , pvCanonical = Solved.canonical solved
        , pvLookupNode = Solved.lookupNode solved
        , pvLookupVarBound = Solved.lookupVarBound solved
        , pvLookupBindParent = Solved.lookupBindParent solved
        , pvBindParents = Solved.bindParents solved
        , pvCanonicalConstraint = Solved.canonicalConstraint solved
        }

buildCanonicalMap :: IntMap NodeId -> Constraint -> IntMap NodeId
buildCanonicalMap uf c =
    let nodeKeys = map (nodeIdKey . tnId) (NodeAccess.allNodes c)
        allKeys = IntSet.toList (IntSet.fromList (nodeKeys ++ IntMap.keys uf))
        canonicalNode = chaseUfCanonical uf
    in IntMap.fromList
        [ (k, rep)
        | k <- allKeys
        , let rep = canonicalNode (NodeId k)
        , rep /= NodeId k
        ]

sanitizeSnapshotUf :: Constraint -> IntMap NodeId -> IntMap NodeId
sanitizeSnapshotUf c =
    IntMap.mapMaybeWithKey keepLive
  where
    isLive nid = case NodeAccess.lookupNode c nid of
        Just _ -> True
        Nothing -> False
    keepLive k rep =
        let keyNode = NodeId k
        in if isLive keyNode && isLive rep && keyNode /= rep
            then Just rep
            else Nothing

chaseUfCanonical :: IntMap NodeId -> NodeId -> NodeId
chaseUfCanonical uf = go IntSet.empty
  where
    step nid = IntMap.findWithDefault nid (nodeIdKey nid) uf

    go seen current =
        let next = step current
        in if next == current
            then current
            else if IntSet.member (nodeIdKey next) seen
                then cycleRepresentative next
                else go (IntSet.insert (nodeIdKey current) seen) next

    cycleRepresentative cycleStart = goCycle cycleStart cycleStart
      where
        goCycle minNode current =
            let next = step current
                minNode' = minByNodeId minNode next
            in if next == cycleStart
                then minNode'
                else goCycle minNode' next

    minByNodeId a b =
        if nodeIdKey a <= nodeIdKey b
            then a
            else b

equivCanonical :: IntMap NodeId -> NodeId -> NodeId
equivCanonical canonMap = go IntSet.empty
  where
    step nid = IntMap.findWithDefault nid (nodeIdKey nid) canonMap

    go seen current =
        let next = step current
        in if next == current
            then current
            else if IntSet.member (nodeIdKey next) seen
                then cycleRepresentative next
                else go (IntSet.insert (nodeIdKey current) seen) next

    cycleRepresentative cycleStart = goCycle cycleStart cycleStart
      where
        goCycle minNode current =
            let next = step current
                minNode' = minByNodeId minNode next
            in if next == cycleStart
                then minNode'
                else goCycle minNode' next

    minByNodeId a b =
        if nodeIdKey a <= nodeIdKey b
            then a
            else b

nodeIdKey :: NodeId -> Int
nodeIdKey (NodeId k) = k
