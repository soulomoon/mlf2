module MLF.Binding.NodeRefs (
    allNodeRefs,
    nodeRefExists,
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types

allNodeRefs :: Constraint -> [NodeRef]
allNodeRefs c =
    map (TypeRef . fst) (toListNode (cNodes c))
        ++ map (GenRef . GenNodeId) (IntMap.keys (getGenNodeMap (cGenNodes c)))

nodeRefExists :: Constraint -> NodeRef -> Bool
nodeRefExists c ref = case ref of
    TypeRef nid ->
        case lookupNodeIn (cNodes c) nid of
            Just _ -> True
            Nothing -> False
    GenRef gid ->
        IntMap.member (getGenNodeId gid) (getGenNodeMap (cGenNodes c))
