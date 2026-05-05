module MLF.Binding.NodeRefs (
    allNodeRefs,
    nodeRefExists,
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution

allNodeRefs :: Constraint p -> [NodeRef]
allNodeRefs c =
    map (TypeRef . fst) (toListNode (cNodes c))
        ++ map (GenRef . GenNodeId) (IntMap.keys (getGenNodeMap (cGenNodes c)))

nodeRefExists :: Constraint p -> NodeRef -> Bool
nodeRefExists c ref = case ref of
    TypeRef nid ->
        case lookupNodeIn (cNodes c) nid of
            Just _ -> True
            Nothing -> False
    GenRef gid ->
        IntMap.member (getGenNodeId gid) (getGenNodeMap (cGenNodes c))
