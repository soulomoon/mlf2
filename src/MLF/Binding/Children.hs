module MLF.Binding.Children (
    collectBoundChildren,
    collectBoundChildrenWithFlag,
) where

import Control.Monad (foldM)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Types

collectBoundChildren
    :: (NodeRef -> Maybe NodeId)
    -> Constraint
    -> BindParents
    -> NodeRef
    -> String
    -> Either BindingError [NodeId]
collectBoundChildren childFilter =
    collectBoundChildrenWithFlag childFilter (== BindFlex)

collectBoundChildrenWithFlag
    :: (NodeRef -> Maybe NodeId)
    -> (BindFlag -> Bool)
    -> Constraint
    -> BindParents
    -> NodeRef
    -> String
    -> Either BindingError [NodeId]
collectBoundChildrenWithFlag childFilter flagOk c bindParents binder errCtx =
    reverse <$> foldM step [] (IntMap.toList bindParents)
  where
    step acc (childKey, (parent, flag))
        | parent /= binder || not (flagOk flag) = pure acc
        | otherwise =
            case nodeRefFromKey childKey of
                TypeRef childN ->
                    case NodeAccess.lookupNode c childN of
                        Nothing ->
                            Left $
                                InvalidBindingTree $
                                    errCtx ++ ": child " ++ show childN ++ " not in cNodes"
                        Just _ ->
                            maybe (pure acc) (\nid -> pure (nid : acc)) (childFilter (TypeRef childN))
                GenRef gid ->
                    if IntMap.member (getGenNodeId gid) (getGenNodeMap (cGenNodes c))
                        then pure acc
                        else
                            Left $
                                InvalidBindingTree $
                                    errCtx ++ ": child " ++ show gid ++ " not in cGenNodes"
