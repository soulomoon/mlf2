module MLF.Binding.Children (
    collectBoundChildren,
    collectBoundChildrenWithFlag,
    collectBoundChildrenEntriesWithFlag,
) where

import Control.Monad (foldM)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Types.Graph

collectBoundChildren
    :: (NodeRef -> Maybe NodeId)
    -> Constraint p
    -> BindParents
    -> NodeRef
    -> String
    -> Either BindingError [NodeId]
collectBoundChildren childFilter =
    collectBoundChildrenWithFlag childFilter (== BindFlex)

collectBoundChildrenWithFlag
    :: (NodeRef -> Maybe NodeId)
    -> (BindFlag -> Bool)
    -> Constraint p
    -> BindParents
    -> NodeRef
    -> String
    -> Either BindingError [NodeId]
collectBoundChildrenWithFlag childFilter flagOk c bindParents binder errCtx =
    collectBoundChildrenEntriesWithFlag
        childFilter
        flagOk
        c
        [ entry
        | entry@(_childKey, (parent, _flag)) <- IntMap.toList bindParents
        , parent == binder
        ]
        errCtx

collectBoundChildrenEntriesWithFlag
    :: (NodeRef -> Maybe NodeId)
    -> (BindFlag -> Bool)
    -> Constraint p
    -> [(Int, (NodeRef, BindFlag))]
    -> String
    -> Either BindingError [NodeId]
collectBoundChildrenEntriesWithFlag childFilter flagOk c entries errCtx =
    reverse <$> foldM step [] entries
  where
    step acc (childKey, (_parent, flag))
        | not (flagOk flag) = pure acc
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
