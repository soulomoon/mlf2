module MLF.Frontend.ConstraintGen.Scope (
    pushScope,
    popScope,
    peekScope,
    registerScopeNode,
    rebindScopeNodes
) where

import Control.Monad.State.Strict (get, gets, modify', put)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import MLF.Util.Graph (reachableFromStop)
import MLF.Frontend.ConstraintGen.State (BuildState(..), ConstraintM, ScopeFrame(..))

pushScope :: ConstraintM ()
pushScope =
    modify' $ \st -> st { bsScopes = ScopeFrame IntSet.empty : bsScopes st }

popScope :: ConstraintM ScopeFrame
popScope = do
    st <- get
    case bsScopes st of
        [] -> error "popScope: empty scope stack"
        (frame:rest) -> do
            put st { bsScopes = rest }
            pure frame

peekScope :: ConstraintM ScopeFrame
peekScope = do
    st <- get
    case bsScopes st of
        [] -> error "peekScope: empty scope stack"
        (frame:_) -> pure frame

registerScopeNode :: NodeRef -> ConstraintM ()
registerScopeNode ref =
    modify' $ \st ->
        case bsScopes st of
            [] -> st
            (frame:rest) ->
                let frame' = frame { sfNodes = IntSet.insert (nodeRefKey ref) (sfNodes frame) }
                in st { bsScopes = frame' : rest }

rebindScopeNodes :: NodeRef -> NodeId -> ScopeFrame -> ConstraintM ()
rebindScopeNodes binder root frame = do
    nodes <- gets bsNodes
    genNodes <- gets bsGenNodes
    let binderIsGen = case binder of
            GenRef{} -> True
            TypeRef{} -> False
        schemeRootKeysAll =
            IntSet.fromList
                [ nodeRefKey (typeRef rootN)
                | gen <- IntMap.elems genNodes
                , rootN <- gnSchemes gen
                ]
        schemeRootKeysOwned =
            case binder of
                GenRef gid ->
                    case IntMap.lookup (genNodeKey gid) genNodes of
                        Nothing -> IntSet.empty
                        Just gen ->
                            IntSet.fromList
                                [ nodeRefKey (typeRef rootN)
                                | rootN <- gnSchemes gen
                                ]
                TypeRef _ -> IntSet.empty
        stopSchemeRoots = IntSet.difference schemeRootKeysAll schemeRootKeysOwned
        scopeNodes = sfNodes frame
        scopeTypeKeys = IntSet.filter even scopeNodes
        reachable =
            let shouldStop nid =
                    IntSet.member (nodeRefKey (typeRef nid)) stopSchemeRoots
                        && getNodeId nid /= getNodeId root
                childrenOf nid =
                    case IntMap.lookup (getNodeId nid) nodes of
                        Nothing -> []
                        Just node -> structuralChildrenWithBounds node
            in reachableFromStop
                getNodeId
                id
                childrenOf
                shouldStop
                root
        reachableTypeKeys =
            IntSet.fromList
                [ nodeRefKey (TypeRef (NodeId nid))
                | nid <- IntSet.toList reachable
                ]
        targetTypeKeys =
            if binderIsGen
                then scopeTypeKeys
                else IntSet.intersection reachableTypeKeys scopeNodes
        targetTypeIds =
            [ nid
            | key <- IntSet.toList targetTypeKeys
            , let ref = nodeRefFromKey key
            , TypeRef nid <- pure ref
            ]
        targetTypeSet = IntSet.fromList (map getNodeId targetTypeIds)
        referenced =
            IntSet.fromList
                [ getNodeId child
                | parentId <- targetTypeIds
                , Just parent <- pure (IntMap.lookup (getNodeId parentId) nodes)
                , child <- structuralChildrenWithBounds parent
                , IntSet.member (getNodeId child) targetTypeSet
                ]
        scopeRoots =
            [ NodeId nid
            | nid <- IntSet.toList (IntSet.difference targetTypeSet referenced)
            , case IntMap.lookup nid nodes of
                Just TyExp{} -> False
                _ -> True
            ]
        scopeRootKeys =
            IntSet.fromList
                [ nodeRefKey (TypeRef nid)
                | nid <- scopeRoots
                ]
        genScopeKeys =
            if binderIsGen
                then IntSet.filter odd scopeNodes
                else IntSet.empty
        schemeRootKeys = schemeRootKeysOwned
    modify' $ \st ->
        let parentIsSticky ref = case ref of
                GenRef _ -> True
                TypeRef pid ->
                    case IntMap.lookup (getNodeId pid) nodes of
                        Just TyVar{} -> True
                        Just TyForall{} -> True
                        Just TyArrow{} -> True
                        Just TyExp{} -> True
                        _ -> False
            isSchemeRootParent ref =
                binderIsGen && IntSet.member (nodeRefKey ref) schemeRootKeys
                    && case ref of
                        TypeRef pid ->
                            case IntMap.lookup (getNodeId pid) nodes of
                                Just TyVar{} -> True
                                Just TyForall{} -> True
                                Just TyExp{} -> True
                                _ -> False
                        GenRef _ -> False
            flagFor _childRef = BindFlex
            bindGen bp key =
                let childRef = nodeRefFromKey key
                in if nodeRefKey childRef == nodeRefKey binder
                    then bp
                    else
                        case IntMap.lookup key bp of
                            Just (GenRef gid, _)
                                | GenRef gid == binder -> bp
                            Just (parent, _)
                                | parentIsSticky parent
                                , nodeRefKey parent /= nodeRefKey childRef
                                , not (isSchemeRootParent parent) ->
                                    bp
                            _ -> IntMap.insert key (binder, flagFor childRef) bp
            bindType bp key =
                let childRef = nodeRefFromKey key
                in if nodeRefKey childRef == nodeRefKey binder
                    then bp
                    else
                        case IntMap.lookup key bp of
                            Just _ -> bp
                            Nothing -> IntMap.insert key (binder, BindFlex) bp
            bp0 = bsBindParents st
            bp1 =
                if binderIsGen
                    then IntSet.foldl' bindGen bp0 scopeRootKeys
                    else IntSet.foldl' bindType bp0 scopeRootKeys
            bp2 = IntSet.foldl' bindGen bp1 genScopeKeys
            genNodes' = case binder of
                GenRef gid ->
                    let gens0 = bsGenNodes st
                    in IntMap.adjust (\g -> g { gnSchemes = scopeRoots }) (genNodeKey gid) gens0
                TypeRef _ -> bsGenNodes st
        in st { bsBindParents = bp2, bsGenNodes = genNodes' }
