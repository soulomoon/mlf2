module MLF.Elab.Run.Generalize.Common (
    nodeMapToIntMap,
    insertBindParent,
    applyBindParent,
    flagFromExisting,
    mkOkRef,
    mkIsUpperRef,
    mkAllowBindEdge,
    schemeRootsOf,
    childrenFrom,
    reachableFromWithBounds,
    isTyVarAt,
    isTyVarNode,
    baseFirstGenAncestorWith,
    mapBaseRefWith,
    ownerIsOther,
    chooseRootGenId
) where

import Data.Functor.Foldable (ListF(..), hylo)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , Constraint
    , GenNode(..)
    , GenNodeId(..)
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , genNodeKey
    , getNodeId
    , gnSchemes
    , nodeRefKey
    , structuralChildrenWithBounds
    , typeRef
    )
import qualified MLF.Constraint.Types.Graph as Graph
import MLF.Elab.Run.Generalize.Types
    ( BindParents
    , GenMap
    , InsertMode(..)
    , NodeKey
    , NodeMap
    )

nodeMapToIntMap :: Graph.NodeMap a -> IntMap.IntMap a
nodeMapToIntMap nodes =
    IntMap.fromList
        [ (getNodeId nid, node)
        | (nid, node) <- Graph.toListNode nodes
        ]

insertBindParent :: InsertMode -> NodeRef -> BindFlag -> NodeKey -> BindParents -> BindParents
insertBindParent mode parentRef flag childKey acc =
    let entry = (parentRef, flag)
        insert = IntMap.insert childKey entry acc
    in case mode of
        KeepOld ->
            IntMap.insertWith
                (\_ old -> old)
                childKey
                entry
                acc
        Override ->
            insert
        SelfOrEmpty ->
            case IntMap.lookup childKey acc of
                Nothing ->
                    insert
                Just (parentExisting, _flagExisting)
                    | nodeRefKey parentExisting == childKey ->
                        insert
                    | otherwise -> acc

applyBindParent
    :: (NodeRef -> NodeRef -> Bool)
    -> InsertMode
    -> NodeRef
    -> NodeRef
    -> BindFlag
    -> BindParents
    -> BindParents
applyBindParent allow mode childRef parentRef flag acc
    | allow childRef parentRef =
        insertBindParent mode parentRef flag (nodeRefKey childRef) acc
    | otherwise = acc

flagFromExisting :: BindFlag -> NodeKey -> BindParents -> BindFlag
flagFromExisting defaultFlag childKey acc =
    case IntMap.lookup childKey acc of
        Just (_parentExisting, flag) -> flag
        Nothing -> defaultFlag

-- Common predicate builders
mkOkRef :: NodeMap -> GenMap -> NodeRef -> Bool
mkOkRef nodes gens ref =
    case ref of
        TypeRef nid -> IntMap.member (getNodeId nid) nodes
        GenRef gid -> IntMap.member (genNodeKey gid) gens

mkIsUpperRef :: Constraint -> (NodeRef -> NodeRef -> Bool)
mkIsUpperRef upperConstraint parentRef childRef =
    case parentRef of
        TypeRef _ -> Binding.isUpper upperConstraint parentRef childRef
        GenRef _ -> True

mkAllowBindEdge :: (NodeRef -> Bool) -> (NodeRef -> NodeRef -> Bool) -> NodeRef -> NodeRef -> Bool
mkAllowBindEdge okRef isUpperRef childRef parentRef =
    okRef childRef
        && okRef parentRef
        && nodeRefKey childRef /= nodeRefKey parentRef
        && isUpperRef parentRef childRef

schemeRootsOf :: GenMap -> [NodeId]
schemeRootsOf = foldMap gnSchemes

childrenFrom :: NodeMap -> NodeKey -> [NodeId]
childrenFrom nodes key =
    maybe
        []
        structuralChildrenWithBounds
        (IntMap.lookup key nodes)

reachableFromWithBounds :: NodeMap -> NodeId -> IntSet.IntSet
reachableFromWithBounds nodes start =
    let alg Nil = IntSet.empty
        alg (Cons nid acc) = IntSet.insert (getNodeId nid) acc
        coalg (visited, queue) =
            case queue of
                [] -> Nil
                (nid0:rest) ->
                    let key = getNodeId nid0
                    in if IntSet.member key visited
                        then Cons nid0 (visited, rest)
                        else
                            let visited' = IntSet.insert key visited
                                kids = childrenFrom nodes key
                            in Cons nid0 (visited', kids ++ rest)
    in hylo alg coalg (IntSet.empty, [start])

isTyVarAt :: NodeMap -> NodeKey -> Bool
isTyVarAt nodes key =
    maybe False isTyVarNode (IntMap.lookup key nodes)

isTyVarNode :: TyNode -> Bool
isTyVarNode node =
    case node of
        TyVar{} -> True
        _ -> False

-- | Look up the first gen ancestor of a node in the base constraint's bind parents.
baseFirstGenAncestorWith
    :: IntMap.IntMap NodeId  -- ^ solvedToBase mapping
    -> BindParents           -- ^ base bind parents
    -> NodeRef               -- ^ starting reference
    -> Maybe GenNodeId
baseFirstGenAncestorWith solvedToBase bindParentsBase ref0 =
    let baseRef =
            case ref0 of
                GenRef gid -> GenRef gid
                TypeRef nid ->
                    case IntMap.lookup (getNodeId nid) solvedToBase of
                        Just baseN -> typeRef baseN
                        Nothing -> typeRef nid
        alg Nil = Nothing
        alg (Cons parentRef rest) =
            case parentRef of
                GenRef gid -> Just gid
                _ -> rest
        coalg (visited, ref, stop) =
            if stop || IntSet.member (nodeRefKey ref) visited
                then Nil
                else
                    case IntMap.lookup (nodeRefKey ref) bindParentsBase of
                        Nothing -> Nil
                        Just (parentRef, _) ->
                            let visited' = IntSet.insert (nodeRefKey ref) visited
                                stop' =
                                    case parentRef of
                                        GenRef _ -> True
                                        _ -> False
                            in Cons parentRef (visited', parentRef, stop')
    in hylo alg coalg (IntSet.empty, baseRef, False)

-- | Map a base constraint reference to its solved equivalent.
mapBaseRefWith
    :: (NodeId -> NodeId)     -- ^ canonical function
    -> IntMap.IntMap NodeId   -- ^ baseToSolved mapping
    -> NodeMap                -- ^ nodesSolved
    -> NodeRef                -- ^ reference to map
    -> Maybe NodeRef
mapBaseRefWith canonical baseToSolved nodesSolved ref =
    case ref of
        GenRef gid -> Just (GenRef gid)
        TypeRef nid ->
            case IntMap.lookup (getNodeId nid) baseToSolved of
                Just solvedNid -> Just (TypeRef (canonical solvedNid))
                Nothing ->
                    if IntMap.member (getNodeId nid) nodesSolved
                        then Just (TypeRef (canonical nid))
                        else Nothing

-- | Check if a gen node ID differs from the given one.
ownerIsOther :: GenNodeId -> Maybe GenNodeId -> Bool
ownerIsOther gid = maybe False (/= gid)

-- | Choose the root gen node ID from a constraint.
chooseRootGenId :: Constraint -> GenMap -> GenNodeId
chooseRootGenId constraint gens =
    case [gid | GenRef gid <- Binding.bindingRoots constraint] of
        [gid] -> gid
        _ ->
            case IntMap.keys gens of
                (k:_) -> GenNodeId k
                [] -> GenNodeId 0
