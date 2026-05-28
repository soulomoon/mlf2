{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module MLF.Constraint.Presolution.BindingRepair
    ( BindingRepairModel(..)
    , BindingRepairDirty(..)
    , emptyBindingRepairDirty
    , dirtyAllBindingRepair
    , dirtyBindingRepairTypes
    , dirtyBindingRepairBindRefs
    , mergeBindingRepairDirty
    , bindingRepairDirtyIsEmpty
    , buildBindingRepairModel
    , repairBindingParentsWithModel
    , repairBindingParentsDirtyWithModel
    ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Phase (Phase)

data BindingRepairModel (p :: Phase) = BindingRepairModel
    { brmAllTypeIds :: !IntSet.IntSet
    , brmGenRefs :: ![NodeRef]
    , brmIncomingParents :: !(IntMap.IntMap IntSet.IntSet)
    , brmTermRoots :: !IntSet.IntSet
    , brmStructuralEdges :: !(IntMap.IntMap IntSet.IntSet)
    }
    deriving (Eq, Show)

data BindingRepairDirty = BindingRepairDirty
    { brdDirtyTypeRefs :: !IntSet.IntSet
    , brdDirtyGenRefs :: !IntSet.IntSet
    , brdDirtyBindParentRefs :: !IntSet.IntSet
    , brdDirtyAll :: !Bool
    }
    deriving (Eq, Show)

emptyBindingRepairDirty :: BindingRepairDirty
emptyBindingRepairDirty =
    BindingRepairDirty
        { brdDirtyTypeRefs = IntSet.empty
        , brdDirtyGenRefs = IntSet.empty
        , brdDirtyBindParentRefs = IntSet.empty
        , brdDirtyAll = False
        }

dirtyAllBindingRepair :: BindingRepairDirty
dirtyAllBindingRepair =
    emptyBindingRepairDirty { brdDirtyAll = True }

dirtyBindingRepairTypes :: IntSet.IntSet -> BindingRepairDirty
dirtyBindingRepairTypes dirtyTypes =
    emptyBindingRepairDirty { brdDirtyTypeRefs = dirtyTypes }

dirtyBindingRepairBindRefs :: IntSet.IntSet -> BindingRepairDirty
dirtyBindingRepairBindRefs dirtyBindRefs =
    emptyBindingRepairDirty { brdDirtyBindParentRefs = dirtyBindRefs }

mergeBindingRepairDirty :: BindingRepairDirty -> BindingRepairDirty -> BindingRepairDirty
mergeBindingRepairDirty a b =
    BindingRepairDirty
        { brdDirtyTypeRefs = IntSet.union (brdDirtyTypeRefs a) (brdDirtyTypeRefs b)
        , brdDirtyGenRefs = IntSet.union (brdDirtyGenRefs a) (brdDirtyGenRefs b)
        , brdDirtyBindParentRefs = IntSet.union (brdDirtyBindParentRefs a) (brdDirtyBindParentRefs b)
        , brdDirtyAll = brdDirtyAll a || brdDirtyAll b
        }

bindingRepairDirtyIsEmpty :: BindingRepairDirty -> Bool
bindingRepairDirtyIsEmpty dirty =
    not (brdDirtyAll dirty)
        && IntSet.null (brdDirtyTypeRefs dirty)
        && IntSet.null (brdDirtyGenRefs dirty)
        && IntSet.null (brdDirtyBindParentRefs dirty)

buildBindingRepairModel
    :: (NodeId -> NodeId)
    -> Constraint p
    -> BindingRepairModel p
buildBindingRepairModel canonical c0 =
    BindingRepairModel
        { brmAllTypeIds = allTypeIds
        , brmGenRefs = genRefs
        , brmIncomingParents = incomingParents
        , brmTermRoots = Binding.computeTermDagRootsUnder canonical c0
        , brmStructuralEdges = structuralEdges
        }
  where
    nodeMap = getNodeMap (cNodes c0)
    nodeValues = IntMap.elems nodeMap
    allTypeIds =
        IntSet.fromList
            [ getNodeId (canonical (NodeId k))
            | k <- IntMap.keys nodeMap
            ]
    genRefs =
        [ genRef (GenNodeId gid)
        | gid <- IntMap.keys (getGenNodeMap (cGenNodes c0))
        ]
    incomingParents =
        let addOne parent child m =
                IntMap.insertWith
                    IntSet.union
                    (getNodeId child)
                    (IntSet.singleton (getNodeId parent))
                    m
            addNode m node =
                let parent = canonical (tnId node)
                    kids = map canonical (structuralChildrenWithBounds node)
                in foldl' (flip (addOne parent)) m kids
        in foldl' addNode IntMap.empty nodeValues
    addTypeEdges m node =
        let parentKey = nodeRefKey (TypeRef (canonical (tnId node)))
            childKeys =
                IntSet.fromList
                    [ nodeRefKey (TypeRef (canonical child))
                    | child <- structuralChildrenWithBounds node
                    , canonical child /= canonical (tnId node)
                    ]
        in if IntSet.null childKeys
            then m
            else IntMap.insertWith IntSet.union parentKey childKeys m
    addGenEdges m genNode =
        let parentKey = nodeRefKey (GenRef (gnId genNode))
            childKeys =
                IntSet.fromList
                    [ nodeRefKey (TypeRef (canonical child))
                    | child <- gnSchemes genNode
                    ]
        in if IntSet.null childKeys
            then m
            else IntMap.insertWith IntSet.union parentKey childKeys m
    structuralEdges =
        foldl'
            addTypeEdges
            (foldl' addGenEdges IntMap.empty (NodeAccess.allGenNodes c0))
            nodeValues
{-# INLINE buildBindingRepairModel #-}

repairBindingParentsWithModel
    :: BindingRepairModel p
    -> BindParents
    -> BindParents
repairBindingParentsWithModel model bp0 =
    fixUpper bp1
  where
    rootGen =
        let isRoot ref = not (IntMap.member (nodeRefKey ref) bp0)
        in case filter isRoot (brmGenRefs model) of
            (g:_) -> Just g
            [] -> Nothing

    addMissing bp nidInt =
        let childRef = typeRef (NodeId nidInt)
            childKey = nodeRefKey childRef
        in if IntMap.member childKey bp
            then bp
            else
                let parentRef =
                        case IntMap.lookup nidInt (brmIncomingParents model) of
                            Just ps ->
                                case IntSet.toList ps of
                                    [] -> Nothing
                                    (p:_) -> Just (typeRef (NodeId p))
                            Nothing ->
                                if IntSet.member nidInt (brmTermRoots model)
                                    then rootGen
                                    else Nothing
                in case parentRef of
                    Nothing -> bp
                    Just p ->
                        if p == childRef
                            then bp
                            else IntMap.insert childKey (p, BindFlex) bp
    bp1 = IntSet.foldl' addMissing bp0 (brmAllTypeIds model)

    isUpperUnder parent child =
        let parentKey = nodeRefKey parent
            childKey = nodeRefKey child
            go visited key =
                if IntSet.member key visited
                    then False
                    else if key == childKey
                        then True
                        else
                            let visited' = IntSet.insert key visited
                                kids = IntSet.toList (IntMap.findWithDefault IntSet.empty key (brmStructuralEdges model))
                            in any (go visited') kids
        in if parentKey == childKey
            then True
            else go IntSet.empty parentKey

    pickUpperParent childN =
        case IntMap.lookup (getNodeId childN) (brmIncomingParents model) of
            Just ps ->
                case IntSet.toList ps of
                    (p:_) -> Just (typeRef (NodeId p))
                    [] -> rootGen
            Nothing -> rootGen

    fixUpper bp =
        IntMap.mapWithKey
            ( \childKey (parentRef, flag) ->
                case nodeRefFromKey childKey of
                    GenRef _ -> (parentRef, flag)
                    TypeRef childN ->
                        case parentRef of
                            GenRef _ -> (parentRef, flag)
                            _ ->
                                if isUpperUnder parentRef (typeRef childN)
                                    then (parentRef, flag)
                                    else
                                        case pickUpperParent childN of
                                            Just pRef ->
                                                if pRef == typeRef childN
                                                    then
                                                        case rootGen of
                                                            Just gref -> (gref, flag)
                                                            Nothing -> (parentRef, flag)
                                                    else (pRef, flag)
                                            Nothing -> (parentRef, flag)
            )
            bp
{-# INLINE repairBindingParentsWithModel #-}

repairBindingParentsDirtyWithModel
    :: BindingRepairDirty
    -> BindingRepairModel p
    -> BindParents
    -> BindParents
repairBindingParentsDirtyWithModel dirty model bp0
    | brdDirtyAll dirty = repairBindingParentsWithModel model bp0
    | bindingRepairDirtyIsEmpty dirty = bp0
    | otherwise = fixDirtyUpper bp1 affectedBindKeys
  where
    rootGen =
        let isRoot ref = not (IntMap.member (nodeRefKey ref) bp0)
        in case filter isRoot (brmGenRefs model) of
            (g:_) -> Just g
            [] -> Nothing

    dirtyTypeRefs =
        IntSet.map (nodeRefKey . typeRef . NodeId) (brdDirtyTypeRefs dirty)
    dirtyGenRefs =
        IntSet.map (nodeRefKey . genRef . GenNodeId) (brdDirtyGenRefs dirty)
    dirtyRefs =
        IntSet.unions
            [ dirtyTypeRefs
            , dirtyGenRefs
            , brdDirtyBindParentRefs dirty
            ]
    affectedStructuralChildren =
        IntSet.foldl'
            ( \acc key ->
                IntSet.union acc (IntMap.findWithDefault IntSet.empty key (brmStructuralEdges model))
            )
            IntSet.empty
            dirtyRefs
    affectedBindKeys = IntSet.union dirtyRefs affectedStructuralChildren

    affectedTypeIds =
        IntSet.foldl'
            ( \acc key ->
                case nodeRefFromKey key of
                    TypeRef nid -> IntSet.insert (getNodeId nid) acc
                    GenRef _ -> acc
            )
            IntSet.empty
            affectedBindKeys

    addMissing bp nidInt =
        let childRef = typeRef (NodeId nidInt)
            childKey = nodeRefKey childRef
        in if IntMap.member childKey bp
            then bp
            else
                let parentRef =
                        case IntMap.lookup nidInt (brmIncomingParents model) of
                            Just ps ->
                                case IntSet.toList ps of
                                    [] -> Nothing
                                    (p:_) -> Just (typeRef (NodeId p))
                            Nothing ->
                                if IntSet.member nidInt (brmTermRoots model)
                                    then rootGen
                                    else Nothing
                in case parentRef of
                    Nothing -> bp
                    Just p ->
                        if p == childRef
                            then bp
                            else IntMap.insert childKey (p, BindFlex) bp

    bp1 = IntSet.foldl' addMissing bp0 affectedTypeIds

    isUpperUnder parent child =
        let parentKey = nodeRefKey parent
            childKey = nodeRefKey child
            go visited key =
                if IntSet.member key visited
                    then False
                    else if key == childKey
                        then True
                        else
                            let visited' = IntSet.insert key visited
                                kids = IntSet.toList (IntMap.findWithDefault IntSet.empty key (brmStructuralEdges model))
                            in any (go visited') kids
        in if parentKey == childKey
            then True
            else go IntSet.empty parentKey

    pickUpperParent childN =
        case IntMap.lookup (getNodeId childN) (brmIncomingParents model) of
            Just ps ->
                case IntSet.toList ps of
                    (p:_) -> Just (typeRef (NodeId p))
                    [] -> rootGen
            Nothing -> rootGen

    fixOne childKey bp =
        case IntMap.lookup childKey bp of
            Nothing -> bp
            Just (parentRef, flag) ->
                case nodeRefFromKey childKey of
                    GenRef _ -> bp
                    TypeRef childN ->
                        case parentRef of
                            GenRef _ -> bp
                            _ ->
                                if isUpperUnder parentRef (typeRef childN)
                                    then bp
                                    else
                                        case pickUpperParent childN of
                                            Just pRef ->
                                                if pRef == typeRef childN
                                                    then
                                                        case rootGen of
                                                            Just gref -> IntMap.insert childKey (gref, flag) bp
                                                            Nothing -> bp
                                                    else IntMap.insert childKey (pRef, flag) bp
                                            Nothing -> bp

    fixDirtyUpper bp =
        IntSet.foldl' (flip fixOne) bp
{-# INLINE repairBindingParentsDirtyWithModel #-}
