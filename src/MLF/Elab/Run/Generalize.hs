module MLF.Elab.Run.Generalize (
    pruneBindParentsConstraint,
    instantiationCopyNodes,
    constraintForGeneralization
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Solve (SolveResult, frWith, srConstraint, srUnionFind)
import MLF.Constraint.Types
    ( BindFlag(..)
    , Constraint
    , GenNode(..)
    , GenNodeId(..)
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , cBindParents
    , cGenNodes
    , cNodes
    , genNodeKey
    , getNodeId
    , gnId
    , gnSchemes
    , nodeRefFromKey
    , nodeRefKey
    , structuralChildrenWithBounds
    , typeRef
    )
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Constraint.Presolution.Plan.BindingUtil (bindingPathToRootLocal, firstGenAncestorFrom)
import MLF.Elab.Run.Debug (debugGaScope)
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Util.Graph (reachableFromStop)
import MLF.Frontend.ConstraintGen (AnnExpr)

pruneBindParentsConstraint :: Constraint -> Constraint
pruneBindParentsConstraint c =
    let liveNodes = cNodes c
        liveGens = cGenNodes c
        liveChild childKey =
            case nodeRefFromKey childKey of
                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
        liveParent ref =
            case ref of
                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
        bindParents' =
            IntMap.filterWithKey
                (\childKey (parentRef, _flag) ->
                    liveChild childKey && liveParent parentRef
                )
                (cBindParents c)
    in c { cBindParents = bindParents' }

instantiationCopyNodes :: SolveResult -> IntMap.IntMap NodeId -> IntMap.IntMap EdgeTrace -> IntSet.IntSet
instantiationCopyNodes solved redirects edgeTraces =
    let canonical = frWith (srUnionFind solved)
        adoptNode nid = canonical (chaseRedirects redirects nid)
        collectTrace tr =
            let copyRaw =
                    [ getNodeId node
                    | node <- IntMap.elems (etCopyMap tr)
                    ]
                copyCanon =
                    [ getNodeId (adoptNode node)
                    | node <- IntMap.elems (etCopyMap tr)
                    ]
                interiorRaw =
                    [ nid
                    | nid <- IntSet.toList (etInterior tr)
                    ]
                interiorCanon =
                    [ getNodeId (adoptNode (NodeId nid))
                    | nid <- IntSet.toList (etInterior tr)
                    ]
                rootRaw = getNodeId (etRoot tr)
                rootCanon = getNodeId (adoptNode (etRoot tr))
            in IntSet.fromList (rootRaw : rootCanon : copyRaw ++ copyCanon ++ interiorRaw ++ interiorCanon)
    in IntSet.unions (map collectTrace (IntMap.elems edgeTraces))

constraintForGeneralization :: SolveResult -> IntMap.IntMap NodeId -> IntSet.IntSet -> IntMap.IntMap NodeId -> Constraint -> AnnExpr -> (Constraint, GaBindParents)
constraintForGeneralization solved redirects instCopyNodes instCopyMap base _ann =
    let solvedConstraint = srConstraint solved
        canonical = frWith (srUnionFind solved)
        nodesSolved0 = cNodes solvedConstraint
        schemeRootsBase =
            [ root
            | gen <- IntMap.elems (cGenNodes base)
            , root <- gnSchemes gen
            ]
        restoreSchemeRoot acc root =
            let mbBase =
                    case IntMap.lookup (getNodeId root) (cNodes base) of
                        Just TyVar{ tnBound = mb } ->
                            case mb of
                                Nothing -> Nothing
                                Just bnd ->
                                    case adoptRef (typeRef bnd) of
                                        TypeRef bnd' -> Just bnd'
                                        GenRef _ -> Nothing
                        _ -> Nothing
            in case IntMap.lookup (getNodeId root) acc of
                Nothing ->
                    case mbBase of
                        Just bnd' ->
                            let rootNode = TyVar { tnId = root, tnBound = Just bnd' }
                            in IntMap.insert (getNodeId root) rootNode acc
                        Nothing -> acc
                Just TyVar{ tnId = nid, tnBound = Nothing } ->
                    case mbBase of
                        Just bnd' ->
                            IntMap.insert (getNodeId root) (TyVar { tnId = nid, tnBound = Just bnd' }) acc
                        Nothing -> acc
                Just _ -> acc
        schemeRootsBaseSet =
            IntSet.fromList (map getNodeId schemeRootsBase)
        schemeRootsSolvedSet =
            IntSet.fromList
                [ getNodeId (canonical root)
                | gen <- IntMap.elems (cGenNodes solvedConstraint)
                , root <- gnSchemes gen
                ]
        schemeRootsAllSet =
            IntSet.union schemeRootsBaseSet schemeRootsSolvedSet
        nodesSolved1 = foldl' restoreSchemeRoot nodesSolved0 schemeRootsBase
        nodesSolved =
            let
                nodesSolvedBaseAdjusted =
                    IntMap.foldlWithKey'
                        (\acc key node ->
                            case node of
                                TyVar{ tnBound = Just bndBase } ->
                                    if IntSet.member (getNodeId bndBase) schemeRootsBaseSet
                                        then
                                            case applyRedirectsToRef (typeRef bndBase) of
                                                TypeRef bnd' ->
                                                    IntMap.adjust
                                                        (\n -> case n of
                                                            TyVar{} -> n { tnBound = Just bnd' }
                                                            _ -> n
                                                        )
                                                        key
                                                        acc
                                                GenRef _ -> acc
                                        else acc
                                _ -> acc
                        )
                        nodesSolved1
                        (cNodes base)
                adoptNodeId nid =
                    case adoptRef (typeRef nid) of
                        TypeRef nid' -> nid'
                        GenRef _ -> nid
                restoreNamedVars acc =
                    let baseNodes = cNodes base
                        preferBaseVar new old =
                            case (new, old) of
                                (TyVar{ tnBound = Nothing }, TyVar{ tnBound = Just _ }) -> old
                                (TyVar{}, TyVar{}) -> new
                                _ -> old
                        insertNamed acc' childKey _parentRef =
                            case IntMap.lookup childKey baseNodes of
                                Just TyVar{ tnId = baseId, tnBound = mb } ->
                                    let baseId' = baseId
                                        mb' = fmap adoptNodeId mb
                                        node' = TyVar { tnId = baseId', tnBound = mb' }
                                    in IntMap.insertWith
                                        preferBaseVar
                                        (getNodeId baseId')
                                        node'
                                        acc'
                                _ -> acc'
                    in IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, _flag) ->
                            insertNamed acc' childKey parentRef
                        )
                        acc
                        (cBindParents base)
                restoreSchemeInteriorVars acc =
                    let
                        baseNodes = cNodes base
                        reachableFromWithBoundsBaseLocal start =
                            let go visited [] = visited
                                go visited (nid0:rest) =
                                    let key = getNodeId nid0
                                    in if IntSet.member key visited
                                        then go visited rest
                                        else
                                            let visited' = IntSet.insert key visited
                                                kids =
                                                    case IntMap.lookup key baseNodes of
                                                        Nothing -> []
                                                        Just node ->
                                                            structuralChildrenWithBounds node
                                            in go visited' (kids ++ rest)
                            in go IntSet.empty [start]
                        schemeInteriorsBase =
                            IntSet.unions
                                [ reachableFromWithBoundsBaseLocal root
                                | gen <- IntMap.elems (cGenNodes base)
                                , root <- gnSchemes gen
                                ]
                        preferBaseVar new old =
                            case (new, old) of
                                (TyVar{ tnBound = Nothing }, TyVar{ tnBound = Just _ }) -> old
                                (TyVar{}, TyVar{}) -> new
                                _ -> old
                        insertVarFromBase acc' key =
                            if IntSet.member key schemeRootsBaseSet
                                then acc'
                                else
                                    case IntMap.lookup key baseNodes of
                                        Just TyVar{ tnId = baseId, tnBound = mb } ->
                                            let baseId' = adoptNodeId baseId
                                                mb' = fmap adoptNodeId mb
                                                node' = TyVar { tnId = baseId', tnBound = mb' }
                                            in IntMap.insertWith
                                                preferBaseVar
                                                (getNodeId baseId')
                                                node'
                                                acc'
                                        _ -> acc'
                    in IntSet.foldl' insertVarFromBase acc schemeInteriorsBase
                restoreBindParentVars acc =
                    let
                        baseNodes = cNodes base
                        preferBaseVar new old =
                            case (new, old) of
                                (TyVar{ tnBound = Nothing }, TyVar{ tnBound = Just _ }) -> old
                                (TyVar{}, TyVar{}) -> new
                                _ -> old
                        parentKeys =
                            [ getNodeId parent
                            | (_childKey, (TypeRef parent, _flag)) <- IntMap.toList (cBindParents base)
                            ]
                        childKeys =
                            [ getNodeId child
                            | (childKey, _parent) <- IntMap.toList (cBindParents base)
                            , TypeRef child <- [nodeRefFromKey childKey]
                            ]
                        keys = IntSet.fromList (parentKeys ++ childKeys)
                        insertVarFromBase acc' key =
                            case IntMap.lookup key baseNodes of
                                Just TyVar{ tnId = baseId, tnBound = mb } ->
                                    let baseId' = adoptNodeId baseId
                                        mb' = fmap adoptNodeId mb
                                        node' = TyVar { tnId = baseId', tnBound = mb' }
                                    in IntMap.insertWith
                                        preferBaseVar
                                        (getNodeId baseId')
                                        node'
                                        acc'
                                _ -> acc'
                    in IntSet.foldl' insertVarFromBase acc keys
            in restoreBindParentVars (restoreSchemeInteriorVars (restoreNamedVars nodesSolvedBaseAdjusted))
        genSolved = cGenNodes solvedConstraint
        genMerged = IntMap.union (cGenNodes base) genSolved
        applyRedirectsToRef ref =
            case ref of
                TypeRef nid -> TypeRef (chaseRedirects redirects nid)
                GenRef gid -> GenRef gid
        canonicalRef = Canonicalize.canonicalRef canonical
        adoptRef = canonicalRef . applyRedirectsToRef
        okRef ref =
            case ref of
                TypeRef nid -> IntMap.member (getNodeId nid) nodesSolved
                GenRef gid -> IntMap.member (genNodeKey gid) genMerged
        upperConstraint =
            solvedConstraint { cNodes = nodesSolved }
        isUpperRef parentRef childRef =
            case parentRef of
                TypeRef _ -> Binding.isUpper upperConstraint parentRef childRef
                GenRef _ -> True
        wasRedirected ref =
            case ref of
                TypeRef nid ->
                    let ref' = applyRedirectsToRef ref
                    in nodeRefKey ref' /= nodeRefKey (TypeRef nid)
                GenRef _ -> False
        bindParentsBase = cBindParents base
        stickyTypeParentsBase =
            IntSet.fromList
                [ childKey
                | (childKey, (parentRef, _flag)) <- IntMap.toList bindParentsBase
                , case parentRef of
                    TypeRef _ -> True
                    _ -> False
                ]
        baseNamedKeys =
            IntSet.fromList
                [ childKey
                | (childKey, _parentRef) <- IntMap.toList bindParentsBase
                , TypeRef child <- [nodeRefFromKey childKey]
                , case IntMap.lookup (getNodeId child) (cNodes base) of
                    Just TyVar{} -> True
                    _ -> False
                ]
        bindParentsSolved = cBindParents solvedConstraint
        insertBindParentBase acc childKey parentRef flag =
            let childRef = nodeRefFromKey childKey
                childRef' = adoptRef childRef
                parentRef' = adoptRef parentRef
                childKey' = nodeRefKey childRef'
                allowParent =
                    case parentRef of
                        TypeRef _ -> True
                        GenRef _ -> isUpperRef parentRef' childRef'
            in if not (okRef childRef' && okRef parentRef')
                then acc
            else if nodeRefKey childRef' == nodeRefKey parentRef'
                then acc
            else if not allowParent
                then acc
            else IntMap.insertWith (\_ old -> old) childKey' (parentRef', flag) acc

        insertBindParentSolved acc childKey parentRef flag =
            let childRef = nodeRefFromKey childKey
                childRef' = adoptRef childRef
                parentRef' = adoptRef parentRef
                childKey' = nodeRefKey childRef'
                isSelf = nodeRefKey childRef' == nodeRefKey parentRef'
                existing = IntMap.lookup childKey' acc
                childIsCopy =
                    case childRef' of
                        TypeRef nid -> IntSet.member (getNodeId nid) instCopyNodes
                        GenRef _ -> False
                existingSelf =
                    case existing of
                        Just (parentExisting, _) -> nodeRefKey parentExisting == childKey'
                        Nothing -> False
            in if not (okRef childRef' && okRef parentRef')
                then acc
            else if isSelf
                then acc
            else if not (isUpperRef parentRef' childRef')
                then acc
            else case existing of
                Just _ | childIsCopy && not existingSelf -> acc
                Just _ | childIsCopy ->
                    debugGaScope
                        ("constraintForGeneralization: bind-parent override copy child="
                            ++ show childRef'
                            ++ " parent="
                            ++ show parentRef'
                            ++ " redirected="
                            ++ show (wasRedirected childRef)
                        )
                        (IntMap.insert childKey' (parentRef', flag) acc)
                Nothing ->
                    debugGaScope
                        ("constraintForGeneralization: bind-parent fill child="
                            ++ show childRef'
                            ++ " parent="
                            ++ show parentRef'
                            ++ " redirected="
                            ++ show (wasRedirected childRef)
                        )
                        (IntMap.insert childKey' (parentRef', flag) acc)
                Just _ | existingSelf ->
                    debugGaScope
                        ("constraintForGeneralization: bind-parent override self-parent child="
                            ++ show childRef'
                            ++ " parent="
                            ++ show parentRef'
                            ++ " redirected="
                            ++ show (wasRedirected childRef)
                        )
                        (IntMap.insert childKey' (parentRef', flag) acc)
                _ -> acc

        bindParentsBase' =
            IntMap.foldlWithKey'
                (\acc childKey (parentRef, flag) ->
                    insertBindParentBase acc childKey parentRef flag
                )
                IntMap.empty
                bindParentsBase
        baseToSolved =
            let baseNodes = cNodes base
                baseReachableFromWithBounds start =
                    let go visited [] = visited
                        go visited (nid0:rest) =
                            let key = getNodeId nid0
                            in if IntSet.member key visited
                                then go visited rest
                                else
                                    let visited' = IntSet.insert key visited
                                        kids =
                                            case IntMap.lookup key baseNodes of
                                                Nothing -> []
                                                Just node ->
                                                    structuralChildrenWithBounds node
                                    in go visited' (kids ++ rest)
                    in go IntSet.empty [start]
                schemeInteriorKeys =
                    IntSet.unions
                        [ baseReachableFromWithBounds root
                        | gen <- IntMap.elems (cGenNodes base)
                        , root <- gnSchemes gen
                        ]
                bindParentKeys =
                    IntSet.fromList
                        ( [ getNodeId child
                          | (childKey, _parent) <- IntMap.toList bindParentsBase
                          , TypeRef child <- [nodeRefFromKey childKey]
                          ]
                            ++ [ getNodeId parent
                               | (_childKey, (TypeRef parent, _flag)) <- IntMap.toList bindParentsBase
                               ]
                        )
                baseKeys =
                    IntSet.unions
                        [ IntSet.fromList (IntMap.keys baseNodes)
                        , schemeInteriorKeys
                        , bindParentKeys
                        ]
                adoptNode nid0 =
                    case adoptRef (typeRef (NodeId nid0)) of
                        TypeRef nid' -> nid'
                        GenRef _ -> NodeId nid0
                chooseMapping nid0 =
                    let adopted = adoptNode nid0
                        adoptedKey = getNodeId adopted
                        baseNodeExists = IntMap.member nid0 nodesSolved
                        adoptedExists = IntMap.member adoptedKey nodesSolved
                        baseIsNamed = IntSet.member nid0 baseNamedKeys
                        baseIsEliminated = VarStore.isEliminatedVar solvedConstraint (NodeId nid0)
                    in if baseIsNamed && baseNodeExists && not baseIsEliminated
                        then NodeId nid0
                        else if adoptedExists
                            then adopted
                            else if baseNodeExists
                                then NodeId nid0
                                else adopted
            in IntMap.fromList
                [ (nid0, chooseMapping nid0)
                | nid0 <- IntSet.toList baseKeys
                ]
        solvedToBase0 =
            IntMap.foldlWithKey'
                (\acc baseKey solvedNid ->
                    let solvedKeyC = getNodeId (canonical solvedNid)
                        solvedKeyRaw = getNodeId solvedNid
                        acc' = IntMap.insertWith (\_ old -> old) solvedKeyC (NodeId baseKey) acc
                    in IntMap.insertWith (\_ old -> old) solvedKeyRaw (NodeId baseKey) acc'
                )
                IntMap.empty
                baseToSolved
        solvedToBase =
            let copyOverrides =
                    IntMap.fromList
                        [ (copyKeyC, baseN)
                        | (copyKey, baseN) <- IntMap.toList instCopyMap
                        , let copyKeyC = getNodeId (canonical (NodeId copyKey))
                        , IntMap.member copyKeyC nodesSolved
                        ]
            in IntMap.union copyOverrides solvedToBase0

        bindParents' =
            IntMap.foldlWithKey'
                (\acc childKey (parentRef, flag) ->
                    insertBindParentSolved acc childKey parentRef flag
                )
                bindParentsBase'
                bindParentsSolved
        bindParentsWithCopies =
            IntMap.foldlWithKey'
                (\acc copyKey baseN ->
                    let childRef = typeRef (NodeId copyKey)
                        childRef' = adoptRef childRef
                        childKey' = nodeRefKey childRef'
                    in case IntMap.lookup (nodeRefKey (typeRef baseN)) bindParentsBase of
                        Just (parentRef, flag) ->
                            let parentRef' = adoptRef parentRef
                            in if not (okRef childRef' && okRef parentRef')
                                then acc
                                else if nodeRefKey childRef' == nodeRefKey parentRef'
                                    then acc
                                    else IntMap.insertWith (\_ old -> old) childKey' (parentRef', flag) acc
                        Nothing -> acc
                )
                bindParents'
                instCopyMap
        schemeRootOwners =
            IntMap.fromList
                [ (getNodeId (canonical root), gnId gen)
                | gen <- IntMap.elems genMerged
                , root <- gnSchemes gen
                ]
        schemeRootOwnersBase =
            IntMap.filterWithKey
                (\k _ -> IntSet.member k schemeRootsAllSet)
                schemeRootOwners
        schemeRootOwnersFromBase =
            IntMap.fromList
                [ (getNodeId (canonical solvedRoot), gid)
                | root <- schemeRootsBase
                , Just gid <- [baseFirstGenAncestor (typeRef root)]
                , Just (TypeRef solvedRoot) <- [mapBaseRef (typeRef root)]
                ]
        allSchemeRoots =
            IntSet.fromList
                (IntMap.keys schemeRootOwnersBase)
        reachableFromWithBoundsStop start =
            let stopSet = allSchemeRoots
                shouldStop nid = IntSet.member (getNodeId nid) stopSet
                children nid =
                    case IntMap.lookup (getNodeId nid) nodesSolved of
                        Nothing -> []
                        Just node ->
                            structuralChildrenWithBounds node
            in reachableFromStop getNodeId canonical children shouldStop start
        reachableFromWithBoundsBaseStop start =
            let baseNodes = cNodes base
                stopSet = schemeRootsBaseSet
                adoptNodeId nid =
                    case adoptRef (typeRef nid) of
                        TypeRef nid' -> nid'
                        GenRef _ -> nid
                startKey = getNodeId start
                go _ acc [] = acc
                go visited acc (nid0:rest) =
                    let key = getNodeId nid0
                    in if IntSet.member key visited
                        then go visited acc rest
                        else
                            let visited' = IntSet.insert key visited
                            in if key /= startKey && IntSet.member key stopSet
                                then go visited' acc rest
                                else
                                    let acc' = IntSet.insert (getNodeId (adoptNodeId nid0)) acc
                                        kids =
                                            case IntMap.lookup key baseNodes of
                                                Nothing -> []
                                                Just node ->
                                                    structuralChildrenWithBounds node
                                    in go visited' acc' (kids ++ rest)
            in go IntSet.empty IntSet.empty [start]
        boundSchemeRoots =
            IntSet.fromList
                [ getNodeId (canonical bnd)
                | node <- IntMap.elems nodesSolved
                , TyVar{ tnBound = Just bnd } <- [node]
                , IntSet.member (getNodeId (canonical bnd)) allSchemeRoots
                ]
        schemeRootOwnersFiltered =
            IntMap.union schemeRootOwnersFromBase schemeRootOwnersBase
        schemeRootByBodySolved =
            IntMap.fromListWith
                (\a _ -> a)
                [ (getNodeId (canonical bnd), root)
                | gen <- IntMap.elems genMerged
                , root <- gnSchemes gen
                , Just bnd <- [VarStore.lookupVarBound upperConstraint root]
                , case IntMap.lookup (getNodeId (canonical bnd)) nodesSolved of
                    Just TyBase{} -> False
                    Just TyBottom{} -> False
                    _ -> True
                ]
        schemeInteriorOwnersFiltered =
            IntMap.fromListWith
                (\_ old -> old)
                [ (nidInt, gid)
                | (rootKey, gid) <- IntMap.toList schemeRootOwnersFiltered
                , nidInt <- IntSet.toList (reachableFromWithBoundsStop (NodeId rootKey))
                , case IntMap.lookup nidInt nodesSolved of
                    Just TyVar{} -> True
                    _ -> False
                ]
        schemeInteriorOwnersBase =
            IntMap.fromListWith
                (\_ old -> old)
                [ (getNodeId nid, gid)
                | (rootKey, gid) <- IntMap.toList schemeRootOwnersFiltered
                , nid <- map NodeId (IntSet.toList (reachableFromWithBoundsBaseStop (NodeId rootKey)))
                , okRef (typeRef nid)
                , case IntMap.lookup (getNodeId nid) nodesSolved of
                    Just TyVar{} -> True
                    _ -> False
                ]
        schemeInteriorOwnersFiltered' =
            IntMap.union schemeInteriorOwnersFiltered schemeInteriorOwnersBase
        _ =
            debugGaScope
                ("constraintForGeneralization: schemeRootOwnersFiltered="
                    ++ show (IntMap.toList schemeRootOwnersFiltered)
                )
                ()
        allSchemeRootsFiltered =
            IntSet.fromList (IntMap.keys schemeRootOwnersFiltered)
        bindParentsWithParentOwners =
            let attachParent acc nidInt gid =
                    case IntMap.lookup nidInt nodesSolved of
                        Just TyVar{} ->
                            let childRef = typeRef (NodeId nidInt)
                                childKey = nodeRefKey childRef
                                setChildParent parentRef flag acc' =
                                    if okRef parentRef
                                        then IntMap.insertWith
                                            (\(parentNew, flagNew) (_parentOld, flagOld) ->
                                                (parentNew, max flagNew flagOld)
                                            )
                                            childKey
                                            (parentRef, flag)
                                            acc'
                                        else acc'
                            in case IntMap.lookup nidInt bindParentsWithCopies of
                                Just (GenRef gidParent, flag)
                                    | gidParent /= gid ->
                                        setChildParent (GenRef gid) flag acc
                                Just (TypeRef parentN, _flag) ->
                                    let parentRef = typeRef parentN
                                        parentKey = nodeRefKey parentRef
                                    in if okRef parentRef
                                        then
                                            case IntMap.lookup parentKey acc of
                                                Nothing ->
                                                    IntMap.insert parentKey (GenRef gid, BindFlex) acc
                                                Just (parentExisting, flag)
                                                    | nodeRefKey parentExisting == parentKey ->
                                                        IntMap.insert parentKey (GenRef gid, flag) acc
                                                    | otherwise -> acc
                                        else acc
                                Nothing ->
                                    setChildParent (GenRef gid) BindFlex acc
                                _ -> acc
                        _ -> acc
            in IntMap.foldlWithKey' attachParent bindParentsWithCopies schemeInteriorOwnersFiltered'
        baseFirstGenAncestor ref0 =
            let baseRef =
                    case ref0 of
                        GenRef gid -> GenRef gid
                        TypeRef nid ->
                            case IntMap.lookup (getNodeId nid) solvedToBase of
                                Just baseN -> typeRef baseN
                                Nothing -> typeRef nid
                go visited ref =
                    if IntSet.member (nodeRefKey ref) visited
                        then Nothing
                        else
                            case IntMap.lookup (nodeRefKey ref) bindParentsBase of
                                Nothing -> Nothing
                                Just (parentRef, _) ->
                                    case parentRef of
                                        GenRef gid -> Just gid
                                        TypeRef parentN ->
                                            go
                                                (IntSet.insert (nodeRefKey ref) visited)
                                                (typeRef parentN)
            in go IntSet.empty baseRef
        firstGenAncestorWith bindParents0 ref0 =
            firstGenAncestorFrom bindParents0 ref0
        dropSelfParents bp =
            IntMap.filterWithKey
                (\childKey (parentRef, _flag) ->
                    nodeRefKey parentRef /= childKey
                )
                bp
        mapBaseRef ref =
            case ref of
                GenRef gid -> Just (GenRef gid)
                TypeRef nid ->
                    case IntMap.lookup (getNodeId nid) baseToSolved of
                        Just solvedNid -> Just (TypeRef (canonical solvedNid))
                        Nothing ->
                            if IntMap.member (getNodeId nid) nodesSolved
                                then Just (TypeRef (canonical nid))
                                else Nothing
        restoreSchemeRootParentsFromBase bp0 =
            let baseRootAncestor root =
                    firstGenAncestorWith bindParentsBase (typeRef root)
                attachOne acc root =
                    case baseRootAncestor root of
                        Just gid ->
                            case IntMap.lookup (getNodeId root) baseToSolved of
                                Just solvedRoot ->
                                    let childKey = nodeRefKey (typeRef solvedRoot)
                                    in IntMap.insert childKey (GenRef gid, BindFlex) acc
                                Nothing -> acc
                        Nothing -> acc
            in foldl' attachOne bp0 schemeRootsBase
        restoreSchemeRootParentsByBody bp0 =
            let attachOne acc rootKey =
                    let root = NodeId rootKey
                    in case VarStore.lookupVarBound upperConstraint root of
                        Just bnd ->
                            let bodyRef = typeRef (canonical bnd)
                            in case firstGenAncestorWith acc bodyRef of
                                Just gid ->
                                    IntMap.insert (nodeRefKey (typeRef root)) (GenRef gid, BindFlex) acc
                                Nothing -> acc
                        Nothing -> acc
            in foldl' attachOne bp0 (IntSet.toList allSchemeRoots)
        preserveBaseGaPrime bp0 =
            let insertEdge acc childRef parentRef flag =
                    let childKey = nodeRefKey childRef
                    in if not (okRef childRef && okRef parentRef)
                        then acc
                    else if nodeRefKey childRef == nodeRefKey parentRef
                        then acc
                    else if not (isUpperRef parentRef childRef)
                        then acc
                    else IntMap.insert childKey (parentRef, flag) acc
                prefixToGen gid =
                    let go acc [] = reverse acc
                        go acc (ref:rest) =
                            let acc' = ref : acc
                            in case ref of
                                GenRef gid' | gid' == gid -> reverse acc'
                                _ -> go acc' rest
                    in go []
                applyPath acc path =
                    foldl'
                        (\acc' (childRef, parentRef) ->
                            case (mapBaseRef childRef, mapBaseRef parentRef) of
                                (Just childRef', Just parentRef') ->
                                    case IntMap.lookup (nodeRefKey childRef) bindParentsBase of
                                        Just (_parentBase, flag) ->
                                            insertEdge acc' childRef' parentRef' flag
                                        Nothing -> acc'
                                _ -> acc'
                        )
                        acc
                        (zip path (drop 1 path))
                applyForChild acc childKey _ =
                    case nodeRefFromKey childKey of
                        GenRef _ -> acc
                        TypeRef childN ->
                            let baseRef = typeRef childN
                                baseAncestor = firstGenAncestorWith bindParentsBase baseRef
                                solvedRef = mapBaseRef baseRef
                                solvedAncestor =
                                    case solvedRef of
                                        Just ref -> firstGenAncestorWith acc ref
                                        Nothing -> Nothing
                                childIsCopy =
                                    case solvedRef of
                                        Just (TypeRef nid) -> IntMap.member (getNodeId nid) instCopyMap
                                        _ -> False
                            in if childIsCopy
                                then acc
                                else
                                    case (baseAncestor, solvedRef) of
                                        (Just gidBase, Just _) ->
                                            if solvedAncestor == Just gidBase
                                                then acc
                                                else
                                                    case bindingPathToRootLocal bindParentsBase baseRef of
                                                        Left _ -> acc
                                                        Right path ->
                                                            let prefix = prefixToGen gidBase path
                                                            in if null prefix
                                                                then acc
                                                                else applyPath acc prefix
                                        _ -> acc
            in IntMap.foldlWithKey' applyForChild bp0 bindParentsBase
        bindParents'' =
            let
                forceSchemeRootParents bp =
                    foldl'
                        (\acc gen ->
                            let gid = gnId gen
                            in foldl'
                                (\acc' root ->
                                    let rootC = canonical root
                                        rootRef = typeRef rootC
                                        rootKey = nodeRefKey rootRef
                                        accWithRoot =
                                            if okRef rootRef
                                                then
                                                    case IntMap.lookup rootKey acc' of
                                                        Nothing -> IntMap.insert rootKey (GenRef gid, BindFlex) acc'
                                                        Just (parentExisting, flag)
                                                            | parentExisting == GenRef gid -> acc'
                                                            | nodeRefKey parentExisting == rootKey ->
                                                                IntMap.insert rootKey (GenRef gid, flag) acc'
                                                            | otherwise ->
                                                                IntMap.insert rootKey (GenRef gid, flag) acc'
                                                else acc'
                                    in case IntMap.lookup (getNodeId rootC) nodesSolved of
                                        Just TyVar{ tnBound = Just bnd } ->
                                            let bndRef = typeRef bnd
                                                bndKey = nodeRefKey bndRef
                                            in if okRef bndRef
                                                then
                                                    case IntMap.lookup bndKey accWithRoot of
                                                        Nothing ->
                                                            IntMap.insert bndKey (rootRef, BindFlex) accWithRoot
                                                        Just (parentExisting, flag)
                                                            | nodeRefKey parentExisting == bndKey ->
                                                                IntMap.insert bndKey (rootRef, flag) accWithRoot
                                                            | otherwise -> accWithRoot
                                                else accWithRoot
                                        _ -> accWithRoot
                                )
                                acc
                                (gnSchemes gen)
                        )
                        bp
                        (IntMap.elems genMerged)
                restoreBoundParents bp =
                    IntMap.foldlWithKey'
                        (\acc _key node ->
                            case node of
                                TyVar{ tnId = vid, tnBound = Just bnd } ->
                                    let varRef = typeRef (canonical vid)
                                        bndRef = typeRef (canonical bnd)
                                        bndKey = nodeRefKey bndRef
                                    in if okRef varRef && okRef bndRef
                                        then
                                            case IntMap.lookup bndKey acc of
                                                Nothing ->
                                                    IntMap.insert bndKey (varRef, BindFlex) acc
                                                Just (parentExisting, flag)
                                                    | nodeRefKey parentExisting == bndKey ->
                                                        IntMap.insert bndKey (varRef, flag) acc
                                                    | otherwise -> acc
                                        else acc
                                _ -> acc
                        )
                        bp
                        nodesSolved

                addMissingUnderGen bp gid root =
                    let rootC = canonical root
                        interior = reachableFromWithBoundsStop rootC
                    in IntSet.foldl'
                        (\acc nidInt ->
                            let child = NodeId nidInt
                                childKey = nodeRefKey (typeRef child)
                                baseKey =
                                    case IntMap.lookup nidInt solvedToBase of
                                        Just (NodeId key) -> key
                                        Nothing -> nidInt
                                sticky = IntSet.member baseKey stickyTypeParentsBase
                                owner = IntMap.lookup nidInt schemeInteriorOwnersFiltered'
                                baseOwner = baseFirstGenAncestor (typeRef child)
                                existing = IntMap.lookup childKey acc
                            in if sticky
                                then acc
                                else if IntSet.member nidInt instCopyNodes
                                then acc
                                else if IntMap.member childKey acc
                                then
                                    case existing of
                                        Just (parentExisting, flag)
                                            | nodeRefKey parentExisting == childKey ->
                                                case owner of
                                                    Just gid' | gid' /= gid -> acc
                                                    _ -> IntMap.insert childKey (GenRef gid, flag) acc
                                        _ -> acc
                                else if owner /= Nothing && owner /= Just gid
                                    then acc
                                else if baseOwner /= Nothing && baseOwner /= Just gid
                                    then acc
                                else
                                    case IntMap.lookup nidInt nodesSolved of
                                        Just TyVar{} ->
                                            IntMap.insert childKey (GenRef gid, BindFlex) acc
                                        _ -> acc
                        )
                        bp
                        interior
            in foldl'
                (\bp gen ->
                    let gid = gnId gen
                    in foldl' (\bp' root -> addMissingUnderGen bp' gid root) bp (gnSchemes gen)
                )
                (restoreBoundParents (forceSchemeRootParents bindParentsWithParentOwners))
                (IntMap.elems genMerged)
        bindParentsFinal0 =
            let applyBaseGenParent acc (childKey, (parent, flag)) =
                    case parent of
                        GenRef _ ->
                            case IntMap.lookup childKey acc of
                                Nothing -> IntMap.insert childKey (parent, flag) acc
                                Just (parent0, flag0)
                                    | nodeRefKey parent0 == childKey ->
                                        IntMap.insert childKey (parent, max flag0 flag) acc
                                    | parent0 == parent ->
                                        IntMap.insert childKey (parent0, max flag0 flag) acc
                                    | otherwise -> acc
                        _ -> acc
            in foldl' applyBaseGenParent bindParents'' (IntMap.toList bindParentsBase')
        forceBoundRootParents bp =
            IntSet.foldl'
                (\acc rootKey ->
                    case IntMap.lookup rootKey schemeRootOwnersFiltered of
                        Nothing -> acc
                        Just gid ->
                            let root = NodeId rootKey
                                interior = reachableFromWithBoundsStop root
                                step acc' nidInt =
                                    if IntSet.member nidInt instCopyNodes
                                        then acc'
                                        else case IntMap.lookup nidInt nodesSolved of
                                            Just TyVar{} ->
                                                let child = NodeId nidInt
                                                    childKey = nodeRefKey (typeRef child)
                                                    baseKey =
                                                        case IntMap.lookup nidInt solvedToBase of
                                                            Just (NodeId key) -> key
                                                            Nothing -> nidInt
                                                    sticky = IntSet.member baseKey stickyTypeParentsBase
                                                    owner = IntMap.lookup nidInt schemeRootOwnersFiltered
                                                    baseOwner = baseFirstGenAncestor (typeRef child)
                                                in if sticky
                                                    || (baseOwner /= Nothing && baseOwner /= Just gid)
                                                    then acc'
                                                    else
                                                        (case owner of
                                                            Just gid' | gid' /= gid -> acc'
                                                            _ ->
                                                                    case IntMap.lookup childKey acc' of
                                                                        Just (parentExisting, flag)
                                                                            | nodeRefKey parentExisting == childKey ->
                                                                                IntMap.insert childKey (GenRef gid, flag) acc'
                                                                            | otherwise -> acc'
                                                                        Nothing -> IntMap.insert childKey (GenRef gid, BindFlex) acc'
                                                        )
                                            _ -> acc'
                            in IntSet.foldl'
                                step
                                acc
                                interior
                )
                bp
                boundSchemeRoots
        overrideSchemeInteriorParentsWith schemeOwnerMap gens bp =
            foldl'
                (\acc gen ->
                    let gid = gnId gen
                    in foldl'
                        (\acc' root ->
                            let rootC = canonical root
                                interior = reachableFromWithBoundsStop rootC
                            in IntSet.foldl'
                                (\acc'' nidInt ->
                                    let isInstCopy = IntSet.member nidInt instCopyNodes
                                        ownerFinal = IntMap.lookup nidInt schemeOwnerMap
                                        ownerOk =
                                            case ownerFinal of
                                                Just gid' -> gid' == gid
                                                Nothing -> False
                                    in if isInstCopy && not ownerOk
                                        then acc''
                                        else case IntMap.lookup nidInt nodesSolved of
                                            Just TyVar{} ->
                                                case ownerFinal of
                                                    Just gid' | gid' /= gid -> acc''
                                                    _ ->
                                                        let childRef = typeRef (NodeId nidInt)
                                                            childKey = nodeRefKey childRef
                                                            currentOwner = firstGenAncestorWith acc'' childRef
                                                            shouldOverride =
                                                                case currentOwner of
                                                                    Just gid' -> gid' /= gid
                                                                    Nothing -> True
                                                            baseKey =
                                                                case IntMap.lookup nidInt solvedToBase of
                                                                    Just (NodeId key) -> key
                                                                    Nothing -> nidInt
                                                            sticky = IntSet.member baseKey stickyTypeParentsBase
                                                            baseOwner = baseFirstGenAncestor childRef
                                                            oldOwner = IntMap.lookup nidInt schemeInteriorOwnersFiltered'
                                                            ownerChanged =
                                                                case (oldOwner, ownerFinal) of
                                                                    (Just oldG, Just newG) -> oldG /= newG
                                                                    _ -> False
                                                            allowFromBase =
                                                                case baseOwner of
                                                                    Nothing -> True
                                                                    Just gidBase ->
                                                                        if gidBase == gid
                                                                            then True
                                                                            else case oldOwner of
                                                                                Just gidOld
                                                                                    | gidOld == gid -> True
                                                                                Just gidOld
                                                                                    | gidOld == gidBase && ownerChanged -> True
                                                                                _ -> False
                                                        in if sticky || not allowFromBase
                                                            then acc''
                                                            else if shouldOverride
                                                                then
                                                                    case IntMap.lookup childKey acc'' of
                                                                        Just (_parentExisting, flag) ->
                                                                            IntMap.insert childKey (GenRef gid, flag) acc''
                                                                        Nothing -> IntMap.insert childKey (GenRef gid, BindFlex) acc''
                                                            else
                                                                case IntMap.lookup childKey acc'' of
                                                                    Just (parentExisting, flag)
                                                                        | nodeRefKey parentExisting == childKey ->
                                                                            IntMap.insert childKey (GenRef gid, flag) acc''
                                                                        | otherwise -> acc''
                                                                    Nothing -> IntMap.insert childKey (GenRef gid, BindFlex) acc''
                                            _ -> acc''
                                )
                                acc'
                                interior
                        )
                        acc
                        (gnSchemes gen)
                )
                bp
                (IntMap.elems gens)
        bindParentsFinal0' =
            overrideSchemeInteriorParentsWith schemeInteriorOwnersFiltered' genMerged (forceBoundRootParents bindParentsFinal0)
        bindParentsFinal =
            foldl'
                (\acc gen ->
                    let gid = gnId gen
                    in foldl'
                        (\acc' root ->
                            let interior = reachableFromWithBoundsStop (canonical root)
                            in IntSet.foldl'
                                (\acc'' nidInt ->
                                    let isInstCopy = IntSet.member nidInt instCopyNodes
                                        owner = IntMap.lookup nidInt schemeInteriorOwnersFiltered'
                                        allowInstCopy = case owner of
                                            Just gid' -> gid' == gid
                                            Nothing -> False
                                    in if isInstCopy && not allowInstCopy
                                        then acc''
                                        else case IntMap.lookup nidInt nodesSolved of
                                            Just TyVar{} ->
                                                let child = NodeId nidInt
                                                    childKey = nodeRefKey (typeRef child)
                                                    baseKey =
                                                        case IntMap.lookup nidInt solvedToBase of
                                                            Just (NodeId key) -> key
                                                            Nothing -> nidInt
                                                    sticky = IntSet.member baseKey stickyTypeParentsBase
                                                    baseOwner = baseFirstGenAncestor (typeRef child)
                                                    baseParent = IntMap.lookup childKey bindParentsBase'
                                                    existing = IntMap.lookup childKey acc''
                                                    shouldPreserve =
                                                        case baseOwner of
                                                            Just gid' -> gid' /= gid
                                                            Nothing ->
                                                                case baseParent of
                                                                    Just (GenRef gid', _) -> gid' /= gid
                                                                    _ ->
                                                                        case existing of
                                                                            Just (GenRef gid', _) -> gid' /= gid
                                                                            _ -> False
                                                in if sticky
                                                    then acc''
                                                    else if owner /= Nothing && owner /= Just gid
                                                    then acc''
                                                    else if shouldPreserve
                                                    then acc''
                                                    else
                                                        case existing of
                                                            Nothing ->
                                                                IntMap.insert
                                                                    childKey
                                                                    (GenRef gid, BindFlex)
                                                                    acc''
                                                            Just (parentExisting, flag)
                                                                | nodeRefKey parentExisting == childKey ->
                                                                    IntMap.insert childKey (GenRef gid, flag) acc''
                                                                | otherwise -> acc''
                                            _ -> acc''
                                )
                                acc'
                                interior
                        )
                        acc
                        (gnSchemes gen)
                )
                bindParentsFinal0'
                (IntMap.elems genMerged)
        rebindSchemeBodyAliases bp =
            IntMap.foldlWithKey'
                (\acc nidInt node ->
                    case node of
                        TyVar{ tnBound = Just bnd }
                            | IntSet.member nidInt instCopyNodes ->
                            let bndC = canonical bnd
                            in case IntMap.lookup (getNodeId bndC) schemeRootByBodySolved of
                                Just root ->
                                    case IntMap.lookup (getNodeId (canonical root)) schemeRootOwnersFiltered of
                                        Just gid ->
                                            let childKey = nodeRefKey (typeRef (NodeId nidInt))
                                            in case IntMap.lookup childKey acc of
                                                Just (GenRef _, _) -> acc
                                                Just (TypeRef _, flag) ->
                                                    IntMap.insert childKey (GenRef gid, flag) acc
                                                Nothing ->
                                                    IntMap.insert childKey (GenRef gid, BindFlex) acc
                                        Nothing -> acc
                                Nothing -> acc
                        _ -> acc
                )
                bp
                nodesSolved
        rootGenId =
            case [gid | GenRef gid <- Binding.bindingRoots base] of
                [gid] -> gid
                _ ->
                    case IntMap.keys genMerged of
                        (k:_) -> GenNodeId k
                        [] -> GenNodeId 0
        attachOrphans bp =
            let constraint0 =
                    solvedConstraint
                        { cNodes = nodesSolved
                        , cGenNodes = genMerged
                        , cBindParents = bp
                        }
                roots = Binding.bindingRoots constraint0
                rootKey = nodeRefKey (GenRef rootGenId)
            in foldl'
                (\acc ref ->
                    if nodeRefKey ref == rootKey
                        then acc
                        else
                            let parent =
                                    case ref of
                                        GenRef _ -> GenRef rootGenId
                                        TypeRef nid ->
                                            case baseFirstGenAncestor (typeRef nid) of
                                                Just gid -> GenRef gid
                                                Nothing -> GenRef rootGenId
                            in IntMap.insert (nodeRefKey ref) (parent, BindFlex) acc
                )
                bp
                roots
        bindParentsFinalRebound = rebindSchemeBodyAliases bindParentsFinal
        bindParentsFinal' =
            let msg =
                    "constraintForGeneralization: scheme interiors="
                        ++ show
                            [ (gnId gen, root, map (\nid -> (nid, IntMap.lookup (nodeRefKey (typeRef (NodeId nid))) bindParentsFinal))
                                    [ nidInt
                                    | nidInt <- IntSet.toList (reachableFromWithBoundsStop (canonical root))
                                    , case IntMap.lookup nidInt nodesSolved of
                                        Just TyVar{} -> True
                                        _ -> False
                                    ])
                            | gen <- IntMap.elems genMerged
                            , root <- gnSchemes gen
                            ]
                probeId = NodeId 17
                probeKey = nodeRefKey (typeRef probeId)
                probeSolvedParent = IntMap.lookup probeKey bindParentsSolved
                probeSolvedToBase = IntMap.lookup (getNodeId probeId) solvedToBase
                probeInstCopy = IntSet.member (getNodeId probeId) instCopyNodes
                parentInfo = IntMap.lookup (nodeRefKey (typeRef probeId)) bindParentsFinal
                msgProbe =
                    "constraintForGeneralization: probe bind-parent node="
                        ++ show probeId
                        ++ " parent="
                        ++ show parentInfo
                        ++ " solvedParent="
                        ++ show probeSolvedParent
                        ++ " solvedToBase="
                        ++ show probeSolvedToBase
                        ++ " instCopy="
                        ++ show probeInstCopy
            in debugGaScope msgProbe (debugGaScope msg bindParentsFinalRebound)
        bindParentsFinal'' = attachOrphans bindParentsFinal'
        bindParentsFinalClean = dropSelfParents bindParentsFinal''
        restoreSchemeRootParentsFromOwners bp0 =
            IntMap.foldlWithKey'
                (\acc rootKey gid ->
                    let rootRef = typeRef (NodeId rootKey)
                        rootKeyRef = nodeRefKey rootRef
                    in if okRef rootRef
                        then
                            case IntMap.lookup rootKeyRef acc of
                                Just (_parentExisting, flag) ->
                                    IntMap.insert rootKeyRef (GenRef gid, flag) acc
                                Nothing -> IntMap.insert rootKeyRef (GenRef gid, BindFlex) acc
                        else acc
                )
                bp0
                schemeRootOwnersFiltered
        bindParentsFinalPreserved =
            restoreSchemeRootParentsFromOwners
                (restoreSchemeRootParentsByBody
                    (restoreSchemeRootParentsFromBase (preserveBaseGaPrime bindParentsFinalClean))
                )
        schemeRootsByGen =
            IntMap.fromListWith
                (\a b -> a ++ b)
                [ (genNodeKey gid, [NodeId rootKey])
                | rootKey <- IntSet.toList allSchemeRootsFiltered
                , Just (GenRef gid, _) <- [IntMap.lookup (nodeRefKey (typeRef (NodeId rootKey))) bindParentsFinalPreserved]
                ]
        genMerged' =
            IntMap.mapWithKey
                (\k gen ->
                    let roots =
                            case IntMap.lookup k schemeRootsByGen of
                                Just rs -> rs
                                Nothing -> []
                    in gen { gnSchemes = roots }
                )
                genMerged
        schemeRootOwnersFinal =
            IntMap.fromList
                [ (getNodeId (canonical root), gnId gen)
                | gen <- IntMap.elems genMerged'
                , root <- gnSchemes gen
                ]
        schemeInteriorOwnersFinal =
            IntMap.fromListWith
                (\_ old -> old)
                [ (nidInt, gid)
                | (rootKey, gid) <- IntMap.toList schemeRootOwnersFinal
                , nidInt <- IntSet.toList (reachableFromWithBoundsStop (NodeId rootKey))
                , case IntMap.lookup nidInt nodesSolved of
                    Just TyVar{} -> True
                    _ -> False
                ]
        bindParentsFinalAligned =
            overrideSchemeInteriorParentsWith
                schemeInteriorOwnersFinal
                genMerged'
                bindParentsFinalPreserved
        schemeRootsMerged' =
            [ (gnId gen, map canonical (gnSchemes gen))
            | gen <- IntMap.elems genMerged'
            ]
        constraintForGen =
            let restoreTypeParents acc =
                    IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, flag) ->
                            case (nodeRefFromKey childKey, parentRef) of
                                (TypeRef childBase, TypeRef _parentBase) ->
                                    case (mapBaseRef (typeRef childBase), mapBaseRef parentRef) of
                                        (Just childRef', Just parentRef') ->
                                            let childKey' = nodeRefKey childRef'
                                            in if not (okRef childRef' && okRef parentRef')
                                                then acc'
                                            else if nodeRefKey childRef' == nodeRefKey parentRef'
                                                then acc'
                                            else if not (isUpperRef parentRef' childRef')
                                                then acc'
                                            else
                                                case IntMap.lookup childKey' acc' of
                                                    Nothing -> IntMap.insert childKey' (parentRef', flag) acc'
                                                    Just (parentExisting, _flagExisting)
                                                        | nodeRefKey parentExisting == childKey' ->
                                                            IntMap.insert childKey' (parentRef', flag) acc'
                                                        | otherwise -> acc'
                                        _ -> acc'
                                _ -> acc'
                        )
                        acc
                        bindParentsBase
                bindParentsFinalAligned' = restoreTypeParents bindParentsFinalAligned
                rootGenIdBase =
                    case [gid | GenRef gid <- Binding.bindingRoots base] of
                        [gid] -> gid
                        _ ->
                            case IntMap.keys genMerged of
                                (k:_) -> GenNodeId k
                                [] -> GenNodeId 0
                bindParentsFinalAligned'' =
                    IntMap.foldlWithKey'
                        (\acc copyKey baseN ->
                            let childRef = typeRef (NodeId copyKey)
                                childRef' = adoptRef childRef
                                childKey' = nodeRefKey childRef'
                            in case IntMap.lookup (nodeRefKey (typeRef baseN)) bindParentsBase of
                                Just (GenRef gid, flag)
                                    | gid == rootGenIdBase
                                    , okRef childRef' ->
                                        IntMap.insert childKey' (GenRef gid, flag) acc
                                _ -> acc
                        )
                        bindParentsFinalAligned'
                        instCopyMap
                pruneBindParents c =
                    let liveNodes = cNodes c
                        liveGens = cGenNodes c
                        liveChild childKey =
                            case nodeRefFromKey childKey of
                                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
                        liveParent ref =
                            case ref of
                                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
                        bindParentsPruned =
                            IntMap.filterWithKey
                                (\childKey (parentRef, _flag) ->
                                    liveChild childKey && liveParent parentRef
                                )
                                (cBindParents c)
                    in c { cBindParents = bindParentsPruned }
                constraint0 = solvedConstraint { cNodes = nodesSolved, cBindParents = bindParentsFinalAligned'', cGenNodes = genMerged' }
                constraint1 = pruneBindParents constraint0
                probeIds = [NodeId 1, NodeId 2, NodeId 3]
                probeInfo =
                    [ ( pid
                      , IntMap.lookup (getNodeId pid) (cNodes constraint1)
                      , IntMap.lookup (nodeRefKey (typeRef pid)) (cBindParents constraint1)
                      )
                    | pid <- probeIds
                    ]
            in debugGaScope ("constraintForGeneralization: probe nodes " ++ show probeInfo) constraint1
        (qAlignSolvedToBase, qAlignBaseToSolved) =
            let canonicalBase = id
                alignOne (accSolved, accBase) gen =
                    let gid = gnId gen
                    in case
                        ( Binding.boundFlexChildrenUnder canonicalBase base (GenRef gid)
                        , Binding.boundFlexChildrenUnder canonical constraintForGen (GenRef gid)
                        ) of
                            (Right baseBinders, Right solvedBinders) ->
                                foldl'
                                    (\(accSolved', accBase') (solvedB, baseB) ->
                                        let solvedKey = getNodeId (canonical solvedB)
                                            baseKey = getNodeId baseB
                                        in ( IntMap.insertWith (\_ old -> old) solvedKey baseB accSolved'
                                           , IntMap.insertWith (\_ old -> old) baseKey (canonical solvedB) accBase'
                                           )
                                    )
                                    (accSolved, accBase)
                                    (zip solvedBinders baseBinders)
                            _ -> (accSolved, accBase)
            in foldl' alignOne (IntMap.empty, IntMap.empty) (IntMap.elems (cGenNodes base))
        baseToSolvedAligned = IntMap.union baseToSolved qAlignBaseToSolved
        solvedToBaseAligned0 =
            IntMap.foldlWithKey'
                (\acc baseKey solvedNid ->
                    let solvedKeyC = getNodeId (canonical solvedNid)
                        solvedKeyRaw = getNodeId solvedNid
                        acc' = IntMap.insertWith (\_ old -> old) solvedKeyC (NodeId baseKey) acc
                    in IntMap.insertWith (\_ old -> old) solvedKeyRaw (NodeId baseKey) acc'
                )
                IntMap.empty
                baseToSolvedAligned
        solvedToBaseAligned =
            let copyOverrides =
                    IntMap.fromList
                        [ (copyKeyC, baseN)
                        | (copyKey, baseN) <- IntMap.toList instCopyMap
                        , let copyKeyC = getNodeId (canonical (NodeId copyKey))
                        , IntMap.member copyKeyC nodesSolved
                        ]
            in IntMap.union copyOverrides (IntMap.union solvedToBaseAligned0 qAlignSolvedToBase)
    in debugGaScope
            ("constraintForGeneralization: merged gens="
                ++ show (map fst schemeRootsMerged')
                ++ " schemes="
                ++ show (map snd schemeRootsMerged')
            )
            ( constraintForGen
            , GaBindParents
                { gaBindParentsBase = bindParentsBase
                , gaBaseConstraint = base
                , gaBaseToSolved = baseToSolvedAligned
                , gaSolvedToBase = solvedToBaseAligned
                }
            )
