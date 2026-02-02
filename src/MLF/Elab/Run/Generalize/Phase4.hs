module MLF.Elab.Run.Generalize.Phase4 (
    computeSchemeOwnership
) where

import Data.Functor.Foldable (ListF(..), cata, hylo)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.BindingUtil (bindingPathToRootLocal, firstGenAncestorFrom)
import MLF.Constraint.Types
    ( BindFlag(..)
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
    , typeRef
    )
import qualified MLF.Constraint.Types as Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Run.Debug (debugGaScope)
import MLF.Elab.Run.Generalize.Common
    ( applyBindParent
    , baseFirstGenAncestorWith
    , childrenFrom
    , chooseRootGenId
    , flagFromExisting
    , insertBindParent
    , isTyVarAt
    , mapBaseRefWith
    , mkAllowBindEdge
    , mkIsUpperRef
    , mkOkRef
    , nodeMapToIntMap
    , ownerIsOther
    )
import MLF.Elab.Run.Generalize.Types
    ( GeneralizeEnv(..)
    , InsertMode(..)
    , NodeMapping(..)
    , Phase1Result(..)
    , Phase2Result(..)
    , Phase3Result(..)
    , Phase4Result(..)
    )
import MLF.Util.Graph (reachableFromStop)
import MLF.Util.IntMapUtils (keepOld)

computeSchemeOwnership :: GeneralizeEnv -> Phase1Result -> Phase2Result -> Phase3Result -> Phase4Result
computeSchemeOwnership env phase1 phase2 phase3 =
    let base = geBaseConstraint env
        solvedConstraint = geSolvedConstraint env
        baseNodes = nodeMapToIntMap (cNodes base)
        nodesSolved = p1NodesSolved phase1
        schemeRootsBase = p1SchemeRootsBase phase1
        schemeRootsBaseSet = p1SchemeRootsBaseSet phase1
        schemeRootsAllSet = p1SchemeRootsAllSet phase1
        genMerged = p2GenMerged phase2
        bindParentsBase = p2BindParentsBase phase2
        bindParentsBase' = p3BindParentsBaseAdjusted phase3
        bindParentsWithCopies = p3BindParentsWithCopies phase3
        stickyTypeParentsBase = p2StickyTypeParentsBase phase2
        instCopyNodes = geInstCopyNodes env
        instCopyMap = geInstCopyMap env
        NodeMapping { mapBaseToSolved = baseToSolved, mapSolvedToBase = solvedToBase } = p2NodeMapping phase2
        canonical = geCanonical env
        adoptNodeId = geAdoptNodeId env
        debug msg = debugGaScope (geTraceConfig env) ("constraintForGeneralization: " ++ msg)
        upperConstraint = solvedConstraint { cNodes = Types.NodeMap nodesSolved }
        okRef = mkOkRef nodesSolved genMerged
        isUpperRef = mkIsUpperRef upperConstraint
        allowBindEdge = mkAllowBindEdge okRef isUpperRef
        baseFirstGenAncestor = baseFirstGenAncestorWith solvedToBase bindParentsBase
        mapBaseRef = mapBaseRefWith canonical baseToSolved nodesSolved
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
                children nid = childrenFrom nodesSolved (getNodeId nid)
            in reachableFromStop getNodeId canonical children shouldStop start
        reachableFromWithBoundsBaseStop start =
            let stopSet = schemeRootsBaseSet
                startKey = getNodeId start
                isStop nid =
                    let key = getNodeId nid
                    in key /= startKey && IntSet.member key stopSet
                alg Nil = IntSet.empty
                alg (Cons nid acc)
                    | isStop nid = acc
                    | otherwise = IntSet.insert (getNodeId (adoptNodeId nid)) acc
                coalg (visited, queue) =
                    case queue of
                        [] -> Nil
                        (nid0:rest) ->
                            let key = getNodeId nid0
                                visited' = IntSet.insert key visited
                            in if IntSet.member key visited
                                then Cons nid0 (visited, rest)
                                else if isStop nid0
                                    then Cons nid0 (visited', rest)
                                    else
                                        let kids = childrenFrom baseNodes key
                                        in Cons nid0 (visited', kids ++ rest)
            in hylo alg coalg (IntSet.empty, [start])
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
                const
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
                keepOld
                [ (nidInt, gid)
                | (rootKey, gid) <- IntMap.toList schemeRootOwnersFiltered
                , nidInt <- IntSet.toList (reachableFromWithBoundsStop (NodeId rootKey))
                , isTyVarAt nodesSolved nidInt
                ]
        schemeInteriorOwnersBase =
            IntMap.fromListWith
                keepOld
                [ (getNodeId nid, gid)
                | (rootKey, gid) <- IntMap.toList schemeRootOwnersFiltered
                , nid <- map NodeId (IntSet.toList (reachableFromWithBoundsBaseStop (NodeId rootKey)))
                , okRef (typeRef nid)
                , isTyVarAt nodesSolved (getNodeId nid)
                ]
        schemeInteriorOwnersFiltered' =
            IntMap.union schemeInteriorOwnersFiltered schemeInteriorOwnersBase
        _ =
            debug
                ("schemeRootOwnersFiltered="
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
        firstGenAncestorWith = firstGenAncestorFrom
        dropSelfParents =
            IntMap.filterWithKey
                (\childKey (parentRef, _flag) ->
                    nodeRefKey parentRef /= childKey
                )
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
                    applyBindParent allowBindEdge Override childRef parentRef flag acc
                prefixToGen gid =
                    let isTarget ref =
                            case ref of
                                GenRef gid' -> gid' == gid
                                _ -> False
                        alg Nil = (False, [])
                        alg (Cons ref (found, acc))
                            | isTarget ref = (True, [ref])
                            | otherwise = (found, ref : acc)
                    in snd . cata alg
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
                                                        Just (parentExisting, _flag)
                                                            | parentExisting == GenRef gid -> acc'
                                                        _ ->
                                                            insertBindParent
                                                                Override
                                                                (GenRef gid)
                                                                (flagFromExisting BindFlex rootKey acc')
                                                                rootKey
                                                                acc'
                                                else acc'
                                    in case IntMap.lookup (getNodeId rootC) nodesSolved of
                                        Just TyVar{ tnBound = Just bnd } ->
                                            let bndRef = typeRef bnd
                                                bndKey = nodeRefKey bndRef
                                            in if okRef bndRef
                                                then
                                                    insertBindParent
                                                        SelfOrEmpty
                                                        rootRef
                                                        (flagFromExisting BindFlex bndKey accWithRoot)
                                                        bndKey
                                                        accWithRoot
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
                                            insertBindParent
                                                SelfOrEmpty
                                                varRef
                                                (flagFromExisting BindFlex bndKey acc)
                                                bndKey
                                                acc
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
                                ownerMismatch = ownerIsOther gid owner
                                baseOwnerMismatch = ownerIsOther gid baseOwner
                            in if sticky || IntSet.member nidInt instCopyNodes
                                then acc
                                else case IntMap.lookup childKey acc of
                                    Just (parentExisting, _flag)
                                        | nodeRefKey parentExisting == childKey ->
                                            if ownerMismatch
                                                then acc
                                                else
                                                    insertBindParent
                                                        SelfOrEmpty
                                                        (GenRef gid)
                                                        (flagFromExisting BindFlex childKey acc)
                                                        childKey
                                                        acc
                                    Just _ -> acc
                                    Nothing
                                        | ownerMismatch || baseOwnerMismatch -> acc
                                        | otherwise ->
                                            case IntMap.lookup nidInt nodesSolved of
                                                Just TyVar{} ->
                                                    insertBindParent
                                                        SelfOrEmpty
                                                        (GenRef gid)
                                                        (flagFromExisting BindFlex childKey acc)
                                                        childKey
                                                        acc
                                                _ -> acc
                        )
                        bp
                        interior
            in foldl'
                (\bp gen ->
                    let gid = gnId gen
                    in foldl' (`addMissingUnderGen` gid) bp (gnSchemes gen)
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
                                                in if sticky || ownerIsOther gid baseOwner || ownerIsOther gid owner
                                                    then acc'
                                                    else
                                                        insertBindParent
                                                            SelfOrEmpty
                                                            (GenRef gid)
                                                            (flagFromExisting BindFlex childKey acc')
                                                            childKey
                                                            acc'
                                            _ -> acc'
                            in IntSet.foldl'
                                step
                                acc
                                interior
                )
                bp
                boundSchemeRoots
        foldSchemeInteriors gens acc0 step =
            foldl'
                (\acc gen ->
                    let gid = gnId gen
                    in foldl'
                        (\acc' root ->
                            let interior = reachableFromWithBoundsStop (canonical root)
                            in IntSet.foldl' (step gid) acc' interior
                        )
                        acc
                        (gnSchemes gen)
                )
                acc0
                (IntMap.elems gens)
        overrideSchemeInteriorParentsWith schemeOwnerMap gens bp =
            foldSchemeInteriors gens bp $ \gid acc nidInt ->
                let isInstCopy = IntSet.member nidInt instCopyNodes
                    ownerFinal = IntMap.lookup nidInt schemeOwnerMap
                    ownerOk = ownerFinal == Just gid
                in if isInstCopy && not ownerOk
                    then acc
                    else case IntMap.lookup nidInt nodesSolved of
                        Just TyVar{} ->
                            case ownerFinal of
                                Just gid' | gid' /= gid -> acc
                                _ ->
                                    let childRef = typeRef (NodeId nidInt)
                                        childKey = nodeRefKey childRef
                                        currentOwner = firstGenAncestorWith acc childRef
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
                                                    gidBase == gid
                                                        || case oldOwner of
                                                            Just gidOld
                                                                | gidOld == gid -> True
                                                            Just gidOld
                                                                | gidOld == gidBase && ownerChanged -> True
                                                            _ -> False
                                    in if sticky || not allowFromBase
                                        then acc
                                        else if shouldOverride
                                            then
                                                insertBindParent
                                                    Override
                                                    (GenRef gid)
                                                    (flagFromExisting BindFlex childKey acc)
                                                    childKey
                                                    acc
                                        else
                                            insertBindParent
                                                SelfOrEmpty
                                                (GenRef gid)
                                                (flagFromExisting BindFlex childKey acc)
                                                childKey
                                                acc
                        _ -> acc
        bindParentsFinal0' =
            overrideSchemeInteriorParentsWith schemeInteriorOwnersFiltered' genMerged (forceBoundRootParents bindParentsFinal0)
        bindParentsFinal =
            foldSchemeInteriors genMerged bindParentsFinal0' $ \gid acc nidInt ->
                let isInstCopy = IntSet.member nidInt instCopyNodes
                    owner = IntMap.lookup nidInt schemeInteriorOwnersFiltered'
                    allowInstCopy = owner == Just gid
                in if isInstCopy && not allowInstCopy
                    then acc
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
                                existing = IntMap.lookup childKey acc
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
                            in if sticky || ownerIsOther gid owner || shouldPreserve
                                then acc
                                else
                                    insertBindParent
                                        SelfOrEmpty
                                        (GenRef gid)
                                        (flagFromExisting BindFlex childKey acc)
                                        childKey
                                        acc
                        _ -> acc
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
        rootGenIdBase = chooseRootGenId base genMerged
        rootGenId = rootGenIdBase
        attachOrphans bp =
            let constraint0 =
                    solvedConstraint
                        { cNodes = Types.NodeMap nodesSolved
                        , cGenNodes = Types.GenNodeMap genMerged
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
        bindParentsFinal' = debug
            ("scheme interiors=" ++ show
                [ (gnId gen, root,
                    [ (nid, IntMap.lookup (nodeRefKey (typeRef (NodeId nid))) bindParentsFinal)
                    | nid <- IntSet.toList (reachableFromWithBoundsStop (canonical root))
                    , isTyVarAt nodesSolved nid
                    ])
                | gen <- IntMap.elems genMerged
                , root <- gnSchemes gen
                ])
            bindParentsFinalRebound
        bindParentsFinal'' = attachOrphans bindParentsFinal'
        bindParentsFinalClean = dropSelfParents bindParentsFinal''
        restoreSchemeRootParentsFromOwners bp0 =
            IntMap.foldlWithKey'
                (\acc rootKey gid ->
                    let rootRef = typeRef (NodeId rootKey)
                        rootKeyRef = nodeRefKey rootRef
                    in if okRef rootRef
                        then
                            insertBindParent
                                Override
                                (GenRef gid)
                                (flagFromExisting BindFlex rootKeyRef acc)
                                rootKeyRef
                                acc
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
                (++)
                [ (genNodeKey gid, [NodeId rootKey])
                | rootKey <- IntSet.toList allSchemeRootsFiltered
                , Just (GenRef gid, _) <- [IntMap.lookup (nodeRefKey (typeRef (NodeId rootKey))) bindParentsFinalPreserved]
                ]
        genMerged' =
            IntMap.mapWithKey
                (\k gen ->
                    let roots = IntMap.findWithDefault [] k schemeRootsByGen
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
                keepOld
                [ (nidInt, gid)
                | (rootKey, gid) <- IntMap.toList schemeRootOwnersFinal
                , nidInt <- IntSet.toList (reachableFromWithBoundsStop (NodeId rootKey))
                , isTyVarAt nodesSolved nidInt
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
    in Phase4Result
        { p4BindParentsFinalAligned = bindParentsFinalAligned
        , p4GenMerged = genMerged'
        , p4SchemeRootsMerged = schemeRootsMerged'
        , p4RootGenIdBase = rootGenIdBase
        }
