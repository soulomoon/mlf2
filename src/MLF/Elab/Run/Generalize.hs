module MLF.Elab.Run.Generalize (
    pruneBindParentsConstraint,
    instantiationCopyNodes,
    constraintForGeneralization,
    generalizeAtWithBuilder
) where

import Data.Functor.Foldable (ListF(..), cata, hylo)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionPlanBuilder(..)
    )
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
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Util.IntMapUtils as IntMapUtils
import MLF.Elab.Generalize (GaBindParents(..), applyGeneralizePlan)
import MLF.Constraint.BindingUtil (bindingPathToRootLocal, firstGenAncestorFrom)
import MLF.Elab.Run.Debug (debugGaScope)
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Util.Graph (reachableFromStop)
import MLF.Frontend.ConstraintGen (AnnExpr)
import MLF.Elab.Types (ElabScheme)
import MLF.Util.ElabError (ElabError)

type NodeKey = Int
type NodeKeySet = IntSet.IntSet
type NodeMap = IntMap.IntMap TyNode
type GenMap = IntMap.IntMap GenNode
type BindParents = IntMap.IntMap (NodeRef, BindFlag)

data NodeMapping = NodeMapping
    { mapBaseToSolved :: IntMap.IntMap NodeId
    , mapSolvedToBase :: IntMap.IntMap NodeId
    }

data BindParentPolicy = BindParentPolicy
    { bppAllow :: NodeRef -> NodeRef -> Bool
    , bppInsert :: NodeRef -> BindFlag -> NodeKey -> BindParents -> BindParents
    }

applyBindParent :: BindParentPolicy -> NodeRef -> NodeRef -> BindFlag -> BindParents -> BindParents
applyBindParent policy childRef parentRef flag acc
    | bppAllow policy childRef parentRef =
        bppInsert policy parentRef flag (nodeRefKey childRef) acc
    | otherwise = acc

schemeRootsOf :: GenMap -> [NodeId]
schemeRootsOf = foldMap gnSchemes

childrenFrom :: NodeMap -> NodeKey -> [NodeId]
childrenFrom nodes key =
    maybe
        []
        structuralChildrenWithBounds
        (IntMap.lookup key nodes)

isTyVarAt :: NodeMap -> NodeKey -> Bool
isTyVarAt nodes key =
    case IntMap.lookup key nodes of
        Just TyVar{} -> True
        _ -> False

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

instantiationCopyNodes :: SolveResult -> IntMap.IntMap NodeId -> IntMap.IntMap EdgeTrace -> NodeKeySet
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
                interiorRaw = IntSet.toList (etInterior tr)
                interiorCanon =
                    [ getNodeId (adoptNode (NodeId nid))
                    | nid <- IntSet.toList (etInterior tr)
                    ]
                rootRaw = getNodeId (etRoot tr)
                rootCanon = getNodeId (adoptNode (etRoot tr))
            in IntSet.fromList (rootRaw : rootCanon : copyRaw ++ copyCanon ++ interiorRaw ++ interiorCanon)
    in IntSet.unions (map collectTrace (IntMap.elems edgeTraces))

constraintForGeneralization :: SolveResult -> IntMap.IntMap NodeId -> NodeKeySet -> IntMap.IntMap NodeId -> Constraint -> AnnExpr -> (Constraint, GaBindParents)
constraintForGeneralization solved redirects instCopyNodes instCopyMap base _ann =
    let solvedConstraint = srConstraint solved
        canonical = frWith (srUnionFind solved)
        nodesSolved0 = cNodes solvedConstraint
        baseNodes = cNodes base
        -- Phase 1: scheme roots and node restoration.
        schemeRootsBase = schemeRootsOf (cGenNodes base)
        preferBaseVar new old =
            case (new, old) of
                (TyVar{ tnBound = Nothing }, TyVar{ tnBound = Just _ }) -> old
                (TyVar{}, TyVar{}) -> new
                _ -> old
        insertBaseVarWith adoptId adoptBound acc key =
            case IntMap.lookup key baseNodes of
                Just TyVar{ tnId = baseId, tnBound = mb } ->
                    let baseId' = adoptId baseId
                        node' = TyVar { tnId = baseId', tnBound = fmap adoptBound mb }
                    in IntMap.insertWith
                        preferBaseVar
                        (getNodeId baseId')
                        node'
                        acc
                _ -> acc
        reachableFromWithBoundsBase start =
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
                                        kids = childrenFrom baseNodes key
                                    in Cons nid0 (visited', kids ++ rest)
            in hylo alg coalg (IntSet.empty, [start])
        restoreSchemeRoot acc root =
            let mbBase = do
                    TyVar{ tnBound = mb } <- IntMap.lookup (getNodeId root) baseNodes
                    bnd <- mb
                    case adoptRef (typeRef bnd) of
                        TypeRef bnd' -> Just bnd'
                        GenRef _ -> Nothing
                insertRoot bnd' =
                    IntMap.insert (getNodeId root) (TyVar { tnId = root, tnBound = Just bnd' }) acc
                fillMissing nid bnd' =
                    IntMap.insert (getNodeId root) (TyVar { tnId = nid, tnBound = Just bnd' }) acc
            in case IntMap.lookup (getNodeId root) acc of
                Nothing -> maybe acc insertRoot mbBase
                Just TyVar{ tnId = nid, tnBound = Nothing } ->
                    maybe acc (fillMissing nid) mbBase
                Just _ -> acc
        (schemeRootsBaseSet, schemeRootsAllSet) =
            let rootsSolved = schemeRootsOf (cGenNodes solvedConstraint)
                baseSet = IntSet.fromList (map getNodeId schemeRootsBase)
                solvedSet = IntSet.fromList (map (getNodeId . canonical) rootsSolved)
            in (baseSet, IntSet.union baseSet solvedSet)
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
                        baseNodes
                restoreNamedVars acc =
                    let insertNamed acc' childKey _parentRef =
                            insertBaseVarWith id adoptNodeId acc' childKey
                    in IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, _flag) ->
                            insertNamed acc' childKey parentRef
                        )
                        acc
                        (cBindParents base)
                restoreSchemeInteriorVars acc =
                    let
                        schemeInteriorsBase =
                            IntSet.unions
                                [ reachableFromWithBoundsBase root
                                | gen <- NodeAccess.allGenNodes base
                                , root <- gnSchemes gen
                                ]
                        insertVarFromBase acc' key =
                            if IntSet.member key schemeRootsBaseSet
                                then acc'
                                else insertBaseVarWith adoptNodeId adoptNodeId acc' key
                    in IntSet.foldl' insertVarFromBase acc schemeInteriorsBase
                restoreBindParentVars acc =
                    let
                        parentKeys = map getNodeId (IntMapUtils.typeParentNodes (cBindParents base))
                        childKeys = map getNodeId (IntMapUtils.typeChildNodes (cBindParents base))
                        keys = IntSet.fromList (parentKeys ++ childKeys)
                        insertVarFromBase =
                            insertBaseVarWith adoptNodeId adoptNodeId
                    in IntSet.foldl' insertVarFromBase acc keys
            in restoreBindParentVars (restoreSchemeInteriorVars (restoreNamedVars nodesSolvedBaseAdjusted))
        -- Phase 2: base/solved mappings and merged gen nodes.
        genSolved = cGenNodes solvedConstraint
        genMerged = IntMap.union (cGenNodes base) genSolved
        applyRedirectsToRef ref =
            case ref of
                TypeRef nid -> TypeRef (chaseRedirects redirects nid)
                GenRef gid -> GenRef gid
        canonicalRef = Canonicalize.canonicalRef canonical
        adoptRef = canonicalRef . applyRedirectsToRef
        adoptNodeId nid =
            case adoptRef (typeRef nid) of
                TypeRef nid' -> nid'
                GenRef _ -> nid
        keepOld _ old = old
        chooseRootGenId constraint gens =
            case [gid | GenRef gid <- Binding.bindingRoots constraint] of
                [gid] -> gid
                _ ->
                    case IntMap.keys gens of
                        (k:_) -> GenNodeId k
                        [] -> GenNodeId 0
        ownerIsOther gid = maybe False (/= gid)
        insertParentPreserveFlag parentRef childKey acc =
            case IntMap.lookup childKey acc of
                Just (_parentExisting, flag) ->
                    IntMap.insert childKey (parentRef, flag) acc
                Nothing ->
                    IntMap.insert childKey (parentRef, BindFlex) acc
        insertParentIfSelfOrEmpty parentRef childKey acc =
            case IntMap.lookup childKey acc of
                Just (parentExisting, flag)
                    | nodeRefKey parentExisting == childKey ->
                        IntMap.insert childKey (parentRef, flag) acc
                    | otherwise -> acc
                Nothing ->
                    IntMap.insert childKey (parentRef, BindFlex) acc
        insertParentIfSelfOrEmptyWith parentRef flag childKey acc =
            case IntMap.lookup childKey acc of
                Nothing ->
                    IntMap.insert childKey (parentRef, flag) acc
                Just (parentExisting, _flagExisting)
                    | nodeRefKey parentExisting == childKey ->
                        IntMap.insert childKey (parentRef, flag) acc
                    | otherwise -> acc
        insertGenParentPreserveFlag gid =
            insertParentPreserveFlag (GenRef gid)
        insertGenParentIfSelfOrEmpty gid =
            insertParentIfSelfOrEmpty (GenRef gid)
        allowBindEdge childRef parentRef =
            okRef childRef
                && okRef parentRef
                && nodeRefKey childRef /= nodeRefKey parentRef
                && isUpperRef parentRef childRef
        policyKeepOld =
            BindParentPolicy
                { bppAllow = allowBindEdge
                , bppInsert = \parentRef flag childKey acc ->
                    IntMap.insertWith keepOld childKey (parentRef, flag) acc
                }
        policyInsert =
            BindParentPolicy
                { bppAllow = allowBindEdge
                , bppInsert = \parentRef flag childKey acc ->
                    IntMap.insert childKey (parentRef, flag) acc
                }
        policySelfOrEmpty =
            BindParentPolicy
                { bppAllow = allowBindEdge
                , bppInsert = insertParentIfSelfOrEmptyWith
                }
        debug msg = debugGaScope ("constraintForGeneralization: " ++ msg)
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
        -- Phase 3: bind parents (base, solved, and copies).
        bindParentsBase = cBindParents base
        stickyTypeParentsBase =
            IntSet.fromList
                [ childKey
                | (childKey, _parent, _flag) <- IntMapUtils.childrenWithTypeParent bindParentsBase
                ]
        baseNamedKeys =
            IntSet.fromList
                [ childKey
                | (childKey, child) <- IntMapUtils.allTypeChildrenWithKey bindParentsBase
                , isTyVarAt baseNodes (getNodeId child)
                ]
        bindParentsSolved = cBindParents solvedConstraint
        insertBindParentBase acc childKey parentRef flag =
            let childRef = nodeRefFromKey childKey
                childRef' = adoptRef childRef
                parentRef' = adoptRef parentRef
                allowParent =
                    case parentRef of
                        TypeRef _ -> True
                        GenRef _ -> isUpperRef parentRef' childRef'
            in if allowParent
                then applyBindParent policyKeepOld childRef' parentRef' flag acc
                else acc

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
                overrideCopy =
                    debug
                        ("bind-parent override copy child="
                            ++ show childRef'
                            ++ " parent="
                            ++ show parentRef'
                            ++ " redirected="
                            ++ show (wasRedirected childRef)
                        )
                        (IntMap.insert childKey' (parentRef', flag) acc)
                fillChild =
                    debug
                        ("bind-parent fill child="
                            ++ show childRef'
                            ++ " parent="
                            ++ show parentRef'
                            ++ " redirected="
                            ++ show (wasRedirected childRef)
                        )
                        (IntMap.insert childKey' (parentRef', flag) acc)
                overrideSelf =
                    debug
                        ("bind-parent override self-parent child="
                            ++ show childRef'
                            ++ " parent="
                            ++ show parentRef'
                            ++ " redirected="
                            ++ show (wasRedirected childRef)
                        )
                        (IntMap.insert childKey' (parentRef', flag) acc)
            in case () of
                _ | not (okRef childRef' && okRef parentRef') -> acc
                  | isSelf -> acc
                  | not (isUpperRef parentRef' childRef') -> acc
                  | otherwise ->
                        case existing of
                            Just _ | childIsCopy && not existingSelf -> acc
                            Just _ | childIsCopy -> overrideCopy
                            Nothing -> fillChild
                            Just _ | existingSelf -> overrideSelf
                            _ -> acc

        bindParentsBase' =
            IntMap.foldlWithKey'
                (\acc childKey (parentRef, flag) ->
                    insertBindParentBase acc childKey parentRef flag
                )
                IntMap.empty
                bindParentsBase
        copyOverrides =
            IntMap.fromList
                [ (copyKeyC, baseN)
                | (copyKey, baseN) <- IntMap.toList instCopyMap
                , let copyKeyC = getNodeId (canonical (NodeId copyKey))
                , IntMap.member copyKeyC nodesSolved
                ]
        (baseToSolved, solvedToBase) =
            let schemeInteriorKeys =
                    IntSet.unions
                        [ reachableFromWithBoundsBase root
                        | gen <- NodeAccess.allGenNodes base
                        , root <- gnSchemes gen
                        ]
                bindParentKeys =
                    IntSet.fromList
                        ( map getNodeId (IntMapUtils.typeChildNodes bindParentsBase)
                            ++ map getNodeId (IntMapUtils.typeParentNodes bindParentsBase)
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
                baseToSolved0 =
                    IntMap.fromList
                        [ (nid0, chooseMapping nid0)
                        | nid0 <- IntSet.toList baseKeys
                        ]
                solvedToBase0 =
                    IntMap.foldlWithKey'
                        (\acc baseKey solvedNid ->
                            let solvedKeyC = getNodeId (canonical solvedNid)
                                solvedKeyRaw = getNodeId solvedNid
                                acc' = IntMap.insertWith keepOld solvedKeyC (NodeId baseKey) acc
                            in IntMap.insertWith keepOld solvedKeyRaw (NodeId baseKey) acc'
                        )
                        IntMap.empty
                        baseToSolved0
                solvedToBase1 =
                    IntMap.union copyOverrides solvedToBase0
            in (baseToSolved0, solvedToBase1)

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
                    in case IntMap.lookup (nodeRefKey (typeRef baseN)) bindParentsBase of
                        Just (parentRef, flag) ->
                            let parentRef' = adoptRef parentRef
                            in applyBindParent policyKeepOld childRef' parentRef' flag acc
                        Nothing -> acc
                )
                bindParents'
                instCopyMap
        -- Phase 4: scheme ownership (roots and interiors).
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
        baseFirstGenAncestor ref0 =
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
        firstGenAncestorWith = firstGenAncestorFrom
        dropSelfParents =
            IntMap.filterWithKey
                (\childKey (parentRef, _flag) ->
                    nodeRefKey parentRef /= childKey
                )
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
            let insertEdge acc childRef parentRef flag
                    = applyBindParent policyInsert childRef parentRef flag acc
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
                                                        _ -> insertGenParentPreserveFlag gid rootKey acc'
                                                else acc'
                                    in case IntMap.lookup (getNodeId rootC) nodesSolved of
                                        Just TyVar{ tnBound = Just bnd } ->
                                            let bndRef = typeRef bnd
                                                bndKey = nodeRefKey bndRef
                                            in if okRef bndRef
                                                then insertParentIfSelfOrEmpty rootRef bndKey accWithRoot
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
                                        then insertParentIfSelfOrEmpty varRef bndKey acc
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
                                                else insertGenParentIfSelfOrEmpty gid childKey acc
                                    Just _ -> acc
                                    Nothing
                                        | ownerMismatch || baseOwnerMismatch -> acc
                                        | otherwise ->
                                            case IntMap.lookup nidInt nodesSolved of
                                                Just TyVar{} -> insertGenParentIfSelfOrEmpty gid childKey acc
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
                                                            insertGenParentIfSelfOrEmpty gid childKey acc'
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
                                                insertGenParentPreserveFlag gid childKey acc
                                        else
                                            insertGenParentIfSelfOrEmpty gid childKey acc
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
                                    insertGenParentIfSelfOrEmpty gid childKey acc
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
                    "scheme interiors="
                        ++ show
                            [ (gnId gen, root,
                                [ (nid, IntMap.lookup (nodeRefKey (typeRef (NodeId nid))) bindParentsFinal)
                                | nid <- IntSet.toList (reachableFromWithBoundsStop (canonical root))
                                , isTyVarAt nodesSolved nid
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
                    "probe bind-parent node="
                        ++ show probeId
                        ++ " parent="
                        ++ show parentInfo
                        ++ " solvedParent="
                        ++ show probeSolvedParent
                        ++ " solvedToBase="
                        ++ show probeSolvedToBase
                        ++ " instCopy="
                        ++ show probeInstCopy
            in debug msgProbe (debug msg bindParentsFinalRebound)
        bindParentsFinal'' = attachOrphans bindParentsFinal'
        bindParentsFinalClean = dropSelfParents bindParentsFinal''
        restoreSchemeRootParentsFromOwners bp0 =
            IntMap.foldlWithKey'
                (\acc rootKey gid ->
                    let rootRef = typeRef (NodeId rootKey)
                        rootKeyRef = nodeRefKey rootRef
                    in if okRef rootRef
                        then insertGenParentPreserveFlag gid rootKeyRef acc
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
        -- Phase 5: finalize constraint and align base/solved mappings.
        constraintForGen =
            let restoreTypeParents acc =
                    IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, flag) ->
                            case (nodeRefFromKey childKey, parentRef) of
                                (TypeRef childBase, TypeRef _parentBase) ->
                                    case (mapBaseRef (typeRef childBase), mapBaseRef parentRef) of
                                        (Just childRef', Just parentRef') ->
                                            applyBindParent policySelfOrEmpty childRef' parentRef' flag acc'
                                        _ -> acc'
                                _ -> acc'
                        )
                        acc
                        bindParentsBase
                bindParentsFinalAligned' = restoreTypeParents bindParentsFinalAligned
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
                constraint0 = solvedConstraint { cNodes = nodesSolved, cBindParents = bindParentsFinalAligned'', cGenNodes = genMerged' }
                constraint1 = pruneBindParentsConstraint constraint0
                probeIds = [NodeId 1, NodeId 2, NodeId 3]
                probeInfo =
                    [ ( pid
                      , NodeAccess.lookupNode constraint1 pid
                      , IntMap.lookup (nodeRefKey (typeRef pid)) (cBindParents constraint1)
                      )
                    | pid <- probeIds
                    ]
            in debug ("probe nodes " ++ show probeInfo) constraint1
        alignedMapping =
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
                                        in ( IntMap.insertWith keepOld solvedKey baseB accSolved'
                                           , IntMap.insertWith keepOld baseKey (canonical solvedB) accBase'
                                           )
                                    )
                                    (accSolved, accBase)
                                    (zip solvedBinders baseBinders)
                            _ -> (accSolved, accBase)
                (qAlignSolvedToBase, qAlignBaseToSolved) =
                    foldl' alignOne (IntMap.empty, IntMap.empty) (NodeAccess.allGenNodes base)
                baseToSolvedAligned = IntMap.union baseToSolved qAlignBaseToSolved
                solvedToBaseAligned0 =
                    IntMap.foldlWithKey'
                        (\acc baseKey solvedNid ->
                            let solvedKeyC = getNodeId (canonical solvedNid)
                                solvedKeyRaw = getNodeId solvedNid
                                acc' = IntMap.insertWith keepOld solvedKeyC (NodeId baseKey) acc
                            in IntMap.insertWith keepOld solvedKeyRaw (NodeId baseKey) acc'
                        )
                        IntMap.empty
                        baseToSolvedAligned
                solvedToBaseAligned =
                    IntMap.union copyOverrides (IntMap.union solvedToBaseAligned0 qAlignSolvedToBase)
            in NodeMapping
                { mapBaseToSolved = baseToSolvedAligned
                , mapSolvedToBase = solvedToBaseAligned
                }
    in debug
            ("merged gens="
                ++ show (map fst schemeRootsMerged')
                ++ " schemes="
                ++ show (map snd schemeRootsMerged')
            )
            ( constraintForGen
            , GaBindParents
                { gaBindParentsBase = bindParentsBase
                , gaBaseConstraint = base
                , gaBaseToSolved = mapBaseToSolved alignedMapping
                , gaSolvedToBase = mapSolvedToBase alignedMapping
                }
            )

generalizeAtWithBuilder
    :: PresolutionPlanBuilder
    -> Maybe GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtWithBuilder planBuilder mbBindParentsGa res scopeRoot targetNode =
    let PresolutionPlanBuilder buildPlans = planBuilder
        go mbGa res' scope target = do
            (genPlan, reifyPlan) <- buildPlans res' mbGa scope target
            let fallback scope' target' = fst <$> go mbGa res' scope' target'
            applyGeneralizePlan fallback genPlan reifyPlan
    in go mbBindParentsGa res scopeRoot targetNode
