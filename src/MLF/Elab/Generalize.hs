module MLF.Elab.Generalize (
    GaBindParents(..),
    generalizeAt,
    generalizeAtKeepTarget,
    generalizeAtAllowRigid,
    generalizeAtKeepTargetAllowRigid,
    generalizeAtAllowRigidWithBindParents,
    generalizeAtKeepTargetAllowRigidWithBindParents
) where

import Data.Functor.Foldable (cata, para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort, sortBy)
import Data.Maybe (listToMaybe, isNothing)
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Generalize.Util (boundRootWith, firstSchemeRootAncestorWith)
import MLF.Elab.Util (reachableFrom, reachableFromStop, topoSortBy)
import MLF.Elab.Reify
    ( reifyBoundWithNames
    , reifyBoundWithNamesOnConstraint
    , reifyTypeWithNamesNoFallback
    , reifyTypeWithNamesNoFallbackOnConstraint
    )
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Binding.Tree as Binding

data GaBindParents = GaBindParents
    { gaBindParentsBase :: BindParents
    , gaBaseConstraint :: Constraint
    , gaBaseToSolved :: IntMap.IntMap NodeId
    , gaSolvedToBase :: IntMap.IntMap NodeId
    }

data GeneralizeEnv = GeneralizeEnv
    { geConstraint :: Constraint
    , geNodes :: IntMap.IntMap TyNode
    , geCanonical :: NodeId -> NodeId
    , geCanonKey :: NodeId -> Int
    , geLookupNode :: Int -> Maybe TyNode
    , geIsTyVarKey :: Int -> Bool
    , geIsTyForallKey :: Int -> Bool
    , geIsBaseLikeKey :: Int -> Bool
    , geBindParentsGa :: Maybe GaBindParents
    , geRes :: SolveResult
    , geDebugEnabled :: Bool
    }

data GeneralizeCtx = GeneralizeCtx
    { gcTarget0 :: NodeId
    , gcTargetBase :: NodeId
    , gcScopeRootC :: NodeRef
    , gcOrderRoot :: NodeId
    , gcTypeRoot0 :: NodeId
    , gcOrderRootBase :: NodeId
    , gcScopeGen :: Maybe GenNodeId
    , gcBindParents :: BindParents
    , gcFirstGenAncestor :: NodeRef -> Maybe GenNodeId
    , gcConstraintForReify :: Constraint
    , gcResForReify :: SolveResult
    }

data ResolveTarget = ResolveTarget
    { rtTarget0 :: NodeId
    , rtTargetBase :: NodeId
    , rtScopeRootC :: NodeRef
    , rtOrderRoot :: NodeId
    , rtTypeRoot0 :: NodeId
    , rtOrderRootBase :: NodeId
    }

data ResolveScope = ResolveScope
    { rsScopeGen :: Maybe GenNodeId
    }

data ResolveBinds = ResolveBinds
    { rbBindParents :: BindParents
    , rbFirstGenAncestor :: NodeRef -> Maybe GenNodeId
    , rbConstraintForReify :: Constraint
    , rbResForReify :: SolveResult
    }

mkGeneralizeEnv :: Maybe GaBindParents -> SolveResult -> GeneralizeEnv
mkGeneralizeEnv mbBindParentsGa res =
    let constraint = srConstraint res
        nodes = cNodes constraint
        uf = srUnionFind res
        canonical = Solve.frWith uf
        canonKey nid = getNodeId (canonical nid)
        lookupNode key = IntMap.lookup key nodes
        isTyVarNode node = case node of
            TyVar{} -> True
            _ -> False
        isTyForallNode node = case node of
            TyForall{} -> True
            _ -> False
        isBaseLikeNode node = case node of
            TyBase{} -> True
            TyBottom{} -> True
            _ -> False
        isTyVarKey key = maybe False isTyVarNode (lookupNode key)
        isTyForallKey key = maybe False isTyForallNode (lookupNode key)
        isBaseLikeKey key = maybe False isBaseLikeNode (lookupNode key)
    in GeneralizeEnv
        { geConstraint = constraint
        , geNodes = nodes
        , geCanonical = canonical
        , geCanonKey = canonKey
        , geLookupNode = lookupNode
        , geIsTyVarKey = isTyVarKey
        , geIsTyForallKey = isTyForallKey
        , geIsBaseLikeKey = isBaseLikeKey
        , geBindParentsGa = mbBindParentsGa
        , geRes = res
        , geDebugEnabled = debugGeneralizeEnabled
        }

softenBindParents :: (NodeId -> NodeId) -> Constraint -> BindParents -> BindParents
softenBindParents canonical constraint =
    let weakened = cWeakenedVars constraint
        softenOne childKey (parent, flag) =
            case (flag, nodeRefFromKey childKey) of
                (BindRigid, TypeRef childN)
                    | IntSet.member (getNodeId (canonical childN)) weakened ->
                        (parent, BindFlex)
                _ -> (parent, flag)
    in IntMap.mapWithKey softenOne

-- | Generalize a node at the given binding site into a polymorphic scheme.
-- For xMLF, quantified variables can have bounds.
-- Returns the scheme and the substitution used to rename variables.
generalizeAt :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAt = generalizeAtWith True False Nothing

-- | Variant of 'generalizeAt' that keeps the target binder even when it would
-- normally be dropped as an alias wrapper.
generalizeAtKeepTarget :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtKeepTarget = generalizeAtWith False True Nothing

-- | Variant of 'generalizeAt' that allows rigid binders to be quantified
-- while still dropping alias targets.
generalizeAtAllowRigid :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtAllowRigid = generalizeAtWith True True Nothing

-- | Variant of 'generalizeAt' that allows rigid binders while keeping
-- alias targets.
generalizeAtKeepTargetAllowRigid :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtKeepTargetAllowRigid = generalizeAtWith False True Nothing

generalizeAtAllowRigidWithBindParents
    :: GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtAllowRigidWithBindParents gaParents =
    generalizeAtWith True True (Just gaParents)

generalizeAtKeepTargetAllowRigidWithBindParents
    :: GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtKeepTargetAllowRigidWithBindParents gaParents =
    generalizeAtWith False True (Just gaParents)

generalizeAtWith
    :: Bool
    -> Bool
    -> Maybe GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtWith allowDropTarget allowRigidBinders mbBindParentsGa res scopeRoot targetNode = do
    let env = mkGeneralizeEnv mbBindParentsGa res
        constraint = geConstraint env
        nodes = geNodes env
        canonical = geCanonical env
        canonKey = geCanonKey env
        lookupNode = geLookupNode env
        isTyVarKey = geIsTyVarKey env
        isTyForallKey = geIsTyForallKey env
        isBaseLikeKey = geIsBaseLikeKey env
    bindParents0 <- bindingToElab (Binding.canonicalizeBindParentsUnder canonical constraint)
    let bindParentsSoft = softenBindParents canonical constraint bindParents0
    let _ =
            traceGeneralize env
                ("generalizeAt: gaParents sizes="
                    ++ case mbBindParentsGa of
                        Nothing -> "None"
                        Just ga ->
                            " baseParents="
                                ++ show (IntMap.size (gaBindParentsBase ga))
                                ++ " baseToSolved="
                                ++ show (IntMap.size (gaBaseToSolved ga))
                                ++ " solvedToBase="
                                ++ show (IntMap.size (gaSolvedToBase ga))
                )
                ()
    ctx <- resolveContext env bindParentsSoft scopeRoot targetNode
    let GeneralizeCtx
            { gcTarget0 = target0
            , gcScopeRootC = scopeRootC
            , gcOrderRoot = orderRoot
            , gcTypeRoot0 = typeRoot0
            , gcOrderRootBase = orderRootBase
            , gcScopeGen = scopeGen
            , gcBindParents = bindParents
            , gcFirstGenAncestor = firstGenAncestorGa
            , gcResForReify = resForReify
            } = ctx
    -- Phase 4: scheme-root metadata and bound traversal policy.
    let schemeRootsWithGen =
            [ (gnId gen, root)
            | gen <- IntMap.elems (cGenNodes constraint)
            , root <- gnSchemes gen
            ]
        schemeRootKeySetRaw =
            IntSet.fromList [ getNodeId root | (_gid, root) <- schemeRootsWithGen ]
        schemeRootOwner =
            IntMap.fromList
                [ (getNodeId (canonical root), gid)
                | (gid, root) <- schemeRootsWithGen
                ]
        schemeRootKeySet =
            IntSet.union schemeRootKeySetRaw $
                IntSet.fromList
                    [ getNodeId (canonical root)
                    | (_gid, root) <- schemeRootsWithGen
                    ]
        schemeRootByBody =
            IntMap.fromListWith
                (\a _ -> a)
                [ (getNodeId (canonical bnd), root)
                | (_gid, root) <- schemeRootsWithGen
                , Just bnd <- [VarStore.lookupVarBound constraint root]
                , case IntMap.lookup (getNodeId (canonical bnd)) nodes of
                    Just TyBase{} -> False
                    Just TyBottom{} -> False
                    _ -> True
                ]
        lookupSchemeRootOwner bnd =
            let bndC = canonical bnd
                key = getNodeId bndC
            in case IntMap.lookup key schemeRootOwner of
                Just gid -> Just gid
                Nothing ->
                    case IntMap.lookup key schemeRootByBody of
                        Just root ->
                            IntMap.lookup (getNodeId (canonical root)) schemeRootOwner
                        Nothing -> Nothing
        isSchemeRootBody bnd =
            case IntMap.lookup (getNodeId (canonical bnd)) schemeRootByBody of
                Just root -> Just root
                Nothing -> Nothing
        parseNameId name =
            case name of
                ('t':rest) ->
                    case reads rest of
                        [(n, "")] -> Just n
                        _ -> Nothing
                _ -> Nothing
        bindingScopeFor ref =
            case Binding.bindingPathToRoot constraint ref of
                Right path -> listToMaybe [gid | GenRef gid <- drop 1 path]
                Left _ -> Nothing
        typeRootFromBoundVar =
            case scopeGen of
                Just gid ->
                    listToMaybe
                        [ canonical child
                        | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents
                        , parentRef == GenRef gid
                        , TypeRef child <- [nodeRefFromKey childKey]
                        , isTyVarKey (canonKey child)
                        , case VarStore.lookupVarBound constraint (canonical child) of
                            Just bnd -> canonical bnd == canonical target0
                            Nothing -> False
                        ]
                Nothing -> Nothing
        containsForallFrom treatAsForall start =
            let walkForall visited nid0 =
                    let nid = canonical nid0
                        key = getNodeId nid
                    in if IntSet.member key visited
                        then False
                        else
                            if treatAsForall key
                                then True
                                else case IntMap.lookup key nodes of
                                    Just TyForall{} -> True
                                    Just TyVar{ tnBound = Just bnd } ->
                                        walkForall (IntSet.insert key visited) bnd
                                    Just TyExp{ tnBody = b } ->
                                        walkForall (IntSet.insert key visited) b
                                    Just node ->
                                        let visited' = IntSet.insert key visited
                                        in any (walkForall visited') (structuralChildren node)
                                    Nothing -> False
            in walkForall IntSet.empty start
        containsForallForTarget bnd =
            containsForallFrom
                (\key -> IntSet.member key schemeRootKeySet || IntMap.member key schemeRootByBody)
                bnd
        boundHasForallForVar v =
            let treatAsForall key =
                    IntSet.member key schemeRootKeySet || IntMap.member key schemeRootByBody
            in case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd -> containsForallFrom treatAsForall bnd
                Nothing -> False
        (schemeRootOwnerBase, schemeRootByBodyBase) =
            case mbBindParentsGa of
                Just ga ->
                    let baseConstraint = gaBaseConstraint ga
                        baseNodes = cNodes baseConstraint
                        baseParents = gaBindParentsBase ga
                        baseSchemeRoots =
                            [ root
                            | gen <- IntMap.elems (cGenNodes baseConstraint)
                            , root <- gnSchemes gen
                            ]
                        ownerFromBinding root =
                            firstGenAncestor baseParents (TypeRef root)
                    in ( IntMap.fromList
                            [ (getNodeId root, gid)
                            | root <- baseSchemeRoots
                            , Just gid <- [ownerFromBinding root]
                            ]
                       , IntMap.fromListWith
                            (\a _ -> a)
                            [ (getNodeId bnd, root)
                            | root <- baseSchemeRoots
                            , Just bnd <- [VarStore.lookupVarBound baseConstraint root]
                            , case IntMap.lookup (getNodeId bnd) baseNodes of
                                Just TyBase{} -> False
                                Just TyBottom{} -> False
                                _ -> True
                            ]
                       )
                Nothing -> (IntMap.empty, IntMap.empty)
    let orderRootForBinders = orderRoot
        orderRootBaseForBinders =
            case mbBindParentsGa of
                Nothing -> orderRootForBinders
                Just ga ->
                    let baseConstraint = gaBaseConstraint ga
                        baseNodes = cNodes baseConstraint
                        useSchemeBody baseN =
                            case IntMap.lookup (getNodeId baseN) baseNodes of
                                Just TyVar{ tnBound = Just bnd }
                                    | IntMap.member (getNodeId baseN) schemeRootOwnerBase ->
                                        bnd
                                _ -> baseN
                    in case IntMap.lookup (getNodeId orderRootForBinders) (gaSolvedToBase ga) of
                        Just baseN -> useSchemeBody baseN
                        Nothing -> orderRootBase
        boundIsTargetSchemeBody bnd =
            case isSchemeRootBody bnd of
                Just root -> canonical root == canonical target0
                Nothing -> False
        allowBoundTraversal bnd =
            case lookupSchemeRootOwner bnd of
                Nothing -> True
                Just gid ->
                    case scopeGen of
                        Just scopeGid -> gid == scopeGid || boundIsTargetSchemeBody bnd
                        Nothing -> False

        childrenWithBounds nid =
            case IntMap.lookup (getNodeId nid) nodes of
                Nothing -> []
                Just node ->
                    let boundKids =
                            case node of
                                TyVar{ tnBound = Just bnd }
                                    | allowBoundTraversal bnd -> [bnd]
                                _ -> []
                    in structuralChildren node ++ boundKids
        reachableFromWithBounds root0 =
            reachableFrom getNodeId canonical childrenWithBounds root0

        childrenStructural nid =
            case IntMap.lookup (getNodeId nid) nodes of
                Nothing -> []
                Just node -> structuralChildren node
        reachableFromStructural root0 =
            reachableFrom getNodeId canonical childrenStructural root0

    reachable <- Right (reachableFromWithBounds orderRoot)
    let reachableForBinders0 =
            case mbBindParentsGa of
                Nothing -> reachable
                Just ga ->
                    let baseConstraint = gaBaseConstraint ga
                        baseNodes = cNodes baseConstraint
                        scopeGenBase = scopeGen
                        boundSchemeOwnerBase bnd =
                            case IntMap.lookup (getNodeId bnd) schemeRootOwnerBase of
                                Just gid -> Just gid
                                Nothing ->
                                    case IntMap.lookup (getNodeId bnd) schemeRootByBodyBase of
                                        Just root ->
                                            IntMap.lookup (getNodeId root) schemeRootOwnerBase
                                        Nothing -> Nothing
                        allowBoundTraversalBase bnd =
                            case boundSchemeOwnerBase bnd of
                                Nothing -> True
                                Just gid ->
                                    case scopeGenBase of
                                        Just scopeGid -> gid == scopeGid
                                        Nothing -> False
                        reachableFromWithBoundsBase root0 =
                            let children nid =
                                    case IntMap.lookup (getNodeId nid) baseNodes of
                                        Nothing -> []
                                        Just node ->
                                            let boundKids =
                                                    case node of
                                                        TyVar{ tnBound = Just bnd }
                                                            | allowBoundTraversalBase bnd -> [bnd]
                                                        _ -> []
                                            in structuralChildren node ++ boundKids
                            in reachableFrom getNodeId id children root0
                        reachableBase = reachableFromWithBoundsBase orderRootBaseForBinders
                        reachableBaseSolved =
                            IntSet.fromList
                                [ getNodeId solvedNid
                                | baseKey <- IntSet.toList reachableBase
                                , Just solvedNid <- [IntMap.lookup baseKey (gaBaseToSolved ga)]
                                ]
                    in IntSet.union reachable reachableBaseSolved
    traceGeneralizeM env
        ("generalizeAt: reachable var parents="
            ++ show
                [ (NodeId nid, IntMap.lookup (nodeRefKey (typeRef (NodeId nid))) bindParents)
                | nid <- IntSet.toList reachable
                , case IntMap.lookup nid nodes of
                    Just TyVar{} -> True
                    _ -> False
                ]
        )
    traceGeneralizeM env
        ("generalizeAt: schemeRootByBodyKeys=" ++ show (IntMap.keys schemeRootByBody))
    -- Phase 5: binder selection helpers and candidates.
    let scopeSchemeRoots =
            case scopeGen of
                Nothing -> IntSet.empty
                Just gid ->
                    case IntMap.lookup (getGenNodeId gid) (cGenNodes constraint) of
                        Nothing -> IntSet.empty
                        Just gen ->
                            IntSet.fromList
                                [ getNodeId (canonical root)
                                | root <- gnSchemes gen
                                , case IntMap.lookup (nodeRefKey (typeRef root)) bindParents of
                                    Just (GenRef gid', _) | gid' == gid -> True
                                    _ -> False
                                ]
        isQuantifiable child =
            if isTyVarKey (getNodeId child)
                then not (VarStore.isEliminatedVar constraint (canonical child))
                else False
        bindFlags =
            IntMap.fromList
                [ (childKey, flag)
                | (childKey, (_parent, flag)) <- IntMap.toList bindParents
                ]
        boundContainsForall v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd -> containsForallFrom (const False) bnd
                Nothing -> False
        isScopeSchemeRoot child =
            IntSet.member (canonKey child) scopeSchemeRoots
        isBindable key child =
            case IntMap.lookup key bindFlags of
                Just BindFlex -> isQuantifiable child
                Just BindRigid
                    | allowRigidBinders || isScopeSchemeRoot child || boundContainsForall child ->
                        isQuantifiable child
                _ -> False
        scopeHasStructuralScheme =
            case scopeRootC of
                GenRef gid ->
                    case IntMap.lookup (genNodeKey gid) (cGenNodes constraint) of
                        Just gen ->
                            any
                                (\root ->
                                    not (isTyVarKey (canonKey root))
                                )
                                (gnSchemes gen)
                        Nothing -> False
                _ -> False
        aliasBinderInfo =
            let baseBoundTargets =
                    IntSet.fromList
                        [ getNodeId bndC
                        | (vidKey, node) <- IntMap.toList nodes
                        , TyVar{} <- [node]
                        , Just bnd <- [VarStore.lookupVarBound constraint (NodeId vidKey)]
                        , let bndC = canonical bnd
                        , case IntMap.lookup (getNodeId bndC) nodes of
                            Just TyBase{} -> True
                            Just TyBottom{} -> True
                            _ -> False
                        ]
            in case scopeRootC of
                GenRef gid
                    | scopeHasStructuralScheme ->
                        let bases =
                                IntSet.fromList
                                    [ key
                                    | (childKey, (parent, _flag)) <- IntMap.toList bindParents
                                    , parent == GenRef gid
                                    , TypeRef child <- [nodeRefFromKey childKey]
                                    , let key = getNodeId child
                                    , IntSet.member key baseBoundTargets
                                    , case IntMap.lookup key nodes of
                                        Just TyBase{} -> True
                                        Just TyBottom{} -> True
                                        _ -> False
                                    ]
                        in (bases, [ NodeId key | key <- IntSet.toList bases ])
                _ -> (IntSet.empty, [])
        (aliasBinderBases, aliasBinderNodes) = aliasBinderInfo
    traceGeneralizeM env
        ("generalizeAt: aliasBinderBases="
            ++ show (IntSet.toList aliasBinderBases)
            ++ " scopeHasStructuralScheme="
            ++ show scopeHasStructuralScheme
        )
    let boundFlexChildrenUnder parentRef =
            [ canonical child
            | (childKey, (parent, flag)) <- IntMap.toList bindParents
            , parent == parentRef
            , flag == BindFlex
            , TypeRef child <- [nodeRefFromKey childKey]
            , isBindable childKey child
            ]
        bindableChildrenUnder parentRef =
            [ canonical child
            | (childKey, (parent, _flag)) <- IntMap.toList bindParents
            , parent == parentRef
            , TypeRef child <- [nodeRefFromKey childKey]
            , isBindable childKey child
            ]
        bindingScopeGen child = bindingScopeFor (typeRef child)
        hasExplicitBound v =
            case IntMap.lookup (getNodeId (canonical v)) nodes of
                Just TyVar{} ->
                    case VarStore.lookupVarBound constraint (canonical v) of
                        Nothing -> False
                        Just _ -> True
                _ -> False
        bindersForGen gid = do
            let allChildren =
                    [ (child, flag)
                    | (childKey, (parent, flag)) <- IntMap.toList bindParents
                    , parent == GenRef gid
                    , TypeRef child <- [nodeRefFromKey childKey]
                    ]
            traceGeneralizeM env
                ("generalizeAt: scopeGen child nodes="
                    ++ show
                        [ (child, flag, IntMap.lookup (getNodeId child) nodes, VarStore.isEliminatedVar constraint (canonical child))
                        | (child, flag) <- allChildren
                        ]
                )
            traceGeneralizeM env
                ("generalizeAt: scopeGen children="
                    ++ show allChildren
                )
            let bindableByScope =
                    [ canonical child
                    | (childKey, node) <- IntMap.toList nodes
                    , TyVar{} <- [node]
                    , let child = NodeId childKey
                    , isBindable childKey child
                    , bindingScopeFor (typeRef child) == Just gid
                    ]
            pure (bindableChildrenUnder (GenRef gid) ++ aliasBinderNodes ++ bindableByScope)
        bindersForType scopeRootN = do
            let direct = boundFlexChildrenUnder (typeRef scopeRootN)
            case IntMap.lookup (getNodeId scopeRootN) nodes of
                Just TyForall{} -> pure direct
                _ ->
                    if IntSet.member (canonKey scopeRootN) scopeSchemeRoots
                        then pure direct
                        else pure [ v | v <- direct, not (hasExplicitBound v) ]
    binders0 <- case scopeRootC of
        GenRef gid -> bindersForGen gid
        TypeRef scopeRootN -> bindersForType scopeRootN
    traceGeneralizeM env
        ("generalizeAt: scopeRoot=" ++ show scopeRootC
            ++ " scopeGen=" ++ show scopeGen
            ++ " target=" ++ show target0
            ++ " binders=" ++ show binders0
        )
    let schemeRootSkipSet =
            IntSet.difference
                (IntSet.fromList
                    [ getNodeId (canonical root)
                    | gen <- IntMap.elems (cGenNodes constraint)
                    , let gid = gnId gen
                    , Just gid /= scopeGen
                    , root <- gnSchemes gen
                    ]
                )
                scopeSchemeRoots
        schemeRootBodySkipSet =
            IntSet.fromList
                [ bodyKey
                | (bodyKey, root) <- IntMap.toList schemeRootByBody
                , IntSet.member (canonKey root) schemeRootSkipSet
                ]
        reachableFromWithBoundsStop root0 =
            let stopSet = schemeRootKeySet
                shouldStop nid = IntSet.member (getNodeId nid) stopSet
            in reachableFromStop getNodeId canonical childrenWithBounds shouldStop root0
        nestedSchemeInteriorSet =
            IntSet.unions
                [ reachableFromWithBoundsStop (NodeId rootKey)
                | rootKey <- IntSet.toList schemeRootSkipSet
                ]
        isNestedSchemeBound v =
            case IntMap.lookup (canonKey v) nodes of
                Just TyVar{ tnBound = Just bnd } ->
                    let bndC = canonical bnd
                    in (IntSet.member (getNodeId bndC) schemeRootSkipSet
                        || IntSet.member (getNodeId bndC) schemeRootBodySkipSet)
                        && not (IntSet.member (getNodeId bndC) reachable)
                _ -> False
        boundIsSchemeRootVar v =
            let walkBoundChain visited nid =
                    let nidC = canonical nid
                        key = getNodeId nidC
                    in if IntSet.member key visited
                        then False
                        else
                            case VarStore.lookupVarBound constraint nidC of
                                Just bnd ->
                                    let bndC = canonical bnd
                                    in if IntSet.member (getNodeId bndC) schemeRootSkipSet
                                        then True
                                        else
                                            case IntMap.lookup (getNodeId bndC) nodes of
                                                Just TyVar{} ->
                                                    walkBoundChain (IntSet.insert key visited) bndC
                                                _ -> False
                                Nothing -> False
            in walkBoundChain IntSet.empty v
        boundIsSchemeRootAll v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    let bndC = canonical bnd
                        hasSchemeRoot =
                            IntSet.member (getNodeId bndC) schemeRootSkipSet
                                || IntMap.member (getNodeId bndC) schemeRootByBody
                    in hasSchemeRoot
                Nothing -> False
        targetBoundFromBase =
            case mbBindParentsGa of
                Just ga ->
                    case IntMap.lookup (getNodeId target0) (gaSolvedToBase ga) of
                        Just baseN ->
                            case VarStore.lookupVarBound (gaBaseConstraint ga) baseN of
                                Just bndBase ->
                                    case IntMap.lookup (getNodeId bndBase) (gaBaseToSolved ga) of
                                        Just solvedB -> Just (canonical solvedB)
                                        Nothing ->
                                            let bndKey = getNodeId bndBase
                                            in if IntMap.member bndKey nodes
                                                then Just (canonical (NodeId bndKey))
                                                else Nothing
                                Nothing -> Nothing
                        Nothing -> Nothing
                Nothing -> Nothing
        targetBoundRaw =
            case (mbBindParentsGa, targetBoundFromBase) of
                (Just _, Just bnd) -> Just bnd
                _ ->
                    case IntMap.lookup (getNodeId target0) nodes of
                        Just TyVar{ tnBound = Just bnd } -> Just bnd
                        _ -> targetBoundFromBase
        targetBound =
            case targetBoundRaw of
                Just bnd | IntSet.member (getNodeId bnd) schemeRootKeySetRaw -> Just bnd
                Just bnd -> Just (canonical bnd)
                Nothing -> Nothing
    traceGeneralizeM env
        ("generalizeAt: targetBound=" ++ show targetBound
            ++ " schemeRootSkipSet=" ++ show (IntSet.toList schemeRootSkipSet)
            ++ " boundParent="
                ++ case targetBound of
                    Just bnd ->
                        case IntMap.lookup (nodeRefKey (typeRef bnd)) bindParents of
                            Just (parentRef, _flag) -> show parentRef
                            Nothing -> "None"
                    Nothing -> "None"
        )
    traceGeneralizeM env
        ("generalizeAt: targetBoundOwner="
            ++ case targetBound of
                Just bnd -> show (lookupSchemeRootOwner bnd)
                Nothing -> "None"
            ++ " scopeGen="
            ++ show scopeGen
        )
    let targetBindParent = IntMap.lookup (nodeRefKey (typeRef target0)) bindParents
        targetBoundUnderOtherGen =
            case (scopeGen, targetBindParent) of
                (Just gidScope, Just (GenRef gidTarget, _)) -> gidTarget /= gidScope
                _ -> False
        boundUnderOtherGen =
            case (scopeGen, targetBound) of
                (Just gidScope, Just bnd) ->
                    case IntMap.lookup (nodeRefKey (typeRef bnd)) bindParents of
                        Just (GenRef gidBound, _) -> gidBound /= gidScope
                        _ -> False
                _ -> False
    traceGeneralizeM env
        ("generalizeAt: targetBoundNode="
            ++ case targetBound of
                Just bnd ->
                    show (IntMap.lookup (getNodeId bnd) nodes)
                        ++ " boundOfBound="
                        ++ show (VarStore.lookupVarBound constraint bnd)
                Nothing -> "None"
        )
    let
        boundIsSchemeRoot =
            case (scopeRootC, targetBound) of
                (GenRef gid, Just bnd) ->
                    case IntMap.lookup (getGenNodeId gid) (cGenNodes constraint) of
                        Nothing -> False
                        Just gen ->
                            IntSet.member
                                (getNodeId (canonical bnd))
                                (IntSet.fromList (map (getNodeId . canonical) (gnSchemes gen)))
                _ -> False
        boundIsVar =
            case targetBound >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                Just TyVar{} -> True
                _ -> False
        boundIsBase =
            case targetBound >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                Just TyBase{} -> True
                _ -> False
        boundIsStructural =
            case targetBound >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                Just TyArrow{} -> True
                Just TyForall{} -> True
                Just TyExp{} -> True
                _ -> False
        boundIsChild =
            case targetBound of
                Just bnd ->
                    let bndC = canonical bnd
                        quantifiable =
                            case IntMap.lookup (getNodeId bndC) nodes of
                                Just TyVar{} -> True
                                _ -> False
                    in quantifiable
                        && case IntMap.lookup (nodeRefKey (typeRef bndC)) bindParents of
                            Just (parentRef, _) -> parentRef == typeRef target0
                            Nothing -> False
                Nothing -> False
        boundIsDirectChild =
            case targetBound of
                Just bnd ->
                    let bndC = canonical bnd
                    in case IntMap.lookup (nodeRefKey (typeRef bndC)) bindParents of
                        Just (parentRef, _) -> parentRef == typeRef target0
                        Nothing -> False
                Nothing -> False
        boundMentionsTarget =
            case targetBound of
                Nothing -> False
                Just bnd ->
                    let targetC = canonical target0
                        walkMentions visited nid0 =
                            let nid = canonical nid0
                                key = getNodeId nid
                            in if nid == targetC
                                then True
                                else if IntSet.member key visited
                                    then False
                                    else
                                        case IntMap.lookup key nodes of
                                            Nothing -> False
                                            Just node ->
                                                let visited' = IntSet.insert key visited
                                                    boundKids =
                                                        case node of
                                                            TyVar{ tnBound = Just bnd' } -> [bnd']
                                                            _ -> []
                                                    kids = structuralChildren node ++ boundKids
                                                in any (walkMentions visited') kids
                    in walkMentions IntSet.empty bnd
        boundHasForall =
            let boundHasForallBase =
                    case (mbBindParentsGa, targetBound) of
                        (Just ga, Just bnd) ->
                            case IntMap.lookup (getNodeId (canonical bnd)) (gaSolvedToBase ga) of
                                Just baseBnd ->
                                    IntMap.member (getNodeId baseBnd) schemeRootOwnerBase
                                        || IntMap.member (getNodeId baseBnd) schemeRootByBodyBase
                                Nothing -> False
                        _ -> False
            in case targetBound of
                Nothing -> False
                Just bnd -> containsForallForTarget bnd || boundHasForallBase
        boundHasNestedGen =
            case (targetBound, scopeGen) of
                (Just bnd, Just gid) ->
                    let reachableBound = reachableFromWithBounds bnd
                        isNested nidInt =
                            case IntMap.lookup nidInt nodes of
                                Just TyVar{} ->
                                    case firstGenAncestorGa (typeRef (NodeId nidInt)) of
                                        Just gid' -> gid' /= gid
                                        Nothing -> False
                                _ -> False
                    in any isNested (IntSet.toList reachableBound)
                _ -> False
        targetRigid =
            case IntMap.lookup (nodeRefKey (typeRef target0)) bindFlags of
                Just BindRigid -> True
                _ -> False
        targetIsSchemeRoot =
            IntSet.member (canonKey target0) schemeRootKeySet
        targetIsSchemeRootForScope =
            case (scopeGen, mbBindParentsGa) of
                (Just gid, Just ga) ->
                    case IntMap.lookup (canonKey target0) (gaSolvedToBase ga) of
                        Just baseN ->
                            case IntMap.lookup (getNodeId baseN) schemeRootOwnerBase of
                                Just ownerGid -> ownerGid == gid
                                Nothing -> False
                        Nothing -> False
                (Just gid, Nothing) ->
                    case IntMap.lookup (getGenNodeId gid) (cGenNodes constraint) of
                        Just gen -> any (\root -> canonical root == canonical target0) (gnSchemes gen)
                        Nothing -> False
                _ -> False
        targetIsTyVar = isTyVarKey (canonKey target0)
        namedUnderGaRaw =
            case scopeGen of
                Just gid ->
                    case mbBindParentsGa of
                        Nothing ->
                            IntMap.elems $
                                IntMap.fromList
                                    [ (getNodeId nid, nid)
                                    | nid <- bindableChildrenUnder (GenRef gid) ++ aliasBinderNodes
                                    ]
                        Just ga ->
                            let solvedKids = bindableChildrenUnder (GenRef gid)
                                baseKids =
                                    [ canonical solvedNid
                                    | (childKey, (parent, flag)) <- IntMap.toList (gaBindParentsBase ga)
                                    , parent == GenRef gid
                                    , flag == BindFlex || (allowRigidBinders && flag == BindRigid)
                                    , case IntMap.lookup childKey (cNodes (gaBaseConstraint ga)) of
                                        Just TyVar{} -> True
                                        _ -> False
                                    , Just solvedNid <- [IntMap.lookup childKey (gaBaseToSolved ga)]
                                    , case IntMap.lookup (getNodeId (canonical solvedNid)) nodes of
                                        Just TyVar{} -> True
                                        _ -> False
                                    ]
                            in IntMap.elems $
                                IntMap.fromList
                                    [ (getNodeId nid, nid)
                                    | nid <- baseKids ++ solvedKids ++ aliasBinderNodes
                                    ]
                Nothing -> []
        namedUnderGaInterior = IntSet.empty
    let (baseGammaSet, baseGammaRep, namedUnderGaSet, solvedToBasePref) =
            case (scopeGen, mbBindParentsGa) of
                (Just gid, Just ga) ->
                    let baseConstraint = gaBaseConstraint ga
                        baseNodes = cNodes baseConstraint
                        baseGammaSetLocalRaw =
                            IntSet.fromList
                                [ childKey
                                | (childKey, (parent, flag)) <- IntMap.toList (gaBindParentsBase ga)
                                , parent == GenRef gid
                                , flag == BindFlex || (allowRigidBinders && flag == BindRigid)
                                , case IntMap.lookup childKey baseNodes of
                                    Just TyVar{} -> True
                                    _ -> False
                                ]
                        baseSchemeRootSetLocal =
                            IntSet.fromList
                                [ rootKey
                                | rootKey <- IntMap.keys schemeRootOwnerBase
                                ]
                        firstSchemeRootAncestorBase baseKey =
                            let parentOf ref =
                                    fmap fst (IntMap.lookup (nodeRefKey ref) (gaBindParentsBase ga))
                                keyOfRef ref = nodeRefKey ref
                                isSchemeRootKey = (`IntSet.member` baseSchemeRootSetLocal)
                            in firstSchemeRootAncestorWith parentOf keyOfRef isSchemeRootKey baseKey
                        keepBaseGamma baseKey =
                            case firstSchemeRootAncestorBase baseKey of
                                Nothing -> True
                                Just rootKey ->
                                    case IntMap.lookup rootKey schemeRootOwnerBase of
                                        Just ownerGid -> ownerGid == gid
                                        Nothing -> True
                        baseGammaSetLocal0 =
                            IntSet.filter keepBaseGamma baseGammaSetLocalRaw
                        baseGammaDirect =
                            IntSet.fromList
                                [ getNodeId baseN
                                | solvedN <- namedUnderGaRaw
                                , Just baseN <- [IntMap.lookup (getNodeId (canonical solvedN)) (gaSolvedToBase ga)]
                                ]
                        baseGammaSetLocal =
                            IntSet.union baseGammaSetLocal0 baseGammaDirect
                        firstSchemeRootAncestorSolved solvedKey =
                            let parentOf ref =
                                    fmap fst (IntMap.lookup (nodeRefKey ref) bindParents)
                                keyOfRef ref =
                                    case ref of
                                        GenRef gidRef -> genNodeKey gidRef
                                        TypeRef nid -> canonKey nid
                                isSchemeRootKey = (`IntSet.member` schemeRootKeySet)
                            in firstSchemeRootAncestorWith parentOf keyOfRef isSchemeRootKey solvedKey
                        keepSolvedGamma solvedKey =
                            case firstSchemeRootAncestorSolved solvedKey of
                                Nothing -> True
                                Just rootKey ->
                                    case IntMap.lookup rootKey schemeRootOwner of
                                        Just ownerGid -> ownerGid == gid
                                        Nothing -> False
                        solvedGammaSetLocal =
                            case Binding.boundFlexChildren constraint (GenRef gid) of
                                Right kids ->
                                    IntSet.fromList
                                        [ getNodeId (canonical kid)
                                        | kid <- kids
                                        , let key = getNodeId (canonical kid)
                                        , case IntMap.lookup key nodes of
                                            Just TyVar{} -> keepSolvedGamma key
                                            _ -> False
                                        ]
                                Left _ -> IntSet.empty
                        baseToSolved = gaBaseToSolved ga
                        solvedToBase = gaSolvedToBase ga
                        keysSolved = Order.orderKeysFromConstraintWith canonical constraint orderRoot Nothing
                        keysBase = Order.orderKeysFromConstraintWith id baseConstraint orderRootBase Nothing
                        sortByKeys keys =
                            let keyMaybe k = IntMap.lookup k keys
                                cmp a b =
                                    case (keyMaybe a, keyMaybe b) of
                                        (Just ka, Just kb) ->
                                            case Order.compareOrderKey ka kb of
                                                EQ -> compare a b
                                                other -> other
                                        (Just _, Nothing) -> LT
                                        (Nothing, Just _) -> GT
                                        _ -> compare a b
                            in sortBy cmp
                        solvedGammaOrdered = sortByKeys keysSolved (IntSet.toList solvedGammaSetLocal)
                        baseGammaOrdered = sortByKeys keysBase (IntSet.toList baseGammaSetLocal)
                        qAlignSolvedToBaseLocal =
                            IntMap.fromList
                                [ (solvedKey, NodeId baseKey)
                                | (solvedKey, baseKey) <- zip solvedGammaOrdered baseGammaOrdered
                                ]
                        alignBaseToSolved =
                            case ( Binding.boundFlexChildren (gaBaseConstraint ga) (GenRef gid)
                                 , Binding.boundFlexChildrenUnder canonical constraint (GenRef gid)
                                 ) of
                                (Right baseBinders, Right solvedBinders) ->
                                    IntMap.fromList
                                        [ (getNodeId baseB, getNodeId (canonical solvedB))
                                        | (baseB, solvedB) <- zip baseBinders solvedBinders
                                        ]
                                _ -> IntMap.empty
                        solvedToBaseAll =
                            IntMap.fromListWith
                                (++)
                                [ (getNodeId (canonical solvedNid), [NodeId baseKey])
                                | (baseKey, solvedNid) <- IntMap.toList baseToSolved
                                ]
                        solvedUnderScope solvedKey =
                            firstGenAncestor bindParents (TypeRef (NodeId solvedKey)) == Just gid
                        preferGamma =
                            IntMap.mapMaybeWithKey
                                (\_solvedKey baseList ->
                                    listToMaybe
                                        [ baseN
                                        | baseN@(NodeId baseKey) <- baseList
                                        , IntSet.member baseKey baseGammaSetLocal
                                        ]
                                )
                                solvedToBaseAll
                        identityGamma =
                            IntMap.fromList
                                [ (baseKey, NodeId baseKey)
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                , IntMap.member baseKey nodes
                                ]
                        identityGammaScoped =
                            IntMap.fromList
                                [ (baseKey, NodeId baseKey)
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                , IntMap.member baseKey nodes
                                , solvedUnderScope baseKey
                                ]
                        solvedToBasePrefLocal =
                            IntMap.union qAlignSolvedToBaseLocal $
                                IntMap.union identityGammaScoped
                                    (IntMap.union preferGamma (IntMap.union solvedToBase identityGamma))
                        solvedByBasePref =
                            IntMap.fromListWith
                                (++)
                                [ (getNodeId baseN, [solvedKey])
                                | (solvedKey, baseN) <- IntMap.toList solvedToBasePrefLocal
                                ]
                        solvedFallback =
                            IntMap.fromListWith
                                min
                                [ (getNodeId baseNid, getNodeId (canonical (NodeId solvedKey)))
                                | (solvedKey, baseNid) <- IntMap.toList solvedToBase
                                ]
                        pickSolved baseKey =
                            if IntMap.member baseKey nodes && solvedUnderScope baseKey
                                then Just baseKey
                                else
                                    case IntMap.lookup baseKey baseToSolved of
                                        Just solvedNid ->
                                            let solvedKey = getNodeId (canonical solvedNid)
                                            in if solvedUnderScope solvedKey
                                                then Just solvedKey
                                                else
                                                    case IntMap.lookup baseKey solvedByBasePref of
                                                        Just solvedKeys ->
                                                            let underScopeKeys = filter solvedUnderScope solvedKeys
                                                                pickFrom keys =
                                                                    case filter isTyVarKey keys of
                                                                        (k:_) -> Just k
                                                                        [] ->
                                                                            case keys of
                                                                                (k:_) -> Just k
                                                                                _ -> Nothing
                                                            in case pickFrom underScopeKeys of
                                                                Just k -> Just k
                                                                Nothing -> pickFrom solvedKeys
                                                        _ ->
                                                            case IntMap.lookup baseKey alignBaseToSolved of
                                                                Just solvedKey' -> Just solvedKey'
                                                                Nothing ->
                                                                    case IntMap.lookup baseKey solvedFallback of
                                                                        Just solvedKey' -> Just solvedKey'
                                                                        Nothing ->
                                                                            if IntMap.member baseKey nodes
                                                                                then Just baseKey
                                                                                else Nothing
                                        Nothing ->
                                            case IntMap.lookup baseKey solvedByBasePref of
                                                Just solvedKeys ->
                                                    let underScopeKeys = filter solvedUnderScope solvedKeys
                                                        pickFrom keys =
                                                            case filter isTyVarKey keys of
                                                                (k:_) -> Just k
                                                                [] ->
                                                                    case keys of
                                                                        (k:_) -> Just k
                                                                        _ -> Nothing
                                                    in case pickFrom underScopeKeys of
                                                        Just k -> Just k
                                                        Nothing -> pickFrom solvedKeys
                                                _ ->
                                                    case IntMap.lookup baseKey alignBaseToSolved of
                                                        Just solvedKey -> Just solvedKey
                                                        Nothing ->
                                                            case IntMap.lookup baseKey solvedFallback of
                                                                Just solvedKey -> Just solvedKey
                                                                Nothing ->
                                                                    if IntMap.member baseKey nodes
                                                                        then Just baseKey
                                                                        else Nothing
                        baseGammaRepLocal =
                            IntMap.fromList
                                [ (baseKey, solvedKey)
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                , Just solvedKey <- [pickSolved baseKey]
                                , case IntMap.lookup solvedKey nodes of
                                    Just TyVar{} -> True
                                    _ -> False
                                ]
                        solvedBindersUnderScope =
                            [ canonical child
                            | (childKey, (parentRef, flag)) <- IntMap.toList bindParents
                            , parentRef == GenRef gid
                            , flag == BindFlex
                            , TypeRef child <- [nodeRefFromKey childKey]
                            , case IntMap.lookup (getNodeId (canonical child)) nodes of
                                Just TyVar{} -> True
                                _ -> False
                            ]
                        isSchemeRootAliasSolved nid =
                            case VarStore.lookupVarBound constraint (canonical nid) of
                                Just bnd ->
                                    let bndC = canonical bnd
                                    in IntSet.member (getNodeId bndC) schemeRootKeySet
                                        || IntMap.member (getNodeId bndC) schemeRootByBody
                                Nothing -> False
                        baseSchemeRootSet =
                            IntSet.fromList
                                [ getNodeId root
                                | gen <- IntMap.elems (cGenNodes baseConstraint)
                                , root <- gnSchemes gen
                                ]
                        isSchemeRootAliasBase baseKey =
                            case IntMap.lookup baseKey baseNodes of
                                Just TyVar{ tnBound = Just bnd } ->
                                    IntSet.member (getNodeId bnd) baseSchemeRootSet
                                _ -> False
                        baseSchemeAliases =
                            let keys = IntSet.toList baseGammaSetLocal
                            in filter isSchemeRootAliasBase keys
                        solvedSchemeAliases =
                            let keys = map getNodeId solvedBindersUnderScope
                            in filter (\k -> isSchemeRootAliasSolved (NodeId k)) keys
                        scopeAliasOverrides =
                            IntMap.fromList
                                [ (solvedKey, NodeId baseKey)
                                | (solvedKey, baseKey) <- zip solvedSchemeAliases baseSchemeAliases
                                ]
                        alignSolvedToBase =
                            case ( Binding.boundFlexChildren baseConstraint (GenRef gid)
                                 , Binding.boundFlexChildrenUnder canonical constraint (GenRef gid)
                                 ) of
                                (Right baseBinders, Right solvedBinders) ->
                                    IntMap.fromList
                                        [ (getNodeId (canonical solvedB), NodeId (getNodeId baseB))
                                        | (baseB, solvedB) <- zip baseBinders solvedBinders
                                        ]
                                _ -> IntMap.empty
                        solvedBinderKeys =
                            IntSet.fromList (map getNodeId solvedBindersUnderScope)
                        alignPrefer =
                            IntMap.filterWithKey (\k _ -> IntSet.member k solvedBinderKeys) alignSolvedToBase
                        solvedToBasePrefLocal' =
                            IntMap.union alignPrefer
                                (IntMap.union scopeAliasOverrides solvedToBasePrefLocal)
                        namedUnderGaSetLocal =
                            IntSet.union
                                (IntSet.fromList
                                    [ solvedKey
                                    | solvedKey <- IntMap.elems baseGammaRepLocal
                                    ])
                                namedUnderGaInterior
                    in traceGeneralize env
                        ("generalizeAt: baseGammaSet="
                            ++ show (IntSet.toList baseGammaSetLocal)
                            ++ " baseGammaPick="
                            ++ show
                                [ ( baseKey
                                  , IntMap.findWithDefault [] baseKey solvedByBasePref
                                  , pickSolved baseKey
                                  , IntMap.lookup baseKey nodes
                                  )
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                ]
                            ++ " solvedToBasePref[6]="
                            ++ show (IntMap.lookup 6 solvedToBasePrefLocal')
                            ++ " scopeAliasOverrides="
                            ++ show (IntMap.toList scopeAliasOverrides)
                            ++ " baseGammaRep="
                            ++ show (IntMap.toList baseGammaRepLocal)
                            ++ " namedUnderGaSet="
                            ++ show (IntSet.toList namedUnderGaSetLocal)
                        )
                        (baseGammaSetLocal, baseGammaRepLocal, namedUnderGaSetLocal, solvedToBasePrefLocal')
                (Nothing, Just ga) ->
                    ( IntSet.empty
                    , IntMap.empty
                    , IntSet.union
                        (IntSet.fromList
                            [ getNodeId nid
                            | nid <- namedUnderGaRaw
                            , not (IntSet.member (getNodeId (canonical nid)) nestedSchemeInteriorSet)
                            ])
                        namedUnderGaInterior
                    , gaSolvedToBase ga
                    )
                _ ->
                    ( IntSet.empty
                    , IntMap.empty
                    , IntSet.union
                        (IntSet.fromList
                            [ getNodeId nid
                            | nid <- namedUnderGaRaw
                            , not (IntSet.member (getNodeId (canonical nid)) nestedSchemeInteriorSet)
                            ])
                        namedUnderGaInterior
                    , IntMap.empty
                    )
        gammaAlias =
            case mbBindParentsGa of
                Just ga ->
                    let baseToSolved = gaBaseToSolved ga
                        aliasEligible solvedKey =
                            case scopeGen of
                                Nothing -> True
                                Just gid ->
                                    let underSolved =
                                            firstGenAncestorGa (typeRef (NodeId solvedKey)) == Just gid
                                        underBasePref =
                                            case IntMap.lookup solvedKey solvedToBasePref of
                                                Just baseN ->
                                                    firstGenAncestor (gaBindParentsBase ga) (TypeRef baseN) == Just gid
                                                Nothing -> False
                                        underBaseGamma =
                                            case IntMap.lookup solvedKey solvedToBasePref of
                                                Just baseN -> IntSet.member (getNodeId baseN) baseGammaSet
                                                Nothing -> False
                                    in underSolved || underBasePref || underBaseGamma
                        solvedToBaseAll =
                            IntMap.fromListWith
                                (++)
                                [ (getNodeId (canonical solvedNid), [baseKey])
                                | (baseKey, solvedNid) <- IntMap.toList baseToSolved
                                ]
                        pickBaseGamma baseKeys =
                            listToMaybe
                                [ baseKey
                                | baseKey <- sort baseKeys
                                , IntSet.member baseKey baseGammaSet
                                ]
                        aliasFromBase =
                            IntMap.fromList
                                [ (solvedKey, repKey)
                                | (solvedKey, baseKeys) <- IntMap.toList solvedToBaseAll
                                , aliasEligible solvedKey
                                , Just baseKey <- [pickBaseGamma baseKeys]
                                , Just repKey <- [IntMap.lookup baseKey baseGammaRep]
                                ]
                        aliasFromPref =
                            IntMap.fromList
                                [ (solvedKeyC, repKey)
                                | (solvedKey, node) <- IntMap.toList nodes
                                , case node of
                                    TyVar{} -> True
                                    _ -> False
                                , let solvedKeyC = getNodeId (canonical (NodeId solvedKey))
                                , aliasEligible solvedKeyC
                                , Just baseNid <- [IntMap.lookup solvedKeyC solvedToBasePref]
                                , let baseKey = getNodeId baseNid
                                , IntSet.member baseKey baseGammaSet
                                , Just repKey <- [IntMap.lookup baseKey baseGammaRep]
                                ]
                    in IntMap.union aliasFromBase aliasFromPref
                Nothing -> IntMap.empty
        baseGammaRepSet =
            IntSet.fromList (IntMap.elems baseGammaRep)
        reachableForBinders =
            let aliasReachable =
                    [ repKey
                    | (aliasKey, repKey) <- IntMap.toList gammaAlias
                    , IntSet.member aliasKey reachableForBinders0
                    ]
                typeRootC = canonical typeRoot0
                schemeBodyAliasReachable =
                    [ getNodeId (canonical (NodeId vidKey))
                    | (vidKey, node) <- IntMap.toList nodes
                    , TyVar{} <- [node]
                    , case VarStore.lookupVarBound constraint (NodeId vidKey) of
                        Just bnd ->
                            let bndC = canonical bnd
                            in bndC == typeRootC
                        Nothing -> False
                    ]
            in IntSet.union
                reachableForBinders0
                (IntSet.fromList (aliasReachable ++ schemeBodyAliasReachable))
        gammaKeyFor binderKey k =
            case IntMap.lookup k gammaAlias of
                Just repKey | repKey == binderKey -> k
                Just repKey -> repKey
                Nothing -> k
        namedUnderGa =
            [ NodeId nid
            | nid <- IntSet.toList namedUnderGaSet
            ]
        boundHasNamedOutsideGamma =
            case targetBound of
                Just bnd ->
                    let reachableBound = reachableFromWithBounds bnd
                        targetKey = getNodeId (canonical target0)
                        isNamedOutside nidInt =
                            let nidC = canonical (NodeId nidInt)
                                keyC = getNodeId nidC
                            in case IntMap.lookup keyC nodes of
                                Just TyVar{} ->
                                    if IntSet.member keyC nestedSchemeInteriorSet
                                        then False
                                        else case IntMap.lookup (nodeRefKey (typeRef nidC)) bindParents of
                                            Just (GenRef _, _) ->
                                                not (IntSet.member (gammaKeyFor targetKey keyC) namedUnderGaSet)
                                            _ -> False
                                _ -> False
                    in any isNamedOutside (IntSet.toList reachableBound)
                Nothing -> False
        typeRootHasNamedOutsideGamma = False
    let dropTarget =
            not targetIsSchemeRoot &&
            case IntMap.lookup (getNodeId target0) nodes of
                Just TyVar{} ->
                    case targetBound >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                        Just TyVar{} ->
                            allowDropTarget &&
                                targetRigid &&
                                (boundIsBase || boundIsStructural) &&
                                (boundIsVar || boundIsChild) &&
                                not boundHasNestedGen &&
                                not (boundIsSchemeRootAll target0) &&
                                not boundHasNamedOutsideGamma &&
                                case targetBound of
                                    Just bndVar -> not (hasExplicitBound bndVar)
                                    Nothing -> False
                        Just _ ->
                            allowDropTarget &&
                                boundIsBase &&
                                not boundMentionsTarget &&
                                not boundHasForall &&
                                not boundHasNestedGen &&
                                not boundHasNamedOutsideGamma
                        Nothing -> False
                Just _ -> False
                Nothing -> False
        schemeRoots =
            case scopeRootC of
                GenRef _ | dropTarget -> IntSet.singleton (canonKey target0)
                _ -> IntSet.empty
        liftToForall bnd0 =
            let climbToForall cur =
                    case IntMap.lookup (nodeRefKey (typeRef (canonical cur))) bindParents of
                        Just (TypeRef parent, _) ->
                            case IntMap.lookup (canonKey parent) nodes of
                                Just TyForall{} -> climbToForall (canonical parent)
                                _ -> cur
                        _ -> cur
            in climbToForall bnd0
        useBoundTypeRoot =
            not targetIsSchemeRoot &&
            case targetBound of
                Just bnd ->
                    IntMap.member (getNodeId (canonical bnd)) schemeRootByBody
                Nothing -> False
        schemeBodyRoot =
            case targetBound of
                Just bnd ->
                    case IntMap.lookup (getNodeId (canonical bnd)) nodes of
                        Just TyForall{ tnBody = b } -> canonical b
                        _ -> canonical bnd
                Nothing -> typeRoot0
        useSchemeBodyForScope = False
        targetInGamma =
            IntSet.member (canonKey target0) namedUnderGaSet
        targetIsBaseLike =
            isBaseLikeKey (canonKey target0)
        schemeBodyChildUnderGen =
            case scopeRootC of
                GenRef gid | targetIsSchemeRootForScope && targetIsTyVar ->
                    let children =
                            [ canonical child
                            | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents
                            , parentRef == GenRef gid
                            , TypeRef child <- [nodeRefFromKey childKey]
                            , not (isTyVarKey (canonKey child))
                            ]
                    in case children of
                        [child] -> Just child
                        _ -> Nothing
                _ -> Nothing
        boundRootForType bnd0 =
            boundRootWith
                getNodeId
                canonical
                (`IntMap.lookup` nodes)
                (VarStore.lookupVarBound constraint)
                (const Nothing)
                True
                bnd0
        typeRootFromTargetBound =
            case (allowDropTarget, scopeGen, targetIsTyVar, targetBound) of
                (True, Just _, True, Just bnd) ->
                    let targetUnderScope =
                            case (scopeGen, IntMap.lookup (nodeRefKey (typeRef target0)) bindParents) of
                                (Just gid, Just (GenRef gid', _)) -> gid' == gid
                                _ -> False
                        targetIsSchemeRootAlias =
                            let bndC = canonical bnd
                                bndKey = getNodeId bndC
                            in (IntSet.member bndKey schemeRootKeySet
                                || IntMap.member bndKey schemeRootByBody)
                                && not targetIsSchemeRootForScope
                        root = boundRootForType bnd
                        useBoundRoot =
                            not targetUnderScope
                                || targetIsSchemeRootAlias
                                || boundUnderOtherGen
                                || (boundIsDirectChild && not targetIsSchemeRootForScope && not targetInGamma)
                    in if useBoundRoot
                            && canonical root /= canonical target0
                        then Just root
                        else Nothing
                _ -> Nothing
        typeRoot0' =
            if useSchemeBodyForScope
                then schemeBodyRoot
                else
                    case (scopeRootC, targetIsSchemeRootForScope, targetIsTyVar, targetBound) of
                        (GenRef _, True, True, Nothing) ->
                            case schemeBodyChildUnderGen of
                                Just child -> child
                                Nothing -> schemeBodyRoot
                        _ ->
                            case (dropTarget || useBoundTypeRoot, targetBound) of
                                (True, Just bnd) -> liftToForall bnd
                                _ ->
                                    case typeRootFromTargetBound of
                                        Just v -> v
                                        Nothing ->
                                            case typeRootFromBoundVar of
                                                Just v
                                                    | targetIsTyVar
                                                        && not (boundHasForallForVar v) -> v
                                                Just v
                                                    | not targetIsTyVar
                                                        && not (boundHasForallForVar v)
                                                        && not targetIsBaseLike
                                                        && not targetIsSchemeRootForScope -> v
                                                Just v
                                                    | targetIsTyVar
                                                        && targetBoundUnderOtherGen -> v
                                                Just v
                                                    | typeRootHasNamedOutsideGamma -> v
                                                _ -> typeRoot0
        typeRoot =
            case lookupNode (canonKey typeRoot0') of
                Just TyForall{ tnBody = b }
                    | targetIsTyVar
                    , targetIsSchemeRootForScope
                    , Just gid <- scopeGen
                    , Just gidOwner <- IntMap.lookup (canonKey typeRoot0') schemeRootOwner
                    , gid == gidOwner ->
                        canonical b
                _ -> typeRoot0'
    let dropTypeRoot =
            dropTarget &&
            boundIsSchemeRoot &&
            not (isTyVarKey (canonKey typeRoot))
    reachableType <- Right (reachableFromWithBounds typeRoot)
    reachableTypeStructural <- Right (reachableFromStructural typeRoot)
    let typeRootIsForall =
            isTyForallKey (canonKey typeRoot)
    let forallBoundBinders =
            IntSet.fromList
                [ canonKey child
                | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents
                , TypeRef parent <- [parentRef]
                , IntSet.member (canonKey parent) reachableType
                , case IntMap.lookup (canonKey parent) nodes of
                    Just TyForall{} -> True
                    _ -> False
                , IntSet.member (canonKey parent) schemeRootKeySet
                , TypeRef child <- [nodeRefFromKey childKey]
                , isTyVarKey (canonKey child)
                ]
    let binders0Adjusted =
            let freeVarsFromBound =
                    case targetBound of
                        Just bnd ->
                            [ canonical (NodeId nid)
                            | nid <- IntSet.toList (reachableFromWithBounds bnd)
                            , case IntMap.lookup nid nodes of
                                Just TyVar{} -> not (VarStore.isEliminatedVar constraint (NodeId nid))
                                _ -> False
                            ]
                        Nothing -> []
                targetC = canonical target0
                activeBinders =
                    [ v
                    | v <- binders0
                    , IntSet.member (canonKey v) reachableForBinders
                    ]
                onlyTarget = case activeBinders of
                    [] -> True
                    [v] -> canonical v == targetC
                    _ -> False
                extras =
                    if targetIsSchemeRoot && onlyTarget && not (boundIsSchemeRootAll target0)
                        then freeVarsFromBound
                        else []
            in binders0 ++ extras ++ namedUnderGa
    traceGeneralizeM env
        ("generalizeAt: bindersAdjusted=" ++ show binders0Adjusted
            ++ " reachable=" ++ show (IntSet.toList reachableForBinders)
        )
    let isTargetSchemeBinder v =
            if targetIsBaseLike
                then False
                else
                    canonical v == canonical target0
                        || case VarStore.lookupVarBound constraint (canonical v) of
                            Just bnd -> canonical bnd == canonical target0
                            Nothing -> False
    let extraReachable =
            case (scopeGen, IntMap.lookup (getNodeId typeRoot0) nodes) of
                (Just _, _) -> []
                (_, Just TyVar{}) -> []
                (Nothing, _) ->
                    [ canonical v
                    | nid <- IntSet.toList reachable
                    , Just TyVar{} <- [IntMap.lookup nid nodes]
                    , let v = NodeId nid
                    , not (VarStore.isEliminatedVar constraint (canonical v))
                    , not (elem (canonical v) binders0Adjusted)
                    , case IntMap.lookup (nodeRefKey (typeRef (canonical v))) bindParents of
                        Just (GenRef _, _) -> False
                        _ -> True
                    ]
        bindersCandidates = binders0Adjusted ++ extraReachable ++ namedUnderGa
        canonicalizeBinder v =
            let vC = canonical v
                vKey = getNodeId vC
            in case IntMap.lookup vKey gammaAlias of
                Just repKey
                    | IntSet.member vKey baseGammaRepSet -> vC
                    | otherwise -> canonical (NodeId repKey)
                Nothing -> vC
        normalizedBinders =
            [ canonicalizeBinder v
            | v <- bindersCandidates
            ]
        bindersCandidatesCanonical =
            IntMap.elems $
                IntMap.fromList
                    [ (getNodeId v, v)
                    | v <- normalizedBinders
                    ]
    let typeRootForScheme = liftToForall typeRoot
        isSchemeRootAlias v =
            IntSet.member (getNodeId (canonical v)) schemeRootKeySet
                && canonical v /= canonical target0
                && case VarStore.lookupVarBound constraint (canonical v) of
                    Just bnd ->
                        let bndC = canonical bnd
                        in bndC == canonical typeRoot || bndC == canonical typeRootForScheme
                    Nothing -> False
        boundIsSchemeBodyAlias v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    let bndC = canonical bnd
                    in bndC == canonical typeRoot || bndC == canonical typeRootForScheme
                Nothing -> False
        boundIsStructuralAlias v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    let bndC = canonical bnd
                        isStruct =
                            case IntMap.lookup (getNodeId bndC) nodes of
                                Just TyArrow{} -> True
                                Just TyForall{} -> True
                                Just TyExp{} -> True
                                _ -> False
                    in (bndC == canonical typeRoot || bndC == canonical typeRootForScheme) && isStruct
                Nothing -> False
        boundIsTypeRootAlias v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    let bndC = canonical bnd
                    in bndC == canonical typeRoot || bndC == canonical typeRootForScheme
                Nothing -> False
        targetBoundIsTypeRootAlias =
            case targetBound of
                Just bnd ->
                    let bndC = canonical bnd
                    in bndC == canonical typeRoot || bndC == canonical typeRootForScheme
                Nothing -> False
        boundHasNamedOutsideGammaFor v =
            case mbBindParentsGa of
                Just ga
                    | Just baseK <- IntMap.lookup (getNodeId (canonical v)) solvedToBasePref ->
                        let baseConstraint = gaBaseConstraint ga
                            baseNodes = cNodes baseConstraint
                            baseParents = gaBindParentsBase ga
                            scopeGenBase = scopeGen
                            boundSchemeOwnerBase bnd =
                                case IntMap.lookup (getNodeId bnd) schemeRootOwnerBase of
                                    Just gid -> Just gid
                                    Nothing ->
                                        case IntMap.lookup (getNodeId bnd) schemeRootByBodyBase of
                                            Just root ->
                                                IntMap.lookup (getNodeId root) schemeRootOwnerBase
                                            Nothing -> Nothing
                            allowBoundTraversalBase bnd =
                                case boundSchemeOwnerBase bnd of
                                    Nothing -> True
                                    Just gid ->
                                        case scopeGenBase of
                                            Just scopeGid -> gid == scopeGid
                                            Nothing -> False
                            reachableFromWithBoundsBase root0 =
                                let children nid =
                                        case IntMap.lookup (getNodeId nid) baseNodes of
                                            Nothing -> []
                                            Just node ->
                                                let boundKids =
                                                        case node of
                                                            TyVar{ tnBound = Just bnd }
                                                                | allowBoundTraversalBase bnd -> [bnd]
                                                            _ -> []
                                                in structuralChildren node ++ boundKids
                                in reachableFrom getNodeId id children root0
                            isNamedOutsideBase nidInt =
                                case IntMap.lookup nidInt baseNodes of
                                    Just TyVar{} ->
                                        case IntMap.lookup (nodeRefKey (TypeRef (NodeId nidInt))) baseParents of
                                            Just (GenRef _, _) ->
                                                not (IntSet.member nidInt baseGammaSet)
                                            _ -> False
                                    _ -> False
                        in case VarStore.lookupVarBound baseConstraint baseK of
                            Just bnd ->
                                let reachableBound = reachableFromWithBoundsBase bnd
                                in any isNamedOutsideBase (IntSet.toList reachableBound)
                            Nothing -> False
                _ ->
                    case VarStore.lookupVarBound constraint (canonical v) of
                        Just bnd ->
                            let reachableBound = reachableFromWithBounds bnd
                                binderKey = getNodeId (canonical v)
                                isNamedOutside nidInt =
                                    let nidC = canonical (NodeId nidInt)
                                        keyC = getNodeId nidC
                                    in case IntMap.lookup keyC nodes of
                                        Just TyVar{} ->
                                            if IntSet.member keyC nestedSchemeInteriorSet
                                                then False
                                                else case IntMap.lookup (nodeRefKey (typeRef nidC)) bindParents of
                                                    Just (GenRef _, _) ->
                                                        not (IntSet.member (gammaKeyFor binderKey keyC) namedUnderGaSet)
                                                    _ -> False
                                        _ -> False
                            in any isNamedOutside (IntSet.toList reachableBound)
                        Nothing -> False
        boundMentionsSelfAlias v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    let reachableBound = reachableFromWithBounds bnd
                        binderKey = getNodeId (canonical v)
                        mentionsSelf nidInt =
                            let nidC = canonical (NodeId nidInt)
                                keyC = getNodeId nidC
                            in if IntSet.member keyC nestedSchemeInteriorSet
                                then False
                                else case IntMap.lookup keyC nodes of
                                    Just TyVar{} ->
                                        case IntMap.lookup keyC gammaAlias of
                                            Just repKey -> repKey == binderKey
                                            Nothing -> False
                                    _ -> False
                    in any mentionsSelf (IntSet.toList reachableBound)
                Nothing -> False
        aliasBoundIsBottomOrNone v =
            isSchemeRootAlias v
                && not (isTargetSchemeBinder v)
                && case VarStore.lookupVarBound constraint (canonical v) of
                    Nothing -> True
                    Just bnd ->
                        let bndC = canonical bnd
                        in case (IntMap.lookup (getNodeId bndC) nodes, VarStore.lookupVarBound constraint bndC) of
                            (Just TyVar{}, Nothing) -> True
                            (Just TyBottom{}, _) -> True
                            _ -> False
        boundIsVarAlias v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    case IntMap.lookup (getNodeId (canonical bnd)) nodes of
                        Just TyVar{} -> True
                        _ -> False
                Nothing -> False
        isTypeRootBinder v =
            case typeRootFromBoundVar of
                Just v0 -> canonical v == canonical v0
                Nothing -> False
        aliasBinderIsTrivial v =
            not (isTargetSchemeBinder v)
                && not (isTypeRootBinder v)
                && boundIsVarAlias v
                && not (IntSet.member (getNodeId (canonical v)) reachableTypeStructural)
                && not (boundHasNamedOutsideGammaFor v)
                && not (boundMentionsSelfAlias v)
        aliasBinderIsRedundant v inGamma =
            not (isTargetSchemeBinder v)
                && not (isTypeRootBinder v)
                && (isSchemeRootAlias v || boundIsTypeRootAlias v)
                && not (IntSet.member (getNodeId (canonical v)) reachableType)
                && (not inGamma || boundIsTypeRootAlias v)
                && not (boundIsSchemeBodyAlias v)
        dropTargetAliasBinder v inGamma =
            dropTarget
                && not inGamma
                && canonical target0 == canonical v
                && canonical typeRoot /= canonical target0
                && targetBoundIsTypeRootAlias
                && (boundUnderOtherGen || not (boundHasNamedOutsideGammaFor v))
                && not (boundMentionsSelfAlias v)
    let binderCandidateKeys =
            IntSet.fromList [ canonKey v | v <- bindersCandidatesCanonical ]
    let binders =
            [ canonical v
            | v <- bindersCandidatesCanonical
            , let vKey = canonKey v
            , let gammaKey =
                    case IntMap.lookup vKey gammaAlias of
                        Just repKey -> repKey
                        Nothing -> vKey
            , let keepTypeRootBinder =
                    case typeRootFromBoundVar of
                        Just v0 -> canonKey v0 == vKey
                        Nothing -> False
            , let inGamma =
                    case mbBindParentsGa of
                        Just _ ->
                            let keysToCheck = [vKey, gammaKey]
                                inBase =
                                    any
                                        (\k ->
                                            case IntMap.lookup k solvedToBasePref of
                                                Just baseN -> IntSet.member (getNodeId baseN) baseGammaSet
                                                Nothing -> False
                                        )
                                        keysToCheck
                            in inBase || IntSet.member gammaKey namedUnderGaSet
                        Nothing ->
                            IntSet.member gammaKey namedUnderGaSet
                                || IntSet.member gammaKey aliasBinderBases
                                || IntSet.member vKey aliasBinderBases
            , case scopeRootC of
                GenRef _ ->
                    (inGamma || IntSet.member vKey aliasBinderBases)
                        && (IntSet.member vKey reachableForBinders || isTargetSchemeBinder v)
                TypeRef _ -> IntSet.member vKey reachableForBinders
            , case mbBindParentsGa of
                Just _ ->
                    if keepTypeRootBinder
                        then True
                        else
                            case IntMap.lookup vKey solvedToBasePref of
                                Just baseN ->
                                    case IntMap.lookup (getNodeId baseN) baseGammaRep of
                                        Just repKey ->
                                            repKey == vKey
                                                || not (IntSet.member repKey binderCandidateKeys)
                                                || IntSet.member vKey baseGammaRepSet
                                        Nothing -> True
                                Nothing -> True
                Nothing -> True
            , not (IntSet.member (canonKey v) nestedSchemeInteriorSet
                && not (isTargetSchemeBinder v)
                && not (IntSet.member vKey aliasBinderBases))
            , not (isNestedSchemeBound v && not inGamma && not (isTargetSchemeBinder v))
            , not (boundIsSchemeRootVar v && not (isTargetSchemeBinder v) && not (hasExplicitBound v) && not inGamma)
            , not (boundIsSchemeRootAll v
                && not (isTargetSchemeBinder v)
                && not (boundHasNamedOutsideGammaFor v)
                && not (IntSet.member (canonKey v) reachableForBinders)
                )
            , not (isSchemeRootAlias v
                && not (isTargetSchemeBinder v)
                && not (boundHasNamedOutsideGammaFor v)
                && not (boundMentionsSelfAlias v)
                && (not inGamma || boundIsTypeRootAlias v)
                )
            , not (boundIsSchemeBodyAlias v
                && not (isTargetSchemeBinder v)
                && not inGamma
                )
            , not (boundIsStructuralAlias v
                && canonical v /= canonical target0
                && not (boundIsSchemeBodyAlias v)
                )
            , not (aliasBoundIsBottomOrNone v && not inGamma)
            , not (aliasBinderIsTrivial v)
            , not (aliasBinderIsRedundant v inGamma)
            , not (boundUnderOtherGen
                && canonical v == canonical target0
                && targetBoundIsTypeRootAlias
                && dropTarget)
            , not (dropTargetAliasBinder v inGamma)
            , not (IntSet.member (canonKey v) schemeRoots && not inGamma)
            , not (dropTypeRoot && canonical v == canonical typeRoot)
            , not (IntSet.member (canonKey v) forallBoundBinders && not typeRootIsForall)
            ]
    traceGeneralizeM env
        ("generalizeAt: binder filters="
            ++ show
                [ let vKey = canonKey v
                      gammaKey =
                          case IntMap.lookup vKey gammaAlias of
                              Just repKey -> repKey
                              Nothing -> vKey
                      inGammaDbg =
                          case mbBindParentsGa of
                              Just _ ->
                                  let keysToCheck = [vKey, gammaKey]
                                      inBase =
                                            any
                                                (\k ->
                                                    case IntMap.lookup k solvedToBasePref of
                                                        Just baseN -> IntSet.member (getNodeId baseN) baseGammaSet
                                                        Nothing -> False
                                                )
                                                keysToCheck
                                  in inBase
                              Nothing -> IntSet.member gammaKey namedUnderGaSet
                  in ( v
                     , IntSet.member vKey reachable
                     , isNestedSchemeBound v
                     , boundIsSchemeRootVar v
                     , boundIsSchemeRootAll v
                     , boundIsTypeRootAlias v
                     , boundUnderOtherGen
                     , IntSet.member vKey schemeRoots
                     , dropTypeRoot && canonical v == canonical typeRoot
                     , IntSet.member vKey forallBoundBinders
                     , inGammaDbg
                     , IntMap.lookup (nodeRefKey (typeRef (canonical v))) bindParents
                     )
                | v <- binders0Adjusted
                ]
        )
    let requiredGammaBinders :: [NodeId]
        requiredGammaBinders = []
        bindersWithRequired =
            binders
        binders' =
            traceGeneralize env
                ("generalizeAt: bindersFiltered=" ++ show binders
                    ++ " requiredGammaBinders=" ++ show requiredGammaBinders
                    ++ " dropTarget=" ++ show dropTarget
                    ++ " dropTypeRoot=" ++ show dropTypeRoot
                    ++ " typeRoot=" ++ show typeRoot
                )
                bindersWithRequired
        bindersCanon =
            IntMap.elems $
                IntMap.fromList
                    [ (getNodeId v, v)
                    | v <- binders'
                    ]
    -- Phase 6: dependency analysis and binder ordering.
    let binderIds = map getNodeId bindersCanon
        candidateSet = IntSet.fromList binderIds
        nameForDep k = "t" ++ show k
        substDeps =
            IntMap.fromList
                [ (k, nameForDep k)
                | k <- binderIds
                ]
        substDepsBase =
            case mbBindParentsGa of
                Just _ ->
                    IntMap.fromListWith
                        (\_ old -> old)
                        [ (getNodeId baseN, nameForDep k)
                        | k <- binderIds
                        , Just baseN <- [IntMap.lookup k solvedToBasePref]
                        ]
                Nothing -> IntMap.empty
        nameToId = parseNameId
        substDepsFor k =
            IntMap.union substDeps $
                IntMap.fromList
                    [ (aliasKey, nameForDep binderKey)
                    | (aliasKey, binderKey) <- IntMap.toList gammaAlias
                    , aliasKey /= binderKey
                    , binderKey /= k
                    ]
        boundRootForDeps bnd0 =
            boundRootWith
                getNodeId
                canonical
                (`IntMap.lookup` nodes)
                (VarStore.lookupVarBound constraint)
                (`IntMap.lookup` schemeRootByBody)
                False
                bnd0
        boundDepsForCandidate k =
            let isBaseRep =
                    case IntMap.lookup k solvedToBasePref of
                        Just baseK ->
                            case IntMap.lookup (getNodeId baseK) baseGammaRep of
                                Just repKey -> repKey == k
                                Nothing -> False
                        Nothing -> False
            in case mbBindParentsGa of
                Just ga
                    | Just baseK <- IntMap.lookup k solvedToBasePref
                    , isBaseRep ->
                        let baseConstraint = gaBaseConstraint ga
                            baseNodes = cNodes baseConstraint
                            boundRootForDepsBase bnd0 =
                                boundRootWith
                                    getNodeId
                                    id
                                    (`IntMap.lookup` baseNodes)
                                    (VarStore.lookupVarBound baseConstraint)
                                    (`IntMap.lookup` schemeRootByBodyBase)
                                    False
                                    bnd0
                        in case VarStore.lookupVarBound baseConstraint baseK of
                            Nothing -> do
                                let boundTy = TVar (nameForDep k)
                                    freeNames = Set.toList (freeNamesFrom Set.empty boundTy)
                                    deps =
                                        [ dep
                                        | name <- freeNames
                                        , Just dep <- [nameToId name]
                                        , dep /= k
                                        , IntSet.member dep candidateSet
                                        ]
                                pure deps
                            Just bnd -> do
                                let bndRoot = boundRootForDepsBase bnd
                                boundTy <- reifyBoundWithNamesOnConstraint baseConstraint substDepsBase bndRoot
                                let bndRootKey = getNodeId bndRoot
                                    freeNames0 = Set.toList (freeNamesFrom Set.empty boundTy)
                                    freeNames =
                                        case (boundTy, IntMap.lookup bndRootKey baseNodes, VarStore.lookupVarBound baseConstraint bndRoot) of
                                            (TBottom, Just TyVar{}, Nothing) ->
                                                [nameForDep bndRootKey]
                                            _ -> freeNames0
                                    deps =
                                        [ dep
                                        | name <- freeNames
                                        , Just dep <- [nameToId name]
                                        , dep /= k
                                        , IntSet.member dep candidateSet
                                        ]
                                traceGeneralizeM env
                                    ("generalizeAt: boundDeps k="
                                        ++ show k
                                        ++ " bndRoot="
                                        ++ show bndRoot
                                        ++ " boundTy="
                                        ++ show boundTy
                                        ++ " freeNames="
                                        ++ show freeNames
                                        ++ " deps="
                                        ++ show deps
                                    )
                                pure deps
                _ ->
                    case VarStore.lookupVarBound constraint (canonical (NodeId k)) of
                        Nothing -> do
                            let boundTy = TVar (nameForDep k)
                                freeNames = Set.toList (freeNamesFrom Set.empty boundTy)
                                deps =
                                    [ dep
                                    | name <- freeNames
                                    , Just dep <- [nameToId name]
                                    , dep /= k
                                    , IntSet.member dep candidateSet
                                    ]
                            pure deps
                        Just bnd -> do
                            let bndRoot = boundRootForDeps bnd
                            boundTy <- reifyBoundWithNames resForReify (substDepsFor k) bndRoot
                            let bndRootC = canonical bndRoot
                                bndRootKey = getNodeId bndRootC
                                freeNames0 = Set.toList (freeNamesFrom Set.empty boundTy)
                                freeNames =
                                    case (boundTy, IntMap.lookup bndRootKey nodes, VarStore.lookupVarBound constraint bndRootC) of
                                        (TBottom, Just TyVar{}, Nothing) ->
                                            [nameForDep bndRootKey]
                                        _ -> freeNames0
                                deps =
                                    [ dep
                                    | name <- freeNames
                                    , Just dep <- [nameToId name]
                                    , dep /= k
                                    , IntSet.member dep candidateSet
                                    ]
                            traceGeneralizeM env
                                ("generalizeAt: boundDeps k="
                                    ++ show k
                                    ++ " bndRoot="
                                    ++ show bndRoot
                                    ++ " boundTy="
                                    ++ show boundTy
                                    ++ " freeNames="
                                    ++ show freeNames
                                    ++ " deps="
                                    ++ show deps
                                )
                            pure deps
        orderBinders candidates =
            orderBinderCandidates
                canonical
                constraint
                orderRootForBinders
                orderRootBaseForBinders
                candidates
                boundDepsForCandidate

    ordered0 <- orderBinders binderIds
    traceGeneralizeM env
        ("generalizeAt: binderIds=" ++ show binderIds
            ++ " ordered0=" ++ show ordered0
        )
    let names = zipWith alphaName [0..] ordered0
        subst0 = IntMap.fromList (zip ordered0 names)

    let extraCandidates =
            case scopeRootC of
                GenRef _ -> []
                TypeRef _ ->
                    case IntMap.lookup (getNodeId (canonical typeRoot)) nodes of
                        Just TyForall{} ->
                            [ canonical child
                            | (childKey, (parent, _flag)) <- IntMap.toList bindParents
                            , parent == typeRef (canonical typeRoot)
                            , TypeRef child <- [nodeRefFromKey childKey]
                            , isQuantifiable child
                            , not (IntMap.member (getNodeId (canonical child)) subst0)
                            ]
                        _ -> []
    orderedExtra <- orderBinders (map getNodeId extraCandidates)
    -- Phase 7: substitution maps (base, aliases, and bound-specific views).
    let extraNames = zipWith alphaName [length names ..] orderedExtra
        substExtra = IntMap.fromList (zip orderedExtra extraNames)
        substBase = IntMap.unions [subst0, substExtra]
        substAliases =
            IntMap.fromList
                [ (aliasKey, name)
                | (aliasKey, binderKey) <- IntMap.toList gammaAlias
                , aliasKey /= binderKey
                , not (IntSet.member aliasKey nestedSchemeInteriorSet)
                , Just name <- [IntMap.lookup binderKey substBase]
                ]
        substAliasesCanon =
            IntMap.fromList
                [ (aliasKeyC, name)
                | (aliasKey, name) <- IntMap.toList substAliases
                , let aliasKeyC = getNodeId (canonical (NodeId aliasKey))
                , aliasKeyC /= aliasKey
                , not (IntMap.member aliasKeyC substBase)
                , not (IntMap.member aliasKeyC substAliases)
                ]
        substAliasesFromBase =
            IntMap.fromList
                [ (solvedKey, name)
                | (solvedKey, baseN) <- IntMap.toList solvedToBasePref
                , let baseKey = getNodeId baseN
                , Just repKey <- [IntMap.lookup baseKey baseGammaRep]
                , Just name <- [IntMap.lookup repKey substBase]
                , solvedKey /= repKey
                , not (IntSet.member solvedKey nestedSchemeInteriorSet)
                ]
        subst = IntMap.unions [substBase, substAliases, substAliasesCanon, substAliasesFromBase]
        aliasBinderKeys = aliasBinderBases
        filterAliasKeys =
            if IntSet.null aliasBinderKeys
                then id
                else IntMap.filterWithKey (\k _ -> not (IntSet.member k aliasBinderKeys))
        substAliasesFor _binderKey =
            IntMap.fromList
                [ (aliasKey, name)
                | (aliasKey, binderKey') <- IntMap.toList gammaAlias
                , aliasKey /= binderKey'
                , not (IntSet.member aliasKey nestedSchemeInteriorSet)
                , Just name <- [IntMap.lookup binderKey' substBase]
                ]
        substForBound binderKey =
            filterAliasKeys $
                IntMap.union substBase (substAliasesFor binderKey)
        substBaseByKey =
            case mbBindParentsGa of
                Just ga ->
                    let fromBaseRep =
                            [ (baseKey, name)
                            | (baseKey, solvedKey) <- IntMap.toList baseGammaRep
                            , Just name <- [IntMap.lookup solvedKey substBase]
                            ]
                        fromSolved =
                            [ (getNodeId baseN, name)
                            | (solvedKey, name) <- IntMap.toList substBase
                            , Just baseN <- [IntMap.lookup solvedKey solvedToBasePref]
                            ]
                        fromBaseToSolved =
                            [ (baseKey, name)
                            | (baseKey, solvedN) <- IntMap.toList (gaBaseToSolved ga)
                            , let solvedKey = getNodeId (canonical solvedN)
                            , Just name <- [IntMap.lookup solvedKey substBase]
                            ]
                    in IntMap.unions
                        [ IntMap.fromListWith (\_ old -> old) fromBaseRep
                        , IntMap.fromListWith (\_ old -> old) fromSolved
                        , IntMap.fromListWith (\_ old -> old) fromBaseToSolved
                        ]
                Nothing -> IntMap.empty
        substForBoundBase _binderKey = filterAliasKeys substBaseByKey
        freeTypeVarsType = cata alg
          where
            alg ty = case ty of
                TVarF v -> Set.singleton v
                TArrowF a b -> Set.union a b
                TBaseF _ -> Set.empty
                TBottomF -> Set.empty
                TForallF v mb body ->
                    let boundFv = maybe Set.empty id mb
                        bodyFv = Set.delete v body
                    in Set.union boundFv bodyFv
    let unboundedBinderNames =
            [ name
            | (name, nidInt) <- zip names ordered0
            , case VarStore.lookupVarBound constraint (canonical (NodeId nidInt)) of
                Nothing -> True
                Just _ -> False
            ]
        uniqueUnboundedName =
            case unboundedBinderNames of
                [nm] -> Just nm
                _ -> Nothing
    let binderSet = IntSet.fromList ordered0
        bindingFor (name, nidInt) = do
            let bNodeC = canonical (NodeId nidInt)
                binderIsNamed = IntSet.member (getNodeId bNodeC) namedUnderGaSet
                binderKey = getNodeId bNodeC
                substForBound' = substForBound binderKey
                substNameSetForBound = Set.fromList (IntMap.elems substForBound')
                mbBoundNode = VarStore.lookupVarBound constraint bNodeC
                boundIsLocalSchemeRoot =
                    case (scopeGen, mbBoundNode) of
                        (Just gid, Just bnd) ->
                            case IntMap.lookup (getNodeId (canonical bnd)) schemeRootOwner of
                                Just gid' -> gid' == gid
                                Nothing -> False
                        _ -> False
                boundIsLocalSchemeBody =
                    case (scopeGen, mbBoundNode) of
                        (Just gid, Just bnd) ->
                            case IntMap.lookup (getNodeId (canonical bnd)) schemeRootByBody of
                                Just root ->
                                    case IntMap.lookup (getNodeId (canonical root)) schemeRootOwner of
                                        Just gid' -> gid' == gid
                                        Nothing -> False
                                Nothing -> False
                        _ -> False
                boundParentIsBinder =
                    case mbBoundNode of
                        Just bnd ->
                            case IntMap.lookup (nodeRefKey (typeRef (canonical bnd))) bindParents of
                                Just (TypeRef parent, _) -> canonical parent == bNodeC
                                _ -> False
                        Nothing -> False
                boundRoot =
                    case mbBoundNode of
                        Just bnd ->
                            case IntMap.lookup (getNodeId (canonical bnd)) schemeRootByBody of
                                Just _root
                                    | boundIsLocalSchemeBody -> canonical bnd
                                Just root
                                    | canonical root == bNodeC -> canonical bnd
                                Just root -> root
                                Nothing -> canonical bnd
                        Nothing -> bNodeC
            traceGeneralizeM env
                ("generalizeAt: boundRoot binder="
                    ++ show bNodeC
                    ++ " boundRoot="
                    ++ show boundRoot
                    ++ " boundParentIsBinder="
                    ++ show boundParentIsBinder
                    ++ " boundIsLocalSchemeBody="
                    ++ show boundIsLocalSchemeBody
                )
            boundSchemeBinderKeys <- case IntMap.lookup (getNodeId (canonical boundRoot)) schemeRootOwner of
                Just gid
                    | Just gid /= scopeGen -> do
                        nestedBinders <- bindingToElab (Binding.boundFlexChildrenUnder canonical constraint (genRef gid))
                        pure (IntSet.fromList (map (getNodeId . canonical) nestedBinders))
                _ -> pure IntSet.empty
            let substForBoundFiltered =
                    if IntSet.null boundSchemeBinderKeys
                        then substForBound'
                        else IntMap.filterWithKey (\k _ -> not (IntSet.member k boundSchemeBinderKeys)) substForBound'
            let mbBaseRoot =
                    if boundIsLocalSchemeBody || boundParentIsBinder
                        then Nothing
                        else
                            case mbBindParentsGa of
                                Just ga ->
                                    let baseConstraint = gaBaseConstraint ga
                                        baseBoundForBinder =
                                            case IntMap.lookup binderKey solvedToBasePref of
                                                Just baseBinder ->
                                                    case VarStore.lookupVarBound baseConstraint baseBinder of
                                                        Just baseBnd ->
                                                            case IntMap.lookup (getNodeId baseBnd) schemeRootByBodyBase of
                                                                Just baseSchemeRoot ->
                                                                    if baseSchemeRoot == baseBinder
                                                                        then Just baseBnd
                                                                        else Just baseSchemeRoot
                                                                Nothing -> Just baseBnd
                                                        Nothing -> Nothing
                                                Nothing -> Nothing
                                        fallbackFromBoundRoot =
                                            case IntMap.lookup (getNodeId (canonical boundRoot)) solvedToBasePref of
                                                Just baseRoot ->
                                                    case IntMap.lookup (getNodeId baseRoot) schemeRootByBodyBase of
                                                        Just baseSchemeRoot ->
                                                            if baseSchemeRoot == baseRoot
                                                                then Just baseRoot
                                                                else Just baseSchemeRoot
                                                        Nothing -> Just baseRoot
                                                Nothing -> IntMap.lookup binderKey solvedToBasePref
                                    in case baseBoundForBinder of
                                        Just _ -> baseBoundForBinder
                                        Nothing -> fallbackFromBoundRoot
                                Nothing -> Nothing
            boundTy0 <-
                case (mbBindParentsGa, mbBaseRoot) of
                    (Just ga, Just baseRoot) ->
                        reifyBoundWithNamesOnConstraint (gaBaseConstraint ga) (substForBoundBase binderKey) baseRoot
                    _ -> reifyBoundWithNames resForReify substForBoundFiltered boundRoot
            let fallbackAliasFor nm =
                    case (uniqueUnboundedName, nameToId nm) of
                        (Just fallbackName, Just k)
                            | boundIsLocalSchemeBody
                                && not (Set.member nm substNameSetForBound) ->
                                let nid = canonical (NodeId k)
                                in case bindingScopeGen nid of
                                    Just gid | Just gid /= scopeGen -> Just fallbackName
                                    Nothing | isNothing scopeGen -> Just fallbackName
                                    _ -> Nothing
                        _ -> Nothing
                aliasNameFor nm =
                    case nameToId nm of
                        Just k ->
                            let keyC = getNodeId (canonical (NodeId k))
                                repKey = IntMap.findWithDefault keyC keyC gammaAlias
                            in case IntMap.lookup repKey substForBound' of
                                Just nm' -> Just nm'
                                Nothing -> fallbackAliasFor nm
                        Nothing -> fallbackAliasFor nm
                substAliasTy boundSet ty = case ty of
                    TVar v ->
                        if Set.member v boundSet
                            then TVar v
                        else case aliasNameFor v of
                                Just v' -> TVar v'
                                Nothing -> TVar v
                    TArrow a b -> TArrow (substAliasTy boundSet a) (substAliasTy boundSet b)
                    TBase _ -> ty
                    TBottom -> ty
                    TForall v mb body ->
                        let mb' = fmap (substAliasTy boundSet) mb
                            body' = substAliasTy (Set.insert v boundSet) body
                        in TForall v mb' body'
                boundTy0' =
                    case (boundTy0, mbBoundNode) of
                        (TBottom, Just _)
                            | binderIsNamed -> TBottom
                        (TBottom, Just bnd) ->
                            let bndC = canonical bnd
                                bndKey = getNodeId bndC
                                nameForBound =
                                    case IntMap.lookup bndKey substForBound' of
                                        Just nm -> nm
                                        Nothing -> "t" ++ show bndKey
                            in case (IntMap.lookup bndKey nodes, VarStore.lookupVarBound constraint bndC) of
                                (Just TyVar{}, Nothing) -> TVar nameForBound
                                _ -> boundTy0
                        _ -> boundTy0
                boundTy0'' =
                    if boundMentionsSelfAlias bNodeC
                        then TBottom
                        else boundTy0'
                boundTy0Aliased = substAliasTy Set.empty boundTy0''
                extraBoundNames =
                    let isAliasBound nm =
                            case nameToId nm of
                                Just k ->
                                    let keyC = getNodeId (canonical (NodeId k))
                                        repKey = IntMap.findWithDefault keyC keyC gammaAlias
                                    in IntMap.member repKey substForBound'
                                Nothing -> False
                        freeNames = Set.toList (freeTypeVarsType boundTy0Aliased)
                    in [ nm
                       | nm <- freeNames
                       , not (Set.member nm substNameSetForBound)
                       , not (isAliasBound nm)
                       ]
                boundTy =
                    foldr
                        (\v acc -> TForall v Nothing acc)
                        boundTy0Aliased
                        (sort extraBoundNames)
            traceGeneralizeM env
                ( "generalizeAt: boundExtras binder="
                    ++ show bNodeC
                    ++ " extras="
                    ++ show extraBoundNames
                    ++ " extraInfo="
                    ++ show
                        [ ( nm
                          , do nid <- nameToId nm
                               let keyC = getNodeId (canonical (NodeId nid))
                               let baseM = IntMap.lookup keyC solvedToBasePref
                               let aliasM = IntMap.lookup keyC gammaAlias
                               pure (keyC, baseM, aliasM, firstGenAncestor bindParents (typeRef (NodeId keyC)))
                          )
                        | nm <- extraBoundNames
                        ]
                )
            traceGeneralizeM env
                ("generalizeAt: boundSelfAlias binder="
                    ++ show bNodeC
                    ++ " mentionsSelf="
                    ++ show (boundMentionsSelfAlias bNodeC)
                    ++ " boundTy0="
                    ++ show boundTy0
                    ++ " boundTy="
                    ++ show boundTy
                )
            let boundIsFreeVar =
                    case boundTy of
                        TVar _ ->
                            case mbBoundNode of
                                Just bnd ->
                                    let bndC = canonical bnd
                                        bndHasExplicitBound = VarStore.lookupVarBound constraint bndC
                                    in case IntMap.lookup (getNodeId bndC) nodes of
                                        Just TyVar{} ->
                                            isNothing bndHasExplicitBound
                                                && not (IntMap.member (getNodeId bndC) subst)
                                        _ -> False
                                _ -> False
                        _ -> False
                boundIsFreeVar' =
                    boundIsFreeVar && not binderIsNamed
                boundIsSelfVar =
                    case boundTy of
                        TVar v -> isNothing mbBoundNode && v == name
                        _ -> False
                boundIsSchemeRootNode =
                    case mbBoundNode of
                        Just bnd -> IntSet.member (getNodeId (canonical bnd)) schemeRootKeySet
                        Nothing -> False
                boundAllowed =
                    if binderIsNamed
                        then True
                    else if boundIsLocalSchemeRoot || boundIsLocalSchemeBody
                        then isTargetSchemeBinder bNodeC
                        else
                            hasExplicitBound bNodeC
                                || boundParentIsBinder
                                || boundIsSchemeRootNode
                                || case mbBoundNode of
                                    Just bnd -> IntSet.member (getNodeId (canonical bnd)) binderSet
                                    Nothing -> False
                                || containsForall boundTy
                mbBound =
                    if IntSet.member (getNodeId bNodeC) aliasBinderBases
                        then
                            if boundTy == TBottom
                                then Nothing
                                else Just boundTy
                        else if boundTy == TBottom || boundIsFreeVar' || boundIsSelfVar || not boundAllowed
                            then Nothing
                            else Just boundTy
            pure (name, mbBound)
    -- Phase 8: construct per-binder bounds.
    bindings <- mapM bindingFor (zip names ordered0)

    -- Phase 9: scheme ownership and type reification.
    let typeRootC = canonical typeRoot
        schemeOwnerFromBodySolved =
            case IntMap.lookup (getNodeId typeRootC) schemeRootByBody of
                Just root ->
                    IntMap.lookup (getNodeId (canonical root)) schemeRootOwner
                Nothing -> Nothing
        schemeOwnerFromBodyBase =
            case mbBindParentsGa of
                Just _ ->
                    case IntMap.lookup (getNodeId typeRootC) solvedToBasePref of
                        Just baseN ->
                            case IntMap.lookup (getNodeId baseN) schemeRootByBodyBase of
                                Just baseRoot ->
                                    IntMap.lookup (getNodeId baseRoot) schemeRootOwnerBase
                                Nothing -> Nothing
                        Nothing -> Nothing
                Nothing -> Nothing
        schemeOwnerFromBody =
            case schemeOwnerFromBodyBase of
                Just _ -> schemeOwnerFromBodyBase
                Nothing -> schemeOwnerFromBodySolved
        schemeOwnerFromBodyIsAlias =
            IntMap.member (getNodeId typeRootC) schemeRootByBody
        ownersByRoot =
            [ gnId gen
            | gen <- IntMap.elems (cGenNodes constraint)
            , any (\root -> canonical root == typeRootC) (gnSchemes gen)
            ]
        schemeOwners =
            maybe ownersByRoot (\gid -> gid : ownersByRoot) schemeOwnerFromBody
    typeInScope <-
        case scopeRootC of
            GenRef gid ->
                pure (bindingScopeFor (typeRef typeRootC) == Just gid)
            _ -> pure False
    let typeInScopeAdjusted =
            case (scopeGen, schemeOwnerFromBody) of
                (Just gid, Just owner)
                    | owner /= gid ->
                        False
                _ -> typeInScope
    let useSchemeType =
            case (scopeRootC, scopeGen, schemeOwnerFromBody) of
                (GenRef _, Just gid, Just owner)
                    | owner /= gid -> True
                (GenRef _, Just gid, _) ->
                    not typeInScopeAdjusted &&
                    not (null schemeOwners) &&
                    not (gid `elem` schemeOwners)
                _ -> False
    let typeRootIsTargetBound =
            case targetBound of
                Just bnd -> canonical bnd == canonical typeRoot
                Nothing -> False
    let useSchemeTypeAdjusted =
            case (schemeOwnerFromBody, scopeGen) of
                (Just owner, Just gid)
                    | owner /= gid && not typeRootIsTargetBound -> False
                _ -> useSchemeType
    let (typeRootForReify, substForReify) =
            case mbBindParentsGa of
                Just _ ->
                    case IntMap.lookup (getNodeId (canonical typeRoot)) solvedToBasePref of
                        Just baseN
                            | canonical baseN /= canonical typeRoot ->
                                (baseN, substBaseByKey)
                        _ -> (typeRoot, subst)
                Nothing -> (typeRoot, subst)
    let reifySchemeType =
            if useSchemeTypeAdjusted
                then do
                    let reachableVars =
                            [ NodeId nid
                            | nid <- IntSet.toList reachableType
                            , nid /= getNodeId typeRootC
                            , case IntMap.lookup nid nodes of
                                Just TyVar{} -> True
                                _ -> False
                            ]
                        hasReachableBinder gid =
                            any
                                (\nid -> firstGenAncestorGa (typeRef nid) == Just gid)
                                reachableVars
                        schemeOwnerCandidates = filter hasReachableBinder schemeOwners
                        schemeScope =
                            case schemeOwnerFromBody of
                                Just gid
                                    | schemeOwnerFromBodyIsAlias
                                    , (owner:_) <- ownersByRoot ->
                                        genRef owner
                                    | otherwise -> genRef gid
                                Nothing ->
                                    case schemeOwnerCandidates of
                                        (gid:_) -> genRef gid
                                        [] ->
                                            case schemeOwners of
                                                (gid:_) -> genRef gid
                                                [] -> typeRef typeRootC
                    if schemeScope == scopeRootC
                        then do
                            traceGeneralizeM env
                                ("generalizeAt: schemeScope equals scopeRootC; skipping recursive scheme-type fallback"
                                    ++ " scopeRootC=" ++ show scopeRootC
                                    ++ " typeRootC=" ++ show typeRootC
                                )
                            reifyTypeWithNamesNoFallback resForReify substForReify typeRootForReify
                        else do
                            (sch, _substScheme) <-
                                generalizeAtWith False True mbBindParentsGa res schemeScope typeRootC
                            pure $
                                case sch of
                                    Forall binds body ->
                                        foldr (\(n, b) t -> TForall n b t) body binds
                else
                    case mbBindParentsGa of
                        Just ga ->
                            case IntMap.lookup (getNodeId (canonical typeRoot)) solvedToBasePref of
                                Just baseN -> do
                                    tyBase <-
                                        reifyTypeWithNamesNoFallbackOnConstraint
                                            (gaBaseConstraint ga)
                                            substBaseByKey
                                            baseN
                                    let freeBase = freeNamesFrom Set.empty tyBase
                                        allowedBase = Set.fromList (IntMap.elems substBaseByKey)
                                    if Set.isSubsetOf freeBase allowedBase
                                        then pure tyBase
                                        else reifyTypeWithNamesNoFallback resForReify substForReify typeRootForReify
                                Nothing ->
                                    reifyTypeWithNamesNoFallback resForReify substForReify typeRootForReify
                        Nothing ->
                            reifyTypeWithNamesNoFallback resForReify substForReify typeRootForReify
    ty0Raw <- reifySchemeType
    -- Phase 10: normalize, rename, and prune bindings + type.
    let normalizeScheme tyRaw binds =
            let tyAdjusted =
                    case (binds, tyRaw) of
                        ((v, mb):_, TForall v' mb' body)
                            | v == v' && mb == mb' -> body
                        _ -> tyRaw
            in traceGeneralize env
                ("generalizeAt: ty0Raw=" ++ show tyAdjusted
                    ++ " subst=" ++ show subst
                    ++ " bindings=" ++ show binds
                )
                (tyAdjusted, binds)
        (ty0RawAdjusted, bindingsAdjusted) = normalizeScheme ty0Raw bindings
        nameForId k = "t" ++ show k
        substNames =
            [ (nameForId k, name)
            | (k, name) <- IntMap.toList subst
            ]
        namedBinderNames =
            Set.union
                (Set.fromList
                    [ name
                    | (nidInt, name) <- IntMap.toList subst
                    , IntSet.member nidInt namedUnderGaSet
                    ])
                (Set.fromList [ name | (name, Just _) <- bindingsAdjusted ])
        renameVars = cata alg
          where
            renameFromSubst v = case lookup v substNames of
                Just v' -> v'
                Nothing -> v
            alg ty = case ty of
                TVarF v -> TVar (renameFromSubst v)
                TArrowF a b -> TArrow a b
                TBaseF b -> TBase b
                TBottomF -> TBottom
                TForallF v mb body ->
                    let v' = renameFromSubst v
                    in TForall v' mb body
        ty0 = renameVars ty0RawAdjusted
        inlineBaseBounds = False
        (bindingsNorm0, tyNorm0) =
            simplifySchemeBindings inlineBaseBounds namedBinderNames bindingsAdjusted ty0
        (bindingsNorm, tyNorm) = promoteArrowAlias bindingsNorm0 tyNorm0
        usedNames =
            Set.unions
                ( freeNamesOf tyNorm
                    : [freeNamesOf b | (_, Just b) <- bindingsNorm]
                )
        bindingsFinal =
            filter
                (\(name, _) ->
                    Set.member name usedNames || Set.member name namedBinderNames
                )
                bindingsNorm
        bindingsFinal' =
            let dropRedundant (name, mb) =
                    not (Set.member name usedNames) &&
                    case mb of
                        Nothing -> True
                        Just bnd ->
                            let freeBound = freeNamesFrom Set.empty bnd
                                boundMentionsSelf = Set.member name freeBound
                                boundIsSimple = isVarBound bnd || isBaseBound bnd
                                boundIsBody = bnd == tyNorm
                            in not boundMentionsSelf && (boundIsSimple || boundIsBody)
            in filter (not . dropRedundant) bindingsFinal
        renameTypeVars = cata alg
          where
            renameFromMap v = Map.findWithDefault v v renameMap
            alg ty = case ty of
                TVarF v -> TVar (renameFromMap v)
                TArrowF a b -> TArrow a b
                TBaseF b -> TBase b
                TBottomF -> TBottom
                TForallF v mb body ->
                    let v' = renameFromMap v
                    in TForall v' mb body
        renameMap =
            Map.fromList
                [ (old, alphaName idx 0)
                | (idx, (old, _)) <- zip [0..] bindingsFinal'
                ]
        renameName name = Map.findWithDefault name name renameMap
        bindingsRenamed =
            [ (renameName name, fmap renameTypeVars mb)
            | (name, mb) <- bindingsFinal'
            ]
        tyRenamed = renameTypeVars tyNorm
        _ =
            traceGeneralize env
                ("generalizeAt: tyNorm=" ++ show tyNorm
                    ++ " usedNames=" ++ show (Set.toList usedNames)
                    ++ " bindingsNorm=" ++ show bindingsNorm
                    ++ " bindingsFinal=" ++ show bindingsFinal'
                    ++ " bindingsRenamed=" ++ show bindingsRenamed
                )
                ()
        usedNamesRenamed =
            Set.unions
                ( freeNamesOf tyRenamed
                    : [freeNamesOf b | (_, Just b) <- bindingsRenamed]
                )
        boundNames = Set.fromList (map fst bindingsRenamed)
        allowedNames = Set.fromList (map fst bindingsRenamed)
        missingNamesRaw = Set.toList (Set.difference usedNamesRenamed boundNames)
        aliasAllowed name =
            case parseNameId name of
                Just nid ->
                    let keyC = getNodeId (canonical (NodeId nid))
                        aliasKey = case IntMap.lookup keyC gammaAlias of
                            Just repKey -> repKey
                            Nothing -> keyC
                    in case IntMap.lookup aliasKey subst of
                        Just nm -> Set.member (renameName nm) boundNames
                        Nothing -> False
                Nothing -> False
        missingNamesRaw' = filter (not . aliasAllowed) missingNamesRaw
        missingNames =
            case scopeGen of
                Nothing -> missingNamesRaw'
                Just gid ->
                    let underScope name =
                            case parseNameId name of
                                Just nid ->
                                    let nidRef = NodeId nid
                                        underSolved =
                                            firstGenAncestorGa (typeRef nidRef) == Just gid
                                        underBase =
                                            case mbBindParentsGa of
                                                Just ga ->
                                                    case IntMap.lookup nid solvedToBasePref of
                                                        Just baseN ->
                                                            firstGenAncestor (gaBindParentsBase ga) (TypeRef baseN) == Just gid
                                                        Nothing -> underSolved
                                                Nothing -> underSolved
                                    in underBase
                                Nothing -> True
                    in filter underScope missingNamesRaw'
        keepNames = map fst bindingsRenamed
        subst' = IntMap.filter (`elem` keepNames) (IntMap.map renameName subst)
        finalizeScheme missing =
            if null missing
                then pure (Forall bindingsRenamed tyRenamed, subst')
                else
                    traceGeneralize env
                        ("generalizeAt: SchemeFreeVars typeRoot="
                            ++ show typeRootC
                            ++ " scopeRoot="
                            ++ show scopeRootC
                            ++ " scopeGen="
                            ++ show scopeGen
                            ++ " missing="
                            ++ show missing
                            ++ " bindingsFinal="
                            ++ show bindingsFinal
                            ++ " usedNames="
                            ++ show (Set.toList usedNames)
                            ++ " boundNames="
                            ++ show (Set.toList boundNames)
                            ++ " allowedNames="
                            ++ show (Set.toList (allowedNames :: Set.Set String))
                            ++ " bindParentsMissing="
                            ++ show
                                [ (name, IntMap.lookup (nodeRefKey (typeRef (NodeId nid))) bindParents)
                                | name <- missing
                                , Just nid <- [parseNameId name]
                                ]
                            ++ " missingBasePaths="
                            ++ show
                                [ ( name
                                  , NodeId nid
                                  , mbBase
                                  , mbBasePref
                                  , case mbBase of
                                        Nothing -> []
                                        Just baseN ->
                                            case bindingPathToRootLocal (gaBindParentsBase ga) (TypeRef baseN) of
                                                Right path -> path
                                                Left _ -> []
                                  , case mbBasePref of
                                        Nothing -> []
                                        Just baseN ->
                                            case bindingPathToRootLocal (gaBindParentsBase ga) (TypeRef baseN) of
                                                Right path -> path
                                                Left _ -> []
                                  , case mbBasePref of
                                        Nothing -> Nothing
                                        Just baseN -> firstGenAncestor (gaBindParentsBase ga) (TypeRef baseN)
                                  , case mbBase of
                                        Nothing -> Nothing
                                        Just baseN ->
                                            IntMap.lookup (nodeRefKey (typeRef baseN)) (gaBindParentsBase ga)
                                  )
                                | name <- missing
                                , Just nid <- [parseNameId name]
                                , Just ga <- [mbBindParentsGa]
                                , let mbBase = IntMap.lookup nid (gaSolvedToBase ga)
                                , let mbBasePref = IntMap.lookup nid solvedToBasePref
                                ]
                        )
                        (Left $ SchemeFreeVars typeRootC missing)
    -- Phase 11: final validation (SchemeFreeVars).
    finalizeScheme missingNames
  where
    resolveContext
        :: GeneralizeEnv
        -> BindParents
        -> NodeRef
        -> NodeId
        -> Either ElabError GeneralizeCtx
    resolveContext env bindParentsSoft scopeRootArg targetNodeArg = do
        let constraint = geConstraint env
            nodes = geNodes env
            canonical = geCanonical env
            canonKey = geCanonKey env
            lookupNode = geLookupNode env
            mbBindParentsGa' = geBindParentsGa env
            scopeRoot0 = case scopeRootArg of
                TypeRef nid -> TypeRef (canonical nid)
                GenRef gid -> GenRef gid
        -- Phase 1: canonicalize roots/targets and choose order roots.
        let resolveTarget node =
                case lookupNode (canonKey node) of
                    Just TyExp{ tnBody = b } -> canonical b
                    _ -> canonical node
            resolveTargetBase target =
                case mbBindParentsGa' of
                    Nothing -> target
                    Just ga ->
                        IntMap.findWithDefault target (getNodeId target) (gaSolvedToBase ga)
            resolveScopeRoot root =
                case (root, mbBindParentsGa') of
                    (TypeRef nid, Just ga) ->
                        case IntMap.lookup (getNodeId nid) (gaSolvedToBase ga) of
                            Nothing -> root
                            Just baseN ->
                                case bindingPathToRootLocal (gaBindParentsBase ga) (typeRef baseN) of
                                    Left _ -> root
                                    Right path ->
                                        case listToMaybe [gid | GenRef gid <- drop 1 path] of
                                            Just gid -> GenRef gid
                                            Nothing -> root
                    _ -> root
            resolveOrderRoots root target =
                case root of
                    GenRef _ ->
                        case lookupNode (canonKey target) of
                            Just TyForall{ tnBody = b } ->
                                let bodyRoot = canonical b
                                in (bodyRoot, target)
                            _ -> (target, target)
                    TypeRef _ ->
                        case lookupNode (canonKey target) of
                            Just TyForall{ tnBody = b } ->
                                let bodyRoot = canonical b
                                in (bodyRoot, target)
                            _ -> (target, target)
            resolveOrderRootBase root target =
                case mbBindParentsGa' of
                    Nothing -> root
                    Just ga ->
                        let baseNodes = cNodes (gaBaseConstraint ga)
                        in case IntMap.lookup (getNodeId target) baseNodes of
                            Just TyForall{ tnBody = b } -> b
                            _ -> target
            resolveTargetPhase root node =
                let target0 = resolveTarget node
                    targetBase = resolveTargetBase target0
                    scopeRootC = resolveScopeRoot root
                    (orderRoot, typeRoot0) = resolveOrderRoots scopeRootC target0
                    orderRootBase = resolveOrderRootBase orderRoot targetBase
                in ResolveTarget
                    { rtTarget0 = target0
                    , rtTargetBase = targetBase
                    , rtScopeRootC = scopeRootC
                    , rtOrderRoot = orderRoot
                    , rtTypeRoot0 = typeRoot0
                    , rtOrderRootBase = orderRootBase
                    }
        let targetPhase = resolveTargetPhase scopeRoot0 targetNodeArg
            ResolveTarget
                { rtTarget0 = target0
                , rtTargetBase = targetBase
                , rtScopeRootC = scopeRootC
                , rtOrderRoot = orderRoot
                , rtTypeRoot0 = typeRoot0
                , rtOrderRootBase = orderRootBase
                } = targetPhase
        traceGeneralizeM env
            ("generalizeAt: scopeRootC=" ++ show scopeRootC
                ++ " target0=" ++ show target0
                ++ " orderRoot=" ++ show orderRoot
                ++ " typeRoot0=" ++ show typeRoot0
                ++ case mbBindParentsGa' of
                    Nothing -> ""
                    Just _ -> " orderRootBase=" ++ show orderRootBase
            )
        -- Phase 2: discover the scope's owning gen node (if any).
        let resolveScopeGen root =
                case root of
                    GenRef gid -> pure (Just gid)
                    TypeRef nid ->
                        case mbBindParentsGa' of
                            Just ga ->
                                case IntMap.lookup (getNodeId nid) (gaSolvedToBase ga) of
                                    Just baseNid ->
                                        pure (firstGenAncestor (gaBindParentsBase ga) (TypeRef baseNid))
                                    Nothing -> do
                                        path <- bindingToElab (Binding.bindingPathToRoot constraint (TypeRef nid))
                                        pure (listToMaybe [gid | GenRef gid <- drop 1 path])
                            Nothing -> do
                                path <- bindingToElab (Binding.bindingPathToRoot constraint (TypeRef nid))
                                pure (listToMaybe [gid | GenRef gid <- drop 1 path])
            resolveScopePhase root = do
                scopeGen <- resolveScopeGen root
                pure ResolveScope { rsScopeGen = scopeGen }
        scopePhase <- resolveScopePhase scopeRootC
        let ResolveScope { rsScopeGen = scopeGen } = scopePhase
        -- Phase 3: merge binding parents (base + solved) into a single view.
        let resolveBindParents scopeGen' =
                case (mbBindParentsGa', scopeGen') of
                    (Just ga, Just gidScope) ->
                        let baseParents = gaBindParentsBase ga
                            baseToSolved = gaBaseToSolved ga
                            solvedToBase = gaSolvedToBase ga
                            findSolvedKey baseKey =
                                case IntMap.lookup baseKey baseToSolved of
                                    Just solvedNid -> Just (getNodeId (canonical solvedNid))
                                    Nothing ->
                                        let candidates =
                                                [ solvedKey
                                                | (solvedKey, baseNid) <- IntMap.toList solvedToBase
                                                , getNodeId baseNid == baseKey
                                                ]
                                        in case candidates of
                                            (k:_) -> Just k
                                            _ ->
                                                if IntMap.member baseKey nodes
                                                    then Just baseKey
                                                    else Nothing
                            mapBaseRefToSolved parentRef =
                                case parentRef of
                                    GenRef gid -> Just (GenRef gid)
                                    TypeRef parentN ->
                                        case findSolvedKey (getNodeId parentN) of
                                            Just solvedKey -> Just (TypeRef (NodeId solvedKey))
                                            Nothing -> Nothing
                            bindParentsGaFix =
                                IntMap.foldlWithKey'
                                    (\acc childKey (parentRef, flag) ->
                                        case nodeRefFromKey childKey of
                                            TypeRef baseChild ->
                                                let baseAncestor = firstGenAncestor baseParents (TypeRef baseChild)
                                                in if baseAncestor /= Just gidScope
                                                    then acc
                                                    else
                                                        case findSolvedKey (getNodeId baseChild) of
                                                            Nothing -> acc
                                                            Just solvedChildKey ->
                                                                case mapBaseRefToSolved parentRef of
                                                                    Nothing -> acc
                                                                    Just parentRef' ->
                                                                        let childKey' = nodeRefKey (TypeRef (NodeId solvedChildKey))
                                                                            existing = IntMap.lookup childKey' bindParentsSoft
                                                                            selfParent =
                                                                                case existing of
                                                                                    Just (parentExisting, _) ->
                                                                                        nodeRefKey parentExisting == childKey'
                                                                                    Nothing -> False
                                                                            redirected = solvedChildKey /= getNodeId baseChild
                                                                            solvedAncestor =
                                                                                firstGenAncestor
                                                                                    bindParentsSoft
                                                                                    (TypeRef (NodeId solvedChildKey))
                                                                            shouldOverride =
                                                                                isNothing existing
                                                                                    || selfParent
                                                                                    || redirected
                                                                                    || solvedAncestor /= Just gidScope
                                                                        in if shouldOverride
                                                                            then
                                                                                IntMap.insertWith
                                                                                    (\(parentNew, flagNew) (_parentOld, flagOld) ->
                                                                                        (parentNew, max flagNew flagOld)
                                                                                    )
                                                                                    childKey'
                                                                                    (parentRef', flag)
                                                                                    acc
                                                                            else acc
                                            _ -> acc
                                    )
                                    IntMap.empty
                                    baseParents
                        in IntMap.union bindParentsGaFix bindParentsSoft
                    _ -> bindParentsSoft
            resolveFirstGenAncestor bindParents' =
                case mbBindParentsGa' of
                    Nothing -> firstGenAncestor bindParents'
                    Just ga ->
                        \ref ->
                            case ref of
                                GenRef gid -> Just gid
                                TypeRef nid ->
                                    let key = getNodeId (canonical nid)
                                        baseConstraint = gaBaseConstraint ga
                                    in case IntMap.lookup key (gaSolvedToBase ga) of
                                        Just baseNid ->
                                            firstGenAncestor (gaBindParentsBase ga) (TypeRef baseNid)
                                        Nothing ->
                                            if IntMap.member key (cNodes baseConstraint)
                                                then firstGenAncestor (gaBindParentsBase ga) (TypeRef (NodeId key))
                                                else Nothing
            resolveBindsPhase scopeGen' =
                let bindParents = resolveBindParents scopeGen'
                    firstGenAncestorGa = resolveFirstGenAncestor bindParents
                    constraintForReify = constraint { cBindParents = bindParents }
                    resForReify = (geRes env) { srConstraint = constraintForReify }
                in ResolveBinds
                    { rbBindParents = bindParents
                    , rbFirstGenAncestor = firstGenAncestorGa
                    , rbConstraintForReify = constraintForReify
                    , rbResForReify = resForReify
                    }
        let bindsPhase = resolveBindsPhase scopeGen
            ResolveBinds
                { rbBindParents = bindParents
                , rbFirstGenAncestor = firstGenAncestorGa
                , rbConstraintForReify = constraintForReify
                , rbResForReify = resForReify
                } = bindsPhase
        pure GeneralizeCtx
            { gcTarget0 = target0
            , gcTargetBase = targetBase
            , gcScopeRootC = scopeRootC
            , gcOrderRoot = orderRoot
            , gcTypeRoot0 = typeRoot0
            , gcOrderRootBase = orderRootBase
            , gcScopeGen = scopeGen
            , gcBindParents = bindParents
            , gcFirstGenAncestor = firstGenAncestorGa
            , gcConstraintForReify = constraintForReify
            , gcResForReify = resForReify
            }

    orderBinderCandidates
        :: (NodeId -> NodeId)
        -> Constraint
        -> NodeId
        -> NodeId
        -> [Int]
        -> (Int -> Either ElabError [Int])
        -> Either ElabError [Int]
    orderBinderCandidates canonical' constraint' root rootBase candidates depsForE =
        let keysSolved = Order.orderKeysFromConstraintWith canonical' constraint' root Nothing
        in case mbBindParentsGa of
            Nothing -> orderBinderCandidatesSolved keysSolved candidates depsForE
            Just ga -> orderBinderCandidatesBase ga keysSolved rootBase candidates depsForE
      where
        orderBinderCandidatesSolved keysSolved candidates' depsForE' = do
            let keys = keysSolved
                candidateSet = IntSet.fromList candidates'
                keyMaybe k = IntMap.lookup k keys
                cmpReady a b =
                    case (keyMaybe a, keyMaybe b) of
                        (Just ka, Just kb) ->
                            case Order.compareOrderKey ka kb of
                                EQ -> compare a b
                                other -> other
                        _ -> compare a b

            depsList <- mapM
                (\k -> do
                    deps <- depsForE' k
                    pure (k, filter (\d -> d /= k && IntSet.member d candidateSet) deps)
                )
                candidates'
            let depsMap = IntMap.fromList depsList
                depsFor k = IntMap.findWithDefault [] k depsMap

            topoSortBy
                "generalizeAt: cycle in binder bound dependencies"
                cmpReady
                depsFor
                candidates'

        orderBinderCandidatesBase ga keysSolved rootBase' candidates' depsForE' = do
            let baseConstraint = gaBaseConstraint ga
                keysBase = Order.orderKeysFromConstraintWith id baseConstraint rootBase' Nothing
                candidateSet = IntSet.fromList candidates'
                toBase k = IntMap.lookup k (gaSolvedToBase ga)
                keyBase k = toBase k >>= (\b -> IntMap.lookup (getNodeId b) keysBase)
                keySolved k = IntMap.lookup k keysSolved
                missingKeys =
                    [ k
                    | k <- candidates'
                    , Just baseN <- [toBase k]
                    , not (IntMap.member (getNodeId baseN) keysBase)
                    ]
                cmpReady a b =
                    case (keyBase a, keyBase b) of
                        (Just ka, Just kb) ->
                            case Order.compareOrderKey ka kb of
                                EQ -> compare a b
                                other -> other
                        _ ->
                            case (keySolved a, keySolved b) of
                                (Just sa, Just sb) ->
                                    case Order.compareOrderKey sa sb of
                                        EQ -> compare a b
                                        other -> other
                                _ -> compare a b

            traceGeneralizeEnabledM debugGeneralizeEnabled
                ("generalizeAt: missing base order keys (falling back to solved) "
                    ++ show (map NodeId missingKeys)
                )

            depsList <- mapM
                (\k -> do
                    deps <- depsForE' k
                    pure (k, filter (\d -> d /= k && IntSet.member d candidateSet) deps)
                )
                candidates'
            let depsMap = IntMap.fromList depsList
                depsFor k = IntMap.findWithDefault [] k depsMap

            topoSortBy
                "generalizeAt: cycle in binder bound dependencies"
                cmpReady
                depsFor
                candidates'

    firstGenAncestor bindParents' start =
        case bindingPathToRootLocal bindParents' start of
            Left _ -> Nothing
            Right path -> listToMaybe [gid | GenRef gid <- drop 1 path]

    bindingPathToRootLocal bindParents' start =
        walkBindingParents bindParents' IntSet.empty [start] (nodeRefKey start)

    walkBindingParents bindParents' visited path key
        | IntSet.member key visited = Left (BindingTreeError (BindingCycleDetected (reverse path)))
        | otherwise =
            case IntMap.lookup key bindParents' of
                Nothing -> Right (reverse path)
                Just (parentRef, _) ->
                    walkBindingParents
                        bindParents'
                        (IntSet.insert key visited)
                        (parentRef : path)
                        (nodeRefKey parentRef)


    freeNamesOf :: ElabType -> Set.Set String
    freeNamesOf = freeNamesFrom Set.empty

    freeNamesFrom :: Set.Set String -> ElabType -> Set.Set String
    freeNamesFrom bound0 ty = (cata alg ty) bound0
      where
        alg node = case node of
            TVarF v ->
                \boundSet ->
                    if Set.member v boundSet then Set.empty else Set.singleton v
            TArrowF d c -> \boundSet -> Set.union (d boundSet) (c boundSet)
            TBaseF _ -> const Set.empty
            TBottomF -> const Set.empty
            TForallF v mb t' ->
                \boundSet ->
                    let bound' = Set.insert v boundSet
                        freeBound = maybe Set.empty ($ bound') mb
                        freeBody = t' bound'
                    in Set.union freeBound freeBody

    substType :: String -> ElabType -> ElabType -> ElabType
    substType name replacement = para alg
      where
        alg ty = case ty of
            TVarF v
                | v == name -> replacement
                | otherwise -> TVar v
            TArrowF d c -> TArrow (snd d) (snd c)
            TBaseF b -> TBase b
            TBottomF -> TBottom
            TForallF v mb t'
                | v == name -> TForall v (fmap fst mb) (fst t')
                | otherwise -> TForall v (fmap snd mb) (snd t')

    simplifySchemeBindings
        :: Bool
        -> Set.Set String
        -> [(String, Maybe ElabType)]
        -> ElabType
        -> ([(String, Maybe ElabType)], ElabType)
    simplifySchemeBindings inlineBaseBounds namedBinders binds ty =
        let binders = Set.fromList (map fst binds)
        in simplify binders binds ty
      where
        simplify _ [] body = ([], body)
        simplify binders ((v, mbBound):rest) body =
            let isNamedBinder = Set.member v namedBinders
            in case mbBound of
                Nothing ->
                    let (rest', body') = simplify binders rest body
                    in ((v, Nothing) : rest', body')
                Just bound ->
                    case body of
                        TVar v' | v' == v ->
                            let freeBound = freeNamesFrom Set.empty bound
                                boundMentionsSelf = Set.member v freeBound
                                boundDeps = Set.delete v freeBound
                                boundIsBase = isBaseBound bound
                                boundMentionsNamed =
                                    not (Set.null (Set.intersection freeBound namedBinders))
                                boundIsAliasToBinder =
                                    case bound of
                                        TVar v2 ->
                                            v2 /= v
                                                && Set.member v2 binders
                                                && not isNamedBinder
                                        _ -> False
                                canInlineAlias =
                                    Set.null boundDeps
                                        && (not boundIsBase || inlineBaseBounds)
                                        && not isNamedBinder
                                        && not boundMentionsNamed
                                        || boundIsAliasToBinder
                            in if not boundMentionsSelf
                                && canInlineAlias
                                && not (containsForall bound)
                                && not (containsArrow bound)
                                then
                                    let body' = bound
                                        restSub =
                                            [ (name, fmap (substType v bound) mb)
                                            | (name, mb) <- rest
                                            ]
                                    in simplify (Set.delete v binders) restSub body'
                                else
                                    let (rest', body') = simplify binders rest body
                                    in ((v, Just bound) : rest', body')
                        _ ->
                            let freeBound = freeNamesFrom Set.empty bound
                                boundMentionsSelf = Set.member v freeBound
                                boundDeps = Set.delete v freeBound
                                dependsOnBinders =
                                    not (Set.null (Set.intersection boundDeps (Set.delete v binders)))
                                boundMentionsNamed =
                                    not (Set.null (Set.intersection freeBound namedBinders))
                                boundIsAliasToBinder =
                                    case bound of
                                        TVar v2 ->
                                            v2 /= v
                                                && Set.member v2 binders
                                                && not isNamedBinder
                                        _ -> False
                                restUsesV =
                                    Set.member v $
                                        Set.unions
                                            [ freeNamesFrom Set.empty b
                                            | (_, Just b) <- rest
                                            ]
                                canInlineBase =
                                    inlineBaseBounds
                                        && not dependsOnBinders
                                        && not restUsesV
                                        && isBaseBound bound
                                        && not boundMentionsNamed
                                canInlineNonBase =
                                    not dependsOnBinders
                                        && not (isBaseBound bound)
                                        && isVarBound bound
                                        && not isNamedBinder
                                        && not boundMentionsNamed
                                        || boundIsAliasToBinder
                            in if not boundMentionsSelf
                                && (canInlineBase || canInlineNonBase)
                                then
                                    let replacement = bound
                                        bodySub = substType v replacement body
                                        restSub =
                                            [ (name, fmap (substType v replacement) mb)
                                            | (name, mb) <- rest
                                            ]
                                    in simplify binders restSub bodySub
                                else
                                    let (rest', body') = simplify binders rest body
                                    in ((v, Just bound) : rest', body')

    promoteArrowAlias :: [(String, Maybe ElabType)] -> ElabType -> ([(String, Maybe ElabType)], ElabType)
    promoteArrowAlias binds ty = case ty of
        TArrow (TVar v1) (TVar v2)
            | v1 == v2 ->
                case lookup v1 binds of
                    Just (Just bnd)
                        | isBaseBound bnd || bnd == TBottom ->
                            let bnd' = TArrow bnd bnd
                                binds' = map (\(n, mb) -> if n == v1 then (n, Just bnd') else (n, mb)) binds
                            in (binds', TVar v1)
                    _ -> (binds, ty)
        _ -> (binds, ty)

    isBaseBound :: ElabType -> Bool
    isBaseBound ty = case ty of
        TBase{} -> True
        TBottom -> True
        _ -> False

    isVarBound :: ElabType -> Bool
    isVarBound ty = case ty of
        TVar{} -> True
        _ -> False

    containsForall :: ElabType -> Bool
    containsForall = cata alg
      where
        alg ty = case ty of
            TForallF _ _ _ -> True
            TArrowF d c -> d || c
            _ -> False

    containsArrow :: ElabType -> Bool
    containsArrow = cata alg
      where
        alg ty = case ty of
            TArrowF _ _ -> True
            TForallF _ mb body ->
                let boundHasArrow = maybe False id mb
                in boundHasArrow || body
            _ -> False

traceGeneralizeEnabled :: Bool -> String -> a -> a
traceGeneralizeEnabled enabled msg value =
    if enabled
        then trace msg value
        else value

traceGeneralize :: GeneralizeEnv -> String -> a -> a
traceGeneralize env = traceGeneralizeEnabled (geDebugEnabled env)

traceGeneralizeM :: GeneralizeEnv -> String -> Either ElabError ()
traceGeneralizeM env msg = traceGeneralize env msg (Right ())

traceGeneralizeEnabledM :: Bool -> String -> Either ElabError ()
traceGeneralizeEnabledM enabled msg = traceGeneralizeEnabled enabled msg (Right ())

debugGeneralizeEnabled :: Bool
debugGeneralizeEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_GENERALIZE"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugGeneralizeEnabled #-}


alphaName :: Int -> Int -> String
alphaName idx _ = letters !! (idx `mod` length letters) ++ suffix
  where
    letters = map (:[]) ['a'..'z']
    suffix = if idx < length letters then "" else show (idx `div` length letters)
