module MLF.Elab.Generalize (
    GaBindParents(..),
    generalizeAt,
    generalizeAtKeepTarget,
    generalizeAtAllowRigid,
    generalizeAtKeepTargetAllowRigid,
    generalizeAtAllowRigidWithBindParents,
    generalizeAtKeepTargetAllowRigidWithBindParents
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort, sortBy)
import Data.Maybe (listToMaybe)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Generalize.Util (boundRootWith, firstSchemeRootAncestorWith)
import MLF.Elab.Generalize.BinderPlan
    ( BinderPlan(..)
    , BinderPlanInput(..)
    , GaBindParentsInfo(..)
    , buildBinderPlan
    )
import MLF.Elab.Generalize.BinderHelpers
    ( boundMentionsSelfAliasFor
    , isTargetSchemeBinderFor
    )
import MLF.Elab.Generalize.BindingUtil
    ( bindingPathToRootLocal
    , bindingScopeFor
    , firstGenAncestorFrom
    )
import MLF.Elab.Generalize.Context
    ( GaBindParents(..)
    , GeneralizeEnv(..)
    , GeneralizeCtx(..)
    , resolveContext
    , traceGeneralize
    , traceGeneralizeM
    )
import MLF.Elab.Generalize.Helpers
    ( bindableChildrenUnder
    , boundContainsForall
    , bindingScopeGen
    , computeAliasBinders
    , hasExplicitBound
    , isQuantifiable
    , isScopeSchemeRoot
    , mkIsBindable
    , selectBinders
    )
import MLF.Elab.Generalize.Normalize
    ( simplifySchemeBindings
    , promoteArrowAlias
    , isBaseBound
    , isVarBound
    , containsForall
    )
import MLF.Elab.Generalize.Names (alphaName, parseNameId)
import MLF.Elab.Generalize.Ordering (orderBinderCandidates)
import MLF.Elab.Generalize.SchemeRoots
    ( SchemeRootInfo(..)
    , SchemeRootsPlan(..)
    , allowBoundTraversalFor
    , buildSchemeRootsPlan
    )
import MLF.Elab.Generalize.ReifyPlan
    ( ReifyPlan(..)
    , ReifyBindingEnv(..)
    , ReifyPlanInput(..)
    , buildReifyPlan
    , bindingFor
    )
import MLF.Elab.FreeNames (freeNamesFrom, freeNamesOf)
import MLF.Elab.Util (reachableFrom, reachableFromStop)
import MLF.Elab.Reify
    ( reifyTypeWithNamesNoFallback
    , reifyTypeWithNamesNoFallbackOnConstraint
    , reifyBoundWithNames
    )
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Binding.Tree as Binding

data TargetPlan = TargetPlan
    { tpTargetBound :: Maybe NodeId
    , tpTargetBoundUnderOtherGen :: Bool
    , tpBoundUnderOtherGen :: Bool
    , tpBoundIsSchemeRoot :: Bool
    , tpBoundIsVar :: Bool
    , tpBoundIsBase :: Bool
    , tpBoundIsStructural :: Bool
    , tpBoundIsChild :: Bool
    , tpBoundIsDirectChild :: Bool
    , tpBoundMentionsTarget :: Bool
    , tpBoundHasForall :: Bool
    , tpBoundHasNestedGen :: Bool
    , tpTargetRigid :: Bool
    , tpTargetIsSchemeRoot :: Bool
    , tpTargetIsSchemeRootForScope :: Bool
    , tpTargetIsTyVar :: Bool
    }

data DropPlan = DropPlan
    { dpDropTarget :: Bool
    , dpSchemeRoots :: IntSet.IntSet
    }

data TypeRootPlan = TypeRootPlan
    { trUseBoundTypeRoot :: Bool
    , trSchemeBodyRoot :: NodeId
    , trTargetInGamma :: Bool
    , trTargetIsBaseLike :: Bool
    , trSchemeBodyChildUnderGen :: Maybe NodeId
    , trTypeRoot0 :: NodeId
    , trTypeRoot :: NodeId
    }

data GammaPlan = GammaPlan
    { gpBaseGammaSet :: IntSet.IntSet
    , gpBaseGammaRep :: IntMap.IntMap Int
    , gpNamedUnderGaSet :: IntSet.IntSet
    , gpSolvedToBasePref :: IntMap.IntMap NodeId
    , gpGammaAlias :: IntMap.IntMap Int
    , gpBaseGammaRepSet :: IntSet.IntSet
    , gpReachableForBinders :: IntSet.IntSet
    , gpGammaKeyFor :: Int -> Int -> Int
    , gpNamedUnderGa :: [NodeId]
    , gpBoundHasNamedOutsideGamma :: Bool
    , gpTypeRootHasNamedOutsideGamma :: Bool
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
        mbBindParentsGaInfo =
            fmap
                (\ga -> GaBindParentsInfo
                    { gbiBindParentsBase = gaBindParentsBase ga
                    , gbiBaseConstraint = gaBaseConstraint ga
                    , gbiBaseToSolved = gaBaseToSolved ga
                    , gbiSolvedToBase = gaSolvedToBase ga
                    })
                mbBindParentsGa
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
    let schemeRootsPlan =
            buildSchemeRootsPlan
                canonical
                constraint
                nodes
                mbBindParentsGaInfo
                firstGenAncestorFrom
        SchemeRootsPlan
            { srInfo = schemeRootInfo
            , srSchemeRootOwnerBase = schemeRootOwnerBase
            , srSchemeRootByBodyBase = schemeRootByBodyBase
            , srLookupSchemeRootOwner = lookupSchemeRootOwner
            , srContainsForallFrom = containsForallFrom
            , srContainsForallForTarget = containsForallForTarget
            , srBoundHasForallForVar = boundHasForallForVar
            } = schemeRootsPlan
        SchemeRootInfo
            { sriRootKeySetRaw = schemeRootKeySetRaw
            , sriRootKeySet = schemeRootKeySet
            , sriRootOwner = schemeRootOwner
            , sriRootByBody = schemeRootByBody
            } = schemeRootInfo
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
        allowBoundTraversal =
            allowBoundTraversalFor schemeRootsPlan canonical scopeGen target0

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
        scopeHasStructuralScheme =
            case scopeRootC of
                GenRef gid ->
                    case IntMap.lookup (genNodeKey gid) (cGenNodes constraint) of
                        Just gen ->
                            any
                                (\root ->
                                    not (IntSet.member (canonKey root) scopeSchemeRoots)
                                )
                                (gnSchemes gen)
                        Nothing -> False
                _ -> False
        isQuantifiable' = isQuantifiable canonical constraint isTyVarKey
        bindFlags =
            IntMap.fromList
                [ (childKey, flag)
                | (childKey, (_parent, flag)) <- IntMap.toList bindParents
                ]
        boundContainsForall' =
            boundContainsForall canonical constraint (containsForallFrom (const False))
        isScopeSchemeRoot' = isScopeSchemeRoot canonKey scopeSchemeRoots
        isBindable =
            mkIsBindable
                allowRigidBinders
                bindFlags
                isQuantifiable'
                isScopeSchemeRoot'
                boundContainsForall'
    (aliasBinderBases, aliasBinderNodes) <-
        computeAliasBinders
            canonical
            canonKey
            constraint
            nodes
            bindParents
            scopeSchemeRoots
            scopeRootC
            (traceGeneralizeM env)
    let bindableChildrenUnder' =
            bindableChildrenUnder canonical bindParents isBindable
        bindingScopeGen' = bindingScopeGen constraint
        hasExplicitBound' = hasExplicitBound canonical nodes constraint
    binders0 <-
        selectBinders
            canonical
            bindParents
            nodes
            constraint
            isBindable
            canonKey
            scopeSchemeRoots
            hasExplicitBound'
            aliasBinderNodes
            (traceGeneralizeM env)
            scopeGen
            scopeRootC
            target0
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
        targetPlan =
            let targetBoundLocalFromBase =
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
                targetBoundLocalRaw =
                    case (mbBindParentsGa, targetBoundLocalFromBase) of
                        (Just _, Just bnd) -> Just bnd
                        _ ->
                            case IntMap.lookup (getNodeId target0) nodes of
                                Just TyVar{ tnBound = Just bnd } -> Just bnd
                                _ -> targetBoundLocalFromBase
                targetBoundLocal =
                    case targetBoundLocalRaw of
                        Just bnd | IntSet.member (getNodeId bnd) schemeRootKeySetRaw -> Just bnd
                        Just bnd -> Just (canonical bnd)
                        Nothing -> Nothing
                targetBindParentLocal = IntMap.lookup (nodeRefKey (typeRef target0)) bindParents
                targetBoundUnderOtherGenLocal =
                    case (scopeGen, targetBindParentLocal) of
                        (Just gidScope, Just (GenRef gidTarget, _)) -> gidTarget /= gidScope
                        _ -> False
                boundUnderOtherGenLocal =
                    case (scopeGen, targetBoundLocal) of
                        (Just gidScope, Just bnd) ->
                            case IntMap.lookup (nodeRefKey (typeRef bnd)) bindParents of
                                Just (GenRef gidBound, _) -> gidBound /= gidScope
                                _ -> False
                        _ -> False
                boundIsSchemeRootLocal =
                    case (scopeRootC, targetBoundLocal) of
                        (GenRef gid, Just bnd) ->
                            case IntMap.lookup (getGenNodeId gid) (cGenNodes constraint) of
                                Nothing -> False
                                Just gen ->
                                    IntSet.member
                                        (getNodeId (canonical bnd))
                                        (IntSet.fromList (map (getNodeId . canonical) (gnSchemes gen)))
                        _ -> False
                boundIsVarLocal =
                    case targetBoundLocal >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                        Just TyVar{} -> True
                        _ -> False
                boundIsBaseLocal =
                    case targetBoundLocal >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                        Just TyBase{} -> True
                        _ -> False
                boundIsStructuralLocal =
                    case targetBoundLocal >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                        Just TyArrow{} -> True
                        Just TyForall{} -> True
                        Just TyExp{} -> True
                        _ -> False
                boundIsChildLocal =
                    case targetBoundLocal of
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
                boundIsDirectChildLocal =
                    case targetBoundLocal of
                        Just bnd ->
                            let bndC = canonical bnd
                            in case IntMap.lookup (nodeRefKey (typeRef bndC)) bindParents of
                                Just (parentRef, _) -> parentRef == typeRef target0
                                Nothing -> False
                        Nothing -> False
                boundMentionsTargetLocal =
                    case targetBoundLocal of
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
                boundHasForallLocal =
                    let boundHasForallLocalBase =
                            case (mbBindParentsGa, targetBoundLocal) of
                                (Just ga, Just bnd) ->
                                    case IntMap.lookup (getNodeId (canonical bnd)) (gaSolvedToBase ga) of
                                        Just baseBnd ->
                                            IntMap.member (getNodeId baseBnd) schemeRootOwnerBase
                                                || IntMap.member (getNodeId baseBnd) schemeRootByBodyBase
                                        Nothing -> False
                                _ -> False
                    in case targetBoundLocal of
                        Nothing -> False
                        Just bnd -> containsForallForTarget bnd || boundHasForallLocalBase
                boundHasNestedGenLocal =
                    case (targetBoundLocal, scopeGen) of
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
                targetRigidLocal =
                    case IntMap.lookup (nodeRefKey (typeRef target0)) bindFlags of
                        Just BindRigid -> True
                        _ -> False
                targetIsSchemeRootLocal =
                    IntSet.member (canonKey target0) schemeRootKeySet
                targetIsSchemeRootForScopeLocal =
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
                targetIsTyVarLocal = isTyVarKey (canonKey target0)
            in TargetPlan
                { tpTargetBound = targetBoundLocal
                , tpTargetBoundUnderOtherGen = targetBoundUnderOtherGenLocal
                , tpBoundUnderOtherGen = boundUnderOtherGenLocal
                , tpBoundIsSchemeRoot = boundIsSchemeRootLocal
                , tpBoundIsVar = boundIsVarLocal
                , tpBoundIsBase = boundIsBaseLocal
                , tpBoundIsStructural = boundIsStructuralLocal
                , tpBoundIsChild = boundIsChildLocal
                , tpBoundIsDirectChild = boundIsDirectChildLocal
                , tpBoundMentionsTarget = boundMentionsTargetLocal
                , tpBoundHasForall = boundHasForallLocal
                , tpBoundHasNestedGen = boundHasNestedGenLocal
                , tpTargetRigid = targetRigidLocal
                , tpTargetIsSchemeRoot = targetIsSchemeRootLocal
                , tpTargetIsSchemeRootForScope = targetIsSchemeRootForScopeLocal
                , tpTargetIsTyVar = targetIsTyVarLocal
                }
        TargetPlan
            { tpTargetBound = targetBound
            , tpTargetBoundUnderOtherGen = targetBoundUnderOtherGen
            , tpBoundUnderOtherGen = boundUnderOtherGen
            , tpBoundIsSchemeRoot = boundIsSchemeRoot
            , tpBoundIsVar = boundIsVar
            , tpBoundIsBase = boundIsBase
            , tpBoundIsStructural = boundIsStructural
            , tpBoundIsChild = boundIsChild
            , tpBoundIsDirectChild = boundIsDirectChild
            , tpBoundMentionsTarget = boundMentionsTarget
            , tpBoundHasForall = boundHasForall
            , tpBoundHasNestedGen = boundHasNestedGen
            , tpTargetRigid = targetRigid
            , tpTargetIsSchemeRoot = targetIsSchemeRoot
            , tpTargetIsSchemeRootForScope = targetIsSchemeRootForScope
            , tpTargetIsTyVar = targetIsTyVar
            } = targetPlan
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
    traceGeneralizeM env
        ("generalizeAt: targetBoundNode="
            ++ case targetBound of
                Just bnd ->
                    show (IntMap.lookup (getNodeId bnd) nodes)
                        ++ " boundOfBound="
                        ++ show (VarStore.lookupVarBound constraint bnd)
                Nothing -> "None"
        )
    let namedUnderGaRaw =
            case scopeGen of
                Just gid ->
                    case mbBindParentsGa of
                        Nothing ->
                            IntMap.elems $
                                IntMap.fromList
                                    [ (getNodeId nid, nid)
                                    | nid <- bindableChildrenUnder' (GenRef gid) ++ aliasBinderNodes
                                    ]
                        Just ga ->
                            let solvedKids = bindableChildrenUnder' (GenRef gid)
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
    let gammaPlan =
            let (baseGammaSetLocalOut, baseGammaRepLocalOut, namedUnderGaSetLocalOut, solvedToBasePrefLocalOut) =
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
                                    firstGenAncestorFrom bindParents (TypeRef (NodeId solvedKey)) == Just gid
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
                gammaAliasLocal =
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
                                                    case IntMap.lookup solvedKey solvedToBasePrefLocalOut of
                                                        Just baseN ->
                                                            firstGenAncestorFrom (gaBindParentsBase ga) (TypeRef baseN) == Just gid
                                                        Nothing -> False
                                                underBaseGamma =
                                                    case IntMap.lookup solvedKey solvedToBasePrefLocalOut of
                                                        Just baseN -> IntSet.member (getNodeId baseN) baseGammaSetLocalOut
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
                                        , IntSet.member baseKey baseGammaSetLocalOut
                                        ]
                                aliasFromBase =
                                    IntMap.fromList
                                        [ (solvedKey, repKey)
                                        | (solvedKey, baseKeys) <- IntMap.toList solvedToBaseAll
                                        , aliasEligible solvedKey
                                        , Just baseKey <- [pickBaseGamma baseKeys]
                                        , Just repKey <- [IntMap.lookup baseKey baseGammaRepLocalOut]
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
                                        , Just baseNid <- [IntMap.lookup solvedKeyC solvedToBasePrefLocalOut]
                                        , let baseKey = getNodeId baseNid
                                        , IntSet.member baseKey baseGammaSetLocalOut
                                        , Just repKey <- [IntMap.lookup baseKey baseGammaRepLocalOut]
                                        ]
                            in IntMap.union aliasFromBase aliasFromPref
                        Nothing -> IntMap.empty
                baseGammaRepSetLocal =
                    IntSet.fromList (IntMap.elems baseGammaRepLocalOut)
                reachableForBindersLocal =
                    let aliasReachable =
                            [ repKey
                            | (aliasKey, repKey) <- IntMap.toList gammaAliasLocal
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
                gammaKeyForLocal binderKey k =
                    case IntMap.lookup k gammaAliasLocal of
                        Just repKey | repKey == binderKey -> k
                        Just repKey -> repKey
                        Nothing -> k
                namedUnderGaLocal =
                    [ NodeId nid
                    | nid <- IntSet.toList namedUnderGaSetLocalOut
                    ]
                boundHasNamedOutsideGammaLocal =
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
                                                        not (IntSet.member (gammaKeyForLocal targetKey keyC) namedUnderGaSetLocalOut)
                                                    _ -> False
                                        _ -> False
                            in any isNamedOutside (IntSet.toList reachableBound)
                        Nothing -> False
                typeRootHasNamedOutsideGammaLocal = False
            in GammaPlan
                { gpBaseGammaSet = baseGammaSetLocalOut
                , gpBaseGammaRep = baseGammaRepLocalOut
                , gpNamedUnderGaSet = namedUnderGaSetLocalOut
                , gpSolvedToBasePref = solvedToBasePrefLocalOut
                , gpGammaAlias = gammaAliasLocal
                , gpBaseGammaRepSet = baseGammaRepSetLocal
                , gpReachableForBinders = reachableForBindersLocal
                , gpGammaKeyFor = gammaKeyForLocal
                , gpNamedUnderGa = namedUnderGaLocal
                , gpBoundHasNamedOutsideGamma = boundHasNamedOutsideGammaLocal
                , gpTypeRootHasNamedOutsideGamma = typeRootHasNamedOutsideGammaLocal
                }
        GammaPlan
            { gpBaseGammaSet = baseGammaSet
            , gpBaseGammaRep = baseGammaRep
            , gpNamedUnderGaSet = namedUnderGaSet
            , gpSolvedToBasePref = solvedToBasePref
            , gpGammaAlias = gammaAlias
            , gpBaseGammaRepSet = baseGammaRepSet
            , gpReachableForBinders = reachableForBinders
            , gpGammaKeyFor = gammaKeyFor
            , gpNamedUnderGa = namedUnderGa
            , gpBoundHasNamedOutsideGamma = boundHasNamedOutsideGamma
            , gpTypeRootHasNamedOutsideGamma = typeRootHasNamedOutsideGamma
            } = gammaPlan
    let dropPlan =
            let dropTargetLocal =
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
                                            Just bndVar -> not (hasExplicitBound' bndVar)
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
                schemeRootsLocal =
                    case scopeRootC of
                        GenRef _ | dropTargetLocal -> IntSet.singleton (canonKey target0)
                        _ -> IntSet.empty
            in DropPlan
                { dpDropTarget = dropTargetLocal
                , dpSchemeRoots = schemeRootsLocal
                }
        DropPlan
            { dpDropTarget = dropTarget
            , dpSchemeRoots = schemeRoots
            } = dropPlan
        liftToForall bnd0 =
            case IntMap.lookup (getNodeId (canonical bnd0)) schemeRootByBody of
                Just root -> canonical root
                Nothing ->
                    let climbToForall cur =
                            case IntMap.lookup (nodeRefKey (typeRef (canonical cur))) bindParents of
                                Just (TypeRef parent, _) ->
                                    case IntMap.lookup (canonKey parent) nodes of
                                        Just TyForall{} -> climbToForall (canonical parent)
                                        _ -> cur
                                _ -> cur
                    in climbToForall bnd0
        useSchemeBodyForScope = False
        typeRootPlan =
            let useBoundTypeRootLocal =
                    not targetIsSchemeRoot &&
                    case targetBound of
                        Just bnd ->
                            IntMap.member (getNodeId (canonical bnd)) schemeRootByBody
                        Nothing -> False
                schemeBodyRootLocal =
                    case targetBound of
                        Just bnd ->
                            case IntMap.lookup (getNodeId (canonical bnd)) nodes of
                                Just TyForall{ tnBody = b } -> canonical b
                                _ -> canonical bnd
                        Nothing -> typeRoot0
                targetInGammaLocal =
                    IntSet.member (canonKey target0) namedUnderGaSet
                targetIsBaseLikeLocal =
                    isBaseLikeKey (canonKey target0)
                schemeBodyChildUnderGenLocal =
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
                boundRootForTypeLocal bnd0 =
                    boundRootWith
                        getNodeId
                        canonical
                        (`IntMap.lookup` nodes)
                        (VarStore.lookupVarBound constraint)
                        (\key -> IntMap.lookup key schemeRootByBody)
                        True
                        bnd0
                typeRootFromTargetBoundLocal =
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
                                root = boundRootForTypeLocal bnd
                                useBoundRoot =
                                    not targetUnderScope
                                        || targetIsSchemeRootAlias
                                        || boundUnderOtherGen
                                        || (boundIsDirectChild && not targetIsSchemeRootForScope && not targetInGammaLocal)
                            in if useBoundRoot
                                    && canonical root /= canonical target0
                                then Just root
                                else Nothing
                        _ -> Nothing
                typeRoot0Local =
                    if useSchemeBodyForScope
                        then schemeBodyRootLocal
                        else
                            case (scopeRootC, targetIsSchemeRootForScope, targetIsTyVar, targetBound) of
                                (GenRef _, True, True, Nothing) ->
                                    case schemeBodyChildUnderGenLocal of
                                        Just child -> child
                                        Nothing -> schemeBodyRootLocal
                                _ ->
                                    case (dropTarget || useBoundTypeRootLocal, targetBound) of
                                        (True, Just bnd) -> liftToForall bnd
                                        _ ->
                                            case typeRootFromTargetBoundLocal of
                                                Just v -> v
                                                Nothing ->
                                                    case typeRootFromBoundVar of
                                                        Just v
                                                            | targetIsTyVar
                                                                && not (boundHasForallForVar v) -> v
                                                        Just v
                                                            | targetIsTyVar
                                                                && targetBoundUnderOtherGen -> v
                                                        Just v
                                                            | targetIsTyVar
                                                                && typeRootHasNamedOutsideGamma -> v
                                                        _ -> typeRoot0
                typeRootLocal =
                    case lookupNode (canonKey typeRoot0Local) of
                        Just TyForall{ tnBody = b }
                            | targetIsTyVar
                            , targetIsSchemeRootForScope
                            , Just gid <- scopeGen
                            , Just gidOwner <- IntMap.lookup (canonKey typeRoot0Local) schemeRootOwner
                            , gid == gidOwner ->
                                canonical b
                        _ -> typeRoot0Local
            in TypeRootPlan
                { trUseBoundTypeRoot = useBoundTypeRootLocal
                , trSchemeBodyRoot = schemeBodyRootLocal
                , trTargetInGamma = targetInGammaLocal
                , trTargetIsBaseLike = targetIsBaseLikeLocal
                , trSchemeBodyChildUnderGen = schemeBodyChildUnderGenLocal
                , trTypeRoot0 = typeRoot0Local
                , trTypeRoot = typeRootLocal
                }
        TypeRootPlan
            { trTargetIsBaseLike = targetIsBaseLike
            , trTypeRoot = typeRoot
            } = typeRootPlan
    let dropTypeRoot =
            dropTarget &&
            boundIsSchemeRoot &&
            not (isTyVarKey (canonKey typeRoot))
    reachableType <- Right (reachableFromWithBounds typeRoot)
    reachableTypeStructural <- Right (reachableFromStructural typeRoot)
    let typeRootIsForall =
            isTyForallKey (canonKey typeRoot)
    let orderBinderCandidatesFor =
            orderBinderCandidates
                (geDebugEnabled env)
                mbBindParentsGaInfo
                canonical
                constraint
                orderRootForBinders
                orderRootBaseForBinders
    binderPlan <-
        buildBinderPlan
            BinderPlanInput
                { bpiDebugEnabled = geDebugEnabled env
                , bpiConstraint = constraint
                , bpiNodes = nodes
                , bpiCanonical = canonical
                , bpiCanonKey = canonKey
                , bpiIsTyVarKey = isTyVarKey
                , bpiBindParents = bindParents
                , bpiBindParentsGa = mbBindParentsGaInfo
                , bpiScopeRootC = scopeRootC
                , bpiScopeGen = scopeGen
                , bpiTarget0 = target0
                , bpiTargetBound = targetBound
                , bpiTargetIsSchemeRoot = targetIsSchemeRoot
                , bpiTargetIsBaseLike = targetIsBaseLike
                , bpiBoundUnderOtherGen = boundUnderOtherGen
                , bpiDropTarget = dropTarget
                , bpiDropTypeRoot = dropTypeRoot
                , bpiSchemeRoots = schemeRoots
                , bpiBinders0 = binders0
                , bpiNamedUnderGa = namedUnderGa
                , bpiGammaAlias = gammaAlias
                , bpiBaseGammaSet = baseGammaSet
                , bpiBaseGammaRep = baseGammaRep
                , bpiBaseGammaRepSet = baseGammaRepSet
                , bpiNamedUnderGaSet = namedUnderGaSet
                , bpiSolvedToBasePref = solvedToBasePref
                , bpiReachable = reachable
                , bpiReachableForBinders = reachableForBinders
                , bpiReachableType = reachableType
                , bpiReachableTypeStructural = reachableTypeStructural
                , bpiTypeRoot0 = typeRoot0
                , bpiTypeRoot = typeRoot
                , bpiTypeRootFromBoundVar = typeRootFromBoundVar
                , bpiTypeRootIsForall = typeRootIsForall
                , bpiLiftToForall = liftToForall
                , bpiReachableFromWithBounds = reachableFromWithBounds
                , bpiResForReify = resForReify
                , bpiGammaKeyFor = gammaKeyFor
                , bpiNestedSchemeInteriorSet = nestedSchemeInteriorSet
                , bpiBoundIsSchemeRootVar = boundIsSchemeRootVar
                , bpiBoundIsSchemeRootAll = boundIsSchemeRootAll
                , bpiIsNestedSchemeBound = isNestedSchemeBound
                , bpiSchemeRootKeySet = schemeRootKeySet
                , bpiSchemeRootByBody = schemeRootByBody
                , bpiSchemeRootOwnerBase = schemeRootOwnerBase
                , bpiSchemeRootByBodyBase = schemeRootByBodyBase
                , bpiAliasBinderBases = aliasBinderBases
                , bpiParseNameId = parseNameId
                , bpiOrderBinderCandidates = orderBinderCandidatesFor
                }
    let binderNames = bpBinderNames binderPlan
        orderedBinders = bpOrderedBinderIds binderPlan
        subst0' = bpSubst0 binderPlan
        gammaAliasPlan = bpGammaAlias binderPlan
        nestedSchemeInteriorSetPlan = bpNestedSchemeInteriorSet binderPlan
        baseGammaRepPlan = bpBaseGammaRep binderPlan
        namedUnderGaSetPlan = bpNamedUnderGaSet binderPlan
        solvedToBasePrefPlan = bpSolvedToBasePref binderPlan
        aliasBinderBasesPlan = bpAliasBinderBases binderPlan
        orderBinders = bpOrderBinders binderPlan
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
                            , isQuantifiable' child
                            , not (IntMap.member (getNodeId (canonical child)) subst0')
                            ]
                        _ -> []
    orderedExtra <- orderBinders (map getNodeId extraCandidates)
    -- Phase 7: substitution maps (base, aliases, and bound-specific views).
    let reifyPlan =
            buildReifyPlan
                ReifyPlanInput
                    { rpiCanonical = canonical
                    , rpiBindParentsGa = mbBindParentsGaInfo
                    , rpiExtraNameStart = length binderNames
                    , rpiOrderedExtra = orderedExtra
                    , rpiSubst0 = subst0'
                    , rpiGammaAlias = gammaAliasPlan
                    , rpiNestedSchemeInteriorSet = nestedSchemeInteriorSetPlan
                    , rpiBaseGammaRep = baseGammaRepPlan
                    , rpiAliasBinderBases = aliasBinderBasesPlan
                    , rpiSolvedToBasePref = solvedToBasePrefPlan
                    , rpiTypeRoot = typeRoot
                    }
        ReifyPlan
            { rpSubst = subst
            , rpSubstBaseByKey = substBaseByKey
            , rpTypeRootForReify = typeRootForReify
            , rpSubstForReify = substForReify
            } = reifyPlan
        typeRootForReifyAdjustedPair =
            case IntMap.lookup (getNodeId (canonical typeRootForReify)) nodes of
                Just TyVar{} ->
                    case VarStore.lookupVarBound constraint (canonical typeRootForReify) of
                        Just bnd
                            | canonical bnd == canonical typeRoot ->
                                (typeRoot, subst)
                        _ -> (typeRootForReify, substForReify)
                _ -> (typeRootForReify, substForReify)
        (typeRootForReifyAdjusted, substForReifyAdjusted) =
            typeRootForReifyAdjustedPair
    let unboundedBinderNames =
            [ name
            | (name, nidInt) <- zip binderNames orderedBinders
            , case VarStore.lookupVarBound constraint (canonical (NodeId nidInt)) of
                Nothing -> True
                Just _ -> False
            ]
        uniqueUnboundedName =
            case unboundedBinderNames of
                [nm] -> Just nm
                _ -> Nothing
    let binderSet = IntSet.fromList orderedBinders
        isTargetSchemeBinder =
            isTargetSchemeBinderFor canonical constraint target0 targetIsBaseLike
        boundMentionsSelfAlias =
            boundMentionsSelfAliasFor
                canonical
                constraint
                nodes
                gammaAliasPlan
                nestedSchemeInteriorSetPlan
                reachableFromWithBounds
        bindingEnv =
            ReifyBindingEnv
                { rbeConstraint = constraint
                , rbeNodes = nodes
                , rbeCanonical = canonical
                , rbeBindParents = bindParents
                , rbeScopeGen = scopeGen
                , rbeSchemeRootOwner = schemeRootOwner
                , rbeSchemeRootByBody = schemeRootByBody
                , rbeSchemeRootByBodyBase = schemeRootByBodyBase
                , rbeSchemeRootKeySet = schemeRootKeySet
                , rbeGammaAlias = gammaAliasPlan
                , rbeAliasBinderBases = aliasBinderBasesPlan
                , rbeSolvedToBasePref = solvedToBasePrefPlan
                , rbeNamedUnderGaSet = namedUnderGaSetPlan
                , rbeBinderSet = binderSet
                , rbeUniqueUnboundedName = uniqueUnboundedName
                , rbeResForReify = resForReify
                , rbeBindParentsGa = mbBindParentsGaInfo
                , rbeBindingScopeGen = bindingScopeGen'
                , rbeHasExplicitBound = hasExplicitBound'
                , rbeIsTargetSchemeBinder = isTargetSchemeBinder
                , rbeBoundMentionsSelfAlias = boundMentionsSelfAlias
                , rbeContainsForall = containsForall
                , rbeParseNameId = parseNameId
                , rbeFirstGenAncestor = firstGenAncestorGa
                , rbeTraceM = traceGeneralizeM env
                }
    -- Phase 8: construct per-binder bounds.
    bindings <- mapM (bindingFor bindingEnv reifyPlan) (zip binderNames orderedBinders)

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
                    case IntMap.lookup (getNodeId typeRootC) solvedToBasePrefPlan of
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
                pure (bindingScopeFor constraint (typeRef typeRootC) == Just gid)
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
    let reifyTypeWithAliases bodyRoot substBase binderPairs = do
            let bodyRootC = canonical bodyRoot
                useConstraintReify =
                    case IntMap.lookup (getNodeId bodyRootC) nodes of
                        Just TyVar{} ->
                            case VarStore.lookupVarBound constraint bodyRootC of
                                Just bnd -> getNodeId bnd == getNodeId bodyRoot
                                Nothing -> False
                        _ -> False
                reifyWith substRoot substMap constraintArg resArg =
                    if useConstraintReify
                        then reifyTypeWithNamesNoFallbackOnConstraint constraintArg substMap substRoot
                        else reifyTypeWithNamesNoFallback resArg substMap substRoot
            let reachableWithoutBound bnd =
                    let stopSet = IntSet.singleton (getNodeId (canonical bnd))
                        shouldStop nid = IntSet.member (getNodeId nid) stopSet
                    in reachableFromStop
                        getNodeId
                        canonical
                        childrenWithBounds
                        shouldStop
                        bodyRoot
                aliasEntries =
                    [ (getNodeId (canonical bnd), name)
                    | (b, name) <- binderPairs
                    , Just bnd <- [VarStore.lookupVarBound constraint (canonical b)]
                    , canonical bnd /= bodyRootC
                    , not (IntSet.member (getNodeId (canonical b)) (reachableWithoutBound bnd))
                    ]
            if null aliasEntries
                then reifyWith bodyRoot substBase constraint resForReify
                else do
                    let aliasNodes =
                            IntMap.fromList
                                [ (key, TyVar { tnId = NodeId key, tnBound = Nothing })
                                | (key, _) <- aliasEntries
                                ]
                        constraintAlias =
                            constraint { cNodes = IntMap.union aliasNodes nodes }
                        substAlias =
                            IntMap.union (IntMap.fromList aliasEntries) substBase
                    let resAlias = resForReify { srConstraint = constraintAlias }
                    reifyWith bodyRoot substAlias constraintAlias resAlias

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
                            reifyTypeWithAliases
                                typeRootForReifyAdjusted
                                substForReifyAdjusted
                                (zip (map NodeId orderedBinders) binderNames)
                        else do
                            (sch, _substScheme) <-
                                generalizeAtWith False True mbBindParentsGa res schemeScope typeRootC
                            pure $
                                case sch of
                                    Forall binds body ->
                                        foldr (\(n, b) t -> TForall n b t) body binds
                else do
                    let explicitSchemeBinders = binders0
                    explicitSchemeTy <-
                        case (null bindings, scopeHasStructuralScheme, explicitSchemeBinders) of
                            (True, True, explicitBinders0@(_:_)) -> do
                                let binderKeys =
                                        IntSet.fromList
                                            [ getNodeId (canonical b)
                                            | b <- explicitBinders0
                                            ]
                                    binderKeysList = IntSet.toList binderKeys
                                    binders = [ NodeId key | key <- binderKeysList ]
                                    names = zipWith alphaName [0..] binderKeysList
                                    substExplicit = IntMap.fromList (zip binderKeysList names)
                                    explicitBodyRoot =
                                        case IntMap.lookup (getNodeId typeRootC) nodes of
                                            Just TyVar{} ->
                                                case VarStore.lookupVarBound constraint (canonical typeRootC) of
                                                    Just bnd -> canonical bnd
                                                    Nothing -> typeRootForReifyAdjusted
                                            _ -> typeRootForReifyAdjusted
                                if null binders
                                    then pure Nothing
                                    else do
                                        bodyTy <-
                                            reifyTypeWithAliases
                                                explicitBodyRoot
                                                substExplicit
                                                (zip binders names)
                                        bounds <-
                                            mapM
                                                (\(b, name) ->
                                                    case VarStore.lookupVarBound constraint (canonical b) of
                                                        Nothing -> pure (name, Nothing)
                                                        Just bnd -> do
                                                            bndTy <-
                                                                reifyBoundWithNames
                                                                    resForReify
                                                                    substExplicit
                                                                    (canonical bnd)
                                                            let selfBound =
                                                                    case bndTy of
                                                                        TVar v -> v == name
                                                                        _ -> False
                                                                mbBound =
                                                                    if bndTy == TBottom || selfBound
                                                                        then Nothing
                                                                        else Just bndTy
                                                            pure (name, mbBound)
                                                )
                                                (zip binders names)
                                        let tyExplicit =
                                                foldr
                                                    (\(n, mb) acc -> TForall n mb acc)
                                                    bodyTy
                                                    bounds
                                        pure (Just tyExplicit)
                            _ -> pure Nothing
                    case explicitSchemeTy of
                        Just ty -> pure ty
                        Nothing ->
                            if scopeHasStructuralScheme && null bindings
                                then
                                    reifyTypeWithNamesNoFallbackOnConstraint
                                        constraint
                                        substForReifyAdjusted
                                        typeRootForReifyAdjusted
                                else
                                    case mbBindParentsGa of
                                        Just ga ->
                                            case IntMap.lookup (getNodeId (canonical typeRoot)) solvedToBasePrefPlan of
                                                Just baseN
                                                    | canonical baseN /= canonical typeRoot -> do
                                                        tyBase <-
                                                            reifyTypeWithNamesNoFallbackOnConstraint
                                                                (gaBaseConstraint ga)
                                                                substBaseByKey
                                                                baseN
                                                        let freeBase = freeNamesFrom Set.empty tyBase
                                                            allowedBase = Set.fromList (IntMap.elems substBaseByKey)
                                                        if Set.isSubsetOf freeBase allowedBase
                                                            then pure tyBase
                                                            else reifyTypeWithAliases
                                                                typeRootForReifyAdjusted
                                                                substForReifyAdjusted
                                                                (zip (map NodeId orderedBinders) binderNames)
                                                _ ->
                                                    reifyTypeWithAliases
                                                        typeRootForReifyAdjusted
                                                        substForReifyAdjusted
                                                        (zip (map NodeId orderedBinders) binderNames)
                                        Nothing ->
                                            reifyTypeWithAliases
                                                typeRootForReifyAdjusted
                                                substForReifyAdjusted
                                                (zip (map NodeId orderedBinders) binderNames)
    ty0Raw <- reifySchemeType
    let aliasToTypeRootNames =
            [ name
            | (nidInt, name) <- zip orderedBinders binderNames
            , let nid = NodeId nidInt
            , Just bnd <- [VarStore.lookupVarBound constraint (canonical nid)]
            , canonical bnd == canonical typeRoot
            ]
        inlineAliasBinder ty binds = case ty of
            TVar v
                | v `elem` aliasToTypeRootNames ->
                    case lookup v binds of
                        Just (Just bnd)
                            | not (isVarBound bnd)
                            , not (isBaseBound bnd) ->
                                (bnd, filter (\(n, _) -> n /= v) binds)
                        _ -> (ty, binds)
            _ -> (ty, binds)
        (ty0RawAlias, bindingsAlias) = inlineAliasBinder ty0Raw bindings
    -- Phase 10: normalize, rename, and prune bindings + type.
    let canonAllVars ty =
            let (ty', _freeEnv, _n) = go [] [] (0 :: Int) ty
            in ty'
          where
            go boundEnv freeEnv n ty0 = case ty0 of
                TVar v ->
                    case lookup v boundEnv of
                        Just v' -> (TVar v', freeEnv, n)
                        Nothing ->
                            case lookup v freeEnv of
                                Just v' -> (TVar v', freeEnv, n)
                                Nothing ->
                                    let v' = "v" ++ show n
                                    in (TVar v', (v, v') : freeEnv, n + 1)
                TBase b -> (TBase b, freeEnv, n)
                TBottom -> (TBottom, freeEnv, n)
                TArrow a b ->
                    let (a', free1, n1) = go boundEnv freeEnv n a
                        (b', free2, n2) = go boundEnv free1 n1 b
                    in (TArrow a' b', free2, n2)
                TForall v mb body ->
                    let v' = "v" ++ show n
                        n1 = n + 1
                        (mb', free1, n2) =
                            case mb of
                                Nothing -> (Nothing, freeEnv, n1)
                                Just bnd ->
                                    let (bnd', free', n') = go boundEnv freeEnv n1 bnd
                                    in (Just bnd', free', n')
                        (body', free2, n3) = go ((v, v') : boundEnv) free1 n2 body
                    in (TForall v' mb' body', free2, n3)
        stripForalls ty = case ty of
            TForall _ _ body -> stripForalls body
            _ -> ty
        replaceAlias boundNorm v = goReplace
          where
            goReplace ty
                | canonAllVars ty == boundNorm = TVar v
                | otherwise =
                    case ty of
                        TArrow a b -> TArrow (goReplace a) (goReplace b)
                        TForall name mb body ->
                            TForall name (fmap goReplace mb) (goReplace body)
                        _ -> ty
        stripAliasForall ty = case ty of
            TForall v (Just bound) body
                | TVar v' <- body
                , v == v' ->
                    stripAliasForall bound
                | otherwise ->
                    TForall v (Just (stripAliasForall bound)) (stripAliasForall body)
            TForall v Nothing body ->
                TForall v Nothing (stripAliasForall body)
            TArrow a b -> TArrow (stripAliasForall a) (stripAliasForall b)
            _ -> ty
        collapseBoundAliases binds ty =
            foldr
                (\(v, mbBound) acc ->
                    case mbBound of
                        Nothing -> acc
                        Just bound ->
                            let boundCore = stripForalls bound
                            in if isVarBound boundCore
                                then acc
                                else
                                    let boundNorm = canonAllVars boundCore
                                    in if canonAllVars acc == boundNorm
                                        then acc
                                        else replaceAlias boundNorm v acc
                )
                ty
                binds
    let normalizeScheme tyRaw binds =
            let tyAdjusted =
                    case (binds, tyRaw) of
                        ((v, mb):_, TForall v' mb' body)
                            | v == v' && mb == mb' -> body
                        _ -> tyRaw
                tyAliased = stripAliasForall (collapseBoundAliases binds tyAdjusted)
            in traceGeneralize env
                ("generalizeAt: ty0Raw=" ++ show tyAliased
                    ++ " subst=" ++ show subst
                    ++ " bindings=" ++ show binds
                )
                (tyAliased, binds)
        (ty0RawAdjusted, bindingsAdjusted) = normalizeScheme ty0RawAlias bindingsAlias
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
                    , IntSet.member nidInt namedUnderGaSetPlan
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
                        aliasKey = case IntMap.lookup keyC gammaAliasPlan of
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
                                                    case IntMap.lookup nid solvedToBasePrefPlan of
                                                        Just baseN ->
                                                            firstGenAncestorFrom (gaBindParentsBase ga) (TypeRef baseN) == Just gid
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
                                        Just baseN -> firstGenAncestorFrom (gaBindParentsBase ga) (TypeRef baseN)
                                  , case mbBase of
                                        Nothing -> Nothing
                                        Just baseN ->
                                            IntMap.lookup (nodeRefKey (typeRef baseN)) (gaBindParentsBase ga)
                                  )
                                | name <- missing
                                , Just nid <- [parseNameId name]
                                , Just ga <- [mbBindParentsGa]
                                , let mbBase = IntMap.lookup nid (gaSolvedToBase ga)
                                , let mbBasePref = IntMap.lookup nid solvedToBasePrefPlan
                                ]
                        )
                        (Left $ SchemeFreeVars typeRootC missing)
    -- Phase 11: final validation (SchemeFreeVars).
    finalizeScheme missingNames
  where
debugGeneralizeEnabled :: Bool
debugGeneralizeEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_GENERALIZE"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugGeneralizeEnabled #-}
