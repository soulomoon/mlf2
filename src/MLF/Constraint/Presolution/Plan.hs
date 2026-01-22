module MLF.Constraint.Presolution.Plan (
    GeneralizePolicy(..),
    policyDefault,
    policyKeepTarget,
    GeneralizePlan(..),
    ReifyPlan(..),
    buildGeneralizePlans
) where

import Data.Maybe (listToMaybe)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Solve (SolveResult(..))
import qualified MLF.Constraint.Solve as Solve
import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Presolution.Plan.BinderPlan
    ( BinderPlan(..)
    , BinderPlanInput(..)
    , buildBinderPlan
    )
import MLF.Constraint.Presolution.Plan.Context
    ( GaBindParents(..)
    , GeneralizeEnv(..)
    , GeneralizeCtx(..)
    , resolveContext
    , traceGeneralize
    , traceGeneralizeM
    )
import MLF.Constraint.Presolution.Plan.Helpers
    ( bindableChildrenUnder
    , computeAliasBinders
    , hasExplicitBound
    , isQuantifiable
    , mkIsBindable
    , selectBinders
    )
import MLF.Constraint.Presolution.Plan.Names (parseNameId)
import MLF.Constraint.Presolution.Plan.Ordering (orderBinderCandidates)
import MLF.Constraint.Presolution.Plan.Target
    ( TargetPlanInput(..)
    , TargetPlan(..)
    , buildTargetPlan
    , GammaPlanInput(..)
    , GammaPlan(..)
    , buildGammaPlan
    , DropPlanInput(..)
    , DropPlan(..)
    , buildDropPlan
    , TypeRootPlanInput(..)
    , TypeRootPlan(..)
    , buildTypeRootPlan
    )
import MLF.Constraint.Presolution.Plan.SchemeRoots
    ( SchemeRootInfo(..)
    , SchemeRootsPlan(..)
    , allowBoundTraversalFor
    )
import qualified MLF.Constraint.Presolution.Plan.ReifyPlan as Reify
import MLF.Util.ElabError (ElabError(..), bindingToElab)
import MLF.Util.Graph (reachableFrom, reachableFromStop)

data GeneralizePolicy = GeneralizePolicy
    { gpInlineAliasTarget :: Bool
    } deriving (Eq, Show)

policyDefault :: GeneralizePolicy
policyDefault =
    GeneralizePolicy
        { gpInlineAliasTarget = True
        }

policyKeepTarget :: GeneralizePolicy
policyKeepTarget =
    GeneralizePolicy
        { gpInlineAliasTarget = False
        }

data PresolutionEnv = PresolutionEnv
    { peConstraint :: Constraint
    , peSolveResult :: SolveResult
    , peCanonical :: NodeId -> NodeId
    , peBindParents :: BindParents
    , pePolicy :: GeneralizePolicy
    , peBindParentsGa :: Maybe GaBindParents
    , peScopeRoot :: NodeRef
    , peTargetNode :: NodeId
    }

data GeneralizePlan = GeneralizePlan
    { gpEnv :: GeneralizeEnv
    , gpContext :: GeneralizeCtx
    , gpSchemeRootsPlan :: SchemeRootsPlan
    , gpTargetPlan :: TargetPlan
    , gpGammaPlan :: GammaPlan
    , gpDropPlan :: DropPlan
    , gpTypeRootPlan :: TypeRootPlan
    , gpBinderPlan :: BinderPlan
    , gpScopeSchemeRoots :: IntSet.IntSet
    , gpScopeHasStructuralScheme :: Bool
    , gpBinders0 :: [NodeId]
    , gpReachableFromWithBounds :: NodeId -> IntSet.IntSet
    , gpReachableFromStructural :: NodeId -> IntSet.IntSet
    , gpBindParents :: BindParents
    }

data ReifyPlan = ReifyPlan
    { rpPlan :: Reify.ReifyPlan
    , rpTypeRootForReifyAdjusted :: NodeId
    , rpSubstForReifyAdjusted :: IntMap.IntMap String
    }

planGeneralizeAt :: PresolutionEnv -> Either ElabError GeneralizePlan
planGeneralizeAt PresolutionEnv
    { peSolveResult = res
    , pePolicy = policy
    , peBindParentsGa = mbBindParentsGa
    , peScopeRoot = scopeRoot
    , peTargetNode = targetNode
    } = do
    let env = mkGeneralizeEnv mbBindParentsGa res
        constraint = geConstraint env
        nodes = geNodes env
        canonical = geCanonical env
        canonKey = geCanonKey env
        isTyVarKey = geIsTyVarKey env
        isTyForallKey = geIsTyForallKey env
        isBaseLikeKey = geIsBaseLikeKey env
        allowDropTarget = gpInlineAliasTarget policy
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
            , gcBindParentsGaInfo = mbBindParentsGaInfo
            , gcSchemeRootsPlan = schemeRootsPlan
            } = ctx
    -- Phase 4: scheme-root metadata and bound traversal policy.
    let SchemeRootsPlan
            { srInfo = schemeRootInfo
            , srSchemeRootOwnerBase = schemeRootOwnerBase
            , srSchemeRootByBodyBase = schemeRootByBodyBase
            , srLookupSchemeRootOwner = lookupSchemeRootOwner
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
        childrenWithBoundsWith nodes' allowBoundTraversal' nid =
            case IntMap.lookup (getNodeId nid) nodes' of
                Nothing -> []
                Just node ->
                    case node of
                        TyVar{ tnBound = Just bnd }
                            | allowBoundTraversal' bnd ->
                                structuralChildrenWithBounds node
                        _ ->
                            structuralChildren node
        reachableFromWithBoundsWith canonical' nodes' allowBoundTraversal' =
            reachableFrom getNodeId canonical' (childrenWithBoundsWith nodes' allowBoundTraversal')
        childrenWithBounds =
            childrenWithBoundsWith nodes allowBoundTraversal
        reachableFromWithBounds root0 =
            reachableFromWithBoundsWith canonical nodes allowBoundTraversal root0
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
                            reachableFromWithBoundsWith id baseNodes allowBoundTraversalBase root0
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
    let scopeSchemeRootsFor gid =
            case IntMap.lookup (genNodeKey gid) (cGenNodes constraint) of
                Just gen ->
                    IntSet.fromList
                        [ getNodeId (canonical root)
                        | root <- gnSchemes gen
                        , case IntMap.lookup (nodeRefKey (typeRef root)) bindParents of
                            Just (GenRef gid', _) | gid' == gid -> True
                            _ -> False
                        ]
                Nothing -> IntSet.empty
        scopeSchemeRoots =
            case scopeGen of
                Just gid -> scopeSchemeRootsFor gid
                Nothing -> IntSet.empty
        scopeHasStructuralScheme =
            case scopeRootC of
                GenRef gid ->
                    case IntMap.lookup (genNodeKey gid) (cGenNodes constraint) of
                        Just gen ->
                            let schemeRoots = scopeSchemeRootsFor gid
                            in any
                                (\root ->
                                    not (IntSet.member (canonKey root) schemeRoots)
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
        isBindable =
            mkIsBindable
                bindFlags
                isQuantifiable'
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
                    | (gid, root) <- sriRootsWithGen schemeRootInfo
                    , Just gid /= scopeGen
                    ]
                )
                scopeSchemeRoots
        schemeRootBodySkipSet =
            IntSet.fromList
                [ bodyKey
                | (bodyKey, root) <- IntMap.toList schemeRootByBody
                , IntSet.member (canonKey root) schemeRootSkipSet
                ]
        schemeRootSkipKey key = IntSet.member key schemeRootSkipSet
        schemeRootBodySkipKey key = IntSet.member key schemeRootBodySkipSet
        schemeRootByBodyKey key = IntMap.member key schemeRootByBody
        schemeRootSkipOrBodyKey key =
            schemeRootSkipKey key || schemeRootByBodyKey key
        schemeRootSkipOrBodySkipKey key =
            schemeRootSkipKey key || schemeRootBodySkipKey key
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
                        bndKey = getNodeId bndC
                    in schemeRootSkipOrBodySkipKey bndKey
                        && not (IntSet.member bndKey reachable)
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
                                        bndKey = getNodeId bndC
                                    in if schemeRootSkipKey bndKey
                                        then True
                                        else
                                            case IntMap.lookup bndKey nodes of
                                                Just TyVar{} ->
                                                    walkBoundChain (IntSet.insert key visited) bndC
                                                _ -> False
                                Nothing -> False
            in walkBoundChain IntSet.empty v
        boundIsSchemeRootAll v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    let bndC = canonical bnd
                        bndKey = getNodeId bndC
                        hasSchemeRoot = schemeRootSkipOrBodyKey bndKey
                    in hasSchemeRoot
                Nothing -> False
        targetPlan =
            buildTargetPlan
                TargetPlanInput
                    { tpiConstraint = constraint
                    , tpiNodes = nodes
                    , tpiCanonical = canonical
                    , tpiCanonKey = canonKey
                    , tpiIsTyVarKey = isTyVarKey
                    , tpiScopeGen = scopeGen
                    , tpiScopeRootC = scopeRootC
                    , tpiBindParents = bindParents
                    , tpiTarget0 = target0
                    , tpiSchemeRootKeySetRaw = schemeRootKeySetRaw
                    , tpiSchemeRootKeySet = schemeRootKeySet
                    , tpiSchemeRootOwnerBase = schemeRootOwnerBase
                    , tpiSchemeRootByBodyBase = schemeRootByBodyBase
                    , tpiContainsForallForTarget = containsForallForTarget
                    , tpiFirstGenAncestor = firstGenAncestorGa
                    , tpiReachableFromWithBounds = reachableFromWithBounds
                    , tpiBindParentsGa = mbBindParentsGaInfo
                    }
        TargetPlan
            { tpTargetBound = targetBound
            , tpTargetBoundUnderOtherGen = targetBoundUnderOtherGen
            , tpBoundUnderOtherGen = boundUnderOtherGen
            , tpBoundIsSchemeRoot = boundIsSchemeRoot
            , tpBoundIsBase = boundIsBase
            , tpBoundIsDirectChild = boundIsDirectChild
            , tpBoundMentionsTarget = boundMentionsTarget
            , tpBoundHasForall = boundHasForall
            , tpBoundHasNestedGen = boundHasNestedGen
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
    let gammaPlan =
            buildGammaPlan
                GammaPlanInput
                    { gpiDebugEnabled = geDebugEnabled env
                    , gpiConstraint = constraint
                    , gpiNodes = nodes
                    , gpiCanonical = canonical
                    , gpiCanonKey = canonKey
                    , gpiIsTyVarKey = isTyVarKey
                    , gpiBindParents = bindParents
                    , gpiBindParentsGa = mbBindParentsGaInfo
                    , gpiScopeGen = scopeGen
                    , gpiTarget0 = target0
                    , gpiTargetBound = targetBound
                    , gpiSchemeRootOwnerBase = schemeRootOwnerBase
                    , gpiSchemeRootOwner = schemeRootOwner
                    , gpiSchemeRootByBody = schemeRootByBody
                    , gpiSchemeRootKeySet = schemeRootKeySet
                    , gpiOrderRoot = orderRoot
                    , gpiOrderRootBase = orderRootBase
                    , gpiTypeRoot0 = typeRoot0
                    , gpiNamedUnderGaInterior = IntSet.empty
                    , gpiNestedSchemeInteriorSet = nestedSchemeInteriorSet
                    , gpiReachableForBinders0 = reachableForBinders0
                    , gpiReachableFromWithBounds = reachableFromWithBounds
                    , gpiBindableChildrenUnder = bindableChildrenUnder'
                    , gpiAliasBinderNodes = aliasBinderNodes
                    , gpiFirstGenAncestor = firstGenAncestorGa
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
            buildDropPlan
                DropPlanInput
                    { dpiAllowDropTarget = allowDropTarget
                    , dpiTargetIsSchemeRoot = targetIsSchemeRoot
                    , dpiNodes = nodes
                    , dpiTarget0 = target0
                    , dpiTargetBound = targetBound
                    , dpiBoundIsBase = boundIsBase
                    , dpiBoundHasNestedGen = boundHasNestedGen
                    , dpiBoundHasNamedOutsideGamma = boundHasNamedOutsideGamma
                    , dpiBoundMentionsTarget = boundMentionsTarget
                    , dpiBoundHasForall = boundHasForall
                    , dpiScopeRootC = scopeRootC
                    , dpiCanonKey = canonKey
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
        typeRootPlan =
            buildTypeRootPlan
                TypeRootPlanInput
                    { trpiConstraint = constraint
                    , trpiNodes = nodes
                    , trpiCanonical = canonical
                    , trpiCanonKey = canonKey
                    , trpiIsTyVarKey = isTyVarKey
                    , trpiIsBaseLikeKey = isBaseLikeKey
                    , trpiBindParents = bindParents
                    , trpiScopeRootC = scopeRootC
                    , trpiScopeGen = scopeGen
                    , trpiTarget0 = target0
                    , trpiTargetBound = targetBound
                    , trpiTargetIsSchemeRoot = targetIsSchemeRoot
                    , trpiTargetIsSchemeRootForScope = targetIsSchemeRootForScope
                    , trpiTargetIsTyVar = targetIsTyVar
                    , trpiTargetBoundUnderOtherGen = targetBoundUnderOtherGen
                    , trpiBoundUnderOtherGen = boundUnderOtherGen
                    , trpiBoundIsDirectChild = boundIsDirectChild
                    , trpiNamedUnderGaSet = namedUnderGaSet
                    , trpiTypeRoot0 = typeRoot0
                    , trpiTypeRootFromBoundVar = typeRootFromBoundVar
                    , trpiTypeRootHasNamedOutsideGamma = typeRootHasNamedOutsideGamma
                    , trpiBoundHasForallForVar = boundHasForallForVar
                    , trpiAllowDropTarget = allowDropTarget
                    , trpiDropTarget = dropTarget
                    , trpiSchemeRootKeySet = schemeRootKeySet
                    , trpiSchemeRootByBody = schemeRootByBody
                    , trpiSchemeRootOwner = schemeRootOwner
                    , trpiLiftToForall = liftToForall
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
    pure GeneralizePlan
        { gpEnv = env
        , gpContext = ctx
        , gpSchemeRootsPlan = schemeRootsPlan
        , gpTargetPlan = targetPlan
        , gpGammaPlan = gammaPlan
        , gpDropPlan = dropPlan
        , gpTypeRootPlan = typeRootPlan
        , gpBinderPlan = binderPlan
        , gpScopeSchemeRoots = scopeSchemeRoots
        , gpScopeHasStructuralScheme = scopeHasStructuralScheme
        , gpBinders0 = binders0
        , gpReachableFromWithBounds = reachableFromWithBounds
        , gpReachableFromStructural = reachableFromStructural
        , gpBindParents = bindParents
        }

planReify :: PresolutionEnv -> GeneralizePlan -> Either ElabError ReifyPlan
planReify _ plan = do
    let GeneralizePlan
            { gpEnv = env
            , gpContext = ctx
            , gpBinderPlan = binderPlan
            , gpGammaPlan = gammaPlan
            , gpTypeRootPlan = typeRootPlan
            , gpBindParents = bindParents
            , gpTargetPlan = targetPlan
            , gpSchemeRootsPlan = schemeRootsPlan
            , gpReachableFromWithBounds = reachableFromWithBounds
            } = plan
        GeneralizeEnv
            { geConstraint = constraint
            , geNodes = nodes
            , geCanonical = canonical
            , geIsTyVarKey = isTyVarKey
            } = env
        GeneralizeCtx
            { gcScopeRootC = scopeRootC
            , gcScopeGen = scopeGen
            , gcTarget0 = target0
            , gcBindParentsGaInfo = mbBindParentsGaInfo
            } = ctx
        TargetPlan
            { tpTargetBound = targetBound
            } = targetPlan
        TypeRootPlan
            { trTypeRoot = typeRoot
            , trTargetIsBaseLike = targetIsBaseLike
            } = typeRootPlan
        GammaPlan
            { gpSolvedToBasePref = solvedToBasePrefPlan
            } = gammaPlan
        BinderPlan
            { bpBinderNames = binderNames
            , bpSubst0 = subst0'
            , bpGammaAlias = gammaAliasPlan
            , bpNestedSchemeInteriorSet = nestedSchemeInteriorSetPlan
            , bpBaseGammaRep = baseGammaRepPlan
            , bpAliasBinderBases = aliasBinderBasesPlan
            , bpOrderBinders = orderBinders
            } = binderPlan
        isQuantifiable' = isQuantifiable canonical constraint isTyVarKey
        extraCandidates =
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
    let reifyPlan =
            Reify.buildReifyPlan
                Reify.ReifyPlanInput
                    { Reify.rpiConstraint = constraint
                    , Reify.rpiNodes = nodes
                    , Reify.rpiCanonical = canonical
                    , Reify.rpiScopeRootC = scopeRootC
                    , Reify.rpiScopeGen = scopeGen
                    , Reify.rpiSchemeRootsPlan = schemeRootsPlan
                    , Reify.rpiTarget0 = target0
                    , Reify.rpiTargetIsBaseLike = targetIsBaseLike
                    , Reify.rpiTargetBound = targetBound
                    , Reify.rpiReachableFromWithBounds = reachableFromWithBounds
                    , Reify.rpiBindParentsGa = mbBindParentsGaInfo
                    , Reify.rpiExtraNameStart = length binderNames
                    , Reify.rpiOrderedExtra = orderedExtra
                    , Reify.rpiSubst0 = subst0'
                    , Reify.rpiGammaAlias = gammaAliasPlan
                    , Reify.rpiNestedSchemeInteriorSet = nestedSchemeInteriorSetPlan
                    , Reify.rpiBaseGammaRep = baseGammaRepPlan
                    , Reify.rpiAliasBinderBases = aliasBinderBasesPlan
                    , Reify.rpiSolvedToBasePref = solvedToBasePrefPlan
                    , Reify.rpiTypeRoot = typeRoot
                    }
    let Reify.ReifyPlan
            { Reify.rpSubst = subst
            , Reify.rpTypeRootForReify = typeRootForReify
            , Reify.rpSubstForReify = substForReify
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
    pure ReifyPlan
        { rpPlan = reifyPlan
        , rpTypeRootForReifyAdjusted = typeRootForReifyAdjusted
        , rpSubstForReifyAdjusted = substForReifyAdjusted
        }

buildGeneralizePlans
    :: SolveResult
    -> GeneralizePolicy
    -> Maybe GaBindParents
    -> NodeRef
    -> NodeId
    -> Either ElabError (GeneralizePlan, ReifyPlan)
buildGeneralizePlans res policy mbBindParentsGa scopeRoot targetNode = do
    let constraint = srConstraint res
        canonical = Solve.frWith (srUnionFind res)
        presEnv =
            PresolutionEnv
                { peConstraint = constraint
                , peSolveResult = res
                , peCanonical = canonical
                , peBindParents = cBindParents constraint
                , pePolicy = policy
                , peBindParentsGa = mbBindParentsGa
                , peScopeRoot = scopeRoot
                , peTargetNode = targetNode
                }
    genPlan <- planGeneralizeAt presEnv
    reifyPlan <- planReify presEnv genPlan
    pure (genPlan, reifyPlan)

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

debugGeneralizeEnabled :: Bool
debugGeneralizeEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_GENERALIZE"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugGeneralizeEnabled #-}
