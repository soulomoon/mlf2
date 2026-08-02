{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.Target.TypeRootPlan (
    TypeRootPlanInput(..),
    TypeRootPlan(..),
    ReifyRootSource(..),
    buildTypeRootPlan
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution.Plan.BinderPlan (GaBindParentsInfo(..))
import MLF.Constraint.Presolution.Plan.SchemeRoots
    ( SchemeRootsPlan(..)
    , schemeOwnerFromBody
    )
import MLF.Constraint.Types.Graph
import qualified MLF.Util.IntMapUtils as IntMapUtils

data TypeRootPlanInput p = TypeRootPlanInput
    { trpiNodes :: IntMap.IntMap TyNode
    , trpiCanonical :: NodeId -> NodeId
    , trpiCanonKey :: NodeId -> Int
    , trpiIsTyVarKey :: Int -> Bool
    , trpiIsBaseLikeKey :: Int -> Bool
    , trpiBindParents :: BindParents
    , trpiScopeRootC :: NodeRef
    , trpiScopeGen :: Maybe GenNodeId
    , trpiTarget0 :: NodeId
    , trpiTargetBound :: Maybe NodeId
    , trpiTargetIsSchemeRoot :: Bool
    , trpiTargetIsSchemeRootForScope :: Bool
    , trpiTargetIsTyVar :: Bool
    , trpiTargetBoundUnderOtherGen :: Bool
    , trpiNamedUnderGaSet :: IntSet.IntSet
    -- | Edge-result variables whose exact declarations are owned by the
    -- required Gamma plan.  Their bound is S'(operated), but the generalized
    -- body remains the result variable itself: @forall (a >= S'(n)). a@.
    , trpiRequiredGammaKeys :: IntSet.IntSet
    , trpiTypeRoot0 :: NodeId
    , trpiTypeRootFromBoundVar :: Maybe NodeId
    , trpiTypeRootHasNamedOutsideGamma :: Bool
    , trpiBoundHasForallForVar :: NodeId -> Bool
    , trpiSchemeRootByBody :: IntMap.IntMap NodeId
    , trpiSchemeRootOwner :: IntMap.IntMap GenNodeId
    , trpiSchemeRootsPlan :: SchemeRootsPlan
    , trpiSolvedToBasePref :: IntMap.IntMap NodeId
    , trpiBindParentsGa :: Maybe (GaBindParentsInfo p)
    , trpiRequiresLiveRefinements :: Bool
    , trpiLiftToForall :: NodeId -> NodeId
    }

-- | The graph domain from which the generalized result type must be reified.
--
-- A live solved root carries semantic refinements introduced by presolution.
-- A base root is valid only when it is the structural source of a scheme body;
-- copy provenance alone is not evidence that the two roots have the same type.
data ReifyRootSource
    = ReifyLiveRoot NodeId
    | ReifyBaseSchemeRoot NodeId
    deriving (Eq, Show)

data TypeRootPlan = TypeRootPlan
    { trUseBoundTypeRoot :: Bool
    , trSchemeBodyRoot :: NodeId
    , trTargetInGamma :: Bool
    , trTargetIsBaseLike :: Bool
    , trSchemeBodyChildUnderGen :: Maybe NodeId
    , trTypeRoot0 :: NodeId
    , trTypeRoot :: NodeId
    , trReifyRootSource :: ReifyRootSource
    }

buildTypeRootPlan :: TypeRootPlanInput p -> TypeRootPlan
buildTypeRootPlan TypeRootPlanInput{..} =
    let nodes = trpiNodes
        canonical = trpiCanonical
        canonKey = trpiCanonKey
        isTyVarKey = trpiIsTyVarKey
        isBaseLikeKey = trpiIsBaseLikeKey
        bindParents = trpiBindParents
        scopeRootC = trpiScopeRootC
        scopeGen = trpiScopeGen
        target0 = trpiTarget0
        targetBound = trpiTargetBound
        targetIsSchemeRootForScope = trpiTargetIsSchemeRootForScope
        targetIsTyVar = trpiTargetIsTyVar
        targetBoundUnderOtherGen = trpiTargetBoundUnderOtherGen
        namedUnderGaSet = trpiNamedUnderGaSet
        requiredGammaKeys = trpiRequiredGammaKeys
        typeRoot0 = trpiTypeRoot0
        typeRootFromBoundVar = trpiTypeRootFromBoundVar
        typeRootHasNamedOutsideGamma = trpiTypeRootHasNamedOutsideGamma
        boundHasForallForVar = trpiBoundHasForallForVar
        schemeRootByBody = trpiSchemeRootByBody
        schemeRootOwner = trpiSchemeRootOwner
        schemeRootsPlan = trpiSchemeRootsPlan
        solvedToBasePref = trpiSolvedToBasePref
        bindParentsGa = trpiBindParentsGa
        requiresLiveRefinements = trpiRequiresLiveRefinements
        liftToForall = trpiLiftToForall
        -- An ordinary bounded scheme root remains a quantified root.  Only
        -- Phase 1's let-edge certificate proves that this root is an
        -- administrative wrapper whose live bound is the semantic type root.
        restoredSchemeRootUsesBound =
            case (targetBound, bindParentsGa) of
                (Just bnd, Just ga) ->
                    case IntMap.lookup
                        (getNodeId target0)
                        (gbiRestoredSchemeRootTargets ga)
                    of
                        Just target -> canonical target == canonical bnd
                        Nothing -> False
                _ -> False
        useBoundTypeRootLocal =
            not targetIsRequiredGamma
                && ( restoredSchemeRootUsesBound
                        || ( (not targetIsSchemeRootForScope || targetBoundUnderOtherGen)
                                && case targetBound of
                                    Just bnd ->
                                        let bndC = canonical bnd
                                            boundIsSchemeBody =
                                                IntMap.member (getNodeId bndC) schemeRootByBody
                                            -- A result wrapper owned by a nested root is not a
                                            -- binder of the enclosing scheme.  When that wrapper
                                            -- owns a complete structural bound, the bound is its
                                            -- authoritative result type; reifying the wrapper
                                            -- itself would manufacture a free type variable and
                                            -- make the real binders look vacuous.  Locally named
                                            -- Gamma variables remain flexible and therefore keep
                                            -- the wrapper as their type root.
                                            targetOwnsStructuralBound =
                                                targetBoundUnderOtherGen
                                                    && not targetInGammaLocal
                                                    && case IntMap.lookup (nodeRefKey (typeRef bndC)) bindParents of
                                                        Just (TypeRef owner, _) ->
                                                            canonical owner == canonical target0
                                                                && isStructuralNode bndC
                                                        _ -> False
                                        in boundIsSchemeBody || targetOwnsStructuralBound
                                    Nothing -> False
                           )
                   )
        schemeBodyRootLocal =
            case targetBound of
                Just bnd ->
                    case IntMap.lookup (getNodeId (canonical bnd)) nodes of
                        Just TyForall{ tnBody = b } -> canonical b
                        Just TyMu{ tnBody = b } -> canonical b
                        _ -> canonical bnd
                Nothing -> typeRoot0
        targetInGammaLocal =
            IntSet.member (canonKey target0) namedUnderGaSet
        targetIsRequiredGamma =
            IntSet.member (canonKey target0) requiredGammaKeys
        targetIsBaseLikeLocal =
            isBaseLikeKey (canonKey target0)
        schemeBodyChildUnderGenLocal =
            case scopeRootC of
                GenRef gid | targetIsSchemeRootForScope && targetIsTyVar ->
                    let children =
                            [ canonical child
                            | child <- IntMapUtils.typeChildrenOfGen bindParents gid
                            , not (isTyVarKey (canonKey child))
                            ]
                    in case children of
                        [child] -> Just child
                        _ -> Nothing
                _ -> Nothing
        typeRoot0Local =
            case (scopeRootC, targetIsSchemeRootForScope, targetIsTyVar, targetBound) of
                (GenRef _, True, True, Nothing) ->
                    case schemeBodyChildUnderGenLocal of
                        Just child -> child
                        Nothing -> schemeBodyRootLocal
                _ ->
                    case (useBoundTypeRootLocal, targetBound) of
                        (True, Just bnd) ->
                            let lifted = liftToForall bnd
                            in if canonical lifted == canonical target0
                                then canonical bnd
                                else lifted
                        _ ->
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
            case IntMap.lookup (canonKey typeRoot0Local) nodes of
                Just TyForall{ tnBody = b }
                    | targetIsTyVar
                    , targetIsSchemeRootForScope
                    , Just gid <- scopeGen
                    , Just gidOwner <- IntMap.lookup (canonKey typeRoot0Local) schemeRootOwner
                    , gid == gidOwner ->
                        canonical b
                _ -> typeRoot0Local
        typeRootCLocal = canonical typeRootLocal
        typeRootOwnerLocal =
            case IntMap.lookup (canonKey typeRootCLocal) schemeRootOwner of
                Just owner -> Just owner
                Nothing ->
                    fst
                        (schemeOwnerFromBody schemeRootsPlan solvedToBasePref typeRootCLocal)
        typeRootIsStructuralLocal =
            isStructuralNode typeRootCLocal
        isStructuralNode node =
            case IntMap.lookup (canonKey node) nodes of
                Just TyVar{} -> False
                Just TyBase{} -> False
                Just TyBottom{} -> False
                Just _ -> True
                Nothing -> False
        liveRootExists = IntMap.member (canonKey typeRootCLocal) nodes
        baseStructuralSource = do
            baseRoot <- IntMap.lookup (canonKey typeRootCLocal) solvedToBasePref
            if baseRootIsStructuralSource baseRoot
                then Just baseRoot
                else Nothing
        baseRootNeedsLiveRefinement =
            case baseStructuralSource of
                Just baseRoot -> baseRootHasRefinedLiveVariable baseRoot
                Nothing -> False
        reifyRootSourceLocal =
            if requiresLiveRefinements
                || baseRootNeedsLiveRefinement
                then liveSource
                else
                    case baseStructuralSource of
                        Just baseRoot
                            | not liveRootExists ->
                                ReifyBaseSchemeRoot baseRoot
                        _ ->
                            case (scopeGen, typeRootOwnerLocal) of
                                (Just scopeGid, Just owner)
                                    | scopeGid == owner
                                    , typeRootIsStructuralLocal ->
                                        case baseStructuralSource of
                                            Just baseRoot
                                                | not (baseRootReturnsToLiveRoot baseRoot) ->
                                                    ReifyBaseSchemeRoot baseRoot
                                            _ -> liveSource
                                _ -> liveSource
        -- A live root must be carried in the live graph's canonical domain.
        -- The pre-canonical key can already have been removed from cNodes;
        -- retaining it here would make reification rediscover a redirect that
        -- the plan has already proved.
        liveSource = ReifyLiveRoot typeRootCLocal
        baseRootIsStructuralSource baseRoot =
            case bindParentsGa >>= \ga -> lookupNodeIn (cNodes (gbiBaseConstraint ga)) baseRoot of
                Just TyVar{} -> False
                Just TyBase{} -> False
                Just TyBottom{} -> False
                Just _ -> True
                Nothing -> False
        -- A base structural root is not a faithful reification source when
        -- one of its variable slots has acquired structure in the live
        -- presolution.  Reifying the base root would leave that old variable
        -- free, while BinderPlan quite correctly plans only the live
        -- declaration tree.  Select the live root here, while both graph
        -- domains and their projection are still available.
        baseRootHasRefinedLiveVariable baseRoot =
            case bindParentsGa of
                Nothing -> False
                Just ga ->
                    any
                        (baseVariableHasLiveRefinement ga)
                        (baseReachableNodes ga baseRoot)
        baseReachableNodes ga root = go IntSet.empty [root]
          where
            baseNodes = cNodes (gbiBaseConstraint ga)

            go seen [] = IntSet.toList seen
            go seen (node : rest)
                | IntSet.member key seen = go seen rest
                | otherwise =
                    let children =
                            case lookupNodeIn baseNodes node of
                                Just tyNode -> structuralChildrenWithBounds tyNode
                                Nothing -> []
                    in go (IntSet.insert key seen) (children ++ rest)
              where
                key = getNodeId node
        baseVariableHasLiveRefinement ga baseKey =
            case
                lookupNodeIn
                    (cNodes (gbiBaseConstraint ga))
                    (NodeId baseKey)
            of
                Just TyVar{} ->
                    any liveNodeIsRefined (liveCandidatesForBase ga baseKey)
                _ -> False
        liveCandidatesForBase ga baseKey =
            directCandidate ++ reverseCandidates
          where
            directCandidate =
                case IntMap.lookup baseKey (gbiBaseToSolved ga) of
                    Just liveNode -> [canonical liveNode]
                    Nothing -> []
            reverseCandidates =
                [ canonical (NodeId liveKey)
                | (liveKey, baseNode) <- IntMap.toList solvedToBasePref
                , getNodeId baseNode == baseKey
                ]
        liveNodeIsRefined liveNode =
            case IntMap.lookup (canonKey liveNode) nodes of
                Just TyVar{tnBound = Nothing} -> False
                Just _ -> True
                Nothing -> False
        baseRootReturnsToLiveRoot baseRoot =
            case bindParentsGa of
                Just ga ->
                    let baseSchemeRoot =
                            IntMap.lookup
                                (getNodeId baseRoot)
                                (srSchemeRootByBodyBase schemeRootsPlan)
                        candidates =
                            case baseSchemeRoot of
                                Just root -> [baseRoot, root]
                                Nothing -> [baseRoot]
                        returns candidate =
                            case IntMap.lookup (getNodeId candidate) (gbiBaseToSolved ga) of
                                Just solvedRoot ->
                                    case IntMap.lookup (canonKey solvedRoot) nodes of
                                        Just TyVar{tnBound = Just bnd} ->
                                            canonical bnd == typeRootCLocal
                                        _ -> False
                                Nothing -> False
                    in any returns candidates
                Nothing -> False
        result =
            TypeRootPlan
                { trUseBoundTypeRoot = useBoundTypeRootLocal
                , trSchemeBodyRoot = schemeBodyRootLocal
                , trTargetInGamma = targetInGammaLocal
                , trTargetIsBaseLike = targetIsBaseLikeLocal
                , trSchemeBodyChildUnderGen = schemeBodyChildUnderGenLocal
                , trTypeRoot0 = typeRoot0Local
                , trTypeRoot = typeRootLocal
                , trReifyRootSource = reifyRootSourceLocal
                }
    in result
