{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.Target.TargetPlan (
    TargetPlanInput(..),
    TargetPlan(..),
    buildTargetPlan
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Presolution.Plan.BinderPlan (GaBindParentsInfo(..))

data TargetPlanInput = TargetPlanInput
    { tpiConstraint :: Constraint
    , tpiNodes :: IntMap.IntMap TyNode
    , tpiCanonical :: NodeId -> NodeId
    , tpiCanonKey :: NodeId -> Int
    , tpiIsTyVarKey :: Int -> Bool
    , tpiScopeGen :: Maybe GenNodeId
    , tpiScopeRootC :: NodeRef
    , tpiBindParents :: BindParents
    , tpiTarget0 :: NodeId
    , tpiSchemeRootKeySetRaw :: IntSet.IntSet
    , tpiSchemeRootKeySet :: IntSet.IntSet
    , tpiSchemeRootOwnerBase :: IntMap.IntMap GenNodeId
    , tpiSchemeRootByBodyBase :: IntMap.IntMap NodeId
    , tpiContainsForallForTarget :: NodeId -> Bool
    , tpiFirstGenAncestor :: NodeRef -> Maybe GenNodeId
    , tpiReachableFromWithBounds :: NodeId -> IntSet.IntSet
    , tpiBindParentsGa :: Maybe GaBindParentsInfo
    }

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
    , tpTargetIsSchemeRoot :: Bool
    , tpTargetIsSchemeRootForScope :: Bool
    , tpTargetIsTyVar :: Bool
    }

buildTargetPlan :: TargetPlanInput -> TargetPlan
buildTargetPlan TargetPlanInput{..} =
    let constraint = tpiConstraint
        nodes = tpiNodes
        canonical = tpiCanonical
        canonKey = tpiCanonKey
        isTyVarKey = tpiIsTyVarKey
        scopeGen = tpiScopeGen
        scopeRootC = tpiScopeRootC
        bindParents = tpiBindParents
        target0 = tpiTarget0
        schemeRootKeySetRaw = tpiSchemeRootKeySetRaw
        schemeRootKeySet = tpiSchemeRootKeySet
        schemeRootOwnerBase = tpiSchemeRootOwnerBase
        schemeRootByBodyBase = tpiSchemeRootByBodyBase
        containsForallForTarget = tpiContainsForallForTarget
        firstGenAncestorGa = tpiFirstGenAncestor
        reachableFromWithBounds = tpiReachableFromWithBounds
        mbBindParentsGa = tpiBindParentsGa
        targetBoundLocalFromBase =
            case mbBindParentsGa of
                Just ga ->
                    case IntMap.lookup (getNodeId target0) (gbiSolvedToBase ga) of
                        Just baseN ->
                            case VarStore.lookupVarBound (gbiBaseConstraint ga) baseN of
                                Just bndBase ->
                                    case IntMap.lookup (getNodeId bndBase) (gbiBaseToSolved ga) of
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
                    case NodeAccess.lookupGenNode constraint gid of
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
                                                    kids = structuralChildrenWithBounds node
                                                in any (walkMentions visited') kids
                    in walkMentions IntSet.empty bnd
        boundHasForallLocal =
            let boundHasForallLocalBase =
                    case (mbBindParentsGa, targetBoundLocal) of
                        (Just ga, Just bnd) ->
                            case IntMap.lookup (getNodeId (canonical bnd)) (gbiSolvedToBase ga) of
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
        targetIsSchemeRootLocal =
            IntSet.member (canonKey target0) schemeRootKeySet
        targetIsSchemeRootForScopeLocal =
            case (scopeGen, mbBindParentsGa) of
                (Just gid, Just ga) ->
                    case IntMap.lookup (canonKey target0) (gbiSolvedToBase ga) of
                        Just baseN ->
                            case IntMap.lookup (getNodeId baseN) schemeRootOwnerBase of
                                Just ownerGid -> ownerGid == gid
                                Nothing -> False
                        Nothing -> False
                (Just gid, Nothing) ->
                    case NodeAccess.lookupGenNode constraint gid of
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
        , tpTargetIsSchemeRoot = targetIsSchemeRootLocal
        , tpTargetIsSchemeRootForScope = targetIsSchemeRootForScopeLocal
        , tpTargetIsTyVar = targetIsTyVarLocal
        }
