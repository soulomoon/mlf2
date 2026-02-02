{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.Target (
    -- * Target plan (re-exported from TargetPlan submodule)
    TargetPlanInput(..),
    TargetPlan(..),
    buildTargetPlan,
    -- * Gamma plan (re-exported from GammaPlan submodule)
    GammaPlanInput(..),
    GammaPlan(..),
    buildGammaPlan,
    -- * Type root plan
    TypeRootPlanInput(..),
    TypeRootPlan(..),
    buildTypeRootPlan
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.IntMapUtils as IntMapUtils

-- Re-export TargetPlan from submodule
import MLF.Constraint.Presolution.Plan.Target.TargetPlan (
    TargetPlanInput(..),
    TargetPlan(..),
    buildTargetPlan
  )

-- Re-export GammaPlan from submodule
import MLF.Constraint.Presolution.Plan.Target.GammaPlan (
    GammaPlanInput(..),
    GammaPlan(..),
    buildGammaPlan
  )




data TypeRootPlanInput = TypeRootPlanInput
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
    , trpiTypeRoot0 :: NodeId
    , trpiTypeRootFromBoundVar :: Maybe NodeId
    , trpiTypeRootHasNamedOutsideGamma :: Bool
    , trpiBoundHasForallForVar :: NodeId -> Bool
    , trpiSchemeRootByBody :: IntMap.IntMap NodeId
    , trpiSchemeRootOwner :: IntMap.IntMap GenNodeId
    , trpiLiftToForall :: NodeId -> NodeId
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

buildTypeRootPlan :: TypeRootPlanInput -> TypeRootPlan
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
        targetIsSchemeRoot = trpiTargetIsSchemeRoot
        targetIsSchemeRootForScope = trpiTargetIsSchemeRootForScope
        targetIsTyVar = trpiTargetIsTyVar
        targetBoundUnderOtherGen = trpiTargetBoundUnderOtherGen
        namedUnderGaSet = trpiNamedUnderGaSet
        typeRoot0 = trpiTypeRoot0
        typeRootFromBoundVar = trpiTypeRootFromBoundVar
        typeRootHasNamedOutsideGamma = trpiTypeRootHasNamedOutsideGamma
        boundHasForallForVar = trpiBoundHasForallForVar
        schemeRootByBody = trpiSchemeRootByBody
        schemeRootOwner = trpiSchemeRootOwner
        liftToForall = trpiLiftToForall
        useBoundTypeRootLocal =
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
                        (True, Just bnd) -> liftToForall bnd
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
    in TypeRootPlan
        { trUseBoundTypeRoot = useBoundTypeRootLocal
        , trSchemeBodyRoot = schemeBodyRootLocal
        , trTargetInGamma = targetInGammaLocal
        , trTargetIsBaseLike = targetIsBaseLikeLocal
        , trSchemeBodyChildUnderGen = schemeBodyChildUnderGenLocal
        , trTypeRoot0 = typeRoot0Local
        , trTypeRoot = typeRootLocal
        }
