{- |
Module      : MLF.Constraint.Presolution.Plan.BinderPlan.Types
Description : Types for binder plan construction
Copyright   : (c) 2024
License     : BSD-3-Clause
-}
module MLF.Constraint.Presolution.Plan.BinderPlan.Types (
    BinderPlanInput(..),
    BinderPlan(..)
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution.Plan.BinderPlan.Order (GaBindParentsInfo)
import MLF.Constraint.Presolution.Plan.Requirements (RequiredGammaBinder)
import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Types.Graph
import MLF.Types.Elab (TypeBinderRef)
import MLF.Util.ElabError (ElabError)

data BinderPlanInput p = BinderPlanInput
    { bpiDebugEnabled :: Bool
    , bpiConstraint :: Constraint p
    , bpiNodes :: IntMap.IntMap TyNode
    , bpiCanonical :: NodeId -> NodeId
    , bpiCanonKey :: NodeId -> Int
    , bpiIsTyVarKey :: Int -> Bool
    , bpiBindParents :: BindParents
    -- | Unsoftened binding colours consumed by final rigid inlining.  Binder
    -- closure must account for identities that this later construction step
    -- exposes from a rigid variable's bound.
    , bpiRigidBindParents :: BindParents
    , bpiBindParentsGa :: Maybe (GaBindParentsInfo p)
    , bpiScopeRootC :: NodeRef
    , bpiScopeGen :: Maybe GenNodeId
    , bpiTarget0 :: NodeId
    , bpiTargetBound :: Maybe NodeId
    , bpiTargetIsSchemeRoot :: Bool
    , bpiTargetIsBaseLike :: Bool
    , bpiBoundUnderOtherGen :: Bool
    , bpiBinders0 :: [NodeId]
    , bpiNamedUnderGa :: [NodeId]
    , bpiGammaAlias :: IntMap.IntMap Int
    , bpiBaseGammaSet :: IntSet.IntSet
    , bpiBaseGammaRep :: IntMap.IntMap Int
    , bpiBaseGammaRepSet :: IntSet.IntSet
    , bpiNamedUnderGaSet :: IntSet.IntSet
    , bpiSolvedToBasePref :: IntMap.IntMap NodeId
    , bpiReachable :: IntSet.IntSet
    , bpiReachableForBinders :: IntSet.IntSet
    , bpiReachableType :: IntSet.IntSet
    , bpiReachableTypeStructural :: IntSet.IntSet
    -- | Live variables whose exact frozen-base origin is a binder owned by a
    -- forall that no longer survives in the selected live reification root.
    -- These are declaration candidates by construction even when solving has
    -- reparented the live occurrence to an ancestor gen.
    , bpiEscapedFrozenForallBinders :: [NodeId]
    -- | Live source-routed occurrences whose exact source identity remains
    -- free after reifying the selected live root.  The exact source-facing
    -- free-variable result, together with exclusion from the ambient binder
    -- set, is the construction proof that the enclosing binder plan must
    -- declare them even when graph scope or nested-scheme filtering alone
    -- would hide the occurrence.
    , bpiEscapedSourceBinderOccurrences :: [NodeId]
    , bpiTypeRoot0 :: NodeId
    , bpiTypeRoot :: NodeId
    , bpiTypeRootFromBoundVar :: Maybe NodeId
    , bpiLiftToForall :: NodeId -> NodeId
    , bpiReachableFromWithBounds :: NodeId -> IntSet.IntSet
    , bpiResForReify :: PresolutionView p
    , bpiGammaKeyFor :: Int -> Int -> Int
    , bpiNestedSchemeInteriorSet :: IntSet.IntSet
    , bpiBoundIsSchemeRootVar :: NodeId -> Bool
    , bpiBoundIsSchemeRootAll :: NodeId -> Bool
    , bpiIsNestedSchemeBound :: NodeId -> Bool
    , bpiSchemeRootKeySet :: IntSet.IntSet
    , bpiSchemeRootByBody :: IntMap.IntMap NodeId
    , bpiSchemeRootOwner :: IntMap.IntMap GenNodeId
    , bpiSchemeRootOwnerBase :: IntMap.IntMap GenNodeId
    , bpiSchemeRootByBodyBase :: IntMap.IntMap NodeId
    , bpiAliasBinderBases :: IntSet.IntSet
    , bpiOrderBinderCandidates :: [Int] -> (Int -> Either ElabError [Int]) -> Either ElabError [Int]
    , bpiRequiredGamma :: IntMap.IntMap RequiredGammaBinder
    , bpiLocallyClosedGammaNodes :: IntSet.IntSet
    , bpiSourceBinderRefs :: IntMap.IntMap TypeBinderRef
    , bpiAmbientBinderRefs :: [TypeBinderRef]
    , bpiTermUsedRootBinderRefs :: [TypeBinderRef]
    }

data BinderPlan = BinderPlan
    { bpOrderedBinders :: [(Int, TypeBinderRef)]
    -- | Exact routes for both locally planned declarations and candidates
    -- discharged by an enclosing ambient declaration.
    , bpBinderRefRoutes :: IntMap.IntMap TypeBinderRef
    , bpRootBodyClosureKeys :: IntSet.IntSet
    , bpInheritedRigidAliasRoutes :: IntMap.IntMap TypeBinderRef
    , bpLocallyClosedGammaKeys :: IntSet.IntSet
    , bpNestedSchemeInteriorSet :: IntSet.IntSet
    , bpGammaAlias :: IntMap.IntMap Int
    , bpBaseGammaSet :: IntSet.IntSet
    , bpBaseGammaRep :: IntMap.IntMap Int
    , bpNamedUnderGaSet :: IntSet.IntSet
    , bpSolvedToBasePref :: IntMap.IntMap NodeId
    , bpReachableForBinders :: IntSet.IntSet
    , bpAliasBinderBases :: IntSet.IntSet
    , bpOrderBinders :: [Int] -> Either ElabError [Int]
    , bpRequiredGamma :: IntMap.IntMap RequiredGammaBinder
    , bpSourceBinderRefs :: IntMap.IntMap TypeBinderRef
    , bpAmbientBinderRefs :: [TypeBinderRef]
    -- | Construction-used refs matched to declarations already selected by
    -- this plan.  Finalization may preserve these declarations, but cannot
    -- use the field to synthesize an unplanned binder.
    , bpTermUsedRootBinderRefs :: [TypeBinderRef]
    }
