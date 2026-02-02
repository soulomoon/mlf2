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
import MLF.Constraint.Solve (SolveResult)
import MLF.Constraint.Types
import MLF.Util.ElabError (ElabError)

data BinderPlanInput = BinderPlanInput
    { bpiDebugEnabled :: Bool
    , bpiConstraint :: Constraint
    , bpiNodes :: IntMap.IntMap TyNode
    , bpiCanonical :: NodeId -> NodeId
    , bpiCanonKey :: NodeId -> Int
    , bpiIsTyVarKey :: Int -> Bool
    , bpiBindParents :: BindParents
    , bpiBindParentsGa :: Maybe GaBindParentsInfo
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
    , bpiTypeRoot0 :: NodeId
    , bpiTypeRoot :: NodeId
    , bpiTypeRootFromBoundVar :: Maybe NodeId
    , bpiTypeRootIsForall :: Bool
    , bpiLiftToForall :: NodeId -> NodeId
    , bpiReachableFromWithBounds :: NodeId -> IntSet.IntSet
    , bpiResForReify :: SolveResult
    , bpiGammaKeyFor :: Int -> Int -> Int
    , bpiNestedSchemeInteriorSet :: IntSet.IntSet
    , bpiBoundIsSchemeRootVar :: NodeId -> Bool
    , bpiBoundIsSchemeRootAll :: NodeId -> Bool
    , bpiIsNestedSchemeBound :: NodeId -> Bool
    , bpiSchemeRootKeySet :: IntSet.IntSet
    , bpiSchemeRootByBody :: IntMap.IntMap NodeId
    , bpiSchemeRootOwnerBase :: IntMap.IntMap GenNodeId
    , bpiSchemeRootByBodyBase :: IntMap.IntMap NodeId
    , bpiParseNameId :: String -> Maybe Int
    , bpiAliasBinderBases :: IntSet.IntSet
    , bpiOrderBinderCandidates :: [Int] -> (Int -> Either ElabError [Int]) -> Either ElabError [Int]
    }

data BinderPlan = BinderPlan
    { bpBindersCanon :: [NodeId]
    , bpBinderIds :: [Int]
    , bpOrderedBinderIds :: [Int]
    , bpBinderNames :: [String]
    , bpSubst0 :: IntMap.IntMap String
    , bpNestedSchemeInteriorSet :: IntSet.IntSet
    , bpGammaAlias :: IntMap.IntMap Int
    , bpBaseGammaSet :: IntSet.IntSet
    , bpBaseGammaRep :: IntMap.IntMap Int
    , bpNamedUnderGaSet :: IntSet.IntSet
    , bpSolvedToBasePref :: IntMap.IntMap NodeId
    , bpReachableForBinders :: IntSet.IntSet
    , bpAliasBinderBases :: IntSet.IntSet
    , bpOrderBinders :: [Int] -> Either ElabError [Int]
    }
