module MLF.Constraint.Presolution.Plan.Finalize.TestSupport
  ( FinalizeBinderPlan,
    mkFinalizeBinderPlan,
    finalizeBinderPlanBinderRefs,
    canonicalizeReifySubstRefsForTest,
    certifiedFromBaseAliasRouteForTest,
    mergeReifySubstRefsForTest,
  )
where

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Presolution.Plan.Finalize
  ( FinalizeBinderPlan,
    finalizeBinderPlanBinderRefs,
    mkFinalizeBinderPlan,
  )
import MLF.Constraint.Presolution.Plan.ReifyPlan
  ( canonicalizeSubstRefs,
    certifiedFromBaseAliasRoute,
    mergeReifySubstRefs,
  )
import MLF.Constraint.Types.Graph (NodeId)
import MLF.Types.Elab (TypeBinderRef)

canonicalizeReifySubstRefsForTest
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
canonicalizeReifySubstRefsForTest =
  canonicalizeSubstRefs

certifiedFromBaseAliasRouteForTest
  :: IntMap.IntMap Int
  -> IntMap.IntMap Int
  -> IntMap.IntMap TypeBinderRef
  -> (Int, NodeId)
  -> Maybe (Int, TypeBinderRef)
certifiedFromBaseAliasRouteForTest =
  certifiedFromBaseAliasRoute

mergeReifySubstRefsForTest
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
mergeReifySubstRefsForTest =
  mergeReifySubstRefs
