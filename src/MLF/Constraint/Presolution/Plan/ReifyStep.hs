-- |
-- Module      : MLF.Constraint.Presolution.Plan.ReifyStep
-- Description : Build the reification plan for a generalized scope
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- Builds a 'ReifyPlan' that determines how to reify the type structure
-- for a generalized scope, including extra binder candidates and adjusted
-- type roots.
module MLF.Constraint.Presolution.Plan.ReifyStep
  ( ReifyPlan (..),
    planReify,
  )
where

{- Note [Reification step planning — planReify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'planReify' constructs a 'ReifyPlan' that determines how to reify the type
structure for a generalized scope.  Given a 'GeneralizePlan' (from
'planGeneralizeAt'), it computes:

  * 'rpSubst' — the binder name substitution map (NodeId → String)
  * 'rpExtraBinders' — additional binder candidates beyond the plan's flex
    children (used for alias binders that must appear in the reified type)
  * 'rpTypeRoot' — the adjusted type root for reification (may differ from
    the plan root when alias/wrapper nodes are present)

This module is separate from 'Plan.Generalize' because reification planning
depends on the completed generalization plan, including finalized binder
names and scheme structure, while generalization planning is independent of
reification details.
-}

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Presolution.Plan.BinderPlan
  ( BinderPlan (..),
    isQuantifiable,
  )
import MLF.Constraint.Presolution.Plan.Context
  ( GeneralizeCtx (..),
    GeneralizeEnv (..),
  )
import MLF.Constraint.Presolution.Plan.Env
  ( PresolutionEnv (..),
    lookupNodeInMap,
  )
import MLF.Constraint.Presolution.Plan.Generalize (GeneralizePlan (..))
import qualified MLF.Constraint.Presolution.Plan.ReifyPlan as Reify
import MLF.Constraint.Presolution.Plan.Target
  ( GammaPlan (..),
    TargetPlan (..),
    TypeRootPlan (..),
  )
import MLF.Constraint.Types.Graph hiding (lookupNode)
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Util.ElabError (ElabError (..))
import qualified MLF.Util.IntMapUtils as IntMapUtils

data ReifyPlan = ReifyPlan
  { rpPlan :: Reify.ReifyPlan,
    rpTypeRootForReifyAdjusted :: NodeId,
    rpSubstForReifyAdjusted :: IntMap.IntMap String
  }

planReify :: PresolutionEnv -> GeneralizePlan -> Either ElabError ReifyPlan
planReify _ plan = do
  let GeneralizePlan
        { gpEnv = env,
          gpContext = ctx,
          gpBinderPlan = binderPlan,
          gpGammaPlan = gammaPlan,
          gpTypeRootPlan = typeRootPlan,
          gpBindParents = bindParents,
          gpTargetPlan = targetPlan,
          gpSchemeRootsPlan = schemeRootsPlan,
          gpReachableFromWithBounds = reachableFromWithBounds
        } = plan
      GeneralizeEnv
        { geConstraint = constraint,
          geNodes = nodes,
          geCanonical = canonical,
          geIsTyVarKey = isTyVarKey
        } = env
      GeneralizeCtx
        { gcScopeRootC = scopeRootC,
          gcScopeGen = scopeGen,
          gcTarget0 = target0,
          gcBindParentsGaInfo = mbBindParentsGaInfo
        } = ctx
      TargetPlan
        { tpTargetBound = targetBound
        } = targetPlan
      TypeRootPlan
        { trTypeRoot = typeRoot,
          trTargetIsBaseLike = targetIsBaseLike
        } = typeRootPlan
      GammaPlan
        { gpSolvedToBasePref = solvedToBasePrefPlan
        } = gammaPlan
      BinderPlan
        { bpBinderNames = binderNames,
          bpSubst0 = subst0',
          bpGammaAlias = gammaAliasPlan,
          bpNestedSchemeInteriorSet = nestedSchemeInteriorSetPlan,
          bpBaseGammaRep = baseGammaRepPlan,
          bpAliasBinderBases = aliasBinderBasesPlan,
          bpOrderBinders = orderBinders
        } = binderPlan
      isQuantifiable' = isQuantifiable canonical constraint isTyVarKey
      extraCandidates =
        case scopeRootC of
          GenRef _ -> []
          TypeRef _ ->
            case lookupNodeInMap nodes (canonical typeRoot) of
              Just TyForall {} ->
                [ canonical child
                | child <- IntMapUtils.typeChildrenOf bindParents (typeRef (canonical typeRoot)),
                  isQuantifiable' child,
                  not (IntMap.member (getNodeId (canonical child)) subst0')
                ]
              Just TyMu {} ->
                [ canonical child
                | child <- IntMapUtils.typeChildrenOf bindParents (typeRef (canonical typeRoot)),
                  isQuantifiable' child,
                  not (IntMap.member (getNodeId (canonical child)) subst0')
                ]
              _ -> []
  orderedExtra <- orderBinders (map getNodeId extraCandidates)
  let reifyPlan =
        Reify.buildReifyPlan
          Reify.ReifyPlanInput
            { Reify.rpiConstraint = constraint,
              Reify.rpiNodes = nodes,
              Reify.rpiCanonical = canonical,
              Reify.rpiScopeRootC = scopeRootC,
              Reify.rpiScopeGen = scopeGen,
              Reify.rpiSchemeRootsPlan = schemeRootsPlan,
              Reify.rpiTarget0 = target0,
              Reify.rpiTargetIsBaseLike = targetIsBaseLike,
              Reify.rpiTargetBound = targetBound,
              Reify.rpiReachableFromWithBounds = reachableFromWithBounds,
              Reify.rpiBindParentsGa = mbBindParentsGaInfo,
              Reify.rpiExtraNameStart = length binderNames,
              Reify.rpiOrderedExtra = orderedExtra,
              Reify.rpiSubst0 = subst0',
              Reify.rpiGammaAlias = gammaAliasPlan,
              Reify.rpiNestedSchemeInteriorSet = nestedSchemeInteriorSetPlan,
              Reify.rpiBaseGammaRep = baseGammaRepPlan,
              Reify.rpiAliasBinderBases = aliasBinderBasesPlan,
              Reify.rpiSolvedToBasePref = solvedToBasePrefPlan,
              Reify.rpiTypeRoot = typeRoot
            }
  let Reify.ReifyPlan
        { Reify.rpSubst = subst,
          Reify.rpTypeRootForReify = typeRootForReify,
          Reify.rpSubstForReify = substForReify
        } = reifyPlan
      typeRootForReifyAdjustedPair =
        case lookupNodeInMap nodes (canonical typeRootForReify) of
          Just TyVar {} ->
            case VarStore.lookupVarBound constraint (canonical typeRootForReify) of
              Just bnd
                | canonical bnd == canonical typeRoot ->
                    (typeRoot, subst)
              _ -> (typeRootForReify, substForReify)
          _ -> (typeRootForReify, substForReify)
      (typeRootForReifyAdjusted, substForReifyAdjusted) =
        typeRootForReifyAdjustedPair
  pure
    ReifyPlan
      { rpPlan = reifyPlan,
        rpTypeRootForReifyAdjusted = typeRootForReifyAdjusted,
        rpSubstForReifyAdjusted = substForReifyAdjusted
      }
