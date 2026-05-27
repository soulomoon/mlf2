{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module MLF.Elab.Elaborate
  ( ElabConfig (ElabConfig, ecTraceConfig, ecGeneralizeAtWith),
    ElabEnv (..),
    elaborateWithEnv,
    elaborateWithEnvReadModel,
  )
where

import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import MLF.Constraint.Presolution (EdgeTrace, PresolutionView (..))
import MLF.Constraint.Presolution.Base (EdgeArtifacts (..))
import MLF.Constraint.Types.Graph (NodeRef)
import MLF.Constraint.Types.Phase (Phase)
import MLF.Constraint.Types.Witness (EdgeWitness, Expansion)
import MLF.Elab.Elaborate.Algebra
  ( AlgebraContext (..),
    ElabOut (..),
    elabAlg,
    mkEnv,
    resolvedLambdaParamNode,
  )
import MLF.Elab.Elaborate.Annotation (AnnotationContext (..))
import MLF.Elab.Elaborate.Scope (GeneralizeAtWith, ScopeContext (..))
import MLF.Elab.Generalize (GaBindParents)
import MLF.Elab.ReadModel (ElabReadModel (..))
import MLF.Elab.Run.TypeOps (mkInlineBoundVarsContextWithReadModel)
import MLF.Elab.Types (ElabError, ElabTerm, SchemeInfo)
import MLF.Frontend.ConstraintGen.Types (AnnExpr)
import MLF.Frontend.Syntax (NormSrcType, VarName)
import MLF.Util.Trace (TraceConfig)

data ElabConfig (p :: Phase) = ElabConfig
  { ecTraceConfig :: TraceConfig,
    ecGeneralizeAtWith :: GeneralizeAtWith p
  }

data ElabEnv (p :: Phase) = ElabEnv
  { eePresolutionView :: PresolutionView p,
    eeReadModel :: Either ElabError (ElabReadModel p),
    eeGaParents :: GaBindParents p,
    eeEdgeArtifacts :: EdgeArtifacts,
    eeScopeOverrides :: IntMap.IntMap NodeRef,
    eeAnnSourceTypes :: IntMap.IntMap NormSrcType,
    eeInitialTermEnv :: Map.Map VarName SchemeInfo
  }

eeEdgeWitnesses :: ElabEnv p -> IntMap.IntMap EdgeWitness
eeEdgeWitnesses = eaEdgeWitnesses . eeEdgeArtifacts

eeEdgeTraces :: ElabEnv p -> IntMap.IntMap EdgeTrace
eeEdgeTraces = eaEdgeTraces . eeEdgeArtifacts

eeEdgeExpansions :: ElabEnv p -> IntMap.IntMap Expansion
eeEdgeExpansions = eaEdgeExpansions . eeEdgeArtifacts

elaborateWithEnv ::
  ElabConfig p ->
  ElabEnv p ->
  AnnExpr ->
  Either ElabError ElabTerm
elaborateWithEnv config elabEnv ann = do
  readModel <- eeReadModel elabEnv
  elaborateWithEnvReadModel config elabEnv readModel ann

elaborateWithEnvReadModel ::
  ElabConfig p ->
  ElabEnv p ->
  ElabReadModel p ->
  AnnExpr ->
  Either ElabError ElabTerm
elaborateWithEnvReadModel config elabEnv readModel ann = do
  let namedSet = ermNamedNodes readModel
      inlineBoundVarsContext = mkInlineBoundVarsContextWithReadModel readModel
  let scopeContext =
        ScopeContext
          { scPresolutionView = presolutionView,
            scGaParents = eeGaParents elabEnv,
            scScopeOverrides = eeScopeOverrides elabEnv,
            scGeneralizeAtWith = ecGeneralizeAtWith config,
            scReadModel = readModel,
            scNamedSetReify = namedSet,
            scInlineBoundVarsContext = inlineBoundVarsContext
          }
      annotationContext =
        AnnotationContext
          { acTraceConfig = ecTraceConfig config,
            acScopeContext = scopeContext,
            acAnnSourceTypes = eeAnnSourceTypes elabEnv,
            acEdgeWitnesses = eeEdgeWitnesses elabEnv,
            acEdgeTraces = eeEdgeTraces elabEnv,
            acEdgeExpansions = eeEdgeExpansions elabEnv
          }
      algebraContext =
        AlgebraContext
          { algPresolutionView = presolutionView,
            algTraceConfig = ecTraceConfig config,
            algCanonical = canonical,
            algResolvedLambdaParamNode = resolvedLambdaParamNode canonical lookupNode,
            algAnnotationContext = annotationContext,
            algNamedSetReify = namedSet,
            algInlineBoundVarsContext = inlineBoundVarsContext,
            algAnnSourceTypes = eeAnnSourceTypes elabEnv
          }
      ElabOut {elabTerm = runElab} = para (elabAlg algebraContext) ann
  runElab (mkEnv (eeInitialTermEnv elabEnv))
  where
    presolutionView = ermPresolutionView readModel
    canonical = pvCanonical presolutionView
    lookupNode = pvLookupNode presolutionView
