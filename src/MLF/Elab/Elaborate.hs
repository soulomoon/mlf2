{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Elab.Elaborate
  ( ElabConfig (ElabConfig, ecTraceConfig, ecGeneralizeAtWith),
    ElabEnv (..),
    expansionToInst,
    elaborateWithEnv,
  )
where

import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import MLF.Constraint.Presolution (EdgeTrace, PresolutionView)
import MLF.Constraint.Presolution.Base (EdgeArtifacts (..))
import MLF.Constraint.Types.Graph (NodeRef)
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Constraint.Types.Witness (EdgeWitness, Expansion)
import MLF.Elab.Elaborate.Algebra
  ( AlgebraContext (..),
    ElabOut (..),
    elabAlg,
    mkEnvBinding,
    resolvedLambdaParamNode,
  )
import MLF.Elab.Elaborate.Annotation (AnnotationContext (..))
import MLF.Elab.Elaborate.Scope (GeneralizeAtWith, ScopeContext (..))
import MLF.Elab.Generalize (GaBindParents)
import MLF.Elab.Legacy (expansionToInst)
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Types (ElabError, ElabTerm, SchemeInfo)
import MLF.Frontend.ConstraintGen.Types (AnnExpr)
import MLF.Frontend.Syntax (NormSrcType, VarName)
import MLF.Reify.Core (namedNodes)
import MLF.Util.Trace (TraceConfig)

data ElabConfig = ElabConfig
  { ecTraceConfig :: TraceConfig,
    ecGeneralizeAtWith :: GeneralizeAtWith
  }

data ElabEnv = ElabEnv
  { eePresolutionView :: PresolutionView 'Raw,
    eeGaParents :: GaBindParents 'Raw,
    eeEdgeArtifacts :: EdgeArtifacts,
    eeScopeOverrides :: IntMap.IntMap NodeRef,
    eeAnnSourceTypes :: IntMap.IntMap NormSrcType,
    eeInitialTermEnv :: Map.Map VarName SchemeInfo
  }

eeEdgeWitnesses :: ElabEnv -> IntMap.IntMap EdgeWitness
eeEdgeWitnesses = eaEdgeWitnesses . eeEdgeArtifacts

eeEdgeTraces :: ElabEnv -> IntMap.IntMap EdgeTrace
eeEdgeTraces = eaEdgeTraces . eeEdgeArtifacts

eeEdgeExpansions :: ElabEnv -> IntMap.IntMap Expansion
eeEdgeExpansions = eaEdgeExpansions . eeEdgeArtifacts

elaborateWithEnv ::
  ElabConfig ->
  ElabEnv ->
  AnnExpr ->
  Either ElabError ElabTerm
elaborateWithEnv config elabEnv ann = do
  namedSet <- namedNodes presolutionView
  let scopeContext =
        ScopeContext
          { scPresolutionView = presolutionView,
            scGaParents = eeGaParents elabEnv,
            scScopeOverrides = eeScopeOverrides elabEnv,
            scGeneralizeAtWith = ecGeneralizeAtWith config
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
            algResolvedLambdaParamNode = resolvedLambdaParamNode canonical chiLookupNode,
            algAnnotationContext = annotationContext,
            algNamedSetReify = namedSet,
            algAnnSourceTypes = eeAnnSourceTypes elabEnv
          }
      ElabOut {elabTerm = runElab} = para (elabAlg algebraContext) ann
  runElab (Map.map (`mkEnvBinding` False) (eeInitialTermEnv elabEnv))
  where
    presolutionView = eePresolutionView elabEnv
    canonical = ChiQuery.chiCanonical presolutionView
    chiLookupNode = ChiQuery.chiLookupNode presolutionView
