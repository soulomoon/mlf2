{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module MLF.Elab.Elaborate
  ( ElabConfig (ElabConfig, ecTraceConfig, ecGeneralizeAtWith, ecGeneralizeAtWithRequirements, ecGeneralizeAtWithResultCertificate),
    ElabEnv (..),
    elaborateWithEnv,
    elaborateWithEnvDetailed,
    elaborateWithEnvReadModel,
  )
where

import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import MLF.Constraint.Presolution (PresolutionView (..))
import MLF.Constraint.Types.Graph (NodeId)
import MLF.Constraint.Types.Phase (Phase)
import MLF.Elab.Elaborate.Algebra
  ( AlgebraContext (..),
    ElaboratedTerm (..),
    Env,
    ElabOut (..),
    elabAlg,
    resolvedLambdaParamNode,
  )
import MLF.Elab.Elaborate.Annotation
  ( AnnotationContext (..),
    AuthorizedElaborationRoot,
    authorizedElaborationConstructionAnn,
    authorizedElaborationEdgeAuthority,
  )
import MLF.Elab.Elaborate.Scope
  ( GeneralizeAtWith,
    GeneralizeAtWithRequirements,
    GeneralizeAtWithResultCertificate,
    ScopeContext (..),
  )
import MLF.Elab.Generalize (GaBindParents, SubtermGeneralizations)
import MLF.Elab.ReadModel (ElabReadModel (..))
import MLF.Elab.Run.Scope (ConstructionScopes)
import MLF.Elab.Run.TypeOps (mkInlineBoundVarsContextWithReadModelCanonical)
import MLF.Elab.Types (ElabError, ElabType, TypeBinderRef, XmlfTerm)
import MLF.Frontend.Symbol (SymbolIdentity)
import MLF.Frontend.Syntax (NormSrcType)
import MLF.Types.Identity (TypeBinderIdentity)
import MLF.Util.Trace (TraceConfig)

data ElabConfig (p :: Phase) = ElabConfig
  { ecTraceConfig :: TraceConfig,
    ecGeneralizeAtWith :: GeneralizeAtWith p,
    ecGeneralizeAtWithRequirements :: GeneralizeAtWithRequirements p,
    ecGeneralizeAtWithResultCertificate ::
      GeneralizeAtWithResultCertificate p
  }

data ElabEnv (p :: Phase) = ElabEnv
  { eePresolutionView :: PresolutionView p,
    eeCanonical :: NodeId -> NodeId,
    eeReadModel :: Either ElabError (ElabReadModel p),
    eeGaParents :: GaBindParents p,
    eeExactProducerTypes :: Either ElabError (IntMap.IntMap ElabType),
    eeCompilerExactConstructionRefs :: Either ElabError (IntMap.IntMap (IntMap.IntMap TypeBinderRef)),
    eeCompilerExactDeclarationRefs :: Either ElabError (IntMap.IntMap (IntMap.IntMap TypeBinderRef)),
    eeScopeOverrides :: ConstructionScopes,
    -- Exact lambdas have no annotation edge.  Their parameter source type is
    -- therefore the only remaining node-keyed annotation authority.
    eeExactLambdaParamSourceTypes :: IntMap.IntMap NormSrcType,
    eeSourceTypeHeadIdentities :: Map.Map String SymbolIdentity,
    eeSourceTypeBinderIdentities :: Map.Map String TypeBinderIdentity,
    eeSourceBinderRefs :: IntMap.IntMap TypeBinderRef,
    -- Only exact declaration keys carry source-binder ownership.  The
    -- expanded source carrier above also contains solved/copy routes used for
    -- reification, which must not make a fresh local Gamma consumer ambient.
    eeDirectSourceBinderKeys :: IntSet.IntSet,
    eeSubtermGeneralizations :: Either ElabError SubtermGeneralizations,
    eeInitialTermEnv :: Env
  }

elaborateWithEnv ::
  ElabConfig p ->
  ElabEnv p ->
  AuthorizedElaborationRoot ->
  Either ElabError XmlfTerm
elaborateWithEnv config elabEnv root =
  elaboratedTerm <$> elaborateWithEnvDetailed config elabEnv root

elaborateWithEnvDetailed ::
  ElabConfig p ->
  ElabEnv p ->
  AuthorizedElaborationRoot ->
  Either ElabError ElaboratedTerm
elaborateWithEnvDetailed config elabEnv root = do
  readModel <- eeReadModel elabEnv
  elaborateWithEnvReadModelDetailed config elabEnv readModel root

elaborateWithEnvReadModel ::
  ElabConfig p ->
  ElabEnv p ->
  ElabReadModel p ->
  AuthorizedElaborationRoot ->
  Either ElabError XmlfTerm
elaborateWithEnvReadModel config elabEnv readModel root =
  elaboratedTerm
    <$> elaborateWithEnvReadModelDetailed config elabEnv readModel root

elaborateWithEnvReadModelDetailed ::
  ElabConfig p ->
  ElabEnv p ->
  ElabReadModel p ->
  AuthorizedElaborationRoot ->
  Either ElabError ElaboratedTerm
elaborateWithEnvReadModelDetailed config elabEnv readModel root = do
  subtermGeneralizations <- eeSubtermGeneralizations elabEnv
  exactProducerTypes <- eeExactProducerTypes elabEnv
  compilerExactConstructionRefs <- eeCompilerExactConstructionRefs elabEnv
  compilerExactDeclarationRefs <- eeCompilerExactDeclarationRefs elabEnv
  let namedSet = ermNamedNodes readModel
      inlineBoundVarsContext =
        mkInlineBoundVarsContextWithReadModelCanonical canonical readModel
  let scopeContext =
        ScopeContext
          { scPresolutionView = presolutionView,
            scCanonical = canonical,
            scGaParents = eeGaParents elabEnv,
            scScopeOverrides = eeScopeOverrides elabEnv,
            scGeneralizeAtWith = ecGeneralizeAtWith config,
            scGeneralizeAtWithRequirements = ecGeneralizeAtWithRequirements config,
            scGeneralizeAtWithResultCertificate =
              ecGeneralizeAtWithResultCertificate config,
            scReadModel = readModel,
            scNamedSetReify = namedSet,
            scInlineBoundVarsContext = inlineBoundVarsContext
          }
      annotationContext =
        AnnotationContext
          { acTraceConfig = ecTraceConfig config,
            acScopeContext = scopeContext,
            acElaborationEdgeAuthority =
              authorizedElaborationEdgeAuthority root,
            acSourceTypeHeadIdentities = eeSourceTypeHeadIdentities elabEnv,
            acSourceTypeBinderIdentities = eeSourceTypeBinderIdentities elabEnv,
            acSourceBinderRefs = eeSourceBinderRefs elabEnv,
            acDirectSourceBinderKeys = eeDirectSourceBinderKeys elabEnv,
            acSubtermGeneralizations = subtermGeneralizations
          }
      algebraContext =
        AlgebraContext
          { algPresolutionView = presolutionView,
            algTraceConfig = ecTraceConfig config,
            algCanonical = canonical,
            algResolvedLambdaParamNode = resolvedLambdaParamNode canonical lookupNode,
            algAnnotationContext = annotationContext,
            algNamedSetReify = namedSet,
            algExactLambdaParamSourceTypes = eeExactLambdaParamSourceTypes elabEnv,
            algSourceTypeHeadIdentities = eeSourceTypeHeadIdentities elabEnv,
            algSourceTypeBinderIdentities = eeSourceTypeBinderIdentities elabEnv,
            algSubtermGeneralizations = subtermGeneralizations,
            algExactProducerTypes = exactProducerTypes,
            algCompilerExactConstructionRefs = compilerExactConstructionRefs,
            algCompilerExactDeclarationRefs = compilerExactDeclarationRefs
          }
      ElabOut {elabDetailed = runElab} =
        para
          (elabAlg algebraContext)
          (authorizedElaborationConstructionAnn root)
  runElab (eeInitialTermEnv elabEnv)
  where
    presolutionView = ermPresolutionView readModel
    canonical = eeCanonical elabEnv
    lookupNode = pvLookupNode presolutionView
