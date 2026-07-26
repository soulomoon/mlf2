module MLF.Elab.Run.Generalize.Prepare (
    PreparedGeneralizationArtifact,
    PreparedRootGeneralization(..),
    PreparedRootClosure(..),
    preparedRootClosureScheme,
    PreparedRootConstructionScope,
    preparedRootConstructionScopeBinders,
    preparedRootConstructionScopeAliases,
    preparedRootConstructionScopeLocalGammaClosures,
    prepareGeneralizationArtifact,
    prepareGeneralizationArtifactForRoots,
    withPreparedResolvedTermSchemes,
    preparedAnnotated,
    canonicalizePreparedAnn,
    preparedReadContextReady,
    preparedResultTypeViewReady,
    preparedIdentityGenerator,
    applyPreparedTermSourceBinderAliases,
    preparedCompilerExactSourceResultBinderRoutes,
    completePreparedCompilerExactSubtermResults,
    preparedCompilerExactExpectedType,
    preparedElaborationConfig,
    preparedElaborationEnv,
    preparedElaborationEnvWithInitialEnv,
    stripPreparedWitnesslessAuthoritativeAnn,
    generalizePreparedRoot,
    generalizePreparedRootDetailed,
    generalizePreparedRootDetailedWithConstructionAnn,
    generalizePreparedRootDetailedWithConstructionResult,
    prepareOrdinaryRootConstructionScope,
    applyPreparedRootSourceTypeBinderIdentities,
    applyPreparedRootBinderIdentities,
    applyPreparedCompilerExactRootBinderIdentities,
    computePreparedResultType,
    computePreparedResultTypeWithRootGeneralization,
) where

import MLF.Elab.Run.Generalize.Prepare.Internal
    ( PreparedGeneralizationArtifact
    , PreparedRootGeneralization(..)
    , PreparedRootClosure(..)
    , preparedRootClosureScheme
    , PreparedRootConstructionScope
    , applyPreparedRootSourceTypeBinderIdentities
    , applyPreparedRootBinderIdentities
    , applyPreparedCompilerExactRootBinderIdentities
    , applyPreparedTermSourceBinderAliases
    , preparedCompilerExactSourceResultBinderRoutes
    , canonicalizePreparedAnn
    , computePreparedResultType
    , computePreparedResultTypeWithRootGeneralization
    , generalizePreparedRoot
    , generalizePreparedRootDetailed
    , generalizePreparedRootDetailedWithConstructionAnn
    , generalizePreparedRootDetailedWithConstructionResult
    , prepareOrdinaryRootConstructionScope
    , prepareGeneralizationArtifact
    , prepareGeneralizationArtifactForRoots
    , withPreparedResolvedTermSchemes
    , preparedAnnotated
    , preparedElaborationConfig
    , preparedElaborationEnv
    , preparedElaborationEnvWithInitialEnv
    , preparedReadContextReady
    , preparedRootConstructionScopeAliases
    , preparedRootConstructionScopeBinders
    , preparedRootConstructionScopeLocalGammaClosures
    , preparedResultTypeViewReady
    , preparedIdentityGenerator
    , completePreparedCompilerExactSubtermResults
    , preparedCompilerExactExpectedType
    , stripPreparedWitnesslessAuthoritativeAnn
    )
