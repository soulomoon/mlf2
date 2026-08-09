module MLF.Elab.Run.Generalize.Prepare (
    PreparedGeneralizationArtifact,
    PreparedRootGeneralization(..),
    preparedRootCertifiedTermBinderRenames,
    PreparedRootClosure(..),
    preparedRootClosureScheme,
    PreparedRootConstructionScope,
    preparedRootConstructionScopeBinders,
    preparedRootConstructionScopeAliases,
    preparedRootConstructionScopeBinderRenames,
    preparedRootConstructionScopeLocalGammaClosures,
    prepareGeneralizationArtifact,
    prepareGeneralizationArtifactForRoots,
    withPreparedResolvedTermSchemes,
    preparedAnnotated,
    authorizePreparedAnn,
    selectPreparedRootScopeAuthority,
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
    , preparedRootCertifiedTermBinderRenames
    , PreparedRootClosure(..)
    , preparedRootClosureScheme
    , PreparedRootConstructionScope
    , applyPreparedRootSourceTypeBinderIdentities
    , applyPreparedRootBinderIdentities
    , applyPreparedCompilerExactRootBinderIdentities
    , applyPreparedTermSourceBinderAliases
    , preparedCompilerExactSourceResultBinderRoutes
    , authorizePreparedAnn
    , selectPreparedRootScopeAuthority
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
    , preparedRootConstructionScopeBinderRenames
    , preparedRootConstructionScopeBinders
    , preparedRootConstructionScopeLocalGammaClosures
    , preparedResultTypeViewReady
    , preparedIdentityGenerator
    , completePreparedCompilerExactSubtermResults
    , preparedCompilerExactExpectedType
    , stripPreparedWitnesslessAuthoritativeAnn
    )
