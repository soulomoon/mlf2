module MLF.Elab.Run.Generalize.Prepare (
    PreparedGeneralizationArtifact,
    PreparedRootGeneralization(..),
    prepareGeneralizationArtifact,
    prepareGeneralizationArtifactForRoots,
    preparedAnnotated,
    canonicalizePreparedAnn,
    preparedReadContextReady,
    preparedResultTypeViewReady,
    preparedElaborationConfig,
    preparedElaborationEnv,
    stripPreparedWitnesslessAuthoritativeAnn,
    generalizePreparedRoot,
    generalizePreparedRootDetailed,
    computePreparedResultType,
    computePreparedResultTypeWithRootGeneralization,
) where

import MLF.Elab.Run.Generalize.Prepare.Internal
    ( PreparedGeneralizationArtifact
    , PreparedRootGeneralization(..)
    , canonicalizePreparedAnn
    , computePreparedResultType
    , computePreparedResultTypeWithRootGeneralization
    , generalizePreparedRoot
    , generalizePreparedRootDetailed
    , prepareGeneralizationArtifact
    , prepareGeneralizationArtifactForRoots
    , preparedAnnotated
    , preparedElaborationConfig
    , preparedElaborationEnv
    , preparedReadContextReady
    , preparedResultTypeViewReady
    , stripPreparedWitnesslessAuthoritativeAnn
    )
