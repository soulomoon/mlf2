module MLF.Elab.Run.Generalize.Prepare (
    PreparedGeneralizationArtifact,
    prepareGeneralizationArtifact,
    preparedAnnotated,
    preparedElaborationConfig,
    preparedElaborationEnv,
    stripPreparedWitnesslessAuthoritativeAnn,
    generalizePreparedRoot,
    computePreparedResultType,
) where

import MLF.Elab.Run.Generalize.Prepare.Internal
    ( PreparedGeneralizationArtifact
    , computePreparedResultType
    , generalizePreparedRoot
    , prepareGeneralizationArtifact
    , preparedAnnotated
    , preparedElaborationConfig
    , preparedElaborationEnv
    , stripPreparedWitnesslessAuthoritativeAnn
    )
