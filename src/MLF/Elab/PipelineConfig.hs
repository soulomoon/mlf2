module MLF.Elab.PipelineConfig (
    PipelineConfig(..),
    defaultPipelineConfig
) where

import MLF.Util.Trace (TraceConfig, defaultTraceConfig)

-- | Configuration for pipeline execution.
data PipelineConfig = PipelineConfig
    { pcTraceConfig :: TraceConfig
    -- | Run result-type reconstruction after the authoritative final typecheck.
    -- This is a diagnostic cross-check and is off by default on hot paths.
    , pcResultTypeDiagnostics :: Bool
    } deriving (Eq, Show)

-- | Default configuration with tracing and diagnostic result reconstruction disabled.
defaultPipelineConfig :: PipelineConfig
defaultPipelineConfig = PipelineConfig
    { pcTraceConfig = defaultTraceConfig
    , pcResultTypeDiagnostics = False
    }
