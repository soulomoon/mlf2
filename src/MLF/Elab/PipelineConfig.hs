module MLF.Elab.PipelineConfig (
    PipelineConfig(..),
    defaultPipelineConfig
) where

import MLF.Util.Trace (TraceConfig, defaultTraceConfig)

-- | Configuration for pipeline execution.
data PipelineConfig = PipelineConfig
    { pcTraceConfig :: TraceConfig
    } deriving (Eq, Show)

-- | Default configuration with tracing disabled.
defaultPipelineConfig :: PipelineConfig
defaultPipelineConfig = PipelineConfig
    { pcTraceConfig = defaultTraceConfig
    }
