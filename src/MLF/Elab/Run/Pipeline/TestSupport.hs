module MLF.Elab.Run.Pipeline.TestSupport
  ( PipelineElabDetailedResult (..),
    PreparedExternalBindings,
    prepareExternalBindings,
    prepareExternalBindingsWithTypeIdentities,
    preparedExternalTypeCheckEnv,
    preparedSourceTypeIdentityMaps,
    extendPreparedExternalBindingTypeIdentities,
    extendPreparedExternalBindingTypeIdentityCandidates,
    preferPreparedExternalBindingTypeIdentities,
    restrictPreparedExternalBindings,
    runPipelineElabDetailedWithExternalBindings,
    runPipelineElabDetailedUncheckedWithExternalBindings,
    runPipelineElabDetailedWithPreparedExternalBindings,
    runPipelineElabDetailedModuleKeyedForTest,
    unionPreparedExternalBindings,
    closePipelineTerm,
    freshenTypeAbsAgainstEnvFromSupply,
    constructLexicalForallCopyInstantiation,
  )
where

import qualified Data.Map.Strict as Map
import MLF.Constraint.Types.Graph (PolySyms)
import MLF.Elab.PipelineError (PipelineError)
import MLF.Elab.Run.Instantiation
  ( constructLexicalForallCopyInstantiation,
  )
import MLF.Elab.Run.Pipeline
  ( PipelineElabDetailedResult (..),
    PreparedExternalBindings,
    prepareExternalBindings,
    prepareExternalBindingsWithTypeIdentities,
    preparedExternalTypeCheckEnv,
    preparedSourceTypeIdentityMaps,
    extendPreparedExternalBindingTypeIdentities,
    extendPreparedExternalBindingTypeIdentityCandidates,
    preferPreparedExternalBindingTypeIdentities,
    restrictPreparedExternalBindings,
    runPipelineElabDetailedWithExternalBindings,
    runPipelineElabDetailedUncheckedWithExternalBindings,
    runPipelineElabDetailedWithPreparedExternalBindings,
    runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming,
    unionPreparedExternalBindings,
    closePipelineTerm,
    freshenTypeAbsAgainstEnvFromSupply,
  )
import MLF.Frontend.Syntax (NormSurfaceExpr, VarName)
import MLF.Util.Timing (defaultTimingConfig)

runPipelineElabDetailedModuleKeyedForTest
  :: (Ord key)
  => PolySyms
  -> PreparedExternalBindings
  -> Map.Map key PreparedExternalBindings
  -> [(key, VarName, NormSurfaceExpr)]
  -> IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedModuleKeyedForTest =
  runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming
    defaultTimingConfig
    "test.module_pipeline"
