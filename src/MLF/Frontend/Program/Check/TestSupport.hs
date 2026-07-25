module MLF.Frontend.Program.Check.TestSupport
    ( BuiltinPreludeCheckCacheHandle
    , newBuiltinPreludeCheckCache
    , readBuiltinPreludeCheckBuildCount
    , BuiltinPreludeCheckProbeHandle
    , newBuiltinPreludeCheckProbeCache
    , newBuiltinPreludeCheckProbeCacheWithBuildAction
    , readBuiltinPreludeCheckProbeBuildCount
    , cacheBuiltinPreludeCheckProbe
    , nextClientIdentityAfterCachedBuiltinPrelude
    , checkLocatedProgramPackageWithCache
    , checkLocatedProgramPackageWithDefaultTiming
    , checkLocatedProgramPackageWithDefaultTimingAndCache
    )
where

import MLF.Frontend.Program.Check.Internal
    ( checkLocatedProgramPackageWithBuiltinPreludeCheckCacheForTest
    , checkLocatedProgramPackageWithTiming
    , checkLocatedProgramPackageWithTimingAndBuiltinPreludeCheckCacheForTest
    , newBuiltinPreludeCheckCacheForTest
    , nextClientIdentityAfterCachedBuiltinPreludeForTest
    )
import MLF.Frontend.Program.Check.Cache
    ( BuiltinPreludeCheckCacheHandle
    , readBuiltinPreludeCheckBuildCount
    )
import qualified MLF.Frontend.Program.Check.Cache as PreludeCache
import MLF.Frontend.Program.Checked (CheckedProgram)
import MLF.Frontend.Program.Package (LocatedProgramPackage)
import MLF.Frontend.Program.Types
    ( ProgramDiagnostic
    , ProgramError (ProgramPipelineError)
    , ResolvedSemanticModule
    )
import MLF.Util.Timing (defaultTimingConfig)
import MLF.Types.Identity (UniqueIdentity)

newBuiltinPreludeCheckCache :: IO BuiltinPreludeCheckCacheHandle
newBuiltinPreludeCheckCache =
    newBuiltinPreludeCheckCacheForTest

nextClientIdentityAfterCachedBuiltinPrelude ::
    BuiltinPreludeCheckCacheHandle ->
    UniqueIdentity ->
    ResolvedSemanticModule ->
    Either ProgramError UniqueIdentity
nextClientIdentityAfterCachedBuiltinPrelude =
    nextClientIdentityAfterCachedBuiltinPreludeForTest

checkLocatedProgramPackageWithCache ::
    BuiltinPreludeCheckCacheHandle ->
    LocatedProgramPackage ->
    Either ProgramDiagnostic CheckedProgram
checkLocatedProgramPackageWithCache =
    checkLocatedProgramPackageWithBuiltinPreludeCheckCacheForTest

-- A distinct cheap cache-owner probe for cache-mechanism tests. Its handle
-- cannot be supplied to the real checker, so test code cannot accidentally
-- populate a semantic Prelude cache with a manufactured result.
newtype BuiltinPreludeCheckProbeHandle =
    BuiltinPreludeCheckProbeHandle
        PreludeCache.BuiltinPreludeCheckCacheHandle

newBuiltinPreludeCheckProbeCache ::
    IO BuiltinPreludeCheckProbeHandle
newBuiltinPreludeCheckProbeCache =
    newBuiltinPreludeCheckProbeCacheWithBuildAction (pure ())

newBuiltinPreludeCheckProbeCacheWithBuildAction ::
    IO () ->
    IO BuiltinPreludeCheckProbeHandle
newBuiltinPreludeCheckProbeCacheWithBuildAction buildAction =
    BuiltinPreludeCheckProbeHandle
        <$> PreludeCache.newBuiltinPreludeCheckCache
            ( \_ -> do
                buildAction
                pure (Left (ProgramPipelineError "builtin Prelude cache probe"))
            )

readBuiltinPreludeCheckProbeBuildCount ::
    BuiltinPreludeCheckProbeHandle ->
    IO Int
readBuiltinPreludeCheckProbeBuildCount (BuiltinPreludeCheckProbeHandle cacheHandle) =
    PreludeCache.readBuiltinPreludeCheckBuildCount cacheHandle

cacheBuiltinPreludeCheckProbe ::
    BuiltinPreludeCheckProbeHandle ->
    ResolvedSemanticModule ->
    IO ()
cacheBuiltinPreludeCheckProbe (BuiltinPreludeCheckProbeHandle cacheHandle) resolvedModule = do
    _ <-
        PreludeCache.cachedBuiltinPreludeCheck
            cacheHandle
            resolvedModule
    pure ()

checkLocatedProgramPackageWithDefaultTiming ::
    LocatedProgramPackage ->
    IO (Either ProgramDiagnostic CheckedProgram)
checkLocatedProgramPackageWithDefaultTiming =
    checkLocatedProgramPackageWithTiming defaultTimingConfig

checkLocatedProgramPackageWithDefaultTimingAndCache ::
    BuiltinPreludeCheckCacheHandle ->
    LocatedProgramPackage ->
    IO (Either ProgramDiagnostic CheckedProgram)
checkLocatedProgramPackageWithDefaultTimingAndCache =
    checkLocatedProgramPackageWithTimingAndBuiltinPreludeCheckCacheForTest defaultTimingConfig
