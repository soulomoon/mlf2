{-# LANGUAGE BangPatterns #-}

module MLF.Frontend.Program.Check.Cache
    ( BuiltinPreludeCheckCacheHandle
    , CachedBuiltinPreludeCheck
    , newBuiltinPreludeCheckCache
    , cachedBuiltinPreludeCheck
    , cachedBuiltinPreludeCheckWithTiming
    , cachedBuiltinPreludeCheckedModule
    , advanceIdentityGeneratorPastCachedBuiltinPrelude
    , readBuiltinPreludeCheckBuildCount
    )
where

import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , withMVar
    )

import MLF.Frontend.Program.Check.Cache.Key
    ( BuiltinPreludeCacheKey
    , builtinPreludeCacheKey
    )
import MLF.Frontend.Program.Types
    ( CheckedModule (..)
    , ProgramError
    , ResolvedSemanticModule
    , checkedModuleGeneratedIdentities
    )
import MLF.Types.Identity
    ( IdentityGenerator
    , UniqueIdentity (..)
    , advanceIdentityGeneratorPast
    )
import MLF.Util.Timing
    ( TimingConfig
    , timeProgramDetailIO
    )

type BuiltinPreludeCheckResult = Either ProgramError CachedBuiltinPreludeCheck

data CachedBuiltinPreludeCheck = CachedBuiltinPreludeCheck
    { cachedBuiltinPreludeCheckedModule :: CheckedModule
    , cachedBuiltinPreludeIdentityExtrema :: !GeneratedIdentityExtrema
    }

data GeneratedIdentityExtrema
    = NoGeneratedIdentities
    | GeneratedIdentityExtrema !UniqueIdentity !UniqueIdentity

data BuiltinPreludeCheckCacheEntry = BuiltinPreludeCheckCacheEntry
    { builtinPreludeCheckCacheEntryKey :: !BuiltinPreludeCacheKey
    , builtinPreludeCheckCacheEntryResult :: !BuiltinPreludeCheckResult
    }

data BuiltinPreludeCheckCache = BuiltinPreludeCheckCache
    { builtinPreludeCheckCacheEntry :: Maybe BuiltinPreludeCheckCacheEntry
    , builtinPreludeCheckBuildCount :: Int
    }

emptyBuiltinPreludeCheckCache :: BuiltinPreludeCheckCache
emptyBuiltinPreludeCheckCache =
    BuiltinPreludeCheckCache
        { builtinPreludeCheckCacheEntry = Nothing
        , builtinPreludeCheckBuildCount = 0
        }

type BuiltinPreludeCheckBuilder =
    ResolvedSemanticModule ->
    IO (Either ProgramError CheckedModule)

data BuiltinPreludeCheckCacheHandle =
    BuiltinPreludeCheckCacheHandle
        (MVar BuiltinPreludeCheckCache)
        BuiltinPreludeCheckBuilder

-- The semantic builder is fixed when the owner creates the handle. A lookup
-- can choose only how to observe that build (ordinary or timed); call order
-- cannot substitute another semantic result for the same module key.
newBuiltinPreludeCheckCache ::
    BuiltinPreludeCheckBuilder ->
    IO BuiltinPreludeCheckCacheHandle
newBuiltinPreludeCheckCache build =
    BuiltinPreludeCheckCacheHandle
        <$> newMVar emptyBuiltinPreludeCheckCache
        <*> pure build

cachedBuiltinPreludeCheck ::
    BuiltinPreludeCheckCacheHandle ->
    ResolvedSemanticModule ->
    IO BuiltinPreludeCheckResult
cachedBuiltinPreludeCheck =
    cachedBuiltinPreludeCheckWith id

cachedBuiltinPreludeCheckWithTiming ::
    TimingConfig ->
    String ->
    BuiltinPreludeCheckCacheHandle ->
    ResolvedSemanticModule ->
    IO BuiltinPreludeCheckResult
cachedBuiltinPreludeCheckWithTiming timing label =
    cachedBuiltinPreludeCheckWith (timeProgramDetailIO timing label)

cachedBuiltinPreludeCheckWith ::
    (IO (Either ProgramError CheckedModule) -> IO (Either ProgramError CheckedModule)) ->
    BuiltinPreludeCheckCacheHandle ->
    ResolvedSemanticModule ->
    IO BuiltinPreludeCheckResult
cachedBuiltinPreludeCheckWith observeBuild (BuiltinPreludeCheckCacheHandle cacheHandle build) resolvedModule =
    let cacheKey = builtinPreludeCacheKey resolvedModule
    in modifyMVar cacheHandle $ \cache ->
        case builtinPreludeCheckCacheEntry cache of
            Just entry
                | builtinPreludeCheckCacheEntryKey entry == cacheKey ->
                    pure (cache, builtinPreludeCheckCacheEntryResult entry)
            _ -> do
                checked <- observeBuild (build resolvedModule)
                cached <-
                    case checked of
                        Left err ->
                            pure (Left err)
                        Right checkedModule -> do
                            let !artifact = mkCachedBuiltinPreludeCheck checkedModule
                            pure (Right artifact)
                pure
                    ( cache
                        { builtinPreludeCheckCacheEntry =
                            Just
                                BuiltinPreludeCheckCacheEntry
                                    { builtinPreludeCheckCacheEntryKey = cacheKey
                                    , builtinPreludeCheckCacheEntryResult = cached
                                    }
                        , builtinPreludeCheckBuildCount = builtinPreludeCheckBuildCount cache + 1
                        }
                    , cached
                    )

-- The cache is deliberately one-slot. Production can construct only one
-- provenance-bearing builtin Prelude in a process, while a test-only semantic
-- variant replaces the prior artifact instead of retaining another complete
-- checked graph indefinitely.

mkCachedBuiltinPreludeCheck :: CheckedModule -> CachedBuiltinPreludeCheck
mkCachedBuiltinPreludeCheck checkedModule =
    let !identityExtrema =
            generatedIdentityExtrema
                (checkedModuleGeneratedIdentities checkedModule)
    in CachedBuiltinPreludeCheck
        { cachedBuiltinPreludeCheckedModule = checkedModule
        , cachedBuiltinPreludeIdentityExtrema = identityExtrema
        }

generatedIdentityExtrema :: [UniqueIdentity] -> GeneratedIdentityExtrema
generatedIdentityExtrema =
    foldl' extendExtrema NoGeneratedIdentities
  where
    extendExtrema NoGeneratedIdentities identity =
        let !forcedIdentity = forceUniqueIdentity identity
        in GeneratedIdentityExtrema forcedIdentity forcedIdentity
    extendExtrema (GeneratedIdentityExtrema minimumIdentity maximumIdentity) identity =
        let !forcedIdentity = forceUniqueIdentity identity
        in GeneratedIdentityExtrema
            (min minimumIdentity forcedIdentity)
            (max maximumIdentity forcedIdentity)

    forceUniqueIdentity identity@(UniqueIdentity value) =
        value `seq` identity

-- Advancing over the extrema is equivalent to replaying the full inventory:
-- an ascending generator observes only the maximum occupied identity and a
-- descending generator observes only the minimum. Keeping both makes this
-- summary independent of the caller's supply direction.
advanceIdentityGeneratorPastCachedBuiltinPrelude ::
    IdentityGenerator ->
    CachedBuiltinPreludeCheck ->
    IdentityGenerator
advanceIdentityGeneratorPastCachedBuiltinPrelude generator cached =
    case cachedBuiltinPreludeIdentityExtrema cached of
        NoGeneratedIdentities ->
            generator
        GeneratedIdentityExtrema minimumIdentity maximumIdentity ->
            advanceIdentityGeneratorPast
                maximumIdentity
                ( advanceIdentityGeneratorPast
                    minimumIdentity
                    generator
                )

readBuiltinPreludeCheckBuildCount :: BuiltinPreludeCheckCacheHandle -> IO Int
readBuiltinPreludeCheckBuildCount (BuiltinPreludeCheckCacheHandle cacheHandle _) =
    withMVar cacheHandle (pure . builtinPreludeCheckBuildCount)
