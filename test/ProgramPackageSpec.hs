{-# LANGUAGE GADTs #-}

module ProgramPackageSpec (spec) where

import Control.Concurrent
    ( MVar
    , ThreadId
    , forkFinally
    , newEmptyMVar
    , putMVar
    , takeMVar
    , yield
    )
import Control.Exception (SomeException, bracket, finally, throwIO)
import Data.Either (isRight)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Conc
    ( BlockReason (BlockedOnMVar)
    , ThreadStatus (ThreadBlocked, ThreadDied, ThreadFinished)
    , threadStatus
    )
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, hPutStr, openTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec

import CheckedProgramTestSupport
    ( checkedArtifactBackendLLVM
    , checkedArtifactModuleNames
    , checkedArtifactNativeLLVM
    , checkedArtifactRunOutput
    , checkedProgramArtifactFromCheckedWithPreparation
    , checkedProgramArtifactFromSourceWithCache
    )
import MLF.Backend.Emission.Prepare (prepareBackendEmissionFromSource)
import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , parseRawProgram
    , renderProgramParseError
    )
import MLF.Frontend.ConstraintGen
    ( ExternalBinding (..)
    , externalBindingIdentityFromDetails
    , ExternalBindingMode (..)
    , ModuleConstraintResult (..)
    , RootOwnershipIndex (..)
    , generateModuleConstraintsWithExternalBindings
    )
import MLF.Frontend.Program.Resolve (resolveProgram)
import MLF.Frontend.Program.Check
    ( checkLocatedProgramPackage
    , checkProgram
    , checkProgramPackage
    )
import qualified MLF.Frontend.Program.Check.TestSupport as PreludeCacheTestSupport
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage (..)
    , LocatedProgramSourceUnit (..)
    , PackageModuleGraph (..)
    , PackageModuleGraphNode (..)
    , PackageModuleId (..)
    , ProgramPackage (..)
    , ProgramSourceUnit (..)
    , locatedProgramPackageModuleGraph
    , locatedProgramPackageModuleIds
    , locatedProgramPackageOrderedProgram
    , locatedProgramSourceUnitFromLocated
    , packageModuleGraphNodeIsBuiltinPrelude
    , programPackageModuleGraph
    , trivialLocatedProgramPackage
    , trivialPackageId
    , trivialProgramPackage
    )
import MLF.Frontend.Program.Prelude
    ( preludeSourcePath
    , withPreludeLocatedPackageIfImported
    , withPreludePackage
    )
import MLF.Frontend.Program.TypeFamilies (normalizeTypeFamiliesInProgram)
import qualified MLF.Frontend.Program.Prelude.TestSupport as PreludeTestSupport
import MLF.Frontend.Program.Checked
    ( CheckedProgram
    , checkedProgramMain
    , checkedProgramModules
    , checkedProgramResolved
    )
import MLF.Frontend.Program.Types
    ( ClassInfo (..)
    , CheckedBinding (..)
    , CheckedModule (..)
    , DataInfo (..)
    , InstanceInfo (..)
    , ResolvedSemanticModule
    , TypeView
    , TypeViewNodeView (..)
    , checkedModuleGeneratedIdentities
    , deferredProgramObligationRef
    , resolvedModuleGeneratedIdentities
    , resolvedProgramSemanticArtifact
    , resolvedSemanticModuleIdentity
    , resolvedSemanticModuleName
    , resolvedSemanticProgramModules
    , typeViewNodeView
    )
import MLF.Frontend.Symbol
    ( symbolIdentityWithUnique
    , symbolUniqueIdentity
    )
import MLF.Frontend.Syntax
    ( Expr (..)
    , NormSrcType
    , NormSurfaceExpr
    , SrcTy (..)
    )
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Program.CLI (checkProgramArgs)
import MLF.Reify.TypeOps (freeTypeVarRefsType)
import qualified MLF.Types.Elab as Elab
import MLF.Types.Identity
    ( IdDetails (EnvId)
    , LocalIdentity (..)
    , UniqueIdentity (..)
    , deferredRefIdentity
    , envRefFromIdentity
    , localRefIdentity
    , typeBinderIdentityGeneratedUnique
    )

spec :: Spec
spec =
    describe "MLF.Program package owner" $ do
        it "turns a located one-file program into one trivial package source unit" $ do
            located <- requireLocatedWithFile "single.mlfp" multiModuleSource
            let package = trivialLocatedProgramPackage located

            locatedProgramPackageId package `shouldBe` trivialPackageId
            map locatedProgramSourceUnitPath (locatedProgramPackageSourceUnits package)
                `shouldBe` [Just "single.mlfp"]
            map (map P.moduleName . locatedProgramSourceUnitModules) (locatedProgramPackageSourceUnits package)
                `shouldBe` [["Lib", "Main"]]
            locatedProgramPackageModuleIds package
                `shouldBe`
                    [ PackageModuleId trivialPackageId "Lib"
                    , PackageModuleId trivialPackageId "Main"
                    ]

        it "checks an existing single-file fixture through the trivial package owner" $ do
            source <- readFile "test/programs/recursive-adt/plain-recursive-nat.mlfp"
            program <- requireParsed source
            checkedProgram <- requireRight (checkProgram program)
            checkedPackage <- requireRight (checkProgramPackage (trivialProgramPackage program))

            checkedProgramMain checkedPackage `shouldBe` checkedProgramMain checkedProgram
            map checkedModuleName (checkedProgramModules checkedPackage)
                `shouldBe` map checkedModuleName (checkedProgramModules checkedProgram)

        it "keeps recursive ADT self occurrences owned by their structural mu binder" $ do
            program <- requireParsed minimalRecursiveAdtSource
            checked <- requireRight (checkProgram program)
            mainBinding <-
                case
                    [ binding
                    | checkedModule <- checkedProgramModules checked
                    , binding <- checkedModuleBindings checkedModule
                    , checkedBindingExportedAsMain binding
                    ]
                of
                    [binding] -> pure binding
                    bindings ->
                        expectationFailure
                            ("expected one checked main binding, got " ++ show (length bindings))
                            >> fail "missing checked main binding"
            let checkedType = checkedBindingType mainBinding
                structuralMuOccurrences = structuralMuBinderOccurrences checkedType

            freeTypeVarRefsType checkedType `shouldBe` []
            structuralMuOccurrences `shouldSatisfy` (not . null)
            structuralMuOccurrences
                `shouldSatisfy` all
                    ( \(binderRef, bodyRefs) ->
                        any (Elab.typeBinderRefsSameIdentity binderRef) bodyRefs
                    )

        it "prepends the Prelude source unit to a located trivial package" $ do
            located <-
                requireLocatedWithFile
                    "explicit-prelude.mlfp"
                    ( unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Nat(..), Option(..));"
                        , "  def main : Option Nat = Some Zero;"
                        , "}"
                        ]
                    )
            let package =
                    withPreludeLocatedPackageIfImported
                        (trivialLocatedProgramPackage located)

            locatedProgramPackageModuleIds package
                `shouldBe`
                    [ PackageModuleId trivialPackageId "Prelude"
                    , PackageModuleId trivialPackageId "Main"
                    ]
            map locatedProgramSourceUnitPath (locatedProgramPackageSourceUnits package)
                `shouldBe` [Just preludeSourcePath, Just "explicit-prelude.mlfp"]
            graph <- requireRight (locatedProgramPackageModuleGraph package)
            preludeNode <- requireGraphNode "Prelude" graph
            packageModuleGraphNodeIsBuiltinPrelude preludeNode `shouldBe` True
            checkLocatedProgramPackage package `shouldSatisfy` isRight

        it "keeps an importless located package free of an unused Prelude source unit" $ do
            located <-
                requireLocatedWithFile
                    "importless-main.mlfp"
                    ( unlines
                        [ "module Main export (main) {"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
                    )
            let package =
                    withPreludeLocatedPackageIfImported
                        (trivialLocatedProgramPackage located)

            locatedProgramPackageModuleIds package
                `shouldBe` [PackageModuleId trivialPackageId "Main"]
            map locatedProgramSourceUnitPath (locatedProgramPackageSourceUnits package)
                `shouldBe` [Just "importless-main.mlfp"]
            graph <- requireRight (locatedProgramPackageModuleGraph package)
            mainNode <- requireGraphNode "Main" graph
            packageModuleGraphNodeIsBuiltinPrelude mainNode `shouldBe` False
            checkLocatedProgramPackage package `shouldSatisfy` isRight

        it "retains a package-provided Prelude instead of injecting the builtin" $ do
            located <-
                requireLocatedWithFile
                    "package-prelude.mlfp"
                    packageProvidedPreludeSource
            let package =
                    withPreludeLocatedPackageIfImported
                        (trivialLocatedProgramPackage located)

            locatedProgramPackageModuleIds package
                `shouldBe`
                    [ PackageModuleId trivialPackageId "Prelude"
                    , PackageModuleId trivialPackageId "Main"
                    ]
            map locatedProgramSourceUnitPath (locatedProgramPackageSourceUnits package)
                `shouldBe` [Just "package-prelude.mlfp"]
            graph <- requireRight (locatedProgramPackageModuleGraph package)
            preludeNode <- requireGraphNode "Prelude" graph
            packageModuleGraphNodeIsBuiltinPrelude preludeNode `shouldBe` False
            checkLocatedProgramPackage package `shouldSatisfy` isRight

        it "retains builtin Prelude provenance in an unlocated package" $ do
            program <- requireParsed smallPreludeClientSource
            let package = withPreludePackage (trivialProgramPackage program)

            map programSourceUnitPath (programPackageSourceUnits package)
                `shouldBe` [Just preludeSourcePath, Nothing]
            graph <- requireRight (programPackageModuleGraph package)
            preludeNode <- requireGraphNode "Prelude" graph
            mainNode <- requireGraphNode "Main" graph
            packageModuleGraphNodeIsBuiltinPrelude preludeNode `shouldBe` True
            packageModuleGraphNodeIsBuiltinPrelude mainNode `shouldBe` False

        it "keeps builtin Prelude identities stable across the former 4096 client boundary" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                resolvedPrelude <- resolvedBuiltinPreludeCacheProbe (cacheProbePreludeSource 1)
                let lastClientIdentity = UniqueIdentity 4096
                    expectedNextClientIdentity = UniqueIdentity 4097

                PreludeCacheTestSupport.nextClientIdentityAfterCachedBuiltinPrelude
                    cacheHandle
                    lastClientIdentity
                    resolvedPrelude
                    `shouldBe` Right expectedNextClientIdentity
                PreludeCacheTestSupport.nextClientIdentityAfterCachedBuiltinPrelude
                    cacheHandle
                    lastClientIdentity
                    resolvedPrelude
                    `shouldBe` Right expectedNextClientIdentity
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 1

        it "keeps cached Prelude construction identities disjoint from the client supply" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                _ <-
                    checkWithCustomBuiltinPrelude
                        cacheHandle
                        highArityCacheProbePreludeSource
                        highArityCacheProbeClientSource
                cached <-
                    checkWithCustomBuiltinPrelude
                        cacheHandle
                        highArityCacheProbePreludeSource
                        highArityCacheProbeClientSource

                let preludeOwners =
                        checkedModuleConstructionIdentityOwners
                            (checkedPreludeModule cached)
                    mainOwners =
                        checkedModuleConstructionIdentityOwners
                            (checkedModuleNamed "Main" cached)
                    crossModuleCollisions =
                        Map.intersectionWith (,) preludeOwners mainOwners
                    resolvedMain =
                        case
                            [ resolvedModule
                            | resolvedModule <-
                                resolvedSemanticProgramModules
                                    ( resolvedProgramSemanticArtifact
                                        (checkedProgramResolved cached)
                                    )
                            , resolvedSemanticModuleName resolvedModule == "Main"
                            ]
                          of
                            [resolvedModule] -> resolvedModule
                            resolvedModules ->
                                error
                                    ( "expected one resolved Main module, got "
                                        ++ show (length resolvedModules)
                                    )
                    preludeConstructionIdentities = Map.keysSet preludeOwners
                    resolvedMainIdentities =
                        Set.fromList
                            (resolvedModuleGeneratedIdentities resolvedMain)

                -- The high-arity constructor deliberately exercises several
                -- checker-owned construction allocations. Compare owner sites
                -- directly, then compare them with the independently resolved
                -- client inventory: references are not owners, and advancing a
                -- client supply after finalization cannot repair an identity
                -- that already collided during source resolution.
                preludeOwners `shouldSatisfy` (not . Map.null)
                mainOwners `shouldSatisfy` (not . Map.null)
                crossModuleCollisions `shouldBe` Map.empty
                Set.intersection
                    preludeConstructionIdentities
                    resolvedMainIdentities
                    `shouldBe` Set.empty
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 1

        it "shares one builtin Prelude semantic build across independent runtime artifacts" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                firstArtifact <-
                    requireRight
                        =<< checkedProgramArtifactFromSourceWithCache
                            cacheHandle
                            "runtime-artifact-first.mlfp"
                            smallPreludeClientSource
                secondArtifact <-
                    requireRight
                        =<< checkedProgramArtifactFromSourceWithCache
                            cacheHandle
                            "runtime-artifact-second.mlfp"
                            secondPreludeClientSource

                checkedArtifactRunOutput firstArtifact `shouldBe` Right "Unit\n"
                checkedArtifactRunOutput secondArtifact `shouldBe` Right "Unit\n"
                checkedArtifactModuleNames firstArtifact `shouldBe` ["Prelude", "Main"]
                checkedArtifactModuleNames secondArtifact `shouldBe` ["Prelude", "Main"]
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 1

        it "shares one builtin Prelude build across interpreter and backend consumers" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                artifact <-
                    requireRight
                        =<< checkedProgramArtifactFromSourceWithCache
                            cacheHandle
                            "runtime-artifact-shared-consumers.mlfp"
                            smallPreludeClientSource

                checkedArtifactRunOutput artifact `shouldBe` Right "Unit\n"
                checkedArtifactBackendLLVM artifact `shouldSatisfy` isRight
                checkedArtifactNativeLLVM artifact `shouldSatisfy` isRight
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 1

        it "does not inject or check Prelude for a higher-kinded runtime artifact without a Prelude import" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                artifact <-
                    requireRight
                        =<< checkedProgramArtifactFromSourceWithCache
                            cacheHandle
                            "runtime-artifact-higher-kinded-no-prelude.mlfp"
                            higherKindedRuntimeWithoutPreludeSource

                checkedArtifactRunOutput artifact `shouldBe` Right "true\n"
                checkedArtifactModuleNames artifact `shouldBe` ["Main"]
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 0

        it "checks a compiler-exact nested lambda spine with one terminal recursive result" $ do
            program <- requireParsed nestedLambdaRecursiveResultSource
            _ <- requireRight (checkProgram program)
            pure ()

        it "includes binding, data, class, and instance metadata in the shared checked-module identity inventory" $ do
            program <- requireParsed metadataInventorySource
            checked <- requireRight (checkProgram program)
            let checkedModule = checkedModuleNamed "Main" checked
                baseIdentities = Set.fromList (checkedModuleGeneratedIdentities checkedModule)
                bindingIdentity = UniqueIdentity 996101
                dataIdentity = UniqueIdentity 996102
                classIdentity = UniqueIdentity 996103
                instanceIdentity = UniqueIdentity 996104
                sentinels = [bindingIdentity, dataIdentity, classIdentity, instanceIdentity]

            all (`Set.notMember` baseIdentities) sentinels `shouldBe` True
            case
                ( checkedModuleBindings checkedModule
                , Map.elems (checkedModuleData checkedModule)
                , Map.elems (checkedModuleClasses checkedModule)
                , checkedModuleInstances checkedModule
                ) of
                (binding : bindings, dataInfo : _, classInfo : _, instanceInfo : _) -> do
                    let resolved = checkedBindingResolvedVar binding
                        bindingWithSentinel =
                            binding
                                { checkedBindingResolvedVar =
                                    resolved
                                        { Elab.resolvedVarDetails =
                                            EnvId (envRefFromIdentity bindingIdentity "$inventory-binding")
                                        }
                                }
                        dataSymbol =
                            symbolIdentityWithUnique dataIdentity (dataInfoSymbol dataInfo)
                        dataWithSentinel = dataInfo {dataInfoSymbol = dataSymbol}
                        classSymbol =
                            symbolIdentityWithUnique classIdentity (classInfoSymbol classInfo)
                        classWithSentinel = classInfo {classInfoSymbol = classSymbol}
                        instanceModuleIdentity =
                            symbolIdentityWithUnique
                                instanceIdentity
                                (instanceOriginModuleIdentity instanceInfo)
                        instanceWithSentinel =
                            instanceInfo
                                { instanceOriginModuleIdentity = instanceModuleIdentity
                                }
                        variants =
                            [ ( bindingIdentity
                              , checkedModule
                                    { checkedModuleBindings = bindingWithSentinel : bindings
                                    }
                              )
                            , ( dataIdentity
                              , checkedModule
                                    { checkedModuleData = Map.singleton dataSymbol dataWithSentinel
                                    }
                              )
                            , ( classIdentity
                              , checkedModule
                                    { checkedModuleClasses = Map.singleton classSymbol classWithSentinel
                                    }
                              )
                            , ( instanceIdentity
                              , checkedModule
                                    { checkedModuleInstances = [instanceWithSentinel]
                                    }
                              )
                            ]
                    mapM_
                        ( \(sentinel, variant) ->
                            Set.fromList (checkedModuleGeneratedIdentities variant)
                                `shouldSatisfy` Set.member sentinel
                        )
                        variants
                metadata ->
                    expectationFailure
                        ("expected binding/data/class/instance inventory fixtures, got " ++ show metadata)

        it "does not infer builtin Prelude provenance from the sentinel path" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                located <- requireLocatedWithFile preludeSourcePath fakeSentinelPreludeSource
                let package = trivialLocatedProgramPackage located
                graph <- requireRight (locatedProgramPackageModuleGraph package)
                preludeNode <- requireGraphNode "Prelude" graph

                packageModuleGraphNodeSourcePath preludeNode `shouldBe` Just preludeSourcePath
                packageModuleGraphNodeIsBuiltinPrelude preludeNode `shouldBe` False
                PreludeCacheTestSupport.checkLocatedProgramPackageWithCache cacheHandle package
                    `shouldSatisfy` isRight
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 0

        it "does not reuse a cached Prelude for different resolved syntax" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                canonical <- checkWithBuiltinPrelude cacheHandle 1
                modified <- checkWithBuiltinPrelude cacheHandle 2

                checkedPreludeModule modified `shouldNotBe` checkedPreludeModule canonical
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 2

        it "does not reuse a cached Prelude when only its module identity changes" $ do
            withFreshBuiltinPreludeCheckProbeCache $ \cacheHandle -> do
                resolvedPrelude <- resolvedBuiltinPreludeCacheProbe (cacheProbePreludeSource 1)
                let originalModuleIdentity = resolvedSemanticModuleIdentity resolvedPrelude
                    identityCandidate = UniqueIdentity 7000001
                    shiftedUnique
                        | symbolUniqueIdentity originalModuleIdentity == identityCandidate =
                            UniqueIdentity 7000002
                        | otherwise = identityCandidate
                    shiftedPrelude =
                        resolvedPrelude
                            { resolvedSemanticModuleIdentity =
                                symbolIdentityWithUnique
                                    shiftedUnique
                                    originalModuleIdentity
                            }

                PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe cacheHandle resolvedPrelude
                PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe cacheHandle shiftedPrelude

                PreludeCacheTestSupport.readBuiltinPreludeCheckProbeBuildCount cacheHandle
                    `shouldReturn` 2

        it "keeps overlapping fresh Prelude cache actions independent" $ do
            firstCache <- PreludeCacheTestSupport.newBuiltinPreludeCheckProbeCache
            secondCache <- PreludeCacheTestSupport.newBuiltinPreludeCheckProbeCache
            firstPrelude <- resolvedBuiltinPreludeCacheProbe (cacheProbePreludeSource 1)
            secondPrelude <- resolvedBuiltinPreludeCacheProbe (cacheProbePreludeSource 2)
            firstReady <- newEmptyMVar
            secondReady <- newEmptyMVar
            startFirstCheck <- newEmptyMVar
            startSecondCheck <- newEmptyMVar
            firstChecked <- newEmptyMVar
            secondChecked <- newEmptyMVar
            readFirstCount <- newEmptyMVar
            readSecondCount <- newEmptyMVar
            firstDone <- newEmptyMVar
            secondDone <- newEmptyMVar

            _ <-
                forkFinally
                    ( overlappingCacheProbe
                        firstCache
                        firstPrelude
                        firstReady
                        startFirstCheck
                        firstChecked
                        readFirstCount
                    )
                    (putMVar firstDone)
            _ <-
                forkFinally
                    ( overlappingCacheProbe
                        secondCache
                        secondPrelude
                        secondReady
                        startSecondCheck
                        secondChecked
                        readSecondCount
                    )
                    (putMVar secondDone)

            takeMVar firstReady
            takeMVar secondReady
            putMVar startFirstCheck ()
            putMVar startSecondCheck ()
            takeMVar firstChecked
            takeMVar secondChecked
            putMVar readFirstCount ()
            putMVar readSecondCount ()

            firstCount <- requireThreadResult =<< takeMVar firstDone
            secondCount <- requireThreadResult =<< takeMVar secondDone
            firstCount `shouldBe` 1
            secondCount `shouldBe` 1

        it "single-flights concurrent misses through one Prelude cache handle" $ do
            firstBuildEntered <- newEmptyMVar
            releaseFirstBuild <- newEmptyMVar
            secondStarted <- newEmptyMVar
            firstDone <- newEmptyMVar
            secondDone <- newEmptyMVar
            cacheHandle <-
                PreludeCacheTestSupport.newBuiltinPreludeCheckProbeCacheWithBuildAction
                    (putMVar firstBuildEntered () >> takeMVar releaseFirstBuild)
            resolvedPrelude <- resolvedBuiltinPreludeCacheProbe (cacheProbePreludeSource 1)

            _ <-
                forkFinally
                    ( PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe
                        cacheHandle
                        resolvedPrelude
                    )
                    (putMVar firstDone)
            requireWithin "first Prelude cache build did not start" (takeMVar firstBuildEntered)

            secondThread <-
                forkFinally
                    ( do
                        putMVar secondStarted ()
                        PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe
                            cacheHandle
                            resolvedPrelude
                    )
                    (putMVar secondDone)
            requireWithin "second Prelude cache lookup did not start" (takeMVar secondStarted)
            requireWithin
                "second Prelude cache lookup did not block behind the in-flight build"
                (waitUntilBlockedOnMVar secondThread)
            putMVar releaseFirstBuild ()

            requireThreadResult
                =<< requireWithin
                    "first Prelude cache lookup did not finish"
                    (takeMVar firstDone)
            requireThreadResult
                =<< requireWithin
                    "second Prelude cache lookup did not finish"
                    (takeMVar secondDone)
            PreludeCacheTestSupport.readBuiltinPreludeCheckProbeBuildCount cacheHandle
                `shouldReturn` 1

        it "does not reuse a cached Prelude when resolved binder spelling changes" $ do
            withFreshBuiltinPreludeCheckProbeCache $ \cacheHandle -> do
                first <-
                    resolvedBuiltinPreludeCacheProbe
                        (cacheProbeBinderPreludeSource "a")
                second <-
                    resolvedBuiltinPreludeCacheProbe
                        (cacheProbeBinderPreludeSource "b")

                second `shouldBe` first
                PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe
                    cacheHandle
                    first
                PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe
                    cacheHandle
                    second
                PreludeCacheTestSupport.readBuiltinPreludeCheckProbeBuildCount cacheHandle
                    `shouldReturn` 2

        it "resolves builtin Prelude before unrelated package roots for stable cache reuse" $ do
            withFreshBuiltinPreludeCheckProbeCache $ \cacheHandle -> do
                preludeFirst <- builtinPreludeOrderProbePackage True
                supportFirst <- builtinPreludeOrderProbePackage False
                firstResolved <- resolvedBuiltinPreludeFromPackage preludeFirst
                secondResolved <- resolvedBuiltinPreludeFromPackage supportFirst

                PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe cacheHandle firstResolved
                PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe cacheHandle secondResolved

                secondResolved `shouldBe` firstResolved
                PreludeCacheTestSupport.readBuiltinPreludeCheckProbeBuildCount cacheHandle
                    `shouldReturn` 1

        it "reuses the ordinary Prelude build from the timed entrypoint" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                package <- cacheProbePreludePackage
                ordinary <-
                    requireRight
                        ( PreludeCacheTestSupport.checkLocatedProgramPackageWithCache
                            cacheHandle
                            package
                        )
                timed <-
                    requireRight
                        =<< PreludeCacheTestSupport.checkLocatedProgramPackageWithDefaultTimingAndCache
                            cacheHandle
                            package

                checkedPreludeModule timed `shouldBe` checkedPreludeModule ordinary
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 1

        it "reuses the timed Prelude build from the ordinary entrypoint" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                package <- cacheProbePreludePackage
                timed <-
                    requireRight
                        =<< PreludeCacheTestSupport.checkLocatedProgramPackageWithDefaultTimingAndCache
                            cacheHandle
                            package
                ordinary <-
                    requireRight
                        ( PreludeCacheTestSupport.checkLocatedProgramPackageWithCache
                            cacheHandle
                            package
                        )

                checkedPreludeModule ordinary `shouldBe` checkedPreludeModule timed
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 1

        it "checks a tagged Prelude with imports through the ordinary interface path" $ do
            withFreshBuiltinPreludeCheckCache $ \cacheHandle -> do
                checked <-
                    checkWithCustomBuiltinPrelude
                        cacheHandle
                        importedPreludeSource
                        importedPreludeClientSource

                checkedPreludeModule checked `shouldSatisfy` ((== "Prelude") . checkedModuleName)
                PreludeCacheTestSupport.readBuiltinPreludeCheckBuildCount cacheHandle
                    `shouldReturn` 0

        it "keeps backend preparation on the located trivial package path" $ do
            checked <-
                requireRight $
                    prepareBackendEmissionFromSource
                        "inline-unit.mlfp"
                        ( unlines
                            [ "module Main export (main) {"
                            , "  import Prelude exposing (Unit(..));"
                            , "  def main : Unit = Unit;"
                            , "}"
                            ]
                        )

            map checkedModuleName (checkedProgramModules checked) `shouldBe` ["Prelude", "Main"]

        it "interpreter artifact access does not force deferred backend preparation" $ do
            checked <- requireCheckedArtifactProgram
            let checkedOnlyArtifact =
                    checkedProgramArtifactFromCheckedWithPreparation
                        (\_ -> error "interpreter forced backend preparation")
                        checked

            checkedArtifactRunOutput checkedOnlyArtifact `shouldBe` Right "true\n"

        it "shares one backend preparation evaluation across LLVM/native consumers" $ do
            checked <- requireCheckedArtifactProgram
            preparationCount <- newIORef 0
            let artifact =
                    checkedProgramArtifactFromCheckedWithPreparation
                        (countedIdentityPreparation preparationCount)
                        checked

            checkedArtifactBackendLLVM artifact `shouldSatisfy` isRight
            checkedArtifactNativeLLVM artifact `shouldSatisfy` isRight
            readIORef preparationCount `shouldReturn` 1

        it "checks independent annotated defs equivalently across batch boundaries" $
            withBatchEquivalenceFile $ \path -> do
                productionBatch <- checkProgramArgsWithBatchSize Nothing [path]
                batch1 <- checkProgramArgsWithBatchSize (Just "1") [path]
                batch2 <- checkProgramArgsWithBatchSize (Just "2") [path]

                batch1 `shouldBe` productionBatch
                batch2 `shouldBe` productionBatch
                productionBatch `shouldBe` Right "OK\n"

        it "preserves recursive and unsupported work after a deferred prefix batch" $ do
            PreludeCacheTestSupport.splitContiguousEligibleBatchForTest
                8
                [True, True, False, True]
                `shouldBe` ([True, True], [False, True])
            PreludeCacheTestSupport.splitContiguousEligibleBatchForTest
                2
                [True, True, True, False]
                `shouldBe` ([True, True], [True, False])

        it "keeps construction scopes root-local for String-heavy definitions in one batch" $
            withProgramSourceFile "root-local-string" rootLocalStringBatchSource $ \path -> do
                isolatedRoots <- checkProgramArgsWithBatchSize (Just "1") [path]
                sharedBatch <- checkProgramArgsWithBatchSize Nothing [path]

                sharedBatch `shouldBe` isolatedRoots
                sharedBatch `shouldBe` Right "OK\n"

        it "keeps nested monomorphic application results in their exact lexical scopes" $
            withProgramSourceFile "nested-string-results" nestedStringResultsSource $ \path ->
                checkProgramArgsWithBatchSize (Just "1") [path]
                    `shouldReturn` Right "OK\n"

        it "retains a declared recursive endpoint through a deep partial-application spine with a reused argument" $
            withProgramSourceFile "deep-partial-application" deepPartialApplicationSource $ \path ->
                checkProgramArgsWithBatchSize (Just "1") [path]
                    `shouldReturn` Right "OK\n"

        it "constructs a local application Gamma above its deeper result scope" $
            withProgramSourceFile "local-application-gamma" localApplicationGammaSource $ \path ->
                checkProgramArgsWithBatchSize (Just "1") [path]
                    `shouldReturn` Right "OK\n"

        it "uses exact ambient aliases for repeated recursive branch results" $
            withProgramSourceFile "repeated-recursive-choice" repeatedRecursiveChoiceSource $ \path ->
                checkProgramArgsWithBatchSize (Just "1") [path]
                    `shouldReturn` Right "OK\n"

        it "coalesces cross-module root RaiseMerge edges only after their flexible routes solve away" $
            withProgramSourceFile "shared-root-raise-merge" sharedRootRaiseMergeSource $ \path ->
                checkProgramArgsWithBatchSize (Just "1") [path]
                    `shouldReturn` Right "OK\n"

        it "keeps external scheme instantiations root-local in module constraint batches" $ do
            ModuleConstraintResult {mcrRootOwnership = rootOwnership} <-
                requireRight $
                    generateModuleConstraintsWithExternalBindings
                        Set.empty
                        ( Map.singleton
                            "one"
                            ExternalBinding
                                { externalBindingType = intSourceType
                                , externalBindingMode = ExternalBindingScheme
                                , externalBindingIdentity =
                                    externalBindingIdentityFromDetails
                                        (EnvId (envRefFromIdentity (UniqueIdentity 991000) "one"))
                                , externalBindingTypeHeadIdentities = Map.empty
                                , externalBindingTypeBinderIdentities = Map.empty
                                }
                        )
                        [ ("value1", externalOneExpr)
                        , ("value2", externalOneExpr)
                        ]
            let root0Nodes = nodesOwnedByRoot 0 rootOwnership
                root1Nodes = nodesOwnedByRoot 1 rootOwnership

            root0Nodes `shouldSatisfy` (not . IntSet.null)
            root1Nodes `shouldSatisfy` (not . IntSet.null)
            root0Nodes `IntSet.intersection` root1Nodes `shouldBe` IntSet.empty
            all ((== 1) . IntSet.size) (IntMap.elems (roiNodeOwners rootOwnership))
                `shouldBe` True

multiModuleSource :: String
multiModuleSource =
    unlines
        [ "module Lib export (one) {"
        , "  def one : Int = 1;"
        , "}"
        , "module Main export (main) {"
        , "  import Lib exposing (one);"
        , "  def main : Int = one;"
        , "}"
        ]

minimalRecursiveAdtSource :: String
minimalRecursiveAdtSource =
    unlines
        [ "module Main export (main) {"
        , "  data Nat = Succ : Nat -> Nat;"
        , "  def main : Nat -> Bool = λn true;"
        , "}"
        ]

structuralMuBinderOccurrences
    :: Elab.ElabType
    -> [(Elab.TypeBinderRef, [Elab.TypeBinderRef])]
structuralMuBinderOccurrences ty =
    case ty of
        Elab.TVarRef{} -> []
        Elab.TArrow argument result ->
            structuralMuBinderOccurrences argument
                ++ structuralMuBinderOccurrences result
        Elab.TConWithIdentity _ _ arguments ->
            foldMap structuralMuBinderOccurrences arguments
        Elab.TVarAppRef _ arguments ->
            foldMap structuralMuBinderOccurrences arguments
        Elab.TBaseWithIdentity{} -> []
        Elab.TForallRef _ _ body -> structuralMuBinderOccurrences body
        Elab.TMuRef binderRef body ->
            (binderRef, freeTypeVarRefsType body)
                : structuralMuBinderOccurrences body
        Elab.TBottom -> []

batchEquivalenceSource :: String
batchEquivalenceSource =
    unlines $
        [ "module Helper export (one) {"
        , "  def one : Int = 1;"
        , "}"
        , "module Main export (main) {"
        , "  import Helper exposing (one);"
        ]
            -- Cross both the production size-16 boundary and the diagnostic
            -- size-2 boundary.  Parser-scale load belongs in the benchmark,
            -- while this test owns semantic equivalence between batch plans.
            ++ [ "  def value" ++ show index ++ " : Int = one;"
               | index <- [(1 :: Int) .. 18]
               ]
            ++ [ "  def main : Int = value1;"
               , "}"
               ]

rootLocalStringBatchSource :: String
rootLocalStringBatchSource =
    unlines
        [ "module Main export (main) {"
        , "  data StringMatch ="
        , "      StringMatched : StringMatch"
        , "    | StringMismatch : StringMatch;"
        , "  def cursorEnd : String -> String -> String -> String ="
        , "    λ(_lineNumber : String) λ(_linePrefix : String) λ(sourceLexeme : String) sourceLexeme;"
        , "  def stringExactMatch : String -> String -> StringMatch ="
        , "    λ(_expected : String) λ(_actual : String) StringMatched;"
        , "  def main : StringMatch = stringExactMatch (cursorEnd \"1\" \"\" \"x\") \"x\";"
        , "}"
        ]

nestedStringResultsSource :: String
nestedStringResultsSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (stringAppend, stringFromInt, stringLength);"
        , "  def cursorEnd : String -> String -> String -> String ="
        , "    λ(lineNumber : String) λ(linePrefix : String) λ(sourceLexeme : String)"
        , "      stringAppend lineNumber"
        , "        (stringAppend \":\""
        , "          (stringFromInt (stringLength (stringAppend (stringAppend \" \" linePrefix) sourceLexeme))));"
        , "  def main : String = cursorEnd \"1\" \"\" \"x\";"
        , "}"
        ]

deepPartialApplicationSource :: String
deepPartialApplicationSource =
    unlines
        [ "module Main export (main) {"
        , "  data Value ="
        , "      ValueUnit : Value"
        , "    | ValueText : String -> Value;"
        , "  def tokenStart : Value -> String ="
        , "    λ(_token : Value) \"1:1\";"
        , "  def spanToToken : String -> Value -> String ="
        , "    λ(start : String) λ(_token : Value) start;"
        , "  def useString : String -> String ="
        , "    λ(value : String) value;"
        , "  def bind : String -> (Value -> String) -> String ="
        , "    λ(first : String) λ(_next : Value -> String) first;"
        , "  def finish : Value -> Value -> Value -> Value -> String ="
        , "    λ(_name : Value) λ(_parameter : Value) λ(_semicolon : Value) λ(_rows : Value) \"done\";"
        , "  def target : Value -> Value -> Value -> Value -> String ="
        , "    λ(constructor : Value) λ(name : Value) λ(parameter : Value) λ(semicolon : Value)"
        , "      bind (useString (spanToToken (tokenStart constructor) semicolon))"
        , "        (finish name parameter semicolon);"
        , "  def main : Bool = true;"
        , "}"
        ]

localApplicationGammaSource :: String
localApplicationGammaSource =
    unlines
        [ "module Main export (main) {"
        , "  data ParserState ="
        , "      ParserState : ParserState;"
        , "  data ParserValue ="
        , "      ParserValue : ParserValue;"
        , "  data ParserStep ="
        , "      ParserStepOk : ParserState -> ParserValue -> ParserStep;"
        , "  data ParserExpectation ="
        , "      ParserExpectImportSemicolon : ParserExpectation;"
        , "  data Parser a ="
        , "      Parser : (ParserState -> ParserStep) -> Parser a;"
        , "  def parserChoice : Parser ParserValue -> Parser ParserValue -> Parser ParserValue ="
        , "    λ(first : Parser ParserValue) λ(_second : Parser ParserValue) first;"
        , "  def expectText : String -> Parser ParserValue ="
        , "    λ(_text : String)"
        , "      (Parser (λ(state : ParserState) ParserStepOk state ParserValue) : Parser ParserValue);"
        , "  def parserFailExpectedAtCurrent : ParserExpectation -> Parser ParserValue ="
        , "    λ(_expectation : ParserExpectation)"
        , "      (Parser (λ(state : ParserState) ParserStepOk state ParserValue) : Parser ParserValue);"
        , "  def target : Parser ParserValue ="
        , "    parserChoice (expectText \";\") (parserFailExpectedAtCurrent ParserExpectImportSemicolon);"
        , "  def main : Bool = true;"
        , "}"
        ]

repeatedRecursiveChoiceSource :: String
repeatedRecursiveChoiceSource =
    unlines
        [ "module Main export (main) {"
        , "  data ParserState = ParserState : ParserState;"
        , "  data ParserValue = ParserValue : ParserValue;"
        , "  data ParserStep = ParserStepOk : ParserState -> ParserValue -> ParserStep;"
        , "  data Parser a = Parser : (ParserState -> ParserStep) -> Parser a;"
        , "  def parserChoice : Parser ParserValue -> Parser ParserValue -> Parser ParserValue ="
        , "    λ(first : Parser ParserValue) λ(_second : Parser ParserValue) first;"
        , "  def branchOne : String -> ParserValue -> Parser ParserValue ="
        , "    λ(_sourceFile : String) λ(_start : ParserValue)"
        , "      (Parser (λ(state : ParserState) ParserStepOk state ParserValue) : Parser ParserValue);"
        , "  def branchTwo : String -> ParserValue -> Parser ParserValue ="
        , "    λ(_sourceFile : String) λ(_start : ParserValue)"
        , "      (Parser (λ(state : ParserState) ParserStepOk state ParserValue) : Parser ParserValue);"
        , "  def branchThree : String -> ParserValue -> Parser ParserValue ="
        , "    λ(_sourceFile : String) λ(_start : ParserValue)"
        , "      (Parser (λ(state : ParserState) ParserStepOk state ParserValue) : Parser ParserValue);"
        , "  def branchFour : String -> ParserValue -> Parser ParserValue ="
        , "    λ(_sourceFile : String) λ(_start : ParserValue)"
        , "      (Parser (λ(state : ParserState) ParserStepOk state ParserValue) : Parser ParserValue);"
        , "  def choiceRows : String -> ParserValue -> Parser ParserValue ="
        , "    λ(sourceFile : String) λ(_start : ParserValue)"
        , "      parserChoice (branchOne sourceFile ParserValue)"
        , "        (parserChoice (branchTwo sourceFile ParserValue)"
        , "          (parserChoice (branchThree sourceFile ParserValue)"
        , "            (branchFour sourceFile ParserValue)));"
        , "  def main : Bool = true;"
        , "}"
        ]

sharedRootRaiseMergeSource :: String
sharedRootRaiseMergeSource =
    unlines
        [ "module ParserReplyDiagnostic export (ParserDiagnostic(..)) {"
        , "  data ParserDiagnostic ="
        , "      UnexpectedSourceText : String -> ParserDiagnostic"
        , "    | ExpectedCompleteModule : String -> ParserDiagnostic"
        , "    | ExpectedEquals : String -> ParserDiagnostic;"
        , "}"
        , "module ParserReplySource export (basicUnexpectedSpan) {"
        , "  def basicUnexpectedSpan : String = \"unexpected\";"
        , "}"
        , "module ParserReplyTypes export (ParserState(..), ParserEnd(..), ParserValue(..), ParserStep(..), parserStateAtEnd) {"
        , "  import ParserReplyDiagnostic exposing (ParserDiagnostic);"
        , "  data ParserState ="
        , "      ParserState : String -> ParserState;"
        , "  data ParserEnd ="
        , "      ParserAtEnd : ParserEnd"
        , "    | ParserNotAtEnd : ParserEnd"
        , "    | ParserEndUnknown : ParserEnd;"
        , "  data ParserValue ="
        , "      ValueModuleKey : String -> ParserValue"
        , "    | ValueProjectionRows : String -> ParserValue"
        , "    | ValueConstructorRows : String -> ParserValue"
        , "    | ValueUnit : ParserValue"
        , "    | ValueToken : String -> ParserValue;"
        , "  data ParserStep ="
        , "      ParserStepOk : ParserState -> ParserValue -> ParserStep"
        , "    | ParserStepError : ParserDiagnostic -> ParserStep;"
        , "  def parserStateAtEnd : ParserState -> ParserEnd ="
        , "    λ(_state : ParserState) ParserAtEnd;"
        , "}"
        , "module Main export (main) {"
        , "  import ParserReplyDiagnostic exposing (ParserDiagnostic(..));"
        , "  import ParserReplySource exposing (basicUnexpectedSpan);"
        , "  import ParserReplyTypes exposing (ParserEnd(..), ParserState, ParserStep(..), ParserValue(..), parserStateAtEnd);"
        , "  data ParserResult ="
        , "      ParserOk : String -> ParserResult"
        , "    | ParserError : ParserDiagnostic -> ParserResult;"
        , "  def parserReplyToResult : ParserStep -> ParserResult ="
        , "    λ(reply : ParserStep) case reply of {"
        , "      ParserStepOk state value -> case parserStateAtEnd state of {"
        , "        ParserAtEnd -> case value of {"
        , "          ValueModuleKey key -> ParserOk key;"
        , "          ValueProjectionRows rows -> ParserOk rows;"
        , "          ValueConstructorRows _ -> ParserError (ExpectedCompleteModule basicUnexpectedSpan);"
        , "          ValueUnit -> ParserError (ExpectedCompleteModule basicUnexpectedSpan);"
        , "          ValueToken _ -> ParserError (ExpectedCompleteModule basicUnexpectedSpan)"
        , "        };"
        , "        ParserNotAtEnd -> ParserError (ExpectedCompleteModule basicUnexpectedSpan);"
        , "        ParserEndUnknown -> ParserError (ExpectedCompleteModule basicUnexpectedSpan)"
        , "      };"
        , "      ParserStepError diagnostic -> ParserError diagnostic"
        , "    };"
        , "  def main : Bool = true;"
        , "}"
        ]

smallPreludeClientSource :: String
smallPreludeClientSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..));"
        , "  def main : Unit = Unit;"
        , "}"
        ]

secondPreludeClientSource :: String
secondPreludeClientSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..));"
        , "  def constantUnit : Unit = Unit;"
        , "  def main : Unit = constantUnit;"
        , "}"
        ]

higherKindedRuntimeWithoutPreludeSource :: String
higherKindedRuntimeWithoutPreludeSource =
    unlines
        [ "module Main export (Boxed, Box(..), truthy, main) {"
        , "  class Boxed (f :: * -> *) {"
        , "    truthy : f Bool -> Bool;"
        , "  }"
        , "  data Box a ="
        , "      Box : a -> Box a;"
        , "  instance Boxed Box {"
        , "    truthy = λbox true;"
        , "  }"
        , "  def main : Bool = truthy (Box false);"
        , "}"
        ]

nestedLambdaRecursiveResultSource :: String
nestedLambdaRecursiveResultSource =
    unlines
        [ "module SeedSource export (SourceSpan(..), SeedIdentifier(..), SeedBoolLiteral(..), SeedInputSymbol(..), SeedInput(..)) {"
        , "  data SourceSpan ="
        , "      SpanDefKeyword : SourceSpan"
        , "    | SpanIdentifierMain : SourceSpan"
        , "    | SpanEquals : SourceSpan"
        , "    | SpanBoolTrue : SourceSpan"
        , "    | SpanUnknownSymbol : SourceSpan;"
        , "  data SeedIdentifier ="
        , "      IdentifierMain : SeedIdentifier;"
        , "  data SeedBoolLiteral ="
        , "      BoolLiteralTrue : SeedBoolLiteral;"
        , "  data SeedInputSymbol ="
        , "      InputDef : SourceSpan -> SeedInputSymbol"
        , "    | InputIdentifier : SourceSpan -> SeedIdentifier -> SeedInputSymbol"
        , "    | InputEquals : SourceSpan -> SeedInputSymbol"
        , "    | InputBoolLiteral : SourceSpan -> SeedBoolLiteral -> SeedInputSymbol"
        , "    | InputUnknown : SourceSpan -> SeedInputSymbol;"
        , "  data SeedInput ="
        , "      SeedInputNil : SeedInput"
        , "    | SeedInputCons : SeedInputSymbol -> SeedInput -> SeedInput;"
        , "}"
        , "module SeedToken export (SeedToken(..), SeedTokenStream(..)) {"
        , "  import SeedSource exposing (SourceSpan, SeedIdentifier, SeedBoolLiteral);"
        , "  data SeedToken ="
        , "      TokenDef : SourceSpan -> SeedToken"
        , "    | TokenIdentifier : SourceSpan -> SeedIdentifier -> SeedToken"
        , "    | TokenEquals : SourceSpan -> SeedToken"
        , "    | TokenBoolLiteral : SourceSpan -> SeedBoolLiteral -> SeedToken;"
        , "  data SeedTokenStream ="
        , "      SeedTokenNil : SeedTokenStream"
        , "    | SeedTokenCons : SeedToken -> SeedTokenStream -> SeedTokenStream;"
        , "}"
        , "module SeedDiagnostic export (LexerDiagnosticKind(..), LexerDiagnostic(..)) {"
        , "  import SeedSource exposing (SourceSpan);"
        , "  data LexerDiagnosticKind ="
        , "      UnknownInputSymbol : LexerDiagnosticKind;"
        , "  data LexerDiagnostic ="
        , "      LexerDiagnostic : SourceSpan -> LexerDiagnosticKind -> LexerDiagnostic;"
        , "}"
        , "module Main export (LexerResult(..), lexAfterIdentifier, lexAfterEquals, main) {"
        , "  import SeedSource exposing (SourceSpan(..), SeedIdentifier(..), SeedInput(..), SeedInputSymbol(..));"
        , "  import SeedToken exposing (SeedTokenStream(..));"
        , "  import SeedDiagnostic exposing (LexerDiagnosticKind(..), LexerDiagnostic(..));"
        , "  data LexerResult ="
        , "      LexerOk : SeedTokenStream -> LexerResult"
        , "    | LexerError : LexerDiagnostic -> LexerResult;"
        , "  def lexAfterIdentifier : SourceSpan -> SourceSpan -> SeedIdentifier -> SeedInput -> LexerResult ="
        , "    λ(defSpan : SourceSpan) λ(identSpan : SourceSpan) λ(identifier : SeedIdentifier) λ(input : SeedInput) case input of {"
        , "      SeedInputNil -> LexerError (LexerDiagnostic identSpan UnknownInputSymbol);"
        , "      SeedInputCons symbol rest -> case symbol of {"
        , "        InputDef span -> LexerError (LexerDiagnostic span UnknownInputSymbol);"
        , "        InputIdentifier span _ -> LexerError (LexerDiagnostic span UnknownInputSymbol);"
        , "        InputEquals span -> LexerError (LexerDiagnostic span UnknownInputSymbol);"
        , "        InputBoolLiteral span _ -> LexerError (LexerDiagnostic span UnknownInputSymbol);"
        , "        InputUnknown span -> LexerError (LexerDiagnostic span UnknownInputSymbol)"
        , "      }"
        , "    };"
        , "  def lexAfterEquals : SourceSpan -> SourceSpan -> SeedIdentifier -> SourceSpan -> SeedInput -> LexerResult ="
        , "    λ(defSpan : SourceSpan) λ(identSpan : SourceSpan) λ(identifier : SeedIdentifier) λ(equalsSpan : SourceSpan) λ(input : SeedInput) case input of {"
        , "      SeedInputNil -> LexerError (LexerDiagnostic equalsSpan UnknownInputSymbol);"
        , "      SeedInputCons symbol rest -> case symbol of {"
        , "        InputDef span -> LexerError (LexerDiagnostic span UnknownInputSymbol);"
        , "        InputIdentifier span _ -> LexerError (LexerDiagnostic span UnknownInputSymbol);"
        , "        InputEquals span -> LexerError (LexerDiagnostic span UnknownInputSymbol);"
        , "        InputBoolLiteral span _ -> LexerError (LexerDiagnostic span UnknownInputSymbol);"
        , "        InputUnknown span -> LexerError (LexerDiagnostic span UnknownInputSymbol)"
        , "      }"
        , "    };"
        , "  def main : LexerResult ="
        , "    lexAfterEquals SpanDefKeyword SpanIdentifierMain IdentifierMain SpanEquals SeedInputNil;"
        , "}"
        ]

packageProvidedPreludeSource :: String
packageProvidedPreludeSource =
    unlines
        [ "module Prelude export (Unit(..)) {"
        , "  data Unit ="
        , "      Unit : Unit;"
        , "}"
        , "module Main export (main) {"
        , "  import Prelude exposing (Unit(..));"
        , "  def main : Unit = Unit;"
        , "}"
        ]

metadataInventorySource :: String
metadataInventorySource =
    unlines
        [ "module Main export (Marker, Token(..), mark, main) {"
        , "  class Marker a {"
        , "    mark : a -> Bool;"
        , "  }"
        , "  data Token ="
        , "      Token : Token;"
        , "  instance Marker Token {"
        , "    mark = λtoken true;"
        , "  }"
        , "  def main : Bool = mark Token;"
        , "}"
        ]

cacheProbeClientSource :: String
cacheProbeClientSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (cachedValue);"
        , "  def main : Int = cachedValue;"
        , "}"
        ]

cacheProbePreludeSource :: Int -> String
cacheProbePreludeSource value =
    unlines
        [ "module Prelude export (cachedValue) {"
        , "  def cachedValue : Int = " ++ show value ++ ";"
        , "}"
        ]

cacheProbeBinderPreludeSource :: String -> String
cacheProbeBinderPreludeSource binderName =
    unlines
        [ "module Prelude export (cachedId) {"
        , "  def cachedId : ∀ " ++ binderName ++ ". " ++ binderName ++ " -> " ++ binderName ++ " = λx x;"
        , "}"
        ]

highArityCacheProbePreludeSource :: String
highArityCacheProbePreludeSource =
    unlines
        [ "module Prelude export (Token(..)) {"
        , "  data Token ="
        , "      Token : " ++ concat (replicate 10 "Int -> ") ++ "Token;"
        , "}"
        ]

highArityCacheProbeClientSource :: String
highArityCacheProbeClientSource =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Token(..));"
        , "  def main : Token = Token " ++ unwords (replicate 10 "1") ++ ";"
        , "}"
        ]

importedPreludeSource :: String
importedPreludeSource =
    unlines
        [ "module Prelude export (cachedValue) {"
        , "  import Support exposing (supportValue);"
        , "  def cachedValue : Int = supportValue;"
        , "}"
        ]

importedPreludeClientSource :: String
importedPreludeClientSource =
    unlines
        [ "module Support export (supportValue) {"
        , "  def supportValue : Int = 1;"
        , "}"
        , "module Main export (main) {"
        , "  import Prelude exposing (cachedValue);"
        , "  def main : Int = cachedValue;"
        , "}"
        ]

fakeSentinelPreludeSource :: String
fakeSentinelPreludeSource =
    unlines
        [ "module Prelude export (Token(..)) {"
        , "  data Token ="
        , "      Token : Token;"
        , "}"
        , "module Main export (main) {"
        , "  import Prelude exposing (Token(..));"
        , "  def main : Token = Token;"
        , "}"
        ]

intSourceType :: NormSrcType
intSourceType = STBase "Int"

externalOneExpr :: NormSurfaceExpr
externalOneExpr = EAnn (EVar "one") intSourceType

nodesOwnedByRoot :: Int -> RootOwnershipIndex -> IntSet.IntSet
nodesOwnedByRoot rootKey =
    IntMap.keysSet . IntMap.filter (IntSet.member rootKey) . roiNodeOwners

withBatchEquivalenceFile :: (FilePath -> IO a) -> IO a
withBatchEquivalenceFile =
    withProgramSourceFile "batch-equivalence" batchEquivalenceSource

withProgramSourceFile :: String -> String -> (FilePath -> IO a) -> IO a
withProgramSourceFile stem source =
    bracket setup removePathForcibly
  where
    setup = do
        tmpDir <- getTemporaryDirectory
        (path, handle) <- openTempFile tmpDir (stem ++ ".mlfp")
        hPutStr handle source
        hClose handle
        pure path

checkProgramArgsWithBatchSize ::
    Maybe String ->
    [String] ->
    IO (Either String String)
checkProgramArgsWithBatchSize mbBatchSize args =
    withEnv "MLF_MODULE_DEF_BATCH_SIZE" mbBatchSize $
        checkProgramArgs args

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action =
    bracket (lookupEnv name) restore (const (setRequested >> action))
  where
    setRequested =
        case value of
            Nothing -> unsetEnv name
            Just value0 -> setEnv name value0
    restore oldValue =
        case oldValue of
            Nothing -> unsetEnv name
            Just value0 -> setEnv name value0

requireParsed :: String -> IO P.Program
requireParsed input =
    case parseRawProgram input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program

requireLocatedWithFile :: FilePath -> String -> IO P.LocatedProgram
requireLocatedWithFile path input =
    case parseLocatedProgramWithFile path input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program

requireCheckedArtifactProgram :: IO CheckedProgram
requireCheckedArtifactProgram = do
    program <-
        requireParsed $
            unlines
                [ "module Main export (main) {"
                , "  def main : Bool = true;"
                , "}"
                ]
    requireRight (checkProgram program)

-- The artifact seam is intentionally pure and lazy. Keep this observation of
-- its evaluation count test-local: it verifies that both backend consumers
-- force the same stored preparation thunk without making counters part of the
-- production or test-support API.
countedIdentityPreparation :: IORef Int -> CheckedProgram -> Either String CheckedProgram
countedIdentityPreparation preparationCount checked =
    unsafePerformIO $ do
        atomicModifyIORef' preparationCount $ \count ->
            let nextCount = count + 1
             in (nextCount, ())
        pure (Right checked)
{-# NOINLINE countedIdentityPreparation #-}

checkedPreludeModule :: CheckedProgram -> CheckedModule
checkedPreludeModule =
    checkedModuleNamed "Prelude"

checkedModuleNamed :: P.ModuleName -> CheckedProgram -> CheckedModule
checkedModuleNamed moduleName0 checked =
    case [checkedModule | checkedModule <- checkedProgramModules checked, checkedModuleName checkedModule == moduleName0] of
        [checkedModule] -> checkedModule
        checkedModules ->
            error
                ( "expected one checked "
                    ++ moduleName0
                    ++ " module, got "
                    ++ show (length checkedModules)
                )

data IdentityOwner
    = GeneratedLocalBinderOwner String
    | GeneratedTypeBinderOwner String
    | GeneratedDeferredOwner String
    deriving (Eq, Ord, Show)

type IdentityOwners = Map.Map UniqueIdentity (Set.Set IdentityOwner)

checkedModuleConstructionIdentityOwners :: CheckedModule -> IdentityOwners
checkedModuleConstructionIdentityOwners =
    identityOwnerMap
        . concatMap checkedBindingConstructionIdentityOwners
        . checkedModuleBindings

checkedBindingConstructionIdentityOwners :: CheckedBinding -> [(UniqueIdentity, IdentityOwner)]
checkedBindingConstructionIdentityOwners binding =
    generatedTypeOwners (bindingOwner "resolved type") (Elab.resolvedVarType resolved)
        ++ generatedTypeViewOwners (bindingOwner "source type") (checkedBindingSourceTypeView binding)
        ++ generatedTypeOwners (bindingOwner "checked type") (checkedBindingType binding)
        ++ generatedTermOwners bindingName (checkedBindingTerm binding)
        ++ map
            (\ref -> (deferredRefIdentity ref, bindingDeferredOwner ref))
            ( Map.keys (checkedBindingDeferredObligations binding)
                ++ map deferredProgramObligationRef (Map.elems (checkedBindingDeferredObligations binding))
            )
  where
    resolved = checkedBindingResolvedVar binding
    bindingName = Elab.resolvedVarRuntimeName resolved
    bindingOwner location =
        GeneratedTypeBinderOwner (bindingName ++ " " ++ location)
    bindingDeferredOwner ref =
        GeneratedDeferredOwner
            (bindingName ++ " deferred " ++ show (deferredRefIdentity ref))

generatedTermOwners :: String -> Elab.XmlfTerm -> [(UniqueIdentity, IdentityOwner)]
generatedTermOwners bindingName term =
    case term of
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.ELam resolved body ->
            generatedResolvedLocalOwner bindingName resolved
                ++ generatedTermOwners bindingName body
        Elab.EApp fun arg ->
            generatedTermOwners bindingName fun
                ++ generatedTermOwners bindingName arg
        Elab.ELet resolved scheme rhs body ->
            generatedResolvedLocalOwner bindingName resolved
                ++ concatMap
                    (generatedTypeBinderRefOwner (GeneratedTypeBinderOwner (bindingName ++ " let scheme")) . fst)
                    (Elab.schemeBinderRefs scheme)
                ++ concatMap
                    (maybe [] (generatedTypeOwners (GeneratedTypeBinderOwner (bindingName ++ " let bound"))) . snd)
                    (Elab.schemeBinderRefs scheme)
                ++ generatedTypeOwners
                    (GeneratedTypeBinderOwner (bindingName ++ " let body"))
                    (Elab.schemeBody scheme)
                ++ generatedTermOwners bindingName rhs
                ++ generatedTermOwners bindingName body
        Elab.ETyAbsRef ref mbBound body ->
            generatedTypeBinderRefOwner
                (GeneratedTypeBinderOwner (bindingName ++ " type abstraction"))
                ref
                ++ maybe
                    []
                    (generatedTypeOwners (GeneratedTypeBinderOwner (bindingName ++ " type abstraction bound")))
                    mbBound
                ++ generatedTermOwners bindingName body
        Elab.ETyInst body _ -> generatedTermOwners bindingName body
        Elab.ERoll _ body -> generatedTermOwners bindingName body
        Elab.EUnroll body -> generatedTermOwners bindingName body

generatedResolvedLocalOwner :: String -> Elab.ResolvedVar -> [(UniqueIdentity, IdentityOwner)]
generatedResolvedLocalOwner bindingName resolved =
    case Elab.resolvedVarLocalRef resolved of
        Nothing -> []
        Just ref ->
            case localRefIdentity ref of
                GeneratedGraphLocalId unique _ -> owned unique
                GeneratedLocalId unique -> owned unique
                GraphLocalId{} -> []
                ScopedGraphLocalId{} -> []
  where
    owned unique =
        [ ( unique
          , GeneratedLocalBinderOwner
                (bindingName ++ " local " ++ Elab.resolvedVarRuntimeName resolved)
          )
        ]

generatedTypeOwners :: IdentityOwner -> Elab.Ty v -> [(UniqueIdentity, IdentityOwner)]
generatedTypeOwners owner ty =
    case ty of
        Elab.TVarRef{} -> []
        Elab.TArrow arg result ->
            generatedTypeOwners owner arg ++ generatedTypeOwners owner result
        Elab.TConWithIdentity _ _ args ->
            foldMap (generatedTypeOwners owner) args
        Elab.TVarAppRef _ args ->
            foldMap (generatedTypeOwners owner) args
        Elab.TBaseWithIdentity{} -> []
        Elab.TForallRef ref mbBound body ->
            generatedTypeBinderRefOwner owner ref
                ++ foldMap (generatedTypeOwners owner) mbBound
                ++ generatedTypeOwners owner body
        Elab.TMuRef ref body ->
            generatedTypeBinderRefOwner owner ref
                ++ generatedTypeOwners owner body
        Elab.TBottom -> []

generatedTypeViewOwners :: IdentityOwner -> TypeView -> [(UniqueIdentity, IdentityOwner)]
generatedTypeViewOwners owner view =
    case typeViewNodeView view of
        TypeViewVarNode{} -> []
        TypeViewArrowNode arg result ->
            generatedTypeViewOwners owner arg
                ++ generatedTypeViewOwners owner result
        TypeViewBaseNode{} -> []
        TypeViewConNode _ _ args ->
            foldMap (generatedTypeViewOwners owner) args
        TypeViewVarAppNode _ _ args ->
            foldMap (generatedTypeViewOwners owner) args
        TypeViewTyLamNode _ identity body ->
            generatedTypeBinderIdentityOwner owner identity
                ++ generatedTypeViewOwners owner body
        TypeViewTyAppNode fun arg ->
            generatedTypeViewOwners owner fun
                ++ generatedTypeViewOwners owner arg
        TypeViewForallNode _ identity mbBound body ->
            generatedTypeBinderIdentityOwner owner identity
                ++ foldMap (generatedTypeViewOwners owner) mbBound
                ++ generatedTypeViewOwners owner body
        TypeViewMuNode _ identity body ->
            generatedTypeBinderIdentityOwner owner identity
                ++ generatedTypeViewOwners owner body
        TypeViewBottomNode -> []

generatedTypeBinderRefOwner :: IdentityOwner -> Elab.TypeBinderRef -> [(UniqueIdentity, IdentityOwner)]
generatedTypeBinderRefOwner owner ref =
    generatedTypeBinderIdentityOwner owner (Elab.typeBinderRefIdentity ref)

generatedTypeBinderIdentityOwner :: IdentityOwner -> Elab.TypeBinderIdentity -> [(UniqueIdentity, IdentityOwner)]
generatedTypeBinderIdentityOwner owner identity =
    case typeBinderIdentityGeneratedUnique identity of
        Nothing -> []
        Just unique -> [(unique, owner)]

identityOwnerMap :: [(UniqueIdentity, IdentityOwner)] -> IdentityOwners
identityOwnerMap owners =
    Map.fromListWith Set.union
        [ (unique, Set.singleton owner)
        | (unique, owner) <- owners
        ]

withFreshBuiltinPreludeCheckCache ::
    (PreludeCacheTestSupport.BuiltinPreludeCheckCacheHandle -> IO a) ->
    IO a
withFreshBuiltinPreludeCheckCache action =
    PreludeCacheTestSupport.newBuiltinPreludeCheckCache >>= action

withFreshBuiltinPreludeCheckProbeCache ::
    (PreludeCacheTestSupport.BuiltinPreludeCheckProbeHandle -> IO a) ->
    IO a
withFreshBuiltinPreludeCheckProbeCache action =
    PreludeCacheTestSupport.newBuiltinPreludeCheckProbeCache >>= action

checkWithBuiltinPrelude ::
    PreludeCacheTestSupport.BuiltinPreludeCheckCacheHandle ->
    Int ->
    IO CheckedProgram
checkWithBuiltinPrelude cacheHandle value =
    checkWithCustomBuiltinPrelude
        cacheHandle
        (cacheProbePreludeSource value)
        cacheProbeClientSource

cacheProbePreludePackage :: IO LocatedProgramPackage
cacheProbePreludePackage =
    customBuiltinPreludePackage
        (cacheProbePreludeSource 1)
        cacheProbeClientSource

checkWithCustomBuiltinPrelude ::
    PreludeCacheTestSupport.BuiltinPreludeCheckCacheHandle ->
    String ->
    String ->
    IO CheckedProgram
checkWithCustomBuiltinPrelude cacheHandle preludeSource0 clientSource = do
    package <- customBuiltinPreludePackage preludeSource0 clientSource
    requireRight
        ( PreludeCacheTestSupport.checkLocatedProgramPackageWithCache
            cacheHandle
            package
        )

overlappingCacheProbe ::
    PreludeCacheTestSupport.BuiltinPreludeCheckProbeHandle ->
    ResolvedSemanticModule ->
    MVar () ->
    MVar () ->
    MVar () ->
    MVar () ->
    IO Int
overlappingCacheProbe cacheHandle resolvedPrelude ready start checked readCount = do
    putMVar ready ()
    ( do
        takeMVar start
        PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe cacheHandle resolvedPrelude
        PreludeCacheTestSupport.cacheBuiltinPreludeCheckProbe cacheHandle resolvedPrelude
      )
        `finally` putMVar checked ()
    takeMVar readCount
    PreludeCacheTestSupport.readBuiltinPreludeCheckProbeBuildCount cacheHandle

resolvedBuiltinPreludeCacheProbe :: String -> IO ResolvedSemanticModule
resolvedBuiltinPreludeCacheProbe source = do
    program <- requireParsed source
    normalized <- requireRight (normalizeTypeFamiliesInProgram program)
    resolved <- requireRight (resolveProgram normalized)
    case
        [ resolvedModule
        | resolvedModule <-
            resolvedSemanticProgramModules
                (resolvedProgramSemanticArtifact resolved)
        , resolvedSemanticModuleName resolvedModule == "Prelude"
        ] of
        [resolvedPrelude] -> pure resolvedPrelude
        resolvedModules ->
            expectationFailure
                ( "expected one resolved Prelude cache probe, got "
                    ++ show (length resolvedModules)
                )
                >> fail "missing resolved Prelude cache probe"

resolvedBuiltinPreludeFromPackage :: LocatedProgramPackage -> IO ResolvedSemanticModule
resolvedBuiltinPreludeFromPackage package = do
    ordered <- requireRight (locatedProgramPackageOrderedProgram package)
    normalized <- requireRight (normalizeTypeFamiliesInProgram (P.locatedProgram ordered))
    resolved <- requireRight (resolveProgram normalized)
    case
        [ resolvedModule
        | resolvedModule <-
            resolvedSemanticProgramModules
                (resolvedProgramSemanticArtifact resolved)
        , resolvedSemanticModuleName resolvedModule == "Prelude"
        ] of
        [resolvedPrelude] -> pure resolvedPrelude
        resolvedModules ->
            expectationFailure
                ( "expected one ordered Prelude cache probe, got "
                    ++ show (length resolvedModules)
                )
                >> fail "missing ordered Prelude cache probe"

requireThreadResult :: Either SomeException a -> IO a
requireThreadResult =
    either throwIO pure

waitUntilBlockedOnMVar :: ThreadId -> IO ()
waitUntilBlockedOnMVar threadId = do
    status <- threadStatus threadId
    case status of
        ThreadBlocked BlockedOnMVar -> pure ()
        ThreadFinished ->
            expectationFailure "cache lookup finished before the in-flight build was released"
        ThreadDied ->
            expectationFailure "cache lookup thread died before the in-flight build was released"
        _ ->
            yield >> waitUntilBlockedOnMVar threadId

requireWithin :: String -> IO a -> IO a
requireWithin message action = do
    result <- timeout 5000000 action
    case result of
        Just value -> pure value
        Nothing -> expectationFailure message >> fail message

customBuiltinPreludePackage :: String -> String -> IO LocatedProgramPackage
customBuiltinPreludePackage preludeSource0 clientSource = do
    prelude <- requireLocatedWithFile preludeSourcePath preludeSource0
    mainModule <- requireLocatedWithFile "cache-probe-main.mlfp" clientSource
    pure
        LocatedProgramPackage
            { locatedProgramPackageId = trivialPackageId
            , locatedProgramPackageSourceUnits =
                [ PreludeTestSupport.customBuiltinPreludeLocatedProgramSourceUnit prelude
                , locatedProgramSourceUnitFromLocated mainModule
                ]
            }

builtinPreludeOrderProbePackage :: Bool -> IO LocatedProgramPackage
builtinPreludeOrderProbePackage preludeFirst = do
    prelude <- requireLocatedWithFile preludeSourcePath (cacheProbePreludeSource 1)
    support <-
        requireLocatedWithFile
            "cache-probe-support.mlfp"
            ( unlines
                [ "module Support export (supportValue) {"
                , "  def supportValue : Int = 1;"
                , "}"
                ]
            )
    mainModule <- requireLocatedWithFile "cache-probe-main.mlfp" cacheProbeClientSource
    let preludeUnit = PreludeTestSupport.customBuiltinPreludeLocatedProgramSourceUnit prelude
        supportUnit = locatedProgramSourceUnitFromLocated support
        sourceUnits
            | preludeFirst = [preludeUnit, supportUnit]
            | otherwise = [supportUnit, preludeUnit]
    pure
        LocatedProgramPackage
            { locatedProgramPackageId = trivialPackageId
            , locatedProgramPackageSourceUnits =
                sourceUnits ++ [locatedProgramSourceUnitFromLocated mainModule]
            }

requireGraphNode :: P.ModuleName -> PackageModuleGraph -> IO PackageModuleGraphNode
requireGraphNode moduleName0 graph =
    case
        [ node
        | node <- packageModuleGraphNodes graph
        , packageModuleName (packageModuleGraphNodeId node) == moduleName0
        ] of
        [node] -> pure node
        nodes -> expectationFailure ("expected one graph node for " ++ moduleName0 ++ ", got " ++ show (length nodes)) >> fail "missing graph node"

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value
