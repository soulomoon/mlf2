{-# LANGUAGE LambdaCase #-}

module ProgramPackageBuildGraphSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.Either (isRight)
import Data.List (find, isInfixOf)
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, openTempFile)
import Test.Hspec

import MLF.Frontend.Parse.Program
    ( parseRawProgram
    , renderProgramParseError
    )
import MLF.Frontend.Program.BuildGraph
    ( PackageBuildCacheEntry (..)
    , PackageBuildCacheValidationError (..)
    , PackageBuildGraph
    , PackageBuildGraphError (..)
    , PackageBuildGraphNode (..)
    , moduleInterfaceSummaryMetadata
    , packageBuildCacheEntries
    , packageBuildGraphNodeById
    , packageBuildGraphOrder
    , programPackageBuildGraphWithInterfaces
    , renderPackageBuildCacheValidationError
    , validatePackageBuildCacheEntry
    , validatePackageBuildCacheEntryFor
    )
import MLF.Frontend.Program.Check
    ( checkLocatedProgramPackage
    , checkProgramPackage
    )
import MLF.Frontend.Program.Interface
    ( ModuleInterface (..)
    , PackageInterface (..)
    , ProgramInterfaceError (..)
    , packageInterfaceFromCheckedProgram
    , packageInterfaceModuleById
    )
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage
    , PackageId (..)
    , PackageModuleId (..)
    , PackageRoot (..)
    , PackageSearchPath (..)
    , ProgramPackage (..)
    , ProgramPackageDiscoveryError (..)
    , ProgramSourceUnit (..)
    , discoverLocatedProgramPackageFromSearchPath
    , locatedProgramPackagePackage
    , programPackageModuleGraph
    )
import MLF.Frontend.Program.Types
    ( ProgramDiagnostic (..)
    , ProgramError (..)
    , renderProgramDiagnostic
    )
import MLF.Frontend.Syntax.Program qualified as P

spec :: Spec
spec =
    describe "MLF.Program package build graph policy" $ do
        it "resolves explicit local roots through an ordered search path" $
            withTempPackageRoots 2 $ \case
                [mainRoot, libRoot] -> do
                    _ <- writePackageFile mainRoot "Main.mlfp" mainSource
                    _ <- writePackageFile libRoot "Lib.mlfp" libSource

                    package <-
                        requireDiscovered
                            (PackageSearchPath [PackageRoot mainRoot, PackageRoot libRoot])
                    (graph, _packageInterface) <-
                        requireCheckedBuildGraph (locatedProgramPackagePackage package)

                    packageBuildGraphOrder graph `shouldBe` [libId, mainId]
                    checkLocatedProgramPackage package `shouldSatisfy` isRight
                roots -> expectationFailure ("expected two roots, got " ++ show roots)

        it "fails missing search-path roots at the importing source path" $
            withTempPackageRoots 2 $ \case
                [mainRoot, _libRoot] -> do
                    mainPath <- writePackageFile mainRoot "Main.mlfp" mainSource

                    package <-
                        requireDiscovered
                            (PackageSearchPath [PackageRoot mainRoot])

                    case checkLocatedProgramPackage package of
                        Left diagnostic -> do
                            diagnosticError diagnostic `shouldBe` ProgramUnknownImportModule "Lib"
                            renderProgramDiagnostic diagnostic
                                `shouldSatisfy` isInfixOf (mainPath ++ ":2:10")
                        Right _ -> expectationFailure "expected missing import diagnostic"
                roots -> expectationFailure ("expected two roots, got " ++ show roots)

        it "rejects duplicate modules across searched roots with source paths" $
            withTempPackageRoots 2 $ \case
                [leftRoot, rightRoot] -> do
                    leftPath <- writePackageFile leftRoot "Lib.mlfp" libSource
                    rightPath <- writePackageFile rightRoot "nested/Lib.mlfp" libSource

                    result <-
                        discoverLocatedProgramPackageFromSearchPath
                            testPackageId
                            (PackageSearchPath [PackageRoot leftRoot, PackageRoot rightRoot])

                    case result of
                        Left (ProgramPackageDiscoveryDuplicateModule moduleName0 sourcePaths) -> do
                            moduleName0 `shouldBe` "Lib"
                            sourcePaths `shouldContain` [leftPath]
                            sourcePaths `shouldContain` [rightPath]
                        Left err -> expectationFailure ("unexpected discovery error " ++ show err)
                        Right _ -> expectationFailure "expected duplicate module discovery error"
                roots -> expectationFailure ("expected two roots, got " ++ show roots)

        it "builds graph nodes with source metadata and dependency interface metadata" $ do
            (graph, packageInterface) <- requireCheckedBuildGraph interfacePackage
            libInterface <- requireInterface libId packageInterface
            libNode <- requireBuildGraphNode libId graph
            mainNode <- requireBuildGraphNode mainId graph

            packageBuildGraphNodeSourcePath libNode `shouldBe` Just "src/Lib.mlfp"
            packageBuildGraphNodeDirectImports libNode `shouldBe` []
            packageBuildGraphNodeSourcePath mainNode `shouldBe` Just "app/Main.mlfp"
            packageBuildGraphNodeDirectImports mainNode `shouldBe` [libId]
            packageBuildGraphNodeSourceMetadata libNode
                `shouldNotBe` packageBuildGraphNodeSourceMetadata mainNode
            packageBuildGraphNodeDependencyInterfaceMetadata mainNode
                `shouldBe` [(libId, moduleInterfaceSummaryMetadata libInterface)]

        it "accepts fresh cache entries derived from the current graph and interfaces" $ do
            (graph, packageInterface) <- requireCheckedBuildGraph interfacePackage
            cacheEntries <- requireRight (packageBuildCacheEntries graph packageInterface)

            mapM_
                ( \entry ->
                    validatePackageBuildCacheEntry graph packageInterface entry
                        `shouldBe` Right ()
                )
                cacheEntries

        it "rejects stale source metadata for the changed module" $ do
            (oldGraph, oldInterface) <- requireCheckedBuildGraph interfacePackage
            oldEntries <- requireRight (packageBuildCacheEntries oldGraph oldInterface)
            oldLibEntry <- requireCacheEntry libId oldEntries
            (newGraph, newInterface) <-
                requireCheckedBuildGraph
                    ( packageFromSourceUnits
                        [ ("src/Lib.mlfp", libSourceChangedBody)
                        , ("app/Main.mlfp", mainSource)
                        ]
                    )

            case validatePackageBuildCacheEntryFor libId newGraph newInterface oldLibEntry of
                Left err@PackageBuildCacheSourceMetadataMismatch {} ->
                    renderPackageBuildCacheValidationError err
                        `shouldSatisfy` isInfixOf "test-package:Lib"
                other -> expectationFailure ("expected stale Lib source metadata, got " ++ show other)

        it "rejects importers when dependency interface summary metadata changes" $ do
            (oldGraph, oldInterface) <- requireCheckedBuildGraph interfacePackage
            oldEntries <- requireRight (packageBuildCacheEntries oldGraph oldInterface)
            oldMainEntry <- requireCacheEntry mainId oldEntries
            (newGraph, newInterface) <-
                requireCheckedBuildGraph
                    ( packageFromSourceUnits
                        [ ("src/Lib.mlfp", libSourceWithExtraExport)
                        , ("app/Main.mlfp", mainSource)
                        ]
                    )

            case validatePackageBuildCacheEntryFor mainId newGraph newInterface oldMainEntry of
                Left err@(PackageBuildCacheDependencyInterfaceMetadataMismatch moduleId dependency _ _) -> do
                    moduleId `shouldBe` mainId
                    dependency `shouldBe` libId
                    let rendered = renderPackageBuildCacheValidationError err
                    rendered `shouldSatisfy` isInfixOf "test-package:Main"
                    rendered `shouldSatisfy` isInfixOf "test-package:Lib"
                other ->
                    expectationFailure ("expected stale dependency interface metadata, got " ++ show other)

        it "rejects malformed interfaces at the build graph boundary" $ do
            (_graph, packageInterface) <- requireCheckedBuildGraph interfacePackage
            libInterface <- requireInterface libId packageInterface
            mainInterface <- requireInterface mainId packageInterface
            let staleInterface =
                    PackageInterface
                        [ libInterface
                        , mainInterface {moduleInterfaceDependencies = []}
                        ]

            programPackageBuildGraphWithInterfaces interfacePackage staleInterface
                `shouldBe`
                    Left
                        ( PackageBuildGraphInterfaceError
                            (ProgramInterfaceDependenciesMismatch mainId [libId] [])
                        )

testPackageId :: PackageId
testPackageId = PackageId "test-package"

libId :: PackageModuleId
libId = PackageModuleId testPackageId "Lib"

mainId :: PackageModuleId
mainId = PackageModuleId testPackageId "Main"

interfacePackage :: ProgramPackage
interfacePackage =
    packageFromSourceUnits
        [ ("src/Lib.mlfp", libSource)
        , ("app/Main.mlfp", mainSource)
        ]

libSource :: String
libSource =
    unlines
        [ "module Lib export (one) {"
        , "  def one : Int = 1;"
        , "}"
        ]

libSourceChangedBody :: String
libSourceChangedBody =
    unlines
        [ "module Lib export (one) {"
        , "  def one : Int = 2;"
        , "}"
        ]

libSourceWithExtraExport :: String
libSourceWithExtraExport =
    unlines
        [ "module Lib export (one, two) {"
        , "  def one : Int = 1;"
        , "  def two : Int = 2;"
        , "}"
        ]

mainSource :: String
mainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Lib exposing (one);"
        , "  def main : Int = one;"
        , "}"
        ]

packageFromSourceUnits :: [(FilePath, String)] -> ProgramPackage
packageFromSourceUnits units =
    ProgramPackage
        { programPackageId = testPackageId
        , programPackageSourceUnits =
            [ ProgramSourceUnit
                { programSourceUnitPath = Just path
                , programSourceUnitModules = P.programModules (parseProgram source)
                }
            | (path, source) <- units
            ]
        }

requireCheckedBuildGraph ::
    ProgramPackage ->
    IO (PackageBuildGraph, PackageInterface)
requireCheckedBuildGraph package = do
    graph <- requireRight (programPackageModuleGraph package)
    checked <- requireRight (checkProgramPackage package)
    packageInterface <- requireRight (packageInterfaceFromCheckedProgram graph checked)
    buildGraph <- requireRight (programPackageBuildGraphWithInterfaces package packageInterface)
    pure (buildGraph, packageInterface)

requireInterface :: PackageModuleId -> PackageInterface -> IO ModuleInterface
requireInterface moduleId packageInterface =
    case packageInterfaceModuleById moduleId packageInterface of
        Just interface -> pure interface
        Nothing -> expectationFailure ("missing interface " ++ show moduleId) >> fail "missing interface"

requireBuildGraphNode :: PackageModuleId -> PackageBuildGraph -> IO PackageBuildGraphNode
requireBuildGraphNode moduleId graph =
    case packageBuildGraphNodeById moduleId graph of
        Just node -> pure node
        Nothing -> expectationFailure ("missing build graph node " ++ show moduleId) >> fail "missing node"

requireCacheEntry ::
    PackageModuleId ->
    [PackageBuildCacheEntry] ->
    IO PackageBuildCacheEntry
requireCacheEntry moduleId entries =
    case find ((== moduleId) . packageBuildCacheEntryModuleId) entries of
        Just entry -> pure entry
        Nothing -> expectationFailure ("missing cache entry " ++ show moduleId) >> fail "missing cache entry"

requireDiscovered :: PackageSearchPath -> IO LocatedProgramPackage
requireDiscovered searchPath =
    requireRight =<< discoverLocatedProgramPackageFromSearchPath testPackageId searchPath

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value

parseProgram :: String -> P.Program
parseProgram source =
    case parseRawProgram source of
        Left err -> error (renderProgramParseError err)
        Right program -> program

withTempPackageRoots :: Int -> ([FilePath] -> IO a) -> IO a
withTempPackageRoots count action =
    bracket
        (replicateM count createTempPackageRoot)
        (mapM_ removeDirectoryRecursive)
        action

createTempPackageRoot :: IO FilePath
createTempPackageRoot = do
    tempDir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempDir "mlf2-package-root"
    hClose handle
    removeFile path
    createDirectory path
    pure path

writePackageFile :: FilePath -> FilePath -> String -> IO FilePath
writePackageFile root relativePath contents = do
    let path = root </> relativePath
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path contents
    pure path
