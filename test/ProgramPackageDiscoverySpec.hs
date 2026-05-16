module ProgramPackageDiscoverySpec (spec) where

import Control.Exception (bracket)
import Data.Either (isRight)
import Data.List (isInfixOf)
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

import MLF.Frontend.Program.Check (checkLocatedProgramPackage)
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage
    , PackageId (..)
    , PackageModuleId (..)
    , discoverLocatedProgramPackage
    , locatedProgramPackageModuleGraph
    , packageModuleGraphOrder
    )
import MLF.Frontend.Program.Types
    ( ProgramDiagnostic (..)
    , ProgramError (..)
    , renderProgramDiagnostic
    )

spec :: Spec
spec =
    describe "MLF.Program package filesystem discovery" $ do
        it "discovers a package root across files and checks cross-file imports" $
            withTempPackageRoot $ \root -> do
                _ <- writePackageFile root "Lib.mlfp" libSource
                _ <- writePackageFile root "Main.mlfp" mainImportsLibSource

                package <- requireDiscovered root

                checkLocatedProgramPackage package `shouldSatisfy` isRight

        it "orders imported modules before importers when file discovery order differs" $
            withTempPackageRoot $ \root -> do
                _ <- writePackageFile root "a-main.mlfp" mainImportsLibSource
                _ <- writePackageFile root "z-lib.mlfp" libSource

                package <- requireDiscovered root
                graph <- requireRight (locatedProgramPackageModuleGraph package)

                packageModuleGraphOrder graph
                    `shouldBe`
                        [ PackageModuleId testPackageId "Lib"
                        , PackageModuleId testPackageId "Main"
                        ]

        it "renders missing cross-file import diagnostics at the importing file" $
            withTempPackageRoot $ \root -> do
                mainPath <-
                    writePackageFile
                        root
                        "Main.mlfp"
                        ( unlines
                            [ "module Main export (main) {"
                            , "  import Missing;"
                            , "  def main : Bool = true;"
                            , "}"
                            ]
                        )

                package <- requireDiscovered root

                case checkLocatedProgramPackage package of
                    Left diagnostic -> do
                        diagnosticError diagnostic `shouldBe` ProgramUnknownImportModule "Missing"
                        renderProgramDiagnostic diagnostic
                            `shouldSatisfy` isInfixOf (mainPath ++ ":2:10")
                    Right _ -> expectationFailure "expected missing import diagnostic"

        it "rejects module cycles with a stable cycle diagnostic and source path" $
            withTempPackageRoot $ \root -> do
                aPath <-
                    writePackageFile
                        root
                        "A.mlfp"
                        ( unlines
                            [ "module A export () {"
                            , "  import B;"
                            , "}"
                            ]
                        )
                _ <-
                    writePackageFile
                        root
                        "B.mlfp"
                        ( unlines
                            [ "module B export () {"
                            , "  import A;"
                            , "}"
                            ]
                        )

                package <- requireDiscovered root

                case checkLocatedProgramPackage package of
                    Left diagnostic -> do
                        diagnosticError diagnostic `shouldBe` ProgramImportCycle ["A", "B", "A"]
                        renderProgramDiagnostic diagnostic
                            `shouldSatisfy` isInfixOf (aPath ++ ":1:1")
                    Right _ -> expectationFailure "expected import cycle diagnostic"

        it "keeps import export-visibility diagnostics at the exposing item across files" $
            withTempPackageRoot $ \root -> do
                _ <-
                    writePackageFile
                        root
                        "Lib.mlfp"
                        ( unlines
                            [ "module Lib export () {"
                            , "  data Nat ="
                            , "      Zero : Nat;"
                            , "}"
                            ]
                        )
                mainPath <-
                    writePackageFile
                        root
                        "Main.mlfp"
                        ( unlines
                            [ "module Main export (main) {"
                            , "  import Lib exposing (Nat);"
                            , "  def main : Bool = true;"
                            , "}"
                            ]
                        )

                package <- requireDiscovered root

                case checkLocatedProgramPackage package of
                    Left diagnostic -> do
                        diagnosticError diagnostic `shouldBe` ProgramImportNotExported "Lib" "Nat"
                        renderProgramDiagnostic diagnostic
                            `shouldSatisfy` isInfixOf (mainPath ++ ":2:24")
                    Right _ -> expectationFailure "expected import visibility diagnostic"

testPackageId :: PackageId
testPackageId = PackageId "test-package"

withTempPackageRoot :: (FilePath -> IO a) -> IO a
withTempPackageRoot =
    bracket createTempPackageRoot removeDirectoryRecursive

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

requireDiscovered :: FilePath -> IO LocatedProgramPackage
requireDiscovered root =
    requireRight =<< discoverLocatedProgramPackage testPackageId root

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value

libSource :: String
libSource =
    unlines
        [ "module Lib export (one) {"
        , "  def one : Int = 1;"
        , "}"
        ]

mainImportsLibSource :: String
mainImportsLibSource =
    unlines
        [ "module Main export (main) {"
        , "  import Lib exposing (one);"
        , "  def main : Int = one;"
        , "}"
        ]
