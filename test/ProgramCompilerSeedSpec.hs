module ProgramCompilerSeedSpec (spec) where

import Data.List (isInfixOf)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

import CheckedProgramTestSupport
    ( CheckedProgramArtifact
    , checkedArtifactBackendLLVM
    , checkedArtifactCheckOutput
    , checkedArtifactNativeLLVM
    , checkedArtifactRunOutput
    , checkedProgramArtifactFromLocatedPackage
    )
import LLVMToolSupport
    ( NativeRunResult (..)
    , runLLVMNativeExecutable
    , validateLLVMAssembly
    , validateLLVMObjectCode
    )
import MLF.Frontend.Program.Package
    ( PackageId (..)
    , PackageModuleId (..)
    , PackageModuleGraph
    , discoverLocatedProgramPackage
    , locatedProgramPackageModuleGraph
    , packageModuleGraphNodes
    , packageModuleGraphNodeSourcePath
    , packageModuleGraphOrder
    )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackage)

spec :: Spec
spec =
    describe "MLF.Program compiler frontend seed package contract" $
        beforeAll loadCompilerSeedFixture $ do
            it "compiler-seed discovers, checks, and runs the fixture as an ordinary package root" $ \fixture -> do
                packageModuleGraphOrder (compilerSeedGraph fixture)
                    `shouldBe`
                        [ PackageModuleId compilerSeedPackageId "Prelude"
                        , PackageModuleId compilerSeedPackageId "SeedContract"
                        , PackageModuleId compilerSeedPackageId "SeedSource"
                        , PackageModuleId compilerSeedPackageId "SeedToken"
                        , PackageModuleId compilerSeedPackageId "SeedDiagnostic"
                        , PackageModuleId compilerSeedPackageId "SeedLexer"
                        , PackageModuleId compilerSeedPackageId "SeedAst"
                        , PackageModuleId compilerSeedPackageId "SeedParser"
                        , PackageModuleId compilerSeedPackageId "Main"
                        ]
                map packageModuleGraphNodeSourcePath (packageModuleGraphNodes (compilerSeedGraph fixture))
                    `shouldMatchList`
                        [ Just "<mlfp-prelude>"
                        , Just (compilerSeedRoot </> "Main.mlfp")
                        , Just (compilerSeedRoot </> "SeedAst.mlfp")
                        , Just (compilerSeedRoot </> "SeedContract.mlfp")
                        , Just (compilerSeedRoot </> "SeedDiagnostic.mlfp")
                        , Just (compilerSeedRoot </> "SeedLexer.mlfp")
                        , Just (compilerSeedRoot </> "SeedParser.mlfp")
                        , Just (compilerSeedRoot </> "SeedSource.mlfp")
                        , Just (compilerSeedRoot </> "SeedToken.mlfp")
                        ]
                checkedArtifactRunOutput (compilerSeedArtifact fixture)
                    `shouldBe` Right compilerSeedFrontendEvidenceOutput

            it "compiler-seed reuses the checked fixture artifact for check and run" $ \fixture -> do
                checkedArtifactCheckOutput (compilerSeedArtifact fixture) `shouldBe` Right "OK\n"
                checkedArtifactRunOutput (compilerSeedArtifact fixture) `shouldBe` Right compilerSeedFrontendEvidenceOutput

            it "compiler-seed emits backend and native LLVM without changing the seed contract" $ \fixture -> do
                backendOutput <- requireRight (checkedArtifactBackendLLVM (compilerSeedArtifact fixture))

                backendOutput `shouldSatisfy` isInfixOf "define ptr @\"SeedLexer__lexSeedInput\""
                backendOutput `shouldSatisfy` isInfixOf "define ptr @\"SeedParser__parseSeedTokens\""
                backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\""
                backendOutput `shouldSatisfy` isInfixOf "define private ptr @\"__io_bind.wrapper\""
                backendOutput `shouldSatisfy` isInfixOf "define private ptr @\"__io_putStrLn.wrapper\""
                validateLLVMAssembly backendOutput

                nativeOutput <- requireRight (checkedArtifactNativeLLVM (compilerSeedArtifact fixture))

                nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
                nativeOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\""
                nativeOutput `shouldSatisfy` isInfixOf "define private ptr @\"__io_bind.wrapper\""
                nativeOutput `shouldSatisfy` isInfixOf "define private ptr @\"__io_putStrLn.wrapper\""
                validateLLVMAssembly nativeOutput
                validateLLVMObjectCode nativeOutput
                runLLVMNativeExecutable nativeOutput
                    `shouldReturn` NativeRunResult ExitSuccess compilerSeedFrontendEvidenceOutput ""

data CompilerSeedFixture = CompilerSeedFixture
    { compilerSeedGraph :: PackageModuleGraph
    , compilerSeedArtifact :: CheckedProgramArtifact
    }

loadCompilerSeedFixture :: IO CompilerSeedFixture
loadCompilerSeedFixture = do
    discovered <- requireRight =<< discoverLocatedProgramPackage compilerSeedPackageId compilerSeedRoot
    let package = withPreludeLocatedPackage discovered
    graph <- requireRight (locatedProgramPackageModuleGraph package)
    artifact <- requireRight (checkedProgramArtifactFromLocatedPackage package)
    pure
        CompilerSeedFixture
            { compilerSeedGraph = graph
            , compilerSeedArtifact = artifact
            }

compilerSeedPackageId :: PackageId
compilerSeedPackageId = PackageId "compiler-frontend-seed"

compilerSeedRoot :: FilePath
compilerSeedRoot = "test/programs/compiler-seed/frontend-contract"

compilerSeedFrontendEvidenceOutput :: String
compilerSeedFrontendEvidenceOutput =
    unlines
        [ "lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol"
        , "parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true"
        ]

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value
