module ProgramCompilerSeedSpec (spec) where

import System.FilePath ((</>))
import Test.Hspec

import MLF.Frontend.Program.Check (checkLocatedProgramPackage)
import MLF.Frontend.Program.Package
    ( PackageId (..)
    , PackageModuleId (..)
    , discoverLocatedProgramPackage
    , locatedProgramPackageModuleGraph
    , packageModuleGraphNodes
    , packageModuleGraphNodeSourcePath
    , packageModuleGraphOrder
    )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackage)
import MLF.Frontend.Program.Run
    ( programRunOutput
    , runLocatedProgramPackageOutput
    )
import MLF.Program.CLI
    ( checkProgramArgs
    , runProgramArgs
    )

spec :: Spec
spec =
    describe "MLF.Program compiler frontend seed package contract" $ do
        it "compiler-seed discovers, checks, and runs the fixture as an ordinary package root" $ do
            discovered <- requireRight =<< discoverLocatedProgramPackage compilerSeedPackageId compilerSeedRoot
            let package = withPreludeLocatedPackage discovered
            graph <- requireRight (locatedProgramPackageModuleGraph package)

            packageModuleGraphOrder graph
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
            map packageModuleGraphNodeSourcePath (packageModuleGraphNodes graph)
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
            _checked <- requireRight (checkLocatedProgramPackage package)
            result <- requireRight (runLocatedProgramPackageOutput package)

            programRunOutput result `shouldBe` compilerSeedFrontendEvidenceOutput

        it "compiler-seed runs the fixture through the public CLI package entrypoint" $ do
            checkProgramArgs [compilerSeedRoot] `shouldReturn` Right "OK\n"
            runProgramArgs [compilerSeedRoot] `shouldReturn` Right compilerSeedFrontendEvidenceOutput

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
