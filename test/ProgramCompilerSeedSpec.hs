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
            package <- requireRight =<< discoverLocatedProgramPackage compilerSeedPackageId compilerSeedRoot
            graph <- requireRight (locatedProgramPackageModuleGraph package)

            packageModuleGraphOrder graph
                `shouldBe`
                    [ PackageModuleId compilerSeedPackageId "SeedContract"
                    , PackageModuleId compilerSeedPackageId "Main"
                    ]
            map packageModuleGraphNodeSourcePath (packageModuleGraphNodes graph)
                `shouldMatchList`
                    [ Just (compilerSeedRoot </> "Main.mlfp")
                    , Just (compilerSeedRoot </> "SeedContract.mlfp")
                    ]
            _checked <- requireRight (checkLocatedProgramPackage package)
            result <- requireRight (runLocatedProgramPackageOutput package)

            programRunOutput result `shouldBe` "true\n"

        it "compiler-seed runs the fixture through the public CLI package entrypoint" $ do
            checkProgramArgs [compilerSeedRoot] `shouldReturn` Right "OK\n"
            runProgramArgs [compilerSeedRoot] `shouldReturn` Right "true\n"

compilerSeedPackageId :: PackageId
compilerSeedPackageId = PackageId "compiler-frontend-seed"

compilerSeedRoot :: FilePath
compilerSeedRoot = "test/programs/compiler-seed/frontend-contract"

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value
