module ProgramFixturePackageSpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import Test.Hspec

import LLVMToolSupport (validateLLVMAssembly)
import MLF.Backend.Emission.Prepare (prepareBackendEmissionFromLocatedPackage)
import MLF.Backend.LLVM (renderCheckedProgramLLVM)
import MLF.Frontend.Program.Check (checkLocatedProgramPackage)
import MLF.Frontend.Program.Package
    ( PackageId (..)
    , PackageModuleId (..)
    , PackageRoot (..)
    , PackageSearchPath (..)
    , discoverLocatedProgramPackage
    , discoverLocatedProgramPackageFromSearchPath
    , locatedProgramPackageModuleGraph
    , packageModuleGraphOrder
    )
import MLF.Frontend.Program.Run
    ( programRunOutput
    , runLocatedProgramPackageOutput
    )
import MLF.Frontend.Program.Types (CheckedProgram (..))
import MLF.Program.CLI
    ( checkProgramArgs
    , checkProgramFile
    , emitBackendArgs
    , runProgramArgs
    , runProgramFile
    )
import Parity.ProgramMatrix
    ( fixtureRuntimeExpectations
    , staticCrossModulePackageRoot
    , staticSearchPathLibRoot
    , staticSearchPathMainRoot
    , staticSearchPathPackageRoots
    )

spec :: Spec
spec =
    describe "MLF.Program fixture package migration" $ do
        forM_ fixtureRuntimeExpectations $ \(path, expectedValue) ->
            it ("checks and runs " ++ path ++ " as a trivial package input") $ do
                checkProgramFile path `shouldReturn` Right "OK\n"
                runProgramFile path `shouldReturn` Right (expectedValue ++ "\n")

        it "discovers, checks, runs, and prepares backend emission for a static package-root fixture" $ do
            package <- requireRight =<< discoverLocatedProgramPackage fixturePackageId staticCrossModulePackageRoot
            graph <- requireRight (locatedProgramPackageModuleGraph package)

            packageModuleGraphOrder graph
                `shouldBe`
                    [ PackageModuleId fixturePackageId "Core"
                    , PackageModuleId fixturePackageId "Main"
                    ]
            checked <- requireRight (checkLocatedProgramPackage package)
            checkedProgramMain checked `shouldBe` "Main__main"
            result <- requireRight (runLocatedProgramPackageOutput package)
            programRunOutput result `shouldBe` "1\n"

            prepared <- requireRight (prepareBackendEmissionFromLocatedPackage package)
            output <- requireRight (renderCheckedProgramLLVM prepared)

            output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
            output `shouldSatisfy` isInfixOf "define i64 @\"Core__applyId\"()"
            output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
            validateLLVMAssembly output

        it "discovers and runs a static ordered search-path package fixture" $ do
            package <-
                requireRight
                    =<< discoverLocatedProgramPackageFromSearchPath
                        fixturePackageId
                        (PackageSearchPath (map PackageRoot staticSearchPathPackageRoots))
            graph <- requireRight (locatedProgramPackageModuleGraph package)

            packageModuleGraphOrder graph
                `shouldBe`
                    [ PackageModuleId fixturePackageId "SearchLib"
                    , PackageModuleId fixturePackageId "Main"
                    ]
            checked <- requireRight (checkLocatedProgramPackage package)
            checkedProgramMain checked `shouldBe` "Main__main"
            result <- requireRight (runLocatedProgramPackageOutput package)
            programRunOutput result `shouldBe` "2\n"

            let cliArgs = [staticSearchPathMainRoot, "--search-path", staticSearchPathLibRoot]
            checkProgramArgs cliArgs `shouldReturn` Right "OK\n"
            runProgramArgs cliArgs `shouldReturn` Right "2\n"
            output <- requireRight =<< emitBackendArgs cliArgs
            output `shouldSatisfy` isInfixOf "define i64 @\"SearchLib__two\"()"
            output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
            validateLLVMAssembly output

fixturePackageId :: PackageId
fixturePackageId = PackageId "fixture-package"

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value
