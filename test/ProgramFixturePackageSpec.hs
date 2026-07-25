module ProgramFixturePackageSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec

import CheckedProgramTestSupport
    ( checkedArtifactBackendLLVM
    , checkedArtifactCheckOutput
    , checkedArtifactRunOutput
    , checkedProgramArtifactFromLocatedPackage
    )
import LLVMToolSupport (validateLLVMAssembly)
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
import Parity.ProgramMatrix
    ( staticCrossModulePackageRoot
    , staticSearchPathPackageRoots
    )

spec :: Spec
spec =
    describe "MLF.Program fixture package migration" $ do
        it "discovers, checks, runs, and prepares backend emission for a static package-root fixture" $ do
            package <- requireRight =<< discoverLocatedProgramPackage fixturePackageId staticCrossModulePackageRoot
            graph <- requireRight (locatedProgramPackageModuleGraph package)
            artifact <- requireRight (checkedProgramArtifactFromLocatedPackage package)

            packageModuleGraphOrder graph
                `shouldBe`
                    [ PackageModuleId fixturePackageId "Core"
                    , PackageModuleId fixturePackageId "Main"
                    ]
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right "1\n"
            output <- requireRight (checkedArtifactBackendLLVM artifact)

            output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
            output `shouldSatisfy` isInfixOf "define i64 @\"Core__applyId\"()"
            output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
            validateLLVMAssembly output

        it "discovers, checks, runs, and prepares backend emission for a static ordered search-path package fixture" $ do
            package <-
                requireRight
                    =<< discoverLocatedProgramPackageFromSearchPath
                        fixturePackageId
                        (PackageSearchPath (map PackageRoot staticSearchPathPackageRoots))
            graph <- requireRight (locatedProgramPackageModuleGraph package)
            artifact <- requireRight (checkedProgramArtifactFromLocatedPackage package)

            packageModuleGraphOrder graph
                `shouldBe`
                    [ PackageModuleId fixturePackageId "SearchLib"
                    , PackageModuleId fixturePackageId "Main"
                    ]
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right "2\n"
            output <- requireRight (checkedArtifactBackendLLVM artifact)

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
