module ProgramPackageSpec (spec) where

import Data.Either (isRight)
import Test.Hspec

import MLF.Backend.Emission.Prepare (prepareBackendEmissionFromSource)
import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , parseRawProgram
    , renderProgramParseError
    )
import MLF.Frontend.Program.Check
    ( checkLocatedProgramPackage
    , checkProgram
    , checkProgramPackage
    )
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage (..)
    , LocatedProgramSourceUnit (..)
    , PackageModuleId (..)
    , locatedProgramPackageModuleIds
    , trivialLocatedProgramPackage
    , trivialPackageId
    , trivialProgramPackage
    )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackage)
import MLF.Frontend.Program.Types
    ( CheckedModule (..)
    , CheckedProgram (..)
    )
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Program.CLI (runProgramFile)

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
            let package = withPreludeLocatedPackage (trivialLocatedProgramPackage located)

            locatedProgramPackageModuleIds package
                `shouldBe`
                    [ PackageModuleId trivialPackageId "Prelude"
                    , PackageModuleId trivialPackageId "Main"
                    ]
            checkLocatedProgramPackage package `shouldSatisfy` isRight

        it "keeps the CLI helper running single-file input as a trivial package" $ do
            runProgramFile "test/programs/recursive-adt/plain-recursive-nat.mlfp"
                `shouldReturn` Right "true\n"

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

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value
