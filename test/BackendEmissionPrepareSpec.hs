{-# LANGUAGE LambdaCase #-}

module BackendEmissionPrepareSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import LLVMToolSupport (validateLLVMAssembly)
import MLF.Backend.Emission.Prepare
    ( prepareBackendEmissionFromLocatedPackage
    , prepareBackendEmissionFromSource
    )
import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage (..)
    , PackageId (..)
    , locatedProgramSourceUnitFromLocated
    )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackage)
import MLF.Backend.LLVM (renderCheckedProgramLLVM)
import MLF.Frontend.Program.Types
    ( CheckedBinding (..)
    , CheckedModule (..)
    , CheckedProgram (..)
    )
import qualified MLF.Frontend.Syntax.Program as ProgramSyntax

spec :: Spec
spec =
    describe "BackendEmissionPrepareSpec" $ do
        it "prepares and renders backend LLVM from a source string without file IO" $ do
            checked <- requireRight (prepareBackendEmissionFromSource "inline-main.mlfp" simpleProgram)
            output <- requireRight (renderCheckedProgramLLVM checked)

            output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
            output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
            validateLLVMAssembly output

        it "prepares and renders backend LLVM from a located package" $ do
            lib <- requireLocated "src/Lib.mlfp" libProgram
            main <- requireLocated "app/Main.mlfp" mainImportsLibProgram
            let package =
                    withPreludeLocatedPackage
                        LocatedProgramPackage
                            { locatedProgramPackageId = PackageId "backend-package"
                            , locatedProgramPackageSourceUnits =
                                [ locatedProgramSourceUnitFromLocated lib
                                , locatedProgramSourceUnitFromLocated main
                                ]
                            }

            checked <- requireRight (prepareBackendEmissionFromLocatedPackage package)
            output <- requireRight (renderCheckedProgramLLVM checked)

            map checkedModuleName (checkedProgramModules checked) `shouldBe` ["Prelude", "Lib", "Main"]
            output `shouldSatisfy` isInfixOf "define i64 @\"Lib__two\"()"
            output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
            validateLLVMAssembly output

        it "retains referenced Prelude data and constructor bindings while pruning unreferenced Prelude bindings" $ do
            checked <- requireRight (prepareBackendEmissionFromSource "inline-unit.mlfp" unitProgram)
            preludeModule <- requirePreludeModule checked

            Map.keysSet (checkedModuleData preludeModule) `shouldBe` Set.singleton "Unit"
            map checkedBindingName (checkedModuleBindings preludeModule) `shouldBe` ["Prelude__Unit"]

simpleProgram :: String
simpleProgram =
    unlines
        [ "module Main export (main) {"
        , "  def main : Int = 1;"
        , "}"
        ]

unitProgram :: String
unitProgram =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..));"
        , "  def main : Unit = Unit;"
        , "}"
        ]

libProgram :: String
libProgram =
    unlines
        [ "module Lib export (two) {"
        , "  def two : Int = 2;"
        , "}"
        ]

mainImportsLibProgram :: String
mainImportsLibProgram =
    unlines
        [ "module Main export (main) {"
        , "  import Lib exposing (two);"
        , "  def main : Int = two;"
        , "}"
        ]

requireLocated :: FilePath -> String -> IO ProgramSyntax.LocatedProgram
requireLocated path source =
    case parseLocatedProgramWithFile path source of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right located -> pure located

requirePreludeModule :: CheckedProgram -> IO CheckedModule
requirePreludeModule checked =
    case [checkedModule | checkedModule <- checkedProgramModules checked, checkedModuleName checkedModule == "Prelude"] of
        [preludeModule] ->
            pure preludeModule
        [] ->
            expectationFailure "expected prepared program to contain the Prelude module"
                >> fail "missing Prelude module"
        preludeModules ->
            expectationFailure ("expected one Prelude module, got " ++ show (length preludeModules))
                >> fail "duplicate Prelude modules"

requireRight :: (Show err) => Either err a -> IO a
requireRight =
    \case
        Left err ->
            expectationFailure (show err) >> fail "unexpected Left"
        Right value ->
            pure value
