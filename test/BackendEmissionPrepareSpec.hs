{-# LANGUAGE LambdaCase #-}

module BackendEmissionPrepareSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import LLVMToolSupport (validateLLVMAssembly)
import MLF.Backend.Emission.Prepare
    ( prepareBackendEmissionFromSource
    )
import MLF.Backend.LLVM (renderCheckedProgramLLVM)
import MLF.Frontend.Program.Types
    ( CheckedBinding (..)
    , CheckedModule (..)
    , CheckedProgram (..)
    )

spec :: Spec
spec =
    describe "BackendEmissionPrepareSpec" $ do
        it "prepares and renders backend LLVM from a source string without file IO" $ do
            checked <- requireRight (prepareBackendEmissionFromSource "inline-main.mlfp" simpleProgram)
            output <- requireRight (renderCheckedProgramLLVM checked)

            output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
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
