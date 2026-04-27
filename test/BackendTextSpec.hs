{-# LANGUAGE LambdaCase #-}

module BackendTextSpec (spec) where

import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))

import MLF.Backend.IR
import MLF.Backend.Text
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Program.Types (CheckedProgram)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Program
    ( checkProgram
    , parseRawProgram
    , renderProgramParseError
    )
import MLF.Program.CLI (emitBackendFile)
import Test.Hspec

spec :: Spec
spec = describe "MLF.Backend.Text" $ do
    it "renders converted checked functions as deterministic backend text" $ do
        checked <- requireChecked simpleFunctionProgram
        output <- requireRight (renderCheckedProgramBackendText checked)

        goldenText "test/golden/backend-simple-function.golden" output

    it "renders top-level references as globals while preserving local shadowing" $ do
        output <- requireRight (renderBackendProgram globalReferenceProgram)

        output `shouldSatisfy` isInfixOf "ret i64 call @\"helper\"(1)"
        output
            `shouldSatisfy` isInfixOf
                "define @\"shadowHelper\"(%\"helper\" : i64) -> i64 {\n    ret i64 %\"helper\""

    it "renders explicit diagnostics for unsupported backend case nodes" $ do
        case renderBackendProgram unsupportedCaseProgram of
            Left err ->
                goldenText "test/golden/backend-unsupported-case.golden" (renderBackendTextError err ++ "\n")
            Right output ->
                expectationFailure ("expected unsupported case diagnostic, got output:\n" ++ output)

    it "emits backend text from the CLI file entrypoint" $ do
        output <- requireRight =<< emitBackendFile "test/programs/unified/authoritative-let-polymorphism.mlfp"

        output `shouldSatisfy` isInfixOf "; mlf2 backend-text v0"
        output `shouldSatisfy` isInfixOf "define @\"Main__main\"() -> i64"

simpleFunctionProgram :: String
simpleFunctionProgram =
    unlines
        [ "module Main export (id, main) {"
        , "  def id : Int -> Int = \\x x;"
        , "  def main : Int = id 1;"
        , "}"
        ]

globalReferenceProgram :: BackendProgram
globalReferenceProgram =
    BackendProgram
        { backendProgramModules =
            [ BackendModule
                { backendModuleName = "Main"
                , backendModuleData = []
                , backendModuleBindings =
                    [ helperBinding
                    , usesHelperBinding
                    , shadowHelperBinding
                    ]
                }
            ]
        , backendProgramMain = "usesHelper"
        }

helperBinding :: BackendBinding
helperBinding =
    BackendBinding
        { backendBindingName = "helper"
        , backendBindingType = unaryIntTy
        , backendBindingExpr =
            BackendLam
                { backendExprType = unaryIntTy
                , backendParamName = "x"
                , backendParamType = intTy
                , backendBody = BackendVar intTy "x"
                }
        , backendBindingExportedAsMain = False
        }

usesHelperBinding :: BackendBinding
usesHelperBinding =
    BackendBinding
        { backendBindingName = "usesHelper"
        , backendBindingType = intTy
        , backendBindingExpr =
            BackendApp
                { backendExprType = intTy
                , backendFunction = BackendVar unaryIntTy "helper"
                , backendArgument = BackendLit intTy (LInt 1)
                }
        , backendBindingExportedAsMain = True
        }

shadowHelperBinding :: BackendBinding
shadowHelperBinding =
    BackendBinding
        { backendBindingName = "shadowHelper"
        , backendBindingType = unaryIntTy
        , backendBindingExpr =
            BackendLam
                { backendExprType = unaryIntTy
                , backendParamName = "helper"
                , backendParamType = intTy
                , backendBody = BackendVar intTy "helper"
                }
        , backendBindingExportedAsMain = False
        }

unaryIntTy :: BackendType
unaryIntTy =
    BTArrow intTy intTy

unsupportedCaseProgram :: BackendProgram
unsupportedCaseProgram =
    BackendProgram
        { backendProgramModules =
            [ BackendModule
                { backendModuleName = "Main"
                , backendModuleData = [boxData]
                , backendModuleBindings =
                    [ BackendBinding
                        { backendBindingName = "main"
                        , backendBindingType = intTy
                        , backendBindingExpr = boxCaseExpr
                        , backendBindingExportedAsMain = True
                        }
                    ]
                }
            ]
        , backendProgramMain = "main"
        }

boxData :: BackendData
boxData =
    BackendData
        { backendDataName = "Box"
        , backendDataParameters = []
        , backendDataConstructors =
            [ BackendConstructor
                { backendConstructorName = "Box"
                , backendConstructorForalls = []
                , backendConstructorFields = [intTy]
                , backendConstructorResult = boxTy
                }
            ]
        }

boxCaseExpr :: BackendExpr
boxCaseExpr =
    BackendCase
        { backendExprType = intTy
        , backendScrutinee =
            BackendConstruct
                { backendExprType = boxTy
                , backendConstructName = "Box"
                , backendConstructArgs = [BackendLit intTy (LInt 1)]
                }
        , backendAlternatives =
            BackendAlternative
                { backendAltPattern = BackendConstructorPattern "Box" ["n"]
                , backendAltBody = BackendVar intTy "n"
                }
                :| []
        }

intTy :: BackendType
intTy =
    BTBase (BaseTy "Int")

boxTy :: BackendType
boxTy =
    BTBase (BaseTy "Box")

requireChecked :: String -> IO CheckedProgram
requireChecked input =
    case parseRawProgram input of
        Left err ->
            expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program ->
            requireRight (checkProgram program)

requireRight :: (Show err) => Either err a -> IO a
requireRight =
    \case
        Left err ->
            expectationFailure (show err) >> fail "unexpected Left"
        Right value ->
            pure value

goldenText :: FilePath -> String -> Expectation
goldenText goldenPath actual = do
    expected <- readFile goldenPath
    length expected `seq` actual `shouldBe` expected
