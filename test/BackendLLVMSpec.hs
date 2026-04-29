{-# LANGUAGE LambdaCase #-}

module BackendLLVMSpec (spec) where

import Control.Monad (when)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import LLVMToolSupport
  ( validateLLVMAssembly,
    validateLLVMObjectCode,
    withTempProgram,
  )
import MLF.Backend.IR
import MLF.Backend.LLVM
import qualified MLF.Backend.LLVM.Lower as Lower
import MLF.Backend.LLVM.Ppr (renderLLVMModule)
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Program.Types (CheckedProgram)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Program
  ( checkProgram,
    parseRawProgram,
    renderProgramParseError,
  )
import MLF.Program.CLI (emitBackendFile)
import Parity.ProgramMatrix
  ( ProgramMatrixCase (..),
    ProgramMatrixExpectation (..),
    ProgramMatrixSource (..),
    ProgramRuntimeCase (..),
    emlfSurfaceParityMatrix,
    programSpecToLLVMParityCases,
    unifiedFixtureExpectations,
  )

spec :: Spec
spec = describe "MLF.Backend.LLVM" $ do
  it "renders converted checked functions as deterministic LLVM IR" $ do
    checked <- requireChecked simpleFunctionProgram
    output <- requireRight (renderCheckedProgramLLVM checked)

    goldenText "test/golden/backend-simple-function.ll.golden" output
    validateLLVMAssembly output

  it "emits LLVM IR from the CLI file entrypoint" $ do
    output <- requireRight =<< emitBackendFile "test/programs/unified/authoritative-let-polymorphism.mlfp"

    output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
    output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
    validateLLVMAssembly output

  describe "shared ProgramSpec first-order parity" $ do
    it "keeps the selected shared first-order parity rows wired" $
      map matrixCaseName backendFirstOrderSurfaceParityCases `shouldBe` backendFirstOrderSurfaceParityNames

    mapM_ runLLVMProgramMatrixCase backendFirstOrderSurfaceParityCases
    mapM_ runLLVMUnifiedFixture backendUnifiedFixturePaths

  it "preserves referenced Prelude bindings and lowers runtime primitive calls" $ do
    output <-
      withTempProgram preludeAndProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "declare i1 @\"__mlfp_and\"(i1, i1)"
    output `shouldSatisfy` isInfixOf "define i1 @\"Prelude__and\""
    output `shouldSatisfy` isInfixOf "call i1 @\"__mlfp_and\""
    validateLLVMAssembly output

  it "lowers string literals to private LLVM globals" $ do
    output <- requireRight (renderBackendProgramLLVM stringProgram)

    goldenText "test/golden/backend-string.ll.golden" output
    validateLLVMAssembly output

  it "uses collision-free names for distinct type specializations" $ do
    output <- requireRight (renderBackendProgramLLVM specializationNameCollisionProgram)

    length (filter (isInfixOf "define private ptr @\"poly$t") (lines output)) `shouldBe` 2
    validateLLVMAssembly output

  it "specializes polymorphic zero-arity globals used through type application" $ do
    output <- requireRight (renderBackendProgramLLVM polymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define private ptr @\"none$t"
    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"none$t"
    validateLLVMAssembly output

  it "instantiates local polymorphic zero-arity values used through type application" $ do
    output <- requireRight (renderBackendProgramLLVM localPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "Unknown backend LLVM function"
    validateLLVMAssembly output

  it "instantiates direct polymorphic zero-arity expressions used through type application" $ do
    output <- requireRight (renderBackendProgramLLVM directPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "escaping type abstraction"
    validateLLVMAssembly output

  it "instantiates polymorphic zero-arity values when type application crosses let heads" $ do
    output <- requireRight (renderBackendProgramLLVM letHeadPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "unexpected type arguments"
    validateLLVMAssembly output

  it "collects global specializations when type application crosses let heads" $ do
    output <- requireRight (renderBackendProgramLLVM letHeadGlobalPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define private ptr @\"none$t"
    output `shouldSatisfy` isInfixOf "call ptr @\"none$t"
    output `shouldNotSatisfy` isInfixOf "missing specialization"
    validateLLVMAssembly output

  it "collects global specializations after direct head type instantiation" $ do
    output <- requireRight (renderBackendProgramLLVM directHeadGlobalPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define private ptr @\"none$t"
    output `shouldSatisfy` isInfixOf "call ptr @\"none$t"
    output `shouldNotSatisfy` isInfixOf "missing specialization"
    validateLLVMAssembly output

  it "treats top-level function parameters as bound during reachability scanning" $ do
    output <- requireRight (renderBackendProgramLLVM parameterNameShadowsDeadGlobalProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"(i64 %\"x\")"
    output `shouldNotSatisfy` isInfixOf "define ptr @\"x\""
    validateLLVMAssembly output

  it "does not collect global specializations for let-bound shadowing names" $ do
    output <- requireRight (renderBackendProgramLLVM letShadowedGlobalSpecializationProgram)

    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "poly$t"
    validateLLVMAssembly output

  it "collects specializations from let-promoted local function aliases" $ do
    output <- requireRight (renderBackendProgramLLVM letPromotedAliasSpecializationProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"poly$t"
    output `shouldSatisfy` isInfixOf "call i64 @\"poly$t"
    output `shouldNotSatisfy` isInfixOf "missing specialization"
    validateLLVMAssembly output

  it "lowers local function aliases without requiring closure conversion" $ do
    output <- requireRight (renderBackendProgramLLVM localFunctionAliasProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldSatisfy` isInfixOf "ret i64 7"
    output `shouldNotSatisfy` isInfixOf "Unknown backend LLVM function"
    validateLLVMAssembly output

  it "preserves top-level function aliases as function forms" $ do
    output <- requireRight (renderBackendProgramLLVM topLevelFunctionAliasProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"g\"(i64"
    output `shouldSatisfy` isInfixOf "call i64 @\"id\""
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM type"
    validateLLVMAssembly output

  it "freshens shadowed lambda parameters before emitting LLVM" $ do
    output <- requireRight (renderBackendProgramLLVM shadowedLambdaParamsProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"shadow\"(i64 %\"x\", i64 %\"x1\")"
    output `shouldSatisfy` isInfixOf "ret i64 %\"x1\""
    validateLLVMAssembly output

  it "lowers let-headed calls before call dispatch" $ do
    output <- requireRight (renderBackendProgramLLVM letHeadedCallProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldSatisfy` isInfixOf "ret i64 7"
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM call"
    validateLLVMAssembly output

  it "lowers case-headed calls before call dispatch" $ do
    output <- requireRight (renderBackendProgramLLVM caseHeadedCallProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldSatisfy` isInfixOf "call i64 @\"id\""
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM call"
    validateLLVMAssembly output

  it "resolves a global named like the runtime and primitive before intrinsic dispatch" $ do
    output <- requireRight (renderBackendProgramLLVM userNamedRuntimeAndProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"__mlfp_and\"(i64"
    output `shouldSatisfy` isInfixOf "call i64 @\"__mlfp_and\""
    output `shouldNotSatisfy` isInfixOf "declare i1 @\"__mlfp_and\"(i1, i1)"
    validateLLVMAssembly output

  it "suppresses the runtime malloc declaration when a global owns that name" $ do
    output <- requireRight (renderBackendProgramLLVM userNamedMallocProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"malloc\"(i64"
    output `shouldSatisfy` isInfixOf "call i64 @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "declare ptr @\"malloc\"(i64)"
    validateLLVMAssembly output

  it "rejects constructor allocation when a global owns the runtime malloc name" $ do
    renderBackendProgramLLVM mallocCollisionConstructorProgram
      `shouldSatisfyLeft` isInfixOf "reserved runtime binding \"malloc\""

  it "lowers Nat construction and case analysis to heap tags and switch" $ do
    output <- requireRight =<< emitBackendFile "test/programs/unified/authoritative-case-analysis.mlfp"

    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldSatisfy` isInfixOf "switch i64"
    output `shouldSatisfy` isInfixOf "phi i64"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers recursive local let functions through helper calls" $ do
    output <- requireRight =<< emitBackendFile "test/programs/unified/authoritative-recursive-let.mlfp"

    output `shouldSatisfy` isInfixOf "define ptr @\"Main__main$letrec$"
    output `shouldSatisfy` isInfixOf "call ptr @\"Main__main$letrec$"
    output `shouldSatisfy` isInfixOf "switch i64"
    output `shouldSatisfy` isInfixOf "phi ptr"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "emits first-order recursive ADT parity fixtures through LLVM" $ do
    mapM_
      ( \path -> do
          output <- requireRight =<< emitBackendFile path
          output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
          output `shouldSatisfy` isInfixOf "switch i64"
          validateLLVMAssembly output
          validateLLVMObjectCode output
      )
      [ "test/programs/recursive-adt/plain-recursive-nat.mlfp",
        "test/programs/recursive-adt/recursive-list-tail.mlfp",
        "test/programs/recursive-adt/recursive-gadt.mlfp",
        "test/programs/recursive-adt/recursive-existential.mlfp",
        "test/programs/recursive-adt/recursive-tree-first-order.mlfp"
      ]

  it "loads only constructor fields used by a case branch" $ do
    output <- requireRight (renderBackendProgramLLVM unusedPolymorphicPatternFieldProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"(ptr %\"box\")"
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"box\", i64 16"
    output `shouldNotSatisfy` isInfixOf "getelementptr i8, ptr %\"box\", i64 8"
    validateLLVMAssembly output

  it "rejects duplicate constructor case alternatives before emitting switch" $ do
    renderBackendProgramLLVM duplicateConstructorCaseProgram
      `shouldSatisfyLeft` isInfixOf "duplicate constructor case tag"

  it "rejects non-tail default case alternatives before emitting switch" $ do
    renderBackendProgramLLVM nonTailDefaultCaseProgram
      `shouldSatisfyLeft` isInfixOf "default case alternative must be last"

  it "lowers nullary and recursive-list constructors through case" $ do
    checked <- requireChecked recursiveListProgram
    output <- requireRight (renderCheckedProgramLLVM checked)

    output `shouldSatisfy` isInfixOf "define ptr @\"Main__tailOrNil\""
    output `shouldSatisfy` isInfixOf "define i1 @\"Main__isNil\""
    output `shouldSatisfy` isInfixOf "phi ptr"
    validateLLVMAssembly output

  it "emits recursive-list CLI fixtures without ambiguous Prelude data" $ do
    output <- requireRight =<< emitBackendFile "test/programs/recursive-adt/recursive-list-tail.mlfp"

    output `shouldSatisfy` isInfixOf "define ptr @\"RecursiveList__tailOrNil\""
    output `shouldSatisfy` isInfixOf "define i1 @\"RecursiveList__isNil\""
    output `shouldSatisfy` isInfixOf "phi ptr"
    validateLLVMAssembly output

  it "lowers a first-order existential/GADT-shaped recursive case fixture" $ do
    output <- requireRight =<< emitBackendFile "test/programs/recursive-adt/recursive-existential.mlfp"

    output `shouldSatisfy` isInfixOf "define i1 @\"RecursiveExistential__unwrapSome\""
    output `shouldSatisfy` isInfixOf "call i1 @\"RecursiveExistential__unwrapSome\""
    validateLLVMAssembly output

  describe "ProgramSpec-to-LLVM parity matrix" $ do
    it "classifies every interpreter-success case exactly once" $ do
      let caseNames = map runtimeCaseName programSpecToLLVMParityCases
          uniqueCaseNames = Set.fromList caseNames
          unsupportedNames = Set.fromList (map fst unsupportedLLVMParityCases)
          objectCodeNames = Set.fromList llvmObjectCodeParityCases

      length caseNames `shouldBe` Set.size uniqueCaseNames
      length unsupportedLLVMParityCases `shouldBe` Set.size unsupportedNames
      unsupportedNames `Set.isSubsetOf` uniqueCaseNames `shouldBe` True
      objectCodeNames `Set.isSubsetOf` Set.difference uniqueCaseNames unsupportedNames `shouldBe` True
      Map.keysSet llvmParityExpectations `shouldBe` uniqueCaseNames
      llvmUnsupportedParityCount `shouldBe` expectedLLVMUnsupportedParityCount

    mapM_ runLLVMParityCase programSpecToLLVMParityCases

  it "rejects partial applications until closure conversion exists" $ do
    renderBackendProgramLLVM partialApplicationProgram
      `shouldSatisfyLeft` isInfixOf "Unsupported backend LLVM type"

  it "rejects escaping lambdas until closure conversion exists" $ do
    renderBackendProgramLLVM escapingLambdaProgram
      `shouldSatisfyLeft` isInfixOf "Unsupported backend LLVM type"

  it "rejects function-typed constructor fields" $ do
    renderBackendProgramLLVM functionFieldProgram
      `shouldSatisfyLeft` isInfixOf "escaping function"

  it "rejects unknown base types" $ do
    renderBackendProgramLLVM unknownBaseProgram
      `shouldSatisfyLeft` isInfixOf "Unsupported backend LLVM type"

  it "rejects representation-changing roll/unroll nodes" $ do
    case Lower.lowerBackendProgram rollMismatchProgram of
      Left err ->
        renderBackendLLVMError (BackendLLVMLoweringFailed err)
          `shouldSatisfy` isInfixOf "representation-changing roll"
      Right llvmModule ->
        expectationFailure ("expected roll mismatch, got output:\n" ++ renderLLVMModule llvmModule)

backendFirstOrderSurfaceParityNames :: [String]
backendFirstOrderSurfaceParityNames =
  [ "runs lambda/application",
    "runs let polymorphism at Int and Bool",
    "runs typed let annotation",
    "runs term annotation"
  ]

backendFirstOrderSurfaceParityCases :: [ProgramMatrixCase]
backendFirstOrderSurfaceParityCases =
  [ matrixCase
    | matrixCase <- emlfSurfaceParityMatrix,
      matrixCaseName matrixCase `elem` backendFirstOrderSurfaceParityNames
  ]

backendUnifiedFixturePaths :: [FilePath]
backendUnifiedFixturePaths =
  [ path
    | (path, _) <- unifiedFixtureExpectations,
      path
        `elem` [ "test/programs/unified/authoritative-let-polymorphism.mlfp",
                 "test/programs/unified/authoritative-cross-module-let-polymorphism.mlfp",
                 "test/programs/unified/authoritative-case-analysis.mlfp"
               ]
  ]

runLLVMProgramMatrixCase :: ProgramMatrixCase -> Spec
runLLVMProgramMatrixCase matrixCase =
  it ("lowers " ++ matrixCaseName matrixCase ++ " through LLVM") $ do
    case matrixCaseExpectation matrixCase of
      ExpectRunValue _ -> pure ()
      ExpectCheckSuccess -> pure ()
      ExpectCheckFailureContaining fragment ->
        expectationFailure ("selected backend LLVM parity row expects a frontend failure: " ++ fragment)
    output <- renderProgramMatrixSourceLLVM (matrixCaseSource matrixCase)
    output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
    validateLLVMAssembly output

runLLVMUnifiedFixture :: FilePath -> Spec
runLLVMUnifiedFixture path =
  it ("lowers " ++ path ++ " through the CLI LLVM backend") $ do
    output <- requireRight =<< emitBackendFile path
    output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
    case path of
      "test/programs/unified/authoritative-cross-module-let-polymorphism.mlfp" -> do
        output `shouldSatisfy` isInfixOf "define i64 @\"Core__applyId\"()"
        output `shouldSatisfy` isInfixOf "define i64 @\"User__main\"()"
        validateLLVMAssembly output
        validateLLVMObjectCode output
      "test/programs/unified/authoritative-case-analysis.mlfp" -> do
        output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
        output `shouldSatisfy` isInfixOf "switch i64"
        output `shouldSatisfy` isInfixOf "phi i64"
        validateLLVMAssembly output
        validateLLVMObjectCode output
      _ -> validateLLVMAssembly output

renderProgramMatrixSourceLLVM :: ProgramMatrixSource -> IO String
renderProgramMatrixSourceLLVM source =
  case source of
    InlineProgram programText ->
      requireRight =<< withTempProgram programText emitBackendFile
    ProgramFile path ->
      requireRight =<< emitBackendFile path

simpleFunctionProgram :: String
simpleFunctionProgram =
  unlines
    [ "module Main export (id, main) {",
      "  def id : Int -> Int = \\x x;",
      "  def main : Int = id 1;",
      "}"
    ]

preludeAndProgram :: String
preludeAndProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (and);",
      "  def main : Bool = and true false;",
      "}"
    ]

recursiveListProgram :: String
recursiveListProgram =
  unlines
    [ "module Main export (Nat(..), RList(..), tailOrNil, isNil, main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  data RList =",
      "      RNil : RList",
      "    | RCons : Nat -> RList -> RList;",
      "",
      "  def tailOrNil : RList -> RList = \\(xs : RList) case xs of {",
      "    RNil -> RNil;",
      "    RCons _ rest -> rest",
      "  };",
      "",
      "  def isNil : RList -> Bool = \\(xs : RList) case xs of {",
      "    RNil -> true;",
      "    RCons _ _ -> false",
      "  };",
      "",
      "  def main : Bool = isNil (tailOrNil (RCons Zero RNil));",
      "}"
    ]

stringProgram :: BackendProgram
stringProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = stringTy,
                      backendBindingExpr = BackendLit stringTy (LString "hello"),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

specializationNameCollisionProgram :: BackendProgram
specializationNameCollisionProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData =
                [ BackendData "A_B" [] [BackendConstructor "MkA_B" [] [] aUnderscoreTy],
                  BackendData "A'B" [] [BackendConstructor "MkA'B" [] [] aPrimeTy]
                ],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "poly",
                      backendBindingType = polyIdTy,
                      backendBindingExpr = polyIdExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLet
                          intTy
                          "left"
                          aUnderscoreTy
                          (polyIdCall aUnderscoreTy (BackendConstruct aUnderscoreTy "MkA_B" []))
                          ( BackendLet
                              intTy
                              "right"
                              aPrimeTy
                              (polyIdCall aPrimeTy (BackendConstruct aPrimeTy "MkA'B" []))
                              (intLit 0)
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

polymorphicZeroArityProgram :: BackendProgram
polymorphicZeroArityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "none",
                      backendBindingType = nonePolyTy,
                      backendBindingExpr = nonePolyExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr =
                        BackendTyApp
                          (optionTy intTy)
                          (BackendVar nonePolyTy "none")
                          intTy,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

localPolymorphicZeroArityProgram :: BackendProgram
localPolymorphicZeroArityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr =
                        BackendLet
                          (optionTy intTy)
                          "none"
                          nonePolyTy
                          nonePolyExpr
                          ( BackendTyApp
                              (optionTy intTy)
                              (BackendVar nonePolyTy "none")
                              intTy
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

directPolymorphicZeroArityProgram :: BackendProgram
directPolymorphicZeroArityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr =
                        BackendTyApp
                          (optionTy intTy)
                          nonePolyExpr
                          intTy,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

letHeadPolymorphicZeroArityProgram :: BackendProgram
letHeadPolymorphicZeroArityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr =
                        BackendTyApp
                          (optionTy intTy)
                          ( BackendLet
                              nonePolyTy
                              "none"
                              nonePolyTy
                              nonePolyExpr
                              (BackendVar nonePolyTy "none")
                          )
                          intTy,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

letHeadGlobalPolymorphicZeroArityProgram :: BackendProgram
letHeadGlobalPolymorphicZeroArityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "none",
                      backendBindingType = nonePolyTy,
                      backendBindingExpr = nonePolyExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr =
                        BackendTyApp
                          (optionTy intTy)
                          ( BackendLet
                              nonePolyTy
                              "ignored"
                              intTy
                              (intLit 0)
                              (BackendVar nonePolyTy "none")
                          )
                          intTy,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

directHeadGlobalPolymorphicZeroArityProgram :: BackendProgram
directHeadGlobalPolymorphicZeroArityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "none",
                      backendBindingType = nonePolyTy,
                      backendBindingExpr = nonePolyExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr =
                        BackendTyApp
                          (optionTy intTy)
                          noneViaDirectTyAbsExpr
                          intTy,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

parameterNameShadowsDeadGlobalProgram :: BackendProgram
parameterNameShadowsDeadGlobalProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ helperBinding,
                  BackendBinding
                    { backendBindingName = "x",
                      backendBindingType = fnBoxTy,
                      backendBindingExpr =
                        BackendConstruct fnBoxTy "FnBox" [BackendVar unaryIntTy "helper"],
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr = intIdentityExpr,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

letShadowedGlobalSpecializationProgram :: BackendProgram
letShadowedGlobalSpecializationProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData, fnBoxData],
              backendModuleBindings =
                [ helperBinding,
                  BackendBinding
                    { backendBindingName = "poly",
                      backendBindingType = badPolyTy,
                      backendBindingExpr = badPolyExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr =
                        BackendLet
                          (optionTy intTy)
                          "poly"
                          nonePolyTy
                          nonePolyExpr
                          ( BackendTyApp
                              (optionTy intTy)
                              (BackendVar nonePolyTy "poly")
                              intTy
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

letPromotedAliasSpecializationProgram :: BackendProgram
letPromotedAliasSpecializationProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "poly",
          backendBindingType = polyIdTy,
          backendBindingExpr = polyIdExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLet
              intTy
              "g"
              unaryIntTy
              (BackendTyApp unaryIntTy (BackendVar polyIdTy "poly") intTy)
              (BackendApp intTy (BackendVar unaryIntTy "g") (intLit 7)),
          backendBindingExportedAsMain = True
        }
    ]

localFunctionAliasProgram :: BackendProgram
localFunctionAliasProgram =
  programWithMainExpr intTy $
    BackendLet
      intTy
      "f"
      unaryIntTy
      intIdentityExpr
      ( BackendLet
          intTy
          "g"
          unaryIntTy
          (BackendVar unaryIntTy "f")
          (BackendApp intTy (BackendVar unaryIntTy "g") (intLit 7))
      )

topLevelFunctionAliasProgram :: BackendProgram
topLevelFunctionAliasProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "id",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "g",
          backendBindingType = unaryIntTy,
          backendBindingExpr = BackendVar unaryIntTy "id",
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr = BackendApp intTy (BackendVar unaryIntTy "g") (intLit 7),
          backendBindingExportedAsMain = True
        }
    ]

shadowedLambdaParamsProgram :: BackendProgram
shadowedLambdaParamsProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "shadow",
          backendBindingType = binaryIntTy,
          backendBindingExpr =
            BackendLam
              binaryIntTy
              "x"
              intTy
              (BackendLam unaryIntTy "x" intTy (BackendVar intTy "x")),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (BackendApp unaryIntTy (BackendVar binaryIntTy "shadow") (intLit 1))
              (intLit 7),
          backendBindingExportedAsMain = True
        }
    ]

letHeadedCallProgram :: BackendProgram
letHeadedCallProgram =
  programWithMainExpr intTy $
    BackendApp
      intTy
      (BackendLet unaryIntTy "id" unaryIntTy intIdentityExpr (BackendVar unaryIntTy "id"))
      (intLit 7)

caseHeadedCallProgram :: BackendProgram
caseHeadedCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "id",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr = intIdentityExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "fallback",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr =
                        BackendLam unaryIntTy "x" intTy (intLit 9),
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendCase
                              unaryIntTy
                              (BackendConstruct (optionTy intTy) "Some" [intLit 0])
                              ( BackendAlternative (BackendConstructorPattern "Some" ["value"]) (BackendVar unaryIntTy "id")
                                  :| [BackendAlternative (BackendConstructorPattern "None" []) (BackendVar unaryIntTy "fallback")]
                              )
                          )
                          (intLit 7),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

userNamedRuntimeAndProgram :: BackendProgram
userNamedRuntimeAndProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "__mlfp_and",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr = BackendApp intTy (BackendVar unaryIntTy "__mlfp_and") (intLit 7),
          backendBindingExportedAsMain = True
        }
    ]

userNamedMallocProgram :: BackendProgram
userNamedMallocProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "malloc",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr = BackendApp intTy (BackendVar unaryIntTy "malloc") (intLit 7),
          backendBindingExportedAsMain = True
        }
    ]

mallocCollisionConstructorProgram :: BackendProgram
mallocCollisionConstructorProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "malloc",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr = intIdentityExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr = BackendConstruct (optionTy intTy) "None" [],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

unusedPolymorphicPatternFieldProgram :: BackendProgram
unusedPolymorphicPatternFieldProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [lazyFieldBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = BTArrow lazyFieldBoxTy intTy,
                      backendBindingExpr =
                        BackendLam
                          (BTArrow lazyFieldBoxTy intTy)
                          "box"
                          lazyFieldBoxTy
                          ( BackendCase
                              intTy
                              (BackendVar lazyFieldBoxTy "box")
                              ( BackendAlternative
                                  (BackendConstructorPattern "Packed" ["unused", "value"])
                                  (BackendVar intTy "value")
                                  :| []
                              )
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

duplicateConstructorCaseProgram :: BackendProgram
duplicateConstructorCaseProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          (BackendConstruct (optionTy intTy) "None" [])
                          ( BackendAlternative (BackendConstructorPattern "None" []) (intLit 0)
                              :| [BackendAlternative (BackendConstructorPattern "None" []) (intLit 1)]
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

nonTailDefaultCaseProgram :: BackendProgram
nonTailDefaultCaseProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          (BackendConstruct (optionTy intTy) "Some" [intLit 7])
                          ( BackendAlternative BackendDefaultPattern (intLit 0)
                              :| [BackendAlternative (BackendConstructorPattern "Some" ["value"]) (BackendVar intTy "value")]
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

optionData :: BackendData
optionData =
  BackendData
    { backendDataName = "Option",
      backendDataParameters = ["a"],
      backendDataConstructors =
        [ BackendConstructor "None" [] [] (optionTy (BTVar "a")),
          BackendConstructor "Some" [] [BTVar "a"] (optionTy (BTVar "a"))
        ]
    }

nonePolyTy :: BackendType
nonePolyTy =
  BTForall "a" Nothing (optionTy (BTVar "a"))

nonePolyExpr :: BackendExpr
nonePolyExpr =
  BackendTyAbs
    nonePolyTy
    "a"
    Nothing
    (BackendConstruct (optionTy (BTVar "a")) "None" [])

noneViaDirectTyAbsExpr :: BackendExpr
noneViaDirectTyAbsExpr =
  BackendTyAbs
    nonePolyTy
    "a"
    Nothing
    ( BackendTyApp
        (optionTy (BTVar "a"))
        (BackendVar nonePolyTy "none")
        (BTVar "a")
    )

polyIdTy :: BackendType
polyIdTy =
  BTForall "a" Nothing (BTArrow (BTVar "a") (BTVar "a"))

polyIdExpr :: BackendExpr
polyIdExpr =
  BackendTyAbs
    polyIdTy
    "a"
    Nothing
    ( BackendLam
        (BTArrow (BTVar "a") (BTVar "a"))
        "x"
        (BTVar "a")
        (BackendVar (BTVar "a") "x")
    )

badPolyTy :: BackendType
badPolyTy =
  BTForall "a" Nothing fnBoxTy

badPolyExpr :: BackendExpr
badPolyExpr =
  BackendTyAbs
    badPolyTy
    "a"
    Nothing
    ( BackendConstruct
        fnBoxTy
        "FnBox"
        [BackendVar unaryIntTy "helper"]
    )

polyIdCall :: BackendType -> BackendExpr -> BackendExpr
polyIdCall ty arg =
  BackendApp
    ty
    (BackendTyApp (BTArrow ty ty) (BackendVar polyIdTy "poly") ty)
    arg

partialApplicationProgram :: BackendProgram
partialApplicationProgram =
  programWithBindings
    [ addBinding,
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = unaryIntTy,
          backendBindingExpr =
            BackendApp
              { backendExprType = unaryIntTy,
                backendFunction = BackendVar binaryIntTy "add",
                backendArgument = intLit 1
              },
          backendBindingExportedAsMain = True
        }
    ]

escapingLambdaProgram :: BackendProgram
escapingLambdaProgram =
  programWithMainExpr unaryIntTy $
    BackendLet
      { backendExprType = unaryIntTy,
        backendLetName = "f",
        backendLetType = unaryIntTy,
        backendLetRhs = intIdentityExpr,
        backendLetBody = BackendVar unaryIntTy "f"
      }

functionFieldProgram :: BackendProgram
functionFieldProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ helperBinding,
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = fnBoxTy,
                      backendBindingExpr = BackendConstruct fnBoxTy "FnBox" [BackendVar unaryIntTy "helper"],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

unknownBaseProgram :: BackendProgram
unknownBaseProgram =
  programWithMainExpr mysteryTy (BackendVar mysteryTy "main")

rollMismatchProgram :: BackendProgram
rollMismatchProgram =
  programWithMainExpr recIntTy (BackendRoll recIntTy (intLit 1))

addBinding :: BackendBinding
addBinding =
  BackendBinding
    { backendBindingName = "add",
      backendBindingType = binaryIntTy,
      backendBindingExpr =
        BackendLam
          { backendExprType = binaryIntTy,
            backendParamName = "x",
            backendParamType = intTy,
            backendBody =
              BackendLam
                { backendExprType = unaryIntTy,
                  backendParamName = "y",
                  backendParamType = intTy,
                  backendBody = BackendVar intTy "x"
                }
          },
      backendBindingExportedAsMain = False
    }

helperBinding :: BackendBinding
helperBinding =
  BackendBinding
    { backendBindingName = "helper",
      backendBindingType = unaryIntTy,
      backendBindingExpr = intIdentityExpr,
      backendBindingExportedAsMain = False
    }

intIdentityExpr :: BackendExpr
intIdentityExpr =
  BackendLam
    { backendExprType = unaryIntTy,
      backendParamName = "x",
      backendParamType = intTy,
      backendBody = BackendVar intTy "x"
    }

fnBoxData :: BackendData
fnBoxData =
  BackendData
    { backendDataName = "FnBox",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "FnBox" [] [unaryIntTy] fnBoxTy]
    }

lazyFieldBoxData :: BackendData
lazyFieldBoxData =
  BackendData
    { backendDataName = "LazyFieldBox",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            "Packed"
            []
            [BTForall "a" Nothing (BTVar "a"), intTy]
            lazyFieldBoxTy
        ]
    }

programWithMainExpr :: BackendType -> BackendExpr -> BackendProgram
programWithMainExpr mainTy expr =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "main",
          backendBindingType = mainTy,
          backendBindingExpr = expr,
          backendBindingExportedAsMain = True
        }
    ]

programWithBindings :: [BackendBinding] -> BackendProgram
programWithBindings bindings =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings = bindings
            }
        ],
      backendProgramMain = "main"
    }

intLit :: Integer -> BackendExpr
intLit value =
  BackendLit intTy (LInt value)

intTy :: BackendType
intTy =
  BTBase (BaseTy "Int")

stringTy :: BackendType
stringTy =
  BTBase (BaseTy "String")

optionTy :: BackendType -> BackendType
optionTy ty =
  BTCon (BaseTy "Option") (ty :| [])

unaryIntTy :: BackendType
unaryIntTy =
  BTArrow intTy intTy

binaryIntTy :: BackendType
binaryIntTy =
  BTArrow intTy unaryIntTy

fnBoxTy :: BackendType
fnBoxTy =
  BTBase (BaseTy "FnBox")

lazyFieldBoxTy :: BackendType
lazyFieldBoxTy =
  BTBase (BaseTy "LazyFieldBox")

aUnderscoreTy :: BackendType
aUnderscoreTy =
  BTBase (BaseTy "A_B")

aPrimeTy :: BackendType
aPrimeTy =
  BTBase (BaseTy "A'B")

mysteryTy :: BackendType
mysteryTy =
  BTBase (BaseTy "Mystery")

recIntTy :: BackendType
recIntTy =
  BTMu "self" intTy

data LLVMParityExpectation
  = ExpectLLVMAssembly
  | ExpectLLVMUnsupported String

llvmParityExpectations :: Map.Map String LLVMParityExpectation
llvmParityExpectations =
  Map.fromList
    [ ( runtimeCaseName runtimeCase,
        case Map.lookup (runtimeCaseName runtimeCase) unsupportedByName of
          Just reason -> ExpectLLVMUnsupported reason
          Nothing -> ExpectLLVMAssembly
      )
    | runtimeCase <- programSpecToLLVMParityCases
    ]
  where
    unsupportedByName = Map.fromList unsupportedLLVMParityCases

unsupportedLLVMParityCases :: [(String, String)]
unsupportedLLVMParityCases =
  [ ("surface: runs first-class polymorphic top-level argument", "Unsupported backend LLVM type at parameter \"$poly#0\" of FirstClassPolymorphism__usePoly"),
    ("surface: runs first-class polymorphic local argument", "could not infer type arguments [\"a\"]"),
    ("boundary: runs overloaded method dispatch with ordinary nullary constructors", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: runs overloaded method dispatch with nested ordinary constructors", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: runs overloaded method dispatch with lambda/application-inferred argument", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: runs overloaded method dispatch with let-polymorphism-inferred argument", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: runs overloaded method dispatch with explicit argument annotation", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: runs overloaded method dispatch on pattern-bound variable", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: constructor argument inferred as first-class polymorphic value should run", "escaping type abstraction"),
    ("boundary: local first-class polymorphic let through constructor boundary should run", "could not infer type arguments [\"a\"]"),
    ("boundary: pattern-bound first-class polymorphic variable should run", "could not infer type arguments [\"a\"]"),
    ("boundary: partial overloaded method application should run after deferred resolution", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: runs constrained helper through hidden Eq evidence", "BackendVariableTypeMismatch \"Main__Eq__Nat__eq\""),
    ("boundary: runs ground constrained helper alias with resolved evidence", "Unsupported backend LLVM type at parameter \"$evidence_Eq_eq#0\" of Main__sameBool"),
    ("boundary: runs constrained helper after local lambda inference", "escaping function \"Main__Eq__Bool__eq\""),
    ("boundary: runs deferred method with method-level type variable constraint", "BackendTypeCheckFailed"),
    ("boundary: runs partial deferred method after method-local evidence is fixed by application", "BackendTypeCheckFailed"),
    ("boundary: runs deferred method when only a later forall binder is inferred", "BackendUnsupportedInstantiation InstElim"),
    ("boundary: runs constrained helper with method-local evidence fixed by call args", "BackendTypeCheckFailed"),
    ("boundary: runs deferred class method with method-level Eq constraint", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: runs constrained helper through method-level evidence constraints", "BackendApplicationResultMismatch"),
    ("boundary: runs explicit constrained parameterized Eq instance", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: runs parameterized deriving Eq for Option", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("boundary: runs parameterized deriving Eq for recursive List", "BackendUnsupportedRecursiveLet"),
    ("boundary: runs qualified import with alias-only value and constructor access", "Unsupported backend LLVM type at return type of Core__Eq__Nat__eq"),
    ("boundary: runs aliased import with exposed method and qualified constructors", "Unsupported backend LLVM type at return type of Core__Eq__Nat__eq"),
    ("boundary: runs aliased import exposing a type without duplicate alias-head instance matches", "Unsupported backend LLVM type at return type of Core__Eq__Nat__eq"),
    ("boundary: deduplicates equivalent instances from mixed unqualified and aliased imports", "Unsupported backend LLVM type at return type of Core__Eq__Nat__eq"),
    ("boundary: deduplicates constrained imported instances from mixed unqualified and aliased imports", "escaping function \"Core__Eq__Bool__eq\""),
    ("fixture: test/programs/recursive-adt/deriving-eq.mlfp", "Unsupported backend LLVM type at return type of DerivingEq__Eq__Nat__eq"),
    ("fixture: test/programs/recursive-adt/recursive-tree-deriving.mlfp", "Unsupported backend LLVM type at return type of RecursiveTreeDeriving__Eq__Tree__eq"),
    ("fixture: test/programs/recursive-adt/complex-recursive-program.mlfp", "Unsupported backend LLVM type at return type of ComplexRecursiveProgram__Eq__Nat__eq"),
    ("fixture: test/programs/recursive-adt/module-integrated.mlfp", "Unsupported backend LLVM type at return type of Core__Eq__Nat__eq"),
    ("unified fixture: test/programs/unified/authoritative-overloaded-method.mlfp", "Unsupported backend LLVM type at return type of Main__Eq__Nat__eq"),
    ("unified fixture: test/programs/unified/first-class-polymorphism.mlfp", "Unsupported backend LLVM type at parameter \"$poly#0\" of FirstClassPolymorphism__usePoly"),
    ("standalone: allows importing a module declared later in the file", "Unsupported backend LLVM type at return type of Core__Eq__Nat__eq"),
    ("standalone: does not decode typed non-data constructor fields through fallback ADT decoding", "escaping lambda"),
    ("standalone: evaluates a recursive Nat equality example at representative depth", "Unsupported backend LLVM type at return type of Baseline__Eq__Nat__eq")
  ]

expectedLLVMUnsupportedParityCount :: Int
expectedLLVMUnsupportedParityCount =
  38

llvmUnsupportedParityCount :: Int
llvmUnsupportedParityCount =
  length
    [ ()
    | ExpectLLVMUnsupported _ <- Map.elems llvmParityExpectations
    ]

llvmObjectCodeParityCases :: [String]
llvmObjectCodeParityCases =
  [ "surface: runs lambda/application",
    "unified fixture: test/programs/unified/authoritative-case-analysis.mlfp",
    "unified fixture: test/programs/unified/authoritative-recursive-let.mlfp",
    "boundary: runs value-exported constructor when owner type is not exported",
    "boundary: runs aliased bulk-imported hidden-owner constructors in one case"
  ]

llvmObjectCodeParityCaseNames :: Set.Set String
llvmObjectCodeParityCaseNames =
  Set.fromList llvmObjectCodeParityCases

runLLVMParityCase :: ProgramRuntimeCase -> Spec
runLLVMParityCase runtimeCase =
  it (runtimeCaseName runtimeCase) $ do
    result <- emitProgramRuntimeLLVM (runtimeCaseSource runtimeCase)
    case Map.lookup (runtimeCaseName runtimeCase) llvmParityExpectations of
      Nothing ->
        expectationFailure ("missing LLVM parity expectation for " ++ runtimeCaseName runtimeCase)
      Just ExpectLLVMAssembly -> do
        output <- requireRight result
        validateLLVMAssembly output
        when (runtimeCaseName runtimeCase `Set.member` llvmObjectCodeParityCaseNames) $
          validateLLVMObjectCode output
      Just (ExpectLLVMUnsupported expectedFragment) ->
        case result of
          Left err ->
            err `shouldSatisfy` isInfixOf expectedFragment
          Right output ->
            expectationFailure $
              "LLVM parity case is now supported; remove its ExpectLLVMUnsupported classification:\n"
                ++ output

emitProgramRuntimeLLVM :: ProgramMatrixSource -> IO (Either String String)
emitProgramRuntimeLLVM source =
  case source of
    InlineProgram programText ->
      withTempProgram programText emitBackendFile
    ProgramFile path ->
      emitBackendFile path

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

shouldSatisfyLeft :: Either BackendLLVMError String -> (String -> Bool) -> Expectation
shouldSatisfyLeft result predicate =
  case result of
    Left err ->
      renderBackendLLVMError err `shouldSatisfy` predicate
    Right output ->
      expectationFailure ("expected backend LLVM failure, got output:\n" ++ output)

goldenText :: FilePath -> String -> Expectation
goldenText goldenPath actual = do
  expected <- readFile goldenPath
  length expected `seq` actual `shouldBe` expected
