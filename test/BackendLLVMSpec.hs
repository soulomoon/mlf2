{-# LANGUAGE LambdaCase #-}

module BackendLLVMSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (filterM)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import System.Directory (doesFileExist, findExecutable, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName)
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

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

  it "lowers Nat construction and case analysis to heap tags and switch" $ do
    output <- requireRight =<< emitBackendFile "test/programs/unified/authoritative-case-analysis.mlfp"

    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldSatisfy` isInfixOf "switch i64"
    output `shouldSatisfy` isInfixOf "phi i64"
    validateLLVMAssembly output
    validateLLVMObjectCode output

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

withTempProgram :: String -> (FilePath -> IO a) -> IO a
withTempProgram contents action = do
  tempDir <- getTemporaryDirectory
  bracket (writeTempProgram tempDir) removeFile action
  where
    writeTempProgram tempDir = do
      (path, handle) <- openTempFile tempDir "mlf2-backend-llvm.mlfp"
      hPutStr handle contents
      hClose handle
      pure path

validateLLVMAssembly :: String -> Expectation
validateLLVMAssembly output = do
  mbLlvmAs <- findLLVMTool "llvm-as"
  case mbLlvmAs of
    Nothing ->
      pendingWith "required LLVM tool not found: llvm-as"
    Just llvmAs ->
      withTempLLVM output $ \path -> do
        (exitCode, stderr) <- runLLVMTool llvmAs ["-o", "/dev/null", path]
        case exitCode of
          ExitSuccess -> pure ()
          ExitFailure _ -> expectationFailure ("llvm-as rejected backend output:\n" ++ stderr)

validateLLVMObjectCode :: String -> Expectation
validateLLVMObjectCode output = do
  mbLlc <- findLLVMTool "llc"
  case mbLlc of
    Nothing ->
      pendingWith "required LLVM tool not found: llc"
    Just llc ->
      withTempLLVM output $ \path -> do
        (exitCode, stderr) <- runLLVMTool llc ["-filetype=obj", "-o", "/dev/null", path]
        case exitCode of
          ExitSuccess -> pure ()
          ExitFailure _ -> expectationFailure ("llc rejected backend output:\n" ++ stderr)

runLLVMTool :: FilePath -> [String] -> IO (ExitCode, String)
runLLVMTool tool args = do
  (plainExitCode, _plainStdout, plainStderr) <- readProcessWithExitCode tool args ""
  case plainExitCode of
    ExitSuccess -> pure (ExitSuccess, "")
    ExitFailure _ -> do
      (opaqueExitCode, _opaqueStdout, opaqueStderr) <-
        readProcessWithExitCode tool ("-opaque-pointers" : args) ""
      pure $
        case opaqueExitCode of
          ExitSuccess -> (ExitSuccess, "")
          ExitFailure _ ->
            ( opaqueExitCode,
              plainStderr
                ++ "\nWith -opaque-pointers:\n"
                ++ opaqueStderr
            )

findLLVMTool :: String -> IO (Maybe FilePath)
findLLVMTool name = do
  envCandidates <- filterM doesFileExist =<< traverseMaybeEnv (toolEnvNames name)
  case envCandidates of
    path : _ -> pure (Just path)
    [] -> do
      result <- findExecutable name
      case result of
        Just path -> pure (Just path)
        Nothing -> findKnownLLVMTool name knownLLVMToolPaths

traverseMaybeEnv :: [String] -> IO [FilePath]
traverseMaybeEnv names = do
  values <- traverse lookupEnv names
  pure [path | Just path <- values]

toolEnvNames :: String -> [String]
toolEnvNames name =
  case name of
    "llvm-as" -> ["LLVM_AS"]
    "llc" -> ["LLC", "LLVM_LLC"]
    _ -> []

findKnownLLVMTool :: String -> [FilePath] -> IO (Maybe FilePath)
findKnownLLVMTool name paths = do
  existing <- filterM doesFileExist [path | path <- paths, takeFileName path == name]
  case existing of
    path : _ -> pure (Just path)
    [] -> pure Nothing

knownLLVMToolPaths :: [FilePath]
knownLLVMToolPaths =
  [ "/opt/homebrew/opt/llvm/bin/llvm-as",
    "/opt/homebrew/opt/llvm/bin/llc",
    "/usr/local/opt/llvm/bin/llvm-as",
    "/usr/local/opt/llvm/bin/llc"
  ]

withTempLLVM :: String -> (FilePath -> IO a) -> IO a
withTempLLVM output action = do
  tempDir <- getTemporaryDirectory
  bracket (writeTempLLVM tempDir) removeFile action
  where
    writeTempLLVM tempDir = do
      (path, handle) <- openTempFile tempDir "mlf2-backend-llvm.ll"
      hPutStr handle output
      hClose handle
      pure path
