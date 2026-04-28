{-# LANGUAGE LambdaCase #-}

module BackendLLVMSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (filterM)
import Data.List (isInfixOf)
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

unaryIntTy :: BackendType
unaryIntTy =
  BTArrow intTy intTy

binaryIntTy :: BackendType
binaryIntTy =
  BTArrow intTy unaryIntTy

fnBoxTy :: BackendType
fnBoxTy =
  BTBase (BaseTy "FnBox")

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
  llvmAs <- requireTool "llvm-as"
  withTempLLVM output $ \path -> do
    (exitCode, _stdout, stderr) <- readProcessWithExitCode llvmAs ["-o", "/dev/null", path] ""
    case exitCode of
      ExitSuccess -> pure ()
      ExitFailure _ -> expectationFailure ("llvm-as rejected backend output:\n" ++ stderr)

validateLLVMObjectCode :: String -> Expectation
validateLLVMObjectCode output = do
  llc <- requireTool "llc"
  withTempLLVM output $ \path -> do
    (exitCode, _stdout, stderr) <- readProcessWithExitCode llc ["-filetype=obj", "-o", "/dev/null", path] ""
    case exitCode of
      ExitSuccess -> pure ()
      ExitFailure _ -> expectationFailure ("llc rejected backend output:\n" ++ stderr)

requireTool :: String -> IO FilePath
requireTool name = do
  envCandidates <- filterM doesFileExist =<< traverseMaybeEnv (toolEnvNames name)
  case envCandidates of
    path : _ -> pure path
    [] -> do
      result <- findExecutable name
      case result of
        Just path -> pure path
        Nothing -> requireKnownLLVMTool name knownLLVMToolPaths

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

requireKnownLLVMTool :: String -> [FilePath] -> IO FilePath
requireKnownLLVMTool name paths = do
  existing <- filterM doesFileExist [path | path <- paths, takeFileName path == name]
  case existing of
    path : _ -> pure path
    [] -> expectationFailure ("required LLVM tool not found: " ++ name) >> fail ("missing " ++ name)

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
