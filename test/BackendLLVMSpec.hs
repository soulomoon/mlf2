{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module BackendLLVMSpec (spec) where

import BackendIRTestSupport
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import Test.Hspec

import CheckedProgramTestSupport
  ( CheckedProgramArtifact,
    checkedArtifactBackendLLVM,
    checkedArtifactCheckOutput,
    checkedArtifactNativeLLVM,
    checkedArtifactRunOutput,
    checkedProgramArtifactFromFile,
    checkedProgramArtifactFromSource,
    runCheckedProgramArtifact,
  )
import LLVMToolSupport
  ( NativeRunResult (..),
    parseExecutableCommand,
    runLLVMNativeExecutable,
    validateLLVMAssembly,
    validateLLVMObjectCode,
    withTempProgram,
  )
import MLF.Backend.IR hiding
  ( BTBase,
    BTCon,
    BTForall,
    BTMu,
    BTVar,
    BTVarApp,
    BackendBinding,
    backendBindingExpr,
    backendBindingExportedAsMain,
    backendBindingName,
    backendBindingType,
    BackendClosure,
    backendClosureParams,
    BackendConstruct,
    BackendConstructor,
    backendConstructorFields,
    backendConstructorForalls,
    backendConstructorName,
    BackendConstructorPattern,
    backendConstructorResult,
    BackendData,
    backendDataConstructors,
    backendDataName,
    backendDataParameters,
    BackendLam,
    BackendLet,
    BackendModule,
    backendModuleBindings,
    backendModuleData,
    backendModuleName,
    BackendProgram,
    backendProgramMain,
    backendProgramModules,
    BackendTyAbs,
    BackendTypeBinder,
    BackendVar
  )
import MLF.Backend.IR
  ( type BackendBinding,
    type BackendConstructor,
    type BackendData,
    type BackendProgram,
  )
import MLF.Backend.LLVM hiding (renderBackendProgramLLVM, renderBackendProgramNativeLLVM)
import qualified MLF.Backend.LLVM as BackendLLVM
import qualified MLF.Backend.LLVM.Lower as Lower
import MLF.Backend.LLVM.Lower.Types
  ( BindingInfo (..),
    ClosureCaptureSlot (..),
    ClosureEntry (..),
    ClosureEntryOrigin (..),
    ConstructorRuntime (..),
    DataRuntime (..),
    ExprEnv (..),
    FunctionParam (..),
    FunctionForm (..),
    LocalFunction (..),
    LowerValueKind (..),
    NativeRenderSpec (..),
    ProgramBase (..),
    SpecRequest (..),
    Specialization (..),
    Wrapper (..),
    WrapperKind (..),
    backendBindingRefFromGenerated,
    backendBindingRefFromIdentity,
    backendTypeIdentityKey,
    closureEntryIdentityKey,
    constructedFieldValueKind,
    constructedValueForConstructor,
    constructorValueKeyFromIdentity,
    lookupProgramBindingByIdentityExact,
    lookupProgramConstructorByIdentityExact,
    lookupProgramDataByIdentityExact,
    mergeConstructedValues,
    specializationIdentityKey,
    wrapperIdentityKey,
  )
import MLF.Backend.LLVM.Ppr (renderLLVMModule)
import MLF.Backend.LLVM.Syntax (LLVMModule)
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.API (parseRawProgram, renderProgramParseError)
import MLF.Frontend.Program.Checked (CheckedProgram)
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), SymbolOwnerIdentity (..), renameSymbolDefiningName, symbolIdentityFromParts, symbolUniqueIdentity)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Pipeline (checkProgram)
import qualified MLF.Program.CLI as CLI
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (IdDetails (..), LocalIdentity (..), StructuralTypeBinderRole (..), TypeBinderIdentity, idDetailsStableName, initialIdentityGenerator, localRefFromIdentity, primitiveRefFromSymbol, typeBinderIdentityFromStructural, typeBinderIdentityFromUnique, typeBinderIdentityStableName)
import MLF.Types.Unique (UniqueIdentity (..))
import Parity.ProgramMatrix
  ( ProgramMatrixSource (..),
    ProgramRuntimeExpectation (..),
  )
import Parity.ProgramMatrix.NativePolicy
  ( BackendLLVMAssemblyPolicy (..),
    InterpreterRuntimePolicy (..),
    NativeRunPolicy (..),
    ObjectCodePolicy (..),
    ProgramLLVMNativeParityPolicy,
    describeProgramLLVMNativeParityPolicy,
    parityBackendLLVMAssembly,
    parityBackendLLVMForbiddenFragments,
    parityBackendLLVMRequiredFragments,
    parityCaseSource,
    parityExpectedRuntime,
    parityInterpreterRuntime,
    parityNativeRun,
    parityObjectCode,
    programLLVMNativeParityPolicies,
    programLLVMNativeParityPolicyDiagnostics,
  )

renderBackendProgramLLVM :: BackendProgram -> Either BackendLLVMError String
renderBackendProgramLLVM program =
  case mkProductionBackendProgram program of
    Left err -> Left (BackendLLVMLoweringFailed (Lower.BackendLLVMValidationFailed err))
    Right productionProgram -> BackendLLVM.renderBackendProgramLLVM productionProgram

renderBackendProgramNativeLLVM :: BackendProgram -> Either BackendLLVMError String
renderBackendProgramNativeLLVM program =
  case mkProductionBackendProgram program of
    Left err -> Left (BackendLLVMLoweringFailed (Lower.BackendLLVMValidationFailed err))
    Right productionProgram -> BackendLLVM.renderBackendProgramNativeLLVM productionProgram

lowerTestBackendProgram :: BackendProgram -> Either Lower.BackendLLVMError LLVMModule
lowerTestBackendProgram program =
  case mkProductionBackendProgram program of
    Left err -> Left (Lower.BackendLLVMValidationFailed err)
    Right productionProgram -> Lower.lowerBackendProgram productionProgram

spec :: Spec
spec = describe "MLF.Backend.LLVM" $ do
  it "renders converted checked functions as deterministic LLVM IR" $ do
    checked <- requireChecked simpleFunctionProgram
    output <- requireRight (renderCheckedProgramLLVM checked)

    goldenText "test/golden/backend-simple-function.ll.golden" output
    validateLLVMAssembly output

  it "emits LLVM IR from the CLI file entrypoint" $ do
    output <- requireRight =<< CLI.emitBackendFile "test/programs/unified/authoritative-let-polymorphism.mlfp"

    output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
    output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
    validateLLVMAssembly output

  describe "IO backend contract" $ do
    it "accepts checked main : IO Unit and emits native LLVM" $ do
      output <- requireRight =<< emitNativeSource ioPureUnitMainProgram

      output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
      validateLLVMAssembly output

    it "accepts primitive IO operations and emits native LLVM" $ do
      output <- requireRight =<< emitNativeSource ioPutStrLnMainProgram
      output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_putStrLn.wrapper\""
      validateLLVMAssembly output

    it "derives native IO wrapper coverage from the primitive inventory" $ do
      output <- requireRight =<< emitNativeSource ioPutStrLnMainProgram
      output `shouldSatisfy` isInfixOf ("define i1 @\"" ++ PrimitiveInventory.nativeAndPrimitiveName ++ "\"")
      forM_ (Set.toList PrimitiveInventory.nativeIOPrimitiveNames) $ \name ->
        output `shouldSatisfy` isInfixOf ("define private ptr @\"" ++ name ++ ".wrapper\"")
      validateLLVMAssembly output

    it "executes __io_bind main through the native IO runtime" $ do
      output <- requireRight =<< emitNativeSource ioDirectPrimitiveMainProgram
      output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_bind.wrapper\""
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_putStrLn.wrapper\""
      validateLLVMAssembly output
      validateLLVMObjectCode output
      runLLVMNativeExecutable output
        `shouldReturn` NativeRunResult ExitSuccess "world\n" ""

    it "executes Prelude Monad IO methods through the native IO runtime" $ do
      output <- requireRight =<< emitNativeSource ioPreludeMonadMainProgram
      output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_bind.wrapper\""
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_pure.wrapper\""
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_putStrLn.wrapper\""
      validateLLVMAssembly output
      validateLLVMObjectCode output
      runLLVMNativeExecutable output
        `shouldReturn` NativeRunResult ExitSuccess "prelude\n" ""

    it "executes Prelude Functor IO map through the native IO wrapper" $ do
      output <- requireRight =<< emitNativeSource ioPreludeFunctorApplicativeMainProgram
      output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_bind.wrapper\""
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_pure.wrapper\""
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_map.wrapper\""
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_putStrLn.wrapper\""
      validateLLVMAssembly output
      validateLLVMObjectCode output
      runLLVMNativeExecutable output
        `shouldReturn` NativeRunResult ExitSuccess "map\n" ""

    it "executes nested __io_bind / __io_putStrLn actions in written order through the native IO runtime" $ do
      output <- requireRight =<< emitNativeSource ioNestedPrimitiveMainProgram
      output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_bind.wrapper\""
      output `shouldSatisfy` isInfixOf "call ptr @\"__io_putStrLn.wrapper\""
      validateLLVMAssembly output
      validateLLVMObjectCode output
      runLLVMNativeExecutable output
        `shouldReturn` NativeRunResult ExitSuccess "first\nsecond\nthird\n" ""

    it "executes __io_putStr without trailing newline" $ do
      output <- requireRight =<< emitNativeSource ioPutStrMainProgram
      validateLLVMAssembly output
      validateLLVMObjectCode output
      runLLVMNativeExecutable output
        `shouldReturn` NativeRunResult ExitSuccess "hello" ""

    it "executes __io_writeFile through the native IO runtime" $ do
      output <- requireRight =<< emitNativeSource ioWriteFileMainProgram
      validateLLVMAssembly output
      validateLLVMObjectCode output
      result <- runLLVMNativeExecutable output
      nativeRunExitCode result `shouldBe` ExitSuccess
      -- Verify the file was written
      contents <- readFile "/tmp/mlf2-test-write.txt"
      contents `shouldBe` "hello from mlfp"

    it "executes __io_appendFile through the native IO runtime" $ do
      -- Write initial content, then append
      writeFile "/tmp/mlf2-test-append.txt" "base"
      output <- requireRight =<< emitNativeSource ioAppendFileMainProgram
      validateLLVMAssembly output
      validateLLVMObjectCode output
      result <- runLLVMNativeExecutable output
      nativeRunExitCode result `shouldBe` ExitSuccess
      contents <- readFile "/tmp/mlf2-test-append.txt"
      contents `shouldBe` "baseappended"

    it "executes __io_readFile through the native IO runtime" $ do
      -- Write a test file first
      writeFile "/tmp/mlf2-test-read-input.txt" "read me please"
      output <- requireRight =<< emitNativeSource ioReadFileMainProgram
      validateLLVMAssembly output
      validateLLVMObjectCode output
      runLLVMNativeExecutable output
        `shouldReturn` NativeRunResult ExitSuccess "read me please\n" ""

    it "executes __io_exitWith with the given exit code" $ do
      output <- requireRight =<< emitNativeSource ioExitWithMainProgram
      validateLLVMAssembly output
      validateLLVMObjectCode output
      result <- runLLVMNativeExecutable output
      nativeRunExitCode result `shouldBe` ExitFailure 42

    it "executes __io_newIORef through the native IO runtime" $ do
      output <- requireRight =<< emitNativeSource ioNewIORefMainProgram
      validateLLVMAssembly output
      validateLLVMObjectCode output
      result <- runLLVMNativeExecutable output
      nativeRunExitCode result `shouldBe` ExitSuccess

    it "emits valid LLVM for __io_writeIORef" $ do
      output <- requireRight =<< emitBackendSource ioWriteIORefProgram
      output `shouldSatisfy` isInfixOf "__io_writeIORef.wrapper"
      validateLLVMAssembly output

    it "emits valid LLVM for __io_readIORef" $ do
      output <- requireRight =<< emitBackendSource ioReadIORefProgram
      output `shouldSatisfy` isInfixOf "__io_readIORef.wrapper"
      validateLLVMAssembly output

    it "executes __io_getArgs through the native IO runtime" $ do
      output <- requireRight =<< emitNativeSource ioGetArgsProgram
      validateLLVMAssembly output
      validateLLVMObjectCode output
      result <- runLLVMNativeExecutable output
      nativeRunExitCode result `shouldBe` ExitSuccess

    it "accepts pure mains that depend on IO-typed helpers" $ do
      output <- requireRight =<< emitBackendSource pureMainIODependencyProgram

      output `shouldSatisfy` isInfixOf "Main__main"
      validateLLVMAssembly output

    it "accepts pure mains that directly reference opaque IO primitives" $ do
      output <- requireRight =<< emitNativeSource pureMainDirectIOPrimitiveProgram

      output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
      validateLLVMAssembly output

    it "accepts pure mains when IO-typed bindings are unused" $ do
      output <- requireRight =<< emitBackendSource pureMainUnusedIOProgram

      output `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
      validateLLVMAssembly output

  describe "native process entrypoint" $ do
    it "prints an Int main value to stdout and exits successfully" $
      assertNativeProgram simpleFunctionProgram "1"

    it "prints a Bool main value to stdout and exits successfully" $
      assertNativeProgram boolMainProgram "true"

    it "prints nested first-order ADT values with ProgramSpec rendering" $
      assertNativeProgram nativeNestedAdtProgram "Some (Succ Zero)"

    it "preserves user-authored double underscores in constructor names" $
      assertNativeProgram nativeDoubleUnderscoreConstructorProgram "A__B"

    it "links the backend-owned __mlfp_and runtime primitive in native mode" $
      assertNativeProgram preludeAndProgram "false"

    it "renders String main values with quoted escaping in native mode" $
      assertNativeProgram nativeStringSourceProgram "\"hello\""

    it "Unicode String literal source checks, runs, emits backend, and executes natively" $
      withTempProgram nativeUnicodeStringSourceProgram $ \path -> do
        artifact <- requireRight =<< checkedProgramArtifactFromFile path
        checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
        checkedArtifactRunOutput artifact `shouldBe` Right "\"\\955\"\n"

        backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
        backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
        validateLLVMAssembly backendOutput
        validateLLVMObjectCode backendOutput

        nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
        nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
        validateLLVMAssembly nativeOutput
        validateLLVMObjectCode nativeOutput
        runLLVMNativeExecutable nativeOutput
          `shouldReturn` NativeRunResult ExitSuccess "\"\\955\"\n" ""

    it "Unicode stringLength source checks, runs, emits backend, and executes natively" $
      withTempProgram nativeUnicodeStringLengthSourceProgram $ \path -> do
        artifact <- requireRight =<< checkedProgramArtifactFromFile path
        checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
        checkedArtifactRunOutput artifact `shouldBe` Right "2\n"

        backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
        backendOutput `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
        validateLLVMAssembly backendOutput
        validateLLVMObjectCode backendOutput

        nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
        nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
        validateLLVMAssembly nativeOutput
        validateLLVMObjectCode nativeOutput
        runLLVMNativeExecutable nativeOutput
          `shouldReturn` NativeRunResult ExitSuccess "2\n" ""

    it "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" $
      forM_
        [ (nativeEmptyStringIsEmptySourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeUnicodeStringIsEmptySourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringContainsChar searches Unicode scalars through native execution" $
      forM_
        [ (nativePresentStringContainsCharSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAbsentStringContainsCharSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringContains searches Unicode substrings through native execution" $
      forM_
        [ (nativePresentStringContainsSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAbsentStringContainsSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    {- Round 301 public fixtures:
       stringEquals "aλ" "aλ"
       stringEquals "aλ" "a"
       stringEquals "" ""
       stringEquals "a\0b" "a"
       stringEquals (stringAppend "a" "\0b") "a"
    -}
    it "stringEquals compares Unicode scalar strings through native execution" $
      forM_
        [ (nativeEqualStringEqualsSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeUnequalStringEqualsSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeEmptyStringEqualsSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeEmbeddedNulStringEqualsSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeAppendEmbeddedNulStringEqualsSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringStartsWith classifies Unicode prefixes through native execution" $
      forM_
        [ (nativePresentStringStartsWithSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAbsentStringStartsWithSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringEndsWith classifies Unicode suffixes through native execution" $
      forM_
        [ (nativePresentStringEndsWithSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAbsentStringEndsWithSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringAppend concatenates Unicode scalar strings through native execution" $
      forM_
        [ (nativeUnicodeStringAppendSourceProgram, "\"a\\955b\"\n", NativeRunResult ExitSuccess "\"a\\955b\"\n" ""),
          (nativeLeftEmptyStringAppendSourceProgram, "\"\\955\"\n", NativeRunResult ExitSuccess "\"\\955\"\n" ""),
          (nativeRightEmptyStringAppendSourceProgram, "\"\\955\"\n", NativeRunResult ExitSuccess "\"\\955\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringReplaceChar replaces Unicode scalar characters through native execution" $
      forM_
        [ (nativeUnicodeStringReplaceCharSourceProgram, "\"axbx\"\n", NativeRunResult ExitSuccess "\"axbx\"\n" ""),
          (nativeNoMatchStringReplaceCharSourceProgram, "\"ab\"\n", NativeRunResult ExitSuccess "\"ab\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringReplace replaces Unicode scalar substrings through native execution" $
      forM_
        [ (nativeUnicodeStringReplaceSourceProgram, "\"aWXYZWXYZ\"\n", NativeRunResult ExitSuccess "\"aWXYZWXYZ\"\n" ""),
          (nativeNoMatchStringReplaceSourceProgram, "\"abc\"\n", NativeRunResult ExitSuccess "\"abc\"\n" ""),
          (nativeEmptyNeedleStringReplaceSourceProgram, "\"abc\"\n", NativeRunResult ExitSuccess "\"abc\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringIndexOfChar indexes Unicode scalar characters through native execution" $
      forM_
        [ (nativePresentStringIndexOfCharSourceProgram, "Some 1\n", NativeRunResult ExitSuccess "Some 1\n" ""),
          (nativeAbsentStringIndexOfCharSourceProgram, "None\n", NativeRunResult ExitSuccess "None\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringIndexOf indexes Unicode scalar substrings through native execution" $
      forM_
        [ (nativePresentStringIndexOfSourceProgram, "Some 1\n", NativeRunResult ExitSuccess "Some 1\n" ""),
          (nativeAbsentStringIndexOfSourceProgram, "None\n", NativeRunResult ExitSuccess "None\n" ""),
          (nativeEmptyNeedleStringIndexOfSourceProgram, "Some 0\n", NativeRunResult ExitSuccess "Some 0\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringSplit splits Unicode scalar substrings through native execution" $
      forM_
        [ (nativeUnicodeStringSplitSourceProgram, "Cons \"a\" (Cons \"b\" (Cons \"c\" Nil))\n", NativeRunResult ExitSuccess "Cons \"a\" (Cons \"b\" (Cons \"c\" Nil))\n" ""),
          (nativeNoMatchStringSplitSourceProgram, "Cons \"abc\" Nil\n", NativeRunResult ExitSuccess "Cons \"abc\" Nil\n" ""),
          (nativeEmptyNeedleStringSplitSourceProgram, "Cons \"abc\" Nil\n", NativeRunResult ExitSuccess "Cons \"abc\" Nil\n" ""),
          (nativeEdgeEmptyStringSplitSourceProgram, "Cons \"\" (Cons \"a\" (Cons \"\" Nil))\n", NativeRunResult ExitSuccess "Cons \"\" (Cons \"a\" (Cons \"\" Nil))\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringFromChar converts Unicode scalar Chars to singleton strings through native execution" $
      forM_
        [ (nativeUnicodeStringFromCharSourceProgram, "\"\\955\"\n", NativeRunResult ExitSuccess "\"\\955\"\n" ""),
          (nativeAsciiStringFromCharSourceProgram, "\"A\"\n", NativeRunResult ExitSuccess "\"A\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringFromList converts List Char values to Unicode scalar strings through native execution" $
      forM_
        [ (nativeUnicodeStringFromListSourceProgram, "\"a\\955\"\n", NativeRunResult ExitSuccess "\"a\\955\"\n" ""),
          (nativeEmptyStringFromListSourceProgram, "\"\"\n", NativeRunResult ExitSuccess "\"\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "named functions returning List Char source check without PhiReorder binder identity failure" $
      withTempProgram namedListCharReturnSourceProgram $ \path ->
        checkProgramFile path `shouldReturn` Right "OK\n"

    it "stringToList converts Unicode scalar strings to List Char values through native execution" $
      forM_
        [ (nativeUnicodeStringToListSourceProgram, "Cons 'a' (Cons '\\955' Nil)\n", NativeRunResult ExitSuccess "Cons 'a' (Cons '\\955' Nil)\n" ""),
          (nativeEmptyStringToListSourceProgram, "Nil\n", NativeRunResult ExitSuccess "Nil\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringFromInt formats Int values as decimal strings through native execution" $
      forM_
        [ (nativePositiveStringFromIntSourceProgram, "\"42\"\n", NativeRunResult ExitSuccess "\"42\"\n" ""),
          (nativeZeroStringFromIntSourceProgram, "\"0\"\n", NativeRunResult ExitSuccess "\"0\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringFromBool formats Bool values as strings through native execution" $
      forM_
        [ (nativeTrueStringFromBoolSourceProgram, "\"true\"\n", NativeRunResult ExitSuccess "\"true\"\n" ""),
          (nativeFalseStringFromBoolSourceProgram, "\"false\"\n", NativeRunResult ExitSuccess "\"false\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringFromNat formats Nat values as decimal strings through native execution" $
      forM_
        [ (nativeZeroStringFromNatSourceProgram, "\"0\"\n", NativeRunResult ExitSuccess "\"0\"\n" ""),
          (nativeTwoStringFromNatSourceProgram, "\"2\"\n", NativeRunResult ExitSuccess "\"2\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringFromUnit formats Unit as a string through native execution" $
      withTempProgram nativeStringFromUnitSourceProgram $ \path -> do
        artifact <- requireRight =<< checkedProgramArtifactFromFile path
        checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
        checkedArtifactRunOutput artifact `shouldBe` Right "\"Unit\"\n"

        backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
        backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
        validateLLVMAssembly backendOutput
        validateLLVMObjectCode backendOutput

        nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
        nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
        validateLLVMAssembly nativeOutput
        validateLLVMObjectCode nativeOutput
        runLLVMNativeExecutable nativeOutput
          `shouldReturn` NativeRunResult ExitSuccess "\"Unit\"\n" ""

    it "stringDrop slices Unicode scalar prefixes through native execution" $
      forM_
        [ (nativeDropLeadingUnicodeStringSourceProgram, "\"ab\"\n", NativeRunResult ExitSuccess "\"ab\"\n" ""),
          (nativeDropMixedUnicodeStringSourceProgram, "\"b\"\n", NativeRunResult ExitSuccess "\"b\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringTake slices Unicode scalar prefixes through native execution" $
      forM_
        [ (nativeTakeLeadingUnicodeStringSourceProgram, "\"\\955\"\n", NativeRunResult ExitSuccess "\"\\955\"\n" ""),
          (nativeTakeMixedUnicodeStringSourceProgram, "\"a\\955\"\n", NativeRunResult ExitSuccess "\"a\\955\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringSlice slices Unicode scalar ranges through native execution" $
      forM_
        [ (nativeSliceMixedUnicodeStringSourceProgram, "\"\\955b\"\n", NativeRunResult ExitSuccess "\"\\955b\"\n" ""),
          (nativeSliceAfterLeadingUnicodeStringSourceProgram, "\"ab\"\n", NativeRunResult ExitSuccess "\"ab\"\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "stringCharAt indexes Unicode scalar cursor positions through native execution" $
      forM_
        [ (nativeCharAtMixedUnicodeStringSourceProgram, "'\\955'\n", NativeRunResult ExitSuccess "'\\955'\n" ""),
          (nativeCharAtAfterLeadingUnicodeStringSourceProgram, "'b'\n", NativeRunResult ExitSuccess "'b'\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i32 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    {- Round 300 public fixtures:
       stringCharAtOption "aλb" 1
       stringCharAtOption "λab" 2
       stringCharAtOption "λ" 1
       stringCharAtOption "" 0
    -}
    it "stringCharAtOption returns optional Unicode scalar cursor lookups through native execution" $
      forM_
        [ (nativeCharAtOptionMixedUnicodeStringSourceProgram, "Some '\\955'\n", NativeRunResult ExitSuccess "Some '\\955'\n" ""),
          (nativeCharAtOptionAfterLeadingUnicodeStringSourceProgram, "Some 'b'\n", NativeRunResult ExitSuccess "Some 'b'\n" ""),
          (nativeCharAtOptionEndOfInputSourceProgram, "None\n", NativeRunResult ExitSuccess "None\n" ""),
          (nativeCharAtOptionEmptyStringSourceProgram, "None\n", NativeRunResult ExitSuccess "None\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsDigit classifies decimal Char values through native execution" $
      forM_
        [ (nativeDecimalCharIsDigitSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeNonDecimalCharIsDigitSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsAsciiLower classifies ASCII lowercase Char values through native execution" $
      forM_
        [ (nativeAsciiLowercaseCharIsAsciiLowerSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUppercaseCharIsAsciiLowerSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeNonAsciiCharIsAsciiLowerSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsAsciiUpper classifies ASCII uppercase Char values through native execution" $
      forM_
        [ (nativeAsciiUppercaseCharIsAsciiUpperSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiLowercaseCharIsAsciiUpperSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeNonAsciiCharIsAsciiUpperSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsAsciiAlpha classifies ASCII alphabetic Char values through native execution" $
      forM_
        [ (nativeAsciiLowercaseCharIsAsciiAlphaSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUppercaseCharIsAsciiAlphaSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiDigitCharIsAsciiAlphaSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeNonAsciiCharIsAsciiAlphaSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsAsciiAlphaNum classifies ASCII alphanumeric Char values through native execution" $
      forM_
        [ (nativeAsciiLowercaseCharIsAsciiAlphaNumSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUppercaseCharIsAsciiAlphaNumSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiDigitCharIsAsciiAlphaNumSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUnderscoreCharIsAsciiAlphaNumSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeNonAsciiCharIsAsciiAlphaNumSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsAsciiIdentifierStart classifies ASCII identifier-start Char values through native execution" $
      forM_
        [ (nativeAsciiLowercaseCharIsAsciiIdentifierStartSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUppercaseCharIsAsciiIdentifierStartSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUnderscoreCharIsAsciiIdentifierStartSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiDigitCharIsAsciiIdentifierStartSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeAsciiApostropheCharIsAsciiIdentifierStartSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeNonAsciiCharIsAsciiIdentifierStartSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsAsciiIdentifierContinue classifies ASCII identifier-continuation Char values through native execution" $
      forM_
        [ (nativeAsciiLowercaseCharIsAsciiIdentifierContinueSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUppercaseCharIsAsciiIdentifierContinueSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiDigitCharIsAsciiIdentifierContinueSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUnderscoreCharIsAsciiIdentifierContinueSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiApostropheCharIsAsciiIdentifierContinueSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeNonAsciiCharIsAsciiIdentifierContinueSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsAsciiWhitespace classifies ASCII whitespace Char values through native execution" $
      forM_
        [ (nativeAsciiSpaceCharIsAsciiWhitespaceSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiTabCharIsAsciiWhitespaceSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiNewlineCharIsAsciiWhitespaceSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiCarriageReturnCharIsAsciiWhitespaceSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiFormFeedCharIsAsciiWhitespaceSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiVerticalTabCharIsAsciiWhitespaceSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiLowercaseCharIsAsciiWhitespaceSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeNonAsciiCharIsAsciiWhitespaceSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution" $
      forM_
        [ (nativeAsciiExclamationCharIsAsciiPunctuationSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUnderscoreCharIsAsciiPunctuationSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiTildeCharIsAsciiPunctuationSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiLowercaseCharIsAsciiPunctuationSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeAsciiDigitCharIsAsciiPunctuationSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeAsciiSpaceCharIsAsciiPunctuationSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeNonAsciiCharIsAsciiPunctuationSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "charIsAsciiPrintable classifies ASCII printable Char values through native execution" $
      forM_
        [ (nativeAsciiSpaceCharIsAsciiPrintableSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiExclamationCharIsAsciiPrintableSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiUppercaseCharIsAsciiPrintableSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiDigitCharIsAsciiPrintableSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiTildeCharIsAsciiPrintableSourceProgram, "true\n", NativeRunResult ExitSuccess "true\n" ""),
          (nativeAsciiTabCharIsAsciiPrintableSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeAsciiNewlineCharIsAsciiPrintableSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" ""),
          (nativeNonAsciiCharIsAsciiPrintableSourceProgram, "false\n", NativeRunResult ExitSuccess "false\n" "")
        ]
        $ \(programText, expectedOutput, expectedNativeResult) ->
          withTempProgram programText $ \path -> do
            artifact <- requireRight =<< checkedProgramArtifactFromFile path
            checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
            checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

            backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
            backendOutput `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
            validateLLVMAssembly backendOutput
            validateLLVMObjectCode backendOutput

            nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
            nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
            validateLLVMAssembly nativeOutput
            validateLLVMObjectCode nativeOutput
            runLLVMNativeExecutable nativeOutput
              `shouldReturn` expectedNativeResult

    it "broad string library exposes the rev-004 public API through native execution" $
      forM_
        [ ( nativePreludeMain "List(..), stringJoin" "String" "stringJoin \",\" (Cons \"a\" (Cons \"b\" Nil))",
            "\"a,b\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringSplitChar" "List String" "stringSplitChar \"aλb\" 'λ'",
            "Cons \"a\" (Cons \"b\" Nil)\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringCompare" "Int" "stringCompare \"a\" \"b\"",
            "-1\n",
            "define i64 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charIsAsciiHexDigit" "Bool" "charIsAsciiHexDigit 'f'",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charIsAsciiLineBreak" "Bool" "charIsAsciiLineBreak '\\n'",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charIsAsciiControl" "Bool" "charIsAsciiControl '\\t'",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charToAsciiLower" "Char" "charToAsciiLower 'A'",
            "'a'\n",
            "define i32 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charToAsciiUpper" "Char" "charToAsciiUpper 'z'",
            "'Z'\n",
            "define i32 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringToAsciiLower" "String" "stringToAsciiLower \"AλZ!\"",
            "\"a\\955z!\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringToAsciiUpper" "String" "stringToAsciiUpper \"aλz!\"",
            "\"A\\955Z!\"\n",
            "define ptr @\"Main__main\"()"
          )
        ]
        $ \(programText, expectedOutput, backendSignature) ->
          assertNativeProgramBehavior programText expectedOutput backendSignature

    it "broad string library fixes slicing and cursor boundary behavior through native execution" $
      forM_
        [ ( nativePreludeMain "stringDrop" "String" "stringDrop \"abc\" (-1)",
            "\"abc\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringTake" "String" "stringTake \"abc\" (-1)",
            "\"\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringTake" "String" "stringTake \"abc\" 0",
            "\"\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringDrop" "String" "stringDrop \"abc\" 3",
            "\"\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringDrop" "String" "stringDrop \"abc\" 99",
            "\"\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringTake" "String" "stringTake \"λab\" 99",
            "\"\\955ab\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringSlice" "String" "stringSlice \"abc\" (-1) 2",
            "\"ab\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringSlice" "String" "stringSlice \"abc\" 1 0",
            "\"\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringSlice" "String" "stringSlice \"abc\" 9 2",
            "\"\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringSlice" "String" "stringSlice \"aλbc\" 1 2",
            "\"\\955b\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "Option(..), stringCharAtOption" "Option Char" "stringCharAtOption \"abc\" (-1)",
            "None\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "Option(..), stringCharAtOption" "Option Char" "stringCharAtOption \"abc\" 3",
            "None\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "Option(..), stringCharAtOption" "Option Char" "stringCharAtOption \"\" 0",
            "None\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "Option(..), stringCharAtOption" "Option Char" "stringCharAtOption \"aλb\" 1",
            "Some '\\955'\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringLength, stringTake" "Int" "stringLength (stringTake \"a\\0b\" 3)",
            "3\n",
            "define i64 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringLength, stringDrop" "Int" "stringLength (stringDrop \"a\\0b\" 1)",
            "2\n",
            "define i64 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringLength, stringSlice" "Int" "stringLength (stringSlice \"a\\0b\" 1 2)",
            "2\n",
            "define i64 @\"Main__main\"()"
          ),
          ( nativePreludeMain "Option(..), stringCharAtOption, charIsAsciiControl" "Bool" "case stringCharAtOption \"a\\0b\" 1 of { None -> false; Some ch -> charIsAsciiControl ch }",
            "true\n",
            "define i1 @\"Main__main\"()"
          )
        ]
        $ \(programText, expectedOutput, backendSignature) ->
          assertNativeProgramBehavior programText expectedOutput backendSignature

    it "broad string library covers search split replace and join edges through native execution" $
      forM_
        [ ( nativePreludeMain "List(..), stringJoin" "String" "stringJoin \",\" Nil",
            "\"\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringJoin" "String" "stringJoin \",\" (Cons \"\" (Cons \"b\" (Cons \"\" Nil)))",
            "\",b,\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringSplit" "List String" "stringSplit \"λaλ\" \"λ\"",
            "Cons \"\" (Cons \"a\" (Cons \"\" Nil))\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringSplitChar" "List String" "stringSplitChar \"a::b:\" ':'",
            "Cons \"a\" (Cons \"\" (Cons \"b\" (Cons \"\" Nil)))\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringReplace" "String" "stringReplace \"aaaa\" \"aa\" \"b\"",
            "\"bb\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringReplace" "String" "stringReplace \"abc\" \"\" \"x\"",
            "\"abc\"\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringContainsChar" "Bool" "stringContainsChar \"a\\0b\" 'b'",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringContains" "Bool" "stringContains \"a\\0b\" \"b\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "Option(..), stringIndexOfChar" "Option Int" "stringIndexOfChar \"a\\0b\" 'b'",
            "Some 2\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "Option(..), stringIndexOf" "Option Int" "stringIndexOf \"a\\0bc\" \"bc\"",
            "Some 2\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringSplit" "List String" "stringSplit \"a\\0b\\0c\" \"\\0\"",
            "Cons \"a\" (Cons \"b\" (Cons \"c\" Nil))\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringSplitChar" "List String" "stringSplitChar \"a\\0b\\0c\" '\\0'",
            "Cons \"a\" (Cons \"b\" (Cons \"c\" Nil))\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringEquals, stringReplace" "Bool" "stringEquals (stringReplace \"a\\0b\" \"\\0\" \"x\") \"axb\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringEquals, stringReplaceChar" "Bool" "stringEquals (stringReplaceChar \"a\\0b\" '\\0' 'x') \"axb\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringEquals, stringJoin" "Bool" "stringEquals (stringJoin \"\\0\" (Cons \"a\" (Cons \"b\" Nil))) \"a\\0b\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          )
        ]
        $ \(programText, expectedOutput, backendSignature) ->
          assertNativeProgramBehavior programText expectedOutput backendSignature

    it "broad string library covers ASCII classification case and scalar compare through native execution" $
      forM_
        [ ( nativePreludeMain "charIsAsciiHexDigit" "Bool" "charIsAsciiHexDigit 'F'",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charIsAsciiHexDigit" "Bool" "charIsAsciiHexDigit 'g'",
            "false\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charIsAsciiHexDigit" "Bool" "charIsAsciiHexDigit 'λ'",
            "false\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charIsAsciiLineBreak" "Bool" "charIsAsciiLineBreak '\\r'",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charIsAsciiLineBreak" "Bool" "charIsAsciiLineBreak '\\t'",
            "false\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charIsAsciiControl" "Bool" "charIsAsciiControl '\\0'",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charIsAsciiControl" "Bool" "charIsAsciiControl ' '",
            "false\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charToAsciiLower" "Char" "charToAsciiLower '!'",
            "'!'\n",
            "define i32 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charToAsciiLower" "Char" "charToAsciiLower 'λ'",
            "'\\955'\n",
            "define i32 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charToAsciiUpper" "Char" "charToAsciiUpper '!'",
            "'!'\n",
            "define i32 @\"Main__main\"()"
          ),
          ( nativePreludeMain "charToAsciiUpper" "Char" "charToAsciiUpper 'λ'",
            "'\\955'\n",
            "define i32 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringEquals, stringToAsciiLower" "Bool" "stringEquals (stringToAsciiLower \"A\\0Zλ!\") \"a\\0zλ!\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringEquals, stringToAsciiUpper" "Bool" "stringEquals (stringToAsciiUpper \"a\\0zλ!\") \"A\\0Zλ!\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringCompare" "Int" "stringCompare \"a\" \"a\"",
            "0\n",
            "define i64 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringCompare" "Int" "stringCompare \"a\" \"aa\"",
            "-1\n",
            "define i64 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringCompare" "Int" "stringCompare \"b\" \"a\"",
            "1\n",
            "define i64 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringCompare" "Int" "stringCompare \"λ\" \"z\"",
            "1\n",
            "define i64 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringCompare" "Int" "stringCompare \"\\0b\" \"\\0c\"",
            "-1\n",
            "define i64 @\"Main__main\"()"
          )
        ]
        $ \(programText, expectedOutput, backendSignature) ->
          assertNativeProgramBehavior programText expectedOutput backendSignature

    it "broad string library preserves String List Char round trips and exact native metadata" $
      forM_
        [ ( nativePreludeMain "List(..), stringEquals, stringFromList, stringToList" "Bool" "stringEquals (stringFromList (stringToList \"\")) \"\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringEquals, stringFromList, stringToList" "Bool" "stringEquals (stringFromList (stringToList \"abc\")) \"abc\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringEquals, stringFromList, stringToList" "Bool" "stringEquals (stringFromList (stringToList \"aλ\")) \"aλ\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringEquals, stringFromList, stringToList" "Bool" "stringEquals (stringFromList (stringToList \"a\\0b\")) \"a\\0b\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringFromList, stringToList" "List Char" "stringToList (stringFromList Nil)",
            "Nil\n",
            "define ptr @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringLength, stringFromList" "Int" "stringLength (stringFromList (Cons 'a' (Cons '\\0' (Cons 'b' Nil))))",
            "3\n",
            "define i64 @\"Main__main\"()"
          ),
          ( nativePreludeMain "List(..), stringEquals, stringFromList" "Bool" "stringEquals (stringFromList (Cons 'a' (Cons '\\0' (Cons 'b' Nil)))) \"a\\0b\"",
            "true\n",
            "define i1 @\"Main__main\"()"
          ),
          ( nativePreludeMain "stringTake" "String" "stringTake \"a\\0b\" 3",
            "\"a\\NULb\"\n",
            "define ptr @\"Main__main\"()"
          )
        ]
        $ \(programText, expectedOutput, backendSignature) ->
          assertNativeProgramBehavior programText expectedOutput backendSignature

    it "Char literal source checks, runs, emits backend, and executes natively" $
      withTempProgram nativeCharLiteralSourceProgram $ \path -> do
        artifact <- requireRight =<< checkedProgramArtifactFromFile path
        checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
        checkedArtifactRunOutput artifact `shouldBe` Right "'\\955'\n"

        backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
        backendOutput `shouldSatisfy` isInfixOf "define i32 @\"Main__main\"()"
        validateLLVMAssembly backendOutput
        validateLLVMObjectCode backendOutput

        nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
        nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
        validateLLVMAssembly nativeOutput
        validateLLVMObjectCode nativeOutput
        runLLVMNativeExecutable nativeOutput
          `shouldReturn` NativeRunResult ExitSuccess "'\\955'\n" ""

    it "rejects source/native entrypoint symbol collisions" $
      renderMainIdentifiedBackendProgramNativeLLVM nativeMainNameCollisionProgram
        `shouldSatisfyLeft` isInfixOf "reserved native LLVM symbol \"main\""

    it "rejects identity-bearing source/native entrypoint symbol collisions" $
      renderBackendProgramNativeLLVM nativeMainIdentityNameCollisionProgram
        `shouldSatisfyLeft` isInfixOf "reserved native LLVM symbol \"main\""

    it "rejects a backend program whose main identity is absent" $
      renderBackendProgramLLVM identityBearingModuleMismatchedBindingProgram
        `shouldSatisfyLeft` isInfixOf "BackendMainNotFound \"main\""

  it "runs a linked native executable and captures process output" $ do
    result <- runLLVMNativeExecutable nativeOutputCaptureLLVM

    nativeRunExitCode result `shouldBe` ExitFailure 7
    nativeRunStdout result `shouldBe` "native stdout\n"
    nativeRunStderr result `shouldBe` "native stderr\n"

  it "parses CC launchers and flags before executable lookup" $ do
    parseExecutableCommand "ccache clang -m64"
      `shouldBe` Just ("ccache", ["clang", "-m64"])
    parseExecutableCommand "xcrun clang"
      `shouldBe` Just ("xcrun", ["clang"])
    parseExecutableCommand "\"/opt/LLVM Tools/bin/clang\" -fuse-ld=lld"
      `shouldBe` Just ("/opt/LLVM Tools/bin/clang", ["-fuse-ld=lld"])
    parseExecutableCommand "'/opt/LLVM Tools/bin/clang' '-Wl,-dead_strip'"
      `shouldBe` Just ("/opt/LLVM Tools/bin/clang", ["-Wl,-dead_strip"])
    parseExecutableCommand "\"unterminated"
      `shouldBe` Nothing

  it "preserves referenced Prelude bindings and lowers runtime primitive calls" $ do
    output <-
      withTempProgram preludeAndProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "declare i1 @\"__mlfp_and\"(i1, i1)"
    output `shouldSatisfy` isInfixOf "define i1 @\"Prelude__and\""
    output `shouldSatisfy` isInfixOf "call i1 @\"__mlfp_and\""
    validateLLVMAssembly output

  it "lowers string literals to private LLVM globals" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM stringProgram)

    goldenText "test/golden/backend-string.ll.golden" output
    validateLLVMAssembly output

  it "lowers program main by identity when the backend main name is stale" $ do
    output <- requireRight (renderBackendProgramLLVM staleMainIdentityProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"actual-main\"()"
    output `shouldSatisfy` (not . isInfixOf "$stale-main")
    validateLLVMAssembly output

  it "rejects native main self-reference by identity when the var name is stale" $
    renderBackendProgramNativeLLVM staleMainSelfReferenceProgram
      `shouldSatisfyLeft` isInfixOf "opaque main binding `actual-main`"

  it "rejects a local identity used as a top-level main self-reference" $
    renderBackendProgramNativeLLVM staleMainMismatchedSelfReferenceProgram
      `shouldSatisfyLeft` isInfixOf "BackendUnknownVariable \"actual-main\""

  it "keeps identity-referenced helpers reachable when runtime names are stale" $ do
    output <- requireRight (renderBackendProgramLLVM staleMainWithHelperIdentityProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"actual-helper\"()"
    output `shouldSatisfy` isInfixOf "call i64 @\"actual-helper\"()"
    output `shouldSatisfy` (not . isInfixOf "$stale-helper")
    validateLLVMAssembly output

  it "uses collision-free names for distinct type specializations" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM specializationNameCollisionProgram)

    length (filter (isInfixOf "define private ptr @\"poly$t") (lines output)) `shouldBe` 2
    validateLLVMAssembly output

  it "deduplicates type specialization keys by carried type identity" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM staleTypeKeySpecializationProgram)

    length (filter (isInfixOf "define private ptr @\"poly$t") (lines output)) `shouldBe` 1
    validateLLVMAssembly output

  it "reuses result forall identity when a type abstraction binder name is stale" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM staleTypeAbsResultBinderIdentityProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldNotSatisfy` isInfixOf "BackendTypeAbsTypeMismatch"
    validateLLVMAssembly output

  it "resolves stale-spelled type vars by carried binder identity" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM typeBinderStableAliasProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldSatisfy` isInfixOf "ret i64 42"
    validateLLVMAssembly output

  describe "polymorphism lowerability contract" $ do
    it "constructs returned forall calls as static forms with their exact binder bound" $ do
      let resultIdentity =
            typeBinderIdentityFromUnique (UniqueIdentity 994101)
          resultVar =
            BTVarWithIdentity resultIdentity "result"
          resultBinder =
            BackendTypeBinderWithIdentity resultIdentity "result" (Just polyIdTy)
          returnedTy =
            BTForallWithIdentity
              resultIdentity
              "result"
              (Just polyIdTy)
              (BTArrow resultVar resultVar)
          returnedValue =
            BackendTyAbsWithIdentity
              returnedTy
              resultIdentity
              "result"
              (Just polyIdTy)
              ( BackendLam
                  (BTArrow resultVar resultVar)
                  "x"
                  resultVar
                  (BackendVar resultVar "x")
              )
          returnedCall =
            BackendApp
              returnedTy
              ( BackendLam
                  (BTArrow intTy returnedTy)
                  "ignored"
                  intTy
                  returnedValue
              )
              (intLit 0)
          form =
            Lower.functionFormFromExpectedForTest returnedTy returnedCall

      ffTypeBinders form `shouldBe` [resultBinder]
      ffReturnType form `shouldBe` resultVar
      case (ffParameters form, ffBody form) of
        ( [param],
          BackendApp
            bodyTy
            (BackendTyApp instantiatedTy call typeArg)
            (BackendVarWithIdentity argTy argIdentity _)
          ) -> do
            functionParamType param `shouldBe` resultVar
            bodyTy `shouldBe` resultVar
            instantiatedTy `shouldBe` BTArrow resultVar resultVar
            call `shouldBe` returnedCall
            typeArg `shouldBe` resultVar
            argTy `shouldBe` resultVar
            argIdentity `shouldBe` functionParamIdentity param
        other ->
          expectationFailure
            ("expected returned forall call eta form, got " ++ show other)

    it "does not eta-complete ordinary monomorphic calls" $ do
      let call =
            BackendApp
              intTy
              ( BackendLam
                  unaryIntTy
                  "x"
                  intTy
                  (BackendVar intTy "x")
              )
              (intLit 1)
          form =
            Lower.functionFormFromExpectedForTest intTy call

      ffTypeBinders form `shouldBe` []
      ffParameters form `shouldBe` []
      ffBody form `shouldBe` call
      ffReturnType form `shouldBe` intTy

    it "supports top-level complete type application through static specialization" $ do
      output <- requireRight (renderFixtureBackendProgramLLVM polymorphicZeroArityProgram)

      output `shouldSatisfy` isInfixOf "define private ptr @\"none$t"
      output `shouldSatisfy` isInfixOf "call ptr @\"none$t"
      validateLLVMAssembly output

    it "preserves function form for a top-level Quant-Elim redex" $ do
      output <- requireRight (renderFixtureBackendProgramLLVM quantElimFunctionProgram)

      output `shouldSatisfy` isInfixOf "define i64 @\"specialized\"(i64"
      output `shouldSatisfy` isInfixOf "call i64 @\"specialized\"(i64 42)"
      output `shouldNotSatisfy` isInfixOf "escaping function"
      validateLLVMAssembly output

    it "runs the paper bounded self-application through LLVM and native emission" $ do
      artifact <-
        requireRight
          =<< checkedProgramArtifactFromSource
            "<paper-bounded-self-application>"
            paperBoundedSelfApplicationProgram
      checkedArtifactRunOutput artifact `shouldBe` Right "true\n"

      output <- requireRight (checkedArtifactBackendLLVM artifact)
      output `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
      output `shouldNotSatisfy` isInfixOf "could not infer type arguments"
      validateLLVMAssembly output
      validateLLVMObjectCode output
      assertNativeCheckedProgramArtifact artifact "true"

    it "rejects unspecialized polymorphic bindings that escape specialization" $ do
      renderFixtureBackendProgramLLVM unspecializedPolymorphicBindingProgram
        `shouldSatisfyLeft` isInfixOf "unspecialized polymorphic binding"

    it "rejects escaping type abstractions and escaping polymorphic bindings as runtime values" $ do
      renderFixtureBackendProgramLLVM escapingTypeAbstractionProgram
        `shouldSatisfyLeft` isInfixOf "escaping type abstraction"
      renderFixtureBackendProgramLLVM escapingPolymorphicBindingProgram
        `shouldSatisfyLeft` isInfixOf "escaping polymorphic binding"

    it "rejects partial type application instead of inventing runtime polymorphism" $ do
      renderFixtureBackendProgramLLVM partialTypeApplicationProgram
        `shouldSatisfyLeft` isInfixOf "partial type application"

  it "specializes polymorphic zero-arity globals used through type application" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM polymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define private ptr @\"none$t"
    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"none$t"
    validateLLVMAssembly output

  it "instantiates local polymorphic zero-arity values used through type application" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM localPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "Unknown backend LLVM function"
    validateLLVMAssembly output

  it "collects local polymorphic closure entries after type application" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM localPolymorphicClosureEntryProgram)

    output `shouldSatisfy` isInfixOf "$__mlfp_closure$local_poly"
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM type"
    validateLLVMAssembly output

  it "collects local function forms by identity when type-app head names are stale" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM localFunctionFormIdentityTypeAppProgram)

    output `shouldSatisfy` isInfixOf "$__mlfp_closure$local_poly_identity"
    output `shouldNotSatisfy` isInfixOf "$stale_polyLocal"
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM type"
    validateLLVMAssembly output

  it "uses qualified closure entries for directly called type applications" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM directPolymorphicClosureCallProgram)

    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_direct_typeapp$"
    output `shouldSatisfy` isInfixOf "$__mlfp_closure$direct_call_poly\""
    output `shouldNotSatisfy` isInfixOf "store ptr @\"__mlfp_closure$direct_call_poly\""
    validateLLVMAssembly output

  it "collects closure entries when direct type applications cross let heads" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM letHeadPolymorphicClosureCallProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"polyLocal$"
    output `shouldSatisfy` isInfixOf "store ptr @\"polyLocal$"
    output `shouldSatisfy` isInfixOf "$__mlfp_closure$direct_call_poly\""
    validateLLVMAssembly output

  it "lowers structurally matched closure call results" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM structuralClosureResultProgram)

    output `shouldSatisfy` isInfixOf "__mlfp_closure$result_structural"
    validateLLVMAssembly output

  it "instantiates direct polymorphic zero-arity expressions used through type application" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM directPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "escaping type abstraction"
    validateLLVMAssembly output

  it "instantiates polymorphic zero-arity values when type application crosses let heads" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM letHeadPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "unexpected type arguments"
    validateLLVMAssembly output

  it "collects global specializations when type application crosses let heads" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM letHeadGlobalPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define private ptr @\"none$t"
    output `shouldSatisfy` isInfixOf "call ptr @\"none$t"
    output `shouldNotSatisfy` isInfixOf "missing specialization"
    validateLLVMAssembly output

  it "collects global specializations by resolved identity when type-app head names are stale" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM staleGlobalPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define private ptr @\"none$t"
    output `shouldNotSatisfy` isInfixOf "$stale_none"
    output `shouldNotSatisfy` isInfixOf "missing specialization"
    validateLLVMAssembly output

  it "collects global specializations by identity through same-named local shadowing" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM shadowedNameGlobalPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define private ptr @\"none$t"
    output `shouldNotSatisfy` isInfixOf "missing specialization"
    validateLLVMAssembly output

  it "collects global specializations after direct head type instantiation" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM directHeadGlobalPolymorphicZeroArityProgram)

    output `shouldSatisfy` isInfixOf "define private ptr @\"none$t"
    output `shouldSatisfy` isInfixOf "call ptr @\"none$t"
    output `shouldNotSatisfy` isInfixOf "missing specialization"
    validateLLVMAssembly output

  it "treats top-level function parameters as bound during reachability scanning" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM parameterNameShadowsDeadGlobalProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"(i64 %\"x\")"
    output `shouldNotSatisfy` isInfixOf "define ptr @\"x\""
    validateLLVMAssembly output

  it "does not collect global specializations for let-bound shadowing names" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM letShadowedGlobalSpecializationProgram)

    output `shouldSatisfy` isInfixOf "define ptr @\"main\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "poly$t"
    validateLLVMAssembly output

  it "collects specializations from let-promoted local function aliases" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM letPromotedAliasSpecializationProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"poly$t"
    output `shouldSatisfy` isInfixOf "call i64 @\"poly$t"
    output `shouldNotSatisfy` isInfixOf "missing specialization"
    validateLLVMAssembly output

  it "rejects rigid applied type heads during type-argument inference" $ do
    case
      Lower.inferTypeArguments
        "rigid application head"
        [BackendTypeBinder "a" Nothing]
        [("value", BTVarApp "f" (BTVar "a" :| []))]
        [BackendVar (BTVarApp "g" (intTy :| [])) "value"]
      of
        Left err ->
          Lower.renderBackendLLVMError err `shouldSatisfy` isInfixOf "rigid type application head mismatch"
        Right substitution ->
          expectationFailure ("expected rigid head mismatch, got substitution: " ++ show substitution)

    let headIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991210)
    case
      Lower.inferTypeArguments
        "identity-less application head"
        [BackendTypeBinder "a" Nothing]
        [("value", BTVarAppWithIdentity (headIdentity) "f" (BTVar "a" :| []))]
        [BackendVar (BTVarApp "f" (intTy :| [])) "value"]
      of
        Left err ->
          Lower.renderBackendLLVMError err `shouldSatisfy` isInfixOf "rigid type application head mismatch"
        Right substitution ->
          expectationFailure ("expected mixed identity head mismatch, got substitution: " ++ show substitution)

  it "matches rigid applied type heads by explicit identity during type-argument inference" $ do
    let headIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991211)
        binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991212)
        stableHeadName = typeBinderIdentityStableName headIdentity
    case
      Lower.inferTypeArguments
        "identity application head"
        [BackendTypeBinderWithIdentity (binderIdentity) "a" Nothing]
        [("value", BTVarAppWithIdentity (headIdentity) "f" (BTVarWithIdentity (binderIdentity) "a" :| []))]
        [BackendVar (BTVarAppWithIdentity (headIdentity) "renamed" (intTy :| [])) "value"]
      of
        Right substitution ->
          Map.lookup (backendTypeSubstitutionKeyFromIdentity binderIdentity) substitution `shouldBe` Just intTy
        Left err ->
          expectationFailure ("expected identity head match, got error: " ++ Lower.renderBackendLLVMError err)
    case
      Lower.inferTypeArguments
        "stable identity application head"
        [BackendTypeBinderWithIdentity (binderIdentity) "a" Nothing]
        [("value", BTVarApp stableHeadName (BTVarWithIdentity (binderIdentity) "a" :| []))]
        [BackendVar (BTVarAppWithIdentity (headIdentity) "$stale" (intTy :| [])) "value"]
      of
        Left err ->
          Lower.renderBackendLLVMError err `shouldSatisfy` isInfixOf "rigid type application head mismatch"
        Right substitution ->
          expectationFailure ("expected stable string head mismatch, got substitution: " ++ show substitution)

  it "does not refine explicit type arguments through same-named distinct identities" $ do
    let explicitIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991213)
        residualIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991214)
        paramIdentity = localIdentity 991215 "x"
        residualTy = BTVarWithIdentity (residualIdentity) "a"
        explicitBinder =
          BackendTypeBinderWithIdentity (explicitIdentity) "a" Nothing
        form =
          FunctionForm
            { ffTypeBinders = [explicitBinder],
              ffParameters = [FunctionParam paramIdentity "x" residualTy],
              ffEvidenceParams = Set.empty,
              ffBody = BackendVarWithIdentity residualTy (paramIdentity) "x",
              ffReturnType = residualTy
            }

    case Lower.refineExplicitTypeArgumentsForTest [explicitBinder] form [intTy] [] of
      Left err ->
        expectationFailure ("expected explicit substitution, got error: " ++ Lower.renderBackendLLVMError err)
      Right substitution -> do
        Map.lookup (backendTypeSubstitutionKeyFromIdentity explicitIdentity) substitution `shouldBe` Just intTy
        Map.lookup (backendTypeSubstitutionKeyFromIdentity residualIdentity) substitution `shouldBe` Nothing

  it "applies accumulated residual identity substitutions to later explicit-call arguments" $ do
    let explicitIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991216)
        residualIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991217)
        explicitBinder =
          BackendTypeBinderWithIdentity (explicitIdentity) "f" Nothing
        explicitVar = BTVarWithIdentity (explicitIdentity) "f"
        residualVar = BTVarWithIdentity (residualIdentity) "a"
        residualOptionIdentity =
          symbolIdentityFromParts (UniqueIdentity 991220) SymbolType "Main" "Option" Nothing
        option ty =
          BTConWithIdentity (residualOptionIdentity) (BaseTy "Main.Option") (ty :| [])
        optionResidual = option residualVar
        optionInt = option intTy
        form =
          FunctionForm
            { ffTypeBinders = [explicitBinder],
              ffParameters =
                [ FunctionParam (localIdentity 991218 "evidence") "evidence" (BTArrow explicitVar explicitVar),
                  FunctionParam (localIdentity 991219 "value") "value" explicitVar
                ],
              ffEvidenceParams = Set.empty,
              ffBody = BackendVar explicitVar "value",
              ffReturnType = explicitVar
            }
        args =
          [ BackendVar (BTArrow optionInt optionInt) "evidence",
            BackendVar optionResidual "value"
          ]

    case Lower.refineExplicitTypeArgumentsForTest [explicitBinder] form [optionResidual] args of
      Left err ->
        expectationFailure ("expected accumulated refinement, got error: " ++ Lower.renderBackendLLVMError err)
      Right substitution -> do
        Map.lookup (backendTypeSubstitutionKeyFromIdentity explicitIdentity) substitution
          `shouldBe` Just optionInt
        Map.lookup (backendTypeSubstitutionKeyFromIdentity residualIdentity) substitution
          `shouldBe` Just intTy

  it "rejects mismatched applied type arguments during type-argument inference" $ do
    case
      Lower.inferTypeArguments
        "applied argument mismatch"
        [BackendTypeBinder "f" Nothing]
        [("value", BTVarApp "f" (intTy :| []))]
        [BackendVar (BTCon (BaseTy "Box") (boolTy :| [])) "value"]
      of
        Left err ->
          Lower.renderBackendLLVMError err `shouldSatisfy` isInfixOf "type application argument mismatch"
        Right substitution ->
          expectationFailure ("expected applied argument mismatch, got substitution: " ++ show substitution)

  it "statically lowers first-class polymorphic local arguments" $ do
    output <-
      withTempProgram localFirstClassPolymorphismProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM type"
    validateLLVMAssembly output

  it "rejects source-level escaping polymorphic main values with a stable diagnostic" $ do
    result <- withTempProgram sourceEscapingPolymorphicMainProgram emitBackendFile

    result
      `shouldSatisfy` either
        (isInfixOf "polymorphic main binding")
        (const False)

  it "statically lowers first-class function-typed arguments" $ do
    output <-
      withTempProgram firstClassFunctionArgumentProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
    output `shouldNotSatisfy` isInfixOf "Main__use"
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM type"
    validateLLVMAssembly output

  it "statically lowers stale-named top-level function arguments by resolved identity" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM staleStaticFunctionArgumentProgram)

    output `shouldNotSatisfy` isInfixOf "$stale_id"
    validateLLVMAssembly output

  it "follows stale-named eta alias targets by resolved identity" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM staleEtaAliasStaticFunctionArgumentProgram)

    output `shouldNotSatisfy` isInfixOf "$stale_id"
    validateLLVMAssembly output

  it "follows eta alias arguments by local identity when parameter names are stale" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM staleEtaAliasParamIdentityProgram)

    output `shouldNotSatisfy` isInfixOf "$stale_id"
    output `shouldNotSatisfy` isInfixOf "$stale_y"
    validateLLVMAssembly output

  it "does not lower global binding uses by stale name when identities differ" $ do
    renderMainIdentifiedBackendProgramLLVM mismatchedGlobalBindingIdentityProgram
      `shouldSatisfyLeft` isInfixOf "BackendUnknownVariable \"id\""

  it "keeps same-named top-level identity references reachable through local shadowing" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM shadowedNameGlobalReachabilityProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"helper\""
    output `shouldSatisfy` isInfixOf "call i64 @\"helper\""
    validateLLVMAssembly output

  it "rejects a mismatched local identity before same-named globals" $ do
    renderMainIdentifiedBackendProgramLLVM shadowedNameGlobalMismatchedReachabilityProgram
      `shouldSatisfyLeft` isInfixOf "BackendUnknownVariable \"helper\""

  it "lowers stale data type heads by carried data identity" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM dataIdentityStaleTypeHeadProgram)

    output `shouldSatisfy` isInfixOf "define ptr @\"main\"(ptr %\"x\")"
    output `shouldNotSatisfy` isInfixOf "stale.IdentityBox"
    validateLLVMAssembly output

  it "does not lower data type heads by stale name when identities differ" $ do
    renderFixtureBackendProgramLLVM dataIdentityMismatchedTypeHeadProgram
      `shouldSatisfyLeft` isInfixOf "Unsupported backend LLVM type"

  it "rejects duplicate data parameter identities" $
    renderMainIdentifiedBackendProgramLLVM conflictingIdentityDataParameterProgram
      `shouldSatisfyLeft` isInfixOf "BackendDuplicateDataParameter \"Pair\""

  it "case-analyzes let-bound native primitive results by checked data identity" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM letBoundNativeOptionCaseProgram)

    output `shouldSatisfy` isInfixOf ("call ptr @\"" ++ PrimitiveInventory.stringCharAtOptionPrimitiveName ++ "\"")
    validateLLVMAssembly output

  it "does not lower same-named fake builtin type heads as builtin scalars" $ do
    renderFixtureBackendProgramLLVM fakeBuiltinIntProgram
      `shouldSatisfyLeft` isInfixOf "Unsupported backend LLVM type"

  it "rejects unsupported static function arguments instead of erasing them" $ do
    renderFixtureBackendProgramLLVM staticPartialApplicationArgumentProgram
      `shouldSatisfyLeft` isInfixOf "unsupported static function argument \"f\""

  it "collects specializations for type-applied global static aliases" $ do
    output <-
      withTempProgram typeAppliedGlobalStaticAliasProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
    output `shouldSatisfy` isInfixOf "define private i64 @\"Main__id$t"
    output `shouldNotSatisfy` isInfixOf "missing specialization"
    validateLLVMAssembly output

  it "rejects static-argument entrypoints instead of eliding main" $ do
    renderFixtureBackendProgramLLVM staticArgumentMainProgram
      `shouldSatisfyLeft` isInfixOf "parameter \"poly\" of main"

  it "rejects recursive static global inlining instead of diverging" $ do
    renderFixtureBackendProgramLLVM recursiveStaticGlobalProgram
      `shouldSatisfyLeft` isInfixOf "recursive static global \"loop\""

  it "statically lowers immediate constructor fields carrying forall values" $ do
    output <-
      withTempProgram constructorFirstClassPolymorphismProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
    output `shouldNotSatisfy` isInfixOf "escaping type abstraction"
    validateLLVMAssembly output

  it "lowers local function aliases without requiring closure conversion" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM localFunctionAliasProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldSatisfy` isInfixOf "ret i64 7"
    output `shouldNotSatisfy` isInfixOf "Unknown backend LLVM function"
    validateLLVMAssembly output

  it "resolves same-named local lets by identity before name fallback" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramLLVM localIdentityShadowedLetProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldSatisfy` isInfixOf "ret i64 41"
    output `shouldNotSatisfy` isInfixOf "ret i64 99"
    validateLLVMAssembly output

  it "preserves stable-spelled local vars when the reference carries binder identity" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramLLVM localIdentityStableAliasProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldSatisfy` isInfixOf "ret i64 42"
    validateLLVMAssembly output

  it "resolves same-named lambda parameters by identity before name fallback" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM lambdaIdentityShadowedParamProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"shadow\"(i64 %\"x\", i64 %\"x1\")"
    output `shouldSatisfy` isInfixOf "ret i64 %\"x\""
    output `shouldNotSatisfy` isInfixOf "ret i64 %\"x1\""
    validateLLVMAssembly output

  it "resolves same-named top-level values by identity before local fallback" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramLLVM shadowedNameGlobalValueIdentityProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"x\"()"
    output `shouldSatisfy` isInfixOf "call i64 @\"x\"()"
    output `shouldNotSatisfy` isInfixOf "ret i64 1"
    validateLLVMAssembly output

  it "resolves stale-named case binders by identity before name fallback" $ do
    output <- requireRight (renderFixtureBackendProgramNativeLLVM casePatternIdentityStaleBinderProgram)

    validateLLVMAssembly output
    validateLLVMObjectCode output
    runLLVMNativeExecutable output
      `shouldReturn` NativeRunResult ExitSuccess "99\n" ""

  it "resolves same-named case binders by identity before name fallback" $ do
    output <- requireRight (renderFixtureBackendProgramNativeLLVM casePatternIdentityDuplicateDisplayProgram)

    validateLLVMAssembly output
    validateLLVMObjectCode output
    runLLVMNativeExecutable output
      `shouldReturn` NativeRunResult ExitSuccess "99\n" ""

  it "does not infer mismatched-identity pattern field types from identity-bearing same-named variables" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramNativeLLVM casePatternMismatchedFieldTypeProgram)

    validateLLVMAssembly output
    validateLLVMObjectCode output
    runLLVMNativeExecutable output
      `shouldReturn` NativeRunResult ExitSuccess "41\n" ""

  it "resolves stale-named closure parameters by identity before name fallback" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramNativeLLVM closureParamIdentityStaleNameProgram)

    validateLLVMAssembly output
    validateLLVMObjectCode output
    runLLVMNativeExecutable output
      `shouldReturn` NativeRunResult ExitSuccess "99\n" ""

  it "preserves identity-bearing returned-partial closure slots while assigning entry identity" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramNativeLLVM returnedPartialClosureSlotIdentityProgram)

    validateLLVMAssembly output
    validateLLVMObjectCode output
    runLLVMNativeExecutable output
      `shouldReturn` NativeRunResult ExitSuccess "99\n" ""

  it "does not infer returned-partial provenance from a source closure name" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramNativeLLVM returnedPartialPrefixSourceClosureProgram)

    validateLLVMAssembly output
    validateLLVMObjectCode output
    runLLVMNativeExecutable output
      `shouldReturn` NativeRunResult ExitSuccess "99\n" ""

  it "freshens same-named closure parameters while resolving by identity" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramNativeLLVM closureParamIdentityDuplicateDisplayProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$duplicate_param_display\"(ptr %\"__mlfp_env\", i64 %\"x\", i64 %\"x1\")"
    output `shouldSatisfy` isInfixOf "ret i64 %\"x1\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    runLLVMNativeExecutable output
      `shouldReturn` NativeRunResult ExitSuccess "99\n" ""

  it "rejects duplicate closure parameter identities" $
    renderBackendProgramLLVM duplicateIdentityClosureParamProgram
      `shouldSatisfyLeft` isInfixOf "BackendDuplicateClosureParameter \"x\""

  it "rejects a closure parameter use whose identity matches neither same-named binder" $
    renderMainIdentifiedBackendProgramLLVM ambiguousIdentityClosureParamUseProgram
      `shouldSatisfyLeft` isInfixOf "BackendUnknownVariable \"x\""

  it "classifies stale-named closure captures by identity before name fallback" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM closureCaptureValueKindIdentityProgram)

    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.env.field"
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.closure.env.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "preserves top-level function aliases as function forms" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM topLevelFunctionAliasProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"g\"(i64"
    output `shouldSatisfy` isInfixOf "call i64 @\"id\""
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM type"
    validateLLVMAssembly output

  it "freshens shadowed lambda parameters before emitting LLVM" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM shadowedLambdaParamsProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"shadow\"(i64 %\"x\", i64 %\"x1\")"
    output `shouldSatisfy` isInfixOf "ret i64 %\"x1\""
    validateLLVMAssembly output

  it "lowers let-headed calls before call dispatch" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM letHeadedCallProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldSatisfy` isInfixOf "ret i64 7"
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM call"
    validateLLVMAssembly output

  it "lowers case-headed calls before call dispatch" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramLLVM caseHeadedCallProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldSatisfy` isInfixOf "call i64 @\"id\""
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM call"
    validateLLVMAssembly output

  it "resolves a global named like the runtime and primitive before intrinsic dispatch" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM userNamedRuntimeAndProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"__mlfp_and\"(i64"
    output `shouldSatisfy` isInfixOf "call i64 @\"__mlfp_and\""
    output `shouldNotSatisfy` isInfixOf "declare i1 @\"__mlfp_and\"(i1, i1)"
    validateLLVMAssembly output

  it "does not dispatch primitive spelling carried by a local identity" $
    renderMainIdentifiedBackendProgramLLVM mismatchedRuntimePrimitiveProgram
      `shouldSatisfyLeft` isInfixOf "BackendUnknownVariable \"__mlfp_and\""

  it "does not dispatch primitive calls through stale identity payloads" $
    renderMainIdentifiedBackendProgramLLVM staleNamedRuntimePrimitiveProgram
      `shouldSatisfyLeft` isInfixOf "BackendUnknownVariable \"$stale_and\""

  it "suppresses the runtime malloc declaration when a global owns that name" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM userNamedMallocProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"malloc\"(i64"
    output `shouldSatisfy` isInfixOf "call i64 @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "declare ptr @\"malloc\"(i64)"
    validateLLVMAssembly output

  it "suppresses the runtime malloc declaration when an identity-bearing global owns that name" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramLLVM userNamedMallocIdentityProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"malloc\"(i64"
    output `shouldSatisfy` isInfixOf "call i64 @\"malloc\""
    output `shouldNotSatisfy` isInfixOf "declare ptr @\"malloc\"(i64)"
    validateLLVMAssembly output

  it "rejects constructor allocation when a global owns the runtime malloc name" $ do
    renderMainIdentifiedBackendProgramLLVM mallocCollisionConstructorProgram
      `shouldSatisfyLeft` isInfixOf "reserved runtime binding \"malloc\""

  it "rejects constructor allocation when an identity-bearing global owns the runtime malloc name" $ do
    renderMainIdentifiedBackendProgramLLVM mallocIdentityCollisionConstructorProgram
      `shouldSatisfyLeft` isInfixOf "reserved runtime binding \"malloc\""

  it "lowers top-level recursive higher-order functions through closure arguments" $ do
    artifact <-
      requireRight =<< withTempProgram sourceTopLevelRecursiveHigherOrderProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "define i64 @\"Main__loop\"(ptr %\"$f#"
    output `shouldSatisfy` isInfixOf "ptr %\"$n#"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "lowers local recursive higher-order helpers through closure arguments" $ do
    artifact <-
      requireRight =<< withTempProgram sourceLocalRecursiveHigherOrderProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "Main__main$letrec$"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "loads only constructor fields used by a case branch" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramLLVM unusedPolymorphicPatternFieldProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"(ptr %\"box\")"
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"box\", i64 0"
    output `shouldSatisfy` isInfixOf "load i64, ptr %\"__llvm.case.tag.ptr."
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"box\", i64 16"
    output `shouldSatisfy` isInfixOf "load i64, ptr %\"__llvm.case.field.ptr."
    output `shouldNotSatisfy` isInfixOf "getelementptr i8, ptr %\"box\", i64 8"
    validateLLVMAssembly output

  it "evaluates unused immediate constructor fields before static erasure" $ do
    renderMainIdentifiedBackendProgramLLVM strictImmediateConstructFieldProgram
      `shouldSatisfyLeft` isInfixOf "representation-changing roll"

  it "evaluates immediate constructor fields before default alternatives" $ do
    renderMainIdentifiedBackendProgramLLVM strictImmediateDefaultProgram
      `shouldSatisfyLeft` isInfixOf "representation-changing roll"

  it "lowers unmatched immediate constructor cases to unreachable" $ do
    output <- requireRight (renderMainIdentifiedBackendProgramLLVM unmatchedImmediateConstructorProgram)

    output `shouldSatisfy` isInfixOf "unreachable"
    output `shouldNotSatisfy` isInfixOf "no matching immediate constructor alternative"
    validateLLVMAssembly output

  it "does not match immediate constructor alternatives by stale name when identities differ" $ do
    renderMainIdentifiedBackendProgramLLVM mismatchedImmediateConstructorIdentityCaseProgram
      `shouldSatisfyLeft` isInfixOf "BackendUnknownConstructor \"WithStatic\""

  it "does not lower constructor uses by stale name when identities differ" $ do
    renderMainIdentifiedBackendProgramLLVM mismatchedConstructorUseIdentityProgram
      `shouldSatisfyLeft` isInfixOf "BackendUnknownConstructor \"WithStatic\""

  it "resolves matching-identity constructor uses before field lowerability checks" $ do
    renderMainIdentifiedBackendProgramLLVM matchingConstructorUseIdentityProgram
      `shouldSatisfyLeft` isInfixOf "escaping type abstraction"

  it "resolves constructor display aliases before field lowerability checks" $ do
    renderMainIdentifiedBackendProgramLLVM displayAliasConstructorUseIdentityProgram
      `shouldSatisfyLeft` isInfixOf "escaping type abstraction"

  it "evaluates immediate constructor fields before unmatched alternatives" $ do
    renderMainIdentifiedBackendProgramLLVM strictImmediateUnmatchedProgram
      `shouldSatisfyLeft` isInfixOf "representation-changing roll"

  it "rejects duplicate constructor case alternatives before emitting switch" $ do
    renderMainIdentifiedBackendProgramLLVM duplicateConstructorCaseProgram
      `shouldSatisfyLeft` isInfixOf "duplicate constructor case tag"

  it "rejects duplicate immediate constructor alternatives by identity" $ do
    renderMainIdentifiedBackendProgramLLVM duplicateImmediateConstructorIdentityCaseProgram
      `shouldSatisfyLeft` isInfixOf "duplicate constructor case alternative"

  it "keys constructed field value kinds by constructor identity" $ do
    let constructed =
          constructedValueForConstructor
            immediateChoiceConstructorIdentity
            [LowerClosureRecord]

    constructedFieldValueKind immediateChoiceConstructorIdentity 0 constructed
      `shouldBe` Just LowerClosureRecord
    constructedFieldValueKind otherImmediateChoiceConstructorIdentity 0 constructed
      `shouldBe` Nothing
    ( mergeConstructedValues
        [ Just constructed,
          Just (constructedValueForConstructor immediateChoiceConstructorIdentity [LowerClosureRecord])
        ]
        >>= constructedFieldValueKind immediateChoiceConstructorIdentity 0
      )
      `shouldBe` Just LowerClosureRecord

  it "compares lowering function forms and bindings by identity when names are stale" $ do
    let paramIdentity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991710)) "x")
        stableBody = BackendVarWithIdentity intTy paramIdentity "x"
        staleBody = BackendVarWithIdentity intTy paramIdentity "$stale_x"
        stableForm = FunctionForm [] [FunctionParam paramIdentity "x" intTy] Set.empty stableBody intTy
        staleForm = FunctionForm [] [FunctionParam paramIdentity "$stale_x" intTy] Set.empty staleBody intTy
        stableBinding = BindingInfo helperIdentity "helper" stableForm False
        staleBinding = BindingInfo helperIdentity "$stale_helper" staleForm False
    stableForm `shouldBe` staleForm
    stableBinding `shouldBe` staleBinding

  it "does not resolve program metadata through stale identity payloads" $ do
    case backendDataConstructors immediateChoiceDataWithConstructorIdentity of
      constructor0 : _ -> do
        let binding = BindingInfo staleHelperIdentity "helper" stableForm False
            stableForm = FunctionForm [] [] Set.empty (intLit 41) intTy
            dataRuntime0 = DataRuntime dataIdentityBoxData []
            constructorRuntime0 =
              ConstructorRuntime
                { crConstructor = constructor0,
                  crData = immediateChoiceDataWithConstructorIdentity,
                  crTag = 0,
                  crValueKey = constructorValueKeyFromIdentity immediateChoiceConstructorIdentity
                }
            base =
              ProgramBase
                { pbBindingsByIdentity = Map.singleton staleHelperIdentity binding,
                  pbBindingsByRef = Map.empty,
                  pbBindingOrder = [],
                  pbConstructorsByIdentity = Map.singleton immediateChoiceConstructorIdentity constructorRuntime0,
                  pbDataByIdentity = Map.singleton dataIdentityBoxIdentity dataRuntime0,
                  pbIdentityGenerator = initialIdentityGenerator
                }
        lookupProgramBindingByIdentityExact base (staleHelperIdentity) `shouldBe` Just binding
        lookupProgramBindingByIdentityExact base (conflictingStaleHelperIdentity) `shouldBe` Nothing
        lookupProgramDataByIdentityExact base (dataIdentityBoxIdentity) `shouldBe` Just dataRuntime0
        lookupProgramDataByIdentityExact base (conflictingStaleDataIdentity) `shouldBe` Nothing
        lookupProgramConstructorByIdentityExact base (immediateChoiceConstructorIdentity) `shouldBe` Just constructorRuntime0
        lookupProgramConstructorByIdentityExact base (conflictingStaleConstructorIdentity) `shouldBe` Nothing
      [] ->
        expectationFailure "expected immediate choice constructor fixture"

  it "compares closure capture slots by identity when names are stale" $ do
    let captureIdentity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991709)) "captured")
        stableCapture = ClosureCaptureSlot (captureIdentity) "captured" intTy LowerClosureRecord
        staleCapture = ClosureCaptureSlot (captureIdentity) "$stale_captured" intTy LowerClosureRecord
    stableCapture `shouldBe` staleCapture

  it "compares native render specs by identity-bearing type when generated names are stale" $ do
    let stringIdentity = PrimitiveInventory.builtinTypeIdentity "String"
        stableSpec = NativeRenderSpec (BTBaseWithIdentity (stringIdentity) (BaseTy "String")) "__mlfp_native_render$string"
        staleSpec = NativeRenderSpec (BTBaseWithIdentity (stringIdentity) (BaseTy "$stale_String")) "__mlfp_native_render$stale"
    stableSpec `shouldBe` staleSpec

  it "keys LLVM semantic caches by carried identities instead of display strings" $ do
    let stringIdentity = PrimitiveInventory.builtinTypeIdentity "String"
        conflictingIdentity = renameSymbolDefiningName "$conflicting_String" stringIdentity
        stableTy = BTBaseWithIdentity (stringIdentity) (BaseTy "String")
        staleTy = BTBaseWithIdentity (stringIdentity) (BaseTy "$stale_String")
        conflictingTy = BTBaseWithIdentity (conflictingIdentity) (BaseTy "String")
        bindingRef = backendBindingRefFromIdentity helperIdentity
        stableRequest = SpecRequest bindingRef "poly" [stableTy]
        staleRequest = SpecRequest bindingRef "$stale_poly" [staleTy]
        conflictingRequest = SpecRequest bindingRef "poly" [conflictingTy]
        stableExpr = BackendLit stableTy (LString "value")
        staleExpr = BackendLit staleTy (LString "value")
        conflictingExpr = BackendLit conflictingTy (LString "value")
        calleeTy ty = BTArrow ty (BTArrow ty ty)
        resultTy ty = BTArrow ty ty
        returnedPartialEntry identity name ty =
          ClosureEntry
            { ceOrigin = GeneratedReturnedPartialOrigin,
              ceFunctionType = resultTy ty,
              ceEntryIdentity = identity,
              ceEntryName = name,
              ceCaptures =
                [ ClosureCaptureSlot (fixtureLocalDetails "callee") "callee" (calleeTy ty) LowerFunctionPointer,
                  ClosureCaptureSlot (fixtureLocalDetails "supplied") "supplied" ty LowerRuntimeValue
                ],
              ceParameters = [FunctionParam (fixtureLocalDetails "remaining") "remaining" ty],
              ceEvidenceParams = Set.empty,
              ceBody = BackendLit ty (LString "unused")
            }
        stableEntry = returnedPartialEntry (UniqueIdentity 991717) "__mlfp_returned_partial$stable" stableTy
        staleEntry = returnedPartialEntry (UniqueIdentity 991717) "__mlfp_returned_partial$stale" staleTy
        conflictingEntry = returnedPartialEntry (UniqueIdentity 991718) "__mlfp_returned_partial$stable" conflictingTy
    backendTypeIdentityKey stableTy `shouldBe` backendTypeIdentityKey staleTy
    backendTypeIdentityKey stableTy `shouldNotBe` backendTypeIdentityKey conflictingTy
    specializationIdentityKey stableRequest `shouldBe` specializationIdentityKey staleRequest
    specializationIdentityKey stableRequest `shouldNotBe` specializationIdentityKey conflictingRequest
    wrapperIdentityKey stableTy stableExpr `shouldBe` wrapperIdentityKey staleTy staleExpr
    wrapperIdentityKey stableTy stableExpr `shouldNotBe` wrapperIdentityKey conflictingTy conflictingExpr
    closureEntryIdentityKey stableEntry `shouldBe` closureEntryIdentityKey staleEntry
    closureEntryIdentityKey stableEntry `shouldNotBe` closureEntryIdentityKey conflictingEntry

  it "compares specializations by request and identity-bearing form when generated names are stale" $ do
    let paramIdentity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991713)) "x")
        stableBody = BackendVarWithIdentity intTy paramIdentity "x"
        staleBody = BackendVarWithIdentity intTy paramIdentity "$stale_x"
        stableForm = FunctionForm [] [FunctionParam paramIdentity "x" intTy] Set.empty stableBody intTy
        staleForm = FunctionForm [] [FunctionParam paramIdentity "$stale_x" intTy] Set.empty staleBody intTy
        request = SpecRequest (backendBindingRefFromIdentity helperIdentity) "poly" [intTy]
        staleRequest = SpecRequest (backendBindingRefFromIdentity helperIdentity) "$stale_poly" [intTy]
        stableSpec =
          Specialization
            request
            (backendBindingRefFromGenerated (UniqueIdentity 991715) "__mlfp_specialization$stable")
            "__mlfp_specialization$stable"
            stableForm
        staleSpec =
          Specialization
            staleRequest
            (backendBindingRefFromGenerated (UniqueIdentity 991715) "__mlfp_specialization$stale")
            "__mlfp_specialization$stale"
            staleForm
        otherSpec =
          Specialization
            (SpecRequest (backendBindingRefFromGenerated (UniqueIdentity 991716) "other") "other" [intTy])
            (backendBindingRefFromGenerated (UniqueIdentity 991716) "__mlfp_specialization$stable")
            "__mlfp_specialization$stable"
            stableForm
    stableSpec `shouldBe` staleSpec
    stableSpec `shouldNotBe` otherSpec

  it "compares local functions by identity-bearing form when owner names are stale" $ do
    let paramIdentity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991714)) "x")
        stableBody = BackendVarWithIdentity intTy paramIdentity "x"
        staleBody = BackendVarWithIdentity intTy paramIdentity "$stale_x"
        stableForm = FunctionForm [] [FunctionParam paramIdentity "x" intTy] Set.empty stableBody intTy
        staleForm = FunctionForm [] [FunctionParam paramIdentity "$stale_x" intTy] Set.empty staleBody intTy
        emptyEnv =
          ExprEnv
            { eeValuesByIdentity = Map.empty,
              eeLocalFunctionsByIdentity = Map.empty,
              eeActiveGlobalInlines = Set.empty
            }
        stableLocal = LocalFunction "owner" stableForm emptyEnv Nothing
        staleLocal = LocalFunction "$stale_owner" staleForm emptyEnv Nothing
    stableLocal `shouldBe` staleLocal

  it "compares wrappers by identity-bearing payloads when generated names are stale" $ do
    let paramIdentity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991712)) "x")
        wrapperTy = BTArrow intTy intTy
        stableWrapper =
          Wrapper
            { wrapperKind = FunctionWrapperKind,
              wrapperBindingRef = backendBindingRefFromGenerated (UniqueIdentity 991713) "__mlfp_function_wrapper$stable",
              wrapperFunctionName = "__mlfp_function_wrapper$stable",
              wrapperExpr = BackendVarWithIdentity wrapperTy paramIdentity "x",
              wrapperParameters = [FunctionParam paramIdentity "x" intTy],
              wrapperReturnType = intTy
            }
        staleWrapper =
          stableWrapper
            { wrapperBindingRef = backendBindingRefFromGenerated (UniqueIdentity 991713) "__mlfp_function_wrapper$stale",
              wrapperFunctionName = "__mlfp_function_wrapper$stale",
              wrapperExpr = BackendVarWithIdentity wrapperTy paramIdentity "$stale_x"
            }
    stableWrapper `shouldBe` staleWrapper

  it "compares closure entry params by identity when names are stale" $ do
    let paramIdentity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991711)) "x")
        stableBody = BackendVarWithIdentity intTy paramIdentity "x"
        staleBody = BackendVarWithIdentity intTy paramIdentity "$stale_x"
        stableEntry =
          ClosureEntry
            { ceOrigin = BackendClosureOrigin,
              ceFunctionType = BTArrow intTy intTy,
              ceEntryIdentity = UniqueIdentity 991710,
              ceEntryName = "__mlfp_closure$stable",
              ceCaptures = [],
              ceParameters = [FunctionParam paramIdentity "x" intTy],
              ceEvidenceParams = Set.empty,
              ceBody = stableBody
            }
        staleEntry =
          stableEntry
            { ceEntryIdentity = (UniqueIdentity 991710),
              ceEntryName = "__mlfp_closure$stale",
              ceParameters = [FunctionParam paramIdentity "$stale_x" intTy],
              ceBody = staleBody
            }
        differentNameEntry =
          stableEntry
            { ceEntryName = "__mlfp_closure$different"
            }
        identityEntry =
          stableEntry
            { ceEntryIdentity = (UniqueIdentity 991710)
            }
        differentIdentityEntry =
          identityEntry
            { ceEntryIdentity = (UniqueIdentity 991709)
            }
    identityEntry `shouldBe` staleEntry
    identityEntry `shouldNotBe` differentIdentityEntry
    stableEntry `shouldBe` differentNameEntry

  it "rejects non-tail default case alternatives before emitting switch" $ do
    renderMainIdentifiedBackendProgramLLVM nonTailDefaultCaseProgram
      `shouldSatisfyLeft` isInfixOf "default case alternative must be last"

  it "lowers nullary and recursive-list constructors through case" $ do
    checked <- requireChecked recursiveListProgram
    output <- requireRight (renderCheckedProgramLLVM checked)
    let nullaryTagOnlyBlock =
          unlines
            [ "  %\"__llvm.malloc.2\" = call ptr @\"malloc\"(i64 8)",
              "  %\"__llvm.tag.ptr.3\" = getelementptr i8, ptr %\"__llvm.malloc.2\", i64 0",
              "  store i64 0, ptr %\"__llvm.tag.ptr.3\""
            ]

    output `shouldSatisfy` isInfixOf "define ptr @\"Main__tailOrNil\""
    output `shouldSatisfy` isInfixOf "define i1 @\"Main__isNil\""
    output `shouldSatisfy` isInfixOf nullaryTagOnlyBlock
    output `shouldSatisfy` isInfixOf "call ptr @\"malloc\"(i64 24)"
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.malloc.4\", i64 8"
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.malloc.4\", i64 16"
    output
      `shouldSatisfy` isInfixOf
        "switch i64 %\"__llvm.case.tag.1\", label %case.default.2 [ i64 0, label %case.alt.0 i64 1, label %case.alt.1 ]"
    output `shouldSatisfy` isInfixOf "phi ptr"
    validateLLVMAssembly output

  it "keeps ordinary function-valued method arguments out of evidence lowering" $ do
    output <- requireRight =<< withTempProgram ordinaryFunctionEvidenceMethodProgram emitBackendFile

    output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
    output `shouldNotSatisfy` isInfixOf "Unknown backend LLVM function"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "inlines opaque evidence helpers with local function-valued method arguments" $ do
    output <- requireRight =<< withTempProgram localFunctionEvidenceMethodProgram emitBackendFile

    output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
    output `shouldNotSatisfy` isInfixOf "Unknown backend LLVM function"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "emits inline-only callees passed through opaque evidence pointers" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM inlineOnlyEvidenceCalleeProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"callee\"(ptr %\"f\")"
    output `shouldSatisfy` isInfixOf "ptr @\"callee\""
    validateLLVMAssembly output

  it "emits referenced callees that call opaque evidence parameters inline-only" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM inlineOnlyEvidenceParameterCallCalleeProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"calleeWithEvidenceCall\"(ptr %\"$evidence_apply\")"
    output `shouldSatisfy` isInfixOf "ptr @\"calleeWithEvidenceCall\""
    validateLLVMAssembly output

  it "emits referenced inline-only callees passed through local aliases" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM aliasedInlineOnlyEvidenceCalleeProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"calleeWithEvidenceCall\"(ptr %\"$evidence_apply\")"
    output `shouldSatisfy` isInfixOf "ptr @\"calleeWithEvidenceCall\""
    validateLLVMAssembly output

  it "does not collect shadowed local aliases as referenced callees" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM shadowedLocalAliasReferenceCollectorProgram)

    output `shouldNotSatisfy` isInfixOf "define i64 @\"shadowedCalleeWithEvidenceCall\""
    validateLLVMAssembly output

  it "collects referenced callees by identity through same-named local shadowing" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM shadowedNameReferencedCalleeIdentityProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"calleeWithEvidenceCall\"(ptr %\"$evidence_apply\")"
    output `shouldSatisfy` isInfixOf "ptr @\"calleeWithEvidenceCall\""
    validateLLVMAssembly output

  it "lowers nested evidence wrapper parameters as pointers" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM nestedEvidenceWrapperParameterProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_evidence_wrapper$"
    output `shouldSatisfy` isInfixOf "ptr %\"__mlfp_evidence_arg0\""
    validateLLVMAssembly output

  it "rejects higher-order evidence parameters with mismatched nested function shapes" $
    Lower.evidenceFunctionTypesCompatible
      (BTArrow (BTArrow unaryIntTy intTy) intTy)
      (BTArrow (BTArrow unaryBoolTy intTy) intTy)
      `shouldBe` False

  it "accepts higher-order evidence parameters with matching nested function shapes" $
    Lower.evidenceFunctionTypesCompatible
      (BTArrow (BTArrow unaryIntTy intTy) intTy)
      (BTArrow (BTArrow unaryIntTy intTy) intTy)
      `shouldBe` True

  it "accepts identity-renamed forall evidence parameters" $ do
    let leftIdentity = typeBinderIdentityFromUnique (UniqueIdentity 2069001)
        rightIdentity = typeBinderIdentityFromUnique (UniqueIdentity 2069002)
        leftForall =
          BTForallWithIdentity
            (leftIdentity)
            "a"
            Nothing
            (BTArrow (BTVarWithIdentity (leftIdentity) "a") intTy)
        rightForall =
          BTForallWithIdentity
            (rightIdentity)
            "b"
            Nothing
            (BTArrow (BTVarWithIdentity (rightIdentity) "b") intTy)
    Lower.evidenceFunctionTypesCompatible
      (BTArrow leftForall intTy)
      (BTArrow rightForall intTy)
      `shouldBe` True

  it "emits self-recursive higher-order calls instead of expanding them inline" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM selfRecursiveHigherOrderProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"loop\"(ptr %\"f\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "call i64 @\"loop\"(ptr %\"f\", i64 %\"x\")"
    validateLLVMAssembly output

  it "resolves inlined local function calls before captured values" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM localFunctionCallShadowsValueProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldNotSatisfy` isInfixOf "unexpected type arguments"
    validateLLVMAssembly output

  it "resolves local function references before captured values for indirect arguments" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM localFunctionReferenceShadowsValueProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldNotSatisfy` isInfixOf "evidence function type mismatch"
    validateLLVMAssembly output

  it "preserves inline function argument shadowing beside closure-valued fields" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM inlineFunctionArgumentShadowsValueProgram)

    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_shadow\""
    output `shouldNotSatisfy` isInfixOf "escaping function \"f\""
    validateLLVMAssembly output

  it "rejects evidence wrappers that capture local term bindings" $ do
    renderFixtureBackendProgramLLVM capturingEvidenceWrapperProgram
      `shouldSatisfyLeft` isInfixOf "unsupported evidence function argument"

  it "rejects evidence wrappers that capture stale-named locals by identity" $ do
    renderFixtureBackendProgramLLVM staleLocalIdentityEvidenceWrapperProgram
      `shouldSatisfyLeft` isInfixOf "unsupported evidence function argument"

  it "allows evidence wrappers that reference same-named globals by identity" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM globalIdentityEvidenceWrapperProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_evidence_wrapper$"
    output `shouldNotSatisfy` isInfixOf "unsupported evidence function argument"
    validateLLVMAssembly output

  it "collects evidence wrappers only after polymorphic forms are specialized" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM polymorphicEvidenceWrapperProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_evidence_wrapper$"
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM type"
    validateLLVMAssembly output

  it "collects local polymorphic evidence wrappers after type application" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM localPolymorphicEvidenceWrapperProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_evidence_wrapper$"
    output `shouldNotSatisfy` isInfixOf "unsupported evidence function argument"
    validateLLVMAssembly output

  describe "merged interpreter/LLVM/native parity matrix" $ do
    it "classifies every interpreter-success case exactly once" $ do
      case programLLVMNativeParityPolicyDiagnostics of
        [] -> pure ()
        diagnostics ->
          expectationFailure $
            unlines $
              "interpreter/LLVM/native parity policy is inconsistent:"
                : diagnostics
                    ++ ("classified rows:" : map describeProgramLLVMNativeParityPolicy programLLVMNativeParityPolicies)

    mapM_ runLLVMParityPolicy programLLVMNativeParityPolicies

  it "lowers packaged partial applications through the explicit closure ABI" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM partialApplicationProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$addOne\""
    output `shouldSatisfy` isInfixOf "store i64 1"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "rejects raw captured functions stored in constructor fields until closure construction is explicit" $ do
    renderFixtureBackendProgramLLVM escapingLambdaProgram
      `shouldSatisfyLeft` isInfixOf "unsupported function argument"

  it "packages source-level top-level partial applications as closure values" $ do
    artifact <-
      requireRight =<< withTempProgram sourceTopLevelPartialApplicationProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "__mlfp_closure$Main__main$Main__keepLeft$partial"
    output `shouldSatisfy` isInfixOf "store i64 1"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "packages source-level local partial applications as closure values" $ do
    artifact <-
      requireRight =<< withTempProgram sourceLocalPartialApplicationProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "packages partial applications with closure-valued supplied arguments" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationClosureArgumentProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "41"

  it "packages partial applications with global closure-valued supplied arguments" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationGlobalClosureArgumentProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "41"

  it "packages partial applications headed by closure-valued parameters" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationClosureParameterProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "BackendClosureCallExpectedClosureValue"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "tracks partial closure-valued argument demand through top-level aliases" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationClosureDemandAliasProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "tracks partial closure-valued argument demand through wrapped aliases" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationClosureDemandWrappedAliasProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "offsets propagated closure-demand indices through eta-expanded aliases" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationClosureDemandEtaAliasProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "tracks partial closure-valued argument demand for local helpers" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationLocalClosureDemandProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "wraps closure-demanded arguments after evidence arguments" $ do
    artifact <-
      requireRight =<< withTempProgram sourceClosureDemandAfterEvidenceProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    output `shouldNotSatisfy` isInfixOf "BackendClosureCallExpectedClosureValue"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "packages constrained partial applications after hidden evidence" $ do
    artifact <-
      requireRight =<< withTempProgram sourceConstrainedPartialApplicationProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "packages constrained partial applications through constrained aliases" $ do
    artifact <-
      requireRight =<< withTempProgram sourceConstrainedPartialApplicationAliasProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldNotSatisfy` isInfixOf "Backend LLVM arity mismatch"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "freshens generated partial capture names against local binders" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationGeneratedNameCollisionProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldNotSatisfy` isInfixOf "BackendDuplicateClosureCapture"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "wraps direct function arguments before packaging partial applications" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationDirectFunctionArgumentProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "4"

  it "wraps closure-demanded arguments for let-headed call aliases" $ do
    artifact <-
      requireRight =<< withTempProgram sourceClosureDemandLetHeadedCallProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "wraps closure-demanded arguments for eta-expanded call heads" $ do
    artifact <-
      requireRight =<< withTempProgram sourceClosureDemandEtaCallHeadProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "keeps polymorphic supplied partial arguments on the static specialization path" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationPolymorphicArgumentProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldNotSatisfy` isInfixOf "escaping polymorphic binding"
    output `shouldNotSatisfy` isInfixOf "Main__usePoly$partial"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "true"

  it "keeps higher-rank supplied partial functions on the static specialization path" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationHigherRankFunctionArgumentProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldNotSatisfy` isInfixOf "escaping function value"
    output `shouldNotSatisfy` isInfixOf "Main__useHigher$partial"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "captures locals when wrapping demanded inline function arguments" $ do
    artifact <-
      requireRight =<< withTempProgram sourceClosureDemandedInlineFunctionArgumentProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "41"

  it "captures locals for non-variable partial callees" $ do
    artifact <-
      requireRight =<< withTempProgram sourcePartialApplicationNonVariableCalleeProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "Backend LLVM arity mismatch"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "1"

  it "lowers saturated captured local aliases on the direct call path" $ do
    output <-
      withTempProgram sourceCapturedClosureCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
    output `shouldSatisfy` isInfixOf "ret i64 41"
    output `shouldNotSatisfy` isInfixOf "__mlfp_closure$Main__main$"
    output `shouldNotSatisfy` isInfixOf "__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers closure-valued function parameters through the explicit closure ABI" $ do
    output <-
      withTempProgram sourceFunctionParameterClosureCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output
      `shouldSatisfy` containsLLVMLineFragments
        ["define private i64 @\"", "__mlfp_closure$Main__main$"]
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.malloc"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers closure-valued function parameters through let aliases" $ do
    output <-
      withTempProgram sourceFunctionParameterClosureAliasCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output
      `shouldSatisfy` containsLLVMLineFragments
        ["define private i64 @\"", "__mlfp_closure$Main__main$"]
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "BackendClosureCallExpectedClosureValue"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers source-level returned closure values as pointer results" $ do
    output <-
      withTempProgram sourceReturnedClosureProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
    output
      `shouldSatisfy` containsLLVMLineFragments
        ["store ptr @\"", "__mlfp_closure$Main__main$"]
    validateLLVMAssembly output

  it "lowers source-level top-level closure calls through the explicit closure ABI" $ do
    output <-
      withTempProgram sourceTopLevelClosureCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define ptr @\"Main__maker\"()"
    output `shouldSatisfy` isInfixOf "call ptr @\"Main__maker\"()"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers source-level local returned closure calls through the explicit closure ABI" $ do
    artifact <-
      requireRight =<< withTempProgram sourceLocalReturnedClosureCallProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output
      `shouldSatisfy` containsLLVMLineFragments
        ["define private i64 @\"", "__mlfp_closure$Main__main$"]
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "Backend LLVM arity mismatch"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "41"

  it "lowers let-bound returned closure records through the explicit closure ABI" $ do
    artifact <-
      requireRight =<< withTempProgram sourceLetBoundReturnedClosureRecordCallProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output
      `shouldSatisfy` containsLLVMLineFragments
        ["define private i64 @\"", "__mlfp_closure$Main__main$"]
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.malloc"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "41"

  it "lowers direct returned closure applications through the explicit closure ABI" $ do
    artifact <-
      requireRight =<< withTempProgram sourceDirectReturnedClosureApplicationProgram checkedProgramArtifactFromFile
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output
      `shouldSatisfy` containsLLVMLineFragments
        ["define private i64 @\"", "__mlfp_closure$Main__main$"]
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "Backend LLVM arity mismatch"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "41"

  it "lowers type-abstracted top-level closure calls through the explicit closure ABI" $ do
    output <-
      withTempProgram sourcePolymorphicTopLevelClosureCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define private ptr @\"Main__maker"
    output `shouldSatisfy` isInfixOf "call ptr @\"Main__maker"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers top-level closure values passed as function arguments" $ do
    output <-
      withTempProgram sourceTopLevelClosureArgumentProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "call ptr @\"Main__maker\"()"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers closure-valued function parameters through nested let aliases" $ do
    output <-
      withTempProgram sourceFunctionParameterNestedClosureAliasCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output
      `shouldSatisfy` containsLLVMLineFragments
        ["define private i64 @\"", "__mlfp_closure$Main__main$"]
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "BackendClosureCallExpectedClosureValue"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers returned lambdas behind lets with complete closure parameters" $ do
    output <-
      withTempProgram sourceReturnedLetLambdaClosureProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
    output `shouldSatisfy` isInfixOf "(ptr %\"__mlfp_env\", i64 %\""
    output `shouldSatisfy` isInfixOf "\", i64 %\""
    validateLLVMAssembly output

  it "preserves shadowed lambda parameters collected through lets" $
    assertNativeProgram sourceReturnedLetLambdaShadowingProgram "7"

  it "lowers source closure-valued constructor fields through the explicit closure ABI" $ do
    output <-
      withTempProgram sourceClosureValuedConstructorFieldProgram $ \path ->
        requireRight =<< emitBackendFile path

    output
      `shouldSatisfy` containsLLVMLineFragments
        ["define private i64 @\"", "__mlfp_closure$Main__main$"]
    output
      `shouldSatisfy` containsLLVMLineFragments
        ["store ptr @\"", "__mlfp_closure$Main__main$"]
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.malloc.5\", i64 8"
    output `shouldSatisfy` isInfixOf "store ptr %\"__llvm.malloc.2\", ptr %\"__llvm.field.ptr."
    output `shouldSatisfy` isInfixOf "load ptr, ptr %\"__llvm.case.field.ptr."
    output `shouldSatisfy` isInfixOf "store i64 41"
    output `shouldSatisfy` isInfixOf "load ptr"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "closure-valued constructor field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers zero-capture closures through the explicit closure ABI" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM zeroCaptureClosureProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$identity\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$identity\""
    output `shouldSatisfy` isInfixOf "load ptr"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output

  it "lowers captured local values through closure environments" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM capturedClosureProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$constCaptured\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "store i64 41"
    output `shouldSatisfy` isInfixOf "load i64"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output

  it "lowers case-selected closure callees through the explicit closure ABI" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM caseSelectedClosureCalleeProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$case_some\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$case_none\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "phi ptr"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output

  it "lowers let-selected closure callees through the explicit closure ABI" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM letSelectedClosureCalleeProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$let_callee\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$let_callee\""
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output

  it "qualifies closure entry names emitted from type specializations" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM polymorphicClosureSpecializationProgram)

    let closureDefinitions =
          filter
            (\line -> "define private i64 @\"poly$t" `isInfixOf` line && "$__mlfp_closure$poly\"" `isInfixOf` line)
            (lines output)
    length closureDefinitions `shouldBe` 2
    output `shouldNotSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$poly\""
    output `shouldNotSatisfy` isInfixOf "store ptr @\"__mlfp_closure$poly\""
    validateLLVMAssembly output

  it "keeps closure-valued constructor fields on the closure ABI across specializations" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM polymorphicClosureFunctionWrapperProgram)

    output `shouldSatisfy` isInfixOf "$__mlfp_closure$wrapper_key"
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "qualifies closure entries in inlined polymorphic global calls" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM inlinePolymorphicClosureProgram)

    let qualifiedStores =
          filter
            (\line -> "store ptr @\"polyInline$t" `isInfixOf` line && "$__mlfp_closure$inline\"" `isInfixOf` line)
            (lines output)
    length qualifiedStores `shouldBe` 1
    output `shouldNotSatisfy` isInfixOf "store ptr @\"__mlfp_closure$inline\""
    validateLLVMAssembly output

  it "does not reserve retired stored-function wrapper names without generated wrappers" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM closureEntryFunctionWrapperCollisionProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_function_wrapper$0\""
    output `shouldNotSatisfy` isInfixOf "Duplicate backend LLVM symbol"
    validateLLVMAssembly output

  it "deduplicates evidence wrapper keys by carried term identity" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM staleEvidenceWrapperKeyProgram)

    let wrapperDefinitions =
          filter (isInfixOf "define private i64 @\"__mlfp_evidence_wrapper$") (lines output)
    length wrapperDefinitions `shouldBe` 1
    validateLLVMAssembly output

  it "rejects closure entry names that collide with runtime declarations" $
    renderFixtureBackendProgramLLVM closureEntryRuntimeDeclarationCollisionProgram
      `shouldSatisfyLeft` isInfixOf "Duplicate backend LLVM symbol: \"malloc\""

  it "lowers stored explicit closure constructor fields" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM functionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_top\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_top\""
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "lowers stored direct closure constructor fields through closure records" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM directFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_direct\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_direct\""
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "lowers stored local closure constructor fields through pointer aliases" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM localFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_local\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_local\""
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "lowers stored transitive local closure aliases through pointer aliases" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM transitiveLocalFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_transitive\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_transitive\""
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "re-stores case-projected constructor fields carrying closures" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM immediateRestoredFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_restored\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_restored\""
    output `shouldSatisfy` isInfixOf "load ptr"
    output `shouldNotSatisfy` isInfixOf "unsupported function argument"
    validateLLVMAssembly output

  it "lowers captured function constructor fields through closure environments" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM capturedFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_captured\""
    output `shouldSatisfy` isInfixOf "store i64 1"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_captured\""
    output `shouldNotSatisfy` isInfixOf "unsupported function argument"
    validateLLVMAssembly output

  it "preserves stored function-pointer captures on the indirect call path" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM capturedFunctionPointerCallProgram)

    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.env.field"
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.closure.env.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "infers captured nullary global callable aliases from their bodies" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM capturedNullaryGlobalFunctionAliasProgram)

    output `shouldSatisfy` isInfixOf "call ptr @\"get\"()"
    output `shouldNotSatisfy` isInfixOf "$stale_get"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.env.field"
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.closure.env.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

    nativeOutput <- requireRight (renderFixtureBackendProgramNativeLLVM capturedNullaryGlobalFunctionAliasProgram)
    runLLVMNativeExecutable nativeOutput
      `shouldReturn` NativeRunResult ExitSuccess "41\n" ""

  it "infers captured nullary global case closures from their bodies" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM capturedNullaryGlobalCaseClosureProgram)

    output `shouldSatisfy` isInfixOf "call ptr @\"getCaseClosure\"()"
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.closure.env.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

    nativeOutput <- requireRight (renderFixtureBackendProgramNativeLLVM capturedNullaryGlobalCaseClosureProgram)
    runLLVMNativeExecutable nativeOutput
      `shouldReturn` NativeRunResult ExitSuccess "41\n" ""

  it "classifies complete direct type applications by their instantiated closure bodies" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM completeTypeAppliedClosureReturnProgram)

    output `shouldSatisfy` isInfixOf "call ptr @\"makeTypeAppliedClosure\""
    output `shouldSatisfy` isInfixOf "__mlfp_closure$complete_direct_typeapp_return"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "returned value is not callable"
    validateLLVMAssembly output
    validateLLVMObjectCode output

    nativeOutput <- requireRight (renderFixtureBackendProgramNativeLLVM completeTypeAppliedClosureReturnProgram)
    runLLVMNativeExecutable nativeOutput
      `shouldReturn` NativeRunResult ExitSuccess "41\n" ""

  it "routes over-applied raw function-pointer results through indirect calls" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM rawReturnedFunctionPointerCallProgram)

    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.call."
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.call."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "preserves let-bound raw function-pointer aliases as values" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM rawFunctionPointerAliasReturnProgram)

    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.call."
    output `shouldNotSatisfy` isInfixOf "escaping function"
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.call."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "classifies first-order function parameters as raw function pointers" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM firstOrderFunctionParameterCallProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"applyFirst\"(ptr %\"f\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "call i64 %\"f\"(i64 %\"x\")"
    output `shouldNotSatisfy` isInfixOf "getelementptr i8, ptr %\"f\""
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "classifies first-order closure parameters as raw function pointers" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM closureFirstOrderFunctionParameterCallProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$call_first_order_param\""
    output `shouldSatisfy` isInfixOf "call i64 %\"f\"(i64 %\"x\")"
    output `shouldNotSatisfy` isInfixOf "getelementptr i8, ptr %\"f\""
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "normalizes first-order function fields through closure records" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM rawFunctionPointerFieldCallProgram)

    output `shouldSatisfy` isInfixOf "__mlfp_returned_partial$function$0"
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.case.field"
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.case.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "preserves closure-valued first-order function fields through case binders" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM closureFunctionFieldCallProgram)

    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.case.field"
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.case.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

    nativeOutput <- requireRight (renderFixtureBackendProgramNativeLLVM closureFunctionFieldCallProgram)
    runLLVMNativeExecutable nativeOutput
      `shouldReturn` NativeRunResult ExitSuccess "41\n" ""

  it "preserves closure-valued fields from returned constructors through case binders" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM returnedClosureFunctionFieldCallProgram)

    output `shouldSatisfy` isInfixOf "call ptr @\"makeBox\""
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.case.field"
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.case.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

    nativeOutput <- requireRight (renderFixtureBackendProgramNativeLLVM returnedClosureFunctionFieldCallProgram)
    runLLVMNativeExecutable nativeOutput
      `shouldReturn` NativeRunResult ExitSuccess "41\n" ""

  it "preserves case-reboxed closure fields from returned constructors" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM returnedReboxedClosureFunctionFieldCallProgram)

    output `shouldSatisfy` isInfixOf "call ptr @\"makeRestored\""
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.case.field"
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.case.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

    nativeOutput <- requireRight (renderFixtureBackendProgramNativeLLVM returnedReboxedClosureFunctionFieldCallProgram)
    runLLVMNativeExecutable nativeOutput
      `shouldReturn` NativeRunResult ExitSuccess "41\n" ""

  it "classifies case-returned closures for over-applied global calls" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM caseReturnedClosureOverApplicationProgram)

    output `shouldSatisfy` isInfixOf "call ptr @\"makeFromCase\""
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.call"
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.call"
    validateLLVMAssembly output
    validateLLVMObjectCode output

    nativeOutput <- requireRight (renderFixtureBackendProgramNativeLLVM caseReturnedClosureOverApplicationProgram)
    runLLVMNativeExecutable nativeOutput
      `shouldReturn` NativeRunResult ExitSuccess "41\n" ""

  it "returns global functions from source-level case branches" $ do
    artifact <- requireRight =<< checkedProgramArtifactFromSource "<inline-test>" sourceCaseReturnedGlobalFunctionProgram
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "@\"Main__helper\""
    output `shouldNotSatisfy` isInfixOf "escaping function \"Main__helper\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "41"

  it "preserves closure-valued fields through function-parameter scrutinees" $ do
    artifact <- requireRight =<< checkedProgramArtifactFromSource "<inline-test>" sourceCaseParameterClosureFieldProgram
    output <- requireRight (checkedArtifactBackendLLVM artifact)

    output `shouldSatisfy` isInfixOf "load ptr, ptr %\"__llvm.case.field.ptr"
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.call"
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.case.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeCheckedProgramArtifact artifact "41"

  it "normalizes mixed callable case results to closure records" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM mixedCallableCaseResultProgram)

    output `shouldSatisfy` isInfixOf "__mlfp_returned_partial$function$0"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_returned_partial$function$0"
    output `shouldSatisfy` isInfixOf "getelementptr i8, ptr %\"__llvm.case.result"
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.call"
    validateLLVMAssembly output
    validateLLVMObjectCode output

    nativeOutput <- requireRight (renderFixtureBackendProgramNativeLLVM mixedCallableCaseResultProgram)
    runLLVMNativeExecutable nativeOutput
      `shouldReturn` NativeRunResult ExitSuccess "41\n" ""

  it "rejects BackendApp heads that select closure values through let or case" $ do
    renderFixtureBackendProgramLLVM caseHeadedDirectClosureBackendAppProgram
      `shouldSatisfyLeft` isInfixOf "Backend LLVM validation failed: BackendClosureCalledWithBackendApp Nothing"
    renderFixtureBackendProgramLLVM letHeadedDirectClosureBackendAppProgram
      `shouldSatisfyLeft` isInfixOf "Backend LLVM validation failed: BackendClosureCalledWithBackendApp (Just \"__mlfp_closure$backend_app_let\")"

  it "captures first-order lambda parameters as raw function pointers" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM capturedFirstOrderParameterClosureProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$capture_first_order_param\""
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.env.field"
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.closure.env.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "preserves let-bound first-order parameters captured by nested closures" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM letBoundFirstOrderParameterClosureProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"makeCaller$vk$function$__mlfp_closure$let_capture_first_order_param\""
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.env.field"
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.closure.env.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "specializes closure entries by callable capture representation" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM mixedCallableCaptureClosureEntryProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"makeCaller$vk$function$__mlfp_closure$mixed_callable_capture\""
    output `shouldSatisfy` isInfixOf "define private i64 @\"makeCaller$vk$closure$__mlfp_closure$mixed_callable_capture\""
    output `shouldNotSatisfy` isInfixOf "duplicate closure entry"
    validateLLVMAssembly output
    validateLLVMObjectCode output

    nativeOutput <- requireRight (renderFixtureBackendProgramNativeLLVM mixedCallableCaptureClosureEntryProgram)
    runLLVMNativeExecutable nativeOutput
      `shouldReturn` NativeRunResult ExitSuccess "42\n" ""

  it "packages partial applications of returned closures" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM returnedClosurePartialApplicationProgram)

    output `shouldSatisfy` isInfixOf "__mlfp_returned_partial$closure"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_returned_partial$closure"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "packages partial applications of returned raw function pointers" $ do
    output <- requireRight (renderFixtureBackendProgramLLVM rawReturnedFunctionPointerPartialApplicationProgram)

    output `shouldSatisfy` isInfixOf "__mlfp_returned_partial$function"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.env.field"
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.closure.env.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "guards nullary global value-kind cycles" $ do
    result <- timeout 1000000 (evaluate (renderFixtureBackendProgramLLVM nullaryGlobalValueKindCycleProgram))
    case result of
      Nothing ->
        expectationFailure "value-kind classification did not terminate"
      Just llvmResult -> do
        output <- requireRight llvmResult
        output `shouldSatisfy` isInfixOf "define ptr @\"left\"()"
        output `shouldSatisfy` isInfixOf "call ptr @\"right\"()"
        output `shouldSatisfy` isInfixOf "call ptr @\"left\"()"
        validateLLVMAssembly output

  it "rejects unknown base types" $ do
    renderMainIdentifiedBackendProgramLLVM unknownBaseProgram
      `shouldSatisfyLeft` isInfixOf "Unsupported backend LLVM type"

  it "rejects representation-changing roll/unroll nodes" $ do
    case lowerTestBackendProgram rollMismatchProgram of
      Left err ->
        renderBackendLLVMError (BackendLLVMLoweringFailed err)
          `shouldSatisfy` isInfixOf "representation-changing roll"
      Right llvmModule ->
        expectationFailure ("expected roll mismatch, got output:\n" ++ renderLLVMModule llvmModule)

  it "lowers structural unrolls through their recursive runtime carrier" $ do
    _ <- requireRight (lowerTestBackendProgram structuralRollUnrollCarrierProgram)
    pure ()

assertNativeProgram :: String -> String -> Expectation
assertNativeProgram programText expectedValue = do
  artifact <- requireRight =<< checkedProgramArtifactFromSource "<inline-test>" programText
  assertNativeCheckedProgramArtifact artifact expectedValue

assertNativeCheckedProgramArtifact :: CheckedProgramArtifact -> String -> Expectation
assertNativeCheckedProgramArtifact artifact expectedValue = do
  output <- requireRight (checkedArtifactNativeLLVM artifact)
  output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
  output `shouldSatisfy` isInfixOf "declare i32 @\"printf\"(ptr, ...)"
  output `shouldSatisfy` isInfixOf "call i32 (ptr, ...) @\"printf\"("
  validateLLVMAssembly output
  validateLLVMObjectCode output
  runLLVMNativeExecutable output
    `shouldReturn` NativeRunResult ExitSuccess (expectedValue ++ "\n") ""

assertNativeProgramBehavior :: String -> String -> String -> Expectation
assertNativeProgramBehavior programText expectedOutput backendSignature = do
  artifact <- requireRight =<< checkedProgramArtifactFromSource "<inline-test>" programText
  checkedArtifactCheckOutput artifact `shouldBe` Right "OK\n"
  checkedArtifactRunOutput artifact `shouldBe` Right expectedOutput

  backendOutput <- requireRight (checkedArtifactBackendLLVM artifact)
  backendOutput `shouldSatisfy` isInfixOf backendSignature
  validateLLVMAssembly backendOutput
  validateLLVMObjectCode backendOutput

  nativeOutput <- requireRight (checkedArtifactNativeLLVM artifact)
  nativeOutput `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
  validateLLVMAssembly nativeOutput
  validateLLVMObjectCode nativeOutput
  runLLVMNativeExecutable nativeOutput
    `shouldReturn` NativeRunResult ExitSuccess expectedOutput ""

emitNativeSource :: String -> IO (Either String String)
emitNativeSource programText =
  runCheckedProgramArtifact
    checkedArtifactNativeLLVM
    (checkedProgramArtifactFromSource "<inline-test>" programText)

emitBackendSource :: String -> IO (Either String String)
emitBackendSource programText =
  runCheckedProgramArtifact
    checkedArtifactBackendLLVM
    (checkedProgramArtifactFromSource "<inline-test>" programText)

checkProgramFile :: FilePath -> IO (Either String String)
checkProgramFile path =
  runCheckedProgramArtifact
    checkedArtifactCheckOutput
    (checkedProgramArtifactFromFile path)

emitBackendFile :: FilePath -> IO (Either String String)
emitBackendFile path =
  runCheckedProgramArtifact
    checkedArtifactBackendLLVM
    (checkedProgramArtifactFromFile path)

nativePreludeMain :: String -> String -> String -> String
nativePreludeMain imports resultType expression =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (" ++ imports ++ ");",
      "  def main : " ++ resultType ++ " = " ++ expression ++ ";",
      "}"
    ]

simpleFunctionProgram :: String
simpleFunctionProgram =
  unlines
    [ "module Main export (id, main) {",
      "  def id : Int -> Int = λx x;",
      "  def main : Int = id 1;",
      "}"
    ]

ioPureUnitMainProgram :: String
ioPureUnitMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, pure);",
      "  def main : IO Unit = pure Unit;",
      "}"
    ]

ioPutStrLnMainProgram :: String
ioPutStrLnMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, putStrLn);",
      "  def main : IO Unit = putStrLn \"hello\";",
      "}"
    ]

ioDirectPrimitiveMainProgram :: String
ioDirectPrimitiveMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO);",
      "  def main : IO Unit = __io_bind (__io_pure Unit) (λ(_done : Unit) __io_putStrLn \"world\");",
      "}"
    ]

ioPreludeMonadMainProgram :: String
ioPreludeMonadMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, pure, bind, putStrLn);",
      "  def action : IO Unit = pure Unit;",
      "  def main : IO Unit = bind action (λ(_done : Unit) putStrLn \"prelude\");",
      "}"
    ]

ioPreludeFunctorApplicativeMainProgram :: String
ioPreludeFunctorApplicativeMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, Functor, Applicative, map, pure);",
      "  def action : IO Int = pure 1;",
      "  def mapped : IO Int = map (λ(_n : Int) 2) action;",
      "  def main : IO Unit = __io_bind mapped (λ(_m : Int) __io_putStrLn \"map\");",
      "}"
    ]

ioNestedPrimitiveMainProgram :: String
ioNestedPrimitiveMainProgram =
  unlines
    [ "module Main export (afterSecond, afterFirst, main) {",
      "  import Prelude exposing (Unit(..), IO);",
      "  def afterSecond : Unit -> IO Unit = λ(_second : Unit) __io_putStrLn \"third\";",
      "  def afterFirst : Unit -> IO Unit = λ(_first : Unit) __io_bind (__io_putStrLn \"second\") afterSecond;",
      "  def main : IO Unit = __io_bind (__io_putStrLn \"first\") afterFirst;",
      "}"
    ]

ioPutStrMainProgram :: String
ioPutStrMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, putStr);",
      "  def main : IO Unit = putStr \"hello\";",
      "}"
    ]

ioWriteFileMainProgram :: String
ioWriteFileMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, writeFile);",
      "  def main : IO Unit = writeFile \"/tmp/mlf2-test-write.txt\" \"hello from mlfp\";",
      "}"
    ]

ioAppendFileMainProgram :: String
ioAppendFileMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, appendFile);",
      "  def main : IO Unit = appendFile \"/tmp/mlf2-test-append.txt\" \"appended\";",
      "}"
    ]

ioReadFileMainProgram :: String
ioReadFileMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, readFile, putStrLn);",
      "  def main : IO Unit = __io_bind (readFile \"/tmp/mlf2-test-read-input.txt\") (λ(contents : String) putStrLn contents);",
      "}"
    ]

ioExitWithMainProgram :: String
ioExitWithMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, exitWith);",
      "  def main : IO Unit = exitWith 42;",
      "}"
    ]

ioNewIORefMainProgram :: String
ioNewIORefMainProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, newIORef);",
      "  def main : IO (IORef String) = newIORef \"hello\";",
      "}"
    ]

ioWriteIORefProgram :: String
ioWriteIORefProgram =
  unlines
    [ "module Main export (writeIt, main) {",
      "  import Prelude exposing (Unit(..), IO, writeIORef, pure);",
      "  def writeIt : IORef String -> String -> IO Unit = λ(ref : IORef String) λ(val : String) writeIORef ref val;",
      "  def main : IO Unit = pure Unit;",
      "}"
    ]

ioGetArgsProgram :: String
ioGetArgsProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, List(..), getArgs);",
      "  def main : IO (List String) = getArgs;",
      "}"
    ]

ioReadIORefProgram :: String
ioReadIORefProgram =
  unlines
    [ "module Main export (readIt, main) {",
      "  import Prelude exposing (Unit(..), IO, readIORef, pure);",
      "  def readIt : IORef String -> IO String = λ(ref : IORef String) readIORef ref;",
      "  def main : IO Unit = pure Unit;",
      "}"
    ]

pureMainIODependencyProgram :: String
pureMainIODependencyProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, pure);",
      "  def discard : IO Unit -> Unit = λ(_action : IO Unit) Unit;",
      "  def main : Unit = discard (pure Unit);",
      "}"
    ]

pureMainDirectIOPrimitiveProgram :: String
pureMainDirectIOPrimitiveProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO);",
      "  def main : Unit = (λ(_action : IO Unit) Unit) (__io_pure Unit);",
      "}"
    ]

pureMainUnusedIOProgram :: String
pureMainUnusedIOProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, pure);",
      "  def unused : IO Unit = pure Unit;",
      "  def main : Bool = true;",
      "}"
    ]

nativeOutputCaptureLLVM :: String
nativeOutputCaptureLLVM =
  unlines
    [ "; mlf2 native runner smoke",
      "source_filename = \"mlf2-native-runner-smoke\"",
      "@\"stdout_text\" = private unnamed_addr constant [14 x i8] c\"native stdout\\0A\"",
      "@\"stderr_text\" = private unnamed_addr constant [14 x i8] c\"native stderr\\0A\"",
      "declare i64 @\"write\"(i32, ptr, i64)",
      "",
      "define i32 @\"main\"() {",
      "entry:",
      "  %\"stdout.ptr\" = getelementptr inbounds [14 x i8], ptr @\"stdout_text\", i64 0, i64 0",
      "  %\"stderr.ptr\" = getelementptr inbounds [14 x i8], ptr @\"stderr_text\", i64 0, i64 0",
      "  call i64 @\"write\"(i32 1, ptr %\"stdout.ptr\", i64 14)",
      "  call i64 @\"write\"(i32 2, ptr %\"stderr.ptr\", i64 14)",
      "  ret i32 7",
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

boolMainProgram :: String
boolMainProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Bool = true;",
      "}"
    ]

nativeNestedAdtProgram :: String
nativeNestedAdtProgram =
  unlines
    [ "module Main export (Nat(..), Option(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  data Option a =",
      "      None : Option a",
      "    | Some : a -> Option a;",
      "",
      "  def main : Option Nat = Some (Succ Zero);",
      "}"
    ]

nativeDoubleUnderscoreConstructorProgram :: String
nativeDoubleUnderscoreConstructorProgram =
  unlines
    [ "module Main export (Weird(..), main) {",
      "  data Weird =",
      "      A__B : Weird;",
      "",
      "  def main : Weird = A__B;",
      "}"
    ]

nativeStringSourceProgram :: String
nativeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : String = \"hello\";",
      "}"
    ]

nativeUnicodeStringSourceProgram :: String
nativeUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : String = \"λ\";",
      "}"
    ]

nativeUnicodeStringLengthSourceProgram :: String
nativeUnicodeStringLengthSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringLength);",
      "  def main : Int = stringLength \"λa\";",
      "}"
    ]

nativeEmptyStringIsEmptySourceProgram :: String
nativeEmptyStringIsEmptySourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringIsEmpty);",
      "  def main : Bool = stringIsEmpty \"\";",
      "}"
    ]

nativeUnicodeStringIsEmptySourceProgram :: String
nativeUnicodeStringIsEmptySourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringIsEmpty);",
      "  def main : Bool = stringIsEmpty \"λ\";",
      "}"
    ]

nativePresentStringContainsCharSourceProgram :: String
nativePresentStringContainsCharSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringContainsChar);",
      "  def main : Bool = stringContainsChar \"aλb\" 'λ';",
      "}"
    ]

nativeAbsentStringContainsCharSourceProgram :: String
nativeAbsentStringContainsCharSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringContainsChar);",
      "  def main : Bool = stringContainsChar \"ab\" 'λ';",
      "}"
    ]

nativePresentStringContainsSourceProgram :: String
nativePresentStringContainsSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringContains);",
      "  def main : Bool = stringContains \"aλb\" \"λ\";",
      "}"
    ]

nativeAbsentStringContainsSourceProgram :: String
nativeAbsentStringContainsSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringContains);",
      "  def main : Bool = stringContains \"ab\" \"λ\";",
      "}"
    ]

nativeEqualStringEqualsSourceProgram :: String
nativeEqualStringEqualsSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringEquals);",
      "  def main : Bool = stringEquals \"aλ\" \"aλ\";",
      "}"
    ]

nativeUnequalStringEqualsSourceProgram :: String
nativeUnequalStringEqualsSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringEquals);",
      "  def main : Bool = stringEquals \"aλ\" \"a\";",
      "}"
    ]

nativeEmptyStringEqualsSourceProgram :: String
nativeEmptyStringEqualsSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringEquals);",
      "  def main : Bool = stringEquals \"\" \"\";",
      "}"
    ]

nativeEmbeddedNulStringEqualsSourceProgram :: String
nativeEmbeddedNulStringEqualsSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringEquals);",
      "  def main : Bool = stringEquals \"a\\0b\" \"a\";",
      "}"
    ]

nativeAppendEmbeddedNulStringEqualsSourceProgram :: String
nativeAppendEmbeddedNulStringEqualsSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringAppend, stringEquals);",
      "  def main : Bool = stringEquals (stringAppend \"a\" \"\\0b\") \"a\";",
      "}"
    ]

nativePresentStringStartsWithSourceProgram :: String
nativePresentStringStartsWithSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringStartsWith);",
      "  def main : Bool = stringStartsWith \"λab\" \"λ\";",
      "}"
    ]

nativeAbsentStringStartsWithSourceProgram :: String
nativeAbsentStringStartsWithSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringStartsWith);",
      "  def main : Bool = stringStartsWith \"aλb\" \"λ\";",
      "}"
    ]

nativePresentStringEndsWithSourceProgram :: String
nativePresentStringEndsWithSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringEndsWith);",
      "  def main : Bool = stringEndsWith \"abλ\" \"λ\";",
      "}"
    ]

nativeAbsentStringEndsWithSourceProgram :: String
nativeAbsentStringEndsWithSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringEndsWith);",
      "  def main : Bool = stringEndsWith \"λab\" \"λ\";",
      "}"
    ]

nativeUnicodeStringAppendSourceProgram :: String
nativeUnicodeStringAppendSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringAppend);",
      "  def main : String = stringAppend \"aλ\" \"b\";",
      "}"
    ]

nativeLeftEmptyStringAppendSourceProgram :: String
nativeLeftEmptyStringAppendSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringAppend);",
      "  def main : String = stringAppend \"\" \"λ\";",
      "}"
    ]

nativeRightEmptyStringAppendSourceProgram :: String
nativeRightEmptyStringAppendSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringAppend);",
      "  def main : String = stringAppend \"λ\" \"\";",
      "}"
    ]

nativeUnicodeStringReplaceCharSourceProgram :: String
nativeUnicodeStringReplaceCharSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringReplaceChar);",
      "  def main : String = stringReplaceChar \"aλbλ\" 'λ' 'x';",
      "}"
    ]

nativeNoMatchStringReplaceCharSourceProgram :: String
nativeNoMatchStringReplaceCharSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringReplaceChar);",
      "  def main : String = stringReplaceChar \"ab\" 'λ' 'x';",
      "}"
    ]

nativeUnicodeStringReplaceSourceProgram :: String
nativeUnicodeStringReplaceSourceProgram =
  -- Fixture: import Prelude exposing (stringReplace); def main : String = stringReplace "aλbλb" "λb" "WXYZ";
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringReplace);",
      "  def main : String = stringReplace \"aλbλb\" \"λb\" \"WXYZ\";",
      "}"
    ]

nativeNoMatchStringReplaceSourceProgram :: String
nativeNoMatchStringReplaceSourceProgram =
  -- Fixture: import Prelude exposing (stringReplace); def main : String = stringReplace "abc" "λ" "x";
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringReplace);",
      "  def main : String = stringReplace \"abc\" \"λ\" \"x\";",
      "}"
    ]

nativeEmptyNeedleStringReplaceSourceProgram :: String
nativeEmptyNeedleStringReplaceSourceProgram =
  -- Fixture: import Prelude exposing (stringReplace); def main : String = stringReplace "abc" "" "x";
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringReplace);",
      "  def main : String = stringReplace \"abc\" \"\" \"x\";",
      "}"
    ]

nativePresentStringIndexOfCharSourceProgram :: String
nativePresentStringIndexOfCharSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Option(..), stringIndexOfChar);",
      "  def main : Option Int = stringIndexOfChar \"aλbλ\" 'λ';",
      "}"
    ]

nativeAbsentStringIndexOfCharSourceProgram :: String
nativeAbsentStringIndexOfCharSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Option(..), stringIndexOfChar);",
      "  def main : Option Int = stringIndexOfChar \"ab\" 'λ';",
      "}"
    ]

nativePresentStringIndexOfSourceProgram :: String
nativePresentStringIndexOfSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Option(..), stringIndexOf);",
      "  def main : Option Int = stringIndexOf \"aλbcλ\" \"λb\";",
      "}"
    ]

-- Round evidence snippets for the public source fixtures:
-- stringIndexOf "aλbcλ" "λb"
-- stringIndexOf "abc" "λ"
-- stringIndexOf "λ" ""

nativeAbsentStringIndexOfSourceProgram :: String
nativeAbsentStringIndexOfSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Option(..), stringIndexOf);",
      "  def main : Option Int = stringIndexOf \"abc\" \"λ\";",
      "}"
    ]

nativeEmptyNeedleStringIndexOfSourceProgram :: String
nativeEmptyNeedleStringIndexOfSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Option(..), stringIndexOf);",
      "  def main : Option Int = stringIndexOf \"λ\" \"\";",
      "}"
    ]

nativeUnicodeStringSplitSourceProgram :: String
nativeUnicodeStringSplitSourceProgram =
  -- Fixture: import Prelude exposing (List(..), stringSplit); def main : List String = stringSplit "aλbλc" "λ";
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (List(..), stringSplit);",
      "  def main : List String = stringSplit \"aλbλc\" \"λ\";",
      "}"
    ]

nativeNoMatchStringSplitSourceProgram :: String
nativeNoMatchStringSplitSourceProgram =
  -- Fixture: import Prelude exposing (List(..), stringSplit); def main : List String = stringSplit "abc" "λ";
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (List(..), stringSplit);",
      "  def main : List String = stringSplit \"abc\" \"λ\";",
      "}"
    ]

nativeEmptyNeedleStringSplitSourceProgram :: String
nativeEmptyNeedleStringSplitSourceProgram =
  -- Fixture: import Prelude exposing (List(..), stringSplit); def main : List String = stringSplit "abc" "";
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (List(..), stringSplit);",
      "  def main : List String = stringSplit \"abc\" \"\";",
      "}"
    ]

nativeEdgeEmptyStringSplitSourceProgram :: String
nativeEdgeEmptyStringSplitSourceProgram =
  -- Fixture: import Prelude exposing (List(..), stringSplit); def main : List String = stringSplit "λaλ" "λ";
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (List(..), stringSplit);",
      "  def main : List String = stringSplit \"λaλ\" \"λ\";",
      "}"
    ]

nativeUnicodeStringFromCharSourceProgram :: String
nativeUnicodeStringFromCharSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringFromChar);",
      "  def main : String = stringFromChar 'λ';",
      "}"
    ]

nativeAsciiStringFromCharSourceProgram :: String
nativeAsciiStringFromCharSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringFromChar);",
      "  def main : String = stringFromChar 'A';",
      "}"
    ]

nativeUnicodeStringFromListSourceProgram :: String
nativeUnicodeStringFromListSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (List(..), stringFromList);",
      "  def main : String = stringFromList (Cons 'a' (Cons 'λ' Nil));",
      "}"
    ]

nativeEmptyStringFromListSourceProgram :: String
nativeEmptyStringFromListSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (List(..), stringFromList);",
      "  def main : String = stringFromList Nil;",
      "}"
    ]

namedListCharReturnSourceProgram :: String
namedListCharReturnSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (List(..));",
      "  def f : String -> List Char = λ(value : String) Nil;",
      "  def main : List Char = f \"aλ\";",
      "}"
    ]

nativeUnicodeStringToListSourceProgram :: String
nativeUnicodeStringToListSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (List(..), stringToList);",
      "  def main : List Char = stringToList \"aλ\";",
      "}"
    ]

nativeEmptyStringToListSourceProgram :: String
nativeEmptyStringToListSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (List(..), stringToList);",
      "  def main : List Char = stringToList \"\";",
      "}"
    ]

nativePositiveStringFromIntSourceProgram :: String
nativePositiveStringFromIntSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringFromInt);",
      "  def main : String = stringFromInt 42;",
      "}"
    ]

nativeZeroStringFromIntSourceProgram :: String
nativeZeroStringFromIntSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringFromInt);",
      "  def main : String = stringFromInt 0;",
      "}"
    ]

nativeTrueStringFromBoolSourceProgram :: String
nativeTrueStringFromBoolSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringFromBool);",
      "  def main : String = stringFromBool true;",
      "}"
    ]

nativeFalseStringFromBoolSourceProgram :: String
nativeFalseStringFromBoolSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringFromBool);",
      "  def main : String = stringFromBool false;",
      "}"
    ]

nativeZeroStringFromNatSourceProgram :: String
nativeZeroStringFromNatSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Nat(..), stringFromNat);",
      "  def main : String = stringFromNat Zero;",
      "}"
    ]

nativeTwoStringFromNatSourceProgram :: String
nativeTwoStringFromNatSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Nat(..), stringFromNat);",
      "  def main : String = stringFromNat (Succ (Succ Zero));",
      "}"
    ]

nativeStringFromUnitSourceProgram :: String
nativeStringFromUnitSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), stringFromUnit);",
      "  def main : String = stringFromUnit Unit;",
      "}"
    ]

nativeDropLeadingUnicodeStringSourceProgram :: String
nativeDropLeadingUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringDrop);",
      "  def main : String = stringDrop \"λab\" 1;",
      "}"
    ]

nativeDropMixedUnicodeStringSourceProgram :: String
nativeDropMixedUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringDrop);",
      "  def main : String = stringDrop \"aλb\" 2;",
      "}"
    ]

nativeTakeLeadingUnicodeStringSourceProgram :: String
nativeTakeLeadingUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringTake);",
      "  def main : String = stringTake \"λab\" 1;",
      "}"
    ]

nativeTakeMixedUnicodeStringSourceProgram :: String
nativeTakeMixedUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringTake);",
      "  def main : String = stringTake \"aλb\" 2;",
      "}"
    ]

nativeSliceMixedUnicodeStringSourceProgram :: String
nativeSliceMixedUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringSlice);",
      "  def main : String = stringSlice \"aλbc\" 1 2;",
      "}"
    ]

nativeSliceAfterLeadingUnicodeStringSourceProgram :: String
nativeSliceAfterLeadingUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringSlice);",
      "  def main : String = stringSlice \"λabc\" 1 2;",
      "}"
    ]

nativeCharAtMixedUnicodeStringSourceProgram :: String
nativeCharAtMixedUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringCharAt);",
      "  def main : Char = stringCharAt \"aλb\" 1;",
      "}"
    ]

nativeCharAtAfterLeadingUnicodeStringSourceProgram :: String
nativeCharAtAfterLeadingUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (stringCharAt);",
      "  def main : Char = stringCharAt \"λab\" 2;",
      "}"
    ]

nativeCharAtOptionMixedUnicodeStringSourceProgram :: String
nativeCharAtOptionMixedUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Option(..), stringCharAtOption);",
      "  def main : Option Char = stringCharAtOption \"aλb\" 1;",
      "}"
    ]

nativeCharAtOptionAfterLeadingUnicodeStringSourceProgram :: String
nativeCharAtOptionAfterLeadingUnicodeStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Option(..), stringCharAtOption);",
      "  def main : Option Char = stringCharAtOption \"λab\" 2;",
      "}"
    ]

nativeCharAtOptionEndOfInputSourceProgram :: String
nativeCharAtOptionEndOfInputSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Option(..), stringCharAtOption);",
      "  def main : Option Char = stringCharAtOption \"λ\" 1;",
      "}"
    ]

nativeCharAtOptionEmptyStringSourceProgram :: String
nativeCharAtOptionEmptyStringSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Option(..), stringCharAtOption);",
      "  def main : Option Char = stringCharAtOption \"\" 0;",
      "}"
    ]

nativeDecimalCharIsDigitSourceProgram :: String
nativeDecimalCharIsDigitSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsDigit);",
      "  def main : Bool = charIsDigit '7';",
      "}"
    ]

nativeNonDecimalCharIsDigitSourceProgram :: String
nativeNonDecimalCharIsDigitSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsDigit);",
      "  def main : Bool = charIsDigit 'λ';",
      "}"
    ]

nativeAsciiLowercaseCharIsAsciiLowerSourceProgram :: String
nativeAsciiLowercaseCharIsAsciiLowerSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiLower);",
      "  def main : Bool = charIsAsciiLower 'a';",
      "}"
    ]

nativeAsciiUppercaseCharIsAsciiLowerSourceProgram :: String
nativeAsciiUppercaseCharIsAsciiLowerSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiLower);",
      "  def main : Bool = charIsAsciiLower 'A';",
      "}"
    ]

nativeNonAsciiCharIsAsciiLowerSourceProgram :: String
nativeNonAsciiCharIsAsciiLowerSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiLower);",
      "  def main : Bool = charIsAsciiLower 'λ';",
      "}"
    ]

nativeAsciiUppercaseCharIsAsciiUpperSourceProgram :: String
nativeAsciiUppercaseCharIsAsciiUpperSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiUpper);",
      "  def main : Bool = charIsAsciiUpper 'A';",
      "}"
    ]

nativeAsciiLowercaseCharIsAsciiUpperSourceProgram :: String
nativeAsciiLowercaseCharIsAsciiUpperSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiUpper);",
      "  def main : Bool = charIsAsciiUpper 'a';",
      "}"
    ]

nativeNonAsciiCharIsAsciiUpperSourceProgram :: String
nativeNonAsciiCharIsAsciiUpperSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiUpper);",
      "  def main : Bool = charIsAsciiUpper 'λ';",
      "}"
    ]

nativeAsciiLowercaseCharIsAsciiAlphaSourceProgram :: String
nativeAsciiLowercaseCharIsAsciiAlphaSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiAlpha);",
      "  def main : Bool = charIsAsciiAlpha 'a';",
      "}"
    ]

nativeAsciiUppercaseCharIsAsciiAlphaSourceProgram :: String
nativeAsciiUppercaseCharIsAsciiAlphaSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiAlpha);",
      "  def main : Bool = charIsAsciiAlpha 'A';",
      "}"
    ]

nativeAsciiDigitCharIsAsciiAlphaSourceProgram :: String
nativeAsciiDigitCharIsAsciiAlphaSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiAlpha);",
      "  def main : Bool = charIsAsciiAlpha '7';",
      "}"
    ]

nativeNonAsciiCharIsAsciiAlphaSourceProgram :: String
nativeNonAsciiCharIsAsciiAlphaSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiAlpha);",
      "  def main : Bool = charIsAsciiAlpha 'λ';",
      "}"
    ]

nativeAsciiLowercaseCharIsAsciiAlphaNumSourceProgram :: String
nativeAsciiLowercaseCharIsAsciiAlphaNumSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiAlphaNum);",
      "  def main : Bool = charIsAsciiAlphaNum 'a';",
      "}"
    ]

nativeAsciiUppercaseCharIsAsciiAlphaNumSourceProgram :: String
nativeAsciiUppercaseCharIsAsciiAlphaNumSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiAlphaNum);",
      "  def main : Bool = charIsAsciiAlphaNum 'A';",
      "}"
    ]

nativeAsciiDigitCharIsAsciiAlphaNumSourceProgram :: String
nativeAsciiDigitCharIsAsciiAlphaNumSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiAlphaNum);",
      "  def main : Bool = charIsAsciiAlphaNum '7';",
      "}"
    ]

nativeAsciiUnderscoreCharIsAsciiAlphaNumSourceProgram :: String
nativeAsciiUnderscoreCharIsAsciiAlphaNumSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiAlphaNum);",
      "  def main : Bool = charIsAsciiAlphaNum '_';",
      "}"
    ]

nativeNonAsciiCharIsAsciiAlphaNumSourceProgram :: String
nativeNonAsciiCharIsAsciiAlphaNumSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiAlphaNum);",
      "  def main : Bool = charIsAsciiAlphaNum 'λ';",
      "}"
    ]

nativeAsciiLowercaseCharIsAsciiIdentifierStartSourceProgram :: String
nativeAsciiLowercaseCharIsAsciiIdentifierStartSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierStart);",
      "  def main : Bool = charIsAsciiIdentifierStart 'a';",
      "}"
    ]

nativeAsciiUppercaseCharIsAsciiIdentifierStartSourceProgram :: String
nativeAsciiUppercaseCharIsAsciiIdentifierStartSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierStart);",
      "  def main : Bool = charIsAsciiIdentifierStart 'A';",
      "}"
    ]

nativeAsciiUnderscoreCharIsAsciiIdentifierStartSourceProgram :: String
nativeAsciiUnderscoreCharIsAsciiIdentifierStartSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierStart);",
      "  def main : Bool = charIsAsciiIdentifierStart '_';",
      "}"
    ]

nativeAsciiDigitCharIsAsciiIdentifierStartSourceProgram :: String
nativeAsciiDigitCharIsAsciiIdentifierStartSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierStart);",
      "  def main : Bool = charIsAsciiIdentifierStart '7';",
      "}"
    ]

nativeAsciiApostropheCharIsAsciiIdentifierStartSourceProgram :: String
nativeAsciiApostropheCharIsAsciiIdentifierStartSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierStart);",
      "  def main : Bool = charIsAsciiIdentifierStart '\\'';",
      "}"
    ]

nativeNonAsciiCharIsAsciiIdentifierStartSourceProgram :: String
nativeNonAsciiCharIsAsciiIdentifierStartSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierStart);",
      "  def main : Bool = charIsAsciiIdentifierStart 'λ';",
      "}"
    ]

nativeAsciiLowercaseCharIsAsciiIdentifierContinueSourceProgram :: String
nativeAsciiLowercaseCharIsAsciiIdentifierContinueSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierContinue);",
      "  def main : Bool = charIsAsciiIdentifierContinue 'a';",
      "}"
    ]

nativeAsciiUppercaseCharIsAsciiIdentifierContinueSourceProgram :: String
nativeAsciiUppercaseCharIsAsciiIdentifierContinueSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierContinue);",
      "  def main : Bool = charIsAsciiIdentifierContinue 'A';",
      "}"
    ]

nativeAsciiDigitCharIsAsciiIdentifierContinueSourceProgram :: String
nativeAsciiDigitCharIsAsciiIdentifierContinueSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierContinue);",
      "  def main : Bool = charIsAsciiIdentifierContinue '7';",
      "}"
    ]

nativeAsciiUnderscoreCharIsAsciiIdentifierContinueSourceProgram :: String
nativeAsciiUnderscoreCharIsAsciiIdentifierContinueSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierContinue);",
      "  def main : Bool = charIsAsciiIdentifierContinue '_';",
      "}"
    ]

nativeAsciiApostropheCharIsAsciiIdentifierContinueSourceProgram :: String
nativeAsciiApostropheCharIsAsciiIdentifierContinueSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierContinue);",
      "  def main : Bool = charIsAsciiIdentifierContinue '\\'';",
      "}"
    ]

nativeNonAsciiCharIsAsciiIdentifierContinueSourceProgram :: String
nativeNonAsciiCharIsAsciiIdentifierContinueSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiIdentifierContinue);",
      "  def main : Bool = charIsAsciiIdentifierContinue 'λ';",
      "}"
    ]

nativeAsciiSpaceCharIsAsciiWhitespaceSourceProgram :: String
nativeAsciiSpaceCharIsAsciiWhitespaceSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiWhitespace);",
      "  def main : Bool = charIsAsciiWhitespace ' ';",
      "}"
    ]

nativeAsciiTabCharIsAsciiWhitespaceSourceProgram :: String
nativeAsciiTabCharIsAsciiWhitespaceSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiWhitespace);",
      "  def main : Bool = charIsAsciiWhitespace '\\t';",
      "}"
    ]

nativeAsciiNewlineCharIsAsciiWhitespaceSourceProgram :: String
nativeAsciiNewlineCharIsAsciiWhitespaceSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiWhitespace);",
      "  def main : Bool = charIsAsciiWhitespace '\\n';",
      "}"
    ]

nativeAsciiCarriageReturnCharIsAsciiWhitespaceSourceProgram :: String
nativeAsciiCarriageReturnCharIsAsciiWhitespaceSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiWhitespace);",
      "  def main : Bool = charIsAsciiWhitespace '\\r';",
      "}"
    ]

nativeAsciiFormFeedCharIsAsciiWhitespaceSourceProgram :: String
nativeAsciiFormFeedCharIsAsciiWhitespaceSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiWhitespace);",
      "  def main : Bool = charIsAsciiWhitespace '\\f';",
      "}"
    ]

nativeAsciiVerticalTabCharIsAsciiWhitespaceSourceProgram :: String
nativeAsciiVerticalTabCharIsAsciiWhitespaceSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiWhitespace);",
      "  def main : Bool = charIsAsciiWhitespace '\\v';",
      "}"
    ]

nativeAsciiLowercaseCharIsAsciiWhitespaceSourceProgram :: String
nativeAsciiLowercaseCharIsAsciiWhitespaceSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiWhitespace);",
      "  def main : Bool = charIsAsciiWhitespace 'a';",
      "}"
    ]

nativeNonAsciiCharIsAsciiWhitespaceSourceProgram :: String
nativeNonAsciiCharIsAsciiWhitespaceSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiWhitespace);",
      "  def main : Bool = charIsAsciiWhitespace 'λ';",
      "}"
    ]

nativeAsciiExclamationCharIsAsciiPunctuationSourceProgram :: String
nativeAsciiExclamationCharIsAsciiPunctuationSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPunctuation);",
      "  def main : Bool = charIsAsciiPunctuation '!';",
      "}"
    ]

nativeAsciiUnderscoreCharIsAsciiPunctuationSourceProgram :: String
nativeAsciiUnderscoreCharIsAsciiPunctuationSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPunctuation);",
      "  def main : Bool = charIsAsciiPunctuation '_';",
      "}"
    ]

nativeAsciiTildeCharIsAsciiPunctuationSourceProgram :: String
nativeAsciiTildeCharIsAsciiPunctuationSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPunctuation);",
      "  def main : Bool = charIsAsciiPunctuation '~';",
      "}"
    ]

nativeAsciiLowercaseCharIsAsciiPunctuationSourceProgram :: String
nativeAsciiLowercaseCharIsAsciiPunctuationSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPunctuation);",
      "  def main : Bool = charIsAsciiPunctuation 'a';",
      "}"
    ]

nativeAsciiDigitCharIsAsciiPunctuationSourceProgram :: String
nativeAsciiDigitCharIsAsciiPunctuationSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPunctuation);",
      "  def main : Bool = charIsAsciiPunctuation '7';",
      "}"
    ]

nativeAsciiSpaceCharIsAsciiPunctuationSourceProgram :: String
nativeAsciiSpaceCharIsAsciiPunctuationSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPunctuation);",
      "  def main : Bool = charIsAsciiPunctuation ' ';",
      "}"
    ]

nativeNonAsciiCharIsAsciiPunctuationSourceProgram :: String
nativeNonAsciiCharIsAsciiPunctuationSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPunctuation);",
      "  def main : Bool = charIsAsciiPunctuation 'λ';",
      "}"
    ]

nativeAsciiSpaceCharIsAsciiPrintableSourceProgram :: String
nativeAsciiSpaceCharIsAsciiPrintableSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPrintable);",
      "  def main : Bool = charIsAsciiPrintable ' ';",
      "}"
    ]

nativeAsciiExclamationCharIsAsciiPrintableSourceProgram :: String
nativeAsciiExclamationCharIsAsciiPrintableSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPrintable);",
      "  def main : Bool = charIsAsciiPrintable '!';",
      "}"
    ]

nativeAsciiUppercaseCharIsAsciiPrintableSourceProgram :: String
nativeAsciiUppercaseCharIsAsciiPrintableSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPrintable);",
      "  def main : Bool = charIsAsciiPrintable 'A';",
      "}"
    ]

nativeAsciiDigitCharIsAsciiPrintableSourceProgram :: String
nativeAsciiDigitCharIsAsciiPrintableSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPrintable);",
      "  def main : Bool = charIsAsciiPrintable '7';",
      "}"
    ]

nativeAsciiTildeCharIsAsciiPrintableSourceProgram :: String
nativeAsciiTildeCharIsAsciiPrintableSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPrintable);",
      "  def main : Bool = charIsAsciiPrintable '~';",
      "}"
    ]

nativeAsciiTabCharIsAsciiPrintableSourceProgram :: String
nativeAsciiTabCharIsAsciiPrintableSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPrintable);",
      "  def main : Bool = charIsAsciiPrintable '\\t';",
      "}"
    ]

nativeAsciiNewlineCharIsAsciiPrintableSourceProgram :: String
nativeAsciiNewlineCharIsAsciiPrintableSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPrintable);",
      "  def main : Bool = charIsAsciiPrintable '\\n';",
      "}"
    ]

nativeNonAsciiCharIsAsciiPrintableSourceProgram :: String
nativeNonAsciiCharIsAsciiPrintableSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (charIsAsciiPrintable);",
      "  def main : Bool = charIsAsciiPrintable 'λ';",
      "}"
    ]

nativeCharLiteralSourceProgram :: String
nativeCharLiteralSourceProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Char = 'λ';",
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
      "  def tailOrNil : RList -> RList = λ(xs : RList) case xs of {",
      "    RNil -> RNil;",
      "    RCons _ rest -> rest",
      "  };",
      "",
      "  def isNil : RList -> Bool = λ(xs : RList) case xs of {",
      "    RNil -> true;",
      "    RCons _ _ -> false",
      "  };",
      "",
      "  def main : Bool = isNil (tailOrNil (RCons Zero RNil));",
      "}"
    ]

localFirstClassPolymorphismProgram :: String
localFirstClassPolymorphismProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Bool =",
      "    let usePoly : (∀ a. a -> a) -> Bool =",
      "      λ(poly : ∀ a. a -> a) let keepInt = poly 1 in poly true",
      "    in let id : ∀ a. a -> a = λx x in usePoly id;",
      "}"
    ]

paperBoundedSelfApplicationProgram :: String
paperBoundedSelfApplicationProgram =
  unlines
    [ "module Main export (omega, id, main) {",
      "  def omega : ∀(result ⩾ ∀ a. a -> a). (∀ a. a -> a) -> result =",
      "    λ(g : ∀ a. a -> a) g g;",
      "  def id : ∀ a. a -> a = λx x;",
      "  def main : Bool =",
      "    let recovered : ∀ a. a -> a = omega id in recovered true;",
      "}"
    ]

sourceEscapingPolymorphicMainProgram :: String
sourceEscapingPolymorphicMainProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : ∀ a. a -> a = λx x;",
      "}"
    ]

firstClassFunctionArgumentProgram :: String
firstClassFunctionArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int) f 1;",
      "  def main : Int = use (λ(x : Int) x);",
      "}"
    ]

typeAppliedGlobalStaticAliasProgram :: String
typeAppliedGlobalStaticAliasProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : ∀ a. a -> a = λx x;",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int) f 1;",
      "  def main : Int = use id;",
      "}"
    ]

staleStaticFunctionArgumentProgram :: BackendProgram
staleStaticFunctionArgumentProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = staleStaticFunctionIdentity,
          backendBindingNameWithMetadata = "id",
          backendBindingTypeWithMetadata = unaryIntTy,
          backendBindingExprWithMetadata =
            BackendLam unaryIntTy "x" intTy (BackendVar intTy "x"),
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "use",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow unaryIntTy intTy)
              "f"
              unaryIntTy
              (BackendApp intTy (BackendVar unaryIntTy "f") (intLit 1)),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (fixtureTopLevelVar (BTArrow unaryIntTy intTy) "use")
              (BackendVarWithIdentity unaryIntTy ((TopLevelId staleStaticFunctionIdentity)) "$stale_id"),
          backendBindingExportedAsMain = True
        }
    ]

staleEtaAliasStaticFunctionArgumentProgram :: BackendProgram
staleEtaAliasStaticFunctionArgumentProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = staleStaticFunctionIdentity,
          backendBindingNameWithMetadata = "id",
          backendBindingTypeWithMetadata = unaryIntTy,
          backendBindingExprWithMetadata =
            BackendLam unaryIntTy "x" intTy (BackendVar intTy "x"),
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "use",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow unaryIntTy intTy)
              "f"
              unaryIntTy
              (BackendApp intTy (BackendVar unaryIntTy "f") (intLit 1)),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (fixtureTopLevelVar (BTArrow unaryIntTy intTy) "use")
              ( BackendLam
                  unaryIntTy
                  "y"
                  intTy
                  ( BackendApp
                      intTy
                      (BackendVarWithIdentity unaryIntTy ((TopLevelId staleStaticFunctionIdentity)) "$stale_id")
                      (BackendVar intTy "y")
                  )
              ),
          backendBindingExportedAsMain = True
        }
    ]

staleEtaAliasParamIdentityProgram :: BackendProgram
staleEtaAliasParamIdentityProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = staleStaticFunctionIdentity,
          backendBindingNameWithMetadata = "id",
          backendBindingTypeWithMetadata = unaryIntTy,
          backendBindingExprWithMetadata =
            BackendLam unaryIntTy "x" intTy (BackendVar intTy "x"),
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "use",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow unaryIntTy intTy)
              "f"
              unaryIntTy
              (BackendApp intTy (BackendVar unaryIntTy "f") (intLit 1)),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (fixtureTopLevelVar (BTArrow unaryIntTy intTy) "use")
              ( BackendLamWithIdentity
                  unaryIntTy
                  (localYIdentity)
                  "$stale_y"
                  intTy
                  ( BackendApp
                      intTy
                      (BackendVarWithIdentity unaryIntTy ((TopLevelId staleStaticFunctionIdentity)) "$stale_id")
                      (BackendVarWithIdentity intTy (localYIdentity) "y")
                  )
              ),
          backendBindingExportedAsMain = True
        }
    ]
  where
    localYIdentity = localIdentity 2069119 "$stale_y"

mismatchedGlobalBindingIdentityProgram :: BackendProgram
mismatchedGlobalBindingIdentityProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = staleStaticFunctionIdentity,
          backendBindingNameWithMetadata = "id",
          backendBindingTypeWithMetadata = intTy,
          backendBindingExprWithMetadata = intLit 1,
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendVarWithIdentity intTy ((TopLevelId otherStaticFunctionIdentity)) "id",
          backendBindingExportedAsMain = True
        }
    ]

staleStaticFunctionIdentity :: SymbolIdentity
staleStaticFunctionIdentity =
  symbolIdentityFromParts (UniqueIdentity 990001) SymbolValue "Main" "id" Nothing

otherStaticFunctionIdentity :: SymbolIdentity
otherStaticFunctionIdentity =
  symbolIdentityFromParts (UniqueIdentity 990008) SymbolValue "Other" "id" Nothing

shadowedNameGlobalReachabilityProgram :: BackendProgram
shadowedNameGlobalReachabilityProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = helperIdentity,
          backendBindingNameWithMetadata = "helper",
          backendBindingTypeWithMetadata = unaryIntTy,
          backendBindingExprWithMetadata = intIdentityExpr,
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLetWithIdentity
              intTy
              (localHelperIdentity)
              "helper"
              intTy
              (intLit 0)
              ( BackendApp
                  intTy
                  (BackendVarWithIdentity unaryIntTy ((TopLevelId helperIdentity)) "helper")
                  (intLit 7)
              ),
          backendBindingExportedAsMain = True
        }
    ]
  where
    localHelperIdentity = localIdentity 2069114 "helper"

shadowedNameGlobalMismatchedReachabilityProgram :: BackendProgram
shadowedNameGlobalMismatchedReachabilityProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = helperIdentity,
          backendBindingNameWithMetadata = "helper",
          backendBindingTypeWithMetadata = unaryIntTy,
          backendBindingExprWithMetadata = intIdentityExpr,
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLetWithIdentity
              intTy
              (localHelperIdentity)
              "helper"
              intTy
              (intLit 0)
              (BackendApp intTy (BackendVar unaryIntTy "helper") (intLit 7)),
          backendBindingExportedAsMain = True
        }
    ]
  where
    localHelperIdentity = localIdentity 2069120 "helper"

helperIdentity :: SymbolIdentity
helperIdentity =
  symbolIdentityFromParts (UniqueIdentity 990003) SymbolValue "Main" "helper" Nothing

shadowedNameGlobalValueIdentityProgram :: BackendProgram
shadowedNameGlobalValueIdentityProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = shadowedGlobalValueIdentity,
          backendBindingNameWithMetadata = "x",
          backendBindingTypeWithMetadata = intTy,
          backendBindingExprWithMetadata = intLit 41,
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLetWithIdentity
              intTy
              (localXIdentity)
              "x"
              intTy
              (intLit 1)
              (BackendVarWithIdentity intTy ((TopLevelId shadowedGlobalValueIdentity)) "x"),
          backendBindingExportedAsMain = True
        }
    ]
  where
    localXIdentity = localIdentity 2069118 "x"

shadowedGlobalValueIdentity :: SymbolIdentity
shadowedGlobalValueIdentity =
  symbolIdentityFromParts (UniqueIdentity 990010) SymbolValue "Main" "x" Nothing

dataIdentityStaleTypeHeadProgram :: BackendProgram
dataIdentityStaleTypeHeadProgram =
  dataIdentityTypeHeadProgramWith dataIdentityBoxStaleTy

dataIdentityMismatchedTypeHeadProgram :: BackendProgram
dataIdentityMismatchedTypeHeadProgram =
  dataIdentityTypeHeadProgramWith dataIdentityBoxMismatchedTy

letBoundNativeOptionCaseProgram :: BackendProgram
letBoundNativeOptionCaseProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [letBoundNativeOptionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = boolTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          boolTy
                          letBoundNativeOptionLocal
                          "opt"
                          letBoundNativeOptionTy
                          letBoundNativeOptionCall
                          ( BackendCase
                              boolTy
                              (BackendVarWithIdentity letBoundNativeOptionTy (letBoundNativeOptionLocal) "$stale_opt")
                              ( BackendAlternative
                                  (BackendConstructorPatternWithBinderIdentities (letBoundNativeOptionNoneIdentity) "$stale_None" [])
                                  (boolLit False)
                                  :| [ BackendAlternative
                                         ( BackendConstructorPatternWithBinderIdentities
                                             (letBoundNativeOptionSomeIdentity)
                                             "$stale_Some"
                                             [BackendPatternBinder (letBoundNativeOptionCharLocal) "$stale_ch"]
                                         )
                                         (boolLit True)
                                     ]
                              )
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

dataIdentityTypeHeadProgramWith :: BackendType -> BackendProgram
dataIdentityTypeHeadProgramWith identityBoxTy =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [dataIdentityBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = BTArrow identityBoxTy identityBoxTy,
                      backendBindingExpr =
                        BackendLam
                          (BTArrow identityBoxTy identityBoxTy)
                          "x"
                          identityBoxTy
                          (BackendVar identityBoxTy "x"),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

dataIdentityBoxData :: BackendData
dataIdentityBoxData =
  BackendDataWithIdentity
    { backendDataIdentity = dataIdentityBoxIdentity,
      backendDataNameWithIdentity = "IdentityBox",
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity = []
    }

dataIdentityBoxStaleTy :: BackendType
dataIdentityBoxStaleTy =
  BTBaseWithIdentity (dataIdentityBoxIdentity) (BaseTy "stale.IdentityBox")

dataIdentityBoxMismatchedTy :: BackendType
dataIdentityBoxMismatchedTy =
  BTBaseWithIdentity (otherDataIdentityBoxIdentity) (BaseTy "IdentityBox")

letBoundNativeOptionData :: BackendData
letBoundNativeOptionData =
  BackendDataWithIdentity
    { backendDataIdentity = letBoundNativeOptionIdentity,
      backendDataNameWithIdentity = "Prelude.Option",
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity =
        [ BackendConstructorWithIdentity
            { backendConstructorIdentity = letBoundNativeOptionNoneIdentity,
              backendConstructorNameWithIdentity = "None",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [],
              backendConstructorResultWithIdentity = letBoundNativeOptionTy
            },
          BackendConstructorWithIdentity
            { backendConstructorIdentity = letBoundNativeOptionSomeIdentity,
              backendConstructorNameWithIdentity = "Some",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [charTy],
              backendConstructorResultWithIdentity = letBoundNativeOptionTy
            }
        ]
    }

letBoundNativeOptionTy :: BackendType
letBoundNativeOptionTy =
  BTConWithIdentity (letBoundNativeOptionIdentity) (BaseTy "Prelude.Option") (charTy :| [])

letBoundNativeOptionCall :: BackendExpr
letBoundNativeOptionCall =
  BackendApp
    letBoundNativeOptionTy
    ( BackendApp
        (BTArrow intTy letBoundNativeOptionTy)
        ( BackendVarWithIdentity
            letBoundStringCharAtOptionTy
            (PrimitiveId (primitiveRefFromSymbol (PrimitiveInventory.builtinValueIdentity PrimitiveInventory.stringCharAtOptionPrimitiveName)))
            "$stale_string_char_at_option"
        )
        (BackendLit stringTy (LString "ab"))
    )
    (intLit 0)

letBoundStringCharAtOptionTy :: BackendType
letBoundStringCharAtOptionTy =
  BTArrow stringTy (BTArrow intTy letBoundNativeOptionTy)

letBoundNativeOptionLocal :: IdDetails
letBoundNativeOptionLocal =
  localIdentity 991730 "opt"

letBoundNativeOptionCharLocal :: IdDetails
letBoundNativeOptionCharLocal =
  localIdentity 991731 "ch"

letBoundNativeOptionIdentity :: SymbolIdentity
letBoundNativeOptionIdentity =
  symbolIdentityFromParts (UniqueIdentity 991732) SymbolType "Prelude" "Option" Nothing

letBoundNativeOptionNoneIdentity :: SymbolIdentity
letBoundNativeOptionNoneIdentity =
  symbolIdentityFromParts (UniqueIdentity 991733) SymbolConstructor "Prelude" "None" (Just (SymbolOwnerType letBoundNativeOptionIdentity))

letBoundNativeOptionSomeIdentity :: SymbolIdentity
letBoundNativeOptionSomeIdentity =
  symbolIdentityFromParts (UniqueIdentity 991734) SymbolConstructor "Prelude" "Some" (Just (SymbolOwnerType letBoundNativeOptionIdentity))

dataIdentityBoxIdentity :: SymbolIdentity
dataIdentityBoxIdentity =
  symbolIdentityFromParts (UniqueIdentity 990004) SymbolType "Main" "IdentityBox" Nothing

conflictingStaleDataIdentity :: SymbolIdentity
conflictingStaleDataIdentity =
  renameSymbolDefiningName "$stale.IdentityBox" dataIdentityBoxIdentity

otherDataIdentityBoxIdentity :: SymbolIdentity
otherDataIdentityBoxIdentity =
  symbolIdentityFromParts (UniqueIdentity 990009) SymbolType "Other" "IdentityBox" Nothing

fakeBuiltinIntIdentity :: SymbolIdentity
fakeBuiltinIntIdentity =
  symbolIdentityFromParts (UniqueIdentity 990010) SymbolType "Other" "Int" Nothing

fakeBuiltinIntTy :: BackendType
fakeBuiltinIntTy =
  BTBaseWithIdentity (fakeBuiltinIntIdentity) (BaseTy "Int")

fakeBuiltinIntProgram :: BackendProgram
fakeBuiltinIntProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "main",
          backendBindingType = BTArrow fakeBuiltinIntTy fakeBuiltinIntTy,
          backendBindingExpr =
            BackendLam
              (BTArrow fakeBuiltinIntTy fakeBuiltinIntTy)
              "x"
              fakeBuiltinIntTy
              (BackendVar fakeBuiltinIntTy "x"),
          backendBindingExportedAsMain = True
        }
    ]

constructorFirstClassPolymorphismProgram :: String
constructorFirstClassPolymorphismProgram =
  unlines
    [ "module Main export (PolyBox(..), main) {",
      "  data PolyBox =",
      "      PolyBox : (∀ a. a -> a) -> PolyBox;",
      "",
      "  def main : Bool = case PolyBox (λx x) of {",
      "    PolyBox poly -> let keepInt = poly 1 in poly true",
      "  };",
      "}"
    ]

staticArgumentMainProgram :: BackendProgram
staticArgumentMainProgram =
  programWithMainExpr staticPolyBoolTy $
    BackendLam staticPolyBoolTy "poly" polyIdTy (boolLit True)

recursiveStaticGlobalProgram :: BackendProgram
recursiveStaticGlobalProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = recursiveStaticLoopIdentity,
          backendBindingNameWithMetadata = "loop",
          backendBindingTypeWithMetadata = staticPolyBoolTy,
          backendBindingExprWithMetadata =
            BackendLamWithIdentity
              staticPolyBoolTy
              (recursiveStaticPolyIdentity)
              "poly"
              polyIdTy
              ( BackendApp
                  boolTy
                  (BackendVarWithIdentity staticPolyBoolTy ((TopLevelId recursiveStaticLoopIdentity)) "loop")
                  (BackendVarWithIdentity polyIdTy (recursiveStaticPolyIdentity) "poly")
              ),
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = boolTy,
          backendBindingExpr =
            BackendApp
              boolTy
              (BackendVarWithIdentity staticPolyBoolTy ((TopLevelId recursiveStaticLoopIdentity)) "loop")
              polyIdExpr,
          backendBindingExportedAsMain = True
        }
    ]
  where
    recursiveStaticLoopIdentity = symbolIdentityFromParts (UniqueIdentity 992032) SymbolValue "Main" "loop" Nothing
    recursiveStaticPolyIdentity = localIdentity 992033 "poly"

ordinaryFunctionEvidenceMethodProgram :: String
ordinaryFunctionEvidenceMethodProgram =
  unlines
    [ "module Main export (C, apply, idInt, use, main) {",
      "  class C a {",
      "    apply : (a -> a) -> a -> a;",
      "  }",
      "",
      "  instance C Int {",
      "    apply = λf λx f x;",
      "  }",
      "",
      "  def idInt : Int -> Int = λx x;",
      "  def use : C a => (a -> a) -> a -> a = λf λx apply f x;",
      "  def main : Int = use idInt 1;",
      "}"
    ]

sourceTopLevelRecursiveHigherOrderProgram :: String
sourceTopLevelRecursiveHigherOrderProgram =
  unlines
    [ "module Main export (Nat(..), loop, idInt, main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def idInt : Int -> Int = λ(x : Int) x;",
      "  def loop : (Int -> Int) -> Nat -> Int = λ(f : Int -> Int) λ(n : Nat) case n of {",
      "    Zero -> f 1;",
      "    Succ inner -> loop f inner",
      "  };",
      "  def main : Int = loop idInt (Succ Zero);",
      "}"
    ]

sourceLocalRecursiveHigherOrderProgram :: String
sourceLocalRecursiveHigherOrderProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Int =",
      "    let idInt : Int -> Int = λ(x : Int) x in",
      "    let loop : (Int -> Int) -> Nat -> Int = λ(f : Int -> Int) λ(n : Nat) case n of {",
      "      Zero -> f 1;",
      "      Succ inner -> loop f inner",
      "    } in",
      "    loop idInt (Succ Zero);",
      "}"
    ]

localFunctionEvidenceMethodProgram :: String
localFunctionEvidenceMethodProgram =
  unlines
    [ "module Main export (C, apply, use, main) {",
      "  class C a {",
      "    apply : (a -> a) -> a -> a;",
      "  }",
      "",
      "  instance C Int {",
      "    apply = λf λx f x;",
      "  }",
      "",
      "  def use : C a => a -> a = λx let f : a -> a = λy y in apply f x;",
      "  def main : Int = use 1;",
      "}"
    ]

selfRecursiveHigherOrderProgram :: BackendProgram
selfRecursiveHigherOrderProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = selfRecursiveIdIdentity,
          backendBindingNameWithMetadata = "id",
          backendBindingTypeWithMetadata = unaryIntTy,
          backendBindingExprWithMetadata = intIdentityExpr,
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBindingWithMetadata
        { backendBindingIdentity = selfRecursiveLoopIdentity,
          backendBindingNameWithMetadata = "loop",
          backendBindingTypeWithMetadata = BTArrow unaryIntTy unaryIntTy,
          backendBindingExprWithMetadata =
            BackendLamWithIdentity
              (BTArrow unaryIntTy unaryIntTy)
              (selfRecursiveFIdentity)
              "f"
              unaryIntTy
              ( BackendLamWithIdentity
                  unaryIntTy
                  (selfRecursiveXIdentity)
                  "x"
                  intTy
                  ( BackendApp
                      intTy
                      ( BackendApp
                          unaryIntTy
                          (BackendVarWithIdentity (BTArrow unaryIntTy unaryIntTy) ((TopLevelId selfRecursiveLoopIdentity)) "loop")
                          (BackendVarWithIdentity unaryIntTy (selfRecursiveFIdentity) "f")
                      )
                      (BackendVarWithIdentity intTy (selfRecursiveXIdentity) "x")
                  )
              ),
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              ( BackendApp
                  unaryIntTy
                  (BackendVarWithIdentity (BTArrow unaryIntTy unaryIntTy) ((TopLevelId selfRecursiveLoopIdentity)) "loop")
                  (BackendVarWithIdentity unaryIntTy ((TopLevelId selfRecursiveIdIdentity)) "id")
              )
              (intLit 1),
          backendBindingExportedAsMain = True
        }
    ]
  where
    selfRecursiveIdIdentity = symbolIdentityFromParts (UniqueIdentity 992040) SymbolValue "Main" "id" Nothing
    selfRecursiveLoopIdentity = symbolIdentityFromParts (UniqueIdentity 992041) SymbolValue "Main" "loop" Nothing
    selfRecursiveFIdentity = localIdentity 992042 "f"
    selfRecursiveXIdentity = localIdentity 992043 "x"

inlineOnlyEvidenceCalleeProgram :: BackendProgram
inlineOnlyEvidenceCalleeProgram =
  programWithEvidenceParamIndices
    [("$evidence_C", [0]), ("caller", [0])]
    [ BackendBinding
        { backendBindingName = "id",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "callee",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow unaryIntTy intTy)
              "f"
              unaryIntTy
              (BackendApp intTy (BackendVar unaryIntTy "f") (intLit 1)),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow (BTArrow unaryIntTy intTy) intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow (BTArrow unaryIntTy intTy) intTy)
              "$evidence_method"
              (BTArrow unaryIntTy intTy)
              ( BackendApp
                  intTy
                  (BackendVar (BTArrow unaryIntTy intTy) "$evidence_method")
                  (fixtureTopLevelVar unaryIntTy "id")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "caller",
          backendBindingType = BTArrow (BTArrow (BTArrow unaryIntTy intTy) intTy) intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow (BTArrow (BTArrow unaryIntTy intTy) intTy) intTy)
              "$evidence_C"
              (BTArrow (BTArrow unaryIntTy intTy) intTy)
              ( BackendApp
                  intTy
                  (BackendVar (BTArrow (BTArrow unaryIntTy intTy) intTy) "$evidence_C")
                  (fixtureTopLevelVar (BTArrow unaryIntTy intTy) "callee")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (fixtureTopLevelVar (BTArrow (BTArrow (BTArrow unaryIntTy intTy) intTy) intTy) "caller")
              (fixtureTopLevelVar (BTArrow (BTArrow unaryIntTy intTy) intTy) "$evidence_C"),
          backendBindingExportedAsMain = True
        }
    ]

inlineOnlyEvidenceParameterCallCalleeProgram :: BackendProgram
inlineOnlyEvidenceParameterCallCalleeProgram =
  programWithEvidenceParamIndices
    [("calleeWithEvidenceCall", [0]), ("$evidence_C", [0]), ("caller", [0])]
    [ BackendBinding
        { backendBindingName = "id",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "$evidence_apply",
          backendBindingType = higherOrderEvidenceTy,
          backendBindingExpr =
            BackendLam
              higherOrderEvidenceTy
              "f"
              unaryIntTy
              ( BackendLam
                  unaryIntTy
                  "x"
                  intTy
                  (BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x"))
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "calleeWithEvidenceCall",
          backendBindingType = BTArrow higherOrderEvidenceTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow higherOrderEvidenceTy intTy)
              "$evidence_apply"
              higherOrderEvidenceTy
              ( BackendLet
                  intTy
                  "localId"
                  unaryIntTy
                  (fixtureTopLevelVar unaryIntTy "id")
                  ( BackendApp
                      intTy
                      ( BackendApp
                          unaryIntTy
                          (BackendVar higherOrderEvidenceTy "$evidence_apply")
                          (BackendVar unaryIntTy "localId")
                      )
                      (intLit 1)
                  )
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy)
              "$evidence_method"
              (BTArrow higherOrderEvidenceTy intTy)
              ( BackendApp
                  intTy
                  (BackendVar (BTArrow higherOrderEvidenceTy intTy) "$evidence_method")
                  (fixtureTopLevelVar higherOrderEvidenceTy "$evidence_apply")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "caller",
          backendBindingType = BTArrow (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) intTy)
              "$evidence_C"
              (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy)
              ( BackendApp
                  intTy
                  (BackendVar (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) "$evidence_C")
                  (fixtureTopLevelVar (BTArrow higherOrderEvidenceTy intTy) "calleeWithEvidenceCall")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (fixtureTopLevelVar (BTArrow (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) intTy) "caller")
              (fixtureTopLevelVar (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) "$evidence_C"),
          backendBindingExportedAsMain = True
        }
    ]

staleEvidenceWrapperKeyProgram :: BackendProgram
staleEvidenceWrapperKeyProgram =
  programWithEvidenceParamIndices
    [("caller", [0])]
    [ BackendBinding
        { backendBindingName = "caller",
          backendBindingType = BTArrow higherOrderEvidenceTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow higherOrderEvidenceTy intTy)
              "$evidence_apply"
              higherOrderEvidenceTy
              (intLit 0),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLet
              intTy
              "left"
              intTy
              (BackendApp intTy (fixtureTopLevelVar (BTArrow higherOrderEvidenceTy intTy) "caller") stableEvidenceWrapperKeyLambda)
              (BackendApp intTy (fixtureTopLevelVar (BTArrow higherOrderEvidenceTy intTy) "caller") staleEvidenceWrapperKeyLambda),
          backendBindingExportedAsMain = True
        }
    ]

stableEvidenceWrapperKeyLambda :: BackendExpr
stableEvidenceWrapperKeyLambda =
  BackendLamWithIdentity
    higherOrderEvidenceTy
    staleEvidenceWrapperParamIdentity
    "f"
    unaryIntTy
    (BackendVarWithIdentity unaryIntTy staleEvidenceWrapperParamIdentity "f")

staleEvidenceWrapperKeyLambda :: BackendExpr
staleEvidenceWrapperKeyLambda =
  BackendLamWithIdentity
    higherOrderEvidenceTy
    staleEvidenceWrapperParamIdentity
    "$stale_f"
    unaryIntTy
    (BackendVarWithIdentity unaryIntTy staleEvidenceWrapperParamIdentity "$stale_f")

staleEvidenceWrapperParamIdentity :: IdDetails
staleEvidenceWrapperParamIdentity =
  LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991718)) "f")

aliasedInlineOnlyEvidenceCalleeProgram :: BackendProgram
aliasedInlineOnlyEvidenceCalleeProgram =
  programWithEvidenceParamIndices
    [("calleeWithEvidenceCall", [0]), ("$evidence_C", [0]), ("caller", [0])]
    [ BackendBinding
        { backendBindingName = "id",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "$evidence_apply",
          backendBindingType = higherOrderEvidenceTy,
          backendBindingExpr =
            BackendLam
              higherOrderEvidenceTy
              "f"
              unaryIntTy
              ( BackendLam
                  unaryIntTy
                  "x"
                  intTy
                  (BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x"))
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "calleeWithEvidenceCall",
          backendBindingType = BTArrow higherOrderEvidenceTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow higherOrderEvidenceTy intTy)
              "$evidence_apply"
              higherOrderEvidenceTy
              ( BackendLet
                  intTy
                  "localId"
                  unaryIntTy
                  (fixtureTopLevelVar unaryIntTy "id")
                  ( BackendApp
                      intTy
                      ( BackendApp
                          unaryIntTy
                          (BackendVar higherOrderEvidenceTy "$evidence_apply")
                          (BackendVar unaryIntTy "localId")
                      )
                      (intLit 1)
                  )
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy)
              "$evidence_method"
              (BTArrow higherOrderEvidenceTy intTy)
              ( BackendApp
                  intTy
                  (BackendVar (BTArrow higherOrderEvidenceTy intTy) "$evidence_method")
                  (fixtureTopLevelVar higherOrderEvidenceTy "$evidence_apply")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "caller",
          backendBindingType = BTArrow (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) intTy)
              "$evidence_C"
              (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy)
              ( BackendLet
                  intTy
                  "methodAlias"
                  (BTArrow higherOrderEvidenceTy intTy)
                  (fixtureTopLevelVar (BTArrow higherOrderEvidenceTy intTy) "calleeWithEvidenceCall")
                  ( BackendApp
                      intTy
                      (BackendVar (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) "$evidence_C")
                      (BackendVar (BTArrow higherOrderEvidenceTy intTy) "methodAlias")
                  )
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (fixtureTopLevelVar (BTArrow (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) intTy) "caller")
              (fixtureTopLevelVar (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) "$evidence_C"),
          backendBindingExportedAsMain = True
        }
    ]

shadowedLocalAliasReferenceCollectorProgram :: BackendProgram
shadowedLocalAliasReferenceCollectorProgram =
  programWithEvidenceParamIndices
    [("actualCalleeWithEvidenceCall", [0]), ("shadowedCalleeWithEvidenceCall", [0]), ("$evidence_C", [0])]
    [ BackendBinding
        { backendBindingName = "id",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "$evidence_apply",
          backendBindingType = higherOrderEvidenceTy,
          backendBindingExpr =
            BackendLam
              higherOrderEvidenceTy
              "f"
              unaryIntTy
              ( BackendLam
                  unaryIntTy
                  "x"
                  intTy
                  (BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x"))
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "actualCalleeWithEvidenceCall",
          backendBindingType = BTArrow higherOrderEvidenceTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow higherOrderEvidenceTy intTy)
              "$evidence_apply"
              higherOrderEvidenceTy
              ( BackendLet
                  intTy
                  "localId"
                  unaryIntTy
                  (fixtureTopLevelVar unaryIntTy "id")
                  ( BackendApp
                      intTy
                      ( BackendApp
                          unaryIntTy
                          (BackendVar higherOrderEvidenceTy "$evidence_apply")
                          (BackendVar unaryIntTy "localId")
                      )
                      (intLit 1)
                  )
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "shadowedCalleeWithEvidenceCall",
          backendBindingType = BTArrow higherOrderEvidenceTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow higherOrderEvidenceTy intTy)
              "$evidence_apply"
              higherOrderEvidenceTy
              ( BackendLet
                  intTy
                  "localId"
                  unaryIntTy
                  (fixtureTopLevelVar unaryIntTy "id")
                  ( BackendApp
                      intTy
                      ( BackendApp
                          unaryIntTy
                          (BackendVar higherOrderEvidenceTy "$evidence_apply")
                          (BackendVar unaryIntTy "localId")
                      )
                      (intLit 1)
                  )
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy)
              "$evidence_method"
              (BTArrow higherOrderEvidenceTy intTy)
              ( BackendApp
                  intTy
                  (BackendVar (BTArrow higherOrderEvidenceTy intTy) "$evidence_method")
                  (fixtureTopLevelVar higherOrderEvidenceTy "$evidence_apply")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLet
              intTy
              "$evidence_method"
              (BTArrow higherOrderEvidenceTy intTy)
              (fixtureTopLevelVar (BTArrow higherOrderEvidenceTy intTy) "shadowedCalleeWithEvidenceCall")
              ( BackendApp
                  intTy
                  ( BackendLam
                      (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy)
                      "$evidence_method"
                      (BTArrow higherOrderEvidenceTy intTy)
                      ( BackendApp
                          intTy
                          (fixtureTopLevelVar (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) "$evidence_C")
                          (BackendVar (BTArrow higherOrderEvidenceTy intTy) "$evidence_method")
                      )
                  )
                  (fixtureTopLevelVar (BTArrow higherOrderEvidenceTy intTy) "actualCalleeWithEvidenceCall")
              ),
          backendBindingExportedAsMain = True
        }
    ]

shadowedNameReferencedCalleeIdentityProgram :: BackendProgram
shadowedNameReferencedCalleeIdentityProgram =
  programWithEvidenceParamIndices
    [("calleeWithEvidenceCall", [0]), ("$evidence_C", [0])]
    [ BackendBindingWithMetadata
        { backendBindingIdentity = calleeWithEvidenceCallIdentity,
          backendBindingNameWithMetadata = "calleeWithEvidenceCall",
          backendBindingTypeWithMetadata = BTArrow higherOrderEvidenceTy intTy,
          backendBindingExprWithMetadata =
            BackendLam
              (BTArrow higherOrderEvidenceTy intTy)
              "$evidence_apply"
              higherOrderEvidenceTy
              (intLit 0),
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "$evidence_apply",
          backendBindingType = higherOrderEvidenceTy,
          backendBindingExpr =
            BackendLam
              higherOrderEvidenceTy
              "f"
              unaryIntTy
              ( BackendLam
                  unaryIntTy
                  "x"
                  intTy
                  (BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x"))
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy)
              "$evidence_method"
              (BTArrow higherOrderEvidenceTy intTy)
              ( BackendApp
                  intTy
                  (BackendVar (BTArrow higherOrderEvidenceTy intTy) "$evidence_method")
                  (fixtureTopLevelVar higherOrderEvidenceTy "$evidence_apply")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLetWithIdentity
              intTy
              (localCalleeIdentity)
              "calleeWithEvidenceCall"
              intTy
              (intLit 0)
              ( BackendApp
                  intTy
                  (fixtureTopLevelVar (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) "$evidence_C")
                  ( BackendVarWithIdentity
                      (BTArrow higherOrderEvidenceTy intTy)
                      (TopLevelId calleeWithEvidenceCallIdentity)
                      "calleeWithEvidenceCall"
                  )
              ),
          backendBindingExportedAsMain = True
        }
    ]
  where
    localCalleeIdentity = localIdentity 2069115 "calleeWithEvidenceCall"

calleeWithEvidenceCallIdentity :: SymbolIdentity
calleeWithEvidenceCallIdentity =
  symbolIdentityFromParts (UniqueIdentity 990004) SymbolValue "Main" "calleeWithEvidenceCall" Nothing

capturingEvidenceWrapperProgram :: BackendProgram
capturingEvidenceWrapperProgram =
  programWithEvidenceParamIndices
    [("$evidence_C", [0])]
    [ BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLamWithIdentity
              { backendParamIdentity = fixtureLocalDetails "$evidence_apply", backendExprType = BTArrow unaryIntTy intTy,
                backendParamName = "$evidence_apply",
                backendParamType = unaryIntTy,
                backendBody =
                  BackendApp
                    intTy
                    (BackendVar unaryIntTy "$evidence_apply")
                    (intLit 0)
              },
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLet
              intTy
              "x"
              intTy
              (intLit 1)
              ( BackendApp
                  intTy
                  (fixtureTopLevelVar (BTArrow unaryIntTy intTy) "$evidence_C")
                  ( BackendLam
                      unaryIntTy
                      "y"
                      intTy
                      (BackendVar intTy "x")
                  )
              ),
          backendBindingExportedAsMain = True
        }
    ]

staleLocalIdentityEvidenceWrapperProgram :: BackendProgram
staleLocalIdentityEvidenceWrapperProgram =
  programWithEvidenceParamIndices
    [("$evidence_C", [0])]
    [ BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLamWithIdentity
              { backendParamIdentity = fixtureLocalDetails "$evidence_apply", backendExprType = BTArrow unaryIntTy intTy,
                backendParamName = "$evidence_apply",
                backendParamType = unaryIntTy,
                backendBody =
                  BackendApp
                    intTy
                    (BackendVar unaryIntTy "$evidence_apply")
                    (intLit 0)
              },
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "$stale_x",
          backendBindingType = intTy,
          backendBindingExpr = intLit 41,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLetWithIdentity
              intTy
              (localXIdentity)
              "x"
              intTy
              (intLit 1)
              ( BackendApp
                  intTy
                  (fixtureTopLevelVar (BTArrow unaryIntTy intTy) "$evidence_C")
                  ( BackendLam
                      unaryIntTy
                      "y"
                      intTy
                      (BackendVarWithIdentity intTy (localXIdentity) "$stale_x")
                  )
              ),
          backendBindingExportedAsMain = True
        }
    ]
  where
    localXIdentity = localIdentity 2069117 "x"

globalIdentityEvidenceWrapperProgram :: BackendProgram
globalIdentityEvidenceWrapperProgram =
  programWithEvidenceParamIndices
    [("$evidence_C", [0])]
    [ BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLamWithIdentity
              { backendParamIdentity = fixtureLocalDetails "$evidence_apply", backendExprType = BTArrow unaryIntTy intTy,
                backendParamName = "$evidence_apply",
                backendParamType = unaryIntTy,
                backendBody =
                  BackendApp
                    intTy
                    (BackendVar unaryIntTy "$evidence_apply")
                    (intLit 0)
              },
          backendBindingExportedAsMain = False
        },
      BackendBindingWithMetadata
        { backendBindingIdentity = globalEvidenceXIdentity,
          backendBindingNameWithMetadata = "x",
          backendBindingTypeWithMetadata = intTy,
          backendBindingExprWithMetadata = intLit 41,
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLetWithIdentity
              intTy
              (localEvidenceXIdentity)
              "x"
              intTy
              (intLit 1)
              ( BackendApp
                  intTy
                  (fixtureTopLevelVar (BTArrow unaryIntTy intTy) "$evidence_C")
                  ( BackendLam
                      unaryIntTy
                      "y"
                      intTy
                      (BackendVarWithIdentity intTy ((TopLevelId globalEvidenceXIdentity)) "x")
                  )
              ),
          backendBindingExportedAsMain = True
        }
    ]
  where
    localEvidenceXIdentity = localIdentity 2069116 "x"

globalEvidenceXIdentity :: SymbolIdentity
globalEvidenceXIdentity =
  symbolIdentityFromParts (UniqueIdentity 990009) SymbolValue "Main" "x" Nothing

nestedEvidenceWrapperParameterProgram :: BackendProgram
nestedEvidenceWrapperParameterProgram =
  programWithEvidenceParamIndices
    [("$evidence_C", [0])]
    [ BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow higherOrderEvidenceTy intTy,
          backendBindingExpr =
            BackendLamWithIdentity
              { backendParamIdentity = fixtureLocalDetails "$evidence_apply", backendExprType = BTArrow higherOrderEvidenceTy intTy,
                backendParamName = "$evidence_apply",
                backendParamType = higherOrderEvidenceTy,
                backendBody =
                  BackendApp
                    intTy
                    ( BackendApp
                        unaryIntTy
                        (BackendVar higherOrderEvidenceTy "$evidence_apply")
                        intIdentityExpr
                    )
                    (intLit 1)
              },
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (fixtureTopLevelVar (BTArrow higherOrderEvidenceTy intTy) "$evidence_C")
              ( BackendLam
                  higherOrderEvidenceTy
                  "f"
                  unaryIntTy
                  ( BackendLam
                      unaryIntTy
                      "x"
                      intTy
                      (BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x"))
                  )
              ),
          backendBindingExportedAsMain = True
        }
    ]

polymorphicEvidenceWrapperProgram :: BackendProgram
polymorphicEvidenceWrapperProgram =
  programWithEvidenceParamIndices
    [("$evidence_C", [0])]
    [ BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = polyEvidenceConsumerTy,
          backendBindingExpr = polyEvidenceConsumerExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "poly",
          backendBindingType = polyEvidenceCallerTy,
          backendBindingExpr = polyEvidenceCallerExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendTyApp
              intTy
              (fixtureTopLevelVar polyEvidenceCallerTy "poly")
              intTy,
          backendBindingExportedAsMain = True
        }
    ]

localPolymorphicEvidenceWrapperProgram :: BackendProgram
localPolymorphicEvidenceWrapperProgram =
  programWithEvidenceParamIndices
    [("$evidence_C", [0])]
    [ BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = polyEvidenceConsumerTy,
          backendBindingExpr = polyEvidenceConsumerExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLet
              intTy
              "poly"
              polyEvidenceCallerTy
              polyEvidenceCallerExpr
              ( BackendTyApp
                  intTy
                  (BackendVar polyEvidenceCallerTy "poly")
                  intTy
              ),
          backendBindingExportedAsMain = True
        }
    ]

polyEvidenceConsumerTy :: BackendType
polyEvidenceConsumerTy =
  BTForall "a" Nothing (BTArrow (BTArrow (BTVar "a") intTy) intTy)

polyEvidenceConsumerExpr :: BackendExpr
polyEvidenceConsumerExpr =
  BackendTyAbs
    polyEvidenceConsumerTy
    "a"
    Nothing
    ( BackendLam
        (BTArrow (BTArrow (BTVar "a") intTy) intTy)
        "$evidence_apply"
        (BTArrow (BTVar "a") intTy)
        (intLit 0)
    )

polyEvidenceCallerTy :: BackendType
polyEvidenceCallerTy =
  BTForall "a" Nothing intTy

polyEvidenceCallerExpr :: BackendExpr
polyEvidenceCallerExpr =
  BackendTyAbs
    polyEvidenceCallerTy
    "a"
    Nothing
    ( BackendApp
        intTy
        ( BackendTyApp
            (BTArrow (BTArrow (BTVar "a") intTy) intTy)
            (fixtureTopLevelVar polyEvidenceConsumerTy "$evidence_C")
            (BTVar "a")
        )
        ( BackendLam
            (BTArrow (BTVar "a") intTy)
            "x"
            (BTVar "a")
            (intLit 0)
        )
    )

localFunctionCallShadowsValueProgram :: BackendProgram
localFunctionCallShadowsValueProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "inner",
          backendBindingType = polyIdTy,
          backendBindingExpr = polyIdExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "id",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "callee",
          backendBindingType = BTArrow polyIdTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow polyIdTy intTy)
              "$evidence_E"
              polyIdTy
              ( BackendApp
                  intTy
                  (BackendTyApp unaryIntTy (BackendVar polyIdTy "$evidence_E") intTy)
                  (intLit 7)
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "outer",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow unaryIntTy intTy)
              "$evidence_E"
              unaryIntTy
              ( BackendApp
                  intTy
                  (fixtureTopLevelVar (BTArrow polyIdTy intTy) "callee")
                  (fixtureTopLevelVar polyIdTy "inner")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (fixtureTopLevelVar (BTArrow unaryIntTy intTy) "outer")
              (fixtureTopLevelVar unaryIntTy "id"),
          backendBindingExportedAsMain = True
        }
    ]

localFunctionReferenceShadowsValueProgram :: BackendProgram
localFunctionReferenceShadowsValueProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "idInt",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "idBool",
          backendBindingType = unaryBoolTy,
          backendBindingExpr = boolIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "callBool",
          backendBindingType = BTArrow unaryBoolTy intTy,
          backendBindingExpr =
            BackendLam
              (BTArrow unaryBoolTy intTy)
              "f"
              unaryBoolTy
              (intLit 1),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "outer",
          backendBindingType = BTArrow unaryIntTy (BTArrow (BTArrow unaryBoolTy intTy) intTy),
          backendBindingExpr =
            BackendLam
              (BTArrow unaryIntTy (BTArrow (BTArrow unaryBoolTy intTy) intTy))
              "$evidence_E"
              unaryIntTy
              ( BackendLam
                  (BTArrow (BTArrow unaryBoolTy intTy) intTy)
                  "$evidence_Call"
                  (BTArrow unaryBoolTy intTy)
                  ( BackendLet
                      intTy
                      "$evidence_E"
                      unaryBoolTy
                      (fixtureTopLevelVar unaryBoolTy "idBool")
                      ( BackendApp
                          intTy
                          (BackendVar (BTArrow unaryBoolTy intTy) "$evidence_Call")
                          (BackendVar unaryBoolTy "$evidence_E")
                      )
                  )
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              ( BackendApp
                  (BTArrow (BTArrow unaryBoolTy intTy) intTy)
                  (fixtureTopLevelVar (BTArrow unaryIntTy (BTArrow (BTArrow unaryBoolTy intTy) intTy)) "outer")
                  (fixtureTopLevelVar unaryIntTy "idInt")
              )
              (fixtureTopLevelVar (BTArrow unaryBoolTy intTy) "callBool"),
          backendBindingExportedAsMain = True
        }
    ]

inlineFunctionArgumentShadowsValueProgram :: BackendProgram
inlineFunctionArgumentShadowsValueProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "idInt",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr = intIdentityExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "callee",
                      backendBindingType = BTArrow unaryIntTy fnBoxTy,
                      backendBindingExpr =
                        BackendLam
                          (BTArrow unaryIntTy fnBoxTy)
                          "f"
                          unaryIntTy
                          (fnBoxConstruct [closureWithEntry "__mlfp_closure$field_shadow"]),
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "outer",
                      backendBindingType = BTArrow unaryIntTy fnBoxTy,
                      backendBindingExpr =
                        BackendLam
                          (BTArrow unaryIntTy fnBoxTy)
                          "f"
                          unaryIntTy
                          ( BackendApp
                              fnBoxTy
                              (fixtureTopLevelVar (BTArrow unaryIntTy fnBoxTy) "callee")
                              (fixtureTopLevelVar unaryIntTy "idInt")
                          ),
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = fnBoxTy,
                      backendBindingExpr =
                        BackendApp
                          fnBoxTy
                          (fixtureTopLevelVar (BTArrow unaryIntTy fnBoxTy) "outer")
                          (fixtureTopLevelVar unaryIntTy "idInt"),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }
stringProgram :: BackendProgram
stringProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModuleWithIdentity
            { backendModuleIdentity = mainFixtureModuleIdentity,
              backendModuleNameWithIdentity = "Main",
              backendModuleDataWithIdentity = [],
              backendModuleBindingsWithIdentity =
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

staleMainIdentityProgram :: BackendProgram
staleMainIdentityProgram =
  BackendProgramWithIdentity
    { backendProgramModulesWithIdentity =
        [ BackendModuleWithIdentity
            { backendModuleIdentity = mainFixtureModuleIdentity,
              backendModuleNameWithIdentity = "Main",
              backendModuleDataWithIdentity = [],
              backendModuleBindingsWithIdentity =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = staleMainIdentity,
                      backendBindingNameWithMetadata = "actual-main",
                      backendBindingTypeWithMetadata = intTy,
                      backendBindingExprWithMetadata = intLit 1,
                      backendBindingExportedAsMainWithMetadata = True,
                      backendBindingEvidenceParamIndices = Set.empty
                    }
                ]
            }
        ],
      backendProgramMainIdentity = staleMainIdentity,
      backendProgramMainWithIdentity = "$stale-main"
    }

staleMainSelfReferenceProgram :: BackendProgram
staleMainSelfReferenceProgram =
  BackendProgramWithIdentity
    { backendProgramModulesWithIdentity =
        [ BackendModuleWithIdentity
            { backendModuleIdentity = mainFixtureModuleIdentity,
              backendModuleNameWithIdentity = "Main",
              backendModuleDataWithIdentity = [],
              backendModuleBindingsWithIdentity =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = staleMainIdentity,
                      backendBindingNameWithMetadata = "actual-main",
                      backendBindingTypeWithMetadata = intTy,
                      backendBindingExprWithMetadata =
                        BackendVarWithIdentity intTy ((TopLevelId staleMainIdentity)) "$stale-main",
                      backendBindingExportedAsMainWithMetadata = True,
                      backendBindingEvidenceParamIndices = Set.empty
                    }
                ]
            }
        ],
      backendProgramMainIdentity = staleMainIdentity,
      backendProgramMainWithIdentity = "$stale-main"
    }

staleMainMismatchedSelfReferenceProgram :: BackendProgram
staleMainMismatchedSelfReferenceProgram =
  BackendProgramWithIdentity
    { backendProgramModulesWithIdentity =
        [ BackendModuleWithIdentity
            { backendModuleIdentity = mainFixtureModuleIdentity,
              backendModuleNameWithIdentity = "Main",
              backendModuleDataWithIdentity = [],
              backendModuleBindingsWithIdentity =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = staleMainIdentity,
                      backendBindingNameWithMetadata = "actual-main",
                      backendBindingTypeWithMetadata = intTy,
                      backendBindingExprWithMetadata = BackendVar intTy "actual-main",
                      backendBindingExportedAsMainWithMetadata = True,
                      backendBindingEvidenceParamIndices = Set.empty
                    }
                ]
            }
        ],
      backendProgramMainIdentity = staleMainIdentity,
      backendProgramMainWithIdentity = "$stale-main"
    }

staleMainWithHelperIdentityProgram :: BackendProgram
staleMainWithHelperIdentityProgram =
  BackendProgramWithIdentity
    { backendProgramModulesWithIdentity =
        [ BackendModuleWithIdentity
            { backendModuleIdentity = mainFixtureModuleIdentity,
              backendModuleNameWithIdentity = "Main",
              backendModuleDataWithIdentity = [],
              backendModuleBindingsWithIdentity =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = staleHelperIdentity,
                      backendBindingNameWithMetadata = "actual-helper",
                      backendBindingTypeWithMetadata = intTy,
                      backendBindingExprWithMetadata = intLit 41,
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBindingWithMetadata
                    { backendBindingIdentity = staleMainIdentity,
                      backendBindingNameWithMetadata = "actual-main",
                      backendBindingTypeWithMetadata = intTy,
                      backendBindingExprWithMetadata =
                        BackendVarWithIdentity intTy ((TopLevelId staleHelperIdentity)) "$stale-helper",
                      backendBindingExportedAsMainWithMetadata = True,
                      backendBindingEvidenceParamIndices = Set.empty
                    }
                ]
            }
        ],
      backendProgramMainIdentity = staleMainIdentity,
      backendProgramMainWithIdentity = "$stale-main"
    }

staleMainIdentity :: SymbolIdentity
staleMainIdentity =
  symbolIdentityFromParts (UniqueIdentity 990011) SymbolValue "Main" "main" Nothing

staleHelperIdentity :: SymbolIdentity
staleHelperIdentity =
  symbolIdentityFromParts (UniqueIdentity 990012) SymbolValue "Main" "helper" Nothing

mainFixtureModuleIdentity :: SymbolIdentity
mainFixtureModuleIdentity =
  symbolIdentityFromParts (UniqueIdentity 990013) SymbolModule "Main" "Main" Nothing

conflictingStaleHelperIdentity :: SymbolIdentity
conflictingStaleHelperIdentity =
  renameSymbolDefiningName "$stale-helper" staleHelperIdentity


nativeMainNameCollisionProgram :: BackendProgram
nativeMainNameCollisionProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr = intLit 1,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

nativeMainIdentityNameCollisionProgram :: BackendProgram
nativeMainIdentityNameCollisionProgram =
  BackendProgramWithIdentity
    { backendProgramModulesWithIdentity =
        [ BackendModuleWithIdentity
            { backendModuleIdentity = mainFixtureModuleIdentity,
              backendModuleNameWithIdentity = "Main",
              backendModuleDataWithIdentity = [],
              backendModuleBindingsWithIdentity =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = nativeMainCollisionIdentity,
                      backendBindingNameWithMetadata = "main",
                      backendBindingTypeWithMetadata = intTy,
                      backendBindingExprWithMetadata = intLit 1,
                      backendBindingExportedAsMainWithMetadata = True,
                      backendBindingEvidenceParamIndices = Set.empty
                    }
                ]
            }
        ],
      backendProgramMainIdentity = nativeMainCollisionIdentity,
      backendProgramMainWithIdentity = "main"
    }

nativeMainCollisionIdentity :: SymbolIdentity
nativeMainCollisionIdentity =
  symbolIdentityFromParts (UniqueIdentity 991735) SymbolValue "Main" "main" Nothing

identityBearingModuleMismatchedBindingProgram :: BackendProgram
identityBearingModuleMismatchedBindingProgram =
  BackendProgramWithIdentity
    { backendProgramModulesWithIdentity =
        [ BackendModuleWithIdentity
            { backendModuleIdentity = identityBearingModuleIdentity,
              backendModuleNameWithIdentity = "Main",
              backendModuleDataWithIdentity = [],
              backendModuleBindingsWithIdentity =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr = intLit 1,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMainIdentity = identityBearingMismatchedMainIdentity,
      backendProgramMainWithIdentity = "main"
    }

identityBearingModuleIdentity :: SymbolIdentity
identityBearingModuleIdentity =
  symbolIdentityFromParts (UniqueIdentity 991736) SymbolModule "Main" "Main" Nothing

identityBearingMismatchedMainIdentity :: SymbolIdentity
identityBearingMismatchedMainIdentity =
  symbolIdentityFromParts (UniqueIdentity 991737) SymbolValue "Main" "main" Nothing

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

staleTypeKeySpecializationProgram :: BackendProgram
staleTypeKeySpecializationProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
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
                          stableIdentityStringTy
                          (polyIdCall stableIdentityStringTy (BackendLit stableIdentityStringTy (LString "left")))
                          ( BackendLet
                              intTy
                              "right"
                              staleIdentityStringTy
                              (polyIdCall staleIdentityStringTy (BackendLit staleIdentityStringTy (LString "right")))
                              (intLit 0)
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

stableIdentityStringTy :: BackendType
stableIdentityStringTy =
  BTBaseWithIdentity ((PrimitiveInventory.builtinTypeIdentity "String")) (BaseTy "String")

staleIdentityStringTy :: BackendType
staleIdentityStringTy =
  BTBaseWithIdentity ((PrimitiveInventory.builtinTypeIdentity "String")) (BaseTy "$stale_String")

staleTypeAbsResultBinderIdentityProgram :: BackendProgram
staleTypeAbsResultBinderIdentityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "poly",
                      backendBindingType = staleTypeAbsResultTy,
                      backendBindingExpr = staleTypeAbsResultExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendTyApp
                              (BTArrow intTy intTy)
                              (fixtureTopLevelVar staleTypeAbsResultTy "poly")
                              intTy
                          )
                          (intLit 42),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

staleTypeAbsResultTy :: BackendType
staleTypeAbsResultTy =
  BTForallWithIdentity
    (staleTypeAbsBinderIdentity)
    "$stale_a"
    Nothing
    ( BTArrow
        (BTVarWithIdentity (staleTypeAbsBinderIdentity) "$stale_a")
        (BTVarWithIdentity (staleTypeAbsBinderIdentity) "$stale_a")
    )

staleTypeAbsResultExpr :: BackendExpr
staleTypeAbsResultExpr =
  BackendTyAbsWithIdentity
    staleTypeAbsResultTy
    staleTypeAbsBinderIdentity
    "a"
    Nothing
    ( BackendLam
        (BTArrow binderTy binderTy)
        "x"
        binderTy
        (BackendVar binderTy "x")
    )
  where
    binderTy = BTVarWithIdentity (staleTypeAbsBinderIdentity) "a"

staleTypeAbsBinderIdentity :: TypeBinderIdentity
staleTypeAbsBinderIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 990006)

typeBinderStableAliasProgram :: BackendProgram
typeBinderStableAliasProgram =
  programWithMainExpr intTy $
    BackendApp
      intTy
      (BackendTyApp (BTArrow intTy intTy) typeAbs intTy)
      (intLit 42)
  where
    binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 990007)
    stableName = typeBinderIdentityStableName binderIdentity
    binderTy = BTVarWithIdentity (binderIdentity) "a"
    polyTy = BTForallWithIdentity (binderIdentity) "a" Nothing (BTArrow binderTy binderTy)
    stableTy = BTVarWithIdentity (binderIdentity) stableName
    typeAbs =
      BackendTyAbsWithIdentity
        polyTy
        binderIdentity
        "a"
        Nothing
        ( BackendLam
            (BTArrow stableTy stableTy)
            "x"
            stableTy
            (BackendVar stableTy "x")
        )

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
                          (fixtureTopLevelVar nonePolyTy "none")
                          intTy,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

staleGlobalPolymorphicZeroArityProgram :: BackendProgram
staleGlobalPolymorphicZeroArityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = staleNoneIdentity,
                      backendBindingNameWithMetadata = "none",
                      backendBindingTypeWithMetadata = nonePolyTy,
                      backendBindingExprWithMetadata = nonePolyExpr,
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr =
                        BackendTyApp
                          (optionTy intTy)
                          (BackendVarWithIdentity nonePolyTy ((TopLevelId staleNoneIdentity)) "$stale_none")
                          intTy,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

staleNoneIdentity :: SymbolIdentity
staleNoneIdentity =
  symbolIdentityFromParts (UniqueIdentity 990002) SymbolValue "Main" "none" Nothing

shadowedNameGlobalPolymorphicZeroArityProgram :: BackendProgram
shadowedNameGlobalPolymorphicZeroArityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = staleNoneIdentity,
                      backendBindingNameWithMetadata = "none",
                      backendBindingTypeWithMetadata = nonePolyTy,
                      backendBindingExprWithMetadata = nonePolyExpr,
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          (optionTy intTy)
                          (localNoneIdentity)
                          "none"
                          intTy
                          (intLit 0)
                          ( BackendTyApp
                              (optionTy intTy)
                              (BackendVarWithIdentity nonePolyTy ((TopLevelId staleNoneIdentity)) "none")
                              intTy
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }
  where
    localNoneIdentity = localIdentity 2069113 "none"

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

localPolymorphicClosureEntryProgram :: BackendProgram
localPolymorphicClosureEntryProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLet
                          intTy
                          "polyLocal"
                          localPolymorphicClosureEntryTy
                          localPolymorphicClosureEntryExpr
                          ( BackendApp
                              intTy
                              ( BackendTyApp
                                  (BTArrow intTy intTy)
                                  (BackendVar localPolymorphicClosureEntryTy "polyLocal")
                                  intTy
                              )
                              (intLit 3)
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

localPolymorphicClosureEntryTy :: BackendType
localPolymorphicClosureEntryTy =
  BTForall "a" Nothing (BTArrow (BTVar "a") (BTVar "a"))

localPolymorphicClosureEntryExpr :: BackendExpr
localPolymorphicClosureEntryExpr =
  BackendTyAbs
    localPolymorphicClosureEntryTy
    "a"
    Nothing
    ( BackendLam
        (BTArrow (BTVar "a") (BTVar "a"))
        "x"
        (BTVar "a")
        ( BackendLet
            (BTVar "a")
            "f"
            (BTArrow (BTVar "a") (BTVar "a"))
            ( BackendClosureWithParamIdentities
                { backendExprType = BTArrow (BTVar "a") (BTVar "a"),
              backendClosureEntryIdentity = fixtureClosureEntryIdentity "__mlfp_closure$local_poly_identity",
              backendClosureEntryName = "__mlfp_closure$local_poly",
                  backendClosureCaptures = [],
                  backendClosureParamsWithIdentities = backendClosureParams [("y", BTVar "a")],
                  backendClosureBody = BackendVar (BTVar "a") "y"
                }
            )
            (BackendClosureCall (BTVar "a") (BackendVar (BTArrow (BTVar "a") (BTVar "a")) "f") [BackendVar (BTVar "a") "x"])
        )
    )

localFunctionFormIdentityTypeAppProgram :: BackendProgram
localFunctionFormIdentityTypeAppProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          intTy
                          (polyLocalIdentity)
                          "polyLocal"
                          localPolymorphicClosureEntryTy
                          localPolyExpr
                          ( BackendApp
                              intTy
                              ( BackendTyApp
                                  (BTArrow intTy intTy)
                                  (BackendVarWithIdentity localPolymorphicClosureEntryTy (polyLocalIdentity) "$stale_polyLocal")
                                  intTy
                              )
                              (intLit 3)
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }
  where
    polyLocalIdentity = localIdentity 2069112 "polyLocal"
    localPolyExpr =
      BackendTyAbs
        localPolymorphicClosureEntryTy
        "a"
        Nothing
        ( BackendLam
            (BTArrow (BTVar "a") (BTVar "a"))
            "x"
            (BTVar "a")
            ( BackendLet
                (BTVar "a")
                "f"
                (BTArrow (BTVar "a") (BTVar "a"))
                ( BackendClosureWithParamIdentities
                    { backendExprType = BTArrow (BTVar "a") (BTVar "a"),
              backendClosureEntryIdentity = fixtureClosureEntryIdentity "__mlfp_closure$shadow_f",
              backendClosureEntryName = "__mlfp_closure$local_poly_identity",
                      backendClosureCaptures = [],
                      backendClosureParamsWithIdentities = backendClosureParams [("y", BTVar "a")],
                      backendClosureBody = BackendVar (BTVar "a") "y"
                    }
                )
                (BackendClosureCall (BTVar "a") (BackendVar (BTArrow (BTVar "a") (BTVar "a")) "f") [BackendVar (BTVar "a") "x"])
            )
        )

directPolymorphicClosureCallProgram :: BackendProgram
directPolymorphicClosureCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          (BackendTyApp (BTArrow intTy intTy) directPolymorphicClosureCallExpr intTy)
                          (intLit 3),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

directPolymorphicClosureCallExpr :: BackendExpr
directPolymorphicClosureCallExpr =
  BackendTyAbs
    localPolymorphicClosureEntryTy
    "a"
    Nothing
    ( BackendLam
        (BTArrow (BTVar "a") (BTVar "a"))
        "x"
        (BTVar "a")
        ( BackendLet
            (BTVar "a")
            "f"
            (BTArrow (BTVar "a") (BTVar "a"))
            ( BackendClosureWithParamIdentities
                { backendExprType = BTArrow (BTVar "a") (BTVar "a"),
              backendClosureEntryIdentity = fixtureClosureEntryIdentity "__mlfp_closure$capture_stale_f",
              backendClosureEntryName = "__mlfp_closure$direct_call_poly",
                  backendClosureCaptures = [],
                  backendClosureParamsWithIdentities = backendClosureParams [("y", BTVar "a")],
                  backendClosureBody = BackendVar (BTVar "a") "y"
                }
            )
            (BackendClosureCall (BTVar "a") (BackendVar (BTArrow (BTVar "a") (BTVar "a")) "f") [BackendVar (BTVar "a") "x"])
        )
    )

letHeadPolymorphicClosureCallProgram :: BackendProgram
letHeadPolymorphicClosureCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendTyApp
                              (BTArrow intTy intTy)
                              ( BackendLet
                                  localPolymorphicClosureEntryTy
                                  "polyLocal"
                                  localPolymorphicClosureEntryTy
                                  directPolymorphicClosureCallExpr
                                  (BackendVar localPolymorphicClosureEntryTy "polyLocal")
                              )
                              intTy
                          )
                          (intLit 3),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

structuralClosureResultProgram :: BackendProgram
structuralClosureResultProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [resultBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = resultBoxStructuralTy,
                      backendBindingExpr =
                        BackendClosureCall
                          resultBoxStructuralTy
                          ( BackendClosureWithParamIdentities
                              { backendExprType = BTArrow intTy resultBoxTy,
              backendClosureEntryIdentity = UniqueIdentity (-992000),
              backendClosureEntryName = "__mlfp_closure$result_structural",
                                backendClosureCaptures = [],
                                backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                backendClosureBody =
                                  BackendConstruct resultBoxTy "ResultBox" [BackendVar intTy "x"]
                              }
                          )
                          [intLit 3],
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
                              (fixtureTopLevelVar nonePolyTy "none")
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
                        fnBoxConstruct [fixtureTopLevelVar unaryIntTy "helper"],
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
              (BackendTyApp unaryIntTy (fixtureTopLevelVar polyIdTy "poly") intTy)
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

localIdentityShadowedLetProgram :: BackendProgram
localIdentityShadowedLetProgram =
  programWithMainExpr intTy $
    BackendLetWithIdentity
      intTy
      (outerIdentity)
      "x"
      intTy
      (intLit 41)
      ( BackendLetWithIdentity
          intTy
          (innerIdentity)
          "x"
          intTy
          (intLit 99)
          (BackendVarWithIdentity intTy (outerIdentity) "x")
      )
  where
    outerIdentity = localIdentity 2069101 "x"
    innerIdentity = localIdentity 2069102 "x"

localIdentityStableAliasProgram :: BackendProgram
localIdentityStableAliasProgram =
  programWithMainExpr intTy $
    BackendLetWithIdentity
      intTy
      localX
      "x"
      intTy
      (intLit 42)
      (BackendVarWithIdentity intTy (localX) (idDetailsStableName localX))
  where
    localX = localIdentity 2069105 "x"

lambdaIdentityShadowedParamProgram :: BackendProgram
lambdaIdentityShadowedParamProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "shadow",
          backendBindingType = binaryIntTy,
          backendBindingExpr =
            BackendLamWithIdentity
              binaryIntTy
              (outerIdentity)
              "x"
              intTy
              ( BackendLamWithIdentity
                  unaryIntTy
                  (innerIdentity)
                  "x"
                  intTy
                  (BackendVarWithIdentity intTy (outerIdentity) "x")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (BackendApp unaryIntTy (fixtureTopLevelVar binaryIntTy "shadow") (intLit 41))
              (intLit 99),
          backendBindingExportedAsMain = True
        }
    ]
  where
    outerIdentity = localIdentity 2069103 "x"
    innerIdentity = localIdentity 2069104 "x"

casePatternIdentityStaleBinderProgram :: BackendProgram
casePatternIdentityStaleBinderProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          intTy
                          (outerIdentity)
                          "x"
                          intTy
                          (intLit 41)
                          ( BackendCase
                              intTy
                              (optionSome intTy [intLit 99])
                              ( BackendAlternative
                                  ( BackendConstructorPatternWithBinderIdentities
                                      (optionSomeIdentity)
                                      "Some"
                                      [BackendPatternBinder (fieldIdentity) "$stale_x"]
                                  )
                                  (BackendVarWithIdentity intTy (fieldIdentity) "x")
                                  :| []
                              )
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    outerIdentity = localIdentity 2069105 "x"
    fieldIdentity = localIdentity 2069106 "$stale_x"

casePatternIdentityDuplicateDisplayProgram :: BackendProgram
casePatternIdentityDuplicateDisplayProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [intPairData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          (BackendConstruct intPairTy "IntPair" [intLit 41, intLit 99])
                          ( BackendAlternative
                              ( BackendConstructorPatternWithBinderIdentities
                                  (fixtureSymbolIdentity SymbolConstructor "IntPair")
                                  "IntPair"
                                  [ BackendPatternBinder (leftFieldIdentity) "x",
                                    BackendPatternBinder (rightFieldIdentity) "x"
                                  ]
                              )
                              (BackendVarWithIdentity intTy (rightFieldIdentity) "x")
                              :| []
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    leftFieldIdentity = localIdentity 2069124 "x"
    rightFieldIdentity = localIdentity 2069125 "x"

casePatternMismatchedFieldTypeProgram :: BackendProgram
casePatternMismatchedFieldTypeProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          intTy
                          (outerIdentity)
                          "x"
                          unaryIntTy
                          intIdentityExpr
                          ( BackendCase
                              intTy
                              (optionSome intTy [intLit 41])
                              ( BackendAlternative
                                  ( BackendConstructorPatternWithBinderIdentities
                                      (optionSomeIdentity)
                                      "Some"
                                      [BackendPatternBinder (fieldIdentity) "x"]
                                  )
                                  ( BackendApp
                                      intTy
                                      (BackendVarWithIdentity unaryIntTy (outerIdentity) "x")
                                      (BackendVarWithIdentity intTy (fieldIdentity) "x")
                                  )
                                  :| []
                              )
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    outerIdentity = localIdentity 2069121 "x"
    fieldIdentity = localIdentity 2069122 "x"

closureParamIdentityStaleNameProgram :: BackendProgram
closureParamIdentityStaleNameProgram =
  closureParamIdentityStaleNameProgramWithEntry "__mlfp_closure$stale_param_identity"

returnedPartialClosureSlotIdentityProgram :: BackendProgram
returnedPartialClosureSlotIdentityProgram =
  closureParamIdentityStaleNameProgramWithEntry "__mlfp_returned_partial$stale_param_identity"

returnedPartialPrefixSourceClosureProgram :: BackendProgram
returnedPartialPrefixSourceClosureProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = functionSymbol,
                      backendBindingNameWithMetadata = "f",
                      backendBindingTypeWithMetadata = unaryIntTy,
                      backendBindingExprWithMetadata =
                        BackendLamWithIdentity
                          unaryIntTy
                          (ignoredIdentity)
                          "ignored"
                          intTy
                          (intLit 41),
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendClosureCall
                          intTy
                          ( BackendClosureWithParamIdentities
                              { backendExprType = unaryIntTy,
                                backendClosureEntryIdentity = (UniqueIdentity 2069109),
                                backendClosureEntryName = "__mlfp_returned_partial$source_closure",
                                backendClosureCaptures =
                                  [ BackendClosureCapture
                                      (captureIdentity)
                                      "f"
                                      unaryIntTy
                                      (BackendVarWithIdentity unaryIntTy ((TopLevelId functionSymbol)) "f")
                                  ],
                                backendClosureParamsWithIdentities =
                                  [BackendClosureParam (paramIdentity) "x" intTy],
                                backendClosureBody =
                                  BackendVarWithIdentity intTy (paramIdentity) "x"
                              }
                          )
                          [intLit 99],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    functionSymbol = symbolIdentityFromParts (UniqueIdentity 2069110) SymbolValue "Main" "f" Nothing
    captureIdentity = localIdentity 2069113 "f"
    ignoredIdentity = localIdentity 2069111 "ignored"
    paramIdentity = localIdentity 2069112 "x"

closureParamIdentityStaleNameProgramWithEntry :: String -> BackendProgram
closureParamIdentityStaleNameProgramWithEntry entryName =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          intTy
                          (outerIdentity)
                          "x"
                          intTy
                          (intLit 41)
                          ( BackendClosureCall
                              intTy
                              ( BackendClosureWithParamIdentities
                                  { backendExprType = unaryIntTy,
                                    backendClosureEntryIdentity = UniqueIdentity (-992001),
                                    backendClosureEntryName = entryName,
                                    backendClosureCaptures =
                                      [ BackendClosureCapture
                                          (outerIdentity)
                                          "x"
                                          intTy
                                          (BackendVarWithIdentity intTy (outerIdentity) "x")
                                      ],
                                    backendClosureParamsWithIdentities =
                                      [BackendClosureParam (paramIdentity) "$stale_x" intTy],
                                    backendClosureBody =
                                      BackendVarWithIdentity intTy (paramIdentity) "x"
                                  }
                              )
                              [intLit 99]
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    outerIdentity = localIdentity 2069107 "x"
    paramIdentity = localIdentity 2069108 "$stale_x"

closureParamIdentityDuplicateDisplayProgram :: BackendProgram
closureParamIdentityDuplicateDisplayProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendClosureCall
                          intTy
                          ( BackendClosureWithParamIdentities
                              { backendExprType = binaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992002),
              backendClosureEntryName = "__mlfp_closure$duplicate_param_display",
                                backendClosureCaptures = [],
                                backendClosureParamsWithIdentities =
                                  [ BackendClosureParam (leftParamIdentity) "x" intTy,
                                    BackendClosureParam (rightParamIdentity) "x" intTy
                                  ],
                                backendClosureBody =
                                  BackendVarWithIdentity intTy (rightParamIdentity) "x"
                              }
                          )
                          [intLit 41, intLit 99],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    leftParamIdentity = localIdentity 2069122 "x"
    rightParamIdentity = localIdentity 2069123 "x"

duplicateIdentityClosureParamProgram :: BackendProgram
duplicateIdentityClosureParamProgram =
  singleBindingProgram "main" $
    BackendClosureCall
      intTy
      ( BackendClosureWithParamIdentities
          { backendExprType = binaryIntTy,
            backendClosureEntryIdentity = (UniqueIdentity 2069137),
            backendClosureEntryName = "__mlfp_closure$duplicate_identity_param",
            backendClosureCaptures = [],
            backendClosureParamsWithIdentities =
              [ BackendClosureParam (fixtureLocalDetails "x") "x" intTy,
                BackendClosureParam (fixtureLocalDetails "x") "x" intTy
              ],
            backendClosureBody = intLit 0
          }
      )
      [intLit 1, intLit 2]

ambiguousIdentityClosureParamUseProgram :: BackendProgram
ambiguousIdentityClosureParamUseProgram =
  programWithMainExpr intTy $
    BackendClosureCall
      intTy
      ( BackendClosureWithParamIdentities
          { backendExprType = binaryIntTy,
            backendClosureEntryIdentity = UniqueIdentity (-992003),
            backendClosureEntryName = "__mlfp_closure$ambiguous_identity_param",
            backendClosureCaptures = [],
            backendClosureParamsWithIdentities =
              [ BackendClosureParam (leftParamIdentity) "x" intTy,
                BackendClosureParam (rightParamIdentity) "x" intTy
              ],
            backendClosureBody = BackendVar intTy "x"
          }
      )
      [intLit 1, intLit 2]
  where
    leftParamIdentity = localIdentity 2069124 "x"
    rightParamIdentity = localIdentity 2069125 "x"

closureCaptureValueKindIdentityProgram :: BackendProgram
closureCaptureValueKindIdentityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "idRaw",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr = intIdentityExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "makeCaller",
                      backendBindingType = BTArrow unaryIntTy unaryIntTy,
                      backendBindingExpr =
                        BackendLamWithIdentity
                          (BTArrow unaryIntTy unaryIntTy)
                          (functionParamIdentity)
                          "$stale_f"
                          unaryIntTy
                          ( BackendLetWithIdentity
                              unaryIntTy
                              (shadowClosureIdentity)
                              "f"
                              unaryIntTy
                              shadowClosure
                              capturedParamClosure
                          ),
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendApp
                              unaryIntTy
                              (fixtureTopLevelVar (BTArrow unaryIntTy unaryIntTy) "makeCaller")
                              (fixtureTopLevelVar unaryIntTy "idRaw")
                          )
                          (intLit 99),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    functionParamIdentity = localIdentity 2069109 "$stale_f"
    shadowClosureIdentity = localIdentity 2069110 "f"
    argumentIdentity = localIdentity 2069111 "x"
    shadowClosure =
      BackendClosureWithParamIdentities
        { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992004),
              backendClosureEntryName = "__mlfp_closure$shadow_f",
          backendClosureCaptures = [],
          backendClosureParamsWithIdentities = [BackendClosureParam (argumentIdentity) "x" intTy],
          backendClosureBody = intLit 0
        }
    capturedParamClosure =
      BackendClosureWithParamIdentities
        { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992005),
              backendClosureEntryName = "__mlfp_closure$capture_stale_f",
          backendClosureCaptures =
            [ BackendClosureCapture
                (functionParamIdentity)
                "f"
                unaryIntTy
                (BackendVarWithIdentity unaryIntTy (functionParamIdentity) "f")
            ],
          backendClosureParamsWithIdentities = [BackendClosureParam (argumentIdentity) "x" intTy],
          backendClosureBody =
            BackendApp
              intTy
              (BackendVarWithIdentity unaryIntTy (functionParamIdentity) "f")
              (BackendVarWithIdentity intTy (argumentIdentity) "x")
        }

localIdentity :: Int -> String -> IdDetails
localIdentity unique name =
  LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity unique)) name)

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
          backendBindingExpr = fixtureTopLevelVar unaryIntTy "id",
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr = BackendApp intTy (fixtureTopLevelVar unaryIntTy "g") (intLit 7),
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
              (BackendApp unaryIntTy (fixtureTopLevelVar binaryIntTy "shadow") (intLit 1))
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
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = caseHeadedIdIdentity,
                      backendBindingNameWithMetadata = "id",
                      backendBindingTypeWithMetadata = unaryIntTy,
                      backendBindingExprWithMetadata = intIdentityExpr,
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBindingWithMetadata
                    { backendBindingIdentity = caseHeadedFallbackIdentity,
                      backendBindingNameWithMetadata = "fallback",
                      backendBindingTypeWithMetadata = unaryIntTy,
                      backendBindingExprWithMetadata =
                        BackendLam unaryIntTy "x" intTy (intLit 9),
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendCase
                              unaryIntTy
                              (optionSome intTy [intLit 0])
                              ( BackendAlternative
                                  (optionSomePattern [BackendPatternBinder (caseHeadedValueIdentity) "value"])
                                  (BackendVarWithIdentity unaryIntTy ((TopLevelId caseHeadedIdIdentity)) "id")
                                  :| [ BackendAlternative
                                         optionNonePattern
                                         (BackendVarWithIdentity unaryIntTy ((TopLevelId caseHeadedFallbackIdentity)) "fallback")
                                     ]
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
  where
    caseHeadedIdIdentity = symbolIdentityFromParts (UniqueIdentity 992010) SymbolValue "Main" "id" Nothing
    caseHeadedFallbackIdentity = symbolIdentityFromParts (UniqueIdentity 992011) SymbolValue "Main" "fallback" Nothing
    caseHeadedValueIdentity = localIdentity 992012 "value"

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
          backendBindingExpr = BackendApp intTy (fixtureTopLevelVar unaryIntTy "__mlfp_and") (intLit 7),
          backendBindingExportedAsMain = True
        }
    ]

mismatchedRuntimePrimitiveProgram :: BackendProgram
mismatchedRuntimePrimitiveProgram =
  programWithMainExpr boolTy $
    BackendApp
      boolTy
      (BackendApp unaryBoolTy (BackendVar (BTArrow boolTy unaryBoolTy) "__mlfp_and") (boolLit True))
      (boolLit False)

staleNamedRuntimePrimitiveProgram :: BackendProgram
staleNamedRuntimePrimitiveProgram =
  programWithMainExpr boolTy $
    BackendApp
      boolTy
      (BackendApp unaryBoolTy andVar (boolLit True))
      (boolLit False)
  where
    andVar =
      BackendVarWithIdentity
        (BTArrow boolTy unaryBoolTy)
        (PrimitiveId (primitiveRefFromSymbol staleAndIdentity))
        "$stale_and"
    staleAndIdentity =
      renameSymbolDefiningName "$stale_and" (PrimitiveInventory.builtinValueIdentity PrimitiveInventory.nativeAndPrimitiveName)

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
          backendBindingExpr = BackendApp intTy (fixtureTopLevelVar unaryIntTy "malloc") (intLit 7),
          backendBindingExportedAsMain = True
        }
    ]

userNamedMallocIdentityProgram :: BackendProgram
userNamedMallocIdentityProgram =
  programWithBindings
    [ BackendBindingWithMetadata
        { backendBindingIdentity = userMallocIdentity,
          backendBindingNameWithMetadata = "malloc",
          backendBindingTypeWithMetadata = unaryIntTy,
          backendBindingExprWithMetadata = intIdentityExpr,
          backendBindingExportedAsMainWithMetadata = False,
          backendBindingEvidenceParamIndices = Set.empty
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (BackendVarWithIdentity unaryIntTy ((TopLevelId userMallocIdentity)) "$stale_malloc")
              (intLit 7),
          backendBindingExportedAsMain = True
        }
    ]

userMallocIdentity :: SymbolIdentity
userMallocIdentity =
  symbolIdentityFromParts (UniqueIdentity 991736) SymbolValue "Main" "malloc" Nothing

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
                      backendBindingExpr = optionNone intTy,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

mallocIdentityCollisionConstructorProgram :: BackendProgram
mallocIdentityCollisionConstructorProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = userMallocIdentity,
                      backendBindingNameWithMetadata = "malloc",
                      backendBindingTypeWithMetadata = unaryIntTy,
                      backendBindingExprWithMetadata = intIdentityExpr,
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = optionTy intTy,
                      backendBindingExpr = optionNone intTy,
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
                        BackendLamWithIdentity
                          (BTArrow lazyFieldBoxTy intTy)
                          (boxIdentity)
                          "box"
                          lazyFieldBoxTy
                          ( BackendCase
                              intTy
                              (BackendVarWithIdentity lazyFieldBoxTy (boxIdentity) "box")
                              ( BackendAlternative
                                  ( BackendConstructorPatternWithBinderIdentities
                                      (lazyFieldBoxPackedIdentity)
                                      "Packed"
                                      [ BackendPatternBinder (unusedIdentity) "unused",
                                        BackendPatternBinder (valueIdentity) "value"
                                      ]
                                  )
                                  (BackendVarWithIdentity intTy (valueIdentity) "value")
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
  where
    boxIdentity = localIdentity 992029 "box"
    unusedIdentity = localIdentity 992030 "unused"
    valueIdentity = localIdentity 992031 "value"

strictImmediateConstructFieldProgram :: BackendProgram
strictImmediateConstructFieldProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [strictBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          ( BackendConstructWithIdentity
                              strictBoxTy
                              (strictBoxConstructorIdentity)
                              "StrictBox"
                              [polyIdExpr, BackendRoll recIntTy (intLit 1)]
                          )
                          ( BackendAlternative
                              ( BackendConstructorPatternWithBinderIdentities
                                  (strictBoxConstructorIdentity)
                                  "StrictBox"
                                  ([BackendPatternBinder (fixtureLocalDetails name) name | name <- ["poly", "unused"]])
                              )
                              (intLit 0)
                              :| []
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

strictImmediateDefaultProgram :: BackendProgram
strictImmediateDefaultProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [strictBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          ( BackendConstructWithIdentity
                              strictBoxTy
                              (strictBoxConstructorIdentity)
                              "StrictBox"
                              [polyIdExpr, BackendRoll recIntTy (intLit 1)]
                          )
                          (BackendAlternative BackendDefaultPattern (intLit 0) :| []),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

unmatchedImmediateConstructorProgram :: BackendProgram
unmatchedImmediateConstructorProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [immediateChoiceData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          ( BackendConstructWithIdentity
                              immediateChoiceTy
                              (immediateChoiceConstructorIdentity)
                              "WithStatic"
                              [polyIdExpr, intLit 1]
                          )
                          ( BackendAlternative
                              (BackendConstructorPatternWithBinderIdentities (immediateChoiceOtherIdentity) "Other" [])
                              (intLit 0)
                              :| []
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

mismatchedImmediateConstructorIdentityCaseProgram :: BackendProgram
mismatchedImmediateConstructorIdentityCaseProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [immediateChoiceDataWithConstructorIdentity],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          ( BackendConstructWithIdentity
                              immediateChoiceTy
                              (immediateChoiceConstructorIdentity)
                              "WithStatic"
                              [polyIdExpr, intLit 1]
                          )
                          ( BackendAlternative
                              ( BackendConstructorPatternWithBinderIdentities
                                  (otherImmediateChoiceConstructorIdentity)
                                  "WithStatic"
                                  ([BackendPatternBinder (fixtureLocalDetails name) name | name <- ["poly", "value"]])
                              )
                              (intLit 7)
                              :| []
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

matchingConstructorUseIdentityProgram :: BackendProgram
matchingConstructorUseIdentityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [immediateChoiceDataWithConstructorIdentity],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = immediateChoiceTy,
                        backendBindingExpr =
                          BackendConstructWithIdentity
                            immediateChoiceTy
                            (immediateChoiceConstructorIdentity)
                            "WithStatic"
                            [polyIdExpr, intLit 1],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

mismatchedConstructorUseIdentityProgram :: BackendProgram
mismatchedConstructorUseIdentityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [immediateChoiceDataWithConstructorIdentity],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          intTy
                          (matchingConstructorIgnoredIdentity)
                          "ignored"
                          immediateChoiceTy
                          ( BackendConstructWithIdentity
                              immediateChoiceTy
                              (otherImmediateChoiceConstructorIdentity)
                              "WithStatic"
                              [polyIdExpr, intLit 1]
                          )
                          (intLit 0),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }
  where
    matchingConstructorIgnoredIdentity = localIdentity 992034 "ignored"

displayAliasConstructorUseIdentityProgram :: BackendProgram
displayAliasConstructorUseIdentityProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [immediateChoiceDataWithStaleConstructorDisplayIdentity],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          intTy
                          (displayAliasConstructorIgnoredIdentity)
                          "ignored"
                          immediateChoiceTy
                          ( BackendConstructWithIdentity
                              immediateChoiceTy
                              (immediateChoiceConstructorIdentity)
                              "$stale_WithStatic"
                              [polyIdExpr, intLit 1]
                          )
                          (intLit 0),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }
  where
    displayAliasConstructorIgnoredIdentity = localIdentity 992035 "ignored"

strictImmediateUnmatchedProgram :: BackendProgram
strictImmediateUnmatchedProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [immediateStrictChoiceData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          ( BackendConstructWithIdentity
                              immediateStrictChoiceTy
                              (immediateStrictWithStaticIdentity)
                              "WithStrictStatic"
                              [polyIdExpr, BackendRoll recIntTy (intLit 1)]
                          )
                          ( BackendAlternative
                              (BackendConstructorPatternWithBinderIdentities (immediateStrictOtherIdentity) "StrictOther" [])
                              (intLit 0)
                              :| []
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
                          (optionNone intTy)
                          ( BackendAlternative optionNonePattern (intLit 0)
                              :| [BackendAlternative optionNonePattern (intLit 1)]
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

duplicateImmediateConstructorIdentityCaseProgram :: BackendProgram
duplicateImmediateConstructorIdentityCaseProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [strictBoxDataWithConstructorIdentity],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          ( BackendConstructWithIdentity
                              strictBoxTy
                              (strictBoxConstructorIdentity)
                              "$stale_StrictBox"
                              [polyIdExpr, BackendRoll recIntTy (intLit 1)]
                          )
                          ( BackendAlternative
                              ( BackendConstructorPatternWithBinderIdentities
                                  (strictBoxConstructorIdentity)
                                  "$stale_StrictBox"
                                  ([BackendPatternBinder (fixtureLocalDetails name) name | name <- ["poly", "unused"]])
                              )
                              (intLit 0)
                              :| [ BackendAlternative
                                    ( BackendConstructorPatternWithBinderIdentities
                                        (strictBoxConstructorIdentity)
                                        "$stale_StrictBox_again"
                                        ([BackendPatternBinder (fixtureLocalDetails name) name | name <- ["poly2", "unused2"]])
                                    )
                                    (intLit 1)
                                 ]
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
                          (optionSome intTy [intLit 7])
                          ( BackendAlternative BackendDefaultPattern (intLit 0)
                              :| [ BackendAlternative
                                     (optionSomePattern [BackendPatternBinder (nonTailValueIdentity) "value"])
                                     (BackendVarWithIdentity intTy (nonTailValueIdentity) "value")
                                 ]
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }
  where
    nonTailValueIdentity = localIdentity 992013 "value"

optionData :: BackendData
optionData =
  BackendDataWithIdentity
    { backendDataIdentity = optionIdentity,
      backendDataNameWithIdentity = "Option",
      backendDataParameterRefsWithIdentity = [backendDataParameterRefFromIdentity optionTypeParameterIdentity "a"],
      backendDataConstructorsWithIdentity =
        [ BackendConstructorWithIdentity
            { backendConstructorIdentity = optionNoneIdentity,
              backendConstructorNameWithIdentity = "None",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [],
              backendConstructorResultWithIdentity = optionTy optionTypeParameterTy
            },
          BackendConstructorWithIdentity
            { backendConstructorIdentity = optionSomeIdentity,
              backendConstructorNameWithIdentity = "Some",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [optionTypeParameterTy],
              backendConstructorResultWithIdentity = optionTy optionTypeParameterTy
            }
        ]
    }

optionIdentity :: SymbolIdentity
optionIdentity =
  symbolIdentityFromParts (UniqueIdentity 992000) SymbolType "Main" "Option" Nothing

optionNoneIdentity :: SymbolIdentity
optionNoneIdentity =
  symbolIdentityFromParts (UniqueIdentity 992001) SymbolConstructor "Main" "None" (Just (SymbolOwnerType optionIdentity))

optionSomeIdentity :: SymbolIdentity
optionSomeIdentity =
  symbolIdentityFromParts (UniqueIdentity 992002) SymbolConstructor "Main" "Some" (Just (SymbolOwnerType optionIdentity))

optionTypeParameterIdentity :: TypeBinderIdentity
optionTypeParameterIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 992003)

optionTypeParameterTy :: BackendType
optionTypeParameterTy =
  BTVarWithIdentity (optionTypeParameterIdentity) "a"

intPairData :: BackendData
intPairData =
  BackendData
    { backendDataName = "IntPair",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "IntPair" [] [intTy, intTy] intPairTy]
    }

intPairTy :: BackendType
intPairTy =
  BTBase (BaseTy "IntPair")

resultBoxData :: BackendData
resultBoxData =
  BackendData
    { backendDataName = "ResultBox",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "ResultBox" [] [intTy] resultBoxTy]
    }

singleFieldBoxTy :: String -> BackendType
singleFieldBoxTy name =
  BTBase (BaseTy name)

singleFieldBoxData :: String -> BackendType -> BackendData
singleFieldBoxData name fieldTy =
  BackendData
    { backendDataName = name,
      backendDataParameters = [],
      backendDataConstructors =
        [BackendConstructor ("Mk" ++ name) [] [fieldTy] (singleFieldBoxTy name)]
    }

singleFieldBoxExpr :: String -> BackendExpr -> BackendExpr
singleFieldBoxExpr name field =
  BackendConstruct (singleFieldBoxTy name) ("Mk" ++ name) [field]

nonePolyTy :: BackendType
nonePolyTy =
  BTForallWithIdentity (nonePolyTypeParameterIdentity) "a" Nothing (optionTy nonePolyTypeParameterTy)

nonePolyExpr :: BackendExpr
nonePolyExpr =
  BackendTyAbsWithIdentity
    nonePolyTy
    (nonePolyTypeParameterIdentity)
    "a"
    Nothing
    (optionNone nonePolyTypeParameterTy)

noneViaDirectTyAbsExpr :: BackendExpr
noneViaDirectTyAbsExpr =
  BackendTyAbsWithIdentity
    nonePolyTy
    (nonePolyTypeParameterIdentity)
    "a"
    Nothing
    ( BackendTyApp
        (optionTy nonePolyTypeParameterTy)
        (fixtureTopLevelVar nonePolyTy "none")
        nonePolyTypeParameterTy
    )

polyIdTy :: BackendType
polyIdTy =
  BTForallWithIdentity (polyIdTypeParameterIdentity) "a" Nothing (BTArrow polyIdTypeParameterTy polyIdTypeParameterTy)

polyIdExpr :: BackendExpr
polyIdExpr =
  BackendTyAbsWithIdentity
    polyIdTy
    (polyIdTypeParameterIdentity)
    "a"
    Nothing
    ( BackendLamWithIdentity
        (BTArrow polyIdTypeParameterTy polyIdTypeParameterTy)
        (polyIdParamIdentity)
        "x"
        polyIdTypeParameterTy
        (BackendVarWithIdentity polyIdTypeParameterTy (polyIdParamIdentity) "x")
    )

quantElimFunctionProgram :: BackendProgram
quantElimFunctionProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "specialized",
          backendBindingType = unaryIntTy,
          backendBindingExpr =
            BackendTyApp
              unaryIntTy
              polyIdExpr
              intTy,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (fixtureTopLevelVar unaryIntTy "specialized")
              (intLit 42),
          backendBindingExportedAsMain = True
        }
    ]

nonePolyTypeParameterIdentity :: TypeBinderIdentity
nonePolyTypeParameterIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 992004)

nonePolyTypeParameterTy :: BackendType
nonePolyTypeParameterTy =
  BTVarWithIdentity (nonePolyTypeParameterIdentity) "a"

polyIdTypeParameterIdentity :: TypeBinderIdentity
polyIdTypeParameterIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 992005)

polyIdTypeParameterTy :: BackendType
polyIdTypeParameterTy =
  BTVarWithIdentity (polyIdTypeParameterIdentity) "a"

polyIdParamIdentity :: IdDetails
polyIdParamIdentity =
  localIdentity 992006 "x"

staticPolyBoolTy :: BackendType
staticPolyBoolTy =
  BTArrow polyIdTy boolTy

badPolyTy :: BackendType
badPolyTy =
  BTForallWithIdentity (badPolyTypeParameterIdentity) "a" Nothing fnBoxTy

badPolyExpr :: BackendExpr
badPolyExpr =
  BackendTyAbsWithIdentity
    badPolyTy
    (badPolyTypeParameterIdentity)
    "a"
    Nothing
    ( fnBoxConstruct
        [fixtureTopLevelVar unaryIntTy "helper"]
    )

badPolyTypeParameterIdentity :: TypeBinderIdentity
badPolyTypeParameterIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 992007)

polyIdCall :: BackendType -> BackendExpr -> BackendExpr
polyIdCall ty arg =
  BackendApp
    ty
    (BackendTyApp (BTArrow ty ty) (fixtureTopLevelVar polyIdTy "poly") ty)
    arg

unspecializedPolymorphicBindingProgram :: BackendProgram
unspecializedPolymorphicBindingProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData, singleFieldBoxData "UnspecializedPolyBox" nonePolyTy],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "none",
                      backendBindingType = nonePolyTy,
                      backendBindingExpr = nonePolyExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = singleFieldBoxTy "UnspecializedPolyBox",
                      backendBindingExpr =
                        singleFieldBoxExpr "UnspecializedPolyBox" (fixtureTopLevelVar nonePolyTy "none"),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

escapingTypeAbstractionProgram :: BackendProgram
escapingTypeAbstractionProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData, singleFieldBoxData "EscapingTyAbsBox" nonePolyTy],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = singleFieldBoxTy "EscapingTyAbsBox",
                      backendBindingExpr =
                        singleFieldBoxExpr "EscapingTyAbsBox" nonePolyExpr,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

escapingPolymorphicBindingProgram :: BackendProgram
escapingPolymorphicBindingProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [singleFieldBoxData "EscapingPolyBindingBox" polyIdTy],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "poly",
                      backendBindingType = polyIdTy,
                      backendBindingExpr = polyIdExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = singleFieldBoxTy "EscapingPolyBindingBox",
                      backendBindingExpr =
                        singleFieldBoxExpr "EscapingPolyBindingBox" (fixtureTopLevelVar polyIdTy "poly"),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

partialTypeApplicationProgram :: BackendProgram
partialTypeApplicationProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [singleFieldBoxData "PartialTypeAppBox" partialPolyAfterIntTy],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "polyPartial",
                      backendBindingType = partialPolyTy,
                      backendBindingExpr = partialPolyExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = singleFieldBoxTy "PartialTypeAppBox",
                      backendBindingExpr =
                        singleFieldBoxExpr
                          "PartialTypeAppBox"
                          (BackendTyApp partialPolyAfterIntTy (fixtureTopLevelVar partialPolyTy "polyPartial") intTy),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

partialPolyTy :: BackendType
partialPolyTy =
  BTForall "a" Nothing (BTForall "b" Nothing (BTArrow (BTVar "a") (BTVar "a")))

partialPolyAfterIntTy :: BackendType
partialPolyAfterIntTy =
  BTForall "b" Nothing unaryIntTy

partialPolyExpr :: BackendExpr
partialPolyExpr =
  BackendTyAbs
    partialPolyTy
    "a"
    Nothing
    ( BackendTyAbs
        partialPolyAfterATy
        "b"
        Nothing
        ( BackendLam
            (BTArrow (BTVar "a") (BTVar "a"))
            "x"
            (BTVar "a")
            (BackendVar (BTVar "a") "x")
        )
    )

partialPolyAfterATy :: BackendType
partialPolyAfterATy =
  BTForall "b" Nothing (BTArrow (BTVar "a") (BTVar "a"))

partialApplicationProgram :: BackendProgram
partialApplicationProgram =
  programWithBindings
    [ addBinding,
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLetWithIdentity
              { backendLetIdentity = fixtureLocalDetails "addOne", backendExprType = intTy,
                backendLetName = "addOne",
                backendLetType = unaryIntTy,
                backendLetRhs =
                  BackendClosureWithParamIdentities
                    { backendExprType = unaryIntTy,
                      backendClosureEntryIdentity = fixtureClosureEntryIdentity "__mlfp_closure$addOne",
              backendClosureEntryName = "__mlfp_closure$addOne",
                      backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "__mlfp_partial_capture0") "__mlfp_partial_capture0" intTy (intLit 1)],
                      backendClosureParamsWithIdentities = backendClosureParams [("__mlfp_partial_arg0", intTy)],
                      backendClosureBody =
                        BackendApp
                          { backendExprType = intTy,
                            backendFunction =
                              BackendApp
                                { backendExprType = unaryIntTy,
                                  backendFunction = fixtureTopLevelVar binaryIntTy "add",
                                  backendArgument = BackendVar intTy "__mlfp_partial_capture0"
                                },
                            backendArgument = BackendVar intTy "__mlfp_partial_arg0"
                          }
                    },
                backendLetBody =
                  BackendClosureCall
                    { backendExprType = intTy,
                      backendClosureFunction = BackendVar unaryIntTy "addOne",
                      backendClosureArguments = [intLit 2]
                    }
              },
          backendBindingExportedAsMain = True
        }
    ]

staticPartialApplicationArgumentProgram :: BackendProgram
staticPartialApplicationArgumentProgram =
  programWithBindings
    [ addBinding,
      BackendBinding
        { backendBindingName = "use",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLamWithIdentity
              { backendParamIdentity = fixtureLocalDetails "f", backendExprType = BTArrow unaryIntTy intTy,
                backendParamName = "f",
                backendParamType = unaryIntTy,
                backendBody = intLit 0
              },
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              { backendExprType = intTy,
                backendFunction = fixtureTopLevelVar (BTArrow unaryIntTy intTy) "use",
                backendArgument =
                  BackendApp
                    { backendExprType = unaryIntTy,
                      backendFunction = fixtureTopLevelVar binaryIntTy "add",
                      backendArgument = intLit 1
                    }
              },
          backendBindingExportedAsMain = True
        }
    ]

escapingLambdaProgram :: BackendProgram
escapingLambdaProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = BTArrow intTy fnBoxTy,
                      backendBindingExpr =
                        BackendLam (BTArrow intTy fnBoxTy) "captured" intTy $
                          BackendLet fnBoxTy "f" unaryIntTy
                            ( BackendLam unaryIntTy "x" intTy
                                (BackendVar intTy "captured")
                            )
                            (fnBoxConstruct [BackendVar unaryIntTy "f"]),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

sourceCapturedClosureCallProgram :: String
sourceCapturedClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    let g : Int -> Int = f in",
      "    g 0;",
      "}"
    ]

sourceFunctionParameterClosureCallProgram :: String
sourceFunctionParameterClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int) f 1;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    use f;",
      "}"
    ]

sourceFunctionParameterClosureAliasCallProgram :: String
sourceFunctionParameterClosureAliasCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int) let g : Int -> Int = f in g 1;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    use f;",
      "}"
    ]

sourceReturnedClosureProgram :: String
sourceReturnedClosureProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int -> Int =",
      "    let captured : Int = 41 in λ(x : Int) captured;",
      "}"
    ]

sourceTopLevelClosureCallProgram :: String
sourceTopLevelClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def maker : Int -> Int = let captured : Int = 41 in λ(x : Int) captured;",
      "  def main : Int = maker 0;",
      "}"
    ]

sourceLocalReturnedClosureCallProgram :: String
sourceLocalReturnedClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    let make : Int -> (Int -> Int) =",
      "      λ(base : Int) let captured : Int = base in λ(x : Int) captured in",
      "    (make 41) 0;",
      "}"
    ]

sourceLetBoundReturnedClosureRecordCallProgram :: String
sourceLetBoundReturnedClosureRecordCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    let f : Int -> Int =",
      "      ((λ(base : Int) let captured : Int = base in λ(x : Int) captured) 41) in",
      "    f 0;",
      "}"
    ]

sourceDirectReturnedClosureApplicationProgram :: String
sourceDirectReturnedClosureApplicationProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    (λ(base : Int) let captured : Int = base in λ(x : Int) captured) 41 0;",
      "}"
    ]

sourceTopLevelPartialApplicationProgram :: String
sourceTopLevelPartialApplicationProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def main : Int = apply (keepLeft 1);",
      "}"
    ]

sourceLocalPartialApplicationProgram :: String
sourceLocalPartialApplicationProgram =
  unlines
    [ "module Main export (main) {",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def main : Int =",
      "    let keepLeft : Int -> Int -> Int = λx λy x",
      "    in apply (keepLeft 1);",
      "}"
    ]

sourcePartialApplicationClosureArgumentProgram :: String
sourcePartialApplicationClosureArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def choose : (Int -> Int) -> Int -> Int -> Int = λf λignored λx f x;",
      "  def apply : (Int -> Int) -> Int = λf f 4;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let inc : Int -> Int = λ(x : Int) captured in",
      "    apply (choose inc 0);",
      "}"
    ]

sourcePartialApplicationGlobalClosureArgumentProgram :: String
sourcePartialApplicationGlobalClosureArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def choose : (Int -> Int) -> Int -> Int -> Int = λf λignored λx f x;",
      "  def apply : (Int -> Int) -> Int = λf f 4;",
      "  def globalInc : Int -> Int =",
      "    let captured : Int = 41 in",
      "    λ(x : Int) captured;",
      "  def main : Int = apply (choose globalInc 0);",
      "}"
    ]

sourcePartialApplicationClosureParameterProgram :: String
sourcePartialApplicationClosureParameterProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def use : (Int -> Int -> Int) -> Int = λf apply (f 1);",
      "  def main : Int = use keepLeft;",
      "}"
    ]

sourcePartialApplicationClosureDemandAliasProgram :: String
sourcePartialApplicationClosureDemandAliasProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def use : (Int -> Int -> Int) -> Int = λf apply (f 1);",
      "  def useAlias : (Int -> Int -> Int) -> Int = use;",
      "  def main : Int = useAlias keepLeft;",
      "}"
    ]

sourcePartialApplicationClosureDemandWrappedAliasProgram :: String
sourcePartialApplicationClosureDemandWrappedAliasProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def use : (Int -> Int -> Int) -> Int = λf apply (f 1);",
      "  def useWrapped : (Int -> Int -> Int) -> Int =",
      "    let f : (Int -> Int -> Int) -> Int = use in f;",
      "  def main : Int = useWrapped keepLeft;",
      "}"
    ]

sourcePartialApplicationClosureDemandEtaAliasProgram :: String
sourcePartialApplicationClosureDemandEtaAliasProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def use : (Int -> Int -> Int) -> Int = λf apply (f 1);",
      "  def useAfter : Int -> (Int -> Int -> Int) -> Int = λn use;",
      "  def main : Int = useAfter 0 keepLeft;",
      "}"
    ]

sourcePartialApplicationLocalClosureDemandProgram :: String
sourcePartialApplicationLocalClosureDemandProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def main : Int =",
      "    let use : (Int -> Int -> Int) -> Int = λf apply (f 1)",
      "    in use keepLeft;",
      "}"
    ]

sourceClosureDemandAfterEvidenceProgram :: String
sourceClosureDemandAfterEvidenceProgram =
  unlines
    [ "module Main export (Marker, main) {",
      "  class Marker a {",
      "  }",
      "  instance Marker Bool {",
      "  }",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def use : Marker Bool => (Int -> Int -> Int) -> Int = λf apply (f 1);",
      "  def main : Int = use keepLeft;",
      "}"
    ]

sourceConstrainedPartialApplicationProgram :: String
sourceConstrainedPartialApplicationProgram =
  unlines
    [ "module Main export (Pick, main) {",
      "  class Pick a {",
      "    pick : a -> a -> a;",
      "  }",
      "  instance Pick Int {",
      "    pick = λx λy x;",
      "  }",
      "  def keep : Pick Int => Int -> Int -> Int = λx λy pick x y;",
      "  def apply : (Int -> Int) -> Int = λf f 1;",
      "  def main : Int = apply (keep 1);",
      "}"
    ]

sourceConstrainedPartialApplicationAliasProgram :: String
sourceConstrainedPartialApplicationAliasProgram =
  unlines
    [ "module Main export (Pick, main) {",
      "  class Pick a {",
      "    pick : a -> a -> a;",
      "  }",
      "  instance Pick Int {",
      "    pick = λx λy x;",
      "  }",
      "  def keep : Pick Int => Int -> Int -> Int = λx λy pick x y;",
      "  def keepAlias : Pick Int => Int -> Int -> Int = keep;",
      "  def apply : (Int -> Int) -> Int = λf f 1;",
      "  def main : Int = apply (keepAlias 1);",
      "}"
    ]

sourcePartialApplicationGeneratedNameCollisionProgram :: String
sourcePartialApplicationGeneratedNameCollisionProgram =
  unlines
    [ "module Main export (main) {",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def main : Int =",
      "    let __mlfp_partial_capture0 : Int -> Int -> Int = λx λy x in",
      "    apply (__mlfp_partial_capture0 1);",
      "}"
    ]

sourcePartialApplicationDirectFunctionArgumentProgram :: String
sourcePartialApplicationDirectFunctionArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def choose : (Int -> Int -> Int) -> Int -> Int -> Int = λf λignored λx f x ignored;",
      "  def apply : (Int -> Int) -> Int = λf f 4;",
      "  def main : Int = apply (choose keepLeft 0);",
      "}"
    ]

sourceClosureDemandLetHeadedCallProgram :: String
sourceClosureDemandLetHeadedCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def use : (Int -> Int -> Int) -> Int = λf apply (f 1);",
      "  def main : Int =",
      "    (let f : (Int -> Int -> Int) -> Int = use in f) keepLeft;",
      "}"
    ]

sourceClosureDemandEtaCallHeadProgram :: String
sourceClosureDemandEtaCallHeadProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def use : (Int -> Int -> Int) -> Int = λf apply (f 1);",
      "  def main : Int = (λ(u : Int -> Int -> Int) use u) keepLeft;",
      "}"
    ]

sourcePartialApplicationPolymorphicArgumentProgram :: String
sourcePartialApplicationPolymorphicArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : ∀ a. a -> a = λx x;",
      "  def usePoly : (∀ a. a -> a) -> Int -> Bool =",
      "    λ(poly : ∀ a. a -> a) λignored let keepInt : Int = poly 1 in poly true;",
      "  def apply : (Int -> Bool) -> Bool = λf f 0;",
      "  def main : Bool = apply (usePoly id);",
      "}"
    ]

sourcePartialApplicationHigherRankFunctionArgumentProgram :: String
sourcePartialApplicationHigherRankFunctionArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : ∀ a. a -> a = λx x;",
      "  def idScore : (∀ a. a -> a) -> Int = λ(poly : ∀ a. a -> a) poly 1;",
      "  def useHigher : ((∀ a. a -> a) -> Int) -> Int -> Int =",
      "    λ(score : (∀ a. a -> a) -> Int) λignored score id;",
      "  def apply : (Int -> Int) -> Int = λf f 0;",
      "  def main : Int = apply (useHigher idScore);",
      "}"
    ]

sourceClosureDemandedInlineFunctionArgumentProgram :: String
sourceClosureDemandedInlineFunctionArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def choose : (Int -> Int -> Int) -> Int -> Int -> Int = λf λignored λx f x ignored;",
      "  def apply : (Int -> Int) -> Int = λf f 4;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let use : (Int -> Int -> Int) -> Int = λfn apply (choose fn 0) in",
      "    use (λx λy captured);",
      "}"
    ]

sourcePartialApplicationNonVariableCalleeProgram :: String
sourcePartialApplicationNonVariableCalleeProgram =
  unlines
    [ "module Main export (main) {",
      "  def make : Int -> Int -> Int -> Int = λbase λignored λx base;",
      "  def apply : (Int -> Int) -> Int = λf f 4;",
      "  def main : Int =",
      "    let base : Int = 1 in",
      "    apply (((λz make z) base) 2);",
      "}"
    ]

sourcePolymorphicTopLevelClosureCallProgram :: String
sourcePolymorphicTopLevelClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def maker : ∀ a. a -> Int = let captured : Int = 41 in λ(x : a) captured;",
      "  def main : Int = maker 0;",
      "}"
    ]

sourceTopLevelClosureArgumentProgram :: String
sourceTopLevelClosureArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def maker : Int -> Int = let captured : Int = 41 in λ(x : Int) captured;",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int) f 1;",
      "  def main : Int = use maker;",
      "}"
    ]

sourceFunctionParameterNestedClosureAliasCallProgram :: String
sourceFunctionParameterNestedClosureAliasCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int)",
      "    let g : Int -> Int = (let h : Int -> Int = f in h) in",
      "    g 1;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    use f;",
      "}"
    ]

sourceReturnedLetLambdaClosureProgram :: String
sourceReturnedLetLambdaClosureProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int -> Int -> Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int -> Int = λ(x : Int) let y : Int = captured in λ(z : Int) y in",
      "    f;",
      "}"
    ]

sourceReturnedLetLambdaShadowingProgram :: String
sourceReturnedLetLambdaShadowingProgram =
  unlines
    [ "module Main export (main) {",
      "  def make : Int -> Int = let y : Int = 1 in λ(y : Int) y;",
      "  def main : Int = make 7;",
      "}"
    ]

sourceClosureValuedConstructorFieldProgram :: String
sourceClosureValuedConstructorFieldProgram =
  unlines
    [ "module Main export (FnBox(..), main) {",
      "  data FnBox = FnBox : (Int -> Int) -> FnBox;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    case FnBox f of { FnBox g -> g 0 };",
      "}"
    ]

sourceCaseReturnedGlobalFunctionProgram :: String
sourceCaseReturnedGlobalFunctionProgram =
  unlines
    [ "module Main export (Choice(..), main) {",
      "  data Choice =",
      "      ChoiceSome : (Int -> Int) -> Choice",
      "    | ChoiceNone : Choice;",
      "",
      "  def helper : Int -> Int = λ(x : Int) x;",
      "  def pick : Choice -> (Int -> Int) = λ(choice : Choice) case choice of {",
      "    ChoiceSome f -> f;",
      "    ChoiceNone -> helper",
      "  };",
      "  def main : Int = (pick ChoiceNone) 41;",
      "}"
    ]

sourceCaseParameterClosureFieldProgram :: String
sourceCaseParameterClosureFieldProgram =
  unlines
    [ "module Main export (Choice(..), main) {",
      "  data Choice =",
      "      ChoiceSome : (Int -> Int) -> Choice",
      "    | ChoiceNone : Choice;",
      "",
      "  def helper : Int -> Int = λ(x : Int) x;",
      "  def pick : Choice -> (Int -> Int) = λ(choice : Choice) case choice of {",
      "    ChoiceSome f -> f;",
      "    ChoiceNone -> helper",
      "  };",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let local : Int -> Int = λ(x : Int) captured in",
      "    (pick (ChoiceSome local)) 0;",
      "}"
    ]

zeroCaptureClosureProgram :: BackendProgram
zeroCaptureClosureProgram =
  programWithMainExpr intTy $
    BackendLetWithIdentity
      { backendLetIdentity = fixtureLocalDetails "f", backendExprType = intTy,
        backendLetName = "f",
        backendLetType = unaryIntTy,
        backendLetRhs =
          BackendClosureWithParamIdentities
            { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992006),
              backendClosureEntryName = "__mlfp_closure$identity",
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            },
        backendLetBody = BackendClosureCall intTy (BackendVar unaryIntTy "f") [intLit 7]
      }

capturedClosureProgram :: BackendProgram
capturedClosureProgram =
  programWithMainExpr intTy $
    BackendLetWithIdentity
      { backendLetIdentity = fixtureLocalDetails "captured", backendExprType = intTy,
        backendLetName = "captured",
        backendLetType = intTy,
        backendLetRhs = intLit 41,
        backendLetBody =
          BackendLetWithIdentity
            { backendLetIdentity = fixtureLocalDetails "f", backendExprType = intTy,
              backendLetName = "f",
              backendLetType = unaryIntTy,
              backendLetRhs =
                BackendClosureWithParamIdentities
                  { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992007),
              backendClosureEntryName = "__mlfp_closure$constCaptured",
                    backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "captured") "captured" intTy (BackendVar intTy "captured")],
                    backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                    backendClosureBody = BackendVar intTy "captured"
                  },
              backendLetBody = BackendClosureCall intTy (BackendVar unaryIntTy "f") [intLit 0]
            }
      }

caseSelectedClosureCalleeProgram :: BackendProgram
caseSelectedClosureCalleeProgram =
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
                        BackendClosureCall
                          intTy
                          ( BackendCase
                              unaryIntTy
                              (optionSome intTy [intLit 0])
                              ( BackendAlternative
                                  (optionSomePattern [BackendPatternBinder (fixtureLocalDetails "n") "n"])
                                  (caseClosure "__mlfp_closure$case_some" (BackendVar intTy "x"))
                                  :| [ BackendAlternative
                                         optionNonePattern
                                         (caseClosure "__mlfp_closure$case_none" (intLit 0))
                                     ]
                              )
                          )
                          [intLit 7],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }
  where
    caseClosure entryName body =
      BackendClosureWithParamIdentities
        { backendExprType = unaryIntTy,
          backendClosureEntryIdentity = fixtureClosureEntryIdentity entryName,
          backendClosureEntryName = entryName,
          backendClosureCaptures = [],
          backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
          backendClosureBody = body
        }

letSelectedClosureCalleeProgram :: BackendProgram
letSelectedClosureCalleeProgram =
  programWithMainExpr intTy $
    BackendClosureCall
      intTy
      ( BackendLet
          unaryIntTy
          "f"
          unaryIntTy
          ( BackendClosureWithParamIdentities
              { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992009),
              backendClosureEntryName = "__mlfp_closure$let_callee",
                backendClosureCaptures = [],
                backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                backendClosureBody = BackendVar intTy "x"
              }
          )
          (BackendVar unaryIntTy "f")
      )
      [intLit 7]

polymorphicClosureSpecializationProgram :: BackendProgram
polymorphicClosureSpecializationProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "poly",
                      backendBindingType = polymorphicClosureSpecializationTy,
                      backendBindingExpr = polymorphicClosureSpecializationExpr,
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLet
                          intTy
                          "left"
                          intTy
                          (polymorphicClosureSpecializationCall intTy (intLit 7))
                          ( BackendLet
                              intTy
                              "right"
                              intTy
                              (polymorphicClosureSpecializationCall boolTy (boolLit True))
                              (intLit 0)
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

polymorphicClosureSpecializationTy :: BackendType
polymorphicClosureSpecializationTy =
  BTForall "a" Nothing (BTArrow (BTVar "a") intTy)

polymorphicClosureSpecializationExpr :: BackendExpr
polymorphicClosureSpecializationExpr =
  BackendTyAbs
    polymorphicClosureSpecializationTy
    "a"
    Nothing
    ( BackendLam
        (BTArrow (BTVar "a") intTy)
        "ignored"
        (BTVar "a")
        ( BackendLet
            intTy
            "f"
            unaryIntTy
            ( BackendClosureWithParamIdentities
                { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992010),
              backendClosureEntryName = "__mlfp_closure$poly",
                  backendClosureCaptures = [],
                  backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                  backendClosureBody = BackendVar intTy "x"
                }
            )
            (BackendClosureCall intTy (BackendVar unaryIntTy "f") [intLit 11])
        )
    )

polymorphicClosureSpecializationCall :: BackendType -> BackendExpr -> BackendExpr
polymorphicClosureSpecializationCall argTy arg =
  BackendApp
    intTy
    (BackendTyApp (BTArrow argTy intTy) (fixtureTopLevelVar polymorphicClosureSpecializationTy "poly") argTy)
    arg

polymorphicClosureFunctionWrapperProgram :: BackendProgram
polymorphicClosureFunctionWrapperProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = polymorphicClosureFunctionWrapperIdentity,
                      backendBindingNameWithMetadata = "polyWrapper",
                      backendBindingTypeWithMetadata = polymorphicClosureFunctionWrapperTy,
                      backendBindingExprWithMetadata = polymorphicClosureFunctionWrapperExpr,
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLet
                          intTy
                          "left"
                          fnBoxTy
                          (polymorphicClosureFunctionWrapperCall intTy (intLit 7))
                          ( BackendLet
                              intTy
                              "right"
                              fnBoxTy
                              (polymorphicClosureFunctionWrapperCall boolTy (boolLit True))
                              (intLit 0)
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

polymorphicClosureFunctionWrapperTy :: BackendType
polymorphicClosureFunctionWrapperTy =
  BTForallWithIdentity
    (polymorphicClosureFunctionWrapperTypeIdentity)
    "a"
    Nothing
    (BTArrow polymorphicClosureFunctionWrapperTypeVar fnBoxTy)

polymorphicClosureFunctionWrapperTypeIdentity :: TypeBinderIdentity
polymorphicClosureFunctionWrapperTypeIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 990023)

polymorphicClosureFunctionWrapperTypeVar :: BackendType
polymorphicClosureFunctionWrapperTypeVar =
  BTVarWithIdentity (polymorphicClosureFunctionWrapperTypeIdentity) "a"

polymorphicClosureFunctionWrapperIdentity :: SymbolIdentity
polymorphicClosureFunctionWrapperIdentity =
  symbolIdentityFromParts (UniqueIdentity 990025) SymbolValue "Main" "polyWrapper" Nothing

polymorphicClosureFunctionWrapperExpr :: BackendExpr
polymorphicClosureFunctionWrapperExpr =
  BackendTyAbsWithIdentity
    polymorphicClosureFunctionWrapperTy
    (polymorphicClosureFunctionWrapperTypeIdentity)
    "a"
    Nothing
    ( BackendLamWithIdentity
        (BTArrow polymorphicClosureFunctionWrapperTypeVar fnBoxTy)
        (polymorphicClosureFunctionWrapperParamIdentity)
        "ignored"
        polymorphicClosureFunctionWrapperTypeVar
        (fnBoxConstruct [closureContainingFunctionExpr])
    )

polymorphicClosureFunctionWrapperParamIdentity :: IdDetails
polymorphicClosureFunctionWrapperParamIdentity =
  localIdentity 990024 "ignored"

closureContainingFunctionExpr :: BackendExpr
closureContainingFunctionExpr =
  closureWithEntry "__mlfp_closure$wrapper_key"

polymorphicClosureFunctionWrapperCall :: BackendType -> BackendExpr -> BackendExpr
polymorphicClosureFunctionWrapperCall argTy arg =
  BackendApp
    fnBoxTy
    (BackendTyApp (BTArrow argTy fnBoxTy) (BackendVarWithIdentity polymorphicClosureFunctionWrapperTy ((TopLevelId polymorphicClosureFunctionWrapperIdentity)) "polyWrapper") argTy)
    arg

inlinePolymorphicClosureProgram :: BackendProgram
inlinePolymorphicClosureProgram =
  programWithBindings
    [ helperBinding,
      BackendBinding
        { backendBindingName = "polyInline",
          backendBindingType = inlinePolymorphicClosureTy,
          backendBindingExpr = inlinePolymorphicClosureExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              ( BackendApp
                  (BTArrow intTy intTy)
                  (BackendTyApp (BTArrow unaryIntTy (BTArrow intTy intTy)) (fixtureTopLevelVar inlinePolymorphicClosureTy "polyInline") intTy)
                  (fixtureTopLevelVar unaryIntTy "helper")
              )
              (intLit 0),
          backendBindingExportedAsMain = True
        }
    ]

inlinePolymorphicClosureTy :: BackendType
inlinePolymorphicClosureTy =
  BTForall "a" Nothing (BTArrow unaryIntTy (BTArrow (BTVar "a") intTy))

inlinePolymorphicClosureExpr :: BackendExpr
inlinePolymorphicClosureExpr =
  BackendTyAbs
    inlinePolymorphicClosureTy
    "a"
    Nothing
    ( BackendLam
        (BTArrow unaryIntTy (BTArrow (BTVar "a") intTy))
        "f"
        unaryIntTy
        ( BackendLam
            (BTArrow (BTVar "a") intTy)
            "ignored"
            (BTVar "a")
            ( BackendLet
                intTy
                "closure"
                unaryIntTy
                ( BackendClosureWithParamIdentities
                    { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992011),
              backendClosureEntryName = "__mlfp_closure$inline",
                      backendClosureCaptures = [],
                      backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                      backendClosureBody = BackendVar intTy "x"
                    }
                )
                ( BackendApp
                    intTy
                    (BackendVar unaryIntTy "f")
                    (BackendClosureCall intTy (BackendVar unaryIntTy "closure") [intLit 9])
                )
            )
        )
    )

closureEntryFunctionWrapperCollisionProgram :: BackendProgram
closureEntryFunctionWrapperCollisionProgram =
  programWithFnBoxMainExpr $
    fnBoxConstruct
      [closureWithEntry "__mlfp_function_wrapper$0"]

closureEntryRuntimeDeclarationCollisionProgram :: BackendProgram
closureEntryRuntimeDeclarationCollisionProgram =
  programWithMainExpr intTy $
    BackendLet
      intTy
      "f"
      unaryIntTy
      (closureWithEntry "malloc")
      (BackendClosureCall intTy (BackendVar unaryIntTy "f") [intLit 7])

closureWithEntry :: String -> BackendExpr
closureWithEntry entryName =
  BackendClosureWithParamIdentities
    { backendExprType = unaryIntTy,
      backendClosureEntryIdentity = UniqueIdentity (-992012),
      backendClosureEntryName = entryName,
      backendClosureCaptures = [],
      backendClosureParamsWithIdentities = [BackendClosureParam (closureWithEntryParamIdentity) "x" intTy],
      backendClosureBody = BackendVarWithIdentity intTy (closureWithEntryParamIdentity) "x"
    }

closureWithEntryParamIdentity :: IdDetails
closureWithEntryParamIdentity =
  localIdentity 990022 "x"

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
                      backendBindingExpr = fnBoxConstruct [closureWithEntry "__mlfp_closure$field_top"],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

directFunctionFieldProgram :: BackendProgram
directFunctionFieldProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = fnBoxTy,
                      backendBindingExpr = fnBoxConstruct [closureWithEntry "__mlfp_closure$field_direct"],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

localFunctionFieldProgram :: BackendProgram
localFunctionFieldProgram =
  programWithFnBoxMainExpr $
    BackendLetWithIdentity
      { backendLetIdentity = localFunctionFieldFIdentity, backendExprType = fnBoxTy,
        backendLetName = "f",
        backendLetType = unaryIntTy,
        backendLetRhs = closureWithEntry "__mlfp_closure$field_local",
        backendLetBody = fnBoxConstruct [BackendVarWithIdentity unaryIntTy (localFunctionFieldFIdentity) "f"]
      }
  where
    localFunctionFieldFIdentity = localIdentity 992044 "f"

transitiveLocalFunctionFieldProgram :: BackendProgram
transitiveLocalFunctionFieldProgram =
  programWithFnBoxMainExpr $
    BackendLetWithIdentity
      { backendLetIdentity = fixtureLocalDetails "f", backendExprType = fnBoxTy,
        backendLetName = "f",
        backendLetType = unaryIntTy,
        backendLetRhs = closureWithEntry "__mlfp_closure$field_transitive",
        backendLetBody =
          BackendLetWithIdentity
            { backendLetIdentity = fixtureLocalDetails "g", backendExprType = fnBoxTy,
              backendLetName = "g",
              backendLetType = unaryIntTy,
              backendLetRhs = BackendVar unaryIntTy "f",
              backendLetBody = fnBoxConstruct [BackendVar unaryIntTy "g"]
            }
      }

immediateRestoredFunctionFieldProgram :: BackendProgram
immediateRestoredFunctionFieldProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = fnBoxTy,
                      backendBindingExpr =
                        BackendCase
                          fnBoxTy
                          (fnBoxConstruct [closureWithEntry "__mlfp_closure$field_restored"])
                          ( BackendAlternative
                              (fnBoxPatternWithBinders [BackendPatternBinder (immediateRestoredFIdentity) "f"])
                              (fnBoxConstruct [BackendVarWithIdentity unaryIntTy (immediateRestoredFIdentity) "f"])
                              :| []
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }
  where
    immediateRestoredFIdentity = localIdentity 992045 "f"

capturedFunctionFieldProgram :: BackendProgram
capturedFunctionFieldProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = fnBoxTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          fnBoxTy
                          (capturedFunctionCapturedIdentity)
                          "captured"
                          intTy
                          (intLit 1)
                          ( fnBoxConstruct
                              [ BackendClosureWithParamIdentities
                                  { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992013),
              backendClosureEntryName = "__mlfp_closure$field_captured",
                                    backendClosureCaptures =
                                      [ BackendClosureCapture
                                          (capturedFunctionCapturedIdentity)
                                          "captured"
                                          intTy
                                          (BackendVarWithIdentity intTy (capturedFunctionCapturedIdentity) "captured")
                                      ],
                                    backendClosureParamsWithIdentities = [BackendClosureParam (capturedFunctionXIdentity) "x" intTy],
                                    backendClosureBody = BackendVarWithIdentity intTy (capturedFunctionCapturedIdentity) "captured"
                                  }
                              ]
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }
  where
    capturedFunctionCapturedIdentity = localIdentity 992046 "captured"
    capturedFunctionXIdentity = localIdentity 992047 "x"

capturedFunctionPointerCallProgram :: BackendProgram
capturedFunctionPointerCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "inc",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr =
                        BackendLamWithIdentity
                          { backendParamIdentity = fixtureLocalDetails "x", backendExprType = unaryIntTy,
                            backendParamName = "x",
                            backendParamType = intTy,
                            backendBody = BackendVar intTy "x"
                          },
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendClosureCall
                          intTy
                          ( BackendClosureWithParamIdentities
                              { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992014),
              backendClosureEntryName = "__mlfp_closure$call_captured_function",
                                backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "f") "f" unaryIntTy (fixtureTopLevelVar unaryIntTy "inc")],
                                backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                backendClosureBody = BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x")
                              }
                          )
                          [intLit 41],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

capturedNullaryGlobalFunctionAliasProgram :: BackendProgram
capturedNullaryGlobalFunctionAliasProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "helper",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr =
                        BackendLamWithIdentity
                          { backendParamIdentity = fixtureLocalDetails "x", backendExprType = unaryIntTy,
                            backendParamName = "x",
                            backendParamType = intTy,
                            backendBody = BackendVar intTy "x"
                          },
                      backendBindingExportedAsMain = False
                    },
                  BackendBindingWithMetadata
                    { backendBindingIdentity = capturedNullaryGlobalAliasIdentity,
                      backendBindingNameWithMetadata = "get",
                      backendBindingTypeWithMetadata = unaryIntTy,
                      backendBindingExprWithMetadata =
                        BackendLet
                          unaryIntTy
                          "ignored"
                          intTy
                          (intLit 0)
                          (fixtureTopLevelVar unaryIntTy "helper"),
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendClosureCall
                          intTy
                          ( BackendClosureWithParamIdentities
                              { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992015),
              backendClosureEntryName = "__mlfp_closure$call_captured_nullary_global_alias",
                                backendClosureCaptures =
                                  [ BackendClosureCapture
                                      (fixtureLocalDetails "f")
                                      "f"
                                      unaryIntTy
                                      (BackendVarWithIdentity unaryIntTy ((TopLevelId capturedNullaryGlobalAliasIdentity)) "$stale_get")
                                  ],
                                backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                backendClosureBody = BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x")
                              }
                          )
                          [intLit 41],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }

capturedNullaryGlobalAliasIdentity :: SymbolIdentity
capturedNullaryGlobalAliasIdentity =
  symbolIdentityFromParts (UniqueIdentity 990003) SymbolValue "Main" "get" Nothing

capturedNullaryGlobalCaseClosureProgram :: BackendProgram
capturedNullaryGlobalCaseClosureProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "getCaseClosure",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr =
                        BackendCase
                          unaryIntTy
                          (optionSome intTy [intLit 41])
                          ( BackendAlternative
                              (optionSomePattern [BackendPatternBinder (fixtureLocalDetails "n") "n"])
                              ( BackendClosureWithParamIdentities
                                  { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992016),
              backendClosureEntryName = "__mlfp_closure$nullary_global_case_some",
                                    backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "n") "n" intTy (BackendVar intTy "n")],
                                    backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                    backendClosureBody = BackendVar intTy "n"
                                  }
                              )
                              :| [ BackendAlternative
                                     optionNonePattern
                                     ( BackendClosureWithParamIdentities
                                         { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992017),
              backendClosureEntryName = "__mlfp_closure$nullary_global_case_none",
                                           backendClosureCaptures = [],
                                           backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                           backendClosureBody = intLit 0
                                         }
                                     )
                                 ]
                          ),
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendClosureCall
                          intTy
                          ( BackendClosureWithParamIdentities
                              { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992018),
              backendClosureEntryName = "__mlfp_closure$call_captured_nullary_global_case",
                                backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "f") "f" unaryIntTy (fixtureTopLevelVar unaryIntTy "getCaseClosure")],
                                backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                backendClosureBody = BackendClosureCall intTy (BackendVar unaryIntTy "f") [BackendVar intTy "x"]
                              }
                          )
                          [intLit 999],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }

completeTypeAppliedClosureReturnProgram :: BackendProgram
completeTypeAppliedClosureReturnProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "makeTypeAppliedClosure",
                      backendBindingType = BTArrow intTy unaryIntTy,
                      backendBindingExpr =
                        BackendLamWithIdentity
                          (BTArrow intTy unaryIntTy)
                          completeTypeAppliedCaptureIdentity
                          "captured"
                          intTy
                          ( BackendTyApp
                              unaryIntTy
                              ( BackendTyAbsWithIdentity
                                  completeTypeAppliedClosurePolyTy
                                  completeTypeAppliedBinderIdentity
                                  "result"
                                  Nothing
                                  ( BackendClosureWithParamIdentities
                                      { backendExprType = unaryIntTy,
                                        backendClosureEntryIdentity = fixtureClosureEntryIdentity "__mlfp_closure$complete_direct_typeapp_return",
                                        backendClosureEntryName = "__mlfp_closure$complete_direct_typeapp_return",
                                        backendClosureCaptures =
                                          [ BackendClosureCapture
                                              completeTypeAppliedCaptureIdentity
                                              "captured"
                                              intTy
                                              (BackendVarWithIdentity intTy completeTypeAppliedCaptureIdentity "captured")
                                          ],
                                        backendClosureParamsWithIdentities =
                                          [BackendClosureParam completeTypeAppliedParamIdentity "x" intTy],
                                        backendClosureBody = BackendVarWithIdentity intTy completeTypeAppliedCaptureIdentity "captured"
                                      }
                                  )
                              )
                              intTy
                          ),
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendApp
                              unaryIntTy
                              (fixtureTopLevelVar (BTArrow intTy unaryIntTy) "makeTypeAppliedClosure")
                              (intLit 41)
                          )
                          (intLit 0),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }

completeTypeAppliedClosurePolyTy :: BackendType
completeTypeAppliedClosurePolyTy =
  BTForallWithIdentity completeTypeAppliedBinderIdentity "result" Nothing unaryIntTy

completeTypeAppliedBinderIdentity :: TypeBinderIdentity
completeTypeAppliedBinderIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 996500)

completeTypeAppliedCaptureIdentity :: IdDetails
completeTypeAppliedCaptureIdentity =
  localIdentity 996501 "captured"

completeTypeAppliedParamIdentity :: IdDetails
completeTypeAppliedParamIdentity =
  localIdentity 996502 "x"

rawReturnedFunctionPointerCallProgram :: BackendProgram
rawReturnedFunctionPointerCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "inc",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr =
                        BackendLamWithIdentity
                          { backendParamIdentity = fixtureLocalDetails "x", backendExprType = unaryIntTy,
                            backendParamName = "x",
                            backendParamType = intTy,
                            backendBody = BackendVar intTy "x"
                          },
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "idRaw",
                      backendBindingType = BTArrow unaryIntTy (BTArrow intTy unaryIntTy),
                      backendBindingExpr =
                        BackendLamWithIdentity
                          { backendParamIdentity = fixtureLocalDetails "$evidence_f", backendExprType = BTArrow unaryIntTy (BTArrow intTy unaryIntTy),
                            backendParamName = "$evidence_f",
                            backendParamType = unaryIntTy,
                            backendBody =
                              BackendLamWithIdentity
                                { backendParamIdentity = fixtureLocalDetails "ignored", backendExprType = BTArrow intTy unaryIntTy,
                                  backendParamName = "ignored",
                                  backendParamType = intTy,
                                  backendBody =
                                    BackendLet
                                      unaryIntTy
                                      "dummy"
                                      intTy
                                      (intLit 0)
                                      (BackendVar unaryIntTy "$evidence_f")
                                }
                          },
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendApp
                              unaryIntTy
                              ( BackendApp
                                  (BTArrow intTy unaryIntTy)
                                  (fixtureTopLevelVar (BTArrow unaryIntTy (BTArrow intTy unaryIntTy)) "idRaw")
                                  (fixtureTopLevelVar unaryIntTy "inc")
                              )
                              (intLit 0)
                          )
                          (intLit 41),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

rawFunctionPointerAliasReturnProgram :: BackendProgram
rawFunctionPointerAliasReturnProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "inc",
                      backendBindingType = unaryIntTy,
                      backendBindingExpr =
                        BackendLamWithIdentity
                          { backendParamIdentity = fixtureLocalDetails "x", backendExprType = unaryIntTy,
                            backendParamName = "x",
                            backendParamType = intTy,
                            backendBody = BackendVar intTy "x"
                          },
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "idAlias",
                      backendBindingType = BTArrow unaryIntTy (BTArrow intTy unaryIntTy),
                      backendBindingExpr =
                        BackendLamWithIdentity
                          { backendParamIdentity = fixtureLocalDetails "$evidence_f", backendExprType = BTArrow unaryIntTy (BTArrow intTy unaryIntTy),
                            backendParamName = "$evidence_f",
                            backendParamType = unaryIntTy,
                            backendBody =
                              BackendLamWithIdentity
                                { backendParamIdentity = fixtureLocalDetails "ignored", backendExprType = BTArrow intTy unaryIntTy,
                                  backendParamName = "ignored",
                                  backendParamType = intTy,
                                  backendBody =
                                    BackendLet
                                      unaryIntTy
                                      "dummy"
                                      intTy
                                      (intLit 0)
                                      ( BackendLet
                                          unaryIntTy
                                          "g"
                                          unaryIntTy
                                          (BackendVar unaryIntTy "$evidence_f")
                                          (BackendVar unaryIntTy "g")
                                      )
                                }
                          },
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendApp
                              unaryIntTy
                              ( BackendApp
                                  (BTArrow intTy unaryIntTy)
                                  (fixtureTopLevelVar (BTArrow unaryIntTy (BTArrow intTy unaryIntTy)) "idAlias")
                                  (fixtureTopLevelVar unaryIntTy "inc")
                              )
                              (intLit 0)
                          )
                          (intLit 41),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

firstOrderFunctionParameterCallProgram :: BackendProgram
firstOrderFunctionParameterCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "applyFirst",
                      backendBindingType = BTArrow unaryIntTy unaryIntTy,
                      backendBindingExpr =
                        BackendLamWithIdentity
                          { backendParamIdentity = fixtureLocalDetails "f", backendExprType = BTArrow unaryIntTy unaryIntTy,
                            backendParamName = "f",
                            backendParamType = unaryIntTy,
                            backendBody =
                              BackendLamWithIdentity
                                { backendParamIdentity = fixtureLocalDetails "x", backendExprType = unaryIntTy,
                                  backendParamName = "x",
                                  backendParamType = intTy,
                                  backendBody = BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x")
                                }
                          },
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "applyFirst"
    }

closureFirstOrderFunctionParameterCallProgram :: BackendProgram
closureFirstOrderFunctionParameterCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ helperBinding,
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendClosureCall
                          intTy
                          ( BackendClosureWithParamIdentities
                              { backendExprType = BTArrow unaryIntTy unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992019),
              backendClosureEntryName = "__mlfp_closure$call_first_order_param",
                                backendClosureCaptures = [],
                                backendClosureParamsWithIdentities = backendClosureParams [("f", unaryIntTy), ("x", intTy)],
                                backendClosureBody = BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x")
                              }
                          )
                          [fixtureTopLevelVar unaryIntTy "helper", intLit 41],
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

rawFunctionPointerFieldCallProgram :: BackendProgram
rawFunctionPointerFieldCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ helperBinding,
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLet
                          intTy
                          "box"
                          fnBoxTy
                          (fnBoxConstruct [fixtureTopLevelVar unaryIntTy "helper"])
                              ( BackendCase
                                  intTy
                                  (BackendVar fnBoxTy "box")
                                  ( BackendAlternative
                                      (fnBoxPattern ["f"])
                                      (BackendClosureCall intTy (BackendVar unaryIntTy "f") [intLit 41])
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

closureFunctionFieldCallProgram :: BackendProgram
closureFunctionFieldCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          intTy
                          (closureFunctionCapturedIdentity)
                          "captured"
                          intTy
                          (intLit 41)
                          ( BackendLetWithIdentity
                              intTy
                              (closureFunctionBoxIdentity)
                              "box"
                              fnBoxTy
                              ( fnBoxConstruct
                                  [ BackendClosureWithParamIdentities
                                      { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992020),
              backendClosureEntryName = "__mlfp_closure$field_case_closure",
                                        backendClosureCaptures =
                                          [ BackendClosureCapture
                                              (closureFunctionCapturedIdentity)
                                              "captured"
                                              intTy
                                              (BackendVarWithIdentity intTy (closureFunctionCapturedIdentity) "captured")
                                          ],
                                        backendClosureParamsWithIdentities = [BackendClosureParam (closureFunctionXIdentity) "x" intTy],
                                        backendClosureBody = BackendVarWithIdentity intTy (closureFunctionCapturedIdentity) "captured"
                                      }
                                  ]
                              )
                              ( BackendCase
                                  intTy
                                  (BackendVarWithIdentity fnBoxTy (closureFunctionBoxIdentity) "box")
                                  ( BackendAlternative
                                      (fnBoxPatternWithBinders [BackendPatternBinder (closureFunctionFIdentity) "f"])
                                      (BackendClosureCall intTy (BackendVarWithIdentity unaryIntTy (closureFunctionFIdentity) "f") [intLit 0])
                                      :| []
                                  )
                              )
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    closureFunctionCapturedIdentity = localIdentity 992058 "captured"
    closureFunctionBoxIdentity = localIdentity 992059 "box"
    closureFunctionXIdentity = localIdentity 992060 "x"
    closureFunctionFIdentity = localIdentity 992061 "f"

returnedClosureFunctionFieldCallProgram :: BackendProgram
returnedClosureFunctionFieldCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = returnedClosureMakeBoxIdentity,
                      backendBindingNameWithMetadata = "makeBox",
                      backendBindingTypeWithMetadata = BTArrow intTy fnBoxTy,
                      backendBindingExprWithMetadata =
                        BackendLamWithIdentity
                          (BTArrow intTy fnBoxTy)
                          (returnedClosureBaseIdentity)
                          "base"
                          intTy
                          ( fnBoxConstruct
                              [ BackendClosureWithParamIdentities
                                  { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992021),
              backendClosureEntryName = "__mlfp_closure$returned_field_case_closure",
                                    backendClosureCaptures =
                                      [ BackendClosureCapture
                                          (returnedClosureBaseIdentity)
                                          "base"
                                          intTy
                                          (BackendVarWithIdentity intTy (returnedClosureBaseIdentity) "base")
                                      ],
                                    backendClosureParamsWithIdentities = [BackendClosureParam (returnedClosureXIdentity) "x" intTy],
                                    backendClosureBody = BackendVarWithIdentity intTy (returnedClosureBaseIdentity) "base"
                                  }
                              ]
                          ),
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendCase
                          intTy
                          (BackendApp fnBoxTy (BackendVarWithIdentity (BTArrow intTy fnBoxTy) ((TopLevelId returnedClosureMakeBoxIdentity)) "makeBox") (intLit 41))
                          ( BackendAlternative
                              (fnBoxPatternWithBinders [BackendPatternBinder (returnedClosureFIdentity) "f"])
                              (BackendClosureCall intTy (BackendVarWithIdentity unaryIntTy (returnedClosureFIdentity) "f") [intLit 0])
                              :| []
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    returnedClosureMakeBoxIdentity = symbolIdentityFromParts (UniqueIdentity 992048) SymbolValue "Main" "makeBox" Nothing
    returnedClosureBaseIdentity = localIdentity 992049 "base"
    returnedClosureXIdentity = localIdentity 992050 "x"
    returnedClosureFIdentity = localIdentity 992051 "f"

returnedReboxedClosureFunctionFieldCallProgram :: BackendProgram
returnedReboxedClosureFunctionFieldCallProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = returnedReboxedMakeRestoredIdentity,
                      backendBindingNameWithMetadata = "makeRestored",
                      backendBindingTypeWithMetadata = BTArrow fnBoxTy fnBoxTy,
                      backendBindingExprWithMetadata =
                        BackendLamWithIdentity
                          (BTArrow fnBoxTy fnBoxTy)
                          (returnedReboxedBoxIdentity)
                          "box"
                          fnBoxTy
                          ( BackendCase
                              fnBoxTy
                              (BackendVarWithIdentity fnBoxTy (returnedReboxedBoxIdentity) "box")
                              ( BackendAlternative
                                  (fnBoxPatternWithBinders [BackendPatternBinder (returnedReboxedInnerFIdentity) "f"])
                                  (fnBoxConstruct [BackendVarWithIdentity unaryIntTy (returnedReboxedInnerFIdentity) "f"])
                                  :| []
                              )
                          ),
                      backendBindingExportedAsMainWithMetadata = False,
                      backendBindingEvidenceParamIndices = Set.empty
                    },
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLetWithIdentity
                          intTy
                          (returnedReboxedCapturedIdentity)
                          "captured"
                          intTy
                          (intLit 41)
                          ( BackendCase
                              intTy
                              ( BackendApp
                                  fnBoxTy
                                  (BackendVarWithIdentity (BTArrow fnBoxTy fnBoxTy) ((TopLevelId returnedReboxedMakeRestoredIdentity)) "makeRestored")
                                  ( fnBoxConstruct
                                      [ BackendClosureWithParamIdentities
                                          { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992022),
              backendClosureEntryName = "__mlfp_closure$returned_reboxed_field_case_closure",
                                            backendClosureCaptures =
                                              [ BackendClosureCapture
                                                  (returnedReboxedCapturedIdentity)
                                                  "captured"
                                                  intTy
                                                  (BackendVarWithIdentity intTy (returnedReboxedCapturedIdentity) "captured")
                                              ],
                                            backendClosureParamsWithIdentities = [BackendClosureParam (returnedReboxedXIdentity) "x" intTy],
                                            backendClosureBody = BackendVarWithIdentity intTy (returnedReboxedCapturedIdentity) "captured"
                                          }
                                      ]
                                  )
                              )
                              ( BackendAlternative
                                  (fnBoxPatternWithBinders [BackendPatternBinder (returnedReboxedOuterFIdentity) "f"])
                                  (BackendClosureCall intTy (BackendVarWithIdentity unaryIntTy (returnedReboxedOuterFIdentity) "f") [intLit 0])
                                  :| []
                              )
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }
  where
    returnedReboxedMakeRestoredIdentity = symbolIdentityFromParts (UniqueIdentity 992052) SymbolValue "Main" "makeRestored" Nothing
    returnedReboxedBoxIdentity = localIdentity 992053 "box"
    returnedReboxedInnerFIdentity = localIdentity 992054 "f"
    returnedReboxedCapturedIdentity = localIdentity 992055 "captured"
    returnedReboxedXIdentity = localIdentity 992056 "x"
    returnedReboxedOuterFIdentity = localIdentity 992057 "f"

caseReturnedClosureOverApplicationProgram :: BackendProgram
caseReturnedClosureOverApplicationProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "makeFromCase",
                      backendBindingType = BTArrow intTy unaryIntTy,
                      backendBindingExpr =
                        BackendLam
                          (BTArrow intTy unaryIntTy)
                          "base"
                          intTy
                          ( BackendCase
                              unaryIntTy
                              (optionSome intTy [BackendVar intTy "base"])
                              ( BackendAlternative
                                  (optionSomePattern [BackendPatternBinder (fixtureLocalDetails "captured") "captured"])
                                  ( BackendClosureWithParamIdentities
                                      { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992023),
              backendClosureEntryName = "__mlfp_closure$case_returned_some",
                                        backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "captured") "captured" intTy (BackendVar intTy "captured")],
                                        backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                        backendClosureBody = BackendVar intTy "captured"
                                      }
                                  )
                                  :| [ BackendAlternative
                                         optionNonePattern
                                         ( BackendClosureWithParamIdentities
                                             { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992024),
              backendClosureEntryName = "__mlfp_closure$case_returned_none",
                                               backendClosureCaptures = [],
                                               backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                               backendClosureBody = intLit 0
                                             }
                                         )
                                     ]
                              )
                          ),
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          (BackendApp unaryIntTy (fixtureTopLevelVar (BTArrow intTy unaryIntTy) "makeFromCase") (intLit 41))
                          (intLit 0),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }

mixedCallableCaseResultProgram :: BackendProgram
mixedCallableCaseResultProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ helperBinding,
                  BackendBinding
                    { backendBindingName = "makeMixed",
                      backendBindingType = BTArrow unaryIntTy (BTArrow intTy unaryIntTy),
                      backendBindingExpr =
                        BackendLam
                          (BTArrow unaryIntTy (BTArrow intTy unaryIntTy))
                          "$evidence_f"
                          unaryIntTy
                          ( BackendLam
                              (BTArrow intTy unaryIntTy)
                              "base"
                              intTy
                              ( BackendCase
                                  unaryIntTy
                                  (optionSome intTy [BackendVar intTy "base"])
                                  ( BackendAlternative
                                      (optionSomePattern [BackendPatternBinder (fixtureLocalDetails "captured") "captured"])
                                      ( BackendClosureWithParamIdentities
                                          { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992025),
              backendClosureEntryName = "__mlfp_closure$mixed_case_closure",
                                            backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "captured") "captured" intTy (BackendVar intTy "captured")],
                                            backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                            backendClosureBody = BackendVar intTy "captured"
                                          }
                                      )
                                      :| [ BackendAlternative
                                             optionNonePattern
                                             (BackendVar unaryIntTy "$evidence_f")
                                         ]
                                  )
                              )
                          ),
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendApp
                              unaryIntTy
                              ( BackendApp
                                  (BTArrow intTy unaryIntTy)
                                  (fixtureTopLevelVar (BTArrow unaryIntTy (BTArrow intTy unaryIntTy)) "makeMixed")
                                  (fixtureTopLevelVar unaryIntTy "helper")
                              )
                              (intLit 41)
                          )
                          (intLit 0),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }

caseHeadedDirectClosureBackendAppProgram :: BackendProgram
caseHeadedDirectClosureBackendAppProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [optionData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendCase
                              unaryIntTy
                              (optionSome intTy [intLit 41])
                              ( BackendAlternative
                                  (optionSomePattern [BackendPatternBinder (fixtureLocalDetails "captured") "captured"])
                                  ( BackendClosureWithParamIdentities
                                      { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992026),
              backendClosureEntryName = "__mlfp_closure$backend_app_case_some",
                                        backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "captured") "captured" intTy (BackendVar intTy "captured")],
                                        backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                        backendClosureBody = BackendVar intTy "captured"
                                      }
                                  )
                                  :| [ BackendAlternative
                                         optionNonePattern
                                         ( BackendClosureWithParamIdentities
                                             { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992027),
              backendClosureEntryName = "__mlfp_closure$backend_app_case_none",
                                               backendClosureCaptures = [],
                                               backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                               backendClosureBody = intLit 0
                                             }
                                         )
                                     ]
                              )
                          )
                          (intLit 0),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }

letHeadedDirectClosureBackendAppProgram :: BackendProgram
letHeadedDirectClosureBackendAppProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          ( BackendLet
                              unaryIntTy
                              "captured"
                              intTy
                              (intLit 41)
                              ( BackendClosureWithParamIdentities
                                  { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992028),
              backendClosureEntryName = "__mlfp_closure$backend_app_let",
                                    backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "captured") "captured" intTy (BackendVar intTy "captured")],
                                    backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                    backendClosureBody = BackendVar intTy "captured"
                                  }
                              )
                          )
                          (intLit 0),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }

capturedFirstOrderParameterClosureProgram :: BackendProgram
capturedFirstOrderParameterClosureProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ helperBinding,
                  BackendBinding
                    { backendBindingName = "makeCaller",
                      backendBindingType = BTArrow unaryIntTy unaryIntTy,
                      backendBindingExpr =
                        BackendLamWithIdentity
                          { backendParamIdentity = fixtureLocalDetails "f", backendExprType = BTArrow unaryIntTy unaryIntTy,
                            backendParamName = "f",
                            backendParamType = unaryIntTy,
                            backendBody =
                              BackendClosureWithParamIdentities
                                { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992029),
              backendClosureEntryName = "__mlfp_closure$capture_first_order_param",
                                  backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "f") "f" unaryIntTy (BackendVar unaryIntTy "f")],
                                  backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                  backendClosureBody = BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x")
                                }
                          },
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendApp
                          intTy
                          (BackendApp unaryIntTy (fixtureTopLevelVar (BTArrow unaryIntTy unaryIntTy) "makeCaller") (fixtureTopLevelVar unaryIntTy "helper"))
                          (intLit 41),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

letBoundFirstOrderParameterClosureProgram :: BackendProgram
letBoundFirstOrderParameterClosureProgram =
  programWithBindings
    [ helperBinding,
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              ( BackendApp
                  unaryIntTy
                  ( BackendLet
                      (BTArrow unaryIntTy unaryIntTy)
                      "makeCaller"
                      (BTArrow unaryIntTy unaryIntTy)
                      ( BackendLamWithIdentity
                          { backendParamIdentity = fixtureLocalDetails "f", backendExprType = BTArrow unaryIntTy unaryIntTy,
                            backendParamName = "f",
                            backendParamType = unaryIntTy,
                            backendBody =
                              BackendClosureWithParamIdentities
                                { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992030),
              backendClosureEntryName = "__mlfp_closure$let_capture_first_order_param",
                                  backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "f") "f" unaryIntTy (BackendVar unaryIntTy "f")],
                                  backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                                  backendClosureBody = BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x")
                                }
                          }
                      )
                      (BackendVar (BTArrow unaryIntTy unaryIntTy) "makeCaller")
                  )
                  (fixtureTopLevelVar unaryIntTy "helper")
              )
              (intLit 41),
          backendBindingExportedAsMain = True
        }
    ]

mixedCallableCaptureClosureEntryProgram :: BackendProgram
mixedCallableCaptureClosureEntryProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ helperBinding,
                  BackendBinding
                    { backendBindingName = "entry",
                      backendBindingType = intTy,
                      backendBindingExpr =
                        BackendLet
                          intTy
                          "makeCaller"
                          (BTArrow unaryIntTy unaryIntTy)
                          mixedCallableCaptureFunction
                          ( BackendLet
                              intTy
                              "ignored"
                              intTy
                              ( BackendApp
                                  intTy
                                  ( BackendApp
                                      unaryIntTy
                                      (BackendVar (BTArrow unaryIntTy unaryIntTy) "makeCaller")
                                      (fixtureTopLevelVar unaryIntTy "helper")
                                  )
                                  (intLit 7)
                              )
                              ( BackendApp
                                  intTy
                                  ( BackendApp
                                      unaryIntTy
                                      (BackendVar (BTArrow unaryIntTy unaryIntTy) "makeCaller")
                                      mixedCallableCaptureClosureArgument
                                  )
                                  (intLit 0)
                              )
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "entry"
    }

mixedCallableCaptureFunction :: BackendExpr
mixedCallableCaptureFunction =
  BackendLamWithIdentity
    { backendParamIdentity = fixtureLocalDetails "f", backendExprType = BTArrow unaryIntTy unaryIntTy,
      backendParamName = "f",
      backendParamType = unaryIntTy,
      backendBody =
        BackendClosureWithParamIdentities
          { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992031),
              backendClosureEntryName = "__mlfp_closure$mixed_callable_capture",
            backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "f") "f" unaryIntTy (BackendVar unaryIntTy "f")],
            backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
            backendClosureBody = BackendApp intTy (BackendVar unaryIntTy "f") (BackendVar intTy "x")
          }
    }

mixedCallableCaptureClosureArgument :: BackendExpr
mixedCallableCaptureClosureArgument =
  BackendClosureWithParamIdentities
    { backendExprType = unaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992032),
              backendClosureEntryName = "__mlfp_closure$mixed_callable_argument",
      backendClosureCaptures = [],
      backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
      backendClosureBody = intLit 42
    }

returnedClosurePartialApplicationProgram :: BackendProgram
returnedClosurePartialApplicationProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "makeBinary",
          backendBindingType = BTArrow intTy binaryIntTy,
          backendBindingExpr =
            BackendLamWithIdentity
              { backendParamIdentity = fixtureLocalDetails "base", backendExprType = BTArrow intTy binaryIntTy,
                backendParamName = "base",
                backendParamType = intTy,
                backendBody =
                  BackendClosureWithParamIdentities
                    { backendExprType = binaryIntTy,
              backendClosureEntryIdentity = UniqueIdentity (-992033),
              backendClosureEntryName = "__mlfp_closure$returned_binary",
                      backendClosureCaptures = [BackendClosureCapture (fixtureLocalDetails "base") "base" intTy (BackendVar intTy "base")],
                      backendClosureParamsWithIdentities = backendClosureParams [("x", intTy), ("y", intTy)],
                      backendClosureBody = BackendVar intTy "base"
                    }
              },
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = unaryIntTy,
          backendBindingExpr =
            BackendApp
              unaryIntTy
              ( BackendApp
                  binaryIntTy
                  (fixtureTopLevelVar (BTArrow intTy binaryIntTy) "makeBinary")
                  (intLit 41)
              )
              (intLit 0),
          backendBindingExportedAsMain = True
        }
    ]

rawReturnedFunctionPointerPartialApplicationProgram :: BackendProgram
rawReturnedFunctionPointerPartialApplicationProgram =
  programWithBindings
    [ addBinding,
      BackendBinding
        { backendBindingName = "idRawBinary",
          backendBindingType = BTArrow binaryIntTy (BTArrow intTy binaryIntTy),
          backendBindingExpr =
            BackendLamWithIdentity
              { backendParamIdentity = fixtureLocalDetails "$evidence_f", backendExprType = BTArrow binaryIntTy (BTArrow intTy binaryIntTy),
                backendParamName = "$evidence_f",
                backendParamType = binaryIntTy,
                backendBody =
                  BackendLamWithIdentity
                    { backendParamIdentity = fixtureLocalDetails "ignored", backendExprType = BTArrow intTy binaryIntTy,
                      backendParamName = "ignored",
                      backendParamType = intTy,
                      backendBody =
                        BackendLet
                          binaryIntTy
                          "dummy"
                          intTy
                          (intLit 0)
                          (BackendVar binaryIntTy "$evidence_f")
                    }
              },
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = unaryIntTy,
          backendBindingExpr =
            BackendApp
              unaryIntTy
              ( BackendApp
                  binaryIntTy
                  ( BackendApp
                      (BTArrow intTy binaryIntTy)
                      (fixtureTopLevelVar (BTArrow binaryIntTy (BTArrow intTy binaryIntTy)) "idRawBinary")
                      (fixtureTopLevelVar binaryIntTy "add")
                  )
                  (intLit 0)
              )
              (intLit 1),
          backendBindingExportedAsMain = True
        }
    ]

nullaryGlobalValueKindCycleProgram :: BackendProgram
nullaryGlobalValueKindCycleProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ nullaryFunctionAliasBinding "left" "right",
                  nullaryFunctionAliasBinding "right" "left",
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = intTy,
                      backendBindingExpr = BackendApp intTy (fixtureTopLevelVar unaryIntTy "left") (intLit 41),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

nullaryFunctionAliasBinding :: String -> String -> BackendBinding
nullaryFunctionAliasBinding name target =
  BackendBinding
    { backendBindingName = name,
      backendBindingType = unaryIntTy,
      backendBindingExpr =
        BackendLet
          unaryIntTy
          "dummy"
          intTy
          (intLit 0)
          (fixtureTopLevelVar unaryIntTy target),
      backendBindingExportedAsMain = False
    }

unknownBaseProgram :: BackendProgram
unknownBaseProgram =
  programWithMainExpr mysteryTy (fixtureTopLevelVar mysteryTy "main")

rollMismatchProgram :: BackendProgram
rollMismatchProgram =
  programWithMainExpr recIntTy (BackendRoll recIntTy (intLit 1))

structuralRollUnrollCarrierProgram :: BackendProgram
structuralRollUnrollCarrierProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [resultBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = resultBoxStructuralTy,
                      backendBindingExpr =
                        BackendRoll
                          resultBoxStructuralTy
                          ( BackendUnroll
                              (singleFieldStructuralBody intTy)
                              (BackendConstruct resultBoxStructuralTy "ResultBox" [intLit 1])
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

conflictingIdentityDataParameterProgram :: BackendProgram
conflictingIdentityDataParameterProgram =
  BackendProgram
    [ BackendModule
        "Main"
        [ BackendData
            "Pair"
            ["a", "a"]
            [BackendConstructor "Pair" [] [BTVar "a"] (BTCon (BaseTy "Pair") (BTVar "a" :| [BTVar "a"]))]
        ]
        [ BackendBinding
            { backendBindingName = "main",
              backendBindingType = intTy,
              backendBindingExpr = intLit 1,
              backendBindingExportedAsMain = True
            }
        ]
    ]
    "main"

addBinding :: BackendBinding
addBinding =
  BackendBinding
    { backendBindingName = "add",
      backendBindingType = binaryIntTy,
      backendBindingExpr =
        BackendLamWithIdentity
          { backendParamIdentity = fixtureLocalDetails "x", backendExprType = binaryIntTy,
            backendParamName = "x",
            backendParamType = intTy,
            backendBody =
              BackendLamWithIdentity
                { backendParamIdentity = fixtureLocalDetails "y", backendExprType = unaryIntTy,
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
  BackendLamWithIdentity
    { backendParamIdentity = intIdentityParamIdentity, backendExprType = unaryIntTy,
      backendParamName = "x",
      backendParamType = intTy,
      backendBody = BackendVarWithIdentity intTy (intIdentityParamIdentity) "x"
    }

intIdentityParamIdentity :: IdDetails
intIdentityParamIdentity =
  localIdentity 990026 "x"

boolIdentityExpr :: BackendExpr
boolIdentityExpr =
  BackendLamWithIdentity
    { backendParamIdentity = boolIdentityParamIdentity, backendExprType = unaryBoolTy,
      backendParamName = "x",
      backendParamType = boolTy,
      backendBody = BackendVarWithIdentity boolTy (boolIdentityParamIdentity) "x"
    }

boolIdentityParamIdentity :: IdDetails
boolIdentityParamIdentity =
  localIdentity 990027 "x"

fnBoxData :: BackendData
fnBoxData =
  BackendDataWithIdentity
    { backendDataIdentity = fnBoxIdentity,
      backendDataNameWithIdentity = "FnBox",
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity =
        [ BackendConstructorWithIdentity
            { backendConstructorIdentity = fnBoxConstructorIdentity,
              backendConstructorNameWithIdentity = "FnBox",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [unaryIntTy],
              backendConstructorResultWithIdentity = fnBoxTy
            }
        ]
    }

fnBoxIdentity :: SymbolIdentity
fnBoxIdentity =
  symbolIdentityFromParts (UniqueIdentity 990020) SymbolType "Main" "FnBox" Nothing

fnBoxConstructorIdentity :: SymbolIdentity
fnBoxConstructorIdentity =
  symbolIdentityFromParts (UniqueIdentity 990021) SymbolConstructor "Main" "FnBox" (Just (SymbolOwnerType fnBoxIdentity))

fnBoxConstruct :: [BackendExpr] -> BackendExpr
fnBoxConstruct =
  BackendConstructWithIdentity fnBoxTy (fnBoxConstructorIdentity) "FnBox"

fnBoxPattern :: [String] -> BackendPattern
fnBoxPattern binders =
  fnBoxPatternWithBinders [BackendPatternBinder (fixtureLocalDetails binder) binder | binder <- binders]

fnBoxPatternWithBinders :: [BackendPatternBinder] -> BackendPattern
fnBoxPatternWithBinders binders =
  BackendConstructorPatternWithBinderIdentities
    (fnBoxConstructorIdentity)
    "FnBox"
    binders

lazyFieldBoxData :: BackendData
lazyFieldBoxData =
  BackendDataWithIdentity
    { backendDataIdentity = lazyFieldBoxIdentity,
      backendDataNameWithIdentity = "LazyFieldBox",
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity =
        [ BackendConstructorWithIdentity
            { backendConstructorIdentity = lazyFieldBoxPackedIdentity,
              backendConstructorNameWithIdentity = "Packed",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [lazyFieldPolyTy, intTy],
              backendConstructorResultWithIdentity = lazyFieldBoxTy
            }
        ]
    }

lazyFieldPolyTy :: BackendType
lazyFieldPolyTy =
  BTForallWithIdentity (lazyFieldPolyTypeParameterIdentity) "a" Nothing lazyFieldPolyTypeParameterTy

lazyFieldPolyTypeParameterIdentity :: TypeBinderIdentity
lazyFieldPolyTypeParameterIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 992020)

lazyFieldPolyTypeParameterTy :: BackendType
lazyFieldPolyTypeParameterTy =
  BTVarWithIdentity (lazyFieldPolyTypeParameterIdentity) "a"

strictBoxData :: BackendData
strictBoxData =
  BackendDataWithIdentity
    { backendDataIdentity = strictBoxIdentity,
      backendDataNameWithIdentity = "StrictBox",
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity =
        [ BackendConstructorWithIdentity
            { backendConstructorIdentity = strictBoxConstructorIdentity,
              backendConstructorNameWithIdentity = "StrictBox",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [polyIdTy, recIntTy],
              backendConstructorResultWithIdentity = strictBoxTy
            }
        ]
    }

strictBoxDataWithConstructorIdentity :: BackendData
strictBoxDataWithConstructorIdentity =
  strictBoxData

strictBoxIdentity :: SymbolIdentity
strictBoxIdentity =
  symbolIdentityFromParts (UniqueIdentity 992021) SymbolType "Main" "StrictBox" Nothing

strictBoxConstructorIdentity :: SymbolIdentity
strictBoxConstructorIdentity =
  symbolIdentityFromParts (UniqueIdentity 990005) SymbolConstructor "Main" "StrictBox" (Just (SymbolOwnerType strictBoxIdentity))

immediateChoiceData :: BackendData
immediateChoiceData =
  immediateChoiceDataWithConstructors
    [ BackendConstructorWithIdentity
        { backendConstructorIdentity = immediateChoiceConstructorIdentity,
          backendConstructorNameWithIdentity = "WithStatic",
          backendConstructorForallsWithIdentity = [],
          backendConstructorFieldsWithIdentity = [polyIdTy, intTy],
          backendConstructorResultWithIdentity = immediateChoiceTy
        },
      BackendConstructorWithIdentity
        { backendConstructorIdentity = immediateChoiceOtherIdentity,
          backendConstructorNameWithIdentity = "Other",
          backendConstructorForallsWithIdentity = [],
          backendConstructorFieldsWithIdentity = [],
          backendConstructorResultWithIdentity = immediateChoiceTy
        }
    ]

immediateChoiceDataWithConstructors :: [BackendConstructor] -> BackendData
immediateChoiceDataWithConstructors constructors =
  BackendDataWithIdentity
    { backendDataIdentity = immediateChoiceIdentity,
      backendDataNameWithIdentity = "ImmediateChoice",
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity = constructors
    }

immediateChoiceDataWithConstructorIdentity :: BackendData
immediateChoiceDataWithConstructorIdentity =
  immediateChoiceData

immediateChoiceDataWithStaleConstructorDisplayIdentity :: BackendData
immediateChoiceDataWithStaleConstructorDisplayIdentity =
  immediateChoiceDataWithConstructors
    [ BackendConstructorWithIdentity
        { backendConstructorIdentity = immediateChoiceConstructorIdentity,
          backendConstructorNameWithIdentity = "$stale_WithStatic",
          backendConstructorForallsWithIdentity = [],
          backendConstructorFieldsWithIdentity = [polyIdTy, intTy],
          backendConstructorResultWithIdentity = immediateChoiceTy
        },
      BackendConstructorWithIdentity
        { backendConstructorIdentity = immediateChoiceOtherIdentity,
          backendConstructorNameWithIdentity = "Other",
          backendConstructorForallsWithIdentity = [],
          backendConstructorFieldsWithIdentity = [],
          backendConstructorResultWithIdentity = immediateChoiceTy
        }
    ]

immediateChoiceIdentity :: SymbolIdentity
immediateChoiceIdentity =
  symbolIdentityFromParts (UniqueIdentity 992022) SymbolType "Main" "ImmediateChoice" Nothing

immediateChoiceConstructorIdentity :: SymbolIdentity
immediateChoiceConstructorIdentity =
  symbolIdentityFromParts (UniqueIdentity 990006) SymbolConstructor "Main" "WithStatic" (Just (SymbolOwnerType immediateChoiceIdentity))

immediateChoiceOtherIdentity :: SymbolIdentity
immediateChoiceOtherIdentity =
  symbolIdentityFromParts (UniqueIdentity 992023) SymbolConstructor "Main" "Other" (Just (SymbolOwnerType immediateChoiceIdentity))

conflictingStaleConstructorIdentity :: SymbolIdentity
conflictingStaleConstructorIdentity =
  renameSymbolDefiningName "$stale_WithStatic" immediateChoiceConstructorIdentity

otherImmediateChoiceConstructorIdentity :: SymbolIdentity
otherImmediateChoiceConstructorIdentity =
  symbolIdentityFromParts (UniqueIdentity 990007) SymbolConstructor "Other" "WithStatic" Nothing

immediateStrictChoiceData :: BackendData
immediateStrictChoiceData =
  BackendDataWithIdentity
    { backendDataIdentity = immediateStrictChoiceIdentity,
      backendDataNameWithIdentity = "ImmediateStrictChoice",
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity =
        [ BackendConstructorWithIdentity
            { backendConstructorIdentity = immediateStrictWithStaticIdentity,
              backendConstructorNameWithIdentity = "WithStrictStatic",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [polyIdTy, recIntTy],
              backendConstructorResultWithIdentity = immediateStrictChoiceTy
            },
          BackendConstructorWithIdentity
            { backendConstructorIdentity = immediateStrictOtherIdentity,
              backendConstructorNameWithIdentity = "StrictOther",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [],
              backendConstructorResultWithIdentity = immediateStrictChoiceTy
            }
        ]
    }

immediateStrictChoiceIdentity :: SymbolIdentity
immediateStrictChoiceIdentity =
  symbolIdentityFromParts (UniqueIdentity 992024) SymbolType "Main" "ImmediateStrictChoice" Nothing

immediateStrictWithStaticIdentity :: SymbolIdentity
immediateStrictWithStaticIdentity =
  symbolIdentityFromParts (UniqueIdentity 992025) SymbolConstructor "Main" "WithStrictStatic" (Just (SymbolOwnerType immediateStrictChoiceIdentity))

immediateStrictOtherIdentity :: SymbolIdentity
immediateStrictOtherIdentity =
  symbolIdentityFromParts (UniqueIdentity 992026) SymbolConstructor "Main" "StrictOther" (Just (SymbolOwnerType immediateStrictChoiceIdentity))

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

programWithFnBoxMainExpr :: BackendExpr -> BackendProgram
programWithFnBoxMainExpr expr =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = fnBoxTy,
                      backendBindingExpr = expr,
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

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

singleBindingProgram :: String -> BackendExpr -> BackendProgram
singleBindingProgram name expr =
  BackendProgramWithIdentity
    { backendProgramModulesWithIdentity =
        [ BackendModuleWithIdentity
            { backendModuleIdentity = singleBindingModuleIdentity,
              backendModuleNameWithIdentity = "Main",
              backendModuleDataWithIdentity = [],
              backendModuleBindingsWithIdentity =
                [ BackendBindingWithMetadata
                    { backendBindingIdentity = singleBindingMainIdentity,
                      backendBindingNameWithMetadata = name,
                      backendBindingTypeWithMetadata = backendExprType expr,
                      backendBindingExprWithMetadata = expr,
                      backendBindingExportedAsMainWithMetadata = True,
                      backendBindingEvidenceParamIndices = Set.empty
                    }
                ]
            }
        ],
      backendProgramMainIdentity = singleBindingMainIdentity,
      backendProgramMainWithIdentity = name
    }

singleBindingModuleIdentity :: SymbolIdentity
singleBindingModuleIdentity =
  symbolIdentityFromParts (UniqueIdentity 2069135) SymbolModule "Main" "Main" Nothing

singleBindingMainIdentity :: SymbolIdentity
singleBindingMainIdentity =
  symbolIdentityFromParts (UniqueIdentity 2069136) SymbolValue "Main" "main" Nothing

programWithEvidenceParamIndices :: [(String, [Int])] -> [BackendBinding] -> BackendProgram
programWithEvidenceParamIndices evidenceParamIndices bindings =
  programWithBindings (map attachEvidence bindings)
  where
    attachEvidence binding =
      binding
        { backendBindingEvidenceParamIndices =
            maybe Set.empty Set.fromList (lookup (backendBindingName binding) evidenceParamIndices)
        }

intLit :: Integer -> BackendExpr
intLit value =
  BackendLit intTy (LInt value)

boolLit :: Bool -> BackendExpr
boolLit value =
  BackendLit boolTy (LBool value)

intTy :: BackendType
intTy =
  literalBackendType (LInt 0)

boolTy :: BackendType
boolTy =
  literalBackendType (LBool False)

stringTy :: BackendType
stringTy =
  literalBackendType (LString "")

charTy :: BackendType
charTy =
  literalBackendType (LChar '\0')

optionTy :: BackendType -> BackendType
optionTy ty =
  BTConWithIdentity (optionIdentity) (BaseTy "Option") (ty :| [])

optionNone :: BackendType -> BackendExpr
optionNone ty =
  BackendConstructWithIdentity (optionTy ty) (optionNoneIdentity) "None" []

optionSome :: BackendType -> [BackendExpr] -> BackendExpr
optionSome ty =
  BackendConstructWithIdentity (optionTy ty) (optionSomeIdentity) "Some"

optionNonePattern :: BackendPattern
optionNonePattern =
  BackendConstructorPatternWithBinderIdentities (optionNoneIdentity) "None" []

optionSomePattern :: [BackendPatternBinder] -> BackendPattern
optionSomePattern =
  BackendConstructorPatternWithBinderIdentities (optionSomeIdentity) "Some"

resultBoxTy :: BackendType
resultBoxTy =
  BTBase (BaseTy "ResultBox")

resultBoxStructuralTy :: BackendType
resultBoxStructuralTy =
  BTMuWithIdentity
    ( typeBinderIdentityFromStructural
        (symbolUniqueIdentity (fixtureSymbolIdentity SymbolType "ResultBox"))
        StructuralSelfBinder
    )
    "$ResultBox_self"
    (singleFieldStructuralBody intTy)

singleFieldStructuralBody :: BackendType -> BackendType
singleFieldStructuralBody fieldTy =
  BTForall "r" Nothing (BTArrow (BTArrow fieldTy (BTVar "r")) (BTVar "r"))

unaryIntTy :: BackendType
unaryIntTy =
  BTArrow intTy intTy

unaryBoolTy :: BackendType
unaryBoolTy =
  BTArrow boolTy boolTy

binaryIntTy :: BackendType
binaryIntTy =
  BTArrow intTy unaryIntTy

higherOrderEvidenceTy :: BackendType
higherOrderEvidenceTy =
  BTArrow unaryIntTy unaryIntTy

fnBoxTy :: BackendType
fnBoxTy =
  BTBaseWithIdentity (fnBoxIdentity) (BaseTy "FnBox")

lazyFieldBoxTy :: BackendType
lazyFieldBoxTy =
  BTBaseWithIdentity (lazyFieldBoxIdentity) (BaseTy "LazyFieldBox")

lazyFieldBoxIdentity :: SymbolIdentity
lazyFieldBoxIdentity =
  symbolIdentityFromParts (UniqueIdentity 992027) SymbolType "Main" "LazyFieldBox" Nothing

lazyFieldBoxPackedIdentity :: SymbolIdentity
lazyFieldBoxPackedIdentity =
  symbolIdentityFromParts (UniqueIdentity 992028) SymbolConstructor "Main" "Packed" (Just (SymbolOwnerType lazyFieldBoxIdentity))

strictBoxTy :: BackendType
strictBoxTy =
  BTBaseWithIdentity (strictBoxIdentity) (BaseTy "StrictBox")

immediateChoiceTy :: BackendType
immediateChoiceTy =
  BTBaseWithIdentity (immediateChoiceIdentity) (BaseTy "ImmediateChoice")

immediateStrictChoiceTy :: BackendType
immediateStrictChoiceTy =
  BTBaseWithIdentity (immediateStrictChoiceIdentity) (BaseTy "ImmediateStrictChoice")

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

runLLVMParityPolicy :: ProgramLLVMNativeParityPolicy -> Spec
runLLVMParityPolicy policy =
  it (describeProgramLLVMNativeParityPolicy policy) $ do
    artifactResult <- loadProgramRuntimeArtifact (parityCaseSource policy)
    case parityInterpreterRuntime policy of
      InterpreterRuntimeSucceeds ->
        assertInterpreterRuntimeResult
          (parityExpectedRuntime policy)
          (artifactResult >>= checkedArtifactRunOutput)

    let backendResult = artifactResult >>= checkedArtifactBackendLLVM
    case parityBackendLLVMAssembly policy of
      BackendLLVMAssemblyRequired -> do
        output <- requireRight backendResult
        forM_ (parityBackendLLVMRequiredFragments policy) $ \fragment ->
          output `shouldSatisfy` isInfixOf fragment
        forM_ (parityBackendLLVMForbiddenFragments policy) $ \fragment ->
          output `shouldNotSatisfy` isInfixOf fragment
        validateLLVMAssembly output
        case parityObjectCode policy of
          ObjectCodeRequired ->
            validateLLVMObjectCode output
          ObjectCodeNotRequired ->
            pure ()
      BackendLLVMAssemblyUnsupported diagnostic ->
        backendResult `shouldSatisfyStringLeft` isInfixOf diagnostic

    case parityNativeRun policy of
      NativeRunRequired -> do
        nativeOutput <- requireRight (artifactResult >>= checkedArtifactNativeLLVM)
        validateLLVMAssembly nativeOutput
        validateLLVMObjectCode nativeOutput
        nativeResult <- runLLVMNativeExecutable nativeOutput
        assertNativeRuntimeResult (parityExpectedRuntime policy) nativeResult
      NativeRunUnsupported diagnostic -> do
        let nativeResult = artifactResult >>= checkedArtifactNativeLLVM
        nativeResult `shouldSatisfyStringLeft` isInfixOf diagnostic

loadProgramRuntimeArtifact :: ProgramMatrixSource -> IO (Either String CheckedProgramArtifact)
loadProgramRuntimeArtifact source =
  case source of
    InlineProgram programText ->
      checkedProgramArtifactFromSource "<inline-test>" programText
    ProgramFile path ->
      checkedProgramArtifactFromFile path

assertInterpreterRuntimeResult :: ProgramRuntimeExpectation -> Either String String -> Expectation
assertInterpreterRuntimeResult expectation result =
  case result of
    Left err ->
      expectationFailure ("unexpected interpreter failure: " ++ take 20000 err)
    Right output ->
      let rendered = stripOptionalTrailingNewline output
       in case expectation of
            ExpectRuntimeValue expectedValue ->
              rendered `shouldBe` expectedValue
            ExpectRuntimePredicate label predicate
              | predicate rendered -> pure ()
              | otherwise ->
                  expectationFailure $
                    "expected interpreter "
                      ++ label
                      ++ ", got: "
                      ++ rendered

stripOptionalTrailingNewline :: String -> String
stripOptionalTrailingNewline output =
  case reverse output of
    '\n' : rest -> reverse rest
    _ -> output

assertNativeRuntimeResult :: ProgramRuntimeExpectation -> NativeRunResult -> Expectation
assertNativeRuntimeResult expectation result =
  case expectation of
    ExpectRuntimeValue expectedValue -> do
      when (nativeRunExitCode result /= ExitSuccess) $
        expectationFailure $
          nativeRunMismatch
            ("expected native process exit success for value " ++ show expectedValue)
            result
      when (nativeRunStdout result /= expectedValue ++ "\n") $
        expectationFailure $
          nativeRunMismatch
            ("expected stdout " ++ show (expectedValue ++ "\n"))
            result
      when (nativeRunStderr result /= "") $
        expectationFailure $
          nativeRunMismatch "expected empty stderr" result
    ExpectRuntimePredicate label predicate -> do
      when (nativeRunExitCode result /= ExitSuccess) $
        expectationFailure $
          nativeRunMismatch
            ("expected native process exit success for predicate " ++ label)
            result
      when (nativeRunStderr result /= "") $
        expectationFailure $
          nativeRunMismatch "expected empty stderr" result
      case stripSingleTrailingNewline (nativeRunStdout result) of
        Nothing ->
          expectationFailure $
            nativeRunMismatch "expected stdout with one trailing newline" result
        Just rendered
          | predicate rendered -> pure ()
          | otherwise ->
              expectationFailure $
                nativeRunMismatch
                  ("expected " ++ label ++ ", got " ++ show rendered)
                  result

stripSingleTrailingNewline :: String -> Maybe String
stripSingleTrailingNewline output =
  case reverse output of
    '\n' : rest -> Just (reverse rest)
    _ -> Nothing

nativeRunMismatch :: String -> NativeRunResult -> String
nativeRunMismatch label result =
  unlines
    [ label,
      "exit code: " ++ show (nativeRunExitCode result),
      "stdout:",
      nativeRunStdout result,
      "stderr:",
      nativeRunStderr result
    ]

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

containsLLVMLineFragments :: [String] -> String -> Bool
containsLLVMLineFragments fragments =
  any (\line -> all (`isInfixOf` line) fragments) . lines

shouldSatisfyLeft :: Either BackendLLVMError String -> (String -> Bool) -> Expectation
shouldSatisfyLeft result predicate =
  case result of
    Left err ->
      renderBackendLLVMError err `shouldSatisfy` predicate
    Right output ->
      expectationFailure ("expected backend LLVM failure, got output:\n" ++ output)

shouldSatisfyStringLeft :: Either String String -> (String -> Bool) -> Expectation
shouldSatisfyStringLeft result predicate =
  case result of
    Left err ->
      err `shouldSatisfy` predicate
    Right output ->
      expectationFailure ("expected backend LLVM failure, got output:\n" ++ output)

renderFixtureBackendProgramLLVM :: BackendProgram -> Either BackendLLVMError String
renderFixtureBackendProgramLLVM =
  renderBackendProgramLLVM

renderFixtureBackendProgramNativeLLVM :: BackendProgram -> Either BackendLLVMError String
renderFixtureBackendProgramNativeLLVM =
  renderBackendProgramNativeLLVM

renderMainIdentifiedBackendProgramLLVM :: BackendProgram -> Either BackendLLVMError String
renderMainIdentifiedBackendProgramLLVM =
  renderBackendProgramLLVM

renderMainIdentifiedBackendProgramNativeLLVM :: BackendProgram -> Either BackendLLVMError String
renderMainIdentifiedBackendProgramNativeLLVM =
  renderBackendProgramNativeLLVM

goldenText :: FilePath -> String -> Expectation
goldenText goldenPath actual = do
  expected <- readFile goldenPath
  length expected `seq` actual `shouldBe` expected
