{-# LANGUAGE LambdaCase #-}

module BackendLLVMSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import Test.Hspec

import LLVMToolSupport
  ( NativeRunResult (..),
    parseExecutableCommand,
    runLLVMNativeExecutable,
    validateLLVMAssembly,
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
import MLF.Program.CLI (emitBackendFile, emitNativeFile)
import Parity.ProgramMatrix
  ( ProgramMatrixCase (..),
    ProgramMatrixExpectation (..),
    ProgramMatrixSource (..),
    ProgramRuntimeCase (..),
    ProgramRuntimeExpectation (..),
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

  describe "IO backend contract" $ do
    it "rejects checked main : IO Unit before LLVM lowering" $ do
      result <- emitBackendSource ioPureUnitMainProgram

      result `shouldFailWithAll` backendIOUnsupportedFragments

    it "rejects primitive IO operations without emitting LLVM" $
      forM_
        [ ("putStrLn", ioPutStrLnMainProgram),
          ("direct __io_pure/__io_bind", ioDirectPrimitiveMainProgram)
        ]
        ( \(_label, programText) -> do
            result <- emitBackendSource programText
            result `shouldFailWithAll` backendIOUnsupportedFragments
        )

    it "rejects pure mains that depend on IO-typed helpers" $ do
      result <- emitBackendSource pureMainIODependencyProgram

      result
        `shouldFailWithAll` [ "IO dependencies are not supported by backend conversion yet",
                              "Main__main -> Main__discard"
                            ]

    it "rejects pure mains that directly reference opaque IO primitives" $ do
      result <- emitBackendSource pureMainDirectIOPrimitiveProgram

      result
        `shouldFailWithAll` [ "IO dependencies are not supported by backend conversion yet",
                              "Main__main -> __io_pure"
                            ]

    it "accepts pure mains when IO-typed bindings are unused" $ do
      output <- requireRight =<< emitBackendSource pureMainUnusedIOProgram

      output `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
      output `shouldNotSatisfy` isInfixOf "__io_"
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

    it "rejects unsupported native string result rendering before native assertions use it" $
      renderBackendProgramNativeLLVM nativeStringResultProgram
        `shouldSatisfyLeft` isInfixOf "String main values are not supported"

    it "rejects source/native entrypoint symbol collisions" $
      renderBackendProgramNativeLLVM nativeMainNameCollisionProgram
        `shouldSatisfyLeft` isInfixOf "reserved native LLVM symbol \"main\""

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

  it "collects local polymorphic closure entries after type application" $ do
    output <- requireRight (renderBackendProgramLLVM localPolymorphicClosureEntryProgram)

    output `shouldSatisfy` isInfixOf "$__mlfp_closure$local_poly"
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM type"
    validateLLVMAssembly output

  it "uses qualified closure entries for directly called type applications" $ do
    output <- requireRight (renderBackendProgramLLVM directPolymorphicClosureCallProgram)

    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_direct_typeapp$"
    output `shouldSatisfy` isInfixOf "$__mlfp_closure$direct_call_poly\""
    output `shouldNotSatisfy` isInfixOf "store ptr @\"__mlfp_closure$direct_call_poly\""
    validateLLVMAssembly output

  it "collects closure entries when direct type applications cross let heads" $ do
    output <- requireRight (renderBackendProgramLLVM letHeadPolymorphicClosureCallProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"polyLocal$"
    output `shouldSatisfy` isInfixOf "store ptr @\"polyLocal$"
    output `shouldSatisfy` isInfixOf "$__mlfp_closure$direct_call_poly\""
    validateLLVMAssembly output

  it "lowers structurally matched closure call results" $ do
    output <- requireRight (renderBackendProgramLLVM structuralClosureResultProgram)

    output `shouldSatisfy` isInfixOf "__mlfp_closure$result_structural"
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

  it "rejects rigid applied type heads during type-argument inference" $ do
    case
      Lower.inferTypeArgumentsForTest
        "rigid application head"
        ["a"]
        [("value", BTVarApp "f" (BTVar "a" :| []))]
        [BackendVar (BTVarApp "g" (intTy :| [])) "value"]
      of
        Left err ->
          Lower.renderBackendLLVMError err `shouldSatisfy` isInfixOf "rigid type application head mismatch"
        Right substitution ->
          expectationFailure ("expected rigid head mismatch, got substitution: " ++ show substitution)

  it "rejects mismatched applied type arguments during type-argument inference" $ do
    case
      Lower.inferTypeArgumentsForTest
        "applied argument mismatch"
        ["f"]
        [("value", BTVarApp "f" (intTy :| []))]
        [BackendVar (BTCon (BaseTy "Box") (boolTy :| [])) "value"]
      of
        Left err ->
          Lower.renderBackendLLVMError err `shouldSatisfy` isInfixOf "type application argument mismatch"
        Right substitution ->
          expectationFailure ("expected applied argument mismatch, got substitution: " ++ show substitution)

  it "statically lowers first-class polymorphic top-level arguments" $ do
    output <- requireRight =<< emitBackendFile "test/programs/unified/first-class-polymorphism.mlfp"

    output `shouldSatisfy` isInfixOf "define i1 @\"FirstClassPolymorphism__main\"()"
    output `shouldNotSatisfy` isInfixOf "FirstClassPolymorphism__usePoly"
    validateLLVMAssembly output
    validateLLVMObjectCode output

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

  it "rejects unsupported static function arguments instead of erasing them" $ do
    renderBackendProgramLLVM staticPartialApplicationArgumentProgram
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
    renderBackendProgramLLVM staticArgumentMainProgram
      `shouldSatisfyLeft` isInfixOf "parameter \"poly\" of main"

  it "rejects recursive static global inlining instead of diverging" $ do
    renderBackendProgramLLVM recursiveStaticGlobalProgram
      `shouldSatisfyLeft` isInfixOf "recursive static global \"loop\""

  it "statically lowers immediate constructor fields carrying forall values" $ do
    output <-
      withTempProgram constructorFirstClassPolymorphismProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define i1 @\"Main__main\"()"
    output `shouldNotSatisfy` isInfixOf "escaping type abstraction"
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

  it "lowers top-level recursive higher-order functions through closure arguments" $ do
    output <-
      withTempProgram sourceTopLevelRecursiveHigherOrderProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define i64 @\"Main__loop\"(ptr %\"$f#"
    output `shouldSatisfy` isInfixOf "ptr %\"$n#"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceTopLevelRecursiveHigherOrderProgram "1"

  it "lowers local recursive higher-order helpers through closure arguments" $ do
    output <-
      withTempProgram sourceLocalRecursiveHigherOrderProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "Main__main$letrec$"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceLocalRecursiveHigherOrderProgram "1"

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

  it "evaluates unused immediate constructor fields before static erasure" $ do
    renderBackendProgramLLVM strictImmediateConstructFieldProgram
      `shouldSatisfyLeft` isInfixOf "representation-changing roll"

  it "evaluates immediate constructor fields before default alternatives" $ do
    renderBackendProgramLLVM strictImmediateDefaultProgram
      `shouldSatisfyLeft` isInfixOf "representation-changing roll"

  it "lowers unmatched immediate constructor cases to unreachable" $ do
    output <- requireRight (renderBackendProgramLLVM unmatchedImmediateConstructorProgram)

    output `shouldSatisfy` isInfixOf "unreachable"
    output `shouldNotSatisfy` isInfixOf "no matching immediate constructor alternative"
    validateLLVMAssembly output

  it "evaluates immediate constructor fields before unmatched alternatives" $ do
    renderBackendProgramLLVM strictImmediateUnmatchedProgram
      `shouldSatisfyLeft` isInfixOf "representation-changing roll"

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
    output <- requireRight (renderBackendProgramLLVM inlineOnlyEvidenceCalleeProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"callee\"(ptr %\"f\")"
    output `shouldSatisfy` isInfixOf "ptr @\"callee\""
    validateLLVMAssembly output

  it "emits referenced callees that call opaque evidence parameters inline-only" $ do
    output <- requireRight (renderBackendProgramLLVM inlineOnlyEvidenceParameterCallCalleeProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"calleeWithEvidenceCall\"(ptr %\"$evidence_apply\")"
    output `shouldSatisfy` isInfixOf "ptr @\"calleeWithEvidenceCall\""
    validateLLVMAssembly output

  it "emits referenced inline-only callees passed through local aliases" $ do
    output <- requireRight (renderBackendProgramLLVM aliasedInlineOnlyEvidenceCalleeProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"calleeWithEvidenceCall\"(ptr %\"$evidence_apply\")"
    output `shouldSatisfy` isInfixOf "ptr @\"calleeWithEvidenceCall\""
    validateLLVMAssembly output

  it "does not collect shadowed local aliases as referenced callees" $ do
    output <- requireRight (renderBackendProgramLLVM shadowedLocalAliasReferenceCollectorProgram)

    output `shouldNotSatisfy` isInfixOf "define i64 @\"shadowedCalleeWithEvidenceCall\""
    validateLLVMAssembly output

  it "lowers nested evidence wrapper parameters as pointers" $ do
    output <- requireRight (renderBackendProgramLLVM nestedEvidenceWrapperParameterProgram)

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

  it "emits self-recursive higher-order calls instead of expanding them inline" $ do
    output <- requireRight (renderBackendProgramLLVM selfRecursiveHigherOrderProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"loop\"(ptr %\"f\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "call i64 @\"loop\"(ptr %\"f\", i64 %\"x\")"
    validateLLVMAssembly output

  it "resolves inlined local function calls before captured values" $ do
    output <- requireRight (renderBackendProgramLLVM localFunctionCallShadowsValueProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldNotSatisfy` isInfixOf "unexpected type arguments"
    validateLLVMAssembly output

  it "resolves local function references before captured values for indirect arguments" $ do
    output <- requireRight (renderBackendProgramLLVM localFunctionReferenceShadowsValueProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"main\"()"
    output `shouldNotSatisfy` isInfixOf "evidence function type mismatch"
    validateLLVMAssembly output

  it "preserves inline function argument shadowing beside closure-valued fields" $ do
    output <- requireRight (renderBackendProgramLLVM inlineFunctionArgumentShadowsValueProgram)

    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_shadow\""
    output `shouldNotSatisfy` isInfixOf "escaping function \"f\""
    validateLLVMAssembly output

  it "rejects evidence wrappers that capture local term bindings" $ do
    renderBackendProgramLLVM capturingEvidenceWrapperProgram
      `shouldSatisfyLeft` isInfixOf "unsupported evidence function argument"

  it "collects evidence wrappers only after polymorphic forms are specialized" $ do
    output <- requireRight (renderBackendProgramLLVM polymorphicEvidenceWrapperProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_evidence_wrapper$"
    output `shouldNotSatisfy` isInfixOf "Unsupported backend LLVM type"
    validateLLVMAssembly output

  it "collects local polymorphic evidence wrappers after type application" $ do
    output <- requireRight (renderBackendProgramLLVM localPolymorphicEvidenceWrapperProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_evidence_wrapper$"
    output `shouldNotSatisfy` isInfixOf "unsupported evidence function argument"
    validateLLVMAssembly output

  describe "ProgramSpec-to-LLVM parity matrix" $ do
    it "classifies every interpreter-success case exactly once" $ do
      let caseNames = map runtimeCaseName programSpecToLLVMParityCases
          uniqueCaseNames = Set.fromList caseNames
          objectCodeNames = Set.fromList llvmObjectCodeParityCases
          nativeUnsupportedNames = Map.keysSet llvmNativeUnsupportedParityCases
          nativeRepresentativeNames = Set.fromList llvmNativeRepresentativeParityCases

      length caseNames `shouldBe` Set.size uniqueCaseNames
      objectCodeNames `Set.isSubsetOf` uniqueCaseNames `shouldBe` True
      nativeUnsupportedNames `Set.isSubsetOf` uniqueCaseNames `shouldBe` True
      nativeRepresentativeNames `Set.isSubsetOf` uniqueCaseNames `shouldBe` True
      nativeRepresentativeNames `Set.disjoint` nativeUnsupportedNames `shouldBe` True
      Map.keysSet llvmParityExpectations `shouldBe` uniqueCaseNames

    mapM_ runLLVMParityCase programSpecToLLVMParityCases

  it "lowers packaged partial applications through the explicit closure ABI" $ do
    output <- requireRight (renderBackendProgramLLVM partialApplicationProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$addOne\""
    output `shouldSatisfy` isInfixOf "store i64 1"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "rejects raw escaping lambdas until closure construction is explicit in backend IR" $ do
    renderBackendProgramLLVM escapingLambdaProgram
      `shouldSatisfyLeft` isInfixOf "escaping function"

  it "packages source-level top-level partial applications as closure values" $ do
    output <-
      withTempProgram sourceTopLevelPartialApplicationProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "__mlfp_closure$Main__main$Main__keepLeft$partial"
    output `shouldSatisfy` isInfixOf "store i64 1"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceTopLevelPartialApplicationProgram "1"

  it "packages source-level local partial applications as closure values" $ do
    output <-
      withTempProgram sourceLocalPartialApplicationProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceLocalPartialApplicationProgram "1"

  it "packages partial applications with closure-valued supplied arguments" $ do
    output <-
      withTempProgram sourcePartialApplicationClosureArgumentProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationClosureArgumentProgram "41"

  it "packages partial applications with global closure-valued supplied arguments" $ do
    output <-
      withTempProgram sourcePartialApplicationGlobalClosureArgumentProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationGlobalClosureArgumentProgram "41"

  it "packages partial applications headed by closure-valued parameters" $ do
    output <-
      withTempProgram sourcePartialApplicationClosureParameterProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "BackendClosureCallExpectedClosureValue"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationClosureParameterProgram "1"

  it "tracks partial closure-valued argument demand through top-level aliases" $ do
    output <-
      withTempProgram sourcePartialApplicationClosureDemandAliasProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationClosureDemandAliasProgram "1"

  it "tracks partial closure-valued argument demand through wrapped aliases" $ do
    output <-
      withTempProgram sourcePartialApplicationClosureDemandWrappedAliasProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationClosureDemandWrappedAliasProgram "1"

  it "offsets propagated closure-demand indices through eta-expanded aliases" $ do
    output <-
      withTempProgram sourcePartialApplicationClosureDemandEtaAliasProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationClosureDemandEtaAliasProgram "1"

  it "tracks partial closure-valued argument demand for local helpers" $ do
    output <-
      withTempProgram sourcePartialApplicationLocalClosureDemandProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationLocalClosureDemandProgram "1"

  it "wraps closure-demanded arguments after evidence arguments" $ do
    output <-
      withTempProgram sourceClosureDemandAfterEvidenceProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    output `shouldNotSatisfy` isInfixOf "BackendClosureCallExpectedClosureValue"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceClosureDemandAfterEvidenceProgram "1"

  it "packages constrained partial applications after hidden evidence" $ do
    output <-
      withTempProgram sourceConstrainedPartialApplicationProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceConstrainedPartialApplicationProgram "1"

  it "packages constrained partial applications through constrained aliases" $ do
    output <-
      withTempProgram sourceConstrainedPartialApplicationAliasProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldNotSatisfy` isInfixOf "Backend LLVM arity mismatch"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceConstrainedPartialApplicationAliasProgram "1"

  it "freshens generated partial capture names against local binders" $ do
    output <-
      withTempProgram sourcePartialApplicationGeneratedNameCollisionProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldNotSatisfy` isInfixOf "BackendDuplicateClosureCapture"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationGeneratedNameCollisionProgram "1"

  it "wraps direct function arguments before packaging partial applications" $ do
    output <-
      withTempProgram sourcePartialApplicationDirectFunctionArgumentProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationDirectFunctionArgumentProgram "4"

  it "wraps closure-demanded arguments for let-headed call aliases" $ do
    output <-
      withTempProgram sourceClosureDemandLetHeadedCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceClosureDemandLetHeadedCallProgram "1"

  it "wraps closure-demanded arguments for eta-expanded call heads" $ do
    output <-
      withTempProgram sourceClosureDemandEtaCallHeadProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldNotSatisfy` isInfixOf "store ptr @\"Main__keepLeft\""
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceClosureDemandEtaCallHeadProgram "1"

  it "keeps polymorphic supplied partial arguments on the static specialization path" $ do
    output <-
      withTempProgram sourcePartialApplicationPolymorphicArgumentProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldNotSatisfy` isInfixOf "escaping polymorphic binding"
    output `shouldNotSatisfy` isInfixOf "Main__usePoly$partial"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationPolymorphicArgumentProgram "true"

  it "keeps higher-rank supplied partial functions on the static specialization path" $ do
    output <-
      withTempProgram sourcePartialApplicationHigherRankFunctionArgumentProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldNotSatisfy` isInfixOf "escaping function value"
    output `shouldNotSatisfy` isInfixOf "Main__useHigher$partial"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationHigherRankFunctionArgumentProgram "1"

  it "captures locals when wrapping demanded inline function arguments" $ do
    output <-
      withTempProgram sourceClosureDemandedInlineFunctionArgumentProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "unsupported static function argument"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceClosureDemandedInlineFunctionArgumentProgram "41"

  it "captures locals for non-variable partial callees" $ do
    output <-
      withTempProgram sourcePartialApplicationNonVariableCalleeProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "$partial"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "Backend LLVM arity mismatch"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourcePartialApplicationNonVariableCalleeProgram "1"

  it "lowers source-level captured closure calls through the explicit closure ABI" $ do
    output <-
      withTempProgram sourceCapturedClosureCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$Main__main$"
    output `shouldSatisfy` isInfixOf "store i64 41"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers closure-valued function parameters through the explicit closure ABI" $ do
    output <-
      withTempProgram sourceFunctionParameterClosureCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$Main__main$"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.malloc"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers closure-valued function parameters through let aliases" $ do
    output <-
      withTempProgram sourceFunctionParameterClosureAliasCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$Main__main$"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "BackendClosureCallExpectedClosureValue"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers source-level returned closure values as pointer results" $ do
    output <-
      withTempProgram sourceReturnedClosureProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define ptr @\"Main__main\"()"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$Main__main$"
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
    output <-
      withTempProgram sourceLocalReturnedClosureCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$Main__main$"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "Backend LLVM arity mismatch"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceLocalReturnedClosureCallProgram "41"

  it "lowers let-bound returned closure records through the explicit closure ABI" $ do
    output <-
      withTempProgram sourceLetBoundReturnedClosureRecordCallProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$Main__main$"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "call i64 %\"__llvm.malloc"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceLetBoundReturnedClosureRecordCallProgram "41"

  it "lowers direct returned closure applications through the explicit closure ABI" $ do
    output <-
      withTempProgram sourceDirectReturnedClosureApplicationProgram $ \path ->
        requireRight =<< emitBackendFile path

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$Main__main$"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "Backend LLVM arity mismatch"
    validateLLVMAssembly output
    validateLLVMObjectCode output
    assertNativeProgram sourceDirectReturnedClosureApplicationProgram "41"

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

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$Main__main$"
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

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$Main__main$"
    output `shouldSatisfy` isInfixOf "store i64 41"
    output `shouldSatisfy` isInfixOf "load ptr"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    output `shouldNotSatisfy` isInfixOf "closure-valued constructor field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "lowers zero-capture closures through the explicit closure ABI" $ do
    output <- requireRight (renderBackendProgramLLVM zeroCaptureClosureProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$identity\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$identity\""
    output `shouldSatisfy` isInfixOf "load ptr"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output

  it "lowers captured local values through closure environments" $ do
    output <- requireRight (renderBackendProgramLLVM capturedClosureProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$constCaptured\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "store i64 41"
    output `shouldSatisfy` isInfixOf "load i64"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output

  it "lowers case-selected closure callees through the explicit closure ABI" $ do
    output <- requireRight (renderBackendProgramLLVM caseSelectedClosureCalleeProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$case_some\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$case_none\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "phi ptr"
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output

  it "lowers let-selected closure callees through the explicit closure ABI" $ do
    output <- requireRight (renderBackendProgramLLVM letSelectedClosureCalleeProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$let_callee\"(ptr %\"__mlfp_env\", i64 %\"x\")"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$let_callee\""
    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.code."
    validateLLVMAssembly output

  it "qualifies closure entry names emitted from type specializations" $ do
    output <- requireRight (renderBackendProgramLLVM polymorphicClosureSpecializationProgram)

    let closureDefinitions =
          filter
            (\line -> "define private i64 @\"poly$t" `isInfixOf` line && "$__mlfp_closure$poly\"" `isInfixOf` line)
            (lines output)
    length closureDefinitions `shouldBe` 2
    output `shouldNotSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$poly\""
    output `shouldNotSatisfy` isInfixOf "store ptr @\"__mlfp_closure$poly\""
    validateLLVMAssembly output

  it "keeps closure-valued constructor fields on the closure ABI across specializations" $ do
    output <- requireRight (renderBackendProgramLLVM polymorphicClosureFunctionWrapperProgram)

    output `shouldSatisfy` isInfixOf "$__mlfp_closure$wrapper_key"
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "qualifies closure entries in inlined polymorphic global calls" $ do
    output <- requireRight (renderBackendProgramLLVM inlinePolymorphicClosureProgram)

    let qualifiedStores =
          filter
            (\line -> "store ptr @\"polyInline$t" `isInfixOf` line && "$__mlfp_closure$inline\"" `isInfixOf` line)
            (lines output)
    length qualifiedStores `shouldBe` 1
    output `shouldNotSatisfy` isInfixOf "store ptr @\"__mlfp_closure$inline\""
    validateLLVMAssembly output

  it "does not reserve retired stored-function wrapper names without generated wrappers" $ do
    output <- requireRight (renderBackendProgramLLVM closureEntryFunctionWrapperCollisionProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_function_wrapper$0\""
    output `shouldNotSatisfy` isInfixOf "Duplicate backend LLVM symbol"
    validateLLVMAssembly output

  it "rejects closure entry names that collide with runtime declarations" $
    renderBackendProgramLLVM closureEntryRuntimeDeclarationCollisionProgram
      `shouldSatisfyLeft` isInfixOf "Duplicate backend LLVM symbol: \"malloc\""

  it "lowers stored explicit closure constructor fields" $ do
    output <- requireRight (renderBackendProgramLLVM functionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_top\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_top\""
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "lowers stored direct closure constructor fields through closure records" $ do
    output <- requireRight (renderBackendProgramLLVM directFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_direct\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_direct\""
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "lowers stored local closure constructor fields through pointer aliases" $ do
    output <- requireRight (renderBackendProgramLLVM localFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_local\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_local\""
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "lowers stored transitive local closure aliases through pointer aliases" $ do
    output <- requireRight (renderBackendProgramLLVM transitiveLocalFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_transitive\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_transitive\""
    output `shouldNotSatisfy` isInfixOf "__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "re-stores case-projected constructor fields carrying closures" $ do
    output <- requireRight (renderBackendProgramLLVM immediateRestoredFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_restored\""
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_restored\""
    output `shouldSatisfy` isInfixOf "load ptr"
    output `shouldNotSatisfy` isInfixOf "unsupported function argument"
    validateLLVMAssembly output

  it "lowers captured function constructor fields through closure environments" $ do
    output <- requireRight (renderBackendProgramLLVM capturedFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_closure$field_captured\""
    output `shouldSatisfy` isInfixOf "store i64 1"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_closure$field_captured\""
    output `shouldNotSatisfy` isInfixOf "unsupported function argument"
    validateLLVMAssembly output

  it "preserves stored function-pointer captures on the indirect call path" $ do
    output <- requireRight (renderBackendProgramLLVM capturedFunctionPointerCallProgram)

    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.closure.env.field"
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.closure.env.field"
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "routes over-applied raw function-pointer results through indirect calls" $ do
    output <- requireRight (renderBackendProgramLLVM rawReturnedFunctionPointerCallProgram)

    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.call."
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.call."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "preserves let-bound raw function-pointer aliases as values" $ do
    output <- requireRight (renderBackendProgramLLVM rawFunctionPointerAliasReturnProgram)

    output `shouldSatisfy` isInfixOf "call i64 %\"__llvm.call."
    output `shouldNotSatisfy` isInfixOf "escaping function"
    output `shouldNotSatisfy` isInfixOf "closure.code.ptr\" %\"__llvm.call."
    validateLLVMAssembly output
    validateLLVMObjectCode output

  it "guards nullary global value-kind cycles" $ do
    result <- timeout 1000000 (evaluate (renderBackendProgramLLVM nullaryGlobalValueKindCycleProgram))
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

assertNativeProgram :: String -> String -> Expectation
assertNativeProgram programText expectedValue = do
  output <- requireRight =<< emitNativeSource programText
  output `shouldSatisfy` isInfixOf "define i32 @\"main\"()"
  output `shouldSatisfy` isInfixOf "declare i32 @\"printf\"(ptr, ...)"
  validateLLVMAssembly output
  validateLLVMObjectCode output
  runLLVMNativeExecutable output
    `shouldReturn` NativeRunResult ExitSuccess (expectedValue ++ "\n") ""

emitNativeSource :: String -> IO (Either String String)
emitNativeSource programText =
  withTempProgram programText emitNativeFile

emitBackendSource :: String -> IO (Either String String)
emitBackendSource programText =
  withTempProgram programText emitBackendFile

shouldFailWithAll :: Either String String -> [String] -> Expectation
shouldFailWithAll result fragments =
  case result of
    Left message ->
      mapM_ (\fragment -> message `shouldSatisfy` isInfixOf fragment) fragments
    Right output ->
      expectationFailure ("expected backend failure, got output:\n" ++ output)

backendIOUnsupportedFragments :: [String]
backendIOUnsupportedFragments =
  [ "Backend LLVM conversion failed",
    "Unsupported backend conversion shape",
    "IO programs are not supported by backend conversion yet"
  ]

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
      "  def main : IO Unit = __io_bind (__io_pure Unit) (\\(_done : Unit) __io_putStrLn \"world\");",
      "}"
    ]

pureMainIODependencyProgram :: String
pureMainIODependencyProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO, pure);",
      "  def discard : IO Unit -> Unit = \\(_action : IO Unit) Unit;",
      "  def main : Unit = discard (pure Unit);",
      "}"
    ]

pureMainDirectIOPrimitiveProgram :: String
pureMainDirectIOPrimitiveProgram =
  unlines
    [ "module Main export (main) {",
      "  import Prelude exposing (Unit(..), IO);",
      "  def main : Unit = (\\(_action : IO Unit) Unit) (__io_pure Unit);",
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

localFirstClassPolymorphismProgram :: String
localFirstClassPolymorphismProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Bool =",
      "    let usePoly : (forall a. a -> a) -> Bool =",
      "      \\(poly : forall a. a -> a) let keepInt = poly 1 in poly true",
      "    in let id : forall a. a -> a = \\x x in usePoly id;",
      "}"
    ]

sourceEscapingPolymorphicMainProgram :: String
sourceEscapingPolymorphicMainProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : forall a. a -> a = \\x x;",
      "}"
    ]

firstClassFunctionArgumentProgram :: String
firstClassFunctionArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = \\(f : Int -> Int) f 1;",
      "  def main : Int = use (\\(x : Int) x);",
      "}"
    ]

typeAppliedGlobalStaticAliasProgram :: String
typeAppliedGlobalStaticAliasProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : forall a. a -> a = \\x x;",
      "  def use : (Int -> Int) -> Int = \\(f : Int -> Int) f 1;",
      "  def main : Int = use id;",
      "}"
    ]

constructorFirstClassPolymorphismProgram :: String
constructorFirstClassPolymorphismProgram =
  unlines
    [ "module Main export (PolyBox(..), main) {",
      "  data PolyBox =",
      "      PolyBox : (forall a. a -> a) -> PolyBox;",
      "",
      "  def main : Bool = case PolyBox (\\x x) of {",
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
    [ BackendBinding
        { backendBindingName = "loop",
          backendBindingType = staticPolyBoolTy,
          backendBindingExpr =
            BackendLam
              staticPolyBoolTy
              "poly"
              polyIdTy
              ( BackendApp
                  boolTy
                  (BackendVar staticPolyBoolTy "loop")
                  (BackendVar polyIdTy "poly")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = boolTy,
          backendBindingExpr =
            BackendApp
              boolTy
              (BackendVar staticPolyBoolTy "loop")
              polyIdExpr,
          backendBindingExportedAsMain = True
        }
    ]

ordinaryFunctionEvidenceMethodProgram :: String
ordinaryFunctionEvidenceMethodProgram =
  unlines
    [ "module Main export (C, apply, idInt, use, main) {",
      "  class C a {",
      "    apply : (a -> a) -> a -> a;",
      "  }",
      "",
      "  instance C Int {",
      "    apply = \\f \\x f x;",
      "  }",
      "",
      "  def idInt : Int -> Int = \\x x;",
      "  def use : C a => (a -> a) -> a -> a = \\f \\x apply f x;",
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
      "  def idInt : Int -> Int = \\(x : Int) x;",
      "  def loop : (Int -> Int) -> Nat -> Int = \\(f : Int -> Int) \\(n : Nat) case n of {",
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
      "    let idInt : Int -> Int = \\(x : Int) x in",
      "    let loop : (Int -> Int) -> Nat -> Int = \\(f : Int -> Int) \\(n : Nat) case n of {",
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
      "    apply = \\f \\x f x;",
      "  }",
      "",
      "  def use : C a => a -> a = \\x let f : a -> a = \\y y in apply f x;",
      "  def main : Int = use 1;",
      "}"
    ]

selfRecursiveHigherOrderProgram :: BackendProgram
selfRecursiveHigherOrderProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "id",
          backendBindingType = unaryIntTy,
          backendBindingExpr = intIdentityExpr,
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "loop",
          backendBindingType = BTArrow unaryIntTy unaryIntTy,
          backendBindingExpr =
            BackendLam
              (BTArrow unaryIntTy unaryIntTy)
              "f"
              unaryIntTy
              ( BackendLam
                  unaryIntTy
                  "x"
                  intTy
                  ( BackendApp
                      intTy
                      ( BackendApp
                          unaryIntTy
                          (BackendVar (BTArrow unaryIntTy unaryIntTy) "loop")
                          (BackendVar unaryIntTy "f")
                      )
                      (BackendVar intTy "x")
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
                  unaryIntTy
                  (BackendVar (BTArrow unaryIntTy unaryIntTy) "loop")
                  (BackendVar unaryIntTy "id")
              )
              (intLit 1),
          backendBindingExportedAsMain = True
        }
    ]

inlineOnlyEvidenceCalleeProgram :: BackendProgram
inlineOnlyEvidenceCalleeProgram =
  programWithBindings
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
                  (BackendVar unaryIntTy "id")
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
                  (BackendVar (BTArrow unaryIntTy intTy) "callee")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (BackendVar (BTArrow (BTArrow (BTArrow unaryIntTy intTy) intTy) intTy) "caller")
              (BackendVar (BTArrow (BTArrow unaryIntTy intTy) intTy) "$evidence_C"),
          backendBindingExportedAsMain = True
        }
    ]

inlineOnlyEvidenceParameterCallCalleeProgram :: BackendProgram
inlineOnlyEvidenceParameterCallCalleeProgram =
  programWithBindings
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
                  (BackendVar unaryIntTy "id")
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
                  (BackendVar higherOrderEvidenceTy "$evidence_apply")
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
                  (BackendVar (BTArrow higherOrderEvidenceTy intTy) "calleeWithEvidenceCall")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (BackendVar (BTArrow (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) intTy) "caller")
              (BackendVar (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) "$evidence_C"),
          backendBindingExportedAsMain = True
        }
    ]

aliasedInlineOnlyEvidenceCalleeProgram :: BackendProgram
aliasedInlineOnlyEvidenceCalleeProgram =
  programWithBindings
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
                  (BackendVar unaryIntTy "id")
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
                  (BackendVar higherOrderEvidenceTy "$evidence_apply")
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
                  (BackendVar (BTArrow higherOrderEvidenceTy intTy) "calleeWithEvidenceCall")
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
              (BackendVar (BTArrow (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) intTy) "caller")
              (BackendVar (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) "$evidence_C"),
          backendBindingExportedAsMain = True
        }
    ]

shadowedLocalAliasReferenceCollectorProgram :: BackendProgram
shadowedLocalAliasReferenceCollectorProgram =
  programWithBindings
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
                  (BackendVar unaryIntTy "id")
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
                  (BackendVar unaryIntTy "id")
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
                  (BackendVar higherOrderEvidenceTy "$evidence_apply")
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
              (BackendVar (BTArrow higherOrderEvidenceTy intTy) "shadowedCalleeWithEvidenceCall")
              ( BackendApp
                  intTy
                  ( BackendLam
                      (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy)
                      "$evidence_method"
                      (BTArrow higherOrderEvidenceTy intTy)
                      ( BackendApp
                          intTy
                          (BackendVar (BTArrow (BTArrow higherOrderEvidenceTy intTy) intTy) "$evidence_C")
                          (BackendVar (BTArrow higherOrderEvidenceTy intTy) "$evidence_method")
                      )
                  )
                  (BackendVar (BTArrow higherOrderEvidenceTy intTy) "actualCalleeWithEvidenceCall")
              ),
          backendBindingExportedAsMain = True
        }
    ]

capturingEvidenceWrapperProgram :: BackendProgram
capturingEvidenceWrapperProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow unaryIntTy intTy,
          backendBindingExpr =
            BackendLam
              { backendExprType = BTArrow unaryIntTy intTy,
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
                  (BackendVar (BTArrow unaryIntTy intTy) "$evidence_C")
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

nestedEvidenceWrapperParameterProgram :: BackendProgram
nestedEvidenceWrapperParameterProgram =
  programWithBindings
    [ BackendBinding
        { backendBindingName = "$evidence_C",
          backendBindingType = BTArrow higherOrderEvidenceTy intTy,
          backendBindingExpr =
            BackendLam
              { backendExprType = BTArrow higherOrderEvidenceTy intTy,
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
              (BackendVar (BTArrow higherOrderEvidenceTy intTy) "$evidence_C")
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
  programWithBindings
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
              (BackendVar polyEvidenceCallerTy "poly")
              intTy,
          backendBindingExportedAsMain = True
        }
    ]

localPolymorphicEvidenceWrapperProgram :: BackendProgram
localPolymorphicEvidenceWrapperProgram =
  programWithBindings
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
            (BackendVar polyEvidenceConsumerTy "$evidence_C")
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
                  (BackendVar (BTArrow polyIdTy intTy) "callee")
                  (BackendVar polyIdTy "inner")
              ),
          backendBindingExportedAsMain = False
        },
      BackendBinding
        { backendBindingName = "main",
          backendBindingType = intTy,
          backendBindingExpr =
            BackendApp
              intTy
              (BackendVar (BTArrow unaryIntTy intTy) "outer")
              (BackendVar unaryIntTy "id"),
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
                      (BackendVar unaryBoolTy "idBool")
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
                  (BackendVar (BTArrow unaryIntTy (BTArrow (BTArrow unaryBoolTy intTy) intTy)) "outer")
                  (BackendVar unaryIntTy "idInt")
              )
              (BackendVar (BTArrow unaryBoolTy intTy) "callBool"),
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
                          (BackendConstruct fnBoxTy "FnBox" [closureWithEntry "__mlfp_closure$field_shadow"]),
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
                              (BackendVar (BTArrow unaryIntTy fnBoxTy) "callee")
                              (BackendVar unaryIntTy "idInt")
                          ),
                      backendBindingExportedAsMain = False
                    },
                  BackendBinding
                    { backendBindingName = "main",
                      backendBindingType = fnBoxTy,
                      backendBindingExpr =
                        BackendApp
                          fnBoxTy
                          (BackendVar (BTArrow unaryIntTy fnBoxTy) "outer")
                          (BackendVar unaryIntTy "idInt"),
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

nativeStringResultProgram :: BackendProgram
nativeStringResultProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "Main__main",
                      backendBindingType = stringTy,
                      backendBindingExpr = BackendLit stringTy (LString "hello"),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "Main__main"
    }

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
            ( BackendClosure
                { backendExprType = BTArrow (BTVar "a") (BTVar "a"),
                  backendClosureEntryName = "__mlfp_closure$local_poly",
                  backendClosureCaptures = [],
                  backendClosureParams = [("y", BTVar "a")],
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
            ( BackendClosure
                { backendExprType = BTArrow (BTVar "a") (BTVar "a"),
                  backendClosureEntryName = "__mlfp_closure$direct_call_poly",
                  backendClosureCaptures = [],
                  backendClosureParams = [("y", BTVar "a")],
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
                          ( BackendClosure
                              { backendExprType = BTArrow intTy resultBoxTy,
                                backendClosureEntryName = "__mlfp_closure$result_structural",
                                backendClosureCaptures = [],
                                backendClosureParams = [("x", intTy)],
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
                          ( BackendConstruct
                              strictBoxTy
                              "StrictBox"
                              [polyIdExpr, BackendRoll recIntTy (intLit 1)]
                          )
                          ( BackendAlternative
                              (BackendConstructorPattern "StrictBox" ["poly", "unused"])
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
                          ( BackendConstruct
                              strictBoxTy
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
                          ( BackendConstruct
                              immediateChoiceTy
                              "WithStatic"
                              [polyIdExpr, intLit 1]
                          )
                          (BackendAlternative (BackendConstructorPattern "Other" []) (intLit 0) :| []),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

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
                          ( BackendConstruct
                              immediateStrictChoiceTy
                              "WithStrictStatic"
                              [polyIdExpr, BackendRoll recIntTy (intLit 1)]
                          )
                          (BackendAlternative (BackendConstructorPattern "StrictOther" []) (intLit 0) :| []),
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

resultBoxData :: BackendData
resultBoxData =
  BackendData
    { backendDataName = "ResultBox",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "ResultBox" [] [intTy] resultBoxTy]
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

staticPolyBoolTy :: BackendType
staticPolyBoolTy =
  BTArrow polyIdTy boolTy

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
          backendBindingType = intTy,
          backendBindingExpr =
            BackendLet
              { backendExprType = intTy,
                backendLetName = "addOne",
                backendLetType = unaryIntTy,
                backendLetRhs =
                  BackendClosure
                    { backendExprType = unaryIntTy,
                      backendClosureEntryName = "__mlfp_closure$addOne",
                      backendClosureCaptures = [BackendClosureCapture "__mlfp_partial_capture0" intTy (intLit 1)],
                      backendClosureParams = [("__mlfp_partial_arg0", intTy)],
                      backendClosureBody =
                        BackendApp
                          { backendExprType = intTy,
                            backendFunction =
                              BackendApp
                                { backendExprType = unaryIntTy,
                                  backendFunction = BackendVar binaryIntTy "add",
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
            BackendLam
              { backendExprType = BTArrow unaryIntTy intTy,
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
                backendFunction = BackendVar (BTArrow unaryIntTy intTy) "use",
                backendArgument =
                  BackendApp
                    { backendExprType = unaryIntTy,
                      backendFunction = BackendVar binaryIntTy "add",
                      backendArgument = intLit 1
                    }
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

sourceCapturedClosureCallProgram :: String
sourceCapturedClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = \\(x : Int) captured in",
      "    let g : Int -> Int = f in",
      "    g 0;",
      "}"
    ]

sourceFunctionParameterClosureCallProgram :: String
sourceFunctionParameterClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = \\(f : Int -> Int) f 1;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = \\(x : Int) captured in",
      "    use f;",
      "}"
    ]

sourceFunctionParameterClosureAliasCallProgram :: String
sourceFunctionParameterClosureAliasCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = \\(f : Int -> Int) let g : Int -> Int = f in g 1;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = \\(x : Int) captured in",
      "    use f;",
      "}"
    ]

sourceReturnedClosureProgram :: String
sourceReturnedClosureProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int -> Int =",
      "    let captured : Int = 41 in \\(x : Int) captured;",
      "}"
    ]

sourceTopLevelClosureCallProgram :: String
sourceTopLevelClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def maker : Int -> Int = let captured : Int = 41 in \\(x : Int) captured;",
      "  def main : Int = maker 0;",
      "}"
    ]

sourceLocalReturnedClosureCallProgram :: String
sourceLocalReturnedClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    let make : Int -> (Int -> Int) =",
      "      \\(base : Int) let captured : Int = base in \\(x : Int) captured in",
      "    (make 41) 0;",
      "}"
    ]

sourceLetBoundReturnedClosureRecordCallProgram :: String
sourceLetBoundReturnedClosureRecordCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    let f : Int -> Int =",
      "      ((\\(base : Int) let captured : Int = base in \\(x : Int) captured) 41) in",
      "    f 0;",
      "}"
    ]

sourceDirectReturnedClosureApplicationProgram :: String
sourceDirectReturnedClosureApplicationProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    (\\(base : Int) let captured : Int = base in \\(x : Int) captured) 41 0;",
      "}"
    ]

sourceTopLevelPartialApplicationProgram :: String
sourceTopLevelPartialApplicationProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def main : Int = apply (keepLeft 1);",
      "}"
    ]

sourceLocalPartialApplicationProgram :: String
sourceLocalPartialApplicationProgram =
  unlines
    [ "module Main export (main) {",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def main : Int =",
      "    let keepLeft : Int -> Int -> Int = \\x \\y x",
      "    in apply (keepLeft 1);",
      "}"
    ]

sourcePartialApplicationClosureArgumentProgram :: String
sourcePartialApplicationClosureArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def choose : (Int -> Int) -> Int -> Int -> Int = \\f \\ignored \\x f x;",
      "  def apply : (Int -> Int) -> Int = \\f f 4;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let inc : Int -> Int = \\(x : Int) captured in",
      "    apply (choose inc 0);",
      "}"
    ]

sourcePartialApplicationGlobalClosureArgumentProgram :: String
sourcePartialApplicationGlobalClosureArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def choose : (Int -> Int) -> Int -> Int -> Int = \\f \\ignored \\x f x;",
      "  def apply : (Int -> Int) -> Int = \\f f 4;",
      "  def globalInc : Int -> Int =",
      "    let captured : Int = 41 in",
      "    \\(x : Int) captured;",
      "  def main : Int = apply (choose globalInc 0);",
      "}"
    ]

sourcePartialApplicationClosureParameterProgram :: String
sourcePartialApplicationClosureParameterProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def use : (Int -> Int -> Int) -> Int = \\f apply (f 1);",
      "  def main : Int = use keepLeft;",
      "}"
    ]

sourcePartialApplicationClosureDemandAliasProgram :: String
sourcePartialApplicationClosureDemandAliasProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def use : (Int -> Int -> Int) -> Int = \\f apply (f 1);",
      "  def useAlias : (Int -> Int -> Int) -> Int = use;",
      "  def main : Int = useAlias keepLeft;",
      "}"
    ]

sourcePartialApplicationClosureDemandWrappedAliasProgram :: String
sourcePartialApplicationClosureDemandWrappedAliasProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def use : (Int -> Int -> Int) -> Int = \\f apply (f 1);",
      "  def useWrapped : (Int -> Int -> Int) -> Int =",
      "    let f : (Int -> Int -> Int) -> Int = use in f;",
      "  def main : Int = useWrapped keepLeft;",
      "}"
    ]

sourcePartialApplicationClosureDemandEtaAliasProgram :: String
sourcePartialApplicationClosureDemandEtaAliasProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def use : (Int -> Int -> Int) -> Int = \\f apply (f 1);",
      "  def useAfter : Int -> (Int -> Int -> Int) -> Int = \\n use;",
      "  def main : Int = useAfter 0 keepLeft;",
      "}"
    ]

sourcePartialApplicationLocalClosureDemandProgram :: String
sourcePartialApplicationLocalClosureDemandProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def main : Int =",
      "    let use : (Int -> Int -> Int) -> Int = \\f apply (f 1)",
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
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def use : Marker Bool => (Int -> Int -> Int) -> Int = \\f apply (f 1);",
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
      "    pick = \\x \\y x;",
      "  }",
      "  def keep : Pick Int => Int -> Int -> Int = \\x \\y pick x y;",
      "  def apply : (Int -> Int) -> Int = \\f f 1;",
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
      "    pick = \\x \\y x;",
      "  }",
      "  def keep : Pick Int => Int -> Int -> Int = \\x \\y pick x y;",
      "  def keepAlias : Pick Int => Int -> Int -> Int = keep;",
      "  def apply : (Int -> Int) -> Int = \\f f 1;",
      "  def main : Int = apply (keepAlias 1);",
      "}"
    ]

sourcePartialApplicationGeneratedNameCollisionProgram :: String
sourcePartialApplicationGeneratedNameCollisionProgram =
  unlines
    [ "module Main export (main) {",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def main : Int =",
      "    let __mlfp_partial_capture0 : Int -> Int -> Int = \\x \\y x in",
      "    apply (__mlfp_partial_capture0 1);",
      "}"
    ]

sourcePartialApplicationDirectFunctionArgumentProgram :: String
sourcePartialApplicationDirectFunctionArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def choose : (Int -> Int -> Int) -> Int -> Int -> Int = \\f \\ignored \\x f x ignored;",
      "  def apply : (Int -> Int) -> Int = \\f f 4;",
      "  def main : Int = apply (choose keepLeft 0);",
      "}"
    ]

sourceClosureDemandLetHeadedCallProgram :: String
sourceClosureDemandLetHeadedCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def use : (Int -> Int -> Int) -> Int = \\f apply (f 1);",
      "  def main : Int =",
      "    (let f : (Int -> Int -> Int) -> Int = use in f) keepLeft;",
      "}"
    ]

sourceClosureDemandEtaCallHeadProgram :: String
sourceClosureDemandEtaCallHeadProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = \\x \\y x;",
      "  def apply : (Int -> Int) -> Int = \\f f 2;",
      "  def use : (Int -> Int -> Int) -> Int = \\f apply (f 1);",
      "  def main : Int = (\\(u : Int -> Int -> Int) use u) keepLeft;",
      "}"
    ]

sourcePartialApplicationPolymorphicArgumentProgram :: String
sourcePartialApplicationPolymorphicArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : forall a. a -> a = \\x x;",
      "  def usePoly : (forall a. a -> a) -> Int -> Bool =",
      "    \\poly \\ignored let keepInt : Int = poly 1 in poly true;",
      "  def apply : (Int -> Bool) -> Bool = \\f f 0;",
      "  def main : Bool = apply (usePoly id);",
      "}"
    ]

sourcePartialApplicationHigherRankFunctionArgumentProgram :: String
sourcePartialApplicationHigherRankFunctionArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : forall a. a -> a = \\x x;",
      "  def idScore : (forall a. a -> a) -> Int = \\poly poly 1;",
      "  def useHigher : ((forall a. a -> a) -> Int) -> Int -> Int =",
      "    \\score \\ignored score id;",
      "  def apply : (Int -> Int) -> Int = \\f f 0;",
      "  def main : Int = apply (useHigher idScore);",
      "}"
    ]

sourceClosureDemandedInlineFunctionArgumentProgram :: String
sourceClosureDemandedInlineFunctionArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def choose : (Int -> Int -> Int) -> Int -> Int -> Int = \\f \\ignored \\x f x ignored;",
      "  def apply : (Int -> Int) -> Int = \\f f 4;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let use : (Int -> Int -> Int) -> Int = \\fn apply (choose fn 0) in",
      "    use (\\x \\y captured);",
      "}"
    ]

sourcePartialApplicationNonVariableCalleeProgram :: String
sourcePartialApplicationNonVariableCalleeProgram =
  unlines
    [ "module Main export (main) {",
      "  def make : Int -> Int -> Int -> Int = \\base \\ignored \\x base;",
      "  def apply : (Int -> Int) -> Int = \\f f 4;",
      "  def main : Int =",
      "    let base : Int = 1 in",
      "    apply (((\\z make z) base) 2);",
      "}"
    ]

sourcePolymorphicTopLevelClosureCallProgram :: String
sourcePolymorphicTopLevelClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def maker : forall a. a -> Int = let captured : Int = 41 in \\(x : a) captured;",
      "  def main : Int = maker 0;",
      "}"
    ]

sourceTopLevelClosureArgumentProgram :: String
sourceTopLevelClosureArgumentProgram =
  unlines
    [ "module Main export (main) {",
      "  def maker : Int -> Int = let captured : Int = 41 in \\(x : Int) captured;",
      "  def use : (Int -> Int) -> Int = \\(f : Int -> Int) f 1;",
      "  def main : Int = use maker;",
      "}"
    ]

sourceFunctionParameterNestedClosureAliasCallProgram :: String
sourceFunctionParameterNestedClosureAliasCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = \\(f : Int -> Int)",
      "    let g : Int -> Int = (let h : Int -> Int = f in h) in",
      "    g 1;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = \\(x : Int) captured in",
      "    use f;",
      "}"
    ]

sourceReturnedLetLambdaClosureProgram :: String
sourceReturnedLetLambdaClosureProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int -> Int -> Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int -> Int = \\(x : Int) let y : Int = captured in \\(z : Int) y in",
      "    f;",
      "}"
    ]

sourceReturnedLetLambdaShadowingProgram :: String
sourceReturnedLetLambdaShadowingProgram =
  unlines
    [ "module Main export (main) {",
      "  def make : Int -> Int = let y : Int = 1 in \\(y : Int) y;",
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
      "    let f : Int -> Int = \\(x : Int) captured in",
      "    case FnBox f of { FnBox g -> g 0 };",
      "}"
    ]

zeroCaptureClosureProgram :: BackendProgram
zeroCaptureClosureProgram =
  programWithMainExpr intTy $
    BackendLet
      { backendExprType = intTy,
        backendLetName = "f",
        backendLetType = unaryIntTy,
        backendLetRhs =
          BackendClosure
            { backendExprType = unaryIntTy,
              backendClosureEntryName = "__mlfp_closure$identity",
              backendClosureCaptures = [],
              backendClosureParams = [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            },
        backendLetBody = BackendClosureCall intTy (BackendVar unaryIntTy "f") [intLit 7]
      }

capturedClosureProgram :: BackendProgram
capturedClosureProgram =
  programWithMainExpr intTy $
    BackendLet
      { backendExprType = intTy,
        backendLetName = "captured",
        backendLetType = intTy,
        backendLetRhs = intLit 41,
        backendLetBody =
          BackendLet
            { backendExprType = intTy,
              backendLetName = "f",
              backendLetType = unaryIntTy,
              backendLetRhs =
                BackendClosure
                  { backendExprType = unaryIntTy,
                    backendClosureEntryName = "__mlfp_closure$constCaptured",
                    backendClosureCaptures = [BackendClosureCapture "captured" intTy (BackendVar intTy "captured")],
                    backendClosureParams = [("x", intTy)],
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
                              (BackendConstruct (optionTy intTy) "Some" [intLit 0])
                              ( BackendAlternative
                                  (BackendConstructorPattern "Some" ["n"])
                                  (caseClosure "__mlfp_closure$case_some" (BackendVar intTy "x"))
                                  :| [ BackendAlternative
                                         (BackendConstructorPattern "None" [])
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
      BackendClosure
        { backendExprType = unaryIntTy,
          backendClosureEntryName = entryName,
          backendClosureCaptures = [],
          backendClosureParams = [("x", intTy)],
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
          ( BackendClosure
              { backendExprType = unaryIntTy,
                backendClosureEntryName = "__mlfp_closure$let_callee",
                backendClosureCaptures = [],
                backendClosureParams = [("x", intTy)],
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
            ( BackendClosure
                { backendExprType = unaryIntTy,
                  backendClosureEntryName = "__mlfp_closure$poly",
                  backendClosureCaptures = [],
                  backendClosureParams = [("x", intTy)],
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
    (BackendTyApp (BTArrow argTy intTy) (BackendVar polymorphicClosureSpecializationTy "poly") argTy)
    arg

polymorphicClosureFunctionWrapperProgram :: BackendProgram
polymorphicClosureFunctionWrapperProgram =
  BackendProgram
    { backendProgramModules =
        [ BackendModule
            { backendModuleName = "Main",
              backendModuleData = [fnBoxData],
              backendModuleBindings =
                [ BackendBinding
                    { backendBindingName = "polyWrapper",
                      backendBindingType = polymorphicClosureFunctionWrapperTy,
                      backendBindingExpr = polymorphicClosureFunctionWrapperExpr,
                      backendBindingExportedAsMain = False
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
  BTForall "a" Nothing (BTArrow (BTVar "a") fnBoxTy)

polymorphicClosureFunctionWrapperExpr :: BackendExpr
polymorphicClosureFunctionWrapperExpr =
  BackendTyAbs
    polymorphicClosureFunctionWrapperTy
    "a"
    Nothing
    ( BackendLam
        (BTArrow (BTVar "a") fnBoxTy)
        "ignored"
        (BTVar "a")
        (BackendConstruct fnBoxTy "FnBox" [closureContainingFunctionExpr])
    )

closureContainingFunctionExpr :: BackendExpr
closureContainingFunctionExpr =
  closureWithEntry "__mlfp_closure$wrapper_key"

polymorphicClosureFunctionWrapperCall :: BackendType -> BackendExpr -> BackendExpr
polymorphicClosureFunctionWrapperCall argTy arg =
  BackendApp
    fnBoxTy
    (BackendTyApp (BTArrow argTy fnBoxTy) (BackendVar polymorphicClosureFunctionWrapperTy "polyWrapper") argTy)
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
                  (BackendTyApp (BTArrow unaryIntTy (BTArrow intTy intTy)) (BackendVar inlinePolymorphicClosureTy "polyInline") intTy)
                  (BackendVar unaryIntTy "helper")
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
                ( BackendClosure
                    { backendExprType = unaryIntTy,
                      backendClosureEntryName = "__mlfp_closure$inline",
                      backendClosureCaptures = [],
                      backendClosureParams = [("x", intTy)],
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
    BackendConstruct
      fnBoxTy
      "FnBox"
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
  BackendClosure
    { backendExprType = unaryIntTy,
      backendClosureEntryName = entryName,
      backendClosureCaptures = [],
      backendClosureParams = [("x", intTy)],
      backendClosureBody = BackendVar intTy "x"
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
                      backendBindingExpr = BackendConstruct fnBoxTy "FnBox" [closureWithEntry "__mlfp_closure$field_top"],
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
                      backendBindingExpr = BackendConstruct fnBoxTy "FnBox" [closureWithEntry "__mlfp_closure$field_direct"],
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
    BackendLet
      { backendExprType = fnBoxTy,
        backendLetName = "f",
        backendLetType = unaryIntTy,
        backendLetRhs = closureWithEntry "__mlfp_closure$field_local",
        backendLetBody = BackendConstruct fnBoxTy "FnBox" [BackendVar unaryIntTy "f"]
      }

transitiveLocalFunctionFieldProgram :: BackendProgram
transitiveLocalFunctionFieldProgram =
  programWithFnBoxMainExpr $
    BackendLet
      { backendExprType = fnBoxTy,
        backendLetName = "f",
        backendLetType = unaryIntTy,
        backendLetRhs = closureWithEntry "__mlfp_closure$field_transitive",
        backendLetBody =
          BackendLet
            { backendExprType = fnBoxTy,
              backendLetName = "g",
              backendLetType = unaryIntTy,
              backendLetRhs = BackendVar unaryIntTy "f",
              backendLetBody = BackendConstruct fnBoxTy "FnBox" [BackendVar unaryIntTy "g"]
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
                          (BackendConstruct fnBoxTy "FnBox" [closureWithEntry "__mlfp_closure$field_restored"])
                          ( BackendAlternative
                              (BackendConstructorPattern "FnBox" ["f"])
                              (BackendConstruct fnBoxTy "FnBox" [BackendVar unaryIntTy "f"])
                              :| []
                          ),
                      backendBindingExportedAsMain = True
                    }
                ]
            }
        ],
      backendProgramMain = "main"
    }

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
                        BackendLet
                          fnBoxTy
                          "captured"
                          intTy
                          (intLit 1)
                          ( BackendConstruct
                              fnBoxTy
                              "FnBox"
                              [ BackendClosure
                                  { backendExprType = unaryIntTy,
                                    backendClosureEntryName = "__mlfp_closure$field_captured",
                                    backendClosureCaptures = [BackendClosureCapture "captured" intTy (BackendVar intTy "captured")],
                                    backendClosureParams = [("x", intTy)],
                                    backendClosureBody = BackendVar intTy "captured"
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
                        BackendLam
                          { backendExprType = unaryIntTy,
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
                          ( BackendClosure
                              { backendExprType = unaryIntTy,
                                backendClosureEntryName = "__mlfp_closure$call_captured_function",
                                backendClosureCaptures = [BackendClosureCapture "f" unaryIntTy (BackendVar unaryIntTy "inc")],
                                backendClosureParams = [("x", intTy)],
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
                        BackendLam
                          { backendExprType = unaryIntTy,
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
                        BackendLam
                          { backendExprType = BTArrow unaryIntTy (BTArrow intTy unaryIntTy),
                            backendParamName = "$evidence_f",
                            backendParamType = unaryIntTy,
                            backendBody =
                              BackendLam
                                { backendExprType = BTArrow intTy unaryIntTy,
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
                                  (BackendVar (BTArrow unaryIntTy (BTArrow intTy unaryIntTy)) "idRaw")
                                  (BackendVar unaryIntTy "inc")
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
                        BackendLam
                          { backendExprType = unaryIntTy,
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
                        BackendLam
                          { backendExprType = BTArrow unaryIntTy (BTArrow intTy unaryIntTy),
                            backendParamName = "$evidence_f",
                            backendParamType = unaryIntTy,
                            backendBody =
                              BackendLam
                                { backendExprType = BTArrow intTy unaryIntTy,
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
                                  (BackendVar (BTArrow unaryIntTy (BTArrow intTy unaryIntTy)) "idAlias")
                                  (BackendVar unaryIntTy "inc")
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
                      backendBindingExpr = BackendApp intTy (BackendVar unaryIntTy "left") (intLit 41),
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
          (BackendVar unaryIntTy target),
      backendBindingExportedAsMain = False
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

boolIdentityExpr :: BackendExpr
boolIdentityExpr =
  BackendLam
    { backendExprType = unaryBoolTy,
      backendParamName = "x",
      backendParamType = boolTy,
      backendBody = BackendVar boolTy "x"
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

strictBoxData :: BackendData
strictBoxData =
  BackendData
    { backendDataName = "StrictBox",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            "StrictBox"
            []
            [polyIdTy, recIntTy]
            strictBoxTy
        ]
    }

immediateChoiceData :: BackendData
immediateChoiceData =
  BackendData
    { backendDataName = "ImmediateChoice",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            "WithStatic"
            []
            [polyIdTy, intTy]
            immediateChoiceTy,
          BackendConstructor
            "Other"
            []
            []
            immediateChoiceTy
        ]
    }

immediateStrictChoiceData :: BackendData
immediateStrictChoiceData =
  BackendData
    { backendDataName = "ImmediateStrictChoice",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            "WithStrictStatic"
            []
            [polyIdTy, recIntTy]
            immediateStrictChoiceTy,
          BackendConstructor
            "StrictOther"
            []
            []
            immediateStrictChoiceTy
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

intLit :: Integer -> BackendExpr
intLit value =
  BackendLit intTy (LInt value)

boolLit :: Bool -> BackendExpr
boolLit value =
  BackendLit boolTy (LBool value)

intTy :: BackendType
intTy =
  BTBase (BaseTy "Int")

boolTy :: BackendType
boolTy =
  BTBase (BaseTy "Bool")

stringTy :: BackendType
stringTy =
  BTBase (BaseTy "String")

optionTy :: BackendType -> BackendType
optionTy ty =
  BTCon (BaseTy "Option") (ty :| [])

resultBoxTy :: BackendType
resultBoxTy =
  BTBase (BaseTy "ResultBox")

resultBoxStructuralTy :: BackendType
resultBoxStructuralTy =
  BTMu "$ResultBox_self" (singleFieldStructuralBody intTy)

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
  BTBase (BaseTy "FnBox")

lazyFieldBoxTy :: BackendType
lazyFieldBoxTy =
  BTBase (BaseTy "LazyFieldBox")

strictBoxTy :: BackendType
strictBoxTy =
  BTBase (BaseTy "StrictBox")

immediateChoiceTy :: BackendType
immediateChoiceTy =
  BTBase (BaseTy "ImmediateChoice")

immediateStrictChoiceTy :: BackendType
immediateStrictChoiceTy =
  BTBase (BaseTy "ImmediateStrictChoice")

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
  = ExpectLLVMNativeRun
  | ExpectLLVMNativeUnsupported String

llvmParityExpectations :: Map.Map String LLVMParityExpectation
llvmParityExpectations =
  Map.fromList
    [ ( runtimeCaseName runtimeCase,
        case Map.lookup (runtimeCaseName runtimeCase) llvmNativeUnsupportedParityCases of
          Just reason -> ExpectLLVMNativeUnsupported reason
          Nothing -> ExpectLLVMNativeRun
      )
    | runtimeCase <- programSpecToLLVMParityCases
    ]

llvmNativeUnsupportedParityCases :: Map.Map String String
llvmNativeUnsupportedParityCases =
  Map.fromList
    [ ( "standalone: does not decode non-data main values through fallback ADT decoding",
        "main must be a zero-argument pure value"
      ),
      ( "standalone: does not decode typed non-data constructor fields through fallback ADT decoding",
        "function main values are not native-renderable"
      )
    ]

llvmNativeRepresentativeParityCases :: [String]
llvmNativeRepresentativeParityCases =
  [ "surface: runs lambda/application",
    "surface: runs let polymorphism at Int and Bool",
    "unified fixture: test/programs/unified/authoritative-case-analysis.mlfp",
    "unified fixture: test/programs/unified/authoritative-recursive-let.mlfp",
    "unified fixture: test/programs/unified/authoritative-overloaded-method.mlfp",
    "unified fixture: test/programs/unified/first-class-polymorphism.mlfp"
  ]

llvmObjectCodeParityCases :: [String]
llvmObjectCodeParityCases =
    [ "surface: runs lambda/application",
      "surface: runs top-level partial application",
      "unified fixture: test/programs/unified/authoritative-case-analysis.mlfp",
      "unified fixture: test/programs/unified/authoritative-recursive-let.mlfp",
      "boundary: runs value-exported constructor when owner type is not exported",
      "boundary: runs aliased bulk-imported hidden-owner constructors in one case",
      "boundary: runs exposed constructor with qualified alias type identity",
      "unified fixture: test/programs/unified/first-class-polymorphism.mlfp",
      "unified fixture: test/programs/unified/higher-order-function-field.mlfp",
      "unified fixture: test/programs/unified/higher-order-local-function-flow.mlfp",
      "unified fixture: test/programs/unified/higher-order-partial-application.mlfp",
      "unified fixture: test/programs/unified/higher-order-returned-function.mlfp",
      "standalone: does not decode typed non-data constructor fields through fallback ADT decoding",
      "standalone: applies captured function-valued constructor fields"
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
      Just ExpectLLVMNativeRun -> do
        output <- requireRight result
        validateLLVMAssembly output
        when (runtimeCaseName runtimeCase `Set.member` llvmObjectCodeParityCaseNames) $
          validateLLVMObjectCode output
        nativeOutput <- requireRight =<< emitProgramRuntimeNativeLLVM (runtimeCaseSource runtimeCase)
        validateLLVMAssembly nativeOutput
        validateLLVMObjectCode nativeOutput
        nativeResult <- runLLVMNativeExecutable nativeOutput
        assertNativeRuntimeResult (runtimeCaseExpectation runtimeCase) nativeResult
      Just (ExpectLLVMNativeUnsupported fragment) -> do
        output <- requireRight result
        validateLLVMAssembly output
        when (runtimeCaseName runtimeCase `Set.member` llvmObjectCodeParityCaseNames) $
          validateLLVMObjectCode output
        nativeResult <- emitProgramRuntimeNativeLLVM (runtimeCaseSource runtimeCase)
        case nativeResult of
          Left err ->
            err `shouldSatisfy` isInfixOf fragment
          Right nativeOutput ->
            expectationFailure $
              "expected native LLVM emission to reject "
                ++ runtimeCaseName runtimeCase
                ++ " with "
                ++ show fragment
                ++ ", but it emitted:\n"
                ++ nativeOutput

emitProgramRuntimeLLVM :: ProgramMatrixSource -> IO (Either String String)
emitProgramRuntimeLLVM source =
  case source of
    InlineProgram programText ->
      withTempProgram programText emitBackendFile
    ProgramFile path ->
      emitBackendFile path

emitProgramRuntimeNativeLLVM :: ProgramMatrixSource -> IO (Either String String)
emitProgramRuntimeNativeLLVM source =
  case source of
    InlineProgram programText ->
      withTempProgram programText emitNativeFile
    ProgramFile path ->
      emitNativeFile path

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
