{-# LANGUAGE LambdaCase #-}

module BackendLLVMSpec (spec) where

import Control.Monad (when)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit (ExitCode (..))
import Test.Hspec

import LLVMToolSupport
  ( NativeRunResult (..),
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

  it "runs a linked native executable and captures process output" $ do
    result <- runLLVMNativeExecutable nativeOutputCaptureLLVM

    nativeRunExitCode result `shouldBe` ExitFailure 7
    nativeRunStdout result `shouldBe` "native stdout\n"
    nativeRunStderr result `shouldBe` "native stderr\n"

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

  it "preserves inline function argument shadowing for bare references" $ do
    output <- requireRight (renderBackendProgramLLVM inlineFunctionArgumentShadowsValueProgram)

    output `shouldSatisfy` isInfixOf "store ptr @\"idInt\""
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

      length caseNames `shouldBe` Set.size uniqueCaseNames
      objectCodeNames `Set.isSubsetOf` uniqueCaseNames `shouldBe` True
      Map.keysSet llvmParityExpectations `shouldBe` uniqueCaseNames

    mapM_ runLLVMParityCase programSpecToLLVMParityCases

  it "rejects partial applications until closure conversion exists" $ do
    renderBackendProgramLLVM partialApplicationProgram
      `shouldSatisfyLeft` isInfixOf "Unsupported backend LLVM type"

  it "rejects escaping lambdas until closure conversion exists" $ do
    renderBackendProgramLLVM escapingLambdaProgram
      `shouldSatisfyLeft` isInfixOf "Unsupported backend LLVM type"

  it "lowers stored top-level function constructor fields" $ do
    output <- requireRight (renderBackendProgramLLVM functionFieldProgram)

    output `shouldSatisfy` isInfixOf "define i64 @\"helper\""
    output `shouldSatisfy` isInfixOf "store ptr @\"helper\""
    validateLLVMAssembly output

  it "lowers stored direct function constructor fields through private wrappers" $ do
    output <- requireRight (renderBackendProgramLLVM directFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_function_wrapper$"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "lowers stored local function constructor fields through private wrappers" $ do
    output <- requireRight (renderBackendProgramLLVM localFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_function_wrapper$"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "lowers stored transitive local function aliases through private wrappers" $ do
    output <- requireRight (renderBackendProgramLLVM transitiveLocalFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_function_wrapper$"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_function_wrapper$"
    validateLLVMAssembly output

  it "re-stores immediate constructor fields carrying closed direct functions" $ do
    output <- requireRight (renderBackendProgramLLVM immediateRestoredFunctionFieldProgram)

    output `shouldSatisfy` isInfixOf "define private i64 @\"__mlfp_function_wrapper$"
    output `shouldSatisfy` isInfixOf "store ptr @\"__mlfp_function_wrapper$"
    output `shouldNotSatisfy` isInfixOf "unsupported function argument"
    validateLLVMAssembly output

  it "rejects captured function constructor fields until closure conversion exists" $ do
    renderBackendProgramLLVM capturedFunctionFieldProgram
      `shouldSatisfyLeft` isInfixOf "unsupported function argument"

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
                          (BackendConstruct fnBoxTy "FnBox" [BackendVar unaryIntTy "f"]),
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
                      backendBindingExpr = BackendConstruct fnBoxTy "FnBox" [intIdentityExpr],
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
        backendLetRhs = intIdentityExpr,
        backendLetBody = BackendConstruct fnBoxTy "FnBox" [BackendVar unaryIntTy "f"]
      }

transitiveLocalFunctionFieldProgram :: BackendProgram
transitiveLocalFunctionFieldProgram =
  programWithFnBoxMainExpr $
    BackendLet
      { backendExprType = fnBoxTy,
        backendLetName = "f",
        backendLetType = unaryIntTy,
        backendLetRhs = intIdentityExpr,
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
                          (BackendConstruct fnBoxTy "FnBox" [intIdentityExpr])
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
                              [ BackendLam
                                  unaryIntTy
                                  "x"
                                  intTy
                                  (BackendVar intTy "captured")
                              ]
                          ),
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
  = ExpectLLVMAssembly

llvmParityExpectations :: Map.Map String LLVMParityExpectation
llvmParityExpectations =
  Map.fromList
    [ (runtimeCaseName runtimeCase, ExpectLLVMAssembly)
    | runtimeCase <- programSpecToLLVMParityCases
    ]

llvmObjectCodeParityCases :: [String]
llvmObjectCodeParityCases =
    [ "surface: runs lambda/application",
      "unified fixture: test/programs/unified/authoritative-case-analysis.mlfp",
      "unified fixture: test/programs/unified/authoritative-recursive-let.mlfp",
      "boundary: runs value-exported constructor when owner type is not exported",
      "boundary: runs aliased bulk-imported hidden-owner constructors in one case",
      "boundary: runs exposed constructor with qualified alias type identity",
      "unified fixture: test/programs/unified/first-class-polymorphism.mlfp",
      "standalone: does not decode typed non-data constructor fields through fallback ADT decoding"
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
