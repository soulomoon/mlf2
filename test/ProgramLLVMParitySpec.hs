module ProgramLLVMParitySpec (spec) where

import Control.Monad (when)
import Data.List (intercalate, isInfixOf, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import LLVMToolSupport
    ( validateLLVMAssembly
    , validateLLVMObjectCode
    , withTempProgram
    )
import MLF.Program.CLI (emitBackendFile)
import ProgramParityMatrix
import Test.Hspec

data LLVMParityExpectation
    = ExpectLLVMAssembles
    | ExpectLLVMUnsupported String

data LLVMParityCase = LLVMParityCase
    { llvmParityRuntimeCase :: ProgramRuntimeCase
    , llvmParityExpectation :: LLVMParityExpectation
    }

spec :: Spec
spec = describe "MLF.Program LLVM parity matrix" $ do
    it "covers exactly the shared runtime-success surface" $ do
        map llvmParityCaseName llvmParityCases
            `shouldBe` map programRuntimeCaseName programRuntimeSuccessCases

    it "keeps temporary unsupported LLVM cases explicit, named, and counted" $ do
        length llvmUnsupportedCases `shouldBe` expectedLLVMUnsupportedCount
        assertNoDuplicateNames "unsupported LLVM parity case" (map fst llvmUnsupportedCases)
        Set.fromList (map fst llvmUnsupportedCases)
            `shouldSatisfy` (`Set.isSubsetOf` runtimeCaseNameSet)

    mapM_ runLLVMParityCase llvmParityCases

llvmParityCases :: [LLVMParityCase]
llvmParityCases =
    [ LLVMParityCase runtimeCase (unsupportedExpectation runtimeCase)
    | runtimeCase <- programRuntimeSuccessCases
    ]

unsupportedExpectation :: ProgramRuntimeCase -> LLVMParityExpectation
unsupportedExpectation runtimeCase =
    Map.findWithDefault
        ExpectLLVMAssembles
        (programRuntimeCaseName runtimeCase)
        llvmUnsupportedCaseMap

llvmUnsupportedCaseMap :: Map.Map String LLVMParityExpectation
llvmUnsupportedCaseMap =
    Map.fromList
        [ (name, ExpectLLVMUnsupported expectedFragment)
        | (name, expectedFragment) <- llvmUnsupportedCases
        ]

llvmUnsupportedCases :: [(String, String)]
llvmUnsupportedCases =
    [ ("runs test/programs/recursive-adt/deriving-eq.mlfp", "BackendUnsupportedRecursiveLet")
    , ("runs test/programs/recursive-adt/recursive-tree-deriving.mlfp", "BackendUnsupportedRecursiveLet")
    , ("runs test/programs/recursive-adt/complex-recursive-program.mlfp", "BackendUnsupportedRecursiveLet")
    , ("runs test/programs/recursive-adt/module-integrated.mlfp", "BackendUnsupportedRecursiveLet")
    , ("runs test/programs/unified/authoritative-overloaded-method.mlfp through the eMLF-owned `.mlfp` path", "BackendUnsupportedRecursiveLet")
    , ("runs test/programs/unified/authoritative-recursive-let.mlfp through the eMLF-owned `.mlfp` path", "BackendUnsupportedRecursiveLet")
    , ("runs test/programs/unified/first-class-polymorphism.mlfp through the eMLF-owned `.mlfp` path", "Unsupported backend LLVM type")
    , ("runs first-class polymorphic top-level argument", "Unsupported backend LLVM type")
    , ("runs first-class polymorphic local argument", "could not infer type arguments")
    , ("runs overloaded method dispatch with ordinary nullary constructors", "BackendUnsupportedRecursiveLet")
    , ("runs overloaded method dispatch with nested ordinary constructors", "BackendUnsupportedRecursiveLet")
    , ("runs overloaded method dispatch with lambda/application-inferred argument", "BackendUnsupportedRecursiveLet")
    , ("runs overloaded method dispatch with let-polymorphism-inferred argument", "BackendUnsupportedRecursiveLet")
    , ("runs overloaded method dispatch with explicit argument annotation", "BackendUnsupportedRecursiveLet")
    , ("runs overloaded method dispatch on pattern-bound variable", "BackendUnsupportedRecursiveLet")
    , ("constructor argument inferred as first-class polymorphic value should run", "escaping type abstraction")
    , ("local first-class polymorphic let through constructor boundary should run", "could not infer type arguments")
    , ("pattern-bound first-class polymorphic variable should run", "could not infer type arguments")
    , ("partial overloaded method application should run after deferred resolution", "BackendUnsupportedRecursiveLet")
    , ("runs higher-kinded data field over a concrete constructor head", "BackendUnsupportedSourceType")
    , ("runs nullary constructor from mixed higher-kinded data type", "BackendUnsupportedSourceType")
    , ("runs first-class nullary constructor from mixed higher-kinded data type", "BackendUnsupportedSourceType")
    , ("runs value-imported constructor from mixed higher-kinded data type", "BackendUnsupportedSourceType")
    , ("runs value-exported constructor when owner type is not exported", "BackendUnsupportedSourceType")
    , ("runs value-exported constructor from bulk import when owner type is not exported", "BackendUnsupportedSourceType")
    , ("runs value-exported constructor from aliased bulk import when owner type is not exported", "BackendUnsupportedSourceType")
    , ("runs aliased bulk-imported hidden-owner constructors in one case", "BackendUnsupportedSourceType")
    , ("runs value-exported GADT constructor when owner type is not exported", "BackendUnsupportedSourceType")
    , ("runs value-imported nonzero-index constructor from mixed higher-kinded data type", "BackendUnsupportedSourceType")
    , ("runs constrained helper through hidden Eq evidence", "BackendUnsupportedRecursiveLet")
    , ("runs ground constrained helper alias with resolved evidence", "Unsupported backend LLVM type")
    , ("runs constrained helper after local lambda inference", "escaping function")
    , ("runs deferred method with method-level type variable constraint", "BackendTypeCheckFailed")
    , ("runs partial deferred method after method-local evidence is fixed by application", "BackendTypeCheckFailed")
    , ("runs deferred method when only a later forall binder is inferred", "BackendUnsupportedInstantiation InstElim")
    , ("runs constrained helper with method-local evidence fixed by call args", "BackendTypeCheckFailed")
    , ("runs deferred class method with method-level Eq constraint", "BackendUnsupportedRecursiveLet")
    , ("runs constrained helper through method-level evidence constraints", "BackendUnsupportedRecursiveLet")
    , ("runs explicit constrained parameterized Eq instance", "BackendUnsupportedRecursiveLet")
    , ("runs parameterized deriving Eq for Option", "BackendUnsupportedRecursiveLet")
    , ("runs parameterized deriving Eq for recursive List", "BackendUnsupportedRecursiveLet")
    , ("runs qualified import with alias-only value and constructor access", "BackendUnsupportedRecursiveLet")
    , ("runs aliased import with exposed method and qualified constructors", "BackendUnsupportedRecursiveLet")
    , ("runs aliased import exposing a type without duplicate alias-head instance matches", "BackendUnsupportedRecursiveLet")
    , ("deduplicates equivalent instances from mixed unqualified and aliased imports", "BackendUnsupportedRecursiveLet")
    , ("deduplicates constrained imported instances from mixed unqualified and aliased imports", "escaping function")
    , ("allows importing a module declared later in the file", "BackendUnsupportedRecursiveLet")
    , ("does not decode typed non-data constructor fields through fallback ADT decoding", "escaping lambda")
    , ("evaluates a recursive Nat equality example at representative depth", "BackendUnsupportedRecursiveLet")
    ]

expectedLLVMUnsupportedCount :: Int
expectedLLVMUnsupportedCount = 49

llcRepresentativeCaseNames :: Set.Set String
llcRepresentativeCaseNames =
    Set.fromList
        [ "runs lambda/application"
        , "runs test/programs/unified/authoritative-case-analysis.mlfp through the eMLF-owned `.mlfp` path"
        ]

runtimeCaseNameSet :: Set.Set String
runtimeCaseNameSet = Set.fromList (map programRuntimeCaseName programRuntimeSuccessCases)

llvmParityCaseName :: LLVMParityCase -> String
llvmParityCaseName = programRuntimeCaseName . llvmParityRuntimeCase

runLLVMParityCase :: LLVMParityCase -> Spec
runLLVMParityCase parityCase =
    it (llvmParityCaseName parityCase) $ do
        result <- emitLLVMForRuntimeCase (llvmParityRuntimeCase parityCase)
        case llvmParityExpectation parityCase of
            ExpectLLVMAssembles -> do
                output <- requireRightString result
                validateLLVMAssembly output
                when (llvmParityCaseName parityCase `Set.member` llcRepresentativeCaseNames) $
                    validateLLVMObjectCode output
            ExpectLLVMUnsupported expectedFragment ->
                case result of
                    Left err ->
                        err `shouldSatisfy` isInfixOf expectedFragment
                    Right output ->
                        expectationFailure
                            ( "expected unsupported LLVM parity case, got emitted LLVM:\n"
                                ++ output
                            )

emitLLVMForRuntimeCase :: ProgramRuntimeCase -> IO (Either String String)
emitLLVMForRuntimeCase runtimeCase =
    case programRuntimeCaseSource runtimeCase of
        ProgramFile path ->
            emitBackendFile path
        InlineProgram programText ->
            withTempProgram programText emitBackendFile

requireRightString :: Either String String -> IO String
requireRightString result =
    case result of
        Left err ->
            expectationFailure err >> fail "unexpected Left"
        Right value ->
            pure value

assertNoDuplicateNames :: String -> [String] -> Expectation
assertNoDuplicateNames label names =
    case duplicates names of
        [] -> pure ()
        duplicateNames ->
            expectationFailure $
                label
                    ++ " duplicates: "
                    ++ intercalate ", " duplicateNames

duplicates :: [String] -> [String]
duplicates = collect . sort
  where
    collect (firstName : secondName : rest)
        | firstName == secondName = firstName : collect (dropWhile (== firstName) rest)
        | otherwise = collect (secondName : rest)
    collect _ = []
