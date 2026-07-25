module Parity.ProgramMatrix.NativePolicy
    ( SourceCheckingPolicy (..)
    , InterpreterRuntimePolicy (..)
    , BackendLLVMAssemblyPolicy (..)
    , ObjectCodePolicy (..)
    , NativeRunPolicy (..)
    , ToolAvailabilityPolicy (..)
    , ProgramLLVMNativeParityPolicy
    , parityCaseName
    , parityCaseSource
    , parityExpectedRuntime
    , paritySourceChecking
    , parityInterpreterRuntime
    , parityBackendLLVMAssembly
    , parityBackendLLVMRequiredFragments
    , parityBackendLLVMForbiddenFragments
    , parityObjectCode
    , parityNativeRun
    , parityToolAvailability
    , programLLVMNativeParityPolicies
    , programLLVMNativeParityPolicyDiagnostics
    , objectCodeParityCaseNames
    , requiredNativeRunParityCaseNames
    , nativeUnsupportedParityCaseNames
    , describeProgramLLVMNativeParityPolicy
    , summarizeProgramLLVMNativeParityPolicy
    ) where

import Data.List (group, intercalate, sort)
import qualified Data.Set as Set

import Parity.ProgramMatrix
    ( ProgramMatrixSource (..)
    , ProgramRuntimeCase (..)
    , ProgramRuntimeExpectation
    , programRuntimeSuccessCases
    )

data SourceCheckingPolicy
    = SourceCheckingSucceeds
    deriving (Eq, Show)

data InterpreterRuntimePolicy
    = InterpreterRuntimeSucceeds
    deriving (Eq, Show)

data BackendLLVMAssemblyPolicy
    = BackendLLVMAssemblyRequired
    | BackendLLVMAssemblyUnsupported String
    deriving (Eq, Show)

data ObjectCodePolicy
    = ObjectCodeRequired
    | ObjectCodeNotRequired
    deriving (Eq, Show)

data NativeRunPolicy
    = NativeRunRequired
    | NativeRunUnsupported String
    deriving (Eq, Show)

data ToolAvailabilityPolicy = ToolAvailabilityPolicy
    { toolRequiresLLVMAssembler :: Bool
    , toolRequiresLLCForObjectCodeSmoke :: Bool
    , toolRequiresLLCForNativeRun :: Bool
    , toolRequiresNativeLinkerForNativeRun :: Bool
    }
    deriving (Eq, Show)

newtype ProgramLLVMNativeParityPolicy =
    ProgramLLVMNativeParityPolicy ProgramRuntimeCase

programLLVMNativeParityPolicies :: [ProgramLLVMNativeParityPolicy]
programLLVMNativeParityPolicies =
    map ProgramLLVMNativeParityPolicy programRuntimeSuccessCases

parityCaseName :: ProgramLLVMNativeParityPolicy -> String
parityCaseName (ProgramLLVMNativeParityPolicy runtimeCase) =
    runtimeCaseName runtimeCase

parityCaseSource :: ProgramLLVMNativeParityPolicy -> ProgramMatrixSource
parityCaseSource (ProgramLLVMNativeParityPolicy runtimeCase) =
    runtimeCaseSource runtimeCase

parityExpectedRuntime :: ProgramLLVMNativeParityPolicy -> ProgramRuntimeExpectation
parityExpectedRuntime (ProgramLLVMNativeParityPolicy runtimeCase) =
    runtimeCaseExpectation runtimeCase

paritySourceChecking :: ProgramLLVMNativeParityPolicy -> SourceCheckingPolicy
paritySourceChecking _ =
    SourceCheckingSucceeds

parityInterpreterRuntime :: ProgramLLVMNativeParityPolicy -> InterpreterRuntimePolicy
parityInterpreterRuntime _ =
    InterpreterRuntimeSucceeds

parityBackendLLVMAssembly :: ProgramLLVMNativeParityPolicy -> BackendLLVMAssemblyPolicy
parityBackendLLVMAssembly _ =
    BackendLLVMAssemblyRequired

parityBackendLLVMRequiredFragments :: ProgramLLVMNativeParityPolicy -> [String]
parityBackendLLVMRequiredFragments =
    backendLLVMRequiredFragments . parityCaseSource

parityBackendLLVMForbiddenFragments :: ProgramLLVMNativeParityPolicy -> [String]
parityBackendLLVMForbiddenFragments =
    backendLLVMForbiddenFragments . parityCaseSource

parityObjectCode :: ProgramLLVMNativeParityPolicy -> ObjectCodePolicy
parityObjectCode =
    objectCodePolicy . parityCaseName

parityNativeRun :: ProgramLLVMNativeParityPolicy -> NativeRunPolicy
parityNativeRun =
    nativeRunPolicy . parityCaseName

parityToolAvailability :: ProgramLLVMNativeParityPolicy -> ToolAvailabilityPolicy
parityToolAvailability policy =
    toolAvailabilityPolicy (parityObjectCode policy) (parityNativeRun policy)

backendLLVMRequiredFragments :: ProgramMatrixSource -> [String]
backendLLVMRequiredFragments source =
    case source of
        ProgramFile path
            | path == "test/programs/unified/authoritative-cross-module-let-polymorphism.mlfp" ->
                [ "; mlf2 LLVM backend v0"
                , "define i64 @\"Core__applyId\"()"
                , "define i64 @\"User__main\"()"
                ]
            | path == "test/programs/unified/authoritative-case-analysis.mlfp" ->
                [ "; mlf2 LLVM backend v0"
                , "call ptr @\"malloc\""
                , "call ptr @\"malloc\"(i64 8)"
                , "call ptr @\"malloc\"(i64 16)"
                , "getelementptr i8, ptr %\"__llvm.malloc.0\", i64 0"
                , "store i64 0, ptr %\"__llvm.tag.ptr.1\""
                , "getelementptr i8, ptr %\"__llvm.malloc.2\", i64 0"
                , "store i64 1, ptr %\"__llvm.tag.ptr.3\""
                , "getelementptr i8, ptr %\"__llvm.malloc.2\", i64 8"
                , "switch i64"
                , "switch i64 %\"__llvm.case.tag.6\", label %case.default.2 [ i64 0, label %case.alt.0 i64 1, label %case.alt.1 ]"
                , "phi i64"
                ]
            | path == "test/programs/unified/first-class-polymorphism.mlfp" ->
                ["define i1 @\"FirstClassPolymorphism__main\"()"]
            | path == "test/programs/unified/authoritative-recursive-let.mlfp" ->
                [ "define ptr @\"Main__main$letrec$"
                , "call ptr @\"Main__main$letrec$"
                , "switch i64"
                , "phi ptr"
                ]
            | path == "test/programs/recursive-adt/recursive-list-tail.mlfp" ->
                firstOrderRecursiveFragments
                    ++ [ "define ptr @\"RecursiveList__tailOrNil\""
                       , "define i1 @\"RecursiveList__isNil\""
                       , "phi ptr"
                       ]
            | path == "test/programs/recursive-adt/recursive-existential.mlfp" ->
                firstOrderRecursiveFragments
                    ++ [ "define i1 @\"RecursiveExistential__unwrapSome\""
                       , "call i1 @\"RecursiveExistential__unwrapSome\""
                       ]
            | path `elem` firstOrderRecursiveFixturePaths ->
                firstOrderRecursiveFragments
        _ ->
            []

backendLLVMForbiddenFragments :: ProgramMatrixSource -> [String]
backendLLVMForbiddenFragments source =
    case source of
        ProgramFile "test/programs/unified/first-class-polymorphism.mlfp" ->
            ["FirstClassPolymorphism__usePoly"]
        _ -> []

firstOrderRecursiveFixturePaths :: [FilePath]
firstOrderRecursiveFixturePaths =
    [ "test/programs/recursive-adt/plain-recursive-nat.mlfp"
    , "test/programs/recursive-adt/recursive-list-tail.mlfp"
    , "test/programs/recursive-adt/recursive-gadt.mlfp"
    , "test/programs/recursive-adt/recursive-existential.mlfp"
    , "test/programs/recursive-adt/recursive-tree-first-order.mlfp"
    ]

firstOrderRecursiveFragments :: [String]
firstOrderRecursiveFragments =
    [ "call ptr @\"malloc\""
    , "switch i64"
    ]

objectCodePolicy :: String -> ObjectCodePolicy
objectCodePolicy caseName
    | caseName `Set.member` objectCodeParityCaseNameSet = ObjectCodeRequired
    | otherwise = ObjectCodeNotRequired

nativeRunPolicy :: String -> NativeRunPolicy
nativeRunPolicy caseName =
    case lookup caseName nativeUnsupportedParityCaseDiagnostics of
        Just diagnostic -> NativeRunUnsupported diagnostic
        Nothing -> NativeRunRequired

toolAvailabilityPolicy :: ObjectCodePolicy -> NativeRunPolicy -> ToolAvailabilityPolicy
toolAvailabilityPolicy objectCode nativeRun =
    ToolAvailabilityPolicy
        { toolRequiresLLVMAssembler = True
        , toolRequiresLLCForObjectCodeSmoke =
            case objectCode of
                ObjectCodeRequired -> True
                ObjectCodeNotRequired -> False
        , toolRequiresLLCForNativeRun =
            case nativeRun of
                NativeRunRequired -> True
                NativeRunUnsupported _ -> False
        , toolRequiresNativeLinkerForNativeRun =
            case nativeRun of
                NativeRunRequired -> True
                NativeRunUnsupported _ -> False
        }

requiredNativeRunParityCaseNames :: [String]
requiredNativeRunParityCaseNames =
    [ parityCaseName policy
    | policy <- programLLVMNativeParityPolicies
    , NativeRunRequired <- [parityNativeRun policy]
    ]

nativeUnsupportedParityCaseNames :: [String]
nativeUnsupportedParityCaseNames =
    map fst nativeUnsupportedParityCaseDiagnostics

nativeUnsupportedParityCaseDiagnostics :: [(String, String)]
nativeUnsupportedParityCaseDiagnostics =
    []

objectCodeParityCaseNames :: [String]
objectCodeParityCaseNames =
    [ "surface: runs lambda/application"
    , "surface: runs top-level partial application"
    , "unified fixture: test/programs/unified/authoritative-cross-module-let-polymorphism.mlfp"
    , "unified fixture: test/programs/unified/authoritative-case-analysis.mlfp"
    , "unified fixture: test/programs/unified/authoritative-recursive-let.mlfp"
    , "boundary: runs value-exported constructor when owner type is not exported"
    , "boundary: runs aliased bulk-imported hidden-owner constructors in one case"
    , "boundary: runs exposed constructor with qualified alias type identity"
    , "unified fixture: test/programs/unified/first-class-polymorphism.mlfp"
    , "unified fixture: test/programs/unified/higher-order-function-field.mlfp"
    , "unified fixture: test/programs/unified/higher-order-local-function-flow.mlfp"
    , "unified fixture: test/programs/unified/higher-order-partial-application.mlfp"
    , "unified fixture: test/programs/unified/higher-order-returned-function.mlfp"
    , "fixture: test/programs/recursive-adt/plain-recursive-nat.mlfp"
    , "fixture: test/programs/recursive-adt/recursive-list-tail.mlfp"
    , "fixture: test/programs/recursive-adt/recursive-gadt.mlfp"
    , "fixture: test/programs/recursive-adt/recursive-existential.mlfp"
    , "fixture: test/programs/recursive-adt/recursive-tree-first-order.mlfp"
    , "standalone: does not decode typed non-data constructor fields through fallback ADT decoding"
    , "standalone: applies captured function-valued constructor fields"
    ]

objectCodeParityCaseNameSet :: Set.Set String
objectCodeParityCaseNameSet =
    Set.fromList objectCodeParityCaseNames

programLLVMNativeParityPolicyDiagnostics :: [String]
programLLVMNativeParityPolicyDiagnostics =
    [ "interpreter-success runtime case matrix is empty"
    | null expectedCaseNames
    ]
        ++ duplicateDiagnostics "interpreter-success runtime case" expectedCaseNames
        ++ duplicateDiagnostics "native parity policy case" policyCaseNames
        ++ mismatchDiagnostics "policy rows" expectedCaseNames policyCaseNames
        ++ unknownNameDiagnostics "object-code parity case" objectCodeParityCaseNames expectedCaseNameSet
        ++ unknownNameDiagnostics "native-run unsupported case" nativeUnsupportedParityCaseNames expectedCaseNameSet
        ++ nativeCoverageDiagnostics
  where
    expectedCaseNames = map runtimeCaseName programRuntimeSuccessCases
    expectedCaseNameSet = Set.fromList expectedCaseNames
    policyCaseNames = map parityCaseName programLLVMNativeParityPolicies
    requiredNativeRunNameSet = Set.fromList requiredNativeRunParityCaseNames
    nativeUnsupportedNameSet = Set.fromList nativeUnsupportedParityCaseNames
    classifiedNativeNames = requiredNativeRunNameSet `Set.union` nativeUnsupportedNameSet
    missingNativeClassifications = Set.toList (expectedCaseNameSet `Set.difference` classifiedNativeNames)
    overlappingNativeClassifications = Set.toList (requiredNativeRunNameSet `Set.intersection` nativeUnsupportedNameSet)
    nativeCoverageDiagnostics =
        [ "missing native-run classification for: " ++ intercalate ", " missingNativeClassifications
        | not (null missingNativeClassifications)
        ]
            ++ [ "native-run cases marked both required and unsupported: "
                    ++ intercalate ", " overlappingNativeClassifications
               | not (null overlappingNativeClassifications)
               ]

duplicateDiagnostics :: String -> [String] -> [String]
duplicateDiagnostics label names =
    [ "duplicate " ++ label ++ " names: " ++ intercalate ", " duplicates
    | let duplicates = duplicateNames names
    , not (null duplicates)
    ]

duplicateNames :: [String] -> [String]
duplicateNames names =
    [ name
    | name : _ : _ <- group (sort names)
    ]

mismatchDiagnostics :: String -> [String] -> [String] -> [String]
mismatchDiagnostics label expected actual =
    [ label ++ " differ from ProgramMatrix interpreter-success rows"
        ++ "\nexpected: "
        ++ show expected
        ++ "\nactual: "
        ++ show actual
    | expected /= actual
    ]

unknownNameDiagnostics :: String -> [String] -> Set.Set String -> [String]
unknownNameDiagnostics label names knownNames =
    [ "unknown " ++ label ++ " names: " ++ intercalate ", " unknownNames
    | let unknownNames = filter (`Set.notMember` knownNames) names
    , not (null unknownNames)
    ]

describeProgramLLVMNativeParityPolicy :: ProgramLLVMNativeParityPolicy -> String
describeProgramLLVMNativeParityPolicy policy =
    parityCaseName policy ++ " [" ++ summarizeProgramLLVMNativeParityPolicy policy ++ "]"

summarizeProgramLLVMNativeParityPolicy :: ProgramLLVMNativeParityPolicy -> String
summarizeProgramLLVMNativeParityPolicy policy =
    intercalate
        ", "
        [ "source=" ++ summarizeSourceCheckingPolicy (paritySourceChecking policy)
        , "runtime=" ++ summarizeInterpreterRuntimePolicy (parityInterpreterRuntime policy)
        , "backend=" ++ summarizeBackendLLVMAssemblyPolicy (parityBackendLLVMAssembly policy)
        , "object-code=" ++ summarizeObjectCodePolicy (parityObjectCode policy)
        , "native-run=" ++ summarizeNativeRunPolicy (parityNativeRun policy)
        , "tools=" ++ summarizeToolAvailabilityPolicy (parityToolAvailability policy)
        ]

summarizeSourceCheckingPolicy :: SourceCheckingPolicy -> String
summarizeSourceCheckingPolicy SourceCheckingSucceeds = "check-success"

summarizeInterpreterRuntimePolicy :: InterpreterRuntimePolicy -> String
summarizeInterpreterRuntimePolicy InterpreterRuntimeSucceeds = "interpreter-success"

summarizeBackendLLVMAssemblyPolicy :: BackendLLVMAssemblyPolicy -> String
summarizeBackendLLVMAssemblyPolicy policy =
    case policy of
        BackendLLVMAssemblyRequired -> "assembly-required"
        BackendLLVMAssemblyUnsupported _ -> "unsupported-diagnostic"

summarizeObjectCodePolicy :: ObjectCodePolicy -> String
summarizeObjectCodePolicy policy =
    case policy of
        ObjectCodeRequired -> "smoke-required"
        ObjectCodeNotRequired -> "native-object-only"

summarizeNativeRunPolicy :: NativeRunPolicy -> String
summarizeNativeRunPolicy policy =
    case policy of
        NativeRunRequired -> "required"
        NativeRunUnsupported _ -> "unsupported-diagnostic"

summarizeToolAvailabilityPolicy :: ToolAvailabilityPolicy -> String
summarizeToolAvailabilityPolicy policy =
    intercalate "+" $
        filter
            (not . null)
            [ if toolRequiresLLVMAssembler policy then "llvm-as" else ""
            , if toolRequiresLLCForObjectCodeSmoke policy then "llc-object" else ""
            , if toolRequiresLLCForNativeRun policy then "llc-native" else ""
            , if toolRequiresNativeLinkerForNativeRun policy then "native-linker" else ""
            ]
