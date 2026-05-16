module Parity.ProgramMatrix.NativePolicy
    ( SourceCheckingPolicy (..)
    , InterpreterRuntimePolicy (..)
    , BackendLLVMAssemblyPolicy (..)
    , ObjectCodePolicy (..)
    , NativeRunPolicy (..)
    , ToolAvailabilityPolicy (..)
    , ProgramLLVMNativeParityPolicy (..)
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
    ( ProgramMatrixSource
    , ProgramRuntimeCase (..)
    , ProgramRuntimeExpectation
    , programSpecToLLVMParityCases
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

data ProgramLLVMNativeParityPolicy = ProgramLLVMNativeParityPolicy
    { parityRuntimeCase :: ProgramRuntimeCase
    , parityCaseName :: String
    , parityCaseSource :: ProgramMatrixSource
    , parityExpectedRuntime :: ProgramRuntimeExpectation
    , paritySourceChecking :: SourceCheckingPolicy
    , parityInterpreterRuntime :: InterpreterRuntimePolicy
    , parityBackendLLVMAssembly :: BackendLLVMAssemblyPolicy
    , parityObjectCode :: ObjectCodePolicy
    , parityNativeRun :: NativeRunPolicy
    , parityToolAvailability :: ToolAvailabilityPolicy
    }

programLLVMNativeParityPolicies :: [ProgramLLVMNativeParityPolicy]
programLLVMNativeParityPolicies =
    map mkProgramLLVMNativeParityPolicy programSpecToLLVMParityCases

mkProgramLLVMNativeParityPolicy :: ProgramRuntimeCase -> ProgramLLVMNativeParityPolicy
mkProgramLLVMNativeParityPolicy runtimeCase =
    ProgramLLVMNativeParityPolicy
        { parityRuntimeCase = runtimeCase
        , parityCaseName = caseName
        , parityCaseSource = runtimeCaseSource runtimeCase
        , parityExpectedRuntime = runtimeCaseExpectation runtimeCase
        , paritySourceChecking = SourceCheckingSucceeds
        , parityInterpreterRuntime = InterpreterRuntimeSucceeds
        , parityBackendLLVMAssembly = BackendLLVMAssemblyRequired
        , parityObjectCode = objectCodePolicy caseName
        , parityNativeRun = nativeRunPolicy caseName
        , parityToolAvailability = toolAvailabilityPolicy (objectCodePolicy caseName) (nativeRunPolicy caseName)
        }
  where
    caseName = runtimeCaseName runtimeCase

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
    , "standalone: does not decode typed non-data constructor fields through fallback ADT decoding"
    , "standalone: applies captured function-valued constructor fields"
    ]

objectCodeParityCaseNameSet :: Set.Set String
objectCodeParityCaseNameSet =
    Set.fromList objectCodeParityCaseNames

programLLVMNativeParityPolicyDiagnostics :: [String]
programLLVMNativeParityPolicyDiagnostics =
    duplicateDiagnostics "interpreter-success runtime case" expectedCaseNames
        ++ duplicateDiagnostics "native parity policy case" policyCaseNames
        ++ mismatchDiagnostics "policy rows" expectedCaseNames policyCaseNames
        ++ unknownNameDiagnostics "object-code parity case" objectCodeParityCaseNames expectedCaseNameSet
        ++ unknownNameDiagnostics "native-run unsupported case" nativeUnsupportedParityCaseNames expectedCaseNameSet
        ++ nativeCoverageDiagnostics
  where
    expectedCaseNames = map runtimeCaseName programSpecToLLVMParityCases
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
