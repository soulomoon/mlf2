module MLF.Backend.LLVM.Lower.Types
  ( BackendLLVMError (..),
    BindingInfo (..),
    ClosureCaptureSlot (..),
    ClosureEntry (..),
    ConstructorRuntime (..),
    ConstructedValue (..),
    DataRuntime (..),
    ExprEnv (..),
    FunctionForm (..),
    FunctionState (..),
    LocalFunction (..),
    LowerM,
    LowerValue (..),
    LowerValueKind (..),
    LoweredProgram (..),
    NativeRenderSpec (..),
    ProgramBase (..),
    ProgramEnv (..),
    SpecRequest (..),
    Specialization (..),
    Wrapper (..),
    WrapperKind (..),
    atMay,
    combineValueKinds,
    constructedFieldValueKind,
    constructedValueForConstructor,
    constructorFieldOffset,
    constructorObjectBytes,
    constructorTagOffset,
    constructorWordBytes,
    exprEnvValueKinds,
    mergeConstructedValues,
  )
where

import Control.Monad (foldM)
import Control.Monad.State.Strict (StateT)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import MLF.Backend.IR
import MLF.Backend.LLVM.Syntax (LLVMBasicBlock, LLVMFunction, LLVMInstruction, LLVMOperand, LLVMType)

data BackendLLVMError
  = BackendLLVMValidationFailed BackendValidationError
  | BackendLLVMUnsupportedType String BackendType
  | BackendLLVMUnsupportedExpression String String
  | BackendLLVMUnsupportedCall String
  | BackendLLVMUnknownFunction String
  | BackendLLVMUnknownConstructor String
  | BackendLLVMArityMismatch String Int Int
  | BackendLLVMUnsupportedString String
  | BackendLLVMDuplicateSymbol String
  | BackendLLVMInternalError String
  deriving (Eq, Show)

data ProgramBase = ProgramBase
  { pbBindings :: Map String BindingInfo,
    pbBindingOrder :: [String],
    pbConstructors :: Map String ConstructorRuntime,
    pbData :: Map String DataRuntime,
    pbDataNames :: Set String
  }

data DataRuntime = DataRuntime
  { drData :: BackendData,
    drConstructors :: [ConstructorRuntime]
  }
  deriving (Eq, Show)

data ProgramEnv = ProgramEnv
  { peBase :: ProgramBase,
    peSpecializations :: Map String Specialization,
    peEvidenceWrappers :: Map String Wrapper,
    peFunctionWrappers :: Map String Wrapper,
    peStringGlobals :: Map String String
  }

data BindingInfo = BindingInfo
  { biName :: String,
    biForm :: FunctionForm,
    biExportedAsMain :: Bool
  }
  deriving (Eq, Show)

data FunctionForm = FunctionForm
  { ffTypeBinders :: [(String, Maybe BackendType)],
    ffParams :: [(String, BackendType)],
    ffBody :: BackendExpr,
    ffReturnType :: BackendType
  }
  deriving (Eq, Show)

data ConstructorRuntime = ConstructorRuntime
  { crConstructor :: BackendConstructor,
    crDataParameters :: [String],
    crTag :: Integer
  }
  deriving (Eq, Show)

constructorWordBytes :: Int
constructorWordBytes = 8

constructorTagOffset :: Int
constructorTagOffset = 0

constructorObjectBytes :: Int -> Int
constructorObjectBytes fieldCount =
  constructorWordBytes * (1 + fieldCount)

constructorFieldOffset :: Int -> Int
constructorFieldOffset index0 =
  constructorWordBytes * (index0 + 1)

data SpecRequest = SpecRequest
  { srBindingName :: String,
    srTypeArgs :: [BackendType]
  }
  deriving (Eq, Show)

data Specialization = Specialization
  { spRequest :: SpecRequest,
    spFunctionName :: String,
    spForm :: FunctionForm
  }
  deriving (Eq, Show)

data WrapperKind = EvidenceWrapperKind | FunctionWrapperKind
  deriving (Eq, Show)

data Wrapper = Wrapper
  { wrapperKind :: WrapperKind,
    wrapperKey :: String,
    wrapperFunctionName :: String,
    wrapperExpectedType :: BackendType,
    wrapperExpr :: BackendExpr
  }
  deriving (Eq, Show)

data ClosureEntry = ClosureEntry
  { ceFunctionType :: BackendType,
    ceEntryName :: String,
    ceCaptures :: [ClosureCaptureSlot],
    ceParams :: [(String, BackendType)],
    ceBody :: BackendExpr
  }
  deriving (Eq, Show)

data ClosureCaptureSlot = ClosureCaptureSlot
  { ccsName :: String,
    ccsType :: BackendType,
    ccsValueKind :: LowerValueKind
  }
  deriving (Eq, Show)

data ConstructedValue = ConstructedValue
  { cvFieldValueKindsByConstructor :: Map String [LowerValueKind]
  }
  deriving (Eq, Show)

data LoweredProgram = LoweredProgram
  { lpBase :: ProgramBase,
    lpEnv :: ProgramEnv,
    lpMainBinding :: BindingInfo,
    lpFunctions :: [LLVMFunction]
  }

data NativeRenderSpec = NativeRenderSpec
  { nrsType :: BackendType,
    nrsFunctionName :: String
  }
  deriving (Eq, Show)

data LowerValue = LowerValue
  { lvBackendType :: BackendType,
    lvLLVMType :: LLVMType,
    lvOperand :: LLVMOperand,
    lvValueKind :: LowerValueKind,
    lvConstructedValue :: Maybe ConstructedValue
  }
  deriving (Eq, Show)

data LowerValueKind
  = LowerRuntimeValue
  | LowerClosureRecord
  | LowerFunctionPointer
  deriving (Eq, Show)

constructedValueForConstructor :: String -> [LowerValueKind] -> ConstructedValue
constructedValueForConstructor name fieldKinds =
  ConstructedValue (Map.singleton name fieldKinds)

constructedFieldValueKind :: String -> Int -> ConstructedValue -> Maybe LowerValueKind
constructedFieldValueKind constructorName index0 constructed =
  Map.lookup constructorName (cvFieldValueKindsByConstructor constructed) >>= flip atMay index0

mergeConstructedValues :: [Maybe ConstructedValue] -> Maybe ConstructedValue
mergeConstructedValues values =
  case foldM mergeValue Map.empty constructedFields of
    Just fieldsByConstructor
      | length constructedFields == length values,
        not (Map.null fieldsByConstructor) ->
          Just (ConstructedValue fieldsByConstructor)
    _ ->
      Nothing
  where
    constructedFields =
      [fieldsByConstructor | Just (ConstructedValue fieldsByConstructor) <- values]

    mergeValue acc fieldsByConstructor =
      foldM mergeConstructor acc (Map.toList fieldsByConstructor)

    mergeConstructor acc (constructorName, fieldKinds) =
      case Map.lookup constructorName acc of
        Nothing ->
          Just (Map.insert constructorName fieldKinds acc)
        Just existingKinds
          | existingKinds == fieldKinds ->
              Just acc
        Just _ ->
          Nothing

combineValueKinds :: BackendType -> [LowerValueKind] -> LowerValueKind
combineValueKinds resultTy kinds =
  case uniqueKinds of
    [kind] -> kind
    _
      | isFirstOrderFunctionPointerType resultTy,
        LowerClosureRecord `elem` uniqueKinds,
        LowerFunctionPointer `elem` uniqueKinds ->
          LowerClosureRecord
    _ -> valueKindForType resultTy
  where
    uniqueKinds = nub kinds

    isFirstOrderFunctionPointerType ty =
      case ty of
        BTArrow {} ->
          let (params, returnTy) = collectArrowsType ty
           in all isFirstOrderPointerValueType (returnTy : params)
        _ ->
          False

    isFirstOrderPointerValueType =
      \case
        BTVar {} -> False
        BTArrow {} -> False
        BTBase {} -> True
        BTCon _ args -> all isFirstOrderPointerValueType args
        BTVarApp {} -> False
        BTForall {} -> False

    collectArrowsType ty =
      case ty of
        BTArrow dom cod ->
          let (params, ret) = collectArrowsType cod
           in (dom : params, ret)
        _ -> ([], ty)

    valueKindForType ty
      | isFirstOrderFunctionPointerType ty = LowerFunctionPointer
      | isClosureRuntimeValueType ty = LowerClosureRecord
      | otherwise = LowerRuntimeValue

    isClosureRuntimeValueType =
      \case
        BTForall {} -> True
        BTArrow {} -> True
        _ -> False

data LocalFunction = LocalFunction
  { lfForm :: FunctionForm,
    lfCapturedEnv :: ExprEnv,
    lfStoredReference :: Maybe (BackendType, BackendExpr)
  }
  deriving (Eq, Show)

data ExprEnv = ExprEnv
  { eeValues :: Map String LowerValue,
    eeLocalFunctions :: Map String LocalFunction,
    eeActiveGlobalInlines :: Set String
  }
  deriving (Eq, Show)

exprEnvValueKinds :: ExprEnv -> Map String LowerValueKind
exprEnvValueKinds =
  Map.map lvValueKind . eeValues

data FunctionState = FunctionState
  { fsNextLocal :: Int,
    fsNextBlock :: Int,
    fsCurrentLabel :: String,
    fsCurrentInstructions :: [LLVMInstruction],
    fsCompletedBlocks :: [LLVMBasicBlock]
  }
  deriving (Eq, Show)

type LowerM = StateT FunctionState (Either BackendLLVMError)

atMay :: [a] -> Int -> Maybe a
atMay xs index0
  | index0 < 0 = Nothing
  | otherwise =
      case drop index0 xs of
        (x : _) -> Just x
        [] -> Nothing
