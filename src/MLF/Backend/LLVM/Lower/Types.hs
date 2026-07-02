{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module MLF.Backend.LLVM.Lower.Types
  ( BackendLLVMError (..),
    BackendBindingRef,
    BindingInfo (..),
    ClosureCaptureSlot (..),
    ClosureEntry (..),
    ConstructorRuntime (..),
    ConstructorValueKey,
    ConstructedValue (..),
    DataRuntime (..),
    ExprEnv (..),
    FunctionForm (..),
    FunctionState (..),
    LocalFunction (..),
    LowerM,
    LowerLocalKey,
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
    backendBindingRefFromGenerated,
    backendBindingRefFromIdentity,
    backendBindingRefIdentity,
    bindingInfoRef,
    combineValueKinds,
    constructedFieldValueKind,
    constructedFieldValueKindByKey,
    constructedValueForConstructor,
    constructedValueForConstructorKey,
    constructorValueKeyFromGenerated,
    constructorValueKeyFromIdentity,
    constructorFieldOffset,
    constructorObjectBytes,
    constructorTagOffset,
    constructorWordBytes,
    lowerLocalKey,
    mergeConstructedValues,
    pattern LowerValue,
  )
where

import Control.Monad (foldM)
import Control.Monad.State.Strict (StateT)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import MLF.Backend.IR
import MLF.Backend.IR.Types (backendTermRefMatches, closureEntryRefMatches, symbolRefMatches)
import MLF.Backend.LLVM.Syntax (LLVMBasicBlock, LLVMFunction, LLVMInstruction, LLVMOperand, LLVMType)
import MLF.Frontend.Symbol (SymbolIdentity)
import MLF.Types.Identity (DeferredRef, EnvRef, IdDetails (..), IdentityGenerator, LocalRef, UniqueIdentity)

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
  { pbBindingsByIdentity :: Map SymbolIdentity BindingInfo,
    pbBindingsByRef :: Map BackendBindingRef BindingInfo,
    pbBindingOrder :: [BackendBindingRef],
    pbConstructorsByIdentity :: Map SymbolIdentity ConstructorRuntime,
    pbDataByIdentity :: Map SymbolIdentity DataRuntime,
    pbIdentityGenerator :: IdentityGenerator
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
  { biRef :: BackendBindingRef,
    biIdentity :: Maybe SymbolIdentity,
    biName :: String,
    biForm :: FunctionForm,
    biExportedAsMain :: Bool
  }
  deriving (Show)

data BackendBindingRef
  = BackendBindingByIdentity SymbolIdentity
  | BackendBindingByGenerated UniqueIdentity String
  deriving (Show)

instance Eq BackendBindingRef where
  left == right =
    compare left right == EQ

instance Ord BackendBindingRef where
  compare left right =
    case (left, right) of
      (BackendBindingByIdentity leftIdentity, BackendBindingByIdentity rightIdentity) ->
        compare leftIdentity rightIdentity
      (BackendBindingByGenerated leftIdentity _, BackendBindingByGenerated rightIdentity _) ->
        compare leftIdentity rightIdentity
      (BackendBindingByIdentity {}, _) ->
        LT
      (_, BackendBindingByIdentity {}) ->
        GT

backendBindingRefFromIdentity :: SymbolIdentity -> BackendBindingRef
backendBindingRefFromIdentity =
  BackendBindingByIdentity

backendBindingRefFromGenerated :: UniqueIdentity -> String -> BackendBindingRef
backendBindingRefFromGenerated =
  BackendBindingByGenerated

backendBindingRefIdentity :: BackendBindingRef -> Maybe SymbolIdentity
backendBindingRefIdentity =
  \case
    BackendBindingByIdentity identity -> Just identity
    BackendBindingByGenerated {} -> Nothing

bindingInfoRef :: BindingInfo -> BackendBindingRef
bindingInfoRef =
  biRef

instance Eq BindingInfo where
  left == right =
    biRef left == biRef right
      && symbolRefMatches (biIdentity left) (biName left) (biIdentity right) (biName right)
      && biForm left == biForm right
      && biExportedAsMain left == biExportedAsMain right

data FunctionForm = FunctionForm
  { ffTypeBinders :: [BackendTypeBinder],
    ffParams :: [(String, BackendType)],
    ffParamIdentities :: [Maybe IdDetails],
    ffEvidenceParams :: Set Int,
    ffBody :: BackendExpr,
    ffReturnType :: BackendType
  }
  deriving (Show)

instance Eq FunctionForm where
  left == right =
    ffTypeBinders left == ffTypeBinders right
      && functionFormParamsMatch (ffParamIdentities left) (ffParams left) (ffParamIdentities right) (ffParams right)
      && ffEvidenceParams left == ffEvidenceParams right
      && ffBody left == ffBody right
      && ffReturnType left == ffReturnType right

functionFormParamsMatch :: [Maybe IdDetails] -> [(String, BackendType)] -> [Maybe IdDetails] -> [(String, BackendType)] -> Bool
functionFormParamsMatch leftIdentities leftParams rightIdentities rightParams =
  length leftParams == length rightParams
    && and
      ( zipWith
          functionFormParamMatches
          (zip (leftIdentities ++ repeat Nothing) leftParams)
          (zip (rightIdentities ++ repeat Nothing) rightParams)
      )

functionFormParamMatches :: (Maybe IdDetails, (String, BackendType)) -> (Maybe IdDetails, (String, BackendType)) -> Bool
functionFormParamMatches (leftIdentity, (leftName, leftType)) (rightIdentity, (rightName, rightType)) =
  backendTermRefMatches leftIdentity leftName rightIdentity rightName
    && leftType == rightType

data ConstructorRuntime = ConstructorRuntime
  { crConstructor :: BackendConstructor,
    crData :: BackendData,
    crTag :: Integer,
    crValueKey :: ConstructorValueKey
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
  { srBindingIdentity :: Maybe SymbolIdentity,
    srBindingName :: String,
    srTypeArgs :: [BackendType]
  }
  deriving (Show)

instance Eq SpecRequest where
  left == right =
    symbolRefMatches (srBindingIdentity left) (srBindingName left) (srBindingIdentity right) (srBindingName right)
      && srTypeArgs left == srTypeArgs right

data Specialization = Specialization
  { spRequest :: SpecRequest,
    spBindingRef :: BackendBindingRef,
    spFunctionName :: String,
    spForm :: FunctionForm
  }
  deriving (Show)

instance Eq Specialization where
  left == right =
    spRequest left == spRequest right
      && spBindingRef left == spBindingRef right
      && spForm left == spForm right

data WrapperKind = EvidenceWrapperKind | FunctionWrapperKind
  deriving (Eq, Show)

data Wrapper = Wrapper
  { wrapperKind :: WrapperKind,
    wrapperBindingRef :: BackendBindingRef,
    wrapperKey :: String,
    wrapperFunctionName :: String,
    wrapperExpectedType :: BackendType,
    wrapperExpr :: BackendExpr,
    wrapperParamIdentities :: [Maybe IdDetails]
  }
  deriving (Show)

instance Eq Wrapper where
  left == right =
    wrapperKind left == wrapperKind right
      && wrapperBindingRef left == wrapperBindingRef right
      && wrapperExpectedType left == wrapperExpectedType right
      && wrapperExpr left == wrapperExpr right
      && wrapperParamIdentities left == wrapperParamIdentities right

data ClosureEntry = ClosureEntry
  { ceFunctionType :: BackendType,
    ceEntryIdentity :: Maybe UniqueIdentity,
    ceEntryName :: String,
    ceCaptures :: [ClosureCaptureSlot],
    ceParams :: [(String, BackendType)],
    ceParamIdentities :: [Maybe IdDetails],
    ceEvidenceParams :: Set Int,
    ceBody :: BackendExpr
  }
  deriving (Show)

instance Eq ClosureEntry where
  left == right =
    ceFunctionType left == ceFunctionType right
      && closureEntryRefMatches (ceEntryIdentity left) (ceEntryName left) (ceEntryIdentity right) (ceEntryName right)
      && ceCaptures left == ceCaptures right
      && functionFormParamsMatch (ceParamIdentities left) (ceParams left) (ceParamIdentities right) (ceParams right)
      && ceEvidenceParams left == ceEvidenceParams right
      && ceBody left == ceBody right

data ClosureCaptureSlot = ClosureCaptureSlot
  { ccsIdentity :: Maybe IdDetails,
    ccsName :: String,
    ccsType :: BackendType,
    ccsValueKind :: LowerValueKind
  }
  deriving (Show)

instance Eq ClosureCaptureSlot where
  left == right =
    backendTermRefMatches (ccsIdentity left) (ccsName left) (ccsIdentity right) (ccsName right)
      && ccsType left == ccsType right
      && ccsValueKind left == ccsValueKind right

data ConstructedValue = ConstructedValue
  { cvFieldValueKindsByConstructor :: Map ConstructorValueKey [LowerValueKind]
  }
  deriving (Eq, Show)

data ConstructorValueKey
  = ConstructorValueByIdentity SymbolIdentity
  | ConstructorValueByGenerated UniqueIdentity String
  deriving (Eq, Ord, Show)

constructorValueKeyFromIdentity :: SymbolIdentity -> ConstructorValueKey
constructorValueKeyFromIdentity =
  ConstructorValueByIdentity

constructorValueKeyFromGenerated :: UniqueIdentity -> String -> ConstructorValueKey
constructorValueKeyFromGenerated =
  ConstructorValueByGenerated

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
  deriving (Show)

instance Eq NativeRenderSpec where
  left == right =
    nrsType left == nrsType right

data LowerValue = LowerValueWithIdentity
  { lvBackendType :: BackendType,
    lvLLVMType :: LLVMType,
    lvOperand :: LLVMOperand,
    lvValueKind :: LowerValueKind,
    lvConstructedValue :: Maybe ConstructedValue,
    lvSymbolIdentity :: Maybe SymbolIdentity,
    lvBindingRef :: Maybe BackendBindingRef
  }
  deriving (Eq, Show)

pattern LowerValue :: BackendType -> LLVMType -> LLVMOperand -> LowerValueKind -> Maybe ConstructedValue -> LowerValue
pattern LowerValue backendType llvmType operand valueKind constructedValue =
  LowerValueWithIdentity backendType llvmType operand valueKind constructedValue Nothing Nothing

{-# COMPLETE LowerValue #-}

data LowerValueKind
  = LowerRuntimeValue
  | LowerClosureRecord
  | LowerFunctionPointer
  deriving (Eq, Show)

constructedValueForConstructor :: SymbolIdentity -> String -> [LowerValueKind] -> ConstructedValue
constructedValueForConstructor identity _name =
  constructedValueForConstructorKey (constructorValueKeyFromIdentity identity)

constructedValueForConstructorKey :: ConstructorValueKey -> [LowerValueKind] -> ConstructedValue
constructedValueForConstructorKey key fieldKinds =
  ConstructedValue (Map.singleton key fieldKinds)

constructedFieldValueKind :: SymbolIdentity -> String -> Int -> ConstructedValue -> Maybe LowerValueKind
constructedFieldValueKind identity _constructorName =
  constructedFieldValueKindByKey (constructorValueKeyFromIdentity identity)

constructedFieldValueKindByKey :: ConstructorValueKey -> Int -> ConstructedValue -> Maybe LowerValueKind
constructedFieldValueKindByKey key index0 constructed =
  Map.lookup key (cvFieldValueKindsByConstructor constructed) >>= flip atMay index0

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
        BTMu _ body -> isFirstOrderPointerValueType body
        BTBottom -> False

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
  { lfName :: String,
    lfForm :: FunctionForm,
    lfCapturedEnv :: ExprEnv,
    lfStoredReference :: Maybe (BackendType, BackendExpr)
  }
  deriving (Show)

instance Eq LocalFunction where
  left == right =
    lfForm left == lfForm right
      && lfCapturedEnv left == lfCapturedEnv right
      && lfStoredReference left == lfStoredReference right

data ExprEnv = ExprEnv
  { eeValuesByIdentity :: Map LowerLocalKey LowerValue,
    eeLocalFunctionsByIdentity :: Map LowerLocalKey LocalFunction,
    eeActiveGlobalInlines :: Set BackendBindingRef
  }
  deriving (Eq, Show)

data LowerLocalKey
  = LowerLocalRef LocalRef
  | LowerEnvRef EnvRef
  | LowerDeferredRef DeferredRef
  deriving (Eq, Ord, Show)

lowerLocalKey :: IdDetails -> Maybe LowerLocalKey
lowerLocalKey =
  \case
    LocalId ref -> Just (LowerLocalRef ref)
    EvidenceId ref -> Just (LowerLocalRef ref)
    EnvId ref -> Just (LowerEnvRef ref)
    DeferredId ref -> Just (LowerDeferredRef ref)
    TopLevelId {} -> Nothing
    ConstructorId {} -> Nothing
    MethodId {} -> Nothing
    PrimitiveId {} -> Nothing

data FunctionState = FunctionState
  { fsNextLocal :: Int,
    fsNextBlock :: Int,
    fsIdentityGenerator :: IdentityGenerator,
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
