{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module MLF.Backend.LLVM.Lower.Types
  ( BackendLLVMError (..),
    BackendBindingRef,
    BackendExprIdentityKey,
    BackendTypeIdentityKey,
    BindingInfo (..),
    ClosureCaptureSlot (..),
    ClosureEntry (..),
    ClosureEntryIdentityKey,
    ClosureEntryOrigin (..),
    ConstructorRuntime (..),
    ConstructorValueKey,
    ConstructedValue (..),
    DataRuntime (..),
    ExprEnv (..),
    FunctionParam (..),
    FunctionForm (..),
    ffParams,
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
    SpecializationKey,
    Wrapper (..),
    wrapperExpectedType,
    WrapperKey,
    WrapperKind (..),
    atMay,
    backendBindingRefFromGenerated,
    backendBindingRefFromIdentity,
    backendBindingRefIdentity,
    backendExprIdentityKey,
    backendTypeIdentityKey,
    bindingInfoRef,
    combineValueKinds,
    closureEntryIdentityKey,
    constructedFieldValueKind,
    constructedFieldValueKindByKey,
    constructedValueForConstructor,
    constructedValueForConstructorKey,
    constructorValueKeyFromIdentity,
    constructorFieldOffset,
    constructorObjectBytes,
    constructorTagOffset,
    constructorWordBytes,
    lowerLocalKey,
    lookupProgramBindingByIdentityExact,
    lookupProgramConstructorByIdentityExact,
    lookupProgramDataByIdentityExact,
    mergeConstructedValues,
    specializationIdentityKey,
    wrapperIdentityKey,
    pattern LowerValue,
  )
where

import Control.Monad (foldM)
import Control.Monad.State.Strict (StateT)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import MLF.Backend.IR
import MLF.Backend.LLVM.Syntax (LLVMBasicBlock, LLVMFunction, LLVMInstruction, LLVMOperand, LLVMType)
import MLF.Frontend.Symbol (SymbolIdentity, SymbolIdentityPayloadKey, lookupSymbolIdentityExact, sameSymbolIdentity, symbolIdentityPayloadKey)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity (DeferredRef, EnvRef, IdDetails (..), IdentityGenerator, LocalRef, ResolvedTermIdentityKey, TypeBinderIdentity, UniqueIdentity, idDetailsIdentityKey)

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
    peSpecializations :: Map SpecializationKey Specialization,
    peEvidenceWrappers :: Map WrapperKey Wrapper,
    peFunctionWrappers :: Map WrapperKey Wrapper,
    peStringGlobals :: Map String String
  }

data BindingInfo = BindingInfo
  { biIdentity :: SymbolIdentity,
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
  backendBindingRefFromIdentity . biIdentity

lookupProgramBindingByIdentityExact :: ProgramBase -> SymbolIdentity -> Maybe BindingInfo
lookupProgramBindingByIdentityExact base identity =
  lookupSymbolIdentityExact identity (pbBindingsByIdentity base)

lookupProgramConstructorByIdentityExact :: ProgramBase -> SymbolIdentity -> Maybe ConstructorRuntime
lookupProgramConstructorByIdentityExact base identity =
  lookupSymbolIdentityExact identity (pbConstructorsByIdentity base)

lookupProgramDataByIdentityExact :: ProgramBase -> SymbolIdentity -> Maybe DataRuntime
lookupProgramDataByIdentityExact base identity =
  lookupSymbolIdentityExact identity (pbDataByIdentity base)

instance Eq BindingInfo where
  left == right =
    sameSymbolIdentity (biIdentity left) (biIdentity right)
      && biForm left == biForm right
      && biExportedAsMain left == biExportedAsMain right

data FunctionParam = FunctionParam
  { functionParamIdentity :: IdDetails,
    functionParamName :: String,
    functionParamType :: BackendType
  }
  deriving (Show)

instance Eq FunctionParam where
  left == right =
    backendTermRefMatches (functionParamIdentity left) (functionParamIdentity right)
      && functionParamType left == functionParamType right

data FunctionForm = FunctionForm
  { ffTypeBinders :: [BackendTypeBinder],
    ffParameters :: [FunctionParam],
    ffEvidenceParams :: Set Int,
    ffBody :: BackendExpr,
    ffReturnType :: BackendType
  }
  deriving (Show)

ffParams :: FunctionForm -> [(String, BackendType)]
ffParams =
  map (\param -> (functionParamName param, functionParamType param)) . ffParameters

instance Eq FunctionForm where
  left == right =
    ffTypeBinders left == ffTypeBinders right
      && ffParameters left == ffParameters right
      && ffEvidenceParams left == ffEvidenceParams right
      && ffBody left == ffBody right
      && ffReturnType left == ffReturnType right

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
  { srBindingRef :: BackendBindingRef,
    srBindingName :: String,
    srTypeArgs :: [BackendType]
  }
  deriving (Show)

instance Eq SpecRequest where
  left == right =
    srBindingRef left == srBindingRef right
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
    wrapperFunctionName :: String,
    wrapperExpr :: BackendExpr,
    wrapperParameters :: [FunctionParam],
    wrapperReturnType :: BackendType
  }
  deriving (Show)

wrapperExpectedType :: Wrapper -> BackendType
wrapperExpectedType wrapper =
  foldr BTArrow (wrapperReturnType wrapper) (map functionParamType (wrapperParameters wrapper))

instance Eq Wrapper where
  left == right =
    wrapperKind left == wrapperKind right
      && wrapperBindingRef left == wrapperBindingRef right
      && wrapperExpr left == wrapperExpr right
      && wrapperParameters left == wrapperParameters right
      && wrapperReturnType left == wrapperReturnType right

data BackendLiteralIdentityKey
  = BackendIntLiteralKey Integer
  | BackendBoolLiteralKey Bool
  | BackendStringLiteralKey String
  | BackendCharLiteralKey Char
  deriving (Eq, Ord, Show)

data BackendTypeIdentityKey
  = BackendTypeVarKey TypeBinderIdentity
  | BackendTypeArrowKey BackendTypeIdentityKey BackendTypeIdentityKey
  | BackendTypeBaseKey SymbolIdentityPayloadKey
  | BackendTypeConKey SymbolIdentityPayloadKey (NonEmpty BackendTypeIdentityKey)
  | BackendTypeVarAppKey TypeBinderIdentity (NonEmpty BackendTypeIdentityKey)
  | BackendTypeForallKey TypeBinderIdentity (Maybe BackendTypeIdentityKey) BackendTypeIdentityKey
  | BackendTypeMuKey TypeBinderIdentity BackendTypeIdentityKey
  | BackendTypeBottomKey
  deriving (Eq, Ord, Show)

data BackendPatternIdentityKey
  = BackendDefaultPatternKey
  | BackendConstructorPatternKey
      SymbolIdentityPayloadKey
      [ResolvedTermIdentityKey]
  deriving (Eq, Ord, Show)

data BackendExprIdentityKey
  = BackendVarExprKey BackendTypeIdentityKey ResolvedTermIdentityKey
  | BackendLitExprKey BackendTypeIdentityKey BackendLiteralIdentityKey
  | BackendLamExprKey BackendTypeIdentityKey ResolvedTermIdentityKey BackendTypeIdentityKey BackendExprIdentityKey
  | BackendAppExprKey BackendTypeIdentityKey BackendExprIdentityKey BackendExprIdentityKey
  | BackendLetExprKey BackendTypeIdentityKey ResolvedTermIdentityKey BackendTypeIdentityKey BackendExprIdentityKey BackendExprIdentityKey
  | BackendTyAbsExprKey BackendTypeIdentityKey TypeBinderIdentity (Maybe BackendTypeIdentityKey) BackendExprIdentityKey
  | BackendTyAppExprKey BackendTypeIdentityKey BackendExprIdentityKey BackendTypeIdentityKey
  | BackendRollExprKey BackendTypeIdentityKey BackendExprIdentityKey
  | BackendUnrollExprKey BackendTypeIdentityKey BackendExprIdentityKey
  | BackendClosureExprKey
      BackendTypeIdentityKey
      UniqueIdentity
      [ ( ResolvedTermIdentityKey,
          BackendTypeIdentityKey,
          BackendExprIdentityKey
        )
      ]
      [(ResolvedTermIdentityKey, BackendTypeIdentityKey)]
      BackendExprIdentityKey
  | BackendClosureCallExprKey BackendTypeIdentityKey BackendExprIdentityKey [BackendExprIdentityKey]
  | BackendConstructExprKey BackendTypeIdentityKey SymbolIdentityPayloadKey [BackendExprIdentityKey]
  | BackendCaseExprKey BackendTypeIdentityKey BackendExprIdentityKey (NonEmpty (BackendPatternIdentityKey, BackendExprIdentityKey))
  deriving (Eq, Ord, Show)

data SpecializationKey = SpecializationKey BackendBindingRef [BackendTypeIdentityKey]
  deriving (Eq, Ord, Show)

data WrapperKey = WrapperKey BackendTypeIdentityKey BackendExprIdentityKey
  deriving (Eq, Ord, Show)

backendTypeIdentityKey :: BackendType -> BackendTypeIdentityKey
backendTypeIdentityKey =
  \case
    BTVarWithIdentity identity _ ->
      BackendTypeVarKey identity
    BTArrow domain codomain ->
      BackendTypeArrowKey (backendTypeIdentityKey domain) (backendTypeIdentityKey codomain)
    BTBaseWithIdentity identity _ ->
      BackendTypeBaseKey (symbolIdentityPayloadKey identity)
    BTConWithIdentity identity _ args ->
      BackendTypeConKey
        (symbolIdentityPayloadKey identity)
        (fmap backendTypeIdentityKey args)
    BTVarAppWithIdentity identity _ args ->
      BackendTypeVarAppKey identity (fmap backendTypeIdentityKey args)
    BTForallWithIdentity identity _ mbBound body ->
      BackendTypeForallKey
        identity
        (backendTypeIdentityKey <$> mbBound)
        (backendTypeIdentityKey body)
    BTMuWithIdentity identity _ body ->
      BackendTypeMuKey identity (backendTypeIdentityKey body)
    BTBottom ->
      BackendTypeBottomKey

backendExprIdentityKey :: BackendExpr -> BackendExprIdentityKey
backendExprIdentityKey =
  \case
    BackendVarWithIdentity resultTy identity _ ->
      BackendVarExprKey (backendTypeIdentityKey resultTy) (termRefKey identity)
    BackendLit resultTy lit ->
      BackendLitExprKey (backendTypeIdentityKey resultTy) (literalKey lit)
    BackendLamWithIdentity resultTy identity _ paramTy body ->
      BackendLamExprKey
        (backendTypeIdentityKey resultTy)
        (termRefKey identity)
        (backendTypeIdentityKey paramTy)
        (backendExprIdentityKey body)
    BackendApp resultTy fun arg ->
      BackendAppExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey fun) (backendExprIdentityKey arg)
    BackendLetWithIdentity resultTy identity _ bindingTy rhs body ->
      BackendLetExprKey
        (backendTypeIdentityKey resultTy)
        (termRefKey identity)
        (backendTypeIdentityKey bindingTy)
        (backendExprIdentityKey rhs)
        (backendExprIdentityKey body)
    BackendTyAbsWithIdentity resultTy identity _ mbBound body ->
      BackendTyAbsExprKey
        (backendTypeIdentityKey resultTy)
        identity
        (backendTypeIdentityKey <$> mbBound)
        (backendExprIdentityKey body)
    BackendTyApp resultTy fun ty ->
      BackendTyAppExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey fun) (backendTypeIdentityKey ty)
    BackendRoll resultTy payload ->
      BackendRollExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey payload)
    BackendUnroll resultTy payload ->
      BackendUnrollExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey payload)
    BackendClosureWithParamIdentities resultTy entryIdentity _ captures params body ->
      BackendClosureExprKey
        (backendTypeIdentityKey resultTy)
        entryIdentity
        (map captureKey captures)
        (map paramKey params)
        (backendExprIdentityKey body)
    BackendClosureCall resultTy fun args ->
      BackendClosureCallExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey fun) (map backendExprIdentityKey args)
    BackendConstructWithIdentity resultTy identity _ args ->
      BackendConstructExprKey
        (backendTypeIdentityKey resultTy)
        (symbolIdentityPayloadKey identity)
        (map backendExprIdentityKey args)
    BackendCase resultTy scrutinee alternatives ->
      BackendCaseExprKey
        (backendTypeIdentityKey resultTy)
        (backendExprIdentityKey scrutinee)
        (fmap alternativeKey alternatives)
  where
    termRefKey = idDetailsIdentityKey

    literalKey lit =
      case lit of
        LInt value -> BackendIntLiteralKey value
        LBool value -> BackendBoolLiteralKey value
        LString value -> BackendStringLiteralKey value
        LChar value -> BackendCharLiteralKey value

    captureKey capture =
      ( termRefKey (backendClosureCaptureIdentity capture),
        backendTypeIdentityKey (backendClosureCaptureType capture),
        backendExprIdentityKey (backendClosureCaptureExpr capture)
      )

    paramKey param =
      ( termRefKey (backendClosureParamIdentity param),
        backendTypeIdentityKey (backendClosureParamType param)
      )

    alternativeKey (BackendAlternative pattern0 body) =
      (patternKey pattern0, backendExprIdentityKey body)

    patternKey pattern0 =
      case pattern0 of
        BackendDefaultPattern ->
          BackendDefaultPatternKey
        BackendConstructorPatternWithBinderIdentities identity _ binders ->
          BackendConstructorPatternKey
            (symbolIdentityPayloadKey identity)
            [ termRefKey (backendPatternBinderIdentity binder)
            | binder <- binders
            ]

specializationIdentityKey :: SpecRequest -> SpecializationKey
specializationIdentityKey request =
  SpecializationKey (srBindingRef request) (map backendTypeIdentityKey (srTypeArgs request))

wrapperIdentityKey :: BackendType -> BackendExpr -> WrapperKey
wrapperIdentityKey expected expr =
  WrapperKey (backendTypeIdentityKey expected) (backendExprIdentityKey expr)

data ClosureEntryOrigin
  = BackendClosureOrigin
  | GeneratedReturnedPartialOrigin
  deriving (Eq, Show)

newtype ClosureEntryIdentityKey
  = BackendClosureEntryIdentityKey UniqueIdentity
  deriving (Eq, Ord, Show)

data ClosureEntry = ClosureEntry
  { ceOrigin :: ClosureEntryOrigin,
    ceFunctionType :: BackendType,
    ceEntryIdentity :: UniqueIdentity,
    ceEntryName :: String,
    ceCaptures :: [ClosureCaptureSlot],
    ceParameters :: [FunctionParam],
    ceEvidenceParams :: Set Int,
    ceBody :: BackendExpr
  }
  deriving (Show)

{- Note [Identity assignment for qualified closure entries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every closure entry is identity-complete when constructed. Generated entry
seeds live outside this type and are assigned their entry, capture, and
parameter identities before becoming a 'ClosureEntry'.
-}
closureEntryIdentityKey :: ClosureEntry -> ClosureEntryIdentityKey
closureEntryIdentityKey =
  BackendClosureEntryIdentityKey . ceEntryIdentity

instance Eq ClosureEntry where
  left == right =
    ceOrigin left == ceOrigin right
      && ceFunctionType left == ceFunctionType right
      && closureEntryRefMatches (ceEntryIdentity left) (ceEntryIdentity right)
      && ceCaptures left == ceCaptures right
      && ceParameters left == ceParameters right
      && ceEvidenceParams left == ceEvidenceParams right
      && ceBody left == ceBody right

data ClosureCaptureSlot = ClosureCaptureSlot
  { ccsIdentity :: IdDetails,
    ccsName :: String,
    ccsType :: BackendType,
    ccsValueKind :: LowerValueKind
  }
  deriving (Show)

instance Eq ClosureCaptureSlot where
  left == right =
    backendTermRefMatches (ccsIdentity left) (ccsIdentity right)
      && ccsType left == ccsType right
      && ccsValueKind left == ccsValueKind right

data ConstructedValue = ConstructedValue
  { cvFieldValueKindsByConstructor :: Map ConstructorValueKey [LowerValueKind]
  }
  deriving (Eq, Show)

newtype ConstructorValueKey = ConstructorValueKey SymbolIdentity
  deriving (Eq, Ord, Show)

constructorValueKeyFromIdentity :: SymbolIdentity -> ConstructorValueKey
constructorValueKeyFromIdentity =
  ConstructorValueKey

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
    lvBindingRef :: Maybe BackendBindingRef
  }
  deriving (Eq, Show)

pattern LowerValue :: BackendType -> LLVMType -> LLVMOperand -> LowerValueKind -> Maybe ConstructedValue -> LowerValue
pattern LowerValue backendType llvmType operand valueKind constructedValue =
  LowerValueWithIdentity backendType llvmType operand valueKind constructedValue Nothing

{-# COMPLETE LowerValue #-}

data LowerValueKind
  = LowerRuntimeValue
  | LowerClosureRecord
  | LowerFunctionPointer
  deriving (Eq, Ord, Show)

constructedValueForConstructor :: SymbolIdentity -> [LowerValueKind] -> ConstructedValue
constructedValueForConstructor identity =
  constructedValueForConstructorKey (constructorValueKeyFromIdentity identity)

constructedValueForConstructorKey :: ConstructorValueKey -> [LowerValueKind] -> ConstructedValue
constructedValueForConstructorKey key fieldKinds =
  ConstructedValue (Map.singleton key fieldKinds)

constructedFieldValueKind :: SymbolIdentity -> Int -> ConstructedValue -> Maybe LowerValueKind
constructedFieldValueKind identity =
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
