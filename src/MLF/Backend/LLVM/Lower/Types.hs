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
    SpecializationKey,
    Wrapper (..),
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
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Symbol (SymbolIdentity, SymbolIdentityPayloadKey, lookupSymbolIdentityExact, symbolIdentityPayloadKey, symbolRefMatchesWith)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity (DeferredRef, EnvRef, IdDetails (..), IdentityGenerator, LocalRef, ResolvedTermIdentityKey, TypeBinderIdentity, UniqueIdentity, idDetailsIdentityKey)
import MLF.Types.Reference (ReferenceMode (..))

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

lookupProgramBindingByIdentityExact :: ProgramBase -> Maybe SymbolIdentity -> Maybe BindingInfo
lookupProgramBindingByIdentityExact base mbIdentity =
  mbIdentity >>= (`lookupSymbolIdentityExact` pbBindingsByIdentity base)

lookupProgramConstructorByIdentityExact :: ProgramBase -> Maybe SymbolIdentity -> Maybe ConstructorRuntime
lookupProgramConstructorByIdentityExact base mbIdentity =
  mbIdentity >>= (`lookupSymbolIdentityExact` pbConstructorsByIdentity base)

lookupProgramDataByIdentityExact :: ProgramBase -> Maybe SymbolIdentity -> Maybe DataRuntime
lookupProgramDataByIdentityExact base mbIdentity =
  mbIdentity >>= (`lookupSymbolIdentityExact` pbDataByIdentity base)

instance Eq BindingInfo where
  left == right =
    biRef left == biRef right
      && bindingInfoIdentityMatches left right
      && biForm left == biForm right
      && biExportedAsMain left == biExportedAsMain right

bindingInfoIdentityMatches :: BindingInfo -> BindingInfo -> Bool
bindingInfoIdentityMatches left right =
  case (biIdentity left, biIdentity right) of
    (Just leftIdentity, Just rightIdentity) ->
      symbolRefMatchesWith IdentityOnly (Just leftIdentity) (biName left) (Just rightIdentity) (biName right)
    (Nothing, Nothing) ->
      backendBindingRefIdentity (biRef left) == Nothing && backendBindingRefIdentity (biRef right) == Nothing
    _ ->
      False

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
  backendTermRefMatchesWith IdentityOnly leftIdentity leftName rightIdentity rightName
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

data IdentityRefKey identity
  = IdentityRefKey identity
  | MetadataLightNameRefKey String
  deriving (Eq, Ord, Show)

data BackendLiteralIdentityKey
  = BackendIntLiteralKey Integer
  | BackendBoolLiteralKey Bool
  | BackendStringLiteralKey String
  | BackendCharLiteralKey Char
  deriving (Eq, Ord, Show)

data BackendTypeIdentityKey
  = BackendTypeVarKey (IdentityRefKey TypeBinderIdentity)
  | BackendTypeArrowKey BackendTypeIdentityKey BackendTypeIdentityKey
  | BackendTypeBaseKey (IdentityRefKey SymbolIdentityPayloadKey)
  | BackendTypeConKey (IdentityRefKey SymbolIdentityPayloadKey) (NonEmpty BackendTypeIdentityKey)
  | BackendTypeVarAppKey (IdentityRefKey TypeBinderIdentity) (NonEmpty BackendTypeIdentityKey)
  | BackendTypeForallKey (IdentityRefKey TypeBinderIdentity) (Maybe BackendTypeIdentityKey) BackendTypeIdentityKey
  | BackendTypeMuKey (IdentityRefKey TypeBinderIdentity) BackendTypeIdentityKey
  | BackendTypeBottomKey
  deriving (Eq, Ord, Show)

data BackendPatternIdentityKey
  = BackendDefaultPatternKey
  | BackendConstructorPatternKey
      (IdentityRefKey SymbolIdentityPayloadKey)
      [IdentityRefKey ResolvedTermIdentityKey]
  deriving (Eq, Ord, Show)

data BackendExprIdentityKey
  = BackendVarExprKey BackendTypeIdentityKey (IdentityRefKey ResolvedTermIdentityKey)
  | BackendLitExprKey BackendTypeIdentityKey BackendLiteralIdentityKey
  | BackendLamExprKey BackendTypeIdentityKey (IdentityRefKey ResolvedTermIdentityKey) BackendTypeIdentityKey BackendExprIdentityKey
  | BackendAppExprKey BackendTypeIdentityKey BackendExprIdentityKey BackendExprIdentityKey
  | BackendLetExprKey BackendTypeIdentityKey (IdentityRefKey ResolvedTermIdentityKey) BackendTypeIdentityKey BackendExprIdentityKey BackendExprIdentityKey
  | BackendTyAbsExprKey BackendTypeIdentityKey (IdentityRefKey TypeBinderIdentity) (Maybe BackendTypeIdentityKey) BackendExprIdentityKey
  | BackendTyAppExprKey BackendTypeIdentityKey BackendExprIdentityKey BackendTypeIdentityKey
  | BackendRollExprKey BackendTypeIdentityKey BackendExprIdentityKey
  | BackendUnrollExprKey BackendTypeIdentityKey BackendExprIdentityKey
  | BackendClosureExprKey
      BackendTypeIdentityKey
      (IdentityRefKey UniqueIdentity)
      [ ( IdentityRefKey ResolvedTermIdentityKey,
          BackendTypeIdentityKey,
          BackendExprIdentityKey
        )
      ]
      [(IdentityRefKey ResolvedTermIdentityKey, BackendTypeIdentityKey)]
      BackendExprIdentityKey
  | BackendClosureCallExprKey BackendTypeIdentityKey BackendExprIdentityKey [BackendExprIdentityKey]
  | BackendConstructExprKey BackendTypeIdentityKey (IdentityRefKey SymbolIdentityPayloadKey) [BackendExprIdentityKey]
  | BackendCaseExprKey BackendTypeIdentityKey BackendExprIdentityKey (NonEmpty (BackendPatternIdentityKey, BackendExprIdentityKey))
  deriving (Eq, Ord, Show)

data SpecializationKey = SpecializationKey BackendBindingRef [BackendTypeIdentityKey]
  deriving (Eq, Ord, Show)

data WrapperKey = WrapperKey BackendTypeIdentityKey BackendExprIdentityKey
  deriving (Eq, Ord, Show)

identityRefKey :: Maybe identity -> String -> IdentityRefKey identity
identityRefKey mbIdentity name =
  case mbIdentity of
    Just identity -> IdentityRefKey identity
    Nothing -> MetadataLightNameRefKey name

backendTypeIdentityKey :: BackendType -> BackendTypeIdentityKey
backendTypeIdentityKey =
  \case
    BTVarWithIdentity identity name ->
      BackendTypeVarKey (identityRefKey identity name)
    BTArrow domain codomain ->
      BackendTypeArrowKey (backendTypeIdentityKey domain) (backendTypeIdentityKey codomain)
    BTBaseWithIdentity identity (BaseTy name) ->
      BackendTypeBaseKey (identityRefKey (symbolIdentityPayloadKey <$> identity) name)
    BTConWithIdentity identity (BaseTy name) args ->
      BackendTypeConKey
        (identityRefKey (symbolIdentityPayloadKey <$> identity) name)
        (fmap backendTypeIdentityKey args)
    BTVarAppWithIdentity identity name args ->
      BackendTypeVarAppKey (identityRefKey identity name) (fmap backendTypeIdentityKey args)
    BTForallWithIdentity identity name mbBound body ->
      BackendTypeForallKey
        (identityRefKey identity name)
        (backendTypeIdentityKey <$> mbBound)
        (backendTypeIdentityKey body)
    BTMuWithIdentity identity name body ->
      BackendTypeMuKey (identityRefKey identity name) (backendTypeIdentityKey body)
    BTBottom ->
      BackendTypeBottomKey

backendExprIdentityKey :: BackendExpr -> BackendExprIdentityKey
backendExprIdentityKey =
  \case
    BackendVarWithIdentity resultTy identity name ->
      BackendVarExprKey (backendTypeIdentityKey resultTy) (termRefKey identity name)
    BackendLit resultTy lit ->
      BackendLitExprKey (backendTypeIdentityKey resultTy) (literalKey lit)
    BackendLamWithIdentity resultTy identity name paramTy body ->
      BackendLamExprKey
        (backendTypeIdentityKey resultTy)
        (termRefKey identity name)
        (backendTypeIdentityKey paramTy)
        (backendExprIdentityKey body)
    BackendApp resultTy fun arg ->
      BackendAppExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey fun) (backendExprIdentityKey arg)
    BackendLetWithIdentity resultTy identity name bindingTy rhs body ->
      BackendLetExprKey
        (backendTypeIdentityKey resultTy)
        (termRefKey identity name)
        (backendTypeIdentityKey bindingTy)
        (backendExprIdentityKey rhs)
        (backendExprIdentityKey body)
    BackendTyAbsWithIdentity resultTy identity name mbBound body ->
      BackendTyAbsExprKey
        (backendTypeIdentityKey resultTy)
        (identityRefKey identity name)
        (backendTypeIdentityKey <$> mbBound)
        (backendExprIdentityKey body)
    BackendTyApp resultTy fun ty ->
      BackendTyAppExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey fun) (backendTypeIdentityKey ty)
    BackendRoll resultTy payload ->
      BackendRollExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey payload)
    BackendUnroll resultTy payload ->
      BackendUnrollExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey payload)
    BackendClosureWithParamIdentities resultTy entryIdentity entryName captures params body ->
      BackendClosureExprKey
        (backendTypeIdentityKey resultTy)
        (identityRefKey entryIdentity entryName)
        (map captureKey captures)
        (map paramKey params)
        (backendExprIdentityKey body)
    BackendClosureCall resultTy fun args ->
      BackendClosureCallExprKey (backendTypeIdentityKey resultTy) (backendExprIdentityKey fun) (map backendExprIdentityKey args)
    BackendConstructWithIdentity resultTy identity name args ->
      BackendConstructExprKey
        (backendTypeIdentityKey resultTy)
        (identityRefKey (symbolIdentityPayloadKey <$> identity) name)
        (map backendExprIdentityKey args)
    BackendCase resultTy scrutinee alternatives ->
      BackendCaseExprKey
        (backendTypeIdentityKey resultTy)
        (backendExprIdentityKey scrutinee)
        (fmap alternativeKey alternatives)
  where
    termRefKey identity name =
      identityRefKey (idDetailsIdentityKey <$> identity) name

    literalKey lit =
      case lit of
        LInt value -> BackendIntLiteralKey value
        LBool value -> BackendBoolLiteralKey value
        LString value -> BackendStringLiteralKey value
        LChar value -> BackendCharLiteralKey value

    captureKey capture =
      ( termRefKey (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture),
        backendTypeIdentityKey (backendClosureCaptureType capture),
        backendExprIdentityKey (backendClosureCaptureExpr capture)
      )

    paramKey param =
      ( termRefKey (backendClosureParamIdentity param) (backendClosureParamName param),
        backendTypeIdentityKey (backendClosureParamType param)
      )

    alternativeKey (BackendAlternative pattern0 body) =
      (patternKey pattern0, backendExprIdentityKey body)

    patternKey pattern0 =
      case pattern0 of
        BackendDefaultPattern ->
          BackendDefaultPatternKey
        BackendConstructorPatternWithBinderIdentities identity name binders ->
          BackendConstructorPatternKey
            (identityRefKey (symbolIdentityPayloadKey <$> identity) name)
            [ termRefKey (backendPatternBinderIdentity binder) (backendPatternBinderName binder)
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

data ClosureEntryIdentityKey
  = BackendClosureEntryIdentityKey UniqueIdentity
  | UnassignedBackendClosureEntryIdentityKey
      String
      BackendTypeIdentityKey
      [ ( IdentityRefKey ResolvedTermIdentityKey,
          LowerValueKind,
          BackendTypeIdentityKey
        )
      ]
      [(IdentityRefKey ResolvedTermIdentityKey, BackendTypeIdentityKey)]
      (Set Int)
      BackendExprIdentityKey
  | ReturnedPartialClosureEntryIdentityKey
      LowerValueKind
      BackendTypeIdentityKey
      [(LowerValueKind, BackendTypeIdentityKey)]
      [BackendTypeIdentityKey]
      (Set Int)
      BackendTypeIdentityKey
  deriving (Eq, Ord, Show)

data ClosureEntry = ClosureEntry
  { ceOrigin :: ClosureEntryOrigin,
    ceFunctionType :: BackendType,
    ceEntryIdentity :: Maybe UniqueIdentity,
    ceEntryName :: String,
    ceCaptures :: [ClosureCaptureSlot],
    ceParams :: [(String, BackendType)],
    ceParamIdentities :: [Maybe IdDetails],
    ceEvidenceParams :: Set Int,
    ceBody :: BackendExpr
  }
  deriving (Show)

{- Note [Identity assignment for qualified closure entries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Production backend closures carry an entry identity.  Specialization
qualification deliberately clears it because one source closure can yield
several emitted entries.  Before fresh identities are assigned, repeated
collection of the same qualified entry must share one generated identity;
otherwise the two copies become falsely distinct.

The qualified emission name is used only in this unassigned construction key,
together with the full identity-bearing closure payload.  It is not a semantic
reference after assignment.  Returned-partial entries do not need that seam:
their callable/capture/parameter shape is their complete synthetic origin, so
their emitted names remain absent from the key.
-}
closureEntryIdentityKey :: ClosureEntry -> Maybe ClosureEntryIdentityKey
closureEntryIdentityKey entry =
  case ceOrigin entry of
    BackendClosureOrigin ->
      case ceEntryIdentity entry of
        Just identity ->
          Just (BackendClosureEntryIdentityKey identity)
        Nothing ->
          Just
            ( UnassignedBackendClosureEntryIdentityKey
                (ceEntryName entry)
                (backendTypeIdentityKey (ceFunctionType entry))
                [ ( termIdentityRefKey (ccsIdentity capture) (ccsName capture),
                    ccsValueKind capture,
                    backendTypeIdentityKey (ccsType capture)
                  )
                | capture <- ceCaptures entry
                ]
                [ ( termIdentityRefKey mbIdentity name,
                    backendTypeIdentityKey paramTy
                  )
                | ((name, paramTy), mbIdentity) <- zip (ceParams entry) (ceParamIdentities entry ++ repeat Nothing)
                ]
                (ceEvidenceParams entry)
                (backendExprIdentityKey (ceBody entry))
            )
    GeneratedReturnedPartialOrigin ->
      case ceCaptures entry of
        callee : supplied ->
          Just
            ( ReturnedPartialClosureEntryIdentityKey
                (ccsValueKind callee)
                (backendTypeIdentityKey (ccsType callee))
                [ (ccsValueKind capture, backendTypeIdentityKey (ccsType capture))
                  | capture <- supplied
                ]
                (map (backendTypeIdentityKey . snd) (ceParams entry))
                (ceEvidenceParams entry)
                (backendTypeIdentityKey (ceFunctionType entry))
            )
        [] ->
          Nothing

termIdentityRefKey :: Maybe IdDetails -> String -> IdentityRefKey ResolvedTermIdentityKey
termIdentityRefKey mbIdentity name =
  identityRefKey (idDetailsIdentityKey <$> mbIdentity) name

instance Eq ClosureEntry where
  left == right =
    ceOrigin left == ceOrigin right
      && ceFunctionType left == ceFunctionType right
      && closureEntryRefMatchesWith IdentityOnly (ceEntryIdentity left) (ceEntryName left) (ceEntryIdentity right) (ceEntryName right)
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
    backendTermRefMatchesWith IdentityOnly (ccsIdentity left) (ccsName left) (ccsIdentity right) (ccsName right)
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
  deriving (Eq, Ord, Show)

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
