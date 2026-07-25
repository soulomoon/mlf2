{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module MLF.Backend.IR.Types
  ( BackendProgram
      ( BackendProgramWithIdentity,
        backendProgramModulesWithIdentity,
        backendProgramMainIdentity,
        backendProgramMainWithIdentity
      ),
    pattern BackendProgram,
    backendProgramModules,
    backendProgramMain,
    BackendModule
      ( BackendModuleWithIdentity,
        backendModuleIdentity,
        backendModuleNameWithIdentity,
        backendModuleDataWithIdentity,
        backendModuleBindingsWithIdentity
      ),
    pattern BackendModule,
    backendModuleName,
    backendModuleData,
    backendModuleBindings,
    BackendBinding
      ( BackendBindingWithMetadata,
        backendBindingIdentity,
        backendBindingNameWithMetadata,
        backendBindingTypeWithMetadata,
        backendBindingExprWithMetadata,
        backendBindingExportedAsMainWithMetadata,
        backendBindingEvidenceParamIndices
      ),
    pattern BackendBinding,
    backendBindingName,
    backendBindingType,
    backendBindingExpr,
    backendBindingExportedAsMain,
    BackendData
      ( BackendDataWithIdentity,
        backendDataIdentity,
        backendDataNameWithIdentity,
        backendDataParameterRefsWithIdentity,
        backendDataConstructorsWithIdentity
      ),
    pattern BackendData,
    backendDataName,
    backendDataParameters,
    BackendDataParameterRef,
    backendDataParameterRefFromIdentity,
    backendDataParameterRefIdentity,
    backendDataParameterRefName,
    backendDataParameterRefKey,
    backendDataParameterRefType,
    backendDataParameterRefs,
    backendDataParameterKeys,
    backendDataConstructors,
    BackendConstructor (..),
    pattern BackendConstructor,
    backendConstructorName,
    backendConstructorForalls,
    backendConstructorFields,
    backendConstructorResult,
    BackendClosureCapture (..),
    BackendClosureParam (..),
    BackendTypeBinder
      ( BackendTypeBinderWithIdentity,
        backendTypeBinderIdentity,
        backendTypeBinderName,
        backendTypeBinderBound
      ),
    pattern BackendTypeBinder,
    BackendType (..),
    BackendTypeSubstitutionKey,
    backendTypeSubstitutionKeyFromIdentity,
    backendTypeSubstitutionKeyIdentity,
    backendTypeSubstitutionKeyName,
    pattern BTVar,
    pattern BTBase,
    pattern BTCon,
    pattern BTVarApp,
    pattern BTForall,
    pattern BTMu,
    BackendExpr (..),
    backendVarWithResolvedIdentity,
    backendLamWithResolvedIdentity,
    backendLetWithResolvedIdentity,
    backendClosureWithResolvedEntry,
    backendConstructWithResolvedIdentity,
    pattern BackendVar,
    pattern BackendLam,
    pattern BackendLet,
    pattern BackendTyAbs,
    pattern BackendClosure,
    backendClosureParams,
    pattern BackendConstruct,
    BackendAlternative (..),
    backendClosureCaptureWithResolvedIdentity,
    backendClosureParamWithResolvedIdentity,
    BackendPatternBinder (..),
    backendPatternBinderWithResolvedIdentity,
    BackendPattern (..),
    backendConstructorPatternWithResolvedIdentity,
    pattern BackendConstructorPattern,
    freeBackendTypeVars,
    freeBackendTypeVarsInKeyed,
    freeBackendTypeVarRefs,
    freeBackendTypeVarKeys,
    freeBackendTypeVarKeysInKeyed,
    generatedIdentitiesInBackendProgram,
    generatedIdentitiesInBackendTypes,
    generatedIdentitiesInBackendExpr,
    typeBinderRefMatches,
    backendTypeHeadMatches,
    backendTypeRefinesScrutinee,
    backendTermRefMatches,
    closureEntryRefMatches,
    literalBackendType,
    substituteBackendTypeByIdentity,
    substituteBackendTypeForBinder,
    substituteBackendTypesByKey,
    unfoldBackendRecursiveType,
  )
where

import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Program.Builtins (builtinTypeIdentity)
import MLF.Frontend.Symbol (SymbolIdentity, sameSymbolIdentity)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity
  ( IdDetails,
    IdentityGenerator,
    TypeBinderIdentity,
    UniqueIdentity (..),
    freshIdentity,
    idDetailsGeneratedIdentities,
    idDetailsSameIdentity,
    identityGeneratorAfter,
    symbolGeneratedIdentities,
    typeBinderGeneratedIdentities,
    typeBinderIdentityAliasNames,
    typeBinderIdentityFromStructural,
    typeBinderIdentityFromUnique,
    typeBinderIdentityStableName,
    typeBinderIdentityStructural,
  )
import MLF.Util.Names (freshNameLike)

{- Note [Backend IR identity-complete production references]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Post-resolution Backend IR references are semantic identities plus
display/runtime names. The names remain useful for diagnostics and generated
symbols, but production construction should go through the resolved helpers in
this module and production validation in MLF.Backend.IR rejects missing
identities.

Every semantic reference below carries its identity in the constructor. Raw
spellings belong to parser or fixture types outside this production IR.
-}

-- | A checked backend program. Module order is preserved from the source
-- program for diagnostics/debug output, but backend binding names are global
-- runtime names.
data BackendProgram = BackendProgramWithIdentity
  { backendProgramModulesWithIdentity :: [BackendModule],
    backendProgramMainIdentity :: SymbolIdentity,
    backendProgramMainWithIdentity :: String
  }
  deriving (Show)

instance Eq BackendProgram where
  left == right =
    backendProgramModules left == backendProgramModules right
      && sameSymbolIdentity (backendProgramMainIdentity left) (backendProgramMainIdentity right)

-- Identity-erasing view; 'BackendProgramWithIdentity' is the only constructor.
pattern BackendProgram :: [BackendModule] -> String -> BackendProgram
pattern BackendProgram
  { backendProgramModules,
    backendProgramMain
  } <-
  BackendProgramWithIdentity
    backendProgramModules
    _
    backendProgramMain

{-# COMPLETE BackendProgram #-}

-- | Backend-owned module payload. Imports/exports have already been resolved
-- by the `.mlfp` checker; this record keeps only the data and binding shapes
-- needed by backend conversion and lowering.
data BackendModule = BackendModuleWithIdentity
  { backendModuleIdentity :: SymbolIdentity,
    backendModuleNameWithIdentity :: String,
    backendModuleDataWithIdentity :: [BackendData],
    backendModuleBindingsWithIdentity :: [BackendBinding]
  }
  deriving (Show)

instance Eq BackendModule where
  left == right =
    sameSymbolIdentity (backendModuleIdentity left) (backendModuleIdentity right)
      && backendModuleData left == backendModuleData right
      && backendModuleBindings left == backendModuleBindings right

-- Identity-erasing view; 'BackendModuleWithIdentity' is the only constructor.
pattern BackendModule :: String -> [BackendData] -> [BackendBinding] -> BackendModule
pattern BackendModule
  { backendModuleName,
    backendModuleData,
    backendModuleBindings
  } <-
  BackendModuleWithIdentity
    _
    backendModuleName
    backendModuleData
    backendModuleBindings

{-# COMPLETE BackendModule #-}

-- | Explicit ADT metadata available to lowerers. Constructor result types are
-- kept explicit so GADT-style results can survive the source-to-backend cut.
data BackendDataParameterRef
  = BackendDataParameterByIdentity TypeBinderIdentity String
  deriving (Show)

instance Eq BackendDataParameterRef where
  left == right =
    compare left right == EQ

instance Ord BackendDataParameterRef where
  compare left right =
    case (left, right) of
      (BackendDataParameterByIdentity leftIdentity _, BackendDataParameterByIdentity rightIdentity _) ->
        compare leftIdentity rightIdentity

backendDataParameterRefFromIdentity :: TypeBinderIdentity -> String -> BackendDataParameterRef
backendDataParameterRefFromIdentity =
  BackendDataParameterByIdentity

backendDataParameterRefIdentity :: BackendDataParameterRef -> TypeBinderIdentity
backendDataParameterRefIdentity =
  \case
    BackendDataParameterByIdentity identity _ -> identity

backendDataParameterRefName :: BackendDataParameterRef -> String
backendDataParameterRefName =
  \case
    BackendDataParameterByIdentity _ name -> name

data BackendData = BackendDataWithIdentity
  { backendDataIdentity :: SymbolIdentity,
    backendDataNameWithIdentity :: String,
    backendDataParameterRefsWithIdentity :: [BackendDataParameterRef],
    backendDataConstructorsWithIdentity :: [BackendConstructor]
  }
  deriving (Show)

instance Eq BackendData where
  left == right =
    sameSymbolIdentity (backendDataIdentity left) (backendDataIdentity right)
      && backendDataParameterRefs left == backendDataParameterRefs right
      && backendDataConstructors left == backendDataConstructors right

-- Identity-erasing view used by renderers and compact test pattern matches.
-- Construction remains identity-complete through 'BackendDataWithIdentity'.
pattern BackendData :: String -> [String] -> [BackendConstructor] -> BackendData
pattern BackendData
  { backendDataName,
    backendDataParameters,
    backendDataConstructors
  } <-
  BackendDataWithIdentity
    _
    backendDataName
    (map backendDataParameterRefName -> backendDataParameters)
    backendDataConstructors

{-# COMPLETE BackendData #-}

backendDataParameterRefKey :: BackendDataParameterRef -> BackendTypeSubstitutionKey
backendDataParameterRefKey =
  \case
    BackendDataParameterByIdentity identity _ -> backendTypeSubstitutionKeyFromIdentity identity

backendDataParameterRefType :: BackendDataParameterRef -> BackendType
backendDataParameterRefType ref =
  BTVarWithIdentity (backendDataParameterRefIdentity ref) (backendDataParameterRefName ref)

backendDataParameterRefs :: BackendData -> [BackendDataParameterRef]
backendDataParameterRefs =
  backendDataParameterRefsWithIdentity

backendDataParameterKeys :: BackendData -> [BackendTypeSubstitutionKey]
backendDataParameterKeys dataDecl =
  map backendDataParameterRefKey (backendDataParameterRefs dataDecl)

data BackendConstructor = BackendConstructorWithIdentity
  { backendConstructorIdentity :: SymbolIdentity,
    backendConstructorNameWithIdentity :: String,
    backendConstructorForallsWithIdentity :: [BackendTypeBinder],
    backendConstructorFieldsWithIdentity :: [BackendType],
    backendConstructorResultWithIdentity :: BackendType
  }
  deriving (Show)

instance Eq BackendConstructor where
  left == right =
    sameSymbolIdentity (backendConstructorIdentity left) (backendConstructorIdentity right)
      && backendConstructorForalls left == backendConstructorForalls right
      && backendConstructorFields left == backendConstructorFields right
      && backendConstructorResult left == backendConstructorResult right

-- Identity-erasing view; 'BackendConstructorWithIdentity' is the only
-- constructor.
pattern BackendConstructor :: String -> [BackendTypeBinder] -> [BackendType] -> BackendType -> BackendConstructor
pattern BackendConstructor
  { backendConstructorName,
    backendConstructorForalls,
    backendConstructorFields,
    backendConstructorResult
  } <-
  BackendConstructorWithIdentity
    _
    backendConstructorName
    backendConstructorForalls
    backendConstructorFields
    backendConstructorResult

{-# COMPLETE BackendConstructor #-}

data BackendClosureCapture = BackendClosureCapture
  { backendClosureCaptureIdentity :: IdDetails,
    backendClosureCaptureName :: String,
    backendClosureCaptureType :: BackendType,
    backendClosureCaptureExpr :: BackendExpr
  }
  deriving (Show)

instance Eq BackendClosureCapture where
  left == right =
    idDetailsSameIdentity (backendClosureCaptureIdentity left) (backendClosureCaptureIdentity right)
      && backendClosureCaptureType left == backendClosureCaptureType right
      && backendClosureCaptureExpr left == backendClosureCaptureExpr right

data BackendClosureParam = BackendClosureParam
  { backendClosureParamIdentity :: IdDetails,
    backendClosureParamName :: String,
    backendClosureParamType :: BackendType
  }
  deriving (Show)

instance Eq BackendClosureParam where
  left == right =
    idDetailsSameIdentity (backendClosureParamIdentity left) (backendClosureParamIdentity right)
      && backendClosureParamType left == backendClosureParamType right

data BackendTypeBinder = BackendTypeBinderWithIdentity
  { backendTypeBinderIdentity :: TypeBinderIdentity,
    backendTypeBinderName :: String,
    backendTypeBinderBound :: Maybe BackendType
  }
  deriving (Show)

instance Eq BackendTypeBinder where
  left == right =
    backendTypeBinderIdentity left == backendTypeBinderIdentity right
      && backendTypeBinderBound left == backendTypeBinderBound right

-- Identity-erasing view; 'BackendTypeBinderWithIdentity' is the only
-- constructor.
pattern BackendTypeBinder :: String -> Maybe BackendType -> BackendTypeBinder
pattern BackendTypeBinder name bound <-
  BackendTypeBinderWithIdentity _ name bound

{-# COMPLETE BackendTypeBinder #-}

data BackendBinding = BackendBindingWithMetadata
  { backendBindingIdentity :: SymbolIdentity,
    backendBindingNameWithMetadata :: String,
    backendBindingTypeWithMetadata :: BackendType,
    backendBindingExprWithMetadata :: BackendExpr,
    backendBindingExportedAsMainWithMetadata :: Bool,
    backendBindingEvidenceParamIndices :: Set.Set Int
  }
  deriving (Show)

instance Eq BackendBinding where
  left == right =
    sameSymbolIdentity (backendBindingIdentity left) (backendBindingIdentity right)
      && backendBindingType left == backendBindingType right
      && backendBindingExpr left == backendBindingExpr right
      && backendBindingExportedAsMain left == backendBindingExportedAsMain right
      && backendBindingEvidenceParamIndices left == backendBindingEvidenceParamIndices right

-- Identity-erasing view; 'BackendBindingWithMetadata' is the only constructor.
pattern BackendBinding :: String -> BackendType -> BackendExpr -> Bool -> BackendBinding
pattern BackendBinding
  { backendBindingName,
    backendBindingType,
    backendBindingExpr,
    backendBindingExportedAsMain
  } <-
  BackendBindingWithMetadata _ backendBindingName backendBindingType backendBindingExpr backendBindingExportedAsMain _

{-# COMPLETE BackendBinding #-}

-- | Backend type language. This mirrors the checked xMLF type shapes that are
-- meaningful after `.mlfp` checking, but keeps the backend boundary independent
-- from the elaborator's term representation.
data BackendType
  = BTVarWithIdentity TypeBinderIdentity String
  | BTArrow BackendType BackendType
  | BTBaseWithIdentity SymbolIdentity BaseTy
  | BTConWithIdentity SymbolIdentity BaseTy (NonEmpty BackendType)
  | BTVarAppWithIdentity TypeBinderIdentity String (NonEmpty BackendType)
  | BTForallWithIdentity TypeBinderIdentity String (Maybe BackendType) BackendType
  | BTMuWithIdentity TypeBinderIdentity String BackendType
  | BTBottom
  deriving (Show)

instance Eq BackendType where
  left == right =
    case (left, right) of
      (BTVarWithIdentity leftIdentity _, BTVarWithIdentity rightIdentity _) ->
        leftIdentity == rightIdentity
      (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
        leftDom == rightDom && leftCod == rightCod
      (BTBaseWithIdentity leftIdentity _, BTBaseWithIdentity rightIdentity _) ->
        sameSymbolIdentity leftIdentity rightIdentity
      (BTConWithIdentity leftIdentity _ leftArgs, BTConWithIdentity rightIdentity _ rightArgs) ->
        sameSymbolIdentity leftIdentity rightIdentity && leftArgs == rightArgs
      (BTVarAppWithIdentity leftIdentity _ leftArgs, BTVarAppWithIdentity rightIdentity _ rightArgs) ->
        leftIdentity == rightIdentity && leftArgs == rightArgs
      (BTForallWithIdentity leftIdentity _ leftBound leftBody, BTForallWithIdentity rightIdentity _ rightBound rightBody) ->
        leftIdentity == rightIdentity && leftBound == rightBound && leftBody == rightBody
      (BTMuWithIdentity leftIdentity _ leftBody, BTMuWithIdentity rightIdentity _ rightBody) ->
        leftIdentity == rightIdentity && leftBody == rightBody
      (BTBottom, BTBottom) ->
        True
      _ ->
        False

typeBinderRefMatches :: TypeBinderIdentity -> TypeBinderIdentity -> Bool
typeBinderRefMatches leftIdentity rightIdentity =
  leftIdentity == rightIdentity

typeHeadRefMatches :: SymbolIdentity -> SymbolIdentity -> Bool
typeHeadRefMatches leftIdentity rightIdentity =
  sameSymbolIdentity leftIdentity rightIdentity

backendTermRefMatches :: IdDetails -> IdDetails -> Bool
backendTermRefMatches leftIdentity rightIdentity =
  idDetailsSameIdentity leftIdentity rightIdentity

closureEntryRefMatches :: UniqueIdentity -> UniqueIdentity -> Bool
closureEntryRefMatches leftIdentity rightIdentity =
  leftIdentity == rightIdentity

-- Identity-erasing views for display-oriented pattern matches. Every
-- constructible 'BackendType' still carries binder/head identity.
pattern BTVar :: String -> BackendType
pattern BTVar name <-
  BTVarWithIdentity _ name

pattern BTBase :: BaseTy -> BackendType
pattern BTBase base <-
  BTBaseWithIdentity _ base

pattern BTCon :: BaseTy -> NonEmpty BackendType -> BackendType
pattern BTCon base args <-
  BTConWithIdentity _ base args

pattern BTVarApp :: String -> NonEmpty BackendType -> BackendType
pattern BTVarApp name args <-
  BTVarAppWithIdentity _ name args

pattern BTForall :: String -> Maybe BackendType -> BackendType -> BackendType
pattern BTForall name mbBound body <-
  BTForallWithIdentity _ name mbBound body

pattern BTMu :: String -> BackendType -> BackendType
pattern BTMu name body <-
  BTMuWithIdentity _ name body

{-# COMPLETE BTVar, BTArrow, BTBase, BTCon, BTVarApp, BTForall, BTMu, BTBottom #-}

type BackendTypeSubstitutionKey = TypeBinderIdentity

backendTypeSubstitutionKeyFromIdentity :: TypeBinderIdentity -> BackendTypeSubstitutionKey
backendTypeSubstitutionKeyFromIdentity = id

backendTypeSubstitutionKeyIdentity :: BackendTypeSubstitutionKey -> TypeBinderIdentity
backendTypeSubstitutionKeyIdentity = id

backendTypeSubstitutionKeyName :: BackendTypeSubstitutionKey -> String
backendTypeSubstitutionKeyName = typeBinderIdentityStableName

backendTypeSubstitutionKeyAliasNames :: BackendTypeSubstitutionKey -> Set.Set String
backendTypeSubstitutionKeyAliasNames =
  Set.singleton . typeBinderIdentityStableName

-- | Typed backend expression. `backendExprType` is the result type of the node.
data BackendExpr
  = BackendVarWithIdentity
      { backendExprType :: BackendType,
        backendVarIdentity :: IdDetails,
        backendVarName :: String
      }
  | BackendLit
      { backendExprType :: BackendType,
        backendLit :: Lit
      }
  | BackendLamWithIdentity
      { backendExprType :: BackendType,
        backendParamIdentity :: IdDetails,
        backendParamName :: String,
        backendParamType :: BackendType,
        backendBody :: BackendExpr
      }
  | BackendApp
      { backendExprType :: BackendType,
        backendFunction :: BackendExpr,
        backendArgument :: BackendExpr
      }
  | BackendLetWithIdentity
      { backendExprType :: BackendType,
        backendLetIdentity :: IdDetails,
        backendLetName :: String,
        backendLetType :: BackendType,
        backendLetRhs :: BackendExpr,
        backendLetBody :: BackendExpr
      }
  | BackendTyAbsWithIdentity
      { backendExprType :: BackendType,
        backendTyParamIdentity :: TypeBinderIdentity,
        backendTyParamName :: String,
        backendTyParamBound :: Maybe BackendType,
        backendTyAbsBody :: BackendExpr
      }
  | BackendTyApp
      { backendExprType :: BackendType,
        backendTyFunction :: BackendExpr,
        backendTyArgument :: BackendType
      }
  | BackendRoll
      { backendExprType :: BackendType,
        backendRollPayload :: BackendExpr
      }
  | BackendUnroll
      { backendExprType :: BackendType,
        backendUnrollPayload :: BackendExpr
      }
  | BackendClosureWithParamIdentities
      { backendExprType :: BackendType,
        backendClosureEntryIdentity :: UniqueIdentity,
        backendClosureEntryName :: String,
        backendClosureCaptures :: [BackendClosureCapture],
        backendClosureParamsWithIdentities :: [BackendClosureParam],
        backendClosureBody :: BackendExpr
      }
  | BackendClosureCall
      { backendExprType :: BackendType,
        backendClosureFunction :: BackendExpr,
        backendClosureArguments :: [BackendExpr]
      }
  | BackendConstructWithIdentity
      { backendExprType :: BackendType,
        backendConstructIdentity :: SymbolIdentity,
        backendConstructName :: String,
        backendConstructArgs :: [BackendExpr]
      }
  | BackendCase
      { backendExprType :: BackendType,
        backendScrutinee :: BackendExpr,
        backendAlternatives :: NonEmpty BackendAlternative
      }
  deriving (Show)

backendVarWithResolvedIdentity :: BackendType -> IdDetails -> String -> BackendExpr
backendVarWithResolvedIdentity resultTy identity name =
  BackendVarWithIdentity resultTy identity name

backendLamWithResolvedIdentity :: BackendType -> IdDetails -> String -> BackendType -> BackendExpr -> BackendExpr
backendLamWithResolvedIdentity resultTy identity name paramTy body =
  BackendLamWithIdentity resultTy identity name paramTy body

backendLetWithResolvedIdentity :: BackendType -> IdDetails -> String -> BackendType -> BackendExpr -> BackendExpr -> BackendExpr
backendLetWithResolvedIdentity resultTy identity name bindingTy rhs body =
  BackendLetWithIdentity resultTy identity name bindingTy rhs body

backendClosureWithResolvedEntry ::
  BackendType ->
  UniqueIdentity ->
  String ->
  [BackendClosureCapture] ->
  [BackendClosureParam] ->
  BackendExpr ->
  BackendExpr
backendClosureWithResolvedEntry resultTy identity entryName captures params body =
  BackendClosureWithParamIdentities resultTy identity entryName captures params body

backendConstructWithResolvedIdentity :: BackendType -> SymbolIdentity -> String -> [BackendExpr] -> BackendExpr
backendConstructWithResolvedIdentity resultTy identity name args =
  BackendConstructWithIdentity resultTy identity name args

instance Eq BackendExpr where
  left == right =
    case (left, right) of
      (BackendVarWithIdentity leftTy leftIdentity _, BackendVarWithIdentity rightTy rightIdentity _) ->
        leftTy == rightTy && idDetailsSameIdentity leftIdentity rightIdentity
      (BackendLit leftTy leftLit, BackendLit rightTy rightLit) ->
        leftTy == rightTy && leftLit == rightLit
      (BackendLamWithIdentity leftTy leftIdentity _ leftParamTy leftBody, BackendLamWithIdentity rightTy rightIdentity _ rightParamTy rightBody) ->
        leftTy == rightTy
          && idDetailsSameIdentity leftIdentity rightIdentity
          && leftParamTy == rightParamTy
          && leftBody == rightBody
      (BackendApp leftTy leftFun leftArg, BackendApp rightTy rightFun rightArg) ->
        leftTy == rightTy && leftFun == rightFun && leftArg == rightArg
      (BackendLetWithIdentity leftTy leftIdentity _ leftBindingTy leftRhs leftBody, BackendLetWithIdentity rightTy rightIdentity _ rightBindingTy rightRhs rightBody) ->
        leftTy == rightTy
          && idDetailsSameIdentity leftIdentity rightIdentity
          && leftBindingTy == rightBindingTy
          && leftRhs == rightRhs
          && leftBody == rightBody
      (BackendTyAbsWithIdentity leftTy leftIdentity _ leftBound leftBody, BackendTyAbsWithIdentity rightTy rightIdentity _ rightBound rightBody) ->
        leftTy == rightTy
          && leftIdentity == rightIdentity
          && leftBound == rightBound
          && leftBody == rightBody
      (BackendTyApp leftTy leftFun leftArg, BackendTyApp rightTy rightFun rightArg) ->
        leftTy == rightTy && leftFun == rightFun && leftArg == rightArg
      (BackendRoll leftTy leftPayload, BackendRoll rightTy rightPayload) ->
        leftTy == rightTy && leftPayload == rightPayload
      (BackendUnroll leftTy leftPayload, BackendUnroll rightTy rightPayload) ->
        leftTy == rightTy && leftPayload == rightPayload
      (BackendClosureWithParamIdentities leftTy leftEntryIdentity _ leftCaptures leftParams leftBody, BackendClosureWithParamIdentities rightTy rightEntryIdentity _ rightCaptures rightParams rightBody) ->
        leftTy == rightTy
          && leftEntryIdentity == rightEntryIdentity
          && leftCaptures == rightCaptures
          && leftParams == rightParams
          && leftBody == rightBody
      (BackendClosureCall leftTy leftFun leftArgs, BackendClosureCall rightTy rightFun rightArgs) ->
        leftTy == rightTy && leftFun == rightFun && leftArgs == rightArgs
      (BackendConstructWithIdentity leftTy leftIdentity _ leftArgs, BackendConstructWithIdentity rightTy rightIdentity _ rightArgs) ->
        leftTy == rightTy
          && sameSymbolIdentity leftIdentity rightIdentity
          && leftArgs == rightArgs
      (BackendCase leftTy leftScrutinee leftAlternatives, BackendCase rightTy rightScrutinee rightAlternatives) ->
        leftTy == rightTy && leftScrutinee == rightScrutinee && leftAlternatives == rightAlternatives
      _ ->
        False

-- Identity-erasing views for display-oriented pattern matches. Every
-- constructible semantic reference still carries identity.
pattern BackendVar :: BackendType -> String -> BackendExpr
pattern BackendVar resultTy name <-
  BackendVarWithIdentity resultTy _ name

pattern BackendLam :: BackendType -> String -> BackendType -> BackendExpr -> BackendExpr
pattern BackendLam resultTy name paramTy body <-
  BackendLamWithIdentity resultTy _ name paramTy body

pattern BackendLet :: BackendType -> String -> BackendType -> BackendExpr -> BackendExpr -> BackendExpr
pattern BackendLet resultTy name bindingTy rhs body <-
  BackendLetWithIdentity resultTy _ name bindingTy rhs body

pattern BackendConstruct :: BackendType -> String -> [BackendExpr] -> BackendExpr
pattern BackendConstruct resultTy name args <-
  BackendConstructWithIdentity resultTy _ name args

pattern BackendTyAbs :: BackendType -> String -> Maybe BackendType -> BackendExpr -> BackendExpr
pattern BackendTyAbs resultTy name mbBound body <-
  BackendTyAbsWithIdentity resultTy _ name mbBound body

pattern BackendClosure :: BackendType -> String -> [BackendClosureCapture] -> [(String, BackendType)] -> BackendExpr -> BackendExpr
pattern BackendClosure resultTy entryName captures params body <-
  BackendClosureWithParamIdentities
    resultTy
    _
    entryName
    captures
    (map backendClosureParamPair -> params)
    body

{-# COMPLETE BackendVar, BackendLit, BackendLam, BackendApp, BackendLet, BackendTyAbs, BackendTyApp, BackendRoll, BackendUnroll, BackendClosure, BackendClosureCall, BackendConstruct, BackendCase #-}
{-# COMPLETE BackendVarWithIdentity, BackendLit, BackendLam, BackendApp, BackendLet, BackendTyAbs, BackendTyApp, BackendRoll, BackendUnroll, BackendClosure, BackendClosureCall, BackendConstructWithIdentity, BackendCase #-}
{-# COMPLETE BackendVarWithIdentity, BackendLit, BackendLamWithIdentity, BackendApp, BackendLetWithIdentity, BackendTyAbs, BackendTyApp, BackendRoll, BackendUnroll, BackendClosureWithParamIdentities, BackendClosureCall, BackendConstructWithIdentity, BackendCase #-}

backendClosureParamPair :: BackendClosureParam -> (String, BackendType)
backendClosureParamPair param =
  (backendClosureParamName param, backendClosureParamType param)

backendClosureParams :: [(IdDetails, String, BackendType)] -> [BackendClosureParam]
backendClosureParams =
  map (\(identity, name, ty) -> BackendClosureParam identity name ty)

backendClosureCaptureWithResolvedIdentity :: IdDetails -> String -> BackendType -> BackendExpr -> BackendClosureCapture
backendClosureCaptureWithResolvedIdentity identity name ty expr =
  BackendClosureCapture
    { backendClosureCaptureIdentity = identity,
      backendClosureCaptureName = name,
      backendClosureCaptureType = ty,
      backendClosureCaptureExpr = expr
    }

backendClosureParamWithResolvedIdentity :: IdDetails -> String -> BackendType -> BackendClosureParam
backendClosureParamWithResolvedIdentity identity name ty =
  BackendClosureParam
    { backendClosureParamIdentity = identity,
      backendClosureParamName = name,
      backendClosureParamType = ty
    }

data BackendAlternative = BackendAlternative
  { backendAltPattern :: BackendPattern,
    backendAltBody :: BackendExpr
  }
  deriving (Eq, Show)

data BackendPatternBinder = BackendPatternBinder
  { backendPatternBinderIdentity :: IdDetails,
    backendPatternBinderName :: String
  }
  deriving (Show)

backendPatternBinderWithResolvedIdentity :: IdDetails -> String -> BackendPatternBinder
backendPatternBinderWithResolvedIdentity identity name =
  BackendPatternBinder identity name

instance Eq BackendPatternBinder where
  left == right =
    idDetailsSameIdentity (backendPatternBinderIdentity left) (backendPatternBinderIdentity right)

data BackendPattern
  = BackendDefaultPattern
  | BackendConstructorPatternWithBinderIdentities SymbolIdentity String [BackendPatternBinder]
  deriving (Show)

backendConstructorPatternWithResolvedIdentity :: SymbolIdentity -> String -> [BackendPatternBinder] -> BackendPattern
backendConstructorPatternWithResolvedIdentity identity name binders =
  BackendConstructorPatternWithBinderIdentities identity name binders

instance Eq BackendPattern where
  left == right =
    case (left, right) of
      (BackendDefaultPattern, BackendDefaultPattern) ->
        True
      (BackendConstructorPatternWithBinderIdentities leftIdentity _ leftBinders, BackendConstructorPatternWithBinderIdentities rightIdentity _ rightBinders) ->
        sameSymbolIdentity leftIdentity rightIdentity && leftBinders == rightBinders
      _ ->
        False

-- Identity-erasing view; the constructible alternative retains constructor
-- and binder identities.
pattern BackendConstructorPattern :: String -> [String] -> BackendPattern
pattern BackendConstructorPattern name binders <-
  BackendConstructorPatternWithBinderIdentities _ name (map backendPatternBinderName -> binders)

{-# COMPLETE BackendDefaultPattern, BackendConstructorPattern #-}
{-# COMPLETE BackendDefaultPattern, BackendConstructorPatternWithBinderIdentities #-}

literalBackendType :: Lit -> BackendType
literalBackendType = \case
  LInt _ -> builtinLiteralType "Int"
  LBool _ -> builtinLiteralType "Bool"
  LChar _ -> builtinLiteralType "Char"
  LString _ -> builtinLiteralType "String"

builtinLiteralType :: String -> BackendType
builtinLiteralType name =
  BTBaseWithIdentity (builtinTypeIdentity name) (BaseTy name)

freeBackendTypeVars :: BackendType -> Set.Set String
freeBackendTypeVars =
  Set.map backendTypeSubstitutionKeyName . freeBackendTypeVarKeys

freeBackendTypeVarsInKeyed :: Map.Map BackendTypeSubstitutionKey BackendType -> Set.Set String
freeBackendTypeVarsInKeyed replacements =
  Set.unions (map freeBackendTypeVars (Map.elems replacements))

freeBackendTypeVarAliasNames :: BackendType -> Set.Set String
freeBackendTypeVarAliasNames =
  Set.unions . map backendDataParameterRefAliasNames . Set.toList . freeBackendTypeVarRefs

freeBackendTypeVarAliasNamesInKeyed :: Map.Map BackendTypeSubstitutionKey BackendType -> Set.Set String
freeBackendTypeVarAliasNamesInKeyed replacements =
  Set.unions (map freeBackendTypeVarAliasNames (Map.elems replacements))

backendDataParameterRefAliasNames :: BackendDataParameterRef -> Set.Set String
backendDataParameterRefAliasNames ref =
  Set.fromList
    ( typeBinderIdentityAliasNames
        (backendDataParameterRefName ref)
        (backendDataParameterRefIdentity ref)
    )

freeBackendTypeVarRefs :: BackendType -> Set.Set BackendDataParameterRef
freeBackendTypeVarRefs =
  go Set.empty
  where
    go bound =
      \case
        BTVarWithIdentity identity name
          | Set.member (backendTypeSubstitutionKeyFromIdentity identity) bound -> Set.empty
          | otherwise -> freeBackendTypeVarRef identity name
        BTArrow dom cod ->
          Set.union (go bound dom) (go bound cod)
        BTBaseWithIdentity {} ->
          Set.empty
        BTConWithIdentity _ _ args ->
          Set.unions (map (go bound) (NE.toList args))
        BTVarAppWithIdentity identity name args ->
          let headRefs =
                if Set.member (backendTypeSubstitutionKeyFromIdentity identity) bound
                  then Set.empty
                  else freeBackendTypeVarRef identity name
           in Set.union headRefs (Set.unions (map (go bound) (NE.toList args)))
        BTForallWithIdentity identity _ mbBound body ->
          Set.union
            (maybe Set.empty (go bound) mbBound)
            (go (Set.insert (backendTypeSubstitutionKeyFromIdentity identity) bound) body)
        BTMuWithIdentity identity _ body ->
          go (Set.insert (backendTypeSubstitutionKeyFromIdentity identity) bound) body
        BTBottom ->
          Set.empty

    freeBackendTypeVarRef identity name =
      Set.singleton (backendDataParameterRefFromIdentity identity name)

freeBackendTypeVarKeys :: BackendType -> Set.Set BackendTypeSubstitutionKey
freeBackendTypeVarKeys =
  \case
    BTVarWithIdentity identity _ ->
      Set.singleton (backendTypeSubstitutionKeyFromIdentity identity)
    BTArrow dom cod ->
      Set.union (freeBackendTypeVarKeys dom) (freeBackendTypeVarKeys cod)
    BTBaseWithIdentity {} ->
      Set.empty
    BTConWithIdentity _ _ args ->
      Set.unions (map freeBackendTypeVarKeys (NE.toList args))
    BTVarAppWithIdentity identity _ args ->
      Set.insert
        (backendTypeSubstitutionKeyFromIdentity identity)
        (Set.unions (map freeBackendTypeVarKeys (NE.toList args)))
    BTForallWithIdentity identity _ mbBound body ->
      Set.union
        (maybe Set.empty freeBackendTypeVarKeys mbBound)
        (Set.delete (backendTypeSubstitutionKeyFromIdentity identity) (freeBackendTypeVarKeys body))
    BTMuWithIdentity identity _ body ->
      Set.delete (backendTypeSubstitutionKeyFromIdentity identity) (freeBackendTypeVarKeys body)
    BTBottom ->
      Set.empty

freeBackendTypeVarKeysInKeyed :: Map.Map BackendTypeSubstitutionKey BackendType -> Set.Set BackendTypeSubstitutionKey
freeBackendTypeVarKeysInKeyed replacements =
  Set.unions (map freeBackendTypeVarKeys (Map.elems replacements))

generatedIdentitiesInBackendTypes :: [BackendType] -> [UniqueIdentity]
generatedIdentitiesInBackendTypes =
  foldMap generatedIdentitiesInBackendType

generatedIdentitiesInBackendProgram :: BackendProgram -> [UniqueIdentity]
generatedIdentitiesInBackendProgram program =
  symbolGeneratedIdentities (backendProgramMainIdentity program)
    ++ foldMap generatedIdentitiesInBackendModule (backendProgramModules program)

generatedIdentitiesInBackendModule :: BackendModule -> [UniqueIdentity]
generatedIdentitiesInBackendModule backendModule =
  symbolGeneratedIdentities (backendModuleIdentity backendModule)
    ++ foldMap generatedIdentitiesInBackendData (backendModuleData backendModule)
    ++ foldMap generatedIdentitiesInBackendBinding (backendModuleBindings backendModule)

generatedIdentitiesInBackendData :: BackendData -> [UniqueIdentity]
generatedIdentitiesInBackendData dataDecl =
  symbolGeneratedIdentities (backendDataIdentity dataDecl)
    ++ foldMap generatedIdentitiesInBackendDataParameterRef (backendDataParameterRefs dataDecl)
    ++ foldMap generatedIdentitiesInBackendConstructor (backendDataConstructors dataDecl)

generatedIdentitiesInBackendDataParameterRef :: BackendDataParameterRef -> [UniqueIdentity]
generatedIdentitiesInBackendDataParameterRef ref =
  generatedIdentitiesInTypeBinderRef (backendDataParameterRefIdentity ref)

generatedIdentitiesInBackendConstructor :: BackendConstructor -> [UniqueIdentity]
generatedIdentitiesInBackendConstructor constructor =
  symbolGeneratedIdentities (backendConstructorIdentity constructor)
    ++ foldMap generatedIdentitiesInBackendTypeBinder (backendConstructorForalls constructor)
    ++ generatedIdentitiesInBackendTypes (backendConstructorFields constructor)
    ++ generatedIdentitiesInBackendType (backendConstructorResult constructor)

generatedIdentitiesInBackendTypeBinder :: BackendTypeBinder -> [UniqueIdentity]
generatedIdentitiesInBackendTypeBinder binder =
  generatedIdentitiesInTypeBinderRef (backendTypeBinderIdentity binder)
    ++ maybe [] generatedIdentitiesInBackendType (backendTypeBinderBound binder)

generatedIdentitiesInBackendBinding :: BackendBinding -> [UniqueIdentity]
generatedIdentitiesInBackendBinding binding =
  symbolGeneratedIdentities (backendBindingIdentity binding)
    ++ generatedIdentitiesInBackendType (backendBindingType binding)
    ++ generatedIdentitiesInBackendExpr (backendBindingExpr binding)

generatedIdentitiesInBackendExpr :: BackendExpr -> [UniqueIdentity]
generatedIdentitiesInBackendExpr =
  \case
    BackendVarWithIdentity ty identity _ ->
      generatedIdentitiesInBackendType ty ++ idDetailsGeneratedIdentities identity
    BackendLit ty _ ->
      generatedIdentitiesInBackendType ty
    BackendLamWithIdentity resultTy identity _ paramTy body ->
      generatedIdentitiesInBackendType resultTy
        ++ idDetailsGeneratedIdentities identity
        ++ generatedIdentitiesInBackendType paramTy
        ++ generatedIdentitiesInBackendExpr body
    BackendApp resultTy fun arg ->
      generatedIdentitiesInBackendType resultTy ++ generatedIdentitiesInBackendExpr fun ++ generatedIdentitiesInBackendExpr arg
    BackendLetWithIdentity resultTy identity _ bindingTy rhs body ->
      generatedIdentitiesInBackendType resultTy
        ++ idDetailsGeneratedIdentities identity
        ++ generatedIdentitiesInBackendType bindingTy
        ++ generatedIdentitiesInBackendExpr rhs
        ++ generatedIdentitiesInBackendExpr body
    BackendTyAbsWithIdentity resultTy identity _ mbBound body ->
      generatedIdentitiesInBackendType resultTy
        ++ generatedIdentitiesInTypeBinderIdentity identity
        ++ maybe [] generatedIdentitiesInBackendType mbBound
        ++ generatedIdentitiesInBackendExpr body
    BackendTyApp resultTy fun ty ->
      generatedIdentitiesInBackendType resultTy ++ generatedIdentitiesInBackendExpr fun ++ generatedIdentitiesInBackendType ty
    BackendRoll resultTy payload ->
      generatedIdentitiesInBackendType resultTy ++ generatedIdentitiesInBackendExpr payload
    BackendUnroll resultTy payload ->
      generatedIdentitiesInBackendType resultTy ++ generatedIdentitiesInBackendExpr payload
    BackendClosureWithParamIdentities resultTy identity _ captures params body ->
      generatedIdentitiesInBackendType resultTy
        ++ [identity]
        ++ foldMap generatedIdentitiesInBackendClosureCapture captures
        ++ foldMap generatedIdentitiesInBackendClosureParam params
        ++ generatedIdentitiesInBackendExpr body
    BackendClosureCall resultTy fun args ->
      generatedIdentitiesInBackendType resultTy ++ generatedIdentitiesInBackendExpr fun ++ foldMap generatedIdentitiesInBackendExpr args
    BackendConstructWithIdentity resultTy identity _ args ->
      generatedIdentitiesInBackendType resultTy
        ++ symbolGeneratedIdentities identity
        ++ foldMap generatedIdentitiesInBackendExpr args
    BackendCase resultTy scrutinee alternatives ->
      generatedIdentitiesInBackendType resultTy
        ++ generatedIdentitiesInBackendExpr scrutinee
        ++ foldMap generatedIdentitiesInBackendAlternative alternatives

generatedIdentitiesInBackendClosureCapture :: BackendClosureCapture -> [UniqueIdentity]
generatedIdentitiesInBackendClosureCapture capture =
  idDetailsGeneratedIdentities (backendClosureCaptureIdentity capture)
    ++ generatedIdentitiesInBackendType (backendClosureCaptureType capture)
    ++ generatedIdentitiesInBackendExpr (backendClosureCaptureExpr capture)

generatedIdentitiesInBackendClosureParam :: BackendClosureParam -> [UniqueIdentity]
generatedIdentitiesInBackendClosureParam param =
  idDetailsGeneratedIdentities (backendClosureParamIdentity param)
    ++ generatedIdentitiesInBackendType (backendClosureParamType param)

generatedIdentitiesInBackendAlternative :: BackendAlternative -> [UniqueIdentity]
generatedIdentitiesInBackendAlternative (BackendAlternative pattern0 body) =
  generatedIdentitiesInBackendPattern pattern0 ++ generatedIdentitiesInBackendExpr body

generatedIdentitiesInBackendPattern :: BackendPattern -> [UniqueIdentity]
generatedIdentitiesInBackendPattern =
  \case
    BackendDefaultPattern ->
      []
    BackendConstructorPatternWithBinderIdentities identity _ binders ->
      symbolGeneratedIdentities identity
        ++ foldMap generatedIdentitiesInBackendPatternBinder binders

generatedIdentitiesInBackendPatternBinder :: BackendPatternBinder -> [UniqueIdentity]
generatedIdentitiesInBackendPatternBinder binder =
  idDetailsGeneratedIdentities (backendPatternBinderIdentity binder)

generatedIdentitiesInBackendType :: BackendType -> [UniqueIdentity]
generatedIdentitiesInBackendType =
  \case
    BTVarWithIdentity identity _ ->
      generatedIdentitiesInTypeBinderRef identity
    BTArrow dom cod ->
      generatedIdentitiesInBackendType dom ++ generatedIdentitiesInBackendType cod
    BTBaseWithIdentity identity _ ->
      symbolGeneratedIdentities identity
    BTConWithIdentity identity _ args ->
      symbolGeneratedIdentities identity ++ foldMap generatedIdentitiesInBackendType args
    BTVarAppWithIdentity identity _ args ->
      generatedIdentitiesInTypeBinderRef identity ++ foldMap generatedIdentitiesInBackendType args
    BTForallWithIdentity identity _ mbBound body ->
      generatedIdentitiesInTypeBinderRef identity
        ++ maybe [] generatedIdentitiesInBackendType mbBound
        ++ generatedIdentitiesInBackendType body
    BTMuWithIdentity identity _ body ->
      generatedIdentitiesInTypeBinderRef identity ++ generatedIdentitiesInBackendType body
    BTBottom ->
      []

generatedIdentitiesInTypeBinderIdentity :: TypeBinderIdentity -> [UniqueIdentity]
generatedIdentitiesInTypeBinderIdentity =
  typeBinderGeneratedIdentities

generatedIdentitiesInTypeBinderRef :: TypeBinderIdentity -> [UniqueIdentity]
generatedIdentitiesInTypeBinderRef identity =
  generatedIdentitiesInTypeBinderIdentity identity

generatedIdentitiesInBackendTypeSubstitutionKey :: BackendTypeSubstitutionKey -> [UniqueIdentity]
generatedIdentitiesInBackendTypeSubstitutionKey =
  generatedIdentitiesInTypeBinderIdentity

backendTypeHeadMatches :: SymbolIdentity -> SymbolIdentity -> Bool
backendTypeHeadMatches =
  typeHeadRefMatches

backendTypeRefinesScrutinee :: BackendType -> BackendType -> Bool
backendTypeRefinesScrutinee =
  go False
  where
    go allowVariableRefinement constructorResult scrutineeTy
      | sameType constructorResult scrutineeTy = True
      | otherwise =
          case (constructorResult, scrutineeTy) of
            (_, BTVarWithIdentity {})
              | allowVariableRefinement ->
                  True
            (BTArrow resultDom resultCod, BTArrow scrutineeDom scrutineeCod) ->
              go False resultDom scrutineeDom
                && go False resultCod scrutineeCod
            (BTBaseWithIdentity resultIdentity _, BTBaseWithIdentity scrutineeIdentity _) ->
              backendTypeHeadMatches resultIdentity scrutineeIdentity
            (BTConWithIdentity resultIdentity _ resultArgs, BTConWithIdentity scrutineeIdentity _ scrutineeArgs) ->
              backendTypeHeadMatches resultIdentity scrutineeIdentity
                && length resultArgs == length scrutineeArgs
                && and (zipWith (go True) (NE.toList resultArgs) (NE.toList scrutineeArgs))
            (BTVarAppWithIdentity resultIdentity _ resultArgs, BTVarAppWithIdentity scrutineeIdentity _ scrutineeArgs) ->
              typeBinderRefMatches resultIdentity scrutineeIdentity
                && length resultArgs == length scrutineeArgs
                && and (zipWith (go True) (NE.toList resultArgs) (NE.toList scrutineeArgs))
            (BTForallWithIdentity resultIdentity _ resultBound resultBody, BTForallWithIdentity scrutineeIdentity _ scrutineeBound scrutineeBody) ->
              typeBinderRefMatches resultIdentity scrutineeIdentity
                && backendTypeBoundRefines resultBound scrutineeBound
                && go False resultBody scrutineeBody
            (BTMuWithIdentity resultIdentity _ resultBody, BTMuWithIdentity scrutineeIdentity _ scrutineeBody) ->
              typeBinderRefMatches resultIdentity scrutineeIdentity
                && go False resultBody scrutineeBody
            (BTBottom, BTBottom) ->
              True
            _ ->
              False

    backendTypeBoundRefines Nothing Nothing =
      True
    backendTypeBoundRefines (Just resultBound) (Just scrutineeBound) =
      go False resultBound scrutineeBound
    backendTypeBoundRefines _ _ =
      False

    sameType left right =
      case (left, right) of
        (BTVarWithIdentity leftIdentity _, BTVarWithIdentity rightIdentity _) ->
          typeBinderRefMatches leftIdentity rightIdentity
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          sameType leftDom rightDom && sameType leftCod rightCod
        (BTBaseWithIdentity leftIdentity _, BTBaseWithIdentity rightIdentity _) ->
          backendTypeHeadMatches leftIdentity rightIdentity
        (BTConWithIdentity leftIdentity _ leftArgs, BTConWithIdentity rightIdentity _ rightArgs) ->
          backendTypeHeadMatches leftIdentity rightIdentity
            && length leftArgs == length rightArgs
            && and (zipWith sameType (NE.toList leftArgs) (NE.toList rightArgs))
        (BTVarAppWithIdentity leftIdentity _ leftArgs, BTVarAppWithIdentity rightIdentity _ rightArgs) ->
          typeBinderRefMatches leftIdentity rightIdentity
            && length leftArgs == length rightArgs
            && and (zipWith sameType (NE.toList leftArgs) (NE.toList rightArgs))
        (BTForallWithIdentity leftIdentity _ leftBound leftBody, BTForallWithIdentity rightIdentity _ rightBound rightBody) ->
          typeBinderRefMatches leftIdentity rightIdentity
            && sameMaybeType leftBound rightBound
            && sameType leftBody rightBody
        (BTMuWithIdentity leftIdentity _ leftBody, BTMuWithIdentity rightIdentity _ rightBody) ->
          typeBinderRefMatches leftIdentity rightIdentity
            && sameType leftBody rightBody
        (BTBottom, BTBottom) ->
          True
        _ ->
          False

    sameMaybeType Nothing Nothing =
      True
    sameMaybeType (Just left) (Just right) =
      sameType left right
    sameMaybeType _ _ =
      False

substituteBackendTypeByIdentity :: TypeBinderIdentity -> BackendType -> BackendType -> BackendType
substituteBackendTypeByIdentity needle replacement =
  substituteBackendTypesByKey (Map.singleton needle replacement)

substituteBackendTypesByKey :: Map.Map BackendTypeSubstitutionKey BackendType -> BackendType -> BackendType
substituteBackendTypesByKey replacements0 ty0 =
  fst (go replacements0 initialGenerator ty0)
  where
    initialGenerator =
      identityGeneratorAfter
        ( generatedIdentitiesInBackendTypes (ty0 : Map.elems replacements0)
            ++ concatMap generatedIdentitiesInBackendTypeSubstitutionKey (Map.keys replacements0)
        )

    go replacements generator ty =
      case ty of
        BTVarWithIdentity identity _ ->
          (lookupTypeReplacement identity replacements ty, generator)
        BTArrow dom cod ->
          let (dom', generator') = go replacements generator dom
              (cod', generator'') = go replacements generator' cod
           in (BTArrow dom' cod', generator'')
        BTBaseWithIdentity {} -> (ty, generator)
        BTConWithIdentity identity con args ->
          let (args', generator') = goNonEmpty replacements generator args
           in (BTConWithIdentity identity con args', generator')
        BTVarAppWithIdentity identity name args ->
          let (args', generator') = goNonEmpty replacements generator args
           in case lookupTypeReplacementMaybe identity replacements >>= (`applyBackendTypeHead` NE.toList args') of
                Just ty' -> (ty', generator')
                Nothing -> (BTVarAppWithIdentity identity name args', generator')
        BTForallWithIdentity identity name mbBound body
          ->
            let (mbBound', generator') = goMaybe replacements generator mbBound
                bodyReplacements = deleteTypeReplacement identity replacements
             in if Map.null bodyReplacements
                  then (BTForallWithIdentity identity name mbBound' body, generator')
                  else
                    let binderKey = backendTypeSubstitutionKeyFromIdentity identity
                        freeBodyReplacementKeys = freeBackendTypeVarKeysInKeyed bodyReplacements
                     in if Set.member binderKey freeBodyReplacementKeys
                          then
                            let used =
                                  Set.unions
                                    [ freeBackendTypeVarAliasNamesInKeyed bodyReplacements,
                                      freeBackendTypeVarAliasNames body,
                                      maybe Set.empty freeBackendTypeVarAliasNames mbBound,
                                      Set.unions (map backendTypeSubstitutionKeyAliasNames (Map.keys bodyReplacements)),
                                      Set.singleton name
                                    ]
                                name' = freshNameLike name used
                                (identity', generator'') = freshBackendBinderIdentity identity generator'
                                body' = renameBackendTypeBinder identity identity' name' body
                                (body'', generator''') = go bodyReplacements generator'' body'
                             in (BTForallWithIdentity identity' name' mbBound' body'', generator''')
                          else
                            let (body', generator'') = go bodyReplacements generator' body
                             in (BTForallWithIdentity identity name mbBound' body', generator'')
        BTMuWithIdentity identity name body
          ->
            let bodyReplacements = deleteTypeReplacement identity replacements
             in if Map.null bodyReplacements
                  then (ty, generator)
                  else
                    let binderKey = backendTypeSubstitutionKeyFromIdentity identity
                        freeBodyReplacementKeys = freeBackendTypeVarKeysInKeyed bodyReplacements
                     in if Set.member binderKey freeBodyReplacementKeys
                          then
                            let used =
                                  Set.unions
                                    [ freeBackendTypeVarAliasNamesInKeyed bodyReplacements,
                                      freeBackendTypeVarAliasNames body,
                                      Set.unions (map backendTypeSubstitutionKeyAliasNames (Map.keys bodyReplacements)),
                                      Set.singleton name
                                    ]
                                name' = freshNameLike name used
                                (identity', generator') = freshBackendBinderIdentity identity generator
                                body' = renameBackendTypeBinder identity identity' name' body
                                (body'', generator'') = go bodyReplacements generator' body'
                             in (BTMuWithIdentity identity' name' body'', generator'')
                          else
                            let (body', generator') = go bodyReplacements generator body
                             in (BTMuWithIdentity identity name body', generator')
        BTBottom -> (BTBottom, generator)

    goMaybe replacements generator =
      \case
        Nothing -> (Nothing, generator)
        Just ty ->
          let (ty', generator') = go replacements generator ty
           in (Just ty', generator')

    goNonEmpty replacements generator (ty :| tys) =
      let (ty', generator') = go replacements generator ty
          (generator'', tys') =
            mapAccumL
              ( \acc item ->
                  let (item', acc') = go replacements acc item
                   in (acc', item')
              )
              generator'
              tys
       in (ty' :| tys', generator'')

lookupTypeReplacement :: TypeBinderIdentity -> Map.Map BackendTypeSubstitutionKey BackendType -> BackendType -> BackendType
lookupTypeReplacement identity replacements fallback =
  case lookupTypeReplacementMaybe identity replacements of
    Just replacement -> replacement
    Nothing -> fallback

lookupTypeReplacementMaybe :: TypeBinderIdentity -> Map.Map BackendTypeSubstitutionKey BackendType -> Maybe BackendType
lookupTypeReplacementMaybe identity replacements =
  Map.lookup (backendTypeSubstitutionKeyFromIdentity identity) replacements

deleteTypeReplacement :: TypeBinderIdentity -> Map.Map BackendTypeSubstitutionKey BackendType -> Map.Map BackendTypeSubstitutionKey BackendType
deleteTypeReplacement identity =
  Map.delete (backendTypeSubstitutionKeyFromIdentity identity)

freshBackendBinderIdentity :: TypeBinderIdentity -> IdentityGenerator -> (TypeBinderIdentity, IdentityGenerator)
freshBackendBinderIdentity identity generator
  | Just (unique, role) <- typeBinderIdentityStructural identity =
      (typeBinderIdentityFromStructural unique role, generator)
freshBackendBinderIdentity _ generator =
  let (unique, generator') = freshIdentity generator
   in (typeBinderIdentityFromUnique unique, generator')

renameBackendTypeBinder ::
  TypeBinderIdentity ->
  TypeBinderIdentity ->
  String ->
  BackendType ->
  BackendType
renameBackendTypeBinder oldIdentity newIdentity newName =
  go
  where
    replacement = BTVarWithIdentity newIdentity newName

    matches identity =
      typeBinderRefMatches identity oldIdentity

    go ty =
      case ty of
        BTVarWithIdentity identity _
          | matches identity -> replacement
          | otherwise -> ty
        BTArrow dom cod ->
          BTArrow (go dom) (go cod)
        BTBaseWithIdentity {} ->
          ty
        BTConWithIdentity identity con args ->
          BTConWithIdentity identity con (fmap go args)
        BTVarAppWithIdentity identity name args
          | matches identity ->
              maybe replacement id (applyBackendTypeHead replacement (NE.toList (fmap go args)))
          | otherwise ->
              BTVarAppWithIdentity identity name (fmap go args)
        BTForallWithIdentity identity name mbBound body
          | matches identity ->
              BTForallWithIdentity identity name (fmap go mbBound) body
          | otherwise ->
              BTForallWithIdentity identity name (fmap go mbBound) (go body)
        BTMuWithIdentity identity name body
          | matches identity -> ty
          | otherwise -> BTMuWithIdentity identity name (go body)
        BTBottom ->
          BTBottom

substituteBackendTypeForBinder :: TypeBinderIdentity -> BackendType -> BackendType -> BackendType
substituteBackendTypeForBinder identity replacement =
  substituteBackendTypeByIdentity identity replacement

applyBackendTypeHead :: BackendType -> [BackendType] -> Maybe BackendType
applyBackendTypeHead headTy args =
  case headTy of
    BTVarWithIdentity identity name -> Just (mkVarHead identity name args)
    BTBaseWithIdentity identity name -> Just (mkConHead identity name args)
    BTConWithIdentity identity name existingArgs -> Just (mkConHead identity name (NE.toList existingArgs ++ args))
    BTVarAppWithIdentity identity name existingArgs -> Just (mkVarHead identity name (NE.toList existingArgs ++ args))
    _ -> Nothing
  where
    mkVarHead identity name = \case
      [] -> BTVarWithIdentity identity name
      arg : rest -> BTVarAppWithIdentity identity name (arg :| rest)

    mkConHead identity name = \case
      [] -> BTBaseWithIdentity identity name
      arg : rest -> BTConWithIdentity identity name (arg :| rest)

unfoldBackendRecursiveType :: BackendType -> Maybe BackendType
unfoldBackendRecursiveType ty =
  case ty of
    BTMuWithIdentity identity _ body -> Just (substituteBackendTypeForBinder identity ty body)
    _ -> Nothing
