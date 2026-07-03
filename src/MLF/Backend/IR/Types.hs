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
    backendTypeSubstitutionKeyFor,
    backendTypeSubstitutionKeyIdentity,
    backendTypeSubstitutionKeyName,
    pattern BTVar,
    pattern BTBase,
    pattern BTCon,
    pattern BTVarApp,
    pattern BTForall,
    pattern BTMu,
    BackendExpr (..),
    pattern BackendVar,
    pattern BackendLam,
    pattern BackendLet,
    pattern BackendTyAbs,
    pattern BackendClosure,
    backendClosureParams,
    pattern BackendConstruct,
    BackendAlternative (..),
    BackendPatternBinder (..),
    BackendPattern (..),
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
    backendTermRefMatches,
    closureEntryRefMatches,
    literalBackendType,
    symbolRefMatches,
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
import MLF.Frontend.Program.Builtins (builtinTypeHeadIdentity, builtinTypeIdentity)
import MLF.Frontend.Symbol (SymbolIdentity, symbolRefMatches)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity
  ( IdDetails,
    IdentityGenerator,
    TypeBinderIdentity,
    UniqueIdentity (..),
    freshIdentity,
    idDetailsGeneratedIdentities,
    idDetailsRefMatches,
    identityGeneratorAfter,
    symbolGeneratedIdentities,
    typeBinderGeneratedIdentities,
    typeBinderIdentityFromUnique,
    typeBinderIdentityStableName,
  )
import MLF.Util.Names (freshNameLike)

-- | A checked backend program. Module order is preserved from the source
-- program for diagnostics/debug output, but backend binding names are global
-- runtime names.
data BackendProgram = BackendProgramWithIdentity
  { backendProgramModulesWithIdentity :: [BackendModule],
    backendProgramMainIdentity :: Maybe SymbolIdentity,
    backendProgramMainWithIdentity :: String
  }
  deriving (Show)

instance Eq BackendProgram where
  left == right =
    backendProgramModules left == backendProgramModules right
      && symbolRefMatches (backendProgramMainIdentity left) (backendProgramMain left) (backendProgramMainIdentity right) (backendProgramMain right)

pattern BackendProgram :: [BackendModule] -> String -> BackendProgram
pattern BackendProgram
  { backendProgramModules,
    backendProgramMain
  } <-
  BackendProgramWithIdentity
    backendProgramModules
    _
    backendProgramMain
  where
    BackendProgram modules0 mainName =
      BackendProgramWithIdentity modules0 Nothing mainName

{-# COMPLETE BackendProgram #-}

-- | Backend-owned module payload. Imports/exports have already been resolved
-- by the `.mlfp` checker; this record keeps only the data and binding shapes
-- needed by backend conversion and lowering.
data BackendModule = BackendModuleWithIdentity
  { backendModuleIdentity :: Maybe SymbolIdentity,
    backendModuleNameWithIdentity :: String,
    backendModuleDataWithIdentity :: [BackendData],
    backendModuleBindingsWithIdentity :: [BackendBinding]
  }
  deriving (Show)

instance Eq BackendModule where
  left == right =
    symbolRefMatches (backendModuleIdentity left) (backendModuleName left) (backendModuleIdentity right) (backendModuleName right)
      && backendModuleData left == backendModuleData right
      && backendModuleBindings left == backendModuleBindings right

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
  where
    BackendModule name dataDecls bindings =
      BackendModuleWithIdentity Nothing name dataDecls bindings

{-# COMPLETE BackendModule #-}

-- | Explicit ADT metadata available to lowerers. Constructor result types are
-- kept explicit so GADT-style results can survive the source-to-backend cut.
data BackendDataParameterRef
  = BackendDataParameterByIdentity TypeBinderIdentity String
  | BackendDataParameterByName String
  deriving (Show)

instance Eq BackendDataParameterRef where
  left == right =
    compare left right == EQ

instance Ord BackendDataParameterRef where
  compare left right =
    case (left, right) of
      (BackendDataParameterByIdentity leftIdentity _, BackendDataParameterByIdentity rightIdentity _) ->
        compare leftIdentity rightIdentity
      (BackendDataParameterByIdentity {}, BackendDataParameterByName {}) ->
        LT
      (BackendDataParameterByName {}, BackendDataParameterByIdentity {}) ->
        GT
      (BackendDataParameterByName leftName, BackendDataParameterByName rightName) ->
        compare leftName rightName

backendDataParameterRef :: Maybe TypeBinderIdentity -> String -> BackendDataParameterRef
backendDataParameterRef mbIdentity name =
  case mbIdentity of
    Just identity -> BackendDataParameterByIdentity identity name
    Nothing -> BackendDataParameterByName name

backendDataParameterRefFromIdentity :: TypeBinderIdentity -> String -> BackendDataParameterRef
backendDataParameterRefFromIdentity =
  BackendDataParameterByIdentity

backendDataParameterRefIdentity :: BackendDataParameterRef -> Maybe TypeBinderIdentity
backendDataParameterRefIdentity =
  \case
    BackendDataParameterByIdentity identity _ -> Just identity
    BackendDataParameterByName {} -> Nothing

backendDataParameterRefName :: BackendDataParameterRef -> String
backendDataParameterRefName =
  \case
    BackendDataParameterByIdentity _ name -> name
    BackendDataParameterByName name -> name

data BackendData = BackendDataWithIdentity
  { backendDataIdentity :: Maybe SymbolIdentity,
    backendDataNameWithIdentity :: String,
    backendDataParameterRefsWithIdentity :: [BackendDataParameterRef],
    backendDataConstructorsWithIdentity :: [BackendConstructor]
  }
  deriving (Show)

instance Eq BackendData where
  left == right =
    symbolRefMatches (backendDataIdentity left) (backendDataName left) (backendDataIdentity right) (backendDataName right)
      && backendDataParameterRefs left == backendDataParameterRefs right
      && backendDataConstructors left == backendDataConstructors right

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
  where
    BackendData name parameters constructors =
      BackendDataWithIdentity Nothing name (map (backendDataParameterRef Nothing) parameters) constructors

{-# COMPLETE BackendData #-}

backendDataParameterRefKey :: BackendDataParameterRef -> BackendTypeSubstitutionKey
backendDataParameterRefKey =
  \case
    BackendDataParameterByIdentity identity _ -> backendTypeSubstitutionKeyFromIdentity identity
    BackendDataParameterByName name -> BackendTypeSubstitutionByName name

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
  { backendConstructorIdentity :: Maybe SymbolIdentity,
    backendConstructorNameWithIdentity :: String,
    backendConstructorForallsWithIdentity :: [BackendTypeBinder],
    backendConstructorFieldsWithIdentity :: [BackendType],
    backendConstructorResultWithIdentity :: BackendType
  }
  deriving (Show)

instance Eq BackendConstructor where
  left == right =
    symbolRefMatches (backendConstructorIdentity left) (backendConstructorName left) (backendConstructorIdentity right) (backendConstructorName right)
      && backendConstructorForalls left == backendConstructorForalls right
      && backendConstructorFields left == backendConstructorFields right
      && backendConstructorResult left == backendConstructorResult right

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
  where
    BackendConstructor name foralls fields result =
      BackendConstructorWithIdentity Nothing name foralls fields result

{-# COMPLETE BackendConstructor #-}

data BackendClosureCapture = BackendClosureCapture
  { backendClosureCaptureIdentity :: Maybe IdDetails,
    backendClosureCaptureName :: String,
    backendClosureCaptureType :: BackendType,
    backendClosureCaptureExpr :: BackendExpr
  }
  deriving (Show)

instance Eq BackendClosureCapture where
  left == right =
    backendTermRefMatches (backendClosureCaptureIdentity left) (backendClosureCaptureName left) (backendClosureCaptureIdentity right) (backendClosureCaptureName right)
      && backendClosureCaptureType left == backendClosureCaptureType right
      && backendClosureCaptureExpr left == backendClosureCaptureExpr right

data BackendClosureParam = BackendClosureParam
  { backendClosureParamIdentity :: Maybe IdDetails,
    backendClosureParamName :: String,
    backendClosureParamType :: BackendType
  }
  deriving (Show)

instance Eq BackendClosureParam where
  left == right =
    backendTermRefMatches (backendClosureParamIdentity left) (backendClosureParamName left) (backendClosureParamIdentity right) (backendClosureParamName right)
      && backendClosureParamType left == backendClosureParamType right

data BackendTypeBinder = BackendTypeBinderWithIdentity
  { backendTypeBinderIdentity :: Maybe TypeBinderIdentity,
    backendTypeBinderName :: String,
    backendTypeBinderBound :: Maybe BackendType
  }
  deriving (Show)

instance Eq BackendTypeBinder where
  left == right =
    typeBinderRefMatches (backendTypeBinderIdentity left) (backendTypeBinderName left) (backendTypeBinderIdentity right) (backendTypeBinderName right)
      && backendTypeBinderBound left == backendTypeBinderBound right

pattern BackendTypeBinder :: String -> Maybe BackendType -> BackendTypeBinder
pattern BackendTypeBinder name bound <-
  BackendTypeBinderWithIdentity _ name bound
  where
    BackendTypeBinder name bound =
      BackendTypeBinderWithIdentity Nothing name bound

{-# COMPLETE BackendTypeBinder #-}

data BackendBinding = BackendBindingWithMetadata
  { backendBindingIdentity :: Maybe SymbolIdentity,
    backendBindingNameWithMetadata :: String,
    backendBindingTypeWithMetadata :: BackendType,
    backendBindingExprWithMetadata :: BackendExpr,
    backendBindingExportedAsMainWithMetadata :: Bool,
    backendBindingEvidenceParamIndices :: Set.Set Int
  }
  deriving (Show)

instance Eq BackendBinding where
  left == right =
    symbolRefMatches (backendBindingIdentity left) (backendBindingName left) (backendBindingIdentity right) (backendBindingName right)
      && backendBindingType left == backendBindingType right
      && backendBindingExpr left == backendBindingExpr right
      && backendBindingExportedAsMain left == backendBindingExportedAsMain right
      && backendBindingEvidenceParamIndices left == backendBindingEvidenceParamIndices right

pattern BackendBinding :: String -> BackendType -> BackendExpr -> Bool -> BackendBinding
pattern BackendBinding
  { backendBindingName,
    backendBindingType,
    backendBindingExpr,
    backendBindingExportedAsMain
  } <-
  BackendBindingWithMetadata _ backendBindingName backendBindingType backendBindingExpr backendBindingExportedAsMain _
  where
    BackendBinding name ty expr exportedAsMain =
      BackendBindingWithMetadata Nothing name ty expr exportedAsMain Set.empty

{-# COMPLETE BackendBinding #-}

-- | Backend type language. This mirrors the checked xMLF type shapes that are
-- meaningful after `.mlfp` checking, but keeps the backend boundary independent
-- from the elaborator's term representation.
data BackendType
  = BTVarWithIdentity (Maybe TypeBinderIdentity) String
  | BTArrow BackendType BackendType
  | BTBaseWithIdentity (Maybe SymbolIdentity) BaseTy
  | BTConWithIdentity (Maybe SymbolIdentity) BaseTy (NonEmpty BackendType)
  | BTVarAppWithIdentity (Maybe TypeBinderIdentity) String (NonEmpty BackendType)
  | BTForallWithIdentity (Maybe TypeBinderIdentity) String (Maybe BackendType) BackendType
  | BTMuWithIdentity (Maybe TypeBinderIdentity) String BackendType
  | BTBottom
  deriving (Show)

instance Eq BackendType where
  left == right =
    case (left, right) of
      (BTVarWithIdentity leftIdentity leftName, BTVarWithIdentity rightIdentity rightName) ->
        typeBinderRefMatches leftIdentity leftName rightIdentity rightName
      (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
        leftDom == rightDom && leftCod == rightCod
      (BTBaseWithIdentity leftIdentity leftBase, BTBaseWithIdentity rightIdentity rightBase) ->
        typeHeadRefMatches leftIdentity leftBase rightIdentity rightBase
      (BTConWithIdentity leftIdentity leftBase leftArgs, BTConWithIdentity rightIdentity rightBase rightArgs) ->
        typeHeadRefMatches leftIdentity leftBase rightIdentity rightBase && leftArgs == rightArgs
      (BTVarAppWithIdentity leftIdentity leftName leftArgs, BTVarAppWithIdentity rightIdentity rightName rightArgs) ->
        typeBinderRefMatches leftIdentity leftName rightIdentity rightName && leftArgs == rightArgs
      (BTForallWithIdentity leftIdentity leftName leftBound leftBody, BTForallWithIdentity rightIdentity rightName rightBound rightBody) ->
        typeBinderRefMatches leftIdentity leftName rightIdentity rightName && leftBound == rightBound && leftBody == rightBody
      (BTMuWithIdentity leftIdentity leftName leftBody, BTMuWithIdentity rightIdentity rightName rightBody) ->
        typeBinderRefMatches leftIdentity leftName rightIdentity rightName && leftBody == rightBody
      (BTBottom, BTBottom) ->
        True
      _ ->
        False

typeBinderRefMatches :: Maybe TypeBinderIdentity -> String -> Maybe TypeBinderIdentity -> String -> Bool
typeBinderRefMatches (Just left) _ (Just right) _ =
  left == right
typeBinderRefMatches Nothing leftName Nothing rightName =
  leftName == rightName
typeBinderRefMatches _ _ _ _ =
  False

typeHeadRefMatches :: Maybe SymbolIdentity -> BaseTy -> Maybe SymbolIdentity -> BaseTy -> Bool
typeHeadRefMatches leftIdentity (BaseTy leftName) rightIdentity (BaseTy rightName) =
  symbolRefMatches leftIdentity leftName rightIdentity rightName

backendTermRefMatches :: Maybe IdDetails -> String -> Maybe IdDetails -> String -> Bool
backendTermRefMatches =
  idDetailsRefMatches

closureEntryRefMatches :: Maybe UniqueIdentity -> String -> Maybe UniqueIdentity -> String -> Bool
closureEntryRefMatches (Just left) _ (Just right) _ =
  left == right
closureEntryRefMatches Nothing leftName Nothing rightName =
  leftName == rightName
closureEntryRefMatches _ _ _ _ =
  False

pattern BTVar :: String -> BackendType
pattern BTVar name <-
  BTVarWithIdentity _ name
  where
    BTVar name =
      BTVarWithIdentity Nothing name

pattern BTBase :: BaseTy -> BackendType
pattern BTBase base <-
  BTBaseWithIdentity _ base
  where
    BTBase base@(BaseTy name) =
      BTBaseWithIdentity (builtinTypeHeadIdentity name) base

pattern BTCon :: BaseTy -> NonEmpty BackendType -> BackendType
pattern BTCon base args <-
  BTConWithIdentity _ base args
  where
    BTCon base@(BaseTy name) args =
      BTConWithIdentity (builtinTypeHeadIdentity name) base args

pattern BTVarApp :: String -> NonEmpty BackendType -> BackendType
pattern BTVarApp name args <-
  BTVarAppWithIdentity _ name args
  where
    BTVarApp name args =
      BTVarAppWithIdentity Nothing name args

pattern BTForall :: String -> Maybe BackendType -> BackendType -> BackendType
pattern BTForall name mbBound body <-
  BTForallWithIdentity _ name mbBound body
  where
    BTForall name mbBound body =
      BTForallWithIdentity Nothing name mbBound body

pattern BTMu :: String -> BackendType -> BackendType
pattern BTMu name body <-
  BTMuWithIdentity _ name body
  where
    BTMu name body =
      BTMuWithIdentity Nothing name body

{-# COMPLETE BTVar, BTArrow, BTBase, BTCon, BTVarApp, BTForall, BTMu, BTBottom #-}

data BackendTypeSubstitutionKey
  = BackendTypeSubstitutionByIdentity TypeBinderIdentity
  | BackendTypeSubstitutionByName String
  deriving (Eq, Ord, Show)

backendTypeSubstitutionKeyFromIdentity :: TypeBinderIdentity -> BackendTypeSubstitutionKey
backendTypeSubstitutionKeyFromIdentity =
  BackendTypeSubstitutionByIdentity

backendTypeSubstitutionKeyFor :: Maybe TypeBinderIdentity -> String -> BackendTypeSubstitutionKey
backendTypeSubstitutionKeyFor mbIdentity name =
  case mbIdentity of
    Just identity -> BackendTypeSubstitutionByIdentity identity
    Nothing -> BackendTypeSubstitutionByName name

backendTypeSubstitutionKeyIdentity :: BackendTypeSubstitutionKey -> Maybe TypeBinderIdentity
backendTypeSubstitutionKeyIdentity =
  \case
    BackendTypeSubstitutionByIdentity identity -> Just identity
    BackendTypeSubstitutionByName {} -> Nothing

backendTypeSubstitutionKeyName :: BackendTypeSubstitutionKey -> String
backendTypeSubstitutionKeyName =
  \case
    BackendTypeSubstitutionByIdentity identity -> typeBinderIdentityStableName identity
    BackendTypeSubstitutionByName name -> name

-- | Typed backend expression. `backendExprType` is the result type of the node.
data BackendExpr
  = BackendVarWithIdentity
      { backendExprType :: BackendType,
        backendVarIdentity :: Maybe IdDetails,
        backendVarName :: String
      }
  | BackendLit
      { backendExprType :: BackendType,
        backendLit :: Lit
      }
  | BackendLamWithIdentity
      { backendExprType :: BackendType,
        backendParamIdentity :: Maybe IdDetails,
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
        backendLetIdentity :: Maybe IdDetails,
        backendLetName :: String,
        backendLetType :: BackendType,
        backendLetRhs :: BackendExpr,
        backendLetBody :: BackendExpr
      }
  | BackendTyAbsWithIdentity
      { backendExprType :: BackendType,
        backendTyParamIdentity :: Maybe TypeBinderIdentity,
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
        backendClosureEntryIdentity :: Maybe UniqueIdentity,
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
        backendConstructIdentity :: Maybe SymbolIdentity,
        backendConstructName :: String,
        backendConstructArgs :: [BackendExpr]
      }
  | BackendCase
      { backendExprType :: BackendType,
        backendScrutinee :: BackendExpr,
        backendAlternatives :: NonEmpty BackendAlternative
      }
  deriving (Show)

instance Eq BackendExpr where
  left == right =
    case (left, right) of
      (BackendVarWithIdentity leftTy leftIdentity leftName, BackendVarWithIdentity rightTy rightIdentity rightName) ->
        leftTy == rightTy && backendTermRefMatches leftIdentity leftName rightIdentity rightName
      (BackendLit leftTy leftLit, BackendLit rightTy rightLit) ->
        leftTy == rightTy && leftLit == rightLit
      (BackendLamWithIdentity leftTy leftIdentity leftName leftParamTy leftBody, BackendLamWithIdentity rightTy rightIdentity rightName rightParamTy rightBody) ->
        leftTy == rightTy
          && backendTermRefMatches leftIdentity leftName rightIdentity rightName
          && leftParamTy == rightParamTy
          && leftBody == rightBody
      (BackendApp leftTy leftFun leftArg, BackendApp rightTy rightFun rightArg) ->
        leftTy == rightTy && leftFun == rightFun && leftArg == rightArg
      (BackendLetWithIdentity leftTy leftIdentity leftName leftBindingTy leftRhs leftBody, BackendLetWithIdentity rightTy rightIdentity rightName rightBindingTy rightRhs rightBody) ->
        leftTy == rightTy
          && backendTermRefMatches leftIdentity leftName rightIdentity rightName
          && leftBindingTy == rightBindingTy
          && leftRhs == rightRhs
          && leftBody == rightBody
      (BackendTyAbsWithIdentity leftTy leftIdentity leftName leftBound leftBody, BackendTyAbsWithIdentity rightTy rightIdentity rightName rightBound rightBody) ->
        leftTy == rightTy
          && typeBinderRefMatches leftIdentity leftName rightIdentity rightName
          && leftBound == rightBound
          && leftBody == rightBody
      (BackendTyApp leftTy leftFun leftArg, BackendTyApp rightTy rightFun rightArg) ->
        leftTy == rightTy && leftFun == rightFun && leftArg == rightArg
      (BackendRoll leftTy leftPayload, BackendRoll rightTy rightPayload) ->
        leftTy == rightTy && leftPayload == rightPayload
      (BackendUnroll leftTy leftPayload, BackendUnroll rightTy rightPayload) ->
        leftTy == rightTy && leftPayload == rightPayload
      (BackendClosureWithParamIdentities leftTy leftEntryIdentity leftEntry leftCaptures leftParams leftBody, BackendClosureWithParamIdentities rightTy rightEntryIdentity rightEntry rightCaptures rightParams rightBody) ->
        leftTy == rightTy
          && closureEntryRefMatches leftEntryIdentity leftEntry rightEntryIdentity rightEntry
          && leftCaptures == rightCaptures
          && leftParams == rightParams
          && leftBody == rightBody
      (BackendClosureCall leftTy leftFun leftArgs, BackendClosureCall rightTy rightFun rightArgs) ->
        leftTy == rightTy && leftFun == rightFun && leftArgs == rightArgs
      (BackendConstructWithIdentity leftTy leftIdentity leftName leftArgs, BackendConstructWithIdentity rightTy rightIdentity rightName rightArgs) ->
        leftTy == rightTy
          && symbolRefMatches leftIdentity leftName rightIdentity rightName
          && leftArgs == rightArgs
      (BackendCase leftTy leftScrutinee leftAlternatives, BackendCase rightTy rightScrutinee rightAlternatives) ->
        leftTy == rightTy && leftScrutinee == rightScrutinee && leftAlternatives == rightAlternatives
      _ ->
        False

pattern BackendVar :: BackendType -> String -> BackendExpr
pattern BackendVar resultTy name <-
  BackendVarWithIdentity resultTy _ name
  where
    BackendVar resultTy name =
      BackendVarWithIdentity resultTy Nothing name

pattern BackendLam :: BackendType -> String -> BackendType -> BackendExpr -> BackendExpr
pattern BackendLam resultTy name paramTy body <-
  BackendLamWithIdentity resultTy _ name paramTy body
  where
    BackendLam resultTy name paramTy body =
      BackendLamWithIdentity resultTy Nothing name paramTy body

pattern BackendLet :: BackendType -> String -> BackendType -> BackendExpr -> BackendExpr -> BackendExpr
pattern BackendLet resultTy name bindingTy rhs body <-
  BackendLetWithIdentity resultTy _ name bindingTy rhs body
  where
    BackendLet resultTy name bindingTy rhs body =
      BackendLetWithIdentity resultTy Nothing name bindingTy rhs body

pattern BackendConstruct :: BackendType -> String -> [BackendExpr] -> BackendExpr
pattern BackendConstruct resultTy name args <-
  BackendConstructWithIdentity resultTy _ name args
  where
    BackendConstruct resultTy name args =
      BackendConstructWithIdentity resultTy Nothing name args

pattern BackendTyAbs :: BackendType -> String -> Maybe BackendType -> BackendExpr -> BackendExpr
pattern BackendTyAbs resultTy name mbBound body <-
  BackendTyAbsWithIdentity resultTy _ name mbBound body
  where
    BackendTyAbs resultTy name mbBound body =
      BackendTyAbsWithIdentity resultTy Nothing name mbBound body

pattern BackendClosure :: BackendType -> String -> [BackendClosureCapture] -> [(String, BackendType)] -> BackendExpr -> BackendExpr
pattern BackendClosure resultTy entryName captures params body <-
  BackendClosureWithParamIdentities
    resultTy
    _
    entryName
    captures
    (map backendClosureParamPair -> params)
    body
  where
    BackendClosure resultTy entryName captures params body =
      BackendClosureWithParamIdentities
        resultTy
        Nothing
        entryName
        captures
        (backendClosureParams params)
        body

{-# COMPLETE BackendVar, BackendLit, BackendLam, BackendApp, BackendLet, BackendTyAbs, BackendTyApp, BackendRoll, BackendUnroll, BackendClosure, BackendClosureCall, BackendConstruct, BackendCase #-}
{-# COMPLETE BackendVarWithIdentity, BackendLit, BackendLam, BackendApp, BackendLet, BackendTyAbs, BackendTyApp, BackendRoll, BackendUnroll, BackendClosure, BackendClosureCall, BackendConstructWithIdentity, BackendCase #-}
{-# COMPLETE BackendVarWithIdentity, BackendLit, BackendLamWithIdentity, BackendApp, BackendLetWithIdentity, BackendTyAbs, BackendTyApp, BackendRoll, BackendUnroll, BackendClosureWithParamIdentities, BackendClosureCall, BackendConstructWithIdentity, BackendCase #-}

backendClosureParamPair :: BackendClosureParam -> (String, BackendType)
backendClosureParamPair param =
  (backendClosureParamName param, backendClosureParamType param)

backendClosureParams :: [(String, BackendType)] -> [BackendClosureParam]
backendClosureParams params =
  [BackendClosureParam Nothing name ty | (name, ty) <- params]

data BackendAlternative = BackendAlternative
  { backendAltPattern :: BackendPattern,
    backendAltBody :: BackendExpr
  }
  deriving (Eq, Show)

data BackendPatternBinder = BackendPatternBinder
  { backendPatternBinderIdentity :: Maybe IdDetails,
    backendPatternBinderName :: String
  }
  deriving (Show)

instance Eq BackendPatternBinder where
  left == right =
    backendTermRefMatches (backendPatternBinderIdentity left) (backendPatternBinderName left) (backendPatternBinderIdentity right) (backendPatternBinderName right)

data BackendPattern
  = BackendDefaultPattern
  | BackendConstructorPatternWithBinderIdentities (Maybe SymbolIdentity) String [BackendPatternBinder]
  deriving (Show)

instance Eq BackendPattern where
  left == right =
    case (left, right) of
      (BackendDefaultPattern, BackendDefaultPattern) ->
        True
      (BackendConstructorPatternWithBinderIdentities leftIdentity leftName leftBinders, BackendConstructorPatternWithBinderIdentities rightIdentity rightName rightBinders) ->
        symbolRefMatches leftIdentity leftName rightIdentity rightName && leftBinders == rightBinders
      _ ->
        False

pattern BackendConstructorPattern :: String -> [String] -> BackendPattern
pattern BackendConstructorPattern name binders <-
  BackendConstructorPatternWithBinderIdentities _ name (map backendPatternBinderName -> binders)
  where
    BackendConstructorPattern name binders =
      BackendConstructorPatternWithBinderIdentities
        Nothing
        name
        [BackendPatternBinder Nothing binder | binder <- binders]

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
  BTBaseWithIdentity (Just (builtinTypeIdentity name)) (BaseTy name)

freeBackendTypeVars :: BackendType -> Set.Set String
freeBackendTypeVars =
  Set.map backendTypeSubstitutionKeyName . freeBackendTypeVarKeys

freeBackendTypeVarsInKeyed :: Map.Map BackendTypeSubstitutionKey BackendType -> Set.Set String
freeBackendTypeVarsInKeyed replacements =
  Set.unions (map freeBackendTypeVars (Map.elems replacements))

freeBackendTypeVarRefs :: BackendType -> Set.Set BackendDataParameterRef
freeBackendTypeVarRefs =
  go Set.empty
  where
    go bound =
      \case
        BTVarWithIdentity identity name
          | Set.member (backendTypeSubstitutionKeyFor identity name) bound -> Set.empty
          | otherwise -> freeBackendTypeVarRef identity name
        BTArrow dom cod ->
          Set.union (go bound dom) (go bound cod)
        BTBaseWithIdentity {} ->
          Set.empty
        BTConWithIdentity _ _ args ->
          Set.unions (map (go bound) (NE.toList args))
        BTVarAppWithIdentity identity name args ->
          let headRefs =
                if Set.member (backendTypeSubstitutionKeyFor identity name) bound
                  then Set.empty
                  else freeBackendTypeVarRef identity name
           in Set.union headRefs (Set.unions (map (go bound) (NE.toList args)))
        BTForallWithIdentity identity name mbBound body ->
          Set.union
            (maybe Set.empty (go bound) mbBound)
            (go (Set.insert (backendTypeSubstitutionKeyFor identity name) bound) body)
        BTMuWithIdentity identity name body ->
          go (Set.insert (backendTypeSubstitutionKeyFor identity name) bound) body
        BTBottom ->
          Set.empty

    freeBackendTypeVarRef identity name =
      case identity of
        Just refIdentity ->
          Set.singleton (backendDataParameterRefFromIdentity refIdentity name)
        Nothing ->
          Set.empty

freeBackendTypeVarKeys :: BackendType -> Set.Set BackendTypeSubstitutionKey
freeBackendTypeVarKeys =
  \case
    BTVarWithIdentity identity name ->
      Set.singleton (backendTypeSubstitutionKeyFor identity name)
    BTArrow dom cod ->
      Set.union (freeBackendTypeVarKeys dom) (freeBackendTypeVarKeys cod)
    BTBaseWithIdentity {} ->
      Set.empty
    BTConWithIdentity _ _ args ->
      Set.unions (map freeBackendTypeVarKeys (NE.toList args))
    BTVarAppWithIdentity identity name args ->
      Set.insert
        (backendTypeSubstitutionKeyFor identity name)
        (Set.unions (map freeBackendTypeVarKeys (NE.toList args)))
    BTForallWithIdentity identity name mbBound body ->
      Set.union
        (maybe Set.empty freeBackendTypeVarKeys mbBound)
        (Set.delete (backendTypeSubstitutionKeyFor identity name) (freeBackendTypeVarKeys body))
    BTMuWithIdentity identity name body ->
      Set.delete (backendTypeSubstitutionKeyFor identity name) (freeBackendTypeVarKeys body)
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
  maybe [] symbolGeneratedIdentities (backendProgramMainIdentity program)
    ++ foldMap generatedIdentitiesInBackendModule (backendProgramModules program)

generatedIdentitiesInBackendModule :: BackendModule -> [UniqueIdentity]
generatedIdentitiesInBackendModule backendModule =
  maybe [] symbolGeneratedIdentities (backendModuleIdentity backendModule)
    ++ foldMap generatedIdentitiesInBackendData (backendModuleData backendModule)
    ++ foldMap generatedIdentitiesInBackendBinding (backendModuleBindings backendModule)

generatedIdentitiesInBackendData :: BackendData -> [UniqueIdentity]
generatedIdentitiesInBackendData dataDecl =
  maybe [] symbolGeneratedIdentities (backendDataIdentity dataDecl)
    ++ foldMap generatedIdentitiesInBackendDataParameterRef (backendDataParameterRefs dataDecl)
    ++ foldMap generatedIdentitiesInBackendConstructor (backendDataConstructors dataDecl)

generatedIdentitiesInBackendDataParameterRef :: BackendDataParameterRef -> [UniqueIdentity]
generatedIdentitiesInBackendDataParameterRef ref =
  generatedIdentitiesInTypeBinderRef (backendDataParameterRefIdentity ref) (backendDataParameterRefName ref)

generatedIdentitiesInBackendConstructor :: BackendConstructor -> [UniqueIdentity]
generatedIdentitiesInBackendConstructor constructor =
  maybe [] symbolGeneratedIdentities (backendConstructorIdentity constructor)
    ++ foldMap generatedIdentitiesInBackendTypeBinder (backendConstructorForalls constructor)
    ++ generatedIdentitiesInBackendTypes (backendConstructorFields constructor)
    ++ generatedIdentitiesInBackendType (backendConstructorResult constructor)

generatedIdentitiesInBackendTypeBinder :: BackendTypeBinder -> [UniqueIdentity]
generatedIdentitiesInBackendTypeBinder binder =
  generatedIdentitiesInTypeBinderRef (backendTypeBinderIdentity binder) (backendTypeBinderName binder)
    ++ maybe [] generatedIdentitiesInBackendType (backendTypeBinderBound binder)

generatedIdentitiesInBackendBinding :: BackendBinding -> [UniqueIdentity]
generatedIdentitiesInBackendBinding binding =
  maybe [] symbolGeneratedIdentities (backendBindingIdentity binding)
    ++ generatedIdentitiesInBackendType (backendBindingType binding)
    ++ generatedIdentitiesInBackendExpr (backendBindingExpr binding)

generatedIdentitiesInBackendExpr :: BackendExpr -> [UniqueIdentity]
generatedIdentitiesInBackendExpr =
  \case
    BackendVarWithIdentity ty identity _ ->
      generatedIdentitiesInBackendType ty ++ foldMap idDetailsGeneratedIdentities identity
    BackendLit ty _ ->
      generatedIdentitiesInBackendType ty
    BackendLamWithIdentity resultTy identity _ paramTy body ->
      generatedIdentitiesInBackendType resultTy
        ++ foldMap idDetailsGeneratedIdentities identity
        ++ generatedIdentitiesInBackendType paramTy
        ++ generatedIdentitiesInBackendExpr body
    BackendApp resultTy fun arg ->
      generatedIdentitiesInBackendType resultTy ++ generatedIdentitiesInBackendExpr fun ++ generatedIdentitiesInBackendExpr arg
    BackendLetWithIdentity resultTy identity _ bindingTy rhs body ->
      generatedIdentitiesInBackendType resultTy
        ++ foldMap idDetailsGeneratedIdentities identity
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
        ++ maybe [] (: []) identity
        ++ foldMap generatedIdentitiesInBackendClosureCapture captures
        ++ foldMap generatedIdentitiesInBackendClosureParam params
        ++ generatedIdentitiesInBackendExpr body
    BackendClosureCall resultTy fun args ->
      generatedIdentitiesInBackendType resultTy ++ generatedIdentitiesInBackendExpr fun ++ foldMap generatedIdentitiesInBackendExpr args
    BackendConstructWithIdentity resultTy identity _ args ->
      generatedIdentitiesInBackendType resultTy
        ++ maybe [] symbolGeneratedIdentities identity
        ++ foldMap generatedIdentitiesInBackendExpr args
    BackendCase resultTy scrutinee alternatives ->
      generatedIdentitiesInBackendType resultTy
        ++ generatedIdentitiesInBackendExpr scrutinee
        ++ foldMap generatedIdentitiesInBackendAlternative alternatives

generatedIdentitiesInBackendClosureCapture :: BackendClosureCapture -> [UniqueIdentity]
generatedIdentitiesInBackendClosureCapture capture =
  foldMap idDetailsGeneratedIdentities (backendClosureCaptureIdentity capture)
    ++ generatedIdentitiesInBackendType (backendClosureCaptureType capture)
    ++ generatedIdentitiesInBackendExpr (backendClosureCaptureExpr capture)

generatedIdentitiesInBackendClosureParam :: BackendClosureParam -> [UniqueIdentity]
generatedIdentitiesInBackendClosureParam param =
  foldMap idDetailsGeneratedIdentities (backendClosureParamIdentity param)
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
      maybe [] symbolGeneratedIdentities identity
        ++ foldMap generatedIdentitiesInBackendPatternBinder binders

generatedIdentitiesInBackendPatternBinder :: BackendPatternBinder -> [UniqueIdentity]
generatedIdentitiesInBackendPatternBinder binder =
  foldMap idDetailsGeneratedIdentities (backendPatternBinderIdentity binder)

generatedIdentitiesInBackendType :: BackendType -> [UniqueIdentity]
generatedIdentitiesInBackendType =
  \case
    BTVarWithIdentity identity name ->
      generatedIdentitiesInTypeBinderRef identity name
    BTArrow dom cod ->
      generatedIdentitiesInBackendType dom ++ generatedIdentitiesInBackendType cod
    BTBaseWithIdentity identity _ ->
      maybe [] symbolGeneratedIdentities identity
    BTConWithIdentity identity _ args ->
      maybe [] symbolGeneratedIdentities identity ++ foldMap generatedIdentitiesInBackendType args
    BTVarAppWithIdentity identity name args ->
      generatedIdentitiesInTypeBinderRef identity name ++ foldMap generatedIdentitiesInBackendType args
    BTForallWithIdentity identity name mbBound body ->
      generatedIdentitiesInTypeBinderRef identity name
        ++ maybe [] generatedIdentitiesInBackendType mbBound
        ++ generatedIdentitiesInBackendType body
    BTMuWithIdentity identity name body ->
      generatedIdentitiesInTypeBinderRef identity name ++ generatedIdentitiesInBackendType body
    BTBottom ->
      []

generatedIdentitiesInTypeBinderIdentity :: Maybe TypeBinderIdentity -> [UniqueIdentity]
generatedIdentitiesInTypeBinderIdentity =
  maybe [] typeBinderGeneratedIdentities

generatedIdentitiesInTypeBinderRef :: Maybe TypeBinderIdentity -> String -> [UniqueIdentity]
generatedIdentitiesInTypeBinderRef identity _ =
  generatedIdentitiesInTypeBinderIdentity identity

generatedIdentitiesInBackendTypeSubstitutionKey :: BackendTypeSubstitutionKey -> [UniqueIdentity]
generatedIdentitiesInBackendTypeSubstitutionKey =
  \case
    BackendTypeSubstitutionByIdentity identity -> generatedIdentitiesInTypeBinderIdentity (Just identity)
    BackendTypeSubstitutionByName {} -> []

backendTypeHeadMatches :: Maybe SymbolIdentity -> BaseTy -> Maybe SymbolIdentity -> BaseTy -> Bool
backendTypeHeadMatches =
  typeHeadRefMatches

substituteBackendTypeByIdentity :: TypeBinderIdentity -> BackendType -> BackendType -> BackendType
substituteBackendTypeByIdentity needle replacement =
  substituteBackendTypesByKey (Map.singleton (BackendTypeSubstitutionByIdentity needle) replacement)

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
        BTVarWithIdentity identity name ->
          (lookupTypeReplacement identity name replacements ty, generator)
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
           in case lookupTypeReplacementMaybe identity name replacements >>= (`applyBackendTypeHead` NE.toList args') of
                Just ty' -> (ty', generator')
                Nothing -> (BTVarAppWithIdentity identity name args', generator')
        BTForallWithIdentity identity name mbBound body
          ->
            let (mbBound', generator') = goMaybe replacements generator mbBound
                bodyReplacements = deleteTypeReplacement identity name replacements
             in if Map.null bodyReplacements
                  then (BTForallWithIdentity identity name mbBound' body, generator')
                  else
                    let binderKey = backendTypeSubstitutionKeyFor identity name
                        freeBodyReplacementKeys = freeBackendTypeVarKeysInKeyed bodyReplacements
                     in if Set.member binderKey freeBodyReplacementKeys
                          then
                            let used =
                                  Set.unions
                                    [ freeBackendTypeVarsInKeyed bodyReplacements,
                                      freeBackendTypeVars body,
                                      maybe Set.empty freeBackendTypeVars mbBound,
                                      Set.map backendTypeSubstitutionKeyName (Map.keysSet bodyReplacements),
                                      Set.singleton name
                                    ]
                                name' = freshNameLike name used
                                (identity', generator'') = freshBackendBinderIdentity identity generator'
                                body' = renameBackendTypeBinder identity name identity' name' body
                                (body'', generator''') = go bodyReplacements generator'' body'
                             in (BTForallWithIdentity identity' name' mbBound' body'', generator''')
                          else
                            let (body', generator'') = go bodyReplacements generator' body
                             in (BTForallWithIdentity identity name mbBound' body', generator'')
        BTMuWithIdentity identity name body
          ->
            let bodyReplacements = deleteTypeReplacement identity name replacements
             in if Map.null bodyReplacements
                  then (ty, generator)
                  else
                    let binderKey = backendTypeSubstitutionKeyFor identity name
                        freeBodyReplacementKeys = freeBackendTypeVarKeysInKeyed bodyReplacements
                     in if Set.member binderKey freeBodyReplacementKeys
                          then
                            let used =
                                  Set.unions
                                    [ freeBackendTypeVarsInKeyed bodyReplacements,
                                      freeBackendTypeVars body,
                                      Set.map backendTypeSubstitutionKeyName (Map.keysSet bodyReplacements),
                                      Set.singleton name
                                    ]
                                name' = freshNameLike name used
                                (identity', generator') = freshBackendBinderIdentity identity generator
                                body' = renameBackendTypeBinder identity name identity' name' body
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

lookupTypeReplacement :: Maybe TypeBinderIdentity -> String -> Map.Map BackendTypeSubstitutionKey BackendType -> BackendType -> BackendType
lookupTypeReplacement identity name replacements fallback =
  case lookupTypeReplacementMaybe identity name replacements of
    Just replacement -> replacement
    Nothing -> fallback

lookupTypeReplacementMaybe :: Maybe TypeBinderIdentity -> String -> Map.Map BackendTypeSubstitutionKey BackendType -> Maybe BackendType
lookupTypeReplacementMaybe identity name replacements =
  Map.lookup (backendTypeSubstitutionKeyFor identity name) replacements

deleteTypeReplacement :: Maybe TypeBinderIdentity -> String -> Map.Map BackendTypeSubstitutionKey BackendType -> Map.Map BackendTypeSubstitutionKey BackendType
deleteTypeReplacement identity name =
  Map.delete (backendTypeSubstitutionKeyFor identity name)

freshBackendBinderIdentity :: Maybe TypeBinderIdentity -> IdentityGenerator -> (Maybe TypeBinderIdentity, IdentityGenerator)
freshBackendBinderIdentity _ generator =
  let (unique, generator') = freshIdentity generator
   in (Just (typeBinderIdentityFromUnique unique), generator')

renameBackendTypeBinder ::
  Maybe TypeBinderIdentity ->
  String ->
  Maybe TypeBinderIdentity ->
  String ->
  BackendType ->
  BackendType
renameBackendTypeBinder oldIdentity oldName newIdentity newName =
  go
  where
    replacement = BTVarWithIdentity newIdentity newName

    matches identity name =
      typeBinderRefMatches identity name oldIdentity oldName

    go ty =
      case ty of
        BTVarWithIdentity identity name
          | matches identity name -> replacement
          | otherwise -> ty
        BTArrow dom cod ->
          BTArrow (go dom) (go cod)
        BTBaseWithIdentity {} ->
          ty
        BTConWithIdentity identity con args ->
          BTConWithIdentity identity con (fmap go args)
        BTVarAppWithIdentity identity name args
          | matches identity name ->
              maybe replacement id (applyBackendTypeHead replacement (NE.toList (fmap go args)))
          | otherwise ->
              BTVarAppWithIdentity identity name (fmap go args)
        BTForallWithIdentity identity name mbBound body
          | matches identity name ->
              BTForallWithIdentity identity name (fmap go mbBound) body
          | otherwise ->
              BTForallWithIdentity identity name (fmap go mbBound) (go body)
        BTMuWithIdentity identity name body
          | matches identity name -> ty
          | otherwise -> BTMuWithIdentity identity name (go body)
        BTBottom ->
          BTBottom

substituteBackendTypeForBinder :: Maybe TypeBinderIdentity -> String -> BackendType -> BackendType -> BackendType
substituteBackendTypeForBinder identity _ replacement ty =
  case identity of
    Just resolvedIdentity -> substituteBackendTypeByIdentity resolvedIdentity replacement ty
    Nothing -> ty

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
    BTMuWithIdentity identity name body -> Just (substituteBackendTypeForBinder identity name ty body)
    _ -> Nothing
