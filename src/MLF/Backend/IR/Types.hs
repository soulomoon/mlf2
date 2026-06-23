{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module MLF.Backend.IR.Types
  ( BackendProgram (..),
    BackendModule (..),
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
        backendDataParametersWithIdentity,
        backendDataParameterIdentities,
        backendDataConstructorsWithIdentity
      ),
    pattern BackendData,
    backendDataName,
    backendDataParameters,
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
    BackendTypeSubstitutionKey (..),
    backendTypeSubstitutionKeyFor,
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
    pattern BackendConstructorPatternWithIdentity,
    pattern BackendConstructorPattern,
    freeBackendTypeVars,
    freeBackendTypeVarsIn,
    freeBackendTypeVarsInKeyed,
    backendTypeHeadMatches,
    literalBackendType,
    substituteBackendType,
    substituteBackendTypeByIdentity,
    substituteBackendTypeForBinder,
    substituteBackendTypes,
    substituteBackendTypesByKey,
    unfoldBackendRecursiveType,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Program.Builtins (builtinTypeIdentity)
import MLF.Frontend.Symbol (SymbolIdentity (..))
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity (IdDetails, TypeBinderIdentity)
import MLF.Util.Names (freshNameLike)

-- | A checked backend program. Module order is preserved from the source
-- program for diagnostics/debug output, but backend binding names are global
-- runtime names.
data BackendProgram = BackendProgram
  { backendProgramModules :: [BackendModule],
    backendProgramMain :: String
  }
  deriving (Eq, Show)

-- | Backend-owned module payload. Imports/exports have already been resolved
-- by the `.mlfp` checker; this record keeps only the data and binding shapes
-- needed by backend conversion and lowering.
data BackendModule = BackendModule
  { backendModuleName :: String,
    backendModuleData :: [BackendData],
    backendModuleBindings :: [BackendBinding]
  }
  deriving (Eq, Show)

-- | Explicit ADT metadata available to lowerers. Constructor result types are
-- kept explicit so GADT-style results can survive the source-to-backend cut.
data BackendData = BackendDataWithIdentity
  { backendDataIdentity :: Maybe SymbolIdentity,
    backendDataNameWithIdentity :: String,
    backendDataParametersWithIdentity :: [String],
    backendDataParameterIdentities :: [Maybe TypeBinderIdentity],
    backendDataConstructorsWithIdentity :: [BackendConstructor]
  }
  deriving (Eq, Show)

pattern BackendData :: String -> [String] -> [BackendConstructor] -> BackendData
pattern BackendData
  { backendDataName,
    backendDataParameters,
    backendDataConstructors
  } <-
  BackendDataWithIdentity
    _
    backendDataName
    backendDataParameters
    _
    backendDataConstructors
  where
    BackendData name parameters constructors =
      BackendDataWithIdentity Nothing name parameters (replicate (length parameters) Nothing) constructors

{-# COMPLETE BackendData #-}

backendDataParameterKeys :: BackendData -> [BackendTypeSubstitutionKey]
backendDataParameterKeys dataDecl =
  zipWith backendTypeSubstitutionKeyFor identities (backendDataParameters dataDecl)
  where
    identities =
      backendDataParameterIdentities dataDecl ++ repeat Nothing

data BackendConstructor = BackendConstructorWithIdentity
  { backendConstructorIdentity :: Maybe SymbolIdentity,
    backendConstructorNameWithIdentity :: String,
    backendConstructorForallsWithIdentity :: [BackendTypeBinder],
    backendConstructorFieldsWithIdentity :: [BackendType],
    backendConstructorResultWithIdentity :: BackendType
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data BackendClosureParam = BackendClosureParam
  { backendClosureParamIdentity :: Maybe IdDetails,
    backendClosureParamName :: String,
    backendClosureParamType :: BackendType
  }
  deriving (Eq, Show)

data BackendTypeBinder = BackendTypeBinderWithIdentity
  { backendTypeBinderIdentity :: Maybe TypeBinderIdentity,
    backendTypeBinderName :: String,
    backendTypeBinderBound :: Maybe BackendType
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
        leftIdentity == rightIdentity && leftBase == rightBase
      (BTConWithIdentity leftIdentity leftBase leftArgs, BTConWithIdentity rightIdentity rightBase rightArgs) ->
        leftIdentity == rightIdentity && leftBase == rightBase && leftArgs == rightArgs
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
    BTBase base =
      BTBaseWithIdentity Nothing base

pattern BTCon :: BaseTy -> NonEmpty BackendType -> BackendType
pattern BTCon base args <-
  BTConWithIdentity _ base args
  where
    BTCon base args =
      BTConWithIdentity Nothing base args

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

backendTypeSubstitutionKeyFor :: Maybe TypeBinderIdentity -> String -> BackendTypeSubstitutionKey
backendTypeSubstitutionKeyFor (Just identity) _ =
  BackendTypeSubstitutionByIdentity identity
backendTypeSubstitutionKeyFor Nothing name =
  BackendTypeSubstitutionByName name

backendTypeSubstitutionKeyName :: BackendTypeSubstitutionKey -> String
backendTypeSubstitutionKeyName =
  \case
    BackendTypeSubstitutionByIdentity identity -> show identity
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
  deriving (Eq, Show)

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
    entryName
    captures
    (map backendClosureParamPair -> params)
    body
  where
    BackendClosure resultTy entryName captures params body =
      BackendClosureWithParamIdentities
        resultTy
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
  deriving (Eq, Show)

data BackendPattern
  = BackendDefaultPattern
  | BackendConstructorPatternWithBinderIdentities (Maybe SymbolIdentity) String [BackendPatternBinder]
  deriving (Eq, Show)

pattern BackendConstructorPatternWithIdentity :: Maybe SymbolIdentity -> String -> [String] -> BackendPattern
pattern BackendConstructorPatternWithIdentity identity name binders <-
  BackendConstructorPatternWithBinderIdentities identity name (map backendPatternBinderName -> binders)
  where
    BackendConstructorPatternWithIdentity identity name binders =
      BackendConstructorPatternWithBinderIdentities
        identity
        name
        [BackendPatternBinder Nothing binder | binder <- binders]

pattern BackendConstructorPattern :: String -> [String] -> BackendPattern
pattern BackendConstructorPattern name binders <-
  BackendConstructorPatternWithIdentity _ name binders
  where
    BackendConstructorPattern name binders =
      BackendConstructorPatternWithIdentity Nothing name binders

{-# COMPLETE BackendDefaultPattern, BackendConstructorPattern #-}
{-# COMPLETE BackendDefaultPattern, BackendConstructorPatternWithIdentity #-}
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
  \case
    BTVar name ->
      Set.singleton name
    BTArrow dom cod ->
      Set.union (freeBackendTypeVars dom) (freeBackendTypeVars cod)
    BTBase {} ->
      Set.empty
    BTCon _ args ->
      Set.unions (map freeBackendTypeVars (NE.toList args))
    BTVarApp name args ->
      Set.insert name (Set.unions (map freeBackendTypeVars (NE.toList args)))
    BTForall name mbBound body ->
      Set.union
        (maybe Set.empty freeBackendTypeVars mbBound)
        (Set.delete name (freeBackendTypeVars body))
    BTMu name body ->
      Set.delete name (freeBackendTypeVars body)
    BTBottom ->
      Set.empty

freeBackendTypeVarsIn :: Map.Map String BackendType -> Set.Set String
freeBackendTypeVarsIn replacements =
  Set.unions (map freeBackendTypeVars (Map.elems replacements))

freeBackendTypeVarsInKeyed :: Map.Map BackendTypeSubstitutionKey BackendType -> Set.Set String
freeBackendTypeVarsInKeyed replacements =
  Set.unions (map freeBackendTypeVars (Map.elems replacements))

backendTypeHeadMatches :: Maybe SymbolIdentity -> BaseTy -> Maybe SymbolIdentity -> BaseTy -> Bool
backendTypeHeadMatches leftIdentity leftBase rightIdentity rightBase =
  case (leftIdentity, rightIdentity) of
    (Just left, Just right) -> left == right
    _ ->
      leftBase == rightBase
        || identityHeadMatches leftIdentity rightBase
        || identityHeadMatches rightIdentity leftBase
  where
    identityHeadMatches Nothing _ = False
    identityHeadMatches (Just identity) (BaseTy name) =
      name == qualifiedHeadName identity || name == symbolDefiningName identity

    qualifiedHeadName identity =
      symbolDefiningModule identity ++ "." ++ symbolDefiningName identity

-- | Capture-avoiding substitution for backend types. Forall binders scope over
-- their body but not their optional bound, matching the frontend type syntax.
substituteBackendType :: String -> BackendType -> BackendType -> BackendType
substituteBackendType needle replacement =
  substituteBackendTypes (Map.singleton needle replacement)

substituteBackendTypeByIdentity :: TypeBinderIdentity -> BackendType -> BackendType -> BackendType
substituteBackendTypeByIdentity needle replacement =
  substituteBackendTypesByKey (Map.singleton (BackendTypeSubstitutionByIdentity needle) replacement)

substituteBackendTypes :: Map.Map String BackendType -> BackendType -> BackendType
substituteBackendTypes replacements0 =
  substituteBackendTypesByKey (Map.mapKeys BackendTypeSubstitutionByName replacements0)

substituteBackendTypesByKey :: Map.Map BackendTypeSubstitutionKey BackendType -> BackendType -> BackendType
substituteBackendTypesByKey replacements0 =
  go replacements0
  where
    go replacements ty =
      case ty of
        BTVarWithIdentity identity name ->
          lookupTypeReplacement identity name replacements ty
        BTArrow dom cod -> BTArrow (go replacements dom) (go replacements cod)
        BTBaseWithIdentity {} -> ty
        BTConWithIdentity identity con args ->
          BTConWithIdentity identity con (fmap (go replacements) args)
        BTVarAppWithIdentity identity name args ->
          let args' = fmap (go replacements) args
           in case lookupTypeReplacementMaybe identity name replacements >>= (`applyBackendTypeHead` NE.toList args') of
                Just ty' -> ty'
                Nothing -> BTVarAppWithIdentity identity name args'
        BTForallWithIdentity identity name mbBound body
          | Map.null bodyReplacements ->
              BTForallWithIdentity identity name (fmap (go replacements) mbBound) body
          | Set.member name freeBodyReplacements ->
              let used =
                    Set.unions
                      [ freeBodyReplacements,
                        freeBackendTypeVars body,
                        maybe Set.empty freeBackendTypeVars mbBound,
                        Set.map backendTypeSubstitutionKeyName (Map.keysSet bodyReplacements),
                        Set.singleton name
                      ]
                  name' = freshNameLike name used
                  body' = substituteBackendTypeForBinder identity name (BTVarWithIdentity identity name') body
               in BTForallWithIdentity identity name' (fmap (go replacements) mbBound) (go bodyReplacements body')
          | otherwise ->
              BTForallWithIdentity identity name (fmap (go replacements) mbBound) (go bodyReplacements body)
          where
            bodyReplacements = deleteTypeReplacement identity name replacements
            freeBodyReplacements = freeBackendTypeVarsInKeyed bodyReplacements
        BTMuWithIdentity identity name body
          | Map.null bodyReplacements ->
              ty
          | Set.member name freeBodyReplacements ->
              let used =
                    Set.unions
                      [ freeBodyReplacements,
                        freeBackendTypeVars body,
                        Set.map backendTypeSubstitutionKeyName (Map.keysSet bodyReplacements),
                        Set.singleton name
                      ]
                  name' = freshNameLike name used
                  body' = substituteBackendTypeForBinder identity name (BTVarWithIdentity identity name') body
               in BTMuWithIdentity identity name' (go bodyReplacements body')
          | otherwise ->
              BTMuWithIdentity identity name (go bodyReplacements body)
          where
            bodyReplacements = deleteTypeReplacement identity name replacements
            freeBodyReplacements = freeBackendTypeVarsInKeyed bodyReplacements
        BTBottom -> BTBottom

lookupTypeReplacement :: Maybe TypeBinderIdentity -> String -> Map.Map BackendTypeSubstitutionKey BackendType -> BackendType -> BackendType
lookupTypeReplacement identity name replacements fallback =
  case lookupTypeReplacementMaybe identity name replacements of
    Just replacement -> replacement
    Nothing -> fallback

lookupTypeReplacementMaybe :: Maybe TypeBinderIdentity -> String -> Map.Map BackendTypeSubstitutionKey BackendType -> Maybe BackendType
lookupTypeReplacementMaybe identity name replacements =
  case identity of
    Just typeIdentity -> Map.lookup (BackendTypeSubstitutionByIdentity typeIdentity) replacements
    Nothing -> Map.lookup (BackendTypeSubstitutionByName name) replacements

deleteTypeReplacement :: Maybe TypeBinderIdentity -> String -> Map.Map BackendTypeSubstitutionKey BackendType -> Map.Map BackendTypeSubstitutionKey BackendType
deleteTypeReplacement identity name =
  maybe id (Map.delete . BackendTypeSubstitutionByIdentity) identity
    . Map.delete (BackendTypeSubstitutionByName name)

substituteBackendTypeForBinder :: Maybe TypeBinderIdentity -> String -> BackendType -> BackendType -> BackendType
substituteBackendTypeForBinder (Just identity) _ replacement =
  substituteBackendTypeByIdentity identity replacement
substituteBackendTypeForBinder Nothing name replacement =
  substituteBackendType name replacement

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
    BTMu name body -> Just (substituteBackendType name ty body)
    _ -> Nothing
