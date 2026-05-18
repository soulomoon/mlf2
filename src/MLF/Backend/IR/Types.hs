{-# LANGUAGE LambdaCase #-}

module MLF.Backend.IR.Types
  ( BackendProgram (..),
    BackendModule (..),
    BackendBinding (..),
    BackendData (..),
    BackendConstructor (..),
    BackendClosureCapture (..),
    BackendTypeBinder (..),
    BackendType (..),
    BackendExpr (..),
    BackendAlternative (..),
    BackendPattern (..),
    freeBackendTypeVars,
    freeBackendTypeVarsIn,
    literalBackendType,
    substituteBackendType,
    substituteBackendTypes,
    unfoldBackendRecursiveType,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Syntax (Lit (..))
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
data BackendData = BackendData
  { backendDataName :: String,
    backendDataParameters :: [String],
    backendDataConstructors :: [BackendConstructor]
  }
  deriving (Eq, Show)

data BackendConstructor = BackendConstructor
  { backendConstructorName :: String,
    backendConstructorForalls :: [BackendTypeBinder],
    backendConstructorFields :: [BackendType],
    backendConstructorResult :: BackendType
  }
  deriving (Eq, Show)

data BackendClosureCapture = BackendClosureCapture
  { backendClosureCaptureName :: String,
    backendClosureCaptureType :: BackendType,
    backendClosureCaptureExpr :: BackendExpr
  }
  deriving (Eq, Show)

data BackendTypeBinder = BackendTypeBinder
  { backendTypeBinderName :: String,
    backendTypeBinderBound :: Maybe BackendType
  }
  deriving (Eq, Show)

data BackendBinding = BackendBinding
  { backendBindingName :: String,
    backendBindingType :: BackendType,
    backendBindingExpr :: BackendExpr,
    backendBindingExportedAsMain :: Bool
  }
  deriving (Eq, Show)

-- | Backend type language. This mirrors the checked xMLF type shapes that are
-- meaningful after `.mlfp` checking, but keeps the backend boundary independent
-- from the elaborator's term representation.
data BackendType
  = BTVar String
  | BTArrow BackendType BackendType
  | BTBase BaseTy
  | BTCon BaseTy (NonEmpty BackendType)
  | BTVarApp String (NonEmpty BackendType)
  | BTForall String (Maybe BackendType) BackendType
  | BTMu String BackendType
  | BTBottom
  deriving (Eq, Show)

-- | Typed backend expression. `backendExprType` is the result type of the node.
data BackendExpr
  = BackendVar
      { backendExprType :: BackendType,
        backendVarName :: String
      }
  | BackendLit
      { backendExprType :: BackendType,
        backendLit :: Lit
      }
  | BackendLam
      { backendExprType :: BackendType,
        backendParamName :: String,
        backendParamType :: BackendType,
        backendBody :: BackendExpr
      }
  | BackendApp
      { backendExprType :: BackendType,
        backendFunction :: BackendExpr,
        backendArgument :: BackendExpr
      }
  | BackendLet
      { backendExprType :: BackendType,
        backendLetName :: String,
        backendLetType :: BackendType,
        backendLetRhs :: BackendExpr,
        backendLetBody :: BackendExpr
      }
  | BackendTyAbs
      { backendExprType :: BackendType,
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
  | BackendClosure
      { backendExprType :: BackendType,
        backendClosureEntryName :: String,
        backendClosureCaptures :: [BackendClosureCapture],
        backendClosureParams :: [(String, BackendType)],
        backendClosureBody :: BackendExpr
      }
  | BackendClosureCall
      { backendExprType :: BackendType,
        backendClosureFunction :: BackendExpr,
        backendClosureArguments :: [BackendExpr]
      }
  | BackendConstruct
      { backendExprType :: BackendType,
        backendConstructName :: String,
        backendConstructArgs :: [BackendExpr]
      }
  | BackendCase
      { backendExprType :: BackendType,
        backendScrutinee :: BackendExpr,
        backendAlternatives :: NonEmpty BackendAlternative
      }
  deriving (Eq, Show)

data BackendAlternative = BackendAlternative
  { backendAltPattern :: BackendPattern,
    backendAltBody :: BackendExpr
  }
  deriving (Eq, Show)

data BackendPattern
  = BackendDefaultPattern
  | BackendConstructorPattern String [String]
  deriving (Eq, Show)

literalBackendType :: Lit -> BackendType
literalBackendType = \case
  LInt _ -> BTBase (BaseTy "Int")
  LBool _ -> BTBase (BaseTy "Bool")
  LChar _ -> BTBase (BaseTy "Char")
  LString _ -> BTBase (BaseTy "String")

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

-- | Capture-avoiding substitution for backend types. Forall binders scope over
-- their body but not their optional bound, matching the frontend type syntax.
substituteBackendType :: String -> BackendType -> BackendType -> BackendType
substituteBackendType needle replacement =
  substituteBackendTypes (Map.singleton needle replacement)

substituteBackendTypes :: Map.Map String BackendType -> BackendType -> BackendType
substituteBackendTypes replacements0 =
  go replacements0
  where
    go replacements ty =
      case ty of
        BTVar name ->
          Map.findWithDefault ty name replacements
        BTArrow dom cod -> BTArrow (go replacements dom) (go replacements cod)
        BTBase {} -> ty
        BTCon con args -> BTCon con (fmap (go replacements) args)
        BTVarApp name args ->
          let args' = fmap (go replacements) args
           in case Map.lookup name replacements >>= (`applyBackendTypeHead` NE.toList args') of
                Just ty' -> ty'
                Nothing -> BTVarApp name args'
        BTForall name mbBound body
          | Map.null bodyReplacements ->
              BTForall name (fmap (go replacements) mbBound) body
          | Set.member name freeBodyReplacements ->
              let used =
                    Set.unions
                      [ freeBodyReplacements,
                        freeBackendTypeVars body,
                        maybe Set.empty freeBackendTypeVars mbBound,
                        Map.keysSet bodyReplacements,
                        Set.singleton name
                      ]
                  name' = freshNameLike name used
                  body' = substituteBackendType name (BTVar name') body
               in BTForall name' (fmap (go replacements) mbBound) (go bodyReplacements body')
          | otherwise ->
              BTForall name (fmap (go replacements) mbBound) (go bodyReplacements body)
          where
            bodyReplacements = Map.delete name replacements
            freeBodyReplacements = freeBackendTypeVarsIn bodyReplacements
        BTMu name body
          | Map.null bodyReplacements ->
              ty
          | Set.member name freeBodyReplacements ->
              let used =
                    Set.unions
                      [ freeBodyReplacements,
                        freeBackendTypeVars body,
                        Map.keysSet bodyReplacements,
                        Set.singleton name
                      ]
                  name' = freshNameLike name used
                  body' = substituteBackendType name (BTVar name') body
               in BTMu name' (go bodyReplacements body')
          | otherwise ->
              BTMu name (go bodyReplacements body)
          where
            bodyReplacements = Map.delete name replacements
            freeBodyReplacements = freeBackendTypeVarsIn bodyReplacements
        BTBottom -> BTBottom

applyBackendTypeHead :: BackendType -> [BackendType] -> Maybe BackendType
applyBackendTypeHead headTy args =
  case headTy of
    BTVar name -> Just (mkVarHead name args)
    BTBase name -> Just (mkConHead name args)
    BTCon name existingArgs -> Just (mkConHead name (NE.toList existingArgs ++ args))
    BTVarApp name existingArgs -> Just (mkVarHead name (NE.toList existingArgs ++ args))
    _ -> Nothing
  where
    mkVarHead name = \case
      [] -> BTVar name
      arg : rest -> BTVarApp name (arg :| rest)

    mkConHead name = \case
      [] -> BTBase name
      arg : rest -> BTCon name (arg :| rest)

unfoldBackendRecursiveType :: BackendType -> Maybe BackendType
unfoldBackendRecursiveType ty =
  case ty of
    BTMu name body -> Just (substituteBackendType name ty body)
    _ -> Nothing
