{-# LANGUAGE LambdaCase #-}

{- |
Module      : MLF.Backend.IR
Description : Typed backend IR boundary for checked .mlfp programs

This module defines the first backend-owned representation after the current
`.mlfp` checker/eMLF/xMLF path has accepted a program. It is deliberately not a
new inference or typing authority: values entering this module must already
come from a checked program, and 'validateBackendProgram' only checks the local
IR invariants that a converter/lowerer should preserve.

{- Note [Typed backend IR boundary]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The backend IR sits after `MLF.Frontend.Program.Check` and the xMLF
typechecking guard, and before any textual or LLVM-like lowering. The boundary
is intentionally narrow:

* every expression node carries its result type;
* module-level binding names are runtime names and must be globally unique;
* a program `main` names one of those checked bindings;
* variable references resolve through lexical binders or the global runtime
  binding table, with the carried type matching the binding;
* application/lambda/let/type-application/recursive fold-unfold nodes satisfy
  local type equalities;
* ADT construction and case analysis are explicit backend nodes, so a backend
  lowerer does not have to inspect source syntax or Church-encoded runtime
  terms to find the intended control/data boundary; constructor uses and case
  alternatives are checked against backend constructor metadata.

The IR may still carry explicit type abstraction/application and recursive
roll/unroll nodes. Lowering passes are expected to reject unsupported backend
features at their own boundary rather than weakening this checked IR contract.
-}
-}
module MLF.Backend.IR
  ( BackendProgram (..),
    BackendModule (..),
    BackendBinding (..),
    BackendData (..),
    BackendConstructor (..),
    BackendType (..),
    BackendExpr (..),
    BackendAlternative (..),
    BackendPattern (..),
    BackendValidationError (..),
    literalBackendType,
    substituteBackendType,
    unfoldBackendRecursiveType,
    validateBackendProgram,
    validateBackendBinding,
    validateBackendExpr,
  )
where

import Control.Monad (unless, zipWithM_)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Syntax (Lit (..))

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
    backendConstructorFields :: [BackendType],
    backendConstructorResult :: BackendType
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
  | BackendRoll
      { backendExprType :: BackendType,
        backendRollPayload :: BackendExpr
      }
  | BackendUnroll
      { backendExprType :: BackendType,
        backendUnrollPayload :: BackendExpr
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

data BackendValidationError
  = BackendDuplicateModule String
  | BackendDuplicateBinding String
  | BackendDuplicateConstructor String
  | BackendMainNotFound String
  | BackendUnknownVariable String
  | BackendVariableTypeMismatch String BackendType BackendType
  | BackendBindingTypeMismatch String BackendType BackendType
  | BackendLiteralTypeMismatch Lit BackendType BackendType
  | BackendLambdaTypeMismatch BackendType BackendType
  | BackendApplicationExpectedFunction BackendType
  | BackendApplicationArgumentMismatch BackendType BackendType
  | BackendApplicationResultMismatch BackendType BackendType
  | BackendLetTypeMismatch String BackendType BackendType
  | BackendLetBodyTypeMismatch BackendType BackendType
  | BackendTypeAbsTypeMismatch String BackendType BackendType
  | BackendTypeAppExpectedForall BackendType
  | BackendTypeAppResultMismatch BackendType BackendType
  | BackendRollExpectedRecursive BackendType
  | BackendRollPayloadMismatch BackendType BackendType
  | BackendUnrollExpectedRecursive BackendType
  | BackendUnrollResultMismatch BackendType BackendType
  | BackendUnknownConstructor String
  | BackendConstructorArityMismatch String Int Int
  | BackendConstructorArgumentMismatch String Int BackendType BackendType
  | BackendConstructorResultMismatch String BackendType BackendType
  | BackendPatternArityMismatch String Int Int
  | BackendDuplicatePatternBinding String
  | BackendCaseConstructorScrutineeMismatch String BackendType BackendType
  | BackendCaseResultMismatch BackendType BackendType
  deriving (Eq, Show)

data BackendValidationContext = BackendValidationContext
  { bvcGlobals :: Map.Map String BackendType,
    bvcConstructors :: Map.Map String BackendConstructor,
    bvcLocals :: Map.Map String BackendType
  }

literalBackendType :: Lit -> BackendType
literalBackendType = \case
  LInt _ -> BTBase (BaseTy "Int")
  LBool _ -> BTBase (BaseTy "Bool")
  LString _ -> BTBase (BaseTy "String")

substituteBackendType :: String -> BackendType -> BackendType -> BackendType
substituteBackendType needle replacement =
  go
  where
    go ty =
      case ty of
        BTVar name
          | name == needle -> replacement
          | otherwise -> ty
        BTArrow dom cod -> BTArrow (go dom) (go cod)
        BTBase {} -> ty
        BTCon con args -> BTCon con (fmap go args)
        BTForall name mbBound body
          | name == needle -> BTForall name mbBound body
          | otherwise -> BTForall name (fmap go mbBound) (go body)
        BTMu name body
          | name == needle -> ty
          | otherwise -> BTMu name (go body)
        BTBottom -> BTBottom

unfoldBackendRecursiveType :: BackendType -> Maybe BackendType
unfoldBackendRecursiveType ty =
  case ty of
    BTMu name body -> Just (substituteBackendType name ty body)
    _ -> Nothing

validateBackendProgram :: BackendProgram -> Either BackendValidationError ()
validateBackendProgram program = do
  requireUnique BackendDuplicateModule (map backendModuleName modules0)
  requireUnique BackendDuplicateBinding (map backendBindingName bindings)
  requireUnique BackendDuplicateConstructor (map backendConstructorName constructors)
  unless (backendProgramMain program `elem` map backendBindingName bindings) $
    Left (BackendMainNotFound (backendProgramMain program))
  mapM_ (validateBackendBindingInContext context0) bindings
  where
    modules0 = backendProgramModules program
    bindings = concatMap backendModuleBindings modules0
    constructors = concatMap backendDataConstructors (concatMap backendModuleData modules0)
    context0 =
      BackendValidationContext
        { bvcGlobals = Map.fromList [(backendBindingName binding, backendBindingType binding) | binding <- bindings],
          bvcConstructors = Map.fromList [(backendConstructorName constructor, constructor) | constructor <- constructors],
          bvcLocals = Map.empty
        }

-- | Validate a binding without a program context. This checks local carried
-- type equalities only; 'validateBackendProgram' adds global references,
-- constructor metadata, and lexical scope checks.
validateBackendBinding :: BackendBinding -> Either BackendValidationError ()
validateBackendBinding =
  validateBackendBindingWith Nothing

validateBackendBindingInContext :: BackendValidationContext -> BackendBinding -> Either BackendValidationError ()
validateBackendBindingInContext context0 =
  validateBackendBindingWith (Just context0)

validateBackendBindingWith :: Maybe BackendValidationContext -> BackendBinding -> Either BackendValidationError ()
validateBackendBindingWith mbContext binding = do
  validateBackendExprWith mbContext expr
  unless (backendBindingType binding == backendExprType expr) $
    Left (BackendBindingTypeMismatch (backendBindingName binding) (backendBindingType binding) (backendExprType expr))
  where
    expr = backendBindingExpr binding

-- | Validate an expression without a program context. This checks local carried
-- type equalities only; 'validateBackendProgram' validates global and lexical
-- references against the surrounding backend program.
validateBackendExpr :: BackendExpr -> Either BackendValidationError ()
validateBackendExpr =
  validateBackendExprWith Nothing

validateBackendExprWith :: Maybe BackendValidationContext -> BackendExpr -> Either BackendValidationError ()
validateBackendExprWith mbContext expr =
  case expr of
    BackendVar resultTy name ->
      validateBackendVariable mbContext name resultTy
    BackendLit resultTy lit ->
      let expected = literalBackendType lit
       in unless (resultTy == expected) $
            Left (BackendLiteralTypeMismatch lit expected resultTy)
    BackendLam resultTy paramName paramTy body -> do
      validateBackendExprWith (extendLocalMaybe mbContext paramName paramTy) body
      let expected = BTArrow paramTy (backendExprType body)
      unless (resultTy == expected) $
        Left (BackendLambdaTypeMismatch resultTy expected)
    BackendApp resultTy fun arg -> do
      validateBackendExprWith mbContext fun
      validateBackendExprWith mbContext arg
      case backendExprType fun of
        BTArrow expectedArg expectedResult -> do
          unless (backendExprType arg == expectedArg) $
            Left (BackendApplicationArgumentMismatch expectedArg (backendExprType arg))
          unless (resultTy == expectedResult) $
            Left (BackendApplicationResultMismatch resultTy expectedResult)
        other ->
          Left (BackendApplicationExpectedFunction other)
    BackendLet resultTy name bindingTy rhs body -> do
      validateBackendExprWith mbContext rhs
      unless (backendExprType rhs == bindingTy) $
        Left (BackendLetTypeMismatch name bindingTy (backendExprType rhs))
      validateBackendExprWith (extendLocalMaybe mbContext name bindingTy) body
      unless (backendExprType body == resultTy) $
        Left (BackendLetBodyTypeMismatch resultTy (backendExprType body))
    BackendTyAbs resultTy name mbBound body -> do
      validateBackendExprWith mbContext body
      let expected = BTForall name mbBound (backendExprType body)
      unless (resultTy == expected) $
        Left (BackendTypeAbsTypeMismatch name resultTy expected)
    BackendTyApp resultTy fun tyArg -> do
      validateBackendExprWith mbContext fun
      case backendExprType fun of
        BTForall name _ bodyTy -> do
          let expected = substituteBackendType name tyArg bodyTy
          unless (resultTy == expected) $
            Left (BackendTypeAppResultMismatch resultTy expected)
        other ->
          Left (BackendTypeAppExpectedForall other)
    BackendConstruct resultTy name args -> do
      validateBackendConstructorUse mbContext name resultTy args
      mapM_ (validateBackendExprWith mbContext) args
    BackendCase resultTy scrutinee alternatives -> do
      validateBackendExprWith mbContext scrutinee
      mapM_ (validateBackendAlternative mbContext (backendExprType scrutinee) resultTy) (NE.toList alternatives)
    BackendRoll resultTy payload -> do
      validateBackendExprWith mbContext payload
      case unfoldBackendRecursiveType resultTy of
        Just expectedPayloadTy ->
          unless (backendExprType payload == expectedPayloadTy) $
            Left (BackendRollPayloadMismatch expectedPayloadTy (backendExprType payload))
        Nothing ->
          Left (BackendRollExpectedRecursive resultTy)
    BackendUnroll resultTy payload -> do
      validateBackendExprWith mbContext payload
      case unfoldBackendRecursiveType (backendExprType payload) of
        Just expectedResultTy ->
          unless (resultTy == expectedResultTy) $
            Left (BackendUnrollResultMismatch resultTy expectedResultTy)
        Nothing ->
          Left (BackendUnrollExpectedRecursive (backendExprType payload))

validateBackendVariable :: Maybe BackendValidationContext -> String -> BackendType -> Either BackendValidationError ()
validateBackendVariable Nothing _ _ =
  pure ()
validateBackendVariable (Just context0) name actualTy =
  case lookupBackendVariable context0 name of
    Nothing ->
      Left (BackendUnknownVariable name)
    Just expectedTy ->
      unless (actualTy == expectedTy) $
        Left (BackendVariableTypeMismatch name expectedTy actualTy)

lookupBackendVariable :: BackendValidationContext -> String -> Maybe BackendType
lookupBackendVariable context0 name =
  case Map.lookup name (bvcLocals context0) of
    Just localTy -> Just localTy
    Nothing -> Map.lookup name (bvcGlobals context0)

extendLocalMaybe :: Maybe BackendValidationContext -> String -> BackendType -> Maybe BackendValidationContext
extendLocalMaybe mbContext name ty =
  fmap (\context0 -> extendLocal context0 name ty) mbContext

extendLocal :: BackendValidationContext -> String -> BackendType -> BackendValidationContext
extendLocal context0 name ty =
  context0 {bvcLocals = Map.insert name ty (bvcLocals context0)}

extendLocals :: BackendValidationContext -> [(String, BackendType)] -> BackendValidationContext
extendLocals context0 bindings =
  context0 {bvcLocals = foldr (uncurry Map.insert) (bvcLocals context0) bindings}

validateBackendConstructorUse :: Maybe BackendValidationContext -> String -> BackendType -> [BackendExpr] -> Either BackendValidationError ()
validateBackendConstructorUse Nothing _ _ _ =
  pure ()
validateBackendConstructorUse (Just context0) name resultTy args =
  case Map.lookup name (bvcConstructors context0) of
    Nothing ->
      Left (BackendUnknownConstructor name)
    Just constructor -> do
      let fields = backendConstructorFields constructor
      unless (length fields == length args) $
        Left (BackendConstructorArityMismatch name (length fields) (length args))
      unless (backendConstructorResult constructor == resultTy) $
        Left (BackendConstructorResultMismatch name (backendConstructorResult constructor) resultTy)
      zipWithM_ (validateBackendConstructorArgument name) [0 ..] (zip fields args)

validateBackendConstructorArgument :: String -> Int -> (BackendType, BackendExpr) -> Either BackendValidationError ()
validateBackendConstructorArgument name index0 (expectedTy, arg) =
  unless (backendExprType arg == expectedTy) $
    Left (BackendConstructorArgumentMismatch name index0 expectedTy (backendExprType arg))

validateBackendAlternative :: Maybe BackendValidationContext -> BackendType -> BackendType -> BackendAlternative -> Either BackendValidationError ()
validateBackendAlternative mbContext scrutineeTy resultTy alternative = do
  contextForBody <- validateBackendPattern mbContext scrutineeTy (backendAltPattern alternative)
  validateBackendExprWith contextForBody (backendAltBody alternative)
  validateCaseAlternative resultTy alternative

validateBackendPattern :: Maybe BackendValidationContext -> BackendType -> BackendPattern -> Either BackendValidationError (Maybe BackendValidationContext)
validateBackendPattern Nothing _ _ =
  pure Nothing
validateBackendPattern (Just context0) _ BackendDefaultPattern =
  pure (Just context0)
validateBackendPattern (Just context0) scrutineeTy (BackendConstructorPattern name binders) =
  case Map.lookup name (bvcConstructors context0) of
    Nothing ->
      Left (BackendUnknownConstructor name)
    Just constructor -> do
      let fields = backendConstructorFields constructor
      requireUnique BackendDuplicatePatternBinding binders
      unless (length fields == length binders) $
        Left (BackendPatternArityMismatch name (length fields) (length binders))
      unless (backendConstructorResult constructor == scrutineeTy) $
        Left (BackendCaseConstructorScrutineeMismatch name scrutineeTy (backendConstructorResult constructor))
      pure (Just (extendLocals context0 (zip binders fields)))

validateCaseAlternative :: BackendType -> BackendAlternative -> Either BackendValidationError ()
validateCaseAlternative resultTy alternative =
  unless (backendExprType (backendAltBody alternative) == resultTy) $
    Left (BackendCaseResultMismatch resultTy (backendExprType (backendAltBody alternative)))

requireUnique :: (String -> BackendValidationError) -> [String] -> Either BackendValidationError ()
requireUnique mkError names =
  case duplicates names of
    name : _ -> Left (mkError name)
    [] -> Right ()

duplicates :: [String] -> [String]
duplicates =
  go . sort
  where
    go [] = []
    go [_] = []
    go (x : y : rest)
      | x == y = x : go (dropWhile (== x) rest)
      | otherwise = go (y : rest)
