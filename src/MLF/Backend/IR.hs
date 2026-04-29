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
typechecking guard, and before LLVM lowering. The boundary is intentionally
narrow:

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
    BackendTypeBinder (..),
    BackendType (..),
    BackendExpr (..),
    BackendAlternative (..),
    BackendPattern (..),
    BackendValidationError (..),
    alphaEqBackendType,
    literalBackendType,
    substituteBackendType,
    substituteBackendTypes,
    unfoldBackendRecursiveType,
    validateBackendProgram,
    validateBackendBinding,
    validateBackendExpr,
  )
where

import Control.Monad (foldM, unless)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
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
  | BackendTypeAppBoundMismatch BackendType BackendType
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
    bvcConstructors :: Map.Map String BackendConstructorInfo,
    bvcLocals :: Map.Map String BackendType,
    bvcTypeBounds :: Map.Map String (Maybe BackendType)
  }

data BackendConstructorInfo = BackendConstructorInfo
  { bciDataParameters :: [String],
    bciConstructor :: BackendConstructor
  }

type BackendParameterBounds = Map.Map String (Maybe BackendType)

literalBackendType :: Lit -> BackendType
literalBackendType = \case
  LInt _ -> BTBase (BaseTy "Int")
  LBool _ -> BTBase (BaseTy "Bool")
  LString _ -> BTBase (BaseTy "String")

freeBackendTypeVars :: BackendType -> Set.Set String
freeBackendTypeVars =
  go Set.empty
  where
    go bound ty =
      case ty of
        BTVar name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        BTArrow dom cod ->
          Set.union (go bound dom) (go bound cod)
        BTBase {} ->
          Set.empty
        BTCon _ args ->
          foldMap (go bound) args
        BTVarApp name args ->
          let headVars =
                if Set.member name bound
                  then Set.empty
                  else Set.singleton name
           in Set.union headVars (foldMap (go bound) args)
        BTForall name mbBound body ->
          let freeBound = maybe Set.empty (go bound) mbBound
              freeBody = go (Set.insert name bound) body
           in Set.union freeBound freeBody
        BTMu name body ->
          go (Set.insert name bound) body
        BTBottom ->
          Set.empty

freeBackendTypeVarsIn :: Map.Map String BackendType -> Set.Set String
freeBackendTypeVarsIn replacements =
  Set.unions (map freeBackendTypeVars (Map.elems replacements))

alphaEqBackendType :: BackendType -> BackendType -> Bool
alphaEqBackendType =
  go Map.empty Map.empty
  where
    go leftEnv rightEnv leftTy rightTy =
      case (leftTy, rightTy) of
        (BTVar leftName, BTVar rightName) ->
          case (Map.lookup leftName leftEnv, Map.lookup rightName rightEnv) of
            (Just expectedRight, Just expectedLeft) ->
              expectedRight == rightName && expectedLeft == leftName
            (Nothing, Nothing) ->
              leftName == rightName
            _ ->
              False
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          go leftEnv rightEnv leftDom rightDom && go leftEnv rightEnv leftCod rightCod
        (BTBase leftBase, BTBase rightBase) ->
          leftBase == rightBase
        (BTBase leftBase, BTMu rightName rightBody) ->
          structuralMuMatchesData leftBase [] rightName rightBody
        (BTMu leftName leftBody, BTBase rightBase) ->
          structuralMuMatchesData rightBase [] leftName leftBody
        (BTCon leftCon leftArgs, BTCon rightCon rightArgs) ->
          leftCon == rightCon && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTCon leftCon leftArgs, BTMu rightName rightBody) ->
          structuralMuMatchesData leftCon (NE.toList leftArgs) rightName rightBody
        (BTMu leftName leftBody, BTCon rightCon rightArgs) ->
          structuralMuMatchesData rightCon (NE.toList rightArgs) leftName leftBody
        (BTVarApp leftName leftArgs, BTVarApp rightName rightArgs) ->
          typeVarNamesMatch leftEnv rightEnv leftName rightName
            && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTForall leftName leftBound leftBody, BTForall rightName rightBound rightBody) ->
          maybeAlphaEq leftEnv rightEnv leftBound rightBound
            && go
              (Map.insert leftName rightName leftEnv)
              (Map.insert rightName leftName rightEnv)
              leftBody
              rightBody
        (BTMu leftName leftBody, BTMu rightName rightBody) ->
          go
            (Map.insert leftName rightName leftEnv)
            (Map.insert rightName leftName rightEnv)
            leftBody
            rightBody
        (BTBottom, BTBottom) ->
          True
        _ ->
          False

    typeVarNamesMatch leftEnv rightEnv leftName rightName =
      case (Map.lookup leftName leftEnv, Map.lookup rightName rightEnv) of
        (Just expectedRight, Just expectedLeft) ->
          expectedRight == rightName && expectedLeft == leftName
        (Nothing, Nothing) ->
          leftName == rightName
        _ ->
          False

    maybeAlphaEq _ _ Nothing Nothing =
      True
    maybeAlphaEq leftEnv rightEnv (Just leftTy) (Just rightTy) =
      go leftEnv rightEnv leftTy rightTy
    maybeAlphaEq _ _ _ _ =
      False

structuralMuMatchesData :: BaseTy -> [BackendType] -> String -> BackendType -> Bool
structuralMuMatchesData (BaseTy dataName) args muName body =
  structuralMuNameMatches dataName muName
    && case structuralMuPayloadTypes body of
      Just payloadTypes
        | null args -> True
        | null payloadTypes -> all isBareTypeVariable args
        | otherwise -> zipAllWith alphaEqBackendType args payloadTypes
      Nothing -> null args

isBareTypeVariable :: BackendType -> Bool
isBareTypeVariable =
  \case
    BTVar {} -> True
    _ -> False

structuralMuAsDataType :: [String] -> String -> Maybe BackendType
structuralMuAsDataType dataParameterOrder muName = do
  dataName <- structuralMuDataName muName
  let parameterArgs = map BTVar dataParameterOrder
  Just $
    case parameterArgs of
      [] -> BTBase (BaseTy dataName)
      arg : rest -> BTCon (BaseTy dataName) (arg NE.:| rest)

structuralMuAsActualDataType :: String -> BackendType -> Maybe BackendType
structuralMuAsActualDataType muName actual =
  case actual of
    BTBase (BaseTy actualName)
      | structuralMuNameMatches actualName muName -> Just actual
    BTCon (BaseTy actualName) _
      | structuralMuNameMatches actualName muName -> Just actual
    _ -> Nothing

structuralMuNameMatches :: String -> String -> Bool
structuralMuNameMatches dataName muName =
  case structuralMuDataName muName of
    Just structuralName -> dataName == structuralName
    Nothing -> False

structuralMuDataName :: String -> Maybe String
structuralMuDataName name =
  stripSuffixSimple "_self" (dropWhile (== '$') name)

stripSuffixSimple :: String -> String -> Maybe String
stripSuffixSimple suffix value =
  reverse <$> stripPrefixSimple (reverse suffix) (reverse value)

stripPrefixSimple :: String -> String -> Maybe String
stripPrefixSimple [] value =
  Just value
stripPrefixSimple _ [] =
  Nothing
stripPrefixSimple (expected : expectedRest) (actual : actualRest)
  | expected == actual = stripPrefixSimple expectedRest actualRest
  | otherwise = Nothing

structuralMuPayloadTypes :: BackendType -> Maybe [BackendType]
structuralMuPayloadTypes =
  \case
    BTForall resultName _ handlerTy ->
      concat <$> collectHandlers resultName handlerTy
    _ -> Nothing
  where
    collectHandlers resultName =
      go []
      where
        go handlers ty
          | alphaEqBackendType ty (BTVar resultName) = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> do
                  fields <- collectHandlerFields resultName handlerTy
                  go (handlers ++ [fields]) rest
                _ -> Nothing

    collectHandlerFields resultName =
      go []
      where
        go fields ty
          | alphaEqBackendType ty (BTVar resultName) = Just fields
          | otherwise =
              case ty of
                BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
                _ -> Nothing

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
      arg : rest -> BTVarApp name (arg NE.:| rest)

    mkConHead name = \case
      [] -> BTBase name
      arg : rest -> BTCon name (arg NE.:| rest)

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
    constructorInfos =
      [ (backendConstructorName constructor, BackendConstructorInfo (backendDataParameters dataDecl) constructor)
        | dataDecl <- concatMap backendModuleData modules0,
          constructor <- backendDataConstructors dataDecl
      ]
    context0 =
      BackendValidationContext
        { bvcGlobals =
            Map.fromList [(backendBindingName binding, backendBindingType binding) | binding <- bindings]
              `Map.union` backendRuntimePrimitiveTypes,
          bvcConstructors = Map.fromList constructorInfos,
          bvcLocals = Map.empty,
          bvcTypeBounds = Map.empty
        }

backendRuntimePrimitiveTypes :: Map.Map String BackendType
backendRuntimePrimitiveTypes =
  Map.fromList
    [ ( "__mlfp_and",
        BTArrow
          (BTBase (BaseTy "Bool"))
          (BTArrow (BTBase (BaseTy "Bool")) (BTBase (BaseTy "Bool")))
      )
    ]

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
  unless (alphaEqBackendType (backendBindingType binding) (backendExprType expr)) $
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
       in unless (alphaEqBackendType resultTy expected) $
            Left (BackendLiteralTypeMismatch lit expected resultTy)
    BackendLam resultTy paramName paramTy body -> do
      validateBackendExprWith (extendLocalMaybe mbContext paramName paramTy) body
      let expected = BTArrow paramTy (backendExprType body)
      unless (alphaEqBackendType resultTy expected) $
        Left (BackendLambdaTypeMismatch resultTy expected)
    BackendApp resultTy fun arg -> do
      validateBackendExprWith mbContext fun
      validateBackendExprWith mbContext arg
      case backendExprType fun of
        BTArrow expectedArg expectedResult -> do
          unless (alphaEqBackendType (backendExprType arg) expectedArg) $
            Left (BackendApplicationArgumentMismatch expectedArg (backendExprType arg))
          unless (alphaEqBackendType resultTy expectedResult) $
            Left (BackendApplicationResultMismatch resultTy expectedResult)
        other ->
          Left (BackendApplicationExpectedFunction other)
    BackendLet resultTy name bindingTy rhs body -> do
      validateBackendExprWith mbContext rhs
      unless (alphaEqBackendType (backendExprType rhs) bindingTy) $
        Left (BackendLetTypeMismatch name bindingTy (backendExprType rhs))
      validateBackendExprWith (extendLocalMaybe mbContext name bindingTy) body
      unless (alphaEqBackendType (backendExprType body) resultTy) $
        Left (BackendLetBodyTypeMismatch resultTy (backendExprType body))
    BackendTyAbs resultTy name mbBound body -> do
      validateBackendExprWith (extendTypeBoundMaybe mbContext name mbBound) body
      let expected = BTForall name mbBound (backendExprType body)
      unless (alphaEqBackendType resultTy expected) $
        Left (BackendTypeAbsTypeMismatch name resultTy expected)
    BackendTyApp resultTy fun tyArg -> do
      validateBackendExprWith mbContext fun
      case backendExprType fun of
        BTForall name mbBound bodyTy -> do
          validateBackendTypeArgumentBound mbBound tyArg
          let expected = substituteBackendType name tyArg bodyTy
          unless (alphaEqBackendType resultTy expected) $
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
          unless (alphaEqBackendType (backendExprType payload) expectedPayloadTy) $
            Left (BackendRollPayloadMismatch expectedPayloadTy (backendExprType payload))
        Nothing ->
          Left (BackendRollExpectedRecursive resultTy)
    BackendUnroll resultTy payload -> do
      validateBackendExprWith mbContext payload
      case unfoldBackendRecursiveType (backendExprType payload) of
        Just expectedResultTy ->
          unless (alphaEqBackendType resultTy expectedResultTy) $
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
      unless (backendVariableTypeMatches (bvcTypeBounds context0) expectedTy actualTy) $
        Left (BackendVariableTypeMismatch name expectedTy actualTy)

backendVariableTypeMatches :: Map.Map String (Maybe BackendType) -> BackendType -> BackendType -> Bool
backendVariableTypeMatches typeBounds expectedTy actualTy =
  go Set.empty expectedTy actualTy
  where
    go bound expected actual =
      alphaEqBackendType actual expected
        || typeVariableBoundMatches bound expected actual
        || typeVariableBoundMatches bound actual expected
        || case (expected, actual) of
          (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
            go bound expectedDom actualDom && go bound expectedCod actualCod
          (BTBase expectedBase, BTBase actualBase) ->
            expectedBase == actualBase
          (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs) ->
            expectedCon == actualCon
              && zipAllWith (go bound) (NE.toList expectedArgs) (NE.toList actualArgs)
          (BTVarApp expectedName expectedArgs, BTVarApp actualName actualArgs) ->
            expectedName == actualName
              && zipAllWith (go bound) (NE.toList expectedArgs) (NE.toList actualArgs)
          (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) ->
            maybeBoundMatches bound expectedBound actualBound
              && let freshName = freshBinderName expectedName actualName expectedBound actualBound expectedBody actualBody
                     expectedBody' = substituteBackendType expectedName (BTVar freshName) expectedBody
                     actualBody' = substituteBackendType actualName (BTVar freshName) actualBody
                  in go (Set.insert freshName bound) expectedBody' actualBody'
          (BTMu expectedName expectedBody, BTMu actualName actualBody) ->
            let freshName = freshBinderName expectedName actualName Nothing Nothing expectedBody actualBody
                expectedBody' = substituteBackendType expectedName (BTVar freshName) expectedBody
                actualBody' = substituteBackendType actualName (BTVar freshName) actualBody
             in go (Set.insert freshName bound) expectedBody' actualBody'
          (BTBottom, BTBottom) ->
            True
          _ ->
            False

    maybeBoundMatches _ Nothing Nothing =
      True
    maybeBoundMatches bound (Just expectedBound) (Just actualBound) =
      go bound expectedBound actualBound
    maybeBoundMatches _ _ _ =
      False

    typeVariableBoundMatches bound ty otherTy =
      case ty of
        BTVar name
          | Set.notMember name bound ->
              case Map.lookup name typeBounds of
                Just (Just boundTy)
                  | not (alphaEqBackendType boundTy BTBottom) ->
                      go bound boundTy otherTy
                _ ->
                  False
        _ ->
          False

    freshBinderName leftName rightName leftBound rightBound leftBody rightBody =
      freshNameLike
        leftName
        ( Set.unions
            [ Set.fromList [leftName, rightName],
              Map.keysSet typeBounds,
              maybe Set.empty freeBackendTypeVars leftBound,
              maybe Set.empty freeBackendTypeVars rightBound,
              freeBackendTypeVars leftBody,
              freeBackendTypeVars rightBody
            ]
        )

validateBackendTypeArgumentBound :: Maybe BackendType -> BackendType -> Either BackendValidationError ()
validateBackendTypeArgumentBound Nothing _ =
  pure ()
validateBackendTypeArgumentBound (Just BTBottom) _ =
  pure ()
validateBackendTypeArgumentBound (Just boundTy) actualTy =
  unless (alphaEqBackendType actualTy boundTy) $
    Left (BackendTypeAppBoundMismatch boundTy actualTy)

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

extendTypeBoundMaybe :: Maybe BackendValidationContext -> String -> Maybe BackendType -> Maybe BackendValidationContext
extendTypeBoundMaybe mbContext name mbBound =
  fmap (\context0 -> context0 {bvcTypeBounds = Map.insert name mbBound (bvcTypeBounds context0)}) mbContext

extendTypeBounds :: BackendValidationContext -> [(String, Maybe BackendType)] -> BackendValidationContext
extendTypeBounds context0 bounds =
  context0 {bvcTypeBounds = foldr (uncurry Map.insert) (bvcTypeBounds context0) bounds}

validateBackendConstructorUse :: Maybe BackendValidationContext -> String -> BackendType -> [BackendExpr] -> Either BackendValidationError ()
validateBackendConstructorUse Nothing _ _ _ =
  pure ()
validateBackendConstructorUse (Just context0) name resultTy args =
  case Map.lookup name (bvcConstructors context0) of
    Nothing ->
      Left (BackendUnknownConstructor name)
    Just constructorInfo -> do
      let constructor = bciConstructor constructorInfo
          dataParameters = bciDataParameters constructorInfo
          parameters = constructorTypeParameterBounds constructorInfo
          fields = backendConstructorFields constructor
      unless (length fields == length args) $
        Left (BackendConstructorArityMismatch name (length fields) (length args))
      substitution <-
        case matchBackendTypeParametersWithTypeBounds (bvcTypeBounds context0) dataParameters parameters Map.empty (backendConstructorResult constructor) resultTy of
          Just substitution -> pure substitution
          Nothing -> Left (BackendConstructorResultMismatch name (backendConstructorResult constructor) resultTy)
      _ <-
        foldM
          (validateBackendConstructorArgument (bvcTypeBounds context0) dataParameters parameters name)
          substitution
          (zip [0 ..] (zip fields args))
      pure ()

validateBackendConstructorArgument ::
  Map.Map String (Maybe BackendType) ->
  [String] ->
  BackendParameterBounds ->
  String ->
  Map.Map String BackendType ->
  (Int, (BackendType, BackendExpr)) ->
  Either BackendValidationError (Map.Map String BackendType)
validateBackendConstructorArgument typeBounds dataParameters parameters name substitution (index0, (expectedTy, arg)) =
  case matchBackendTypeParametersWithTypeBounds typeBounds dataParameters parameters substitution expectedTy (backendExprType arg) of
    Just substitution' ->
      pure substitution'
    Nothing ->
      Left
        ( BackendConstructorArgumentMismatch
            name
            index0
            (substituteBackendTypes (completeBackendParameterSubstitution parameters substitution) expectedTy)
            (backendExprType arg)
        )

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
    Just constructorInfo -> do
      let constructor = bciConstructor constructorInfo
          dataParameters = bciDataParameters constructorInfo
          parameters = constructorTypeParameterBounds constructorInfo
          fields = backendConstructorFields constructor
      requireUnique BackendDuplicatePatternBinding binders
      unless (length fields == length binders) $
        Left (BackendPatternArityMismatch name (length fields) (length binders))
      substitution <-
        case matchBackendTypeParametersWithTypeBounds (bvcTypeBounds context0) dataParameters parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
          Just substitution -> pure substitution
          Nothing -> Left (BackendCaseConstructorScrutineeMismatch name scrutineeTy (backendConstructorResult constructor))
      let fresheningSubstitution = constructorPatternFresheningSubstitution context0 substitution constructor
          patternSubstitution = Map.union fresheningSubstitution substitution
          instantiatedFields = map (substituteBackendTypes patternSubstitution) fields
          contextForBody =
            extendTypeBounds
              context0
              (constructorPatternTypeBounds substitution fresheningSubstitution constructor)
      pure (Just (extendLocals contextForBody (zip binders instantiatedFields)))

constructorPatternFresheningSubstitution ::
  BackendValidationContext ->
  Map.Map String BackendType ->
  BackendConstructor ->
  Map.Map String BackendType
constructorPatternFresheningSubstitution context0 substitution constructor =
  snd (foldl freshen (reservedNames0, Map.empty) unresolvedNames)
  where
    unresolvedNames =
      [ backendTypeBinderName binder
        | binder <- backendConstructorForalls constructor,
          Map.notMember (backendTypeBinderName binder) substitution
      ]

    externalNames =
      Set.union (Map.keysSet (bvcTypeBounds context0)) (freeBackendTypeVarsIn substitution)

    reservedNames0 =
      Set.union externalNames (Set.fromList unresolvedNames)

    freshen (reservedNames, freshening) name
      | Set.member name externalNames =
          let freshName = freshNameLike name reservedNames
           in (Set.insert freshName reservedNames, Map.insert name (BTVar freshName) freshening)
      | otherwise =
          (Set.insert name reservedNames, freshening)

constructorPatternTypeBounds ::
  Map.Map String BackendType ->
  Map.Map String BackendType ->
  BackendConstructor ->
  [(String, Maybe BackendType)]
constructorPatternTypeBounds substitution fresheningSubstitution constructor =
  [ (freshenedName name, fmap (substituteBackendTypes patternSubstitution) mbBound)
    | binder <- backendConstructorForalls constructor,
      let name = backendTypeBinderName binder,
      let mbBound = backendTypeBinderBound binder,
      Map.notMember name substitution
  ]
  where
    patternSubstitution =
      Map.union fresheningSubstitution substitution

    freshenedName name =
      case Map.lookup name fresheningSubstitution of
        Just (BTVar freshName) -> freshName
        _ -> name

constructorTypeParameterBounds :: BackendConstructorInfo -> BackendParameterBounds
constructorTypeParameterBounds constructorInfo =
  Map.fromList $
    [(name, Nothing) | name <- bciDataParameters constructorInfo]
      ++ [ (backendTypeBinderName binder, backendTypeBinderBound binder)
           | binder <- backendConstructorForalls (bciConstructor constructorInfo)
         ]

validateCaseAlternative :: BackendType -> BackendAlternative -> Either BackendValidationError ()
validateCaseAlternative resultTy alternative =
  unless (alphaEqBackendType (backendExprType (backendAltBody alternative)) resultTy) $
    Left (BackendCaseResultMismatch resultTy (backendExprType (backendAltBody alternative)))

matchBackendTypeParametersWithTypeBounds ::
  Map.Map String (Maybe BackendType) ->
  [String] ->
  BackendParameterBounds ->
  Map.Map String BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map.Map String BackendType)
matchBackendTypeParametersWithTypeBounds typeBounds dataParameterOrder parameterBounds =
  go Set.empty
  where
    go bound substitution expected actual =
      case expected of
        BTVar name
          | Map.member name parameterBounds && Set.notMember name bound ->
              insertParameterSubstitution name actual substitution
        _ ->
          if alphaEqBackendType expected actual
            then Just substitution
            else case (expected, actual) of
              (BTVar {}, _) ->
                requireAlphaEq substitution expected actual
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                go bound substitution expectedDom actualDom
                  >>= \substitution' -> go bound substitution' expectedCod actualCod
              (BTBase expectedBase, BTBase actualBase)
                | expectedBase == actualBase ->
                    Just substitution
              (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs)
                | expectedCon == actualCon ->
                    foldM
                      ( \(substitutionAcc, matched) (expectedArg, actualArg) ->
                          if matched
                            then fmap (\substitutionNext -> (substitutionNext, True)) (go bound substitutionAcc expectedArg actualArg)
                            else Just (substitutionAcc, False)
                      )
                      (substitution, length expectedArgsList == length actualArgsList)
                      (zip expectedArgsList actualArgsList)
                      >>= \(substitution', matched) ->
                        if matched
                          then Just substitution'
                          else Nothing
                where
                  expectedArgsList = NE.toList expectedArgs
                  actualArgsList = NE.toList actualArgs
              (BTMu expectedName expectedBody, actualTy@(BTBase {})) ->
                matchStructuralMuExpected bound substitution expectedName expectedBody actualTy
              (BTMu expectedName expectedBody, actualTy@(BTCon {})) ->
                matchStructuralMuExpected bound substitution expectedName expectedBody actualTy
              (expectedTy@(BTBase {}), BTMu actualName actualBody) ->
                matchStructuralMuActual bound substitution expectedTy actualName actualBody
              (expectedTy@(BTCon {}), BTMu actualName actualBody) ->
                matchStructuralMuActual bound substitution expectedTy actualName actualBody
              (BTVarApp expectedName expectedArgs, _) ->
                matchBackendTypeApplication bound substitution expectedName (NE.toList expectedArgs) actual
              (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) -> do
                substitution' <- matchMaybeBound bound substitution expectedBound actualBound
                let used =
                      Set.unions
                        [ Set.fromList [expectedName, actualName],
                          Map.keysSet substitution',
                          freeBackendTypeVarsIn substitution',
                          Map.keysSet parameterBounds,
                          freeBackendTypeVars expectedBody,
                          freeBackendTypeVars actualBody,
                          maybe Set.empty freeBackendTypeVars expectedBound,
                          maybe Set.empty freeBackendTypeVars actualBound
                        ]
                    freshName = freshNameLike expectedName used
                    expectedBody' = substituteBackendType expectedName (BTVar freshName) expectedBody
                    actualBody' = substituteBackendType actualName (BTVar freshName) actualBody
                go (Set.insert freshName bound) substitution' expectedBody' actualBody'
              (BTMu expectedName expectedBody, BTMu actualName actualBody) -> do
                let used =
                      Set.unions
                        [ Set.fromList [expectedName, actualName],
                          Map.keysSet substitution,
                          freeBackendTypeVarsIn substitution,
                          Map.keysSet parameterBounds,
                          freeBackendTypeVars expectedBody,
                          freeBackendTypeVars actualBody
                        ]
                    freshName = freshNameLike expectedName used
                    expectedBody' = substituteBackendType expectedName (BTVar freshName) expectedBody
                    actualBody' = substituteBackendType actualName (BTVar freshName) actualBody
                go (Set.insert freshName bound) substitution expectedBody' actualBody'
              (BTBottom, BTBottom) ->
                Just substitution
              _ ->
                Nothing

    matchMaybeBound _ substitution Nothing Nothing =
      Just substitution
    matchMaybeBound bound substitution (Just expectedBound) (Just actualBound) =
      go bound substitution expectedBound actualBound
    matchMaybeBound _ _ _ _ =
      Nothing

    matchStructuralMuExpected bound substitution muName _body actualTy =
      firstJust
        [ structuralMuAsDataType dataParameterOrder muName
            >>= \expectedTy -> go bound substitution expectedTy actualTy,
          structuralMuAsActualDataType muName actualTy
            >>= \expectedTy -> go bound substitution expectedTy actualTy
        ]

    matchStructuralMuActual bound substitution expectedTy muName _body =
      firstJust
        [ structuralMuAsDataType dataParameterOrder muName
            >>= \actualTy -> go bound substitution expectedTy actualTy,
          structuralMuAsActualDataType muName expectedTy
            >>= \actualTy -> go bound substitution expectedTy actualTy
        ]

    matchBackendTypeApplication bound substitution name expectedArgs actual =
      case decomposeBackendTypeHead actual of
        Just (actualHead, actualArgs)
          | length expectedArgs == length actualArgs -> do
              substitution' <-
                if Map.member name parameterBounds && Set.notMember name bound
                  then insertParameterSubstitution name actualHead substitution
                  else go bound substitution (BTVar name) actualHead
              foldM
                (\substitutionAcc (expectedArg, actualArg) -> go bound substitutionAcc expectedArg actualArg)
                substitution'
                (zip expectedArgs actualArgs)
        _ -> Nothing

    requireAlphaEq substitution expected actual
      | alphaEqBackendType expected actual = Just substitution
      | otherwise = Nothing

    firstJust =
      \case
        [] -> Nothing
        candidate : rest ->
          case candidate of
            Just value -> Just value
            Nothing -> firstJust rest

    insertParameterSubstitution name actual substitution =
      case Map.lookup name substitution of
        Nothing ->
          if backendParameterBoundMatches name actual substitution
            then Just (Map.insert name actual substitution)
            else Nothing
        Just previous
          | alphaEqBackendType previous actual && backendParameterBoundMatches name previous substitution ->
              Just substitution
        _ ->
          Nothing

    backendParameterBoundMatches name actual substitution =
      case Map.lookup name parameterBounds of
        Just (Just boundTy)
          | not (alphaEqBackendType boundTy BTBottom) ->
              let dependencySubstitution =
                    completeBackendParameterSubstitution
                      (Map.delete name parameterBounds)
                      (Map.delete name substitution)
                  expectedBound = substituteBackendTypes dependencySubstitution boundTy
               in typeBoundDependenciesMatch actual expectedBound || actualTypeVariableBoundMatches actual expectedBound
        _ ->
          True

    typeBoundDependenciesMatch actual expectedBound =
      alphaEqBackendType
        (resolveTypeBoundDependencies actual)
        (resolveTypeBoundDependencies expectedBound)

    actualTypeVariableBoundMatches actual expectedBound =
      case actual of
        BTVar actualName ->
          case Map.lookup actualName typeBounds of
            Just (Just actualBound) ->
              typeBoundDependenciesMatch actualBound expectedBound
            _ ->
              False
        _ ->
          False

    resolveTypeBoundDependencies =
      substituteBackendTypes resolvedTypeBounds

    resolvedTypeBounds =
      completeBackendParameterSubstitution typeBounds Map.empty

decomposeBackendTypeHead :: BackendType -> Maybe (BackendType, [BackendType])
decomposeBackendTypeHead ty =
  case ty of
    BTVar name -> Just (BTVar name, [])
    BTBase name -> Just (BTBase name, [])
    BTCon name args -> Just (BTBase name, NE.toList args)
    BTVarApp name args -> Just (BTVar name, NE.toList args)
    _ -> Nothing

completeBackendParameterSubstitution :: BackendParameterBounds -> Map.Map String BackendType -> Map.Map String BackendType
completeBackendParameterSubstitution parameterBounds substitution0 =
  resolveDefaultedBounds defaultedNames substitution1
  where
    substitution1 =
      foldl insertBoundDefault substitution0 (Map.toList parameterBounds)

    defaultedNames =
      Set.fromList
        [ name
          | (name, Just boundTy) <- Map.toList parameterBounds,
            Map.notMember name substitution0,
            not (alphaEqBackendType boundTy BTBottom)
        ]

    insertBoundDefault substitution (name, Just boundTy)
      | Map.member name substitution = substitution
      | alphaEqBackendType boundTy BTBottom = substitution
      | otherwise = Map.insert name (substituteBackendTypes substitution boundTy) substitution
    insertBoundDefault substitution _ =
      substitution

    resolveDefaultedBounds names =
      go (Set.size names + Map.size parameterBounds + 1)
      where
        go remaining substitution
          | remaining <= 0 = substitution
          | substitution' == substitution = substitution
          | otherwise = go (remaining - 1) substitution'
          where
            substitution' =
              foldl resolveDefaultedBound substitution (Set.toList names)

    resolveDefaultedBound substitution name =
      case Map.lookup name substitution of
        Just ty ->
          Map.insert name (substituteBackendTypes (Map.delete name substitution) ty) substitution
        Nothing ->
          substitution

requireUnique :: (String -> BackendValidationError) -> [String] -> Either BackendValidationError ()
requireUnique mkError names =
  case duplicates names of
    name : _ -> Left (mkError name)
    [] -> Right ()

zipAllWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
zipAllWith _ [] [] =
  True
zipAllWith f (left : leftRest) (right : rightRest) =
  f left right && zipAllWith f leftRest rightRest
zipAllWith _ _ _ =
  False

duplicates :: [String] -> [String]
duplicates =
  go . sort
  where
    go [] = []
    go [_] = []
    go (x : y : rest)
      | x == y = x : go (dropWhile (== x) rest)
      | otherwise = go (y : rest)
