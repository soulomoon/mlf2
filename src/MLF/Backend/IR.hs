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

* xMLF remains the thesis-faithful typed elaboration IR;
* `MLF.Backend.IR` is the single executable eager backend IR;
* `MLF.Backend.IR` owns the eager executable representation consumed by the
  rest of the backend: typed direct application, explicit closures and
  `BackendClosureCall`, ADT construction and case analysis, lets, lambdas,
  type abstraction/application, and roll/unroll;
* validation-visible invariants for those executable shapes live at this
  boundary;
* closure-record layout, native process entrypoints, renderer helpers, native
  wrapper/runtime symbol emission, and other lowering-only runtime details
  stay downstream of this IR;
* no thunks, no update frames, no CAF update semantics, no graph reduction,
  and no implicit laziness rescue;
* no second executable backend IR, no public `LowerableBackend.IR`, and no
  second checked-program authority inside this family;
* any ANF-like normalization, layout-only structure, or lowerability-only
  representation stays private to backend-owned lowering helpers.

A later lower IR may be introduced only when all of the following hold:

* distinct backend-owned executable invariants that cannot live in
  `MLF.Backend.IR` or a private lowering helper;
* a dedicated validation/evidence owner for that new boundary; and
* a later accepted roadmap revision before any new durable or public surface
  is added.

* every expression node carries its result type;
* module-level binding names are runtime names and must be globally unique;
* a program `main` names one of those checked bindings;
* variable references resolve through lexical binders or the global runtime
  binding table, with the carried type matching the binding;
* `BackendApp` is the direct first-order call node, so local direct aliases
  that remain first-order stay on this path and closure-valued heads violate a
  named backend callable invariant;
* application/lambda/let/type-application/recursive fold-unfold nodes satisfy
  local type equalities;
* ADT construction and case analysis are explicit backend nodes, so a backend
  lowerer does not have to inspect source syntax or Church-encoded runtime
  terms to find the intended control/data boundary; constructor uses and case
  alternatives are checked against backend constructor metadata.
* `BackendClosureCall` is the indirect closure-call node, so closure-valued
  aliases, captured closures, constructor-field projections, and case/let-
  selected closure values stay on this explicit path, and confused direct-call
  heads are rejected with explicit callable diagnostics.

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
    BackendClosureCapture (..),
    BackendTypeBinder (..),
    BackendType (..),
    BackendExpr (..),
    BackendAlternative (..),
    BackendPattern (..),
    BackendCallableBindingKind (..),
    BackendCallableHead (..),
    BackendValidationError (..),
    alphaEqBackendType,
    backendCallableHead,
    literalBackendType,
    substituteBackendType,
    substituteBackendTypes,
    unfoldBackendRecursiveType,
    validateBackendProgram,
    validateBackendBinding,
    validateBackendExpr,
  )
where

import Control.Monad (foldM, unless, zipWithM_)
import Data.Char (isDigit)
import Data.List (sort)
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
  | BackendClosureCalledWithBackendApp String
  | BackendDirectCalledWithBackendClosureCall String
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
  | BackendDuplicateClosureEntry String
  | BackendClosureEntryNameCollision String
  | BackendDuplicateClosureCapture String
  | BackendDuplicateClosureParameter String
  | BackendClosureCaptureTypeMismatch String BackendType BackendType
  | BackendClosureExpectedFunction String BackendType
  | BackendClosureParameterArityMismatch String Int Int
  | BackendClosureTypeMismatch String BackendType BackendType
  | BackendClosureCallExpectedFunction BackendType
  | BackendClosureCallExpectedClosureValue BackendType
  | BackendClosureCallArityMismatch Int Int
  | BackendClosureCallArgumentMismatch Int BackendType BackendType
  | BackendClosureCallResultMismatch BackendType BackendType
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
    bvcData :: Map.Map String BackendData,
    bvcConstructors :: Map.Map String BackendConstructorInfo,
    bvcLocals :: Map.Map String BackendType,
    bvcClosureGlobals :: Set.Set String,
    bvcClosureLocals :: Set.Set String,
    bvcPossibleClosureLocals :: Set.Set String,
    bvcTypeBounds :: Map.Map String (Maybe BackendType)
  }

data BackendConstructorInfo = BackendConstructorInfo
  { bciDataName :: String,
    bciDataParameters :: [String],
    bciDataConstructors :: [BackendConstructor],
    bciConstructor :: BackendConstructor
  }

data TypeVariableInstantiation
  = RejectFreeTypeVariableInstantiation FreshenedTypeVariableAliases
  | AllowStructuralPayloadInstantiation
  deriving (Eq, Show)

data BackendCallableBindingKind
  = BackendCallableBindingDirect
  | BackendCallableBindingClosure
  | BackendCallableBindingUnknown
  deriving (Eq, Show)

data BackendCallableHead
  = BackendDirectCallableHead (Maybe String)
  | BackendClosureCallableHead String
  | BackendUnknownCallableHead
  deriving (Eq, Show)

data FreshenedTypeVariableAliases
  = RejectFreshenedTypeVariableAliases
  | AllowFreshenedTypeVariableAliases
  deriving (Eq, Show)

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
        | null args -> null payloadTypes
        | null payloadTypes -> all isBareTypeVariable args
        | otherwise -> zipAllWith alphaEqBackendType args payloadTypes
      Nothing -> False

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
    _ -> Nothing

structuralMuNameMatches :: String -> String -> Bool
structuralMuNameMatches dataName muName =
  case structuralMuDataName muName of
    Just structuralName ->
      dataName == structuralName
        || dataName == dropModulePrefix structuralName
    Nothing -> False
  where
    dropModulePrefix name =
      case break (== '.') name of
        (_, '.' : rest) -> dropModulePrefix rest
        _ -> name

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
structuralMuPayloadTypes body =
  concat <$> structuralMuPayloadHandlers body

structuralMuHandlerTypes :: BackendType -> Maybe (String, [BackendType])
structuralMuHandlerTypes =
  \case
    BTForall resultName _ handlerTy -> do
      handlers <- collectHandlerTypes resultName handlerTy
      Just (resultName, handlers)
    _ -> Nothing
  where
    collectHandlerTypes resultName =
      go []
      where
        go handlers ty
          | alphaEqBackendType ty (BTVar resultName) = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> go (handlers ++ [handlerTy]) rest
                _ -> Nothing

structuralMuPayloadHandlers :: BackendType -> Maybe [[BackendType]]
structuralMuPayloadHandlers body = do
  (resultName, handlers) <- structuralMuHandlerTypes body
  traverse (collectHandlerFields resultName) handlers
  where
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

backendClosureEntryNames :: BackendExpr -> [String]
backendClosureEntryNames =
  \case
    BackendVar {} -> []
    BackendLit {} -> []
    BackendLam _ _ _ body -> backendClosureEntryNames body
    BackendApp _ fun arg -> backendClosureEntryNames fun ++ backendClosureEntryNames arg
    BackendLet _ _ _ rhs body -> backendClosureEntryNames rhs ++ backendClosureEntryNames body
    BackendTyAbs _ _ _ body -> backendClosureEntryNames body
    BackendTyApp _ fun _ -> backendClosureEntryNames fun
    BackendConstruct _ _ args -> concatMap backendClosureEntryNames args
    BackendCase _ scrutinee alternatives ->
      backendClosureEntryNames scrutinee ++ concatMap (backendClosureEntryNames . backendAltBody) (NE.toList alternatives)
    BackendRoll _ payload -> backendClosureEntryNames payload
    BackendUnroll _ payload -> backendClosureEntryNames payload
    BackendClosure _ entryName captures _ body ->
      entryName
        : concatMap (backendClosureEntryNames . backendClosureCaptureExpr) captures
          ++ backendClosureEntryNames body
    BackendClosureCall _ fun args ->
      backendClosureEntryNames fun ++ concatMap backendClosureEntryNames args

validateBackendProgram :: BackendProgram -> Either BackendValidationError ()
validateBackendProgram program = do
  requireUnique BackendDuplicateModule (map backendModuleName modules0)
  requireUnique BackendDuplicateBinding (map backendBindingName bindings)
  requireUnique BackendDuplicateConstructor (map backendConstructorName constructors)
  requireUnique BackendDuplicateClosureEntry closureEntryNames
  rejectClosureEntryNameCollisions closureEntryNames (map backendBindingName bindings ++ Map.keys backendRuntimePrimitiveTypes)
  unless (backendProgramMain program `elem` map backendBindingName bindings) $
    Left (BackendMainNotFound (backendProgramMain program))
  mapM_ (validateBackendBindingInContext context0) bindings
  where
    modules0 = backendProgramModules program
    bindings = concatMap backendModuleBindings modules0
    constructors = concatMap backendDataConstructors (concatMap backendModuleData modules0)
    closureEntryNames = concatMap (backendClosureEntryNames . backendBindingExpr) bindings
    constructorInfos =
      [ ( backendConstructorName constructor,
          BackendConstructorInfo
            (backendDataName dataDecl)
            (backendDataParameters dataDecl)
            (backendDataConstructors dataDecl)
            constructor
        )
        | dataDecl <- concatMap backendModuleData modules0,
          constructor <- backendDataConstructors dataDecl
      ]
    baseContext =
      BackendValidationContext
        { bvcGlobals =
            Map.fromList [(backendBindingName binding, backendBindingType binding) | binding <- bindings]
              `Map.union` backendRuntimePrimitiveTypes,
          bvcData = Map.fromList [(backendDataName dataDecl, dataDecl) | dataDecl <- concatMap backendModuleData modules0],
          bvcConstructors = Map.fromList constructorInfos,
          bvcLocals = Map.empty,
          bvcClosureGlobals = Set.empty,
          bvcClosureLocals = Set.empty,
          bvcPossibleClosureLocals = Set.empty,
          bvcTypeBounds = Map.empty
        }
    closureGlobals = backendClosureGlobalNames baseContext bindings
    context0 =
      baseContext {bvcClosureGlobals = closureGlobals}

backendClosureGlobalNames :: BackendValidationContext -> [BackendBinding] -> Set.Set String
backendClosureGlobalNames baseContext bindings =
  go Set.empty
  where
    go globals =
      let globals' =
            Set.fromList
              [ backendBindingName binding
                | binding <- bindings,
                  BackendClosureCallableHead _ <- [backendCallableHeadInContext (Just (baseContext {bvcClosureGlobals = globals})) (backendBindingExpr binding)]
              ]
       in if globals' == globals
            then globals
            else go globals'

backendRuntimePrimitiveTypes :: Map.Map String BackendType
backendRuntimePrimitiveTypes =
  Map.fromList
    [ ( "__mlfp_and",
        BTArrow
          (BTBase (BaseTy "Bool"))
          (BTArrow (BTBase (BaseTy "Bool")) (BTBase (BaseTy "Bool")))
      ),
      ( "__io_pure",
        BTForall "a" Nothing
          (BTArrow (BTVar "a") (BTCon (BaseTy "IO") (BTVar "a" :| [])))
      ),
      ( "__io_bind",
        BTForall "a" Nothing
          (BTForall "b" Nothing
            (BTArrow
              (BTCon (BaseTy "IO") (BTVar "a" :| []))
              (BTArrow
                (BTArrow (BTVar "a") (BTCon (BaseTy "IO") (BTVar "b" :| [])))
                (BTCon (BaseTy "IO") (BTVar "b" :| [])))))
      ),
      ( "__io_putStrLn",
        BTArrow
          (BTBase (BaseTy "String"))
          (BTCon (BaseTy "IO") (BTBase (BaseTy "Unit") :| []))
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
      validateBackendExprWith (extendFunctionParamLocalMaybe mbContext paramName paramTy body) body
      let expected = BTArrow paramTy (backendExprType body)
      unless (alphaEqBackendType resultTy expected) $
        Left (BackendLambdaTypeMismatch resultTy expected)
    BackendApp resultTy fun arg -> do
      validateBackendExprWith mbContext fun
      validateBackendExprWith mbContext arg
      case backendCallableHeadInContext mbContext fun of
        BackendClosureCallableHead name ->
          Left (BackendClosureCalledWithBackendApp name)
        _ ->
          pure ()
      case backendExprType fun of
        BTArrow expectedArg expectedResult -> do
          unless (backendApplicationTypeMatches mbContext expectedArg (backendExprType arg)) $
            Left (BackendApplicationArgumentMismatch expectedArg (backendExprType arg))
          unless (backendApplicationTypeMatches mbContext expectedResult resultTy) $
            Left (BackendApplicationResultMismatch resultTy expectedResult)
        other ->
          Left (BackendApplicationExpectedFunction other)
    BackendLet resultTy name bindingTy rhs body -> do
      validateBackendExprWith mbContext rhs
      unless (alphaEqBackendType (backendExprType rhs) bindingTy) $
        Left (BackendLetTypeMismatch name bindingTy (backendExprType rhs))
      validateBackendExprWith (extendLetLocalMaybe mbContext name bindingTy rhs) body
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
    BackendClosure resultTy entryName captures params body -> do
      requireUnique BackendDuplicateClosureCapture (map backendClosureCaptureName captures)
      requireUnique BackendDuplicateClosureParameter (map fst params)
      requireUnique BackendDuplicateClosureParameter (map backendClosureCaptureName captures ++ map fst params)
      mapM_ (validateBackendClosureCapture mbContext) captures
      let bodyContext =
            foldl
              (extendClosureCaptureLocalMaybe mbContext)
              (dropTermLocalsMaybe mbContext)
              captures
          bodyParamContext =
            foldl
              (\context0 (paramName, paramTy) -> extendFunctionParamLocalMaybe context0 paramName paramTy body)
              bodyContext
              params
      validateBackendExprWith bodyParamContext body
      validateBackendClosureFunctionType entryName resultTy params (backendExprType body)
    BackendClosureCall resultTy fun args -> do
      validateBackendExprWith mbContext fun
      mapM_ (validateBackendExprWith mbContext) args
      validateBackendClosureCall mbContext resultTy fun args

validateBackendClosureCapture :: Maybe BackendValidationContext -> BackendClosureCapture -> Either BackendValidationError ()
validateBackendClosureCapture mbContext capture = do
  validateBackendExprWith mbContext expr
  unless (alphaEqBackendType (backendClosureCaptureType capture) (backendExprType expr)) $
    Left (BackendClosureCaptureTypeMismatch (backendClosureCaptureName capture) (backendClosureCaptureType capture) (backendExprType expr))
  where
    expr = backendClosureCaptureExpr capture

validateBackendClosureFunctionType :: String -> BackendType -> [(String, BackendType)] -> BackendType -> Either BackendValidationError ()
validateBackendClosureFunctionType entryName resultTy params bodyTy =
  case collectClosureCallType resultTy of
    Nothing ->
      Left (BackendClosureExpectedFunction entryName resultTy)
    Just (declaredParamTys, declaredResultTy) -> do
      unless (length declaredParamTys == length params) $
        Left (BackendClosureParameterArityMismatch entryName (length params) (length declaredParamTys))
      let expected = foldr BTArrow bodyTy (map snd params)
      unless (and (zipWith alphaEqBackendType declaredParamTys (map snd params)) && alphaEqBackendType declaredResultTy bodyTy) $
        Left (BackendClosureTypeMismatch entryName resultTy expected)

backendCallableHead :: (String -> BackendCallableBindingKind) -> BackendExpr -> BackendCallableHead
backendCallableHead resolve0 =
  go resolve0
  where
    go resolve =
      \case
        BackendVar _ name ->
          case resolve name of
            BackendCallableBindingDirect ->
              BackendDirectCallableHead (Just name)
            BackendCallableBindingClosure ->
              BackendClosureCallableHead name
            BackendCallableBindingUnknown ->
              BackendUnknownCallableHead
        BackendLam {} ->
          BackendDirectCallableHead Nothing
        BackendClosure _ entryName _ _ _ ->
          BackendClosureCallableHead entryName
        BackendTyAbs _ _ _ body ->
          go resolve body
        BackendTyApp _ fun _ ->
          go resolve fun
        BackendLet _ name _ rhs body ->
          go (extendBindingKind resolve name (go resolve rhs)) body
        BackendCase _ _ alternatives ->
          collapseCallableHeads
            [ go (extendPatternBindingKinds resolve (backendAltPattern alternative) (backendAltBody alternative)) (backendAltBody alternative)
            | alternative <- NE.toList alternatives
            ]
        _ ->
          BackendUnknownCallableHead

    extendBindingKind resolve name headShape localName
      | localName == name =
          callableBindingKindForHead headShape
      | otherwise =
          resolve localName

backendCallableHeadInContext :: Maybe BackendValidationContext -> BackendExpr -> BackendCallableHead
backendCallableHeadInContext mbContext =
  backendCallableHead (backendCallableBindingKindInContext mbContext)

backendCallableBindingKindInContext :: Maybe BackendValidationContext -> String -> BackendCallableBindingKind
backendCallableBindingKindInContext Nothing _ =
  BackendCallableBindingUnknown
backendCallableBindingKindInContext (Just context0) name
  | Set.member name (bvcClosureLocals context0) =
      BackendCallableBindingClosure
  | Set.member name (bvcPossibleClosureLocals context0) =
      BackendCallableBindingUnknown
  | Map.member name (bvcLocals context0) =
      BackendCallableBindingDirect
  | Set.member name (bvcClosureGlobals context0) =
      BackendCallableBindingClosure
  | Map.member name (bvcGlobals context0) =
      BackendCallableBindingDirect
  | otherwise =
      BackendCallableBindingUnknown

callableBindingKindForHead :: BackendCallableHead -> BackendCallableBindingKind
callableBindingKindForHead =
  \case
    BackendDirectCallableHead _ ->
      BackendCallableBindingDirect
    BackendClosureCallableHead _ ->
      BackendCallableBindingClosure
    BackendUnknownCallableHead ->
      BackendCallableBindingUnknown

extendPatternBindingKinds ::
  (String -> BackendCallableBindingKind) ->
  BackendPattern ->
  BackendExpr ->
  String ->
  BackendCallableBindingKind
extendPatternBindingKinds resolve pattern0 body name
  | Set.member name binders,
    backendExprMentionsNameWithCallableType name body =
      BackendCallableBindingClosure
  | Set.member name binders =
      BackendCallableBindingDirect
  | otherwise =
      resolve name
  where
    binders = patternBinders pattern0

collapseCallableHeads :: [BackendCallableHead] -> BackendCallableHead
collapseCallableHeads heads
  | all isClosureHead heads =
      BackendClosureCallableHead (firstClosureHeadName heads)
  | all isDirectHead heads =
      BackendDirectCallableHead (firstDirectHeadName heads)
  | otherwise =
      BackendUnknownCallableHead
  where
    isClosureHead =
      \case
        BackendClosureCallableHead _ -> True
        _ -> False

    isDirectHead =
      \case
        BackendDirectCallableHead _ -> True
        _ -> False

firstClosureHeadName :: [BackendCallableHead] -> String
firstClosureHeadName =
  go
  where
    go [] =
      "__mlfp_unknown_closure_head"
    go (BackendClosureCallableHead name : _) =
      name
    go (_ : rest) =
      go rest

firstDirectHeadName :: [BackendCallableHead] -> Maybe String
firstDirectHeadName =
  go
  where
    go [] =
      Nothing
    go (BackendDirectCallableHead (Just name) : _) =
      Just name
    go (_ : rest) =
      go rest

backendExprMentionsNameWithCallableType :: String -> BackendExpr -> Bool
backendExprMentionsNameWithCallableType needle =
  go
  where
    go =
      \case
        BackendVar ty name ->
          name == needle && backendTypeIsClosureValue ty
        BackendLit {} ->
          False
        BackendLam _ name _ body
          | name == needle -> False
          | otherwise -> go body
        BackendApp _ fun arg ->
          go fun || go arg
        BackendLet _ name _ rhs body
          | name == needle -> go rhs
          | otherwise -> go rhs || go body
        BackendTyAbs _ _ _ body ->
          go body
        BackendTyApp ty (BackendVar _ name) _
          | name == needle,
            backendTypeIsClosureValue ty ->
              True
        BackendTyApp _ fun _ ->
          go fun
        BackendConstruct _ _ args ->
          any go args
        BackendCase _ scrutinee alternatives ->
          go scrutinee || any goAlternative (NE.toList alternatives)
        BackendRoll _ payload ->
          go payload
        BackendUnroll _ payload ->
          go payload
        BackendClosure _ _ captures params body ->
          any (go . backendClosureCaptureExpr) captures
            || (not (Set.member needle closureBinders) && go body)
          where
            closureBinders = Set.fromList (map backendClosureCaptureName captures ++ map fst params)
        BackendClosureCall _ fun args ->
          go fun || any go args

    goAlternative (BackendAlternative pattern0 body)
      | Set.member needle (patternBinders pattern0) = False
      | otherwise = go body

backendExprCallsNameAsClosureHead :: String -> BackendExpr -> Bool
backendExprCallsNameAsClosureHead needle =
  go (Set.singleton needle)
  where
    go aliases =
      \case
        BackendVar {} ->
          False
        BackendLit {} ->
          False
        BackendLam _ name _ body
          | Set.member name aliases -> False
          | otherwise -> go aliases body
        BackendApp _ fun arg ->
          go aliases fun || go aliases arg
        BackendLet _ name _ rhs body
          | Set.member name aliases -> go aliases rhs
          | otherwise ->
              let aliasesForBody =
                    if closureCallHeadReferencesAny aliases rhs
                      then Set.insert name aliases
                      else aliases
               in go aliases rhs || go aliasesForBody body
        BackendTyAbs _ _ _ body ->
          go aliases body
        BackendTyApp _ fun _ ->
          go aliases fun
        BackendConstruct _ _ args ->
          any (go aliases) args
        BackendCase _ scrutinee alternatives ->
          go aliases scrutinee || any (goAlternative aliases) (NE.toList alternatives)
        BackendRoll _ payload ->
          go aliases payload
        BackendUnroll _ payload ->
          go aliases payload
        BackendClosure _ _ captures params body ->
          any (go aliases . backendClosureCaptureExpr) captures
            || capturedNeedleFeedsClosureCall
            || (Set.disjoint aliases closureBinders && go aliases body)
          where
            closureBinders = Set.fromList (map backendClosureCaptureName captures ++ map fst params)
            capturedNeedleFeedsClosureCall =
              any capturesAlias captures
                && Set.disjoint aliases (Set.fromList (map fst params))
                && any (`backendExprCallsNameAsClosureHead` body) aliases
            capturesAlias capture =
              Set.member (backendClosureCaptureName capture) aliases
                && any (\alias -> backendExprReferencesName alias (backendClosureCaptureExpr capture)) aliases
        BackendClosureCall _ fun args ->
          closureCallHeadReferencesAny aliases fun || go aliases fun || any (go aliases) args

    goAlternative aliases (BackendAlternative pattern0 body)
      | not (Set.disjoint aliases (patternBinders pattern0)) = False
      | otherwise = go aliases body

closureCallHeadReferencesAny :: Set.Set String -> BackendExpr -> Bool
closureCallHeadReferencesAny needles expr =
  closureCallHeadReferencesAnyFrom needles expr

closureCallHeadReferencesAnyFrom :: Set.Set String -> BackendExpr -> Bool
closureCallHeadReferencesAnyFrom aliases0 =
  \case
    BackendVar _ name ->
      Set.member name aliases0
    BackendTyApp _ fun _ ->
      closureCallHeadReferencesAnyFrom aliases0 fun
    BackendLet _ name _ rhs body ->
      let aliasesWithoutShadow = Set.delete name aliases0
          aliasesForBody =
            if closureCallHeadReferencesAnyFrom aliases0 rhs
              then Set.insert name aliasesWithoutShadow
              else aliasesWithoutShadow
       in closureCallHeadReferencesAnyFrom aliasesForBody body
    _ ->
      False

backendExprReferencesName :: String -> BackendExpr -> Bool
backendExprReferencesName needle =
  go
  where
    go =
      \case
        BackendVar _ name ->
          name == needle
        BackendLit {} ->
          False
        BackendLam _ name _ body
          | name == needle -> False
          | otherwise -> go body
        BackendApp _ fun arg ->
          go fun || go arg
        BackendLet _ name _ rhs body
          | name == needle -> go rhs
          | otherwise -> go rhs || go body
        BackendTyAbs _ _ _ body ->
          go body
        BackendTyApp _ fun _ ->
          go fun
        BackendConstruct _ _ args ->
          any go args
        BackendCase _ scrutinee alternatives ->
          go scrutinee || any goAlternative (NE.toList alternatives)
        BackendRoll _ payload ->
          go payload
        BackendUnroll _ payload ->
          go payload
        BackendClosure _ _ captures params body ->
          any (go . backendClosureCaptureExpr) captures
            || (not (Set.member needle closureBinders) && go body)
          where
            closureBinders = Set.fromList (map backendClosureCaptureName captures ++ map fst params)
        BackendClosureCall _ fun args ->
          go fun || any go args

    goAlternative (BackendAlternative pattern0 body)
      | Set.member needle (patternBinders pattern0) = False
      | otherwise = go body

patternBinders :: BackendPattern -> Set.Set String
patternBinders =
  \case
    BackendDefaultPattern -> Set.empty
    BackendConstructorPattern _ binders -> Set.fromList binders

validateBackendClosureCall :: Maybe BackendValidationContext -> BackendType -> BackendExpr -> [BackendExpr] -> Either BackendValidationError ()
validateBackendClosureCall mbContext resultTy fun args =
  case collectClosureCallType funTy of
    Nothing ->
      Left (BackendClosureCallExpectedFunction funTy)
    Just (paramTys, expectedResultTy) -> do
      case backendCallableHeadInContext mbContext fun of
        BackendClosureCallableHead _ ->
          pure ()
        BackendDirectCallableHead (Just name) ->
          Left (BackendDirectCalledWithBackendClosureCall name)
        _ ->
          Left (BackendClosureCallExpectedClosureValue funTy)
      unless (length paramTys == length args) $
        Left (BackendClosureCallArityMismatch (length paramTys) (length args))
      zipWithM_
        validateArg
        [0 :: Int ..]
        (zip paramTys args)
      unless (backendApplicationTypeMatches mbContext expectedResultTy resultTy) $
        Left (BackendClosureCallResultMismatch resultTy expectedResultTy)
  where
    funTy =
      backendExprType fun

    validateArg index0 (expectedArgTy, arg) =
      unless (backendApplicationTypeMatches mbContext expectedArgTy (backendExprType arg)) $
        Left (BackendClosureCallArgumentMismatch index0 expectedArgTy (backendExprType arg))

collectClosureCallType :: BackendType -> Maybe ([BackendType], BackendType)
collectClosureCallType =
  go []
  where
    go params =
      \case
        BTArrow paramTy resultTy ->
          go (params ++ [paramTy]) resultTy
        other
          | null params -> Nothing
          | otherwise -> Just (params, other)

validateBackendVariable :: Maybe BackendValidationContext -> String -> BackendType -> Either BackendValidationError ()
validateBackendVariable Nothing _ _ =
  pure ()
validateBackendVariable (Just context0) name actualTy =
  case lookupBackendVariable context0 name of
    Nothing ->
      Left (BackendUnknownVariable name)
    Just expectedTy ->
      unless (backendVariableTypeMatches context0 name expectedTy actualTy) $
        Left (BackendVariableTypeMismatch name expectedTy actualTy)

backendApplicationTypeMatches :: Maybe BackendValidationContext -> BackendType -> BackendType -> Bool
backendApplicationTypeMatches mbContext expectedTy actualTy =
  backendTypeMatchesWith AllowStructuralPayloadInstantiation typeBounds dataDecls expectedTy actualTy
  where
    typeBounds = maybe Map.empty bvcTypeBounds mbContext
    dataDecls = bvcData <$> mbContext

backendVariableTypeMatches :: BackendValidationContext -> String -> BackendType -> BackendType -> Bool
backendVariableTypeMatches context0 name expectedTy actualTy =
  backendTypeMatchesWith
    (RejectFreeTypeVariableInstantiation (freshenedAliasesForVariable name))
    (bvcTypeBounds context0)
    (Just (bvcData context0))
    expectedTy
    actualTy

backendVariableTypeMatchesWithBounds :: Map.Map String (Maybe BackendType) -> BackendType -> BackendType -> Bool
backendVariableTypeMatchesWithBounds typeBounds expectedTy actualTy =
  backendTypeMatchesWith
    (RejectFreeTypeVariableInstantiation RejectFreshenedTypeVariableAliases)
    typeBounds
    Nothing
    expectedTy
    actualTy

freshenedAliasesForVariable :: String -> FreshenedTypeVariableAliases
freshenedAliasesForVariable ('$' : _) =
  AllowFreshenedTypeVariableAliases
freshenedAliasesForVariable _ =
  RejectFreshenedTypeVariableAliases

backendTypeMatchesWith ::
  TypeVariableInstantiation ->
  Map.Map String (Maybe BackendType) ->
  Maybe (Map.Map String BackendData) ->
  BackendType ->
  BackendType ->
  Bool
backendTypeMatchesWith typeVariableInstantiation typeBounds mbDataDecls expectedTy actualTy =
  go Set.empty expectedTy actualTy
  where
    go bound expected actual =
      alphaEqBackendType actual expected
        || typeVariableBoundMatches bound expected actual
        || typeVariableBoundMatches bound actual expected
        || case (expected, actual) of
          (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod)
            | opaqueIOFunctionCompatible bound expectedDom expectedCod actualDom actualCod ->
                True
          (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
            go bound expectedDom actualDom && go bound expectedCod actualCod
          (BTBase expectedBase, BTBase actualBase) ->
            expectedBase == actualBase
          (BTBase expectedBase, BTMu actualName actualBody) ->
            structuralMuMatchesKnownData expectedBase [] actualName actualBody
          (BTMu expectedName expectedBody, BTBase actualBase) ->
            structuralMuMatchesKnownData actualBase [] expectedName expectedBody
          (BTVar expectedName, BTVar actualName)
            | freshenedTypeVariablesMayMatch bound expectedName actualName ->
                True
          (BTCon expectedCon (_ :| []), BTCon actualCon (_ :| []))
            | isOpaqueIOBackendName expectedCon && isOpaqueIOBackendName actualCon ->
                True
          (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs) ->
            expectedCon == actualCon
              && zipAllWith (go bound) (NE.toList expectedArgs) (NE.toList actualArgs)
          (BTCon expectedCon expectedArgs, BTMu actualName actualBody) ->
            structuralMuMatchesKnownData expectedCon (NE.toList expectedArgs) actualName actualBody
          (BTMu expectedName expectedBody, BTCon actualCon actualArgs) ->
            structuralMuMatchesKnownData actualCon (NE.toList actualArgs) expectedName expectedBody
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
            structuralMuPayloadMayInstantiate expectedName expectedBody actualName actualBody
              || case (isVacuousRecursiveBinder expectedName expectedBody, isVacuousRecursiveBinder actualName actualBody) of
                (True, True) ->
                  go bound expectedBody actualBody
                (True, False) ->
                  recursiveBodyCompatible actualName actualBody expectedBody || go bound expectedBody actual
                (False, True) ->
                  recursiveBodyCompatible expectedName expectedBody actualBody || go bound expected actualBody
                (False, False) ->
                  let freshName = freshBinderName expectedName actualName Nothing Nothing expectedBody actualBody
                      expectedBody' = substituteBackendType expectedName (BTVar freshName) expectedBody
                      actualBody' = substituteBackendType actualName (BTVar freshName) actualBody
                   in go (Set.insert freshName bound) expectedBody' actualBody'
          (BTMu expectedName expectedBody, _)
            | isVacuousRecursiveBinder expectedName expectedBody ->
                go bound expectedBody actual
          (_, BTMu actualName actualBody)
            | isVacuousRecursiveBinder actualName actualBody ->
                go bound expected actualBody
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

    opaqueIOFunctionCompatible bound expectedDom expectedCod actualDom actualCod =
      opaqueIODomainCompatible bound expectedDom actualDom
        && go bound expectedCod actualCod

    opaqueIODomainCompatible bound expected actual =
      alphaEqBackendType expected actual
        || typeVariableBoundMatches bound expected actual
        || typeVariableBoundMatches bound actual expected
        || case (expected, actual) of
          (BTVar {}, BTVar {}) -> True
          _ -> False

    -- Conversion may alpha-freshen generated case/evidence binders while their
    -- lexical variable names stay fixed. Keep that escape hatch scoped to
    -- generated variables; user-facing variables still require exact names.
    freshenedTypeVariablesMayMatch bound expectedName actualName =
      case typeVariableInstantiation of
        RejectFreeTypeVariableInstantiation AllowFreshenedTypeVariableAliases ->
          Set.notMember expectedName bound
            && Set.notMember actualName bound
            && freshenedNameVariant expectedName actualName
        _ ->
          False

    freshenedNameVariant leftName rightName =
      leftName /= rightName
        && (isFreshenedFrom leftName rightName || isFreshenedFrom rightName leftName)

    isFreshenedFrom baseName candidateName =
      let (digits, prefix) = span isDigit (reverse candidateName)
       in not (null digits) && reverse prefix == baseName

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

    structuralMuMatchesKnownData base@(BaseTy dataName) args muName body =
      structuralMuMatchesData base args muName body
        || case mbDataDecls >>= Map.lookup dataName of
          Just dataDecl
            | structuralMuNameMatches dataName muName,
              Just substitution <- structuralDataArgumentSubstitution dataDecl args ->
                structuralMuPayloadMatchesDataDecl typeBounds dataDecl substitution (BTMu muName body)
          _ ->
            False

    -- Structural ADT payloads encode data parameters inside handler fields. Keep
    -- that instantiation path local to matching structural encodings of the same
    -- owner so ordinary recursive type matching still treats free variables
    -- strictly.
    structuralMuPayloadMayInstantiate expectedName expectedBody actualName actualBody =
      case typeVariableInstantiation of
        RejectFreeTypeVariableInstantiation {} ->
          False
        AllowStructuralPayloadInstantiation ->
          case (structuralMuDataName expectedName, structuralMuDataName actualName) of
            (Just expectedDataName, Just actualDataName)
              | expectedDataName == actualDataName ->
                  let freshSelf =
                        freshNameLike
                          expectedName
                          ( Set.unions
                              [ Set.fromList [expectedName, actualName],
                                Map.keysSet typeBounds,
                                freeBackendTypeVars expectedBody,
                                freeBackendTypeVars actualBody
                              ]
                          )
                      expectedBody' = substituteBackendType expectedName (BTVar freshSelf) expectedBody
                      actualBody' = substituteBackendType actualName (BTVar freshSelf) actualBody
                   in case (structuralMuPayloadTypes expectedBody', structuralMuPayloadTypes actualBody') of
                        (Just expectedPayloadTypes, Just actualPayloadTypes) ->
                          structuralPayloadTypesMayInstantiate
                            (Set.singleton freshSelf)
                            expectedPayloadTypes
                            actualPayloadTypes
                        _ ->
                          False
            _ ->
              False

    structuralPayloadTypeMayInstantiate bound expected actual =
      alphaEqBackendType expected actual
        || case (expected, actual) of
          (BTVar name, _)
            | Set.notMember name bound && Map.notMember name typeBounds ->
                True
          (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
            structuralPayloadTypeMayInstantiate bound expectedDom actualDom
              && structuralPayloadTypeMayInstantiate bound expectedCod actualCod
          (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs) ->
            expectedCon == actualCon
              && zipAllWith
                (structuralPayloadTypeMayInstantiate bound)
                (NE.toList expectedArgs)
                (NE.toList actualArgs)
          (BTVarApp expectedName expectedArgs, BTVarApp actualName actualArgs) ->
            expectedName == actualName
              && zipAllWith
                (structuralPayloadTypeMayInstantiate bound)
                (NE.toList expectedArgs)
                (NE.toList actualArgs)
          (BTForall expectedBinder expectedBound expectedForallBody, BTForall actualBinder actualBound actualForallBody) ->
            structuralPayloadMaybeBoundMayInstantiate bound expectedBound actualBound
              && let freshName =
                       freshNameLike
                         expectedBinder
                         ( Set.unions
                             [ Set.fromList [expectedBinder, actualBinder],
                               Map.keysSet typeBounds,
                               maybe Set.empty freeBackendTypeVars expectedBound,
                               maybe Set.empty freeBackendTypeVars actualBound,
                               freeBackendTypeVars expectedForallBody,
                               freeBackendTypeVars actualForallBody
                             ]
                         )
                     expectedForallBody' = substituteBackendType expectedBinder (BTVar freshName) expectedForallBody
                     actualForallBody' = substituteBackendType actualBinder (BTVar freshName) actualForallBody
                  in structuralPayloadTypeMayInstantiate (Set.insert freshName bound) expectedForallBody' actualForallBody'
          (BTMu expectedMuName expectedMuBody, BTMu actualMuName actualMuBody) ->
            let freshName =
                  freshNameLike
                    expectedMuName
                    ( Set.unions
                        [ Set.fromList [expectedMuName, actualMuName],
                          Map.keysSet typeBounds,
                          freeBackendTypeVars expectedMuBody,
                          freeBackendTypeVars actualMuBody
                        ]
                    )
                expectedMuBody' = substituteBackendType expectedMuName (BTVar freshName) expectedMuBody
                actualMuBody' = substituteBackendType actualMuName (BTVar freshName) actualMuBody
             in structuralPayloadTypeMayInstantiate (Set.insert freshName bound) expectedMuBody' actualMuBody'
          _ ->
            go bound expected actual

    structuralPayloadTypesMayInstantiate bound expectedPayloadTypes actualPayloadTypes =
      zipAllWith
        (structuralPayloadTypeMayInstantiate bound)
        expectedPayloadTypes
        actualPayloadTypes
        || zipAllWith
          (structuralPayloadTypeMayInstantiate bound)
          actualPayloadTypes
          expectedPayloadTypes

    structuralPayloadMaybeBoundMayInstantiate _ Nothing Nothing =
      True
    structuralPayloadMaybeBoundMayInstantiate bound (Just expectedBound) (Just actualBound) =
      structuralPayloadTypeMayInstantiate bound expectedBound actualBound
    structuralPayloadMaybeBoundMayInstantiate _ _ _ =
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

extendFunctionParamLocalMaybe :: Maybe BackendValidationContext -> String -> BackendType -> BackendExpr -> Maybe BackendValidationContext
extendFunctionParamLocalMaybe mbContext name ty body
  | backendExprCallsNameAsClosureHead name body =
      extendClosureLocalMaybe mbContext name ty
  | otherwise =
      extendLocalMaybe mbContext name ty

extendLocal :: BackendValidationContext -> String -> BackendType -> BackendValidationContext
extendLocal context0 name ty =
  context0
    { bvcLocals = Map.insert name ty (bvcLocals context0),
      bvcClosureLocals = Set.delete name (bvcClosureLocals context0),
      bvcPossibleClosureLocals = Set.delete name (bvcPossibleClosureLocals context0)
    }

extendClosureLocalMaybe :: Maybe BackendValidationContext -> String -> BackendType -> Maybe BackendValidationContext
extendClosureLocalMaybe mbContext name ty =
  fmap (\context0 -> extendClosureLocal context0 name ty) mbContext

extendClosureLocal :: BackendValidationContext -> String -> BackendType -> BackendValidationContext
extendClosureLocal context0 name ty =
  context0
    { bvcLocals = Map.insert name ty (bvcLocals context0),
      bvcClosureLocals = Set.insert name (bvcClosureLocals context0),
      bvcPossibleClosureLocals = Set.delete name (bvcPossibleClosureLocals context0)
    }

extendPossibleClosureLocalMaybe :: Maybe BackendValidationContext -> String -> BackendType -> Maybe BackendValidationContext
extendPossibleClosureLocalMaybe mbContext name ty =
  fmap (\context0 -> extendPossibleClosureLocal context0 name ty) mbContext

extendPossibleClosureLocal :: BackendValidationContext -> String -> BackendType -> BackendValidationContext
extendPossibleClosureLocal context0 name ty =
  context0
    { bvcLocals = Map.insert name ty (bvcLocals context0),
      bvcClosureLocals = Set.delete name (bvcClosureLocals context0),
      bvcPossibleClosureLocals = Set.insert name (bvcPossibleClosureLocals context0)
    }

extendLetLocalMaybe :: Maybe BackendValidationContext -> String -> BackendType -> BackendExpr -> Maybe BackendValidationContext
extendLetLocalMaybe mbContext name ty rhs =
  extendClosureShapeLocalMaybe mbContext mbContext name ty rhs

extendClosureCaptureLocalMaybe ::
  Maybe BackendValidationContext ->
  Maybe BackendValidationContext ->
  BackendClosureCapture ->
  Maybe BackendValidationContext
extendClosureCaptureLocalMaybe outerContext bodyContext capture =
  extendClosureShapeLocalMaybe
    outerContext
    bodyContext
    (backendClosureCaptureName capture)
    (backendClosureCaptureType capture)
    (backendClosureCaptureExpr capture)

extendClosureShapeLocalMaybe ::
  Maybe BackendValidationContext ->
  Maybe BackendValidationContext ->
  String ->
  BackendType ->
  BackendExpr ->
  Maybe BackendValidationContext
extendClosureShapeLocalMaybe sourceContext targetContext name ty rhs
  | not (backendTypeIsClosureValue ty) =
      extendLocalMaybe targetContext name ty
  | otherwise =
      case backendCallableHeadInContext sourceContext rhs of
        BackendClosureCallableHead _ ->
          extendClosureLocalMaybe targetContext name ty
        BackendUnknownCallableHead ->
          extendPossibleClosureLocalMaybe targetContext name ty
        BackendDirectCallableHead _ ->
          extendLocalMaybe targetContext name ty

extendPatternLocals :: BackendValidationContext -> [(String, BackendType)] -> BackendValidationContext
extendPatternLocals =
  foldr extendOne
  where
    extendOne (name, ty) context0
      | backendTypeIsClosureValue ty = extendClosureLocal context0 name ty
      | otherwise = extendLocal context0 name ty

backendTypeIsClosureValue :: BackendType -> Bool
backendTypeIsClosureValue =
  \case
    BTArrow {} -> True
    _ -> False

isOpaqueIOBackendName :: BaseTy -> Bool
isOpaqueIOBackendName (BaseTy name) =
  name == "IO" || name == "<builtin>.IO"

dropTermLocalsMaybe :: Maybe BackendValidationContext -> Maybe BackendValidationContext
dropTermLocalsMaybe =
  fmap
    ( \context0 ->
        context0
          { bvcLocals = Map.empty,
            bvcClosureLocals = Set.empty,
            bvcPossibleClosureLocals = Set.empty
          }
    )

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
      validateBackendConstructorStructuralPayload
        (bvcTypeBounds context0)
        constructorInfo
        substitution
        (backendConstructorResult constructor)
        (BackendConstructorResultMismatch name (backendConstructorResult constructor) resultTy)
      validateBackendConstructorStructuralPayload
        (bvcTypeBounds context0)
        constructorInfo
        substitution
        resultTy
        (BackendConstructorResultMismatch name (backendConstructorResult constructor) resultTy)
      finalSubstitution <-
        foldM
          (validateBackendConstructorArgument (bvcTypeBounds context0) (Just (bvcData context0)) dataParameters parameters name)
          substitution
          (zip [0 ..] (zip fields args))
      validateBackendConstructorResultSubstitution
        (bvcTypeBounds context0)
        (Just (bvcData context0))
        constructorInfo
        finalSubstitution
        resultTy
        (BackendConstructorResultMismatch name (backendConstructorResult constructor) resultTy)
      pure ()

validateBackendConstructorArgument ::
  Map.Map String (Maybe BackendType) ->
  Maybe (Map.Map String BackendData) ->
  [String] ->
  BackendParameterBounds ->
  String ->
  Map.Map String BackendType ->
  (Int, (BackendType, BackendExpr)) ->
  Either BackendValidationError (Map.Map String BackendType)
validateBackendConstructorArgument typeBounds mbDataDecls dataParameters parameters name substitution (index0, (expectedTy, arg)) =
  case matchBackendTypeParametersWithTypeBounds typeBounds dataParameters parameters substitution expectedTy (backendExprType arg) of
    Just substitution' ->
      pure substitution'
    Nothing ->
      let completedSubstitution = completeBackendParameterSubstitution parameters substitution
          substitutedExpectedTy = substituteBackendTypes completedSubstitution expectedTy
       in if backendConstructorFieldTypeMatches substitutedExpectedTy
            then pure substitution
            else
              Left
                ( BackendConstructorArgumentMismatch
                    name
                    index0
                    substitutedExpectedTy
                    (backendExprType arg)
                )
  where
    backendConstructorFieldTypeMatches substitutedExpectedTy =
      ( backendTypeContainsVarApp expectedTy
          && backendVariableTypeMatchesWithBounds typeBounds substitutedExpectedTy (backendExprType arg)
      )
        || backendStructuralDataBoundaryMatches typeBounds mbDataDecls substitutedExpectedTy (backendExprType arg)

backendTypeContainsVarApp :: BackendType -> Bool
backendTypeContainsVarApp =
  \case
    BTVar {} -> False
    BTArrow dom cod -> backendTypeContainsVarApp dom || backendTypeContainsVarApp cod
    BTBase {} -> False
    BTCon _ args -> any backendTypeContainsVarApp args
    BTVarApp {} -> True
    BTForall _ mb body -> maybe False backendTypeContainsVarApp mb || backendTypeContainsVarApp body
    BTMu _ body -> backendTypeContainsVarApp body
    BTBottom -> False

backendStructuralDataBoundaryMatches ::
  Map.Map String (Maybe BackendType) ->
  Maybe (Map.Map String BackendData) ->
  BackendType ->
  BackendType ->
  Bool
backendStructuralDataBoundaryMatches typeBounds mbDataDecls expectedTy actualTy =
  go expectedTy actualTy
  where
    go expected actual =
      alphaEqBackendType expected actual
        || case (expected, actual) of
          (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
            go expectedDom actualDom && go expectedCod actualCod
          (BTBase expectedBase, BTBase actualBase) ->
            expectedBase == actualBase
          (BTBase expectedBase, BTMu actualName actualBody) ->
            structuralMuMatchesKnownData expectedBase [] actualName actualBody
          (BTMu expectedName expectedBody, BTBase actualBase) ->
            structuralMuMatchesKnownData actualBase [] expectedName expectedBody
          (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs) ->
            expectedCon == actualCon
              && zipAllWith go (NE.toList expectedArgs) (NE.toList actualArgs)
          (BTCon expectedCon expectedArgs, BTMu actualName actualBody) ->
            structuralMuMatchesKnownData expectedCon (NE.toList expectedArgs) actualName actualBody
          (BTMu expectedName expectedBody, BTCon actualCon actualArgs) ->
            structuralMuMatchesKnownData actualCon (NE.toList actualArgs) expectedName expectedBody
          (BTMu expectedName expectedBody, BTMu actualName actualBody) ->
            structuralMuBodiesMatchKnownData expectedName expectedBody actualName actualBody
          (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) ->
            maybeBoundaryMatches expectedBound actualBound
              && let freshName = freshBinderName expectedName actualName expectedBound actualBound expectedBody actualBody
                     expectedBody' = substituteBackendType expectedName (BTVar freshName) expectedBody
                     actualBody' = substituteBackendType actualName (BTVar freshName) actualBody
                  in go expectedBody' actualBody'
          (BTBottom, BTBottom) ->
            True
          _ ->
            False

    structuralMuBodiesMatchKnownData expectedName expectedBody actualName actualBody =
      case
        ( structuralMuDataName expectedName,
          structuralMuDataName actualName,
          structuralMuHandlerTypes expectedBody,
          structuralMuHandlerTypes actualBody
        )
        of
          ( Just expectedDataName,
            Just actualDataName,
            Just (expectedResultName, expectedHandlers),
            Just (actualResultName, actualHandlers)
            )
              | expectedDataName == actualDataName,
                Just dataDecl <- mbDataDecls >>= Map.lookup expectedDataName,
                length expectedHandlers == length actualHandlers,
                length expectedHandlers == length (backendDataConstructors dataDecl) ->
                  let freshSelf =
                        freshNameLike
                          expectedName
                          ( Set.unions
                              [ Set.fromList [expectedName, actualName],
                                freeBackendTypeVars expectedBody,
                                freeBackendTypeVars actualBody
                              ]
                          )
                      freshResult =
                        freshNameLike
                          expectedResultName
                          ( Set.unions
                              [ Set.fromList [expectedResultName, actualResultName, freshSelf],
                                freeBackendTypeVars expectedBody,
                                freeBackendTypeVars actualBody
                              ]
                          )
                      normalizeHandler selfName resultName =
                        substituteBackendTypes
                          ( Map.fromList
                              [ (selfName, BTVar freshSelf),
                                (resultName, BTVar freshResult)
                              ]
                          )
                   in zipAllWith
                        go
                        (map (normalizeHandler expectedName expectedResultName) expectedHandlers)
                        (map (normalizeHandler actualName actualResultName) actualHandlers)
          _ ->
            False

    maybeBoundaryMatches Nothing Nothing =
      True
    maybeBoundaryMatches (Just expectedBound) (Just actualBound) =
      go expectedBound actualBound
    maybeBoundaryMatches _ _ =
      False

    structuralMuMatchesKnownData base@(BaseTy dataName) args muName body =
      structuralMuMatchesData base args muName body
        || case mbDataDecls >>= Map.lookup dataName of
          Just dataDecl
            | structuralMuNameMatches dataName muName,
              Just substitution <- structuralDataArgumentSubstitution dataDecl args ->
                structuralMuPayloadMatchesDataDecl typeBounds dataDecl substitution (BTMu muName body)
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
      validateBackendConstructorStructuralPayload
        (bvcTypeBounds context0)
        constructorInfo
        substitution
        (backendConstructorResult constructor)
        (BackendCaseConstructorScrutineeMismatch name scrutineeTy (backendConstructorResult constructor))
      validateBackendConstructorStructuralPayload
        (bvcTypeBounds context0)
        constructorInfo
        substitution
        scrutineeTy
        (BackendCaseConstructorScrutineeMismatch name scrutineeTy (backendConstructorResult constructor))
      let fresheningSubstitution = constructorPatternFresheningSubstitution context0 substitution constructor
          patternSubstitution = Map.union fresheningSubstitution substitution
          instantiatedFields = map (substituteBackendTypes patternSubstitution) fields
          contextForBody =
            extendTypeBounds
              context0
              (constructorPatternTypeBounds substitution fresheningSubstitution constructor)
      pure (Just (extendPatternLocals contextForBody (zip binders instantiatedFields)))

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
  constructorTypeParameterBoundsForData (bciDataParameters constructorInfo) (bciConstructor constructorInfo)

constructorTypeParameterBoundsForData :: [String] -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsForData dataParameters constructor =
  Map.fromList $
    [(name, Nothing) | name <- dataParameters]
      ++ [ (backendTypeBinderName binder, backendTypeBinderBound binder)
           | binder <- backendConstructorForalls constructor
         ]

validateBackendConstructorStructuralPayload ::
  Map.Map String (Maybe BackendType) ->
  BackendConstructorInfo ->
  Map.Map String BackendType ->
  BackendType ->
  BackendValidationError ->
  Either BackendValidationError ()
validateBackendConstructorStructuralPayload typeBounds constructorInfo substitution ty mismatchError =
  unless (backendConstructorStructuralPayloadMatches typeBounds constructorInfo substitution ty) $
    Left mismatchError

validateBackendConstructorResultSubstitution ::
  Map.Map String (Maybe BackendType) ->
  Maybe (Map.Map String BackendData) ->
  BackendConstructorInfo ->
  Map.Map String BackendType ->
  BackendType ->
  BackendValidationError ->
  Either BackendValidationError ()
validateBackendConstructorResultSubstitution typeBounds mbDataDecls constructorInfo substitution resultTy mismatchError =
  unless (backendStructuralDataBoundaryMatches typeBounds mbDataDecls substitutedResultTy resultTy) $
    Left mismatchError
  where
    constructor =
      bciConstructor constructorInfo
    completedSubstitution =
      completeBackendParameterSubstitution (constructorTypeParameterBounds constructorInfo) substitution
    substitutedResultTy =
      substituteBackendTypes completedSubstitution (backendConstructorResult constructor)

backendConstructorStructuralPayloadMatches ::
  Map.Map String (Maybe BackendType) ->
  BackendConstructorInfo ->
  Map.Map String BackendType ->
  BackendType ->
  Bool
backendConstructorStructuralPayloadMatches typeBounds constructorInfo substitution =
  \case
    BTMu muName body
      | structuralMuNameMatches (bciDataName constructorInfo) muName ->
          case structuralMuHandlerTypes body of
            Just (resultName, handlers) ->
              structuralPayloadHandlersMatchForData
                typeBounds
                (bciDataParameters constructorInfo)
                (bciDataConstructors constructorInfo)
                substitution
                (BTMu muName body)
                muName
                resultName
                handlers
            Nothing ->
              False
    _ ->
      True

structuralMuPayloadMatchesDataDecl ::
  Map.Map String (Maybe BackendType) ->
  BackendData ->
  Map.Map String BackendType ->
  BackendType ->
  Bool
structuralMuPayloadMatchesDataDecl typeBounds dataDecl substitution =
  \case
    BTMu muName body
      | structuralMuNameMatches (backendDataName dataDecl) muName ->
          case structuralMuHandlerTypes body of
            Just (resultName, handlers) ->
              structuralPayloadHandlersMatchForData
                typeBounds
                (backendDataParameters dataDecl)
                (backendDataConstructors dataDecl)
                substitution
                (BTMu muName body)
                muName
                resultName
                handlers
            Nothing ->
              False
    _ ->
      True

structuralDataArgumentSubstitution :: BackendData -> [BackendType] -> Maybe (Map.Map String BackendType)
structuralDataArgumentSubstitution dataDecl args
  | length dataParameters == length args =
      Just (Map.fromList (zip dataParameters args))
  | otherwise =
      Nothing
  where
    dataParameters =
      backendDataParameters dataDecl

structuralPayloadHandlersMatchForData ::
  Map.Map String (Maybe BackendType) ->
  [String] ->
  [BackendConstructor] ->
  Map.Map String BackendType ->
  BackendType ->
  String ->
  String ->
  [BackendType] ->
  Bool
structuralPayloadHandlersMatchForData typeBounds dataParameters constructors substitution structuralTy muName resultName handlers =
  length constructors == length handlers
    && and (zipWith constructorHandlerMatches constructors handlers)
  where
    dataSubstitution =
      Map.filterWithKey (\name _ -> name `elem` dataParameters) substitution
    structuralTyWithData =
      substituteBackendTypes dataSubstitution structuralTy
    knownSubstitution =
      Map.insert muName structuralTyWithData dataSubstitution
    substituteKnownTypes =
      substituteBackendTypes knownSubstitution
    constructorHandlerMatches constructor handlerTy =
      case
        matchBackendTypeParametersWithTypeBounds
          typeBounds
          dataParameters
          parameters
          Map.empty
          expectedHandlerTy
          actualHandlerTy
        of
          Just _ -> True
          Nothing -> False
      where
        expectedHandlerTy =
          substituteKnownTypes (constructorStructuralHandlerType resultName constructor)
        actualHandlerTy =
          substituteKnownTypes handlerTy
        parameters =
          Map.map (fmap substituteKnownTypes) $
            constructorTypeParameterBoundsForData dataParameters constructor

constructorStructuralHandlerType :: String -> BackendConstructor -> BackendType
constructorStructuralHandlerType resultName constructor =
  foldr wrapForall handlerBody (backendConstructorForalls constructor)
  where
    handlerBody =
      foldr BTArrow (BTVar resultName) (backendConstructorFields constructor)

    wrapForall binder body =
      BTForall (backendTypeBinderName binder) (backendTypeBinderBound binder) body

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
          case (expected, actual) of
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
                case (isVacuousRecursiveBinder expectedName expectedBody, isVacuousRecursiveBinder actualName actualBody) of
                  (True, True) ->
                    go bound substitution expectedBody actualBody
                  (True, False)
                    | recursiveBodyCompatible actualName actualBody expectedBody
                        && expectedBodyHasNoParameters expectedBody ->
                        Just substitution
                    | otherwise ->
                        go bound substitution expectedBody actual
                  (False, True)
                    | recursiveBodyCompatible expectedName expectedBody actualBody
                        && expectedBodyHasNoParameters expectedBody ->
                        Just substitution
                    | otherwise ->
                        go bound substitution expected actualBody
                  (False, False) -> do
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
              (BTMu expectedName expectedBody, _)
                | isVacuousRecursiveBinder expectedName expectedBody ->
                    go bound substitution expectedBody actual
              (_, BTMu actualName actualBody)
                | isVacuousRecursiveBinder actualName actualBody ->
                    go bound substitution expected actualBody
              (BTBottom, BTBottom) ->
                Just substitution
              _ ->
                if alphaEqBackendType expected actual
                  then Just substitution
                  else Nothing

    matchMaybeBound _ substitution Nothing Nothing =
      Just substitution
    matchMaybeBound bound substitution (Just expectedBound) (Just actualBound) =
      go bound substitution expectedBound actualBound
    matchMaybeBound _ _ _ _ =
      Nothing

    matchStructuralMuExpected bound substitution muName body actualTy =
      firstJust
        [ structuralMuNominalTypeMatches actualTy muName body >>= \() -> Just substitution,
          structuralMuAsDataTypeForBody muName body
            >>= \expectedTy -> go bound substitution expectedTy actualTy,
          structuralMuPayloadTypes body
            *> structuralMuAsActualDataType muName actualTy
            >>= \expectedTy -> go bound substitution expectedTy actualTy
        ]

    matchStructuralMuActual bound substitution expectedTy muName body =
      firstJust
        [ structuralMuNominalTypeMatches expectedTy muName body >>= \() -> Just substitution,
          structuralMuAsDataTypeForBody muName body
            >>= \actualTy -> go bound substitution expectedTy actualTy,
          structuralMuPayloadTypes body
            *> structuralMuAsActualDataType muName expectedTy
            >>= \actualTy -> go bound substitution expectedTy actualTy
        ]

    structuralMuAsDataTypeForBody muName body =
      structuralMuPayloadTypes body *> structuralMuAsDataType dataParameterOrder muName

    structuralMuNominalTypeMatches nominalTy muName body =
      if nominalMatches
        then Just ()
        else Nothing
      where
        nominalMatches =
          case nominalTy of
            BTBase base ->
              structuralMuMatchesData base [] muName body
            BTCon base args ->
              structuralMuMatchesData base (NE.toList args) muName body
            _ ->
              False

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

    expectedBodyHasNoParameters expectedBody =
      Set.null (freeBackendTypeVars expectedBody `Set.intersection` Map.keysSet parameterBounds)

decomposeBackendTypeHead :: BackendType -> Maybe (BackendType, [BackendType])
decomposeBackendTypeHead ty =
  case ty of
    BTVar name -> Just (BTVar name, [])
    BTBase name -> Just (BTBase name, [])
    BTCon name args -> Just (BTBase name, NE.toList args)
    BTVarApp name args -> Just (BTVar name, NE.toList args)
    _ -> Nothing

isVacuousRecursiveBinder :: String -> BackendType -> Bool
isVacuousRecursiveBinder name body =
  Set.notMember name (freeBackendTypeVars body)

recursiveBodyCompatible :: String -> BackendType -> BackendType -> Bool
recursiveBodyCompatible recursiveName recursiveBody plainBody =
  case go Set.empty Map.empty Nothing recursiveBody plainBody of
    Just _ -> True
    Nothing -> False
  where
    go patternVars patternBindings recursiveAlias leftTy rightTy =
      case (leftTy, rightTy) of
        (BTVar name, _)
          | name == recursiveName ->
              matchRecursiveAlias patternBindings recursiveAlias rightTy
          | Set.member name patternVars ->
              matchPatternVar name patternBindings recursiveAlias rightTy
        (BTVar leftName, BTVar rightName)
          | leftName == rightName ->
              Just (patternBindings, recursiveAlias)
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          go patternVars patternBindings recursiveAlias leftDom rightDom
            >>= \(patternBindings', recursiveAlias') ->
              go patternVars patternBindings' recursiveAlias' leftCod rightCod
        (BTBase leftBase, BTBase rightBase)
          | leftBase == rightBase ->
              Just (patternBindings, recursiveAlias)
        (BTCon leftCon leftArgs, BTCon rightCon rightArgs)
          | leftCon == rightCon ->
              foldM
                ( \(patternBindingsAcc, recursiveAliasAcc) (leftArg, rightArg) ->
                    go patternVars patternBindingsAcc recursiveAliasAcc leftArg rightArg
                )
                (patternBindings, recursiveAlias)
                (zip (NE.toList leftArgs) (NE.toList rightArgs))
                >>= \(patternBindings', recursiveAlias') ->
                  if length leftArgs == length rightArgs
                    then Just (patternBindings', recursiveAlias')
                    else Nothing
        (BTForall leftName Nothing leftBody, BTForall rightName Nothing rightBody) ->
          let freshName = freshRecursiveBodyBinder leftName rightName leftBody rightBody
              leftBody' = substituteBackendType leftName (BTVar freshName) leftBody
              rightBody' = substituteBackendType rightName (BTVar freshName) rightBody
           in go patternVars patternBindings recursiveAlias leftBody' rightBody'
        (BTForall leftName (Just leftBound) leftBody, BTForall rightName (Just rightBound) rightBody)
          | alphaEqBackendType leftBound rightBound ->
              let freshName = freshRecursiveBodyBinder leftName rightName leftBody rightBody
                  leftBody' = substituteBackendType leftName (BTVar freshName) leftBody
                  rightBody' = substituteBackendType rightName (BTVar freshName) rightBody
               in go patternVars patternBindings recursiveAlias leftBody' rightBody'
        (BTForall leftName Nothing leftBody, _) ->
          go (Set.insert leftName patternVars) patternBindings recursiveAlias leftBody rightTy
        (_, BTForall rightName Nothing rightBody)
          | Set.member recursiveName (freeBackendTypeVars leftTy) ->
              let aliasName = freshNameLike rightName (freeBackendTypeVars leftTy `Set.union` freeBackendTypeVars rightBody)
                  rightBody' = substituteBackendType rightName (BTVar aliasName) rightBody
               in case recursiveAlias of
                    Nothing ->
                      go patternVars patternBindings (Just aliasName) leftTy rightBody'
                    Just previous
                      | previous == aliasName ->
                          go patternVars patternBindings recursiveAlias leftTy rightBody'
                    _ ->
                      Nothing
        (BTBottom, BTBottom) ->
          Just (patternBindings, recursiveAlias)
        _ ->
          Nothing

    matchPatternVar name patternBindings recursiveAlias rightTy =
      case Map.lookup name patternBindings of
        Nothing ->
          Just (Map.insert name rightTy patternBindings, recursiveAlias)
        Just previous
          | alphaEqBackendType previous rightTy ->
              Just (patternBindings, recursiveAlias)
        _ ->
          Nothing

    matchRecursiveAlias patternBindings recursiveAlias rightTy =
      case rightTy of
        BTVar rightName ->
          case recursiveAlias of
            Nothing ->
              Just (patternBindings, Just rightName)
            Just expectedName
              | expectedName == rightName ->
                  Just (patternBindings, recursiveAlias)
            _ ->
              Nothing
        _ ->
          Nothing

    freshRecursiveBodyBinder leftName rightName leftBody rightBody =
      freshNameLike
        leftName
        ( Set.unions
            [ Set.fromList [leftName, rightName, recursiveName],
              freeBackendTypeVars leftBody,
              freeBackendTypeVars rightBody
            ]
        )

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

rejectClosureEntryNameCollisions :: [String] -> [String] -> Either BackendValidationError ()
rejectClosureEntryNameCollisions closureEntryNames reservedNames =
  case [name | name <- sort closureEntryNames, Set.member name reservedNameSet] of
    name : _ -> Left (BackendClosureEntryNameCollision name)
    [] -> Right ()
  where
    reservedNameSet = Set.fromList reservedNames

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
