{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
* the shared callable-head classifier for that invariant lives in the private
  owner `MLF.Backend.CallableShape`; `MLF.Backend.IR` supplies the executable
  IR adapter and validation context that consume it;
* application/lambda/let/type-application/recursive fold-unfold nodes satisfy
  local type equalities;
* ADT construction and case analysis are explicit backend nodes, so a backend
  lowerer does not have to inspect source syntax or Church-encoded runtime
  terms to find the intended control/data boundary; constructor uses and case
  alternatives are checked against backend constructor metadata.
* Row-4 ADT/case ownership keeps semantic constructor/case nodes in
  `MLF.Backend.IR`: `BackendData`, `BackendConstructor`, `BackendConstruct`,
  and `BackendCase` preserve metadata, constructor use, and alternatives
  only. Runtime tags, field slots, closure-record storage for function-like
  fields, and nullary tag-only representation stay private to LLVM/native
  lowering. The IR does not carry tag numbers, field offsets, nullary layout
  witnesses, or layout-only forms.
* Row-5 primitive/eager ownership keeps the primitive surface at the
  inventory-owned reserved runtime-binding set in `MLF.Primitive.Inventory`:
  `__mlfp_and` plus the IO primitive names classified there for native support;
* those primitives reach this IR through ordinary `BackendVar`, `BackendApp`, and `BackendTyApp` nodes, with no new `BackendPrim`, no broad FFI surface, and no second executable IR;
* the eager boundary is reviewable here: let RHS before body, case scrutinee before branch selection, direct/primitive call arguments in written order, and effect sequencing remains explicit through `__io_bind`;
* unsupported broader primitive or ordering-sensitive shapes stay on explicit
  backend diagnostic paths instead of a fallback runtime lane;
* checked `Backend.IR` may still carry `BackendTyAbs` and `BackendTyApp`;
* LLVM/native lowering owns only the specialization-based lowerable subset;
* complete type applications may specialize privately inside the lowerer; and
* residual runtime polymorphism remains unsupported and must fail with explicit diagnostics without widening the backend boundary;
* `BackendClosureCall` is the indirect closure-call node, so closure-valued
  aliases, captured closures, constructor-field projections, and case/let-
  selected closure values stay on this explicit path, and confused direct-call
  heads are rejected with explicit callable diagnostics.

The IR may still carry explicit type abstraction/application and recursive
roll/unroll nodes. Lowering passes are expected to reject unsupported backend
features at their own boundary rather than weakening this checked IR contract
or erasing runtime polymorphism by accident.
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
import MLF.Backend.CallableShape
import MLF.Backend.IR.Types
import MLF.Backend.StructuralRecursiveData
  ( BackendParameterBounds,
    alphaEqBackendType,
    backendStructuralDataBoundaryMatches,
    completeBackendParameterSubstitution,
    isVacuousRecursiveBinder,
    matchBackendTypeParametersWithTypeBounds,
    metadataLightStructuralDataMatches,
    structuralDataArgumentSubstitution,
    structuralDataDeclarationMatches,
    structuralPayloadsMayInstantiate,
    structuralRecursiveDataName,
    recursiveBodyCompatible,
  )
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Syntax (Lit (..))
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Util.Names (freshNameLike)

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

data FreshenedTypeVariableAliases
  = RejectFreshenedTypeVariableAliases
  | AllowFreshenedTypeVariableAliases
  deriving (Eq, Show)

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
  Map.map (primitiveTypeToBackendType . PrimitiveInventory.primitiveValueType) PrimitiveInventory.primitiveValueSpecs

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

instance BackendCallableExpr BackendExpr where
  backendCallableExprView =
    \case
      BackendVar _ name ->
        BackendCallableVar name
      BackendLam {} ->
        BackendCallableLam
      BackendClosure _ entryName _ _ _ ->
        BackendCallableClosure entryName
      BackendTyAbs _ _ _ body ->
        BackendCallableTyAbs body
      BackendTyApp _ fun _ ->
        BackendCallableTyApp fun
      BackendLet _ name _ rhs body ->
        BackendCallableLet name rhs body
      BackendCase _ _ alternatives ->
        BackendCallableCase
          [ let binders = patternBinders (backendAltPattern alternative)
                body = backendAltBody alternative
             in BackendCallableAlternative
                  { backendCallableAltBinders = binders,
                    backendCallableAltClosureBinders =
                      Set.filter
                        (\name -> backendExprMentionsNameWithCallableType name body)
                        binders,
                    backendCallableAltBody = body
                  }
          | alternative <- NE.toList alternatives
          ]
      _ ->
        BackendCallableOpaque

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
    || backendStructuralDataBoundaryMatches
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
      opaqueIOResultCompatible expectedCod actualCod
        && opaqueIODomainCompatible bound expectedDom actualDom
        && go bound expectedCod actualCod

    opaqueIOResultCompatible expected actual =
      case (expected, actual) of
        (BTCon expectedCon (_ :| []), BTCon actualCon (_ :| [])) ->
          isOpaqueIOBackendName expectedCon && isOpaqueIOBackendName actualCon
        _ ->
          False

    opaqueIODomainCompatible bound expected actual =
      alphaEqBackendType expected actual
        || typeVariableBoundMatches bound expected actual
        || typeVariableBoundMatches bound actual expected
        || case (expected, actual) of
          (BTVar expectedName, _)
            | Set.notMember expectedName bound -> True
          (_, BTVar actualName)
            | Set.notMember actualName bound -> True
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
      metadataLightStructuralDataMatches base args muName body
        || maybe False (\structuralName -> PrimitiveInventory.matchesBuiltinTypeName dataName structuralName && metadataLightStructuralDataMatches (BaseTy structuralName) args muName body) (structuralRecursiveDataName muName)
        || any structuralDataDeclMatches (structuralLookupNames dataName muName)
      where
        structuralDataDeclMatches lookupName =
          case mbDataDecls >>= Map.lookup lookupName of
            Just dataDecl
              | Just substitution <- structuralDataArgumentSubstitution dataDecl args ->
                  structuralDataDeclarationMatches typeBounds dataDecl substitution (BTMu muName body)
            _ ->
              False

    structuralLookupNames dataName muName =
      dataName
        : [ structuralName
            | Just structuralName <- [structuralRecursiveDataName muName],
              PrimitiveInventory.matchesBuiltinTypeName dataName structuralName
          ]

    -- Structural ADT payloads encode data parameters inside handler fields. Keep
    -- that instantiation path local to matching structural encodings of the same
    -- owner so ordinary recursive type matching still treats free variables
    -- strictly.
    structuralMuPayloadMayInstantiate expectedName expectedBody actualName actualBody =
      case typeVariableInstantiation of
        RejectFreeTypeVariableInstantiation {} ->
          False
        AllowStructuralPayloadInstantiation ->
          structuralPayloadsMayInstantiate typeBounds expectedName expectedBody actualName actualBody

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
  PrimitiveInventory.matchesBuiltinTypeName "IO" name

primitiveTypeToBackendType :: PrimitiveInventory.PrimitiveType -> BackendType
primitiveTypeToBackendType =
  \case
    PrimitiveInventory.PrimitiveTypeVar name -> BTVar name
    PrimitiveInventory.PrimitiveTypeArrow dom cod ->
      BTArrow (primitiveTypeToBackendType dom) (primitiveTypeToBackendType cod)
    PrimitiveInventory.PrimitiveTypeBase name ->
      BTBase (BaseTy name)
    PrimitiveInventory.PrimitiveTypeCon name args ->
      BTCon (BaseTy name) (fmap primitiveTypeToBackendType args)
    PrimitiveInventory.PrimitiveTypeForall name body ->
      BTForall name Nothing (primitiveTypeToBackendType body)

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
  unless (structuralDataDeclarationMatches typeBounds (constructorInfoDataDecl constructorInfo) substitution ty) $
    Left mismatchError

constructorInfoDataDecl :: BackendConstructorInfo -> BackendData
constructorInfoDataDecl constructorInfo =
  BackendData
    { backendDataName = bciDataName constructorInfo,
      backendDataParameters = bciDataParameters constructorInfo,
      backendDataConstructors = bciDataConstructors constructorInfo
    }

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

validateCaseAlternative :: BackendType -> BackendAlternative -> Either BackendValidationError ()
validateCaseAlternative resultTy alternative =
  unless (alphaEqBackendType (backendExprType (backendAltBody alternative)) resultTy) $
    Left (BackendCaseResultMismatch resultTy (backendExprType (backendAltBody alternative)))

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
