{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
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
* a program `main` carries the identity of one of those checked bindings;
* production variable, constructor, type-binder, and data-head references carry
  identities. Display/runtime names remain attached to those references, but
  validation and callable classification must not recover semantic references
  from String names after resolution;
* semantic lookup is identity-keyed throughout the backend; attached names are
  diagnostics and runtime/display spelling only;
* variable references resolve through lexical binder identities or the global
  identity binding table, with the carried type matching the binding;
* `BackendApp` is the direct first-order call node, so local direct aliases
  that remain first-order stay on this path and closure-valued heads violate a
  named backend callable invariant;
* callable-head classification destructures `BackendExpr` directly; the
  private `MLF.Backend.CallableShape` module owns only shared reference and
  result datatypes;
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
  ( BackendProgram
      ( BackendProgramWithIdentity,
        backendProgramModulesWithIdentity,
        backendProgramMainIdentity,
        backendProgramMainWithIdentity
      ),
    pattern BackendProgram,
    backendProgramModules,
    backendProgramMain,
    ProductionBackendProgram,
    mkProductionBackendProgram,
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
    BackendCallableBindingKind (..),
    BackendCallableHead (..),
    BackendValidationError (..),
    alphaEqBackendType,
    backendTypeHeadMatches,
    backendTypeRefinesScrutinee,
    typeBinderRefMatches,
    backendTermRefMatches,
    closureEntryRefMatches,
    freeBackendTypeVarKeys,
    generatedIdentitiesInBackendProgram,
    generatedIdentitiesInBackendTypes,
    generatedIdentitiesInBackendExpr,
    backendCallableHead,
    literalBackendType,
    substituteBackendTypeByIdentity,
    substituteBackendTypeForBinder,
    substituteBackendTypesByKey,
    unfoldBackendRecursiveType,
    validateBackendProgram,
    validateBackendBinding,
    validateBackendExpr,
    primitiveTypeToBackendType,
    primitiveTypeToBackendTypeFromWithHeadIdentities,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, zipWithM_)
import Data.List (sort, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Backend.CallableShape
import MLF.Backend.IR.Types
import MLF.Backend.StructuralRecursiveData
  ( BackendDataScope (..),
    BackendParameterBounds,
    alphaEqBackendType,
    backendDataScope,
    backendStructuralDataBoundaryMatches,
    completeBackendParameterSubstitution,
    isVacuousRecursiveBinderWithIdentity,
    matchBackendTypeParametersWithTypeBounds,
    structuralBackendHandlerFields,
    structuralDataArgumentSubstitution,
    structuralDataDeclarationMatches,
    structuralDataSelfFieldMatches,
    structuralMuPayloadTypes,
    structuralMuTypesHaveBinderIdentityMismatch,
    structuralPayloadsMayInstantiate,
    structuralRecursiveDataName,
    recursiveBodyCompatibleWithIdentity,
  )
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Program.Builtins (builtinValueIdentity)
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), lookupSymbolIdentityAlias, lookupSymbolIdentityExact, symbolDefiningModule, symbolDefiningName, symbolIdentityPayloadKey, symbolIdentityPayloadMatches, symbolIdentityStableName, symbolNamespace, symbolUniqueIdentity)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity
  ( DeferredRef,
    EnvRef,
    IdDetails (..),
    IdentityGenerator,
    LocalRef,
    ResolvedTermIdentityKey,
    StructuralTypeBinderRole (..),
    TypeBinderIdentity,
    UniqueIdentity,
    advanceIdentityGeneratorPastMany,
    freshIdentity,
    idDetailsIdentityKey,
    idDetailsIsLocal,
    idDetailsSameIdentity,
    idDetailsSymbolIdentity,
    initialIdentityGenerator,
    symbolGeneratedIdentities,
    typeBinderIdentityFromStructural,
    typeBinderIdentityFromUnique,
    typeBinderIdentityStableName,
    typeBinderIdentityStructural,
  )
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import qualified MLF.Primitive.Identity as PrimitiveIdentity
import MLF.Util.Names (freshNameLike)
import MLF.Backend.IR.Production.Internal
  ( ProductionBackendProgram,
    productionBackendProgramFromValidated,
  )

mkProductionBackendProgram :: BackendProgram -> Either BackendValidationError ProductionBackendProgram
mkProductionBackendProgram program = do
  validateBackendProgram program
  pure (productionBackendProgramFromValidated program)

data BackendValidationError
  = BackendDuplicateModule String
  | BackendDuplicateData String
  | BackendDuplicateDataParameter String String
  | BackendConstructorUnknownTypeVariable String String
  | BackendConflictingIdentityPayload String String
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
  | BackendClosureCalledWithBackendApp (Maybe String)
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
  | BackendNonLocalBinder String
  | BackendCaseConstructorScrutineeMismatch String BackendType BackendType
  | BackendCaseResultMismatch BackendType BackendType
  deriving (Eq, Show)

data BackendValidationContext = BackendValidationContext
  { bvcGlobals :: Map.Map SymbolIdentity BackendType,
    bvcData :: Map.Map SymbolIdentity BackendData,
    bvcConstructors :: Map.Map SymbolIdentity BackendConstructorInfo,
    bvcLocals :: Map.Map BackendLocalKey BackendType,
    bvcCasePatternLocals :: Set.Set BackendLocalKey,
    bvcClosureGlobals :: Set.Set SymbolIdentity,
    bvcClosureLocals :: Set.Set BackendLocalKey,
    bvcPossibleClosureLocals :: Set.Set BackendLocalKey,
    bvcTypeBounds :: BackendParameterBounds
  }

data BackendConstructorInfo = BackendConstructorInfo
  { bciDataIdentity :: SymbolIdentity,
    bciDataName :: String,
    bciDataParameterRefs :: [BackendDataParameterRef],
    bciDataConstructors :: [BackendConstructor],
    bciConstructor :: BackendConstructor
  }

typeBoundKeyNames :: BackendParameterBounds -> Set.Set String
typeBoundKeyNames =
  Set.map backendTypeSubstitutionKeyName . Map.keysSet

typeBoundReferenceKey :: TypeBinderIdentity -> BackendTypeSubstitutionKey
typeBoundReferenceKey identity =
  backendTypeSubstitutionKeyFromIdentity identity

data BackendLocalKey
  = BackendLocalRef LocalRef
  | BackendEnvRef EnvRef
  | BackendDeferredRef DeferredRef
  deriving (Eq, Ord, Show)

data TypeVariableInstantiation
  = RejectFreeTypeVariableInstantiation
  | AllowStructuralPayloadInstantiation
  deriving (Eq, Show)

backendClosureEntryNames :: BackendExpr -> [String]
backendClosureEntryNames =
  \case
    BackendVarWithIdentity {} -> []
    BackendLit {} -> []
    BackendLam _ _ _ body -> backendClosureEntryNames body
    BackendApp _ fun arg -> backendClosureEntryNames fun ++ backendClosureEntryNames arg
    BackendLet _ _ _ rhs body -> backendClosureEntryNames rhs ++ backendClosureEntryNames body
    BackendTyAbs _ _ _ body -> backendClosureEntryNames body
    BackendTyApp _ fun _ -> backendClosureEntryNames fun
    BackendConstructWithIdentity _ _ _ args -> concatMap backendClosureEntryNames args
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
validateBackendProgram =
  validateBackendProgramWith

validateBackendProgramWith :: BackendProgram -> Either BackendValidationError ()
validateBackendProgramWith program = do
  requireUnique BackendDuplicateModule (map backendModuleName modules0)
  requireUniqueSymbolIdentities "module" BackendDuplicateModule (map backendModuleIdentity modules0)
  requireUnique BackendDuplicateData (map backendDataName dataDecls)
  requireUniqueSymbolIdentities "data" BackendDuplicateData (map backendDataIdentity dataDecls)
  mapM_ validateBackendDataParameterIdentities dataDecls
  mapM_ validateBackendDataConstructorTypeVariables dataDecls
  requireUnique BackendDuplicateBinding (map backendBindingName bindings)
  requireUniqueSymbolIdentities "binding" BackendDuplicateBinding (map backendBindingIdentity bindings)
  requireUnique BackendDuplicateConstructor (map backendConstructorName constructors)
  requireUniqueSymbolIdentities "constructor" BackendDuplicateConstructor (map backendConstructorIdentity constructors)
  requireUnique BackendDuplicateClosureEntry closureEntryNames
  rejectClosureEntryNameCollisions closureEntryNames (map backendBindingName bindings ++ Map.keys runtimePrimitiveTypes)
  unless (backendProgramMainExists program bindings) $
    Left (BackendMainNotFound (backendProgramMain program))
  mapM_ (validateBackendBindingInContext context0) bindings
  where
    modules0 = backendProgramModules program
    dataDecls = concatMap backendModuleData modules0
    bindings = concatMap backendModuleBindings modules0
    constructors = concatMap backendDataConstructors dataDecls
    closureEntryNames = concatMap (backendClosureEntryNames . backendBindingExpr) bindings
    dataDeclsByIdentity =
      Map.fromList
        [ (identity, dataDecl)
        | dataDecl <- dataDecls,
          let identity = backendDataIdentity dataDecl
        ]
    runtimePrimitiveTypes =
      backendRuntimePrimitiveTypesWithHeadIdentities (preludePrimitiveDataHeadIdentities dataDeclsByIdentity)
    runtimePrimitiveTypesByIdentity =
      backendRuntimePrimitiveTypesByIdentityFrom runtimePrimitiveTypes
    constructorInfos =
      [ ( backendConstructorName constructor,
          BackendConstructorInfo
            (backendDataIdentity dataDecl)
            (backendDataName dataDecl)
            (backendDataParameterRefs dataDecl)
            (backendDataConstructors dataDecl)
            constructor
        )
        | dataDecl <- dataDecls,
          constructor <- backendDataConstructors dataDecl
      ]
    baseContext =
      BackendValidationContext
        { bvcGlobals =
            Map.fromList
              [ (backendBindingIdentity binding, backendBindingType binding)
              | binding <- bindings
              ]
              `Map.union` runtimePrimitiveTypesByIdentity,
          bvcData =
            Map.fromList
              [ (backendDataIdentity dataDecl, dataDecl)
              | dataDecl <- dataDecls
              ],
          bvcConstructors =
            Map.fromList
              [ (backendConstructorIdentity constructor, info)
              | (_, info@(BackendConstructorInfo {bciConstructor = constructor})) <- constructorInfos
              ],
          bvcLocals = Map.empty,
          bvcCasePatternLocals = Set.empty,
          bvcClosureGlobals = Set.empty,
          bvcClosureLocals = Set.empty,
          bvcPossibleClosureLocals = Set.empty,
          bvcTypeBounds = Map.empty
        }
    closureGlobals = backendClosureGlobals baseContext bindings
    context0 =
      baseContext
        { bvcClosureGlobals = closureGlobals
        }

validateBackendDataParameterIdentities :: BackendData -> Either BackendValidationError ()
validateBackendDataParameterIdentities dataDecl =
  requireUniqueBy
    (BackendDuplicateDataParameter (backendDataName dataDecl))
    [ (key, backendTypeSubstitutionKeyName key)
    | ref <- backendDataParameterRefs dataDecl,
      let key = backendDataParameterRefKey ref
    ]

validateBackendDataConstructorTypeVariables :: BackendData -> Either BackendValidationError ()
validateBackendDataConstructorTypeVariables dataDecl =
  mapM_ validateConstructor (backendDataConstructors dataDecl)
  where
    dataKeys =
      Set.fromList (backendDataParameterKeys dataDecl)

    validateConstructor constructor = do
      let forallKeys =
            Set.fromList
              [ backendTypeSubstitutionKeyFromIdentity (backendTypeBinderIdentity binder)
              | binder <- backendConstructorForalls constructor
              ]
          allowedKeys =
            Set.union dataKeys forallKeys
      mapM_ (validateMaybeType allowedKeys constructor . backendTypeBinderBound) (backendConstructorForalls constructor)
      mapM_ (validateType allowedKeys constructor) (backendConstructorFields constructor)
      validateType allowedKeys constructor (backendConstructorResult constructor)

    validateMaybeType _ _ Nothing =
      pure ()
    validateMaybeType allowedKeys constructor (Just ty) =
      validateType allowedKeys constructor ty

    validateType allowedKeys constructor ty =
      case [ key
           | key <- Set.toList (Set.difference (freeBackendTypeVarKeys ty) allowedKeys)
           ] of
        [] -> pure ()
        key : _ -> Left (BackendConstructorUnknownTypeVariable (backendConstructorName constructor) (backendTypeSubstitutionKeyName key))

backendDataScopeForContext :: BackendValidationContext -> BackendDataScope
backendDataScopeForContext context0 =
  backendDataScope (bvcData context0)

backendProgramMainExists :: BackendProgram -> [BackendBinding] -> Bool
backendProgramMainExists program bindings =
  any
    (symbolIdentityPayloadMatches (backendProgramMainIdentity program) . backendBindingIdentity)
    bindings

backendClosureGlobals :: BackendValidationContext -> [BackendBinding] -> Set.Set SymbolIdentity
backendClosureGlobals baseContext bindings =
  go Set.empty
  where
    go globals =
      let context0 =
            baseContext
              { bvcClosureGlobals = globals
              }
          closureBindings =
            [ binding
            | binding <- bindings,
              BackendClosureCallableHead _ <- [backendCallableHeadInContext (Just context0) (backendBindingExpr binding)]
            ]
          detectedGlobals =
            Set.fromList
              [ backendBindingIdentity binding
              | binding <- closureBindings
              ]
          globals' =
            globals <> detectedGlobals
       in if globals' == globals
            then globals
            else go globals'

backendRuntimePrimitiveTypes :: Map.Map String BackendType
backendRuntimePrimitiveTypes =
  backendRuntimePrimitiveTypesWithHeadIdentities Map.empty

backendRuntimePrimitiveTypesWithHeadIdentities :: Map.Map String SymbolIdentity -> Map.Map String BackendType
backendRuntimePrimitiveTypesWithHeadIdentities headIdentities =
  snd $
    Map.mapAccumWithKey
      ( \generator _name spec0 ->
          let (ty, generator') =
                primitiveTypeToBackendTypeFromWithHeadIdentities headIdentities generator (PrimitiveInventory.primitiveValueType spec0)
           in (generator', ty)
      )
      initialIdentityGenerator
      PrimitiveInventory.primitiveValueSpecs

backendRuntimePrimitiveTypesByIdentityFrom :: Map.Map String BackendType -> Map.Map SymbolIdentity BackendType
backendRuntimePrimitiveTypesByIdentityFrom runtimePrimitiveTypes =
  Map.fromList
    [ (builtinValueIdentity name, ty)
    | (name, ty) <- Map.toList runtimePrimitiveTypes
    ]

preludePrimitiveDataHeadIdentities :: Map.Map SymbolIdentity BackendData -> Map.Map String SymbolIdentity
preludePrimitiveDataHeadIdentities dataDeclsByIdentity =
  Map.fromList
    [ (symbolDefiningName identity, identity)
    | identity <- Map.keys dataDeclsByIdentity,
      symbolNamespace identity == SymbolType,
      symbolDefiningModule identity == "Prelude",
      symbolDefiningName identity `Set.member` preludePrimitiveDataTypeNames
    ]

preludePrimitiveDataTypeNames :: Set.Set String
preludePrimitiveDataTypeNames =
  Set.fromList ["List", "Nat", "Option", "Unit"]

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
  unless (backendApplicationTypeMatches mbContext (backendBindingType binding) (backendExprType expr)) $
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
    BackendVarWithIdentity resultTy mbIdentity name ->
      validateBackendVariable mbContext mbIdentity name resultTy
    BackendLit resultTy lit ->
      let expected = literalBackendType lit
       in unless (alphaEqBackendType resultTy expected) $
            Left (BackendLiteralTypeMismatch lit expected resultTy)
    BackendLamWithIdentity resultTy mbIdentity paramName paramTy body -> do
      requireLocalBinder paramName mbIdentity
      validateBackendExprWith (extendFunctionParamLocalMaybe mbContext mbIdentity paramName paramTy body) body
      let expected = BTArrow paramTy (backendExprType body)
      unless (backendApplicationTypeMatches mbContext expected resultTy) $
        Left (BackendLambdaTypeMismatch resultTy expected)
    BackendApp resultTy fun arg -> do
      validateBackendExprWith mbContext fun
      validateBackendExprWith mbContext arg
      case backendCallableHeadInContext mbContext fun of
        BackendClosureCallableHead mbRef ->
          Left (BackendClosureCalledWithBackendApp (backendCallableRefName <$> mbRef))
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
    BackendLetWithIdentity resultTy mbIdentity name bindingTy rhs body -> do
      requireLocalBinder name mbIdentity
      validateBackendExprWith mbContext rhs
      unless (alphaEqBackendType (backendExprType rhs) bindingTy) $
        Left (BackendLetTypeMismatch name bindingTy (backendExprType rhs))
      validateBackendExprWith (extendLetLocalMaybe mbContext mbIdentity bindingTy rhs) body
      unless (backendApplicationTypeMatches mbContext resultTy (backendExprType body)) $
        Left (BackendLetBodyTypeMismatch resultTy (backendExprType body))
    BackendTyAbsWithIdentity resultTy mbIdentity name mbBound body -> do
      validateBackendExprWith (extendTypeBoundMaybe mbContext mbIdentity mbBound) body
      let expected = BTForallWithIdentity mbIdentity name mbBound (backendExprType body)
      unless (backendApplicationTypeMatches mbContext resultTy expected) $
        Left (BackendTypeAbsTypeMismatch name resultTy expected)
    BackendTyApp resultTy fun tyArg -> do
      validateBackendExprWith mbContext fun
      case backendExprType fun of
        BTForallWithIdentity mbIdentity _ mbBound bodyTy -> do
          validateBackendTypeArgumentBound mbContext mbBound tyArg
          let expected = substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFromIdentity mbIdentity) tyArg) bodyTy
          unless (alphaEqBackendType resultTy expected) $
            Left (BackendTypeAppResultMismatch resultTy expected)
        other ->
          Left (BackendTypeAppExpectedForall other)
    BackendConstructWithIdentity resultTy mbIdentity name args -> do
      validateBackendConstructorUse mbContext mbIdentity name resultTy args
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
    BackendClosureWithParamIdentities resultTy _ entryName captures params body -> do
      mapM_ (\capture -> requireLocalBinder (backendClosureCaptureName capture) (backendClosureCaptureIdentity capture)) captures
      mapM_ (\param -> requireLocalBinder (backendClosureParamName param) (backendClosureParamIdentity param)) params
      requireUniqueBy BackendDuplicateClosureCapture (map closureCaptureBinderRef captures)
      requireUniqueBy BackendDuplicateClosureParameter (map closureParamBinderRef params)
      requireUniqueBy BackendDuplicateClosureParameter (map closureCaptureBinderRef captures ++ map closureParamBinderRef params)
      mapM_ (validateBackendClosureCapture mbContext) captures
      let bodyContext =
            foldl
              (extendClosureCaptureLocalMaybe mbContext)
              (dropTermLocalsMaybe mbContext)
              captures
          bodyParamContext =
            foldl
              (\context0 param -> extendFunctionParamLocalMaybe context0 (backendClosureParamIdentity param) (backendClosureParamName param) (backendClosureParamType param) body)
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

requireLocalBinder :: String -> IdDetails -> Either BackendValidationError ()
requireLocalBinder name details =
  unless (idDetailsIsLocal details) (Left (BackendNonLocalBinder name))

validateBackendClosureFunctionType :: String -> BackendType -> [BackendClosureParam] -> BackendType -> Either BackendValidationError ()
validateBackendClosureFunctionType entryName resultTy params bodyTy =
  case collectClosureCallType resultTy of
    Nothing ->
      Left (BackendClosureExpectedFunction entryName resultTy)
    Just (declaredParamTys, declaredResultTy) -> do
      unless (length declaredParamTys == length params) $
        Left (BackendClosureParameterArityMismatch entryName (length params) (length declaredParamTys))
      let paramTys = map backendClosureParamType params
          expected = foldr BTArrow bodyTy paramTys
      unless (and (zipWith alphaEqBackendType declaredParamTys paramTys) && alphaEqBackendType declaredResultTy bodyTy) $
        Left (BackendClosureTypeMismatch entryName resultTy expected)

backendCallableHead :: (IdDetails -> BackendCallableBindingKind) -> BackendExpr -> BackendCallableHead
backendCallableHead resolve0 =
  go resolve0
  where
    go resolve =
      \case
        BackendVarWithIdentity _ mbIdentity name ->
          case resolve mbIdentity of
            BackendCallableBindingDirect ->
              BackendDirectCallableHead (Just (backendCallableRef mbIdentity name))
            BackendCallableBindingClosure ->
              BackendClosureCallableHead (Just (backendCallableRef mbIdentity name))
            BackendCallableBindingUnknown ->
              BackendUnknownCallableHead
        BackendLam {} ->
          BackendDirectCallableHead Nothing
        BackendClosureWithParamIdentities _ entryIdentity entryName _ _ _ ->
          BackendClosureCallableHead (Just (backendCallableClosureRef entryIdentity entryName))
        BackendTyAbs _ _ _ body ->
          go resolve body
        BackendTyApp _ fun _ ->
          go resolve fun
        BackendLetWithIdentity _ mbIdentity _ _ rhs body ->
          go (extendBindingKind resolve mbIdentity (go resolve rhs)) body
        BackendCase _ _ alternatives ->
          collapseCallableHeads
            ( fmap
                ( \alternative ->
                    let binders = patternBinderDetails (backendAltPattern alternative)
                        body = backendAltBody alternative
                        closureBinders =
                          filter (\binder -> backendExprMentionsBindingWithCallableType binder body) binders
                     in go (extendPatternBindingKinds binders closureBinders resolve) body
                )
                alternatives
            )
        _ ->
          BackendUnknownCallableHead

    extendBindingKind resolve bindingIdentity headShape localIdentity
      | idDetailsSameIdentity bindingIdentity localIdentity =
          callableBindingKindForHead headShape
      | otherwise =
          resolve localIdentity

    extendPatternBindingKinds binders closureBinders resolve localIdentity
      | any (callableBinderMatches localIdentity) closureBinders =
          BackendCallableBindingClosure
      | any (callableBinderMatches localIdentity) binders =
          BackendCallableBindingDirect
      | otherwise =
          resolve localIdentity

callableBinderMatches :: IdDetails -> BackendCallableRef -> Bool
callableBinderMatches localIdentity binder =
  maybe False (idDetailsSameIdentity localIdentity) (backendCallableRefIdentity binder)

callableBindingKindForHead :: BackendCallableHead -> BackendCallableBindingKind
callableBindingKindForHead =
  \case
    BackendDirectCallableHead _ -> BackendCallableBindingDirect
    BackendClosureCallableHead _ -> BackendCallableBindingClosure
    BackendUnknownCallableHead -> BackendCallableBindingUnknown

collapseCallableHeads :: NonEmpty BackendCallableHead -> BackendCallableHead
collapseCallableHeads heads
  | all isClosureHead heads = BackendClosureCallableHead (sameClosureHeadRef heads)
  | all isDirectHead heads = BackendDirectCallableHead (sameDirectHeadRef heads)
  | otherwise = BackendUnknownCallableHead
  where
    isClosureHead BackendClosureCallableHead {} = True
    isClosureHead _ = False

    isDirectHead BackendDirectCallableHead {} = True
    isDirectHead _ = False

sameClosureHeadRef :: NonEmpty BackendCallableHead -> Maybe BackendCallableRef
sameClosureHeadRef heads =
  case traverse closureHeadRef heads of
    Just (ref :| rest)
      | all (backendCallableRefMatches ref) rest -> Just ref
    _ -> Nothing
  where
    closureHeadRef =
      \case
        BackendClosureCallableHead mbRef -> mbRef
        _ -> Nothing

sameDirectHeadRef :: NonEmpty BackendCallableHead -> Maybe BackendCallableRef
sameDirectHeadRef heads =
  case traverse directHeadRef heads of
    Just (ref :| rest)
      | all (directHeadRefMatches ref) rest -> ref
    _ -> Nothing
  where
    directHeadRef =
      \case
        BackendDirectCallableHead ref -> Just ref
        _ -> Nothing

directHeadRefMatches :: Maybe BackendCallableRef -> Maybe BackendCallableRef -> Bool
directHeadRefMatches (Just left) (Just right) =
  backendCallableRefMatches left right
directHeadRefMatches Nothing Nothing =
  True
directHeadRefMatches _ _ =
  False

backendCallableHeadInContext :: Maybe BackendValidationContext -> BackendExpr -> BackendCallableHead
backendCallableHeadInContext mbContext =
  backendCallableHead (backendCallableBindingKindInContext mbContext)

backendCallableBindingKindInContext :: Maybe BackendValidationContext -> IdDetails -> BackendCallableBindingKind
backendCallableBindingKindInContext Nothing _ =
  BackendCallableBindingUnknown
backendCallableBindingKindInContext (Just context0) details
  | Just key <- idDetailsLocalKey details =
      maybe BackendCallableBindingUnknown id (lookupLocalCallableBindingKind context0 key)
  | Just identity <- idDetailsSymbolIdentity details =
      maybe BackendCallableBindingUnknown id (lookupGlobalCallableBindingKind context0 identity)
  | otherwise = BackendCallableBindingUnknown

lookupLocalCallableBindingKind :: BackendValidationContext -> BackendLocalKey -> Maybe BackendCallableBindingKind
lookupLocalCallableBindingKind context0 key
  | Set.member key (bvcClosureLocals context0) =
      Just BackendCallableBindingClosure
  | Set.member key (bvcPossibleClosureLocals context0) =
      Just BackendCallableBindingUnknown
  | Map.member key (bvcLocals context0) =
      Just BackendCallableBindingDirect
  | otherwise =
      Nothing

lookupGlobalCallableBindingKind :: BackendValidationContext -> SymbolIdentity -> Maybe BackendCallableBindingKind
lookupGlobalCallableBindingKind context0 key
  | Set.member key (bvcClosureGlobals context0) =
      Just BackendCallableBindingClosure
  | Map.member key (bvcGlobals context0) =
      Just BackendCallableBindingDirect
  | otherwise =
      Nothing

backendExprMentionsBindingWithCallableType :: BackendBinderRef -> BackendExpr -> Bool
backendExprMentionsBindingWithCallableType needle =
  go
  where
    go =
      \case
        BackendVarWithIdentity ty mbIdentity name ->
          backendBinderMatches needle (backendCallableRef mbIdentity name) && backendTypeIsClosureValue ty
        BackendLit {} ->
          False
        BackendLamWithIdentity _ mbIdentity name _ body
          | backendBinderMatches needle (backendCallableRef mbIdentity name) -> False
          | otherwise -> go body
        BackendApp _ fun arg ->
          go fun || go arg
        BackendLetWithIdentity _ mbIdentity name _ rhs body
          | backendBinderMatches needle (backendCallableRef mbIdentity name) -> go rhs
          | otherwise -> go rhs || go body
        BackendTyAbs _ _ _ body ->
          go body
        BackendTyApp ty (BackendVarWithIdentity _ mbIdentity name) _
          | backendBinderMatches needle (backendCallableRef mbIdentity name),
            backendTypeIsClosureValue ty ->
              True
        BackendTyApp _ fun _ ->
          go fun
        BackendConstructWithIdentity _ _ _ args ->
          any go args
        BackendCase _ scrutinee alternatives ->
          go scrutinee || any goAlternative (NE.toList alternatives)
        BackendRoll _ payload ->
          go payload
        BackendUnroll _ payload ->
          go payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          any (go . backendClosureCaptureExpr) captures
            || (not (any (backendBinderMatches needle) closureBinders) && go body)
          where
            closureBinders =
              [backendCallableRef (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture) | capture <- captures]
                ++ [backendCallableRef (backendClosureParamIdentity param) (backendClosureParamName param) | param <- params]
        BackendClosureCall _ fun args ->
          go fun || any go args

    goAlternative (BackendAlternative pattern0 body)
      | any (backendBinderMatches needle) (patternBinderDetails pattern0) = False
      | otherwise = go body

backendBinderMatches :: BackendBinderRef -> BackendBinderRef -> Bool
backendBinderMatches =
  backendCallableRefMatches

type BackendBinderRef = BackendCallableRef

backendExprCallsBinderAsClosureHead :: BackendBinderRef -> BackendExpr -> Bool
backendExprCallsBinderAsClosureHead needle =
  go [needle]
  where
    go aliases =
      \case
        BackendVarWithIdentity {} ->
          False
        BackendLit {} ->
          False
        BackendLamWithIdentity _ mbIdentity name _ body
          | any (backendBinderMatches (backendCallableRef mbIdentity name)) aliases -> False
          | otherwise -> go aliases body
        BackendApp _ fun arg ->
          go aliases fun || go aliases arg
        BackendLetWithIdentity _ mbIdentity name _ rhs body
          | any (backendBinderMatches (backendCallableRef mbIdentity name)) aliases -> go aliases rhs
          | otherwise ->
              let aliasesForBody =
                    if closureCallHeadReferencesAny aliases rhs
                      then insertBackendBinderAlias (backendCallableRef mbIdentity name) aliases
                      else aliases
               in go aliases rhs || go aliasesForBody body
        BackendTyAbs _ _ _ body ->
          go aliases body
        BackendTyApp _ fun _ ->
          go aliases fun
        BackendConstructWithIdentity _ _ _ args ->
          any (go aliases) args
        BackendCase _ scrutinee alternatives ->
          go aliases scrutinee || any (goAlternative aliases) (NE.toList alternatives)
        BackendRoll _ payload ->
          go aliases payload
        BackendUnroll _ payload ->
          go aliases payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          any (go aliases . backendClosureCaptureExpr) captures
            || (backendBindersDisjoint aliases closureParamBinders && go aliasesForBody body)
          where
            closureParamBinders =
              [backendCallableRef (backendClosureParamIdentity param) (backendClosureParamName param) | param <- params]
            aliasesForBody =
              foldr (insertBackendBinderAlias) aliases capturedAliases
            capturedAliases =
              [ captureBinder
              | capture <- captures,
                let captureBinder = backendCallableRef (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture),
                any (\alias -> backendExprReferencesBinding alias (backendClosureCaptureExpr capture)) aliases
              ]
        BackendClosureCall _ fun args ->
          closureCallHeadReferencesAny aliases fun || go aliases fun || any (go aliases) args

    goAlternative aliases (BackendAlternative pattern0 body)
      | not (backendBindersDisjoint aliases (patternBinderDetails pattern0)) = False
      | otherwise = go aliases body

insertBackendBinderAlias :: BackendBinderRef -> [BackendBinderRef] -> [BackendBinderRef]
insertBackendBinderAlias alias aliases =
  alias : filter (not . backendBinderMatches alias) aliases

backendBindersDisjoint :: [BackendBinderRef] -> [BackendBinderRef] -> Bool
backendBindersDisjoint left right =
  not (any (\leftBinder -> any (backendBinderMatches leftBinder) right) left)

closureCallHeadReferencesAny :: [BackendBinderRef] -> BackendExpr -> Bool
closureCallHeadReferencesAny aliases0 =
  \case
    BackendVarWithIdentity _ mbIdentity name ->
      any (backendBinderMatches (backendCallableRef mbIdentity name)) aliases0
    BackendTyApp _ fun _ ->
      closureCallHeadReferencesAny aliases0 fun
    BackendLetWithIdentity _ mbIdentity name _ rhs body ->
      let binder = backendCallableRef mbIdentity name
          aliasesWithoutShadow =
            filter (not . backendBinderMatches binder) aliases0
          aliasesForBody =
            if closureCallHeadReferencesAny aliases0 rhs
              then insertBackendBinderAlias binder aliasesWithoutShadow
              else aliasesWithoutShadow
       in closureCallHeadReferencesAny aliasesForBody body
    _ ->
      False

backendExprReferencesBinding :: BackendBinderRef -> BackendExpr -> Bool
backendExprReferencesBinding needle =
  go
  where
    go =
      \case
        BackendVarWithIdentity _ mbIdentity name ->
          backendBinderMatches needle (backendCallableRef mbIdentity name)
        BackendLit {} ->
          False
        BackendLamWithIdentity _ mbIdentity name _ body
          | backendBinderMatches needle (backendCallableRef mbIdentity name) -> False
          | otherwise -> go body
        BackendApp _ fun arg ->
          go fun || go arg
        BackendLetWithIdentity _ mbIdentity name _ rhs body
          | backendBinderMatches needle (backendCallableRef mbIdentity name) -> go rhs
          | otherwise -> go rhs || go body
        BackendTyAbs _ _ _ body ->
          go body
        BackendTyApp _ fun _ ->
          go fun
        BackendConstructWithIdentity _ _ _ args ->
          any go args
        BackendCase _ scrutinee alternatives ->
          go scrutinee || any goAlternative (NE.toList alternatives)
        BackendRoll _ payload ->
          go payload
        BackendUnroll _ payload ->
          go payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          any (go . backendClosureCaptureExpr) captures
            || (not (any (backendBinderMatches needle) closureBinders) && go body)
          where
            closureBinders =
              [backendCallableRef (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture) | capture <- captures]
                ++ [backendCallableRef (backendClosureParamIdentity param) (backendClosureParamName param) | param <- params]
        BackendClosureCall _ fun args ->
          go fun || any go args

    goAlternative (BackendAlternative pattern0 body)
      | any (backendBinderMatches needle) (patternBinderDetails pattern0) = False
      | otherwise = go body

patternBinderDetails :: BackendPattern -> [BackendBinderRef]
patternBinderDetails =
  \case
    BackendDefaultPattern -> []
    BackendConstructorPatternWithBinderIdentities _ _ binders ->
      [backendCallableRef (backendPatternBinderIdentity binder) (backendPatternBinderName binder) | binder <- binders]

validateBackendClosureCall :: Maybe BackendValidationContext -> BackendType -> BackendExpr -> [BackendExpr] -> Either BackendValidationError ()
validateBackendClosureCall mbContext resultTy fun args =
  case collectClosureCallType funTy of
    Nothing ->
      Left (BackendClosureCallExpectedFunction funTy)
    Just (paramTys, expectedResultTy) -> do
      case backendCallableHeadInContext mbContext fun of
        BackendClosureCallableHead _ ->
          pure ()
        BackendDirectCallableHead (Just ref) ->
          Left (BackendDirectCalledWithBackendClosureCall (backendCallableRefName ref))
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

validateBackendVariable :: Maybe BackendValidationContext -> IdDetails -> String -> BackendType -> Either BackendValidationError ()
validateBackendVariable Nothing _ _ _ =
  pure ()
validateBackendVariable (Just context0) identity name actualTy =
  case lookupBackendVariable context0 identity of
    Nothing ->
      Left (BackendUnknownVariable name)
    Just expectedTy -> do
      unless (backendVariableTypeMatches context0 identity expectedTy actualTy) $
        Left (BackendVariableTypeMismatch name expectedTy actualTy)

backendApplicationTypeMatches :: Maybe BackendValidationContext -> BackendType -> BackendType -> Bool
backendApplicationTypeMatches mbContext expectedTy actualTy =
  matches expectedTy actualTy
    || (not (backendTypeContainsMu expectedTy || backendTypeContainsMu actualTy) && matches expectedTy' actualTy')
  where
    typeBounds = maybe Map.empty bvcTypeBounds mbContext
    dataScope = backendDataScopeForContext <$> mbContext
    expectedTy' = maybe expectedTy (`canonicalizeBackendTypeDataHeads` expectedTy) mbContext
    actualTy' = maybe actualTy (`canonicalizeBackendTypeDataHeads` actualTy) mbContext
    matches expected actual =
      typeMatches expected actual
        || typeMatches actual expected
        || ( not (identityBearingNominalStructuralBoundary expected actual)
               && backendStructuralDataBoundaryMatches
                 typeBounds
                 dataScope
                 expected
                 actual
           )

    typeMatches =
      backendTypeMatchesWith AllowStructuralPayloadInstantiation typeBounds dataScope

backendTypeContainsMu :: BackendType -> Bool
backendTypeContainsMu =
  \case
    BTVarWithIdentity {} -> False
    BTArrow dom cod -> backendTypeContainsMu dom || backendTypeContainsMu cod
    BTBaseWithIdentity {} -> False
    BTConWithIdentity _ _ args -> any backendTypeContainsMu args
    BTVarAppWithIdentity _ _ args -> any backendTypeContainsMu args
    BTForallWithIdentity _ _ mbBound body -> maybe False backendTypeContainsMu mbBound || backendTypeContainsMu body
    BTMuWithIdentity {} -> True
    BTBottom -> False

identityBearingNominalStructuralBoundary :: BackendType -> BackendType -> Bool
identityBearingNominalStructuralBoundary expected actual =
  case (expected, actual) of
    (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
      identityBearingNominalStructuralBoundary expectedDom actualDom
        || identityBearingNominalStructuralBoundary expectedCod actualCod
    (BTConWithIdentity _ _ expectedArgs, BTConWithIdentity _ _ actualArgs) ->
      or (zipWith identityBearingNominalStructuralBoundary (NE.toList expectedArgs) (NE.toList actualArgs))
    (BTVarAppWithIdentity _ _ expectedArgs, BTVarAppWithIdentity _ _ actualArgs) ->
      or (zipWith identityBearingNominalStructuralBoundary (NE.toList expectedArgs) (NE.toList actualArgs))
    (BTForallWithIdentity _ _ expectedBound expectedBody, BTForallWithIdentity _ _ actualBound actualBody) ->
      maybe False (uncurry identityBearingNominalStructuralBoundary) ((,) <$> expectedBound <*> actualBound)
        || identityBearingNominalStructuralBoundary expectedBody actualBody
    (BTMuWithIdentity _ _ expectedBody, BTMuWithIdentity _ _ actualBody) ->
      identityBearingNominalStructuralBoundary expectedBody actualBody
    (BTBaseWithIdentity {}, BTMuWithIdentity {}) -> True
    (BTMuWithIdentity {}, BTBaseWithIdentity {}) -> True
    (BTConWithIdentity {}, BTMuWithIdentity {}) -> True
    (BTMuWithIdentity {}, BTConWithIdentity {}) -> True
    _ -> False

backendVariableTypeMatches :: BackendValidationContext -> IdDetails -> BackendType -> BackendType -> Bool
backendVariableTypeMatches context0 identity expectedTy actualTy =
  rawMatches || canonicalMatches
  where
    dataScope = backendDataScopeForContext context0
    typeBounds = bvcTypeBounds context0
    rawMatches =
      backendTypeMatchesWith
        RejectFreeTypeVariableInstantiation
        typeBounds
        (Just dataScope)
        expectedTy
        actualTy
        || backendTypeMatchesWith
          AllowStructuralPayloadInstantiation
          typeBounds
          (Just dataScope)
          expectedTy
          actualTy
        || ( not (identityBearingNominalStructuralBoundary expectedTy actualTy)
               && backendStructuralDataBoundaryMatches
                 typeBounds
                 (Just dataScope)
                 expectedTy
                 actualTy
           )
        || backendApplicationTypeMatches (Just context0) expectedTy actualTy
        || generatedCasePatternVariableTypeMatches context0 identity expectedTy
        || primitiveRuntimeVariableTypeMatches
          identity
          expectedTy
          actualTy
    canonicalMatches =
      let expectedTy' = canonicalizeBackendTypeDataHeads context0 expectedTy
          actualTy' = canonicalizeBackendTypeDataHeads context0 actualTy
       in backendTypeMatchesWith
            RejectFreeTypeVariableInstantiation
            typeBounds
            (Just dataScope)
            expectedTy'
            actualTy'
            || backendTypeMatchesWith
              AllowStructuralPayloadInstantiation
              typeBounds
              (Just dataScope)
              expectedTy'
              actualTy'
            || ( not (identityBearingNominalStructuralBoundary expectedTy' actualTy')
                   && backendStructuralDataBoundaryMatches
                     typeBounds
                     (Just dataScope)
                     expectedTy'
                     actualTy'
               )
            || generatedCasePatternVariableTypeMatches context0 identity expectedTy'
            || primitiveRuntimeVariableTypeMatches
              identity
              expectedTy'
              actualTy'

generatedCasePatternVariableTypeMatches :: BackendValidationContext -> IdDetails -> BackendType -> Bool
generatedCasePatternVariableTypeMatches context0 identityDetails expectedTy =
  case (idDetailsLocalKey identityDetails, expectedTy) of
    (Just localKey, BTVarWithIdentity identity _)
      | Set.member localKey (bvcCasePatternLocals context0) ->
          not (hasConcreteTypeBound (typeBoundReferenceKey identity))
    _ ->
      False
  where
    typeBounds = bvcTypeBounds context0
    hasConcreteTypeBound key =
      case Map.lookup key typeBounds of
        Just (Just boundTy) -> not (alphaEqBackendType boundTy BTBottom)
        _ -> False

primitiveRuntimeVariableTypeMatches :: IdDetails -> BackendType -> BackendType -> Bool
primitiveRuntimeVariableTypeMatches identity expectedTy actualTy
  | primitiveRuntimeVariableReference identity =
      go expectedTy actualTy
  | otherwise =
      False
  where
    primitiveRuntimeVariableReference details =
      case idDetailsSymbolIdentity details >>= PrimitiveInventory.primitiveValueNameByIdentity of
        Just primitiveName -> Map.member primitiveName backendRuntimePrimitiveTypes
        Nothing -> False

    go expected actual
      | structuralMuTypesHaveBinderIdentityMismatch expected actual =
          False
      | otherwise =
          alphaEqBackendType expected actual
            || case (expected, actual) of
              (_, BTVarWithIdentity {}) ->
                True
              (BTVarWithIdentity {}, _) ->
                True
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                go expectedDom actualDom && go expectedCod actualCod
              (BTBaseWithIdentity expectedIdentity _, BTBaseWithIdentity actualIdentity _) ->
                backendPrimitiveTypeHeadMatches expectedIdentity actualIdentity
              (BTConWithIdentity expectedIdentity _ expectedArgs, BTConWithIdentity actualIdentity _ actualArgs) ->
                backendPrimitiveTypeHeadMatches expectedIdentity actualIdentity
                  && zipAllWith go (NE.toList expectedArgs) (NE.toList actualArgs)
              (BTMuWithIdentity expectedIdentity _ expectedBody, BTMuWithIdentity actualIdentity _ actualBody) ->
                structuralPrimitiveMuMatches expectedIdentity expectedBody actualIdentity actualBody
              (BTMuWithIdentity expectedIdentity _ expectedBody, BTConWithIdentity actualIdentity _ actualArgs) ->
                structuralPrimitiveTypeMatches expectedIdentity expectedBody actualIdentity (NE.toList actualArgs)
              (BTConWithIdentity expectedIdentity _ expectedArgs, BTMuWithIdentity actualIdentity _ actualBody) ->
                structuralPrimitiveTypeMatches actualIdentity actualBody expectedIdentity (NE.toList expectedArgs)
              _ ->
                False

    structuralPrimitiveMuMatches expectedIdentity expectedBody actualIdentity actualBody =
      structuralPrimitiveMuOwnersMatch expectedIdentity actualIdentity
        && case (structuralPrimitivePayloadTypes expectedIdentity expectedBody, structuralPrimitivePayloadTypes actualIdentity actualBody) of
          (Just expectedPayloadTypes, Just actualPayloadTypes) ->
            zipAllWith go expectedPayloadTypes actualPayloadTypes
          _ ->
            False

    backendPrimitiveTypeHeadMatches expectedIdentity actualIdentity =
      backendTypeHeadMatches expectedIdentity actualIdentity

    structuralPrimitiveTypeMatches muIdentity muBody dataIdentity args =
      structuralPrimitiveDataOwnerMatches muIdentity dataIdentity
        && case structuralPrimitivePayloadTypes muIdentity muBody of
          Just payloadTypes -> zipAllWith go payloadTypes args
          Nothing -> False

    structuralPrimitiveMuOwnersMatch leftIdentity rightIdentity =
      case (structuralSelfIdentityUnique leftIdentity, structuralSelfIdentityUnique rightIdentity) of
        (Just leftOwner, Just rightOwner) -> leftOwner == rightOwner
        _ -> False

    structuralPrimitiveDataOwnerMatches muIdentity dataIdentity =
      case structuralSelfIdentityUnique muIdentity of
        Just structuralOwner -> structuralOwner == symbolUniqueIdentity dataIdentity
        _ -> False

    structuralPrimitivePayloadTypes muIdentity body =
      filter (not . structuralSelfField muIdentity) <$> structuralMuPayloadTypes body

    structuralSelfField muIdentity =
      \case
        BTVarWithIdentity fieldIdentity _ ->
          typeBinderRefMatches muIdentity fieldIdentity
        _ ->
          False

backendVariableTypeMatchesWithBounds :: BackendParameterBounds -> BackendType -> BackendType -> Bool
backendVariableTypeMatchesWithBounds typeBounds expectedTy actualTy =
  backendTypeMatchesWith
    RejectFreeTypeVariableInstantiation
    typeBounds
    Nothing
    expectedTy
    actualTy

backendTypeMatchesWith ::
  TypeVariableInstantiation ->
  BackendParameterBounds ->
  Maybe BackendDataScope ->
  BackendType ->
  BackendType ->
  Bool
backendTypeMatchesWith typeVariableInstantiation typeBounds mbDataDecls expectedTy actualTy =
  go Set.empty expectedTy actualTy
  where
    typeHeadMatches =
      backendTypeHeadMatches

    typeBinderMatches =
      typeBinderRefMatches

    go bound expected actual =
      alphaEqWithinDataScope actual expected
        || typeVariableBoundMatches bound expected actual
        || typeVariableBoundMatches bound actual expected
        || case (expected, actual) of
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod)
                | opaqueIOFunctionCompatible bound expectedDom expectedCod actualDom actualCod ->
                    True
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                go bound expectedDom actualDom && go bound expectedCod actualCod
              (BTBaseWithIdentity expectedIdentity _, BTBaseWithIdentity actualIdentity _) ->
                typeHeadMatches expectedIdentity actualIdentity
              (BTBaseWithIdentity expectedDataIdentity _, BTMuWithIdentity actualIdentity _ actualBody) ->
                structuralMuMatchesKnownData expectedDataIdentity [] actualIdentity actualBody
              (BTMuWithIdentity expectedIdentity _ expectedBody, BTBaseWithIdentity actualDataIdentity _) ->
                structuralMuMatchesKnownData actualDataIdentity [] expectedIdentity expectedBody
              (BTConWithIdentity expectedIdentity _ (_ :| []), BTConWithIdentity actualIdentity _ (_ :| []))
                | opaqueIOBackendHeadMatches expectedIdentity && opaqueIOBackendHeadMatches actualIdentity ->
                    True
              (BTConWithIdentity expectedIdentity _ expectedArgs, BTConWithIdentity actualIdentity _ actualArgs) ->
                typeHeadMatches expectedIdentity actualIdentity
                  && zipAllWith
                    (metadataBackedTypeArgumentMatchesEither (metadataBackedTypeHead expectedIdentity) bound)
                    (NE.toList expectedArgs)
                    (NE.toList actualArgs)
              (BTConWithIdentity expectedDataIdentity _ expectedArgs, BTMuWithIdentity actualIdentity _ actualBody) ->
                nominalStructuralTypeVarArgsMatch expectedDataIdentity (NE.toList expectedArgs) actualIdentity
                  || structuralMuMatchesKnownData expectedDataIdentity (NE.toList expectedArgs) actualIdentity actualBody
              (BTMuWithIdentity expectedIdentity _ expectedBody, BTConWithIdentity actualDataIdentity _ actualArgs) ->
                nominalStructuralTypeVarArgsMatch actualDataIdentity (NE.toList actualArgs) expectedIdentity
                  || structuralMuMatchesKnownData actualDataIdentity (NE.toList actualArgs) expectedIdentity expectedBody
              (BTVarAppWithIdentity expectedIdentity _ expectedArgs, BTVarAppWithIdentity actualIdentity _ actualArgs) ->
                typeBinderMatches expectedIdentity actualIdentity
                  && zipAllWith (go bound) (NE.toList expectedArgs) (NE.toList actualArgs)
              (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity actualName actualBound actualBody) ->
                maybeBoundMatches bound expectedBound actualBound
                  && let freshName = freshBinderName expectedName actualName expectedBound actualBound expectedBody actualBody
                         freshTy = freshBinderTy expectedIdentity freshName
                         freshKey = freshBinderKey expectedIdentity
                         expectedBody' = substituteBackendTypeForBinder expectedIdentity freshTy expectedBody
                         actualBody' = substituteBackendTypeForBinder actualIdentity freshTy actualBody
                      in go (Set.insert freshKey bound) expectedBody' actualBody'
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
                let freshName = freshBinderName expectedName actualName Nothing Nothing expectedBody actualBody
                    freshTy = freshBinderTy expectedIdentity freshName
                    freshKey = freshBinderKey expectedIdentity
                    expectedBody' = substituteBackendTypeForBinder expectedIdentity freshTy expectedBody
                    actualBody' = substituteBackendTypeForBinder actualIdentity freshTy actualBody
                    bodiesMatch = go (Set.insert freshKey bound) expectedBody' actualBody'
                 in if sameStructuralDataOwner expectedIdentity actualIdentity
                  then
                    (typeBinderMatches expectedIdentity actualIdentity && bodiesMatch)
                      || structuralMuPayloadMayInstantiate expectedIdentity expectedName expectedBody actualIdentity actualName actualBody
                  else case (isVacuousRecursiveBinderWithIdentity expectedIdentity expectedBody, isVacuousRecursiveBinderWithIdentity actualIdentity actualBody) of
                    (True, True) ->
                      go bound expectedBody actualBody
                    (True, False) ->
                      vacuousRecursiveWrapperMayUnwrap expectedBody
                        && (recursiveBodyCompatibleWithIdentity actualIdentity actualBody expectedBody || go bound expectedBody actual)
                    (False, True) ->
                      vacuousRecursiveWrapperMayUnwrap actualBody
                        && (recursiveBodyCompatibleWithIdentity expectedIdentity expectedBody actualBody || go bound expected actualBody)
                    (False, False) ->
                      bodiesMatch
              (BTMuWithIdentity expectedIdentity _ expectedBody, _)
                | isVacuousRecursiveBinderWithIdentity expectedIdentity expectedBody,
                  vacuousRecursiveWrapperMayUnwrap expectedBody ->
                    go bound expectedBody actual
              (_, BTMuWithIdentity actualIdentity _ actualBody)
                | isVacuousRecursiveBinderWithIdentity actualIdentity actualBody,
                  vacuousRecursiveWrapperMayUnwrap actualBody ->
                    go bound expected actualBody
              (BTBottom, BTBottom) ->
                True
              _ ->
                False

    alphaEqWithinDataScope expected actual =
      alphaEqBackendType expected actual
        && not (structuralMuTypesHaveBinderIdentityMismatch expected actual)
        && not (identityHeadNeedsScopedData expected actual)

    identityHeadNeedsScopedData expected actual =
      identityBearingNominalStructuralBoundary expected actual
        || case (expected, actual, mbDataDecls) of
          (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod, _) ->
            identityHeadNeedsScopedData expectedDom actualDom
              || identityHeadNeedsScopedData expectedCod actualCod
          (BTConWithIdentity _ _ expectedArgs, BTConWithIdentity _ _ actualArgs, _) ->
            or (zipWith identityHeadNeedsScopedData (NE.toList expectedArgs) (NE.toList actualArgs))
          (BTVarAppWithIdentity _ _ expectedArgs, BTVarAppWithIdentity _ _ actualArgs, _) ->
            or (zipWith identityHeadNeedsScopedData (NE.toList expectedArgs) (NE.toList actualArgs))
          (BTForallWithIdentity _ _ expectedBound expectedBody, BTForallWithIdentity _ _ actualBound actualBody, _) ->
            maybe False (uncurry identityHeadNeedsScopedData) ((,) <$> expectedBound <*> actualBound)
              || identityHeadNeedsScopedData expectedBody actualBody
          (BTMuWithIdentity _ _ expectedBody, BTMuWithIdentity _ _ actualBody, _) ->
            identityHeadNeedsScopedData expectedBody actualBody
          _ -> False

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
        (BTConWithIdentity expectedIdentity _ (_ :| []), BTConWithIdentity actualIdentity _ (_ :| [])) ->
          opaqueIOBackendHeadMatches expectedIdentity && opaqueIOBackendHeadMatches actualIdentity
        _ ->
          False

    opaqueIODomainCompatible bound expected actual =
      alphaEqWithinDataScope expected actual
        || typeVariableBoundMatches bound expected actual
        || typeVariableBoundMatches bound actual expected
        || case (expected, actual) of
          (BTVarWithIdentity expectedIdentity _, _)
            | freeTypeVariableMayInstantiate bound expectedIdentity -> True
          (_, BTVarWithIdentity actualIdentity _)
            | freeTypeVariableMayInstantiate bound actualIdentity -> True
          (BTVarWithIdentity expectedIdentity _, BTVarWithIdentity actualIdentity _) ->
            typeBinderMatches expectedIdentity actualIdentity
          _ -> False

    typeVariableBoundMatches bound ty otherTy =
      case ty of
        BTVarWithIdentity identity _
          | let key = typeBoundReferenceKey identity,
            Set.notMember key bound ->
              case Map.lookup key typeBounds of
                Just (Just boundTy)
                  | not (alphaEqBackendType boundTy BTBottom) ->
                      go bound boundTy otherTy
                _ ->
                  False
        _ ->
          False

    sameStructuralDataOwner expectedIdentity actualIdentity =
      case (structuralSelfIdentityUnique expectedIdentity, structuralSelfIdentityUnique actualIdentity) of
        (Just expectedUnique, Just actualUnique) -> expectedUnique == actualUnique
        _ -> False

    nominalStructuralTypeVarArgsMatch dataIdentity args muIdentity =
      nominalStructuralOwnerMatches dataIdentity muIdentity
        && all freeTypeVarArg args
      where
        nominalStructuralOwnerMatches identity identity0 =
          structuralSelfIdentityUnique identity0 == Just (symbolUniqueIdentity identity)

        freeTypeVarArg =
          \case
            BTVarWithIdentity identity _ ->
              not (hasConcreteTypeBound (typeBoundReferenceKey identity))
            _ ->
              False

    metadataBackedTypeArgumentMatches metadataBacked bound expected actual =
      go bound expected actual || (metadataBacked && freeExpectedTypeVariableMayInstantiate bound expected)

    metadataBackedTypeArgumentMatchesEither metadataBacked bound expected actual =
      metadataBackedTypeArgumentMatches metadataBacked bound expected actual
        || metadataBackedTypeArgumentMatches metadataBacked bound actual expected

    freeExpectedTypeVariableMayInstantiate bound =
      \case
        BTVarWithIdentity identity _ ->
          let key = typeBoundReferenceKey identity
           in Set.notMember key bound
                && not (hasConcreteTypeBound key)
        _ ->
          False

    freeTypeVariableMayInstantiate bound identity =
      Set.notMember (typeBoundReferenceKey identity) bound

    hasConcreteTypeBound key =
      case Map.lookup key typeBounds of
        Just (Just boundTy) -> not (alphaEqBackendType boundTy BTBottom)
        _ -> False

    metadataBackedTypeHead dataIdentity =
      maybe False (const True) (lookupDataByIdentity dataIdentity)

    structuralMuMatchesKnownData dataIdentity args muIdentity body =
      structuralSelfIdentityCompatibleWithDataIdentity dataIdentity muIdentity
        && maybe False structuralDataDeclMatches (matchingDataDecl dataIdentity muIdentity)
      where
        structuralDataDeclMatches dataDecl
          | Just substitution <- structuralDataArgumentSubstitution dataDecl args =
              structuralDataDeclarationMatches typeBounds dataDecl substitution (BTMuWithIdentity muIdentity (typeBinderIdentityStableName muIdentity) body)
        structuralDataDeclMatches _ =
          False

    structuralSelfIdentityCompatibleWithDataIdentity dataIdentity muIdentity =
      structuralSelfIdentityUnique muIdentity == Just (symbolUniqueIdentity dataIdentity)

    matchingDataDecl dataIdentity muIdentity =
      case lookupDataByIdentity dataIdentity of
        Just dataDecl
          | structuralSelfIdentityMatchesDataByIdentity muIdentity dataDecl -> Just dataDecl
          | otherwise -> Nothing
        Nothing ->
          lookupDataByStructuralSelfIdentity muIdentity

    lookupDataByIdentity identity = do
      dataDeclsByIdentity <- backendDataScopeByIdentity <$> mbDataDecls
      lookupSymbolIdentityExact identity dataDeclsByIdentity

    lookupDataByStructuralSelfIdentity muIdentity = do
      unique <- structuralSelfIdentityUnique muIdentity
      dataDeclsByIdentity <- backendDataScopeByIdentity <$> mbDataDecls
      case [ dataDecl
           | dataDecl <- Map.elems dataDeclsByIdentity,
             symbolUniqueIdentity (backendDataIdentity dataDecl) == unique
           ] of
        [dataDecl] -> Just dataDecl
        _ -> Nothing

    structuralSelfIdentityMatchesDataByIdentity muIdentity dataDecl =
      case structuralSelfIdentityUnique muIdentity of
        Just unique -> symbolUniqueIdentity (backendDataIdentity dataDecl) == unique
        Nothing -> False

    -- Structural ADT payloads encode data parameters inside handler fields. Keep
    -- that instantiation path local to matching structural encodings of the same
    -- owner so ordinary recursive type matching still treats free variables
    -- strictly.
    structuralMuPayloadMayInstantiate expectedIdentity expectedName expectedBody actualIdentity actualName actualBody =
      case typeVariableInstantiation of
        RejectFreeTypeVariableInstantiation ->
          False
        AllowStructuralPayloadInstantiation ->
          structuralPayloadsMayInstantiate typeBounds expectedIdentity expectedName expectedBody actualIdentity actualName actualBody

    vacuousRecursiveWrapperMayUnwrap =
      Set.null . freeBackendTypeVarKeys

    freshBinderName leftName rightName leftBound rightBound leftBody rightBody =
      freshNameLike
        leftName
        ( Set.unions
            [ Set.fromList [leftName, rightName],
              typeBoundKeyNames typeBounds,
              maybe Set.empty freeBackendTypeVars leftBound,
              maybe Set.empty freeBackendTypeVars rightBound,
              freeBackendTypeVars leftBody,
              freeBackendTypeVars rightBody
            ]
        )

    freshBinderTy leftIdentity name =
      BTVarWithIdentity leftIdentity name

    freshBinderKey leftIdentity =
      backendTypeSubstitutionKeyFromIdentity leftIdentity

{- Note [Backend bounded-forall instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A bounded xMLF application may eliminate a polymorphic bound after first
instantiating one or more of that bound's leading quantifiers.  Only those
quantified identities are matchable parameters.  Free variables in the bound
and in the supplied type remain rigid; otherwise validation could accept an
unrelated open type merely because its spelling has a compatible shape.
-}
validateBackendTypeArgumentBound :: Maybe BackendValidationContext -> Maybe BackendType -> BackendType -> Either BackendValidationError ()
validateBackendTypeArgumentBound _ Nothing _ =
  pure ()
validateBackendTypeArgumentBound _ (Just BTBottom) _ =
  pure ()
validateBackendTypeArgumentBound mbContext (Just boundTy) actualTy =
  unless (boundMatchesInContext || boundInstantiationMatches || alphaEqBackendType actualTy boundTy) $
    Left (BackendTypeAppBoundMismatch boundTy actualTy)
  where
    boundMatchesInContext =
      case mbContext of
        Nothing -> False
        Just context0 ->
          backendTypeMatchesWith
            RejectFreeTypeVariableInstantiation
            (bvcTypeBounds context0)
            (Just (backendDataScopeForContext context0))
            boundTy
            actualTy

    -- See Note [Backend bounded-forall instantiation].
    boundInstantiationMatches =
      leadingForallInstantiationMatches Map.empty boundTy

    leadingForallInstantiationMatches parameterBounds =
      \case
        BTForallWithIdentity identity _ mbBound body ->
          let parameterBounds' =
                Map.insert
                  (backendTypeSubstitutionKeyFromIdentity identity)
                  mbBound
                  parameterBounds
           in parametersMatch parameterBounds' body
                || leadingForallInstantiationMatches parameterBounds' body
        _ ->
          False

    parametersMatch parameterBounds expectedBody =
      case
          matchBackendTypeParametersWithTypeBounds
            enclosingTypeBounds
            []
            parameterBounds
            Map.empty
            expectedBody
            actualTy
        of
          Just _ -> True
          Nothing -> False

    enclosingTypeBounds =
      maybe Map.empty bvcTypeBounds mbContext

lookupBackendVariable :: BackendValidationContext -> IdDetails -> Maybe BackendType
lookupBackendVariable context0 details
  | Just key <- idDetailsLocalKey details =
      Map.lookup key (bvcLocals context0)
  | Just identity <- idDetailsSymbolIdentity details =
      Map.lookup identity (bvcGlobals context0)
        <|> lookupPrimitiveRuntimeVariable context0 identity
  | otherwise = Nothing

lookupPrimitiveRuntimeVariable :: BackendValidationContext -> SymbolIdentity -> Maybe BackendType
lookupPrimitiveRuntimeVariable context0 identity = do
  primitiveName <- PrimitiveInventory.primitiveValueNameByIdentity identity
  Map.lookup (builtinValueIdentity primitiveName) (bvcGlobals context0)

idDetailsLocalKey :: IdDetails -> Maybe BackendLocalKey
idDetailsLocalKey =
  \case
    LocalId ref -> Just (BackendLocalRef ref)
    EvidenceId ref -> Just (BackendLocalRef ref)
    EnvId ref -> Just (BackendEnvRef ref)
    DeferredId ref -> Just (BackendDeferredRef ref)
    _ -> Nothing

extendLocalMaybe :: Maybe BackendValidationContext -> IdDetails -> BackendType -> Maybe BackendValidationContext
extendLocalMaybe mbContext identity ty =
  fmap (\context0 -> extendLocal context0 identity ty) mbContext

extendFunctionParamLocalMaybe :: Maybe BackendValidationContext -> IdDetails -> String -> BackendType -> BackendExpr -> Maybe BackendValidationContext
extendFunctionParamLocalMaybe mbContext identity name ty body
  | backendExprCallsBinderAsClosureHead (backendCallableRef identity name) body =
      extendClosureLocalMaybe mbContext identity ty
  | otherwise =
      extendLocalMaybe mbContext identity ty

extendLocal :: BackendValidationContext -> IdDetails -> BackendType -> BackendValidationContext
extendLocal context0 identity ty =
  context0
    { bvcLocals = bindLocalReference identity ty (bvcLocals context0),
      bvcCasePatternLocals = deleteLocalIdentity identity (bvcCasePatternLocals context0),
      bvcClosureLocals = deleteLocalReference identity (bvcClosureLocals context0),
      bvcPossibleClosureLocals = deleteLocalReference identity (bvcPossibleClosureLocals context0)
    }

extendClosureLocalMaybe :: Maybe BackendValidationContext -> IdDetails -> BackendType -> Maybe BackendValidationContext
extendClosureLocalMaybe mbContext identity ty =
  fmap (\context0 -> extendClosureLocal context0 identity ty) mbContext

extendClosureLocal :: BackendValidationContext -> IdDetails -> BackendType -> BackendValidationContext
extendClosureLocal context0 identity ty =
  context0
    { bvcLocals = bindLocalReference identity ty (bvcLocals context0),
      bvcCasePatternLocals = deleteLocalIdentity identity (bvcCasePatternLocals context0),
      bvcClosureLocals = insertLocalReference identity (bvcClosureLocals context0),
      bvcPossibleClosureLocals = deleteLocalReference identity (bvcPossibleClosureLocals context0)
    }

extendPossibleClosureLocalMaybe :: Maybe BackendValidationContext -> IdDetails -> BackendType -> Maybe BackendValidationContext
extendPossibleClosureLocalMaybe mbContext identity ty =
  fmap (\context0 -> extendPossibleClosureLocal context0 identity ty) mbContext

extendPossibleClosureLocal :: BackendValidationContext -> IdDetails -> BackendType -> BackendValidationContext
extendPossibleClosureLocal context0 identity ty =
  context0
    { bvcLocals = bindLocalReference identity ty (bvcLocals context0),
      bvcCasePatternLocals = deleteLocalIdentity identity (bvcCasePatternLocals context0),
      bvcClosureLocals = deleteLocalReference identity (bvcClosureLocals context0),
      bvcPossibleClosureLocals = insertLocalReference identity (bvcPossibleClosureLocals context0)
    }

localReferenceKey :: IdDetails -> Maybe BackendLocalKey
localReferenceKey = idDetailsLocalKey

bindLocalReference :: IdDetails -> BackendType -> Map.Map BackendLocalKey BackendType -> Map.Map BackendLocalKey BackendType
bindLocalReference identity ty entries =
  maybe entries (\key -> Map.insert key ty entries) (localReferenceKey identity)

insertLocalReference :: IdDetails -> Set.Set BackendLocalKey -> Set.Set BackendLocalKey
insertLocalReference identity entries =
  maybe entries (`Set.insert` entries) (localReferenceKey identity)

deleteLocalReference :: IdDetails -> Set.Set BackendLocalKey -> Set.Set BackendLocalKey
deleteLocalReference identity entries =
  maybe entries (`Set.delete` entries) (localReferenceKey identity)

insertLocalIdentityKey :: IdDetails -> Set.Set BackendLocalKey -> Set.Set BackendLocalKey
insertLocalIdentityKey identity keys =
  maybe keys (`Set.insert` keys) (idDetailsLocalKey identity)

deleteLocalIdentity :: IdDetails -> Set.Set BackendLocalKey -> Set.Set BackendLocalKey
deleteLocalIdentity identity keys =
  maybe keys (`Set.delete` keys) (idDetailsLocalKey identity)

extendLetLocalMaybe :: Maybe BackendValidationContext -> IdDetails -> BackendType -> BackendExpr -> Maybe BackendValidationContext
extendLetLocalMaybe mbContext identity ty rhs =
  extendClosureShapeLocalMaybe mbContext mbContext identity ty rhs

extendClosureCaptureLocalMaybe ::
  Maybe BackendValidationContext ->
  Maybe BackendValidationContext ->
  BackendClosureCapture ->
  Maybe BackendValidationContext
extendClosureCaptureLocalMaybe outerContext bodyContext capture =
  extendClosureShapeLocalMaybe
    outerContext
    bodyContext
    (backendClosureCaptureIdentity capture)
    (backendClosureCaptureType capture)
    (backendClosureCaptureExpr capture)

extendClosureShapeLocalMaybe ::
  Maybe BackendValidationContext ->
  Maybe BackendValidationContext ->
  IdDetails ->
  BackendType ->
  BackendExpr ->
  Maybe BackendValidationContext
extendClosureShapeLocalMaybe sourceContext targetContext identity ty rhs
  | not (backendTypeIsClosureValue ty) =
      extendLocalMaybe targetContext identity ty
  | otherwise =
      case backendCallableHeadInContext sourceContext rhs of
        BackendClosureCallableHead _ ->
          extendClosureLocalMaybe targetContext identity ty
        BackendUnknownCallableHead ->
          extendPossibleClosureLocalMaybe targetContext identity ty
        BackendDirectCallableHead _ ->
          extendLocalMaybe targetContext identity ty

extendPatternLocals :: BackendValidationContext -> [(BackendBinderRef, BackendType)] -> BackendValidationContext
extendPatternLocals =
  foldr extendOne
  where
    extendOne (ref, ty) context0 =
      case backendCallableRefIdentity ref of
        Nothing -> context0
        Just identity
          | backendTypeIsClosureValue ty -> markCasePatternLocal identity (extendClosureLocal context0 identity ty)
          | otherwise -> markCasePatternLocal identity (extendLocal context0 identity ty)

    markCasePatternLocal identity context0 =
      context0
        { bvcCasePatternLocals = insertLocalIdentityKey identity (bvcCasePatternLocals context0)
        }

backendTypeIsClosureValue :: BackendType -> Bool
backendTypeIsClosureValue =
  \case
    BTArrow {} -> True
    _ -> False

opaqueIOBackendHeadMatches :: SymbolIdentity -> Bool
opaqueIOBackendHeadMatches identity =
  identity == PrimitiveInventory.builtinTypeIdentity "IO"

primitiveTypeToBackendType :: PrimitiveInventory.PrimitiveType -> BackendType
primitiveTypeToBackendType ty =
  fst (primitiveTypeToBackendTypeFrom initialIdentityGenerator ty)

primitiveTypeToBackendTypeFrom :: IdentityGenerator -> PrimitiveInventory.PrimitiveType -> (BackendType, IdentityGenerator)
primitiveTypeToBackendTypeFrom generator0 ty =
  primitiveTypeToBackendTypeFromWithHeadIdentities Map.empty generator0 ty

primitiveTypeToBackendTypeFromWithHeadIdentities :: Map.Map String SymbolIdentity -> IdentityGenerator -> PrimitiveInventory.PrimitiveType -> (BackendType, IdentityGenerator)
primitiveTypeToBackendTypeFromWithHeadIdentities headIdentities0 generator0 ty =
  let (freeEnv, generator) =
        foldl
          ( \(env, gen) name ->
              let (identity, gen') = freshBackendTypeIdentity gen
               in (Map.insert name identity env, gen')
          )
          (Map.empty, generatorAfterHeads)
          (Set.toAscList (PrimitiveInventory.freePrimitiveTypeVars ty))
      (ty', generator') = go freeEnv generator ty
   in (ty', generator')
  where
    generatorAfterHeads =
      advanceIdentityGeneratorPastMany
        (concatMap symbolGeneratedIdentities (Map.elems headIdentities0))
        generator0

    go env generator =
      \case
        PrimitiveInventory.PrimitiveTypeVar name ->
          case Map.lookup name env of
            Just identity -> (BTVarWithIdentity identity name, generator)
            Nothing ->
              let (identity, generator') = freshBackendTypeIdentity generator
               in (BTVarWithIdentity identity name, generator')
        PrimitiveInventory.PrimitiveTypeArrow dom cod ->
          let (dom', generator1) = go env generator dom
              (cod', generator2) = go env generator1 cod
           in (BTArrow dom' cod', generator2)
        PrimitiveInventory.PrimitiveTypeBase name ->
          (BTBaseWithIdentity (primitiveTypeHeadIdentity name) (BaseTy name), generator)
        PrimitiveInventory.PrimitiveTypeCon name args ->
          let (args', generator') = mapAccumPrimitiveBackendTypes env generator args
           in (BTConWithIdentity (primitiveTypeHeadIdentity name) (BaseTy name) args', generator')
        PrimitiveInventory.PrimitiveTypeForall name body ->
          let (identity, generator1) = freshBackendTypeIdentity generator
              (body', generator2) = go (Map.insert name identity env) generator1 body
           in (BTForallWithIdentity identity name Nothing body', generator2)
        PrimitiveInventory.PrimitiveTypeMu name body ->
          let (identity, generator1) =
                case primitiveStructuralOwnerIdentity name of
                  Just ownerIdentity -> (ownerIdentity, generator)
                  Nothing -> freshBackendTypeIdentity generator
              (body', generator2) = go (Map.insert name identity env) generator1 body
           in (BTMuWithIdentity identity name body', generator2)

    mapAccumPrimitiveBackendTypes env generator (arg :| args) =
      let (arg', generator1) = go env generator arg
          (argsRev, generator') =
            foldl
              ( \(acc, gen) item ->
                  let (item', gen') = go env gen item
                   in (item' : acc, gen')
              )
              ([], generator1)
              args
           in (arg' :| reverse argsRev, generator')

    primitiveTypeHeadIdentity name =
      case lookupPrimitiveTypeHeadIdentity headIdentities0 name <|> PrimitiveIdentity.primitiveTypeHeadIdentity name of
        Just identity -> identity
        Nothing -> error ("primitive type head is missing an ABI identity: " ++ name)

    -- Primitive specs are string-shaped, but a visible data identity pins the
    -- structural self binder before the type reaches production validation.
    primitiveStructuralOwnerIdentity name = do
      dataName <- structuralRecursiveDataName name
      dataIdentity <- lookupPrimitiveTypeHeadIdentity headIdentities0 dataName
      pure (typeBinderIdentityFromStructural (symbolUniqueIdentity dataIdentity) StructuralSelfBinder)

    lookupPrimitiveTypeHeadIdentity headIdentities name =
      lookupSymbolIdentityAlias headIdentities name

    freshBackendTypeIdentity generator =
      let (unique, generator') = freshIdentity generator
       in (typeBinderIdentityFromUnique unique, generator')

dropTermLocalsMaybe :: Maybe BackendValidationContext -> Maybe BackendValidationContext
dropTermLocalsMaybe =
  fmap
    ( \context0 ->
        context0
          { bvcLocals = Map.empty,
            bvcCasePatternLocals = Set.empty,
            bvcClosureLocals = Set.empty,
            bvcPossibleClosureLocals = Set.empty
          }
    )

extendTypeBoundMaybe :: Maybe BackendValidationContext -> TypeBinderIdentity -> Maybe BackendType -> Maybe BackendValidationContext
extendTypeBoundMaybe mbContext identity mbBound =
  fmap
    ( \context0 ->
        context0
          { bvcTypeBounds =
              Map.insert
                (backendTypeSubstitutionKeyFromIdentity identity)
                mbBound
                (bvcTypeBounds context0)
          }
    )
    mbContext

extendTypeBounds :: BackendValidationContext -> [(BackendTypeSubstitutionKey, Maybe BackendType)] -> BackendValidationContext
extendTypeBounds context0 bounds =
  context0 {bvcTypeBounds = foldr (uncurry Map.insert) (bvcTypeBounds context0) bounds}

lookupBackendConstructorInfo :: BackendValidationContext -> SymbolIdentity -> Maybe BackendConstructorInfo
lookupBackendConstructorInfo context0 identity =
  Map.lookup identity (bvcConstructors context0)

canonicalizeBackendTypeDataHeads :: BackendValidationContext -> BackendType -> BackendType
canonicalizeBackendTypeDataHeads context0 =
  canonicalizeBackendTypeDataHeadsWith (bvcData context0)

structuralSelfIdentityUnique :: TypeBinderIdentity -> Maybe UniqueIdentity
structuralSelfIdentityUnique identity = do
  (unique, StructuralSelfBinder) <- typeBinderIdentityStructural identity
  pure unique

canonicalizeBackendTypeDataHeadsWith :: Map.Map SymbolIdentity BackendData -> BackendType -> BackendType
canonicalizeBackendTypeDataHeadsWith dataDeclsByIdentity =
  go
  where
    go ty =
      case ty of
        BTBaseWithIdentity identity (BaseTy name) ->
          let (identity', name') = canonicalHead identity name
           in BTBaseWithIdentity identity' (BaseTy name')
        BTConWithIdentity identity (BaseTy name) args ->
          let (identity', name') = canonicalHead identity name
           in BTConWithIdentity identity' (BaseTy name') (fmap go args)
        BTArrow dom cod ->
          BTArrow (go dom) (go cod)
        BTVarAppWithIdentity identity name args ->
          BTVarAppWithIdentity identity name (fmap go args)
        BTForallWithIdentity identity name mb body ->
          BTForallWithIdentity identity name (fmap go mb) (go body)
        BTMuWithIdentity identity name body ->
          let (name', body0) = canonicalizeBuiltinStructuralMuBinder identity name body
           in case recoverStructuralDataType identity body0 of
                Just recovered -> go recovered
                Nothing ->
                  let body' = go body0
                   in case recoverStructuralDataType identity body' of
                        Just recovered -> recovered
                        Nothing -> BTMuWithIdentity identity name' body'
        _ ->
          ty

    canonicalHead identity name =
      case lookupSymbolIdentityExact identity dataDeclsByIdentity of
        Just dataDecl -> (backendDataIdentity dataDecl, backendDataName dataDecl)
        Nothing -> (identity, name)

    canonicalizeBuiltinStructuralMuBinder identity name body =
      case structuralRecursiveDataName name of
        Just dataName
          | let normalizedDataName = normalizeBackendBuiltinTypeReference dataName,
            normalizedDataName /= dataName ->
              let canonicalName = "$" ++ normalizedDataName ++ "_self"
               in ( canonicalName,
                    substituteBackendTypeForBinder identity (BTVarWithIdentity identity canonicalName) body
                  )
        _ ->
          (name, body)

    recoverStructuralDataType identity body =
      case structuralDataByIdentity of
        Just dataDecl -> do
          args <- structuralBackendDataArguments identity dataDecl body
          Just (backendDataType dataDecl args)
        Nothing ->
          Nothing
      where
        structuralDataByIdentity = do
          unique <- structuralSelfIdentityUnique identity
          case [ dataDecl
               | dataDecl <- Map.elems dataDeclsByIdentity,
                 let dataIdentity = backendDataIdentity dataDecl,
                 symbolUniqueIdentity dataIdentity == unique
               ] of
            [dataDecl] -> Just dataDecl
            _ -> Nothing

    backendDataType dataDecl args =
      case args of
        [] -> BTBaseWithIdentity (backendDataIdentity dataDecl) (BaseTy (backendDataName dataDecl))
        arg : rest -> BTConWithIdentity (backendDataIdentity dataDecl) (BaseTy (backendDataName dataDecl)) (arg :| rest)

    structuralBackendDataArguments muIdentity dataDecl body = do
      handlerFields <- structuralBackendHandlerFields body
      let dataParameterRefs = backendDataParameterRefs dataDecl
          constructors = backendDataConstructors dataDecl
          parameterBounds =
            Map.fromList
              [ (backendDataParameterRefKey ref, Nothing)
              | ref <- dataParameterRefs
              ]
      if length handlerFields == length constructors
        then do
          substitution <-
            foldM
              (matchConstructorFields muIdentity dataDecl dataParameterRefs parameterBounds)
              Map.empty
              (zip constructors handlerFields)
          let completedSubstitution = completeBackendParameterSubstitution parameterBounds substitution
          Just
            [ Map.findWithDefault
                (backendDataParameterRefType ref)
                (backendDataParameterRefKey ref)
                completedSubstitution
            | ref <- dataParameterRefs
            ]
        else Nothing

    matchConstructorFields muIdentity dataDecl dataParameterRefs parameterBounds substitution (constructor, fields) =
      if length fields == length (backendConstructorFields constructor)
        then
          foldM
            ( \substitutionAcc (expectedTy, actualTy) ->
                matchBackendTypeParametersWithTypeBounds
                  Map.empty
                  dataParameterRefs
                  (constructorParameterBounds parameterBounds constructor)
                  substitutionAcc
                  (go expectedTy)
                  (go (recoverDataSelfField muIdentity dataDecl actualTy))
            )
            substitution
            (zip (backendConstructorFields constructor) fields)
        else Nothing

    constructorParameterBounds parameterBounds constructor =
      parameterBounds
        `Map.union` Map.fromList
          [ (backendTypeSubstitutionKeyFromIdentity (backendTypeBinderIdentity binder), backendTypeBinderBound binder)
          | binder <- backendConstructorForalls constructor
          ]

    recoverDataSelfField muIdentity dataDecl ty =
      case ty of
        BTVarWithIdentity fieldIdentity _
          | structuralDataSelfField fieldIdentity ->
              backendDataType dataDecl dataSelfArgs
        _ ->
          ty
      where
        structuralDataSelfField fieldIdentity =
          structuralDataSelfFieldMatches muIdentity fieldIdentity

        dataSelfArgs =
          map backendDataParameterRefType (backendDataParameterRefs dataDecl)

normalizeBackendBuiltinTypeReference :: String -> String
normalizeBackendBuiltinTypeReference name =
  case stripPrefix "Prelude." name of
    Just unqualifiedName
      | PrimitiveInventory.isBuiltinTypeName unqualifiedName ->
          unqualifiedName
    _ ->
      PrimitiveInventory.normalizeBuiltinTypeReference name

validateBackendConstructorUse :: Maybe BackendValidationContext -> SymbolIdentity -> String -> BackendType -> [BackendExpr] -> Either BackendValidationError ()
validateBackendConstructorUse Nothing _ _ _ _ =
  pure ()
validateBackendConstructorUse (Just context0) identity name resultTy0 args =
  case lookupBackendConstructorInfo context0 identity of
    Nothing ->
      Left (BackendUnknownConstructor name)
    Just constructorInfo -> do
      let constructor = bciConstructor constructorInfo
          dataParameters =
            constructorResultParameterRefs constructorInfo
          parameters =
            constructorResultParameterBounds constructorInfo
          fields = backendConstructorFields constructor
      unless (length fields == length args) $
        Left (BackendConstructorArityMismatch name (length fields) (length args))
      let resultTy = resultTy0
          constructorResultTy = backendConstructorResult constructor
      substitution <-
        case matchBackendTypeParametersWithTypeBounds (bvcTypeBounds context0) dataParameters parameters Map.empty constructorResultTy resultTy of
          Just substitution -> pure substitution
          Nothing
            | backendConstructorResultPlaceholderMatchesEither (bvcTypeBounds context0) constructorResultTy resultTy ->
                pure Map.empty
          Nothing -> Left (BackendConstructorResultMismatch name constructorResultTy resultTy)
      validateBackendConstructorStructuralPayload
        (bvcTypeBounds context0)
        constructorInfo
        substitution
        constructorResultTy
        (BackendConstructorResultMismatch name constructorResultTy resultTy)
      validateBackendConstructorStructuralPayload
        (bvcTypeBounds context0)
        constructorInfo
        substitution
        resultTy
        (BackendConstructorResultMismatch name constructorResultTy resultTy)
      let dataScope = backendDataScopeForContext context0
      finalSubstitution <-
        foldM
          (validateBackendConstructorArgument (bvcTypeBounds context0) (Just dataScope) dataParameters parameters name)
          substitution
          (zip [0 ..] (zip fields args))
      validateBackendConstructorResultSubstitution
        (bvcTypeBounds context0)
        (Just dataScope)
        constructorInfo
        finalSubstitution
        resultTy
        (BackendConstructorResultMismatch name constructorResultTy resultTy)
      pure ()

validateBackendConstructorArgument ::
  BackendParameterBounds ->
  Maybe BackendDataScope ->
  [BackendDataParameterRef] ->
  BackendParameterBounds ->
  String ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  (Int, (BackendType, BackendExpr)) ->
  Either BackendValidationError (Map.Map BackendTypeSubstitutionKey BackendType)
validateBackendConstructorArgument typeBounds mbDataDecls dataParameters parameters name substitution (index0, (expectedTy, arg)) =
  case matchBackendTypeParametersWithTypeBounds typeBounds dataParameters parameters substitution expectedTy argTy of
    Just substitution' ->
      pure substitution'
    Nothing ->
      let completedSubstitution = completeBackendParameterSubstitution parameters substitution
          substitutedExpectedTy = substituteBackendTypesByKey completedSubstitution expectedTy
       in if backendConstructorFieldTypeMatches substitutedExpectedTy
            then pure substitution
            else
              Left
                ( BackendConstructorArgumentMismatch
                    name
                    index0
                    substitutedExpectedTy
                    argTy
                )
  where
    argTy =
      backendExprType arg

    backendConstructorFieldTypeMatches substitutedExpectedTy =
      backendFieldPlaceholderMatches substitutedExpectedTy argTy
        || ( backendTypeContainsVarApp expectedTy
          && backendVariableTypeMatchesWithBounds typeBounds substitutedExpectedTy argTy
           )
        || backendVariableTypeMatchesWithBounds typeBounds substitutedExpectedTy argTy
        || backendStructuralDataBoundaryMatches
          typeBounds
          mbDataDecls
          substitutedExpectedTy
          argTy

    backendFieldPlaceholderMatches expected actual =
      case (expected, actual) of
        (BTVarWithIdentity identity _, _)
          | placeholderTypeVariable identity ->
              True
        (_, BTVarWithIdentity identity _)
          | placeholderTypeVariable identity ->
              True
        _ ->
          False

    placeholderTypeVariable identity =
      let key = typeBoundReferenceKey identity
       in Map.notMember key parameters
            && Map.notMember key typeBounds

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
  validateCaseAlternative contextForBody resultTy alternative

validateBackendPattern :: Maybe BackendValidationContext -> BackendType -> BackendPattern -> Either BackendValidationError (Maybe BackendValidationContext)
validateBackendPattern Nothing _ _ =
  pure Nothing
validateBackendPattern (Just context0) _ BackendDefaultPattern =
  pure (Just context0)
validateBackendPattern (Just context0) scrutineeTy0 (BackendConstructorPatternWithBinderIdentities mbIdentity name binders) =
  case lookupBackendConstructorInfo context0 mbIdentity of
    Nothing ->
      Left (BackendUnknownConstructor name)
    Just constructorInfo -> do
      mapM_ (\binder -> requireLocalBinder (backendPatternBinderName binder) (backendPatternBinderIdentity binder)) binders
      let constructor = bciConstructor constructorInfo
          dataParameters = constructorResultParameterRefs constructorInfo
          parameters = constructorResultParameterBounds constructorInfo
          fields = backendConstructorFields constructor
          binderNames = map backendPatternBinderName binders
          scrutineeTy = scrutineeTy0
          constructorResultTy = backendConstructorResult constructor
      requireUniqueBy BackendDuplicatePatternBinding (map patternBinderRef binders)
      unless (length fields == length binderNames) $
        Left (BackendPatternArityMismatch name (length fields) (length binderNames))
      substitution <-
        case matchBackendTypeParametersWithTypeBounds (bvcTypeBounds context0) dataParameters parameters Map.empty constructorResultTy scrutineeTy of
          Just substitution -> pure substitution
          Nothing
            | backendTypeRefinesScrutinee constructorResultTy scrutineeTy ->
                pure Map.empty
          Nothing -> Left (BackendCaseConstructorScrutineeMismatch name scrutineeTy constructorResultTy)
      validateBackendConstructorStructuralPayload
        (bvcTypeBounds context0)
        constructorInfo
        substitution
        constructorResultTy
        (BackendCaseConstructorScrutineeMismatch name scrutineeTy constructorResultTy)
      validateBackendConstructorStructuralPayload
        (bvcTypeBounds context0)
        constructorInfo
        substitution
        scrutineeTy
        (BackendCaseConstructorScrutineeMismatch name scrutineeTy constructorResultTy)
      let completedSubstitution =
            completeBackendParameterSubstitution parameters substitution
          fresheningSubstitution = constructorPatternFresheningSubstitution context0 substitution constructor
          patternSubstitution = Map.union completedSubstitution fresheningSubstitution
          instantiatedFields = map (substituteBackendTypesByKey patternSubstitution) fields
          contextForBody =
            extendTypeBounds
              context0
              (constructorPatternTypeBounds substitution fresheningSubstitution constructor)
      pure (Just (extendPatternLocals contextForBody (zipWith patternLocal binders instantiatedFields)))
  where
    patternLocal binder ty =
      (backendCallableRef (backendPatternBinderIdentity binder) (backendPatternBinderName binder), ty)

constructorPatternFresheningSubstitution ::
  BackendValidationContext ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendConstructor ->
  Map.Map BackendTypeSubstitutionKey BackendType
constructorPatternFresheningSubstitution context0 substitution constructor =
  snd (foldl freshen (reservedNames0, Map.empty) unresolvedNames)
  where
    unresolvedNames =
      [ (backendTypeSubstitutionKeyFromIdentity (backendTypeBinderIdentity binder), backendTypeBinderIdentity binder, backendTypeBinderName binder)
        | binder <- backendConstructorForalls constructor,
          Map.notMember (backendTypeSubstitutionKeyFromIdentity (backendTypeBinderIdentity binder)) substitution
      ]

    externalNames =
      Set.union (typeBoundKeyNames (bvcTypeBounds context0)) (freeBackendTypeVarsInKeyed substitution)

    reservedNames0 =
      Set.union externalNames (Set.fromList [name | (_, _, name) <- unresolvedNames])

    freshen (reservedNames, freshening) (key, identity, name)
      | Set.member name externalNames =
          let freshName = freshNameLike name reservedNames
           in (Set.insert freshName reservedNames, Map.insert key (BTVarWithIdentity identity freshName) freshening)
      | otherwise =
          (Set.insert name reservedNames, freshening)

constructorPatternTypeBounds ::
  Map.Map BackendTypeSubstitutionKey BackendType ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendConstructor ->
  [(BackendTypeSubstitutionKey, Maybe BackendType)]
constructorPatternTypeBounds substitution fresheningSubstitution constructor =
  [ (freshenedKey key identity, fmap (substituteBackendTypesByKey patternSubstitution) mbBound)
    | binder <- backendConstructorForalls constructor,
      let key = backendTypeSubstitutionKeyFromIdentity (backendTypeBinderIdentity binder),
      let identity = backendTypeBinderIdentity binder,
      let mbBound = backendTypeBinderBound binder,
      Map.notMember key substitution
  ]
  where
    patternSubstitution =
      Map.union fresheningSubstitution substitution

    freshenedKey key identity =
      case Map.lookup key fresheningSubstitution of
        Just (BTVarWithIdentity freshBinderIdentity _) -> backendTypeSubstitutionKeyFromIdentity freshBinderIdentity
        _ -> backendTypeSubstitutionKeyFromIdentity identity

constructorTypeParameterBounds :: BackendConstructorInfo -> BackendParameterBounds
constructorTypeParameterBounds constructorInfo =
  constructorTypeParameterBoundsForData (constructorInfoDataDecl constructorInfo) (bciConstructor constructorInfo)

constructorInfoDataParameterRefs :: BackendConstructorInfo -> [BackendDataParameterRef]
constructorInfoDataParameterRefs =
  bciDataParameterRefs

constructorResultParameterRefs :: BackendConstructorInfo -> [BackendDataParameterRef]
constructorResultParameterRefs constructorInfo =
  dataRefs ++ resultRefs
  where
    constructor =
      bciConstructor constructorInfo
    dataRefs =
      constructorInfoDataParameterRefs constructorInfo
    dataKeys =
      Set.fromList (map backendDataParameterRefKey dataRefs)
    resultRefs =
      [ ref
      | ref <- Set.toList (freeBackendTypeVarRefs (backendConstructorResult constructor)),
        Set.notMember (backendDataParameterRefKey ref) dataKeys
      ]

constructorResultParameterBounds :: BackendConstructorInfo -> BackendParameterBounds
constructorResultParameterBounds constructorInfo =
  constructorTypeParameterBounds constructorInfo
    `Map.union` Map.fromList
      [ (backendDataParameterRefKey ref, Nothing)
      | ref <- constructorResultParameterRefs constructorInfo
      ]

constructorTypeParameterBoundsForData :: BackendData -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsForData dataDecl constructor =
  Map.fromList $
    [(backendDataParameterRefKey ref, Nothing) | ref <- backendDataParameterRefs dataDecl]
      ++ [ (backendTypeSubstitutionKeyFromIdentity (backendTypeBinderIdentity binder), backendTypeBinderBound binder)
           | binder <- backendConstructorForalls constructor
         ]

validateBackendConstructorStructuralPayload ::
  BackendParameterBounds ->
  BackendConstructorInfo ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  BackendValidationError ->
  Either BackendValidationError ()
validateBackendConstructorStructuralPayload typeBounds constructorInfo substitution ty mismatchError =
  unless (constructorStructuralPayloadIdentityAllowed dataDecl ty && structuralDataDeclarationMatches typeBounds dataDecl substitution ty) $
    Left mismatchError
  where
    dataDecl =
      constructorInfoDataDecl constructorInfo

constructorStructuralPayloadIdentityAllowed :: BackendData -> BackendType -> Bool
constructorStructuralPayloadIdentityAllowed dataDecl =
  \case
    BTMuWithIdentity identity _ _ ->
      structuralSelfIdentityPinsData (backendDataIdentity dataDecl) identity
    _ ->
      True
  where
    structuralSelfIdentityPinsData dataIdentity muIdentity =
      maybe False ((== symbolUniqueIdentity dataIdentity)) (structuralSelfIdentityUnique muIdentity)

constructorInfoDataDecl :: BackendConstructorInfo -> BackendData
constructorInfoDataDecl constructorInfo =
  BackendDataWithIdentity
    { backendDataIdentity = bciDataIdentity constructorInfo,
      backendDataNameWithIdentity = bciDataName constructorInfo,
      backendDataParameterRefsWithIdentity = bciDataParameterRefs constructorInfo,
      backendDataConstructorsWithIdentity = bciDataConstructors constructorInfo
    }

validateBackendConstructorResultSubstitution ::
  BackendParameterBounds ->
  Maybe BackendDataScope ->
  BackendConstructorInfo ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  BackendValidationError ->
  Either BackendValidationError ()
validateBackendConstructorResultSubstitution typeBounds mbDataDecls constructorInfo substitution resultTy mismatchError =
  unless
    ( backendStructuralDataBoundaryMatches
        typeBounds
        mbDataDecls
        substitutedResultTy
        resultTy
        || backendConstructorResultPlaceholderMatchesEither typeBounds substitutedResultTy resultTy
    )
    $
    Left mismatchError
  where
    constructor =
      bciConstructor constructorInfo
    constructorResultTy =
      backendConstructorResult constructor
    resultParameterBounds =
      case constructorResultTy of
        BTMu {} ->
          Map.filterWithKey (\key _ -> Set.member key dataParameterKeys) (constructorTypeParameterBounds constructorInfo)
        _ ->
          constructorTypeParameterBounds constructorInfo
    resultSubstitution =
      Map.filterWithKey (\key _ -> Map.member key resultParameterBounds || Set.member key dataParameterKeys || Set.member key resultFreeKeys) substitution
    completedSubstitution =
      completeBackendParameterSubstitution resultParameterBounds resultSubstitution
    substitutedResultTy =
      substituteBackendTypesByKey completedSubstitution constructorResultTy
    dataParameterKeys =
      Set.fromList (backendDataParameterKeys (constructorInfoDataDecl constructorInfo))
    resultFreeKeys =
      freeBackendTypeVarKeys constructorResultTy

validateCaseAlternative :: Maybe BackendValidationContext -> BackendType -> BackendAlternative -> Either BackendValidationError ()
validateCaseAlternative mbContext resultTy alternative =
  unless (backendApplicationTypeMatches mbContext (backendExprType (backendAltBody alternative)) resultTy) $
    Left (BackendCaseResultMismatch resultTy (backendExprType (backendAltBody alternative)))

backendConstructorResultPlaceholderMatchesEither :: BackendParameterBounds -> BackendType -> BackendType -> Bool
backendConstructorResultPlaceholderMatchesEither typeBounds left right =
  backendConstructorResultPlaceholderMatches typeBounds left right
    || backendConstructorResultPlaceholderMatches typeBounds right left

backendConstructorResultPlaceholderMatches :: BackendParameterBounds -> BackendType -> BackendType -> Bool
backendConstructorResultPlaceholderMatches typeBounds actual expected =
  case (actual, expected) of
    (_, BTVarWithIdentity identity _)
      | placeholderOpen identity -> True
    (_, BTVarAppWithIdentity identity _ _)
      | placeholderOpen identity -> True
    (BTArrow actualDom actualCod, BTArrow expectedDom expectedCod) ->
      backendConstructorResultPlaceholderMatches typeBounds actualDom expectedDom
        && backendConstructorResultPlaceholderMatches typeBounds actualCod expectedCod
    (BTConWithIdentity actualIdentity _ actualArgs, BTConWithIdentity expectedIdentity _ expectedArgs)
      | backendTypeHeadMatches actualIdentity expectedIdentity,
        length actualArgs == length expectedArgs ->
          and (zipWith (backendConstructorResultPlaceholderMatches typeBounds) (NE.toList actualArgs) (NE.toList expectedArgs))
    (BTVarAppWithIdentity actualIdentity _ actualArgs, BTVarAppWithIdentity expectedIdentity _ expectedArgs)
      | typeBinderRefMatches actualIdentity expectedIdentity,
        length actualArgs == length expectedArgs ->
          and (zipWith (backendConstructorResultPlaceholderMatches typeBounds) (NE.toList actualArgs) (NE.toList expectedArgs))
    (BTForallWithIdentity actualIdentity _ actualBound actualBody, BTForallWithIdentity expectedIdentity _ expectedBound expectedBody) ->
      backendConstructorResultPlaceholderBoundMatches typeBounds actualBound expectedBound
        && backendConstructorResultPlaceholderMatches
          (extendPlaceholderBound expectedIdentity (extendPlaceholderBound actualIdentity typeBounds))
          actualBody
          expectedBody
    _ -> alphaEqBackendType actual expected
  where
    placeholderOpen identity =
      Map.notMember (typeBoundReferenceKey identity) typeBounds

    extendPlaceholderBound identity bounds =
      Map.insert (typeBoundReferenceKey identity) Nothing bounds

backendConstructorResultPlaceholderBoundMatches :: BackendParameterBounds -> Maybe BackendType -> Maybe BackendType -> Bool
backendConstructorResultPlaceholderBoundMatches _ Nothing Nothing = True
backendConstructorResultPlaceholderBoundMatches typeBounds (Just actual) (Just expected) =
  backendConstructorResultPlaceholderMatches typeBounds actual expected
backendConstructorResultPlaceholderBoundMatches _ _ _ = False

requireUnique :: (String -> BackendValidationError) -> [String] -> Either BackendValidationError ()
requireUnique mkError names =
  case duplicates names of
    name : _ -> Left (mkError name)
    [] -> Right ()

requireUniqueBy :: (Ord key) => (String -> BackendValidationError) -> [(key, String)] -> Either BackendValidationError ()
requireUniqueBy mkError =
  go Set.empty
  where
    go _ [] = Right ()
    go seen ((key, name) : rest)
      | Set.member key seen = Left (mkError name)
      | otherwise = go (Set.insert key seen) rest

requireUniqueSymbolIdentities :: String -> (String -> BackendValidationError) -> [SymbolIdentity] -> Either BackendValidationError ()
requireUniqueSymbolIdentities label mkError =
  go Map.empty
  where
    go _ [] = Right ()
    go seen (identity : rest)
      | Just existing <- Map.lookup (symbolUniqueIdentity identity) seen =
          if symbolIdentityPayloadKey existing == symbolIdentityPayloadKey identity
            then Left (mkError (symbolIdentityStableName identity))
            else Left (BackendConflictingIdentityPayload label (symbolIdentityStableName identity))
      | otherwise =
          go (Map.insert (symbolUniqueIdentity identity) identity seen) rest


closureCaptureBinderRef :: BackendClosureCapture -> (ResolvedTermIdentityKey, String)
closureCaptureBinderRef capture =
  termBinderRef (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture)

closureParamBinderRef :: BackendClosureParam -> (ResolvedTermIdentityKey, String)
closureParamBinderRef param =
  termBinderRef (backendClosureParamIdentity param) (backendClosureParamName param)

patternBinderRef :: BackendPatternBinder -> (ResolvedTermIdentityKey, String)
patternBinderRef binder =
  termBinderRef (backendPatternBinderIdentity binder) (backendPatternBinderName binder)

termBinderRef :: IdDetails -> String -> (ResolvedTermIdentityKey, String)
termBinderRef identity name =
  (idDetailsIdentityKey identity, name)

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
