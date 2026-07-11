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
* the remaining String-keyed maps are explicitly metadata-light boundary/test
  indexes for identityless shapes, not production lookup authority;
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
    backendDataParameterRefFromMetadataLightName,
    backendDataParameterRefFromMaybeMetadataLight,
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
    backendTypeSubstitutionKeyFromMetadataLightName,
    backendTypeSubstitutionKeyFromMaybeMetadataLight,
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
    backendTypeHeadMatchesWith,
    backendTypeHeadMatches,
    backendTypeRefinesScrutineeWith,
    backendTypeRefinesScrutinee,
    typeBinderRefMatchesWith,
    typeBinderRefMatches,
    backendTermRefMatchesWith,
    backendTermRefMatches,
    closureEntryRefMatchesWith,
    closureEntryRefMatches,
    freeBackendTypeVarKeys,
    generatedIdentitiesInBackendProgram,
    generatedIdentitiesInBackendTypes,
    generatedIdentitiesInBackendExpr,
    backendCallableHeadWith,
    backendCallableHead,
    literalBackendType,
    substituteBackendTypeByIdentity,
    substituteBackendTypeForBinder,
    substituteBackendTypesByKey,
    unfoldBackendRecursiveType,
    validateBackendProgram,
    validateBackendProgramMetadataLight,
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
    backendStructuralDataBoundaryMatchesWith,
    completeBackendParameterSubstitution,
    completeDataParameterSubstitution,
    isVacuousRecursiveBinderWithIdentity,
    matchBackendTypeParametersWithTypeBounds,
    metadataLightStructuralDataMatchesWithIdentity,
    structuralIdentityAllowsNameFallback,
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
import MLF.Frontend.Program.Builtins (builtinTypeHeadIdentity, builtinValueIdentity)
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), lookupSymbolIdentityAlias, lookupSymbolIdentityExact, symbolDefiningModule, symbolDefiningName, symbolIdentityPayloadKey, symbolIdentityPayloadMatches, symbolIdentityStableName, symbolNamespace, symbolUniqueIdentity)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity
  ( DeferredRef,
    EnvRef,
    IdDetails (..),
    IdentityGenerator,
    LocalRef,
    StructuralTypeBinderRole (..),
    TypeBinderIdentity,
    UniqueIdentity,
    advanceIdentityGeneratorPastMany,
    freshIdentity,
    idDetailsAliasNames,
    idDetailsSymbolIdentity,
    initialIdentityGenerator,
    symbolGeneratedIdentities,
    typeBinderIdentityFromStructural,
    typeBinderIdentityFromUnique,
    typeBinderIdentityStructural,
  )
import MLF.Types.Reference (ReferenceMode (..))
import qualified MLF.Primitive.Inventory as PrimitiveInventory
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
  | BackendProgramMainIdentityMissing String
  | BackendModuleIdentityMissing String
  | BackendDuplicateData String
  | BackendDataIdentityMissing String
  | BackendModuleDataIdentityMissing String String
  | BackendModuleBindingIdentityMissing String String
  | BackendDataParameterIdentityMissing String String
  | BackendDataConstructorIdentityMissing String String
  | BackendDuplicateDataParameter String String
  | BackendConstructorUnknownTypeVariable String String
  | BackendConstructorTypeBinderIdentityMissing String String
  | BackendConflictingIdentityPayload String String
  | BackendDuplicateBinding String
  | BackendBindingIdentityMissing String
  | BackendDuplicateConstructor String
  | BackendConstructorIdentityMissing String
  | BackendMainNotFound String
  | BackendTypeVariableIdentityMissing String
  | BackendTypeHeadIdentityMissing String
  | BackendTypeApplicationHeadIdentityMissing String
  | BackendTypeForallIdentityMissing String
  | BackendRecursiveTypeIdentityMissing String
  | BackendVariableIdentityMissing String
  | BackendUnknownVariable String
  | BackendVariableTypeMismatch String BackendType BackendType
  | BackendBindingTypeMismatch String BackendType BackendType
  | BackendLiteralTypeMismatch Lit BackendType BackendType
  | BackendLambdaTypeMismatch BackendType BackendType
  | BackendApplicationExpectedFunction BackendType
  | BackendApplicationArgumentMismatch BackendType BackendType
  | BackendApplicationResultMismatch BackendType BackendType
  | BackendLambdaParameterIdentityMissing String
  | BackendClosureCalledWithBackendApp (Maybe String)
  | BackendDirectCalledWithBackendClosureCall String
  | BackendLetIdentityMissing String
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
  | BackendClosureEntryIdentityMissing String
  | BackendClosureEntryNameCollision String
  | BackendClosureCaptureIdentityMissing String
  | BackendDuplicateClosureCapture String
  | BackendClosureParameterIdentityMissing String
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
  | BackendConstructorUseIdentityMissing String
  | BackendConstructorArityMismatch String Int Int
  | BackendConstructorArgumentMismatch String Int BackendType BackendType
  | BackendConstructorResultMismatch String BackendType BackendType
  | BackendPatternArityMismatch String Int Int
  | BackendPatternConstructorIdentityMissing String
  | BackendPatternBinderIdentityMissing String
  | BackendDuplicatePatternBinding String
  | BackendCaseConstructorScrutineeMismatch String BackendType BackendType
  | BackendCaseResultMismatch BackendType BackendType
  deriving (Eq, Show)

data BackendReferenceKey identity
  = BackendMetadataLightKey String
  | BackendIdentityKey identity
  deriving (Eq, Ord, Show)

data BackendValidationContext = BackendValidationContext
  { bvcGlobals :: Map.Map (BackendReferenceKey SymbolIdentity) BackendType,
    bvcData :: Map.Map (BackendReferenceKey SymbolIdentity) BackendData,
    bvcConstructors :: Map.Map (BackendReferenceKey SymbolIdentity) BackendConstructorInfo,
    bvcLocals :: Map.Map (BackendReferenceKey BackendLocalKey) BackendType,
    bvcCasePatternLocals :: Set.Set BackendLocalKey,
    bvcClosureGlobals :: Set.Set (BackendReferenceKey SymbolIdentity),
    bvcClosureLocals :: Set.Set (BackendReferenceKey BackendLocalKey),
    bvcPossibleClosureLocals :: Set.Set (BackendReferenceKey BackendLocalKey),
    bvcTypeBounds :: BackendParameterBounds
  }

data BackendConstructorInfo = BackendConstructorInfo
  { bciDataIdentity :: Maybe SymbolIdentity,
    bciDataName :: String,
    bciDataParameterRefs :: [BackendDataParameterRef],
    bciDataConstructors :: [BackendConstructor],
    bciConstructor :: BackendConstructor
  }

typeBoundKeyNames :: BackendParameterBounds -> Set.Set String
typeBoundKeyNames =
  Set.map backendTypeSubstitutionKeyName . Map.keysSet

typeBoundReferenceKey :: ReferenceMode -> Maybe TypeBinderIdentity -> String -> Maybe BackendTypeSubstitutionKey
typeBoundReferenceKey mode mbIdentity name =
  case mbIdentity of
    Just identity -> Just (backendTypeSubstitutionKeyFromIdentity identity)
    Nothing ->
      case mode of
        IdentityOnly -> Nothing
        MetadataLight -> Just (backendTypeSubstitutionKeyFromMetadataLightName name)

data BackendLocalKey
  = BackendLocalRef LocalRef
  | BackendEnvRef EnvRef
  | BackendDeferredRef DeferredRef
  deriving (Eq, Ord, Show)

backendReferenceKey :: Maybe identity -> String -> BackendReferenceKey identity
backendReferenceKey mbIdentity name =
  maybe (BackendMetadataLightKey name) BackendIdentityKey mbIdentity

metadataLightEntries :: Map.Map (BackendReferenceKey identity) value -> Map.Map String value
metadataLightEntries entries =
  Map.fromList
    [ (name, value)
    | (BackendMetadataLightKey name, value) <- Map.toList entries
    ]

identityEntries :: Ord identity => Map.Map (BackendReferenceKey identity) value -> Map.Map identity value
identityEntries entries =
  Map.fromList
    [ (identity, value)
    | (BackendIdentityKey identity, value) <- Map.toList entries
    ]

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
validateBackendProgram program = do
  validateBackendProgramSemanticReferences program
  validateBackendProgramWith program

validateBackendProgramMetadataLight :: BackendProgram -> Either BackendValidationError ()
validateBackendProgramMetadataLight =
  validateBackendProgramWith

validateBackendProgramWith :: BackendProgram -> Either BackendValidationError ()
validateBackendProgramWith program = do
  requireUnique BackendDuplicateModule (map backendModuleName modules0)
  requireUniqueSymbolIdentities "module" BackendDuplicateModule [identity | module0 <- modules0, Just identity <- [backendModuleIdentity module0]]
  mapM_ validateBackendModuleIdentities modules0
  requireUnique BackendDuplicateData (map backendDataName dataDecls)
  requireUniqueSymbolIdentities "data" BackendDuplicateData [identity | dataDecl <- dataDecls, Just identity <- [backendDataIdentity dataDecl]]
  mapM_ validateBackendDataConstructorIdentities dataDecls
  mapM_ validateBackendDataParameterIdentities dataDecls
  mapM_ validateBackendDataConstructorBinderIdentities dataDecls
  mapM_ validateBackendDataConstructorTypeVariables dataDecls
  requireUnique BackendDuplicateBinding (map backendBindingName bindings)
  requireUniqueSymbolIdentities "binding" BackendDuplicateBinding [identity | binding <- bindings, Just identity <- [backendBindingIdentity binding]]
  requireUnique BackendDuplicateConstructor (map backendConstructorName constructors)
  requireUniqueSymbolIdentities "constructor" BackendDuplicateConstructor [identity | constructor <- constructors, Just identity <- [backendConstructorIdentity constructor]]
  requireUnique BackendDuplicateClosureEntry closureEntryNames
  rejectClosureEntryNameCollisions closureEntryNames (map backendBindingName bindings ++ Map.keys runtimePrimitiveTypes)
  validateBackendProgramMainIdentity program bindings
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
          Just identity <- [backendDataIdentity dataDecl]
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
              [ (backendReferenceKey (backendBindingIdentity binding) (backendBindingName binding), backendBindingType binding)
              | binding <- bindings
              ]
              `Map.union` Map.mapKeys BackendIdentityKey runtimePrimitiveTypesByIdentity,
          bvcData =
            Map.fromList
              [ (backendReferenceKey (backendDataIdentity dataDecl) (backendDataName dataDecl), dataDecl)
              | dataDecl <- dataDecls
              ],
          bvcConstructors =
            Map.fromList
              [ (backendReferenceKey (backendConstructorIdentity constructor) name, info)
              | (name, info@(BackendConstructorInfo {bciConstructor = constructor})) <- constructorInfos
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

validateBackendProgramSemanticReferences :: BackendProgram -> Either BackendValidationError ()
validateBackendProgramSemanticReferences program = do
  requireIdentity BackendProgramMainIdentityMissing (backendProgramMain program) (backendProgramMainIdentity program)
  mapM_ validateBackendModuleSemanticReferences (backendProgramModules program)

validateBackendModuleSemanticReferences :: BackendModule -> Either BackendValidationError ()
validateBackendModuleSemanticReferences backendModule = do
  requireIdentity BackendModuleIdentityMissing (backendModuleName backendModule) (backendModuleIdentity backendModule)
  mapM_ validateBackendDataSemanticReferences (backendModuleData backendModule)
  mapM_ validateBackendBindingSemanticReferences (backendModuleBindings backendModule)

validateBackendDataSemanticReferences :: BackendData -> Either BackendValidationError ()
validateBackendDataSemanticReferences dataDecl = do
  requireIdentity BackendDataIdentityMissing (backendDataName dataDecl) (backendDataIdentity dataDecl)
  mapM_ validateBackendDataParameterSemanticReference (backendDataParameterRefs dataDecl)
  mapM_ validateBackendConstructorSemanticReferences (backendDataConstructors dataDecl)
  where
    validateBackendDataParameterSemanticReference ref =
      requireIdentity
        (BackendDataParameterIdentityMissing (backendDataName dataDecl))
        (backendDataParameterRefName ref)
        (backendDataParameterRefIdentity ref)

validateBackendConstructorSemanticReferences :: BackendConstructor -> Either BackendValidationError ()
validateBackendConstructorSemanticReferences constructor = do
  requireIdentity BackendConstructorIdentityMissing (backendConstructorName constructor) (backendConstructorIdentity constructor)
  mapM_ validateBackendTypeBinderSemanticReferences (backendConstructorForalls constructor)
  mapM_ validateBackendTypeSemanticReferences (backendConstructorFields constructor)
  validateBackendTypeSemanticReferences (backendConstructorResult constructor)

validateBackendTypeBinderSemanticReferences :: BackendTypeBinder -> Either BackendValidationError ()
validateBackendTypeBinderSemanticReferences binder = do
  requireIdentity BackendTypeForallIdentityMissing (backendTypeBinderName binder) (backendTypeBinderIdentity binder)
  maybe (pure ()) validateBackendTypeSemanticReferences (backendTypeBinderBound binder)

validateBackendBindingSemanticReferences :: BackendBinding -> Either BackendValidationError ()
validateBackendBindingSemanticReferences binding = do
  requireIdentity BackendBindingIdentityMissing (backendBindingName binding) (backendBindingIdentity binding)
  validateBackendTypeSemanticReferences (backendBindingType binding)
  validateBackendExprSemanticReferences (backendBindingExpr binding)

validateBackendExprSemanticReferences :: BackendExpr -> Either BackendValidationError ()
validateBackendExprSemanticReferences expr = do
  validateBackendTypeSemanticReferences (backendExprType expr)
  case expr of
    BackendVarWithIdentity _ mbIdentity name ->
      requireIdentity BackendVariableIdentityMissing name mbIdentity
    BackendLit {} ->
      pure ()
    BackendLamWithIdentity _ mbIdentity name paramTy body -> do
      requireLocalIdentity BackendLambdaParameterIdentityMissing name mbIdentity
      validateBackendTypeSemanticReferences paramTy
      validateBackendExprSemanticReferences body
    BackendApp _ fun arg -> do
      validateBackendExprSemanticReferences fun
      validateBackendExprSemanticReferences arg
    BackendLetWithIdentity _ mbIdentity name bindingTy rhs body -> do
      requireLocalIdentity BackendLetIdentityMissing name mbIdentity
      validateBackendTypeSemanticReferences bindingTy
      validateBackendExprSemanticReferences rhs
      validateBackendExprSemanticReferences body
    BackendTyAbsWithIdentity _ mbIdentity name mbBound body -> do
      requireIdentity BackendTypeForallIdentityMissing name mbIdentity
      maybe (pure ()) validateBackendTypeSemanticReferences mbBound
      validateBackendExprSemanticReferences body
    BackendTyApp _ fun tyArg -> do
      validateBackendExprSemanticReferences fun
      validateBackendTypeSemanticReferences tyArg
    BackendRoll _ payload ->
      validateBackendExprSemanticReferences payload
    BackendUnroll _ payload ->
      validateBackendExprSemanticReferences payload
    BackendClosureWithParamIdentities _ mbIdentity entryName captures params body -> do
      requireIdentity BackendClosureEntryIdentityMissing entryName mbIdentity
      mapM_ validateBackendClosureCaptureSemanticReferences captures
      mapM_ validateBackendClosureParamSemanticReferences params
      validateBackendExprSemanticReferences body
    BackendClosureCall _ fun args -> do
      validateBackendExprSemanticReferences fun
      mapM_ validateBackendExprSemanticReferences args
    BackendConstructWithIdentity _ mbIdentity name args -> do
      requireIdentity BackendConstructorUseIdentityMissing name mbIdentity
      mapM_ validateBackendExprSemanticReferences args
    BackendCase _ scrutinee alternatives -> do
      validateBackendExprSemanticReferences scrutinee
      mapM_ validateBackendAlternativeSemanticReferences (NE.toList alternatives)

validateBackendClosureCaptureSemanticReferences :: BackendClosureCapture -> Either BackendValidationError ()
validateBackendClosureCaptureSemanticReferences capture = do
  requireLocalIdentity BackendClosureCaptureIdentityMissing (backendClosureCaptureName capture) (backendClosureCaptureIdentity capture)
  validateBackendTypeSemanticReferences (backendClosureCaptureType capture)
  validateBackendExprSemanticReferences (backendClosureCaptureExpr capture)

validateBackendClosureParamSemanticReferences :: BackendClosureParam -> Either BackendValidationError ()
validateBackendClosureParamSemanticReferences param = do
  requireLocalIdentity BackendClosureParameterIdentityMissing (backendClosureParamName param) (backendClosureParamIdentity param)
  validateBackendTypeSemanticReferences (backendClosureParamType param)

validateBackendAlternativeSemanticReferences :: BackendAlternative -> Either BackendValidationError ()
validateBackendAlternativeSemanticReferences alternative = do
  validateBackendPatternSemanticReferences (backendAltPattern alternative)
  validateBackendExprSemanticReferences (backendAltBody alternative)

validateBackendPatternSemanticReferences :: BackendPattern -> Either BackendValidationError ()
validateBackendPatternSemanticReferences =
  \case
    BackendDefaultPattern ->
      pure ()
    BackendConstructorPatternWithBinderIdentities mbIdentity name binders -> do
      requireIdentity BackendPatternConstructorIdentityMissing name mbIdentity
      mapM_ validateBackendPatternBinderSemanticReferences binders

validateBackendPatternBinderSemanticReferences :: BackendPatternBinder -> Either BackendValidationError ()
validateBackendPatternBinderSemanticReferences binder =
  requireLocalIdentity BackendPatternBinderIdentityMissing (backendPatternBinderName binder) (backendPatternBinderIdentity binder)

validateBackendTypeSemanticReferences :: BackendType -> Either BackendValidationError ()
validateBackendTypeSemanticReferences =
  \case
    BTVarWithIdentity mbIdentity name ->
      requireIdentity BackendTypeVariableIdentityMissing name mbIdentity
    BTArrow dom cod -> do
      validateBackendTypeSemanticReferences dom
      validateBackendTypeSemanticReferences cod
    BTBaseWithIdentity mbIdentity (BaseTy name) ->
      requireIdentity BackendTypeHeadIdentityMissing name mbIdentity
    BTConWithIdentity mbIdentity (BaseTy name) args -> do
      requireIdentity BackendTypeHeadIdentityMissing name mbIdentity
      mapM_ validateBackendTypeSemanticReferences (NE.toList args)
    BTVarAppWithIdentity mbIdentity name args -> do
      requireIdentity BackendTypeApplicationHeadIdentityMissing name mbIdentity
      mapM_ validateBackendTypeSemanticReferences (NE.toList args)
    BTForallWithIdentity mbIdentity name mbBound body -> do
      requireIdentity BackendTypeForallIdentityMissing name mbIdentity
      maybe (pure ()) validateBackendTypeSemanticReferences mbBound
      validateBackendTypeSemanticReferences body
    BTMuWithIdentity mbIdentity name body -> do
      requireIdentity BackendRecursiveTypeIdentityMissing name mbIdentity
      validateBackendTypeSemanticReferences body
    BTBottom ->
      pure ()

requireIdentity :: (String -> BackendValidationError) -> String -> Maybe identity -> Either BackendValidationError ()
requireIdentity mkError name =
  maybe (Left (mkError name)) (const (Right ()))

requireLocalIdentity :: (String -> BackendValidationError) -> String -> Maybe IdDetails -> Either BackendValidationError ()
requireLocalIdentity mkError name mbIdentity =
  case mbIdentity >>= idDetailsLocalKey of
    Just {} -> Right ()
    Nothing -> Left (mkError name)

validateBackendModuleIdentities :: BackendModule -> Either BackendValidationError ()
validateBackendModuleIdentities backendModule =
  case backendModuleIdentity backendModule of
    Just {} -> do
      mapM_ requireDataIdentity (backendModuleData backendModule)
      mapM_ requireBindingIdentity (backendModuleBindings backendModule)
    Nothing -> pure ()
  where
    moduleName0 =
      backendModuleName backendModule

    requireDataIdentity dataDecl =
      case backendDataIdentity dataDecl of
        Just {} -> pure ()
        Nothing -> Left (BackendModuleDataIdentityMissing moduleName0 (backendDataName dataDecl))

    requireBindingIdentity binding =
      case backendBindingIdentity binding of
        Just {} -> pure ()
        Nothing -> Left (BackendModuleBindingIdentityMissing moduleName0 (backendBindingName binding))

validateBackendDataConstructorIdentities :: BackendData -> Either BackendValidationError ()
validateBackendDataConstructorIdentities dataDecl =
  case backendDataIdentity dataDecl of
    Just {} -> mapM_ requireConstructorIdentity (backendDataConstructors dataDecl)
    Nothing -> pure ()
  where
    requireConstructorIdentity constructor =
      case backendConstructorIdentity constructor of
        Just {} -> pure ()
        Nothing -> Left (BackendDataConstructorIdentityMissing (backendDataName dataDecl) (backendConstructorName constructor))

validateBackendDataParameterIdentities :: BackendData -> Either BackendValidationError ()
validateBackendDataParameterIdentities dataDecl = do
  requireUniqueBy
    (BackendDuplicateDataParameter (backendDataName dataDecl))
    [ (key, backendTypeSubstitutionKeyName key)
    | ref <- backendDataParameterRefs dataDecl,
      let key = backendDataParameterRefKey ref
    ]
  case backendDataIdentity dataDecl of
    Just {} -> mapM_ requireParameterIdentity (backendDataParameterRefs dataDecl)
    Nothing -> pure ()
  where
    requireParameterIdentity ref =
      case backendDataParameterRefIdentity ref of
        Just {} -> pure ()
        Nothing -> Left (BackendDataParameterIdentityMissing (backendDataName dataDecl) (backendDataParameterRefName ref))

validateBackendDataConstructorBinderIdentities :: BackendData -> Either BackendValidationError ()
validateBackendDataConstructorBinderIdentities dataDecl =
  case backendDataIdentity dataDecl of
    Just {} -> mapM_ validateConstructor (backendDataConstructors dataDecl)
    Nothing -> pure ()
  where
    validateConstructor constructor =
      mapM_ (requireConstructorBinderIdentity constructor) (backendConstructorForalls constructor)

    requireConstructorBinderIdentity constructor binder =
      case backendTypeBinderIdentity binder of
        Just {} -> pure ()
        Nothing -> Left (BackendConstructorTypeBinderIdentityMissing (backendConstructorName constructor) (backendTypeBinderName binder))

validateBackendDataConstructorTypeVariables :: BackendData -> Either BackendValidationError ()
validateBackendDataConstructorTypeVariables dataDecl =
  case backendDataIdentity dataDecl of
    Just {} -> mapM_ validateConstructor (backendDataConstructors dataDecl)
    Nothing -> pure ()
  where
    dataKeys =
      Set.fromList (backendDataParameterKeys dataDecl)

    validateConstructor constructor = do
      let forallKeys =
            Set.fromList
              [ backendTypeSubstitutionKeyFromMaybeMetadataLight (backendTypeBinderIdentity binder) (backendTypeBinderName binder)
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
  backendDataScope (metadataLightEntries (bvcData context0)) (identityEntries (bvcData context0))

validateBackendProgramMainIdentity :: BackendProgram -> [BackendBinding] -> Either BackendValidationError ()
validateBackendProgramMainIdentity program bindings =
  case backendProgramMainIdentity program of
    Just {} ->
      Right ()
    Nothing ->
      case identityBearingMainBindings of
        _ : _ -> Left (BackendProgramMainIdentityMissing (backendProgramMain program))
        [] -> Right ()
  where
    identityBearingMainBindings =
      [ ()
      | binding <- bindings,
        backendBindingName binding == backendProgramMain program,
        Just {} <- [backendBindingIdentity binding]
      ]

backendProgramMainExists :: BackendProgram -> [BackendBinding] -> Bool
backendProgramMainExists program bindings =
  case backendProgramMainIdentity program of
    Just identity ->
      any (maybe False (symbolIdentityPayloadMatches identity) . backendBindingIdentity) bindings
    Nothing ->
      any
        ( \binding ->
            case backendBindingIdentity binding of
              Just {} -> False
              Nothing -> backendProgramMain program == backendBindingName binding
        )
        bindings

backendClosureGlobals :: BackendValidationContext -> [BackendBinding] -> Set.Set (BackendReferenceKey SymbolIdentity)
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
              [ backendReferenceKey (backendBindingIdentity binding) (backendBindingName binding)
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
      validateBackendExprWith mbContext rhs
      unless (alphaEqBackendType (backendExprType rhs) bindingTy) $
        Left (BackendLetTypeMismatch name bindingTy (backendExprType rhs))
      validateBackendExprWith (extendLetLocalMaybe mbContext mbIdentity name bindingTy rhs) body
      unless (backendApplicationTypeMatches mbContext resultTy (backendExprType body)) $
        Left (BackendLetBodyTypeMismatch resultTy (backendExprType body))
    BackendTyAbsWithIdentity resultTy mbIdentity name mbBound body -> do
      validateBackendExprWith (extendTypeBoundMaybe mbContext mbIdentity name mbBound) body
      let expected = BTForallWithIdentity mbIdentity name mbBound (backendExprType body)
      unless (backendApplicationTypeMatches mbContext resultTy expected) $
        Left (BackendTypeAbsTypeMismatch name resultTy expected)
    BackendTyApp resultTy fun tyArg -> do
      validateBackendExprWith mbContext fun
      case backendExprType fun of
        BTForallWithIdentity mbIdentity name mbBound bodyTy -> do
          validateBackendTypeArgumentBound mbBound tyArg
          let expected = substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFromMaybeMetadataLight mbIdentity name) tyArg) bodyTy
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

backendCallableHead :: (Maybe IdDetails -> String -> BackendCallableBindingKind) -> BackendExpr -> BackendCallableHead
backendCallableHead =
  backendCallableHeadWith IdentityOnly

backendCallableHeadWith :: ReferenceMode -> (Maybe IdDetails -> String -> BackendCallableBindingKind) -> BackendExpr -> BackendCallableHead
backendCallableHeadWith mode resolve0 =
  go resolve0
  where
    go resolve =
      \case
        BackendVarWithIdentity _ mbIdentity name ->
          case resolve mbIdentity name of
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
        BackendLetWithIdentity _ mbIdentity name _ rhs body ->
          go (extendBindingKind resolve mbIdentity name (go resolve rhs)) body
        BackendCase _ _ alternatives ->
          collapseCallableHeadsWith mode
            ( fmap
                ( \alternative ->
                    let binders = patternBinderDetails (backendAltPattern alternative)
                        body = backendAltBody alternative
                        closureBinders =
                          filter (\binder -> backendExprMentionsBindingWithCallableType mode binder body) binders
                     in go (extendPatternBindingKinds binders closureBinders resolve) body
                )
                alternatives
            )
        _ ->
          BackendUnknownCallableHead

    extendBindingKind resolve mbIdentity name headShape localIdentity localName
      | backendCallableRefMatchesWith mode (backendCallableRef mbIdentity name) (backendCallableRef localIdentity localName) =
          callableBindingKindForHead headShape
      | otherwise =
          resolve localIdentity localName

    extendPatternBindingKinds binders closureBinders resolve localIdentity name
      | any (callableBinderMatches mode localIdentity name) closureBinders =
          BackendCallableBindingClosure
      | any (callableBinderMatches mode localIdentity name) binders =
          BackendCallableBindingDirect
      | otherwise =
          resolve localIdentity name

callableBinderMatches :: ReferenceMode -> Maybe IdDetails -> String -> BackendCallableRef -> Bool
callableBinderMatches mode localIdentity localName binder =
  backendCallableRefMatchesWith mode binder (backendCallableRef localIdentity localName)

callableBindingKindForHead :: BackendCallableHead -> BackendCallableBindingKind
callableBindingKindForHead =
  \case
    BackendDirectCallableHead _ -> BackendCallableBindingDirect
    BackendClosureCallableHead _ -> BackendCallableBindingClosure
    BackendUnknownCallableHead -> BackendCallableBindingUnknown

collapseCallableHeadsWith :: ReferenceMode -> NonEmpty BackendCallableHead -> BackendCallableHead
collapseCallableHeadsWith mode heads
  | all isClosureHead heads = BackendClosureCallableHead (sameClosureHeadRef mode heads)
  | all isDirectHead heads = BackendDirectCallableHead (sameDirectHeadRef mode heads)
  | otherwise = BackendUnknownCallableHead
  where
    isClosureHead BackendClosureCallableHead {} = True
    isClosureHead _ = False

    isDirectHead BackendDirectCallableHead {} = True
    isDirectHead _ = False

sameClosureHeadRef :: ReferenceMode -> NonEmpty BackendCallableHead -> Maybe BackendCallableRef
sameClosureHeadRef mode heads =
  case traverse closureHeadRef heads of
    Just (ref :| rest)
      | all (backendCallableRefMatchesWith mode ref) rest -> Just ref
    _ -> Nothing
  where
    closureHeadRef =
      \case
        BackendClosureCallableHead mbRef -> mbRef
        _ -> Nothing

sameDirectHeadRef :: ReferenceMode -> NonEmpty BackendCallableHead -> Maybe BackendCallableRef
sameDirectHeadRef mode heads =
  case traverse directHeadRef heads of
    Just (ref :| rest)
      | all (directHeadRefMatches mode ref) rest -> ref
    _ -> Nothing
  where
    directHeadRef =
      \case
        BackendDirectCallableHead ref -> Just ref
        _ -> Nothing

directHeadRefMatches :: ReferenceMode -> Maybe BackendCallableRef -> Maybe BackendCallableRef -> Bool
directHeadRefMatches mode (Just left) (Just right) =
  backendCallableRefMatchesWith mode left right
directHeadRefMatches _ Nothing Nothing =
  True
directHeadRefMatches _ _ _ =
  False

backendCallableHeadInContext :: Maybe BackendValidationContext -> BackendExpr -> BackendCallableHead
backendCallableHeadInContext mbContext =
  backendCallableHeadWith
    MetadataLight
    (backendCallableBindingKindInContext mbContext)

backendCallableBindingKindInContext :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendCallableBindingKind
backendCallableBindingKindInContext Nothing _ _ =
  BackendCallableBindingUnknown
backendCallableBindingKindInContext (Just context0) mbIdentity name =
  case mbIdentity of
    Just details
      | Just key <- idDetailsLocalKey details ->
          maybe BackendCallableBindingUnknown id (lookupLocalCallableBindingKind context0 (BackendIdentityKey key))
      | Just identity <- idDetailsSymbolIdentity details ->
          maybe BackendCallableBindingUnknown id (lookupGlobalCallableBindingKind context0 (BackendIdentityKey identity))
      | otherwise -> BackendCallableBindingUnknown
    Nothing ->
      let key = BackendMetadataLightKey name
       in maybe
            BackendCallableBindingUnknown
            id
            (lookupLocalCallableBindingKind context0 key <|> lookupGlobalCallableBindingKind context0 key)

lookupLocalCallableBindingKind :: BackendValidationContext -> BackendReferenceKey BackendLocalKey -> Maybe BackendCallableBindingKind
lookupLocalCallableBindingKind context0 key
  | Set.member key (bvcClosureLocals context0) =
      Just BackendCallableBindingClosure
  | Set.member key (bvcPossibleClosureLocals context0) =
      Just BackendCallableBindingUnknown
  | Map.member key (bvcLocals context0) =
      Just BackendCallableBindingDirect
  | otherwise =
      Nothing

lookupGlobalCallableBindingKind :: BackendValidationContext -> BackendReferenceKey SymbolIdentity -> Maybe BackendCallableBindingKind
lookupGlobalCallableBindingKind context0 key
  | Set.member key (bvcClosureGlobals context0) =
      Just BackendCallableBindingClosure
  | Map.member key (bvcGlobals context0) =
      Just BackendCallableBindingDirect
  | otherwise =
      Nothing

backendExprMentionsBindingWithCallableType :: ReferenceMode -> BackendBinderRef -> BackendExpr -> Bool
backendExprMentionsBindingWithCallableType mode needle =
  go
  where
    go =
      \case
        BackendVarWithIdentity ty mbIdentity name ->
          backendBinderMatchesWith mode needle (backendCallableRef mbIdentity name) && backendTypeIsClosureValue ty
        BackendLit {} ->
          False
        BackendLamWithIdentity _ mbIdentity name _ body
          | backendBinderMatchesWith mode needle (backendCallableRef mbIdentity name) -> False
          | otherwise -> go body
        BackendApp _ fun arg ->
          go fun || go arg
        BackendLetWithIdentity _ mbIdentity name _ rhs body
          | backendBinderMatchesWith mode needle (backendCallableRef mbIdentity name) -> go rhs
          | otherwise -> go rhs || go body
        BackendTyAbs _ _ _ body ->
          go body
        BackendTyApp ty (BackendVarWithIdentity _ mbIdentity name) _
          | backendBinderMatchesWith mode needle (backendCallableRef mbIdentity name),
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
            || (not (any (backendBinderMatchesWith mode needle) closureBinders) && go body)
          where
            closureBinders =
              [backendCallableRef (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture) | capture <- captures]
                ++ [backendCallableRef (backendClosureParamIdentity param) (backendClosureParamName param) | param <- params]
        BackendClosureCall _ fun args ->
          go fun || any go args

    goAlternative (BackendAlternative pattern0 body)
      | any (backendBinderMatchesWith mode needle) (patternBinderDetails pattern0) = False
      | otherwise = go body

backendBinderMatchesWith :: ReferenceMode -> BackendBinderRef -> BackendBinderRef -> Bool
backendBinderMatchesWith =
  backendCallableRefMatchesWith

type BackendBinderRef = BackendCallableRef

backendExprCallsBinderAsClosureHead :: ReferenceMode -> BackendBinderRef -> BackendExpr -> Bool
backendExprCallsBinderAsClosureHead mode needle =
  go [needle]
  where
    go aliases =
      \case
        BackendVarWithIdentity {} ->
          False
        BackendLit {} ->
          False
        BackendLamWithIdentity _ mbIdentity name _ body
          | any (backendBinderMatchesWith mode (backendCallableRef mbIdentity name)) aliases -> False
          | otherwise -> go aliases body
        BackendApp _ fun arg ->
          go aliases fun || go aliases arg
        BackendLetWithIdentity _ mbIdentity name _ rhs body
          | any (backendBinderMatchesWith mode (backendCallableRef mbIdentity name)) aliases -> go aliases rhs
          | otherwise ->
              let aliasesForBody =
                    if closureCallHeadReferencesAny mode aliases rhs
                      then insertBackendBinderAlias mode (backendCallableRef mbIdentity name) aliases
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
            || (backendBindersDisjoint mode aliases closureParamBinders && go aliasesForBody body)
          where
            closureParamBinders =
              [backendCallableRef (backendClosureParamIdentity param) (backendClosureParamName param) | param <- params]
            aliasesForBody =
              foldr (insertBackendBinderAlias mode) aliases capturedAliases
            capturedAliases =
              [ captureBinder
              | capture <- captures,
                let captureBinder = backendCallableRef (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture),
                any (\alias -> backendExprReferencesBinding mode alias (backendClosureCaptureExpr capture)) aliases
              ]
        BackendClosureCall _ fun args ->
          closureCallHeadReferencesAny mode aliases fun || go aliases fun || any (go aliases) args

    goAlternative aliases (BackendAlternative pattern0 body)
      | not (backendBindersDisjoint mode aliases (patternBinderDetails pattern0)) = False
      | otherwise = go aliases body

insertBackendBinderAlias :: ReferenceMode -> BackendBinderRef -> [BackendBinderRef] -> [BackendBinderRef]
insertBackendBinderAlias mode alias aliases =
  alias : filter (not . backendBinderMatchesWith mode alias) aliases

backendBindersDisjoint :: ReferenceMode -> [BackendBinderRef] -> [BackendBinderRef] -> Bool
backendBindersDisjoint mode left right =
  not (any (\leftBinder -> any (backendBinderMatchesWith mode leftBinder) right) left)

closureCallHeadReferencesAny :: ReferenceMode -> [BackendBinderRef] -> BackendExpr -> Bool
closureCallHeadReferencesAny mode aliases0 =
  \case
    BackendVarWithIdentity _ mbIdentity name ->
      any (backendBinderMatchesWith mode (backendCallableRef mbIdentity name)) aliases0
    BackendTyApp _ fun _ ->
      closureCallHeadReferencesAny mode aliases0 fun
    BackendLetWithIdentity _ mbIdentity name _ rhs body ->
      let binder = backendCallableRef mbIdentity name
          aliasesWithoutShadow =
            filter (not . backendBinderMatchesWith mode binder) aliases0
          aliasesForBody =
            if closureCallHeadReferencesAny mode aliases0 rhs
              then insertBackendBinderAlias mode binder aliasesWithoutShadow
              else aliasesWithoutShadow
       in closureCallHeadReferencesAny mode aliasesForBody body
    _ ->
      False

backendExprReferencesBinding :: ReferenceMode -> BackendBinderRef -> BackendExpr -> Bool
backendExprReferencesBinding mode needle =
  go
  where
    go =
      \case
        BackendVarWithIdentity _ mbIdentity name ->
          backendBinderMatchesWith mode needle (backendCallableRef mbIdentity name)
        BackendLit {} ->
          False
        BackendLamWithIdentity _ mbIdentity name _ body
          | backendBinderMatchesWith mode needle (backendCallableRef mbIdentity name) -> False
          | otherwise -> go body
        BackendApp _ fun arg ->
          go fun || go arg
        BackendLetWithIdentity _ mbIdentity name _ rhs body
          | backendBinderMatchesWith mode needle (backendCallableRef mbIdentity name) -> go rhs
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
            || (not (any (backendBinderMatchesWith mode needle) closureBinders) && go body)
          where
            closureBinders =
              [backendCallableRef (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture) | capture <- captures]
                ++ [backendCallableRef (backendClosureParamIdentity param) (backendClosureParamName param) | param <- params]
        BackendClosureCall _ fun args ->
          go fun || any go args

    goAlternative (BackendAlternative pattern0 body)
      | any (backendBinderMatchesWith mode needle) (patternBinderDetails pattern0) = False
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

validateBackendVariable :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> Either BackendValidationError ()
validateBackendVariable Nothing _ _ _ =
  pure ()
validateBackendVariable (Just context0) mbIdentity name actualTy =
  case lookupBackendVariable context0 mbIdentity name of
    Nothing ->
      if nameOnlyRefMatchesIdentityLocalAlias context0 mbIdentity name
        then Left (BackendVariableIdentityMissing name)
        else Left (BackendUnknownVariable name)
    Just expectedTy -> do
      unless (backendVariableTypeMatches context0 mbIdentity name expectedTy actualTy) $
        Left (BackendVariableTypeMismatch name expectedTy actualTy)

nameOnlyRefMatchesIdentityLocalAlias :: BackendValidationContext -> Maybe IdDetails -> String -> Bool
nameOnlyRefMatchesIdentityLocalAlias _ Just {} _ =
  False
nameOnlyRefMatchesIdentityLocalAlias context0 Nothing name =
  case
    [ key
    | BackendIdentityKey key <- Map.keys (bvcLocals context0),
      name `elem` backendLocalKeyAliasNames key
    ]
    of
      [_] -> True
      _ -> False

backendLocalKeyAliasNames :: BackendLocalKey -> [String]
backendLocalKeyAliasNames =
  \case
    BackendLocalRef ref -> idDetailsAliasNames (LocalId ref)
    BackendEnvRef ref -> idDetailsAliasNames (EnvId ref)
    BackendDeferredRef ref -> idDetailsAliasNames (DeferredId ref)

backendApplicationTypeMatches :: Maybe BackendValidationContext -> BackendType -> BackendType -> Bool
backendApplicationTypeMatches mbContext expectedTy actualTy =
  matches expectedTy actualTy
    || (not (backendTypeContainsMu expectedTy || backendTypeContainsMu actualTy) && matches expectedTy' actualTy')
  where
    typeBoundMode = MetadataLight
    typeBounds = maybe Map.empty bvcTypeBounds mbContext
    dataScope = backendDataScopeForContext <$> mbContext
    expectedTy' = maybe expectedTy (`canonicalizeBackendTypeDataHeads` expectedTy) mbContext
    actualTy' = maybe actualTy (`canonicalizeBackendTypeDataHeads` actualTy) mbContext
    matches expected actual =
      typeMatches expected actual
        || typeMatches actual expected
        || ( not (identityBearingNominalStructuralBoundary expected actual)
               && backendStructuralDataBoundaryMatchesWith
                 typeBoundMode
                 typeBounds
                 dataScope
                 expected
                 actual
           )

    typeMatches =
      backendTypeMatchesWith typeBoundMode AllowStructuralPayloadInstantiation typeBounds dataScope

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
    (BTBaseWithIdentity (Just {}) _, BTMuWithIdentity {}) -> True
    (BTMuWithIdentity {}, BTBaseWithIdentity (Just {}) _) -> True
    (BTConWithIdentity (Just {}) _ _, BTMuWithIdentity {}) -> True
    (BTMuWithIdentity {}, BTConWithIdentity (Just {}) _ _) -> True
    _ -> False

backendVariableTypeMatches :: BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendType -> Bool
backendVariableTypeMatches context0 mbIdentity name expectedTy actualTy =
  rawMatches || canonicalMatches
  where
    dataScope = backendDataScopeForContext context0
    typeBounds = bvcTypeBounds context0
    typeBoundMode = MetadataLight
    rawMatches =
      backendTypeMatchesWith
        typeBoundMode
        RejectFreeTypeVariableInstantiation
        typeBounds
        (Just dataScope)
        expectedTy
        actualTy
        || backendTypeMatchesWith
          typeBoundMode
          AllowStructuralPayloadInstantiation
          typeBounds
          (Just dataScope)
          expectedTy
          actualTy
        || ( not (identityBearingNominalStructuralBoundary expectedTy actualTy)
               && backendStructuralDataBoundaryMatchesWith
                 typeBoundMode
                 typeBounds
                 (Just dataScope)
                 expectedTy
                 actualTy
           )
        || backendApplicationTypeMatches (Just context0) expectedTy actualTy
        || generatedCasePatternVariableTypeMatches context0 mbIdentity expectedTy
        || primitiveRuntimeVariableTypeMatches
          typeBoundMode
          mbIdentity
          name
          expectedTy
          actualTy
    canonicalMatches =
      let expectedTy' = canonicalizeBackendTypeDataHeads context0 expectedTy
          actualTy' = canonicalizeBackendTypeDataHeads context0 actualTy
       in backendTypeMatchesWith
           typeBoundMode
            RejectFreeTypeVariableInstantiation
            typeBounds
            (Just dataScope)
            expectedTy'
            actualTy'
            || backendTypeMatchesWith
              typeBoundMode
              AllowStructuralPayloadInstantiation
              typeBounds
              (Just dataScope)
              expectedTy'
              actualTy'
            || ( not (identityBearingNominalStructuralBoundary expectedTy' actualTy')
                   && backendStructuralDataBoundaryMatchesWith
                     typeBoundMode
                     typeBounds
                     (Just dataScope)
                     expectedTy'
                     actualTy'
               )
            || generatedCasePatternVariableTypeMatches context0 mbIdentity expectedTy'
            || primitiveRuntimeVariableTypeMatches
              typeBoundMode
              mbIdentity
              name
              expectedTy'
              actualTy'

generatedCasePatternVariableTypeMatches :: BackendValidationContext -> Maybe IdDetails -> BackendType -> Bool
generatedCasePatternVariableTypeMatches context0 mbIdentity expectedTy =
  case (mbIdentity >>= idDetailsLocalKey, expectedTy) of
    (Just localKey, BTVarWithIdentity identity typeName)
      | Set.member localKey (bvcCasePatternLocals context0) ->
          maybe False (not . hasConcreteTypeBound) (typeBoundReferenceKey typeBoundMode identity typeName)
    _ ->
      False
  where
    typeBoundMode = MetadataLight
    typeBounds = bvcTypeBounds context0
    hasConcreteTypeBound key =
      case Map.lookup key typeBounds of
        Just (Just boundTy) -> not (alphaEqBackendType boundTy BTBottom)
        _ -> False

primitiveRuntimeVariableTypeMatches :: ReferenceMode -> Maybe IdDetails -> String -> BackendType -> BackendType -> Bool
primitiveRuntimeVariableTypeMatches referenceMode mbIdentity _name expectedTy actualTy
  | primitiveRuntimeVariableReference mbIdentity =
      go expectedTy actualTy
  | otherwise =
      False
  where
    primitiveRuntimeVariableReference Nothing =
      False
    primitiveRuntimeVariableReference (Just details) =
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
              (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase) ->
                backendPrimitiveTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase
              (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs) ->
                backendPrimitiveTypeHeadMatches expectedIdentity expectedCon actualIdentity actualCon
                  && zipAllWith go (NE.toList expectedArgs) (NE.toList actualArgs)
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralPrimitiveMuMatches expectedIdentity expectedName expectedBody actualIdentity actualName actualBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTConWithIdentity actualIdentity actualCon actualArgs) ->
                structuralPrimitiveTypeMatches expectedIdentity expectedName expectedBody actualIdentity actualCon (NE.toList actualArgs)
              (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralPrimitiveTypeMatches actualIdentity actualName actualBody expectedIdentity expectedCon (NE.toList expectedArgs)
              _ ->
                False

    structuralPrimitiveMuMatches expectedIdentity expectedName expectedBody actualIdentity actualName actualBody =
      structuralPrimitiveMuOwnersMatch expectedIdentity expectedName actualIdentity actualName
        && case (structuralPrimitivePayloadTypes expectedIdentity expectedName expectedBody, structuralPrimitivePayloadTypes actualIdentity actualName actualBody) of
          (Just expectedPayloadTypes, Just actualPayloadTypes) ->
            zipAllWith go expectedPayloadTypes actualPayloadTypes
          _ ->
            False

    backendPrimitiveTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase =
      backendTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase
        || case (expectedIdentity, actualIdentity) of
          (Nothing, Nothing) -> normalizedBaseName expectedBase == normalizedBaseName actualBase
          _ -> False

    normalizedBaseName (BaseTy baseName) =
      normalizeBackendBuiltinTypeReference baseName

    structuralPrimitiveTypeMatches muIdentity muName muBody dataIdentity con args =
      structuralPrimitiveDataOwnerMatches muIdentity muName dataIdentity con
        && case structuralPrimitivePayloadTypes muIdentity muName muBody of
          Just payloadTypes -> zipAllWith go payloadTypes args
          Nothing -> False

    structuralPrimitiveMuOwnersMatch leftIdentity leftName rightIdentity rightName =
      case (structuralSelfIdentityUnique leftIdentity, structuralSelfIdentityUnique rightIdentity) of
        (Just leftOwner, Just rightOwner) -> leftOwner == rightOwner
        (Nothing, Nothing)
          | referenceMode == MetadataLight ->
              case (structuralRecursiveDataName leftName, structuralRecursiveDataName rightName) of
                (Just leftDataName, Just rightDataName) -> backendPrimitiveDataNameMatches leftDataName rightDataName
                _ -> False
        _ -> False

    structuralPrimitiveDataOwnerMatches muIdentity muName dataIdentity con =
      case (structuralSelfIdentityUnique muIdentity, symbolUniqueIdentity <$> dataIdentity) of
        (Just structuralOwner, Just nominalOwner) -> structuralOwner == nominalOwner
        (Nothing, Nothing)
          | referenceMode == MetadataLight ->
              case structuralRecursiveDataName muName of
                Just dataName -> backendPrimitiveDataNameMatches dataName (getBaseName con)
                Nothing -> False
        _ -> False

    structuralPrimitivePayloadTypes muIdentity muName body =
      filter (not . structuralSelfField muIdentity muName) <$> structuralMuPayloadTypes body

    structuralSelfField muIdentity muName =
      \case
        BTVarWithIdentity fieldIdentity fieldName ->
          typeBinderRefMatchesWith IdentityOnly muIdentity muName fieldIdentity fieldName
            || ( referenceMode == MetadataLight
                   && case structuralRecursiveDataName muName of
                     Just dataName -> structuralDataSelfFieldMatches dataName muIdentity fieldIdentity fieldName
                     Nothing -> False
               )
        _ ->
          False

    backendPrimitiveDataNameMatches leftName rightName =
      normalizeBackendBuiltinTypeReference leftName == normalizeBackendBuiltinTypeReference rightName
        || leftName == unqualifiedBackendDataName rightName
        || unqualifiedBackendDataName leftName == rightName
        || unqualifiedBackendDataName leftName == unqualifiedBackendDataName rightName

backendVariableTypeMatchesWithBounds :: ReferenceMode -> BackendParameterBounds -> BackendType -> BackendType -> Bool
backendVariableTypeMatchesWithBounds typeBoundMode typeBounds expectedTy actualTy =
  backendTypeMatchesWith
    typeBoundMode
    RejectFreeTypeVariableInstantiation
    typeBounds
    Nothing
    expectedTy
    actualTy

backendTypeMatchesWith ::
  ReferenceMode ->
  TypeVariableInstantiation ->
  BackendParameterBounds ->
  Maybe BackendDataScope ->
  BackendType ->
  BackendType ->
  Bool
backendTypeMatchesWith typeBoundMode typeVariableInstantiation typeBounds mbDataDecls expectedTy actualTy =
  go Set.empty expectedTy actualTy
  where
    typeHeadMatches =
      backendTypeHeadMatchesWith typeBoundMode

    typeBinderMatches =
      typeBinderRefMatchesWith typeBoundMode

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
              (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase) ->
                typeHeadMatches expectedIdentity expectedBase actualIdentity actualBase
              (BTBaseWithIdentity expectedDataIdentity expectedBase, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralMuMatchesKnownData expectedDataIdentity expectedBase [] actualIdentity actualName actualBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTBaseWithIdentity actualDataIdentity actualBase) ->
                structuralMuMatchesKnownData actualDataIdentity actualBase [] expectedIdentity expectedName expectedBody
              (BTConWithIdentity expectedIdentity expectedCon (_ :| []), BTConWithIdentity actualIdentity actualCon (_ :| []))
                | opaqueIOBackendHeadMatches expectedIdentity expectedCon && opaqueIOBackendHeadMatches actualIdentity actualCon ->
                    True
              (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs) ->
                typeHeadMatches expectedIdentity expectedCon actualIdentity actualCon
                  && zipAllWith
                    (metadataBackedTypeArgumentMatchesEither (metadataBackedTypeHead expectedIdentity expectedCon) bound)
                    (NE.toList expectedArgs)
                    (NE.toList actualArgs)
              (BTConWithIdentity expectedDataIdentity expectedCon expectedArgs, BTMuWithIdentity actualIdentity actualName actualBody) ->
                nominalStructuralTypeVarArgsMatch expectedDataIdentity expectedCon (NE.toList expectedArgs) actualIdentity actualName
                  || structuralMuMatchesKnownData expectedDataIdentity expectedCon (NE.toList expectedArgs) actualIdentity actualName actualBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTConWithIdentity actualDataIdentity actualCon actualArgs) ->
                nominalStructuralTypeVarArgsMatch actualDataIdentity actualCon (NE.toList actualArgs) expectedIdentity expectedName
                  || structuralMuMatchesKnownData actualDataIdentity actualCon (NE.toList actualArgs) expectedIdentity expectedName expectedBody
              (BTVarAppWithIdentity expectedIdentity expectedName expectedArgs, BTVarAppWithIdentity actualIdentity actualName actualArgs) ->
                typeBinderMatches expectedIdentity expectedName actualIdentity actualName
                  && zipAllWith (go bound) (NE.toList expectedArgs) (NE.toList actualArgs)
              (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity actualName actualBound actualBody) ->
                maybeBoundMatches bound expectedBound actualBound
                  && let freshName = freshBinderName expectedName actualName expectedBound actualBound expectedBody actualBody
                         freshTy = freshBinderTy expectedIdentity actualIdentity freshName
                         freshKey = freshBinderKey expectedIdentity actualIdentity freshName
                         expectedBody' = substituteBackendTypeForBinder expectedIdentity expectedName freshTy expectedBody
                         actualBody' = substituteBackendTypeForBinder actualIdentity actualName freshTy actualBody
                      in go (Set.insert freshKey bound) expectedBody' actualBody'
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
                let freshName = freshBinderName expectedName actualName Nothing Nothing expectedBody actualBody
                    freshTy = freshBinderTy expectedIdentity actualIdentity freshName
                    freshKey = freshBinderKey expectedIdentity actualIdentity freshName
                    expectedBody' = substituteBackendTypeForBinder expectedIdentity expectedName freshTy expectedBody
                    actualBody' = substituteBackendTypeForBinder actualIdentity actualName freshTy actualBody
                    bodiesMatch = go (Set.insert freshKey bound) expectedBody' actualBody'
                 in if sameStructuralDataOwner expectedIdentity expectedName actualIdentity actualName
                  then
                    (typeBinderMatches expectedIdentity expectedName actualIdentity actualName && bodiesMatch)
                      || structuralMuPayloadMayInstantiate expectedIdentity expectedName expectedBody actualIdentity actualName actualBody
                  else case (isVacuousRecursiveBinderWithIdentity expectedIdentity expectedName expectedBody, isVacuousRecursiveBinderWithIdentity actualIdentity actualName actualBody) of
                    (True, True) ->
                      go bound expectedBody actualBody
                    (True, False) ->
                      vacuousRecursiveWrapperMayUnwrap expectedBody
                        && (recursiveBodyCompatibleWithIdentity actualIdentity actualName actualBody expectedBody || go bound expectedBody actual)
                    (False, True) ->
                      vacuousRecursiveWrapperMayUnwrap actualBody
                        && (recursiveBodyCompatibleWithIdentity expectedIdentity expectedName expectedBody actualBody || go bound expected actualBody)
                    (False, False) ->
                      bodiesMatch
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, _)
                | isVacuousRecursiveBinderWithIdentity expectedIdentity expectedName expectedBody,
                  vacuousRecursiveWrapperMayUnwrap expectedBody ->
                    go bound expectedBody actual
              (_, BTMuWithIdentity actualIdentity actualName actualBody)
                | isVacuousRecursiveBinderWithIdentity actualIdentity actualName actualBody,
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
          (BTBaseWithIdentity Nothing base, BTMuWithIdentity _ muName _, Just dataScope) ->
            identitylessHeadNeedsScopedData dataScope base muName
          (BTMuWithIdentity _ muName _, BTBaseWithIdentity Nothing base, Just dataScope) ->
            identitylessHeadNeedsScopedData dataScope base muName
          (BTConWithIdentity Nothing base _, BTMuWithIdentity _ muName _, Just dataScope) ->
            identitylessHeadNeedsScopedData dataScope base muName
          (BTMuWithIdentity _ muName _, BTConWithIdentity Nothing base _, Just dataScope) ->
            identitylessHeadNeedsScopedData dataScope base muName
          _ -> False

    identitylessHeadNeedsScopedData dataScope (BaseTy name) muName =
      identityBearingScopedDataName dataScope name
        || maybe False (identityBearingScopedDataName dataScope) (structuralRecursiveDataName muName)

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
        (BTConWithIdentity expectedIdentity expectedCon (_ :| []), BTConWithIdentity actualIdentity actualCon (_ :| [])) ->
          opaqueIOBackendHeadMatches expectedIdentity expectedCon && opaqueIOBackendHeadMatches actualIdentity actualCon
        _ ->
          False

    opaqueIODomainCompatible bound expected actual =
      alphaEqWithinDataScope expected actual
        || typeVariableBoundMatches bound expected actual
        || typeVariableBoundMatches bound actual expected
        || case (expected, actual) of
          (BTVarWithIdentity expectedIdentity expectedName, _)
            | freeTypeVariableMayInstantiate bound expectedIdentity expectedName -> True
          (_, BTVarWithIdentity actualIdentity actualName)
            | freeTypeVariableMayInstantiate bound actualIdentity actualName -> True
          (BTVarWithIdentity expectedIdentity expectedName, BTVarWithIdentity actualIdentity actualName) ->
            typeBinderMatches expectedIdentity expectedName actualIdentity actualName
          _ -> False

    typeVariableBoundMatches bound ty otherTy =
      case ty of
        BTVarWithIdentity identity name
          | Just key <- typeBoundReferenceKey typeBoundMode identity name,
            Set.notMember key bound ->
              case Map.lookup key typeBounds of
                Just (Just boundTy)
                  | not (alphaEqBackendType boundTy BTBottom) ->
                      go bound boundTy otherTy
                _ ->
                  False
        _ ->
          False

    sameStructuralDataOwner expectedIdentity expectedName actualIdentity actualName =
      case (structuralSelfIdentityUnique expectedIdentity, structuralSelfIdentityUnique actualIdentity, expectedIdentity, actualIdentity) of
        (Just expectedUnique, Just actualUnique, _, _) -> expectedUnique == actualUnique
        (Nothing, Nothing, Nothing, Nothing)
          | typeBoundMode == MetadataLight ->
              structuralRecursiveDataName expectedName == structuralRecursiveDataName actualName
        _ -> False

    nominalStructuralTypeVarArgsMatch dataIdentity (BaseTy dataName) args muIdentity muName =
      nominalStructuralOwnerMatches dataIdentity dataName muIdentity muName
        && all freeTypeVarArg args
      where
        nominalStructuralOwnerMatches (Just identity) _ identity0 _ =
          structuralSelfIdentityUnique identity0 == Just (symbolUniqueIdentity identity)
        nominalStructuralOwnerMatches Nothing dataName0 Nothing muName0 =
          typeBoundMode == MetadataLight
            && structuralRecursiveDataName muName0 == Just dataName0
        nominalStructuralOwnerMatches _ _ _ _ =
          False

        freeTypeVarArg =
          \case
            BTVarWithIdentity identity name ->
              case typeBoundReferenceKey typeBoundMode identity name of
                Just key -> not (hasConcreteTypeBound key)
                Nothing -> False
            _ ->
              False

    metadataBackedTypeArgumentMatches metadataBacked bound expected actual =
      go bound expected actual || (metadataBacked && freeExpectedTypeVariableMayInstantiate bound expected)

    metadataBackedTypeArgumentMatchesEither metadataBacked bound expected actual =
      metadataBackedTypeArgumentMatches metadataBacked bound expected actual
        || metadataBackedTypeArgumentMatches metadataBacked bound actual expected

    freeExpectedTypeVariableMayInstantiate bound =
      \case
        BTVarWithIdentity identity name ->
          case typeBoundReferenceKey typeBoundMode identity name of
            Just key ->
              Set.notMember key bound
                && not (hasConcreteTypeBound key)
            Nothing ->
              False
        _ ->
          False

    freeTypeVariableMayInstantiate bound identity name =
      case typeBoundReferenceKey typeBoundMode identity name of
        Just key -> Set.notMember key bound
        Nothing -> False

    hasConcreteTypeBound key =
      case Map.lookup key typeBounds of
        Just (Just boundTy) -> not (alphaEqBackendType boundTy BTBottom)
        _ -> False

    metadataBackedTypeHead dataIdentity (BaseTy name) =
      case dataIdentity >>= lookupDataByIdentity of
        Just {} -> True
        Nothing
          | Just {} <- dataIdentity,
            Just {} <- mbDataDecls ->
              False
        Nothing ->
          case typeBoundMode of
            IdentityOnly -> False
            MetadataLight -> maybe False (Map.member name . backendDataScopeByName) mbDataDecls

    structuralMuMatchesKnownData dataIdentity base@(BaseTy dataName) args muIdentity muName body =
      structuralSelfIdentityCompatibleWithDataIdentity dataIdentity muIdentity
        && case structuralSelfIdentityUnique muIdentity of
          Just {} -> structuralDataDeclMatchesKnownIdentity
          Nothing -> metadataLightStructuralMatches || structuralDataDeclMatchesKnownIdentity
      where
        structuralDataDeclMatchesKnownIdentity =
          maybe False structuralDataDeclMatches (matchingDataDecl dataIdentity dataName muIdentity muName)

        metadataLightStructuralMatches =
          ( metadataLightAllowed dataIdentity dataName
              && metadataLightStructuralDataMatchesWithIdentity base args muIdentity muName body
          )
            || maybe
              False
              ( \structuralName ->
                  PrimitiveInventory.matchesBuiltinTypeName dataName structuralName
                    && metadataLightAllowed dataIdentity structuralName
                    && metadataLightStructuralDataMatchesWithIdentity (BaseTy structuralName) args muIdentity muName body
              )
              (structuralRecursiveDataName muName)

        structuralDataDeclMatches dataDecl
          | Just substitution <- structuralDataArgumentSubstitution dataDecl args =
              structuralDataDeclarationMatches typeBounds dataDecl substitution (BTMuWithIdentity muIdentity muName body)
        structuralDataDeclMatches _ =
          False

    structuralSelfIdentityCompatibleWithDataIdentity dataIdentity muIdentity =
      case (dataIdentity, structuralSelfIdentityUnique muIdentity) of
        (Just identity, Just unique) -> symbolUniqueIdentity identity == unique
        (Just {}, Nothing) -> False
        _ -> True

    metadataLightAllowed dataIdentity dataName =
      case (dataIdentity, mbDataDecls) of
        (Just {}, _) -> False
        (Nothing, Just dataScope) ->
          not (identityBearingScopedDataName dataScope dataName)
        _ -> True

    identityBearingScopedDataName dataScope dataName =
      identityBearingDataName dataName (backendDataScopeByName dataScope)
        || identityBearingDataName dataName (backendDataScopeByIdentity dataScope)

    identityBearingDataName dataName =
      any
        ( \dataDecl ->
            backendDataName dataDecl == dataName
              && backendDataIdentity dataDecl /= Nothing
        )
        . Map.elems

    matchingDataDecl dataIdentity dataName muIdentity muName =
      case typeBoundMode of
        IdentityOnly ->
          matchingDataDeclByIdentity dataIdentity muIdentity
        MetadataLight ->
          matchingDataDeclMetadataLight dataIdentity dataName muIdentity muName

    matchingDataDeclByIdentity dataIdentity muIdentity =
      case dataIdentity >>= lookupDataByIdentity of
        Just dataDecl
          | structuralSelfIdentityMatchesDataByIdentity muIdentity dataDecl -> Just dataDecl
          | otherwise -> Nothing
        Nothing ->
          lookupDataByStructuralSelfIdentity muIdentity

    matchingDataDeclMetadataLight dataIdentity dataName muIdentity muName =
      case dataIdentity >>= lookupDataByIdentity of
        Just dataDecl
          | structuralSelfIdentityMatchesDataByIdentity muIdentity dataDecl -> Just dataDecl
          | otherwise -> Nothing
        Nothing
          | Just {} <- dataIdentity,
            Just {} <- mbDataDecls ->
              Nothing
          | Just {} <- structuralSelfIdentityUnique muIdentity ->
              lookupDataByStructuralSelfIdentity muIdentity
        Nothing ->
          foldr ((<|>) . lookupDataByName) Nothing (structuralLookupNames dataName muName)

    lookupDataByName name = do
      dataDecls <- backendDataScopeByName <$> mbDataDecls
      Map.lookup name dataDecls

    lookupDataByIdentity identity = do
      dataDeclsByIdentity <- backendDataScopeByIdentity <$> mbDataDecls
      lookupSymbolIdentityExact identity dataDeclsByIdentity

    lookupDataByStructuralSelfIdentity muIdentity = do
      unique <- structuralSelfIdentityUnique muIdentity
      dataDeclsByIdentity <- backendDataScopeByIdentity <$> mbDataDecls
      case [ dataDecl
           | dataDecl <- Map.elems dataDeclsByIdentity,
             Just dataDeclIdentity <- [backendDataIdentity dataDecl],
             symbolUniqueIdentity dataDeclIdentity == unique
           ] of
        [dataDecl] -> Just dataDecl
        _ -> Nothing

    structuralSelfIdentityMatchesDataByIdentity muIdentity dataDecl =
      case structuralSelfIdentityUnique muIdentity of
        Just unique ->
          case backendDataIdentity dataDecl of
            Just dataDeclIdentity -> symbolUniqueIdentity dataDeclIdentity == unique
            Nothing -> False
        Nothing -> False

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

    freshBinderTy leftIdentity rightIdentity name =
      BTVarWithIdentity (leftIdentity <|> rightIdentity) name

    freshBinderKey leftIdentity rightIdentity name =
      backendTypeSubstitutionKeyFromMaybeMetadataLight (leftIdentity <|> rightIdentity) name

validateBackendTypeArgumentBound :: Maybe BackendType -> BackendType -> Either BackendValidationError ()
validateBackendTypeArgumentBound Nothing _ =
  pure ()
validateBackendTypeArgumentBound (Just BTBottom) _ =
  pure ()
validateBackendTypeArgumentBound (Just boundTy) actualTy =
  unless (alphaEqBackendType actualTy boundTy) $
    Left (BackendTypeAppBoundMismatch boundTy actualTy)

lookupBackendVariable :: BackendValidationContext -> Maybe IdDetails -> String -> Maybe BackendType
lookupBackendVariable context0 mbIdentity name =
  case mbIdentity of
    Just details
      | Just key <- idDetailsLocalKey details ->
          Map.lookup (BackendIdentityKey key) (bvcLocals context0)
      | Just identity <- idDetailsSymbolIdentity details ->
          Map.lookup (BackendIdentityKey identity) (bvcGlobals context0)
            <|> lookupPrimitiveRuntimeVariable context0 identity
    _ ->
      Map.lookup (BackendMetadataLightKey name) (bvcLocals context0)
        <|> Map.lookup (BackendMetadataLightKey name) (bvcGlobals context0)

lookupPrimitiveRuntimeVariable :: BackendValidationContext -> SymbolIdentity -> Maybe BackendType
lookupPrimitiveRuntimeVariable context0 identity = do
  primitiveName <- PrimitiveInventory.primitiveValueNameByIdentity identity
  Map.lookup (BackendIdentityKey (builtinValueIdentity primitiveName)) (bvcGlobals context0)

idDetailsLocalKey :: IdDetails -> Maybe BackendLocalKey
idDetailsLocalKey =
  \case
    LocalId ref -> Just (BackendLocalRef ref)
    EvidenceId ref -> Just (BackendLocalRef ref)
    EnvId ref -> Just (BackendEnvRef ref)
    DeferredId ref -> Just (BackendDeferredRef ref)
    _ -> Nothing

extendLocalMaybe :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> Maybe BackendValidationContext
extendLocalMaybe mbContext mbIdentity name ty =
  fmap (\context0 -> extendLocal context0 mbIdentity name ty) mbContext

extendFunctionParamLocalMaybe :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendExpr -> Maybe BackendValidationContext
extendFunctionParamLocalMaybe mbContext mbIdentity name ty body
  | backendExprCallsBinderAsClosureHead MetadataLight (backendCallableRef mbIdentity name) body =
      extendClosureLocalMaybe mbContext mbIdentity name ty
  | otherwise =
      extendLocalMaybe mbContext mbIdentity name ty

extendLocal :: BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendValidationContext
extendLocal context0 mbIdentity name ty =
  context0
    { bvcLocals = bindLocalReference mbIdentity name ty (bvcLocals context0),
      bvcCasePatternLocals = deleteLocalIdentity mbIdentity (bvcCasePatternLocals context0),
      bvcClosureLocals = deleteLocalReference mbIdentity name (bvcClosureLocals context0),
      bvcPossibleClosureLocals = deleteLocalReference mbIdentity name (bvcPossibleClosureLocals context0)
    }

extendClosureLocalMaybe :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> Maybe BackendValidationContext
extendClosureLocalMaybe mbContext mbIdentity name ty =
  fmap (\context0 -> extendClosureLocal context0 mbIdentity name ty) mbContext

extendClosureLocal :: BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendValidationContext
extendClosureLocal context0 mbIdentity name ty =
  context0
    { bvcLocals = bindLocalReference mbIdentity name ty (bvcLocals context0),
      bvcCasePatternLocals = deleteLocalIdentity mbIdentity (bvcCasePatternLocals context0),
      bvcClosureLocals = insertLocalReference mbIdentity name (bvcClosureLocals context0),
      bvcPossibleClosureLocals = deleteLocalReference mbIdentity name (bvcPossibleClosureLocals context0)
    }

extendPossibleClosureLocalMaybe :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> Maybe BackendValidationContext
extendPossibleClosureLocalMaybe mbContext mbIdentity name ty =
  fmap (\context0 -> extendPossibleClosureLocal context0 mbIdentity name ty) mbContext

extendPossibleClosureLocal :: BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendValidationContext
extendPossibleClosureLocal context0 mbIdentity name ty =
  context0
    { bvcLocals = bindLocalReference mbIdentity name ty (bvcLocals context0),
      bvcCasePatternLocals = deleteLocalIdentity mbIdentity (bvcCasePatternLocals context0),
      bvcClosureLocals = deleteLocalReference mbIdentity name (bvcClosureLocals context0),
      bvcPossibleClosureLocals = insertLocalReference mbIdentity name (bvcPossibleClosureLocals context0)
    }

localReferenceKey :: Maybe IdDetails -> String -> BackendReferenceKey BackendLocalKey
localReferenceKey mbIdentity name =
  maybe (BackendMetadataLightKey name) BackendIdentityKey (mbIdentity >>= idDetailsLocalKey)

bindLocalReference :: Maybe IdDetails -> String -> BackendType -> Map.Map (BackendReferenceKey BackendLocalKey) BackendType -> Map.Map (BackendReferenceKey BackendLocalKey) BackendType
bindLocalReference mbIdentity name ty =
  Map.insert (localReferenceKey mbIdentity name) ty
    . Map.delete (BackendMetadataLightKey name)

insertLocalReference :: Maybe IdDetails -> String -> Set.Set (BackendReferenceKey BackendLocalKey) -> Set.Set (BackendReferenceKey BackendLocalKey)
insertLocalReference mbIdentity name =
  Set.insert (localReferenceKey mbIdentity name)
    . Set.delete (BackendMetadataLightKey name)

deleteLocalReference :: Maybe IdDetails -> String -> Set.Set (BackendReferenceKey BackendLocalKey) -> Set.Set (BackendReferenceKey BackendLocalKey)
deleteLocalReference mbIdentity name =
  Set.delete (localReferenceKey mbIdentity name)
    . Set.delete (BackendMetadataLightKey name)

insertLocalIdentityKey :: Maybe IdDetails -> Set.Set BackendLocalKey -> Set.Set BackendLocalKey
insertLocalIdentityKey mbIdentity keys =
  maybe keys (`Set.insert` keys) (mbIdentity >>= idDetailsLocalKey)

deleteLocalIdentity :: Maybe IdDetails -> Set.Set BackendLocalKey -> Set.Set BackendLocalKey
deleteLocalIdentity mbIdentity keys =
  maybe keys (`Set.delete` keys) (mbIdentity >>= idDetailsLocalKey)

extendLetLocalMaybe :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendExpr -> Maybe BackendValidationContext
extendLetLocalMaybe mbContext mbIdentity name ty rhs =
  extendClosureShapeLocalMaybe mbContext mbContext mbIdentity name ty rhs

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
    (backendClosureCaptureName capture)
    (backendClosureCaptureType capture)
    (backendClosureCaptureExpr capture)

extendClosureShapeLocalMaybe ::
  Maybe BackendValidationContext ->
  Maybe BackendValidationContext ->
  Maybe IdDetails ->
  String ->
  BackendType ->
  BackendExpr ->
  Maybe BackendValidationContext
extendClosureShapeLocalMaybe sourceContext targetContext mbIdentity name ty rhs
  | not (backendTypeIsClosureValue ty) =
      extendLocalMaybe targetContext mbIdentity name ty
  | otherwise =
      case backendCallableHeadInContext sourceContext rhs of
        BackendClosureCallableHead _ ->
          extendClosureLocalMaybe targetContext mbIdentity name ty
        BackendUnknownCallableHead ->
          extendPossibleClosureLocalMaybe targetContext mbIdentity name ty
        BackendDirectCallableHead _ ->
          extendLocalMaybe targetContext mbIdentity name ty

extendPatternLocals :: BackendValidationContext -> [(BackendBinderRef, BackendType)] -> BackendValidationContext
extendPatternLocals =
  foldr extendOne
  where
    extendOne (ref, ty) context0
      | backendTypeIsClosureValue ty = markCasePatternLocal mbIdentity (extendClosureLocal context0 mbIdentity name ty)
      | otherwise = markCasePatternLocal mbIdentity (extendLocal context0 mbIdentity name ty)
      where
        mbIdentity = backendCallableRefIdentity ref
        name = backendCallableRefName ref

    markCasePatternLocal mbIdentity context0 =
      context0
        { bvcCasePatternLocals = insertLocalIdentityKey mbIdentity (bvcCasePatternLocals context0)
        }

backendTypeIsClosureValue :: BackendType -> Bool
backendTypeIsClosureValue =
  \case
    BTArrow {} -> True
    _ -> False

opaqueIOBackendHeadMatches :: Maybe SymbolIdentity -> BaseTy -> Bool
opaqueIOBackendHeadMatches (Just identity) _ =
  identity == PrimitiveInventory.builtinTypeIdentity "IO"
opaqueIOBackendHeadMatches Nothing _ =
  False

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
            Just identity -> (BTVarWithIdentity (Just identity) name, generator)
            Nothing ->
              let (identity, generator') = freshBackendTypeIdentity generator
               in (BTVarWithIdentity (Just identity) name, generator')
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
           in (BTForallWithIdentity (Just identity) name Nothing body', generator2)
        PrimitiveInventory.PrimitiveTypeMu name body ->
          let (identity, generator1) =
                case primitiveStructuralOwnerIdentity name of
                  Just ownerIdentity -> (ownerIdentity, generator)
                  Nothing -> freshBackendTypeIdentity generator
              (body', generator2) = go (Map.insert name identity env) generator1 body
           in (BTMuWithIdentity (Just identity) name body', generator2)

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
      lookupPrimitiveTypeHeadIdentity headIdentities0 name <|> builtinTypeHeadIdentity name

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

extendTypeBoundMaybe :: Maybe BackendValidationContext -> Maybe TypeBinderIdentity -> String -> Maybe BackendType -> Maybe BackendValidationContext
extendTypeBoundMaybe mbContext identity name mbBound =
  fmap
    ( \context0 ->
        context0
          { bvcTypeBounds =
              Map.insert
                (backendTypeSubstitutionKeyFromMaybeMetadataLight identity name)
                mbBound
                (bvcTypeBounds context0)
          }
    )
    mbContext

extendTypeBounds :: BackendValidationContext -> [(BackendTypeSubstitutionKey, Maybe BackendType)] -> BackendValidationContext
extendTypeBounds context0 bounds =
  context0 {bvcTypeBounds = foldr (uncurry Map.insert) (bvcTypeBounds context0) bounds}

lookupBackendConstructorInfo :: BackendValidationContext -> Maybe SymbolIdentity -> String -> Maybe BackendConstructorInfo
lookupBackendConstructorInfo context0 mbIdentity name =
  Map.lookup (backendReferenceKey mbIdentity name) (bvcConstructors context0)

canonicalizeBackendTypeDataHeads :: BackendValidationContext -> BackendType -> BackendType
canonicalizeBackendTypeDataHeads context0 =
  canonicalizeBackendTypeDataHeadsWith
    (metadataLightEntries (bvcData context0))
    (identityEntries (bvcData context0))

structuralSelfIdentityUnique :: Maybe TypeBinderIdentity -> Maybe UniqueIdentity
structuralSelfIdentityUnique identity = do
  selfIdentity <- identity
  (unique, StructuralSelfBinder) <- typeBinderIdentityStructural selfIdentity
  pure unique

canonicalizeBackendTypeDataHeadsWith :: Map.Map String BackendData -> Map.Map SymbolIdentity BackendData -> BackendType -> BackendType
canonicalizeBackendTypeDataHeadsWith dataDecls dataDeclsByIdentity =
  go
  where
    go ty =
      case ty of
        BTBaseWithIdentity mbIdentity (BaseTy name) ->
          let (mbIdentity', name') = canonicalHead mbIdentity name
           in BTBaseWithIdentity mbIdentity' (BaseTy name')
        BTConWithIdentity mbIdentity (BaseTy name) args ->
          let (mbIdentity', name') = canonicalHead mbIdentity name
           in BTConWithIdentity mbIdentity' (BaseTy name') (fmap go args)
        BTArrow dom cod ->
          BTArrow (go dom) (go cod)
        BTVarAppWithIdentity identity name args ->
          BTVarAppWithIdentity identity name (fmap go args)
        BTForallWithIdentity identity name mb body ->
          BTForallWithIdentity identity name (fmap go mb) (go body)
        BTMuWithIdentity identity name body ->
          let (name', body0) = canonicalizeBuiltinStructuralMuBinder identity name body
           in case recoverStructuralDataType identity name' body0 of
                Just recovered -> go recovered
                Nothing ->
                  let body' = go body0
                   in case recoverStructuralDataType identity name' body' of
                        Just recovered -> recovered
                        Nothing -> BTMuWithIdentity identity name' body'
        _ ->
          ty

    canonicalHead mbIdentity name =
      case mbIdentity of
        Just identity ->
          case lookupSymbolIdentityExact identity dataDeclsByIdentity of
            Just dataDecl -> (backendDataIdentity dataDecl <|> mbIdentity, backendDataName dataDecl)
            Nothing -> (mbIdentity, name)
        Nothing ->
          case
            Map.lookup name dataDecls
              <|> uniqueDataDeclByDisplayName name (Map.elems dataDecls) of
            Just dataDecl -> (backendDataIdentity dataDecl, backendDataName dataDecl)
            Nothing -> (Nothing, name)

    canonicalizeBuiltinStructuralMuBinder identity name body =
      case structuralRecursiveDataName name of
        Just dataName
          | let normalizedDataName = normalizeBackendBuiltinTypeReference dataName,
            normalizedDataName /= dataName ->
              let canonicalName = "$" ++ normalizedDataName ++ "_self"
               in ( canonicalName,
                    substituteBackendTypeForBinder identity name (BTVarWithIdentity identity canonicalName) body
                  )
        _ ->
          (name, body)

    recoverStructuralDataType identity name body =
      case structuralDataByIdentity <|> structuralDataByFallback of
        Just dataDecl -> do
          args <- structuralBackendDataArguments identity name dataDecl body
          Just (backendDataType dataDecl args)
        Nothing ->
          Nothing
      where
        structuralDataByIdentity = do
          unique <- structuralSelfIdentityUnique identity
          case [ dataDecl
               | dataDecl <- Map.elems dataDeclsByIdentity,
                 Just dataIdentity <- [backendDataIdentity dataDecl],
                 symbolUniqueIdentity dataIdentity == unique
               ] of
            [dataDecl] -> Just dataDecl
            _ -> Nothing

        structuralDataByFallback
          | not (structuralIdentityAllowsNameFallback identity) = Nothing
          | otherwise = structuralDataByName <|> structuralDataByBody

        structuralDataByName =
          structuralRecursiveDataName name >>= (`Map.lookup` dataDecls)
        structuralDataByBody =
          let matches =
                [ dataDecl
                | dataDecl <- uniqueDataDecls,
                  Just _ <- [structuralBackendDataArguments identity name dataDecl body]
                ]
           in case matches of
                [dataDecl] -> Just dataDecl
                _ -> Nothing

    uniqueDataDecls =
      Map.elems (Map.fromList [(backendDataName dataDecl, dataDecl) | dataDecl <- Map.elems dataDecls])

    backendDataType dataDecl args =
      case args of
        [] -> BTBaseWithIdentity (backendDataIdentity dataDecl) (BaseTy (backendDataName dataDecl))
        arg : rest -> BTConWithIdentity (backendDataIdentity dataDecl) (BaseTy (backendDataName dataDecl)) (arg :| rest)

    structuralBackendDataArguments muIdentity muName dataDecl body = do
      handlerFields <- structuralBackendHandlerFields body
      let dataParameterRefs = backendDataParameterRefs dataDecl
          dataParameterKeys = backendDataParameterKeys dataDecl
          constructors = backendDataConstructors dataDecl
          parameterBounds = Map.fromList [(key, Nothing) | key <- dataParameterKeys]
      if length handlerFields == length constructors
        then do
          substitution <-
            foldM
              (matchConstructorFields muIdentity muName dataDecl dataParameterRefs parameterBounds)
              Map.empty
              (zip constructors handlerFields)
          let completedSubstitution = completeBackendParameterSubstitution parameterBounds substitution
          Just
            [ Map.findWithDefault (backendDataParameterRefType ref) key completedSubstitution
            | (ref, key) <- zip dataParameterRefs dataParameterKeys
            ]
        else Nothing

    matchConstructorFields muIdentity muName dataDecl dataParameterRefs parameterBounds substitution (constructor, fields) =
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
                  (go (recoverDataSelfField muIdentity muName dataDecl actualTy))
            )
            substitution
            (zip (backendConstructorFields constructor) fields)
        else Nothing

    constructorParameterBounds parameterBounds constructor =
      parameterBounds
        `Map.union` Map.fromList
          [ (backendTypeSubstitutionKeyFromMaybeMetadataLight (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderBound binder)
          | binder <- backendConstructorForalls constructor
          ]

    recoverDataSelfField muIdentity _muName dataDecl ty =
      case ty of
        BTVarWithIdentity fieldIdentity fieldName
          | structuralDataSelfField fieldIdentity fieldName ->
              backendDataType dataDecl dataSelfArgs
        _ ->
          ty
      where
        structuralDataSelfField fieldIdentity fieldName =
          structuralDataSelfFieldMatches (backendDataName dataDecl) muIdentity fieldIdentity fieldName

        dataSelfArgs =
          map backendDataParameterRefType (backendDataParameterRefs dataDecl)

    uniqueDataDeclByDisplayName name candidates =
      case filter dataDeclDisplayNameMatches candidates of
        [dataDecl] -> Just dataDecl
        _ -> Nothing
      where
        dataDeclDisplayNameMatches dataDecl =
          let backendName = backendDataName dataDecl
           in name == backendName || name == unqualifiedBackendDataName backendName

normalizeBackendBuiltinTypeReference :: String -> String
normalizeBackendBuiltinTypeReference name =
  case stripPrefix "Prelude." name of
    Just unqualifiedName
      | PrimitiveInventory.isBuiltinTypeName unqualifiedName ->
          unqualifiedName
    _ ->
      PrimitiveInventory.normalizeBuiltinTypeReference name

unqualifiedBackendDataName :: String -> String
unqualifiedBackendDataName =
  reverse . takeWhile (/= '.') . reverse

validateBackendConstructorUse :: Maybe BackendValidationContext -> Maybe SymbolIdentity -> String -> BackendType -> [BackendExpr] -> Either BackendValidationError ()
validateBackendConstructorUse Nothing _ _ _ _ =
  pure ()
validateBackendConstructorUse (Just context0) mbIdentity name resultTy0 args =
  case lookupBackendConstructorInfo context0 mbIdentity name of
    Nothing ->
      Left (BackendUnknownConstructor name)
    Just constructorInfo -> do
      let constructor = bciConstructor constructorInfo
          typeBoundMode = MetadataLight
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
            | backendConstructorResultPlaceholderMatchesEither typeBoundMode (bvcTypeBounds context0) constructorResultTy resultTy ->
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
          (validateBackendConstructorArgument typeBoundMode (bvcTypeBounds context0) (Just dataScope) dataParameters parameters name)
          substitution
          (zip [0 ..] (zip fields args))
      validateBackendConstructorResultSubstitution
        (bvcTypeBounds context0)
        typeBoundMode
        (Just dataScope)
        constructorInfo
        finalSubstitution
        resultTy
        (BackendConstructorResultMismatch name constructorResultTy resultTy)
      pure ()

validateBackendConstructorArgument ::
  ReferenceMode ->
  BackendParameterBounds ->
  Maybe BackendDataScope ->
  [BackendDataParameterRef] ->
  BackendParameterBounds ->
  String ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  (Int, (BackendType, BackendExpr)) ->
  Either BackendValidationError (Map.Map BackendTypeSubstitutionKey BackendType)
validateBackendConstructorArgument typeBoundMode typeBounds mbDataDecls dataParameters parameters name substitution (index0, (expectedTy, arg)) =
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
          && backendVariableTypeMatchesWithBounds typeBoundMode typeBounds substitutedExpectedTy argTy
           )
        || backendVariableTypeMatchesWithBounds typeBoundMode typeBounds substitutedExpectedTy argTy
        || backendStructuralDataBoundaryMatchesWith
          typeBoundMode
          typeBounds
          mbDataDecls
          substitutedExpectedTy
          argTy

    backendFieldPlaceholderMatches expected actual =
      case (expected, actual) of
        (BTVarWithIdentity identity tyName, _)
          | placeholderTypeVariable identity tyName ->
              True
        (_, BTVarWithIdentity identity tyName)
          | placeholderTypeVariable identity tyName ->
              True
        _ ->
          False

    placeholderTypeVariable identity tyName =
      case typeBoundReferenceKey typeBoundMode identity tyName of
        Just key ->
          Map.notMember key parameters
            && Map.notMember key typeBounds
        Nothing ->
          False

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
  case lookupBackendConstructorInfo context0 mbIdentity name of
    Nothing ->
      Left (BackendUnknownConstructor name)
    Just constructorInfo -> do
      let constructor = bciConstructor constructorInfo
          dataParameters = constructorResultParameterRefs constructorInfo
          parameters = constructorResultParameterBounds constructorInfo
          fields = backendConstructorFields constructor
          binderNames = map backendPatternBinderName binders
          scrutineeTy = scrutineeTy0
          constructorResultTy = backendConstructorResult constructor
          typeBoundMode = MetadataLight
      requireUniqueBy BackendDuplicatePatternBinding (map patternBinderRef binders)
      unless (length fields == length binderNames) $
        Left (BackendPatternArityMismatch name (length fields) (length binderNames))
      substitution <-
        case matchBackendTypeParametersWithTypeBounds (bvcTypeBounds context0) dataParameters parameters Map.empty constructorResultTy scrutineeTy of
          Just substitution -> pure substitution
          Nothing
            | backendTypeRefinesScrutineeWith
                typeBoundMode
                constructorResultTy
                scrutineeTy ->
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
            completeDataParameterSubstitution (constructorInfoDataDecl constructorInfo) $
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
      [ (backendTypeSubstitutionKeyFromMaybeMetadataLight (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderIdentity binder, backendTypeBinderName binder)
        | binder <- backendConstructorForalls constructor,
          Map.notMember (backendTypeSubstitutionKeyFromMaybeMetadataLight (backendTypeBinderIdentity binder) (backendTypeBinderName binder)) substitution
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
  [ (freshenedKey key identity name, fmap (substituteBackendTypesByKey patternSubstitution) mbBound)
    | binder <- backendConstructorForalls constructor,
      let key = backendTypeSubstitutionKeyFromMaybeMetadataLight (backendTypeBinderIdentity binder) (backendTypeBinderName binder),
      let identity = backendTypeBinderIdentity binder,
      let name = backendTypeBinderName binder,
      let mbBound = backendTypeBinderBound binder,
      Map.notMember key substitution
  ]
  where
    patternSubstitution =
      Map.union fresheningSubstitution substitution

    freshenedKey key identity name =
      case Map.lookup key fresheningSubstitution of
        Just (BTVarWithIdentity freshBinderIdentity freshName) -> backendTypeSubstitutionKeyFromMaybeMetadataLight freshBinderIdentity freshName
        _ -> backendTypeSubstitutionKeyFromMaybeMetadataLight identity name

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
      ++ [ (backendTypeSubstitutionKeyFromMaybeMetadataLight (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderBound binder)
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
      structuralIdentityAllowsNameFallback identity || structuralSelfIdentityPinsData (backendDataIdentity dataDecl) identity
    _ ->
      True
  where
    structuralSelfIdentityPinsData dataIdentity muIdentity =
      case (dataIdentity, structuralSelfIdentityUnique muIdentity) of
        (Just identity, Just unique) -> symbolUniqueIdentity identity == unique
        _ -> False

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
  ReferenceMode ->
  Maybe BackendDataScope ->
  BackendConstructorInfo ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  BackendValidationError ->
  Either BackendValidationError ()
validateBackendConstructorResultSubstitution typeBounds typeBoundMode mbDataDecls constructorInfo substitution resultTy mismatchError =
  unless
    ( backendStructuralDataBoundaryMatchesWith
        typeBoundMode
        typeBounds
        mbDataDecls
        substitutedResultTy
        resultTy
        || backendConstructorResultPlaceholderMatchesEither typeBoundMode typeBounds substitutedResultTy resultTy
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
      completeDataParameterSubstitution (constructorInfoDataDecl constructorInfo) $
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

backendConstructorResultPlaceholderMatchesEither :: ReferenceMode -> BackendParameterBounds -> BackendType -> BackendType -> Bool
backendConstructorResultPlaceholderMatchesEither typeBoundMode typeBounds left right =
  backendConstructorResultPlaceholderMatches typeBoundMode typeBounds left right
    || backendConstructorResultPlaceholderMatches typeBoundMode typeBounds right left

backendConstructorResultPlaceholderMatches :: ReferenceMode -> BackendParameterBounds -> BackendType -> BackendType -> Bool
backendConstructorResultPlaceholderMatches typeBoundMode typeBounds actual expected =
  case (actual, expected) of
    (_, BTVarWithIdentity identity name)
      | placeholderOpen identity name -> True
    (_, BTVarAppWithIdentity identity name _)
      | placeholderOpen identity name -> True
    (BTArrow actualDom actualCod, BTArrow expectedDom expectedCod) ->
      backendConstructorResultPlaceholderMatches typeBoundMode typeBounds actualDom expectedDom
        && backendConstructorResultPlaceholderMatches typeBoundMode typeBounds actualCod expectedCod
    (BTConWithIdentity actualIdentity actualCon actualArgs, BTConWithIdentity expectedIdentity expectedCon expectedArgs)
      | backendTypeHeadMatchesWith typeBoundMode actualIdentity actualCon expectedIdentity expectedCon,
        length actualArgs == length expectedArgs ->
          and (zipWith (backendConstructorResultPlaceholderMatches typeBoundMode typeBounds) (NE.toList actualArgs) (NE.toList expectedArgs))
    (BTVarAppWithIdentity actualIdentity actualName actualArgs, BTVarAppWithIdentity expectedIdentity expectedName expectedArgs)
      | typeBinderRefMatchesWith typeBoundMode actualIdentity actualName expectedIdentity expectedName,
        length actualArgs == length expectedArgs ->
          and (zipWith (backendConstructorResultPlaceholderMatches typeBoundMode typeBounds) (NE.toList actualArgs) (NE.toList expectedArgs))
    (BTForallWithIdentity actualIdentity actualName actualBound actualBody, BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody) ->
      backendConstructorResultPlaceholderBoundMatches typeBoundMode typeBounds actualBound expectedBound
        && backendConstructorResultPlaceholderMatches
          typeBoundMode
          (extendPlaceholderBound expectedIdentity expectedName (extendPlaceholderBound actualIdentity actualName typeBounds))
          actualBody
          expectedBody
    _ -> alphaEqBackendType actual expected
  where
    placeholderOpen identity name =
      case typeBoundReferenceKey typeBoundMode identity name of
        Just key -> Map.notMember key typeBounds
        Nothing -> False

    extendPlaceholderBound identity name bounds =
      case typeBoundReferenceKey typeBoundMode identity name of
        Just key -> Map.insert key Nothing bounds
        Nothing -> bounds

backendConstructorResultPlaceholderBoundMatches :: ReferenceMode -> BackendParameterBounds -> Maybe BackendType -> Maybe BackendType -> Bool
backendConstructorResultPlaceholderBoundMatches _ _ Nothing Nothing = True
backendConstructorResultPlaceholderBoundMatches typeBoundMode typeBounds (Just actual) (Just expected) =
  backendConstructorResultPlaceholderMatches typeBoundMode typeBounds actual expected
backendConstructorResultPlaceholderBoundMatches _ _ _ _ = False

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


data TermBinderKey
  = TermBinderIdentity BackendLocalKey
  | TermBinderName String
  deriving (Eq, Ord)

closureCaptureBinderRef :: BackendClosureCapture -> (TermBinderKey, String)
closureCaptureBinderRef capture =
  termBinderRef (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture)

closureParamBinderRef :: BackendClosureParam -> (TermBinderKey, String)
closureParamBinderRef param =
  termBinderRef (backendClosureParamIdentity param) (backendClosureParamName param)

patternBinderRef :: BackendPatternBinder -> (TermBinderKey, String)
patternBinderRef binder =
  termBinderRef (backendPatternBinderIdentity binder) (backendPatternBinderName binder)

termBinderRef :: Maybe IdDetails -> String -> (TermBinderKey, String)
termBinderRef mbIdentity name =
  (maybe (TermBinderName name) TermBinderIdentity (mbIdentity >>= idDetailsLocalKey), name)

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
