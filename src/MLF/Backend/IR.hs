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
    BackendCallableBindingKind (..),
    BackendCallableHead (..),
    BackendValidationError (..),
    alphaEqBackendType,
    backendTypeHeadMatches,
    typeBinderRefMatches,
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
    completeDataParameterSubstitution,
    isVacuousRecursiveBinderWithIdentity,
    matchBackendTypeParametersWithTypeBounds,
    metadataLightStructuralDataMatchesWithIdentity,
    structuralBackendHandlerFields,
    structuralDataArgumentSubstitution,
    structuralDataDeclarationMatches,
    structuralMuPayloadTypes,
    structuralMuTypesHaveBinderIdentityMismatch,
    structuralPayloadsMayInstantiate,
    structuralRecursiveDataName,
    recursiveBodyCompatibleWithIdentity,
  )
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Program.Builtins (builtinTypeHeadIdentity, builtinValueIdentity)
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), symbolDefiningModule, symbolDefiningName, symbolIdentityStableName, symbolNamespace, symbolUniqueIdentity)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity
  ( constructorRefSymbol,
    DeferredRef,
    EnvRef,
    IdDetails (..),
    IdentityGenerator,
    LocalRef,
    primitiveRefSymbol,
    StructuralTypeBinderRole (..),
    TypeBinderIdentity,
    UniqueIdentity,
    advanceIdentityGeneratorPast,
    freshIdentity,
    idDetailsReferenceName,
    initialIdentityGenerator,
    symbolGeneratedIdentities,
    typeBinderIdentityGeneratedUnique,
    typeBinderIdentityStructural,
    typeBinderIdentityFromUnique,
  )
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Util.Names (freshNameLike)

data BackendValidationError
  = BackendDuplicateModule String
  | BackendDuplicateData String
  | BackendDataParameterIdentityMissing String String
  | BackendConstructorUnknownTypeVariable String String
  | BackendConstructorTypeBinderIdentityMissing String String
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
    bvcGlobalsByIdentity :: Map.Map SymbolIdentity BackendType,
    bvcData :: Map.Map String BackendData,
    bvcDataByIdentity :: Map.Map SymbolIdentity BackendData,
    bvcConstructors :: Map.Map String BackendConstructorInfo,
    bvcConstructorsByIdentity :: Map.Map SymbolIdentity BackendConstructorInfo,
    bvcLocals :: Map.Map String BackendType,
    bvcLocalsByIdentity :: Map.Map BackendLocalKey BackendType,
    bvcClosureGlobals :: Set.Set String,
    bvcClosureGlobalsByIdentity :: Set.Set SymbolIdentity,
    bvcClosureLocals :: Set.Set String,
    bvcClosureLocalsByIdentity :: Set.Set BackendLocalKey,
    bvcPossibleClosureLocals :: Set.Set String,
    bvcPossibleClosureLocalsByIdentity :: Set.Set BackendLocalKey,
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

data BackendLocalKey
  = BackendLocalRef LocalRef
  | BackendEnvRef EnvRef
  | BackendDeferredRef DeferredRef
  deriving (Eq, Ord, Show)

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
  requireUnique BackendDuplicateModule (map backendModuleName modules0)
  requireUniqueBy BackendDuplicateModule [(identity, symbolIdentityStableName identity) | module0 <- modules0, Just identity <- [backendModuleIdentity module0]]
  requireUnique BackendDuplicateData (map backendDataName dataDecls)
  requireUniqueBy BackendDuplicateData [(identity, symbolIdentityStableName identity) | dataDecl <- dataDecls, Just identity <- [backendDataIdentity dataDecl]]
  mapM_ validateBackendDataParameterIdentities dataDecls
  mapM_ validateBackendDataConstructorBinderIdentities dataDecls
  mapM_ validateBackendDataConstructorTypeVariables dataDecls
  requireUnique BackendDuplicateBinding (map backendBindingName bindings)
  requireUniqueBy BackendDuplicateBinding [(identity, symbolIdentityStableName identity) | binding <- bindings, Just identity <- [backendBindingIdentity binding]]
  requireUnique BackendDuplicateConstructor (map backendConstructorName constructors)
  requireUniqueBy BackendDuplicateConstructor [(identity, symbolIdentityStableName identity) | constructor <- constructors, Just identity <- [backendConstructorIdentity constructor]]
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
    dataDeclsByName =
      Map.fromList
        [ (backendDataName dataDecl, dataDecl)
        | dataDecl <- dataDecls,
          backendDataIdentity dataDecl == Nothing
        ]
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
              [ (backendBindingName binding, backendBindingType binding)
              | binding <- bindings,
                backendBindingIdentity binding == Nothing
              ],
          bvcGlobalsByIdentity =
            Map.fromList
              [ (identity, backendBindingType binding)
              | binding <- bindings,
                Just identity <- [backendBindingIdentity binding]
              ]
              `Map.union` runtimePrimitiveTypesByIdentity,
          bvcData = dataDeclsByName,
          bvcDataByIdentity = dataDeclsByIdentity,
          bvcConstructors =
            Map.fromList
              [ (name, info)
              | (name, info@(BackendConstructorInfo {bciConstructor = constructor})) <- constructorInfos,
                backendConstructorIdentity constructor == Nothing
              ],
          bvcConstructorsByIdentity =
            Map.fromList
              [ (identity, info)
              | (_, info@(BackendConstructorInfo {bciConstructor = constructor})) <- constructorInfos,
                Just identity <- [backendConstructorIdentity constructor]
              ],
          bvcLocals = Map.empty,
          bvcLocalsByIdentity = Map.empty,
          bvcClosureGlobals = Set.empty,
          bvcClosureGlobalsByIdentity = Set.empty,
          bvcClosureLocals = Set.empty,
          bvcClosureLocalsByIdentity = Set.empty,
          bvcPossibleClosureLocals = Set.empty,
          bvcPossibleClosureLocalsByIdentity = Set.empty,
          bvcTypeBounds = Map.empty
        }
    (closureGlobals, closureGlobalIdentities) = backendClosureGlobals baseContext bindings
    context0 =
      baseContext
        { bvcClosureGlobals = closureGlobals,
          bvcClosureGlobalsByIdentity = closureGlobalIdentities
        }

validateBackendDataParameterIdentities :: BackendData -> Either BackendValidationError ()
validateBackendDataParameterIdentities dataDecl =
  case backendDataIdentity dataDecl of
    Just {} -> mapM_ requireIdentity (backendDataParameterRefs dataDecl)
    Nothing -> pure ()
  where
    requireIdentity ref =
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
      mapM_ (requireIdentity constructor) (backendConstructorForalls constructor)

    requireIdentity constructor binder =
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
              [ backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder)
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
           | key <- Set.toList (Set.difference (freeBackendTypeVarKeys ty) allowedKeys),
             backendTypeSubstitutionKeyIdentity key == Nothing
           ] of
        [] -> pure ()
        key : _ -> Left (BackendConstructorUnknownTypeVariable (backendConstructorName constructor) (backendTypeSubstitutionKeyName key))

backendDataScopeForContext :: BackendValidationContext -> BackendDataScope
backendDataScopeForContext context0 =
  backendDataScope (bvcData context0) (bvcDataByIdentity context0)

backendProgramMainExists :: BackendProgram -> [BackendBinding] -> Bool
backendProgramMainExists program bindings =
  case backendProgramMainIdentity program of
    Just identity ->
      any ((== Just identity) . backendBindingIdentity) bindings
    Nothing ->
      any
        ( \binding ->
            case backendBindingIdentity binding of
              Just {} -> False
              Nothing -> backendProgramMain program == backendBindingName binding
        )
        bindings

backendClosureGlobals :: BackendValidationContext -> [BackendBinding] -> (Set.Set String, Set.Set SymbolIdentity)
backendClosureGlobals baseContext bindings =
  go Set.empty Set.empty
  where
    go globals identities =
      let context0 =
            baseContext
              { bvcClosureGlobals = globals,
                bvcClosureGlobalsByIdentity = identities
              }
          closureBindings =
            [ binding
            | binding <- bindings,
              BackendClosureCallableHead _ <- [backendCallableHeadInContext (Just context0) (backendBindingExpr binding)]
            ]
          detectedGlobals =
            Set.fromList
              [ backendBindingName binding
              | binding <- closureBindings,
                backendBindingIdentity binding == Nothing
              ]
          detectedIdentities =
            Set.fromList
              [identity | binding <- closureBindings, Just identity <- [backendBindingIdentity binding]]
          globals' =
            globals <> detectedGlobals
          identities' =
            identities <> detectedIdentities
       in if globals' == globals && identities' == identities
            then (globals, identities)
            else go globals' identities'

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

backendRuntimePrimitiveTypesByIdentity :: Map.Map SymbolIdentity BackendType
backendRuntimePrimitiveTypesByIdentity =
  backendRuntimePrimitiveTypesByIdentityFrom backendRuntimePrimitiveTypes

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
        BackendClosureCallableHead ref ->
          Left (BackendClosureCalledWithBackendApp (backendCallableRefName ref))
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
      unless (alphaEqBackendType resultTy expected) $
        Left (BackendTypeAbsTypeMismatch name resultTy expected)
    BackendTyApp resultTy fun tyArg -> do
      validateBackendExprWith mbContext fun
      case backendExprType fun of
        BTForallWithIdentity mbIdentity name mbBound bodyTy -> do
          validateBackendTypeArgumentBound mbBound tyArg
          let expected = substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFor mbIdentity name) tyArg) bodyTy
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

instance BackendCallableExpr BackendExpr where
  backendCallableExprView =
    \case
      BackendVarWithIdentity _ mbIdentity name ->
        BackendCallableVar mbIdentity name
      BackendLam _ _ _ _ ->
        BackendCallableLam
      BackendClosureWithParamIdentities _ entryIdentity entryName _ _ _ ->
        BackendCallableClosure entryIdentity entryName
      BackendTyAbs _ _ _ body ->
        BackendCallableTyAbs body
      BackendTyApp _ fun _ ->
        BackendCallableTyApp fun
      BackendLetWithIdentity _ mbIdentity name _ rhs body ->
        BackendCallableLet mbIdentity name rhs body
      BackendCase _ _ alternatives ->
        BackendCallableCase
          [ let binders = patternBinderDetails (backendAltPattern alternative)
                body = backendAltBody alternative
             in BackendCallableAlternative
                  { backendCallableAltBinders = binders,
                    backendCallableAltClosureBinders =
                      filter (`backendExprMentionsBindingWithCallableType` body) binders,
                    backendCallableAltBody = body
                  }
          | alternative <- NE.toList alternatives
          ]
      _ ->
        BackendCallableOpaque

backendCallableHeadInContext :: Maybe BackendValidationContext -> BackendExpr -> BackendCallableHead
backendCallableHeadInContext mbContext =
  backendCallableHead (backendCallableBindingKindInContext mbContext)

backendCallableBindingKindInContext :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendCallableBindingKind
backendCallableBindingKindInContext Nothing _ _ =
  BackendCallableBindingUnknown
backendCallableBindingKindInContext (Just context0) mbIdentity name =
  case mbIdentity of
    Just {} -> maybe BackendCallableBindingUnknown id (lookupCallableBindingKindByIdentity context0 mbIdentity)
    Nothing -> lookupCallableBindingKindByName context0 name

lookupCallableBindingKindByIdentity :: BackendValidationContext -> Maybe IdDetails -> Maybe BackendCallableBindingKind
lookupCallableBindingKindByIdentity context0 mbIdentity =
  case mbIdentity of
    Just details
      | Just key <- idDetailsLocalKey details ->
          lookupLocalCallableBindingKindByIdentity context0 key
      | Just identity <- idDetailsSymbolIdentity details ->
          lookupGlobalCallableBindingKindByIdentity context0 identity
    _ ->
      Nothing

lookupLocalCallableBindingKindByIdentity :: BackendValidationContext -> BackendLocalKey -> Maybe BackendCallableBindingKind
lookupLocalCallableBindingKindByIdentity context0 key
  | Set.member key (bvcClosureLocalsByIdentity context0) =
      Just BackendCallableBindingClosure
  | Set.member key (bvcPossibleClosureLocalsByIdentity context0) =
      Just BackendCallableBindingUnknown
  | Map.member key (bvcLocalsByIdentity context0) =
      Just BackendCallableBindingDirect
  | otherwise =
      Nothing

lookupGlobalCallableBindingKindByIdentity :: BackendValidationContext -> SymbolIdentity -> Maybe BackendCallableBindingKind
lookupGlobalCallableBindingKindByIdentity context0 identity
  | Set.member identity (bvcClosureGlobalsByIdentity context0) =
      Just BackendCallableBindingClosure
  | Map.member identity (bvcGlobalsByIdentity context0) =
      Just BackendCallableBindingDirect
  | otherwise =
      Nothing

lookupCallableBindingKindByName :: BackendValidationContext -> String -> BackendCallableBindingKind
lookupCallableBindingKindByName context0 name
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
              foldr insertBackendBinderAlias aliases capturedAliases
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
closureCallHeadReferencesAny needles expr =
  closureCallHeadReferencesAnyFrom needles expr

closureCallHeadReferencesAnyFrom :: [BackendBinderRef] -> BackendExpr -> Bool
closureCallHeadReferencesAnyFrom aliases0 =
  \case
    BackendVarWithIdentity _ mbIdentity name ->
      any (backendBinderMatches (backendCallableRef mbIdentity name)) aliases0
    BackendTyApp _ fun _ ->
      closureCallHeadReferencesAnyFrom aliases0 fun
    BackendLetWithIdentity _ mbIdentity name _ rhs body ->
      let binder = backendCallableRef mbIdentity name
          aliasesWithoutShadow =
            filter (not . backendBinderMatches binder) aliases0
          aliasesForBody =
            if closureCallHeadReferencesAnyFrom aliases0 rhs
              then insertBackendBinderAlias binder aliasesWithoutShadow
              else aliasesWithoutShadow
       in closureCallHeadReferencesAnyFrom aliasesForBody body
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

validateBackendVariable :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> Either BackendValidationError ()
validateBackendVariable Nothing _ _ _ =
  pure ()
validateBackendVariable (Just context0) mbIdentity name actualTy =
  case lookupBackendVariable context0 mbIdentity name of
    Nothing ->
      Left (BackendUnknownVariable name)
    Just expectedTy -> do
      unless (backendVariableTypeMatches context0 mbIdentity name expectedTy actualTy) $
        Left (BackendVariableTypeMismatch name expectedTy actualTy)

backendApplicationTypeMatches :: Maybe BackendValidationContext -> BackendType -> BackendType -> Bool
backendApplicationTypeMatches mbContext expectedTy actualTy =
  matches expectedTy actualTy || matches expectedTy' actualTy'
  where
    typeBounds = maybe Map.empty bvcTypeBounds mbContext
    dataScope = backendDataScopeForContext <$> mbContext
    expectedTy' = maybe expectedTy (`canonicalizeBackendTypeDataHeads` expectedTy) mbContext
    actualTy' = maybe actualTy (`canonicalizeBackendTypeDataHeads` actualTy) mbContext
    matches expected actual =
      typeMatches expected actual
        || typeMatches actual expected
        || backendStructuralDataBoundaryMatches typeBounds dataScope expected actual
        || backendStructuralDataBoundaryMatches typeBounds dataScope actual expected

    typeMatches =
      backendTypeMatchesWith AllowStructuralPayloadInstantiation typeBounds dataScope

backendVariableTypeMatches :: BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendType -> Bool
backendVariableTypeMatches context0 mbIdentity name expectedTy actualTy =
  rawMatches || canonicalMatches
  where
    referenceName = maybe name (idDetailsReferenceName name) mbIdentity
    dataScope = backendDataScopeForContext context0
    typeBounds = bvcTypeBounds context0
    rawMatches =
      backendTypeMatchesWith
        (RejectFreeTypeVariableInstantiation (freshenedAliasesForVariable referenceName))
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
        || backendStructuralDataBoundaryMatches
          typeBounds
          (Just dataScope)
          expectedTy
          actualTy
        || backendApplicationTypeMatches (Just context0) expectedTy actualTy
        || generatedCasePatternVariableTypeMatches referenceName typeBounds expectedTy
        || primitiveRuntimeVariableTypeMatches mbIdentity name expectedTy actualTy
    canonicalMatches =
      let expectedTy' = canonicalizeBackendTypeDataHeads context0 expectedTy
          actualTy' = canonicalizeBackendTypeDataHeads context0 actualTy
       in backendTypeMatchesWith
            (RejectFreeTypeVariableInstantiation (freshenedAliasesForVariable referenceName))
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
            || backendStructuralDataBoundaryMatches
              typeBounds
              (Just dataScope)
              expectedTy'
              actualTy'
            || generatedCasePatternVariableTypeMatches referenceName typeBounds expectedTy'
            || primitiveRuntimeVariableTypeMatches mbIdentity name expectedTy' actualTy'

generatedCasePatternVariableTypeMatches :: String -> BackendParameterBounds -> BackendType -> Bool
generatedCasePatternVariableTypeMatches name typeBounds expectedTy =
  case (stripPrefix "$case" name, expectedTy) of
    (Just _, BTVarWithIdentity identity typeName) ->
      not (hasConcreteTypeBound (backendTypeSubstitutionKeyFor identity typeName))
    _ ->
      False
  where
    hasConcreteTypeBound key =
      case Map.lookup key typeBounds of
        Just (Just boundTy) -> not (alphaEqBackendType boundTy BTBottom)
        _ -> False

primitiveRuntimeVariableTypeMatches :: Maybe IdDetails -> String -> BackendType -> BackendType -> Bool
primitiveRuntimeVariableTypeMatches mbIdentity name expectedTy actualTy
  | primitiveRuntimeVariableReference mbIdentity name =
      go expectedTy actualTy
  | otherwise =
      False
  where
    primitiveRuntimeVariableReference Nothing _ =
      False
    primitiveRuntimeVariableReference (Just details) _ =
      maybe False (`Map.member` backendRuntimePrimitiveTypesByIdentity) (idDetailsSymbolIdentity details)

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
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTConWithIdentity _ actualCon actualArgs) ->
                structuralPrimitiveTypeMatches expectedIdentity expectedName expectedBody actualCon (NE.toList actualArgs)
              (BTConWithIdentity _ expectedCon expectedArgs, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralPrimitiveTypeMatches actualIdentity actualName actualBody expectedCon (NE.toList expectedArgs)
              _ ->
                False

    structuralPrimitiveMuMatches expectedIdentity expectedName expectedBody actualIdentity actualName actualBody =
      case (structuralRecursiveDataName expectedName, structuralRecursiveDataName actualName) of
        (Just expectedDataName, Just actualDataName)
          | backendPrimitiveDataNameMatches expectedDataName actualDataName ->
              case (structuralPrimitivePayloadTypes expectedIdentity expectedName expectedBody, structuralPrimitivePayloadTypes actualIdentity actualName actualBody) of
                (Just expectedPayloadTypes, Just actualPayloadTypes) ->
                  zipAllWith go expectedPayloadTypes actualPayloadTypes
                _ ->
                  False
        _ ->
          False

    backendPrimitiveTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase =
      backendTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase
        || case (expectedIdentity, actualIdentity) of
          (Nothing, Nothing) -> normalizedBaseName expectedBase == normalizedBaseName actualBase
          _ -> False

    normalizedBaseName (BaseTy baseName) =
      normalizeBackendBuiltinTypeReference baseName

    structuralPrimitiveTypeMatches muIdentity muName muBody con args =
      case structuralRecursiveDataName muName of
        Just dataName
          | backendPrimitiveDataNameMatches dataName (getBaseName con) ->
              case structuralPrimitivePayloadTypes muIdentity muName muBody of
                Just payloadTypes -> zipAllWith go payloadTypes args
                Nothing -> False
        _ ->
          False

    structuralPrimitivePayloadTypes muIdentity muName body =
      filter (not . structuralSelfField muIdentity muName) <$> structuralMuPayloadTypes body

    structuralSelfField muIdentity muName =
      \case
        BTVarWithIdentity fieldIdentity fieldName ->
          case (muIdentity, fieldIdentity) of
            (Just {}, Just {}) ->
              fieldIdentity == muIdentity
            _ ->
              structuralRecursiveDataName fieldName == structuralRecursiveDataName muName
        _ ->
          False

    backendPrimitiveDataNameMatches leftName rightName =
      normalizeBackendBuiltinTypeReference leftName == normalizeBackendBuiltinTypeReference rightName
        || leftName == unqualifiedBackendDataName rightName
        || unqualifiedBackendDataName leftName == rightName
        || unqualifiedBackendDataName leftName == unqualifiedBackendDataName rightName

backendVariableTypeMatchesWithBounds :: BackendParameterBounds -> BackendType -> BackendType -> Bool
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
  BackendParameterBounds ->
  Maybe BackendDataScope ->
  BackendType ->
  BackendType ->
  Bool
backendTypeMatchesWith typeVariableInstantiation typeBounds mbDataDecls expectedTy actualTy =
  go Set.empty expectedTy actualTy
  where
    go bound expected actual =
      alphaEqWithinDataScope actual expected
        || typeVariableBoundMatches bound expected actual
        || typeVariableBoundMatches bound actual expected
        || case (expected, actual) of
              (BTVarWithIdentity expectedIdentity expectedName, BTVarWithIdentity actualIdentity actualName)
                | generatedTypeVariableAliasMatches expectedIdentity expectedName actualIdentity actualName ->
                    True
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod)
                | opaqueIOFunctionCompatible bound expectedDom expectedCod actualDom actualCod ->
                    True
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                go bound expectedDom actualDom && go bound expectedCod actualCod
              (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase) ->
                backendTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase
              (BTBaseWithIdentity expectedDataIdentity expectedBase, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralMuMatchesKnownData expectedDataIdentity expectedBase [] actualIdentity actualName actualBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTBaseWithIdentity actualDataIdentity actualBase) ->
                structuralMuMatchesKnownData actualDataIdentity actualBase [] expectedIdentity expectedName expectedBody
              (BTConWithIdentity expectedIdentity expectedCon (_ :| []), BTConWithIdentity actualIdentity actualCon (_ :| []))
                | opaqueIOBackendHeadMatches expectedIdentity expectedCon && opaqueIOBackendHeadMatches actualIdentity actualCon ->
                    True
              (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs) ->
                backendTypeHeadMatches expectedIdentity expectedCon actualIdentity actualCon
                  && zipAllWith
                    (metadataBackedTypeArgumentMatchesEither (metadataBackedTypeHead expectedIdentity expectedCon) bound)
                    (NE.toList expectedArgs)
                    (NE.toList actualArgs)
              (BTConWithIdentity expectedDataIdentity expectedCon expectedArgs, BTMuWithIdentity actualIdentity actualName actualBody) ->
                nominalStructuralTypeVarArgsMatch expectedCon (NE.toList expectedArgs) actualName
                  || structuralMuMatchesKnownData expectedDataIdentity expectedCon (NE.toList expectedArgs) actualIdentity actualName actualBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTConWithIdentity actualDataIdentity actualCon actualArgs) ->
                nominalStructuralTypeVarArgsMatch actualCon (NE.toList actualArgs) expectedName
                  || structuralMuMatchesKnownData actualDataIdentity actualCon (NE.toList actualArgs) expectedIdentity expectedName expectedBody
              (BTVarAppWithIdentity expectedIdentity expectedName expectedArgs, BTVarAppWithIdentity actualIdentity actualName actualArgs) ->
                typeBinderRefMatches expectedIdentity expectedName actualIdentity actualName
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
                if sameStructuralDataName expectedName actualName
                  then
                    typeBinderRefMatches expectedIdentity expectedName actualIdentity actualName
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
                      let freshName = freshBinderName expectedName actualName Nothing Nothing expectedBody actualBody
                          freshTy = freshBinderTy expectedIdentity actualIdentity freshName
                          freshKey = freshBinderKey expectedIdentity actualIdentity freshName
                          expectedBody' = substituteBackendTypeForBinder expectedIdentity expectedName freshTy expectedBody
                          actualBody' = substituteBackendTypeForBinder actualIdentity actualName freshTy actualBody
                       in go (Set.insert freshKey bound) expectedBody' actualBody'
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
      case (expected, actual, mbDataDecls) of
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
        (BTBaseWithIdentity (Just {}) _, BTMuWithIdentity {}, Just {}) -> True
        (BTMuWithIdentity {}, BTBaseWithIdentity (Just {}) _, Just {}) -> True
        (BTConWithIdentity (Just {}) _ _, BTMuWithIdentity {}, Just {}) -> True
        (BTMuWithIdentity {}, BTConWithIdentity (Just {}) _ _, Just {}) -> True
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
            | Set.notMember (backendTypeSubstitutionKeyFor expectedIdentity expectedName) bound -> True
          (_, BTVarWithIdentity actualIdentity actualName)
            | Set.notMember (backendTypeSubstitutionKeyFor actualIdentity actualName) bound -> True
          (BTVarWithIdentity expectedIdentity expectedName, BTVarWithIdentity actualIdentity actualName) ->
            typeBinderRefMatches expectedIdentity expectedName actualIdentity actualName
          _ -> False

    typeVariableBoundMatches bound ty otherTy =
      case ty of
        BTVarWithIdentity identity name
          | Set.notMember key bound ->
              case Map.lookup key typeBounds of
                Just (Just boundTy)
                  | not (alphaEqBackendType boundTy BTBottom) ->
                      go bound boundTy otherTy
                _ ->
                  False
          where
            key = backendTypeSubstitutionKeyFor identity name
        _ ->
          False

    sameStructuralDataName expectedName actualName =
      case (structuralRecursiveDataName expectedName, structuralRecursiveDataName actualName) of
        (Just expectedDataName, Just actualDataName) -> expectedDataName == actualDataName
        _ -> False

    nominalStructuralTypeVarArgsMatch (BaseTy dataName) args muName =
      structuralRecursiveDataName muName == Just dataName
        && all freeTypeVarArg args
      where
        freeTypeVarArg =
          \case
            BTVarWithIdentity identity name ->
              not (hasConcreteTypeBound (backendTypeSubstitutionKeyFor identity name))
            _ ->
              False

    generatedTypeVariableAliasMatches expectedIdentity expectedName actualIdentity actualName =
      expectedName == actualName
        && maybe False isGeneratedTypeBinder expectedIdentity
        && maybe False isGeneratedTypeBinder actualIdentity

    isGeneratedTypeBinder =
      maybe False (const True) . typeBinderIdentityGeneratedUnique

    metadataBackedTypeArgumentMatches metadataBacked bound expected actual =
      go bound expected actual || (metadataBacked && freeExpectedTypeVariableMayInstantiate bound expected)

    metadataBackedTypeArgumentMatchesEither metadataBacked bound expected actual =
      metadataBackedTypeArgumentMatches metadataBacked bound expected actual
        || metadataBackedTypeArgumentMatches metadataBacked bound actual expected

    freeExpectedTypeVariableMayInstantiate bound =
      \case
        BTVarWithIdentity identity name ->
          Set.notMember key bound
            && not (hasConcreteTypeBound key)
          where
            key = backendTypeSubstitutionKeyFor identity name
        _ ->
          False

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
          maybe False (Map.member name . backendDataScopeByName) mbDataDecls

    structuralMuMatchesKnownData dataIdentity base@(BaseTy dataName) args muIdentity muName body =
      (metadataLightAllowed dataIdentity dataName && metadataLightStructuralDataMatchesWithIdentity base args muIdentity muName body)
        || ( maybe
              False
              ( \structuralName ->
                  PrimitiveInventory.matchesBuiltinTypeName dataName structuralName
                    && metadataLightAllowed dataIdentity structuralName
                    && metadataLightStructuralDataMatchesWithIdentity (BaseTy structuralName) args muIdentity muName body
              )
              (structuralRecursiveDataName muName)
           )
        || maybe False structuralDataDeclMatches (matchingDataDecl dataIdentity dataName muIdentity muName)
      where
        structuralDataDeclMatches dataDecl
          | Just substitution <- structuralDataArgumentSubstitution dataDecl args =
              structuralDataDeclarationMatches typeBounds dataDecl substitution (BTMuWithIdentity muIdentity muName body)
        structuralDataDeclMatches _ =
          False

    metadataLightAllowed dataIdentity dataName =
      case (dataIdentity, mbDataDecls) of
        (Just {}, Just {}) -> False
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
      case dataIdentity >>= lookupDataByIdentity of
        Just dataDecl
          | structuralSelfIdentityMatchesData muIdentity dataDecl || structuralMuNameMatchesData muName dataDecl -> Just dataDecl
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
      Map.lookup identity dataDeclsByIdentity

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

    structuralSelfIdentityMatchesData muIdentity dataDecl =
      case structuralSelfIdentityUnique muIdentity of
        Nothing -> True
        Just unique ->
          case backendDataIdentity dataDecl of
            Just dataDeclIdentity -> symbolUniqueIdentity dataDeclIdentity == unique
            Nothing -> False

    structuralMuNameMatchesData muName dataDecl =
      structuralRecursiveDataName muName == Just (backendDataName dataDecl)

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
        RejectFreeTypeVariableInstantiation {} ->
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
      backendTypeSubstitutionKeyFor (leftIdentity <|> rightIdentity) name

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
          Map.lookup key (bvcLocalsByIdentity context0)
      | Just identity <- idDetailsSymbolIdentity details ->
          Map.lookup identity (bvcGlobalsByIdentity context0)
    _ ->
      lookupByName
  where
    lookupByName =
      Map.lookup name (bvcLocals context0) <|> Map.lookup name (bvcGlobals context0)

idDetailsSymbolIdentity :: IdDetails -> Maybe SymbolIdentity
idDetailsSymbolIdentity =
  \case
    TopLevelId symbol -> Just symbol
    ConstructorId ref -> Just (constructorRefSymbol ref)
    MethodId symbol -> Just symbol
    PrimitiveId ref -> Just (primitiveRefSymbol ref)
    _ -> Nothing

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
  | backendExprCallsBinderAsClosureHead (backendCallableRef mbIdentity name) body =
      extendClosureLocalMaybe mbContext mbIdentity name ty
  | otherwise =
      extendLocalMaybe mbContext mbIdentity name ty

extendLocal :: BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendValidationContext
extendLocal context0 mbIdentity name ty =
  insertLocalIdentity mbIdentity name ty
    context0
      { bvcLocals = bindLocalName mbIdentity name ty (bvcLocals context0),
        bvcClosureLocals = Set.delete name (bvcClosureLocals context0),
        bvcClosureLocalsByIdentity = deleteLocalIdentity mbIdentity (bvcClosureLocalsByIdentity context0),
        bvcPossibleClosureLocals = Set.delete name (bvcPossibleClosureLocals context0),
        bvcPossibleClosureLocalsByIdentity = deleteLocalIdentity mbIdentity (bvcPossibleClosureLocalsByIdentity context0)
      }

extendClosureLocalMaybe :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> Maybe BackendValidationContext
extendClosureLocalMaybe mbContext mbIdentity name ty =
  fmap (\context0 -> extendClosureLocal context0 mbIdentity name ty) mbContext

extendClosureLocal :: BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendValidationContext
extendClosureLocal context0 mbIdentity name ty =
  insertLocalIdentity mbIdentity name ty
    context0
      { bvcLocals = bindLocalName mbIdentity name ty (bvcLocals context0),
        bvcClosureLocals = bindLocalNameSet mbIdentity name (bvcClosureLocals context0),
        bvcClosureLocalsByIdentity = insertLocalIdentityKey mbIdentity (bvcClosureLocalsByIdentity context0),
        bvcPossibleClosureLocals = Set.delete name (bvcPossibleClosureLocals context0),
        bvcPossibleClosureLocalsByIdentity = deleteLocalIdentity mbIdentity (bvcPossibleClosureLocalsByIdentity context0)
      }

extendPossibleClosureLocalMaybe :: Maybe BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> Maybe BackendValidationContext
extendPossibleClosureLocalMaybe mbContext mbIdentity name ty =
  fmap (\context0 -> extendPossibleClosureLocal context0 mbIdentity name ty) mbContext

extendPossibleClosureLocal :: BackendValidationContext -> Maybe IdDetails -> String -> BackendType -> BackendValidationContext
extendPossibleClosureLocal context0 mbIdentity name ty =
  insertLocalIdentity mbIdentity name ty
    context0
      { bvcLocals = bindLocalName mbIdentity name ty (bvcLocals context0),
        bvcClosureLocals = Set.delete name (bvcClosureLocals context0),
        bvcClosureLocalsByIdentity = deleteLocalIdentity mbIdentity (bvcClosureLocalsByIdentity context0),
        bvcPossibleClosureLocals = bindLocalNameSet mbIdentity name (bvcPossibleClosureLocals context0),
        bvcPossibleClosureLocalsByIdentity = insertLocalIdentityKey mbIdentity (bvcPossibleClosureLocalsByIdentity context0)
      }

bindLocalName :: Maybe IdDetails -> String -> BackendType -> Map.Map String BackendType -> Map.Map String BackendType
bindLocalName mbIdentity name ty locals =
  case mbIdentity >>= idDetailsLocalKey of
    Just _ -> Map.delete name locals
    Nothing -> Map.insert name ty locals

bindLocalNameSet :: Maybe IdDetails -> String -> Set.Set String -> Set.Set String
bindLocalNameSet mbIdentity name names =
  case mbIdentity >>= idDetailsLocalKey of
    Just _ -> Set.delete name names
    Nothing -> Set.insert name names

insertLocalIdentity :: Maybe IdDetails -> String -> BackendType -> BackendValidationContext -> BackendValidationContext
insertLocalIdentity mbIdentity _name ty context0 =
  case mbIdentity >>= idDetailsLocalKey of
    Just key ->
      context0
        { bvcLocalsByIdentity = Map.insert key ty (bvcLocalsByIdentity context0)
        }
    Nothing -> context0

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
      | backendTypeIsClosureValue ty = extendClosureLocal context0 mbIdentity name ty
      | otherwise = extendLocal context0 mbIdentity name ty
      where
        mbIdentity = backendCallableRefIdentity ref
        name = backendCallableRefName ref

backendTypeIsClosureValue :: BackendType -> Bool
backendTypeIsClosureValue =
  \case
    BTArrow {} -> True
    _ -> False

isOpaqueIOBackendName :: BaseTy -> Bool
isOpaqueIOBackendName (BaseTy name) =
  PrimitiveInventory.matchesBuiltinTypeName "IO" name

opaqueIOBackendHeadMatches :: Maybe SymbolIdentity -> BaseTy -> Bool
opaqueIOBackendHeadMatches (Just identity) _ =
  identity == PrimitiveInventory.builtinTypeIdentity "IO"
opaqueIOBackendHeadMatches Nothing base =
  isOpaqueIOBackendName base

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
              let (identity, gen') = backendTypeIdentityForName name gen
               in (Map.insert name identity env, gen')
          )
          (Map.empty, generatorAfterHeads)
          (Set.toAscList (PrimitiveInventory.freePrimitiveTypeVars ty))
      (ty', generator') = go freeEnv generator ty
   in (ty', generator')
  where
    generatorAfterHeads =
      foldr
        advanceIdentityGeneratorPast
        generator0
        (concatMap symbolGeneratedIdentities (Map.elems headIdentities0))

    go env generator =
      \case
        PrimitiveInventory.PrimitiveTypeVar name ->
          case Map.lookup name env of
            Just identity -> (BTVarWithIdentity (Just identity) name, generator)
            Nothing ->
              let (identity, generator') = backendTypeIdentityForName name generator
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
          let (identity, generator1) = backendTypeIdentityForName name generator
              (body', generator2) = go (Map.insert name identity env) generator1 body
           in (BTForallWithIdentity (Just identity) name Nothing body', generator2)
        PrimitiveInventory.PrimitiveTypeMu name body ->
          let (identity, generator1) = backendTypeIdentityForName name generator
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

    lookupPrimitiveTypeHeadIdentity headIdentities name =
      Map.lookup name headIdentities <|> Map.lookup name (primitiveTypeHeadStableAliases headIdentities)

    primitiveTypeHeadStableAliases =
      Map.fromList . map (\identity -> (symbolIdentityStableName identity, identity)) . Map.elems

    freshBackendTypeIdentity generator =
      let (unique, generator') = freshIdentity generator
       in (typeBinderIdentityFromUnique unique, generator')

    backendTypeIdentityForName _ generator =
      freshBackendTypeIdentity generator

dropTermLocalsMaybe :: Maybe BackendValidationContext -> Maybe BackendValidationContext
dropTermLocalsMaybe =
  fmap
    ( \context0 ->
        context0
          { bvcLocals = Map.empty,
            bvcLocalsByIdentity = Map.empty,
            bvcClosureLocals = Set.empty,
            bvcClosureLocalsByIdentity = Set.empty,
            bvcPossibleClosureLocals = Set.empty,
            bvcPossibleClosureLocalsByIdentity = Set.empty
          }
    )

extendTypeBoundMaybe :: Maybe BackendValidationContext -> Maybe TypeBinderIdentity -> String -> Maybe BackendType -> Maybe BackendValidationContext
extendTypeBoundMaybe mbContext identity name mbBound =
  fmap (\context0 -> context0 {bvcTypeBounds = Map.insert (backendTypeSubstitutionKeyFor identity name) mbBound (bvcTypeBounds context0)}) mbContext

extendTypeBounds :: BackendValidationContext -> [(BackendTypeSubstitutionKey, Maybe BackendType)] -> BackendValidationContext
extendTypeBounds context0 bounds =
  context0 {bvcTypeBounds = foldr (uncurry Map.insert) (bvcTypeBounds context0) bounds}

lookupBackendConstructorInfo :: BackendValidationContext -> Maybe SymbolIdentity -> String -> Maybe BackendConstructorInfo
lookupBackendConstructorInfo context0 mbIdentity name =
  case mbIdentity of
    Just identity -> Map.lookup identity (bvcConstructorsByIdentity context0)
    Nothing -> Map.lookup name (bvcConstructors context0)

canonicalizeBackendTypeDataHeads :: BackendValidationContext -> BackendType -> BackendType
canonicalizeBackendTypeDataHeads context0 =
  canonicalizeBackendTypeDataHeadsWith (bvcData context0) (bvcDataByIdentity context0)

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
          case Map.lookup identity dataDeclsByIdentity of
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
          | Just {} <- structuralSelfIdentityUnique identity = Nothing
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
          [ (backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderBound binder)
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
          case (muIdentity, fieldIdentity) of
            (Just {}, Just {}) ->
              fieldIdentity == muIdentity
            _ ->
              structuralRecursiveDataName fieldName == Just (backendDataName dataDecl)

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
        || backendStructuralDataBoundaryMatches typeBounds mbDataDecls substitutedExpectedTy argTy

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
      Map.notMember key parameters
        && Map.notMember key typeBounds
      where
        key = backendTypeSubstitutionKeyFor identity tyName

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
      requireUniqueBy BackendDuplicatePatternBinding (map patternBinderRef binders)
      unless (length fields == length binderNames) $
        Left (BackendPatternArityMismatch name (length fields) (length binderNames))
      substitution <-
        case matchBackendTypeParametersWithTypeBounds (bvcTypeBounds context0) dataParameters parameters Map.empty constructorResultTy scrutineeTy of
          Just substitution -> pure substitution
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
      [ (backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderIdentity binder, backendTypeBinderName binder)
        | binder <- backendConstructorForalls constructor,
          Map.notMember (backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder)) substitution
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
      let key = backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder),
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
        Just (BTVarWithIdentity freshBinderIdentity freshName) -> backendTypeSubstitutionKeyFor freshBinderIdentity freshName
        _ -> backendTypeSubstitutionKeyFor identity name

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
      ++ [ (backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderBound binder)
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
  unless (structuralDataDeclarationMatches typeBounds (constructorInfoDataDecl constructorInfo) substitution ty) $
    Left mismatchError

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
    ( backendStructuralDataBoundaryMatches typeBounds mbDataDecls substitutedResultTy resultTy
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

backendConstructorResultPlaceholderMatchesEither :: BackendParameterBounds -> BackendType -> BackendType -> Bool
backendConstructorResultPlaceholderMatchesEither typeBounds left right =
  backendConstructorResultPlaceholderMatches typeBounds left right
    || backendConstructorResultPlaceholderMatches typeBounds right left

backendConstructorResultPlaceholderMatches :: BackendParameterBounds -> BackendType -> BackendType -> Bool
backendConstructorResultPlaceholderMatches typeBounds actual expected =
  case (actual, expected) of
    (_, BTVarWithIdentity identity name)
      | not (typeBoundsContain identity name) -> True
    (_, BTVarAppWithIdentity identity name _)
      | not (typeBoundsContain identity name) -> True
    (BTArrow actualDom actualCod, BTArrow expectedDom expectedCod) ->
      backendConstructorResultPlaceholderMatches typeBounds actualDom expectedDom
        && backendConstructorResultPlaceholderMatches typeBounds actualCod expectedCod
    (BTConWithIdentity actualIdentity actualCon actualArgs, BTConWithIdentity expectedIdentity expectedCon expectedArgs)
      | backendTypeHeadMatches actualIdentity actualCon expectedIdentity expectedCon,
        length actualArgs == length expectedArgs ->
          and (zipWith (backendConstructorResultPlaceholderMatches typeBounds) (NE.toList actualArgs) (NE.toList expectedArgs))
    (BTVarAppWithIdentity actualIdentity actualName actualArgs, BTVarAppWithIdentity expectedIdentity expectedName expectedArgs)
      | typeBinderRefMatches actualIdentity actualName expectedIdentity expectedName,
        length actualArgs == length expectedArgs ->
          and (zipWith (backendConstructorResultPlaceholderMatches typeBounds) (NE.toList actualArgs) (NE.toList expectedArgs))
    (BTForallWithIdentity actualIdentity actualName actualBound actualBody, BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody) ->
      backendConstructorResultPlaceholderBoundMatches typeBounds actualBound expectedBound
        && backendConstructorResultPlaceholderMatches
          ( Map.insert (backendTypeSubstitutionKeyFor expectedIdentity expectedName) Nothing $
              Map.insert (backendTypeSubstitutionKeyFor actualIdentity actualName) Nothing typeBounds
          )
          actualBody
          expectedBody
    _ -> alphaEqBackendType actual expected
  where
    typeBoundsContain identity name =
      Map.member (backendTypeSubstitutionKeyFor identity name) typeBounds

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
