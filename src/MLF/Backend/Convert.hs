{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : MLF.Backend.Convert
Description : Convert checked .mlfp programs to typed backend IR

This module is the backend-owned cut from checked `.mlfp` artifacts into the
typed IR from "MLF.Backend.IR". xMLF remains the thesis-faithful typed
elaboration IR, and `MLF.Backend.IR` is the single executable eager backend
IR. Checked-program conversion publishes that eager executable representation
into `MLF.Backend.IR`: direct application, explicit closures and
`BackendClosureCall`, ADT construction and case analysis, lets, lambdas, type
abstraction/application, and roll/unroll. Within that executable contract,
`BackendApp` is reserved for direct first-order callable heads, while
closure-valued aliases, captured closures, and case/let-selected closure
values are emitted as `BackendClosureCall`. The private owner
`MLF.Backend.CallableShape` supplies the shared direct-vs-closure classifier
that conversion consumes with conversion-local scope bookkeeping. Checked-program conversion stops at `MLF.Backend.IR`;
unsupported checked shapes must fail here instead of being rerouted through a second IR layer.
Unsupported checked shapes fail here instead of being normalized into lazy runtime artifacts, lowerer-private
layout forms, or native-wrapper-specific machinery. There are no thunks, no update frames, no CAF update semantics,
no graph reduction, and no implicit laziness rescue at this conversion boundary.
Row-4 ADT/case ownership keeps semantic constructor/case nodes in
`MLF.Backend.IR`: conversion emits `BackendData`, `BackendConstructor`,
`BackendConstruct`, and `BackendCase` metadata/use/alternative nodes only.
Runtime tags, field slots, closure-record storage for function-like fields,
and nullary tag-only representation stay private to LLVM/native lowering, so
checked-program conversion must not assign tag numbers, field offsets, boxing
or storage policy, nullary layout, or layout-only witnesses.
Row-5 primitive/eager ownership keeps the primitive surface at the
inventory-owned reserved runtime-binding set in `MLF.Primitive.Inventory`:
`__mlfp_and` plus the IO primitive names classified there for native support.
Checked-program conversion keeps those primitives on the existing `BackendVar`, `BackendApp`, and `BackendTyApp` surface, with no new `BackendPrim`, no broad FFI surface, and no fallback runtime lane.
The emitted eager structure stays reviewable here: let RHS before body, case scrutinee before branch selection, direct/primitive call arguments in written order, and effect sequencing remains explicit through `__io_bind`.
Row-6 polymorphism/lowerability stays explicit here too: checked `Backend.IR` may still carry `BackendTyAbs` and `BackendTyApp`.
Checked-program conversion preserves those nodes when the checked program needs them instead of erasing polymorphism just to satisfy LLVM.
LLVM/native lowering owns only the specialization-based lowerable subset.
Complete type applications may specialize privately inside the lowerer.
Residual runtime polymorphism remains unsupported and must fail with explicit diagnostics without widening the backend boundary.
Any ANF-like normalization, layout-only structure, or lowerability-only
representation stays private to backend-owned lowering helpers rather than
becoming a second executable IR, a public `LowerableBackend.IR`, or a second
checked-program authority.

A later lower IR may be introduced only when all of the following hold:

* distinct backend-owned executable invariants that cannot live in
  `MLF.Backend.IR` or a private lowering helper;
* a dedicated validation/evidence owner for that new boundary; and
* a later accepted roadmap revision before any new durable or public surface
  is added.
-}
module MLF.Backend.Convert
  ( BackendConversionError (..),
    convertCheckedProgram,
    convertElabType,
    backendTypeToElabType,
    convertSourceType,
    renderBackendConversionError,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, unless, when, zipWithM)
import Control.Monad.State.Strict (StateT (StateT), get, modify, runStateT)
import Data.Char (isAlphaNum)
import Data.List (find, intercalate, nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import MLF.Backend.CallableShape
  ( BackendCallableBindingKind (..),
    BackendCallableHead (..),
    backendCallableHead,
  )
import MLF.Backend.IR hiding
  ( BackendCallableBindingKind (..),
    BackendCallableHead (..),
    backendCallableHead,
  )
import MLF.Backend.IR.Types (freeBackendTypeVarRefs)
import qualified MLF.Backend.StructuralRecursiveData as Structural
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.TypeCheck
  ( Env (..),
    insertResolvedTermBinding,
    insertResolvedTermEnv,
    insertTypeBindingRef,
    lookupResolvedTermEnvEntry,
    mkTypeCheckEnvWithResolvedTerms,
    resolvedTermEnvEntries,
    resolvedTermEnvFromList,
    typeCheckWithEnv,
    unionEnvs,
  )
import MLF.Elab.Types
  ( XmlfTerm (..),
    ResolvedVar (..),
    ResolvedTermIdentityKey,
    ElabScheme,
    ElabType,
    BoundType,
    Instantiation (..),
    Ty (..),
    TypeBinderIdentity,
    TypeBinderRef,
    typeBinderRefIdentity,
    typeBinderRefFromIdentity,
    typeBinderRefName,
    typeBinderRefsSameIdentity,
    sourceTypeBinderRefForName,
    TypeCheckError,
    deferredResolvedVarRef,
    elabToBound,
    identityGeneratorAfterType,
    identityGeneratorAfterTerm,
    generatedIdentitiesInType,
    generatedIdentitiesInTerm,
    localResolvedVarFromRef,
    mapResolvedVarType,
    freshenResolvedLocalVar,
    idDetailsIdentityKey,
    renameResolvedLocalVar,
    renameTypeBinderRef,
    resolvedVarBoundBy,
    resolvedVarConstructorRef,
    resolvedVarIdentityKey,
    resolvedVarIsLocal,
    resolvedVarLocalRef,
    resolvedVarReferenceName,
    resolvedVarSameIdentity,
    resolvedVarSymbolIdentity,
    resolvedVarType,
    schemeFromType,
    tyToElab,
  )

import MLF.Frontend.Program.Builtins (builtinTypeHeadIdentity, builtinValueIdentity, normalizeBuiltinTypeReference, srcTypeMentionsOpaqueBuiltin)
import MLF.Frontend.Program.Elaborate (ElaborateScope, elaborateScopeDataTypes, lowerType, lowerTypeView, mkElaborateScope, sourceTypeBinderIdentitiesInScope)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ClassInfo (..),
    ConstraintInfo (..),
    ConstructorForallBinder (..),
    ConstructorInfo (..),
    ConstructorShape (..),
    DeferredCaseCall (..),
    DeferredConstructorCall (..),
    DataInfo (..),
    DeferredMethodCall (..),
    DeferredMethodEvidence (..),
    DeferredProgramObligation (..),
    EvidenceInfo (..),
    EvidenceMethod (..),
    FunctionalDependencyInfo (..),
    InstanceInfo (..),
    MethodInfo (..),
    ResolvedModule (..),
    ResolvedProgram (..),
    ResolvedScope (..),
    ResolvedSymbol,
    resolvedProgramGeneratedIdentities,
    resolvedProgramSemanticArtifact,
    resolvedSymbolIdentity,
    SymbolNamespace (..),
    SymbolIdentity,
    symbolDefiningModule,
    symbolDefiningName,
    symbolIdentityFromParts,
    TypeView (..),
    ValueInfo (..),
    checkedBindingConstructorRef,
    checkedBindingSourceTypeIdentity,
    checkedProgramMain,
    constructorRefFromInfo,
    constructorRefSymbol,
    dataInfoIdentityName,
    dataInfoIdentityQualifiedName,
    dataParamBinders,
    typeParamBinderIdentity,
    mergeTypeBinderIdentityMaps,
    mergeSymbolIdentityMaps,
    methodParamBinderIdentities,
    ordinaryValueTypeView,
    resolvedModuleIdentity,
    resolvedModuleScope,
    splitArrows,
    splitForalls,
    typeViewBinderIdentityForAlias,
    typeViewHeadIdentityForAlias,
    typeViewMentionedHeadIdentities,
  )
import MLF.Frontend.Symbol (lookupSymbolIdentityAlias, symbolIdentityAliasMap, symbolIdentityAliasNames, symbolIdentityStableName, symbolUniqueIdentity)
import MLF.Frontend.Syntax (Lit, SrcBound (..), SrcTy (..), SrcType, TypeParam)
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (DeferredRef, IdDetails (..), IdentityGenerator, LocalRef, StructuralTypeBinderRole (..), UniqueIdentity (..), advanceIdentityGeneratorPast, deferredRefIdentity, deferredRefName, freshDeferredRef, freshIdentity, freshLocalRef, idDetailsGeneratedIdentities, idDetailsSymbolIdentity, identityGeneratorAfter, symbolGeneratedIdentities, typeBinderGeneratedIdentities, typeBinderIdentityFromStructural, typeBinderIdentityNode, typeBinderIdentityStructural)
import MLF.Util.Names (freshNameLike)

data BackendConversionError
  = BackendUnsupportedSourceType SrcType
  | BackendUnsupportedInstantiation Instantiation
  | BackendUnsupportedRecursiveLet String
  | BackendUnsupportedCaseShape String
  | BackendTypeCheckFailed XmlfTerm TypeCheckError
  | BackendValidationFailed BackendValidationError
  deriving (Eq, Show)

renderBackendConversionError :: BackendConversionError -> String
renderBackendConversionError err =
  case err of
    BackendUnsupportedSourceType ty ->
      "Unsupported backend source type: " ++ show ty
    BackendUnsupportedInstantiation inst ->
      "Unsupported backend instantiation: " ++ show inst
    BackendUnsupportedRecursiveLet detail ->
      "Unsupported backend recursive let: " ++ detail
    BackendUnsupportedCaseShape detail ->
      "Unsupported backend conversion shape: " ++ detail
    BackendTypeCheckFailed term typeErr ->
      "Backend typecheck failed for converted term " ++ show term ++ ": " ++ show typeErr
    BackendValidationFailed validationErr ->
      "Backend IR validation failed: " ++ show validationErr

data ConvertContext = ConvertContext
  { ccModuleScopes :: Map SymbolIdentity ElaborateScope,
    ccConstructorsByIdentity :: Map SymbolIdentity ConstructorMeta,
    ccTermRuntimeNamesByIdentity :: Map SymbolIdentity String,
    ccBindingData :: Map SymbolIdentity DataMeta,
    ccDataByIdentity :: Map SymbolIdentity DataMeta,
    ccDataModuleIdentities :: Map SymbolIdentity SymbolIdentity,
    ccData :: [DataMeta],
    ccClosureGlobalsByIdentity :: Set.Set SymbolIdentity,
    ccClosureValueArgumentsByIdentity :: Map SymbolIdentity (Set.Set Int),
    ccEvidenceValueArgumentsByIdentity :: Map SymbolIdentity (Set.Set Int),
    ccClosureValueArgumentsByDeferred :: Map DeferredRef (Set.Set Int),
    ccEvidenceValueArgumentsByDeferred :: Map DeferredRef (Set.Set Int),
    ccEvidenceResolvedVarKeys :: Set.Set ResolvedTermIdentityKey,
    ccIdentityGenerator :: IdentityGenerator,
    ccCurrentModuleIdentity :: Maybe SymbolIdentity,
    ccCurrentBindingName :: String
  }

data ConstructorMeta = ConstructorMeta
  { cmInfo :: ConstructorInfo,
    cmBackend :: BackendConstructor,
    cmData :: DataMeta
  }

data DataMeta = DataMeta
  { dmInfo :: DataInfo,
    dmBackend :: BackendData
  }

data ConstructorApplication = ConstructorApplication ConstructorMeta [BackendType] [XmlfTerm]

data ConstructorHeadKey
  = ConstructorHeadIdentity SymbolIdentity
  deriving (Eq, Show)

type BackendParameterBounds = Map BackendTypeSubstitutionKey (Maybe BackendType)

type BackendParameterSubstitution = Map BackendTypeSubstitutionKey BackendType

type BackendTypeBounds = Map BackendTypeSubstitutionKey (Maybe BackendType)

data BackendTypeAbsBinder = BackendTypeAbsBinder (Maybe TypeBinderIdentity) String (Maybe BackendType)

data LiftedRecursiveLet = LiftedRecursiveLet
  { lrlName :: String,
    lrlRef :: DeferredRef,
    lrlSymbol :: SymbolIdentity,
    lrlResolved :: ResolvedVar,
    lrlElabType :: ElabType,
    lrlBackendType :: BackendType,
    lrlTerm :: XmlfTerm,
    lrlClosureValueArguments :: Set.Set Int,
    lrlEvidenceValueArguments :: Set.Set Int
  }

data LiftState = LiftState
  { lsNextHelperIndex :: Int,
    lsLiftedRecursiveLets :: [LiftedRecursiveLet],
    lsGeneratedHelperNames :: Set.Set String,
    lsIdentityGenerator :: IdentityGenerator
  }

type LiftM = StateT LiftState (Either BackendConversionError)

data ConvertState = ConvertState
  { csGeneratedClosureNames :: Set.Set String,
    csIdentityGenerator :: IdentityGenerator
  }

type ConvertM = StateT ConvertState (Either BackendConversionError)

data ClosureScope = ClosureScope
  { closureScopeResolvedTerms :: [ResolvedVar],
    closureScopeBoundResolvedTerms :: [ResolvedVar],
    closureScopeLocalResolvedTerms :: [ResolvedVar],
    closureScopeResolvedTermKeys :: Set.Set ResolvedTermIdentityKey,
    closureScopeBoundResolvedTermKeys :: Set.Set ResolvedTermIdentityKey,
    closureScopeLocalResolvedTermKeys :: Set.Set ResolvedTermIdentityKey,
    closureScopeClosureValueArgumentsByLocal :: Map LocalRef (Set.Set Int),
    closureScopeEvidenceValueArgumentsByLocal :: Map LocalRef (Set.Set Int)
  }

data LambdaMode
  = DirectLambda
  | ClosureLambda (Maybe String)

data PartialApplicationMode
  = AllowPartialApplications
  | SuppressPartialApplications

emptyClosureScope :: ClosureScope
emptyClosureScope =
  ClosureScope
    { closureScopeResolvedTerms = [],
      closureScopeBoundResolvedTerms = [],
      closureScopeLocalResolvedTerms = [],
      closureScopeResolvedTermKeys = Set.empty,
      closureScopeBoundResolvedTermKeys = Set.empty,
      closureScopeLocalResolvedTermKeys = Set.empty,
      closureScopeClosureValueArgumentsByLocal = Map.empty,
      closureScopeEvidenceValueArgumentsByLocal = Map.empty
    }

extendClosureScopeResolvedTerm :: ResolvedVar -> ElabType -> Bool -> ClosureScope -> ClosureScope
extendClosureScopeResolvedTerm resolved ty isClosure scope =
  let resolved' = mapResolvedVarType (const ty) resolved
      key = resolvedVarIdentityKey resolved'
   in scope
        { closureScopeResolvedTerms =
            resolved' : removeResolvedTermKey key (closureScopeResolvedTerms scope),
          closureScopeBoundResolvedTerms =
            resolved' : removeResolvedTermKey key (closureScopeBoundResolvedTerms scope),
          closureScopeLocalResolvedTerms =
            if isClosure
              then resolved' : removeResolvedTermKey key (closureScopeLocalResolvedTerms scope)
              else removeResolvedTermKey key (closureScopeLocalResolvedTerms scope),
          closureScopeResolvedTermKeys =
            Set.insert key (closureScopeResolvedTermKeys scope),
          closureScopeBoundResolvedTermKeys =
            Set.insert key (closureScopeBoundResolvedTermKeys scope),
          closureScopeLocalResolvedTermKeys =
            if isClosure
              then Set.insert key (closureScopeLocalResolvedTermKeys scope)
              else Set.delete key (closureScopeLocalResolvedTermKeys scope),
          closureScopeClosureValueArgumentsByLocal =
            maybe id Map.delete (resolvedVarLocalRef resolved') (closureScopeClosureValueArgumentsByLocal scope),
          closureScopeEvidenceValueArgumentsByLocal =
            maybe id Map.delete (resolvedVarLocalRef resolved') (closureScopeEvidenceValueArgumentsByLocal scope)
        }
  where
    removeResolvedTermKey key =
      filter ((/= key) . resolvedVarIdentityKey)

closureScopeHasBoundTerm :: ResolvedVar -> ClosureScope -> Bool
closureScopeHasBoundTerm resolved scope =
  Set.member (resolvedVarIdentityKey resolved) (closureScopeBoundResolvedTermKeys scope)

closureScopeHasLocalTerm :: ResolvedVar -> ClosureScope -> Bool
closureScopeHasLocalTerm resolved scope =
  Set.member (resolvedVarIdentityKey resolved) (closureScopeLocalResolvedTermKeys scope)

closureScopeHasLocalDetails :: IdDetails -> ClosureScope -> Bool
closureScopeHasLocalDetails details scope =
  Set.member (idDetailsIdentityKey details) (closureScopeLocalResolvedTermKeys scope)

closureScopeHasBoundDetails :: IdDetails -> ClosureScope -> Bool
closureScopeHasBoundDetails details scope =
  Set.member (idDetailsIdentityKey details) (closureScopeBoundResolvedTermKeys scope)

extendClosureScopeValueArguments :: ResolvedVar -> Set.Set Int -> ClosureScope -> ClosureScope
extendClosureScopeValueArguments resolved demanded scope =
  scope
    { closureScopeClosureValueArgumentsByLocal =
        case resolvedVarLocalRef resolved of
          Just localRef
            | Set.null demanded -> Map.delete localRef (closureScopeClosureValueArgumentsByLocal scope)
            | otherwise -> Map.insert localRef demanded (closureScopeClosureValueArgumentsByLocal scope)
          Nothing -> closureScopeClosureValueArgumentsByLocal scope
    }

extendClosureScopeEvidenceArguments :: ResolvedVar -> Set.Set Int -> ClosureScope -> ClosureScope
extendClosureScopeEvidenceArguments resolved evidence scope =
  scope
    { closureScopeEvidenceValueArgumentsByLocal =
        case resolvedVarLocalRef resolved of
          Just localRef
            | Set.null evidence -> Map.delete localRef (closureScopeEvidenceValueArgumentsByLocal scope)
            | otherwise -> Map.insert localRef evidence (closureScopeEvidenceValueArgumentsByLocal scope)
          Nothing -> closureScopeEvidenceValueArgumentsByLocal scope
    }

closureScopeBoundTermNames :: ClosureScope -> Set.Set String
closureScopeBoundTermNames =
  Set.fromList . map resolvedVarReferenceName . closureScopeBoundResolvedTerms

extendClosureScopePatternFields :: [((ResolvedVar, ElabType), BackendType)] -> ClosureScope -> ClosureScope
extendClosureScopePatternFields bindings scope =
  foldr
    ( \((resolved, ty), fieldTy) acc ->
        extendClosureScopeResolvedTerm resolved ty (isClosureConvertibleFunctionType fieldTy) acc
    )
    scope
    bindings

extendClosureScopeLambdaParams :: ConvertContext -> [TermCapture] -> ClosureScope -> ClosureScope
extendClosureScopeLambdaParams context bindings scope =
  foldr
    ( \(resolved, ty) acc ->
        extendClosureScopeResolvedTerm
          resolved
          ty
          (isClosureConvertibleResolvedBinding context resolved ty)
          acc
    )
    scope
    bindings

isClosureConvertibleResolvedBinding :: ConvertContext -> ResolvedVar -> ElabType -> Bool
isClosureConvertibleResolvedBinding context resolved ty =
  not (isEvidenceCapture context resolved) && isClosureConvertibleElabType ty

isEvidenceCapture :: ConvertContext -> ResolvedVar -> Bool
isEvidenceCapture context resolved =
  Set.member (resolvedVarIdentityKey resolved) (ccEvidenceResolvedVarKeys context)

runConvertMWithGenerator :: IdentityGenerator -> ConvertM a -> Either BackendConversionError (a, IdentityGenerator)
runConvertMWithGenerator generator action = do
  (value, state') <-
    runStateT
      action
      ConvertState
        { csGeneratedClosureNames = Set.empty,
          csIdentityGenerator = generator
        }
  Right (value, csIdentityGenerator state')

liftEitherConvert :: Either BackendConversionError a -> ConvertM a
liftEitherConvert result =
  StateT $ \state0 ->
    case result of
      Right value -> Right (value, state0)
      Left err -> Left err

convertCheckedProgram :: CheckedProgram -> Either BackendConversionError BackendProgram
convertCheckedProgram checked = do
  rejectOpaqueBuiltinMain checked
  context0 <- buildConvertContext checked
  initialEnv <- buildInitialEnv context0 checked
  closureGlobals <- convertedProgramClosureGlobals context0 initialEnv checked
  let context =
        context0
          { ccClosureGlobalsByIdentity = closureGlobals
          }
  modules0 <- convertCheckedModules context initialEnv (checkedProgramModules checked)
  let mainIdentity = resolvedVarSymbolIdentity (checkedProgramMainResolvedVar checked)
      mainName = backendMainName mainIdentity modules0 (checkedProgramMain checked)
  let program =
        BackendProgramWithIdentity
          { backendProgramModulesWithIdentity = modules0,
            backendProgramMainIdentity = mainIdentity,
            backendProgramMainWithIdentity = mainName
          }
  case validateBackendProgram program of
    Right () -> Right program
    Left err -> Left (BackendValidationFailed err)

convertedProgramClosureGlobals :: ConvertContext -> Env -> CheckedProgram -> Either BackendConversionError (Set.Set SymbolIdentity)
convertedProgramClosureGlobals context0 env checked =
  closureGlobalFixedPoint Set.empty
  where
    closureGlobalFixedPoint globalIdentities = do
      let context =
            context0
              { ccClosureGlobalsByIdentity = globalIdentities
              }
      convertedBindings <-
        concat
          <$> mapM
            ( \checkedModule ->
                concat
                  <$> mapM
                    ( \binding -> do
                        (converted, _) <- convertCheckedBinding context env checkedModule (ccIdentityGenerator context) binding
                        pure [(binding, convertedBinding) | convertedBinding <- converted]
                    )
                    (checkedModuleBindings checkedModule)
            )
            (checkedProgramModules checked)
      let detectedGlobalIdentities =
            Set.fromList
              [ symbol
              | (binding, convertedBinding) <- convertedBindings,
                backendExprIsClosureValue context emptyClosureScope (backendBindingExpr convertedBinding),
                Just symbol <- [checkedBindingSymbolIdentity binding]
              ]
          globalIdentities' =
            globalIdentities <> detectedGlobalIdentities
      if globalIdentities' == globalIdentities
        then pure globalIdentities
        else closureGlobalFixedPoint globalIdentities'

backendMainName :: Maybe SymbolIdentity -> [BackendModule] -> String -> String
backendMainName mbIdentity modules0 fallback =
  case mbIdentity of
    Just identity ->
      case find ((== Just identity) . backendBindingIdentity) bindings of
        Just binding -> backendBindingName binding
        Nothing -> fallback
    Nothing ->
      fallback
  where
    bindings =
      concatMap backendModuleBindings modules0

buildInitialEnv :: ConvertContext -> CheckedProgram -> Either BackendConversionError Env
buildInitialEnv context checked = do
  resolvedTerms <-
    forM
      [ (checkedModule, binding)
        | checkedModule <- checkedProgramModules checked,
          binding <- checkedModuleBindings checkedModule
      ]
      ( \(checkedModule, binding) -> do
          bindingTy <- checkedBindingEnvType context checkedModule binding
          Right (checkedBindingResolvedVar binding, bindingTy)
      )
  Right
    ( mkTypeCheckEnvWithResolvedTerms resolvedTerms Map.empty
        `unionEnvs` mkTypeCheckEnvWithResolvedTerms (backendBuiltinResolvedTermTypes context) Map.empty
    )

checkedBindingEnvType :: ConvertContext -> CheckedModule -> CheckedBinding -> Either BackendConversionError ElabType
checkedBindingEnvType context checkedModule binding = do
  canonicalElabTyOpen <- checkedBindingCanonicalTypeOpen context checkedModule binding
  let freeTypeBinders = sortTypeBinderRefsByIdentity (freeElabTypeVarRefs canonicalElabTyOpen)
      canonicalElabTy = quantifyFreeElabTypeVarRefs freeTypeBinders canonicalElabTyOpen
  rawBackendTy <- convertElabType canonicalElabTy
  let sourceBindingTy =
        canonicalizeBackendType context $
          applySourceTypeIdentity
            context
            (scopeForModule context (checkedModuleIdentity checkedModule))
            (checkedBindingSourceTypeView binding)
            rawBackendTy
      finalBindingTy =
        case constructorMetaForBinding context binding of
          Just constructorMeta
            | constructorBindingResultMatches sourceBindingTy constructorMeta ->
                constructorBackendBindingType constructorMeta
          _ ->
            sourceBindingTy
  case backendTypeToElabTypeSeededByElabType canonicalElabTy finalBindingTy of
    Just envTy -> Right envTy
    Nothing -> Right canonicalElabTy

backendBuiltinTermTypes :: Map String ElabType
backendBuiltinTermTypes =
  PrimitiveInventory.primitiveValueElabTypes

backendBuiltinResolvedTermTypes :: ConvertContext -> [(ResolvedVar, ElabType)]
backendBuiltinResolvedTermTypes context =
  [ ( builtinResolvedVar name canonicalTy,
      canonicalTy
    )
  | (name, ty) <- Map.toList backendBuiltinTermTypes,
    let canonicalTy = canonicalizeBuiltinEnvType context ty
  ]

canonicalizeBuiltinEnvType :: ConvertContext -> ElabType -> ElabType
canonicalizeBuiltinEnvType context ty =
  case convertElabType ty of
    Right backendTy ->
      case backendTypeToElabTypeSeededByElabType ty (normalizeBackendTypeForContext context backendTy) of
        Just canonicalTy -> canonicalTy
        Nothing -> ty
    Left _ ->
      ty

builtinResolvedVar :: String -> ElabType -> ResolvedVar
builtinResolvedVar name ty =
  ResolvedVar
    { resolvedVarRuntimeName = name,
      resolvedVarType = ty,
      resolvedVarDetails = TopLevelId (builtinValueIdentity name)
    }

convertCheckedModules :: ConvertContext -> Env -> [CheckedModule] -> Either BackendConversionError [BackendModule]
convertCheckedModules context env modules0 =
  reverse . snd <$> foldM convertOne (ccIdentityGenerator context, []) modules0
  where
    convertOne (generator, modulesRev) checkedModule = do
      (backendModule, generator') <- convertCheckedModule context env generator checkedModule
      pure (generator', backendModule : modulesRev)

convertCheckedModule :: ConvertContext -> Env -> IdentityGenerator -> CheckedModule -> Either BackendConversionError (BackendModule, IdentityGenerator)
convertCheckedModule context env generator0 checkedModule = do
  dataDecls <- mapM (convertDataInfo context) (Map.elems (checkedModuleData checkedModule))
  (bindings, generator') <- convertCheckedBindings context env checkedModule generator0 (checkedModuleBindings checkedModule)
  Right
    ( BackendModuleWithIdentity
        { backendModuleIdentity = Just (checkedModuleIdentity checkedModule),
          backendModuleNameWithIdentity = symbolDefiningName (checkedModuleIdentity checkedModule),
          backendModuleDataWithIdentity = dataDecls,
          backendModuleBindingsWithIdentity = bindings
        },
      generator'
    )

convertCheckedBindings :: ConvertContext -> Env -> CheckedModule -> IdentityGenerator -> [CheckedBinding] -> Either BackendConversionError ([BackendBinding], IdentityGenerator)
convertCheckedBindings context env checkedModule generator0 bindings0 =
  foldM convertOne ([], generator0) bindings0 >>= \(bindingsRev, generator') ->
    Right (reverse bindingsRev, generator')
  where
    convertOne (bindingsRev, generator) binding = do
      (converted, generator') <- convertCheckedBinding context env checkedModule generator binding
      pure (reverse converted ++ bindingsRev, generator')

rejectOpaqueBuiltinMain :: CheckedProgram -> Either BackendConversionError ()
rejectOpaqueBuiltinMain _checked =
  Right ()

convertCheckedBinding :: ConvertContext -> Env -> CheckedModule -> IdentityGenerator -> CheckedBinding -> Either BackendConversionError ([BackendBinding], IdentityGenerator)
convertCheckedBinding context env checkedModule generator0 binding = do
  let bindingContext =
        context
          { ccCurrentModuleIdentity = Just (checkedModuleIdentity checkedModule),
            ccCurrentBindingName = checkedBindingRuntimeName binding
          }
  canonicalElabTyOpen <- checkedBindingCanonicalTypeOpen context checkedModule binding
  let freeTypeBinders = sortTypeBinderRefsByIdentity (freeElabTypeVarRefs canonicalElabTyOpen)
      canonicalElabTy = quantifyFreeElabTypeVarRefs freeTypeBinders canonicalElabTyOpen
      checkedBindingTermClosed =
        wrapElabTypeAbsRefs freeTypeBinders $
          alignLeadingTypeAbsRefsToType canonicalElabTy (checkedBindingTerm binding)
  rawBindingTy <- convertElabType canonicalElabTy
  let bindingTy =
        canonicalizeBackendType context $
          applySourceTypeIdentity
            context
            (scopeForModule context (checkedModuleIdentity checkedModule))
            (checkedBindingSourceTypeView binding)
            rawBindingTy
  (convertedBindingTy, expr, liftedBindings, generator') <-
    case constructorMetaForBinding context binding of
      Just constructorMeta
        | constructorBindingResultMatches bindingTy constructorMeta ->
            do
              let constructorBindingTy = constructorBackendBindingType constructorMeta
              (expr, generator') <- runConvertMWithGenerator generator0 (synthesizeConstructorBinding constructorBindingTy constructorMeta)
              Right (constructorBindingTy, expr, [], generator')
      _ -> do
        (liftedTerm, liftedSpecs, generatorAfterLift) <- liftRecursiveLetsInBinding bindingContext generator0 canonicalElabTy checkedBindingTermClosed
        let bindingContextWithLifted =
              extendContextWithLiftedRecursiveLets bindingContext liftedSpecs
        let envWithLifted =
              foldr
                (\lifted acc -> insertResolvedTermBinding (lrlResolved lifted) (lrlElabType lifted) acc)
                env
                liftedSpecs
            opaqueBinding =
              srcTypeMentionsOpaqueBuiltin (checkedBindingSourceTypeIdentity binding)
            expectedBindingTy =
              if opaqueBinding && not (checkedBindingExportedAsMain binding)
                then Nothing
                else Just bindingTy
        ((liftedBindings, expr), generatorAfterConvert) <-
          runConvertMWithGenerator generatorAfterLift $ do
            liftedBindings <-
              zipWith
                (\lifted converted -> converted {backendBindingEvidenceParamIndices = lrlEvidenceValueArguments lifted})
                liftedSpecs
                <$> mapM (convertLiftedRecursiveLet bindingContextWithLifted envWithLifted) liftedSpecs
            expr <- convertTermExpectedMode DirectLambda bindingContextWithLifted envWithLifted emptyClosureScope expectedBindingTy liftedTerm
            pure (liftedBindings, expr)
        -- For opaque bindings (types mentioning IO etc.), the expression type
        -- from the builtin is authoritative. Prelude primitive data annotations
        -- keep their source identity when primitive results are only
        -- structurally compatible; other compatible mismatches keep the legacy
        -- expression type.
        let exprTy = backendExprType expr
            finalBindingTy
              | alphaEqBackendType bindingTy exprTy = bindingTy
              | opaqueBinding = exprTy
              | backendTypesCompatible context bindingTy exprTy =
                  if preludePrimitiveBackendTypeHead context bindingTy
                    then bindingTy
                    else exprTy
              | otherwise = bindingTy
        Right (finalBindingTy, expr, liftedBindings, generatorAfterConvert)
  let convertedBinding =
        BackendBindingWithMetadata
          { backendBindingIdentity = checkedBindingSymbolIdentity binding,
            backendBindingNameWithMetadata = checkedBindingRuntimeName binding,
            backendBindingTypeWithMetadata = convertedBindingTy,
            backendBindingExprWithMetadata = expr,
            backendBindingExportedAsMainWithMetadata = checkedBindingExportedAsMain binding,
            backendBindingEvidenceParamIndices = bindingEvidenceParams
          }
      bindingEvidenceParams =
        case checkedBindingSymbolIdentity binding of
          Just symbol -> Map.findWithDefault Set.empty symbol (ccEvidenceValueArgumentsByIdentity context)
          Nothing -> Set.empty
  Right (convertedBinding : liftedBindings, generator')

constructorMetaForBinding :: ConvertContext -> CheckedBinding -> Maybe ConstructorMeta
constructorMetaForBinding context binding = do
  constructorRef <- checkedBindingConstructorRef binding
  Map.lookup (constructorRefSymbol constructorRef) (ccConstructorsByIdentity context)

extendContextWithLiftedRecursiveLets :: ConvertContext -> [LiftedRecursiveLet] -> ConvertContext
extendContextWithLiftedRecursiveLets context liftedSpecs =
  context
    { ccTermRuntimeNamesByIdentity =
        Map.union (Map.fromList (map liftedRuntimeName liftedSpecs)) (ccTermRuntimeNamesByIdentity context),
      ccClosureValueArgumentsByIdentity =
        mergeDemands liftedClosureValueArgumentsByIdentity (ccClosureValueArgumentsByIdentity context),
      ccEvidenceValueArgumentsByIdentity =
        mergeDemands liftedEvidenceValueArgumentsByIdentity (ccEvidenceValueArgumentsByIdentity context),
      ccClosureValueArgumentsByDeferred =
        mergeDemands liftedClosureValueArguments (ccClosureValueArgumentsByDeferred context),
      ccEvidenceValueArgumentsByDeferred =
        mergeDemands liftedEvidenceValueArguments (ccEvidenceValueArgumentsByDeferred context)
    }
  where
    mergeDemands :: Ord key => (LiftedRecursiveLet -> (key, Set.Set Int)) -> Map key (Set.Set Int) -> Map key (Set.Set Int)
    mergeDemands build =
      Map.unionWith Set.union (Map.filter (not . Set.null) (Map.fromList (map build liftedSpecs)))

    liftedRuntimeName lifted =
      (lrlSymbol lifted, lrlName lifted)

    liftedClosureValueArgumentsByIdentity lifted =
      (lrlSymbol lifted, lrlClosureValueArguments lifted)

    liftedEvidenceValueArgumentsByIdentity lifted =
      (lrlSymbol lifted, lrlEvidenceValueArguments lifted)

    liftedClosureValueArguments lifted =
      (lrlRef lifted, lrlClosureValueArguments lifted)

    liftedEvidenceValueArguments lifted =
      (lrlRef lifted, lrlEvidenceValueArguments lifted)

liftRecursiveLetsInBinding :: ConvertContext -> IdentityGenerator -> ElabType -> XmlfTerm -> Either BackendConversionError (XmlfTerm, [LiftedRecursiveLet], IdentityGenerator)
liftRecursiveLetsInBinding context generator0 bindingTy term = do
  (term', state') <-
    runStateT
      (liftRecursiveLetsInTerm context [] (leadingElabForallCaptures bindingTy) term)
      LiftState
        { lsNextHelperIndex = 0,
          lsLiftedRecursiveLets = [],
          lsGeneratedHelperNames = Set.empty,
          lsIdentityGenerator = generator0
        }
  Right (term', lsLiftedRecursiveLets state', lsIdentityGenerator state')

leadingElabForallCaptures :: ElabType -> [TypeCapture]
leadingElabForallCaptures =
  \case
    TForallRef ref mb body -> (ref, mb) : leadingElabForallCaptures body
    _ -> []

convertLiftedRecursiveLet :: ConvertContext -> Env -> LiftedRecursiveLet -> ConvertM BackendBinding
convertLiftedRecursiveLet context env lifted = do
  let bindingTy = canonicalizeBackendType context (lrlBackendType lifted)
  expr <- convertTermExpectedMode DirectLambda context env emptyClosureScope (Just bindingTy) (lrlTerm lifted)
  pure
    BackendBindingWithMetadata
      { backendBindingIdentity = Just (lrlSymbol lifted),
        backendBindingNameWithMetadata = lrlName lifted,
        backendBindingTypeWithMetadata = bindingTy,
        backendBindingExprWithMetadata = expr,
        backendBindingExportedAsMainWithMetadata = False,
        backendBindingEvidenceParamIndices = Set.empty
      }

liftRecursiveLetsInTerm ::
  ConvertContext ->
  [ResolvedVar] ->
  [TypeCapture] ->
  XmlfTerm ->
  LiftM XmlfTerm
liftRecursiveLetsInTerm context lexicalTerms lexicalTypes term =
  case term of
    EVarNode {} ->
      pure term
    ELit {} ->
      pure term
    ELam resolved body ->
      ELam resolved
        <$> liftRecursiveLetsInTerm context (extendLexicalResolvedTerm resolved lexicalTerms) lexicalTypes body
    EApp fun arg ->
      EApp
        <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes fun
        <*> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes arg
    ELet resolved scheme rhs body ->
      liftLet
        resolved
        scheme
        rhs
        body
        (ELet resolved scheme)
    ETyAbsRef ref mbBound body ->
      ETyAbsRef ref mbBound <$> liftRecursiveLetsInTerm context lexicalTerms (insertLexicalTypeBinding ref mbBound lexicalTypes) body
    ETyInst inner inst ->
      ETyInst <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes inner <*> pure inst
    ERoll ty body ->
      ERoll ty <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes body
    EUnroll body ->
      EUnroll <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes body
  where
    liftLet ::
      ResolvedVar ->
      ElabScheme ->
      XmlfTerm ->
      XmlfTerm ->
      (XmlfTerm -> XmlfTerm -> XmlfTerm) ->
      LiftM XmlfTerm
    liftLet resolved scheme rhs body rebuild = do
      let schemeTy = schemeToType scheme
          name = resolvedVarReferenceName resolved
          key = TermVarResolved resolved
          bodyResolved = mapResolvedVarType (const schemeTy) resolved
          bodyTerms = extendLexicalResolvedTerm bodyResolved lexicalTerms
          recursiveRhs = isFunctionValueTerm rhs && termMentionsFreeVariable key rhs
      if recursiveRhs
        then do
          bindingTy0 <- liftEitherConversion (convertElabType schemeTy)
          let bindingTy = normalizeBackendTypeForContext context bindingTy0
          termCaptures <- capturedTermBindings (removeLexicalResolvedTerm bodyResolved lexicalTerms) rhs
          typeCaptures <- capturedTypeBindings lexicalTypes schemeTy termCaptures rhs
          ensureLiftableRecursiveLet context name bindingTy termCaptures rhs
          let helperTypeCaptures = closeHelperTypeCaptures typeCaptures termCaptures schemeTy
              helperElabType = helperType helperTypeCaptures termCaptures schemeTy
          helperRef <- freshLiftedRecursiveLetRef context name
          let helperSymbol = liftedRecursiveLetSymbol context helperRef
              helperResolved =
                ResolvedVar
                  { resolvedVarRuntimeName = deferredRefName helperRef,
                    resolvedVarType = helperElabType,
                    resolvedVarDetails = TopLevelId helperSymbol
                  }
          rhs' <- liftRecursiveLetsInTerm context bodyTerms lexicalTypes rhs
          let helperApplication = applyHelperCaptures helperResolved helperTypeCaptures termCaptures
              helperTerm =
                wrapHelperTypeCaptures helperTypeCaptures $
                  wrapHelperTermCaptures termCaptures $
                    replaceFreeTermVariable key helperApplication rhs'
          helperBackendType <- liftEitherConversion (canonicalizeBackendType context <$> convertElabType helperElabType)
          let helperClosureValueArguments =
                bindingClosureValueArguments context emptyClosureScope helperBackendType helperTerm
              helperEvidenceValueArguments =
                bindingEvidenceValueArguments context emptyClosureScope helperBackendType helperTerm
          emitLiftedRecursiveLet
            LiftedRecursiveLet
              { lrlName = deferredRefName helperRef,
                lrlRef = helperRef,
                lrlSymbol = helperSymbol,
                lrlResolved = helperResolved,
                lrlElabType = helperElabType,
                lrlBackendType = helperBackendType,
                lrlTerm = helperTerm,
                lrlClosureValueArguments = helperClosureValueArguments,
                lrlEvidenceValueArguments = helperEvidenceValueArguments
              }
          body' <- liftRecursiveLetsInTerm context bodyTerms lexicalTypes body
          pure (rebuild helperApplication body')
        else
          rebuild
            <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes rhs
            <*> liftRecursiveLetsInTerm context bodyTerms lexicalTypes body

    extendLexicalResolvedTerm resolved =
      (resolved :) . removeLexicalResolvedTerm resolved

    removeLexicalResolvedTerm resolved =
      filter ((/= resolvedVarIdentityKey resolved) . resolvedVarIdentityKey)

type TermCapture = (ResolvedVar, ElabType)
type TypeCapture = (TypeBinderRef, Maybe BoundType)

capturedTermBindings :: [ResolvedVar] -> XmlfTerm -> LiftM [TermCapture]
capturedTermBindings lexicalTerms rhs =
  pure (capturedTermBindingsIn lexicalTerms rhs)

capturedTermBindingsIn :: [ResolvedVar] -> XmlfTerm -> [TermCapture]
capturedTermBindingsIn lexicalTerms rhs =
  [ (resolved, resolvedVarType resolved)
  | resolved <- lexicalTerms,
    Set.member (resolvedVarIdentityKey resolved) freeKeys
  ]
  where
    freeKeys =
      Set.fromList (map resolvedVarIdentityKey (freeResolvedTermVariables rhs))

insertLexicalTypeBinding :: TypeBinderRef -> Maybe BoundType -> [TypeCapture] -> [TypeCapture]
insertLexicalTypeBinding ref mbBound lexicalTypes =
  filter (not . typeBinderRefsSameIdentity ref . fst) lexicalTypes ++ [(ref, mbBound)]

capturedTypeBindings :: [TypeCapture] -> ElabType -> [TermCapture] -> XmlfTerm -> LiftM [TypeCapture]
capturedTypeBindings lexicalTypes schemeTy termCaptures rhs =
  pure
    [ capture
    | capture@(ref, _) <- lexicalTypes,
      any (typeRefMayBeBoundBy ref) freeVars
    ]
  where
    freeVars =
      freeElabTypeVarRefs schemeTy
        `unionTypeRefs` unionsTypeRefs (map (freeElabTypeVarRefs . snd) termCaptures)
        `unionTypeRefs` freeXmlfTermTypeVarRefs rhs

helperType :: [TypeCapture] -> [TermCapture] -> ElabType -> ElabType
helperType typeCaptures termCaptures bodyTy =
  foldr wrapType (foldr (TArrow . snd) bodyTy termCaptures) typeCaptures
  where
    wrapType (ref, mbBound) acc =
      TForallRef ref mbBound acc

closeHelperTypeCaptures :: [TypeCapture] -> [TermCapture] -> ElabType -> [TypeCapture]
closeHelperTypeCaptures typeCaptures termCaptures bodyTy =
  foldl appendMissingTypeCapture typeCaptures (freeElabTypeVarRefs openHelperTy)
  where
    openHelperTy = helperType typeCaptures termCaptures bodyTy
    appendMissingTypeCapture captures ref
      | any (typeBinderRefsSameIdentity ref . fst) captures = captures
      | otherwise = captures ++ [(ref, Nothing)]

wrapHelperTypeCaptures :: [TypeCapture] -> XmlfTerm -> XmlfTerm
wrapHelperTypeCaptures typeCaptures body =
  foldr wrap body typeCaptures
  where
    wrap (ref, mbBound) acc =
      ETyAbsRef ref mbBound acc

wrapHelperTermCaptures :: [TermCapture] -> XmlfTerm -> XmlfTerm
wrapHelperTermCaptures termCaptures body =
  foldr wrap body termCaptures
  where
    wrap (resolved, ty) acc =
      ELam (mapResolvedVarType (const ty) resolved) acc

applyHelperCaptures :: ResolvedVar -> [TypeCapture] -> [TermCapture] -> XmlfTerm
applyHelperCaptures helperResolved typeCaptures termCaptures =
  foldl EApp typedHelper [EVarNode (mapResolvedVarType (const ty) resolved) | (resolved, ty) <- termCaptures]
  where
    typedHelper =
      foldl
        (\acc (ref, _) -> ETyInst acc (InstApp (TVarRef ref)))
        (EVarNode helperResolved)
        typeCaptures

ensureLiftableRecursiveLet :: ConvertContext -> String -> BackendType -> [TermCapture] -> XmlfTerm -> LiftM ()
ensureLiftableRecursiveLet context name bindingTy captures rhs = do
  let unsupported reason =
        BackendUnsupportedRecursiveLet
          ( name
              ++ " ("
              ++ reason
              ++ ")"
          )
  unless (all (isEvidenceCapture context . fst) captures) $
    throwLiftError
      ( unsupported
          ("captures lexical bindings: " ++ intercalate ", " (map (resolvedVarReferenceName . fst) captures))
      )
  unless (isLiftableRecursiveFunctionType bindingTy) $
    throwLiftError (unsupported "expected a monomorphic runtime-representable function type")
  unless (isFunctionValueTerm rhs) $
    throwLiftError (unsupported "expected a function-valued recursive right-hand side")

freshLiftedRecursiveLetRef :: ConvertContext -> String -> LiftM DeferredRef
freshLiftedRecursiveLetRef context localName = do
  state0 <- get
  let (name, nextIndex) = pickName (lsNextHelperIndex state0)
      (ref, generator') = freshDeferredRef name (lsIdentityGenerator state0)
  modify
    ( \state1 ->
        state1
          { lsNextHelperIndex = nextIndex,
            lsGeneratedHelperNames = Set.insert name (lsGeneratedHelperNames state1),
            lsIdentityGenerator = generator'
          }
    )
  pure ref
  where
    pickName index0 =
      let candidate =
            ccCurrentBindingName context
              ++ "$letrec$"
              ++ localName
              ++ "$"
              ++ show index0
       in if Set.member candidate (globalTermRuntimeNames context)
            then pickName (index0 + 1)
            else (candidate, index0 + 1)

liftedRecursiveLetSymbol :: ConvertContext -> DeferredRef -> SymbolIdentity
liftedRecursiveLetSymbol context ref =
  symbolIdentityFromParts
    (deferredRefIdentity ref)
    SymbolValue
    (maybe "" symbolDefiningModule (ccCurrentModuleIdentity context))
    (deferredRefName ref)
    Nothing

emitLiftedRecursiveLet :: LiftedRecursiveLet -> LiftM ()
emitLiftedRecursiveLet lifted =
  modify
    ( \state0 ->
        state0
          { lsLiftedRecursiveLets = lsLiftedRecursiveLets state0 ++ [lifted]
          }
    )

liftEitherConversion :: Either BackendConversionError a -> LiftM a
liftEitherConversion result =
  StateT $ \state0 ->
    case result of
      Right value -> Right (value, state0)
      Left err -> Left err

throwLiftError :: BackendConversionError -> LiftM a
throwLiftError err =
  StateT (const (Left err))

isLiftableRecursiveFunctionType :: BackendType -> Bool
isLiftableRecursiveFunctionType ty =
  case ty of
    BTForall {} ->
      False
    _ ->
      let (args, resultTy) = splitBackendArrows ty
       in not (null args) && all isLiftableRecursiveValueType (resultTy : args)

isLiftableRecursiveValueType :: BackendType -> Bool
isLiftableRecursiveValueType =
  \case
    BTVar {} ->
      False
    ty@BTArrow {} ->
      isFirstOrderFunctionCaptureType ty
    BTBase {} ->
      True
    BTCon _ args ->
      all isLiftableRecursiveValueType args
    BTVarApp {} ->
      False
    BTForall {} ->
      False
    BTMu {} ->
      True
    BTBottom ->
      False

isFunctionValueTerm :: XmlfTerm -> Bool
isFunctionValueTerm term =
  case stripAdministrativeTermWrappers term of
    ELam {} -> True
    _ -> False

stripAdministrativeTermWrappers :: XmlfTerm -> XmlfTerm
stripAdministrativeTermWrappers =
  \case
    ETyAbsRef _ _ body -> stripAdministrativeTermWrappers body
    ETyInst inner _ -> stripAdministrativeTermWrappers inner
    ERoll _ body -> stripAdministrativeTermWrappers body
    term -> term

newtype TermVarKey
  = TermVarResolved ResolvedVar

termVarKeyReferenceName :: TermVarKey -> String
termVarKeyReferenceName =
  \case
    TermVarResolved resolved ->
      resolvedVarReferenceName resolved

termVarKeyMatchesReference :: TermVarKey -> ResolvedVar -> Bool
termVarKeyMatchesReference key resolved =
  case key of
    TermVarResolved expected ->
      resolvedVarSameIdentity expected resolved

termVarKeyMatchesLocalOccurrence :: TermVarKey -> ResolvedVar -> Bool
termVarKeyMatchesLocalOccurrence key resolved =
  case key of
    TermVarResolved expected ->
      resolvedVarSameIdentity expected resolved

freeResolvedTermVariables :: XmlfTerm -> [ResolvedVar]
freeResolvedTermVariables =
  go []
  where
    go bound =
      \case
        EVarNode resolved ->
          if resolvedVarBoundBy bound resolved
            then []
            else [resolved]
        ELit {} ->
          []
        ELam resolved body ->
          go (resolved : bound) body
        EApp fun arg ->
          go bound fun ++ go bound arg
        ELet resolved _ rhs body ->
          go bound rhs ++ go (resolved : bound) body
        ETyAbsRef _ _ body ->
          go bound body
        ETyInst inner _ ->
          go bound inner
        ERoll _ body ->
          go bound body
        EUnroll body ->
          go bound body

freeResolvedTermReferenceNames :: XmlfTerm -> Set.Set String
freeResolvedTermReferenceNames =
  Set.fromList . map resolvedVarReferenceName . freeResolvedTermVariables

freeElabTypeVarRefs :: Ty var -> [TypeBinderRef]
freeElabTypeVarRefs =
  freeElabTypeVarRefsIn []

freeElabTypeVarRefsIn :: [TypeBinderRef] -> Ty var -> [TypeBinderRef]
freeElabTypeVarRefsIn initialBound =
  go initialBound
  where
    go :: [TypeBinderRef] -> Ty v -> [TypeBinderRef]
    go bound =
      \case
        TVarRef ref
          | typeRefBoundBy ref bound -> []
          | otherwise -> [ref]
        TArrow dom cod ->
          go bound dom `unionTypeRefs` go bound cod
        TCon _ args ->
          unionsTypeRefs (map (go bound) (NE.toList args))
        TVarAppRef ref args ->
          let headFree =
                if typeRefBoundBy ref bound
                  then []
                  else [ref]
           in headFree `unionTypeRefs` unionsTypeRefs (map (go bound) (NE.toList args))
        TBase {} ->
          []
        TForallRef ref mb body ->
          maybe [] (go bound) mb
            `unionTypeRefs` go (insertTypeRef ref bound) body
        TMuRef ref body ->
          go (insertTypeRef ref bound) body
        TBottom ->
          []

freeXmlfTermTypeVarRefs :: XmlfTerm -> [TypeBinderRef]
freeXmlfTermTypeVarRefs =
  go []
  where
    go bound =
      \case
        EVarNode resolved ->
          freeElabTypeVarRefsIn bound (resolvedVarType resolved)
        ELit {} ->
          []
        ELam resolved body ->
          freeElabTypeVarRefsIn bound (resolvedVarType resolved) `unionTypeRefs` go bound body
        EApp fun arg ->
          go bound fun `unionTypeRefs` go bound arg
        ELet resolved scheme rhs body ->
          unionsTypeRefs
            [ freeElabTypeVarRefsIn bound (resolvedVarType resolved),
              freeElabTypeVarRefsIn bound (schemeToType scheme),
              go bound rhs,
              go bound body
            ]
        ETyAbsRef ref mbBound body ->
          maybe [] (freeElabTypeVarRefsIn bound) mbBound
            `unionTypeRefs` go (insertTypeRef ref bound) body
        ETyInst inner inst ->
          go bound inner `unionTypeRefs` freeInstantiationTypeVarRefsIn bound inst
        ERoll ty body ->
          freeElabTypeVarRefsIn bound ty `unionTypeRefs` go bound body
        EUnroll body ->
          go bound body

freeInstantiationTypeVarRefsIn :: [TypeBinderRef] -> Instantiation -> [TypeBinderRef]
freeInstantiationTypeVarRefsIn bound =
  \case
    InstId ->
      []
    InstApp ty ->
      freeElabTypeVarRefsIn bound ty
    InstBot ty ->
      freeElabTypeVarRefsIn bound ty
    InstIntro ->
      []
    InstElim ->
      []
    InstAbstrRef ref
      | typeRefBoundBy ref bound -> []
      | otherwise -> [ref]
    InstUnderRef ref inner ->
      freeInstantiationTypeVarRefsIn (insertTypeRef ref bound) inner
    InstInside inner ->
      freeInstantiationTypeVarRefsIn bound inner
    InstSeq left right ->
      freeInstantiationTypeVarRefsIn bound left `unionTypeRefs` freeInstantiationTypeVarRefsIn bound right

sortTypeBinderRefsByIdentity :: [TypeBinderRef] -> [TypeBinderRef]
sortTypeBinderRefsByIdentity =
  sortOn typeBinderRefIdentity

unionsTypeRefs :: [[TypeBinderRef]] -> [TypeBinderRef]
unionsTypeRefs =
  foldr unionTypeRefs []

unionTypeRefs :: [TypeBinderRef] -> [TypeBinderRef] -> [TypeBinderRef]
unionTypeRefs left right =
  foldr insertTypeRef right left

insertTypeRef :: TypeBinderRef -> [TypeBinderRef] -> [TypeBinderRef]
insertTypeRef ref refs
  | typeRefMember ref refs = refs
  | otherwise = ref : refs

typeRefMember :: TypeBinderRef -> [TypeBinderRef] -> Bool
typeRefMember ref =
  any (typeBinderRefsSameIdentity ref)

typeRefBoundBy :: TypeBinderRef -> [TypeBinderRef] -> Bool
typeRefBoundBy ref =
  any (`typeRefMayBeBoundBy` ref)

typeRefMayBeBoundBy :: TypeBinderRef -> TypeBinderRef -> Bool
typeRefMayBeBoundBy binder ref =
  typeBinderRefsSameIdentity binder ref

termVariableNames :: XmlfTerm -> Set.Set String
termVariableNames =
  \case
    EVarNode resolved ->
      Set.singleton (resolvedVarReferenceName resolved)
    ELam resolved body ->
      Set.insert (resolvedVarReferenceName resolved) (termVariableNames body)
    ELit {} ->
      Set.empty
    EApp fun arg ->
      termVariableNames fun `Set.union` termVariableNames arg
    ELet resolved _ rhs body ->
      Set.insert (resolvedVarReferenceName resolved) (termVariableNames rhs `Set.union` termVariableNames body)
    ETyAbsRef _ _ body ->
      termVariableNames body
    ETyInst inner _ ->
      termVariableNames inner
    ERoll _ body ->
      termVariableNames body
    EUnroll body ->
      termVariableNames body

elabTermTypeVariableNames :: XmlfTerm -> Set.Set String
elabTermTypeVariableNames =
  \case
    EVarNode resolved ->
      elabTypeVariableNames (resolvedVarType resolved)
    ELit {} ->
      Set.empty
    ELam resolved body ->
      elabTypeVariableNames (resolvedVarType resolved) `Set.union` elabTermTypeVariableNames body
    EApp fun arg ->
      elabTermTypeVariableNames fun `Set.union` elabTermTypeVariableNames arg
    ELet resolved scheme rhs body ->
      Set.unions
        [ elabTypeVariableNames (resolvedVarType resolved),
          elabTypeVariableNames (schemeToType scheme),
          elabTermTypeVariableNames rhs,
          elabTermTypeVariableNames body
        ]
    ETyAbsRef ref mbBound body ->
      Set.insert (typeBinderRefName ref) $
        maybe Set.empty (elabTypeVariableNames . tyToElab) mbBound
          `Set.union` elabTermTypeVariableNames body
    ETyInst inner inst ->
      elabTermTypeVariableNames inner `Set.union` instantiationTypeVariableNames inst
    ERoll ty body ->
      elabTypeVariableNames ty `Set.union` elabTermTypeVariableNames body
    EUnroll body ->
      elabTermTypeVariableNames body

elabTypeVariableNames :: ElabType -> Set.Set String
elabTypeVariableNames =
  \case
    TVarRef ref ->
      Set.singleton (typeBinderRefName ref)
    TArrow dom cod ->
      elabTypeVariableNames dom `Set.union` elabTypeVariableNames cod
    TCon _ args ->
      Set.unions (map elabTypeVariableNames (NE.toList args))
    TVarAppRef ref args ->
      Set.insert (typeBinderRefName ref) (Set.unions (map elabTypeVariableNames (NE.toList args)))
    TBase {} ->
      Set.empty
    TForallRef ref mbBound body ->
      Set.insert (typeBinderRefName ref) $
        maybe Set.empty (elabTypeVariableNames . tyToElab) mbBound
          `Set.union` elabTypeVariableNames body
    TMuRef ref body ->
      Set.insert (typeBinderRefName ref) (elabTypeVariableNames body)
    TBottom ->
      Set.empty

instantiationTypeVariableNames :: Instantiation -> Set.Set String
instantiationTypeVariableNames =
  \case
    InstId ->
      Set.empty
    InstApp ty ->
      elabTypeVariableNames ty
    InstBot ty ->
      elabTypeVariableNames ty
    InstIntro ->
      Set.empty
    InstElim ->
      Set.empty
    InstAbstrRef ref ->
      Set.singleton (typeBinderRefName ref)
    InstUnderRef ref inner ->
      Set.insert (typeBinderRefName ref) (instantiationTypeVariableNames inner)
    InstInside inner ->
      instantiationTypeVariableNames inner
    InstSeq left right ->
      instantiationTypeVariableNames left `Set.union` instantiationTypeVariableNames right

replaceFreeTermVariable :: TermVarKey -> XmlfTerm -> XmlfTerm -> XmlfTerm
replaceFreeTermVariable needle replacement =
  go
  where
    needleName = termVarKeyReferenceName needle
    replacementFreeTermKeys =
      Set.fromList (map resolvedVarIdentityKey (freeResolvedTermVariables replacement))
    replacementFreeTypes = freeXmlfTermTypeVarRefs replacement

    go =
      \case
        EVarNode resolved
          | termVarKeyMatchesLocalOccurrence needle resolved ->
              replacement
          | otherwise -> EVarNode resolved
        ELit lit ->
          ELit lit
        ELam resolved body
          | termVarKeyMatchesReference needle resolved ->
              ELam resolved body
          | shouldRenameTermBinder resolved body ->
              let used = Set.unions [termVariableNames body, termVariableNames replacement, Set.singleton needleName]
                  binderName' = freshNameLike binderName used
                  (resolved', _) = freshenResolvedLocalVar binderName' (identityGeneratorAfterTerm (EApp replacement body)) resolved
                  body' = replaceBoundTermVariable (TermVarResolved resolved) resolved' body
               in ELam resolved' (go body')
          | otherwise ->
              ELam resolved (go body)
          where
            binderName = resolvedVarReferenceName resolved
        EApp fun arg ->
          EApp (go fun) (go arg)
        ELet resolved scheme rhs body
          | termVarKeyMatchesReference needle resolved ->
              ELet resolved scheme (go rhs) body
          | shouldRenameTermBinder resolved body ->
              let used =
                    Set.unions
                      [ termVariableNames rhs,
                        termVariableNames body,
                        termVariableNames replacement,
                        Set.singleton needleName
                      ]
                  binderName' = freshNameLike binderName used
                  (resolved', _) = freshenResolvedLocalVar binderName' (identityGeneratorAfterTerm (EApp replacement (ELet resolved scheme rhs body))) resolved
                  body' = replaceBoundTermVariable (TermVarResolved resolved) resolved' body
               in ELet resolved' scheme (go rhs) (go body')
          | otherwise ->
              ELet resolved scheme (go rhs) (go body)
          where
            binderName = resolvedVarReferenceName resolved
        ETyAbsRef ref mbBound body
          | shouldRenameTypeBinder ref body ->
              let used =
                    Set.unions
                      [ elabTermTypeVariableNames body,
                        maybe Set.empty (elabTypeVariableNames . tyToElab) mbBound,
                        elabTermTypeVariableNames replacement
                      ]
                  name' = freshNameLike name used
                  ref' = renameTypeBinderRef name' ref
                  body' = renameTermTypeVariable ref ref' body
               in ETyAbsRef ref' mbBound (go body')
          | otherwise ->
              ETyAbsRef ref mbBound (go body)
          where
            name = typeBinderRefName ref
        ETyInst inner inst ->
          ETyInst (go inner) inst
        ERoll ty body ->
          ERoll ty (go body)
        EUnroll body ->
          EUnroll (go body)

    shouldRenameTermBinder resolved body =
      Set.member (resolvedVarIdentityKey resolved) replacementFreeTermKeys && termMentionsFreeVariable needle body

    shouldRenameTypeBinder ref body =
      typeRefMember ref replacementFreeTypes && termMentionsFreeVariable needle body

renameBoundTermVariable :: TermVarKey -> String -> XmlfTerm -> XmlfTerm
renameBoundTermVariable old new =
  mapBoundTermVariable old (renameResolvedLocalVar new)

replaceBoundTermVariable :: TermVarKey -> ResolvedVar -> XmlfTerm -> XmlfTerm
replaceBoundTermVariable old new =
  mapBoundTermVariable old (const new)

mapBoundTermVariable :: TermVarKey -> (ResolvedVar -> ResolvedVar) -> XmlfTerm -> XmlfTerm
mapBoundTermVariable old rewrite =
  go
  where
    go =
      \case
        EVarNode resolved
          | termVarKeyMatchesLocalOccurrence old resolved ->
              EVarNode (rewrite resolved)
          | otherwise -> EVarNode resolved
        ELit lit ->
          ELit lit
        ELam resolved body
          | termVarKeyMatchesReference old resolved -> ELam resolved body
          | otherwise -> ELam resolved (go body)
        EApp fun arg ->
          EApp (go fun) (go arg)
        ELet resolved scheme rhs body
          | termVarKeyMatchesReference old resolved -> ELet resolved scheme (go rhs) body
          | otherwise -> ELet resolved scheme (go rhs) (go body)
        ETyAbsRef ref mbBound body ->
          ETyAbsRef ref mbBound (go body)
        ETyInst inner inst ->
          ETyInst (go inner) inst
        ERoll ty body ->
          ERoll ty (go body)
        EUnroll body ->
          EUnroll (go body)

renameTermTypeVariable :: TypeBinderRef -> TypeBinderRef -> XmlfTerm -> XmlfTerm
renameTermTypeVariable oldRef newRef =
  go
  where
    go =
      \case
        EVarNode resolved ->
          EVarNode (mapResolvedVarType (renameElabTypeVariable oldRef newRef) resolved)
        ELit lit ->
          ELit lit
        ELam resolved body ->
          ELam (mapResolvedVarType (renameElabTypeVariable oldRef newRef) resolved) (go body)
        EApp fun arg ->
          EApp (go fun) (go arg)
        ELet resolved scheme rhs body ->
          ELet
            (mapResolvedVarType (renameElabTypeVariable oldRef newRef) resolved)
            (renameElabSchemeTypeVariable oldRef newRef scheme)
            (go rhs)
            (go body)
        ETyAbsRef ref mbBound body
          | typeBinderRefsSameIdentity ref oldRef ->
              ETyAbsRef ref (fmap (renameElabTypeVariable oldRef newRef) mbBound) body
          | otherwise ->
              ETyAbsRef ref (fmap (renameElabTypeVariable oldRef newRef) mbBound) (go body)
        ETyInst inner inst ->
          ETyInst (go inner) (renameInstantiationTypeVariable oldRef newRef inst)
        ERoll ty body ->
          ERoll (renameElabTypeVariable oldRef newRef ty) (go body)
        EUnroll body ->
          EUnroll (go body)

renameElabSchemeTypeVariable :: TypeBinderRef -> TypeBinderRef -> ElabScheme -> ElabScheme
renameElabSchemeTypeVariable oldRef newRef =
  schemeFromType . renameElabTypeVariable oldRef newRef . schemeToType

renameElabTypeVariable :: TypeBinderRef -> TypeBinderRef -> Ty var -> Ty var
renameElabTypeVariable oldRef newRef =
  \case
    TVarRef ref
      | typeBinderRefsSameIdentity ref oldRef -> TVarRef newRef
      | otherwise -> TVarRef ref
    TArrow dom cod ->
      TArrow (renameElabTypeVariable oldRef newRef dom) (renameElabTypeVariable oldRef newRef cod)
    TConWithIdentity identity con args ->
      TConWithIdentity identity con (fmap (renameElabTypeVariable oldRef newRef) args)
    TVarAppRef ref args ->
      TVarAppRef
        (if typeBinderRefsSameIdentity ref oldRef then newRef else ref)
        (fmap (renameElabTypeVariable oldRef newRef) args)
    TBaseWithIdentity identity base ->
      TBaseWithIdentity identity base
    TForallRef ref mbBound body
      | typeBinderRefsSameIdentity ref oldRef ->
          TForallRef ref (fmap (renameElabTypeVariable oldRef newRef) mbBound) body
      | otherwise ->
          TForallRef ref (fmap (renameElabTypeVariable oldRef newRef) mbBound) (renameElabTypeVariable oldRef newRef body)
    TMuRef ref body
      | typeBinderRefsSameIdentity ref oldRef -> TMuRef ref body
      | otherwise -> TMuRef ref (renameElabTypeVariable oldRef newRef body)
    TBottom ->
      TBottom

renameInstantiationTypeVariable :: TypeBinderRef -> TypeBinderRef -> Instantiation -> Instantiation
renameInstantiationTypeVariable oldRef newRef =
  go
  where
    go =
      \case
        InstId ->
          InstId
        InstApp ty ->
          InstApp (renameElabTypeVariable oldRef newRef ty)
        InstBot ty ->
          InstBot (renameElabTypeVariable oldRef newRef ty)
        InstIntro ->
          InstIntro
        InstElim ->
          InstElim
        InstAbstrRef ref
          | typeBinderRefsSameIdentity ref oldRef -> InstAbstrRef newRef
          | otherwise -> InstAbstrRef ref
        InstUnderRef ref inner
          | typeBinderRefsSameIdentity ref oldRef -> InstUnderRef ref inner
          | otherwise -> InstUnderRef ref (go inner)
        InstInside inner ->
          InstInside (go inner)
        InstSeq left right ->
          InstSeq (go left) (go right)

checkedBindingCanonicalTypeOpen :: ConvertContext -> CheckedModule -> CheckedBinding -> Either BackendConversionError ElabType
checkedBindingCanonicalTypeOpen context checkedModule binding = do
  let checkedTy = normalizeBuiltinElabType (checkedBindingType binding)
      scope = scopeForModule context (checkedModuleIdentity checkedModule)
      sourceView = checkedBindingSourceTypeView binding
      sourceViewWithScopeHeads =
        sourceView
          { typeViewHeadIdentities =
              mergeSymbolIdentityMaps
                [ typeViewHeadIdentities sourceView,
                  typeHeadIdentitiesInScope scope
                ]
          }
      loweredSourceView = lowerTypeViewForScope scope sourceView
      sourceCandidates =
        nub $
          if typeViewMentionsPreludePrimitiveData (ccDataByIdentity context) sourceViewWithScopeHeads
            then [sourceView, loweredSourceView]
            else [loweredSourceView]
  checkedBackendTy <- convertElabType checkedTy
  pure
    ( maybe
        checkedTy
        id
        (foldr (<|>) Nothing (map (canonicalCandidate checkedTy checkedBackendTy) sourceCandidates))
    )
  where
    canonicalCandidate checkedTy checkedBackendTy sourceCandidate = do
      canonicalTy0 <-
        case sourceTypeViewToElabTypeWithGenerator (identityGeneratorAfterType checkedTy) sourceCandidate of
          Right ty -> Just ty
          Left _ -> Nothing
      let canonicalTy = normalizeBuiltinElabType canonicalTy0
      canonicalBackendTy0 <- either (const Nothing) Just (convertElabType canonicalTy)
      let canonicalBackendTy = canonicalizeBackendType context canonicalBackendTy0
          strippedCheckedBackendTy = stripVacuousBackendForalls checkedBackendTy
          preferCanonicalTy = preludePrimitiveBackendTypeHead context canonicalBackendTy0
      if alphaEqBackendType checkedBackendTy canonicalBackendTy
        then Just canonicalTy
        else
          if preferCanonicalTy && backendTypesCompatible context checkedBackendTy canonicalBackendTy
            then Just canonicalTy
            else
              if alphaEqBackendType (normalizeBuiltinBackendType strippedCheckedBackendTy) (normalizeBuiltinBackendType canonicalBackendTy)
                then backendTypeToElabTypeSeededByElabType checkedTy strippedCheckedBackendTy
                else
                  case (checkedBackendTy, canonicalBackendTy) of
                    (BTVarWithIdentity checkedIdentity checkedName, BTVarWithIdentity canonicalIdentity canonicalName)
                      | typeBinderRefMatches checkedIdentity checkedName canonicalIdentity canonicalName ->
                          Just checkedTy
                    _ -> Nothing

stripVacuousBackendForalls :: BackendType -> BackendType
stripVacuousBackendForalls =
  \case
    BTArrow dom cod ->
      BTArrow (stripVacuousBackendForalls dom) (stripVacuousBackendForalls cod)
    BTConWithIdentity identity con args ->
      BTConWithIdentity identity con (fmap stripVacuousBackendForalls args)
    BTVarAppWithIdentity identity name args ->
      BTVarAppWithIdentity identity name (fmap stripVacuousBackendForalls args)
    BTForallWithIdentity identity name mbBound body ->
      let body' = stripVacuousBackendForalls body
          mbBound' = fmap stripVacuousBackendForalls mbBound
       in if Set.member (backendTypeSubstitutionKeyFor identity name) (freeBackendTypeVarKeys body')
            then BTForallWithIdentity identity name mbBound' body'
            else body'
    BTMuWithIdentity identity name body ->
      BTMuWithIdentity identity name (stripVacuousBackendForalls body)
    ty ->
      ty

normalizeBuiltinBackendType :: BackendType -> BackendType
normalizeBuiltinBackendType =
  \case
    BTArrow dom cod ->
      BTArrow (normalizeBuiltinBackendType dom) (normalizeBuiltinBackendType cod)
    BTBaseWithIdentity identity base ->
      BTBaseWithIdentity identity (normalizeBuiltinBase base)
    BTConWithIdentity identity con args ->
      BTConWithIdentity identity (normalizeBuiltinBase con) (fmap normalizeBuiltinBackendType args)
    BTVarAppWithIdentity identity name args ->
      BTVarAppWithIdentity identity name (fmap normalizeBuiltinBackendType args)
    BTForallWithIdentity identity name mbBound body ->
      BTForallWithIdentity identity name (fmap normalizeBuiltinBackendType mbBound) (normalizeBuiltinBackendType body)
    BTMuWithIdentity identity name body ->
      BTMuWithIdentity identity name (normalizeBuiltinBackendType body)
    ty ->
      ty

normalizeBuiltinBase :: BaseTy -> BaseTy
normalizeBuiltinBase (BaseTy name) =
  BaseTy (normalizeBuiltinTypeReference name)

quantifyFreeElabTypeVarRefs :: [TypeBinderRef] -> ElabType -> ElabType
quantifyFreeElabTypeVarRefs refs ty =
  foldr (`TForallRef` Nothing) ty refs

wrapElabTypeAbsRefs :: [TypeBinderRef] -> XmlfTerm -> XmlfTerm
wrapElabTypeAbsRefs refs term =
  foldr (\ref acc -> ETyAbsRef ref Nothing acc) term refs

alignLeadingTypeAbsRefsToType :: ElabType -> XmlfTerm -> XmlfTerm
alignLeadingTypeAbsRefsToType expectedTy term =
  case (expectedTy, term) of
    (TForallRef targetRef _ targetBody, ETyAbsRef termRef mbBound body)
      | typeBinderRefsSameIdentity targetRef termRef ->
          ETyAbsRef termRef mbBound (alignLeadingTypeAbsRefsToType targetBody body)
      | otherwise ->
          ETyAbsRef
            targetRef
            mbBound
            (alignLeadingTypeAbsRefsToType targetBody (renameTermTypeVariable termRef targetRef body))
    _ -> term

normalizeBackendTypeForContext :: ConvertContext -> BackendType -> BackendType
normalizeBackendTypeForContext context ty =
  let canonicalTy = canonicalizeStructuralMuNames context (canonicalizeBackendType context ty)
   in if backendTypeNeedsStructuralRecovery context canonicalTy
        then recoverStructuralBackendType context canonicalTy
        else canonicalTy

scopeForModule :: ConvertContext -> SymbolIdentity -> ElaborateScope
scopeForModule context moduleIdentity =
  Map.findWithDefault
    (fallbackElaborateScope (map dmInfo (ccData context)))
    moduleIdentity
    (ccModuleScopes context)

fallbackElaborateScope :: [DataInfo] -> ElaborateScope
fallbackElaborateScope dataInfos =
  mkElaborateScope Map.empty (qualifiedDataInfoMap dataInfos) Map.empty []

sourceTypeViewToElabTypeWithGenerator :: IdentityGenerator -> TypeView -> Either BackendConversionError ElabType
sourceTypeViewToElabTypeWithGenerator generator view =
  case convertSourceTypeViewWithIdentities view of
    Left err -> Left err
    Right backendTy ->
      case backendTypeToElabTypeWithGenerator (advanceIdentityGeneratorPastMany (typeViewGeneratedIdentities view) generator) backendTy of
        Just ty -> Right ty
        Nothing -> Left (BackendUnsupportedCaseShape "source type view did not convert to elaborated type")

typeViewMentionsPreludePrimitiveData :: Map SymbolIdentity DataMeta -> TypeView -> Bool
typeViewMentionsPreludePrimitiveData dataMetasByIdentity view =
  any mentionsPreludePrimitiveData (Set.toList (typeViewMentionedHeadIdentities view))
  where
    mentionsPreludePrimitiveData identity =
      case Map.lookup identity dataMetasByIdentity of
        Just dataMeta -> preludePrimitiveDataMeta dataMeta
        Nothing -> False

constructorBindingResultMatches :: BackendType -> ConstructorMeta -> Bool
constructorBindingResultMatches bindingTy constructorMeta =
  case matchBackendTypeParametersWithDataIdentity dataIdentity Map.empty dataParameters parameters Map.empty (backendConstructorResult constructor) resultTy of
    Just _ -> True
    Nothing -> False
  where
    constructor = cmBackend constructorMeta
    dataIdentity = backendDataIdentity (dmBackend (cmData constructorMeta))
    dataParameters = constructorDataParameterRefs constructorMeta
    parameters = constructorTypeParameters constructorMeta
    (_, bodyTy) = splitBackendForalls bindingTy
    (_, resultTy) = splitBackendArrows bodyTy

synthesizeConstructorBinding :: BackendType -> ConstructorMeta -> ConvertM BackendExpr
synthesizeConstructorBinding bindingTy constructorMeta = do
  let constructor = cmBackend constructorMeta
      (typeBinders, bodyTy) = splitBackendForalls bindingTy
      (argTys, resultTy) = splitBackendArrows bodyTy
      fields = backendConstructorFields constructor
  when (length argTys /= length fields) $
    liftEitherConvert $
      Left
        ( BackendUnsupportedCaseShape
            ("constructor binding arity does not match metadata for `" ++ backendConstructorName constructor ++ "`")
        )
  let argNames = ["$" ++ backendConstructorName constructor ++ "_arg" ++ show ix | ix <- [1 .. length argTys]]
  argIdentities <- traverse freshBackendLocalDetails argNames
  let args = zip3 argNames argTys argIdentities
      argExprs =
        [ BackendVarWithIdentity
            { backendExprType = argTy,
              backendVarIdentity = Just identity,
              backendVarName = name
            }
        | (name, argTy, identity) <- args
        ]
      constructExpr =
        BackendConstructWithIdentity
          { backendExprType = resultTy,
            backendConstructIdentity = backendConstructorIdentity constructor,
            backendConstructName = backendConstructorName constructor,
            backendConstructArgs = argExprs
          }
      expr =
        wrapBackendTypeAbs typeBinders $
          wrapBackendLams args constructExpr
  unless (alphaEqBackendType (backendExprType expr) bindingTy) $
    liftEitherConvert $
      Left
        ( BackendUnsupportedCaseShape
            ("synthesized constructor binding type does not match checked binding type for `" ++ backendConstructorName constructor ++ "`")
        )
  pure expr

constructorBackendBindingType :: ConstructorMeta -> BackendType
constructorBackendBindingType constructorMeta =
  foldr wrapForall body binders
  where
    constructor = cmBackend constructorMeta
    body =
      foldr BTArrow (backendConstructorResult constructor) (backendConstructorFields constructor)
    binders =
      [ BackendTypeBinderWithIdentity (backendDataParameterRefIdentity ref) (backendDataParameterRefName ref) Nothing
        | ref <- backendDataParameterRefs dataDecl
      ]
        ++ backendConstructorForalls constructor
    dataDecl =
      dmBackend (cmData constructorMeta)

    wrapForall binder bodyTy =
      BTForallWithIdentity
        (backendTypeBinderIdentity binder)
        (backendTypeBinderName binder)
        (backendTypeBinderBound binder)
        bodyTy

splitBackendForalls :: BackendType -> ([BackendTypeAbsBinder], BackendType)
splitBackendForalls =
  go []
  where
    go binders ty =
      case ty of
        BTForallWithIdentity identity name mbBound body -> go (binders ++ [BackendTypeAbsBinder identity name mbBound]) body
        _ -> (binders, ty)

splitBackendArrows :: BackendType -> ([BackendType], BackendType)
splitBackendArrows =
  go []
  where
    go args ty =
      case ty of
        BTArrow arg result -> go (args ++ [arg]) result
        _ -> (args, ty)

wrapBackendTypeAbs :: [BackendTypeAbsBinder] -> BackendExpr -> BackendExpr
wrapBackendTypeAbs binders body =
  foldr wrap body binders
  where
    wrap (BackendTypeAbsBinder identity name mbBound) expr =
      BackendTyAbsWithIdentity
        { backendExprType = BTForallWithIdentity identity name mbBound (backendExprType expr),
          backendTyParamIdentity = identity,
          backendTyParamName = name,
          backendTyParamBound = mbBound,
          backendTyAbsBody = expr
        }

wrapBackendLams :: [(String, BackendType, IdDetails)] -> BackendExpr -> BackendExpr
wrapBackendLams params body =
  foldr wrap body params
  where
    wrap (name, paramTy, identity) expr =
      BackendLamWithIdentity
        { backendExprType = BTArrow paramTy (backendExprType expr),
          backendParamIdentity = Just identity,
          backendParamName = name,
          backendParamType = paramTy,
          backendBody = expr
        }

buildConvertContext :: CheckedProgram -> Either BackendConversionError ConvertContext
buildConvertContext checked = do
  let dataInfos = allDataInfos checked
  _modulesByIdentity <- uniqueModulesByIdentity checked
  _bindingsByIdentity <- uniqueBindingsByIdentity checked
  dataByIdentity <- uniqueDataInfosByIdentity dataInfos
  termRuntimeNames <- checkedProgramTermRuntimeNamesByIdentity checked
  let dataModuleIdentities = dataInfoModuleIdentityMap checked
      moduleScopes = moduleElaborateScopes checked dataByIdentity
  dataMetas <- mapM (buildDataMetaForDataInfo moduleScopes dataModuleIdentities dataInfos) dataInfos
  constructorMetasByIdentity <-
    uniqueConstructorMetasByIdentity
      [ constructorMeta
        | dataMeta <- dataMetas,
          constructorMeta <- constructorMetasForData dataMeta
      ]
  let dataMetasByIdentity =
        Map.fromList
          [ (dataInfoSymbol (dmInfo dataMeta), dataMeta)
          | dataMeta <- dataMetas
          ]
      bindingData = bindingDataHints dataMetasByIdentity checked
  let context0 =
        ConvertContext
          { ccModuleScopes = moduleScopes,
            ccConstructorsByIdentity = constructorMetasByIdentity,
            ccTermRuntimeNamesByIdentity = termRuntimeNames,
            ccBindingData = bindingData,
            ccDataByIdentity = dataMetasByIdentity,
            ccDataModuleIdentities = dataModuleIdentities,
            ccData = dataMetas,
            ccClosureGlobalsByIdentity = Set.empty,
            ccClosureValueArgumentsByIdentity = builtinClosureValueArguments,
            ccEvidenceValueArgumentsByIdentity = Map.empty,
            ccClosureValueArgumentsByDeferred = Map.empty,
            ccEvidenceValueArgumentsByDeferred = Map.empty,
            ccEvidenceResolvedVarKeys = Set.empty,
            ccIdentityGenerator = checkedProgramIdentityGenerator checked,
            ccCurrentModuleIdentity = Nothing,
            ccCurrentBindingName = ""
          }
  evidenceResolvedVars <- checkedProgramEvidenceResolvedVars context0 checked
  let contextWithEvidence =
        context0
          { ccEvidenceResolvedVarKeys = Set.fromList (map resolvedVarIdentityKey evidenceResolvedVars)
          }
  evidenceValueArguments <- checkedProgramEvidenceValueArguments contextWithEvidence checked
  closureValueArguments <- checkedProgramClosureValueArguments contextWithEvidence checked
  Right
    contextWithEvidence
      { ccClosureValueArgumentsByIdentity = closureValueArguments,
        ccEvidenceValueArgumentsByIdentity = evidenceValueArguments
      }

uniqueModulesByIdentity :: CheckedProgram -> Either BackendConversionError (Map SymbolIdentity CheckedModule)
uniqueModulesByIdentity checked =
  uniqueCheckedInfoByIdentity BackendDuplicateModule checkedModuleIdentity (checkedProgramModules checked)

uniqueBindingsByIdentity :: CheckedProgram -> Either BackendConversionError (Map SymbolIdentity CheckedBinding)
uniqueBindingsByIdentity checked =
  Map.map snd
    <$> uniqueCheckedInfoByIdentity BackendDuplicateBinding fst
      [ (symbol, binding)
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule,
        Just symbol <- [checkedBindingSymbolIdentity binding]
      ]

uniqueDataInfosByIdentity :: [DataInfo] -> Either BackendConversionError (Map SymbolIdentity DataInfo)
uniqueDataInfosByIdentity =
  uniqueCheckedInfoByIdentity BackendDuplicateData dataInfoSymbol

uniqueConstructorMetasByIdentity :: [ConstructorMeta] -> Either BackendConversionError (Map SymbolIdentity ConstructorMeta)
uniqueConstructorMetasByIdentity =
  uniqueCheckedInfoByIdentity BackendDuplicateConstructor (ctorInfoSymbol . cmInfo)

uniqueCheckedInfoByIdentity ::
  (String -> BackendValidationError) ->
  (a -> SymbolIdentity) ->
  [a] ->
  Either BackendConversionError (Map SymbolIdentity a)
uniqueCheckedInfoByIdentity mkError identityFor =
  go Map.empty
  where
    go entries [] = Right entries
    go entries (info : rest) =
      let identity = identityFor info
       in case Map.lookup identity entries of
            Just _ ->
              Left (BackendValidationFailed (mkError (symbolIdentityStableName identity)))
            Nothing ->
              go (Map.insert identity info entries) rest

checkedProgramIdentityGenerator :: CheckedProgram -> IdentityGenerator
checkedProgramIdentityGenerator checked =
  identityGeneratorAfter (checkedProgramGeneratedIdentities checked)

checkedProgramGeneratedIdentities :: CheckedProgram -> [UniqueIdentity]
checkedProgramGeneratedIdentities checked =
  resolvedProgramGeneratedIdentities (resolvedProgramSemanticArtifact (checkedProgramResolved checked))
    ++ idDetailsGeneratedIdentities (resolvedVarDetails (checkedProgramMainResolvedVar checked))
    ++ concatMap checkedModuleGeneratedIdentities (checkedProgramModules checked)

checkedModuleGeneratedIdentities :: CheckedModule -> [UniqueIdentity]
checkedModuleGeneratedIdentities checkedModule =
  symbolGeneratedIdentities (checkedModuleIdentity checkedModule)
    ++ concatMap checkedBindingGeneratedIdentities (checkedModuleBindings checkedModule)
    ++ concatMap dataInfoGeneratedIdentities (Map.elems (checkedModuleData checkedModule))
    ++ concatMap classInfoGeneratedIdentities (Map.elems (checkedModuleClasses checkedModule))
    ++ concatMap instanceInfoGeneratedIdentities (checkedModuleInstances checkedModule)

checkedBindingGeneratedIdentities :: CheckedBinding -> [UniqueIdentity]
checkedBindingGeneratedIdentities binding =
  idDetailsGeneratedIdentities (resolvedVarDetails resolved)
    ++ generatedIdentitiesInType (resolvedVarType resolved)
    ++ typeViewGeneratedIdentities (checkedBindingSourceTypeView binding)
    ++ generatedIdentitiesInType (checkedBindingType binding)
    ++ generatedIdentitiesInTerm (checkedBindingTerm binding)
    ++ concatMap (idDetailsGeneratedIdentities . DeferredId) (Map.keys (checkedBindingDeferredObligations binding))
    ++ concatMap deferredProgramObligationGeneratedIdentities (Map.elems (checkedBindingDeferredObligations binding))
  where
    resolved =
      checkedBindingResolvedVar binding

dataInfoGeneratedIdentities :: DataInfo -> [UniqueIdentity]
dataInfoGeneratedIdentities info =
  symbolGeneratedIdentities (dataInfoSymbol info)
    ++ concatMap typeParamGeneratedIdentities (dataTypeParams info)
    ++ concatMap constructorInfoGeneratedIdentities (dataConstructors info)

constructorInfoGeneratedIdentities :: ConstructorInfo -> [UniqueIdentity]
constructorInfoGeneratedIdentities ctorInfo =
  symbolGeneratedIdentities (ctorInfoSymbol ctorInfo)
    ++ symbolGeneratedIdentities (ctorOwningTypeIdentity ctorInfo)
    ++ typeViewGeneratedIdentities (ctorTypeView ctorInfo)
    ++ concatMap constructorForallBinderGeneratedIdentities (ctorForallBinderInfo ctorInfo)
    ++ concatMap constructorShapeGeneratedIdentities (ctorOwnerConstructors ctorInfo)

constructorShapeGeneratedIdentities :: ConstructorShape -> [UniqueIdentity]
constructorShapeGeneratedIdentities shape =
  symbolGeneratedIdentities (constructorShapeSymbol shape)
    ++ typeViewGeneratedIdentities (constructorShapeTypeView shape)
    ++ concatMap constructorForallBinderGeneratedIdentities (constructorShapeForallBinderInfo shape)
    ++ concatMap typeParamGeneratedIdentities (constructorShapeOwnerTypeParams shape)

constructorForallBinderGeneratedIdentities :: ConstructorForallBinder -> [UniqueIdentity]
constructorForallBinderGeneratedIdentities binder =
  typeBinderIdentityGeneratedIdentities (Just (constructorForallIdentity binder))

classInfoGeneratedIdentities :: ClassInfo -> [UniqueIdentity]
classInfoGeneratedIdentities info =
  symbolGeneratedIdentities (classInfoSymbol info)
    ++ foldMap typeParamGeneratedIdentities (classTypeParams info)
    ++ concatMap constraintInfoGeneratedIdentities (classSuperclassInfos info)
    ++ concatMap functionalDependencyGeneratedIdentities (classFunctionalDependencies info)
    ++ concatMap methodInfoGeneratedIdentities (Map.elems (classMethodsByIdentity info))

functionalDependencyGeneratedIdentities :: FunctionalDependencyInfo -> [UniqueIdentity]
functionalDependencyGeneratedIdentities info =
  foldMap (typeBinderIdentityGeneratedIdentities . Just) (functionalDependencyDeterminerRefs info)
    ++ foldMap (typeBinderIdentityGeneratedIdentities . Just) (functionalDependencyDeterminedRefs info)

instanceInfoGeneratedIdentities :: InstanceInfo -> [UniqueIdentity]
instanceInfoGeneratedIdentities info =
  symbolGeneratedIdentities (instanceClassSymbol info)
    ++ symbolGeneratedIdentities (instanceOriginModuleIdentity info)
    ++ concatMap constraintInfoGeneratedIdentities (instanceConstraintInfos info)
    ++ foldMap typeViewGeneratedIdentities (instanceHeadTypeViews info)
    ++ concatMap valueInfoGeneratedIdentities (Map.elems (instanceMethodsByIdentity info))

methodInfoGeneratedIdentities :: MethodInfo -> [UniqueIdentity]
methodInfoGeneratedIdentities info =
  symbolGeneratedIdentities (methodInfoSymbol info)
    ++ typeViewGeneratedIdentities (methodTypeViewRaw info)
    ++ concatMap constraintInfoGeneratedIdentities (methodConstraintInfos info)
    ++ foldMap (typeBinderIdentityGeneratedIdentities . Just) (methodParamBinderIdentities info)

valueInfoGeneratedIdentities :: ValueInfo -> [UniqueIdentity]
valueInfoGeneratedIdentities valueInfo =
  case valueInfo of
    OrdinaryValue {valueInfoSymbol = symbol, valueConstraintInfos = constraints} ->
      symbolGeneratedIdentities symbol
        ++ typeViewGeneratedIdentities (ordinaryValueTypeView valueInfo)
        ++ concatMap constraintInfoGeneratedIdentities constraints
    ConstructorValue {valueInfoSymbol = symbol, valueCtorInfo = ctorInfo} ->
      symbolGeneratedIdentities symbol ++ constructorInfoGeneratedIdentities ctorInfo
    OverloadedMethod {valueInfoSymbol = symbol, valueMethodInfo = methodInfo} ->
      symbolGeneratedIdentities symbol ++ methodInfoGeneratedIdentities methodInfo

constraintInfoGeneratedIdentities :: ConstraintInfo -> [UniqueIdentity]
constraintInfoGeneratedIdentities info =
  symbolGeneratedIdentities (constraintClassSymbol info)
    ++ foldMap typeViewGeneratedIdentities (constraintTypeViews info)

evidenceInfoGeneratedIdentities :: EvidenceInfo -> [UniqueIdentity]
evidenceInfoGeneratedIdentities info =
  symbolGeneratedIdentities (evidenceClassSymbol info)
    ++ foldMap typeViewGeneratedIdentities (evidenceTypeViews info)
    ++ concatMap evidenceMethodGeneratedIdentities (Map.elems (evidenceMethodsByIdentity info))

evidenceMethodGeneratedIdentities :: EvidenceMethod -> [UniqueIdentity]
evidenceMethodGeneratedIdentities method =
  symbolGeneratedIdentities (evidenceMethodSymbol method)
    ++ maybe [] (idDetailsGeneratedIdentities . resolvedVarDetails) (evidenceMethodResolvedVar method)
    ++ typeViewGeneratedIdentities (evidenceMethodTypeView method)

deferredProgramObligationGeneratedIdentities :: DeferredProgramObligation -> [UniqueIdentity]
deferredProgramObligationGeneratedIdentities obligation =
  case obligation of
    DeferredMethod deferred ->
      idDetailsGeneratedIdentities (DeferredId (deferredMethodRef deferred))
        ++ methodInfoGeneratedIdentities (deferredMethodInfo deferred)
        ++ maybe [] typeViewGeneratedIdentities (deferredMethodExpectedResult deferred)
        ++ maybe [] deferredMethodEvidenceGeneratedIdentities (deferredMethodEvidence deferred)
        ++ concatMap evidenceInfoGeneratedIdentities (deferredMethodLocalEvidence deferred)
    DeferredConstructor deferred ->
      idDetailsGeneratedIdentities (DeferredId (deferredConstructorRef deferred))
        ++ constructorInfoGeneratedIdentities (deferredConstructorInfo deferred)
        ++ concatMap symbolGeneratedIdentities (Map.elems (deferredConstructorTypeHeadIdentities deferred))
        ++ concatMap (typeBinderIdentityGeneratedIdentities . Just . snd) (deferredConstructorInstBinders deferred)
    DeferredCase deferred ->
      idDetailsGeneratedIdentities (DeferredId (deferredCaseRef deferred))
        ++ dataInfoGeneratedIdentities (deferredCaseDataInfo deferred)

deferredMethodEvidenceGeneratedIdentities :: DeferredMethodEvidence -> [UniqueIdentity]
deferredMethodEvidenceGeneratedIdentities evidence =
  typeViewGeneratedIdentities (deferredMethodEvidenceClassArg evidence)
    ++ foldMap typeViewGeneratedIdentities (deferredMethodEvidenceClassArgs evidence)
    ++ evidenceMethodGeneratedIdentities (deferredMethodEvidenceMethod evidence)

typeViewGeneratedIdentities :: TypeView -> [UniqueIdentity]
typeViewGeneratedIdentities view =
  concatMap symbolGeneratedIdentities (Map.elems (typeViewHeadIdentities view))
    ++ concatMap (typeBinderIdentityGeneratedIdentities . Just) (Map.elems (typeViewBinderIdentities view))

typeParamGeneratedIdentities :: TypeParam -> [UniqueIdentity]
typeParamGeneratedIdentities =
  typeBinderIdentityGeneratedIdentities . typeParamBinderIdentity

typeBinderIdentityGeneratedIdentities :: Maybe TypeBinderIdentity -> [UniqueIdentity]
typeBinderIdentityGeneratedIdentities =
  maybe [] typeBinderGeneratedIdentities

checkedProgramEvidenceResolvedVars :: ConvertContext -> CheckedProgram -> Either BackendConversionError [ResolvedVar]
checkedProgramEvidenceResolvedVars context checked =
  concat
    <$> forM
      [ (checkedModule, binding)
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule
      ]
      ( \(checkedModule, binding) -> do
          bindingTy <- checkedBindingBackendValueType context checkedModule binding
          pure $
            declaredEvidenceResolvedVars (checkedBindingSourceTypeIdentity binding) bindingTy (checkedBindingTerm binding)
              ++ checkedBindingDeferredEvidenceResolvedVars binding
      )

declaredEvidenceResolvedVars :: SrcType -> BackendType -> XmlfTerm -> [ResolvedVar]
declaredEvidenceResolvedVars sourceTy bindingTy term =
  [ resolved
  | (index0, (resolved, _)) <- zip [0 :: Int ..] params,
    index0 `Set.member` declaredEvidenceValueArguments sourceTy bindingTy
  ]
  where
    (_, valueTy) = splitBackendForalls bindingTy
    (paramTys, _) = splitBackendArrows valueTy
    (params, _) = collectLeadingResolvedLams (length paramTys) term

checkedBindingDeferredEvidenceResolvedVars :: CheckedBinding -> [ResolvedVar]
checkedBindingDeferredEvidenceResolvedVars binding =
  concatMap obligationEvidenceResolvedVars (Map.elems (checkedBindingDeferredObligations binding))

obligationEvidenceResolvedVars :: DeferredProgramObligation -> [ResolvedVar]
obligationEvidenceResolvedVars obligation =
  case obligation of
    DeferredMethod deferred ->
      maybe [] deferredEvidenceResolvedVars (deferredMethodEvidence deferred)
        ++ concatMap evidenceInfoResolvedVars (deferredMethodLocalEvidence deferred)
    DeferredConstructor {} -> []
    DeferredCase {} -> []

deferredEvidenceResolvedVars :: DeferredMethodEvidence -> [ResolvedVar]
deferredEvidenceResolvedVars =
  evidenceMethodResolvedVars . deferredMethodEvidenceMethod

evidenceInfoResolvedVars :: EvidenceInfo -> [ResolvedVar]
evidenceInfoResolvedVars evidence =
  concatMap evidenceMethodResolvedVars (Map.elems (evidenceMethodsByIdentity evidence))

evidenceMethodResolvedVars :: EvidenceMethod -> [ResolvedVar]
evidenceMethodResolvedVars method =
  maybe [] (: []) (evidenceMethodResolvedVar method)

checkedProgramEvidenceValueArguments :: ConvertContext -> CheckedProgram -> Either BackendConversionError (Map SymbolIdentity (Set.Set Int))
checkedProgramEvidenceValueArguments context checked = do
  sources <-
    forM
      [ (checkedModule, binding)
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule
      ]
      ( \(checkedModule, binding) -> do
          bindingTy <- checkedBindingBackendValueType context checkedModule binding
          pure (checkedBindingSymbolIdentity binding, checkedBindingSourceTypeIdentity binding, bindingTy, checkedBindingTerm binding)
      )
  pure $
    evidenceValueArgumentFixedPoint context sources Map.empty

evidenceValueArgumentFixedPoint :: ConvertContext -> [(Maybe SymbolIdentity, SrcType, BackendType, XmlfTerm)] -> Map SymbolIdentity (Set.Set Int) -> Map SymbolIdentity (Set.Set Int)
evidenceValueArgumentFixedPoint context sources demands =
  let context' = context {ccEvidenceValueArgumentsByIdentity = demands}
      detectedDemands =
        Map.filter (not . Set.null) $
          Map.fromList
            [ (symbol, checkedBindingEvidenceValueArguments context' emptyClosureScope sourceTy bindingTy term)
            | (Just symbol, sourceTy, bindingTy, term) <- sources
            ]
      demands' =
        Map.unionWith Set.union demands detectedDemands
   in if demands' == demands
        then demands
        else evidenceValueArgumentFixedPoint context sources demands'

checkedProgramClosureValueArguments :: ConvertContext -> CheckedProgram -> Either BackendConversionError (Map SymbolIdentity (Set.Set Int))
checkedProgramClosureValueArguments context checked = do
  sources <-
    forM
      [ (checkedModule, binding)
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule
      ]
      ( \(checkedModule, binding) -> do
          bindingTy <- checkedBindingBackendValueType context checkedModule binding
          pure (checkedBindingSymbolIdentity binding, bindingTy, checkedBindingTerm binding)
      )
  pure (closureValueArgumentFixedPoint sources builtinClosureValueArguments)
  where
    closureValueArgumentFixedPoint sources demands =
      let context' = context {ccClosureValueArgumentsByIdentity = demands}
          detectedDemands =
            Map.unionWith
              Set.union
              builtinClosureValueArguments
              ( Map.filter (not . Set.null) $
                  Map.fromList
                    [ (symbol, bindingClosureValueArguments context' emptyClosureScope bindingTy term)
                    | (Just symbol, bindingTy, term) <- sources
                    ]
              )
          demands' =
            Map.unionWith Set.union demands detectedDemands
       in if demands' == demands
            then demands
            else closureValueArgumentFixedPoint sources demands'

builtinClosureValueArguments :: Map SymbolIdentity (Set.Set Int)
builtinClosureValueArguments =
  Map.fromList
    [ (builtinValueIdentity name, demanded)
    | (name, spec) <- Map.toList PrimitiveInventory.primitiveValueSpecs,
      let demanded = PrimitiveInventory.primitiveValueClosureValueArguments spec,
      not (Set.null demanded)
    ]

checkedBindingBackendValueType :: ConvertContext -> CheckedModule -> CheckedBinding -> Either BackendConversionError BackendType
checkedBindingBackendValueType context checkedModule binding = do
  canonicalElabTyOpen <- checkedBindingCanonicalTypeOpen context checkedModule binding
  rawBindingTy <- convertElabType canonicalElabTyOpen
  pure (canonicalizeBackendType context rawBindingTy)

bindingClosureValueArguments :: ConvertContext -> ClosureScope -> BackendType -> XmlfTerm -> Set.Set Int
bindingClosureValueArguments context scope bindingTy term =
  directClosureValueArguments context bindingTy term
    `Set.union` aliasedClosureValueArguments context scope bindingTy term

checkedBindingEvidenceValueArguments :: ConvertContext -> ClosureScope -> SrcType -> BackendType -> XmlfTerm -> Set.Set Int
checkedBindingEvidenceValueArguments context scope sourceTy bindingTy term =
  declaredEvidenceValueArguments sourceTy bindingTy
    `Set.union` bindingEvidenceValueArguments context scope bindingTy term

bindingEvidenceValueArguments :: ConvertContext -> ClosureScope -> BackendType -> XmlfTerm -> Set.Set Int
bindingEvidenceValueArguments context scope bindingTy term =
  directEvidenceValueArguments context bindingTy term
    `Set.union` aliasedEvidenceValueArguments context scope bindingTy term

declaredEvidenceValueArguments :: SrcType -> BackendType -> Set.Set Int
declaredEvidenceValueArguments sourceTy bindingTy =
  Set.fromList [0 .. evidenceCount - 1]
  where
    evidenceCount =
      max 0 (runtimeArity - visibleArity)
    runtimeArity =
      length runtimeParamTys
    visibleArity =
      sourceValueArity sourceTy
    (_, runtimeValueTy) =
      splitBackendForalls bindingTy
    (runtimeParamTys, _) =
      splitBackendArrows runtimeValueTy

sourceValueArity :: SrcType -> Int
sourceValueArity sourceTy =
  length paramTys
  where
    (paramTys, _) =
      splitSourceArrows (dropSourceForalls sourceTy)

directEvidenceValueArguments :: ConvertContext -> BackendType -> XmlfTerm -> Set.Set Int
directEvidenceValueArguments context bindingTy term =
  Set.fromList
    [ index0
    | (index0, (resolved, _)) <- zip [0 :: Int ..] params,
      isEvidenceCapture context resolved
    ]
  where
    (_, valueTy) = splitBackendForalls bindingTy
    (paramTys, _) = splitBackendArrows valueTy
    (params, _) = collectLeadingResolvedLams (length paramTys) term

directClosureValueArguments :: ConvertContext -> BackendType -> XmlfTerm -> Set.Set Int
directClosureValueArguments context bindingTy term =
  Set.fromList
    [ index0
    | (index0, ((resolved, _), paramTy)) <- zip [0 :: Int ..] (zip params paramTys),
      not (isEvidenceCapture context resolved),
      isClosureConvertibleFunctionType paramTy,
      termUsesFunctionAsValue paramTy (TermVarResolved resolved) body
    ]
  where
    (_, valueTy) = splitBackendForalls bindingTy
    (paramTys, _) = splitBackendArrows valueTy
    (params, body) = collectLeadingResolvedLams (length paramTys) term

aliasedClosureValueArguments :: ConvertContext -> ClosureScope -> BackendType -> XmlfTerm -> Set.Set Int
aliasedClosureValueArguments context scope bindingTy term =
  aliasedValueArgumentIndices lookupClosureValueArgumentDemand context scope bindingTy term

aliasedEvidenceValueArguments :: ConvertContext -> ClosureScope -> BackendType -> XmlfTerm -> Set.Set Int
aliasedEvidenceValueArguments context scope bindingTy term =
  aliasedValueArgumentIndices lookupEvidenceValueArguments context scope bindingTy term

aliasedValueArgumentIndices ::
  (ConvertContext -> ClosureScope -> XmlfTerm -> Set.Set Int) ->
  ConvertContext ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  Set.Set Int
aliasedValueArgumentIndices lookupDemand context scope bindingTy term =
  Set.fromList
    [ paramOffset + exposedIndex
    | demandedIndex <- Set.toList (lookupDemand context scope headTerm),
      let exposedIndex = demandedIndex - suppliedCount,
      demandedIndex >= suppliedCount,
      exposedIndex < exposedCount
    ]
  where
    (_, valueTy) = splitBackendForalls bindingTy
    (paramTys, _) = splitBackendArrows valueTy
    (params, body) = collectLeadingResolvedLams (length paramTys) term
    paramOffset = length params
    exposedCount = length paramTys - length params
    (headTerm, suppliedArgs) = collectAliasedApps body
    suppliedCount = length suppliedArgs

lookupClosureValueArgumentDemand :: ConvertContext -> ClosureScope -> XmlfTerm -> Set.Set Int
lookupClosureValueArgumentDemand context scope rawHeadTerm =
  localDemand (closureScopeClosureValueArgumentsByLocal scope)
    `Set.union` identityDemand (ccClosureValueArgumentsByIdentity context)
    `Set.union` deferredDemand (ccClosureValueArgumentsByDeferred context)
  where
    headTerm = stripClosureHeadTypeInsts rawHeadTerm
    localDemand scopeMap =
      case termHeadLocalRef headTerm of
        Just localRef -> Map.findWithDefault Set.empty localRef scopeMap
        Nothing -> Set.empty
    identityDemand contextMap =
      case termHeadSymbolIdentity context headTerm of
        Just symbol -> Map.findWithDefault Set.empty symbol contextMap
        Nothing -> Set.empty
    deferredDemand contextMap =
      case termHeadDeferredRef headTerm of
        Just ref -> Map.findWithDefault Set.empty ref contextMap
        Nothing -> Set.empty

lookupEvidenceValueArguments :: ConvertContext -> ClosureScope -> XmlfTerm -> Set.Set Int
lookupEvidenceValueArguments context scope rawHeadTerm =
  localDemand (closureScopeEvidenceValueArgumentsByLocal scope)
    `Set.union` identityDemand (ccEvidenceValueArgumentsByIdentity context)
    `Set.union` deferredDemand (ccEvidenceValueArgumentsByDeferred context)
  where
    headTerm = stripClosureHeadTypeInsts rawHeadTerm
    localDemand scopeMap =
      case termHeadLocalRef headTerm of
        Just localRef -> Map.findWithDefault Set.empty localRef scopeMap
        Nothing -> Set.empty
    identityDemand contextMap =
      case termHeadSymbolIdentity context headTerm of
        Just symbol -> Map.findWithDefault Set.empty symbol contextMap
        Nothing -> Set.empty
    deferredDemand contextMap =
      case termHeadDeferredRef headTerm of
        Just ref -> Map.findWithDefault Set.empty ref contextMap
        Nothing -> Set.empty

termHeadSymbolIdentity :: ConvertContext -> XmlfTerm -> Maybe SymbolIdentity
termHeadSymbolIdentity _context term =
  case term of
    EVarNode resolved ->
      resolvedVarSymbolIdentity resolved
    _ ->
      Nothing

termHeadDeferredRef :: XmlfTerm -> Maybe DeferredRef
termHeadDeferredRef term =
  case term of
    EVarNode resolved -> deferredResolvedVarRef resolved
    _ ->
      Nothing

termHeadLocalRef :: XmlfTerm -> Maybe LocalRef
termHeadLocalRef term =
  case term of
    EVarNode resolved -> resolvedVarLocalRef resolved
    _ -> Nothing

allDataInfos :: CheckedProgram -> [DataInfo]
allDataInfos checked =
  [ dataInfo
    | checkedModule <- checkedProgramModules checked,
      dataInfo <- Map.elems (checkedModuleData checkedModule)
  ]

checkedProgramTermRuntimeNamesByIdentity :: CheckedProgram -> Either BackendConversionError (Map SymbolIdentity String)
checkedProgramTermRuntimeNamesByIdentity checked =
  Map.map snd
    <$> uniqueCheckedInfoByIdentity BackendDuplicateBinding fst (checkedBindings ++ builtinBindings)
  where
    checkedBindings =
      [ (symbol, checkedBindingRuntimeName binding)
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule,
        Just symbol <- [checkedBindingSymbolIdentity binding]
      ]

    builtinBindings =
      [ (builtinValueIdentity name, name)
      | name <- Map.keys PrimitiveInventory.primitiveValueSpecs
      ]

dataInfoModuleIdentityMap :: CheckedProgram -> Map SymbolIdentity SymbolIdentity
dataInfoModuleIdentityMap checked =
  Map.fromList
    [ (dataInfoSymbol info, checkedModuleIdentity checkedModule)
      | checkedModule <- checkedProgramModules checked,
        info <- Map.elems (checkedModuleData checkedModule)
    ]

moduleElaborateScopes :: CheckedProgram -> Map SymbolIdentity DataInfo -> Map SymbolIdentity ElaborateScope
moduleElaborateScopes checked dataByIdentity =
  Map.fromList
    [ (resolvedModuleIdentity resolvedModule, elaborateScopeForResolvedModule dataByIdentity resolvedModule)
      | resolvedModule <- resolvedProgramModules (checkedProgramResolved checked)
    ]

elaborateScopeForResolvedModule :: Map SymbolIdentity DataInfo -> ResolvedModule -> ElaborateScope
elaborateScopeForResolvedModule dataByIdentity resolvedModule =
  mkElaborateScope Map.empty dataTypes Map.empty []
  where
    dataTypes =
      visibleDataInfoMap dataByIdentity (resolvedScopeTypes (resolvedModuleScope resolvedModule))
        `Map.union` qualifiedDataInfoMap (Map.elems dataByIdentity)

visibleDataInfoMap :: Map SymbolIdentity DataInfo -> Map String ResolvedSymbol -> Map String DataInfo
visibleDataInfoMap dataByIdentity =
  Map.mapMaybe (\symbol -> canonicalDataInfo <$> Map.lookup (resolvedSymbolIdentity symbol) dataByIdentity)

qualifiedDataInfoMap :: [DataInfo] -> Map String DataInfo
qualifiedDataInfoMap dataInfos =
  Map.fromList [(qualifiedDataName info, canonicalDataInfo info) | info <- dataInfos]

fallbackElaborateScopeForDataInfo :: Map SymbolIdentity SymbolIdentity -> [DataInfo] -> DataInfo -> ElaborateScope
fallbackElaborateScopeForDataInfo dataModuleIdentities dataInfos info =
  mkElaborateScope Map.empty dataTypes Map.empty []
  where
    dataTypes =
      localDataInfoMap dataModuleIdentities dataInfos info
        `Map.union` uniqueUnqualifiedDataInfoMap dataInfos
        `Map.union` qualifiedDataInfoMap dataInfos

localDataInfoMap :: Map SymbolIdentity SymbolIdentity -> [DataInfo] -> DataInfo -> Map String DataInfo
localDataInfoMap dataModuleIdentities dataInfos info =
  Map.fromList
    [ (dataInfoUnqualifiedName candidate, canonicalDataInfo candidate)
      | candidate <- dataInfos,
        sameDataModule candidate
    ]
  where
    sameDataModule candidate =
      case Map.lookup (dataInfoSymbol info) dataModuleIdentities of
        Nothing -> False
        Just moduleIdentity ->
          Map.lookup (dataInfoSymbol candidate) dataModuleIdentities == Just moduleIdentity


uniqueUnqualifiedDataInfoMap :: [DataInfo] -> Map String DataInfo
uniqueUnqualifiedDataInfoMap dataInfos =
  Map.fromList
    [ (name, canonicalDataInfo info)
      | (name, infos) <- Map.toList grouped,
        [info] <- [infos]
    ]
  where
    grouped =
      Map.fromListWith (++)
        [ (dataInfoUnqualifiedName info, [info])
          | info <- dataInfos
        ]

dataInfoUnqualifiedName :: DataInfo -> String
dataInfoUnqualifiedName =
  dataInfoIdentityName

canonicalDataInfo :: DataInfo -> DataInfo
canonicalDataInfo = id

bindingDataHints :: Map SymbolIdentity DataMeta -> CheckedProgram -> Map SymbolIdentity DataMeta
bindingDataHints dataMetasByIdentity checked =
  Map.fromList
    [ (symbol, dataMeta)
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule,
        Just symbol <- [checkedBindingSymbolIdentity binding],
        Just dataMeta <- [bindingDataHint dataMetasByIdentity binding]
    ]

checkedBindingSymbolIdentity :: CheckedBinding -> Maybe SymbolIdentity
checkedBindingSymbolIdentity =
  resolvedVarSymbolIdentity . checkedBindingResolvedVar

checkedBindingRuntimeName :: CheckedBinding -> String
checkedBindingRuntimeName =
  resolvedVarRuntimeName . checkedBindingResolvedVar

bindingDataHint :: Map SymbolIdentity DataMeta -> CheckedBinding -> Maybe DataMeta
bindingDataHint dataMetasByIdentity binding =
  elabTypeDataMeta dataMetasByIdentity (checkedBindingType binding)
    <|> sourceBindingDataHint dataMetasByIdentity binding

sourceBindingDataHint :: Map SymbolIdentity DataMeta -> CheckedBinding -> Maybe DataMeta
sourceBindingDataHint dataMetasByIdentity binding =
  case splitSourceArrows (dropSourceForalls (typeViewIdentity sourceView)) of
    ([], _) -> sourceTypeDataMetaForView dataMetasByIdentity sourceView
    _ -> Nothing
  where
    sourceView = checkedBindingSourceTypeView binding

elabTypeDataMeta :: Map SymbolIdentity DataMeta -> ElabType -> Maybe DataMeta
elabTypeDataMeta dataMetasByIdentity ty =
  case dropElabForalls ty of
    TBaseWithIdentity (Just identity) _ ->
      Map.lookup identity dataMetasByIdentity
    TConWithIdentity (Just identity) _ _ ->
      Map.lookup identity dataMetasByIdentity
    _ ->
      Nothing

dropElabForalls :: ElabType -> ElabType
dropElabForalls =
  \case
    TForallRef _ _ body -> dropElabForalls body
    ty -> ty

sourceTypeDataMetaForView :: Map SymbolIdentity DataMeta -> TypeView -> Maybe DataMeta
sourceTypeDataMetaForView dataMetasByIdentity view =
  case (sourceTypeDataHead (sourceTypeResult (typeViewDisplay view)), sourceTypeDataHead (sourceTypeResult (typeViewIdentity view))) of
    (displayHead, identityHead) ->
      firstMaybe
        [ identityHead >>= typeViewHeadIdentityForAlias view >>= (`Map.lookup` dataMetasByIdentity),
          displayHead >>= typeViewHeadIdentityForAlias view >>= (`Map.lookup` dataMetasByIdentity)
        ]
  where
    sourceTypeResult =
      snd . splitSourceArrows . dropSourceForalls

applySourceTypeIdentity :: ConvertContext -> ElaborateScope -> TypeView -> BackendType -> BackendType
applySourceTypeIdentity context scope view =
  applySourceTypeViewIdentityWith context scope Map.empty view

applySourceTypeViewIdentityWith :: ConvertContext -> ElaborateScope -> Map BackendTypeSubstitutionKey BackendType -> TypeView -> BackendType -> BackendType
applySourceTypeViewIdentityWith context scope sourceTypeVars view backendTy =
  case (typeViewDisplay view, typeViewIdentity view, backendTy) of
    (STArrow displayDom displayCod, STArrow identityDom identityCod, BTArrow backendDom backendCod) ->
      BTArrow
        (applySourceTypeViewIdentityWith context scope sourceTypeVars (view {typeViewDisplay = displayDom, typeViewIdentity = identityDom}) backendDom)
        (applySourceTypeViewIdentityWith context scope sourceTypeVars (view {typeViewDisplay = displayCod, typeViewIdentity = identityCod}) backendCod)
    (STForall displayName displayBound displayBody, STForall identityName identityBound identityBody, BTForallWithIdentity backendIdentity backendName backendBound backendForallBody) ->
      BTForallWithIdentity
        backendIdentity
        backendName
        (applySourceTypeViewIdentityWith context scope sourceTypeVars (view {typeViewDisplay = maybe STBottom unSrcBound displayBound, typeViewIdentity = maybe STBottom unSrcBound identityBound}) <$> backendBound)
        ( applySourceTypeViewIdentityWith
            context
            scope
            (insertSourceTypeVarAliases backendIdentity [displayName, identityName, backendName] (BTVarWithIdentity backendIdentity backendName) sourceTypeVars)
            (view {typeViewDisplay = displayBody, typeViewIdentity = identityBody})
            backendForallBody
        )
    _
      | backendTypeIsDataLike backendTy,
        Just dataTy <- sourceBackendDataType view <|> sourceBackendDataType (lowerTypeViewForScope scope view) ->
          dataTy
    _ ->
      backendTy
  where
    sourceBackendDataType candidateView = do
      dataMeta <- sourceTypeDataMetaForView (ccDataByIdentity context) candidateView
      backendTy0 <- either (const Nothing) Just (convertSourceTypeViewWithIdentities candidateView)
      let sourceBackendTy =
            applyConstructorTypeBinderIdentities sourceTypeVars (typeViewIdentity candidateView) $
              canonicalizeSourceBackendTypeHeads (ccDataByIdentity context) backendTy0
      sourceDataTypeForSource dataMeta sourceBackendTy <|> canonicalDataTypeForSource dataMeta sourceBackendTy

canonicalizeSourceBackendTypeHeads :: Map SymbolIdentity DataMeta -> BackendType -> BackendType
canonicalizeSourceBackendTypeHeads dataMetasByIdentity =
  \case
    BTBaseWithIdentity mbIdentity (BaseTy name) ->
      let (mbIdentity', name') = canonicalHead mbIdentity name
       in BTBaseWithIdentity mbIdentity' (BaseTy name')
    BTConWithIdentity mbIdentity (BaseTy name) args ->
      let (mbIdentity', name') = canonicalHead mbIdentity name
       in BTConWithIdentity mbIdentity' (BaseTy name') (fmap (canonicalizeSourceBackendTypeHeads dataMetasByIdentity) args)
    BTArrow dom cod ->
      BTArrow
        (canonicalizeSourceBackendTypeHeads dataMetasByIdentity dom)
        (canonicalizeSourceBackendTypeHeads dataMetasByIdentity cod)
    BTVarAppWithIdentity identity name args ->
      BTVarAppWithIdentity identity name (fmap (canonicalizeSourceBackendTypeHeads dataMetasByIdentity) args)
    BTForallWithIdentity identity name mb body ->
      BTForallWithIdentity
        identity
        name
        (canonicalizeSourceBackendTypeHeads dataMetasByIdentity <$> mb)
        (canonicalizeSourceBackendTypeHeads dataMetasByIdentity body)
    BTMuWithIdentity identity name body ->
      BTMuWithIdentity identity name (canonicalizeSourceBackendTypeHeads dataMetasByIdentity body)
    ty -> ty
  where
    canonicalHead mbIdentity name =
      case mbIdentity of
        Just identity ->
          case Map.lookup identity dataMetasByIdentity of
            Just dataMeta -> (Just (dataInfoSymbol (dmInfo dataMeta)), backendDataName (dmBackend dataMeta))
            Nothing -> (mbIdentity, name)
        Nothing ->
          case
            [ dataMeta
            | dataMeta <- Map.elems dataMetasByIdentity,
              name `elem` dataMetaStructuralNames dataMeta
            ]
          of
            [dataMeta] -> (Just (dataInfoSymbol (dmInfo dataMeta)), backendDataName (dmBackend dataMeta))
            _ -> (Nothing, name)

canonicalDataTypeForSource :: DataMeta -> BackendType -> Maybe BackendType
canonicalDataTypeForSource dataMeta sourceBackendTy =
  case candidates of
    candidate : _ -> Just candidate
    [] -> Nothing
  where
    candidates =
      nub
        [ candidate
        | constructor <- backendDataConstructors (dmBackend dataMeta),
          candidate <- candidateConstructorResultTypes (dmBackend dataMeta) constructor sourceBackendTy
        ]

sourceDataTypeForSource :: DataMeta -> BackendType -> Maybe BackendType
sourceDataTypeForSource dataMeta sourceBackendTy =
  if preludePrimitiveDataMeta dataMeta
    then
      case sourceBackendTy of
        BTBaseWithIdentity identity base
          | dataHeadMatches identity base ->
              Just (backendDataType (backendDataIdentity dataDecl) (backendDataName dataDecl) [])
        BTConWithIdentity identity base args
          | dataHeadMatches identity base ->
              Just (backendDataType (backendDataIdentity dataDecl) (backendDataName dataDecl) (NE.toList args))
        _ ->
          Nothing
    else Nothing
  where
    dataDecl =
      dmBackend dataMeta

    dataHeadMatches identity base =
      backendTypeHeadMatches identity base (backendDataIdentity dataDecl) (BaseTy (backendDataName dataDecl))

preludePrimitiveDataMeta :: DataMeta -> Bool
preludePrimitiveDataMeta dataMeta =
  symbolDefiningModule symbol == "Prelude"
    && dataInfoIdentityName info `Set.member` preludePrimitiveDataTypeNames
  where
    info =
      dmInfo dataMeta
    symbol =
      dataInfoSymbol info

preludePrimitiveDataTypeNames :: Set.Set String
preludePrimitiveDataTypeNames =
  Set.fromList ["List", "Nat", "Option", "Unit"]

preludePrimitiveBackendTypeHead :: ConvertContext -> BackendType -> Bool
preludePrimitiveBackendTypeHead context =
  \case
    BTBaseWithIdentity (Just identity) _ -> maybe False preludePrimitiveDataMeta (Map.lookup identity (ccDataByIdentity context))
    BTConWithIdentity (Just identity) _ _ -> maybe False preludePrimitiveDataMeta (Map.lookup identity (ccDataByIdentity context))
    _ -> False

backendTypeIsDataLike :: BackendType -> Bool
backendTypeIsDataLike =
  \case
    BTBase {} -> True
    BTCon {} -> True
    BTMu {} -> True
    _ -> False

sourceTypeDataHead :: SrcType -> Maybe String
sourceTypeDataHead =
  \case
    STBase name -> Just name
    STCon name _ -> Just name
    _ -> Nothing

dropSourceForalls :: SrcType -> SrcType
dropSourceForalls =
  \case
    STForall _ _ body -> dropSourceForalls body
    ty -> ty

splitSourceArrows :: SrcType -> ([SrcType], SrcType)
splitSourceArrows =
  go []
  where
    go args ty =
      case ty of
        STArrow arg result -> go (args ++ [arg]) result
        _ -> (args, ty)

buildDataMetaForDataInfo ::
  Map SymbolIdentity ElaborateScope ->
  Map SymbolIdentity SymbolIdentity ->
  [DataInfo] ->
  DataInfo ->
  Either BackendConversionError DataMeta
buildDataMetaForDataInfo moduleScopes dataModuleIdentities dataInfos info =
  buildDataMeta
    (lookupDataInfoModuleIdentity dataModuleIdentities info)
    (elaborateScopeForDataInfo moduleScopes dataModuleIdentities dataInfos info)
    info

elaborateScopeForDataInfo ::
  Map SymbolIdentity ElaborateScope ->
  Map SymbolIdentity SymbolIdentity ->
  [DataInfo] ->
  DataInfo ->
  ElaborateScope
elaborateScopeForDataInfo moduleScopes dataModuleIdentities dataInfos info =
  case lookupDataInfoModuleIdentity dataModuleIdentities info >>= (`Map.lookup` moduleScopes) of
    Just scope -> scope
    Nothing -> fallbackElaborateScopeForDataInfo dataModuleIdentities dataInfos info

lookupDataInfoModuleIdentity :: Map SymbolIdentity SymbolIdentity -> DataInfo -> Maybe SymbolIdentity
lookupDataInfoModuleIdentity dataModuleIdentities info =
  Map.lookup (dataInfoSymbol info) dataModuleIdentities

qualifiedDataName :: DataInfo -> String
qualifiedDataName =
  dataInfoIdentityQualifiedName

buildDataMeta :: Maybe SymbolIdentity -> ElaborateScope -> DataInfo -> Either BackendConversionError DataMeta
buildDataMeta moduleIdentity scope info = do
  rawConstructors <- mapM (convertConstructorInfo scope (dataParamBinders info)) (dataConstructors info)
  let rawData =
        BackendDataWithIdentity
          { backendDataIdentity = Just (dataInfoSymbol info),
            backendDataNameWithIdentity = qualifiedDataName info,
            backendDataParameterRefsWithIdentity = dataInfoParameterRefs info,
            backendDataConstructorsWithIdentity = rawConstructors
          }
      rawMeta =
        DataMeta
          { dmInfo = info,
            dmBackend = rawData
          }
      rawRecoveryContext =
        ConvertContext
          { ccModuleScopes = maybe Map.empty (`Map.singleton` scope) moduleIdentity,
            ccConstructorsByIdentity = Map.empty,
            ccTermRuntimeNamesByIdentity = Map.empty,
            ccBindingData = Map.empty,
            ccDataByIdentity = Map.singleton (dataInfoSymbol info) rawMeta,
            ccDataModuleIdentities =
              maybe Map.empty (Map.singleton (dataInfoSymbol info)) moduleIdentity,
            ccData = [rawMeta],
            ccClosureGlobalsByIdentity = Set.empty,
            ccClosureValueArgumentsByIdentity = Map.empty,
            ccEvidenceValueArgumentsByIdentity = Map.empty,
            ccClosureValueArgumentsByDeferred = Map.empty,
            ccEvidenceValueArgumentsByDeferred = Map.empty,
            ccEvidenceResolvedVarKeys = Set.empty,
            ccIdentityGenerator = identityGeneratorAfter (dataInfoGeneratedIdentities info),
            ccCurrentModuleIdentity = moduleIdentity,
            ccCurrentBindingName = ""
          }
      canonicalConstructors =
        map (canonicalizeBackendConstructorTypes rawRecoveryContext (dataInfoParameterRefs info)) rawConstructors
      canonicalData =
        rawData {backendDataConstructorsWithIdentity = canonicalConstructors}
      canonicalMeta =
        rawMeta {dmBackend = canonicalData}
      recoveryContext =
        rawRecoveryContext
          { ccDataByIdentity = Map.singleton (dataInfoSymbol info) canonicalMeta,
            ccData = [canonicalMeta]
          }
      constructors =
        if null (dataTypeParams info) || any backendConstructorContainsVarApp rawConstructors
          then map (recoverBackendConstructorTypes recoveryContext (dataInfoParameterRefs info)) canonicalConstructors
          else canonicalConstructors
  Right
    DataMeta
      { dmInfo = info,
        dmBackend =
          BackendDataWithIdentity
            { backendDataIdentity = Just (dataInfoSymbol info),
              backendDataNameWithIdentity = qualifiedDataName info,
              backendDataParameterRefsWithIdentity = dataInfoParameterRefs info,
              backendDataConstructorsWithIdentity = constructors
            }
      }

dataInfoParameterRefs :: DataInfo -> [BackendDataParameterRef]
dataInfoParameterRefs info =
  [ backendDataParameterRefFromIdentity identity name
  | (name, identity) <- dataParamBinders info
  ]

canonicalizeBackendConstructorTypes :: ConvertContext -> [BackendDataParameterRef] -> BackendConstructor -> BackendConstructor
canonicalizeBackendConstructorTypes context dataParameterRefs constructor =
  BackendConstructorWithIdentity
    (backendConstructorIdentity constructor)
    (backendConstructorName constructor)
    (map canonicalizeTypeBinder (backendConstructorForalls constructor))
    (map canonicalizeTy (backendConstructorFields constructor))
    (canonicalizeTy (backendConstructorResult constructor))
  where
    canonicalizeTy =
      canonicalizeDataParameterRefs dataParameterRefs
        . canonicalizeSourceBackendTypeHeads (ccDataByIdentity context)
        . canonicalizeStructuralMuNames context

    canonicalizeTypeBinder binder =
      binder {backendTypeBinderBound = fmap canonicalizeTy (backendTypeBinderBound binder)}

canonicalizeDataParameterRefs :: [BackendDataParameterRef] -> BackendType -> BackendType
canonicalizeDataParameterRefs refs =
  go Set.empty
  where
    go bound ty =
      case ty of
        BTVarWithIdentity identity name
          | not (typeBinderBound bound identity name),
            Just ref <- dataParameterRefFor identity name ->
              backendDataParameterRefType ref
        BTVarWithIdentity {} ->
          ty
        BTArrow dom cod ->
          BTArrow (go bound dom) (go bound cod)
        BTBaseWithIdentity {} ->
          ty
        BTConWithIdentity identity name args ->
          BTConWithIdentity identity name (fmap (go bound) args)
        BTVarAppWithIdentity identity name args
          | not (typeBinderBound bound identity name),
            Just ref <- dataParameterRefFor identity name,
            BTVarWithIdentity refIdentity refName <- backendDataParameterRefType ref ->
              BTVarAppWithIdentity refIdentity refName (fmap (go bound) args)
        BTVarAppWithIdentity identity name args ->
          BTVarAppWithIdentity identity name (fmap (go bound) args)
        BTForallWithIdentity identity name mb body ->
          BTForallWithIdentity identity name (fmap (go bound) mb) (go (insertTypeBinderBound identity name bound) body)
        BTMuWithIdentity identity name body ->
          BTMuWithIdentity identity name (go (insertTypeBinderBound identity name bound) body)
        BTBottom ->
          BTBottom

    dataParameterRefFor identity name =
      find ((== backendTypeSubstitutionKeyFor identity name) . backendDataParameterRefKey) refs

    typeBinderBound bound identity name =
      Set.member (backendTypeSubstitutionKeyFor identity name) bound

    insertTypeBinderBound identity name =
      Set.insert (backendTypeSubstitutionKeyFor identity name)

recoverBackendConstructorTypes :: ConvertContext -> [BackendDataParameterRef] -> BackendConstructor -> BackendConstructor
recoverBackendConstructorTypes context dataParameterRefs constructor =
  BackendConstructorWithIdentity
    (backendConstructorIdentity constructor)
    (backendConstructorName constructor)
    (map recoverTypeBinder (backendConstructorForalls constructor))
    (map recoverTy (backendConstructorFields constructor))
    (recoverTy (backendConstructorResult constructor))
  where
    recoverTy =
      canonicalizeDataParameterRefs dataParameterRefs
        . canonicalizeSourceBackendTypeHeads (ccDataByIdentity context)
        . recoverStructuralBackendType context

    recoverTypeBinder binder =
      binder {backendTypeBinderBound = fmap recoverTy (backendTypeBinderBound binder)}

backendConstructorContainsVarApp :: BackendConstructor -> Bool
backendConstructorContainsVarApp constructor =
  any backendTypeContainsVarApp (backendConstructorFields constructor)
    || backendTypeContainsVarApp (backendConstructorResult constructor)
    || any (maybe False backendTypeContainsVarApp . backendTypeBinderBound) (backendConstructorForalls constructor)

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

backendTypeNeedsStructuralRecovery :: ConvertContext -> BackendType -> Bool
backendTypeNeedsStructuralRecovery context =
  \case
    BTVar {} -> False
    BTArrow dom cod -> backendTypeNeedsStructuralRecovery context dom || backendTypeNeedsStructuralRecovery context cod
    BTBase {} -> False
    BTCon _ args -> any (backendTypeNeedsStructuralRecovery context) args
    BTVarApp {} -> True
    BTForall _ mb body -> maybe False (backendTypeNeedsStructuralRecovery context) mb || backendTypeNeedsStructuralRecovery context body
    BTMuWithIdentity identity name body ->
      maybe False dataMetaNeedsStructuralRecovery (structuralRecursiveDataMetaByIdentity context identity <|> structuralRecursiveDataMetaByFallback context identity name)
        || backendTypeNeedsStructuralRecovery context body
    BTMu name body ->
      maybe False dataMetaNeedsStructuralRecovery (structuralRecursiveDataMetaByFallback context Nothing name)
        || backendTypeNeedsStructuralRecovery context body
    BTBottom -> False

dataMetaNeedsStructuralRecovery :: DataMeta -> Bool
dataMetaNeedsStructuralRecovery dataMeta =
  any backendConstructorContainsVarApp (backendDataConstructors (dmBackend dataMeta))

contextForDataMeta :: ConvertContext -> DataMeta -> ConvertContext
contextForDataMeta context dataMeta =
  context
    { ccCurrentModuleIdentity =
        lookupDataInfoModuleIdentity (ccDataModuleIdentities context) (dmInfo dataMeta)
    }

constructorMetasForData :: DataMeta -> [ConstructorMeta]
constructorMetasForData dataMeta =
  [ ConstructorMeta
      { cmInfo = ctorInfo,
        cmBackend = backendCtor,
        cmData = dataMeta
      }
    | (ctorInfo, backendCtor) <- zip (dataConstructors (dmInfo dataMeta)) (backendDataConstructors (dmBackend dataMeta))
  ]

convertDataInfo :: ConvertContext -> DataInfo -> Either BackendConversionError BackendData
convertDataInfo context info =
  case Map.lookup (dataInfoSymbol info) (ccDataByIdentity context) of
    Just dataMeta -> Right (dmBackend dataMeta)
    Nothing ->
      buildDataMeta
        (lookupDataInfoModuleIdentity (ccDataModuleIdentities context) info)
        ( elaborateScopeForDataInfo
            (ccModuleScopes context)
            (ccDataModuleIdentities context)
            (map dmInfo (ccData context))
            info
        )
        info
        >>= Right . dmBackend

convertConstructorInfo :: ElaborateScope -> [(String, TypeBinderIdentity)] -> ConstructorInfo -> Either BackendConversionError BackendConstructor
convertConstructorInfo scope dataTypeParams0 info = do
  (forallViews, fieldViews, resultView) <- constructorInfoTypeViews info
  let typeVars = constructorTypeVars dataTypeParams0 forallViews
      binderIdentities = constructorTypeBinderIdentities dataTypeParams0 forallViews
      attachBinderIdentities view =
        view
          { typeViewBinderIdentities =
              mergeTypeBinderIdentityMaps [binderIdentities, typeViewBinderIdentities view]
          }
  foralls <- mapM (convertConstructorForallView scope typeVars attachBinderIdentities) forallViews
  fields <- mapM (convertConstructorTypeView scope typeVars . attachBinderIdentities) fieldViews
  resultTy <- convertConstructorTypeView scope typeVars (attachBinderIdentities resultView)
  Right
    BackendConstructorWithIdentity
      { backendConstructorIdentity = Just (ctorInfoSymbol info),
        backendConstructorNameWithIdentity = ctorRuntimeName info,
        backendConstructorForallsWithIdentity = foralls,
        backendConstructorFieldsWithIdentity = fields,
        backendConstructorResultWithIdentity = resultTy
      }

constructorInfoTypeViews ::
  ConstructorInfo ->
  Either BackendConversionError ([(String, TypeBinderIdentity, Maybe TypeView)], [TypeView], TypeView)
constructorInfoTypeViews info = do
  foralls <- zipConstructorForalls view displayForalls identityForalls (ctorForallBinderInfo info)
  fields <- zipTypeViews view "constructor field" displayArgs identityArgs
  resultTy <- zipTypeView view "constructor result" displayResult identityResult
  Right (foralls, fields, resultTy)
  where
    view = ctorTypeView info
    (displayForalls, displayBody) = splitForalls (typeViewDisplay view)
    (displayArgs, displayResult) = splitArrows displayBody
    (identityForalls, identityBody) = splitForalls (typeViewIdentity view)
    (identityArgs, identityResult) = splitArrows identityBody

zipConstructorForalls ::
  TypeView ->
  [(String, Maybe SrcType)] ->
  [(String, Maybe SrcType)] ->
  [ConstructorForallBinder] ->
  Either BackendConversionError [(String, TypeBinderIdentity, Maybe TypeView)]
zipConstructorForalls view displayForalls identityForalls binders =
  go displayForalls identityForalls binders
  where
    go [] [] [] =
      Right []
    go ((_, displayBound) : displayRest) ((_, identityBound) : identityRest) (binder : binderRest) = do
      bound <- zipMaybeTypeView view "constructor forall bound" displayBound identityBound
      rest <- go displayRest identityRest binderRest
      Right
        ( ( constructorForallDisplayName binder,
            constructorForallIdentity binder,
            bound
          )
            : rest
        )
    go _ _ _ =
      Left (BackendUnsupportedCaseShape "constructor display and identity forall shapes differ")

zipTypeViews :: TypeView -> String -> [SrcType] -> [SrcType] -> Either BackendConversionError [TypeView]
zipTypeViews _ _ [] [] =
  Right []
zipTypeViews baseView role (displayTy : displayRest) (identityTy : identityRest) = do
  view <- zipTypeView baseView role displayTy identityTy
  rest <- zipTypeViews baseView role displayRest identityRest
  Right (view : rest)
zipTypeViews _ role _ _ =
  Left (BackendUnsupportedCaseShape (role ++ " display and identity shapes differ"))

zipMaybeTypeView :: TypeView -> String -> Maybe SrcType -> Maybe SrcType -> Either BackendConversionError (Maybe TypeView)
zipMaybeTypeView _ _ Nothing Nothing =
  Right Nothing
zipMaybeTypeView baseView role (Just displayTy) (Just identityTy) =
  Just <$> zipTypeView baseView role displayTy identityTy
zipMaybeTypeView _ role _ _ =
  Left (BackendUnsupportedCaseShape (role ++ " display and identity presence differs"))

zipTypeView :: TypeView -> String -> SrcType -> SrcType -> Either BackendConversionError TypeView
zipTypeView view _ displayTy identityTy =
  Right
    view
      { typeViewDisplay = displayTy,
        typeViewIdentity = identityTy
      }

constructorTypeVars :: [(String, TypeBinderIdentity)] -> [(String, TypeBinderIdentity, Maybe TypeView)] -> Map BackendTypeSubstitutionKey BackendType
constructorTypeVars dataTypeParams0 forallViews =
  foldr insertOne Map.empty (dataParamVars ++ forallVars)
  where
    dataTypeParamVar (name, identity) =
      (Just identity, [name], backendTy)
      where
        backendTy = BTVarWithIdentity (Just identity) name

    forallVar (name, identity, _) =
      (Just identity, [name], backendTy)
      where
        backendTy = BTVarWithIdentity (Just identity) name

    dataParamVars =
      map dataTypeParamVar dataTypeParams0

    forallVars =
      map forallVar forallViews

    insertOne (identity, names, backendTy) =
      insertConstructorTypeVarAliases identity names backendTy

constructorTypeBinderIdentities :: [(String, TypeBinderIdentity)] -> [(String, TypeBinderIdentity, Maybe TypeView)] -> Map String TypeBinderIdentity
constructorTypeBinderIdentities dataTypeParams0 forallViews =
  Map.fromList (dataParamIdentities ++ forallIdentities)
  where
    dataParamIdentities =
      [ (name, identity)
      | (paramName, identity) <- dataTypeParams0,
        name <- [paramName]
      ]

    forallIdentities =
      [ (binderName, identity)
      | (name, identity, _) <- forallViews,
        binderName <- [name]
      ]

convertConstructorForallView ::
  ElaborateScope ->
  Map BackendTypeSubstitutionKey BackendType ->
  (TypeView -> TypeView) ->
  (String, TypeBinderIdentity, Maybe TypeView) ->
  Either BackendConversionError BackendTypeBinder
convertConstructorForallView scope typeVars attachBinderIdentities (name, identity, mbBound) =
  BackendTypeBinderWithIdentity (Just identity) name
    <$> traverse (convertConstructorTypeView scope typeVars . attachBinderIdentities) mbBound

convertConstructorTypeView :: ElaborateScope -> Map BackendTypeSubstitutionKey BackendType -> TypeView -> Either BackendConversionError BackendType
convertConstructorTypeView scope typeVars view =
  applyConstructorTypeBinderIdentities typeVars (typeViewIdentity view) <$> convertLoweredTypeView scope view

applyConstructorTypeBinderIdentities :: Map BackendTypeSubstitutionKey BackendType -> SrcType -> BackendType -> BackendType
applyConstructorTypeBinderIdentities typeVars sourceTy backendTy =
  case (sourceTy, backendTy) of
    (STVar sourceName, BTVarWithIdentity identity backendName) ->
      case lookupSourceTypeVar identity backendName sourceName typeVars of
        Just replacement -> replacement
        Nothing -> backendTy
    (STVar name, _) ->
      case lookupConstructorTypeVar Nothing name typeVars of
        Just replacement -> replacement
        Nothing -> backendTy
    (STVarApp sourceName sourceArgs, BTVarAppWithIdentity identity backendName backendArgs)
      | Just headTy <- lookupSourceTypeVar identity backendName sourceName typeVars ->
          applyBackendHeadLocal headTy (zipWithNE (applyConstructorTypeBinderIdentities typeVars) sourceArgs backendArgs)
    (STArrow sourceDom sourceCod, BTArrow backendDom backendCod) ->
      BTArrow
        (applyConstructorTypeBinderIdentities typeVars sourceDom backendDom)
        (applyConstructorTypeBinderIdentities typeVars sourceCod backendCod)
    (STForall sourceName sourceBound sourceBody, BTForallWithIdentity identity name backendBound backendTyBody) ->
      BTForallWithIdentity
        identity
        name
        (applyConstructorTypeBinderIdentities typeVars (maybe STBottom unSrcBound sourceBound) <$> backendBound)
        (applyConstructorTypeBinderIdentities (deleteSourceTypeVarAliases identity [sourceName, name] typeVars) sourceBody backendTyBody)
    (STMu sourceName sourceBody, BTMuWithIdentity identity name backendTyBody) ->
      BTMuWithIdentity identity name (applyConstructorTypeBinderIdentities (deleteSourceTypeVarAliases identity [sourceName, name] typeVars) sourceBody backendTyBody)
    (STCon _ sourceArgs, BTConWithIdentity identity name backendArgs) ->
      BTConWithIdentity identity name (zipWithNE (applyConstructorTypeBinderIdentities typeVars) sourceArgs backendArgs)
    _ ->
      applyConstructorTypeVarNames typeVars backendTy

applyConstructorTypeVarNames :: Map BackendTypeSubstitutionKey BackendType -> BackendType -> BackendType
applyConstructorTypeVarNames typeVars =
  \case
    BTVarWithIdentity identity name ->
      Map.findWithDefault (BTVarWithIdentity identity name) (backendTypeSubstitutionKeyFor identity name) typeVars
    BTArrow dom cod ->
      BTArrow
        (applyConstructorTypeVarNames typeVars dom)
        (applyConstructorTypeVarNames typeVars cod)
    BTBaseWithIdentity identity name ->
      BTBaseWithIdentity identity name
    BTConWithIdentity identity name args ->
      BTConWithIdentity identity name (fmap (applyConstructorTypeVarNames typeVars) args)
    BTVarAppWithIdentity identity name args
      | Just headTy <- lookupConstructorTypeVar identity name typeVars ->
          applyBackendHeadLocal headTy (fmap (applyConstructorTypeVarNames typeVars) args)
    BTVarAppWithIdentity identity name args ->
      BTVarAppWithIdentity identity name (fmap (applyConstructorTypeVarNames typeVars) args)
    BTForallWithIdentity identity name mb body ->
      BTForallWithIdentity
        identity
        name
        (applyConstructorTypeVarNames typeVars <$> mb)
        (applyConstructorTypeVarNames (deleteConstructorTypeVarAliases identity [name] typeVars) body)
    BTMuWithIdentity identity name body ->
      BTMuWithIdentity identity name (applyConstructorTypeVarNames (deleteConstructorTypeVarAliases identity [name] typeVars) body)
    BTBottom ->
      BTBottom

insertConstructorTypeVarAliases :: Maybe TypeBinderIdentity -> [String] -> BackendType -> Map BackendTypeSubstitutionKey BackendType -> Map BackendTypeSubstitutionKey BackendType
insertConstructorTypeVarAliases identity names backendTy typeVars =
  foldr (`Map.insert` backendTy) typeVars (constructorTypeVarAliasKeys identity names)

insertSourceTypeVarAliases :: Maybe TypeBinderIdentity -> [String] -> BackendType -> Map BackendTypeSubstitutionKey BackendType -> Map BackendTypeSubstitutionKey BackendType
insertSourceTypeVarAliases identity names backendTy typeVars =
  insertConstructorTypeVarAliases identity names backendTy typeVars

deleteConstructorTypeVarAliases :: Maybe TypeBinderIdentity -> [String] -> Map BackendTypeSubstitutionKey BackendType -> Map BackendTypeSubstitutionKey BackendType
deleteConstructorTypeVarAliases identity names typeVars =
  foldr Map.delete typeVars (constructorTypeVarAliasKeys identity names)

deleteSourceTypeVarAliases :: Maybe TypeBinderIdentity -> [String] -> Map BackendTypeSubstitutionKey BackendType -> Map BackendTypeSubstitutionKey BackendType
deleteSourceTypeVarAliases identity names typeVars =
  deleteConstructorTypeVarAliases identity names typeVars

lookupConstructorTypeVar :: Maybe TypeBinderIdentity -> String -> Map BackendTypeSubstitutionKey BackendType -> Maybe BackendType
lookupConstructorTypeVar identity name typeVars =
  Map.lookup (backendTypeSubstitutionKeyFor identity name) typeVars

lookupSourceTypeVar :: Maybe TypeBinderIdentity -> String -> String -> Map BackendTypeSubstitutionKey BackendType -> Maybe BackendType
lookupSourceTypeVar identity backendName sourceName typeVars =
  case identity of
    Just {} ->
      lookupConstructorTypeVar identity backendName typeVars
    Nothing ->
      lookupConstructorTypeVar Nothing sourceName typeVars

constructorTypeVarAliasKeys :: Maybe TypeBinderIdentity -> [String] -> [BackendTypeSubstitutionKey]
constructorTypeVarAliasKeys identity =
  map keyFor
  where
    keyFor =
      backendTypeSubstitutionKeyFor identity

zipWithNE :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWithNE f (a :| as) (b :| bs) =
  f a b :| zipWith f as bs

applyBackendHeadLocal :: BackendType -> NonEmpty BackendType -> BackendType
applyBackendHeadLocal headTy args =
  case headTy of
    BTVarWithIdentity identity name -> BTVarAppWithIdentity identity name args
    BTVarAppWithIdentity identity name existingArgs -> BTVarAppWithIdentity identity name (existingArgs <> args)
    _ -> headTy

convertLoweredTypeView :: ElaborateScope -> TypeView -> Either BackendConversionError BackendType
convertLoweredTypeView scope view =
  convertSourceTypeViewWithIdentities (lowerTypeViewForScope scope view)

lowerTypeViewForScope :: ElaborateScope -> TypeView -> TypeView
lowerTypeViewForScope scope view =
  view
    { typeViewDisplay = loweredDisplay,
      typeViewIdentity = loweredIdentity,
      typeViewHeadIdentities = mergeSymbolIdentityMaps [typeViewHeadIdentities view, typeHeadIdentitiesInScope scope],
      typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ typeViewBinderIdentities view,
            sourceTypeBinderIdentitiesInScope scope loweredDisplay,
            sourceTypeBinderIdentitiesInScope scope loweredIdentity
          ]
    }
  where
    loweredDisplay = lowerTypeView scope view
    loweredIdentity = lowerType scope (typeViewIdentity view)

convertSourceTypeViewWithIdentities :: TypeView -> Either BackendConversionError BackendType
convertSourceTypeViewWithIdentities view =
  case go Map.empty Map.empty (typeViewDisplay view) (typeViewIdentity view) of
    Right backendTy -> Right backendTy
    Left _ -> convertSourceTypeWithTypeViewIdentities view (typeViewDisplay view)
  where
    go binderNames binderIdentities display identityTy0 =
      case (display, identityTy0) of
        (STVar displayName, STVar identityName) ->
          Right (BTVarWithIdentity (lookupBinderOccurrenceIdentity binderNames binderIdentities identityName displayName) (binderDisplayName binderNames identityName displayName))
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          BTArrow
            <$> go binderNames binderIdentities displayDom identityDom
            <*> go binderNames binderIdentities displayCod identityCod
        (STBase displayName, STBase identityName) ->
          Right (BTBaseWithIdentity (sourceTypeHeadIdentityFromNames identityName displayName) (backendBaseTy displayName))
        (STCon displayName displayArgs, STCon identityName identityArgs) ->
          BTConWithIdentity (sourceTypeHeadIdentityFromNames identityName displayName) (backendBaseTy displayName)
            <$> zipWithNEEither (go binderNames binderIdentities) displayArgs identityArgs
        (STVarApp displayName displayArgs, STVarApp identityName identityArgs) ->
          BTVarAppWithIdentity (lookupBinderOccurrenceIdentity binderNames binderIdentities identityName displayName) (binderDisplayName binderNames identityName displayName)
            <$> zipWithNEEither (go binderNames binderIdentities) displayArgs identityArgs
        (STForall displayName displayBound displayBody, STForall identityName identityBound identityBody) ->
          let identity = lookupBinderDeclarationIdentity binderNames identityName displayName
              binderNames' = Map.insert identityName displayName binderNames
              binderIdentities' = insertBinderIdentityAliases identityName displayName identity binderIdentities
           in BTForallWithIdentity identity displayName
                <$> zipMaybeBounds (go binderNames binderIdentities) displayBound identityBound
                <*> go binderNames' binderIdentities' displayBody identityBody
        (STMu displayName displayBody, STMu identityName identityBody) ->
          let identity = lookupBinderDeclarationIdentity binderNames identityName displayName
              binderNames' = Map.insert identityName displayName binderNames
              binderIdentities' = insertBinderIdentityAliases identityName displayName identity binderIdentities
           in BTMuWithIdentity identity displayName <$> go binderNames' binderIdentities' displayBody identityBody
        (STBottom, STBottom) ->
          Right BTBottom
        _ ->
          Left (BackendUnsupportedCaseShape "type view display and identity shapes differ")

    lookupBinderIdentity binderIdentities identityName displayName =
      Map.lookup identityName binderIdentities
        <|> typeViewBinderIdentityForAlias view identityName
        <|> if identityName == displayName
          then Map.lookup displayName binderIdentities <|> typeViewBinderIdentityForAlias view displayName
          else Nothing

    lookupBinderOccurrenceIdentity binderNames binderIdentities identityName displayName =
      Map.lookup identityName binderIdentities
        <|> if displayName `elem` Map.elems binderNames
          then Nothing
          else lookupBinderIdentity binderIdentities identityName displayName

    lookupBinderDeclarationIdentity binderNames identityName displayName =
      lookupBinderIdentityByMetadata identityName
        <|> if displayName `elem` Map.elems binderNames
          then Nothing
          else lookupBinderIdentityByMetadata displayName

    lookupBinderIdentityByMetadata name =
      typeViewBinderIdentityForAlias view name

    insertBinderIdentityAliases identityName displayName identity binderIdentities =
      case identity of
        Just resolvedIdentity ->
          foldr (`Map.insert` resolvedIdentity) binderIdentities [identityName, displayName]
        Nothing ->
          binderIdentities

    binderDisplayName binderNames identityName displayName =
      Map.findWithDefault displayName identityName binderNames

    sourceTypeHeadIdentityFromNames identityName displayName =
      typeViewHeadIdentityForAlias view identityName
        <|> builtinTypeHeadIdentity identityName
        <|> builtinTypeHeadIdentity displayName

    zipMaybeBounds _ Nothing Nothing =
      Right Nothing
    zipMaybeBounds convert (Just (SrcBound displayBound)) (Just (SrcBound identityBound)) =
      Just <$> convert displayBound identityBound
    zipMaybeBounds _ _ _ =
      Left (BackendUnsupportedCaseShape "type view bound display and identity shapes differ")

zipWithNEEither :: (a -> b -> Either BackendConversionError c) -> NonEmpty a -> NonEmpty b -> Either BackendConversionError (NonEmpty c)
zipWithNEEither f (a :| as) (b :| bs) =
  (:|)
    <$> f a b
    <*> zipWithMEither f as bs

zipWithMEither :: (a -> b -> Either BackendConversionError c) -> [a] -> [b] -> Either BackendConversionError [c]
zipWithMEither _ [] [] =
  Right []
zipWithMEither f (a : as) (b : bs) =
  (:) <$> f a b <*> zipWithMEither f as bs
zipWithMEither _ _ _ =
  Left (BackendUnsupportedCaseShape "type view argument display and identity shapes differ")

typeHeadIdentitiesInScope :: ElaborateScope -> Map String SymbolIdentity
typeHeadIdentitiesInScope scope =
  mergeSymbolIdentityMaps
    [ Map.map dataInfoSymbol dataTypes,
      unambiguousDataTypeHeadIdentities dataTypes
    ]
  where
    dataTypes = elaborateScopeDataTypes scope

unambiguousDataTypeHeadIdentities :: Map String DataInfo -> Map String SymbolIdentity
unambiguousDataTypeHeadIdentities =
  symbolIdentityAliasMap . map dataInfoSymbol . Map.elems

convertSourceType :: SrcType -> Either BackendConversionError BackendType
convertSourceType =
  convertSourceTypeWithHeadIdentities Map.empty

convertSourceTypeWithTypeViewIdentities :: TypeView -> SrcType -> Either BackendConversionError BackendType
convertSourceTypeWithTypeViewIdentities view =
  go Map.empty
  where
    go binderIdentities =
      \case
        STVar name -> Right (BTVarWithIdentity (sourceTypeBinderIdentity binderIdentities name) name)
        STArrow dom cod ->
          BTArrow
            <$> go binderIdentities dom
            <*> go binderIdentities cod
        STBase name ->
          Right (BTBaseWithIdentity (sourceTypeHeadIdentity name) (backendBaseTy name))
        STCon name args ->
          BTConWithIdentity (sourceTypeHeadIdentity name) (backendBaseTy name)
            <$> traverse (go binderIdentities) args
        STVarApp name args ->
          BTVarAppWithIdentity (sourceTypeBinderIdentity binderIdentities name) name
            <$> traverse (go binderIdentities) args
        STTyLam {} -> Left (BackendUnsupportedCaseShape "residual type lambda reached backend type conversion")
        STTyApp {} -> Left (BackendUnsupportedCaseShape "residual type application reached backend type conversion")
        STForall name mb body ->
          let identity = sourceTypeBinderDeclarationIdentity binderIdentities name
              binderIdentities' = Map.insert name identity binderIdentities
           in BTForallWithIdentity identity name
                <$> traverse (go binderIdentities . unSrcBound) mb
                <*> go binderIdentities' body
        STMu name body ->
          let identity = sourceTypeBinderDeclarationIdentity binderIdentities name
              binderIdentities' = Map.insert name identity binderIdentities
           in BTMuWithIdentity identity name <$> go binderIdentities' body
        STBottom -> Right BTBottom
    sourceTypeHeadIdentity name =
      typeViewHeadIdentityForAlias view name <|> builtinTypeHeadIdentity name

    sourceTypeBinderIdentity binderIdentities name =
      case Map.lookup name binderIdentities of
        Just identity -> identity
        Nothing -> typeViewBinderIdentityForAlias view name

    sourceTypeBinderDeclarationIdentity binderIdentities name =
      if Map.member name binderIdentities
        then Nothing
        else typeViewBinderIdentityForAlias view name

convertSourceTypeWithHeadIdentities :: Map String SymbolIdentity -> SrcType -> Either BackendConversionError BackendType
convertSourceTypeWithHeadIdentities headIdentities0 =
  \case
    STVar name -> Right (BTVarWithIdentity (sourceTypeBinderIdentity name) name)
    STArrow dom cod ->
      BTArrow
        <$> convertSourceTypeWithHeadIdentities headIdentities0 dom
        <*> convertSourceTypeWithHeadIdentities headIdentities0 cod
    STBase name ->
      Right (BTBaseWithIdentity (sourceTypeHeadIdentity name) (backendBaseTy name))
    STCon name args ->
      BTConWithIdentity (sourceTypeHeadIdentity name) (backendBaseTy name)
        <$> traverse (convertSourceTypeWithHeadIdentities headIdentities0) args
    STVarApp name args ->
      BTVarAppWithIdentity (sourceTypeBinderIdentity name) name <$> traverse (convertSourceTypeWithHeadIdentities headIdentities0) args
    STTyLam {} -> Left (BackendUnsupportedCaseShape "residual type lambda reached backend type conversion")
    STTyApp {} -> Left (BackendUnsupportedCaseShape "residual type application reached backend type conversion")
    STForall name mb body ->
      BTForallWithIdentity (sourceTypeBinderIdentity name) name
        <$> traverse (convertSourceTypeWithHeadIdentities headIdentities0 . unSrcBound) mb
        <*> convertSourceTypeWithHeadIdentities headIdentities0 body
    STMu name body -> BTMuWithIdentity (sourceTypeBinderIdentity name) name <$> convertSourceTypeWithHeadIdentities headIdentities0 body
    STBottom -> Right BTBottom
  where
    sourceTypeHeadIdentity name =
      lookupSymbolIdentityAlias headIdentities0 name <|> builtinTypeHeadIdentity name

    sourceTypeBinderIdentity _ =
      Nothing

type BackendTypeBinderNames = Map TypeBinderIdentity String

convertElabType :: ElabType -> Either BackendConversionError BackendType
convertElabType ty =
  convertElabTypeWith (canonicalBackendTypeBinderNames ty) ty

convertElabTypeWith :: BackendTypeBinderNames -> ElabType -> Either BackendConversionError BackendType
convertElabTypeWith names =
  \case
    TVarRef ref -> Right (BTVarWithIdentity (Just (typeBinderRefIdentity ref)) (backendTypeVarName names ref))
    TArrow dom cod -> BTArrow <$> convertElabTypeWith names dom <*> convertElabTypeWith names cod
    TConWithIdentity identity (BaseTy name) args ->
      BTConWithIdentity (identity <|> builtinTypeHeadIdentity name) (backendBaseTy name) <$> traverse (convertElabTypeWith names) args
    TVarAppRef ref args ->
      BTVarAppWithIdentity (Just (typeBinderRefIdentity ref)) (backendTypeVarName names ref) <$> traverse (convertElabTypeWith names) args
    TBaseWithIdentity identity (BaseTy name) -> Right (BTBaseWithIdentity (identity <|> builtinTypeHeadIdentity name) (backendBaseTy name))
    TForallRef ref mb body ->
      BTForallWithIdentity (Just (typeBinderRefIdentity ref)) (backendTypeVarName names ref)
            <$> traverse (convertElabTypeWith names . tyToElab) mb
            <*> convertElabTypeWith names body
    TMuRef ref body ->
      BTMuWithIdentity (Just (typeBinderRefIdentity ref)) (backendTypeVarName names ref) <$> convertElabTypeWith names body
    TBottom -> Right BTBottom

backendTypeVarName :: BackendTypeBinderNames -> TypeBinderRef -> String
backendTypeVarName names ref =
  Map.findWithDefault (typeBinderRefName ref) (typeBinderRefIdentity ref) names

canonicalBackendTypeBinderNames :: ElabType -> BackendTypeBinderNames
canonicalBackendTypeBinderNames ty =
  canonicalBackendTypeBinderNamesFromRefs (elabTypeBinderRefs ty)

canonicalBackendTypeBinderNamesFromRefs :: [TypeBinderRef] -> BackendTypeBinderNames
canonicalBackendTypeBinderNamesFromRefs refs =
  fst (foldl assign (Map.empty, Set.empty) refsByIdentity)
  where
    refsByIdentity =
      Map.toList $
        Map.fromListWith (++)
          [ (typeBinderRefIdentity ref, [typeBinderRefName ref])
          | ref <- refs
          ]

    assign (names, used) (identity, refNames) =
      case sortOn (\candidate -> (length candidate, candidate)) refNames of
        [] -> (names, used)
        preferred : _ ->
          let backendName = freshNameLike preferred used
           in (Map.insert identity backendName names, Set.insert backendName used)

elabTypeBinderRefs :: ElabType -> [TypeBinderRef]
elabTypeBinderRefs =
  \case
    TVarRef ref -> [ref]
    TArrow dom cod -> elabTypeBinderRefs dom ++ elabTypeBinderRefs cod
    TCon _ args -> concatMap elabTypeBinderRefs (NE.toList args)
    TVarAppRef ref args -> ref : concatMap elabTypeBinderRefs (NE.toList args)
    TBase {} -> []
    TForallRef ref mb body -> ref : maybe [] (elabTypeBinderRefs . tyToElab) mb ++ elabTypeBinderRefs body
    TMuRef ref body -> ref : elabTypeBinderRefs body
    TBottom -> []

backendBaseTy :: String -> BaseTy
backendBaseTy name =
  BaseTy (normalizeBuiltinTypeReference name)

normalizeBuiltinElabType :: Ty v -> Ty v
normalizeBuiltinElabType =
  \case
    TVarRef ref -> TVarRef ref
    TArrow dom cod -> TArrow (normalizeBuiltinElabType dom) (normalizeBuiltinElabType cod)
    TConWithIdentity identity (BaseTy name) args -> TConWithIdentity identity (backendBaseTy name) (fmap normalizeBuiltinElabType args)
    TVarAppRef ref args -> TVarAppRef ref (fmap normalizeBuiltinElabType args)
    TBaseWithIdentity identity (BaseTy name) -> TBaseWithIdentity identity (backendBaseTy name)
    TForallRef ref mb body ->
      TForallRef ref (fmap normalizeBuiltinElabType mb) (normalizeBuiltinElabType body)
    TMuRef ref body -> TMuRef ref (normalizeBuiltinElabType body)
    TBottom -> TBottom

normalizeBuiltinElabScheme :: ElabScheme -> ElabScheme
normalizeBuiltinElabScheme =
  schemeFromType . normalizeBuiltinElabType . schemeToType

normalizeBuiltinInstantiation :: Instantiation -> Instantiation
normalizeBuiltinInstantiation =
  \case
    InstId -> InstId
    InstApp ty -> InstApp (normalizeBuiltinElabType ty)
    InstBot ty -> InstBot (normalizeBuiltinElabType ty)
    InstIntro -> InstIntro
    InstElim -> InstElim
    InstAbstrRef ref -> InstAbstrRef ref
    InstUnderRef ref inst -> InstUnderRef ref (normalizeBuiltinInstantiation inst)
    InstInside inst -> InstInside (normalizeBuiltinInstantiation inst)
    InstSeq left right -> InstSeq (normalizeBuiltinInstantiation left) (normalizeBuiltinInstantiation right)

normalizeBuiltinXmlfTerm :: XmlfTerm -> XmlfTerm
normalizeBuiltinXmlfTerm =
  \case
    EVarNode resolved ->
      EVarNode (mapResolvedVarType normalizeBuiltinElabType resolved)
    ELit lit -> ELit lit
    ELam resolved body ->
      ELam
        (mapResolvedVarType normalizeBuiltinElabType resolved)
        (normalizeBuiltinXmlfTerm body)
    EApp fun arg -> EApp (normalizeBuiltinXmlfTerm fun) (normalizeBuiltinXmlfTerm arg)
    ELet resolved scheme rhs body ->
      ELet
        (mapResolvedVarType normalizeBuiltinElabType resolved)
        (normalizeBuiltinElabScheme scheme)
        (normalizeBuiltinXmlfTerm rhs)
        (normalizeBuiltinXmlfTerm body)
    ETyAbsRef ref mbBound body ->
      ETyAbsRef ref (fmap normalizeBuiltinElabType mbBound) (normalizeBuiltinXmlfTerm body)
    ETyInst inner inst ->
      ETyInst (normalizeBuiltinXmlfTerm inner) (normalizeBuiltinInstantiation inst)
    ERoll ty body -> ERoll (normalizeBuiltinElabType ty) (normalizeBuiltinXmlfTerm body)
    EUnroll body -> EUnroll (normalizeBuiltinXmlfTerm body)

normalizeBuiltinEnv :: Env -> Env
normalizeBuiltinEnv env =
  Env
    { typeEnv = Map.map normalizeBuiltinElabType (typeEnv env),
      resolvedTermEnv =
        resolvedTermEnvFromList
          [ (mapResolvedVarType normalizeBuiltinElabType resolved, normalizeBuiltinElabType ty)
          | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv env)
          ]
    }

backendTypeToElabType :: BackendType -> Maybe ElabType
backendTypeToElabType ty =
  backendTypeToElabTypeWithGenerator (identityGeneratorAfter (generatedIdentitiesInBackendTypes [ty])) ty

backendTypeToElabTypeSeededByElabType :: ElabType -> BackendType -> Maybe ElabType
backendTypeToElabTypeSeededByElabType seedTy ty =
  backendTypeToElabTypeWithGenerator
    (advanceIdentityGeneratorPastMany (generatedIdentitiesInBackendTypes [ty]) (identityGeneratorAfterType seedTy))
    ty

advanceIdentityGeneratorPastMany :: [UniqueIdentity] -> IdentityGenerator -> IdentityGenerator
advanceIdentityGeneratorPastMany identities generator =
  foldr advanceIdentityGeneratorPast generator identities

backendTypeToElabTypeWithGenerator :: IdentityGenerator -> BackendType -> Maybe ElabType
backendTypeToElabTypeWithGenerator generator0 ty =
  let (refs, generator) = backendTypeBinderRefs (Set.toList (freeBackendTypeVarRefs ty)) generator0
   in fst <$> backendTypeToElabTypeWith refs generator ty

backendTypeBinderRefs :: [BackendDataParameterRef] -> IdentityGenerator -> (Map BackendTypeSubstitutionKey TypeBinderRef, IdentityGenerator)
backendTypeBinderRefs refs0 generator0 =
  go refs0 Map.empty generator0
  where
    go [] refs generator = (refs, generator)
    go (backendRef : rest) refs generator =
      case backendDataParameterRefIdentity backendRef of
        Just identity ->
          let name = backendDataParameterRefName backendRef
              ref = typeBinderRefFromIdentity identity name
           in go rest (Map.insert (backendTypeSubstitutionKeyFromIdentity identity) ref refs) generator
        Nothing ->
          go rest refs generator

backendTypeToElabTypeWith :: Map BackendTypeSubstitutionKey TypeBinderRef -> IdentityGenerator -> BackendType -> Maybe (ElabType, IdentityGenerator)
backendTypeToElabTypeWith refs generator =
  \case
    BTVarWithIdentity identity name -> do
      ref <- backendTypeBinderRefWithIdentity refs identity name
      Just (TVarRef ref, generator)
    BTArrow dom cod -> do
      (dom', generator1) <- backendTypeToElabTypeWith refs generator dom
      (cod', generator2) <- backendTypeToElabTypeWith refs generator1 cod
      Just (TArrow dom' cod', generator2)
    BTBaseWithIdentity identity name -> Just (TBaseWithIdentity identity name, generator)
    BTConWithIdentity identity name args -> do
      (args', generator') <- backendTypesToElabTypesWith refs generator args
      Just (TConWithIdentity identity name args', generator')
    BTVarAppWithIdentity identity name args -> do
      (args', generator') <- backendTypesToElabTypesWith refs generator args
      ref <- backendTypeBinderRefWithIdentity refs identity name
      Just (TVarAppRef ref args', generator')
    BTForallWithIdentity identity name mb body ->
      let (ref, generator1) = backendTypeBinderRefForBinder identity name generator
          refs' = insertBackendTypeBinderRef identity name ref refs
       in do
            (mb', generator2) <- maybe (Just (Nothing, generator1)) (backendTypeToBoundTypeWith refs generator1) mb
            (body', generator3) <- backendTypeToElabTypeWith refs' generator2 body
            Just (TForallRef ref mb' body', generator3)
    BTMuWithIdentity identity name body ->
      let (ref, generator1) = backendTypeBinderRefForBinder identity name generator
       in do
            (body', generator2) <- backendTypeToElabTypeWith (insertBackendTypeBinderRef identity name ref refs) generator1 body
            Just (TMuRef ref body', generator2)
    BTBottom -> Just (TBottom, generator)

insertBackendTypeBinderRef :: Maybe TypeBinderIdentity -> String -> TypeBinderRef -> Map BackendTypeSubstitutionKey TypeBinderRef -> Map BackendTypeSubstitutionKey TypeBinderRef
insertBackendTypeBinderRef identity name ref refs =
  Map.insert (backendTypeSubstitutionKeyFor identity name) ref refs

backendTypeBinderRefWithIdentity :: Map BackendTypeSubstitutionKey TypeBinderRef -> Maybe TypeBinderIdentity -> String -> Maybe TypeBinderRef
backendTypeBinderRefWithIdentity refs identity name =
  backendTypeBinderRef refs identity name <|> fallbackIdentityRef
  where
    fallbackIdentityRef =
      case identity of
        Just knownIdentity -> Just (typeBinderRefFromIdentity knownIdentity name)
        Nothing -> Nothing

backendTypeBinderRefForBinder :: Maybe TypeBinderIdentity -> String -> IdentityGenerator -> (TypeBinderRef, IdentityGenerator)
backendTypeBinderRefForBinder (Just identity) name generator =
  (typeBinderRefFromIdentity identity name, generator)
backendTypeBinderRefForBinder Nothing name generator =
  sourceTypeBinderRefForName name generator

backendTypeBinderRef :: Map BackendTypeSubstitutionKey TypeBinderRef -> Maybe TypeBinderIdentity -> String -> Maybe TypeBinderRef
backendTypeBinderRef env identity name =
  Map.lookup (backendTypeSubstitutionKeyFor identity name) env

backendTypesToElabTypesWith :: Map BackendTypeSubstitutionKey TypeBinderRef -> IdentityGenerator -> NonEmpty BackendType -> Maybe (NonEmpty ElabType, IdentityGenerator)
backendTypesToElabTypesWith refs0 generator0 (arg :| args) = do
  (arg', generator1) <- backendTypeToElabTypeWith refs0 generator0 arg
  (argsRev, generator') <-
    foldM
      ( \(acc, gen) next -> do
          (next', gen') <- backendTypeToElabTypeWith refs0 gen next
          Just (next' : acc, gen')
      )
      ([], generator1)
      args
  Just (arg' :| reverse argsRev, generator')

backendTypeToBoundTypeWith :: Map BackendTypeSubstitutionKey TypeBinderRef -> IdentityGenerator -> BackendType -> Maybe (Maybe BoundType, IdentityGenerator)
backendTypeToBoundTypeWith refs generator ty = do
  (elabTy, generator') <- backendTypeToElabTypeWith refs generator ty
  boundTy <- either (const Nothing) Just (elabToBound elabTy)
  Just (Just boundTy, generator')

canonicalizeBackendType :: ConvertContext -> BackendType -> BackendType
canonicalizeBackendType context =
  canonicalizeDataResult . canonicalizeSourceBackendTypeHeads (ccDataByIdentity context) . go
  where
    go =
      \case
        BTArrow dom cod ->
          BTArrow (go dom) (go cod)
        BTBaseWithIdentity identity (BaseTy name) ->
          BTBaseWithIdentity (identity <|> builtinTypeHeadIdentity name) (backendBaseTy name)
        BTConWithIdentity identity (BaseTy name) args ->
          BTConWithIdentity (identity <|> builtinTypeHeadIdentity name) (backendBaseTy name) (fmap go args)
        BTVarAppWithIdentity identity name args ->
          BTVarAppWithIdentity identity name (fmap go args)
        BTForallWithIdentity identity name mb body ->
          BTForallWithIdentity identity name (fmap go mb) (go body)
        BTMuWithIdentity identity name body ->
          BTMuWithIdentity identity name (go body)
        ty ->
          ty

    canonicalizeDataResult ty =
      if identityBearingDataTypeHead ty
        then ty
        else
          case exactMatches of
            [candidate] -> candidate
            _ ->
              case structuralMatches of
                [candidate] -> candidate
                _ -> ty
      where
        candidates = candidateDataResultTypes context ty
        exactMatches = [candidate | candidate <- candidates, candidate == ty]
        structuralMatches = candidates

    identityBearingDataTypeHead =
      \case
        BTBaseWithIdentity (Just identity) _ -> maybe False preludePrimitiveDataMeta (Map.lookup identity (ccDataByIdentity context))
        BTConWithIdentity (Just identity) _ _ -> maybe False preludePrimitiveDataMeta (Map.lookup identity (ccDataByIdentity context))
        _ -> False

candidateDataResultTypes :: ConvertContext -> BackendType -> [BackendType]
candidateDataResultTypes context ty =
  nub
    [ substituteBackendTypesByKey completed (backendConstructorResult constructor)
    | dataMeta <- ccData context,
      let dataDecl = dmBackend dataMeta,
      constructor <- backendDataConstructors dataDecl,
      let parameters = constructorTypeParameterBoundsFor dataDecl constructor,
      Just substitution <- [matchBackendTypeParametersWithDataIdentity (backendDataIdentity dataDecl) Map.empty (backendDataParameterRefs dataDecl) parameters Map.empty (backendConstructorResult constructor) ty],
      let completed =
            completeDataParameterSubstitution dataDecl $
              completeBackendParameterSubstitution parameters substitution
    ]

convertTerm :: ConvertContext -> Env -> ClosureScope -> XmlfTerm -> ConvertM BackendExpr
convertTerm context env scope =
  convertTermExpectedMode DirectLambda context env scope Nothing

convertTermExpectedMode :: LambdaMode -> ConvertContext -> Env -> ClosureScope -> Maybe BackendType -> XmlfTerm -> ConvertM BackendExpr
convertTermExpectedMode =
  convertTermExpectedModeWith AllowPartialApplications

convertTermExpectedModeNoPartial :: LambdaMode -> ConvertContext -> Env -> ClosureScope -> Maybe BackendType -> XmlfTerm -> ConvertM BackendExpr
convertTermExpectedModeNoPartial =
  convertTermExpectedModeWith SuppressPartialApplications

convertTermExpectedModeWith :: PartialApplicationMode -> LambdaMode -> ConvertContext -> Env -> ClosureScope -> Maybe BackendType -> XmlfTerm -> ConvertM BackendExpr
convertTermExpectedModeWith partialMode mode context env scope mbExpectedTy term =
  case mbExpectedTy of
    Just resultTy0 ->
      let resultTy = canonicalizeBackendType context resultTy0
       in convertSpecialTerm partialMode mode context env scope term resultTy
            >>= \case
              Just expr -> pure expr
              Nothing -> convertOrdinaryTerm mode context env scope term resultTy
    Nothing -> do
      resultTy <- canonicalizeBackendType context <$> liftEitherConvert (inferBackendType context env term)
      convertSpecialTerm partialMode mode context env scope term resultTy
        >>= \case
          Just expr -> pure expr
          Nothing -> convertOrdinaryTerm mode context env scope term resultTy

convertSpecialTerm ::
  PartialApplicationMode ->
  LambdaMode ->
  ConvertContext ->
  Env ->
  ClosureScope ->
  XmlfTerm ->
  BackendType ->
  ConvertM (Maybe BackendExpr)
convertSpecialTerm partialMode mode context env scope term resultTy =
  convertCaseApplication mode context env scope term resultTy
    >>= \case
      Just expr -> pure (Just expr)
      Nothing ->
        convertConstructorApplication mode context env scope term resultTy
          >>= \case
            Just expr -> pure (Just expr)
            Nothing ->
              case partialMode of
                AllowPartialApplications -> convertPartialApplication context env scope term resultTy
                SuppressPartialApplications -> pure Nothing

convertPartialApplication ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  XmlfTerm ->
  BackendType ->
  ConvertM (Maybe BackendExpr)
convertPartialApplication context env scope term resultTy =
  case collectApps term of
    (rawHeadTerm, rawSuppliedArgs) ->
      case normalizePartialApplicationSpine rawHeadTerm rawSuppliedArgs of
        (headTerm, suppliedArgs)
          | not (null suppliedArgs),
            not (isConstructorHeadTerm context headTerm) -> do
              headTy <- normalizeBackendTypeForContext context <$> liftEitherConvert (inferBackendType context env headTerm)
              let (paramTys, finalTy) = splitBackendArrows headTy
                  suppliedCount = length suppliedArgs
                  suppliedParamTys = take suppliedCount paramTys
                  remainingParamTys = drop suppliedCount paramTys
                  expectedPartialTy = foldr BTArrow finalTy remainingParamTys
              if suppliedCount < length paramTys
                && alphaEqBackendType resultTy expectedPartialTy
                && not (null remainingParamTys)
                && partialApplicationSuppliesValueArgument context scope headTerm suppliedCount
                then do
                  suppliedArgExprs <-
                    zipWithM
                      ( \index0 (paramTy, arg) ->
                          convertPartialApplicationArgument context env scope headTerm index0 paramTy arg
                      )
                      [0 :: Int ..]
                      (zip suppliedParamTys suppliedArgs)
                  if partialApplicationCanCaptureSuppliedArgs context scope headTerm (zip3 [0 :: Int ..] suppliedParamTys suppliedArgExprs)
                    then Just <$> packagePartialApplication context env scope resultTy headTerm headTy suppliedParamTys suppliedArgExprs remainingParamTys finalTy
                    else pure Nothing
                else pure Nothing
        _ -> pure Nothing

normalizePartialApplicationSpine :: XmlfTerm -> [XmlfTerm] -> (XmlfTerm, [XmlfTerm])
normalizePartialApplicationSpine headTerm suppliedArgs =
  case (headTerm, suppliedArgs) of
    (ELam resolved body, arg : rest) ->
      normalizePartialApplicationSpine (replaceFreeTermVariable (TermVarResolved resolved) arg body) rest
    _ ->
      case collectApps headTerm of
        (nestedHead, nestedArgs)
          | not (null nestedArgs) ->
              normalizePartialApplicationSpine nestedHead (nestedArgs ++ suppliedArgs)
        _ ->
          (headTerm, suppliedArgs)

convertPartialApplicationArgument ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  XmlfTerm ->
  Int ->
  BackendType ->
  XmlfTerm ->
  ConvertM BackendExpr
convertPartialApplicationArgument context env scope headTerm index0 expectedTy arg
  | partialApplicationArgumentNeedsClosureValue context scope headTerm index0 expectedTy =
      convertClosureValueArgument context env scope expectedTy arg
  | otherwise =
      convertTermExpectedMode DirectLambda context env scope (Just expectedTy) arg

partialApplicationArgumentNeedsClosureValue ::
  ConvertContext ->
  ClosureScope ->
  XmlfTerm ->
  Int ->
  BackendType ->
  Bool
partialApplicationArgumentNeedsClosureValue context scope headTerm index0 expectedTy =
  capturedFunctionArgument || demandedByCallee
  where
    capturedFunctionArgument =
      isFirstOrderFunctionCaptureType expectedTy && not evidenceArgument
    evidenceArgument =
      Set.member index0 (lookupEvidenceValueArguments context scope headTerm)
    demandedByCallee =
      isFirstOrderFunctionCaptureType expectedTy
        && demandedByCalleeName
    demandedByCalleeName =
      Set.member index0 (lookupClosureValueArgumentDemand context scope headTerm)

partialApplicationSuppliesValueArgument :: ConvertContext -> ClosureScope -> XmlfTerm -> Int -> Bool
partialApplicationSuppliesValueArgument context scope headTerm suppliedCount =
  any
    (not . partialApplicationArgumentIsEvidence context scope headTerm)
    [0 .. suppliedCount - 1]

convertClosureValueArgument ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  ConvertM BackendExpr
convertClosureValueArgument context env scope expectedTy arg = do
  argExpr <- convertTermExpectedMode DirectLambda context env scope (Just expectedTy) arg
  if backendExprIsClosureValue context scope argExpr
    then pure argExpr
    else packageDirectFunctionValue context scope expectedTy arg argExpr

convertCallArgument ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  XmlfTerm ->
  BackendType ->
  XmlfTerm ->
  ConvertM BackendExpr
convertCallArgument context env scope fun expectedArgTy arg
  | applicationArgumentNeedsClosureValue context scope expectedArgTy fun =
      convertClosureValueArgument context env scope expectedArgTy arg
  | otherwise =
      convertTermExpectedMode DirectLambda context env scope (Just expectedArgTy) arg

applicationArgumentNeedsClosureValue :: ConvertContext -> ClosureScope -> BackendType -> XmlfTerm -> Bool
applicationArgumentNeedsClosureValue context scope expectedArgTy fun =
  isFirstOrderFunctionCaptureType expectedArgTy
    && Set.notMember suppliedCount (lookupEvidenceValueArguments context scope headTerm)
    && Set.member suppliedCount (lookupClosureValueArgumentDemand context scope headTerm)
  where
    (headTerm, suppliedArgs) = collectAliasedApps fun
    suppliedCount = length suppliedArgs

packageDirectFunctionValue ::
  ConvertContext ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  BackendExpr ->
  ConvertM BackendExpr
packageDirectFunctionValue context scope resultTy headTerm headExpr = do
  (entryIdentity, entryName) <- freshClosureEntryName context (partialApplicationHint headTerm)
  let (paramTys, _) = splitBackendArrows resultTy
      (paramNames, _) =
        freshNamesLike
          (closureGeneratedNameScope context scope headTerm)
          (take (length paramTys) closureArgNameCandidates)
  params <- freshBackendClosureParams (zip paramNames paramTys)
  let paramVars = map backendClosureParamVar params
  (captures, functionExpr) <-
    case directLocalCalleeCapture context scope headTerm of
      Just captureResolved -> do
        let captureName = resolvedBackendReferenceName context captureResolved
            captureIdentity = Just (resolvedVarDetails captureResolved)
            funCapture =
              BackendClosureCapture
                { backendClosureCaptureIdentity = captureIdentity,
                  backendClosureCaptureName = captureName,
                  backendClosureCaptureType = resultTy,
                  backendClosureCaptureExpr = headExpr
                }
        pure ([funCapture], backendClosureCaptureVar funCapture)
      Nothing ->
        do
          calleeCaptures <- localClosureCapturesForTerm context scope headTerm
          pure (calleeCaptures, headExpr)
  body <- liftEitherConvert (applyPartialDirectArguments functionExpr resultTy paramVars)
  pure
    BackendClosureWithParamIdentities
      { backendExprType = resultTy,
        backendClosureEntryIdentity = Just entryIdentity,
        backendClosureEntryName = entryName,
        backendClosureCaptures = captures,
        backendClosureParamsWithIdentities = params,
        backendClosureBody = body
      }
  where
    closureArgNameCandidates =
      ["__mlfp_closure_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]

packagePartialApplication ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  BackendType ->
  [BackendType] ->
  [BackendExpr] ->
  [BackendType] ->
  BackendType ->
  ConvertM BackendExpr
packagePartialApplication context env scope resultTy headTerm headTy suppliedParamTys suppliedArgExprs remainingParamTys finalTy = do
  (entryIdentity, entryName) <- freshClosureEntryName context (partialApplicationHint headTerm)
  let (suppliedCaptureNames, usedAfterSuppliedCaptures) =
        freshNamesLike
          (closureGeneratedNameScope context scope headTerm)
          (take (length suppliedParamTys) suppliedCaptureNameCandidates)
      (remainingParamNames, _) =
        freshNamesLike
          usedAfterSuppliedCaptures
          (take (length remainingParamTys) remainingParamNameCandidates)
      functionCaptureName =
        freshNameLike
          (partialFunctionCaptureNameFor headTerm)
          (Set.fromList (suppliedCaptureNames ++ remainingParamNames))
  suppliedCaptures <-
    sequence
      [ freshBackendClosureCapture name argTy argExpr
      | (name, argTy, argExpr) <- zip3 suppliedCaptureNames suppliedParamTys suppliedArgExprs
      ]
  remainingParams <- freshBackendClosureParams (zip remainingParamNames remainingParamTys)
  let suppliedVars = map backendClosureCaptureVar suppliedCaptures
      remainingVars = map backendClosureParamVar remainingParams
      allArgs = suppliedVars ++ remainingVars
  (captures, body) <-
    if isClosureHeadTerm context scope headTerm
      then do
        funExpr <- convertTermExpectedMode DirectLambda context env scope (Just headTy) headTerm
        funCapture <- freshBackendClosureCapture functionCaptureName headTy funExpr
        let funVar = backendClosureCaptureVar funCapture
        pure
          ( funCapture : suppliedCaptures,
            BackendClosureCall
              { backendExprType = finalTy,
                backendClosureFunction = funVar,
                backendClosureArguments = allArgs
              }
          )
      else do
        headExpr <- convertTermExpectedMode DirectLambda context env scope (Just headTy) headTerm
        if backendExprIsClosureValue context scope headExpr
          then do
            funCapture <- freshBackendClosureCapture functionCaptureName headTy headExpr
            let funVar = backendClosureCaptureVar funCapture
            pure
              ( funCapture : suppliedCaptures,
                BackendClosureCall
                  { backendExprType = finalTy,
                    backendClosureFunction = funVar,
                    backendClosureArguments = allArgs
                  }
              )
          else do
            case directLocalCalleeCapture context scope headTerm of
              Just captureResolved -> do
                let captureName = resolvedBackendReferenceName context captureResolved
                    captureIdentity = Just (resolvedVarDetails captureResolved)
                    funCapture =
                      BackendClosureCapture
                        { backendClosureCaptureIdentity = captureIdentity,
                          backendClosureCaptureName = captureName,
                          backendClosureCaptureType = headTy,
                          backendClosureCaptureExpr = headExpr
                        }
                    funVar = backendClosureCaptureVar funCapture
                bodyExpr <- liftEitherConvert (applyPartialDirectArguments funVar headTy allArgs)
                pure (funCapture : suppliedCaptures, bodyExpr)
              Nothing -> do
                calleeCaptures <- localClosureCapturesForTerm context scope headTerm
                bodyExpr <- liftEitherConvert (applyPartialDirectArguments headExpr headTy allArgs)
                pure (calleeCaptures ++ suppliedCaptures, bodyExpr)
  pure
    BackendClosureWithParamIdentities
      { backendExprType = resultTy,
        backendClosureEntryIdentity = Just entryIdentity,
        backendClosureEntryName = entryName,
        backendClosureCaptures = captures,
        backendClosureParamsWithIdentities = remainingParams,
        backendClosureBody = body
      }
  where
    suppliedCaptureNameCandidates =
      ["__mlfp_partial_capture" ++ show index0 | index0 <- [(0 :: Int) ..]]
    remainingParamNameCandidates =
      ["__mlfp_partial_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]

freshNamesLike :: Set.Set String -> [String] -> ([String], Set.Set String)
freshNamesLike used0 =
  go used0 []
  where
    go used names [] =
      (reverse names, used)
    go used names (candidate : rest) =
      let name = freshNameLike candidate used
       in go (Set.insert name used) (name : names) rest

closureGeneratedNameScope :: ConvertContext -> ClosureScope -> XmlfTerm -> Set.Set String
closureGeneratedNameScope context scope term =
  Set.unions
    [ closureScopeBoundTermNames scope,
      globalTermRuntimeNames context,
      freeResolvedTermReferenceNames term
    ]

globalTermRuntimeNames :: ConvertContext -> Set.Set String
globalTermRuntimeNames =
  Set.fromList . Map.elems . ccTermRuntimeNamesByIdentity

partialFunctionCaptureName :: String
partialFunctionCaptureName =
  "__mlfp_partial_function"

termReferenceName :: XmlfTerm -> Maybe String
termReferenceName =
  \case
    EVarNode resolved ->
      Just (resolvedVarReferenceName resolved)
    _ ->
      Nothing

termResolvedVar :: XmlfTerm -> Maybe ResolvedVar
termResolvedVar =
  \case
    EVarNode resolved -> Just resolved
    _ -> Nothing

resolvedBackendReferenceName :: ConvertContext -> ResolvedVar -> String
resolvedBackendReferenceName context resolved =
  case resolvedVarSymbolIdentity resolved >>= (`Map.lookup` ccTermRuntimeNamesByIdentity context) of
    Just runtimeName -> runtimeName
    Nothing -> resolvedVarReferenceName resolved

partialFunctionCaptureNameFor :: XmlfTerm -> String
partialFunctionCaptureNameFor term =
  case termReferenceName (stripClosureHeadTypeInsts term) of
    Just name -> name
    Nothing -> partialFunctionCaptureName

directLocalCalleeCapture :: ConvertContext -> ClosureScope -> XmlfTerm -> Maybe ResolvedVar
directLocalCalleeCapture context scope term =
  case stripClosureHeadTypeInsts term of
    EVarNode resolved
      | closureScopeHasBoundTerm resolved scope,
        not (resolvedIsGlobalTerm context resolved) ->
          Just resolved
    _ -> Nothing

localClosureCapturesForTerm :: ConvertContext -> ClosureScope -> XmlfTerm -> ConvertM [BackendClosureCapture]
localClosureCapturesForTerm context scope term = do
  traverse convertCapture resolvedLocalCaptures
  where
    convertCapture (resolved, elabTy) = do
      captureTy <- normalizeBackendTypeForContext context <$> liftEitherConvert (convertElabType elabTy)
      let name = resolvedVarReferenceName resolved
      pure
        BackendClosureCapture
          { backendClosureCaptureIdentity = Just (resolvedVarDetails resolved),
            backendClosureCaptureName = name,
            backendClosureCaptureType = captureTy,
            backendClosureCaptureExpr = BackendVarWithIdentity captureTy (Just (resolvedVarDetails resolved)) name
          }

    freeResolvedVarKeys =
      Set.fromList (map resolvedVarIdentityKey (freeResolvedTermVariables term))

    resolvedLocalCaptures =
      [ (resolved, resolvedVarType resolved)
      | resolved <- closureScopeResolvedTerms scope,
        Set.member (resolvedVarIdentityKey resolved) freeResolvedVarKeys,
        not (resolvedIsGlobalTerm context resolved)
      ]

resolvedIsGlobalTerm :: ConvertContext -> ResolvedVar -> Bool
resolvedIsGlobalTerm context resolved =
  case resolvedVarSymbolIdentity resolved of
    Just symbol -> Map.member symbol (ccTermRuntimeNamesByIdentity context)
    Nothing -> False

stripBackendHeadTypeApps :: BackendExpr -> BackendExpr
stripBackendHeadTypeApps =
  \case
    BackendTyApp _ fun _ -> stripBackendHeadTypeApps fun
    other -> other

applyPartialDirectArguments :: BackendExpr -> BackendType -> [BackendExpr] -> Either BackendConversionError BackendExpr
applyPartialDirectArguments headExpr headTy args =
  go headExpr headTy args
  where
    go current _ [] =
      Right current
    go current (BTArrow expectedArgTy resultTy) (arg : rest)
      | alphaEqBackendType expectedArgTy (backendExprType arg) =
          go (BackendApp resultTy current arg) resultTy rest
      | otherwise =
          Left
            ( BackendUnsupportedCaseShape
                ( "partial application argument type mismatch: expected "
                    ++ show expectedArgTy
                    ++ ", got "
                    ++ show (backendExprType arg)
                )
            )
    go _ otherTy (_ : _) =
      Left
        ( BackendUnsupportedCaseShape
            ("partial application expected a function type, got " ++ show otherTy)
        )

partialApplicationHint :: XmlfTerm -> String
partialApplicationHint term =
  case termReferenceName (stripClosureHeadTypeInsts term) of
    Just name -> name ++ "$partial"
    Nothing -> "partial"

isConstructorHeadTerm :: ConvertContext -> XmlfTerm -> Bool
isConstructorHeadTerm context term =
  case constructorHeadMeta context (stripClosureHeadTypeInsts term) of
    Just _ -> True
    Nothing -> False

partialApplicationCanCaptureSuppliedArgs :: ConvertContext -> ClosureScope -> XmlfTerm -> [(Int, BackendType, BackendExpr)] -> Bool
partialApplicationCanCaptureSuppliedArgs context scope headTerm =
  all canCapture
  where
    canCapture (index0, argTy, argExpr)
      | partialApplicationArgumentIsEvidence context scope headTerm index0 =
          canCaptureEvidence argTy argExpr
      | isClosureConvertibleFunctionType argTy =
          isFirstOrderFunctionCaptureType argTy
            && backendExprIsClosureValue context scope argExpr
      | isFunctionLikeBackendType argTy =
          False
      | otherwise =
          True

    canCaptureEvidence argTy argExpr
      | isFunctionLikeBackendType argTy =
          isFirstOrderFunctionCaptureType argTy
            && backendExprCanStoreFunctionReference context scope argExpr
      | otherwise =
          True

partialApplicationArgumentIsEvidence :: ConvertContext -> ClosureScope -> XmlfTerm -> Int -> Bool
partialApplicationArgumentIsEvidence context scope headTerm index0 =
  Set.member index0 (lookupEvidenceValueArguments context scope headTerm)

backendExprCanStoreFunctionReference :: ConvertContext -> ClosureScope -> BackendExpr -> Bool
backendExprCanStoreFunctionReference context scope expr =
  backendExprIsClosureValue context scope expr
    || case stripBackendHeadTypeApps expr of
      BackendVarWithIdentity {} -> True
      _ -> False

isFunctionLikeBackendType :: BackendType -> Bool
isFunctionLikeBackendType =
  \case
    BTForall _ _ body ->
      isFunctionLikeBackendType body
    BTArrow {} ->
      True
    _ ->
      False

isFirstOrderFunctionCaptureType :: BackendType -> Bool
isFirstOrderFunctionCaptureType ty =
  case ty of
    BTArrow {} ->
      let (paramTys, returnTy) = splitBackendArrows ty
       in all isFirstOrderCaptureValueType (returnTy : paramTys)
    _ ->
      False

isFirstOrderCaptureValueType :: BackendType -> Bool
isFirstOrderCaptureValueType =
  \case
    BTVar {} ->
      False
    BTArrow {} ->
      False
    BTBase {} ->
      True
    BTCon _ args ->
      all isFirstOrderCaptureValueType args
    BTVarApp {} ->
      False
    BTForall {} ->
      False
    BTMu {} ->
      True
    BTBottom ->
      False

canonicalLiteralResultType :: Lit -> BackendType -> BackendType
canonicalLiteralResultType lit resultTy
  | alphaEqBackendType resultTy expectedTy = expectedTy
  | BTVarWithIdentity {} <- resultTy = expectedTy
  | otherwise = resultTy
  where
    expectedTy = literalBackendType lit

convertOrdinaryTerm :: LambdaMode -> ConvertContext -> Env -> ClosureScope -> XmlfTerm -> BackendType -> ConvertM BackendExpr
convertOrdinaryTerm mode context env scope term resultTy0 =
      let resultTy = normalizeBackendTypeForContext context resultTy0
       in case resultTy of
        BTForallWithIdentity identity name mbBound bodyTy
          | shouldSynthesizeTypeAbs term -> do
              bodyExpr <- convertTermExpectedMode mode context env scope (Just bodyTy) term
              if alphaEqBackendType (backendExprType bodyExpr) resultTy
                then pure bodyExpr
                else
                  pure
                    BackendTyAbsWithIdentity
                      { backendExprType = resultTy,
                        backendTyParamIdentity = identity,
                        backendTyParamName = name,
                        backendTyParamBound = mbBound,
                        backendTyAbsBody = bodyExpr
                      }
        _ -> convertOrdinaryTermByShape resultTy term
  where
    shouldSynthesizeTypeAbs = \case
      EVarNode {} -> False
      ETyAbsRef {} -> False
      ETyInst {} -> False
      _ -> True

    convertOrdinaryTermByShape resultTy term0 =
      case term0 of
          EVarNode resolved ->
            resolvedReferenceBackendExpr resolved resultTy
          ELit lit ->
            pure
              BackendLit
                { backendExprType = canonicalLiteralResultType lit resultTy,
                  backendLit = lit
                }
          ELam resolved body ->
            convertLambdaTerm term0 resultTy resolved body
          EApp {} ->
            convertApplicationTerm context env scope resultTy term0
          ELet resolved scheme rhs body ->
            convertLetTerm resultTy resolved scheme rhs body
          ETyAbsRef ref mbBound body ->
            case resultTy of
              BTForallWithIdentity expectedIdentity expectedName expectedBound bodyTy -> do
                let boundTy = maybe TBottom tyToElab mbBound
                    name = expectedName
                    bodyExpected = Just bodyTy
                bodyExpr <- convertTermExpectedMode mode context (extendTypeEnv ref boundTy env) scope bodyExpected body
                pure
                  BackendTyAbsWithIdentity
                    { backendExprType = resultTy,
                      backendTyParamIdentity = expectedIdentity,
                      backendTyParamName = name,
                      backendTyParamBound = expectedBound,
                      backendTyAbsBody = bodyExpr
                    }
              _ ->
                convertTermExpectedMode mode context env scope (Just resultTy) body
          ETyInst inner inst ->
            convertTypeInstantiation context env scope resultTy inner inst
          ERoll rollTy body -> do
            rollBackendTy <- normalizeBackendTypeForContext context <$> liftEitherConvert (convertElabType rollTy)
            let rollResultTy =
                  case unfoldBackendRecursiveType rollBackendTy of
                    Just {} -> rollBackendTy
                    Nothing -> structuralRollResultType context rollBackendTy
                bodyExpected = unfoldBackendRecursiveType rollResultTy
            bodyExpr <- convertTermExpectedMode mode context env scope bodyExpected body
            pure
              BackendRoll
                { backendExprType = rollResultTy,
                  backendRollPayload = bodyExpr
                }
          EUnroll body -> do
            bodyExpr <- convertTerm context env scope body
            pure
              BackendUnroll
                { backendExprType = resultTy,
                  backendUnrollPayload = bodyExpr
                }

    resolvedReferenceBackendExpr resolved fallbackTy =
      case lookupResolvedTermEnvEntry (resolvedTermEnv env) resolved of
        Just (_, envTy) -> do
          backendTy <- normalizeBackendTypeForContext context <$> liftEitherConvert (convertElabType envTy)
          let varExpr =
                BackendVarWithIdentity
                  { backendExprType = backendTy,
                    backendVarIdentity = Just (resolvedVarDetails resolved),
                    backendVarName = resolvedBackendReferenceName context resolved
                  }
          case inferExpectedTypeApplications context fallbackTy backendTy of
            Just args
              | not (null args) ->
                  pure (applyBackendTypeApplications context fallbackTy varExpr args)
            _ | resolvedVarIsLocal resolved ->
                  pure varExpr
            _ | not (resolvedVarIsLocal resolved), BTForall {} <- backendTy ->
                  pure varExpr
              | alphaEqBackendType backendTy fallbackTy ->
                  pure varExpr
              | backendTypesCompatible context backendTy fallbackTy ->
                  pure varExpr
              | otherwise ->
                  pure (fallbackReferenceExpr resolved fallbackTy)
        Nothing ->
          pure (fallbackReferenceExpr resolved fallbackTy)

    fallbackReferenceExpr resolved fallbackTy =
      BackendVarWithIdentity
        { backendExprType = fallbackTy,
          backendVarIdentity = Just (resolvedVarDetails resolved),
          backendVarName = resolvedBackendReferenceName context resolved
        }

    convertLambdaTerm termForClosure resultTy resolved body
      | shouldClosureConvertLambda mode resultTy =
          convertLambdaClosure mode context env scope resultTy termForClosure
      | otherwise = do
          let name = resolvedVarReferenceName resolved
              paramTy = resolvedVarType resolved
          rawParamBackendTy <- liftEitherConvert (convertElabType paramTy)
          let (paramBackendTy, bodyExpected) =
                case resultTy of
                  BTArrow expectedParam cod -> (expectedParam, Just cod)
                  _ -> (normalizeBackendTypeForContext context rawParamBackendTy, Nothing)
              paramEnvTy =
                case backendTypeToElabType paramBackendTy of
                  Just canonicalTy -> canonicalTy
                  Nothing -> paramTy
          paramEnvBackendTy <- normalizeBackendTypeForContext context <$> liftEitherConvert (convertElabType paramEnvTy)
          let
              bodyMode = directLambdaBodyMode bodyExpected body
              paramResolved = mapResolvedVarType (const paramEnvTy) resolved
              bodyScope =
                extendClosureScopeResolvedTerm
                  paramResolved
                  paramEnvTy
                  (not (isEvidenceCapture context paramResolved) && isClosureConvertibleFunctionType paramEnvBackendTy)
                  scope
          bodyExpr <- convertTermExpectedMode bodyMode context (extendResolvedTermEnv paramResolved paramEnvTy env) bodyScope bodyExpected body
          let lambdaTy = BTArrow paramEnvBackendTy (backendExprType bodyExpr)
          pure
            BackendLamWithIdentity
              { backendExprType = lambdaTy,
                backendParamIdentity = Just (resolvedVarDetails resolved),
                backendParamName = name,
                backendParamType = paramEnvBackendTy,
                backendBody = bodyExpr
              }

    convertLetTerm resultTy resolved scheme rhs body = do
      let schemeTy = schemeToType scheme
          name = resolvedVarReferenceName resolved
          key = TermVarResolved resolved
      when (termMentionsFreeVariable key rhs) $
        liftEitherConvert (Left (BackendUnsupportedRecursiveLet name))
      bindingTy <- normalizeBackendTypeForContext context <$> liftEitherConvert (convertElabType schemeTy)
      let bindingClosure = letBindingNeedsClosure context scope key bindingTy rhs body
          rhsMode =
            if bindingClosure
              then ClosureLambda (Just name)
              else DirectLambda
      rhsExpr <- convertTermExpectedMode rhsMode context env scope (Just bindingTy) rhs
      let bindingEnvTy =
            case backendTypeToElabType bindingTy of
              Just canonicalTy -> canonicalTy
              Nothing -> schemeTy
          demandedClosureValueArguments =
            bindingClosureValueArguments context scope bindingTy rhs
          evidenceValueArguments =
            bindingEvidenceValueArguments context scope bindingTy rhs
          bindingResolved =
            mapResolvedVarType (const bindingEnvTy) resolved
          bodyScope =
            extendClosureScopeEvidenceArguments bindingResolved evidenceValueArguments $
              extendClosureScopeValueArguments bindingResolved demandedClosureValueArguments $
                extendClosureScopeResolvedTerm
                  bindingResolved
                  bindingEnvTy
                  (bindingClosure || backendExprIsClosureValue context scope rhsExpr)
                  scope
          bodyMode =
            if isClosureConvertibleFunctionType resultTy
              then ClosureLambda Nothing
              else mode
      bodyExpr <-
        convertTermExpectedMode
          bodyMode
          context
          (extendResolvedTermEnv bindingResolved bindingEnvTy env)
          bodyScope
          (Just resultTy)
          body
      let letResultTy =
            if alphaEqBackendType (backendExprType bodyExpr) resultTy
              || backendTypesCompatible context (backendExprType bodyExpr) resultTy
              then backendExprType bodyExpr
              else resultTy
      pure
        BackendLetWithIdentity
          { backendExprType = letResultTy,
            backendLetIdentity = Just (resolvedVarDetails resolved),
            backendLetName = name,
            backendLetType = bindingTy,
            backendLetRhs = rhsExpr,
            backendLetBody = bodyExpr
          }

convertApplication ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  XmlfTerm ->
  ConvertM BackendExpr
convertApplication context env scope resultTy fun arg =
  if termContainsTypeInstantiation fun
    then
      convertApplicationFromExpectedResult context env scope resultTy fun arg
        `orElseConvertM` convertApplicationFromFunction context env scope resultTy fun arg
    else
      convertApplicationFromFunction context env scope resultTy fun arg
        `orElseConvertM` convertApplicationFromExpectedResult context env scope resultTy fun arg

convertApplicationTerm ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  ConvertM BackendExpr
convertApplicationTerm context env scope resultTy term =
  case collectApps term of
    (headTerm, args)
      | not (null args),
        isClosureHeadTerm context scope headTerm -> do
          funTy <- normalizeBackendTypeForContext context <$> liftEitherConvert (inferBackendType context env headTerm)
          let (paramTys, expectedResultTy) = splitBackendArrows funTy
          if length paramTys == length args && not (null paramTys)
            then do
              funExpr <- convertTermExpectedMode DirectLambda context env scope (Just funTy) headTerm
              argExprs <- zipWithM (convertTermExpectedMode DirectLambda context env scope . Just) paramTys args
              let callResultTy =
                    if alphaEqBackendType resultTy expectedResultTy
                      then resultTy
                      else expectedResultTy
              pure
                BackendClosureCall
                  { backendExprType = callResultTy,
                    backendClosureFunction = funExpr,
                    backendClosureArguments = argExprs
                  }
            else convertApplicationFallback
    _ -> convertApplicationFallback
  where
    convertApplicationFallback =
      case term of
        EApp fun arg -> convertApplication context env scope resultTy fun arg
        _ -> liftEitherConvert (Left (BackendUnsupportedCaseShape "expected application term"))

convertApplicationFromFunction ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  XmlfTerm ->
  ConvertM BackendExpr
convertApplicationFromFunction context env scope resultTy fun arg = do
  funExpr <- convertTermExpectedModeNoPartial DirectLambda context env scope Nothing fun
  argExpr <-
    case backendExprType funExpr of
      BTArrow expectedArg _ -> convertCallArgument context env scope fun expectedArg arg
      other -> liftEitherConvert (Left (BackendUnsupportedCaseShape ("expected function, got " ++ show other)))
  let callResultTy = applicationResultType resultTy funExpr
  if backendExprIsClosureValue context scope funExpr
    then
      pure
        BackendClosureCall
          { backendExprType = callResultTy,
            backendClosureFunction = funExpr,
            backendClosureArguments = [argExpr]
          }
    else pure (backendApplication callResultTy funExpr argExpr)

convertApplicationFromExpectedResult ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  XmlfTerm ->
  ConvertM BackendExpr
convertApplicationFromExpectedResult context env scope resultTy fun arg = do
  rawArgTy <- liftEitherConvert (constructorArgumentMatchType context env arg)
  let argTy = canonicalizeBackendType context rawArgTy
  funExpr <- convertTermExpectedModeNoPartial DirectLambda context env scope (Just (BTArrow argTy resultTy)) fun
  argExpr <- convertCallArgument context env scope fun argTy arg
  let callResultTy = applicationResultType resultTy funExpr
  if backendExprIsClosureValue context scope funExpr
    then
      pure
        BackendClosureCall
          { backendExprType = callResultTy,
            backendClosureFunction = funExpr,
            backendClosureArguments = [argExpr]
          }
    else pure (backendApplication callResultTy funExpr argExpr)

backendApplication :: BackendType -> BackendExpr -> BackendExpr -> BackendExpr
backendApplication resultTy funExpr argExpr =
  BackendApp
    { backendExprType = resultTy,
      backendFunction = funExpr,
      backendArgument = argExpr
    }

applicationResultType :: BackendType -> BackendExpr -> BackendType
applicationResultType resultTy funExpr =
  case backendExprType funExpr of
    BTArrow _ actualResultTy
      | not (alphaEqBackendType actualResultTy resultTy) -> actualResultTy
    _ -> resultTy

orElseConvertM :: ConvertM a -> ConvertM a -> ConvertM a
orElseConvertM primary fallback =
  StateT $ \state0 ->
    case runStateT primary state0 of
      Right value -> Right value
      Left _ -> runStateT fallback state0

shouldClosureConvertLambda :: LambdaMode -> BackendType -> Bool
shouldClosureConvertLambda mode resultTy =
  case mode of
    ClosureLambda {} -> isClosureConvertibleFunctionType resultTy
    DirectLambda -> False

isClosureConvertibleFunctionType :: BackendType -> Bool
isClosureConvertibleFunctionType =
  \case
    BTArrow {} -> True
    _ -> False

isClosureConvertibleElabType :: ElabType -> Bool
isClosureConvertibleElabType =
  \case
    TArrow {} -> True
    _ -> False

directLambdaBodyMode :: Maybe BackendType -> XmlfTerm -> LambdaMode
directLambdaBodyMode bodyExpected body =
  case bodyExpected of
    Just bodyTy
      | isClosureConvertibleFunctionType bodyTy,
        not (isImmediateLambda body) ->
          ClosureLambda Nothing
    _ -> DirectLambda

isImmediateLambda :: XmlfTerm -> Bool
isImmediateLambda =
  \case
    ELam {} -> True
    ETyAbsRef _ _ body -> isImmediateLambda body
    ETyInst inner _ -> isImmediateLambda inner
    _ -> False

letBindingNeedsClosure :: ConvertContext -> ClosureScope -> TermVarKey -> BackendType -> XmlfTerm -> XmlfTerm -> Bool
letBindingNeedsClosure context scope key bindingTy rhs body =
  isClosureConvertibleFunctionType bindingTy
    && (isClosureAliasTerm context scope rhs || (isClosureConvertibleFunctionTerm rhs && termUsesFunctionAsValue bindingTy key body))

isClosureConvertibleFunctionTerm :: XmlfTerm -> Bool
isClosureConvertibleFunctionTerm term =
  not (null params)
  where
    (params, _) = collectClosureLams (stripAdministrativeTermWrappers term)

isClosureAliasTerm :: ConvertContext -> ClosureScope -> XmlfTerm -> Bool
isClosureAliasTerm context scope term =
  termUsesClosureCallPath context scope (stripClosureHeadTypeInsts term)

backendExprIsClosureValue :: ConvertContext -> ClosureScope -> BackendExpr -> Bool
backendExprIsClosureValue context scope expr =
  case backendCallableHead (callableBindingKindInClosureScope context scope) expr of
    BackendClosureCallableHead _ -> True
    _ -> False

callableBindingKindInClosureScope :: ConvertContext -> ClosureScope -> Maybe IdDetails -> String -> BackendCallableBindingKind
callableBindingKindInClosureScope context scope mbIdentity _name =
  case mbIdentity >>= callableBindingKindByIdentity context scope of
    Just kind -> kind
    Nothing
      | Just _ <- mbIdentity ->
          BackendCallableBindingUnknown
      | otherwise ->
          BackendCallableBindingDirect

callableBindingKindByIdentity :: ConvertContext -> ClosureScope -> IdDetails -> Maybe BackendCallableBindingKind
callableBindingKindByIdentity context scope details
  | closureScopeHasLocalDetails details scope =
      Just BackendCallableBindingClosure
  | closureScopeHasBoundDetails details scope =
      Just BackendCallableBindingDirect
  | Just symbol <- idDetailsSymbolIdentity details,
    Set.member symbol (ccClosureGlobalsByIdentity context) =
      Just BackendCallableBindingClosure
  | Just symbol <- idDetailsSymbolIdentity details,
    Map.member symbol (ccTermRuntimeNamesByIdentity context) =
      Just BackendCallableBindingDirect
  | otherwise =
      Nothing

isClosureHeadTerm :: ConvertContext -> ClosureScope -> XmlfTerm -> Bool
isClosureHeadTerm context scope term =
  termUsesClosureCallPath context scope (stripClosureHeadTypeInsts term)

termUsesClosureCallPath :: ConvertContext -> ClosureScope -> XmlfTerm -> Bool
termUsesClosureCallPath context scope headTerm =
  case localClosurePath of
    Just usesClosure -> usesClosure
    Nothing ->
      case termHeadSymbolIdentity context headTerm of
        Just symbol
          | Set.member symbol (ccClosureGlobalsByIdentity context) -> True
        _ -> False
  where
    localClosurePath =
      case headTerm of
        EVarNode resolved
          | closureScopeHasLocalTerm resolved scope ->
              Just True
          | closureScopeHasBoundTerm resolved scope ->
              Just False
        _ ->
          Nothing

stripClosureHeadTypeInsts :: XmlfTerm -> XmlfTerm
stripClosureHeadTypeInsts =
  \case
    ETyInst inner _ -> stripClosureHeadTypeInsts inner
    other -> other

termUsesFunctionAsValue :: BackendType -> TermVarKey -> XmlfTerm -> Bool
termUsesFunctionAsValue bindingTy needle =
  go False
  where
    functionArity =
      length (fst (splitBackendArrows bindingTy))

    go underLambda term =
      case term of
        EVarNode resolved ->
          termVarKeyMatchesReference needle resolved
        ELit {} ->
          False
        ELam resolved body
          | termVarKeyMatchesReference needle resolved -> False
          | otherwise -> go True body
        EApp {} ->
          let (headTerm, args) = collectApps term
              headUse =
                if termHeadMatchesNeedle headTerm
                  then underLambda || length args < functionArity
                  else go underLambda headTerm
           in headUse || any (go underLambda) args
        ELet resolved _ rhs body
          | termVarKeyMatchesReference needle resolved -> go underLambda rhs
          | otherwise -> go underLambda rhs || go underLambda body
        ETyAbsRef _ _ body ->
          go underLambda body
        ETyInst inner _ ->
          go underLambda inner
        ERoll _ body ->
          go underLambda body
        EUnroll body ->
          go underLambda body

    termHeadMatchesNeedle headTerm =
      case stripClosureHeadTypeInsts headTerm of
        EVarNode resolved -> termVarKeyMatchesReference needle resolved
        _ -> False

convertLambdaClosure :: LambdaMode -> ConvertContext -> Env -> ClosureScope -> BackendType -> XmlfTerm -> ConvertM BackendExpr
convertLambdaClosure mode context env scope resultTy term = do
  let (rawParams, body) = collectClosureLams term
      (declaredParamTys, _) = splitBackendArrows resultTy
  when (null rawParams) $
    liftEitherConvert (Left (BackendUnsupportedCaseShape "closure conversion expected a lambda"))
  unless (length rawParams == length declaredParamTys) $
    liftEitherConvert
      ( Left
          ( BackendUnsupportedCaseShape
              ( "closure conversion expected "
                  ++ show (length declaredParamTys)
                  ++ " lambda parameters, collected "
                  ++ show (length rawParams)
              )
          )
      )
  (params, bodyExpected) <- closureBackendParams context resultTy rawParams
  let paramEnvBindings =
        [ (mapResolvedVarType (const envTy) resolved, envTy)
        | ((resolved, rawTy), backendTy) <- zip rawParams (map backendClosureParamType params),
          let envTy = maybe rawTy id (backendTypeToElabType backendTy)
        ]
      captures = capturedTermBindingsIn (closureScopeResolvedTerms scope) term
  captureExprs <- traverse convertCapture captures
  (entryIdentity, entryName) <- freshClosureEntryName context (closureHint mode rawParams)
  let captureScope =
        foldr
          ( \(resolved, ty) acc ->
              extendClosureScopeResolvedTerm
                resolved
                ty
                ( closureScopeHasLocalTerm resolved scope
                    || isClosureConvertibleResolvedBinding context resolved ty
                )
                acc
          )
          emptyClosureScope
          captures
      bodyScope = extendClosureScopeLambdaParams context paramEnvBindings captureScope
      bodyEnv =
        foldr
          (\(resolved, ty) acc -> extendResolvedTermEnv resolved ty acc)
          env
          (captures ++ paramEnvBindings)
  bodyExpr <- convertTermExpectedMode (ClosureLambda Nothing) context bodyEnv bodyScope (Just bodyExpected) body
  pure
    BackendClosureWithParamIdentities
      { backendExprType = resultTy,
        backendClosureEntryIdentity = Just entryIdentity,
        backendClosureEntryName = entryName,
        backendClosureCaptures = captureExprs,
        backendClosureParamsWithIdentities = params,
        backendClosureBody = bodyExpr
      }
  where
    convertCapture (resolved, ty) = do
      let name = resolvedVarReferenceName resolved
      backendTy <- normalizeBackendTypeForContext context <$> liftEitherConvert (convertElabType ty)
      expr <- convertTermExpectedMode DirectLambda context env scope (Just backendTy) (EVarNode (mapResolvedVarType (const ty) resolved))
      pure
        BackendClosureCapture
          { backendClosureCaptureIdentity = Just (resolvedVarDetails resolved),
            backendClosureCaptureName = name,
            backendClosureCaptureType = backendTy,
            backendClosureCaptureExpr = expr
          }

closureHint :: LambdaMode -> [TermCapture] -> String
closureHint mode params =
  case mode of
    ClosureLambda (Just hint) -> hint
    _ ->
      case params of
        (resolved, _) : _ -> resolvedVarReferenceName resolved
        [] -> "lambda"

collectClosureLams :: XmlfTerm -> ([TermCapture], XmlfTerm)
collectClosureLams =
  go Set.empty []
  where
    go avoid params =
      \case
        ELam resolved body ->
          let name = resolvedVarReferenceName resolved
              ty = resolvedVarType resolved
              paramNames = Set.fromList (map (resolvedVarReferenceName . fst) params)
              needsFreshName =
                Set.member name avoid || Set.member name paramNames
              used =
                Set.unions
                  [ avoid,
                    paramNames,
                    termVariableNames body,
                    Set.singleton name
                  ]
              name' =
                if needsFreshName
                  then freshNameLike name used
                  else name
              body' =
                if name' == name
                  then body
                  else renameBoundTermVariable (TermVarResolved resolved) name' body
              resolved' = renameResolvedLocalVar name' resolved
           in go avoid (params ++ [(resolved', ty)]) body'
        ELet resolved scheme rhs body ->
          let name = resolvedVarReferenceName resolved
              avoidForBody =
                Set.insert name $
                  Set.unions
                    [ avoid,
                      Set.fromList (map (resolvedVarReferenceName . fst) params),
                      termVariableNames rhs
                    ]
           in case go avoidForBody [] body of
            ([], _) -> (params, ELet resolved scheme rhs body)
            (bodyParams, bodyCore) -> (params ++ bodyParams, ELet resolved scheme rhs bodyCore)
        other -> (params, other)

closureBackendParams :: ConvertContext -> BackendType -> [TermCapture] -> ConvertM ([BackendClosureParam], BackendType)
closureBackendParams context resultTy rawParams =
  go resultTy rawParams
  where
    go bodyTy [] =
      pure ([], bodyTy)
    go (BTArrow expectedParam restTy) ((resolved, _) : rest) = do
      (params, finalTy) <- go restTy rest
      pure (resolvedBackendClosureParam resolved expectedParam : params, finalTy)
    go otherTy ((resolved, rawTy) : rest) = do
      rawBackendTy <- normalizeBackendTypeForContext context <$> liftEitherConvert (convertElabType rawTy)
      (params, finalTy) <- go otherTy rest
      pure (resolvedBackendClosureParam resolved rawBackendTy : params, finalTy)

freshClosureEntryName :: ConvertContext -> String -> ConvertM (UniqueIdentity, String)
freshClosureEntryName context hint = do
  state0 <- get
  let generatedNames = csGeneratedClosureNames state0
      ((identity, name), generator') = pickName generatedNames (csIdentityGenerator state0)
  modify
    ( \state1 ->
        state1
          { csGeneratedClosureNames = Set.insert name (csGeneratedClosureNames state1),
            csIdentityGenerator = generator'
          }
    )
  pure (identity, name)
  where
    pickName generatedNames generator =
      let candidate =
            "__mlfp_closure$"
              ++ sanitizeClosureName (ccCurrentBindingName context)
              ++ "$"
              ++ sanitizeClosureName hint
              ++ "$"
              ++ show (uniqueIdentityValue identity)
          (identity, generator') = freshIdentity generator
       in if Set.member candidate (globalTermRuntimeNames context) || Set.member candidate generatedNames
            then pickName generatedNames generator'
            else ((identity, candidate), generator')

freshBackendLocalRef :: String -> ConvertM LocalRef
freshBackendLocalRef name = do
  state0 <- get
  let (localRef, generator') = freshLocalRef name (csIdentityGenerator state0)
  modify (\state1 -> state1 {csIdentityGenerator = generator'})
  pure localRef

freshBackendLocalDetails :: String -> ConvertM IdDetails
freshBackendLocalDetails name =
  LocalId <$> freshBackendLocalRef name

freshBackendClosureParam :: String -> BackendType -> ConvertM BackendClosureParam
freshBackendClosureParam name ty = do
  identity <- freshBackendLocalDetails name
  pure
    BackendClosureParam
      { backendClosureParamIdentity = Just identity,
        backendClosureParamName = name,
        backendClosureParamType = ty
      }

freshBackendClosureParams :: [(String, BackendType)] -> ConvertM [BackendClosureParam]
freshBackendClosureParams =
  traverse (uncurry freshBackendClosureParam)

resolvedBackendClosureParam :: ResolvedVar -> BackendType -> BackendClosureParam
resolvedBackendClosureParam resolved ty =
  BackendClosureParam
    { backendClosureParamIdentity = Just (resolvedVarDetails resolved),
      backendClosureParamName = resolvedVarReferenceName resolved,
      backendClosureParamType = ty
    }

backendClosureParamVar :: BackendClosureParam -> BackendExpr
backendClosureParamVar param =
  BackendVarWithIdentity
    (backendClosureParamType param)
    (backendClosureParamIdentity param)
    (backendClosureParamName param)

freshBackendClosureCapture :: String -> BackendType -> BackendExpr -> ConvertM BackendClosureCapture
freshBackendClosureCapture name ty expr = do
  identity <- freshBackendLocalDetails name
  pure
    BackendClosureCapture
      { backendClosureCaptureIdentity = Just identity,
        backendClosureCaptureName = name,
        backendClosureCaptureType = ty,
        backendClosureCaptureExpr = expr
      }

backendClosureCaptureVar :: BackendClosureCapture -> BackendExpr
backendClosureCaptureVar capture =
  BackendVarWithIdentity
    (backendClosureCaptureType capture)
    (backendClosureCaptureIdentity capture)
    (backendClosureCaptureName capture)

sanitizeClosureName :: String -> String
sanitizeClosureName =
  map sanitizeChar
  where
    sanitizeChar c
      | isAlphaNum c || c == '_' || c == '$' || c == '.' = c
      | otherwise = '_'

termMentionsFreeVariable :: TermVarKey -> XmlfTerm -> Bool
termMentionsFreeVariable needle =
  go
  where
    go term =
      case term of
        EVarNode resolved ->
          termVarKeyMatchesReference needle resolved
        ELit {} ->
          False
        ELam resolved body
          | termVarKeyMatchesReference needle resolved -> False
          | otherwise -> go body
        EApp fun arg ->
          go fun || go arg
        ELet resolved _ rhs body
          | termVarKeyMatchesReference needle resolved -> go rhs
          | otherwise -> go rhs || go body
        ETyAbsRef _ _ body ->
          go body
        ETyInst inner _ ->
          go inner
        ERoll _ body ->
          go body
        EUnroll body ->
          go body

convertTypeInstantiation ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  Instantiation ->
  ConvertM BackendExpr
convertTypeInstantiation context env scope resultTy inner inst =
  case inst of
    InstId -> do
      innerExpr <- convertTerm context env scope inner
      if alphaEqBackendType (backendExprType innerExpr) resultTy
        then pure innerExpr
        else do
          expectedExpr <- convertTermExpectedMode DirectLambda context env scope (Just resultTy) inner
          if alphaEqBackendType (backendExprType expectedExpr) resultTy
            then pure expectedExpr
            else liftEitherConvert (Left (BackendUnsupportedInstantiation inst))
    _ ->
      case appLikeInstantiationTypes inst of
        Just tyArgs -> do
          innerExpr <- convertAppLikeInstantiationFunction context env scope inner
          backendTyArgs0 <- mapM (liftEitherConvert . convertElabType) tyArgs
          let backendTyArgs = map (normalizeBackendTypeForContext context) backendTyArgs0
          case chooseExpectedTypeApplications context resultTy (backendExprType innerExpr) backendTyArgs of
            Just chosenArgs -> pure (applyBackendTypeApplications context resultTy innerExpr chosenArgs)
            Nothing
              | alphaEqBackendType (backendExprType innerExpr) resultTy -> pure innerExpr
              | otherwise -> liftEitherConvert (Left (BackendUnsupportedInstantiation inst))
        Nothing -> liftEitherConvert (Left (BackendUnsupportedInstantiation inst))

appLikeInstantiationTypes :: Instantiation -> Maybe [ElabType]
appLikeInstantiationTypes inst =
  case inst of
    InstApp ty -> Just [ty]
    InstElim -> Just [TBottom]
    InstSeq (InstInside (InstBot ty)) InstElim -> Just [ty]
    InstSeq (InstInside (InstApp ty)) InstElim -> Just [ty]
    InstSeq left right -> (++) <$> appLikeInstantiationTypes left <*> appLikeInstantiationTypes right
    _ -> Nothing

chooseExpectedTypeApplications ::
  ConvertContext ->
  BackendType ->
  BackendType ->
  [BackendType] ->
  Maybe [BackendType]
chooseExpectedTypeApplications context resultTy functionTy explicitArgs =
  case peelBackendForalls (length explicitArgs) functionTy of
    Just (binders, finalBodyTy) ->
      let hasGraphPlaceholder =
            any backendTypeContainsGraphPlaceholder explicitArgs
          recoveredArgs = recoverEvidenceTypeApplicationArgs context binders finalBodyTy explicitArgs
          inferredArgs =
            if hasGraphPlaceholder
              then inferExpectedTypeApplicationsFromBody context resultTy binders finalBodyTy
              else Nothing
          candidates =
            nub $
              if hasGraphPlaceholder
                then maybe [] (: []) inferredArgs ++ [recoveredArgs, explicitArgs]
                else [recoveredArgs, explicitArgs]
          chosenArgs =
            case find (typeApplicationsMatchExpected context resultTy binders finalBodyTy) candidates of
              Just args -> args
              Nothing -> explicitArgs
       in Just chosenArgs
    Nothing -> Nothing

chooseTypeApplicationsForArgument ::
  ConvertContext ->
  BackendType ->
  BackendType ->
  [BackendType] ->
  Maybe [BackendType]
chooseTypeApplicationsForArgument context actualArgTy functionTy explicitArgs =
  case peelBackendForalls (length explicitArgs) functionTy of
    Just (binders, finalBodyTy) ->
      case splitBackendArrows finalBodyTy of
        (expectedArgTy : _, _) ->
          let recoveredArgs = recoverEvidenceTypeApplicationArgs context binders finalBodyTy explicitArgs
              inferredArgs = inferTypeApplicationsFromArgument context actualArgTy binders expectedArgTy
              candidates =
                nub $
                  maybe [] (: []) inferredArgs ++ [recoveredArgs, explicitArgs]
           in find (typeApplicationArgumentMatches context actualArgTy binders expectedArgTy) candidates
        _ -> Nothing
    Nothing -> Nothing

backendTypeContainsGraphPlaceholder :: BackendType -> Bool
backendTypeContainsGraphPlaceholder =
  \case
    BTVarWithIdentity identity _ ->
      isGraphIdentity identity
    BTArrow dom cod ->
      backendTypeContainsGraphPlaceholder dom || backendTypeContainsGraphPlaceholder cod
    BTBaseWithIdentity _ _ ->
      False
    BTConWithIdentity _ _ args ->
      any backendTypeContainsGraphPlaceholder args
    BTVarAppWithIdentity identity _ args ->
      isGraphIdentity identity || any backendTypeContainsGraphPlaceholder args
    BTForallWithIdentity identity _ mbBound body ->
      isGraphIdentity identity
        || maybe False backendTypeContainsGraphPlaceholder mbBound
        || backendTypeContainsGraphPlaceholder body
    BTMuWithIdentity identity _ body ->
      isGraphIdentity identity || backendTypeContainsGraphPlaceholder body
    BTBottom ->
      False
  where
    isGraphIdentity Nothing =
      False
    isGraphIdentity (Just identity) =
      case typeBinderIdentityNode identity of
        Just {} -> True
        Nothing -> False

recoverEvidenceTypeApplicationArgs :: ConvertContext -> [BackendTypeAbsBinder] -> BackendType -> [BackendType] -> [BackendType]
recoverEvidenceTypeApplicationArgs context binders finalBodyTy explicitArgs =
  case splitBackendArrows finalBodyTy of
    (evidenceParamTy : _, _)
      | length binders == length explicitArgs,
        Just recoveredArgs' <- firstJust (map (recoverArgsFromExplicitCandidate evidenceParamTy) explicitArgCandidates),
        not (and (zipWith alphaEqBackendType recoveredArgs' explicitArgs)) ->
          recoveredArgs'
    _ ->
      explicitArgs
  where
    parameterBounds =
      backendTypeAbsBinderBounds binders
    firstExplicitArg =
      case explicitArgs of
        arg : _ -> arg
        [] -> BTBottom

    explicitArgCandidates =
      nub
        [ recoverStructuralBackendType context firstExplicitArg,
          normalizeBackendTypeForContext context firstExplicitArg,
          canonicalizeStructuralMuNames context firstExplicitArg,
          firstExplicitArg
        ]

    recoverArgsFromExplicitCandidate evidenceParamTy candidate = do
      recoveredArgs <-
        recoverArgsFromCandidatePreserving evidenceParamTy candidate
          <|> fmap (map (recoverStructuralBackendType context)) (recoverArgsFromCandidate evidenceParamTy candidate)
      Just (peelEvidenceArgs evidenceParamTy recoveredArgs)

    recoverArgsFromCandidate evidenceParamTy candidate = do
      substitution <-
        Structural.matchBackendTypeParametersWithTypeBounds
          Map.empty
          []
          parameterBounds
          Map.empty
          evidenceParamTy
          candidate
      let completed = completeBackendParameterSubstitution parameterBounds substitution
      traverse (lookupBackendTypeAbsBinderArg completed) binders

    recoverArgsFromCandidatePreserving evidenceParamTy candidate = do
      substitution <- matchEvidenceTypeParameters Map.empty evidenceParamTy candidate
      let completed = completeBackendParameterSubstitution parameterBounds substitution
      traverse (lookupBackendTypeAbsBinderArg completed) binders

    matchEvidenceTypeParameters substitution expected actual
      | Structural.structuralMuTypesHaveBinderIdentityMismatch expected actual =
          Nothing
      | otherwise =
          case expected of
            BTVarWithIdentity identity name
              | Just key <- parameterKey identity name ->
                  insertParameterSubstitution key actual substitution
            _ ->
              case (expected, actual) of
                (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                  matchEvidenceTypeParameters substitution expectedDom actualDom
                    >>= \substitution' -> matchEvidenceTypeParameters substitution' expectedCod actualCod
                (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase)
                  | backendTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase ->
                      Just substitution
                (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs)
                  | backendTypeHeadMatches expectedIdentity expectedCon actualIdentity actualCon,
                    length expectedArgs == length actualArgs ->
                      foldM
                        ( \substitution' (expectedArg, actualArg) ->
                            matchEvidenceTypeParameters substitution' expectedArg actualArg
                        )
                        substitution
                        (zip (NE.toList expectedArgs) (NE.toList actualArgs))
                (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity actualName actualBound actualBody) -> do
                  substitution' <-
                    case (expectedBound, actualBound) of
                      (Nothing, Nothing) -> Just substitution
                      (Just expectedBoundTy, Just actualBoundTy) -> matchEvidenceTypeParameters substitution expectedBoundTy actualBoundTy
                      _ -> Nothing
                  let actualBody' =
                        substituteBackendTypeForBinder
                          actualIdentity
                          actualName
                          (BTVarWithIdentity expectedIdentity expectedName)
                          actualBody
                  matchEvidenceTypeParameters substitution' expectedBody actualBody'
                (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
                  let actualBody' =
                        substituteBackendTypeForBinder
                          actualIdentity
                          actualName
                          (BTVarWithIdentity expectedIdentity expectedName)
                          actualBody
                   in matchEvidenceTypeParameters substitution expectedBody actualBody'
                _
                  | backendTypesCompatible context expected actual ->
                      Just substitution
                  | otherwise ->
                      Nothing

    parameterKey identity name =
      let key = backendTypeSubstitutionKeyFor identity name
       in if Map.member key parameterBounds
            then Just key
            else Nothing

    insertParameterSubstitution key actual substitution =
      case Map.lookup key substitution of
        Nothing -> Just (Map.insert key actual substitution)
        Just previous
          | backendTypesCompatible context previous actual -> Just substitution
          | backendTypesCompatible context actual previous -> Just (Map.insert key actual substitution)
          | otherwise -> Nothing

    peelEvidenceArgs evidenceParamTy =
      \case
        [arg] -> [peelEvidenceArg evidenceParamTy arg]
        args -> args

    peelEvidenceArg evidenceParamTy =
      go []
      where
        go seen arg =
          case recoverArgsFromCandidatePreserving evidenceParamTy arg <|> recoverArgsFromCandidate evidenceParamTy arg of
            Just [arg']
              | not (alphaEqBackendType arg arg'),
                not (any (alphaEqBackendType arg') (arg : seen)) ->
                  go (arg : seen) arg'
            _ ->
              arg

    firstJust =
      foldr (<|>) Nothing

inferExpectedTypeApplicationsFromBody :: ConvertContext -> BackendType -> [BackendTypeAbsBinder] -> BackendType -> Maybe [BackendType]
inferExpectedTypeApplicationsFromBody context resultTy binders finalBodyTy =
  firstJust $
    [ inferFrom visibleFinalBodyTy visibleResultTy
    | Just (visibleFinalBodyTy, visibleResultTy) <- [dropLeadingBackendArrows finalBodyTy resultTy]
    ]
      ++ [inferFrom finalBodyTy resultTy]
  where
    parameterBounds =
      backendTypeAbsBinderBounds binders

    inferFrom expected actual =
      case Structural.matchBackendTypeParametersWithTypeBounds Map.empty [] parameterBounds Map.empty expected actual of
        Just substitution ->
          let completed = completeBackendParameterSubstitution parameterBounds substitution
           in case traverse (lookupBackendTypeAbsBinderArg completed) binders of
                Just args
                  | typeApplicationsMatchExpected context resultTy binders finalBodyTy args ->
                      Just args
                _ ->
                  Nothing
        Nothing ->
          Nothing

    dropLeadingBackendArrows expected actual =
      case (expected, actual) of
        (BTArrow _ expectedTail, BTArrow _ actualTail) -> Just (expectedTail, actualTail)
        _ -> Nothing

    firstJust =
      foldr (<|>) Nothing

inferTypeApplicationsFromArgument :: ConvertContext -> BackendType -> [BackendTypeAbsBinder] -> BackendType -> Maybe [BackendType]
inferTypeApplicationsFromArgument context actualArgTy binders expectedArgTy =
  firstJust [inferFrom candidate | candidate <- actualArgCandidates]
  where
    parameterBounds =
      backendTypeAbsBinderBounds binders

    actualArgCandidates =
      nub
        [ recoverStructuralBackendType context actualArgTy,
          normalizeBackendTypeForContext context actualArgTy,
          canonicalizeStructuralMuNames context actualArgTy,
          actualArgTy
        ]

    inferFrom candidate =
      case Structural.matchBackendTypeParametersWithTypeBounds Map.empty [] parameterBounds Map.empty expectedArgTy candidate of
        Just substitution ->
          let completed = completeBackendParameterSubstitution parameterBounds substitution
           in traverse (lookupBackendTypeAbsBinderArg completed) binders
        Nothing ->
          Nothing

    firstJust =
      foldr (<|>) Nothing

typeApplicationsMatchExpected :: ConvertContext -> BackendType -> [BackendTypeAbsBinder] -> BackendType -> [BackendType] -> Bool
typeApplicationsMatchExpected context resultTy binders finalBodyTy args =
  length binders == length args
    && typeApplicationResultMatches context appliedTy resultTy
  where
    appliedTy =
      normalizeBackendTypeForContext context $
        substituteBackendTypesByKey
          (backendTypeAbsBinderSubstitution binders args)
          finalBodyTy

typeApplicationArgumentMatches :: ConvertContext -> BackendType -> [BackendTypeAbsBinder] -> BackendType -> [BackendType] -> Bool
typeApplicationArgumentMatches context actualArgTy binders expectedArgTy args =
  length binders == length args
    && typeApplicationResultMatches context appliedArgTy actualArgTy
  where
    appliedArgTy =
      normalizeBackendTypeForContext context $
        substituteBackendTypesByKey
          (backendTypeAbsBinderSubstitution binders args)
          expectedArgTy

typeApplicationResultMatches :: ConvertContext -> BackendType -> BackendType -> Bool
typeApplicationResultMatches context expected actual
  | Structural.structuralMuTypesHaveBinderIdentityMismatch expected actual =
      False
  | otherwise =
      backendTypesCompatible context expected actual
        || case (expected, actual) of
          (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
            typeApplicationResultMatches context expectedDom actualDom
              && typeApplicationResultMatches context expectedCod actualCod
          (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs) ->
            backendTypeHeadMatches expectedIdentity expectedCon actualIdentity actualCon
              && length expectedArgs == length actualArgs
              && and (zipWith (typeApplicationResultMatches context) (NE.toList expectedArgs) (NE.toList actualArgs))
          (BTVarAppWithIdentity expectedIdentity expectedName expectedArgs, BTVarAppWithIdentity actualIdentity actualName actualArgs) ->
            typeBinderRefMatches expectedIdentity expectedName actualIdentity actualName
              && length expectedArgs == length actualArgs
              && and (zipWith (typeApplicationResultMatches context) (NE.toList expectedArgs) (NE.toList actualArgs))
          (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity actualName actualBound actualBody) ->
            maybeTypesMatch expectedBound actualBound
              && typeApplicationResultMatches context expectedBody (substituteBackendTypeForBinder actualIdentity actualName (BTVarWithIdentity expectedIdentity expectedName) actualBody)
          (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
            typeApplicationResultMatches context expectedBody (substituteBackendTypeForBinder actualIdentity actualName (BTVarWithIdentity expectedIdentity expectedName) actualBody)
          _ ->
            False
  where
    maybeTypesMatch Nothing Nothing =
      True
    maybeTypesMatch (Just expectedTy) (Just actualTy) =
      typeApplicationResultMatches context expectedTy actualTy
    maybeTypesMatch _ _ =
      False

inferExpectedTypeApplications :: ConvertContext -> BackendType -> BackendType -> Maybe [BackendType]
inferExpectedTypeApplications context resultTy functionTy =
  case candidates of
    args : _ -> Just args
    [] -> Nothing
  where
    candidates =
      [ args
      | count <- [1 .. leadingBackendForallCount functionTy],
        Just (binders, finalBodyTy) <- [peelBackendForalls count functionTy],
        Just args <- [inferExpectedTypeApplicationsFromBody context resultTy binders finalBodyTy]
      ]

leadingBackendForallCount :: BackendType -> Int
leadingBackendForallCount =
  \case
    BTForallWithIdentity _ _ _ bodyTy -> 1 + leadingBackendForallCount bodyTy
    _ -> 0

peelBackendForalls :: Int -> BackendType -> Maybe ([BackendTypeAbsBinder], BackendType)
peelBackendForalls 0 ty = Just ([], ty)
peelBackendForalls count ty =
  case ty of
    BTForallWithIdentity identity name mbBound bodyTy -> do
      (rest, finalTy) <- peelBackendForalls (count - 1) bodyTy
      Just (BackendTypeAbsBinder identity name mbBound : rest, finalTy)
    _ -> Nothing

backendTypeAbsBinderKey :: BackendTypeAbsBinder -> BackendTypeSubstitutionKey
backendTypeAbsBinderKey (BackendTypeAbsBinder identity name _) =
  backendTypeSubstitutionKeyFor identity name

backendTypeAbsBinderKeys :: BackendTypeAbsBinder -> [BackendTypeSubstitutionKey]
backendTypeAbsBinderKeys binder =
  [backendTypeAbsBinderKey binder]

backendTypeAbsBinderBounds :: [BackendTypeAbsBinder] -> BackendParameterBounds
backendTypeAbsBinderBounds binders =
  Map.fromList
    [ (key, mbBound)
    | binder@(BackendTypeAbsBinder _ _ mbBound) <- binders,
      key <- backendTypeAbsBinderKeys binder
    ]

lookupBackendTypeAbsBinderArg :: Map BackendTypeSubstitutionKey BackendType -> BackendTypeAbsBinder -> Maybe BackendType
lookupBackendTypeAbsBinderArg substitution binder =
  Map.lookup (backendTypeAbsBinderKey binder) substitution

backendTypeAbsBinderSubstitution :: [BackendTypeAbsBinder] -> [BackendType] -> Map BackendTypeSubstitutionKey BackendType
backendTypeAbsBinderSubstitution binders args =
  Map.fromList
    [ (key, arg)
    | (binder, arg) <- zip binders args,
      key <- backendTypeAbsBinderKeys binder
    ]

applyBackendTypeApplications :: ConvertContext -> BackendType -> BackendExpr -> [BackendType] -> BackendExpr
applyBackendTypeApplications context resultTy expr0 args0 =
  go expr0 args0
  where
    go expr [] = expr
    go expr (arg : rest) =
      case backendExprType expr of
        BTForallWithIdentity identity name _ bodyTy ->
          let appliedTy0 =
                normalizeBackendTypeForContext context $
                  substituteBackendTypesByKey
                    (backendTypeAbsBinderSubstitution [BackendTypeAbsBinder identity name Nothing] [arg])
                    bodyTy
              appliedTy =
                if null rest && alphaEqBackendType appliedTy0 resultTy
                  then resultTy
                  else appliedTy0
              expr' =
                BackendTyApp
                  { backendExprType = appliedTy,
                    backendTyFunction = expr,
                    backendTyArgument = arg
                  }
           in go expr' rest
        _ -> expr

applyBackendTypeArgumentsToType :: ConvertContext -> BackendType -> [BackendType] -> BackendType
applyBackendTypeArgumentsToType context =
  foldl applyOne
  where
    applyOne ty arg =
      case ty of
        BTForallWithIdentity identity name _ bodyTy ->
          normalizeBackendTypeForContext context $
            substituteBackendTypesByKey
              (backendTypeAbsBinderSubstitution [BackendTypeAbsBinder identity name Nothing] [arg])
              bodyTy
        _ -> ty

termContainsTypeInstantiation :: XmlfTerm -> Bool
termContainsTypeInstantiation =
  \case
    EVarNode {} -> False
    ELit {} -> False
    ELam _ body -> termContainsTypeInstantiation body
    EApp fun arg -> termContainsTypeInstantiation fun || termContainsTypeInstantiation arg
    ELet _ _ rhs body -> termContainsTypeInstantiation rhs || termContainsTypeInstantiation body
    ETyAbsRef _ _ body -> termContainsTypeInstantiation body
    ETyInst {} -> True
    ERoll _ body -> termContainsTypeInstantiation body
    EUnroll body -> termContainsTypeInstantiation body

convertAppLikeInstantiationFunction ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  XmlfTerm ->
  ConvertM BackendExpr
convertAppLikeInstantiationFunction context env scope inner =
  convertInner `orElseConvertM` convertStrippedElim
  where
    convertInner = do
      expr <- convertTerm context env scope inner
      if hasForallResult expr
        then pure expr
        else
          ( do
              strippedExpr <- convertStrippedElim
              if hasForallResult strippedExpr
                then pure strippedExpr
                else pure expr
          )
            `orElseConvertM` pure expr

    hasForallResult expr =
      case backendExprType expr of
        BTForall {} -> True
        _ -> False

    convertStrippedElim =
      let stripped = dropLeadingElimInstantiations inner
       in if stripped == inner
            then liftEitherConvert (Left (BackendUnsupportedInstantiation InstElim))
            else convertTerm context env scope stripped

dropLeadingElimInstantiations :: XmlfTerm -> XmlfTerm
dropLeadingElimInstantiations term =
  case term of
    ETyInst inner InstElim -> dropLeadingElimInstantiations inner
    _ -> term

appLikeInstantiationType :: Instantiation -> Maybe ElabType
appLikeInstantiationType inst =
  case inst of
    InstApp ty -> Just ty
    InstSeq (InstInside (InstBot ty)) InstElim -> Just ty
    InstSeq (InstInside (InstApp ty)) InstElim -> Just ty
    _ -> Nothing

convertConstructorApplication ::
  LambdaMode ->
  ConvertContext ->
  Env ->
  ClosureScope ->
  XmlfTerm ->
  BackendType ->
  ConvertM (Maybe BackendExpr)
convertConstructorApplication _mode context env scope term resultTy =
  liftEitherConvert (constructorApplicationTerm context term) >>= \case
    Just (ConstructorApplication constructorMeta headTypeArgs args) -> do
      let constructor = cmBackend constructorMeta
          ownerContext = contextForDataMeta context (cmData constructorMeta)
          dataParameterRefs = constructorDataParameterRefs constructorMeta
          parameters = constructorTypeParameters constructorMeta
          rawFields = backendConstructorFields constructor
          effectiveResultTy = constructorExpectedResultType context ownerContext constructorMeta resultTy
          constructorResultTy = canonicalizeStructuralMuNames ownerContext (backendConstructorResult constructor)
      typeBounds <- liftEitherConvert (backendTypeBoundsFromEnv env)
      initialSubstitutions <- liftEitherConvert (constructorTypeApplicationSubstitutions env constructorMeta headTypeArgs)
      substitution <-
        liftEitherConvert $
          firstRightOr
            (constructorResultMismatch constructor)
            [ do
                resultSubstitution <-
                  case constructorResultSubstitution
                    context
                    ownerContext
                    typeBounds
                    (backendDataIdentity (dmBackend (cmData constructorMeta)))
                    dataParameterRefs
                    parameters
                    initialSubstitution
                    constructorResultTy
                    effectiveResultTy of
                    Just substitution -> Right substitution
                    Nothing -> Left (constructorResultMismatch constructor)
                foldM
                  (matchConstructorApplicationArgument context env typeBounds (backendDataIdentity (dmBackend (cmData constructorMeta))) dataParameterRefs parameters)
                  resultSubstitution
                  (zip rawFields args)
              | initialSubstitution <- initialSubstitutions
            ]
      let completedSubstitution =
            completeDataParameterSubstitution (dmBackend (cmData constructorMeta)) $
              completeBackendParameterSubstitution parameters substitution
          fields = map (substituteBackendTypesByKey completedSubstitution) rawFields
          substitutedResultTy0 = substituteBackendTypesByKey completedSubstitution constructorResultTy
          substitutedResultTy =
            case constructorNominalResultType (backendDataIdentity (dmBackend (cmData constructorMeta))) dataParameterRefs completedSubstitution constructorResultTy of
              Just nominalTy -> nominalTy
              Nothing -> recoverStructuralBackendType ownerContext substitutedResultTy0
      unless
        ( constructorResultTypesMatch context ownerContext typeBounds substitutedResultTy effectiveResultTy
            || constructorBoundaryTypesMatch context ownerContext typeBounds substitutedResultTy0 effectiveResultTy
        )
        $
        liftEitherConvert
          ( Left
              ( BackendUnsupportedCaseShape
                  ( "constructor result type does not match expected result for `"
                      ++ backendConstructorName constructor
                      ++ "`"
                  )
              )
          )
      liftEitherConvert (mapM_ (checkConstructorFieldArgumentSourceType context ownerContext env typeBounds constructor) (zip fields args))
      argExprs0 <- zipWithM (convertConstructorFieldArgument context env scope) fields args
      argExprs <- liftEitherConvert (zipWithM (retagConstructorFieldArgument context ownerContext typeBounds) fields argExprs0)
      liftEitherConvert (mapM_ (checkConstructorArgumentType context ownerContext typeBounds constructor) (zip [0 :: Int ..] (zip fields argExprs)))
      pure
        ( Just
            BackendConstructWithIdentity
              { backendExprType = effectiveResultTy,
                backendConstructIdentity = backendConstructorIdentity constructor,
                backendConstructName = backendConstructorName constructor,
                backendConstructArgs = argExprs
              }
        )
    Nothing -> pure Nothing
  where
    constructorResultMismatch constructor =
      BackendUnsupportedCaseShape
        ( "constructor result type does not match expected result for `"
            ++ backendConstructorName constructor
            ++ "`"
        )

    constructorResultSubstitution globalContext ownerContext typeBounds dataIdentity dataParameters parameters explicitSubstitution constructorResultTy effectiveResultTy =
      let direct = matchConstructorResult constructorResultTy effectiveResultTy normalizedExplicitSubstitution
          inferred = do
            inferredSubstitution <-
              matchConstructorResult constructorResultTy effectiveResultTy Map.empty
            if explicitSubstitutionAgreesWithInferred globalContext ownerContext typeBounds explicitSubstitution inferredSubstitution
              then Just (Map.union normalizedExplicitSubstitution inferredSubstitution)
              else Nothing
          boundaryCompatible =
            if ( alphaEqBackendType constructorResultTy effectiveResultTy
                   || constructorResultOpenPlaceholder typeBounds effectiveResultTy
               )
              && constructorBoundaryTypesMatch globalContext ownerContext typeBounds constructorResultTy effectiveResultTy
              then Just normalizedExplicitSubstitution
              else Nothing
       in direct <|> inferred <|> boundaryCompatible
      where
        normalizeResultType =
          normalizeConstructorBoundaryType ownerContext typeBounds
            . normalizeConstructorBoundaryType globalContext typeBounds

        normalizedExplicitSubstitution =
          Map.map normalizeResultType explicitSubstitution

        matchConstructorResult expected actual substitution =
          matchBackendTypeParametersWithDataIdentity dataIdentity typeBounds dataParameters parameters substitution expected actual
            <|> ( constructorNominalResultType dataIdentity dataParameters substitution expected
                    >>= \nominalExpected ->
                      matchBackendTypeParametersWithDataIdentity dataIdentity typeBounds dataParameters parameters substitution nominalExpected actual
                )
            <|> matchBackendTypeParametersWithDataIdentity
              dataIdentity
              typeBounds
              dataParameters
              parameters
              substitution
              (normalizeResultType expected)
              (normalizeResultType actual)

    explicitSubstitutionAgreesWithInferred globalContext ownerContext typeBounds explicitSubstitution inferredSubstitution =
      all explicitArgumentAgrees (Map.toList explicitSubstitution)
      where
        explicitArgumentAgrees (name, explicitTy) =
          case Map.lookup name inferredSubstitution of
            Just inferredTy ->
              alphaEqBackendType (resolveTypeBoundDependencies explicitTy) (resolveTypeBoundDependencies inferredTy)
            Nothing -> False

        resolveTypeBoundDependencies =
          recoverStructuralBackendType ownerContext
            . recoverStructuralBackendType globalContext
            . substituteBackendTypesByKey (completeBackendParameterSubstitution (typeBoundsAsParameterBounds typeBounds) Map.empty)

    checkConstructorArgumentType globalContext ownerContext typeBounds constructor (index, (expectedTy, argExpr)) =
      unless (constructorBoundaryTypesMatch globalContext ownerContext typeBounds (backendExprType argExpr) expectedTy) $
        Left
          ( BackendUnsupportedCaseShape
              ( "constructor argument "
                  ++ show index
                  ++ " type does not match expected field for `"
                  ++ backendConstructorName constructor
                  ++ "`"
              )
          )

    retagConstructorFieldArgument globalContext ownerContext typeBounds expectedTy argExpr
      | alphaEqBackendType actualTy expectedTy =
          Right argExpr {backendExprType = expectedTy}
      | Just retaggedTy <- constructorBoundaryRetagType globalContext ownerContext typeBounds expectedTy actualTy =
          Right argExpr {backendExprType = retaggedTy}
      | constructorBoundaryTypesMatch globalContext ownerContext typeBounds actualTy expectedTy =
          Right argExpr {backendExprType = expectedTy}
      | otherwise =
          Right argExpr
      where
        actualTy =
          backendExprType argExpr

    constructorBoundaryRetagType globalContext ownerContext typeBounds expectedTy actualTy =
      firstMaybe
        [ Just retaggedTy
        | (retaggedTy, actualCandidate) <-
            [ (expectedTy, actualTy),
              (expectedTy, normalizeBoundaryType globalContext ownerContext typeBounds actualTy),
              (normalizeBoundaryType globalContext ownerContext typeBounds expectedTy, normalizeBoundaryType globalContext ownerContext typeBounds actualTy)
            ],
          Just () <- [structuralActualCanRetag globalContext ownerContext typeBounds retaggedTy actualCandidate]
        ]

    structuralActualCanRetag globalContext ownerContext typeBounds expectedTy actualTy =
      case actualTy of
        BTMuWithIdentity muIdentity muName muBody ->
          structuralMuActualCanRetag globalContext ownerContext typeBounds expectedTy muIdentity muName muBody
        _ ->
          Nothing

    structuralMuActualCanRetag globalContext ownerContext typeBounds expectedTy muIdentity muName muBody =
      firstMaybe
        [ do
            dataMeta <- structuralRecursiveDataMetaByIdentity candidateContext muIdentity <|> structuralRecursiveDataMetaByFallback candidateContext muIdentity muName
            let dataDecl = dmBackend dataMeta
            if expectedHeadMatchesDataDecl dataDecl expectedTy
              then do
                actualArgs <- structuralBackendDataArguments muIdentity muName recoverFieldTy dataMeta muBody
                expectedArgs <- nominalDataTypeArgs expectedTy
                if length actualArgs == length expectedArgs
                  && and (zipWith (constructorBoundaryTypesMatch globalContext ownerContext typeBounds) actualArgs expectedArgs)
                  then Just ()
                  else Nothing
              else Nothing
        | candidateContext <- [ownerContext, globalContext]
        ]
      where
        recoverFieldTy =
          normalizeBoundaryType globalContext ownerContext typeBounds

    expectedHeadMatchesDataDecl dataDecl expectedTy =
      case expectedTy of
        BTBaseWithIdentity expectedIdentity (BaseTy expectedName) ->
          backendDataHeadMatches expectedIdentity expectedName dataDecl
        BTConWithIdentity expectedIdentity (BaseTy expectedName) _ ->
          backendDataHeadMatches expectedIdentity expectedName dataDecl
        _ ->
          False

    backendDataHeadMatches expectedIdentity expectedName dataDecl =
      backendTypeHeadMatches expectedIdentity (BaseTy expectedName) (backendDataIdentity dataDecl) (BaseTy (backendDataName dataDecl))

    nominalDataTypeArgs =
      \case
        BTBaseWithIdentity {} -> Just []
        BTConWithIdentity _ _ args -> Just (NE.toList args)
        _ -> Nothing

    checkConstructorFieldArgumentSourceType globalContext ownerContext env0 typeBounds constructor (expectedTy, arg) =
      case arg of
        EVarNode resolved ->
          case lookupResolvedTermEnvEntry (resolvedTermEnv env0) resolved of
            Just (_, envTy) ->
              case convertElabType envTy of
                Right argTy0 ->
                  let argTy = normalizeBackendTypeForContext globalContext argTy0
                   in unless
                        ( constructorBoundaryTypesMatch globalContext ownerContext typeBounds expectedTy argTy
                            || constructorFieldIdentityPlaceholder typeBounds expectedTy
                        )
                        (Left (constructorResultMismatch constructor))
                Left err -> Left err
            Nothing -> Right ()
        _ -> Right ()

    constructorFieldIdentityPlaceholder typeBounds =
      \case
        BTVarWithIdentity identity@(Just {}) name ->
          Map.notMember (backendTypeSubstitutionKeyFor identity name) typeBounds
        BTVarAppWithIdentity identity@(Just {}) name _ ->
          Map.notMember (backendTypeSubstitutionKeyFor identity name) typeBounds
        _ ->
          False

    constructorBoundaryTypesMatch globalContext ownerContext typeBounds left right =
      alphaEqBackendType left right
        || normalizedTypesMatch (normalizeBoundaryType globalContext ownerContext typeBounds left) (normalizeBoundaryType globalContext ownerContext typeBounds right)
      where
        normalizedTypesMatch leftTy rightTy =
          alphaEqBackendType leftTy rightTy
            || resultTypePlaceholderMatches typeBounds leftTy rightTy
            || resultTypePlaceholderMatches typeBounds rightTy leftTy
            || maybe False (const True) (matchBackendTypeParameters typeBounds [] Map.empty Map.empty leftTy rightTy)
            || maybe False (const True) (matchBackendTypeParameters typeBounds [] Map.empty Map.empty rightTy leftTy)

    constructorResultTypesMatch globalContext ownerContext typeBounds left right =
      constructorBoundaryTypesMatch globalContext ownerContext typeBounds left right
        || resultTypePlaceholderMatches
          typeBounds
          (normalizeBoundaryType globalContext ownerContext typeBounds left)
          (normalizeBoundaryType globalContext ownerContext typeBounds right)

    normalizeBoundaryType globalContext ownerContext typeBounds =
      normalizeConstructorBoundaryType ownerContext typeBounds
        . normalizeConstructorBoundaryType globalContext typeBounds

    normalizeConstructorBoundaryType ownerContext typeBounds =
      nominalizeStructuralRecursiveHead ownerContext
        . recoverStructuralBackendType ownerContext
        . substituteBackendTypesByKey (completeBackendParameterSubstitution (typeBoundsAsParameterBounds typeBounds) Map.empty)

    nominalizeStructuralRecursiveHead ownerContext ty =
      case ty of
        BTMuWithIdentity identity name _
          | Just dataMeta <- structuralRecursiveDataMetaByIdentity ownerContext identity <|> structuralRecursiveDataMetaByFallback ownerContext identity name ->
              case Structural.structuralMuAsDataType (backendDataIdentity (dmBackend dataMeta)) (backendDataParameterRefs (dmBackend dataMeta)) identity name of
                Just nominalTy -> nominalTy
                Nothing -> ty
        _ ->
          ty

    resultTypePlaceholderMatches typeBounds actual expected =
      case (actual, expected) of
        (_, BTVarWithIdentity identity name)
          | not (typeBoundsContain identity name typeBounds) -> True
        (_, BTVarAppWithIdentity identity name _)
          | not (typeBoundsContain identity name typeBounds) -> True
        (BTArrow actualDom actualCod, BTArrow expectedDom expectedCod) ->
          resultTypePlaceholderMatches typeBounds actualDom expectedDom
            && resultTypePlaceholderMatches typeBounds actualCod expectedCod
        (BTCon actualCon actualArgs, BTCon expectedCon expectedArgs)
          | actualCon == expectedCon,
            length actualArgs == length expectedArgs ->
              and (zipWith (resultTypePlaceholderMatches typeBounds) (NE.toList actualArgs) (NE.toList expectedArgs))
        (BTVarAppWithIdentity actualIdentity actualName actualArgs, BTVarAppWithIdentity expectedIdentity expectedName expectedArgs)
          | typeBinderRefMatches actualIdentity actualName expectedIdentity expectedName,
            length actualArgs == length expectedArgs ->
              and (zipWith (resultTypePlaceholderMatches typeBounds) (NE.toList actualArgs) (NE.toList expectedArgs))
        (BTForallWithIdentity actualIdentity actualName actualBound actualBody, BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody) ->
          resultTypePlaceholderBoundMatches typeBounds actualBound expectedBound
            && resultTypePlaceholderMatches
              ( insertTypeBound expectedIdentity expectedName Nothing $
                  insertTypeBound actualIdentity actualName Nothing typeBounds
              )
              actualBody
              expectedBody
        _ -> alphaEqBackendType actual expected

    constructorResultOpenPlaceholder typeBounds =
      \case
        BTVarWithIdentity identity name ->
          not (typeBoundsContain identity name typeBounds)
        BTVarAppWithIdentity identity name _ ->
          not (typeBoundsContain identity name typeBounds)
        _ ->
          False

    typeBoundsContain identity name bounds =
      Map.member (backendTypeSubstitutionKeyFor identity name) bounds

    insertTypeBound identity name mbBound =
      Map.insert (backendTypeSubstitutionKeyFor identity name) mbBound

    resultTypePlaceholderBoundMatches _ Nothing Nothing = True
    resultTypePlaceholderBoundMatches typeBounds (Just actual) (Just expected) =
      resultTypePlaceholderMatches typeBounds actual expected
    resultTypePlaceholderBoundMatches _ _ _ = False

convertConstructorFieldArgument ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  ConvertM BackendExpr
convertConstructorFieldArgument context env scope fieldTy arg
  | isClosureConvertibleFunctionType fieldTy = do
      closureExpr <- convertTermExpectedMode (ClosureLambda Nothing) context env scope (Just fieldTy) arg
      if backendExprIsClosureValue context scope closureExpr
        then pure closureExpr
        else convertEtaExpandedConstructorFieldClosure context env scope fieldTy arg
  | otherwise =
      convertTermExpectedMode DirectLambda context env scope (Just fieldTy) arg

convertEtaExpandedConstructorFieldClosure ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  XmlfTerm ->
  ConvertM BackendExpr
convertEtaExpandedConstructorFieldClosure context env scope fieldTy arg = do
  params <- closureFieldEtaParams fieldTy arg
  resolvedParams <- traverse freshEtaParam params
  let applied = foldl EApp arg (map (EVarNode . fst) resolvedParams)
      etaTerm = foldr (\(resolved, _) body -> ELam resolved body) applied resolvedParams
  convertTermExpectedMode (ClosureLambda Nothing) context env scope (Just fieldTy) etaTerm
  where
    freshEtaParam (name, ty) = do
      localRef <- freshBackendLocalRef name
      pure (localResolvedVarFromRef localRef ty, ty)

closureFieldEtaParams :: BackendType -> XmlfTerm -> ConvertM [(String, ElabType)]
closureFieldEtaParams fieldTy arg = do
  let (paramTys, _) = splitBackendArrows fieldTy
  when (null paramTys) $
    liftEitherConvert (Left (BackendUnsupportedCaseShape "closure-valued constructor field expected a function type"))
  paramElabTys <-
    traverse
      ( \paramTy ->
          case backendTypeToElabType paramTy of
            Just elabTy -> pure elabTy
            Nothing ->
              liftEitherConvert
                ( Left
                    ( BackendUnsupportedCaseShape
                        "closure-valued constructor field has a parameter type that cannot be eta-expanded"
                    )
                )
      )
      paramTys
  let paramNames = freshConstructorFieldParamNames (length paramElabTys) (termVariableNames arg)
  pure (zip paramNames paramElabTys)

freshConstructorFieldParamNames :: Int -> Set.Set String -> [String]
freshConstructorFieldParamNames count used0 =
  go 0 used0
  where
    go index0 used
      | index0 >= count = []
      | otherwise =
          let candidate = freshNameLike ("__mlfp_field_arg" ++ show index0) used
           in candidate : go (index0 + 1) (Set.insert candidate used)

constructorApplicationTerm :: ConvertContext -> XmlfTerm -> Either BackendConversionError (Maybe ConstructorApplication)
constructorApplicationTerm context term =
  case collectApps term of
    (headTerm, args) ->
      case directConstructorApplication headTerm args of
        Just application -> Right (Just application)
        Nothing -> structuralConstructorApplication headTerm args
  where
    directConstructorApplication headTerm args =
      case constructorHead context headTerm of
        Just (constructorKey, headTypeArgs) -> do
          constructorMeta <- lookupConstructorHeadKey context constructorKey
          guardConstructorArity constructorMeta args
          Just (ConstructorApplication constructorMeta headTypeArgs args)
        Nothing -> Nothing

    structuralConstructorApplication headTerm args =
      case filter (`constructorArityMatches` args) (Map.elems (ccConstructorsByIdentity context)) of
        [] -> Right Nothing
        candidates -> do
          (strippedHead, headTypeArgs) <- structuralConstructorHeadTypeArgs context headTerm
          let matches = filter (\candidate -> structuralConstructorHeadMatches context candidate strippedHead) candidates
          case matches of
            [constructorMeta] -> Right (Just (ConstructorApplication constructorMeta headTypeArgs args))
            [] -> Right Nothing
            _ ->
              Left
                ( BackendUnsupportedCaseShape
                    ( "ambiguous structural constructor matches: "
                        ++ show (map (backendConstructorName . cmBackend) matches)
                    )
                )

    constructorArityMatches constructorMeta args =
      length args == length (backendConstructorFields (cmBackend constructorMeta))

    guardConstructorArity constructorMeta args =
      if constructorArityMatches constructorMeta args
        then Just ()
        else Nothing

structuralConstructorHeadMatches :: ConvertContext -> ConstructorMeta -> XmlfTerm -> Bool
structuralConstructorHeadMatches context constructorMeta headTerm =
  case collectStructuralLams fieldArity (stripLeadingTypeAbs headTerm) of
    Just (argBinders, ERoll resultTy rolledBody)
      | structuralConstructorResultMatches context constructorMeta resultTy ->
          case collectStructuralLams ownerArity (stripLeadingTypeAbs rolledBody) of
            Just (handlers, selectedBody) ->
              case drop constructorIndex handlers of
                selectedHandler : _ ->
                  selectedHandlerCallMatches selectedHandler argBinders selectedBody
                [] -> False
            Nothing -> False
    _ -> False
  where
    constructor = cmBackend constructorMeta
    fieldArity = length (backendConstructorFields constructor)
    ownerArity = length (backendDataConstructors (dmBackend (cmData constructorMeta)))
    constructorIndex = ctorIndex (cmInfo constructorMeta)

structuralConstructorResultMatches :: ConvertContext -> ConstructorMeta -> ElabType -> Bool
structuralConstructorResultMatches context constructorMeta resultTy =
  case convertElabType resultTy >>= backendTypeStructuralDataHead of
    Right resultDataHead -> constructorDataNameMatches context constructorMeta resultDataHead
    Left _ -> False

data BackendStructuralDataHead
  = BackendStructuralDataHeadByIdentity SymbolIdentity
  | BackendStructuralDataHeadBySelfIdentity TypeBinderIdentity String
  | BackendStructuralDataHeadByName String
  deriving (Eq, Show)

constructorDataNameMatches :: ConvertContext -> ConstructorMeta -> BackendStructuralDataHead -> Bool
constructorDataNameMatches _context constructorMeta (BackendStructuralDataHeadByIdentity resultIdentity) =
  resultIdentity == dataInfoSymbol (dmInfo (cmData constructorMeta))
constructorDataNameMatches context constructorMeta (BackendStructuralDataHeadBySelfIdentity resultIdentity resultDataName) =
  case structuralSelfIdentityUnique (Just resultIdentity) of
    Just unique ->
      unique == symbolUniqueIdentity (dataInfoSymbol (dmInfo dataMeta))
    Nothing ->
      if Structural.structuralIdentityAllowsNameFallback (Just resultIdentity)
        then
          constructorDataNameMatches context constructorMeta (BackendStructuralDataHeadByName resultDataName)
        else
          False
  where
    dataMeta = cmData constructorMeta
constructorDataNameMatches context constructorMeta (BackendStructuralDataHeadByName resultDataName) =
  resultDataName == backendDataName (dmBackend dataMeta)
    || structuralDataNameMatches context dataMeta resultDataName
  where
    dataMeta = cmData constructorMeta

structuralDataNameMatches :: ConvertContext -> DataMeta -> String -> Bool
structuralDataNameMatches context dataMeta resultDataName =
  case dataMetaByStructuralName context resultDataName of
    Just resolvedDataMeta ->
      dataInfoSymbol (dmInfo resolvedDataMeta) == dataInfoSymbol (dmInfo dataMeta)
    Nothing -> False

backendTypeStructuralDataHead :: BackendType -> Either BackendConversionError BackendStructuralDataHead
backendTypeStructuralDataHead =
  \case
    BTBaseWithIdentity (Just identity) _ -> Right (BackendStructuralDataHeadByIdentity identity)
    BTBaseWithIdentity Nothing (BaseTy name) -> Right (BackendStructuralDataHeadByName name)
    BTConWithIdentity (Just identity) _ _ -> Right (BackendStructuralDataHeadByIdentity identity)
    BTConWithIdentity Nothing (BaseTy name) _ -> Right (BackendStructuralDataHeadByName name)
    BTMuWithIdentity (Just identity) name _ ->
      case Structural.structuralRecursiveDataName name of
        Just resultDataName -> Right (BackendStructuralDataHeadBySelfIdentity identity resultDataName)
        Nothing -> Left (BackendUnsupportedCaseShape ("unsupported structural constructor result type " ++ show name))
    BTMuWithIdentity Nothing name _ ->
      case Structural.structuralRecursiveDataName name of
        Just resultDataName -> Right (BackendStructuralDataHeadByName resultDataName)
        Nothing -> Left (BackendUnsupportedCaseShape ("unsupported structural constructor result type " ++ show name))
    BTMu name _ ->
      case Structural.structuralRecursiveDataName name of
        Just resultDataName -> Right (BackendStructuralDataHeadByName resultDataName)
        Nothing -> Left (BackendUnsupportedCaseShape ("unsupported structural constructor result type " ++ show name))
    ty -> Left (BackendUnsupportedCaseShape ("unsupported constructor result type " ++ show ty))

collectStructuralLams :: Int -> XmlfTerm -> Maybe ([ResolvedVar], XmlfTerm)
collectStructuralLams expectedCount =
  go [] expectedCount
  where
    go names remaining term
      | remaining <= 0 = Just (names, term)
      | otherwise =
          case term of
            ELam resolved body ->
              go (names ++ [resolved]) (remaining - 1) body
            _ -> Nothing

stripLeadingTypeAbs :: XmlfTerm -> XmlfTerm
stripLeadingTypeAbs =
  \case
    ETyAbsRef _ _ body -> stripLeadingTypeAbs body
    term -> term

selectedHandlerCallMatches :: ResolvedVar -> [ResolvedVar] -> XmlfTerm -> Bool
selectedHandlerCallMatches selectedHandler expectedArgs body =
  case collectApps body of
    (headTerm, callArgs)
      | Just handler <- termResolvedVar headTerm ->
          resolvedVarSameIdentity selectedHandler handler
            && length callArgs == length expectedArgs
            && and (zipWith sameArg expectedArgs callArgs)
    _ -> False
  where
    sameArg expected actual =
      maybe False (resolvedVarSameIdentity expected) (termResolvedVar actual)

constructorTypeParameters :: ConstructorMeta -> BackendParameterBounds
constructorTypeParameters constructorMeta =
  constructorTypeParameterBoundsFor (dmBackend (cmData constructorMeta)) (cmBackend constructorMeta)

constructorDataParameterRefs :: ConstructorMeta -> [BackendDataParameterRef]
constructorDataParameterRefs =
  backendDataParameterRefs . dmBackend . cmData

constructorTypeParameterBoundsFor :: BackendData -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsFor dataDecl constructor =
  Map.fromList $
    [(key, Nothing) | key <- backendDataParameterKeys dataDecl]
      ++ [ (backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderBound binder)
           | binder <- backendConstructorForalls constructor
         ]

existingParameterKeyFor :: BackendParameterBounds -> Maybe TypeBinderIdentity -> String -> Maybe BackendTypeSubstitutionKey
existingParameterKeyFor parameterBounds identity name =
  if Map.member key parameterBounds
    then Just key
    else Nothing
  where
    key = backendTypeSubstitutionKeyFor identity name

typeBoundsAsParameterBounds :: BackendTypeBounds -> BackendParameterBounds
typeBoundsAsParameterBounds =
  id

constructorTypeApplicationSubstitutions ::
  Env ->
  ConstructorMeta ->
  [BackendType] ->
  Either BackendConversionError [BackendParameterSubstitution]
constructorTypeApplicationSubstitutions env constructorMeta typeArgs =
  case usableTypeApplicationNames of
    [] ->
      Left
        ( BackendUnsupportedCaseShape
            ( "constructor type application arity mismatch for `"
                ++ backendConstructorName (cmBackend constructorMeta)
                ++ "`"
            )
        )
    names : otherNames ->
      Right (nub (map (`substitutionFor` typeArgs) (names : otherNames)))
  where
    usableTypeApplicationNames =
      filter arityMatches typeApplicationNameOrders

    arityMatches names
      | null typeArgs = True
      | otherwise = length names == length typeArgs

    typeApplicationNameOrders =
      nub
        ( constructorTypeApplicationParameterNames constructorMeta
            : checkedConstructorTypeApplicationParameterNames env constructorMeta
        )

    substitutionFor keys args =
      Map.fromList (zip keys args)

checkedConstructorTypeApplicationParameterNames :: Env -> ConstructorMeta -> [[BackendTypeSubstitutionKey]]
checkedConstructorTypeApplicationParameterNames env constructorMeta =
  case lookupResolvedTermEnvEntry resolvedEnv (resolvedVarFromConstructorMeta constructorMeta) of
    Just (_, constructorTy)
      | Right backendTy <- convertElabType constructorTy,
        let (binders, _) = splitBackendForalls backendTy,
        Just keys <- traverse binderKey binders ->
          [keys]
    _ ->
      []
  where
    parameters = constructorTypeParameters constructorMeta
    resolvedEnv = resolvedTermEnv env

    binderKey (BackendTypeAbsBinder identity name _) =
      existingParameterKeyFor parameters identity name

resolvedVarFromConstructorMeta :: ConstructorMeta -> ResolvedVar
resolvedVarFromConstructorMeta constructorMeta =
  ResolvedVar
    { resolvedVarRuntimeName = ctorRuntimeName ctorInfo,
      resolvedVarType = TBottom,
      resolvedVarDetails = ConstructorId (constructorRefFromInfo ctorInfo)
    }
  where
    ctorInfo = cmInfo constructorMeta

firstRightOr :: e -> [Either e a] -> Either e a
firstRightOr fallback =
  go Nothing
  where
    go firstErr =
      \case
        [] ->
          maybe (Left fallback) Left firstErr
        Right value : _ ->
          Right value
        Left err : rest ->
          go (firstErr <|> Just err) rest

constructorTypeApplicationParameterNames :: ConstructorMeta -> [BackendTypeSubstitutionKey]
constructorTypeApplicationParameterNames constructorMeta =
  [ key
    | ref <- constructorDataParameterKeys constructorMeta,
      let key = backendDataParameterRefKey ref,
      constructorResultIsStructural || Set.member key resultVariableKeys
  ]
    ++ [ backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder)
         | binder <- backendConstructorForalls (cmBackend constructorMeta)
       ]
  where
    constructorResultIsStructural =
      case backendConstructorResult (cmBackend constructorMeta) of
        BTMu {} -> True
        _ -> False

    resultVariableKeys =
      freeBackendTypeVarKeys (backendConstructorResult (cmBackend constructorMeta))

constructorDataParameterKeys :: ConstructorMeta -> [BackendDataParameterRef]
constructorDataParameterKeys constructorMeta =
  constructorDataParameterRefs constructorMeta

constructorHeadMeta :: ConvertContext -> XmlfTerm -> Maybe ConstructorMeta
constructorHeadMeta context term =
  constructorHead context term >>= lookupConstructorHeadKey context . fst

constructorHead :: ConvertContext -> XmlfTerm -> Maybe (ConstructorHeadKey, [BackendType])
constructorHead context term =
  case collectConstructorHeadTypes [] term of
    Just (key, typeArgs) ->
      case traverse (fmap (normalizeBackendTypeForContext context) . convertElabType) typeArgs of
        Right backendTypeArgs -> Just (key, backendTypeArgs)
        Left _ -> Nothing
    Nothing -> Nothing
  where
    collectConstructorHeadTypes typeArgs =
      \case
        ETyInst inner inst
          | Just ty <- appLikeInstantiationType inst ->
              collectConstructorHeadTypes (ty : typeArgs) inner
        headTerm ->
          (\key -> (key, typeArgs)) <$> constructorHeadKey headTerm

lookupConstructorHeadKey :: ConvertContext -> ConstructorHeadKey -> Maybe ConstructorMeta
lookupConstructorHeadKey context key =
  case key of
    ConstructorHeadIdentity symbol ->
      Map.lookup symbol (ccConstructorsByIdentity context)

constructorHeadKey :: XmlfTerm -> Maybe ConstructorHeadKey
constructorHeadKey =
  \case
    EVarNode resolved ->
      case resolvedVarConstructorRef resolved of
        Just ref -> Just (ConstructorHeadIdentity (constructorRefSymbol ref))
        Nothing -> Nothing
    _ ->
      Nothing

stripTypeInsts :: XmlfTerm -> XmlfTerm
stripTypeInsts =
  \case
    ETyInst inner _ -> stripTypeInsts inner
    other -> other

structuralConstructorHeadTypeArgs :: ConvertContext -> XmlfTerm -> Either BackendConversionError (XmlfTerm, [BackendType])
structuralConstructorHeadTypeArgs context =
  go []
  where
    go typeArgs =
      \case
        ETyInst inner inst
          | Just ty <- appLikeInstantiationType inst -> do
              backendTy <- convertTypeArg ty
              go (backendTy : typeArgs) inner
          | otherwise ->
              go typeArgs inner
        other -> Right (other, typeArgs)

    convertTypeArg ty =
      normalizeBackendTypeForContext context <$> convertElabType ty

convertCaseApplication ::
  LambdaMode ->
  ConvertContext ->
  Env ->
  ClosureScope ->
  XmlfTerm ->
  BackendType ->
  ConvertM (Maybe BackendExpr)
convertCaseApplication mode context env scope term resultTy =
  case collectApps term of
    (headTerm, args) ->
      case caseScrutinee headTerm of
        Nothing -> pure Nothing
        Just scrutineeTerm -> do
          (backendScrutineeTy, mbScrutineeData) <- liftEitherConvert (caseScrutineeInfo context env scrutineeTerm)
          dataMeta <-
            case mbScrutineeData of
              Just scrutineeData -> pure scrutineeData
              Nothing -> do
                typeBounds <- liftEitherConvert (backendTypeBoundsFromEnv env)
                liftEitherConvert (requireCaseData context typeBounds backendScrutineeTy)
          let constructors = backendDataConstructors (dmBackend dataMeta)
          refinedScrutineeTy <- liftEitherConvert (refineCaseScrutineeTypeFromHandlers context env dataMeta backendScrutineeTy constructors args)
          scrutineeExpr <- convertTermExpectedMode DirectLambda context env scope (Just refinedScrutineeTy) scrutineeTerm
          case compare (length args) (length constructors) of
            EQ -> Just <$> convertCaseWithHandlers mode context env scope resultTy scrutineeExpr dataMeta constructors args
            GT -> do
              let (handlers, extraArgs) = splitAt (length constructors) args
              extraArgTys <- liftEitherConvert (mapM (inferBackendType context env) extraArgs)
              case scanr BTArrow resultTy extraArgTys of
                caseResultTy : appliedResultTys -> do
                  caseExpr <- convertCaseWithHandlers mode context env scope caseResultTy scrutineeExpr dataMeta constructors handlers
                  Just
                    <$> ( if backendExprIsClosureValue context scope caseExpr
                            then applyCaseClosureArguments context env scope resultTy caseExpr (zip extraArgs extraArgTys)
                            else
                              foldM
                                (applyCaseExtraArgument context env scope)
                                caseExpr
                                (zip3 extraArgs extraArgTys appliedResultTys)
                        )
                [] -> pure Nothing
            LT -> pure Nothing

convertCaseWithHandlers ::
  LambdaMode ->
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  BackendExpr ->
  DataMeta ->
  [BackendConstructor] ->
  [XmlfTerm] ->
  ConvertM BackendExpr
convertCaseWithHandlers mode context env scope resultTy scrutineeExpr dataMeta constructors handlers = do
  alternatives <- zipWithMCase (convertCaseAlternative mode context env scope resultTy dataMeta (backendExprType scrutineeExpr)) constructors handlers
  pure
    BackendCase
      { backendExprType = resultTy,
        backendScrutinee = scrutineeExpr,
        backendAlternatives = alternatives
      }

applyCaseClosureArguments ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  BackendExpr ->
  [(XmlfTerm, BackendType)] ->
  ConvertM BackendExpr
applyCaseClosureArguments context env scope resultTy funExpr args = do
  argExprs <-
    traverse
      (\(arg, argTy) -> convertTermExpectedMode DirectLambda context env scope (Just argTy) arg)
      args
  pure
    BackendClosureCall
      { backendExprType = resultTy,
        backendClosureFunction = funExpr,
        backendClosureArguments = argExprs
      }

applyCaseExtraArgument ::
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendExpr ->
  (XmlfTerm, BackendType, BackendType) ->
  ConvertM BackendExpr
applyCaseExtraArgument context env scope funExpr (arg, argTy, resultTy) = do
  argExpr <- convertTermExpectedMode DirectLambda context env scope (Just argTy) arg
  pure
    BackendApp
      { backendExprType = resultTy,
        backendFunction = funExpr,
        backendArgument = argExpr
      }

caseScrutinee :: XmlfTerm -> Maybe XmlfTerm
caseScrutinee term =
  case term of
    ETyInst (EUnroll scrutinee) inst
      | Just _ <- appLikeInstantiationType inst -> Just scrutinee
    _ -> Nothing

caseScrutineeInfo :: ConvertContext -> Env -> XmlfTerm -> Either BackendConversionError (BackendType, Maybe DataMeta)
caseScrutineeInfo context env scrutineeTerm =
  constructorApplicationResultType context env scrutineeTerm
    >>= \case
      Just info -> Right info
      Nothing -> do
        scrutineeTy0 <- inferBackendType context env scrutineeTerm
        let scrutineeTy = normalizeBackendTypeForContext context scrutineeTy0
            mbDataMeta =
              scrutineeDataHint context scrutineeTerm
                <|> backendTypeDataMeta context scrutineeTy
            scrutineeTy' =
              case mbDataMeta of
                Just _ -> canonicalizeSourceBackendTypeHeads (ccDataByIdentity context) scrutineeTy
                Nothing -> scrutineeTy
        Right
          ( scrutineeTy',
            mbDataMeta
          )

scrutineeDataHint :: ConvertContext -> XmlfTerm -> Maybe DataMeta
scrutineeDataHint context term =
  case stripTypeInsts term of
    EVarNode resolved ->
      resolvedVarSymbolIdentity resolved >>= (`Map.lookup` ccBindingData context)
    _ -> Nothing

backendTypeDataMeta :: ConvertContext -> BackendType -> Maybe DataMeta
backendTypeDataMeta context ty =
  case ty of
    BTBaseWithIdentity mbIdentity _ ->
      case mbIdentity of
        Just identity -> dataMetaBySymbol context identity
        Nothing -> Nothing
    BTConWithIdentity mbIdentity _ _ ->
      case mbIdentity of
        Just identity -> dataMetaBySymbol context identity
        Nothing -> Nothing
    BTMuWithIdentity identity name _ ->
      structuralRecursiveDataMetaByIdentity context identity <|> structuralRecursiveDataMetaByFallback context identity name
    _ -> Nothing

structuralRollResultType :: ConvertContext -> BackendType -> BackendType
structuralRollResultType context ty =
  fromMaybe ty $ do
    dataMeta <- backendTypeDataMeta context ty
    args <- nominalDataTypeArgumentsFor (dmBackend dataMeta) ty
    pure (structuralRecursiveTypeForData (dmBackend dataMeta) args)

nominalDataTypeArgumentsFor :: BackendData -> BackendType -> Maybe [BackendType]
nominalDataTypeArgumentsFor dataDecl =
  \case
    BTBaseWithIdentity identity (BaseTy name)
      | dataHeadMatches identity name,
        null (backendDataParameterRefs dataDecl) ->
          Just []
    BTConWithIdentity identity (BaseTy name) args
      | dataHeadMatches identity name,
        length (NE.toList args) == length (backendDataParameterRefs dataDecl) ->
          Just (NE.toList args)
    _ ->
      Nothing
  where
    dataHeadMatches identity name =
      backendTypeHeadMatches identity (BaseTy name) (backendDataIdentity dataDecl) (BaseTy (backendDataName dataDecl))

structuralRecursiveTypeForData :: BackendData -> [BackendType] -> BackendType
structuralRecursiveTypeForData dataDecl dataArgs =
  BTMuWithIdentity
    selfIdentity
    selfName
    ( BTForallWithIdentity
        resultIdentity
        resultName
        Nothing
        (foldr BTArrow resultVar handlerTypes)
    )
  where
    dataName =
      backendDataName dataDecl
    selfName =
      "$" ++ dataName ++ "_self"
    resultName =
      "$" ++ dataName ++ "_result"
    selfIdentity =
      structuralTypeBinderIdentity StructuralSelfBinder
    resultIdentity =
      structuralTypeBinderIdentity StructuralResultBinder
    selfVar =
      BTVarWithIdentity selfIdentity selfName
    resultVar =
      BTVarWithIdentity resultIdentity resultName
    substitution =
      Map.fromList (zip (backendDataParameterKeys dataDecl) dataArgs)
    handlerTypes =
      map constructorHandlerType (backendDataConstructors dataDecl)
    constructorHandlerType constructor =
      foldr BTArrow resultVar (map structuralFieldType (backendConstructorFields constructor))
    structuralFieldType =
      replaceDataSelf . substituteBackendTypesByKey substitution

    structuralTypeBinderIdentity role =
      typeBinderIdentityFromStructural . symbolUniqueIdentity <$> backendDataIdentity dataDecl <*> pure role

    replaceDataSelf =
      \case
        BTVarWithIdentity identity name ->
          BTVarWithIdentity identity name
        BTArrow dom cod ->
          BTArrow (replaceDataSelf dom) (replaceDataSelf cod)
        ty@(BTBaseWithIdentity identity (BaseTy name))
          | dataHeadMatches identity name,
            null dataArgs ->
              selfVar
          | otherwise -> ty
        BTConWithIdentity identity (BaseTy name) args
          | dataHeadMatches identity name,
            length (NE.toList args) == length dataArgs,
            and (zipWith alphaEqBackendType (NE.toList args) dataArgs) ->
              selfVar
          | otherwise -> BTConWithIdentity identity (BaseTy name) (fmap replaceDataSelf args)
        BTVarAppWithIdentity identity name args ->
          BTVarAppWithIdentity identity name (fmap replaceDataSelf args)
        BTForallWithIdentity identity name mb body ->
          BTForallWithIdentity identity name (fmap replaceDataSelf mb) (replaceDataSelf body)
        BTMuWithIdentity identity name body ->
          BTMuWithIdentity identity name (replaceDataSelf body)
        BTBottom ->
          BTBottom

    dataHeadMatches identity name =
      backendTypeHeadMatches identity (BaseTy name) (backendDataIdentity dataDecl) (BaseTy (backendDataName dataDecl))

dataMetaByStructuralName :: ConvertContext -> String -> Maybe DataMeta
dataMetaByStructuralName context name =
  dataMetaByCurrentScopeStructuralName context name
    <|> uniqueDataMetaByStructuralName context name

dataMetaByCurrentScopeStructuralName :: ConvertContext -> String -> Maybe DataMeta
dataMetaByCurrentScopeStructuralName context name = do
  moduleIdentity <- ccCurrentModuleIdentity context
  scope <- Map.lookup moduleIdentity (ccModuleScopes context)
  info <- firstMaybe [Map.lookup candidate (elaborateScopeDataTypes scope) | candidate <- structuralNameCandidates name]
  dataMetaBySymbol context (dataInfoSymbol info)

uniqueDataMetaByStructuralName :: ConvertContext -> String -> Maybe DataMeta
uniqueDataMetaByStructuralName context name =
  case matches of
    [dataMeta] -> Just dataMeta
    _ -> Nothing
  where
    candidates =
      structuralNameCandidates name
    matches =
      [ dataMeta
      | dataMeta <- ccData context,
        any (`elem` dataMetaStructuralNames dataMeta) candidates
      ]

dataMetaStructuralNames :: DataMeta -> [String]
dataMetaStructuralNames dataMeta =
  nub
    ( symbolIdentityAliasNames (dataInfoSymbol info)
        ++ [backendDataName (dmBackend dataMeta)]
    )
  where
    info =
      dmInfo dataMeta

structuralNameCandidates :: String -> [String]
structuralNameCandidates value =
  nub [value, structuralSuffixAfterDot value]

structuralSuffixAfterDot :: String -> String
structuralSuffixAfterDot value =
  case break (== '.') (reverse value) of
    (suffix, _ : _) -> reverse suffix
    _ -> value

firstMaybe :: [Maybe a] -> Maybe a
firstMaybe =
  foldr (<|>) Nothing

dataMetaBySymbol :: ConvertContext -> SymbolIdentity -> Maybe DataMeta
dataMetaBySymbol context symbol =
  Map.lookup symbol (ccDataByIdentity context)

canonicalizeStructuralMuNames :: ConvertContext -> BackendType -> BackendType
canonicalizeStructuralMuNames context =
  go
  where
    go ty =
      case ty of
        BTVarWithIdentity {} -> ty
        BTArrow dom cod -> BTArrow (go dom) (go cod)
        BTBaseWithIdentity {} -> ty
        BTConWithIdentity identity name args -> BTConWithIdentity identity name (fmap go args)
        BTVarAppWithIdentity identity name args -> BTVarAppWithIdentity identity name (fmap go args)
        BTForallWithIdentity identity name mb body -> BTForallWithIdentity identity name (fmap go mb) (go body)
        BTMuWithIdentity identity name body ->
          let (name', body') = canonicalizeStructuralMuBinder context identity name body
           in BTMuWithIdentity identity name' (go body')
        BTBottom -> BTBottom

canonicalizeStructuralMuBinder :: ConvertContext -> Maybe TypeBinderIdentity -> String -> BackendType -> (String, BackendType)
canonicalizeStructuralMuBinder context identity name body =
  case structuralRecursiveDataMetaByIdentity context identity <|> structuralRecursiveDataMetaByFallback context identity name of
    Just dataMeta ->
      let canonicalName = "$" ++ backendDataName (dmBackend dataMeta) ++ "_self"
       in if name == canonicalName
            then (name, body)
            else (canonicalName, substituteBackendTypeForBinder identity name (BTVarWithIdentity identity canonicalName) body)
    Nothing -> (name, body)

structuralRecursiveDataMetaByIdentity :: ConvertContext -> Maybe TypeBinderIdentity -> Maybe DataMeta
structuralRecursiveDataMetaByIdentity context identity = do
  selfIdentity <- identity
  (unique, StructuralSelfBinder) <- typeBinderIdentityStructural selfIdentity
  case
    [ dataMeta
    | dataMeta <- ccData context,
      symbolUniqueIdentity (dataInfoSymbol (dmInfo dataMeta)) == unique
    ]
    of
      [dataMeta] -> Just dataMeta
      _ -> Nothing

structuralRecursiveDataMetaByFallback :: ConvertContext -> Maybe TypeBinderIdentity -> String -> Maybe DataMeta
structuralRecursiveDataMetaByFallback context identity name
  | Structural.structuralIdentityAllowsNameFallback identity = structuralRecursiveDataMeta context name
  | otherwise = Nothing

structuralSelfIdentityUnique :: Maybe TypeBinderIdentity -> Maybe UniqueIdentity
structuralSelfIdentityUnique identity = do
  selfIdentity <- identity
  (unique, StructuralSelfBinder) <- typeBinderIdentityStructural selfIdentity
  pure unique

recoverStructuralBackendType :: ConvertContext -> BackendType -> BackendType
recoverStructuralBackendType context =
  go Set.empty
  where
    go seen ty =
      case ty of
        BTVarWithIdentity {} -> ty
        BTArrow dom cod -> BTArrow (go seen dom) (go seen cod)
        BTBaseWithIdentity {} -> ty
        BTConWithIdentity identity name args -> BTConWithIdentity identity name (fmap (go seen) args)
        BTVarAppWithIdentity identity name args -> BTVarAppWithIdentity identity name (fmap (go seen) args)
        BTForallWithIdentity identity name mb body -> BTForallWithIdentity identity name (fmap (go seen) mb) (go seen body)
        BTMuWithIdentity identity name body ->
          let (name', body') = canonicalizeStructuralMuBinder context identity name body
              seen' = Set.insert name' (Set.insert name seen)
           in if Set.member name seen || Set.member name' seen
                then BTMuWithIdentity identity name' (canonicalizeStructuralMuNames context body')
                else case structuralRecursiveDataMetaByIdentity context identity <|> structuralRecursiveDataMetaByFallback context identity name' <|> structuralRecursiveDataMetaByBody identity name' (go seen') body' of
                  Just dataMeta
                    | null (backendDataParameterRefs (dmBackend dataMeta)) ->
                        backendDataType (backendDataIdentity (dmBackend dataMeta)) (backendDataName (dmBackend dataMeta)) []
                  Just dataMeta
                    | Just args <- structuralBackendDataArguments identity name' (go seen') dataMeta body' ->
                        backendDataType (backendDataIdentity (dmBackend dataMeta)) (backendDataName (dmBackend dataMeta)) args
                  _ -> BTMuWithIdentity identity name' (go seen' body')
        BTBottom -> BTBottom

    structuralRecursiveDataMetaByBody muIdentity muName recoverFieldTy body =
      case
        [ dataMeta
        | dataMeta <- ccData context,
          Just _ <- [structuralBackendDataArguments muIdentity muName recoverFieldTy dataMeta body]
        ]
      of
        [dataMeta] -> Just dataMeta
        _ -> Nothing

backendDataType :: Maybe SymbolIdentity -> String -> [BackendType] -> BackendType
backendDataType identity name args =
  case args of
    [] -> BTBaseWithIdentity identity (BaseTy name)
    arg : rest -> BTConWithIdentity identity (BaseTy name) (arg :| rest)

structuralRecursiveDataMeta :: ConvertContext -> String -> Maybe DataMeta
structuralRecursiveDataMeta context name =
  Structural.structuralRecursiveDataName name >>= dataMetaByStructuralName context

structuralBackendDataArguments :: Maybe TypeBinderIdentity -> String -> (BackendType -> BackendType) -> DataMeta -> BackendType -> Maybe [BackendType]
structuralBackendDataArguments muIdentity _muName recoverFieldTy dataMeta body = do
  handlerFields <- Structural.structuralBackendHandlerFields body
  let dataDecl = dmBackend dataMeta
      dataParameterRefs = backendDataParameterRefs dataDecl
      dataParameterKeys = backendDataParameterKeys dataDecl
      constructors = backendDataConstructors dataDecl
      parameterBounds =
        Map.fromList [(key, Nothing) | key <- dataParameterKeys]
  if length handlerFields == length constructors
    then do
      substitution <-
        foldM
          (matchConstructorFields dataDecl dataParameterRefs parameterBounds)
          Map.empty
          (zip constructors handlerFields)
      let completedSubstitution = completeBackendParameterSubstitution parameterBounds substitution
      Just
        [ Map.findWithDefault (backendDataParameterRefType ref) key completedSubstitution
        | (ref, key) <- zip dataParameterRefs dataParameterKeys
        ]
    else Nothing
  where
    matchConstructorFields dataDecl dataParameters parameterBounds substitution (constructor, fields) =
      if length fields == length (backendConstructorFields constructor)
        then
          foldM
            ( \substitutionAcc (expectedTy, actualTy) ->
                matchBackendTypeParametersWithDataIdentity
                  (backendDataIdentity dataDecl)
                  Map.empty
                  dataParameters
                  (constructorParameterBounds parameterBounds constructor)
                  substitutionAcc
                  (recoverFieldTy expectedTy)
                  (recoverDataSelfField dataDecl (recoverFieldTy actualTy))
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

    recoverDataSelfField dataDecl ty =
      case ty of
        BTVarWithIdentity fieldIdentity fieldName
          | structuralDataSelfField fieldIdentity fieldName ->
              backendDataType (backendDataIdentity dataDecl) (backendDataName dataDecl) dataSelfArgs
        _ ->
          ty
      where
        structuralDataSelfField fieldIdentity fieldName =
          Structural.structuralDataSelfFieldMatches (backendDataName dataDecl) muIdentity fieldIdentity fieldName

        dataSelfArgs =
          map backendDataParameterRefType (backendDataParameterRefs dataDecl)

constructorApplicationResultType :: ConvertContext -> Env -> XmlfTerm -> Either BackendConversionError (Maybe (BackendType, Maybe DataMeta))
constructorApplicationResultType context env term =
  constructorApplicationTerm context term >>= \case
    Just (ConstructorApplication constructorMeta headTypeArgs args) -> do
      let constructor = cmBackend constructorMeta
          ownerContext = contextForDataMeta context (cmData constructorMeta)
          fields = backendConstructorFields constructor
          dataParameterRefs = constructorDataParameterRefs constructorMeta
          parameters = constructorTypeParameters constructorMeta
          constructorResultTy = canonicalizeStructuralMuNames ownerContext (backendConstructorResult constructor)
      typeBounds <- backendTypeBoundsFromEnv env
      initialSubstitutions <- constructorTypeApplicationSubstitutions env constructorMeta headTypeArgs
      substitution <-
        firstRightOr
          (BackendUnsupportedCaseShape ("constructor arguments do not match type applications for `" ++ backendConstructorName constructor ++ "`"))
          [ foldM
              (matchConstructorApplicationArgument context env typeBounds (backendDataIdentity (dmBackend (cmData constructorMeta))) dataParameterRefs parameters)
              initialSubstitution
              (zip fields args)
            | initialSubstitution <- initialSubstitutions
          ]
      let completedSubstitution =
            completeDataParameterSubstitution (dmBackend (cmData constructorMeta)) $
              completeBackendParameterSubstitution parameters substitution
          resultTy0 = substituteBackendTypesByKey completedSubstitution constructorResultTy
          resultTy =
            case constructorNominalResultType (backendDataIdentity (dmBackend (cmData constructorMeta))) dataParameterRefs completedSubstitution constructorResultTy of
              Just nominalTy -> nominalTy
              Nothing -> recoverStructuralBackendType ownerContext resultTy0
      Right (Just (resultTy, Just (cmData constructorMeta)))
    Nothing -> Right Nothing

constructorNominalResultType :: Maybe SymbolIdentity -> [BackendDataParameterRef] -> BackendParameterSubstitution -> BackendType -> Maybe BackendType
constructorNominalResultType dataIdentity dataParameters substitution =
  \case
    BTMuWithIdentity identity name _ ->
      substituteBackendTypesByKey substitution <$> Structural.structuralMuAsDataType dataIdentity dataParameters identity name
    _ -> Nothing

constructorExpectedResultType :: ConvertContext -> ConvertContext -> ConstructorMeta -> BackendType -> BackendType
constructorExpectedResultType context ownerContext constructorMeta resultTy =
  canonicalizeBackendType ownerContext $
    case canonicalResultTy of
      BTMuWithIdentity identity name _
        | Just dataMeta <- structuralRecursiveDataMetaByIdentity ownerContext identity <|> structuralRecursiveDataMetaByFallback ownerContext identity name,
          backendDataName (dmBackend dataMeta) == ownerName ->
            canonicalResultTy
      _ ->
        recoverStructuralBackendType ownerContext (recoverStructuralBackendType context resultTy)
  where
    ownerName = backendDataName (dmBackend (cmData constructorMeta))
    canonicalResultTy = canonicalizeStructuralMuNames ownerContext resultTy

matchConstructorApplicationArgument ::
  ConvertContext ->
  Env ->
  BackendTypeBounds ->
  Maybe SymbolIdentity ->
  [BackendDataParameterRef] ->
  BackendParameterBounds ->
  BackendParameterSubstitution ->
  (BackendType, XmlfTerm) ->
  Either BackendConversionError BackendParameterSubstitution
matchConstructorApplicationArgument context env typeBounds dataIdentity dataParameters parameters substitution (expectedTy, arg) =
  -- This is only a best-effort way to recover constructor type parameters.
  -- Expected-type conversion of the argument remains authoritative because it
  -- can canonicalize nested constructor applications before validation.
  case constructorArgumentMatchType context env arg of
    Right actualTy0 ->
      let actualTy = recoverStructuralBackendType context actualTy0
       in case matchBackendTypeParametersWithDataIdentity dataIdentity typeBounds dataParameters parameters substitution expectedTy actualTy of
            Just substitution' -> Right substitution'
            Nothing -> Right substitution
    Left _ -> Right substitution

constructorArgumentMatchType :: ConvertContext -> Env -> XmlfTerm -> Either BackendConversionError BackendType
constructorArgumentMatchType context env arg =
  constructorApplicationResultType context env arg >>= \case
    Just (constructorTy, _) -> Right constructorTy
    Nothing ->
      case arg of
        EVarNode resolved ->
          case lookupResolvedTermEnvEntry (resolvedTermEnv env) resolved of
            Just (_, envTy) -> normalizeBackendTypeForContext context <$> convertElabType envTy
            Nothing
              | resolvedVarIsLocal resolved ->
                  Left (BackendUnsupportedCaseShape "unbound local resolved variable")
            Nothing -> normalizeBackendTypeForContext context <$> convertElabType (resolvedVarType resolved)
        _
          | termContainsTypeInstantiation arg ->
              case inferBackendTypeByShape context env arg of
                Right (Just ty) -> Right ty
                _ -> inferBackendType context env arg
        _ ->
          inferBackendType context env arg

requireCaseData :: ConvertContext -> BackendTypeBounds -> BackendType -> Either BackendConversionError DataMeta
requireCaseData context typeBounds scrutineeTy =
  case filter (dataMatchesScrutineeExactly scrutineeTy) (ccData context) of
    [dataMeta] -> Right dataMeta
    [] ->
      case filter (dataMatchesRecursiveBinderHint scrutineeTy) (ccData context) of
        [dataMeta] -> Right dataMeta
        _ ->
          case filter (dataMatchesScrutinee typeBounds scrutineeTy) (ccData context) of
            [dataMeta] -> Right dataMeta
            [] -> Left (BackendUnsupportedCaseShape ("no backend data matches scrutinee type " ++ show scrutineeTy))
            matches ->
              Left
                ( BackendUnsupportedCaseShape
                    ("ambiguous backend data matches scrutinee type " ++ show scrutineeTy ++ ": " ++ show (map (backendDataName . dmBackend) matches))
                )
    matches ->
      Left
        ( BackendUnsupportedCaseShape
            ("ambiguous exact backend data matches scrutinee type " ++ show scrutineeTy ++ ": " ++ show (map (backendDataName . dmBackend) matches))
        )

dataMatchesScrutineeExactly :: BackendType -> DataMeta -> Bool
dataMatchesScrutineeExactly scrutineeTy dataMeta =
  any
    ( \constructor ->
        any
          (== scrutineeTy)
          (candidateConstructorResultTypes (dmBackend dataMeta) constructor scrutineeTy)
    )
    (backendDataConstructors (dmBackend dataMeta))

dataMatchesRecursiveBinderHint :: BackendType -> DataMeta -> Bool
dataMatchesRecursiveBinderHint scrutineeTy dataMeta =
  case scrutineeTy of
    BTMuWithIdentity identity binderName _ ->
      let hints = recursiveBinderNameHints binderName
          names =
            [ dataInfoIdentityName (dmInfo dataMeta),
              backendDataName (dmBackend dataMeta)
            ]
       in structuralSelfIdentityUnique identity == Just (symbolUniqueIdentity (dataInfoSymbol (dmInfo dataMeta)))
            || any (`elem` hints) names
    _ -> False

recursiveBinderNameHints :: String -> [String]
recursiveBinderNameHints binderName =
  nub (rawHints ++ map suffixAfterDot rawHints)
  where
    raw = binderName
    withoutDollar =
      case raw of
        '$' : rest -> rest
        _ -> raw
    rawHints =
      [raw, withoutDollar, beforeSelf raw, beforeSelf withoutDollar]

    beforeSelf value =
      case reverse value of
        'f' : 'l' : 'e' : 's' : '_' : rest -> reverse rest
        _ -> value

    suffixAfterDot value =
      case break (== '.') (reverse value) of
        (suffix, _ : _) -> reverse suffix
        _ -> value

candidateConstructorResultTypes :: BackendData -> BackendConstructor -> BackendType -> [BackendType]
candidateConstructorResultTypes dataDecl constructor scrutineeTy =
  case matchBackendTypeParametersWithDataIdentity (backendDataIdentity dataDecl) Map.empty (backendDataParameterRefs dataDecl) parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
    Just substitution ->
      let completed = completeBackendParameterSubstitution parameters substitution
       in [substituteBackendTypesByKey completed (backendConstructorResult constructor)]
    Nothing ->
      []
  where
    parameters = constructorTypeParameterBoundsFor dataDecl constructor

dataMatchesScrutinee :: BackendTypeBounds -> BackendType -> DataMeta -> Bool
dataMatchesScrutinee typeBounds scrutineeTy dataMeta =
  any
    ( \constructor ->
        case
          matchBackendTypeParametersWithDataIdentity
            (backendDataIdentity (dmBackend dataMeta))
            typeBounds
            (backendDataParameterRefs (dmBackend dataMeta))
            (constructorTypeParameterBoundsFor (dmBackend dataMeta) constructor)
            Map.empty
            (backendConstructorResult constructor)
            scrutineeTy of
          Just _ -> True
          Nothing -> False
    )
    (backendDataConstructors (dmBackend dataMeta))

convertCaseAlternative ::
  LambdaMode ->
  ConvertContext ->
  Env ->
  ClosureScope ->
  BackendType ->
  DataMeta ->
  BackendType ->
  BackendConstructor ->
  XmlfTerm ->
  ConvertM BackendAlternative
convertCaseAlternative mode context env scope resultTy dataMeta scrutineeTy constructor handler = do
  fields0 <- liftEitherConvert (caseAlternativeFieldTypes env dataMeta scrutineeTy constructor)
  let fields = map canonicalFieldType fields0
  let (params, body) = collectLeadingResolvedLams (length fields) handler
  when (length params /= length fields) $
    liftEitherConvert
      ( Left
          ( BackendUnsupportedCaseShape
              ("handler arity does not match constructor `" ++ backendConstructorName constructor ++ "`")
          )
      )
  fieldEnvTypes <-
    liftEitherConvert $
      traverse
        ( \((resolved, paramTy), ty) ->
            case backendTypeToElabType ty of
              Just elabTy -> Right (resolved, elabTy)
              Nothing -> Right (resolved, paramTy)
        )
        (zip params fields)
  let env' =
        foldr
          (\(resolved, ty) acc -> extendResolvedTermEnv resolved ty acc)
          env
          fieldEnvTypes
      scope' = extendClosureScopePatternFields (zip fieldEnvTypes fields) scope
      bodyMode =
        if isClosureConvertibleFunctionType resultTy
          then ClosureLambda Nothing
          else mode
  bodyExpr <- convertTermExpectedMode bodyMode context env' scope' (Just resultTy) body
  unless (backendTypesCompatible context (backendExprType bodyExpr) resultTy) $
    liftEitherConvert
      ( Left
          ( BackendUnsupportedCaseShape
              ("handler result type does not match case result for `" ++ backendConstructorName constructor ++ "`")
          )
      )
  pure
    BackendAlternative
      { backendAltPattern =
          BackendConstructorPatternWithBinderIdentities
            (backendConstructorIdentity constructor)
            (backendConstructorName constructor)
            [ BackendPatternBinder
                { backendPatternBinderIdentity = Just (resolvedVarDetails resolved),
                  backendPatternBinderName = resolvedVarReferenceName resolved
                }
            | (resolved, _) <- params
            ],
        backendAltBody = bodyExpr
      }
  where
    canonicalFieldType =
      normalizeBackendTypeForContext context

caseAlternativeFieldTypes :: Env -> DataMeta -> BackendType -> BackendConstructor -> Either BackendConversionError [BackendType]
caseAlternativeFieldTypes env dataMeta scrutineeTy constructor = do
  typeBounds <- backendTypeBoundsFromEnv env
  let dataDecl = dmBackend dataMeta
      parameters = constructorTypeParameterBoundsFor dataDecl constructor
  case Structural.matchFocusedStructuralConstructor typeBounds dataDecl constructor Map.empty scrutineeTy of
    Right structuralMatch ->
      Right (Structural.srcmFieldTypes structuralMatch)
    Left _ ->
      case matchBackendTypeParametersWithDataIdentity (backendDataIdentity dataDecl) typeBounds (backendDataParameterRefs dataDecl) parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
        Just substitution ->
          let completed =
                completeDataParameterSubstitution dataDecl $
                  completeBackendParameterSubstitution parameters substitution
           in Right (map (substituteBackendTypesByKey completed) (backendConstructorFields constructor))
        Nothing ->
          Left
            ( BackendUnsupportedCaseShape
                ( "constructor result type does not match case scrutinee for `"
                    ++ backendConstructorName constructor
                    ++ "`: result "
                    ++ show (backendConstructorResult constructor)
                    ++ ", scrutinee "
                    ++ show scrutineeTy
                )
            )

refineCaseScrutineeTypeFromHandlers ::
  ConvertContext ->
  Env ->
  DataMeta ->
  BackendType ->
  [BackendConstructor] ->
  [XmlfTerm] ->
  Either BackendConversionError BackendType
refineCaseScrutineeTypeFromHandlers context env dataMeta scrutineeTy constructors handlers = do
  typeBounds <- backendTypeBoundsFromEnv env
  substitution <- foldM (refineAlternative typeBounds) Map.empty (zip constructors handlers)
  let completed = completeDataParameterSubstitution dataDecl substitution
  pure (substituteBackendTypesByKey completed scrutineeTy)
  where
    dataDecl = dmBackend dataMeta
    dataParameters = backendDataParameterRefs dataDecl

    refineAlternative typeBounds substitution (constructor, handler) = do
      let fields = backendConstructorFields constructor
          (params, _) = collectLeadingResolvedLams (length fields) handler
      if length params /= length fields
        then pure substitution
        else do
          paramTys <- traverse (convertHandlerParamType . snd) params
          let parameters = constructorTypeParameterBoundsFor dataDecl constructor
          case
            foldM
              ( \substitutionAcc (expectedTy, actualTy) ->
                  matchBackendTypeParametersWithDataIdentity
                    (backendDataIdentity dataDecl)
                    typeBounds
                    dataParameters
                    parameters
                    substitutionAcc
                    expectedTy
                    actualTy
              )
              substitution
              (zip fields paramTys)
            of
            Just substitution' -> pure substitution'
            Nothing -> pure substitution

    convertHandlerParamType =
      fmap (normalizeBackendTypeForContext context) . convertElabType

collectLeadingResolvedLams :: Int -> XmlfTerm -> ([(ResolvedVar, ElabType)], XmlfTerm)
collectLeadingResolvedLams arity =
  go [] arity . stripLeadingTypeWrappers
  where
    go params remaining term
      | remaining <= 0 = (params, term)
      | otherwise =
          case term of
            ETyAbsRef _ _ body -> go params remaining body
            ETyInst inner _ -> go params remaining inner
            ELam resolved body -> go (params ++ [(resolved, resolvedVarType resolved)]) (remaining - 1) body
            other -> (params, other)

    stripLeadingTypeWrappers term =
      case term of
        ETyAbsRef _ _ body -> stripLeadingTypeWrappers body
        ETyInst inner _ -> stripLeadingTypeWrappers inner
        other -> other

collectApps :: XmlfTerm -> (XmlfTerm, [XmlfTerm])
collectApps =
  go []
  where
    go args term =
      case term of
        EApp fun arg -> go (arg : args) fun
        other -> (other, args)

collectAliasedApps :: XmlfTerm -> (XmlfTerm, [XmlfTerm])
collectAliasedApps =
  go Set.empty
  where
    go seen term =
      let (headTerm, args) = collectApps (stripAdministrativeTermWrappers term)
          (resolvedHead, aliasArgs) = resolveHead seen headTerm
       in (resolvedHead, aliasArgs ++ args)

    resolveHead seen term =
      case stripAdministrativeTermWrappers term of
        ELet resolved _ rhs body
          | not (Set.member key seen) ->
              let seen' = Set.insert key seen
                  (bodyHead, bodyArgs) = go seen' body
               in case stripClosureHeadTypeInsts bodyHead of
                    EVarNode bodyResolved
                      | termVarKeyMatchesReference (TermVarResolved resolved) bodyResolved ->
                          let (rhsHead, rhsArgs) = go seen' rhs
                           in (rhsHead, rhsArgs ++ bodyArgs)
                    _ ->
                      (term, [])
          where
            key = resolvedVarIdentityKey resolved
        other
          | Just etaHead <- etaAliasHead other ->
              resolveHead seen etaHead
        other ->
          (other, [])

    etaAliasHead term =
      let (params, body) = collectEtaLams [] term
          (bodyHead, bodyArgs) = collectApps (stripAdministrativeTermWrappers body)
       in if not (null params) && etaArgsMatch params bodyArgs
            then Just bodyHead
            else Nothing

    collectEtaLams params term =
      case stripAdministrativeTermWrappers term of
        ELam resolved body -> collectEtaLams (params ++ [resolved]) body
        other -> (params, other)

    etaArgsMatch params args =
      length params == length args
        && and
          [ case stripAdministrativeTermWrappers arg of
              EVarNode argResolved -> termVarKeyMatchesReference (TermVarResolved param) argResolved
              _ -> False
          | (param, arg) <- zip params args
          ]

inferBackendType :: ConvertContext -> Env -> XmlfTerm -> Either BackendConversionError BackendType
inferBackendType context env term =
  let normalizedEnv = normalizeBuiltinEnv env
      normalizedTerm =
        reconcileBackendLocalResolvedTypes
          context
          normalizedEnv
          (normalizeBuiltinXmlfTerm term)
   in case typeCheckWithEnv normalizedEnv normalizedTerm of
    Right ty -> convertElabType ty
    Left err ->
      case inferBackendTypeByShape context normalizedEnv normalizedTerm of
        Right (Just ty) -> Right ty
        _ -> Left (BackendTypeCheckFailed term err)

inferBackendTypeByShape :: ConvertContext -> Env -> XmlfTerm -> Either BackendConversionError (Maybe BackendType)
inferBackendTypeByShape context env term =
  constructorApplicationResultType context env term >>= \case
    Just (constructorTy, _) -> Right (Just constructorTy)
    Nothing ->
      case term of
        EVarNode resolved ->
          Just <$> inferResolvedBackendType context env resolved
        ELit lit ->
          Right (Just (literalBackendType lit))
        EApp fun arg -> do
          mbFunTy <- inferApplicationFunctionTypeByShape context env fun arg
          Right $
            case mbFunTy of
              Just (BTArrow _ resultTy) -> Just resultTy
              _ -> Nothing
        ETyInst inner inst -> do
          mbInnerTy <- inferBackendTypeByShape context env inner
          case mbInnerTy of
            Just innerTy -> applyBackendInstantiation context innerTy inst
            Nothing -> Right Nothing
        ELam resolved body -> do
          paramTy <- normalizeBackendTypeForContext context <$> convertElabType (resolvedVarType resolved)
          let env' = extendResolvedTermEnv resolved (resolvedVarType resolved) env
          mbBodyTy <- inferBackendTypeByShape context env' body
          Right (BTArrow paramTy <$> mbBodyTy)
        ELet resolved scheme _rhs body ->
          inferBackendTypeByShape context (extendResolvedTermEnv resolved (schemeToType scheme) env) body
        ETyAbsRef ref mbBound body -> do
          let env' = extendTypeEnv ref (maybe TBottom tyToElab mbBound) env
          mbBodyTy <- inferBackendTypeByShape context env' body
          mbBoundTy <- traverse (convertElabType . tyToElab) mbBound
          pure $
            BTForallWithIdentity
              (Just (typeBinderRefIdentity ref))
              (typeBinderRefName ref)
              mbBoundTy
              <$> mbBodyTy
        ERoll ty _body ->
          Just . normalizeBackendTypeForContext context <$> convertElabType ty
        EUnroll body -> do
          mbBodyTy <- inferBackendTypeByShape context env body
          Right (mbBodyTy >>= unfoldBackendRecursiveType)

inferApplicationFunctionTypeByShape :: ConvertContext -> Env -> XmlfTerm -> XmlfTerm -> Either BackendConversionError (Maybe BackendType)
inferApplicationFunctionTypeByShape context env fun arg = do
  mbFunTy <- inferBackendTypeByShape context env fun
  case fun of
    ETyInst inner inst
      | Just tyArgs <- appLikeInstantiationTypes inst -> do
          mbInnerTy <- inferBackendTypeByShape context env inner
          case mbInnerTy of
            Just innerTy -> do
              explicitArgs0 <- traverse convertElabType tyArgs
              mbArgTy <- inferBackendTypeByShape context env arg
              let explicitArgs = map (normalizeBackendTypeForContext context) explicitArgs0
                  refinedFunTy = do
                    argTy <- mbArgTy
                    chosenArgs <- chooseTypeApplicationsForArgument context argTy innerTy explicitArgs
                    Just (applyBackendTypeArgumentsToType context innerTy chosenArgs)
              pure (refinedFunTy <|> mbFunTy)
            Nothing ->
              pure mbFunTy
    _ ->
      pure mbFunTy

inferResolvedBackendType :: ConvertContext -> Env -> ResolvedVar -> Either BackendConversionError BackendType
inferResolvedBackendType context env resolved =
  case lookupResolvedTermEnvEntry (resolvedTermEnv env) resolved of
    Just (_, envTy) -> normalizeBackendTypeForContext context <$> convertElabType envTy
    Nothing
      | resolvedVarIsLocal resolved ->
          Left (BackendUnsupportedCaseShape "unbound local resolved variable")
    Nothing -> normalizeBackendTypeForContext context <$> convertElabType (resolvedVarType resolved)

applyBackendInstantiation :: ConvertContext -> BackendType -> Instantiation -> Either BackendConversionError (Maybe BackendType)
applyBackendInstantiation context funTy =
  \case
    InstId -> Right (Just funTy)
    InstApp ty -> applyBackendTypeArgument ty
    InstBot ty -> applyBackendTypeArgument ty
    InstIntro -> Right (Just funTy)
    InstElim -> Right (Just funTy)
    InstInside inst -> applyBackendInstantiation context funTy inst
    InstSeq left right -> do
      mbLeftTy <- applyBackendInstantiation context funTy left
      case mbLeftTy of
        Just leftTy -> applyBackendInstantiation context leftTy right
        Nothing -> Right Nothing
    InstAbstrRef {} -> Right (Just funTy)
    InstUnderRef _ inst -> applyBackendInstantiation context funTy inst
  where
    applyBackendTypeArgument ty =
      case funTy of
        BTForallWithIdentity identity name _ body -> do
          argTy <- normalizeBackendTypeForContext context <$> convertElabType ty
          Right (Just (substituteBackendTypeForBinder identity name argTy body))
        _ -> Right Nothing

reconcileBackendLocalResolvedTypes :: ConvertContext -> Env -> XmlfTerm -> XmlfTerm
reconcileBackendLocalResolvedTypes context env0 =
  goTerm env0 (resolvedTermEnv env0)
  where
    goTerm env resolvedEnv term =
      case term of
        EVarNode resolved ->
          EVarNode (reconcileResolved resolvedEnv resolved)
        ELit lit ->
          ELit lit
        ELam resolved body ->
          let (env', resolvedEnv') = extendLocalTerm resolved (resolvedVarType resolved) env resolvedEnv
           in ELam resolved (goTerm env' resolvedEnv' body)
        EApp fun arg ->
          EApp (goTerm env resolvedEnv fun) (goTerm env resolvedEnv arg)
        ELet resolved scheme rhs body ->
          let schemeTy = schemeToType scheme
              (env', resolvedEnv') = extendLocalTerm resolved schemeTy env resolvedEnv
           in ELet resolved scheme (goTerm env' resolvedEnv' rhs) (goTerm env' resolvedEnv' body)
        ETyAbsRef ref mbBound body ->
          ETyAbsRef ref mbBound (goTerm (extendLocalTypeRef ref mbBound env) resolvedEnv body)
        ETyInst inner inst ->
          ETyInst (goTerm env resolvedEnv inner) inst
        ERoll ty body ->
          ERoll ty (goTerm env resolvedEnv body)
        EUnroll body ->
          EUnroll (goTerm env resolvedEnv body)

    reconcileResolved resolvedEnv resolved =
      case lookupResolvedTermEnvEntry resolvedEnv resolved of
        Just (_, envTy)
          | not (resolvedVarIsLocal resolved) ->
              mapResolvedVarType (const envTy) resolved
        Just (_, envTy)
          | backendElabTypesCompatible context envTy (resolvedVarType resolved) ->
              mapResolvedVarType (const envTy) resolved
        _ -> resolved

    extendLocalTerm resolved ty env resolvedEnv =
      (insertResolvedTermBinding resolved ty env, insertResolvedTermEnv resolved ty resolvedEnv)

    extendLocalTypeRef ref mbBound env =
      insertTypeBindingRef ref (maybe TBottom tyToElab mbBound) env

backendTypesCompatible :: ConvertContext -> BackendType -> BackendType -> Bool
backendTypesCompatible context leftTy rightTy =
  ( not (backendTypeContainsMu leftTy || backendTypeContainsMu rightTy)
      && alphaEqBackendType leftTy rightTy
  )
    || or
      [ not (Structural.structuralMuTypesHaveBinderIdentityMismatch leftCandidate rightCandidate)
          && ( alphaEqBackendType leftCandidate rightCandidate
                 || Structural.backendStructuralDataBoundaryMatches Map.empty (Just dataScope) leftCandidate rightCandidate
                 || nominalStructuralHeadsMatch dataDecls leftCandidate rightCandidate
             )
      | leftCandidate <- backendTypeCompatibilityVariants context leftTy,
        rightCandidate <- backendTypeCompatibilityVariants context rightTy
      ]
  where
    dataDecls =
      Map.fromList
        [ (backendDataName dataDecl, dataDecl)
        | dataMeta <- ccData context,
          let dataDecl = dmBackend dataMeta,
          backendDataIdentity dataDecl == Nothing
        ]
    dataDeclsByIdentity =
      Map.fromList
        [ (identity, dataDecl)
        | dataMeta <- ccData context,
          let dataDecl = dmBackend dataMeta,
          Just identity <- [backendDataIdentity dataDecl]
        ]
    dataScope =
      Structural.backendDataScope dataDecls dataDeclsByIdentity

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

backendTypeCompatibilityVariants :: ConvertContext -> BackendType -> [BackendType]
backendTypeCompatibilityVariants context ty =
  nub $
    [ ty,
      structuralCanonicalTy,
      normalizedTy
    ]
      ++ [ recoveredTy
         | backendTypeNeedsStructuralRecovery context structuralCanonicalTy
         ]
  where
    structuralCanonicalTy =
      canonicalizeStructuralMuNames context ty
    recoveredTy =
      recoverStructuralBackendType context structuralCanonicalTy
    normalizedTy =
      normalizeBackendTypeForContext context ty

nominalStructuralHeadsMatch :: Map String BackendData -> BackendType -> BackendType -> Bool
nominalStructuralHeadsMatch dataDecls leftTy rightTy =
  nominalStructuralHeadMatches dataDecls leftTy rightTy || nominalStructuralHeadMatches dataDecls rightTy leftTy

nominalStructuralHeadMatches :: Map String BackendData -> BackendType -> BackendType -> Bool
nominalStructuralHeadMatches dataDecls nominal structural =
  case (nominal, structural) of
    (BTBaseWithIdentity nominalIdentity (BaseTy nominalName), BTMuWithIdentity structuralIdentity structuralName _) ->
      nominalHeadMatchesStructuralName nominalIdentity nominalName structuralIdentity structuralName
    (BTConWithIdentity nominalIdentity (BaseTy nominalName) _, BTMuWithIdentity structuralIdentity structuralName _) ->
      nominalHeadMatchesStructuralName nominalIdentity nominalName structuralIdentity structuralName
    _ ->
      False
  where
    nominalHeadMatchesStructuralName nominalIdentity nominalName structuralIdentity structuralName =
      case Structural.structuralRecursiveDataName structuralName of
        Just dataName ->
          case nominalIdentity of
            Just identity ->
              structuralIdentity == Nothing && (Map.lookup dataName dataDecls >>= backendDataIdentity) == Just identity
            Nothing ->
              structuralIdentity == Nothing
                && dataName == nominalName
                && maybe True ((== Nothing) . backendDataIdentity) (Map.lookup dataName dataDecls)
        Nothing ->
          False

backendElabTypesCompatible :: ConvertContext -> ElabType -> ElabType -> Bool
backendElabTypesCompatible context left right =
  case (convertElabType left, convertElabType right) of
    (Right leftTy, Right rightTy) ->
      backendTypesCompatible context leftTy rightTy
    _ ->
      False

extendResolvedTermEnv :: ResolvedVar -> ElabType -> Env -> Env
extendResolvedTermEnv resolved ty env =
  let ty' = normalizeBuiltinElabType ty
   in insertResolvedTermBinding (mapResolvedVarType (const ty') resolved) ty' env

extendTypeEnv :: TypeBinderRef -> ElabType -> Env -> Env
extendTypeEnv ref ty env =
  insertTypeBindingRef ref (normalizeBuiltinElabType ty) env

backendTypeBoundsFromEnv :: Env -> Either BackendConversionError BackendTypeBounds
backendTypeBoundsFromEnv env =
  Map.fromList . concat <$> traverse convertEntry entries
  where
    entries = Map.toList (typeEnv env)

    names =
      canonicalBackendTypeBinderNamesFromRefs $
        map fst entries ++ concatMap (elabTypeBinderRefs . snd) entries

    convertEntry (ref, boundTy) = do
      bound <- convertTypeBound boundTy
      let identityKey = backendTypeSubstitutionKeyFromIdentity (typeBinderRefIdentity ref)
      Right [(identityKey, bound)]

    convertTypeBound TBottom = Right Nothing
    convertTypeBound boundTy = Just <$> convertElabTypeWith names boundTy

zipWithMCase ::
  (BackendConstructor -> XmlfTerm -> ConvertM BackendAlternative) ->
  [BackendConstructor] ->
  [XmlfTerm] ->
  ConvertM (NonEmpty BackendAlternative)
zipWithMCase f constructors handlers =
  case zipWith f constructors handlers of
    firstAlt : restAlts ->
      (:|) <$> firstAlt <*> sequence restAlts
    [] ->
      liftEitherConvert (Left (BackendUnsupportedCaseShape "case expression has no alternatives"))

matchBackendTypeParameters ::
  BackendTypeBounds ->
  [BackendDataParameterRef] ->
  BackendParameterBounds ->
  BackendParameterSubstitution ->
  BackendType ->
  BackendType ->
  Maybe BackendParameterSubstitution
matchBackendTypeParameters =
  matchBackendTypeParametersWithDataIdentity Nothing

matchBackendTypeParametersWithDataIdentity ::
  Maybe SymbolIdentity ->
  BackendTypeBounds ->
  [BackendDataParameterRef] ->
  BackendParameterBounds ->
  BackendParameterSubstitution ->
  BackendType ->
  BackendType ->
  Maybe BackendParameterSubstitution
matchBackendTypeParametersWithDataIdentity dataIdentity typeBounds dataParameterOrder parameterBounds =
  go Map.empty Map.empty
  where
    matchParameterKey identity name =
      case identity of
        Just {} ->
          if Map.member key parameterBounds
            then Just key
            else Nothing
        Nothing
          | Map.member key parameterBounds -> Just key
          | otherwise -> Nothing
      where
        key = backendTypeSubstitutionKeyFor identity name

    go leftEnv rightEnv substitution expected actual
      | Structural.structuralMuTypesHaveBinderIdentityMismatch expected actual =
          Nothing
      | otherwise =
          case expected of
            BTVarWithIdentity identity name
              | Just key <- matchParameterKey identity name,
                Map.notMember key leftEnv ->
                  insertParameterSubstitution key actual substitution
            _ ->
              case (expected, actual) of
                (BTVarWithIdentity expectedIdentity expectedName, BTVarWithIdentity actualIdentity actualName)
                  | sameTypeVar leftEnv rightEnv expectedIdentity expectedName actualIdentity actualName ->
                      Just substitution
                (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                  go leftEnv rightEnv substitution expectedDom actualDom
                    >>= \substitution' -> go leftEnv rightEnv substitution' expectedCod actualCod
                (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase)
                  | backendTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase ->
                    Just substitution
                (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs)
                  | backendTypeHeadMatches expectedIdentity expectedCon actualIdentity actualCon,
                    length expectedArgs == length actualArgs ->
                      foldM
                        ( \substitutionAcc (expectedArg, actualArg) ->
                            go leftEnv rightEnv substitutionAcc expectedArg actualArg
                        )
                        substitution
                        (zip (NE.toList expectedArgs) (NE.toList actualArgs))
                (BTMuWithIdentity expectedIdentity expectedName expectedBody, actualTy@(BTBase {})) ->
                  matchStructuralMuExpected leftEnv rightEnv substitution expectedIdentity expectedName expectedBody actualTy
                (BTMuWithIdentity expectedIdentity expectedName expectedBody, actualTy@(BTCon {})) ->
                  matchStructuralMuExpected leftEnv rightEnv substitution expectedIdentity expectedName expectedBody actualTy
                (expectedTy@(BTBase {}), BTMuWithIdentity actualIdentity actualName actualBody) ->
                  matchStructuralMuActual leftEnv rightEnv substitution expectedTy actualIdentity actualName actualBody
                (expectedTy@(BTCon {}), BTMuWithIdentity actualIdentity actualName actualBody) ->
                  matchStructuralMuActual leftEnv rightEnv substitution expectedTy actualIdentity actualName actualBody
                (BTVarAppWithIdentity expectedIdentity expectedName expectedArgs, _) ->
                  matchBackendTypeApplication leftEnv rightEnv substitution expectedIdentity expectedName (NE.toList expectedArgs) actual
                (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity actualName actualBound actualBody) -> do
                  substitution' <-
                    case (expectedBound, actualBound) of
                      (Nothing, Nothing) -> Just substitution
                      (Just expectedBoundTy, Just actualBoundTy) -> go leftEnv rightEnv substitution expectedBoundTy actualBoundTy
                      _ -> Nothing
                  let actualBody' =
                        substituteBackendTypeForBinder
                          actualIdentity
                          actualName
                          (BTVarWithIdentity expectedIdentity expectedName)
                          actualBody
                  go leftEnv rightEnv substitution' expectedBody actualBody'
                (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
                  let actualBody' =
                        substituteBackendTypeForBinder
                          actualIdentity
                          actualName
                          (BTVarWithIdentity expectedIdentity expectedName)
                          actualBody
                   in go leftEnv rightEnv substitution expectedBody actualBody'
                (BTBottom, BTBottom) ->
                  Just substitution
                _ ->
                  Nothing

    matchBackendTypeApplication leftEnv rightEnv substitution identity name expectedArgs actual =
      case Structural.decomposeBackendTypeHead actual of
        Just (actualHead, actualArgs)
          | length expectedArgs == length actualArgs -> do
              substitution' <-
                case matchParameterKey identity name of
                  Just key
                    | Map.notMember key leftEnv ->
                        insertParameterSubstitution key actualHead substitution
                  _ ->
                    go leftEnv rightEnv substitution (BTVarWithIdentity identity name) actualHead
              foldM
                (\substitutionAcc (expectedArg, actualArg) -> go leftEnv rightEnv substitutionAcc expectedArg actualArg)
                substitution'
                (zip expectedArgs actualArgs)
        _ -> Nothing

    matchStructuralMuExpected leftEnv rightEnv substitution muIdentity muName body actualTy =
      ( structuralMuAsDataTypeForBody muIdentity muName body
          >>= \expectedTy -> go leftEnv rightEnv substitution expectedTy actualTy
      )
        <|> ( Structural.structuralMuPayloadTypes body
                *> Structural.structuralMuAsActualDataType dataIdentity muIdentity muName actualTy
                >>= \expectedTy -> go leftEnv rightEnv substitution expectedTy actualTy
            )

    matchStructuralMuActual leftEnv rightEnv substitution expectedTy muIdentity muName body =
      ( structuralMuAsDataTypeForBody muIdentity muName body
          >>= \actualTy -> go leftEnv rightEnv substitution expectedTy actualTy
      )
        <|> ( Structural.structuralMuPayloadTypes body
                *> Structural.structuralMuAsActualDataType dataIdentity muIdentity muName expectedTy
                >>= \actualTy -> go leftEnv rightEnv substitution expectedTy actualTy
            )

    structuralMuAsDataTypeForBody muIdentity muName body =
      Structural.structuralMuPayloadTypes body *> Structural.structuralMuAsDataType dataIdentity dataParameterOrder muIdentity muName

    sameTypeVar leftEnv rightEnv expectedIdentity expectedName actualIdentity actualName =
      case (Map.lookup expectedKey leftEnv, Map.lookup actualKey rightEnv) of
        (Just expectedActual, Just actualExpected) -> expectedActual == actualKey && actualExpected == expectedKey
        (Nothing, Nothing) -> expectedKey == actualKey
        _ -> False
      where
        expectedKey = backendTypeSubstitutionKeyFor expectedIdentity expectedName
        actualKey = backendTypeSubstitutionKeyFor actualIdentity actualName

    insertParameterSubstitution key actual substitution =
      case Map.lookup key substitution of
        Nothing ->
          if backendParameterBoundMatches key actual substitution
            then Just (Map.insert key actual substitution)
            else Nothing
        Just previous
          | parameterPlaceholderMatchesKey key previous,
            backendParameterBoundMatches key actual substitution ->
              Just (Map.insert key actual substitution)
        Just previous
          | explicitParameterSubstitutionMatches previous actual
              && backendParameterBoundMatches key previous substitution ->
              Just substitution
        _ -> Nothing

    backendParameterBoundMatches key actual substitution =
      case Map.lookup key parameterBounds of
        Just (Just _)
          | actualBackendTypeVarMatchesKey key actual ->
              True
        Just (Just boundTy)
          | not (alphaEqBackendType boundTy BTBottom) ->
              let dependencySubstitution =
                    completeBackendParameterSubstitution
                      (Map.delete key parameterBounds)
                      (Map.delete key substitution)
                  expectedBound = substituteBackendTypesByKey dependencySubstitution boundTy
               in typeBoundDependenciesMatch actual expectedBound || actualTypeVariableBoundMatches actual expectedBound
        _ ->
          True

    actualBackendTypeVarMatchesKey key =
      \case
        BTVarWithIdentity identity name ->
          backendTypeSubstitutionKeyFor identity name == key
        _ ->
          False

    parameterPlaceholderMatchesKey key =
      \case
        BTVarWithIdentity identity name ->
          matchParameterKey identity name == Just key
        _ ->
          False

    explicitParameterSubstitutionMatches previous actual =
      not (Structural.structuralMuTypesHaveBinderIdentityMismatch previous actual)
        && (alphaEqBackendType previous actual || typeBoundDependenciesMatch previous actual)

    typeBoundDependenciesMatch actual expectedBound =
      alphaEqBackendType
        (resolveTypeBoundDependencies actual)
        (resolveTypeBoundDependencies expectedBound)

    actualTypeVariableBoundMatches actual expectedBound =
      case actual of
        BTVarWithIdentity actualIdentity actualName ->
          case Structural.lookupTypeBound actualIdentity actualName typeBounds of
            Just (Just actualBound) ->
              typeBoundDependenciesMatch actualBound expectedBound
            _ ->
              False
        _ ->
          False

    resolveTypeBoundDependencies =
      substituteBackendTypesByKey resolvedTypeBounds

    resolvedTypeBounds =
      completeBackendParameterSubstitution (typeBoundsAsParameterBounds typeBounds) Map.empty

completeBackendParameterSubstitution :: BackendParameterBounds -> BackendParameterSubstitution -> BackendParameterSubstitution
completeBackendParameterSubstitution =
  Structural.completeBackendParameterSubstitution

completeDataParameterSubstitution :: BackendData -> BackendParameterSubstitution -> BackendParameterSubstitution
completeDataParameterSubstitution =
  Structural.completeDataParameterSubstitution
