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
    convertSourceType,
    renderBackendConversionError,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, unless, when, zipWithM)
import Control.Monad.State.Strict (StateT (StateT), evalStateT, get, modify, runStateT)
import Data.Char (isAlphaNum)
import Data.List (find, intercalate, nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
    freshTypeBinderRef,
    TypeCheckError,
    elabToBound,
    identityGeneratorAfterType,
    identityGeneratorAfterTerm,
    generatedIdentitiesInTerm,
    localResolvedVarFromRef,
    mapResolvedVarType,
    renameResolvedLocalVar,
    renameTypeBinderRef,
    resolvedVarBoundBy,
    resolvedVarConstructorRef,
    resolvedVarIsLocal,
    resolvedVarReferenceName,
    resolvedVarSameIdentity,
    resolvedVarType,
    schemeFromType,
    tyToElab,
  )

import MLF.Frontend.Program.Builtins (builtinTypeHeadIdentity, builtinValueIdentity, normalizeBuiltinTypeReference, srcTypeMentionsOpaqueBuiltin)
import MLF.Frontend.Program.Elaborate (ElaborateScope, elaborateScopeDataTypes, lowerType, lowerTypeView, mkElaborateScope)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ConstructorRef (..),
    ConstructorInfo (..),
    DataInfo (..),
    DeferredMethodCall (..),
    DeferredMethodEvidence (..),
    DeferredProgramObligation (..),
    EvidenceInfo (..),
    EvidenceMethod (..),
    ResolvedModule (..),
    ResolvedProgram (..),
    ResolvedScope (..),
    ResolvedSymbol (..),
    SymbolNamespace (..),
    SymbolIdentity (..),
    TypeView (..),
    checkedBindingConstructorRef,
    checkedProgramMain,
    constructorRefFromInfo,
    dataInfoIdentityName,
    dataInfoIdentityQualifiedName,
    dataParams,
    typeParamBinderIdentity,
    mkTypeView,
    resolvedModuleIdentity,
    resolvedModuleScope,
    splitArrows,
    splitForalls,
  )
import MLF.Frontend.Symbol (symbolIdentityStableName)
import MLF.Frontend.Syntax (Lit, SrcBound (..), SrcTy (..), SrcType)
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (DeferredRef (..), IdDetails (..), IdentityGenerator, LocalRef, PrimitiveRef (..), TypeBinderIdentity (..), freshDeferredRef, freshLocalRef, idDetailsGeneratedIdentities, idDetailsSameIdentity, identityGeneratorAfter, initialIdentityGenerator)
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
    ccEvidenceResolvedVars :: [ResolvedVar],
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
  { csNextClosureIndex :: Int,
    csGeneratedClosureNames :: Set.Set String,
    csIdentityGenerator :: IdentityGenerator
  }

type ConvertM = StateT ConvertState (Either BackendConversionError)

data ClosureScope = ClosureScope
  { closureScopeResolvedTerms :: [ResolvedVar],
    closureScopeBoundResolvedTerms :: [ResolvedVar],
    closureScopeLocalResolvedTerms :: [ResolvedVar],
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
      closureScopeClosureValueArgumentsByLocal = Map.empty,
      closureScopeEvidenceValueArgumentsByLocal = Map.empty
    }

extendClosureScopeResolvedTerm :: ResolvedVar -> ElabType -> Bool -> ClosureScope -> ClosureScope
extendClosureScopeResolvedTerm resolved ty isClosure scope =
  let resolved' = mapResolvedVarType (const ty) resolved
   in scope
        { closureScopeResolvedTerms =
            resolved' : filter (not . resolvedVarSameIdentity resolved') (closureScopeResolvedTerms scope),
          closureScopeBoundResolvedTerms =
            resolved' : filter (not . resolvedVarSameIdentity resolved') (closureScopeBoundResolvedTerms scope),
          closureScopeLocalResolvedTerms =
            if isClosure
              then resolved' : filter (not . resolvedVarSameIdentity resolved') (closureScopeLocalResolvedTerms scope)
              else filter (not . resolvedVarSameIdentity resolved') (closureScopeLocalResolvedTerms scope),
          closureScopeClosureValueArgumentsByLocal =
            maybe id Map.delete (resolvedVarLocalRef resolved') (closureScopeClosureValueArgumentsByLocal scope),
          closureScopeEvidenceValueArgumentsByLocal =
            maybe id Map.delete (resolvedVarLocalRef resolved') (closureScopeEvidenceValueArgumentsByLocal scope)
        }

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

closureScopeLocalNames :: ClosureScope -> Set.Set String
closureScopeLocalNames =
  Set.fromList . map resolvedVarReferenceName . closureScopeLocalResolvedTerms

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
  any (resolvedVarSameIdentity resolved) (ccEvidenceResolvedVars context)

runConvertM :: IdentityGenerator -> ConvertM a -> Either BackendConversionError a
runConvertM generator action =
  evalStateT
    action
    ConvertState
      { csNextClosureIndex = 0,
        csGeneratedClosureNames = Set.empty,
        csIdentityGenerator = generator
      }

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
  modules0 <- mapM (convertCheckedModule context initialEnv) (checkedProgramModules checked)
  let program =
        BackendProgram
          { backendProgramModules = modules0,
            backendProgramMain = checkedProgramMain checked
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
                        converted <- convertCheckedBinding context env checkedModule binding
                        pure [(binding, convertedBinding) | convertedBinding <- converted]
                    )
                    (checkedModuleBindings checkedModule)
            )
            (checkedProgramModules checked)
      let globalIdentities' =
            Set.fromList
              [ symbol
              | (binding, convertedBinding) <- convertedBindings,
                backendExprIsClosureValue context emptyClosureScope (backendBindingExpr convertedBinding),
                Just symbol <- [checkedBindingSymbolIdentity binding]
              ]
      if globalIdentities' == globalIdentities
        then pure globalIdentities
        else closureGlobalFixedPoint globalIdentities'

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
        `unionEnvs` mkTypeCheckEnvWithResolvedTerms backendBuiltinResolvedTermTypes Map.empty
    )

checkedBindingEnvType :: ConvertContext -> CheckedModule -> CheckedBinding -> Either BackendConversionError ElabType
checkedBindingEnvType context checkedModule binding = do
  canonicalElabTyOpen <- checkedBindingCanonicalTypeOpen context checkedModule binding
  let freeTypeBinders = sortTypeBinderRefsByName (freeElabTypeVarRefs canonicalElabTyOpen)
      canonicalElabTy = quantifyFreeElabTypeVarRefs freeTypeBinders canonicalElabTyOpen
  rawBackendTy <- convertElabType canonicalElabTy
  let sourceBindingTy =
        canonicalizeBackendType context $
          applySourceTypeIdentity
            context
            (scopeForModule context (checkedModuleIdentity checkedModule))
            (checkedBindingSourceType binding)
            rawBackendTy
      finalBindingTy =
        case constructorMetaForBinding context binding of
          Just constructorMeta
            | constructorBindingResultMatches sourceBindingTy constructorMeta,
              backendConstructorContainsVarApp (cmBackend constructorMeta) ->
                constructorBackendBindingType constructorMeta
          _ ->
            sourceBindingTy
  case backendTypeToElabTypeWithGenerator (identityGeneratorAfterType canonicalElabTy) finalBindingTy of
    Just envTy -> Right envTy
    Nothing -> Right canonicalElabTy

backendBuiltinTermTypes :: Map String ElabType
backendBuiltinTermTypes =
  PrimitiveInventory.primitiveValueElabTypes

backendBuiltinResolvedTermTypes :: [(ResolvedVar, ElabType)]
backendBuiltinResolvedTermTypes =
  [ ( builtinResolvedVar name ty,
      ty
    )
  | (name, ty) <- Map.toList backendBuiltinTermTypes
  ]

builtinResolvedVar :: String -> ElabType -> ResolvedVar
builtinResolvedVar name ty =
  ResolvedVar
    { resolvedVarRuntimeName = name,
      resolvedVarType = ty,
      resolvedVarDetails = TopLevelId (builtinValueIdentity name)
    }

convertCheckedModule :: ConvertContext -> Env -> CheckedModule -> Either BackendConversionError BackendModule
convertCheckedModule context env checkedModule = do
  dataDecls <- mapM (convertDataInfo context) (Map.elems (checkedModuleData checkedModule))
  bindings <-
    concat
      <$> mapM
        (convertCheckedBinding context env checkedModule)
        (checkedModuleBindings checkedModule)
  Right
    BackendModule
      { backendModuleName = checkedModuleName checkedModule,
        backendModuleData = dataDecls,
        backendModuleBindings = bindings
      }

rejectOpaqueBuiltinMain :: CheckedProgram -> Either BackendConversionError ()
rejectOpaqueBuiltinMain _checked =
  Right ()

convertCheckedBinding :: ConvertContext -> Env -> CheckedModule -> CheckedBinding -> Either BackendConversionError [BackendBinding]
convertCheckedBinding context env checkedModule binding = do
  let bindingContext =
        context
          { ccCurrentModuleIdentity = Just (checkedModuleIdentity checkedModule),
            ccCurrentBindingName = checkedBindingRuntimeName binding
          }
  canonicalElabTyOpen <- checkedBindingCanonicalTypeOpen context checkedModule binding
  let freeTypeBinders = sortTypeBinderRefsByName (freeElabTypeVarRefs canonicalElabTyOpen)
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
            (checkedBindingSourceType binding)
            rawBindingTy
  (convertedBindingTy, expr, liftedBindings) <-
    case constructorMetaForBinding context binding of
      Just constructorMeta
        | constructorBindingResultMatches bindingTy constructorMeta ->
            do
              let constructorBindingTy =
                    if backendConstructorContainsVarApp (cmBackend constructorMeta)
                      then constructorBackendBindingType constructorMeta
                      else bindingTy
              expr <- synthesizeConstructorBinding constructorBindingTy constructorMeta
              Right (constructorBindingTy, expr, [])
      _ -> do
        (liftedTerm, liftedSpecs) <- liftRecursiveLetsInBinding bindingContext canonicalElabTy checkedBindingTermClosed
        let bindingContextWithLifted =
              extendContextWithLiftedRecursiveLets bindingContext liftedSpecs
        let envWithLifted =
              foldr
                (\lifted acc -> insertResolvedTermBinding (lrlResolved lifted) (lrlElabType lifted) acc)
                env
                liftedSpecs
            opaqueBinding =
              srcTypeMentionsOpaqueBuiltin (checkedBindingSourceType binding)
            expectedBindingTy =
              if opaqueBinding && not (checkedBindingExportedAsMain binding)
                then Nothing
                else Just bindingTy
        (liftedBindings, expr) <-
          runConvertM (convertIdentityGenerator liftedTerm liftedSpecs) $ do
            liftedBindings <-
                zipWith
                  (\lifted converted -> converted {backendBindingEvidenceParamIndices = lrlEvidenceValueArguments lifted})
                  liftedSpecs
                  <$> mapM (convertLiftedRecursiveLet bindingContextWithLifted envWithLifted) liftedSpecs
            expr <- convertTermExpectedMode DirectLambda bindingContextWithLifted envWithLifted emptyClosureScope expectedBindingTy liftedTerm
            pure (liftedBindings, expr)
        -- For opaque bindings (types mentioning IO etc.), the expression type
        -- from the builtin is authoritative. Use it to avoid Mu/base mismatches.
        let finalBindingTy =
              if opaqueBinding
                && not (alphaEqBackendType bindingTy (backendExprType expr))
                then backendExprType expr
                else bindingTy
        Right (finalBindingTy, expr, liftedBindings)
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
  Right (convertedBinding : liftedBindings)

convertIdentityGenerator :: XmlfTerm -> [LiftedRecursiveLet] -> IdentityGenerator
convertIdentityGenerator term liftedSpecs =
  identityGeneratorAfter (generatedIdentitiesInTerm term ++ concatMap liftedGeneratedIdentities liftedSpecs)
  where
    liftedGeneratedIdentities lifted =
      idDetailsGeneratedIdentities (resolvedVarDetails (lrlResolved lifted))
        ++ generatedIdentitiesInTerm (lrlTerm lifted)

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

liftRecursiveLetsInBinding :: ConvertContext -> ElabType -> XmlfTerm -> Either BackendConversionError (XmlfTerm, [LiftedRecursiveLet])
liftRecursiveLetsInBinding context bindingTy term = do
  (term', state') <-
    runStateT
      (liftRecursiveLetsInTerm context [] (leadingElabForallCaptures bindingTy) term)
      LiftState
        { lsNextHelperIndex = 0,
          lsLiftedRecursiveLets = [],
          lsGeneratedHelperNames = Set.empty,
          lsIdentityGenerator = identityGeneratorAfterTerm term
        }
  Right (term', lsLiftedRecursiveLets state')

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
      filter (not . resolvedVarSameIdentity resolved)

type TermCapture = (ResolvedVar, ElabType)
type TypeCapture = (TypeBinderRef, Maybe BoundType)

capturedTermBindings :: [ResolvedVar] -> XmlfTerm -> LiftM [TermCapture]
capturedTermBindings lexicalTerms rhs =
  pure (capturedTermBindingsIn lexicalTerms rhs)

capturedTermBindingsIn :: [ResolvedVar] -> XmlfTerm -> [TermCapture]
capturedTermBindingsIn lexicalTerms rhs =
  [ (resolved, resolvedVarType resolved)
  | resolved <- lexicalTerms,
    termMentionsFreeVariable (TermVarResolved resolved) rhs
  ]

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
  SymbolIdentity
    { symbolUniqueIdentity = deferredRefIdentity ref,
      symbolNamespace = SymbolValue,
      symbolDefiningModule = maybe "" symbolDefiningModule (ccCurrentModuleIdentity context),
      symbolDefiningName = deferredRefName ref,
      symbolOwnerIdentity = Nothing
    }

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

freeXmlfTermTypeVars :: XmlfTerm -> Set.Set String
freeXmlfTermTypeVars =
  typeRefNameSet . freeXmlfTermTypeVarRefs

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

sortTypeBinderRefsByName :: [TypeBinderRef] -> [TypeBinderRef]
sortTypeBinderRefsByName =
  sortOn typeBinderRefName

typeRefNameSet :: [TypeBinderRef] -> Set.Set String
typeRefNameSet =
  Set.fromList . map typeBinderRefName

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
    replacementFreeTerms = freeResolvedTermReferenceNames replacement
    replacementFreeTypes = freeXmlfTermTypeVars replacement

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
          | shouldRenameTermBinder binderName body ->
              let used = Set.unions [termVariableNames body, termVariableNames replacement, Set.singleton needleName]
                  binderName' = freshNameLike binderName used
                  resolved' = renameResolvedLocalVar binderName' resolved
                  body' = renameBoundTermVariable (TermVarResolved resolved) binderName' body
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
          | shouldRenameTermBinder binderName body ->
              let used =
                    Set.unions
                      [ termVariableNames rhs,
                        termVariableNames body,
                        termVariableNames replacement,
                        Set.singleton needleName
                      ]
                  binderName' = freshNameLike binderName used
                  resolved' = renameResolvedLocalVar binderName' resolved
                  body' = renameBoundTermVariable (TermVarResolved resolved) binderName' body
               in ELet resolved' scheme (go rhs) (go body')
          | otherwise ->
              ELet resolved scheme (go rhs) (go body)
          where
            binderName = resolvedVarReferenceName resolved
        ETyAbsRef ref mbBound body
          | shouldRenameTypeBinder name body ->
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

    shouldRenameTermBinder name body =
      Set.member name replacementFreeTerms && termMentionsFreeVariable needle body

    shouldRenameTypeBinder name body =
      Set.member name replacementFreeTypes && termMentionsFreeVariable needle body

renameBoundTermVariable :: TermVarKey -> String -> XmlfTerm -> XmlfTerm
renameBoundTermVariable old new =
  go
  where
    go =
      \case
        EVarNode resolved
          | termVarKeyMatchesLocalOccurrence old resolved ->
              EVarNode (renameResolvedLocalVar new resolved)
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
  checkedBackendTy <- convertElabType checkedTy
  case sourceTypeToElabTypeWithGenerator (typeHeadIdentitiesInScope scope) (identityGeneratorAfterType checkedTy) (lowerType scope (checkedBindingSourceType binding)) of
    Left _ ->
      Right checkedTy
    Right canonicalTy0 -> do
      let canonicalTy = normalizeBuiltinElabType canonicalTy0
      canonicalBackendTy <- convertElabType canonicalTy
      let strippedCheckedBackendTy = stripVacuousBackendForalls checkedBackendTy
      if alphaEqBackendType checkedBackendTy canonicalBackendTy
        then Right canonicalTy
        else
          if alphaEqBackendType (normalizeBuiltinBackendType strippedCheckedBackendTy) (normalizeBuiltinBackendType canonicalBackendTy)
            then maybe (Right checkedTy) Right (backendTypeToElabTypeWithGenerator (identityGeneratorAfterType checkedTy) strippedCheckedBackendTy)
            else
              case (checkedBackendTy, canonicalBackendTy) of
                (BTVar {}, BTVar {}) -> Right checkedTy
                (BTVar {}, _) -> Right canonicalTy
                _ -> Right checkedTy

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
       in if Set.member name (freeBackendTypeVars body')
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

sourceTypeToElabTypeWithGenerator :: Map String SymbolIdentity -> IdentityGenerator -> SrcTy n v -> Either BackendConversionError ElabType
sourceTypeToElabTypeWithGenerator headIdentities generator0 ty =
  let (refs, generator) = freshTypeBinderRefs (Set.toList (freeSourceTypeVars ty)) generator0
   in fst <$> sourceTypeToElabTypeFrom headIdentities refs generator ty

freeSourceTypeVars :: SrcTy n v -> Set.Set String
freeSourceTypeVars ty =
  go Set.empty ty
  where
    go :: Set.Set String -> SrcTy n0 v0 -> Set.Set String
    go bound srcTy =
      case srcTy of
        STVar name
          | name `Set.member` bound -> Set.empty
          | otherwise -> Set.singleton name
        STArrow dom cod -> go bound dom `Set.union` go bound cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap (go bound) args
        STVarApp name args ->
          let headVars =
                if name `Set.member` bound
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` foldMap (go bound) args
        STTyLam name body -> go (Set.insert name bound) body
        STTyApp fun arg -> go bound fun `Set.union` go bound arg
        STForall name mb body ->
          maybe Set.empty (go bound . unSrcBound) mb
            `Set.union` go (Set.insert name bound) body
        STMu name body -> go (Set.insert name bound) body
        STBottom -> Set.empty

freshTypeBinderRefs :: [String] -> IdentityGenerator -> (Map String TypeBinderRef, IdentityGenerator)
freshTypeBinderRefs names generator0 =
  go names Map.empty generator0
  where
    go [] refs generator = (refs, generator)
    go (name : rest) refs generator =
      let (ref, generator1) = freshTypeBinderRef name generator
       in go rest (Map.insert name ref refs) generator1

sourceTypeToElabTypeFrom ::
  Map String SymbolIdentity ->
  Map String TypeBinderRef ->
  IdentityGenerator ->
  SrcTy n v ->
  Either BackendConversionError (ElabType, IdentityGenerator)
sourceTypeToElabTypeFrom headIdentities env generator ty =
  case ty of
    STVar name -> do
      ref <- sourceTypeBinderRef env name
      Right (TVarRef ref, generator)
    STArrow dom cod -> do
      (dom', generator1) <- sourceTypeToElabTypeFrom headIdentities env generator dom
      (cod', generator2) <- sourceTypeToElabTypeFrom headIdentities env generator1 cod
      Right (TArrow dom' cod', generator2)
    STBase name ->
      Right (TBaseWithIdentity (sourceTypeHeadIdentity name) (BaseTy (normalizeBuiltinTypeReference name)), generator)
    STCon name args -> do
      (args', generator') <- sourceTypesToElabTypesFrom env generator args
      Right (TConWithIdentity (sourceTypeHeadIdentity name) (BaseTy (normalizeBuiltinTypeReference name)) args', generator')
    STVarApp name args -> do
      (args', generator') <- sourceTypesToElabTypesFrom env generator args
      ref <- sourceTypeBinderRef env name
      Right (TVarAppRef ref args', generator')
    STTyLam {} -> Left (BackendUnsupportedCaseShape "residual type lambda reached backend conversion")
    STTyApp {} -> Left (BackendUnsupportedCaseShape "residual type application reached backend conversion")
    STForall name mb body -> do
      let (ref, generator1) = freshTypeBinderRef name generator
      (mb', generator2) <- maybe (Right (Nothing, generator1)) (sourceBoundToElabBoundFrom headIdentities env generator1) mb
      (body', generator3) <- sourceTypeToElabTypeFrom headIdentities (Map.insert name ref env) generator2 body
      Right (TForallRef ref mb' body', generator3)
    STMu name body -> do
      let (ref, generator1) = freshTypeBinderRef name generator
      (body', generator2) <- sourceTypeToElabTypeFrom headIdentities (Map.insert name ref env) generator1 body
      Right (TMuRef ref body', generator2)
    STBottom -> Right (TBottom, generator)
  where
    sourceTypeBinderRef refs name =
      case Map.lookup name refs of
        Just ref -> Right ref
        Nothing -> Left (BackendUnsupportedCaseShape ("unresolved source type binder `" ++ name ++ "` reached backend conversion"))

    sourceTypeHeadIdentity name =
      Map.lookup name headIdentities <|> builtinTypeHeadIdentity name

    sourceTypesToElabTypesFrom refs generator0 (arg :| args) = do
      (arg', generator1) <- sourceTypeToElabTypeFrom headIdentities refs generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- sourceTypeToElabTypeFrom headIdentities refs gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

sourceBoundToElabBoundFrom ::
  Map String SymbolIdentity ->
  Map String TypeBinderRef ->
  IdentityGenerator ->
  SrcBound n ->
  Either BackendConversionError (Maybe BoundType, IdentityGenerator)
sourceBoundToElabBoundFrom headIdentities env generator (SrcBound boundTy) = do
  (boundTy', generator') <- sourceTypeToElabTypeFrom headIdentities env generator boundTy
  Right (elabTypeToBoundType boundTy', generator')

elabTypeToBoundType :: ElabType -> Maybe BoundType
elabTypeToBoundType = \case
  TVarRef {} -> Nothing
  TBottom -> Nothing
  TArrow dom cod -> Just (TArrow dom cod)
  TBaseWithIdentity identity base -> Just (TBaseWithIdentity identity base)
  TConWithIdentity identity con args -> Just (TConWithIdentity identity con args)
  TVarAppRef ref args -> Just (TVarAppRef ref args)
  TForallRef ref mb body -> Just (TForallRef ref mb body)
  TMuRef ref body -> Just (TMuRef ref body)

constructorBindingResultMatches :: BackendType -> ConstructorMeta -> Bool
constructorBindingResultMatches bindingTy constructorMeta =
  case matchBackendTypeParameters Map.empty dataParameters parameters Map.empty (backendConstructorResult constructor) resultTy of
    Just _ -> True
    Nothing -> False
  where
    constructor = cmBackend constructorMeta
    dataParameters = constructorDataParameters constructorMeta
    parameters = constructorTypeParameters constructorMeta
    (_, bodyTy) = splitBackendForalls bindingTy
    (_, resultTy) = splitBackendArrows bodyTy

synthesizeConstructorBinding :: BackendType -> ConstructorMeta -> Either BackendConversionError BackendExpr
synthesizeConstructorBinding bindingTy constructorMeta = do
  let constructor = cmBackend constructorMeta
      (typeBinders, bodyTy) = splitBackendForalls bindingTy
      (argTys, resultTy) = splitBackendArrows bodyTy
      fields = backendConstructorFields constructor
  unless (length argTys == length fields) $
    Left
      ( BackendUnsupportedCaseShape
          ("constructor binding arity does not match metadata for `" ++ backendConstructorName constructor ++ "`")
      )
  let argNames = ["$" ++ backendConstructorName constructor ++ "_arg" ++ show ix | ix <- [1 .. length argTys]]
      argExprs = zipWith BackendVar argTys argNames
      constructExpr =
        BackendConstructWithIdentity
          { backendExprType = resultTy,
            backendConstructIdentity = backendConstructorIdentity constructor,
            backendConstructName = backendConstructorName constructor,
            backendConstructArgs = argExprs
          }
      expr =
        wrapBackendTypeAbs typeBinders $
          wrapBackendLams (zip argNames argTys) constructExpr
  unless (alphaEqBackendType (backendExprType expr) bindingTy) $
    Left
      ( BackendUnsupportedCaseShape
          ("synthesized constructor binding type does not match checked binding type for `" ++ backendConstructorName constructor ++ "`")
      )
  Right expr

constructorBackendBindingType :: ConstructorMeta -> BackendType
constructorBackendBindingType constructorMeta =
  foldr wrapForall body binders
  where
    constructor = cmBackend constructorMeta
    body =
      foldr BTArrow (backendConstructorResult constructor) (backendConstructorFields constructor)
    binders =
      [ BackendTypeBinder name Nothing
        | name <- backendDataParameters (dmBackend (cmData constructorMeta))
      ]
        ++ backendConstructorForalls constructor

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

wrapBackendLams :: [(String, BackendType)] -> BackendExpr -> BackendExpr
wrapBackendLams params body =
  foldr wrap body params
  where
    wrap (name, paramTy) expr =
      BackendLamWithIdentity
        { backendExprType = BTArrow paramTy (backendExprType expr),
          backendParamIdentity = Nothing,
          backendParamName = name,
          backendParamType = paramTy,
          backendBody = expr
        }

buildConvertContext :: CheckedProgram -> Either BackendConversionError ConvertContext
buildConvertContext checked = do
  let dataInfos = allDataInfos checked
      dataByIdentity = dataInfoIdentityMap dataInfos
      dataModuleIdentities = dataInfoModuleIdentityMap checked
      moduleScopes = moduleElaborateScopes checked dataByIdentity
      termRuntimeNames = checkedProgramTermRuntimeNamesByIdentity checked
  dataMetas <- mapM (buildDataMetaForDataInfo moduleScopes dataModuleIdentities dataInfos) dataInfos
  let constructorMetasByIdentity =
        [ (ctorInfoSymbol (cmInfo constructorMeta), constructorMeta)
          | dataMeta <- dataMetas,
            constructorMeta <- constructorMetasForData dataMeta
        ]
      dataMetasByIdentity =
        Map.fromList
          [ (dataInfoSymbol (dmInfo dataMeta), dataMeta)
          | dataMeta <- dataMetas
          ]
      bindingData = bindingDataHints dataMetasByIdentity checked
  let context0 =
        ConvertContext
          { ccModuleScopes = moduleScopes,
            ccConstructorsByIdentity = Map.fromList constructorMetasByIdentity,
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
            ccEvidenceResolvedVars = [],
            ccCurrentModuleIdentity = Nothing,
            ccCurrentBindingName = ""
          }
  evidenceResolvedVars <- checkedProgramEvidenceResolvedVars context0 checked
  let contextWithEvidence =
        context0
          { ccEvidenceResolvedVars = evidenceResolvedVars
          }
  evidenceValueArguments <- checkedProgramEvidenceValueArguments contextWithEvidence checked
  closureValueArguments <- checkedProgramClosureValueArguments contextWithEvidence checked
  Right
    contextWithEvidence
      { ccClosureValueArgumentsByIdentity = closureValueArguments,
        ccEvidenceValueArgumentsByIdentity = evidenceValueArguments
      }

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
            declaredEvidenceResolvedVars (checkedBindingSourceType binding) bindingTy (checkedBindingTerm binding)
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
          pure (checkedBindingSymbolIdentity binding, checkedBindingSourceType binding, bindingTy, checkedBindingTerm binding)
      )
  pure $
    evidenceValueArgumentFixedPoint context sources Map.empty

evidenceValueArgumentFixedPoint :: ConvertContext -> [(Maybe SymbolIdentity, SrcType, BackendType, XmlfTerm)] -> Map SymbolIdentity (Set.Set Int) -> Map SymbolIdentity (Set.Set Int)
evidenceValueArgumentFixedPoint context sources demands =
  let context' = context {ccEvidenceValueArgumentsByIdentity = demands}
      demands' =
        Map.filter (not . Set.null) $
          Map.fromList
            [ (symbol, checkedBindingEvidenceValueArguments context' emptyClosureScope sourceTy bindingTy term)
            | (Just symbol, sourceTy, bindingTy, term) <- sources
            ]
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
          demands' =
            Map.union
              builtinClosureValueArguments
              ( Map.filter (not . Set.null) $
                  Map.fromList
                    [ (symbol, bindingClosureValueArguments context' emptyClosureScope bindingTy term)
                    | (Just symbol, bindingTy, term) <- sources
                    ]
              )
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
    EVarNode resolved ->
      case resolvedVarDetails resolved of
        DeferredId ref -> Just ref
        _ -> Nothing
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

checkedProgramTermRuntimeNamesByIdentity :: CheckedProgram -> Map SymbolIdentity String
checkedProgramTermRuntimeNamesByIdentity checked =
  Map.fromList checkedBindings `Map.union` builtinBindings
  where
    checkedBindings =
      [ (symbol, checkedBindingRuntimeName binding)
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule,
        Just symbol <- [checkedBindingSymbolIdentity binding]
      ]

    builtinBindings =
      Map.fromList
        [ (builtinValueIdentity name, name)
        | name <- Map.keys PrimitiveInventory.primitiveValueSpecs
        ]

dataInfoIdentityMap :: [DataInfo] -> Map SymbolIdentity DataInfo
dataInfoIdentityMap dataInfos =
  Map.fromList [(dataInfoSymbol info, info) | info <- dataInfos]

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

resolvedVarSymbolIdentity :: ResolvedVar -> Maybe SymbolIdentity
resolvedVarSymbolIdentity resolved =
  case resolvedVarDetails resolved of
    TopLevelId symbol -> Just symbol
    ConstructorId ref -> Just (constructorRefSymbol ref)
    MethodId symbol -> Just symbol
    PrimitiveId ref -> Just (primitiveRefSymbol ref)
    _ -> Nothing

resolvedVarLocalRef :: ResolvedVar -> Maybe LocalRef
resolvedVarLocalRef resolved =
  case resolvedVarDetails resolved of
    LocalId localRef -> Just localRef
    EvidenceId localRef -> Just localRef
    _ -> Nothing

bindingDataHint :: Map SymbolIdentity DataMeta -> CheckedBinding -> Maybe DataMeta
bindingDataHint dataMetasByIdentity binding =
  elabTypeDataMeta dataMetasByIdentity (checkedBindingType binding)
    <|> sourceBindingDataHint dataMetasByIdentity binding

sourceBindingDataHint :: Map SymbolIdentity DataMeta -> CheckedBinding -> Maybe DataMeta
sourceBindingDataHint dataMetasByIdentity binding =
  case splitSourceArrows (dropSourceForalls (checkedBindingSourceType binding)) of
    ([], resultTy) -> sourceTypeDataMeta dataMetasByIdentity resultTy
    _ -> Nothing

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

sourceTypeDataMeta :: Map SymbolIdentity DataMeta -> SrcType -> Maybe DataMeta
sourceTypeDataMeta dataMetasByIdentity ty =
  sourceTypeDataHead ty >>= \name ->
    case filter (sourceTypeHeadMatchesData name) (Map.elems dataMetasByIdentity) of
      [dataMeta] -> Just dataMeta
      _ -> Nothing

sourceTypeHeadMatchesData :: String -> DataMeta -> Bool
sourceTypeHeadMatchesData name dataMeta =
  name == symbolIdentityStableName (dataInfoSymbol (dmInfo dataMeta))
    || name == backendDataName (dmBackend dataMeta)
    || name == qualifiedDataName (dmInfo dataMeta)
    || name == dataInfoIdentityName (dmInfo dataMeta)

applySourceTypeIdentity :: ConvertContext -> ElaborateScope -> SrcType -> BackendType -> BackendType
applySourceTypeIdentity context scope =
  applySourceTypeIdentityWith context scope Map.empty

applySourceTypeIdentityWith :: ConvertContext -> ElaborateScope -> Map String BackendType -> SrcType -> BackendType -> BackendType
applySourceTypeIdentityWith context scope sourceTypeVars sourceTy backendTy =
  case (sourceTy, backendTy) of
    (STArrow sourceDom sourceCod, BTArrow backendDom backendCod) ->
      BTArrow
        (applySourceTypeIdentityWith context scope sourceTypeVars sourceDom backendDom)
        (applySourceTypeIdentityWith context scope sourceTypeVars sourceCod backendCod)
    (STForall sourceName sourceBound sourceBody, BTForallWithIdentity backendIdentity backendName backendBound backendForallBody) ->
      BTForallWithIdentity
        backendIdentity
        backendName
        (applySourceTypeIdentityWith context scope sourceTypeVars (maybe STBottom unSrcBound sourceBound) <$> backendBound)
        (applySourceTypeIdentityWith context scope (Map.insert sourceName (BTVarWithIdentity backendIdentity backendName) sourceTypeVars) sourceBody backendForallBody)
    _
      | backendTypeIsDataLike backendTy,
        let loweredSourceTy = lowerType scope sourceTy,
        Just dataMeta <- sourceTypeDataMeta (ccDataByIdentity context) sourceTy <|> sourceTypeDataMeta (ccDataByIdentity context) loweredSourceTy,
        Just sourceBackendTy0 <- either (const Nothing) Just (convertSourceType sourceTy) <|> either (const Nothing) Just (convertSourceType loweredSourceTy),
        let sourceBackendTy =
              substituteBackendTypes sourceTypeVars $
                canonicalizeSourceBackendTypeHeads (ccDataByIdentity context) sourceBackendTy0,
        Just dataTy <- canonicalDataTypeForSource dataMeta sourceBackendTy ->
          dataTy
    _ ->
      backendTy

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
          case dataMetaByName of
            Just dataMeta -> (Just (dataInfoSymbol (dmInfo dataMeta)), backendDataName (dmBackend dataMeta))
            Nothing -> (Nothing, name)
      where
        dataMetaByName =
          case filter (sourceTypeHeadMatchesData name) (Map.elems dataMetasByIdentity) of
            [dataMeta] -> Just dataMeta
            _ -> Nothing

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
  rawConstructors <- mapM (convertConstructorInfo scope) (dataConstructors info)
  let rawData =
        BackendDataWithIdentity
          { backendDataIdentity = Just (dataInfoSymbol info),
            backendDataNameWithIdentity = qualifiedDataName info,
            backendDataParametersWithIdentity = dataParams info,
            backendDataParameterIdentities = map typeParamBinderIdentity (dataTypeParams info),
            backendDataConstructorsWithIdentity = rawConstructors
          }
      rawMeta =
        DataMeta
          { dmInfo = info,
            dmBackend = rawData
          }
      rawRecoveryContext =
        ConvertContext
          { ccModuleScopes = Map.empty,
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
            ccEvidenceResolvedVars = [],
            ccCurrentModuleIdentity = moduleIdentity,
            ccCurrentBindingName = ""
          }
      canonicalConstructors =
        map (canonicalizeBackendConstructorTypes rawRecoveryContext) rawConstructors
      canonicalData =
        rawData {backendDataConstructors = canonicalConstructors}
      canonicalMeta =
        rawMeta {dmBackend = canonicalData}
      recoveryContext =
        rawRecoveryContext
          { ccDataByIdentity = Map.singleton (dataInfoSymbol info) canonicalMeta,
            ccData = [canonicalMeta]
          }
      constructors =
        if any backendConstructorContainsVarApp rawConstructors
          then map (recoverBackendConstructorTypes recoveryContext) canonicalConstructors
          else canonicalConstructors
  Right
    DataMeta
      { dmInfo = info,
        dmBackend =
          BackendDataWithIdentity
            { backendDataIdentity = Just (dataInfoSymbol info),
              backendDataNameWithIdentity = qualifiedDataName info,
              backendDataParametersWithIdentity = dataParams info,
              backendDataParameterIdentities = map typeParamBinderIdentity (dataTypeParams info),
              backendDataConstructorsWithIdentity = constructors
            }
      }

canonicalizeBackendConstructorTypes :: ConvertContext -> BackendConstructor -> BackendConstructor
canonicalizeBackendConstructorTypes context constructor =
  BackendConstructorWithIdentity
    (backendConstructorIdentity constructor)
    (backendConstructorName constructor)
    (map canonicalizeTypeBinder (backendConstructorForalls constructor))
    (map canonicalizeTy (backendConstructorFields constructor))
    (canonicalizeTy (backendConstructorResult constructor))
  where
    canonicalizeTy =
      canonicalizeSourceBackendTypeHeads (ccDataByIdentity context)
        . canonicalizeStructuralMuNames context

    canonicalizeTypeBinder binder =
      binder {backendTypeBinderBound = fmap canonicalizeTy (backendTypeBinderBound binder)}

recoverBackendConstructorTypes :: ConvertContext -> BackendConstructor -> BackendConstructor
recoverBackendConstructorTypes context constructor =
  BackendConstructorWithIdentity
    (backendConstructorIdentity constructor)
    (backendConstructorName constructor)
    (map recoverTypeBinder (backendConstructorForalls constructor))
    (map recoverTy (backendConstructorFields constructor))
    (recoverTy (backendConstructorResult constructor))
  where
    recoverTy =
      canonicalizeSourceBackendTypeHeads (ccDataByIdentity context)
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
    BTMu name body ->
      maybe False dataMetaNeedsStructuralRecovery (structuralRecursiveDataMeta context name)
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

convertConstructorInfo :: ElaborateScope -> ConstructorInfo -> Either BackendConversionError BackendConstructor
convertConstructorInfo scope info = do
  (forallViews, fieldViews, resultView) <- constructorInfoTypeViews info
  let typeVars = constructorForallTypeVars forallViews
  foralls <- mapM (convertConstructorForallView scope typeVars) forallViews
  fields <- mapM (convertConstructorTypeView scope typeVars) fieldViews
  resultTy <- convertConstructorTypeView scope typeVars resultView
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
  Either BackendConversionError ([(String, String, Maybe TypeBinderIdentity, Maybe TypeView)], [TypeView], TypeView)
constructorInfoTypeViews info = do
  foralls <- zipConstructorForalls displayForalls identityForalls (ctorForallBinderIdentities info)
  fields <- zipTypeViews "constructor field" displayArgs identityArgs
  resultTy <- zipTypeView "constructor result" displayResult identityResult
  Right (foralls, fields, resultTy)
  where
    displayForalls = ctorForalls info
    displayArgs = ctorArgs info
    displayResult = ctorResult info
    (identityForalls, identityBody) = splitForalls (ctorTypeIdentity info)
    (identityArgs, identityResult) = splitArrows identityBody

zipConstructorForalls ::
  [(String, Maybe SrcType)] ->
  [(String, Maybe SrcType)] ->
  [Maybe TypeBinderIdentity] ->
  Either BackendConversionError [(String, String, Maybe TypeBinderIdentity, Maybe TypeView)]
zipConstructorForalls displayForalls identityForalls identities =
  go displayForalls identityForalls (identities ++ repeat Nothing)
  where
    go [] [] _ =
      Right []
    go ((name, displayBound) : displayRest) ((identityName, identityBound) : identityRest) (identity : identityRest') = do
      bound <- zipMaybeTypeView "constructor forall bound" displayBound identityBound
      rest <- go displayRest identityRest identityRest'
      Right ((name, identityName, identity, bound) : rest)
    go _ _ _ =
      Left (BackendUnsupportedCaseShape "constructor display and identity forall shapes differ")

zipTypeViews :: String -> [SrcType] -> [SrcType] -> Either BackendConversionError [TypeView]
zipTypeViews _ [] [] =
  Right []
zipTypeViews role (displayTy : displayRest) (identityTy : identityRest) = do
  view <- zipTypeView role displayTy identityTy
  rest <- zipTypeViews role displayRest identityRest
  Right (view : rest)
zipTypeViews role _ _ =
  Left (BackendUnsupportedCaseShape (role ++ " display and identity shapes differ"))

zipMaybeTypeView :: String -> Maybe SrcType -> Maybe SrcType -> Either BackendConversionError (Maybe TypeView)
zipMaybeTypeView _ Nothing Nothing =
  Right Nothing
zipMaybeTypeView role (Just displayTy) (Just identityTy) =
  Just <$> zipTypeView role displayTy identityTy
zipMaybeTypeView role _ _ =
  Left (BackendUnsupportedCaseShape (role ++ " display and identity presence differs"))

zipTypeView :: String -> SrcType -> SrcType -> Either BackendConversionError TypeView
zipTypeView _ displayTy identityTy =
  Right (mkTypeView displayTy identityTy)

constructorForallTypeVars :: [(String, String, Maybe TypeBinderIdentity, Maybe TypeView)] -> Map String BackendType
constructorForallTypeVars =
  Map.fromList . map (\(name, identityName, identity, _) -> (identityName, BTVarWithIdentity identity name))

convertConstructorForallView :: ElaborateScope -> Map String BackendType -> (String, String, Maybe TypeBinderIdentity, Maybe TypeView) -> Either BackendConversionError BackendTypeBinder
convertConstructorForallView scope typeVars (name, _, identity, mbBound) =
  BackendTypeBinderWithIdentity identity name <$> traverse (convertConstructorTypeView scope typeVars) mbBound

convertConstructorTypeView :: ElaborateScope -> Map String BackendType -> TypeView -> Either BackendConversionError BackendType
convertConstructorTypeView scope typeVars view =
  applyConstructorTypeBinderIdentities typeVars (typeViewIdentity view) <$> convertLoweredTypeView scope view

applyConstructorTypeBinderIdentities :: Map String BackendType -> SrcType -> BackendType -> BackendType
applyConstructorTypeBinderIdentities typeVars sourceTy backendTy =
  case (sourceTy, backendTy) of
    (STVar name, _) ->
      Map.findWithDefault backendTy name typeVars
    (STVarApp name sourceArgs, BTVarAppWithIdentity _ _ backendArgs)
      | Just headTy <- Map.lookup name typeVars ->
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
        (applyConstructorTypeBinderIdentities (Map.delete sourceName typeVars) sourceBody backendTyBody)
    (STMu sourceName sourceBody, BTMuWithIdentity identity name backendTyBody) ->
      BTMuWithIdentity identity name (applyConstructorTypeBinderIdentities (Map.delete sourceName typeVars) sourceBody backendTyBody)
    (STCon _ sourceArgs, BTConWithIdentity identity name backendArgs) ->
      BTConWithIdentity identity name (zipWithNE (applyConstructorTypeBinderIdentities typeVars) sourceArgs backendArgs)
    _ ->
      backendTy

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
convertLoweredTypeView scope =
  convertSourceTypeWithHeadIdentities (typeHeadIdentitiesInScope scope) . lowerTypeView scope

typeHeadIdentitiesInScope :: ElaborateScope -> Map String SymbolIdentity
typeHeadIdentitiesInScope =
  Map.map dataInfoSymbol . elaborateScopeDataTypes

convertSourceType :: SrcType -> Either BackendConversionError BackendType
convertSourceType =
  convertSourceTypeWithHeadIdentities Map.empty

convertSourceTypeWithHeadIdentities :: Map String SymbolIdentity -> SrcType -> Either BackendConversionError BackendType
convertSourceTypeWithHeadIdentities headIdentities =
  \case
    STVar name -> Right (BTVar name)
    STArrow dom cod ->
      BTArrow
        <$> convertSourceTypeWithHeadIdentities headIdentities dom
        <*> convertSourceTypeWithHeadIdentities headIdentities cod
    STBase name ->
      Right (BTBaseWithIdentity (sourceTypeHeadIdentity name) (backendBaseTy name))
    STCon name args ->
      BTConWithIdentity (sourceTypeHeadIdentity name) (backendBaseTy name)
        <$> traverse (convertSourceTypeWithHeadIdentities headIdentities) args
    STVarApp name args ->
      BTVarApp name <$> traverse (convertSourceTypeWithHeadIdentities headIdentities) args
    STTyLam {} -> Left (BackendUnsupportedCaseShape "residual type lambda reached backend type conversion")
    STTyApp {} -> Left (BackendUnsupportedCaseShape "residual type application reached backend type conversion")
    STForall name mb body ->
      BTForall name
        <$> traverse (convertSourceTypeWithHeadIdentities headIdentities . unSrcBound) mb
        <*> convertSourceTypeWithHeadIdentities headIdentities body
    STMu name body -> BTMu name <$> convertSourceTypeWithHeadIdentities headIdentities body
    STBottom -> Right BTBottom
  where
    sourceTypeHeadIdentity name =
      Map.lookup name headIdentities <|> builtinTypeHeadIdentity name

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
backendTypeToElabType =
  backendTypeToElabTypeWithGenerator initialIdentityGenerator

backendTypeToElabTypeWithGenerator :: IdentityGenerator -> BackendType -> Maybe ElabType
backendTypeToElabTypeWithGenerator generator0 ty =
  let (refs, generator) = freshTypeBinderRefs (Set.toList (freeBackendTypeVars ty)) generator0
   in fst <$> backendTypeToElabTypeWith refs generator ty

backendTypeToElabTypeWith :: Map String TypeBinderRef -> IdentityGenerator -> BackendType -> Maybe (ElabType, IdentityGenerator)
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
          refs' = Map.insert name ref refs
       in do
            (mb', generator2) <- maybe (Just (Nothing, generator1)) (backendTypeToBoundTypeWith refs generator1) mb
            (body', generator3) <- backendTypeToElabTypeWith refs' generator2 body
            Just (TForallRef ref mb' body', generator3)
    BTMuWithIdentity identity name body ->
      let (ref, generator1) = backendTypeBinderRefForBinder identity name generator
       in do
            (body', generator2) <- backendTypeToElabTypeWith (Map.insert name ref refs) generator1 body
            Just (TMuRef ref body', generator2)
    BTBottom -> Just (TBottom, generator)

backendTypeBinderRefWithIdentity :: Map String TypeBinderRef -> Maybe TypeBinderIdentity -> String -> Maybe TypeBinderRef
backendTypeBinderRefWithIdentity _ (Just identity) name =
  Just (typeBinderRefFromIdentity identity name)
backendTypeBinderRefWithIdentity refs Nothing name =
  backendTypeBinderRef refs name

backendTypeBinderRefForBinder :: Maybe TypeBinderIdentity -> String -> IdentityGenerator -> (TypeBinderRef, IdentityGenerator)
backendTypeBinderRefForBinder (Just identity) name generator =
  (typeBinderRefFromIdentity identity name, generator)
backendTypeBinderRefForBinder Nothing name generator =
  freshTypeBinderRef name generator

backendTypeBinderRef :: Map String TypeBinderRef -> String -> Maybe TypeBinderRef
backendTypeBinderRef env name =
  Map.lookup name env

backendTypesToElabTypesWith :: Map String TypeBinderRef -> IdentityGenerator -> NonEmpty BackendType -> Maybe (NonEmpty ElabType, IdentityGenerator)
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

backendTypeToBoundTypeWith :: Map String TypeBinderRef -> IdentityGenerator -> BackendType -> Maybe (Maybe BoundType, IdentityGenerator)
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
        BTConWithIdentity identity con args ->
          BTConWithIdentity identity con (fmap go args)
        BTVarAppWithIdentity identity name args ->
          BTVarAppWithIdentity identity name (fmap go args)
        BTForallWithIdentity identity name mb body ->
          BTForallWithIdentity identity name (fmap go mb) (go body)
        BTMuWithIdentity identity name body ->
          BTMuWithIdentity identity name (go body)
        ty ->
          ty

    canonicalizeDataResult ty =
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

candidateDataResultTypes :: ConvertContext -> BackendType -> [BackendType]
candidateDataResultTypes context ty =
  nub
    [ substituteBackendTypesByKey completed (backendConstructorResult constructor)
    | dataMeta <- ccData context,
      constructor <- backendDataConstructors (dmBackend dataMeta),
      let parameters = constructorTypeParameterBoundsFor (dmBackend dataMeta) constructor,
      Just substitution <- [matchBackendTypeParameters Map.empty (backendDataParameters (dmBackend dataMeta)) parameters Map.empty (backendConstructorResult constructor) ty],
      let completed = completeBackendParameterSubstitution parameters substitution
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
  entryName <- freshClosureEntryName context (partialApplicationHint headTerm)
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
  entryName <- freshClosureEntryName context (partialApplicationHint headTerm)
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
      | any (resolvedVarSameIdentity resolved) (closureScopeBoundResolvedTerms scope),
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

    freeResolvedVars =
      freeResolvedTermVariables term

    resolvedLocalCaptures =
      [ (resolved, resolvedVarType resolved)
      | resolved <- closureScopeResolvedTerms scope,
        any (resolvedVarSameIdentity resolved) freeResolvedVars,
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
              BTForallWithIdentity _ expectedName _ bodyTy -> do
                mbBackendBound <- liftEitherConvert (traverse (fmap (normalizeBackendTypeForContext context) . convertElabType . tyToElab) mbBound)
                let boundTy = maybe TBottom tyToElab mbBound
                    name = expectedName
                    bodyExpected = Just bodyTy
                bodyExpr <- convertTermExpectedMode mode context (extendTypeEnv ref boundTy env) scope bodyExpected body
                pure
                  BackendTyAbsWithIdentity
                    { backendExprType = resultTy,
                      backendTyParamIdentity = Just (typeBinderRefIdentity ref),
                      backendTyParamName = name,
                      backendTyParamBound = mbBackendBound,
                      backendTyAbsBody = bodyExpr
                    }
              _ ->
                convertTermExpectedMode mode context env scope (Just resultTy) body
          ETyInst inner inst ->
            convertTypeInstantiation context env scope resultTy inner inst
          ERoll _ body -> do
            let bodyExpected = unfoldBackendRecursiveType resultTy
            bodyExpr <- convertTermExpectedMode mode context env scope bodyExpected body
            pure
              BackendRoll
                { backendExprType = resultTy,
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
            _ | not (resolvedVarIsLocal resolved), BTForall {} <- backendTy ->
                  pure varExpr
              | alphaEqBackendType backendTy fallbackTy ->
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
              bodyMode = directLambdaBodyMode bodyExpected body
              paramResolved = mapResolvedVarType (const paramEnvTy) resolved
              bodyScope =
                extendClosureScopeResolvedTerm
                  paramResolved
                  paramEnvTy
                  (not (isEvidenceCapture context paramResolved) && isClosureConvertibleFunctionType paramBackendTy)
                  scope
          bodyExpr <- convertTermExpectedMode bodyMode context (extendResolvedTermEnv paramResolved paramEnvTy env) bodyScope bodyExpected body
          pure
            BackendLamWithIdentity
              { backendExprType = resultTy,
                backendParamIdentity = Just (resolvedVarDetails resolved),
                backendParamName = name,
                backendParamType = paramBackendTy,
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
      pure
        BackendLetWithIdentity
          { backendExprType = resultTy,
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
  rawArgTy <- liftEitherConvert (inferBackendType context env arg)
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
callableBindingKindInClosureScope context scope mbIdentity name =
  case mbIdentity >>= callableBindingKindByIdentity context scope name of
    Just kind -> kind
    Nothing
      | Just _ <- mbIdentity ->
          BackendCallableBindingUnknown
      | otherwise ->
          callableBindingKindByName context scope name

callableBindingKindByIdentity :: ConvertContext -> ClosureScope -> String -> IdDetails -> Maybe BackendCallableBindingKind
callableBindingKindByIdentity context scope _name details
  | any (idDetailsMatchesResolved details) (closureScopeLocalResolvedTerms scope) =
      Just BackendCallableBindingClosure
  | any (idDetailsMatchesResolved details) (closureScopeBoundResolvedTerms scope) =
      Just BackendCallableBindingDirect
  | Just symbol <- idDetailsTermSymbolIdentity details,
    Set.member symbol (ccClosureGlobalsByIdentity context) =
      Just BackendCallableBindingClosure
  | Just symbol <- idDetailsTermSymbolIdentity details,
    Map.member symbol (ccTermRuntimeNamesByIdentity context) =
      Just BackendCallableBindingDirect
  | otherwise =
      Nothing

callableBindingKindByName :: ConvertContext -> ClosureScope -> String -> BackendCallableBindingKind
callableBindingKindByName context scope name
  | Set.member name (closureScopeLocalNames scope) =
      BackendCallableBindingClosure
  | Set.member name (closureScopeBoundTermNames scope) =
      BackendCallableBindingDirect
  | Set.member name (closureGlobalRuntimeNames context) =
      BackendCallableBindingClosure
  | otherwise =
      BackendCallableBindingDirect

idDetailsMatchesResolved :: IdDetails -> ResolvedVar -> Bool
idDetailsMatchesResolved details resolved =
  idDetailsSameIdentity details (resolvedVarDetails resolved)

idDetailsTermSymbolIdentity :: IdDetails -> Maybe SymbolIdentity
idDetailsTermSymbolIdentity =
  \case
    TopLevelId symbol -> Just symbol
    ConstructorId ref -> Just (constructorRefSymbol ref)
    MethodId symbol -> Just symbol
    PrimitiveId ref -> Just (primitiveRefSymbol ref)
    _ -> Nothing

closureGlobalRuntimeNames :: ConvertContext -> Set.Set String
closureGlobalRuntimeNames context =
  Set.fromList
    [ runtimeName
    | symbol <- Set.toList (ccClosureGlobalsByIdentity context),
      Just runtimeName <- [Map.lookup symbol (ccTermRuntimeNamesByIdentity context)]
    ]

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
          | any (resolvedVarSameIdentity resolved) (closureScopeLocalResolvedTerms scope) ->
              Just True
          | any (resolvedVarSameIdentity resolved) (closureScopeBoundResolvedTerms scope) ->
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
  entryName <- freshClosureEntryName context (closureHint mode rawParams)
  let captureScope =
        foldr
          ( \(resolved, ty) acc ->
              extendClosureScopeResolvedTerm
                resolved
                ty
                ( any (resolvedVarSameIdentity resolved) (closureScopeLocalResolvedTerms scope)
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

freshClosureEntryName :: ConvertContext -> String -> ConvertM String
freshClosureEntryName context hint = do
  state0 <- get
  let generatedNames = csGeneratedClosureNames state0
      (name, nextIndex) = pickName generatedNames (csNextClosureIndex state0)
  modify
    ( \state1 ->
        state1
          { csNextClosureIndex = nextIndex,
            csGeneratedClosureNames = Set.insert name (csGeneratedClosureNames state1)
          }
    )
  pure name
  where
    pickName generatedNames index0 =
      let candidate =
            "__mlfp_closure$"
              ++ sanitizeClosureName (ccCurrentBindingName context)
              ++ "$"
              ++ sanitizeClosureName hint
              ++ "$"
              ++ show index0
       in if Set.member candidate (globalTermRuntimeNames context) || Set.member candidate generatedNames
            then pickName generatedNames (index0 + 1)
            else (candidate, index0 + 1)

freshBackendLocalDetails :: String -> ConvertM IdDetails
freshBackendLocalDetails name = do
  state0 <- get
  let (localRef, generator') = freshLocalRef name (csIdentityGenerator state0)
  modify (\state1 -> state1 {csIdentityGenerator = generator'})
  pure (LocalId localRef)

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
      let recoveredArgs = recoverEvidenceTypeApplicationArgs context binders finalBodyTy explicitArgs
          inferredArgs =
            if any backendTypeContainsGraphPlaceholder explicitArgs
              then inferExpectedTypeApplicationsFromBody context resultTy binders finalBodyTy
              else Nothing
          candidates =
            nub $
              [recoveredArgs]
                ++ maybe [] (: []) inferredArgs
                ++ [explicitArgs]
          recoveredChanged =
            not (and (zipWith alphaEqBackendType recoveredArgs explicitArgs))
          chosenArgs =
            if recoveredChanged
              then recoveredArgs
              else
                case find (typeApplicationsMatchExpected context resultTy binders finalBodyTy) candidates of
                  Just args -> args
                  Nothing -> explicitArgs
       in Just chosenArgs
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
    isGraphIdentity =
      \case
        Just GraphTypeBinderIdentity {} -> True
        _ -> False

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
      substitution <-
        Structural.matchBackendTypeParametersWithTypeBounds
          Map.empty
          []
          parameterBounds
          Map.empty
          evidenceParamTy
          candidate
      let completed = completeBackendParameterSubstitution parameterBounds substitution
      recoveredArgs <- traverse (lookupBackendTypeAbsBinderArg completed) binders
      Just (map (recoverStructuralBackendType context) recoveredArgs)

    firstJust =
      foldr (<|>) Nothing

inferExpectedTypeApplicationsFromBody :: ConvertContext -> BackendType -> [BackendTypeAbsBinder] -> BackendType -> Maybe [BackendType]
inferExpectedTypeApplicationsFromBody context resultTy binders finalBodyTy =
  case Structural.matchBackendTypeParametersWithTypeBounds Map.empty [] parameterBounds Map.empty finalBodyTy resultTy of
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
  where
    parameterBounds =
      backendTypeAbsBinderBounds binders

typeApplicationsMatchExpected :: ConvertContext -> BackendType -> [BackendTypeAbsBinder] -> BackendType -> [BackendType] -> Bool
typeApplicationsMatchExpected context resultTy binders finalBodyTy args =
  length binders == length args
    && backendTypesCompatible context appliedTy resultTy
  where
    appliedTy =
      normalizeBackendTypeForContext context $
        substituteBackendTypesByKey
          (backendTypeAbsBinderSubstitution binders args)
          finalBodyTy

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
backendTypeAbsBinderKeys binder@(BackendTypeAbsBinder _ name _) =
  let identityKey = backendTypeAbsBinderKey binder
      nameKey = BackendTypeSubstitutionByName name
   in if identityKey == nameKey
        then [identityKey]
        else [identityKey, nameKey]

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
          dataParameters = constructorDataParameters constructorMeta
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
                    dataParameters
                    parameters
                    initialSubstitution
                    constructorResultTy
                    effectiveResultTy of
                    Just substitution -> Right substitution
                    Nothing -> Left (constructorResultMismatch constructor)
                foldM
                  (matchConstructorApplicationArgument context env typeBounds dataParameters parameters)
                  resultSubstitution
                  (zip rawFields args)
              | initialSubstitution <- initialSubstitutions
            ]
      unless (constructorParameterSubstitutionsAgree context ownerContext typeBounds (constructorDataParameterKeys constructorMeta) substitution) $
        liftEitherConvert (Left (constructorResultMismatch constructor))
      let completedSubstitution = completeBackendParameterSubstitution parameters substitution
          fields = map (substituteBackendTypesByKey completedSubstitution) rawFields
          substitutedResultTy0 = substituteBackendTypesByKey completedSubstitution constructorResultTy
          substitutedResultTy =
            case constructorNominalResultType dataParameters completedSubstitution constructorResultTy of
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
      argExprs <- zipWithM (convertConstructorFieldArgument context env scope) fields args
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

    constructorResultSubstitution globalContext ownerContext typeBounds dataParameters parameters explicitSubstitution constructorResultTy effectiveResultTy =
      let direct = matchConstructorResult constructorResultTy effectiveResultTy normalizedExplicitSubstitution
          inferred = do
            inferredSubstitution <-
              matchConstructorResult constructorResultTy effectiveResultTy Map.empty
            if explicitSubstitutionAgreesWithInferred globalContext ownerContext typeBounds explicitSubstitution inferredSubstitution
              then Just (Map.union normalizedExplicitSubstitution inferredSubstitution)
              else Nothing
       in direct <|> inferred
      where
        normalizeResultType =
          normalizeConstructorBoundaryType ownerContext typeBounds
            . normalizeConstructorBoundaryType globalContext typeBounds

        normalizedExplicitSubstitution =
          Map.map normalizeResultType explicitSubstitution

        matchConstructorResult expected actual substitution =
          matchBackendTypeParameters typeBounds dataParameters parameters substitution expected actual
            <|> matchBackendTypeParameters
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

    constructorParameterSubstitutionsAgree globalContext ownerContext typeBounds parameterKeys substitution =
      all parameterSubstitutionAgrees parameterKeys
      where
        parameterSubstitutionAgrees (name, key)
          | key == nameKey = True
          | otherwise =
              case (Map.lookup key substitution, Map.lookup nameKey substitution) of
                (Just keyedTy, Just namedTy) ->
                  constructorBoundaryTypesMatch globalContext ownerContext typeBounds keyedTy namedTy
                _ ->
                  True
          where
            nameKey = BackendTypeSubstitutionByName name

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
        BTMu name _
          | Just dataMeta <- structuralRecursiveDataMeta ownerContext name ->
              case Structural.structuralMuAsDataType (backendDataParameters (dmBackend dataMeta)) name of
                Just nominalTy -> nominalTy
                Nothing -> ty
        _ ->
          ty

    resultTypePlaceholderMatches typeBounds actual expected =
      case (actual, expected) of
        (_, BTVarWithIdentity identity name)
          | not (typeBoundsContain identity name typeBounds) -> True
        (BTArrow actualDom actualCod, BTArrow expectedDom expectedCod) ->
          resultTypePlaceholderMatches typeBounds actualDom expectedDom
            && resultTypePlaceholderMatches typeBounds actualCod expectedCod
        (BTCon actualCon actualArgs, BTCon expectedCon expectedArgs)
          | actualCon == expectedCon,
            length actualArgs == length expectedArgs ->
              and (zipWith (resultTypePlaceholderMatches typeBounds) (NE.toList actualArgs) (NE.toList expectedArgs))
        (BTVarApp actualName actualArgs, BTVarApp expectedName expectedArgs)
          | actualName == expectedName,
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

    typeBoundsContain identity name bounds =
      case identity of
        Just {} ->
          Map.member (backendTypeSubstitutionKeyFor identity name) bounds
        Nothing ->
          Map.member (BackendTypeSubstitutionByName name) bounds

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
  let resolvedParams = freshEtaParams (identityGeneratorAfterTerm arg) params
      applied = foldl EApp arg (map (EVarNode . fst) resolvedParams)
      etaTerm = foldr (\(resolved, _) body -> ELam resolved body) applied resolvedParams
  convertTermExpectedMode (ClosureLambda Nothing) context env scope (Just fieldTy) etaTerm
  where
    freshEtaParams _ [] = []
    freshEtaParams generator ((name, ty) : rest) =
      let (localRef, generator') = freshLocalRef name generator
          resolved = localResolvedVarFromRef localRef ty
       in (resolved, ty) : freshEtaParams generator' rest

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
  case collectStructuralLams fieldArity headTerm of
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
  case convertElabType resultTy >>= backendTypeStructuralDataName of
    Right resultDataName -> constructorDataNameMatches context constructorMeta resultDataName
    Left _ -> False

constructorDataNameMatches :: ConvertContext -> ConstructorMeta -> String -> Bool
constructorDataNameMatches context constructorMeta resultDataName =
  resultDataName == backendDataName (dmBackend dataMeta)
    || localUnqualifiedDataNameMatches context dataMeta resultDataName
  where
    dataMeta = cmData constructorMeta

localUnqualifiedDataNameMatches :: ConvertContext -> DataMeta -> String -> Bool
localUnqualifiedDataNameMatches context dataMeta resultDataName =
  case dataMetaByCurrentScopeStructuralName context resultDataName of
    Just localDataMeta ->
      dataInfoSymbol (dmInfo localDataMeta) == dataInfoSymbol (dmInfo dataMeta)
    Nothing -> False

backendTypeStructuralDataName :: BackendType -> Either BackendConversionError String
backendTypeStructuralDataName =
  \case
    BTBase (BaseTy name) -> Right name
    BTCon (BaseTy name) _ -> Right name
    BTMu name _ ->
      case Structural.structuralRecursiveDataName name of
        Just resultDataName -> Right resultDataName
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

constructorDataParameters :: ConstructorMeta -> [String]
constructorDataParameters =
  backendDataParameters . dmBackend . cmData

freeBackendTypeVars :: BackendType -> Set.Set String
freeBackendTypeVars =
  go Set.empty
  where
    go bound =
      \case
        BTVar name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        BTArrow dom cod ->
          go bound dom `Set.union` go bound cod
        BTBase {} ->
          Set.empty
        BTCon _ args ->
          Set.unions (map (go bound) (NE.toList args))
        BTVarApp name args ->
          let headVars =
                if Set.member name bound
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` Set.unions (map (go bound) (NE.toList args))
        BTForall name mb body ->
          maybe Set.empty (go bound) mb `Set.union` go (Set.insert name bound) body
        BTMu name body ->
          go (Set.insert name bound) body
        BTBottom ->
          Set.empty

constructorTypeParameterBoundsFor :: BackendData -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsFor dataDecl constructor =
  Map.fromList $
    [(key, Nothing) | key <- backendDataParameterKeys dataDecl]
      ++ [ (backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderBound binder)
           | binder <- backendConstructorForalls constructor
         ]

existingParameterKeyFor :: BackendParameterBounds -> Maybe TypeBinderIdentity -> String -> Maybe BackendTypeSubstitutionKey
existingParameterKeyFor parameterBounds identity name =
  case identity of
    Just {}
      | Map.member identityKey parameterBounds -> Just identityKey
      | otherwise -> Nothing
    Nothing
      | Map.member nameKey parameterBounds -> Just nameKey
      | otherwise -> Nothing
  where
    identityKey = backendTypeSubstitutionKeyFor identity name
    nameKey = BackendTypeSubstitutionByName name

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
    | (name, key) <- constructorDataParameterKeys constructorMeta,
      constructorResultIsStructural || Set.member name resultVariables
  ]
    ++ [ backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder)
         | binder <- backendConstructorForalls (cmBackend constructorMeta)
       ]
  where
    constructorResultIsStructural =
      case backendConstructorResult (cmBackend constructorMeta) of
        BTMu {} -> True
        _ -> False

    resultVariables =
      freeBackendTypeVars (backendConstructorResult (cmBackend constructorMeta))

constructorDataParameterKeys :: ConstructorMeta -> [(String, BackendTypeSubstitutionKey)]
constructorDataParameterKeys constructorMeta =
  zip (constructorDataParameters constructorMeta) (backendDataParameterKeys (dmBackend (cmData constructorMeta)))

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
    BTBaseWithIdentity mbIdentity (BaseTy name) ->
      case mbIdentity of
        Just identity -> dataMetaBySymbol context identity
        Nothing -> dataMetaByBackendName context name
    BTConWithIdentity mbIdentity (BaseTy name) _ ->
      case mbIdentity of
        Just identity -> dataMetaBySymbol context identity
        Nothing -> dataMetaByBackendName context name
    BTMu name _ -> structuralRecursiveDataMeta context name
    _ -> Nothing

dataMetaByBackendName :: ConvertContext -> String -> Maybe DataMeta
dataMetaByBackendName context name =
  find (sourceTypeHeadMatchesData name) (ccData context)

dataMetaByStructuralName :: ConvertContext -> String -> Maybe DataMeta
dataMetaByStructuralName context name =
  dataMetaByBackendName context name
    <|> dataMetaByCurrentScopeStructuralName context name

dataMetaByCurrentScopeStructuralName :: ConvertContext -> String -> Maybe DataMeta
dataMetaByCurrentScopeStructuralName context name = do
  moduleIdentity <- ccCurrentModuleIdentity context
  scope <- Map.lookup moduleIdentity (ccModuleScopes context)
  info <- Map.lookup name (elaborateScopeDataTypes scope)
  dataMetaBySymbol context (dataInfoSymbol info)

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
  case structuralRecursiveDataMeta context name of
    Just dataMeta ->
      let canonicalName = "$" ++ backendDataName (dmBackend dataMeta) ++ "_self"
       in if name == canonicalName
            then (name, body)
            else (canonicalName, substituteBackendType name (BTVarWithIdentity identity canonicalName) body)
    Nothing -> (name, body)

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
                else case structuralRecursiveDataMeta context name' <|> structuralRecursiveDataMetaByBody (go seen') body' of
                  Just dataMeta
                    | Just args <- structuralBackendDataArguments (go seen') dataMeta body' ->
                        backendDataType (backendDataIdentity (dmBackend dataMeta)) (backendDataName (dmBackend dataMeta)) args
                  _ -> BTMuWithIdentity identity name' (go seen' body')
        BTBottom -> BTBottom

    structuralRecursiveDataMetaByBody recoverFieldTy body =
      case
        [ dataMeta
        | dataMeta <- ccData context,
          Just _ <- [structuralBackendDataArguments recoverFieldTy dataMeta body]
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

structuralBackendDataArguments :: (BackendType -> BackendType) -> DataMeta -> BackendType -> Maybe [BackendType]
structuralBackendDataArguments recoverFieldTy dataMeta body = do
  handlerFields <- Structural.structuralBackendHandlerFields body
  let dataDecl = dmBackend dataMeta
      dataParameters = backendDataParameters dataDecl
      dataParameterKeys = backendDataParameterKeys dataDecl
      constructors = backendDataConstructors dataDecl
      parameterBounds =
        Map.fromList [(key, Nothing) | key <- dataParameterKeys]
  if length handlerFields == length constructors
    then do
      substitution <-
        foldM
          (matchConstructorFields dataDecl dataParameters parameterBounds)
          Map.empty
          (zip constructors handlerFields)
      let completedSubstitution = completeBackendParameterSubstitution parameterBounds substitution
      Just [Map.findWithDefault (BTVar name) key completedSubstitution | (name, key) <- zip dataParameters dataParameterKeys]
    else Nothing
  where
    matchConstructorFields dataDecl dataParameters parameterBounds substitution (constructor, fields) =
      if length fields == length (backendConstructorFields constructor)
        then
          foldM
            ( \substitutionAcc (expectedTy, actualTy) ->
                matchBackendTypeParameters
                  Map.empty
                  dataParameters
                  (constructorParameterBounds parameterBounds constructor)
                  substitutionAcc
                  expectedTy
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
        BTVar fieldName
          | Structural.structuralRecursiveDataName fieldName == Just (backendDataName dataDecl) ->
              backendDataType (backendDataIdentity dataDecl) (backendDataName dataDecl) dataSelfArgs
        _ ->
          ty
      where
        dataSelfArgs =
          zipWith
            BTVarWithIdentity
            (backendDataParameterIdentities dataDecl ++ repeat Nothing)
            (backendDataParameters dataDecl)

constructorApplicationResultType :: ConvertContext -> Env -> XmlfTerm -> Either BackendConversionError (Maybe (BackendType, Maybe DataMeta))
constructorApplicationResultType context env term =
  constructorApplicationTerm context term >>= \case
    Just (ConstructorApplication constructorMeta headTypeArgs args) -> do
      let constructor = cmBackend constructorMeta
          ownerContext = contextForDataMeta context (cmData constructorMeta)
          fields = backendConstructorFields constructor
          dataParameters = constructorDataParameters constructorMeta
          parameters = constructorTypeParameters constructorMeta
          constructorResultTy = canonicalizeStructuralMuNames ownerContext (backendConstructorResult constructor)
      typeBounds <- backendTypeBoundsFromEnv env
      initialSubstitutions <- constructorTypeApplicationSubstitutions env constructorMeta headTypeArgs
      substitution <-
        firstRightOr
          (BackendUnsupportedCaseShape ("constructor arguments do not match type applications for `" ++ backendConstructorName constructor ++ "`"))
          [ foldM
              (matchConstructorApplicationArgument context env typeBounds dataParameters parameters)
              initialSubstitution
              (zip fields args)
            | initialSubstitution <- initialSubstitutions
          ]
      let completedSubstitution =
            completeDataParameterSubstitution (dmBackend (cmData constructorMeta)) $
              completeBackendParameterSubstitution parameters substitution
          resultTy0 = substituteBackendTypesByKey completedSubstitution constructorResultTy
          resultTy =
            case constructorNominalResultType dataParameters completedSubstitution constructorResultTy of
              Just nominalTy -> nominalTy
              Nothing -> recoverStructuralBackendType ownerContext resultTy0
      Right (Just (resultTy, Just (cmData constructorMeta)))
    Nothing -> Right Nothing

constructorNominalResultType :: [String] -> BackendParameterSubstitution -> BackendType -> Maybe BackendType
constructorNominalResultType dataParameters substitution =
  \case
    BTMu name _ ->
      substituteBackendTypesByKey substitution <$> Structural.structuralMuAsDataType dataParameters name
    _ -> Nothing

constructorExpectedResultType :: ConvertContext -> ConvertContext -> ConstructorMeta -> BackendType -> BackendType
constructorExpectedResultType context ownerContext constructorMeta resultTy =
  case canonicalResultTy of
    BTMu name _
      | Structural.structuralRecursiveDataName name == Just ownerName ->
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
  [String] ->
  BackendParameterBounds ->
  BackendParameterSubstitution ->
  (BackendType, XmlfTerm) ->
  Either BackendConversionError BackendParameterSubstitution
matchConstructorApplicationArgument context env typeBounds dataParameters parameters substitution (expectedTy, arg) =
  -- This is only a best-effort way to recover constructor type parameters.
  -- Expected-type conversion of the argument remains authoritative because it
  -- can canonicalize nested constructor applications before validation.
  case constructorArgumentMatchType context env arg of
    Right actualTy0 ->
      let actualTy = recoverStructuralBackendType context actualTy0
       in case matchBackendTypeParameters typeBounds dataParameters parameters substitution expectedTy actualTy of
            Just substitution' -> Right substitution'
            Nothing -> Right substitution
    Left _ -> Right substitution

constructorArgumentMatchType :: ConvertContext -> Env -> XmlfTerm -> Either BackendConversionError BackendType
constructorArgumentMatchType context env arg =
  case arg of
    EVarNode resolved ->
      case lookupResolvedTermEnvEntry (resolvedTermEnv env) resolved of
        Just (_, envTy) -> normalizeBackendTypeForContext context <$> convertElabType envTy
        Nothing -> normalizeBackendTypeForContext context <$> convertElabType (resolvedVarType resolved)
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
    BTMu binderName _ ->
      dataInfoIdentityName (dmInfo dataMeta) `elem` recursiveBinderNameHints binderName
        || backendDataName (dmBackend dataMeta) `elem` recursiveBinderNameHints binderName
    _ -> False

recursiveBinderNameHints :: String -> [String]
recursiveBinderNameHints binderName =
  nub [raw, withoutDollar, beforeSelf withoutDollar, suffixAfterDot (beforeSelf withoutDollar)]
  where
    raw = binderName
    withoutDollar =
      case raw of
        '$' : rest -> rest
        _ -> raw

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
  case matchBackendTypeParameters Map.empty (backendDataParameters dataDecl) parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
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
          matchBackendTypeParameters
            typeBounds
            (backendDataParameters (dmBackend dataMeta))
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
  fields <- liftEitherConvert (caseAlternativeFieldTypes env dataMeta scrutineeTy constructor)
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

caseAlternativeFieldTypes :: Env -> DataMeta -> BackendType -> BackendConstructor -> Either BackendConversionError [BackendType]
caseAlternativeFieldTypes env dataMeta scrutineeTy constructor = do
  typeBounds <- backendTypeBoundsFromEnv env
  let parameters = constructorTypeParameterBoundsFor (dmBackend dataMeta) constructor
  case matchBackendTypeParameters typeBounds (backendDataParameters (dmBackend dataMeta)) parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
    Just substitution ->
      let completed =
            completeDataParameterSubstitution (dmBackend dataMeta) $
              completeBackendParameterSubstitution parameters substitution
       in Right (map (substituteBackendTypesByKey completed) (backendConstructorFields constructor))
    Nothing ->
      Left
        ( BackendUnsupportedCaseShape
            ("constructor result type does not match case scrutinee for `" ++ backendConstructorName constructor ++ "`")
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
    dataParameters = backendDataParameters dataDecl

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
                  matchBackendTypeParameters
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
  go []
  where
    go seen term =
      let (headTerm, args) = collectApps (stripAdministrativeTermWrappers term)
          (resolvedHead, aliasArgs) = resolveHead seen headTerm
       in (resolvedHead, aliasArgs ++ args)

    resolveHead seen term =
      case stripAdministrativeTermWrappers term of
        ELet resolved _ rhs body
          | not (any (resolvedVarSameIdentity resolved) seen) ->
              let seen' = resolved : seen
                  (bodyHead, bodyArgs) = go seen' body
               in case stripClosureHeadTypeInsts bodyHead of
                    EVarNode bodyResolved
                      | termVarKeyMatchesReference (TermVarResolved resolved) bodyResolved ->
                          let (rhsHead, rhsArgs) = go seen' rhs
                           in (rhsHead, rhsArgs ++ bodyArgs)
                    _ ->
                      (term, [])
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
    Left err -> Left (BackendTypeCheckFailed term err)

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
          | resolvedVarIsLocal resolved
              && backendElabTypesCompatible context envTy (resolvedVarType resolved) ->
              mapResolvedVarType (const envTy) resolved
        _ -> resolved

    extendLocalTerm resolved ty env resolvedEnv =
      (insertResolvedTermBinding resolved ty env, insertResolvedTermEnv resolved ty resolvedEnv)

    extendLocalTypeRef ref mbBound env =
      insertTypeBindingRef ref (maybe TBottom tyToElab mbBound) env

backendTypesCompatible :: ConvertContext -> BackendType -> BackendType -> Bool
backendTypesCompatible context leftTy rightTy =
  or
    [ alphaEqBackendType leftCandidate rightCandidate
        || Structural.backendStructuralDataBoundaryMatches Map.empty (Just dataDecls) leftCandidate rightCandidate
        || nominalStructuralHeadsMatch leftCandidate rightCandidate
    | leftCandidate <- backendTypeCompatibilityVariants context leftTy,
      rightCandidate <- backendTypeCompatibilityVariants context rightTy
    ]
  where
    dataDecls =
      Map.fromList [(backendDataName (dmBackend dataMeta), dmBackend dataMeta) | dataMeta <- ccData context]

backendTypeCompatibilityVariants :: ConvertContext -> BackendType -> [BackendType]
backendTypeCompatibilityVariants context ty =
  [ ty,
    canonicalizeStructuralMuNames context ty,
    recoverStructuralBackendType context ty,
    normalizeBackendTypeForContext context ty,
    eraseBackendTypeBinderIdentities ty
  ]

nominalStructuralHeadsMatch :: BackendType -> BackendType -> Bool
nominalStructuralHeadsMatch leftTy rightTy =
  nominalStructuralHeadMatches leftTy rightTy || nominalStructuralHeadMatches rightTy leftTy

nominalStructuralHeadMatches :: BackendType -> BackendType -> Bool
nominalStructuralHeadMatches nominal structural =
  case (nominal, structural) of
    (BTBase (BaseTy nominalName), BTMu structuralName _) ->
      Structural.structuralRecursiveDataName structuralName == Just nominalName
    (BTCon (BaseTy nominalName) _, BTMu structuralName _) ->
      Structural.structuralRecursiveDataName structuralName == Just nominalName
    _ ->
      False

backendElabTypesCompatible :: ConvertContext -> ElabType -> ElabType -> Bool
backendElabTypesCompatible context left right =
  case (convertElabType left, convertElabType right) of
    (Right leftTy, Right rightTy) ->
      backendTypesCompatible context leftTy rightTy
    _ ->
      False

eraseBackendTypeBinderIdentities :: BackendType -> BackendType
eraseBackendTypeBinderIdentities =
  \case
    BTVarWithIdentity _ name ->
      BTVarWithIdentity Nothing name
    BTArrow dom cod ->
      BTArrow (eraseBackendTypeBinderIdentities dom) (eraseBackendTypeBinderIdentities cod)
    BTBaseWithIdentity identity base ->
      BTBaseWithIdentity identity base
    BTConWithIdentity identity base args ->
      BTConWithIdentity identity base (fmap eraseBackendTypeBinderIdentities args)
    BTVarAppWithIdentity _ name args ->
      BTVarAppWithIdentity Nothing name (fmap eraseBackendTypeBinderIdentities args)
    BTForallWithIdentity _ name mb body ->
      BTForallWithIdentity Nothing name (fmap eraseBackendTypeBinderIdentities mb) (eraseBackendTypeBinderIdentities body)
    BTMuWithIdentity _ name body ->
      BTMuWithIdentity Nothing name (eraseBackendTypeBinderIdentities body)
    BTBottom ->
      BTBottom

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
      let name = backendTypeVarName names ref
          identityKey = BackendTypeSubstitutionByIdentity (typeBinderRefIdentity ref)
          nameKey = BackendTypeSubstitutionByName name
      Right [(identityKey, bound), (nameKey, bound)]

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
  [String] ->
  BackendParameterBounds ->
  BackendParameterSubstitution ->
  BackendType ->
  BackendType ->
  Maybe BackendParameterSubstitution
matchBackendTypeParameters typeBounds dataParameterOrder parameterBounds =
  go Map.empty Map.empty
  where
    dataParameterNames =
      Set.fromList dataParameterOrder

    matchParameterKey identity name =
      case identity of
        Just {} ->
          if Map.member key parameterBounds || Set.member name dataParameterNames
            then Just key
            else Nothing
        Nothing
          | Map.member nameKey parameterBounds || Set.member name dataParameterNames -> Just nameKey
          | otherwise -> Nothing
      where
        key = backendTypeSubstitutionKeyFor identity name
        nameKey = BackendTypeSubstitutionByName name

    go leftEnv rightEnv substitution expected actual =
      case expected of
        BTVarWithIdentity identity name
          | Just key <- matchParameterKey identity name,
            Map.notMember name leftEnv ->
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
            (BTMu expectedName expectedBody, actualTy@(BTBase {})) ->
              matchStructuralMuExpected leftEnv rightEnv substitution expectedName expectedBody actualTy
            (BTMu expectedName expectedBody, actualTy@(BTCon {})) ->
              matchStructuralMuExpected leftEnv rightEnv substitution expectedName expectedBody actualTy
            (expectedTy@(BTBase {}), BTMu actualName actualBody) ->
              matchStructuralMuActual leftEnv rightEnv substitution expectedTy actualName actualBody
            (expectedTy@(BTCon {}), BTMu actualName actualBody) ->
              matchStructuralMuActual leftEnv rightEnv substitution expectedTy actualName actualBody
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
      case decomposeBackendTypeHead actual of
        Just (actualHead, actualArgs)
          | length expectedArgs == length actualArgs -> do
              substitution' <-
                case matchParameterKey identity name of
                  Just key
                    | Map.notMember name leftEnv ->
                        insertParameterSubstitution key actualHead substitution
                  _ ->
                    go leftEnv rightEnv substitution (BTVarWithIdentity identity name) actualHead
              foldM
                (\substitutionAcc (expectedArg, actualArg) -> go leftEnv rightEnv substitutionAcc expectedArg actualArg)
                substitution'
                (zip expectedArgs actualArgs)
        _ -> Nothing

    matchStructuralMuExpected leftEnv rightEnv substitution muName body actualTy =
      ( structuralMuAsDataTypeForBody muName body
          >>= \expectedTy -> go leftEnv rightEnv substitution expectedTy actualTy
      )
        <|> ( Structural.structuralMuPayloadTypes body
                *> Structural.structuralMuAsActualDataType muName actualTy
                >>= \expectedTy -> go leftEnv rightEnv substitution expectedTy actualTy
            )

    matchStructuralMuActual leftEnv rightEnv substitution expectedTy muName body =
      ( structuralMuAsDataTypeForBody muName body
          >>= \actualTy -> go leftEnv rightEnv substitution expectedTy actualTy
      )
        <|> ( Structural.structuralMuPayloadTypes body
                *> Structural.structuralMuAsActualDataType muName expectedTy
                >>= \actualTy -> go leftEnv rightEnv substitution expectedTy actualTy
            )

    structuralMuAsDataTypeForBody muName body =
      Structural.structuralMuPayloadTypes body *> Structural.structuralMuAsDataType dataParameterOrder muName

    sameTypeVar leftEnv rightEnv expectedIdentity expectedName actualIdentity actualName =
      case (Map.lookup expectedName leftEnv, Map.lookup actualName rightEnv) of
        (Just expectedActual, Just actualExpected) -> expectedActual == actualName && actualExpected == expectedName
        (Nothing, Nothing) ->
          case (expectedIdentity, actualIdentity) of
            (Just expectedTypeIdentity, Just actualTypeIdentity) -> expectedTypeIdentity == actualTypeIdentity
            (Nothing, Nothing) -> expectedName == actualName
            _ -> False
        _ -> False

    insertParameterSubstitution key actual substitution =
      case Map.lookup key substitution of
        Nothing ->
          if backendParameterBoundMatches key actual substitution
            then Just (Map.insert key actual substitution)
            else Nothing
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

    explicitParameterSubstitutionMatches previous actual =
      alphaEqBackendType previous actual
        || typeBoundDependenciesMatch previous actual

    typeBoundDependenciesMatch actual expectedBound =
      alphaEqBackendType
        (resolveTypeBoundDependencies actual)
        (resolveTypeBoundDependencies expectedBound)

    actualTypeVariableBoundMatches actual expectedBound =
      case actual of
        BTVarWithIdentity actualIdentity actualName ->
          case lookupTypeBound actualIdentity actualName typeBounds of
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

    lookupTypeBound identity name bounds =
      Map.lookup (backendTypeSubstitutionKeyFor identity name) bounds

decomposeBackendTypeHead :: BackendType -> Maybe (BackendType, [BackendType])
decomposeBackendTypeHead ty =
  case ty of
    BTVarWithIdentity identity name -> Just (BTVarWithIdentity identity name, [])
    BTBaseWithIdentity identity name -> Just (BTBaseWithIdentity identity name, [])
    BTConWithIdentity identity name args -> Just (BTBaseWithIdentity identity name, NE.toList args)
    BTVarAppWithIdentity identity name args -> Just (BTVarWithIdentity identity name, NE.toList args)
    _ -> Nothing

completeBackendParameterSubstitution :: BackendParameterBounds -> BackendParameterSubstitution -> BackendParameterSubstitution
completeBackendParameterSubstitution parameterBounds substitution0 =
  resolveDefaultedBounds defaultedNames substitution1
  where
    substitution1 =
      foldl insertBoundDefault substitution0 (Map.toList parameterBounds)

    defaultedNames =
      Set.fromList
        [ key
          | (key, Just boundTy) <- Map.toList parameterBounds,
            Map.notMember key substitution0,
            not (alphaEqBackendType boundTy BTBottom)
        ]

    insertBoundDefault substitution (key, Just boundTy)
      | Map.member key substitution = substitution
      | alphaEqBackendType boundTy BTBottom = substitution
      | otherwise = Map.insert key (substituteBackendTypesByKey substitution boundTy) substitution
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

    resolveDefaultedBound substitution key =
      case Map.lookup key substitution of
        Just ty ->
          Map.insert key (substituteBackendTypesByKey (Map.delete key substitution) ty) substitution
        Nothing ->
          substitution

completeDataParameterSubstitution :: BackendData -> BackendParameterSubstitution -> BackendParameterSubstitution
completeDataParameterSubstitution dataDecl substitution0 =
  foldr completeOne substitution0 (zip (backendDataParameterIdentities dataDecl ++ repeat Nothing) (backendDataParameters dataDecl))
  where
    completeOne (identity, name) substitution =
      case Map.lookup identityKey substitution of
        Just ty -> Map.insert identityKey ty (Map.insert nameKey ty substitution)
        Nothing -> substitution
      where
        identityKey = backendTypeSubstitutionKeyFor identity name
        nameKey = BackendTypeSubstitutionByName name
