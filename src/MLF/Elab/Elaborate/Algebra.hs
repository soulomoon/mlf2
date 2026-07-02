{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module MLF.Elab.Elaborate.Algebra
  ( Env,
    EnvBinding (..),
    ElabOut (..),
    AlgebraContext (..),
    elabAlg,
    mkEnv,
    mkEnvBinding,
    mkEnvFromBindings,
    mkEnvWithBindingDetails,
    lookupSchemeInfoForResolved,
    typeCheckEnvFrom,
    freshenSchemeInfoAgainstEnv,
    resolvedLambdaParamNode,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find, mapAccumL)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Set as Set
import MLF.Constraint.Presolution (PresolutionView)
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    NodeId,
    TyNode (..),
    getNodeId,
  )
import MLF.Constraint.Types.Phase (Phase)
import MLF.Elab.Elaborate.Annotation
  ( AnnotationContext (..),
    desugaredAnnLambdaInfo,
    elaborateAnnotationTerm,
    instSeqApps,
    reifyInst,
    sourceAnnIsPolymorphic,
    stripUnusedTopTyAbs,
  )
import MLF.Elab.Elaborate.Scope
  ( ScopeContext,
    generalizeAtNode,
    normalizeSchemeSubstPair,
    normalizeSubstForScheme,
    reifyNodeTypeDirect,
    reifyNodeTypePreferringBound,
    scopeRootForNode,
  )
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.Reduce (normalize)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromSchemeRefs)
import MLF.Elab.Run.ResultType.Util
  ( CandidateSelection (..),
    selectUniqueCandidateBy,
  )
import MLF.Elab.Run.TypeOps
  ( InlineBoundVarsContext,
    inlineBoundVarsTypeWithContext,
    simplifyAnnotationType,
  )
import MLF.Elab.TermClosure (closeTermWithSchemeSubstRefsIfNeeded)
import qualified MLF.Elab.TypeCheck as TypeCheck (Env (..), insertResolvedTermBinding, mkTypeCheckEnvWithResolvedTerms, resolvedTermEnvEntries, typeCheckWithEnv)
import MLF.Elab.Types
  ( BoundType,
    ElabScheme,
    ElabError (..),
    XmlfTerm (..),
    ElabType,
    Instantiation (..),
    ResolvedVar (..),
    SchemeInfo (..),
    Ty (..),
    freshTypeBinderRef,
    sourceTypeBinderRefForName,
    generatedIdentitiesInType,
    generatedIdentitiesInTerm,
    identityGeneratorAfterType,
    mapBoundType,
    mapResolvedVarType,
    mkElabSchemeWithRefs,
    schemeBinderRefs,
    schemeBody,
    schemeInfoFromRefSubst,
    schemeInfoBinderRefSubst,
    renameTypeBinderRef,
    TypeBinderRef,
    typeBinderRefFromIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
    typeBinderRefsSameIdentityAndName,
    localResolvedVarFromRef,
    resolvedVarReferenceName,
    resolvedVarSameIdentity,
    resolvedVarType,
    schemeFromType,
    tyToElab,
  )
import MLF.Frontend.ConstraintGen.Types (AnnExpr (..), AnnExprF (..))
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Symbol (SymbolIdentity, symbolIdentityAliasMap)
import MLF.Frontend.Syntax (NormSrcType, SrcBound (..), SrcNorm (..), SrcTy (..), StructBound, VarName)
import MLF.Reify.TypeOps
  ( alphaEqType,
    churchAwareEqType,
    firstNonContractiveRecursiveType,
    freeTypeVarRefsType,
    freeTypeVarsType,
    freshNameLike,
    matchTypeRefs,
    splitForallsRefs,
    substTypeCaptureRef,
  )
import MLF.Types.Identity
  ( EnvRef,
    IdDetails (..),
    IdentityGenerator,
    LocalRef,
    TypeBinderIdentity,
    DeferredRef,
    constructorRefSymbol,
    freshEnvRef,
    freshLocalRef,
    idDetailsGeneratedIdentities,
    idDetailsIsDiscard,
    identityGeneratorAfter,
    localRefFromNodeId,
    primitiveRefSymbol,
    symbolGeneratedIdentities,
    typeBinderGeneratedIdentities,
    typeBinderIdentityAliasMap,
  )
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data EnvBinding = EnvBinding
  { ebSchemeInfo :: SchemeInfo,
    ebSchemeType :: ElabType,
    ebIdentityDetails :: IdDetails,
    ebTransparentMediator :: Bool,
    ebAliasTarget :: Maybe VarName,
    ebExplicitRecursiveParam :: Bool
  }

data IdentityWrapperAlias
  = IdentityWrapperRoot
  | IdentityWrapperMediator

data StructuralRecursiveCandidate
  = StructuralRecursiveCandidateFromHelper ElabType
  | StructuralRecursiveCandidateFromDirectCarrier ElabType

type StructuralRecursiveCandidateSelection = CandidateSelection StructuralRecursiveCandidate

pattern NoStructuralRecursiveCandidate :: StructuralRecursiveCandidateSelection
pattern NoStructuralRecursiveCandidate = NoCandidateSelection

pattern UniqueStructuralRecursiveCandidate :: StructuralRecursiveCandidate -> StructuralRecursiveCandidateSelection
pattern UniqueStructuralRecursiveCandidate candidate = UniqueCandidateSelection candidate

pattern AmbiguousStructuralRecursiveCandidate :: StructuralRecursiveCandidateSelection
pattern AmbiguousStructuralRecursiveCandidate = AmbiguousCandidateSelection

{-# COMPLETE NoStructuralRecursiveCandidate, UniqueStructuralRecursiveCandidate, AmbiguousStructuralRecursiveCandidate #-}

data Env = Env
  { envBindings :: Map.Map VarName EnvBinding,
    envBindingsByIdentity :: Map.Map EnvBindingIdentityKey EnvBinding,
    envTypeCheck :: TypeCheck.Env
  }

data EnvBindingIdentityKey
  = EnvBindingLocalKey LocalRef
  | EnvBindingEnvKey EnvRef
  | EnvBindingTopLevelKey SymbolIdentity
  | EnvBindingConstructorKey SymbolIdentity
  | EnvBindingMethodKey SymbolIdentity
  | EnvBindingPrimitiveKey SymbolIdentity
  | EnvBindingDeferredKey DeferredRef
  deriving (Eq, Ord, Show)

data ElabOut = ElabOut
  { elabTerm :: Env -> Either ElabError XmlfTerm,
    elabStripped :: Env -> Either ElabError XmlfTerm
  }

data TypedTerm = TypedTerm
  { ttTerm :: !XmlfTerm,
    ttType :: !ElabType
  }

data AlgebraContext (p :: Phase) = AlgebraContext
  { algPresolutionView :: PresolutionView p,
    algTraceConfig :: TraceConfig,
    algCanonical :: NodeId -> NodeId,
    algResolvedLambdaParamNode :: NodeId -> Maybe NodeId,
    algAnnotationContext :: AnnotationContext p,
    algNamedSetReify :: IntSet.IntSet,
    algInlineBoundVarsContext :: InlineBoundVarsContext p,
    -- | Original source annotation types from constraint generation, keyed by
    -- canonicalized AAnn codomain NodeId.  Used in ALamF to recover annotation
    -- types that presolution strips (e.g. TForall inside a μ body).
    algAnnSourceTypes :: IntMap.IntMap NormSrcType,
    algSourceTypeHeadIdentities :: Map.Map String SymbolIdentity,
    algSourceTypeBinderIdentities :: Map.Map String TypeBinderIdentity
  }

containsMuType :: ElabType -> Bool
containsMuType ty =
  case ty of
    TMuRef {} -> True
    TArrow dom cod -> containsMuType dom || containsMuType cod
    TCon _ args -> any containsMuType args
    TVarAppRef _ args -> any containsMuType args
    TForallRef _ mb body -> maybe False containsMuBound mb || containsMuType body
    _ -> False
  where
    containsMuBound bound = case bound of
      TArrow dom cod -> containsMuType dom || containsMuType cod
      TCon _ args -> any containsMuType args
      TVarAppRef _ args -> any containsMuType args
      TForallRef _ mb body -> maybe False containsMuBound mb || containsMuType body
      TMuRef {} -> True
      _ -> False

hasContractiveRecursiveWitness :: ElabType -> Bool
hasContractiveRecursiveWitness ty =
  containsMuType ty && isNothing (firstNonContractiveRecursiveType ty)

isSingleBinderIdentityScheme :: SchemeInfo -> Bool
isSingleBinderIdentityScheme schemeInfo =
  case (schemeBinderRefs (siScheme schemeInfo), schemeBody (siScheme schemeInfo)) of
    ([(binderRef, Nothing)], TArrow (TVarRef domRef) (TVarRef codRef)) ->
      typeBinderRefsSameIdentity binderRef domRef && typeBinderRefsSameIdentity binderRef codRef
    _ -> False

containsInternalTypeVar :: ElabType -> Bool
containsInternalTypeVar ty =
  case ty of
    TVarRef ref -> isInternalTypeBinderRef ref
    TArrow dom cod -> containsInternalTypeVar dom || containsInternalTypeVar cod
    TCon _ args -> any containsInternalTypeVar args
    TVarAppRef ref args -> isInternalTypeBinderRef ref || any containsInternalTypeVar args
    TForallRef _ mb body -> maybe False containsInternalBoundVar mb || containsInternalTypeVar body
    TMuRef _ body -> containsInternalTypeVar body
    _ -> False
  where
    containsInternalBoundVar bound =
      case bound of
        TArrow dom cod -> containsInternalTypeVar dom || containsInternalTypeVar cod
        TCon _ args -> any containsInternalTypeVar args
        TVarAppRef ref args -> isInternalTypeBinderRef ref || any containsInternalTypeVar args
        TForallRef _ mb body -> maybe False containsInternalBoundVar mb || containsInternalTypeVar body
        TMuRef _ body -> containsInternalTypeVar body
        _ -> False

isInternalTypeBinderRef :: TypeBinderRef -> Bool
isInternalTypeBinderRef ref =
  isJust (typeBinderRefNode ref)

mkEnvBinding :: VarName -> IdDetails -> SchemeInfo -> Bool -> EnvBinding
mkEnvBinding _ details schemeInfo transparentMediator =
  EnvBinding
    { ebSchemeInfo = schemeInfo,
      ebSchemeType = schemeToType (siScheme schemeInfo),
      ebIdentityDetails = details,
      ebTransparentMediator = transparentMediator,
      ebAliasTarget = Nothing,
      ebExplicitRecursiveParam = False
    }

mkEnvValueBinding :: VarName -> EnvRef -> SchemeInfo -> Bool -> EnvBinding
mkEnvValueBinding name envRef =
  mkEnvBinding name (EnvId envRef)

mkLocalEnvBinding :: VarName -> NodeId -> SchemeInfo -> Bool -> EnvBinding
mkLocalEnvBinding name nodeId =
  mkEnvBinding name (LocalId (localRefFromNodeId name nodeId))

localBinderIsDiscard :: VarName -> NodeId -> Bool
localBinderIsDiscard name nodeId =
  idDetailsIsDiscard (LocalId (localRefFromNodeId name nodeId))

resolvedLocalBinderFromNode :: VarName -> NodeId -> ElabType -> ResolvedVar
resolvedLocalBinderFromNode name nodeId ty =
  ResolvedVar
    { resolvedVarRuntimeName = name,
      resolvedVarType = ty,
      resolvedVarDetails = LocalId (localRefFromNodeId name nodeId)
    }

mkLocalLamFromNode :: VarName -> NodeId -> ElabType -> XmlfTerm -> XmlfTerm
mkLocalLamFromNode name nodeId ty =
  ELam (resolvedLocalBinderFromNode name nodeId ty)

mkLocalLetFromNode :: VarName -> NodeId -> ElabScheme -> XmlfTerm -> XmlfTerm -> XmlfTerm
mkLocalLetFromNode name nodeId scheme =
  ELet (resolvedLocalBinderFromNode name nodeId (schemeToType scheme)) scheme

mkEnv :: Map.Map VarName SchemeInfo -> Env
mkEnv schemeInfos =
  mkEnvFromBindings (Map.fromList bindings)
  where
    (_, bindings) =
      mapAccumL mkBinding initialGenerator (Map.toList schemeInfos)

    initialGenerator =
      identityGeneratorAfter $
        concatMap (generatedIdentitiesInType . schemeToType . siScheme) (Map.elems schemeInfos)

    mkBinding generator (name, schemeInfo) =
      let (envRef, generator') = freshEnvRef name generator
       in (generator', (name, mkEnvValueBinding name envRef schemeInfo False))

mkEnvWithBindingDetails :: Map.Map VarName (SchemeInfo, IdDetails) -> Env
mkEnvWithBindingDetails schemeInfos =
  mkEnvFromBindings bindings
  where
    bindings =
      Map.mapWithKey
        (\name (schemeInfo, details) -> mkEnvBinding name details schemeInfo False)
        schemeInfos

mkEnvFromBindings :: Map.Map VarName EnvBinding -> Env
mkEnvFromBindings bindings =
  Env
    { envBindings = bindings,
      envBindingsByIdentity = envBindingIdentityIndex bindings,
      envTypeCheck = typeCheckEnvFromBindings bindings
    }

envBindingIdentityIndex :: Map.Map VarName EnvBinding -> Map.Map EnvBindingIdentityKey EnvBinding
envBindingIdentityIndex bindings =
  foldr
    ( \(_, binding) ->
        Map.insert (envBindingIdentityKey binding) binding
    )
    Map.empty
    (Map.toList bindings)

typeCheckEnvFromBindings :: Map.Map VarName EnvBinding -> TypeCheck.Env
typeCheckEnvFromBindings bindings =
  TypeCheck.mkTypeCheckEnvWithResolvedTerms
    [ (resolvedEnvBindingVar name binding, ebSchemeType binding)
    | (name, binding) <- Map.toList bindings
    ]
    Map.empty

envSchemeInfos :: Env -> Map.Map VarName SchemeInfo
envSchemeInfos = Map.map ebSchemeInfo . envBindings

envSchemeTypes :: Env -> Map.Map VarName ElabType
envSchemeTypes = Map.map ebSchemeType . envBindings

envIdentityGenerator :: Env -> IdentityGenerator
envIdentityGenerator env =
  identityGeneratorAfter (concatMap envBindingGeneratedIdentities (Map.elems (envBindings env)))
  where
    envBindingGeneratedIdentities binding =
      idDetailsGeneratedIdentities (ebIdentityDetails binding)
        ++ generatedIdentitiesInType (ebSchemeType binding)

typeCheckEnvFrom :: Env -> TypeCheck.Env
typeCheckEnvFrom = envTypeCheck

lookupEnvBinding :: VarName -> Env -> Maybe EnvBinding
lookupEnvBinding name env = Map.lookup name (envBindings env)

insertEnvBinding :: VarName -> EnvBinding -> Env -> Env
insertEnvBinding name binding env =
  env
    { envBindings = Map.insert name binding (envBindings env),
      envBindingsByIdentity =
        Map.insert newKey binding $
          maybe id Map.delete oldKey (envBindingsByIdentity env),
      envTypeCheck =
        TypeCheck.insertResolvedTermBinding
          (resolvedEnvBindingVar name binding)
          (ebSchemeType binding)
          (envTypeCheck env)
    }
  where
    oldKey = envBindingIdentityKey <$> Map.lookup name (envBindings env)
    newKey = envBindingIdentityKey binding

adjustEnvBinding :: (EnvBinding -> EnvBinding) -> VarName -> Env -> Env
adjustEnvBinding f name env =
  case lookupEnvBinding name env of
    Nothing -> env
    Just binding -> insertEnvBinding name (f binding) env

lookupSchemeInfo :: VarName -> Env -> Maybe SchemeInfo
lookupSchemeInfo name env = ebSchemeInfo <$> lookupEnvBinding name env

lookupSchemeInfoForResolved :: ResolvedVar -> Env -> Maybe SchemeInfo
lookupSchemeInfoForResolved resolved env =
  ebSchemeInfo <$> Map.lookup (envBindingDetailsKey (resolvedVarDetails resolved)) (envBindingsByIdentity env)

lookupSchemeType :: VarName -> Env -> Maybe ElabType
lookupSchemeType name env = ebSchemeType <$> lookupEnvBinding name env

envBindingIdentityKey :: EnvBinding -> EnvBindingIdentityKey
envBindingIdentityKey =
  envBindingDetailsKey . ebIdentityDetails

envBindingDetailsKey :: IdDetails -> EnvBindingIdentityKey
envBindingDetailsKey details =
  case details of
    LocalId ref -> EnvBindingLocalKey ref
    EvidenceId ref -> EnvBindingLocalKey ref
    EnvId ref -> EnvBindingEnvKey ref
    TopLevelId symbol -> EnvBindingTopLevelKey symbol
    ConstructorId ref -> EnvBindingConstructorKey (constructorRefSymbol ref)
    MethodId symbol -> EnvBindingMethodKey symbol
    PrimitiveId ref -> EnvBindingPrimitiveKey (primitiveRefSymbol ref)
    DeferredId ref -> EnvBindingDeferredKey ref

resolvedEnvBindingVar :: VarName -> EnvBinding -> ResolvedVar
resolvedEnvBindingVar name binding =
  ResolvedVar
    { resolvedVarRuntimeName = name,
      resolvedVarType = ebSchemeType binding,
      resolvedVarDetails = ebIdentityDetails binding
    }

newtype LocalVarKey
  = LocalVarResolved ResolvedVar

localVarKeyMatchesReference :: LocalVarKey -> ResolvedVar -> Bool
localVarKeyMatchesReference key resolved =
  case key of
    LocalVarResolved expected ->
      resolvedVarSameIdentity expected resolved

localVarKeyMatchesLocalOccurrence :: LocalVarKey -> ResolvedVar -> Bool
localVarKeyMatchesLocalOccurrence key resolved =
  case key of
    LocalVarResolved expected ->
      resolvedVarSameIdentity expected resolved

refreshLocalResolvedVarType :: LocalVarKey -> ElabType -> XmlfTerm -> XmlfTerm
refreshLocalResolvedVarType target ty =
  go
  where
    matches resolved =
      localVarKeyMatchesLocalOccurrence target resolved

    go term =
      case term of
        EVarNode resolved
          | matches resolved ->
              EVarNode (mapResolvedVarType (const ty) resolved)
          | otherwise -> term
        ELit {} -> term
        ELam resolved body
          | localVarKeyMatchesReference target resolved -> ELam resolved body
          | otherwise -> ELam resolved (go body)
        EApp fun arg -> EApp (go fun) (go arg)
        ELet resolved scheme rhs body
          | localVarKeyMatchesReference target resolved -> ELet resolved scheme rhs body
          | otherwise -> ELet resolved scheme (go rhs) (go body)
        ETyAbsRef ref mb body -> ETyAbsRef ref mb (go body)
        ETyInst inner inst -> ETyInst (go inner) inst
        ERoll rollTy body -> ERoll rollTy (go body)
        EUnroll body -> EUnroll (go body)

sourceAnnotatedTypeFrom :: AlgebraContext p -> Env -> AnnExpr -> Either ElabError (Maybe ElabType)
sourceAnnotatedTypeFrom algebraContext env ann =
  case ann of
    AVar name _ -> pure (lookupSchemeType name env)
    AAnn inner annNodeId _ ->
      case IntMap.lookup (getNodeId annNodeId) (algAnnSourceTypes algebraContext) of
        Just srcTy -> Just <$> srcTypeToElabType algebraContext srcTy
        Nothing -> sourceAnnotatedTypeFrom algebraContext env inner
    AUnfold inner _ _ -> sourceAnnotatedTypeFrom algebraContext env inner
    _ -> pure Nothing

sourceSchemePairFromType :: AlgebraContext p -> NormSrcType -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
sourceSchemePairFromType algebraContext srcTy = do
  ty <- srcTypeToElabType algebraContext srcTy
  pure (schemeFromType ty, IntMap.empty)

sourceSchemePairForNode :: AlgebraContext p -> ScopeContext p -> NodeId -> Either ElabError (Maybe (ElabScheme, IntMap.IntMap TypeBinderRef))
sourceSchemePairForNode algebraContext scopeContext nodeId =
  case IntMap.lookup (getNodeId nodeId) (algAnnSourceTypes algebraContext) of
    Just srcTy -> do
      fallback <- sourceSchemePairFromType algebraContext srcTy
      pure $
        case reifyNodeTypePreferringBound scopeContext nodeId of
          Right ty@TMuRef {} -> Just (schemeFromType ty, IntMap.empty)
          _ -> Just fallback
    Nothing -> pure Nothing

sourceSchemePairForOuterAnnotation :: AlgebraContext p -> ScopeContext p -> AnnExpr -> Either ElabError (Maybe (ElabScheme, IntMap.IntMap TypeBinderRef))
sourceSchemePairForOuterAnnotation algebraContext scopeContext annExpr =
  case annExpr of
    AAnn _ annNodeId _ -> sourceSchemePairForNode algebraContext scopeContext annNodeId
    AUnfold (AAnn _ annNodeId _) _ _ -> sourceSchemePairForNode algebraContext scopeContext annNodeId
    _ -> pure Nothing

sourceSchemePairForAnnotation :: AlgebraContext p -> ScopeContext p -> AnnExpr -> Either ElabError (Maybe (ElabScheme, IntMap.IntMap TypeBinderRef))
sourceSchemePairForAnnotation algebraContext scopeContext annExpr =
  case annExpr of
    AAnn inner annNodeId _ -> do
      current <- sourceSchemePairForNode algebraContext scopeContext annNodeId
      case current of
        Just _ -> pure current
        Nothing -> sourceSchemePairForAnnotation algebraContext scopeContext inner
    ALam _ _ _ body _ -> sourceSchemePairForAnnotation algebraContext scopeContext body
    AApp fun arg _ _ _ ->
      firstJustE
        (sourceSchemePairForAnnotation algebraContext scopeContext fun)
        (sourceSchemePairForAnnotation algebraContext scopeContext arg)
    ALet _ _ _ _ _ rhs body _ ->
      firstJustE
        (sourceSchemePairForAnnotation algebraContext scopeContext rhs)
        (sourceSchemePairForAnnotation algebraContext scopeContext body)
    AUnfold inner _ _ -> sourceSchemePairForAnnotation algebraContext scopeContext inner
    _ -> pure Nothing

firstJustE :: Either ElabError (Maybe a) -> Either ElabError (Maybe a) -> Either ElabError (Maybe a)
firstJustE left right = do
  result <- left
  case result of
    Just _ -> pure result
    Nothing -> right

lookupAliasTarget :: VarName -> Env -> Maybe VarName
lookupAliasTarget name env = lookupEnvBinding name env >>= ebAliasTarget

resolveAliasVar :: Env -> VarName -> VarName
resolveAliasVar env name =
  case lookupAliasTarget name env of
    Just target -> resolveAliasVar env target
    Nothing -> name

isTransparentMediatorVar :: VarName -> Env -> Bool
isTransparentMediatorVar name env = maybe False ebTransparentMediator (lookupEnvBinding name env)

freeTypeVarsEnvSchemes :: Env -> Set.Set String
freeTypeVarsEnvSchemes env =
  Set.unions
    [ freeTypeVarsType schemeTy
      | schemeTy <- Map.elems (envSchemeTypes env)
    ]

freeTypeVarRefsInOccurrenceOrder :: ElabType -> [TypeBinderRef]
freeTypeVarRefsInOccurrenceOrder ty0 = reverse (snd (goType [] [] [] ty0))
  where
    refMember ref =
      any (typeBinderRefsSameIdentity ref)

    addRef bound seen acc ref
      | refMember ref bound = (seen, acc)
      | refMember ref seen = (seen, acc)
      | otherwise = (ref : seen, ref : acc)

    goType bound seen acc ty =
      case ty of
        TVarRef ref -> addRef bound seen acc ref
        TArrow dom cod ->
          let (seen', acc') = goType bound seen acc dom
           in goType bound seen' acc' cod
        TCon _ args ->
          foldl' (\(seen', acc') arg -> goType bound seen' acc' arg) (seen, acc) args
        TVarAppRef ref args ->
          let (seen', acc') = addRef bound seen acc ref
           in foldl' (\(seen'', acc'') arg -> goType bound seen'' acc'' arg) (seen', acc') args
        TForallRef ref mb body ->
          let (seen', acc') =
                maybe (seen, acc) (\boundTy -> goType bound seen acc (tyToElab boundTy)) mb
           in goType (ref : bound) seen' acc' body
        TMuRef ref body -> goType (ref : bound) seen acc body
        TBase _ -> (seen, acc)
        TBottom -> (seen, acc)

freshenSchemeInfoAgainstEnv :: Env -> SchemeInfo -> SchemeInfo
freshenSchemeInfoAgainstEnv env schemeInfo =
  let reservedNames = freeTypeVarsEnvSchemes env
      scheme0 = siScheme schemeInfo
      binds = schemeBinderRefs scheme0
      body0 = schemeBody scheme0
      binderNames = map (typeBinderRefName . fst) binds
      binderDomain = Set.fromList binderNames
      renames = reverse (snd (foldl' (chooseFreshBinder binderDomain) (reservedNames, []) (map fst binds)))
      refRenames =
        [ (oldRef, renameTypeBinderRef newName oldRef)
          | (oldRef, newName) <- renames,
            typeBinderRefName oldRef /= newName
        ]
   in if null refRenames
        then schemeInfo
        else
          let binds' = renameSchemeBinds refRenames binds
              body' = applyTypeVarRefRenames refRenames body0
              scheme' = mkElabSchemeWithRefs binds' body'
              subst' = IntMap.map (applyRefRenames refRenames) (schemeInfoBinderRefSubst schemeInfo)
           in schemeInfoFromRefSubst scheme' subst'
  where
    chooseFreshBinder binderDomain (used, acc) binder =
      let name = typeBinderRefName binder
          name' =
            if Set.member name used
              then freshNameLike name (Set.union used binderDomain)
              else name
       in (Set.insert name' used, (binder, name') : acc)

    renameSchemeBinds renames0 = go []
      where
        go _ [] = []
        go prev ((ref, mbBound) : restBinds) =
          let ref' = applyRefRenames renames0 ref
              mbBound' = fmap (mapBoundType (applyTypeVarRefRenames prev)) mbBound
              prev'
                | typeBinderRefsSameIdentityAndName ref ref' = prev
                | otherwise = prev ++ [(ref, ref')]
           in (ref', mbBound') : go prev' restBinds

    applyRefRenames [] ref = ref
    applyRefRenames ((oldRef, newRef) : rest) ref
      | typeBinderRefsSameIdentity oldRef ref = newRef
      | otherwise = applyRefRenames rest ref

    applyTypeVarRefRenames :: [(TypeBinderRef, TypeBinderRef)] -> ElabType -> ElabType
    applyTypeVarRefRenames renames0 ty0 =
      foldl'
        ( \ty (oldRef, newRef) ->
            if typeBinderRefsSameIdentityAndName oldRef newRef
              then ty
              else substTypeCaptureRef oldRef (TVarRef newRef) ty
        )
        ty0
        renames0

structuralRecursiveCandidateType :: StructuralRecursiveCandidate -> ElabType
structuralRecursiveCandidateType candidate =
  case candidate of
    StructuralRecursiveCandidateFromHelper ty -> ty
    StructuralRecursiveCandidateFromDirectCarrier ty -> ty

selectStructuralRecursiveCandidate :: [StructuralRecursiveCandidate] -> StructuralRecursiveCandidateSelection
selectStructuralRecursiveCandidate =
  selectUniqueCandidateBy
    ( \existing candidate ->
        alphaEqType
          (structuralRecursiveCandidateType existing)
          (structuralRecursiveCandidateType candidate)
    )

schemeHasForwardBoundReference :: ElabType -> Bool
schemeHasForwardBoundReference schemeTy =
  let (binds, _) = splitForallsRefs schemeTy
      go [] = False
      go ((_, mbBound) : rest) =
        let laterRefs = map fst rest
            boundMentionsLater =
              case mbBound of
                Just bound ->
                  any
                    (\laterRef -> any (typeBinderRefsSameIdentity laterRef) (freeTypeVarRefsType bound))
                    laterRefs
                Nothing -> False
         in boundMentionsLater || go rest
   in go binds

schemeTypeHasExplicitBound :: ElabType -> Bool
schemeTypeHasExplicitBound schemeTy =
  let (binds, _) = splitForallsRefs schemeTy
   in any (isJust . snd) binds

stripAnnExpr :: AnnExpr -> AnnExpr
stripAnnExpr annExpr =
  case annExpr of
    AAnn inner _ _ -> stripAnnExpr inner
    AUnfold inner _ _ -> stripAnnExpr inner
    _ -> annExpr

stripLeadingTyAbs :: XmlfTerm -> XmlfTerm
stripLeadingTyAbs term =
  case term of
    ETyAbsRef _ _ body -> stripLeadingTyAbs body
    _ -> term

annAppSpine :: AnnExpr -> (AnnExpr, [AnnExpr])
annAppSpine annExpr =
  let go args expr =
        case stripAnnExpr expr of
          AApp fun arg _ _ _ -> go (arg : args) fun
          other -> (other, args)
   in go [] annExpr

transparentMediatorSignatureFor :: VarName -> AnnExpr -> Maybe ([NodeId], AnnExpr)
transparentMediatorSignatureFor rootParam = transparentMediatorBody rootParam Map.empty []
  where
    transparentMediatorBody root aliases etaParams expr =
      case stripAnnExpr expr of
        ALam param paramNode _ body _
          | param == root
              || Map.member param aliases
              || param `elem` map fst etaParams ->
              Nothing
          | otherwise ->
              transparentMediatorBody root aliases (etaParams ++ [(param, paramNode)]) body
        ALet boundName _ _ _ _ rhs body _
          | boundName == root
              || Map.member boundName aliases
              || boundName `elem` map fst etaParams ->
              Nothing
          | Just origin <- transparentMediatorAliasOrigin root aliases etaParams rhs ->
              transparentMediatorBody root (Map.insert boundName origin aliases) etaParams body
          | otherwise ->
              Nothing
        other ->
          let (funExpr, argExprs) = annAppSpine other
           in if transparentMediatorHead root aliases funExpr
                && length argExprs == length etaParams
                && and (zipWith (transparentMediatorArg aliases) argExprs (map fst etaParams))
                then Just (map snd etaParams, other)
                else Nothing

    transparentMediatorAliasOrigin root aliases etaParams rhs =
      case stripAnnExpr rhs of
        AVar name _
          | resolvedMediatorName aliases name `elem` (root : map fst etaParams) ->
              Just (resolvedMediatorName aliases name)
        _ -> Nothing

    transparentMediatorHead root aliases expr =
      case stripAnnExpr expr of
        AVar name _ -> resolvedMediatorName aliases name == root
        _ -> False

    transparentMediatorArg aliases expr expectedParam =
      case stripAnnExpr expr of
        AVar name _ -> resolvedMediatorName aliases name == expectedParam
        _ -> False

    resolvedMediatorName aliases name =
      case Map.lookup name aliases of
        Just origin -> origin
        Nothing -> name

isTransparentMediatorBodyFor :: VarName -> AnnExpr -> Bool
isTransparentMediatorBodyFor rootParam = isJust . transparentMediatorSignatureFor rootParam

isTransparentMediatorAnn :: AnnExpr -> Bool
isTransparentMediatorAnn annExpr =
  case stripAnnExpr annExpr of
    ALam rootParam _ _ body _ -> isTransparentMediatorBodyFor rootParam body
    _ -> False

elabAlg :: AlgebraContext p -> AnnExprF (AnnExpr, ElabOut) -> ElabOut
elabAlg algebraContext layer =
  case layer of
    AVarF v _ -> mkOut $ \env ->
      maybe (Left (EnvLookup v)) (Right . EVarNode . resolvedEnvBindingVar v) (lookupEnvBinding v env)
    ALitF lit _ -> mkOut $ \_ -> Right (ELit lit)
    ALamF v paramNode _ (bodyAnn, bodyOut) lamNodeId ->
      let f env = do
            let mAnnLambda = desugaredAnnLambdaInfo v bodyAnn
                resolvedParam = algResolvedLambdaParamNode algebraContext lamNodeId
                isBareInternalTyVar ty =
                  case ty of
                    TVarRef ref -> isInternalTypeBinderRef ref
                    _ -> False
                recursiveParamTyFromEnv annExpr =
                  let mediatedVarUse expr =
                        case expr of
                          AVar name _ -> name == v
                          AAnn inner _ _ -> mediatedVarUse inner
                          AUnfold inner _ _ -> mediatedVarUse inner
                          AApp fun arg _ _ _ ->
                            case (fun, arg) of
                              (AVar funName _, innerArg)
                                | isTransparentMediatorVar funName env ->
                                    mediatedVarUse innerArg
                              _ -> False
                          _ -> False
                      firstRecursiveDomain expr =
                        case expr of
                          AApp (AVar recurName _) arg _ _ _
                            | mediatedVarUse arg,
                              Just schemeTy <- lookupSchemeType recurName env ->
                                case schemeTy of
                                  muTy@(TMuRef muRef muBody)
                                    | hasContractiveRecursiveWitness muTy ->
                                        case substTypeCaptureRef muRef muTy muBody of
                                          TArrow dom _ -> Just dom
                                          _ -> Nothing
                                  _ -> Nothing
                          ALam boundName _ _ inner _
                            | boundName == v -> Nothing
                            | otherwise -> firstRecursiveDomain inner
                          AApp fun arg _ _ _ ->
                            case firstRecursiveDomain fun of
                              Just dom -> Just dom
                              Nothing -> firstRecursiveDomain arg
                          ALet boundName _ _ _ _ rhs body _
                            | boundName == v ->
                                firstRecursiveDomain rhs
                            | otherwise ->
                                case firstRecursiveDomain rhs of
                                  Just dom -> Just dom
                                  Nothing -> firstRecursiveDomain body
                          AAnn inner _ _ -> firstRecursiveDomain inner
                          AUnfold inner _ _ -> firstRecursiveDomain inner
                          _ -> Nothing
                   in firstRecursiveDomain annExpr
                transparentParamTyFromBody annExpr =
                  case transparentMediatorSignatureFor v annExpr of
                    Just (etaParamNodes, resultExpr) -> do
                      etaParamTys <-
                        traverse
                          (\etaParamNode -> either (const Nothing) Just (reifyNodeTypePreferringBound scopeContext etaParamNode))
                          etaParamNodes
                      resultTy <- either (const Nothing) Just (reifyNodeTypePreferringBound scopeContext (annNode resultExpr))
                      pure (foldr TArrow resultTy etaParamTys)
                    Nothing -> Nothing
            paramSource <-
              case mAnnLambda of
                Just _ -> pure (fromMaybe paramNode resolvedParam)
                Nothing ->
                  case resolvedParam of
                    Nothing -> pure paramNode
                    Just resolvedNode ->
                      case reifyNodeTypePreferringBound scopeContext resolvedNode of
                        Right TBottom -> pure paramNode
                        Right ty
                          | isBareInternalTyVar ty -> pure paramNode
                        _ -> pure resolvedNode
            let bodyElabOut =
                  case mAnnLambda of
                    Just (_, _, innerBodyAnn) -> para (elabAlg algebraContext) innerBodyAnn
                    Nothing -> bodyOut
            paramTySurface0 <- reifyNodeTypePreferringBound scopeContext paramSource
            let paramTySurface =
                  if isBareInternalTyVar paramTySurface0
                    then
                      fromMaybe
                        (fromMaybe paramTySurface0 (transparentParamTyFromBody bodyAnn))
                        (recursiveParamTyFromEnv bodyAnn)
                    else paramTySurface0
            (paramTy, paramSchemeInfo) <-
              case mAnnLambda of
                Just (annNodeId, _, _) ->
                  -- First, check if we have the original source annotation type
                  -- preserved from constraint generation.  This is the exact type
                  -- the user wrote (after lowering), which presolution may have
                  -- corrupted (e.g. stripping TForall inside a μ body).
                  case IntMap.lookup (getNodeId annNodeId) (algAnnSourceTypes algebraContext) of
                    Just srcTy -> do
                      preservedTy <- srcTypeToElabType algebraContext srcTy
                      pure
                        ( preservedTy,
                          schemeInfoFromRefSubst (schemeFromType preservedTy) IntMap.empty
                        )
                    Nothing ->
                      case generalizeAtNode scopeContext annNodeId of
                        Right (paramScheme, _subst) ->
                          let paramTy0 = case (schemeBinderRefs paramScheme, schemeBody paramScheme) of
                                ([(ref, Just bnd)], TVarRef bodyRef)
                                  | typeBinderRefsSameIdentity ref bodyRef -> tyToElab bnd
                                _ -> schemeToType paramScheme
                              -- If generalizeAtNode returned a bare TVar (over-generalized)
                              -- or a base type that disagrees with the constraint graph's
                              -- solved μ type, fall back to reifyNodeTypePreferringBound.
                              -- This handles the case where ELamAnn's desugared
                              -- annotation-let picks up the body's result type (e.g. Bool)
                              -- instead of the actual annotation type (e.g. μ Nat).
                              paramTyResolved = case paramTy0 of
                                TVarRef {} ->
                                  case reifyNodeTypePreferringBound scopeContext annNodeId of
                                    Right ty@TMuRef {} -> ty
                                    _ -> paramTy0
                                TBase {} ->
                                  case reifyNodeTypePreferringBound scopeContext annNodeId of
                                    Right ty@TMuRef {} -> ty
                                    _ -> paramTy0
                                _
                                  | TMuRef {} <- paramTySurface,
                                    Just unfoldedSurface <- unfoldMuOnce paramTySurface,
                                    (alphaEqType unfoldedSurface paramTy0 || churchAwareEqType unfoldedSurface paramTy0) ->
                                      paramTySurface
                                _ -> paramTy0
                           in pure
                                ( paramTyResolved,
                                  schemeInfoFromRefSubst (schemeFromType paramTyResolved) IntMap.empty
                                )
                        Left (SchemeFreeVars _ _) ->
                          pure
                            ( paramTySurface,
                              schemeInfoFromRefSubst (schemeFromType paramTySurface) IntMap.empty
                            )
                        Left err -> Left err
                Nothing ->
                  pure
                    ( paramTySurface,
                      schemeInfoFromRefSubst (schemeFromType paramTySurface) IntMap.empty
                    )
            let env' = insertEnvBinding v (mkLocalEnvBinding v paramNode paramSchemeInfo False) env
                env'' =
                  adjustEnvBinding
                    ( \binding ->
                        binding
                          { ebExplicitRecursiveParam =
                              isJust mAnnLambda && hasContractiveRecursiveWitness paramTy
                          }
                    )
                    v
                    env'
            bodyRaw <- elabTerm bodyElabOut env''
            let bodyTcEnv = typeCheckEnvFrom env''
                body' = stripUnusedTopTyAbsWithEnv bodyTcEnv bodyRaw
            pure (mkLocalLamFromNode v paramNode paramTy body')
       in mkOut f
    AAppF (fAnn, fOut) (aAnn, aOut) funEid argEid appNodeId ->
      let f env = do
            f' <- elabTerm fOut env
            a' <- elabTerm aOut env
            argSourceSchemeTy <- sourceAnnotatedTypeFrom algebraContext env aAnn
            let schemeEnv = envSchemeInfos env
                tcEnv = typeCheckEnvFrom env
                typeCheckLocal = TypeCheck.typeCheckWithEnv tcEnv
                fTyChecked = typeCheckLocal f'
                argTyChecked = typeCheckLocal a'
                fSourceName = sourceVarName fAnn
                aSourceName = sourceVarName aAnn
                resolvedTermHead term =
                  case term of
                    EVarNode resolved -> Just resolved
                    ETyInst inner _ -> resolvedTermHead inner
                    EUnroll inner -> resolvedTermHead inner
                    _ -> Nothing
                schemeInfoForTermOrName term mbName =
                  case resolvedTermHead term >>= (`lookupSchemeInfoForResolved` env) of
                    Just schemeInfo -> Just schemeInfo
                    Nothing -> mbName >>= (`lookupSchemeInfo` env)
                schemeTypeForTermOrName term mbName =
                  schemeToType . siScheme <$> schemeInfoForTermOrName term mbName
                appTargetTy =
                  let directTy = either (const Nothing) Just (reifyNodeTypeDirect scopeContext appNodeId)
                      boundTy = either (const Nothing) Just (reifyNodeTypePreferringBound scopeContext appNodeId)
                   in case directTy of
                        Just TVarRef {} -> boundTy <|> directTy
                        Just TBottom -> boundTy <|> directTy
                        Just directTy'
                          | Just boundMu@TMuRef {} <- boundTy,
                            Just unfoldedBound <- unfoldMuOnce boundMu,
                            let directNorm = stripVacuousForallsDeep directTy',
                            let unfoldedNorm = stripVacuousForallsDeep unfoldedBound,
                            (alphaEqType unfoldedNorm directNorm || churchAwareEqType unfoldedNorm directNorm) ->
                              boundTy
                        _ -> directTy
                annHasMuScheme ann =
                  case sourceVarName ann >>= (`lookupSchemeType` env) of
                    Just schemeTy ->
                      case schemeTy of
                        TMuRef {} -> True
                        _ -> False
                    Nothing -> False
                argIsExplicitRecursiveParam ann =
                  case sourceVarName ann >>= (`lookupEnvBinding` env) of
                    Just binding -> ebExplicitRecursiveParam binding
                    Nothing -> False
                sourceMuMatchesActualType sourceTy actualTy =
                  alphaEqType sourceTy actualTy
                    || churchAwareEqType sourceTy actualTy
                    || case sourceTy of
                      TMuRef {} ->
                        case unfoldMuOnce sourceTy of
                          Just unfoldedTy ->
                            let unfoldedTy' = stripVacuousForallsDeep unfoldedTy
                                actualTy' = stripVacuousForallsDeep actualTy
                             in alphaEqType unfoldedTy' actualTy' || churchAwareEqType unfoldedTy' actualTy'
                          Nothing -> False
                      _ -> False
                preferSourceMuArgTy actualTy =
                  case argSourceSchemeTy of
                    Just sourceTy@TMuRef {}
                      | sourceMuMatchesActualType sourceTy actualTy -> sourceTy
                    _ -> actualTy
                recoverIdentityLikeRecursiveFunInst ann =
                  case (sourceVarName ann, argTyChecked) of
                    (Just {}, Right argTy)
                      | hasContractiveRecursiveWitness argTy ->
                          case schemeInfoForTermOrName f' (sourceVarName ann) of
                            Just schemeInfo
                              | isSingleBinderIdentityScheme schemeInfo ->
                                  let candidate = InstApp argTy
                                      fAppCandidate = ETyInst f' candidate
                                   in case typeCheckLocal (EApp fAppCandidate a') of
                                        Right _ -> Just candidate
                                        Left _ -> Nothing
                            _ -> Nothing
                    _ -> Nothing
                reifyInstWithRecovery ann eid _term termTy
                  | Nothing <- sourceVarName ann,
                    Right ty <- termTy,
                    not (case ty of TForallRef {} -> True; _ -> False) =
                      Right InstId
                  | otherwise =
                      case reifyInst annotationContext namedSetReify schemeEnv ann eid of
                        Right inst -> Right inst
                        Left err@(PhiTranslatabilityError _)
                          | Just inst <- recoverIdentityLikeRecursiveFunInst ann -> Right inst
                          | annHasMuScheme ann -> Right InstId
                          | otherwise -> Left err
                        Left err -> Left err
                reifyInstIfPolymorphic ann eid term termTy
                  | sourceAnnIsPolymorphic schemeEnv ann =
                      reifyInstWithRecovery ann eid term termTy
                  | otherwise = Right InstId
                recursiveWitnessArgTerm =
                  case argTyChecked of
                    Right argTy
                      | hasContractiveRecursiveWitness argTy -> Just a'
                    _ ->
                      case aSourceName >>= (`lookupSchemeType` env) of
                        Just argSchemeTy
                          | hasContractiveRecursiveWitness argSchemeTy ->
                              Just a'
                        _ -> Nothing
                transparentOrIdentityBypassTerm =
                  case fSourceName of
                    Just fName
                      | isTransparentMediatorVar fName env ->
                          let aStripped = stripUnusedTopTyAbs a'
                           in case aStripped of
                                ELam {} -> Just a'
                                _ -> recursiveWitnessArgTerm
                      | Just schemeInfo <- schemeInfoForTermOrName f' fSourceName,
                        isSingleBinderIdentityScheme schemeInfo ->
                          recursiveWitnessArgTerm
                    _ -> Nothing
            funInst <-
              case transparentOrIdentityBypassTerm of
                Just _ -> Right InstId
                Nothing -> reifyInstIfPolymorphic fAnn funEid f' fTyChecked
            argInst <-
              case transparentOrIdentityBypassTerm of
                Just _ -> Right InstId
                Nothing -> reifyInstIfPolymorphic aAnn argEid a' argTyChecked
            let fHead = appHeadTermFromType fTyChecked f'
                fHeadTyChecked = typeCheckLocal fHead
                fHeadTy = either (const Nothing) Just fHeadTyChecked
                fIsMuHead =
                  case fTyChecked of
                    Right TMuRef {} -> True
                    _ -> False
                recoveredArgTy =
                  either
                    ( const
                        ( either
                            (const Nothing)
                            (Just . preferSourceMuArgTy)
                            (reifyNodeTypePreferringBound scopeContext (annNode aAnn))
                        )
                    )
                    (Just . preferSourceMuArgTy)
                    argTyChecked
                funInstByFunType =
                  case funInst of
                    inst0@(InstApp _) ->
                      case fHeadTy of
                        Just TForallRef {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    inst0@(InstInside (InstBot _)) ->
                      case fHeadTy of
                        Just TForallRef {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    inst0@(InstInside (InstApp _)) ->
                      case fHeadTy of
                        Just TForallRef {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    inst0@(InstSeq (InstInside (InstBot _)) InstElim) ->
                      case fHeadTy of
                        Just TForallRef {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    inst0@(InstSeq (InstInside (InstApp _)) InstElim) ->
                      case fHeadTy of
                        Just TForallRef {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    _ -> funInst
                funInst' =
                  case recoveredArgTy of
                    recoveredArg ->
                      case funInstByFunType of
                        inst0@(InstApp ty0) ->
                          case ty0 of
                            TVarRef {} -> maybe inst0 InstApp recoveredArg
                            TForallRef {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        inst0@(InstSeq (InstInside (InstBot ty0)) InstElim) ->
                          case ty0 of
                            TVarRef {} -> maybe inst0 InstApp recoveredArg
                            TForallRef {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        inst0@(InstSeq (InstInside (InstApp ty0)) InstElim) ->
                          case ty0 of
                            TVarRef {} -> maybe inst0 InstApp recoveredArg
                            TForallRef {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        _ -> funInstByFunType
                normalizeFunInst inst0 =
                  case fHeadTy of
                    Just fTy -> go 0 inst0
                      where
                        isAppLikeInst instX =
                          case instX of
                            InstApp {} -> True
                            InstSeq (InstInside (InstBot _)) InstElim -> True
                            InstSeq (InstInside (InstApp _)) InstElim -> True
                            _ -> False
                        canonicalizeAppLikeInst instX =
                          case instX of
                            InstApp ty -> InstApp ty
                            InstSeq (InstInside (InstBot ty)) InstElim -> InstApp ty
                            InstSeq (InstInside (InstApp ty)) InstElim -> InstApp ty
                            _ -> instX
                        go n instN
                          | n >= (8 :: Int) = instN
                          | otherwise =
                              case applyInstantiation fTy instN of
                                Right (TForallRef _ (Just _) _) ->
                                  if isAppLikeInst instN
                                    then canonicalizeAppLikeInst instN
                                    else go (n + 1) (InstSeq instN InstElim)
                                Right (TForallRef _ Nothing _) ->
                                  case (instN, recoveredArgTy) of
                                    (InstId, Just argTy) ->
                                      fromMaybe
                                        (InstApp argTy)
                                        (inferredFunInstForArg argTy)
                                    _ -> instN
                                Right _ -> instN
                                Left _ -> instN
                    Nothing -> inst0
                targetResultInstCandidates =
                  case (fIsMuHead, fHeadTy) of
                    (True, Just TForallRef {}) ->
                      [ InstApp (finalCodomain argTy)
                      | Just argTy <- [recoveredArgTy]
                      ]
                        ++ [ InstApp (finalCodomain targetTy)
                           | Just targetTy <- [appTargetTy]
                           ]
                    _ -> []
                funInstNorm0 = normalizeFunInst funInst'
                validatesFunInstForArg instCandidate =
                  let fCandidate = ETyInst fHead instCandidate
                   in case typeCheckLocal (EApp fCandidate a') of
                        Right _ -> True
                        Left _ -> False
                inferredFunInstForArg argTy =
                  case schemeInfoForTermOrName f' fSourceName of
                    Nothing -> Nothing
                    Just schemeInfo ->
                      inferFullSpineInstFromArgTypes (siScheme schemeInfo) [argTy]
                        <|> let targets =
                                  maybe [] (\targetTy -> [TArrow argTy targetTy]) appTargetTy
                                    ++ [TArrow argTy TBottom]
                                candidateFor targetTy = do
                                  args <- inferInstAppArgs (siScheme schemeInfo) targetTy
                                  let args' =
                                        map
                                          (inlineBoundVarsTypeWithContext inlineBoundVarsContext)
                                          args
                                      instCandidate = instSeqApps args'
                                  if null args'
                                    then Nothing
                                    else Just instCandidate
                             in listToMaybe
                                  [ instCandidate
                                  | Just instCandidate <- map candidateFor targets,
                                    validatesFunInstForArg instCandidate
                                  ]
                validatesTargetResultInst instCandidate =
                  let fCandidate = ETyInst fHead instCandidate
                   in case typeCheckLocal (EApp fCandidate a') of
                        Right _ -> True
                        Left _ -> False
                funInstNorm =
                  fromMaybe
                    funInstNorm0
                    (find validatesTargetResultInst targetResultInstCandidates)
                funInstRecovered =
                  let fApp0 = case funInstNorm of
                        InstId -> fHead
                        _ -> ETyInst fHead funInstNorm
                   in case ( typeCheckLocal (EApp fApp0 a'),
                             fSourceName,
                             aSourceName,
                             argTyChecked
                           ) of
                        (Right (TArrow _ TBottom), Just {}, mArgName, Right argTy) ->
                          case schemeTypeForTermOrName f' fSourceName of
                            Just fSchemeTy ->
                              let argTyPreferred =
                                    case schemeTypeForTermOrName a' mArgName of
                                      Just argSchemeTy ->
                                        case splitForallsRefs argSchemeTy of
                                          ([], monoTy) -> monoTy
                                          _ -> argTy
                                      Nothing -> argTy
                                  (fBinds, fBodyTy) = splitForallsRefs fSchemeTy
                                  fBinderRefs = map fst fBinds
                               in case fBodyTy of
                                    TArrow (TVarRef headRef) retTy
                                      | any (typeBinderRefsSameIdentity headRef) fBinderRefs
                                          && any (typeBinderRefsSameIdentity headRef) (freeTypeVarRefsType retTy) ->
                                          normalizeFunInst (InstApp argTyPreferred)
                                    _ -> funInstNorm
                            Nothing -> funInstNorm
                        (Left _, Just {}, _, Right argTy)
                          | Just inst <- inferredFunInstForArg argTy ->
                              inst
                        (Left _, _, _, _) ->
                          fromMaybe funInstNorm $
                            case (fSourceName, argTyChecked) of
                              (Just fName, Right argTy)
                                | hasContractiveRecursiveWitness argTy ->
                                    case schemeInfoForTermOrName f' fSourceName of
                                      Just _
                                        | isTransparentMediatorVar fName env ->
                                            let candidate = normalizeFunInst (InstApp argTy)
                                                fAppCandidate = case candidate of
                                                  InstId -> fHead
                                                  _ -> ETyInst fHead candidate
                                             in case typeCheckLocal (EApp fAppCandidate a') of
                                                  Right _ -> Just candidate
                                                  Left _ -> Nothing
                                      _ -> Nothing
                              _ -> Nothing
                        _ -> funInstNorm
                funInstValidated =
                  case funInstRecovered of
                    InstId -> InstId
                    instCandidate ->
                      let isAppLikeInst inst0 =
                            case inst0 of
                              InstApp {} -> True
                              InstSeq (InstInside (InstBot _)) InstElim -> True
                              InstSeq (InstInside (InstApp _)) InstElim -> True
                              _ -> False
                          fCandidate = ETyInst fHead instCandidate
                          fCandidateTy = typeCheckLocal fCandidate
                          keepCandidate =
                            case (isAppLikeInst instCandidate, fHeadTy, fSourceName) of
                              (True, Just TForallRef {}, _) ->
                                case fCandidateTy of
                                  Right _ -> True
                                  Left _ -> False
                              (True, Nothing, Nothing) -> False
                              (True, _, _) -> False
                              _ ->
                                case fCandidateTy of
                                  Right _ -> True
                                  Left _ -> False
                       in if keepCandidate
                            then instCandidate
                            else case recoveredArgTy of
                              Just argTy ->
                                let recoveredCandidate = normalizeFunInst (InstApp argTy)
                                 in case recoveredCandidate of
                                      InstId -> InstId
                                      _ ->
                                        let fRecovered = ETyInst fHead recoveredCandidate
                                         in case typeCheckLocal fRecovered of
                                              Right _ -> recoveredCandidate
                                              Left _ -> InstId
                              Nothing -> InstId
                fAppForArgInference = case funInstValidated of
                  InstId -> fHead
                  _ -> ETyInst fHead funInstValidated
                fAppForArgInferenceTy = typeCheckLocal fAppForArgInference
                firstClassPolymorphicArgInst =
                  case (sourceAnnIsPolymorphic schemeEnv aAnn, fSourceName, aSourceName, argSourceSchemeTy, fAppForArgInferenceTy) of
                    (True, Just fName, Just argName, Just sourceTy, Right (TArrow paramTy _))
                      | fName == argName,
                        alphaEqType paramTy sourceTy || churchAwareEqType paramTy sourceTy,
                        Right _ <- typeCheckLocal (EApp fAppForArgInference a') ->
                          Just (InstSeq InstIntro InstElim)
                    (True, _, _, Just sourceTy, Right (TArrow paramTy _))
                      | alphaEqType paramTy sourceTy || churchAwareEqType paramTy sourceTy,
                        Right _ <- typeCheckLocal (EApp fAppForArgInference a') ->
                          Just InstId
                    _ -> Nothing
                argInstFromFun =
                  let shouldInlineParamTy =
                        case (fSourceName, aSourceName) of
                          (Just fName, Just argName) -> fName /= argName
                          _ -> False
                      shouldInferArgInst =
                        case (fSourceName, aSourceName) of
                          (Just fName, Just argName) -> fName /= argName
                          _ -> True
                   in if not shouldInferArgInst
                        then Nothing
                        else case (aSourceName, f') of
                          (Just vName, ELam resolved _) -> do
                            schemeInfo <- schemeInfoForTermOrName a' (Just vName)
                            let paramTy = resolvedVarType resolved
                            let paramTy' =
                                  if shouldInlineParamTy
                                    then inlineBoundVarsTypeWithContext inlineBoundVarsContext paramTy
                                    else paramTy
                            args <- inferInstAppArgs (siScheme schemeInfo) paramTy'
                            pure (instSeqApps (map (inlineBoundVarsTypeWithContext inlineBoundVarsContext) args))
                          (Just vName, _) -> do
                            schemeInfo <- schemeInfoForTermOrName a' (Just vName)
                            case fAppForArgInferenceTy of
                              Right (TArrow paramTy _) -> do
                                let paramTy' =
                                      if shouldInlineParamTy
                                        then inlineBoundVarsTypeWithContext inlineBoundVarsContext paramTy
                                        else paramTy
                                args <- inferInstAppArgs (siScheme schemeInfo) paramTy'
                                pure (instSeqApps (map (inlineBoundVarsTypeWithContext inlineBoundVarsContext) args))
                              _ -> Nothing
                          _ -> Nothing
                argInstFallback =
                  case (fSourceName, aSourceName, fAppForArgInferenceTy, argInst) of
                    (Just fName, Just argName, Right (TArrow paramTy _), InstApp argTy)
                      | fName == argName,
                        Just schemeInfo <- schemeInfoForTermOrName a' aSourceName,
                        case schemeBinderRefs (siScheme schemeInfo) of
                          [(_, Nothing)] -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- typeCheckLocal (ETyInst a' instCandidate),
                        alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstInside (InstBot argTy))
                      | fName == argName,
                        Just schemeInfo <- schemeInfoForTermOrName a' aSourceName,
                        case schemeBinderRefs (siScheme schemeInfo) of
                          [(_, Nothing)] -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- typeCheckLocal (ETyInst a' instCandidate),
                        alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstInside (InstApp argTy))
                      | fName == argName,
                        Just schemeInfo <- schemeInfoForTermOrName a' aSourceName,
                        case schemeBinderRefs (siScheme schemeInfo) of
                          [(_, Nothing)] -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- typeCheckLocal (ETyInst a' instCandidate),
                        alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstSeq (InstInside (InstBot argTy)) InstElim)
                      | fName == argName,
                        Just schemeInfo <- schemeInfoForTermOrName a' aSourceName,
                        case schemeBinderRefs (siScheme schemeInfo) of
                          [(_, Nothing)] -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- typeCheckLocal (ETyInst a' instCandidate),
                        alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstSeq (InstInside (InstApp argTy)) InstElim)
                      | fName == argName,
                        Just schemeInfo <- schemeInfoForTermOrName a' aSourceName,
                        case schemeBinderRefs (siScheme schemeInfo) of
                          [(_, Nothing)] -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- typeCheckLocal (ETyInst a' instCandidate),
                        alphaEqType argTy' paramTy ->
                          instCandidate
                    _ ->
                      case (sourceAnnIsPolymorphic schemeEnv aAnn, argInstFromFun) of
                        (True, Just inst) -> inst
                        _ -> argInst
                argInst' =
                  fromMaybe argInstFallback firstClassPolymorphicArgInst
                argInstFinal =
                  case transparentOrIdentityBypassTerm of
                    Just _ -> InstId
                    Nothing ->
                      let isAppLikeInst inst0 =
                            case inst0 of
                              InstApp {} -> True
                              InstSeq (InstInside (InstBot _)) InstElim -> True
                              InstSeq (InstInside (InstApp _)) InstElim -> True
                              _ -> False
                          canonicalizeAppLikeInst inst0 =
                            case inst0 of
                              InstApp ty -> InstApp ty
                              InstSeq (InstInside (InstBot ty)) InstElim -> InstApp ty
                              InstSeq (InstInside (InstApp ty)) InstElim -> InstApp ty
                              _ -> inst0
                       in case argInst' of
                            InstId -> InstId
                            _
                                      | isAppLikeInst argInst' ->
                                          case argTyChecked of
                                            Right TForallRef {} -> canonicalizeAppLikeInst argInst'
                                            _ -> InstId
                                      | otherwise ->
                                          case argTyChecked of
                                            Right (TForallRef _ (Just _) _) -> InstElim
                                            Right TForallRef {} -> argInst'
                                            _ -> InstId
                aApp =
                  case transparentOrIdentityBypassTerm of
                    Just bypassTerm -> bypassTerm
                    Nothing ->
                      case argInstFinal of
                        InstId -> a'
                        _ -> ETyInst a' argInstFinal
                aAppTyChecked = typeCheckLocal aApp
                repairAppliedPolymorphicHead funTerm currentArg =
                  case typeCheckLocal (EApp funTerm currentArg) of
                    Right _ -> funTerm
                    Left _ ->
                      case (collectApplicationSpine funTerm, appTargetTy, aAppTyChecked) of
                        ((headTerm, previousArgs@(_:_)), Just resultTy, Right currentArgTy) ->
                          case headTerm of
                            EVarNode resolved -> repair resolved resultTy currentArgTy previousArgs
                            ETyInst (EVarNode resolved) _ -> repair resolved resultTy currentArgTy previousArgs
                            _ -> funTerm
                        _ -> funTerm
                  where
                    repair resolved _resultTy currentArgTy previousArgs =
                      case (lookupSchemeInfoForResolved resolved env, traverse typeCheckLocal previousArgs) of
                        (Just schemeInfo, Right previousArgTys) ->
                          case inferFullSpineInstFromArgTypes (siScheme schemeInfo) (previousArgTys ++ [currentArgTy]) of
                                Just headInst ->
                                      let rebuilt =
                                            foldl'
                                              EApp
                                              (ETyInst (EVarNode resolved) headInst)
                                              previousArgs
                                       in case typeCheckLocal (EApp rebuilt currentArg) of
                                            Right _ -> rebuilt
                                            Left _ -> funTerm
                                _ -> funTerm
                        _ -> funTerm
                fApp =
                  let fApp0Raw = case funInstValidated of
                        InstId -> fHead
                        _ -> ETyInst fHead funInstValidated
                      fApp0 = repairAppliedPolymorphicHead fApp0Raw aApp
                      containsInternalTyVar ty =
                        case ty of
                          TVarRef ref -> isInternalTypeBinderRef ref
                          TArrow dom cod -> containsInternalTyVar dom || containsInternalTyVar cod
                          TCon _ args -> any containsInternalTyVar args
                          TForallRef _ mb body ->
                            maybe False containsInternalBoundTy mb || containsInternalTyVar body
                          TMuRef _ body -> containsInternalTyVar body
                          _ -> False
                      containsInternalBoundTy bound =
                        case bound of
                          TArrow dom cod -> containsInternalTyVar dom || containsInternalTyVar cod
                          TCon _ args -> any containsInternalTyVar args
                          TForallRef _ mb body ->
                            maybe False containsInternalBoundTy mb || containsInternalTyVar body
                          TMuRef _ body -> containsInternalTyVar body
                          _ -> False
                      isIdentityLambdaBody param body =
                        case body of
                          EVarNode bodyVar -> localVarKeyMatchesReference (LocalVarResolved param) bodyVar
                          _ -> False
                   in case (fApp0, aSourceName, aAppTyChecked) of
                        (ELam resolved body, Just argName, Right argTy)
                          | containsInternalTyVar paramTy
                              && isIdentityLambdaBody resolved body
                              && hasContractiveRecursiveWitness argTy
                              && maybe False hasContractiveRecursiveWitness (lookupSchemeType argName env) ->
                              case typeCheckLocal (EApp fApp0 aApp) of
                                Left _ ->
                                  ELam
                                    (mapResolvedVarType (const argTy) resolved)
                                    (refreshLocalResolvedVarType (LocalVarResolved resolved) argTy body)
                                Right _ -> fApp0
                          where
                            paramTy = resolvedVarType resolved
                        _ -> fApp0
                bypassApp = transparentOrIdentityBypassTerm
            let fAppTyChecked = typeCheckLocal fApp
            (app0, mbApp0Ty) <-
              case bypassApp of
                Just bypassTerm -> Right (bypassTerm, Nothing)
                Nothing ->
                  insertMuUseSiteCoercions
                    tcEnv
                    (argIsExplicitRecursiveParam aAnn)
                    (isJust aSourceName)
                    argSourceSchemeTy
                    (either (const Nothing) Just fAppTyChecked)
                    (either (const Nothing) Just aAppTyChecked)
                    fApp
                    aApp
            let (app, appTyChecked) =
                  case maybe (typeCheckLocal app0) Right mbApp0Ty of
                    Right app0Ty ->
                      let appTyped = rollResultToExpectedMu tcEnv appTargetTy (TypedTerm app0 app0Ty)
                       in (ttTerm appTyped, Right (ttType appTyped))
                    Left tcErr -> (app0, Left tcErr)
            case ( ( \go ->
                       sourceAnnIsPolymorphic schemeEnv aAnn
                         && ( case funInst of
                                InstApp ty -> go ty
                                InstInside (InstBot ty) -> go ty
                                InstInside (InstApp ty) -> go ty
                                InstSeq (InstInside (InstBot ty)) InstElim -> go ty
                                InstSeq (InstInside (InstApp ty)) InstElim -> go ty
                                _ -> False
                                || case argInst of
                                  InstApp ty -> go ty
                                  InstInside (InstBot ty) -> go ty
                                  InstInside (InstApp ty) -> go ty
                                  InstSeq (InstInside (InstBot ty)) InstElim -> go ty
                                  InstSeq (InstInside (InstApp ty)) InstElim -> go ty
                                  _ -> False
                            )
                   )
                     ( let go ty =
                             case ty of
                               TVarRef ref -> isInternalTypeBinderRef ref
                               TArrow dom cod -> go dom && go cod
                               TForallRef _ _ body -> go body
                               _ -> False
                        in go
                     ),
                   appTyChecked
                 ) of
              (True, Left tcErr) ->
                if take (length "TCArgumentMismatch") (show tcErr) == "TCArgumentMismatch"
                  || take (length "TCExpectedArrow") (show tcErr) == "TCExpectedArrow"
                  then
                    Left
                      ( PhiTranslatabilityError
                          [ "AAppF: unresolved non-self polymorphic alias instantiation",
                            "function=" ++ show fSourceName,
                            "argument=" ++ show aSourceName,
                            "typeCheck=" ++ show tcErr
                          ]
                      )
                  else Right app
              _ -> Right app
       in mkOut f
    ALetF v _schemeGenId schemeRootId _ _rhsScopeGen (rhsAnn, rhsOut) (bodyAnn, bodyOut) trivialRoot ->
      let elaborateLet env = do
            let debugGeneralize = traceGeneralize (algTraceConfig algebraContext)
                transparentMediatorSourceName annExpr =
                  case sourceVarName annExpr of
                    Just sourceName -> Just sourceName
                    Nothing ->
                      case stripAnnExpr annExpr of
                        AApp funAnn argAnn _ _ _
                          | maybe False (`isTransparentMediatorVar` env) (sourceVarName funAnn) ->
                              transparentMediatorSourceName argAnn
                        _ -> Nothing
                peelTransparentMediatorSubject annExpr =
                  case stripAnnExpr annExpr of
                    AApp funAnn argAnn _ _ _
                      | maybe False (`isTransparentMediatorVar` env) (sourceVarName funAnn) ->
                          peelTransparentMediatorSubject argAnn
                    _ -> annExpr
                aliasSourceName = transparentMediatorSourceName rhsAnn
                aliasSourceSchemeInfo = aliasSourceName >>= (`lookupSchemeInfo` env)
                containsRecursiveSelfAppToParam selfName paramName annExpr =
                  case annExpr of
                    AVar _ _ -> False
                    ALit _ _ -> False
                    AApp (AVar recurName _) arg _ _ _
                      | recurName == selfName -> annContainsVar paramName arg
                    AApp fun arg _ _ _ ->
                      containsRecursiveSelfAppToParam selfName paramName fun
                        || containsRecursiveSelfAppToParam selfName paramName arg
                    ALam boundName _ _ body _
                      | boundName == selfName || boundName == paramName -> False
                      | otherwise -> containsRecursiveSelfAppToParam selfName paramName body
                    ALet boundName _ _ _ _ rhs body _
                      | boundName == selfName || boundName == paramName ->
                          containsRecursiveSelfAppToParam selfName paramName rhs
                      | otherwise ->
                          containsRecursiveSelfAppToParam selfName paramName rhs
                            || containsRecursiveSelfAppToParam selfName paramName body
                    AAnn inner _ _ -> containsRecursiveSelfAppToParam selfName paramName inner
                    AUnfold inner _ _ -> containsRecursiveSelfAppToParam selfName paramName inner
                hasNestedRecursiveSelfAppToParam selfName paramName annExpr =
                  case annExpr of
                    AVar _ _ -> False
                    ALit _ _ -> False
                    AApp fun arg _ _ _ ->
                      containsRecursiveSelfAppToParam selfName paramName arg
                        || hasNestedRecursiveSelfAppToParam selfName paramName fun
                        || hasNestedRecursiveSelfAppToParam selfName paramName arg
                    ALam boundName _ _ body _
                      | boundName == selfName || boundName == paramName -> False
                      | otherwise -> hasNestedRecursiveSelfAppToParam selfName paramName body
                    ALet boundName _ _ _ _ rhs body _
                      | boundName == selfName || boundName == paramName ->
                          hasNestedRecursiveSelfAppToParam selfName paramName rhs
                      | otherwise ->
                          hasNestedRecursiveSelfAppToParam selfName paramName rhs
                            || hasNestedRecursiveSelfAppToParam selfName paramName body
                    AAnn inner _ _ -> hasNestedRecursiveSelfAppToParam selfName paramName inner
                    AUnfold inner _ _ -> hasNestedRecursiveSelfAppToParam selfName paramName inner
                recursiveArrowCarrier extraUsedNames codTy =
                  let usedNames = Set.union extraUsedNames (freeTypeVarsType codTy)
                      pickFreshMuName idx =
                        let candidate =
                              if idx == (0 :: Int)
                                then "a"
                                else "a" ++ show idx
                         in if Set.member candidate usedNames
                              then pickFreshMuName (idx + 1)
                              else candidate
                      muName = pickFreshMuName 0
                      (muRef, _) = freshTypeBinderRef muName (identityGeneratorAfterType codTy)
                   in TMuRef muRef (TArrow (TVarRef muRef) codTy)
                recursiveFixedPointCarrier extraUsedNames =
                  let pickFreshMuName idx =
                        let candidate =
                              if idx == (0 :: Int)
                                then "a"
                                else "a" ++ show idx
                         in if Set.member candidate extraUsedNames
                              then pickFreshMuName (idx + 1)
                              else candidate
                      muName = pickFreshMuName 0
                      (muRef, _) = freshTypeBinderRef muName (envIdentityGenerator env)
                   in TMuRef muRef (TArrow (TVarRef muRef) (TVarRef muRef))
                previewRecursiveCarrierTy selfName annExpr =
                  case annExpr of
                    ALam lamParam _ _ lamBody _
                      | containsRecursiveSelfAppToParam selfName lamParam lamBody ->
                          do
                            resultTy <- either (const Nothing) Just (reifyNodeTypePreferringBound scopeContext (annNode lamBody))
                            pure $
                              if resultTy == TBottom && hasNestedRecursiveSelfAppToParam selfName lamParam lamBody
                                then recursiveFixedPointCarrier Set.empty
                                else recursiveArrowCarrier Set.empty resultTy
                    AAnn inner _ _ -> previewRecursiveCarrierTy selfName inner
                    AUnfold inner _ _ -> previewRecursiveCarrierTy selfName inner
                    _ -> Nothing
            letResultSourceScheme <- sourceSchemePairForNode algebraContext scopeContext (canonical trivialRoot)
            schemeRootSourceScheme <- sourceSchemePairForNode algebraContext scopeContext schemeRootId
            rhsOuterSourceScheme <- sourceSchemePairForOuterAnnotation algebraContext scopeContext rhsAnn
            explicitRhsSourceScheme <- sourceSchemePairForAnnotation algebraContext scopeContext rhsAnn
            let recoverGeneralizeAtNode err =
                  case err of
                    SchemeFreeVars _ _
                      | Just schemePair <- rhsOuterSourceScheme ->
                          Right schemePair
                      | Just schemePair <- letResultSourceScheme ->
                          Right schemePair
                      | Just schemePair <- schemeRootSourceScheme ->
                          Right schemePair
                      | Just schemePair <- explicitRhsSourceScheme ->
                          Right schemePair
                      | Just aliasInfo <- aliasSourceSchemeInfo ->
                          Right (siScheme aliasInfo, schemeInfoBinderRefSubst aliasInfo)
                      | Just carrierTy <- previewRecursiveCarrierTy v (peelTransparentMediatorSubject rhsAnn),
                        hasContractiveRecursiveWitness carrierTy ->
                          Right (schemeFromType carrierTy, IntMap.empty)
                    _ -> Left err
            _ <-
              pure $
                debugGeneralize
                  ( "elaborate let("
                      ++ v
                      ++ "): schemeRootId="
                      ++ show schemeRootId
                      ++ " scopeRoot="
                      ++ show (scopeRootForNode scopeContext schemeRootId)
                  )
                  ()
            (scheme0Raw, subst0Raw) <-
              case rhsOuterSourceScheme <|> explicitRhsSourceScheme <|> letResultSourceScheme <|> schemeRootSourceScheme of
                Just schemePair -> Right schemePair
                Nothing ->
                  case generalizeAtNode scopeContext schemeRootId of
                    Right (scheme, substRefs) -> Right (scheme, substRefs)
                    Left err -> recoverGeneralizeAtNode err
            let lambdaParamNodes annExpr =
                  case annExpr of
                    ALam _ paramNode _ body _ -> paramNode : lambdaParamNodes body
                    AAnn inner _ _ -> lambdaParamNodes inner
                    AUnfold inner _ _ -> lambdaParamNodes inner
                    _ -> []
                deriveLambdaBinderSubst scheme0 subst0' =
                  let (binds, _) = splitForallsRefs (schemeToType scheme0)
                      binderRefs = map fst (schemeBinderRefs scheme0)
                      binderNames = map typeBinderRefName binderRefs
                      binderBounds = map snd binds
                      paramNodes = lambdaParamNodes rhsAnn
                      binderPairs = zip binderRefs paramNodes
                      canAugment =
                        length binderNames == length paramNodes
                          && all (== Nothing) binderBounds
                   in if canAugment
                        then
                          foldl'
                            ( \acc (ref, paramNode) ->
                                let key = getNodeId (canonical paramNode)
                                 in IntMap.insertWith (\_ old -> old) key ref acc
                            )
                            subst0'
                            binderPairs
                        else subst0'
                (scheme0Norm, subst0Norm) = normalizeSchemeSubstPair (scheme0Raw, subst0Raw)
                scheme0Ty = schemeToType scheme0Norm
                schemeBase =
                  if schemeTypeHasExplicitBound scheme0Ty
                    then scheme0Norm
                    else schemeFromType (simplifyAnnotationType scheme0Ty)
            {- Note [Mu-type annotation override for let schemes]
               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               When a let-bound RHS is a lambda with a μ-type annotation on its
               parameter (e.g. let g = (λx:μα.α→Int. x) in …), the generalization
               may produce an overly-generic scheme (∀a.∀b. a→b) because the
               constraint graph's μ-node lives under the lambda scope and is not
               visible as a binder-bound at the let scope.

               We detect this case by inspecting the RHS annotation structure for
               a desugared annotated lambda whose annotation node reifies to a
               contractive TMu witness. When found, we override the scheme with a
               monomorphic function type that uses the witnessed μ-type as both
               domain and codomain (identity-like), or more precisely, domain =
               μ-type and codomain = μ-type when the body simply returns the
               parameter. -}
            scheme <-
              let firstNonContractiveMuAnnotation annExpr =
                    case annExpr of
                      ALam lamParam _ _ lamBody _ ->
                        case desugaredAnnLambdaInfo lamParam lamBody of
                          Just (annNodeId, _, _) ->
                            case reifyNodeTypePreferringBound scopeContext annNodeId of
                              Right annTy ->
                                firstNonContractiveRecursiveType annTy
                              _ -> Nothing
                          Nothing -> Nothing
                      AAnn inner _ _ -> firstNonContractiveMuAnnotation inner
                      AUnfold inner _ _ -> firstNonContractiveMuAnnotation inner
                      _ -> Nothing
                  muAnnotationTy annExpr =
                    case annExpr of
                      ALam lamParam _ _ lamBody _ ->
                        case desugaredAnnLambdaInfo lamParam lamBody of
                          Just (annNodeId, _, _) ->
                            case reifyNodeTypePreferringBound scopeContext annNodeId of
                              Right annTy@TMuRef {}
                                | hasContractiveRecursiveWitness annTy -> Just annTy
                              _ -> Nothing
                          Nothing -> Nothing
                      AAnn inner _ _ -> muAnnotationTy inner
                      AUnfold inner _ _ -> muAnnotationTy inner
                      _ -> Nothing
                  muAnnotatedIdentityBody annExpr =
                    case annExpr of
                      ALam lamParam _ _ lamBody _ ->
                        case desugaredAnnLambdaInfo lamParam lamBody of
                          Just (_, _, innerBodyAnn) -> sourceVarName innerBodyAnn == Just lamParam
                          Nothing -> False
                      AAnn inner _ _ -> muAnnotatedIdentityBody inner
                      AUnfold inner _ _ -> muAnnotatedIdentityBody inner
                      _ -> False
                  overrideMuAnnotatedCodomain muTy =
                    let stripForalls ty =
                          case ty of
                            TForallRef _ _ inner -> stripForalls inner
                            other -> other
                        strippedSchemeBody = stripForalls (schemeToType schemeBase)
                        quantRefs = map fst (fst (splitForallsRefs (schemeToType schemeBase)))
                        isUnquantifiedTVar (TVarRef ref) =
                          not (any (typeBinderRefsSameIdentity ref) quantRefs)
                        isUnquantifiedTVar _ = False
                     in case strippedSchemeBody of
                          TArrow _dom cod
                            | isUnquantifiedTVar cod ->
                                -- Codomain is an unquantified internal variable:
                                -- override both domain and codomain to μ.
                                schemeFromType (TArrow muTy muTy)
                          _ -> schemeBase
                  recursiveCarrierTyFor selfName extraUsedNames annExpr =
                    let inferredCarrier = inferredRecursiveCarrierTyFor selfName extraUsedNames annExpr
                     in case (reifyNodeTypePreferringBound scopeContext (annNode annExpr), inferredCarrier) of
                          (Right carrierTy, Just inferredTy)
                            | hasContractiveRecursiveWitness carrierTy,
                              shouldPreferInferredRecursiveCarrier carrierTy inferredTy ->
                                Just inferredTy
                          (Right carrierTy, _)
                            | hasContractiveRecursiveWitness carrierTy -> Just carrierTy
                          (_, Just inferredTy) -> Just inferredTy
                          _ -> Nothing
                  inferredRecursiveCarrierTyFor selfName extraUsedNames annExpr =
                    case annExpr of
                      ALam lamParam _ _ lamBody _ ->
                        if containsRecursiveSelfAppToParam selfName lamParam lamBody
                          then do
                            resultTy <- either (const Nothing) Just (reifyNodeTypePreferringBound scopeContext (annNode lamBody))
                            pure $
                              if resultTy == TBottom && hasNestedRecursiveSelfAppToParam selfName lamParam lamBody
                                then recursiveFixedPointCarrier extraUsedNames
                                else recursiveArrowCarrier extraUsedNames resultTy
                          else Nothing
                      AAnn inner _ _ -> inferredRecursiveCarrierTyFor selfName extraUsedNames inner
                      AUnfold inner _ _ -> inferredRecursiveCarrierTyFor selfName extraUsedNames inner
                      _ -> Nothing
                  shouldPreferInferredRecursiveCarrier carrierTy inferredTy =
                    (isBottomRecursiveCarrier carrierTy && isFixedPointRecursiveCarrier inferredTy)
                      || hasInternalRecursiveCodomain carrierTy && not (hasInternalRecursiveCodomain inferredTy)
                  isBottomRecursiveCarrier carrierTy =
                    case carrierTy of
                      TMuRef _ (TArrow _ TBottom) -> True
                      _ -> False
                  isFixedPointRecursiveCarrier carrierTy =
                    case carrierTy of
                      TMuRef muRef (TArrow (TVarRef domRef) (TVarRef codRef)) ->
                        typeBinderRefsSameIdentity muRef domRef && typeBinderRefsSameIdentity muRef codRef
                      _ -> False
                  hasInternalRecursiveCodomain carrierTy =
                    case carrierTy of
                      TMuRef muRef (TArrow (TVarRef domRef) codTy) ->
                        typeBinderRefsSameIdentity muRef domRef && internalOnlyType codTy
                      _ -> False
                  internalOnlyType ty =
                    case ty of
                      TVarRef ref -> isInternalTypeBinderRef ref
                      TArrow dom cod -> internalOnlyType dom && internalOnlyType cod
                      TCon _ args -> not (null args) && all internalOnlyType args
                      TForallRef _ mb body ->
                        maybe True internalOnlyBound mb && internalOnlyType body
                      TMuRef _ body -> internalOnlyType body
                      _ -> False
                  internalOnlyBound bound =
                    case bound of
                      TArrow dom cod -> internalOnlyType dom && internalOnlyType cod
                      TCon _ args -> not (null args) && all internalOnlyType args
                      TForallRef _ mb body ->
                        maybe True internalOnlyBound mb && internalOnlyType body
                      TMuRef _ body -> internalOnlyType body
                      _ -> False
                  returnedRecursiveHelperArrowTy annExpr =
                    case annExpr of
                      ALam _ _ _ lamBody _ -> do
                        (outerDomTy, helperTy) <- returnedRecursiveHelperSignature lamBody
                        pure (TArrow outerDomTy helperTy)
                      AAnn inner _ _ -> returnedRecursiveHelperArrowTy inner
                      AUnfold inner _ _ -> returnedRecursiveHelperArrowTy inner
                      _ -> Nothing
                  returnedRecursiveHelperSignature lamBody =
                    case lamBody of
                      ALet helperName _ _ _ _ helperRhs@(ALam helperParam _ _ _ _) helperBody _
                        | sourceVarName helperBody == Just helperName -> do
                            helperTy <- recursiveCarrierTyFor helperName Set.empty helperRhs
                            outerDomTy <- recursiveCallArgumentTyFor v (Just (helperName, helperParam, helperTy)) helperRhs
                            pure (outerDomTy, helperTy)
                      ALet helperName _ _ _ _ helperRhs helperBody _
                        | sourceVarName helperBody == Just helperName -> do
                            helperTy <- recursiveCarrierTyFor helperName Set.empty helperRhs
                            outerDomTy <- recursiveCallArgumentTyFor v Nothing helperRhs
                            pure (outerDomTy, helperTy)
                      AAnn inner _ _ -> returnedRecursiveHelperSignature inner
                      AUnfold inner _ _ -> returnedRecursiveHelperSignature inner
                      _ -> Nothing
                  recursiveCallArgumentTyFor selfName mbHelper annExpr =
                    case annExpr of
                      AApp (AVar recurName _) arg _ _ _
                        | recurName == selfName ->
                            case helperRecursiveSelfAppResultTy mbHelper arg of
                              Just argTy -> Just argTy
                              Nothing -> either (const Nothing) Just (reifyNodeTypePreferringBound scopeContext (annNode arg))
                      AApp fun arg _ _ _ ->
                        case recursiveCallArgumentTyFor selfName mbHelper fun of
                          Just argTy -> Just argTy
                          Nothing -> recursiveCallArgumentTyFor selfName mbHelper arg
                      ALam boundName _ _ body _
                        | boundName == selfName -> Nothing
                        | otherwise -> recursiveCallArgumentTyFor selfName mbHelper body
                      ALet boundName _ _ _ _ rhs body _
                        | boundName == selfName ->
                            recursiveCallArgumentTyFor selfName mbHelper rhs
                        | otherwise ->
                            case recursiveCallArgumentTyFor selfName mbHelper rhs of
                              Just argTy -> Just argTy
                              Nothing -> recursiveCallArgumentTyFor selfName mbHelper body
                      AAnn inner _ _ -> recursiveCallArgumentTyFor selfName mbHelper inner
                      AUnfold inner _ _ -> recursiveCallArgumentTyFor selfName mbHelper inner
                      _ -> Nothing
                  helperRecursiveSelfAppResultTy mbHelper argExpr =
                    case mbHelper of
                      Just (helperName, helperParam, helperTy)
                        | isRecursiveSelfAppToParam helperName helperParam argExpr ->
                            recursiveSelfAppResultTy helperTy
                      _ -> Nothing
                  isRecursiveSelfAppToParam helperName helperParam annExpr =
                    case annExpr of
                      AApp (AVar recurName _) arg _ _ _
                        | recurName == helperName -> annContainsVar helperParam arg
                      AAnn inner _ _ -> isRecursiveSelfAppToParam helperName helperParam inner
                      AUnfold inner _ _ -> isRecursiveSelfAppToParam helperName helperParam inner
                      _ -> False
                  recursiveSelfAppResultTy helperTy =
                    case helperTy of
                      TForallRef _ _ bodyTy -> recursiveSelfAppResultTy bodyTy
                      TArrow _ codTy -> Just codTy
                      muTy@TMuRef {} ->
                        case unfoldMuOnce muTy of
                          Just (TArrow _ codTy) -> Just codTy
                          _ -> Nothing
                      _ -> Nothing
                  mediatedMuSubject = peelTransparentMediatorSubject rhsAnn
                  recursiveCarrierPreview = recursiveCarrierTyFor v Set.empty mediatedMuSubject
                  structuralRecursiveCandidateSelection =
                    selectStructuralRecursiveCandidate $
                      maybe [] (pure . StructuralRecursiveCandidateFromHelper) (returnedRecursiveHelperArrowTy mediatedMuSubject)
                        ++ maybe [] (pure . StructuralRecursiveCandidateFromDirectCarrier) recursiveCarrierPreview
               in case aliasSourceSchemeInfo of
                    Just aliasInfo ->
                      pure (siScheme aliasInfo)
                    Nothing ->
                      case (structuralRecursiveCandidateSelection, annContainsVar v rhsAnn, blockedAliasMuType (schemeToType schemeBase)) of
                        (UniqueStructuralRecursiveCandidate (StructuralRecursiveCandidateFromHelper candidateTy), True, _)
                          | not (hasContractiveRecursiveWitness (schemeToType schemeBase)) ->
                              pure (schemeFromType candidateTy)
                        (UniqueStructuralRecursiveCandidate (StructuralRecursiveCandidateFromDirectCarrier candidateTy), True, Just muTy)
                          | shouldPreferInferredRecursiveCarrier muTy candidateTy ->
                              pure (schemeFromType candidateTy)
                        (UniqueStructuralRecursiveCandidate (StructuralRecursiveCandidateFromDirectCarrier candidateTy), True, Nothing)
                          | not (hasContractiveRecursiveWitness (schemeToType schemeBase)) ->
                              pure (schemeFromType candidateTy)
                        (AmbiguousStructuralRecursiveCandidate, True, _) ->
                          pure schemeBase
                        (NoStructuralRecursiveCandidate, True, Just muTy) ->
                          pure (schemeFromType muTy)
                        _ ->
                          case firstNonContractiveMuAnnotation mediatedMuSubject of
                            Just badTy ->
                              Left (InstantiationError ("non-contractive recursive annotation: " ++ show badTy))
                            Nothing ->
                              pure $
                                case muAnnotationTy mediatedMuSubject of
                                  Just muTy ->
                                    {- Note [Selective codomain override for μ-annotated lambdas]
                                       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                       The domain is always overridden to the μ-type since
                                       the surrounding μ-annotation detection confirms that the lambda parameter has
                                       an explicit contractive μ-annotation (e.g. λx:μα.α→Int. x).

                                       For the codomain: when the scheme is fully polymorphic
                                       (e.g. ∀a.∀b. a→b with both vars quantified), generalization
                                       captured the correct parametricity and downstream elaboration
                                       handles the μ-type through normal instantiation — so we leave
                                       schemeBase intact. When the codomain is a constraint-internal
                                       variable (e.g. TVar "t10" that wasn't quantified), generalization
                                       lost track of its relationship to the μ-annotated parameter,
                                       and we override it to the μ-type. -}
                                    if muAnnotatedIdentityBody mediatedMuSubject
                                      then schemeFromType (TArrow muTy muTy)
                                      else overrideMuAnnotatedCodomain muTy
                                  Nothing ->
                                    case recursiveCarrierTyFor v Set.empty mediatedMuSubject of
                                      Just carrierTy
                                        | annContainsVar v rhsAnn,
                                          not (hasContractiveRecursiveWitness (schemeToType schemeBase)) ->
                                            schemeFromType carrierTy
                                      Nothing
                                        | not (hasContractiveRecursiveWitness (schemeToType schemeBase)) ->
                                            schemeBase
                                      _ -> schemeBase
            let subst0 = normalizeSubstForScheme scheme (deriveLambdaBinderSubst scheme0Norm subst0Norm)
                subst =
                  case aliasSourceSchemeInfo of
                    Just aliasInfo -> schemeInfoBinderRefSubst aliasInfo
                    Nothing ->
                      let (binds, _) = splitForallsRefs (schemeToType scheme)
                       in if null binds then IntMap.empty else subst0
                schemeInfo =
                  freshenSchemeInfoAgainstEnv
                    env
                    (schemeInfoFromRefSubst scheme subst)
                transparentMediator =
                  isTransparentMediatorAnn rhsAnn
                    || maybe False (`isTransparentMediatorVar` env) aliasSourceName
                envBindingFor bindingSchemeInfo =
                  case aliasSourceName of
                    Just sourceName ->
                      (mkLocalEnvBinding v schemeRootId bindingSchemeInfo transparentMediator)
                        { ebAliasTarget = Just (resolveAliasVar env sourceName)
                        }
                    Nothing -> mkLocalEnvBinding v schemeRootId bindingSchemeInfo transparentMediator
                tcEnvBase = typeCheckEnvFrom env
                typeCheckBase = TypeCheck.typeCheckWithEnv tcEnvBase
                authoritativeSourceSchemeInfo =
                  case rhsOuterSourceScheme <|> explicitRhsSourceScheme <|> letResultSourceScheme <|> schemeRootSourceScheme of
                    Just (schemeSrc, substSrc) ->
                      Just
                        ( freshenSchemeInfoAgainstEnv
                            env
                            (schemeInfoFromRefSubst schemeSrc substSrc)
                        )
                    Nothing -> Nothing
                envSchemeInfoForRhs = fromMaybe schemeInfo authoritativeSourceSchemeInfo
                env' = insertEnvBinding v (envBindingFor envSchemeInfoForRhs) env
                tcEnv = typeCheckEnvFrom env'
                typeCheckLet = TypeCheck.typeCheckWithEnv tcEnv
            rhs' <- elabTerm rhsOut env'
            let closeFreeVarsToScheme ty =
                  let (binds, body) = splitForallsRefs ty
                      boundRefs = map fst binds
                      extraBinds =
                        [ (ref, Nothing)
                          | ref <- freeTypeVarRefsInOccurrenceOrder body,
                            not (any (typeBinderRefsSameIdentity ref) boundRefs)
                        ]
                   in mkElabSchemeWithRefs (binds ++ extraBinds) body
                splitArrowN n ty
                  | n <= (0 :: Int) = Just ([], ty)
                  | otherwise =
                      case ty of
                        TArrow dom cod -> do
                          (doms, resultTy) <- splitArrowN (n - 1) cod
                          pure (dom : doms, resultTy)
                        _ -> Nothing
                collectLeadingLambdaParams term =
                  case term of
                    ELam resolved body ->
                      let (params, core) = collectLeadingLambdaParams body
                       in (resolved : params, core)
                    ELet _ sch (EVarNode _) body
                      | null (schemeBinderRefs sch) ->
                          collectLeadingLambdaParams body
                    _ -> ([], term)
                collapsedIdentityWrapperScheme ty =
                  case splitForallsRefs ty of
                    (_, TArrow _ codTy) ->
                      codTy == TBottom || containsInternalTypeVar codTy
                    _ -> False
                rebuildTransparentMediatorTerm rootName etaParams resultTy =
                  let rootParamTy = foldr TArrow resultTy (map resolvedVarType etaParams)
                      (rootRef, _) =
                        freshLocalRef
                          rootName
                          ( identityGeneratorAfter $
                              generatedIdentitiesInType rootParamTy
                                ++ concatMap (generatedIdentitiesInTerm . EVarNode) etaParams
                          )
                      rootResolved = localResolvedVarFromRef rootRef rootParamTy
                      mediatorBody =
                        foldr
                          ELam
                          (foldl' EApp (EVarNode rootResolved) (map EVarNode etaParams))
                          etaParams
                   in (rootParamTy, ELam rootResolved mediatorBody)
                rhsAliasTerm = stripUnusedTopTyAbsWithEnv tcEnvBase rhs'
                rhsTransparentMediatorTerm = stripLeadingTyAbs rhsAliasTerm
                identityWrapperMediatorExpr aliases expr =
                  case stripAnnExpr expr of
                    AVar name _ ->
                      case Map.lookup name aliases of
                        Just IdentityWrapperMediator -> True
                        Just IdentityWrapperRoot -> False
                        Nothing ->
                          let resolved = resolveAliasVar env name
                           in isTransparentMediatorVar resolved env
                                || maybe False isSingleBinderIdentityScheme (lookupSchemeInfo resolved env)
                    ALam param _ _ body _ -> identityWrapperBody param Map.empty body
                    ALet boundName _ _ _ _ rhs body _
                      | boundName `Map.member` aliases -> False
                      | Just origin <- identityWrapperAliasOrigin boundName aliases rhs ->
                          identityWrapperMediatorExpr (Map.insert boundName origin aliases) body
                      | otherwise -> False
                    _ -> False
                identityWrapperHead root aliases expr =
                  case stripAnnExpr expr of
                    AVar name _ ->
                      case Map.lookup name aliases of
                        Just IdentityWrapperMediator -> True
                        Just IdentityWrapperRoot -> False
                        Nothing
                          | name == root -> False
                          | otherwise ->
                              let resolved = resolveAliasVar env name
                               in isTransparentMediatorVar resolved env
                                    || maybe False isSingleBinderIdentityScheme (lookupSchemeInfo resolved env)
                    _ -> False
                identityWrapperExpr root aliases expr =
                  case stripAnnExpr expr of
                    AVar name _ ->
                      case Map.lookup name aliases of
                        Just IdentityWrapperRoot -> True
                        Just IdentityWrapperMediator -> False
                        Nothing -> name == root
                    ALet boundName _ _ _ _ rhs body _
                      | boundName == root
                          || Map.member boundName aliases ->
                          False
                      | Just origin <- identityWrapperAliasOrigin root aliases rhs ->
                          identityWrapperExpr root (Map.insert boundName origin aliases) body
                      | otherwise ->
                          False
                    other ->
                      let (funExpr, argExprs) = annAppSpine other
                       in case argExprs of
                            [argExpr] ->
                              identityWrapperHead root aliases funExpr
                                && identityWrapperExpr root aliases argExpr
                            _ -> False
                identityWrapperAliasOrigin root aliases rhsExpr =
                  if identityWrapperExpr root aliases rhsExpr
                    then Just IdentityWrapperRoot
                    else
                      if identityWrapperMediatorExpr aliases rhsExpr
                        then Just IdentityWrapperMediator
                        else Nothing
                identityWrapperBody root aliases expr =
                  identityWrapperExpr root aliases expr
                schemeNeedsStructuralRecovery schemeTy =
                  not (schemeTypeHasExplicitBound schemeTy)
                    && (containsInternalTypeVar schemeTy || schemeHasForwardBoundReference schemeTy)
                rhsTransparentMediatorOverride =
                  if transparentMediator
                    then case rhsTransparentMediatorTerm of
                      ELam rootResolved body ->
                        let rootParamTy = resolvedVarType rootResolved
                            (etaParams, _core) = collectLeadingLambdaParams body
                            etaParamTys = map resolvedVarType etaParams
                         in case splitArrowN (length etaParams) rootParamTy of
                              Just (_expectedEtaParamTys, resultTy)
                                | not (null etaParams),
                                  let (structuralRootParamTy, structuralMediatorTerm) =
                                        rebuildTransparentMediatorTerm v etaParams resultTy,
                                  let rhsScheme =
                                        closeFreeVarsToScheme
                                          (TArrow structuralRootParamTy (foldr TArrow resultTy etaParamTys)),
                                  let candidateSubst =
                                        case splitForallsRefs (schemeToType rhsScheme) of
                                          ([], _) -> IntMap.empty
                                          _ -> normalizeSubstForScheme rhsScheme subst,
                                  let candidateSchemeInfo =
                                        schemeInfoFromRefSubst rhsScheme candidateSubst,
                                  let rhsClosed =
                                        closeTermWithSchemeSubstRefsIfNeeded
                                          (schemeInfoBinderRefSubst candidateSchemeInfo)
                                          (siScheme candidateSchemeInfo)
                                          structuralMediatorTerm,
                                  let candidateSchemeAdmitsRhs =
                                        case typeCheckBase rhsClosed of
                                          Right rhsTy -> alphaEqType rhsTy (schemeToType rhsScheme)
                                          Left _ -> False,
                                  candidateSchemeAdmitsRhs
                                    || containsInternalTypeVar (schemeToType scheme)
                                    || schemeHasForwardBoundReference (schemeToType scheme)
                                    || not (alphaEqType (schemeToType scheme) (schemeToType rhsScheme)) ->
                                    Just
                                      ( structuralMediatorTerm,
                                        candidateSchemeInfo
                                      )
                              _ -> Nothing
                      _ -> Nothing
                    else Nothing
                rhsIdentityWrapperOverride =
                  case (stripAnnExpr rhsAnn, rhsTransparentMediatorTerm) of
                    (ALam rootParam _ _ body _, ELam rootResolved _)
                      | let rootName = resolvedVarReferenceName rootResolved,
                        rootParam == rootName,
                        identityWrapperBody rootParam Map.empty body ->
                          let rootParamTy = resolvedVarType rootResolved
                              rootResolved' = mapResolvedVarType (const rootParamTy) rootResolved
                              rhsTerm = ELam rootResolved' (EVarNode rootResolved')
                              rhsScheme = closeFreeVarsToScheme (TArrow rootParamTy rootParamTy)
                              candidateSubst =
                                case splitForallsRefs (schemeToType rhsScheme) of
                                  ([], _) -> IntMap.empty
                                  _ -> normalizeSubstForScheme rhsScheme subst
                              candidateSchemeInfo =
                                schemeInfoFromRefSubst rhsScheme candidateSubst
                              rhsClosed =
                                closeTermWithSchemeSubstRefsIfNeeded
                                  (schemeInfoBinderRefSubst candidateSchemeInfo)
                                  (siScheme candidateSchemeInfo)
                                  rhsTerm
                              candidateSchemeAdmitsRhs =
                                case typeCheckBase rhsClosed of
                                  Right rhsTy -> alphaEqType rhsTy (schemeToType rhsScheme)
                                  Left _ -> False
                              generalizedSchemeTy = schemeToType scheme
                              generalizedSchemeNeedsRecovery =
                                schemeNeedsStructuralRecovery generalizedSchemeTy
                                  || collapsedIdentityWrapperScheme generalizedSchemeTy
                           in if candidateSchemeAdmitsRhs && generalizedSchemeNeedsRecovery
                                then
                                  Just
                                    ( rhsTerm,
                                      candidateSchemeInfo
                                    )
                                else Nothing
                    _ -> Nothing
                rhsAliasOverride =
                  case (rhsAliasTerm, rhsAliasTy) of
                    (EVarNode _, Right rhsTy)
                      | not (alphaEqType rhsTy (schemeToType scheme)) ->
                          let rhsScheme = closeFreeVarsToScheme rhsTy
                              rhsSubst =
                                case splitForallsRefs rhsTy of
                                  ([], _) -> IntMap.empty
                                  _ -> subst
                           in Just (rhsAliasTerm, schemeInfoFromRefSubst rhsScheme rhsSubst)
                    _ -> Nothing
                effectiveRhsOverride =
                  case rhsTransparentMediatorOverride of
                    Just overrideInfo -> Just overrideInfo
                    Nothing ->
                      case rhsIdentityWrapperOverride of
                        Just overrideInfo -> Just overrideInfo
                        Nothing -> rhsAliasOverride
                effectiveSchemeInfo =
                  freshenSchemeInfoAgainstEnv
                    env
                    ( case effectiveRhsOverride of
                        Just (_, overrideInfo) -> overrideInfo
                        Nothing -> schemeInfo
                    )
                effectiveRhsTerm =
                  case effectiveRhsOverride of
                    Just (overrideTerm, _) -> overrideTerm
                    Nothing -> rhs'
                effectiveRhsTy = typeCheckLet effectiveRhsTerm
                authoritativeEnvSchemeInfo =
                  fromMaybe
                    (freshenSchemeInfoAgainstEnv env schemeInfo)
                    authoritativeSourceSchemeInfo
                effectiveScheme = siScheme effectiveSchemeInfo
                effectiveSubstRefs = schemeInfoBinderRefSubst effectiveSchemeInfo
                effectiveSubst = effectiveSubstRefs
                envSchemeInfoForBody =
                  if isJust rhsOuterSourceScheme
                    || isJust explicitRhsSourceScheme
                    || isJust letResultSourceScheme
                    || isJust schemeRootSourceScheme
                    then authoritativeEnvSchemeInfo
                    else effectiveSchemeInfo
                envForBody = insertEnvBinding v (envBindingFor envSchemeInfoForBody) env
                tcEnvForBody = typeCheckEnvFrom envForBody
                typeCheckForBody = TypeCheck.typeCheckWithEnv tcEnvForBody
                rhsAliasTy = typeCheckBase rhsAliasTerm
            let rhsAbs0 =
                  let schemeTy = schemeToType effectiveScheme
                      rhsMatchesScheme rhsTy =
                        alphaEqType rhsTy schemeTy
                          || case schemeTy of
                            muTy@(TMuRef muRef muBody) ->
                              let expectedBodyTy = substTypeCaptureRef muRef muTy muBody
                               in alphaEqType rhsTy expectedBodyTy
                            _ -> False
                   in case (effectiveRhsTerm, effectiveRhsTy) of
                        (EVarNode _, _) ->
                          closeTermWithSchemeSubstRefsIfNeeded effectiveSubstRefs effectiveScheme effectiveRhsTerm
                        (_, Right rhsTy)
                          | rhsMatchesScheme rhsTy -> effectiveRhsTerm
                        _ -> closeTermWithSchemeSubstRefsIfNeeded effectiveSubstRefs effectiveScheme effectiveRhsTerm
                rhsAbs =
                  let schemeTy = schemeToType effectiveScheme
                      rhsAbs0Ty = typeCheckLet rhsAbs0
                      rhsAbsBase =
                        if not (null (schemeBinderRefs effectiveScheme))
                          then
                            case rhsAbs0Ty of
                              Right rhsTy
                                | alphaEqType rhsTy schemeTy -> rhsAbs0
                              _ ->
                                case case (rhsAbs0, rhsAbs0Ty) of
                                  (ETyAbsRef _ _ body, Right (TForallRef _ _ bodyTy))
                                    | alphaEqType bodyTy schemeTy ->
                                        body
                                  _ -> stripUnusedTopTyAbsWithEnv tcEnv rhsAbs0 of
                                  rhsAbsCandidate ->
                                    case typeCheckLet rhsAbsCandidate of
                                      Right rhsTy
                                        | alphaEqType rhsTy schemeTy ->
                                            rhsAbsCandidate
                                      _ ->
                                        case rhsAbs0Ty of
                                          Left _ -> rhsAbsCandidate
                                          _ -> rhsAbs0
                          else
                            case (rhsAbs0, rhsAbs0Ty) of
                              (ETyAbsRef _ _ body, Right (TForallRef _ _ bodyTy))
                                | alphaEqType bodyTy schemeTy ->
                                    body
                              _ -> stripUnusedTopTyAbsWithEnv tcEnv rhsAbs0
                      rhsAbsAligned =
                        let withTyAbs = addMissingLeadingTyAbsAlongType tcEnv schemeTy rhsAbsBase
                            aligned = alignLeadingLambdasToType schemeTy withTyAbs
                         in if localBinderIsDiscard v schemeRootId
                              then
                                let stripped = stripUnusedTopTyAbsWithEnv tcEnv aligned
                                 in case typeCheckLet stripped of
                                      Right _ -> stripped
                                      Left _ -> aligned
                              else aligned
                      rhsAbsBaseTy = typeCheckLet rhsAbsBase
                      rhsAbsAlignedTy = typeCheckLet rhsAbsAligned
                   in case rhsAbsBaseTy of
                        Right rhsTy
                          | alphaEqType rhsTy schemeTy -> rhsAbsBase
                        _ ->
                          case rhsAbsAlignedTy of
                            Right rhsTy
                              | alphaEqType rhsTy schemeTy -> rhsAbsAligned
                            _ -> rhsAbsBase
                rhsAbsTyChecked = typeCheckLet rhsAbs
            case debugGeneralize
              ( "elaborate let("
                  ++ v
                  ++ "): scheme="
                  ++ show effectiveScheme
                  ++ " subst="
                  ++ show effectiveSubst
                  ++ " rhsAbs="
                  ++ show rhsAbs
                  ++ " rhsAbsTy="
                  ++ show rhsAbsTyChecked
              )
              () of
              () -> pure ()
            let effectiveRhsTyForBody = typeCheckForBody effectiveRhsTerm
                rhsAbsTyForBody = typeCheckForBody rhsAbs
                rhsForRoll =
                  case schemeToType effectiveScheme of
                    muTy@(TMuRef muRef muBody) ->
                      let expectedBodyTy = substTypeCaptureRef muRef muTy muBody
                          rhsRollAligned = alignLeadingLambdasToType expectedBodyTy rhsAbs
                          rhsRollAlignedTy = typeCheckForBody rhsRollAligned
                       in case effectiveRhsTyForBody of
                            Right rhsTy
                              | alphaEqType rhsTy expectedBodyTy -> effectiveRhsTerm
                            _ ->
                              case rhsAbsTyForBody of
                                Right rhsTy
                                  | alphaEqType rhsTy expectedBodyTy -> rhsAbs
                                _ ->
                                  case rhsRollAlignedTy of
                                    Right rhsTy
                                      | alphaEqType rhsTy expectedBodyTy -> rhsRollAligned
                                    _ -> rhsAbs
                    _ -> rhsAbs
            let bodyElab =
                  case bodyAnn of
                    AAnn _ target _ | canonical target == canonical trivialRoot -> elabStripped bodyOut
                    AUnfold _ target _ | canonical target == canonical trivialRoot -> elabStripped bodyOut
                    _ -> elabTerm bodyOut
            body' <-
              bodyElab
                ( case rhsAliasOverride of
                    Just (_, aliasInfo) -> insertEnvBinding v (envBindingFor aliasInfo) env
                    Nothing ->
                      insertEnvBinding
                        v
                        ( envBindingFor
                            ( case (effectiveRhsTerm, effectiveRhsTy) of
                                (EVarNode _, Right rhsTy)
                                  | not (alphaEqType rhsTy (schemeToType effectiveScheme)) ->
                                      schemeInfoFromRefSubst (schemeFromType rhsTy) effectiveSubst
                                _ -> effectiveSchemeInfo
                            )
                        )
                        env
                )
            let rhsFinal =
                  case rhsAliasOverride of
                    Just (aliasTerm, _) -> aliasTerm
                    Nothing ->
                      case schemeToType effectiveScheme of
                        muTy@TMuRef {} ->
                          case effectiveRhsTyForBody of
                            Right rhsTy
                              | alphaEqType rhsTy muTy -> effectiveRhsTerm
                            _ ->
                              case rhsAbsTyForBody of
                                Right rhsTy
                                  | alphaEqType rhsTy muTy -> rhsAbs
                                _ -> ERoll muTy rhsForRoll
                        _ -> rhsAbs
                rhsFinalTy = typeCheckForBody rhsFinal
            let schemeFinal =
                  case rhsFinalTy of
                    Right rhsTy
                      | localBinderIsDiscard v schemeRootId ->
                          schemeFromType rhsTy
                      | sourceVarName bodyAnn == Just v,
                        lambdaAnn rhsAnn,
                        isNothing rhsOuterSourceScheme,
                        isNothing explicitRhsSourceScheme,
                        isNothing letResultSourceScheme,
                        isNothing schemeRootSourceScheme,
                        not (alphaEqType rhsTy (schemeToType effectiveScheme)) ->
                          schemeFromType rhsTy
                    _ ->
                      case rhsAliasOverride of
                        Just (_, aliasInfo) -> siScheme aliasInfo
                        Nothing -> effectiveScheme
                finalTy = schemeToType schemeFinal
                finalResolved = resolvedLocalBinderFromNode v schemeRootId finalTy
                rhsFinal' = refreshLocalResolvedVarType (LocalVarResolved finalResolved) finalTy rhsFinal
                body'' = refreshLocalResolvedVarType (LocalVarResolved finalResolved) finalTy body'
            pure (schemeFinal, rhsFinal', body'')
          unusedIdentityWrapperBinding =
            not (annContainsVar v bodyAnn) && identityWrapperAnn rhsAnn
          f env =
            if unusedIdentityWrapperBinding
              then elabTerm bodyOut env
              else do
                (scheme, rhsFinal, body') <- elaborateLet env
                let finalResolved = resolvedLocalBinderFromNode v schemeRootId (schemeToType scheme)
                if isJust (sourceVarName rhsAnn) && not (containsFreeVar (LocalVarResolved finalResolved) body')
                  then pure body'
                  else pure (mkLocalLetFromNode v schemeRootId scheme rhsFinal body')
          fStripped env =
            if unusedIdentityWrapperBinding
              then elabTerm bodyOut env
              else do
                (scheme, rhsFinal, body') <- elaborateLet env
                let finalResolved = resolvedLocalBinderFromNode v schemeRootId (schemeToType scheme)
                if isJust (sourceVarName rhsAnn) && not (containsFreeVar (LocalVarResolved finalResolved) body')
                  then pure body'
                  else
                    if containsFreeVar (LocalVarResolved finalResolved) rhsFinal
                      then pure (mkLocalLetFromNode v schemeRootId scheme rhsFinal body')
                      else pure body'
       in ElabOut
            { elabTerm = f,
              elabStripped = fStripped
            }
    AAnnF (exprAnn, exprOut) annNodeId eid ->
      ElabOut
        { elabTerm = \env -> do
            expr' <- elabTerm exprOut env
            elaborateAnnotationTerm annotationContext namedSetReify (envSchemeInfos env) (typeCheckEnvFrom env) exprAnn annNodeId eid expr',
          elabStripped = \env -> elabTerm exprOut env
        }
    AUnfoldF (_exprAnn, exprOut) _unfoldNodeId _eid ->
      ElabOut
        { elabTerm = \env -> do
            expr' <- elabTerm exprOut env
            pure (EUnroll expr'),
          elabStripped = \env -> do
            expr' <- elabTerm exprOut env
            pure (EUnroll expr')
        }
  where
    annotationContext = algAnnotationContext algebraContext
    scopeContext = acScopeContext annotationContext
    canonical = algCanonical algebraContext
    namedSetReify = algNamedSetReify algebraContext
    inlineBoundVarsContext = algInlineBoundVarsContext algebraContext

    inferInstAppArgs scheme targetTy =
      inferInstAppArgsFromSchemeRefs (schemeBinderRefs scheme) (schemeBody scheme) targetTy

    inferFullSpineInstFromArgTypes scheme argTys =
      case matchDomains Map.empty (schemeBody scheme) argTys of
        Nothing -> Nothing
        Just subst ->
          instFromSubst subst
      where
        binderRefs = map fst (schemeBinderRefs scheme)

        matchDomains subst bodyTy args =
          case (bodyTy, args) of
            (_, []) -> Just subst
            (TArrow bodyDom bodyCod, argTy : rest) -> do
              domainSubst <-
                case matchTypeRefs binderRefs bodyDom argTy of
                  Left _ -> Nothing
                  Right matched -> Just matched
              subst' <- mergeSubst subst domainSubst
              matchDomains subst' bodyCod rest
            _ -> Nothing

        mergeSubst subst domainSubst =
          foldM
            ( \substAcc (ref, ty) ->
                case lookupSubst ref substAcc of
                  Nothing -> Just (Map.insert ref ty substAcc)
                  Just prev
                    | alphaEqType prev ty -> Just substAcc
                    | otherwise -> Nothing
            )
            subst
            (Map.toList domainSubst)

        instFromSubst subst =
          let args =
                [ (ref, lookupSubst ref subst)
                | ref <- binderRefs
                ]
              neededPrefix =
                reverse (dropWhile (isNothing . snd) (reverse args))
              insts =
                [ maybe InstElim (InstApp . inlineBoundVarsTypeWithContext inlineBoundVarsContext) mbTy
                | (_, mbTy) <- neededPrefix
                ]
           in case insts of
                [] -> Nothing
                firstInst : rest -> Just (foldl' InstSeq firstInst rest)

        lookupSubst ref subst =
          snd <$> find (typeBinderRefsSameIdentity ref . fst) (Map.toList subst)

    collectApplicationSpine term =
      go [] term
      where
        go args (EApp fun arg) = go (arg : args) fun
        go args headTerm = (headTerm, args)

    sourceVarName annExpr =
      case annExpr of
        AVar v _ -> Just v
        AAnn inner _ _ -> sourceVarName inner
        AUnfold inner _ _ -> sourceVarName inner
        _ -> Nothing

    lambdaAnn annExpr =
      case stripAnnExpr annExpr of
        ALam {} -> True
        _ -> False

    identityWrapperAnn annExpr =
      case annExpr of
        ALam param _ _ body _ -> sourceVarName body == Just param
        AAnn inner _ _ -> identityWrapperAnn inner
        AUnfold inner _ _ -> identityWrapperAnn inner
        _ -> False

    {- Note [μ-headed application support]
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       Church-encoded ADTs produce types of the form
         TMu name (TForall ... (TForall ... (TArrow ... ...)))
       When such a term is used in function position, we need to unroll the μ
       to expose the leading TForall/TArrow for instantiation and application.

       appHeadTermFromType: wraps the term in EUnroll when its known type is a
                     TMu whose unfolding eventually reaches TForall or TArrow.

       These helpers allow the existing InstApp/InstElim machinery in AAppF to
       work transparently on Church-encoded eliminators without duplicating
       instantiation logic inside insertMuUseSiteCoercions. -}
    appHeadTermFromType :: Either err ElabType -> XmlfTerm -> XmlfTerm
    appHeadTermFromType termTy term =
      case termTy of
        Right TMuRef {} -> EUnroll term
        _ -> term

    finalCodomain :: ElabType -> ElabType
    finalCodomain = go . peelLeadingForalls
      where
        peelLeadingForalls ty =
          case ty of
            TForallRef _ _ body -> peelLeadingForalls body
            _ -> ty
        go ty =
          case ty of
            TArrow _ cod -> go cod
            _ -> ty

    insertMuUseSiteCoercions :: TypeCheck.Env -> Bool -> Bool -> Maybe ElabType -> Maybe ElabType -> Maybe ElabType -> XmlfTerm -> XmlfTerm -> Either ElabError (XmlfTerm, Maybe ElabType)
    insertMuUseSiteCoercions tcEnv preserveRecursiveArg sourceArgIsVar mbArgSourceTy mbFTy mbArgTy fTerm aTerm = do
      let fTermTy =
            maybe (TypeCheck.typeCheckWithEnv tcEnv fTerm) Right mbFTy
          argTermTy =
            maybe (TypeCheck.typeCheckWithEnv tcEnv aTerm) Right mbArgTy
      let (fUnrolled, unfoldedFromMu) =
            case fTermTy of
              Right muTy@TMuRef {} ->
                case unfoldMuOnce muTy of
                  Just TArrow {} -> (EUnroll fTerm, True)
                  Just TForallRef {} -> (EUnroll fTerm, True)
                  _ -> (fTerm, False)
              _ -> (fTerm, False)
          fUnrolledTy =
            if unfoldedFromMu
              then TypeCheck.typeCheckWithEnv tcEnv fUnrolled
              else fTermTy
          fUnrolledChecked = (fUnrolled, fUnrolledTy)
      {- Note [Instantiate leading ∀ after μ-unfold]
         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         Church-encoded ADTs unfold to ∀result. arrow → ... → result.
         After EUnroll the type is TForall, not TArrow, so we must
         instantiate the leading quantifier before applying arguments.
         We infer the instantiation type from the argument's type. -}
      let checkFunctionCandidate instN =
            case instN of
              InstId -> fUnrolledChecked
              _ ->
                let candidate = ETyInst fUnrolled instN
                 in (candidate, TypeCheck.typeCheckWithEnv tcEnv candidate)
          peelLeadingUnboundedForalls inst0 =
            let go n candidate
                  | n >= (8 :: Int) = candidate
                  | otherwise =
                      case snd candidate of
                        Right (TForallRef _ Nothing _) -> candidate
                        _ -> candidate
             in go 0 (checkFunctionCandidate inst0)
          validatedForVarArg candidate =
            let candidateTy = snd candidate
             in if sourceArgIsVar
                  then case candidateTy of
                    Right TArrow {} -> candidate
                    _ -> fUnrolledChecked
                  else candidate
          fInstantiatedChecked =
            case (unfoldedFromMu, fUnrolledTy) of
              (True, Right (TForallRef _v Nothing _arrowBody)) ->
                case argTermTy of
                  Right argTy ->
                    let sourceMuMatchesActual sourceTy =
                          alphaEqType sourceTy argTy
                            || churchAwareEqType sourceTy argTy
                            || case sourceTy of
                              TMuRef {} ->
                                case unfoldMuOnce sourceTy of
                                  Just unfoldedTy ->
                                    let unfoldedTy' = stripVacuousForallsDeep unfoldedTy
                                        argTy' = stripVacuousForallsDeep argTy
                                     in alphaEqType unfoldedTy' argTy' || churchAwareEqType unfoldedTy' argTy'
                                  Nothing -> False
                              _ -> False
                        instArgTy =
                          case (sourceArgIsVar, mbArgSourceTy) of
                            (True, Just sourceTy) -> sourceTy
                            (_, Just sourceTy@TMuRef {})
                              | sourceMuMatchesActual sourceTy -> sourceTy
                            _ -> argTy
                        inst0 = InstApp instArgTy
                     in validatedForVarArg (peelLeadingUnboundedForalls inst0)
                  Left _ -> fUnrolledChecked
              (_, Right _) ->
                validatedForVarArg (peelLeadingUnboundedForalls InstId)
              _ -> fUnrolledChecked
          fInstantiated = fst fInstantiatedChecked
          fInstantiatedTy = snd fInstantiatedChecked
          appArgTypesCompatible expectedTy actualTy =
            let expectedTy' = stripVacuousForallsDeep expectedTy
                actualTy' = stripVacuousForallsDeep actualTy
             in expectedTy' == TBottom
                  || alphaEqType expectedTy' actualTy'
                  || churchAwareEqType expectedTy' actualTy'
      (aCoerced, mbKnownAppTy) <-
        case fInstantiatedTy of
          Right (TArrow paramTy resTy)
            | preserveRecursiveArg,
              TMuRef {} <- paramTy,
              Right argTy <- argTermTy,
              (alphaEqType argTy paramTy || churchAwareEqType argTy paramTy) ->
                Right (aTerm, Just resTy)
            | Right argTy <- argTermTy,
              appArgTypesCompatible paramTy argTy ->
                Right (aTerm, Just resTy)
            | otherwise -> do
                coerced <- coerceArgForParam tcEnv sourceArgIsVar mbArgSourceTy (either (const Nothing) Just argTermTy) paramTy aTerm
                let mbCoercedAppTy =
                      case argTermTy of
                        Left _
                          | ttTerm coerced == aTerm,
                            ttType coerced == TBottom -> Nothing
                        _ ->
                          let coercedTy = ttType coerced
                           in if appArgTypesCompatible paramTy coercedTy
                                then Just resTy
                                else Nothing
                Right (ttTerm coerced, mbCoercedAppTy)
          _ -> Right (aTerm, Nothing)
      pure (EApp fInstantiated aCoerced, mbKnownAppTy)

    unfoldMuOnce :: ElabType -> Maybe ElabType
    unfoldMuOnce muTy =
      case muTy of
        TMuRef ref body -> Just (substTypeCaptureRef ref muTy body)
        _ -> Nothing

    peelLeadingUnboundedForallsType :: ElabType -> ElabType
    peelLeadingUnboundedForallsType ty =
      case ty of
        TForallRef _ Nothing body -> peelLeadingUnboundedForallsType body
        _ -> ty

    coerceArgForParam :: TypeCheck.Env -> Bool -> Maybe ElabType -> Maybe ElabType -> ElabType -> XmlfTerm -> Either ElabError TypedTerm
    coerceArgForParam tcEnv sourceArgIsVar mbArgSourceTy mbArgTy paramTy argTerm =
      case maybe (TypeCheck.typeCheckWithEnv tcEnv argTerm) Right mbArgTy of
        Left _ -> Right (TypedTerm argTerm TBottom)
        Right argTy ->
          case paramTy of
            TVarRef _
              | Just muTy@TMuRef {} <- mbArgSourceTy ->
                  case unfoldMuOnce muTy of
                    Just unfoldedTy
                      | alphaEqType unfoldedTy argTy || churchAwareEqType unfoldedTy argTy -> Right (TypedTerm (ERoll muTy argTerm) muTy)
                      | otherwise ->
                          let argAligned = alignLeadingLambdasToType unfoldedTy argTerm
                              argStripped = stripUnusedTyAbsAlongType tcEnv unfoldedTy argAligned
                              argRebuilt = rebuildRecursiveArgAlongType tcEnv unfoldedTy argTerm
                           in case TypeCheck.typeCheckWithEnv tcEnv argStripped of
                                Right argTy'
                                  | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (TypedTerm (ERoll muTy argStripped) muTy)
                                _ ->
                                  case TypeCheck.typeCheckWithEnv tcEnv argRebuilt of
                                    Right argTy'
                                      | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (TypedTerm (ERoll muTy argRebuilt) muTy)
                                    _ -> Right (TypedTerm argTerm argTy)
                    _ -> Right (TypedTerm argTerm argTy)
            muTy@TMuRef {} ->
              let sourceMatchesMu =
                    maybe
                      False
                      (\sourceTy -> alphaEqType sourceTy muTy || churchAwareEqType sourceTy muTy)
                      mbArgSourceTy
                      && sourceArgIsVar
                  actualMatchesMu = alphaEqType argTy muTy || churchAwareEqType argTy muTy
                  fallbackMuVar = Right (TypedTerm argTerm argTy)
                  coerceMuFromUnfolded muTy0 argTy0 =
                    case unfoldMuOnce muTy0 of
                      Just unfoldedTy
                        | alphaEqType unfoldedTy argTy0 || churchAwareEqType unfoldedTy argTy0 -> Right (TypedTerm (ERoll muTy0 argTerm) muTy0)
                        | otherwise ->
                            let argAligned = alignLeadingLambdasToType unfoldedTy argTerm
                                argStripped = stripUnusedTyAbsAlongType tcEnv unfoldedTy argAligned
                                argRebuilt = rebuildRecursiveArgAlongType tcEnv unfoldedTy argTerm
                             in case TypeCheck.typeCheckWithEnv tcEnv argStripped of
                                  Right argTy'
                                    | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (TypedTerm (ERoll muTy0 argStripped) muTy0)
                                  _ ->
                                    case TypeCheck.typeCheckWithEnv tcEnv argRebuilt of
                                      Right argTy'
                                        | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (TypedTerm (ERoll muTy0 argRebuilt) muTy0)
                                      _ -> fallbackMuVar
                      _ | shouldRollMuVar muTy0 argTy0 -> Right (TypedTerm (ERoll muTy0 argTerm) muTy0)
                      _ -> fallbackMuVar
               in if sourceMatchesMu && actualMatchesMu
                    then Right (TypedTerm argTerm argTy)
                    else
                      if sourceMatchesMu
                        then case argTy of
                          TMuRef {} -> Right (TypedTerm (ERoll muTy (EUnroll argTerm)) muTy)
                          _
                            | Just unfoldedTy <- unfoldMuOnce muTy,
                              let unfoldedTyPeeled = peelLeadingUnboundedForallsType unfoldedTy,
                              alphaEqType unfoldedTyPeeled argTy || churchAwareEqType unfoldedTyPeeled argTy ->
                                let argRebuilt = rebuildRecursiveArgAlongType tcEnv unfoldedTy argTerm
                                 in case TypeCheck.typeCheckWithEnv tcEnv argRebuilt of
                                      Right argTy'
                                        | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (TypedTerm (ERoll muTy argRebuilt) muTy)
                                      _ -> coerceMuFromUnfolded muTy argTy
                          _ -> coerceMuFromUnfolded muTy argTy
                        else case argTy of
                          argMu@TMuRef {}
                            | alphaEqType argMu muTy || churchAwareEqType argMu muTy ->
                                Right (TypedTerm argTerm argTy)
                            | otherwise ->
                                case (unfoldMuOnce muTy, unfoldMuOnce argMu) of
                                  (Just expectedBodyTy, Just argBodyTy)
                                    | alphaEqType expectedBodyTy argBodyTy || churchAwareEqType expectedBodyTy argBodyTy ->
                                        Right (TypedTerm (ERoll muTy (EUnroll argTerm)) muTy)
                                    | otherwise ->
                                        let argUnrolled = EUnroll argTerm
                                            argAligned = alignLeadingLambdasToType expectedBodyTy argUnrolled
                                            argStripped = stripUnusedTyAbsAlongType tcEnv expectedBodyTy argAligned
                                            argRebuilt = rebuildRecursiveArgAlongType tcEnv expectedBodyTy argUnrolled
                                         in case TypeCheck.typeCheckWithEnv tcEnv argStripped of
                                              Right argTy'
                                                | alphaEqType expectedBodyTy argTy' || churchAwareEqType expectedBodyTy argTy' ->
                                                    Right (TypedTerm (ERoll muTy argStripped) muTy)
                                              _ ->
                                                case TypeCheck.typeCheckWithEnv tcEnv argRebuilt of
                                                  Right argTy'
                                                    | alphaEqType expectedBodyTy argTy' || churchAwareEqType expectedBodyTy argTy' ->
                                                        Right (TypedTerm (ERoll muTy argRebuilt) muTy)
                                                  _ -> coerceMuFromUnfolded muTy argTy
                                  _ -> coerceMuFromUnfolded muTy argTy
                          _ -> coerceMuFromUnfolded muTy argTy
            _ ->
              case argTy of
                muTy@TMuRef {} ->
                  case unfoldMuOnce muTy of
                    Just unfoldedTy
                      | alphaEqType paramTy unfoldedTy || churchAwareEqType paramTy unfoldedTy -> Right (TypedTerm (EUnroll argTerm) unfoldedTy)
                    _ -> Right (TypedTerm argTerm argTy)
                _ -> Right (TypedTerm argTerm argTy)

    rollResultToExpectedMu :: TypeCheck.Env -> Maybe ElabType -> TypedTerm -> TypedTerm
    rollResultToExpectedMu tcEnv mbTargetTy typedTerm =
      case mbTargetTy of
        Just muTy@TMuRef {} ->
          let term = ttTerm typedTerm
              termTy = ttType typedTerm
           in if alphaEqType termTy muTy
                then typedTerm
                else
                  case termTy of
                    actualMu@(TMuRef actualRef actualBody)
                      | churchAwareEqType termTy muTy ->
                          case unfoldMuOnce muTy of
                            Just expectedBodyTy ->
                              let actualBodyTy = stripVacuousForallsDeep (substTypeCaptureRef actualRef actualMu actualBody)
                               in if alphaEqType actualBodyTy expectedBodyTy || churchAwareEqType actualBodyTy expectedBodyTy
                                    then TypedTerm (ERoll muTy (EUnroll term)) muTy
                                    else typedTerm
                            _ -> typedTerm
                    _ ->
                      case unfoldMuOnce muTy of
                        Just unfoldedTy
                          | alphaEqType termTy unfoldedTy || churchAwareEqType termTy unfoldedTy ->
                              TypedTerm (ERoll muTy term) muTy
                          | otherwise ->
                              let termAligned = alignLeadingLambdasToType unfoldedTy term
                                  termStripped = stripUnusedTyAbsAlongType tcEnv unfoldedTy termAligned
                                  termRebuilt = rebuildRecursiveArgAlongType tcEnv unfoldedTy term
                               in case TypeCheck.typeCheckWithEnv tcEnv termStripped of
                                    Right termTy'
                                      | alphaEqType termTy' unfoldedTy || churchAwareEqType termTy' unfoldedTy ->
                                          TypedTerm (ERoll muTy termStripped) muTy
                                    _ ->
                                      case TypeCheck.typeCheckWithEnv tcEnv termRebuilt of
                                        Right termTy'
                                          | alphaEqType termTy' unfoldedTy || churchAwareEqType termTy' unfoldedTy ->
                                              TypedTerm (ERoll muTy termRebuilt) muTy
                                        _ -> typedTerm
                        _ -> typedTerm
        _ -> typedTerm

    stripVacuousForallsDeep :: ElabType -> ElabType
    stripVacuousForallsDeep ty = case ty of
      TForallRef ref Nothing body
        | not (any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType body)) ->
            stripVacuousForallsDeep body
      TForallRef ref mb body ->
        TForallRef ref (fmap stripVacuousForallsDeepBound mb) (stripVacuousForallsDeep body)
      TArrow dom cod -> TArrow (stripVacuousForallsDeep dom) (stripVacuousForallsDeep cod)
      TConWithIdentity identity con args -> TConWithIdentity identity con (fmap stripVacuousForallsDeep args)
      TVarAppRef ref args -> TVarAppRef ref (fmap stripVacuousForallsDeep args)
      TMuRef ref body -> TMuRef ref (stripVacuousForallsDeep body)
      _ -> ty

    stripVacuousForallsDeepBound :: BoundType -> BoundType
    stripVacuousForallsDeepBound bound = case bound of
      TArrow dom cod -> TArrow (stripVacuousForallsDeep dom) (stripVacuousForallsDeep cod)
      TBaseWithIdentity _ _ -> bound
      TConWithIdentity identity con args -> TConWithIdentity identity con (fmap stripVacuousForallsDeep args)
      TVarAppRef ref args -> TVarAppRef ref (fmap stripVacuousForallsDeep args)
      TForallRef ref mb body -> TForallRef ref (fmap stripVacuousForallsDeepBound mb) (stripVacuousForallsDeep body)
      TMuRef ref body -> TMuRef ref (stripVacuousForallsDeep body)
      TBottom -> TBottom

    containsFreeVar :: LocalVarKey -> XmlfTerm -> Bool
    containsFreeVar v term =
      case term of
        EVarNode resolved -> localVarKeyMatchesReference v resolved
        ELit _ -> False
        ELam resolved body
          | localVarKeyMatchesReference v resolved -> False
          | otherwise -> containsFreeVar v body
        EApp f a -> containsFreeVar v f || containsFreeVar v a
        ELet resolved _ rhs body
          | localVarKeyMatchesReference v resolved -> containsFreeVar v rhs
          | otherwise -> containsFreeVar v rhs || containsFreeVar v body
        ETyAbsRef _ _ body -> containsFreeVar v body
        ETyInst e _ -> containsFreeVar v e
        ERoll _ body -> containsFreeVar v body
        EUnroll e -> containsFreeVar v e

    alignLeadingLambdasToType :: ElabType -> XmlfTerm -> XmlfTerm
    alignLeadingLambdasToType ty term =
      case (ty, term) of
        (TForallRef targetRef _ bodyTy, ETyAbsRef termRef mb body) ->
          let bodyTy' = substTypeCaptureRef targetRef (TVarRef termRef) bodyTy
           in ETyAbsRef termRef mb (alignLeadingLambdasToType bodyTy' body)
        (TArrow dom cod, ELam resolved body) ->
          let body' = alignLeadingLambdasToType cod body
           in ELam (mapResolvedVarType (const dom) resolved) (refreshLocalResolvedVarType (LocalVarResolved resolved) dom body')
        _ -> term

    stripUnusedTopTyAbsWithEnv :: TypeCheck.Env -> XmlfTerm -> XmlfTerm
    stripUnusedTopTyAbsWithEnv tcEnv term =
      case term of
        ETyAbsRef ref mbBound body ->
          let body' = stripUnusedTopTyAbsWithEnv tcEnv body
              term' = ETyAbsRef ref mbBound body'
           in case TypeCheck.typeCheckWithEnv tcEnv term' of
                Right (TForallRef _ _ bodyTy)
                  | not (any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType bodyTy)) -> body'
                _ -> term'
        _ -> term

    stripUnusedTyAbsAlongType :: TypeCheck.Env -> ElabType -> XmlfTerm -> XmlfTerm
    stripUnusedTyAbsAlongType tcEnv targetTy term =
      let term' = stripUnusedTopTyAbsWithEnv tcEnv term
       in case (targetTy, term') of
            (TForallRef targetRef _ targetBody, ETyAbsRef termRef mbBound body)
              | typeBinderRefsSameIdentity targetRef termRef ->
                  ETyAbsRef termRef mbBound (stripUnusedTyAbsAlongType tcEnv targetBody body)
            (TArrow dom cod, ELam resolved body) ->
              let body' = stripUnusedTyAbsAlongType tcEnv cod body
               in ELam (mapResolvedVarType (const dom) resolved) (refreshLocalResolvedVarType (LocalVarResolved resolved) dom body')
            _ -> term'

    hoistFloatingTyAbsThroughLambdas :: XmlfTerm -> XmlfTerm
    hoistFloatingTyAbsThroughLambdas term =
      case term of
        ELam resolved body ->
          let body' = hoistFloatingTyAbsThroughLambdas body
              ty = resolvedVarType resolved
           in case body' of
                ETyAbsRef tyRef mbBound inner
                  | not (any (typeBinderRefsSameIdentity tyRef) (freeTypeVarRefsType ty)) ->
                      ETyAbsRef tyRef mbBound (hoistFloatingTyAbsThroughLambdas (ELam resolved inner))
                _ -> ELam resolved body'
        EApp fun arg -> EApp (hoistFloatingTyAbsThroughLambdas fun) (hoistFloatingTyAbsThroughLambdas arg)
        ELet resolved sch rhs body ->
          ELet resolved sch (hoistFloatingTyAbsThroughLambdas rhs) (hoistFloatingTyAbsThroughLambdas body)
        ETyAbsRef ref mbBound body -> ETyAbsRef ref mbBound (hoistFloatingTyAbsThroughLambdas body)
        ETyInst body inst -> ETyInst (hoistFloatingTyAbsThroughLambdas body) inst
        ERoll ty body -> ERoll ty (hoistFloatingTyAbsThroughLambdas body)
        EUnroll body -> EUnroll (hoistFloatingTyAbsThroughLambdas body)
        _ -> term

    addMissingLeadingTyAbsAlongType :: TypeCheck.Env -> ElabType -> XmlfTerm -> XmlfTerm
    addMissingLeadingTyAbsAlongType tcEnv targetTy term =
      let initialReserved =
            Set.unions
              ( Set.union (typeAbsNamesInTerm term) (typeVarNamesInTerm term)
                  : map freeTypeVarsType (map snd (TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv tcEnv)))
                  ++ [Set.fromList (map typeBinderRefName (Map.keys (TypeCheck.typeEnv tcEnv))), forallBinderNames targetTy]
              )
       in go initialReserved targetTy term
      where
        go reserved targetTy' term' =
          case targetTy' of
            TForallRef targetRef mbBound targetBody ->
              case stripUnusedTopTyAbsWithEnv tcEnv term' of
                ETyAbsRef termRef termBound body
                  | typeBinderRefsSameIdentity targetRef termRef ->
                      ETyAbsRef termRef termBound (go (Set.insert (typeBinderRefName termRef) reserved) targetBody body)
                term'' ->
                  let targetName = typeBinderRefName targetRef
                      (targetRef', targetBody') =
                        if Set.member targetName reserved
                          then
                            let fresh = freshNameLike targetName reserved
                                freshTargetRef = renameTypeBinderRef fresh targetRef
                             in (freshTargetRef, substTypeCaptureRef targetRef (TVarRef freshTargetRef) targetBody)
                          else (targetRef, targetBody)
                      reserved' = Set.insert (typeBinderRefName targetRef') reserved
                      body' = go reserved' targetBody' term''
                   in ETyAbsRef targetRef' mbBound body'
            TArrow dom cod ->
              case term' of
                ELam resolved body ->
                  let body' = go reserved cod body
                   in ELam (mapResolvedVarType (const dom) resolved) (refreshLocalResolvedVarType (LocalVarResolved resolved) dom body')
                _ -> term'
            _ -> term'

        forallBinderNames ty =
          case ty of
            TForallRef ref _ body -> Set.insert (typeBinderRefName ref) (forallBinderNames body)
            _ -> Set.empty

    typeAbsNamesInTerm :: XmlfTerm -> Set.Set String
    typeAbsNamesInTerm term =
      case term of
        ETyAbsRef ref _ body -> Set.insert (typeBinderRefName ref) (typeAbsNamesInTerm body)
        ELam _ body -> typeAbsNamesInTerm body
        EApp f a -> Set.union (typeAbsNamesInTerm f) (typeAbsNamesInTerm a)
        ELet _ _ rhs body -> Set.union (typeAbsNamesInTerm rhs) (typeAbsNamesInTerm body)
        ETyInst body _ -> typeAbsNamesInTerm body
        ERoll _ body -> typeAbsNamesInTerm body
        EUnroll body -> typeAbsNamesInTerm body
        _ -> Set.empty

    typeVarNamesInTerm :: XmlfTerm -> Set.Set String
    typeVarNamesInTerm term =
      case term of
        ETyAbsRef ref mb body ->
          Set.insert (typeBinderRefName ref) (maybe Set.empty freeTypeVarsType mb `Set.union` typeVarNamesInTerm body)
        ELam resolved body -> Set.union (freeTypeVarsType (resolvedVarType resolved)) (typeVarNamesInTerm body)
        EApp f a -> Set.union (typeVarNamesInTerm f) (typeVarNamesInTerm a)
        ELet _ sch rhs body -> Set.unions [freeTypeVarsType (schemeToType sch), typeVarNamesInTerm rhs, typeVarNamesInTerm body]
        ETyInst body inst -> Set.union (typeVarNamesInTerm body) (goInst inst)
        ERoll ty body -> Set.union (freeTypeVarsType ty) (typeVarNamesInTerm body)
        EUnroll body -> typeVarNamesInTerm body
        _ -> Set.empty
      where
        goInst inst =
          case inst of
            InstId -> Set.empty
            InstApp ty -> freeTypeVarsType ty
            InstIntro -> Set.empty
            InstElim -> Set.empty
            InstInside inner -> goInst inner
            InstSeq a b -> Set.union (goInst a) (goInst b)
            InstUnderRef _ inner -> goInst inner
            InstBot ty -> freeTypeVarsType ty
            InstAbstrRef _ -> Set.empty

    rebuildRecursiveArgAlongType :: TypeCheck.Env -> ElabType -> XmlfTerm -> XmlfTerm
    rebuildRecursiveArgAlongType tcEnv targetTy term =
      let normalized = normalize term
          hoisted = hoistFloatingTyAbsThroughLambdas normalized
          stripped = stripUnusedTopTyAbsWithEnv tcEnv hoisted
          withTyAbs = addMissingLeadingTyAbsAlongType tcEnv targetTy stripped
       in alignLeadingLambdasToType targetTy withTyAbs

    annContainsVar :: VarName -> AnnExpr -> Bool
    annContainsVar v annExpr =
      case annExpr of
        ALit _ _ -> False
        AVar x _ -> x == v
        ALam x _ _ body _
          | x == v -> False
          | otherwise -> annContainsVar v body
        AApp f a _ _ _ -> annContainsVar v f || annContainsVar v a
        ALet x _ _ _ _ rhs body _
          | x == v -> annContainsVar v rhs
          | otherwise -> annContainsVar v rhs || annContainsVar v body
        AAnn inner _ _ -> annContainsVar v inner
        AUnfold inner _ _ -> annContainsVar v inner

    blockedAliasMuType :: ElabType -> Maybe ElabType
    blockedAliasMuType ty =
      case ty of
        TForallRef ref Nothing (TArrow (TVarRef domRef) cod)
          | typeBinderRefsSameIdentity ref domRef -> Just (TMuRef ref (TArrow (TVarRef ref) cod))
        _ -> Nothing

    shouldRollMuVar :: ElabType -> ElabType -> Bool
    shouldRollMuVar muTy argTy =
      case (muTy, argTy) of
        (TMuRef _ _, TVarRef _) -> True
        _ -> False

mkOut :: (Env -> Either ElabError XmlfTerm) -> ElabOut
mkOut f = ElabOut f f

resolvedLambdaParamNode :: (NodeId -> NodeId) -> (NodeId -> Maybe TyNode) -> NodeId -> Maybe NodeId
resolvedLambdaParamNode canonical lookupNode lamNodeId =
  let lamC = canonical lamNodeId
   in case lookupNode lamC of
        Just TyArrow {tnDom = dom} -> Just dom
        Just TyVar {tnBound = Just bnd} ->
          case lookupNode (canonical bnd) of
            Just TyArrow {tnDom = dom} -> Just dom
            _ -> Nothing
        _ -> Nothing

{- Note [srcTypeToElabType in Algebra]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Local copy of the NormSrcType → ElabType conversion.  The canonical copy lives
in MLF.Frontend.Program.Elaborate but is not exported (production surface is
kept narrow).  We need this conversion in ALamF to recover the original source
annotation type that presolution may have stripped (e.g. TForall inside a μ
body).  Keeping it local avoids widening a production facade for a single
internal consumer.
-}

-- | Convert a normalized source type to its elaboration-level equivalent.
srcTypeToElabType :: AlgebraContext p -> NormSrcType -> Either ElabError ElabType
srcTypeToElabType algebraContext ty =
  let (refs, generator) =
        sourceTypeBinderRefs
          (algSourceTypeBinderIdentities algebraContext)
          (Set.toList (freeSrcTypeVars ty))
          (sourceTypeIdentityGenerator algebraContext ty)
   in fmap fst (srcTypeToElabTypeWith algebraContext refs generator ty)

sourceTypeIdentityGenerator :: AlgebraContext p -> NormSrcType -> IdentityGenerator
sourceTypeIdentityGenerator algebraContext ty =
  identityGeneratorAfter
    ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
        ++ concatMap typeBinderGeneratedIdentities (Map.elems (algSourceTypeBinderIdentities algebraContext))
    )
  where
    headIdentities =
      Map.union
        (algSourceTypeHeadIdentities algebraContext)
        (Builtins.builtinSourceTypeHeadIdentities ty)

freeSrcTypeVars :: SrcTy n v -> Set.Set String
freeSrcTypeVars ty =
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

sourceTypeBinderRefs :: Map.Map String TypeBinderIdentity -> [String] -> IdentityGenerator -> (Map.Map String TypeBinderRef, IdentityGenerator)
sourceTypeBinderRefs binderIdentities names generator0 =
  go names Map.empty generator0
  where
    go [] refs generator = (refs, generator)
    go (name : rest) refs generator =
      let (ref, generator1) = sourceTypeBinderRefOrFresh binderIdentities name generator
       in go rest (Map.insert name ref refs) generator1

sourceTypeBinderRefOrFresh :: Map.Map String TypeBinderIdentity -> String -> IdentityGenerator -> (TypeBinderRef, IdentityGenerator)
sourceTypeBinderRefOrFresh binderIdentities name generator =
  case lookupSourceTypeBinderIdentity binderIdentities name of
    Just identity -> (typeBinderRefFromIdentity identity name, generator)
    Nothing -> sourceTypeBinderRefForName name generator

sourceTypeBinderRefOrFreshInScope :: Bool -> Map.Map String TypeBinderIdentity -> String -> IdentityGenerator -> (TypeBinderRef, IdentityGenerator)
sourceTypeBinderRefOrFreshInScope shadowed binderIdentities name generator
  | shadowed = sourceTypeBinderRefForName name generator
  | otherwise = sourceTypeBinderRefOrFresh binderIdentities name generator

lookupSourceTypeBinderIdentity :: Map.Map String TypeBinderIdentity -> String -> Maybe TypeBinderIdentity
lookupSourceTypeBinderIdentity binderIdentities name =
  case Map.lookup name binderIdentities of
    Just identity -> Just identity
    Nothing -> Map.lookup name (sourceTypeBinderStableAliases binderIdentities)

sourceTypeBinderStableAliases :: Map.Map String TypeBinderIdentity -> Map.Map String TypeBinderIdentity
sourceTypeBinderStableAliases =
  typeBinderIdentityAliasMap . Map.toList

lookupSourceTypeHeadIdentity :: Map.Map String SymbolIdentity -> String -> Maybe SymbolIdentity
lookupSourceTypeHeadIdentity headIdentities name =
  case Map.lookup name headIdentities of
    Just identity -> Just identity
    Nothing -> Map.lookup name (sourceTypeHeadStableAliases headIdentities)

sourceTypeHeadStableAliases :: Map.Map String SymbolIdentity -> Map.Map String SymbolIdentity
sourceTypeHeadStableAliases =
  symbolIdentityAliasMap . Map.elems

srcTypeToElabTypeWith :: AlgebraContext p -> Map.Map String TypeBinderRef -> IdentityGenerator -> NormSrcType -> Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWith =
  srcTypeToElabTypeWithBound Set.empty

srcTypeToElabTypeWithBound ::
  Set.Set String ->
  AlgebraContext p ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  NormSrcType ->
  Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithBound boundNames algebraContext refs generator ty = case ty of
  STVar name -> do
    ref <- sourceTypeBinderRef refs name
    Right (TVarRef ref, generator)
  STArrow dom cod -> do
    (dom', generator1) <- srcTypeToElabTypeWithBound boundNames algebraContext refs generator dom
    (cod', generator2) <- srcTypeToElabTypeWithBound boundNames algebraContext refs generator1 cod
    Right (TArrow dom' cod', generator2)
  STBase name -> Right (TBaseWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name), generator)
  STCon name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    Right (TConWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name) args', generator')
  STVarApp name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    ref <- sourceTypeBinderRef refs name
    Right (TVarAppRef ref args', generator')
  STTyLam {} ->
    Left (InstantiationError "residual type lambda reached elaboration")
  STTyApp {} ->
    Left (InstantiationError "residual type application reached elaboration")
  STForall name mb body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) (algSourceTypeBinderIdentities algebraContext) name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWith boundNames refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithBound boundNames' algebraContext refs' generator2 body
          Right (TForallRef ref mb' body', generator3)
  STMu name body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) (algSourceTypeBinderIdentities algebraContext) name generator
        boundNames' = Set.insert name boundNames
     in do
          (body', generator2) <- srcTypeToElabTypeWithBound boundNames' algebraContext (Map.insert name ref refs) generator1 body
          Right (TMuRef ref body', generator2)
  STBottom -> Right (TBottom, generator)
  where
    sourceTypeHeadIdentity name =
      lookupSourceTypeHeadIdentity (algSourceTypeHeadIdentities algebraContext) name <|> Builtins.builtinTypeHeadIdentity name

    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing -> Left (InstantiationError ("unresolved source type binder `" ++ name ++ "` reached algebra elaboration"))

    srcTypesToElabTypesWith boundNames' refs0 generator0 (arg :| args) = do
      (arg', generator1) <- srcTypeToElabTypeWithBound boundNames' algebraContext refs0 generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- srcTypeToElabTypeWithBound boundNames' algebraContext refs0 gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

    srcBoundToElabBoundWith :: Set.Set String -> Map.Map String TypeBinderRef -> IdentityGenerator -> SrcBound 'NormN -> Either ElabError (Maybe BoundType, IdentityGenerator)
    srcBoundToElabBoundWith boundNames' refs' generator0 (SrcBound boundTy) = structBoundToElabBoundWith boundNames' refs' generator0 boundTy

    structBoundToElabBoundWith :: Set.Set String -> Map.Map String TypeBinderRef -> IdentityGenerator -> StructBound -> Either ElabError (Maybe BoundType, IdentityGenerator)
    structBoundToElabBoundWith boundNames' refs' generator0 bTy = case bTy of
      STArrow dom cod -> do
        (dom', generator1) <- srcTypeToElabTypeWithBound boundNames' algebraContext refs' generator0 dom
        (cod', generator2) <- srcTypeToElabTypeWithBound boundNames' algebraContext refs' generator1 cod
        Right (Just (TArrow dom' cod'), generator2)
      STBase name -> Right (Just (TBaseWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name)), generator0)
      STCon name args -> do
        (args', generator1) <- srcTypesToElabTypesWith boundNames' refs' generator0 args
        Right (Just (TConWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name) args'), generator1)
      STVarApp name args -> do
        (args', generator1) <- srcTypesToElabTypesWith boundNames' refs' generator0 args
        ref <- sourceTypeBinderRef refs' name
        Right (Just (TVarAppRef ref args'), generator1)
      STTyLam {} ->
        Left (InstantiationError "residual type lambda reached elaboration")
      STTyApp {} ->
        Left (InstantiationError "residual type application reached elaboration")
      STForall name mb body ->
        let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames') (algSourceTypeBinderIdentities algebraContext) name generator0
            refs'' = Map.insert name ref refs'
            boundNames'' = Set.insert name boundNames'
         in do
              (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWith boundNames' refs' generator1) mb
              (body', generator3) <- srcTypeToElabTypeWithBound boundNames'' algebraContext refs'' generator2 body
              Right (Just (TForallRef ref mb' body'), generator3)
      STMu name body ->
        let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames') (algSourceTypeBinderIdentities algebraContext) name generator0
            boundNames'' = Set.insert name boundNames'
         in do
              (body', generator2) <- srcTypeToElabTypeWithBound boundNames'' algebraContext (Map.insert name ref refs') generator1 body
              Right (Just (TMuRef ref body'), generator2)
      STBottom -> Right (Nothing, generator0)

builtinBaseTy :: String -> BaseTy
builtinBaseTy =
  BaseTy . Builtins.normalizeBuiltinTypeReference

annNode :: AnnExpr -> NodeId
annNode ann =
  case ann of
    ALit _ nid -> nid
    AVar _ nid -> nid
    ALam _ _ _ _ nid -> nid
    AApp _ _ _ _ nid -> nid
    ALet _ _ _ _ _ _ _ nid -> nid
    AAnn _ nid _ -> nid
    AUnfold _ nid _ -> nid
