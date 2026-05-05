{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module MLF.Elab.Elaborate.Algebra
  ( Env,
    ElabOut (..),
    AlgebraContext (..),
    elabAlg,
    mkEnvBinding,
    resolvedLambdaParamNode,
  )
where

import Control.Applicative ((<|>))
import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import MLF.Constraint.Presolution (PresolutionView)
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    NodeId,
    TyNode (..),
    getNodeId,
  )
import MLF.Constraint.Types.Phase (Phase(Raw))
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
import qualified MLF.Elab.Inst as Inst
import MLF.Elab.Reduce (normalize)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme)
import MLF.Elab.Run.ResultType.Util
  ( CandidateSelection (..),
    selectUniqueCandidateBy,
  )
import MLF.Elab.Run.TypeOps (inlineBoundVarsType, simplifyAnnotationType)
import MLF.Elab.TermClosure (closeTermWithSchemeSubstIfNeeded)
import qualified MLF.Elab.TypeCheck as TypeCheck (Env (..), typeCheckWithEnv)
import MLF.Elab.Types
  ( BoundType,
    ElabScheme,
    ElabError (..),
    ElabTerm (..),
    ElabType,
    Instantiation (..),
    SchemeInfo (..),
    Ty (..),
    mapBoundType,
    schemeFromType,
    tyToElab,
    pattern Forall,
  )
import MLF.Frontend.ConstraintGen.Types (AnnExpr (..), AnnExprF (..))
import MLF.Frontend.Syntax (NormSrcType, SrcBound (..), SrcNorm (..), SrcTy (..), StructBound, VarName)
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, firstNonContractiveRecursiveType, freeTypeVarsType, freshNameLike, parseNameId, substTypeCapture)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data EnvBinding = EnvBinding
  { ebSchemeInfo :: SchemeInfo,
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

type Env = Map.Map VarName EnvBinding

data ElabOut = ElabOut
  { elabTerm :: Env -> Either ElabError ElabTerm,
    elabStripped :: Env -> Either ElabError ElabTerm
  }

data AlgebraContext = AlgebraContext
  { algPresolutionView :: PresolutionView 'Raw,
    algTraceConfig :: TraceConfig,
    algCanonical :: NodeId -> NodeId,
    algResolvedLambdaParamNode :: NodeId -> Maybe NodeId,
    algAnnotationContext :: AnnotationContext,
    algNamedSetReify :: IntSet.IntSet,
    -- | Original source annotation types from constraint generation, keyed by
    -- canonicalized AAnn codomain NodeId.  Used in ALamF to recover annotation
    -- types that presolution strips (e.g. TForall inside a μ body).
    algAnnSourceTypes :: IntMap.IntMap NormSrcType
  }

containsMuType :: ElabType -> Bool
containsMuType ty =
  case ty of
    TMu {} -> True
    TArrow dom cod -> containsMuType dom || containsMuType cod
    TCon _ args -> any containsMuType args
    TForall _ mb body -> maybe False containsMuBound mb || containsMuType body
    _ -> False
  where
    containsMuBound bound = case bound of
      TArrow dom cod -> containsMuType dom || containsMuType cod
      TCon _ args -> any containsMuType args
      TForall _ mb body -> maybe False containsMuBound mb || containsMuType body
      TMu {} -> True
      _ -> False

hasContractiveRecursiveWitness :: ElabType -> Bool
hasContractiveRecursiveWitness ty =
  containsMuType ty && isNothing (firstNonContractiveRecursiveType ty)

isSingleBinderIdentityScheme :: SchemeInfo -> Bool
isSingleBinderIdentityScheme schemeInfo =
  case Inst.splitForalls (schemeToType (siScheme schemeInfo)) of
    ([(binderName, Nothing)], TArrow (TVar dom) (TVar cod)) ->
      dom == binderName && cod == binderName
    _ -> False

containsInternalTypeVar :: ElabType -> Bool
containsInternalTypeVar ty =
  case ty of
    TVar name -> isJust (parseNameId name)
    TArrow dom cod -> containsInternalTypeVar dom || containsInternalTypeVar cod
    TCon _ args -> any containsInternalTypeVar args
    TForall _ mb body -> maybe False containsInternalBoundVar mb || containsInternalTypeVar body
    TMu _ body -> containsInternalTypeVar body
    _ -> False
  where
    containsInternalBoundVar bound =
      case bound of
        TArrow dom cod -> containsInternalTypeVar dom || containsInternalTypeVar cod
        TCon _ args -> any containsInternalTypeVar args
        TForall _ mb body -> maybe False containsInternalBoundVar mb || containsInternalTypeVar body
        TMu _ body -> containsInternalTypeVar body
        _ -> False

mkEnvBinding :: SchemeInfo -> Bool -> EnvBinding
mkEnvBinding schemeInfo transparentMediator =
  EnvBinding
    { ebSchemeInfo = schemeInfo,
      ebTransparentMediator = transparentMediator,
      ebAliasTarget = Nothing,
      ebExplicitRecursiveParam = False
    }

envSchemeInfos :: Env -> Map.Map VarName SchemeInfo
envSchemeInfos = Map.map ebSchemeInfo

lookupSchemeInfo :: VarName -> Env -> Maybe SchemeInfo
lookupSchemeInfo name env = ebSchemeInfo <$> Map.lookup name env

sourceAnnotatedTypeFrom :: AlgebraContext -> Env -> AnnExpr -> Either ElabError (Maybe ElabType)
sourceAnnotatedTypeFrom algebraContext env ann =
  case ann of
    AVar name _ -> pure (schemeToType . siScheme <$> lookupSchemeInfo name env)
    AAnn inner annNodeId _ ->
      case IntMap.lookup (getNodeId annNodeId) (algAnnSourceTypes algebraContext) of
        Just srcTy -> Just <$> srcTypeToElabType srcTy
        Nothing -> sourceAnnotatedTypeFrom algebraContext env inner
    AUnfold inner _ _ -> sourceAnnotatedTypeFrom algebraContext env inner
    _ -> pure Nothing

sourceSchemePairFromType :: NormSrcType -> Either ElabError (ElabScheme, IntMap.IntMap String)
sourceSchemePairFromType srcTy = do
  ty <- srcTypeToElabType srcTy
  pure (schemeFromType ty, IntMap.empty)

sourceSchemePairForNode :: AlgebraContext -> ScopeContext -> NodeId -> Either ElabError (Maybe (ElabScheme, IntMap.IntMap String))
sourceSchemePairForNode algebraContext scopeContext nodeId =
  case IntMap.lookup (getNodeId nodeId) (algAnnSourceTypes algebraContext) of
    Just srcTy -> do
      fallback <- sourceSchemePairFromType srcTy
      pure $
        case reifyNodeTypePreferringBound scopeContext nodeId of
          Right ty@TMu {} -> Just (schemeFromType ty, IntMap.empty)
          _ -> Just fallback
    Nothing -> pure Nothing

sourceSchemePairForOuterAnnotation :: AlgebraContext -> ScopeContext -> AnnExpr -> Either ElabError (Maybe (ElabScheme, IntMap.IntMap String))
sourceSchemePairForOuterAnnotation algebraContext scopeContext annExpr =
  case annExpr of
    AAnn _ annNodeId _ -> sourceSchemePairForNode algebraContext scopeContext annNodeId
    AUnfold (AAnn _ annNodeId _) _ _ -> sourceSchemePairForNode algebraContext scopeContext annNodeId
    _ -> pure Nothing

sourceSchemePairForAnnotation :: AlgebraContext -> ScopeContext -> AnnExpr -> Either ElabError (Maybe (ElabScheme, IntMap.IntMap String))
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
lookupAliasTarget name env = Map.lookup name env >>= ebAliasTarget

resolveAliasVar :: Env -> VarName -> VarName
resolveAliasVar env name =
  case lookupAliasTarget name env of
    Just target -> resolveAliasVar env target
    Nothing -> name

isTransparentMediatorVar :: VarName -> Env -> Bool
isTransparentMediatorVar name env = maybe False ebTransparentMediator (Map.lookup name env)

freeTypeVarsEnvSchemes :: Env -> Set.Set String
freeTypeVarsEnvSchemes env =
  Set.unions
    [ freeTypeVarsType (schemeToType (siScheme schemeInfo))
      | schemeInfo <- Map.elems (envSchemeInfos env)
    ]

applyTypeVarRenames :: [(String, String)] -> ElabType -> ElabType
applyTypeVarRenames renames ty0 =
  foldl'
    ( \ty (old, new) ->
        if old == new
          then ty
          else substTypeCapture old (TVar new) ty
    )
    ty0
    renames

freeTypeVarsInOccurrenceOrder :: ElabType -> [String]
freeTypeVarsInOccurrenceOrder ty0 = reverse (snd (goType Set.empty Set.empty [] ty0))
  where
    addName bound seen acc name
      | name `Set.member` bound = (seen, acc)
      | name `Set.member` seen = (seen, acc)
      | otherwise = (Set.insert name seen, name : acc)

    goType bound seen acc ty =
      case ty of
        TVar name -> addName bound seen acc name
        TArrow dom cod ->
          let (seen', acc') = goType bound seen acc dom
           in goType bound seen' acc' cod
        TCon _ args ->
          foldl' (\(seen', acc') arg -> goType bound seen' acc' arg) (seen, acc) args
        TForall name mb body ->
          let (seen', acc') =
                maybe (seen, acc) (\boundTy -> goType bound seen acc (tyToElab boundTy)) mb
           in goType (Set.insert name bound) seen' acc' body
        TMu name body -> goType (Set.insert name bound) seen acc body
        TBase _ -> (seen, acc)
        TBottom -> (seen, acc)

freshenSchemeInfoAgainstEnv :: Env -> SchemeInfo -> SchemeInfo
freshenSchemeInfoAgainstEnv env schemeInfo =
  let reservedNames = freeTypeVarsEnvSchemes env
      schemeTy = schemeToType (siScheme schemeInfo)
      (binds, body0) = Inst.splitForalls schemeTy
      binderNames = map fst binds
      binderDomain = Set.fromList binderNames
      renames = reverse (snd (foldl' (chooseFreshBinder binderDomain) (reservedNames, []) binderNames))
      actualRenames = filter (\(old, new) -> old /= new) renames
      renameMap = Map.fromList actualRenames
   in if null actualRenames
        then schemeInfo
        else
          let binds' = renameSchemeBinds [] renames binds
              body' = applyTypeVarRenames actualRenames body0
              subst' =
                IntMap.map
                  (\name -> Map.findWithDefault name name renameMap)
                  (siSubst schemeInfo)
           in SchemeInfo
                { siScheme = Forall binds' body',
                  siSubst = subst'
                }
  where
    chooseFreshBinder binderDomain (used, acc) binder =
      let binder' =
            if Set.member binder used
              then freshNameLike binder (Set.union used binderDomain)
              else binder
       in (Set.insert binder' used, (binder, binder') : acc)

    renameSchemeBinds _ [] [] = []
    renameSchemeBinds prev ((old, new) : restRenames) ((_, mbBound) : restBinds) =
      let mbBound' = fmap (mapBoundType (applyTypeVarRenames prev)) mbBound
          prev'
            | old == new = prev
            | otherwise = prev ++ [(old, new)]
       in (new, mbBound') : renameSchemeBinds prev' restRenames restBinds
    renameSchemeBinds _ _ binds = binds

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
  let (binds, _) = Inst.splitForalls schemeTy
      go [] = False
      go ((_, mbBound) : rest) =
        let laterNames = Set.fromList (map fst rest)
            boundMentionsLater =
              case mbBound of
                Just bound ->
                  not
                    ( Set.null
                        (Set.intersection laterNames (freeTypeVarsType (tyToElab bound)))
                    )
                Nothing -> False
         in boundMentionsLater || go rest
   in go binds

schemeTypeHasExplicitBound :: ElabType -> Bool
schemeTypeHasExplicitBound schemeTy =
  let (binds, _) = Inst.splitForalls schemeTy
   in any (isJust . snd) binds

stripAnnExpr :: AnnExpr -> AnnExpr
stripAnnExpr annExpr =
  case annExpr of
    AAnn inner _ _ -> stripAnnExpr inner
    AUnfold inner _ _ -> stripAnnExpr inner
    _ -> annExpr

stripLeadingTyAbs :: ElabTerm -> ElabTerm
stripLeadingTyAbs term =
  case term of
    ETyAbs _ _ body -> stripLeadingTyAbs body
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

elabAlg :: AlgebraContext -> AnnExprF (AnnExpr, ElabOut) -> ElabOut
elabAlg algebraContext layer =
  case layer of
    AVarF v _ -> mkOut $ \env ->
      maybe (Left (EnvLookup v)) (const (Right (EVar v))) (Map.lookup v env)
    ALitF lit _ -> mkOut $ \_ -> Right (ELit lit)
    ALamF v paramNode _ (bodyAnn, bodyOut) lamNodeId ->
      let f env = do
            let mAnnLambda = desugaredAnnLambdaInfo v bodyAnn
                resolvedParam = algResolvedLambdaParamNode algebraContext lamNodeId
                isBareInternalTyVar ty =
                  case ty of
                    TVar name -> isJust (parseNameId name)
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
                              Just schemeInfo <- lookupSchemeInfo recurName env ->
                                case schemeToType (siScheme schemeInfo) of
                                  muTy@(TMu muName muBody)
                                    | hasContractiveRecursiveWitness muTy ->
                                        case substTypeCapture muName muTy muBody of
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
                      preservedTy <- srcTypeToElabType srcTy
                      pure
                        ( preservedTy,
                          SchemeInfo
                            { siScheme = schemeFromType preservedTy,
                              siSubst = IntMap.empty
                            }
                        )
                    Nothing ->
                      case generalizeAtNode scopeContext annNodeId of
                        Right (paramScheme, _subst) ->
                          let paramTy0 = case paramScheme of
                                Forall [(name, Just bnd)] bodyTy
                                  | bodyTy == TVar name -> tyToElab bnd
                                _ -> schemeToType paramScheme
                              -- If generalizeAtNode returned a bare TVar (over-generalized)
                              -- or a base type that disagrees with the constraint graph's
                              -- solved μ type, fall back to reifyNodeTypePreferringBound.
                              -- This handles the case where ELamAnn's desugared
                              -- annotation-let picks up the body's result type (e.g. Bool)
                              -- instead of the actual annotation type (e.g. μ Nat).
                              paramTyResolved = case paramTy0 of
                                TVar {} ->
                                  case reifyNodeTypePreferringBound scopeContext annNodeId of
                                    Right ty@TMu {} -> ty
                                    _ -> paramTy0
                                TBase {} ->
                                  case reifyNodeTypePreferringBound scopeContext annNodeId of
                                    Right ty@TMu {} -> ty
                                    _ -> paramTy0
                                _
                                  | TMu {} <- paramTySurface,
                                    Just unfoldedSurface <- unfoldMuOnce paramTySurface,
                                    (alphaEqType unfoldedSurface paramTy0 || churchAwareEqType unfoldedSurface paramTy0) ->
                                      paramTySurface
                                _ -> paramTy0
                           in pure
                                ( paramTyResolved,
                                  SchemeInfo
                                    { siScheme = schemeFromType paramTyResolved,
                                      siSubst = IntMap.empty
                                    }
                                )
                        Left (SchemeFreeVars _ _) ->
                          pure
                            ( paramTySurface,
                              SchemeInfo
                                { siScheme = schemeFromType paramTySurface,
                                  siSubst = IntMap.empty
                                }
                            )
                        Left err -> Left err
                Nothing ->
                  pure
                    ( paramTySurface,
                      SchemeInfo
                        { siScheme = schemeFromType paramTySurface,
                          siSubst = IntMap.empty
                        }
                    )
            let env' = Map.insert v (mkEnvBinding paramSchemeInfo False) env
                env'' =
                  Map.adjust
                    ( \binding ->
                        binding
                          { ebExplicitRecursiveParam =
                              isJust mAnnLambda && hasContractiveRecursiveWitness paramTy
                          }
                    )
                    v
                    env'
            bodyRaw <- elabTerm bodyElabOut env''
            let bodyTcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) (envSchemeInfos env'')) Map.empty
                body' = stripUnusedTopTyAbsWithEnv bodyTcEnv bodyRaw
            pure (ELam v paramTy body')
       in mkOut f
    AAppF (fAnn, fOut) (aAnn, aOut) funEid argEid appNodeId ->
      let f env = do
            f' <- elabTerm fOut env
            a' <- elabTerm aOut env
            argSourceSchemeTy <- sourceAnnotatedTypeFrom algebraContext env aAnn
            let schemeEnv = envSchemeInfos env
                tcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) schemeEnv) Map.empty
                appTargetTy =
                  let directTy = either (const Nothing) Just (reifyNodeTypeDirect scopeContext appNodeId)
                      boundTy = either (const Nothing) Just (reifyNodeTypePreferringBound scopeContext appNodeId)
                   in case directTy of
                        Just TVar {} -> boundTy <|> directTy
                        Just TBottom -> boundTy <|> directTy
                        Just directTy'
                          | Just boundMu@TMu {} <- boundTy,
                            Just unfoldedBound <- unfoldMuOnce boundMu,
                            let directNorm = stripVacuousForallsDeep directTy',
                            let unfoldedNorm = stripVacuousForallsDeep unfoldedBound,
                            (alphaEqType unfoldedNorm directNorm || churchAwareEqType unfoldedNorm directNorm) ->
                              boundTy
                        _ -> directTy
                annHasMuScheme ann =
                  case sourceVarName ann >>= (`lookupSchemeInfo` env) of
                    Just schemeInfo ->
                      case schemeToType (siScheme schemeInfo) of
                        TMu {} -> True
                        _ -> False
                    Nothing -> False
                argIsExplicitRecursiveParam ann =
                  case sourceVarName ann >>= (`Map.lookup` env) of
                    Just binding -> ebExplicitRecursiveParam binding
                    Nothing -> False
                sourceMuMatchesActualType sourceTy actualTy =
                  alphaEqType sourceTy actualTy
                    || churchAwareEqType sourceTy actualTy
                    || case sourceTy of
                      TMu {} ->
                        case unfoldMuOnce sourceTy of
                          Just unfoldedTy ->
                            let unfoldedTy' = stripVacuousForallsDeep unfoldedTy
                                actualTy' = stripVacuousForallsDeep actualTy
                             in alphaEqType unfoldedTy' actualTy' || churchAwareEqType unfoldedTy' actualTy'
                          Nothing -> False
                      _ -> False
                preferSourceMuArgTy actualTy =
                  case argSourceSchemeTy of
                    Just sourceTy@TMu {}
                      | sourceMuMatchesActualType sourceTy actualTy -> sourceTy
                    _ -> actualTy
                recoverIdentityLikeRecursiveFunInst ann =
                  case (sourceVarName ann, TypeCheck.typeCheckWithEnv tcEnv a') of
                    (Just fName, Right argTy)
                      | hasContractiveRecursiveWitness argTy ->
                          case lookupSchemeInfo fName env of
                            Just schemeInfo
                              | isSingleBinderIdentityScheme schemeInfo ->
                                  let candidate = InstApp argTy
                                      fAppCandidate = ETyInst f' candidate
                                   in case TypeCheck.typeCheckWithEnv tcEnv (EApp fAppCandidate a') of
                                        Right _ -> Just candidate
                                        Left _ -> Nothing
                            _ -> Nothing
                    _ -> Nothing
                reifyInstWithRecovery ann eid term
                  | Nothing <- sourceVarName ann,
                    Right ty <- TypeCheck.typeCheckWithEnv tcEnv term,
                    not (case ty of TForall {} -> True; _ -> False) =
                      Right InstId
                  | otherwise =
                      case reifyInst annotationContext namedSetReify schemeEnv ann eid of
                        Right inst -> Right inst
                        Left err@(PhiTranslatabilityError _)
                          | Just inst <- recoverIdentityLikeRecursiveFunInst ann -> Right inst
                          | annHasMuScheme ann -> Right InstId
                          | otherwise -> Left err
                        Left err -> Left err
                reifyInstIfPolymorphic ann eid term
                  | sourceAnnIsPolymorphic schemeEnv ann =
                      reifyInstWithRecovery ann eid term
                  | otherwise = Right InstId
                recursiveWitnessArgTerm =
                  case TypeCheck.typeCheckWithEnv tcEnv a' of
                    Right argTy
                      | hasContractiveRecursiveWitness argTy -> Just a'
                    _ ->
                      case sourceVarName aAnn >>= (`lookupSchemeInfo` env) of
                        Just argSchemeInfo
                          | hasContractiveRecursiveWitness (schemeToType (siScheme argSchemeInfo)) ->
                              Just a'
                        _ -> Nothing
                transparentOrIdentityBypassTerm =
                  case sourceVarName fAnn of
                    Just fName
                      | isTransparentMediatorVar fName env ->
                          let aStripped = stripUnusedTopTyAbs a'
                           in case aStripped of
                                ELam {} -> Just a'
                                _ -> recursiveWitnessArgTerm
                      | Just schemeInfo <- lookupSchemeInfo fName env,
                        isSingleBinderIdentityScheme schemeInfo ->
                          recursiveWitnessArgTerm
                    _ -> Nothing
            funInst <-
              case transparentOrIdentityBypassTerm of
                Just _ -> Right InstId
                Nothing -> reifyInstIfPolymorphic fAnn funEid f'
            argInst <-
              case transparentOrIdentityBypassTerm of
                Just _ -> Right InstId
                Nothing -> reifyInstIfPolymorphic aAnn argEid a'
            let fHeadTy = appHeadType tcEnv f'
                fHead = appHeadTerm tcEnv f'
                fIsMuHead =
                  case TypeCheck.typeCheckWithEnv tcEnv f' of
                    Right TMu {} -> True
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
                    (TypeCheck.typeCheckWithEnv tcEnv a')
                funInstByFunType =
                  case funInst of
                    inst0@(InstApp _) ->
                      case fHeadTy of
                        Just TForall {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    inst0@(InstInside (InstBot _)) ->
                      case fHeadTy of
                        Just TForall {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    inst0@(InstInside (InstApp _)) ->
                      case fHeadTy of
                        Just TForall {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    inst0@(InstSeq (InstInside (InstBot _)) InstElim) ->
                      case fHeadTy of
                        Just TForall {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    inst0@(InstSeq (InstInside (InstApp _)) InstElim) ->
                      case fHeadTy of
                        Just TForall {} -> inst0
                        Just _ -> InstId
                        Nothing -> inst0
                    _ -> funInst
                funInst' =
                  case recoveredArgTy of
                    recoveredArg ->
                      case funInstByFunType of
                        inst0@(InstApp ty0) ->
                          case ty0 of
                            TVar {} -> maybe inst0 InstApp recoveredArg
                            TForall {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        inst0@(InstSeq (InstInside (InstBot ty0)) InstElim) ->
                          case ty0 of
                            TVar {} -> maybe inst0 InstApp recoveredArg
                            TForall {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        inst0@(InstSeq (InstInside (InstApp ty0)) InstElim) ->
                          case ty0 of
                            TVar {} -> maybe inst0 InstApp recoveredArg
                            TForall {} -> maybe inst0 InstApp recoveredArg
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
                                Right (TForall _ (Just _) _) ->
                                  if isAppLikeInst instN
                                    then canonicalizeAppLikeInst instN
                                    else go (n + 1) (InstSeq instN InstElim)
                                Right (TForall _ Nothing _) ->
                                  case (instN, recoveredArgTy) of
                                    (InstId, Just argTy) -> InstApp argTy
                                    _ -> instN
                                Right _ -> instN
                                Left _ -> instN
                    Nothing -> inst0
                targetResultInstCandidates =
                  case (fIsMuHead, fHeadTy) of
                    (True, Just TForall {}) ->
                      [ InstApp (finalCodomain argTy)
                      | Just argTy <- [recoveredArgTy]
                      ]
                        ++ [ InstApp (finalCodomain targetTy)
                           | Just targetTy <- [appTargetTy]
                           ]
                    _ -> []
                validatesTargetResultInst instCandidate =
                  let fCandidate = ETyInst fHead instCandidate
                   in case TypeCheck.typeCheckWithEnv tcEnv (EApp fCandidate a') of
                        Right _ -> True
                        Left _ -> False
                funInstNorm0 = normalizeFunInst funInst'
                funInstNorm =
                  fromMaybe
                    funInstNorm0
                    (find validatesTargetResultInst targetResultInstCandidates)
                funInstRecovered =
                  let fApp0 = case funInstNorm of
                        InstId -> fHead
                        _ -> ETyInst fHead funInstNorm
                   in case ( TypeCheck.typeCheckWithEnv tcEnv (EApp fApp0 a'),
                             sourceVarName fAnn,
                             sourceVarName aAnn,
                             TypeCheck.typeCheckWithEnv tcEnv a'
                           ) of
                        (Right (TArrow _ TBottom), Just fName, mArgName, Right argTy) ->
                          case lookupSchemeInfo fName env of
                            Just fSchemeInfo ->
                              let argTyPreferred =
                                    case mArgName >>= (`lookupSchemeInfo` env) of
                                      Just argSchemeInfo ->
                                        case Inst.splitForalls (schemeToType (siScheme argSchemeInfo)) of
                                          ([], monoTy) -> monoTy
                                          _ -> argTy
                                      Nothing -> argTy
                                  (fBinds, fBodyTy) = Inst.splitForalls (schemeToType (siScheme fSchemeInfo))
                                  fBinderNames = map fst fBinds
                               in case fBodyTy of
                                    TArrow (TVar headBinder) retTy
                                      | headBinder `elem` fBinderNames
                                          && Set.member headBinder (freeTypeVarsType retTy) ->
                                          normalizeFunInst (InstApp argTyPreferred)
                                    _ -> funInstNorm
                            Nothing -> funInstNorm
                        (Left _, _, _, _) ->
                          fromMaybe funInstNorm $
                            case (sourceVarName fAnn, TypeCheck.typeCheckWithEnv tcEnv a') of
                              (Just fName, Right argTy)
                                | hasContractiveRecursiveWitness argTy ->
                                    case lookupSchemeInfo fName env of
                                      Just _
                                        | isTransparentMediatorVar fName env ->
                                            let candidate = normalizeFunInst (InstApp argTy)
                                                fAppCandidate = case candidate of
                                                  InstId -> fHead
                                                  _ -> ETyInst fHead candidate
                                             in case TypeCheck.typeCheckWithEnv tcEnv (EApp fAppCandidate a') of
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
                          keepCandidate =
                            case (isAppLikeInst instCandidate, fHeadTy, sourceVarName fAnn) of
                              (True, Just TForall {}, _) ->
                                case TypeCheck.typeCheckWithEnv tcEnv fCandidate of
                                  Right _ -> True
                                  Left _ -> False
                              (True, Nothing, Nothing) -> False
                              (True, _, _) -> False
                              _ ->
                                case TypeCheck.typeCheckWithEnv tcEnv fCandidate of
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
                                         in case TypeCheck.typeCheckWithEnv tcEnv fRecovered of
                                              Right _ -> recoveredCandidate
                                              Left _ -> InstId
                              Nothing -> InstId
                fAppForArgInference = case funInstValidated of
                  InstId -> fHead
                  _ -> ETyInst fHead funInstValidated
                firstClassPolymorphicArgInst =
                  case (sourceAnnIsPolymorphic schemeEnv aAnn, argSourceSchemeTy, TypeCheck.typeCheckWithEnv tcEnv fAppForArgInference) of
                    (True, Just sourceTy, Right (TArrow paramTy _))
                      | alphaEqType paramTy sourceTy || churchAwareEqType paramTy sourceTy,
                        Right _ <- TypeCheck.typeCheckWithEnv tcEnv (EApp fAppForArgInference a') ->
                          Just InstId
                    _ -> Nothing
                argInstFromFun =
                  let shouldInlineParamTy =
                        case (sourceVarName fAnn, sourceVarName aAnn) of
                          (Just fName, Just argName) -> fName /= argName
                          _ -> False
                      shouldInferArgInst =
                        case (sourceVarName fAnn, sourceVarName aAnn) of
                          (Just fName, Just argName) -> fName /= argName
                          _ -> True
                   in if not shouldInferArgInst
                        then Nothing
                        else case (sourceVarName aAnn, f') of
                          (Just vName, ELam _ paramTy _) -> do
                            schemeInfo <- lookupSchemeInfo vName env
                            let paramTy' =
                                  if shouldInlineParamTy
                                    then inlineBoundVarsType presolutionView paramTy
                                    else paramTy
                            args <- inferInstAppArgs (siScheme schemeInfo) paramTy'
                            pure (instSeqApps (map (inlineBoundVarsType presolutionView) args))
                          (Just vName, _) -> do
                            schemeInfo <- lookupSchemeInfo vName env
                            case TypeCheck.typeCheckWithEnv tcEnv fAppForArgInference of
                              Right (TArrow paramTy _) -> do
                                let paramTy' =
                                      if shouldInlineParamTy
                                        then inlineBoundVarsType presolutionView paramTy
                                        else paramTy
                                args <- inferInstAppArgs (siScheme schemeInfo) paramTy'
                                pure (instSeqApps (map (inlineBoundVarsType presolutionView) args))
                              _ -> Nothing
                          _ -> Nothing
                argInstFallback =
                  case (sourceVarName fAnn, sourceVarName aAnn, TypeCheck.typeCheckWithEnv tcEnv fAppForArgInference, argInst) of
                    (Just fName, Just argName, Right (TArrow paramTy _), InstApp argTy)
                      | fName == argName,
                        Just schemeInfo <- lookupSchemeInfo fName env,
                        case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate),
                        alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstInside (InstBot argTy))
                      | fName == argName,
                        Just schemeInfo <- lookupSchemeInfo fName env,
                        case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate),
                        alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstInside (InstApp argTy))
                      | fName == argName,
                        Just schemeInfo <- lookupSchemeInfo fName env,
                        case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate),
                        alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstSeq (InstInside (InstBot argTy)) InstElim)
                      | fName == argName,
                        Just schemeInfo <- lookupSchemeInfo fName env,
                        case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate),
                        alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstSeq (InstInside (InstApp argTy)) InstElim)
                      | fName == argName,
                        Just schemeInfo <- lookupSchemeInfo fName env,
                        case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False,
                        let instCandidate = InstApp argTy,
                        Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate),
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
                                  case TypeCheck.typeCheckWithEnv tcEnv a' of
                                    Right TForall {} -> canonicalizeAppLikeInst argInst'
                                    _ -> InstId
                              | otherwise ->
                                  case TypeCheck.typeCheckWithEnv tcEnv a' of
                                    Right (TForall _ (Just _) _) -> InstElim
                                    Right TForall {} -> argInst'
                                    _ -> InstId
                aApp =
                  case transparentOrIdentityBypassTerm of
                    Just bypassTerm -> bypassTerm
                    Nothing ->
                      case argInstFinal of
                        InstId -> a'
                        _ -> ETyInst a' argInstFinal
                fApp =
                  let fApp0 = case funInstValidated of
                        InstId -> fHead
                        _ -> ETyInst fHead funInstValidated
                      containsInternalTyVar ty =
                        case ty of
                          TVar name -> isJust (parseNameId name)
                          TArrow dom cod -> containsInternalTyVar dom || containsInternalTyVar cod
                          TCon _ args -> any containsInternalTyVar args
                          TForall _ mb body ->
                            maybe False containsInternalBoundTy mb || containsInternalTyVar body
                          TMu _ body -> containsInternalTyVar body
                          _ -> False
                      containsInternalBoundTy bound =
                        case bound of
                          TArrow dom cod -> containsInternalTyVar dom || containsInternalTyVar cod
                          TCon _ args -> any containsInternalTyVar args
                          TForall _ mb body ->
                            maybe False containsInternalBoundTy mb || containsInternalTyVar body
                          TMu _ body -> containsInternalTyVar body
                          _ -> False
                      isIdentityLambdaBody paramName body =
                        case body of
                          EVar bodyName -> bodyName == paramName
                          _ -> False
                   in case (TypeCheck.typeCheckWithEnv tcEnv (EApp fApp0 aApp), fApp0, sourceVarName aAnn, TypeCheck.typeCheckWithEnv tcEnv aApp) of
                        (Left _, ELam paramName paramTy body, Just argName, Right argTy)
                          | containsInternalTyVar paramTy
                              && isIdentityLambdaBody paramName body
                              && hasContractiveRecursiveWitness argTy
                              && maybe False (hasContractiveRecursiveWitness . schemeToType . siScheme) (lookupSchemeInfo argName env) ->
                              ELam paramName argTy body
                        _ -> fApp0
                bypassApp = transparentOrIdentityBypassTerm
            app0 <-
              case bypassApp of
                Just bypassTerm -> Right bypassTerm
                Nothing ->
                  insertMuUseSiteCoercions
                    tcEnv
                    (argIsExplicitRecursiveParam aAnn)
                    (isJust (sourceVarName aAnn))
                    argSourceSchemeTy
                    fApp
                    aApp
            let app = rollResultToExpectedMu tcEnv appTargetTy app0
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
                               TVar name -> isJust (parseNameId name)
                               TArrow dom cod -> go dom && go cod
                               TForall _ _ body -> go body
                               _ -> False
                        in go
                     ),
                   TypeCheck.typeCheckWithEnv tcEnv app
                 ) of
              (True, Left tcErr) ->
                if take (length "TCArgumentMismatch") (show tcErr) == "TCArgumentMismatch"
                  || take (length "TCExpectedArrow") (show tcErr) == "TCExpectedArrow"
                  then
                    Left
                      ( PhiTranslatabilityError
                          [ "AAppF: unresolved non-self polymorphic alias instantiation",
                            "function=" ++ show (sourceVarName fAnn),
                            "argument=" ++ show (sourceVarName aAnn),
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
                   in TMu muName (TArrow (TVar muName) codTy)
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
                   in TMu muName (TArrow (TVar muName) (TVar muName))
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
                          Right (siScheme aliasInfo, siSubst aliasInfo)
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
                    Right schemePair -> Right schemePair
                    Left err -> recoverGeneralizeAtNode err
            let lambdaParamNodes annExpr =
                  case annExpr of
                    ALam _ paramNode _ body _ -> paramNode : lambdaParamNodes body
                    AAnn inner _ _ -> lambdaParamNodes inner
                    AUnfold inner _ _ -> lambdaParamNodes inner
                    _ -> []
                deriveLambdaBinderSubst scheme0 subst0' =
                  let (binds, _) = Inst.splitForalls (schemeToType scheme0)
                      binderNames = map fst binds
                      binderBounds = map snd binds
                      paramNodes = lambdaParamNodes rhsAnn
                      binderPairs = zip binderNames paramNodes
                      canAugment =
                        length binderNames == length paramNodes
                          && all (== Nothing) binderBounds
                   in if canAugment
                        then
                          foldl'
                            ( \acc (name, paramNode) ->
                                let key = getNodeId (canonical paramNode)
                                 in IntMap.insertWith (\_ old -> old) key name acc
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
                              Right annTy@TMu {}
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
                            TForall _ _ inner -> stripForalls inner
                            other -> other
                        schemeBody = stripForalls (schemeToType schemeBase)
                        quantVars = Set.fromList [n | (n, _) <- fst (Inst.splitForalls (schemeToType schemeBase))]
                        isUnquantifiedTVar (TVar v') = not (Set.member v' quantVars)
                        isUnquantifiedTVar _ = False
                     in case schemeBody of
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
                      TMu _ (TArrow _ TBottom) -> True
                      _ -> False
                  isFixedPointRecursiveCarrier carrierTy =
                    case carrierTy of
                      TMu muName (TArrow (TVar domName) (TVar codName)) ->
                        muName == domName && muName == codName
                      _ -> False
                  hasInternalRecursiveCodomain carrierTy =
                    case carrierTy of
                      TMu muName (TArrow (TVar domName) codTy) ->
                        muName == domName && internalOnlyType codTy
                      _ -> False
                  internalOnlyType ty =
                    case ty of
                      TVar name -> isJust (parseNameId name)
                      TArrow dom cod -> internalOnlyType dom && internalOnlyType cod
                      TCon _ args -> not (null args) && all internalOnlyType args
                      TForall _ mb body ->
                        maybe True internalOnlyBound mb && internalOnlyType body
                      TMu _ body -> internalOnlyType body
                      _ -> False
                  internalOnlyBound bound =
                    case bound of
                      TArrow dom cod -> internalOnlyType dom && internalOnlyType cod
                      TCon _ args -> not (null args) && all internalOnlyType args
                      TForall _ mb body ->
                        maybe True internalOnlyBound mb && internalOnlyType body
                      TMu _ body -> internalOnlyType body
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
                      TForall _ _ bodyTy -> recursiveSelfAppResultTy bodyTy
                      TArrow _ codTy -> Just codTy
                      muTy@TMu {} ->
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
                    Just aliasInfo -> siSubst aliasInfo
                    Nothing ->
                      let (binds, _) = Inst.splitForalls (schemeToType scheme)
                       in if null binds then IntMap.empty else subst0
                schemeInfo =
                  freshenSchemeInfoAgainstEnv
                    env
                    SchemeInfo
                      { siScheme = scheme,
                        siSubst = subst
                      }
                transparentMediator =
                  isTransparentMediatorAnn rhsAnn
                    || maybe False (`isTransparentMediatorVar` env) aliasSourceName
                envBindingFor bindingSchemeInfo =
                  case aliasSourceName of
                    Just sourceName ->
                      (mkEnvBinding bindingSchemeInfo transparentMediator)
                        { ebAliasTarget = Just (resolveAliasVar env sourceName)
                        }
                    Nothing -> mkEnvBinding bindingSchemeInfo transparentMediator
                tcEnvBase = TypeCheck.Env (Map.map (schemeToType . siScheme) (envSchemeInfos env)) Map.empty
                authoritativeSourceSchemeInfo =
                  case rhsOuterSourceScheme <|> explicitRhsSourceScheme <|> letResultSourceScheme <|> schemeRootSourceScheme of
                    Just (schemeSrc, substSrc) ->
                      Just
                        ( freshenSchemeInfoAgainstEnv
                            env
                            SchemeInfo
                              { siScheme = schemeSrc,
                                siSubst = substSrc
                              }
                        )
                    Nothing -> Nothing
                envSchemeInfoForRhs = fromMaybe schemeInfo authoritativeSourceSchemeInfo
                env' = Map.insert v (envBindingFor envSchemeInfoForRhs) env
                tcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) (envSchemeInfos env')) Map.empty
            rhs' <- elabTerm rhsOut env'
            let closeFreeVarsToScheme ty =
                  let (binds, body) = Inst.splitForalls ty
                      boundNames = Set.fromList (map fst binds)
                      extraBinds =
                        [ (name, Nothing)
                          | name <- freeTypeVarsInOccurrenceOrder body,
                            Set.notMember name boundNames
                        ]
                   in Forall (binds ++ extraBinds) body
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
                    ELam paramName paramTy body ->
                      let (params, core) = collectLeadingLambdaParams body
                       in ((paramName, paramTy) : params, core)
                    ELet _ (Forall [] _) (EVar _) body ->
                      collectLeadingLambdaParams body
                    _ -> ([], term)
                collapsedIdentityWrapperScheme ty =
                  case Inst.splitForalls ty of
                    (_, TArrow _ codTy) ->
                      codTy == TBottom || containsInternalTypeVar codTy
                    _ -> False
                rebuildTransparentMediatorTerm rootName etaParams resultTy =
                  let rootParamTy = foldr TArrow resultTy (map snd etaParams)
                      mediatorBody =
                        foldr
                          (\(paramName, paramTy) acc -> ELam paramName paramTy acc)
                          (foldl' EApp (EVar rootName) (map (EVar . fst) etaParams))
                          etaParams
                   in (rootParamTy, ELam rootName rootParamTy mediatorBody)
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
                      ELam _ rootParamTy body ->
                        let (etaParams, _core) = collectLeadingLambdaParams body
                            etaParamTys = map snd etaParams
                         in case splitArrowN (length etaParams) rootParamTy of
                              Just (_expectedEtaParamTys, resultTy)
                                | not (null etaParams),
                                  let (structuralRootParamTy, structuralMediatorTerm) =
                                        rebuildTransparentMediatorTerm v etaParams resultTy,
                                  let rhsScheme =
                                        closeFreeVarsToScheme
                                          (TArrow structuralRootParamTy (foldr TArrow resultTy etaParamTys)),
                                  let candidateSubst =
                                        case Inst.splitForalls (schemeToType rhsScheme) of
                                          ([], _) -> IntMap.empty
                                          _ -> normalizeSubstForScheme rhsScheme subst,
                                  let rhsClosed =
                                        closeTermWithSchemeSubstIfNeeded
                                          candidateSubst
                                          rhsScheme
                                          structuralMediatorTerm,
                                  let candidateSchemeAdmitsRhs =
                                        case TypeCheck.typeCheckWithEnv tcEnvBase rhsClosed of
                                          Right rhsTy -> alphaEqType rhsTy (schemeToType rhsScheme)
                                          Left _ -> False,
                                  candidateSchemeAdmitsRhs
                                    || containsInternalTypeVar (schemeToType scheme)
                                    || schemeHasForwardBoundReference (schemeToType scheme)
                                    || not (alphaEqType (schemeToType scheme) (schemeToType rhsScheme)) ->
                                    Just
                                      ( structuralMediatorTerm,
                                        SchemeInfo
                                          { siScheme = rhsScheme,
                                            siSubst = candidateSubst
                                          }
                                      )
                              _ -> Nothing
                      _ -> Nothing
                    else Nothing
                rhsIdentityWrapperOverride =
                  case (stripAnnExpr rhsAnn, rhsTransparentMediatorTerm) of
                    (ALam rootParam _ _ body _, ELam rootName rootParamTy _)
                      | rootParam == rootName,
                        identityWrapperBody rootParam Map.empty body ->
                          let rhsTerm = ELam rootName rootParamTy (EVar rootName)
                              rhsScheme = closeFreeVarsToScheme (TArrow rootParamTy rootParamTy)
                              candidateSubst =
                                case Inst.splitForalls (schemeToType rhsScheme) of
                                  ([], _) -> IntMap.empty
                                  _ -> normalizeSubstForScheme rhsScheme subst
                              rhsClosed =
                                closeTermWithSchemeSubstIfNeeded candidateSubst rhsScheme rhsTerm
                              candidateSchemeAdmitsRhs =
                                case TypeCheck.typeCheckWithEnv tcEnvBase rhsClosed of
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
                                      SchemeInfo
                                        { siScheme = rhsScheme,
                                          siSubst = candidateSubst
                                        }
                                    )
                                else Nothing
                    _ -> Nothing
                rhsAliasOverride =
                  case (rhsAliasTerm, TypeCheck.typeCheckWithEnv tcEnvBase rhsAliasTerm) of
                    (EVar _, Right rhsTy)
                      | not (alphaEqType rhsTy (schemeToType scheme)) ->
                          let rhsScheme = closeFreeVarsToScheme rhsTy
                              rhsSubst =
                                case Inst.splitForalls rhsTy of
                                  ([], _) -> IntMap.empty
                                  _ -> subst
                           in Just (rhsAliasTerm, SchemeInfo {siScheme = rhsScheme, siSubst = rhsSubst})
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
                authoritativeEnvSchemeInfo =
                  fromMaybe
                    (freshenSchemeInfoAgainstEnv env schemeInfo)
                    authoritativeSourceSchemeInfo
                effectiveScheme = siScheme effectiveSchemeInfo
                effectiveSubst = siSubst effectiveSchemeInfo
                envSchemeInfoForBody =
                  if isJust rhsOuterSourceScheme
                    || isJust explicitRhsSourceScheme
                    || isJust letResultSourceScheme
                    || isJust schemeRootSourceScheme
                    then authoritativeEnvSchemeInfo
                    else effectiveSchemeInfo
                envForBody = Map.insert v (envBindingFor envSchemeInfoForBody) env
                tcEnvForBody = TypeCheck.Env (Map.map (schemeToType . siScheme) (envSchemeInfos envForBody)) Map.empty
            let rhsAbs0 =
                  let schemeTy = schemeToType effectiveScheme
                      rhsMatchesScheme rhsTy =
                        alphaEqType rhsTy schemeTy
                          || case schemeTy of
                            muTy@(TMu muName muBody) ->
                              let expectedBodyTy = substTypeCapture muName muTy muBody
                               in alphaEqType rhsTy expectedBodyTy
                            _ -> False
                   in case (effectiveRhsTerm, TypeCheck.typeCheckWithEnv tcEnv effectiveRhsTerm) of
                        (EVar _, _) ->
                          closeTermWithSchemeSubstIfNeeded effectiveSubst effectiveScheme effectiveRhsTerm
                        (_, Right rhsTy)
                          | rhsMatchesScheme rhsTy -> effectiveRhsTerm
                        _ -> closeTermWithSchemeSubstIfNeeded effectiveSubst effectiveScheme effectiveRhsTerm
                rhsAbs =
                  let schemeTy = schemeToType effectiveScheme
                      rhsAbsBase =
                        case TypeCheck.typeCheckWithEnv tcEnv rhsAbs0 of
                          rhsAbs0Ty ->
                            case effectiveScheme of
                              Forall binds _
                                | not (null binds),
                                  Right rhsTy <- rhsAbs0Ty,
                                  alphaEqType rhsTy schemeTy ->
                                    rhsAbs0
                                | not (null binds) ->
                                    case case (rhsAbs0, rhsAbs0Ty) of
                                      (ETyAbs _ _ body, Right (TForall _ _ bodyTy))
                                        | alphaEqType bodyTy schemeTy ->
                                            body
                                      _ -> stripUnusedTopTyAbsWithEnv tcEnv rhsAbs0 of
                                      rhsAbsCandidate ->
                                        case TypeCheck.typeCheckWithEnv tcEnv rhsAbsCandidate of
                                          Right rhsTy
                                            | alphaEqType rhsTy schemeTy ->
                                                rhsAbsCandidate
                                          _ ->
                                            case rhsAbs0Ty of
                                              Left _ -> rhsAbsCandidate
                                              _ -> rhsAbs0
                              _ ->
                                case (rhsAbs0, rhsAbs0Ty) of
                                  (ETyAbs _ _ body, Right (TForall _ _ bodyTy))
                                    | alphaEqType bodyTy schemeTy ->
                                        body
                                  _ -> stripUnusedTopTyAbsWithEnv tcEnv rhsAbs0
                      rhsAbsAligned =
                        let aligned = alignLeadingLambdasToType schemeTy rhsAbsBase
                         in if v == "_"
                              then
                                let stripped = stripUnusedTopTyAbsWithEnv tcEnv aligned
                                 in case TypeCheck.typeCheckWithEnv tcEnv stripped of
                                      Right _ -> stripped
                                      Left _ -> aligned
                              else aligned
                   in case TypeCheck.typeCheckWithEnv tcEnv rhsAbsBase of
                        Right rhsTy
                          | alphaEqType rhsTy schemeTy -> rhsAbsBase
                        _ ->
                          case TypeCheck.typeCheckWithEnv tcEnv rhsAbsAligned of
                            Right rhsTy
                              | alphaEqType rhsTy schemeTy -> rhsAbsAligned
                            _ -> rhsAbsBase
                rhsAbsTyChecked = TypeCheck.typeCheckWithEnv tcEnv rhsAbs
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
            let rhsForRoll =
                  case schemeToType effectiveScheme of
                    muTy@(TMu muName muBody) ->
                      let expectedBodyTy = substTypeCapture muName muTy muBody
                          rhsRollAligned = alignLeadingLambdasToType expectedBodyTy rhsAbs
                       in case TypeCheck.typeCheckWithEnv tcEnvForBody effectiveRhsTerm of
                            Right rhsTy
                              | alphaEqType rhsTy expectedBodyTy -> effectiveRhsTerm
                            _ ->
                              case TypeCheck.typeCheckWithEnv tcEnvForBody rhsAbs of
                                Right rhsTy
                                  | alphaEqType rhsTy expectedBodyTy -> rhsAbs
                                _ ->
                                  case TypeCheck.typeCheckWithEnv tcEnvForBody rhsRollAligned of
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
                    Just (_, aliasInfo) -> Map.insert v (envBindingFor aliasInfo) env
                    Nothing ->
                      Map.insert
                        v
                        ( envBindingFor
                            ( case (effectiveRhsTerm, TypeCheck.typeCheckWithEnv tcEnv effectiveRhsTerm) of
                                (EVar _, Right rhsTy)
                                  | not (alphaEqType rhsTy (schemeToType effectiveScheme)) ->
                                      SchemeInfo
                                        { siScheme = schemeFromType rhsTy,
                                          siSubst = effectiveSubst
                                        }
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
                        muTy@TMu {} ->
                          case TypeCheck.typeCheckWithEnv tcEnvForBody effectiveRhsTerm of
                            Right rhsTy
                              | alphaEqType rhsTy muTy -> effectiveRhsTerm
                            _ ->
                              case TypeCheck.typeCheckWithEnv tcEnvForBody rhsAbs of
                                Right rhsTy
                                  | alphaEqType rhsTy muTy -> rhsAbs
                                _ -> ERoll muTy rhsForRoll
                        _ -> rhsAbs
            let schemeFinal =
                  case TypeCheck.typeCheckWithEnv tcEnvForBody rhsFinal of
                    Right rhsTy
                      | v == "_" ->
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
            pure (schemeFinal, rhsFinal, body')
          unusedIdentityWrapperBinding =
            not (annContainsVar v bodyAnn) && identityWrapperAnn rhsAnn
          f env =
            if unusedIdentityWrapperBinding
              then elabTerm bodyOut env
              else do
                (scheme, rhsFinal, body') <- elaborateLet env
                if isJust (sourceVarName rhsAnn) && not (containsFreeVar v body')
                  then pure body'
                  else pure (ELet v scheme rhsFinal body')
          fStripped env =
            if unusedIdentityWrapperBinding
              then elabTerm bodyOut env
              else do
                (scheme, rhsFinal, body') <- elaborateLet env
                if isJust (sourceVarName rhsAnn) && not (containsFreeVar v body')
                  then pure body'
                  else
                    if containsFreeVar v rhsFinal
                      then pure (ELet v scheme rhsFinal body')
                      else pure body'
       in ElabOut
            { elabTerm = f,
              elabStripped = fStripped
            }
    AAnnF (exprAnn, exprOut) annNodeId eid ->
      ElabOut
        { elabTerm = \env -> do
            expr' <- elabTerm exprOut env
            elaborateAnnotationTerm annotationContext namedSetReify (envSchemeInfos env) exprAnn annNodeId eid expr',
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
    presolutionView = algPresolutionView algebraContext
    canonical = algCanonical algebraContext
    namedSetReify = algNamedSetReify algebraContext

    inferInstAppArgs scheme targetTy =
      let (binds, body) = Inst.splitForalls (schemeToType scheme)
       in inferInstAppArgsFromScheme binds body targetTy

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

       appHeadTerm:  wraps the term in EUnroll when its type is a TMu whose
                     unfolding eventually reaches TForall or TArrow.
       appHeadType:  returns the type of appHeadTerm — i.e. the unfolded μ body.

       These helpers allow the existing InstApp/InstElim machinery in AAppF to
       work transparently on Church-encoded eliminators without duplicating
       instantiation logic inside insertMuUseSiteCoercions. -}
    appHeadTerm :: TypeCheck.Env -> ElabTerm -> ElabTerm
    appHeadTerm tcEnv term =
      case TypeCheck.typeCheckWithEnv tcEnv term of
        Right TMu {} -> EUnroll term
        _ -> term

    appHeadType :: TypeCheck.Env -> ElabTerm -> Maybe ElabType
    appHeadType tcEnv term =
      case TypeCheck.typeCheckWithEnv tcEnv (appHeadTerm tcEnv term) of
        Right ty -> Just ty
        Left _ -> Nothing

    finalCodomain :: ElabType -> ElabType
    finalCodomain = go . peelLeadingForalls
      where
        peelLeadingForalls ty =
          case ty of
            TForall _ _ body -> peelLeadingForalls body
            _ -> ty
        go ty =
          case ty of
            TArrow _ cod -> go cod
            _ -> ty

    insertMuUseSiteCoercions :: TypeCheck.Env -> Bool -> Bool -> Maybe ElabType -> ElabTerm -> ElabTerm -> Either ElabError ElabTerm
    insertMuUseSiteCoercions tcEnv preserveRecursiveArg sourceArgIsVar mbArgSourceTy fTerm aTerm = do
      let (fUnrolled, unfoldedFromMu) =
            case TypeCheck.typeCheckWithEnv tcEnv fTerm of
              Right muTy@TMu {} ->
                case unfoldMuOnce muTy of
                  Just TArrow {} -> (EUnroll fTerm, True)
                  Just TForall {} -> (EUnroll fTerm, True)
                  _ -> (fTerm, False)
              _ -> (fTerm, False)
      {- Note [Instantiate leading ∀ after μ-unfold]
         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         Church-encoded ADTs unfold to ∀result. arrow → ... → result.
         After EUnroll the type is TForall, not TArrow, so we must
         instantiate the leading quantifier before applying arguments.
         We infer the instantiation type from the argument's type. -}
      let peelLeadingUnboundedForalls inst0 =
            let go n instN
                  | n >= (8 :: Int) = instN
                  | otherwise =
                      case TypeCheck.typeCheckWithEnv tcEnv (ETyInst fUnrolled instN) of
                        Right (TForall _ Nothing _) -> instN
                        _ -> instN
             in go 0 inst0
          validatedForVarArg instN =
            let candidate =
                  case instN of
                    InstId -> fUnrolled
                    _ -> ETyInst fUnrolled instN
             in if sourceArgIsVar
                  then case TypeCheck.typeCheckWithEnv tcEnv candidate of
                    Right TArrow {} -> candidate
                    _ -> fUnrolled
                  else candidate
          fInstantiated =
            case (unfoldedFromMu, TypeCheck.typeCheckWithEnv tcEnv fUnrolled) of
              (True, Right (TForall _v Nothing _arrowBody)) ->
                case TypeCheck.typeCheckWithEnv tcEnv aTerm of
                  Right argTy ->
                    let sourceMuMatchesActual sourceTy =
                          alphaEqType sourceTy argTy
                            || churchAwareEqType sourceTy argTy
                            || case sourceTy of
                              TMu {} ->
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
                            (_, Just sourceTy@TMu {})
                              | sourceMuMatchesActual sourceTy -> sourceTy
                            _ -> argTy
                        inst0 = InstApp instArgTy
                     in case peelLeadingUnboundedForalls inst0 of
                          instN -> validatedForVarArg instN
                  Left _ -> fUnrolled
              (_, Right _) ->
                case peelLeadingUnboundedForalls InstId of
                  instN -> validatedForVarArg instN
              _ -> fUnrolled
      aCoerced <-
        case TypeCheck.typeCheckWithEnv tcEnv fInstantiated of
          Right (TArrow paramTy _resTy)
            | preserveRecursiveArg,
              TMu {} <- paramTy,
              Right argTy <- TypeCheck.typeCheckWithEnv tcEnv aTerm,
              (alphaEqType argTy paramTy || churchAwareEqType argTy paramTy) ->
                Right aTerm
            | otherwise -> coerceArgForParam tcEnv sourceArgIsVar mbArgSourceTy paramTy aTerm
          _ -> Right aTerm
      pure (EApp fInstantiated aCoerced)

    unfoldMuOnce :: ElabType -> Maybe ElabType
    unfoldMuOnce muTy =
      case muTy of
        TMu name body -> Just (substTypeCapture name muTy body)
        _ -> Nothing

    peelLeadingUnboundedForallsType :: ElabType -> ElabType
    peelLeadingUnboundedForallsType ty =
      case ty of
        TForall _ Nothing body -> peelLeadingUnboundedForallsType body
        _ -> ty

    coerceArgForParam :: TypeCheck.Env -> Bool -> Maybe ElabType -> ElabType -> ElabTerm -> Either ElabError ElabTerm
    coerceArgForParam tcEnv sourceArgIsVar mbArgSourceTy paramTy argTerm =
      case TypeCheck.typeCheckWithEnv tcEnv argTerm of
        Left _ -> Right argTerm
        Right argTy ->
          case paramTy of
            TVar _
              | Just muTy@TMu {} <- mbArgSourceTy ->
                  case unfoldMuOnce muTy of
                    Just unfoldedTy
                      | alphaEqType unfoldedTy argTy || churchAwareEqType unfoldedTy argTy -> Right (ERoll muTy argTerm)
                      | otherwise ->
                          let argAligned = alignLeadingLambdasToType unfoldedTy argTerm
                              argStripped = stripUnusedTyAbsAlongType tcEnv unfoldedTy argAligned
                              argRebuilt = rebuildRecursiveArgAlongType tcEnv unfoldedTy argTerm
                           in case TypeCheck.typeCheckWithEnv tcEnv argStripped of
                                Right argTy'
                                  | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (ERoll muTy argStripped)
                                _ ->
                                  case TypeCheck.typeCheckWithEnv tcEnv argRebuilt of
                                    Right argTy'
                                      | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (ERoll muTy argRebuilt)
                                    _ -> Right argTerm
                    _ -> Right argTerm
            muTy@TMu {} ->
              let sourceMatchesMu =
                    maybe
                      False
                      (\sourceTy -> alphaEqType sourceTy muTy || churchAwareEqType sourceTy muTy)
                      mbArgSourceTy
                      && sourceArgIsVar
                  actualMatchesMu = alphaEqType argTy muTy || churchAwareEqType argTy muTy
                  fallbackMuVar = Right argTerm
                  coerceMuFromUnfolded muTy0 argTy0 =
                    case unfoldMuOnce muTy0 of
                      Just unfoldedTy
                        | alphaEqType unfoldedTy argTy0 || churchAwareEqType unfoldedTy argTy0 -> Right (ERoll muTy0 argTerm)
                        | otherwise ->
                            let argAligned = alignLeadingLambdasToType unfoldedTy argTerm
                                argStripped = stripUnusedTyAbsAlongType tcEnv unfoldedTy argAligned
                                argRebuilt = rebuildRecursiveArgAlongType tcEnv unfoldedTy argTerm
                             in case TypeCheck.typeCheckWithEnv tcEnv argStripped of
                                  Right argTy'
                                    | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (ERoll muTy0 argStripped)
                                  _ ->
                                    case TypeCheck.typeCheckWithEnv tcEnv argRebuilt of
                                      Right argTy'
                                        | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (ERoll muTy0 argRebuilt)
                                      _ -> fallbackMuVar
                      _ | shouldRollMuVar muTy0 argTy0 -> Right (ERoll muTy0 argTerm)
                      _ -> fallbackMuVar
               in if sourceMatchesMu && actualMatchesMu
                    then Right argTerm
                    else
                      if sourceMatchesMu
                        then case argTy of
                          TMu {} -> Right (ERoll muTy (EUnroll argTerm))
                          _
                            | Just unfoldedTy <- unfoldMuOnce muTy,
                              let unfoldedTyPeeled = peelLeadingUnboundedForallsType unfoldedTy,
                              alphaEqType unfoldedTyPeeled argTy || churchAwareEqType unfoldedTyPeeled argTy ->
                                let argRebuilt = rebuildRecursiveArgAlongType tcEnv unfoldedTy argTerm
                                 in case TypeCheck.typeCheckWithEnv tcEnv argRebuilt of
                                      Right argTy'
                                        | alphaEqType unfoldedTy argTy' || churchAwareEqType unfoldedTy argTy' -> Right (ERoll muTy argRebuilt)
                                      _ -> coerceMuFromUnfolded muTy argTy
                          _ -> coerceMuFromUnfolded muTy argTy
                        else case argTy of
                          argMu@TMu {}
                            | alphaEqType argMu muTy || churchAwareEqType argMu muTy ->
                                Right argTerm
                            | otherwise ->
                                case (unfoldMuOnce muTy, unfoldMuOnce argMu) of
                                  (Just expectedBodyTy, Just argBodyTy)
                                    | alphaEqType expectedBodyTy argBodyTy || churchAwareEqType expectedBodyTy argBodyTy ->
                                        Right (ERoll muTy (EUnroll argTerm))
                                    | otherwise ->
                                        let argUnrolled = EUnroll argTerm
                                            argAligned = alignLeadingLambdasToType expectedBodyTy argUnrolled
                                            argStripped = stripUnusedTyAbsAlongType tcEnv expectedBodyTy argAligned
                                            argRebuilt = rebuildRecursiveArgAlongType tcEnv expectedBodyTy argUnrolled
                                         in case TypeCheck.typeCheckWithEnv tcEnv argStripped of
                                              Right argTy'
                                                | alphaEqType expectedBodyTy argTy' || churchAwareEqType expectedBodyTy argTy' ->
                                                    Right (ERoll muTy argStripped)
                                              _ ->
                                                case TypeCheck.typeCheckWithEnv tcEnv argRebuilt of
                                                  Right argTy'
                                                    | alphaEqType expectedBodyTy argTy' || churchAwareEqType expectedBodyTy argTy' ->
                                                        Right (ERoll muTy argRebuilt)
                                                  _ -> coerceMuFromUnfolded muTy argTy
                                  _ -> coerceMuFromUnfolded muTy argTy
                          _ -> coerceMuFromUnfolded muTy argTy
            _ ->
              case argTy of
                muTy@TMu {} ->
                  case unfoldMuOnce muTy of
                    Just unfoldedTy
                      | alphaEqType paramTy unfoldedTy || churchAwareEqType paramTy unfoldedTy -> Right (EUnroll argTerm)
                    _ -> Right argTerm
                _ -> Right argTerm

    rollResultToExpectedMu :: TypeCheck.Env -> Maybe ElabType -> ElabTerm -> ElabTerm
    rollResultToExpectedMu tcEnv mbTargetTy term =
      case mbTargetTy of
        Just muTy@TMu {} ->
          case TypeCheck.typeCheckWithEnv tcEnv term of
            Right termTy
              | alphaEqType termTy muTy -> term
              | otherwise,
                actualMu@(TMu actualName actualBody) <- termTy,
                churchAwareEqType termTy muTy ->
                  case unfoldMuOnce muTy of
                    Just expectedBodyTy ->
                      let actualBodyTy = stripVacuousForallsDeep (substTypeCapture actualName actualMu actualBody)
                       in if alphaEqType actualBodyTy expectedBodyTy || churchAwareEqType actualBodyTy expectedBodyTy
                            then ERoll muTy (EUnroll term)
                            else term
                    _ -> term
              | otherwise ->
                  case unfoldMuOnce muTy of
                    Just unfoldedTy
                      | alphaEqType termTy unfoldedTy || churchAwareEqType termTy unfoldedTy ->
                          ERoll muTy term
                      | otherwise ->
                          let termAligned = alignLeadingLambdasToType unfoldedTy term
                              termStripped = stripUnusedTyAbsAlongType tcEnv unfoldedTy termAligned
                              termRebuilt = rebuildRecursiveArgAlongType tcEnv unfoldedTy term
                           in case TypeCheck.typeCheckWithEnv tcEnv termStripped of
                                Right termTy'
                                  | alphaEqType termTy' unfoldedTy || churchAwareEqType termTy' unfoldedTy ->
                                      ERoll muTy termStripped
                                _ ->
                                  case TypeCheck.typeCheckWithEnv tcEnv termRebuilt of
                                    Right termTy'
                                      | alphaEqType termTy' unfoldedTy || churchAwareEqType termTy' unfoldedTy ->
                                          ERoll muTy termRebuilt
                                    _ -> term
                    _ -> term
            Left _ -> term
        _ -> term

    stripVacuousForallsDeep :: ElabType -> ElabType
    stripVacuousForallsDeep ty = case ty of
      TForall name Nothing body
        | not (Set.member name (freeTypeVarsType body)) ->
            stripVacuousForallsDeep body
      TForall name mb body ->
        TForall name (fmap stripVacuousForallsDeepBound mb) (stripVacuousForallsDeep body)
      TArrow dom cod -> TArrow (stripVacuousForallsDeep dom) (stripVacuousForallsDeep cod)
      TCon con args -> TCon con (fmap stripVacuousForallsDeep args)
      TMu name body -> TMu name (stripVacuousForallsDeep body)
      _ -> ty

    stripVacuousForallsDeepBound :: BoundType -> BoundType
    stripVacuousForallsDeepBound bound = case bound of
      TArrow dom cod -> TArrow (stripVacuousForallsDeep dom) (stripVacuousForallsDeep cod)
      TBase _ -> bound
      TCon con args -> TCon con (fmap stripVacuousForallsDeep args)
      TForall name mb body -> TForall name (fmap stripVacuousForallsDeepBound mb) (stripVacuousForallsDeep body)
      TMu name body -> TMu name (stripVacuousForallsDeep body)
      TBottom -> TBottom

    containsFreeVar :: VarName -> ElabTerm -> Bool
    containsFreeVar v term =
      case term of
        EVar x -> x == v
        ELit _ -> False
        ELam x _ body
          | x == v -> False
          | otherwise -> containsFreeVar v body
        EApp f a -> containsFreeVar v f || containsFreeVar v a
        ELet x _ rhs body
          | x == v -> containsFreeVar v rhs
          | otherwise -> containsFreeVar v rhs || containsFreeVar v body
        ETyAbs _ _ body -> containsFreeVar v body
        ETyInst e _ -> containsFreeVar v e
        ERoll _ body -> containsFreeVar v body
        EUnroll e -> containsFreeVar v e

    alignLeadingLambdasToType :: ElabType -> ElabTerm -> ElabTerm
    alignLeadingLambdasToType ty term =
      case (ty, term) of
        (TForall targetName _ bodyTy, ETyAbs v mb body) ->
          let bodyTy' = substTypeCapture targetName (TVar v) bodyTy
           in ETyAbs v mb (alignLeadingLambdasToType bodyTy' body)
        (TArrow dom cod, ELam v _ body) ->
          ELam v dom (alignLeadingLambdasToType cod body)
        _ -> term

    stripUnusedTopTyAbsWithEnv :: TypeCheck.Env -> ElabTerm -> ElabTerm
    stripUnusedTopTyAbsWithEnv tcEnv term =
      case term of
        ETyAbs v mbBound body ->
          let body' = stripUnusedTopTyAbsWithEnv tcEnv body
              term' = ETyAbs v mbBound body'
           in case TypeCheck.typeCheckWithEnv tcEnv term' of
                Right (TForall _ _ bodyTy)
                  | v `notElem` freeTypeVarsType bodyTy -> body'
                _ -> term'
        _ -> term

    stripUnusedTyAbsAlongType :: TypeCheck.Env -> ElabType -> ElabTerm -> ElabTerm
    stripUnusedTyAbsAlongType tcEnv targetTy term =
      let term' = stripUnusedTopTyAbsWithEnv tcEnv term
       in case (targetTy, term') of
            (TForall targetName _ targetBody, ETyAbs termName mbBound body)
              | targetName == termName ->
                  ETyAbs termName mbBound (stripUnusedTyAbsAlongType tcEnv targetBody body)
            (TArrow dom cod, ELam name _ body) ->
              ELam name dom (stripUnusedTyAbsAlongType tcEnv cod body)
            _ -> term'

    hoistFloatingTyAbsThroughLambdas :: ElabTerm -> ElabTerm
    hoistFloatingTyAbsThroughLambdas term =
      case term of
        ELam name ty body ->
          let body' = hoistFloatingTyAbsThroughLambdas body
           in case body' of
                ETyAbs tyName mbBound inner
                  | tyName `Set.notMember` freeTypeVarsType ty ->
                      ETyAbs tyName mbBound (hoistFloatingTyAbsThroughLambdas (ELam name ty inner))
                _ -> ELam name ty body'
        EApp fun arg -> EApp (hoistFloatingTyAbsThroughLambdas fun) (hoistFloatingTyAbsThroughLambdas arg)
        ELet name sch rhs body ->
          ELet name sch (hoistFloatingTyAbsThroughLambdas rhs) (hoistFloatingTyAbsThroughLambdas body)
        ETyAbs name mbBound body -> ETyAbs name mbBound (hoistFloatingTyAbsThroughLambdas body)
        ETyInst body inst -> ETyInst (hoistFloatingTyAbsThroughLambdas body) inst
        ERoll ty body -> ERoll ty (hoistFloatingTyAbsThroughLambdas body)
        EUnroll body -> EUnroll (hoistFloatingTyAbsThroughLambdas body)
        _ -> term

    addMissingLeadingTyAbsAlongType :: TypeCheck.Env -> ElabType -> ElabTerm -> ElabTerm
    addMissingLeadingTyAbsAlongType tcEnv targetTy term =
      let initialReserved =
            Set.unions
              ( Set.union (typeAbsNamesInTerm term) (typeVarNamesInTerm term)
                  : map freeTypeVarsType (Map.elems (TypeCheck.termEnv tcEnv))
                  ++ [Set.fromList (Map.keys (TypeCheck.typeEnv tcEnv)), forallBinderNames targetTy]
              )
       in go initialReserved targetTy term
      where
        go reserved targetTy' term' =
          case targetTy' of
            TForall targetName mbBound targetBody ->
              case stripUnusedTopTyAbsWithEnv tcEnv term' of
                ETyAbs termName termBound body
                  | targetName == termName ->
                      ETyAbs termName termBound (go (Set.insert termName reserved) targetBody body)
                term'' ->
                  let (targetName', targetBody') =
                        if Set.member targetName reserved
                          then
                            let fresh = freshNameLike targetName reserved
                             in (fresh, substTypeCapture targetName (TVar fresh) targetBody)
                          else (targetName, targetBody)
                      reserved' = Set.insert targetName' reserved
                      body' = go reserved' targetBody' term''
                   in ETyAbs targetName' mbBound body'
            TArrow dom cod ->
              case term' of
                ELam name _ body -> ELam name dom (go reserved cod body)
                _ -> term'
            _ -> term'

        forallBinderNames ty =
          case ty of
            TForall name _ body -> Set.insert name (forallBinderNames body)
            _ -> Set.empty

    typeAbsNamesInTerm :: ElabTerm -> Set.Set String
    typeAbsNamesInTerm term =
      case term of
        ETyAbs name _ body -> Set.insert name (typeAbsNamesInTerm body)
        ELam _ _ body -> typeAbsNamesInTerm body
        EApp f a -> Set.union (typeAbsNamesInTerm f) (typeAbsNamesInTerm a)
        ELet _ _ rhs body -> Set.union (typeAbsNamesInTerm rhs) (typeAbsNamesInTerm body)
        ETyInst body _ -> typeAbsNamesInTerm body
        ERoll _ body -> typeAbsNamesInTerm body
        EUnroll body -> typeAbsNamesInTerm body
        _ -> Set.empty

    typeVarNamesInTerm :: ElabTerm -> Set.Set String
    typeVarNamesInTerm term =
      case term of
        ETyAbs name mb body ->
          Set.insert name (maybe Set.empty freeTypeVarsType mb `Set.union` typeVarNamesInTerm body)
        ELam _ ty body -> Set.union (freeTypeVarsType ty) (typeVarNamesInTerm body)
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
            InstUnder _ inner -> goInst inner
            InstBot ty -> freeTypeVarsType ty
            InstAbstr _ -> Set.empty

    rebuildRecursiveArgAlongType :: TypeCheck.Env -> ElabType -> ElabTerm -> ElabTerm
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
        TForall a Nothing (TArrow (TVar b) cod)
          | a == b -> Just (TMu a (TArrow (TVar a) cod))
        _ -> Nothing

    shouldRollMuVar :: ElabType -> ElabType -> Bool
    shouldRollMuVar muTy argTy =
      case (muTy, argTy) of
        (TMu _ _, TVar _) -> True
        _ -> False

mkOut :: (Env -> Either ElabError ElabTerm) -> ElabOut
mkOut f = ElabOut f f

resolvedLambdaParamNode :: (NodeId -> NodeId) -> (NodeId -> Maybe TyNode) -> NodeId -> Maybe NodeId
resolvedLambdaParamNode canonical chiLookupNode lamNodeId =
  let lamC = canonical lamNodeId
   in case chiLookupNode lamC of
        Just TyArrow {tnDom = dom} -> Just dom
        Just TyVar {tnBound = Just bnd} ->
          case chiLookupNode (canonical bnd) of
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
srcTypeToElabType :: NormSrcType -> Either ElabError ElabType
srcTypeToElabType ty = case ty of
  STVar name -> Right (TVar name)
  STArrow dom cod -> TArrow <$> srcTypeToElabType dom <*> srcTypeToElabType cod
  STBase name -> Right (TBase (BaseTy name))
  STCon name args -> TCon (BaseTy name) <$> traverse srcTypeToElabType args
  STVarApp name _ ->
    Left (unsupportedVariableHeadType name)
  STForall name mb body ->
    TForall name
      <$> maybe (Right Nothing) srcBoundToElabBound mb
      <*> srcTypeToElabType body
  STMu name body -> TMu name <$> srcTypeToElabType body
  STBottom -> Right TBottom
  where
    srcBoundToElabBound :: SrcBound 'NormN -> Either ElabError (Maybe BoundType)
    srcBoundToElabBound (SrcBound boundTy) = structBoundToElabBound boundTy

    structBoundToElabBound :: StructBound -> Either ElabError (Maybe BoundType)
    structBoundToElabBound bTy = case bTy of
      STArrow dom cod -> Just <$> (TArrow <$> srcTypeToElabType dom <*> srcTypeToElabType cod)
      STBase name -> Right (Just (TBase (BaseTy name)))
      STCon name args -> Just . TCon (BaseTy name) <$> traverse srcTypeToElabType args
      STVarApp name _ ->
        Left (unsupportedVariableHeadType name)
      STForall name mb body ->
        Just
          <$> ( TForall name
                  <$> maybe (Right Nothing) srcBoundToElabBound mb
                  <*> srcTypeToElabType body
              )
      STMu name body -> Just . TMu name <$> srcTypeToElabType body
      STBottom -> Right Nothing

unsupportedVariableHeadType :: String -> ElabError
unsupportedVariableHeadType name =
  InstantiationError
    ("variable-headed source type application `" ++ name ++ "` is not supported before higher-kinded elaboration")

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
