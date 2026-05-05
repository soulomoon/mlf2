{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.Pipeline
  ( runPipelineElab,
    runPipelineElabChecked,
    runPipelineElabWithConfig,
    runPipelineElabCheckedWithConfig,
    runPipelineElabWithEnv,
    runPipelineElabWithConfigAndEnv,
    PipelineElabDetailedResult (..),
    runPipelineElabDetailedWithEnv,
    runPipelineElabDetailedWithConfigAndEnv,
    runPipelineElabDetailedWithExternalBindings,
    runPipelineElabDetailedWithConfigAndExternalBindings,
    runPipelineElabDetailedUncheckedWithExternalBindings,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Acyclicity (breakCyclesAndCheckAcyclicity)
import MLF.Constraint.Canonicalizer (Canonicalizer, canonicalizeNode)
import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Presolution
  ( EdgeTrace (..),
    PresolutionResult (..),
    computePresolution,
  )
import MLF.Constraint.Presolution.Base (EdgeArtifacts (..))
import MLF.Constraint.Presolution.View (PresolutionView, pvCanonical, toRawPresolutionViewForLegacy)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph (BaseTy (..), Constraint, NodeId (..), PolySyms, cNodes, getEdgeId, getNodeId, lookupNodeIn, toRawConstraintForLegacy)
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Constraint.Types.Presolution (PresolutionSnapshot (..))
import MLF.Elab.Elaborate (ElabConfig (..), ElabEnv (..), elaborateWithEnv)
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.PipelineConfig (PipelineConfig (..), defaultPipelineConfig)
import MLF.Elab.PipelineError
  ( PipelineError (..),
    fromConstraintError,
    fromCycleError,
    fromElabError,
    fromPresolutionError,
    fromSolveError,
    fromTypeCheckError,
  )
import MLF.Elab.Run.Annotation (annNode, redirectAndCanonicalizeAnn)
import MLF.Elab.Run.Generalize
  ( constraintForGeneralization,
    generalizeAtWithBuilder,
    instantiationCopyNodes,
  )
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.ResultType (computeResultTypeFallback, computeResultTypeFromAnn, mkResultTypeInputs)
import MLF.Elab.Run.Scope
  ( letScopeOverrides,
    resolveCanonicalScope,
    schemeBodyTarget,
  )
import MLF.Elab.Run.Util
  ( canonicalizeExpansion,
    canonicalizeTrace,
    canonicalizeWitness,
    makeCanonicalizer,
  )
import MLF.Elab.TermClosure
  ( closeTermWithSchemeSubstIfNeeded,
    preserveRetainedChildAuthoritativeResult,
    substInTerm,
  )
import MLF.Elab.TypeCheck (typeCheckWithEnv)
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen
  ( AnnExpr (..),
    ConstraintError (..),
    ConstraintResult (..),
    ExternalBinding (..),
    ExternalBindingMode (..),
    ExternalBindings,
    ExternalEnv,
    generateConstraints,
    generateConstraintsWithExternalBindings,
  )
import MLF.Frontend.Syntax (NormSrcType, NormSurfaceExpr, StructBound)
import qualified MLF.Frontend.Syntax as Surface
import MLF.Reify.TypeOps (freeTypeVarsType, freshNameLike, substTypeCapture)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data SnapshotViews = SnapshotViews
  { svSolvedClean :: Solved.Solved,
    svPresolutionViewClean :: PresolutionView 'Raw,
    svCanonNode :: Canonicalizer
  }

data TraceCopyArtifacts = TraceCopyArtifacts
  { tcaEdgeTracesForCopy :: IntMap.IntMap EdgeTrace,
    tcaInstCopyNodes :: IntSet.IntSet,
    tcaInstCopyMapFull :: IntMap.IntMap NodeId
  }

data PipelineElabDetailedResult = PipelineElabDetailedResult
  { pedTerm :: ElabTerm,
    pedType :: ElabType,
    pedRootAnn :: AnnExpr,
    pedTypeCheckEnv :: TypeCheck.Env
  }

validateDirectRecursiveAnnotations :: NormSurfaceExpr -> Either ConstraintError ()
validateDirectRecursiveAnnotations = goExpr
  where
    goExpr expr =
      case expr of
        Surface.EVar _ -> Right ()
        Surface.ELit _ -> Right ()
        Surface.ELam _ body -> goExpr body
        Surface.EApp fun arg -> goExpr fun >> goExpr arg
        Surface.ELet _ rhs body -> goExpr rhs >> goExpr body
        Surface.ELamAnn _ annTy body -> validateAnn annTy >> goExpr body
        Surface.EAnn inner annTy -> goExpr inner >> validateAnn annTy
        Surface.ECoerceConst _ -> Right ()

    validateAnn annTy =
      case directNonContractiveMu annTy of
        Just badTy -> Left (RecursiveAnnotationNotSupported badTy)
        Nothing -> Right ()

    directNonContractiveMu annTy =
      case annTy of
        Surface.STMu v body
          | not (muBodyContractive v body) -> Just annTy
        _ -> Nothing

    muBodyContractive needle = bodyType False False
      where
        bodyType guarded shadowed ty =
          case ty of
            Surface.STVar v -> shadowed || v /= needle || guarded
            Surface.STArrow dom cod -> bodyType True shadowed dom && bodyType True shadowed cod
            Surface.STBase _ -> True
            Surface.STCon _ args -> all (bodyType True shadowed) args
            Surface.STVarApp v args ->
              (shadowed || v /= needle || guarded) && all (bodyType True shadowed) args
            Surface.STForall v mb body ->
              let shadowed' = shadowed || v == needle
                  boundOk = maybe True (bodyBound guarded shadowed' . Surface.unNormBound) mb
               in boundOk && bodyType guarded shadowed' body
            Surface.STMu v body ->
              let shadowed' = shadowed || v == needle
               in bodyType guarded shadowed' body
            Surface.STBottom -> True

        bodyBound guarded shadowed bound =
          case bound of
            Surface.STArrow dom cod -> bodyType True shadowed dom && bodyType True shadowed cod
            Surface.STBase _ -> True
            Surface.STCon _ args -> all (bodyType True shadowed) args
            Surface.STVarApp v args ->
              (shadowed || v /= needle || guarded) && all (bodyType True shadowed) args
            Surface.STForall v mb body ->
              let shadowed' = shadowed || v == needle
                  boundOk = maybe True (bodyBound guarded shadowed' . Surface.unNormBound) mb
               in boundOk && bodyType guarded shadowed' body
            Surface.STMu v body ->
              let shadowed' = shadowed || v == needle
               in bodyType guarded shadowed' body
            Surface.STBottom -> True

runPipelineElab :: PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElab = runPipelineElabWithConfig defaultPipelineConfig

runPipelineElabChecked :: PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabChecked = runPipelineElabCheckedWithConfig defaultPipelineConfig

runPipelineElabWithConfig :: PipelineConfig -> PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWithConfig config polySyms expr =
  detailedPair <$> runPipelineElabWith True (pcTraceConfig config) Map.empty (generateConstraints polySyms) expr

-- Compatibility alias: the shared pipeline now reports the typechecker result
-- as authoritative, so the checked entrypoint no longer selects a second path.
runPipelineElabCheckedWithConfig :: PipelineConfig -> PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabCheckedWithConfig = runPipelineElabWithConfig

-- | Run the pipeline with an external environment of type assumptions
-- for free variables, avoiding the ELamAnn wrapping approach.
runPipelineElabWithEnv :: PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWithEnv = runPipelineElabWithConfigAndEnv defaultPipelineConfig

runPipelineElabWithConfigAndEnv :: PipelineConfig -> PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWithConfigAndEnv config polySyms extEnv expr =
  detailedPair <$> runPipelineElabDetailedWithConfigAndEnv config polySyms extEnv expr

runPipelineElabDetailedWithEnv :: PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithEnv = runPipelineElabDetailedWithConfigAndEnv defaultPipelineConfig

runPipelineElabDetailedWithConfigAndEnv :: PipelineConfig -> PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithConfigAndEnv config polySyms extEnv =
  runPipelineElabDetailedWithConfigAndExternalBindings config polySyms (schemeExternalBindings extEnv)

runPipelineElabDetailedWithExternalBindings :: PolySyms -> ExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithExternalBindings =
  runPipelineElabDetailedWithConfigAndExternalBindings defaultPipelineConfig

runPipelineElabDetailedWithConfigAndExternalBindings :: PipelineConfig -> PolySyms -> ExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithConfigAndExternalBindings config polySyms extBindings =
  runPipelineElabWith True (pcTraceConfig config) extBindings (generateConstraintsWithExternalBindings polySyms extBindings)

runPipelineElabDetailedUncheckedWithExternalBindings :: PolySyms -> ExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedUncheckedWithExternalBindings polySyms extBindings =
  runPipelineElabWith False (pcTraceConfig defaultPipelineConfig) extBindings (generateConstraintsWithExternalBindings polySyms extBindings)

schemeExternalBindings :: ExternalEnv -> ExternalBindings
schemeExternalBindings =
  Map.map (\srcTy -> ExternalBinding {externalBindingType = srcTy, externalBindingMode = ExternalBindingScheme})

detailedPair :: PipelineElabDetailedResult -> (ElabTerm, ElabType)
detailedPair result = (pedTerm result, pedType result)

runPipelineElabWith ::
  Bool ->
  TraceConfig ->
  ExternalBindings ->
  (NormSurfaceExpr -> Either ConstraintError (ConstraintResult 'Raw)) ->
  NormSurfaceExpr ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWith requireFinalTypeCheck traceCfg extBindings genConstraints expr = do
  () <- fromConstraintError (validateDirectRecursiveAnnotations expr)
  initialSchemeInfos <- fromConstraintError (traverse externalBindingSchemeInfo extBindings)
  ConstraintResult {crConstraint = c0, crAnnotated = ann, crAnnSourceTypes = annSourceTypes, crInitialEnv = _initialBindings} <- fromConstraintError (genConstraints expr)
  let c1 = normalize c0
  (cAcyclic, acyc) <- fromCycleError (breakCyclesAndCheckAcyclicity c1)
  pres <- fromPresolutionError (computePresolution traceCfg acyc cAcyclic)
  SnapshotViews
    { svSolvedClean = solvedClean,
      svPresolutionViewClean = presolutionViewClean,
      svCanonNode = canonNode
    } <-
    prepareSnapshotViews pres
  let cAcyclicRaw = toRawConstraintForLegacy cAcyclic
  let planBuilder = prPlanBuilder pres
  TraceCopyArtifacts
    { tcaInstCopyNodes = instCopyNodes,
      tcaInstCopyMapFull = instCopyMapFull
    } <-
    pure (prepareTraceCopyArtifacts cAcyclicRaw presolutionViewClean (prRedirects pres) canonNode (prEdgeTraces pres))
  let (constraintForGen, bindParentsGa) =
        constraintForGeneralization traceCfg presolutionViewClean (prRedirects pres) instCopyNodes instCopyMapFull cAcyclicRaw ann
  presolutionViewForGen <- fromSolveError (Finalize.finalizePresolutionViewFromSnapshot constraintForGen (Solved.canonicalMap solvedClean))
  let generalizeAtWithView mbGa =
        generalizeAtWithBuilder
          planBuilder
          mbGa
          presolutionViewForGen
  -- Build authoritative SchemeInfo map and TypeCheck.Env from external
  -- source types.  We derive schemes directly from the caller-supplied
  -- NormSrcType values (which preserve lowerType naming) instead of
  -- re-generalizing through the constraint graph, which would produce
  -- graph-internal variable names that conflict with constructor types.
  let initialTcEnv =
        TypeCheck.Env
          { TypeCheck.termEnv = Map.map (schemeToType . siScheme) initialSchemeInfos,
            TypeCheck.typeEnv = Map.empty
          }
  let annCanon = redirectAndCanonicalizeAnn (canonicalizeNode canonNode) (prRedirects pres) ann
  let edgeArtifacts =
        EdgeArtifacts
          { eaEdgeExpansions = IntMap.map (canonicalizeExpansion canonNode) (prEdgeExpansions pres),
            eaEdgeWitnesses = IntMap.map (canonicalizeWitness canonNode) (prEdgeWitnesses pres),
            eaEdgeTraces = IntMap.map (canonicalizeTrace canonNode) (prEdgeTraces pres)
          }
  let scopeOverrides =
        letScopeOverrides
          cAcyclicRaw
          constraintForGen
          presolutionViewClean
          (prRedirects pres)
          annCanon
  let elabConfig =
        ElabConfig
          { ecTraceConfig = traceCfg,
            ecGeneralizeAtWith = generalizeAtWithView
          }
      elabEnv =
        ElabEnv
          { eePresolutionView = presolutionViewForGen,
            eeGaParents = bindParentsGa,
            eeEdgeArtifacts = edgeArtifacts,
            eeScopeOverrides = scopeOverrides,
            eeAnnSourceTypes =
              IntMap.fromList
                [ (getNodeId (canonicalizeNode canonNode nid), ty)
                  | (k, ty) <- IntMap.toList annSourceTypes,
                    let nid = NodeId k
                ],
            eeInitialTermEnv = initialSchemeInfos
          }
  term <- fromElabError (elaborateWithEnv elabConfig elabEnv annCanon)
  case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
    () -> pure ()
  let authoritativeAnnCanon = authoritativeRootAnn term annCanon
      authoritativeAnnPre = authoritativeRootAnn term ann
      (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) =
        stripWitnesslessAuthoritativeAnn (eaEdgeWitnesses edgeArtifacts) authoritativeAnnCanon authoritativeAnnPre
  rootScope <-
    fromElabError $
      bindingToElab $
        resolveCanonicalScope cAcyclicRaw presolutionViewForGen (prRedirects pres) (annNode authoritativeAnnPreFinal)
  let rootTarget = schemeBodyTarget presolutionViewForGen (annNode authoritativeAnnCanonFinal)
  (rootScheme, rootSubst) <-
    fromElabError $
      generalizeAtWithView (Just bindParentsGa) rootScope rootTarget
  let termSubst = substInTerm rootSubst term

  -- Build context for result type computation
  let canonical = pvCanonical presolutionViewForGen
      resultTypeInputs =
        mkResultTypeInputs
          canonical
          edgeArtifacts
          presolutionViewForGen
          bindParentsGa
          planBuilder
          cAcyclicRaw
          (prRedirects pres)
          traceCfg
      retainedChildAuthoritativeCandidate =
        case preserveRetainedChildAuthoritativeResult termSubst of
          Just _ -> True
          Nothing -> False
      termClosed0 =
        if retainedChildAuthoritativeCandidate
          then closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
          else case typeCheckWithEnv initialTcEnv termSubst of
            Right ty | null (freeTypeVarsType ty) -> termSubst
            Right ty ->
              case rootScheme of
                Forall binds _
                  | null binds ->
                      let freeBinds =
                            [ (name, Nothing)
                              | name <- Set.toList (freeTypeVarsType ty)
                            ]
                          freeScheme = Forall freeBinds ty
                       in closeTermWithSchemeSubstIfNeeded IntMap.empty freeScheme termSubst
                _ -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
            Left _ -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
      termClosed =
        case preserveRetainedChildAuthoritativeResult termClosed0 of
          Just termAdjusted -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme termAdjusted
          Nothing -> termClosed0
  let termClosedFresh = freshenTypeAbsAgainstEnv initialTcEnv termClosed
      uncheckedAuthoritative =
        pure
          PipelineElabDetailedResult
            { pedTerm = termClosedFresh,
              pedType = schemeToType rootScheme,
              pedRootAnn = authoritativeAnnCanonFinal,
              pedTypeCheckEnv = initialTcEnv
            }
      checkedAuthoritative = do
        tyChecked <-
          case typeCheckWithEnv initialTcEnv termClosedFresh of
            Right ty -> pure ty
            Left err -> fromTypeCheckError (Left err)
        pure
          PipelineElabDetailedResult
            { pedTerm = termClosedFresh,
              pedType = tyChecked,
              pedRootAnn = authoritativeAnnCanonFinal,
              pedTypeCheckEnv = initialTcEnv
            }
      authoritativeResult =
        if requireFinalTypeCheck
          then checkedAuthoritative
          else uncheckedAuthoritative

  -- Keep result-type reconstruction for diagnostics, but report the
  -- type-checker result as authoritative.
  if not requireFinalTypeCheck
    then authoritativeResult
    else case (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) of
      (AAnn inner annNodeId eid, AAnn innerPre _ _) -> do
        _ <- fromElabError (computeResultTypeFromAnn resultTypeInputs inner innerPre annNodeId eid)
        authoritativeResult
      (AUnfold inner annNodeId eid, _) -> do
        let innerPre = case authoritativeAnnPreFinal of
              AUnfold ip _ _ -> ip
              AAnn ip _ _ -> ip
              other -> other
        _ <- fromElabError (computeResultTypeFromAnn resultTypeInputs inner innerPre annNodeId eid)
        authoritativeResult
      _ -> do
        _ <- fromElabError (computeResultTypeFallback resultTypeInputs authoritativeAnnCanonFinal authoritativeAnnPreFinal)
        authoritativeResult

freshenTypeAbsAgainstEnv :: TypeCheck.Env -> ElabTerm -> ElabTerm
freshenTypeAbsAgainstEnv env term0 = pruneVacuousLeadingTyAbsAgainstEnv env (go reserved term0)
  where
    reserved =
      Set.unions
        ( map freeTypeVarsType (Map.elems (TypeCheck.termEnv env))
            ++ [Set.fromList (Map.keys (TypeCheck.typeEnv env))]
        )

    go used term = case term of
      ETyAbs name mb body ->
        let usedForBinder = Set.union used (maybe Set.empty freeTypeVarsType mb)
            (name', bodyForName) =
              if Set.member name usedForBinder
                then
                  let fresh = freshNameLike name usedForBinder
                      bodyRenamed = renameTypeVarInTerm name fresh body
                   in (fresh, bodyRenamed)
                else (name, body)
            usedBody = Set.insert name' usedForBinder
            body' = go usedBody bodyForName
         in ETyAbs name' mb body'
      ELam v ty body ->
        ELam v ty (go (Set.union used (freeTypeVarsType ty)) body)
      EApp f a -> EApp (go used f) (go used a)
      ELet v sch rhs body ->
        let used' = Set.union used (freeTypeVarsType (schemeToType sch))
         in ELet v sch (go used' rhs) (go used' body)
      ETyInst t inst -> ETyInst (go used t) inst
      ERoll ty body -> ERoll ty (go used body)
      EUnroll body -> EUnroll (go used body)
      _ -> term

pruneVacuousLeadingTyAbsAgainstEnv :: TypeCheck.Env -> ElabTerm -> ElabTerm
pruneVacuousLeadingTyAbsAgainstEnv env term = case term of
  ETyAbs name mb body ->
    let env' =
          env
            { TypeCheck.typeEnv =
                Map.insert name (maybe TBottom tyToElab mb) (TypeCheck.typeEnv env)
            }
        body' = pruneVacuousLeadingTyAbsAgainstEnv env' body
     in case typeCheckWithEnv env' body' of
          Right bodyTy
            | name `Set.notMember` freeTypeVarsType bodyTy,
              not (containsRecursiveType bodyTy) ->
                case mb of
                  Nothing -> pruneVacuousLeadingTyAbsAgainstEnv env body'
                  Just _ ->
                    case Set.toList (freeTypeVarsType bodyTy `Set.difference` freeTypeVarsTypeCheckEnv env) of
                      [freeName] ->
                        let bodyRenamed = renameTypeVarInTerm freeName name body'
                         in case typeCheckWithEnv env' bodyRenamed of
                              Right renamedTy
                                | name `Set.member` freeTypeVarsType renamedTy ->
                                    ETyAbs name mb bodyRenamed
                              _ -> pruneVacuousLeadingTyAbsAgainstEnv env body'
                      _ -> pruneVacuousLeadingTyAbsAgainstEnv env body'
          _ -> ETyAbs name mb body'
  _ -> term

freeTypeVarsTypeCheckEnv :: TypeCheck.Env -> Set.Set String
freeTypeVarsTypeCheckEnv env =
  Set.unions
    ( map freeTypeVarsType (Map.elems (TypeCheck.termEnv env))
        ++ map freeTypeVarsType (Map.elems (TypeCheck.typeEnv env))
        ++ [Set.fromList (Map.keys (TypeCheck.typeEnv env))]
    )

containsRecursiveType :: ElabType -> Bool
containsRecursiveType ty = case ty of
  TMu _ _ -> True
  TArrow dom cod -> containsRecursiveType dom || containsRecursiveType cod
  TCon _ args -> any containsRecursiveType args
  TForall _ mb body -> maybe False containsRecursiveBound mb || containsRecursiveType body
  _ -> False
  where
    containsRecursiveBound bound = case bound of
      TArrow dom cod -> containsRecursiveType dom || containsRecursiveType cod
      TCon _ args -> any containsRecursiveType args
      TForall _ mb body -> maybe False containsRecursiveBound mb || containsRecursiveType body
      TMu _ _ -> True
      _ -> False

renameTypeVarInTerm :: String -> String -> ElabTerm -> ElabTerm
renameTypeVarInTerm old new term =
  let ty' = TVar new
      renameTy = substTypeCapture old ty'
      renameBound = mapBoundType renameTy
      renameScheme sch = schemeFromType (renameTy (schemeToType sch))
      renameName v
        | v == old = new
        | otherwise = v
      renameInst inst = case inst of
        InstId -> InstId
        InstApp ty -> InstApp (renameTy ty)
        InstIntro -> InstIntro
        InstElim -> InstElim
        InstInside inner -> InstInside (renameInst inner)
        InstSeq a b -> InstSeq (renameInst a) (renameInst b)
        InstUnder v inner -> InstUnder (renameName v) (renameInst inner)
        InstBot ty -> InstBot (renameTy ty)
        InstAbstr v -> InstAbstr (renameName v)
   in case term of
        EVar _ -> term
        ELit _ -> term
        ELam v ty body -> ELam v (renameTy ty) (renameTypeVarInTerm old new body)
        EApp f a -> EApp (renameTypeVarInTerm old new f) (renameTypeVarInTerm old new a)
        ELet v sch rhs body -> ELet v (renameScheme sch) (renameTypeVarInTerm old new rhs) (renameTypeVarInTerm old new body)
        ETyAbs v mb body
          | v == old -> ETyAbs v (fmap renameBound mb) body
          | otherwise -> ETyAbs v (fmap renameBound mb) (renameTypeVarInTerm old new body)
        ETyInst t inst -> ETyInst (renameTypeVarInTerm old new t) (renameInst inst)
        ERoll ty body -> ERoll (renameTy ty) (renameTypeVarInTerm old new body)
        EUnroll body -> EUnroll (renameTypeVarInTerm old new body)

prepareSnapshotViews :: PresolutionResult -> Either PipelineError SnapshotViews
prepareSnapshotViews pres = do
  let preRewrite = snapshotConstraint pres
  solvedClean <- fromSolveError (Finalize.finalizeSolvedFromSnapshot preRewrite (snapshotUnionFind pres))
  presolutionViewClean <- toRawPresolutionViewForLegacy <$> fromSolveError (Finalize.finalizePresolutionViewFromSnapshot preRewrite (snapshotUnionFind pres))
  let canonNode = makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
  pure
    SnapshotViews
      { svSolvedClean = solvedClean,
        svPresolutionViewClean = presolutionViewClean,
        svCanonNode = canonNode
      }

prepareTraceCopyArtifacts ::
  Constraint 'Raw ->
  PresolutionView 'Raw ->
  IntMap.IntMap NodeId ->
  Canonicalizer ->
  IntMap.IntMap EdgeTrace ->
  TraceCopyArtifacts
prepareTraceCopyArtifacts baseConstraint presolutionView redirects canonNode edgeTraces =
  let adoptNode = canonicalizeNode canonNode
      baseNodes = cNodes baseConstraint
      edgeTracesForCopy =
        IntMap.filter
          ( \tr ->
              case lookupNodeIn baseNodes (etRoot tr) of
                Just _ -> True
                Nothing -> False
          )
          edgeTraces
      instCopyNodes =
        instantiationCopyNodes presolutionView redirects edgeTracesForCopy
      instCopyMapFull =
        let baseNamedKeysAll = collectBaseNamedKeys baseConstraint
            traceMaps =
              map
                (buildTraceCopyMap baseConstraint baseNamedKeysAll adoptNode)
                (IntMap.elems edgeTracesForCopy)
         in foldl' IntMap.union IntMap.empty traceMaps
   in TraceCopyArtifacts
        { tcaEdgeTracesForCopy = edgeTracesForCopy,
          tcaInstCopyNodes = instCopyNodes,
          tcaInstCopyMapFull = instCopyMapFull
        }

authoritativeRootAnn :: ElabTerm -> AnnExpr -> AnnExpr
authoritativeRootAnn term annExpr =
  case (stripLeadingTyAbs term, annExpr) of
    (term0, AAnn inner _ _)
      | shouldStripAuthoritativeAnn term0 ->
          authoritativeRootAnn term0 inner
    (term0, AUnfold inner _ _)
      | shouldStripAuthoritativeAnn term0 ->
          authoritativeRootAnn term0 inner
    (ELet termName _ _ bodyTerm, ALet annName _ _ _ _ _ bodyAnn _)
      | termName == annName ->
          authoritativeRootAnn bodyTerm bodyAnn
    (EApp (ELam param _ (EVar bodyVar)) argTerm, AApp _ argAnn _ _ _)
      | param == bodyVar ->
          authoritativeRootAnn argTerm argAnn
    (EVar termName, AApp _ argAnn _ _ _)
      | annProducesVar termName argAnn ->
          authoritativeRootAnn (EVar termName) argAnn
    _ -> annExpr

shouldStripAuthoritativeAnn :: ElabTerm -> Bool
shouldStripAuthoritativeAnn term =
  case term of
    ELet {} -> True
    EVar {} -> True
    EApp (ELam param _ (EVar bodyVar)) _ -> param == bodyVar
    _ -> False

stripWitnesslessAuthoritativeAnn ::
  IntMap.IntMap edgeWitness ->
  AnnExpr ->
  AnnExpr ->
  (AnnExpr, AnnExpr)
stripWitnesslessAuthoritativeAnn edgeWitnesses annCanon annPre =
  case annCanon of
    AAnn innerCanon _ eid
      | IntMap.notMember (getEdgeId eid) edgeWitnesses ->
          let innerPre =
                case annPre of
                  AAnn inner _ _ -> inner
                  AUnfold inner _ _ -> inner
                  other -> other
           in stripWitnesslessAuthoritativeAnn edgeWitnesses innerCanon innerPre
    AUnfold innerCanon _ eid
      | IntMap.notMember (getEdgeId eid) edgeWitnesses ->
          let innerPre =
                case annPre of
                  AAnn inner _ _ -> inner
                  AUnfold inner _ _ -> inner
                  other -> other
           in stripWitnesslessAuthoritativeAnn edgeWitnesses innerCanon innerPre
    _ -> (annCanon, annPre)

annProducesVar :: Surface.VarName -> AnnExpr -> Bool
annProducesVar termName = go
  where
    go annExpr =
      case annExpr of
        AVar annName _ -> annName == termName
        AAnn inner _ _ -> go inner
        AUnfold inner _ _ -> go inner
        _ -> False

stripLeadingTyAbs :: ElabTerm -> ElabTerm
stripLeadingTyAbs term =
  case term of
    ETyAbs _ _ body -> stripLeadingTyAbs body
    _ -> term

{- Note [srcTypeToElabType in Pipeline]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Local copy of the NormSrcType → ElabType conversion used to build
   authoritative SchemeInfo for external environment bindings.  The
   canonical copy lives in MLF.Elab.Elaborate.Algebra (internal) and
   MLF.Frontend.Program.Elaborate (also internal, not exported).
   We keep this local to avoid widening production facades. -}

externalBindingSchemeInfo :: ExternalBinding -> Either ConstraintError SchemeInfo
externalBindingSchemeInfo ExternalBinding {externalBindingType = srcTy} = do
  ty <- srcTypeToElabType srcTy
  pure SchemeInfo {siScheme = schemeFromType ty, siSubst = IntMap.empty}

srcTypeToElabType :: NormSrcType -> Either ConstraintError ElabType
srcTypeToElabType ty = case ty of
  Surface.STVar name -> Right (TVar name)
  Surface.STArrow dom cod -> TArrow <$> srcTypeToElabType dom <*> srcTypeToElabType cod
  Surface.STBase name -> Right (TBase (BaseTy name))
  Surface.STCon name args -> TCon (BaseTy name) <$> traverse srcTypeToElabType args
  Surface.STVarApp name _ ->
    unsupportedVariableHead name
  Surface.STForall name mb body ->
    TForall name
      <$> maybe (Right Nothing) srcBoundToElabBound mb
      <*> srcTypeToElabType body
  Surface.STMu name body -> TMu name <$> srcTypeToElabType body
  Surface.STBottom -> Right TBottom
  where
    srcBoundToElabBound :: Surface.SrcBound 'Surface.NormN -> Either ConstraintError (Maybe BoundType)
    srcBoundToElabBound (Surface.SrcBound boundTy) = structBoundToElabBound boundTy

    structBoundToElabBound :: StructBound -> Either ConstraintError (Maybe BoundType)
    structBoundToElabBound bTy = case bTy of
      Surface.STArrow dom cod -> Just <$> (TArrow <$> srcTypeToElabType dom <*> srcTypeToElabType cod)
      Surface.STBase name -> Right (Just (TBase (BaseTy name)))
      Surface.STCon name args -> Just . TCon (BaseTy name) <$> traverse srcTypeToElabType args
      Surface.STVarApp name _ ->
        unsupportedVariableHead name
      Surface.STForall name mb body ->
        Just
          <$> ( TForall name
                  <$> maybe (Right Nothing) srcBoundToElabBound mb
                  <*> srcTypeToElabType body
              )
      Surface.STMu name body -> Just . TMu name <$> srcTypeToElabType body
      Surface.STBottom -> Right Nothing

    unsupportedVariableHead name =
      Left (InternalConstraintError ("variable-headed source type application `" ++ name ++ "` is not supported before higher-kinded elaboration"))
