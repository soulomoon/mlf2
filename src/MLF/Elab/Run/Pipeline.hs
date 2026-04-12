{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.Pipeline
  ( runPipelineElab,
    runPipelineElabChecked,
    runPipelineElabWithConfig,
    runPipelineElabCheckedWithConfig,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
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
import MLF.Constraint.Presolution.View (PresolutionView, pvCanonical)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types (Constraint, NodeId, PolySyms, cNodes, getEdgeId, lookupNodeIn)
import MLF.Constraint.Types.Presolution (PresolutionSnapshot (..))
import MLF.Elab.Elaborate (ElabConfig (..), ElabEnv (..), elaborateWithEnv)
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
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen (AnnExpr (..), ConstraintError (..), ConstraintResult (..), generateConstraints)
import MLF.Frontend.Syntax (NormSurfaceExpr)
import qualified MLF.Frontend.Syntax as Surface
import MLF.Reify.TypeOps (freeTypeVarsType)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data SnapshotViews = SnapshotViews
  { svSolvedClean :: Solved.Solved,
    svPresolutionViewClean :: PresolutionView,
    svCanonNode :: Canonicalizer
  }

data TraceCopyArtifacts = TraceCopyArtifacts
  { tcaEdgeTracesForCopy :: IntMap.IntMap EdgeTrace,
    tcaInstCopyNodes :: IntSet.IntSet,
    tcaInstCopyMapFull :: IntMap.IntMap NodeId
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
runPipelineElabWithConfig config polySyms =
  runPipelineElabWith (pcTraceConfig config) (generateConstraints polySyms)

runPipelineElabCheckedWithConfig :: PipelineConfig -> PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabCheckedWithConfig = runPipelineElabWithConfig

runPipelineElabWith ::
  TraceConfig ->
  (NormSurfaceExpr -> Either ConstraintError ConstraintResult) ->
  NormSurfaceExpr ->
  Either PipelineError (ElabTerm, ElabType)
runPipelineElabWith traceCfg genConstraints expr = do
  () <- fromConstraintError (validateDirectRecursiveAnnotations expr)
  ConstraintResult {crConstraint = c0, crAnnotated = ann} <- fromConstraintError (genConstraints expr)
  let c1 = normalize c0
  (c1Broken, acyc) <- fromCycleError (breakCyclesAndCheckAcyclicity c1)
  pres <- fromPresolutionError (computePresolution traceCfg acyc c1Broken)
  SnapshotViews
    { svSolvedClean = solvedClean,
      svPresolutionViewClean = presolutionViewClean,
      svCanonNode = canonNode
    } <-
    prepareSnapshotViews pres
  let planBuilder = prPlanBuilder pres
  TraceCopyArtifacts
    { tcaInstCopyNodes = instCopyNodes,
      tcaInstCopyMapFull = instCopyMapFull
    } <-
    pure (prepareTraceCopyArtifacts c1Broken presolutionViewClean (prRedirects pres) canonNode (prEdgeTraces pres))
  let (constraintForGen, bindParentsGa) =
        constraintForGeneralization traceCfg presolutionViewClean (prRedirects pres) instCopyNodes instCopyMapFull c1Broken ann
  presolutionViewForGen <- fromSolveError (Finalize.finalizePresolutionViewFromSnapshot constraintForGen (Solved.canonicalMap solvedClean))
  let generalizeAtWithView mbGa =
        generalizeAtWithBuilder
          planBuilder
          mbGa
          presolutionViewForGen
  let annCanon = redirectAndCanonicalizeAnn (canonicalizeNode canonNode) (prRedirects pres) ann
  let edgeArtifacts =
        EdgeArtifacts
          { eaEdgeExpansions = IntMap.map (canonicalizeExpansion canonNode) (prEdgeExpansions pres),
            eaEdgeWitnesses = IntMap.map (canonicalizeWitness canonNode) (prEdgeWitnesses pres),
            eaEdgeTraces = IntMap.map (canonicalizeTrace canonNode) (prEdgeTraces pres)
          }
  let scopeOverrides =
        letScopeOverrides
          c1Broken
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
            eeScopeOverrides = scopeOverrides
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
        resolveCanonicalScope c1Broken presolutionViewForGen (prRedirects pres) (annNode authoritativeAnnPreFinal)
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
          c1Broken
          (prRedirects pres)
          traceCfg
      retainedChildAuthoritativeCandidate =
        case preserveRetainedChildAuthoritativeResult termSubst of
          Just _ -> True
          Nothing -> False
      termClosed0 =
        if retainedChildAuthoritativeCandidate
          then closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
          else case typeCheck termSubst of
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
  let checkedAuthoritative = do
        tyChecked <- fromTypeCheckError (typeCheck termClosed)
        pure (termClosed, tyChecked)

  -- Keep result-type reconstruction for diagnostics, but report the
  -- type-checker result as authoritative.
  case (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) of
    (AAnn inner annNodeId eid, AAnn innerPre _ _) -> do
      _ <- fromElabError (computeResultTypeFromAnn resultTypeInputs inner innerPre annNodeId eid)
      checkedAuthoritative
    _ -> do
      _ <- fromElabError (computeResultTypeFallback resultTypeInputs authoritativeAnnCanonFinal authoritativeAnnPreFinal)
      checkedAuthoritative

prepareSnapshotViews :: PresolutionResult -> Either PipelineError SnapshotViews
prepareSnapshotViews pres = do
  let preRewrite = snapshotConstraint pres
  solvedClean <- fromSolveError (Finalize.finalizeSolvedFromSnapshot preRewrite (snapshotUnionFind pres))
  presolutionViewClean <- fromSolveError (Finalize.finalizePresolutionViewFromSnapshot preRewrite (snapshotUnionFind pres))
  let canonNode = makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
  pure
    SnapshotViews
      { svSolvedClean = solvedClean,
        svPresolutionViewClean = presolutionViewClean,
        svCanonNode = canonNode
      }

prepareTraceCopyArtifacts ::
  Constraint ->
  PresolutionView ->
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
        _ -> False

stripLeadingTyAbs :: ElabTerm -> ElabTerm
stripLeadingTyAbs term =
  case term of
    ETyAbs _ _ body -> stripLeadingTyAbs body
    _ -> term
