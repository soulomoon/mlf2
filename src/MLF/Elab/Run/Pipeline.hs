{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.Pipeline (
    runPipelineElab,
    runPipelineElabChecked,
    runPipelineElabWithConfig,
    runPipelineElabCheckedWithConfig,
    runPipelineElabProjectionFirst
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

import MLF.Frontend.Syntax (NormSurfaceExpr)
import MLF.Frontend.ConstraintGen (AnnExpr(..), ConstraintError, ConstraintResult(..), generateConstraints)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Acyclicity (checkAcyclicity)
import MLF.Constraint.Canonicalizer (canonicalizeNode)
import MLF.Constraint.Presolution
    ( PresolutionResult(..)
    , computePresolution
    , EdgeTrace(..)
    )
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph (PolySyms)
import MLF.Constraint.Types (cNodes, lookupNodeIn)
import MLF.Elab.Elaborate (ElabConfig(..), ElabEnv(..), elaborateWithEnv)
import MLF.Elab.PipelineConfig (PipelineConfig(..), defaultPipelineConfig)
import MLF.Elab.PipelineError
    ( PipelineError(..)
    , fromConstraintError
    , fromCycleError
    , fromElabError
    , fromPresolutionError
    , fromSolveError
    , fromTypeCheckError
    )
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types
import MLF.Elab.TermClosure (closeTermWithSchemeSubst, substInTerm)
import MLF.Elab.Run.Annotation (applyRedirectsToAnn, canonicalizeAnn, annNode)
import MLF.Elab.Run.Generalize
    ( constraintForGeneralization
    , generalizeAtWithBuilder
    , instantiationCopyNodes
    )
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.Scope (letScopeOverrides)
import MLF.Elab.Run.Scope (resolveCanonicalScope, schemeBodyTarget)
import MLF.Elab.Run.Util
    ( canonicalizeExpansion
    , canonicalizeTrace
    , canonicalizeWitness
    , makeCanonicalizer
    )
import MLF.Elab.Run.ResultType (ResultTypeContext(..), computeResultTypeFromAnn, computeResultTypeFallback)
import MLF.Reify.Core (reifyType)
import MLF.Reify.TypeOps (freeTypeVarsType)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

runPipelineElab :: PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElab = runPipelineElabWithConfig defaultPipelineConfig

runPipelineElabChecked :: PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabChecked = runPipelineElabCheckedWithConfig defaultPipelineConfig

runPipelineElabWithConfig :: PipelineConfig -> PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWithConfig config polySyms =
    runPipelineElabWith (pcTraceConfig config) (generateConstraints polySyms)

runPipelineElabCheckedWithConfig :: PipelineConfig -> PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabCheckedWithConfig = runPipelineElabWithConfig

runPipelineElabWith
    :: TraceConfig
    -> (NormSurfaceExpr -> Either ConstraintError ConstraintResult)
    -> NormSurfaceExpr
    -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWith traceCfg genConstraints expr = do
    runPipelineElabWithSolvedBuilder traceCfg genConstraints buildSolvedNativeFromPresolution expr

type SolvedBuilder = TraceConfig -> PresolutionResult -> Either PipelineError Solved.Solved

buildSolvedNativeFromPresolution :: SolvedBuilder
buildSolvedNativeFromPresolution _traceCfg pres =
    fromSolveError (Solved.fromPresolutionResult pres)

runPipelineElabWithSolvedBuilder
    :: TraceConfig
    -> (NormSurfaceExpr -> Either ConstraintError ConstraintResult)
    -> SolvedBuilder
    -> NormSurfaceExpr
    -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWithSolvedBuilder traceCfg genConstraints buildSolved expr = do
    ConstraintResult { crConstraint = c0, crAnnotated = ann } <- fromConstraintError (genConstraints expr)
    let c1 = normalize c0
    acyc <- fromCycleError (checkAcyclicity c1)
    pres <- fromPresolutionError (computePresolution traceCfg acyc c1)
    let planBuilder = prPlanBuilder pres
        generalizeAtWith = generalizeAtWithBuilder planBuilder
    solvedView <- buildSolved traceCfg pres
    let setSolvedConstraint res c' =
            let cCanon = rewriteConstraintWithUF (Solved.canonicalMap res) c'
            in Solved.rebuildWithConstraint res cCanon
        solvedClean = Solved.pruneBindParentsSolved solvedView
        solvedCleanView = solvedClean
    case Solved.validateCanonicalGraphStrict solvedClean of
        [] -> do
            let canonNode = makeCanonicalizer (Solved.canonicalMap solvedCleanView) (prRedirects pres)
                adoptNode = canonicalizeNode canonNode
                baseNodes = cNodes c1
                edgeTracesForCopy =
                    IntMap.filter
                        (\tr ->
                            case lookupNodeIn baseNodes (etRoot tr) of
                                Just _ -> True
                                Nothing -> False
                        )
                        (prEdgeTraces pres)
                instCopyNodes =
                    instantiationCopyNodes solvedClean (prRedirects pres) edgeTracesForCopy
                instCopyMapFull =
                    let baseNamedKeysAll = collectBaseNamedKeys c1
                        traceMaps = map (buildTraceCopyMap c1 baseNamedKeysAll adoptNode)
                                         (IntMap.elems edgeTracesForCopy)
                    in foldl' IntMap.union IntMap.empty traceMaps
                (constraintForGen, bindParentsGa) =
                    constraintForGeneralization traceCfg solvedClean (prRedirects pres) instCopyNodes instCopyMapFull c1 ann
            let solvedForGen = setSolvedConstraint solvedClean constraintForGen
                solvedForGenView = solvedForGen
            let ann' = applyRedirectsToAnn (prRedirects pres) ann
            let annCanon = canonicalizeAnn (canonicalizeNode canonNode) ann'
            let edgeWitnesses = IntMap.map (canonicalizeWitness canonNode) (prEdgeWitnesses pres)
                edgeTraces = IntMap.map (canonicalizeTrace canonNode) (prEdgeTraces pres)
                edgeExpansions = IntMap.map (canonicalizeExpansion canonNode) (prEdgeExpansions pres)
            let scopeOverrides =
                    letScopeOverrides
                        c1
                        (Solved.originalConstraint solvedForGenView)
                        solvedClean
                        (prRedirects pres)
                        annCanon
            let elabConfig = ElabConfig
                    { ecTraceConfig = traceCfg
                    , ecGeneralizeAtWith = generalizeAtWith
                    }
                elabEnv = ElabEnv
                    { eeResPhi = solvedClean
                    , eeResReify = solvedClean
                    , eeResGen = solvedForGen
                    , eeGaParents = bindParentsGa
                    , eeEdgeWitnesses = edgeWitnesses
                    , eeEdgeTraces = edgeTraces
                    , eeEdgeExpansions = edgeExpansions
                    , eeScopeOverrides = scopeOverrides
                    }
            term <- fromElabError (elaborateWithEnv elabConfig elabEnv annCanon)
            case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
                () -> pure ()
            rootScope <- fromElabError $
                bindingToElab $
                    resolveCanonicalScope c1 solvedForGen (prRedirects pres) (annNode ann)
            let rootTarget = schemeBodyTarget solvedForGen (annNode annCanon)
            let generalizeNeedsFallback err = case err of
                    SchemeFreeVars{} -> True
                    _ -> False
            (rootScheme, rootSubst) <- fromElabError $
                case generalizeAtWith (Just bindParentsGa) solvedForGen rootScope rootTarget of
                    Right out -> Right out
                    Left err
                        | generalizeNeedsFallback err ->
                            case generalizeAtWith Nothing solvedForGen rootScope rootTarget of
                                Right out -> Right out
                                Left err2
                                    | generalizeNeedsFallback err2 -> do
                                        tyFallback <- reifyType solvedForGen rootTarget
                                        pure (schemeFromType tyFallback, IntMap.empty)
                                Left err2 -> Left err2
                    Left err -> Left err
            let termSubst = substInTerm rootSubst term
                termClosed =
                    case typeCheck termSubst of
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
                                        in closeTermWithSchemeSubst IntMap.empty freeScheme termSubst
                                _ -> closeTermWithSchemeSubst rootSubst rootScheme term
                        Left _ -> closeTermWithSchemeSubst rootSubst rootScheme term
            let checkedAuthoritative = do
                    tyChecked <- fromTypeCheckError (typeCheck termClosed)
                    pure (termClosed, tyChecked)

            -- Build context for result type computation
            let canonical = Solved.canonical solvedCleanView
                resultTypeCtx = ResultTypeContext
                    { rtcCanonical = canonical
                    , rtcEdgeWitnesses = edgeWitnesses
                    , rtcEdgeTraces = edgeTraces
                    , rtcEdgeExpansions = edgeExpansions
                    , rtcSolvedForGen = solvedForGen
                    , rtcSolvedClean = solvedClean
                    , rtcBindParentsGa = bindParentsGa
                    , rtcPlanBuilder = planBuilder
                    , rtcBaseConstraint = c1
                    , rtcRedirects = prRedirects pres
                    , rtcTraceConfig = traceCfg
                    }

            -- Keep result-type reconstruction for diagnostics, but report the
            -- type-checker result as authoritative.
            case annCanon of
                AAnn inner annNodeId eid -> do
                    _ <- fromElabError (computeResultTypeFromAnn resultTypeCtx inner inner annNodeId eid)
                    checkedAuthoritative
                _ -> do
                    _ <- fromElabError (computeResultTypeFallback resultTypeCtx annCanon ann)
                    checkedAuthoritative
        vs -> Left (PipelineSolveError (Solve.ValidationFailed vs))

-- | Run pipeline with projection-first Solved queries.
--
-- Uses original-domain lookups as primary source; canonical projection
-- only for alias reconciliation.  For dual-path verification (Phase E).
--
-- The key difference from 'runPipelineElab': after constructing the
-- 'Solved' view, we validate that original-domain and canonical-domain
-- node tags agree.  If any call site was missed during Phase B/C
-- migration and still reads from canonical-domain queries, the
-- projection-first path will expose the divergence.
runPipelineElabProjectionFirst
    :: PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabProjectionFirst polySyms =
    runPipelineElabWithSolvedBuilder
        (pcTraceConfig defaultPipelineConfig)
        (generateConstraints polySyms)
        buildSolvedProjectionFirst
  where
    buildSolvedProjectionFirst :: SolvedBuilder
    buildSolvedProjectionFirst _traceCfg pres = do
        solved <- fromSolveError (Solved.fromPresolutionResult pres)
        case Solved.validateOriginalCanonicalAgreement solved of
            [] -> Right solved
            mismatches ->
                Left (PipelineSolveError
                    (Solve.ValidationFailed mismatches))
