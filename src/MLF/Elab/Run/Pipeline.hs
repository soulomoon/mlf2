{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.Pipeline (
    runPipelineElab,
    runPipelineElabChecked,
    runPipelineElabWithConfig,
    runPipelineElabCheckedWithConfig
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
    , PresolutionView(..)
    , computePresolution
    , EdgeTrace(..)
    )
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph (PolySyms)
import MLF.Constraint.Types (Constraint, NodeId(..), cNodes, lookupNodeIn)
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))
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
import MLF.Elab.Run.ResultType (ResultTypeInputs(..), computeResultTypeFromAnn, computeResultTypeFallback)
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
    ConstraintResult { crConstraint = c0, crAnnotated = ann } <- fromConstraintError (genConstraints expr)
    let c1 = normalize c0
    acyc <- fromCycleError (checkAcyclicity c1)
    pres <- fromPresolutionError (computePresolution traceCfg acyc c1)
    let planBuilder = prPlanBuilder pres
        generalizeAtWith = generalizeAtWithBuilder planBuilder
        preRewrite = snapshotConstraint pres
        uf = sanitizeSnapshotUf preRewrite (snapshotUnionFind pres)
    solvedView <- fromSolveError (Solved.fromPreRewriteState uf preRewrite)
    let setSolvedConstraint res c' = do
            replayed <- fromSolveError $
                solveResultFromSnapshot
                    SolveSnapshot
                        { snapUnionFind = Solved.canonicalMap res
                        , snapPreRewriteConstraint = c'
                        }
            let rebuilt = Solved.rebuildWithConstraint res (srConstraint replayed)
            case Solved.validateCanonicalGraphStrict rebuilt of
                [] -> pure rebuilt
                vs -> Left (PipelineSolveError (Solve.ValidationFailed vs))
        solvedClean = Solved.pruneBindParentsSolved solvedView
        solvedCleanView = solvedClean
        presolutionViewClean = solvedToPresolutionView solvedClean
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
                    instantiationCopyNodes presolutionViewClean (prRedirects pres) edgeTracesForCopy
                instCopyMapFull =
                    let baseNamedKeysAll = collectBaseNamedKeys c1
                        traceMaps = map (buildTraceCopyMap c1 baseNamedKeysAll adoptNode)
                                         (IntMap.elems edgeTracesForCopy)
                    in foldl' IntMap.union IntMap.empty traceMaps
                (constraintForGen, bindParentsGa) =
                    constraintForGeneralization traceCfg presolutionViewClean (prRedirects pres) instCopyNodes instCopyMapFull c1 ann
            solvedForGen <- setSolvedConstraint solvedClean constraintForGen
            let solvedForGenView = solvedForGen
                presolutionViewForGen = solvedToPresolutionView solvedForGen
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
                    , ecSolved = solvedForGen
                    }
                elabEnv = ElabEnv
                    { eePresolutionView = presolutionViewForGen
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
            let canonical = pvCanonical presolutionViewForGen
                resultTypeInputs = ResultTypeInputs
                    { rtcCanonical = canonical
                    , rtcEdgeWitnesses = edgeWitnesses
                    , rtcEdgeTraces = edgeTraces
                    , rtcEdgeExpansions = edgeExpansions
                    , rtcPresolutionView = presolutionViewForGen
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
                    _ <- fromElabError (computeResultTypeFromAnn resultTypeInputs inner inner annNodeId eid)
                    checkedAuthoritative
                _ -> do
                    _ <- fromElabError (computeResultTypeFallback resultTypeInputs annCanon ann)
                    checkedAuthoritative
        vs -> Left (PipelineSolveError (Solve.ValidationFailed vs))

sanitizeSnapshotUf :: Constraint -> IntMap.IntMap NodeId -> IntMap.IntMap NodeId
sanitizeSnapshotUf preRewrite =
    IntMap.mapMaybeWithKey keepLive
  where
    isLive nid = case lookupNodeIn (cNodes preRewrite) nid of
        Just _ -> True
        Nothing -> False

    keepLive key rep =
        let keyNode = NodeId key
        in if isLive keyNode && isLive rep && keyNode /= rep
            then Just rep
            else Nothing

solvedToPresolutionView :: Solved.Solved -> PresolutionView
solvedToPresolutionView solved =
    PresolutionView
        { pvConstraint = Solved.originalConstraint solved
        , pvCanonicalMap = Solved.canonicalMap solved
        , pvCanonical = Solved.canonical solved
        , pvLookupNode = Solved.lookupNode solved
        , pvLookupVarBound = Solved.lookupVarBound solved
        , pvLookupBindParent = Solved.lookupBindParent solved
        , pvBindParents = Solved.bindParents solved
        , pvCanonicalConstraint = Solved.canonicalConstraint solved
        }
