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
import MLF.Constraint.Canonicalizer (Canonicalizer, canonicalizeNode)
import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Presolution
    ( PresolutionResult(..)
    , computePresolution
    , EdgeTrace(..)
    )
import MLF.Constraint.Presolution.View (PresolutionView, pvCanonical)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types (PolySyms, cNodes, lookupNodeIn)
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
import MLF.Elab.TermClosure (closeTermWithSchemeSubstIfNeeded, substInTerm)
import MLF.Elab.Run.Annotation (annNode, redirectAndCanonicalizeAnn)
import MLF.Elab.Run.Generalize
    ( constraintForGeneralization
    , generalizeAtWithBuilder
    , instantiationCopyNodes
    )
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.Scope
    ( letScopeOverrides
    , resolveCanonicalScope
    , schemeBodyTarget
    )
import MLF.Elab.Run.Util
    ( canonicalizeExpansion
    , canonicalizeTrace
    , canonicalizeWitness
    , makeCanonicalizer
    )
import MLF.Elab.Run.ResultType (mkResultTypeInputs, computeResultTypeFromAnn, computeResultTypeFallback)
import MLF.Reify.TypeOps (freeTypeVarsType)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data SnapshotViews = SnapshotViews
    { svSolvedClean :: Solved.Solved
    , svPresolutionViewClean :: PresolutionView
    , svCanonNode :: Canonicalizer
    }

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
    SnapshotViews
        { svSolvedClean = solvedClean
        , svPresolutionViewClean = presolutionViewClean
        , svCanonNode = canonNode
        } <- prepareSnapshotViews pres
    let planBuilder = prPlanBuilder pres
    let
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
    presolutionViewForGen <- fromSolveError (Finalize.finalizePresolutionViewFromSnapshot constraintForGen (Solved.canonicalMap solvedClean))
    let
        generalizeAtWithView mbGa =
            generalizeAtWithBuilder
                planBuilder
                mbGa
                presolutionViewForGen
    let annCanon = redirectAndCanonicalizeAnn (canonicalizeNode canonNode) (prRedirects pres) ann
    let edgeWitnesses = IntMap.map (canonicalizeWitness canonNode) (prEdgeWitnesses pres)
        edgeTraces = IntMap.map (canonicalizeTrace canonNode) (prEdgeTraces pres)
        edgeExpansions = IntMap.map (canonicalizeExpansion canonNode) (prEdgeExpansions pres)
    let scopeOverrides =
            letScopeOverrides
                c1
                constraintForGen
                presolutionViewClean
                (prRedirects pres)
                annCanon
    let elabConfig = ElabConfig
            { ecTraceConfig = traceCfg
            , ecGeneralizeAtWith = generalizeAtWithView
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
            resolveCanonicalScope c1 presolutionViewForGen (prRedirects pres) (annNode ann)
    let rootTarget = schemeBodyTarget presolutionViewForGen (annNode annCanon)
    (rootScheme, rootSubst) <- fromElabError $
        generalizeAtWithView (Just bindParentsGa) rootScope rootTarget
    let termSubst = substInTerm rootSubst term

    -- Build context for result type computation
    let canonical = pvCanonical presolutionViewForGen
        resultTypeInputs =
            mkResultTypeInputs
                canonical
                edgeWitnesses
                edgeTraces
                edgeExpansions
                presolutionViewForGen
                bindParentsGa
                planBuilder
                c1
                (prRedirects pres)
                traceCfg
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
                                in closeTermWithSchemeSubstIfNeeded IntMap.empty freeScheme termSubst
                        _ -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
                Left _ -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
    let checkedAuthoritative = do
            tyChecked <- fromTypeCheckError (typeCheck termClosed)
            pure (termClosed, tyChecked)

    -- Keep result-type reconstruction for diagnostics, but report the
    -- type-checker result as authoritative.
    case annCanon of
        AAnn inner annNodeId eid -> do
            _ <- fromElabError (computeResultTypeFromAnn resultTypeInputs inner inner annNodeId eid)
            checkedAuthoritative
        _ -> do
            _ <- fromElabError (computeResultTypeFallback resultTypeInputs annCanon ann)
            checkedAuthoritative

prepareSnapshotViews :: PresolutionResult -> Either PipelineError SnapshotViews
prepareSnapshotViews pres = do
    let preRewrite = snapshotConstraint pres
    solvedClean <- fromSolveError (Finalize.finalizeSolvedFromSnapshot preRewrite (snapshotUnionFind pres))
    presolutionViewClean <- fromSolveError (Finalize.finalizePresolutionViewFromSnapshot preRewrite (snapshotUnionFind pres))
    let canonNode = makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
    pure SnapshotViews
        { svSolvedClean = solvedClean
        , svPresolutionViewClean = presolutionViewClean
        , svCanonNode = canonNode
        }
