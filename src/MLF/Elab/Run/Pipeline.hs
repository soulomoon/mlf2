{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.Pipeline (
    runPipelineElab,
    runPipelineElabChecked,
    runPipelineElabWithConfig,
    runPipelineElabCheckedWithConfig
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Frontend.Syntax (Expr)
import MLF.Frontend.ConstraintGen (AnnExpr(..), ConstraintError, ConstraintResult(..), generateConstraints)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Acyclicity (checkAcyclicity)
import MLF.Constraint.Canonicalizer (canonicalizeNode)
import MLF.Constraint.Presolution
    ( PresolutionResult(..)
    , computePresolution
    )
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve
import MLF.Constraint.Types.Graph (PolySyms)
import MLF.Elab.Elaborate (elaborateWithScope)
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
import MLF.Elab.Run.Annotation (applyRedirectsToAnn, canonicalizeAnn)
import MLF.Elab.Run.Generalize
    ( constraintForGeneralization
    , generalizeAtWithBuilder
    , instantiationCopyNodes
    , pruneBindParentsConstraint
    )
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.Scope (letScopeOverrides)
import MLF.Elab.Run.Util
    ( canonicalizeExpansion
    , canonicalizeTrace
    , canonicalizeWitness
    , makeCanonicalizer
    )
import MLF.Elab.Run.ResultType (ResultTypeContext(..), computeResultTypeFromAnn, computeResultTypeFallback)
import MLF.Util.Trace (TraceConfig)

runPipelineElab :: PolySyms -> Expr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElab = runPipelineElabWithConfig defaultPipelineConfig

runPipelineElabChecked :: PolySyms -> Expr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabChecked = runPipelineElabCheckedWithConfig defaultPipelineConfig

runPipelineElabWithConfig :: PipelineConfig -> PolySyms -> Expr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWithConfig config polySyms =
    runPipelineElabWith (pcTraceConfig config) (generateConstraints polySyms)

runPipelineElabCheckedWithConfig :: PipelineConfig -> PolySyms -> Expr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabCheckedWithConfig config polySyms expr = do
    (term, _ty) <- runPipelineElabWithConfig config polySyms expr
    tyChecked <- fromTypeCheckError (typeCheck term)
    pure (term, tyChecked)

runPipelineElabWith
    :: TraceConfig
    -> (Expr -> Either ConstraintError ConstraintResult)
    -> Expr
    -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWith traceCfg genConstraints expr = do
    ConstraintResult { crConstraint = c0, crAnnotated = ann } <- fromConstraintError (genConstraints expr)
    let c1 = normalize c0
    acyc <- fromCycleError (checkAcyclicity c1)
    pres <- fromPresolutionError (computePresolution traceCfg acyc c1)
    let planBuilder = prPlanBuilder pres
        generalizeAtWith = generalizeAtWithBuilder planBuilder
    solved <- fromSolveError (solveUnify traceCfg (prConstraint pres))
    let solvedClean = solved { srConstraint = pruneBindParentsConstraint (srConstraint solved) }
    case validateSolvedGraphStrict solvedClean of
        [] -> do
            let canonNode = makeCanonicalizer (srUnionFind solvedClean) (prRedirects pres)
                adoptNode = canonicalizeNode canonNode
                instCopyNodes =
                    instantiationCopyNodes solvedClean (prRedirects pres) (prEdgeTraces pres)
                instCopyMapFull =
                    let baseNamedKeysAll = collectBaseNamedKeys c1
                        traceMaps = map (buildTraceCopyMap c1 baseNamedKeysAll adoptNode)
                                         (IntMap.elems (prEdgeTraces pres))
                    in foldl' IntMap.union IntMap.empty traceMaps
                (constraintForGen, bindParentsGa) =
                    constraintForGeneralization traceCfg solvedClean (prRedirects pres) instCopyNodes instCopyMapFull c1 ann
            let solvedForGen = solvedClean { srConstraint = constraintForGen }
            let ann' = applyRedirectsToAnn (prRedirects pres) ann
            let annCanon = canonicalizeAnn (canonicalizeNode canonNode) ann'
            let edgeWitnesses = IntMap.map (canonicalizeWitness canonNode) (prEdgeWitnesses pres)
                edgeTraces = IntMap.map (canonicalizeTrace canonNode) (prEdgeTraces pres)
                edgeExpansions = IntMap.map (canonicalizeExpansion canonNode) (prEdgeExpansions pres)
            let scopeOverrides = letScopeOverrides c1 (srConstraint solvedForGen) solvedClean (prRedirects pres) annCanon
            term <- fromElabError (elaborateWithScope traceCfg generalizeAtWith solvedClean solvedClean solvedForGen bindParentsGa edgeWitnesses edgeTraces edgeExpansions scopeOverrides annCanon)

            -- Build context for result type computation
            let canonical = frWith (srUnionFind solvedClean)
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

            -- Compute result type
            case annCanon of
                AAnn inner annNodeId eid -> do
                    ty <- fromElabError (computeResultTypeFromAnn resultTypeCtx inner inner annNodeId eid)
                    pure (term, ty)
                _ -> do
                    ty <- fromElabError (computeResultTypeFallback resultTypeCtx annCanon ann)
                    pure (term, ty)
        vs -> Left (PipelineSolveError (Solve.ValidationFailed vs))
