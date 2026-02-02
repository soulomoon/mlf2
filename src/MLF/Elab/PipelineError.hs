module MLF.Elab.PipelineError (
    PipelineError(..),
    renderPipelineError,
    liftPipelineError,
    fromConstraintError,
    fromCycleError,
    fromPresolutionError,
    fromSolveError,
    fromElabError,
    fromTypeCheckError
) where

import MLF.Constraint.Acyclicity (CycleError)
import MLF.Constraint.Presolution (PresolutionError)
import MLF.Constraint.Solve (SolveError)
import MLF.Elab.Types (ElabError, TypeCheckError)
import MLF.Frontend.ConstraintGen (ConstraintError)

data PipelineError
    = PipelineConstraintError ConstraintError
    | PipelineCycleError CycleError
    | PipelinePresolutionError PresolutionError
    | PipelineSolveError SolveError
    | PipelineElabError ElabError
    | PipelineTypeCheckError TypeCheckError
    deriving (Eq, Show)

renderPipelineError :: PipelineError -> String
renderPipelineError err = case err of
    PipelineConstraintError e ->
        "Phase 1 (constraint generation): " ++ show e
    PipelineCycleError e ->
        "Phase 3 (acyclicity): " ++ show e
    PipelinePresolutionError e ->
        "Phase 4 (presolution): " ++ show e
    PipelineSolveError e ->
        "Phase 5 (solve): " ++ show e
    PipelineElabError e ->
        "Phase 6 (elaboration): " ++ show e
    PipelineTypeCheckError e ->
        "Phase 7 (type checking): " ++ show e

liftPipelineError :: (e -> PipelineError) -> Either e a -> Either PipelineError a
liftPipelineError tag = either (Left . tag) Right

fromConstraintError :: Either ConstraintError a -> Either PipelineError a
fromConstraintError = liftPipelineError PipelineConstraintError

fromCycleError :: Either CycleError a -> Either PipelineError a
fromCycleError = liftPipelineError PipelineCycleError

fromPresolutionError :: Either PresolutionError a -> Either PipelineError a
fromPresolutionError = liftPipelineError PipelinePresolutionError

fromSolveError :: Either SolveError a -> Either PipelineError a
fromSolveError = liftPipelineError PipelineSolveError

fromElabError :: Either ElabError a -> Either PipelineError a
fromElabError = liftPipelineError PipelineElabError

fromTypeCheckError :: Either TypeCheckError a -> Either PipelineError a
fromTypeCheckError = liftPipelineError PipelineTypeCheckError
