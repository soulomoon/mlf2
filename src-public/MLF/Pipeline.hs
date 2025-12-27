module MLF.Pipeline
    ( inferConstraintGraph
    , runPipelineElab
    ) where

import MLF.Frontend.Syntax (Expr)
import MLF.Frontend.ConstraintGen (ConstraintError, ConstraintResult, generateConstraints)
import MLF.Elab.Pipeline (runPipelineElab)

inferConstraintGraph :: Expr -> Either ConstraintError ConstraintResult
inferConstraintGraph = generateConstraints
