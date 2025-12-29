module MLF.Pipeline
    ( inferConstraintGraph
    , runPipelineElab
    , runPipelineElabChecked
    , typeCheck
    , step
    , normalize
    , isValue
    ) where

import MLF.Frontend.Syntax (Expr)
import MLF.Frontend.ConstraintGen (ConstraintError, ConstraintResult, generateConstraints)
import MLF.Elab.Pipeline (isValue, normalize, runPipelineElab, runPipelineElabChecked, step, typeCheck)

inferConstraintGraph :: Expr -> Either ConstraintError ConstraintResult
inferConstraintGraph = generateConstraints
