module MLF.Pipeline
    ( BaseTy (..)
    , PolySyms
    , inferConstraintGraph
    , runPipelineElab
    , runPipelineElabChecked
    , typeCheck
    , step
    , normalize
    , isValue
    ) where

import MLF.Frontend.Syntax (Expr)
import MLF.Frontend.ConstraintGen (ConstraintError, ConstraintResult, generateConstraints)
import MLF.Constraint.Types (BaseTy(..), PolySyms)
import MLF.Elab.Pipeline (isValue, normalize, runPipelineElab, runPipelineElabChecked, step, typeCheck)

inferConstraintGraph :: PolySyms -> Expr -> Either ConstraintError ConstraintResult
inferConstraintGraph = generateConstraints
