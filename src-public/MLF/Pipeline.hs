module MLF.Pipeline
    ( BaseTy (..)
    , PolySyms
    , PipelineConfig(..)
    , defaultPipelineConfig
    , TraceConfig(..)
    , defaultTraceConfig
    , PipelineError(..)
    , renderPipelineError
    , inferConstraintGraph
    , runPipelineElab
    , runPipelineElabChecked
    , runPipelineElabWithConfig
    , runPipelineElabCheckedWithConfig
    , typeCheck
    , step
    , normalize
    , isValue
    ) where

import MLF.Frontend.Syntax (SurfaceExpr)
import MLF.Frontend.ConstraintGen (ConstraintError, ConstraintResult, generateConstraints)
import MLF.Constraint.Types.Graph (BaseTy(..), PolySyms)
import MLF.Elab.Pipeline
    ( PipelineConfig(..)
    , PipelineError(..)
    , defaultPipelineConfig
    , TraceConfig(..)
    , defaultTraceConfig
    , isValue
    , normalize
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabChecked
    , runPipelineElabWithConfig
    , runPipelineElabCheckedWithConfig
    , step
    , typeCheck
    )

inferConstraintGraph :: PolySyms -> SurfaceExpr -> Either ConstraintError ConstraintResult
inferConstraintGraph = generateConstraints
