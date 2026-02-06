{-# LANGUAGE PatternSynonyms #-}
module MLF.Elab.Pipeline (
    ElabType,
    Ty(..),
    BoundType,
    ElabScheme,
    pattern Forall,
    ElabTerm(..),
    Instantiation(..),
    ElabError(..),
    TypeCheckError(..),
    PipelineConfig(..),
    defaultPipelineConfig,
    TraceConfig(..),
    defaultTraceConfig,
    PipelineError(..),
    renderPipelineError,
    liftPipelineError,
    fromConstraintError,
    fromCycleError,
    fromPresolutionError,
    fromSolveError,
    fromElabError,
    fromTypeCheckError,
    Pretty(..),
    PrettyDisplay(..),
    elaborate,
    reifyType,
    reifyTypeWithNames,
    reifyTypeWithNamedSet,
    reifyBoundWithNames,
    generalizeAtWithBuilder,
    expansionToInst,
    schemeToType,
    applyInstantiation,
    schemeFromType,
    sigmaReorder,
    -- * Witness translation (production path)
    phiFromEdgeWitnessWithTrace,
    runPipelineElab,
    runPipelineElabChecked,
    runPipelineElabWithConfig,
    runPipelineElabCheckedWithConfig,
    applyRedirectsToAnn,
    -- * Exported for testing/debugging
    chaseRedirects,
    SchemeInfo(..),
    Env(..),
    namedNodes,
    -- * Context representation for non-spine Raise (paper Fig. 10)
    ContextStep(..),
    contextToNodeBound,
    selectMinPrecInsertionIndex,
    -- * Phase 7 helpers
    typeCheck,
    typeCheckWithEnv,
    checkInstantiation,
    isValue,
    step,
    normalize
) where

import MLF.Elab.Types
import MLF.Elab.Elaborate (elaborate)
import MLF.Elab.Legacy (expansionToInst)
import MLF.Elab.PipelineConfig (PipelineConfig(..), defaultPipelineConfig)
import MLF.Elab.PipelineError
    ( PipelineError(..)
    , renderPipelineError
    , liftPipelineError
    , fromConstraintError
    , fromCycleError
    , fromPresolutionError
    , fromSolveError
    , fromElabError
    , fromTypeCheckError
    )
import MLF.Util.Trace (TraceConfig(..), defaultTraceConfig)
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.TypeCheck (Env(..), checkInstantiation, typeCheck, typeCheckWithEnv)
import MLF.Elab.Reduce (isValue, normalize, step)
import MLF.Elab.Phi (contextToNodeBound, phiFromEdgeWitnessWithTrace)
import MLF.Reify.Core (namedNodes, reifyBoundWithNames, reifyType, reifyTypeWithNamedSet, reifyTypeWithNames)
import MLF.Elab.Run
    ( applyRedirectsToAnn
    , chaseRedirects
    , runPipelineElab
    , runPipelineElabChecked
    , runPipelineElabWithConfig
    , runPipelineElabCheckedWithConfig
    )
import MLF.Elab.Sigma (sigmaReorder)

-- `runPipelineElab` / redirect helpers live in `MLF.Elab.Run`.
