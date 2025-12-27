module MLF.Elab.Pipeline (
    ElabType(..),
    ElabScheme(..),
    ElabTerm(..),
    Instantiation(..),
    ElabError(..),
    Pretty(..),
    elaborate,
    reifyType,
    generalizeAt,
    expansionToInst,
    schemeToType,
    applyInstantiation,
    sigmaReorder,
    -- * Witness translation (for tests/debugging)
    phiFromEdgeWitness,
    phiFromEdgeWitnessWithTrace,
    runPipelineElab,
    applyRedirectsToAnn,
    -- * Exported for testing/debugging
    chaseRedirects,
    SchemeInfo(..),
    -- * Context representation for non-spine Raise (paper Fig. 10)
    ContextStep(..),
    contextToNodeBound,
    selectMinPrecInsertionIndex
) where

import MLF.Elab.Types
import MLF.Elab.Elaborate (elaborate, expansionToInst)
import MLF.Elab.Generalize (generalizeAt)
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.Phi (contextToNodeBound, phiFromEdgeWitness, phiFromEdgeWitnessWithTrace)
import MLF.Elab.Reify (reifyType)
import MLF.Elab.Run (applyRedirectsToAnn, chaseRedirects, runPipelineElab)
import MLF.Elab.Sigma (sigmaReorder)

-- `runPipelineElab` / redirect helpers live in `MLF.Elab.Run`.
