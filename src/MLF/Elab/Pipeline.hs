module MLF.Elab.Pipeline (
    ElabType(..),
    ElabScheme(..),
    ElabTerm(..),
    Instantiation(..),
    ElabError(..),
    TypeCheckError(..),
    Pretty(..),
    elaborate,
    reifyType,
    reifyTypeWithNames,
    reifyTypeWithNamedSet,
    reifyBoundWithNames,
    generalizeAt,
    expansionToInst,
    schemeToType,
    applyInstantiation,
    sigmaReorder,
    -- * Witness translation (for tests/debugging)
    phiFromEdgeWitness,
    phiFromEdgeWitnessWithTrace,
    runPipelineElab,
    runPipelineElabChecked,
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
import MLF.Elab.Elaborate (elaborate, expansionToInst)
import MLF.Elab.Generalize (generalizeAt)
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.TypeCheck (Env(..), checkInstantiation, typeCheck, typeCheckWithEnv)
import MLF.Elab.Reduce (isValue, normalize, step)
import MLF.Elab.Phi (contextToNodeBound, phiFromEdgeWitness, phiFromEdgeWitnessWithTrace)
import MLF.Elab.Reify (namedNodes, reifyBoundWithNames, reifyType, reifyTypeWithNamedSet, reifyTypeWithNames)
import MLF.Elab.Run (applyRedirectsToAnn, chaseRedirects, runPipelineElab, runPipelineElabChecked)
import MLF.Elab.Sigma (sigmaReorder)

-- `runPipelineElab` / redirect helpers live in `MLF.Elab.Run`.
