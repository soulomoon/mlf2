module MLF.Elab.Pipeline
  ( ElabType,
    Ty
      ( TVarRef,
        TArrow,
        TConWithIdentity,
        TCon,
        TVarAppRef,
        TBaseWithIdentity,
        TBase,
        TForallRef,
        TMuRef,
        TBottom
      ),
    BoundType,
    UniqueIdentity (..),
    TypeBinderIdentity,
    typeBinderIdentityFromUnique,
    TypeBinderRef,
    typeBinderRefFromIdentity,
    typeBinderRefIdentity,
    typeBinderRefNode,
    typeBinderRefName,
    typeBinderRefsSameIdentity,
    ElabScheme,
    mkElabSchemeWithRefs,
    schemeBinderRefs,
    schemeBody,
    XmlfTerm (..),
    Instantiation (..),
    instAbstrWithRef,
    instUnderWithRef,
    ElabError (..),
    TypeCheckError (..),
    PipelineConfig (..),
    defaultPipelineConfig,
    TraceConfig (..),
    defaultTraceConfig,
    PipelineError (..),
    renderPipelineError,
    liftPipelineError,
    fromConstraintError,
    fromCycleError,
    fromPresolutionError,
    fromSolveError,
    fromElabError,
    fromTypeCheckError,
    Pretty (..),
    PrettyDisplay (..),
    reifyType,
    generalizeAtWithBuilder,
    schemeToType,
    applyInstantiation,
    schemeFromType,
    freeTypeVarsType,
    sigmaReorder,

    -- * Witness translation (production path)
    phiFromEdgeWitnessWithTrace,
    runPipelineElab,
    runPipelineElabWithConfig,
    runPipelineElabWithEnv,
    runPipelineElabWithConfigAndEnv,
    ExternalEnv,
    applyRedirectsToAnn,
    canonicalizeAnn,

    -- * Exported for testing/debugging
    authoritativeRootAnn,
    chaseRedirects,
    SchemeInfo (..),
    schemeInfoFromRefSubst,
    Env (..),
    namedNodes,
    freshenTypeAbsAgainstEnv,

    -- * Context representation for non-spine Raise (paper Fig. 10)
    ContextStep (StepUnderRef, StepInside),
    contextToNodeBound,
    selectMinPrecInsertionIndex,

    -- * Phase 7 helpers
    mkTypeCheckEnvWithResolvedTerms,
    resolvedTermEnvEntries,
    insertResolvedTermBinding,
    insertTypeBindingRef,
    restrictResolvedTermBindings,
    unionEnvs,
    typeCheck,
    typeCheckWithEnv,
    checkInstantiation,
    isValue,
    step,
    normalize,
  )
where

import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.Phi (contextToNodeBound, phiFromEdgeWitnessWithTrace)
import MLF.Elab.PipelineConfig (PipelineConfig (..), defaultPipelineConfig)
import MLF.Elab.PipelineError
  ( PipelineError (..),
    fromConstraintError,
    fromCycleError,
    fromElabError,
    fromPresolutionError,
    fromSolveError,
    fromTypeCheckError,
    liftPipelineError,
    renderPipelineError,
  )
import MLF.Elab.Reduce (isValue, normalize, step)
import MLF.Elab.Run
  ( applyRedirectsToAnn,
    authoritativeRootAnn,
    canonicalizeAnn,
    chaseRedirects,
    freshenTypeAbsAgainstEnv,
    runPipelineElab,
    runPipelineElabWithConfig,
    runPipelineElabWithConfigAndEnv,
    runPipelineElabWithEnv,
  )
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Sigma (sigmaReorder)
import MLF.Elab.TypeCheck
  ( Env (..),
    checkInstantiation,
    insertResolvedTermBinding,
    insertTypeBindingRef,
    mkTypeCheckEnvWithResolvedTerms,
    resolvedTermEnvEntries,
    restrictResolvedTermBindings,
    typeCheck,
    typeCheckWithEnv,
    unionEnvs,
  )
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen (ExternalEnv)
import MLF.Reify.Core (namedNodes, reifyType)
import MLF.Reify.TypeOps (freeTypeVarsType)
import MLF.Util.Trace (TraceConfig (..), defaultTraceConfig)

-- `runPipelineElab` / redirect helpers live in `MLF.Elab.Run`.
