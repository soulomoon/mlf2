module MLF.Constraint.Presolution.TestSupport (
    PresolutionState(..),
    CopyMapping(..),
    CopyMap,
    lookupCopy,
    insertCopy,
    copiedNodes,
    originalNodes,
    InteriorNodes(..),
    fromListInterior,
    toListInterior,
    runPresolutionM,
    defaultPlanBuilder,
    decideMinimalExpansion,
    processInstEdge,
    validateReplayMapTraceContract,
    unifyAcyclicRawWithRaiseTrace,
    runEdgeUnifyForTest,
    instantiateScheme,
    instantiateSchemeWithTrace,
    mergeExpansions,
    applyExpansion,
    normalizeEdgeWitnessesM,
    validateTranslatablePresolution,
    translatableWeakenedNodes
) where

import MLF.Constraint.Presolution.Base
    ( CopyMap
    , CopyMapping(..)
    , InteriorNodes(..)
    , PresolutionPlanBuilder(..)
    , PresolutionState(..)
    , copiedNodes
    , fromListInterior
    , insertCopy
    , lookupCopy
    , originalNodes
    , runPresolutionM
    , toListInterior
    )
import MLF.Constraint.Presolution.Copy
    ( instantiateScheme
    , instantiateSchemeWithTrace
    )
import MLF.Constraint.Presolution.Driver
    ( validateReplayMapTraceContract
    )
import MLF.Constraint.Presolution.EdgeProcessing (processInstEdge)
import MLF.Constraint.Presolution.EdgeUnify (runEdgeUnifyForTest)
import MLF.Constraint.Presolution.Expansion
    ( applyExpansion
    , decideMinimalExpansion
    , mergeExpansions
    )
import MLF.Constraint.Presolution.Plan (buildGeneralizePlans)
import MLF.Constraint.Presolution.Unify (unifyAcyclicRawWithRaiseTrace)
import MLF.Constraint.Presolution.Validation
    ( translatableWeakenedNodes
    , validateTranslatablePresolution
    )
import MLF.Constraint.Presolution.WitnessNorm (normalizeEdgeWitnessesM)
import MLF.Util.Trace (TraceConfig)

defaultPlanBuilder :: TraceConfig -> PresolutionPlanBuilder
defaultPlanBuilder traceCfg = PresolutionPlanBuilder (buildGeneralizePlans traceCfg)
