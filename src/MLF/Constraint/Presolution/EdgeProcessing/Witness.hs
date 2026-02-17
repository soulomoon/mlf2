{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Witness
Description : Edge witness construction helpers
Copyright   : (c) 2024
License     : BSD-3-Clause

Helpers for constructing per-edge witnesses and traces during presolution.
-}
module MLF.Constraint.Presolution.EdgeProcessing.Witness (
    EdgeWitnessPlan(..),
    edgeWitnessPlan,
    buildEdgeWitness,
    buildEdgeTrace,
    dropWeakenSteps
) where

import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (
    CopyMap,
    EdgeTrace(..),
    FrontierSet,
    InteriorSet,
    PresolutionM,
    fromListInterior
    )
import MLF.Constraint.Presolution.Ops (findRoot)
import MLF.Constraint.Presolution.StateAccess (
    getConstraintAndCanonical,
    liftBindingError
    )
import MLF.Constraint.Presolution.Witness (
    binderArgsFromExpansion,
    integratePhase2Steps,
    witnessFromExpansion
    )

{- Note [Edge witness emission]
The edge witness combines two sources of information:

  - the Omega steps induced directly by the chosen expansion recipe, and
  - the Phase-2 instance ops (Raise/Merge/Weaken variants) emitted by
    edge-local unification.

We keep both the interleaved step list (`ewSteps`) and the Omega-only
projection (`ewWitness`) so that later normalization can reconcile them in a
paper-faithful way (see `papers/these-finale-english.txt` and
`papers/xmlf.txt` Fig. 10).
-}

{- Note [Weaken suppression on annotation edges]
Some instantiation edges correspond to user annotations (recorded in
`cAnnEdges`). For those edges we suppress `OpWeaken` steps derived from
`ExpInstantiate` so the witness does not introduce implicit weakenings on
annotated paths. EdgeProcessing decides when to suppress and threads the flag
into `edgeWitnessPlan`.
-}

-- | Precompute the base (Omega) steps and ops for a witness.
data EdgeWitnessPlan = EdgeWitnessPlan
    { ewpBaseSteps :: [InstanceStep]
    , ewpBaseOps :: [InstanceOp]
    }

edgeWitnessPlan :: GenNodeId -> Bool -> NodeId -> TyNode -> Expansion -> PresolutionM EdgeWitnessPlan
edgeWitnessPlan gid suppressWeaken leftId leftRaw expn = do
    let root = case leftRaw of
            TyExp{ tnBody = b } -> b
            _ -> leftId
    baseSteps0 <- witnessFromExpansion gid root leftRaw expn
    let baseSteps = if suppressWeaken then dropWeakenSteps baseSteps0 else baseSteps0
        baseOps = [op | StepOmega op <- baseSteps]
    pure EdgeWitnessPlan { ewpBaseSteps = baseSteps, ewpBaseOps = baseOps }

-- | Build an edge witness from the chosen expansion recipe and extra ops.
buildEdgeWitness
    :: EdgeId
    -> NodeId
    -> NodeId
    -> TyNode
    -> [InstanceStep]
    -> [InstanceOp]
    -> PresolutionM EdgeWitness
buildEdgeWitness eid left right leftRaw baseSteps extraOps = do
    let root = case leftRaw of
            TyExp{ tnBody = b } -> b
            _ -> left
        steps0 = integratePhase2Steps baseSteps extraOps
        ops = [op | StepOmega op <- steps0]
        iw = InstanceWitness ops
    pure EdgeWitness
        { ewEdgeId = eid
        , ewLeft = left
        , ewRight = right
        , ewRoot = root
        , ewSteps = steps0
        , ewWitness = iw
        }

-- | Remove Weaken steps from a witness step list.
dropWeakenSteps :: [InstanceStep] -> [InstanceStep]
dropWeakenSteps = filter (not . isWeakenStep)
  where
    isWeakenStep step = case step of
        StepOmega OpWeaken{} -> True
        _ -> False

-- | Build an edge trace.
buildEdgeTrace
    :: GenNodeId
    -> EdgeId
    -> NodeId
    -> TyNode
    -> Expansion
    -> (CopyMap, InteriorSet, FrontierSet)
    -> PresolutionM EdgeTrace
buildEdgeTrace gid _eid left leftRaw expn (copyMap0, _interior0, _frontier0) = do
    bas <- binderArgsFromExpansion gid leftRaw expn
    -- Paper root `r` for Φ/Σ is the TyExp body, not the TyExp wrapper itself.
    let rootSeed = case leftRaw of
            TyExp{ tnBody = b } -> b
            _ -> left
    root <- findRoot rootSeed
    (c0, canonical) <- getConstraintAndCanonical
    let interiorRootRef = genRef gid
    interiorRaw <- do
        s <- liftBindingError $ Binding.interiorOfUnder canonical c0 interiorRootRef
        pure
            [ nid
            | key <- IntSet.toList s
            , TypeRef nid <- [nodeRefFromKey key]
            ]
    let interior = fromListInterior (map canonical interiorRaw)
    pure EdgeTrace
        { etRoot = root
        , etBinderArgs = bas
        , etInterior = interior
        , etBinderReplayHints = mempty
        , etCopyMap = copyMap0
        }
