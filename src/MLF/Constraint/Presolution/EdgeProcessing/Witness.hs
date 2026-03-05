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
    buildEdgeTrace
) where

import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types
import MLF.Constraint.Types.Witness (ReplayContract(..))
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

  - the Omega ops and forall-intro count induced directly by the chosen
    expansion recipe, and
  - the Phase-2 instance ops (Raise/Merge/Weaken variants) emitted by
    edge-local unification.

We keep the forall-intro count (`ewForallIntros`) separate from the Omega-only
ops (`ewWitness`) so that later Phi translation can apply O as a prefix of
InstIntro steps, then replay Omega ops independently (thesis Def. 15.3.4).
-}

-- | Precompute the base forall-intro count and ops for a witness.
data EdgeWitnessPlan = EdgeWitnessPlan
    { ewpForallIntros :: Int
    , ewpBaseOps :: [InstanceOp]
    }

edgeWitnessPlan :: GenNodeId -> NodeId -> TyNode -> Expansion -> PresolutionM EdgeWitnessPlan
edgeWitnessPlan gid leftId leftRaw expn = do
    let root = case leftRaw of
            TyExp{ tnBody = b } -> b
            _ -> leftId
    (introCount, baseOps) <- witnessFromExpansion gid root leftRaw expn
    pure EdgeWitnessPlan { ewpForallIntros = introCount, ewpBaseOps = baseOps }

-- | Build an edge witness from the chosen expansion recipe and extra ops.
buildEdgeWitness
    :: EdgeId
    -> NodeId
    -> NodeId
    -> TyNode
    -> Int
    -> [InstanceOp]
    -> [InstanceOp]
    -> PresolutionM EdgeWitness
buildEdgeWitness eid left right leftRaw introCount baseOps extraOps = do
    let root = case leftRaw of
            TyExp{ tnBody = b } -> b
            _ -> left
        (intros, ops) = integratePhase2Steps (introCount, baseOps) extraOps
        iw = InstanceWitness ops
    pure EdgeWitness
        { ewEdgeId = eid
        , ewLeft = left
        , ewRight = right
        , ewRoot = root
        , ewForallIntros = intros
        , ewWitness = iw
        }

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
        , etReplayContract = ReplayContractNone
        , etBinderReplayMap = mempty
        , etCopyMap = copyMap0
        }
