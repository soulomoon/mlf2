{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Plan
Description : Edge plan types for presolution edge execution
Copyright   : (c) 2024
License     : BSD-3-Clause

Typed plan model for the two-pass edge processing architecture.
After planner fail-fast, all valid plans reaching execution are TyExp-left and
run through expansion semantics.
-}
module MLF.Constraint.Presolution.EdgeProcessing.Plan (
    -- * Refined TyExp-left payload
    ResolvedTyExp (..),
    mkResolvedTyExp,
    resolvedTyExpNode,
    -- * Resolved plan
    EdgePlan (..),
    -- * Helpers
    edgePlanEdge,
    mkEmptyResolvedPlan,
) where

import MLF.Constraint.Types (ExpVarId, InstEdge, NodeId, TyNode (..))

-- | Refined TyExp payload used by resolved edge plans.
data ResolvedTyExp = ResolvedTyExp
    { rteNodeId :: NodeId
    , rteExpVar :: ExpVarId
    , rteBodyId :: NodeId
    }
    deriving (Eq, Show)

-- | Refine a raw node into a TyExp payload.
mkResolvedTyExp :: TyNode -> Maybe ResolvedTyExp
mkResolvedTyExp TyExp { tnId = nid, tnExpVar = s, tnBody = body } =
    Just ResolvedTyExp
        { rteNodeId = nid
        , rteExpVar = s
        , rteBodyId = body
        }
mkResolvedTyExp _ = Nothing

-- | Recover the corresponding raw TyExp node.
resolvedTyExpNode :: ResolvedTyExp -> TyNode
resolvedTyExpNode ResolvedTyExp { rteNodeId = nid, rteExpVar = s, rteBodyId = body } =
    TyExp { tnId = nid, tnExpVar = s, tnBody = body }

-- | Resolved edge plan consumed by the interpreter.
data EdgePlan = EdgePlanResolved
    { eprEdge :: InstEdge
    , eprLeftTyExp :: ResolvedTyExp
    , eprRightNode :: TyNode
    , eprLeftCanonical :: NodeId
    , eprRightCanonical :: NodeId
    , eprAllowTrivial :: Bool
    , eprSuppressWeaken :: Bool
    }
    deriving (Eq, Show)

-- | Extract the original edge from a plan.
edgePlanEdge :: EdgePlan -> InstEdge
edgePlanEdge (EdgePlanResolved { eprEdge = e }) = e

-- | Construct a minimal resolved plan for testing.
mkEmptyResolvedPlan ::
    InstEdge ->
    ResolvedTyExp ->
    TyNode ->
    NodeId ->
    NodeId ->
    EdgePlan
mkEmptyResolvedPlan edge leftTyExp rightNode leftCan rightCan =
    EdgePlanResolved
        { eprEdge = edge
        , eprLeftTyExp = leftTyExp
        , eprRightNode = rightNode
        , eprLeftCanonical = leftCan
        , eprRightCanonical = rightCan
        , eprAllowTrivial = True
        , eprSuppressWeaken = False
        }
