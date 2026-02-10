{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Plan
Description : Stage-indexed edge plan types with dual-mode discriminator
Copyright   : (c) 2024
License     : BSD-3-Clause

Typed plan model for the two-pass edge processing architecture.
Each instantiation edge is classified into an 'EdgePlanMode' that
determines which execution path the interpreter uses:

* 'ExpansionMode' — left node is TyExp; use decideMinimalExpansion + unifyStructure.
* 'LegacyDirectMode' — left node is non-TyExp; use solveNonExpInstantiation.

The plan progresses through stages ('EdgeStage') from resolution to commitment.
-}
module MLF.Constraint.Presolution.EdgeProcessing.Plan (
    -- * Mode discriminator
    EdgePlanMode (..),
    -- * Stage index
    EdgeStage (..),
    -- * Stage-indexed plan
    EdgePlan (..),
    -- * Helpers
    edgePlanStage,
    edgePlanMode,
    edgePlanEdge,
    mkEmptyResolvedPlan,
) where

import MLF.Constraint.Types (InstEdge, TyNode, NodeId)

-- | Execution mode for an edge plan.
data EdgePlanMode
    = ExpansionMode
    -- ^ Left node is TyExp. Use decideMinimalExpansion + unifyStructure.
    | LegacyDirectMode
    -- ^ Left node is non-TyExp. Use solveNonExpInstantiation.
    deriving (Eq, Ord, Show)

-- | Processing stage for an edge plan.
data EdgeStage
    = StageResolved
    -- ^ Edge operands resolved, mode classified, ready for execution.
    deriving (Eq, Ord, Show)

-- | A stage-indexed edge plan.
data EdgePlan (s :: EdgeStage) where
    EdgePlanResolved ::
        { eprEdge :: InstEdge
        , eprLeftNode :: TyNode
        , eprRightNode :: TyNode
        , eprLeftCanonical :: NodeId
        , eprRightCanonical :: NodeId
        , eprMode :: EdgePlanMode
        , eprAllowTrivial :: Bool
        , eprSuppressWeaken :: Bool
        } -> EdgePlan 'StageResolved

deriving instance Eq (EdgePlan s)
deriving instance Show (EdgePlan s)

-- | Extract the stage tag from a plan.
edgePlanStage :: EdgePlan s -> EdgeStage
edgePlanStage EdgePlanResolved {} = StageResolved

-- | Extract the mode from a plan.
edgePlanMode :: EdgePlan s -> EdgePlanMode
edgePlanMode (EdgePlanResolved { eprMode = m }) = m

-- | Extract the original edge from a plan.
edgePlanEdge :: EdgePlan s -> InstEdge
edgePlanEdge (EdgePlanResolved { eprEdge = e }) = e

-- | Construct a minimal resolved plan for testing.
mkEmptyResolvedPlan :: InstEdge -> TyNode -> TyNode -> NodeId -> NodeId -> EdgePlanMode -> EdgePlan 'StageResolved
mkEmptyResolvedPlan edge leftNode rightNode leftCan rightCan mode = EdgePlanResolved
    { eprEdge = edge
    , eprLeftNode = leftNode
    , eprRightNode = rightNode
    , eprLeftCanonical = leftCan
    , eprRightCanonical = rightCan
    , eprMode = mode
    , eprAllowTrivial = True
    , eprSuppressWeaken = False
    }
