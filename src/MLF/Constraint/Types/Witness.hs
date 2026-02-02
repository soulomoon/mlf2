{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      : MLF.Constraint.Types.Witness
Description : Witness and expansion types for constraint solving
Copyright   : (c) 2024
License     : BSD-3-Clause

This module defines witness-focused constraint types so callers can depend on a
smaller surface area than "MLF.Constraint.Types".
-}
module MLF.Constraint.Types.Witness (
    NodeId(..),
    EdgeId(..),
    BoundRef(..),
    ForallSpec(..),
    Expansion(..),
    ExpansionF(..),
    InstanceOp(..),
    InstanceStep(..),
    InstanceWitness(..),
    EdgeWitness(..)
) where

import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.List.NonEmpty (NonEmpty)

import MLF.Constraint.Types.Graph (EdgeId(..), NodeId(..))

-- | Reference to a bound used in forall introductions.
--
-- BoundBinder uses a binder index so bounds can be remapped to fresh binders.
data BoundRef
    = BoundNode NodeId
    | BoundBinder Int
    deriving (Eq, Show)

-- | Binder specification for quantifier introduction.
data ForallSpec = ForallSpec
    { fsBinderCount :: Int
    , fsBounds :: [Maybe BoundRef]
    }
    deriving (Eq, Show)

-- | A presolution "expansion recipe" assigned to an `ExpVarId`.
--
-- These recipes are the bridge between the graphic-constraint world and xMLF:
-- they encode the *shape* of the instantiation we need (identity / ∀-intro /
-- elimination+substitution / composition). See `papers/these-finale-english.txt`
-- and `papers/xmlf.txt` Figures 1–3 for the instantiation grammar and §3.4 +
-- Figure 10 for witness translation.
data Expansion
    = ExpIdentity                    -- ^ Do nothing; reuse the underlying body.
    | ExpForall (NonEmpty ForallSpec)
        -- ^ Introduce one or more ∀ binders around the body (paper Q(n) shape).
        --
        -- Each `ForallSpec` describes the binder arity and per-binder bounds for
        -- one newly introduced quantifier.
        --
        -- Intended correspondence: repeated quantifier-introduction (`O`) / `InstIntro`.
    | ExpInstantiate [NodeId]
        -- ^ Eliminate outer quantifiers and substitute their binders.
        --
        -- The payload is a list of *NodeIds* (not yet reified types) that will be
        -- substituted for the binders, left-to-right. The arity is checked against
        -- the binders present at the quantified level(s).
        -- Intended correspondence: type application sugar ⟨τ⟩ (paper) which expands
        -- to (∀(⩾ τ); N) on types.
    | ExpCompose (NonEmpty Expansion)
        -- ^ Sequential composition of steps.
        -- Intended correspondence: `φ; φ′`.
    deriving (Eq, Show)

data ExpansionF a
    = ExpIdentityF
    | ExpForallF (NonEmpty ForallSpec)
    | ExpInstantiateF [NodeId]
    | ExpComposeF (NonEmpty a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base Expansion = ExpansionF

instance Recursive Expansion where
    project expn = case expn of
        ExpIdentity -> ExpIdentityF
        ExpForall specs -> ExpForallF specs
        ExpInstantiate args -> ExpInstantiateF args
        ExpCompose es -> ExpComposeF es

instance Corecursive Expansion where
    embed expn = case expn of
        ExpIdentityF -> ExpIdentity
        ExpForallF specs -> ExpForall specs
        ExpInstantiateF args -> ExpInstantiate args
        ExpComposeF es -> ExpCompose es

-- | Atomic instance operations used in instantiation witnesses.
--
-- In `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.4), solved
-- instantiation edges come with a *normalized*
-- witness Ω (a sequence of operations such as graft/merge/raise/weaken).
-- Figure 10 then translates these operations to an xMLF instantiation (Φ),
-- possibly preceded by a quantifier-reordering instantiation Σ(g).
--
-- We store the operations on `NodeId`s of our constraint graph.
--
-- We store "graft σ at n" as `(OpGraft sigmaRoot n)` where `sigmaRoot` is the
-- root `NodeId` of a (possibly shared) type subgraph in the constraint.
data InstanceOp
    = OpGraft NodeId NodeId
    | OpMerge NodeId NodeId
    -- | Paper Raise(n): raising a binding edge for an interior node.
    --
    -- `papers/these-finale-english.txt` translates this operation to an
    -- instantiation that (see `papers/xmlf.txt`)
    -- introduces a fresh quantifier one level higher and aliases/eliminates the
    -- original binder (Fig. 10, §3.4). The current presolution records Raise
    -- steps during instantiation-edge solving via binding-edge harmonization,
    -- including interior nodes (not just binders). Remaining alignment notes
    -- are tracked in `plans/merge_raise_merge_plan.txt`.
    | OpRaise NodeId
    | OpWeaken NodeId
    | OpRaiseMerge NodeId NodeId
    deriving (Eq, Show)

-- | A (normalized) instance-operation witness: a sequence of atomic operations.
--
-- Paper mapping (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4):
-- this corresponds to an instantiation
-- witness Ω. The paper assumes witnesses can be normalized (Yakobowski 2008)
-- before translating them with Φ (Figure 10). Our presolution records witnesses
-- in a form intended to be suitable for that translation.
newtype InstanceWitness = InstanceWitness { getInstanceOps :: [InstanceOp] }
    deriving (Eq, Show)

-- | A witness step that can interleave quantifier introductions with Ω ops.
data InstanceStep
    = StepOmega InstanceOp
    | StepIntro
    deriving (Eq, Show)

-- | Per-instantiation-edge witness metadata.
--
-- This is the data that Phase 6 uses to reconstruct an xMLF instantiation for
-- an application site.
--
-- Paper mapping (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4):
--   - `ewWitness` corresponds to a (normalized) witness Ω.
--   - `ewRoot` corresponds to the expansion root `r` in χₑ.
--   - `ewLeft`/`ewRight` are the endpoints of the original instantiation edge.
--   - The paper’s Φ(e) is Σ(g); Φχe(Ω). Our elaborator computes the Σ(g) part
--     using scheme information (binder order), and then translates Ω.
data EdgeWitness = EdgeWitness
    { ewEdgeId :: EdgeId
    , ewLeft :: NodeId
    , ewRight :: NodeId
    , ewRoot :: NodeId
    , ewSteps :: [InstanceStep]
        -- ^ Interleaved witness steps for this edge, in expansion order.
    , ewWitness :: InstanceWitness
    }
    deriving (Eq, Show)
