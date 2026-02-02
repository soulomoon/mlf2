{- |
Module      : MLF.Constraint.Types
Description : Facade for core constraint, witness, and presolution types
Copyright   : (c) 2024
License     : BSD-3-Clause

This module re-exports the core constraint graph types alongside witness and
presolution types, preserving a compatibility layer for existing imports.
-}
module MLF.Constraint.Types (
    NodeId (..),
    NodeMap (..),
    lookupNode,
    insertNode,
    deleteNode,
    fromListNode,
    toListNode,
    ExpVarId (..),
    EdgeId (..),
    BaseTy (..),
    TyNode (..),
    structuralChildren,
    structuralChildrenWithBounds,
    lookupNodeIn,
    InstEdge (..),
    UnifyEdge (..),
    Constraint (..),
    maxNodeIdKeyOr0,
    GenNodeId (..),
    GenNodeMap (..),
    lookupGen,
    insertGen,
    deleteGen,
    fromListGen,
    toListGen,
    GenNode (..),
    NodeRef (..),
    typeRef,
    genRef,
    nodeRefKey,
    nodeRefFromKey,
    typeRefKey,
    genRefKey,
    genNodeKey,
    -- * Variable bounds + elimination stores (scope-model retirement)
    EliminatedVars,
    WeakenedVars,
    PolySyms,
    BoundRef(..),
    ForallSpec(..),
    Expansion (..),
    ExpansionF (..),
    InstanceOp(..),
    InstanceStep(..),
    InstanceWitness(..),
    EdgeWitness(..),
    Presolution (..),
    SolverState (..),
    DepGraph (..),
    -- * Binding tree types
    BindFlag (..),
    BindParents,
    -- * Binding tree errors
    BindingError (..)
) where

import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Presolution (DepGraph(..), Presolution(..), SolverState(..))
import MLF.Constraint.Types.Witness
    ( BoundRef(..)
    , EdgeWitness(..)
    , Expansion(..)
    , ExpansionF(..)
    , ForallSpec(..)
    , InstanceOp(..)
    , InstanceStep(..)
    , InstanceWitness(..)
    )
