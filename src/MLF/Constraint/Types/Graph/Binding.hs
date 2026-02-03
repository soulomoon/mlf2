{- |
Module      : MLF.Constraint.Types.Graph.Binding
Description : Binding tree types for the constraint graph
Copyright   : (c) 2024
License     : BSD-3-Clause

This module defines binding-related types for the constraint graph:

* 'BindFlag' - Flexible vs rigid binding edges
* 'BindParents' - The binding tree as child -> (parent, flag) map
* 'BindingError' - Errors from binding tree operations
* 'EliminatedVars', 'WeakenedVars' - Variable tracking sets
* 'PolySyms' - Polymorphic type constructor symbols

= Paper References

* Rémy & Yakobowski, "Graphic Type Constraints" - Binding tree structure
* `papers/these-finale-english.txt` - Binding edge semantics
* `papers/xmlf.txt` §3.1 - Flexible/rigid distinction
-}
module MLF.Constraint.Types.Graph.Binding (
    -- * Binding flag
    BindFlag (..),
    -- * Binding tree
    BindParents,
    -- * Binding errors
    BindingError (..),
    -- * Variable tracking
    EliminatedVars,
    WeakenedVars,
    -- * Polymorphic symbols
    PolySyms,
) where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Set (Set)

import MLF.Constraint.Types.Graph.NodeEdge (BaseTy, GenNodeId, NodeId, NodeRef)

-- | Flag indicating whether a binding edge is flexible or rigid.
--
-- Paper mapping (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1):
-- flexible binders can be raised/weakened
-- during instantiation; rigid binders are locked and cannot be modified.
data BindFlag = BindFlex | BindRigid
    deriving (Eq, Ord, Show)

-- | Binding-edge map: child NodeRef -> (parent NodeRef, flag).
--
-- This represents the paper's binding tree explicitly. Every non-root node
-- has exactly one binding parent. Roots are nodes that do not appear as keys.
type BindParents = IntMap (NodeRef, BindFlag)

-- | Errors that can occur when validating or manipulating the binding tree.
data BindingError
    = MissingBindParent NodeRef
        -- ^ A node that should have a binding parent does not have one.
    | BindingCycleDetected [NodeRef]
        -- ^ A cycle was detected in the binding-parent chain.
    | NoCommonAncestor NodeRef NodeRef
        -- ^ Two nodes have no common ancestor in the binding tree.
    | ParentNotUpper NodeRef NodeRef
        -- ^ The parent is not "upper" than the child in the term-DAG.
        -- First NodeId is the child, second is the parent.
    | OperationOnLockedNode NodeRef
        -- ^ Attempted to raise/weaken a node that is locked (rigidly bound path).
    | RaiseNotPossible NodeRef
        -- ^ Raise step not possible (e.g., parent is already a root).
    | GenFallbackRequired
        { fallbackBinder :: NodeId
        , fallbackGen :: GenNodeId
        , fallbackBinders :: [NodeId]
        }
        -- ^ Type-node binder enumeration would require a gen-ancestor fallback.
    | GenSchemeFreeVars
        { schemeRoot :: NodeId
        , schemeGen :: GenNodeId
        , freeNodes :: [NodeId]
        }
        -- ^ A scheme root reaches named nodes not bound under its gen node.
    | InvalidBindingTree String
        -- ^ Generic binding tree invariant violation with description.
    deriving (Eq, Show)

-- | Persistent marker for variables eliminated during ω execution / presolution.
type EliminatedVars = IntSet

-- | Variables whose binding edge was weakened by ω (OpWeaken).
type WeakenedVars = IntSet

-- | Polymorphic type constructor symbols (paper Poly set).
type PolySyms = Set BaseTy
