{- |
Module      : MLF.Constraint.Normalize.Internal
Description : Shared normalization state and helper operations
Copyright   : (c) 2024
License     : BSD-3-Clause
-}
module MLF.Constraint.Normalize.Internal (
    NormalizeState (..),
    NormalizeM,
    freshVar,
    freshNodeId,
    freshSynthExpVarNorm,
    insertNode,
    setBindParentNorm,
    setBindParentRefNorm,
    findRoot,
    unionNodes
) where

{- Note [Normalization state and shared helpers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module provides the shared mutable state ('NormalizeState p', 'NormalizeM p')
and primitive operations used by both the grafting ('Normalize.Graft') and
merging ('Normalize.Merge') normalization submodules.

Key state components:
  * 'nsConstraint'        — the constraint being normalized (modified in place)
  * 'nsUnionFind'         — union-find forest for merging (applied at end)
  * 'nsNextNodeId'        — fresh node-ID counter for grafted copies
  * 'nsSynthExpVarSupply'  — fresh expansion-variable allocator

The helpers ('freshVar', 'insertNode', 'findRoot', 'unionNodes', etc.) are
intentionally low-level primitives; higher-level normalization logic lives in
'Normalize.Graft' and 'Normalize.Merge'.
-}

import Control.Monad (when)
import Control.Monad.State.Strict (State, gets, modify')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Types.Graph as Graph
import MLF.Constraint.Types.Graph
    ( BindFlag
    , Constraint (..)
    , ExpVarId
    , NodeId (..)
    , NodeRef
    , TyNode (..)
    , typeRef
    )
import MLF.Constraint.Types.SynthesizedExpVar
    ( SynthExpVarSupply
    , takeSynthExpVar
    )
import qualified MLF.Util.UnionFind as UnionFind

-- | State maintained during normalization.
data NormalizeState p = NormalizeState { nsNextNodeId :: !Int
      -- ^ Counter for allocating fresh nodes during grafting.
    , nsSynthExpVarSupply :: !SynthExpVarSupply
      -- ^ Opaque allocator for synthesized-wrapper expansion variables in
      -- a reserved negative-ID space.
    , nsUnionFind :: !(IntMap NodeId)
      -- ^ Union-find structure: maps node IDs to their canonical representative.
    , nsConstraint :: !(Constraint p)
      -- ^ The constraint being normalized.
    }
    deriving (Eq, Show)

type NormalizeM p a = State (NormalizeState p) a

-- | Allocate a fresh variable node.
freshVar :: NormalizeM p NodeId
freshVar = do
    nid <- freshNodeId
    let node = TyVar { tnId = nid, tnBound = Nothing }
    insertNode node
    -- Note: binding parent is set by the caller when needed.
    pure nid

-- | Set the binding parent for a node in the constraint.
setBindParentNorm :: NodeId -> NodeId -> BindFlag -> NormalizeM p ()
setBindParentNorm child parent flag =
    when (child /= parent) $
        modify' $ \s ->
            let c = nsConstraint s
                c' = Binding.setBindParent (typeRef child) (typeRef parent, flag) c
            in s { nsConstraint = c' }

setBindParentRefNorm :: NodeRef -> NodeRef -> BindFlag -> NormalizeM p ()
setBindParentRefNorm child parent flag =
    when (child /= parent) $
        modify' $ \s ->
            let c = nsConstraint s
                c' = Binding.setBindParent child (parent, flag) c
            in s { nsConstraint = c' }

-- | Get a fresh NodeId.
freshNodeId :: NormalizeM p NodeId
freshNodeId = do
    n <- gets nsNextNodeId
    modify' $ \s -> s { nsNextNodeId = n + 1 }
    pure (NodeId n)

-- | Insert a node into the constraint.
insertNode :: TyNode -> NormalizeM p ()
insertNode node = modify' $ \s ->
    let c = nsConstraint s
        nodes' = Graph.insertNode (tnId node) node (cNodes c)
    in s { nsConstraint = c { cNodes = nodes' } }

-- Union-find link; caller is responsible for any scope maintenance (e.g.
-- binding-edge harmonization / paper Raise(n) for Var/Var unions).
unionNodes :: NodeId -> NodeId -> NormalizeM p ()
unionNodes from to = modify' $ \s ->
    s { nsUnionFind = IntMap.insert (getNodeId from) to (nsUnionFind s) }

-- | Find the root/canonical representative of a node.
findRoot :: IntMap NodeId -> NodeId -> NodeId
findRoot = UnionFind.frWith

-- | Allocate a fresh expansion variable for synthesized wrappers.
freshSynthExpVarNorm :: NormalizeM p ExpVarId
freshSynthExpVarNorm = do
    supply <- gets nsSynthExpVarSupply
    let (expVar, supply') = takeSynthExpVar supply
    modify' $ \st -> st { nsSynthExpVarSupply = supply' }
    pure expVar
