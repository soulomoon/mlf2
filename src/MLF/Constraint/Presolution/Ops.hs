{- |
Module      : MLF.Constraint.Presolution.Ops
Description : Low-level stateful operations for presolution

This module is a small “foundation” layer for presolution submodules. It
contains stateful operations on `PresolutionState` / `Constraint p` shared by
the public presolution entrypoint and its owner modules.

Keeping these operations separate makes it easier to keep those modules
cohesive without introducing import cycles.
-}
module MLF.Constraint.Presolution.Ops (
    createFreshNodeId,
    registerNode,
    setBindParentM,
    getNode,
    getCanonicalNode,
    findRoot,
    lookupVarBound,
    createFreshVar,
    setVarBound,
    setCanonicalVarBound,
    dropVarBind
) where

import Control.Monad.State (get, gets, modify', put)
import Control.Monad.Except (throwError)
import qualified Data.IntSet as IntSet

import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.UnionFind as UnionFind
import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution.Base
    ( PresolutionM
    , PresolutionError(..)
    , PresolutionState(..)
    , compressUnionFindState
    , modifyConstraintDirtyTypesState
    , setBindParentState
    )

-- | Allocate a fresh NodeId from the presolution state counter.
createFreshNodeId :: PresolutionM p NodeId
createFreshNodeId = do
    st <- get
    let nid = NodeId (psNextNodeId st)
    put $ st { psNextNodeId = psNextNodeId st + 1 }
    pure nid

-- | Register a node in the constraint’s node map.
registerNode :: NodeId -> TyNode -> PresolutionM p ()
registerNode nid node =
    modify' $ \st ->
        let c0 = psConstraint st
            nodes' = insertNode nid node (cNodes c0)
            dirty =
                IntSet.fromList $
                    getNodeId nid : map getNodeId (structuralChildrenWithBounds node)
        in modifyConstraintDirtyTypesState dirty (\c -> c { cNodes = nodes' }) st

-- | Set a binding parent for a node in the constraint.
--
-- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1):
-- this mutates the explicit binding tree relation `cBindParents`.
setBindParentM :: NodeRef -> (NodeRef, BindFlag) -> PresolutionM p ()
setBindParentM child parentInfo =
    modify' (setBindParentState child parentInfo)

-- | Lookup a node in the term-DAG or fail.
{-# INLINE getNode #-}
getNode :: NodeId -> PresolutionM p TyNode
getNode nid = do
    nodes <- gets (cNodes . psConstraint)
    case lookupNode nid nodes of
        Just n -> pure n
        Nothing -> throwError $ NodeLookupFailed nid

-- | Find the canonical representative of a node (with path compression).
{-# INLINE findRoot #-}
findRoot :: NodeId -> PresolutionM p NodeId
findRoot nid = do
    uf <- gets psUnionFind
    let (root, uf') = UnionFind.findRootWithCompression uf nid
    modify' (compressUnionFindState uf')
    pure root

-- | Lookup a node at its current canonical representative.
{-# INLINE getCanonicalNode #-}
getCanonicalNode :: NodeId -> PresolutionM p TyNode
getCanonicalNode nid = do
    rootId <- findRoot nid
    nodes <- gets (cNodes . psConstraint)
    case lookupNode rootId nodes of
        Just node -> pure node
        Nothing -> throwError $ NodeLookupFailed rootId

-- | Lookup the instance bound of a variable (⊥ represented as 'Nothing').
--
-- Uses canonical representatives so aliasing through UF stays coherent.
{-# INLINE lookupVarBound #-}
lookupVarBound :: NodeId -> PresolutionM p (Maybe NodeId)
lookupVarBound bv = do
    root <- findRoot bv
    c <- gets psConstraint
    pure (VarStore.lookupVarBound c root)

-- | Helper to create a fresh variable node.
createFreshVar :: PresolutionM p NodeId
createFreshVar = do
    nid <- createFreshNodeId
    let node = TyVar { tnId = nid, tnBound = Nothing }
    registerNode nid node
    pure nid

-- | Update the instance bound of a type variable.
--
-- Missing keys are treated as ⊥; updates use UF roots so bounds follow
-- canonical representatives.
setVarBound :: NodeId -> Maybe NodeId -> PresolutionM p ()
setVarBound vid mb = do
    node <- getNode vid
    case node of
        TyVar{} -> do
            root <- findRoot vid
            mbRoot <- mapM findRoot mb
            modify' $ \st ->
                let c0 = psConstraint st
                    c1 = VarStore.setVarBound root mbRoot c0
                    dirty =
                        IntSet.fromList $
                            getNodeId root : maybe [] ((: []) . getNodeId) mbRoot
                in modifyConstraintDirtyTypesState dirty (const c1) st
        _ -> pure ()

-- | Update the instance bound when caller already holds canonical
-- representatives for the variable and optional bound.
setCanonicalVarBound :: NodeId -> Maybe NodeId -> PresolutionM p ()
setCanonicalVarBound root mbRoot = do
    nodes <- gets (cNodes . psConstraint)
    case lookupNode root nodes of
        Just TyVar{} ->
            modify' $ \st ->
                let c0 = psConstraint st
                    c1 = VarStore.setVarBound root mbRoot c0
                    dirty =
                        IntSet.fromList $
                            getNodeId root : maybe [] ((: []) . getNodeId) mbRoot
                in modifyConstraintDirtyTypesState dirty (const c1) st
        _ -> pure ()

-- | Mark a type variable as eliminated so elaboration will not re-quantify it.
dropVarBind :: NodeId -> PresolutionM p ()
dropVarBind vid = do
    node <- getNode vid
    case node of
        TyVar{} ->
            modify' $ \st ->
                let c0 = psConstraint st
                    c1 = VarStore.markEliminatedVar vid c0
                    dirty = IntSet.singleton (getNodeId vid)
                in modifyConstraintDirtyTypesState dirty (const c1) st
        _ -> pure ()
