{- |
Module      : MLF.Constraint.Presolution.Ops
Description : Low-level stateful operations for presolution

This module is a small “foundation” layer for presolution submodules. It
contains stateful operations on `PresolutionState` / `Constraint` that were
previously embedded in `MLF.Constraint.Presolution.Core`.

Keeping these operations separate makes it easier to split `Core` into
cohesive submodules without introducing import cycles.
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
    dropVarBind
) where

import Control.Monad.State (get, gets, modify', put)
import Control.Monad.Except (throwError)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.UnionFind as UnionFind
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (PresolutionM, PresolutionError(..), PresolutionState(..))

-- | Allocate a fresh NodeId from the presolution state counter.
createFreshNodeId :: PresolutionM NodeId
createFreshNodeId = do
    st <- get
    let nid = NodeId (psNextNodeId st)
    put $ st { psNextNodeId = psNextNodeId st + 1 }
    pure nid

-- | Register a node in the constraint’s node map.
registerNode :: NodeId -> TyNode -> PresolutionM ()
registerNode nid node =
    modify' $ \st ->
        let c0 = psConstraint st
            nodes' = IntMap.insert (getNodeId nid) node (cNodes c0)
        in st { psConstraint = c0 { cNodes = nodes' } }

-- | Set a binding parent for a node in the constraint.
--
-- Paper alignment (`papers/xmlf.txt` §3.1): this mutates the explicit binding
-- tree relation `cBindParents`.
setBindParentM :: NodeRef -> (NodeRef, BindFlag) -> PresolutionM ()
setBindParentM child parentInfo =
    modify' $ \st ->
        let c0 = psConstraint st
            c1 = Binding.setBindParent child parentInfo c0
        in st { psConstraint = c1 }

-- | Lookup a node in the term-DAG or fail.
getNode :: NodeId -> PresolutionM TyNode
getNode nid = do
    nodes <- gets (cNodes . psConstraint)
    case IntMap.lookup (getNodeId nid) nodes of
        Just n -> pure n
        Nothing -> throwError $ NodeLookupFailed nid

-- | Find the canonical representative of a node (with path compression).
findRoot :: NodeId -> PresolutionM NodeId
findRoot nid = do
    uf <- gets psUnionFind
    let (root, uf') = UnionFind.findRootWithCompression uf nid
    modify' $ \st -> st { psUnionFind = uf' }
    pure root

-- | Lookup a node at its current canonical representative.
getCanonicalNode :: NodeId -> PresolutionM TyNode
getCanonicalNode nid = do
    rootId <- findRoot nid
    nodes <- gets (cNodes . psConstraint)
    case IntMap.lookup (getNodeId rootId) nodes of
        Just node -> pure node
        Nothing -> throwError $ NodeLookupFailed rootId

-- | Lookup the instance bound of a variable (⊥ represented as 'Nothing').
--
-- Uses canonical representatives so aliasing through UF stays coherent.
lookupVarBound :: NodeId -> PresolutionM (Maybe NodeId)
lookupVarBound bv = do
    root <- findRoot bv
    c <- gets psConstraint
    pure (VarStore.lookupVarBound c root)

-- | Helper to create a fresh variable node.
createFreshVar :: PresolutionM NodeId
createFreshVar = do
    nid <- createFreshNodeId
    let node = TyVar { tnId = nid, tnBound = Nothing }
    registerNode nid node
    pure nid

-- | Update the instance bound of a type variable.
--
-- Missing keys are treated as ⊥; updates use UF roots so bounds follow
-- canonical representatives.
setVarBound :: NodeId -> Maybe NodeId -> PresolutionM ()
setVarBound vid mb = do
    node <- getNode vid
    case node of
        TyVar{} -> do
            root <- findRoot vid
            mbRoot <- mapM findRoot mb
            modify' $ \st ->
                let c0 = psConstraint st
                    c1 = VarStore.setVarBound root mbRoot c0
                in st { psConstraint = c1 }
        _ -> pure ()

-- | Mark a type variable as eliminated so elaboration will not re-quantify it.
dropVarBind :: NodeId -> PresolutionM ()
dropVarBind vid = do
    node <- getNode vid
    case node of
        TyVar{} ->
            modify' $ \st ->
                let c0 = psConstraint st
                    c1 = VarStore.markEliminatedVar vid c0
                in st { psConstraint = c1 }
        _ -> pure ()
