{- |
Module      : MLF.Constraint.Presolution.Unify
Description : UF unification with binding-edge Raise trace

This module contains the presolution unification helpers that:
  * perform an occurs-check (reachability) on the term DAG
  * merge UF classes while harmonizing binding parents (paper’s `Raise(n)`)
  * return the exact raised-node trace used to record `OpRaise`
  * keep `cBindParents` canonical under UF
-}
module MLF.Constraint.Presolution.Unify (
    unifyAcyclic,
    unifyAcyclicRawWithRaiseTrace
) where

import Control.Monad.State (get, modify, put)
import Control.Monad.Except (throwError)
import Control.Monad (when)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Adjustment as BindingAdjustment
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (PresolutionError(..), PresolutionM, PresolutionState(..))
import MLF.Constraint.Presolution.Ops (findRoot)
import qualified MLF.Util.UnionFind as UnionFind

-- | Lightweight reachability to prevent emitting a unification that would
-- immediately create a self-reference (occurs-check for presolution).
occursIn :: NodeId -> NodeId -> PresolutionM Bool
occursIn needle start = do
    st <- get
    let uf = psUnionFind st
        nodes = cNodes (psConstraint st)
        canonical = UnionFind.frWith uf
        lookupNode = lookupNodeIn nodes
    case Traversal.occursInUnder canonical lookupNode needle start of
        Left (Traversal.MissingNode nid) -> throwError (NodeLookupFailed nid)
        Right ok -> pure ok

-- | Union-Find merge with occurs-check, returning the Raise trace induced by
-- binding-edge harmonization.
--
-- Paper anchor (`papers/xmlf.txt`): `Raise(n)` is a binding-edge raising
-- operation (a real χe graph transformation).
--
-- Returns the exact raised-node trace (with multiplicity) induced by binding-edge
-- harmonization. Presolution records `OpRaise` based on this trace (filtered to
-- interior nodes and not under rigid binders).
unifyAcyclicRawWithRaiseTrace :: NodeId -> NodeId -> PresolutionM [NodeId]
unifyAcyclicRawWithRaiseTrace n1 n2 = do
    root1 <- findRoot n1
    root2 <- findRoot n2
    if root1 == root2
        then pure []
        else unifyAcyclicRootsWithRaiseTrace root1 root2

unifyAcyclicRootsWithRaiseTrace :: NodeId -> NodeId -> PresolutionM [NodeId]
unifyAcyclicRootsWithRaiseTrace root1 root2 = do
    occurs12 <- occursIn root1 root2
    when occurs12 $ throwError $ OccursCheckPresolution root1 root2

    occurs21 <- occursIn root2 root1
    when occurs21 $ throwError $ OccursCheckPresolution root2 root1

    st0 <- get
    let c0 = psConstraint st0

    (c1, trace0) <-
        case BindingAdjustment.harmonizeBindParentsWithTrace (typeRef root1) (typeRef root2) c0 of
            Left err -> throwError (BindingTreeError err)
            Right result -> pure result

    put st0 { psConstraint = c1 }
    let nodes = cNodes c1
        aElim = VarStore.isEliminatedVar c1 root1
        bElim = VarStore.isEliminatedVar c1 root2
        isTyVar nid =
            case IntMap.lookup (getNodeId nid) nodes of
                Just TyVar{} -> True
                _ -> False
        hasBound nid =
            case IntMap.lookup (getNodeId nid) nodes of
                Just TyVar{ tnBound = Just _ } -> True
                _ -> False
        (fromRoot0, toRoot0) =
            case (aElim, bElim) of
                (False, True) -> (root2, root1)
                _ -> (root1, root2)
        (fromRoot, toRoot)
            | not aElim
            , not bElim
            , isTyVar root1
            , isTyVar root2 =
                case (hasBound root1, hasBound root2) of
                    (True, False) -> (root2, root1)
                    (False, True) -> (root1, root2)
                    _ -> (fromRoot0, toRoot0)
            | otherwise = (fromRoot0, toRoot0)
    modify $ \st ->
        st { psUnionFind = IntMap.insert (getNodeId fromRoot) toRoot (psUnionFind st) }

    pure trace0

unifyAcyclicRawWithRaiseCounts :: NodeId -> NodeId -> PresolutionM (Int, Int)
unifyAcyclicRawWithRaiseCounts n1 n2 = do
    root1 <- findRoot n1
    root2 <- findRoot n2
    if root1 == root2
        then pure (0, 0)
        else do
            trace <- unifyAcyclicRootsWithRaiseTrace root1 root2
            let k1 = length (filter (== root1) trace)
                k2 = length (filter (== root2) trace)
            pure (k1, k2)

-- | Union-Find merge with occurs-check.
unifyAcyclicRaw :: NodeId -> NodeId -> PresolutionM ()
unifyAcyclicRaw n1 n2 = do
    _ <- unifyAcyclicRawWithRaiseCounts n1 n2
    pure ()

unifyAcyclic :: NodeId -> NodeId -> PresolutionM ()
unifyAcyclic = unifyAcyclicRaw
