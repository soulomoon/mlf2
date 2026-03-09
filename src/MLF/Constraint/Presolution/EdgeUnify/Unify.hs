{- |
Module      : MLF.Constraint.Presolution.EdgeUnify.Unify
Description : Edge-local unification logic for presolution instantiation edges
-}
module MLF.Constraint.Presolution.EdgeUnify.Unify (
    unifyAcyclicEdge,
    unifyStructureEdge
) where

import Control.Monad (foldM, forM_, when)
import Control.Monad.Reader (ask)
import Control.Monad.State (gets)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base (
    memberInterior,
    PresolutionError(..)
    )
import qualified MLF.Constraint.Presolution.Ops as Ops
import MLF.Constraint.Presolution.EdgeUnify.State (
    EdgeUnifyM,
    EdgeUnifyState(..),
    MonadEdgeUnify(..),
    deleteInteriorKey,
    insertInteriorKey,
    isEliminated,
    nullInteriorNodes,
    preferBinderMetaRoot,
    recordEliminate,
    recordRaisesFromTrace,
    unifyWithLockedFallback
    )
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.Unify.Decompose as UnifyDecompose
import MLF.Constraint.Types
import qualified MLF.Util.Order as Order
import MLF.Util.Trace (traceBindingM)

recordOp :: InstanceOp -> EdgeUnifyM ()
recordOp = recordInstanceOp

compareBinderIdsByPrec :: Int -> Int -> EdgeUnifyM Ordering
compareBinderIdsByPrec bid1 bid2 = do
    keys <- gets eusOrderKeys
    binderMeta <- gets eusBinderMeta
    let keyFor bid = do
            meta <- IntMap.lookup bid binderMeta
            IntMap.lookup (getNodeId meta) keys
        k1 = keyFor bid1
        k2 = keyFor bid2
    pure $ case (k1, k2) of
        (Just a, Just b) ->
            case Order.compareOrderKey a b of
                EQ -> compare bid1 bid2
                other -> other
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT
        (Nothing, Nothing) -> compare bid1 bid2

pickRepBinderId :: IntSet.IntSet -> EdgeUnifyM Int
pickRepBinderId bs =
    case IntSet.toList bs of
        [] -> throwPresolutionErrorM (InternalError "pickRepBinderId: empty binder set")
        (x:xs) -> foldM pick x xs
  where
    pick best cand = do
        ord <- compareBinderIdsByPrec cand best
        pure $ case ord of
            LT -> cand
            _ -> best

recordMergesIntoRep :: IntSet.IntSet -> EdgeUnifyM ()
recordMergesIntoRep bs
    | IntSet.size bs <= 1 = pure ()
    | otherwise = do
        eliminated <- gets eusEliminatedBinders
        let live = IntSet.filter (\bid -> not (IntSet.member bid eliminated)) bs
        repId <- pickRepBinderId (if IntSet.null live then bs else live)
        let rep = NodeId repId
            others = filter (/= repId) (IntSet.toList bs)
        othersSorted <- sortByM (\a b -> compareBinderIdsByPrec b a) others
        forM_ othersSorted $ \bid -> do
            let b = NodeId bid
            already <- isEliminated b
            when (not already) $ do
                recordOp (OpMerge b rep)
                setVarBoundM b (Just rep)
                recordEliminate b
  where
    sortByM :: (a -> a -> EdgeUnifyM Ordering) -> [a] -> EdgeUnifyM [a]
    sortByM _ [] = pure []
    sortByM cmp xs = do
        let insertOne x [] = pure [x]
            insertOne x (y:ys) = do
                o <- cmp x y
                if o == LT then pure (x : y : ys) else (y :) <$> insertOne x ys
        foldM (\acc x -> insertOne x acc) [] xs

{- Note [Edge-local Raise/Merge emission]
Edge-local unification mirrors the paper's chi_e operations. Binding-edge
harmonization produces a raise trace, and we emit `OpRaise` for the interior
nodes that were actually raised. When binder metas become aliased we emit
`OpMerge` (and eliminate the merged binder). If a binder class merges with an
exterior TyVar class, we emit RaiseMerge as an explicit Raise followed by Merge
so that `normalizeInstanceOpsFull` can coalesce it later. See
`papers/these-finale-english.txt` and `papers/xmlf.txt` section 3.4 / Fig. 10.

Invariant: RaiseMerge emission is forbidden for self-class merges. If operated
`n` and target `m` are already in the same UF class, the operation is a no-op
and must not record/source-write `OpMerge n n` side effects.
-}

unifyAcyclicEdge :: NodeId -> NodeId -> EdgeUnifyM ()
unifyAcyclicEdge n1 n2 = do
    root1 <- findRootM n1
    root2 <- findRootM n2
    when (root1 /= root2) $ do
        st0 <- getEdgeUnifyState
        let r1 = getNodeId root1
            r2 = getNodeId root2
            inInt1 = memberInterior root1 (eusInteriorRoots st0)
            inInt2 = memberInterior root2 (eusInteriorRoots st0)
            bs1 = IntMap.findWithDefault IntSet.empty r1 (eusBindersByRoot st0)
            bs2 = IntMap.findWithDefault IntSet.empty r2 (eusBindersByRoot st0)
            bs = IntSet.union bs1 bs2

        prefer <- preferBinderMetaRoot root1 root2
        raiseTrace <- unifyWithLockedFallback prefer root1 root2
        rep <- findRootM root2
        let repId = getNodeId rep
            int1 = IntMap.findWithDefault mempty r1 (eusInteriorByRoot st0)
            int2 = IntMap.findWithDefault mempty r2 (eusInteriorByRoot st0)
            intAll = int1 <> int2

        recordRaisesFromTrace intAll raiseTrace

        modifyEdgeUnifyState $ \st ->
            let roots' =
                    if inInt1 || inInt2
                        then
                            insertInteriorKey
                                repId
                                (deleteInteriorKey r2 (deleteInteriorKey r1 (eusInteriorRoots st)))
                        else eusInteriorRoots st
                binders' =
                    let m0 = eusBindersByRoot st
                        m1 =
                            if IntSet.null bs
                                then IntMap.delete r2 (IntMap.delete r1 m0)
                                else IntMap.insert repId bs (IntMap.delete r2 (IntMap.delete r1 m0))
                    in m1
                interior' =
                    let m0 = eusInteriorByRoot st
                        m1 =
                            if nullInteriorNodes intAll
                                then IntMap.delete r2 (IntMap.delete r1 m0)
                                else IntMap.insert repId intAll (IntMap.delete r2 (IntMap.delete r1 m0))
                    in m1
            in st { eusInteriorRoots = roots', eusBindersByRoot = binders', eusInteriorByRoot = interior' }

        recordMergesIntoRep bs

        when (IntSet.size bs >= 1) $ do
            eliminated <- gets eusEliminatedBinders
            let live = IntSet.filter (\bid -> not (IntSet.member bid eliminated)) bs
            when (not (IntSet.null live)) $ do
                repBinderId <- pickRepBinderId live
                let repBinder = NodeId repBinderId
                    recordRaiseMergeUnlessSelf extCandidate = do
                        repRoot <- findRootM repBinder
                        extRoot <- findRootM extCandidate
                        if repRoot == extRoot
                            then
                                debugEdgeUnify
                                    ( "raise-merge skipped: self-class merge binder="
                                        ++ show repBinder
                                        ++ " ext="
                                        ++ show extCandidate
                                        ++ " root="
                                        ++ show repRoot
                                    )
                            else do
                                recordOp (OpRaise repBinder)
                                recordOp (OpMerge repBinder extCandidate)
                                setVarBoundM repBinder (Just extCandidate)
                                recordEliminate repBinder
                case (IntSet.null bs1, IntSet.null bs2) of
                    (False, True) | inInt1 && not inInt2 -> do
                        should <- shouldRecordRaiseMerge repBinder root2
                        when should $
                            recordRaiseMergeUnlessSelf root2
                    (True, False) | inInt2 && not inInt1 -> do
                        should <- shouldRecordRaiseMerge repBinder root1
                        when should $
                            recordRaiseMergeUnlessSelf root1
                    _ -> pure ()

-- | Decide whether to record a RaiseMerge(binder, ext) operation.
--
-- The decision depends on five purely structural, live graph facts:
--
--   1. **Node kind**: @ext@ must be a @TyVar@ (non-variable nodes cannot be
--      RaiseMerge targets).
--   2. **Live bound root**: @binder@ must have a canonical bound in the current
--      constraint graph (unbounded binders use InstApp/graft instead).
--   3. **Same-root exclusion**: the canonical bound root and @ext@ root must
--      differ (same UF class means no raise is needed).
--   4. **Edge-root ancestry / interior membership**: @ext@ is bound above the
--      edge root in the binding tree, or @ext@ is outside the interior @I(r)@.
--   5. **Elimination state**: @binder@ must not already be eliminated by a
--      prior Merge/RaiseMerge in this edge.
--
-- All queries use the current canonical graph state (UF roots, binding tree,
-- interior set) — no precomputed snapshots.
shouldRecordRaiseMerge :: NodeId -> NodeId -> EdgeUnifyM Bool
shouldRecordRaiseMerge binder ext = do
    already <- isEliminated binder
    if already
        then pure False
        else do
            edgeRoot <- gets eusEdgeRoot
            extNode <- liftPresolution $ Ops.getNode ext
            case extNode of
                TyVar{} -> do
                    mbBnd <- lookupVarBoundM binder
                    case mbBnd of
                        Nothing -> do
                            debugEdgeUnify
                                ( "shouldRecordRaiseMerge: binder="
                                    ++ show binder
                                    ++ " ext="
                                    ++ show ext
                                    ++ " bound=None"
                                )
                            pure False
                        Just bndOrig -> do
                            bndRoot <- findRootM bndOrig
                            extRoot <- findRootM ext
                            if bndRoot == extRoot
                                then do
                                    debugEdgeUnify
                                        ( "shouldRecordRaiseMerge: binder="
                                            ++ show binder
                                            ++ " ext="
                                            ++ show ext
                                            ++ " boundRoot="
                                            ++ show bndRoot
                                            ++ " extRoot="
                                            ++ show extRoot
                                            ++ " sameRoot=True"
                                        )
                                    pure False
                                else do
                                    above <- isBoundAboveInBindingTreeM edgeRoot extRoot
                                    interiorRoots <- gets eusInteriorRoots
                                    let inInterior = memberInterior extRoot interiorRoots
                                    debugEdgeUnify
                                        ( "shouldRecordRaiseMerge: binder="
                                            ++ show binder
                                            ++ " ext="
                                            ++ show ext
                                            ++ " boundRoot="
                                            ++ show bndRoot
                                            ++ " extRoot="
                                            ++ show extRoot
                                            ++ " edgeRoot="
                                            ++ show edgeRoot
                                            ++ " above="
                                            ++ show above
                                            ++ " inInterior="
                                            ++ show inInterior
                                        )
                                    pure (above || not inInterior)
                _ -> pure False

debugEdgeUnify :: String -> EdgeUnifyM ()
debugEdgeUnify msg = do
    cfg <- ask
    traceBindingM cfg msg

unifyStructureEdge :: NodeId -> NodeId -> EdgeUnifyM ()
unifyStructureEdge n1 n2 = do
    root1 <- findRootM n1
    root2 <- findRootM n2
    debugEdgeUnify
        ( "unifyStructureEdge: n1="
            ++ show n1
            ++ " root1="
            ++ show root1
            ++ " n2="
            ++ show n2
            ++ " root2="
            ++ show root2
        )
    if root1 == root2 then pure ()
    else do
        node1 <- liftPresolution $ Ops.getCanonicalNode n1
        node2 <- liftPresolution $ Ops.getCanonicalNode n2
        isMeta1 <- isBinderMetaRoot root1
        isMeta2 <- isBinderMetaRoot root2
        let isVar1 = case node1 of
                TyVar{} -> True
                _ -> False
            isVar2 = case node2 of
                TyVar{} -> True
                _ -> False
            unifyVarBounds nA nB =
                case (nA, nB) of
                    (TyVar { tnBound = mb1 }, TyVar { tnBound = mb2 }) ->
                        case (mb1, mb2) of
                            (Just b1, Just b2) ->
                                when (b1 /= b2) (unifyStructureEdge b1 b2)
                            _ -> pure ()
                    _ -> pure ()
            trySetBound target bnd = do
                (c0, canonical) <- liftPresolution getConstraintAndCanonical
                let targetC = canonical target
                    bndC = canonical bnd
                occurs <-
                    case Traversal.occursInUnder canonical (NodeAccess.lookupNode c0) targetC bndC of
                        Left _ -> pure True
                        Right ok -> pure ok
                if occurs
                    then throwPresolutionErrorM (OccursCheckPresolution targetC bndC)
                    else
                        if bndC /= targetC
                            then setVarBoundM targetC (Just bndC) >> pure True
                            else pure False
            unifyStructureChildren nodeA nodeB =
                case (nodeA, nodeB) of
                    (TyVar{}, _) -> pure ()
                    (_, TyVar{}) -> pure ()
                    (TyExp{}, _) -> pure ()
                    (_, TyExp{}) -> pure ()
                    _ ->
                        case UnifyDecompose.decomposeUnifyChildren nodeA nodeB of
                            Right edges ->
                                mapM_ (\edge -> unifyStructureEdge (uniLeft edge) (uniRight edge)) edges
                            Left _ -> pure ()
        if isMeta1 || isMeta2
            then
                if isVar1 && isVar2
                    then do
                        unifyAcyclicEdge n1 n2
                        unifyVarBounds node1 node2
                    else do
                        let (metaRoot, otherNode) =
                                if isMeta1 then (root1, node2) else (root2, node1)
                        mbMetaBound <- lookupVarBoundM metaRoot
                        case mbMetaBound of
                            Just bMeta -> do
                                bMetaNode <- liftPresolution $ Ops.getCanonicalNode bMeta
                                case bMetaNode of
                                    TyVar{} -> do
                                        _ <- trySetBound bMeta (tnId otherNode)
                                        pure ()
                                    _ -> unifyStructureEdge bMeta (tnId otherNode)
                            Nothing -> do
                                _ <- trySetBound metaRoot (tnId otherNode)
                                pure ()
            else do
                unifyAcyclicEdge n1 n2
                case (node1, node2) of
                    (TyVar { tnBound = mb1 }, TyVar { tnBound = mb2 }) ->
                        case (mb1, mb2) of
                            (Just b1, Just b2) ->
                                when (b1 /= b2) (unifyStructureEdge b1 b2)
                            (Just b1, Nothing) -> do
                                _ <- trySetBound (tnId node2) b1
                                pure ()
                            (Nothing, Just b2) -> do
                                _ <- trySetBound (tnId node1) b2
                                pure ()
                            _ -> pure ()
                    (TyVar { tnBound = Just b1 }, _) ->
                        when (b1 /= tnId node2) (unifyStructureEdge b1 (tnId node2))
                    (_, TyVar { tnBound = Just b2 }) ->
                        when (b2 /= tnId node1) (unifyStructureEdge (tnId node1) b2)
                    _ ->
                        unifyStructureChildren node1 node2

isBinderMetaRoot :: NodeId -> EdgeUnifyM Bool
isBinderMetaRoot root = do
    metaRoots <- gets eusBinderMeta >>= mapM findRootM . IntMap.elems
    let metaSet = IntSet.fromList (map getNodeId metaRoots)
    pure (IntSet.member (getNodeId root) metaSet)
