{- |
Module      : MLF.Constraint.Presolution.EdgeUnify
Description : Edge-local unification and ω execution helpers

This module contains the edge-local unification machinery used during
presolution to execute and witness χe operations (paper `Raise`/`Merge`/`Weaken`)
while processing a single instantiation edge.

It is extracted from `MLF.Constraint.Presolution.Core` to keep the presolution
driver cohesive and to avoid further growth in the main phase module.
-}
module MLF.Constraint.Presolution.EdgeUnify (
    EdgeUnifyState(..),
    EdgeUnifyM,
    flushPendingWeakens,
    initEdgeUnifyState,
    mkOmegaExecEnv,
    runEdgeUnifyForTest,
    unifyAcyclicEdge,
    unifyStructureEdge
) where

import Control.Monad.State
import Control.Monad.Except (throwError)
import Control.Monad (foldM, forM, forM_, when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe, listToMaybe)

import qualified MLF.Binding.GraphOps as GraphOps
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Util.Order as Order
import qualified MLF.Util.UnionFind as UnionFind
import qualified MLF.Witness.OmegaExec as OmegaExec
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (
    CopyMap,
    InteriorSet,
    PresolutionError(..),
    PresolutionM,
    PresolutionState(..),
    requireValidBindingTree
    )
import MLF.Constraint.Presolution.Ops (
    dropVarBind,
    findRoot,
    getCanonicalNode,
    getNode,
    lookupVarBound,
    setVarBound
    )
import MLF.Constraint.Presolution.Unify (unifyAcyclicRawWithRaiseTracePrefer)
import qualified MLF.Constraint.Traversal as Traversal

data EdgeUnifyState = EdgeUnifyState
    { eusInteriorRoots :: IntSet.IntSet
    , eusBindersByRoot :: IntMap IntSet.IntSet -- ^ UF-root -> binder NodeIds whose args live in that class
    , eusInteriorByRoot :: IntMap IntSet.IntSet -- ^ UF-root -> interior NodeIds (all nodes in I(r))
    , eusEdgeRoot :: NodeId -- ^ Expansion root r (edge-local χe root)
    , eusEliminatedBinders :: IntSet.IntSet -- ^ binders eliminated by Merge/RaiseMerge ops we record
    , eusBinderMeta :: IntMap NodeId -- ^ source binder -> copied/meta node in χe
    , eusBinderBounds :: IntMap NodeId -- ^ source binder -> original bound (if any) before ω execution
    , eusOrderKeys :: IntMap Order.OrderKey -- ^ order keys for meta nodes (edge-local ≺)
    , eusOps :: [InstanceOp]
    }

type EdgeUnifyM = StateT EdgeUnifyState PresolutionM

-- | Testing helper: run a single edge-local unification and return the recorded
-- instance-operation witness slice.
--
-- This bypasses expansion copying and is intended for unit tests that want to
-- assert the precise `OpRaise` targets produced by binding-parent harmonization
-- (including the “no spray” behavior for interior nodes).
runEdgeUnifyForTest
    :: NodeId -- ^ edge root (for ≺ ordering keys)
    -> IntSet.IntSet -- ^ interior nodes (I(r))
    -> NodeId -- ^ left node to unify
    -> NodeId -- ^ right node to unify
    -> PresolutionM [InstanceOp]
runEdgeUnifyForTest edgeRoot interior n1 n2 = do
    requireValidBindingTree
    eu0 <- initEdgeUnifyState [] IntMap.empty interior edgeRoot
    (_a, eu1) <- runStateT (unifyAcyclicEdge n1 n2) eu0
    pure (eusOps eu1)

-- | Build an ω executor environment for χe base ops (Graft/Merge/Weaken).
--
-- This is used to execute the base operations induced directly by
-- `ExpInstantiate` as real χe transformations, but split into two phases so
-- that bounded binders can still trigger `RaiseMerge` during unification with
-- the edge target.
--
-- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4):
-- `Weaken` occurs after other
-- operations on nodes below it. Executing it eagerly can preempt the unification
-- that should be witnessed as `RaiseMerge`.
mkOmegaExecEnv :: CopyMap -> OmegaExec.OmegaExecEnv EdgeUnifyM
mkOmegaExecEnv copyMap =
    OmegaExec.OmegaExecEnv
        { OmegaExec.omegaMetaFor = metaFor
        , OmegaExec.omegaLookupMeta = \bv -> pure (IntMap.lookup (getNodeId bv) copyMap)
        , OmegaExec.omegaLookupVarBound = \meta -> lift $ lookupVarBound meta
        , OmegaExec.omegaSetVarBound = \meta mb -> lift $ setVarBound meta mb
        , OmegaExec.omegaDropVarBind = \meta -> lift $ dropVarBind meta
        , OmegaExec.omegaUnifyNoMerge = unifyAcyclicEdgeNoMerge
        , OmegaExec.omegaRecordEliminate = recordEliminate
        , OmegaExec.omegaIsEliminated = isEliminated
        , OmegaExec.omegaEliminatedBinders = do
            elims <- gets eusEliminatedBinders
            pure (map NodeId (IntSet.toList elims))
        , OmegaExec.omegaWeakenMeta = \meta -> do
            metaRoot <- lift $ findRoot meta
            lift $ queuePendingWeaken metaRoot
        }
  where
    metaFor :: NodeId -> EdgeUnifyM NodeId
    metaFor bv =
        case IntMap.lookup (getNodeId bv) copyMap of
            Just m -> pure m
            Nothing ->
                lift $ throwError (InternalError ("mkOmegaExecEnv: missing copy for binder " ++ show bv))

queuePendingWeaken :: NodeId -> PresolutionM ()
queuePendingWeaken nid =
    modify' $ \st ->
        st { psPendingWeakens = IntSet.insert (getNodeId nid) (psPendingWeakens st) }

flushPendingWeakens :: PresolutionM ()
flushPendingWeakens = do
    pending <- gets psPendingWeakens
    when (not (IntSet.null pending)) $
        forM_ (IntSet.toList pending) $ \nidInt -> do
            let nid0 = NodeId nidInt
            nid <- findRoot nid0
            c0 <- gets psConstraint
            -- Redundant weakens can arise when multiple edges share the same
            -- expansion variable (merged χe). Treat "already rigid" as a no-op.
            case Binding.lookupBindParent c0 (typeRef nid) of
                Nothing -> pure ()
                Just (_p, BindRigid) -> pure ()
                Just _ ->
                    case GraphOps.applyWeaken (typeRef nid) c0 of
                        Left err -> throwError (BindingTreeError err)
                        Right (c', _op) ->
                            modify' $ \st -> st { psConstraint = c' }
    modify' $ \st -> st { psPendingWeakens = IntSet.empty }

-- | Edge-local union like 'unifyAcyclicEdge', but without emitting merge-like
-- witness ops. This is used to *execute* base `Merge` operations (already
-- recorded in Ω) without accidentally introducing an opposing Phase-2 merge.
unifyAcyclicEdgeNoMerge :: NodeId -> NodeId -> EdgeUnifyM ()
unifyAcyclicEdgeNoMerge n1 n2 = do
    root1 <- lift $ findRoot n1
    root2 <- lift $ findRoot n2
    when (root1 /= root2) $ do
        st0 <- get
        let r1 = getNodeId root1
            r2 = getNodeId root2
            inInt1 = IntSet.member r1 (eusInteriorRoots st0)
            inInt2 = IntSet.member r2 (eusInteriorRoots st0)
            bs1 = IntMap.findWithDefault IntSet.empty r1 (eusBindersByRoot st0)
            bs2 = IntMap.findWithDefault IntSet.empty r2 (eusBindersByRoot st0)
            bs = IntSet.union bs1 bs2
        
        prefer <- preferBinderMetaRoot root1 root2
        trace <- lift $ unifyAcyclicRawWithRaiseTracePrefer prefer root1 root2
        rep <- lift $ findRoot root2
        let repId = getNodeId rep
            int1 = IntMap.findWithDefault IntSet.empty r1 (eusInteriorByRoot st0)
            int2 = IntMap.findWithDefault IntSet.empty r2 (eusInteriorByRoot st0)
            intAll = IntSet.union int1 int2
        recordRaisesFromTrace intAll trace

        modify $ \st ->
            let roots' =
                    if inInt1 || inInt2
                        then IntSet.insert repId (IntSet.delete r2 (IntSet.delete r1 (eusInteriorRoots st)))
                        else eusInteriorRoots st
                binders' =
                    let m0 = eusBindersByRoot st
                        m1 = if IntSet.null bs
                            then IntMap.delete r2 (IntMap.delete r1 m0)
                            else IntMap.insert repId bs (IntMap.delete r2 (IntMap.delete r1 m0))
                    in m1
                interior' =
                    let m0 = eusInteriorByRoot st
                        m1 = if IntSet.null intAll
                            then IntMap.delete r2 (IntMap.delete r1 m0)
                            else IntMap.insert repId intAll (IntMap.delete r2 (IntMap.delete r1 m0))
                    in m1
            in st { eusInteriorRoots = roots', eusBindersByRoot = binders', eusInteriorByRoot = interior' }

initEdgeUnifyState :: [(NodeId, NodeId)] -> IntMap NodeId -> InteriorSet -> NodeId -> PresolutionM EdgeUnifyState
initEdgeUnifyState binderArgs binderBounds interior edgeRoot = do
    roots <- forM (IntSet.toList interior) $ \i -> findRoot (NodeId i)
    let interiorRoots = IntSet.fromList (map getNodeId roots)
    bindersByRoot <- foldM
        (\m (bv, arg) -> do
            r <- findRoot arg
            let k = getNodeId r
                v = IntSet.singleton (getNodeId bv)
            pure (IntMap.insertWith IntSet.union k v m)
        )
        IntMap.empty
        binderArgs
    -- Build interior-by-root map: for each interior node, track which UF root it belongs to
    -- This allows us to record OpRaise for ALL interior nodes, not just binders
    interiorByRoot <- foldM
        (\m i -> do
            r <- findRoot (NodeId i)
            let k = getNodeId r
                v = IntSet.singleton i
            pure (IntMap.insertWith IntSet.union k v m)
        )
        IntMap.empty
        (IntSet.toList interior)
    constraint <- gets psConstraint
    let interiorRootRef =
            case Binding.lookupBindParent constraint (typeRef edgeRoot) of
                Just (parent, _) -> parent
                Nothing -> typeRef edgeRoot
        interiorRoot =
            case interiorRootRef of
                TypeRef nid -> nid
                GenRef gid ->
                    case IntMap.lookup (getGenNodeId gid) (cGenNodes constraint) of
                        Just genNode ->
                            let schemes = gnSchemes genNode
                                pick =
                                    listToMaybe
                                        [ r
                                        | r <- schemes
                                        , Binding.isUpper constraint (typeRef r) (typeRef edgeRoot)
                                        ]
                            in fromMaybe edgeRoot pick
                        Nothing -> edgeRoot
        binderMetaMap = IntMap.fromList [ (getNodeId bv, meta) | (bv, meta) <- binderArgs ]
    -- For edge-local ordering we use the expanded χe ids directly (no UF canonicalization).
    let keys = Order.orderKeysFromConstraintWith id constraint interiorRoot Nothing
    pure EdgeUnifyState
        { eusInteriorRoots = interiorRoots
        , eusBindersByRoot = bindersByRoot
        , eusInteriorByRoot = interiorByRoot
        , eusEdgeRoot = edgeRoot
        , eusEliminatedBinders = IntSet.empty
        , eusBinderMeta = binderMetaMap
        , eusBinderBounds = binderBounds
        , eusOrderKeys = keys
        , eusOps = []
        }

recordOp :: InstanceOp -> EdgeUnifyM ()
recordOp op = modify $ \st -> st { eusOps = eusOps st ++ [op] }

recordEliminate :: NodeId -> EdgeUnifyM ()
recordEliminate bv = do
    lift $ dropVarBind bv
    modify $ \st ->
        st { eusEliminatedBinders = IntSet.insert (getNodeId bv) (eusEliminatedBinders st) }

isEliminated :: NodeId -> EdgeUnifyM Bool
isEliminated bv = gets (IntSet.member (getNodeId bv) . eusEliminatedBinders)

recordRaisesFromTrace :: IntSet.IntSet -> [NodeId] -> EdgeUnifyM ()
recordRaisesFromTrace interiorNodes trace =
    forM_ trace $ \nid ->
        when (IntSet.member (getNodeId nid) interiorNodes) $ do
            already <- isEliminated nid
            isLocked <- checkNodeLocked nid
            when (not already && not isLocked) $
                recordOp (OpRaise nid)

preferBinderMetaRoot :: NodeId -> NodeId -> EdgeUnifyM (Maybe NodeId)
preferBinderMetaRoot root1 root2 = do
    st <- get
    metaRoots <- forM (IntMap.elems (eusBinderMeta st)) (lift . findRoot)
    let metaSet = IntSet.fromList (map getNodeId metaRoots)
        r1 = getNodeId root1
        r2 = getNodeId root2
    pure $ case (IntSet.member r1 metaSet, IntSet.member r2 metaSet) of
        (True, False) -> Just root1
        (False, True) -> Just root2
        _ -> Nothing

-- | Check if a node is under a rigid binder (locked) in the binding tree.
--
-- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4):
-- operations under rigidly bound nodes should be absent or rejected in the
-- normalized witness Ω.
--
-- Requirements: 5.2
checkNodeLocked :: NodeId -> EdgeUnifyM Bool
checkNodeLocked nid = do
    c <- lift $ gets psConstraint
    uf <- lift $ gets psUnionFind
    let canonical = UnionFind.frWith uf
        lookupParent :: NodeId -> EdgeUnifyM (Maybe (NodeId, BindFlag))
        lookupParent n =
            case Binding.lookupBindParentUnder canonical c (typeRef n) of
                Left err -> lift $ throwError (BindingTreeError err)
                Right Nothing -> pure Nothing
                Right (Just (TypeRef parent, flag)) -> pure (Just (parent, flag))
                Right (Just (GenRef _, _flag)) -> pure Nothing

        -- Paper `locked`/“under rigid binder” check: consider only strict ancestors,
        -- so a restricted node (its own edge rigid) is not treated as locked
        -- solely because of that edge.
        goStrict :: NodeId -> EdgeUnifyM Bool
        goStrict n = do
            mbParent <- lookupParent n
            case mbParent of
                Nothing -> pure False
                Just (_, BindRigid) -> pure True
                Just (parent, BindFlex) -> goStrict parent

    mbSelf <- lookupParent nid
    case mbSelf of
        Nothing -> pure False
        Just (parent, _flag) -> goStrict parent

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
        [] -> lift $ throwError (InternalError "pickRepBinderId: empty binder set")
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
                recordEliminate b
  where
    sortByM :: (a -> a -> EdgeUnifyM Ordering) -> [a] -> EdgeUnifyM [a]
    sortByM _ [] = pure []
    sortByM cmp xs = do
        let insertOne x [] = pure [x]
            insertOne x (y:ys) = do
                o <- cmp x y
                if o == LT then pure (x:y:ys) else (y:) <$> insertOne x ys
        foldM (\acc x -> insertOne x acc) [] xs

unifyAcyclicEdge :: NodeId -> NodeId -> EdgeUnifyM ()
unifyAcyclicEdge n1 n2 = do
    root1 <- lift $ findRoot n1
    root2 <- lift $ findRoot n2
    when (root1 /= root2) $ do
        st0 <- get
        let r1 = getNodeId root1
            r2 = getNodeId root2
            inInt1 = IntSet.member r1 (eusInteriorRoots st0)
            inInt2 = IntSet.member r2 (eusInteriorRoots st0)
            bs1 = IntMap.findWithDefault IntSet.empty r1 (eusBindersByRoot st0)
            bs2 = IntMap.findWithDefault IntSet.empty r2 (eusBindersByRoot st0)
            bs = IntSet.union bs1 bs2

        -- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4 / Fig. 10):
        -- `Raise(n)` is a real
        -- transformation of χe’s binding edges. We implement that effect via
        -- binding-edge harmonization and record the corresponding `OpRaise`
        -- steps in Ω.
        prefer <- preferBinderMetaRoot root1 root2
        trace <- lift $ unifyAcyclicRawWithRaiseTracePrefer prefer root1 root2
        rep <- lift $ findRoot root2
        let repId = getNodeId rep
            int1 = IntMap.findWithDefault IntSet.empty r1 (eusInteriorByRoot st0)
            int2 = IntMap.findWithDefault IntSet.empty r2 (eusInteriorByRoot st0)
            intAll = IntSet.union int1 int2

        -- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4):
        -- record Raise(n) for exactly the node(s)
        -- that were raised by binding-edge harmonization, restricted to I(r) and
        -- eliding operations under rigid binders.
        recordRaisesFromTrace intAll trace

        -- Update which UF roots correspond to classes containing interior nodes.
        modify $ \st ->
            let roots' =
                    if inInt1 || inInt2
                        then IntSet.insert repId (IntSet.delete r2 (IntSet.delete r1 (eusInteriorRoots st)))
                        else eusInteriorRoots st
                binders' =
                    let m0 = eusBindersByRoot st
                        m1 = if IntSet.null bs
                            then IntMap.delete r2 (IntMap.delete r1 m0)
                            else IntMap.insert repId bs (IntMap.delete r2 (IntMap.delete r1 m0))
                    in m1
                -- Also update interior-by-root map
                interior' =
                    let m0 = eusInteriorByRoot st
                        m1 = if IntSet.null intAll
                            then IntMap.delete r2 (IntMap.delete r1 m0)
                            else IntMap.insert repId intAll (IntMap.delete r2 (IntMap.delete r1 m0))
                    in m1
            in st { eusInteriorRoots = roots', eusBindersByRoot = binders', eusInteriorByRoot = interior' }

        -- Record merges among binders that became aliased (inside the interior).
        recordMergesIntoRep bs

        -- Record RaiseMerge when a binder-class merges with an exterior TyVar-class.
        when (IntSet.size bs >= 1) $ do
            repBinderId <- pickRepBinderId bs
            let repBinder = NodeId repBinderId
            case (IntSet.null bs1, IntSet.null bs2) of
                (False, True) | inInt1 && not inInt2 -> do
                    node2 <- lift $ getCanonicalNode root2
                    case node2 of
                        TyVar{} -> do
                            should <- shouldRecordRaiseMerge repBinder root2
                            already <- isEliminated repBinder
                            when (should && not already) $ do
                                -- Paper-shaped RaiseMerge is a sequence (Raise(n))^k; Merge(n, m).
                                -- We record it in that explicit form and let `normalizeInstanceOpsFull`
                                -- coalesce it back to `OpRaiseMerge`.
                                recordOp (OpRaise repBinder)
                                recordOp (OpMerge repBinder root2)
                                recordEliminate repBinder
                        _ -> pure ()
                (True, False) | inInt2 && not inInt1 -> do
                    node1 <- lift $ getCanonicalNode root1
                    case node1 of
                        TyVar{} -> do
                            should <- shouldRecordRaiseMerge rep root1
                            already <- isEliminated rep
                            when (should && not already) $ do
                                recordOp (OpRaise rep)
                                recordOp (OpMerge rep root1)
                                recordEliminate rep
                        _ -> pure ()
                _ -> pure ()

shouldRecordRaiseMerge :: NodeId -> NodeId -> EdgeUnifyM Bool
shouldRecordRaiseMerge binder ext = do
    edgeRoot <- gets eusEdgeRoot
    binderBounds <- gets eusBinderBounds
    extNode <- lift $ getNode ext
    case extNode of
        TyVar{} -> do
            case IntMap.lookup (getNodeId binder) binderBounds of
                Nothing -> do
                    case debugEdgeUnify
                        ( "shouldRecordRaiseMerge: binder="
                            ++ show binder
                            ++ " ext="
                            ++ show ext
                            ++ " bound=None"
                        )
                        ()
                        of
                            () -> pure ()
                    -- Unbounded binder: RaiseMerge is not needed; InstApp is expressible.
                    pure False
                Just bndOrig -> do
                    bndRoot <- lift $ findRoot bndOrig
                    extRoot <- lift $ findRoot ext
                    if bndRoot == extRoot
                        then do
                            case debugEdgeUnify
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
                                ()
                                of
                                    () -> pure ()
                            pure False
                        else do
                            above <- lift $ isBoundAboveInBindingTree edgeRoot extRoot
                            interiorRoots <- gets eusInteriorRoots
                            let inInterior = IntSet.member (getNodeId extRoot) interiorRoots
                            case debugEdgeUnify
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
                                ()
                                of
                                    () -> pure ()
                            pure (above || not inInterior)
        _ -> pure False

debugEdgeUnify :: String -> a -> a
debugEdgeUnify msg value =
    if debugEdgeUnifyEnabled
        then trace msg value
        else value

debugEdgeUnifyEnabled :: Bool
debugEdgeUnifyEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_BINDING"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugEdgeUnifyEnabled #-}

-- | True iff @ext@ is bound above @edgeRoot@ in the binding tree.
--
-- This corresponds to the paper side-condition for `RaiseMerge(n, m)`:
-- @m ∉ I(r)@ and @m@ is "bound above r".
isBoundAboveInBindingTree :: NodeId -> NodeId -> PresolutionM Bool
isBoundAboveInBindingTree edgeRoot ext = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
        edgeRootC = canonical edgeRoot
        extC = canonical ext
    pathRoot <- case Binding.bindingPathToRoot c0 (typeRef edgeRootC) of
        Left err -> throwError (BindingTreeError err)
        Right p -> pure p
    pathExt <- case Binding.bindingPathToRoot c0 (typeRef extC) of
        Left err -> throwError (BindingTreeError err)
        Right p -> pure p
    let rootAncestors =
            IntSet.fromList
                [ nodeRefKey ref
                | ref <- drop 1 pathRoot
                ]
        extHasAncestor =
            any (\ref -> IntSet.member (nodeRefKey ref) rootAncestors) pathExt
    pure extHasAncestor

unifyStructureEdge :: NodeId -> NodeId -> EdgeUnifyM ()
unifyStructureEdge n1 n2 = do
    root1 <- lift $ findRoot n1
    root2 <- lift $ findRoot n2
    case debugEdgeUnify
        ( "unifyStructureEdge: n1="
            ++ show n1
            ++ " root1="
            ++ show root1
            ++ " n2="
            ++ show n2
            ++ " root2="
            ++ show root2
        )
        ()
        of
            () -> pure ()
    if root1 == root2 then pure ()
    else do
        node1 <- lift $ getCanonicalNode n1
        node2 <- lift $ getCanonicalNode n2
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
                uf <- lift $ gets psUnionFind
                c0 <- lift $ gets psConstraint
                let canonical = UnionFind.frWith uf
                    lookupNode nid = IntMap.lookup (getNodeId nid) (cNodes c0)
                    targetC = canonical target
                    bndC = canonical bnd
                occurs <- case Traversal.occursInUnder canonical lookupNode targetC bndC of
                    Left _ -> pure True
                    Right ok -> pure ok
                if occurs
                    then lift $ throwError (OccursCheckPresolution targetC bndC)
                    else if bndC /= targetC
                        then lift (setVarBound targetC (Just bndC)) >> pure True
                        else pure False
        if isMeta1 || isMeta2
            then
                if isVar1 && isVar2
                    then do
                        unifyAcyclicEdge n1 n2
                        unifyVarBounds node1 node2
                    else do
                        let (metaRoot, otherNode) =
                                if isMeta1 then (root1, node2) else (root2, node1)
                        mbMetaBound <- lift $ lookupVarBound metaRoot
                        case mbMetaBound of
                            Just bMeta -> do
                                bMetaNode <- lift $ getCanonicalNode bMeta
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
                    (TyArrow { tnDom = d1, tnCod = c1 }, TyArrow { tnDom = d2, tnCod = c2 }) -> do
                        unifyStructureEdge d1 d2
                        unifyStructureEdge c1 c2
                    (TyForall { tnBody = b1 }, TyForall { tnBody = b2 }) ->
                        unifyStructureEdge b1 b2
                    _ -> pure ()

isBinderMetaRoot :: NodeId -> EdgeUnifyM Bool
isBinderMetaRoot root = do
    metaRoots <- gets eusBinderMeta >>= mapM (lift . findRoot) . IntMap.elems
    let metaSet = IntSet.fromList (map getNodeId metaRoots)
    pure (IntSet.member (getNodeId root) metaSet)
