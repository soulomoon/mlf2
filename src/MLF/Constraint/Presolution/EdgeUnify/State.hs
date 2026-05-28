{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : MLF.Constraint.Presolution.EdgeUnify.State
Description : Edge-local unification state and shared primitives
-}
module MLF.Constraint.Presolution.EdgeUnify.State (
    EdgeUnifyState(..),
    EdgeUnifyM,
    MonadEdgeUnify(..),
    initEdgeUnifyState,
    mkOmegaExecEnv,
    applyPendingWeaken,
    deleteInteriorKey,
    insertInteriorKey,
    isEliminated,
    mergeBinderMetaRoots,
    nullInteriorNodes,
    preferBinderMetaRoot,
    recordEliminate,
    recordRaisesFromTrace,
    unifyWithLockedFallback
) where

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe, listToMaybe)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base (
    CopyMap,
    InteriorNodes(..),
    InteriorSet,
    PendingWeakenOwner(..),
    memberInterior,
    lookupCopy,
    MonadPresolution(..),
    PresolutionError(..),
    PresolutionM,
    PresolutionState(..),
    mergeUnionFindState,
    setBindParentState
    )
import qualified MLF.Constraint.Presolution.Ops as Ops
import qualified MLF.Constraint.Presolution.Unify as PresolutionUnify
import MLF.Constraint.Presolution.StateAccess (
    PresolutionBindingSnapshot(..),
    bindingSnapshotLookupBindParent,
    bindingSnapshotPathToRoot,
    getBindingSnapshot,
    getConstraintAndCanonical,
    )
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import qualified MLF.Util.Order as Order
import qualified MLF.Witness.OmegaExec as OmegaExec

data EdgeUnifyState = EdgeUnifyState
    { eusInteriorRoots :: InteriorNodes
    , eusBindersByRoot :: IntMap IntSet.IntSet
    , eusInteriorByRoot :: IntMap InteriorNodes
    , eusEdgeRoot :: NodeId
    , eusInheritedPendingWeakens :: IntSet.IntSet
    , eusEliminatedBinders :: IntSet.IntSet
    , eusBinderMeta :: IntMap NodeId
    , eusBinderMetaRoots :: IntSet.IntSet
    , eusOrderKeys :: IntMap Order.OrderKey
    , eusPendingWeakenOwner :: PendingWeakenOwner
    , eusOps :: [InstanceOp]
    }

type EdgeUnifyM p = StateT EdgeUnifyState (PresolutionM p)

insertInteriorKey :: Int -> InteriorNodes -> InteriorNodes
insertInteriorKey k (InteriorNodes s) = InteriorNodes (IntSet.insert k s)

deleteInteriorKey :: Int -> InteriorNodes -> InteriorNodes
deleteInteriorKey k (InteriorNodes s) = InteriorNodes (IntSet.delete k s)

nullInteriorNodes :: InteriorNodes -> Bool
nullInteriorNodes (InteriorNodes s) = IntSet.null s

mergeBinderMetaRoots :: Int -> Int -> Int -> IntSet.IntSet -> IntSet.IntSet
mergeBinderMetaRoots r1 r2 rep roots
    | IntSet.member r1 roots || IntSet.member r2 roots =
        IntSet.insert rep (IntSet.delete r2 (IntSet.delete r1 roots))
    | otherwise = roots

-- | Typeclass for monads that support edge-local unification operations.
-- This allows functions to be polymorphic over the concrete monad stack,
-- reducing the need for explicit lift calls.
class MonadPresolution m => MonadEdgeUnify m where
    getEdgeUnifyState :: m EdgeUnifyState
    putEdgeUnifyState :: EdgeUnifyState -> m ()
    modifyEdgeUnifyState :: (EdgeUnifyState -> EdgeUnifyState) -> m ()
    getInteriorRoots :: m InteriorNodes
    getEdgeRoot :: m NodeId
    getBinderMeta :: m (IntMap NodeId)
    getOrderKeys :: m (IntMap Order.OrderKey)
    recordInstanceOp :: InstanceOp -> m ()
    liftPresolution :: PresolutionM (PresolutionPhaseOf m) a -> m a
    findRootM :: NodeId -> m NodeId
    unifyAcyclicRawWithRaiseTracePreferM :: Maybe NodeId -> NodeId -> NodeId -> m [NodeId]
    lookupVarBoundM :: NodeId -> m (Maybe NodeId)
    setVarBoundM :: NodeId -> Maybe NodeId -> m ()
    dropVarBindM :: NodeId -> m ()
    throwPresolutionErrorM :: PresolutionError -> m a
    isBoundAboveInBindingTreeM :: NodeId -> NodeId -> m Bool
    queuePendingWeakenM :: NodeId -> m ()

instance MonadEdgeUnify (EdgeUnifyM p) where
    getEdgeUnifyState = get
    putEdgeUnifyState = put
    modifyEdgeUnifyState = modify'
    getInteriorRoots = gets eusInteriorRoots
    getEdgeRoot = gets eusEdgeRoot
    getBinderMeta = gets eusBinderMeta
    getOrderKeys = gets eusOrderKeys
    recordInstanceOp op = modify' $ \st -> st { eusOps = op : eusOps st }
    liftPresolution = lift
    findRootM nid = lift $ Ops.findRoot nid
    unifyAcyclicRawWithRaiseTracePreferM prefer n1 n2 =
        lift $ PresolutionUnify.unifyAcyclicRawWithRaiseTracePrefer prefer n1 n2
    lookupVarBoundM nid = lift $ Ops.lookupVarBound nid
    setVarBoundM nid mb =
        case mb of
            Nothing -> lift $ Ops.setVarBound nid Nothing
            Just bnd -> do
                nidRoot <- findRootM nid
                bndRoot <- findRootM bnd
                if nidRoot == bndRoot
                    then pure ()
                    else lift $ Ops.setVarBound nid (Just bnd)
    dropVarBindM nid = lift $ Ops.dropVarBind nid
    throwPresolutionErrorM err = lift $ throwError err
    isBoundAboveInBindingTreeM edgeRoot ext =
        liftPresolution $ isBoundAboveInBindingTree edgeRoot ext
    queuePendingWeakenM nid = do
        owner <- gets eusPendingWeakenOwner
        liftPresolution $ queuePendingWeakenWithOwner owner nid

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
mkOmegaExecEnv :: CopyMap -> OmegaExec.OmegaExecEnv (EdgeUnifyM p)
mkOmegaExecEnv copyMap =
    OmegaExec.OmegaExecEnv
        { OmegaExec.omegaMetaFor = metaFor
        , OmegaExec.omegaLookupMeta = \bv -> pure (lookupCopy bv copyMap)
        , OmegaExec.omegaLookupVarBound = \meta -> lookupVarBoundM meta
        , OmegaExec.omegaSetVarBound = \meta mb -> setVarBoundM meta mb
        , OmegaExec.omegaDropVarBind = \meta -> dropVarBindM meta
        , OmegaExec.omegaUnifyNoMerge = unifyAcyclicEdgeNoMerge
        , OmegaExec.omegaRecordEliminate = recordEliminate
        , OmegaExec.omegaIsEliminated = isEliminated
        , OmegaExec.omegaEliminatedBinders = do
            elims <- gets eusEliminatedBinders
            pure (map NodeId (IntSet.toList elims))
        , OmegaExec.omegaWeakenMeta = \meta -> queuePendingWeakenM meta
        }
  where
    metaFor :: NodeId -> EdgeUnifyM p NodeId
    metaFor bv =
        case lookupCopy bv copyMap of
            Just m -> pure m
            Nothing ->
                throwPresolutionErrorM
                    (InternalError ("mkOmegaExecEnv: missing copy for binder " ++ show bv))

queuePendingWeakenWithOwner :: PendingWeakenOwner -> NodeId -> PresolutionM p ()
queuePendingWeakenWithOwner owner nid =
    modify' $ \st ->
        st
            { psPendingWeakens = IntSet.insert (getNodeId nid) (psPendingWeakens st)
            , psPendingWeakenOwners =
                IntMap.insertWith
                    (\existing _new -> existing)
                    (getNodeId nid)
                    owner
                    (psPendingWeakenOwners st)
            }

applyPendingWeaken :: NodeId -> PresolutionM p ()
applyPendingWeaken nid0 = do
    retriable <- applyAtTarget nid0
    when retriable $ do
        (_c0, canonical) <- getConstraintAndCanonical
        let nidCanon = canonical nid0
        when (nidCanon /= nid0) $ do
            _ <- applyAtTarget nidCanon
            pure ()
  where
    applyAtTarget :: NodeId -> PresolutionM p Bool
    applyAtTarget target = do
        c0 <- getConstraint
        case Binding.lookupBindParent c0 (typeRef target) of
            Nothing -> pure False
            Just (_p, BindRigid) -> pure False
            Just (parent, BindFlex) -> do
                modify' $ \st ->
                    let st1 =
                            setBindParentState
                                (typeRef target)
                                (parent, BindRigid)
                                st
                        c1 = psConstraint st1
                    in st1
                        { psConstraint =
                            c1
                                { cWeakenedVars =
                                    IntSet.insert
                                        (getNodeId target)
                                        (cWeakenedVars c1)
                                }
                        }
                pure False

-- | Edge-local union like 'unifyAcyclicEdge', but without emitting merge-like
-- witness ops. This is used to *execute* base `Merge` operations (already
-- recorded in Ω) without accidentally introducing an opposing Phase-2 merge.
unifyAcyclicEdgeNoMerge :: NodeId -> NodeId -> EdgeUnifyM p ()
unifyAcyclicEdgeNoMerge n1 n2 = do
    root1 <- findRootM n1
    root2 <- findRootM n2
    when (root1 /= root2) $ do
        st0 <- get
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

        modify' $ \st ->
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
                metaRoots' = mergeBinderMetaRoots r1 r2 repId (eusBinderMetaRoots st)
            in st
                { eusInteriorRoots = roots'
                , eusBindersByRoot = binders'
                , eusInteriorByRoot = interior'
                , eusBinderMetaRoots = metaRoots'
                }

initEdgeUnifyState
    :: [(NodeId, NodeId)]
    -> InteriorSet
    -> NodeId
    -> PendingWeakenOwner
    -> PresolutionM p EdgeUnifyState
initEdgeUnifyState binderArgs interior edgeRoot pendingOwner = do
    inheritedPendingWeakens <- gets psPendingWeakens
    interiorRootEntries <- forM (IntSet.toList interior) $ \i -> do
        r <- Ops.findRoot (NodeId i)
        pure (i, r)
    let interiorRoots =
            InteriorNodes (IntSet.fromList [getNodeId r | (_i, r) <- interiorRootEntries])
    binderRootEntries <- forM binderArgs $ \(bv, arg) -> do
        r <- Ops.findRoot arg
        pure (bv, r)
    let bindersByRoot =
            IntMap.fromListWith
                IntSet.union
                [ (getNodeId r, IntSet.singleton (getNodeId bv))
                | (bv, r) <- binderRootEntries
                ]
        binderMetaRoots =
            IntSet.fromList [getNodeId r | (_bv, r) <- binderRootEntries]
        interiorByRoot =
            IntMap.fromListWith
                (<>)
                [ (getNodeId r, InteriorNodes (IntSet.singleton i))
                | (i, r) <- interiorRootEntries
                ]
    constraint <- getConstraint
    let interiorRootRef =
            case Binding.lookupBindParent constraint (typeRef edgeRoot) of
                Just (parent, _) -> parent
                Nothing -> typeRef edgeRoot
        interiorRoot =
            case interiorRootRef of
                TypeRef nid -> nid
                GenRef gid ->
                    case NodeAccess.lookupGenNode constraint gid of
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
        binderMetaMap = IntMap.fromList [(getNodeId bv, meta) | (bv, meta) <- binderArgs]
    let keys =
            if length binderArgs <= 1
                then IntMap.empty
                else Order.orderKeysFromConstraintWith id constraint interiorRoot Nothing
    pure EdgeUnifyState
        { eusInteriorRoots = interiorRoots
        , eusBindersByRoot = bindersByRoot
        , eusInteriorByRoot = interiorByRoot
        , eusEdgeRoot = edgeRoot
        , eusInheritedPendingWeakens = inheritedPendingWeakens
        , eusEliminatedBinders = IntSet.empty
        , eusBinderMeta = binderMetaMap
        , eusBinderMetaRoots = binderMetaRoots
        , eusOrderKeys = keys
        , eusPendingWeakenOwner = pendingOwner
        , eusOps = []
        }

flushInheritedPendingWeakensOnce :: EdgeUnifyM p Bool
flushInheritedPendingWeakensOnce = do
    inherited <- gets eusInheritedPendingWeakens
    if IntSet.null inherited
        then pure False
        else do
            pendingNow <- liftPresolution (psPendingWeakens <$> getPresolutionState)
            let toFlush = IntSet.intersection inherited pendingNow
            modify' $ \st -> st { eusInheritedPendingWeakens = IntSet.empty }
            if IntSet.null toFlush
                then pure False
                else do
                    liftPresolution $ forM_ (IntSet.toList toFlush) (applyPendingWeaken . NodeId)
                    liftPresolution $
                        modify' $ \st ->
                            st
                                { psPendingWeakens = IntSet.difference (psPendingWeakens st) toFlush
                                , psPendingWeakenOwners =
                                    IntMap.withoutKeys (psPendingWeakenOwners st) toFlush
                                }
                    pure True

unifyWithLockedFallback :: Maybe NodeId -> NodeId -> NodeId -> EdgeUnifyM p [NodeId]
unifyWithLockedFallback prefer left right =
    unifyAcyclicRawWithRaiseTracePreferM prefer left right
        `catchError` handleLocked
  where
    forceUnionWithoutRaise :: EdgeUnifyM p [NodeId]
    forceUnionWithoutRaise = do
        rootLeft <- findRootM left
        rootRight <- findRootM right
        when (rootLeft /= rootRight) $ do
            let (fromRoot, toRoot) =
                    case prefer of
                        Just p
                            | p == rootLeft -> (rootRight, rootLeft)
                            | p == rootRight -> (rootLeft, rootRight)
                        _ -> (rootLeft, rootRight)
            liftPresolution $
                modify' (mergeUnionFindState fromRoot toRoot)
        pure []

    retryAfterFlush :: EdgeUnifyM p [NodeId]
    retryAfterFlush = do
        recovered <- flushInheritedPendingWeakensOnce
        if recovered
            then
                unifyAcyclicRawWithRaiseTracePreferM prefer left right
                    `catchError` \retryErr ->
                        case retryErr of
                            BindingTreeError OperationOnLockedNode{} -> forceUnionWithoutRaise
                            _ -> throwPresolutionErrorM retryErr
            else forceUnionWithoutRaise

    trySwap :: EdgeUnifyM p [NodeId]
    trySwap =
        unifyAcyclicRawWithRaiseTracePreferM prefer right left
            `catchError` \swapErr ->
                case swapErr of
                    BindingTreeError OperationOnLockedNode{} -> retryAfterFlush
                    _ -> throwPresolutionErrorM swapErr

    handleLocked :: PresolutionError -> EdgeUnifyM p [NodeId]
    handleLocked err =
        case err of
            BindingTreeError OperationOnLockedNode{} -> trySwap
            _ -> throwPresolutionErrorM err

recordEliminate :: NodeId -> EdgeUnifyM p ()
recordEliminate bv = do
    dropVarBindM bv
    modify' $ \st ->
        st { eusEliminatedBinders = IntSet.insert (getNodeId bv) (eusEliminatedBinders st) }

isEliminated :: NodeId -> EdgeUnifyM p Bool
isEliminated bv = gets (IntSet.member (getNodeId bv) . eusEliminatedBinders)

recordRaisesFromTrace :: InteriorNodes -> [NodeId] -> EdgeUnifyM p ()
recordRaisesFromTrace interiorNodes raiseTrace = do
    candidates <-
        foldM
            (\acc nid ->
                if memberInterior nid interiorNodes
                    then do
                        already <- isEliminated nid
                        pure $ if already then acc else nid : acc
                    else pure acc
            )
            []
            raiseTrace
    when (not (null candidates)) $ do
        snapshot <- lift getBindingSnapshot
        forM_ (reverse candidates) $ \nid -> do
            isLocked <- lift $ checkNodeLockedInSnapshot snapshot nid
            when (not isLocked) $
                recordInstanceOp (OpRaise nid)

preferBinderMetaRoot :: NodeId -> NodeId -> EdgeUnifyM p (Maybe NodeId)
preferBinderMetaRoot root1 root2 = do
    st <- get
    let metaSet = eusBinderMetaRoots st
        r1 = getNodeId root1
        r2 = getNodeId root2
    pure $ case (IntSet.member r1 metaSet, IntSet.member r2 metaSet) of
        (True, False) -> Just root1
        (False, True) -> Just root2
        _ -> Nothing

checkNodeLockedInSnapshot :: PresolutionBindingSnapshot p -> NodeId -> PresolutionM p Bool
checkNodeLockedInSnapshot snapshot nid =
    goSelf IntSet.empty (typeRef nid)
  where
    goSelf visited ref
        | IntSet.member (nodeRefKey ref) visited = pure False
        | otherwise = do
            mbSelf <- bindingSnapshotLookupBindParent snapshot ref
            case mbSelf of
                Nothing -> pure False
                Just (TypeRef parent, _flag) ->
                    goStrict (IntSet.insert (nodeRefKey ref) visited) (typeRef parent)
                Just (GenRef _, _flag) -> pure False

    goStrict visited ref
        | IntSet.member (nodeRefKey ref) visited = pure False
        | otherwise = do
            mbParent <- bindingSnapshotLookupBindParent snapshot ref
            case mbParent of
                Nothing -> pure False
                Just (_, BindRigid) -> pure True
                Just (TypeRef parent, BindFlex) ->
                    goStrict (IntSet.insert (nodeRefKey ref) visited) (typeRef parent)
                Just (GenRef _, BindFlex) -> pure False

isBoundAboveInBindingTree :: NodeId -> NodeId -> PresolutionM p Bool
isBoundAboveInBindingTree edgeRoot ext = do
    snapshot <- getBindingSnapshot
    let canonical = pbsCanonical snapshot
    let edgeRootC = canonical edgeRoot
        extC = canonical ext
    pathRoot <- bindingSnapshotPathToRoot snapshot (typeRef edgeRootC)
    pathExt <- bindingSnapshotPathToRoot snapshot (typeRef extC)
    let rootAncestors =
            IntSet.fromList
                [ nodeRefKey ref
                | ref <- drop 1 pathRoot
                ]
        extHasAncestor =
            any (\ref -> IntSet.member (nodeRefKey ref) rootAncestors) pathExt
    pure extHasAncestor
