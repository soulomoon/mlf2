{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : MLF.Constraint.Presolution.EdgeUnify.State
Description : Edge-local unification state and shared primitives
-}
module MLF.Constraint.Presolution.EdgeUnify.State (
    EdgeUnifyStats(..),
    EdgeUnifyState(..),
    EdgeUnifyM,
    MonadEdgeUnify(..),
    addEdgeUnifyStats,
    clearEdgeUnifyStructureCache,
    emptyEdgeUnifyStats,
    initEdgeUnifyState,
    initEdgeUnifyStateWithCopyMap,
    initEdgeUnifyStateWithStats,
    mkOmegaExecEnv,
    applyPendingWeaken,
    deleteInteriorKey,
    insertInteriorKey,
    isEliminated,
    isScheduledUnboundedBinderMetaRoot,
    mergeBinderMetaRoots,
    nullInteriorNodes,
    preferBinderMetaRoot,
    recordEliminate,
    recordEdgeUnifyStat,
    recordEdgeUnifyStatN,
    recordCurrentInteriorRaises,
    recordRaisesFromTrace,
    sourceWitnessNode,
    sourceWitnessNodeFor,
    sourceWitnessNodeIgnoringAmbiguity,
    structurePairSeenOrInsert,
    unifyWithLockedFallback
) where

import Control.Monad (foldM, forM_, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Word (Word64)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Presolution.Base (
    CopyMap,
    EdgeSourceInterior(..),
    EdgeTrace(..),
    InteriorNodes(..),
    InteriorSet,
    PendingWeakenOwner(..),
    memberInterior,
    lookupCopy,
    MonadPresolution(..),
    PresolutionError(..),
    PresolutionM,
    PresolutionState(..),
    psEdgeTraces,
    psEdgeWitnesses,
    WeakenReplayCertificate,
    certifyAppliedNonRootWeakenReplay,
    certifyEliminatedNonRootWeakenReplay,
    getCopyMapping,
    setBindParentState,
    weakenReplayCertificateSource
    )
import qualified MLF.Util.UnionFind as UnionFind
import qualified MLF.Constraint.Presolution.Ops as Ops
import qualified MLF.Constraint.Presolution.Unify as PresolutionUnify
import MLF.Constraint.Presolution.StateAccess (
    PresolutionBindingSnapshot(..),
    bindingSnapshotNodeKind,
    bindingSnapshotPathToRoot,
    getBindingSnapshot,
    getConstraintAndCanonical,
    )
import MLF.Constraint.Presolution.Witness (EdgeWitnessOp(..), edgeWitnessInstanceOp)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import qualified MLF.Util.Order as Order
import qualified MLF.Witness.OmegaExec as OmegaExec

data EdgeUnifyStats = EdgeUnifyStats
    { eusFindRootCalls :: !Word64
    , eusCanonicalNodeLookups :: !Word64
    , eusLookupVarBoundCalls :: !Word64
    , eusSetVarBoundAttempts :: !Word64
    , eusOccursChecks :: !Word64
    , eusUnifyAcyclicCalls :: !Word64
    , eusUnifyStructureCalls :: !Word64
    , eusUnifyStructureSameRoot :: !Word64
    , eusUnifyStructureMetaPath :: !Word64
    , eusUnifyStructureVarVar :: !Word64
    , eusUnifyStructureChildEdges :: !Word64
    }
    deriving (Eq, Show)

emptyEdgeUnifyStats :: EdgeUnifyStats
emptyEdgeUnifyStats =
    EdgeUnifyStats
        { eusFindRootCalls = 0
        , eusCanonicalNodeLookups = 0
        , eusLookupVarBoundCalls = 0
        , eusSetVarBoundAttempts = 0
        , eusOccursChecks = 0
        , eusUnifyAcyclicCalls = 0
        , eusUnifyStructureCalls = 0
        , eusUnifyStructureSameRoot = 0
        , eusUnifyStructureMetaPath = 0
        , eusUnifyStructureVarVar = 0
        , eusUnifyStructureChildEdges = 0
        }

addEdgeUnifyStats :: EdgeUnifyStats -> EdgeUnifyStats -> EdgeUnifyStats
addEdgeUnifyStats a b =
    EdgeUnifyStats
        { eusFindRootCalls = eusFindRootCalls a + eusFindRootCalls b
        , eusCanonicalNodeLookups = eusCanonicalNodeLookups a + eusCanonicalNodeLookups b
        , eusLookupVarBoundCalls = eusLookupVarBoundCalls a + eusLookupVarBoundCalls b
        , eusSetVarBoundAttempts = eusSetVarBoundAttempts a + eusSetVarBoundAttempts b
        , eusOccursChecks = eusOccursChecks a + eusOccursChecks b
        , eusUnifyAcyclicCalls = eusUnifyAcyclicCalls a + eusUnifyAcyclicCalls b
        , eusUnifyStructureCalls = eusUnifyStructureCalls a + eusUnifyStructureCalls b
        , eusUnifyStructureSameRoot = eusUnifyStructureSameRoot a + eusUnifyStructureSameRoot b
        , eusUnifyStructureMetaPath = eusUnifyStructureMetaPath a + eusUnifyStructureMetaPath b
        , eusUnifyStructureVarVar = eusUnifyStructureVarVar a + eusUnifyStructureVarVar b
        , eusUnifyStructureChildEdges = eusUnifyStructureChildEdges a + eusUnifyStructureChildEdges b
        }

data EdgeUnifyState = EdgeUnifyState
    { eusInteriorRoots :: InteriorNodes
    , eusBindersByRoot :: IntMap IntSet.IntSet
    , eusInteriorByRoot :: IntMap InteriorNodes
    , eusSourceInterior :: EdgeSourceInterior
    , eusLockedSourceNodes :: IntSet.IntSet
    , eusSourceRaiseAuthorityNodes :: IntSet.IntSet
    , eusSourceEdgeRoot :: NodeId
    , eusEdgeRoot :: NodeId
    , eusTerminalRootTransition :: Bool
    , eusRootRaiseMergeRecorded :: Bool
    , eusTerminalTransitionSources :: IntSet.IntSet
    , eusInheritedPendingWeakens :: IntSet.IntSet
    , eusEliminatedBinders :: IntSet.IntSet
    , eusBinderMeta :: IntMap NodeId
    , eusOriginallyBoundBinders :: IntSet.IntSet
    , eusScheduledWeakenMetas :: IntSet.IntSet
    , eusBinderMetaRoots :: IntSet.IntSet
    , eusOrderKeys :: Maybe (IntMap Order.OrderKey)
    , eusPendingWeakenOwner :: PendingWeakenOwner
    , eusOps :: [EdgeWitnessOp]
    , eusCopyMap :: CopyMap
    , eusCopySourcesByRawDestination :: IntMap IntSet.IntSet
    , eusSourceNodeKeys :: IntSet.IntSet
    , eusRootCache :: IntMap NodeId
    , eusStructurePairs :: IntMap IntSet.IntSet
    , eusCollectStats :: !Bool
    , eusStats :: !EdgeUnifyStats
    , eusRootCacheVersion :: !Int
    , eusRootCacheGen :: !Int
    }

type EdgeUnifyM p = StateT EdgeUnifyState (PresolutionM p)

-- | Recover the unique frozen construction source for a live node class.
-- Exact copy provenance is authoritative; live source aliases are considered
-- only when no copied source reaches the class.
sourceWitnessNode :: NodeId -> EdgeUnifyM p (Maybe NodeId)
sourceWitnessNode = sourceWitnessNodeFor "external query"

sourceWitnessNodeFor :: String -> NodeId -> EdgeUnifyM p (Maybe NodeId)
sourceWitnessNodeFor owner = sourceWitnessNodeWithAmbiguity owner False

sourceWitnessNodeIgnoringAmbiguity
    :: String
    -> NodeId
    -> EdgeUnifyM p (Maybe NodeId)
sourceWitnessNodeIgnoringAmbiguity owner =
    sourceWitnessNodeWithAmbiguity owner True

sourceWitnessNodeWithAmbiguity
    :: String
    -> Bool
    -> NodeId
    -> EdgeUnifyM p (Maybe NodeId)
sourceWitnessNodeWithAmbiguity owner ignoreAmbiguity node = do
    nodeRoot <- findRootM node
    rawSources <- gets eusCopySourcesByRawDestination
    sourceNodeKeys <- gets eusSourceNodeKeys
    copyCandidates <-
        foldM
            (collectCopySources nodeRoot)
            IntSet.empty
            (IntMap.toList rawSources)
    case uniqueSource "copy" nodeRoot copyCandidates of
        Left _ | ignoreAmbiguity -> pure Nothing
        Left err -> throwPresolutionErrorM err
        Right (Just source) -> pure (Just source)
        Right Nothing ->
            if IntSet.member (getNodeId node) sourceNodeKeys
                then pure (Just node)
                else do
                    aliasCandidates <-
                        foldM
                            (collectSourceAliases nodeRoot)
                            IntSet.empty
                            (IntSet.toList sourceNodeKeys)
                    case uniqueSource "source-class" nodeRoot aliasCandidates of
                        Left _ | ignoreAmbiguity -> pure Nothing
                        Left err -> throwPresolutionErrorM err
                        Right source -> pure source
  where
    collectCopySources nodeRoot candidates (rawDestinationKey, rawSourceKeys) = do
        destinationRoot <- findRootM (NodeId rawDestinationKey)
        if destinationRoot /= nodeRoot
            then pure candidates
            else pure (IntSet.union candidates rawSourceKeys)

    collectSourceAliases nodeRoot candidates rawSourceKey = do
        sourceRoot <- findRootM (NodeId rawSourceKey)
        pure $
            if sourceRoot /= nodeRoot
                then candidates
                else IntSet.insert rawSourceKey candidates

    uniqueSource label nodeRoot candidates =
        case map NodeId (IntSet.toList candidates) of
            [] -> Right Nothing
            [source] -> Right (Just source)
            sourceCandidates ->
                Left
                    ( InternalError
                        ( "ambiguous construction-time "
                            ++ label
                            ++ " source for "
                            ++ show nodeRoot
                            ++ ": "
                            ++ show sourceCandidates
                            ++ " while resolving "
                            ++ owner
                        )
                    )

insertInteriorKey :: Int -> InteriorNodes -> InteriorNodes
insertInteriorKey k (InteriorNodes s) = InteriorNodes (IntSet.insert k s)

deleteInteriorKey :: Int -> InteriorNodes -> InteriorNodes
deleteInteriorKey k (InteriorNodes s) = InteriorNodes (IntSet.delete k s)

nullInteriorNodes :: InteriorNodes -> Bool
nullInteriorNodes (InteriorNodes s) = IntSet.null s

recordEdgeUnifyStat :: (EdgeUnifyStats -> EdgeUnifyStats) -> EdgeUnifyM p ()
recordEdgeUnifyStat update = do
    st <- get
    when (eusCollectStats st) $
        put $! st { eusStats = update (eusStats st) }

recordEdgeUnifyStatN :: Word64 -> (Word64 -> EdgeUnifyStats -> EdgeUnifyStats) -> EdgeUnifyM p ()
recordEdgeUnifyStatN count update =
    recordEdgeUnifyStat (update count)

clearEdgeUnifyRootCache :: EdgeUnifyM p ()
clearEdgeUnifyRootCache =
    modify' $ \st -> st { eusRootCacheVersion = eusRootCacheVersion st + 1 }

clearEdgeUnifyStructureCache :: EdgeUnifyM p ()
clearEdgeUnifyStructureCache =
    modify' $ \st -> st { eusRootCacheVersion = eusRootCacheVersion st + 1 }

-- | Lazily clear root/structure caches if the version counter has advanced.
-- Returns the (possibly updated) state.
syncRootCacheVersion :: EdgeUnifyState -> EdgeUnifyM p EdgeUnifyState
syncRootCacheVersion st =
    let ver = eusRootCacheVersion st
    in if eusRootCacheGen st /= ver
        then do
            let st' = st { eusRootCache = IntMap.empty, eusStructurePairs = IntMap.empty, eusRootCacheGen = ver }
            put $! st'
            pure st'
        else pure st

structurePairSeenOrInsert :: NodeId -> NodeId -> EdgeUnifyM p Bool
structurePairSeenOrInsert left right = do
    let leftKey = getNodeId left
        rightKey = getNodeId right
        (lo, hi) =
            if leftKey <= rightKey
                then (leftKey, rightKey)
                else (rightKey, leftKey)
    st0 <- get
    st <- syncRootCacheVersion st0
    let peers = IntMap.findWithDefault IntSet.empty lo (eusStructurePairs st)
    if IntSet.member hi peers
        then pure True
        else do
            let pairs' = IntMap.insert lo (IntSet.insert hi peers) (eusStructurePairs st)
            put $! st { eusStructurePairs = pairs' }
            pure False

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
    getOrderKeys :: m (Maybe (IntMap Order.OrderKey))
    recordInstanceOp :: EdgeWitnessOp -> m ()
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
    findRootM nid = do
        st0 <- get
        st <- syncRootCacheVersion st0
        let key = getNodeId nid
            collectStats = eusCollectStats st
            stats' =
                if collectStats
                    then
                        let stats = eusStats st
                        in stats { eusFindRootCalls = eusFindRootCalls stats + 1 }
                    else eusStats st
        case IntMap.lookup key (eusRootCache st) of
            Just root -> do
                when collectStats $
                    put $! st { eusStats = stats' }
                pure root
            Nothing -> do
                root <- lift $ Ops.findRoot nid
                let cache' =
                        IntMap.insert
                            (getNodeId root)
                            root
                            (IntMap.insert key root (eusRootCache st))
                put $! st { eusRootCache = cache', eusStats = stats' }
                pure root
    unifyAcyclicRawWithRaiseTracePreferM prefer n1 n2 =
        lift $ PresolutionUnify.unifyAcyclicRawWithRaiseTracePrefer prefer n1 n2
    lookupVarBoundM nid = do
        recordEdgeUnifyStat $ \stats ->
            stats { eusLookupVarBoundCalls = eusLookupVarBoundCalls stats + 1 }
        root <- findRootM nid
        c <- lift getConstraint
        pure (VarStore.lookupVarBound c root)
    setVarBoundM nid mb = do
        recordEdgeUnifyStat $ \stats ->
            stats { eusSetVarBoundAttempts = eusSetVarBoundAttempts stats + 1 }
        case mb of
            Nothing -> do
                nidRoot <- findRootM nid
                raiseTrace <-
                    lift $ Ops.setCanonicalVarBoundForEdgeWithRaiseTrace nidRoot Nothing
                recordCurrentInteriorRaises raiseTrace
                clearEdgeUnifyStructureCache
            Just bnd -> do
                nidRoot <- findRootM nid
                bndRoot <- findRootM bnd
                if nidRoot == bndRoot
                    then pure ()
                    else do
                        raiseTrace <-
                            lift $
                                Ops.setCanonicalVarBoundForEdgeWithRaiseTrace
                                    nidRoot
                                    (Just bndRoot)
                        recordCurrentInteriorRaises raiseTrace
                        clearEdgeUnifyStructureCache
    dropVarBindM nid = do
        -- Elimination is keyed by the copied binder's own identity, not its
        -- current UF representative.  `Ops.dropVarBind` records that exact
        -- identity, so an aliased exterior representative remains live.
        lift $ Ops.dropVarBind nid
        clearEdgeUnifyStructureCache
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
        , OmegaExec.omegaRecordEliminate = markEliminated
        , OmegaExec.omegaIsEliminated = isEliminated
        , OmegaExec.omegaEliminatedBinders = do
            elims <- gets eusEliminatedBinders
            pure (map NodeId (IntSet.toList elims))
        , OmegaExec.omegaRegisterWeakenMeta = \meta ->
            modify' $ \st ->
                st
                    { eusScheduledWeakenMetas =
                        IntSet.insert (getNodeId meta) (eusScheduledWeakenMetas st)
                    }
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
    (c0, canonical) <- getConstraintAndCanonical
    stBeforeDecision <- get
    -- The edge witness eliminates an aliased source binder.  Physical
    -- weakening is needed only while its copied meta remains its own class;
    -- weakening the live exterior representative would destroy polymorphism.
    if canonical nid0 == nid0
        then do
            changed <- applyAtTarget nid0
            when changed $ do
                c1 <- getConstraint
                let certificates =
                        mapMaybe
                            (certifyAppliedCandidate c0 c1 canonical)
                            (weakenCandidates stBeforeDecision nid0)
                recordCertificates certificates
        else do
            let certificates =
                    mapMaybe
                        (certifyEliminatedCandidate c0 canonical)
                        (weakenCandidates stBeforeDecision nid0)
            recordCertificates certificates
  where
    weakenCandidates
        :: PresolutionState p
        -> NodeId
        -> [(Int, NodeId, NodeId, Bool, Bool)]
    weakenCandidates st target =
        [ ( edgeKey
          , source
          , etResultRoot trace
          , mergedBeforeWeaken
          , graftedBeforeWeaken
          )
        | (edgeKey, trace) <- IntMap.toList (psEdgeTraces st)
        , Just witness <- [IntMap.lookup edgeKey (psEdgeWitnesses st)]
        , (source, mergedBeforeWeaken, graftedBeforeWeaken) <-
            weakenSources (getInstanceOps (ewWitness witness))
        , Just copied <- [lookupCopy source (etCopyMap trace)]
        , copied == target
        ]

    weakenSources :: [InstanceOp] -> [(NodeId, Bool, Bool)]
    weakenSources = go IntSet.empty IntSet.empty
      where
        go _ _ [] = []
        go merged grafted (op : rest) =
            case op of
                OpGraft _ target ->
                    go
                        merged
                        (IntSet.insert (getNodeId target) grafted)
                        rest
                OpMerge operated _ ->
                    go
                        (IntSet.insert (getNodeId operated) merged)
                        grafted
                        rest
                OpWeaken source ->
                    ( source
                    , IntSet.member (getNodeId source) merged
                    , IntSet.member (getNodeId source) grafted
                    ) : go merged grafted rest
                _ -> go merged grafted rest

    certifyAppliedCandidate
        :: Constraint p
        -> Constraint p
        -> (NodeId -> NodeId)
        -> (Int, NodeId, NodeId, Bool, Bool)
        -> Maybe (Int, WeakenReplayCertificate)
    certifyAppliedCandidate before after canonical (edgeKey, source, root, _merged, grafted)
        | grafted = Nothing
        | otherwise =
            fmap
                (\certificate -> (edgeKey, certificate))
                ( certifyAppliedNonRootWeakenReplay
                    before
                    after
                    canonical
                    source
                    nid0
                    root
                )

    certifyEliminatedCandidate
        :: Constraint p
        -> (NodeId -> NodeId)
        -> (Int, NodeId, NodeId, Bool, Bool)
        -> Maybe (Int, WeakenReplayCertificate)
    certifyEliminatedCandidate constraint canonical (edgeKey, source, root, merged, grafted)
        | not merged || grafted = Nothing
        | otherwise =
            fmap
                (\certificate -> (edgeKey, certificate))
                ( certifyEliminatedNonRootWeakenReplay
                    constraint
                    constraint
                    canonical
                    source
                    nid0
                    root
                )

    recordCertificates :: [(Int, WeakenReplayCertificate)] -> PresolutionM p ()
    recordCertificates certificates = do
        existing <- gets psWeakenReplayCertificates
        updated <- foldM insertCertificate existing certificates
        modify' $ \st -> st {psWeakenReplayCertificates = updated}

    insertCertificate
        :: IntMap (IntMap WeakenReplayCertificate)
        -> (Int, WeakenReplayCertificate)
        -> PresolutionM p (IntMap (IntMap WeakenReplayCertificate))
    insertCertificate allCertificates (edgeKey, certificate) =
        let sourceKey = getNodeId (weakenReplayCertificateSource certificate)
            edgeCertificates =
                IntMap.findWithDefault IntMap.empty edgeKey allCertificates
        in case IntMap.lookup sourceKey edgeCertificates of
            Nothing ->
                pure $
                    IntMap.insert
                        edgeKey
                        (IntMap.insert sourceKey certificate edgeCertificates)
                        allCertificates
            Just previous
                | previous == certificate -> pure allCertificates
                | otherwise ->
                    throwError $
                        InternalError $
                            "conflicting construction-time Weaken replay certificates for edge "
                                ++ show (EdgeId edgeKey)
                                ++ ", source "
                                ++ show (NodeId sourceKey)

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
                pure True

-- | Edge-local union like 'unifyAcyclicEdge', but without emitting merge-like
-- witness ops. This is used to *execute* base `Merge` operations (already
-- recorded in Ω) without accidentally introducing an opposing Phase-2 merge.
unifyAcyclicEdgeNoMerge :: NodeId -> NodeId -> EdgeUnifyM p ()
unifyAcyclicEdgeNoMerge n1 n2 = do
    recordEdgeUnifyStat $ \stats ->
        stats { eusUnifyAcyclicCalls = eusUnifyAcyclicCalls stats + 1 }
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

transitivelyFlexBoundTo :: Constraint p -> NodeId -> NodeId -> Bool
transitivelyFlexBoundTo constraint sourceRoot = go IntSet.empty
  where
    go visited source
        | source == sourceRoot = True
        | IntSet.member (getNodeId source) visited = False
        | otherwise =
            case Binding.lookupBindParent constraint (typeRef source) of
                Just (TypeRef parent, BindFlex) ->
                    go (IntSet.insert (getNodeId source) visited) parent
                _ -> False

hasStandaloneRaiseAuthority :: Constraint p -> NodeId -> NodeId -> Bool
hasStandaloneRaiseAuthority constraint sourceRoot source =
    source /= sourceRoot
        && transitivelyFlexBoundTo constraint sourceRoot source

initEdgeUnifyState
    :: [(NodeId, NodeId)]
    -> InteriorSet
    -> NodeId
    -> PendingWeakenOwner
    -> PresolutionM p EdgeUnifyState
initEdgeUnifyState =
    initEdgeUnifyStateWithStatsAndCopyMap
        False
        mempty
        IntSet.empty
        Nothing

initEdgeUnifyStateWithCopyMap
    :: CopyMap
    -> IntSet.IntSet
    -> NodeId
    -> EdgeSourceInterior
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> [(NodeId, NodeId)]
    -> InteriorSet
    -> NodeId
    -> PendingWeakenOwner
    -> PresolutionM p EdgeUnifyState
initEdgeUnifyStateWithCopyMap copyMap sourceNodeKeys sourceRoot sourceInterior lockedSourceNodes sourceRaiseAuthorityNodes =
    initEdgeUnifyStateWithStatsAndCopyMap
        False
        copyMap
        sourceNodeKeys
        (Just (sourceRoot, sourceInterior, lockedSourceNodes, sourceRaiseAuthorityNodes))

initEdgeUnifyStateWithStats
    :: Bool
    -> [(NodeId, NodeId)]
    -> InteriorSet
    -> NodeId
    -> PendingWeakenOwner
    -> PresolutionM p EdgeUnifyState
initEdgeUnifyStateWithStats collectStats binderArgs interior edgeRoot pendingOwner = do
    initEdgeUnifyStateWithStatsAndCopyMap
        collectStats
        mempty
        IntSet.empty
        Nothing
        binderArgs
        interior
        edgeRoot
        pendingOwner

initEdgeUnifyStateWithStatsAndCopyMap
    :: Bool
    -> CopyMap
    -> IntSet.IntSet
    -> Maybe (NodeId, EdgeSourceInterior, IntSet.IntSet, IntSet.IntSet)
    -> [(NodeId, NodeId)]
    -> InteriorSet
    -> NodeId
    -> PendingWeakenOwner
    -> PresolutionM p EdgeUnifyState
initEdgeUnifyStateWithStatsAndCopyMap collectStats copyMap sourceNodeKeys mbSourceDomain binderArgs interior edgeRoot pendingOwner = do
    inheritedPendingWeakens <- gets psPendingWeakens
    uf <- gets psUnionFind
    let interiorRootEntries = [(i, UnionFind.frWith uf (NodeId i)) | i <- IntSet.toList interior]
    let interiorRoots =
            InteriorNodes (IntSet.fromList [getNodeId r | (_i, r) <- interiorRootEntries])
    let binderRootEntries = [(bv, UnionFind.frWith uf arg) | (bv, arg) <- binderArgs]
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
    let effectiveSourceNodeKeys =
            if IntSet.null sourceNodeKeys
                then
                    IntSet.fromList
                        [ getNodeId (tnId node)
                        | node <- NodeAccess.allNodes constraint
                        ]
                else sourceNodeKeys
    let originallyBoundBinders =
            IntSet.fromList
                [ getNodeId binder
                | (binder, _meta) <- binderArgs
                , Just TyVar{tnBound = Just _} <- [NodeAccess.lookupNode constraint binder]
                ]
    let frozenSourceInterior =
            case mbSourceDomain of
                Just (_sourceRoot, sourceInterior, _lockedSourceNodes, _sourceRaiseAuthorityNodes) -> sourceInterior
                Nothing -> EdgeSourceInterior (InteriorNodes interior)
        EdgeSourceInterior (InteriorNodes frozenSourceKeys) =
            frozenSourceInterior
        lockedSourceNodes =
            case mbSourceDomain of
                Just (_sourceRoot, _sourceInterior, lockedNodes, _sourceRaiseAuthorityNodes) -> lockedNodes
                Nothing ->
                    IntSet.filter
                        ( \sourceKey ->
                            Binding.nodeKind constraint (typeRef (NodeId sourceKey))
                                == Right Binding.NodeLocked
                        )
                        frozenSourceKeys
        sourceRootForAuthority =
            case mbSourceDomain of
                Just (sourceRoot, _sourceInterior, _lockedSourceNodes, _sourceRaiseAuthorityNodes) -> sourceRoot
                Nothing -> edgeRoot
        sourceRaiseAuthorityNodes =
            case mbSourceDomain of
                Just (_sourceRoot, _sourceInterior, _lockedSourceNodes, authorityNodes) -> authorityNodes
                Nothing ->
                    IntSet.filter
                        (hasStandaloneRaiseAuthority constraint sourceRootForAuthority . NodeId)
                        frozenSourceKeys
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
                then Nothing
                else Just (Order.orderKeysFromConstraintWith id constraint interiorRoot Nothing)
    pure EdgeUnifyState
        { eusInteriorRoots = interiorRoots
        , eusBindersByRoot = bindersByRoot
        , eusInteriorByRoot = interiorByRoot
        , eusSourceInterior = frozenSourceInterior
        , eusLockedSourceNodes = lockedSourceNodes
        , eusSourceRaiseAuthorityNodes = sourceRaiseAuthorityNodes
        , eusSourceEdgeRoot =
            case mbSourceDomain of
                Just (sourceRoot, _sourceInterior, _lockedSourceNodes, _sourceRaiseAuthorityNodes) -> sourceRoot
                Nothing -> edgeRoot
        , eusEdgeRoot = edgeRoot
        , eusTerminalRootTransition = False
        , eusRootRaiseMergeRecorded = False
        , eusTerminalTransitionSources = IntSet.empty
        , eusInheritedPendingWeakens = inheritedPendingWeakens
        , eusEliminatedBinders = IntSet.empty
        , eusBinderMeta = binderMetaMap
        , eusOriginallyBoundBinders = originallyBoundBinders
        , eusScheduledWeakenMetas = IntSet.empty
        , eusBinderMetaRoots = binderMetaRoots
        , eusOrderKeys = keys
        , eusPendingWeakenOwner = pendingOwner
        , eusOps = []
        , eusCopyMap = copyMap
        , eusCopySourcesByRawDestination =
            IntMap.fromListWith
                IntSet.union
                [ ( getNodeId destination
                  , IntSet.singleton sourceKey
                  )
                | (sourceKey, destination) <- IntMap.toList (getCopyMapping copyMap)
                ]
        , eusSourceNodeKeys = effectiveSourceNodeKeys
        , eusRootCache = IntMap.empty
        , eusStructurePairs = IntMap.empty
        , eusCollectStats = collectStats
        , eusStats = emptyEdgeUnifyStats
        , eusRootCacheVersion = 0
        , eusRootCacheGen = 0
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
                    clearEdgeUnifyRootCache
                    pure True

unifyWithLockedFallback :: Maybe NodeId -> NodeId -> NodeId -> EdgeUnifyM p [NodeId]
unifyWithLockedFallback prefer left right = do
    clearEdgeUnifyRootCache
    raiseTrace <-
        unifyAcyclicRawWithRaiseTracePreferM prefer left right
            `catchError` handleLocked
    clearEdgeUnifyRootCache
    pure raiseTrace
  where
    retryAfterFlush lockedErr = do
        recovered <- flushInheritedPendingWeakensOnce
        if recovered
            then
                clearEdgeUnifyRootCache >>
                unifyAcyclicRawWithRaiseTracePreferM prefer left right
            else throwPresolutionErrorM lockedErr

    trySwap =
        clearEdgeUnifyRootCache >>
        unifyAcyclicRawWithRaiseTracePreferM prefer right left
            `catchError` \swapErr ->
                case swapErr of
                    BindingTreeError OperationOnLockedNode{} ->
                        clearEdgeUnifyRootCache >> retryAfterFlush swapErr
                    _ -> throwPresolutionErrorM swapErr

    handleLocked err =
        case err of
            BindingTreeError OperationOnLockedNode{} ->
                clearEdgeUnifyRootCache >> trySwap
            _ -> throwPresolutionErrorM err

recordEliminate :: NodeId -> EdgeUnifyM p ()
recordEliminate bv = do
    binderMeta <- requireBinderMeta bv
    dropVarBindM binderMeta
    markEliminated bv

markEliminated :: NodeId -> EdgeUnifyM p ()
markEliminated bv =
    modify' $ \st ->
        st { eusEliminatedBinders = IntSet.insert (getNodeId bv) (eusEliminatedBinders st) }

requireBinderMeta :: NodeId -> EdgeUnifyM p NodeId
requireBinderMeta binder = do
    binderMeta <- gets eusBinderMeta
    case IntMap.lookup (getNodeId binder) binderMeta of
        Just meta -> pure meta
        Nothing ->
            throwPresolutionErrorM
                (InternalError ("requireBinderMeta: missing copy for binder " ++ show binder))

isEliminated :: NodeId -> EdgeUnifyM p Bool
isEliminated bv = gets (IntSet.member (getNodeId bv) . eusEliminatedBinders)

-- | Whether this UF root belongs to an originally-unbounded copied binder
-- whose witness schedules @Weaken@.
--
-- Such a binder already carries its instantiation argument as a grafted lower
-- bound.  Its relation to an exterior variable must therefore be preserved as
-- a bound relation, not collapsed into UF equality.
isScheduledUnboundedBinderMetaRoot :: NodeId -> EdgeUnifyM p Bool
isScheduledUnboundedBinderMetaRoot root = do
    st <- get
    matches <-
        mapM
            ( \(binderKey, meta) -> do
                metaRoot <- findRootM meta
                pure
                    ( metaRoot == root
                        && IntSet.notMember binderKey (eusOriginallyBoundBinders st)
                        && IntSet.member (getNodeId meta) (eusScheduledWeakenMetas st)
                    )
            )
            (IntMap.toList (eusBinderMeta st))
    pure (or matches)

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
        sourceInterior <- gets (getEdgeSourceInterior . eusSourceInterior)
        -- Suppress an operation already constructed by an earlier authority,
        -- but preserve multiplicity inside this raise trace: each repeated
        -- node denotes one paper Raise step.
        preexistingOperatedSources <-
            gets $
                IntSet.fromList
                    . mapMaybe (operatedSource . edgeWitnessInstanceOp)
                    . eusOps
        forM_ (reverse candidates) $ \nid -> do
            isLocked <- lift $ checkNodeLockedInSnapshot snapshot nid
            when (not isLocked) $ do
                mbSource <-
                    sourceWitnessNodeIgnoringAmbiguity
                        "binding-parent Raise trace"
                        nid
                forM_ mbSource $ \source ->
                    when (memberInterior source sourceInterior) $ do
                        sourceLocked <-
                            gets
                                ( IntSet.member (getNodeId source)
                                    . eusLockedSourceNodes
                                )
                        sourceHasRaiseAuthority <-
                            gets
                                ( IntSet.member (getNodeId source)
                                    . eusSourceRaiseAuthorityNodes
                                )
                        let alreadyConstructed =
                                IntSet.member
                                    (getNodeId source)
                                    preexistingOperatedSources
                        when
                            ( not sourceLocked
                                && sourceHasRaiseAuthority
                                && not alreadyConstructed
                            ) $
                            recordInstanceOp (SourceEdgeWitnessOp (OpRaise source))
  where
    operatedSource op =
        case op of
            OpRaise operated -> Just (getNodeId operated)
            OpRaiseMerge operated _ -> Just (getNodeId operated)
            OpMerge operated _ -> Just (getNodeId operated)
            _ -> Nothing

-- | Record construction-time bound-frontier Raises against the complete
-- edge-local interior.  Unlike a UF merge, a bound update has no pair of root
-- buckets whose interiors can be selected more narrowly.
recordCurrentInteriorRaises :: [NodeId] -> EdgeUnifyM p ()
recordCurrentInteriorRaises raiseTrace = do
    interiorByRoot <- gets eusInteriorByRoot
    let interior = mconcat (IntMap.elems interiorByRoot)
        outside = filter (not . (`memberInterior` interior)) raiseTrace
    when (not (null outside)) $ do
        edgeRoot <- gets eusEdgeRoot
        throwPresolutionErrorM (EdgeBoundRaiseOutsideInterior edgeRoot outside)
    recordRaisesFromTrace interior raiseTrace

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
checkNodeLockedInSnapshot snapshot nid = do
    kind <- bindingSnapshotNodeKind snapshot (typeRef nid)
    pure (kind == Binding.NodeLocked)

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
