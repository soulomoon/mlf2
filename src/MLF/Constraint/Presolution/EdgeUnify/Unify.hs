{- |
Module      : MLF.Constraint.Presolution.EdgeUnify.Unify
Description : Edge-local unification logic for presolution instantiation edges
-}
module MLF.Constraint.Presolution.EdgeUnify.Unify (
    TerminalRootTransition(..),
    classifyTerminalRootTransition,
    sourceWitnessNode,
    unifyAcyclicEdge,
    constructNondegenerateIdentityTerminalRootAuthority,
    constructUncopiedTerminalRootAuthority,
    recordExpansionWrapperResult,
    unifyStructureEdge,
    unifyQuotientTerminalStructureEdge,
    unifyTerminalStructureEdge,
    unifyUncopiedTerminalStructureEdge
) where

import Control.Monad (foldM, forM_, void, when)
import Control.Monad.Reader (ask)
import Control.Monad.State (gets, modify')
import qualified Data.List.NonEmpty as NE
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Adjustment as BindingAdjustment
import qualified MLF.Binding.GraphOps as BindingGraphOps
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base (
    EdgeSourceInterior(..),
    lookupCopy,
    memberInterior,
    modifyConstraint,
    PresolutionError(..),
    psConstraint,
    recordExpansionResult
    )
import qualified MLF.Constraint.Presolution.Ops as Ops
import MLF.Constraint.Presolution.EdgeUnify.State (
    EdgeUnifyM,
    EdgeUnifyStats(..),
    EdgeUnifyState(..),
    MonadEdgeUnify(..),
    clearEdgeUnifyStructureCache,
    compareEdgeBinderOrder,
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
    recordRaisesFromTrace,
    sourceWitnessNode,
    sourceWitnessNodeFor,
    sourceWitnessNodeIgnoringAmbiguity,
    structurePairSeenOrInsert,
    unifyWithLockedFallback
    )
import MLF.Constraint.Presolution.StateAccess
    ( getConstraintAndCanonical
    )
import MLF.Constraint.Presolution.Witness (EdgeWitnessOp(..))
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import qualified MLF.Util.Order as Order
import MLF.Util.Trace (traceBindingM)

recordOp :: InstanceOp -> EdgeUnifyM p ()
recordOp = recordInstanceOp . SourceEdgeWitnessOp

recordFlexibleTerminalOp :: InstanceOp -> EdgeUnifyM p ()
recordFlexibleTerminalOp =
    recordInstanceOp . FlexibleTerminalSourceEdgeWitnessOp

data TerminalTransitionAuthority
    = TerminalInteriorRaiseMerge
        TerminalInteriorTransition
        NodeId
        NodeId
        NodeId
    | TerminalRigidInteriorIdentity NodeId
    | TerminalRootAuthority
        TerminalRootTransition
        NodeId
        NodeId
        NodeId

data TerminalInteriorTransition
    = PreserveFlexibleInterior
    | WeakenFlexibleInterior

-- | The complete Figure 15.3.4 transition selected from the two root binding
-- flags before terminal unification mutates either operand.
data TerminalRootTransition
    = RootRaiseMerge
    | RootWeakenRaiseMerge
    | RigidRootIdentity
    deriving (Eq, Show)

-- | Whether the terminal source root may construct Figure 15.3.4's exterior
-- Gamma entry.  Identity expansions whose source and target have the same gen
-- owner are quotient work only; copied or explicitly outer destinations use
-- the construction lane.  Keeping the distinction in a sum prevents the
-- children-first traversal from recovering root authority after its caller has
-- already rejected that authority from the frozen binding tree.
data TerminalRootAuthorityMode
    = QuotientTerminalRoot
    | ConstructTerminalRoot

-- | Evidence used to classify an uncopied source root.  Ordinary callers
-- read the live binding flag.  A nondegenerate identity expansion has stronger
-- construction-time evidence from Section 10.1.2's binding reset: its
-- expansion root is flexible regardless of the source flag, even if an
-- earlier edge has since rigidified that live node.
data UncopiedTerminalRootSource
    = LiveUncopiedTerminalRoot
    | NondegenerateIdentityTerminalRoot

-- | Classify a terminal-root transition only from complete binding evidence.
-- A rigid operated root makes the semantic transition an identity regardless
-- of the target flag.  A flexible root, however, cannot construct authority
-- when the target binding edge is absent.
classifyTerminalRootTransition
    :: Maybe BindFlag
    -> Maybe BindFlag
    -> Maybe TerminalRootTransition
classifyTerminalRootTransition mbSourceFlag mbTargetFlag =
    case mbSourceFlag of
        Nothing -> Nothing
        Just BindRigid -> Just RigidRootIdentity
        Just BindFlex ->
            case mbTargetFlag of
                Nothing -> Nothing
                Just BindFlex -> Just RootRaiseMerge
                Just BindRigid -> Just RootWeakenRaiseMerge

recordTerminalRootTransition
    :: TerminalRootTransition
    -> NodeId
    -> NodeId
    -> EdgeUnifyM p ()
recordTerminalRootTransition transition operatedSource targetSource =
    case transition of
        RootRaiseMerge -> do
            recordOp (OpRaise operatedSource)
            recordOp (OpMerge operatedSource targetSource)
        RootWeakenRaiseMerge -> do
            recordOp (OpWeaken operatedSource)
            recordOp (OpRaise operatedSource)
            recordOp (OpMerge operatedSource targetSource)
        RigidRootIdentity -> pure ()

-- | Construct the one terminal-root transition admitted by Figure 15.3.4.
-- For a flexible root, the expansion root leaves its frozen source interior
-- and acquires an exterior lower bound; a rigid root instead constructs the
-- identity transition.  Both operands are captured before the bound update,
-- while the copy map can still identify their exact source-domain nodes.
--
-- Ordinary bound harmonization must not be promoted to witness authority.  In
-- particular, destination equality, a solved lower bound, or a non-root
-- interior node is insufficient evidence for this operation.
recordRootTransitionToExterior :: NodeId -> NodeId -> EdgeUnifyM p Bool
recordRootTransitionToExterior operatedRoot exteriorRoot = do
    st <- getEdgeUnifyState
    if eusRootRaiseMergeRecorded st || not (eusTerminalRootTransition st)
        then pure False
        else do
            edgeRootClass <- findRootM (eusEdgeRoot st)
            operatedClass <- findRootM operatedRoot
            exteriorClass <- findRootM exteriorRoot
            mbExteriorSource <-
                sourceWitnessNodeIgnoringAmbiguity
                    "terminal root transition exterior"
                    exteriorClass
            constraint <- liftPresolution (fst <$> getConstraintAndCanonical)
            let sourceRoot = eusSourceEdgeRoot st
                sourceInterior = getEdgeSourceInterior (eusSourceInterior st)
                sourceRootIsLocked =
                    IntSet.member
                        (getNodeId sourceRoot)
                        (eusLockedSourceNodes st)
                sourceRootFlag =
                    snd <$> Binding.lookupBindParent constraint (typeRef sourceRoot)
                exteriorFlag =
                    snd <$> Binding.lookupBindParent constraint (typeRef exteriorClass)
            sourceRootCopyIsOperated <-
                case lookupCopy sourceRoot (eusCopyMap st) of
                    Nothing -> pure False
                    Just destination -> (== operatedClass) <$> findRootM destination
            case mbExteriorSource of
                Just exteriorSource
                    | operatedClass == edgeRootClass
                    , sourceRootCopyIsOperated
                    , exteriorClass /= edgeRootClass
                    , memberInterior sourceRoot sourceInterior
                    , not sourceRootIsLocked
                    , not (memberInterior exteriorSource sourceInterior)
                    , sourceRoot /= exteriorSource
                    , Just transition <-
                        classifyTerminalRootTransition
                            sourceRootFlag
                            exteriorFlag -> do
                        recordTerminalRootTransition
                            transition
                            sourceRoot
                            exteriorSource
                        modifyEdgeUnifyState $ \st' ->
                            st' { eusRootRaiseMergeRecorded = True }
                        eliminateSourceRootBinderIfPresent sourceRoot
                        pure True
                _ -> pure False

recordRootTransitionBeforeUnion :: NodeId -> NodeId -> EdgeUnifyM p Bool
recordRootTransitionBeforeUnion left right = do
    alreadyRecorded <- gets eusRootRaiseMergeRecorded
    if alreadyRecorded
        then pure True
        else do
            recorded <- recordRootTransitionToExterior left right
            if recorded
                then pure True
                else recordRootTransitionToExterior right left

-- | Capture the uncopied terminal root escape from the frozen source-domain
-- identities.  A previous edge may already have quotiented the live UF
-- classes before this edge executes; the original source root and terminal
-- target are still distinct construction operands.  The caller admits this
-- lane only when the binding tree proves a flexible source root and places
-- the target on an outer gen.  The target flag then distinguishes RaiseMerge
-- from Weaken-RaiseMerge; same-owner aliases remain quotient operations.
recordUncopiedTerminalRootTransition
    :: UncopiedTerminalRootSource
    -> NodeId
    -> EdgeUnifyM p Bool
recordUncopiedTerminalRootTransition sourceEvidence terminalTarget = do
    st <- getEdgeUnifyState
    if eusRootRaiseMergeRecorded st
        then pure False
        else do
            constraint <- liftPresolution (fst <$> getConstraintAndCanonical)
            let sourceRoot = eusSourceEdgeRoot st
                sourceInterior = getEdgeSourceInterior (eusSourceInterior st)
                sourceRootIsLocked =
                    IntSet.member
                        (getNodeId sourceRoot)
                        (eusLockedSourceNodes st)
                sourceRootFlag =
                    snd <$> Binding.lookupBindParent constraint (typeRef sourceRoot)
            mbExteriorSource <-
                sourceWitnessNodeIgnoringAmbiguity
                    "uncopied terminal root target"
                    terminalTarget
            case mbExteriorSource of
                Just exteriorSource
                    | memberInterior sourceRoot sourceInterior
                    , not sourceRootIsLocked
                    , not (memberInterior exteriorSource sourceInterior)
                    , sourceRoot /= exteriorSource -> do
                        let exteriorFlag =
                                snd
                                    <$> Binding.lookupBindParent
                                        constraint
                                        (typeRef exteriorSource)
                        case
                            classifyTerminalRootTransition
                                ( case sourceEvidence of
                                    LiveUncopiedTerminalRoot -> sourceRootFlag
                                    NondegenerateIdentityTerminalRoot -> Just BindFlex
                                )
                                exteriorFlag of
                            Nothing -> pure False
                            Just transition -> do
                                recordTerminalRootTransition
                                    transition
                                    sourceRoot
                                    exteriorSource
                                modifyEdgeUnifyState $ \st' ->
                                    st' { eusRootRaiseMergeRecorded = True }
                                eliminateSourceRootBinderIfPresent sourceRoot
                                pure True
                _ -> pure False

-- | Construct the edge-level terminal authority for an identity expansion.
-- Unlike copied expansions, ExpIdentity has no destination root on which the
-- children-first matcher can rediscover Figure 15.3.4's final transition.
-- The caller must therefore select this once from the whole instantiation
-- edge, before executing any component equalities chosen by shape matching.
constructUncopiedTerminalRootAuthority :: NodeId -> EdgeUnifyM p ()
constructUncopiedTerminalRootAuthority =
    void . recordUncopiedTerminalRootTransition LiveUncopiedTerminalRoot

-- | Construct Figure 15.3.4 authority for a nondegenerate identity expansion.
-- The frozen source domain plus binding reset, rather than a later edge's
-- mutation of the live root flag, proves that this expansion root is flexible.
constructNondegenerateIdentityTerminalRootAuthority
    :: NodeId
    -> EdgeUnifyM p ()
constructNondegenerateIdentityTerminalRootAuthority =
    void
        . recordUncopiedTerminalRootTransition
            NondegenerateIdentityTerminalRoot

eliminateSourceRootBinderIfPresent :: NodeId -> EdgeUnifyM p ()
eliminateSourceRootBinderIfPresent sourceRoot = do
    binderMeta <- gets eusBinderMeta
    when (IntMap.member (getNodeId sourceRoot) binderMeta) $
        recordEliminate sourceRoot

recordChildEdges :: Int -> EdgeUnifyM p ()
recordChildEdges count =
    recordEdgeUnifyStatN (fromIntegral count) $ \n stats ->
        stats { eusUnifyStructureChildEdges = eusUnifyStructureChildEdges stats + n }

recordCanonicalNodeLookups :: Int -> EdgeUnifyM p ()
recordCanonicalNodeLookups count =
    recordEdgeUnifyStatN (fromIntegral count) $ \n stats ->
        stats { eusCanonicalNodeLookups = eusCanonicalNodeLookups stats + n }

getCanonicalNodeEdge :: NodeId -> EdgeUnifyM p TyNode
getCanonicalNodeEdge nid = do
    root <- findRootM nid
    liftPresolution $ Ops.getNode root

requireBinderMeta :: NodeId -> EdgeUnifyM p NodeId
requireBinderMeta binder = do
    binderMeta <- getBinderMeta
    case IntMap.lookup (getNodeId binder) binderMeta of
        Just meta -> pure meta
        Nothing ->
            throwPresolutionErrorM
                (InternalError ("requireBinderMeta: missing copy for binder " ++ show binder))

compareBinderIdsByPrec :: Int -> Int -> EdgeUnifyM p Ordering
compareBinderIdsByPrec bid1 bid2 =
    compareEdgeBinderOrder (NodeId bid1) (NodeId bid2)

pickRepBinderId :: IntSet.IntSet -> EdgeUnifyM p Int
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

recordMergesIntoRep :: IntSet.IntSet -> EdgeUnifyM p ()
recordMergesIntoRep bs
    | IntSet.size bs <= 1 = pure ()
    | otherwise = do
        eliminated <- gets eusEliminatedBinders
        let live = IntSet.filter (\bid -> not (IntSet.member bid eliminated)) bs
        repId <- pickRepBinderId (if IntSet.null live then bs else live)
        let rep = NodeId repId
            others = filter (/= repId) (IntSet.toList bs)
        othersSorted <- Order.sortByM (\a b -> compareBinderIdsByPrec b a) others
        forM_ othersSorted $ \bid -> do
            let b = NodeId bid
            already <- isEliminated b
            when (not already) $ do
                bMeta <- requireBinderMeta b
                repMeta <- requireBinderMeta rep
                recordOp (OpMerge b rep)
                setVarBoundM bMeta (Just repMeta)
                recordEliminate b
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

unifyAcyclicEdge :: NodeId -> NodeId -> EdgeUnifyM p ()
unifyAcyclicEdge n1 n2 = do
    recordEdgeUnifyStat $ \stats ->
        stats { eusUnifyAcyclicCalls = eusUnifyAcyclicCalls stats + 1 }
    root1 <- findRootM n1
    root2 <- findRootM n2
    when (root1 /= root2) $ do
        node1 <- liftPresolution $ Ops.getNode root1
        node2 <- liftPresolution $ Ops.getNode root2
        unifyAcyclicEdgeCore node1 node2 root1 root2

-- | Record the administrative replacement of a 'TyExp' occurrence wrapper.
-- This intentionally does not touch semantic UF or edge-local witness state:
-- χe contains only the result/target unification, not a second wrapper merge.
recordExpansionWrapperResult :: NodeId -> NodeId -> EdgeUnifyM p ()
recordExpansionWrapperResult wrapper result =
    liftPresolution (recordExpansionResult wrapper result)

-- | Internal body of 'unifyAcyclicEdge', accepting pre-fetched 'TyNode' values
-- to avoid redundant 'Ops.getNode' lookups when the caller (e.g.
-- 'unifyStructureRoots') has already fetched them.
unifyAcyclicEdgeCore :: TyNode -> TyNode -> NodeId -> NodeId -> EdgeUnifyM p ()
unifyAcyclicEdgeCore node1 node2 root1 root2 = do
    rootTransitionRecorded <- recordRootTransitionBeforeUnion root1 root2
    st0 <- getEdgeUnifyState
    let r1 = getNodeId root1
        r2 = getNodeId root2
        inInt1 = memberInterior root1 (eusInteriorRoots st0)
        inInt2 = memberInterior root2 (eusInteriorRoots st0)
        bs1 = IntMap.findWithDefault IntSet.empty r1 (eusBindersByRoot st0)
        bs2 = IntMap.findWithDefault IntSet.empty r2 (eusBindersByRoot st0)
        bs = IntSet.union bs1 bs2

    pendingRaiseMerge <-
        if rootTransitionRecorded
            then pure Nothing
            else
                case (IntSet.null bs1, IntSet.null bs2) of
                    (False, True) | inInt1 && not inInt2 ->
                        prepareRaiseMerge node2 root2 bs
                    (True, False) | inInt2 && not inInt1 ->
                        prepareRaiseMerge node1 root1 bs
                    _ -> pure Nothing

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
            metaRoots' = mergeBinderMetaRoots r1 r2 repId (eusBinderMetaRoots st)
        in st
            { eusInteriorRoots = roots'
            , eusBindersByRoot = binders'
            , eusInteriorByRoot = interior'
            , eusBinderMetaRoots = metaRoots'
            }

    recordMergesIntoRep bs

    case pendingRaiseMerge of
        Nothing -> pure ()
        Just (repBinder, extRoot) -> do
            already <- isEliminated repBinder
            when (not already) $ do
                -- The UF merge has now executed the structural part.  Record
                -- the source-domain witness chosen from the pre-merge roots;
                -- looking up the meta here would collapse both sides to the
                -- same representative and lose the paper operation.
                recordOp (OpRaise repBinder)
                mbExtSource <- sourceWitnessNodeFor "pending binder RaiseMerge exterior" extRoot
                case mbExtSource of
                    Just extSource -> recordOp (OpMerge repBinder extSource)
                    Nothing ->
                        recordInstanceOp
                            (SourceDestinationEdgeWitnessMerge repBinder extRoot)
                recordEliminate repBinder

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
-- Queries use the edge-local graph state captured before destructive union,
-- then validate against the current canonical roots.
--
-- The @extNode@ parameter is the pre-fetched 'TyNode' for @ext@, threaded
-- from the caller to avoid a redundant 'Ops.getNode' lookup.
prepareRaiseMerge :: TyNode -> NodeId -> IntSet.IntSet -> EdgeUnifyM p (Maybe (NodeId, NodeId))
prepareRaiseMerge extNode extRoot binders = do
    eliminated <- gets eusEliminatedBinders
    let live = IntSet.filter (\bid -> not (IntSet.member bid eliminated)) binders
    if IntSet.null live
        then pure Nothing
        else do
            repBinder <- NodeId <$> pickRepBinderId live
            st <- getEdgeUnifyState
            let sourceKey = getNodeId repBinder
                hasRaiseAuthority =
                    IntSet.member sourceKey (eusSourceRaiseAuthorityNodes st)
                sourceIsLocked =
                    IntSet.member sourceKey (eusLockedSourceNodes st)
            should <-
                if hasRaiseAuthority && not sourceIsLocked
                    then shouldRecordRaiseMergeBefore extNode extRoot repBinder
                    else pure False
            pure $ if should then Just (repBinder, extRoot) else Nothing

shouldRecordRaiseMergeBefore :: TyNode -> NodeId -> NodeId -> EdgeUnifyM p Bool
shouldRecordRaiseMergeBefore extNode extRoot binder = do
    already <- isEliminated binder
    if already
        then pure False
        else do
            edgeRoot <- gets eusEdgeRoot
            case extNode of
                TyVar {tnBound = Nothing} -> do
                    -- Thread the pre-computed binder root to avoid the
                    -- redundant findRootM inside lookupVarBoundM.
                    binderMeta <- requireBinderMeta binder
                    binderRoot <- findRootM binderMeta
                    recordEdgeUnifyStat $ \stats ->
                        stats { eusLookupVarBoundCalls = eusLookupVarBoundCalls stats + 1 }
                    c <- liftPresolution (gets psConstraint)
                    let ext = tnId extNode
                    case NodeAccess.lookupVarBound c binderRoot of
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
                -- Both sides already carry lower bounds.  Their bounds are
                -- matched structurally after the variable union; this is not
                -- the bounded-binder escape represented by RaiseMerge.
                TyVar {tnBound = Just _} -> pure False
                _ -> pure False

debugEdgeUnify :: String -> EdgeUnifyM p ()
debugEdgeUnify msg = do
    cfg <- ask
    traceBindingM cfg msg

unifyStructureEdge :: NodeId -> NodeId -> EdgeUnifyM p ()
unifyStructureEdge n1 n2 = do
    when (n1 /= n2) $ do
        root1 <- findRootM n1
        root2 <- findRootM n2
        if root1 == root2
            then
                recordEdgeUnifyStat $ \stats ->
                    stats { eusUnifyStructureSameRoot = eusUnifyStructureSameRoot stats + 1 }
            else do
                seenRoots <- structurePairSeenOrInsert root1 root2
                when (not seenRoots) $ do
                    recordEdgeUnifyStat $ \stats ->
                        stats { eusUnifyStructureCalls = eusUnifyStructureCalls stats + 1 }
                    unifyStructureRoots root1 root2

-- | Execute the one terminal expansion-root/edge-target unification.  Root
-- RaiseMerge authority may be constructed only inside this scope; recursive
-- frontier and child unifications are intermediate χe work and cannot emit
-- the terminal abstraction from Figure 15.3.4.
unifyTerminalStructureEdge :: NodeId -> NodeId -> EdgeUnifyM p ()
unifyTerminalStructureEdge =
    unifyTerminalStructureEdgeWith ConstructTerminalRoot

-- | Quotient an identity-expansion root with a same-owner target without
-- manufacturing an exterior Gamma entry.  Interior Raise/Merge work remains
-- enabled; only the terminal source root is construction-inert.
unifyQuotientTerminalStructureEdge :: NodeId -> NodeId -> EdgeUnifyM p ()
unifyQuotientTerminalStructureEdge =
    unifyTerminalStructureEdgeWith QuotientTerminalRoot

-- | Identity expansions do not have a copied root.  An ordinary edge can
-- still carry the terminal root abstraction when its frozen source root and
-- target are distinct.  Administrative let/annotation edges must use
-- 'unifyTerminalStructureEdge' instead.
unifyUncopiedTerminalStructureEdge :: NodeId -> NodeId -> EdgeUnifyM p ()
unifyUncopiedTerminalStructureEdge left right = do
    constructUncopiedTerminalRootAuthority right
    unifyTerminalStructureEdgeWith ConstructTerminalRoot left right

unifyTerminalStructureEdgeWith
    :: TerminalRootAuthorityMode
    -> NodeId
    -> NodeId
    -> EdgeUnifyM p ()
unifyTerminalStructureEdgeWith authorityMode left right = do
    unifyTerminalStructureChildrenFirst authorityMode left right

-- | Solve the terminal congruence from the leaves towards the roots.
--
-- Definition 11.5.3 constructs the normalized derivation in that order:
-- corresponding nodes leave the source interior first (step 3), and only then
-- may the expansion root be merged with the rigid destination (step 4).  The
-- ordinary structural unifier is root-first; using it here can make an
-- otherwise green copied child red before its own RaiseMerge is executed.
unifyTerminalStructureChildrenFirst
    :: TerminalRootAuthorityMode
    -> NodeId
    -> NodeId
    -> EdgeUnifyM p ()
unifyTerminalStructureChildrenFirst authorityMode left right = do
    leftRoot <- findRootM left
    rightRoot <- findRootM right
    when (leftRoot /= rightRoot) $ do
        seenRoots <- structurePairSeenOrInsert leftRoot rightRoot
        when (not seenRoots) $ do
            leftNode <- getCanonicalNodeEdge leftRoot
            rightNode <- getCanonicalNodeEdge rightRoot
            transitionAuthority <-
                terminalTransitionAuthority authorityMode leftRoot rightRoot
            lockedStructuralPair <-
                structuralPairContainsLockedNode leftRoot rightRoot
            case (leftNode, rightNode) of
                (TyVar {tnBound = Just leftBound}, TyVar {tnBound = Just rightBound}) -> do
                    leftBoundNode <- getCanonicalNodeEdge leftBound
                    rightBoundNode <- getCanonicalNodeEdge rightBound
                    case (leftBoundNode, rightBoundNode) of
                        -- Rigid forall bounds are red graph structure.  They
                        -- must be compared by the bounded-scheme matcher after
                        -- the owning variables are quotiented, never merged by
                        -- the terminal UF traversal itself.
                        (TyForall {}, TyForall {}) -> pure ()
                        _ ->
                            unifyTerminalStructureChildrenFirst authorityMode leftBound rightBound
                (TyVar {tnBound = Just leftBound}, rightNode')
                    | not (isTyVarNode rightNode') ->
                        unifyTerminalStructureChildrenFirst authorityMode leftBound rightRoot
                (leftNode', TyVar {tnBound = Just rightBound})
                    | not (isTyVarNode leftNode') ->
                        unifyTerminalStructureChildrenFirst authorityMode leftRoot rightBound
                (TyArrow {tnDom = leftDom, tnCod = leftCod}, TyArrow {tnDom = rightDom, tnCod = rightCod}) -> do
                    unifyTerminalStructureChildrenFirst authorityMode leftDom rightDom
                    unifyTerminalStructureChildrenFirst authorityMode leftCod rightCod
                (TyForall {}, TyForall {})
                    | lockedStructuralPair -> pure ()
                (TyForall {tnBody = leftBody}, TyForall {tnBody = rightBody}) ->
                    unifyTerminalStructureChildrenFirst authorityMode leftBody rightBody
                (TyMu {}, TyMu {})
                    | lockedStructuralPair -> pure ()
                (TyMu {tnBody = leftBody}, TyMu {tnBody = rightBody}) ->
                    unifyTerminalStructureChildrenFirst authorityMode leftBody rightBody
                (TyCon {tnConIdentity = leftIdentity, tnArgs = leftArgs}, TyCon {tnConIdentity = rightIdentity, tnArgs = rightArgs})
                    | leftIdentity == rightIdentity
                    , NE.length leftArgs == NE.length rightArgs ->
                        mapM_
                            (uncurry (unifyTerminalStructureChildrenFirst authorityMode))
                            (zip (NE.toList leftArgs) (NE.toList rightArgs))
                (TyVarApp {tnVarHead = leftHead, tnArgs = leftArgs}, TyVarApp {tnVarHead = rightHead, tnArgs = rightArgs})
                    | NE.length leftArgs == NE.length rightArgs -> do
                        unifyTerminalStructureChildrenFirst authorityMode leftHead rightHead
                        mapM_
                            (uncurry (unifyTerminalStructureChildrenFirst authorityMode))
                            (zip (NE.toList leftArgs) (NE.toList rightArgs))
                _ -> pure ()
            leftRoot' <- findRootM leftRoot
            rightRoot' <- findRootM rightRoot
            -- Authority is frozen before descending.  Children may quotient
            -- the live roots, but they cannot erase the already-constructed
            -- source-domain operation.
            executeTerminalTransitionAuthority transitionAuthority
            if leftRoot' == rightRoot'
                then
                    recordEdgeUnifyStat $ \stats ->
                        stats {eusUnifyStructureSameRoot = eusUnifyStructureSameRoot stats + 1}
                else do
                    recordEdgeUnifyStat $ \stats ->
                        stats {eusUnifyStructureCalls = eusUnifyStructureCalls stats + 1}
                    modify' $ \st -> st {eusTerminalRootTransition = True}
                    -- Preserve the existing meta-root, scheduled-unbounded,
                    -- bound, interior-bucket, and statistics semantics.  Its
                    -- recursive child calls are now no-ops because the
                    -- terminal children were already quotiented above.
                    unifyStructureRoots leftRoot' rightRoot'
                    modify' $ \st -> st {eusTerminalRootTransition = False}
  where
    isTyVarNode node =
        case node of
            TyVar {} -> True
            _ -> False

-- | Capture the source-domain operands before child unification quotients the
-- live target class.  Step 3 of Definition 11.5.3 emits RaiseMerge for a
-- corresponding interior node.  Step 4 classifies the source root and
-- terminal destination together as RaiseMerge, Weaken-RaiseMerge, or rigid
-- identity before any child traversal mutates their live classes.
terminalTransitionAuthority
    :: TerminalRootAuthorityMode
    -> NodeId
    -> NodeId
    -> EdgeUnifyM p (Maybe TerminalTransitionAuthority)
terminalTransitionAuthority authorityMode leftRoot rightRoot = do
    st <- getEdgeUnifyState
    -- A terminal operation needs one exact frozen source on each side.  Live
    -- quotienting can place several distinct construction sources in the same
    -- destination class; that is absence of operation authority, not license
    -- to choose an interior-looking representative.  The public/source
    -- witness query remains strict so callers that require an operand still
    -- reject this ambiguity.
    leftSource <-
        sourceWitnessNodeIgnoringAmbiguity
            "terminal transition left"
            leftRoot
    rightSource <-
        sourceWitnessNodeIgnoringAmbiguity
            "terminal transition right"
            rightRoot
    constraint <- liftPresolution (fst <$> getConstraintAndCanonical)
    let sourceInterior = getEdgeSourceInterior (eusSourceInterior st)
        sourceRoot = eusSourceEdgeRoot st
        sourceIsLocked source =
            IntSet.member (getNodeId source) (eusLockedSourceNodes st)
        sourceHasRaiseAuthority source =
            IntSet.member
                (getNodeId source)
                (eusSourceRaiseAuthorityNodes st)
        classify operated operatedSource target targetSource
            | not (memberInterior operatedSource sourceInterior) = Nothing
            | memberInterior targetSource sourceInterior = Nothing
            | operatedSource == targetSource = Nothing
            | sourceIsLocked operatedSource = Nothing
            | operatedSource == sourceRoot =
                let sourceFlag =
                        snd
                            <$> Binding.lookupBindParent
                                constraint
                                (typeRef operatedSource)
                    targetFlag =
                        snd
                            <$> Binding.lookupBindParent
                                constraint
                                (typeRef target)
                in case
                    ( authorityMode
                    , classifyTerminalRootTransition sourceFlag targetFlag
                    ) of
                    (_, Just RigidRootIdentity) ->
                        Just
                            ( TerminalRootAuthority
                                RigidRootIdentity
                                operated
                                operatedSource
                                targetSource
                            )
                    (ConstructTerminalRoot, Just transition) ->
                        Just
                            ( TerminalRootAuthority
                                transition
                                operated
                                operatedSource
                                targetSource
                            )
                    _ -> Nothing
            | not (sourceHasRaiseAuthority operatedSource) = Nothing
            | otherwise =
                case Binding.nodeKind constraint (typeRef operatedSource) of
                    Right Binding.NodeRestricted ->
                        Just (TerminalRigidInteriorIdentity operatedSource)
                    _ ->
                        let transition =
                                case Binding.lookupBindParent constraint (typeRef target) of
                                    Just (_targetParent, BindRigid) -> WeakenFlexibleInterior
                                    _ -> PreserveFlexibleInterior
                        in Just
                            ( TerminalInteriorRaiseMerge
                                transition
                                operated
                                operatedSource
                                targetSource
                            )
    let result =
            case (leftSource, rightSource) of
                (Just leftSourceNode, Just rightSourceNode) ->
                    case classify leftRoot leftSourceNode rightRoot rightSourceNode of
                        Just authority -> Just authority
                        Nothing ->
                            classify rightRoot rightSourceNode leftRoot leftSourceNode
                _ -> Nothing
    case result of
        Just authority@(TerminalRootAuthority _ _ _ _)
            | eusRootRaiseMergeRecorded st -> pure Nothing
            | otherwise -> do
                -- Reserve the unique terminal-root authority before child
                -- traversal.  Otherwise a child that aliases the live root
                -- can manufacture an earlier RaiseMerge from mutable UF state.
                modify' $ \st' -> st' {eusRootRaiseMergeRecorded = True}
                pure (Just authority)
        _ -> pure result

executeTerminalTransitionAuthority
    :: Maybe TerminalTransitionAuthority
    -> EdgeUnifyM p ()
executeTerminalTransitionAuthority mbAuthority =
    case mbAuthority of
        Nothing -> pure ()
        Just (TerminalRigidInteriorIdentity operatedSource) ->
            void (claimTerminalTransitionSource operatedSource)
        Just (TerminalInteriorRaiseMerge transition operated operatedSource targetSource) -> do
            claimed <- claimTerminalTransitionSource operatedSource
            when claimed $ do
                case transition of
                    PreserveFlexibleInterior -> pure ()
                    WeakenFlexibleInterior -> do
                        constraint <- liftPresolution (fst <$> getConstraintAndCanonical)
                        case Binding.lookupBindParent constraint (typeRef operated) of
                            Just (_parent, BindRigid) -> pure ()
                            _ -> do
                                constraint' <-
                                    case BindingGraphOps.applyWeaken (TypeRefTag operated) constraint of
                                        Left err -> throwPresolutionErrorM (BindingTreeError err)
                                        Right (weakened, _op) -> pure weakened
                                liftPresolution $ modifyConstraint (const constraint')
                case transition of
                    PreserveFlexibleInterior -> do
                        -- The corresponding destination may become rigid only
                        -- after later graph work.  Preserve this construction
                        -- fact on both steps so witness finalization can add
                        -- the paper Weaken exactly when that happens.
                        recordFlexibleTerminalOp (OpRaise operatedSource)
                        recordFlexibleTerminalOp (OpMerge operatedSource targetSource)
                    -- Φ consumes this delayed form as one rigid-bound
                    -- application (Lemma 15.3.11).  The graph mutation has
                    -- already happened before the terminal merge; the
                    -- witness records its normalized delayed position.
                    WeakenFlexibleInterior -> do
                        recordOp (OpRaise operatedSource)
                        recordOp (OpMerge operatedSource targetSource)
                        recordOp (OpWeaken operatedSource)
        Just (TerminalRootAuthority transition operated operatedSource targetSource) -> do
            claimed <- claimTerminalTransitionSource operatedSource
            when claimed $ do
                case transition of
                    RootRaiseMerge -> pure ()
                    RigidRootIdentity -> pure ()
                    RootWeakenRaiseMerge -> do
                        -- Child-first unification runs after authority capture
                        -- and can rigidify the live copy before the terminal root
                        -- transition.  Re-read that physical edge so Weaken stays
                        -- idempotent.  The frozen transition still owns witness
                        -- authority even when the live graph is already rigid.
                        constraint <- liftPresolution (fst <$> getConstraintAndCanonical)
                        case Binding.lookupBindParent constraint (typeRef operated) of
                            Just (_parent, BindRigid) -> pure ()
                            _ -> do
                                constraint' <-
                                    case BindingGraphOps.applyWeaken (TypeRefTag operated) constraint of
                                        Left err -> throwPresolutionErrorM (BindingTreeError err)
                                        Right (weakened, _op) -> pure weakened
                                liftPresolution $ modifyConstraint (const constraint')
                recordTerminalRootTransition
                    transition
                    operatedSource
                    targetSource
                modify' $ \st -> st {eusRootRaiseMergeRecorded = True}

-- | Claim the one Figure 15.3.4 terminal transition owned by a frozen source
-- node.  A shared source child can occur more than once in a structural type;
-- children-first traversal may therefore capture several authorities before
-- the first one executes.  Claiming at execution time makes the later aliases
-- quotient-only work instead of constructing a second RaiseMerge for an
-- already-eliminated source node.
claimTerminalTransitionSource :: NodeId -> EdgeUnifyM p Bool
claimTerminalTransitionSource source = do
    claimed <- gets eusTerminalTransitionSources
    let key = getNodeId source
    if IntSet.member key claimed
        then pure False
        else do
            modify' $ \st ->
                st
                    { eusTerminalTransitionSources =
                        IntSet.insert key (eusTerminalTransitionSources st)
                    }
            pure True

-- | Red structure is immutable in the paper graph.  Congruence may inspect it
-- structurally, but must not merge it or construct Raise operations below its
-- rigid ancestor.
structuralPairContainsLockedNode :: NodeId -> NodeId -> EdgeUnifyM p Bool
structuralPairContainsLockedNode left right = do
    constraint <- liftPresolution (fst <$> getConstraintAndCanonical)
    leftKind <-
        case Binding.nodeKind constraint (typeRef left) of
            Left err -> throwPresolutionErrorM (BindingTreeError err)
            Right kind -> pure kind
    rightKind <-
        case Binding.nodeKind constraint (typeRef right) of
            Left err -> throwPresolutionErrorM (BindingTreeError err)
            Right kind -> pure kind
    pure (leftKind == Binding.NodeLocked || rightKind == Binding.NodeLocked)

unifyStructureRoots :: NodeId -> NodeId -> EdgeUnifyM p ()
unifyStructureRoots root1 root2 = do
    recordCanonicalNodeLookups 2
    node1 <- liftPresolution $ Ops.getNode root1
    node2 <- liftPresolution $ Ops.getNode root2
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
                            when (b1 /= b2) $ do
                                bound1 <- getCanonicalNodeEdge b1
                                bound2 <- getCanonicalNodeEdge b2
                                case (bound1, bound2) of
                                    (TyForall{}, TyForall{}) ->
                                        void
                                            ( matchBoundBody
                                                IntSet.empty
                                                IntMap.empty
                                                IntMap.empty
                                                IntMap.empty
                                                b1
                                                b2
                                            )
                                    _ -> unifyStructureEdge b1 b2
                        (Just b1, Nothing) ->
                            void (trySetBound (tnId nB) b1)
                        (Nothing, Just b2) ->
                            void (trySetBound (tnId nA) b2)
                        _ -> pure ()
                _ -> pure ()
        trySetBound target bnd = do
            recordEdgeUnifyStat $ \stats ->
                stats { eusSetVarBoundAttempts = eusSetVarBoundAttempts stats + 1 }
            if target == bnd
                then pure False
                else do
                    targetC <- findRootM target
                    bndC <- findRootM bnd
                    if bndC == targetC
                        then pure False
                        else do
                            (c0, canonical) <- liftPresolution getConstraintAndCanonical
                            recordEdgeUnifyStat $ \stats ->
                                stats { eusOccursChecks = eusOccursChecks stats + 1 }
                            occurs <-
                                case Traversal.occursInUnder canonical (NodeAccess.lookupNode c0) targetC bndC of
                                    Left _ -> pure True
                                    Right ok -> pure ok
                            if occurs
                                then throwPresolutionErrorM (OccursCheckPresolution targetC bndC)
                                else do
                                    void (recordRootTransitionToExterior targetC bndC)
                                    stBeforeBound <- getEdgeUnifyState
                                    let targetInterior =
                                            IntMap.findWithDefault
                                                mempty
                                                (getNodeId targetC)
                                                (eusInteriorByRoot stBeforeBound)
                                        boundInterior =
                                            IntMap.findWithDefault
                                                mempty
                                                (getNodeId bndC)
                                                (eusInteriorByRoot stBeforeBound)
                                    raiseTrace <-
                                        liftPresolution $
                                            Ops.setCanonicalVarBoundForEdgeWithRaiseTrace
                                                targetC
                                                (Just bndC)
                                    -- This is ordinary UF/bound harmonization,
                                    -- not an Ω construction step.  Preserve
                                    -- only Raises witnessed by the involved
                                    -- source buckets; exterior target Raises
                                    -- remain graph administration.
                                    recordRaisesFromTrace
                                        (targetInterior <> boundInterior)
                                        raiseTrace
                                    clearEdgeUnifyStructureCache
                                    pure True
        instantiateForallBound forallRoot targetRoot = do
            (constraint, canonical) <- liftPresolution getConstraintAndCanonical
            binders <-
                case Binding.orderedBinders canonical constraint (typeRef forallRoot) of
                    Left err -> throwPresolutionErrorM (BindingTreeError err)
                    Right ordered -> pure ordered
            forallNode <- getCanonicalNodeEdge forallRoot
            case forallNode of
                TyForall {tnBody = body} -> do
                    _ <-
                        matchBoundBody
                            (IntSet.fromList (map (getNodeId . canonical) binders))
                            IntMap.empty
                            IntMap.empty
                            IntMap.empty
                            body
                            targetRoot
                    pure ()
                _ -> unifyStructureEdge forallRoot targetRoot

        matchBoundBody binderKeys recursiveForward recursiveReverse seen source target = do
            sourceRoot <- findRootM source
            targetRoot <- findRootM target
            recursiveMatch <-
                matchRecursiveBinderOccurrence
                    recursiveForward
                    recursiveReverse
                    seen
                    sourceRoot
                    targetRoot
            case recursiveMatch of
                Just seen' -> pure seen'
                Nothing
                    | sourceRoot == targetRoot -> pure seen
                    | IntSet.member (getNodeId sourceRoot) binderKeys ->
                        matchBinderOccurrence seen sourceRoot targetRoot
                    | otherwise -> do
                            sourceNode <- getCanonicalNodeEdge sourceRoot
                            targetNode <- getCanonicalNodeEdge targetRoot
                            case (sourceNode, targetNode) of
                                (TyForall {tnBody = sourceBody}, TyForall {tnBody = targetBody}) -> do
                                    (constraint, canonical) <- liftPresolution getConstraintAndCanonical
                                    nestedBinders <-
                                        case Binding.orderedBinders canonical constraint (typeRef sourceRoot) of
                                            Left err -> throwPresolutionErrorM (BindingTreeError err)
                                            Right ordered -> pure ordered
                                    let nestedKeys =
                                            IntSet.fromList
                                                (map (getNodeId . canonical) nestedBinders)
                                    matchBoundBody
                                        (IntSet.union binderKeys nestedKeys)
                                        recursiveForward
                                        recursiveReverse
                                        seen
                                        sourceBody
                                        targetBody
                                (TyForall {tnBody = sourceBody}, _) -> do
                                    (constraint, canonical) <- liftPresolution getConstraintAndCanonical
                                    nestedBinders <-
                                        case Binding.orderedBinders canonical constraint (typeRef sourceRoot) of
                                            Left err -> throwPresolutionErrorM (BindingTreeError err)
                                            Right ordered -> pure ordered
                                    let nestedKeys =
                                            IntSet.fromList
                                                (map (getNodeId . canonical) nestedBinders)
                                    matchBoundBody
                                        (IntSet.union binderKeys nestedKeys)
                                        recursiveForward
                                        recursiveReverse
                                        seen
                                        sourceBody
                                        targetRoot
                                -- A bounded proxy below a locked structural owner
                                -- cannot be merged or rebound.  Its existing lower
                                -- bound is nevertheless the structural evidence that
                                -- the source instance exposes, so compare that bound
                                -- recursively against the target without mutating the
                                -- locked proxy itself.
                                (TyVar {tnBound = Just sourceBound}, _) ->
                                    matchBoundBody
                                        binderKeys
                                        recursiveForward
                                        recursiveReverse
                                        seen
                                        sourceBound
                                        targetRoot
                                (_, TyVar {}) -> do
                                    constrainTarget targetRoot sourceRoot
                                    pure seen
                                (TyArrow {tnDom = sourceDom, tnCod = sourceCod}, TyArrow {tnDom = targetDom, tnCod = targetCod}) -> do
                                    seen' <-
                                        matchBoundBody
                                            binderKeys
                                            recursiveForward
                                            recursiveReverse
                                            seen
                                            sourceDom
                                            targetDom
                                    matchBoundBody
                                        binderKeys
                                        recursiveForward
                                        recursiveReverse
                                        seen'
                                        sourceCod
                                        targetCod
                                (TyMu {tnBody = sourceBody}, TyMu {tnBody = targetBody}) -> do
                                    (constraint, canonical) <- liftPresolution getConstraintAndCanonical
                                    let sourceBinders =
                                            recursiveBinders
                                                canonical
                                                constraint
                                                sourceRoot
                                                sourceBody
                                        targetBinders =
                                            recursiveBinders
                                                canonical
                                                constraint
                                                targetRoot
                                                targetBody
                                    if length sourceBinders /= length targetBinders
                                        then
                                            throwPresolutionErrorM
                                                ( UnmatchableTypes
                                                    sourceRoot
                                                    targetRoot
                                                    "recursive binder arity mismatch"
                                                )
                                        else do
                                            let binderPairs = zip sourceBinders targetBinders
                                            (recursiveForward', recursiveReverse') <-
                                                foldM
                                                    recordRecursiveBinder
                                                    (recursiveForward, recursiveReverse)
                                                    binderPairs
                                            matchBoundBody
                                                binderKeys
                                                recursiveForward'
                                                recursiveReverse'
                                                seen
                                                sourceBody
                                                targetBody
                                (TyBase {tnBaseIdentity = sourceIdentity}, TyBase {tnBaseIdentity = targetIdentity})
                                    | sourceIdentity == targetIdentity -> pure seen
                                (TyBottom {}, TyBottom {}) -> pure seen
                                (TyCon {tnConIdentity = sourceIdentity, tnArgs = sourceArgs}, TyCon {tnConIdentity = targetIdentity, tnArgs = targetArgs})
                                    | sourceIdentity == targetIdentity
                                    , NE.length sourceArgs == NE.length targetArgs ->
                                        foldM
                                            (\acc (sourceArg, targetArg) ->
                                                matchBoundBody
                                                    binderKeys
                                                    recursiveForward
                                                    recursiveReverse
                                                    acc
                                                    sourceArg
                                                    targetArg
                                            )
                                            seen
                                            (zip (NE.toList sourceArgs) (NE.toList targetArgs))
                                (TyVarApp {tnVarHead = sourceHead, tnArgs = sourceArgs}, TyVarApp {tnVarHead = targetHead, tnArgs = targetArgs})
                                    | NE.length sourceArgs == NE.length targetArgs -> do
                                        seen' <-
                                            matchBoundBody
                                                binderKeys
                                                recursiveForward
                                                recursiveReverse
                                                seen
                                                sourceHead
                                                targetHead
                                        foldM
                                            (\acc (sourceArg, targetArg) ->
                                                matchBoundBody
                                                    binderKeys
                                                    recursiveForward
                                                    recursiveReverse
                                                    acc
                                                    sourceArg
                                                    targetArg
                                            )
                                            seen'
                                            (zip (NE.toList sourceArgs) (NE.toList targetArgs))
                                _ -> do
                                    lockedMismatch <-
                                        structuralPairContainsLockedNode sourceRoot targetRoot
                                    if lockedMismatch
                                        then
                                            throwPresolutionErrorM
                                                ( UnmatchableTypes
                                                    sourceRoot
                                                    targetRoot
                                                    "locked structural mismatch"
                                                )
                                        else do
                                            unifyStructureEdge sourceRoot targetRoot
                                            pure seen

        matchBinderOccurrence seen binder target =
            case IntMap.lookup (getNodeId binder) seen of
                Nothing -> do
                    binderNode <- getCanonicalNodeEdge binder
                    case binderNode of
                        TyVar {tnBound = Just lowerBound} ->
                            constrainTarget target lowerBound
                        _ -> pure ()
                    pure (IntMap.insert (getNodeId binder) target seen)
                Just firstTarget -> do
                    constrainTarget target firstTarget
                    pure seen

        matchRecursiveBinderOccurrence recursiveForward recursiveReverse seen source target =
            case
                ( IntMap.lookup (getNodeId source) recursiveForward
                , IntMap.lookup (getNodeId target) recursiveReverse
                )
            of
                (Nothing, Nothing) -> pure Nothing
                (Just expectedTarget, mbExpectedSource) -> do
                    expectedTargetRoot <- findRootM expectedTarget
                    if expectedTargetRoot /= target
                        then
                            throwPresolutionErrorM
                                ( UnmatchableTypes
                                    source
                                    target
                                    "inconsistent recursive binder occurrence"
                                )
                        else
                            case mbExpectedSource of
                                Nothing -> pure (Just seen)
                                Just expectedSource -> do
                                    expectedSourceRoot <- findRootM expectedSource
                                    if expectedSourceRoot == source
                                        then pure (Just seen)
                                        else
                                            throwPresolutionErrorM
                                                ( UnmatchableTypes
                                                    source
                                                    target
                                                    "non-injective recursive binder match"
                                                )
                (Nothing, Just expectedSource) -> do
                    expectedSourceRoot <- findRootM expectedSource
                    if expectedSourceRoot == source
                        then pure (Just seen)
                        else
                            throwPresolutionErrorM
                                ( UnmatchableTypes
                                    source
                                    target
                                    "non-injective recursive binder match"
                                )

        recordRecursiveBinder (recursiveForward, recursiveReverse) (sourceBinder, targetBinder) =
            case IntMap.lookup (getNodeId sourceBinder) recursiveForward of
                Just expectedTarget
                    | expectedTarget /= targetBinder ->
                        throwPresolutionErrorM
                            ( UnmatchableTypes
                                sourceBinder
                                targetBinder
                                "inconsistent recursive binder occurrence"
                            )
                _
                    | Just expectedSource <-
                        IntMap.lookup (getNodeId targetBinder) recursiveReverse
                    , expectedSource /= sourceBinder ->
                        throwPresolutionErrorM
                            ( UnmatchableTypes
                                sourceBinder
                                targetBinder
                                "non-injective recursive binder match"
                            )
                _ ->
                    pure
                        ( IntMap.insert
                            (getNodeId sourceBinder)
                            targetBinder
                            recursiveForward
                        , IntMap.insert
                            (getNodeId targetBinder)
                            sourceBinder
                            recursiveReverse
                        )

        recursiveBinders canonical constraint root body =
            let rootC = canonical root
                reachable =
                    Traversal.reachableFromWithBounds
                        canonical
                        (NodeAccess.lookupNode constraint)
                        (canonical body)
                binderKeys =
                    IntSet.fromList
                        [ getNodeId binderC
                        | binderKey <- IntSet.toAscList reachable
                        , let binderC = NodeId binderKey
                        , Just TyVar {} <- [NodeAccess.lookupNode constraint binderC]
                        , case Binding.lookupBindParent constraint (typeRef binderC) of
                            Just (TypeRef parent, _) -> canonical parent == rootC
                            _ -> False
                        ]
             in map NodeId (IntSet.toAscList binderKeys)

        constrainTarget target lowerBound = do
            targetRoot <- findRootM target
            lowerRoot <- findRootM lowerBound
            when (targetRoot /= lowerRoot) $ do
                targetNode <- getCanonicalNodeEdge targetRoot
                case targetNode of
                    TyVar {tnBound = Nothing} -> do
                        harmonizeBoundRelation targetRoot lowerRoot
                        _ <- trySetBound targetRoot lowerRoot
                        pure ()
                    TyVar {tnBound = Just currentBound} ->
                        when (currentBound /= lowerRoot) $
                            unifyStructureEdge currentBound lowerRoot
                    _ -> unifyStructureEdge lowerRoot targetRoot

        harmonizeBoundRelation target lowerBound = do
            (constraint, _canonical) <- liftPresolution getConstraintAndCanonical
            case BindingAdjustment.harmonizeBindParentsWithTrace
                (TypeRefTag target)
                (TypeRefTag lowerBound)
                constraint of
                Left err ->
                    throwPresolutionErrorM (BindingTreeError err)
                Right (constraint', _raiseTrace) -> do
                    liftPresolution $ modifyConstraint (const constraint')
                    -- This harmonization establishes the target-side lower-bound
                    -- relation needed to match the copied source bound.  It is not
                    -- an operation on the source scheme interior, so it must not be
                    -- encoded in the edge witness Ω.
                    clearEdgeUnifyStructureCache

        unifyStructureChildren nodeA nodeB =
            case (nodeA, nodeB) of
                (TyVar{}, _) -> pure ()
                (_, TyVar{}) -> pure ()
                (TyExp{}, _) -> pure ()
                (_, TyExp{}) -> pure ()
                (TyArrow { tnDom = d1, tnCod = c1 }, TyArrow { tnDom = d2, tnCod = c2 }) -> do
                    recordChildEdges 2
                    unifyStructureEdge d1 d2
                    unifyStructureEdge c1 c2
                (TyForall { tnBody = b1 }, TyForall { tnBody = b2 }) -> do
                    recordChildEdges 1
                    unifyStructureEdge b1 b2
                (TyMu { tnBody = b1 }, TyMu { tnBody = b2 }) -> do
                    recordChildEdges 1
                    unifyStructureEdge b1 b2
                (TyBase { tnBaseIdentity = identity1 }, TyBase { tnBaseIdentity = identity2 })
                    | identity1 == identity2 -> pure ()
                (TyBottom{}, TyBottom{}) ->
                    pure ()
                (TyCon { tnConIdentity = identity1, tnArgs = args1 }, TyCon { tnConIdentity = identity2, tnArgs = args2 })
                    | identity1 == identity2
                    , NE.length args1 == NE.length args2 -> do
                        recordChildEdges (NE.length args1)
                        mapM_ (uncurry unifyStructureEdge) (zip (NE.toList args1) (NE.toList args2))
                (TyVarApp { tnVarHead = head1, tnArgs = args1 }, TyVarApp { tnVarHead = head2, tnArgs = args2 })
                    | NE.length args1 == NE.length args2 -> do
                        recordChildEdges (1 + NE.length args1)
                        unifyStructureEdge head1 head2
                        mapM_ (uncurry unifyStructureEdge) (zip (NE.toList args1) (NE.toList args2))
                _ -> pure ()
        preserveGraftedMetaRelation metaRoot exteriorRoot = do
            st <- getEdgeUnifyState
            let metaBinders =
                    IntMap.findWithDefault
                        IntSet.empty
                        (getNodeId metaRoot)
                        (eusBindersByRoot st)
                exteriorBinders =
                    IntMap.findWithDefault
                        IntSet.empty
                        (getNodeId exteriorRoot)
                        (eusBindersByRoot st)
                binders = IntSet.union metaBinders exteriorBinders
            modifyEdgeUnifyState $ \st' ->
                st'
                    { eusBindersByRoot =
                        if IntSet.null binders
                            then eusBindersByRoot st'
                            else
                                IntMap.insert
                                    (getNodeId exteriorRoot)
                                    binders
                                    (eusBindersByRoot st')
                    }
            recordMergesIntoRep binders
    lockedStructuralPair <-
        case (node1, node2) of
            (TyVar {}, _) -> pure False
            (_, TyVar {}) -> pure False
            (TyExp {}, _) -> pure False
            (_, TyExp {}) -> pure False
            _ ->
                structuralPairContainsLockedNode root1 root2
    if lockedStructuralPair
        then
            void
                ( matchBoundBody
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    root1
                    root2
                )
        else if isMeta1 || isMeta2
        then do
            recordEdgeUnifyStat $ \stats ->
                stats { eusUnifyStructureMetaPath = eusUnifyStructureMetaPath stats + 1 }
            if isVar1 && isVar2
                then do
                    recordEdgeUnifyStat $ \stats ->
                        stats { eusUnifyStructureVarVar = eusUnifyStructureVarVar stats + 1 }
                    recordEdgeUnifyStat $ \stats ->
                        stats { eusUnifyAcyclicCalls = eusUnifyAcyclicCalls stats + 1 }
                    preserve1 <- isScheduledUnboundedBinderMetaRoot root1
                    preserve2 <- isScheduledUnboundedBinderMetaRoot root2
                    case (preserve1, preserve2) of
                        (True, False) -> do
                            unifyVarBounds node1 node2
                            preserveGraftedMetaRelation root1 root2
                        (False, True) -> do
                            unifyVarBounds node1 node2
                            preserveGraftedMetaRelation root2 root1
                        _ -> do
                            unifyAcyclicEdgeCore node1 node2 root1 root2
                            unifyVarBounds node1 node2
                else do
                    let (metaRoot, otherNode) =
                            if isMeta1 then (root1, node2) else (root2, node1)
                    mbMetaBound <- lookupVarBoundM metaRoot
                    case mbMetaBound of
                        Just bMeta -> do
                            recordCanonicalNodeLookups 1
                            bMetaNode <- getCanonicalNodeEdge bMeta
                            case bMetaNode of
                                TyVar{} -> do
                                    _ <- trySetBound bMeta (tnId otherNode)
                                    pure ()
                                TyForall{} -> do
                                    instantiateForallBound (tnId bMetaNode) (tnId otherNode)
                                    void (trySetBound metaRoot (tnId otherNode))
                                _ -> unifyStructureEdge bMeta (tnId otherNode)
                        Nothing -> do
                            _ <- trySetBound metaRoot (tnId otherNode)
                            pure ()
        else do
            recordEdgeUnifyStat $ \stats ->
                stats { eusUnifyAcyclicCalls = eusUnifyAcyclicCalls stats + 1 }
            case (node1, node2) of
                (TyVar {tnBound = Just lower}, TyVar {tnId = target, tnBound = Nothing}) -> do
                    unifyAcyclicEdgeCore node1 node2 root1 root2
                    void (trySetBound target lower)
                (TyVar {tnId = target, tnBound = Nothing}, TyVar {tnBound = Just lower}) -> do
                    unifyAcyclicEdgeCore node1 node2 root1 root2
                    void (trySetBound target lower)
                (TyVar {tnBound = Just b1}, TyVar {tnBound = Just b2}) -> do
                    unifyAcyclicEdgeCore node1 node2 root1 root2
                    when (b1 /= b2) (unifyVarBounds node1 node2)
                (TyVar {}, TyVar {}) ->
                    unifyAcyclicEdgeCore node1 node2 root1 root2
                (TyVar {tnBound = Just b1}, _) -> do
                    unifyAcyclicEdgeCore node1 node2 root1 root2
                    when (b1 /= tnId node2) (unifyStructureEdge b1 (tnId node2))
                (_, TyVar {tnBound = Just b2}) -> do
                    unifyAcyclicEdgeCore node1 node2 root1 root2
                    when (b2 /= tnId node1) (unifyStructureEdge (tnId node1) b2)
                _ -> do
                    unifyAcyclicEdgeCore node1 node2 root1 root2
                    unifyStructureChildren node1 node2

isBinderMetaRoot :: NodeId -> EdgeUnifyM p Bool
isBinderMetaRoot root = do
    metaRoots <- gets eusBinderMetaRoots
    pure (IntSet.member (getNodeId root) metaRoots)
