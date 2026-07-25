{- |
Module      : MLF.Constraint.Presolution.Copy
Description : χe copying for instantiation

This module implements the χe-style copying performed during instantiation:
copying a ∀-body graph while substituting its bound variables with fresh nodes
at the target level, while preserving binding edges/flags and internal sharing.
-}
module MLF.Constraint.Presolution.Copy (
    expansionCopySetsM,
    instantiateScheme,
    instantiateSchemeWithTrace,
    instantiateSchemeWithTraceSnapshot,
    instantiateExpansionWithTraceAtTargetSnapshot,
    copyForallBoundProjectionAtBinder,
    ExpansionBinderProjection(..),
    projectExpansionBinders,
    bindExpansionRootLikeTarget,
    bindUnboundCopiedNodes
) where

import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution.Base (
    CopyMap,
    CopyMapping(..),
    MonadPresolution(getConstraint),
    PresolutionError(..),
    PresolutionM,
    PresolutionState(..),
    RawExpansionConstruction,
    copiedNodes,
    instantiationBindersM,
    lookupCopy,
    mkRawExpansionConstruction,
    modifyConstraintDirtyTypesState,
    setConstraintDirtyBindRefsState
    )
import MLF.Constraint.Presolution.Ops (
    createFreshNodeId,
    registerNode,
    setBindParentM,
    validateLowerBoundGraph
    )
import qualified MLF.Constraint.Presolution.BoundScope as BoundScope
import MLF.Constraint.Presolution.StateAccess (
    PresolutionBindingSnapshot(..),
    bindingSnapshotBoundFlexChildren,
    bindingSnapshotFindSchemeIntroducer,
    bindingSnapshotInteriorOf,
    bindingSnapshotLookupBindParent,
    bindingSnapshotPathToRoot,
    getBindingSnapshot,
    getConstraintAndCanonical,
    lookupBindParentM
    )
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Types.Graph hiding (lookupNode)

data CopyState = CopyState
    { csCache :: IntMap NodeId
    , csCopyMap :: IntMap NodeId
    , csInterior :: IntSet.IntSet
    , csPendingBounds :: IntMap NodeId
    , csEncounteredSubstitutions :: IntSet.IntSet
    }

{- Note [Retained scheme-root wrappers are not chi_e binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Constraint generation can retain a lower-bounded 'TyVar' as the presentation
root of a nested scheme.  Such a node belongs to the structural projection
copied by Definition 10.1.1, but it is not one of the enclosing scheme's
quantified variables: substituting it would erase the nested scheme boundary.

The copy traversal therefore retains these wrappers as ordinary copied nodes.
Expansion planning must use the same classification before allocating recipe
arguments; otherwise an 'ExpInstantiate' advertises an argument that chi_e
intentionally does not consume.
-}
retainedSchemeRootWrapperKeys
    :: Constraint p
    -> (NodeId -> NodeId)
    -> IntSet.IntSet
retainedSchemeRootWrapperKeys constraint canonical =
    IntSet.fromList
        [ getNodeId root
        | gen <- NodeAccess.allGenNodes constraint
        , root0 <- gnSchemes gen
        , let root = canonical root0
        , Just TyVar{tnBound = Just _} <- [NodeAccess.lookupNode constraint root]
        ]

-- | Classification of raw instantiation-binder candidates against the exact
-- semantic traversal performed by χe.
data ExpansionBinderProjection = ExpansionBinderProjection
    { ebpSemanticBinders :: [NodeId]
    , ebpRetainedSchemeRootWrappers :: [NodeId]
    , ebpOutsideSemanticLane :: [NodeId]
    }
    deriving (Eq, Show)

-- | Classify raw binder candidates before allocating an 'ExpInstantiate'
-- recipe.  Structural reachability alone is insufficient: χe stops at its
-- frontier and shares nodes outside its copy interior.  A binder whose every
-- occurrence lies behind either boundary is not part of the semantic
-- substitution lane, even when it is flex-bound under the source gen.
--
-- This is the allocation-time counterpart of
-- 'validateSemanticBinderProjection'.  The constructor still validates the
-- realized copy, so this projection cannot turn a traversal mismatch into a
-- silent fallback.
projectExpansionBinders
    :: PresolutionBindingSnapshot p
    -> GenNodeId
    -> NodeId
    -> [NodeId]
    -> PresolutionM p ExpansionBinderProjection
projectExpansionBinders snapshot sourceOwner bodyId candidates0 = do
    (copyInterior0, frontierSet0) <-
        expansionCopySetsForOwnerWithSnapshot snapshot sourceOwner bodyId
    let constraint = pbsConstraint snapshot
        canonical = pbsCanonical snapshot
        candidates = deduplicateCanonicalNodes canonical candidates0
        retainedKeys = retainedSchemeRootWrapperKeys constraint canonical
        isRetained binder =
            IntSet.member (getNodeId (canonical binder)) retainedKeys
        retained = filter isRetained candidates
        substitutionCandidates = filter (not . isRetained) candidates
        substitutionKeys =
            IntSet.fromList (map (getNodeId . canonical) substitutionCandidates)
        body = canonical bodyId
        bodyKey = getNodeId body
        isDegenerate = IntSet.notMember bodyKey copyInterior0
        copyInterior
            | isDegenerate = IntSet.insert bodyKey copyInterior0
            | otherwise = copyInterior0
        frontierWithDegenerateRoot
            | isDegenerate = IntSet.insert bodyKey frontierSet0
            | otherwise = frontierSet0
        frontier =
            IntSet.difference frontierWithDegenerateRoot substitutionKeys
        encountered =
            semanticSubstitutionDomain
                (cNodes constraint)
                canonical
                copyInterior
                frontier
                substitutionKeys
                body
        semantic =
            filter
                ( \binder ->
                    IntSet.member
                        (getNodeId (canonical binder))
                        encountered
                )
                substitutionCandidates
        outside =
            filter
                ( \binder ->
                    IntSet.notMember
                        (getNodeId (canonical binder))
                        encountered
                )
                substitutionCandidates
    pure
        ExpansionBinderProjection
            { ebpSemanticBinders = semantic
            , ebpRetainedSchemeRootWrappers = retained
            , ebpOutsideSemanticLane = outside
            }

-- | Compute the substitution keys that the allocation-free χe traversal will
-- encounter.  The branch order deliberately mirrors 'atomicCopyNode':
-- administrative expansions are transparent, substitution precedes frontier
-- handling, and both frontier and exterior nodes terminate traversal.
semanticSubstitutionDomain
    :: NodeMap TyNode
    -> (NodeId -> NodeId)
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> NodeId
    -> IntSet.IntSet
semanticSubstitutionDomain nodes canonical copyInterior frontier substitutions root =
    go IntSet.empty IntSet.empty [root]
  where
    go _ encountered [] = encountered
    go visited encountered (node0 : rest)
        | IntSet.member key visited =
            go visited encountered rest
        | otherwise =
            case lookupNodeIn nodes node of
                Nothing ->
                    go visited' encountered rest
                Just TyExp{tnBody = body} ->
                    go visited' encountered (body : rest)
                Just sourceNode
                    | IntSet.member key substitutions ->
                        let boundChildren =
                                case sourceNode of
                                    TyVar{tnBound = Just bound} -> [bound]
                                    _ -> []
                        in go
                            visited'
                            (IntSet.insert key encountered)
                            (boundChildren ++ rest)
                    | IntSet.member key frontier ->
                        go visited' encountered rest
                    | IntSet.notMember key copyInterior ->
                        go visited' encountered rest
                    | otherwise ->
                        go
                            visited'
                            encountered
                            (structuralChildrenWithBounds sourceNode ++ rest)
      where
        node = canonical node0
        key = getNodeId node
        visited' = IntSet.insert key visited

expansionCopySetsM :: NodeId -> PresolutionM p (GenNodeId, IntSet.IntSet, IntSet.IntSet)
expansionCopySetsM bodyId = do
    snapshot <- getBindingSnapshot
    expansionCopySetsWithSnapshot snapshot bodyId

expansionCopySetsWithSnapshot
    :: PresolutionBindingSnapshot p
    -> NodeId
    -> PresolutionM p (GenNodeId, IntSet.IntSet, IntSet.IntSet)
expansionCopySetsWithSnapshot snapshot bodyId = do
    let canonical = pbsCanonical snapshot
        bodyC = canonical bodyId
    gid <- bindingSnapshotFindSchemeIntroducer snapshot bodyC
    (interior, frontier) <-
        expansionCopySetsForOwnerWithSnapshot snapshot gid bodyC
    pure (gid, interior, frontier)

-- | Compute the paper's @I^s(g)@ and @F^s(g)@ projection for an explicit
-- source owner.  Edge planning has already established @g@; in particular an
-- annotation wrapper can be owned by a coercion-local gen while its body stays
-- in the enclosing scope.  Re-deriving @g@ from that body would incorrectly
-- turn the annotation's degenerate scheme into an ordinary local scheme.
expansionCopySetsForOwnerWithSnapshot
    :: PresolutionBindingSnapshot p
    -> GenNodeId
    -> NodeId
    -> PresolutionM p (IntSet.IntSet, IntSet.IntSet)
expansionCopySetsForOwnerWithSnapshot snapshot gid bodyId = do
    let c0 = pbsConstraint snapshot
        canonical = pbsCanonical snapshot
        bodyC = canonical bodyId
        lookupNode = lookupNodeIn (cNodes c0)
        children :: NodeId -> [NodeId]
        children nid =
            case lookupNode nid of
                Nothing -> []
                Just node -> structuralChildrenWithBounds node
        childrenRef :: NodeRef -> [NodeRef]
        childrenRef ref = case ref of
            TypeRef nid ->
                case lookupNode nid of
                    Nothing -> []
                    Just node ->
                        map TypeRef (structuralChildrenWithBounds node)
            GenRef _ ->
                []
    ownerInterior <- bindingSnapshotInteriorOf snapshot (genRef gid)
    binderRef <- do
        mbParentInfo <- bindingSnapshotLookupBindParent snapshot (typeRef bodyC)
        pure $ case mbParentInfo of
            Just (TypeRef pid, _flag) ->
                case NodeAccess.lookupNode c0 (canonical pid) of
                    Just TyForall{} -> typeRef (canonical pid)
                    _ -> typeRef bodyC
            _ -> typeRef bodyC
    -- Copy nodes in the explicit scheme interior that are structurally
    -- reachable from the body.  A legacy TyVar scheme root without a
    -- TyForall parent still falls back to its gen-node interior; an explicit
    -- TyForall already owns the exact binder set and must not absorb free
    -- variables that merely share the surrounding gen node.
    let useGenInterior =
            case NodeAccess.lookupNode c0 bodyC of
                Just TyVar{ tnBound = Just _ } -> binderRef == typeRef bodyC
                _ -> False
        interiorRoot =
            if useGenInterior
                then genRef gid
                else binderRef
    interiorAll0 <- bindingSnapshotInteriorOf snapshot interiorRoot
    bindersUnderGen <- bindingSnapshotBoundFlexChildren snapshot (genRef gid)
    let binderKeysGen =
            IntSet.fromList
                [ nodeRefKey (typeRef (canonical b))
                | b <- bindersUnderGen
                ]
        interiorAll0'
            | useGenInterior = IntSet.union interiorAll0 binderKeysGen
            | otherwise = interiorAll0
    let reachFromS =
            Traversal.reachableFromNodes canonical children [bodyC]
        reachFromSKeys =
            IntSet.fromAscList [typeRefKey (NodeId nid) | nid <- IntSet.toList reachFromS]
    let -- A scheme body outside the supplied owner is the paper's degenerate
        -- case.  The empty intersection is completed by the copy constructor,
        -- which copies the body and records it in the frontier for unification.
        interiorAll = IntSet.intersection ownerInterior interiorAll0'
    (_root, binders) <- instantiationBindersM gid bodyC
    let binderKeys =
            IntSet.fromList
                [ nodeRefKey (typeRef (canonical b))
                | b <- binders
                ]
        interiorStructRefs0 = IntSet.intersection interiorAll reachFromSKeys
        interiorStructRefs =
            IntSet.union interiorStructRefs0 (IntSet.intersection binderKeys reachFromSKeys)
        frontierAll =
            foldl'
                (\acc key ->
                    let ref = nodeRefFromKey key
                        childRefs = childrenRef ref
                    in foldl'
                        (\acc0 child ->
                            let childC = Canonicalize.canonicalRef canonical child
                                childKey = nodeRefKey childC
                            in if IntSet.member childKey interiorStructRefs
                                then acc0
                                else case childC of
                                    TypeRef nid -> IntSet.insert (getNodeId nid) acc0
                                    GenRef _ -> acc0
                        )
                        acc
                        childRefs
                )
                IntSet.empty
                (IntSet.toList interiorStructRefs)
        interiorTypeSet =
            IntSet.fromList
                [ getNodeId nid
                | key <- IntSet.toList interiorStructRefs
                , TypeRef nid <- [nodeRefFromKey key]
                ]
        frontierTypeSet =
            IntSet.fromList
                [ nid
                | nid <- IntSet.toList frontierAll
                , IntSet.member (typeRefKey (NodeId nid)) reachFromSKeys
                ]
    pure (interiorTypeSet, frontierTypeSet)

{- Note [instantiateScheme]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Goal
    Copy a ∀-body graph while substituting its bound vars with fresh nodes at the
    target level (per `papers/recasting-mlf-RR.txt` §5, Def. 5 and
    `papers/Remy-Yakobowski@icfp08_mlf-type-inference.txt` §4).

Guarantees
    • Bound vars substitute: `substList` replaces exactly the binders being
        instantiated.
    • Share outer scope: nodes with level < quantLevel are reused, not copied,
        preserving context and avoiding spurious polymorphism.
    • Preserve structure: arrows / foralls / expansions are recursively copied.
        Base nodes outside the copy set are shared, while interior bases are
        copied with the same identity so their binding ownership stays local.
    • Preserve sharing: a StateT cache copies each source node at most once,
        keeping internal sharing and breaking cycles.
    • One pass, registered: `copyNode` both allocates fresh NodeIds and registers
        them into `cNodes`, so everything it creates is live in the constraint.
    • Necessity: plain ID substitution cannot simultaneously freshen binders,
        share outer nodes, and preserve internal sharing; `copyNode` implements the
        paper’s copy-with-subst traversal to do all three at once.

Failure mode
    • Missing node lookups raise `NodeLookupFailed` (tests cover this), keeping
        instantiation total on well-formed graphs.
-}
-- | Instantiate a scheme by copying the graph and replacing bound variables.
instantiateScheme :: NodeId -> [(NodeId, NodeId)] -> PresolutionM p NodeId
instantiateScheme bodyId substList = do
    (root, _copyMap, _interior, _frontier) <- instantiateSchemeWithMode False bodyId substList
    pure root

-- | Like 'instantiateScheme', but also return:
--   • a copy provenance map (original node → copied/replaced node), and
--   • the expansion interior I(r) as an IntSet (computed from binding edges), and
--   • the frontier nodes that were copied as ⊥.
--
-- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.2):
-- when expanding an instantiation edge, we copy exactly the nodes "structurally
-- strictly under g and in I(g)" and preserve binding edges/flags for copied nodes.
-- The expansion root is bound at the same binder as the target node.
instantiateSchemeWithTrace :: NodeId -> [(NodeId, NodeId)] -> PresolutionM p (NodeId, CopyMap, IntSet.IntSet, IntSet.IntSet)
instantiateSchemeWithTrace bodyId substList =
    instantiateSchemeWithMode True bodyId substList

instantiateSchemeWithTraceSnapshot
    :: PresolutionBindingSnapshot p
    -> NodeId
    -> [(NodeId, NodeId)]
    -> PresolutionM p (NodeId, CopyMap, IntSet.IntSet, IntSet.IntSet)
instantiateSchemeWithTraceSnapshot snapshot bodyId substList =
    instantiateSchemeWithModeSnapshot True snapshot bodyId substList

{- Note [Atomic two-lane edge instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ExpInstantiate edge has two related, but distinct, copies of quantified
lower bounds.

  * The semantic copy substitutes source binders with the fresh binder metas
    used by χe.  This is the only lane represented in CopyMap/interior/frontier
    provenance.
  * The recipe arguments are existing type nodes.  Each bounded argument owns
    an auxiliary copy made with binder-to-argument substitution.  Sharing the
    meta-owned bound here would force a later Raise to their common gen scope.

Both lanes are allocated first and publish one combined binding-parent and
tnBound projection.  Consequently no intermediate graph can trigger a
post-copy scope repair, and the auxiliary implementation graph cannot leak
into the semantic witness domain.
-}

data AtomicCopyLane = AtomicCopyLane
    { aclGenId :: GenNodeId
    , aclSchemeRoot :: NodeId
    , aclRoot :: NodeId
    , aclCopyMap :: CopyMap
    , aclInterior :: IntSet.IntSet
    , aclFrontier :: IntSet.IntSet
    , aclFrontierForCopy :: IntSet.IntSet
    , aclSubstitutionKeys :: IntSet.IntSet
    , aclEncounteredSubstitutions :: IntSet.IntSet
    , aclPendingBounds :: IntMap NodeId
    , aclRootBinder :: NodeRef
    }

-- | The one authoritative binder plan for an atomic χe construction.
--
-- Both semantic metas and recipe arguments must describe exactly the same
-- reachable source binders.  Keeping the two substitutions in one plan makes
-- it impossible for an unused meta to enter Ω provenance or for the auxiliary
-- argument lane to silently use a different binder domain.
data AtomicBinderPlan = AtomicBinderPlan
    { abpBinderMetas :: [(NodeId, NodeId)]
    , abpBinderArgs :: [(NodeId, NodeId)]
    , abpSourceKeys :: IntSet.IntSet
    }

-- | Construct a complete ExpInstantiate copy at its edge destination.
--
-- The first result is the complete semantic trace, including every semantic
-- lower bound reached while substituting a binder.  The second result is kept
-- temporarily for the existing caller shape and is always empty.  Auxiliary
-- argument-owned copies are deliberately absent from both traces.
instantiateExpansionWithTraceAtTargetSnapshot
    :: PresolutionBindingSnapshot p
    -> GenNodeId
    -> NodeId
    -> NodeId
    -> [(NodeId, NodeId)]
    -> [(NodeId, NodeId)]
    -> PresolutionM p
        ( (NodeId, CopyMap, IntSet.IntSet, IntSet.IntSet)
        , (CopyMap, IntSet.IntSet, IntSet.IntSet)
        , RawExpansionConstruction
        )
instantiateExpansionWithTraceAtTargetSnapshot snapshot sourceOwner targetNode bodyId binderMetas binderArgs = do
    let canonical = pbsCanonical snapshot
        c0 = pbsConstraint snapshot
    targetBinder <- expansionTargetBinder targetNode
    destinationGen <- atomicDestinationGen snapshot targetBinder
    binderPlan <-
        buildAtomicBinderPlan
            snapshot
            sourceOwner
            bodyId
            binderMetas
            binderArgs
    let activeBinderMetas = abpBinderMetas binderPlan
        activeBinderArgs = abpBinderArgs binderPlan
    metaSubstitution <- atomicStrictSubstitutionMap canonical activeBinderMetas
    argumentSubstitution <- atomicStrictSubstitutionMap canonical activeBinderArgs

    semanticBody <-
        buildAtomicCopyLane
            sourceOwner
            True
            snapshot
            targetBinder
            True
            bodyId
            activeBinderMetas
    validateSemanticBinderProjection
        c0
        canonical
        binderPlan
        metaSubstitution
        semanticBody

    (argumentLanesRev, argumentPending) <-
        do
            foldM
                ( constructArgumentBoundLane
                    activeBinderArgs
                    c0
                    canonical
                    targetBinder
                    metaSubstitution
                    argumentSubstitution
                )
                ([], IntMap.empty)
                activeBinderArgs

    argumentEdits <-
        foldM
            (planFreshArgumentParent canonical metaSubstitution destinationGen)
            IntMap.empty
            activeBinderArgs
    let argumentLanes = reverse argumentLanesRev
        allLanes = semanticBody : argumentLanes
    pendingBounds <-
        foldM
            (atomicMergePendingBounds canonical)
            IntMap.empty
            ( map aclPendingBounds allLanes
                ++ [argumentPending]
            )
    (laneEdits, projectedRoots) <-
        foldM
            (planAtomicCopyLane snapshot)
            (argumentEdits, IntSet.empty)
            allLanes
    bindingEdits <-
        foldM
            (planSemanticMetaParent canonical semanticBody)
            laneEdits
            activeBinderMetas
    commitAtomicCopiedProjection
        snapshot
        (aclRoot semanticBody)
        projectedRoots
        bindingEdits
        pendingBounds
    let constructionRoleKeys pairs =
            IntSet.fromList
                [ getNodeId node
                | (_sourceBinder, node0) <- pairs
                , let node = canonical node0
                , IntMap.member
                    (nodeRefKey (typeRef node))
                    bindingEdits
                ]
    construction <-
        case
            mkRawExpansionConstruction
                bindingEdits
                (constructionRoleKeys activeBinderArgs)
                (constructionRoleKeys activeBinderMetas)
          of
            Left err ->
                throwError
                    (InternalError ("invalid atomic expansion construction certificate: " ++ err))
            Right certificate -> pure certificate
    pure
        ( ( aclRoot semanticBody
          , aclCopyMap semanticBody
          , aclInterior semanticBody
          , aclFrontier semanticBody
          )
        , (mempty, IntSet.empty, IntSet.empty)
        , construction
        )
  where
    -- Graft is the first Omega operation for an unbounded source binder.  Its
    -- copied meta must already expose that argument while a following
    -- ExpForall step computes Q(n); the later Omega execution is idempotent and
    -- still records the explicit witness operation.  An argument whose owner
    -- is not on the expansion destination path cannot be referenced sideways,
    -- so copy that projection into the destination first.
    constructArgumentBoundLane activeBinderArgs c0 canonical targetBinder metaSubstitution argumentSubstitution (lanes, pendingAcc) (sourceBinder, argument) =
        case IntMap.lookup (getNodeId (canonical sourceBinder)) metaSubstitution of
            Nothing ->
                throwError
                    (InternalError ("missing binder meta for expansion argument " ++ show sourceBinder))
            Just meta
                | canonical meta == canonical argument -> pure (lanes, pendingAcc)
                | otherwise ->
                    case VarStore.lookupVarBound c0 (canonical sourceBinder) of
                        Nothing -> do
                            argumentIsAmbient <-
                                nodeIsAmbientAtBinder
                                    snapshot
                                    targetBinder
                                    argument
                            if not argumentIsAmbient
                                then do
                                    argumentOwner <-
                                        bindingSnapshotFindSchemeIntroducer
                                            snapshot
                                            (canonical argument)
                                    lane <-
                                        buildAtomicCopyLane
                                            argumentOwner
                                            True
                                            snapshot
                                            targetBinder
                                            False
                                            argument
                                            []
                                    pending <-
                                        atomicInsertPendingBound
                                            canonical
                                            meta
                                            (aclRoot lane)
                                            pendingAcc
                                    pure (lane : lanes, pending)
                                else do
                                    pending <-
                                        atomicInsertPendingBound
                                            canonical
                                            meta
                                            argument
                                            pendingAcc
                                    pure (lanes, pending)
                        Just sourceBound ->
                            case IntMap.lookup (getNodeId (canonical sourceBound)) argumentSubstitution of
                                Just argumentBound -> do
                                    pending <- atomicInsertPendingBound canonical argument argumentBound pendingAcc
                                    pure (lanes, pending)
                                Nothing -> do
                                    sourceBoundOwner <-
                                        bindingSnapshotFindSchemeIntroducer
                                            snapshot
                                            (canonical sourceBound)
                                    lane <-
                                        buildAtomicCopyLane
                                            sourceBoundOwner
                                            True
                                            snapshot
                                            (typeRef argument)
                                            False
                                            sourceBound
                                            activeBinderArgs
                                    pending <- atomicInsertPendingBound canonical argument (aclRoot lane) pendingAcc
                                    pure (lane : lanes, pending)

    planFreshArgumentParent canonical metaSubstitution destinationGen edits (sourceBinder, argument) =
        case IntMap.lookup (getNodeId (canonical sourceBinder)) metaSubstitution of
            Just meta
                | canonical meta == canonical argument -> pure edits
            _ ->
                let constraint = pbsConstraint snapshot
                    argumentC = canonical argument
                    mbExisting =
                        case Binding.lookupBindParent constraint (typeRef argument) of
                            Just parent -> Just parent
                            Nothing -> Binding.lookupBindParent constraint (typeRef argumentC)
                in case mbExisting of
                    Just _ -> pure edits
                    Nothing ->
                        atomicInsertBindingEdit
                            canonical
                            edits
                            (typeRef argument)
                            (destinationGen, BindFlex)

    planSemanticMetaParent canonical semanticBody edits (_sourceBinder, meta) =
        let metaC = canonical meta
            rootC = canonical (aclRoot semanticBody)
            parent
                | metaC == rootC = aclRootBinder semanticBody
                | otherwise = typeRef rootC
        in atomicInsertBindingEdit
            canonical
            edits
            (typeRef metaC)
            (parent, BindFlex)

-- | Copy a source-forall bound projection directly under its destination
-- binder.  The source-to-destination binder substitution is part of the copy,
-- so the returned root is well-scoped before it is installed as a lower bound.
--
-- Unlike an instantiation edge, a forall-bound projection shares its external
-- frontier and does not copy bounds from substituted source binders: each
-- destination binder receives its own bound from the enclosing 'ForallSpec'.
copyForallBoundProjectionAtBinder
    :: PresolutionBindingSnapshot p
    -> NodeRef
    -> NodeId
    -> [(NodeId, NodeId)]
    -> PresolutionM p NodeId
copyForallBoundProjectionAtBinder snapshot destinationBinder boundRoot substitutions = do
    sourceOwner <- bindingSnapshotFindSchemeIntroducer snapshot boundRoot
    lane <-
        buildAtomicCopyLane
            sourceOwner
            False
            snapshot
            destinationBinder
            False
            boundRoot
            substitutions
    (bindingEdits, projectedRoots) <-
        planAtomicCopyLane
            snapshot
            (IntMap.empty, IntSet.empty)
            lane
    commitAtomicCopiedProjection
        snapshot
        (aclRoot lane)
        projectedRoots
        bindingEdits
        (aclPendingBounds lane)
    pure (aclRoot lane)

buildAtomicBinderPlan
    :: PresolutionBindingSnapshot p
    -> GenNodeId
    -> NodeId
    -> [(NodeId, NodeId)]
    -> [(NodeId, NodeId)]
    -> PresolutionM p AtomicBinderPlan
buildAtomicBinderPlan snapshot sourceOwner bodyId binderMetas binderArgs = do
    let c0 = pbsConstraint snapshot
        canonical = pbsCanonical snapshot
        body = canonical bodyId
        nodes = cNodes c0
    case lookupNodeIn nodes body of
        Nothing -> throwError (NodeLookupFailed body)
        Just _ -> pure ()
    metaSubstitution <- atomicStrictSubstitutionMap canonical binderMetas
    argumentSubstitution <- atomicStrictSubstitutionMap canonical binderArgs
    let plannedBinders =
            deduplicateCanonicalNodes canonical (map fst binderMetas)
        metaKeys = IntSet.fromAscList (IntMap.keys metaSubstitution)
        argumentKeys = IntSet.fromAscList (IntMap.keys argumentSubstitution)
        plannedKeys = metaKeys
    unless (argumentKeys == plannedKeys) $
        throwError $
            InternalError $
                "atomic recipe-argument substitution does not match the semantic binder plan: expected "
                    ++ show (map NodeId (IntSet.toAscList plannedKeys))
                    ++ ", got "
                    ++ show (map NodeId (IntSet.toAscList argumentKeys))
    let reachable =
            Traversal.reachableFromWithBounds
                canonical
                (lookupNodeIn nodes)
                body
        unreachableKeys = IntSet.difference plannedKeys reachable
    unless (IntSet.null unreachableKeys) $
        throwError $
            InternalError $
                "atomic semantic substitution does not match reachable instantiation binders; unreachable: "
                    ++ show (map NodeId (IntSet.toAscList unreachableKeys))
    projection <-
        projectExpansionBinders
            snapshot
            sourceOwner
            body
            plannedBinders
    let semanticKeys =
            IntSet.fromList
                (map (getNodeId . canonical) (ebpSemanticBinders projection))
        outsideSemanticLane =
            IntSet.difference plannedKeys semanticKeys
    unless (IntSet.null outsideSemanticLane) $
        throwError $
            InternalError $
                "atomic binder plan contains sources outside the semantic copy domain: "
                    ++ show
                        (map NodeId (IntSet.toAscList outsideSemanticLane))
    forM_ plannedBinders $ \binder ->
        case lookupNodeIn nodes binder of
            Just TyVar{}
                | not (VarStore.isEliminatedVar c0 binder) -> pure ()
            _ ->
                throwError $
                    InternalError $
                        "atomic binder plan contains a non-live variable: "
                            ++ show binder
    plannedMetas <-
        mapM
            (lookupPlannedReplacement "semantic meta" metaSubstitution)
            plannedBinders
    plannedArgs <-
        mapM
            (lookupPlannedReplacement "recipe argument" argumentSubstitution)
            plannedBinders
    pure
        AtomicBinderPlan
            { abpBinderMetas = zip plannedBinders plannedMetas
            , abpBinderArgs = zip plannedBinders plannedArgs
            , abpSourceKeys = plannedKeys
            }
  where
    lookupPlannedReplacement
        :: String
        -> IntMap NodeId
        -> NodeId
        -> PresolutionM q NodeId
    lookupPlannedReplacement label substitution source =
        case IntMap.lookup (getNodeId source) substitution of
            Just replacement -> pure replacement
            Nothing ->
                throwError $
                    InternalError $
                        "atomic binder plan lost "
                            ++ label
                            ++ " for "
                            ++ show source

deduplicateCanonicalNodes :: (NodeId -> NodeId) -> [NodeId] -> [NodeId]
deduplicateCanonicalNodes canonical = reverse . snd . foldl' step (IntSet.empty, [])
  where
    step (seen, nodes) node0 =
        let node = canonical node0
            key = getNodeId node
        in if IntSet.member key seen
            then (seen, nodes)
            else (IntSet.insert key seen, node : nodes)

validateSemanticBinderProjection
    :: Constraint p
    -> (NodeId -> NodeId)
    -> AtomicBinderPlan
    -> IntMap NodeId
    -> AtomicCopyLane
    -> PresolutionM q ()
validateSemanticBinderProjection c0 canonical plan metaSubstitution lane = do
    let encountered = aclEncounteredSubstitutions lane
        planned = abpSourceKeys plan
    unless (encountered == planned) $
        throwError $
            InternalError $
                "atomic semantic traversal did not consume exactly its binder plan: expected "
                    ++ show (map NodeId (IntSet.toAscList planned))
                    ++ ", encountered "
                    ++ show (map NodeId (IntSet.toAscList encountered))
    forM_ (abpBinderMetas plan) $ \(sourceBinder, meta) -> do
        let source = canonical sourceBinder
            metaC = canonical meta
        case lookupCopy source (aclCopyMap lane) of
            Just copied
                | canonical copied == metaC -> pure ()
            other ->
                throwError $
                    InternalError $
                        "atomic semantic trace lost active binder "
                            ++ show source
                            ++ " -> "
                            ++ show metaC
                            ++ ": "
                            ++ show other
        case VarStore.lookupVarBound c0 source of
            Nothing -> pure ()
            Just sourceBound0 -> do
                let sourceBound = canonical sourceBound0
                    expectedBound =
                        case atomicMappedBound canonical metaSubstitution (aclCopyMap lane) sourceBound of
                            Just mapped -> canonical mapped
                            Nothing -> sourceBound
                    actualBound =
                        atomicLookupPendingBound
                            canonical
                            metaC
                            (aclPendingBounds lane)
                unless (actualBound == Just expectedBound) $
                    throwError $
                        InternalError $
                            "atomic semantic traversal did not reuse its mapped lower bound for "
                                ++ show source
                                ++ ": expected "
                                ++ show expectedBound
                                ++ ", got "
                                ++ show actualBound

atomicDestinationGen
    :: PresolutionBindingSnapshot p
    -> NodeRef
    -> PresolutionM p NodeRef
atomicDestinationGen snapshot rootBinder = do
    path <- bindingSnapshotPathToRoot snapshot rootBinder
    case [gref | gref@GenRef{} <- path] of
        (gref : _) -> pure gref
        [] -> throwError (InternalError "copied expansion destination has no gen ancestor")

atomicStrictSubstitutionMap
    :: (NodeId -> NodeId)
    -> [(NodeId, NodeId)]
    -> PresolutionM p (IntMap NodeId)
atomicStrictSubstitutionMap canonical substitutions =
    foldM
        (\acc (source0, replacement0) ->
            let source = canonical source0
                replacement = canonical replacement0
                key = getNodeId source
            in case IntMap.lookup key acc of
                Nothing -> pure (IntMap.insert key replacement acc)
                Just existing
                    | existing == replacement -> pure acc
                    | otherwise -> throwError (CopySubstitutionConflict source existing replacement)
        )
        IntMap.empty
        substitutions

atomicMappedBound
    :: (NodeId -> NodeId)
    -> IntMap NodeId
    -> CopyMap
    -> NodeId
    -> Maybe NodeId
atomicMappedBound canonical substitution copyMap sourceBound =
    let sourceBoundC = canonical sourceBound
    in case IntMap.lookup (getNodeId sourceBoundC) substitution of
        Just replacement -> Just replacement
        Nothing -> lookupCopy sourceBoundC copyMap

atomicLookupPendingBound
    :: (NodeId -> NodeId)
    -> NodeId
    -> IntMap NodeId
    -> Maybe NodeId
atomicLookupPendingBound canonical variable pendingBounds =
    let variableC = canonical variable
    in foldl'
        (\found (rawKey, bound) ->
            if canonical (NodeId rawKey) == variableC
                then Just (canonical bound)
                else found
        )
        Nothing
        (IntMap.toList pendingBounds)

atomicInsertPendingBound
    :: (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> IntMap NodeId
    -> PresolutionM p (IntMap NodeId)
atomicInsertPendingBound canonical variable0 bound0 pendingBounds =
    let variable = canonical variable0
        bound = canonical bound0
        key = getNodeId variable
    in case IntMap.lookup key pendingBounds of
        Nothing -> pure (IntMap.insert key bound pendingBounds)
        Just existing
            | existing == bound -> pure pendingBounds
            | otherwise -> throwError (CopyPendingBoundConflict variable existing bound)

atomicMergePendingBounds
    :: (NodeId -> NodeId)
    -> IntMap NodeId
    -> IntMap NodeId
    -> PresolutionM p (IntMap NodeId)
atomicMergePendingBounds canonical pendingAcc pending =
    foldM
        (\acc (variableKey, bound) ->
            atomicInsertPendingBound canonical (NodeId variableKey) bound acc
        )
        pendingAcc
        (IntMap.toList pending)

buildAtomicCopyLane
    :: GenNodeId
    -> Bool
    -> PresolutionBindingSnapshot p
    -> NodeRef
    -> Bool
    -> NodeId
    -> [(NodeId, NodeId)]
    -> PresolutionM p AtomicCopyLane
buildAtomicCopyLane sourceOwner replaceFrontier snapshot rootBinder0 copySubstitutionBounds bodyId substList = do
    let c0 = pbsConstraint snapshot
        canonical = pbsCanonical snapshot
        bodyC = canonical bodyId
        nodes = cNodes c0
    case lookupNodeIn (cNodes c0) bodyC of
        Nothing -> throwError (NodeLookupFailed bodyC)
        Just _ -> pure ()

    (copyInterior0, frontierSet0) <-
        expansionCopySetsForOwnerWithSnapshot snapshot sourceOwner bodyId
    relocatedBoundInterior <-
        if replaceFrontier
            then pure IntSet.empty
            else
                ownerLocalRelocationInterior
                    snapshot
                    sourceOwner
                    rootBinder0
                    bodyC
    let bodyKey = getNodeId bodyC
        substSourceSet =
            IntSet.fromList
                [getNodeId (canonical source) | (source, _replacement) <- substList]
        copyInteriorSeed =
            IntSet.union copyInterior0 relocatedBoundInterior
        isDegenerate = not (IntSet.member bodyKey copyInteriorSeed)
        copyInterior
            | isDegenerate = IntSet.insert bodyKey copyInteriorSeed
            | otherwise = copyInteriorSeed
        frontierSetBase
            | replaceFrontier = frontierSet0
            | otherwise =
                structuralFrontierForInterior
                    nodes
                    canonical
                    copyInterior
                    bodyC
        frontierSetWithDegenerateRoot
            | isDegenerate = IntSet.insert bodyKey frontierSetBase
            | otherwise = frontierSetBase
        frontierSet =
            IntSet.difference frontierSetWithDegenerateRoot substSourceSet
        frontierForCopy
            | replaceFrontier = frontierSet
            | otherwise = IntSet.empty
    substAll <- atomicStrictSubstitutionMap canonical substList
    let lookupSourceNode
            :: NodeId
            -> StateT CopyState (PresolutionM q) (NodeId, TyNode)
        lookupSourceNode nid =
            let nidC = canonical nid
            in case lookupNodeIn nodes nidC of
                Just node -> pure (nidC, node)
                Nothing -> lift (throwError (NodeLookupFailed nidC))
        retainedWrapperKeys = retainedSchemeRootWrapperKeys c0 canonical
        subst =
            IntMap.filterWithKey
                (\key _ -> not (IntSet.member key retainedWrapperKeys))
                substAll
        initialState =
            CopyState
                { csCache = IntMap.empty
                , csCopyMap = IntMap.empty
                , csInterior = IntSet.empty
                , csPendingBounds = IntMap.empty
                , csEncounteredSubstitutions = IntSet.empty
                }
    (root, finalState) <-
        runStateT
            ( atomicCopyNode
                lookupSourceNode
                copyInterior
                frontierForCopy
                canonical
                subst
                copySubstitutionBounds
                bodyId
            )
            initialState
    let encounteredSubstitutions = csEncounteredSubstitutions finalState
        encounteredAliasMappings =
            IntMap.fromList
                [ (getNodeId source, canonical replacement)
                | (source, replacement) <- substList
                , IntSet.member
                    (getNodeId (canonical source))
                    encounteredSubstitutions
                ]
        finalCopyMap =
            IntMap.union
                (csCopyMap finalState)
                encounteredAliasMappings
    pure
        AtomicCopyLane
            { aclGenId = sourceOwner
            , aclSchemeRoot = bodyC
            , aclRoot = root
            , aclCopyMap = CopyMapping finalCopyMap
            , aclInterior = csInterior finalState
            , aclFrontier = frontierSet
            , aclFrontierForCopy = frontierForCopy
            , aclSubstitutionKeys = encounteredSubstitutions
            , aclEncounteredSubstitutions = encounteredSubstitutions
            , aclPendingBounds = csPendingBounds finalState
            , aclRootBinder = Canonicalize.canonicalRef canonical rootBinder0
            }

-- | Whether a node can be shared by a value constructed directly below the
-- supplied binder.  A parent on the binder's path is already ambient; a
-- sibling owner would make the new lower bound demand an after-the-fact Raise.
nodeIsAmbientAtBinder
    :: PresolutionBindingSnapshot p
    -> NodeRef
    -> NodeId
    -> PresolutionM p Bool
nodeIsAmbientAtBinder snapshot destinationBinder node0 = do
    destinationPath <-
        bindingSnapshotPathToRoot snapshot destinationBinder
    let canonical = pbsCanonical snapshot
        node = canonical node0
        quotientParents =
            Binding.qbpBindParents (pbsQuotient snapshot)
        destinationPathKeys =
            IntSet.fromList
                ( map
                    (nodeRefKey . Canonicalize.canonicalRef canonical)
                    destinationPath
                )
    pure $
        case
            IntMap.lookup
                (nodeRefKey (typeRef node))
                quotientParents
        of
            Nothing -> True
            Just (parent, _flag) ->
                IntSet.member
                    ( nodeRefKey
                        (Canonicalize.canonicalRef canonical parent)
                    )
                    destinationPathKeys

-- | A forall-bound projection is relocated below a fresh destination binder.
-- Nodes that are merely exterior to the source body are shareable only when
-- their current owner is already on that destination path.  Source-owner-local
-- siblings would otherwise require a later Raise after the copied bound is
-- installed.  Include those siblings in the copy domain from the outset,
-- while continuing to share genuinely ambient nodes.
ownerLocalRelocationInterior
    :: PresolutionBindingSnapshot p
    -> GenNodeId
    -> NodeRef
    -> NodeId
    -> PresolutionM p IntSet.IntSet
ownerLocalRelocationInterior snapshot sourceOwner destinationBinder body = do
    ownerInterior <-
        bindingSnapshotInteriorOf snapshot (genRef sourceOwner)
    destinationPath <-
        bindingSnapshotPathToRoot snapshot destinationBinder
    let canonical = pbsCanonical snapshot
        constraint = pbsConstraint snapshot
        nodes = cNodes constraint
        quotientParents =
            Binding.qbpBindParents (pbsQuotient snapshot)
        destinationPathKeys =
            IntSet.fromList
                ( map
                    (nodeRefKey . Canonicalize.canonicalRef canonical)
                    destinationPath
                )
        reachable =
            Traversal.reachableFromWithBounds
                canonical
                (lookupNodeIn nodes)
                body
        ownerLocal node =
            IntSet.member
                (nodeRefKey (typeRef node))
                ownerInterior
        alreadyAmbient node =
            case
                IntMap.lookup
                    (nodeRefKey (typeRef node))
                    quotientParents
            of
                Nothing -> True
                Just (parent, _flag) ->
                    IntSet.member
                        ( nodeRefKey
                            (Canonicalize.canonicalRef canonical parent)
                        )
                        destinationPathKeys
    pure $
        IntSet.fromList
            [ getNodeId node
            | key <- IntSet.toAscList reachable
            , let node = canonical (NodeId key)
            , ownerLocal node
            , not (alreadyAmbient node)
            ]

structuralFrontierForInterior
    :: NodeMap TyNode
    -> (NodeId -> NodeId)
    -> IntSet.IntSet
    -> NodeId
    -> IntSet.IntSet
structuralFrontierForInterior nodes canonical interior body =
    IntSet.fromList
        [ getNodeId child
        | sourceKey <- IntSet.toAscList interior
        , Just source <- [lookupNodeIn nodes (NodeId sourceKey)]
        , child0 <- structuralChildrenWithBounds source
        , let child = canonical child0
        , IntSet.member (getNodeId child) reachable
        , IntSet.notMember (getNodeId child) interior
        ]
  where
    reachable =
        Traversal.reachableFromWithBounds
            canonical
            (lookupNodeIn nodes)
            body

atomicRecordNew :: NodeId -> StateT CopyState (PresolutionM p) ()
atomicRecordNew freshId =
    modify $ \st -> st {csInterior = IntSet.insert (getNodeId freshId) (csInterior st)}

atomicRecordCopy :: NodeId -> NodeId -> StateT CopyState (PresolutionM p) ()
atomicRecordCopy source copied =
    modify $ \st -> st {csCopyMap = IntMap.insert (getNodeId source) copied (csCopyMap st)}

atomicRecordPendingBound
    :: (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> StateT CopyState (PresolutionM p) ()
atomicRecordPendingBound canonical variable bound = do
    pending <- gets csPendingBounds
    pending' <- lift (atomicInsertPendingBound canonical variable bound pending)
    modify $ \st -> st {csPendingBounds = pending'}

atomicCopyNode
    :: (NodeId -> StateT CopyState (PresolutionM p) (NodeId, TyNode))
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> (NodeId -> NodeId)
    -> IntMap NodeId
    -> Bool
    -> NodeId
    -> StateT CopyState (PresolutionM p) NodeId
atomicCopyNode lookupSourceNode copyInterior frontierSet canonical subst copySubstitutionBounds nid = do
    (nidC, node) <- lookupSourceNode nid
    mbCached <- gets (IntMap.lookup (getNodeId nidC) . csCache)
    case mbCached of
        Just existing -> do
            atomicRecordCopy nid existing
            atomicRecordCopy nidC existing
            pure existing
        Nothing ->
            case node of
                TyExp{tnBody = body} ->
                    recurse body
                _ -> do
                    let key = getNodeId nidC
                    case IntMap.lookup key subst of
                        Just replacement -> do
                            cache nidC replacement
                            atomicRecordCopy nid replacement
                            atomicRecordCopy nidC replacement
                            atomicRecordNew replacement
                            modify $ \st ->
                                st
                                    { csEncounteredSubstitutions =
                                        IntSet.insert key (csEncounteredSubstitutions st)
                                    }
                            when copySubstitutionBounds $
                                case node of
                                    TyVar{tnBound = Just boundRoot} -> do
                                        copiedBound <- recurse boundRoot
                                        atomicRecordPendingBound canonical replacement copiedBound
                                    _ -> pure ()
                            pure replacement
                        Nothing
                            | IntSet.member key frontierSet -> do
                                freshId <- lift createFreshNodeId
                                cache nidC freshId
                                atomicRecordCopy nid freshId
                                atomicRecordCopy nidC freshId
                                lift (registerNode freshId (TyBottom freshId))
                                pure freshId
                            | not (IntSet.member key copyInterior) -> pure nidC
                            | otherwise -> do
                                freshId <- lift createFreshNodeId
                                cache nidC freshId
                                atomicRecordCopy nid freshId
                                atomicRecordCopy nidC freshId
                                atomicRecordNew freshId
                                lift (registerNode freshId (atomicPlaceholder freshId node))
                                newNode <-
                                    case node of
                                        TyArrow{tnDom = dom, tnCod = cod} ->
                                            TyArrow freshId <$> recurse dom <*> recurse cod
                                        TyForall{tnBody = body} ->
                                            TyForall freshId <$> recurse body
                                        TyMu{tnBody = body} ->
                                            TyMu freshId <$> recurse body
                                        TyVar{tnBound = mbBound} -> do
                                            copiedBound <- traverse recurse mbBound
                                            forM_ copiedBound (atomicRecordPendingBound canonical freshId)
                                            pure (TyVar freshId Nothing)
                                        TyBottom{} -> pure (TyBottom freshId)
                                        TyBase{tnBaseIdentity = identity, tnBase = base} ->
                                            pure (TyBase freshId identity base)
                                        TyCon{tnConIdentity = identity, tnCon = con, tnArgs = args} ->
                                            TyCon freshId identity con <$> traverse recurse args
                                        TyVarApp{tnVarHead = headNode, tnArgs = args} ->
                                            TyVarApp freshId <$> recurse headNode <*> traverse recurse args
                                lift (registerNode freshId newNode)
                                pure freshId
  where
    recurse =
        atomicCopyNode
            lookupSourceNode
            copyInterior
            frontierSet
            canonical
            subst
            copySubstitutionBounds
    cache
        :: NodeId
        -> NodeId
        -> StateT CopyState (PresolutionM q) ()
    cache source copied =
        modify $ \st -> st {csCache = IntMap.insert (getNodeId source) copied (csCache st)}

atomicPlaceholder :: NodeId -> TyNode -> TyNode
atomicPlaceholder freshId node =
    case node of
        TyArrow{tnDom = dom, tnCod = cod} -> TyArrow freshId dom cod
        TyForall{tnBody = body} -> TyForall freshId body
        TyMu{tnBody = body} -> TyMu freshId body
        TyVar{} -> TyVar freshId Nothing
        TyBottom{} -> TyBottom freshId
        TyBase{tnBaseIdentity = identity, tnBase = base} -> TyBase freshId identity base
        TyCon{tnConIdentity = identity, tnCon = con, tnArgs = args} ->
            TyCon freshId identity con args
        TyVarApp{tnVarHead = headNode, tnArgs = args} ->
            TyVarApp freshId headNode args
        TyExp{tnBody = body} -> TyExp freshId (ExpVarId (-1)) body

planAtomicCopyLane
    :: PresolutionBindingSnapshot p
    -> (BindParents, IntSet.IntSet)
    -> AtomicCopyLane
    -> PresolutionM p (BindParents, IntSet.IntSet)
planAtomicCopyLane snapshot (edits0, projected0) lane = do
    let canonical = pbsCanonical snapshot
        qbp = Binding.qbpBindParents (pbsQuotient snapshot)
        schemeRootNode = canonical (aclSchemeRoot lane)
        copyRoot = canonical (aclRoot lane)
        rootBinder = Canonicalize.canonicalRef canonical (aclRootBinder lane)
    copyMap <- atomicStrictCanonicalCopyMap canonical (aclCopyMap lane)
    edits1 <-
        atomicInsertBindingEdit canonical edits0 (typeRef copyRoot) (rootBinder, BindFlex)
    edits2 <-
        foldM
            (\edits sourceKey ->
                let source = canonical (NodeId sourceKey)
                in case IntMap.lookup (getNodeId source) copyMap of
                    Nothing -> pure edits
                    Just copied -> do
                        mbSourceParent <- bindingSnapshotLookupBindParent snapshot (typeRef source)
                        let copiedRef = typeRef copied
                            projectedParent =
                                case mbSourceParent of
                                    Just (TypeRef parent, _flag) ->
                                        case IntMap.lookup (getNodeId (canonical parent)) copyMap of
                                            Just parentCopy
                                                | typeRef parentCopy /= copiedRef -> typeRef parentCopy
                                            _ -> rootBinder
                                    _ -> rootBinder
                        atomicInsertBindingEdit
                            canonical
                            edits
                            copiedRef
                            (projectedParent, BindFlex)
            )
            edits1
            (IntSet.toList (aclFrontierForCopy lane))
    edits3 <-
        foldM
            (planCopiedParent canonical qbp copyMap schemeRootNode copyRoot rootBinder)
            edits2
            (IntMap.toList copyMap)
    let projected =
            IntSet.unions
                [ projected0
                , IntSet.singleton (nodeRefKey (typeRef copyRoot))
                , IntSet.fromList
                    [ nodeRefKey (typeRef copied)
                    | sourceKey <- IntSet.toList (aclFrontierForCopy lane)
                    , let source = canonical (NodeId sourceKey)
                    , Just copied <- [IntMap.lookup (getNodeId source) copyMap]
                    ]
                ]
    pure (edits3, projected)
  where
    planCopiedParent canonical qbp copyMap schemeRootNode copyRoot rootBinder edits (sourceKey, copied) = do
        let source = canonical (NodeId sourceKey)
            isRoot = source == schemeRootNode
            isFrontier = IntSet.member (getNodeId source) (aclFrontierForCopy lane)
        if isRoot || isFrontier
            then pure edits
            else
                if IntSet.member (getNodeId source) (aclSubstitutionKeys lane)
                    then pure edits
                    else
                        case IntMap.lookup (nodeRefKey (typeRef copied)) qbp of
                            Just _ -> pure edits
                            Nothing -> do
                                mbParent <- bindingSnapshotLookupBindParent snapshot (typeRef source)
                                case mbParent of
                                    Nothing ->
                                        throwError (BindingTreeError (MissingBindParent (typeRef source)))
                                    Just (parentRef, flag) -> do
                                        let parentRefC = Canonicalize.canonicalRef canonical parentRef
                                            parent =
                                                case parentRefC of
                                                    GenRef parentGid
                                                        | parentGid == aclGenId lane -> typeRef copyRoot
                                                        | otherwise -> rootBinder
                                                    TypeRef parentId
                                                        | canonical parentId == schemeRootNode -> typeRef copyRoot
                                                        | otherwise ->
                                                            maybe
                                                                (typeRef copyRoot)
                                                                typeRef
                                                                (IntMap.lookup (getNodeId (canonical parentId)) copyMap)
                                            childRef = typeRef copied
                                            parentFinal
                                                | parent == childRef = typeRef copyRoot
                                                | otherwise = parent
                                        atomicInsertBindingEdit canonical edits childRef (parentFinal, flag)

atomicStrictCanonicalCopyMap
    :: (NodeId -> NodeId)
    -> CopyMap
    -> PresolutionM p (IntMap NodeId)
atomicStrictCanonicalCopyMap canonical (CopyMapping mapping) =
    foldM
        (\acc (rawKey, copied0) ->
            let source = canonical (NodeId rawKey)
                copied = canonical copied0
                key = getNodeId source
            in case IntMap.lookup key acc of
                Nothing -> pure (IntMap.insert key copied acc)
                Just existing
                    | existing == copied -> pure acc
                    | otherwise -> throwError (CopySubstitutionConflict source existing copied)
        )
        IntMap.empty
        (IntMap.toList mapping)

atomicInsertBindingEdit
    :: (NodeId -> NodeId)
    -> BindParents
    -> NodeRef
    -> (NodeRef, BindFlag)
    -> PresolutionM p BindParents
atomicInsertBindingEdit canonical edits child0 (parent0, flag) =
    let child = Canonicalize.canonicalRef canonical child0
        parent = Canonicalize.canonicalRef canonical parent0
        key = nodeRefKey child
    in case IntMap.lookup key edits of
        Nothing -> pure (IntMap.insert key (parent, flag) edits)
        Just (existingParent, existingFlag)
            | existingParent == parent ->
                pure (IntMap.insert key (parent, max existingFlag flag) edits)
            | otherwise -> throwError (CopyBindingParentConflict child existingParent parent)

commitAtomicCopiedProjection
    :: PresolutionBindingSnapshot p
    -> NodeId
    -> IntSet.IntSet
    -> BindParents
    -> IntMap NodeId
    -> PresolutionM p ()
commitAtomicCopiedProjection snapshot semanticRoot projectedRoots edits pendingBounds = do
    st <- get
    let canonical = pbsCanonical snapshot
        cBefore = psConstraint st
        existingAssignments =
            Binding.qbpRawParentAssignments (pbsQuotient snapshot)
    validateAtomicExistingParentAssignments existingAssignments edits
    -- A frozen edge snapshot intentionally survives construction-time binding
    -- edits.  Atomic copy commits always write canonical child keys, so this
    -- point lookup catches an earlier commit in the same edge without turning
    -- each copy back into a scan of the complete parent map.
    validateAtomicCurrentParentAssignments
        canonical
        (cBindParents cBefore)
        edits
    let
        cWithParents = cBefore {cBindParents = IntMap.union edits (cBindParents cBefore)}
    cWithBounds <-
        foldM
            (installPendingBound canonical)
            cWithParents
            (IntMap.toAscList pendingBounds)
    forM_ (IntMap.toAscList pendingBounds) $ \(variableKey, boundRoot) ->
        case validateLowerBoundGraph canonical (cNodes cWithBounds) (NodeId variableKey) boundRoot of
            Left err -> throwError err
            Right () -> pure ()
    validateAtomicPendingBoundScopes
        canonical
        semanticRoot
        cWithBounds
        pendingBounds
    forM_ (IntMap.toAscList edits) $ \(childKey, (parent, _flag)) -> do
        let child = nodeRefFromKey childKey
        case Binding.bindingPathToRoot cWithBounds child of
            Left err -> throwError (BindingTreeError err)
            Right _ -> pure ()
        unless (IntSet.member childKey projectedRoots || Binding.isUpper cWithBounds parent child) $
            throwError (BindingTreeError (ParentNotUpper child parent))
    let dirtyTypes =
            IntSet.fromList
                [ key
                | (variableKey, boundRoot) <- IntMap.toList pendingBounds
                , key <- [variableKey, getNodeId boundRoot]
                ]
        -- Parent edits are the complete write set of this transaction; bound
        -- installation changes only cNodes.  Derive invalidation from that
        -- construction-owned write set instead of diffing two whole maps.
        dirtyBindRefs =
            IntMap.foldlWithKey'
                (\dirty childKey parentInfo ->
                    if IntMap.lookup childKey (cBindParents cBefore) == Just parentInfo
                        then dirty
                        else IntSet.insert childKey dirty
                )
                IntSet.empty
                edits
        stWithTypes
            | IntSet.null dirtyTypes = st
            | otherwise = modifyConstraintDirtyTypesState dirtyTypes (const cWithBounds) st
        stFinal
            | IntSet.null dirtyBindRefs = stWithTypes
            | otherwise = setConstraintDirtyBindRefsState dirtyBindRefs cWithBounds stWithTypes
    put stFinal
  where
    installPendingBound
        :: (NodeId -> NodeId)
        -> Constraint q
        -> (Int, NodeId)
        -> PresolutionM r (Constraint q)
    installPendingBound canonical constraint (variableKey, boundRoot0) =
        let variable = canonical (NodeId variableKey)
            boundRoot = canonical boundRoot0
        in case lookupNodeIn (cNodes constraint) variable of
            Nothing -> throwError (NodeLookupFailed variable)
            Just TyVar{tnBound = Nothing} ->
                pure (VarStore.setVarBound variable (Just boundRoot) constraint)
            Just TyVar{tnBound = Just existing0}
                | canonical existing0 == boundRoot -> pure constraint
                | otherwise ->
                    throwError
                        (CopyPendingBoundConflict variable (canonical existing0) boundRoot)
            Just node -> throwError (BoundTargetNotTyVar variable node)

validateAtomicExistingParentAssignments
    :: IntMap [(NodeRef, BindFlag)]
    -> BindParents
    -> PresolutionM q ()
validateAtomicExistingParentAssignments existingAssignments edits = do
    forM_ (IntMap.toAscList edits) $ \(childKey, (plannedParent0, plannedFlag)) -> do
        let child = nodeRefFromKey childKey
        forM_ (IntMap.findWithDefault [] childKey existingAssignments) $ \(existingParent, existingFlag) -> do
            validateOne child plannedParent0 plannedFlag existingParent existingFlag
  where
    validateOne
        :: NodeRef
        -> NodeRef
        -> BindFlag
        -> NodeRef
        -> BindFlag
        -> PresolutionM r ()
    validateOne child plannedParent plannedFlag existingParent existingFlag = do
        if existingParent /= plannedParent
            then
                throwError
                    (CopyBindingParentConflict child existingParent plannedParent)
            else
                unless (existingFlag == plannedFlag) $
                    throwError $
                        InternalError $
                            "atomic copied parent flag conflicts with existing ownership for "
                                ++ show child
                                ++ ": existing "
                                ++ show existingFlag
                                ++ ", planned "
                                ++ show plannedFlag

validateAtomicCurrentParentAssignments
    :: (NodeId -> NodeId)
    -> BindParents
    -> BindParents
    -> PresolutionM q ()
validateAtomicCurrentParentAssignments canonical currentParents edits =
    forM_ (IntMap.toAscList edits) $ \(childKey, (plannedParent0, plannedFlag)) ->
        forM_ (IntMap.lookup childKey currentParents) $ \(existingParent0, existingFlag) -> do
            let child = nodeRefFromKey childKey
                plannedParent = Canonicalize.canonicalRef canonical plannedParent0
                existingParent = Canonicalize.canonicalRef canonical existingParent0
            if existingParent /= plannedParent
                then
                    throwError
                        (CopyBindingParentConflict child existingParent plannedParent)
                else
                    unless (existingFlag == plannedFlag) $
                        throwError $
                            InternalError $
                                "atomic copied parent flag conflicts with current ownership for "
                                    ++ show child
                                    ++ ": existing "
                                    ++ show existingFlag
                                    ++ ", planned "
                                    ++ show plannedFlag

validateAtomicPendingBoundScopes
    :: (NodeId -> NodeId)
    -> NodeId
    -> Constraint p
    -> IntMap NodeId
    -> PresolutionM q ()
validateAtomicPendingBoundScopes canonical semanticRoot constraint pendingBounds = do
    offending <-
        foldM
            (\acc (variableKey, boundRoot) -> do
                frontier <-
                    case atomicFreeBoundFrontier canonical constraint (NodeId variableKey) boundRoot of
                        Left err -> throwError (BindingTreeError err)
                        Right nodes -> pure nodes
                invalid <-
                    foldM
                        (\nodes frontierNode -> do
                            valid <-
                                case atomicBoundFrontierAlreadyScoped canonical constraint (NodeId variableKey) frontierNode of
                                    Left err -> throwError (BindingTreeError err)
                                    Right result -> pure result
                            pure $
                                if valid
                                    then nodes
                                    else frontierNode : nodes
                        )
                        []
                        frontier
                pure (reverse invalid ++ acc)
            )
            []
            (IntMap.toAscList pendingBounds)
    unless (null offending) $
        throwError (CopyBindingScopeRepairRequired semanticRoot (reverse offending))

atomicFreeBoundFrontier
    :: (NodeId -> NodeId)
    -> Constraint p
    -> NodeId
    -> NodeId
    -> Either BindingError [NodeId]
atomicFreeBoundFrontier canonical constraint variable0 boundRoot0 = do
    let nodes = cNodes constraint
        variable = canonical variable0
        boundRoot = canonical boundRoot0
        reachable =
            Traversal.reachableFromWithBounds
                canonical
                (lookupNodeIn nodes)
                boundRoot
        candidates = map NodeId (IntSet.toAscList reachable)
        variableRef = typeRef variable
    freeNodesRev <-
        foldM
            (\accFreeNodes node -> do
                path <- Binding.bindingPathToRoot constraint (typeRef node)
                pure $
                    if variableRef `elem` path
                        then accFreeNodes
                        else node : accFreeNodes
            )
            []
            candidates
    let unscopedNodes = reverse freeNodesRev
        freeKeys = IntSet.fromList (map getNodeId unscopedNodes)
        parentIsFree node =
            case Binding.lookupBindParent constraint (typeRef node) of
                Just (TypeRef parent, _) ->
                    IntSet.member (getNodeId (canonical parent)) freeKeys
                _ -> False
    pure [node | node <- unscopedNodes, not (parentIsFree node)]

atomicBoundFrontierAlreadyScoped
    :: (NodeId -> NodeId)
    -> Constraint p
    -> NodeId
    -> NodeId
    -> Either BindingError Bool
atomicBoundFrontierAlreadyScoped canonical constraint variable0 frontier0 = do
    let variable = canonical variable0
        frontier = canonical frontier0
        canonicalRef = Canonicalize.canonicalRef canonical
        mbVariableParent =
            fmap
                (\(parent, flag) -> (canonicalRef parent, flag))
                (Binding.lookupBindParent constraint (typeRef variable))
        mbFrontierParent =
            fmap
                (\(parent, flag) -> (canonicalRef parent, flag))
                (Binding.lookupBindParent constraint (typeRef frontier))
    frontierPath0 <- Binding.bindingPathToRoot constraint (typeRef frontier)
    let frontierPath = map canonicalRef frontierPath0
    case mbVariableParent of
        Nothing -> pure (maybe True (const False) mbFrontierParent)
        Just (target, _variableFlag)
            | target `elem` frontierPath ->
                pure $
                    case mbFrontierParent of
                        Just (currentParent, _frontierFlag) -> currentParent == target
                        Nothing -> False
            | otherwise ->
                case mbFrontierParent of
                    Nothing -> pure True
                    Just (currentParent, _frontierFlag) -> do
                        targetPath0 <- Binding.bindingPathToRoot constraint target
                        let targetPath = map canonicalRef targetPath0
                        pure (currentParent `elem` targetPath)

instantiateSchemeWithMode
    :: Bool
    -> NodeId
    -> [(NodeId, NodeId)]
    -> PresolutionM p (NodeId, CopyMap, IntSet.IntSet, IntSet.IntSet)
instantiateSchemeWithMode replaceFrontier bodyId substList = do
    snapshot <- getBindingSnapshot
    instantiateSchemeWithModeSnapshot replaceFrontier snapshot bodyId substList

instantiateSchemeWithModeSnapshot
    :: Bool
    -> PresolutionBindingSnapshot p
    -> NodeId
    -> [(NodeId, NodeId)]
    -> PresolutionM p (NodeId, CopyMap, IntSet.IntSet, IntSet.IntSet)
instantiateSchemeWithModeSnapshot replaceFrontier snapshot bodyId substList = do
    let c0 = pbsConstraint snapshot
        canonical = pbsCanonical snapshot

    let bodyC = canonical bodyId
    case lookupNodeIn (cNodes c0) bodyC of
        Nothing -> throwError (NodeLookupFailed bodyC)
        Just _ -> pure ()

    -- Paper (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.2):
    -- expansion copies nodes in I^s(g) and F^s(g) that are reachable from s,
    -- then replaces frontier copies with ⊥ and adds frontier unification edges.
    (gid, copyInterior0, frontierSet0) <- expansionCopySetsWithSnapshot snapshot bodyId
    let bodyKey = getNodeId bodyC
        substSourceSet =
            IntSet.fromList
                [ getNodeId (canonical source)
                | (source, _replacement) <- substList
                ]
        isDegenerate = not (IntSet.member bodyKey copyInterior0)
        copyInterior =
            if isDegenerate
                then IntSet.insert bodyKey copyInterior0
                else copyInterior0
        frontierSetWithDegenerateRoot =
            if isDegenerate
                then IntSet.insert bodyKey frontierSet0
                else frontierSet0
        -- A quantified variable replaced by the instantiation substitution is
        -- owned by the fresh binder meta, not by the shared frontier.  Keeping
        -- it in the frontier would later unify that meta back into the source
        -- let-scheme and let one use specialize every other use.
        frontierSet =
            IntSet.difference frontierSetWithDegenerateRoot substSourceSet
        frontierForCopy =
            if replaceFrontier
                then frontierSet
                else IntSet.empty

    substAll <- strictCanonicalSubstitutionMap canonical substList
    let nodes = cNodes c0
        lookupSourceNode :: NodeId -> StateT CopyState (PresolutionM p) (NodeId, TyNode)
        lookupSourceNode nid =
            let nidC = canonical nid
            in case lookupNodeIn nodes nidC of
                Just node -> pure (nidC, node)
                Nothing -> lift $ throwError (NodeLookupFailed nidC)
        schemeRoots =
            IntSet.fromList
                [ getNodeId (canonical r)
                | gen <- NodeAccess.allGenNodes c0
                , r <- gnSchemes gen
                ]
        isSchemeRootWrapper nid =
            let nidC = canonical nid
                key = getNodeId nidC
            in IntSet.member key schemeRoots &&
                case lookupNodeIn nodes nidC of
                    Just TyVar{ tnBound = Just _ } -> True
                    _ -> False
        subst =
            IntMap.filterWithKey
                (\k _ -> not (isSchemeRootWrapper (NodeId k)))
                substAll
        initialCopyMap =
            IntMap.union
                substAll
                ( IntMap.fromList
                    [ (getNodeId source, canonical replacement)
                    | (source, replacement) <- substList
                    ]
                )
        st0 =
            CopyState
                { csCache = IntMap.empty
                , csCopyMap = initialCopyMap
                , csInterior = IntSet.empty
                , csPendingBounds = IntMap.empty
                , csEncounteredSubstitutions = IntSet.empty
                }
    (root, st1) <- runStateT (copyNode lookupSourceNode copyInterior frontierForCopy canonical subst bodyId) st0
    let cmap = CopyMapping (csCopyMap st1)
        interior = csInterior st1
    let substKeys =
            IntSet.fromList
                [ getNodeId (canonical source)
                | (source, _replacement) <- substList
                ]
    resetBindingsForCopies
        snapshot
        gid
        bodyC
        root
        frontierForCopy
        cmap
        substKeys
        (csEncounteredSubstitutions st1)
        (csPendingBounds st1)
    pure (root, cmap, interior, frontierSet)
  where
    strictCanonicalSubstitutionMap
        :: (NodeId -> NodeId)
        -> [(NodeId, NodeId)]
        -> PresolutionM q (IntMap NodeId)
    strictCanonicalSubstitutionMap canonical substitutions =
        foldM
            (\acc (source0, replacement0) ->
                let source = canonical source0
                    replacement = canonical replacement0
                    key = getNodeId source
                in case IntMap.lookup key acc of
                    Nothing -> pure (IntMap.insert key replacement acc)
                    Just existing
                        | existing == replacement -> pure acc
                        | otherwise ->
                            throwError
                                (CopySubstitutionConflict source existing replacement)
            )
            IntMap.empty
            substitutions

    resetBindingsForCopies
        :: PresolutionBindingSnapshot p
        -> GenNodeId
        -> NodeId
        -> NodeId
        -> IntSet.IntSet
        -> CopyMap
        -> IntSet.IntSet
        -> IntSet.IntSet
        -> IntMap NodeId
        -> PresolutionM p ()
    resetBindingsForCopies snapshot0 gid schemeRootId copyRoot frontierSet cmap0 substKeys encounteredSubstitutions pendingBounds = do
        let canonical = pbsCanonical snapshot0
            qbp = Binding.qbpBindParents (pbsQuotient snapshot0)
        cmap <- strictCanonicalCopyMap canonical cmap0
        let schemeRootC = canonical schemeRootId
            copyRootC = canonical copyRoot
            rootBinder = genRef gid
        rootBinderPath <- bindingSnapshotPathToRoot snapshot0 rootBinder
        substitutionDestination <-
            case [gref | gref@GenRef{} <- rootBinderPath] of
                (gref : _) -> pure gref
                [] ->
                    throwError
                        (InternalError "copied expansion destination has no gen ancestor")
        rootEdits <-
            insertBindingEdit
                canonical
                IntMap.empty
                (typeRef copyRootC)
                (rootBinder, BindFlex)
        frontierEdits <-
            foldM
                (\edits nidInt ->
                    let nidC = canonical (NodeId nidInt)
                    in case IntMap.lookup (getNodeId nidC) cmap of
                        Nothing -> pure edits
                        Just copy ->
                            insertBindingEdit
                                canonical
                                edits
                                (typeRef copy)
                                (rootBinder, BindFlex)
                )
                rootEdits
                (IntSet.toList frontierSet)
        completeEdits <-
            foldM
                ( planCopiedParent
                    canonical
                    qbp
                    cmap
                    schemeRootC
                    copyRootC
                    rootBinder
                    substitutionDestination
                )
                frontierEdits
                (IntMap.toList cmap)
        let projectedDestinationChildren =
                IntSet.insert
                    (nodeRefKey (typeRef copyRootC))
                    ( IntSet.fromList
                        [ nodeRefKey (typeRef copy)
                        | nidInt <- IntSet.toList frontierSet
                        , let nidC = canonical (NodeId nidInt)
                        , Just copy <- [IntMap.lookup (getNodeId nidC) cmap]
                        ]
                    )
        commitCopiedProjection
            canonical
            copyRootC
            projectedDestinationChildren
            completeEdits
            pendingBounds

      where
        planCopiedParent canonical qbp cmap schemeRootC copyRootC rootBinder substitutionDestination edits (origKey, copy) = do
            let orig = NodeId origKey
                origC = canonical orig
                isRoot = origC == schemeRootC
                isFrontier = IntSet.member (getNodeId origC) frontierSet
            if isRoot || isFrontier
                then pure edits
                else
                if IntSet.member origKey substKeys
                    then
                        let childRef = typeRef copy
                            copyRootRef = typeRef copyRootC
                            parentFinal
                                | IntSet.member origKey encounteredSubstitutions
                                , childRef /= copyRootRef = copyRootRef
                                | otherwise = substitutionDestination
                        in insertBindingEdit
                            canonical
                            edits
                            childRef
                            (parentFinal, BindFlex)
                    else
                        case IntMap.lookup (nodeRefKey (typeRef copy)) qbp of
                            Just _ -> pure edits
                            Nothing -> do
                                mbParent <- bindingSnapshotLookupBindParent snapshot0 (typeRef origC)
                                case mbParent of
                                    Nothing ->
                                        throwError (BindingTreeError (MissingBindParent (typeRef origC)))
                                    Just (parentRef, flag) -> do
                                        let parentRefC = Canonicalize.canonicalRef canonical parentRef
                                        parentFinal0 <- case parentRefC of
                                            GenRef parentGid
                                                | parentGid == gid ->
                                                    pure (typeRef copyRootC)
                                                | otherwise ->
                                                    pure rootBinder
                                            TypeRef pid
                                                | canonical pid == schemeRootC ->
                                                    pure (typeRef copyRootC)
                                            TypeRef pid ->
                                                case IntMap.lookup (getNodeId (canonical pid)) cmap of
                                                    Just parentCopy -> pure (typeRef parentCopy)
                                                    Nothing -> pure (typeRef copyRootC)
                                        let childRef = typeRef copy
                                            parentFinal1 =
                                                if parentFinal0 == childRef
                                                    then typeRef copyRootC
                                                    else parentFinal0
                                        insertBindingEdit
                                            canonical
                                            edits
                                            childRef
                                            (parentFinal1, flag)

    strictCanonicalCopyMap
        :: (NodeId -> NodeId)
        -> CopyMap
        -> PresolutionM q (IntMap NodeId)
    strictCanonicalCopyMap canonical (CopyMapping mapping) =
        foldM
            (\acc (rawKey, copy0) ->
                let source = canonical (NodeId rawKey)
                    copy = canonical copy0
                    key = getNodeId source
                in case IntMap.lookup key acc of
                    Nothing -> pure (IntMap.insert key copy acc)
                    Just existing
                        | existing == copy -> pure acc
                        | otherwise ->
                            throwError
                                (CopySubstitutionConflict source existing copy)
            )
            IntMap.empty
            (IntMap.toList mapping)

    insertBindingEdit
        :: (NodeId -> NodeId)
        -> BindParents
        -> NodeRef
        -> (NodeRef, BindFlag)
        -> PresolutionM q BindParents
    insertBindingEdit canonical edits child0 (parent0, flag) =
        let child = Canonicalize.canonicalRef canonical child0
            parent = Canonicalize.canonicalRef canonical parent0
            key = nodeRefKey child
        in case IntMap.lookup key edits of
            Nothing -> pure (IntMap.insert key (parent, flag) edits)
            Just (existingParent, existingFlag)
                | existingParent == parent ->
                    pure (IntMap.insert key (parent, max existingFlag flag) edits)
                | otherwise ->
                    throwError
                        (CopyBindingParentConflict child existingParent parent)

    commitCopiedProjection
        :: (NodeId -> NodeId)
        -> NodeId
        -> IntSet.IntSet
        -> BindParents
        -> IntMap NodeId
        -> PresolutionM q ()
    commitCopiedProjection canonical copyRoot projectedDestinationChildren edits pendingBounds = do
        st <- get
        canonicalPending <-
            atomicMergePendingBounds canonical IntMap.empty pendingBounds
        let cBefore = psConstraint st
            plannedParents = IntMap.union edits (cBindParents cBefore)
            cWithParents = cBefore {cBindParents = plannedParents}
        cWithBounds <-
            foldM
                (\c (variableKey, boundRoot) ->
                    let variable = NodeId variableKey
                    in case lookupNodeIn (cNodes c) variable of
                        Nothing -> throwError (NodeLookupFailed variable)
                        Just TyVar{tnBound = Nothing} ->
                            pure (VarStore.setVarBound variable (Just boundRoot) c)
                        Just TyVar{tnBound = Just existing}
                            | canonical existing == boundRoot -> pure c
                            | otherwise ->
                                throwError
                                    (CopyPendingBoundConflict variable (canonical existing) boundRoot)
                        Just node -> throwError (BoundTargetNotTyVar variable node)
                )
                cWithParents
                (IntMap.toAscList canonicalPending)
        forM_ (IntMap.toAscList canonicalPending) $ \(variableKey, boundRoot) ->
            case
                validateLowerBoundGraph
                    canonical
                    (cNodes cWithBounds)
                    (NodeId variableKey)
                    boundRoot
            of
                Left err -> throwError err
                Right () -> pure ()
        (cScoped, raiseTrace) <-
            case BoundScope.repairAllVarBoundScopes canonical cWithBounds of
                Left err -> throwError (BindingTreeError err)
                Right result -> pure result
        unless (null raiseTrace) $
            throwError (CopyBindingScopeRepairRequired copyRoot raiseTrace)
        forM_ (IntMap.toAscList edits) $ \(childKey, (parent, _flag)) -> do
            let child = nodeRefFromKey childKey
            case Binding.bindingPathToRoot cScoped child of
                Left err -> throwError (BindingTreeError err)
                Right _ -> pure ()
            unless
                ( IntSet.member childKey projectedDestinationChildren
                    || Binding.isUpper cScoped parent child
                ) $
                throwError (BindingTreeError (ParentNotUpper child parent))
        let dirtyTypes =
                IntSet.fromList
                    [ key
                    | (variableKey, boundRoot) <- IntMap.toList canonicalPending
                    , key <- [variableKey, getNodeId boundRoot]
                    ]
            dirtyBindRefs = BoundScope.changedBindParentRefs cBefore cScoped
            stWithTypes
                | IntSet.null dirtyTypes = st
                | otherwise =
                    modifyConstraintDirtyTypesState
                        dirtyTypes
                        (const cScoped)
                        st
            stFinal
                | IntSet.null dirtyBindRefs = stWithTypes
                | otherwise =
                    setConstraintDirtyBindRefsState
                        dirtyBindRefs
                        cScoped
                        stWithTypes
        put stFinal

    recordNew :: NodeId -> StateT CopyState (PresolutionM p) ()
    recordNew freshId =
        modify $ \st ->
            st { csInterior = IntSet.insert (getNodeId freshId) (csInterior st) }

    recordCopy :: NodeId -> NodeId -> StateT CopyState (PresolutionM p) ()
    recordCopy srcNid copiedId =
        modify $ \st ->
            st { csCopyMap = IntMap.insert (getNodeId srcNid) copiedId (csCopyMap st) }

    recordPendingBound :: NodeId -> NodeId -> StateT CopyState (PresolutionM p) ()
    recordPendingBound variable boundRoot = do
        pending <- gets csPendingBounds
        case IntMap.lookup (getNodeId variable) pending of
            Nothing ->
                modify $ \st ->
                    st
                        { csPendingBounds =
                            IntMap.insert
                                (getNodeId variable)
                                boundRoot
                                (csPendingBounds st)
                        }
            Just existing
                | existing == boundRoot -> pure ()
                | otherwise ->
                    lift $
                        throwError
                            (CopyPendingBoundConflict variable existing boundRoot)

    cacheLookup :: NodeId -> StateT CopyState (PresolutionM p) (Maybe NodeId)
    cacheLookup srcNid = gets (IntMap.lookup (getNodeId srcNid) . csCache)

    cacheInsert :: NodeId -> NodeId -> StateT CopyState (PresolutionM p) ()
    cacheInsert srcNid freshId =
        modify $ \st ->
            st { csCache = IntMap.insert (getNodeId srcNid) freshId (csCache st) }

    copyNode
        :: (NodeId -> StateT CopyState (PresolutionM p) (NodeId, TyNode))
        -> IntSet.IntSet
        -> IntSet.IntSet
        -> (NodeId -> NodeId)
        -> IntMap NodeId
        -> NodeId
        -> StateT CopyState (PresolutionM p) NodeId
    copyNode lookupSourceNode copyInterior frontierSet canonical subst nid = do
        (nidC, node) <- lookupSourceNode nid
        mbCached <- cacheLookup nidC
        case mbCached of
            Just existing -> do
                recordCopy nid existing
                recordCopy nidC existing
                pure existing
            Nothing -> do
                case node of
                    TyExp { tnBody = b } -> do
                        b' <- copyNode lookupSourceNode copyInterior frontierSet canonical subst b
                        pure b'
                    _ -> do
                        let k = getNodeId nidC
                        -- Substitution takes precedence over the copy-set
                        -- classification.  A binder can occur at the frontier
                        -- of a separately copied bound; it is still the same
                        -- quantified binder and must be replaced, never shared
                        -- with or copied back into the source scheme.
                        case IntMap.lookup k subst of
                            Just replacement -> do
                                cacheInsert nidC replacement
                                recordCopy nid replacement
                                recordCopy nidC replacement
                                recordNew replacement
                                modify $ \st ->
                                    st
                                        { csEncounteredSubstitutions =
                                            IntSet.insert k (csEncounteredSubstitutions st)
                                        }
                                case node of
                                    TyVar {tnBound = Just boundRoot} ->
                                        copyNode
                                                lookupSourceNode
                                                copyInterior
                                                frontierSet
                                                canonical
                                                subst
                                                boundRoot
                                            >>= recordPendingBound replacement
                                    _ -> pure ()
                                pure replacement
                            Nothing ->
                                if IntSet.member k frontierSet
                                        then do
                                            freshId <- lift createFreshNodeId
                                            cacheInsert nidC freshId
                                            recordCopy nid freshId
                                            recordCopy nidC freshId
                                            lift $ registerNode freshId (TyBottom freshId)
                                            pure freshId
                                    else if not (IntSet.member k copyInterior)
                                        then pure nidC
                                        else do
                                                -- Create fresh node shell
                                                freshId <- lift createFreshNodeId
                                                cacheInsert nidC freshId
                                                recordCopy nid freshId
                                                recordCopy nidC freshId
                                                recordNew freshId
                                                -- Register a placeholder so binding ops see the child/parent ids.
                                                let placeholder =
                                                        case node of
                                                            TyArrow { tnDom = d, tnCod = c } ->
                                                                TyArrow freshId d c
                                                            TyForall { tnBody = b } ->
                                                                TyForall freshId b
                                                            TyMu { tnBody = b } ->
                                                                TyMu freshId b
                                                            TyVar {} ->
                                                                TyVar { tnId = freshId, tnBound = Nothing }
                                                            TyBottom {} ->
                                                                TyBottom freshId
                                                            TyBase { tnBaseIdentity = identity, tnBase = b } ->
                                                                TyBase freshId identity b
                                                            TyCon { tnConIdentity = identity, tnCon = con, tnArgs = args } ->
                                                                TyCon freshId identity con args
                                                            TyVarApp { tnVarHead = headNode, tnArgs = args } ->
                                                                TyVarApp freshId headNode args
                                                lift $ registerNode freshId placeholder

                                                -- Recursively copy children
                                                newNode <- case node of
                                                    TyArrow { tnDom = d, tnCod = c } -> do
                                                        d' <- copyNode lookupSourceNode copyInterior frontierSet canonical subst d
                                                        c' <- copyNode lookupSourceNode copyInterior frontierSet canonical subst c
                                                        return $ TyArrow freshId d' c'
                                                    TyForall { tnBody = b } -> do
                                                        b' <- copyNode lookupSourceNode copyInterior frontierSet canonical subst b
                                                        return $ TyForall freshId b'
                                                    TyMu { tnBody = b } -> do
                                                        b' <- copyNode lookupSourceNode copyInterior frontierSet canonical subst b
                                                        return $ TyMu freshId b'
                                                    TyVar { tnBound = mb } -> do
                                                        mb' <- traverse (copyNode lookupSourceNode copyInterior frontierSet canonical subst) mb
                                                        forM_ mb' (recordPendingBound freshId)
                                                        pure $ TyVar { tnId = freshId, tnBound = Nothing }
                                                    TyBottom {} ->
                                                        pure $ TyBottom freshId
                                                    TyBase { tnBaseIdentity = identity, tnBase = b } -> do
                                                        return $ TyBase freshId identity b
                                                    TyCon { tnConIdentity = identity, tnCon = con, tnArgs = args } -> do
                                                        args' <- traverse (copyNode lookupSourceNode copyInterior frontierSet canonical subst) args
                                                        return $ TyCon freshId identity con args'
                                                    TyVarApp { tnVarHead = headNode, tnArgs = args } -> do
                                                        head' <- copyNode lookupSourceNode copyInterior frontierSet canonical subst headNode
                                                        args' <- traverse (copyNode lookupSourceNode copyInterior frontierSet canonical subst) args
                                                        return $ TyVarApp freshId head' args'

                                                -- Register new node in constraint (overwrite placeholder)
                                                lift $ registerNode freshId newNode

                                                return freshId

-- | Bind the expansion root at the same binder as the edge target.
--
-- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.2):
-- "the root of the expansion is bound at the same binder as the target". This
-- ensures the expansion root is in the correct interior I(r) for subsequent
-- operations.
--
-- If the target has a binding parent, we copy that binding to the expansion
-- root.  A target binding root is represented by ownership under the unique
-- root gen node.
bindExpansionRootLikeTarget :: NodeId -> NodeId -> PresolutionM p NodeRef
bindExpansionRootLikeTarget expansionRoot targetNode = do
    (_c, canonical) <- getConstraintAndCanonical
    let expansionRootC = canonical expansionRoot
    targetBinder <- expansionTargetBinder targetNode
    setBindParentM (typeRef expansionRootC) (targetBinder, BindFlex)
    pure targetBinder

-- | Resolve the binder that owns a fresh expansion at a target node.
-- Kept owner-local so both ordinary expansion and destination-aware copying
-- use exactly the same root-target rule.
expansionTargetBinder :: NodeId -> PresolutionM p NodeRef
expansionTargetBinder targetNode = do
    (c, _canonical) <- getConstraintAndCanonical
    mbParentInfo <- lookupBindParentM (typeRef targetNode)
    case mbParentInfo of
        Just (parentRef, _flag) -> pure parentRef
        Nothing -> do
            -- A target binding root is owned by the binding-tree root gen node.
            let genIds = IntMap.keys (getGenNodeMap (cGenNodes c))
            rootGen <- foldM
                (\acc gidInt -> do
                    case acc of
                        Just _ -> pure acc
                        Nothing -> do
                            let gref = genRef (GenNodeId gidInt)
                            mbParent <- lookupBindParentM gref
                            pure $ case mbParent of
                                Nothing -> Just gref
                                Just _ -> Nothing
                )
                    Nothing
                    genIds
            case rootGen of
                Just gref@(GenRef _) -> pure gref
                Just (TypeRef _) ->
                    throwError (InternalError "expected gen root binder for expansion target")
                Nothing ->
                    throwError (InternalError "missing gen root binder for expansion target")

-- | Bind copied nodes without valid binding parents to an upper binder on the
--   expansion root's binding path.
--
-- During expansion copying, some nodes may not get binding parents because their
-- original parents were not copied. This function ensures all copied nodes have
-- binding parents by binding unbound nodes to the expansion root.
--
-- This maintains the binding tree invariant that all non-term-dag-root nodes
-- have binding parents.
bindUnboundCopiedNodes :: CopyMap -> IntSet.IntSet -> NodeId -> PresolutionM p ()
bindUnboundCopiedNodes copyMap interior expansionRoot = do
    snapshot <- getBindingSnapshot
    let c0 = pbsConstraint snapshot
        canonical = pbsCanonical snapshot
    let expansionRootC = canonical expansionRoot
    expansionPath <- bindingSnapshotPathToRoot snapshot (typeRef expansionRootC)
    let copiedIds = IntSet.fromList (map getNodeId (copiedNodes copyMap))
        candidateIds0 = IntSet.union copiedIds interior

        lookupNode = lookupNodeIn (cNodes c0)

        candidateIds =
            Traversal.reachableFromManyUnderLenient
                canonical
                lookupNode
                (map NodeId (IntSet.toList candidateIds0))

    -- Bind any copied/interior nodes that do not already have a binding parent
    -- to the nearest upper binder on the expansion root's binding path (except the
    -- expansion root itself). This matches the thesis binding-tree invariant: the
    -- only root is the gen node, so freshly-copied term-DAG roots must be attached
    -- under some binder that is upper for them.
    forM_ (IntSet.toList candidateIds) $ \nid -> do
        let node0 = NodeId nid
            nodeC = canonical node0
        c' <- getConstraint
        let chooseParent childRef =
                let uppers = filter (\p -> Binding.isUpper c' p childRef) expansionPath
                    preferGen = [p | p@GenRef{} <- uppers]
                in case preferGen of
                    (p:_) -> Just p
                    [] ->
                        case uppers of
                            (p:_) -> Just p
                            [] -> Nothing
        when (nodeC /= expansionRootC) $
            case Binding.lookupBindParent c' (typeRef nodeC) of
                Just (parentRef, _flag)
                    | Binding.isUpper c' parentRef (typeRef nodeC) -> pure ()
                    | otherwise ->
                        case chooseParent (typeRef nodeC) of
                            Nothing -> pure ()
                            Just chosenParent -> setBindParentM (typeRef nodeC) (chosenParent, BindFlex)
                Nothing ->
                    case chooseParent (typeRef nodeC) of
                        Nothing -> pure ()
                        Just chosenParent -> setBindParentM (typeRef nodeC) (chosenParent, BindFlex)
