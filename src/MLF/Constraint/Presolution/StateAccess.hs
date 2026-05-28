{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.StateAccess
Description : Centralized state access operations for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides centralized access patterns for PresolutionM state,
eliminating duplicate boilerplate across presolution submodules.

= Design Rationale

Previously, 30+ locations duplicated patterns like:
@
c0 <- gets psConstraint
uf <- gets psUnionFind
let canonical = UnionFind.frWith uf
@

And binding tree operations with error lifting:
@
case Binding.lookupBindParentUnder canonical c0 ref of
    Left err -> throwError (BindingTreeError err)
    Right result -> ...
@

This module centralizes these patterns, making code more readable and reducing
duplication by ~150-200 lines across presolution submodules.

= Common Patterns

* Getting the canonical function from union-find state
* Getting constraint and canonical together
* Binding tree operations lifted to PresolutionM with error handling
* Node lookups with canonicalization
-}
module MLF.Constraint.Presolution.StateAccess (
    -- * Canonical function access
    getCanonical,
    getConstraintAndCanonical,
    getConstraintAndUnionFind,
    putConstraintAndUnionFind,
    getPendingUnifyEdgesM,
    getPendingWeakensM,

    -- * Binding tree operations (lifted to PresolutionM)
    lookupBindParentM,
    bindingPathToRootM,
    interiorOfM,
    boundFlexChildrenM,
    boundFlexChildrenAllM,
    orderedBindersM,
    checkBindingTreeM,
    PresolutionBindingSnapshot(..),
    getBindingSnapshot,
    bindingSnapshotLookupBindParent,
    bindingSnapshotInteriorOf,
    bindingSnapshotBoundFlexChildren,
    bindingSnapshotFindSchemeIntroducer,
    bindingSnapshotPathToRoot,

    -- * Node lookups with canonicalization
    lookupNodeCanonM,
    lookupGenNodeCanonM,
    getCanonicalNodeM,

    -- * Scheme provenance
    pendingWeakenOwnerM,
    findSchemeIntroducerM,

    -- * Convenience re-exports
    liftBindingError
) where

import Control.Monad.State (gets, modify')
import Control.Monad.Except (throwError)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Util.UnionFind as UnionFind
import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution.Base (
    CachedBindingModel(..),
    PendingWeakenOwner(..),
    PresolutionM,
    PresolutionError(..),
    PresolutionState(..),
    pendingWeakenOwnerFromMaybe,
    setBindingModelCacheState,
    setConstraintState,
    setUnionFindState
    )

-- -----------------------------------------------------------------------------
-- Canonical function access
-- -----------------------------------------------------------------------------

-- | Get the canonical function from current union-find state.
--
-- This is the most common pattern in presolution: getting the function
-- that maps node IDs to their canonical representatives.
--
-- Replaces:
-- @
-- uf <- gets psUnionFind
-- let canonical = UnionFind.frWith uf
-- @
{-# INLINE getCanonical #-}
getCanonical :: PresolutionM p (NodeId -> NodeId)
getCanonical = do
    uf <- gets psUnionFind
    pure (UnionFind.frWith uf)

-- | Get constraint and canonical function together.
--
-- This is the second most common pattern: getting both the constraint
-- and the canonical function for binding tree operations.
--
-- Replaces:
-- @
-- c0 <- gets psConstraint
-- uf <- gets psUnionFind
-- let canonical = UnionFind.frWith uf
-- @
{-# INLINE getConstraintAndCanonical #-}
getConstraintAndCanonical :: PresolutionM p (Constraint p, NodeId -> NodeId)
getConstraintAndCanonical = do
    c <- gets psConstraint
    uf <- gets psUnionFind
    pure (c, UnionFind.frWith uf)

{-# INLINE getConstraintAndUnionFind #-}
getConstraintAndUnionFind :: PresolutionM p (Constraint p, IntMap NodeId)
getConstraintAndUnionFind = do
    c <- gets psConstraint
    uf <- gets psUnionFind
    pure (c, uf)

{-# INLINE putConstraintAndUnionFind #-}
putConstraintAndUnionFind :: Constraint p -> IntMap NodeId -> PresolutionM p ()
putConstraintAndUnionFind constraint uf = do
    modify' $ \st ->
        setUnionFindState uf (setConstraintState constraint st)

{-# INLINE getPendingUnifyEdgesM #-}
getPendingUnifyEdgesM :: PresolutionM p [UnifyEdge]
getPendingUnifyEdgesM = cUnifyEdges . fst <$> getConstraintAndUnionFind

{-# INLINE getPendingWeakensM #-}
getPendingWeakensM :: PresolutionM p IntSet
getPendingWeakensM = gets psPendingWeakens

-- -----------------------------------------------------------------------------
-- Binding tree operations (lifted to PresolutionM)
-- -----------------------------------------------------------------------------

-- | Lift a BindingError to PresolutionError.
--
-- Use this when you need custom error handling for binding operations.
{-# INLINE liftBindingError #-}
liftBindingError :: Either BindingError a -> PresolutionM p a
liftBindingError = \case
    Left err -> throwError (BindingTreeError err)
    Right result -> pure result

-- | Look up the binding parent of a node, using canonical representatives.
--
-- Returns 'Nothing' if the node is a binding root.
--
-- Replaces:
-- @
-- case Binding.lookupBindParentUnder canonical c0 ref of
--     Left err -> throwError (BindingTreeError err)
--     Right result -> ...
-- @
lookupBindParentM :: NodeRef -> PresolutionM p (Maybe (NodeRef, BindFlag))
lookupBindParentM ref = do
    snapshot <- getBindingSnapshot
    bindingSnapshotLookupBindParent snapshot ref

-- | Trace the binding-parent chain from a node to a root.
--
-- Returns the path as a list of NodeRefs, starting with the given node
-- and ending with a root.
bindingPathToRootM :: NodeRef -> PresolutionM p [NodeRef]
bindingPathToRootM start = do
    snapshot <- getBindingSnapshot
    startC <- requireSnapshotRef "bindingPathToRootM" snapshot start
    liftBindingError $
        Binding.bindingPathToRootLocal
            (Binding.qbpBindParents (pbsQuotient snapshot))
            startC

-- | Compute the interior I(r): all nodes transitively bound to r.
--
-- The returned set contains canonical node keys.
interiorOfM :: NodeRef -> PresolutionM p IntSet
interiorOfM root = do
    snapshot <- getBindingSnapshot
    bindingSnapshotInteriorOf snapshot root

-- | Get flexibly-bound TyVar children of a binder node.
--
-- This corresponds to Q(n) in the paper, restricted to variable nodes.
boundFlexChildrenM :: NodeRef -> PresolutionM p [NodeId]
boundFlexChildrenM binder = do
    snapshot <- getBindingSnapshot
    bindingSnapshotBoundFlexChildren snapshot binder

-- | Get flexibly-bound children (any node type) of a binder node.
--
-- TyExp is internal and skipped; TyBase/TyBottom are atomic.
boundFlexChildrenAllM :: NodeRef -> PresolutionM p [NodeId]
boundFlexChildrenAllM binder = do
    snapshot <- getBindingSnapshot
    bindingSnapshotBoundFlexChildrenAll snapshot binder

-- | Get ordered binders for a binder node (leftmost-lowermost, paper ≺).
orderedBindersM :: NodeRef -> PresolutionM p [NodeId]
orderedBindersM binder = do
    snapshot <- getBindingSnapshot
    binderC <- requireSnapshotRef "orderedBindersM" snapshot binder
    liftBindingError $
        Binding.orderedBindersInQuotient
            (pbsCanonical snapshot)
            (pbsConstraint snapshot)
            (pbsQuotient snapshot)
            binderC

-- | Validate binding-tree invariants on the quotient graph.
checkBindingTreeM :: PresolutionM p ()
checkBindingTreeM = do
    (c, canonical) <- getConstraintAndCanonical
    liftBindingError $ Binding.checkBindingTreeUnder canonical c

-- | Snapshot the quotient binding tree for a stable read phase.
--
-- Several edge-loop operations need multiple binding queries against the same
-- constraint/UF snapshot. Building the quotient once avoids repeatedly scanning
-- the whole binding-parent map inside one edge-local copy or planning step.
data PresolutionBindingSnapshot p = PresolutionBindingSnapshot
    { pbsConstraint :: Constraint p
    , pbsCanonical :: NodeId -> NodeId
    , pbsQuotient :: Binding.QuotientBindParents
    }

getBindingSnapshot :: PresolutionM p (PresolutionBindingSnapshot p)
getBindingSnapshot = do
    st <- gets id
    case psBindingModelCache st of
        Just cached
            | cbmUnionFindVersion cached == psUnionFindVersion st
            , cbmBindParentsVersion cached == psBindParentsVersion st ->
                -- Quotient is valid; return current constraint (types may have
                -- changed via modifyConstraintDirtyTypesState without bumping
                -- psBindParentsVersion).
                pure (snapshotFromCached (psConstraint st) cached)
        _ -> do
            let c = psConstraint st
                uf = psUnionFind st
                canonical = UnionFind.frWith uf
            qbp <- liftBindingError $
                Binding.quotientBindParentsContextUnder canonical c
            let cached =
                    CachedBindingModel
                        { cbmGraphVersion = psGraphVersion st
                        , cbmUnionFindVersion = psUnionFindVersion st
                        , cbmBindParentsVersion = psBindParentsVersion st
                        , cbmConstraint = c
                        , cbmUnionFind = uf
                        , cbmQuotient = qbp
                        }
            modify' (setBindingModelCacheState cached)
            pure (snapshotFromCached c cached)
  where
    snapshotFromCached currentConstraint cached =
        let canonical = UnionFind.frWith (cbmUnionFind cached)
        in PresolutionBindingSnapshot
            { pbsConstraint = currentConstraint
            , pbsCanonical = canonical
            , pbsQuotient = cbmQuotient cached
            }

{-# INLINE canonicalRefInSnapshot #-}
canonicalRefInSnapshot :: PresolutionBindingSnapshot p -> NodeRef -> NodeRef
canonicalRefInSnapshot snapshot =
    Canonicalize.canonicalRef (pbsCanonical snapshot)

requireSnapshotRef :: String -> PresolutionBindingSnapshot p -> NodeRef -> PresolutionM p NodeRef
requireSnapshotRef ctx snapshot ref0 = do
    let refC = canonicalRefInSnapshot snapshot ref0
    if IntSet.member (nodeRefKey refC) (Binding.qbpAllRoots (pbsQuotient snapshot))
        then pure refC
        else
            throwError $
                BindingTreeError $
                    InvalidBindingTree $
                        ctx ++ ": node " ++ show refC ++ " not in constraint"

bindingSnapshotLookupBindParent
    :: PresolutionBindingSnapshot p
    -> NodeRef
    -> PresolutionM p (Maybe (NodeRef, BindFlag))
bindingSnapshotLookupBindParent snapshot ref0 = do
    refC <- requireSnapshotRef "bindingSnapshotLookupBindParent" snapshot ref0
    pure $ IntMap.lookup (nodeRefKey refC) (Binding.qbpBindParents (pbsQuotient snapshot))

bindingSnapshotPathToRoot
    :: PresolutionBindingSnapshot p
    -> NodeRef
    -> PresolutionM p [NodeRef]
bindingSnapshotPathToRoot snapshot start0 = do
    startC <- requireSnapshotRef "bindingSnapshotPathToRoot" snapshot start0
    liftBindingError $
        Binding.bindingPathToRootLocal
            (Binding.qbpBindParents (pbsQuotient snapshot))
            startC

bindingSnapshotInteriorOf :: PresolutionBindingSnapshot p -> NodeRef -> PresolutionM p IntSet
bindingSnapshotInteriorOf snapshot root0 = do
    rootC <- requireSnapshotRef "bindingSnapshotInteriorOf" snapshot root0
    let childrenByParent = Binding.qbpChildrenByParent (pbsQuotient snapshot)
        go visited [] = visited
        go visited (key : rest) =
            let kids =
                    IntSet.fromList
                        [ childKey
                        | (childKey, _info) <- IntMap.findWithDefault [] key childrenByParent
                        ]
                newKids = IntSet.difference kids visited
            in go (IntSet.union visited newKids) (IntSet.toList newKids ++ rest)
        rootKey = nodeRefKey rootC
    pure (go (IntSet.singleton rootKey) [rootKey])

bindingSnapshotBoundFlexChildren
    :: PresolutionBindingSnapshot p
    -> NodeRef
    -> PresolutionM p [NodeId]
bindingSnapshotBoundFlexChildren snapshot binder0 = do
    binderC <- requireSnapshotRef "bindingSnapshotBoundFlexChildren" snapshot binder0
    liftBindingError $
        Binding.boundFlexChildrenInQuotient
            (pbsConstraint snapshot)
            (pbsQuotient snapshot)
            binderC

bindingSnapshotBoundFlexChildrenAll
    :: PresolutionBindingSnapshot p
    -> NodeRef
    -> PresolutionM p [NodeId]
bindingSnapshotBoundFlexChildrenAll snapshot binder0 = do
    binderC <- requireSnapshotRef "bindingSnapshotBoundFlexChildrenAll" snapshot binder0
    liftBindingError $
        Binding.boundFlexChildrenAllInQuotient
            (pbsConstraint snapshot)
            (pbsQuotient snapshot)
            binderC

bindingSnapshotFindSchemeIntroducer
    :: PresolutionBindingSnapshot p
    -> NodeId
    -> PresolutionM p GenNodeId
bindingSnapshotFindSchemeIntroducer snapshot root0 = do
    let root = pbsCanonical snapshot root0
        start = typeRef root
    startC <- requireSnapshotRef "bindingSnapshotFindSchemeIntroducer" snapshot start
    path <- liftBindingError $
        Binding.bindingPathToRootLocal
            (Binding.qbpBindParents (pbsQuotient snapshot))
            startC
    case [gid | GenRef gid <- path] of
        (gid:_) -> pure gid
        [] ->
            throwError
                (InternalError ("scheme introducer not found for " ++ show root))

-- -----------------------------------------------------------------------------
-- Node lookups with canonicalization
-- -----------------------------------------------------------------------------

-- | Look up a type node using canonical representative.
--
-- Returns 'Nothing' if the node doesn't exist.
{-# INLINE lookupNodeCanonM #-}
lookupNodeCanonM :: NodeId -> PresolutionM p (Maybe TyNode)
lookupNodeCanonM nid = do
    (c, canonical) <- getConstraintAndCanonical
    pure $ NodeAccess.lookupNodeCanon canonical c nid

-- | Look up a gen node using canonical representative.
--
-- Returns 'Nothing' if the gen node doesn't exist.
{-# INLINE lookupGenNodeCanonM #-}
lookupGenNodeCanonM :: GenNodeId -> PresolutionM p (Maybe GenNode)
lookupGenNodeCanonM gid = do
    c <- gets psConstraint
    pure $ NodeAccess.lookupGenNode c gid

-- | Look up a node at its canonical representative, failing if not found.
--
-- This is a common pattern that combines findRoot + lookup.
{-# INLINE getCanonicalNodeM #-}
getCanonicalNodeM :: NodeId -> PresolutionM p TyNode
getCanonicalNodeM nid = do
    (c, canonical) <- getConstraintAndCanonical
    let canonNid = canonical nid
    case NodeAccess.lookupNode c canonNid of
        Just node -> pure node
        Nothing -> throwError $ NodeLookupFailed canonNid

-- -----------------------------------------------------------------------------
-- Scheme provenance
-- -----------------------------------------------------------------------------

-- | Resolve the nearest scheme owner for a pending-weaken target.
--
-- This is intentionally total: unknown/malformed paths map to
-- 'PendingWeakenOwnerUnknown' so owner-aware scheduling can remain
-- conservative when a scheme owner cannot be proven.
pendingWeakenOwnerM :: NodeId -> PresolutionM p PendingWeakenOwner
pendingWeakenOwnerM nid = do
    snapshot <- getBindingSnapshot
    pendingWeakenOwnerInSnapshot snapshot nid

-- | Find the nearest gen-node ancestor on the binding path from a type node.
--
-- This is used to identify which ∀-scheme "owns" a given body root during
-- instantiation.  The function canonicalizes the root, walks the binding path
-- upward, and returns the first 'GenNodeId' encountered.
findSchemeIntroducerM :: (NodeId -> NodeId) -> Constraint q -> NodeId -> PresolutionM p GenNodeId
findSchemeIntroducerM _canonical _c0 root0 = do
    snapshot <- getBindingSnapshot
    bindingSnapshotFindSchemeIntroducer snapshot root0

pendingWeakenOwnerInSnapshot :: PresolutionBindingSnapshot p -> NodeId -> PresolutionM p PendingWeakenOwner
pendingWeakenOwnerInSnapshot snapshot nid0 =
    go IntSet.empty (typeRef (pbsCanonical snapshot nid0))
  where
    go visited ref
        | IntSet.member (nodeRefKey ref) visited =
            pure PendingWeakenOwnerUnknown
        | otherwise = do
            mbParent <- bindingSnapshotLookupBindParent snapshot ref
            case mbParent of
                Nothing -> pure PendingWeakenOwnerUnknown
                Just (GenRef gid, _flag) -> pure (pendingWeakenOwnerFromMaybe (Just gid))
                Just (TypeRef parent, _flag) ->
                    go (IntSet.insert (nodeRefKey ref) visited) (typeRef parent)
