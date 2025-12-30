{- |
Module      : MLF.Binding.Tree
Description : Paper-style binding tree operations
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the binding-tree data model and operations from
@papers/xmlf.txt@ §3.1. The binding tree is an explicit representation of
which nodes are bound by which other nodes, with flexible/rigid flags.

Note [Binding Tree]
~~~~~~~~~~~~~~~~~~~
The paper represents scope via a *binding tree* that is separate from the
term-DAG structure. Each non-root node has exactly one binding parent, and
the parent must be "upper" than the child (reachable from the root via
term-DAG edges).

Key operations:
  - 'lookupBindParent': get the binding parent and flag for a node
  - 'setBindParent': set/update the binding parent for a node
  - 'bindingRoots': compute the set of root nodes (no binding parent)
  - 'bindingPathToRoot': trace the binding-parent chain to a root
  - 'bindingLCA': compute the lowest common ancestor in the binding tree
  - 'interiorOf': compute I(r), all nodes transitively bound to r
  - 'checkBindingTree': validate all binding-tree invariants
-}
module MLF.Binding.Tree (
    -- * Lookup and update
    lookupBindParent,
    lookupBindParentUnder,
    setBindParent,
    removeBindParent,
    -- * Binder enumeration (paper Q(n))
    boundFlexChildren,
    boundFlexChildrenUnder,
    orderedBinders,
    forallSpecFromForall,
    -- * Root detection
    bindingRoots,
    isBindingRoot,
    computeTermDagRoots,
    computeTermDagRootsUnder,
    -- * Path operations
    bindingPathToRoot,
    bindingLCA,
    canonicalizeBindParentsUnder,
    -- * Interior computation
    interiorOf,
    interiorOfUnder,
    -- * Invariant checking
    checkBindingTree,
    checkBindingTreeUnder,
    isUpper,
    -- * Node classification (paper §3.1)
    NodeKind(..),
    nodeKind,
    isUnderRigidBinder,
    -- * Utilities
    allNodeIds,
) where

import Control.Monad (foldM, forM_, unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)

import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Util.OrderKey as OrderKey
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Types

-- | Paper node kinds (`papers/xmlf.txt` §3.1).
--
-- These are derived from binding-edge flags along the binding-parent path.
data NodeKind
    = NodeRoot
    | NodeInstantiable
    | NodeRestricted
    | NodeLocked
    deriving (Eq, Show)

-- | Look up the binding parent and flag for a node.
--
-- Returns 'Nothing' if the node is a binding root (has no parent).
lookupBindParent :: Constraint -> NodeId -> Maybe (NodeId, BindFlag)
lookupBindParent c (NodeId nid) = IntMap.lookup nid (cBindParents c)

-- | Look up the binding parent and flag for a node on the quotient binding
-- graph induced by a canonicalization function.
--
-- This mirrors the rewriting logic of 'checkBindingTreeUnder' and
-- 'interiorOfUnder': binding edges are first rewritten to canonical
-- representatives (dropping self-edges), and the parent is looked up on that
-- rewritten relation.
--
-- Returns 'Nothing' if the node is a binding root in the quotient relation.
lookupBindParentUnder
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Either BindingError (Maybe (NodeId, BindFlag))
lookupBindParentUnder canonical c0 nid0 = do
    let nidC = canonical nid0
    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    unless (IntSet.member (getNodeId nidC) allRoots) $
        Left $
            InvalidBindingTree $
                "lookupBindParentUnder: node " ++ show (getNodeId nidC) ++ " not in cNodes"
    pure (IntMap.lookup (getNodeId nidC) bindParents)

-- | Canonicalize the binding-parent relation under a canonicalization function.
--
-- This drops self-edges (where child and parent canonicalize to the same node),
-- merges duplicate edges by requiring the same parent and taking the max flag,
-- and rejects conflicting parents on the same canonical node.
--
-- This is useful when unification maintains a union-find over nodes: after a
-- merge, the raw `cBindParents` relation may contain edges for multiple aliases
-- of the same canonical representative. Canonicalizing makes subsequent binding
-- operations (like LCA and Raise) well-defined on representatives.
canonicalizeBindParentsUnder
    :: (NodeId -> NodeId)
    -> Constraint
    -> Either BindingError BindParents
canonicalizeBindParentsUnder canonical c0 = do
    (_allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    pure bindParents

-- | Set the binding parent for a node.
--
-- This overwrites any existing binding parent for the node.
setBindParent :: NodeId -> (NodeId, BindFlag) -> Constraint -> Constraint
setBindParent (NodeId child) parentInfo c =
    c { cBindParents = IntMap.insert child parentInfo (cBindParents c) }

-- | Remove the binding parent for a node, making it a root.
removeBindParent :: NodeId -> Constraint -> Constraint
removeBindParent (NodeId child) c =
    c { cBindParents = IntMap.delete child (cBindParents c) }

-- | Direct flexibly-bound TyVar children of a binder node.
--
-- This corresponds to Q(n) in the paper, restricted to variable nodes.
boundFlexChildren :: Constraint -> NodeId -> Either BindingError [NodeId]
boundFlexChildren c binder@(NodeId binderId) = do
    unless (IntMap.member binderId (cNodes c)) $
        Left $
            InvalidBindingTree $
                "boundFlexChildren: binder " ++ show binderId ++ " not in cNodes"
    reverse <$> foldM
        (\acc (childId, (parent, flag)) ->
            if parent /= binder || flag /= BindFlex
                then pure acc
                else case IntMap.lookup childId (cNodes c) of
                    Just TyVar{} -> pure (NodeId childId : acc)
                    Just _ -> pure acc
                    Nothing ->
                        Left $
                            InvalidBindingTree $
                                "boundFlexChildren: child " ++ show childId ++ " not in cNodes"
        )
        []
        (IntMap.toList (cBindParents c))

-- | Quotient-aware variant of 'boundFlexChildren' (canonicalized binding tree).
boundFlexChildrenUnder
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Either BindingError [NodeId]
boundFlexChildrenUnder canonical c0 binder0 = do
    let binderC = canonical binder0
    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    unless (IntSet.member (getNodeId binderC) allRoots) $
        Left $
            InvalidBindingTree $
                "boundFlexChildrenUnder: binder " ++ show (getNodeId binderC) ++ " not in cNodes"
    reverse <$> foldM
        (\acc (childId, (parent, flag)) ->
            if parent /= binderC || flag /= BindFlex
                then pure acc
                else case IntMap.lookup childId (cNodes c0) of
                    Just TyVar{} -> pure (NodeId childId : acc)
                    Just _ -> pure acc
                    Nothing ->
                        Left $
                            InvalidBindingTree $
                                "boundFlexChildrenUnder: child " ++ show childId ++ " not in cNodes"
        )
        []
        (IntMap.toList bindParents)

-- | Ordered binders for a binder node (leftmost-lowermost, paper ≺).
--
-- Binders are filtered to those reachable from the binder’s “order root”:
--   • for a `TyForall`, the body root (paper binding-node translation),
--   • otherwise, the binder node itself (useful for top-level/generalization).
orderedBinders
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Either BindingError [NodeId]
orderedBinders canonical c0 binder0 = do
    let binderC = canonical binder0
    unless (IntMap.member (getNodeId binderC) (cNodes c0)) $
        Left $
            InvalidBindingTree $
                "orderedBinders: binder " ++ show (getNodeId binderC) ++ " not in cNodes"
    let nodes = cNodes c0
        orderRoot =
            case IntMap.lookup (getNodeId binderC) nodes of
                Just TyForall{ tnBody = body } -> canonical body
                _ -> binderC

        reachable =
            Traversal.reachableFromUnderLenient
                canonical
                (\nid -> IntMap.lookup (getNodeId nid) nodes)
                orderRoot
    binders <- boundFlexChildrenUnder canonical c0 binderC
    let bindersReachable =
            filter (\nid -> IntSet.member (getNodeId nid) reachable) binders
        boundChildren =
            IntMap.fromListWith (flip (++))
                [ (getNodeId vC, [bC])
                | (vid, Just bnd) <- IntMap.toList (cVarBounds c0)
                , let vC = canonical (NodeId vid)
                , let bC = canonical bnd
                , vC /= bC
                ]
        extraChildren nid =
            IntMap.findWithDefault [] (getNodeId (canonical nid)) boundChildren
        orderKeys = OrderKey.orderKeysFromRootWithExtra canonical nodes extraChildren orderRoot Nothing
        missing =
            [ nid
            | nid <- bindersReachable
            , not (IntMap.member (getNodeId nid) orderKeys)
            ]
    unless (null missing) $
        Left $
            InvalidBindingTree $
                "orderedBinders: missing order keys for " ++ show missing
    pure (sortBy (OrderKey.compareNodesByOrderKey orderKeys) bindersReachable)

-- | Compute a ForallSpec (binder count + bounds) for a forall node.
forallSpecFromForall
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Either BindingError ForallSpec
forallSpecFromForall canonical c0 binder0 = do
    binders <- orderedBinders canonical c0 binder0
    let binderIndex =
            IntMap.fromList
                [ (getNodeId b, idx)
                | (idx, b) <- zip [0..] binders
                ]
        boundFor b =
            case VarStore.lookupVarBound c0 b of
                Nothing -> Nothing
                Just bnd ->
                    let bndC = canonical bnd
                    in case IntMap.lookup (getNodeId bndC) binderIndex of
                        Just idx -> Just (BoundBinder idx)
                        Nothing -> Just (BoundNode bndC)
    pure
        ForallSpec
            { fsBinderCount = length binders
            , fsBounds = map boundFor binders
            }

-- | Compute the set of binding roots (nodes with no binding parent).
--
-- A node is a root if it appears in cNodes but not as a key in cBindParents.
bindingRoots :: Constraint -> [NodeId]
bindingRoots c =
    let allNodes = IntMap.keys (cNodes c)
        hasParent nid = IntMap.member nid (cBindParents c)
    in map NodeId $ filter (not . hasParent) allNodes

-- | Check if a node is a binding root.
isBindingRoot :: Constraint -> NodeId -> Bool
isBindingRoot c (NodeId nid) = not $ IntMap.member nid (cBindParents c)

-- | Trace the binding-parent chain from a node to a root.
--
-- Returns the path as a list of NodeIds, starting with the given node
-- and ending with a root. Returns an error if a cycle is detected.
bindingPathToRoot :: Constraint -> NodeId -> Either BindingError [NodeId]
bindingPathToRoot c start = go IntSet.empty [start] start
  where
    go visited path (NodeId nid)
        | IntSet.member nid visited = Left $ BindingCycleDetected (reverse path)
        | otherwise = case IntMap.lookup nid (cBindParents c) of
            Nothing -> Right (reverse path)  -- Reached a root
            Just (parent, _flag) ->
                go (IntSet.insert nid visited) (parent : path) parent

-- | Compute the lowest common ancestor of two nodes in the binding tree.
--
-- Returns an error if either node has a cycle in its binding path.
bindingLCA :: Constraint -> NodeId -> NodeId -> Either BindingError NodeId
bindingLCA c n1 n2 = do
    path1 <- bindingPathToRoot c n1
    path2 <- bindingPathToRoot c n2
    let set1 = IntSet.fromList $ map getNodeId path1
        -- Find the first node in path2 that is also in path1
        common = filter (\(NodeId nid) -> IntSet.member nid set1) path2
    case common of
        [] -> Left $ NoCommonAncestor n1 n2
        (lca:_) -> Right lca

-- | Classify a node according to the paper’s instantiable/restricted/locked
-- taxonomy (`papers/xmlf.txt` §3.1).
--
--   - Root: no binding parent.
--   - Restricted: the node’s own binding edge is rigid.
--   - Locked: the node’s own edge is flexible, but some strict ancestor edge is rigid.
--   - Instantiable: the entire binding path to the root is flexible.
nodeKind :: Constraint -> NodeId -> Either BindingError NodeKind
nodeKind c nid =
    case lookupBindParent c nid of
        Nothing -> Right NodeRoot
        Just (_parent, BindRigid) -> Right NodeRestricted
        Just (_parent, BindFlex) -> do
            underRigid <- isUnderRigidBinder c nid
            pure $
                if underRigid
                    then NodeLocked
                    else NodeInstantiable

-- | True iff @nid@ is strictly under some rigid binding edge.
--
-- This intentionally does /not/ treat a restricted node as “under rigid” purely
-- because its own binding edge is rigid: only strict ancestors are considered.
--
-- Paper anchor (`papers/xmlf.txt` §3.4): normalized witnesses do not perform
-- operations under rigidly bound nodes.
isUnderRigidBinder :: Constraint -> NodeId -> Either BindingError Bool
isUnderRigidBinder c nid = do
    path <- bindingPathToRoot c nid
    let strictAncestors = drop 1 path
        isRigidEdge n =
            case lookupBindParent c n of
                Just (_, BindRigid) -> True
                _ -> False
    pure (any isRigidEdge strictAncestors)

-- | Compute the interior I(r): all nodes transitively bound to r.
--
-- This includes r itself and all nodes whose binding path passes through r.
interiorOf :: Constraint -> NodeId -> Either BindingError IntSet
interiorOf c root@(NodeId rootId) = do
    -- First verify the root exists in the constraint
    case IntMap.lookup rootId (cNodes c) of
        Nothing -> Left $ InvalidBindingTree $ "Root node " ++ show rootId ++ " not in constraint"
        Just _ -> Right ()
    
    -- Collect all nodes whose binding path includes root
    let allNodes = IntMap.keys (cNodes c)
        checkNode nid = do
            path <- bindingPathToRoot c (NodeId nid)
            return $ root `elem` path
    
    -- Build the interior set
    results <- mapM (\nid -> do
        inInterior <- checkNode nid
        return (nid, inInterior)) allNodes
    
    return $ IntSet.fromList [nid | (nid, True) <- results]

-- | Compute the interior I(r) on the quotient graph induced by a canonicalization
-- function.
--
-- This mirrors `checkBindingTreeUnder`: binding edges are first rewritten to
-- canonical representatives (dropping self-edges), and the interior is computed
-- on that rewritten binding-parent relation.
--
-- The returned set contains canonical node ids.
interiorOfUnder
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Either BindingError IntSet
interiorOfUnder canonical c0 root0 = do
    let rootC = canonical root0

    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    unless (IntSet.member (getNodeId rootC) allRoots) $
        Left $
            InvalidBindingTree $
                "interiorOfUnder: root " ++ show (getNodeId rootC) ++ " not in cNodes"

    let childrenByParent :: IntMap.IntMap IntSet
        childrenByParent =
            foldl'
                (\m (childRootId, (parentRoot, _flag)) ->
                    let parentRootId = getNodeId (canonical parentRoot)
                    in IntMap.insertWith IntSet.union parentRootId (IntSet.singleton childRootId) m
                )
                IntMap.empty
                (IntMap.toList bindParents)

        go :: IntSet -> [Int] -> IntSet
        go visited [] = visited
        go visited (nid : rest) =
            let kids = IntMap.findWithDefault IntSet.empty nid childrenByParent
                newKids = filter (\k -> not (IntSet.member k visited)) (IntSet.toList kids)
                visited' = foldl' (flip IntSet.insert) visited newKids
            in go visited' (newKids ++ rest)

    pure (go (IntSet.singleton (getNodeId rootC)) [getNodeId rootC])

-- | Rewrite `cBindParents` to a canonicalized binding-parent relation (dropping
-- self-edges), merging duplicates by requiring the same parent and taking the
-- max flag.
--
-- This is the shared core used by 'checkBindingTreeUnder', 'interiorOfUnder',
-- and other quotient-aware binding-tree queries.
quotientBindParentsUnder
    :: (NodeId -> NodeId)
    -> Constraint
    -> Either BindingError (IntSet, BindParents)
quotientBindParentsUnder canonical c0 = do
    let nodes0 = cNodes c0
        bindParents0 = cBindParents c0

        allRoots :: IntSet
        allRoots =
            IntSet.fromList
                [ getNodeId (canonical (NodeId nid))
                | nid <- IntMap.keys nodes0
                ]

        entries0 =
            [ (childRootId, (parentRoot, flag))
            | (childId, (parent0, flag)) <- IntMap.toList bindParents0
            , let childRoot = canonical (NodeId childId)
            , let parentRoot = canonical parent0
            , childRoot /= parentRoot
            , let childRootId = getNodeId childRoot
            ]

        insertOne :: BindParents -> (Int, (NodeId, BindFlag)) -> Either BindingError BindParents
        insertOne bp (childRootId, (parentRoot, flag)) =
            case IntMap.lookup childRootId bp of
                Nothing -> Right (IntMap.insert childRootId (parentRoot, flag) bp)
                Just (parent0, flag0)
                    | parent0 == parentRoot ->
                        let flag' = max flag0 flag
                        in Right (IntMap.insert childRootId (parentRoot, flag') bp)
                    | otherwise ->
                        -- Union-find canonicalization can transiently create multiple
                        -- binding parents for the same canonical node (e.g. when two
                        -- disjoint binding trees are unified). We resolve this
                        -- deterministically by keeping the first parent we saw and
                        -- taking the max flag.
                        --
                        -- This matches the conflict-resolution strategy used when
                        -- rewriting constraints through UF in Normalize/Solve.
                        let flag' = max flag0 flag
                        in Right (IntMap.insert childRootId (parent0, flag') bp)

    bindParents <- foldM insertOne IntMap.empty entries0

    -- Sanity: rewritten nodes must correspond to canonical reps of live nodes.
    forM_ (IntMap.keys bindParents) $ \childRootId ->
        unless (IntSet.member childRootId allRoots) $
            Left $ InvalidBindingTree ("quotientBindParentsUnder: binding child " ++ show childRootId ++ " not in cNodes")

    forM_ (IntMap.toList bindParents) $ \(childRootId, (parentRoot, _flag)) -> do
        let parentRootId = getNodeId (canonical parentRoot)
        unless (IntSet.member parentRootId allRoots) $
            Left $
                InvalidBindingTree $
                    "quotientBindParentsUnder: binding parent " ++ show parentRootId
                        ++ " of node " ++ show childRootId ++ " not in cNodes"

    pure (allRoots, bindParents)

-- | Validate all binding-tree invariants.
--
-- Checks:
--   1. Every non-root node has exactly one parent (structural by IntMap)
--   2. The parent pointers are acyclic
--   3. Every parent node exists in cNodes
--   4. Every parent is "upper" than its child in the term-DAG (structural reachability)
--   5. Every non-term-dag-root node has a binding parent
--   6. Term-dag roots have no binding parent
--
-- Note: an empty `cBindParents` is a valid binding tree for root-only graphs;
-- emptiness is not a mode signal.
checkBindingTree :: Constraint -> Either BindingError ()
checkBindingTree c = do
    -- Check 1: Every parent node exists in cNodes
    let bindParents = cBindParents c
        nodes = cNodes c
        checkChildExists (childId, _parentInfo) =
            if IntMap.member childId nodes
            then Nothing
            else Just $ InvalidBindingTree $
                "Binding child " ++ show childId ++ " not in cNodes"
        checkParentExists (childId, (NodeId parentId, _flag)) =
            if IntMap.member parentId nodes
            then Nothing
            else Just $ InvalidBindingTree $
                "Binding parent " ++ show parentId ++
                " of node " ++ show childId ++ " not in cNodes"

    case mapMaybe checkChildExists (IntMap.toList bindParents) of
        (err:_) -> Left err
        [] -> Right ()

    case mapMaybe checkParentExists (IntMap.toList bindParents) of
        (err:_) -> Left err
        [] -> Right ()
    
    -- Check 2: No cycles in binding paths
    let allChildren = IntMap.keys bindParents
    mapM_ (bindingPathToRoot c . NodeId) allChildren
    
    -- Check 3: Every parent is "upper" than its child in the term-DAG
    let checkUpperInvariant (childId, (parentId, _flag)) =
            if isUpper c parentId (NodeId childId)
            then Nothing
            else Just $ ParentNotUpper (NodeId childId) parentId
    
    case mapMaybe checkUpperInvariant (IntMap.toList bindParents) of
        (err:_) -> Left err
        [] -> Right ()
    
    -- Check 4: Every non-term-dag-root node has a binding parent.
    --
    -- Paper anchor (`papers/xmlf.txt` §3.1): every node except the term-dag root
    -- has a binding edge to some upper node at which it is bound.
    --
    -- We generalize this to constraint graphs that may contain multiple
    -- disconnected components: every structural root (no incoming structure
    -- edge) is a binding root; every other node must have a binding parent.
    let termDagRoots = computeTermDagRoots c
        allNodeIdList = IntMap.keys nodes
        needsParent nid = not (IntSet.member nid termDagRoots)
        hasParent nid = IntMap.member nid bindParents
        missingParent = filter (\nid -> needsParent nid && not (hasParent nid)) allNodeIdList
    case missingParent of
        (nid:_) -> Left $ MissingBindParent (NodeId nid)
        [] -> Right ()

    -- Check 5: Term-dag roots have no binding parent (binding roots match term-dag roots).
    let unexpectedParent = filter (\nid -> IntSet.member nid termDagRoots && IntMap.member nid bindParents) allNodeIdList
    case unexpectedParent of
        (nid:_) ->
            Left $
                InvalidBindingTree $
                    "Term-dag root " ++ show (NodeId nid) ++ " unexpectedly has a binding parent"
        [] -> Right ()

-- | Validate binding-tree invariants on the quotient graph induced by a canonicalization function.
--
-- This is useful during presolution, where we maintain a separate union-find for
-- node equivalences but do not immediately rewrite/collapse the term-DAG.
--
-- The check is performed on:
--   • binding edges rewritten to canonical reps, and
--   • a quotient term-DAG where structure edges are the union of all edges in each UF class.
--
-- Note: unlike older transitional code, this check does not treat an empty
-- `cBindParents` map as “legacy mode”; emptiness is valid for root-only graphs.
checkBindingTreeUnder :: (NodeId -> NodeId) -> Constraint -> Either BindingError ()
checkBindingTreeUnder canonical c0 = do
    let nodes0 = cNodes c0
        bindParents0 = cBindParents c0

        rootIdOf :: NodeId -> Int
        rootIdOf = getNodeId . canonical

        addStructEdges :: IntMap.IntMap IntSet -> TyNode -> IntMap.IntMap IntSet
        addStructEdges m node =
            let parentRoot = rootIdOf (tnId node)
                childRoots = IntSet.fromList (map rootIdOf (structuralChildren node))
            in if IntSet.null childRoots
                then m
                else IntMap.insertWith IntSet.union parentRoot childRoots m

        structEdges :: IntMap.IntMap IntSet
        structEdges = foldl' addStructEdges IntMap.empty (IntMap.elems nodes0)

        allRoots :: IntSet
        allRoots = IntSet.fromList [rootIdOf (NodeId nid) | nid <- IntMap.keys nodes0]

        termDagRoots :: IntSet
        termDagRoots = computeTermDagRootsUnder canonical c0

        entries0 =
            [ (childRootId, (parentRoot, flag))
            | (childId, (parent0, flag)) <- IntMap.toList bindParents0
            , let childRoot = canonical (NodeId childId)
            , let parentRoot = canonical parent0
            , childRoot /= parentRoot
            , let childRootId = getNodeId childRoot
            ]

        insertOne :: BindParents -> (Int, (NodeId, BindFlag)) -> Either BindingError BindParents
        insertOne bp (childRootId, (parentRoot, flag)) =
            case IntMap.lookup childRootId bp of
                Nothing -> Right (IntMap.insert childRootId (parentRoot, flag) bp)
                Just (parent0, flag0)
                    | parent0 == parentRoot ->
                        let flag' = max flag0 flag
                        in Right (IntMap.insert childRootId (parentRoot, flag') bp)
                    | otherwise ->
                        Left $
                            InvalidBindingTree $
                                "Conflicting binding parents for " ++ show childRootId
                                    ++ ": " ++ show parent0 ++ " vs " ++ show parentRoot

    bindParents <- foldM insertOne IntMap.empty entries0

    -- Child/parent existence (roots must correspond to some live node).
    forM_ (IntMap.keys bindParents) $ \childRootId ->
        unless (IntSet.member childRootId allRoots) $
            Left $ InvalidBindingTree ("Binding child " ++ show childRootId ++ " not in cNodes")

    forM_ (IntMap.toList bindParents) $ \(childRootId, (parentRoot, _flag)) -> do
        let parentRootId = rootIdOf parentRoot
        unless (IntSet.member parentRootId allRoots) $
            Left $
                InvalidBindingTree $
                    "Binding parent " ++ show parentRootId
                        ++ " of node " ++ show childRootId ++ " not in cNodes"

    -- Cycle check on the canonical binding-parent relation.
    let bindingPathToRootUnder :: Int -> Either BindingError [NodeId]
        bindingPathToRootUnder start = go IntSet.empty [NodeId start] start
          where
            go visited path nid
                | IntSet.member nid visited = Left (BindingCycleDetected (reverse path))
                | otherwise =
                    case IntMap.lookup nid bindParents of
                        Nothing -> Right (reverse path)
                        Just (parent, _flag) ->
                            let pid = rootIdOf parent
                            in go (IntSet.insert nid visited) (NodeId pid : path) pid

    mapM_ bindingPathToRootUnder (IntMap.keys bindParents)

    let isUpperUnder :: NodeId -> NodeId -> Bool
        isUpperUnder parent child
            | parentR == childR = True
            | otherwise = go IntSet.empty parentR
          where
            parentR = rootIdOf parent
            childR = rootIdOf child

            go visited nid
                | nid == childR = True
                | IntSet.member nid visited = False
                | otherwise =
                    let visited' = IntSet.insert nid visited
                        kids = IntMap.findWithDefault IntSet.empty nid structEdges
                    in any (go visited') (IntSet.toList kids)

    -- Upper-than check on the quotient structure graph.
    forM_ (IntMap.toList bindParents) $ \(childRootId, (parentRoot, _flag)) -> do
        let childRoot = NodeId childRootId
        unless (isUpperUnder parentRoot childRoot) $
            Left (ParentNotUpper childRoot parentRoot)

    -- Every non-term-dag-root needs a parent.
    forM_ (IntSet.toList allRoots) $ \nid ->
        unless (IntSet.member nid termDagRoots || IntMap.member nid bindParents) $
            Left (MissingBindParent (NodeId nid))

    -- Term-dag roots have no parent.
    forM_ (IntSet.toList termDagRoots) $ \nid ->
        when (IntMap.member nid bindParents) $
            Left $
                InvalidBindingTree $
                    "Term-dag root " ++ show (NodeId nid) ++ " unexpectedly has a binding parent"

    pure ()

-- | Compute the set of term-DAG root nodes.
--
-- A term-DAG root is a node that has no incoming structure edge, i.e., no other
-- node points to it via TyArrow dom/cod, TyForall body, or TyExp body.
computeTermDagRoots :: Constraint -> IntSet
computeTermDagRoots c =
    let nodes = cNodes c
        allNodeIds' = IntSet.fromList $ IntMap.keys nodes
        -- Collect all nodes that are pointed to by structure edges
        referencedNodes =
            IntSet.fromList
                [ getNodeId child
                | node <- IntMap.elems nodes
                , child <- structuralChildren node
                ]
        -- Term-DAG roots are nodes that are NOT referenced by any structure edge
    in IntSet.difference allNodeIds' referencedNodes

-- | Compute term-DAG roots under a canonicalization function.
--
-- This mirrors 'computeTermDagRoots', but first rewrites each node/child
-- through the provided canonicalization function and deduplicates via IntSet.
computeTermDagRootsUnder :: (NodeId -> NodeId) -> Constraint -> IntSet
computeTermDagRootsUnder canonical c =
    let nodes = cNodes c
        rootIdOf = getNodeId . canonical
        allRoots = IntSet.fromList [rootIdOf (NodeId nid) | nid <- IntMap.keys nodes]
        referencedRoots =
            IntSet.fromList
                [ rootIdOf child
                | node <- IntMap.elems nodes
                , child <- structuralChildren node
                ]
    in IntSet.difference allRoots referencedRoots

-- | Get all NodeIds in the constraint.
allNodeIds :: Constraint -> [NodeId]
allNodeIds c = map NodeId $ IntMap.keys (cNodes c)

-- | Check if a node is "upper" than another in the term-DAG.
--
-- A node @parent@ is upper than @child@ if @parent@ can reach @child@ by
-- following term-DAG structure edges (TyArrow dom/cod, TyForall body, TyExp body).
--
-- This is the paper's definition from §3.1: binding edges must point "upward"
-- in the term-DAG structure.
--
-- Note: A node is considered upper than itself (reflexive).
isUpper :: Constraint -> NodeId -> NodeId -> Bool
isUpper c parent child
    | parent == child = True  -- Reflexive: a node is upper than itself
    | otherwise = go IntSet.empty parent
  where
    go visited (NodeId nid)
        | IntSet.member nid visited = False  -- Avoid infinite loops on cycles
        | NodeId nid == child = True         -- Found the child
        | otherwise = case IntMap.lookup nid (cNodes c) of
            Nothing -> False  -- Node doesn't exist
            Just node -> 
                let visited' = IntSet.insert nid visited
                    children = structuralChildren node
                in any (go visited') children
