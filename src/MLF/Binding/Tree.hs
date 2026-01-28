{- |
Module      : MLF.Binding.Tree
Description : Paper-style binding tree operations
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the binding-tree data model and operations from
@papers/these-finale-english.txt@ (see @papers/xmlf.txt@ §3.1). The binding tree is an explicit representation of
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
    boundFlexChildrenAllUnder,
    boundFlexChildrenUnder,
    orderedBinders,
    forallSpecFromForall,
    -- * Root detection
    bindingRoots,
    isBindingRoot,
    computeTermDagRoots,
    computeTermDagRootsUnder,
    -- * Gen-node scheme roots
    rebuildGenNodesFromBinding,
    -- * Path operations
    bindingPathToRoot,
    bindingPathToRootLocal,
    bindingLCA,
    canonicalizeBindParentsUnder,
    -- * Interior computation
    interiorOf,
    interiorOfUnder,
    -- * Invariant checking
    checkBindingTree,
    checkBindingTreeUnder,
    checkNoGenFallback,
    checkSchemeClosure,
    checkSchemeClosureUnder,
    isUpper,
    -- * Node classification (paper §3.1)
    NodeKind(..),
    nodeKind,
    isUnderRigidBinder,
    -- * Utilities
    allNodeIds,
) where

import Control.Monad (foldM, forM, forM_, unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (sortBy)
import Data.Maybe (listToMaybe, mapMaybe)

import qualified MLF.Util.OrderKey as OrderKey
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Types
import qualified MLF.Constraint.Traversal as Traversal

allNodeRefs :: Constraint -> [NodeRef]
allNodeRefs c =
    map (TypeRef . NodeId) (IntMap.keys (cNodes c)) ++
    map (GenRef . GenNodeId) (IntMap.keys (cGenNodes c))

-- | Node keys that participate in binding-tree invariants.
--
-- With gen-rooted constraints, all type and gen nodes are considered live.
liveNodeKeys :: Constraint -> IntSet
liveNodeKeys c =
    let typeKeys = IntSet.fromList
            [ nodeRefKey (TypeRef (NodeId nid))
            | nid <- IntMap.keys (cNodes c)
            ]
        genKeys = IntSet.fromList
            [ nodeRefKey (GenRef (GenNodeId gid))
            | gid <- IntMap.keys (cGenNodes c)
            ]
    in IntSet.union typeKeys genKeys

nodeRefExists :: Constraint -> NodeRef -> Bool
nodeRefExists c ref = case ref of
    TypeRef nid -> IntMap.member (getNodeId nid) (cNodes c)
    GenRef gid -> IntMap.member (getGenNodeId gid) (cGenNodes c)

canonicalRef :: (NodeId -> NodeId) -> NodeRef -> NodeRef
canonicalRef = Canonicalize.canonicalRef

-- | Build type-level structure edges from nodes.
--
-- This is a shared helper that builds a map from parent node key to the set
-- of child node keys, based on structural children (TyArrow dom/cod, TyForall body, etc.).
-- Self-edges are automatically removed.
buildTypeEdgesFrom
    :: (NodeId -> Int)  -- ^ Key mapping function (e.g., getNodeId or nodeRefKey . TypeRef)
    -> IntMap.IntMap TyNode
    -> IntMap.IntMap IntSet
buildTypeEdgesFrom toKey nodes =
    foldl' addOne IntMap.empty (IntMap.elems nodes)
  where
    addOne m node =
        let parentKey = toKey (tnId node)
            childKeys = IntSet.fromList [ toKey child | child <- structuralChildrenWithBounds node ]
            childKeys' = IntSet.delete parentKey childKeys  -- Remove self-edges
        in if IntSet.null childKeys'
            then m
            else IntMap.insertWith IntSet.union parentKey childKeys' m

-- | Build scope map from type nodes to their gen ancestors.
--
-- Returns a map from gen node ID to the set of type node IDs bound under it.
buildScopeNodesFromPaths
    :: (NodeRef -> Either BindingError [NodeRef])  -- ^ Path lookup function
    -> [Int]  -- ^ Type node IDs to process
    -> Either BindingError (IntMap.IntMap IntSet)
buildScopeNodesFromPaths pathLookup typeIds =
    foldM addOne IntMap.empty typeIds
  where
    addOne m nidInt = do
        path <- pathLookup (typeRef (NodeId nidInt))
        let gens = [ gid | GenRef gid <- path ]
        when (null gens) $
            Left (MissingBindParent (typeRef (NodeId nidInt)))
        pure $
            foldl'
                (\acc gid ->
                    IntMap.insertWith
                        IntSet.union
                        (getGenNodeId gid)
                        (IntSet.singleton nidInt)
                        acc
                )
                m
                gens

-- | Compute structural roots within a scope set.
--
-- Given a scope (set of node IDs) and type edges, returns the IDs of nodes
-- in the scope that are not referenced by any other node in the scope.
rootsForScope
    :: (Int -> Int)  -- ^ Parent key mapping
    -> (Int -> Maybe Int)  -- ^ Child key extraction (returns Nothing to skip)
    -> IntMap.IntMap IntSet  -- ^ Type edges
    -> IntSet  -- ^ Scope set
    -> IntSet
rootsForScope parentKey childKey typeEdges scopeSet =
    let referenced =
            IntSet.fromList
                [ cid
                | nidInt <- IntSet.toList scopeSet
                , let pkey = parentKey nidInt
                , childRawKey <- IntSet.toList (IntMap.findWithDefault IntSet.empty pkey typeEdges)
                , Just cid <- [childKey childRawKey]
                , IntSet.member cid scopeSet
                ]
    in IntSet.difference scopeSet referenced

-- | Find the first gen ancestor of a node by walking up the binding path.
--
-- Returns the first GenNodeId encountered after the starting node, or Nothing
-- if the node is under no gen node.
firstGenAncestorFrom
    :: (NodeRef -> Either BindingError [NodeRef])  -- ^ Binding path lookup
    -> NodeRef  -- ^ Starting node
    -> Maybe GenNodeId
firstGenAncestorFrom pathLookup ref =
    case pathLookup ref of
        Left _ -> Nothing
        Right path -> listToMaybe [gid | GenRef gid <- drop 1 path]

-- | Paper node kinds (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1).
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
lookupBindParent :: Constraint -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent c ref = IntMap.lookup (nodeRefKey ref) (cBindParents c)

-- | Helper: run an action with canonicalized binding parents and validated node.
--
-- This is the shared core for quotient-aware binding-tree operations.
-- It canonicalizes the node, computes the quotient bind parents, validates
-- that the canonical node exists, and runs the given action.
withQuotientBindParents
    :: String  -- ^ Error context for validation failures
    -> (NodeId -> NodeId)
    -> Constraint
    -> NodeRef
    -> (NodeRef -> BindParents -> Either BindingError a)
    -> Either BindingError a
withQuotientBindParents errCtx canonical c0 ref0 f = do
    let refC = canonicalRef canonical ref0
    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    unless (IntSet.member (nodeRefKey refC) allRoots) $
        Left $ InvalidBindingTree $ errCtx ++ ": node " ++ show refC ++ " not in constraint"
    f refC bindParents

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
    -> NodeRef
    -> Either BindingError (Maybe (NodeRef, BindFlag))
lookupBindParentUnder canonical c0 ref0 =
    withQuotientBindParents "lookupBindParentUnder" canonical c0 ref0 $ \refC bindParents ->
        pure (IntMap.lookup (nodeRefKey refC) bindParents)

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
setBindParent :: NodeRef -> (NodeRef, BindFlag) -> Constraint -> Constraint
setBindParent child parentInfo c =
    c { cBindParents = IntMap.insert (nodeRefKey child) parentInfo (cBindParents c) }

-- | Remove the binding parent for a node, making it a root.
removeBindParent :: NodeRef -> Constraint -> Constraint
removeBindParent child c =
    c { cBindParents = IntMap.delete (nodeRefKey child) (cBindParents c) }

-- | Generic bound children collector with configurable filtering.
--
-- This is the shared core for boundFlexChildren variants, parameterized by:
--   - A filter function for child nodes (returns Just nodeId to include, Nothing to skip)
--   - A flag predicate (returns True if the bind flag is acceptable)
--   - The constraint to operate on
--   - The bind parents map to use
--   - The binder node reference
--   - An error context string for error messages
collectBoundChildrenWithFlag
    :: (NodeRef -> Maybe NodeId)  -- ^ Child node filter
    -> (BindFlag -> Bool)         -- ^ Flag predicate
    -> Constraint
    -> BindParents
    -> NodeRef
    -> String
    -> Either BindingError [NodeId]
collectBoundChildrenWithFlag childFilter flagOk c bindParents binder errCtx =
    reverse <$> foldM
        (\acc (childKey, (parent, flag)) ->
            if parent /= binder || not (flagOk flag)
                then pure acc
                else
                    let childRef = nodeRefFromKey childKey
                    in case childRef of
                        TypeRef childN ->
                            case NodeAccess.lookupNode c childN of
                                Nothing ->
                                    Left $
                                        InvalidBindingTree $
                                            errCtx ++ ": child " ++ show childN ++ " not in cNodes"
                                Just _ ->
                                    maybe (pure acc) (\nid -> pure (nid : acc)) (childFilter (TypeRef childN))
                        GenRef gid ->
                            if IntMap.member (getGenNodeId gid) (cGenNodes c)
                                then pure acc
                                else
                                    Left $
                                        InvalidBindingTree $
                                            errCtx ++ ": child " ++ show gid ++ " not in cGenNodes"
        )
        []
        (IntMap.toList bindParents)

-- | Convenience wrapper for collectBoundChildrenWithFlag that only accepts BindFlex.
collectBoundChildren
    :: (NodeRef -> Maybe NodeId)
    -> Constraint
    -> BindParents
    -> NodeRef
    -> String
    -> Either BindingError [NodeId]
collectBoundChildren childFilter =
    collectBoundChildrenWithFlag childFilter (== BindFlex)

tyVarChildFilter :: Constraint -> NodeRef -> Maybe NodeId
tyVarChildFilter c ref = case ref of
    TypeRef nid ->
        case NodeAccess.lookupNode c nid of
            Just TyVar{} -> Just nid
            _ -> Nothing
    _ -> Nothing

-- | Direct flexibly-bound TyVar children of a binder node.
--
-- This corresponds to Q(n) in the paper, restricted to variable nodes.
boundFlexChildren :: Constraint -> NodeRef -> Either BindingError [NodeId]
boundFlexChildren c binder = do
    unless (nodeRefExists c binder) $
        Left $
            InvalidBindingTree $
                "boundFlexChildren: binder " ++ show binder ++ " not in constraint"
    collectBoundChildren (tyVarChildFilter c) c (cBindParents c) binder "boundFlexChildren"

-- | Quotient-aware variant of 'boundFlexChildren' (canonicalized binding tree).
boundFlexChildrenUnder
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeRef
    -> Either BindingError [NodeId]
boundFlexChildrenUnder canonical c0 binder0 =
    withQuotientBindParents "boundFlexChildrenUnder" canonical c0 binder0 $ \binderC bindParents ->
        collectBoundChildren (tyVarChildFilter c0) c0 bindParents binderC "boundFlexChildrenUnder"

-- | Direct flexibly-bound children (any node type) of a binder node, under a
-- canonicalization function.
--
-- TyExp is internal and skipped; TyBase/TyBottom are atomic and not quantified
-- in the xMLF translation.
boundFlexChildrenAllUnder
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeRef
    -> Either BindingError [NodeId]
boundFlexChildrenAllUnder canonical c0 binder0 =
    withQuotientBindParents "boundFlexChildrenAllUnder" canonical c0 binder0 $ \binderC bindParents ->
        let allFilter ref = case ref of
                TypeRef nid ->
                    case NodeAccess.lookupNode c0 nid of
                        Just TyExp{} -> Nothing
                        Just TyBase{} -> Nothing
                        Just TyBottom{} -> Nothing
                        Just _ -> Just nid
                        Nothing -> Nothing
                _ -> Nothing
        in collectBoundChildren allFilter c0 bindParents binderC "boundFlexChildrenAllUnder"

-- | Ordered binders for a binder node (leftmost-lowermost, paper ≺).
--
-- Binders are filtered to those reachable from the binder’s “order root”:
--   • for a `TyForall`, the body root (paper binding-node translation),
--   • otherwise, the binder node itself (useful for top-level/generalization).
--
-- Note: for `TyForall`, we include both flexibly and rigidly bound TyVar
-- children. Rigidification can flip binder edges in presolution, but the
-- quantifier still binds those variables.
orderedBinders
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeRef
    -> Either BindingError [NodeId]
orderedBinders canonical c0 binder0 = do
    let binderC = canonicalRef canonical binder0
    case binderC of
        GenRef _ ->
            Left $
                InvalidBindingTree $
                    "orderedBinders: gen-node binder " ++ show binderC ++ " not supported"
        TypeRef binderN -> do
            unless (IntMap.member (getNodeId binderN) (cNodes c0)) $
                Left $
                    InvalidBindingTree $
                        "orderedBinders: binder " ++ show binderN ++ " not in cNodes"
            let nodes = cNodes c0
                binderNode = IntMap.lookup (getNodeId binderN) nodes
                (orderRoot, includeRigid) = case binderNode of
                    Just TyForall{ tnBody = body } -> (canonical body, True)
                    _ -> (binderN, False)
                reachable =
                    Traversal.reachableFromWithBounds
                        canonical
                        (\nid -> IntMap.lookup (getNodeId nid) nodes)
                        orderRoot
            binders <- boundChildrenUnder canonical c0 (TypeRef binderN) includeRigid
            let bindersReachable =
                    filter (\nid -> IntSet.member (getNodeId nid) reachable) binders
                extraChildren nid =
                    case IntMap.lookup (getNodeId nid) nodes of
                        Just TyVar{ tnBound = Just bnd } -> [bnd]
                        _ -> []
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
            orderBindersByDeps canonical c0 orderKeys bindersReachable
  where
    boundChildrenUnder
        :: (NodeId -> NodeId)
        -> Constraint
        -> NodeRef
        -> Bool
        -> Either BindingError [NodeId]
    boundChildrenUnder canon constraint binderRef includeRigid =
        withQuotientBindParents "orderedBinders" canon constraint binderRef $ \binderC bindParents ->
            let tyVarFilter = tyVarChildFilter constraint
                flagOk flag = flag == BindFlex || (includeRigid && flag == BindRigid)
            in collectBoundChildrenWithFlag tyVarFilter flagOk constraint bindParents binderC "orderedBinders"

orderBindersByDeps
    :: (NodeId -> NodeId)
    -> Constraint
    -> IntMap.IntMap OrderKey.OrderKey
    -> [NodeId]
    -> Either BindingError [NodeId]
orderBindersByDeps canonical c0 orderKeys binders =
    let nodes = cNodes c0
        binderSet = IntSet.fromList (map getNodeId binders)
        depsFor b = case VarStore.lookupVarBound c0 b of
            Nothing -> IntSet.empty
            Just bnd -> IntSet.delete (getNodeId b) $
                IntSet.intersection binderSet (reachableFromWithBounds nodes bnd)
        depsMap = IntMap.fromList [ (getNodeId b, depsFor b) | b <- binders ]
        topoSort _ [] acc = Right acc
        topoSort done remaining acc =
            let ready = [ b | b <- remaining
                        , IntSet.isSubsetOf (IntMap.findWithDefault IntSet.empty (getNodeId b) depsMap) done
                        ]
            in case ready of
                [] -> Left $ InvalidBindingTree "orderedBinders: cycle in bound dependencies"
                _ ->
                    let readySorted = sortBy (OrderKey.compareNodesByOrderKey orderKeys) ready
                        readySet = IntSet.fromList (map getNodeId readySorted)
                    in topoSort (IntSet.union done readySet)
                                (filter (\b -> not (IntSet.member (getNodeId b) readySet)) remaining)
                                (acc ++ readySorted)
    in topoSort IntSet.empty binders []
  where
    reachableFromWithBounds nodes0 root0 =
        Traversal.reachableFromNodes canonical children [root0]
      where
        children nid = maybe [] structuralChildrenWithBounds (IntMap.lookup (getNodeId nid) nodes0)


-- | Compute a ForallSpec (binder count + bounds) for a forall node.
forallSpecFromForall
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Either BindingError ForallSpec
forallSpecFromForall canonical c0 binder0 = do
    binders <- orderedBinders canonical c0 (TypeRef binder0)
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

-- | Check if a node is a binding root.
isBindingRoot :: Constraint -> NodeRef -> Bool
isBindingRoot c ref = not $ IntMap.member (nodeRefKey ref) (cBindParents c)

-- | Compute the set of binding roots (nodes with no binding parent).
bindingRoots :: Constraint -> [NodeRef]
bindingRoots c = filter (isBindingRoot c) (allNodeRefs c)

-- | Trace the binding-parent chain from a node to a root.
--
-- Returns the path as a list of NodeRefs, starting with the given node
-- and ending with a root. Returns an error if a cycle is detected.
bindingPathToRoot :: Constraint -> NodeRef -> Either BindingError [NodeRef]
bindingPathToRoot c =
    bindingPathToRootWithLookup (\key -> IntMap.lookup key (cBindParents c))

-- | Generic binding path tracing with a configurable parent lookup.
--
-- This is the shared core for bindingPathToRoot variants.
bindingPathToRootWithLookup
    :: (Int -> Maybe (NodeRef, BindFlag))  -- ^ Parent lookup by key
    -> NodeRef
    -> Either BindingError [NodeRef]
bindingPathToRootWithLookup lookupParent start =
    go IntSet.empty [start] (nodeRefKey start)
  where
    go visited path key
        | IntSet.member key visited =
            Left $ BindingCycleDetected (reverse path)
        | otherwise =
            case lookupParent key of
                Nothing -> Right (reverse path)
                Just (parentRef, _flag) ->
                    go (IntSet.insert key visited) (parentRef : path) (nodeRefKey parentRef)

bindingPathToRootLocal :: BindParents -> NodeRef -> Either BindingError [NodeRef]
bindingPathToRootLocal bindParents =
    bindingPathToRootWithLookup (\key -> IntMap.lookup key bindParents)

-- | Compute the lowest common ancestor of two nodes in the binding tree.
--
-- Returns an error if either node has a cycle in its binding path.
bindingLCA :: Constraint -> NodeRef -> NodeRef -> Either BindingError NodeRef
bindingLCA c n1 n2 = do
    path1 <- bindingPathToRoot c n1
    path2 <- bindingPathToRoot c n2
    let set1 = IntSet.fromList $ map nodeRefKey path1
        -- Find the first node in path2 that is also in path1
        common = filter (\ref -> IntSet.member (nodeRefKey ref) set1) path2
    case common of
        [] -> Left $ NoCommonAncestor n1 n2
        (lca:_) -> Right lca

-- | Classify a node according to the paper’s instantiable/restricted/locked
-- taxonomy (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1).
--
--   - Root: no binding parent.
--   - Restricted: the node’s own binding edge is rigid.
--   - Locked: the node’s own edge is flexible, but some strict ancestor edge is rigid.
--   - Instantiable: the entire binding path to the root is flexible.
nodeKind :: Constraint -> NodeRef -> Either BindingError NodeKind
nodeKind c nid = case lookupBindParent c nid of
    Nothing                     -> Right NodeRoot
    Just (_, BindRigid)         -> Right NodeRestricted
    Just (_, BindFlex)          -> do
        underRigid <- isUnderRigidBinder c nid
        pure $ if underRigid then NodeLocked else NodeInstantiable

-- | True iff @nid@ is strictly under some rigid binding edge.
--
-- This intentionally does /not/ treat a restricted node as “under rigid” purely
-- because its own binding edge is rigid: only strict ancestors are considered.
--
-- Paper anchor (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4):
-- normalized witnesses do not perform operations under rigidly bound nodes.
isUnderRigidBinder :: Constraint -> NodeRef -> Either BindingError Bool
isUnderRigidBinder c nid =
    any (\ref -> case lookupBindParent c ref of Just (_, BindRigid) -> True; _ -> False)
    . drop 1 <$> bindingPathToRoot c nid

-- | Compute the interior I(r): all nodes transitively bound to r.
--
-- This includes r itself and all nodes whose binding path passes through r.
interiorOf :: Constraint -> NodeRef -> Either BindingError IntSet
interiorOf c root = do
    unless (nodeRefExists c root) $
        Left $ InvalidBindingTree $ "interiorOf: root " ++ show root ++ " not in constraint"
    paths <- forM (allNodeRefs c) $ \ref -> do
        path <- bindingPathToRoot c ref
        pure (ref, path)
    pure $ IntSet.fromList [ nodeRefKey ref | (ref, path) <- paths, root `elem` path ]

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
    -> NodeRef
    -> Either BindingError IntSet
interiorOfUnder canonical c0 root0 =
    withQuotientBindParents "interiorOfUnder" canonical c0 root0 $ \rootC bindParents ->
        let childrenByParent :: IntMap.IntMap IntSet
            childrenByParent =
                foldl'
                    (\m (childRootKey, (parentRoot, _flag)) ->
                        let parentRootKey = nodeRefKey parentRoot
                        in IntMap.insertWith IntSet.union parentRootKey (IntSet.singleton childRootKey) m
                    )
                    IntMap.empty
                    (IntMap.toList bindParents)

            go :: IntSet -> [Int] -> IntSet
            go visited [] = visited
            go visited (nid : rest) =
                let kids = IntMap.findWithDefault IntSet.empty nid childrenByParent
                    newKids = IntSet.difference kids visited
                in go (IntSet.union visited newKids) (IntSet.toList newKids ++ rest)

        in pure (go (IntSet.singleton (nodeRefKey rootC)) [nodeRefKey rootC])

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
    let bindParents0 = cBindParents c0

        allRoots :: IntSet
        allRoots =
            IntSet.fromList
                [ nodeRefKey (canonicalRef canonical ref)
                | ref <- allNodeRefs c0
                ]

        entries0 =
            [ (childRootKey, (parentRoot, flag))
            | (childKey, (parent0, flag)) <- IntMap.toList bindParents0
            , let childRef0 = nodeRefFromKey childKey
            , let childRoot = canonicalRef canonical childRef0
            , let parentRoot = canonicalRef canonical parent0
            , childRoot /= parentRoot
            , let childRootKey = nodeRefKey childRoot
            , IntSet.member childRootKey allRoots
            , IntSet.member (nodeRefKey parentRoot) allRoots
            ]

        -- Union-find canonicalization can transiently create multiple
        -- binding parents for the same canonical node. We resolve this
        -- deterministically by keeping the first parent we saw and
        -- taking the max flag.
        insertOne bp (childRootKey, (parentRoot, flag)) =
            Right $ case IntMap.lookup childRootKey bp of
                Nothing -> IntMap.insert childRootKey (parentRoot, flag) bp
                Just (parent0, flag0) ->
                    IntMap.insert childRootKey (parent0, max flag0 flag) bp

    bindParents <- foldM insertOne IntMap.empty entries0

    -- Sanity: rewritten nodes must correspond to canonical reps of live nodes.
    forM_ (IntMap.keys bindParents) $ \childRootKey ->
        unless (IntSet.member childRootKey allRoots) $
            Left $
                InvalidBindingTree $
                    "quotientBindParentsUnder: binding child " ++ show childRootKey ++ " not in constraint"

    forM_ (IntMap.toList bindParents) $ \(childRootKey, (parentRoot, _flag)) -> do
        let parentRootKey = nodeRefKey parentRoot
        unless (IntSet.member parentRootKey allRoots) $
            Left $
                InvalidBindingTree $
                    "quotientBindParentsUnder: binding parent " ++ show parentRootKey
                        ++ " of node " ++ show childRootKey ++ " not in constraint"

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
    let bindParents = cBindParents c
        liveKeys = liveNodeKeys c
        allRefs = allNodeRefs c

    -- Check 1: Parent/child existence.
    forM_ (IntMap.toList bindParents) $ \(childKey, (parentRef, _flag)) ->
        when (IntSet.member childKey liveKeys) $
            unless (IntSet.member (nodeRefKey parentRef) liveKeys) $
                Left $
                    InvalidBindingTree $
                        "Binding parent " ++ show parentRef ++ " of node " ++ show childKey ++ " not in live constraint"

    -- Check 2: No cycles in binding paths.
    mapM_ (bindingPathToRoot c) allRefs

    -- Check 3: Gen nodes only bind under gen nodes; type nodes must be "upper".
    let checkUpperInvariant (childKey, (parentRef, _flag))
            | not (IntSet.member childKey liveKeys) = Nothing
            | otherwise =
                let childRef = nodeRefFromKey childKey
                in case childRef of
                    GenRef _ ->
                        case parentRef of
                            GenRef _ -> Nothing
                            TypeRef _ ->
                                Just $
                                    InvalidBindingTree $
                                        "Gen node bound under type node " ++ show parentRef
                    TypeRef _ ->
                        if isUpper c parentRef childRef
                            then Nothing
                            else Just $ ParentNotUpper childRef parentRef

    case mapMaybe checkUpperInvariant (IntMap.toList bindParents) of
        (err:_) -> Left err
        [] -> pure ()

    -- Check 4: Exactly one binding root, and it must be a gen node.
    rootRef <- validateSingleGenRoot (bindingRoots c)

    -- Check 4.5: Gen node scheme roots refer to live type nodes.
    validateGenSchemeRoots (cGenNodes c) (\nid -> IntMap.member (getNodeId nid) (cNodes c))

    -- Check 5: Every non-root node has a binding parent.
    let rootKey = nodeRefKey rootRef
        missingParent =
            [ ref
            | ref <- allRefs
            , nodeRefKey ref /= rootKey
            , not (IntMap.member (nodeRefKey ref) bindParents)
            ]
    case missingParent of
        (ref:_) -> Left $ MissingBindParent ref
        [] -> pure ()

    -- Check 6: All gen nodes are flexibly bound (except the root).
    validateGenNodesFlexiblyBound (cGenNodes c) (lookupBindParent c) rootRef

-- | Validate that all gen node scheme roots refer to live type nodes.
validateGenSchemeRoots
    :: IntMap.IntMap GenNode
    -> (NodeId -> Bool)  -- ^ Existence check for type nodes
    -> Either BindingError ()
validateGenSchemeRoots genNodes nodeExists =
    forM_ (IntMap.elems genNodes) $ \genNode ->
        forM_ (gnSchemes genNode) $ \root ->
            unless (nodeExists root) $
                Left $
                    InvalidBindingTree $
                        "Gen node scheme root " ++ show root ++ " not in constraint"

-- | Validate that all gen nodes (except the root) are flexibly bound.
validateGenNodesFlexiblyBound
    :: IntMap.IntMap GenNode
    -> (NodeRef -> Maybe (NodeRef, BindFlag))
    -> NodeRef
    -> Either BindingError ()
validateGenNodesFlexiblyBound genNodes lookupParent rootRef =
    forM_ (IntMap.keys genNodes) $ \gidInt -> do
        let gref = GenRef (GenNodeId gidInt)
        if gref == rootRef
            then pure ()
        else case lookupParent gref of
            Nothing -> Left $ MissingBindParent gref
            Just (_parent, BindFlex) -> pure ()
            Just (_parent, BindRigid) ->
                Left $
                    InvalidBindingTree $
                        "Gen node " ++ show gref ++ " is rigidly bound"

-- | Reject binding trees that would require a gen-ancestor fallback for
-- type-node binders (Q(n) must be direct children of the type node).
checkNoGenFallback :: Constraint -> Either BindingError ()
checkNoGenFallback c = do
    let nodes = cNodes c
        reachableFromWithBounds root0 =
            Traversal.reachableFromWithBounds
                id
                (\nid -> IntMap.lookup (getNodeId nid) nodes)
                root0

        firstGenAncestor nid = pure (firstGenAncestorFrom (bindingPathToRoot c) (typeRef nid))

    forM_ (IntMap.elems nodes) $ \node ->
        case node of
            TyForall{} -> do
                let nid = tnId node
                direct <- boundFlexChildren c (typeRef nid)
                when (null direct) $ do
                    mGen <- firstGenAncestor nid
                    forM_ mGen $ \gid -> do
                        genBinders <- boundFlexChildren c (genRef gid)
                        let orderRoot = tnBody node
                            reachable = reachableFromWithBounds orderRoot
                            reachableBinders =
                                [ b
                                | b <- genBinders
                                , IntSet.member (getNodeId b) reachable
                                ]
                        unless (null reachableBinders) $
                            Left $
                                GenFallbackRequired
                                    { fallbackBinder = nid
                                    , fallbackGen = gid
                                    , fallbackBinders = reachableBinders
                                    }
            _ -> pure ()

-- | Reject scheme roots that reach named nodes not bound under their gen node.
checkSchemeClosure :: Constraint -> Either BindingError ()
checkSchemeClosure = checkSchemeClosureUnder id

-- | Reject scheme roots that reach named nodes not bound under their gen node,
--   under a canonicalization function.
checkSchemeClosureUnder :: (NodeId -> NodeId) -> Constraint -> Either BindingError ()
checkSchemeClosureUnder canonical c0 = do
    let nodes = cNodes c0
        isNamedNode nid =
            case IntMap.lookup (getNodeId nid) nodes of
                Just TyVar{} -> not (VarStore.isEliminatedVar c0 nid)
                _ -> False
    bindParents0 <- canonicalizeBindParentsUnder canonical c0
    let bindParents = softenBindParents canonical c0 bindParents0
    let typeEdges = buildTypeEdges nodes
        schemeRootsByGen =
            IntMap.map
                (IntSet.fromList . map (getNodeId . canonical) . gnSchemes)
                (cGenNodes c0)
        schemeGensByRoot =
            IntMap.fromListWith
                IntSet.union
                [ (getNodeId (canonical root), IntSet.singleton (getGenNodeId (gnId gen)))
                | gen <- NodeAccess.allGenNodes c0
                , root <- gnSchemes gen
                ]
        schemeRootsSet = IntSet.unions (IntMap.elems schemeRootsByGen)
        namedNodes =
            [ canonical child
            | (childKey, (parent, flag)) <- IntMap.toList bindParents
            , GenRef _ <- [parent]
            , TypeRef child <- [nodeRefFromKey childKey]
            , flag == BindFlex
            , isNamedNode (canonical child)
            , not (IntSet.member (getNodeId (canonical child)) schemeRootsSet)
            ]
        reachableFromWithBounds root0 =
            let go visited [] = visited
                go visited (nid0:rest) =
                    let nid = canonical nid0
                        key = getNodeId nid
                    in if IntSet.member key visited
                        then go visited rest
                        else
                            let visited' = IntSet.insert key visited
                                kids =
                                    [ NodeId childId
                                    | childId <- IntSet.toList (IntMap.findWithDefault IntSet.empty key typeEdges)
                                    ]
                            in go visited' (kids ++ rest)
            in go IntSet.empty [canonical root0]
        firstGenAncestorFor nid =
            firstGenAncestorFrom (bindingPathToRootLocal bindParents) (typeRef (canonical nid))
        boundUnderGen allowedGens nid =
            case firstGenAncestorFor nid of
                Just gid' -> IntSet.member (getGenNodeId gid') allowedGens
                Nothing -> True

    let schemeRoots =
            [ NodeId nid
            | nid <- IntSet.toList (IntSet.unions (IntMap.elems schemeRootsByGen))
            ]
    forM_ schemeRoots $ \root ->
        forM_ (schemeGenForRoot bindParents root) $ \gid' -> do
            let reachable = reachableFromWithBounds root
                nestedSchemeRoots =
                    [ r
                    | r <- schemeRoots
                    , canonical r /= canonical root
                    , IntSet.member (getNodeId (canonical r)) reachable
                    ]
                nestedReachable =
                    IntSet.unions (map reachableFromWithBounds nestedSchemeRoots)
                allowedGens =
                    IntMap.findWithDefault
                        IntSet.empty
                        (getNodeId (canonical root))
                        schemeGensByRoot
                ancestorGens =
                    case bindingPathToRootLocal bindParents (GenRef gid') of
                        Right path -> IntSet.fromList [ getGenNodeId pathGid | GenRef pathGid <- path ]
                        Left _ -> IntSet.singleton (getGenNodeId gid')
                allowedGens' = IntSet.unions [allowedGens, ancestorGens]
                freeNodes0 =
                    [ n
                    | n <- namedNodes
                    , IntSet.member (getNodeId (canonical n)) reachable
                    , not (IntSet.member (getNodeId (canonical n)) nestedReachable)
                    , not (boundUnderGen allowedGens' n)
                    ]
            unless (null freeNodes0) $
                Left $
                    GenSchemeFreeVars
                        { schemeRoot = canonical root
                        , schemeGen = gid'
                        , freeNodes = freeNodes0
                        }
  where
    buildTypeEdges nodes0 =
        buildTypeEdgesFrom (getNodeId . canonical) nodes0

    softenBindParents canonical' constraint =
        let weakened = cWeakenedVars constraint
            softenOne childKey (parent, flag) =
                case (flag, nodeRefFromKey childKey) of
                    (BindRigid, TypeRef childN)
                        | IntSet.member (getNodeId (canonical' childN)) weakened ->
                            (parent, BindFlex)
                    _ -> (parent, flag)
        in IntMap.mapWithKey softenOne

    schemeGenForRoot bindParents' root0 =
        firstGenAncestorFrom (bindingPathToRootLocal bindParents') (typeRef (canonical root0))

-- | Validate that there is exactly one binding root and it is a gen node.
validateSingleGenRoot :: [NodeRef] -> Either BindingError NodeRef
validateSingleGenRoot roots = case roots of
    [GenRef root] -> pure (GenRef root)
    [TypeRef _] ->
        Left $ InvalidBindingTree "Binding root is a type node (expected gen root)"
    [] ->
        Left $ InvalidBindingTree "Binding tree has no root"
    _ ->
        Left $ InvalidBindingTree ("Binding tree has multiple roots: " ++ show roots)

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
        genNodes0 = cGenNodes c0

    validateGenSchemeRoots genNodes0 (\nid -> IntMap.member (getNodeId (canonical nid)) nodes0)

    (_allRoots, bindParents0) <- quotientBindParentsUnder canonical c0

    let typeEdges :: IntMap.IntMap IntSet
        typeEdges =
            buildTypeEdgesFrom (nodeRefKey . TypeRef . canonical) nodes0

        genNodesDerived = do
            let genIds = IntMap.keys genNodes0
                allTypeIds =
                    [ getNodeId (canonical (NodeId nid))
                    | nid <- IntMap.keys nodes0
                    ]
                pathLookup = bindingPathToRootWithLookup (\key -> IntMap.lookup key bindParents0)

            scopeNodes <- buildScopeNodesFromPaths pathLookup allTypeIds

            let extractChild childKey = case nodeRefFromKey childKey of
                    TypeRef child -> Just (getNodeId child)
                    _ -> Nothing

                rebuildOne gidInt =
                    let gid = GenNodeId gidInt
                        scopeSet = IntMap.findWithDefault IntSet.empty gidInt scopeNodes
                        roots = rootsForScope (nodeRefKey . TypeRef . NodeId) extractChild typeEdges scopeSet
                    in (gidInt, GenNode gid (map NodeId (IntSet.toList roots)))

            pure (IntMap.fromList (map rebuildOne genIds))

    genNodes1 <- genNodesDerived

    let addGenEdges :: IntMap.IntMap IntSet -> GenNode -> IntMap.IntMap IntSet
        addGenEdges m genNode =
            let parentKey = nodeRefKey (GenRef (gnId genNode))
                childKeys =
                    IntSet.fromList
                        [ nodeRefKey (TypeRef (canonical child))
                        | child <- gnSchemes genNode
                        ]
            in if IntSet.null childKeys
                then m
                else IntMap.insertWith IntSet.union parentKey childKeys m

        structEdges :: IntMap.IntMap IntSet
        structEdges =
            let genEdges = foldl' addGenEdges IntMap.empty (IntMap.elems genNodes1)
            in IntMap.unionWith IntSet.union typeEdges genEdges

        reachableTypeKeys =
            let roots0 = concatMap gnSchemes (IntMap.elems genNodes1)
                rootKeys =
                    [ nodeRefKey (TypeRef (canonical root))
                    | root <- roots0
                    ]
                allTypeKeys =
                    IntSet.fromList
                        [ nodeRefKey (TypeRef (canonical (NodeId nid)))
                        | nid <- IntMap.keys nodes0
                        ]
                go visited [] = visited
                go visited (key:rest) =
                    if IntSet.member key visited
                        then go visited rest
                        else
                            let visited' = IntSet.insert key visited
                                kids = IntSet.toList (IntMap.findWithDefault IntSet.empty key structEdges)
                            in go visited' (kids ++ rest)
                reachableKeys =
                    if IntMap.null genNodes1
                        then allTypeKeys
                        else go IntSet.empty rootKeys
            in IntSet.filter even reachableKeys

        liveKeys =
            IntSet.union
                (IntSet.fromList
                    [ nodeRefKey (GenRef (GenNodeId gid))
                    | gid <- IntMap.keys genNodes1
                    ])
                reachableTypeKeys

        isUpperUnder parent child =
            let parentKey = nodeRefKey (canonicalRef canonical parent)
                childKey = nodeRefKey (canonicalRef canonical child)
            in if parentKey == childKey
                then True
                else
                    let go visited nid =
                            if IntSet.member nid visited
                                then False
                                else if nid == childKey
                                    then True
                                    else
                                        let visited' = IntSet.insert nid visited
                                            kids = IntSet.toList (IntMap.findWithDefault IntSet.empty nid structEdges)
                                        in any (go visited') kids
                    in go IntSet.empty parentKey

    let bindParents =
            IntMap.filterWithKey (\childKey _ -> IntSet.member childKey liveKeys) bindParents0

    -- Child/parent existence (roots must correspond to some live node).
    forM_ (IntMap.toList bindParents) $ \(childKey, (parentRef, _flag)) -> do
        unless (IntSet.member childKey liveKeys) $
            Left $ InvalidBindingTree ("Binding child " ++ show childKey ++ " not in constraint")
        unless (IntSet.member (nodeRefKey parentRef) liveKeys) $
            Left $
                InvalidBindingTree $
                    "Binding parent " ++ show parentRef
                        ++ " of node " ++ show childKey ++ " not in constraint"

    -- Cycle check on the canonical binding-parent relation.
    mapM_ (bindingPathToRootWithLookup (\key -> IntMap.lookup key bindParents) . nodeRefFromKey) (IntMap.keys bindParents)

    -- Upper-than check on the quotient structure graph (gen nodes must bind under gen nodes).
    forM_ (IntMap.toList bindParents) $ \(childKey, (parentRef, _flag)) -> do
        let childRef = nodeRefFromKey childKey
        case childRef of
            GenRef _ ->
                case parentRef of
                    GenRef _ -> pure ()
                    TypeRef _ ->
                        Left $
                            InvalidBindingTree $
                                "Gen node bound under type node " ++ show parentRef
            TypeRef _ ->
                case parentRef of
                    GenRef _ -> pure ()
                    _ ->
                        unless (isUpperUnder parentRef childRef) $
                            Left (ParentNotUpper childRef parentRef)

    -- Root must be a single gen node.
    let roots =
            [ nodeRefFromKey key
            | key <- IntSet.toList liveKeys
            , not (IntMap.member key bindParents)
            ]
    rootRef <- validateSingleGenRoot roots

    -- Every non-root needs a parent.
    let rootKey = nodeRefKey rootRef
    forM_ (IntSet.toList liveKeys) $ \key ->
        unless (key == rootKey || IntMap.member key bindParents) $
            Left (MissingBindParent (nodeRefFromKey key))

    -- All gen nodes (except root) must be flexibly bound.
    let lookupParent ref = IntMap.lookup (nodeRefKey ref) bindParents
    validateGenNodesFlexiblyBound genNodes0 lookupParent rootRef

    pure ()

-- | Recompute gen-node scheme roots from the binding tree and term structure.
--
-- Scheme roots are the structural roots of each gen node's scope as determined
-- by the binding tree. Root-only graphs keep empty schemes.
rebuildGenNodesFromBinding :: Constraint -> Either BindingError (IntMap.IntMap GenNode)
rebuildGenNodesFromBinding c0
    | IntMap.null nodes0 =
        pure $
            IntMap.map
                (\g -> g { gnSchemes = [] })
                genNodes0
    | otherwise = do
        let allTypeIds = IntMap.keys nodes0
        scopeNodes <- buildScopeNodesFromPaths (bindingPathToRoot c0) allTypeIds
        let typeEdges = buildTypeEdgesFrom getNodeId nodes0
            extractChild cid = Just cid

            rebuildOne g =
                let gidInt = getGenNodeId (gnId g)
                    scopeSet = IntMap.findWithDefault IntSet.empty gidInt scopeNodes
                    roots = rootsForScope id extractChild typeEdges scopeSet
                    schemes' = map NodeId (IntSet.toList roots)
                in g { gnSchemes = schemes' }

        pure (IntMap.map rebuildOne genNodes0)
  where
    nodes0 = cNodes c0
    genNodes0 = cGenNodes c0

-- | Compute the set of term-DAG root nodes.
--
-- A term-DAG root is a node that has no incoming structure edge, i.e., no other
-- node points to it via TyArrow dom/cod, TyForall body, or TyExp body.
computeTermDagRoots :: Constraint -> IntSet
computeTermDagRoots c =
    let nodes = cNodes c
        referencedNodes =
            IntSet.fromList
                [ getNodeId child
                | node <- IntMap.elems nodes
                , child <- structuralChildrenWithBounds node
                ]
    in IntSet.difference (IntSet.fromList $ IntMap.keys nodes) referencedNodes

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
                [ childRoot
                | node <- IntMap.elems nodes
                , let parentRoot = rootIdOf (tnId node)
                , child <- structuralChildrenWithBounds node
                , let childRoot = rootIdOf child
                , childRoot /= parentRoot
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
isUpper :: Constraint -> NodeRef -> NodeRef -> Bool
isUpper c parent child
    | parent == child = True  -- Reflexive: a node is upper than itself
    | otherwise =
        case parent of
            GenRef _ -> True
            _ -> go IntSet.empty parent
  where
    go visited ref
        | IntSet.member (nodeRefKey ref) visited = False  -- Avoid cycles
        | ref == child = True
        | otherwise =
            let visited' = IntSet.insert (nodeRefKey ref) visited
                children = case ref of
                    TypeRef nid ->
                        maybe [] (map TypeRef . structuralChildrenWithBounds) (NodeAccess.lookupNode c nid)
                    GenRef gid ->
                        maybe [] (map TypeRef . gnSchemes) (IntMap.lookup (getGenNodeId gid) (cGenNodes c))
            in any (go visited') children
