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
    boundFlexChildrenAllUnder,
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

import qualified MLF.Util.OrderKey as OrderKey
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Types

allNodeRefs :: Constraint -> [NodeRef]
allNodeRefs c =
    let typeRefs = map (TypeRef . NodeId) (IntMap.keys (cNodes c))
        genRefs = map (GenRef . GenNodeId) (IntMap.keys (cGenNodes c))
    in typeRefs ++ genRefs

-- | Type-node ids that are reachable from any gen-node scheme root.
--
-- Detached nodes (unreachable from gen roots) are excluded so they do not
-- participate in binding-tree invariants.
liveTypeIds :: Constraint -> IntSet
liveTypeIds c =
    let nodes = cNodes c
        roots = concatMap gnSchemes (IntMap.elems (cGenNodes c))
        allIds = IntSet.fromList (IntMap.keys nodes)
        go visited [] = visited
        go visited (nid0:rest) =
            let nid = nid0
                key = getNodeId nid
            in if IntSet.member key visited
                then go visited rest
                else
                    let visited' = IntSet.insert key visited
                        kids =
                            case IntMap.lookup key nodes of
                                Nothing -> []
                                Just node ->
                                    let boundKids =
                                            case node of
                                                TyVar{ tnBound = Just bnd } -> [bnd]
                                                _ -> []
                                    in structuralChildren node ++ boundKids
                    in go visited' (kids ++ rest)
        reachable =
            if IntMap.null (cGenNodes c)
                then allIds
                else go IntSet.empty roots
    in reachable

liveTypeKeys :: Constraint -> IntSet
liveTypeKeys c =
    IntSet.fromList
        [ nodeRefKey (TypeRef (NodeId nid))
        | nid <- IntSet.toList (liveTypeIds c)
        ]

liveNodeKeys :: Constraint -> IntSet
liveNodeKeys c =
    let genKeys =
            IntSet.fromList
                [ nodeRefKey (GenRef (GenNodeId gid))
                | gid <- IntMap.keys (cGenNodes c)
                ]
    in IntSet.union genKeys (liveTypeKeys c)

nodeRefExists :: Constraint -> NodeRef -> Bool
nodeRefExists c ref = case ref of
    TypeRef nid -> IntMap.member (getNodeId nid) (cNodes c)
    GenRef gid -> IntMap.member (getGenNodeId gid) (cGenNodes c)

canonicalRef :: (NodeId -> NodeId) -> NodeRef -> NodeRef
canonicalRef canonical ref = case ref of
    TypeRef nid -> TypeRef (canonical nid)
    GenRef _ -> ref

structuralChildrenRef :: Constraint -> NodeRef -> [NodeRef]
structuralChildrenRef c ref = case ref of
    TypeRef nid ->
        case IntMap.lookup (getNodeId nid) (cNodes c) of
            Nothing -> []
            Just node ->
                let boundKids = case node of
                        TyVar{ tnBound = Just bnd } -> [bnd]
                        _ -> []
                in map TypeRef (structuralChildren node ++ boundKids)
    GenRef gid ->
        case IntMap.lookup (getGenNodeId gid) (cGenNodes c) of
            Nothing -> []
            Just genNode -> map TypeRef (gnSchemes genNode)

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
lookupBindParent :: Constraint -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent c ref = IntMap.lookup (nodeRefKey ref) (cBindParents c)

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
lookupBindParentUnder canonical c0 ref0 = do
    let refC = canonicalRef canonical ref0
        refKey = nodeRefKey refC
    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    unless (IntSet.member refKey allRoots) $
        Left $
            InvalidBindingTree $
                "lookupBindParentUnder: node " ++ show refC ++ " not in constraint"
    pure (IntMap.lookup refKey bindParents)

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
    let oldParent = IntMap.lookup (nodeRefKey child) (cBindParents c)
        c1 = c { cBindParents = IntMap.insert (nodeRefKey child) parentInfo (cBindParents c) }
        parentRef = fst parentInfo
        dropSchemeRoot gid nid gens0 =
            let update genNode =
                    genNode { gnSchemes = filter (/= nid) (gnSchemes genNode) }
            in IntMap.adjust update (getGenNodeId gid) gens0
        addSchemeRoot gid nid gens0 =
            let update genNode =
                    let schemes0 = gnSchemes genNode
                        schemes1 = schemes0 ++ [nid]
                        schemesDedup =
                            IntMap.elems $
                                IntMap.fromList
                                    [ (getNodeId n, n)
                                    | n <- schemes1
                                    ]
                    in genNode { gnSchemes = schemesDedup }
            in IntMap.adjust update (getGenNodeId gid) gens0
        removeOld gens0 =
            case (child, oldParent) of
                (TypeRef childN, Just (GenRef oldGid, _)) ->
                    case parentRef of
                        GenRef newGid | oldGid == newGid -> gens0
                        _ -> dropSchemeRoot oldGid childN gens0
                _ -> gens0
        addNew gens0 =
            case child of
                TypeRef childN ->
                    case parentRef of
                        GenRef gid ->
                            if isUpper c1 parentRef child
                                then gens0
                                else addSchemeRoot gid childN gens0
                        TypeRef _ -> gens0
                GenRef _ -> gens0
        gens1 = addNew (removeOld (cGenNodes c1))
    in c1 { cGenNodes = gens1 }

-- | Remove the binding parent for a node, making it a root.
removeBindParent :: NodeRef -> Constraint -> Constraint
removeBindParent child c =
    c { cBindParents = IntMap.delete (nodeRefKey child) (cBindParents c) }

-- | Direct flexibly-bound TyVar children of a binder node.
--
-- This corresponds to Q(n) in the paper, restricted to variable nodes.
boundFlexChildren :: Constraint -> NodeRef -> Either BindingError [NodeId]
boundFlexChildren c binder = do
    unless (nodeRefExists c binder) $
        Left $
            InvalidBindingTree $
                "boundFlexChildren: binder " ++ show binder ++ " not in constraint"
    reverse <$> foldM
        (\acc (childKey, (parent, flag)) ->
            if parent /= binder || flag /= BindFlex
                then pure acc
                else
                    let childRef = nodeRefFromKey childKey
                    in case childRef of
                        TypeRef childN ->
                            case IntMap.lookup (getNodeId childN) (cNodes c) of
                                Just TyVar{} -> pure (childN : acc)
                                Just _ -> pure acc
                                Nothing ->
                                    Left $
                                        InvalidBindingTree $
                                            "boundFlexChildren: child " ++ show childN ++ " not in cNodes"
                        GenRef gid ->
                            if IntMap.member (getGenNodeId gid) (cGenNodes c)
                                then pure acc
                                else
                                    Left $
                                        InvalidBindingTree $
                                            "boundFlexChildren: child " ++ show gid ++ " not in cGenNodes"
        )
        []
        (IntMap.toList (cBindParents c))

-- | Quotient-aware variant of 'boundFlexChildren' (canonicalized binding tree).
boundFlexChildrenUnder
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeRef
    -> Either BindingError [NodeId]
boundFlexChildrenUnder canonical c0 binder0 = do
    let binderC = canonicalRef canonical binder0
    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    unless (IntSet.member (nodeRefKey binderC) allRoots) $
        Left $
            InvalidBindingTree $
                "boundFlexChildrenUnder: binder " ++ show binderC ++ " not in constraint"
    reverse <$> foldM
        (\acc (childKey, (parent, flag)) ->
            if parent /= binderC || flag /= BindFlex
                then pure acc
                else
                    let childRef = nodeRefFromKey childKey
                    in case childRef of
                        TypeRef childN ->
                            case IntMap.lookup (getNodeId childN) (cNodes c0) of
                                Just TyVar{} -> pure (childN : acc)
                                Just _ -> pure acc
                                Nothing ->
                                    Left $
                                        InvalidBindingTree $
                                            "boundFlexChildrenUnder: child " ++ show childN ++ " not in cNodes"
                        GenRef gid ->
                            if IntMap.member (getGenNodeId gid) (cGenNodes c0)
                                then pure acc
                                else
                                    Left $
                                        InvalidBindingTree $
                                            "boundFlexChildrenUnder: child " ++ show gid ++ " not in cGenNodes"
        )
        []
        (IntMap.toList bindParents)

-- | Direct flexibly-bound children (any node type) of a binder node, under a
-- canonicalization function.
--
-- TyRoot/TyExp are internal and skipped; TyBase/TyBottom are atomic and not
-- quantified in the xMLF translation.
boundFlexChildrenAllUnder
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeRef
    -> Either BindingError [NodeId]
boundFlexChildrenAllUnder canonical c0 binder0 = do
    let binderC = canonicalRef canonical binder0
    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    unless (IntSet.member (nodeRefKey binderC) allRoots) $
        Left $
            InvalidBindingTree $
                "boundFlexChildrenAllUnder: binder " ++ show binderC ++ " not in constraint"
    reverse <$> foldM
        (\acc (childKey, (parent, flag)) ->
            if parent /= binderC || flag /= BindFlex
                then pure acc
                else
                    let childRef = nodeRefFromKey childKey
                    in case childRef of
                        TypeRef childN ->
                            case IntMap.lookup (getNodeId childN) (cNodes c0) of
                                Just TyRoot{} -> pure acc
                                Just TyExp{} -> pure acc
                                Just TyBase{} -> pure acc
                                Just TyBottom{} -> pure acc
                                Just _ -> pure (childN : acc)
                                Nothing ->
                                    Left $
                                        InvalidBindingTree $
                                            "boundFlexChildrenAllUnder: child " ++ show childN ++ " not in cNodes"
                        GenRef gid ->
                            if IntMap.member (getGenNodeId gid) (cGenNodes c0)
                                then pure acc
                                else
                                    Left $
                                        InvalidBindingTree $
                                            "boundFlexChildrenAllUnder: child " ++ show gid ++ " not in cGenNodes"
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
                orderRoot =
                    case IntMap.lookup (getNodeId binderN) nodes of
                        Just TyForall{ tnBody = body } -> canonical body
                        _ -> binderN
                reachable =
                    let go visited [] = visited
                        go visited (nid0:rest) =
                            let nid = canonical nid0
                                key = getNodeId nid
                            in if IntSet.member key visited
                                then go visited rest
                                else
                                    let visited' = IntSet.insert key visited
                                        kids =
                                            case IntMap.lookup key nodes of
                                                Nothing -> []
                                                Just node ->
                                                    let boundKids =
                                                            case node of
                                                                TyVar{ tnBound = Just bnd } -> [bnd]
                                                                _ -> []
                                                    in structuralChildren node ++ boundKids
                                    in go visited' (map canonical kids ++ rest)
                    in go IntSet.empty [orderRoot]
            binders <- boundFlexChildrenUnder canonical c0 (TypeRef binderN)
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

orderBindersByDeps
    :: (NodeId -> NodeId)
    -> Constraint
    -> IntMap.IntMap OrderKey.OrderKey
    -> [NodeId]
    -> Either BindingError [NodeId]
orderBindersByDeps canonical c0 orderKeys binders =
    let nodes = cNodes c0
        binderSet = IntSet.fromList (map getNodeId binders)
        depsFor b =
            case VarStore.lookupVarBound c0 b of
                Nothing -> IntSet.empty
                Just bnd ->
                    IntSet.delete
                        (getNodeId b)
                        (IntSet.intersection binderSet (reachableFromWithBounds nodes bnd))
        depsMap =
            IntMap.fromList
                [ (getNodeId b, depsFor b)
                | b <- binders
                ]
        go _ [] acc = Right acc
        go done remaining acc =
            let ready =
                    [ b
                    | b <- remaining
                    , let deps = IntMap.findWithDefault IntSet.empty (getNodeId b) depsMap
                    , IntSet.isSubsetOf deps done
                    ]
            in if null ready
                then Left $
                    InvalidBindingTree "orderedBinders: cycle in bound dependencies"
                else
                    let readySorted = sortBy (OrderKey.compareNodesByOrderKey orderKeys) ready
                        readySet = IntSet.fromList (map getNodeId readySorted)
                        done' = IntSet.union done readySet
                        remaining' = filter (\b -> not (IntSet.member (getNodeId b) readySet)) remaining
                    in go done' remaining' (acc ++ readySorted)
    in go IntSet.empty binders []
  where
    reachableFromWithBounds :: IntMap.IntMap TyNode -> NodeId -> IntSet.IntSet
    reachableFromWithBounds nodes0 root0 =
        let go visited [] = visited
            go visited (nid0:rest) =
                let nid = canonical nid0
                    key = getNodeId nid
                in if IntSet.member key visited
                    then go visited rest
                    else
                        let visited' = IntSet.insert key visited
                            kids =
                                case IntMap.lookup key nodes0 of
                                    Nothing -> []
                                    Just node ->
                                        let boundKids =
                                                case node of
                                                    TyVar{ tnBound = Just bnd } -> [bnd]
                                                    _ -> []
                                        in structuralChildren node ++ boundKids
                        in go visited' (map canonical kids ++ rest)
        in go IntSet.empty [canonical root0]


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

-- | Compute the set of binding roots (nodes with no binding parent).
bindingRoots :: Constraint -> [NodeRef]
bindingRoots c =
    let liveKeys = liveNodeKeys c
        hasParent ref = IntMap.member (nodeRefKey ref) (cBindParents c)
        liveRefs = filter (\ref -> IntSet.member (nodeRefKey ref) liveKeys) (allNodeRefs c)
    in filter (not . hasParent) liveRefs

-- | Check if a node is a binding root.
isBindingRoot :: Constraint -> NodeRef -> Bool
isBindingRoot c ref = not $ IntMap.member (nodeRefKey ref) (cBindParents c)

-- | Trace the binding-parent chain from a node to a root.
--
-- Returns the path as a list of NodeRefs, starting with the given node
-- and ending with a root. Returns an error if a cycle is detected.
bindingPathToRoot :: Constraint -> NodeRef -> Either BindingError [NodeRef]
bindingPathToRoot c start = go IntSet.empty [start] start
  where
    go visited path ref
        | IntSet.member (nodeRefKey ref) visited =
            Left $ BindingCycleDetected (reverse path)
        | otherwise =
            case lookupBindParent c ref of
                Nothing -> Right (reverse path)  -- Reached a root
                Just (parent, _flag) ->
                    go (IntSet.insert (nodeRefKey ref) visited) (parent : path) parent

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
-- taxonomy (`papers/xmlf.txt` §3.1).
--
--   - Root: no binding parent.
--   - Restricted: the node’s own binding edge is rigid.
--   - Locked: the node’s own edge is flexible, but some strict ancestor edge is rigid.
--   - Instantiable: the entire binding path to the root is flexible.
nodeKind :: Constraint -> NodeRef -> Either BindingError NodeKind
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
isUnderRigidBinder :: Constraint -> NodeRef -> Either BindingError Bool
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
interiorOf :: Constraint -> NodeRef -> Either BindingError IntSet
interiorOf c root = do
    unless (nodeRefExists c root) $
        Left $
            InvalidBindingTree $
                "interiorOf: root " ++ show root ++ " not in constraint"

    -- Collect all nodes whose binding path includes root
    let allRefs = allNodeRefs c
        checkNode ref = do
            path <- bindingPathToRoot c ref
            return $ root `elem` path

    -- Build the interior set (keys for NodeRef)
    results <- mapM
        (\ref -> do
            inInterior <- checkNode ref
            return (ref, inInterior)
        )
        allRefs

    return $ IntSet.fromList [nodeRefKey ref | (ref, True) <- results]

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
interiorOfUnder canonical c0 root0 = do
    let rootC = canonicalRef canonical root0

    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    unless (IntSet.member (nodeRefKey rootC) allRoots) $
        Left $
            InvalidBindingTree $
                "interiorOfUnder: root " ++ show rootC ++ " not in constraint"

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
                newKids = filter (\k -> not (IntSet.member k visited)) (IntSet.toList kids)
                visited' = foldl' (flip IntSet.insert) visited newKids
            in go visited' (newKids ++ rest)

    pure (go (IntSet.singleton (nodeRefKey rootC)) [nodeRefKey rootC])

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
            ]

        insertOne :: BindParents -> (Int, (NodeRef, BindFlag)) -> Either BindingError BindParents
        insertOne bp (childRootKey, (parentRoot, flag)) =
            case IntMap.lookup childRootKey bp of
                Nothing -> Right (IntMap.insert childRootKey (parentRoot, flag) bp)
                Just (parent0, flag0)
                    | parent0 == parentRoot ->
                        let flag' = max flag0 flag
                        in Right (IntMap.insert childRootKey (parentRoot, flag') bp)
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
                        in Right (IntMap.insert childRootKey (parent0, flag') bp)

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
        allRefs = filter (\ref -> IntSet.member (nodeRefKey ref) liveKeys) (allNodeRefs c)
        allKeys = liveKeys

    -- Check 1: Parent/child existence.
    forM_ (IntMap.toList bindParents) $ \(childKey, (parentRef, _flag)) -> do
        when (IntSet.member childKey allKeys) $ do
            unless (IntSet.member (nodeRefKey parentRef) allKeys) $
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
        [] -> Right ()

    -- Check 4: Exactly one binding root, and it must be a gen node.
    let roots = bindingRoots c
    rootRef <- case roots of
        [GenRef root] -> pure (GenRef root)
        [TypeRef _] ->
            Left $ InvalidBindingTree "Binding root is a type node (expected gen root)"
        [] ->
            Left $ InvalidBindingTree "Binding tree has no root"
        _ ->
            Left $ InvalidBindingTree ("Binding tree has multiple roots: " ++ show roots)

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
        [] -> Right ()

    -- Check 6: All gen nodes are flexibly bound (except the root).
    forM_ (IntMap.keys (cGenNodes c)) $ \gidInt -> do
        let gref = GenRef (GenNodeId gidInt)
        if gref == rootRef
            then pure ()
            else case lookupBindParent c gref of
                Nothing -> Left $ MissingBindParent gref
                Just (_parent, BindFlex) -> pure ()
                Just (_parent, BindRigid) ->
                    Left $
                        InvalidBindingTree $
                            "Gen node " ++ show gref ++ " is rigidly bound"

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

    (_allRoots, bindParents0) <- quotientBindParentsUnder canonical c0

    let addTypeEdges :: IntMap.IntMap IntSet -> TyNode -> IntMap.IntMap IntSet
        addTypeEdges m node =
            let parentKey = nodeRefKey (TypeRef (canonical (tnId node)))
                boundKids =
                    case node of
                        TyVar{ tnBound = Just bnd } -> [bnd]
                        _ -> []
                childKeys =
                    IntSet.fromList
                        [ nodeRefKey (TypeRef (canonical child))
                        | child <- structuralChildren node ++ boundKids
                        ]
            in if IntSet.null childKeys
                then m
                else IntMap.insertWith IntSet.union parentKey childKeys m

        typeEdges :: IntMap.IntMap IntSet
        typeEdges =
            foldl' addTypeEdges IntMap.empty (IntMap.elems nodes0)

        genNodesDerived = do
            let genIds = IntMap.keys genNodes0
                allTypeIds =
                    IntSet.fromList
                        [ getNodeId (canonical (NodeId nid))
                        | nid <- IntMap.keys nodes0
                        ]

                nearestGen start = go IntSet.empty start
                  where
                    go visited ref
                        | IntSet.member (nodeRefKey ref) visited =
                            Left (BindingCycleDetected [ref])
                        | otherwise =
                            case IntMap.lookup (nodeRefKey ref) bindParents0 of
                                Nothing -> Left (MissingBindParent ref)
                                Just (parentRef, _flag) ->
                                    case parentRef of
                                        GenRef gid -> Right gid
                                        TypeRef _ ->
                                            go (IntSet.insert (nodeRefKey ref) visited) parentRef

            scopeNodes <-
                foldM
                    (\m nidInt -> do
                        gid <- nearestGen (typeRef (NodeId nidInt))
                        pure $
                            IntMap.insertWith
                                IntSet.union
                                (getGenNodeId gid)
                                (IntSet.singleton nidInt)
                                m
                    )
                    IntMap.empty
                    (IntSet.toList allTypeIds)

            let directChildren =
                    IntMap.fromListWith
                        IntSet.union
                        [ (getGenNodeId gid, IntSet.singleton (getNodeId child))
                        | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents0
                        , TypeRef child <- [nodeRefFromKey childKey]
                        , GenRef gid <- [parentRef]
                        ]

                rootsForScope gidInt scopeSet =
                    let referenced =
                            IntSet.fromList
                                [ getNodeId child
                                | nidInt <- IntSet.toList scopeSet
                                , let parentKey = nodeRefKey (TypeRef (NodeId nidInt))
                                , childKey <- IntSet.toList (IntMap.findWithDefault IntSet.empty parentKey typeEdges)
                                , TypeRef child <- [nodeRefFromKey childKey]
                                , IntSet.member (getNodeId child) scopeSet
                                ]
                        roots = IntSet.difference scopeSet referenced
                        direct = IntMap.findWithDefault IntSet.empty gidInt directChildren
                        roots' = IntSet.union roots direct
                    in map NodeId (IntSet.toList roots')

                rebuildOne gidInt =
                    let gid = GenNodeId gidInt
                        scopeSet = IntMap.findWithDefault IntSet.empty gidInt scopeNodes
                        schemes = rootsForScope gidInt scopeSet
                    in (gidInt, GenNode gid schemes)

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
            foldl'
                addTypeEdges
                (foldl' addGenEdges IntMap.empty (IntMap.elems genNodes1))
                (IntMap.elems nodes0)

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
    let bindingPathToRootUnder :: Int -> Either BindingError [NodeRef]
        bindingPathToRootUnder start = go IntSet.empty [nodeRefFromKey start] start
          where
            go visited path childKey
                | IntSet.member childKey visited =
                    Left $ BindingCycleDetected (reverse path)
                | otherwise =
                    case IntMap.lookup childKey bindParents of
                        Nothing -> Right (reverse path)
                        Just (parentRef, _flag) ->
                            go
                                (IntSet.insert childKey visited)
                                (parentRef : path)
                                (nodeRefKey parentRef)

    mapM_ bindingPathToRootUnder (IntMap.keys bindParents)

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
                unless (isUpperUnder parentRef childRef) $
                    Left (ParentNotUpper childRef parentRef)

    -- Root must be a single gen node.
    let roots =
            [ nodeRefFromKey key
            | key <- IntSet.toList liveKeys
            , not (IntMap.member key bindParents)
            ]
    rootRef <- case roots of
        [GenRef root] -> pure (GenRef root)
        [TypeRef _] ->
            Left $ InvalidBindingTree "Binding root is a type node (expected gen root)"
        [] ->
            Left $ InvalidBindingTree "Binding tree has no root"
        _ ->
            Left $ InvalidBindingTree ("Binding tree has multiple roots: " ++ show roots)

    -- Every non-root needs a parent.
    let rootKey = nodeRefKey rootRef
    forM_ (IntSet.toList liveKeys) $ \key ->
        unless (key == rootKey || IntMap.member key bindParents) $
            Left (MissingBindParent (nodeRefFromKey key))

    -- All gen nodes (except root) must be flexibly bound.
    forM_ (IntMap.keys genNodes0) $ \gidInt -> do
        let gref = GenRef (GenNodeId gidInt)
        if gref == rootRef
            then pure ()
            else case IntMap.lookup (nodeRefKey gref) bindParents of
                Nothing -> Left $ MissingBindParent gref
                Just (_parent, BindFlex) -> pure ()
                Just (_parent, BindRigid) ->
                    Left $
                        InvalidBindingTree $
                            "Gen node " ++ show gref ++ " is rigidly bound"

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
                , let boundKids =
                        case node of
                            TyVar{ tnBound = Just bnd } -> [bnd]
                            _ -> []
                , child <- structuralChildren node ++ boundKids
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
                , let boundKids =
                        case node of
                            TyVar{ tnBound = Just bnd } -> [bnd]
                            _ -> []
                , child <- structuralChildren node ++ boundKids
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
    | otherwise = go IntSet.empty parent
  where
    go visited ref
        | IntSet.member (nodeRefKey ref) visited = False  -- Avoid cycles
        | ref == child = True
        | otherwise =
            let visited' = IntSet.insert (nodeRefKey ref) visited
                children = structuralChildrenRef c ref
            in any (go visited') children
