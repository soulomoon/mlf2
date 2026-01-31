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
    -- * Invariant checking (re-exported from Validation)
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

import Control.Monad (foldM, forM, unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import qualified MLF.Util.OrderKey as OrderKey
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Types
import qualified MLF.Constraint.Traversal as Traversal

-- Re-export validation functions
import MLF.Binding.Validation (
    checkBindingTree,
    checkBindingTreeUnder,
    checkNoGenFallback,
    checkSchemeClosure,
    checkSchemeClosureUnder,
    isUpper,
    )

-- Re-export canonicalization functions
import MLF.Binding.Canonicalization (
    canonicalizeBindParentsUnder,
    withQuotientBindParents,
    )

allNodeRefs :: Constraint -> [NodeRef]
allNodeRefs c =
    map (TypeRef . NodeId) (IntMap.keys (cNodes c)) ++
    map (GenRef . GenNodeId) (IntMap.keys (cGenNodes c))

nodeRefExists :: Constraint -> NodeRef -> Bool
nodeRefExists c ref = case ref of
    TypeRef nid -> IntMap.member (getNodeId nid) (cNodes c)
    GenRef gid -> IntMap.member (getGenNodeId gid) (cGenNodes c)

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
    let binderC = Canonicalize.canonicalRef canonical binder0
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
                _ -> case OrderKey.sortByOrderKey orderKeys ready of
                    Left err ->
                        Left $ InvalidBindingTree ("orderedBinders: order key error: " ++ show err)
                    Right readySorted ->
                        let readySet = IntSet.fromList (map getNodeId readySorted)
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
