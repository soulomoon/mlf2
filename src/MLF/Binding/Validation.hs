{- |
Module      : MLF.Binding.Validation
Description : Binding tree invariant checking
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides validation functions for binding tree invariants.
Extracted from MLF.Binding.Tree for modularity.
-}
module MLF.Binding.Validation (
    -- * Invariant checking
    checkBindingTree,
    checkBindingTreeUnder,
    checkNoGenFallback,
    checkSchemeClosure,
    checkSchemeClosureUnder,
    -- * Upper-than check
    isUpper,
    -- * Internal helpers (for Tree.hs)
    liveNodeKeys,
    validateSingleGenRoot,
    validateGenSchemeRoots,
    validateGenNodesFlexiblyBound,
) where

import Control.Monad (forM_, unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Constraint.Types

import MLF.Binding.Children (
    collectBoundChildrenWithFlag,
    )
-- Import canonicalization functions
import MLF.Binding.Canonicalization (
    canonicalizeBindParentsUnder,
    quotientBindParentsUnder,
    )
import MLF.Binding.NodeRefs (
    allNodeRefs,
    nodeRefExists,
    )
import MLF.Binding.Path (
    bindingPathToRoot,
    bindingPathToRootLocal,
    bindingPathToRootWithLookup,
    firstGenAncestorFromPath,
    )
import MLF.Binding.ScopeGraph (
    buildTypeEdgesFrom,
    buildScopeNodesFromPaths,
    rootsForScope,
    )

-- | Node keys that participate in binding-tree invariants.
liveNodeKeys :: Constraint -> IntSet
liveNodeKeys c =
    let typeKeys = IntSet.fromList
            [ nodeRefKey (TypeRef nid)
            | (nid, _node) <- toListNode (cNodes c)
            ]
        genKeys = IntSet.fromList
            [ nodeRefKey (GenRef (GenNodeId gid))
            | gid <- IntMap.keys (getGenNodeMap (cGenNodes c))
            ]
    in IntSet.union typeKeys genKeys

-- | Validate all binding-tree invariants.
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
    validateGenSchemeRoots
        (cGenNodes c)
        (\nid -> case lookupNodeIn (cNodes c) nid of
            Just _ -> True
            Nothing -> False
        )

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
  where
    mapMaybe f = foldr (\x acc -> maybe acc (:acc) (f x)) []

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

-- | Validate that all gen node scheme roots refer to live type nodes.
validateGenSchemeRoots
    :: GenNodeMap GenNode
    -> (NodeId -> Bool)
    -> Either BindingError ()
validateGenSchemeRoots genNodes nodeExists =
    forM_ (IntMap.elems (getGenNodeMap genNodes)) $ \genNode ->
        forM_ (gnSchemes genNode) $ \root ->
            unless (nodeExists root) $
                Left $
                    InvalidBindingTree $
                        "Gen node scheme root " ++ show root ++ " not in constraint"

-- | Validate that all gen nodes (except the root) are flexibly bound.
validateGenNodesFlexiblyBound
    :: GenNodeMap GenNode
    -> (NodeRef -> Maybe (NodeRef, BindFlag))
    -> NodeRef
    -> Either BindingError ()
validateGenNodesFlexiblyBound genNodes lookupParent rootRef =
    forM_ (IntMap.keys (getGenNodeMap genNodes)) $ \gidInt -> do
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

-- | Reject binding trees that would require a gen-ancestor fallback.
checkNoGenFallback :: Constraint -> Either BindingError ()
checkNoGenFallback c = do
    let nodes = cNodes c
        reachableFromWithBounds root0 =
            Traversal.reachableFromWithBounds
                id
                (lookupNodeIn nodes)
                root0

        firstGenAncestor nid = pure (firstGenAncestorFromPath (bindingPathToRoot c) (typeRef nid))

    forM_ (map snd (toListNode nodes)) $ \node ->
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
            case lookupNodeIn nodes nid of
                Just TyVar{} -> not (VarStore.isEliminatedVar c0 nid)
                _ -> False
    bindParents0 <- canonicalizeBindParentsUnder canonical c0
    let bindParents = softenBindParents canonical c0 bindParents0
    let typeEdges = buildTypeEdges nodes
        schemeRootsByGen =
            IntMap.map
                (IntSet.fromList . map (getNodeId . canonical) . gnSchemes)
                (getGenNodeMap (cGenNodes c0))
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
            firstGenAncestorFromPath (bindingPathToRootLocal bindParents) (typeRef (canonical nid))
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
        firstGenAncestorFromPath (bindingPathToRootLocal bindParents') (typeRef (canonical root0))

-- | Check if a node is "upper" than another in the term-DAG.
isUpper :: Constraint -> NodeRef -> NodeRef -> Bool
isUpper c parent child
    | parent == child = True
    | otherwise =
        case parent of
            GenRef _ -> True
            _ -> go IntSet.empty parent
  where
    go visited ref
        | IntSet.member (nodeRefKey ref) visited = False
        | ref == child = True
        | otherwise =
            let visited' = IntSet.insert (nodeRefKey ref) visited
                children = case ref of
                    TypeRef nid ->
                        maybe [] (map TypeRef . structuralChildrenWithBounds) (NodeAccess.lookupNode c nid)
                    GenRef gid ->
                        maybe [] (map TypeRef . gnSchemes)
                            (IntMap.lookup (getGenNodeId gid) (getGenNodeMap (cGenNodes c)))
            in any (go visited') children

-- Local helpers for validation checks.

lookupBindParent :: Constraint -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent c ref = IntMap.lookup (nodeRefKey ref) (cBindParents c)

isBindingRoot :: Constraint -> NodeRef -> Bool
isBindingRoot c ref = not $ IntMap.member (nodeRefKey ref) (cBindParents c)

bindingRoots :: Constraint -> [NodeRef]
bindingRoots c = filter (isBindingRoot c) (allNodeRefs c)

boundFlexChildren :: Constraint -> NodeRef -> Either BindingError [NodeId]
boundFlexChildren c binder = do
    unless (nodeRefExists c binder) $
        Left $
            InvalidBindingTree $
                "boundFlexChildren: binder " ++ show binder ++ " not in constraint"
    collectBoundChildren (tyVarChildFilter c) c (cBindParents c) binder "boundFlexChildren"

tyVarChildFilter :: Constraint -> NodeRef -> Maybe NodeId
tyVarChildFilter c ref = case ref of
    TypeRef nid ->
        case NodeAccess.lookupNode c nid of
            Just TyVar{} -> Just nid
            _ -> Nothing
    _ -> Nothing

collectBoundChildren
    :: (NodeRef -> Maybe NodeId)
    -> Constraint
    -> BindParents
    -> NodeRef
    -> String
    -> Either BindingError [NodeId]
collectBoundChildren childFilter c bindParents binder errCtx =
    collectBoundChildrenWithFlag childFilter (== BindFlex) c bindParents binder errCtx

-- | Validate binding-tree invariants on the quotient graph induced by a canonicalization function.
checkBindingTreeUnder :: (NodeId -> NodeId) -> Constraint -> Either BindingError ()
checkBindingTreeUnder canonical c0 = do
    let nodes0 = cNodes c0
        genNodes0 = cGenNodes c0

    validateGenSchemeRoots
        genNodes0
        (\nid -> case lookupNodeIn nodes0 (canonical nid) of
            Just _ -> True
            Nothing -> False
        )

    (_allRoots, bindParents0) <- quotientBindParentsUnder canonical c0

    let typeEdges :: IntMap.IntMap IntSet
        typeEdges =
            buildTypeEdgesFrom (nodeRefKey . TypeRef . canonical) nodes0

        genNodesDerived = do
            let genIds = IntMap.keys (getGenNodeMap genNodes0)
                allTypeIds =
                    [ getNodeId (canonical nid)
                    | (nid, _node) <- toListNode nodes0
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
                        [ nodeRefKey (TypeRef (canonical nid))
                        | (nid, _node) <- toListNode nodes0
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
            let parentKey = nodeRefKey (Canonicalize.canonicalRef canonical parent)
                childKey = nodeRefKey (Canonicalize.canonicalRef canonical child)
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
    let lookupParent' ref = IntMap.lookup (nodeRefKey ref) bindParents
    validateGenNodesFlexiblyBound genNodes0 lookupParent' rootRef

    pure ()
