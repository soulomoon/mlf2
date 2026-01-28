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
    bindExpansionRootLikeTarget,
    bindUnboundCopiedNodes
) where

import Control.Monad (foldM, forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution.Base (
    PresolutionError(..),
    PresolutionM,
    PresolutionState(..),
    bindingPathToRootUnderM,
    instantiationBindersM
    )
import MLF.Constraint.Presolution.Ops (
    createFreshNodeId,
    getCanonicalNode,
    registerNode,
    setBindParentM
    )
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Constraint.Types
import qualified MLF.Util.UnionFind as UnionFind

data CopyState = CopyState
    { csCache :: IntMap NodeId
    , csCopyMap :: IntMap NodeId
    , csInterior :: IntSet.IntSet
    }

canonicalRef :: (NodeId -> NodeId) -> NodeRef -> NodeRef
canonicalRef = Canonicalize.canonicalRef

findSchemeIntroducerM :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM GenNodeId
findSchemeIntroducerM canonical c0 root0 = do
    let root = canonical root0
    -- The scheme introducer is the nearest gen node on the binding path.
    path <- bindingPathToRootUnderM canonical c0 (typeRef root)
    case [gid | GenRef gid <- path] of
        (gid:_) -> pure gid
        [] ->
            throwError
                (InternalError ("scheme introducer not found for " ++ show root))

expansionCopySetsM :: NodeId -> PresolutionM (GenNodeId, IntSet.IntSet, IntSet.IntSet)
expansionCopySetsM bodyId = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
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
    gid <- findSchemeIntroducerM canonical c0 bodyC
    let binderRef =
            case Binding.lookupBindParentUnder canonical c0 (typeRef bodyC) of
                Left _ -> typeRef bodyC
                Right mbParentInfo ->
                    case mbParentInfo of
                        Just (TypeRef pid, _flag) ->
                            case NodeAccess.lookupNode c0 (canonical pid) of
                                Just TyForall{} -> typeRef (canonical pid)
                                _ -> typeRef bodyC
                        _ -> typeRef bodyC
    -- Copy nodes in I(g) that are structurally reachable from the scheme body.
    -- For wrapper scheme roots (TyVar bound to structure), use the scheme gen
    -- interior so binders remain instantiable across higher-order uses.
    let useGenInterior =
            case NodeAccess.lookupNode c0 bodyC of
                Just TyVar{ tnBound = Just _ } -> True
                _ -> False
        interiorRoot =
            if useGenInterior
                then genRef gid
                else binderRef
    interiorAll0 <- case Binding.interiorOfUnder canonical c0 interiorRoot of
        Left err -> throwError (BindingTreeError err)
        Right s -> pure s
    bindersUnderGen <- case Binding.boundFlexChildrenUnder canonical c0 (genRef gid) of
        Left err -> throwError (BindingTreeError err)
        Right bs -> pure bs
    let binderKeysGen =
            IntSet.fromList
                [ nodeRefKey (typeRef (canonical b))
                | b <- bindersUnderGen
                ]
        interiorAll0' = IntSet.union interiorAll0 binderKeysGen
    let reachFromS =
            Traversal.reachableFromNodes canonical children [bodyC]
        reachFromSKeys =
            IntSet.fromList [typeRefKey (NodeId nid) | nid <- IntSet.toList reachFromS]
    pathRefs <- bindingPathToRootUnderM canonical c0 (typeRef bodyC)
    let pathGenIds =
            IntSet.fromList
                [ getGenNodeId gidPath
                | GenRef gidPath <- pathRefs
                ]
        reachableSchemeGens =
            [ gnId gen
            | gen <- IntMap.elems (cGenNodes c0)
            , not (IntSet.member (getGenNodeId (gnId gen)) pathGenIds)
            , any
                (\root -> IntSet.member (typeRefKey (canonical root)) reachFromSKeys)
                (gnSchemes gen)
            ]
    interiorAllExtra <- foldM
        (\acc gidExtra -> do
            extra <- case Binding.interiorOfUnder canonical c0 (genRef gidExtra) of
                Left err -> throwError (BindingTreeError err)
                Right s -> pure s
            pure (IntSet.union acc extra)
        )
        IntSet.empty
        reachableSchemeGens
    let interiorAll = IntSet.union interiorAll0' interiorAllExtra
    (_root, binders) <- instantiationBindersM bodyC
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
                            let childC = canonicalRef canonical child
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
    pure (gid, interiorTypeSet, frontierTypeSet)

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
    • Preserve structure: arrows / foralls / expansions are recursively copied;
        bases may be shared as an optimization.
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
instantiateScheme :: NodeId -> [(NodeId, NodeId)] -> PresolutionM NodeId
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
instantiateSchemeWithTrace :: NodeId -> [(NodeId, NodeId)] -> PresolutionM (NodeId, IntMap NodeId, IntSet.IntSet, IntSet.IntSet)
instantiateSchemeWithTrace bodyId substList =
    instantiateSchemeWithMode True bodyId substList

instantiateSchemeWithMode
    :: Bool
    -> NodeId
    -> [(NodeId, NodeId)]
    -> PresolutionM (NodeId, IntMap NodeId, IntSet.IntSet, IntSet.IntSet)
instantiateSchemeWithMode replaceFrontier bodyId substList = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0

    let bodyC = canonical bodyId
    when (IntMap.notMember (getNodeId bodyC) (cNodes c0)) $
        throwError (NodeLookupFailed bodyC)

    -- Paper (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.2):
    -- expansion copies nodes in I^s(g) and F^s(g) that are reachable from s,
    -- then replaces frontier copies with ⊥ and adds frontier unification edges.
    (gid, copyInterior0, frontierSet0) <- expansionCopySetsM bodyId
    let bodyKey = getNodeId bodyC
        isDegenerate = not (IntSet.member bodyKey copyInterior0)
        copyInterior =
            if isDegenerate
                then IntSet.insert bodyKey copyInterior0
                else copyInterior0
        frontierSet =
            if isDegenerate
                then IntSet.insert bodyKey frontierSet0
                else frontierSet0
        degenerateRoot =
            if isDegenerate
                then Just bodyC
                else Nothing
        frontierForCopy =
            if replaceFrontier
                then frontierSet
                else IntSet.empty

    let nodes = cNodes c0
        schemeRoots =
            IntSet.fromList
                [ getNodeId (canonical r)
                | gen <- IntMap.elems (cGenNodes c0)
                , r <- gnSchemes gen
                ]
        isSchemeRootWrapper nid =
            let nidC = canonical nid
                key = getNodeId nidC
            in IntSet.member key schemeRoots &&
                case IntMap.lookup key nodes of
                    Just TyVar{ tnBound = Just _ } -> True
                    _ -> False
        substAll = IntMap.fromList [(getNodeId k, v) | (k, v) <- substList]
        subst =
            IntMap.filterWithKey
                (\k _ -> not (isSchemeRootWrapper (NodeId k)))
                substAll
        initialCopyMap = IntMap.fromList [(getNodeId k, v) | (k, v) <- substList]
        initialInterior = IntSet.fromList (map (getNodeId . snd) substList)
        st0 =
            CopyState
                { csCache = IntMap.empty
                , csCopyMap = initialCopyMap
                , csInterior = initialInterior
                }
    (root, st1) <- runStateT (copyNode copyInterior frontierForCopy degenerateRoot canonical subst bodyId) st0
    let cmap = csCopyMap st1
        interior = csInterior st1
    let substKeys = IntSet.fromList (map (getNodeId . fst) substList)
    resetBindingsForCopies canonical c0 gid bodyC root frontierForCopy cmap substKeys
    pure (root, cmap, interior, frontierSet)
  where
    resetBindingsForCopies
        :: (NodeId -> NodeId)
        -> Constraint
        -> GenNodeId
        -> NodeId
        -> NodeId
        -> IntSet.IntSet
        -> IntMap NodeId
        -> IntSet.IntSet
        -> PresolutionM ()
    resetBindingsForCopies canonical c0 gid schemeRootId copyRoot frontierSet cmap0 substKeys = do
        let cmap =
                IntMap.fromListWith
                    (\a _ -> a)
                    [ (getNodeId (canonical (NodeId orig)), copy)
                    | (orig, copy) <- IntMap.toList cmap0
                    ]
            schemeRootC = canonical schemeRootId
            copyRootC = canonical copyRoot
            rootBinder = genRef gid
        -- Flag reset: the copy root and frontier copies are flexibly bound.
        setBindParentM (typeRef copyRootC) (rootBinder, BindFlex)
        forM_ (IntSet.toList frontierSet) $ \nidInt -> do
            let nid = NodeId nidInt
                nidC = canonical nid
            case IntMap.lookup (getNodeId nidC) cmap of
                Nothing -> pure ()
                Just copy -> do
                    c1 <- gets psConstraint
                    let childRef = typeRef copy
                        parentCandidate = typeRef copyRootC
                        parentFinal =
                            if Binding.isUpper c1 parentCandidate childRef
                                then parentCandidate
                                else rootBinder
                    setBindParentM childRef (parentFinal, BindFlex)
        forM_ (IntMap.toList cmap) $ \(origKey, copy) -> do
            let orig = NodeId origKey
                origC = canonical orig
                isRoot = origC == schemeRootC
                isFrontier = IntSet.member (getNodeId origC) frontierSet
            when (not isRoot && not isFrontier) $ do
                if IntSet.member origKey substKeys
                    then do
                        let childRef = typeRef copy
                        setBindParentM childRef (rootBinder, BindFlex)
                    else
                        case Binding.lookupBindParent c0 (typeRef copy) of
                            Just _ -> pure ()
                            Nothing -> do
                                mbParent <- case Binding.lookupBindParentUnder canonical c0 (typeRef origC) of
                                    Left err -> throwError (BindingTreeError err)
                                    Right p -> pure p
                                case mbParent of
                                    Nothing ->
                                        throwError (BindingTreeError (MissingBindParent (typeRef origC)))
                                    Just (parentRef, flag) -> do
                                        let parentRefC = canonicalRef canonical parentRef
                                        parentFinal0 <- case parentRefC of
                                            GenRef _ ->
                                                pure rootBinder
                                            TypeRef pid
                                                | canonical pid == schemeRootC ->
                                                    pure (typeRef copyRootC)
                                            TypeRef pid ->
                                                case IntMap.lookup (getNodeId (canonical pid)) cmap of
                                                    Just parentCopy -> pure (typeRef parentCopy)
                                                    Nothing -> pure (typeRef copyRootC)
                                        c1 <- gets psConstraint
                                        let childRef = typeRef copy
                                            parentFinal1 =
                                                if parentFinal0 == childRef
                                                    then typeRef copyRootC
                                                    else parentFinal0
                                            parentFinal =
                                                if Binding.isUpper c1 parentFinal1 childRef
                                                    then parentFinal1
                                                    else if Binding.isUpper c1 (typeRef copyRootC) childRef
                                                        then typeRef copyRootC
                                                        else rootBinder
                                        setBindParentM childRef (parentFinal, flag)

    recordNew :: NodeId -> StateT CopyState PresolutionM ()
    recordNew freshId =
        modify $ \st ->
            st { csInterior = IntSet.insert (getNodeId freshId) (csInterior st) }

    recordCopy :: NodeId -> NodeId -> StateT CopyState PresolutionM ()
    recordCopy srcNid copiedId =
        modify $ \st ->
            st { csCopyMap = IntMap.insert (getNodeId srcNid) copiedId (csCopyMap st) }

    cacheLookup :: NodeId -> StateT CopyState PresolutionM (Maybe NodeId)
    cacheLookup srcNid = gets (IntMap.lookup (getNodeId srcNid) . csCache)

    cacheInsert :: NodeId -> NodeId -> StateT CopyState PresolutionM ()
    cacheInsert srcNid freshId =
        modify $ \st ->
            st { csCache = IntMap.insert (getNodeId srcNid) freshId (csCache st) }

    copyNode
        :: IntSet.IntSet
        -> IntSet.IntSet
        -> Maybe NodeId
        -> (NodeId -> NodeId)
        -> IntMap NodeId
        -> NodeId
        -> StateT CopyState PresolutionM NodeId
    copyNode copyInterior frontierSet degenerateRoot canonical subst nid = do
        mbCached <- cacheLookup nid
        case mbCached of
            Just existing -> pure existing
            Nothing -> do
                node <- lift $ getCanonicalNode nid
                case node of
                    TyExp { tnBody = b } -> do
                        b' <- copyNode copyInterior frontierSet degenerateRoot canonical subst b
                        pure b'
                    _ -> do
                        let nidC = canonical nid
                        let k = getNodeId nidC
                        let isDegenerateRoot =
                                case degenerateRoot of
                                    Just rootId -> canonical rootId == nidC
                                    Nothing -> False
                        let isBase = case node of
                                TyBase{} -> True
                                _ -> False
                        if isBase
                            then pure nidC
                            else if IntSet.member k frontierSet && not isDegenerateRoot
                                then do
                                    freshId <- lift createFreshNodeId
                                    cacheInsert nid freshId
                                    recordCopy nid freshId
                                    lift $ registerNode freshId (TyBottom freshId)
                                    pure freshId
                                else if not (IntSet.member k copyInterior)
                                    then do
                                        pure nidC
                                    else do
                                        -- Substitute bound variables if present
                                        case IntMap.lookup (getNodeId nidC) subst of
                                            Just replacement -> do
                                                recordCopy nid replacement
                                                pure replacement
                                            Nothing -> do
                                                -- Create fresh node shell
                                                freshId <- lift createFreshNodeId
                                                cacheInsert nid freshId
                                                recordCopy nid freshId
                                                recordNew freshId
                                                -- Register a placeholder so binding ops see the child/parent ids.
                                                let placeholder =
                                                        case node of
                                                            TyArrow { tnDom = d, tnCod = c } ->
                                                                TyArrow freshId d c
                                                            TyForall { tnBody = b } ->
                                                                TyForall freshId b
                                                            TyVar { tnBound = mb } ->
                                                                TyVar { tnId = freshId, tnBound = mb }
                                                            TyBottom {} ->
                                                                TyBottom freshId
                                                            TyBase { tnBase = b } ->
                                                                TyBase freshId b
                                                lift $ registerNode freshId placeholder

                                                -- Recursively copy children
                                                newNode <- case node of
                                                    TyArrow { tnDom = d, tnCod = c } -> do
                                                        d' <- copyNode copyInterior frontierSet degenerateRoot canonical subst d
                                                        c' <- copyNode copyInterior frontierSet degenerateRoot canonical subst c
                                                        return $ TyArrow freshId d' c'
                                                    TyForall { tnBody = b } -> do
                                                        b' <- copyNode copyInterior frontierSet degenerateRoot canonical subst b
                                                        return $ TyForall freshId b'
                                                    TyVar { tnBound = mb } -> do
                                                        mb' <- traverse (copyNode copyInterior frontierSet degenerateRoot canonical subst) mb
                                                        pure $ TyVar { tnId = freshId, tnBound = mb' }
                                                    TyBottom {} ->
                                                        pure $ TyBottom freshId
                                                    TyBase { tnBase = b } -> do
                                                        return $ TyBase freshId b

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
-- If the target has a binding parent, we copy that binding to the expansion root.
-- If the target is a binding root (no parent), the expansion root also becomes
-- a binding root (we don't set a binding parent for it).
bindExpansionRootLikeTarget :: NodeId -> NodeId -> PresolutionM NodeRef
bindExpansionRootLikeTarget expansionRoot targetNode = do
    c <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
        expansionRootC = canonical expansionRoot
    mbParentInfo <- case Binding.lookupBindParentUnder canonical c (typeRef targetNode) of
        Left err -> throwError (BindingTreeError err)
        Right p -> pure p
    case mbParentInfo of
        Just (parentRef, _flag) -> do
            -- Flag reset: the expansion root is always flexibly bound.
            setBindParentM (typeRef expansionRootC) (parentRef, BindFlex)
            pure parentRef
        Nothing -> do
            -- Target is a root: bind the expansion root under the binding-tree root gen node.
            let genIds = IntMap.keys (cGenNodes c)
            rootGen <- foldM
                (\acc gidInt -> do
                    case acc of
                        Just _ -> pure acc
                        Nothing -> do
                            let gref = genRef (GenNodeId gidInt)
                            mbParent <- case Binding.lookupBindParentUnder canonical c gref of
                                Left err -> throwError (BindingTreeError err)
                                Right p -> pure p
                            pure $ case mbParent of
                                Nothing -> Just gref
                                Just _ -> Nothing
                )
                    Nothing
                    genIds
            case rootGen of
                Just gref@(GenRef _) -> do
                    setBindParentM (typeRef expansionRootC) (gref, BindFlex)
                    pure gref
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
bindUnboundCopiedNodes :: IntMap NodeId -> IntSet.IntSet -> NodeId -> PresolutionM ()
bindUnboundCopiedNodes copyMap interior expansionRoot = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
        expansionRootC = canonical expansionRoot
    expansionPath <- bindingPathToRootUnderM canonical c0 (typeRef expansionRootC)
    let copiedIds = IntSet.fromList (map getNodeId (IntMap.elems copyMap))
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
        c' <- gets psConstraint
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
