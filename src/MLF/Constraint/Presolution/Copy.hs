{- |
Module      : MLF.Constraint.Presolution.Copy
Description : χe copying for instantiation

This module implements the χe-style copying performed during instantiation:
copying a ∀-body graph while substituting its bound variables with fresh nodes
at the target level, while preserving binding edges/flags and internal sharing.
-}
module MLF.Constraint.Presolution.Copy (
    instantiateScheme,
    instantiateSchemeWithTrace,
    bindExpansionRootLikeTarget,
    bindUnboundCopiedNodes
) where

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution.Base (
    PresolutionError(..),
    PresolutionM,
    PresolutionState(..)
    )
import MLF.Constraint.Presolution.Ops (
    createFreshNodeId,
    getCanonicalNode,
    registerNode,
    setBindParentM
    )
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Constraint.Types
import qualified MLF.Util.UnionFind as UnionFind

data CopyState = CopyState
    { csCache :: IntMap NodeId
    , csCopyMap :: IntMap NodeId
    , csInterior :: IntSet.IntSet
    }

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
    (root, _copyMap, _interior) <- instantiateSchemeWithTrace bodyId substList
    pure root

-- | Like 'instantiateScheme', but also return:
--   • a copy provenance map (original node → copied/replaced node), and
--   • the expansion interior I(r) as an IntSet (computed from binding edges).
--
-- Paper alignment (`papers/xmlf.txt` §3.2): when expanding an instantiation edge,
-- we copy exactly the nodes "structurally strictly under g and in I(g)" and
-- preserve binding edges/flags for copied nodes. The expansion root is bound
-- at the same binder as the target node.
instantiateSchemeWithTrace :: NodeId -> [(NodeId, NodeId)] -> PresolutionM (NodeId, IntMap NodeId, IntSet.IntSet)
instantiateSchemeWithTrace bodyId substList = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0

    let bodyC = canonical bodyId
    when (IntMap.notMember (getNodeId bodyC) (cNodes c0)) $
        throwError (NodeLookupFailed bodyC)

    -- Paper (`xmlf.txt` §3.2): expansion copies nodes structurally under g that
    -- are in I(g). In our representation, we take g to be the (quotient) binding
    -- parent of the root we are copying, and compute I(g) on the quotient
    -- binding graph.
    --
    -- If the root is itself a binding root in the quotient relation (e.g. when
    -- copying a disconnected instance bound), we use it as g.
    copyInterior <- do
        let boundVars = map fst substList
            pickBinderFromBoundVars = do
                parents <- forM boundVars $ \bv -> do
                    mbParent <- case Binding.lookupBindParentUnder canonical c0 (typeRef bv) of
                        Left err -> throwError (BindingTreeError err)
                        Right p -> pure p
                    case mbParent of
                        Just (parentRef, _flag) -> pure parentRef
                        Nothing -> pure (typeRef bv)
                case parents of
                    [] -> pure Nothing
                    (p:ps)
                        | all (== p) ps -> pure (Just p)
                        | otherwise ->
                            throwError $
                                BindingTreeError $
                                    InvalidBindingTree $
                                        "instantiateSchemeWithTrace: binders have different parents " ++ show parents

        let pickBinderFromRoot = do
                boundAtRoot <- case Binding.boundFlexChildrenUnder canonical c0 (typeRef bodyC) of
                    Left err -> throwError (BindingTreeError err)
                    Right bs -> pure bs
                if null boundAtRoot
                    then do
                        mbParent <- case Binding.lookupBindParentUnder canonical c0 (typeRef bodyC) of
                            Left err -> throwError (BindingTreeError err)
                            Right p -> pure p
                        pure $
                            case mbParent of
                                Nothing -> typeRef bodyC
                                Just (parentRef, _flag) -> parentRef
                    else pure (typeRef bodyC)

        mbBinder <- pickBinderFromBoundVars
        binderRef0 <- case mbBinder of
            Just binderRef -> pure binderRef
            Nothing -> pickBinderFromRoot
        let rootKey = nodeRefKey (typeRef bodyC)
        binderRef <- do
            ok <- case Binding.interiorOfUnder canonical c0 binderRef0 of
                Left err -> throwError (BindingTreeError err)
                Right s -> pure (IntSet.member rootKey s)
            if ok
                then pure binderRef0
                else pickBinderFromRoot
        case Binding.interiorOfUnder canonical c0 binderRef of
            Left err -> throwError (BindingTreeError err)
            Right s ->
                pure $
                    IntSet.fromList
                        [ getNodeId nid
                        | key <- IntSet.toList s
                        , TypeRef nid <- [nodeRefFromKey key]
                        ]

    let subst = IntMap.fromList [(getNodeId k, v) | (k, v) <- substList]
        initialCopyMap = IntMap.fromList [(getNodeId k, v) | (k, v) <- substList]
        initialInterior = IntSet.fromList (map (getNodeId . snd) substList)
        st0 =
            CopyState
                { csCache = IntMap.empty
                , csCopyMap = initialCopyMap
                , csInterior = initialInterior
                }
    (root, st1) <- runStateT (copyNode copyInterior canonical subst bodyId) st0
    let cmap = csCopyMap st1
        interior = csInterior st1
    -- Ensure the binding tree remains valid after copying: substitution nodes
    -- (binder-metas) may become non-roots once referenced by fresh copies, and
    -- copied nodes whose original parents were not copied need a parent.
    bindUnboundCopiedNodes cmap interior root
    pure (root, cmap, interior)
  where
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

    -- | Copy binding edge from source node to fresh node, translating parent through copyMap.
    --
    -- If the source node has a binding parent (under canonicalization), we copy it
    -- to the fresh node, translating the parent through the copy map when possible.
    -- If the parent was not copied, we leave the node unbound; higher-level
    -- re-binding attaches such nodes to the expansion root.
    --
    -- Paper alignment: preserve binding edges/flags for copied nodes.
    copyBindingEdge :: NodeId -> NodeId -> StateT CopyState PresolutionM ()
    copyBindingEdge srcNid freshId = do
        c <- lift $ gets psConstraint
        uf <- lift $ gets psUnionFind
        let canonical = UnionFind.frWith uf
        mbParent <- case Binding.lookupBindParentUnder canonical c (typeRef srcNid) of
            Left err -> lift $ throwError (BindingTreeError err)
            Right p -> pure p
        case mbParent of
            Nothing -> pure ()  -- Source is a root, fresh node remains a root for now
            Just (TypeRef parentId, flag) -> do
                copyMap <- gets csCopyMap
                let parentC = canonical parentId
                case IntMap.lookup (getNodeId parentC) copyMap of
                    Just copiedParent -> lift $ setBindParentM (typeRef freshId) (typeRef copiedParent, flag)
                    Nothing -> pure ()
            Just (GenRef _, _flag) ->
                pure ()

    copyNode :: IntSet.IntSet -> (NodeId -> NodeId) -> IntMap NodeId -> NodeId -> StateT CopyState PresolutionM NodeId
    copyNode copyInterior canonical subst nid = do
        mbCached <- cacheLookup nid
        case mbCached of
            Just existing -> pure existing
            Nothing -> do
                node <- lift $ getCanonicalNode nid
                case node of
                    TyExp { tnBody = b } -> do
                        b' <- copyNode copyInterior canonical subst b
                        pure b'
                    _ -> do
                        let nidC = canonical nid
                        let k = getNodeId nidC
                        -- Check level to decide whether to copy or share
                        let shouldShare =
                                case node of
                                    TyBase {} -> True
                                    TyBottom {} -> True
                                    _ -> not (IntSet.member k copyInterior)

                        if shouldShare
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
                                                    TyRoot { tnChildren = cs } ->
                                                        TyRoot freshId cs
                                        lift $ registerNode freshId placeholder

                                        -- Recursively copy children
                                        newNode <- case node of
                                            TyArrow { tnDom = d, tnCod = c } -> do
                                                d' <- copyNode copyInterior canonical subst d
                                                c' <- copyNode copyInterior canonical subst c
                                                return $ TyArrow freshId d' c'
                                            TyForall { tnBody = b } -> do
                                                b' <- copyNode copyInterior canonical subst b
                                                return $ TyForall freshId b'
                                            TyVar { tnBound = mb } -> do
                                                mb' <- traverse (copyNode copyInterior canonical subst) mb
                                                pure $ TyVar { tnId = freshId, tnBound = mb' }
                                            TyBottom {} ->
                                                pure $ TyBottom freshId
                                            TyBase { tnBase = b } -> do
                                                return $ TyBase freshId b
                                            TyRoot { tnChildren = cs } -> do
                                                cs' <- traverse (copyNode copyInterior canonical subst) cs
                                                pure $ TyRoot freshId cs'

                                        -- Register new node in constraint (overwrite placeholder)
                                        lift $ registerNode freshId newNode

                                        -- Copy binding edge from source to fresh node
                                        -- Paper alignment: preserve binding edges/flags for copied nodes
                                        copyBindingEdge nid freshId

                                        return freshId

-- | Bind the expansion root at the same binder as the edge target.
--
-- Paper alignment (`papers/xmlf.txt` §3.2): "the root of the expansion is bound
-- at the same binder as the target". This ensures the expansion root is in the
-- correct interior I(r) for subsequent operations.
--
-- If the target has a binding parent, we copy that binding to the expansion root.
-- If the target is a binding root (no parent), the expansion root also becomes
-- a binding root (we don't set a binding parent for it).
bindExpansionRootLikeTarget :: NodeId -> NodeId -> PresolutionM ()
bindExpansionRootLikeTarget expansionRoot targetNode = do
    c <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
        expansionRootC = canonical expansionRoot
        addSchemeRoot :: GenNodeId -> PresolutionM ()
        addSchemeRoot gid =
            modify' $ \st ->
                let c0 = psConstraint st
                    gens0 = cGenNodes c0
                    update genNode =
                        if expansionRootC `elem` gnSchemes genNode
                            then genNode
                            else genNode { gnSchemes = gnSchemes genNode ++ [expansionRootC] }
                    gens' = IntMap.adjust update (getGenNodeId gid) gens0
                in st { psConstraint = c0 { cGenNodes = gens' } }
    mbParentInfo <- case Binding.lookupBindParentUnder canonical c (typeRef targetNode) of
        Left err -> throwError (BindingTreeError err)
        Right p -> pure p
    case mbParentInfo of
        Just parentInfo@(parentRef, _flag) -> do
            setBindParentM (typeRef expansionRootC) parentInfo
            case parentRef of
                GenRef gid -> addSchemeRoot gid
                TypeRef _ -> pure ()
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
                Just gref@(GenRef gid) -> do
                    setBindParentM (typeRef expansionRootC) (gref, BindFlex)
                    addSchemeRoot gid
                Just (TypeRef _) -> pure ()
                Nothing -> pure ()

-- | Bind all copied nodes that don't have binding parents to the expansion root.
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
    let copiedIds = IntSet.fromList (map getNodeId (IntMap.elems copyMap))
        candidateIds0 = IntSet.union copiedIds interior

        lookupNode = lookupNodeIn (cNodes c0)

        candidateIds =
            Traversal.reachableFromManyUnderLenient
                canonical
                lookupNode
                (map NodeId (IntSet.toList candidateIds0))

    -- Bind any copied/interior nodes that do not already have a binding parent
    -- to the expansion root (except the expansion root itself). This matches
    -- the thesis binding-tree invariant: the only root is the gen node, so
    -- freshly-copied term-DAG roots must be attached under the expansion root.
    forM_ (IntSet.toList candidateIds) $ \nid -> do
        let node0 = NodeId nid
            nodeC = canonical node0
        c' <- gets psConstraint
        when (nodeC /= expansionRootC) $
            case Binding.lookupBindParent c' (typeRef nodeC) of
                Just _ -> pure ()
                Nothing -> setBindParentM (typeRef nodeC) (typeRef expansionRootC, BindFlex)
