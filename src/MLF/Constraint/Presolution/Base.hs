{-# LANGUAGE FlexibleInstances #-}

module MLF.Constraint.Presolution.Base (
    PresolutionResult(..),
    PresolutionPlanBuilder(..),
    PresolutionError(..),
    TranslatabilityIssue(..),
    PresolutionState(..),
    EdgeTrace(..),
    CopyMapping(..),
    CopyMap,
    lookupCopy,
    insertCopy,
    copiedNodes,
    originalNodes,
    InteriorNodes(..),
    FrontierNodes(..),
    InteriorSet,
    FrontierSet,
    memberInterior,
    memberFrontier,
    toListInterior,
    toListFrontier,
    fromListInterior,
    fromListFrontier,
    emptyTrace,
    unionTrace,
    PresolutionM,
    runPresolutionM,
    MonadPresolution(..),
    bindingPathToRootUnderM,
    requireValidBindingTree,
    edgeInteriorExact,
    instantiationBindersM,
    forallSpecM,
    dropTrivialSchemeEdges
) where

import Control.Applicative ((<|>))
import Control.Monad.State (StateT, get, gets, modify', put, runStateT)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.Except (throwError)
import Control.Monad (forM_, unless, when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Types hiding (lookupNode)
import qualified MLF.Constraint.Types as Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Util.Order as Order
import qualified MLF.Util.UnionFind as UnionFind
import MLF.Util.Trace (TraceConfig, traceBindingM)
import MLF.Constraint.Presolution.Plan (GeneralizePlan, ReifyPlan)
import MLF.Constraint.Presolution.Plan.Context (GaBindParents)
import MLF.Constraint.Solve (SolveResult)
import MLF.Util.ElabError (ElabError)

-- | Result of the presolution phase.
data PresolutionResult = PresolutionResult
    { prConstraint :: Constraint
    , prEdgeExpansions :: IntMap Expansion
    , prEdgeWitnesses :: IntMap EdgeWitness
    , prEdgeTraces :: IntMap EdgeTrace
    , prRedirects :: IntMap NodeId -- ^ Map from old TyExp IDs to their replacement IDs
    , prPlanBuilder :: PresolutionPlanBuilder
    } deriving (Eq, Show)

newtype PresolutionPlanBuilder = PresolutionPlanBuilder
    { ppbBuildGeneralizePlans
        :: SolveResult
        -> Maybe GaBindParents
        -> NodeRef
        -> NodeId
        -> Either ElabError (GeneralizePlan, ReifyPlan)
    }

instance Eq PresolutionPlanBuilder where
    _ == _ = True

instance Show PresolutionPlanBuilder where
    show _ = "<PresolutionPlanBuilder>"

-- | Errors that can occur during presolution.
data PresolutionError
    = UnmatchableTypes NodeId NodeId String  -- ^ Type mismatch during expansion
    | UnresolvedExpVar ExpVarId              -- ^ ExpVar couldn't be resolved
    | ArityMismatch String Int Int           -- ^ (context, expected, actual)
    | InstantiateOnNonForall NodeId          -- ^ Tried to instantiate a non-forall node
    | NodeLookupFailed NodeId                -- ^ Missing node in constraint
    | OccursCheckPresolution NodeId NodeId   -- ^ Unification would make node reachable from itself
    | BindingTreeError BindingError          -- ^ Invalid binding tree when binding edges are in use
    | NonTranslatablePresolution [TranslatabilityIssue]
    | InternalError String                   -- ^ Unexpected internal state
    deriving (Eq, Show)

data TranslatabilityIssue
    = InertLockedNodes [NodeId]
    | SchemeRootNotRigid GenNodeId NodeId
    | ArrowNodeNotRigid NodeId
    | TyConNodeNotRigid NodeId
    | NonInteriorNodeNotRigid GenNodeId NodeId
    deriving (Eq, Show)

-- | State maintained during the presolution process.
data PresolutionState = PresolutionState
    { psConstraint :: Constraint
    , psPresolution :: Presolution
    , psUnionFind :: IntMap NodeId
    , psNextNodeId :: Int
    , psPendingWeakens :: IntSet.IntSet
    , psBinderCache :: IntMap [NodeId]
    , psEdgeExpansions :: IntMap Expansion
    , psEdgeWitnesses :: IntMap EdgeWitness
    , psEdgeTraces :: IntMap EdgeTrace
    }
    deriving (Eq, Show)

-- | Per-edge provenance for instantiation-related operations.
--
-- This is an internal aid for gradually aligning presolution witnesses with
-- `papers/these-finale-english.txt`’s normalized instance-operation language
-- (see `papers/xmlf.txt` Fig. 10). For now, we only track the binder↦argument
-- pairing chosen by `ExpInstantiate`.
data EdgeTrace = EdgeTrace
    { etRoot :: NodeId
    , etBinderArgs :: [(NodeId, NodeId)] -- ^ (binder node, instantiation argument node)
    , etInterior :: InteriorNodes -- ^ Nodes in I(r) (exact, from the binding tree).
    , etCopyMap :: CopyMapping -- ^ Provenance: original node -> copied/replaced node
    }
    deriving (Eq, Show)

newtype CopyMapping = CopyMapping { getCopyMapping :: IntMap NodeId }
    deriving (Eq, Show)

instance Semigroup CopyMapping where
    CopyMapping a <> CopyMapping b = CopyMapping (IntMap.union a b)

instance Monoid CopyMapping where
    mempty = CopyMapping IntMap.empty

lookupCopy :: NodeId -> CopyMapping -> Maybe NodeId
lookupCopy nid (CopyMapping m) = IntMap.lookup (getNodeId nid) m

insertCopy :: NodeId -> NodeId -> CopyMapping -> CopyMapping
insertCopy src dst (CopyMapping m) = CopyMapping (IntMap.insert (getNodeId src) dst m)

originalNodes :: CopyMapping -> [NodeId]
originalNodes (CopyMapping m) = map NodeId (IntMap.keys m)

copiedNodes :: CopyMapping -> [NodeId]
copiedNodes (CopyMapping m) = IntMap.elems m

type CopyMap = CopyMapping
type InteriorSet = IntSet.IntSet
type FrontierSet = IntSet.IntSet

newtype InteriorNodes = InteriorNodes IntSet.IntSet
    deriving (Eq, Show)

instance Semigroup InteriorNodes where
    InteriorNodes a <> InteriorNodes b = InteriorNodes (IntSet.union a b)

instance Monoid InteriorNodes where
    mempty = InteriorNodes IntSet.empty

newtype FrontierNodes = FrontierNodes IntSet.IntSet
    deriving (Eq, Show)

instance Semigroup FrontierNodes where
    FrontierNodes a <> FrontierNodes b = FrontierNodes (IntSet.union a b)

instance Monoid FrontierNodes where
    mempty = FrontierNodes IntSet.empty

memberInterior :: NodeId -> InteriorNodes -> Bool
memberInterior nid (InteriorNodes s) = IntSet.member (getNodeId nid) s

memberFrontier :: NodeId -> FrontierNodes -> Bool
memberFrontier nid (FrontierNodes s) = IntSet.member (getNodeId nid) s

toListInterior :: InteriorNodes -> [NodeId]
toListInterior (InteriorNodes s) = map NodeId (IntSet.toList s)

toListFrontier :: FrontierNodes -> [NodeId]
toListFrontier (FrontierNodes s) = map NodeId (IntSet.toList s)

fromListInterior :: [NodeId] -> InteriorNodes
fromListInterior = InteriorNodes . IntSet.fromList . map getNodeId

fromListFrontier :: [NodeId] -> FrontierNodes
fromListFrontier = FrontierNodes . IntSet.fromList . map getNodeId

emptyTrace :: (CopyMap, InteriorSet, FrontierSet)
emptyTrace = (mempty, IntSet.empty, IntSet.empty)

unionTrace :: (CopyMap, InteriorSet, FrontierSet) -> (CopyMap, InteriorSet, FrontierSet) -> (CopyMap, InteriorSet, FrontierSet)
unionTrace (m1, s1, f1) (m2, s2, f2) =
    (m1 <> m2, IntSet.union s1 s2, IntSet.union f1 f2)

-- | The Presolution monad.
type PresolutionM = ReaderT TraceConfig (StateT PresolutionState (Either PresolutionError))

-- | Run a PresolutionM action with an initial state (testing helper).
runPresolutionM :: TraceConfig -> PresolutionState -> PresolutionM a -> Either PresolutionError (a, PresolutionState)
runPresolutionM cfg st action = runStateT (runReaderT action cfg) st

{- Note [Presolution foundation]
Presolution state access is intentionally layered to keep the core algorithms
paper-faithful while avoiding ad-hoc state plumbing:

  * Preferred abstraction: use the `MonadPresolution` class for functions that
    should work across presolution sub-monads (e.g., `PresolutionM` and
    `EdgeUnifyM`).

  * Preferred helper modules:
      - `MLF.Constraint.Presolution.Ops` for low-level stateful primitives
        (fresh IDs, node registration, union-find roots, variable bounds).
      - `MLF.Constraint.Presolution.StateAccess` for canonical/constraint access,
        binding-tree queries, and `WithCanonicalT` when threading the canonical
        environment through nested helpers.

  * Avoid adding new direct uses of `gets psConstraint` / `gets psUnionFind`,
    ad-hoc `UnionFind.frWith`, or manual `Binding.*` error lifting in submodules.
    Instead, extend the helper modules above when a common access pattern is
    missing.

Deprecation plan: legacy helper functions and direct state access remain for
now, but callers should be migrated to the foundation modules as we converge
presolution layers (see US-019). Once migrated, redundant helpers will be
removed.
-}

-- | Typeclass for monads that support presolution operations.
-- This allows functions to be polymorphic over the concrete monad stack,
-- reducing the need for explicit lift calls.
class Monad m => MonadPresolution m where
    -- | Get the current constraint.
    getConstraint :: m Constraint
    -- | Modify the constraint with a function.
    modifyConstraint :: (Constraint -> Constraint) -> m ()
    -- | Get the full presolution state.
    getPresolutionState :: m PresolutionState
    -- | Put a new presolution state.
    putPresolutionState :: PresolutionState -> m ()
    -- | Throw a presolution error.
    throwPresolutionError :: PresolutionError -> m a
    -- | Modify the presolution state with a function.
    modifyPresolution :: (Presolution -> Presolution) -> m ()
    -- | Bind expansion arguments to the appropriate binder.
    -- Used during instantiation to bind copied argument nodes.
    bindExpansionArgs :: NodeId -> [(NodeId, NodeId)] -> m ()

-- | Instance for the concrete PresolutionM monad.
instance {-# OVERLAPPING #-} MonadPresolution PresolutionM where
    getConstraint = gets psConstraint
    modifyConstraint f = modify' $ \st -> st { psConstraint = f (psConstraint st) }
    getPresolutionState = get
    putPresolutionState = put
    throwPresolutionError = throwError
    modifyPresolution f = modify' $ \st -> st { psPresolution = f (psPresolution st) }
    bindExpansionArgs expansionRoot pairs = do
        c0 <- gets psConstraint
        uf <- gets psUnionFind
        let canonical = UnionFind.frWith uf
        let expansionRootC = canonical expansionRoot
            rootGen =
                let genIds = IntMap.keys (getGenNodeMap (cGenNodes c0))
                    pickRoot acc gidInt =
                        case acc of
                            Just _ -> acc
                            Nothing ->
                                let gref = genRef (GenNodeId gidInt)
                                in case Binding.lookupBindParentUnder canonical c0 gref of
                                    Right Nothing -> Just gref
                                    _ -> Nothing
                in foldl' pickRoot Nothing genIds
        forM_ pairs $ \(_bv, arg) -> do
            let argC = canonical arg
            case Binding.lookupBindParent c0 (typeRef argC) of
                Just _ -> pure ()
                Nothing ->
                    case rootGen of
                        Just gref ->
                            modifyConstraint $ \c ->
                                Binding.setBindParent (typeRef argC) (gref, BindFlex) c
                        Nothing ->
                            when (Binding.isUpper c0 (typeRef expansionRootC) (typeRef argC)) $
                                modifyConstraint $ \c ->
                                    Binding.setBindParent (typeRef argC) (typeRef expansionRootC, BindFlex) c

bindingPathToRootUnderM
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeRef
    -> PresolutionM [NodeRef]
bindingPathToRootUnderM canonical c start = go IntSet.empty [start] start
  where
    go :: IntSet.IntSet -> [NodeRef] -> NodeRef -> PresolutionM [NodeRef]
    go visited path ref
        | IntSet.member (nodeRefKey ref) visited =
            throwError (BindingTreeError (BindingCycleDetected (reverse path)))
        | otherwise = do
            mbParentInfo <- case Binding.lookupBindParentUnder canonical c ref of
                Left err -> throwError (BindingTreeError err)
                Right p -> pure p
            case mbParentInfo of
                Nothing -> pure (reverse path)
                Just (parent, _flag) ->
                    go (IntSet.insert (nodeRefKey ref) visited) (parent : path) parent

requireValidBindingTree :: PresolutionM ()
requireValidBindingTree = do
    ensureBindingParents
    c0 <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
    case Binding.checkBindingTreeUnder canonical c0 of
        Left err -> throwError (BindingTreeError err)
        Right () -> pure ()

ensureBindingParents :: PresolutionM ()
ensureBindingParents = do
    st0 <- get
    let c0 = psConstraint st0
        uf = psUnionFind st0
        canonical = UnionFind.frWith uf
    case Binding.canonicalizeBindParentsUnder canonical c0 of
        Left err -> throwError (BindingTreeError err)
        Right bp0 -> do
            let nodes = cNodes c0
                nodeIds = map fst (toListNode nodes)
                nodeValues = map snd (toListNode nodes)
                allTypeIds =
                    IntSet.fromList
                        [ getNodeId (canonical nid)
                        | nid <- nodeIds
                        ]
                rootGen =
                    let genRefs =
                            [ genRef (GenNodeId gid)
                            | gid <- IntMap.keys (getGenNodeMap (cGenNodes c0))
                            ]
                        isRoot ref = not (IntMap.member (nodeRefKey ref) bp0)
                    in case filter isRoot genRefs of
                        (g:_) -> Just g
                        [] -> Nothing
                incomingParents =
                    let addOne parent child m =
                            IntMap.insertWith
                                IntSet.union
                                (getNodeId child)
                                (IntSet.singleton (getNodeId parent))
                                m
                        addNode m node =
                            let parent = canonical (tnId node)
                                kids = map canonical (structuralChildrenWithBounds node)
                            in foldl' (flip (addOne parent)) m kids
                    in foldl' addNode IntMap.empty nodeValues
                termRoots = Binding.computeTermDagRootsUnder canonical c0
                canonicalRef = Canonicalize.canonicalRef canonical
                addTypeEdges m node =
                    let parentKey = nodeRefKey (TypeRef (canonical (tnId node)))
                        childKeys =
                            IntSet.fromList
                                [ nodeRefKey (TypeRef (canonical child))
                                | child <- structuralChildrenWithBounds node
                                , canonical child /= canonical (tnId node)
                                ]
                    in if IntSet.null childKeys
                        then m
                        else IntMap.insertWith IntSet.union parentKey childKeys m
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
                structEdges =
                    foldl'
                        addTypeEdges
                        (foldl' addGenEdges IntMap.empty (NodeAccess.allGenNodes c0))
                        nodeValues
                isUpperUnder parent child =
                    let parentKey = nodeRefKey (canonicalRef parent)
                        childKey = nodeRefKey (canonicalRef child)
                        go visited key =
                            if IntSet.member key visited
                                then False
                                else if key == childKey
                                    then True
                                    else
                                        let visited' = IntSet.insert key visited
                                            kids = IntSet.toList (IntMap.findWithDefault IntSet.empty key structEdges)
                                        in any (go visited') kids
                    in if parentKey == childKey
                        then True
                        else go IntSet.empty parentKey
                addMissing bp nidInt =
                    let childRef = typeRef (NodeId nidInt)
                        childKey = nodeRefKey childRef
                    in if IntMap.member childKey bp
                        then bp
                        else
                            let parentRef =
                                    case IntMap.lookup nidInt incomingParents of
                                        Just ps ->
                                            case IntSet.toList ps of
                                                [] -> Nothing
                                                (p:_) -> Just (typeRef (NodeId p))
                                        Nothing ->
                                            if IntSet.member nidInt termRoots
                                                then rootGen
                                                else Nothing
                            in case parentRef of
                                Nothing -> bp
                                Just p ->
                                    if p == childRef
                                        then bp
                                        else IntMap.insert childKey (p, BindFlex) bp
                bp1 = IntSet.foldl' addMissing bp0 allTypeIds
                pickUpperParent childN =
                    case IntMap.lookup (getNodeId childN) incomingParents of
                        Just ps ->
                            case IntSet.toList ps of
                                (p:_) -> Just (typeRef (NodeId p))
                                [] -> rootGen
                        Nothing -> rootGen
                fixUpper bp =
                    IntMap.mapWithKey
                        (\childKey (parentRef, flag) ->
                            case nodeRefFromKey childKey of
                                GenRef _ -> (parentRef, flag)
                                TypeRef childN ->
                                    case parentRef of
                                        GenRef _ -> (parentRef, flag)
                                        _ ->
                                            if isUpperUnder parentRef (typeRef childN)
                                                then (parentRef, flag)
                                                else
                                                    case pickUpperParent childN of
                                                        Just pRef ->
                                                            if pRef == typeRef childN
                                                                then
                                                                    case rootGen of
                                                                        Just gref -> (gref, flag)
                                                                        Nothing -> (parentRef, flag)
                                                                else (pRef, flag)
                                                        Nothing -> (parentRef, flag)
                        )
                        bp
                bp2 = fixUpper bp1
            put st0 { psConstraint = c0 { cBindParents = bp2 } }

edgeInteriorExact :: NodeId -> PresolutionM IntSet.IntSet
edgeInteriorExact root0 = do
    c0 <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
    case Binding.interiorOfUnder canonical c0 (typeRef root0) of
        Left err -> throwError (BindingTreeError err)
        Right interior ->
            pure $
                IntSet.fromList
                    [ getNodeId nid
                    | key <- IntSet.toList interior
                    , TypeRef nid <- [nodeRefFromKey key]
                    ]

orderedBindersRawM :: NodeId -> PresolutionM [NodeId]
orderedBindersRawM binder0 = do
    c0 <- gets psConstraint
    case Binding.orderedBinders id c0 (typeRef binder0) of
        Left err -> throwError (BindingTreeError err)
        Right binders -> pure binders

-- | Resolve the instantiation binders for a node, skipping vacuous ∀ nodes.
-- Returns the body root to instantiate and the ordered binder list.
instantiationBindersM :: NodeId -> PresolutionM (NodeId, [NodeId])
instantiationBindersM nid0 = do
    st <- get
    let c0 = psConstraint st
        uf0 = psUnionFind st
        canonical = UnionFind.frWith uf0
        nid = canonical nid0
        cache0 = psBinderCache st
        nodes = cNodes c0
        lookupNode = Types.lookupNode nid nodes
    case IntMap.lookup (getNodeId nid) cache0 of
        Just binders ->
            if null binders
                then pure (nid, binders)
                else do
                    let root =
                            case lookupNode of
                                Just TyForall{ tnBody = inner } -> canonical inner
                                _ -> nid
                    let debugMsg =
                            "instantiationBindersM: nid="
                                ++ show nid
                                ++ " root="
                                ++ show root
                    debugBinders debugMsg
                    pure (root, binders)
        Nothing -> case lookupNode of
            Nothing -> throwError (NodeLookupFailed nid)
            Just node -> case node of
                TyForall { tnId = forallId, tnBody = inner } -> do
                    binders <- orderedBindersRawM forallId
                    if null binders
                        then instantiationBindersM inner
                        else do
                            when (not (null binders)) $
                                modify' $ \st1 ->
                                    let cache1 = psBinderCache st1
                                        cache2 = IntMap.insert (getNodeId nid) binders cache1
                                        cache3 = IntMap.insert (getNodeId inner) binders cache2
                                    in st1 { psBinderCache = cache3 }
                            pure (inner, binders)
                _ -> do
                    binders <- implicitBindersM canonical c0 nid
                    if null binders
                        then case VarStore.lookupVarBound c0 nid of
                            Just bnd -> do
                                binders' <- implicitBindersM canonical c0 (canonical bnd)
                                if null binders'
                                    then pure (nid, binders)
                                    else do
                                        when (not (null binders')) $
                                            modify' $ \st1 ->
                                                let cache1 = psBinderCache st1
                                                    cache2 = IntMap.insert (getNodeId nid) binders' cache1
                                                    cache3 = IntMap.insert (getNodeId (canonical bnd)) binders' cache2
                                                in st1 { psBinderCache = cache3 }
                                        pure (canonical bnd, binders')
                            Nothing -> pure (nid, binders)
                        else do
                            let root =
                                    nid
                            let debugMsg =
                                    "instantiationBindersM: nid="
                                        ++ show nid
                                        ++ " root="
                                        ++ show root
                            debugBinders debugMsg
                            when (not (null binders)) $
                                modify' $ \st1 ->
                                    let cache1 = psBinderCache st1
                                        cache2 = IntMap.insert (getNodeId nid) binders cache1
                                        cache3 = IntMap.insert (getNodeId root) binders cache2
                                    in st1 { psBinderCache = cache3 }
                            pure (root, binders)

implicitBindersM :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM [NodeId]
implicitBindersM canonical c0 root0 = do
    let root = canonical root0
        schemeOwner =
            listToMaybe
                [ gnId gen
                | gen <- NodeAccess.allGenNodes c0
                , any (\r -> canonical r == root) (gnSchemes gen)
                ]
        schemeOwnerByBody =
            listToMaybe
                [ gnId gen
                | gen <- NodeAccess.allGenNodes c0
                , any
                    (\r ->
                        case VarStore.lookupVarBound c0 (canonical r) of
                            Just bnd -> canonical bnd == root
                            Nothing -> False
                    )
                    (gnSchemes gen)
                ]
    path <- bindingPathToRootUnderM canonical c0 (typeRef root)
    case schemeOwner <|> schemeOwnerByBody <|> listToMaybe [gid | GenRef gid <- path] of
        Nothing -> pure []
        Just gid -> do
            let schemeRoots =
                    case NodeAccess.lookupGenNode c0 gid of
                        Just gen -> map canonical (gnSchemes gen)
                        Nothing -> []
                schemeRootSet =
                    IntSet.fromList (map (getNodeId . canonical) schemeRoots)
                isSchemeRoot = IntSet.member (getNodeId root) schemeRootSet
                rootIsWrapper =
                    case NodeAccess.lookupNode c0 root of
                        Just TyVar{ tnBound = Just _ } -> not isSchemeRoot
                        _ -> False
            bindersFromGen <- case Binding.boundFlexChildrenUnder canonical c0 (genRef gid) of
                Left err -> throwError (BindingTreeError err)
                Right bs -> pure bs
            bindersFromRoot <- case Binding.boundFlexChildrenUnder canonical c0 (typeRef root) of
                Left _ -> pure []
                Right bs -> pure bs
            let binders0 =
                    IntMap.elems $
                        IntMap.fromList
                            [ (getNodeId b, b)
                            | b <- bindersFromGen ++ bindersFromRoot
                            ]
            let binders1 =
                    filter (\b -> not (IntSet.member (getNodeId (canonical b)) schemeRootSet)) binders0
                binders2 =
                    if rootIsWrapper
                        then filter (/= root) binders1
                        else binders1
                nodes = cNodes c0
                reachable = reachableFromWithBounds nodes root
                bindersReachable =
                    [ canonical b
                    | b <- binders2
                    , IntSet.member (getNodeId (canonical b)) reachable
                    ]
                bindersCanon =
                    IntMap.elems $
                        IntMap.fromList
                            [ (getNodeId b, b)
                            | b <- bindersReachable
                            ]
                orderKeys = Order.orderKeysFromConstraintWith canonical c0 root Nothing
                missing =
                    [ b
                    | b <- bindersCanon
                    , not (IntMap.member (getNodeId b) orderKeys)
                    ]
            unless (null missing) $
                throwError $
                    InternalError ("implicitBindersM: missing order keys for " ++ show missing)
            orderBindersByDeps orderKeys bindersCanon
  where
    orderBindersByDeps
        :: IntMap.IntMap Order.OrderKey
        -> [NodeId]
        -> PresolutionM [NodeId]
    orderBindersByDeps orderKeys binders =
        let binderSet = IntSet.fromList (map getNodeId binders)
            depsFor b =
                case VarStore.lookupVarBound c0 b of
                    Nothing -> IntSet.empty
                    Just bnd ->
                        IntSet.delete
                            (getNodeId b)
                            (IntSet.intersection binderSet (reachableFromWithBounds (cNodes c0) bnd))
            depsMap =
                IntMap.fromList
                    [ (getNodeId b, depsFor b)
                    | b <- binders
                    ]
            go :: IntSet.IntSet -> [NodeId] -> [NodeId] -> PresolutionM [NodeId]
            go _ [] acc = pure acc
            go done remaining acc =
                let ready =
                        [ b
                        | b <- remaining
                        , let deps = IntMap.findWithDefault IntSet.empty (getNodeId b) depsMap
                        , IntSet.isSubsetOf deps done
                        ]
                in if null ready
                    then
                        throwError $
                            InternalError "implicitBindersM: cycle in bound dependencies"
                    else case Order.sortByOrderKey orderKeys ready of
                        Left err ->
                            throwError $
                                InternalError ("implicitBindersM: order key error: " ++ show err)
                        Right readySorted ->
                            let readySet = IntSet.fromList (map getNodeId readySorted)
                                done' = IntSet.union done readySet
                                remaining' = filter (\b -> not (IntSet.member (getNodeId b) readySet)) remaining
                            in go done' remaining' (acc ++ readySorted)
        in go IntSet.empty binders []

    reachableFromWithBounds
        :: NodeMap TyNode
        -> NodeId
        -> IntSet.IntSet
    reachableFromWithBounds nodes root =
        Traversal.reachableFromNodes canonical children [root]
      where
        children nid =
            case Types.lookupNode nid nodes of
                Nothing -> []
                Just node -> structuralChildrenWithBounds node

forallSpecM :: NodeId -> PresolutionM ForallSpec
forallSpecM binder0 = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
    case Binding.forallSpecFromForall canonical c0 binder0 of
        Left err -> throwError (BindingTreeError err)
        Right fs -> pure fs

-- | Debug binders using explicit trace config.
debugBinders :: String -> PresolutionM ()
debugBinders msg = do
    cfg <- ask
    traceBindingM cfg msg

-- | Drop trivial scheme edges (let edges) from the result maps.
dropTrivialSchemeEdges
    :: Constraint
    -> IntMap EdgeWitness
    -> IntMap EdgeTrace
    -> IntMap Expansion
    -> (IntMap EdgeWitness, IntMap EdgeTrace, IntMap Expansion)
dropTrivialSchemeEdges constraint witnesses traces expansions =
    let dropEdgeIds = cLetEdges constraint
        keepEdge eid = not (IntSet.member eid dropEdgeIds)
        witnesses' = IntMap.filterWithKey (\eid _ -> keepEdge eid) witnesses
        traces' = IntMap.filterWithKey (\eid _ -> keepEdge eid) traces
        expansions' = IntMap.filterWithKey (\eid _ -> keepEdge eid) expansions
    in (witnesses', traces', expansions')

-- | MonadPresolution instance for ReaderT, allowing presolution operations
-- to be used within ReaderT transformers without explicit lift.
instance {-# OVERLAPPABLE #-} MonadPresolution m => MonadPresolution (ReaderT r m) where
    getConstraint = lift getConstraint
    modifyConstraint f = lift (modifyConstraint f)
    getPresolutionState = lift getPresolutionState
    putPresolutionState st = lift (putPresolutionState st)
    throwPresolutionError err = lift (throwPresolutionError err)
    modifyPresolution f = lift (modifyPresolution f)
    bindExpansionArgs root pairs = lift (bindExpansionArgs root pairs)
