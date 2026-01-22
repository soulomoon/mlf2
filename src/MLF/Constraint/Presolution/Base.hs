module MLF.Constraint.Presolution.Base (
    PresolutionResult(..),
    PresolutionError(..),
    TranslatabilityIssue(..),
    PresolutionState(..),
    EdgeTrace(..),
    CopyMap,
    InteriorSet,
    FrontierSet,
    emptyTrace,
    unionTrace,
    PresolutionM,
    runPresolutionM,
    bindingPathToRootUnderM,
    requireValidBindingTree,
    edgeInteriorExact,
    orderedBindersM,
    instantiationBindersM,
    forallSpecM
) where

import Control.Applicative ((<|>))
import Control.Monad.State (StateT, get, gets, modify', put, runStateT)
import Control.Monad.Except (throwError)
import Control.Monad (unless, when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortBy)
import Data.Maybe (listToMaybe)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.Order as Order
import qualified MLF.Util.UnionFind as UnionFind
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Result of the presolution phase.
data PresolutionResult = PresolutionResult
    { prConstraint :: Constraint
    , prEdgeExpansions :: IntMap Expansion
    , prEdgeWitnesses :: IntMap EdgeWitness
    , prEdgeTraces :: IntMap EdgeTrace
    , prRedirects :: IntMap NodeId -- ^ Map from old TyExp IDs to their replacement IDs
    } deriving (Eq, Show)

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
    , etInterior :: IntSet.IntSet -- ^ Nodes in I(r) (exact, from the binding tree).
    , etCopyMap :: IntMap NodeId -- ^ Provenance: original node -> copied/replaced node
    }
    deriving (Eq, Show)

type CopyMap = IntMap NodeId
type InteriorSet = IntSet.IntSet
type FrontierSet = IntSet.IntSet

emptyTrace :: (CopyMap, InteriorSet, FrontierSet)
emptyTrace = (IntMap.empty, IntSet.empty, IntSet.empty)

unionTrace :: (CopyMap, InteriorSet, FrontierSet) -> (CopyMap, InteriorSet, FrontierSet) -> (CopyMap, InteriorSet, FrontierSet)
unionTrace (m1, s1, f1) (m2, s2, f2) =
    (IntMap.union m1 m2, IntSet.union s1 s2, IntSet.union f1 f2)

-- | The Presolution monad.
type PresolutionM = StateT PresolutionState (Either PresolutionError)

-- | Run a PresolutionM action with an initial state (testing helper).
runPresolutionM :: PresolutionState -> PresolutionM a -> Either PresolutionError (a, PresolutionState)
runPresolutionM st action = runStateT action st

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
                allTypeIds =
                    IntSet.fromList
                        [ getNodeId (canonical (NodeId nid))
                        | nid <- IntMap.keys nodes
                        ]
                rootGen =
                    let genRefs =
                            [ genRef (GenNodeId gid)
                            | gid <- IntMap.keys (cGenNodes c0)
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
                                boundKids =
                                    case node of
                                        TyVar{ tnBound = Just bnd } -> [bnd]
                                        _ -> []
                                kids = map canonical (structuralChildren node ++ boundKids)
                            in foldl' (flip (addOne parent)) m kids
                    in IntMap.foldl' addNode IntMap.empty nodes
                termRoots = Binding.computeTermDagRootsUnder canonical c0
                canonicalRef ref = case ref of
                    TypeRef nid -> TypeRef (canonical nid)
                    GenRef _ -> ref
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
                        (foldl' addGenEdges IntMap.empty (IntMap.elems (cGenNodes c0)))
                        (IntMap.elems nodes)
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

orderedBindersM :: NodeId -> PresolutionM [NodeId]
orderedBindersM binder0 = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
    case Binding.orderedBinders canonical c0 (typeRef binder0) of
        Left err -> throwError (BindingTreeError err)
        Right binders -> pure binders

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
        lookupNode = IntMap.lookup (getNodeId nid) nodes
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
                    case debugBinders debugMsg () of
                        () -> pure ()
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
                            case debugBinders debugMsg () of
                                () -> pure ()
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
                | gen <- IntMap.elems (cGenNodes c0)
                , any (\r -> canonical r == root) (gnSchemes gen)
                ]
        schemeOwnerByBody =
            listToMaybe
                [ gnId gen
                | gen <- IntMap.elems (cGenNodes c0)
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
                    case IntMap.lookup (genNodeKey gid) (cGenNodes c0) of
                        Just gen -> map canonical (gnSchemes gen)
                        Nothing -> []
                schemeRootSet =
                    IntSet.fromList (map (getNodeId . canonical) schemeRoots)
                isSchemeRoot = IntSet.member (getNodeId root) schemeRootSet
                rootIsWrapper =
                    case IntMap.lookup (getNodeId root) (cNodes c0) of
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
                    else
                        let readySorted = sortBy (Order.compareNodesByOrderKey orderKeys) ready
                            readySet = IntSet.fromList (map getNodeId readySorted)
                            done' = IntSet.union done readySet
                            remaining' = filter (\b -> not (IntSet.member (getNodeId b) readySet)) remaining
                        in go done' remaining' (acc ++ readySorted)
        in go IntSet.empty binders []

    reachableFromWithBounds
        :: IntMap TyNode
        -> NodeId
        -> IntSet.IntSet
    reachableFromWithBounds nodes root =
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
                                        in map canonical (structuralChildren node ++ boundKids)
                        in go visited' (kids ++ rest)
        in go IntSet.empty [root]

forallSpecM :: NodeId -> PresolutionM ForallSpec
forallSpecM binder0 = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
    case Binding.forallSpecFromForall canonical c0 binder0 of
        Left err -> throwError (BindingTreeError err)
        Right fs -> pure fs

debugBinders :: String -> a -> a
debugBinders msg value =
    if debugBindersEnabled
        then trace msg value
        else value

debugBindersEnabled :: Bool
debugBindersEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_BINDING"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugBindersEnabled #-}
