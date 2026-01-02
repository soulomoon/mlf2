module MLF.Constraint.Presolution.Base (
    PresolutionResult(..),
    PresolutionError(..),
    PresolutionState(..),
    EdgeTrace(..),
    CopyMap,
    InteriorSet,
    emptyTrace,
    unionTrace,
    PresolutionM,
    runPresolutionM,
    requireValidBindingTree,
    edgeInteriorExact,
    orderedBindersM,
    instantiationBindersM,
    forallSpecM
) where

import Control.Monad.State (StateT, get, gets, put, runStateT)
import Control.Monad.Except (throwError)
import Control.Monad (unless)
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
    | InternalError String                   -- ^ Unexpected internal state
    deriving (Eq, Show)

-- | State maintained during the presolution process.
data PresolutionState = PresolutionState
    { psConstraint :: Constraint
    , psPresolution :: Presolution
    , psUnionFind :: IntMap NodeId
    , psNextNodeId :: Int
    , psPendingWeakens :: IntSet.IntSet
    , psEdgeExpansions :: IntMap Expansion
    , psEdgeWitnesses :: IntMap EdgeWitness
    , psEdgeTraces :: IntMap EdgeTrace
    }
    deriving (Eq, Show)

-- | Per-edge provenance for instantiation-related operations.
--
-- This is an internal aid for gradually aligning presolution witnesses with
-- `papers/xmlf.txt`’s normalized instance-operation language (Fig. 10). For now,
-- we only track the binder↦argument pairing chosen by `ExpInstantiate`.
data EdgeTrace = EdgeTrace
    { etRoot :: NodeId
    , etBinderArgs :: [(NodeId, NodeId)] -- ^ (binder node, instantiation argument node)
    , etInterior :: IntSet.IntSet -- ^ Nodes in I(r) (exact, from the binding tree).
    , etCopyMap :: IntMap NodeId -- ^ Provenance: original node -> copied/replaced node
    }
    deriving (Eq, Show)

type CopyMap = IntMap NodeId
type InteriorSet = IntSet.IntSet

emptyTrace :: (CopyMap, InteriorSet)
emptyTrace = (IntMap.empty, IntSet.empty)

unionTrace :: (CopyMap, InteriorSet) -> (CopyMap, InteriorSet) -> (CopyMap, InteriorSet)
unionTrace (m1, s1) (m2, s2) = (IntMap.union m1 m2, IntSet.union s1 s2)

-- | The Presolution monad.
type PresolutionM = StateT PresolutionState (Either PresolutionError)

-- | Run a PresolutionM action with an initial state (testing helper).
runPresolutionM :: PresolutionState -> PresolutionM a -> Either PresolutionError (a, PresolutionState)
runPresolutionM st action = runStateT action st

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
            put st0 { psConstraint = c0 { cBindParents = bp1 } }

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

-- | Resolve the instantiation binders for a node, skipping vacuous ∀ nodes.
-- Returns the body root to instantiate and the ordered binder list.
instantiationBindersM :: NodeId -> PresolutionM (NodeId, [NodeId])
instantiationBindersM nid0 = do
    st <- get
    let c0 = psConstraint st
        uf0 = psUnionFind st
        canonical = UnionFind.frWith uf0
        nid = canonical nid0
        nodes = cNodes c0
        lookupNode = IntMap.lookup (getNodeId nid) nodes
    case lookupNode of
        Nothing -> throwError (NodeLookupFailed nid)
        Just node -> case node of
            TyForall { tnId = forallId, tnBody = inner } -> do
                binders <- orderedBindersM forallId
                if null binders
                    then instantiationBindersM inner
                    else pure (inner, binders)
            _ -> do
                binders <- implicitBindersM canonical c0 nid
                pure (nid, binders)

implicitBindersM :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM [NodeId]
implicitBindersM canonical c0 root0 = do
    let root = canonical root0
    path <- bindingPathToRootUnder c0 (typeRef root)
    case listToMaybe [gid | GenRef gid <- path] of
        Nothing -> pure []
        Just gid -> do
            binders0 <- case Binding.boundFlexChildrenUnder canonical c0 (genRef gid) of
                Left err -> throwError (BindingTreeError err)
                Right bs -> pure bs
            let nodes = cNodes c0
                reachable = reachableFromWithBounds nodes root
                bindersReachable =
                    [ canonical b
                    | b <- binders0
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

    bindingPathToRootUnder
        :: Constraint
        -> NodeRef
        -> PresolutionM [NodeRef]
    bindingPathToRootUnder c start = go IntSet.empty [start] start
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

forallSpecM :: NodeId -> PresolutionM ForallSpec
forallSpecM binder0 = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
    case Binding.forallSpecFromForall canonical c0 binder0 of
        Left err -> throwError (BindingTreeError err)
        Right fs -> pure fs
