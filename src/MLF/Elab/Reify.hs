module MLF.Elab.Reify (
    reifyType,
    reifyTypeWithNames,
    reifyTypeWithNamesNoFallback,
    reifyTypeWithNamedSet,
    reifyBoundWithNames,
    freeVars,
    namedNodes
) where

import Control.Monad (unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Util (topoSortBy)
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Binding.Tree (boundFlexChildrenAllUnder, canonicalizeBindParentsUnder, lookupBindParent, lookupBindParentUnder)
import qualified MLF.Constraint.VarStore as VarStore

data ReifyRoot
    = RootType
    | RootTypeNoFallback
    | RootBound

data ReifyMode
    = ModeType
    | ModeTypeNoFallback
    | ModeBound
    deriving (Eq)

data ReifyCache = ReifyCache
    { cacheType :: IntMap.IntMap ElabType
    , cacheTypeNoFallback :: IntMap.IntMap ElabType
    , cacheBound :: IntMap.IntMap ElabType
    }

emptyCache :: ReifyCache
emptyCache = ReifyCache IntMap.empty IntMap.empty IntMap.empty

cacheLookup :: ReifyMode -> ReifyCache -> Int -> Maybe ElabType
cacheLookup mode cache key = case mode of
    ModeType -> IntMap.lookup key (cacheType cache)
    ModeTypeNoFallback -> IntMap.lookup key (cacheTypeNoFallback cache)
    ModeBound -> IntMap.lookup key (cacheBound cache)

cacheInsert :: ReifyMode -> Int -> ElabType -> ReifyCache -> ReifyCache
cacheInsert mode key ty cache = case mode of
    ModeType -> cache { cacheType = IntMap.insert key ty (cacheType cache) }
    ModeTypeNoFallback -> cache { cacheTypeNoFallback = IntMap.insert key ty (cacheTypeNoFallback cache) }
    ModeBound -> cache { cacheBound = IntMap.insert key ty (cacheBound cache) }

reifyWith
    :: String
    -> SolveResult
    -> (NodeId -> String)
    -> (NodeId -> Bool)
    -> ReifyRoot
    -> NodeId
    -> Either ElabError ElabType
reifyWith _contextLabel res nameForVar isNamed rootMode nid =
    let start = case rootMode of
            RootType -> goType
            RootTypeNoFallback -> goTypeNoFallback
            RootBound -> goBound
    in snd <$> start emptyCache (canonical nid)
  where
    constraint = srConstraint res
    nodes = cNodes constraint
    uf = srUnionFind res
    canonical = Solve.frWith uf

    lookupNode k = maybe (Left (MissingNode k)) Right (IntMap.lookup (getNodeId k) nodes)

    varName n = nameForVar (canonical n)
    varFor n = TVar (varName n)

    goType cache n = goFull cache ModeType n
    goTypeNoFallback cache n = goFull cache ModeTypeNoFallback n

    goFull cache mode n0 =
        let n = canonical n0
            key = getNodeId n
        in case cacheLookup mode cache key of
            Just t -> Right (cache, t)
            Nothing -> do
                node <- lookupNode n
                case node of
                    TyVar{} ->
                        if VarStore.isEliminatedVar constraint n
                            then
                                let t = TBottom
                                    cache' = cacheInsert mode key t cache
                                in pure (cache', t)
                            else
                                let t = varFor n
                                    cache' = cacheInsert mode key t cache
                                in pure (cache', t)
                    _
                        | isNamed (canonical n) ->
                            let t = varFor n
                                cache' = cacheInsert mode key t cache
                            in pure (cache', t)
                        | otherwise -> do
                            binders <- orderedFlexChildren mode n
                            (cache', core) <- case node of
                                TyBase{ tnBase = b } -> pure (cache, TBase b)
                                TyBottom{} -> pure (cache, TBottom)
                                TyArrow{ tnDom = d, tnCod = c } -> do
                                    (cache1, d') <- vChild cache mode (canonical d)
                                    (cache2, c') <- vChild cache1 mode (canonical c)
                                    pure (cache2, TArrow d' c')
                                TyForall{ tnBody = b } ->
                                    let bodyC = canonical b
                                    in if bodyC `elem` binders
                                        then pure (cache, varFor bodyC)
                                        else vChild cache mode bodyC
                                TyExp{ tnBody = b } ->
                                    goFull cache mode (canonical b)
                            (cache'', t) <- wrapBinders cache' core binders
                            let cacheFinal = cacheInsert mode key t cache''
                            pure (cacheFinal, t)

    goBound cache n = do
        node <- lookupNode n
        case node of
            TyVar{} ->
                if VarStore.isEliminatedVar constraint n
                    then pure (cache, TBottom)
                    else case VarStore.lookupVarBound constraint n of
                        Nothing -> pure (cache, TBottom)
                        Just bnd ->
                            let bndC = canonical bnd
                            in if bndC == n
                                then pure (cache, TBottom)
                                else goFull cache ModeBound bndC
            _ -> goFull cache ModeBound n

    vChild cache mode child = do
        mbParent <- bindingToElab (lookupBindParentUnder canonical constraint (typeRef child))
        case mbParent of
            Just (_, BindRigid) -> goBound cache child
            Just (_, BindFlex) ->
                case mode of
                    ModeBound ->
                        case IntMap.lookup (getNodeId (canonical child)) nodes of
                            Just TyVar{} | VarStore.isEliminatedVar constraint (canonical child) ->
                                goBound cache child
                            Just TyVar{} -> pure (cache, varFor child)
                            _ -> goFull cache mode child
                    _ ->
                        case IntMap.lookup (getNodeId (canonical child)) nodes of
                            Just TyVar{} -> pure (cache, varFor child)
                            _ -> goFull cache mode child
            Nothing -> goBound cache child

    wrapBinders cache inner binders =
        foldrM
            (\b (cacheAcc, acc) -> do
                (cache', boundTy) <- goBound cacheAcc b
                let mbBound = if boundTy == TBottom then Nothing else Just boundTy
                pure (cache', TForall (varName b) mbBound acc)
            )
            (cache, inner)
            binders

    orderedFlexChildren mode n0 = do
        let n = canonical n0
        node <- lookupNode n
        let orderRoot =
                case node of
                    TyForall{ tnBody = body } -> canonical body
                    _ -> n
            reachable =
                let go visited [] = visited
                    go visited (nid0:rest) =
                        let nodeId = canonical nid0
                            key = getNodeId nodeId
                        in if IntSet.member key visited
                            then go visited rest
                            else
                                let visited' = IntSet.insert key visited
                                    kids =
                                        case IntMap.lookup key nodes of
                                            Nothing -> []
                                            Just node0 ->
                                                let boundKids =
                                                        case node0 of
                                                            TyVar{ tnBound = Just bnd } -> [bnd]
                                                            _ -> []
                                                in structuralChildren node0 ++ boundKids
                                in go visited' (map canonical kids ++ rest)
                in go IntSet.empty [orderRoot]
        binders0 <- directFlexChildren (mode /= ModeType) n
        immediateGen <- bindingToElab (lookupBindParentUnder canonical constraint (typeRef n))
        bindersBase <-
            if null binders0 && not (isForall node) && mode /= ModeTypeNoFallback
                then case mode of
                    ModeBound ->
                        case immediateGen of
                            Just (GenRef gid, _) ->
                                bindingToElab (boundFlexChildrenAllUnder canonical constraint (genRef gid))
                            _ -> pure binders0
                    _ -> do
                        mGen <- closestGenAncestor n
                        case mGen of
                            Just gid ->
                                bindingToElab (boundFlexChildrenAllUnder canonical constraint (genRef gid))
                            Nothing -> pure binders0
                else pure binders0
        let isBinderNode candidate =
                case IntMap.lookup (getNodeId (canonical candidate)) nodes of
                    Just TyExp{} -> False
                    Just TyBase{} -> False
                    Just TyBottom{} -> False
                    Just _ -> True
                    Nothing -> False
            bindersReachable0 =
                [ canonical b
                | b <- bindersBase
                , isBinderNode b
                , IntSet.member (getNodeId (canonical b)) reachable
                ]
            bindersReachable =
                case mode of
                    ModeBound -> filter (/= n) bindersReachable0
                    _ -> bindersReachable0
            binderKeys = map (getNodeId . canonical) bindersReachable
            binderSet = IntSet.fromList binderKeys
            orderKeys = Order.orderKeysFromRoot res orderRoot
            missing =
                [ NodeId k
                | k <- binderKeys
                , not (IntMap.member k orderKeys)
                ]
            depsFor k =
                [ d
                | d <- IntSet.toList (freeVars res (NodeId k) IntSet.empty)
                , IntSet.member d binderSet
                , d /= k
                ]
            cmpReady a b =
                case Order.compareNodesByOrderKey orderKeys (NodeId a) (NodeId b) of
                    EQ -> compare a b
                    other -> other
        unless (null missing) $
            Left $
                InstantiationError $
                    "reifyType: missing order keys for " ++ show missing
        orderedKeys <-
            topoSortBy
                "reifyType: cycle in binder bound dependencies"
                cmpReady
                depsFor
                binderKeys
        pure [ canonical (NodeId k) | k <- orderedKeys ]
      where
        directFlexChildren includeAll parentN =
            if not includeAll
                then bindingToElab (boundFlexChildrenAllUnder canonical constraint (typeRef parentN))
                else do
                    bindParents <- bindingToElab (canonicalizeBindParentsUnder canonical constraint)
                    let parentRef = typeRef parentN
                        isChild (_, (parent, flag)) =
                            parent == parentRef && flag == BindFlex
                        childNode (childKey, _) =
                            case nodeRefFromKey childKey of
                                TypeRef childN -> Just childN
                                GenRef _ -> Nothing
                        isBindable child =
                            case IntMap.lookup (getNodeId child) nodes of
                                Just TyExp{} -> False
                                Just TyBase{} -> False
                                Just TyBottom{} -> False
                                Just _ -> True
                                Nothing -> False
                    pure
                        [ canonical child
                        | entry <- IntMap.toList bindParents
                        , isChild entry
                        , Just child <- [childNode entry]
                        , isBindable child
                        ]

    isForall :: TyNode -> Bool
    isForall TyForall{} = True
    isForall _ = False

    closestGenAncestor :: NodeId -> Either ElabError (Maybe GenNodeId)
    closestGenAncestor start = go IntSet.empty (typeRef start)
      where
        go visited ref
            | IntSet.member (nodeRefKey ref) visited = Right Nothing
            | otherwise = do
                mbParent <- bindingToElab (lookupBindParentUnder canonical constraint ref)
                case mbParent of
                    Nothing -> Right Nothing
                    Just (GenRef gid, _) -> Right (Just gid)
                    Just (TypeRef parent, _) ->
                        go (IntSet.insert (nodeRefKey ref) visited) (typeRef (canonical parent))


    foldrM :: (a -> b -> Either ElabError b) -> b -> [a] -> Either ElabError b
    foldrM _ z [] = Right z
    foldrM f z (x:xs) = do
        z' <- foldrM f z xs
        f x z'

-- | Reify a solved NodeId into an elaborated type.
-- This version doesn't compute instance bounds (all foralls are unbounded).
reifyType :: SolveResult -> NodeId -> Either ElabError ElabType
reifyType res nid =
    reifyWith "reifyType" res nameFor (const False) RootType nid
  where
    nameFor (NodeId i) = "t" ++ show i

-- | Reify with an explicit name substitution for vars (Sχ′: named nodes become variables).
reifyTypeWithNames :: SolveResult -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNames res subst nid = do
    namedSet <- namedNodes res
    reifyTypeWithNamedSet res subst namedSet nid

-- | Reify with an explicit name substitution, but without ancestor fallback
-- quantifiers (used when an outer scheme already quantifies binders).
reifyTypeWithNamesNoFallback :: SolveResult -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallback res subst nid =
    let uf = srUnionFind res
        canonical = Solve.frWith uf

        nameFor (NodeId i) = "t" ++ show i

        varNameFor v =
            let cv = canonical v
            in maybe (nameFor cv) id (IntMap.lookup (getNodeId cv) subst)

        isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst
    in reifyWith "reifyTypeWithNamesNoFallback" res varNameFor isNamed RootTypeNoFallback nid

-- | Reify with an explicit named-node set (Sχ′).
reifyTypeWithNamedSet :: SolveResult -> IntMap.IntMap String -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
reifyTypeWithNamedSet res subst namedSet nid =
    reifyWith "reifyTypeWithNames" res varNameFor isNamed RootType nid
  where
    uf = srUnionFind res
    canonical = Solve.frWith uf

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in maybe (nameFor cv) id (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

-- | Reify a node for use as a binder bound (T(n) in the paper, Sχp).
reifyBoundWithNames :: SolveResult -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNames res subst nid =
    reifyWith "reifyBoundWithNames" res varNameFor isNamed RootBound nid
  where
    uf = srUnionFind res
    canonical = Solve.frWith uf
    selfKey = getNodeId (canonical nid)

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in maybe (nameFor cv) id (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId =
        let key = getNodeId (canonical nodeId)
        in key /= selfKey && IntMap.member key subst

namedNodes :: SolveResult -> Either ElabError IntSet.IntSet
namedNodes res = do
    let constraint = srConstraint res
        canonical = Solve.frWith (srUnionFind res)
        nodes = cNodes constraint
        genSchemes =
            IntMap.fromList
                [ (getGenNodeId (gnId gen), IntSet.fromList (map (getNodeId . canonical) (gnSchemes gen)))
                | gen <- IntMap.elems (cGenNodes constraint)
                ]
        reachableFrom root0 =
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
                            in go visited' (kids ++ rest)
            in go IntSet.empty [root0]
        reachableFromRoots roots =
            foldl' IntSet.union IntSet.empty (map reachableFrom roots)
        genReachable =
            IntMap.fromList
                [ (getGenNodeId (gnId gen), reachableFromRoots (gnSchemes gen))
                | gen <- IntMap.elems (cGenNodes constraint)
                ]
    bindParents <- bindingToElab (canonicalizeBindParentsUnder canonical constraint)
    let isQuantifiable nid =
            case IntMap.lookup (getNodeId nid) nodes of
                Just TyExp{} -> False
                Just TyBase{} -> False
                Just TyBottom{} -> False
                Just _ -> True
                Nothing -> False
        isSchemeRoot gid nid =
            case IntMap.lookup (getGenNodeId gid) genSchemes of
                Nothing -> False
                Just roots -> IntSet.member (getNodeId (canonical nid)) roots
        inScheme gid nid =
            case IntMap.lookup (getGenNodeId gid) genReachable of
                Nothing -> False
                Just reachable -> IntSet.member (getNodeId (canonical nid)) reachable
        named =
            [ getNodeId childC
            | (childKey, (parent, flag)) <- IntMap.toList bindParents
            , flag == BindFlex
            , GenRef gid <- [parent]
            , TypeRef child <- [nodeRefFromKey childKey]
            , let childC = canonical child
            , isQuantifiable childC
            , not (isSchemeRoot gid childC)
            , inScheme gid childC
            ]
    pure (IntSet.fromList named)

-- | Collect free variables by NodeId, skipping vars under TyForall.
freeVars :: SolveResult -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars res nid visited
    | IntSet.member key visited = IntSet.empty
    | otherwise =
        let visited' = IntSet.insert key visited
        in case IntMap.lookup key nodes of
            Nothing -> IntSet.empty
            Just TyVar{} ->
                case VarStore.lookupVarBound constraint (canonical nid) of
                    Nothing -> IntSet.empty
                    Just bnd -> freeVars res (canonical bnd) visited'
            Just TyBase{} -> IntSet.empty
            Just TyBottom{} -> IntSet.empty
            Just TyArrow{ tnDom = d, tnCod = c } ->
                freeVarsChild visited' d `IntSet.union`
                freeVarsChild visited' c
            Just TyForall{ tnBody = b } ->
                freeVarsChild visited' b
            Just TyExp{ tnBody = b } ->
                freeVars res (canonical b) visited'
  where
    constraint = srConstraint res
    nodes = cNodes constraint
    uf = srUnionFind res
    canonical = Solve.frWith uf
    key = getNodeId (canonical nid)

    freeVarsChild visited' child =
        case lookupBindParent constraint (typeRef (canonical child)) of
            Just (_, BindRigid) -> freeVars res (canonical child) visited'
            _ -> IntSet.singleton (getNodeId (canonical child))
