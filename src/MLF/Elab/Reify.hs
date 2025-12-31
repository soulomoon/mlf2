module MLF.Elab.Reify (
    reifyType,
    reifyTypeWithNames,
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
import qualified MLF.Constraint.Traversal as Traversal

data ReifyRoot
    = RootType
    | RootBound

data ReifyMode
    = ModeType
    | ModeBound
    deriving (Eq)

data ReifyCache = ReifyCache
    { cacheType :: IntMap.IntMap ElabType
    , cacheBound :: IntMap.IntMap ElabType
    }

emptyCache :: ReifyCache
emptyCache = ReifyCache IntMap.empty IntMap.empty

cacheLookup :: ReifyMode -> ReifyCache -> Int -> Maybe ElabType
cacheLookup mode cache key = case mode of
    ModeType -> IntMap.lookup key (cacheType cache)
    ModeBound -> IntMap.lookup key (cacheBound cache)

cacheInsert :: ReifyMode -> Int -> ElabType -> ReifyCache -> ReifyCache
cacheInsert mode key ty cache = case mode of
    ModeType -> cache { cacheType = IntMap.insert key ty (cacheType cache) }
    ModeBound -> cache { cacheBound = IntMap.insert key ty (cacheBound cache) }

reifyWith
    :: String
    -> SolveResult
    -> (NodeId -> String)
    -> (NodeId -> Bool)
    -> ReifyRoot
    -> NodeId
    -> Either ElabError ElabType
reifyWith contextLabel res nameForVar isNamed rootMode nid =
    let start = case rootMode of
            RootType -> goType
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
                        | mode == ModeType && isNamed (canonical n) ->
                            let t = varFor n
                                cache' = cacheInsert mode key t cache
                            in pure (cache', t)
                        | otherwise -> do
                            binders <- orderedFlexChildren n
                            (cache', core) <- case node of
                                TyBase{ tnBase = b } -> pure (cache, TBase b)
                                TyBottom{} -> pure (cache, TBottom)
                                TyArrow{ tnDom = d, tnCod = c } -> do
                                    (cache1, d') <- vChild cache (canonical d)
                                    (cache2, c') <- vChild cache1 (canonical c)
                                    pure (cache2, TArrow d' c')
                                TyForall{ tnBody = b } ->
                                    vChild cache (canonical b)
                                TyExp{ tnBody = b } ->
                                    goFull cache mode (canonical b)
                                TyRoot{} ->
                                    Left $
                                        InstantiationError $
                                            contextLabel ++ ": unexpected TyRoot at " ++ show (getNodeId n)
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

    vChild cache child = do
        mbParent <- bindingToElab (lookupBindParentUnder canonical constraint child)
        case mbParent of
            Just (_, BindRigid) -> goBound cache child
            Just (_, BindFlex) -> pure (cache, varFor child)
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

    orderedFlexChildren n0 = do
        let n = canonical n0
        node <- lookupNode n
        let orderRoot =
                case node of
                    TyForall{ tnBody = body } -> canonical body
                    _ -> n
            reachable =
                Traversal.reachableFromUnderLenient
                    canonical
                    (\nodeId -> IntMap.lookup (getNodeId nodeId) nodes)
                    orderRoot
        binders0 <- bindingToElab (boundFlexChildrenAllUnder canonical constraint n)
        let eliminated = cEliminatedVars constraint
            bindersReachable =
                [ canonical b
                | b <- binders0
                , not (IntSet.member (getNodeId (canonical b)) eliminated)
                , IntSet.member (getNodeId (canonical b)) reachable
                ]
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
    reifyWith "reifyBoundWithNames" res varNameFor (const False) RootBound nid
  where
    uf = srUnionFind res
    canonical = Solve.frWith uf

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in maybe (nameFor cv) id (IntMap.lookup (getNodeId cv) subst)

namedNodes :: SolveResult -> Either ElabError IntSet.IntSet
namedNodes res = do
    let constraint = srConstraint res
        canonical = Solve.frWith (srUnionFind res)
        genNodes = cGenNodes constraint
        eliminated = cEliminatedVars constraint
    bindParents <- bindingToElab (canonicalizeBindParentsUnder canonical constraint)
    let isGenNode nid = IntSet.member (getNodeId (canonical nid)) genNodes
        isEliminated nid = IntSet.member (getNodeId nid) eliminated
        named =
            [ getNodeId child
            | (childId, (parent, flag)) <- IntMap.toList bindParents
            , let child = canonical (NodeId childId)
            , let parentC = canonical parent
            , flag == BindFlex
            , isGenNode parentC
            , not (isGenNode child)
            , not (isEliminated child)
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
            Just TyRoot{ tnChildren = cs } ->
                IntSet.unions (map (\child -> freeVarsChild visited' child) cs)
  where
    constraint = srConstraint res
    nodes = cNodes constraint
    uf = srUnionFind res
    canonical = Solve.frWith uf
    key = getNodeId (canonical nid)

    freeVarsChild visited' child =
        case lookupBindParent constraint (canonical child) of
            Just (_, BindRigid) -> freeVars res (canonical child) visited'
            _ -> IntSet.singleton (getNodeId (canonical child))
