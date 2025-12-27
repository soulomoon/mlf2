module MLF.Elab.Reify (
    reifyType,
    reifyTypeWithNames,
    freeVars
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Util (topoSortBy)
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Binding.Tree (orderedBinders)
import qualified MLF.Constraint.VarStore as VarStore

reifyWith
    :: String
    -> SolveResult
    -> (NodeId -> String)
    -> (NodeId -> String)
    -> (NodeId -> NodeId -> Either ElabError [(NodeId, Maybe NodeId)])
    -> NodeId
    -> Either ElabError ElabType
reifyWith contextLabel res nameForVar nameForDummy bindersFor nid =
    snd <$> go IntMap.empty (canonical nid)
  where
    constraint = srConstraint res
    nodes = cNodes constraint
    uf = srUnionFind res
    canonical = Solve.frWith uf

    lookupNode k = maybe (Left (MissingNode k)) Right (IntMap.lookup (getNodeId k) nodes)

    go cache n = case IntMap.lookup (getNodeId n) cache of
        Just t -> Right (cache, t)
        Nothing -> do
            node <- lookupNode n
            (cache', t) <- case node of
                TyVar{} -> pure (cache, TVar (nameForVar n))
                TyBase{ tnBase = b } -> pure (cache, TBase b)
                TyArrow{ tnDom = d, tnCod = c } -> do
                    (cache1, d') <- go cache (canonical d)
                    (cache2, c') <- go cache1 (canonical c)
                    pure (cache2, TArrow d' c')
                TyForall{ tnBody = b } -> do
                    (cache1, body) <- go cache (canonical b)
                    binders <- bindersFor n b
                    t' <- case binders of
                        [] -> pure (TForall (nameForDummy n) Nothing body)
                        bs -> foldrM (wrapOne cache1) body bs
                    pure (cache1, t')
                TyExp{ tnBody = b } -> do
                    go cache (canonical b)
                TyRoot{} ->
                    Left $
                        InstantiationError $
                            contextLabel ++ ": unexpected TyRoot at " ++ show (getNodeId n)
            let cache'' = IntMap.insert (getNodeId n) t cache'
            pure (cache'', t)

    wrapOne cache1 (v, mBound) inner = do
        let vName = nameForVar v
        mbBoundTy <- case mBound of
            Nothing -> pure Nothing
            Just bn -> Just . snd <$> go cache1 (canonical bn)
        pure (TForall vName mbBoundTy inner)

    foldrM :: (a -> b -> Either ElabError b) -> b -> [a] -> Either ElabError b
    foldrM _ z [] = Right z
    foldrM f z (x:xs) = do
        z' <- foldrM f z xs
        f x z'

-- | Reify a solved NodeId into an elaborated type.
-- This version doesn't compute instance bounds (all foralls are unbounded).
reifyType :: SolveResult -> NodeId -> Either ElabError ElabType
reifyType res nid =
    reifyWith "reifyType" res nameFor nameFor bindersFor nid
  where
    constraint = srConstraint res
    uf = srUnionFind res
    canonical = Solve.frWith uf

    nameFor (NodeId i) = "t" ++ show i

    bindersFor forallId _bodyId = do
        binders0 <- bindingToElab (orderedBinders canonical constraint forallId)
        let binders =
                [ v
                | v <- binders0
                , not (VarStore.isEliminatedVar constraint v || VarStore.isEliminatedVar constraint (canonical v))
                ]
        pure [ (v, Nothing) | v <- binders ]

-- | Reify with an explicit name substitution for vars.
reifyTypeWithNames :: SolveResult -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNames res subst nid =
    reifyWith "reifyTypeWithNames" res varNameFor nameFor bindersFor nid
  where
    constraint = srConstraint res
    uf = srUnionFind res
    canonical = Solve.frWith uf

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in maybe (nameFor cv) id (IntMap.lookup (getNodeId cv) subst)

    bindersFor forallId bodyId = do
        -- Reify the quantifier(s) at this level.
        --
        -- Paper alignment: binders are the (reachable) flexibly bound TyVar
        -- children of the forall node (Q(n)).
        binders0 <- bindingToElab (orderedBinders canonical constraint forallId)
        let usedBinders =
                [ (v, fmap canonical (VarStore.lookupVarBound constraint v))
                | v <- binders0
                , not (VarStore.isEliminatedVar constraint v)
                ]
        orderUsedBinders (canonical bodyId) usedBinders

    orderUsedBinders :: NodeId -> [(NodeId, Maybe NodeId)] -> Either ElabError [(NodeId, Maybe NodeId)]
    orderUsedBinders bodyRoot bs0 = do
        let keys = Order.orderKeysFromRoot res bodyRoot
            canonBinder (v, mb) = (canonical v, fmap canonical mb)
            bs = map canonBinder bs0
            binderKeys = [ getNodeId v | (v, _) <- bs ]
            binderSet = IntSet.fromList binderKeys
            pairsByKey = IntMap.fromList [ (getNodeId v, (v, mb)) | (v, mb) <- bs ]

            depsFor :: (NodeId, Maybe NodeId) -> [Int]
            depsFor (v, mb) =
                case mb of
                    Nothing -> []
                    Just bnd ->
                        [ d
                        | d <- IntSet.toList (freeVars res bnd IntSet.empty)
                        , IntSet.member d binderSet
                        , d /= getNodeId v
                        ]

            depsForKey :: Int -> [Int]
            depsForKey k =
                case IntMap.lookup k pairsByKey of
                    Nothing -> []
                    Just pair -> depsFor pair

            cmpReady a b =
                case Order.compareNodesByOrderKey keys (NodeId a) (NodeId b) of
                    EQ -> compare a b
                    other -> other

        orderedKeys <-
            topoSortBy
                "reifyTypeWithNames: cycle in binder bound dependencies"
                cmpReady
                depsForKey
                binderKeys
        pure [ p | k <- orderedKeys, Just p <- [IntMap.lookup k pairsByKey] ]

-- | Collect free variables by NodeId, skipping vars under TyForall.
freeVars :: SolveResult -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars res nid visited
    | IntSet.member key visited = IntSet.empty
    | otherwise =
        let visited' = IntSet.insert key visited
        in case IntMap.lookup key nodes of
            Nothing -> IntSet.empty
            Just TyVar{} -> IntSet.singleton key
            Just TyBase{} -> IntSet.empty
            Just TyArrow{ tnDom = d, tnCod = c } ->
                freeVars res (canonical d) visited' `IntSet.union`
                freeVars res (canonical c) visited'
            Just TyForall{ tnBody = b } -> freeVars res (canonical b) visited'
            Just TyExp{ tnBody = b } -> freeVars res (canonical b) visited'
            Just TyRoot{ tnChildren = cs } ->
                IntSet.unions (map (\child -> freeVars res (canonical child) visited') cs)
  where
    nodes = cNodes (srConstraint res)
    uf = srUnionFind res
    canonical = Solve.frWith uf
    key = getNodeId (canonical nid)
