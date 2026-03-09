{-# LANGUAGE GADTs #-}
module MLF.Reify.Type (
    reifyType,
    reifyTypeWithNames,
    reifyTypeWithNamesNoFallback,
    reifyTypeWithNamesNoFallbackOnConstraint,
    reifyTypeWithNamedSet,
    reifyTypeWithNamedSetNoFallback,
    reifyWith,
    reifyWithAs,
    ReifyRoot(..),
    solvedFromView,
    freeVars
) where

import Control.Monad (foldM, unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import MLF.Binding.Tree (lookupBindParent)
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Finalize (presolutionViewFromSnapshot)
import MLF.Constraint.Presolution.View (PresolutionView(..))
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import qualified MLF.Constraint.Solved.Internal as SolvedInternal
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Constraint.Types hiding (lookupNode)
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Reify.Cache
import MLF.Reify.Named (namedNodes, softenedBindParentsUnder)
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError(..))
import MLF.Util.Graph (topoSortBy)
import qualified MLF.Util.Order as Order

data ReifyRoot
    = RootType
    | RootTypeNoFallback
    | RootBound

reifyWith
    :: String
    -> Solved
    -> (NodeId -> String)
    -> (NodeId -> Bool)
    -> ReifyRoot
    -> NodeId
    -> Either ElabError ElabType
reifyWith _contextLabel solved nameForVar isNamed rootMode nid =
    let start = case rootMode of
            RootType -> goType
            RootTypeNoFallback -> goTypeNoFallback
            RootBound -> goBoundRoot
    in snd <$> start emptyCache (canonical nid)
  where
    originalConstraint = Solved.originalConstraint solved
    canonicalConstraint = Solved.canonicalConstraint solved
    nodes = cNodes canonicalConstraint
    canonical = Solved.canonical solved
    lookupVarBoundS queryNid = VarStore.lookupVarBound originalConstraint (canonical queryNid)
    originalGenNodes = cGenNodes originalConstraint
    weakened = cWeakenedVars canonicalConstraint
    isEliminatedVarS queryNid = IntSet.member (getNodeId queryNid) (cEliminatedVars canonicalConstraint)
    canonicalGenNodesList =
        map snd (IntMap.toList (getGenNodeMap originalGenNodes))
    schemeRootSetRaw =
        IntSet.fromList
            [ getNodeId root
            | gen <- canonicalGenNodesList
            , root <- gnSchemes gen
            ]
    schemeRootSet =
        IntSet.union schemeRootSetRaw $
            IntSet.fromList
                [ getNodeId (canonical root)
                | gen <- canonicalGenNodesList
                , root <- gnSchemes gen
                ]
    schemeGenByRootRaw =
        IntMap.fromListWith
            const
            [ (getNodeId root, gnId gen)
            | gen <- canonicalGenNodesList
            , root <- gnSchemes gen
            ]
    schemeGenByRoot =
        IntMap.union schemeGenByRootRaw $
            IntMap.fromListWith
                const
                [ (getNodeId (canonical root), gnId gen)
                | gen <- canonicalGenNodesList
                , root <- gnSchemes gen
                ]
    schemeGenSet =
        IntSet.fromList
            [ getGenNodeId gid
            | gid <- IntMap.elems schemeGenByRoot
            ]

    lookupNode k = maybe (Left (MissingNode k)) Right (lookupNodeIn nodes k)

    bindParentsE =
        softenedBindParentsUnder canonical canonicalConstraint

    boundIsSimple start =
        let go visited nid0 =
                    let nidC = canonical nid0
                        key = getNodeId nidC
                    in if IntSet.member key visited
                        then True
                        else
                            case lookupNodeIn nodes nidC of
                                Nothing -> True
                                Just node ->
                                    let visited' = IntSet.insert key visited
                                    in case node of
                                        TyBase{} -> True
                                        TyBottom{} -> True
                                        TyCon{ tnArgs = args } ->
                                            all (go visited') (NE.toList args)
                                        TyVar{} ->
                                            case lookupVarBoundS nidC of
                                                Nothing -> True
                                                Just bnd -> go visited' bnd
                                        TyExp{ tnBody = b } -> go visited' b
                                        TyArrow{} -> False
                                        TyForall{} -> False
            in go IntSet.empty start

    boundIsSimpleFor n =
        case lookupVarBoundS (canonical n) of
            Nothing -> False
            Just bnd -> boundIsSimple bnd

    canonicalRef = Canonicalize.canonicalRef canonical

    nodeRefExists ref = case ref of
        TypeRef nid0 ->
            case lookupNodeIn nodes nid0 of
                Just _ -> True
                Nothing -> False
        GenRef gid ->
            IntMap.member (getGenNodeId gid) (getGenNodeMap (originalGenNodes))

    lookupBindParentUnderSoft ref0 = do
        bindParents <- bindParentsE
        let refC = canonicalRef ref0
        unless (nodeRefExists refC) $
            Left $
                BindingTreeError $
                    InvalidBindingTree $
                        "lookupBindParentUnderSoft: node " ++ show refC ++ " not in constraint"
        pure (IntMap.lookup (nodeRefKey refC) bindParents)

    boundFlexChildrenAllUnderSoft binder0 = do
        bindParents <- bindParentsE
        let binderC = canonicalRef binder0
        unless (nodeRefExists binderC) $
            Left $
                BindingTreeError $
                    InvalidBindingTree $
                        "boundFlexChildrenAllUnderSoft: binder " ++ show binderC ++ " not in constraint"
        reverse <$> foldM
            (\acc (childKey, (parent, flag)) ->
                if parent /= binderC || flag /= BindFlex
                    then pure acc
                    else
                        let childRef = nodeRefFromKey childKey
                        in case childRef of
                            TypeRef childN ->
                                case lookupNodeIn nodes childN of
                                    Just TyExp{} -> pure acc
                                    Just TyBase{} -> pure acc
                                    Just TyBottom{} -> pure acc
                                    Just _ -> pure (childN : acc)
                                    Nothing ->
                                        Left $
                                            BindingTreeError $
                                                InvalidBindingTree $
                                                    "boundFlexChildrenAllUnderSoft: child " ++ show childN ++ " not in cNodes"
                            GenRef gid ->
                                if IntMap.member (getGenNodeId gid) (getGenNodeMap (originalGenNodes))
                                    then pure acc
                                    else
                                        Left $
                                            BindingTreeError $
                                                InvalidBindingTree $
                                                    "boundFlexChildrenAllUnderSoft: child " ++ show gid ++ " not in cGenNodes"
            )
            []
            (IntMap.toList bindParents)

    varName n = nameForVar (canonical n)
    varFor n = TVar (varName n)

    isNamedLocal namedExtra nodeId =
        let key = getNodeId (canonical nodeId)
        in isNamed nodeId || IntSet.member key namedExtra

    cacheLookupLocal mode cache key namedExtra =
        if IntSet.null namedExtra
            then cacheLookup mode cache key
            else Nothing

    cacheInsertLocal mode key ty cache namedExtra =
        if IntSet.null namedExtra
            then cacheInsert mode key ty cache
            else cache

    goType cache = goFull cache IntSet.empty ModeType
    goTypeNoFallback cache = goFull cache IntSet.empty ModeTypeNoFallback
    goBoundRoot cache = goBound cache IntSet.empty

    goFull cache namedExtra mode n0 =
        let n = canonical n0
            key = getNodeId n
            inProgress = cacheInProgress cache
            markDone cache' = cache' { cacheInProgress = IntSet.delete key (cacheInProgress cache') }
            markStart cache' = cache' { cacheInProgress = IntSet.insert key (cacheInProgress cache') }
        in case cacheLookupLocal mode cache key namedExtra of
            Just t -> Right (cache, t)
            Nothing ->
                if IntSet.member key inProgress
                    then Right (cache, varFor n)
                    else do
                        node <- lookupNode n
                        let cache0 = markStart cache
                        case node of
                            TyVar{} ->
                                let cache0' = cacheInsertLocal mode key (varFor n) cache0 namedExtra
                                in if isEliminatedVarS n
                                    then
                                        let t = TBottom
                                            cache' = cacheInsertLocal mode key t cache0' namedExtra
                                        in pure (markDone cache', t)
                                    else case mode of
                                        ModeBound
                                            | IntSet.member key schemeRootSet ->
                                                case lookupVarBoundS n of
                                                    Nothing -> do
                                                        let t = varFor n
                                                            cache' = cacheInsertLocal mode key t cache0' namedExtra
                                                        pure (markDone cache', t)
                                                    Just bnd -> do
                                                        let bndC = canonical bnd
                                                        (cache1, core) <- goFull cache0' namedExtra ModeBound bndC
                                                        binders <- orderedFlexChildren ModeBound namedExtra n
                                                        (cache2, t) <- wrapBinders cache1 namedExtra core binders
                                                        let cacheFinal = cacheInsertLocal mode key t cache2 namedExtra
                                                        pure (markDone cacheFinal, t)
                                        ModeBound -> do
                                            let t = varFor n
                                                cache' = cacheInsertLocal mode key t cache0' namedExtra
                                            pure (markDone cache', t)
                                        _ -> do
                                            mbParent <- lookupBindParentUnderSoft (typeRef n)
                                            let mbSchemeBound =
                                                    case lookupVarBoundS (canonical n) of
                                                        Just bnd
                                                            | IntSet.member (getNodeId (canonical bnd)) schemeRootSet ->
                                                                Just (canonical bnd)
                                                        _ -> Nothing
                                                boundIsBaseOrBottom =
                                                    case lookupVarBoundS n of
                                                        Nothing -> False
                                                        Just bnd ->
                                                            case lookupNodeIn nodes (canonical bnd) of
                                                                Just TyBase{} -> True
                                                                Just TyBottom{} -> True
                                                                _ -> False
                                                boundIsSchemeRootVar =
                                                    case lookupVarBoundS (canonical n) of
                                                        Just bnd ->
                                                            IntSet.member (getNodeId (canonical bnd)) schemeRootSet
                                                        Nothing -> False
                                                shouldInlineWeakened =
                                                    IntSet.member key weakened
                                                        && boundIsBaseOrBottom
                                                        && not (isNamedLocal namedExtra (canonical n))
                                            isGenBinder <- case mbParent of
                                                Just (GenRef gid, BindFlex) ->
                                                    elem (canonical n) <$> boundFlexChildrenAllUnderSoft (genRef gid)
                                                _ -> pure False
                                            let shouldInline =
                                                    shouldInlineWeakened ||
                                                    case (mode, mbParent) of
                                                        (ModeTypeNoFallback, _)
                                                            | boundIsSchemeRootVar
                                                                && not (isNamedLocal namedExtra (canonical n)) ->
                                                                True
                                                        (ModeTypeNoFallback, Just (GenRef _, BindFlex))
                                                            | not (isNamedLocal namedExtra (canonical n))
                                                                && not isGenBinder
                                                                && boundIsSimpleFor n ->
                                                                True
                                                        (ModeTypeNoFallback, Nothing)
                                                            | not (isNamedLocal namedExtra (canonical n)) ->
                                                                boundIsSimpleFor n
                                                        _ -> False
                                            case mbSchemeBound of
                                                Just _ | not (isNamedLocal namedExtra (canonical n)) -> do
                                                    (cache', t) <- goBound cache0' namedExtra n
                                                    pure (markDone cache', t)
                                                _ -> case mbParent of
                                                    Just (TypeRef parent, BindRigid) ->
                                                        case lookupNodeIn nodes (canonical parent) of
                                                            Just TyForall{} ->
                                                                let t = varFor n
                                                                    cache' = cacheInsertLocal mode key t cache0' namedExtra
                                                                in pure (markDone cache', t)
                                                            _ ->
                                                                if isNamedLocal namedExtra (canonical n) || boundIsPoly n
                                                                    then
                                                                        let t = varFor n
                                                                            cache' = cacheInsertLocal mode key t cache0' namedExtra
                                                                        in pure (markDone cache', t)
                                                                    else do
                                                                        (cache', t) <- goBound cache0' namedExtra n
                                                                        pure (markDone cache', t)
                                                    Just (GenRef gid, BindRigid) ->
                                                        let isSchemeBinder =
                                                                IntSet.member (getGenNodeId gid) schemeGenSet
                                                        in if isNamedLocal namedExtra (canonical n)
                                                            || boundIsPoly n
                                                            || isSchemeBinder
                                                            then
                                                                let t = varFor n
                                                                    cache' = cacheInsertLocal mode key t cache0' namedExtra
                                                                in pure (markDone cache', t)
                                                            else do
                                                                (cache', t) <- goBound cache0' namedExtra n
                                                                pure (markDone cache', t)
                                                    _ ->
                                                        if shouldInline
                                                            then do
                                                                (cache', t) <- goBound cache0' namedExtra n
                                                                pure (markDone cache', t)
                                                            else
                                                                let t = varFor n
                                                                    cache' = cacheInsertLocal mode key t cache0' namedExtra
                                                                in pure (markDone cache', t)
                            _
                                | isNamedLocal namedExtra (canonical n) ->
                                    let t = varFor n
                                        cache' = cacheInsertLocal mode key t cache0 namedExtra
                                    in pure (markDone cache', t)
                                | otherwise -> do
                                    binders <- orderedFlexChildren mode namedExtra n
                                    let binderKeys =
                                            IntSet.fromList
                                                [ getNodeId (canonical b)
                                                | b <- binders
                                                ]
                                        namedExtra' = IntSet.union namedExtra binderKeys
                                    (cache', core) <- case node of
                                        TyBase{ tnBase = b } -> pure (cache0, TBase b)
                                        TyBottom{} -> pure (cache0, TBottom)
                                        TyArrow{ tnDom = d, tnCod = c } -> do
                                            (cache1, d') <- vChild cache0 namedExtra' mode (canonical d)
                                            (cache2, c') <- vChild cache1 namedExtra' mode (canonical c)
                                            pure (cache2, TArrow d' c')
                                        TyCon{ tnCon = con, tnArgs = args } -> do
                                            (cache', args') <- foldM
                                                (\(cacheAcc, acc) arg -> do
                                                    (cacheNext, arg') <- vChild cacheAcc namedExtra' mode (canonical arg)
                                                    pure (cacheNext, arg' : acc))
                                                (cache0, [])
                                                (NE.toList args)
                                            pure (cache', TCon con (NE.fromList (reverse args')))
                                        TyForall{ tnBody = b } ->
                                            let bodyC = canonical b
                                            in vChild cache0 namedExtra' mode bodyC
                                        TyExp{ tnBody = b } ->
                                            goFull cache0 namedExtra' mode (canonical b)
                                    (cache'', t) <- wrapBinders cache' namedExtra' core binders
                                    let cacheFinal = cacheInsertLocal mode key t cache'' namedExtra
                                    pure (markDone cacheFinal, t)

    goBound cache namedExtra n = do
        node <- lookupNode n
        case node of
            TyVar{} ->
                if isEliminatedVarS n
                    then pure (cache, TBottom)
                    else case lookupVarBoundS n of
                        Nothing ->
                            if isNamedLocal namedExtra n
                                then pure (cache, varFor n)
                                else pure (cache, TBottom)
                        Just bnd ->
                            let bndC = canonical bnd
                            in if isNamedLocal namedExtra bndC
                                then pure (cache, varFor bndC)
                                else if bndC == n
                                then pure (cache, TBottom)
                                else do
                                    mbParent <- lookupBindParentUnderSoft (typeRef n)
                                    let isRigid =
                                            case mbParent of
                                                Just (_, BindRigid) -> True
                                                _ -> False
                                    mbBoundParent <- lookupBindParentUnderSoft (typeRef bndC)
                                    let bndRoot =
                                            case mbBoundParent of
                                                Just (TypeRef parent, _) ->
                                                    case lookupNodeIn nodes (canonical parent) of
                                                        Just TyForall{} -> canonical parent
                                                        _ -> bndC
                                                _ -> bndC
                                    if isRigid
                                        then goFull cache namedExtra ModeBound bndRoot
                                        else do
                                            goFull cache namedExtra ModeBound bndRoot
            _ -> goFull cache namedExtra ModeBound n

    boundIsPoly n =
        case lookupVarBoundS (canonical n) of
            Nothing -> False
            Just bnd -> boundHasForall IntSet.empty bnd
      where
        boundHasForall visited nid0 =
            let nidC = canonical nid0
                key = getNodeId nidC
            in if IntSet.member key visited
                then False
                else
                    case lookupNodeIn nodes nidC of
                        Just TyForall{} -> True
                        Just TyVar{} ->
                            case lookupVarBoundS nidC of
                                Just bnd' | canonical bnd' /= nidC ->
                                    boundHasForall (IntSet.insert key visited) bnd'
                                _ -> False
                        Just TyExp{ tnBody = b } ->
                            boundHasForall (IntSet.insert key visited) b
                        _ -> False

    vChild cache namedExtra mode child = do
        let childC = canonical child
            childKey = getNodeId childC
            boundIsBaseOrBottom =
                case lookupVarBoundS childC of
                    Nothing -> False
                    Just bnd ->
                        case lookupNodeIn nodes (canonical bnd) of
                            Just TyBase{} -> True
                            Just TyBottom{} -> True
                            _ -> False
            inlineWeakened =
                IntSet.member childKey weakened
                    && boundIsBaseOrBottom
                    && not (isNamedLocal namedExtra childC)
        mbParent <- lookupBindParentUnderSoft (typeRef child)
        case mbParent of
            Just (TypeRef parent, BindRigid) ->
                if boundIsPoly child || isNamedLocal namedExtra (canonical child)
                    then pure (cache, varFor child)
                    else
                        case (mode, lookupNodeIn nodes parent) of
                            (ModeTypeNoFallback, Just TyForall{}) -> goFull cache namedExtra mode child
                            _ -> goBound cache namedExtra child
            Just (GenRef _, BindRigid) ->
                if boundIsPoly child || isNamedLocal namedExtra (canonical child)
                    then pure (cache, varFor child)
                    else goBound cache namedExtra child
            Just (_, BindFlex) ->
                case mode of
                    ModeBound ->
                        case lookupNodeIn nodes (canonical child) of
                            Just TyVar{} | isEliminatedVarS (canonical child) ->
                                goBound cache namedExtra child
                            Just TyVar{} -> pure (cache, varFor child)
                            _ -> goFull cache namedExtra mode child
                    _ | inlineWeakened ->
                        goBound cache namedExtra child
                    _ ->
                        case lookupNodeIn nodes (canonical child) of
                            Just TyVar{} ->
                                let childKey' = getNodeId (canonical child)
                                    isBoundHere = IntSet.member childKey' namedExtra
                                in case lookupVarBoundS (canonical child) of
                                    Just bnd
                                        | IntSet.member (getNodeId (canonical bnd)) schemeRootSet
                                        , not isBoundHere
                                        , not (isNamedLocal namedExtra (canonical child)) ->
                                            goFull cache namedExtra mode (canonical bnd)
                                    _ -> pure (cache, varFor child)
                            _ -> goFull cache namedExtra mode child
            Nothing -> goBound cache namedExtra child

    wrapBinders cache namedExtra inner binders =
        foldrM
            (\b (cacheAcc, acc) -> do
                (cache', boundTy) <- goBound cacheAcc namedExtra b
                let selfBound =
                        case boundTy of
                            TVar v -> v == varName b
                            _ -> False
                    mbBound =
                        case boundTy of
                            TBottom -> Nothing
                            TVar{} -> Nothing
                            _ | selfBound -> Nothing
                              | otherwise -> either (const Nothing) Just (elabToBound boundTy)
                pure (cache', TForall (varName b) mbBound acc)
            )
            (cache, inner)
            binders

    orderedFlexChildren mode namedExtra n0 = do
        let n = canonical n0
        node <- lookupNode n
        let orderRoot =
                case node of
                    TyForall{ tnBody = body } -> canonical body
                    _ -> n
            reachable =
                Traversal.reachableFromWithBounds
                    canonical
                    (lookupNodeIn nodes)
                    orderRoot
        let includeRigid =
                isForall node
                    || mode == ModeBound
                    || IntSet.member (getNodeId n) schemeRootSet
        let schemeOwner =
                if IntSet.member (getNodeId n) schemeRootSet
                    then IntMap.lookup (getNodeId n) schemeGenByRoot
                    else Nothing
        let parentRefForBinders =
                case node of
                    TyForall{} -> typeRef n
                    _ ->
                        case schemeOwner of
                            Just gid -> genRef gid
                            Nothing -> typeRef n
        bindersBase <- directFlexChildren (mode == ModeTypeNoFallback || mode == ModeBound) includeRigid parentRefForBinders
        let keepNamedForScheme =
                mode == ModeBound && IntSet.member (getNodeId n) schemeRootSet
        let isBinderNode candidate =
                case lookupNodeIn nodes (canonical candidate) of
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
                    ModeBound ->
                        let base = filter (/= n) bindersReachable0
                        in if keepNamedForScheme
                            then base
                            else filter (not . isNamedLocal namedExtra . canonical) base
                    ModeTypeNoFallback ->
                        filter (not . isNamedLocal namedExtra . canonical) bindersReachable0
                    _ -> bindersReachable0
            binderKeys = map (getNodeId . canonical) bindersReachable
            binderSet = IntSet.fromList binderKeys
            orderKeys = Order.orderKeysFromRoot solved orderRoot
            missing =
                [ NodeId k
                | k <- binderKeys
                , not (IntMap.member k orderKeys)
                ]
            depsFor k =
                [ d
                | d <- IntSet.toList (freeVars solved (NodeId k) IntSet.empty)
                , IntSet.member d binderSet
                , d /= k
                ]
            cmpReady a b =
                case Order.compareNodesByOrderKey orderKeys (NodeId a) (NodeId b) of
                    Right EQ -> compare a b
                    Right other -> other
                    Left _ -> compare a b  -- fallback if missing key (validated above)
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
        directFlexChildren includeAll includeRigid parentRef =
            if not includeAll
                then boundFlexChildrenAllUnderSoft parentRef
                else do
                    bindParents <- bindParentsE
                    let childNode childKey =
                            case nodeRefFromKey childKey of
                                TypeRef childN -> Just childN
                                GenRef _ -> Nothing
                        isBindableNode child =
                            case lookupNodeIn nodes child of
                                Just TyVar{} -> True
                                _ -> False
                        isBindable flag child = case flag of
                            BindFlex -> isBindableNode child
                            BindRigid -> includeRigid && isBindableNode child
                    pure
                        [ canonical child
                        | (childKey, (parent, flag)) <- IntMap.toList bindParents
                        , parent == parentRef
                        , Just child <- [childNode childKey]
                        , isBindable flag child
                        ]

    isForall :: TyNode -> Bool
    isForall TyForall{} = True
    isForall _ = False

    foldrM :: (a -> b -> Either ElabError b) -> b -> [a] -> Either ElabError b
    foldrM _ z [] = Right z
    foldrM f z (x:xs) = do
        z' <- foldrM f z xs
        f x z'

reifyWithAs
    :: String
    -> Solved
    -> (NodeId -> String)
    -> (NodeId -> Bool)
    -> ReifyRoot
    -> (ElabType -> Either ElabError a)
    -> NodeId
    -> Either ElabError a
reifyWithAs contextLabel solved nameForVar isNamed rootMode convert nid =
    convert =<< reifyWith contextLabel solved nameForVar isNamed rootMode nid

-- | Reify a solved NodeId into an elaborated type.
-- This version doesn't compute instance bounds (all foralls are unbounded).
reifyType :: PresolutionView -> NodeId -> Either ElabError ElabType
reifyType presolutionView =
    let solved = solvedFromView presolutionView
    in reifyWith "reifyType" solved nameFor (const False) RootType
  where
    nameFor (NodeId i) = "t" ++ show i

-- | Reify with an explicit name substitution for vars (Schi': named nodes become variables).
reifyTypeWithNames :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNames presolutionView subst nid = do
    namedSet <- namedNodes presolutionView
    reifyTypeWithNamedSet presolutionView subst namedSet nid

-- | Reify with an explicit name substitution, but without ancestor fallback
-- quantifiers (used when an outer scheme already quantifies binders).
-- See Note [No-fallback reify preserves explicit bounds] in
-- docs/notes/2026-01-27-elab-changes.md.
reifyTypeWithNamesNoFallback :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallback presolutionView subst nid =
    reifyTypeWithNamesNoFallbackSolved (solvedFromView presolutionView) subst nid

reifyTypeWithNamesNoFallbackSolved
    :: Solved
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError ElabType
reifyTypeWithNamesNoFallbackSolved solved subst nid =
    let canonical = Solved.canonical solved
        nameFor (NodeId i) = "t" ++ show i

        varNameFor v =
            let cv = canonical v
            in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

        isNamed nodeId =
            let key = getNodeId (canonical nodeId)
            in IntMap.member key subst
    in reifyWith "reifyTypeWithNamesNoFallback" solved varNameFor isNamed RootTypeNoFallback nid

-- | Reify with an explicit constraint (Schi' on base graphs).
reifyTypeWithNamesNoFallbackOnConstraint :: Constraint -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallbackOnConstraint constraint subst nid =
    let presolutionView = presolutionViewFromSnapshot constraint IntMap.empty
    in reifyTypeWithNamesNoFallback presolutionView subst nid

-- | Reify with an explicit named-node set (Schi').
reifyTypeWithNamedSet :: PresolutionView -> IntMap.IntMap String -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
reifyTypeWithNamedSet presolutionView subst namedSet =
    reifyTypeWithNamedSetSolved (solvedFromView presolutionView) subst namedSet

reifyTypeWithNamedSetSolved
    :: Solved
    -> IntMap.IntMap String
    -> IntSet.IntSet
    -> NodeId
    -> Either ElabError ElabType
reifyTypeWithNamedSetSolved solved subst namedSet =
    reifyWith "reifyTypeWithNames" solved varNameFor isNamed RootType
  where
    canonical = Solved.canonical solved

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

-- | Reify with an explicit named-node set, without ancestor fallback quantifiers.

reifyTypeWithNamedSetNoFallbackSolved
    :: Solved
    -> IntMap.IntMap String
    -> IntSet.IntSet
    -> NodeId
    -> Either ElabError ElabType
reifyTypeWithNamedSetNoFallbackSolved solved subst namedSet =
    reifyWith "reifyTypeWithNamedSetNoFallback" solved varNameFor isNamed RootTypeNoFallback
  where
    canonical = Solved.canonical solved

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

reifyTypeWithNamedSetNoFallback
    :: PresolutionView
    -> IntMap.IntMap String
    -> IntSet.IntSet
    -> NodeId
    -> Either ElabError ElabType
reifyTypeWithNamedSetNoFallback presolutionView subst namedSet nid =
    reifyTypeWithNamedSetNoFallbackSolved (solvedFromView presolutionView) subst namedSet nid

solvedFromView :: PresolutionView -> Solved
solvedFromView presolutionView =
    let solved0 =
            SolvedInternal.fromConstraintAndUf
                (pvConstraint presolutionView)
                (pvCanonicalMap presolutionView)
    in SolvedInternal.rebuildWithConstraint solved0 (pvCanonicalConstraint presolutionView)

-- | Collect free variables by NodeId, skipping vars under TyForall.
freeVars :: Solved -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars solved nid visited
    | IntSet.member key visited = IntSet.empty
    | otherwise =
        let visited' = IntSet.insert key visited
        in case lookupNodeIn nodes (canonical nid) of
            Nothing -> IntSet.empty
            Just TyVar{} ->
                case VarStore.lookupVarBound constraint (canonical nid) of
                    Nothing -> IntSet.empty
                    Just bnd -> freeVars solved (canonical bnd) visited'
            Just TyBase{} -> IntSet.empty
            Just TyBottom{} -> IntSet.empty
            Just TyArrow{ tnDom = d, tnCod = c } ->
                freeVarsChild visited' d `IntSet.union`
                freeVarsChild visited' c
            Just TyCon{ tnArgs = args } ->
                IntSet.unions (map (freeVarsChild visited') (NE.toList args))
            Just TyForall{ tnBody = b } ->
                freeVarsChild visited' b
            Just TyExp{ tnBody = b } ->
                freeVars solved (canonical b) visited'
  where
    constraint = Solved.originalConstraint solved
    nodes = cNodes constraint
    canonical = Solved.canonical solved
    key = getNodeId (canonical nid)

    freeVarsChild visited' child =
        case lookupBindParent constraint (typeRef (canonical child)) of
            Just (_, BindRigid) -> freeVars solved (canonical child) visited'
            _ -> IntSet.singleton (getNodeId (canonical child))
