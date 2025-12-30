{-# LANGUAGE LambdaCase #-}
module MLF.Elab.Phi (
    contextToNodeBound,
    phiFromEdgeWitness,
    phiFromEdgeWitnessWithTrace
) where

import Control.Monad (unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.List (elemIndex, nub, sortBy)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Inst (applyInstantiation, composeInst, instMany, schemeToType, splitForalls)
import MLF.Elab.Reify (reifyType, reifyTypeWithNames)
import MLF.Elab.Sigma (bubbleReorderTo)
import MLF.Elab.Util (topoSortBy)
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Presolution (EdgeTrace(..))
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Binding.Tree (checkBindingTree, lookupBindParent)

-- | Compute an instantiation-context path from a root node to a target node.
--
-- Paper reference: @papers/xmlf.txt@ Figure 10 defines instantiation contexts:
--
--   C ::= {·} | ∀(⩾ C) | ∀(α ⩾) C
--
-- i.e. contexts navigate *only* through quantifiers (under binders) and their
-- bounds (inside-bounds). This helper computes the thesis computation context
-- by descending the type structure and choosing the leftmost binder-bound path
-- that contains @target@.
--
-- Returns 'Nothing' when @target@ is not transitively bound to @root@.
contextToNodeBound :: SolveResult -> NodeId -> NodeId -> Either ElabError (Maybe [ContextStep])
contextToNodeBound res root target = do
    let c = srConstraint res
        uf = srUnionFind res
        canonical = Solve.frWith uf
        rootC = canonical root
        targetC = canonical target

    if rootC == targetC
        then pure (Just [])
        else do
            let keys = Order.orderKeysFromRoot res rootC
            contextToNodeBoundWithOrderKeys canonical keys c rootC targetC

contextToNodeBoundWithOrderKeys
    :: (NodeId -> NodeId)
    -> IntMap.IntMap Order.OrderKey
    -> Constraint
    -> NodeId
    -> NodeId
    -> Either ElabError (Maybe [ContextStep])
contextToNodeBoundWithOrderKeys canonical keys c root target = do
    let rootC = canonical root
        targetC = canonical target

    if rootC == targetC
        then pure (Just [])
        else do
            let start =
                    case IntMap.lookup (getNodeId rootC) (cNodes c) of
                        Just TyVar{ tnBound = Just bnd } -> canonical bnd
                        Just TyVar{} -> rootC
                        _ -> rootC
            go IntSet.empty start
  where
    nameFor nid = "t" ++ show (getNodeId nid)

    dedupeById :: [NodeId] -> [NodeId]
    dedupeById =
        reverse . IntSet.foldl' (\acc i -> NodeId i : acc) [] . IntSet.fromList . map getNodeId

    orderedBindersAt :: NodeId -> Either ElabError [NodeId]
    orderedBindersAt binder0 = do
        let binder = canonical binder0
        binders0 <- bindingToElab (Binding.boundFlexChildrenAllUnder canonical c binder)
        let orderRoot =
                case IntMap.lookup (getNodeId binder) (cNodes c) of
                    Just TyForall{ tnBody = body } -> canonical body
                    _ -> binder
            reachable =
                Traversal.reachableFromUnderLenient
                    canonical
                    (\nid -> IntMap.lookup (getNodeId nid) (cNodes c))
                    orderRoot
            bindersReachable =
                filter (\nid -> IntSet.member (getNodeId nid) reachable) (dedupeById (map canonical binders0))
            missing =
                [ nid
                | nid <- bindersReachable
                , not (IntMap.member (getNodeId nid) keys)
                ]
        unless (null missing) $
            Left $
                InstantiationError $
                    "contextToNodeBound: missing order keys for " ++ show missing
        pure (sortBy (Order.compareNodesByOrderKey keys) bindersReachable)

    go :: IntSet.IntSet -> NodeId -> Either ElabError (Maybe [ContextStep])
    go visited nid0 = do
        let nid = canonical nid0
            key = getNodeId nid
            targetC = canonical target
        if nid == targetC
            then Right (Just [])
            else if IntSet.member key visited
                then
                    Left $
                        InstantiationError $
                            "contextToNodeBound: cycle detected at " ++ show nid
                else
                    case IntMap.lookup key (cNodes c) of
                        Nothing -> Left (MissingNode nid)
                        Just node ->
                            let visited' = IntSet.insert key visited
                            in case node of
                                TyForall{ tnBody = body } ->
                                    goForall visited' nid body
                                TyArrow{ tnDom = dom, tnCod = cod } ->
                                    goChildren visited' [dom, cod]
                                TyExp{ tnBody = body } ->
                                    go visited' body
                                TyRoot{ tnChildren = children } ->
                                    goChildren visited' children
                                TyVar{} ->
                                    Right Nothing
                                TyBase{} ->
                                    Right Nothing
                                TyBottom{} ->
                                    Right Nothing

    goChildren :: IntSet.IntSet -> [NodeId] -> Either ElabError (Maybe [ContextStep])
    goChildren _ [] = Right Nothing
    goChildren visited (child:rest) = do
        res <- go visited child
        case res of
            Just _ -> pure res
            Nothing -> goChildren visited rest

    goForall :: IntSet.IntSet -> NodeId -> NodeId -> Either ElabError (Maybe [ContextStep])
    goForall visited forallId body0 = do
        binders <- orderedBindersAt forallId
        let targetC = canonical target
        case elemIndex targetC binders of
            Just i -> do
                let before = take i binders
                    steps = map (StepUnder . nameFor) before
                pure (Just steps)
            Nothing -> do
                let tryBound [] = Right Nothing
                    tryBound (b : bs) =
                        case IntMap.lookup (getNodeId b) (cNodes c) of
                            Just TyVar{ tnBound = Just bnd } -> do
                                let bndC = canonical bnd
                                res <- go visited bndC
                                case res of
                                    Just ctx ->
                                        let before = takeWhile (/= b) binders
                                            steps = map (StepUnder . nameFor) before ++ [StepInside] ++ ctx
                                        in pure (Just steps)
                                    Nothing -> tryBound bs
                            _ -> tryBound bs
                boundRes <- tryBound binders
                case boundRes of
                    Just _ -> pure boundRes
                    Nothing -> do
                        res <- go visited (canonical body0)
                        case res of
                            Just ctx ->
                                let steps = map (StepUnder . nameFor) binders ++ ctx
                                in pure (Just steps)
                            Nothing -> pure Nothing

-- | Translate a recorded per-edge graph witness to an xMLF instantiation.
phiFromEdgeWitness :: SolveResult -> Maybe SchemeInfo -> EdgeWitness -> Either ElabError Instantiation
phiFromEdgeWitness res mSchemeInfo ew =
    phiFromEdgeWitnessWithTrace res mSchemeInfo Nothing ew

phiFromEdgeWitnessWithTrace :: SolveResult -> Maybe SchemeInfo -> Maybe EdgeTrace -> EdgeWitness -> Either ElabError Instantiation
phiFromEdgeWitnessWithTrace res mSchemeInfo mTrace ew = do
    requireValidBindingTree
    let InstanceWitness ops = ewWitness ew
        steps0 =
            case ewSteps ew of
                [] -> map StepOmega ops
                xs -> xs
    case mSchemeInfo of
        Nothing -> phiFromType steps0
        Just si -> phiWithScheme si steps0
  where
    requireValidBindingTree :: Either ElabError ()
    requireValidBindingTree =
        case checkBindingTree (srConstraint res) of
            Left err -> Left (BindingTreeError err)
            Right () -> Right ()

    canonicalNode :: NodeId -> NodeId
    canonicalNode = Solve.frWith (srUnionFind res)

    orderRoot :: NodeId
    -- Paper root `r` for Φ/Σ is the expansion root (TyExp body), not the TyExp
    -- wrapper itself. Using the wrapper can make ≺-keys missing for the scheme’s
    -- own binders (they are not structurally reachable), which breaks Raise(n)
    -- translation (Fig. 10).
    orderRoot = ewRoot ew

    orderKeys :: IntMap.IntMap Order.OrderKey
    orderKeys =
        case mTrace of
            Just tr | not (IntSet.null (etInterior tr)) ->
                Order.orderKeysFromRootRestricted res orderRoot (etInterior tr)
            _ ->
                Order.orderKeysFromRoot res orderRoot

    phiFromType :: [InstanceStep] -> Either ElabError Instantiation
    phiFromType steps = do
        ty0 <- reifyType res (ewRoot ew)
        let ids0 = idsFromType ty0
            lookupBinder nid = Just ("t" ++ show (getNodeId nid))
            omegaOps = [op | StepOmega op <- steps]
        (sigma, ty1, ids1) <-
            if needsPrec omegaOps
                then reorderBindersByPrec ty0 ids0
                else Right (InstId, ty0, ids0)
        (_, _, phiOps) <- goSteps ty1 ids1 InstId steps lookupBinder
        pure (instMany [sigma, phiOps])

    phiWithScheme :: SchemeInfo -> [InstanceStep] -> Either ElabError Instantiation
    phiWithScheme si steps = do
        let ty0 = schemeToType (siScheme si)
            subst = siSubst si
            lookupBinder (NodeId i) = IntMap.lookup i subst
            ids0 = idsForStartType si ty0
            omegaOps = [op | StepOmega op <- steps]
        (sigma, ty1, ids1) <-
            if needsPrec omegaOps
                then reorderBindersByPrec ty0 ids0
                else Right (InstId, ty0, ids0)
        (_, _, phiOps) <- goSteps ty1 ids1 InstId steps lookupBinder
        pure (instMany [sigma, phiOps])

    needsPrec :: [InstanceOp] -> Bool
    needsPrec = any $ \case
        OpRaise{} -> True
        _ -> False

    goSteps
        :: ElabType
        -> [Maybe NodeId]
        -> Instantiation
        -> [InstanceStep]
        -> (NodeId -> Maybe String)
        -> Either ElabError (ElabType, [Maybe NodeId], Instantiation)
    goSteps ty ids phi steps lookupBinder = case steps of
        [] -> Right (ty, ids, phi)
        StepIntro : rest -> do
            ty' <- applyInstantiation ty InstIntro
            let ids' = Nothing : ids
            goSteps ty' ids' (composeInst phi InstIntro) rest lookupBinder
        _ ->
            let (segment, rest) = span isOmega steps
                ops = [op | StepOmega op <- segment]
            in do
                (ty', ids', phi') <- go ty ids phi ops lookupBinder
                goSteps ty' ids' phi' rest lookupBinder
      where
        isOmega = \case
            StepOmega{} -> True
            _ -> False

    reorderBindersByPrec :: ElabType -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderBindersByPrec ty ids = do
        let (qs, _) = splitForalls ty
        if length qs /= length ids
            then Left (InstantiationError "reorderBindersByPrec: binder spine / identity list length mismatch")
            else if length qs < 2
                then Right (InstId, ty, ids)
                else do
                    let missing =
                            [ nid
                            | Just nid <- ids
                            , not (IntMap.member (getNodeId (canonicalNode nid)) orderKeys)
                            ]
                    unless (null missing) $
                        Left $
                            InstantiationError $
                                "reorderBindersByPrec: missing order keys for " ++ show missing
                    let knownKeyCount =
                            length
                                [ ()
                                | Just nid <- ids
                                , IntMap.member (getNodeId (canonicalNode nid)) orderKeys
                                ]
                    if knownKeyCount < 2
                        then Right (InstId, ty, ids)
                        else do
                            desired <- desiredBinderOrder ty ids
                            reorderTo ty ids desired

    desiredBinderOrder :: ElabType -> [Maybe NodeId] -> Either ElabError [Maybe NodeId]
    desiredBinderOrder ty ids = do
        let (qs, _) = splitForalls ty
            names = map fst qs
            bounds = map snd qs
            n = length qs
            nameIndex nm = elemIndex nm names

            -- Bound dependencies: if a occurs free in b's bound, then a must appear before b.
            depsFor :: Int -> [Int]
            depsFor i =
                case bounds !! i of
                    Nothing -> []
                    Just bnd ->
                        [ j
                        | v <- freeTypeVars bnd
                        , v /= names !! i
                        , Just j <- [nameIndex v]
                        ]

            cmpIdx :: Int -> Int -> Ordering
            cmpIdx i j =
                case (ids !! i, ids !! j) of
                    (Just a, Just b) ->
                        let ca = canonicalNode a
                            cb = canonicalNode b
                        in Order.compareNodesByOrderKey orderKeys ca cb
                    (Just _, Nothing) -> LT
                    (Nothing, Just _) -> GT
                    (Nothing, Nothing) -> compare i j
            indices = [0 .. n - 1]

        idxs <-
            topoSortBy
                "reorderBindersByPrec: cycle in bound dependencies"
                cmpIdx
                depsFor
                indices
        pure [ ids !! i | i <- idxs ]

    reorderTo :: ElabType -> [Maybe NodeId] -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderTo = bubbleReorderTo "reorderBindersByPrec"

    -- Interpret witness ops while tracking the current type.
    --
    -- Paper Fig. 10 uses instantiation contexts (C{·}) to reach a binder rather
    -- than swapping quantifiers. Using `InstUnder` keeps binder nesting intact,
    -- which matters for operations like Merge that reference outer binders.
    graftArgFor :: NodeId -> NodeId -> NodeId
    graftArgFor arg bv =
        case mTrace of
            Nothing -> arg
            Just tr ->
                case IntMap.lookup (getNodeId bv) (etCopyMap tr) of
                    Nothing -> arg
                    Just meta -> meta

    go :: ElabType -> [Maybe NodeId] -> Instantiation -> [InstanceOp] -> (NodeId -> Maybe String)
       -> Either ElabError (ElabType, [Maybe NodeId], Instantiation)
    go ty ids phi ops lookupBinder = case ops of
        [] -> Right (ty, ids, phi)

        (OpGraft arg bv : OpWeaken bv' : rest)
            | bv == bv' -> do
                (inst, ids1) <- atBinder ids ty bv $ do
                    argTy <- reifyType res (graftArgFor arg bv)
                    pure (InstApp argTy)
                ty' <- applyInstantiation ty inst
                go ty' ids1 (composeInst phi inst) rest lookupBinder
            | otherwise ->
                Left (InstantiationError "witness op mismatch: OpGraft/OpWeaken refer to different nodes")

        (OpGraft arg bv : rest) -> do
            (inst, ids1) <- atBinderKeep ids ty bv $ do
                argTy <- reifyType res arg
                pure (InstInside (InstBot argTy))
            ty' <- applyInstantiation ty inst
            go ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpWeaken bv : rest) -> do
            (inst, ids1) <- atBinder ids ty bv (pure InstElim)
            ty' <- applyInstantiation ty inst
            go ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaise n : rest) -> do
            -- Paper Fig. 10: operations on rigid nodes translate to the identity
            -- instantiation (they are inlined and not expressible as xMLF instantiation
            -- steps). Our witnesses can still contain such ops due to normalization and
            -- binding-tree harmonization, so treat them as no-ops here.
            case lookupBindParent (srConstraint res) (canonicalNode n) of
                Just (_, BindRigid) ->
                    go ty ids phi rest lookupBinder
                _ ->
                    -- Paper Fig. 10: Raise(n) introduces a fresh quantifier one level higher,
                    -- bounds it by Tξ(n), then aliases/eliminates the old binder.
                    --
                    -- For spine binders: use the existing logic
                    -- For non-spine nodes: use binding edges + ≺ ordering to compute context
                    case elemIndex (Just n) ids of
                        Just i -> do
                            -- Spine binder case: existing logic
                            let (qs, _) = splitForalls ty
                            when (length qs /= length ids) $
                                Left (InstantiationError "OpRaise: binder spine / identity list length mismatch")
                            when (i < 0 || i >= length qs) $
                                Left (InstantiationError "OpRaise: binder index out of range")

                            let names = map fst qs
                                mbBound = snd (qs !! i)
                                boundTy = maybe TBottom id mbBound
                                boundName = names !! i

                                deps = filter (/= boundName) (freeTypeVars boundTy)
                                depIdxs = mapMaybe (`elemIndex` names) deps
                                cutoff = if null depIdxs then (-1) else maximum depIdxs
                                insertIndex = cutoff + 1

                            when (insertIndex > i) $
                                Left (InstantiationError "OpRaise: computed insertion point is after binder")

                            let prefixBefore = take insertIndex names
                                between = take (i - insertIndex) (drop insertIndex names)
                                hAbsBeta = InstSeq (InstInside (InstAbstr "β")) InstElim
                                aliasOld = underContext between hAbsBeta

                                local =
                                    instMany
                                        [ InstIntro
                                        , InstInside (InstBot boundTy)
                                        , InstUnder "β" aliasOld
                                        ]

                                inst = underContext prefixBefore local
                            ty' <- applyInstantiation ty inst

                            idsNoN <- deleteAt i ids
                            ids1 <- insertAt insertIndex (Just n) idsNoN
                            go ty' ids1 (composeInst phi inst) rest lookupBinder

                        Nothing -> do
                            -- Non-spine node case: select an insertion point `m = min≺{...}` (Fig. 10)
                            -- using the edge-local ≺ ordering, then insert a fresh quantifier bounded
                            -- by `Tξ(n)` at that point, and then alias/eliminate the original
                            -- (nested) binder for `n` inside the chosen `m`'s bound.
                            --
                            -- Paper Fig. 10:
                            --   Φξ(Raise(n)) = C^r_m { O; ∀(⩾ Tξ(n)); ∀(βn ⩾) C^m_n {h!βn i} }
                            -- where `m = min≺{…}`.

                            nodeTy <-
                                case mSchemeInfo of
                                    Just si -> reifyTypeWithNames res (siSubst si) n
                                    Nothing -> reifyType res n

                            let (qs, _) = splitForalls ty
                                names = map fst qs

                            when (length qs /= length ids) $
                                Left (InstantiationError "OpRaise (non-spine): binder spine / identity list length mismatch")

                            -- Compute dependency cutoff: the new binder must be inserted after any
                            -- binder that appears free in `Tξ(n)`.
                            let deps = freeTypeVars nodeTy
                                depIdxs = mapMaybe (`elemIndex` names) deps
                                cutoff = if null depIdxs then (-1) else maximum depIdxs
                                minIdx = min (cutoff + 1) (length ids)

                                nC = canonicalNode n

                                findCandidate :: [Int] -> Either ElabError (Maybe (Int, [ContextStep]))
                                findCandidate [] = Right Nothing
                                findCandidate (i : is) =
                                    case ids !! i of
                                        Nothing -> findCandidate is
                                        Just mNode -> do
                                            ctxOrErr <-
                                                contextToNodeBoundWithOrderKeys
                                                    canonicalNode
                                                    orderKeys
                                                    (srConstraint res)
                                                    (canonicalNode mNode)
                                                    nC
                                            case ctxOrErr of
                                                Nothing -> findCandidate is
                                                Just ctx -> Right (Just (i, ctx))

                            mbCandidate <- findCandidate [minIdx .. length ids - 1]
                            case mbCandidate of
                                Nothing ->
                                    Left $
                                        InstantiationError
                                            "OpRaise (non-spine): no valid context for target binder"
                                Just (insertIdx, ctxMn) -> do
                                    let prefixBefore = take insertIdx names
                                        hAbsBeta = InstSeq (InstInside (InstAbstr "β")) InstElim
                                        aliasOld = InstInside (applyContext ctxMn hAbsBeta)

                                        local =
                                            instMany
                                                [ InstIntro
                                                , InstInside (InstBot nodeTy)
                                                , InstUnder "β" aliasOld
                                                ]

                                        inst = underContext prefixBefore local

                                    ty' <- applyInstantiation ty inst
                                    ids1 <- insertAt insertIdx (Just n) ids
                                    go ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpMerge n m : rest)
            | canonicalNode n == canonicalNode m ->
                go ty ids phi rest lookupBinder
            | otherwise -> do
                mName <- binderNameFor ty ids m lookupBinder
                let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                (inst, ids1) <- atBinder ids ty n (pure hAbs)
                ty' <- applyInstantiation ty inst
                go ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaiseMerge n m : rest) -> do
            -- Paper Fig. 10 special-cases RaiseMerge(r, m) at the (flexible) expansion
            -- root r as !αm. We implement that case precisely: it applies only when
            -- n is the expansion root (up to union-find).
            let nC = canonicalNode n
                rC = canonicalNode orderRoot
            if nC == rC
                then do
                    mName <- binderNameFor ty ids m lookupBinder
                    ty' <- applyInstantiation ty (InstAbstr mName)
                    go ty' [] (composeInst phi (InstAbstr mName)) rest lookupBinder
                else do
                    -- Non-root RaiseMerge behaves like Merge inside the context of n.
                    case elemIndex (Just n) ids of
                        Nothing ->
                            Left (InstantiationError ("OpRaiseMerge: binder " ++ show n ++ " not found in quantifier spine"))
                        Just _ -> do
                            mName <- binderNameFor ty ids m lookupBinder
                            let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                            (inst, ids1) <- atBinder ids ty n (pure hAbs)
                            ty' <- applyInstantiation ty inst
                            go ty' ids1 (composeInst phi inst) rest lookupBinder

    idsForStartType :: SchemeInfo -> ElabType -> [Maybe NodeId]
    idsForStartType si ty =
        let nameToId =
                Map.fromList
                    [ (nm, NodeId k)
                    | (k, nm) <- IntMap.toList (siSubst si)
                    ]
            (qs, _) = splitForalls ty
        in [ case Map.lookup nm nameToId of
                Just nid -> Just nid
                Nothing -> parseBinderId nm
           | (nm, _) <- qs
           ]

    idsFromType :: ElabType -> [Maybe NodeId]
    idsFromType ty =
        let (qs, _) = splitForalls ty
        in map (parseBinderId . fst) qs

    parseBinderId :: String -> Maybe NodeId
    parseBinderId ('t':rest) = NodeId <$> readMaybe rest
    parseBinderId _ = Nothing

    binderNameFor :: ElabType -> [Maybe NodeId] -> NodeId -> (NodeId -> Maybe String) -> Either ElabError String
    binderNameFor ty ids nid lookupBinder =
        case elemIndex (Just nid) ids of
            Just i -> do
                let (qs, _) = splitForalls ty
                    names = map fst qs
                if length names /= length ids
                    then Left (InstantiationError "binderNameFor: binder spine / identity list length mismatch")
                    else if i >= length names
                        then Left (InstantiationError "binderNameFor: index out of range")
                        else Right (names !! i)
            Nothing ->
                case lookupBinder nid of
                    Just nm -> Right nm
                    Nothing -> Right ("t" ++ show (getNodeId nid))

    atBinder :: [Maybe NodeId] -> ElabType -> NodeId -> Either ElabError Instantiation
             -> Either ElabError (Instantiation, [Maybe NodeId])
    atBinder ids ty nid mkInner = do
        i <- binderIndex ids nid
        prefix <- prefixBinderNames ty ids i
        inner <- mkInner
        ids' <- deleteAt i ids
        pure (underContext prefix inner, ids')

    atBinderKeep :: [Maybe NodeId] -> ElabType -> NodeId -> Either ElabError Instantiation
                 -> Either ElabError (Instantiation, [Maybe NodeId])
    atBinderKeep ids ty nid mkInner = do
        i <- binderIndex ids nid
        prefix <- prefixBinderNames ty ids i
        inner <- mkInner
        pure (underContext prefix inner, ids)

    binderIndex :: [Maybe NodeId] -> NodeId -> Either ElabError Int
    binderIndex ids nid =
        case elemIndex (Just nid) ids of
            Just i -> Right i
            Nothing -> Left (InstantiationError ("binder " ++ show nid ++ " not found in identity list"))

    prefixBinderNames :: ElabType -> [Maybe NodeId] -> Int -> Either ElabError [String]
    prefixBinderNames ty ids i = do
        let (qs, _) = splitForalls ty
            names = map fst qs
        if length names /= length ids
            then Left (InstantiationError "prefixBinderNames: binder spine / identity list length mismatch")
            else if i < 0 || i > length names
                then Left (InstantiationError "prefixBinderNames: index out of range")
                else Right (take i names)

    underContext :: [String] -> Instantiation -> Instantiation
    underContext prefix inner = foldr InstUnder inner prefix

    deleteAt :: Int -> [a] -> Either ElabError [a]
    deleteAt i xs
        | i < 0 = Left (InstantiationError "deleteAt: negative index")
        | otherwise =
            let (pre, rest) = splitAt i xs
            in case rest of
                [] -> Left (InstantiationError "deleteAt: index out of range")
                (_:rs) -> Right (pre ++ rs)

    insertAt :: Int -> a -> [a] -> Either ElabError [a]
    insertAt i x xs
        | i < 0 = Left (InstantiationError "insertAt: negative index")
        | i > length xs = Left (InstantiationError "insertAt: index out of range")
        | otherwise =
            let (pre, rest) = splitAt i xs
            in Right (pre ++ (x : rest))

    freeTypeVars :: ElabType -> [String]
    freeTypeVars = nub . goF []
      where
        goF :: [String] -> ElabType -> [String]
        goF bound ty0 = case ty0 of
            TVar v -> if v `elem` bound then [] else [v]
            TArrow a b -> goF bound a ++ goF bound b
            TBase _ -> []
            TBottom -> []
            TForall v mb body ->
                let fvBound = maybe [] (goF bound) mb
                    fvBody = goF (v : bound) body
                in fvBound ++ fvBody
