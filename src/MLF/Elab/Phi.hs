{-# LANGUAGE LambdaCase #-}
module MLF.Elab.Phi (
    contextToNodeBound,
    phiFromEdgeWitness,
    phiFromEdgeWitnessWithTrace
) where

import Control.Monad (when)
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
import MLF.Binding.Tree (checkBindingTree, bindingPathToRoot, lookupBindParent)

-- | Compute an instantiation-context path from a root node to a target node.
--
-- Paper reference: @papers/xmlf.txt@ Figure 10 defines instantiation contexts:
--
--   C ::= {·} | ∀(⩾ C) | ∀(α ⩾) C
--
-- i.e. contexts navigate *only* through quantifiers (under binders) and their
-- bounds (inside-bounds). This helper computes a conservative context path
-- using the binding tree: if @target@ is transitively bound to @root@, we return
-- the sequence of “under binder” steps for the strict ancestors of @target@ on
-- the binding path.
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
    -- bindingPathToRoot returns target ↦ … ↦ binding-root
    path <- bindingToElab (bindingPathToRoot c target)
    let rootId = getNodeId root
        ids = map getNodeId path
    case elemIndex rootId ids of
        Nothing -> pure Nothing
        Just off -> do
            -- Segment is target ↦ … ↦ root; reverse to root ↦ … ↦ target.
            let seg = take (off + 1) path
                chain = reverse seg

            -- Precompute flex-children map (on canonical ids).
            let addChild m (childId, (parent0, flag)) =
                    case flag of
                        BindRigid -> m
                        BindFlex ->
                            let child = canonical (NodeId childId)
                                parent = canonical parent0
                            in if child == parent
                                then m
                                else IntMap.insertWith (++) (getNodeId parent) [child] m

                rawChildren = foldl addChild IntMap.empty (IntMap.toList (cBindParents c))

                childrenOf p =
                    let xs0 = IntMap.findWithDefault [] (getNodeId p) rawChildren
                        -- Deduplicate while preserving ordering.
                        xs =
                            nubSortBy
                                (Order.compareNodesByOrderKey keys)
                                xs0
                    in xs

                nubSortBy :: Eq a => (a -> a -> Ordering) -> [a] -> [a]
                nubSortBy cmp = nub . sortBy cmp

                nameFor nid = "t" ++ show (getNodeId nid)

                go :: [ContextStep] -> [NodeId] -> Either ElabError [ContextStep]
                go acc ns =
                    case ns of
                        (_only : []) -> Right acc
                        (parent : child : rest) -> do
                            let siblings = childrenOf parent
                            idx <- case elemIndex child siblings of
                                Just i -> Right i
                                Nothing ->
                                    Left $
                                        InstantiationError $
                                            "contextToNodeBound: expected child "
                                                ++ show child
                                                ++ " to be flexibly bound to "
                                                ++ show parent
                            let before = take idx siblings
                                underSteps = map (StepUnder . nameFor) before
                                acc' = acc ++ underSteps
                            if null rest
                                then Right acc'
                                else go (acc' ++ [StepInside]) (child : rest)
                        [] -> Right acc

            steps <- go [] chain
            pure (Just steps)
-- | Translate a recorded per-edge graph witness to an xMLF instantiation.
phiFromEdgeWitness :: SolveResult -> Maybe SchemeInfo -> EdgeWitness -> Either ElabError Instantiation
phiFromEdgeWitness res mSchemeInfo ew =
    phiFromEdgeWitnessWithTrace res mSchemeInfo Nothing ew

phiFromEdgeWitnessWithTrace :: SolveResult -> Maybe SchemeInfo -> Maybe EdgeTrace -> EdgeWitness -> Either ElabError Instantiation
phiFromEdgeWitnessWithTrace res mSchemeInfo mTrace ew = do
    requireValidBindingTree
    let InstanceWitness ops = ewWitness ew
        introPhi = instMany (replicate (ewForallIntros ew) InstIntro)
    case mSchemeInfo of
        Nothing -> phiFromType introPhi ops
        Just si -> phiWithScheme si introPhi ops
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

    phiFromType :: Instantiation -> [InstanceOp] -> Either ElabError Instantiation
    phiFromType introPhi ops = do
        ty0 <- reifyType res (ewRoot ew)
        let ids0 = idsFromType ty0
            lookupBinder nid = Just ("t" ++ show (getNodeId nid))
        (sigma, ty1, ids1) <-
            if needsPrec ops
                then reorderBindersByPrec ty0 ids0
                else Right (InstId, ty0, ids0)
        (_, _, phiOps) <- go ty1 ids1 InstId ops lookupBinder
        pure (instMany [sigma, phiOps, introPhi])

    phiWithScheme :: SchemeInfo -> Instantiation -> [InstanceOp] -> Either ElabError Instantiation
    phiWithScheme si introPhi ops = do
        let ty0 = schemeToType (siScheme si)
            subst = siSubst si
            lookupBinder (NodeId i) = IntMap.lookup i subst
            ids0 = idsForStartType si ty0
        (sigma, ty1, ids1) <-
            if needsPrec ops
                then reorderBindersByPrec ty0 ids0
                else Right (InstId, ty0, ids0)
        (_, _, phiOps) <- go ty1 ids1 InstId ops lookupBinder
        pure (instMany [sigma, phiOps, introPhi])

    needsPrec :: [InstanceOp] -> Bool
    needsPrec = any $ \case
        OpRaise{} -> True
        _ -> False

    reorderBindersByPrec :: ElabType -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderBindersByPrec ty ids = do
        let (qs, _) = splitForalls ty
        if length qs /= length ids
            then Left (InstantiationError "reorderBindersByPrec: binder spine / identity list length mismatch")
            else if length qs < 2
                then Right (InstId, ty, ids)
                else do
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

                                canBindTarget :: NodeId -> Maybe [ContextStep]
                                canBindTarget mNode =
                                    case contextToNodeBoundWithOrderKeys canonicalNode orderKeys (srConstraint res) (canonicalNode mNode) nC of
                                        Right (Just steps) -> Just steps
                                        _ -> Nothing

                                candidates =
                                    [ (i, ctx)
                                    | i <- [minIdx .. length ids - 1]
                                    , Just mNode <- [ids !! i]
                                    , Just ctx <- [canBindTarget mNode]
                                    ]

                            case candidates of
                                [] ->
                                    go ty ids phi rest lookupBinder
                                ((insertIdx, ctxMn) : _) -> do
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

        (OpMerge n m : rest) -> do
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
