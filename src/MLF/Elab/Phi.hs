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
import Data.List (elemIndex, findIndex, nub, sortBy)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

import qualified MLF.Util.Order as Order
import qualified MLF.Util.OrderKey as OrderKey
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Inst (applyInstantiation, composeInst, instMany, schemeToType, splitForalls)
import MLF.Elab.Generalize (generalizeAtKeepTarget)
import MLF.Elab.Reify (namedNodes, reifyBoundWithNames)
import MLF.Elab.Sigma (bubbleReorderTo)
import MLF.Elab.Util (topoSortBy)
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Presolution (EdgeTrace(..))
import qualified MLF.Binding.Tree as Binding
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
            namedSet <- namedNodes res
            contextToNodeBoundWithOrderKeys canonical keys c namedSet rootC targetC

contextToNodeBoundWithOrderKeys
    :: (NodeId -> NodeId)
    -> IntMap.IntMap Order.OrderKey
    -> Constraint
    -> IntSet.IntSet
    -> NodeId
    -> NodeId
    -> Either ElabError (Maybe [ContextStep])
contextToNodeBoundWithOrderKeys canonical keys c namedSet root target = do
    let rootC = canonical root
        targetC = canonical target

    if rootC == targetC
        then pure (Just [])
        else do
            let rootNode = IntMap.lookup (getNodeId rootC) (cNodes c)
                needsInsideRoot =
                    case rootNode of
                        Just TyForall{} -> False
                        Just TyExp{} -> False
                        Just TyVar{} -> True
                        Just TyArrow{} -> True
                        Just TyBase{} -> True
                        Just TyBottom{} -> True
                        Nothing -> False
                start =
                    case rootNode of
                        Just TyVar{ tnBound = Just bnd } -> canonical bnd
                        Just TyVar{} -> rootC
                        _ -> rootC
            res <- snd <$> go IntSet.empty IntMap.empty start
            pure $
                if needsInsideRoot
                    then fmap (StepInside :) res
                    else res
  where
    nameFor nid = "t" ++ show (getNodeId nid)

    reachableFromStructural :: NodeId -> IntSet.IntSet
    reachableFromStructural root0 =
        let walk visited [] = visited
            walk visited (nid0:rest) =
                let nid = canonical nid0
                    key = getNodeId nid
                in if IntSet.member key visited
                    then walk visited rest
                    else
                        let visited' = IntSet.insert key visited
                            kids =
                                case IntMap.lookup key (cNodes c) of
                                    Nothing -> []
                                    Just node ->
                                        structuralChildren node
                        in walk visited' (map canonical kids ++ rest)
        in walk IntSet.empty [root0]

    dedupeById :: [NodeId] -> [NodeId]
    dedupeById =
        reverse . IntSet.foldl' (\acc i -> NodeId i : acc) [] . IntSet.fromList . map getNodeId

    orderedBindersAt :: NodeId -> Either ElabError [NodeId]
    orderedBindersAt binder0 = do
        let binder = canonical binder0
        mGenAncestor <- closestGenAncestor (typeRef binder)
        bindersFromGen <-
            case mGenAncestor of
                Nothing -> pure []
                Just genAncestor ->
                    bindingToElab (Binding.boundFlexChildrenAllUnder canonical c (genRef genAncestor))
        let orderRoot =
                case IntMap.lookup (getNodeId binder) (cNodes c) of
                    Just TyForall{ tnBody = body } -> canonical body
                    _ -> binder
            reachable = reachableFromStructural orderRoot
            bindersReachable =
                filter (\nid -> IntSet.member (getNodeId nid) reachable) (dedupeById (map canonical bindersFromGen))
            bindersNamed =
                filter (\nid -> IntSet.member (getNodeId nid) namedSet) bindersReachable
            missing =
                [ nid
                | nid <- bindersNamed
                , not (IntMap.member (getNodeId nid) keys)
                ]
        unless (null missing) $
            Left $
                InstantiationError $
                    "contextToNodeBound: missing order keys for " ++ show missing
        pure (sortBy (Order.compareNodesByOrderKey keys) bindersNamed)

    closestGenAncestor :: NodeRef -> Either ElabError (Maybe GenNodeId)
    closestGenAncestor start = goAncestor IntSet.empty start
      where
        goAncestor visited ref
            | IntSet.member (nodeRefKey ref) visited =
                Left $
                    InstantiationError $
                        "contextToNodeBound: cycle in binding path at " ++ show ref
            | otherwise = do
                mbParent <- bindingToElab (Binding.lookupBindParentUnder canonical c ref)
                case mbParent of
                    Nothing -> Right Nothing
                    Just (GenRef gid, _) -> Right (Just gid)
                    Just (TypeRef parent, _) ->
                        goAncestor (IntSet.insert (nodeRefKey ref) visited) (typeRef (canonical parent))

    go
        :: IntSet.IntSet
        -> IntMap.IntMap (Maybe [ContextStep])
        -> NodeId
        -> Either ElabError (IntMap.IntMap (Maybe [ContextStep]), Maybe [ContextStep])
    go visiting memo nid0 = do
        let nid = canonical nid0
            key = getNodeId nid
            targetC = canonical target
        if nid == targetC
            then
                let res = Just []
                in Right (IntMap.insert key res memo, res)
            else case IntMap.lookup key memo of
                Just res -> Right (memo, res)
                Nothing ->
                    if IntSet.member key visiting
                        then
                            Left $
                                InstantiationError $
                                    "contextToNodeBound: cycle detected at " ++ show nid
                        else
                            case IntMap.lookup key (cNodes c) of
                                Nothing -> Left (MissingNode nid)
                                Just node ->
                                    let visiting' = IntSet.insert key visiting
                                        finish res memo' =
                                            let memo'' = IntMap.insert key res memo'
                                            in Right (memo'', res)
                                    in case node of
                                        TyForall{ tnBody = body } -> do
                                            (memo', res) <- goForall visiting' memo nid body
                                            finish res memo'
                                        TyArrow{ tnDom = dom, tnCod = cod } -> do
                                            (memo', res) <- goChildren visiting' memo [dom, cod]
                                            finish res memo'
                                        TyExp{ tnBody = body } -> do
                                            (memo', res) <- go visiting' memo body
                                            finish res memo'
                                        TyVar{} ->
                                            finish Nothing memo
                                        TyBase{} ->
                                            finish Nothing memo
                                        TyBottom{} ->
                                            finish Nothing memo

    goChildren
        :: IntSet.IntSet
        -> IntMap.IntMap (Maybe [ContextStep])
        -> [NodeId]
        -> Either ElabError (IntMap.IntMap (Maybe [ContextStep]), Maybe [ContextStep])
    goChildren _ memo [] = Right (memo, Nothing)
    goChildren visiting memo (child:rest) = do
        (memo', res) <- go visiting memo child
        case res of
            Just _ -> pure (memo', res)
            Nothing -> goChildren visiting memo' rest

    goForall
        :: IntSet.IntSet
        -> IntMap.IntMap (Maybe [ContextStep])
        -> NodeId
        -> NodeId
        -> Either ElabError (IntMap.IntMap (Maybe [ContextStep]), Maybe [ContextStep])
    goForall visiting memo forallId _body0 = do
        binders <- orderedBindersAt forallId
        let targetC = canonical target
        case elemIndex targetC binders of
            Just i -> do
                let before = take i binders
                    steps = map (StepUnder . nameFor) before
                pure (memo, Just steps)
            Nothing -> do
                let tryBound memoAcc [] = Right (memoAcc, Nothing)
                    tryBound memoAcc (b : bs) =
                        case IntMap.lookup (getNodeId b) (cNodes c) of
                            Just TyVar{ tnBound = Just bnd } -> do
                                let bndC = canonical bnd
                                (memo', res) <- go visiting memoAcc bndC
                                case res of
                                    Just ctx ->
                                        let before = takeWhile (/= b) binders
                                            steps = map (StepUnder . nameFor) before ++ [StepInside] ++ ctx
                                        in pure (memo', Just steps)
                                    Nothing -> tryBound memo' bs
                            Just TyVar{} -> tryBound memoAcc bs
                            Just _ -> tryBound memoAcc bs
                            Nothing -> tryBound memoAcc bs
                (memo', boundRes) <- tryBound memo binders
                case boundRes of
                    Just _ -> pure (memo', boundRes)
                    Nothing -> pure (memo', Nothing)

-- | Translate a recorded per-edge graph witness to an xMLF instantiation.
phiFromEdgeWitness :: SolveResult -> Maybe SchemeInfo -> EdgeWitness -> Either ElabError Instantiation
phiFromEdgeWitness res mSchemeInfo ew =
    phiFromEdgeWitnessWithTrace res mSchemeInfo Nothing ew

phiFromEdgeWitnessWithTrace :: SolveResult -> Maybe SchemeInfo -> Maybe EdgeTrace -> EdgeWitness -> Either ElabError Instantiation
phiFromEdgeWitnessWithTrace res mSchemeInfo mTrace ew = do
    requireValidBindingTree
    namedSet0 <- namedNodes res
    let copied =
            case mTrace of
                Nothing -> IntSet.empty
                Just tr ->
                    IntSet.fromList
                        [ getNodeId (canonicalNode nid)
                        | nid <- IntMap.elems (etCopyMap tr)
                        ]
        interior =
            case mTrace of
                Nothing -> IntSet.empty
                Just tr -> etInterior tr
        namedSet1 = IntSet.difference namedSet0 copied
        rootKey = getNodeId (canonicalNode (ewRoot ew))
        namedSet =
            let base =
                    if IntSet.null interior
                        then namedSet1
                        else IntSet.intersection namedSet1 interior
            in IntSet.delete rootKey base
    let InstanceWitness ops = ewWitness ew
        remapNode nid =
            let nidC = canonicalNode nid
            in if isTyVarNode nidC
                then IntMap.findWithDefault nidC (getNodeId nidC) copyMap
                else nidC
        remapOp op = case op of
            OpGraft arg bv -> OpGraft (remapNode arg) (remapNode bv)
            OpWeaken bv -> OpWeaken (remapNode bv)
            OpRaise n -> OpRaise (remapNode n)
            OpMerge n m -> OpMerge (remapNode n) (remapNode m)
            OpRaiseMerge n m -> OpRaiseMerge (remapNode n) (remapNode m)
        remapStep step = case step of
            StepOmega op -> StepOmega (remapOp op)
            StepIntro -> StepIntro
        steps0 =
            case ewSteps ew of
                [] -> map StepOmega (map remapOp ops)
                xs -> map remapStep xs
    let mSchemeInfo' =
            case (mSchemeInfo, mTrace) of
                (Just si, Just tr) -> Just (remapSchemeInfo tr si)
                _ -> mSchemeInfo
    case mSchemeInfo' of
        Nothing -> do
            (schemeRoot, needsRemap) <- case mTrace of
                Just tr -> do
                    mbParent <- bindingToElab $
                        Binding.lookupBindParentUnder
                            canonicalNode
                            (srConstraint res)
                            (typeRef (canonicalNode (ewRoot ew)))
                    case mbParent of
                        Nothing -> pure (etRoot tr, False)
                        Just _ -> pure (ewRoot ew, True)
                Nothing -> pure (ewRoot ew, False)
            si0 <- schemeInfoForRoot schemeRoot
            let si =
                    case mTrace of
                        Just tr | needsRemap -> remapSchemeInfo tr si0
                        _ -> si0
            phiWithScheme namedSet si steps0
        Just si -> do
            phiWithScheme namedSet si steps0
  where
    requireValidBindingTree :: Either ElabError ()
    requireValidBindingTree =
        case checkBindingTree (srConstraint res) of
            Left err -> Left (BindingTreeError err)
            Right () -> Right ()

    canonicalNode :: NodeId -> NodeId
    canonicalNode = Solve.frWith (srUnionFind res)

    remapSchemeInfo :: EdgeTrace -> SchemeInfo -> SchemeInfo
    remapSchemeInfo tr si =
        let traceCopyMap = etCopyMap tr
            subst' =
                IntMap.fromList
                    [ (getNodeId (canonicalNode mapped), name)
                    | (k, name) <- IntMap.toList (siSubst si)
                    , let nid = NodeId k
                          mapped = IntMap.findWithDefault nid k traceCopyMap
                    ]
        in si { siSubst = subst' }

    schemeInfoForRoot :: NodeId -> Either ElabError SchemeInfo
    schemeInfoForRoot root0 = do
        scopeRoot <- instScopeRoot root0
        (sch, subst) <- generalizeAtKeepTarget res scopeRoot root0
        pure SchemeInfo { siScheme = sch, siSubst = subst }

    instScopeRoot :: NodeId -> Either ElabError NodeRef
    instScopeRoot root0 = goScope IntSet.empty (typeRef (canonicalNode root0))
      where
        goScope visited ref
            | IntSet.member (nodeRefKey ref) visited =
                Right (typeRef (canonicalNode root0))
            | otherwise = do
                mbParent <- bindingToElab (Binding.lookupBindParentUnder canonicalNode (srConstraint res) ref)
                case mbParent of
                    Nothing -> Right (typeRef (canonicalNode root0))
                    Just (GenRef gid, _) -> Right (genRef gid)
                    Just (TypeRef parent, _) ->
                        goScope (IntSet.insert (nodeRefKey ref) visited) (typeRef (canonicalNode parent))

    copyMap :: IntMap.IntMap NodeId
    copyMap =
        case mTrace of
            Nothing -> IntMap.empty
            Just tr -> etCopyMap tr

    invCopyMap :: IntMap.IntMap NodeId
    invCopyMap =
        IntMap.fromList
            [ (getNodeId v, NodeId k)
            | (k, v) <- IntMap.toList copyMap
            ]

    isTyVarNode :: NodeId -> Bool
    isTyVarNode nid =
        let key = getNodeId (canonicalNode nid)
        in case IntMap.lookup key (cNodes (srConstraint res)) of
            Just TyVar{} -> True
            _ -> False

    isBinderNode :: IntSet.IntSet -> NodeId -> Bool
    isBinderNode binderKeys nid =
        let key = getNodeId (canonicalNode nid)
        in case IntMap.lookup key (cNodes (srConstraint res)) of
            Just TyVar{} -> True
            _ -> IntSet.member key binderKeys

    interiorSet :: IntSet.IntSet
    interiorSet =
        case mTrace of
            Nothing -> IntSet.empty
            Just tr -> etInterior tr

    orderRoot :: NodeId
    -- Paper root `r` for Φ/Σ is the expansion root (TyExp body), not the TyExp
    -- wrapper itself. When a trace is available, prefer its root to stay in
    -- the same node space as witness operations.
    orderRoot =
        case mTrace of
            Nothing -> ewRoot ew
            Just tr -> etRoot tr

    orderKeys :: IntMap.IntMap Order.OrderKey
    orderKeys =
        let nodes = cNodes (srConstraint res)
            extraChildren nid =
                let boundKids =
                        case IntMap.lookup (getNodeId nid) nodes of
                            Just TyVar{ tnBound = Just bnd } -> [bnd]
                            _ -> []
                    bindKids =
                        [ canonicalNode child
                        | (childKey, (parentRef, flag)) <- IntMap.toList (cBindParents (srConstraint res))
                        , flag == BindFlex
                        , TypeRef child <- [nodeRefFromKey childKey]
                        , let parentC =
                                case parentRef of
                                    TypeRef parentN -> TypeRef (canonicalNode parentN)
                                    GenRef gid -> GenRef gid
                        , parentC == TypeRef (canonicalNode nid)
                        ]
                in boundKids ++ bindKids
        in OrderKey.orderKeysFromRootWithExtra canonicalNode nodes extraChildren orderRoot Nothing

    substForTypes :: IntMap.IntMap String
    substForTypes =
        case mSchemeInfo of
            Just si -> siSubst si
            Nothing -> IntMap.empty

    reifyTypeArg :: IntSet.IntSet -> NodeId -> Either ElabError ElabType
    reifyTypeArg _ arg =
        reifyBoundWithNames res substForTypes arg

    reifyBoundType :: NodeId -> Either ElabError ElabType
    reifyBoundType = reifyBoundWithNames res substForTypes

    phiWithScheme :: IntSet.IntSet -> SchemeInfo -> [InstanceStep] -> Either ElabError Instantiation
    phiWithScheme namedSet si steps = do
        let ty0 = schemeToType (siScheme si)
            subst = siSubst si
            lookupBinder (NodeId i) = IntMap.lookup i subst
            ids0 = idsForStartType si ty0
            binderKeys = IntSet.fromList (IntMap.keys subst)
            omegaOps = [op | StepOmega op <- steps]
        (sigma, ty1, ids1) <-
            if needsPrec omegaOps
                then reorderBindersByPrec ty0 ids0
                else Right (InstId, ty0, ids0)
        (_, _, phiOps) <- goSteps binderKeys namedSet ty1 ids1 InstId steps lookupBinder
        pure (normalizeInst (instMany [sigma, phiOps]))

    needsPrec :: [InstanceOp] -> Bool
    needsPrec = any $ \case
        OpRaise{} -> True
        _ -> False

    applyInst :: String -> ElabType -> Instantiation -> Either ElabError ElabType
    applyInst label ty0 inst =
        case applyInstantiation ty0 inst of
            Left (InstantiationError msg) ->
                Left $
                    InstantiationError $
                        label
                            ++ ": "
                            ++ msg
                            ++ " ; inst="
                            ++ pretty inst
                            ++ " ; ty="
                            ++ pretty ty0
            Left err -> Left err
            Right ty1 -> Right ty1

    goSteps
        :: IntSet.IntSet
        -> IntSet.IntSet
        -> ElabType
        -> [Maybe NodeId]
        -> Instantiation
        -> [InstanceStep]
        -> (NodeId -> Maybe String)
        -> Either ElabError (ElabType, [Maybe NodeId], Instantiation)
    goSteps binderKeys namedSet ty ids phi steps lookupBinder = case steps of
        [] -> Right (ty, ids, phi)
        StepIntro : rest -> do
            ty' <- applyInst "StepIntro" ty InstIntro
            let ids' = Nothing : ids
            goSteps binderKeys namedSet ty' ids' (composeInst phi InstIntro) rest lookupBinder
        _ ->
            let (segment, rest) = span isOmega steps
                ops = [op | StepOmega op <- segment]
            in do
                (ty', ids', phi') <- go binderKeys namedSet ty ids phi ops lookupBinder
                goSteps binderKeys namedSet ty' ids' phi' rest lookupBinder
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
                    if not (null missing)
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

    go :: IntSet.IntSet -> IntSet.IntSet -> ElabType -> [Maybe NodeId] -> Instantiation -> [InstanceOp] -> (NodeId -> Maybe String)
       -> Either ElabError (ElabType, [Maybe NodeId], Instantiation)
    go binderKeys namedSet ty ids phi ops lookupBinder = case ops of
        [] -> Right (ty, ids, phi)

        (OpGraft arg bv : OpWeaken bv' : rest)
            | bv == bv' -> do
                if not (isBinderNode binderKeys bv)
                    then go binderKeys namedSet ty ids phi rest lookupBinder
                    else do
                        case lookupBinderIndex binderKeys ids bv of
                            Nothing -> go binderKeys namedSet ty ids phi rest lookupBinder
                            Just i -> do
                                let (qs, _) = splitForalls ty
                                when (length qs /= length ids) $
                                    Left (InstantiationError "OpGraft+OpWeaken: binder spine / identity list length mismatch")
                                let mbBound = snd (qs !! i)
                                    boundIsBottom = case mbBound of
                                        Nothing -> True
                                        Just TBottom -> True
                                        _ -> False
                                if not boundIsBottom
                                    then go binderKeys namedSet ty ids phi rest lookupBinder
                                    else do
                                        (inst, ids1) <- atBinder binderKeys ids ty bv $ do
                                            argTy <- reifyTypeArg namedSet (graftArgFor arg bv)
                                            pure (InstApp argTy)
                                        ty' <- applyInst "OpGraft+OpWeaken" ty inst
                                        go binderKeys namedSet ty' ids1 (composeInst phi inst) rest lookupBinder
            | otherwise ->
                Left (InstantiationError "witness op mismatch: OpGraft/OpWeaken refer to different nodes")

        (OpGraft arg bv : rest) -> do
            if not (isBinderNode binderKeys bv)
                then go binderKeys namedSet ty ids phi rest lookupBinder
                else do
                    case lookupBinderIndex binderKeys ids bv of
                        Nothing -> go binderKeys namedSet ty ids phi rest lookupBinder
                        Just i -> do
                            let (qs, _) = splitForalls ty
                            when (length qs /= length ids) $
                                Left (InstantiationError "OpGraft: binder spine / identity list length mismatch")
                            let mbBound = snd (qs !! i)
                                boundIsBottom = case mbBound of
                                    Nothing -> True
                                    Just TBottom -> True
                                    _ -> False
                            if not boundIsBottom
                                then go binderKeys namedSet ty ids phi rest lookupBinder
                                else do
                                    (inst, ids1) <- atBinderKeep binderKeys ids ty bv $ do
                                        argTy <- reifyTypeArg namedSet arg
                                        pure (InstInside (InstBot argTy))
                                    ty' <- applyInst "OpGraft" ty inst
                                    go binderKeys namedSet ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpWeaken bv : rest) -> do
            if not (isBinderNode binderKeys bv)
                then go binderKeys namedSet ty ids phi rest lookupBinder
                else do
                    (inst, ids1) <- atBinder binderKeys ids ty bv (pure InstElim)
                    ty' <- applyInst "OpWeaken" ty inst
                    go binderKeys namedSet ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaise n : rest) -> do
            let nC = canonicalNode n
            if not (IntSet.null interiorSet) && not (IntSet.member (getNodeId nC) interiorSet)
                then go binderKeys namedSet ty ids phi rest lookupBinder
                else
                    -- Paper Fig. 10: operations on rigid nodes translate to the identity
                    -- instantiation (they are inlined and not expressible as xMLF instantiation
                    -- steps). Our witnesses can still contain such ops due to normalization and
                    -- binding-tree harmonization, so treat them as no-ops here.
                    case lookupBindParent (srConstraint res) (typeRef nC) of
                        Just (_, BindRigid) ->
                            go binderKeys namedSet ty ids phi rest lookupBinder
                        _ ->
                            -- Paper Fig. 10: Raise(n) introduces a fresh quantifier one level higher,
                            -- bounds it by Tξ(n), then aliases/eliminates the old binder.
                            --
                            -- For spine binders: use the existing logic
                            -- For non-spine nodes: use binding edges + ≺ ordering to compute context
                            case lookupBinderIndex binderKeys ids n of
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
                                    ty' <- applyInst "OpRaise(spine)" ty inst

                                    idsNoN <- deleteAt i ids
                                    ids1 <- insertAt insertIndex (Just n) idsNoN
                                    go binderKeys namedSet ty' ids1 (composeInst phi inst) rest lookupBinder

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
                                        reifyBoundType n

                                    let idsSynced = resyncIds ty ids
                                        (qs, _) = splitForalls ty
                                        names = map fst qs

                                    when (length qs /= length idsSynced) $
                                        Left (InstantiationError "OpRaise (non-spine): binder spine / identity list length mismatch")

                                    -- Compute dependency cutoff: the new binder must be inserted after any
                                    -- binder that appears free in `Tξ(n)`.
                                    let deps = freeTypeVars nodeTy
                                        depIdxs = mapMaybe (`elemIndex` names) deps
                                        cutoff = if null depIdxs then (-1) else maximum depIdxs
                                        minIdx = min (cutoff + 1) (length ids)

                                        findCandidate :: [Int] -> Either ElabError (Maybe (Int, [ContextStep]))
                                        findCandidate [] = Right Nothing
                                        findCandidate (i : is) =
                                            case idsSynced !! i of
                                                Nothing -> findCandidate is
                                                Just mNode -> do
                                                    ctxOrErr <-
                                                        contextToNodeBoundWithOrderKeys
                                                            canonicalNode
                                                            orderKeys
                                                            (srConstraint res)
                                                            namedSet
                                                            (canonicalNode mNode)
                                                            nC
                                                    case ctxOrErr of
                                                        Nothing -> findCandidate is
                                                        Just ctx -> Right (Just (i, ctx))

                                    mbCandidate <- findCandidate [minIdx .. length idsSynced - 1]
                                    case mbCandidate of
                                        Nothing ->
                                            if minIdx /= 0
                                                then go binderKeys namedSet ty idsSynced phi rest lookupBinder
                                                else do
                                                    ctxOrErr <-
                                                        contextToNodeBoundWithOrderKeys
                                                            canonicalNode
                                                            orderKeys
                                                            (srConstraint res)
                                                            namedSet
                                                            (canonicalNode orderRoot)
                                                            nC
                                                    case ctxOrErr of
                                                        Nothing ->
                                                            go binderKeys namedSet ty idsSynced phi rest lookupBinder
                                                        Just ctxMn -> do
                                                            let hAbsBeta = InstSeq (InstInside (InstAbstr "β")) InstElim
                                                                aliasOld = applyContext ctxMn hAbsBeta

                                                                local =
                                                                    instMany
                                                                        [ InstIntro
                                                                        , InstInside (InstBot nodeTy)
                                                                        , InstUnder "β" aliasOld
                                                                        ]

                                                                inst = local

                                                            ty' <- applyInst "OpRaise(non-spine)" ty inst
                                                            ids1 <- insertAt 0 (Just n) idsSynced
                                                            let ids2 = resyncIds ty' ids1
                                                            go binderKeys namedSet ty' ids2 (composeInst phi inst) rest lookupBinder
                                        Just (insertIdx, ctxMn) -> do
                                            let prefixBefore = take insertIdx names
                                                hAbsBeta = InstSeq (InstInside (InstAbstr "β")) InstElim
                                                aliasOld = applyContext ctxMn hAbsBeta

                                                local =
                                                    instMany
                                                        [ InstIntro
                                                        , InstInside (InstBot nodeTy)
                                                        , InstUnder "β" aliasOld
                                                        ]

                                                inst = underContext prefixBefore local

                                            ty' <- applyInst "OpRaise(non-spine)" ty inst
                                            ids1 <- insertAt insertIdx (Just n) idsSynced
                                            let ids2 = resyncIds ty' ids1
                                            go binderKeys namedSet ty' ids2 (composeInst phi inst) rest lookupBinder

        (OpMerge n m : rest)
            | not (isBinderNode binderKeys n) || not (isBinderNode binderKeys m) ->
                go binderKeys namedSet ty ids phi rest lookupBinder
            | canonicalNode n == canonicalNode m ->
                go binderKeys namedSet ty ids phi rest lookupBinder
            | otherwise -> do
                mName <- binderNameFor binderKeys ty ids m lookupBinder
                let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                (inst, ids1) <- atBinder binderKeys ids ty n (pure hAbs)
                ty' <- applyInst "OpMerge" ty inst
                go binderKeys namedSet ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaiseMerge n m : rest) -> do
            if not (isBinderNode binderKeys n) || not (isBinderNode binderKeys m)
                then go binderKeys namedSet ty ids phi rest lookupBinder
                else do
                    -- Paper Fig. 10 special-cases RaiseMerge(r, m) at the (flexible) expansion
                    -- root r as !αm. We implement that case precisely: it applies only when
                    -- n is the expansion root (up to union-find).
                    let nC = canonicalNode n
                        rC = canonicalNode orderRoot
                    if nC == rC
                        then do
                            mName <- binderNameFor binderKeys ty ids m lookupBinder
                            ty' <- applyInst "OpRaiseMerge(abs)" ty (InstAbstr mName)
                            go binderKeys namedSet ty' [] (composeInst phi (InstAbstr mName)) rest lookupBinder
                        else do
                            -- Non-root RaiseMerge behaves like Merge inside the context of n.
                            case lookupBinderIndex binderKeys ids n of
                                Nothing ->
                                    Left (InstantiationError ("OpRaiseMerge: binder " ++ show n ++ " not found in quantifier spine"))
                                Just _ -> do
                                    mName <- binderNameFor binderKeys ty ids m lookupBinder
                                    let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                                    (inst, ids1) <- atBinder binderKeys ids ty n (pure hAbs)
                                    ty' <- applyInst "OpRaiseMerge" ty inst
                                    go binderKeys namedSet ty' ids1 (composeInst phi inst) rest lookupBinder

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

    resyncIds :: ElabType -> [Maybe NodeId] -> [Maybe NodeId]
    resyncIds ty idsPrev =
        let (qs, _) = splitForalls ty
            names = map fst qs
            nameMap =
                Map.fromList
                    [ (nm, nid)
                    | (Just nid, nm) <- zip idsPrev names
                    ]
            lookupName nm =
                case Map.lookup nm nameMap of
                    Just nid -> Just nid
                    Nothing -> parseBinderId nm
        in map lookupName names

    parseBinderId :: String -> Maybe NodeId
    parseBinderId ('t':rest) = NodeId <$> readMaybe rest
    parseBinderId _ = Nothing

    binderNameFor :: IntSet.IntSet -> ElabType -> [Maybe NodeId] -> NodeId -> (NodeId -> Maybe String) -> Either ElabError String
    binderNameFor binderKeys ty ids nid lookupBinder =
        case lookupBinderIndex binderKeys ids nid of
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

    atBinder :: IntSet.IntSet -> [Maybe NodeId] -> ElabType -> NodeId -> Either ElabError Instantiation
             -> Either ElabError (Instantiation, [Maybe NodeId])
    atBinder binderKeys ids ty nid mkInner = do
        i <- binderIndex binderKeys ids nid
        prefix <- prefixBinderNames ty ids i
        inner <- mkInner
        ids' <- deleteAt i ids
        pure (underContext prefix inner, ids')

    atBinderKeep :: IntSet.IntSet -> [Maybe NodeId] -> ElabType -> NodeId -> Either ElabError Instantiation
                 -> Either ElabError (Instantiation, [Maybe NodeId])
    atBinderKeep binderKeys ids ty nid mkInner = do
        i <- binderIndex binderKeys ids nid
        prefix <- prefixBinderNames ty ids i
        inner <- mkInner
        pure (underContext prefix inner, ids)

    lookupBinderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Maybe Int
    lookupBinderIndex binderKeys ids nid =
        if not (isBinderNode binderKeys nid)
            then Nothing
            else
                let nC = canonicalNode nid
                    key = getNodeId nC
                    candidateKeys =
                        IntSet.fromList $
                            [key]
                                ++ maybe [] (pure . getNodeId . canonicalNode) (IntMap.lookup key copyMap)
                                ++ maybe [] (pure . getNodeId . canonicalNode) (IntMap.lookup key invCopyMap)
                    matches = \case
                        Just nid' -> IntSet.member (getNodeId (canonicalNode nid')) candidateKeys
                        Nothing -> False
                in findIndex matches ids

    binderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Either ElabError Int
    binderIndex binderKeys ids nid =
        case lookupBinderIndex binderKeys ids nid of
            Just i -> Right i
            Nothing ->
                Left $
                    InstantiationError $
                        "binder " ++ show nid ++ " not found in identity list " ++ show ids

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

    normalizeInst :: Instantiation -> Instantiation
    normalizeInst inst = case inst of
        InstSeq a b ->
            let a' = normalizeInst a
                b' = normalizeInst b
            in case (a', b') of
                (InstInside (InstBot t), InstElim) -> InstApp t
                (InstId, x) -> x
                (x, InstId) -> x
                _ -> InstSeq a' b'
        InstInside a -> InstInside (normalizeInst a)
        InstUnder v a -> InstUnder v (normalizeInst a)
        _ -> inst

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
