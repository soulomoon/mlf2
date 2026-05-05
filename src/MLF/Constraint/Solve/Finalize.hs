module MLF.Constraint.Solve.Finalize (
    finalizeConstraintWithUF,
    repairNonUpperParents,
    rewriteConstraintWithUF,
    applyUFConstraint,
    validateSolvedGraph,
    validateSolvedGraphStrict,
    frWith
) where

import Control.Monad (forM, forM_, unless)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe, isNothing)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Canonicalize as Canonicalize
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Solve.Internal (SolveResult(..))
import MLF.Constraint.Solve.Worklist (SolveError(..))
import MLF.Constraint.Types.Graph hiding (lookupNode)
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.UnionFind as UnionFind

-- | Apply snapshot finalization steps to a pre-rewrite constraint and
-- union-find map without running unification.
finalizeConstraintWithUF :: IntMap NodeId -> Constraint p -> Either SolveError (SolveResult p)
finalizeConstraintWithUF uf preRewrite = do
    let rewritten = rewriteConstraintWithUF uf preRewrite
    (rewritten', elimSubst) <- case rewriteEliminatedBinders rewritten of
        Left err -> Left (BindingTreeError err)
        Right out -> Right out
    let uf' = applyElimSubstToUF uf elimSubst
        repaired = pruneBindParentsToLive (repairNonUpperParents rewritten')
        result = SolveResult { srConstraint = repaired, srUnionFind = uf' }
        violations = validateSolvedGraph result
    case Binding.checkBindingTree repaired of
        Left err -> Left (BindingTreeError err)
        Right () ->
            if null violations
                then Right result
                else Left (ValidationFailed violations)

applyElimSubstToUF :: IntMap NodeId -> IntMap NodeId -> IntMap NodeId
applyElimSubstToUF uf subst =
    IntMap.foldlWithKey'
        (\acc elimId rep ->
            let repC = frWith uf rep
            in IntMap.insert elimId repC acc
        )
        uf
        subst

repairNonUpperParents :: Constraint p -> Constraint p
repairNonUpperParents c =
    let bindParents0 = cBindParents c
        nodes = cNodes c
        rootGen =
            let genRefs =
                    [ genRef (GenNodeId gid)
                    | gid <- IntMap.keys (getGenNodeMap (cGenNodes c))
                    ]
                isRoot ref = not (IntMap.member (nodeRefKey ref) bindParents0)
            in case filter isRoot genRefs of
                g : _ -> Just g
                [] -> Nothing
        incomingParents =
            let addOne parent child m =
                    IntMap.insertWith
                        IntSet.union
                        (getNodeId child)
                        (IntSet.singleton (getNodeId parent))
                        m
                addNode m node =
                    let parent = tnId node
                        kids = structuralChildrenWithBounds node
                    in foldl' (flip (addOne parent)) m kids
            in foldl' addNode IntMap.empty (map snd (toListNode nodes))
        pickUpperParent childN =
            case IntMap.lookup (getNodeId childN) incomingParents of
                Just ps ->
                    case IntSet.toList ps of
                        p : _ -> Just (typeRef (NodeId p))
                        [] -> rootGen
                Nothing -> rootGen
        fixOne childKey (parentRef, flag) =
            case nodeRefFromKey childKey of
                GenRef _ -> (parentRef, flag)
                TypeRef childN ->
                    case parentRef of
                        GenRef _ -> (parentRef, flag)
                        _ ->
                            if Binding.isUpper c parentRef (typeRef childN)
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
        bindParents1 = IntMap.mapWithKey fixOne bindParents0
    in c { cBindParents = bindParents1 }

pruneBindParentsToLive :: Constraint p -> Constraint p
pruneBindParentsToLive c =
    let nodes = cNodes c
        genNodes = cGenNodes c
        okRef ref = case ref of
            TypeRef nid ->
                case lookupNodeIn nodes nid of
                    Just _ -> True
                    Nothing -> False
            GenRef gid ->
                IntMap.member (getGenNodeId gid) (getGenNodeMap genNodes)
        keep childKey (parentRef, _flag) =
            okRef (nodeRefFromKey childKey) && okRef parentRef
        bindParents' = IntMap.filterWithKey keep (cBindParents c)
    in c { cBindParents = bindParents' }

-- | Rewrite every node/edge to UF representatives and collapse duplicates,
-- preferring structured nodes when both sides share an id.
rewriteConstraintWithUF :: IntMap NodeId -> Constraint p -> Constraint p
rewriteConstraintWithUF = applyUFConstraint

applyUFConstraint :: IntMap NodeId -> Constraint p -> Constraint p
applyUFConstraint uf c =
    let canonical = frWith uf
        nodes' = IntMap.fromListWith Canonicalize.chooseRepNode (map rewriteNode (NodeAccess.allNodes c))
        bindParents0 = cBindParents c
        bindParentsAdjusted =
            IntMap.mapWithKey
                (\childKey (parent, flag) ->
                    case nodeRefFromKey childKey of
                        TypeRef nid
                            | IntSet.member (getNodeId nid) (cEliminatedVars c) ->
                                (parent, BindFlex)
                        _ -> (parent, flag)
                )
                bindParents0
        bindParents' =
            Canonicalize.rewriteBindParentsLenient
                canonical
                (\childC ->
                    case childC of
                        TypeRef nid -> IntMap.member (getNodeId nid) nodes'
                        GenRef gid ->
                            IntMap.member (getGenNodeId gid) (getGenNodeMap (cGenNodes c))
                )
                bindParentsAdjusted
        eliminated' = rewriteEliminated canonical nodes' (cEliminatedVars c)
        weakened' = rewriteWeakened canonical nodes' (cWeakenedVars c)
        genNodes' = rewriteGenNodes canonical nodes' (cGenNodes c)
    in c
        { cNodes = NodeMap nodes'
        , cInstEdges = Canonicalize.rewriteInstEdges canonical (cInstEdges c)
        , cUnifyEdges = Canonicalize.rewriteUnifyEdges canonical (cUnifyEdges c)
        , cBindParents = bindParents'
        , cEliminatedVars = eliminated'
        , cWeakenedVars = weakened'
        , cGenNodes = genNodes'
        }
  where
    rewriteNode :: TyNode -> (Int, TyNode)
    rewriteNode n =
        let nid' = frWith uf (tnId n)
        in (getNodeId nid', case n of
            TyVar { tnBound = mb } -> TyVar { tnId = nid', tnBound = fmap (frWith uf) mb }
            TyBottom {} -> TyBottom nid'
            TyArrow { tnDom = dom, tnCod = cod } -> TyArrow nid' (frWith uf dom) (frWith uf cod)
            TyBase { tnBase = baseTy } -> TyBase nid' baseTy
            TyForall { tnBody = body } -> TyForall nid' (frWith uf body)
            TyExp { tnExpVar = expVar, tnBody = body } -> TyExp nid' expVar (frWith uf body)
            TyMu { tnBody = body } -> TyMu nid' (frWith uf body)
            TyCon { tnCon = con, tnArgs = args } -> TyCon nid' con (NE.map (frWith uf) args)
            )

    rewriteEliminated :: (NodeId -> NodeId) -> IntMap TyNode -> EliminatedVars -> EliminatedVars
    rewriteEliminated canon nodes0 elims0 =
        IntSet.fromList
            [ getNodeId vC
            | vid <- IntSet.toList elims0
            , let vC = canon (NodeId vid)
            , case IntMap.lookup (getNodeId vC) nodes0 of
                Just TyVar {} -> True
                _ -> False
            ]

    rewriteWeakened :: (NodeId -> NodeId) -> IntMap TyNode -> WeakenedVars -> WeakenedVars
    rewriteWeakened canon nodes0 weakened0 =
        IntSet.fromList
            [ getNodeId vC
            | vid <- IntSet.toList weakened0
            , let vC = canon (NodeId vid)
            , case IntMap.lookup (getNodeId vC) nodes0 of
                Just TyVar {} -> True
                _ -> False
            ]

    rewriteGenNodes :: (NodeId -> NodeId) -> IntMap TyNode -> GenNodeMap GenNode -> GenNodeMap GenNode
    rewriteGenNodes canon nodes0 gen0 =
        let rewriteOne g =
                let (schemesRev, _seen) =
                        foldl'
                            (\(acc, seen) scheme ->
                                let scheme' = canon scheme
                                    key = getNodeId scheme'
                                in if IntMap.member key nodes0 && not (IntSet.member key seen)
                                    then (scheme' : acc, IntSet.insert key seen)
                                    else (acc, seen)
                            )
                            ([], IntSet.empty)
                            (gnSchemes g)
                    schemes' = reverse schemesRev
                in (genNodeKey (gnId g), g { gnSchemes = schemes' })
        in GenNodeMap
            (IntMap.fromListWith const (map rewriteOne (IntMap.elems (getGenNodeMap gen0))))

rewriteEliminatedBinders :: Constraint p -> Either BindingError (Constraint p, IntMap NodeId)
rewriteEliminatedBinders c0
    | IntSet.null elims0 =
        pure (c0, IntMap.empty)
    | otherwise = do
        let nodes0 = cNodes c0
            nodes0Map =
                IntMap.fromList
                    [ (getNodeId nid, node)
                    | (nid, node) <- toListNode nodes0
                    ]
            bindParents0 = cBindParents c0
            maxId = maxNodeIdKeyOr0 c0
            elimList = IntSet.toList elims0

        forM_ elimList $ \vid ->
            unless (IntMap.member vid nodes0Map) $
                Left $
                    InvalidBindingTree $
                        "rewriteEliminatedBinders: eliminated node " ++ show vid ++ " not in cNodes"

        let elimSet = IntSet.fromList elimList
            rootGen =
                let genRefs =
                        [ GenRef (GenNodeId gid)
                        | gid <- IntMap.keys (getGenNodeMap (cGenNodes c0))
                        ]
                    isRoot ref = not (IntMap.member (nodeRefKey ref) bindParents0)
                in find isRoot genRefs
            unbounded =
                [ vid
                | vid <- elimList
                , isNothing (VarStore.lookupVarBound c0 (NodeId vid))
                ]
            findCycle start = go [] start
              where
                go path nid
                    | nid `elem` path =
                        let cyclePath = dropWhile (/= nid) path
                        in cyclePath
                    | not (IntSet.member nid elimSet) = []
                    | otherwise =
                        case VarStore.lookupVarBound c0 (NodeId nid) of
                            Just bnd
                                | IntSet.member (getNodeId bnd) elimSet ->
                                    go (path ++ [nid]) (getNodeId bnd)
                            _ -> []
            cycleNodes = IntSet.fromList (concatMap findCycle elimList)
            bottomTargets =
                let base = unbounded ++ IntSet.toList cycleNodes
                in IntSet.toList (IntSet.fromList base)
            bottomIds = [NodeId i | i <- [maxId + 1 .. maxId + length bottomTargets]]
            bottomMap = IntMap.fromList (zip bottomTargets bottomIds)
            bottomNodes =
                IntMap.fromList
                    [ (getNodeId bid, TyBottom bid)
                    | bid <- bottomIds
                    ]
            bottomSet = IntSet.fromList (map getNodeId bottomIds)

            resolve :: IntSet.IntSet -> NodeId -> Either BindingError NodeId
            resolve seen nid
                | not (IntSet.member (getNodeId nid) elims0) =
                    if IntMap.member (getNodeId nid) nodes0Map
                        then Right nid
                        else Left $
                            InvalidBindingTree $
                                "rewriteEliminatedBinders: missing node " ++ show nid
                | IntSet.member (getNodeId nid) seen =
                    case IntMap.lookup (getNodeId nid) bottomMap of
                        Just b -> Right b
                        Nothing ->
                            Left $
                                InvalidBindingTree "rewriteEliminatedBinders: cycle in eliminated-binder bounds"
                | otherwise =
                    case VarStore.lookupVarBound c0 nid of
                        Nothing ->
                            case IntMap.lookup (getNodeId nid) bottomMap of
                                Just b -> Right b
                                Nothing ->
                                    Left $
                                        InvalidBindingTree $
                                            "rewriteEliminatedBinders: missing bottom mapping for " ++ show nid
                        Just bnd ->
                            resolve (IntSet.insert (getNodeId nid) seen) bnd

        substPairs <- forM elimList $ \vid -> do
            rep <- resolve IntSet.empty (NodeId vid)
            pure (vid, rep)
        let subst = IntMap.fromList substPairs
            substNode nid = IntMap.findWithDefault nid (getNodeId nid) subst

            rewriteNode node = case node of
                TyVar { tnId = nid, tnBound = mb } ->
                    let mb' =
                            case mb of
                                Nothing -> Nothing
                                Just b ->
                                    let b' = substNode b
                                    in if IntSet.member (getNodeId b') bottomSet
                                        then Nothing
                                        else Just b'
                    in TyVar { tnId = nid, tnBound = mb' }
                TyBottom {} -> node
                TyArrow { tnId = nid, tnDom = dom, tnCod = cod } ->
                    TyArrow nid (substNode dom) (substNode cod)
                TyBase {} -> node
                TyForall { tnId = nid, tnBody = body } ->
                    TyForall nid (substNode body)
                TyExp { tnId = nid, tnExpVar = expVar, tnBody = body } ->
                    TyExp nid expVar (substNode body)
                TyMu { tnId = nid, tnBody = body } ->
                    TyMu nid (substNode body)
                TyCon { tnId = nid, tnCon = con, tnArgs = args } ->
                    TyCon nid con (NE.map substNode args)

            nodes1 =
                IntMap.fromList
                    [ (getNodeId (tnId n), rewriteNode n)
                    | n <- IntMap.elems nodes0Map
                    , not (IntSet.member (getNodeId (tnId n)) elimSet)
                    ]
            nodes' = IntMap.union nodes1 bottomNodes
            inNodesRef ref = case ref of
                TypeRef nid -> IntMap.member (getNodeId nid) nodes'
                GenRef gid ->
                    IntMap.member (getGenNodeId gid) (getGenNodeMap (cGenNodes c0))
        let resolveParent ref0 = go IntSet.empty ref0
              where
                go visited ref
                    | IntSet.member (nodeRefKey ref) visited =
                        Left $
                            InvalidBindingTree $
                                "rewriteEliminatedBinders: cycle in eliminated-binder parents at " ++ show ref
                    | otherwise =
                        case ref of
                            GenRef _ -> Right (Just ref)
                            TypeRef nid ->
                                if IntSet.member (getNodeId nid) elims0
                                    then
                                        case IntMap.lookup (nodeRefKey ref) bindParents0 of
                                            Nothing -> Right rootGen
                                            Just (parent', _) ->
                                                go (IntSet.insert (nodeRefKey ref) visited) parent'
                                    else Right (Just (TypeRef nid))

        bindEntries <- forM (IntMap.toList bindParents0) $ \(childKey, (parent0, flag)) -> do
            let childRef = nodeRefFromKey childKey
                childElim = case childRef of
                    TypeRef nid -> IntSet.member (getNodeId nid) elims0
                    GenRef _ -> False
            if childElim
                then pure Nothing
                else do
                    parentResolved <- resolveParent parent0
                    let parent' = fromMaybe parent0 parentResolved
                        child' = case childRef of
                            TypeRef nid -> TypeRef (substNode nid)
                            GenRef gid -> GenRef gid
                    if child' == parent'
                        then pure Nothing
                        else if not (inNodesRef child')
                            then pure Nothing
                            else if not (inNodesRef parent')
                                then Left $
                                    InvalidBindingTree $
                                        "rewriteEliminatedBinders: missing binding parent "
                                            ++ show parent'
                                            ++ " for child "
                                            ++ show child'
                                else pure (Just (nodeRefKey child', (parent', flag)))

        bottomParentsList <- forM (zip bottomTargets bottomIds) $ \(vid, bid) -> do
            case IntMap.lookup (nodeRefKey (typeRef (NodeId vid))) bindParents0 of
                Nothing -> pure []
                Just (parent0, flag) -> do
                    parentResolved <- resolveParent parent0
                    case parentResolved of
                        Nothing -> pure []
                        Just parent' ->
                            if inNodesRef parent'
                                then pure [(nodeRefKey (typeRef bid), (parent', flag))]
                                else pure []

        let bottomParents = concat bottomParentsList
            bindParents0' = IntMap.fromList (catMaybes bindEntries ++ bottomParents)
            bindParents' =
                case rootGen of
                    Nothing -> bindParents0'
                    Just rootRef ->
                        IntSet.foldl'
                            (\bp nidInt ->
                                let childRef = typeRef (NodeId nidInt)
                                    childKey = nodeRefKey childRef
                                in if IntMap.member childKey bp
                                    then bp
                                    else IntMap.insert childKey (rootRef, BindFlex) bp
                            )
                            bindParents0'
                            (IntSet.fromList (IntMap.keys nodes'))

            instEdges' =
                [ InstEdge eid (substNode l) (substNode r)
                | InstEdge eid l r <- cInstEdges c0
                ]
            unifyEdges' =
                [ UnifyEdge (substNode l) (substNode r)
                | UnifyEdge l r <- cUnifyEdges c0
                ]
            genNodesSubst =
                let rewriteOne g =
                        let (schemesRev, _seen) =
                                foldl'
                                    (\(acc, seen) scheme ->
                                        let scheme' = substNode scheme
                                            key = getNodeId scheme'
                                        in if IntMap.member key nodes' && not (IntSet.member key seen)
                                            then (scheme' : acc, IntSet.insert key seen)
                                            else (acc, seen)
                                    )
                                    ([], IntSet.empty)
                                    (gnSchemes g)
                            schemes' = reverse schemesRev
                        in (genNodeKey (gnId g), g { gnSchemes = schemes' })
                in GenNodeMap
                    (IntMap.fromListWith const (map rewriteOne (NodeAccess.allGenNodes c0)))
            weakened' =
                IntSet.fromList
                    [ getNodeId v'
                    | vid <- IntSet.toList (cWeakenedVars c0)
                    , let v' = substNode (NodeId vid)
                    , case IntMap.lookup (getNodeId v') nodes' of
                        Just TyVar {} -> True
                        _ -> False
                    ]

            c1 =
                c0
                    { cNodes = NodeMap nodes'
                    , cBindParents = bindParents'
                    , cInstEdges = instEdges'
                    , cUnifyEdges = unifyEdges'
                    , cEliminatedVars = IntSet.empty
                    , cWeakenedVars = weakened'
                    , cGenNodes = genNodesSubst
                    }

        pure (c1, subst)
  where
    elims0 = cEliminatedVars c0

-- | Read-only UF canonicalization helper for immutable rewrite passes.
frWith :: IntMap NodeId -> NodeId -> NodeId
frWith = UnionFind.frWith

-- | Debug validator for solved-graph invariants. Returns an empty list on success.
validateSolvedGraph :: SolveResult p -> [String]
validateSolvedGraph SolveResult { srConstraint = c, srUnionFind = uf } =
    validateWith ValidateOpts { voCheckInstEdges = False }
        SolveResult { srConstraint = c, srUnionFind = uf }

-- | Strict variant that also requires instantiation edges to be gone.
validateSolvedGraphStrict :: SolveResult p -> [String]
validateSolvedGraphStrict = validateWith ValidateOpts { voCheckInstEdges = True }

data ValidateOpts = ValidateOpts
    { voCheckInstEdges :: Bool
    }

validateWith :: ValidateOpts -> SolveResult p -> [String]
validateWith opts SolveResult { srConstraint = c, srUnionFind = uf } =
    concat
        [ tyExpViolations
        , instEdgeViolations
        , unifyEdgeViolations
        , canonicalViolations
        , childPresenceViolations
        , occursViolations
        ]
  where
    nodes = cNodes c
    nodeList = map snd (toListNode nodes)

    canonical :: NodeId -> NodeId
    canonical = frWith uf

    childRefs :: TyNode -> [NodeId]
    childRefs = structuralChildren

    tyExpViolations =
        [ msg "Unexpected TyExp node" [nid]
        | TyExp { tnId = nid } <- nodeList
        ]

    instEdgeViolations =
        [ msg "Residual instantiation edge" [instLeft e, instRight e]
        | voCheckInstEdges opts
        , e <- cInstEdges c
        ]

    unifyEdgeViolations =
        [ msg "Residual unification edge" [uniLeft e, uniRight e]
        | e <- cUnifyEdges c
        ]

    canonicalViolations =
        concat
            [ [ msg "Node key/id mismatch" [tnId n]
              | n <- nodeList
              , lookupNodeIn nodes (tnId n) /= Just n
              ]
            , [ msg "Non-canonical node id" [tnId n]
              | n <- nodeList
              , let cid = canonical (tnId n)
              , cid /= tnId n
              ]
            , [ msg "Non-canonical child id" [parent, child]
              | n <- nodeList
              , parent <- [tnId n]
              , child <- childRefs n
              , canonical child /= child
              ]
            , [ msg "Non-canonical inst edge" [instLeft e, instRight e]
              | e <- cInstEdges c
              , canonical (instLeft e) /= instLeft e || canonical (instRight e) /= instRight e
              ]
            , [ msg "Non-canonical unify edge" [uniLeft e, uniRight e]
              | e <- cUnifyEdges c
              , canonical (uniLeft e) /= uniLeft e || canonical (uniRight e) /= uniRight e
              ]
            ]

    childPresenceViolations =
        [ msg "Missing child node" [tnId n, child]
        | n <- nodeList
        , child <- childRefs n
        , case lookupNodeIn nodes child of
            Nothing -> True
            Just _ -> False
        ]

    occursViolations =
        [ msg "Occurs-check violation" [varId]
        | var <- [ node | node@TyVar {} <- nodeList ]
        , let varId = tnId var
        , reachesSelf varId
        ]

    reachesSelf :: NodeId -> Bool
    reachesSelf start = go IntSet.empty start
      where
        go seen nid
            | nid == start && not (IntSet.null seen) = True
            | IntSet.member (getNodeId nid) seen = False
            | otherwise = case lookupNodeIn nodes nid of
                Nothing -> False
                Just node ->
                    let seen' = IntSet.insert (getNodeId nid) seen
                        next = map canonical (childRefs node)
                    in any (go seen') next

    msg :: Show a => String -> [a] -> String
    msg label payload = label ++ ": " ++ unwords (map show payload)
