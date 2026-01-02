module MLF.Elab.Generalize (
    generalizeAt,
    generalizeAtKeepTarget
) where

import Control.Monad (unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Util (topoSortBy)
import MLF.Elab.Reify (freeVars, reifyBoundWithNames, reifyTypeWithNamesNoFallback)
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import qualified MLF.Binding.Tree as Binding

-- | Generalize a node at the given binding site into a polymorphic scheme.
-- For xMLF, quantified variables can have bounds.
-- Returns the scheme and the substitution used to rename variables.
generalizeAt :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAt = generalizeAtWith True

-- | Variant of 'generalizeAt' that keeps the target binder even when it would
-- normally be dropped as an alias wrapper.
generalizeAtKeepTarget :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtKeepTarget = generalizeAtWith False

generalizeAtWith :: Bool -> SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtWith allowDropTarget res scopeRoot targetNode = do
    let constraint = srConstraint res
        nodes = cNodes constraint
        uf = srUnionFind res
        canonical = Solve.frWith uf
        scopeRootC = case scopeRoot of
            TypeRef nid -> TypeRef (canonical nid)
            GenRef gid -> GenRef gid
    let target0 =
            case IntMap.lookup (getNodeId (canonical targetNode)) nodes of
                Just TyExp{ tnBody = b } -> canonical b
                _ -> canonical targetNode
    let (binderRoot, orderRoot, typeRoot0) =
            case scopeRootC of
                GenRef _ ->
                    case IntMap.lookup (getNodeId target0) nodes of
                        Just TyForall{ tnBody = b } ->
                            let bodyRoot = canonical b
                            in (TypeRef target0, bodyRoot, bodyRoot)
                        _ -> (scopeRootC, target0, target0)
                TypeRef scopeRootN ->
                    case IntMap.lookup (getNodeId target0) nodes of
                        Just TyForall{ tnBody = b } ->
                            let bodyRoot = canonical b
                            in if scopeRootN == target0
                                then (scopeRootC, bodyRoot, bodyRoot)
                                else (scopeRootC, bodyRoot, target0)
                        _ -> (scopeRootC, target0, target0)
    let reachableWithBounds root0 =
            Right (reachableFromWithBounds root0)

        reachableFromWithBounds root0 =
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
            in go IntSet.empty [canonical root0]

    reachable <- reachableWithBounds orderRoot
    scopeGen <- case scopeRootC of
        GenRef gid -> pure (Just gid)
        TypeRef nid -> do
            path <- bindingToElab (Binding.bindingPathToRoot constraint (TypeRef nid))
            pure (listToMaybe [gid | GenRef gid <- drop 1 path])
    bindParents <- bindingToElab (Binding.canonicalizeBindParentsUnder canonical constraint)
    let scopeTypeKeys =
            case scopeGen of
                Nothing -> Nothing
                Just gid ->
                    let childrenByParent =
                            foldl'
                                (\m (childKey, (parent, _flag)) ->
                                    let parentKey = nodeRefKey parent
                                    in IntMap.insertWith IntSet.union parentKey (IntSet.singleton childKey) m
                                )
                                IntMap.empty
                                (IntMap.toList bindParents)
                        go visited [] = visited
                        go visited (k:ks) =
                            let kids = IntSet.toList (IntMap.findWithDefault IntSet.empty k childrenByParent)
                                typeKids =
                                    [ kid
                                    | kid <- kids
                                    , case nodeRefFromKey kid of
                                        TypeRef _ -> True
                                        GenRef _ -> False
                                    ]
                                newTypeKids = filter (\kid -> not (IntSet.member kid visited)) typeKids
                                visited' = foldl' (flip IntSet.insert) visited newTypeKids
                            in go visited' (newTypeKids ++ ks)
                    in Just (go IntSet.empty [nodeRefKey (genRef gid)])
    binders0 <- do
        let childrenByParent =
                foldl'
                    (\m (childKey, (parent, _flag)) ->
                        let parentKey = nodeRefKey parent
                        in IntMap.insertWith IntSet.union parentKey (IntSet.singleton childKey) m
                    )
                    IntMap.empty
                    (IntMap.toList bindParents)
            bindFlags =
                IntMap.fromList
                    [ (childKey, flag)
                    | (childKey, (_parent, flag)) <- IntMap.toList bindParents
                    ]

            startKey = nodeRefKey binderRoot

            walk visited [] = visited
            walk visited (k:ks) =
                let kids = IntMap.findWithDefault IntSet.empty k childrenByParent
                    newKids = filter (\kid -> not (IntSet.member kid visited)) (IntSet.toList kids)
                    visited' = foldl' (flip IntSet.insert) visited newKids
                in walk visited' (newKids ++ ks)

            flexKeys = walk (IntSet.singleton startKey) [startKey]

            isQuantifiable child =
                case IntMap.lookup (getNodeId child) nodes of
                    Just TyExp{} -> False
                    Just TyBase{} -> False
                    Just TyBottom{} -> False
                    Just _ -> True
                    Nothing -> False
            isBindable key child =
                case IntMap.lookup key bindFlags of
                    Just BindFlex -> isQuantifiable child
                    _ -> False

        pure
            [ canonical child
            | key <- IntSet.toList flexKeys
            , TypeRef child <- [nodeRefFromKey key]
            , isBindable key child
            ]
    let scopeSchemeRoots =
            case scopeGen of
                Nothing -> IntSet.empty
                Just gid ->
                    case IntMap.lookup (getGenNodeId gid) (cGenNodes constraint) of
                        Nothing -> IntSet.empty
                        Just gen ->
                            IntSet.fromList (map (getNodeId . canonical) (gnSchemes gen))
        schemeRootsToSkip =
            IntSet.difference
                (IntSet.fromList
                    [ getNodeId (canonical root)
                    | gen <- IntMap.elems (cGenNodes constraint)
                    , let gid = gnId gen
                    , Just gid /= scopeGen
                    , root <- gnSchemes gen
                    ]
                )
                scopeSchemeRoots
        isNestedSchemeBound v =
            case IntMap.lookup (getNodeId (canonical v)) nodes of
                Just TyVar{ tnBound = Just bnd } ->
                    IntSet.member (getNodeId (canonical bnd)) schemeRootsToSkip
                _ -> False
        targetBound =
            case IntMap.lookup (getNodeId target0) nodes of
                Just TyVar{ tnBound = Just bnd } -> Just (canonical bnd)
                _ -> Nothing
        boundIsSchemeRoot =
            case (scopeRootC, targetBound) of
                (GenRef gid, Just bnd) ->
                    case IntMap.lookup (getGenNodeId gid) (cGenNodes constraint) of
                        Nothing -> False
                        Just gen ->
                            IntSet.member
                                (getNodeId (canonical bnd))
                                (IntSet.fromList (map (getNodeId . canonical) (gnSchemes gen)))
                _ -> False
        boundIsVar =
            case targetBound >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                Just TyVar{} -> True
                _ -> False
        boundIsChild =
            case targetBound of
                Just bnd ->
                    let bndC = canonical bnd
                        quantifiable =
                            case IntMap.lookup (getNodeId bndC) nodes of
                                Just TyExp{} -> False
                                Just TyBase{} -> False
                                Just TyBottom{} -> False
                                Just _ -> True
                                Nothing -> False
                    in quantifiable
                        && case IntMap.lookup (nodeRefKey (typeRef bndC)) bindParents of
                            Just (parentRef, _) -> parentRef == typeRef target0
                            Nothing -> False
                Nothing -> False
    let dropTarget =
            allowDropTarget &&
            case IntMap.lookup (getNodeId target0) nodes of
                Just TyVar{} -> boundIsVar || boundIsChild
                Just _ -> True
                Nothing -> False
        schemeRoots =
            case scopeRootC of
                GenRef _ | dropTarget -> IntSet.singleton (getNodeId (canonical target0))
                _ -> IntSet.empty
        typeRoot =
            case (dropTarget, targetBound) of
                (True, Just bnd) -> bnd
                _ -> typeRoot0
    let dropTypeRoot =
            dropTarget &&
            boundIsSchemeRoot &&
            case IntMap.lookup (getNodeId (canonical typeRoot)) nodes of
                Just TyVar{} -> False
                _ -> True
    let binders =
            [ canonical v
            | v <- binders0
            , IntSet.member (getNodeId (canonical v)) reachable
            , case scopeTypeKeys of
                Nothing -> True
                Just keys -> IntSet.member (nodeRefKey (typeRef (canonical v))) keys
            , not (IntSet.member (getNodeId (canonical v)) schemeRootsToSkip)
            , not (isNestedSchemeBound v)
            , not (IntSet.member (getNodeId (canonical v)) schemeRoots)
            , not (dropTypeRoot && canonical v == canonical typeRoot)
            ]
        bindersCanon =
            IntMap.elems $
                IntMap.fromList
                    [ (getNodeId v, v)
                    | v <- binders
                    ]

    let binderIds = map getNodeId bindersCanon

    ordered0 <- orderBinderCandidates canonical constraint orderRoot binderIds
    let names = zipWith alphaName [0..] ordered0
        subst = IntMap.fromList (zip ordered0 names)

    bindings <- mapM
        (\(name, nidInt) -> do
            let bNodeC = canonical (NodeId nidInt)
            boundTy <- reifyBoundWithNames res subst bNodeC
            let mbBound = if boundTy == TBottom then Nothing else Just boundTy
            pure (name, mbBound)
        )
        (zip names ordered0)

    ty0 <- reifyTypeWithNamesNoFallback res subst typeRoot
    pure (Forall bindings ty0, subst)
  where
    orderBinderCandidates
        :: (NodeId -> NodeId)
        -> Constraint
        -> NodeId
        -> [Int]
        -> Either ElabError [Int]
    orderBinderCandidates canonical' constraint' root candidates = do
        let keys = Order.orderKeysFromConstraintWith canonical' constraint' root Nothing
            candidateSet = IntSet.fromList candidates
            missing =
                [ k
                | k <- candidates
                , not (IntMap.member k keys)
                ]
            depsFor k =
                [ d
                | d <- IntSet.toList (freeVars res (NodeId k) IntSet.empty)
                , IntSet.member d candidateSet
                , d /= k
                ]
            cmpReady a b =
                case Order.compareNodesByOrderKey keys (NodeId a) (NodeId b) of
                    EQ -> compare a b
                    other -> other

        unless (null missing) $
            Left $
                InstantiationError $
                    "generalizeAt: missing order keys for " ++ show (map NodeId missing)

        topoSortBy
            "generalizeAt: cycle in binder bound dependencies"
            cmpReady
            depsFor
            candidates

alphaName :: Int -> Int -> String
alphaName idx _ = letters !! (idx `mod` length letters) ++ suffix
  where
    letters = map (:[]) ['a'..'z']
    suffix = if idx < length letters then "" else show (idx `div` length letters)
