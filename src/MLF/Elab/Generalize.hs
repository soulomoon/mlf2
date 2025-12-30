module MLF.Elab.Generalize (
    generalizeAt
) where

import Control.Monad (foldM, unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Util (topoSortBy)
import MLF.Elab.Reify (freeVars, reifyTypeWithNames)
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.VarStore as VarStore

-- | Generalize a node at the given binding site into a polymorphic scheme.
-- For xMLF, quantified variables can have bounds.
-- Returns the scheme and the substitution used to rename variables.
generalizeAt :: SolveResult -> NodeId -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAt res scopeRoot targetNode = do
    let constraint = srConstraint res
        nodes = cNodes constraint
        uf = srUnionFind res
        canonical = Solve.frWith uf
        scopeRootC = canonical scopeRoot
    let target0 = case IntMap.lookup (getNodeId (canonical targetNode)) nodes of
            Just TyExp{ tnBody = b } -> canonical b
            _ -> canonical targetNode
        (orderRoot, typeRoot) =
            case IntMap.lookup (getNodeId target0) nodes of
                Just TyForall{ tnBody = b } ->
                    let bodyRoot = canonical b
                    in if scopeRootC == target0
                        then (bodyRoot, bodyRoot)
                        else (bodyRoot, target0)
                _ -> (target0, target0)

    let reachableWithBounds root0 = go IntSet.empty [root0]
          where
            lookupNode nid = IntMap.lookup (getNodeId nid) nodes
            boundChildren nid =
                case lookupNode nid of
                    Just TyVar{} ->
                        maybe [] (: []) (VarStore.lookupVarBound constraint nid)
                    _ -> []

            go visited [] = Right visited
            go visited (nid0 : rest) = do
                let nid = canonical nid0
                    key = getNodeId nid
                if IntSet.member key visited
                    then go visited rest
                    else do
                        let visited' = IntSet.insert key visited
                            children =
                                case lookupNode nid of
                                    Nothing -> []
                                    Just node -> map canonical (structuralChildren node)
                            extras = map canonical (boundChildren nid)
                        go visited' (children ++ extras ++ rest)

    reachable <- reachableWithBounds orderRoot
    reachableType <- reachableWithBounds typeRoot

    let isBinderNode nid =
            case IntMap.lookup (getNodeId nid) nodes of
                Just TyForall{} -> True
                Just TyRoot{} -> True
                _ -> False
        boundAtScope v0 = go True IntSet.empty (canonical v0)
          where
            go isFirst visited nid =
                let key = getNodeId nid
                in if IntSet.member key visited
                    then Left $
                        BindingTreeError $
                            InvalidBindingTree "generalizeAt: cycle in binding path"
                    else do
                        mbParent <- bindingToElab (Binding.lookupBindParentUnder canonical constraint nid)
                        case mbParent of
                            Nothing -> Right False
                            Just (parent, flag) ->
                                if isFirst && flag == BindRigid
                                    then Right False
                                    else if isBinderNode parent
                                        then case IntMap.lookup (getNodeId parent) nodes of
                                            Just TyRoot{} ->
                                                Right (canonical parent == scopeRootC)
                                            Just TyForall{} ->
                                                if IntSet.member (getNodeId (canonical parent)) reachableType
                                                    then Right (canonical parent == scopeRootC)
                                                    else go False (IntSet.insert key visited) (canonical parent)
                                            _ ->
                                                Right False
                                        else go False (IntSet.insert key visited) (canonical parent)
    let scopeIsForall =
            case IntMap.lookup (getNodeId scopeRootC) nodes of
                Just TyForall{} -> True
                _ -> False
    binders0 <-
        if scopeIsForall
            then bindingToElab (Binding.boundFlexChildrenUnder canonical constraint scopeRootC)
            else do
                let varCandidates =
                        [ NodeId nid
                        | (nid, TyVar{}) <- IntMap.toList nodes
                        ]
                foldM
                    (\acc v -> do
                        ok <- boundAtScope v
                        pure (if ok then v : acc else acc)
                    )
                    []
                    varCandidates
    let binders =
            [ canonical v
            | v <- binders0
            , IntSet.member (getNodeId (canonical v)) reachable
            ]
        bindersCanon =
            IntMap.elems $
                IntMap.fromList
                    [ (getNodeId v, v)
                    | v <- binders
                    ]

    let grouped =
            IntMap.fromList
                [ (getNodeId v, (v, fmap canonical (VarStore.lookupVarBound constraint v)))
                | v <- bindersCanon
                ]

    ordered <- orderBinderCandidates canonical constraint orderRoot grouped
    let names = zipWith alphaName [0..] ordered
        subst = IntMap.fromList (zip ordered names)
        binderSet = IntSet.fromList ordered

    bindings <- mapM (\(name, nidInt) -> do
            let (_, mbBoundNode) = grouped IntMap.! nidInt
            boundTy <- case mbBoundNode of
                Nothing -> pure Nothing
                Just bNode -> do
                    bNodeC <- pure (canonical bNode)
                    -- Reification nicety: omit “v ⩾ ⊥” bounds. In our constraint
                    -- graph, ⊥ is often represented by an unconstrained fresh `TyVar`
                    -- that is not itself quantified at this site; printing it as a
                    -- bound produces spurious `∀(a ⩾ tN)` in simple programs (e.g. `id id`).
                    if IntSet.member (getNodeId bNodeC) binderSet
                        then Just <$> reifyTypeWithNames res subst bNodeC
                        else case IntMap.lookup (getNodeId bNodeC) nodes of
                            Just TyVar{} ->
                                case VarStore.lookupVarBound constraint bNodeC of
                                    Nothing -> pure Nothing
                                    Just _ -> Just <$> reifyTypeWithNames res subst bNodeC
                            _ ->
                                Just <$> reifyTypeWithNames res subst bNodeC
            pure (name, boundTy)
        ) (zip names ordered)

    ty0 <- reifyTypeWithNames res subst typeRoot
    let ty = stripVacuousForalls ty0
        usedNames = usedBinderNames bindings ty
        bindings' = filter (\(name, _) -> Set.member name usedNames) bindings
    pure (Forall bindings' ty, subst)
  where
    orderBinderCandidates
        :: (NodeId -> NodeId)
        -> Constraint
        -> NodeId
        -> IntMap.IntMap (NodeId, Maybe NodeId)
        -> Either ElabError [Int]
    orderBinderCandidates canonical' constraint' root grouped = do
        let keys = Order.orderKeysFromConstraintWith canonical' constraint' root Nothing
            candidates = IntMap.keys grouped
            candidateSet = IntSet.fromList candidates
            missing =
                [ k
                | k <- candidates
                , not (IntMap.member k keys)
                ]
            depsFor k =
                case snd (grouped IntMap.! k) of
                    Nothing -> []
                    Just bnd ->
                        [ d
                        | d <- IntSet.toList (freeVars res bnd IntSet.empty)
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

    usedBinderNames :: [(String, Maybe ElabType)] -> ElabType -> Set.Set String
    usedBinderNames binds body =
        let needed0 = freeTypeVars body
            depsFor name =
                case lookup name binds of
                    Nothing -> Set.empty
                    Just Nothing -> Set.empty
                    Just (Just bnd) -> freeTypeVars bnd
            close s =
                let s' = foldl'
                        (\acc name -> Set.union acc (depsFor name))
                        s
                        (Set.toList s)
                in if s' == s then s else close s'
        in close needed0

    freeTypeVars :: ElabType -> Set.Set String
    freeTypeVars = go Set.empty
      where
        go bound ty0 = case ty0 of
            TVar v -> if Set.member v bound then Set.empty else Set.singleton v
            TArrow a b -> go bound a `Set.union` go bound b
            TBase _ -> Set.empty
            TBottom -> Set.empty
            TForall v mb body ->
                let fvBound = maybe Set.empty (go bound) mb
                    fvBody = go (Set.insert v bound) body
                in fvBound `Set.union` fvBody

    stripVacuousForalls :: ElabType -> ElabType
    stripVacuousForalls ty0 = case ty0 of
        TArrow a b -> TArrow (stripVacuousForalls a) (stripVacuousForalls b)
        TForall v mb body ->
            let mb' = fmap stripVacuousForalls mb
                body' = stripVacuousForalls body
                appearsInBody = Set.member v (freeTypeVars body')
                appearsInBound = maybe False (Set.member v . freeTypeVars) mb'
            in if appearsInBody || appearsInBound
                then TForall v mb' body'
                else body'
        TVar{} -> ty0
        TBase{} -> ty0
        TBottom -> ty0

alphaName :: Int -> Int -> String
alphaName idx _ = letters !! (idx `mod` length letters) ++ suffix
  where
    letters = map (:[]) ['a'..'z']
    suffix = if idx < length letters then "" else show (idx `div` length letters)
