module MLF.Elab.Generalize (
    generalizeAt
) where

import Data.List (nub)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Util (topoSortBy)
import MLF.Elab.Reify (freeVars, reifyType, reifyTypeWithNames)
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Binding.Tree (orderedBinders)
import qualified MLF.Constraint.VarStore as VarStore

-- | Generalize a node at the given binding site into a polymorphic scheme.
-- For xMLF, quantified variables can have bounds.
-- Returns the scheme and the substitution used to rename variables.
generalizeAt :: SolveResult -> NodeId -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAt res scopeRoot nid = do
    let constraint = srConstraint res
        nodes = cNodes constraint
        uf = srUnionFind res
        canonical = Solve.frWith uf
        scopeRootC = canonical scopeRoot
    let target0 = case IntMap.lookup (getNodeId (canonical nid)) nodes of
            Just TyExp{ tnBody = b } -> canonical b
            _ -> canonical nid
        orderRoot = case IntMap.lookup (getNodeId target0) nodes of
            Just TyForall{ tnBody = b } -> canonical b
            _ -> target0

    binders0 <- bindingToElab (orderedBinders canonical constraint scopeRootC)
    -- Paper-style binder enumeration uses Q(n) (direct flex children).
    --
    -- Top-level generalization is not a paper construct; it exists to give a
    -- principal scheme to the whole program. When the binding tree does not
    -- expose free vars as direct children of the root (common in our multi-root
    -- term-DAG graphs), fall back to HM-style free-variable generalization.
    bindersFallback <-
        if null binders0
            then do
                ty0 <- reifyType res orderRoot
                pure (freeNodeIdsFromType ty0)
            else pure binders0

    let binders =
            [ v
            | v <- bindersFallback
            , not (VarStore.isEliminatedVar constraint v)
            ]
        grouped =
            IntMap.fromList
                [ (getNodeId v, (v, fmap canonical (VarStore.lookupVarBound constraint v)))
                | v <- binders
                ]

    ordered <- orderBinderCandidates orderRoot grouped
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

    ty <- reifyTypeWithNames res subst orderRoot
    pure (Forall bindings ty, subst)
  where
    orderBinderCandidates :: NodeId -> IntMap.IntMap (NodeId, Maybe NodeId) -> Either ElabError [Int]
    orderBinderCandidates root grouped = do
        let keys = Order.orderKeysFromRoot res root
            candidates = IntMap.keys grouped
            candidateSet = IntSet.fromList candidates
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

        topoSortBy
            "generalizeAt: cycle in binder bound dependencies"
            cmpReady
            depsFor
            candidates

    freeNodeIdsFromType :: ElabType -> [NodeId]
    freeNodeIdsFromType ty0 =
        let names = nub (freeTypeVarNames [] ty0)
        in nub (mapMaybe nameToNodeId names)

    freeTypeVarNames :: [String] -> ElabType -> [String]
    freeTypeVarNames bound ty0 = case ty0 of
        TVar v -> if v `elem` bound then [] else [v]
        TArrow a b -> freeTypeVarNames bound a ++ freeTypeVarNames bound b
        TBase _ -> []
        TBottom -> []
        TForall v mb body ->
            let fvBound = maybe [] (freeTypeVarNames bound) mb
                fvBody = freeTypeVarNames (v : bound) body
            in fvBound ++ fvBody

    nameToNodeId :: String -> Maybe NodeId
    nameToNodeId ('t':rest) = NodeId <$> readMaybe rest
    nameToNodeId _ = Nothing

alphaName :: Int -> Int -> String
alphaName idx _ = letters !! (idx `mod` length letters) ++ suffix
  where
    letters = map (:[]) ['a'..'z']
    suffix = if idx < length letters then "" else show (idx `div` length letters)
