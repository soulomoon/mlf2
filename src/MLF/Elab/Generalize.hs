module MLF.Elab.Generalize (
    generalizeAt
) where

import Control.Monad (unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Util (topoSortBy)
import MLF.Elab.Reify (freeVars, reifyBoundWithNames, reifyTypeWithNames)
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import qualified MLF.Binding.Tree as Binding

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

    binders0 <- bindingToElab (Binding.boundFlexChildrenAllUnder canonical constraint scopeRootC)
    let eliminated = cEliminatedVars constraint
        binders =
            [ canonical v
            | v <- binders0
            , not (IntSet.member (getNodeId (canonical v)) eliminated)
            , IntSet.member (getNodeId (canonical v)) reachable
            ]
        bindersCanon =
            IntMap.elems $
                IntMap.fromList
                    [ (getNodeId v, v)
                    | v <- binders
                    ]

    let binderIds = map getNodeId bindersCanon

    ordered <- orderBinderCandidates canonical constraint orderRoot binderIds
    let names = zipWith alphaName [0..] ordered
        subst = IntMap.fromList (zip ordered names)

    bindings <- mapM
        (\(name, nidInt) -> do
            let bNodeC = canonical (NodeId nidInt)
            boundTy <- reifyBoundWithNames res subst bNodeC
            let mbBound = if boundTy == TBottom then Nothing else Just boundTy
            pure (name, mbBound)
        )
        (zip names ordered)

    ty0 <- reifyTypeWithNames res subst typeRoot
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
