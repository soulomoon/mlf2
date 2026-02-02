{- |
Module      : MLF.Constraint.Unify.Core
Description : Shared unification core with configurable policies
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides a small, policy-driven core for monotype unification
used by multiple phases (e.g. Normalize, Solve). Callers supply the union-find
and lookup operations alongside a 'UnifyStrategy' that captures key policy
choices (occurs-check, TyExp handling, forall arity, representative choice).
-}
module MLF.Constraint.Unify.Core (
    UnifyStrategy(..),
    OccursCheckPolicy(..),
    TyExpPolicy(..),
    ForallArityPolicy(..),
    RepresentativeChoice(..),
    UnifyMismatch(..),
    UnifyMismatchAction(..),
    processUnifyEdgesWith
) where

import Control.Monad (foldM, when)

import MLF.Constraint.Types.Graph
    ( BaseTy
    , ExpVarId
    , NodeId
    , TyNode(..)
    , UnifyEdge(..)
    )
import qualified MLF.Constraint.Unify.Decompose as UnifyDecompose

-- | Policy for occurs-check handling when unifying a variable with structure.
data OccursCheckPolicy m
    = OccursCheckNone
    | OccursCheck (NodeId -> NodeId -> m ())

-- | Policy for TyExp handling during unification.
data TyExpPolicy
    = TyExpAllow
    | TyExpReject
    deriving (Eq, Show)

-- | Policy for forall arity handling.
data ForallArityPolicy m
    = ForallArityIgnore
    | ForallArityCheck (NodeId -> m (Maybe Int))

-- | Policy for choosing the union-find representative.
newtype RepresentativeChoice m = RepresentativeChoice
    { chooseRepresentative :: NodeId -> TyNode -> NodeId -> TyNode -> m (NodeId, NodeId)
    }

-- | Strategy bundle that parameterizes the unification core.
data UnifyStrategy m = UnifyStrategy
    { usOccursCheck :: OccursCheckPolicy m
    , usTyExpPolicy :: TyExpPolicy
    , usForallArityPolicy :: ForallArityPolicy m
    , usRepresentative :: RepresentativeChoice m
    , usOnMismatch :: UnifyMismatch -> m UnifyMismatchAction
    }

-- | Mismatch reasons surfaced by the unification core.
data UnifyMismatch
    = MismatchMissingNode NodeId
    | MismatchConstructor TyNode TyNode
    | MismatchBase BaseTy BaseTy
    | MismatchForallArity (Maybe Int) (Maybe Int)
    | MismatchExpVar ExpVarId ExpVarId
    | MismatchUnexpectedExp NodeId
    deriving (Eq, Show)

-- | Actions for mismatches.
data UnifyMismatchAction
    = KeepEdge
    | DropEdge
    deriving (Eq, Show)

-- | Process unification edges using the supplied accessors and strategy.
processUnifyEdgesWith
    :: Monad m
    => UnifyStrategy m
    -> (NodeId -> m NodeId)
    -> (NodeId -> m (Maybe TyNode))
    -> (NodeId -> NodeId -> m ())
    -> [UnifyEdge]
    -> m [UnifyEdge]
processUnifyEdgesWith strategy findRoot lookupNode unionNodes = foldM processOne []
  where
    processOne acc edge = do
        leftRoot <- findRoot (uniLeft edge)
        rightRoot <- findRoot (uniRight edge)
        if leftRoot == rightRoot
            then pure acc
            else do
                leftNode <- lookupNode leftRoot
                rightNode <- lookupNode rightRoot
                case (leftNode, rightNode) of
                    (Nothing, _) -> mismatch (MismatchMissingNode leftRoot) acc edge
                    (_, Nothing) -> mismatch (MismatchMissingNode rightRoot) acc edge
                    (Just ln, Just rn) ->
                        unifyNodes acc edge leftRoot ln rightRoot rn

    unifyNodes acc edge leftRoot leftNode rightRoot rightNode = do
        let hasExp = isTyExp leftNode || isTyExp rightNode
        case usTyExpPolicy strategy of
            TyExpReject
                | hasExp -> mismatch (MismatchUnexpectedExp (pickExpRoot leftRoot leftNode rightRoot rightNode)) acc edge
            _ -> unifyNodesAllowed acc edge leftRoot leftNode rightRoot rightNode

    unifyNodesAllowed acc edge leftRoot leftNode rightRoot rightNode =
        case (leftNode, rightNode) of
            (TyVar{}, TyVar{}) -> do
                unionWithRep leftRoot leftNode rightRoot rightNode
                pure acc

            (TyVar{}, _) -> do
                applyOccursCheck leftRoot rightRoot
                unionWithRep leftRoot leftNode rightRoot rightNode
                pure acc

            (_, TyVar{}) -> do
                applyOccursCheck rightRoot leftRoot
                unionWithRep leftRoot leftNode rightRoot rightNode
                pure acc

            (TyForall { tnBody = b1 }, TyForall { tnBody = b2 }) -> do
                case usForallArityPolicy strategy of
                    ForallArityIgnore -> do
                        unionWithRep leftRoot leftNode rightRoot rightNode
                        pure (prependEdges [UnifyEdge b1 b2] acc)
                    ForallArityCheck arityOf -> do
                        leftArity <- arityOf leftRoot
                        rightArity <- arityOf rightRoot
                        case (leftArity, rightArity) of
                            (Just k1, Just k2)
                                | k1 == k2 -> do
                                    unionWithRep leftRoot leftNode rightRoot rightNode
                                    pure (prependEdges [UnifyEdge b1 b2] acc)
                            _ -> mismatch (MismatchForallArity leftArity rightArity) acc edge

            _ ->
                case UnifyDecompose.decomposeUnifyChildren leftNode rightNode of
                    Right newEdges -> do
                        unionWithRep leftRoot leftNode rightRoot rightNode
                        pure (insertEdges leftNode newEdges acc)
                    Left (UnifyDecompose.MismatchBase b1 b2) ->
                        mismatch (MismatchBase b1 b2) acc edge
                    Left (UnifyDecompose.MismatchExpVar s1 s2) ->
                        mismatch (MismatchExpVar s1 s2) acc edge
                    Left UnifyDecompose.MismatchConstructor ->
                        mismatch (MismatchConstructor leftNode rightNode) acc edge

    unionWithRep leftRoot leftNode rightRoot rightNode = do
        let RepresentativeChoice choose = usRepresentative strategy
        (from, to) <- choose leftRoot leftNode rightRoot rightNode
        when (from /= to) (unionNodes from to)

    applyOccursCheck varRoot targetRoot = case usOccursCheck strategy of
        OccursCheckNone -> pure ()
        OccursCheck checkFn -> checkFn varRoot targetRoot

    mismatch reason acc edge = do
        action <- usOnMismatch strategy reason
        case action of
            KeepEdge -> pure (edge : acc)
            DropEdge -> pure acc

    isTyExp node = case node of
        TyExp{} -> True
        _ -> False

    pickExpRoot leftRoot leftNode rightRoot rightNode =
        case leftNode of
            TyExp{} -> leftRoot
            _ -> case rightNode of
                TyExp{} -> rightRoot
                _ -> leftRoot

    insertEdges node newEdges acc = case node of
        TyArrow{} -> acc ++ newEdges
        TyExp{} -> newEdges ++ acc
        TyForall{} -> newEdges ++ acc
        _ -> acc

    prependEdges newEdges acc = newEdges ++ acc
