{- |
Module      : MLF.Constraint.Unify.Closure
Description : Shared unification-closure engine
Copyright   : (c) 2024
License     : BSD-3-Clause

Runs the solve-phase unification worklist and returns a pre-rewrite snapshot:
the queue is drained and UF classes are computed, but no rewrite/elimination
passes are applied to the graph.
-}
module MLF.Constraint.Unify.Closure (
    SolveError(..),
    UnifyClosureResult(..),
    runUnifyClosure,
    runUnifyClosureWithSeed,
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT, execStateT, gets, modify')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Adjustment as BindingAdjustment
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.Unify.Core as UnifyCore
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Types hiding (lookupNode)
import MLF.Util.Trace (TraceConfig, traceBinding)
import qualified MLF.Util.UnionFind as UnionFind

-- | Errors that can arise during monotype unification.
data SolveError
    = MissingNode NodeId
    | ConstructorClash TyNode TyNode
    | BaseClash BaseTy BaseTy
    | ForallArityMismatch Int Int
    | OccursCheckFailed NodeId NodeId   -- ^ (var, target)
    | UnexpectedExpNode NodeId
    | BindingTreeError BindingError
    | ValidationFailed [String]         -- ^ Post-solve validation failures
    | TyConClash BaseTy BaseTy          -- ^ TyCon head mismatch
    | TyConArityMismatch BaseTy Int Int -- ^ TyCon arity mismatch (head, arity1, arity2)
    deriving (Eq, Show)

-- | Result of draining unification edges without rewrite/elimination passes.
data UnifyClosureResult = UnifyClosureResult
    { ucConstraint :: Constraint
    , ucUnionFind :: IntMap NodeId
    }
    deriving (Eq, Show)

data SolveState = SolveState
    { suConstraint :: Constraint
    , suUnionFind :: IntMap NodeId
    , suQueue :: [UnifyEdge]
    }

type SolveM = StateT SolveState (Either SolveError)

runUnifyClosure :: TraceConfig -> Constraint -> Either SolveError UnifyClosureResult
runUnifyClosure traceCfg =
    runUnifyClosureWithSeed traceCfg IntMap.empty

runUnifyClosureWithSeed
    :: TraceConfig
    -> IntMap NodeId
    -> Constraint
    -> Either SolveError UnifyClosureResult
runUnifyClosureWithSeed traceCfg ufSeed c0 = do
    let debugSolveBinding = traceBinding traceCfg
        probeIds = [NodeId 2, NodeId 3]
        probeInfo =
            [ ( pid
              , NodeAccess.lookupNode c0 pid
              , IntMap.lookup (nodeRefKey (typeRef pid)) (cBindParents c0)
              )
            | pid <- probeIds
            ]
    case debugSolveBinding ("runUnifyClosure: pre-check probe " ++ show probeInfo) () of
        () -> pure ()
    case Binding.checkBindingTree c0 of
        Left err -> Left (BindingTreeError err)
        Right () -> do
            let st = SolveState { suConstraint = c0, suUnionFind = ufSeed, suQueue = cUnifyEdges c0 }
            final <- execStateT (loop >> batchHarmonize) st
            let preRewrite = (suConstraint final) { cUnifyEdges = [] }
                probeInfo' =
                    [ ( pid
                      , NodeAccess.lookupNode preRewrite pid
                      , IntMap.lookup (nodeRefKey (typeRef pid)) (cBindParents preRewrite)
                      )
                    | pid <- probeIds
                    ]
            case Binding.checkBindingTree preRewrite of
                Left err -> Left (BindingTreeError err)
                Right () -> do
                    case debugSolveBinding ("runUnifyClosure: post-check probe " ++ show probeInfo') () of
                        () -> pure ()
                    pure
                        UnifyClosureResult
                            { ucConstraint = preRewrite
                            , ucUnionFind = suUnionFind final
                            }
  where
    loop :: SolveM ()
    loop = do
        q <- gets suQueue
        case q of
            [] -> pure ()
            (e : rest) -> do
                modify' $ \s -> s { suQueue = rest }
                processEdge e
                loop

    enqueue :: [UnifyEdge] -> SolveM ()
    enqueue es = modify' $ \s -> s { suQueue = es ++ suQueue s }

    equivalenceClasses :: IntMap NodeId -> Constraint -> [[NodeId]]
    equivalenceClasses uf c =
        let canonical = UnionFind.frWith uf
            allNodeIds = map fst (toListNode (cNodes c))
            grouped =
                IntMap.toList $
                    foldl'
                        (\acc nid ->
                            let rep = canonical nid
                            in IntMap.insertWith (++) (getNodeId rep) [nid] acc
                        )
                        IntMap.empty
                        allNodeIds
        in [ members | (_, members) <- grouped, length members > 1 ]

    batchHarmonize :: SolveM ()
    batchHarmonize = do
        uf <- gets suUnionFind
        c <- gets suConstraint
        mapM_ harmonizeClass (equivalenceClasses uf c)

    harmonizeClass :: [NodeId] -> SolveM ()
    harmonizeClass members = do
        cBefore <- gets suConstraint
        let refs = map typeRef members
        case BindingAdjustment.harmonizeBindParentsMulti refs cBefore of
            Left err -> throwError (BindingTreeError err)
            Right (c', _trace) -> modify' $ \s -> s { suConstraint = c' }

    processEdge :: UnifyEdge -> SolveM ()
    processEdge (UnifyEdge l r) = do
        lRoot <- findRoot l
        rRoot <- findRoot r
        if lRoot == rRoot
            then pure ()
            else do
                lNode <- lookupNode lRoot
                rNode <- lookupNode rRoot
                let boundEdges = boundEdgesFor lRoot lNode rRoot rNode
                newEdges <-
                    UnifyCore.processUnifyEdgesWith
                        solveUnifyStrategy
                        findRoot
                        lookupNodeMaybe
                        unionNodes
                        [UnifyEdge lRoot rRoot]
                enqueue (boundEdges ++ newEdges)

    boundEdgesFor :: NodeId -> TyNode -> NodeId -> TyNode -> [UnifyEdge]
    boundEdgesFor lRoot lNode rRoot rNode = case (lNode, rNode) of
        (TyVar{ tnBound = mb1 }, TyVar{ tnBound = mb2 }) ->
            case (mb1, mb2) of
                (Just b1, Just b2)
                    | b1 /= b2 -> [UnifyEdge b1 b2]
                _ -> []
        (TyVar{ tnBound = mb1 }, _) ->
            case mb1 of
                Just b1 | b1 /= rRoot -> [UnifyEdge b1 rRoot]
                _ -> []
        (_, TyVar{ tnBound = mb2 }) ->
            case mb2 of
                Just b2 | b2 /= lRoot -> [UnifyEdge b2 lRoot]
                _ -> []
        _ -> []

    solveUnifyStrategy :: UnifyCore.UnifyStrategy SolveM
    solveUnifyStrategy = UnifyCore.UnifyStrategy
        { UnifyCore.usOccursCheck = UnifyCore.OccursCheck occursCheck
        , UnifyCore.usTyExpPolicy = UnifyCore.TyExpReject
        , UnifyCore.usForallArityPolicy = UnifyCore.ForallArityCheck solveForallArity
        , UnifyCore.usRepresentative = UnifyCore.RepresentativeChoice solveRepresentative
        , UnifyCore.usOnMismatch = solveOnMismatch
        }

    solveForallArity :: NodeId -> SolveM (Maybe Int)
    solveForallArity nid = do
        cCur <- gets suConstraint
        uf0 <- gets suUnionFind
        let canonical = UnionFind.frWith uf0
        case Binding.orderedBinders canonical cCur (typeRef nid) of
            Right bs -> pure (Just (length bs))
            Left err -> throwError (BindingTreeError err)

    solveRepresentative :: NodeId -> TyNode -> NodeId -> TyNode -> SolveM (NodeId, NodeId)
    solveRepresentative left leftNode right rightNode = do
        cCur <- gets suConstraint
        let leftElim = VarStore.isEliminatedVar cCur left
            rightElim = VarStore.isEliminatedVar cCur right
            isVarLeft = case leftNode of
                TyVar{} -> True
                _ -> False
            isVarRight = case rightNode of
                TyVar{} -> True
                _ -> False
            hasBoundLeft = case leftNode of
                TyVar{ tnBound = Just _ } -> True
                _ -> False
            hasBoundRight = case rightNode of
                TyVar{ tnBound = Just _ } -> True
                _ -> False
            (from0, to0) =
                case (leftElim, rightElim) of
                    (False, True) -> (right, left)
                    _ -> (left, right)
            (from, to)
                | isVarLeft
                , not isVarRight
                , not leftElim
                , hasBoundLeft = (right, left)
                | not isVarLeft
                , isVarRight
                , not rightElim
                , hasBoundRight = (left, right)
                | not leftElim
                , not rightElim
                , isVarLeft
                , isVarRight =
                    case (hasBoundLeft, hasBoundRight) of
                        (True, False) -> (right, left)
                        (False, True) -> (left, right)
                        _ -> (from0, to0)
                | otherwise = (from0, to0)
        pure (from, to)

    solveOnMismatch :: UnifyCore.UnifyMismatch -> SolveM UnifyCore.UnifyMismatchAction
    solveOnMismatch mismatch = case mismatch of
        UnifyCore.MismatchMissingNode nid ->
            throwError (MissingNode nid)
        UnifyCore.MismatchConstructor n1 n2 ->
            throwError (ConstructorClash n1 n2)
        UnifyCore.MismatchBase b1 b2 ->
            throwError (BaseClash b1 b2)
        UnifyCore.MismatchForallArity (Just k1) (Just k2) ->
            throwError (ForallArityMismatch k1 k2)
        UnifyCore.MismatchForallArity _ _ ->
            throwError (ForallArityMismatch 0 0)
        UnifyCore.MismatchExpVar _ _ ->
            throwError (UnexpectedExpNode (NodeId 0))
        UnifyCore.MismatchUnexpectedExp nid ->
            throwError (UnexpectedExpNode nid)
        UnifyCore.MismatchTyCon c1 c2 ->
            throwError (TyConClash c1 c2)
        UnifyCore.MismatchTyConArity c k1 k2 ->
            throwError (TyConArityMismatch c k1 k2)

    lookupNode :: NodeId -> SolveM TyNode
    lookupNode nid = do
        nodes <- gets (cNodes . suConstraint)
        case lookupNodeIn nodes nid of
            Just n -> pure n
            Nothing -> throwError (MissingNode nid)

    lookupNodeMaybe :: NodeId -> SolveM (Maybe TyNode)
    lookupNodeMaybe nid = do
        nodes <- gets (cNodes . suConstraint)
        pure (lookupNodeIn nodes nid)

    findRoot :: NodeId -> SolveM NodeId
    findRoot nid = do
        uf <- gets suUnionFind
        let (root, uf') = UnionFind.findRootWithCompression uf nid
        modify' $ \s -> s { suUnionFind = uf' }
        pure root

    unionNodes :: NodeId -> NodeId -> SolveM ()
    unionNodes from to =
        when (from /= to) $
            modify' $ \s ->
                s { suUnionFind = IntMap.insert (getNodeId from) to (suUnionFind s) }

    occursCheck :: NodeId -> NodeId -> SolveM ()
    occursCheck var target = do
        varRoot <- findRoot var
        targetRoot <- findRoot target
        when (varRoot == targetRoot) $ throwError (OccursCheckFailed varRoot targetRoot)
        nodes <- gets (cNodes . suConstraint)
        uf <- gets suUnionFind
        let canonical = UnionFind.frWith uf
            lookupTyNode nid = lookupNodeIn nodes nid
        case Traversal.occursInUnder canonical lookupTyNode varRoot targetRoot of
            Left _ -> throwError (OccursCheckFailed varRoot targetRoot)
            Right True -> throwError (OccursCheckFailed varRoot targetRoot)
            Right False -> pure ()
