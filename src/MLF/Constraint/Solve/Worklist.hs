module MLF.Constraint.Solve.Worklist (
    SolveError(..),
    UnifyClosureResult(..),
    runUnifyClosure,
    runUnifyClosureWithSeed
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT, execStateT, gets, modify')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Solve.Harmonize as Harmonize
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.Unify.Core as UnifyCore
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Types.Graph hiding (lookupNode)
import MLF.Constraint.Unify.Closure (SolveError(..), UnifyClosureResult(..))
import MLF.Util.Trace (TraceConfig, traceBinding)
import qualified MLF.Util.UnionFind as UnionFind

data SolveState p = SolveState { suConstraint :: Constraint p
    , suUnionFind :: IntMap NodeId
    , suQueue :: [UnifyEdge]
    }

type SolveM p = StateT (SolveState p) (Either SolveError)

runUnifyClosure :: TraceConfig -> Constraint p -> Either SolveError (UnifyClosureResult p)
runUnifyClosure traceCfg =
    runUnifyClosureWithSeed traceCfg IntMap.empty

runUnifyClosureWithSeed
    :: TraceConfig
    -> IntMap NodeId
    -> Constraint p
    -> Either SolveError (UnifyClosureResult p)
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
            final <- execStateT loop st
            harmonized <- Harmonize.batchHarmonizeConstraint (suUnionFind final) (suConstraint final)
            let preRewrite = harmonized { cUnifyEdges = [] }
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
                        UnifyClosureResult { ucConstraint = preRewrite
                            , ucUnionFind = suUnionFind final
                            }

loop :: SolveM p ()
loop = do
    queue <- gets suQueue
    case queue of
        [] -> pure ()
        edge : rest -> do
            modify' $ \st -> st { suQueue = rest }
            processEdge edge
            loop

enqueue :: [UnifyEdge] -> SolveM p ()
enqueue edges =
    modify' $ \st -> st { suQueue = edges ++ suQueue st }

processEdge :: UnifyEdge -> SolveM p ()
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
    (TyVar { tnBound = mb1 }, TyVar { tnBound = mb2 }) ->
        case (mb1, mb2) of
            (Just b1, Just b2)
                | b1 /= b2 -> [UnifyEdge b1 b2]
            _ -> []
    (TyVar { tnBound = mb1 }, _) ->
        case mb1 of
            Just b1 | b1 /= rRoot -> [UnifyEdge b1 rRoot]
            _ -> []
    (_, TyVar { tnBound = mb2 }) ->
        case mb2 of
            Just b2 | b2 /= lRoot -> [UnifyEdge b2 lRoot]
            _ -> []
    _ -> []

solveUnifyStrategy :: UnifyCore.UnifyStrategy (SolveM p)
solveUnifyStrategy = UnifyCore.UnifyStrategy
    { UnifyCore.usOccursCheck = UnifyCore.OccursCheck occursCheck
    , UnifyCore.usTyExpPolicy = UnifyCore.TyExpReject
    , UnifyCore.usForallArityPolicy = UnifyCore.ForallArityCheck solveForallArity
    , UnifyCore.usRepresentative = UnifyCore.RepresentativeChoice solveRepresentative
    , UnifyCore.usOnMismatch = solveOnMismatch
    }

solveForallArity :: NodeId -> SolveM p (Maybe Int)
solveForallArity nid = do
    cCur <- gets suConstraint
    uf0 <- gets suUnionFind
    let canonical = UnionFind.frWith uf0
    case Binding.orderedBinders canonical cCur (typeRef nid) of
        Right binders -> pure (Just (length binders))
        Left err -> throwError (BindingTreeError err)

solveRepresentative :: NodeId -> TyNode -> NodeId -> TyNode -> SolveM p (NodeId, NodeId)
solveRepresentative left leftNode right rightNode = do
    cCur <- gets suConstraint
    let leftElim = VarStore.isEliminatedVar cCur left
        rightElim = VarStore.isEliminatedVar cCur right
        isVarLeft = case leftNode of
            TyVar {} -> True
            _ -> False
        isVarRight = case rightNode of
            TyVar {} -> True
            _ -> False
        hasBoundLeft = case leftNode of
            TyVar { tnBound = Just _ } -> True
            _ -> False
        hasBoundRight = case rightNode of
            TyVar { tnBound = Just _ } -> True
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

solveOnMismatch :: UnifyCore.UnifyMismatch -> SolveM p UnifyCore.UnifyMismatchAction
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

lookupNode :: NodeId -> SolveM p TyNode
lookupNode nid = do
    nodes <- gets (cNodes . suConstraint)
    case lookupNodeIn nodes nid of
        Just node -> pure node
        Nothing -> throwError (MissingNode nid)

lookupNodeMaybe :: NodeId -> SolveM p (Maybe TyNode)
lookupNodeMaybe nid = do
    nodes <- gets (cNodes . suConstraint)
    pure (lookupNodeIn nodes nid)

findRoot :: NodeId -> SolveM p NodeId
findRoot nid = do
    uf <- gets suUnionFind
    let (root, uf') = UnionFind.findRootWithCompression uf nid
    modify' $ \st -> st { suUnionFind = uf' }
    pure root

unionNodes :: NodeId -> NodeId -> SolveM p ()
unionNodes from to =
    when (from /= to) $
        modify' $ \st ->
            st { suUnionFind = IntMap.insert (getNodeId from) to (suUnionFind st) }

occursCheck :: NodeId -> NodeId -> SolveM p ()
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
