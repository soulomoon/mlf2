{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType (
    ResultTypeContext(..),
    computeResultTypeFromAnn,
    computeResultTypeFallback
) where

import qualified Data.IntSet as IntSet

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Constraint.Types.Graph
    ( EdgeId(..)
    , GenNode(..)
    , NodeId(..)
    , cLetEdges
    , getNodeId
    , gnSchemes
    )
import qualified MLF.Constraint.NodeAccess
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Elab.Types (ElabType, ElabError)
import MLF.Elab.Run.ResultType.Types (ResultTypeContext(..))
import qualified MLF.Elab.Run.ResultType.Ann as Ann
import qualified MLF.Elab.Run.ResultType.Fallback as Fallback

-- Re-export computeResultTypeFromAnn from Ann module
computeResultTypeFromAnn :: ResultTypeContext -> AnnExpr -> AnnExpr -> NodeId -> EdgeId -> Either ElabError ElabType
computeResultTypeFromAnn = Ann.computeResultTypeFromAnn

-- | Compute result type when there's no direct annotation (fallback path).
-- This is a facade that handles the AAnn case by delegating to computeResultTypeFromAnn,
-- and delegates the non-AAnn case to the Fallback submodule.
computeResultTypeFallback
    :: ResultTypeContext
    -> AnnExpr      -- ^ annCanon (post-redirect)
    -> AnnExpr      -- ^ ann (pre-redirect)
    -> Either ElabError ElabType
computeResultTypeFallback ctx annCanon ann = do
    -- First, determine the root (same logic as before to check for AAnn)
    let canonical = rtcCanonical ctx
        c1 = rtcBaseConstraint ctx
        solvedForGen = rtcSolvedForGen ctx

    let schemeRootSet =
            let allGenNodes = MLF.Constraint.NodeAccess.allGenNodes (srConstraint solvedForGen)
            in IntSet.fromList
                [ getNodeId (canonical root)
                | gen <- allGenNodes
                , root <- gnSchemes gen
                ]
        isSchemeRoot nid =
            IntSet.member (getNodeId (canonical nid)) schemeRootSet
        letEdges = cLetEdges c1
        isLetEdge (EdgeId eid) = IntSet.member eid letEdges

    let rootForTypeAnn =
            let peel ann0 = case ann0 of
                    ALet _ _ _ _ _ _ bodyAnn nid ->
                        case bodyAnn of
                            AAnn inner target eid
                                | canonical target == canonical nid
                                    && isLetEdge eid ->
                                        peel inner
                                | canonical target == canonical nid
                                    && not (isSchemeRoot target) ->
                                        peel inner
                            _ -> bodyAnn
                    _ -> ann0
            in peel annCanon
        rootForTypePreAnn =
            let peel ann0 = case ann0 of
                    ALet _ _ _ _ _ _ bodyAnn nid ->
                        case bodyAnn of
                            AAnn inner target eid
                                | target == nid
                                    && isLetEdge eid ->
                                        peel inner
                                | target == nid
                                    && not (isSchemeRoot target) ->
                                        peel inner
                            _ -> bodyAnn
                    _ -> ann0
            in peel ann

    -- Dispatch based on the root type
    case rootForTypeAnn of
        AAnn inner annNodeId eid -> do
            let innerPre =
                    case rootForTypePreAnn of
                        AAnn innerPre0 _ _ -> innerPre0
                        _ -> rootForTypePreAnn
            computeResultTypeFromAnn ctx inner innerPre annNodeId eid
        _ ->
            Fallback.computeResultTypeFallback ctx annCanon ann
