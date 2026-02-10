{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Planner
Description : Pass A — classify instantiation edges into typed plans
Copyright   : (c) 2024
License     : BSD-3-Clause

The planner (pass A of the two-pass architecture) resolves each instantiation
edge's operands and classifies it into an 'EdgePlan' with the appropriate
'EdgePlanMode':

* 'ExpansionMode' when the left node is 'TyExp'
* 'LegacyDirectMode' otherwise

The resulting plan is consumed by the interpreter (pass B).
-}
module MLF.Constraint.Presolution.EdgeProcessing.Planner (
    planEdge,
) where

import Control.Monad.Reader (ask)
import qualified Data.IntSet as IntSet

import MLF.Util.Trace (traceBindingM)
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (PresolutionM)
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import MLF.Constraint.Presolution.Ops (findRoot, getNode, getCanonicalNode)
import MLF.Constraint.Presolution.EdgeProcessing.Plan

-- | Classify an instantiation edge into a resolved plan.
--
-- Reads the canonicalized left and right nodes and per-edge flags
-- ('cLetEdges', 'cAnnEdges') from the presolution state, then
-- selects the execution mode based on the left node shape.
planEdge :: InstEdge -> PresolutionM (EdgePlan 'StageResolved)
planEdge edge = do
    cfg <- ask
    (constraint0, canonical) <- getConstraintAndCanonical
    let edgeId = instEdgeId edge
        n1Id = instLeft edge
        n2Id = instRight edge
        eidInt = getEdgeId edgeId
        allowTrivial = IntSet.member eidInt (cLetEdges constraint0)
        suppressWeaken = IntSet.member eidInt (cAnnEdges constraint0)

    n1Raw <- getNode n1Id
    n2 <- getCanonicalNode n2Id

    let mode = case n1Raw of
            TyExp {} -> ExpansionMode
            _        -> LegacyDirectMode

    traceBindingM cfg
        ( "processInstEdge: edge="
            ++ show edgeId
            ++ " left="
            ++ show n1Id
            ++ " ("
            ++ nodeTag n1Raw
            ++ ") right="
            ++ show n2Id
            ++ " ("
            ++ nodeTag n2
            ++ ") letEdge="
            ++ show allowTrivial
        )
    case (n1Raw, n2) of
        (TyExp{}, TyArrow{ tnDom = dom, tnCod = cod }) -> do
            domR <- findRoot dom
            codR <- findRoot cod
            traceBindingM cfg
                ( "processInstEdge: edge="
                    ++ show edgeId
                    ++ " target arrow dom="
                    ++ show dom
                    ++ " domRoot="
                    ++ show domR
                    ++ " cod="
                    ++ show cod
                    ++ " codRoot="
                    ++ show codR
                )
        _ -> pure ()

    pure EdgePlanResolved
        { eprEdge = edge
        , eprLeftNode = n1Raw
        , eprRightNode = n2
        , eprLeftCanonical = canonical n1Id
        , eprRightCanonical = canonical n2Id
        , eprMode = mode
        , eprAllowTrivial = allowTrivial
        , eprSuppressWeaken = suppressWeaken
        }

nodeTag :: TyNode -> String
nodeTag = \case
    TyVar{} -> "TyVar"
    TyBottom{} -> "TyBottom"
    TyArrow{} -> "TyArrow"
    TyBase{} -> "TyBase"
    TyCon{} -> "TyCon"
    TyForall{} -> "TyForall"
    TyExp{} -> "TyExp"
