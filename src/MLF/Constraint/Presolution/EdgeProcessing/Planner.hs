{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Planner
Description : Pass A — classify instantiation edges into typed plans
Copyright   : (c) 2024
License     : BSD-3-Clause

The planner (pass A of the two-pass architecture) resolves each instantiation
edge's operands and enforces the invariant that the left node is 'TyExp'.

The resulting plan is consumed by the interpreter (pass B).
-}
module MLF.Constraint.Presolution.EdgeProcessing.Planner (
    planEdge,
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import MLF.Constraint.Presolution.Base (PresolutionError (..), PresolutionM, bindingPathToRootUnderM)
import MLF.Constraint.Presolution.EdgeProcessing.Plan
import MLF.Constraint.Presolution.Ops (findRoot, getCanonicalNode, getNode)
import MLF.Constraint.Presolution.StateAccess (findSchemeIntroducerM, getConstraintAndCanonical)
import MLF.Constraint.Types
import MLF.Constraint.Types.SynthesizedExpVar (isSynthesizedExpVar)
import MLF.Util.Trace (traceBindingM)

-- | Resolve an instantiation edge into a typed plan.
--
-- Reads the canonicalized left and right nodes and per-edge flags
-- ('cLetEdges', 'cAnnEdges') from the presolution state, then
-- rejects non-`TyExp` left nodes (fail-fast invariant).
planEdge :: InstEdge -> PresolutionM EdgePlan
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
    leftTyExp <- case mkResolvedTyExp n1Raw of
        Just leftExp -> pure leftExp
        Nothing ->
            throwError
                ( PlanError
                    (ExpectedTyExpLeftInPlanner edgeId n1Raw)
                )

    schemeOwnerGen <- resolveSchemeOwnerGen canonical constraint0 leftTyExp

    n2 <- getCanonicalNode n2Id

    traceBindingM cfg
        ( "processInstEdge: edge="
            ++ show edgeId
            ++ " left="
            ++ show n1Id
            ++ " ("
            ++ nodeTag (resolvedTyExpNode leftTyExp)
            ++ ") right="
            ++ show n2Id
            ++ " ("
            ++ nodeTag n2
            ++ ") letEdge="
            ++ show allowTrivial
        )
    case n2 of
        TyArrow { tnDom = dom, tnCod = cod } -> do
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
        , eprLeftTyExp = leftTyExp
        , eprRightNode = n2
        , eprLeftCanonical = canonical n1Id
        , eprRightCanonical = canonical n2Id
        , eprAllowTrivial = allowTrivial
        , eprSuppressWeaken = suppressWeaken
        , eprSchemeOwnerGen = schemeOwnerGen
        }

-- | Resolve the owning scheme introducer for a TyExp-left edge.
--
-- For frontend TyExp nodes we keep strict body-root provenance.
-- For synthesized wrappers we tolerate sparse test/fixture bind trees by
-- falling back from body-root lookup to wrapper-root lookup.
resolveSchemeOwnerGen :: (NodeId -> NodeId) -> Constraint -> ResolvedTyExp -> PresolutionM GenNodeId
resolveSchemeOwnerGen canonical constraint0 leftTyExp
    | isSynthesizedExpVar (rteExpVar leftTyExp) = do
        mbBody <- firstGenOnPath canonical constraint0 (rteBodyId leftTyExp)
        case mbBody of
            Just gid -> pure gid
            Nothing -> do
                mbWrapper <- firstGenOnPath canonical constraint0 (rteNodeId leftTyExp)
                case mbWrapper of
                    Just gid -> pure gid
                    Nothing ->
                        throwError
                            (InternalError ("scheme introducer not found for " ++ show (canonical (rteBodyId leftTyExp))))
    | otherwise =
        findSchemeIntroducerM canonical constraint0 (rteBodyId leftTyExp)

firstGenOnPath :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM (Maybe GenNodeId)
firstGenOnPath canonical constraint0 start = do
    path <- bindingPathToRootUnderM canonical constraint0 (typeRef (canonical start))
    pure (listToMaybe [gid | GenRef gid <- path])

nodeTag :: TyNode -> String
nodeTag = \case
    TyVar{} -> "TyVar"
    TyBottom{} -> "TyBottom"
    TyArrow{} -> "TyArrow"
    TyBase{} -> "TyBase"
    TyCon{} -> "TyCon"
    TyForall{} -> "TyForall"
    TyExp{} -> "TyExp"
