{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType.View (
    ResultTypeView,
    buildResultTypeView,
    rtvWithBoundOverlay,
    rtvCanonical,
    rtvEdgeWitnesses,
    rtvEdgeTraces,
    rtvEdgeExpansions,
    rtvBindParentsGa,
    rtvPlanBuilder,
    rtvBaseConstraint,
    rtvRedirects,
    rtvTraceConfig,
    rtvSolved,
    rtvOriginalConstraint,
    rtvLookupNode,
    rtvLookupVarBound,
    rtvGenNodes,
    rtvCanonicalBindParents,
    rtvSchemeBodyTarget
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Presolution (EdgeTrace, PresolutionPlanBuilder)
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types
    ( BindParents
    , Constraint
    , EdgeWitness
    , Expansion
    , GenNode
    , NodeId(..)
    , TyNode(..)
    , cNodes
    , fromListNode
    , getNodeId
    , toListGen
    , toListNode
    )
import MLF.Elab.Generalize (GaBindParents)
import MLF.Elab.Run.Scope (schemeBodyTarget)
import MLF.Elab.Run.ResultType.Types (ResultTypeInputs(..), rtcSolveLike)
import MLF.Util.ElabError (ElabError)
import MLF.Util.Trace (TraceConfig)

data ResultTypeView = ResultTypeView
    { rtvInputs0 :: ResultTypeInputs
    , rtvSolvedBase :: Solved
    , rtvBoundOverlay0 :: IntMap.IntMap NodeId
    }

buildResultTypeView :: ResultTypeInputs -> Either ElabError ResultTypeView
buildResultTypeView inputs = do
    solved <- rtcSolveLike inputs
    pure ResultTypeView
        { rtvInputs0 = inputs
        , rtvSolvedBase = solved
        , rtvBoundOverlay0 = IntMap.empty
        }

rtvWithBoundOverlay :: NodeId -> NodeId -> ResultTypeView -> ResultTypeView
rtvWithBoundOverlay rootNid baseBound view =
    let canonical = rtvCanonical view
        rootKey = getNodeId (canonical rootNid)
        boundC = canonical baseBound
    in view
        { rtvBoundOverlay0 = IntMap.insert rootKey boundC (rtvBoundOverlay0 view)
        }

rtvCanonical :: ResultTypeView -> NodeId -> NodeId
rtvCanonical = rtcCanonical . rtvInputs0

rtvEdgeWitnesses :: ResultTypeView -> IntMap.IntMap EdgeWitness
rtvEdgeWitnesses = rtcEdgeWitnesses . rtvInputs0

rtvEdgeTraces :: ResultTypeView -> IntMap.IntMap EdgeTrace
rtvEdgeTraces = rtcEdgeTraces . rtvInputs0

rtvEdgeExpansions :: ResultTypeView -> IntMap.IntMap Expansion
rtvEdgeExpansions = rtcEdgeExpansions . rtvInputs0

rtvBindParentsGa :: ResultTypeView -> GaBindParents
rtvBindParentsGa = rtcBindParentsGa . rtvInputs0

rtvPlanBuilder :: ResultTypeView -> PresolutionPlanBuilder
rtvPlanBuilder = rtcPlanBuilder . rtvInputs0

rtvBaseConstraint :: ResultTypeView -> Constraint
rtvBaseConstraint = rtcBaseConstraint . rtvInputs0

rtvRedirects :: ResultTypeView -> IntMap.IntMap NodeId
rtvRedirects = rtcRedirects . rtvInputs0

rtvTraceConfig :: ResultTypeView -> TraceConfig
rtvTraceConfig = rtcTraceConfig . rtvInputs0

rtvSolved :: ResultTypeView -> Solved
rtvSolved view
    | IntMap.null overlay = solved
    | otherwise = Solved.rebuildWithConstraint solved constraint'
  where
    solved = rtvSolvedBase view
    overlay = rtvBoundOverlay0 view
    constraint0 = Solved.originalConstraint solved
    nodes0 = cNodes constraint0
    patchNode nid node =
        case IntMap.lookup (getNodeId nid) overlay of
            Just bnd ->
                case node of
                    TyVar{ tnId = varId, tnBound = Nothing } ->
                        TyVar{ tnId = varId, tnBound = Just bnd }
                    _ -> node
            Nothing -> node
    nodes' =
        fromListNode
            [ (nid, patchNode nid node)
            | (nid, node) <- toListNode nodes0
            ]
    constraint' = constraint0 { cNodes = nodes' }

rtvOriginalConstraint :: ResultTypeView -> Constraint
rtvOriginalConstraint = Solved.originalConstraint . rtvSolved

rtvLookupNode :: ResultTypeView -> NodeId -> Maybe TyNode
rtvLookupNode view nid = Solved.lookupNode (rtvSolved view) nid

rtvLookupVarBound :: ResultTypeView -> NodeId -> Maybe NodeId
rtvLookupVarBound view nid = Solved.lookupVarBound (rtvSolved view) nid

rtvGenNodes :: ResultTypeView -> [GenNode]
rtvGenNodes view = map snd (toListGen (Solved.genNodes (rtvSolved view)))

rtvCanonicalBindParents :: ResultTypeView -> BindParents
rtvCanonicalBindParents = Solved.canonicalBindParents . rtvSolved

rtvSchemeBodyTarget :: ResultTypeView -> NodeId -> NodeId
rtvSchemeBodyTarget view nid = schemeBodyTarget (rtvSolved view) nid
