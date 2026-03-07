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
    rtvLookupNode,
    rtvLookupVarBound,
    rtvGenNodes,
    rtvCanonicalBindParents,
    rtvSchemeBodyTarget
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Presolution (EdgeTrace, PresolutionPlanBuilder, PresolutionView(..))
import MLF.Constraint.Solve (SolveResult(..))
import qualified MLF.Constraint.Solve as Solve
import MLF.Constraint.Types
    ( BindParents
    , Constraint
    , EdgeWitness
    , Expansion
    , GenNode
    , NodeId(..)
    , TyNode(..)
    , cGenNodes
    , getNodeId
    , toListGen
    )
import MLF.Elab.Generalize (GaBindParents)
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.Scope (schemeBodyTarget)
import MLF.Elab.Run.ResultType.Types (ResultTypeInputs(..))
import MLF.Util.ElabError (ElabError(..))
import MLF.Util.Trace (TraceConfig)

data ResultTypeView = ResultTypeView
    { rtvInputs0 :: ResultTypeInputs
    , rtvBoundOverlay0 :: IntMap.IntMap NodeId
    }

buildResultTypeView :: ResultTypeInputs -> Either ElabError ResultTypeView
buildResultTypeView inputs = do
    let presolutionView = rtcPresolutionView inputs
    case Solve.validateSolvedGraphStrict
        SolveResult
            { srConstraint = ChiQuery.chiCanonicalConstraint presolutionView
            , srUnionFind = ChiQuery.chiCanonicalMap presolutionView
            } of
        [] -> pure ()
        violations -> Left (ValidationFailed violations)
    pure ResultTypeView
        { rtvInputs0 = inputs
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

rtvLookupNode :: ResultTypeView -> NodeId -> Maybe TyNode
rtvLookupNode view nid =
    case ChiQuery.chiLookupNode (rtvPresolutionView view) nid of
        Just TyVar{ tnId = varId, tnBound = Nothing } ->
            case overlayBound view nid of
                Just bnd -> Just TyVar{ tnId = varId, tnBound = Just bnd }
                Nothing -> Just TyVar{ tnId = varId, tnBound = Nothing }
        other -> other

rtvLookupVarBound :: ResultTypeView -> NodeId -> Maybe NodeId
rtvLookupVarBound view nid =
    case overlayBound view nid of
        Just bnd -> Just bnd
        Nothing -> ChiQuery.chiLookupVarBound (rtvPresolutionView view) nid

rtvGenNodes :: ResultTypeView -> [GenNode]
rtvGenNodes view =
    map
        snd
        (toListGen (cGenNodes (ChiQuery.chiConstraint (rtvPresolutionView view))))

rtvCanonicalBindParents :: ResultTypeView -> BindParents
rtvCanonicalBindParents = ChiQuery.chiCanonicalBindParents . rtvPresolutionView

rtvSchemeBodyTarget :: ResultTypeView -> NodeId -> NodeId
rtvSchemeBodyTarget view nid = schemeBodyTarget (rtvPresolutionViewOverlay view) nid

rtvPresolutionView :: ResultTypeView -> PresolutionView
rtvPresolutionView = rtcPresolutionView . rtvInputs0

rtvPresolutionViewOverlay :: ResultTypeView -> PresolutionView
rtvPresolutionViewOverlay view =
    (rtvPresolutionView view)
        { pvLookupNode = rtvLookupNode view
        , pvLookupVarBound = rtvLookupVarBound view
        }

overlayBound :: ResultTypeView -> NodeId -> Maybe NodeId
overlayBound view nid =
    IntMap.lookup (getNodeId (rtvCanonical view nid)) (rtvBoundOverlay0 view)
