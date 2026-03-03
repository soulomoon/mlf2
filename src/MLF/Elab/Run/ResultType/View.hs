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

import MLF.Constraint.Presolution (EdgeTrace, PresolutionPlanBuilder, PresolutionView(..))
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
    , cGenNodes
    , cNodes
    , fromListNode
    , getNodeId
    , gnSchemes
    , toListGen
    , toListNode
    )
import MLF.Elab.Generalize (GaBindParents)
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.ResultType.Types (ResultTypeInputs(..))
import MLF.Util.ElabError (ElabError(..))
import MLF.Util.Trace (TraceConfig)

data ResultTypeView = ResultTypeView
    { rtvInputs0 :: ResultTypeInputs
    , rtvSolvedBase :: Solved
    , rtvBoundOverlay0 :: IntMap.IntMap NodeId
    }

buildResultTypeView :: ResultTypeInputs -> Either ElabError ResultTypeView
buildResultTypeView inputs = do
    solved <- solveFromInputs inputs
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
    let cCanon = ChiQuery.chiCanonicalConstraint (rtvPresolutionView view)
    in map snd (toListGen (cGenNodes cCanon))

rtvCanonicalBindParents :: ResultTypeView -> BindParents
rtvCanonicalBindParents = ChiQuery.chiCanonicalBindParents . rtvPresolutionView

rtvSchemeBodyTarget :: ResultTypeView -> NodeId -> NodeId
rtvSchemeBodyTarget view target =
    let canonical = rtvCanonical view
        targetC = canonical target
        canonicalConstraint = ChiQuery.chiCanonicalConstraint (rtvPresolutionView view)
        genNodes = map snd (toListGen (cGenNodes canonicalConstraint))
        isSchemeRoot =
            any
                (\gen -> any (\root -> canonical root == targetC) (gnSchemes gen))
                genNodes
        schemeRootByBody =
            IntMap.fromListWith
                (\a _ -> a)
                [ (getNodeId (canonical bnd), root)
                | gen <- genNodes
                , root <- gnSchemes gen
                , Just bnd <- [rtvLookupVarBound view root]
                , case rtvLookupNode view (canonical bnd) of
                    Just TyBase{} -> False
                    Just TyBottom{} -> False
                    _ -> True
                ]
    in case rtvLookupNode view targetC of
        Just TyVar{ tnBound = Just bnd } ->
            let bndC = canonical bnd
                boundIsSchemeBody = IntMap.member (getNodeId bndC) schemeRootByBody
            in if isSchemeRoot || boundIsSchemeBody
                then
                    case rtvLookupNode view bndC of
                        Just TyForall{ tnBody = body } -> canonical body
                        _ -> bndC
                else targetC
        Just TyForall{ tnBody = body } -> canonical body
        _ -> targetC

rtvPresolutionView :: ResultTypeView -> PresolutionView
rtvPresolutionView = rtcPresolutionView . rtvInputs0

overlayBound :: ResultTypeView -> NodeId -> Maybe NodeId
overlayBound view nid =
    IntMap.lookup (getNodeId (rtvCanonical view nid)) (rtvBoundOverlay0 view)

solveFromInputs :: ResultTypeInputs -> Either ElabError Solved
solveFromInputs inputs =
    let presolutionView = rtcPresolutionView inputs
        solved0 =
            Solved.fromConstraintAndUf
                (rtcBaseConstraint inputs)
                (pvCanonicalMap presolutionView)
        solved =
            Solved.rebuildWithConstraint
                solved0
                (ChiQuery.chiCanonicalConstraint presolutionView)
    in case Solved.validateCanonicalGraphStrict solved of
        [] -> Right solved
        violations -> Left (ValidationFailed violations)
