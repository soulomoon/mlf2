{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType.View (
    ResultTypeView,
    buildResultTypeView,
    rtvWithBoundOverlay,
    rtvLookupNode,
    rtvLookupVarBound,
    rtvPresolutionViewOverlay
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Presolution (PresolutionView(..))
import MLF.Constraint.Solve (SolveResult(..))
import qualified MLF.Constraint.Solve as Solve
import MLF.Constraint.Types
    ( NodeId(..)
    , TyNode(..)
    , getNodeId
    )
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.ResultType.Types (ResultTypeInputs(..))
import MLF.Util.ElabError (ElabError(..))

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
    let canonical = rtcCanonical (rtvInputs0 view)
        rootKey = getNodeId (canonical rootNid)
        boundC = canonical baseBound
    in view
        { rtvBoundOverlay0 = IntMap.insert rootKey boundC (rtvBoundOverlay0 view)
        }


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
    IntMap.lookup (getNodeId (rtcCanonical (rtvInputs0 view) nid)) (rtvBoundOverlay0 view)
