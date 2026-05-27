{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module MLF.Elab.Run.ResultType.View (
    ResultTypeView,
    buildResultTypeView,
    rtvWithBoundOverlay,
    rtvLookupNode,
    rtvLookupVarBound,
    rtvPresolutionViewOverlay,
    rtvNamedNodes,
    rtvBaseVarOnlyNodes,
    rtvReifyWithNamedSetNoFallback,
    rtvReifyWithNamesNoFallback,
    rtvReifyBaseWithNamesNoFallback,
    rtvSchemeBodyTarget,
    rtvResolveCanonicalScope,
    rtvBindingScopeRefCanonical,
    rtvScopeRefCanonical,
    rtvGeneralizeTarget
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)

import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Presolution (PresolutionView(..))
import MLF.Constraint.Types.Graph
    ( Constraint(..)
    , NodeId(..)
    , NodeMap
    , NodeRef
    , TyNode(..)
    , fromListNode
    , getNodeId
    , toListNode
    )
import MLF.Constraint.Types.Phase (Phase)
import MLF.Elab.ReadModel
    ( ElabReadModel
    , buildElabReadModel
    , ermNamedNodes
    , ermNodesVarOnly
    )
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Run.Scope
    ( bindingScopeRefCanonical
    , canonicalizeScopeRef
    , resolveCanonicalScope
    , schemeBodyTarget
    )
import MLF.Elab.Run.ResultType.Types (ResultTypeInputs(..))
import MLF.Elab.Types (ElabScheme)
import MLF.Reify.Core
    ( reifyTypeWithNamedSetNoFallbackReadModel
    , reifyTypeWithNamesNoFallbackReadModel
    )
import MLF.Types.Elab (ElabType)
import MLF.Util.ElabError (ElabError(..), bindingToElab)

data ResultTypeView (p :: Phase) = ResultTypeView
    { rtvInputs0 :: ResultTypeInputs p
    , rtvBoundOverlay0 :: IntMap.IntMap NodeId
    , rtvReadModel0 :: Either ElabError (ElabReadModel p)
    , rtvBaseReadModel0 :: Either ElabError (ElabReadModel p)
    , rtvBaseVarOnlyNodes0 :: NodeMap TyNode
    }

buildResultTypeView :: ResultTypeInputs p -> Either ElabError (ResultTypeView p)
buildResultTypeView inputs = do
    let presolutionView = rtcPresolutionView inputs
        readModel = fromMaybe (buildElabReadModel presolutionView) (rtcReadModel inputs)
        baseReadModel =
            fromMaybe
                ( buildElabReadModel
                    (Finalize.presolutionViewFromSnapshot (gaBaseConstraint (rtcBindParentsGa inputs)) IntMap.empty)
                )
                (rtcBaseReadModel inputs)
    case Finalize.validateCanonicalSnapshotStrict
            (pvCanonicalConstraint presolutionView)
            (pvCanonicalMap presolutionView) of
        [] -> pure ()
        violations -> Left (ValidationFailed violations)
    pure ResultTypeView
        { rtvInputs0 = inputs
        , rtvBoundOverlay0 = IntMap.empty
        , rtvReadModel0 = readModel
        , rtvBaseReadModel0 = baseReadModel
        , rtvBaseVarOnlyNodes0 =
            case baseReadModel of
                Right model -> ermNodesVarOnly model
                Left _ -> baseVarOnlyNodes
        }
  where
    isTyVar node = case node of
        TyVar{} -> True
        _ -> False
    baseVarOnlyNodes =
        fromListNode
            [ (nid, node)
            | (nid, node) <- toListNode (cNodes (gaBaseConstraint (rtcBindParentsGa inputs)))
            , isTyVar node
            ]

rtvWithBoundOverlay :: NodeId -> NodeId -> ResultTypeView p -> ResultTypeView p
rtvWithBoundOverlay rootNid baseBound view =
    let canonical = rtcCanonical (rtvInputs0 view)
        rootKey = getNodeId (canonical rootNid)
        boundC = canonical baseBound
        viewWithOverlay =
            view
                { rtvBoundOverlay0 = IntMap.insert rootKey boundC (rtvBoundOverlay0 view)
                }
    in viewWithOverlay
        { rtvReadModel0 = buildElabReadModel (rtvPresolutionViewOverlay viewWithOverlay)
        , rtvBaseReadModel0 =
            buildElabReadModel
                (Finalize.presolutionViewFromSnapshot (gaBaseConstraint (rtvGaBindParents viewWithOverlay)) IntMap.empty)
        }

rtvLookupNode :: ResultTypeView p -> NodeId -> Maybe TyNode
rtvLookupNode view nid =
    case pvLookupNode (rtvPresolutionViewBase view) nid of
        Just TyVar{ tnId = varId, tnBound = Nothing } ->
            case overlayBound view nid of
                Just bnd -> Just TyVar{ tnId = varId, tnBound = Just bnd }
                Nothing -> Just TyVar{ tnId = varId, tnBound = Nothing }
        other -> other

rtvLookupVarBound :: ResultTypeView p -> NodeId -> Maybe NodeId
rtvLookupVarBound view nid =
    case overlayBound view nid of
        Just bnd -> Just bnd
        Nothing -> pvLookupVarBound (rtvPresolutionViewBase view) nid

rtvPresolutionViewBase :: ResultTypeView p -> PresolutionView p
rtvPresolutionViewBase = rtcPresolutionView . rtvInputs0

rtvPresolutionViewOverlay :: ResultTypeView p -> PresolutionView p
rtvPresolutionViewOverlay view
    | IntMap.null (rtvBoundOverlay0 view) =
        rtvPresolutionViewBase view
    | otherwise =
        (rtvPresolutionViewBase view)
            { pvConstraint = rtvConstraintOverlay view
            , pvCanonicalConstraint = rtvCanonicalConstraintOverlay view
            , pvLookupNode = rtvLookupNode view
            , pvLookupVarBound = rtvLookupVarBound view
            }

rtvNamedNodes :: ResultTypeView p -> Either ElabError IntSet.IntSet
rtvNamedNodes view = ermNamedNodes <$> rtvReadModel0 view

rtvBaseVarOnlyNodes :: ResultTypeView p -> NodeMap TyNode
rtvBaseVarOnlyNodes view
    | IntMap.null (rtvBoundOverlay0 view) = rtvBaseVarOnlyNodes0 view
    | otherwise = cNodes (applyBaseBoundOverlay view (emptyBaseConstraint { cNodes = rtvBaseVarOnlyNodes0 view }))
  where
    emptyBaseConstraint = gaBaseConstraint (rtcBindParentsGa (rtvInputs0 view))

rtvReifyWithNamedSetNoFallback
    :: ResultTypeView p
    -> IntMap.IntMap String
    -> IntSet.IntSet
    -> NodeId
    -> Either ElabError ElabType
rtvReifyWithNamedSetNoFallback view subst namedSet nid = do
    readModel <- rtvReadModel0 view
    reifyTypeWithNamedSetNoFallbackReadModel readModel subst namedSet nid

rtvReifyWithNamesNoFallback
    :: ResultTypeView p
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError ElabType
rtvReifyWithNamesNoFallback view subst nid = do
    readModel <- rtvReadModel0 view
    reifyTypeWithNamesNoFallbackReadModel readModel subst nid

rtvReifyBaseWithNamesNoFallback
    :: ResultTypeView p
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError ElabType
rtvReifyBaseWithNamesNoFallback view subst nid = do
    readModel <- rtvBaseReadModel0 view
    reifyTypeWithNamesNoFallbackReadModel readModel subst nid

rtvSchemeBodyTarget :: ResultTypeView p -> NodeId -> NodeId
rtvSchemeBodyTarget view =
    schemeBodyTarget (rtvPresolutionViewOverlay view)

rtvResolveCanonicalScope :: ResultTypeView p -> NodeId -> Either ElabError NodeRef
rtvResolveCanonicalScope view =
    bindingToElab
        . resolveCanonicalScope
            (rtcBaseConstraint (rtvInputs0 view))
            (rtvPresolutionViewOverlay view)
            (rtcRedirects (rtvInputs0 view))

rtvBindingScopeRefCanonical :: ResultTypeView p -> NodeId -> Either ElabError NodeRef
rtvBindingScopeRefCanonical view =
    bindingToElab
        . bindingScopeRefCanonical (rtvPresolutionViewOverlay view)

rtvScopeRefCanonical :: ResultTypeView p -> NodeRef -> NodeRef
rtvScopeRefCanonical view =
    canonicalizeScopeRef (rtvPresolutionViewOverlay view) (rtcRedirects (rtvInputs0 view))

rtvGeneralizeTarget
    :: ResultTypeView p
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
rtvGeneralizeTarget view =
    generalizeAtWithBuilder
        (rtcPlanBuilder (rtvInputs0 view))
        (Just (rtvGaBindParents view))
        (rtvPresolutionViewOverlay view)

overlayBound :: ResultTypeView p -> NodeId -> Maybe NodeId
overlayBound view nid =
    IntMap.lookup (getNodeId (rtcCanonical (rtvInputs0 view) nid)) (rtvBoundOverlay0 view)

rtvConstraintOverlay :: ResultTypeView p -> Constraint p
rtvConstraintOverlay view =
    applyBoundOverlay view (pvConstraint (rtvPresolutionViewBase view))

rtvCanonicalConstraintOverlay :: ResultTypeView p -> Constraint p
rtvCanonicalConstraintOverlay view =
    applyBoundOverlay view (pvCanonicalConstraint (rtvPresolutionViewBase view))

rtvGaBindParents :: ResultTypeView p -> GaBindParents p
rtvGaBindParents view =
    let ga0 = rtcBindParentsGa (rtvInputs0 view)
    in if IntMap.null (rtvBoundOverlay0 view)
        then ga0
        else ga0 { gaBaseConstraint = applyBaseBoundOverlay view (gaBaseConstraint ga0) }

applyBoundOverlay :: ResultTypeView p -> Constraint p -> Constraint p
applyBoundOverlay view constraint
    | IntMap.null (rtvBoundOverlay0 view) = constraint
    | otherwise =
    constraint
        { cNodes =
            fromListNode
                [ (nodeIdKey, adjustNode node)
                | (nodeIdKey, node) <- toListNode (cNodes constraint)
                ]
        }
  where
    canonical = rtcCanonical (rtvInputs0 view)
    overlay = rtvBoundOverlay0 view
    adjustNode node =
        case node of
            TyVar{ tnId = varId, tnBound = Nothing } ->
                case IntMap.lookup (getNodeId (canonical varId)) overlay of
                    Just bnd -> TyVar{ tnId = varId, tnBound = Just bnd }
                    Nothing -> node
            _ -> node

applyBaseBoundOverlay :: ResultTypeView p -> Constraint p -> Constraint p
applyBaseBoundOverlay view constraint
    | IntMap.null (rtvBoundOverlay0 view) = constraint
    | otherwise =
    constraint
        { cNodes =
            fromListNode
                [ (nodeIdKey, adjustNode nodeIdKey node)
                | (nodeIdKey, node) <- toListNode (cNodes constraint)
                ]
        }
  where
    inputs = rtvInputs0 view
    overlay = rtvBoundOverlay0 view
    solvedToBase = gaSolvedToBase (rtcBindParentsGa inputs)
    toBase nid =
        IntMap.findWithDefault nid (getNodeId nid) solvedToBase
    baseOverlay =
        IntMap.fromList
            [ ( getNodeId (toBase (NodeId solvedKey))
              , toBase solvedBound
              )
            | (solvedKey, solvedBound) <- IntMap.toList overlay
            ]
    adjustNode nodeIdKey node =
        case node of
            TyVar{ tnId = varId, tnBound = Nothing } ->
                case IntMap.lookup (getNodeId nodeIdKey) baseOverlay of
                    Just bnd -> TyVar{ tnId = varId, tnBound = Just bnd }
                    Nothing -> node
            _ -> node
