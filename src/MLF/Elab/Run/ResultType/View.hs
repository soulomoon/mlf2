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

import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Presolution (PresolutionView(..))
import MLF.Constraint.Types.Graph
    ( Constraint(..)
    , NodeId(..)
    , NodeRef
    , TyNode(..)
    , fromListNode
    , getNodeId
    , toListNode
    )
import MLF.Constraint.Types.Phase (Phase)
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
    ( namedNodes
    , reifyTypeWithNamedSetNoFallback
    , reifyTypeWithNamesNoFallback
    , reifyTypeWithNamesNoFallbackOnConstraint
    )
import MLF.Types.Elab (ElabType)
import MLF.Util.ElabError (ElabError(..), bindingToElab)

data ResultTypeView (p :: Phase) = ResultTypeView
    { rtvInputs0 :: ResultTypeInputs p
    , rtvBoundOverlay0 :: IntMap.IntMap NodeId
    }

buildResultTypeView :: ResultTypeInputs p -> Either ElabError (ResultTypeView p)
buildResultTypeView inputs = do
    let presolutionView = rtcPresolutionView inputs
    case Finalize.validateCanonicalSnapshotStrict
            (pvCanonicalConstraint presolutionView)
            (pvCanonicalMap presolutionView) of
        [] -> pure ()
        violations -> Left (ValidationFailed violations)
    pure ResultTypeView
        { rtvInputs0 = inputs
        , rtvBoundOverlay0 = IntMap.empty
        }

rtvWithBoundOverlay :: NodeId -> NodeId -> ResultTypeView p -> ResultTypeView p
rtvWithBoundOverlay rootNid baseBound view =
    let canonical = rtcCanonical (rtvInputs0 view)
        rootKey = getNodeId (canonical rootNid)
        boundC = canonical baseBound
    in view
        { rtvBoundOverlay0 = IntMap.insert rootKey boundC (rtvBoundOverlay0 view)
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
rtvPresolutionViewOverlay view =
    (rtvPresolutionViewBase view)
        { pvConstraint = rtvConstraintOverlay view
        , pvCanonicalConstraint = rtvCanonicalConstraintOverlay view
        , pvLookupNode = rtvLookupNode view
        , pvLookupVarBound = rtvLookupVarBound view
        }

rtvNamedNodes :: ResultTypeView p -> Either ElabError IntSet.IntSet
rtvNamedNodes = namedNodes . rtvPresolutionViewOverlay

rtvReifyWithNamedSetNoFallback
    :: ResultTypeView p
    -> IntMap.IntMap String
    -> IntSet.IntSet
    -> NodeId
    -> Either ElabError ElabType
rtvReifyWithNamedSetNoFallback view subst namedSet =
    reifyTypeWithNamedSetNoFallback (rtvPresolutionViewOverlay view) subst namedSet

rtvReifyWithNamesNoFallback
    :: ResultTypeView p
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError ElabType
rtvReifyWithNamesNoFallback view subst =
    reifyTypeWithNamesNoFallback (rtvPresolutionViewOverlay view) subst

rtvReifyBaseWithNamesNoFallback
    :: ResultTypeView p
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError ElabType
rtvReifyBaseWithNamesNoFallback view subst =
    reifyTypeWithNamesNoFallbackOnConstraint
        (gaBaseConstraint (rtvGaBindParents view))
        subst

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
    in ga0 { gaBaseConstraint = applyBaseBoundOverlay view (gaBaseConstraint ga0) }

applyBoundOverlay :: ResultTypeView p -> Constraint p -> Constraint p
applyBoundOverlay view constraint =
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
applyBaseBoundOverlay view constraint =
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
