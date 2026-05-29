{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module MLF.Elab.Run.ResultType.View (
    ResultTypeView,
    ResultTypeFallbackIndex(..),
    buildResultTypeView,
    rtvWithKnownGeneralization,
    rtvWithBoundOverlay,
    rtvFallbackIndex,
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
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Presolution (EdgeTrace(..), PresolutionView(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), lookupCopy)
import MLF.Constraint.Types.Graph
    ( BaseTy
    , Constraint(..)
    , GenNodeId
    , GenNode(..)
    , NodeId(..)
    , NodeMap
    , NodeRef
    , TyNode(..)
    , fromListNode
    , gnSchemes
    , getGenNodeMap
    , getNodeId
    , lookupNodeIn
    , nodeRefKey
    , toListNode
    )
import MLF.Constraint.Types.Witness (ewRight)
import MLF.Constraint.Types.Phase (Phase)
import MLF.Elab.ReadModel
    ( ElabReadModel
    , buildElabReadModel
    , ermNamedNodes
    , ermNodesVarOnly
    )
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Run.Generalize.Common (canonicalSchemeRootOwners)
import MLF.Elab.Run.Scope
    ( bindingScopeRefCanonical
    , canonicalizeScopeRef
    , resolveCanonicalScope
    )
import MLF.Elab.Run.ResultType.Types
    ( ResultTypeInputs(..)
    , rtcEdgeTraces
    , rtcEdgeWitnesses
    )
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
    , rtvGeneralizeCache0 :: Map.Map (Int, Int) (ElabScheme, IntMap.IntMap String)
    , rtvFallbackIndex0 :: ResultTypeFallbackIndex
    }

data ResultTypeFallbackIndex = ResultTypeFallbackIndex
    { rtfiEdgeTraceCounts :: IntMap.IntMap Int
    , rtfiTraceBinderArgBaseBounds :: IntMap.IntMap IntSet.IntSet
    , rtfiEdgeBaseBounds :: IntSet.IntSet
    , rtfiInstArgBaseBounds :: IntSet.IntSet
    , rtfiRootBounds :: IntMap.IntMap IntSet.IntSet
    , rtfiInstArgRootMultiBase :: Bool
    , rtfiBaseNodeByTy :: Map.Map BaseTy NodeId
    , rtfiSchemeRootSet :: IntSet.IntSet
    , rtfiSchemeRootOwner :: IntMap.IntMap GenNodeId
    , rtfiSchemeBodyRoot :: IntMap.IntMap NodeId
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
        , rtvGeneralizeCache0 = Map.empty
        , rtvFallbackIndex0 = buildResultTypeFallbackIndex inputs
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

rtvWithKnownGeneralization
    :: NodeRef
    -> NodeId
    -> (ElabScheme, IntMap.IntMap String)
    -> ResultTypeView p
    -> ResultTypeView p
rtvWithKnownGeneralization scopeRoot target result view =
    view
        { rtvGeneralizeCache0 =
            Map.insert
                (generalizeCacheKey scopeRoot target)
                result
                (rtvGeneralizeCache0 view)
        }

rtvWithBoundOverlay :: NodeId -> NodeId -> ResultTypeView p -> ResultTypeView p
rtvWithBoundOverlay rootNid baseBound view =
    let canonical = rtcCanonical (rtvInputs0 view)
        rootKey = getNodeId (canonical rootNid)
        boundC = canonical baseBound
        viewWithOverlay =
            view
                { rtvBoundOverlay0 = IntMap.insert rootKey boundC (rtvBoundOverlay0 view)
                , rtvGeneralizeCache0 = Map.empty
                }
    in viewWithOverlay
        { rtvReadModel0 = rtvReadModel0 view
        , rtvBaseReadModel0 = rtvBaseReadModel0 view
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

rtvFallbackIndex :: ResultTypeView p -> ResultTypeFallbackIndex
rtvFallbackIndex = rtvFallbackIndex0

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
rtvNamedNodes view = ermNamedNodes <$> rtvReadModel view

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
    readModel <- rtvReadModel view
    reifyTypeWithNamedSetNoFallbackReadModel readModel subst namedSet nid

rtvReifyWithNamesNoFallback
    :: ResultTypeView p
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError ElabType
rtvReifyWithNamesNoFallback view subst nid = do
    readModel <- rtvReadModel view
    reifyTypeWithNamesNoFallbackReadModel readModel subst nid

rtvReifyBaseWithNamesNoFallback
    :: ResultTypeView p
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError ElabType
rtvReifyBaseWithNamesNoFallback view subst nid = do
    readModel <- rtvBaseReadModel view
    reifyTypeWithNamesNoFallbackReadModel readModel subst nid

rtvReadModel :: ResultTypeView p -> Either ElabError (ElabReadModel p)
rtvReadModel view
    | IntMap.null (rtvBoundOverlay0 view) = rtvReadModel0 view
    | otherwise = buildElabReadModel (rtvPresolutionViewOverlay view)

rtvBaseReadModel :: ResultTypeView p -> Either ElabError (ElabReadModel p)
rtvBaseReadModel view
    | IntMap.null (rtvBoundOverlay0 view) = rtvBaseReadModel0 view
    | otherwise =
        buildElabReadModel
            (Finalize.presolutionViewFromSnapshot (gaBaseConstraint (rtvGaBindParents view)) IntMap.empty)

rtvSchemeBodyTarget :: ResultTypeView p -> NodeId -> NodeId
rtvSchemeBodyTarget view target =
    let canonical = rtcCanonical (rtvInputs0 view)
        fallbackIndex = rtvFallbackIndex view
        targetC = canonical target
        targetNode = rtvLookupNode view targetC
        boundCanonical =
            case targetNode of
                Just TyVar{tnBound = Just bnd} -> Just (canonical bnd)
                _ -> Nothing
        boundNode = boundCanonical >>= rtvLookupNode view
        isSchemeRoot =
            IntSet.member (getNodeId targetC) (rtfiSchemeRootSet fallbackIndex)
        boundIsSchemeBody =
            maybe
                False
                (\bndC -> IntMap.member (getNodeId bndC) (rtfiSchemeBodyRoot fallbackIndex))
                boundCanonical
    in case targetNode of
        Just TyVar{tnBound = Just _} ->
            if isSchemeRoot || boundIsSchemeBody
                then case (boundCanonical, boundNode) of
                    (Just _, Just TyForall{tnBody = body}) -> canonical body
                    (Just bndC, _) -> bndC
                    _ -> targetC
                else targetC
        Just TyForall{tnBody = body} -> canonical body
        _ -> targetC

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
rtvGeneralizeTarget view scopeRoot target =
    case Map.lookup (generalizeCacheKey scopeRoot target) (rtvGeneralizeCache0 view) of
        Just result -> Right result
        Nothing ->
            generalizeAtWithBuilder
                (rtcPlanBuilder (rtvInputs0 view))
                (Just (rtvGaBindParents view))
                (rtvPresolutionViewOverlay view)
                scopeRoot
                target

generalizeCacheKey :: NodeRef -> NodeId -> (Int, Int)
generalizeCacheKey scopeRoot target =
    (nodeRefKey scopeRoot, getNodeId target)

buildResultTypeFallbackIndex :: ResultTypeInputs p -> ResultTypeFallbackIndex
buildResultTypeFallbackIndex inputs =
    ResultTypeFallbackIndex
        { rtfiEdgeTraceCounts = edgeTraceCounts
        , rtfiTraceBinderArgBaseBounds = traceBinderArgBaseBounds
        , rtfiEdgeBaseBounds = edgeBaseBounds
        , rtfiInstArgBaseBounds = instArgBaseBounds
        , rtfiRootBounds = rootBounds
        , rtfiInstArgRootMultiBase = instArgRootMultiBase
        , rtfiBaseNodeByTy = baseNodeByTy
        , rtfiSchemeRootSet = schemeRootSet
        , rtfiSchemeRootOwner = schemeRootOwner
        , rtfiSchemeBodyRoot = schemeBodyRoot
        }
  where
    presolutionView = rtcPresolutionView inputs
    canonical = rtcCanonical inputs
    nodes = cNodes (pvConstraint presolutionView)
    edgeWitnesses = rtcEdgeWitnesses inputs
    edgeTraces = rtcEdgeTraces inputs
    (schemeRootSet, schemeRootOwner) =
        canonicalSchemeRootOwners canonical genNodes
    genNodes = getGenNodeMap (cGenNodes (pvConstraint presolutionView))

    schemeBodyRoot =
        IntMap.fromListWith
            (\existing _ -> existing)
            [ (getNodeId (canonical bnd), canonical root)
            | gen <- IntMap.elems genNodes
            , root <- gnSchemes gen
            , Just bnd <- [pvLookupVarBound presolutionView root]
            , case pvLookupNode presolutionView (canonical bnd) of
                Just TyBase{} -> False
                Just TyBottom{} -> False
                _ -> True
            ]

    edgeTraceCounts =
        IntMap.fromListWith
            (+)
            [ (getNodeId (etRoot tr), 1 :: Int)
            | tr <- IntMap.elems edgeTraces
            ]

    baseNodeByTy =
        foldl'
            ( \acc node ->
                case node of
                    TyBase{tnId = nid, tnBase = base} ->
                        Map.insertWith (\_old existing -> existing) base nid acc
                    _ -> acc
            )
            Map.empty
            (map snd (toListNode nodes))

    resolveBaseBoundCanonical start =
        let go visited nid0 =
                let nid = canonical nid0
                    key = getNodeId nid
                in if IntSet.member key visited
                    then Nothing
                    else case pvLookupNode presolutionView nid of
                        Just TyBase{} -> Just nid
                        Just TyBottom{} -> Just nid
                        Just TyVar{} ->
                            case pvLookupVarBound presolutionView nid of
                                Just bnd -> go (IntSet.insert key visited) bnd
                                Nothing -> Nothing
                        _ -> Nothing
        in go IntSet.empty start

    argBounds arg = do
        baseC <- resolveBaseBoundCanonical arg
        case lookupNodeIn nodes baseC of
            Just TyBase{} -> Just baseC
            Just TyBottom{} -> Just baseC
            _ -> Nothing

    instArgNode tr binder arg =
        case lookupCopy binder (etCopyMap tr) of
            Just copyN -> copyN
            Nothing -> arg

    traceBinderArgBaseBounds =
        IntMap.map
            ( \tr ->
                IntSet.fromList
                    [ getNodeId baseC
                    | (binder, arg) <- etBinderArgs tr
                    , let argNode = instArgNode tr binder arg
                    , Just baseC <- [argBounds argNode]
                    ]
            )
            edgeTraces

    edgeBaseBounds =
        IntSet.fromList
            [ getNodeId baseC
            | ew <- IntMap.elems edgeWitnesses
            , Just baseC <- [argBounds (ewRight ew)]
            ]

    instArgBaseBounds =
        let binderBounds =
                IntMap.fromListWith
                    IntSet.union
                    [ (getNodeId (canonical binderRoot), IntSet.singleton (getNodeId baseC))
                    | tr <- IntMap.elems edgeTraces
                    , let copyMapInv =
                            IntMap.fromListWith
                                min
                                [ (getNodeId copyN, origKey)
                                | (origKey, copyN) <- IntMap.toList (getCopyMapping (etCopyMap tr))
                                ]
                    , (binder, arg) <- etBinderArgs tr
                    , let binderRoot =
                            case IntMap.lookup (getNodeId binder) copyMapInv of
                                Just origKey -> NodeId origKey
                                Nothing -> binder
                    , let argNode = instArgNode tr binder arg
                    , Just baseC <- [argBounds argNode]
                    ]
        in IntSet.union
            ( IntSet.unions
                [ bounds
                | bounds <- IntMap.elems binderBounds
                , IntSet.size bounds > 1
                ]
            )
            edgeBaseBounds

    rootBounds =
        IntMap.fromListWith
            IntSet.union
            [ (getNodeId (canonical (etRoot tr)), IntSet.singleton (getNodeId baseC))
            | tr <- IntMap.elems edgeTraces
            , (binder, arg) <- etBinderArgs tr
            , let argNode = instArgNode tr binder arg
            , Just baseC <- [argBounds argNode]
            ]

    instArgRootMultiBase =
        any (\s -> IntSet.size s > 1) (IntMap.elems rootBounds)
            || IntSet.size edgeBaseBounds > 1

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
