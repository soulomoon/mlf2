module MLF.Reify.Bound (
    reifyBoundWithRefs,
    reifyBoundWithRefsReadModel,
    reifyBoundWithRefsOnConstraint,
    reifyBoundWithExternalRefs,
    reifyBoundWithExternalRefsOnConstraint,
    reifyBoundWithRefsBound,
    reifyBoundWithRefsOnConstraintBound,
    structuralBinders,
    freeVars
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)

import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Presolution.View (PresolutionView (..))
import MLF.Constraint.Solved (Solved)
import MLF.Constraint.Types.Graph
import MLF.Elab.ReadModel (ElabReadModel, ermPresolutionView)
import qualified MLF.Reify.Type as ReifyType
import MLF.Reify.Type.Core (reifyWithExternalRefs, reifyWithReadModelRefs)
import MLF.Reify.Type (ReifyRoot(..), reifyWithRefs, reifyWithAsRefs)
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError(..))

reifyBoundWithRefs :: PresolutionView p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
reifyBoundWithRefs presolutionView subst =
    reifyWithRefs "reifyBoundWithRefs" presolutionView refForVar isNamed RootBound
  where
    canonical = pvCanonical presolutionView

    refForVar =
        refForSubstRefs canonical subst

    isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst

reifyBoundWithRefsReadModel :: ElabReadModel p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
reifyBoundWithRefsReadModel readModel subst =
    reifyWithReadModelRefs "reifyBoundWithRefs" readModel refForVar isNamed RootBound
  where
    presolutionView = ermPresolutionView readModel
    canonical = pvCanonical presolutionView

    refForVar =
        refForSubstRefs canonical subst

    isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst

reifyBoundWithRefsBound :: PresolutionView p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError BoundType
reifyBoundWithRefsBound presolutionView subst =
    reifyWithAsRefs "reifyBoundWithNamesBound" presolutionView refForVar isNamed RootBound
        (\ty -> either (Left . InstantiationError) Right (elabToBound ty))
  where
    canonical = pvCanonical presolutionView

    refForVar =
        refForSubstRefs canonical subst

    isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst

reifyBoundWithRefsOnConstraint :: Constraint p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
reifyBoundWithRefsOnConstraint constraint subst nid =
    let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
    in reifyBoundWithRefs presolutionView subst nid

reifyBoundWithExternalRefs ::
    PresolutionView p ->
    IntMap.IntMap TypeBinderRef ->
    IntSet.IntSet ->
    IntMap.IntMap [NodeId] ->
    NodeId ->
    Either ElabError ElabType
reifyBoundWithExternalRefs presolutionView subst externalKeys structuralBinderMap =
    reifyWithExternalRefs
        "reifyBoundWithExternalRefs"
        presolutionView
        (refForSubstRefs canonical subst)
        (\nodeId -> IntMap.member (getNodeId (canonical nodeId)) subst)
        (\nodeId -> IntSet.member (getNodeId (canonical nodeId)) externalKeys)
        structuralBinderMap
        RootBound
  where
    canonical = pvCanonical presolutionView

reifyBoundWithExternalRefsOnConstraint ::
    Constraint p ->
    IntMap.IntMap TypeBinderRef ->
    IntSet.IntSet ->
    IntMap.IntMap [NodeId] ->
    NodeId ->
    Either ElabError ElabType
reifyBoundWithExternalRefsOnConstraint constraint subst externalKeys structuralBinderMap nid =
    let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
    in reifyBoundWithExternalRefs presolutionView subst externalKeys structuralBinderMap nid

reifyBoundWithRefsOnConstraintBound :: Constraint p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError BoundType
reifyBoundWithRefsOnConstraintBound constraint subst nid =
    let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
    in reifyBoundWithRefsBound presolutionView subst nid

-- | Recover structural binder children whose source identity is also carried
-- by their enclosing @forall@ or @mu@ node.  Source translation records both
-- nodes with one identity so reification can reconstruct the binder at its
-- structural owner without mistaking that owner for a free variable.
structuralSourceBinders ::
    Constraint p ->
    BindParents ->
    IntMap.IntMap TypeBinderRef ->
    IntMap.IntMap [NodeId]
structuralSourceBinders constraint bindParents sourceRefs =
    IntMap.fromListWith (++)
        [ (getNodeId parent, [NodeId childKey])
        | (childKey, childRef) <- IntMap.toList sourceRefs
        , Just (TypeRef parent, _) <-
            [IntMap.lookup (nodeRefKey (typeRef (NodeId childKey))) bindParents]
        , Just owner <- [lookupNodeIn (cNodes constraint) parent]
        , isStructuralOwner owner
        , Just ownerRef <- [IntMap.lookup (getNodeId parent) sourceRefs]
        , typeBinderRefsSameIdentity childRef ownerRef
        ]
  where
    isStructuralOwner node =
        case node of
            TyForall {} -> True
            TyMu {} -> True
            _ -> False

-- | Recover every structural binder whose declaration is proved either by
-- source identity metadata or directly by the frozen binding tree.  The
-- latter route is required for graph-owned recursive binders: solving may
-- reparent their live occurrence and source annotations need not publish a
-- sidecar entry, but the frozen @mu -> self@ rigid edge still owns the
-- declaration.  Keeping that proof here prevents reification from turning a
-- locally bound recursive self into a free Gamma dependency.
structuralBinders ::
    Constraint p ->
    BindParents ->
    IntMap.IntMap TypeBinderRef ->
    IntMap.IntMap [NodeId]
structuralBinders constraint bindParents sourceRefs =
    IntMap.unionWith mergeChildren
        (structuralSourceBinders constraint bindParents sourceRefs)
        graphOwnedBinders
  where
    graphOwnedBinders =
        IntMap.fromListWith mergeChildren
            [ (getNodeId parent, [child])
            | (childKey, (TypeRef parent, flag)) <- IntMap.toList bindParents
            , TypeRef child <- [nodeRefFromKey childKey]
            , Just TyVar {} <- [lookupNodeIn (cNodes constraint) child]
            , Just owner <- [lookupNodeIn (cNodes constraint) parent]
            , ownerOwnsBinder flag owner
            ]

    ownerOwnsBinder flag owner =
        case owner of
            TyForall {} -> flag == BindFlex
            TyMu {} -> flag == BindRigid
            _ -> False

    mergeChildren left right = foldr insertStructuralNode right left
    insertStructuralNode node nodes
        | node `elem` nodes = nodes
        | otherwise = node : nodes

freeVars :: Solved -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars = ReifyType.freeVars

refForSubstRefs :: (NodeId -> NodeId) -> IntMap.IntMap TypeBinderRef -> NodeId -> TypeBinderRef
refForSubstRefs canonical subst v =
    let cv@(NodeId i) = canonical v
    in fromMaybe
        (typeBinderRefFromIdentity (typeBinderIdentityFromNode cv) ("t" ++ show i))
        (IntMap.lookup i subst)
