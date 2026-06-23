module MLF.Reify.Bound (
    reifyBoundWithRefs,
    reifyBoundWithRefsReadModel,
    reifyBoundWithRefsOnConstraint,
    reifyBoundWithRefsBound,
    reifyBoundWithRefsOnConstraintBound,
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
import MLF.Reify.Type.Core (reifyWithReadModelRefs)
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

reifyBoundWithRefsOnConstraintBound :: Constraint p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError BoundType
reifyBoundWithRefsOnConstraintBound constraint subst nid =
    let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
    in reifyBoundWithRefsBound presolutionView subst nid

freeVars :: Solved -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars = ReifyType.freeVars

refForSubstRefs :: (NodeId -> NodeId) -> IntMap.IntMap TypeBinderRef -> NodeId -> TypeBinderRef
refForSubstRefs canonical subst v =
    let cv@(NodeId i) = canonical v
    in fromMaybe
        (typeBinderRefFromIdentity (typeBinderIdentityFromNode cv) ("t" ++ show i))
        (IntMap.lookup i subst)
