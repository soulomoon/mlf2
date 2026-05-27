module MLF.Reify.Bound (
    reifyBoundWithNames,
    reifyBoundWithNamesReadModel,
    reifyBoundWithNamesOnConstraint,
    reifyBoundWithNamesBound,
    reifyBoundWithNamesOnConstraintBound,
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
import MLF.Reify.Type.Core (reifyWithReadModel)
import MLF.Reify.Type (ReifyRoot(..), reifyWith, reifyWithAs)
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError(..))

reifyBoundWithNames :: PresolutionView p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNames presolutionView subst =
    reifyWith "reifyBoundWithNames" presolutionView varNameFor isNamed RootBound
  where
    canonical = pvCanonical presolutionView

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst

reifyBoundWithNamesReadModel :: ElabReadModel p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNamesReadModel readModel subst =
    reifyWithReadModel "reifyBoundWithNames" readModel varNameFor isNamed RootBound
  where
    presolutionView = ermPresolutionView readModel
    canonical = pvCanonical presolutionView

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst


reifyBoundWithNamesBound :: PresolutionView p -> IntMap.IntMap String -> NodeId -> Either ElabError BoundType
reifyBoundWithNamesBound presolutionView subst =
    reifyWithAs "reifyBoundWithNamesBound" presolutionView varNameFor isNamed RootBound
        (\ty -> either (Left . InstantiationError) Right (elabToBound ty))
  where
    canonical = pvCanonical presolutionView

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst

-- | Reify a node for use as a binder bound on an explicit constraint (Schi_p on base graphs).
reifyBoundWithNamesOnConstraint :: Constraint p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNamesOnConstraint constraint subst nid =
    let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
    in reifyBoundWithNames presolutionView subst nid

reifyBoundWithNamesOnConstraintBound :: Constraint p -> IntMap.IntMap String -> NodeId -> Either ElabError BoundType
reifyBoundWithNamesOnConstraintBound constraint subst nid =
    let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
    in reifyBoundWithNamesBound presolutionView subst nid

freeVars :: Solved -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars = ReifyType.freeVars
