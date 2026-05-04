module MLF.Reify.Bound (
    reifyBoundWithNames,
    reifyBoundWithNamesOnConstraint,
    reifyBoundWithNamesBound,
    reifyBoundWithNamesOnConstraintBound,
    freeVars
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)

import MLF.Constraint.Finalize (presolutionViewFromSnapshot)
import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution
import qualified MLF.Reify.Type as ReifyType
import MLF.Reify.Type (ReifyRoot(..), reifyWith, reifyWithAs, solvedFromView)
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError(..))

reifyBoundWithNamesSolved
    :: Solved
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError ElabType
reifyBoundWithNamesSolved solved subst =
    reifyWith "reifyBoundWithNames" solved varNameFor isNamed RootBound
  where
    canonical = Solved.canonical solved

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst

reifyBoundWithNames :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNames presolutionView subst nid =
    reifyBoundWithNamesSolved (solvedFromView presolutionView) subst nid


reifyBoundWithNamesBound :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError BoundType
reifyBoundWithNamesBound presolutionView subst nid =
    reifyBoundWithNamesBoundSolved (solvedFromView presolutionView) subst nid

reifyBoundWithNamesBoundSolved
    :: Solved
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError BoundType
reifyBoundWithNamesBoundSolved solved subst =
    reifyWithAs "reifyBoundWithNamesBound" solved varNameFor isNamed RootBound
        (\ty -> either (Left . InstantiationError) Right (elabToBound ty))
  where
    canonical = Solved.canonical solved

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
        let cv = canonical v
        in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst

-- | Reify a node for use as a binder bound on an explicit constraint (Schi_p on base graphs).
reifyBoundWithNamesOnConstraint :: Constraint -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNamesOnConstraint constraint subst nid =
    let presolutionView = presolutionViewFromSnapshot constraint IntMap.empty
    in reifyBoundWithNames presolutionView subst nid

reifyBoundWithNamesOnConstraintBound :: Constraint -> IntMap.IntMap String -> NodeId -> Either ElabError BoundType
reifyBoundWithNamesOnConstraintBound constraint subst nid =
    let presolutionView = presolutionViewFromSnapshot constraint IntMap.empty
    in reifyBoundWithNamesBound presolutionView subst nid

freeVars :: Solved -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars = ReifyType.freeVars
