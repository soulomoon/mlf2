module MLF.Reify.Core (
    reifyType,
    reifyTypeWithRefsNoFallback,
    reifyTypeWithRefsNoFallbackOnConstraint,
    reifyBoundWithRefs,
    reifyBoundWithRefsOnConstraint,
    freeVars,
    namedNodes
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Solved (Solved)
import MLF.Constraint.Types.Graph (Constraint, NodeId)
import qualified MLF.Reify.Bound as Bound
import qualified MLF.Reify.Named as Named
import qualified MLF.Reify.Type as Type
import MLF.Types.Elab (ElabType, TypeBinderRef)
import MLF.Util.ElabError (ElabError)

reifyType :: PresolutionView p -> NodeId -> Either ElabError ElabType
reifyType = Type.reifyType

reifyTypeWithRefsNoFallback :: PresolutionView p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
reifyTypeWithRefsNoFallback = Type.reifyTypeWithRefsNoFallback

reifyTypeWithRefsNoFallbackOnConstraint :: Constraint p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
reifyTypeWithRefsNoFallbackOnConstraint = Type.reifyTypeWithRefsNoFallbackOnConstraint

reifyBoundWithRefs :: PresolutionView p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
reifyBoundWithRefs = Bound.reifyBoundWithRefs

reifyBoundWithRefsOnConstraint :: Constraint p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
reifyBoundWithRefsOnConstraint = Bound.reifyBoundWithRefsOnConstraint

freeVars :: Solved -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars = Bound.freeVars

namedNodes :: PresolutionView p -> Either ElabError IntSet.IntSet
namedNodes = Named.namedNodes
