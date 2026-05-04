module MLF.Reify.Core (
    reifyType,
    reifyTypeWithNames,
    reifyTypeWithNamesNoFallback,
    reifyTypeWithNamesNoFallbackOnConstraint,
    reifyTypeWithNamedSet,
    reifyTypeWithNamedSetNoFallback,
    reifyWithAs,
    reifyBoundWithNames,
    reifyBoundWithNamesOnConstraint,
    reifyBoundWithNamesBound,
    reifyBoundWithNamesOnConstraintBound,
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
import MLF.Reify.Type (ReifyRoot)
import MLF.Types.Elab (BoundType, ElabType)
import MLF.Util.ElabError (ElabError)

reifyType :: PresolutionView -> NodeId -> Either ElabError ElabType
reifyType = Type.reifyType

reifyTypeWithNames :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNames = Type.reifyTypeWithNames

reifyTypeWithNamesNoFallback :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallback = Type.reifyTypeWithNamesNoFallback

reifyTypeWithNamesNoFallbackOnConstraint :: Constraint -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallbackOnConstraint = Type.reifyTypeWithNamesNoFallbackOnConstraint

reifyTypeWithNamedSet :: PresolutionView -> IntMap.IntMap String -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
reifyTypeWithNamedSet = Type.reifyTypeWithNamedSet

reifyTypeWithNamedSetNoFallback
    :: PresolutionView
    -> IntMap.IntMap String
    -> IntSet.IntSet
    -> NodeId
    -> Either ElabError ElabType
reifyTypeWithNamedSetNoFallback = Type.reifyTypeWithNamedSetNoFallback

reifyWithAs
    :: String
    -> Solved
    -> (NodeId -> String)
    -> (NodeId -> Bool)
    -> ReifyRoot
    -> (ElabType -> Either ElabError a)
    -> NodeId
    -> Either ElabError a
reifyWithAs = Type.reifyWithAs

reifyBoundWithNames :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNames = Bound.reifyBoundWithNames

reifyBoundWithNamesOnConstraint :: Constraint -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNamesOnConstraint = Bound.reifyBoundWithNamesOnConstraint

reifyBoundWithNamesBound :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError BoundType
reifyBoundWithNamesBound = Bound.reifyBoundWithNamesBound

reifyBoundWithNamesOnConstraintBound :: Constraint -> IntMap.IntMap String -> NodeId -> Either ElabError BoundType
reifyBoundWithNamesOnConstraintBound = Bound.reifyBoundWithNamesOnConstraintBound

freeVars :: Solved -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars = Bound.freeVars

namedNodes :: PresolutionView -> Either ElabError IntSet.IntSet
namedNodes = Named.namedNodes
