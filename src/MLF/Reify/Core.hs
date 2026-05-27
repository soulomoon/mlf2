module MLF.Reify.Core (
    reifyType,
    reifyTypeWithNames,
    reifyTypeWithNamesNoFallback,
    reifyTypeWithNamesNoFallbackOnConstraint,
    reifyTypeWithNamedSet,
    reifyTypeWithNamedSetNoFallback,
    reifyTypeWithNamesNoFallbackReadModel,
    reifyTypeWithNamedSetNoFallbackReadModel,
    reifyWithAs,
    reifyBoundWithNames,
    reifyBoundWithNamesReadModel,
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
import MLF.Elab.ReadModel (ElabReadModel)
import qualified MLF.Reify.Bound as Bound
import qualified MLF.Reify.Named as Named
import qualified MLF.Reify.Type as Type
import MLF.Reify.Type (ReifyRoot)
import MLF.Types.Elab (BoundType, ElabType)
import MLF.Util.ElabError (ElabError)

reifyType :: PresolutionView p -> NodeId -> Either ElabError ElabType
reifyType = Type.reifyType

reifyTypeWithNames :: PresolutionView p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNames = Type.reifyTypeWithNames

reifyTypeWithNamesNoFallback :: PresolutionView p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallback = Type.reifyTypeWithNamesNoFallback

reifyTypeWithNamesNoFallbackOnConstraint :: Constraint p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallbackOnConstraint = Type.reifyTypeWithNamesNoFallbackOnConstraint

reifyTypeWithNamedSet :: PresolutionView p -> IntMap.IntMap String -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
reifyTypeWithNamedSet = Type.reifyTypeWithNamedSet

reifyTypeWithNamedSetNoFallback
    :: PresolutionView p
    -> IntMap.IntMap String
    -> IntSet.IntSet
    -> NodeId
    -> Either ElabError ElabType
reifyTypeWithNamedSetNoFallback = Type.reifyTypeWithNamedSetNoFallback

reifyTypeWithNamesNoFallbackReadModel
    :: ElabReadModel p
    -> IntMap.IntMap String
    -> NodeId
    -> Either ElabError ElabType
reifyTypeWithNamesNoFallbackReadModel = Type.reifyTypeWithNamesNoFallbackReadModel

reifyTypeWithNamedSetNoFallbackReadModel
    :: ElabReadModel p
    -> IntMap.IntMap String
    -> IntSet.IntSet
    -> NodeId
    -> Either ElabError ElabType
reifyTypeWithNamedSetNoFallbackReadModel = Type.reifyTypeWithNamedSetNoFallbackReadModel

reifyWithAs
    :: String
    -> PresolutionView p
    -> (NodeId -> String)
    -> (NodeId -> Bool)
    -> ReifyRoot
    -> (ElabType -> Either ElabError a)
    -> NodeId
    -> Either ElabError a
reifyWithAs = Type.reifyWithAs

reifyBoundWithNames :: PresolutionView p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNames = Bound.reifyBoundWithNames

reifyBoundWithNamesReadModel :: ElabReadModel p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNamesReadModel = Bound.reifyBoundWithNamesReadModel

reifyBoundWithNamesOnConstraint :: Constraint p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyBoundWithNamesOnConstraint = Bound.reifyBoundWithNamesOnConstraint

reifyBoundWithNamesBound :: PresolutionView p -> IntMap.IntMap String -> NodeId -> Either ElabError BoundType
reifyBoundWithNamesBound = Bound.reifyBoundWithNamesBound

reifyBoundWithNamesOnConstraintBound :: Constraint p -> IntMap.IntMap String -> NodeId -> Either ElabError BoundType
reifyBoundWithNamesOnConstraintBound = Bound.reifyBoundWithNamesOnConstraintBound

freeVars :: Solved -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars = Bound.freeVars

namedNodes :: PresolutionView p -> Either ElabError IntSet.IntSet
namedNodes = Named.namedNodes
