module MLF.Elab.Run.ChiQuery (
    chiCanonical,
    chiCanonicalMap,
    chiLookupNode,
    chiLookupVarBound,
    chiLookupBindParent,
    chiConstraint,
    chiCanonicalConstraint,
    chiBindParents
) where

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Presolution (PresolutionView(..))
import MLF.Constraint.Types
    ( BindFlag
    , BindParents
    , Constraint
    , NodeId
    , NodeRef
    , TyNode
    )

chiCanonical :: PresolutionView -> NodeId -> NodeId
chiCanonical = pvCanonical

chiCanonicalMap :: PresolutionView -> IntMap.IntMap NodeId
chiCanonicalMap = pvCanonicalMap

chiLookupNode :: PresolutionView -> NodeId -> Maybe TyNode
chiLookupNode = pvLookupNode

chiLookupVarBound :: PresolutionView -> NodeId -> Maybe NodeId
chiLookupVarBound = pvLookupVarBound

chiLookupBindParent :: PresolutionView -> NodeRef -> Maybe (NodeRef, BindFlag)
chiLookupBindParent = pvLookupBindParent

chiConstraint :: PresolutionView -> Constraint
chiConstraint = pvConstraint

chiCanonicalConstraint :: PresolutionView -> Constraint
chiCanonicalConstraint = pvCanonicalConstraint

chiBindParents :: PresolutionView -> BindParents
chiBindParents = pvBindParents
