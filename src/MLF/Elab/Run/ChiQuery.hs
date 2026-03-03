module MLF.Elab.Run.ChiQuery (
    chiCanonical,
    chiLookupNode,
    chiLookupVarBound,
    chiLookupBindParent,
    chiConstraint,
    chiCanonicalConstraint,
    chiBindParents
) where

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
