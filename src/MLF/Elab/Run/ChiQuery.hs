module MLF.Elab.Run.ChiQuery (
    chiCanonical,
    chiCanonicalMap,
    chiLookupNode,
    chiLookupVarBound,
    chiConstraint,
    chiCanonicalConstraint
) where

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Presolution (PresolutionView(..))
import MLF.Constraint.Types.Graph
    ( Constraint
    , NodeId
    , TyNode
    )

chiCanonical :: PresolutionView p -> NodeId -> NodeId
chiCanonical = pvCanonical

chiCanonicalMap :: PresolutionView p -> IntMap.IntMap NodeId
chiCanonicalMap = pvCanonicalMap

chiLookupNode :: PresolutionView p -> NodeId -> Maybe TyNode
chiLookupNode = pvLookupNode

chiLookupVarBound :: PresolutionView p -> NodeId -> Maybe NodeId
chiLookupVarBound = pvLookupVarBound

chiConstraint :: PresolutionView p -> Constraint p
chiConstraint = pvConstraint

chiCanonicalConstraint :: PresolutionView p -> Constraint p
chiCanonicalConstraint = pvCanonicalConstraint

