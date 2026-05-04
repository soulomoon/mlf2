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

chiCanonical :: PresolutionView -> NodeId -> NodeId
chiCanonical = pvCanonical

chiCanonicalMap :: PresolutionView -> IntMap.IntMap NodeId
chiCanonicalMap = pvCanonicalMap

chiLookupNode :: PresolutionView -> NodeId -> Maybe TyNode
chiLookupNode = pvLookupNode

chiLookupVarBound :: PresolutionView -> NodeId -> Maybe NodeId
chiLookupVarBound = pvLookupVarBound

chiConstraint :: PresolutionView -> Constraint
chiConstraint = pvConstraint

chiCanonicalConstraint :: PresolutionView -> Constraint
chiCanonicalConstraint = pvCanonicalConstraint

