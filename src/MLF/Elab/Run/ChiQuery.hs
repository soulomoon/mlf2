module MLF.Elab.Run.ChiQuery (
    chiCanonical,
    chiLookupNode,
    chiLookupVarBound,
    chiLookupBindParent,
    chiConstraint,
    chiCanonicalConstraint,
    chiBindParents,
    chiCanonicalBindParents,
    chiSolved
) where

import MLF.Constraint.Presolution (PresolutionView(..))
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types
    ( BindFlag
    , BindParents
    , Constraint
    , NodeId
    , NodeRef
    , TyNode
    , cBindParents
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

chiCanonicalBindParents :: PresolutionView -> BindParents
chiCanonicalBindParents = cBindParents . pvCanonicalConstraint

-- | Build a solved-style adapter view rooted in χp data.
--
-- The original constraint comes from `pvConstraint`; we then overwrite the
-- canonical slice with `pvCanonicalConstraint` so downstream solved-domain
-- queries continue to observe the canonicalized graph.
chiSolved :: PresolutionView -> Solved
chiSolved presolutionView =
    let solved0 =
            Solved.fromConstraintAndUf
                (pvConstraint presolutionView)
                (pvCanonicalMap presolutionView)
    in Solved.rebuildWithConstraint solved0 (pvCanonicalConstraint presolutionView)
