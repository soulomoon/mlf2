module MLF.Elab.Run.ChiQuery (
    chiCanonical,
    chiCanonicalMap,
    chiLookupNode,
    chiLookupVarBound,
    chiLookupBindParent,
    chiConstraint,
    chiCanonicalConstraint,
    chiBindParents,
    chiCanonicalBindParents,
    chiSolvedCompat,
    chiSolved
) where

import qualified Data.IntMap.Strict as IntMap
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

chiCanonicalBindParents :: PresolutionView -> BindParents
chiCanonicalBindParents = cBindParents . pvCanonicalConstraint

-- | Build a solved-style adapter view rooted in χp data.
--
-- The original constraint comes from `pvConstraint`; we then overwrite the
-- canonical slice with `pvCanonicalConstraint` so downstream solved-domain
-- queries continue to observe the canonicalized graph.
chiSolvedCompat :: PresolutionView -> Solved
chiSolvedCompat presolutionView =
    let solved0 =
            Solved.fromConstraintAndUf
                (chiConstraint presolutionView)
                (chiCanonicalMap presolutionView)
    in Solved.rebuildWithConstraint solved0 (chiCanonicalConstraint presolutionView)

chiSolved :: PresolutionView -> Solved
chiSolved = chiSolvedCompat
