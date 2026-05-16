{-# LANGUAGE DataKinds #-}

module MLF.Elab.Run.Generalize.Prepare.TestSupport (
    PreparedGeneralizationArtifactTestView(..),
    preparedGeneralizationArtifactTestView,
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph (Constraint, NodeId)
import MLF.Constraint.Types.Phase (Phase(Presolved))
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Run.Generalize.Prepare.Internal (PreparedGeneralizationArtifact(..))

data PreparedGeneralizationArtifactTestView = PreparedGeneralizationArtifactTestView
    { preparedTestBaseConstraint :: Constraint 'Presolved
    , preparedTestSolvedToBase :: IntMap.IntMap NodeId
    , preparedTestCanonicalizeNode :: NodeId -> NodeId
    , preparedTestRedirects :: IntMap.IntMap NodeId
    }

preparedGeneralizationArtifactTestView
    :: PreparedGeneralizationArtifact
    -> PreparedGeneralizationArtifactTestView
preparedGeneralizationArtifactTestView artifact =
    let GaBindParents
            { gaBaseConstraint = baseConstraint
            , gaSolvedToBase = solvedToBase
            } = pgaBindParentsGa artifact
     in PreparedGeneralizationArtifactTestView
            { preparedTestBaseConstraint = baseConstraint
            , preparedTestSolvedToBase = solvedToBase
            , preparedTestCanonicalizeNode = pgaAnnNodeCanonical artifact
            , preparedTestRedirects = pgaRedirects artifact
            }
