{-# LANGUAGE DataKinds #-}

module MLF.Constraint.Finalize.Internal (
    sanitizeSnapshotUf,
    canonicalizeConstraint,
    buildPresolutionViewFromSolved,
    pruneSolvedBindParents,
    validateSolvedStrict
) where

import Data.IntMap.Strict (IntMap)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.View
    ( PresolutionView(..)
    , prepareSnapshotPreparationFromParts
    , spSanitizedUf
    )
import MLF.Constraint.Solve (SolveError)
import qualified MLF.Constraint.Solve as Solve
import qualified MLF.Constraint.Solved.Internal as SolvedInternal
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph (Constraint(..), NodeId)
import MLF.Constraint.Types.Phase (Phase(Raw))

sanitizeSnapshotUf :: Constraint p -> IntMap NodeId -> IntMap NodeId
sanitizeSnapshotUf constraint uf =
    spSanitizedUf (prepareSnapshotPreparationFromParts constraint uf)

canonicalizeConstraint :: Constraint p -> IntMap NodeId -> Constraint p
canonicalizeConstraint constraint uf =
    let ufSanitized = sanitizeSnapshotUf constraint uf
    in Solve.repairNonUpperParents (Solve.rewriteConstraintWithUF ufSanitized constraint)

buildPresolutionViewFromSolved :: Solved.Solved -> PresolutionView 'Raw
buildPresolutionViewFromSolved solved =
    let constraint = Solved.originalConstraint solved
        canonical = Solved.canonical solved
    in PresolutionView
        { pvConstraint = constraint
        , pvCanonicalMap = Solved.canonicalMap solved
        , pvCanonical = canonical
        , pvLookupNode = \nid -> NodeAccess.lookupNode constraint (canonical nid)
        , pvLookupVarBound = \nid -> NodeAccess.lookupVarBound constraint (canonical nid)
        , pvLookupBindParent = NodeAccess.lookupBindParent constraint
        , pvBindParents = cBindParents constraint
        , pvCanonicalConstraint = Solved.canonicalConstraint solved
        }

pruneSolvedBindParents :: Solved.Solved -> Solved.Solved
pruneSolvedBindParents = SolvedInternal.pruneBindParentsSolved

validateSolvedStrict :: Solved.Solved -> Either SolveError Solved.Solved
validateSolvedStrict solved =
    case Binding.checkBindingTree (Solved.canonicalConstraint solved) of
        Left err -> Left (Solve.BindingTreeError err)
        Right () ->
            case Solved.validateCanonicalGraphStrict solved of
                [] -> Right solved
                violations -> Left (Solve.ValidationFailed violations)
