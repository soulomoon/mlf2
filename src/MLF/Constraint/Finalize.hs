module MLF.Constraint.Finalize (
    stepSanitizeSnapshotUf,
    stepCanonicalizeConstraint,
    stepSolvedFromPresolutionView,
    stepPruneSolvedBindParents,
    stepValidateSolvedStrict,
    presolutionViewFromSnapshot,
    finalizePresolutionViewFromSnapshot,
    finalizeSolvedFromSnapshot,
    finalizeSolvedForConstraint
) where

import Data.IntMap.Strict (IntMap)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution.View
    ( PresolutionView(..)
    , SnapshotPreparation(..)
    , buildPresolutionView
    , prepareSnapshotPreparationFromParts
    )
import MLF.Constraint.Solve (SolveError, SolveResult(..))
import qualified MLF.Constraint.Solve as Solve
import qualified MLF.Constraint.Solved.Internal as SolvedInternal
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types (Constraint, NodeId)

stepSanitizeSnapshotUf :: Constraint -> IntMap NodeId -> IntMap NodeId
stepSanitizeSnapshotUf constraint uf =
    spSanitizedUf (prepareSnapshotPreparationFromParts constraint uf)

stepCanonicalizeConstraint :: Constraint -> IntMap NodeId -> Constraint
stepCanonicalizeConstraint constraint uf =
    let ufSanitized = stepSanitizeSnapshotUf constraint uf
    in Solve.repairNonUpperParents (Solve.rewriteConstraintWithUF ufSanitized constraint)

presolutionViewFromSnapshot :: Constraint -> IntMap NodeId -> PresolutionView
presolutionViewFromSnapshot constraint uf =
    let prepared = prepareSnapshotPreparationFromParts constraint uf
    in buildPresolutionView prepared (stepCanonicalizeConstraint constraint (spSanitizedUf prepared))

finalizePresolutionViewFromSnapshot :: Constraint -> IntMap NodeId -> Either SolveError PresolutionView
finalizePresolutionViewFromSnapshot constraint uf = do
    let prepared0 = prepareSnapshotPreparationFromParts constraint uf
    SolveResult{ srConstraint = canonicalConstraint, srUnionFind = ufFinal } <-
        Solve.finalizeConstraintWithUF (spSanitizedUf prepared0) constraint
    let preparedFinal = prepareSnapshotPreparationFromParts constraint ufFinal
    pure (buildPresolutionView preparedFinal canonicalConstraint)

stepSolvedFromPresolutionView :: PresolutionView -> Solved.Solved
stepSolvedFromPresolutionView presolutionView =
    let constraint = pvConstraint presolutionView
        canonicalMap = pvCanonicalMap presolutionView
        canonicalConstraint = stepCanonicalizeConstraint constraint canonicalMap
        solved0 = SolvedInternal.fromConstraintAndUf constraint canonicalMap
    in SolvedInternal.rebuildWithConstraint solved0 canonicalConstraint

stepPruneSolvedBindParents :: Solved.Solved -> Solved.Solved
stepPruneSolvedBindParents = SolvedInternal.pruneBindParentsSolved

stepValidateSolvedStrict :: Solved.Solved -> Either SolveError Solved.Solved
stepValidateSolvedStrict solved =
    case Binding.checkBindingTree (Solved.canonicalConstraint solved) of
        Left err -> Left (Solve.BindingTreeError err)
        Right () ->
            case Solved.validateCanonicalGraphStrict solved of
                [] -> Right solved
                violations -> Left (Solve.ValidationFailed violations)

finalizeSolvedFromSnapshot :: Constraint -> IntMap NodeId -> Either SolveError Solved.Solved
finalizeSolvedFromSnapshot constraint uf =
    let ufSanitized = stepSanitizeSnapshotUf constraint uf
    in do
        SolveResult{ srConstraint = canonicalConstraint, srUnionFind = ufFinal } <-
            Solve.finalizeConstraintWithUF ufSanitized constraint
        let solved0 = SolvedInternal.fromConstraintAndUf constraint ufFinal
            solved1 = SolvedInternal.rebuildWithConstraint solved0 canonicalConstraint
            solved2 = stepPruneSolvedBindParents solved1
        stepValidateSolvedStrict solved2

finalizeSolvedForConstraint :: Solved.Solved -> Constraint -> Either SolveError Solved.Solved
finalizeSolvedForConstraint solved constraint =
    let uf = Solved.canonicalMap solved
    in do
        SolveResult{ srConstraint = canonicalConstraint } <-
            Solve.finalizeConstraintWithUF uf constraint
        let solved0 = SolvedInternal.rebuildWithConstraint solved canonicalConstraint
            solved1 = stepPruneSolvedBindParents solved0
        stepValidateSolvedStrict solved1
