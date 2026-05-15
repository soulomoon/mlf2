{-# LANGUAGE DataKinds #-}

module MLF.Constraint.Finalize (
    stepSanitizeSnapshotUf,
    stepCanonicalizeConstraint,
    stepPruneSolvedBindParents,
    stepValidateSolvedStrict,
    presolutionViewFromSnapshot,
    presolutionViewFromSolved,
    finalizePresolutionViewFromSnapshot,
    validateCanonicalSnapshotStrict,
    finalizeSolvedFromSnapshot,
    finalizeSolvedForConstraint
) where

import Data.IntMap.Strict (IntMap)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.View
    ( PresolutionView(..)
    , SnapshotPreparation(..)
    , buildPresolutionView
    , prepareSnapshotPreparationFromParts
    )
import MLF.Constraint.Solve (SolveError)
import qualified MLF.Constraint.Solve as Solve
import MLF.Constraint.Solve.Internal (SolveResult(..))
import qualified MLF.Constraint.Solved.Internal as SolvedInternal
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph (Constraint(..), NodeId)
import MLF.Constraint.Types.Phase (Phase(Raw))

stepSanitizeSnapshotUf :: Constraint p -> IntMap NodeId -> IntMap NodeId
stepSanitizeSnapshotUf constraint uf =
    spSanitizedUf (prepareSnapshotPreparationFromParts constraint uf)

stepCanonicalizeConstraint :: Constraint p -> IntMap NodeId -> Constraint p
stepCanonicalizeConstraint constraint uf =
    let ufSanitized = stepSanitizeSnapshotUf constraint uf
    in Solve.repairNonUpperParents (Solve.rewriteConstraintWithUF ufSanitized constraint)

presolutionViewFromSnapshot :: Constraint p -> IntMap NodeId -> PresolutionView p
presolutionViewFromSnapshot constraint uf =
    let prepared = prepareSnapshotPreparationFromParts constraint uf
    in buildPresolutionView prepared (stepCanonicalizeConstraint constraint (spSanitizedUf prepared))

presolutionViewFromSolved :: Solved.Solved -> PresolutionView 'Raw
presolutionViewFromSolved solved =
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

finalizePresolutionViewFromSnapshot :: Constraint p -> IntMap NodeId -> Either SolveError (PresolutionView p)
finalizePresolutionViewFromSnapshot constraint uf = do
    let prepared0 = prepareSnapshotPreparationFromParts constraint uf
    SolveResult { srConstraint = canonicalConstraint, srUnionFind = ufFinal } <-
        Solve.finalizeConstraintWithUF (spSanitizedUf prepared0) constraint
    let preparedFinal = prepareSnapshotPreparationFromParts constraint ufFinal
    pure (buildPresolutionView preparedFinal canonicalConstraint)

validateCanonicalSnapshotStrict :: Constraint p -> IntMap NodeId -> [String]
validateCanonicalSnapshotStrict canonicalConstraint canonicalMap =
    Solve.validateSolvedGraphStrict
        SolveResult
            { srConstraint = canonicalConstraint
            , srUnionFind = canonicalMap
            }

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

finalizeSolvedFromSnapshot :: Constraint p -> IntMap NodeId -> Either SolveError Solved.Solved
finalizeSolvedFromSnapshot constraint uf =
    let ufSanitized = stepSanitizeSnapshotUf constraint uf
    in do
        SolveResult { srConstraint = canonicalConstraint, srUnionFind = ufFinal } <-
            Solve.finalizeConstraintWithUF ufSanitized constraint
        let solved0 = SolvedInternal.fromConstraintAndUf constraint ufFinal
            solved1 = SolvedInternal.rebuildWithConstraint solved0 canonicalConstraint
            solved2 = stepPruneSolvedBindParents solved1
        stepValidateSolvedStrict solved2

finalizeSolvedForConstraint :: Solved.Solved -> Constraint p -> Either SolveError Solved.Solved
finalizeSolvedForConstraint solved constraint =
    let uf = Solved.canonicalMap solved
    in do
        SolveResult { srConstraint = canonicalConstraint } <-
            Solve.finalizeConstraintWithUF uf constraint
        let solved0 = SolvedInternal.rebuildWithConstraint solved canonicalConstraint
            solved1 = stepPruneSolvedBindParents solved0
        stepValidateSolvedStrict solved1
