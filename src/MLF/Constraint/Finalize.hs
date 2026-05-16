{-# LANGUAGE DataKinds #-}

module MLF.Constraint.Finalize (
    presolutionViewFromSnapshot,
    finalizePresolutionViewFromSnapshot,
    validateCanonicalSnapshotStrict,
    finalizeSolvedFromSnapshot,
    finalizeSolvedForConstraint
) where

import Data.IntMap.Strict (IntMap)

import qualified MLF.Constraint.Finalize.Internal as Internal
import MLF.Constraint.Presolution.View
    ( PresolutionView
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

presolutionViewFromSnapshot :: Constraint p -> IntMap NodeId -> PresolutionView p
presolutionViewFromSnapshot constraint uf =
    let prepared = prepareSnapshotPreparationFromParts constraint uf
    in buildPresolutionView prepared (Internal.canonicalizeConstraint constraint (spSanitizedUf prepared))

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

finalizeSolvedFromSnapshot :: Constraint p -> IntMap NodeId -> Either SolveError Solved.Solved
finalizeSolvedFromSnapshot constraint uf =
    let ufSanitized = Internal.sanitizeSnapshotUf constraint uf
    in do
        SolveResult { srConstraint = canonicalConstraint, srUnionFind = ufFinal } <-
            Solve.finalizeConstraintWithUF ufSanitized constraint
        let solved0 = SolvedInternal.fromConstraintAndUf constraint ufFinal
            solved1 = SolvedInternal.rebuildWithConstraint solved0 canonicalConstraint
            solved2 = Internal.pruneSolvedBindParents solved1
        Internal.validateSolvedStrict solved2

finalizeSolvedForConstraint :: Solved.Solved -> Constraint p -> Either SolveError Solved.Solved
finalizeSolvedForConstraint solved constraint =
    let uf = Solved.canonicalMap solved
    in do
        SolveResult { srConstraint = canonicalConstraint } <-
            Solve.finalizeConstraintWithUF uf constraint
        let solved0 = SolvedInternal.rebuildWithConstraint solved canonicalConstraint
            solved1 = Internal.pruneSolvedBindParents solved0
        Internal.validateSolvedStrict solved1
