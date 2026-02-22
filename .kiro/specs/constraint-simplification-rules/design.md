# Design Document

## Overview
The Chapter 12.4 constraint simplification rules (Var-Abs, Var-Let) are already
realized implicitly through constraint generation and presolution. This spec
adds thesis-auditable documentation (GHC-style Notes) and per-rule tests to
make the correspondence explicit without restructuring the implementation.

ML-Extrude (§12.4.2) is intentionally omitted: it applies only to ML
constraints and would weaken MLF principal solutions.

## Architecture
No new modules. Add Note references at existing implementation sites and
tests in the existing test infrastructure.

## Components

### Notes (documentation only)
- `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - Note [Constraint simplification: Var-Abs (Ch 12.4.1)]
    at lambda parameter binding (lines ~146-159)
- `src/MLF/Constraint/Presolution/Base.hs`
  - Note [Constraint simplification: Var-Let (Ch 12.4.1)]
    at `dropTrivialSchemeEdges` (lines ~671-684)
- `src/MLF/Constraint/Presolution/Expansion.hs`
  - Note [Constraint simplification: Var-Abs degenerate forall (Ch 12.4.1)]
    at degenerate forall handling (lines ~130-163)
- One of the above files or a central location:
  - Note [ML-Extrude omitted (Ch 12.4.2)] explaining why

### Tests
- Add to existing `test/PipelineSpec.hs` or `test/ElaborationSpec.hs`:
  - Var-Abs: lambda param yields monomorphic binding (no gen node)
  - Var-Let: trivial let expansion is identity

## References
- `papers/these-finale-english.txt` §12.4 (lines 13023-13382)
- Lemma 12.4.1 (Var-Abs), Lemma 12.4.2 (Var-Let), Lemma 12.4.3 (ML-Extrude)
