# CI Test Matrix and Failure Repair — Roadmap

## Context

- This roadmap family succeeds the completed March 29 non-local proxy
  elaboration campaign. The live controller is terminal there; this is a new
  repo-level CI campaign, not a continuation of the recursive-type repair work.
- Current CI coverage is only
  `.github/workflows/thesis-conformance.yml`, a single `ubuntu-latest` lane on
  `ghc-version: 9.12.2`.
- Current local verification baseline on this checkout:
  - `cabal build all && cabal test`: PASS (`1177 examples, 0 failures`)
  - `./scripts/thesis-conformance-gate.sh`: FAIL because
    `/Volumes/src/mlf4/docs/thesis-obligations.md` is out of date
- The thesis-conformance gate is shell-driven. A bounded Unix-first matrix is
  honest immediately; Windows must remain explicitly out of scope unless the
  supporting commands are made Windows-compatible.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Freeze bounded matrix scope and repair the current thesis-conformance baseline
   Item id: `item-1`
   Depends on: none
   Parallel safe: no
   Parallel group: none
   Merge after: none
   Completion notes: complete when:
   - the selected round records the bounded matrix decision explicitly,
     including exact included lanes and any explicit exclusions (for example,
     Windows remaining out of scope while shell-driven gates stay Unix-only)
   - the existing thesis-conformance failure is repaired at the source of
     truth rather than bypassed, including the current
     `/Volumes/src/mlf4/docs/thesis-obligations.md` drift if that is still the
     active blocker
   - `cabal build all && cabal test` passes
   - `./scripts/thesis-conformance-gate.sh` passes
   - no GitHub Actions matrix expansion lands yet beyond any minimal change
     needed to support the repaired baseline

2. [done] Add a bounded GitHub Actions build-and-test matrix around the repaired baseline
   Item id: `item-2`
   Depends on: `item-1`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-1`
   Completion notes: complete when:
   - `.github/workflows/` defines the selected bounded matrix from item `1`
   - the workflow reuses existing repo commands instead of inventing CI-only
     verification logic
   - the matrix scope stays honest about runner support and does not promise
     unsupported lanes
   - the thesis-conformance gate remains present as a dedicated job or an
     explicitly justified equivalent
   - `cabal build all && cabal test` passes
   - `./scripts/thesis-conformance-gate.sh` passes

3. [done] Fix any matrix-exposed runner or test failures at the root cause
   Item id: `item-3`
   Depends on: `item-2`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-2`
   Completion notes: complete when:
   - every newly exposed failure has a bounded reproducer and a concrete root
     cause explanation
   - code, workflow, or generated-artifact fixes avoid compatibility shims and
     CI-only bypasses
   - each real bug fix adds or updates focused regression coverage when tests
     are the right enforcement point
   - if the bounded matrix lands green without exposing new failures, this
     item is marked done with explicit evidence and a no-op explanation
   - `cabal build all && cabal test` passes
   - `./scripts/thesis-conformance-gate.sh` passes

4. [done] Update repo guidance and handoff for ongoing CI maintenance
   Item id: `item-4`
   Depends on: `item-2`, `item-3`
   Parallel safe: no
   Parallel group: none
   Merge after: `item-3`
   Completion notes: complete when:
   - `README.md`, `TODO.md`, and `CHANGELOG.md` reflect the new matrix and any
     consciously bounded runner support
   - the handoff records which commands remain authoritative locally vs in CI
   - any remaining out-of-scope runner limitations are documented explicitly
   - `cabal build all && cabal test` passes
   - `./scripts/thesis-conformance-gate.sh` passes
