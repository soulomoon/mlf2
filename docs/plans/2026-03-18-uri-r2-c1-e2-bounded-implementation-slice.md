# `E2` Bounded Retained-Child Implementation Slice

Date: 2026-03-19
Round: `round-039`
Roadmap item: `E2`
Stage: `implement`
Attempt: `attempt-2`
Retry state: `rejected attempt-1 -> retry`
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded retained-child result-type hardening slice

## Stage Contract Freeze

This artifact implements only roadmap item `E2` for `attempt-2`.

The live subject remains repaired `URI-R2-C1` only. The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

The slice stays bounded to the retained-child branch in
`src/MLF/Elab/Run/ResultType/Fallback.hs:530-674`. It does not reopen replay repair,
`MLF.Elab.Inst`, `InstBot`, equi-recursive reasoning, cyclic structural encoding,
cross-family widening, or compatibility/default-path widening.

## Implemented Change

- `Fallback.hs` now computes retained-child candidates for the accepted local-binding lane
  by checking whether a candidate child stays in the same canonical local `TypeRef` scope
  as `scopeRootPost`, instead of limiting the retained-child scan to the previous direct
  parent-only path.
- `boundHasForallFrom` remains the fail-closed nested-`forall` / nested-owner gate, so
  retained-child candidates still reject when they cross a nested scheme-root boundary.
- The focused `ARI-C1 feasibility characterization (bounded prototype-only)` block in
  `PipelineSpec.hs` now records:
  - a behavioral retained-child success example that rewires one bounded
    `computeResultTypeFallback` input into the selected same-lane local `TypeRef` root so
    the retained child keeps a recursive-looking result in the allowed lane;
  - a source-level guard that the retained-child lookup stays bounded to the same local
    `TypeRef` lane; and
  - a matched fail-closed contrast for the nested-`forall` / nested-owner rejection path.

## Files Changed

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`
- `orchestrator/rounds/round-039/implementation-notes.md`

## Focused Verification

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-039`

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass:
  - `9 examples, 0 failures`

Full-repo verification is recorded in the round implementation notes after the mandatory
`cabal build all && cabal test` gate completes.
