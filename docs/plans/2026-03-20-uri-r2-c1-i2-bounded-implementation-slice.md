# `I2` Bounded Implementation Slice For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-055`
Roadmap item: `I2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded implementation result

## Stage Result

This artifact records the bounded `I2` implementation frozen by the accepted
`I1` bind in
`docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`.

The new `I2` changes stayed inside the selected local single-base slice only:

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`

No other production path was widened. In particular, `I2` did not add a new
retained-target family and did not reopen replay work, `MLF.Elab.Inst`,
`InstBot`, `boundVarTarget` widening, `boundTarget` overlay materialization,
`schemeBodyTarget` consolidation, `ResultType.View`, or non-local recursive
inference.

Pre-existing controller-preparation files remained present in the worktree and
were left untouched:

- `orchestrator/state.json`
- `orchestrator/rounds/round-055/plan.md`
- `orchestrator/rounds/round-055/selection.md`

## Implemented Change

`Fallback.hs` now makes the selected proof explicit as:

- `rootBindingIsLocalType`
- singleton `rootBoundCandidates`
- `not rootHasMultiInst`
- `not instArgRootMultiBase`

That proof is named `rootLocalSingleBase` and is used to gate the selected
`baseTarget -> baseC` `targetC` route. The accepted inherited families remain
separate:

- `rootLocalSchemeAliasBaseLike`
- `rootLocalMultiInst`
- `rootLocalInstArgMultiBase`
- `boundVarTarget`

The only preserved pre-existing `baseTarget` consumer outside the selected
lane is the already-accepted scheme-alias/base-like path, which stays on its
prior route so `I2` does not regress predecessor evidence.

## Focused Evidence

Inside the existing
`ARI-C1 feasibility characterization (bounded prototype-only)` block,
`PipelineSpec.hs` now covers:

- one bounded local single-base success case where the root stays on the local
  `TypeRef` lane, `rootBoundCandidates` collapses to exactly one `Int`
  candidate, and the result is the concrete selected base `TBase Int`;
- one matched non-local contrast where the same wrapper leaves the admissible
  local lane and stays on the quantified fail-closed shell; and
- one source guard that names `rootLocalSingleBase` and verifies `targetC`
  still consults the selected `baseTarget` lane before retained-target and
  `schemeBodyTarget` fallbacks.

The helper for the selected lane explicitly keeps:

- `rootHasMultiInst = False`
- `instArgRootMultiBase = False`
- `rootLocalSchemeAliasBaseLike = False`
- `boundVarTarget = False`

so the positive case is attributable to the selected single-base family only.

## Continuity And Exclusions

`/Volumes/src/mlf4/Bugs.md` remains continuity context only for this round.
Its `Open` section is still empty, and `BUG-2026-03-16-001` remains resolved
only. `I2` therefore does not treat bug state as authority for replay reopen
or any widening beyond the selected local single-base slice.

Preserved exclusions:

- no replay reopen;
- no `MLF.Elab.Inst` or `InstBot` work;
- no `boundVarTarget` widening;
- no `boundTarget` overlay materialization;
- no `schemeBodyTarget` consolidation;
- no `ResultType.View` edits;
- no non-local widening or broader recursive-inference search; and
- no compatibility fallback, convenience shim, or second interface.

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-055`

### TDD Red

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> fail before the production change:
  - the new non-local single-base contrast incorrectly returned `TBase Int`;
  - the new source-guard assertion for `rootLocalSingleBase` was absent.

### TDD Green

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass after adding `rootLocalSingleBase` and gating the selected `targetC`
  route.

### Baseline And Anchor Checks

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass
- `rg -n 'rootLocalSingleBase|baseTarget|rootBoundCandidates|keepTargetFinal|targetC|rootLocalMultiInst|rootLocalInstArgMultiBase|rootLocalSchemeAliasBaseLike|boundVarTarget|schemeBodyTarget' src/MLF/Elab/Run/ResultType/Fallback.hs test/PipelineSpec.hs`
  -> pass

### Full Gate

- `cabal build all && cabal test` -> pass (`1138` examples, `0` failures)

### Final Status Snapshot

- `git diff --check` -> pass
- `git status --short --untracked-files=all` -> bounded `I2` change set plus
  the pre-existing controller files listed above
