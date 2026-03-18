# `C3` Bounded Verification And Evidence Consolidation Gate

Date: 2026-03-18
Round: `round-036`
Roadmap item: `C3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bounded verification/evidence gate

## Stage Contract Freeze

This artifact implements only roadmap item `C3` for `attempt-1` with `retry: null`.

It verifies the accepted `C2` local-binding-only `rootBindingIsLocalType`
fail-closed retention slice only. It does not reopen `C1` selection, does not reopen
`C2` implementation, and does not authorize production or test edits during this
attempt.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

Any verification failure in this stage would be a blocker to record, not permission to
patch `src/MLF/Elab/Run/ResultType/Fallback.hs`, `test/PipelineSpec.hs`, or any other
production/test file inside `round-036` `attempt-1`.

## Accepted Evidence Chain Carried Forward Without Widening

1. `C1` bound the cycle to repaired `URI-R2-C1`, preserved the inherited boundary,
   kept accepted negative findings binding, and froze exactly one future `C2` target:
   the local-binding-only result-type retention hardening keyed off
   `rootBindingIsLocalType`
   (`docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md:14-25`,
   `:43-49`, `:53-66`, `:70-83`).
2. `C2` authoritative `attempt-2` stayed fail-closed, kept the same inherited
   boundary, and recorded that the bounded production slice lives only in
   `Fallback.hs` with focused `PipelineSpec` coverage for the same non-local proxy
   wrapper case
   (`docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md:14-30`,
   `:34-55`, `:57-70`, `:81-87`).
3. `U6` remained aggregate-only, finalized `continue-bounded`, and explicitly refused
   to reinterpret accepted `U2` / `U3` / `U4` negative findings as widening
   clearance
   (`docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md:14-29`,
   `:35-41`, `:100-126`).
4. `round-035` review data remains the authoritative acceptance proof that `C2`
   finalized as `attempt=2`, `attempt_verdict=accepted`, `stage_action=finalize`,
   `status=authoritative`, with `C2-FOCUSED-BLOCK=pass` and `C2-FULL-GATE=pass`
   (`orchestrator/rounds/round-035/review-record.json:4-25`).
5. `Bugs.md` still records `BUG-2026-03-16-001` as replay-lane continuity context
   owned by `MLF.Elab.Inst.applyInstantiation`; it does not provide authority to
   reopen replay repair or `InstBot` in this round (`Bugs.md:7-24`).

`C3` therefore answers only one bounded question: whether the accepted `C2` slice
still looks stable under the focused `ARI-C1 feasibility characterization (bounded
prototype-only)` block, the fresh full repo gate, and predecessor continuity
rechecks.

## Bounded Code/Test Anchor Evidence

### `src/MLF/Elab/Run/ResultType/Fallback.hs`

- `rootBindingIsLocalType` is still computed from the canonical binding scope and is
  true only for local `TypeRef` roots (`Fallback.hs:478-481`).
- `targetPresolutionView` still uses `presolutionViewFinal` only when that
  local-binding gate holds, and falls back to the non-local `presolutionView`
  otherwise (`Fallback.hs:501-504`).
- `keepTargetFinal` still requires `rootBindingIsLocalType` before any retained-target
  heuristic may keep the target (`Fallback.hs:652-658`).
- The final target-selection branch still fails closed for non-local roots by taking
  `rootFinal` rather than `schemeBodyTarget targetPresolutionView rootC`
  (`Fallback.hs:663-674`).

### `test/PipelineSpec.hs`

- The bounded six-example `ARI-C1 feasibility characterization (bounded
  prototype-only)` block still occupies `PipelineSpec.hs:1101-1168`.
- The annotation-anchored recursive positive control remains at
  `PipelineSpec.hs:1102-1116`.
- The local-binding direct-wrapper positive control remains at
  `PipelineSpec.hs:1118-1134`.
- The unannotated non-recursive contrast remains at
  `PipelineSpec.hs:1136-1141`.
- The direct `computeResultTypeFallback` fail-closed check for the same non-local
  proxy wrapper case `let g = (\x : mu a. a -> Int. x) in g g` remains at
  `PipelineSpec.hs:1143-1151`.
- The source-guard assertion for `rootBindingIsLocalType` remains at
  `PipelineSpec.hs:1153-1156`.
- The unchecked/checked pipeline-entrypoint rejection for that same `g g` case
  remains at `PipelineSpec.hs:1158-1168`.

## Fresh Verification Results

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-036`

### Baseline Commands

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json` ->
  pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `36:1. [done] Execute the `C1` continue-bounded bind and exact next-slice target selection for the still-bound live subject`
  - `40:2. [done] Execute the `C2` bounded fail-closed local-binding-only result-type target-retention hardening slice frozen by `C1``
  - `44:3. [pending] Execute the `C3` bounded verification and evidence consolidation gate for the accepted local-binding-only fail-closed retention slice`
  - `48:4. [pending] Execute the `C4` next-cycle decision gate`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md` -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### Focused `ARI-C1` Rerun

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass:
  - `Finished in 0.0124 seconds`
  - `6 examples, 0 failures`
  - all six bounded examples passed: annotation-anchored recursion, local-binding
    direct-wrapper retention, unannotated non-recursive contrast, direct fallback
    fail-closed reconstruction for `let g = (\x : mu a. a -> Int. x) in g g`, the
    `rootBindingIsLocalType` source guard, and unchecked/checked pipeline-entrypoint
    rejection for the same `g g` case

### Full Repo Gate

- `cabal build all && cabal test` -> pass:
  - `build all` completed successfully before the test run
  - the gate built `exe:mlf2` and `exe:frozen-parity-gen` in addition to the library
    and test suite
  - `Finished in 2.1146 seconds`
  - `1127 examples, 0 failures`
  - `Test suite mlf2-test: PASS`

### Predecessor Continuity Recheck

- `python3` continuity validation over required predecessor paths and completed rounds
  `round-028` through `round-035` -> pass:
  - `C3 continuity check: required predecessor paths exist`
  - `C3 continuity check: review records round-028 through round-035 present`
  - `C3 continuity check: C2 authoritative record intact`

## Docs-Only Diff Evidence

This round remains docs-only. No production, test, public API, executable, Cabal,
roadmap, bug-tracker, or controller-state files were edited.

- `git status --short --untracked-files=all` -> docs/orchestrator-only untracked set:
  - `?? docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
  - `?? orchestrator/rounds/round-036/implementation-notes.md`
  - `?? orchestrator/rounds/round-036/plan.md`
  - `?? orchestrator/rounds/round-036/selection.md`
- `git diff --name-only` -> no tracked diffs
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'` ->
  no output

## Stability Conclusion

The accepted `C2` local-binding-only `rootBindingIsLocalType` fail-closed retention
slice remains stable under the current bounded verification gate.

The read-only code/test anchors still show the same local-binding gate and same
bounded six-example test shape, the focused `ARI-C1` block passed again, the full
repo gate passed again, and predecessor continuity remains intact. Nothing in this
round widened beyond repaired `URI-R2-C1`.

## Blockers

None. All required verification commands for `C3` `attempt-1` passed.

## Non-Authorization Statement

This artifact does not authorize:

- edits to `src/MLF/Elab/Inst.hs`;
- replay-lane reopen or `InstBot` changes;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural graph encoding;
- multi-SCC or cross-family widening;
- heuristic authority/owner selection;
- second executable interfaces;
- compatibility shims, convenience fallbacks, or default-path widening;
- roadmap, controller-state, or bug-tracker edits.
