# `C2` Bounded Fail-Closed Local-Binding-Only Implementation Slice

Date: 2026-03-18
Round: `round-035`
Roadmap item: `C2`
Stage: `implement`
Attempt: `attempt-2`
Retry state: `rejected attempt-1 -> retry`
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded result-type retention hardening slice

## Stage Contract Freeze

This artifact refreshes only roadmap item `C2` for `attempt-2` after rejected `attempt-1`
reported the bounded retry reason
`c2-non-local-proxy-entrypoint-coverage-missing`.

The live subject remains repaired `URI-R2-C1` only. The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

This round stays fail-closed. It does not reinterpret accepted `U2`, `U3`, or `U4` negative findings as clearance for broader unannotated recursive inference.

The retry delta is evidence-only:

- retarget the bounded `PipelineSpec` entrypoint check to the same non-local proxy wrapper case already used by the direct fallback control;
- refresh this canonical artifact and `implementation-notes.md`;
- do not reopen `src/MLF/Elab/Run/ResultType/Fallback.hs`, `MLF.Elab.Inst`, replay repair, or any widening lane.

## Carry-Forward Authority

Accepted predecessor facts carried forward without rewrite:

1. `C1` froze exactly one `C2` target: local-binding-only result-type target-retention hardening in `src/MLF/Elab/Run/ResultType/Fallback.hs` with focused coverage in `test/PipelineSpec.hs`.
2. `U5` added the reviewer-auditable `rootBindingIsLocalType` signal in the bounded `ResultType.Fallback` lane without enabling a new recursive success path.
3. `U6` finalized `continue-bounded`, not `widen-approved`, so this round remains one more bounded non-widening slice only.
4. `U2` remains `authority-narrowed`.
5. `U3` remains `uniqueness-owner-stable-refuted`.
6. `U4` remains `constructor-acyclic-termination-refuted`.

`Bugs.md` and open replay-path defect `BUG-2026-03-16-001` remain repaired-lane continuity context only. They do not authorize edits to `MLF.Elab.Inst` or any replay reopen in this round.

## Bounded Production Slice

Production edits remain inside `src/MLF/Elab/Run/ResultType/Fallback.hs` only.

The bounded hardening now uses `rootBindingIsLocalType` as the retained-target gate:

- retained-target behavior is only considered when the fallback root still resolves through a local `TypeRef`;
- non-local wrapper/proxy roots do not take the retained-target path and stay on the fail-closed root-target branch instead;
- candidate matching for `boundVarTargetRoot` follows the same local-vs-non-local view selection, so non-local wrapper/proxy roots do not manufacture a broader retained target.

This remains bounded to the repaired `URI-R2-C1` lane. No heuristic owner ranking, equi-recursive reasoning, implicit unfolding, cyclic graph encoding, replay repair, or convenience fallback path was introduced.

## Focused Pipeline Coverage

Focused coverage remains in `test/PipelineSpec.hs` only:

- the bounded recursive control still proves the annotation-anchored recursive shape is processable;
- the direct wrapper control still proves the local retained recursive-looking shape remains processable;
- the unannotated contrast still proves no broad automatic recursive `TMu` inference was introduced;
- the same bounded non-local proxy wrapper case
  `let g = (\x : mu a. a -> Int. x) in g g`
  is now evidenced three ways:
  - direct `computeResultTypeFallback` fail-closed reconstruction stays non-recursive;
  - unchecked `runPipelineElab` rejects that same expression;
  - checked `runPipelineElabChecked` rejects that same expression;
- the new source guard proves `Fallback.hs` now gates retained-target behavior on `rootBindingIsLocalType`.

## Files Changed

- `src/MLF/Elab/Run/ResultType/Fallback.hs` (landed in `attempt-1`, unchanged in `attempt-2`)
- `test/PipelineSpec.hs`
- `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
- `orchestrator/rounds/round-035/implementation-notes.md`

## Non-Authorization Statement

This round does not authorize:

- edits to `src/MLF/Elab/Inst.hs`;
- replay-repair reopen or `InstBot` changes;
- prototype/research entrypoints;
- public API, executable, or `mlf2.cabal` changes;
- equi-recursive reasoning, implicit unfolding, cyclic structural graph encoding, multi-SCC widening, cross-family widening, heuristic owner selection, or compatibility/default-path widening.

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-035`

### Baseline Commands

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json` -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `35:1. [done] Execute the `C1` continue-bounded bind and exact next-slice target selection for the still-bound live subject`
  - `39:2. [pending] Execute the `C2` bounded fail-closed local-binding-only result-type target-retention hardening slice frozen by `C1``
  - `43:3. [pending] Execute the `C3` bounded verification and evidence consolidation gate for the frozen local-binding-only retention slice`
  - `47:4. [pending] Execute the `C4` next-cycle decision gate`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md` -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### Focused `C2` Checks

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'` -> pass:
  - `6 examples, 0 failures`
  - covers annotation-anchored recursion, the direct wrapper positive control, the non-recursive unannotated contrast, the direct fail-closed reconstruction check for `let g = (\x : mu a. a -> Int. x) in g g`, the `rootBindingIsLocalType` source guard, and the same-case unchecked/checked pipeline-entrypoint rejection

### Full Repo Gate

- `cabal build all && cabal test` -> pass:
  - incremental `build all` completed successfully before the test run
  - `Finished in 2.3541 seconds`
  - `1127 examples, 0 failures`
  - `Test suite mlf2-test: PASS`
