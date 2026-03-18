# Round 034 Plan (`C1` Continue-Bounded Target Bind)

## Objective

Execute only roadmap item `C1` and produce one accepted bind/selection artifact at:
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`.

This round must bind the follow-on cycle to repaired `URI-R2-C1` under the accepted `U6` result `continue-bounded`, preserve the inherited `explicit-only / non-equi-recursive / non-cyclic-graph` boundary, and freeze exactly one bounded next-slice target for `C2`.

The selected `C2` target is:

- promote the already-landed `U5` reviewer-auditable `rootBindingIsLocalType` signal into one fail-closed local-binding-only target-retention hardening slice in `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`, with focused bounded coverage in `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`;
- keep recursive-looking wrapper/proxy shapes fail-closed unless the retained target is still rooted in a local `TypeRef` for repaired `URI-R2-C1`;
- do this without reinterpreting accepted `U2` / `U3` / `U4` negative findings as if authority, uniqueness, or constructor-directed feasibility were already cleared.

## Locked Round Context

- Round id: `round-034`
- Stage: `plan` for roadmap item `C1`
- Active attempt: `attempt-1` (fresh attempt; `retry: null`)
- Active subject (fixed): repaired `URI-R2-C1`
- Mandatory inherited boundary (fixed): `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: docs-only bind/selection round; no production/test edits in `C1`

Inherited carry-forward facts that must remain unchanged:

- `U5` finalized one bounded non-widening `ResultType.Fallback` hardening slice and exposed `rootBindingIsLocalType` only as reviewer-auditable trace evidence, not as a new recursive success path.
- `U6` finalized result token `continue-bounded`, which authorizes one more bounded non-widening cycle only.
- `U2` finalized `authority-narrowed`; provenance-stable unannotated authority is still not cleared.
- `U3` finalized `uniqueness-owner-stable-refuted`; exactly one admissible local owner/root cluster is still not established.
- `U4` finalized `constructor-acyclic-termination-refuted`; constructor-directed admissibility is still fail-closed under the inherited boundary.
- `BUG-2026-03-16-001` in `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` remains continuity context only for repaired-lane history; it is not authority in this round to reopen broad repair work or widen scope.

Therefore `C1` may bind only one bounded next slice. It may not treat accepted negative findings as if they were already cleared, and it may not reopen `MLF.Elab.Inst.applyInstantiation` as active work merely because the repaired lane exists.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-034/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` (continuity-only reference)

## Files Expected In Scope

Primary writable file:

1. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`

Optional bounded note file:

1. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-034/implementation-notes.md` (only if needed for reviewer clarity)

Read-only target-reference files for freezing the `C2` slice:

1. `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`

Files that must remain untouched in this round:

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-034/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`
- production and test surfaces under `/Users/ares/.codex/worktrees/d432/mlf4/src/`, `/Users/ares/.codex/worktrees/d432/mlf4/src-public/`, `/Users/ares/.codex/worktrees/d432/mlf4/app/`, `/Users/ares/.codex/worktrees/d432/mlf4/test/`, and `/Users/ares/.codex/worktrees/d432/mlf4/mlf2.cabal`
- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs` and any historical repair-track / research artifacts
- prior round artifacts under `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-001/` through `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-033/`

If the bind/selection artifact cannot keep the next target confined to the single `ResultType.Fallback` / `PipelineSpec` family above, stop and hand the issue back to review rather than choosing a broader `C2` target.

## Sequential Tasks

### Task 1 - Freeze `C1` attempt-1 contract and inherited continue-bounded boundary

- In the `C1` artifact, state explicitly that this is `attempt-1` with `retry: null`.
- Reassert fixed live subject: repaired `URI-R2-C1`.
- Reassert fixed inherited boundary: explicit-only semantics, non-equi-recursive reasoning, non-cyclic structural graph encoding.
- Reassert that `C1` is a bind/selection stage only and does not itself authorize production edits, solver widening, or roadmap reinterpretation.

### Task 2 - Restate the controlling accepted evidence chain without reopening it

- Summarize only the bounded predecessor facts needed for `C1`:
  - `U5` landed a bounded `ResultType.Fallback` trace-only hardening slice and added `rootBindingIsLocalType` as reviewer-auditable evidence.
  - `U6` recorded `continue-bounded`, not `widen-approved`.
  - `U2`, `U3`, and `U4` remain accepted negative findings that still constrain later work.
- State explicitly that these accepted negative findings remain binding and are not converted into implementation clearance by `C1`.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` only as repaired-lane continuity context; do not reopen `BUG-2026-03-16-001` as the selected next slice.

### Task 3 - Freeze exactly one bounded `C2` target and reject all broader alternatives

- In the `C1` artifact, record exactly one next-slice target:
  - use the already-landed `rootBindingIsLocalType` signal to harden the current result-type target-retention path so it stays fail-closed unless the retained recursive-looking root still resolves to a local `TypeRef` on repaired `URI-R2-C1`;
  - keep wrapper/proxy-shaped retention fail-closed rather than treating it as evidence that authority, uniqueness, or constructor feasibility has been cleared.
- Freeze the future `C2` ownership surface to:
  - `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`
- State explicitly that the selected target does **not** authorize:
  - edits to `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`;
  - reopening replay-repair rounds or prototype/research entrypoints;
  - equi-recursive reasoning, implicit unfolding, cyclic graph encoding, multi-SCC or cross-family widening, heuristic ownership ranking, second interfaces, compatibility shims, or convenience/default-path widening.

### Task 4 - Author the canonical `C1` bind/selection artifact

- Write `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md` with reviewer-auditable sections that include:
  - stage metadata (`Round`, `Roadmap item`, `Attempt`, `Retry state`, `Live subject`);
  - explicit `continue-bounded` carry-forward from `U6`;
  - explicit carry-forward of `U2` / `U3` / `U4` result tokens as still-binding negative evidence;
  - the exact selected `C2` target named above, with exact future file ownership;
  - a non-authorization section stating that `C1` does not clear authority, uniqueness, owner stability, constructor admissibility, replay-lane reopen, or broad automatic recursive inference.
- Make the artifact concrete enough that `C2` cannot later widen its target family without first changing the roadmap in a separate accepted round.

### Task 5 - Run and record docs-only verification for `C1`

- Run baseline checks from `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md` applicable to this docs-only slice:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/retry-subloop.md`
- Add `C1`-specific checks proving:
  - the artifact records exactly one selected next target;
  - the selected target is the bounded `ResultType.Fallback` / `PipelineSpec` family only;
  - accepted `U2` / `U3` / `U4` result tokens are carried forward as negative constraints, not rewritten as clearance.
- Full Cabal gate handling:
  - do not run `cabal build all && cabal test` unless this round unexpectedly edits code-bearing surfaces;
  - if docs-only scope holds, record the exact skip note in round artifacts.

### Task 6 - Prepare reviewer handoff with retry-contract completeness

- Ensure reviewer can lawfully emit one of the allowed non-terminal `C1` combinations:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer record fields are supported by evidence:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve immutability rule: if `C1` later retries, this `attempt-1` bind/selection artifact remains unchanged and later attempts are additive.

## Non-Goals

- No widening beyond repaired `URI-R2-C1`.
- No reinterpretation of accepted `U2` / `U3` / `U4` negative findings as if they were already cleared.
- No reopening of `BUG-2026-03-16-001` or the bounded `applyInstantiation` / `InstBot` repair lane as the selected `C2` target.
- No production-code, test, public API, executable, `mlf2.cabal`, roadmap, or bug-tracker edits in `C1`.
- No equi-recursive semantics, implicit unfolding, cyclic structural graph encoding, multi-SCC widening, cross-family widening, heuristic owner selection, or weakened termination discipline.
- No second executable interface, compatibility fallback, convenience shim, or default-on widening path.
- No preemption of `C2`, `C3`, or `C4` beyond freezing the one selected `C2` target.

## Reviewer Checks

Baseline checks from `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md` still apply.

Round-specific checks:

1. `plan.md` and the produced `C1` artifact explicitly name `attempt-1` with `retry: null`.
2. The artifact binds the new cycle to repaired `URI-R2-C1` under accepted `U6 = continue-bounded` and preserves the inherited `explicit-only / non-equi-recursive / non-cyclic-graph` boundary.
3. The artifact carries forward `U2 = authority-narrowed`, `U3 = uniqueness-owner-stable-refuted`, and `U4 = constructor-acyclic-termination-refuted` as still-binding negative evidence and does not restate them as clearance.
4. The artifact selects exactly one next bounded target: local-binding-only result-type target-retention hardening using the existing `rootBindingIsLocalType` signal, with future ownership frozen to `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs` plus `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`.
5. The artifact explicitly excludes `MLF.Elab.Inst.applyInstantiation`, replay-repair reopen, prototype/research entrypoints, broad solver widening, equi-recursive reasoning, cyclic encoding, second interfaces, fallback/default widening, and bug-tracker edits from this cycle bind.
6. Verification evidence includes the required docs-only baseline checks and an explicit full-gate skip justification because the round stayed outside code-bearing surfaces.
