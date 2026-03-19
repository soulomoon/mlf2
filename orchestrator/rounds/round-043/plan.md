# Round 043 Plan (`F2` Bounded Scheme-Alias/Base-Like Implementation Slice)

## Objective

Execute only roadmap item `F2` and produce one accepted implementation artifact at:
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`.

This is the initial `F2` plan for `attempt-1` with `retry: null`. The round must
land exactly one bounded fail-closed implementation slice under repaired
`URI-R2-C1`:

- in `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`,
  only the adjacent local-binding
  `rootIsSchemeAlias && rootBoundIsBaseLike` branch inside `keepTargetFinal`
  plus the downstream `targetC` selection in the existing
  `Fallback.hs:521-686` window; and
- in `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`, only one
  bounded extension of the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block.

Carry the accepted `E2` / `E3` same-lane retained-child baseline forward as
inherited context only. Treat `boundVarTarget` as absent for this selected
slice. Keep `rootHasMultiInst`, `instArgRootMultiBase`, replay reopen,
`MLF.Elab.Inst`, `InstBot`, non-local binding, equi-recursive reasoning, cyclic
structural encoding, multi-SCC widening, cross-family widening, broader search,
second-interface work, and fallback/convenience widening out of scope.

## Locked Round Context

- Round id: `round-043`
- Roadmap item: `F2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no second interface / no fallback widening`
- Stage mode: bounded implementation slice; production and test edits are
  expected, so the mandatory full repo gate applies.

Accepted carry-forward facts that must remain unchanged:

- `E2` authoritative `attempt-2` landed only the bounded same-lane retained-child
  local-`TypeRef` `boundVarTarget` / nested-`forall` fail-closed slice in
  `ResultType.Fallback` plus focused `PipelineSpec` coverage.
- `E3` authoritative `attempt-1` reverified that exact `E2` slice under the
  bounded `ARI-C1` block and a passing `cabal build all && cabal test` gate.
- `E4` authoritative `attempt-1` finalized `continue-bounded`, so the next
  lawful action is one more bounded non-widening implementation slice rather
  than replay reopen or broader widening.
- `F1` authoritative `round-042` review froze exactly one `F2` target: the
  local-binding `rootIsSchemeAlias && rootBoundIsBaseLike` `keepTargetFinal` /
  `targetC` lane in `Fallback.hs`, with future ownership limited to
  `Fallback.hs` and `PipelineSpec.hs`.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` remains continuity context
  only. Open replay bug `BUG-2026-03-16-001` does not authorize replay reopen,
  `MLF.Elab.Inst`, `MLF.Elab.Inst.applyInstantiation`, or `InstBot` work in
  this round.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-043/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/planner.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-042/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`

## Files Expected In Scope

Primary writable implementation artifact:

1. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`

Bounded production and test files:

1. `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`

Optional bounded note file:

1. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-043/implementation-notes.md`

Files that must remain untouched by `F2` `attempt-1`:

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-043/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/src-public/`
- `/Users/ares/.codex/worktrees/d432/mlf4/app/`
- `/Users/ares/.codex/worktrees/d432/mlf4/mlf2.cabal`
- prior round artifacts under
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-001/`
  through `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-042/`

If the implementer cannot keep the change confined to the artifact above plus
`Fallback.hs`, `PipelineSpec.hs`, and the optional round notes file, stop and
hand the issue back to review instead of widening scope.

## Sequential Tasks

### Task 1 - Freeze the `F2` attempt-1 contract

- In the canonical `F2` artifact, state explicitly:
  - `Round: round-043`
  - `Roadmap item: F2`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the fixed inherited boundary:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding;
  - no second executable interface;
  - no compatibility / convenience / default-path widening.
- State that `F2` implements exactly one bounded fail-closed lane and does not
  authorize replay reopen, broader search, or roadmap/history edits.

### Task 2 - Isolate only the selected local-binding scheme-alias/base-like lane in `Fallback.hs`

- Start from the accepted `E2` / `E3` baseline unchanged. The retained-child
  same-lane `boundVarTarget` / `boundHasForallFrom` behavior remains inherited
  context, not the selected `F2` target family.
- Keep all edits inside the existing local block that computes
  `rootIsSchemeAlias`, `rootBoundIsBaseLike`, `keepTargetFinal`, and `targetC`
  in `Fallback.hs:521-686`.
- Make the selected lane reviewer-auditable by centering it on the already
  computed local-binding proof:
  `rootBindingIsLocalType && rootIsSchemeAlias && rootBoundIsBaseLike`.
- Treat `boundVarTarget` as absent for this selected slice:
  - do not add new retained-child search;
  - do not use `boundVarTarget` to justify the selected
    scheme-alias/base-like admission path; and
  - do not reopen nested-`forall` / retained-child logic as active target work.
- Keep `rootHasMultiInst` and `instArgRootMultiBase` behavior unchanged and out
  of scope. Do not widen either trigger family or turn them into active `F2`
  work.
- Keep the selected lane fail-closed whenever it would require replay reopen,
  `MLF.Elab.Inst`, `InstBot`, non-local binding, equi-recursive reasoning,
  cyclic structural encoding, or any broader recursive-inference search.

### Task 3 - Land bounded behavioral coverage only in the existing `ARI-C1` block

- Extend only the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block in
  `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`.
- Add exactly one new behavioral positive example for the selected `F2` lane:
  - it must exercise the local-binding scheme-alias/base-like
    `keepTargetFinal` / `targetC` path directly;
  - it must stay inside repaired `URI-R2-C1`;
  - it must not reopen retained-child widening or broader search.
- Add exactly one matched fail-closed contrast in the same block:
  - use a same-looking adjacent wrapper that lacks the admissible local
    scheme-alias/base-like proof, or leaves the selected local-binding lane;
  - confirm the fallback or pipeline result stays non-recursive / fail-closed.
- Keep the inherited `E2` / `E3` retained-child examples in place as historical
  baseline only. Do not rewrite them as if they were the new `F2` evidence.
- Do not add a new spec module, new executable entrypoint, or broader
  repository-wide search harness.

### Task 4 - Author the canonical `F2` implementation artifact and round notes

- Write the canonical implementation artifact at:
  `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`.
- The artifact must record:
  - the fixed `F2` stage metadata from Task 1;
  - the accepted `E2` / `E3` / `E4` / `F1` carry-forward chain;
  - the exact implemented code slice in `Fallback.hs`;
  - the exact bounded `PipelineSpec` additions;
  - the unchanged out-of-scope exclusions;
  - focused verification evidence and the mandatory full repo gate result.
- Update
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-043/implementation-notes.md`
  only if needed to give reviewer-facing command output and scope notes. Do not
  rewrite prior attempt artifacts or roadmap history.

### Task 5 - Run and record the required verification

- Run the baseline checks from
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`:
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
- Run the focused bounded checks for this round:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - `rg -n 'rootIsSchemeAlias|rootBoundIsBaseLike|keepTargetFinal|targetC|boundVarTarget' src/MLF/Elab/Run/ResultType/Fallback.hs test/PipelineSpec.hs`
  - `git diff --name-only`
- Run the mandatory full repo gate:
  - `cabal build all && cabal test`
- Record reviewer continuity evidence that predecessor rounds, accepted boundary
  docs, and accepted `E2` / `E3` / `E4` / `F1` history remain intact.

### Task 6 - Prepare reviewer handoff with bounded-scope evidence

- Ensure the reviewer can confirm that only these files changed:
  - `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`
  - `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`
  - optionally
    `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-043/implementation-notes.md`
- Ensure reviewer evidence names the new positive `F2` example and the matched
  fail-closed contrast explicitly, and ties them to the selected
  local-binding scheme-alias/base-like lane rather than to retained-child or
  wider recursive inference.
- Ensure reviewer evidence states explicitly that `boundVarTarget` was treated
  as absent for this selected slice and that
  `rootHasMultiInst` / `instArgRootMultiBase` remained out of scope.
- Ensure reviewer can still lawfully emit one of the allowed `F2` outcomes:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`

## Non-Goals

- No redesign of the accepted `E2` / `E3` retained-child lane.
- No reopening of replay repair, `MLF.Elab.Inst`, `MLF.Elab.Inst.applyInstantiation`,
  or `InstBot`.
- No non-local binding work.
- No equi-recursive reasoning or implicit unfolding.
- No cyclic structural graph encoding.
- No multi-SCC widening, cross-family widening, or broader search.
- No second executable interface.
- No compatibility fallback, convenience fallback, or default-on widening path.
- No edits to `orchestrator/state.json`, `orchestrator/roadmap.md`, `Bugs.md`,
  `src-public/`, `app/`, `mlf2.cabal`, or prior-round attempt artifacts.
- No rewrite of roadmap history or prior review history.

## Reviewer Checks

Baseline checks from
`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md` still
apply.

Round-specific checks:

1. `plan.md` frames this as the initial `F2` plan for `attempt-1` with
   `retry: null`, not as a retry delta plan.
2. The canonical implementation artifact path is exactly
   `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`.
3. The selected implementation slice is confined to the adjacent local-binding
   `rootIsSchemeAlias && rootBoundIsBaseLike` `keepTargetFinal` / `targetC`
   lane in `Fallback.hs`, with bounded coverage only in `PipelineSpec.hs`.
4. Accepted `E2` / `E3` same-lane retained-child evidence is carried forward as
   inherited context only, and `boundVarTarget` is treated as absent for the
   selected `F2` slice.
5. `rootHasMultiInst`, `instArgRootMultiBase`, replay reopen, `MLF.Elab.Inst`,
   `InstBot`, non-local binding, equi-recursive reasoning, cyclic structural
   encoding, multi-SCC widening, cross-family widening, broader search,
   second-interface work, and fallback widening remain explicitly out of scope.
6. Verification evidence includes the focused
   `ARI-C1 feasibility characterization (bounded prototype-only)` rerun, the
   mandatory `cabal build all && cabal test` gate, diff-scope confirmation, and
   predecessor continuity review.
