# Round 032 Plan (`U5` Bounded Solver/Pipeline Implementation Slice)

## Objective

Execute only roadmap item `U5` and produce one bounded implementation artifact at:
`docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`.

This round must land exactly one bounded, fail-closed solver/pipeline slice for repaired `URI-R2-C1` under the accepted `U4` result `constructor-acyclic-termination-refuted`. The slice may harden or stabilize the bounded pipeline path, but it may not reinterpret `U4` as clearance for positive unannotated recursive admission.

## Locked Round Context

- Round id: `round-032`
- Stage: `plan` for roadmap item `U5`
- Active attempt: `attempt-1` (fresh attempt; `retry: null`)
- Active subject (fixed): repaired `URI-R2-C1`
- Mandatory inherited boundary (fixed): `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: one bounded solver/pipeline implementation slice plus focused tests and one implementation artifact

Inherited carry-forward facts that must remain unchanged:

- `U1` finalized in `round-028` and bound the successor lane to repaired `URI-R2-C1`.
- `U2` finalized in `round-029` with bounded result token `authority-narrowed`.
- `U3` finalized in `round-030` with bounded result token `uniqueness-owner-stable-refuted`.
- `U4` finalized in `round-031` with bounded result token `constructor-acyclic-termination-refuted`.
- Because `U4` is refuted, `U5` is not a positive unannotated-recursion enablement round. It must remain fail-closed and bounded to repaired `URI-R2-C1` only.
- No widening is authorized to multi-SCC search, cross-family search, broad automatic recursive inference, equi-recursive reasoning, implicit unfolding, cyclic structural encoding, second interfaces, compatibility shims, convenience fallbacks, or default-on widening.

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-032/selection.md`
- `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-010/verification.md`
- `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-010/retry-subloop.md`
- `docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`
- `docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`
- `docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md`
- `docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md`
- `orchestrator/rounds/round-028/review-record.json`
- `orchestrator/rounds/round-029/review-record.json`
- `orchestrator/rounds/round-030/review-record.json`
- `orchestrator/rounds/round-031/review-record.json`
- `Bugs.md` (continuity-only reference; do not reopen prior repair-track history in this round)

## Files Expected In Scope

Primary writable artifact:

1. `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md`

Bounded production/test slice:

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `test/PipelineSpec.hs`

Optional bounded note file:

1. `orchestrator/rounds/round-032/implementation-notes.md` (only if needed for reviewer clarity)

Files that must remain untouched in this round:

- `orchestrator/rounds/round-032/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-010/roadmap.md`
- `orchestrator/rounds/round-032/selection.md`
- `Bugs.md`
- `src/MLF/Elab/Inst.hs` and repair-track research modules under `src/MLF/Research/`
- public API and executable surfaces under `src-public/`, `app/`, and `mlf2.cabal`
- prior round artifacts under `orchestrator/rounds/round-001/` through `orchestrator/rounds/round-031/`

If the bounded `U5` change cannot be completed inside the single result-type/pipeline slice above, stop and hand the issue back to review rather than widening the production scope.

## Sequential Tasks

### Task 1 - Freeze `U5` attempt-1 contract and inherited fail-closed boundary

- In the `U5` artifact, state explicitly: `attempt-1`, `retry: null`, and `U5` scope only.
- Reassert fixed live subject as repaired `URI-R2-C1` with no subject widening.
- Reassert inherited boundary unchanged: explicit-only baseline, non-equi-recursive semantics, non-cyclic structural graph encoding.
- Reassert that accepted `U4` result `constructor-acyclic-termination-refuted` remains live and is not converted into positive admission authority by this round.

### Task 2 - Land exactly one bounded production slice in the non-annotated result-type lane

- Keep production edits inside `src/MLF/Elab/Run/ResultType/Fallback.hs` only.
- Tighten the non-`AAnn` result-type path so repaired `URI-R2-C1` stays fail-closed unless direct structural authority is already present from the existing bounded pipeline state.
- The purpose of this slice is to harden bounded behavior, not to add a new convenience fallback. The module name `Fallback` does not authorize fallback widening in the roadmap sense.
- Do not introduce recursive success paths that depend on implicit unfolding, equi-recursive equality, cyclic graph encoding, heuristic owner selection, cross-subject search, or replay-lane repair behavior from `src/MLF/Elab/Inst.hs`.

### Task 3 - Add focused positive and negative pipeline regressions

- In `test/PipelineSpec.hs`, keep the bounded positive control that annotation-anchored recursive shape remains processable.
- Add or tighten the corresponding negative control showing the unannotated variant still does not infer a recursive `TMu` shape.
- Add or tighten the out-of-scope negative control showing an unannotated recursive proxy still rejects/fails closed without equi-recursive reasoning, cyclic encoding, or heuristic fallback behavior.
- Keep the examples bounded and local; do not add broad multi-SCC, cross-family, or generalized search cases in this round.

### Task 4 - Author the canonical `U5` implementation artifact

- Write `docs/plans/2026-03-17-uri-r2-c1-u5-bounded-unannotated-implementation-slice.md` with:
  - stage metadata (`Round`, `Roadmap item`, `Attempt`, `Retry state`, `Live subject`);
  - explicit carry-forward of `U1` through `U4`, including the accepted `U4` refuted result;
  - the exact bounded production slice chosen, with exact files changed;
  - focused positive and negative examples proving the slice stays bounded;
  - an explicit statement that this round does not authorize broader unannotated recursive inference and does not pre-decide `U6`.

### Task 5 - Run and record bounded verification for `U5`

- Run applicable baseline checks from `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-010/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-032/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-032/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-010/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-010/retry-subloop.md`
- Run targeted `U5` checks for the focused pipeline examples before the full gate if helpful, but do not treat targeted checks as sufficient evidence.
- Because this round touches production/test files, the full repo gate is mandatory:
  - `cabal build all && cabal test`
- Record verification evidence showing:
  - the diff stays inside the single bounded production slice plus focused tests and artifact;
  - repaired `URI-R2-C1` remains the only live subject;
  - no second interface, compatibility shim, convenience fallback, or default-path widening was introduced.

### Task 6 - Prepare reviewer handoff with retry-contract completeness

- Ensure reviewer can emit one legal `U5` retry-contract combination:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer record fields are fully supported by evidence:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve immutability rule: if later `U5` retries occur, attempt-1 artifacts remain unchanged and later attempts are additive.

## Non-Goals

- No widening beyond repaired `URI-R2-C1`.
- No positive broad automatic recursive inference enablement.
- No equi-recursive semantics, implicit unfolding success path, cyclic structural graph encoding, or weakened termination discipline.
- No reopening of the prior `InstBot` repair owner lane in `src/MLF/Elab/Inst.hs`.
- No solver-core, presolution, unifier, public API, executable, or research-entrypoint changes outside the single bounded result-type/pipeline slice.
- No second executable interface, compatibility fallback, convenience shim, or default-on widening path.
- No `U6` aggregate decision work beyond a bounded carry-forward note.
- No edits to `orchestrator/rounds/round-032/state-snapshot.json`, `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-010/roadmap.md`, `Bugs.md`, or prior-round history.

## Reviewer Checks

Baseline checks from `orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-010/verification.md` still apply.

Round-specific checks:

1. `plan.md` and the produced `U5` artifact explicitly name `attempt-1` with `retry: null`.
2. The artifact preserves repaired `URI-R2-C1` as the only live subject and explicitly carries forward `U4` result `constructor-acyclic-termination-refuted` as a boundary, not a clearance.
3. Production edits stay bounded to `src/MLF/Elab/Run/ResultType/Fallback.hs`; test edits stay bounded to `test/PipelineSpec.hs`; no public API or second-interface surface changes appear.
4. Focused positive coverage still shows the annotation-anchored recursive control remains processable.
5. Focused negative coverage still shows the corresponding unannotated case does not infer recursive shape and the out-of-scope recursive proxy rejects/fails closed.
6. The round introduces no equi-recursive reasoning, implicit unfolding path, cyclic graph encoding, fallback/default widening, compatibility shim, or cross-subject search behavior.
7. Verification evidence includes the mandatory full gate `cabal build all && cabal test`, recorded as passing.
8. The round remains `U5`-only and does not preempt `U6` or rewrite prior accepted evidence.
