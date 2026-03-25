# Round 036 Plan (`C3` Bounded Verification Gate)

## Objective

Execute only roadmap item `C3` and produce one accepted docs/evidence artifact at:
`docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`.

This round must verify the accepted `C2` local-binding-only `rootBindingIsLocalType`
fail-closed retention slice against all three required evidence surfaces named by the
roadmap and selection:

1. the focused `ARI-C1 feasibility characterization (bounded prototype-only)` block in
   `test/PipelineSpec.hs`;
2. a fresh full repo gate via `cabal build all && cabal test`;
3. predecessor continuity from accepted `C1`, accepted `C2`, accepted `U6`, and the
   inherited boundary/repair documents.

`attempt-1` remains docs-only. No production, test, public API, executable, Cabal,
roadmap, bug-tracker, or controller-state edits are planned or authorized in this
round. If any verification step fails, treat that as a blocker to be recorded in the
`C3` artifact and round notes; do not patch code inside `C3` `attempt-1`.

## Locked Round Context

- Round id: `round-036`
- Roadmap item: `C3`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary: `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: docs/evidence consolidation only

Accepted predecessor facts that must remain binding throughout `C3`:

- `C1` froze exactly one bounded `C2` target: the local-binding-only result-type
  target-retention hardening slice in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  with focused coverage in
  `test/PipelineSpec.hs`.
- `C2` finalized in `round-035` as authoritative `attempt-2`, with
  `attempt_verdict=accepted`, `stage_action=finalize`, and a passing focused
  `ARI-C1` block plus a passing full repo gate.
- `U6` finalized `continue-bounded`, not `widen-approved`.
- `U2` remains `authority-narrowed`.
- `U3` remains `uniqueness-owner-stable-refuted`.
- `U4` remains `constructor-acyclic-termination-refuted`.
- `BUG-2026-03-16-001` in `Bugs.md`
  remains replay-lane continuity context only; it is not authority to reopen
  `MLF.Elab.Inst`, replay repair, or any broader recursive-inference lane here.

Current repository status at the root worktree is already non-pristine
(`orchestrator/rounds/round-036/state-snapshot.json` modified, current round files untracked, task folder
untracked). Respect those existing changes. Do not revert or "clean up" unrelated
work while gathering `C3` evidence.

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-036/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-003/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-003/retry-subloop.md`
- `orchestrator/roles/planner.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-003/roadmap.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`
- `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `orchestrator/rounds/round-035/review-record.json`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `Bugs.md`

## Files Expected In Scope

Primary writable artifact:

1. `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
   - create the canonical `C3` verification/evidence record.

Optional bounded note file:

1. `orchestrator/rounds/round-036/implementation-notes.md`
   - optional command transcript / reviewer note file if needed for long outputs or blocker capture.

Read-only evidence anchors:

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `test/PipelineSpec.hs`
3. `orchestrator/rounds/round-035/review-record.json`
4. `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`
5. `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
6. `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
7. `Bugs.md`

Files that must remain untouched by `C3` `attempt-1`:

- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `orchestrator/rounds/round-036/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-003/roadmap.md`
- `Bugs.md`
- reviewer-owned history under `orchestrator/rounds/round-035/`
  and any prior round directories

## Sequential Tasks

### Task 1 - Freeze the `C3` `attempt-1` contract as docs-only verification

- In the canonical `C3` artifact, state explicitly:
  - `Round: round-036`
  - `Roadmap item: C3`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding.
- State that `C3` verifies stability of the accepted `C2` slice only.
- State that `C3` does not reopen `C1` selection, does not reopen `C2`
  implementation, and does not authorize code edits by default.
- State that any verification failure is a blocker to record, not a license to patch
  `Fallback.hs`, `PipelineSpec.hs`, or other production/test files during this attempt.

### Task 2 - Reconstruct the accepted evidence chain without widening it

- Carry forward the exact bounded chain that `C3` is allowed to characterize:
  - `C1` froze one future slice only;
  - `C2` authoritative `attempt-2` landed the local-binding-only
    `rootBindingIsLocalType` fail-closed retention hardening;
  - `U6` authorized `continue-bounded` only;
  - `U2`, `U3`, and `U4` remain accepted negative findings that still constrain the
    lane.
- Use `orchestrator/rounds/round-035/review-record.json`
  as the authoritative acceptance proof for `C2`.
- Treat `Bugs.md` only as continuity context.
  Do not reinterpret `BUG-2026-03-16-001` as current-round repair authority.
- State explicitly that `C3` answers only this bounded question:
  whether the accepted `C2` slice still looks stable under the focused `ARI-C1`
  block, the fresh full repo gate, and predecessor continuity evidence.

### Task 3 - Collect read-only evidence from the bounded code/test anchors

- Inspect the existing retained-target gate in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  without editing it.
- Capture line-referenced evidence for all of the following:
  - `rootBindingIsLocalType`;
  - the `targetPresolutionView` branch that falls back to the non-local view;
  - `keepTargetFinal`;
  - the non-local `else rootFinal` fail-closed branch.
- Inspect the focused
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in `test/PipelineSpec.hs`
  without editing it.
- Capture line-referenced evidence showing the accepted bounded six-example shape:
  - annotation-anchored recursive positive control;
  - local-binding direct-wrapper positive control;
  - unannotated non-recursive contrast;
  - direct `computeResultTypeFallback` fail-closed check for the non-local proxy
    wrapper `let g = (\x : mu a. a -> Int. x) in g g`;
  - source-guard assertion for `rootBindingIsLocalType`;
  - unchecked/checked pipeline-entrypoint rejection for that same `g g` case.
- Reconfirm from `round-035` review data that `C2` finalized as authoritative
  `attempt-2` with `C2-FULL-GATE=pass` and `C2-FOCUSED-BLOCK=pass`.

Recommended evidence commands for this step:

- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '470,715p'`
- `nl -ba test/PipelineSpec.hs | sed -n '1101,1168p'`
- `python3 -m json.tool orchestrator/rounds/round-035/review-record.json`

### Task 4 - Re-run the bounded verification suite required for `C3`

- Run the baseline checks required by
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-003/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-036/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-036/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-003/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-003/retry-subloop.md`
- Run the focused bounded test block exactly:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Re-run the full repo gate exactly:
  - `cabal build all && cabal test`

Although `C3` is docs-only, the fresh full repo gate is still mandatory for this
stage because the roadmap and selection require current verification evidence for the
already-accepted `C2` slice, not merely a restatement of `round-035`.

### Task 5 - Re-run predecessor continuity checks and docs-only diff checks

- Reconfirm predecessor continuity in a reviewer-auditable way. At minimum, verify:
  - required predecessor documents still exist;
  - completed rounds `round-028` through `round-035` still have review records /
    authoritative artifacts where expected;
  - `round-035` still names the accepted `C2` artifact as authoritative;
  - the recursive-types packet path still exists.
- One acceptable continuity command is:

```bash
python3 - <<'PY'
import json
from pathlib import Path

root = Path('.')
required_paths = [
    'docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md',
    'docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md',
    'docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md',
    'docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md',
    'docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md',
    'docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md',
    'tasks/todo/2026-03-11-recursive-types-orchestration',
]
missing = [p for p in required_paths if not (root / p).exists()]
if missing:
    raise SystemExit(f'missing required continuity paths: {missing}')

c2 = json.loads((root / 'orchestrator/rounds/round-035/review-record.json').read_text())
assert c2['stage_id'] == 'C2', c2
assert c2['attempt_verdict'] == 'accepted', c2
assert c2['stage_action'] == 'finalize', c2
assert c2['authoritative_attempt'] == 2, c2
assert c2['artifact_path'] == 'docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md', c2
print('C3 continuity check: C2 authoritative record intact')
PY
```

- Reconfirm the round diff stays docs-only:
  - `git status --short --untracked-files=all`
  - `git diff --name-only`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
- Because the repository root is already non-pristine, interpret those diff checks
  narrowly:
  - do not require a globally clean tree;
  - do require that this round does not add new tracked/untracked edits outside the
    docs/orchestrator artifact set it owns.

### Task 6 - Author the canonical `C3` verification artifact

- Write
  `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
  as the canonical stage result.
- Required sections:
  - stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
    `Retry state`, `Live subject`, `Artifact kind`);
  - stage contract freeze;
  - accepted evidence chain carried forward from `C1`, `C2`, and `U6`;
  - bounded code/test anchor evidence from `Fallback.hs` and `PipelineSpec.hs`;
  - fresh verification results for baseline checks, focused `ARI-C1`, full repo
    gate, and predecessor continuity;
  - explicit stability conclusion for the accepted `C2` slice;
  - explicit blocker section if any verification failed;
  - non-authorization statement preserving the inherited boundary.
- If all required commands pass, the artifact should say the accepted `C2` slice
  remains stable under current bounded verification.
- If any required command fails, the artifact should instead record the exact failing
  command, failure shape, and why that blocks a stability claim. Do not fix the code
  in this round.

### Task 7 - Prepare reviewer handoff for `C3`

- Support the lawful `C3` reviewer outcomes from the retry contract:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure the reviewer can verify all of the following directly from the final tree:
  - `C3` stayed docs-only;
  - the accepted `C2` slice remained the exact subject under review;
  - focused `ARI-C1` evidence was freshly rerun;
  - the full repo gate was freshly rerun;
  - predecessor continuity was freshly rechecked;
  - no broadening into replay repair, `MLF.Elab.Inst`, equi-recursive reasoning,
    cyclic graph encoding, multi-SCC search, cross-family widening, second
    interfaces, compatibility shims, or convenience/default-path widening occurred.

## Non-Goals

- No production-code or test-code edits in `C3` `attempt-1`.
- No edits to `src/MLF/Elab/Inst.hs`.
- No replay-lane reopen, including `InstBot`, as part of this verification round.
- No roadmap mutation, no `Bugs.md` updates, and no controller-state edits.
- No reinterpretation of accepted `U2` / `U3` / `U4` negative findings as if they were
  already cleared.
- No equi-recursive semantics, implicit unfolding, cyclic structural graph encoding,
  multi-SCC widening, cross-family widening, heuristic owner ranking, or hidden
  recursive-inference enablement.
- No second executable interface, compatibility fallback, convenience shim, or
  default-path widening.

## Reviewer Checks

Baseline checks from
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-003/verification.md` still apply.

Round-specific checks:

1. `plan.md` explicitly names `round-036`, `C3`, `attempt-1`, `retry: null`, and the
   repaired `URI-R2-C1` subject.
2. The canonical `C3` artifact stays docs-only and treats verification failures as
   blockers to record, not as permission to patch code in the same round.
3. The artifact carries forward accepted `C1`, accepted `C2` authoritative
   `attempt-2`, accepted `U6 = continue-bounded`, and accepted negative `U2` / `U3` /
   `U4` findings without rewriting them as clearance.
4. Evidence includes line-referenced `Fallback.hs` and `PipelineSpec.hs` anchors for
   the bounded `rootBindingIsLocalType` fail-closed slice and the six-example
   `ARI-C1` block, plus the authoritative `round-035` review record.
5. Fresh verification evidence includes:
   - the focused `ARI-C1` block command;
   - the full repo gate command;
   - predecessor continuity recheck;
   - docs-only diff evidence showing no new code/test edits for this round.
6. The artifact explicitly preserves the inherited `explicit-only / non-equi-recursive /
   non-cyclic-graph` boundary and rejects replay reopen, `MLF.Elab.Inst` edits,
   broad automatic recursive inference, and all other widening paths.
