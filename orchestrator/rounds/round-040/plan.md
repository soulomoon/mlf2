# Round 040 Plan (`E3` Bounded Verification Gate)

## Objective

Execute only roadmap item `E3` and produce one accepted docs/evidence artifact at:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`.

This round must verify the accepted `E2` same-lane retained-child local-`TypeRef`
slice against all three required evidence surfaces named by the roadmap and
selection:

1. the focused `ARI-C1 feasibility characterization (bounded prototype-only)` block
   in
   `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/test/PipelineSpec.hs`,
   including the retained-child same-lane success example, the matched nested-`forall`
   fail-closed contrast, and the inherited bounded controls that remain in scope;
2. a fresh full repo gate via `cabal build all && cabal test`;
3. predecessor continuity from accepted `C1`, `C2`, `C3`, `C4`, `E1`, `E2`, the
   inherited boundary/repair documents, the replay-repair evidence, and the completed
   recursive-types packet.

`attempt-1` remains docs-only. No production, test, public API, executable, Cabal,
roadmap, bug-tracker, or controller-state edits are planned or authorized in this
round. If any verification step fails, treat that as a blocker to be recorded in the
`E3` artifact and round notes; do not patch code inside `E3` `attempt-1`. If the
blocker proves the accepted slice is no longer stable, stop with blocker evidence
rather than widening scope or silently editing `Fallback.hs` / `PipelineSpec.hs`.

## Locked Round Context

- Round id: `round-040`
- Roadmap item: `E3`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary: `explicit-only / non-equi-recursive / non-cyclic-graph`
- Active branch/worktree: `codex/round-040-e3-verification-gate` at
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040`
- Stage mode: docs-only verification/evidence consolidation only

Accepted predecessor facts that must remain binding throughout `E3`:

- `C3` remains the verification model for this cycle: it proved the accepted `C2`
  local-binding-only `rootBindingIsLocalType` slice stayed stable under a focused
  bounded block rerun, a fresh full gate, and predecessor continuity.
- `C4` finalized `continue-bounded`, not `widen-approved` and not `stop-blocked`, so
  the next lawful step is another bounded non-widening verification/evidence gate.
- `E1` froze exactly one bounded `E2` target in
  `src/MLF/Elab/Run/ResultType/Fallback.hs:530-674`: the retained-child
  `boundVarTarget` / nested-`forall` fail-closed lane, with future ownership limited
  to `Fallback.hs` and `PipelineSpec.hs`.
- `E2` finalized in `round-039` as authoritative `attempt-2`, with the same-lane
  retained-child implementation accepted and the focused `ARI-C1` block expanded to a
  nine-example bounded shape that now includes:
  - the retained-child same-lane behavioral success example;
  - the retained-child same-lane source guard;
  - the matched nested-`forall` / nested-owner fail-closed contrast.
- `U2` remains `authority-narrowed`.
- `U3` remains `uniqueness-owner-stable-refuted`.
- `U4` remains `constructor-acyclic-termination-refuted`.
- `Bugs.md` remains replay-lane continuity context only. No entry authorizes replay
  reopen, `MLF.Elab.Inst`, `InstBot`, or broader recursive-inference work in this
  round.

Current round worktree state is already non-pristine because
`orchestrator/rounds/round-040/selection.md` is untracked. Respect that existing
change. Do not revert or "clean up" unrelated work while gathering `E3` evidence.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-040/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/roles/planner.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-036/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-037/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-038/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-039/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/src/MLF/Elab/Run/ResultType/Fallback.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/test/PipelineSpec.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/Bugs.md`

## Files Expected In Scope

Primary writable artifact:

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`
   - create the canonical `E3` verification/evidence record.

Optional bounded note file:

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-040/implementation-notes.md`
   - optional command transcript / blocker capture file if long outputs need a
     bounded home.

Read-only evidence anchors:

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/test/PipelineSpec.hs`
3. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`
4. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`
5. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`
6. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-037/review-record.json`
7. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-038/review-record.json`
8. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-039/review-record.json`
9. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/Bugs.md`

Files that must remain untouched by `E3` `attempt-1`:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/src/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/src-public/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/app/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/test/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/mlf2.cabal`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/Bugs.md`
- reviewer-owned history under
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-001/`
  through
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-039/`

No edit to `Fallback.hs` or `PipelineSpec.hs` is authorized during `E3`
`attempt-1`. If the accepted slice fails reverification, capture the blocker with
evidence and stop; do not repair the slice inside this docs-only gate.

## Sequential Tasks

### Task 1 - Freeze the `E3` `attempt-1` contract as a docs-only verification gate

- In the canonical `E3` artifact, state explicitly:
  - `Round: round-040`
  - `Roadmap item: E3`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding.
- State that `E3` verifies only the accepted `E2` same-lane retained-child
  local-`TypeRef` slice.
- State that `E3` does not reopen `E1` selection, does not reopen `E2`
  implementation, does not preempt `E4`, and does not authorize code edits by
  default.
- State that any verification failure is a blocker to record, not a license to patch
  `Fallback.hs`, `PipelineSpec.hs`, or other production/test files during this
  attempt.

### Task 2 - Reconstruct the accepted evidence chain without widening it

- Carry forward the exact bounded chain that `E3` is allowed to characterize:
  - `C3` supplies the verification model and proves how bounded reverification must
    be recorded;
  - `C4` authorizes only `continue-bounded`;
  - `E1` authoritative `attempt-1` froze exactly one future `E2` target in
    `Fallback.hs:530-674`;
  - `E2` authoritative `attempt-2` landed only that bounded same-lane retained-child
    slice and kept other `keepTargetFinal` trigger families unchanged.
- Use
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-039/review-record.json`
  as the authoritative acceptance proof that `E2` finalized as `attempt=2`,
  `attempt_verdict=accepted`, `stage_action=finalize`, `status=authoritative`, with
  `E2-SAME-LANE-EVIDENCE=pass`, `E2-NEGATIVE-CONTRAST=pass`,
  `E2-FOCUSED-BLOCK=pass`, `E2-FULL-GATE=pass`, and `E2-CONTINUITY=pass`.
- Use
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-038/review-record.json`
  as the authoritative proof that `E1` froze the exact `boundVarTarget` /
  nested-`forall` target and ownership boundary.
- Use
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/rounds/round-037/review-record.json`
  as the authoritative proof that the controlling decision token remains
  `continue-bounded`.
- Treat
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/Bugs.md`
  only as continuity context. Do not reinterpret replay-path bug history as current
  repair authority.
- State explicitly that `E3` answers only this bounded question:
  whether the accepted `E2` same-lane retained-child slice still looks stable under
  the focused `ARI-C1` block, the fresh full repo gate, and predecessor continuity
  evidence.

### Task 3 - Collect read-only evidence from the bounded code/test anchors

- Inspect the existing retained-target gate in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/src/MLF/Elab/Run/ResultType/Fallback.hs`
  without editing it.
- Capture line-referenced evidence for all of the following:
  - `boundHasForallFrom`;
  - `sameLocalTypeLane`;
  - the `pickCandidate` branch that uses `sameLocalTypeLane child` only when
    `rootBindingIsLocalType`;
  - `keepTargetFinal`, including the unchanged out-of-scope trigger families
    `rootHasMultiInst`, `instArgRootMultiBase`, and
    `rootIsSchemeAlias && rootBoundIsBaseLike`;
  - the final `targetC` selection branch that still falls back fail-closed when the
    same-lane retained-child condition is not met.
- Inspect the focused
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/test/PipelineSpec.hs`
  without editing it.
- Capture line-referenced evidence showing the accepted bounded nine-example shape:
  - annotation-anchored recursive positive control;
  - local-binding direct-wrapper positive control;
  - retained-child same-lane behavioral success example;
  - retained-child same-lane source guard;
  - retained-child nested-`forall` fail-closed contrast;
  - unannotated non-recursive contrast;
  - non-local proxy fallback fail-closed reconstruction;
  - local-binding gate source guard;
  - unchecked/checked pipeline-entrypoint rejection for the same non-local proxy
    wrapper.
- Reconfirm from `round-037`, `round-038`, and `round-039` review data that `C4`,
  `E1`, and `E2` finalized as authoritative accepted attempts with the expected
  artifact paths and no retry still pending.

Recommended evidence commands for this step:

- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '530,686p'`
- `nl -ba test/PipelineSpec.hs | sed -n '1118,1268p'`
- `python3 -m json.tool orchestrator/rounds/round-037/review-record.json`
- `python3 -m json.tool orchestrator/rounds/round-038/review-record.json`
- `python3 -m json.tool orchestrator/rounds/round-039/review-record.json`

### Task 4 - Re-run the bounded verification suite required for `E3`

- Run the baseline checks required by
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/verification.md`:
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
- Run the focused bounded test block exactly:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Re-run the full repo gate exactly:
  - `cabal build all && cabal test`

Although `E3` is docs-only, the fresh full repo gate is still mandatory for this
stage because the roadmap and selection require current verification evidence for the
already-accepted `E2` slice, not merely a restatement of `round-039`.

### Task 5 - Re-run predecessor continuity checks and docs-only diff checks

- Reconfirm predecessor continuity in a reviewer-auditable way. At minimum, verify:
  - required predecessor documents still exist;
  - completed rounds `round-001` through `round-039` still exist;
  - authoritative review records for `round-035` through `round-039` still name the
    accepted `C2`, `C3`, `C4`, `E1`, and `E2` artifacts;
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
    'docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md',
    'docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md',
    'docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md',
    'docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md',
    'tasks/todo/2026-03-11-recursive-types-orchestration',
]
missing = [p for p in required_paths if not (root / p).exists()]
if missing:
    raise SystemExit(f'missing required continuity paths: {missing}')

missing_rounds = [
    f'orchestrator/rounds/round-{n:03d}'
    for n in range(1, 40)
    if not (root / f'orchestrator/rounds/round-{n:03d}').exists()
]
if missing_rounds:
    raise SystemExit(f'missing completed round directories: {missing_rounds}')

expected = {
    'round-035': ('C2', 2, 'docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md'),
    'round-036': ('C3', 1, 'docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md'),
    'round-037': ('C4', 1, 'docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md'),
    'round-038': ('E1', 1, 'docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md'),
    'round-039': ('E2', 2, 'docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md'),
}
for round_id, (stage_id, attempt, artifact_path) in expected.items():
    data = json.loads((root / f'orchestrator/rounds/{round_id}/review-record.json').read_text())
    assert data['stage_id'] == stage_id, data
    assert data['attempt_verdict'] == 'accepted', data
    assert data['stage_action'] == 'finalize', data
    assert data['status'] == 'authoritative', data
    assert data['authoritative_attempt'] == attempt, data
    assert data['artifact_path'] == artifact_path, data

print('E3 continuity check: rounds 001-039 present')
print('E3 continuity check: authoritative records round-035 through round-039 intact')
PY
```

- Reconfirm the round diff stays docs-only:
  - `git status --short --untracked-files=all`
  - `git diff --name-only`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
- Because the round worktree already begins with `selection.md` untracked, interpret
  those diff checks narrowly:
  - do not require a globally clean worktree;
  - do require that this round does not add new tracked/untracked edits outside the
    docs/orchestrator artifact set it owns.

### Task 6 - Author the canonical `E3` verification artifact

- Write
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`
  as the canonical stage result.
- Required sections:
  - stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
    `Retry state`, `Live subject`, `Artifact kind`);
  - stage contract freeze;
  - accepted evidence chain carried forward from `C3`, `C4`, `E1`, and `E2`;
  - bounded code/test anchor evidence from `Fallback.hs` and `PipelineSpec.hs`;
  - fresh verification results for baseline checks, focused `ARI-C1`, full repo
    gate, and predecessor continuity;
  - docs-only diff evidence;
  - stability conclusion;
  - blockers;
  - non-authorization statement.
- The artifact must explicitly record that:
  - the retained-child same-lane behavioral success example stayed green;
  - the matched nested-`forall` / nested-owner fail-closed contrast stayed green;
  - the bounded ownership remains `Fallback.hs` plus `PipelineSpec.hs` only;
  - the other `keepTargetFinal` trigger families remain unchanged and fail-closed;
  - repaired `URI-R2-C1` and the inherited boundary remain fixed without widening.

## Required Verification

- Baseline contract checks from
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-040/orchestrator/verification.md`:
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
- Focused bounded block:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Full repo gate:
  - `cabal build all && cabal test`
- Stage-specific evidence:
  - confirm the focused `ARI-C1` rerun still passes with the retained-child
    same-lane behavioral success example and the matched nested-`forall`
    fail-closed contrast both explicitly present in the passing block output context;
  - confirm the read-only `Fallback.hs` anchor still shows `sameLocalTypeLane`,
    `boundHasForallFrom`, the `rootBindingIsLocalType` gate, and unchanged
    fail-closed handling for non-selected trigger families;
  - confirm `round-037`, `round-038`, and `round-039` review records remain
    authoritative and still point at the accepted `C4`, `E1`, and `E2` artifacts;
  - confirm predecessor continuity over completed rounds `round-001` through
    `round-039`, replay-repair evidence, and the recursive-types packet;
  - confirm the round diff remains docs/orchestrator-only.

## Exit Criteria

The round is ready for review only when all of the following are true:

1. the canonical `E3` artifact exists at the path named above;
2. the focused `ARI-C1` block rerun passed and the artifact records the retained-child
   same-lane success example plus the matched nested-`forall` fail-closed contrast as
   still green;
3. the fresh `cabal build all && cabal test` gate passed and the artifact records its
   current result;
4. predecessor continuity through completed rounds `round-001` through `round-039`
   plus replay-repair and recursive-types evidence was rechecked and recorded;
5. no production/test/code-path file was edited for `E3` `attempt-1`;
6. no widening, replay reopen, `MLF.Elab.Inst`, `InstBot`, equi-recursive reasoning,
   cyclic structural encoding, cross-family widening, compatibility widening, or
   second-interface work appeared.
