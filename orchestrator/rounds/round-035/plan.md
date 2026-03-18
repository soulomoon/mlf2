# Round 035 Plan (`C2` Retry Delta, `attempt-2`)

## Objective

Close the rejected `attempt-1` gap recorded as
`c2-non-local-proxy-entrypoint-coverage-missing`
without replanning the whole stage.

This retry only adds the missing bounded evidence for the already-landed fail-closed behavior:
the same non-local proxy wrapper case
`let g = (\x : mu a. a -> Int. x) in g g`
must be covered in `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`
through both `runPipelineElab` and `runPipelineElabChecked`,
and the canonical `C2` artifact at
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
must record that entrypoint evidence.

## Retry Context

- Round: `round-035`
- Roadmap item: `C2`
- Stage: `plan`
- Active attempt: `attempt-2`
- Latest attempt verdict: `rejected`
- Latest stage action: `retry`
- Retry reason: `c2-non-local-proxy-entrypoint-coverage-missing`
- Fix hypothesis: add focused Hspec coverage in `test/PipelineSpec.hs` for the same non-local proxy wrapper case across both `runPipelineElab` and `runPipelineElabChecked`, keep the diff inside the existing bounded slice, and update the canonical `C2` artifact to record that evidence.

`attempt-1` already established that the production behavior in
`/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`
is fail-closed in the intended lane and that the full repo gate passes.
This retry does not reopen that production decision.
It only repairs the missing bounded entrypoint evidence.

The live subject and inherited boundary remain unchanged:

- repaired `URI-R2-C1` only;
- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

Accepted `C1`, `U5`, `U6`, `U2`, `U3`, and `U4` remain binding.
This retry does not reinterpret negative findings as clearance.

## File Map

### Modify / Test

- `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`
  - Responsibility: repair the bounded `C2` negative coverage so the exact non-local proxy wrapper case already exercised through `computeResultTypeFallback` is also exercised through both pipeline entrypoints.
  - Why this file: the rejected review found the missing evidence here and nowhere else.

### Modify

- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
  - Responsibility: refresh the canonical `C2` artifact for `attempt-2` and record the repaired entrypoint evidence for the same bounded case.
  - Why this file: the stage contract requires the canonical artifact to reflect the evidence actually added by the retry.

### Evidence-Only, No Planned Edit

- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`
  - Responsibility: unchanged anchor for the already-landed `rootBindingIsLocalType` fail-closed retention gate.
  - Why this file stays read-only in this retry: the review did not find a production defect; widening the retry into new logic would violate the recorded fix hypothesis.

## Sequential Delta Tasks

### Task 1 - Re-anchor the retry contract to `attempt-2`

- Treat this document as a delta over the rejected `attempt-1` plan, not a fresh stage redesign.
- Keep the stage bounded to the existing `Fallback.hs` + `PipelineSpec.hs` slice plus the canonical `C2` artifact only.
- Preserve reviewer-owned history:
  - do not touch `reviews/attempt-1.md`;
  - do not touch `attempt-log.jsonl`;
  - do not touch controller-owned `orchestrator/state.json`.
- The canonical `C2` artifact is the only stage artifact that should be refreshed for `attempt-2`.

### Task 2 - Retarget the negative `PipelineSpec` entrypoint evidence to the same proxy-wrapper case

- Work only inside the existing
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`.
- Reuse the exact expression already used by the direct reconstruction control:
  `let g = (\x : mu a. a -> Int. x) in g g`
  (encoded in the existing Haskell AST form with `ELet`, `ELamAnn`, and `EApp`).
- Keep the direct `computeResultTypeFallback` fail-closed check for that expression.
- Replace or retarget the stale unchecked/checked entrypoint negative example that currently uses the unrelated out-of-scope unannotated proxy shape
  `\f -> f f`.
  Preferred minimal change: reuse that example slot so the block stays bounded and does not grow a new test family.
- Add bounded entrypoint assertions for the same `g g` expression across both:
  - `runPipelineElab`
  - `runPipelineElabChecked`
- The assertions must show the proxy-wrapper case stays fail-closed at the pipeline entrypoints, not merely at direct reconstruction.
  If matching the concrete error constructor is straightforward, prefer that.
  If the surrounding helpers make constructor matching noisy, it is still acceptable to assert that both entrypoints reject this exact expression, provided the failure messages clearly name the same proxy-wrapper case.
- Keep the existing positive controls and the `rootBindingIsLocalType` source guard intact.
- Do not add a new test module, helper surface, or broader recursive-inference family.

### Task 3 - Refresh the canonical `C2` artifact with the repaired evidence

- Update
  `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
  for `attempt-2`.
- Record that the same non-local proxy wrapper case is now evidenced in three bounded ways:
  - direct `computeResultTypeFallback` fail-closed reconstruction;
  - unchecked `runPipelineElab` rejection for that same expression;
  - checked `runPipelineElabChecked` rejection for that same expression.
- State explicitly that this retry does not reopen `Fallback.hs` logic, `MLF.Elab.Inst`, replay repair, or any widening beyond repaired `URI-R2-C1`.
- Update any focused verification notes so the command output and example counts match the post-retry test run instead of the rejected `attempt-1` snapshot.

### Task 4 - Run the bounded verification needed for retry closure

- Re-run baseline checks required by `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`.
- Re-run the focused bounded spec block:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Because this retry still edits `test/`, the full repo gate remains mandatory:
  - `cabal build all && cabal test`
- Reconfirm the diff stays bounded:
  - only `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`
    and `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
    should need changes for the recorded fix hypothesis;
  - no edits to `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`
    are planned for this retry unless a new blocking mismatch is discovered first.

### Task 5 - Hand reviewer the exact evidence needed to clear retry reason

- Ensure the reviewer can point to one bounded `PipelineSpec` region showing:
  - the shared `g g` proxy-wrapper expression;
  - the direct `computeResultTypeFallback` fail-closed check for that expression;
  - unchecked `runPipelineElab` rejection for that same expression;
  - checked `runPipelineElabChecked` rejection for that same expression.
- Ensure the canonical `C2` artifact explicitly names the repaired evidence and the fact that the retry remained within the original bounded slice.
- Preserve the reviewer outcome contract from `orchestrator/retry-subloop.md`:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`

## Non-Goals

- No new production logic in `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs` unless a newly discovered blocker proves the review diagnosis incomplete.
- No edits to `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`.
- No replay reopen, `InstBot` work, prototype/research entrypoints, public API changes, executable changes, or `mlf2.cabal` changes.
- No roadmap mutation, no `Bugs.md` edit, and no rewrite of prior review artifacts.
- No widening into equi-recursive reasoning, implicit unfolding, cyclic structural graph encoding, multi-SCC support, cross-family search, heuristic owner selection, compatibility shims, or convenience fallbacks.

## Reviewer Checks For This Retry

1. `plan.md` explicitly names `attempt-2`, the retry reason, and the fix hypothesis as a delta only.
2. The diff stays inside the existing bounded `C2` slice and canonical artifact, with no `MLF.Elab.Inst` or replay-lane edits.
3. `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs` uses the same non-local proxy wrapper expression for:
   - direct `computeResultTypeFallback` fail-closed evidence;
   - unchecked `runPipelineElab` fail-closed evidence;
   - checked `runPipelineElabChecked` fail-closed evidence.
4. The stale unrelated `\f -> f f` entrypoint-negative example no longer stands in for the bounded `g g` proxy-wrapper evidence inside this `C2` block.
5. The canonical `C2` artifact records the repaired entrypoint evidence and still states that no widening or production-lane reopen occurred.
6. The focused spec block passes, the full repo gate passes, and the recorded verification output matches the final tree state.
