# Round 056 Plan (`I3` Retry Delta Attempt-2)

## Objective

Execute only the same-round retry for roadmap item `I3`.

This rewrite replaces the stale `attempt-1` plan with the required
`attempt-2` retry delta only. The retry scope is exactly the recorded fix
hypothesis from `orchestrator/rounds/round-056/state-snapshot.json`: restore a working `cabal`
executable on `PATH` in the `round-056` shell, rerun the exact focused
`ARI-C1 feasibility characterization (bounded prototype-only)` command and the
exact full repo gate, then refresh the same canonical `I3` artifact at:

`docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md`

with the actual `attempt-2` outcomes.

All non-retried `attempt-1` contract facts remain in force: this round stays
docs-only, the live subject stays repaired `URI-R2-C1`, the inherited
explicit-only / non-equi-recursive / non-cyclic-graph boundary stays fixed,
and `src/MLF/Elab/Run/ResultType/Fallback.hs` plus
`test/PipelineSpec.hs` remain read-only.

## Locked Retry Context

- Round id: `round-056`
- Roadmap item: `I3`
- Stage: `plan`
- Active attempt: `attempt-2`
- Retry state: active
- Retry reason: `i3-shell-missing-cabal`
- Fix hypothesis:
  `Restore a working cabal executable on PATH in the round-056 shell, rerun the exact focused ARI-C1 command and the exact full repo gate, and refresh the canonical I3 artifact with those actual outcomes while keeping the round docs-only and leaving Fallback.hs / PipelineSpec.hs read-only.`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no second interface / no compatibility or convenience fallback widening`
- Prior accepted scope that remains binding:
  the accepted `I1` / `I2` local single-base
  `rootLocalSingleBase` / `baseTarget -> baseC` / same-lane `targetC` lane
  only

Carry forward without replanning:

- `selection.md` already fixes this round to `I3` only.
- `review.md`, `reviews/attempt-1.md`, and `attempt-log.jsonl` already record
  the accepted `attempt-1` blocker and must remain immutable.
- The existing canonical `I3` artifact already contains the correct bounded
  contract freeze, accepted `I1` / `I2` continuity, read-only anchor evidence,
  and preserved non-authorization sections. `attempt-2` refreshes that same
  file; it does not mint a replacement artifact.

## File Map

### Modify

- `docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md`
  - Responsibility: refresh the existing canonical `I3` artifact with
    `attempt-2` metadata and the actual shell-availability / focused-rerun /
    full-gate outcomes.

### Read-Only Evidence

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - Responsibility: remain the accepted `I2` implementation anchor only; no
    edits are authorized.
- `test/PipelineSpec.hs`
  - Responsibility: remain the accepted focused-test anchor only; no edits are
    authorized.
- `orchestrator/rounds/round-056/review.md`
- `orchestrator/rounds/round-056/reviews/attempt-1.md`
- `orchestrator/rounds/round-056/attempt-log.jsonl`
- `orchestrator/rounds/round-056/state-snapshot.json`
- `orchestrator/rounds/round-056/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-023/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-023/retry-subloop.md`

### Preserve Unchanged

- `orchestrator/rounds/round-056/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-023/roadmap.md`
- `orchestrator/rounds/round-056/selection.md`
- `orchestrator/rounds/round-056/review.md`
- `orchestrator/rounds/round-056/reviews/attempt-1.md`
- `orchestrator/rounds/round-056/attempt-log.jsonl`
- `orchestrator/rounds/round-056/implementation-notes.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `/Volumes/src/mlf4/Bugs.md`

## Sequential Retry Tasks

### Task 1 - Restore a working `cabal` on `PATH` in the `round-056` shell

- In the same `round-056` shell session, prepend `/Users/ares/.ghcup/bin` to
  `PATH` so `cabal` resolves from `PATH` rather than only by absolute-path
  invocation.
- Prove the repair immediately with:
  - `command -v cabal`
  - `cabal --version`
- Record the exact observed path/version results in the canonical `I3`
  artifact.
- If `cabal` still is not callable after the `PATH` repair attempt, refresh
  the canonical `I3` artifact with that exact blocker evidence and stop. Do
  not patch shell startup files, repo scripts, Cabal config, or any Haskell
  source/test file.

### Task 2 - Rerun the exact focused `ARI-C1` command

- After `Task 1` succeeds, run exactly:

  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`

- Record the actual exit status and the actual focused-rerun output in the
  same canonical `I3` artifact.
- Do not substitute a broader test selector, a different runner, or a direct
  `/Users/ares/.ghcup/bin/cabal ...` bypass without first repairing `PATH`.

### Task 3 - Rerun the exact full repo gate

- After `Task 1` succeeds, run exactly:

  `cabal build all && cabal test`

- This retry exists to replace the shell-missing-`cabal` blocker with actual
  verification evidence, so once `cabal` is restored on `PATH`, the attempt
  must capture the real full-gate outcome as well.
- Record the actual exit status and the actual full-gate output summary in the
  same canonical `I3` artifact.

### Task 4 - Refresh the same canonical `I3` artifact with `attempt-2` outcomes only

- Keep the artifact path unchanged:
  `docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md`
- Update the artifact metadata from `attempt-1` / `retry: null` to
  `attempt-2` / active retry state.
- Preserve the accepted `I1` / `I2` continuity, the repaired `URI-R2-C1`
  subject, the inherited explicit-only / non-equi-recursive / non-cyclic-graph
  boundary, the existing read-only anchor narrative, and the preserved
  non-authorization section unless a read-only reinspection is needed to keep
  references truthful.
- Replace the `attempt-1` shell-blocker-only verification notes with the
  actual `attempt-2` evidence from:
  - the `PATH` restoration proof;
  - the exact focused rerun; and
  - the exact full repo gate.
- Keep the artifact docs-only. Do not create a new canonical filename, do not
  write `implementation-notes.md`, and do not edit `Fallback.hs` or
  `PipelineSpec.hs`.

## Completion Criteria

This retry plan is complete only if the implementation stage stays inside all
of the following limits:

1. `cabal` is restored on `PATH` in the `round-056` shell or the failed repair
   attempt is recorded exactly in the same canonical `I3` artifact.
2. The exact focused `ARI-C1` command is rerun after `PATH` repair succeeds,
   and its actual outcome is recorded.
3. The exact full repo gate is rerun after `PATH` repair succeeds, and its
   actual outcome is recorded.
4. The same canonical `I3` artifact is refreshed with `attempt-2` outcomes
   instead of creating a new artifact.
5. The round remains docs-only, keeps repaired `URI-R2-C1`, preserves the
   inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary,
   and leaves `Fallback.hs` / `PipelineSpec.hs` read-only.

## Non-Authorization

This retry delta does not authorize:

- any change to `orchestrator/rounds/round-056/state-snapshot.json`;
- any rewrite of `selection.md`, `review.md`, `reviews/attempt-1.md`,
  `attempt-log.jsonl`, `merge.md`, `review-record.json`, or
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-023/roadmap.md`;
- any production/test/public-API/executable/Cabal edit;
- any widening beyond repaired `URI-R2-C1`;
- any reopening of `I1` target selection or `I2` implementation;
- any reopening of replay work, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget` widening, `boundTarget` overlay materialization,
  `View.hs`, `schemeBodyTarget`, non-local widening, equi-recursive
  reasoning, or cyclic structural graph encoding; or
- any simulation of implement/review/merge-stage results inside this plan.
