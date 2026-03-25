# Round 042 Retry Delta Plan (`F1` Attempt `3`)

## Delta Scope

Repair only the rejected `attempt-2` mismatch recorded as
`f1-review-authority-path-mismatch`.

All repo-local paths in this plan are relative to the active packet rooted at:

`.worktrees/round-042`

The one canonical `F1` bind artifact path remains:

`docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md`

This retry does not redesign `F1`. It preserves the already-selected live
subject, the frozen `F2` slice, the owned future files, the exclusions, and the
docs-only verification/full-gate-skip semantics from `attempt-2`. The only
required repair is to retarget this plan's round-local review-history
references from missing parent-workspace absolute paths to active-packet
repo-relative paths while leaving the canonical artifact path and all other
accepted `F1` content unchanged.

## Locked Retry Context

- Round: `round-042`
- Roadmap item: `F1`
- Stage: `plan`
- Active attempt: `attempt-3`
- Latest attempt verdict: `rejected`
- Latest stage action: `retry`
- Retry reason: `f1-review-authority-path-mismatch`
- Fix hypothesis: `Retarget the round-local review-history references in orchestrator/rounds/round-042/plan.md to the active .worktrees/round-042 packet (or use relative round-local paths) while preserving the now-correct 2026-03-19 canonical F1 artifact path, frozen F2 slice, exclusions, and docs-only verification semantics unchanged.`
- Live subject remains repaired `URI-R2-C1`
- Boundary remains `explicit-only / non-equi-recursive / non-cyclic-graph`

The rejected review authority that this retry must satisfy remains:

- `orchestrator/rounds/round-042/review.md`
- `orchestrator/rounds/round-042/reviews/attempt-1.md`
- `orchestrator/rounds/round-042/attempt-log.jsonl`

The canonical artifact and notes that must remain unchanged remain:

- `docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md`
- `orchestrator/rounds/round-042/implementation-notes.md`

`attempt-2` already repaired the `2026-03-18` versus `2026-03-19` artifact-path
mismatch. This retry must not rename the artifact, rewrite its verification
notes, or refresh implementation notes again; it must only normalize the
round-local review-history references inside this plan.

## File Map

### Modify

- `orchestrator/rounds/round-042/plan.md`
  - Responsibility: retarget review-history and evidence-only round-local
    references to active-packet repo-relative paths, update retry metadata to
    `attempt-3`, and preserve the already-correct canonical artifact path,
    frozen `F2` slice, exclusions, and docs-only semantics unchanged.
  - Why this file: `attempt-2` was rejected only because this plan still pointed
    key review-history references at missing parent-workspace paths.

### Evidence-Only, No Planned Edit

- `docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md`
- `orchestrator/rounds/round-042/implementation-notes.md`
- `orchestrator/rounds/round-042/selection.md`
- `orchestrator/rounds/round-042/review.md`
- `orchestrator/rounds/round-042/reviews/attempt-1.md`
- `orchestrator/rounds/round-042/attempt-log.jsonl`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`

These files already carry the canonical artifact path, frozen `F2` slice,
exclusions, and retry history that this retry must preserve. This retry must
not edit them.

## Sequential Delta Tasks

### Task 1 - Reframe the retry around the actual rejected `attempt-2` issue

- Update this plan to name `attempt-3`,
  `f1-review-authority-path-mismatch`, and the latest fix hypothesis from
  `attempt-log.jsonl`.
- State explicitly that the `2026-03-19` canonical artifact path is already
  correct and is not being changed in this retry.
- Keep this plan retry-only. Do not restore the earlier artifact-rename or
  implementation-notes-refresh plan.

### Task 2 - Retarget round-local review-history references to the active packet

- Replace the missing parent-workspace absolute review-history references in
  this plan with active-packet repo-relative paths.
- The round-local review-history and evidence-only references in this plan must
  resolve as:
  - `orchestrator/rounds/round-042/review.md`
  - `orchestrator/rounds/round-042/reviews/attempt-1.md`
  - `orchestrator/rounds/round-042/attempt-log.jsonl`
  - `orchestrator/rounds/round-042/selection.md`
- Keep non-round-local surfaces repo-relative to the active packet as well
  (`docs/...`, `src/...`, `test/...`) rather than parent-workspace absolute.

### Task 3 - Preserve the already-correct canonical artifact and notes unchanged

- Keep `docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md` as the only
  canonical `F1` artifact path.
- Do not rename the artifact, rewrite its body, or restate the `F1` bind
  differently.
- Do not edit `orchestrator/rounds/round-042/implementation-notes.md`; it
  already cites the canonical artifact path and carries the correct docs-only
  full-gate-skip note.

### Task 4 - Keep the bounded `F1` contract byte-stable apart from review-history path retargeting

- Preserve the already-correct `F1` scope:
  - docs-only bind/selection stage only;
  - no production edits, no test edits, no roadmap mutation, no bug-tracker
    edits, and no controller-state edits.
- Preserve the already-frozen `F2` target exactly:
  - the local-binding `rootIsSchemeAlias && rootBoundIsBaseLike`
    `keepTargetFinal` / `targetC` lane in
    `src/MLF/Elab/Run/ResultType/Fallback.hs`;
  - focused future coverage only in `test/PipelineSpec.hs`;
  - no reopening of the inherited same-lane retained-child baseline except as
    carried-forward context.
- Preserve the already-correct exclusions exactly:
  - no replay reopen;
  - no `MLF.Elab.Inst` or `InstBot`;
  - no `rootHasMultiInst` or `instArgRootMultiBase` as selected target
    families;
  - no equi-recursive reasoning, implicit unfolding, cyclic structural graph
    encoding, multi-SCC widening, cross-family widening, heuristic ownership
    ranking, compatibility fallbacks, convenience widening, or second
    executable interface.

### Task 5 - Re-run only the docs-only exactness checks needed to clear the retry reason

- Keep the same docs-only verification scope and the same intentional full-gate
  skip rationale from `attempt-2`.
- Reconfirm the preserved canonical artifact path:
  - `test -f docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md`
  - `rg -n '2026-03-19-uri-r2-c1-f1-next-target-bind.md' docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md orchestrator/rounds/round-042/implementation-notes.md orchestrator/rounds/round-042/plan.md`
- Reconfirm the plan now points at active-packet round-local review-history
  paths instead of missing parent-workspace absolute paths:
  - `test -e orchestrator/rounds/round-042/review.md`
  - `test -e orchestrator/rounds/round-042/reviews/attempt-1.md`
  - `test -e orchestrator/rounds/round-042/attempt-log.jsonl`
  - `test -e orchestrator/rounds/round-042/selection.md`
  - `rg -n 'orchestrator/rounds/round-042/(review\\.md|reviews/attempt-1\\.md|attempt-log\\.jsonl|selection\\.md)' orchestrator/rounds/round-042/plan.md`
    - expected: no output
  - `rg -n 'orchestrator/rounds/round-042/(review\\.md|reviews/attempt-1\\.md|attempt-log\\.jsonl|selection\\.md)' orchestrator/rounds/round-042/plan.md`
- Keep docs-only diff evidence:
  - `git diff --check`
  - `git status --short --untracked-files=all`
  - `git diff --name-only`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
- Do not rerun `cabal build all && cabal test` unless the retry escapes
  docs/orchestrator surfaces, which this plan forbids.

## Non-Goals

- No change to the selected `F2` slice, its frozen future ownership, or its
  semantic intent.
- No change to the accepted `E1` / `E2` / `E3` / `E4` carry-forward chain.
- No change to the binding effect of accepted `U2`, `U3`, or `U4`.
- No edits to `docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md`,
  `orchestrator/rounds/round-042/implementation-notes.md`,
  `orchestrator/rounds/round-042/state-snapshot.json`, `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-009/roadmap.md`, `/Volumes/src/mlf4/Bugs.md`,
  production code, tests, public API surfaces, executable entrypoints, or
  `mlf2.cabal`.
- No rewrite of review history, `reviews/attempt-1.md`, `review.md`, or
  `attempt-log.jsonl`.
- No alternate canonical filename choice. This retry must keep the
  `2026-03-19` path only.

## Reviewer Checks For This Retry

1. `plan.md` explicitly names `attempt-3` and frames the retry as a delta-only
   repair for `f1-review-authority-path-mismatch`.
2. All round-local review-history references in `plan.md` resolve to the active
   packet via active-packet repo-relative paths:
   `orchestrator/rounds/round-042/review.md`,
   `orchestrator/rounds/round-042/reviews/attempt-1.md`,
   `orchestrator/rounds/round-042/attempt-log.jsonl`, and
   `orchestrator/rounds/round-042/selection.md`.
3. The canonical `F1` artifact path remains
   `docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md`, and the plan no
   longer proposes renaming or reauthoring it.
4. The frozen `F2` slice, future ownership, exclusions, and docs-only/full-gate
   skip semantics are unchanged from the rejected `attempt-2` packet.
5. The retry remains docs/orchestrator only, with no production/test/public
   API/Cabal/roadmap/controller-state/bug-tracker edits.
