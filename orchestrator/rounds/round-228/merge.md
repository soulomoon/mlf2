# Merge Preparation (`round-228` / `milestone-7` / `direction-7a`)

## Lineage

- `roadmap_id`: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- `milestone_id`: `milestone-7`
- `direction_id`: `direction-7a-close-the-mechanism-table-and-guidance-ledger`
- `extracted_item_id`: `null` (`absent` in round wording)
- `base_branch`: `master`

## Squash Commit Title

`Close backend-boundary guidance ledger and harden row-7 guard`

## Squash Summary

- Merge the approved `milestone-7` /
  `direction-7a-close-the-mechanism-table-and-guidance-ledger` closeout slice
  for the active backend-IR executable-boundary roadmap.
- The approved result closes row 7 on merged `710c92eb`, synchronizes the
  repo-facing closeout ledger across `TODO.md`, `implementation_notes.md`,
  `CHANGELOG.md`, and the mechanism table, and hardens the dedicated
  `backend-boundary mechanism table and closeout ledger stay synchronized`
  repository guard so the preserved boundary remains explicit:
  one executable eager backend IR,
  no public `LowerableBackend.IR`, and
  no lazy STG machinery.
- Scope stays bounded to family closeout and guard hardening only. This round
  does not introduce a new backend feature, public/backend surface, or new
  backend architecture decision.

## Payload

- Repo-facing payload files:
  `CHANGELOG.md`,
  `TODO.md`,
  `implementation_notes.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/RepoGuardSpec.hs`
- Round-owned payload artifacts:
  `orchestrator/rounds/round-228/selection.md`,
  `orchestrator/rounds/round-228/plan.md`,
  `orchestrator/rounds/round-228/implementation-notes.md`,
  `orchestrator/rounds/round-228/review.md`,
  `orchestrator/rounds/round-228/review-record.json`,
  `orchestrator/rounds/round-228/reviews/attempt-1.md`,
  `orchestrator/rounds/round-228/reviews/attempt-2.md`, and
  `orchestrator/rounds/round-228/merge.md`.

## Exclusions

- Controller-owned files to leave untouched and unstaged:
  `orchestrator/state.json`,
  `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, and
  `orchestrator/retry-subloop.md`.
- Active roadmap bundle to exclude from this squash unless controller closeout
  later requires it:
  `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/**`.
- Any other non-payload file in the canonical round worktree remains
  excluded, including untracked controller-owned roadmap-bundle files and any
  file not named explicitly in `Payload`.

## Verification Summary

- `orchestrator/rounds/round-228/review.md` records
  `Implemented stage result: accepted`,
  `Attempt verdict: accepted`, and
  `Stage action: finalize`.
- The latest review snapshot is
  `orchestrator/rounds/round-228/reviews/attempt-2.md`; it records the same
  lawful `accepted + finalize` result.
- `orchestrator/rounds/round-228/review-record.json` matches the same lineage
  and records
  `decision: approved`,
  `merge_readiness: satisfied`, and
  `controller_merge_hygiene: satisfied`.
- Base-branch freshness is satisfied:
  `HEAD`, `master`, and `git merge-base HEAD master` all resolve to
  `710c92eb8f4d87961f4b7bc76b5dc21f645f9220`, and
  `git rev-list --left-right --count master...HEAD` reports `0 0`.
- `git diff --check` passed.
- The focused row-7 repository guard passed:
  `1 example, 0 failures`.
- The independent closeout-ledger script passed for
  `TODO.md`,
  `implementation_notes.md`, and
  `CHANGELOG.md`, and the independent mechanism-table readout reported all
  seven rows at `YES`.
- The full required gate passed:
  `cabal build all && cabal test` reported `2353 examples, 0 failures`.
- The canonical round worktree diff remains the expected live uncommitted
  delta against `master`: the tracked diff contains the five repo-facing
  payload files above plus the four controller-owned pointer/state files, and
  the round-owned artifacts remain untracked under
  `orchestrator/rounds/round-228/`.

## Predecessor Continuity

- Accepted `round-222`, merged as `5365d975`, remains the authoritative
  milestone-1 one-backend-IR contract freeze.
- Accepted `round-223`, merged as `006eb569`, remains the authoritative
  milestone-2 eager-runtime lowering contract freeze.
- Accepted `round-224`, merged as `2c1661b3`, remains the authoritative
  milestone-3 callable-shape contract freeze.
- Accepted `round-225`, merged as `5adb3702`, remains the authoritative
  milestone-4 ADT/case semantic-versus-layout ownership freeze.
- Accepted `round-226`, merged as `b4e239c5`, remains the authoritative
  milestone-5 primitive/eager sequencing contract freeze.
- Accepted `round-227`, merged as `710c92eb`, remains the authoritative
  milestone-6 polymorphism-lowerability contract freeze.
- This round updates the same family settlement gate and evidence ledger only
  by closing row 7 on top of merged `710c92eb`; it does not reopen rows 1
  through 6 or change the preserved backend boundary.
- The completed predecessor roadmap family `rev-027` remains immutable
  background evidence only; this round does not reopen it.

## Residual Risk / Follow-Up Notes

- The row-7 closeout now depends on synchronized wording across the mechanism
  table, `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, and the focused
  repository guard, so later wording-only edits must keep those markers aligned.
- Any future widening to a public `LowerableBackend.IR`, a second executable
  backend IR, lazy STG machinery, fallback/runtime-rescue behavior, or a
  broader backend contract still requires a later accepted roadmap revision.

## Merge Readiness

- Latest review snapshot: confirmed `accepted + finalize`.
- Merge readiness: confirmed for the approved bounded payload listed above.
- `round-228` is ready for squash merge, provided staging stays limited to the
  approved payload and continues to exclude controller-owned bookkeeping and
  the active roadmap bundle.

## Controller Merge Instructions

1. From the parent workspace root, stay on `master`, confirm the base commit
   is still `710c92eb8f4d87961f4b7bc76b5dc21f645f9220`, and create only the
   round-owned destination directory:

   ```bash
   cd /Volumes/src/mlf4
   git checkout master
   test "$(git rev-parse HEAD)" = "710c92eb8f4d87961f4b7bc76b5dc21f645f9220"
   mkdir -p orchestrator/rounds/round-228/reviews
   ```

2. Copy only the approved payload from the canonical round worktree:

   ```bash
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/CHANGELOG.md CHANGELOG.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/TODO.md TODO.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/implementation_notes.md implementation_notes.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/test/RepoGuardSpec.hs test/RepoGuardSpec.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/orchestrator/rounds/round-228/selection.md orchestrator/rounds/round-228/selection.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/orchestrator/rounds/round-228/plan.md orchestrator/rounds/round-228/plan.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/orchestrator/rounds/round-228/implementation-notes.md orchestrator/rounds/round-228/implementation-notes.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/orchestrator/rounds/round-228/review.md orchestrator/rounds/round-228/review.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/orchestrator/rounds/round-228/review-record.json orchestrator/rounds/round-228/review-record.json
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/orchestrator/rounds/round-228/reviews/attempt-1.md orchestrator/rounds/round-228/reviews/attempt-1.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/orchestrator/rounds/round-228/reviews/attempt-2.md orchestrator/rounds/round-228/reviews/attempt-2.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-228/orchestrator/rounds/round-228/merge.md orchestrator/rounds/round-228/merge.md
   ```

3. Stage only those copied files and nothing controller-owned:

   ```bash
   git add -- \
     CHANGELOG.md \
     TODO.md \
     implementation_notes.md \
     docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md \
     test/RepoGuardSpec.hs \
     orchestrator/rounds/round-228/selection.md \
     orchestrator/rounds/round-228/plan.md \
     orchestrator/rounds/round-228/implementation-notes.md \
     orchestrator/rounds/round-228/review.md \
     orchestrator/rounds/round-228/review-record.json \
     orchestrator/rounds/round-228/reviews/attempt-1.md \
     orchestrator/rounds/round-228/reviews/attempt-2.md \
     orchestrator/rounds/round-228/merge.md
   ```

4. Verify the staged set is exact and that controller-owned files remain
   unstaged:

   ```bash
   git diff --cached --name-only
   git diff --cached --name-only -- \
     orchestrator/state.json \
     orchestrator/roadmap.md \
     orchestrator/verification.md \
     orchestrator/retry-subloop.md \
     orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001
   git status --short
   ```

   The first command should show only the 13 approved payload files. The
   second command should print nothing.

5. Create the squash commit with the approved title:

   ```bash
   git commit -m "Close backend-boundary guidance ledger and harden row-7 guard"
   ```

## Follow-Up Notes

- Preserve the lineage fields above unchanged in controller closeout.
- Keep later summaries honest about scope: this merge settles only the
  milestone-7 row-7 closeout, evidence-ledger synchronization, and guard
  hardening on top of merged `710c92eb`.
- Do not describe this merge as authorizing a new backend feature, a second
  backend IR, a public lowering surface, lazy runtime machinery, fallback
  lowering/runtime rescue, or any broader backend-boundary widening.
