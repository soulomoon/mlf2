# Merge Preparation (`round-227` / `milestone-6` / `direction-6a`)

## Lineage

- `roadmap_id`: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- `roadmap_item`:
  `milestone-6` / `direction-6a-freeze-polymorphism-lowerability-contract`
- `extracted_item_id`: `null` (`absent` in round wording)
- `base_branch`: `master`

## Squash Commit Title

`Freeze backend polymorphism lowerability contract`

## Squash Summary

- Merge the approved `milestone-6` /
  `direction-6a-freeze-polymorphism-lowerability-contract` slice for the
  active backend-IR executable-boundary roadmap.
- The approved outcome keeps checked `Backend.IR` permissive for
  `BackendTyAbs` / `BackendTyApp`, freezes LLVM/native lowering as the
  smaller specialization-based lowerable subset, hardens the residual
  zero-arity `unspecialized polymorphic binding` rejection path, adds the
  focused `BackendLLVMSpec` row-6 evidence plus the exact repository guard,
  and flips only mechanism-table row 6 to `YES` while row 7 stays `NO`.
- No controller-state, pointer-stub, or active-roadmap-bundle edit belongs to
  the squash substance.

## Payload

- Repo-facing payload files:
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`.
- Round-owned payload artifacts:
  `orchestrator/rounds/round-227/selection.md`,
  `orchestrator/rounds/round-227/plan.md`,
  `orchestrator/rounds/round-227/implementation-notes.md`,
  `orchestrator/rounds/round-227/review.md`,
  `orchestrator/rounds/round-227/review-record.json`,
  `orchestrator/rounds/round-227/reviews/attempt-1.md`, and
  `orchestrator/rounds/round-227/merge.md`.

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

- `orchestrator/rounds/round-227/review.md` records
  `Implemented stage result: accepted`,
  `Attempt verdict: accepted`, and
  `Stage action: finalize`.
- The latest review snapshot is
  `orchestrator/rounds/round-227/reviews/attempt-1.md`; it records the same
  lawful `accepted + finalize` result.
- `orchestrator/rounds/round-227/review-record.json` matches the same lineage
  and records `decision: approved`, `merge_readiness: satisfied`, and
  `controller_merge_hygiene: satisfied`.
- Base-branch freshness is satisfied: `HEAD`, `master`, and
  `git merge-base HEAD master` all resolve to
  `b4e239c5d6fafd55ed56ed81154ec4e3faf79ac8`, and
  `git rev-list --left-right --count master...HEAD` reports `0 0`.
- `git diff --check` passed.
- The canonical round worktree diff remains the expected live uncommitted
  delta against `master`: the tracked diff contains the eight repo-facing
  payload files above plus the four controller-owned pointer/state files, and
  the round-owned artifacts remain untracked under
  `orchestrator/rounds/round-227/`.
- Focused milestone-6 verification passed:
  the focused LLVM row-6 contract slice `8 examples, 0 failures`;
  the exact repository guard `1 example, 0 failures`; and
  the mechanism-table gate reports rows 1-6 = `YES` and row 7 = `NO`.
- Full required gate passed:
  `cabal build all && cabal test` reported `2352 examples, 0 failures`.

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
- This round updates the same family settlement contract and evidence ledger
  by freezing only the row-6 specialization-versus-rejection boundary and
  flipping only mechanism-table row 6 to `YES`; row 7 remains `NO`, so the
  next live blocker stays `milestone-7` /
  `direction-7a-close-the-mechanism-table-and-guidance-ledger`.
- The completed predecessor roadmap family `rev-027` remains immutable
  background evidence only; this round does not reopen it.

## Residual Risks

- `milestone-7` remains intentionally open, and mechanism-table row 7 stays
  `NO`.
- The accepted result freezes only the current specialization-based row-6
  lowerability boundary, so any broader runtime-polymorphism support, public
  lowering surface, fallback lowering lane, or second executable IR will need
  new evidence and a later accepted round or roadmap revision.

## Merge Readiness

- Latest review snapshot: confirmed `accepted + finalize`.
- Merge readiness: confirmed for the approved bounded payload listed above.
- `round-227` is ready for squash merge, provided staging stays limited to the
  approved payload and continues to exclude controller-owned bookkeeping and
  the active roadmap bundle.

## Controller Merge Instructions

1. From the parent workspace root, stay on `master`, confirm the base commit
   is still `b4e239c5d6fafd55ed56ed81154ec4e3faf79ac8`, and create only the
   round-owned destination directory:

   ```bash
   cd /Volumes/src/mlf4
   git checkout master
   test "$(git rev-parse HEAD)" = "b4e239c5d6fafd55ed56ed81154ec4e3faf79ac8"
   mkdir -p orchestrator/rounds/round-227/reviews
   ```

2. Copy only the approved payload from the canonical round worktree:

   ```bash
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/docs/architecture.md docs/architecture.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/docs/backend-native-pipeline.md docs/backend-native-pipeline.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/src/MLF/Backend/IR.hs src/MLF/Backend/IR.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/src/MLF/Backend/Convert.hs src/MLF/Backend/Convert.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/src/MLF/Backend/LLVM/Lower.hs src/MLF/Backend/LLVM/Lower.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/test/BackendLLVMSpec.hs test/BackendLLVMSpec.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/test/RepoGuardSpec.hs test/RepoGuardSpec.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/orchestrator/rounds/round-227/selection.md orchestrator/rounds/round-227/selection.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/orchestrator/rounds/round-227/plan.md orchestrator/rounds/round-227/plan.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/orchestrator/rounds/round-227/implementation-notes.md orchestrator/rounds/round-227/implementation-notes.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/orchestrator/rounds/round-227/review.md orchestrator/rounds/round-227/review.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/orchestrator/rounds/round-227/review-record.json orchestrator/rounds/round-227/review-record.json
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/orchestrator/rounds/round-227/reviews/attempt-1.md orchestrator/rounds/round-227/reviews/attempt-1.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-227/orchestrator/rounds/round-227/merge.md orchestrator/rounds/round-227/merge.md
   ```

3. Stage only those copied files and nothing controller-owned:

   ```bash
   git add -- \
     docs/architecture.md \
     docs/backend-native-pipeline.md \
     docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md \
     src/MLF/Backend/IR.hs \
     src/MLF/Backend/Convert.hs \
     src/MLF/Backend/LLVM/Lower.hs \
     test/BackendLLVMSpec.hs \
     test/RepoGuardSpec.hs \
     orchestrator/rounds/round-227/selection.md \
     orchestrator/rounds/round-227/plan.md \
     orchestrator/rounds/round-227/implementation-notes.md \
     orchestrator/rounds/round-227/review.md \
     orchestrator/rounds/round-227/review-record.json \
     orchestrator/rounds/round-227/reviews/attempt-1.md \
     orchestrator/rounds/round-227/merge.md
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

   The first command should show only the 15 approved payload files. The
   second command should print nothing.

5. Create the squash commit with the approved title:

   ```bash
   git commit -m "Freeze backend polymorphism lowerability contract"
   ```

## Follow-Up Notes

- Preserve the lineage fields above unchanged in controller closeout.
- Keep later summaries honest about scope: this merge settles only the
  milestone-6 polymorphism-erasure/lowerability contract and mechanism-table
  row 6.
- Do not describe this merge as authorizing runtime polymorphism, a second
  backend IR, a public lowering surface, fallback lowering/runtime rescue,
  milestone-7 closeout, or final family completion.
