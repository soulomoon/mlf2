# Merge Preparation (`round-225` / `milestone-4` / `direction-4a`)

## Lineage

- `roadmap_id`: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- `roadmap_item`: `milestone-4` / `direction-4a-freeze-adt-layout-ownership`
- `extracted_item_id`: `null` (`absent` in round wording)
- `base_branch`: `master`

## Squash Commit Title

`Freeze ADT/case semantic ownership and keep layout policy private`

## Squash Summary

- Merge the approved `milestone-4` /
  `direction-4a-freeze-adt-layout-ownership` slice for the active
  backend-IR executable-boundary roadmap.
- The approved outcome keeps `BackendData`, `BackendConstructor`,
  `BackendConstruct`, and `BackendCase` semantic-only at the
  `MLF.Backend.IR` boundary, makes the private LLVM/native layout policy
  explicit in the synchronized contract surfaces, strengthens the focused
  `BackendLLVMSpec` row-4 evidence and synchronized repository guard, and
  flips only mechanism-table row 4 to `YES`.
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
  `orchestrator/rounds/round-225/selection.md`,
  `orchestrator/rounds/round-225/plan.md`,
  `orchestrator/rounds/round-225/implementation-notes.md`,
  `orchestrator/rounds/round-225/review.md`,
  `orchestrator/rounds/round-225/review-record.json`,
  `orchestrator/rounds/round-225/reviews/attempt-1.md`, and
  `orchestrator/rounds/round-225/merge.md`.

## Exclusions

- Controller-owned files to leave untouched and unstaged:
  `orchestrator/state.json`,
  `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, and
  `orchestrator/retry-subloop.md`.
- Active roadmap bundle to exclude from this squash unless controller closeout
  later requires it:
  `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/**`.
- Any other non-payload file in the canonical round worktree remains excluded,
  including untracked controller-owned roadmap-bundle files and any file not
  named explicitly in `Payload`.

## Verification Summary

- `orchestrator/rounds/round-225/review.md` records
  `Implemented stage result: accepted`,
  `Attempt verdict: accepted`, and
  `Stage action: finalize`.
- The latest review snapshot is
  `orchestrator/rounds/round-225/reviews/attempt-1.md`; it records the same
  lawful `accepted + finalize` result.
- `orchestrator/rounds/round-225/review-record.json` matches the same lineage
  and records `decision: approved`, `merge_readiness: satisfied`, and
  `controller_merge_hygiene: satisfied`.
- Base-branch freshness is satisfied: `HEAD`, `master`, and
  `git merge-base HEAD master` all resolve to
  `2c1661b3a21fea0065c6f68c57bac2d5b2585f23`, and
  `git rev-list --left-right --count master...HEAD` reports `0 0`.
- `git diff --check` passed.
- The canonical round worktree diff remains the expected live uncommitted
  delta against `master`: the tracked diff contains the eight repo-facing
  payload files above plus the four controller-owned pointer/state files, and
  the round-owned artifacts remain untracked under `orchestrator/rounds/round-225/`.
- Focused milestone-4 verification passed:
  repo guard `1 example, 0 failures`;
  focused `BackendIR` and `BackendConvert` ADT/case baseline slices
  `1 example, 0 failures` each;
  focused `BackendLLVM` tag/switch, field-offset, closure-field-ABI, and
  nullary-layout slices `1 example, 0 failures` each; and
  the mechanism-table gate reports rows 1-4 = `YES` and rows 5-7 = `NO`.
- Full required gate passed:
  `cabal build all && cabal test` reported `2341 examples, 0 failures`.

## Predecessor Continuity

- Accepted `round-222`, merged as `5365d975`, remains the authoritative
  milestone-1 one-backend-IR contract freeze.
- Accepted `round-223`, merged as `006eb569`, remains the authoritative
  milestone-2 eager-runtime lowering contract freeze.
- Accepted `round-224`, merged as `2c1661b3`, remains the authoritative
  milestone-3 callable-shape contract freeze.
- This round updates the same family evidence ledger by flipping only
  mechanism-table row 4 to `YES`; rows 5 through 7 remain `NO`, so the next
  live blocker stays `milestone-5` /
  `direction-5a-lock-primitive-and-evaluation-order-contract`.
- The completed predecessor roadmap family `rev-027` remains immutable
  background evidence only; this round does not reopen it.

## Residual Risks

- `milestone-5` through `milestone-7` remain intentionally open, and
  mechanism-table rows 5 through 7 still stay `NO`.
- The accepted result freezes the current private lowerer-owned layout policy
  rather than evaluating alternate runtime layouts, so any later
  representation redesign will need new evidence and a new accepted round.

## Merge Readiness

- Latest review snapshot: confirmed `accepted + finalize`.
- Merge readiness: confirmed for the approved bounded payload listed above.
- `round-225` is ready for squash merge, provided staging stays limited to the
  approved payload and continues to exclude controller-owned bookkeeping and
  the active roadmap bundle.

## Controller Merge Instructions

1. From the parent workspace root, stay on `master` and create only the
   round-owned destination directory:

   ```bash
   cd /Volumes/src/mlf4
   git checkout master
   mkdir -p orchestrator/rounds/round-225/reviews
   ```

2. Copy only the approved payload from the canonical round worktree:

   ```bash
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/docs/architecture.md docs/architecture.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/docs/backend-native-pipeline.md docs/backend-native-pipeline.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/src/MLF/Backend/IR.hs src/MLF/Backend/IR.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/src/MLF/Backend/Convert.hs src/MLF/Backend/Convert.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/src/MLF/Backend/LLVM/Lower.hs src/MLF/Backend/LLVM/Lower.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/test/BackendLLVMSpec.hs test/BackendLLVMSpec.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/test/RepoGuardSpec.hs test/RepoGuardSpec.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/selection.md orchestrator/rounds/round-225/selection.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/plan.md orchestrator/rounds/round-225/plan.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/implementation-notes.md orchestrator/rounds/round-225/implementation-notes.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/review.md orchestrator/rounds/round-225/review.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/review-record.json orchestrator/rounds/round-225/review-record.json
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/reviews/attempt-1.md orchestrator/rounds/round-225/reviews/attempt-1.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/merge.md orchestrator/rounds/round-225/merge.md
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
     orchestrator/rounds/round-225/selection.md \
     orchestrator/rounds/round-225/plan.md \
     orchestrator/rounds/round-225/implementation-notes.md \
     orchestrator/rounds/round-225/review.md \
     orchestrator/rounds/round-225/review-record.json \
     orchestrator/rounds/round-225/reviews/attempt-1.md \
     orchestrator/rounds/round-225/merge.md
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
   git commit -m "Freeze ADT/case semantic ownership and keep layout policy private"
   ```

## Follow-Up Notes

- Preserve the lineage fields above unchanged in controller closeout.
- Keep later summaries honest about scope: this merge settles only the
  milestone-4 ADT/case semantic-versus-layout ownership freeze and
  mechanism-table row 4.
- Do not describe this merge as authorizing a second backend IR, a public
  lowering surface, lazy runtime machinery, milestone-5 primitive/evaluation
  order closure, milestone-6 polymorphism lowerability closure, or
  milestone-7 family closeout.
