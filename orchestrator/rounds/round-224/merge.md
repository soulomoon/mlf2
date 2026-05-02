# Merge Preparation (`round-224` / `milestone-3` / `direction-3a`)

## Lineage

- `roadmap_id`: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- `roadmap_item`: `milestone-3` / `direction-3a-clarify-direct-vs-closure-callable-shapes`
- `extracted_item_id`: `null` (`absent` in round wording)
- `base_branch`: `master`

## Squash Commit Title

`Clarify direct-vs-closure callable shapes at the backend IR boundary`

## Squash Summary

- Merge the approved `milestone-3` /
  `direction-3a-clarify-direct-vs-closure-callable-shapes` slice for the
  active backend-IR executable-boundary roadmap.
- The approved outcome makes `BackendApp` the direct first-order call path,
  keeps `BackendClosureCall` as the explicit indirect closure path, publishes
  the shared callable-head classifier in `MLF.Backend.IR`, makes conversion
  and LLVM lowering consume that classifier, rejects malformed closure-headed
  `BackendApp` forms explicitly, and flips only mechanism-table row 3 to
  `YES`.
- No controller-state, pointer-stub, or active-roadmap-bundle edit belongs to
  the squash substance.

## Payload

- Repo-facing payload files:
  `docs/architecture.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `test/BackendConvertSpec.hs`,
  `test/BackendIRSpec.hs`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`.
- Round-owned payload artifacts:
  `orchestrator/rounds/round-224/selection.md`,
  `orchestrator/rounds/round-224/plan.md`,
  `orchestrator/rounds/round-224/implementation-notes.md`,
  `orchestrator/rounds/round-224/review.md`,
  `orchestrator/rounds/round-224/review-record.json`,
  `orchestrator/rounds/round-224/reviews/attempt-1.md`, and
  `orchestrator/rounds/round-224/merge.md`.

## Exclusions

- Controller-owned files to leave untouched and unstaged:
  `orchestrator/state.json`,
  `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, and
  `orchestrator/retry-subloop.md`.
- Active roadmap bundle to exclude from this squash unless controller closeout
  later requires it:
  `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/**`.
- Do not stage any other untracked controller artifacts under
  `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/`
  beyond the explicit round-owned files listed in `Payload`.

## Verification Summary

- `orchestrator/rounds/round-224/review.md` records
  `Implemented stage result: accepted`,
  `Attempt verdict: accepted`, and
  `Stage action: finalize`.
- The latest review snapshot is
  `orchestrator/rounds/round-224/reviews/attempt-1.md`; it records the same
  lawful `accepted + finalize` result.
- `orchestrator/rounds/round-224/review-record.json` matches the same lineage
  and records `decision: approved`, `merge_readiness: satisfied`, and
  `controller_merge_hygiene: satisfied`.
- Base-branch freshness is satisfied: `HEAD`, `master`, and
  `git merge-base HEAD master` all resolve to
  `006eb569ced40de734ec2fcecf76b40f91fc7f74`, and
  `git rev-list --left-right --count master...HEAD` reports `0 0`.
- `git diff --check` passed.
- Focused callable-shape verification passed:
  repo guard `1 example, 0 failures`;
  focused `BackendIR`, `BackendConvert`, and `BackendLLVM` callable-shape
  slices all passed;
  the mechanism-table gate reports rows 1-3 = `YES` and rows 4-7 = `NO`.
- Full required gate passed:
  `cabal build all && cabal test` reported `2340 examples, 0 failures`.

## Predecessor Continuity

- Accepted `round-222`, merged as `5365d975`, remains the authoritative
  milestone-1 one-backend-IR contract freeze.
- Accepted `round-223`, merged as `006eb569`, remains the authoritative
  milestone-2 eager-runtime lowering contract freeze.
- This round extends that same family by flipping only mechanism-table row 3
  to `YES`; rows 4 through 7 remain `NO`, so the next live blocker stays
  `milestone-4` / row 4 ADT/case-versus-layout work.
- The inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback predecessor boundary remains unchanged
  background evidence only; this round does not reopen that settled context.

## Residual Risks

- `milestone-4` through `milestone-7` remain intentionally open, and
  mechanism-table rows 4 through 7 still stay `NO`.
- The shared callable classifier is intentionally conservative for ambiguous
  callable heads and rejects them instead of widening the row-3 contract.

## Merge Readiness

- Latest review snapshot: confirmed `accepted + finalize`.
- Merge readiness: confirmed for the approved bounded payload listed above.
- `round-224` is ready for squash merge, provided staging stays limited to the
  approved payload and continues to exclude controller-owned bookkeeping.

## Controller Merge Instructions

1. From the parent workspace root, stay on `master` and create only the
   round-owned destination directory:

   ```bash
   cd /Volumes/src/mlf4
   git checkout master
   mkdir -p orchestrator/rounds/round-224/reviews
   ```

2. Copy only the approved payload from the canonical round worktree:

   ```bash
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/docs/architecture.md docs/architecture.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/src/MLF/Backend/Convert.hs src/MLF/Backend/Convert.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/src/MLF/Backend/IR.hs src/MLF/Backend/IR.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/src/MLF/Backend/LLVM/Lower.hs src/MLF/Backend/LLVM/Lower.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/test/BackendConvertSpec.hs test/BackendConvertSpec.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/test/BackendIRSpec.hs test/BackendIRSpec.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/test/BackendLLVMSpec.hs test/BackendLLVMSpec.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/test/RepoGuardSpec.hs test/RepoGuardSpec.hs
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/orchestrator/rounds/round-224/selection.md orchestrator/rounds/round-224/selection.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/orchestrator/rounds/round-224/plan.md orchestrator/rounds/round-224/plan.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/orchestrator/rounds/round-224/implementation-notes.md orchestrator/rounds/round-224/implementation-notes.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/orchestrator/rounds/round-224/review.md orchestrator/rounds/round-224/review.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/orchestrator/rounds/round-224/review-record.json orchestrator/rounds/round-224/review-record.json
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/orchestrator/rounds/round-224/reviews/attempt-1.md orchestrator/rounds/round-224/reviews/attempt-1.md
   cp /Volumes/src/mlf4/orchestrator/worktrees/round-224/orchestrator/rounds/round-224/merge.md orchestrator/rounds/round-224/merge.md
   ```

3. Stage only those copied files and nothing controller-owned:

   ```bash
   git add -- \
     docs/architecture.md \
     docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md \
     src/MLF/Backend/Convert.hs \
     src/MLF/Backend/IR.hs \
     src/MLF/Backend/LLVM/Lower.hs \
     test/BackendConvertSpec.hs \
     test/BackendIRSpec.hs \
     test/BackendLLVMSpec.hs \
     test/RepoGuardSpec.hs \
     orchestrator/rounds/round-224/selection.md \
     orchestrator/rounds/round-224/plan.md \
     orchestrator/rounds/round-224/implementation-notes.md \
     orchestrator/rounds/round-224/review.md \
     orchestrator/rounds/round-224/review-record.json \
     orchestrator/rounds/round-224/reviews/attempt-1.md \
     orchestrator/rounds/round-224/merge.md
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

   The first command should show only the 16 approved payload files. The
   second command should print nothing.

5. Create the squash commit with the approved title:

   ```bash
   git commit -m "Clarify direct-vs-closure callable shapes at the backend IR boundary"
   ```

## Follow-Up Notes

- Preserve the lineage fields above unchanged in controller closeout.
- Keep later summaries honest about scope: this merge settles only the
  milestone-3 callable-shape contract and mechanism-table row 3.
- Do not describe this merge as authorizing a second backend IR, a public
  lowering surface, lazy runtime machinery, or any reopening of the inherited
  predecessor settlement.
