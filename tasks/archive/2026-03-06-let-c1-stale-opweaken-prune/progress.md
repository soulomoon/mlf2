# Progress

- Initialized follow-up task for stale OpWeaken pruning in recovery worktree.
- Focused RED->GREEN complete for let-c1-apply-bool; lane-aware stale OpWeaken prune implemented in WitnessNorm.
- Verified the initial rule was over-broad by comparing against a clean `4211bb6` worktree and tracing both `let-c1-apply-bool` and `BUG-002-V4`.
- Refined `WitnessNorm` to preserve abstract under-lambda weaken paths while pruning concretized stale residue.
- Updated `PipelineSpec.hs` BUG-002 sentinel/strict-target matrix expectations from stale fail-fast assertions to the current checked success types, and removed the now-unused `strictReplayFailFast` helper.
- Verification commands run:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "bottom-int arrow"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "let-c1-apply-bool"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004"'`
  - `cabal build all && cabal test`
