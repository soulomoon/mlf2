# Task Plan — 2026-03-01 Pod A Wave 1

## Goal
Remove `Solved.canonical`-driven runtime identity decisions in Phi translation path and enforce strict trace/replay-domain keyed behavior without fallback policy.

## Scope (Owned Files Only)
- src/MLF/Elab/Phi/Translate.hs
- src/MLF/Elab/Phi/Omega.hs
- src/MLF/Elab/Phi/IdentityBridge.hs
- test/Phi/IdentityBridgeSpec.hs
- test/Phi/AlignmentSpec.hs
- test/ElaborationSpec.hs

## Phases
1. [completed] Baseline scan + initialize notes
2. [completed] Add/adjust tests (RED)
3. [completed] Implement strict trace-domain behavior (GREEN)
4. [completed] Verify targeted tests + required checks
5. [in_progress] Commit and summarize

## Decisions
- Preserve existing fail-fast behavior for unresolved source/replay mismatch.
- Keep IdentityBridge as adapter for witness-domain key derivation/ranking only.

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-01 | `tasks/todo/2026-03-01-tmt3-thesis-exact` missing in worktree | 1 | Created folder and initialized plan/findings/progress files |
| 2026-03-01 | `IdentityBridgeSpec` expected canonical-collapsed key behavior after strict source-key refactor | 1 | Updated spec expectation to raw witness key behavior |
