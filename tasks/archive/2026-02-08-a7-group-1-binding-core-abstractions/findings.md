# Findings & Decisions

## Requirements
- Use `executing-plans` and `subagent-driven-development` workflows.
- Execute plan file exactly with checkpointed batches.
- Keep implementation paper-faithful and behavior-preserving.

## Initial Findings
- Target plan defines 4 tasks; Batch 1 should cover Tasks 1-3.
- Repository is already in isolated worktree on branch `codex/a7-binding-core-abstractions`.
- Existing `tasks/todo/` workflow is active in this repository; this run must keep task logs updated.

## Technical Decisions
| Decision | Rationale |
|----------|-----------|
| Dispatch fresh implementer subagent per task | Required by subagent-driven-development |
| Run spec and quality review gates per task | Required by subagent-driven-development |
| Accept Task 1 quality review with minor-only issue | Reviewer approved; no blocking correctness or spec-compliance defects |
| Treat Task 2 as complete after reviewer re-check confirmed compile behavior | Initial missing-import claim contradicted successful build and toolchain behavior |
| Keep `BindingUtil` error-conversion boundary local while delegating ancestor discovery to shared path helper | Matches Task 3 requirement and avoids cross-module error-type coupling |
| Land direct helper-coverage tests as post-checkpoint feedback change | Addresses residual review gap on NodeRefs/Children/buildTypeEdgesFrom coverage |

## Issues Encountered
| Issue | Resolution |
|-------|------------|
| session-catchup script unavailable in this environment | Noted in session-catchup.txt; proceeding |
| Task 2 plan snippet omits `MLF.Binding.Children` sample body | Will extract equivalent logic from existing modules during Task 2 implementation |
| Task 2 reviewers repeatedly reported missing `foldl'` import | Verified by GHCI + passing test suite, then obtained updated approval |
| Task 3 regression test did not go red after insertion | Acceptable for abstraction-only migration; serves as characterization guardrail |
| User feedback after Batch 1 required additional direct helper specs | Implemented in `BindingSharedAbstractionSpec` and re-verified before Task 4 |
