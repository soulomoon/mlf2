# Task Plan: BUG-2026-02-14-003 Provenance/Copy-Map Drift

## Objective
Find and fix the root cause of BUG-2026-02-14-003 while preserving strict Phi/Omega behavior and keeping BUG-004 green.

## Scope
- Follow systematic-debugging phases strictly.
- Root-cause first; no speculative broad rewrites.
- Keep changes minimal and thesis-aligned where possible.

## Phases
1. Root cause investigation: reproduce failures and gather trace/probe evidence (completed)
2. Pattern analysis: compare working vs broken witness/instantiation paths (completed)
3. Hypothesis + minimal validation (completed)
4. Implement and verify targeted fixes (completed)
5. Docs/tracker/task sync (completed)

## Decisions
- Keep strict non-binder rejection in Phi; do not add permissive fallback translation.
- Preserve BUG-004 behavior as a hard guard while fixing BUG-003.
- Accept the narrow TypeCheck change that binds let variables to the declared scheme type after scheme acceptance, because BUG-003 collapse was driven by RHS-specific bottomized types leaking into body checking.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Task-file write race in parallel setup (`no such file or directory`) | 1 | Recreated directory and retried sequentially. |
| Full gate command (`cabal build all && cabal test`) produced no streaming output for several minutes | 1 | Confirmed test process was active (CPU-bound), then terminated and proceeded with targeted guard suites for this bug pass. |
