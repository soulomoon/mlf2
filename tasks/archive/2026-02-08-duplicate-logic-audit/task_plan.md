# Task Plan: Duplicate Logic Audit for Better Abstractions

## Goal
Identify duplicated logic patterns across the mlf4 Haskell codebase and recommend concrete abstraction opportunities with file/function ownership and practical refactor direction.

## Scope
- Code scan across `src/`, `src-public/`, `app/`, and `test/`
- Focus on meaningful logic duplication (not trivial boilerplate)
- Provide prioritized candidates with risk/benefit notes

## Current Phase
Phase 4

## Phases

### Phase 1: Discovery & Scope Confirmation
- [x] Confirm user request and constraints
- [x] Initialize planning files in task folder
- [x] Record baseline codebase context
- **Status:** complete

### Phase 2: Duplicate Pattern Analysis
- [x] Search for repeated logic across modules
- [x] Validate patterns and identify ownership
- [x] Draft abstraction options per pattern
- **Status:** complete

### Phase 3: Recommendation Synthesis
- [x] Prioritize opportunities by impact/risk
- [x] Produce concrete refactor recommendations
- [x] Capture follow-up options for implementation
- **Status:** complete

### Phase 4: Verification & Delivery
- [x] Sanity-check references and claims
- [x] Update findings/progress logs
- [x] Deliver concise audit report to user
- **Status:** complete

## Decisions Made
| Decision | Rationale |
|----------|-----------|
| Use file-based planning in `tasks/todo/2026-02-08-duplicate-logic-audit/` | Required by repository AGENTS workflow for multi-step autonomous work |
| Prioritize binding/path and scope-wiring duplicates first | They are core semantic plumbing and appear in multiple modules with drift risk |
| Treat test harness duplication as second-tier cleanup | High maintainability payoff but lower semantic risk than core constraint/elab code |

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| `session-catchup.py` command failed with `${CLAUDE_PLUGIN_ROOT}` expansion parse error | 1 | Re-ran with explicit path under `/Users/ares/.codex/skills/planning-with-files/scripts/` |
