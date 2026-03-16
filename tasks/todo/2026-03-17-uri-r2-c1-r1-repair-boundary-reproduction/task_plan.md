# Task Plan

## Summary
Goal: implement round-024 `R1` attempt-1 by producing a bounded, reviewer-auditable reproduction contract for `BUG-2026-03-16-001` at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch), without repairing runtime behavior.

## Phases
1. Re-read round/repair-boundary authority and predecessor evidence, then restate the locked scope. - complete
2. Build a focused implementation-facing reproducer in tests for `URI-R2-C1` / `uri-r2-c1-only-v1`. - complete
3. Write the `R1` contract artifact and round implementation notes, then run bounded validation. - complete

## Errors Encountered
| Error | Attempt | Resolution |
|------|---------|------------|
| Helper used `annNode`, which is not in scope in `test/ElaborationSpec.hs` | 1 | Switched the reproducer helper to the existing local `annExprNode` helper and reran the targeted test. |
