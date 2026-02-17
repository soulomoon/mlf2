# Task Plan: BUG-2026-02-14-002 OpRaise Witness/Replay Regression

## Objective
Find and fix the root cause of BUG-2026-02-14-002 where OpRaise emission/replay for interior nodes diverges from expected behavior.

## Scope
- Follow systematic-debugging phases strictly.
- Keep fix minimal and focused on BUG-002.
- Preserve thesis-aligned strict semantics.

## Phases
1. Root cause investigation: reproduce failures and trace data flow (completed)
2. Pattern analysis: compare passing vs failing OpRaise paths (completed)
3. Hypothesis + minimal validation (completed)
4. TDD implementation + verification (completed)
5. Docs/tracker/task sync (completed)

## Decisions
- Use RaiseSpec failing anchors as primary reproduction.
- Delay code edits until root cause is evidenced.
- Investigate `recordRaisesFromTrace` first because it is the direct OpRaise emission gate.
- Keep `runEdgeUnifyForTest` behavior unrestricted for binder-free test harness scenarios.
- Preserve strict Φ failure semantics by fixing witness emission/normalization inputs instead of adding Φ fallback acceptance.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Task-file write race in parallel setup (`no such file or directory`) | 1 | Recreated directory and retried sequentially. |
| Cabal cache/lock race when running test commands in parallel | 1 | Run cabal test matchers sequentially (single process). |
| Compile error (`lookupBindParent` not in scope) | 1 | Switched to `Binding.lookupBindParent`. |
| Cabal object rename race from parallel builds | 1 | Reran failing matcher after previous cabal process exited. |
