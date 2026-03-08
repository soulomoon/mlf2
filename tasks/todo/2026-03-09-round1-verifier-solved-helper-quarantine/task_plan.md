# Task Plan — Round 1 verifier: solved helper quarantine

## Goal
Decide whether removing the duplicate solved test-helper bundle from `MLF.Constraint.Solved.Internal` is still needed, bounded, worthwhile, and thesis-safe.

## Phases
- [in_progress] Set up task context and collect evidence
- [pending] Inspect thesis, code, and project docs
- [pending] Evaluate scope, risks, and current need
- [pending] Emit YES/NO gate with supporting evidence

## Decisions
- Use `papers/these-finale-english.txt` as the primary source of truth; consult `papers/xmlf.txt` only if the thesis is silent.
- Treat this as a verification/research task; no code changes unless guidance files prove stale and require synchronization.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Hspec `--match` with alternation produced `0 examples, 0 failures` | 1 | Treat as a pattern mismatch, not a failing boundary check; rerun with an exact example name if needed. |
