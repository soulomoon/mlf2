# Task Plan: Fallback Removal Audit

## Metadata
- Date: 2026-03-08
- Execution mode: audit-first
- Skills in use: using-superpowers, planning-with-files, haskell-pro

## Goal
Audit the remaining live fallback mechanisms and determine which, if any, can still be removed to make the codebase more thesis-exact.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Initialize audit context | completed | Task folder created and high-signal fallback sites captured |
| 2. Inspect live fallback mechanisms | completed | Reviewed ResultType, Generalize, Elaborate, Instantiation, Binding validation, and docs/deviation notes |
| 3. Classify thesis-exact candidates | completed | Distinguished strong removal candidates from thesis-faithful or naming-only cases |
| 4. Summarize recommendations | completed | Ranked the remaining fallback-removal opportunities |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## Conclusion
- There are still meaningful fallback-removal opportunities.
- The highest-value remaining target is the GA -> no-GA -> reify fallback ladder family across Elaborate, Pipeline, ResultType.Util, and Generalize.
- `checkNoGenFallback` is not a removal candidate; it is an explicit thesis-faithful guard.
