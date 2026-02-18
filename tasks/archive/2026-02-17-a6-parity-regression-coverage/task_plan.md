# Task Plan: A6 (P2) Parity and Regression Coverage

## Objective
Audit and expand test coverage for checked-vs-unchecked parity and bounded/coercion-heavy regressions across:
- `test/ElaborationSpec.hs`
- `test/PipelineSpec.hs`
- `test/TypeCheckSpec.hs`
- `test/ReduceSpec.hs`

## Scope
- Confirm whether requested parity/regression cases already exist.
- Add missing test cases with deterministic expectations.
- Keep tests thesis-faithful and aligned with existing strictness contracts.

## Phases
1. Coverage audit of the four target spec modules. (in progress)
2. Gap analysis and test-case design for missing scenarios. (pending)
3. Implement missing tests in target modules. (pending)
4. Run focused and suite-level verification. (pending)
5. Update tracker docs/task logs and summarize outcomes. (pending)

## Decisions
- Prefer extending existing describe blocks over introducing new parallel structures.
- Keep repro expressions minimal but coercion/bounds-heavy where requested.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## 2026-02-17 execution update
- Phase 1 (coverage audit): completed.
- Phase 2 (gap analysis/test design): completed.
- Phase 3 (test implementation): completed.
- Phase 4 (verification): completed.
- Phase 5 (tracker/doc sync + summary): in progress.

### Errors Encountered (session)
| Error | Attempt | Resolution |
|---|---:|---|
| New bounded/coercion expression variant (`let c = ... in (c : ann) 1 2`) failed unchecked pipeline with `TCLetTypeMismatch` | 1 | Switched bounded parity fixture to known-green bounded/coercion shape already used in baseline matrix (`let c = (rhs : scheme) in (c : ann)`), retaining monomorphic coercion-heavy parity as a separate regression case. |

## 2026-02-17 closure update
- Phase 5 (tracker/doc sync + summary): completed.
- All phases complete; task is ready for archive.
