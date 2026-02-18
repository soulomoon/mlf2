# Task Plan: A5 (P3) Totality and Harness Hardening

## Objective
Close `A5 (P3)` by removing remaining frontend coercion-copy totality/footgun paths and adding a hard harness guard so presolution umbrella wiring omissions fail loudly.

## Scope
- Frontend constraint generation hardening around coercion-copy handling (`STCon` path + bare coercion constant path).
- Test harness wiring hardening for `PresolutionSpec` umbrella execution guarantees.
- Verification and tracker/doc closure for `TODO.md` A5 items.

## Plan Artifact
- Detailed implementation plan: `docs/plans/2026-02-18-a5-totality-harness-hardening-implementation-plan.md`

## Phases
1. Baseline + RED tests for coercion-copy typed failures. (completed)
2. Typed error surface + STCon traversal totalization. (completed)
3. Presolution umbrella single-source wiring. (completed)
4. Harness fail-fast guard for umbrella omission. (completed)
5. Full verification + docs/tracker sync + task closure. (completed)

## Decisions
- Use explicit typed failure constructors instead of stringly internal errors for coercion-copy footguns.
- Keep presolution tests reachable through a single umbrella module and add a deterministic guard in the harness.
- Run full gate before marking A5 checklist items complete.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Subagent edited `/Volumes/src/mlf4` instead of the isolated worktree on first Task 1 pass | 1 | Continued in `/Volumes/src/mlf4` for consistency and re-ran review/fix loop there. |
| Parallel Cabal focused runs hit transient package cache lock/path removal error | 1 | Re-ran affected test commands sequentially; all targeted checks passed. |

## 2026-02-18 closure
- A5 acceptance criteria implemented and verified.
- Full gate status: `cabal build all && cabal test` (PASS).
