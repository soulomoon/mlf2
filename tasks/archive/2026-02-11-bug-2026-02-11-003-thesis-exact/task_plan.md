# Task Plan: BUG-2026-02-11-003 thesis-exact follow-up

## Context
- Source plan: `docs/plans/2026-02-11-bug-2026-02-11-003-thesis-exact-implementation-plan.md`
- Branch/worktree: `codex/bug003-thesis-exact`
- Goal: Remove non-thesis fallback behavior and restore BUG-004 V2/V4 via paper-faithful producer-side derivation.

## Phases
- [x] Task 0: Preflight in dedicated worktree
- [x] Task 1: Strict-theory regression harness (red first)
- [x] Task 2: Strict policy API
- [x] Task 3: Strict cutover (remove runtime compat)
- [x] Task 4: Remove reify fallback synthesis
- [x] Task 5: Thesis-backed producer fix for V2/V4
- [x] Task 6: Remove compat API + lock strict contract
- [x] Task 7: Docs/bug tracker/full verification
- [x] Task 8: Final audit checklist

## Error Log
- 2026-02-11: Initial targeted test run used over-quoted `--match` and matched 0 tests; reran with corrected quoting.
- 2026-02-11: Parallelized `cabal test` ephemeral invocations caused `package.cache` race; reran serially and captured expected red/pending results.
- 2026-02-11: `--test-options` pattern containing spaces (`strict InstBot`) was split by the test runner; reran with a space-safe `--match strict`.
- 2026-02-11: Repeated test-target parallelism on `mlf2-test` caused `package.conf.inplace` contention; reran BUG-004 slices serially.
- 2026-02-11: Introduced a local parse error in `AApp` (`let`/`in` omission around `argInstFromFun`); fixed and re-ran focused tests.
- 2026-02-12: Additional parallel targeted runs hit transient `package.conf.inplace` / `package.cache.lock` contention; reran all verification slices serially.
- 2026-02-12: Full-gate first pass surfaced 3 regressions after strict cutover follow-ups (duplicate top-level forall closure and stale BUG-003 sentinel error substring); fixed via source-scheme substitution-only closure for `InstId` annotation paths and updated BUG-003 sentinel expectations.
