# Task Plan: Thesis-Exact Phi Upfront Normalization (No Runtime Fallback Search)

## Goal
Implement `docs/plans/2026-02-27-thesis-exact-upfront-normalization-no-runtime-fallback-search-plan.md` end-to-end, including strict replay-map contract production/validation, runtime fallback removal in Phi/Omega, strict no-trace behavior, tests, and docs.

## Phases
| Phase | Status | Notes |
|---|---|---|
| Gate 1 schema cutover (`etBinderReplayHints` -> `etBinderReplayMap`) | completed | `EdgeTrace` contract switched and all callsites/tests migrated. |
| Gate 2 deterministic producer replay-map + validation errors | completed | Deterministic replay-map production + `ReplayMapIncomplete`/`ReplayMapNonTyVarTarget`/`ReplayMapNonInjective` enforcement landed. |
| Gate 3 remove runtime replay-map synthesis in Translate | completed | `computeTraceBinderReplayBridge` now validates/bridges strict trace map, using deterministic scheme-domain alignment; no hint/alias runtime synthesis helpers remain. |
| Gate 4 remove fallback search in Omega + IdentityBridge | completed | Removed class-member runtime recovery path; non-root binder recovery now replay-map/source-alias based and fail-fast. |
| Gate 5 strict no-trace path + trace-backed test helper migration | completed | `phiFromEdgeWitnessNoTrace` is strict fail-fast (`MissingEdgeTrace`); positive-path tests use trace-backed helpers. |
| Gate 6 producer-boundary checks + docs updates | completed | Driver boundary checks + docs/task artifacts updated. |
| Verification (`cabal build all && cabal test`) | completed | Full repo gate green. |

## Decisions
- Follow user-provided plan exactly unless blocked by compile/test constraints.
- Keep runtime failure mode fail-fast with deterministic diagnostics.
- Preserve replay targets in replay raw-ID space and align bridge domain to actual scheme quantifier IDs.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| `runghc` package-id resolution failure for local debug harness | 1 | Switched to `cabal repl mlf2-test` script execution instead of standalone `runghc`. |
| `cabal run /tmp/debug_a6_script.hs` could not resolve `mlf2` package | 1 | Dropped cabal-script approach; used test-target repl with project modules loaded. |
| Hidden-module + import ambiguity in debug harness | 2 | Rewrote debug harness with qualified imports and only exported trace config from `MLF.Elab.Pipeline`. |
| Full suite initially left 2 alias-fallback expectation failures | 1 | Updated `ElaborationSpec` expectations to strict fail-fast behavior matching plan. |
