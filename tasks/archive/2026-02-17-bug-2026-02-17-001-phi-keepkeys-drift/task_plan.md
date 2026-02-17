# Task Plan: BUG-2026-02-17-001 Phi Keep-Key Drift

## Objective
Restore thesis-aligned Φ keep-key behavior so `OpWeaken` elimination is not suppressed when target binder coverage is empty, fixing paper baseline regressions (identity and annotation instantiation paths).

## Scope
- Apply `systematic-debugging` end-to-end for deterministic full-gate regressions.
- Keep changes surgical to Φ target binder keep-key selection.
- Validate against failing thesis anchors and relevant BUG matrix slices.

## Phases
1. Root cause investigation: reproduce failing baselines and capture Φ trace evidence. (completed)
2. Pattern analysis: compare current keep-key behavior vs prior intersection semantics. (completed)
3. Hypothesis test: minimal keep-key narrowing patch + targeted tests. (completed)
4. Implementation + regression verification. (completed)
5. Tracker/docs sync (`Bugs.md`, TODO, task files). (completed)

## Decisions
- Target the first deterministic failure root (`id y` bottomization) before broader full-gate rerun.
- Prefer reverting to strict intersection semantics for keep-keys when no target binders are present.
- Keep non-var fallback disabled in generic `reifyInst`; instead apply annotation-bound fallback locally in `AAnnF` for non-variable source annotations to avoid BUG-003 regressions.
- Collapse unbounded `OpGraft -> OpRaise -> OpWeaken` triples on the same binder into direct `InstApp` in Ω translation.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Parallel cabal test invocations collided on `package.conf.inplace` lock | 5 | Reran repro commands sequentially. |
| Keep-key narrowing exposed `prefixBinderNames` identity-length mismatch in `OpRaise` spine path | 1 | Fixed spine alias/eliminate behavior (`hAbsBeta` preserved under empty context) and continued investigation. |
| Global non-var fallback in `reifyInst` regressed BUG-003 (`InstBot expects TBottom` on bounded-alias variants) | 1 | Reverted global fallback gate; moved fallback behavior into `AAnnF` non-variable annotation path only. |

## Closure Update (2026-02-17)
- Closed the residual 3-failure bucket after Task 17 target fixes:
  - added strict fail-fast invariant in `resolveTraceBinderTarget` for missing replay binder candidates on trace-source binder ops;
  - restored non-spine `OpRaise` context-path translation for non-`⊥` bounds when `C^m_n` exists;
  - updated `PipelineSpec` canonicalization sentinel to avoid vacuous union-find assumptions.
- Final verification:
  - `cabal build all && cabal test` => PASS (`678 examples, 0 failures`).
- Task outcome:
  - BUG-2026-02-17-001 and linked residual buckets are now tracker-closed.
