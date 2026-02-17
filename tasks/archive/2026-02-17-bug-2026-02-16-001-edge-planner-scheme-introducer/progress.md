# Progress Log: BUG-2026-02-16-001

## 2026-02-17
- Reproduced both open planner crashes with deterministic seeds:
  - let-edge flag test fails with `scheme introducer not found for NodeId 0`.
  - ann-edge flag test fails with the same error.
- Located throw site and caller chain:
  - throw: `src/MLF/Constraint/Presolution/StateAccess.hs`
  - caller: `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`
- Verified full-suite state includes these failures (and others unrelated to this bug).
- Performed targeted data-path probe with temporary `runghc` script:
  - TyExp body path lacks a gen ancestor in failing fixture topology,
  - TyExp wrapper path has gen ancestor,
  - confirms body-root lookup is the immediate failure trigger.
- Checked regression origin with blame:
  - lookup moved into planner in commit `a8e5781`.
- Implemented minimal fix in planner:
  - added synthesized-wrapper scheme-owner fallback (body path then wrapper path).
  - kept non-synth path strict via existing `findSchemeIntroducerM` contract.
- Strengthened bug repro tests:
  - `EdgePlannerSpec` let/ann classification cases now also assert `eprSchemeOwnerGen == GenNodeId 0`.
- Verification:
  - PASS `--match "/Edge plan types/planner classification/threads let-edge flag into allowTrivial/" --seed 1481579064`
  - PASS `--match "/Edge plan types/planner classification/threads ann-edge flag into suppressWeaken/" --seed 1481579064`
  - PASS `--match "Edge plan types" --seed 1481579064` (`7 examples, 0 failures`)
  - PASS `--match "Edge interpreter" --seed 1481579064` (`4 examples, 0 failures`)
