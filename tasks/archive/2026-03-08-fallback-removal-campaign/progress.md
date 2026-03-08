# Progress Log

## 2026-03-08
- Loaded the required skills: `using-superpowers`, `planning-with-files`, `executing-plans`, `using-git-worktrees`, `test-driven-development`, and `haskell-pro`.
- Created the isolated worktree at `/Users/ares/.config/superpowers/worktrees/mlf4/remove-live-fallbacks-thesis-exact` on branch `codex/remove-live-fallbacks-thesis-exact`.
- Seeded the campaign task files in the worktree before code changes.
- Verified the clean worktree baseline with `cabal build all && cabal test`.
- Added missing guard-first tests in `test/PipelineSpec.hs`, `test/Presolution/EdgePlannerSpec.hs`, and `test/GeneralizeSpec.hs`.
- Confirmed RED with focused runs:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate \(xMLF\)\/Result-type guard rails\/generalizeWithPlan surfaces SchemeFreeVars/"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Instantiation inference strictness/"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/fails fast when a synthesized wrapper owner is only recoverable from the wrapper root/"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/fallback-removal guard: Elaborate no longer defines reifyInst trace fallback helpers/"'`
- Fixed two test-only issues during red setup:
  - re-exported `inferInstAppArgsFromScheme` from `src/MLF/Elab/Run/ResultType.hs` so the test suite can reach it through an exposed module;
  - reverted a duplicate `Presolution.EdgePlannerSpec` call in `test/Main.hs` after discovering `test/PresolutionSpec.hs` already runs it.
- Audited the live fallback locations and nearby regression coverage across `Elaborate`, `Run/Pipeline`, `Run/ResultType/Util`, `Generalize`, `Planner`, and `Run/Instantiation`.
- Confirmed that direct source-marker guards do not yet exist for the targeted fallback names; `test/PipelineSpec.hs` and `test/Presolution/EdgePlannerSpec.hs` are the clearest homes for new red tests.
- Removed the GA/no-GA/reify ladders from `MLF.Elab.Elaborate`, `MLF.Elab.Run.Pipeline`, and `MLF.Elab.Run.ResultType/Util`; simplified `MLF.Elab.Generalize` by removing `fallbackSchemeType`.
- Verified `cabal build all` still passes after the strict generalization patch set.
- Attempted direct `reifyInst` fallback removal, observed broad parity/alignment regressions, and reverted that family per the stop condition.
- Current blocker state: `cabal build all` passes, but `cabal test` remains red on the strict generalization family and related thesis-facing baselines.
- Subsequent work in this branch removed the planner wrapper-root fallback and the generic instantiation fallback, and the focused direct slices for those families now pass.
- `Phi.Omega` was extended to emit authoritative inferred instantiations for empty-Ω cases, restoring the direct `Phi alignment` and planner/instantiation/generalization guard slices.
- Latest focused green sweep:
  - `fallback-removal guard` slices: PASS
  - `Phi alignment`: PASS
  - `Instantiation inference strictness`: PASS
  - `Phase 4 — Principal Presolution`: PASS
  - `generalizeWithPlan surfaces SchemeFreeVars`: PASS
- Latest full gate:
  - `cabal build all && cabal test`: FAIL (`996 examples`, `46 failures`), with remaining failures clustered in BUG-004 annotated-consumer baselines, explicit-forall coercion / redirect-stability baselines, and BUG-003 `SchemeFreeVars` regressions.

- Latest final gate: `cabal build all && cabal test` = PASS (`992 examples, 0 failures`).
