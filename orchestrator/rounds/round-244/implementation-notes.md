### Changes Made
- `src/MLF/Constraint/Finalize.hs`: kept the production Snapshot Finalization facade focused on finalized view/solved construction entrypoints and delegated private mechanics to the new internal module.
- `src/MLF/Constraint/Finalize/Internal.hs`: added owner-local snapshot sanitizing, canonicalization, solved bind-parent pruning, strict solved validation, and solved-to-view record construction mechanics.
- `src/MLF/Constraint/Finalize/TestSupport.hs`: added the test-support seam for solved-to-view fixtures and the prune helper previously exported by the production facade.
- `src/MLF/Constraint/Presolution/View.hs`: added `sanitizedViewCanonicalMap` as the narrow read-model projection needed by generalization planning.
- `src/MLF/Constraint/Presolution/Plan/Env.hs`: moved canonical-map sanitizing off the production `Finalize` step helper and onto the read-model accessor.
- `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, `test/Reify/CoreSpec.hs`, `test/Reify/TypeSpec.hs`, `test/Research/C1AuthoritativeSurfaceSpec.hs`, `test/Research/P5ClearBoundarySpec.hs`: routed solved-to-view fixture and prune-helper use through `MLF.Constraint.Finalize.TestSupport`.
- `test/PresolutionFacadeSpec.hs`: added guards that `MLF.Constraint.Finalize` no longer exposes stepwise or fixture helpers and that solved-to-view fixture mechanics live behind `Finalize.TestSupport`.
- `test/RepoGuardSpec.hs`: added guard coverage forbidding production imports of `Finalize.TestSupport` and restricting `Finalize.Internal` imports to `Finalize` plus `Finalize.TestSupport`.
- `mlf2.cabal`: registered `MLF.Constraint.Finalize.TestSupport` as an internal-library exposed test-support module and `MLF.Constraint.Finalize.Internal` as an implementation module.
- `docs/architecture.md`: updated the solved/finalization ownership table for the production/internal/test-support split.
- `AGENTS.md`: synchronized the subagent model rule with the current repo instruction (`gpt-5.5`, `xhigh`).

### Tests
- `test/PresolutionFacadeSpec.hs`: verifies the production `Finalize` facade excludes stepwise helpers and solved-to-view fixture adapters while `Finalize.TestSupport` owns the fixture seam.
- `test/RepoGuardSpec.hs`: verifies Snapshot Finalization internals stay behind the approved facade/test-support imports.
- `test/Constraint/SolvedSpec.hs`: existing solved/read-model parity and facade guards remain green.

### Notes
- Validation run:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'` passed: 10 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Solved"'` passed: 51 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'` passed: 29 examples, 0 failures.
  - `git diff --check` passed.
  - `cabal build all && cabal test` passed: full suite 2586 examples, 0 failures.
- A parallel focused-test attempt hit a `dist-newstyle` package-conf filesystem race and a literal `Repo guard` matcher selected zero examples; both were rerun sequentially with the exact matchers above.
- The full gate rewrote the generated `runtime/mlfp_io/target/release/libmlfp_io.d` depfile with worktree-local paths; it was restored to the pre-run tracked content.
