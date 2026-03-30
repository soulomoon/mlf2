# Findings

## Initial

- Review packet created on 2026-03-29 for the question of automatic iso-recursion inference completeness.

## Sweep 1

- `CHANGELOG.md` and `TODO.md` both claim automatic iso-recursive type inference completed on 2026-03-29, including cycle detection, `TyMu` introduction, `TMu` reification, `ERoll`/`EUnroll` elaboration, and Phase 7 recursive-type checking/reduction.
- The worktree is dirty in live `orchestrator/` files related to the same roadmap family, so authoritative status cannot be inferred from git cleanliness alone.
- Historical roadmap/task materials repeatedly distinguish bounded or incomplete unannotated recursive inference from the broader completion claim, so implementation must be checked directly rather than trusting summaries.

## Sweep 2

- Positive implementation path exists in production code:
  - Phase 3 rewrites cycles into `TyMu` via `breakCyclesAndCheckAcyclicity` in `src/MLF/Constraint/Acyclicity.hs`.
  - Reification translates solved `TyMu` nodes into `TMu` in `src/MLF/Reify/Type.hs`.
  - Elaboration inserts `ERoll`/`EUnroll` around recursive let bindings and use sites in `src/MLF/Elab/Elaborate/Algebra.hs`.
  - Phase 7 type checking and reduction explicitly support `ERoll`/`EUnroll` in `src/MLF/Elab/TypeCheck.hs` and `src/MLF/Elab/Reduce.hs`.
- Negative implementation/test evidence also exists in production surfaces:
  - `test/PipelineSpec.hs` still expects some recursive families to fail in Phase 4 with `WitnessNormalizationError`.
  - `test/PipelineSpec.hs` still expects some recursive families to fail in Phase 6 with `alias bounds survived scheme finalization`.
  - `src/MLF/Elab/Run/ResultType/Fallback.hs` still contains explicit local-lane guards (`rootBindingIsLocalType`) that preserve non-local recursive reconstruction as fail-closed, and `test/PipelineSpec.hs` asserts that behavior.
- The “fully implemented and tested” docs claim is therefore stronger than at least some live regression expectations in the codebase.

## Verification

- Focused positive test passes live:
  - `self-recursive function infers μ on both authoritative entrypoints`
- Focused negative tests also pass live, confirming these are still intended current behaviors rather than stale comments:
  - `characterizes nested recursive lets as Phase-3-safe with current Phase-4 witness normalization rejection`
  - `characterizes μ/∀ interaction as recursive-at-constraint level with current Phase-6 fail-closed behavior`
  - `keeps the same non-local proxy wrapper fail-closed at pipeline entrypoints`
- Fresh full gate passes live:
  - `cabal build all && cabal test`
  - observed result: `1175 examples, 0 failures`
- Passing full verification does not imply broad recursive completeness here, because the suite itself codifies multiple recursive fail-closed expectations as success criteria.
