# Source String Test Cleanup

## Goal

Remove or replace tests that read repository source, docs, or config files and assert brittle exact string fragments. These tests are not reliable behavior checks because an implementation can be renamed without changing the behavior or architecture risk.

## Scope

- In scope: tests that use `readFile` on `src/`, `src-public/`, `app/`, `docs/`, `README.md`, `CHANGELOG.md`, `TODO.md`, `AGENTS.md`, or `mlf2.cabal` and then assert exact `isInfixOf` presence or absence for implementation/prose fragments.
- Out of scope for this pass: fixture reads under `test/programs/**`, checked-in golden comparisons, emitted LLVM/output assertions, and diagnostic-format checks that operate on runtime results instead of repository source files.
- Structural repo checks may remain only when they check an actual boundary such as file absence, cabal/test wiring, import-boundary offenders, or module registration. Exact symbol/prose string checks should be removed or rewritten later as parser/API/import-graph checks.

## Inventory Summary

- `test/PipelineSpec.hs`: 135 source/doc/config reads; largest cleanup target. Most guards belong outside pipeline behavior tests and should be deleted or moved to structural repo checks only if the boundary can be checked without exact implementation substrings.
- `test/RepoGuardSpec.hs`: 84 source/doc/config reads. Keep file/module/wiring/import-boundary guards; rewrite or remove exact prose and private-symbol substring checks.
- `test/PresolutionFacadeSpec.hs`: 16 source reads, mostly facade/export-shape guards.
- `test/Constraint/SolvedSpec.hs`: 12 source reads, mostly solved-surface retired-helper guards.
- `test/ElaborationSpec.hs`: 8 source reads plus many diagnostic/output substrings. Clean source reads first; leave runtime diagnostics for a separate pass.
- Smaller source-read guards: `test/Presolution/UnificationClosureSpec.hs`, `test/PresolutionSpec.hs`, `test/GeneralizeSpec.hs`, `test/FrontendNormalizeSpec.hs`, `test/FrontendParseSpec.hs`, `test/Reify/TypeSpec.hs`, `test/TranslatablePresolutionSpec.hs`, `test/Presolution/EdgePlannerSpec.hs`, `test/CanonicalizerSpec.hs`, `test/BindingSpec.hs`, and `test/PublicSurfaceSpec.hs`.

## Cleanup Order

1. Delete redundant source guards that already have nearby semantic tests:
   - `test/TranslatablePresolutionSpec.hs`
   - `test/Presolution/EdgePlannerSpec.hs`
   - `test/Presolution/UnificationClosureSpec.hs`
   - selected ARI-C1 source guards in `test/PipelineSpec.hs` with same-test behavioral assertions
2. Remove parser/scaffolding source guards where parser behavior already exercises canonical/legacy forms:
   - `test/FrontendParseSpec.hs`
   - related parser-token guards in `test/RepoGuardSpec.hs`
3. Remove or rewrite small one-off source guards:
   - `test/FrontendNormalizeSpec.hs`
   - `test/GeneralizeSpec.hs`
   - `test/Reify/TypeSpec.hs`
   - `test/CanonicalizerSpec.hs`
   - `test/BindingSpec.hs`
   - `test/PublicSurfaceSpec.hs`
4. Split `test/PipelineSpec.hs`:
   - remove source-shape checks from pipeline behavior sections;
   - keep only behavior assertions in pipeline specs;
   - move any still-needed structural checks into `RepoGuardSpec` only if they can be implemented as file/module/import/export checks instead of exact source snippets.
5. Audit `test/RepoGuardSpec.hs`:
   - keep wiring/file/import-boundary checks;
   - remove exact prose and private-symbol substring checks;
   - leave TODO notes only for structural rewrites that need parser/API support.

## Validation

- Run focused tests for each edited spec.
- Run `cabal test` after the cleanup pass if time allows.
