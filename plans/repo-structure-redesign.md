# Plan

Restructure the repo into clear, domain-based subsystems and hide almost all implementation modules from downstream users. The stable public surface becomes `MLF.API`/`MLF.Pipeline` (plus legacy `MyLib`), while tests keep access to internals via a private sublibrary.

## Requirements
- Keep behavior/semantics unchanged (moves/renames/splits only).
- Prefer domain-based namespaces over phase-based.
- Make most `MLF.*` modules internal (not importable by downstream packages).
- Provide a clear public entrypoint: `MLF.API` (umbrella) and `MLF.Pipeline` (entrypoints).
- Keep `-Wall` warning-free and keep the suite green throughout.

## Scope
- In:
  - `mlf2.cabal` component layout.
  - Module namespaces + file layout under `src/MLF/**`.
  - Public entry modules (`MyLib`, new `MLF.API`, new `MLF.Pipeline`).
  - Test imports and test-suite dependencies.
  - Doc references to modules/paths (as needed).
- Out:
  - Algorithmic changes to solver/elaboration.
  - New features.
  - Renaming the package.

## End State (Proposed)

### Public API (downstream)
- Import recommendation:
  - `import MLF.API` for types + entrypoints.
  - `import MLF.Pipeline` for “runner-only” usage.
- Publicly exposed modules (minimal):
  - `MLF.API`
  - `MLF.Pipeline`
  - `MyLib` (compat; re-exports `MLF.API`)

### Internal implementation (package-private)
Domain-based namespaces (all internal):
- `MLF.Frontend.*` (surface syntax + desugar + constraint generation)
- `MLF.Constraint.*` (graph types + normalize + acyclicity + presolution + solve + constraint-root)
- `MLF.Binding.*` (binding-tree queries + executable χe ops + harmonization)
- `MLF.Witness.*` (Ω types, ω execution, witness normalization helpers)
- `MLF.Elab.*` (reify/generalize + Φ/Σ translation + elaboration)
- `MLF.Util.*` (union-find, order keys, shared utilities)

## Cabal Design (best option)

### Private sublibrary for internals
Add a named sublibrary:
- `library mlf2-internal`
  - `visibility: private` (so downstream packages cannot `build-depends: mlf2:mlf2-internal`)
  - contains all internal `MLF.*` modules (the full implementation)

### Public library
Keep the unnamed `library` stanza as the public library:
- `exposed-modules: MyLib, MLF.API, MLF.Pipeline`
- `build-depends: base, containers, mtl, mlf2:mlf2-internal`

### Tests
Update `test-suite mlf2-test`:
- `build-depends: ..., mlf2:mlf2-internal` (so specs can continue importing internal modules)

This design keeps internals accessible to tests/executables while preventing downstream imports.

## Public API Design

### `MLF.Pipeline`
Owns “run the phases” entrypoints. Suggested exports:
- `inferConstraintGraph :: Expr -> Either ConstraintError ConstraintResult`
- `runPipelineSolve :: Expr -> Either String (SolveResult, NodeId)` (phases 1–5)
- `runPipelineElab :: Expr -> Either String (ElabTerm, ElabType)` (phases 1–6)

### `MLF.API`
Umbrella module that re-exports:
- `MLF.Pipeline`
- surface AST types (`Expr`, `Lit`, etc.)
- elaboration result types (`ElabTerm`, `ElabType`, `Pretty`)
- (optionally) a small set of error types intended for downstream callers

### `MyLib`
Compatibility wrapper:
- Re-export `MLF.API`
- Keep `inferConstraintGraph` and remove/ignore `someFunc` over time (non-breaking: keep it for now).

## Action Items

[x] 0. Baseline + guardrails
- Confirm current baseline passes: `cabal --config-file=.cabal-config test --test-show-details=direct`.
- Decide a “no semantic diffs” rule: only `git mv`, module renames, and import rewrites.

[x] 1. Add `MLF.Pipeline` + `MLF.API` (no module moves yet)
- Implement thin wrappers around existing code (likely calling into `MLF.Elab.Pipeline.runPipelineElab` initially).
- Update `src/MyLib.hs` to re-export `MLF.API`.
- Verification:
  - `cabal --config-file=.cabal-config build`
  - `cabal --config-file=.cabal-config test --test-show-details=direct`

[x] 2. Introduce `mlf2-internal` private sublibrary (no namespace migration yet)
- Create `library mlf2-internal` with `visibility: private`.
- Move all existing `MLF.*` modules from the public library into `mlf2-internal`.
- Shrink public `library` to expose only `MyLib`, `MLF.API`, `MLF.Pipeline`.
- Update public library to depend on `mlf2:mlf2-internal`.
- Update `test-suite` to depend on `mlf2:mlf2-internal`.
- Verification: full test suite.

[x] 3. Domain namespace migration (batch-by-batch, still internal)
Perform in dependency-safe batches; after each batch run the full test suite.
- Batch 3.1: `MLF.Util.*` (union-find, order, small helpers)
- Batch 3.2: `MLF.Binding.*` (binding tree + executable ops + harmonization)
- Batch 3.3: `MLF.Constraint.*` (types + normalize + acyclicity + presolution + solve + constraint-root)
- Batch 3.4: `MLF.Witness.*` (Ω/ω executor pieces)
- Batch 3.5: `MLF.Frontend.*` + `MLF.Elab.*` (syntax/desugar/constraint-gen + elaboration)

For each batch:
- Use `git mv` to move files into new directories.
- Update `module ...` headers.
- Rewrite imports within internal modules.
- Update `mlf2.cabal` module lists for `mlf2-internal`.

[x] 4. Optional: split large modules after names are stable
- Split presolution into submodules (`MLF.Constraint.Presolution.Base`/`MLF.Constraint.Presolution.Core`) and extract xMLF types into `MLF.Elab.Types`.
- Keep exported surface unchanged (only internal).
- Verification: full test suite.

[x] 5. Tests mirror domains (optional but recommended)
- Either keep spec filenames but update imports, or reorganize tests under `test/MLF/**`.
- Update `test/Main.hs` and `mlf2.cabal` `test-suite other-modules`.
- Verification: full test suite.

[x] 6. Docs/layout cleanup (optional)
- Added `docs/architecture.md` explaining public API vs internals and the domain layout.
- Updated `AGENTS.md`/`roadmap.md` (and other plans/notes) to match the current module/layout reality.
- Keep existing research notes (`papers/`) unchanged.

[x] 7. Final “public surface” audit
- Ensure no internal `MLF.*` modules are exposed from the public library.
- Ensure `MLF.API` re-exports all types/functions downstream users need.
- Verification:
  - `cabal --config-file=.cabal-config build`
  - `cabal --config-file=.cabal-config test --test-show-details=direct`

## Testing and validation
- After every step/batch:
  - `cabal --config-file=.cabal-config build`
  - `cabal --config-file=.cabal-config test --test-show-details=direct`

## Risks and edge cases
- Cabal sublibrary wiring mistakes (especially `visibility: private`): mitigate by doing Step 2 with *no* module renames first.
- Import cycles introduced during refactors: mitigate by migrating leaf utilities first.
- Large diffs: mitigate by batching and keeping compatibility wrappers (`MLF.API`/`MLF.Pipeline`) stable from Step 1.

## Open questions
- Should `MLF.Pipeline` expose both `runPipelineSolve` (phases 1–5) and `runPipelineElab` (1–6), or only the elaboration runner?
