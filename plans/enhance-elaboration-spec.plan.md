---
name: Enhance ElaborationSpec
overview: Improve Phase 6 tests by reducing brittleness, adding structural/negative coverage, and expanding Φ/Σ + instantiation-semantics checks while keeping existing golden pretty-print tests.
todos:
  - id: helpers
    content: Add test helpers + hoist canonType/requireRight; refactor repetitive runPipelineElab cases
    status: completed
  - id: structural-type-assertions
    content: Replace brittle substring assertions with canonType-based structural comparisons where appropriate
    status: completed
  - id: applyInstantiation-tests
    content: Add positive + negative unit tests for applyInstantiation (Fig. 3 semantics)
    status: completed
  - id: sigma-tests
    content: Add bounded-quantifier and failure-mode tests for sigmaReorder
    status: completed
  - id: phi-tests
    content: Add phiFromEdgeWitness test with SchemeInfo + iterate all witnesses in soundness test
    status: completed
  - id: verify
    content: Run cabal test all and ensure clean output
    status: completed
---

# Enhance `test/ElaborationSpec.hs`

## Goals

- Make the Phase 6 test suite **more robust to binder naming / fresh ID changes**.
- Increase coverage for **xMLF instantiation semantics** (`applyInstantiation`) and **Φ/Σ** translation.
- Reduce boilerplate and improve failure diagnostics.

## Proposed changes

### 1) Add small test helpers (reduce boilerplate)

In `test/ElaborationSpec.hs`:

- Add helpers like:
- `requirePipeline :: Expr -> IO (Elab.ElabTerm, Elab.ElabType)` (fails via `expectationFailure` on `Left`).
- `requireRight :: Show e => Either e a -> IO a` (currently duplicated in one test).
- Hoist `canonType` to a shared helper (currently nested under “Witness translation (Φ/Σ)”).
- Replace repeated `case Elab.runPipelineElab expr of ...` with these helpers so tests are shorter and error output is consistent.

### 2) Replace fragile string/substring type assertions with structural comparisons

- Keep **strict pretty-string** checks for pretty-printer coverage (those are valuable).
- For tests currently doing substring checks like:
- `Elab.pretty ty 
\`shouldSatisfy\` ("t0 -> t0" \`isInfixOf\`)`
- switch to **structural checks** using `canonType` on the returned `ElabType` and an expected `ElabType`.
- This keeps tests correct even if binder names change while the type is alpha-equivalent.

### 3) Expand unit tests for `applyInstantiation` (xmlf Fig. 3)

Add a new `describe "xMLF instantiation semantics (applyInstantiation)"` section with:

- **Positive cases**:
- `InstElim` on `∀` substitutes the bound (defaulting to `⊥` when absent).
- `InstInside (InstBot τ)` updates a `∀` bound from `⊥` to `τ`.
- `InstUnder` applies an instantiation to the body and respects binder renaming.
- `InstApp τ` behaves like the documented sugar (`∀(⩾ τ); N`) via the existing desugaring.
- **Negative cases** (assert `Left`):
- `InstElim` on a non-`∀` type.
- `InstInside` / `InstUnder` on a non-`∀` type.
- `InstBot` on a non-`⊥` type.

### 4) Expand Σ(g) coverage

Under `describe "Σ(g) quantifier reordering"`:

- Add a test that swaps/permutes **bounded** quantifiers (bounds preserved and moved correctly).
- Add a failure-mode test (e.g. missing desired binder name) that asserts `sigmaReorder` returns `Left`.

### 5) Expand Φ coverage (beyond the “Nothing scheme info” path)

- Add a focused unit test for `phiFromEdgeWitness` with `Just SchemeInfo` that:
- uses a synthetic `EdgeWitness` whose ops target a non-front binder,
- verifies the produced instantiation reorders quantifiers (via swaps) before instantiation,
- checks `applyInstantiation (schemeToType siScheme) φ == expectedType` (up to `canonType`).
- Make the existing “id @ Int” Φ soundness test check **all** witnesses in `prEdgeWitnesses` (instead of `head`), to prevent missing regressions when more edges are present.

## Files to change

- `test/ElaborationSpec.hs`

## Test plan

- Run `cabal test all`.
- Confirm no new warnings and that Φ/Σ tests still pass.