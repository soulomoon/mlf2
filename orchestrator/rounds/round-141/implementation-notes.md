# Round 141 — item-3 implementation notes

## Scope completed

Implemented roadmap item-3 work for explicit iso-recursive elaboration behavior:

- ensured recursive let schemes can be lifted to `TMu` for self-recursive alias shape
- inserted explicit `EUnroll` at recursive application use sites
- inserted explicit `ERoll` at recursive let binding sites
- aligned Phase 7 checker acceptance for explicit roll/unroll terms in this recursive lane
- added focused PipelineSpec coverage for item-3 (`TMu` + `ERoll`/`EUnroll` + checked pipeline parity)

## Files changed

- `src/MLF/Elab/Elaborate/Algebra.hs`
  - Added μ-coercion helpers:
    - `insertMuUseSiteCoercions`
    - `unfoldMuOnce`
    - `coerceArgForParam`
    - helper predicates used for recursive let normalization
  - Wired coercion insertion in `AAppF` assembly.
  - Wired recursive-let RHS wrapping (`ERoll`) in `ALetF` finalization.

- `src/MLF/Elab/TypeCheck.hs`
  - Let-check now evaluates recursive RHS under env extended with the let binder.
  - `ERoll` checking now accepts the recursive-body alias shape through
    `collapseRecursiveAlias` (so explicit iso wrappers from item-3 elaborate/check coherently).

- `test/PipelineSpec.hs`
  - Added `containsRollTerm` / `containsUnrollTerm` term-shape predicates.
  - Added `describe "Automatic μ-introduction (item-3)"` test validating:
    - inferred type contains `TMu`
    - elaborated term contains both `ERoll` and `EUnroll`
    - `typeCheck` succeeds
    - `runPipelineElabChecked` succeeds and agrees with unchecked type

## Verification run

- Focused item-3 gate:
  - `cabal test --test-show-details=direct --test-options='--match "Automatic μ-introduction (item-3)"'` ✅

- Required round gate:
  - `git diff --check` ✅
  - `python3 -m json.tool orchestrator/state.json >/dev/null` ✅
  - roadmap contract files from `orchestrator/state.json` exist ✅
  - `cabal build all && cabal test` ✅ (`1163 examples, 0 failures`)

## Diagnostics note

- `lsp_diagnostics` was attempted on all changed Haskell files after installing HLS, but the tool-side LSP initialization timed out in this environment.
- Build + full test gate is clean and warning-free.
