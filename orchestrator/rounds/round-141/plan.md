# Round 141 Plan — item-3 (`TMu` + `ERoll`/`EUnroll` elaboration correctness)

I'm using the writing-plans skill to create the implementation plan.

## Scope

Bounded strictly to roadmap `item-3`:

- inferred `TyMu` must appear as `TMu` in elaborated output types
- elaborated terms must include explicit `ERoll`/`EUnroll` at recursive use sites
- Phase 7 checker must accept those terms
- focused tests in `PipelineSpec` must prove all of the above

No roadmap reordering, no unrelated refactors.

## Critical gap answer (from code reading)

`TyMu -> TMu` reification is already implemented in `src/MLF/Reify/Type.hs` (notably `goFull` branch handling `TyMu` around lines 352–372).  
The missing piece is elaboration-time term coercion insertion:

- `src/MLF/Elab/Elaborate/Algebra.hs` currently builds `EVar`/`ELam`/`EApp`/`ELet`/`ETyInst`, but does **not** insert `ERoll`/`EUnroll` for recursive `TMu` flows (see `AAppF` lines 134–247 and `ALetF` lines 248–334).

So the work should center on `elabAlg` coercion injection, not reification or acyclicity.

---

## Step 1 — Add μ-coercion insertion in elaboration algebra

**Modify:** `src/MLF/Elab/Elaborate/Algebra.hs`

### 1.1 Add local helpers near the bottom of module (after `sourceVarName` / before `mkOut`)

Target area: around lines 349–381.

Add helper functions with explicit signatures, e.g.:

- `insertMuUseSiteCoercions :: TypeCheck.Env -> ElabTerm -> ElabTerm -> Either ElabError ElabTerm`
- `unfoldMuOnce :: ElabType -> Maybe ElabType`
- `coerceArgForParam :: TypeCheck.Env -> ElabType -> ElabTerm -> Either ElabError ElabTerm`

Behavior:

- if function position type is `TMu ...` whose one-step unfolding is arrow, wrap callee in `EUnroll`
- if parameter expects `TMu` but argument is unfolded body, wrap argument in `ERoll`
- if parameter expects unfolded body but argument is `TMu`, wrap argument in `EUnroll`
- otherwise preserve existing term

Use only explicit wrappers (no typechecker fallback behavior).

### 1.2 Wire helper into `AAppF` assembly

Target area: `AAppF` branch around lines 240–247.

Current code ends with:

- `fApp = ...`
- `aApp = ...`
- `pure (EApp fApp aApp)`

Change this to construct `(fApp, aApp)` first, then call the new coercion helper, returning a possibly wrapped `EApp` containing `EUnroll`/`ERoll` as needed.

### 1.3 Wire helper into recursive let RHS finalization

Target area: `ALetF` branch around lines 297–327 (after `rhsAbs`/`rhsAbsTyChecked`, before `ELet`).

Add a normalization point that:

- checks declared let scheme body type (`schemeToType scheme`)
- if it is `TMu` and `rhsAbs` checks against the unfolded body, wraps RHS with `ERoll schemeType rhsAbs`
- leaves non-μ bindings unchanged

This ensures recursive definitions are rolled at binding sites so recursive uses can unroll explicitly at application sites.

### Step 1 verification

- `cabal test --test-show-details=direct --test-options='--match "Automatic μ-introduction (item-3)"'`

Expected: new item-3 tests (added in Step 2) compile and run; at least one failing before Step 2 assertions are finalized, then passing after Step 2.

---

## Step 2 — Add focused item-3 tests for μ-type + roll/unroll + Phase 7 acceptance

**Modify:** `test/PipelineSpec.hs`

### 2.1 Add term-shape predicates for roll/unroll evidence

Target area: near existing type predicates (`containsMu`, around lines 132–147).

Add helpers:

- `containsRollTerm :: ElabTerm -> Bool`
- `containsUnrollTerm :: ElabTerm -> Bool`

Recursive traversal over `ElabTerm` constructors; return `True` when `ERoll` / `EUnroll` appears anywhere.

### 2.2 Add dedicated `describe "Automatic μ-introduction (item-3)"` block

Target area: near existing item-2 block (around lines 1256+).

Add test cases for expression:

- `let f = \x. f x in f`

Assertions to include exactly roadmap-required checks:

1. `runPipelineElab` returns `(term, ty)` and `ty` includes `TMu` (`containsMu ty == True`)
2. elaborated `term` contains both `ERoll` and `EUnroll`
3. `typeCheck term == Right ty`
4. `runPipelineElabChecked` succeeds and agrees with unchecked result type

Use `renderPipelineError` in failure paths for actionable diagnostics.

### Step 2 verification

- `cabal test --test-show-details=direct --test-options='--match "Automatic μ-introduction (item-3)"'`

Expected: all new item-3 assertions pass; failures should point directly to missing coercion wrappers if regressions occur.

---

## Step 3 — Confirm Phase 7 acceptance path remains explicit and authoritative

**Primary file to validate (no semantic changes expected unless tests fail):**

- `src/MLF/Elab/TypeCheck.hs` (ERoll/EUnroll checking, lines 81–95)

**Secondary file to validate run-path behavior:**

- `src/MLF/Elab/Run/Pipeline.hs` (authoritative `typeCheck termClosed`, lines 201–214)

Action:

- keep typechecker semantics explicit (accept recursive terms via inserted `ERoll`/`EUnroll`)
- do **not** add implicit μ-application fallback in `EApp`; acceptance must come from explicit elaborated coercions
- only patch `TypeCheck.hs` if Step 2 reveals a concrete mismatch with explicit rolled/unrolled terms

### Step 3 verification

- `cabal test --test-show-details=direct --test-options='--match "Automatic μ-introduction (item-3)"'`
- `cabal test --test-show-details=direct --test-options='--match "annotation-heavy path still reports checked-authoritative type"'`

Expected: recursive item-3 case passes through `typeCheck`; existing checked-authoritative path remains green.

---

## Step 4 — Full contract gate for round acceptance

Run required baseline + full suite gate:

1. `git diff --check`
2. `python3 -m json.tool orchestrator/state.json >/dev/null`
3. `roadmap_dir="$(python3 -c "import json; print(json.load(open('orchestrator/state.json'))['roadmap_dir'])")" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
4. `cabal build all && cabal test`

And run diagnostics on changed Haskell files (`Algebra.hs`, optionally `TypeCheck.hs`, `PipelineSpec.hs`) with `lsp_diagnostics` before handoff.

Expected: zero whitespace/conflict issues, valid orchestrator state, full build/test pass, and clean diagnostics on edited files.

---

## Files expected to change for item-3

- `src/MLF/Elab/Elaborate/Algebra.hs` (core implementation)
- `test/PipelineSpec.hs` (focused item-3 regression coverage)
- `src/MLF/Elab/TypeCheck.hs` (**only if** explicit-roll/unroll terms still expose a concrete checker mismatch after Step 1)
