# Round 142 Plan — item-4 edge-case hardening (automatic iso-recursive inference)

## Scope guard

- Selected roadmap item: `item-4` only (edge-case hardening + regression tests).
- Keep roadmap ordering unchanged.
- Prefer test additions in `test/PipelineSpec.hs`; only touch implementation code if an edge-case test exposes a concrete bug.

## Current-state readout (from required files)

- `src/MLF/Constraint/Acyclicity.hs`
  - `breakCyclesAndCheckAcyclicity` is already in the Phase-3 path and rewrites dependency cycles into `TyMu` (`breakSingleCycle`, `cloneWithSubstitution`, `attachClonedBindParentsWithMu`).
- `src/MLF/Elab/Elaborate/Algebra.hs`
  - Item-3 logic exists for recursive coercions (`insertMuUseSiteCoercions`, `coerceArgForParam`) and let-level `ERoll` insertion for `TMu` schemes in `ALetF`.
- `src/MLF/Elab/TypeCheck.hs`
  - Phase-7 checks include `ERoll`/`EUnroll` rules and contractiveness checks.
- `src/MLF/Reify/Type.hs`
  - Reification has explicit `TyMu -> TMu` handling in `goFull` (`TyMu {tnBody = b}` branch).
- `test/PipelineSpec.hs`
  - Item-2 and item-3 blocks already exist, plus explicit-annotation μ tests, but no dedicated item-4 edge-case block covering all required cases together.

## Edge-case matrix (test expression + expected behavior + likely status)

1. **Nested recursive lets**
   - Expression:
     - `let f = λx. let g = λy. f (g y) in g in f`
     - AST form in test: `ELet "f" (ELam "x" (ELet "g" (ELam "y" (EApp (EVar "f") (EApp (EVar "g") (EVar "y")))) (EVar "g"))) (EVar "f")`
   - Expected:
     - both authoritative entrypoints (`runPipelineElab`, `runPipelineElabChecked`) stay past Phase 3
     - elaborated result type contains recursive shape (`containsMu`)
     - elaborated term type-checks via `typeCheck`
   - Current status guess: likely already handled by cycle-breaking + item-3 coercions, but currently unpinned by a direct regression test.

2. **Polymorphic recursion with annotation**
   - Expression candidate:
     - `let f = ((λx. f x) : ∀a. a -> a) in f`
     - AST: `ELet "f" (EAnn (ELam "x" (EApp (EVar "f") (EVar "x"))) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))) ) (EVar "f")`
   - Expected:
     - document behavior explicitly: this case may not create a dependency-cycle rewrite (`TyMu`) because annotation/polymorphism can change graph shape
     - minimum required: no Phase-3 cycle rejection; behavior is stable and explicit (either successful typed result or strict fail-closed mode, but not Phase-3 regression)
   - Current status guess: uncertain; needs a characterization test that locks current intended behavior.

3. **μ/∀ interaction (recursive polymorphic flow)**
   - Expression:
     - `let id = λx. x in let f = λx. f (id x) in f`
     - AST: `ELet "id" (ELam "x" (EVar "x")) (ELet "f" (ELam "x" (EApp (EVar "f") (EApp (EVar "id") (EVar "x")))) (EVar "f"))`
   - Expected:
     - succeeds on both authoritative entrypoints
     - recursive type is present (`containsMu`) and Phase-7 type-check passes for elaborated term
   - Current status guess: likely supported but not currently isolated as a regression.

4. **Higher-order recursion (returns a function)**
   - Expression:
     - `let f = λx. λy. f x y in f`
     - AST: `ELet "f" (ELam "x" (ELam "y" (EApp (EApp (EVar "f") (EVar "x")) (EVar "y")))) (EVar "f")`
   - Expected:
     - succeeds on both entrypoints
     - recursive type is present (`containsMu`)
     - elaborated term type-checks (`typeCheck term == Right ty`), and at least one recursive coercion appears (`containsRollTerm` or `containsUnrollTerm`)
   - Current status guess: plausible but untested; good candidate to expose missing unroll on higher-order application chains.

5. **Already-annotated μ remains unchanged**
   - Expression (reuse existing shape for explicit μ annotation):
     - `let k = (λx. x : μa. a -> Int) in k` via existing helper style
   - Expected:
     - both entrypoints succeed with same recursive-arrow shape (`matchesRecursiveArrow` with expected `TMu` type)
     - no degradation from automatic μ path (explicit μ still accepted and type-checks)
   - Current status guess: already mostly covered by existing tests, but add item-4-local regression assertion to prevent future drift.

## Concrete implementation plan (for the implementer)

### Step 1 — Add dedicated item-4 regression block in PipelineSpec

**File:** `test/PipelineSpec.hs`

1. Add a new sibling block after existing item-3 section:
   - `describe "Automatic μ-introduction (item-4 edge cases)" $ do ...`
2. Add one `it` per edge case above (five tests), using the exact AST expressions listed in the matrix.
3. Reuse existing local helpers already in this file:
   - `expectAlignedPipelinePastPhase3`
   - `expectAlignedPipelineSuccessType`
   - `containsMu`, `containsRollTerm`, `containsUnrollTerm`, `matchesRecursiveArrow`
   - `typeCheck`, `renderPipelineError`

### Step 2 — Pin polymorphic-recursion behavior explicitly (characterization)

**File:** `test/PipelineSpec.hs`

1. In the polymorphic-recursion test, assert one of the explicit acceptable outcomes and document it in the test name/body:
   - preferred: success with typed output and no Phase-3 error
   - if strict fail-closed behavior is the current intended result, assert fail reason class is *not* Phase-3 acyclicity regression (reuse `expectStrictPipelineFailure` + Phase-3 guard check)
2. Also inspect `automaticMuConstraint` for this expression and record whether `constraintContainsTyMu` is `True`/`False`; assert the observed intended value to document “may not produce cycles” behavior.

### Step 3 — Bug-fix contingency only if tests fail

**Files (only if required by failing edge-case tests):**
- `src/MLF/Elab/Elaborate/Algebra.hs`
- optionally `src/MLF/Elab/TypeCheck.hs` (only if Phase-7 mismatch is root cause)

Potential concrete fix target from code read:

1. In `MLF.Elab.Elaborate.Algebra` (`AAppF` branch), helper `annHasMuScheme` currently checks only top-level `TMu {}`.
2. If item-4 tests reveal polymorphic-μ annotations failing recovery, broaden this predicate to detect nested μ under `TForall`/`TArrow`/`TCon` (local helper like `containsMuType`).
3. Keep fix minimal and localized to recovery decision path (`reifyInstWithRecovery`) without changing unrelated instantiation behavior.

### Step 4 — Verification

Run targeted checks first, then full gate:

1. `cabal test --test-show-details=direct --test-options='--match "Automatic μ-introduction (item-4 edge cases)"'`
2. `cabal test --test-show-details=direct --test-options='--match "Automatic μ-introduction (item-2)|Automatic μ-introduction (item-3)|Automatic μ-introduction (item-4 edge cases)"'`
3. Full contract gate: `cabal build all && cabal test`

## Exit criteria for round-142

- Five edge-case regressions exist in `test/PipelineSpec.hs` (nested lets, polymorphic recursion characterization, μ/∀ interaction, higher-order recursion, explicit-μ stability).
- Any discovered bug is fixed at root cause with minimal implementation changes in named functions.
- All item-2/item-3/item-4 recursive-inference tests pass.
- Full verification gate passes: `cabal build all && cabal test`.
