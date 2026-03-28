# Round 140 Plan — item-2 automatic μ-introduction pipeline coverage

roadmap_id: `2026-03-29-00-automatic-iso-recursive-type-inference-implementation`
roadmap_revision: `rev-001`
roadmap_dir: `orchestrator/roadmaps/2026-03-29-00-automatic-iso-recursive-type-inference-implementation/rev-001`

## Scope boundary

- Target only roadmap `item-2`: prove and harden automatic μ-introduction behavior at pipeline entrypoints (`runPipelineElab`, `runPipelineElabChecked`) with focused tests.
- Do **not** modify roadmap/state control-plane files.
- Keep any code changes minimal and directly tied to recursive pipeline behavior surfaced by new tests.

## Concrete implementation steps

1. **Establish current baseline and identify exact gaps in existing recursive coverage**

   - Inspect and classify current tests in `test/PipelineSpec.hs`, especially:
     - `it "keeps recursive lets out of the Phase 3 cycle-error path"`
     - `it "keeps the non-recursive identity control stable"`
     - `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
   - Confirm that these are mostly smoke/guard checks and do not yet satisfy item-2 completion criteria (success + inferred recursive type behavior for unannotated recursion).
   - Verify current pipeline wiring location in `src/MLF/Elab/Run/Pipeline.hs` at `runPipelineElabWith`:
     - `breakCyclesAndCheckAcyclicity` call between `normalize` and `computePresolution`.

   **Verification commands**
   - `cabal test mlf2-test --test-options="--match \"Phase 3 cycle-error\""`
   - `cabal test mlf2-test --test-options="--match \"ARI-C1 feasibility characterization\""`

2. **Add focused end-to-end recursive inference tests for both authoritative entrypoints**

   - Modify `test/PipelineSpec.hs` in the Integration Tests area (near existing recursive smoke checks) to add a compact item-2-focused block (new `describe` section).
   - Add shared helper(s) (in `PipelineSpec.hs`) that execute **both**:
     - `runPipelineElab Set.empty (unsafeNormalizeExpr expr)`
     - `runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr)`
     and assert aligned success/failure shape.
   - Add concrete test cases required by roadmap item-2:
     1. simple self-recursive function: `ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")`
     2. mutually recursive via nested lets (construct nested-let expression that forces a recursive cycle across bound names)
     3. recursive data-like pattern using tuple-like constructor style (`let lst = λx. λxs. ...`) used recursively in body
     4. non-recursive control expression(s) still matching prior type/shape expectations
   - For recursive cases, assert at minimum:
     - both entrypoints do not fail with Phase 3 cycle error
     - inferred result types contain recursive structure (`containsMu == True`) where expected
   - For non-recursive controls, assert:
     - both entrypoints succeed
     - unchecked/checked type equality remains unchanged
     - no accidental μ-introduction (`containsMu == False`)

   **Verification commands**
   - `cabal test mlf2-test --test-options="--match \"automatic μ-introduction\""`
   - `cabal test mlf2-test --test-options="--match \"self-recursive\""`
   - `cabal test mlf2-test --test-options="--match \"mutually recursive\""`
   - `cabal test mlf2-test --test-options="--match \"non-recursive control\""`

3. **Fix implementation issues surfaced by the new tests (only if failures occur)**

   - Primary fix point: `src/MLF/Elab/Run/Pipeline.hs`
     - Function: `runPipelineElabWith`
     - Ensure the cycle-broken constraint (`c1Broken`) is the constraint threaded through downstream stages that must observe introduced `TyMu`.
   - Secondary fix point (if recursive expressions fail before pipeline cycle stage):
     - `src/MLF/Frontend/ConstraintGen/Translate.hs`
     - Function: `buildExprRaw` (`ELet` branch)
     - Adjust only if required to let recursive definitions reach phase-3/phase-4 pipeline processing.
   - If cycle-rewrite semantics are implicated, apply minimal targeted fix in:
     - `src/MLF/Constraint/Acyclicity.hs`
     - Functions: `breakCyclesAndCheckAcyclicity`, `breakSingleCycle`, or rewrite helpers.
   - Keep fixes root-cause and avoid compatibility shims.

   **Verification commands**
   - Re-run failing focused tests from Step 2 until green.
   - `cabal test mlf2-test --test-options="--match \"Pipeline \(Phases 1-5\)\""`

4. **Regression check: ensure existing behavior remains stable outside recursive target**

   - Re-run existing recursive/ARI guardrails to ensure no collateral breakage:
     - recursive annotation-driven checks
     - ARI-C1 characterization block
   - Re-run nearby non-recursive integration checks in `PipelineSpec.hs` to confirm unchanged results.

   **Verification commands**
   - `cabal test mlf2-test --test-options="--match \"annotation-heavy path still reports checked-authoritative type\""`
   - `cabal test mlf2-test --test-options="--match \"ARI-C1 feasibility characterization\""`
   - `cabal test mlf2-test --test-options="--match \"non-recursive identity control stable\""`

5. **Run required full gate and capture evidence**

   - Run full roadmap-required verification after targeted tests are green.
   - Confirm no formatting/conflict marker issues.

   **Verification commands**
   - `git diff --check`
   - `cabal build all && cabal test`

## Expected deliverables for round-140

- Updated `test/PipelineSpec.hs` with item-2-focused recursive inference tests that explicitly exercise both `runPipelineElab` and `runPipelineElabChecked`.
- Minimal implementation adjustments only if new tests reveal gaps (prefer `MLF.Elab.Run.Pipeline` first; touch `ConstraintGen.Translate`/`Constraint.Acyclicity` only when needed).
- Green full gate: `cabal build all && cabal test`.
