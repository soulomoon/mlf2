# Round 139 Review (Reviewer)

- **round_id**: `round-139`
- **roadmap_id**: `2026-03-29-00-automatic-iso-recursive-type-inference-implementation`
- **roadmap_revision**: `rev-001`
- **roadmap_dir**: `orchestrator/roadmaps/2026-03-29-00-automatic-iso-recursive-type-inference-implementation/rev-001`
- **item**: `item-1`
- **implementation commit reviewed**: `e952bda Break dependency cycles with automatic μ introduction`

## Inputs read

1. `orchestrator/rounds/round-139/plan.md`
2. `orchestrator/rounds/round-139/selection.md`
3. `orchestrator/roadmaps/2026-03-29-00-automatic-iso-recursive-type-inference-implementation/rev-001/verification.md`
4. `orchestrator/roadmaps/2026-03-29-00-automatic-iso-recursive-type-inference-implementation/rev-001/roadmap.md`
5. `orchestrator/worktrees/round-139/orchestrator/rounds/round-139/implementation-notes.md`
6. `git diff HEAD~1..HEAD` from `orchestrator/worktrees/round-139/`

## Commands run and outputs

All verification commands were run in:
`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-139`

### Required baseline commands

1. `git diff --check`
   - Output: *(no output)*
   - Result: pass

2. `python3 -m json.tool orchestrator/state.json >/dev/null`
   - Output: *(no output)*
   - Result: pass

3. `roadmap_dir="$(python3 -c "import json; print(json.load(open('orchestrator/state.json'))['roadmap_dir'])")" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
   - Output: *(no output)*
   - Result: pass

4. `cabal build all && cabal test`
   - Output summary (long output): build + full test suite completed successfully.
   - Key tail evidence:
     - `Finished in 2.2600 seconds`
     - `1158 examples, 0 failures`
     - `Test suite mlf2-test: PASS`
     - `1 of 1 test suites (1 of 1 test cases) passed.`
   - Full captured output path:
     - `/Users/ares/.local/share/opencode/tool-output/tool_d359164fe001Y5KNgONBme15ly`
   - Result: pass

### Item-specific evidence commands

5. `dist-newstyle/build/aarch64-osx/ghc-9.12.2/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "Cycle breaking"`
   - Output:
     - `rewrites a simple cycle into an acyclic graph [✔]`
     - `introduces TyMu nodes when it breaks a cycle [✔]`
     - `leaves acyclic input unchanged [✔]`
     - `3 examples, 0 failures`
   - Result: pass

6. `dist-newstyle/build/aarch64-osx/ghc-9.12.2/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "keeps recursive lets out of the Phase 3 cycle-error path"`
   - Output:
     - `keeps recursive lets out of the Phase 3 cycle-error path [✔]`
     - `1 example, 0 failures`
   - Result: pass

7. `dist-newstyle/build/aarch64-osx/ghc-9.12.2/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "keeps the non-recursive identity control stable"`
   - Output:
     - `keeps the non-recursive identity control stable [✔]`
     - `1 example, 0 failures`
   - Result: pass

8. Presence checks in tests:
   - `grep` in `test/AcyclicitySpec.hs` confirms `describe "Cycle breaking"` and `breakCyclesAndCheckAcyclicity` cases.
   - `grep` in `test/PipelineSpec.hs` confirms:
     - `it "keeps recursive lets out of the Phase 3 cycle-error path"`
     - `it "keeps the non-recursive identity control stable"`
   - Result: pass

### Additional diagnostics attempt

9. `lsp_diagnostics` on changed `.hs` files
   - Output: `haskell-language-server-wrapper` not installed in this environment.
   - Note: unable to collect LSP diagnostics; build/test gate is clean.

## Baseline checklist results

- [pass] `git diff --check`
- [pass] `python3 -m json.tool orchestrator/state.json >/dev/null`
- [pass] Roadmap bundle resolution command
- [pass] `cabal build all && cabal test`

## Item-1 specific check results

- [pass] Non-recursive programs remain stable
  - Evidence: pipeline test `keeps the non-recursive identity control stable` passes.
- [pass] Simple recursive expression no longer fails specifically at acyclicity
  - Evidence: pipeline test `keeps recursive lets out of the Phase 3 cycle-error path` passes.
- [pass] New cycle-breaking tests exist in `AcyclicitySpec`
  - Evidence: `Cycle breaking` group with 3 passing examples.
- [pass] Pipeline smoke test for recursive expression exists in `PipelineSpec`
  - Evidence: explicit `keeps recursive lets out of the Phase 3 cycle-error path` test present and passing.

## Diff analysis against plan

Plan conformance check (`orchestrator/rounds/round-139/plan.md`):

1. **Acyclicity API extension + deterministic rewrite loop** — **matched**
   - `src/MLF/Constraint/Acyclicity.hs` exports and implements
     `breakCyclesAndCheckAcyclicity :: Constraint -> Either CycleError (Constraint, AcyclicityResult)`.
   - Repeated cycle-break loop implemented (`go` recursion).
   - Deterministic pivot selection via `pivotId = minimum cycleIds`.

2. **Single-cycle rewrite via μ introduction** — **matched**
   - Rewriter clones RHS with substitution (`cloneWithSubstitution`) and introduces fresh `TyVar` binder + `TyMu` node.
   - Pivot edge RHS rewired to μ root.
   - Binding parents updated (`attachClonedBindParents` + `setBindParentCR`).
   - Non-recursive no-op behavior covered (`leaves acyclic input unchanged`).

3. **Pipeline wiring between normalization and presolution** — **matched**
   - `src/MLF/Elab/Run/Pipeline.hs` replaces direct `checkAcyclicity` call with:
     - `(c1Broken, acyc) <- fromCycleError (breakCyclesAndCheckAcyclicity c1)`
   - Uses `c1Broken` in downstream presolution/generalization/scope/result-type calls.

4. **Focused regressions in AcyclicitySpec/PipelineSpec** — **matched**
   - Added cycle-breaking tests in `test/AcyclicitySpec.hs`.
   - Added recursive smoke + non-recursive control tests in `test/PipelineSpec.hs`.

5. **Full verification gate** — **matched**
   - Full baseline gate passed with zero failures.

No divergence from the approved round-139 plan was found.

## Evidence summary

- Full repo gate passed (`1158 examples, 0 failures`).
- Baseline integrity checks passed (diff check, JSON validity, roadmap bundle resolution).
- Item-1 behavior-specific checks pass:
  - cycle-breaking tests pass,
  - recursive pipeline path avoids Phase 3 acyclicity failure route,
  - non-recursive control remains stable.
- Implementation diff aligns with plan scope and responsibilities.

## Decision

**APPROVED**
