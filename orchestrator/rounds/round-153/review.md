# Round 153 Review — Item 3: Fix OpRaise non-spine missing computation context for non-local bind-parent

## Decision: **APPROVED**

## Baseline Checks

| # | Check | Command | Result |
|---|-------|---------|--------|
| 1 | Whitespace / conflict markers | `git diff --check HEAD~1..HEAD` | PASS (no output) |
| 2 | `orchestrator/state.json` valid JSON | `python3 -m json.tool orchestrator/state.json >/dev/null` | PASS |
| 3 | Roadmap bundle resolves | `test -f "$roadmap_dir/roadmap.md" && ...` | PASS |
| 4 | Full build gate | `cabal build all` | PASS — no warnings, no errors |
| 5 | Full test gate | `cabal test` | **1176 examples, 0 failures** |

## Diff Review

**Commit:** `47ca697` on `orchestrator/round-153-fix-opraise-non-spine-context`

**Files changed:** 1 file, 1 insertion

```diff
diff --git a/src/MLF/Elab/Phi/Omega/Interpret.hs b/src/MLF/Elab/Phi/Omega/Interpret.hs
@@ -1096,6 +1096,7 @@ phiWithSchemeOmega ctx namedSet si introCount omegaOps = phiWithScheme
                                         in if parentC == rootC
                                             || case lookupNodePV parentC of
                                                 Just TyForall{} -> True
+                                                Just TyMu{} -> True
                                                 _ -> False
                                             then
```

### Plan Conformance

| Plan Step | Status | Evidence |
|-----------|--------|----------|
| Step 1: Add `Just TyMu{} -> True` to mbRootInst guard | ✅ Done | Diff shows exactly this line at the correct location (line 1099) |
| Step 2: Assess nodeTy0 guard (DO NOT change) | ✅ Correct | No modification to lines 1038-1042 |
| Step 3: Build and test | ✅ Pass | 1176 examples, 0 failures |
| Step 4: Assess PipelineSpec blocker | ✅ Documented | Case (a): still passes with `expectStrictPipelineFailure` |
| Step 5: Verify ElaborationSpec non-spine OpRaise | ✅ Pass | 1 example, 0 failures |
| Step 6: Check -Wall warnings | ✅ Clean | No warnings in build output |

### Scope Check

- **No files modified outside plan scope** — only `src/MLF/Elab/Phi/Omega/Interpret.hs`
- **No test expectations changed** — all 1176 existing tests pass identically
- **No import changes needed** — `TyMu` already in scope via `MLF.Constraint.Types`
- **`src/MLF/Reify/Type.hs` not touched** — correct, that was item-2

## Task-Specific Checks (verification.md Item 3)

| Check | Result | Evidence |
|-------|--------|----------|
| OpRaise in Interpret.hs handles non-local bind-parent | ✅ | `Just TyMu{} -> True` added to guard |
| Existing spine OpRaise behavior unchanged | ✅ | 23 OpRaise tests pass, 0 failures |
| Existing local non-spine OpRaise behavior unchanged | ✅ | "Φ translates non-spine OpRaise using binding edges and ≺ ordering (non-spine)" passes |
| PipelineSpec blocker test status documented | ✅ | Still passes with `expectStrictPipelineFailure` (case a — additional downstream blocker) |

## Targeted Test Results

```
$ cabal test --test-option='-m' --test-option='non-spine OpRaise'
  Φ translates non-spine OpRaise using binding edges and ≺ ordering (non-spine) [✔]
  1 example, 0 failures

$ cabal test --test-option='-m' --test-option='hits elaboration blocker for non-local proxy'
  hits elaboration blocker for non-local proxy wrapper despite open fallback at pipeline entrypoints [✔]
  1 example, 0 failures

$ cabal test --test-option='-m' --test-option='OpRaise'
  23 examples, 0 failures
```

## Observations

1. **The change is minimal and correct.** A single pattern-match arm treats `TyMu` as binder-like, which is semantically accurate — `TyMu` introduces a recursive variable just as `TyForall` introduces a polymorphic variable.

2. **The PipelineSpec blocker test still expects failure** (case (a) from the plan). This means item-3 unblocked the `mbRootInst` guard but there's at least one more downstream blocker. The test upgrade is correctly deferred to item-4.

3. **No new test was added** for the TyMu bind-parent path. This is acceptable because: (a) the plan did not call for a new test, (b) the fix is exercised via existing pipeline tests, and (c) the test upgrade in item-4 will provide the definitive coverage.

4. **Implementation notes are accurate** and match the observed results.

## Final Verdict

**APPROVED** — The implementation exactly follows the plan, introduces no regressions, the cabal gate passes clean, and all task-specific verification checks succeed.
