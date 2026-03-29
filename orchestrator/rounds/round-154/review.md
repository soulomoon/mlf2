# Round 154 Review — Item 4: Upgrade pipeline entrypoint test from expected-failure to success

## Decision: **APPROVED**

## Baseline Checks

| # | Check | Command | Result |
|---|-------|---------|--------|
| 1 | Whitespace / conflict markers | `git diff --check HEAD~1..HEAD` | PASS (no output) |
| 2 | `orchestrator/state.json` valid JSON | verified | PASS |
| 3 | Roadmap bundle resolves | verified | PASS |
| 4 | Full build gate | `cabal build all` | PASS — no warnings |
| 5 | Full test gate | `cabal test` | **1176 examples, 0 failures** |

## Diff Review

**Commit:** `4b8392f` on `orchestrator/round-154-upgrade-pipeline-test`

**Files changed:** 1 file, 1 line change

```diff
-      it "hits elaboration blocker for non-local proxy wrapper despite open fallback at pipeline entrypoints" $ do
+      it "elaboration succeeds but produces TCArgumentMismatch for non-local proxy wrapper" $ do
```

### Plan Conformance

| Plan Step | Status | Evidence |
|-----------|--------|----------|
| Step 1: Update test description | ✅ Done | Description changed from "hits elaboration blocker" to "TCArgumentMismatch" |
| Step 2: Survey ElaborationSpec | ✅ Done | All 13 sites classified in implementation-notes.md |
| Step 3: Assess TCArgumentMismatch | ✅ Documented | Controller investigation found the error; out of scope to fix |
| Step 4: Build and test | ✅ Pass | 1176 examples, 0 failures |

### Scope Check

- **Only `test/PipelineSpec.hs` modified** — single line description change
- **No source code changes** — correct per plan
- **No assertion logic changes** — test still uses `expectStrictPipelineFailure`
- **Survey documented** — all 13 ElaborationSpec sites classified as legitimate

### Item-4 Roadmap Completion Assessment

The roadmap item says:
> "Upgrade pipeline entrypoint test from expected-failure to success"

The test was NOT upgraded to success because the pipeline still fails — with a
different error (TCArgumentMismatch instead of PhiTranslatabilityError). The
test description was updated to accurately reflect the current failure mode.

This is a **partial completion** of item-4: the survey and reclassification
are done, but the full upgrade to success requires fixing the TCArgumentMismatch
(a deeper elaboration correctness issue).

## Final Verdict

**APPROVED** — The implementation accurately documents the current state, the
survey is thorough, no regressions, and the scope limitation (not attempting
to fix TCArgumentMismatch) is appropriate given the roadmap's completion
criteria note about documenting findings.
