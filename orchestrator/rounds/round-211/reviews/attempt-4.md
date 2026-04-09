# Round 211 Attempt 4 Review

Decision: **REJECTED: rev-009 specifically admitted `src/MLF/Elab/TermClosure.hs`, but the final diff restores the inherited round-211 baseline without any kept `TermClosure.hs` landing; reused implementation evidence plus reviewer verification still show a red full gate (`1341` examples, `26` failures) and a failing thesis-conformance gate on the same remaining fail-fast cluster.**

## Retry Contract

- Implemented stage result: no production landing was kept. The temporary `TermClosure.hs` / `Algebra.hs` experiment was reverted, the final diff preserves only the inherited round-211 baseline in `src/MLF/Elab/Elaborate/Algebra.hs`, `src/MLF/Elab/Elaborate/Annotation.hs`, `src/MLF/Elab/Legacy.hs`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, and `test/Research/P5ClearBoundarySpec.hs`, and `src/MLF/Elab/TermClosure.hs` is untouched in the final diff.
- Attempt verdict: rejected
- Stage action: retry
- Retry reason: rev-009 admitted the `TermClosure.hs` closure / alias-boundary seam as the only new lawful continuation, but the final diff leaves that seam untouched and the carried baseline still fails the mandatory milestone-2 gates. Reused implementation evidence shows the remaining cluster is unchanged on the restored baseline: `BUG-2026-02-06-002` and the A6 dual-coercion pair still fail with `TCExpectedArrow`, the nested-let fail-fast / invariant probes still fail with alias-side `TCLetTypeMismatch`, and `cabal build all && cabal test` still fails with `26` failures.
- Fix hypothesis: do not run another closure-only rev-009 implementation retry on the unchanged baseline. The reverted experiment already proved that a direct `closeTermWithSchemeSubstIfNeeded` repair can remove the alias-side mismatch only by turning the nested-let fail-fast probe into false success `forall a. a -> a`. The next lawful controller move should therefore be a same-family successor revision or other different lawful recovery path that preserves the round-211 baseline and explicitly admits only the next downstream post-closure authoritative classification / witness seam needed after the alias-closure change, while keeping pipeline/public/fallback surfaces and cyclic/second-interface widening closed.

## Commands Run

Reviewer-run commands:

- `python3 - <<'PY' ... verify rev-009 lineage across orchestrator/state.json, selection.md, and pointer stubs ... PY` -> exit `0` (`ROUND211_REV009_LINEAGE_OK`)
- `python3 - <<'PY' ... verify orchestrator/rounds/round-001 through round-098 exist ... PY` -> exit `0` (`ROUND_001_098_CONTINUITY_OK`)
- `python3 - <<'PY' ... print round-094 through round-098 review-record status ... PY` -> exit `0` (`round-094 authoritative`, `round-095 authoritative`, `round-096 authoritative`, `round-097 authoritative`, `round-098 authoritative`)
- `rg -n 'non-cyclic-graph = unknown|bounded subset only|continue within the current architecture|current-architecture blockers|fail-closed rejection|stable visible persistence|repo-level readiness|blocker debt remains within the current architecture' docs/plans/...` -> exit `0`
- `git rev-parse --abbrev-ref HEAD && git rev-parse HEAD && git merge-base HEAD codex/automatic-recursive-type-inference` -> exit `0` (`orchestrator/round-211-repair-same-wrapper-nested-forall-across-authoritative-annotation-and-post-annotation-handoff-seams`, `302a9ef149af3ce36d5f63538f7124dcbb38cfc7`, `302a9ef149af3ce36d5f63538f7124dcbb38cfc7`)
- `git status --short --untracked-files=all` -> exit `0`
- `git diff --stat && printf '\n---\n' && git diff --name-only` -> exit `0` (`10` tracked files changed; implementation-owned diff remains the inherited baseline files only)
- `git diff --check` -> exit `0`
- `git diff --name-only -- src/MLF/Elab/Elaborate/Annotation.hs src/MLF/Elab/Legacy.hs src/MLF/Elab/Elaborate/Algebra.hs src/MLF/Elab/TermClosure.hs test/ElaborationSpec.hs test/PipelineSpec.hs test/Research/P5ClearBoundarySpec.hs` -> exit `0` (lists only `Algebra.hs`, `Annotation.hs`, `Legacy.hs`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, `test/Research/P5ClearBoundarySpec.hs`)
- `git diff --name-only -- src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs` -> exit `0` (no output)
- `git diff --name-only -- src/MLF/Elab/TermClosure.hs && git diff --unified=0 -- src/MLF/Elab/TermClosure.hs` -> exit `0` (no output)
- `git diff --name-only -- orchestrator/rounds/round-208 orchestrator/rounds/round-209 orchestrator/rounds/round-210 orchestrator/rounds/round-094 orchestrator/rounds/round-095 orchestrator/rounds/round-096 orchestrator/rounds/round-097 orchestrator/rounds/round-098 orchestrator/rounds/round-001 orchestrator/rounds/round-002 orchestrator/rounds/round-003 orchestrator/rounds/round-004 orchestrator/rounds/round-005 orchestrator/rounds/round-006 orchestrator/rounds/round-007 orchestrator/rounds/round-008 orchestrator/rounds/round-009 orchestrator/rounds/round-010` -> exit `0` (no output)
- `git diff --name-only -- orchestrator/roadmaps` -> exit `0` (no tracked roadmap drift)
- `./scripts/thesis-conformance-gate.sh` -> exit `1` (`A6 parity regressions` fails on `dual annotated coercion consumers fail fast on unresolved non-root OpWeaken` with `TCExpectedArrow`)
- `tail -n 40 /tmp/round211-full-gate.log` -> exit `0` (tail confirms `1341 examples, 26 failures`, plus the remaining nested-let `TCLetTypeMismatch` failures)
- `python3 - <<'PY' ... summarize /tmp/round211-full-gate.log ... PY` -> exit `0` (`FULL_GATE_SUMMARY ('1341', '26')`)
- `nl -ba orchestrator/rounds/round-211/implementation-notes.md | sed -n '251,320p'` -> exit `0`
- `test -f orchestrator/rounds/round-211/review-record.json; echo $?` -> exit `0` (`1`)
- `find orchestrator/rounds/round-211 -maxdepth 2 -type f | sort` -> exit `0`

Reused implementer evidence from `orchestrator/rounds/round-211/implementation-notes.md`:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'` -> exit `0` (`3` examples, `0` failures)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'` -> exit `1` (`10` examples, `8` failures; remaining failures report `TCExpectedArrow`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "dual annotated coercion consumers fail fast on unresolved non-root OpWeaken"'` -> exit `1` (`2` examples, `2` failures; both report `TCExpectedArrow`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "pipeline fails fast for nested-let when only expansion-derived instantiation remains"'` -> exit `1` (still `TCLetTypeMismatch` on the restored baseline)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "full pipeline fails fast post-boundary-enforcement for: nested-let"'` -> exit `1` (still `TCLetTypeMismatch` on the restored baseline)
- `set -o pipefail; (cabal build all && cabal test) 2>&1 | tee /tmp/round211-full-gate.log` -> exit `1` (`1341` examples, `26` failures)
- Temporary reverted experiment: `cabal test mlf2-test --test-show-details=direct --test-options='--match "pipeline fails fast for nested-let when only expansion-derived instantiation remains"'` -> exit `1`, but with false success `Expected strict failure, got type: TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))`

## Baseline Checks

1. **Roadmap lineage, pointer, and preserved-history consistency**: PASS
   - `orchestrator/state.json`, `selection.md`, and the live pointer stubs agree on `roadmap_id = 2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`, `roadmap_revision = rev-009`, `roadmap_dir = orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-009`, `milestone_id = milestone-2`, `direction_id = direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`, and `extracted_item_id = continue-round-211-selected-same-wrapper-nested-forall-through-term-closure-alias-boundary-and-adjacent-authoritative-witness-instantiation-seam`.
   - `review-record.json` is absent, which is correct for a non-finalized rejected attempt.
   - Historical continuity across `round-001` through `round-098` remains intact, `round-094` through `round-098` remain authoritative predecessor evidence only, and the current diff does not touch `round-208` through `round-210` or the bounded earlier history probe set.
   - Accepted strategic continuity remains unchanged: the cited March strategic artifacts still keep `non-cyclic-graph = unknown`, `bounded subset only`, `continue within the current architecture`, and the blocker-debt / fail-closed posture.

2. **Diff hygiene**: PASS
   - `git diff --check` returned no output.

3. **Build and test gate for production/test changes**: FAIL
   - The round touches `src/` and `test/`, so the full gate is mandatory.
   - Reused implementation evidence still ends at `1341` examples and `26` failures on the restored baseline.

4. **Thesis conformance gate for thesis-facing changes**: FAIL
   - Reviewer rerun of `./scripts/thesis-conformance-gate.sh` exits `1`.
   - The gate fails in the explicitly required A6 parity surface at `dual annotated coercion consumers fail fast on unresolved non-root OpWeaken`, which still reports `TCExpectedArrow`.

5. **Broader-positive boundary discipline**: PASS
   - The implementation-owned final diff stays inside the inherited round-211 baseline files and does not touch pipeline/public/fallback anchors.
   - No cyclic search, multi-SCC widening, equi-recursive reinterpretation, fallback rescue, or second interface appears in the diff.
   - This is a bounded rejection because the admitted rev-009 seam was not kept, not because the attempt widened outside the authorized architecture.

6. **Authoritative-entrypoint discipline**: PASS
   - Reused implementation evidence still shows the selected same-wrapper nested-`forall` packet green on both authoritative entrypoints (`3` examples, `0` failures).

7. **Worker-plan integrity when fan-out is used**: NOT APPLICABLE
   - `worker_mode` is `none`; no `worker-plan.json` exists or is required.

## Milestone-2 Task-Specific Checks

1. **Diff stays inside the rev-009 writable slice**: PASS
   - The final implementation-owned diff is limited to the preserved baseline files already live on round-211.
   - `src/MLF/Elab/TermClosure.hs` is lawfully inside the slice but remains untouched.

2. **Preserved round-211 baseline continuity**: PASS
   - The current diff preserves the already-cleared `Annotation.hs` / `Legacy.hs` / `Algebra.hs` baseline and the focused test surfaces.
   - Read-only anchors remain untouched.

3. **Inherited rev-008 continuation remains in place and keeps the protected positives green**: PASS
   - Reused implementation evidence still shows the selected packet green on both authoritative entrypoints.
   - The inherited round-211 baseline is preserved rather than discarded.

4. **Admitted rev-009 continuation matches the blocker proof and lands at the newly admitted seam**: FAIL
   - The final diff leaves `src/MLF/Elab/TermClosure.hs` untouched.
   - `implementation-notes.md` records that the temporary `TermClosure.hs` repair was reverted because it replaced the nested-let strict failure with false success `forall a. a -> a`.
   - The admitted rev-009 continuation therefore did not land any production repair in the newly admitted seam.

5. **Annotation/Legacy/Algebra edits stay baseline-only or mechanical companions**: PASS
   - The final diff preserves the inherited round-211 baseline in `Annotation.hs`, `Legacy.hs`, and `Algebra.hs`; there is no reopened annotation redesign or new Algebra relitigation kept in the final state.

6. **The remaining fail-fast cluster is explicitly rechecked**: FAIL
   - Reused implementation evidence still shows `BUG-2026-02-06-002` failing with `TCExpectedArrow`.
   - Reused implementation evidence and the thesis gate both show the A6 dual-coercion fail-fast pair still failing with `TCExpectedArrow`.
   - Reused implementation evidence still shows both nested-let fail-fast / invariant probes failing with alias-side `TCLetTypeMismatch`.

7. **Read-only continuity anchors remain untouched**: PASS
   - `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Pipeline.hs`, `src-public/MLF/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, and `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` remain untouched.

8. **Required full `cabal build all && cabal test` gate**: FAIL
   - The reused full gate still exits `1` with `1341` examples and `26` failures.

9. **No forbidden widening**: PASS
   - The rejection is due to an unlanded admitted seam and failing required gates, not because the diff widened into forbidden architecture.

## Diff vs Plan

1. **Reconfirm the preserved baseline and reproduce the remaining cluster before editing**: PASS
   - `implementation-notes.md` records the restored baseline status honestly: selected packet green, remaining fail-fast cluster unchanged, and full gate still red.

2. **Repair the alias-side closure seam in `src/MLF/Elab/TermClosure.hs`**: FAIL
   - No repair was kept.
   - The final diff leaves `TermClosure.hs` untouched, so the newly admitted rev-009 seam did not land.

3. **Apply only the immediately adjacent companion shaping if the closure repair still needs it**: FAIL
   - The temporary experiment proved that closure-only repair is insufficient because it turns the nested-let strict failure into false success.
   - That proves the next needed seam is downstream of the bounded rev-009 closure repair; it was not admitted or landed here.

4. **Touch the authorized round-owned tests only if the guard surface changes honestly**: PASS
   - No extra test-surface widening was introduced beyond the inherited baseline files already live on round-211.

5. **Run the bounded verification gate and prove the continuation green**: FAIL
   - The full gate remains red (`26` failures).
   - The thesis conformance gate also fails on the remaining A6 parity regression.

## Evidence Summary

- The final implementation state is the preserved round-211 baseline, not a landed rev-009 continuation. `git diff --name-only -- src/MLF/Elab/TermClosure.hs` and `git diff --unified=0 -- src/MLF/Elab/TermClosure.hs` both return no output.
- `implementation-notes.md` explicitly records the rev-009 outcome:
  - no production landing was kept;
  - the temporary `TermClosure.hs` / `Algebra.hs` experiment was reverted;
  - `src/MLF/Elab/TermClosure.hs` is untouched in the final diff; and
  - the full gate still fails with `26` failures.
- The same notes also prove why a same-mechanism closure-only retry is not enough: removing the alias-side nested-let mismatch by direct closure repair turned the nested-let fail-fast probe into false success `forall a. a -> a`, which points to an additional downstream post-closure authoritative classification / witness seam outside the bounded rev-009 repair.
- Reviewer verification independently confirms the remaining blocker:
  - `./scripts/thesis-conformance-gate.sh` fails on the A6 parity regression with `TCExpectedArrow`;
  - `/tmp/round211-full-gate.log` still ends at `1341 examples, 26 failures`; and
  - the tail still contains the nested-let `TCLetTypeMismatch` failures.
- The diff remains inside the inherited architecture boundary and preserves the selected packet baseline, but milestone-2 approval still requires a kept rev-009 repair plus passing required gates. This attempt has neither.

## Decision

**REJECTED: rev-009 admitted `TermClosure.hs` as the next exact continuation seam, but the final diff leaves `TermClosure.hs` untouched and restores only the inherited round-211 baseline, while the reused full gate still fails with `26` failures and the reviewer rerun of the thesis-conformance gate still fails on the remaining A6 parity regression.**

The lawful review result is `rejected + retry`. `review-record.json` is intentionally omitted because this attempt does not finalize. The next controller move should be recovery / `update-roadmap`, not another unchanged rev-009 implementation retry.
