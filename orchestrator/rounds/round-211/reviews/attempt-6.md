# Round 211 Attempt 6 Review

Decision: **REJECTED: the current rev-011 working state preserves the selected same-wrapper nested-`forall` packet and repairs `BUG-2026-02-17-002`, but it still fails both required milestone-2 gates and now regresses broader let-polymorphism / representative-corpus behavior outside the selected packet.**

## Retry Contract

- Attempt verdict: rejected
- Stage action: update-roadmap
- Retry reason: the admitted rev-011 downstream `ALamF` / `AAppF` consumer-recovery seam plus immediate authoritative companion is not enough to produce a landable round. Review reran the required focused probes and both mandatory gates. The selected packet, checked-authoritative parity, `BUG-2026-02-06-002`, retained-child exact packet, `BUG-2026-02-17-002`, and the non-local proxy-wrapper `g g` control are green, but `dual annotated coercion consumers fail fast on unresolved non-root OpWeaken` still fails in both typecheck/reduce with `TCArgumentMismatch`, both nested-let fail-fast controls are still red with `TCInstantiationError`, and the full gate regresses classic let-polymorphism / parity rows (`runtime snapshot rebuild stays stable across representative corpus`, `redirected let-use sites keep polymorphic schemes`, `elaborates dual instantiation in application`, `let id = (\\x. x) in id id should have type ∀a. a -> a`, `Phi alignment` let-poly, `AlignmentInvariant` let-poly, frozen parity baseline, plus adjacent bug-matrix rows).
- Fix hypothesis: do not run another same-revision rev-011 implementation retry. Publish a successor roadmap revision that honestly reopens the broader authoritative application / let-polymorphism handoff now named by the surviving evidence: the current downstream consumer repair and immediate authoritative companion can keep the selected packet green, but they still force non-landable typechecking in the A6 dual-consumer lane and monomorphizing regressions across representative let-polymorphism/parity rows. Keep `src/MLF/Elab/TermClosure.hs`, pipeline/public/fallback surfaces, cyclic widening, equi-recursive reasoning, and second-interface work closed unless a later accepted revision explicitly reopens them.

## Commands Run

- `python3 - <<'PY' ... selection/state lineage check ... PY` -> exit `0`
  - output: `missing=[]`, `state_stage=review`, `controller_stage=dispatch-rounds`, `roadmap_item_id_present=False`
- `python3 - <<'PY' ... pointer stub field check ... PY` -> exit `0`
  - output:
    - `roadmap.md True True True`
    - `verification.md True True True`
    - `retry-subloop.md True True True`
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211 diff --name-only -- orchestrator/rounds/round-208 orchestrator/rounds/round-209 orchestrator/rounds/round-210` -> exit `0`
  - output: none
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211 diff --check` -> exit `0`
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211 diff --name-only -- src/MLF/Elab/Elaborate/Algebra.hs src/MLF/Elab/Elaborate/Annotation.hs src/MLF/Elab/Legacy.hs test/ElaborationSpec.hs test/PipelineSpec.hs test/Research/P5ClearBoundarySpec.hs` -> exit `0`
  - output:
    - `src/MLF/Elab/Elaborate/Algebra.hs`
    - `src/MLF/Elab/Elaborate/Annotation.hs`
    - `src/MLF/Elab/Legacy.hs`
    - `test/ElaborationSpec.hs`
    - `test/PipelineSpec.hs`
    - `test/Research/P5ClearBoundarySpec.hs`
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211 diff --name-only -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs` -> exit `0`
  - output: none
- `rg -n "collapseTrivialBoundAlias|singleAppInstArg|recoverSingleAppArg|identityLikeMuArgInst|funInstByFunType|funInstRecovered|argInstFromFun|fAppRecovered" /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211/src/MLF/Elab/Elaborate/Algebra.hs` -> exit `0`
  - output highlights the admitted downstream consumer locals around lines `91`, `117`, `173`, `177`, `197`, `223`, `239`, `285`, `333`, `689`
- `rg -n "reifyInst|authoritativeTargetType|inferAuthoritativeInstArgs|reifyTraceBinderInstArgs|instNeedsAuthoritativeRefinement|instSeqApps" /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211/src/MLF/Elab/Elaborate/Annotation.hs` -> exit `0`
  - output highlights the admitted `reifyInst` helper surface around lines `261`, `334`, `343`, `456`, `524`, `536`, `544`, `557`, `572`
- `rg -n "expInstantiateArgsToInstNoFallback|instAppsFromTypes" /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211/src/MLF/Elab/Legacy.hs` -> exit `0`
  - output highlights only the admitted companion functions at lines `46`, `51`, `53`, `76`, `80`, `81`
- `test -f /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211/orchestrator/rounds/round-211/review-record.json; echo $?` -> exit `0`
  - output: `1`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'` -> exit `0`
  - `3` examples, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'` -> exit `0`
  - `4` examples, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborates polymorphic instantiation"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborates usage of polymorphic let"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "id id should have type"'` -> exit `1`
  - output: `PipelineTypeCheckError (TCArgumentMismatch (TArrow (TVar "t15") (TVar "t15")) (TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))))`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "id y should have type"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "explicit forall coercion in let RHS elaborates through use-site application"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'` -> exit `0`
  - `10` examples, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet"'` -> exit `0`
  - `2` examples, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "A6 parity: bounded alias + coercion-heavy path agrees across unchecked, checked, and typeCheck"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "applied bounded-coercion path succeeds in unchecked and checked pipelines"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "non-local proxy wrapper g g fails with TCArgumentMismatch (correct semantic error)"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "pipeline fails fast for nested-let when only expansion-derived instantiation remains"'` -> exit `1`
  - output: `PipelineTypeCheckError (TCInstantiationError ... "InstElim expects forall, got (t17 -> t17) -> t17 -> t17")`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "full pipeline fails fast post-boundary-enforcement for: nested-let"'` -> exit `1`
  - output: the same `TCInstantiationError`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "dual annotated coercion consumers fail fast on unresolved non-root OpWeaken"'` -> exit `1`
  - `2` examples, `2` failures; both report `TCArgumentMismatch (TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))) (TArrow (TVar "t101") (TVar "t101"))`
- `set -o pipefail; ./scripts/thesis-conformance-gate.sh 2>&1 | tee /tmp/round211-attempt6-thesis-gate-review.log` -> exit `1`
  - thesis obligations ledger, claim validation, and Phi/Omega matrix all pass
  - A6 parity regressions fail at `dual annotated coercion consumers fail fast on unresolved non-root OpWeaken` with `TCArgumentMismatch`
- `set -o pipefail; (cabal build all && cabal test) > /tmp/round211-attempt6-full-gate-review.log 2>&1; rc=$?; tail -n 160 /tmp/round211-attempt6-full-gate-review.log; exit $rc` -> exit `1`
  - full gate ends at `1341` examples, `16` failures
  - surviving failures include:
    - `test/PipelineSpec.hs`: representative corpus runtime snapshot rebuild, redirected let-use sites keep polymorphic schemes
    - `test/TypeCheckSpec.hs` and `test/ReduceSpec.hs`: A6 dual annotated coercion pair
    - `test/ElaborationSpec.hs`: `elaborates dual instantiation in application`, `let id = (\\x. x) in id id should have type ∀a. a -> a`, `BUG-2026-02-06-001`, `BUG-002-V2`, `BUG-004-V1`, `BUG-004-V4`
    - `test/FrozenParitySpec.hs`: frozen parity artifact baseline
    - `test/Phi/AlignmentSpec.hs`: let-poly row and nested-let fail-fast row
    - `test/AlignmentInvariantSpec.hs`: let-poly row and nested-let fail-fast row

## Baseline Checks

1. **Roadmap lineage, pointer, and preserved-history consistency**: PASS
   - `selection.md` matches active `roadmap_id`, `roadmap_revision`, `roadmap_dir`, `milestone_id`, `direction_id`, and `extracted_item_id`.
   - `roadmap_item_id` is absent.
   - `orchestrator/roadmap.md`, `orchestrator/verification.md`, and `orchestrator/retry-subloop.md` all still point at the active rev-011 bundle.
   - `round-208`, `round-209`, and `round-210` remain untouched predecessor evidence.
   - `review-record.json` is absent, which is correct for a rejected non-finalized attempt.

2. **Diff hygiene**: PASS
   - `git diff --check` returned no output.

3. **Build and test gate for production/test changes**: FAIL
   - The round touches `src/` and `test/`.
   - `cabal build all && cabal test` still fails with `16` failures.

4. **Thesis conformance gate**: FAIL
   - `./scripts/thesis-conformance-gate.sh` still fails in `A6 parity regressions` on the dual-annotated consumer row with `TCArgumentMismatch`.

5. **Broader-positive boundary discipline**: FAIL
   - File scope stays within the admitted round-owned production/test files and read-only anchors remain untouched.
   - The resulting behavior is not packet-bounded: it regresses classic let-polymorphism / representative-corpus rows that the rev-011 continuation did not honestly own.

6. **Authoritative-entrypoint discipline**: PASS
   - The selected same-wrapper nested-`forall` packet still succeeds on both authoritative entrypoints.

7. **Worker-plan integrity when fan-out is used**: NOT APPLICABLE
   - `worker_mode` is `none`.

## Milestone-2 Task-Specific Checks

1. **Writable-slice discipline**: PASS
   - The round diff remains inside the admitted milestone-2 files.
   - `TermClosure.hs`, pipeline/public surfaces, and fallback surfaces stay untouched.
   - `Annotation.hs` edits stay inside admitted `reifyInst` helper territory and `Legacy.hs` stays inside `expInstantiateArgsToInstNoFallback` / `instAppsFromTypes`.

2. **Preserved baseline wins**: MIXED
   - PASS: selected packet, checked-authoritative parity, `BUG-2026-02-06-002`, retained-child exact packet, `BUG-2026-02-17-002`, and the non-local proxy-wrapper `g g` control are green.
   - FAIL: classic let-polymorphism / representative-corpus protections are no longer all green; `id id should have type` fails directly, and the full gate shows broader let-poly regressions.

3. **Admitted downstream consumer / immediate companion seam**: PASS
   - Controller-visible edits in `Algebra.hs` hit the named downstream helper/consumer locals, and the companion work in `Annotation.hs` / `Legacy.hs` stays in the admitted authoritative-instantiation helpers.

4. **Remaining fail-fast cluster explicitly rechecked**: FAIL
   - `dual annotated coercion consumers fail fast on unresolved non-root OpWeaken` remains red with `TCArgumentMismatch`.
   - Both nested-let fail-fast probes remain red with `TCInstantiationError`.
   - `BUG-2026-02-17-002` is green.
   - `g g` remains green on the corrected `TCArgumentMismatch` control.

5. **Nested-let probes do not become false success**: PASS
   - The nested-let rows still fail; they do not become false-success `forall a. a -> a`.

6. **Required milestone-2 gates**: FAIL
   - The thesis gate is red.
   - The full repo gate is red.

## Diff vs Plan

1. **Step 1: reconfirm the preserved baseline and active blocker**: PASS
   - Review reproduced the preserved packet/parity wins and the surviving blocker cluster.

2. **Step 2: downstream `ALamF` / `AAppF` consumer recovery in `Algebra.hs`**: PASS
   - The visible `Algebra.hs` edits target the named downstream helper/consumer locals.
   - The selected packet, `BUG-2026-02-17-002`, and `g g` controls show that this seam materially changed behavior.

3. **Step 3: immediate authoritative-instantiation companion only if needed**: PASS
   - Review evidence shows the companion was used and stayed inside the admitted helper surfaces.
   - `BUG-2026-02-17-002` is now green, which justifies that companion.

4. **Step 4: touch only round-owned expectations that honestly moved**: PASS
   - Test edits stay inside the admitted round-owned files.

5. **Step 5: prove the rev-011 continuation green**: FAIL
   - The focused blocker cluster is not fully green.
   - `./scripts/thesis-conformance-gate.sh` fails.
   - `cabal build all && cabal test` fails with `16` failures and broader let-polymorphism/parity regressions.

## Evidence Summary

- The rev-011 continuation is real but incomplete. It keeps the selected same-wrapper nested-`forall` packet green on both authoritative entrypoints and also repairs `BUG-2026-02-17-002`.
- The surviving hard blocker moved to a clearer shape:
  - A6 dual-annotated consumers still fail at `TCArgumentMismatch`
  - both nested-let fail-fast rows are still red, now at `TCInstantiationError`
  - broader let-polymorphism / representative-corpus rows regress across pipeline, elaboration, Phi/alignment, and frozen parity surfaces
- Those regressions are outside the round-owned packet claim and make the current working state non-landable even though the admitted rev-011 seam itself was exercised honestly.

## Decision

**REJECTED: the current rev-011 working state is not an honest milestone-2 landing because the required thesis gate and full repo gate remain red, and the same working diff now regresses broader let-polymorphism / representative-corpus behavior outside the selected packet.**

The next lawful controller move is `update-roadmap`, not another same-revision implementation retry. `review-record.json` remains intentionally absent because this attempt does not finalize.
