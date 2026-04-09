# Round 211 Attempt 8 Review

Decision: **REJECTED: the current rev-013 attempt is not approvable because the required thesis/full gates still fail, but unlike attempt 7 it does land a materially new within-seam `Algebra.hs` repair that restores the direct let-polymorphism/self-application lane and narrows the live blocker cluster enough for one more lawful same-revision retry.**

## Retry Contract

- Implemented stage result: kept one new within-seam `src/MLF/Elab/Elaborate/Algebra.hs` repair on top of the preserved `round-211` baseline; the direct let-polymorphism/self-application lane is green again (`runtime snapshot rebuild stays stable across representative corpus`, `redirected let-use sites keep polymorphic schemes`, `elaborates dual instantiation in application`, and the `let id = (\x. x) in id id` row all pass), while the selected packet and other protected wins remain green.
- Attempt verdict: rejected
- Stage action: retry
- Retry reason: review reran lineage/pointer checks, diff hygiene, the protected-win and blocker probes, `./scripts/thesis-conformance-gate.sh`, and `cabal build all && cabal test`. The active `rev-013` pointers now match in both the parent workspace and canonical round worktree, the selected packet/protected wins stay green, and the full gate improved from attempt 7's recorded `1341` examples / `16` failures to `1341` examples / `9` failures. But approval is still blocked because `./scripts/thesis-conformance-gate.sh` fails at `A6 parity regressions`, and the full gate still fails on the surviving admitted blocker family: A6, the nested-let fail-fast rows, `BUG-2026-02-06-001`, `BUG-002-V2`, `BUG-004-V1`, `BUG-004-V4`, and frozen parity drift.
- Fix hypothesis: continue the same rev-013 round within the admitted broader authoritative application / let-polymorphism handoff in `Algebra.hs`; if stepwise evidence proves it necessary, use only the already-admitted immediate authoritative companion in `Annotation.hs` / `Legacy.hs`. The next repair should target the surviving A6 `TCArgumentMismatch`, the nested-let `InstElim expects forall` fail-fast lane, the duplicated-forall alias lane in `BUG-002-V2`, the annotated-alias mismatch rows in `BUG-2026-02-06-001`, `BUG-004-V1`, and `BUG-004-V4`, and the frozen parity naming drift, while preserving the repaired direct self-application lane and the selected packet.

## Commands Run

- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211 diff --check` -> exit `0`
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211 diff --name-only` -> exit `0`
  - output:
    - `orchestrator/retry-subloop.md`
    - `orchestrator/roadmap.md`
    - `orchestrator/state.json`
    - `orchestrator/verification.md`
    - `src/MLF/Elab/Elaborate/Algebra.hs`
    - `src/MLF/Elab/Elaborate/Annotation.hs`
    - `src/MLF/Elab/Legacy.hs`
    - `test/ElaborationSpec.hs`
    - `test/PipelineSpec.hs`
    - `test/Research/P5ClearBoundarySpec.hs`
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211 diff --name-only -- orchestrator/rounds/round-208 orchestrator/rounds/round-209 orchestrator/rounds/round-210 orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-012` -> exit `0`
  - output: none
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211 diff --name-only -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs` -> exit `0`
  - output: none
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211 diff --unified=0 -- src/MLF/Elab/Elaborate/Algebra.hs src/MLF/Elab/Elaborate/Annotation.hs src/MLF/Elab/Legacy.hs test/ElaborationSpec.hs test/PipelineSpec.hs test/Research/P5ClearBoundarySpec.hs` -> exit `0`
  - output: the preserved round diff remains inside the admitted rev-013 production/test slice; no diff appears in the closed read-only anchors.
- `sed -n '1,40p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md` -> exit `0`
  - output: pointer stub names `roadmap_revision: rev-013`
- `sed -n '1,40p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md` -> exit `0`
  - output: pointer stub names `roadmap_revision: rev-013`
- `sed -n '1,40p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md` -> exit `0`
  - output: pointer stub names `roadmap_revision: rev-013`
- `sed -n '1,40p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211/orchestrator/roadmap.md` -> exit `0`
  - output: pointer stub names `roadmap_revision: rev-013`
- `sed -n '1,40p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211/orchestrator/verification.md` -> exit `0`
  - output: pointer stub names `roadmap_revision: rev-013`
- `sed -n '1,40p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211/orchestrator/retry-subloop.md` -> exit `0`
  - output: pointer stub names `roadmap_revision: rev-013`
- `rg -n "roadmap_item_id" /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211/orchestrator/rounds/round-211/selection.md` -> exit `1`
  - output: none (`roadmap_item_id` absent, as required)
- `if [ -f /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-211/orchestrator/rounds/round-211/review-record.json ]; then echo present; else echo absent; fi` -> exit `0`
  - output: `absent`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'` -> exit `0`
  - `3` examples, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'` -> exit `0`
  - `4` examples, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'` -> exit `0`
  - `10` examples, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet"'` -> exit `0`
  - `2` examples, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "non-local proxy wrapper g g fails with TCArgumentMismatch (correct semantic error)"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "runtime snapshot rebuild stays stable across representative corpus"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "redirected let-use sites keep polymorphic schemes"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborates dual instantiation in application"'` -> exit `0`
  - `1` example, `0` failures
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "let id ="'` -> exit `0`
  - `4` examples, `0` failures
  - output includes `let id = (\x. x) in id id should have type ∀a. a -> a [✔]`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "dual annotated coercion consumers fail fast on unresolved non-root OpWeaken"'` -> exit `1`
  - `2` examples, `2` failures
  - both failures report `TCArgumentMismatch (TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))) (TArrow (TVar "t101") (TVar "t101"))`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "pipeline fails fast for nested-let when only expansion-derived instantiation remains"'` -> exit `1`
  - `1` example, `1` failure
  - failure reports `TCInstantiationError ... "InstElim expects forall, got (t17 -> t17) -> t17 -> t17"`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "full pipeline fails fast post-boundary-enforcement for: nested-let"'` -> exit `1`
  - `1` example, `1` failure
  - failure reports the same `TCInstantiationError`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi alignment"'` -> exit `1`
  - `7` examples, `1` failure
  - the only remaining failure is the same nested-let `TCInstantiationError`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Thesis alignment invariants"'` -> exit `1`
  - `21` examples, `1` failure
  - the only remaining failure is the same nested-let `TCInstantiationError`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'` -> exit `1`
  - `1` example, `1` failure
  - failure shows frozen parity drift from `∀(a ⩾ ⊥) a -> a -> a` to `∀(t32 ⩾ ⊥) t32 -> t32 -> t32`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-001"'` -> exit `1`
  - `2` examples, `1` failure
  - failure reports `TCArgumentMismatch (TArrow (TVar "t51") (TVar "t51")) (TArrow (TVar "t50") (TVar "t50"))`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V2"'` -> exit `1`
  - `1` example, `1` failure
  - failure reports `TCLetTypeMismatch` with duplicated nested `forall`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V1"'` -> exit `1`
  - `1` example, `1` failure
  - failure reports `TCArgumentMismatch (TArrow (TVar "t51") (TVar "t51")) (TArrow (TVar "t50") (TVar "t50"))`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V4"'` -> exit `1`
  - `1` example, `1` failure
  - failure reports `TCArgumentMismatch (TArrow (TVar "t69") (TVar "t69")) (TArrow (TVar "t68") (TVar "t68"))`
- `./scripts/thesis-conformance-gate.sh` -> exit `1`
  - thesis obligations and thesis claims both pass
  - `Phi/Omega translatability matrix rows` passes with `32` examples, `0` failures
  - gate fails at `A6 parity regressions`
  - failing row: `dual annotated coercion consumers fail fast on unresolved non-root OpWeaken`
  - failure reports `TCArgumentMismatch (TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))) (TArrow (TVar "t101") (TVar "t101"))`
- `cabal build all && cabal test` -> exit `1`
  - final summary: `1341` examples, `9` failures
  - surviving failures are:
    - `test/TypeCheckSpec.hs`: A6 dual annotated coercion consumer
    - `test/ReduceSpec.hs`: A6 dual annotated coercion consumer
    - `test/ElaborationSpec.hs`: `BUG-2026-02-06-001`, `BUG-002-V2`, `BUG-004-V1`, `BUG-004-V4`
    - `test/FrozenParitySpec.hs`: frozen parity artifact baseline
    - `test/Phi/AlignmentSpec.hs`: nested-let fail-fast row
    - `test/AlignmentInvariantSpec.hs`: nested-let fail-fast row

## Baseline Checks

1. **Roadmap lineage, pointer, and preserved-history consistency**: PASS
   - `selection.md` matches active `roadmap_id`, `roadmap_revision`, `roadmap_dir`, `milestone_id`, `direction_id`, and `extracted_item_id`.
   - `roadmap_item_id` is absent.
   - Both parent-workspace and canonical round-worktree pointer stubs now point at active `rev-013`.
   - `round-208`, `round-209`, and `round-210` remain untouched predecessor evidence.
   - `review-record.json` remains absent, which is correct for a rejected non-finalized attempt.
   - The same `round-211` branch/worktree remains live and still carries the preserved baseline diff in the admitted round-owned files.

2. **Diff hygiene**: PASS
   - `git diff --check` returned no output.

3. **Build and test gate for production/test changes**: FAIL
   - The round touches `src/` and `test/`.
   - `cabal build all && cabal test` still fails with `9` failures.

4. **Thesis conformance gate**: FAIL
   - `./scripts/thesis-conformance-gate.sh` still fails at `A6 parity regressions`.

5. **Broader-positive boundary discipline**: PASS
   - The preserved round diff stays inside the admitted rev-013 files.
   - No diff appears in `TermClosure.hs`, pipeline/public surfaces, fallback surfaces, cyclic/equi-recursive surfaces, or a second interface.
   - The retained-child clear-boundary lane remains predecessor truth; the current attempt does not silently widen it into whole-frontier closure.

6. **Authoritative-entrypoint discipline**: PASS
   - The selected same-wrapper nested-`forall` packet remains green on both `runPipelineElab` and `runPipelineElabChecked`.

7. **Worker-plan integrity when fan-out is used**: NOT APPLICABLE
   - `worker_mode` is `none`.

## Milestone-2 Task-Specific Checks

1. **Writable-slice discipline**: PASS
   - The current diff remains inside the admitted rev-013 slice.
   - No diff appears in `src/MLF/Elab/TermClosure.hs`, `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Pipeline.hs`, `src-public/MLF/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, or `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`.

2. **Preserved baseline wins**: PASS
   - The selected packet, checked-authoritative representative parity, `BUG-2026-02-06-002`, the retained-child exact packet, `BUG-2026-02-17-002`, and the non-local proxy-wrapper `g g` control are all green.

3. **Admitted broader handoff continuation**: PASS, but incomplete
   - Fresh evidence shows a new landed within-seam repair in `Algebra.hs`: the direct let-polymorphism/self-application lane is green again.
   - The attempt does not widen beyond the admitted broader authoritative application / let-polymorphism handoff and its preserved immediate companion baseline.
   - The continuation is not yet complete because the surviving A6 / nested-let / alias-variant failures remain.

4. **Remaining blocker cluster explicitly rechecked**: PASS, with mixed outcomes
   - Repaired/green now:
     - `runtime snapshot rebuild stays stable across representative corpus`
     - `redirected let-use sites keep polymorphic schemes`
     - `elaborates dual instantiation in application`
     - `let id = (\x. x) in id id should have type ∀a. a -> a`
   - Still red:
     - `dual annotated coercion consumers fail fast on unresolved non-root OpWeaken`
     - `pipeline fails fast for nested-let when only expansion-derived instantiation remains`
     - `full pipeline fails fast post-boundary-enforcement for: nested-let`
     - `BUG-2026-02-06-001`
     - `BUG-002-V2`
     - `BUG-004-V1`
     - `BUG-004-V4`
     - frozen parity baseline

5. **Direct let-polymorphism repair is materially real, not just a failure-shape shuffle**: PASS
   - Compared with attempt 7's recorded evidence, the repaired direct let-polymorphism rows are now genuinely green rather than merely changing to a different visible mismatch.
   - The full-gate failure count drops from `16` to `9`.

6. **Nested-let probes do not become false success**: PASS
   - Both nested-let probes remain fail-fast rows and do not become a false-success `forall a. a -> a`.

7. **Pointer stubs stay aligned with active rev-013 bundle**: PASS
   - Parent-workspace and canonical round-worktree stubs all name `rev-013`.

8. **Required milestone-2 gates**: FAIL
   - `./scripts/thesis-conformance-gate.sh` is red.
   - `cabal build all && cabal test` is red.

## Diff vs Plan

1. **Step 1: reconfirm the preserved baseline and exact rev-013 distinction target**: PASS
   - Review reproduced the protected wins and the surviving blocker cluster on the canonical round worktree.
   - Pointer stubs in both workspaces now match active `rev-013`.

2. **Step 2: repair only the `Algebra.hs` AAppF / ALetF handoff locals**: MATERIALLY ADVANCED, NOT COMPLETE
   - Fresh focused evidence confirms a real within-seam `Algebra.hs` landing: direct self-application and representative let-polymorphism rows are green again.
   - The admitted handoff is still incomplete because A6, nested-let, the alias/annotated bug variants, and frozen parity remain red.

3. **Step 3: admit the immediate authoritative companion only if needed**: NOT NEWLY ADVANCED
   - The preserved baseline still carries companion changes in `Annotation.hs` / `Legacy.hs`, but this attempt's new landed evidence is the within-seam `Algebra.hs` repair rather than a fresh new companion widening.

4. **Step 4: update only round-owned expectations that honestly move**: PASS
   - The preserved selected-packet expectation updates are now honest: the selected same-wrapper nested-`forall` packet succeeds on both authoritative entrypoints and the new tests pass.

5. **Step 5: serial final verification**: FAIL
   - Focused protected wins and repaired let-polymorphism rows are green.
   - `./scripts/thesis-conformance-gate.sh` fails on A6.
   - `cabal build all && cabal test` fails with `9` failures.

## Evidence Summary

- The current rev-013 attempt lands a real within-seam repair in `Algebra.hs`. This is not just blocker documentation or a reverted experiment.
- The protected baseline wins remain green, including the selected same-wrapper nested-`forall` packet on both authoritative entrypoints.
- Direct let-polymorphism/self-application is materially improved versus attempt 7:
  - `runtime snapshot rebuild stays stable across representative corpus` is now green
  - `redirected let-use sites keep polymorphic schemes` is now green
  - `elaborates dual instantiation in application` is now green
  - the `let id = (\x. x) in id id` row is now green
  - full-gate failures drop from `16` to `9`
- The remaining red set is still the same admitted blocker family, not a reopened outer surface:
  - A6 dual annotated coercion consumer (`TCArgumentMismatch`)
  - nested-let fail-fast on both Phi/alignment and thesis-alignment lanes (`TCInstantiationError ... "InstElim expects forall"`)
  - annotated/alias bug variants `BUG-2026-02-06-001`, `BUG-002-V2`, `BUG-004-V1`, `BUG-004-V4`
  - frozen parity artifact drift
- Because the attempt both preserves the current revision boundary and materially narrows the blocker, the retry contract supports one more same-round return to `plan`. Approval is still impossible until the thesis/full gates are green.

## Decision

**REJECTED: the current rev-013 attempt is not approvable yet. The round still fails the required thesis/full gates, but the new within-seam `Algebra.hs` repair is real and materially narrows the blocker cluster, so the next lawful controller move is `retry`, not `update-roadmap`.**

`review-record.json` remains intentionally absent because this attempt does not finalize.
