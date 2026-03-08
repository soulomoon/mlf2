# Findings: Elaboration Input Witness-Authoritative Re-audit

## User Intent
- User asked for a plan to make row `Elaboration input` more thesis-exact.
- Implementation must be organized for agent-team execution.

## Thesis Anchors (Row 1)
- Def. 15.3.12: translation of each instantiation edge uses a chosen propagation witness.
- §15.3.6: elaboration is defined from a translatable presolution (`χp`) on term structure.

## Code Observations (Current Residual Gaps)
1. `src/MLF/Elab/Elaborate.hs:122-131`
- `scopeRootFromBase` swallows binding-tree errors (`Left _ -> typeRef root`).

2. `src/MLF/Elab/Elaborate.hs:143-187`, `:170-179`
- `generalizeAtWith`/`generalizeAtNode` include `SchemeFreeVars` fallback ladders and coherence scoring.

3. `src/MLF/Elab/Elaborate.hs:556-713`
- Let-elaboration still selects fallback schemes from RHS type checks (`fallbackChoiceFromApp/Lam/Var`).

4. `src/MLF/Elab/Elaborate.hs:866-953`
- `reifyInst` still synthesizes fallback instantiations from trace/expansion when `phi` result is `InstId`.

5. `src/MLF/Elab/Run/Pipeline.hs:148-163`
- Root generalization still retries with `Nothing` ga and then `reifyType` fallback when `SchemeFreeVars` occurs.

## Planning Implication
- Existing row and prior "absolute" guard are not sufficient to establish witness-authoritative strictness for elaboration input.
- A new agent-team plan should explicitly retire these fallback mechanisms and add guard coverage for them.

## Artifacts Created
- Plan: `/Volumes/src/mlf4/docs/plans/2026-03-05-elaboration-input-witness-authoritative-agent-team-implementation-plan.md`
- Tracker folder:
  - `/Volumes/src/mlf4/tasks/archive/2026-03-05-elaboration-input-witness-authoritative-agent-team-plan/task_plan.md`
  - `/Volumes/src/mlf4/tasks/archive/2026-03-05-elaboration-input-witness-authoritative-agent-team-plan/findings.md`
  - `/Volumes/src/mlf4/tasks/archive/2026-03-05-elaboration-input-witness-authoritative-agent-team-plan/progress.md`

## 2026-03-08 narrowing update

- `TODO.md` narrowed this task on 2026-03-08 after Task 70 removed the other live fallback ladders originally listed here.
- Items 2-5 above are now closed by later work and should no longer be treated as active residual gaps for this task.
- The remaining live code gap is item 1 only: `src/MLF/Elab/Elaborate.hs:110` still swallows base binding-path failures with `Left _ -> typeRef root`.
- The planned regression `elab-input witness-authoritative guard` is still missing from the test suite (`--match` currently yields `0 examples, 0 failures`), so any future closeout should add a focused guard before or alongside removing the fallback.

## 2026-03-08 closeout

- The narrowed follow-up is now complete: `src/MLF/Elab/Elaborate.hs` no longer swallows `bindingPathToRootLocal` failure as `typeRef root`.
- `test/PipelineSpec.hs` now includes `elab-input witness-authoritative guard` to keep that fallback retired.
- Verification passed for the new guard, the existing row-1 strictness slices, and the full Cabal gate (`1005 examples, 0 failures`).
