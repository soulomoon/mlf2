# Round 234 Review

Date: 2026-05-15
Round: `round-234`
Milestone: `milestone-4`
Direction: `direction-4a-forallspec-binder-safety`
Extracted item: `item-4a-sigma-reorder-index-totalization`
Base branch: `master`
Branch: `orchestrator/round-234-forallspec-binder-closeout`

## Findings

- No blocking findings.
- Residual note: `orchestrator/rounds/round-234/implementation-notes.md` correctly records the rerun commands/results below, but its final note that `src/MLF/Elab/Phi/TestSupport.hs` was "left untouched" is stale relative to the live diff against `master`. The live round diff adds only the private `reorderSpineTo` test seam there, and approval relies on the actual diff and rerun validation below.

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Checks Run

- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Result: pass. The live bundle still resolves to `roadmap_id = 2026-05-05-00-type-level-safety-singletons-roadmap`, `roadmap_revision = rev-001`, and `roadmap_dir = orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`, with `round-234` active at review stage.

- Command: `git status --short --branch && git rev-parse HEAD && git merge-base HEAD master`
  Result: pass. The worktree is on `orchestrator/round-234-forallspec-binder-closeout`, and `HEAD` equals the merge-base with `master` at `9e4394c667410481ff4033358eaf1afdff39c144`. The review target is therefore the live uncommitted worktree diff against `master`, not a committed branch delta.

- Command: `python3 - <<'PY'`
  ```python
  import json
  from pathlib import Path
  base = Path("orchestrator/rounds/round-234")
  state = json.loads(Path("orchestrator/state.json").read_text())
  selection = json.loads((base / "selection-record.json").read_text())
  plan = json.loads((base / "round-plan-record.json").read_text())
  view = json.loads(Path(state["roadmap_dir"], "roadmap-view.json").read_text())
  assert selection["roadmap_id"] == state["roadmap_id"]
  assert selection["roadmap_revision"] == state["roadmap_revision"]
  assert plan["roadmap_dir"] == state["roadmap_dir"]
  for anchor in ["milestone-4", "milestone-4-completion", "roadmap-history-completed-rounds"]:
      assert anchor in view["anchors"]
  print("ok")
  ```
  `PY`
  Result: pass. `selection-record.json`, `round-plan-record.json`, and the active roadmap anchors line up with the live bundle, so a status-only closeout to `milestone-4 -> done` is lawful if the code/test evidence closes the remaining gap.

- Command: `git diff --check`
  Result: pass. No whitespace or patch-format issues.

- Command: `git diff --name-status master`
  Result: pass. The live payload against `master` is limited to controller-owned `orchestrator/state.json`, `src/MLF/Elab/Sigma.hs`, `src/MLF/Elab/Phi/TestSupport.hs`, and `test/ElaborationSpec.hs`, plus the reviewer-owned round artifacts written by this review.

- Command: `git diff master -- src/MLF/Constraint/Types/Witness.hs mlf2.cabal src-public/MLF/API.hs src-public/MLF/Pipeline.hs`
  Result: pass. No output. `ForallSpec`, witness-constructor surfaces, Cabal surface registration, and public API files are unchanged.

- Command: `rg -n "!!" src/MLF/Elab/Sigma.hs`
  Result: pass. No matches. The selected Sigma reorder path no longer uses partial list indexing.

- Command: `rg -n "!!" src/MLF/Elab test/ElaborationSpec.hs`
  Result: pass. No matches anywhere under `src/MLF/Elab` or the touched elaboration spec, which is strong evidence that the known milestone-4 binder-indexing seam is now closed rather than merely moved.

- Command: `rg -n "MLF\\.Elab\\.Phi\\.TestSupport|mlf2-internal" mlf2.cabal && sed -n '1,40p' src/MLF/Elab/Phi/TestSupport.hs`
  Result: pass. `MLF.Elab.Phi.TestSupport` remains exposed only from the private `mlf2-internal` library, and the round adds only `reorderSpineTo = Sigma.bubbleReorderToFromSpine` there as a narrow test-support seam.

- Command: `cabal build mlf2-test`
  Result: pass. The focused test target is up to date after the Sigma/TestSupport/spec changes.

- Command: `cabal test mlf2-test --test-options='--match "Σ(g) quantifier reordering"'`
  Result: pass. The updated Sigma reorder block ran `13 examples, 0 failures`, including exact `InstantiationError` assertions for missing binders and short source spines.

- Command: `cabal test mlf2-test --test-options='--match "scheme-aware Φ can target a non-front binder"'`
  Result: pass. The preserved live Phi-driven reorder path ran `1 example, 0 failures`.

- Command: `cabal build all && cabal test`
  Result: pass. The behavior-changing full gate completed with `2572 examples, 0 failures` in `352.3147` seconds.

- Command: `for r in 094 095 096 097 098; do sed -n '1,40p' "orchestrator/rounds/round-$r/review-record.json"; done`
  Result: pass. The inherited same-lane retained-child review chain remains authoritative through `accepted` / `finalize` records in `round-094` through `round-098`; this round does not alter those artifacts.

- Command: `rg -n "non-cyclic-graph = unknown|no-fallback = keep|no second interface is authorized" docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  Result: pass. The inherited recursive-inference settlement remains bounded and unchanged, and the current round diff does not touch any of those surfaces.

## Plan Compliance

- `Add a local checked lookup boundary in MLF.Elab.Sigma for the current target binder at idx, so reorder logic reports explicit InstantiationError when the desired binder list is inconsistent instead of indexing with !!`: met. `src/MLF/Elab/Sigma.hs` now introduces `checkedReorderBinderPair` and routes both reorder functions through it.

- `Rewrite bubbleReorderTo and bubbleReorderToFromSpine to use the checked lookup boundary for both equality checks and elemIndex searches, preserving the existing successful reorder behavior while removing the remaining partial binder-order reads from this path`: met. Both Sigma reorder loops now stop via `Nothing`, report short-source cases via `InstantiationError`, and still drive successful reorder behavior through the same `bubbleLeft` path.

- `Add focused regression coverage for the new fail-closed behavior in the Sigma/Phi reorder surface, including a mismatch or short-desired-list case that exercises the new checked reads, plus a preservation case that proves live Phi-driven reordering still succeeds`: met. `test/ElaborationSpec.hs` now asserts exact `InstantiationError` messages for missing binders and short source spines, and the existing `scheme-aware Φ can target a non-front binder` preservation path remains green.

- `Keep ForallSpec unchanged unless the evidence forces a narrowly scoped helper or comment update: binder count must remain derived from fsBounds, no heavier type-level dependency may be introduced, and milestone-5 witness-constructor work stays out of scope`: met. `src/MLF/Constraint/Types/Witness.hs` is unchanged, `forallSpecBinderCount = length . fsBounds` remains the contract, and no witness-constructor or dependency changes were introduced.

- `Run focused Sigma/Phi validation first, then diff hygiene, then the full cabal build all && cabal test gate required for behavior-changing milestone-4 work`: met. `git diff --check`, focused `mlf2-test` selectors, and the full gate all passed in the order required for approval.

- `src/MLF/Elab/Phi/TestSupport.hs` touched only as a test-support seam`: accepted. The file remains private to `mlf2-internal`, adds no production behavior, and exposes only the minimum helper needed to assert `bubbleReorderToFromSpine` fail-closed behavior from tests.

## Decision

**APPROVED**

## Evidence

- `src/MLF/Elab/Sigma.hs` closes the exact remaining round-233 gap. `bubbleReorderTo` and `bubbleReorderToFromSpine` no longer use partial `!!`; both now call `checkedReorderBinderPair`, which returns explicit `InstantiationError` when the desired binder order outruns the available source binders and preserves the existing reorder path when the source is valid.

- `test/ElaborationSpec.hs` now covers both sides of the contract requested for this round. The `Σ(g) quantifier reordering` block asserts exact `InstantiationError` messages for missing desired binders and for a desired order longer than the source spine, while the live `scheme-aware Φ can target a non-front binder` case still proves successful non-front-binder reordering through the production Phi path.

- `src/MLF/Elab/Phi/TestSupport.hs` is acceptable scope. The new `reorderSpineTo` helper is private to `mlf2-internal`, does not widen `src-public`, and exists only to give the test suite a narrow seam for the spine-based Sigma helper without changing production behavior.

- `ForallSpec` remains on the accepted list-shaped contract. `src/MLF/Constraint/Types/Witness.hs` still defines `ForallSpec` only in terms of `fsBounds`, and `forallSpecBinderCount` still derives binder arity via `length . fsBounds`. No milestone-5 witness-constructor work is present in the round.

- Milestone-4 can move `in-progress -> done`. Round-233 already totalized the selected Phi/VSpine binder-spine path; round-234 removes the remaining Sigma partial binder-order indexing and adds the missing explicit-failure regressions. With no `!!` left anywhere under `src/MLF/Elab`, the known `ForallSpec` binder-safety scope described by milestone-4 is now closed.

- The inherited recursive-inference settlement chain remains untouched. The accepted `round-094` through `round-098` predecessor evidence is still authoritative and the March plan docs still keep `non-cyclic-graph = unknown`, `no-fallback = keep`, and `no second interface` boundaries in force. This round does not alter any of those surfaces.
