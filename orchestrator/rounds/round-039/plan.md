# Round 039 Plan (`E2` Retry: Same-Lane Behavioral Evidence)

## Exact Target

- Execute only the `E2` retry for repaired `URI-R2-C1` on the existing branch/worktree; do not create or switch branches/worktrees.
- Preserve the inherited boundary exactly: `explicit-only / non-equi-recursive / non-cyclic-graph`.
- Start from the accepted `C2` / `C3` baseline: `rootBindingIsLocalType` remains the mandatory gate before any retained-target behavior is considered.
- Carry forward the accepted `attempt-1` implementation anchor in `src/MLF/Elab/Run/ResultType/Fallback.hs:478-715`: the retained-child lane stays centered on `scopeRootPost`, `boundHasForallFrom`, `boundVarTarget`, `keepTargetFinal`, and the `targetC` selection branch.
- Preserve the repaired retained-child rule exactly: a child-derived retained target is admissible only when it stays in the same canonical local `TypeRef` lane as the accepted local-binding root and `boundHasForallFrom` proves that it does not cross a nested `forall` or nested scheme-root owner boundary.
- Otherwise keep the repaired lane fail-closed and fall back to the already-accepted non-retained behavior from the `C2` / `C3` baseline.
- Keep every other `keepTargetFinal` trigger family fail-closed and out of scope: `rootHasMultiInst`, `instArgRootMultiBase`, and `rootIsSchemeAlias && rootBoundIsBaseLike`.
- In the existing `ARI-C1 feasibility characterization (bounded prototype-only)` block in `test/PipelineSpec.hs`, add exactly one new behavioral retained-child same-lane success example. It must exercise pipeline behavior rather than a source-string guard, and it must be distinct from the pre-existing `"keeps local-binding recursive retention processable through a direct wrapper"` example that review identified as inherited evidence rather than new `E2` evidence.
- Keep the matched nested-`forall` / nested-owner fail-closed contrast in that same `ARI-C1` block as the negative companion example for the retry.
- Update the canonical `E2` artifact `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md` and the round notes `orchestrator/rounds/round-039/implementation-notes.md` so both explicitly record the new behavioral same-lane success example and the retained nested-`forall` fail-closed contrast.
- No widening, no replay reopen, no `MLF.Elab.Inst`, no `InstBot`, no `E1` reopen, no `E3` / `E4` work, no equi-recursive reasoning, no cyclic structural encoding, no cross-family widening, no compatibility/default-path widening, and no second interface.

## Owned Files

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`
- `orchestrator/rounds/round-039/implementation-notes.md`
- No other production, test, public API, executable, Cabal, roadmap, controller-state, bug-tracker, or prior-round artifact file is owned by this retry.

## Required Verification

- Baseline contract checks from `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-006/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-039/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-039/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-006/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-006/retry-subloop.md`
- Focused bounded block:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Full repo gate:
  - `cabal build all && cabal test`
- Retry-specific evidence:
  - confirm `test/PipelineSpec.hs` now contains exactly one new behavioral retained-child same-lane success example inside the existing `ARI-C1` block, and that the example is not just a `readFile`/source-string guard and not the pre-existing `"keeps local-binding recursive retention processable through a direct wrapper"` case;
  - confirm the same `ARI-C1` block still contains the matched nested-`forall` / nested-owner fail-closed contrast;
  - confirm reviewer evidence names both examples explicitly and ties the success case to the repaired same-lane retained-child behavior under `rootBindingIsLocalType` / `boundVarTarget`, while tying the negative case to `boundHasForallFrom`;
  - confirm `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md` and `orchestrator/rounds/round-039/implementation-notes.md` both record the success example and the fail-closed contrast, not just a source-level guard;
  - confirm the retry stays on the same branch/worktree and does not widen ownership or reopen excluded lanes.
- Scope and boundary confirmation:
  - confirm only `src/MLF/Elab/Run/ResultType/Fallback.hs`, `test/PipelineSpec.hs`, `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`, and `orchestrator/rounds/round-039/implementation-notes.md` changed;
  - confirm no edit to `src/MLF/Elab/Inst.hs`, no second executable/interface file, and no roadmap/controller-state/bug-tracker mutation;
  - confirm the other `keepTargetFinal` trigger families remain fail-closed.
