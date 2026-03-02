# Task Plan: Reduce Internal `Solved` Compatibility Reads in Result-Type and Generalize Internals

## Goal
Execute a phased, multi-PR initiative that reduces compatibility-oriented internal `Solved` reads in result-type and generalize codepaths while preserving behavior and keeping full gates green after every wave.

## Source Plan
- Agent-Team Plan: Reduce Internal `Solved` Compatibility Reads in Result-Type and Generalize Internals

## Scope
### In Scope
- Remove redundant compatibility rebuilds and unused compatibility fields in generalize/context internals.
- Introduce a result-type read interface to centralize and shrink direct `Solved.*` reads.
- Replace high-risk `ResultType.Fallback` local solved rebuild path with overlay-based logic.
- Add focused regression matrix for `gaSolvedToBase` same-domain/missing and generalize fallback ladder.
- Preserve behavior parity with targeted + full gates after each wave.

### Out of Scope
- Public API redesign in `src-public/*`.
- Large one-pass architecture rewrite removing all solved-threading.
- Algorithmic behavior changes not covered by explicit parity tests.

## Internal Interface / Type Intent
1. Add `MLF.Elab.Run.ResultType.View` as a read-only interface consumed by `ResultType.hs`, `Ann.hs`, `Fallback.hs`.
2. Keep `ResultTypeInputs` shape stable for now; confine `rtcSolveLike` usage to one view-construction boundary.
3. Remove unused compatibility fields from generalize/context internals:
   - `gcConstraintForReify`
   - `rbConstraintForReify`
4. Keep `generalizeAtWithBuilder` signature stable in this initiative.

## Wave Plan
| Wave | Agent Lanes (Parallel) | Code Changes | Tests / Gates | Exit Criteria |
|---|---|---|---|---|
| Wave 0: Baseline + Task Setup | Explorer A (hotspots), Explorer B (tests), Reviewer | Create task folder and seed task docs. Capture hotspot map + acceptance matrix. | Baseline quick gates + `cabal build all && cabal test`. | Baseline green; risks and acceptance matrix captured. |
| Wave 1: Generalize Compatibility Cleanup (Low/Med) | Worker A (Generalize), Worker B (Context), Worker C (tests), Reviewer | Gate alias rebuild for `resAlias` behind `not useConstraintReify`; move explicit-bound helper reads off direct solved-bound reify where `OnConstraint` is authoritative; remove context compatibility fields/wiring; add lightweight trace for unexpected `SolvedToBaseMissing` on base-domain nodes. | Existing focused checks + full gate; add GA fallback-ladder tests. | No regressions; redundant rebuild/use sites reduced. |
| Wave 2: Result-Type View Abstraction (Med, mechanical) | Worker A (new view module), Worker B (`Ann.hs`), Worker C (`ResultType.hs` + `Fallback.hs`), Reviewer | Add `ResultType/View.hs` and wire `mlf2.cabal`; refactor result-type internals to use view accessors; keep semantics unchanged; keep `rtcSolveLike` in view builder boundary only. | Existing focused checks + full gate. | Direct result-type `Solved.*` reads centralized/reduced. |
| Wave 3: Mapping-Branch Test Hardening (Low) | Worker A (tests), Worker B (helpers), Reviewer | Add integrated coverage for `gaSolvedToBase` same-domain/missing branches in result-type fallback flow; keep mapped-case parity guard. | New mapping tests + existing focused checks + full gate. | Matrix covers mapped/same-domain/missing integrated flows. |
| Wave 4: High-Risk Fallback Overlay Replacement (High) | Worker A (fallback core), Worker B (parity harness), Worker C (cleanup), Reviewer + independent re-review | Replace `Solved.rebuildWithNodes` fallback patch path with overlay/view-based node adjustment; keep temporary dual-path parity comparator; remove legacy branch only after parity proof. Preserve `bindParentsGaFinal` mutation and target selection semantics. | New/existing focused checks; explicit parity runs on replay-relevant + non-AAnn fallback cases; full gate. | No `rebuildWithNodes` usage in fallback core; parity proven; suite green. |

## Agent-Team Execution Pattern (Per Wave)
1. Explorer lane confirms current shape before coding starts.
2. 2-3 worker lanes implement independent file clusters.
3. Reviewer lane runs severity-ranked review with file:line findings.
4. Minimum one fix loop; second review confirms closure.
5. Merge only after focused gates + full gate pass.

## Concrete File Targets
- `src/MLF/Elab/Generalize.hs`
- `src/MLF/Elab/Run/Generalize.hs`
- `src/MLF/Constraint/Presolution/Plan/Context.hs`
- `src/MLF/Elab/Run/ResultType.hs`
- `src/MLF/Elab/Run/ResultType/Ann.hs`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/ResultType/Types.hs`
- `src/MLF/Elab/Run/ResultType/Util.hs`
- `src/MLF/Elab/Run/ResultType/View.hs` (new)
- `mlf2.cabal`
- `test/ElaborationSpec.hs`
- `test/PipelineSpec.hs`
- Optional: `test/GeneralizeSpec.hs` (fallback-ladder unit tests)

## Required New Tests
1. `result-type fallback core handles gaSolvedToBase same-domain roots`
2. `result-type fallback core handles gaSolvedToBase missing roots`
3. `generalizeWithPlan falls back from GA to no-GA on SchemeFreeVars`
4. `generalizeWithPlan falls back to reifyType after double SchemeFreeVars`

## Required Existing Focused Checks (Carry Across Waves)
1. `single-solved migration removes split result-type solved fields`
2. `generalize env derives canonical constraint from PresolutionView`
3. `runtime run-path avoids solved-projection and result-type context boundary adapters`
4. `shared solved-to-presolution adapter matches selected solved queries on representative corpus`
5. `generalizeAt fallback reifies from solved root even when base mapping points elsewhere`
6. `AAnn root: primary annotation result type matches fallback facade with populated GA mappings`
7. `gaSolvedToBase resolution classifies mapped, same-domain, and missing outcomes`
8. `letScopeOverrides inserts override on base-vs-solved scope divergence`
9. `PresolutionView mirrors solved canonical/node/bound queries`
10. `fromSolveOutput derives canonical constraint from snapshot, not soResult payload`
11. `pipeline output unchanged after patchNode elimination for:`
12. `fails hard on solved/base shadow mismatch when base shadow is present`

## Canonical Gate (Each Wave)
- `cabal build all && cabal test`

## Risk Controls
- Wave 4 must keep temporary parity harness (`legacy` vs `overlay`) before deleting legacy fallback path.
- Any parity mismatch blocks merge for Wave 4.
- Preserve strict fail-fast mapping invariants (`ValidationFailed` paths).
- Keep all changes warning-free under `-Wall`.

## Documentation / Task Hygiene Per Wave
1. Update this task folder files: `task_plan.md`, `findings.md`, `progress.md`.
2. Update `CHANGELOG.md` with wave summary + validation evidence.
3. Update `implementation_notes.md` only when architecture/behavior claims change.
4. Update `TODO.md` if priority/order changes.

## Assumptions and Defaults
1. Scope depth: phased medium.
2. Delivery shape: multi-PR waves.
3. Fallback risk handling: include high-risk replacement in final wave.
4. No public API expansion desired for this initiative.
5. Success criterion: compatibility reads reduced with behavior preserved and full gate green per wave.

## Phase Status
| Phase | Status | Notes |
|---|---|---|
| Wave 0: Baseline + task setup | completed | Baseline full gate green (`913 examples, 0 failures`); hotspot map captured for result-type/generalize/context targets. |
| Wave 1: Generalize compatibility cleanup | completed | Removed `gcConstraintForReify`/`rbConstraintForReify`; gated alias solved rebuild under non-OnConstraint branch; explicit-bound helpers now use OnConstraint reify when authoritative; added `SchemeFreeVars` fallback-ladder tests. |
| Wave 2: Result-type view abstraction | completed | Added `MLF.Elab.Run.ResultType.View`; centralized solved reads and confined `rtcSolveLike` usage to view construction boundary. |
| Wave 3: Mapping-branch test hardening | completed | Added integrated fallback coverage for `gaSolvedToBase` same-domain and missing-root branches. |
| Wave 4: Fallback overlay replacement | completed | Replaced local `rebuildWithNodes` path with bound-overlay view materialization; preserved target-selection/base-constraint mutation semantics; no fallback-core `rebuildWithNodes` usage remains. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `mlf2-test: unexpected argument 'migration'` while running focused matcher loop | 1 | Fixed shell quoting for `--test-options=\"--match \\\"...\\\"\"` and reran full focused matrix successfully. |
| `ElaborationSpec` import of hidden internal modules (`ResultType.Util`, `Util.ElabError`) | 1 | Re-exported `generalizeWithPlan` from `MLF.Elab.Run.ResultType`, switched tests to use exposed imports and `Elab.ElabError` constructors. |
