# Findings: 2026-03-05 TMT Improving Loop Orchestrator (Fresh)

## Baseline Inputs
- Prompt: `docs/prompts/improving-loop-agent.prompt.md`
- Table: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Thesis: `papers/these-finale-english.txt`
- Source revision: `554de9e`

## Notes
- Fresh run started from current repository state.
- Detailed mechanism-by-mechanism verifier evidence will be appended after Round 1 sweep.

## Round 1 Verifier Full Sweep

Ordered gates (YES/NO):
1. Elaboration input -> YES
2. Result-type context wiring -> YES
3. Ordering of transformations -> YES
4. Per-edge propagation transform -> NO
5. Graph operation execution (Graft/Merge/Weaken/Raise) -> NO
6. Replay-map producer normalization (upfront strict contract) -> NO
7. Replay-map consumer bridge in Phi -> NO
8. Translatability normalization -> NO
9. Canonicalization source used by Phi -> NO
10. Identity reconciliation mechanism -> NO
11. Non-root weaken/raise binder resolution -> NO
12. Graph mutation during solve/presolution -> NO
13. Dual-path verification mechanism -> NO
14. Campaign classification status -> YES

Target mechanism selected (first NO):
- Per-edge propagation transform

Key evidence anchors (from Verifier):
- Thesis: `papers/these-finale-english.txt` §10.3.2 and §15.2.2
- Code: `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs` synthesized-wrapper `ExpIdentity` branch in `executeUnifiedExpansionPath`
- Test: `test/PipelineSpec.hs` row4 guard (`row4 per-edge propagation guard`)

Target gap summary:
- Runtime still has synthesized-wrapper-specific per-edge behavior; not a uniform per-edge expansion + unification transform.

## Round 1 Planner Output (Target: row4)

PlannerRoundPlan summary:
- target_mechanism: Per-edge propagation transform
- root_cause: `executeUnifiedExpansionPath` still branches on synthesized wrappers with forced `ExpIdentity` instead of one uniform per-edge expansion + unification path.
- primary files:
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
  - `test/PipelineSpec.hs`
  - `test/TranslatablePresolutionSpec.hs` (only if needed)
- key implementation direction:
  - remove synthesized-wrapper special-case and keep uniform `decideMinimalExpansion` + `mergeExpansions` + existing downstream witness/trace flow.
- required QA commands:
  - row4 guard matcher
  - `Phase 6` matcher
  - `Translatable presolution` matcher
  - static grep for removed branch markers
  - full gate: `cabal build all && cabal test`
- explicit abort criteria:
  - stop if Phase 6/translatability regressions appear;
  - stop if fix requires cross-phase redesign (EdgeWitness schema or Phi translation pipeline changes).

## Round 1 Attempt 1 Outcome

Gate decisions:
- Bugfix feasibility: NO
- Review gate: NO
- QA gate: NO
- Thesis gate: YES

Observed behavior:
- The row4 implementation change removed synthesized-wrapper special-casing and passed the row4 thesis guard.
- The same change introduced a Phase 6 regression (`PhiTranslatabilityError`, non-spine `OpRaise` missing computation context) on `\y. let id = (\x. x) in id y`.
- Full validation gate failed with downstream regressions in Phase 6 and checked-authoritative/phase3 matcher slices.

Decision:
- Attempt 1 fails step 5.5 (Review=NO, QA=NO). Continue to Attempt 2 after planner failure analysis and revised plan.

## Round 1 Planner Revision for Attempt 2

Failure analysis:
- Uniform interpreter cutover satisfied row4 thesis shape but enabled witness op shapes that can emit non-spine `OpRaise`/`OpRaiseMerge` on synthesized-wrapper edges lacking replay/computation context.
- This triggers Phase 6 `PhiTranslatabilityError` and cascades to checked-authoritative/phase3 gate failures.

Revised approach:
- Keep row4 interpreter objective (no synthesized-wrapper branch reintroduction).
- Add minimally invasive witness-level compatibility pruning in `WitnessNorm` for synthesized-wrapper/no-replay contexts to suppress non-translatable Raise-family ops.
- Add focused regression tests for the `\y. let id = (\x. x) in id y` gate and witness-shape behavior.

Abort criteria highlights:
- Abort if fix requires reintroducing interpreter forced-`ExpIdentity` branch.
- Abort if fix requires Phi non-spine context algorithm redesign or witness schema changes.

## Round 1 Attempt 2 Implementation Outcome

Bugfixer result:
- Feasibility gate: YES
- Changes made:
  - kept row4 uniform interpreter path (no synthesized-wrapper branch markers)
  - added witness-level Raise-family compatibility pruning in `WitnessNorm`
  - added regression coverage in `test/Presolution/WitnessSpec.hs`
- Reported command status:
  - row4 guard: PASS
  - Phase 3 atomic wrapping gates: PASS
  - checked-authoritative: PASS
  - Phase 6: PASS
  - Translatable presolution: PASS
  - full gate (`cabal build all && cabal test`): PASS

Next gate step:
- Run independent Reviewer, QA, and Verifier gates for Attempt 2 state.

## Round 1 Attempt 2 Gate Outcomes

- Review gate: YES
- QA gate: YES (after one procedural re-run correcting no-match grep semantics)
- Thesis gate: YES

Step 5.5 decision:
- Success conditions met (`Review=YES` and `QA=YES` and `Thesis=YES`).
- Integrator can commit on non-master branch/PR flow.
- Round 1 result: CONTINUE to next planning round.

## Round 2 Verifier Full Sweep

Ordered gates (YES/NO):
1. Elaboration input -> YES
2. Result-type context wiring -> YES
3. Ordering of transformations -> YES
4. Per-edge propagation transform -> YES
5. Graph operation execution (Graft/Merge/Weaken/Raise) -> NO
6. Replay-map producer normalization (upfront strict contract) -> NO
7. Replay-map consumer bridge in Phi -> NO
8. Translatability normalization -> NO
9. Canonicalization source used by Phi -> NO
10. Identity reconciliation mechanism -> NO
11. Non-root weaken/raise binder resolution -> NO
12. Graph mutation during solve/presolution -> NO
13. Dual-path verification mechanism -> NO
14. Campaign classification status -> YES

Target mechanism selected (first NO):
- Graph operation execution (Graft/Merge/Weaken/Raise)

Target gap summary (verifier):
- Runtime remains split across staged machinery (`OmegaExec`, delayed weaken scheduling, witness normalization/validation) rather than an absolutely thesis-direct single mechanism.

## Round 2 Planner Output (Target: row5)

PlannerRoundPlan summary:
- target_mechanism: Graph operation execution (Graft/Merge/Weaken/Raise)
- root-cause framing: runtime semantics split across edge-unify/OmegaExec delayed scheduling and witness normalization stages.
- proposed scope: large multi-module cutover across presolution edge execution, delayed-weaken scheduling, and witness normalization pipeline.
- explicit cross-phase abort risks: potential need to touch Phi semantics or `EdgeWitness`/`InstanceOp` contracts.

Execution note:
- Plan scope is materially larger than Round 1 row4 changes and likely to trigger cross-phase redesign abort criteria.

## Round 2 Attempt 1 Outcome

Gate decisions:
- Review gate: YES
- QA gate: YES
- Thesis gate: YES

Decision:
- Round 2 success conditions met at Attempt 1.
- Integrator is authorized to commit and advance to Round 3.

## Round 3 Verifier Full Sweep

Ordered gates (YES/NO):
1. Elaboration input -> YES
2. Result-type context wiring -> YES
3. Ordering of transformations -> YES
4. Per-edge propagation transform -> YES
5. Graph operation execution (Graft/Merge/Weaken/Raise) -> YES
6. Replay-map producer normalization (upfront strict contract) -> NO
7. Replay-map consumer bridge in Phi -> NO
8. Translatability normalization -> NO
9. Canonicalization source used by Phi -> NO
10. Identity reconciliation mechanism -> NO
11. Non-root weaken/raise binder resolution -> NO
12. Graph mutation during solve/presolution -> NO
13. Dual-path verification mechanism -> NO
14. Campaign classification status -> YES

Target mechanism selected (first NO):
- Replay-map producer normalization (upfront strict contract)

Target gap summary:
- Producer replay-map path still uses heuristic candidate-pool/synthesis/projection layers beyond direct witness-derived mapping contract.

## Round 3 Planner Output (Target: row6)

PlannerRoundPlan summary:
- target_mechanism: Replay-map producer normalization (upfront strict contract)
- root-cause framing: producer remains heuristic and tightly coupled to Phi consumer assumptions.
- scope includes both producer (`WitnessNorm`/validation) and potential consumer tightening (`Phi.Translate`/`Phi.Omega`), with explicit cross-phase redesign risk.

Primary abort risk:
- strict producer contract may be unattainable without semantic changes in Phase 6 consumer behavior.

## Round 3 Attempt 1 Outcome

Gate decisions:
- Review gate: YES
- QA gate: YES
- Thesis gate: NO

Decision:
- Attempt 1 fails step 5.5 due thesis gate NO.
- Planner revision required for Attempt 2.

## Round 3 Attempts 2-6 Consolidated Outcome

Observed pattern:
- Attempts 2-6 remained bounded-scope feasibility failures for row6.
- QA baseline stayed green throughout (`Phase 6`, `checked-authoritative`, full gate).
- Verifier remained NO on row6 because `WitnessNorm` still contains non-thesis strictness scaffolding (`replayCandidatePool`, synthesis/projection/no-replay compatibility paths).

Terminal decision:
- Round 3 reached attempt limit (6) without satisfying step 5.5 success conditions.
- Terminal status is `MAXIMUMRETRY`.
