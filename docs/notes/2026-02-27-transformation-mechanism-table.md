# Transformation Mechanism Table (Thesis vs Codebase)

Last updated (UTC): 2026-03-07
Source revision: `816eb2e`
Note: thesis exact includes test-only code paths

| Transformation mechanism | Thesis pipeline | Current codebase | What to change for thesis-exact | Thesis-exact |
|---|---|---|---|---|
| Elaboration input | Thesis elaboration consumes a translatable presolution `χp`; translation of `a` to xMLF is defined inductively on term shape (Fig. 15.3.5, §15.3.6). For each instantiation edge `e`, choose a propagation witness `I` and define `T(e)` from that witness (Def. 15.3.12); witness choice is intentionally non-deterministic ("pick any propagation witness"). (`papers/these-finale-english.txt:14087`, `papers/these-finale-english.txt:14095`, `papers/these-finale-english.txt:14112`, `papers/these-finale-english.txt:14114`) | The live elaboration entry is `χp`-first: `Pipeline` builds `ElabEnv` from `PresolutionView`, `ElabEnv`/`PhiEnv` carry presolution-view + witness/trace data rather than an entry-time `Solved`, and ga-base scope resolution now propagates `bindingPathToRootLocal` failures instead of falling back. The absolute row-1 guards specifically forbid the old `Left _ -> ref` / `Left _ -> root` fallbacks and `Solved`-typed phi/elaboration entrypoints. (`src/MLF/Elab/Run/Pipeline.hs:110`, `src/MLF/Elab/Run/Pipeline.hs:141`, `src/MLF/Elab/Elaborate.hs:69`, `src/MLF/Elab/Elaborate.hs:97`, `src/MLF/Elab/Phi/Env.hs:42`, `src/MLF/Elab/Run/Scope.hs:103`, `src/MLF/Elab/Run/Scope.hs:198`, `src/MLF/Constraint/Presolution/Plan/Context.hs:145`, `test/PipelineSpec.hs:205`, `test/PipelineSpec.hs:216`, `test/ElaborationSpec.hs:4847`) | Closed in Round 1 by removing the last live ga' scope-preference swallow and adding a behavioral regression for `resolveContext`. Keep mandatory regression gates: `resolveContext propagates ga base binding-path failures instead of falling back`, `elab-input witness-authoritative guard`, `elab-input absolute thesis-exact guard`, `row1 closeout guard`, `checked-authoritative`, `Dual-path verification`, and `cabal build all && cabal test`. | Yes |
| Result-type context wiring | Thesis defines `Typ(a′)`/`Typexp(a′)` from translated constraint artifacts and `T(a)` from `ϕR(a′)` plus chosen `T(e)` witnesses over `χp`; no separate solved-adapter boundary appears in that presentation. (`papers/these-finale-english.txt:13631`, `papers/these-finale-english.txt:13670`, `papers/these-finale-english.txt:14087`, `papers/these-finale-english.txt:14114`, `papers/these-finale-english.txt:14130`) | The live row2 path is now snapshot-native end-to-end: `Pipeline` builds finalized clean/generalization views directly from `Finalize.finalizePresolutionViewFromSnapshot`, `ResultType.View` validates from the canonical constraint + canonical map without rebuilding `Solved`, and `ChiQuery` no longer exposes solved-compat adapters. (`src/MLF/Constraint/Finalize.hs:65`, `src/MLF/Elab/Run/Pipeline.hs:91`, `src/MLF/Elab/Run/Pipeline.hs:112`, `src/MLF/Elab/Run/ResultType/View.hs:51`, `test/PipelineSpec.hs:228`) | Closed in the 2026-03-07 fresh round-2 follow-up by retiring the last live solved-compat adapter path and extending the absolute row2 guard. Keep mandatory regression gates: `row2 absolute thesis-exact guard`, `row2 closeout guard`, `checked-authoritative`, `Dual-path verification`, `Frozen parity artifact baseline`, and `cabal build all && cabal test`. | Yes |
| Ordering of transformations | `SolveConstraint` first topologically orders instantiation edges, solves existing unification edges, then visits each instantiation edge in order, performing propagation and immediately solving the resulting unification edges. (`papers/these-finale-english.txt` §12.1.3) | Presolution mirrors that shape with explicit closure boundaries: it drains unification closure before traversal and after each instantiation edge, and delayed weakenings are queued with stable owner buckets, flushed only for the closed owner (plus `Unknown`) when owner groups change, and rejected if any non-next-owner bucket remains. Finalization still fails fast on residual pending queues, missing/residual `TyExp` materialization, and witness/trace key mismatch. (`src/MLF/Constraint/Presolution/EdgeProcessing.hs`, `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`, `src/MLF/Constraint/Presolution/Driver.hs`) | Closed by Task 47 follow-up to Task 45 (strict owner-boundary scheduling / absolute ordering hardening). Keep mandatory regression gates: `row3 absolute thesis-exact guard`, `Phase 4 thesis-exact unification closure`, `Translatable presolution`, `checked-authoritative`, `Dual-path verification`, and full gate (`cabal build all && cabal test`). | Yes (strict owner-boundary ordering contract) |
| Per-edge propagation transform | Propagation expands schemes and adds unification obligations. (`papers/these-finale-english.txt` §10.3.2, §15.2.2) | `executeUnifiedExpansionPath` is now uniform across frontend `TyExp` edges: each edge computes minimal expansion (`decideMinimalExpansion`), merges with existing expansion (`mergeExpansions`), records edge expansion/trace/witness, and applies the same unification flow with no synthesized-wrapper-specific interpreter branch. (`src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs:53-89`) | Closed in orchestrated runner Round 1 (Attempt 2) with guardrails. Keep regression gates: `row4 per-edge propagation thesis-exact guard`, `Phase 6`, `checked-authoritative`, `Translatable presolution`, and full gate (`cabal build all && cabal test`). | Yes (uniform per-edge expansion + unification path in interpreter) |
| Graph operation execution (Graft/Merge/Weaken/Raise) | Operations are part of normalized witness derivations translated into computations. (`papers/these-finale-english.txt` Fig. 15.3.4) | Edge unification now calls a single edge-local omega execution entrypoint (`executeEdgeLocalOmegaOps`) from `runExpansionUnify`; direct pre/post omega invocations are removed from the edge-processing callsite while preserving operation ordering semantics internally. (`src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs:148-159`, `src/MLF/Constraint/Presolution/EdgeUnify.hs:199-212`, `test/PipelineSpec.hs:277-284`) | Closed in orchestrated runner Round 2 (Attempt 1). Keep regression gates: `row5 graph-op execution thesis-exact guard`, `Phase 4 thesis-exact unification closure`, `Phase 3 — Witness normalization`, `Witness translation`, and full gate (`cabal build all && cabal test`). | Yes (single edge-local graph-op execution entrypoint at callsite) |
| Replay-map producer normalization (upfront strict contract) | Normalized propagation witnesses are translated operation-by-operation into computations; the thesis defines `T(e)` from any chosen propagation witness, but does not define a separate runtime replay-map contract. (`papers/these-finale-english.txt:12789`, `papers/these-finale-english.txt:12839`, `papers/these-finale-english.txt:13925`, `papers/these-finale-english.txt:14087`, `papers/these-finale-english.txt:14095`) | Producer-side replay-map normalization is strict at the producer boundary: `WitnessNorm` classifies replay vs no-replay in restored source identity space, writes back source-key replay maps, emits `ReplayMapIncomplete` when the active source domain is under-covered, and rejects residual no-replay replay-family ops that cannot be projected away; shared replay-map validation checks codomain membership/non-`TyVar`/injectivity, and `Driver` revalidates the finalized trace contract. (`src/MLF/Constraint/Presolution/WitnessNorm.hs:317`, `src/MLF/Constraint/Presolution/WitnessNorm.hs:353`, `src/MLF/Constraint/Presolution/WitnessNorm.hs:414`, `src/MLF/Constraint/Presolution/WitnessValidation.hs:93`, `src/MLF/Constraint/Presolution/Driver.hs:253`, `test/Presolution/WitnessSpec.hs:1662`, `test/Presolution/WitnessSpec.hs:1819`, `test/Presolution/WitnessSpec.hs:1935`, `test/Presolution/WitnessSpec.hs:2034`, `test/Presolution/WitnessSpec.hs:2178`, `test/Phi/AlignmentSpec.hs:41`) | Closed. Keep producer-boundary regression gates for source-domain under-coverage, replay-codomain injectivity, empty replay-domain alignment, `checked-authoritative`, `Dual-path verification`, and `cabal build all && cabal test`. | Yes |
| Replay-map consumer bridge in Phi | Phi consumes normalized witness artifacts; thesis presentation has no runtime repair layer. (`papers/these-finale-english.txt` §15.3.5) | `computeTraceBinderReplayBridge` is now a strict validation/pass-through boundary: it rejects trace/replay source-domain mismatches and replay codomain escapes, then hands the producer-normalized replay map unchanged to Ω. Downstream Φ/Ω fail fast on unresolved replay targets rather than synthesizing repairs. (`src/MLF/Elab/Phi/Translate.hs:269-360`, `src/MLF/Elab/Phi/Omega.hs:651-682`, `src/MLF/Elab/Phi/Omega.hs:759-805`, `test/ElaborationSpec.hs:2098-2250`, `test/Phi/AlignmentSpec.hs:39-58`) | Closed in the 2026-03-07 sweep: keep row-7 regression gates for source-domain parity, replay codomain membership, canonical-alias hard reject, empty replay-domain alignment, and the full suite (`cabal build all && cabal test`). | Yes |
| Translatability normalization | Definition 15.2.10 requires a translatable presolution with no inert-locked nodes and with rigid non-degenerate scheme roots, application/abstraction arrows, and non-interior gen-bound nodes; §15.2.8 then strengthens internal-language normalization by weakening all inert nodes via `W` so translation is modular. (`papers/these-finale-english.txt:13431`, `papers/these-finale-english.txt:13447`, `papers/these-finale-english.txt:13473`) | Live presolution finalization now performs the stronger §15.2.8 all-inert `W`-normalization on the production path: `rigidifyTranslatablePresolutionM` weakens all inert nodes before and after rigidification, while the existing Definition 15.2.10 / Theorem 15.2.11 constructive checks and witness normalization consume the enlarged weakened set. (`src/MLF/Constraint/Presolution/Validation.hs:120`, `src/MLF/Constraint/Presolution/Validation.hs:123`, `src/MLF/Constraint/Presolution/Validation.hs:187`, `src/MLF/Constraint/Inert.hs:146`, `test/TranslatablePresolutionSpec.hs:14`, `test/FrozenParitySpec.hs:13`) | Closed in the 2026-03-07 fresh round-2 follow-up by putting all-inert `W`-normalization on the live presolution path and refreshing the frozen parity oracle to the new thesis-exact artifacts. Keep mandatory regression gates: `row8 thesis-exact guard`, `Translatable presolution`, `O15-TRANS*`, `O05-*`, `checked-authoritative`, `Dual-path verification`, `Frozen parity artifact baseline`, and `cabal build all && cabal test`. | Yes |
| Canonicalization source used by Phi | Canonicalization is implicit in derivation/equivalence reasoning over `χp`. (`papers/these-finale-english.txt` §15.3.5-§15.3.6) | Replay-bridge construction still validates witness-domain trace/replay key spaces first, but Ω no longer performs post-bridge source-candidate recovery. `OpRaise` now chooses only the direct replay/source target, fails fast when no direct target exists, and uses at most the forward `etCopyMap` alias as witness-authoritative evidence for source-domain interior membership; canonicalization remains downstream lookup machinery instead of a source-selection repair layer. (`src/MLF/Elab/Phi/Omega.hs:611-755`, `src/MLF/Elab/Phi/Omega.hs:922-930`, `test/ElaborationSpec.hs:2000-2039`, `test/ElaborationSpec.hs:2430-2462`, `test/PipelineSpec.hs:287-292`) | Closed in Round 2. Keep regression gates: `source-space identity replay target`, both strict `OpRaise` fail-fast slices, `row9-11 direct-target guard`, `IdentityBridge`, `checked-authoritative`, `Dual-path verification`, and `cabal build all && cabal test`. | Yes |
| Identity reconciliation mechanism | Thesis elaboration carries identity directly through named nodes, computation contexts, witness-derived computations (`ε`, `ϕR`, `T(e)`), and quantifier-spine order; it does not define a separate identity-reconciliation layer. (`papers/these-finale-english.txt:13547-13553`, `:13690-13707`, `:13827-13830`, `:13855-13858`, `:13942-13954`, `:14087-14097`, `:14114-14124`) | Runtime Φ/Ω no longer reconciles alternative source candidates through local helper ranking. Binder/target resolution is direct from replay/spine identities, while `IdentityBridge` is retained only as a witness-domain utility/diagnostic surface and dedicated unit-test target rather than a runtime recovery layer. (`src/MLF/Elab/Phi/Omega.hs:611-755`, `src/MLF/Elab/Phi/Omega.hs:1174-1237`, `src/MLF/Elab/Phi/IdentityBridge.hs:142-260`, `test/Phi/IdentityBridgeSpec.hs:1-235`, `test/PipelineSpec.hs:287-292`) | Closed in Round 2 by deleting Ω’s source-candidate reconciliation helpers and keeping witness-domain identity utilities non-authoritative for runtime target repair. Preserve the same row9-11 regression stack plus full-suite validation. | Yes |
| Non-root weaken/raise binder resolution | Thesis translation follows witness/operation structure; unresolved witness targets are not success cases. (`papers/these-finale-english.txt:13986-14003`) | Non-root `OpWeaken` remains direct and fail-fast once a replay target is supplied, and non-root `OpRaise` now likewise requires a direct replay/source target instead of degrading unresolved non-trace targets to `ε`/no-op. The only remaining alias allowance is forward `etCopyMap` evidence for source-domain interior membership, which stays inside witness artifacts rather than runtime recovery. (`src/MLF/Elab/Phi/Omega.hs:720-755`, `src/MLF/Elab/Phi/Omega.hs:922-930`, `src/MLF/Elab/Phi/Omega.hs:1219-1237`, `test/ElaborationSpec.hs:2000-2039`, `test/ElaborationSpec.hs:2390-2462`) | Closed in Round 2. Keep regression gates for source-space replay-target fail-fast, both strict `OpRaise` unresolved-target slices, the row9-11 direct-target guard, `checked-authoritative`, `Dual-path verification`, and the full suite. | Yes |
| Graph mutation during solve/presolution | Thesis solving mutates the current constraint by propagating each instantiation edge and solving the resulting unification edges; the proof explicitly characterizes which nodes are changed, and later summarizes acyclic solving as unification + propagation steps. (`papers/these-finale-english.txt:9536`, `papers/these-finale-english.txt:9554`, `papers/these-finale-english.txt:9561`, `papers/these-finale-english.txt:9671`) | Production still matches that phase boundary: presolution drains unification closure inside the edge loop, and its finalization stage materializes expansions, rewrites/canonicalizes away `TyExp`, rigidifies, and normalizes witnesses; the runtime pipeline then finalizes/rebuilds `Solved` views from the presolution snapshot for elaboration/generalization without rerunning propagation or unification. (`src/MLF/Constraint/Presolution/EdgeProcessing.hs:57`, `src/MLF/Constraint/Presolution/EdgeProcessing.hs:138`, `src/MLF/Constraint/Presolution/Driver.hs:113`, `src/MLF/Elab/Run/Pipeline.hs:84`, `src/MLF/Elab/Run/Pipeline.hs:87`, `src/MLF/Constraint/Finalize.hs:79`, `src/MLF/Constraint/Solve.hs:216`) | Closed in the 2026-03-07 sweep: keep `uses presolution-native solved artifacts`, `runtime snapshot rebuild stays stable across representative corpus`, the runtime callgraph guard forbidding legacy snapshot APIs in production run-path modules, and the normal full-gate regression cadence. (`test/PipelineSpec.hs:353`, `test/PipelineSpec.hs:407`, `test/PipelineSpec.hs:440`) | Yes |
| Dual-path verification mechanism | N/A in thesis | Production elaboration remains single-path: `runPipelineElab` and `runPipelineElabChecked` collapse to the same implementation, while dual-path and frozen-parity coverage live only in the test harness as engineering guardrails, not in runtime control flow. (`src/MLF/Elab/Run/Pipeline.hs:63`, `src/MLF/Elab/Run/Pipeline.hs:73`, `src/MLF/Elab/Run/Pipeline.hs:80`, `test/PipelineSpec.hs:349`, `test/PipelineSpec.hs:353`, `test/FrozenParitySpec.hs:13`, `test/Parity/FrozenArtifacts.hs:128`) | Closed in the 2026-03-07 sweep: keep `Dual-path verification`, `Frozen parity artifact baseline`, and the full suite as non-runtime safety nets. | Yes |
| Campaign classification status | N/A in thesis | DEV-TMT campaign items are retired in repo metadata: `DEV-TMT-*` entries appear under `history.resolved` with no active DEV-TMT IDs in `deviations`. (`docs/thesis-deviations.yaml:3-91`, `:131-303`) | Keep `docs/thesis-deviations.yaml` free of active live `DEV-TMT-*` entries; under the orchestrator rule this row is `YES` when campaign items are retired or resolved. | Yes |

## 2026-03-04 Wave 3 Task 6 verification evidence (row2 adapter retirement closeout)

- Required closeout matcher commands:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
    - PASS (`3 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`).
  - `cabal build all && cabal test`
    - PASS (`929 examples, 0 failures`).

## 2026-03-04 Task 35 elaboration-input thesis-exact closeout evidence

- Required closeout matcher commands:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
    - PASS (`2 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`).
  - `cabal build all && cabal test`
    - PASS (`931 examples, 0 failures`).

## 2026-03-04 Task 39 elaboration-input strict legacy-retirement closeout evidence

- Reclassification policy for this row is strict:
  - test-only code paths are in scope, and `Thesis-exact = Yes` is allowed only
    after the migration plus required gates below.
- Required closeout matcher commands:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
    - PASS (`2 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`).
  - `cabal build all && cabal test`
    - PASS (`931 examples, 0 failures`).

## 2026-03-05 Task 41 elaboration-input absolute strict all-path hardening evidence

- Wave 0 RED baseline:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'`
    - FAIL before Wave 1 (guard detected residual `peResult :: Solved` surface).
- Required GREEN gates after integration:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'`
    - PASS (`1 example, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`).
  - `cabal build all && cabal test`
    - PASS (`934 examples, 0 failures`).

## 2026-03-05 Task 42 row2 result-type context wiring absolute hardening evidence

- Wave 0 RED baseline:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
    - FAIL before Wave 1 (`1 example, 1 failure`).
- Required GREEN gates after Waves 1-2 integration:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
    - PASS (`1 example, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
    - PASS (`3 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`).
  - `cabal build all && cabal test`
    - PASS (`935 examples, 0 failures`).

## 2026-03-05 Task 44 ordering-row closeout evidence

- Wave 0 RED baseline:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 ordering thesis-exact guard"'`
    - FAIL (`2 examples, 2 failures`) before integration.
- Required GREEN gates after Waves 1-3 integration:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 ordering thesis-exact guard"'`
    - PASS (`2 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Translatable presolution"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`).
  - `cabal build all && cabal test`
    - PASS.
  - Full-suite evidence run:
    - `cabal test mlf2-test --test-show-details=direct`
    - PASS (`938 examples, 0 failures`).

## 2026-03-05 Task 45 row3 absolute ordering follow-up evidence

- Wave 0 RED baseline:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`
    - FAIL (`4 examples, 4 failures`) before Wave 1.
- Required GREEN gates after Waves 1-3 integration:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`
    - PASS (`4 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
    - PASS (`10 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Translatable presolution"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "generalizes reused constructors via make const"'`
    - PASS (`1 example, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V1"'`
    - PASS (`1 example, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'`
    - PASS (`1 example, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`).
  - `cabal build all && cabal test`
    - PASS (`942 examples, 0 failures` in `mlf2-test` log summary).

## 2026-03-05 orchestrated runner closeout evidence (rows 4-5)

- Round 1 row4 closeout (`Per-edge propagation transform`):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row4 per-edge propagation thesis-exact guard"'`
    - PASS.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6"'`
    - PASS.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS.
  - `cabal build all && cabal test`
    - PASS.
- Round 2 row5 closeout (`Graph operation execution (Graft/Merge/Weaken/Raise)`):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row5 graph-op execution thesis-exact guard"'`
    - PASS.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
    - PASS.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 — Witness normalization"'`
    - PASS.
  - `cabal build all && cabal test`
    - PASS.

## 2026-03-06 row6 replay-contract recovery evidence

- Historical note: the 2026-03-05 row6 `MAXIMUMRETRY` orchestrator run is archived evidence only; it was superseded by the recovery wave below and the fresh 2026-03-07 full sweep.
- Recovery verification from the green-base replay-contract repair:
  - producer-boundary row6 regression slices for source-domain under-coverage, replay-codomain injectivity, and empty replay-domain alignment all passed.
  - `checked-authoritative` — PASS (`8 examples, 0 failures`).
  - `Dual-path verification` — PASS (`4 examples, 0 failures`).
  - `cabal build all && cabal test` — PASS (`954 examples, 0 failures`).

## 2026-03-07 fresh round-2 full sweep closeout (historical verifier result)

- Fresh sweep source:
  - `tasks/archive/2026-03-06-tmt-improving-loop-orchestrator-fresh-round-2/task_plan.md`
  - `tasks/archive/2026-03-06-tmt-improving-loop-orchestrator-fresh-round-2/findings.md`
  - `tasks/archive/2026-03-06-tmt-improving-loop-orchestrator-fresh-round-2/orchestrator-log.jsonl`
- Historical outcome at the time: the verifier sweep returned `YES` for all 14 mechanisms from a green base.
- This section is now superseded by the per-row thesis/code re-audit below, which reopened row2 and row8 after a stricter row-by-row review of the current codebase against the thesis text.

## 2026-03-07 per-row fresh review audit

- Review method: one fresh reviewer agent per table row, each comparing the live row against the newest working tree and `papers/these-finale-english.txt`.
- Reclassifications from that audit:
  - Row2 `Result-type context wiring` -> `No` because a hidden solved-compat adapter still remains on the live path (`fromSolved`, `ChiQuery.chiSolved`, `Solved.rebuildWithConstraint`).
  - Row8 `Translatability normalization` -> `No` because the live path enforces Definition 15.2.10 / Theorem 15.2.11 constructive translatability, but not §15.2.8’s stronger all-inert `W` normalization.
- Wording/evidence refreshes landed for rows 1, 3, 6, 12, and 13.
- Rows 4, 5, 7, 9, 10, 11, and 14 remained materially accurate under fresh review.

## 2026-03-07 improving-loop rerun closeout (rows 2 and 8)

- Row2 `Result-type context wiring` reclosed to `Yes` after the live pipeline switched to finalized `PresolutionView` artifacts and `ResultType.View` dropped the last solved-compat validation path.
- Row8 `Translatability normalization` reclosed to `Yes` after direct code + regression review confirmed that `rigidifyTranslatablePresolutionM` already performs §15.2.8 all-inert `W`-normalization on the live path; the earlier reopen was stale.
- Verification gates from the rerun:
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `row2 closeout guard` — PASS (`3 examples, 0 failures`)
  - `row8 thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `row8 translatability normalization guard` — PASS (`1 example, 0 failures`)
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS.
- With rows 2 and 8 reclosed, the live table returns `Yes` for all 14 mechanisms again.

## Rolling priorities after improving-loop rerun

1. Keep the row2/row8 closeout guards plus the full gate (`cabal build all && cabal test`) mandatory for future thesis-alignment changes.
2. Re-run a fresh per-row verification sweep before any future campaign-closeout claim.
3. Keep `Bugs.md`, `TODO.md`, and `implementation_notes.md` synchronized whenever a row classification changes.

## Note

The thesis does not keep the graph fully unaltered: propagation and unification transform it.
`What to change for thesis-exact` describes concrete work to reduce runtime scaffolding where it obscures thesis correspondence, while preserving behavior and explicit safety contracts when they are still carrying proof obligations in code.

## 2026-03-07 fresh round-2 row2/row8 closeout evidence

- Round 1 row2 closeout (`Result-type context wiring`):
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='row2 absolute thesis-exact guard'`
    - PASS (`1 example, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='row2 closeout guard'`
    - PASS (`3 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='checked-authoritative'`
    - PASS (`8 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='Dual-path verification'`
    - PASS (`4 examples, 0 failures`).
- Round 2 row8 closeout (`Translatability normalization`):
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='row8 thesis-exact guard'`
    - PASS (`1 example, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='Translatable presolution'`
    - PASS (`10 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='O15-TRANS'`
    - PASS (`5 examples, 0 failures`).
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='O05-'`
    - PASS (`3 examples, 0 failures`).
  - `cabal run frozen-parity-gen -- --generated-on 2026-03-07 --source-commit 816eb2e308091506a5b1b10b385a3a0984f92209`
    - PASS (baseline refreshed in `test/golden/legacy-replay-baseline-v1.json`).
  - `cabal test mlf2-test --test-show-details=direct --test-option=--match --test-option='Frozen parity artifact baseline'`
    - PASS (`1 example, 0 failures`).
- Final closeout gate:
  - `cabal build all && cabal test`
    - PASS.
- Fresh round-2 closeout outcome: all 14 mechanisms now evaluate to `Yes` on the live codebase.
