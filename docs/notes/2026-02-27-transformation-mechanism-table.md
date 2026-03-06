# Transformation Mechanism Table (Thesis vs Codebase)

Last updated (UTC): 2026-03-07
Source revision: `b9c77d4`
Note: thesis exact includes test-only code paths

| Transformation mechanism | Thesis pipeline | Current codebase | What to change for thesis-exact | Thesis-exact |
|---|---|---|---|---|
| Elaboration input | Thesis elaboration consumes a translatable presolution `χp`; translation of `a` to xMLF is defined inductively on term shape (Fig. 15.3.5, §15.3.6). For each instantiation edge `e`, choose a propagation witness `I` and define `T(e)` from that witness (Def. 15.3.12); witness choice is intentionally non-deterministic ("pick any propagation witness"). (`papers/these-finale-english.txt:14087-14097`, `papers/these-finale-english.txt:14112-14117`) | The live production path is now `χp`-native end-to-end with explicit ga' binding-path failures on the scope-preference path: `resolveContext` no longer swallows `bindingPathToRootLocal` failures, and the absolute row-1 guard now covers both `MLF.Elab.Run.Scope` and `MLF.Constraint.Presolution.Plan.Context`. (`src/MLF/Elab/Run/Pipeline.hs:110-141`, `src/MLF/Constraint/Presolution/Plan/Context.hs:142-154`, `test/PipelineSpec.hs:205-214`, `test/ElaborationSpec.hs:4796-4840`) | Closed in Round 1 by removing the last live ga' scope-preference swallow and adding a behavioral regression for `resolveContext`. Keep mandatory regression gates: `resolveContext propagates ga base binding-path failures instead of falling back`, `elab-input absolute thesis-exact guard`, `checked-authoritative`, `Dual-path verification`, and `cabal build all && cabal test`. | Yes |
| Result-type context wiring | Thesis result-type construction is defined from translated artifacts (Def. 15.3.2) and chosen edge-witness evidence over `χp`; it does not prescribe a split solved adapter object. | Row2 wiring is now fully `χp`-first at runtime boundaries: `ResultTypeInputs` carries only `rtcPresolutionView` + edge artifacts, `ResultType.View` no longer exports/materializes `rtvSolved`/`rtvOriginalConstraint`/`solveFromInputs`, and Ann/Fallback/Util consumers resolve scope/reify/generalize through view-native helpers (`resolveCanonicalScopeView`, `canonicalizeScopeRefView`, `schemeBodyTargetView`, `reifyTypeFromView`). Strict malformed-view validation remains fail-fast at `buildResultTypeView` via canonical graph checks. (`src/MLF/Elab/Run/ResultType/Types.hs:1-39`, `src/MLF/Elab/Run/ResultType/View.hs:1-141`, `src/MLF/Elab/Run/ResultType/Ann.hs:70-306`, `src/MLF/Elab/Run/ResultType/Fallback.hs:52-714`, `src/MLF/Elab/Run/ResultType/Util.hs:38-73`) | Closed in Task 42 (absolute thesis-exact dependency hardening for row2). Keep `row2 absolute thesis-exact guard`, `row2 closeout guard`, `checked-authoritative`, `Dual-path verification`, and full gate in regression cadence. | Yes (absolute strict row2 runtime contract) |
| Ordering of transformations | `SolveConstraint`: ordered edge traversal with propagation + unification. (`papers/these-finale-english.txt` §12.1.3) | Presolution drains unification closure at edge boundaries and executes delayed-weaken flushing at owner transitions inside the edge loop. Boundary handling is now strict: queued weakens are owner-stamped at enqueue time, each boundary flushes only the closed-owner bucket (plus `Unknown`), and remaining non-next-owner buckets fail fast. The old flush-all-owner fallback is removed. Driver finalization keeps explicit fail-fast checks (`pending queues`, `TyExp` coverage/removal, `witness/trace` key alignment). (`src/MLF/Constraint/Presolution/EdgeProcessing.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`, `src/MLF/Constraint/Presolution/Driver.hs`) | Closed in Task 46 (owner-stamped strict owner-boundary scheduling). Keep mandatory regression gates: `row3 absolute thesis-exact guard`, `Phase 4 thesis-exact unification closure`, `Translatable presolution`, `checked-authoritative`, `Dual-path verification`, and full gate (`cabal build all && cabal test`). | Yes (strict owner-boundary ordering contract) |
| Per-edge propagation transform | Propagation expands schemes and adds unification obligations. (`papers/these-finale-english.txt` §10.3.2, §15.2.2) | `executeUnifiedExpansionPath` is now uniform across frontend `TyExp` edges: each edge computes minimal expansion (`decideMinimalExpansion`), merges with existing expansion (`mergeExpansions`), records edge expansion/trace/witness, and applies the same unification flow with no synthesized-wrapper-specific interpreter branch. (`src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs:53-89`) | Closed in orchestrated runner Round 1 (Attempt 2) with guardrails. Keep regression gates: `row4 per-edge propagation thesis-exact guard`, `Phase 6`, `checked-authoritative`, `Translatable presolution`, and full gate (`cabal build all && cabal test`). | Yes (uniform per-edge expansion + unification path in interpreter) |
| Graph operation execution (Graft/Merge/Weaken/Raise) | Operations are part of normalized witness derivations translated into computations. (`papers/these-finale-english.txt` Fig. 15.3.4) | Edge unification now calls a single edge-local omega execution entrypoint (`executeEdgeLocalOmegaOps`) from `runExpansionUnify`; direct pre/post omega invocations are removed from the edge-processing callsite while preserving operation ordering semantics internally. (`src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs:148-159`, `src/MLF/Constraint/Presolution/EdgeUnify.hs:199-212`, `test/PipelineSpec.hs:277-284`) | Closed in orchestrated runner Round 2 (Attempt 1). Keep regression gates: `row5 graph-op execution thesis-exact guard`, `Phase 4 thesis-exact unification closure`, `Phase 3 — Witness normalization`, `Witness translation`, and full gate (`cabal build all && cabal test`). | Yes (single edge-local graph-op execution entrypoint at callsite) |
| Replay-map producer normalization (upfront strict contract) | Witness artifacts must identify binder targets consistently with propagation witnesses. (`papers/these-finale-english.txt` §15.2.2, §15.3.5) | Producer-side replay-map normalization is now strict at the producer boundary: `WitnessNorm` classifies replay/no-replay behavior in restored source identity space, retains strict no-replay only when a surviving source-domain `OpWeaken` remains, rejects residual rogue no-replay replay-family ops, and emits `ReplayMapIncomplete` when the active source domain is under-covered. Driver validation also hard-rejects replay codomain non-injectivity. (`src/MLF/Constraint/Presolution/WitnessNorm.hs:253-387`, `src/MLF/Constraint/Presolution/Driver.hs:310-326`, `test/Presolution/WitnessSpec.hs:1662-1704`, `test/Presolution/WitnessSpec.hs:1819-1889`, `test/Phi/AlignmentSpec.hs:41-49`) | Closed. Keep producer-boundary regression gates for source-domain under-coverage, codomain injectivity, empty replay-domain alignment, `checked-authoritative`, `Dual-path verification`, and `cabal build all && cabal test`. | Yes |
| Replay-map consumer bridge in Phi | Phi consumes normalized witness artifacts; thesis presentation has no runtime repair layer. (`papers/these-finale-english.txt` §15.3.5) | `computeTraceBinderReplayBridge` is now a strict validation/pass-through boundary: it rejects trace/replay source-domain mismatches and replay codomain escapes, then hands the producer-normalized replay map unchanged to Ω. Downstream Φ/Ω fail fast on unresolved replay targets rather than synthesizing repairs. (`src/MLF/Elab/Phi/Translate.hs:269-360`, `src/MLF/Elab/Phi/Omega.hs:651-682`, `src/MLF/Elab/Phi/Omega.hs:759-805`, `test/ElaborationSpec.hs:2098-2250`, `test/Phi/AlignmentSpec.hs:39-58`) | Closed in the 2026-03-07 sweep: keep row-7 regression gates for source-domain parity, replay codomain membership, canonical-alias hard reject, empty replay-domain alignment, and the full suite (`cabal build all && cabal test`). | Yes |
| Translatability normalization | Uses inert weakening assumptions/normalization for translatable presolutions. (`papers/these-finale-english.txt` §15.2.7-§15.2.8) | Production presolution still performs the thesis-required constructive normalization steps explicitly: translatability validation rejects inert-locked constraints, and rigidification weakens/rigidifies scheme roots, arrow nodes, tycon nodes, and other non-interior flex children before translation. (`src/MLF/Constraint/Presolution/Validation.hs:41-189`, `src/MLF/Constraint/Inert.hs:101-156`, `test/TranslatablePresolutionSpec.hs:12-53`, `test/Presolution/EnforcementSpec.hs:24-92`) | Closed in the 2026-03-07 sweep: keep `Translatable presolution`, `Translatable presolution enforcement`, inert-locked, and full-suite regression gates so future refactors preserve the constructive obligations from Def. 15.2.10/§15.2.8. | Yes |
| Canonicalization source used by Phi | Canonicalization is implicit in derivation/equivalence reasoning over `χp`. (`papers/these-finale-english.txt` §15.3.5-§15.3.6) | Replay-bridge construction still validates witness-domain trace/replay key spaces first, but Ω no longer performs post-bridge source-candidate recovery. `OpRaise` now chooses only the direct replay/source target, fails fast when no direct target exists, and uses at most the forward `etCopyMap` alias as witness-authoritative evidence for source-domain interior membership; canonicalization remains downstream lookup machinery instead of a source-selection repair layer. (`src/MLF/Elab/Phi/Omega.hs:611-755`, `src/MLF/Elab/Phi/Omega.hs:922-930`, `test/ElaborationSpec.hs:2000-2039`, `test/ElaborationSpec.hs:2430-2462`, `test/PipelineSpec.hs:287-292`) | Closed in Round 2. Keep regression gates: `source-space identity replay target`, both strict `OpRaise` fail-fast slices, `row9-11 direct-target guard`, `IdentityBridge`, `checked-authoritative`, `Dual-path verification`, and `cabal build all && cabal test`. | Yes |
| Identity reconciliation mechanism | Thesis elaboration carries identity directly through named nodes, computation contexts, witness-derived computations (`ε`, `ϕR`, `T(e)`), and quantifier-spine order; it does not define a separate identity-reconciliation layer. (`papers/these-finale-english.txt:13547-13553`, `:13690-13707`, `:13827-13830`, `:13855-13858`, `:13942-13954`, `:14087-14097`, `:14114-14124`) | Runtime Φ/Ω no longer reconciles alternative source candidates through local helper ranking. Binder/target resolution is direct from replay/spine identities, while `IdentityBridge` is retained only as a witness-domain utility/diagnostic surface and dedicated unit-test target rather than a runtime recovery layer. (`src/MLF/Elab/Phi/Omega.hs:611-755`, `src/MLF/Elab/Phi/Omega.hs:1174-1237`, `src/MLF/Elab/Phi/IdentityBridge.hs:142-260`, `test/Phi/IdentityBridgeSpec.hs:1-235`, `test/PipelineSpec.hs:287-292`) | Closed in Round 2 by deleting Ω’s source-candidate reconciliation helpers and keeping witness-domain identity utilities non-authoritative for runtime target repair. Preserve the same row9-11 regression stack plus full-suite validation. | Yes |
| Non-root weaken/raise binder resolution | Thesis translation follows witness/operation structure; unresolved witness targets are not success cases. (`papers/these-finale-english.txt:13986-14003`) | Non-root `OpWeaken` remains direct and fail-fast once a replay target is supplied, and non-root `OpRaise` now likewise requires a direct replay/source target instead of degrading unresolved non-trace targets to `ε`/no-op. The only remaining alias allowance is forward `etCopyMap` evidence for source-domain interior membership, which stays inside witness artifacts rather than runtime recovery. (`src/MLF/Elab/Phi/Omega.hs:720-755`, `src/MLF/Elab/Phi/Omega.hs:922-930`, `src/MLF/Elab/Phi/Omega.hs:1219-1237`, `test/ElaborationSpec.hs:2000-2039`, `test/ElaborationSpec.hs:2390-2462`) | Closed in Round 2. Keep regression gates for source-space replay-target fail-fast, both strict `OpRaise` unresolved-target slices, the row9-11 direct-target guard, `checked-authoritative`, `Dual-path verification`, and the full suite. | Yes |
| Graph mutation during solve/presolution | Thesis transformations mutate constraints via propagation + unification. (`papers/these-finale-english.txt` §12.1.6 proof) | Production matches that phase boundary: presolution owns the graph mutations (closure drains, expansion materialization, weakening, rigidification, witness normalization), and the runtime pipeline consumes the resulting presolution-native solved artifacts without a separate solve/rewrite layer in the elaboration entrypoint. (`src/MLF/Elab/Run/Pipeline.hs:75-113`, `test/PipelineSpec.hs:414-447`) | Closed in the 2026-03-07 sweep: keep `uses presolution-native solved artifacts`, `runtime snapshot rebuild stays stable across representative corpus`, and the full suite green. | Yes |
| Dual-path verification mechanism | N/A in thesis | Production elaboration remains single-path (`runPipelineElabWith`); dual-path and frozen-parity coverage live only in the test harness as engineering guardrails, not in runtime control flow. (`src/MLF/Elab/Run/Pipeline.hs:75-90`, `test/FrozenParitySpec.hs:12-18`, `test/PipelineSpec.hs:302-356`, `test/ElaborationSpec.hs:311-319`) | Closed in the 2026-03-07 sweep: keep `Dual-path verification`, `Frozen parity artifact baseline`, and the full suite as non-runtime safety nets. | Yes |
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

## 2026-03-05 orchestrated runner row6 status

- Round 3 targeted row6 (`Replay-map producer normalization`).
- Attempt 1 improved strictness but thesis gate remained `NO`.
- Attempts 2-6 reached bounded-scope feasibility `NO` (producer-only closure blocked by producer/consumer coupling).
- Terminal outcome for that run: `MAXIMUMRETRY` while QA baseline stayed green.

## Post-row5 priority ordering

1. Keep closed-row guardrails for rows 1-5 mandatory (`elab-input absolute thesis-exact guard`, `row2 absolute thesis-exact guard`, `row3 absolute thesis-exact guard`, `row4 per-edge propagation thesis-exact guard`, `row5 graph-op execution thesis-exact guard`, `checked-authoritative`, `Dual-path verification`).
2. Treat row6 as cross-phase work: plan one coordinated producer+consumer replay-map contract change (and replay-trace contract updates only if strictly required) rather than producer-only incremental attempts.
3. Maintain full-gate cadence (`cabal build all && cabal test`) after each row closeout or blocked-mode evidence run.

## Note

The thesis does not keep the graph fully unaltered: propagation and unification transform it.
`What to change for thesis-exact` describes concrete work to reduce runtime scaffolding where it obscures thesis correspondence, while preserving behavior and explicit safety contracts when they are still carrying proof obligations in code.
