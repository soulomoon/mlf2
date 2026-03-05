# Transformation Mechanism Table (Thesis vs Codebase)

Last updated (UTC): 2026-03-05
Source revision: `035ec16`
Note: thesis exact includes test-only code paths

| Transformation mechanism | Thesis pipeline | Current codebase | What to change for thesis-exact | Thesis-exact |
|---|---|---|---|---|
| Elaboration input | Thesis elaboration consumes a translatable presolution `χp`; translation of `a` to xMLF is defined inductively on term shape (Fig. 15.3.5, §15.3.6). For each instantiation edge `e`, choose a propagation witness `I` and define `T(e)` from that witness (Def. 15.3.12); witness choice is intentionally non-deterministic ("pick any propagation witness"). (`papers/these-finale-english.txt:14087-14097`, `papers/these-finale-english.txt:14112-14117`) | Active production path remains `χp`-native end-to-end (`presolutionViewForGen` -> `generalizeAtWithBuilderView` -> `elaborateWithEnv`). Task 41 tightened strict all-path contracts by removing residual non-thesis surfaces: `PhiEnv` no longer exposes solved-backed fields/accessors (`peResult`/`askResult` removed), ga' scope preference no longer swallows binding-tree errors (`Left _ -> ref` removed; errors propagate), and test-only Φ no longer exports/implements `phiFromEdgeWitnessAutoTrace` (no-trace helper remains fail-fast as `MissingEdgeTrace`). The absolute guard now asserts these surfaces are absent. (`src/MLF/Elab/Run/Pipeline.hs:110-141`, `src/MLF/Elab/Phi/Env.hs:42-74`, `src/MLF/Elab/Run/Scope.hs:103-113`, `src/MLF/Elab/Phi/TestOnly.hs:9-63`, `test/PipelineSpec.hs:205-213`) | Closed in Task 41 (absolute strict all-path hardening). Keep mandatory regression gates: `elab-input absolute thesis-exact guard`, `checked-authoritative`, `Dual-path verification`, and full gate (`cabal build all && cabal test`). | Yes (absolute strict all-path contract for this row, including test-only paths) |
| Result-type context wiring | Thesis result-type construction is defined from translated artifacts (Def. 15.3.2) and chosen edge-witness evidence over `χp`; it does not prescribe a split solved adapter object. | Row2 wiring is now fully `χp`-first at runtime boundaries: `ResultTypeInputs` carries only `rtcPresolutionView` + edge artifacts, `ResultType.View` no longer exports/materializes `rtvSolved`/`rtvOriginalConstraint`/`solveFromInputs`, and Ann/Fallback/Util consumers resolve scope/reify/generalize through view-native helpers (`resolveCanonicalScopeView`, `canonicalizeScopeRefView`, `schemeBodyTargetView`, `reifyTypeFromView`). Strict malformed-view validation remains fail-fast at `buildResultTypeView` via canonical graph checks. (`src/MLF/Elab/Run/ResultType/Types.hs:1-39`, `src/MLF/Elab/Run/ResultType/View.hs:1-141`, `src/MLF/Elab/Run/ResultType/Ann.hs:70-306`, `src/MLF/Elab/Run/ResultType/Fallback.hs:52-714`, `src/MLF/Elab/Run/ResultType/Util.hs:38-73`) | Closed in Task 42 (absolute thesis-exact dependency hardening for row2). Keep `row2 absolute thesis-exact guard`, `row2 closeout guard`, `checked-authoritative`, `Dual-path verification`, and full gate in regression cadence. | Yes (absolute strict row2 runtime contract) |
| Ordering of transformations | `SolveConstraint`: ordered edge traversal with propagation + unification. (`papers/these-finale-english.txt` §12.1.3) | Presolution drains unification closure at edge boundaries and executes delayed-weaken flushing at owner transitions inside the edge loop. Boundary handling is now strict: queued weakens are owner-stamped at enqueue time, each boundary flushes only the closed-owner bucket (plus `Unknown`), and remaining non-next-owner buckets fail fast. The old flush-all-owner fallback is removed. Driver finalization keeps explicit fail-fast checks (`pending queues`, `TyExp` coverage/removal, `witness/trace` key alignment). (`src/MLF/Constraint/Presolution/EdgeProcessing.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`, `src/MLF/Constraint/Presolution/Driver.hs`) | Closed in Task 46 (owner-stamped strict owner-boundary scheduling). Keep mandatory regression gates: `row3 absolute thesis-exact guard`, `Phase 4 thesis-exact unification closure`, `Translatable presolution`, `checked-authoritative`, `Dual-path verification`, and full gate (`cabal build all && cabal test`). | Yes (strict owner-boundary ordering contract) |
| Per-edge propagation transform | Propagation expands schemes and adds unification obligations. (`papers/these-finale-english.txt` §10.3.2, §15.2.2) | `executeUnifiedExpansionPath` is now uniform across frontend `TyExp` edges: each edge computes minimal expansion (`decideMinimalExpansion`), merges with existing expansion (`mergeExpansions`), records edge expansion/trace/witness, and applies the same unification flow with no synthesized-wrapper-specific interpreter branch. (`src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs:53-89`) | Closed in orchestrated runner Round 1 (Attempt 2) with guardrails. Keep regression gates: `row4 per-edge propagation thesis-exact guard`, `Phase 6`, `checked-authoritative`, `Translatable presolution`, and full gate (`cabal build all && cabal test`). | Yes (uniform per-edge expansion + unification path in interpreter) |
| Graph operation execution (Graft/Merge/Weaken/Raise) | Operations are part of normalized witness derivations translated into computations. (`papers/these-finale-english.txt` Fig. 15.3.4) | Edge unification now calls a single edge-local omega execution entrypoint (`executeEdgeLocalOmegaOps`) from `runExpansionUnify`; direct pre/post omega invocations are removed from the edge-processing callsite while preserving operation ordering semantics internally. (`src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs:148-159`, `src/MLF/Constraint/Presolution/EdgeUnify.hs:199-212`, `test/PipelineSpec.hs:277-284`) | Closed in orchestrated runner Round 2 (Attempt 1). Keep regression gates: `row5 graph-op execution thesis-exact guard`, `Phase 4 thesis-exact unification closure`, `Phase 3 — Witness normalization`, `Witness translation`, and full gate (`cabal build all && cabal test`). | Yes (single edge-local graph-op execution entrypoint at callsite) |
| Replay-map producer normalization (upfront strict contract) | Witness artifacts must identify binder targets consistently with propagation witnesses. (`papers/these-finale-english.txt` §15.2.2, §15.3.5) | Producer-side replay-map construction is still partially heuristic and compatibility-coupled: `WitnessNorm` still includes candidate-pool assignment and synthesis/projection/no-replay compatibility paths, while consumer-side Phi replay bridge/target recovery remains active. (`src/MLF/Constraint/Presolution/WitnessNorm.hs:170-229`, `:372-378`, `:446-470`, `:501-529`, `src/MLF/Elab/Phi/Translate.hs:473-572`, `src/MLF/Elab/Phi/Omega.hs:1306-1396`) | Remaining blocker is cross-phase coupling. To close this row, execute one coordinated redesign across producer + consumer (+ replay-trace contract if needed), not producer-only bounded attempts. Keep explicit fail-fast contract tests while doing this. | No |
| Replay-map consumer bridge in Phi | Phi consumes normalized witness artifacts; thesis presentation has no runtime repair layer. (`papers/these-finale-english.txt` §15.3.5) | `computeTraceBinderReplayBridge` enforces strict source-domain parity + codomain membership and pass-through targets at bridge construction; downstream Φ/Ω performs constrained replay-alias recovery for non-root `OpGraft`/`OpWeaken` and fails fast when recovery cannot be justified. (`src/MLF/Elab/Phi/Translate.hs:488-572`, `src/MLF/Elab/Phi/Omega.hs:1306-1383`) | Potential simplification: reduce bridge/recovery indirection if Φ can consume chosen witness/replay artifacts directly while preserving fail-fast contracts; thesis does not require a specific runtime object model. | No |
| Translatability normalization | Uses inert weakening assumptions/normalization for translatable presolutions. (`papers/these-finale-english.txt` §15.2.7-§15.2.8) | Explicit translatability validation + rigidification (scheme roots, arrow/tycon, non-interior flex children) and inert-locked weakening utilities. (`src/MLF/Constraint/Presolution/Validation.hs:45-103`, `:120-189`, `src/MLF/Constraint/Inert.hs:101-156`) | Keep validation plus construction steps that enforce Def. 15.2.10/§15.2.8 (including inert-locked weakening). Any rigidification refactor must preserve these constructive obligations, not just end-state checks. | No |
| Canonicalization source used by Phi | Canonicalization is implicit in derivation/equivalence reasoning over `χp`. (`papers/these-finale-english.txt` §15.3.5-§15.3.6) | Φ resolves binder/source identity trace-domain first: replay-bridge construction operates on raw `etBinderArgs` keys and replay-map domain/codomain checks; `IdentityBridge` key canonicalizers are identity (`canonicalKeyForNode`/`canonicalKeyForSource`), with solved canonicalization applied separately for alias reconciliation and later scope/node lookups. (`src/MLF/Elab/Phi/Translate.hs:286-327`, `:492-593`, `src/MLF/Elab/Phi/IdentityBridge.hs:70-74`, `:140-156`) | Unify canonicalization into `χp`-level node identity: use the presolution's own merged-node equivalences (from unification) as the single source of canonical identity. Remove the split between trace-domain raw keys and solved-canonicalization for alias reconciliation. | No |
| Identity reconciliation mechanism | No explicit bridge object in thesis presentation. | `IdentityBridge` now restricts identity candidates to raw/copy/trace witness provenance and exact binder-key matching (no runtime equivalence-class expansion fallback). (`src/MLF/Elab/Phi/IdentityBridge.hs:140-173`, `:205-246`) | Optional simplification: shrink `IdentityBridge` responsibilities if equivalent witness-domain semantics are preserved; the thesis presentation does not mandate or forbid a dedicated bridge object. | No |
| Non-root weaken/raise binder resolution | Thesis translation follows witness/operation structure; unresolved witness targets are not success cases. (`papers/these-finale-english.txt` §15.3.5) | Ω resolves non-root `OpWeaken` via replay-map/source-alias binder lookup and fails fast if unresolved; unresolved trace-source `OpRaise` targets fail with invariant error. (`src/MLF/Elab/Phi/Omega.hs:796-829`, `:848-856`) | Prefer target resolution from operation structure plus chosen witness/replay mapping with minimal indirection; unresolved targets should remain fail-fast. | No |
| Graph mutation during solve/presolution | Thesis transformations mutate constraints via propagation + unification. (`papers/these-finale-english.txt` §12.1.6 proof) | Presolution mutates constraints by draining closure around inst edges, then materializing expansions, flushing delayed weakens, rewriting/canonicalizing, rigidifying, and witness-normalizing before emitting `PresolutionResult`. Production pipeline finalizes solved snapshots inline in `MLF.Elab.Run.Pipeline` before elaboration. (`src/MLF/Elab/Run/Pipeline.hs:87-113`) | Keep reducing post-presolution runtime scaffolding that has no thesis counterpart (especially internal solved-adapter layers) while preserving current checked-authoritative behavior and guard coverage. | No |
| Dual-path verification mechanism | N/A in thesis | Production elaboration is single-path (`runPipelineElabWith`); verification confidence is maintained by frozen artifacts plus runtime guard slices (`row2 closeout guard`, `checked-authoritative`, `Dual-path verification`). (`src/MLF/Elab/Run/Pipeline.hs:75-90`, `test/FrozenParitySpec.hs:12-18`, `test/PipelineSpec.hs:176-259`, `test/ElaborationSpec.hs:311-319`) | Keep production elaboration thesis-direct and keep these guardrails as engineering safety nets for post-row2 cleanup. | No |
| Campaign classification status | N/A in thesis | DEV-TMT campaign items are retired in repo metadata: `DEV-TMT-*` entries appear under `history.resolved` with no active DEV-TMT IDs in `deviations`. (`docs/thesis-deviations.yaml:3-91`, `:131-303`) | N/A (repo metadata, not a runtime mechanism). Already aligned; no thesis-exactness dimension applies. | N/A |

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
