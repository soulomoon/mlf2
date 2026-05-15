# Round 235 Review

Date: 2026-05-15
Round: `round-235`
Milestone: `milestone-5`
Direction: `direction-5a-witness-constructor-invariants`
Extracted item: `item-5a-production-constructor-boundary-freeze`
Base branch: `master`
Branch: `orchestrator/round-235-witness-smart-constructors`

## Findings

- No blocking findings.
- Residual note: the selected slice makes the production witness-construction boundary authoritative, but milestone-5 is not complete yet because `mkInstanceWitness` remains a thin constructor seam and downstream context-heavy witness validation is still intentionally retained. Approval is therefore limited to `milestone-5: pending -> in-progress`.

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Checks Run

- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Result: pass. The assigned worktree is reviewing `round-235` on `orchestrator/round-235-witness-smart-constructors`, and the active bundle resolves to `roadmap_id = 2026-05-05-00-type-level-safety-singletons-roadmap`, `roadmap_revision = rev-001`, and `roadmap_dir = orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`.

- Command: `python3 - <<'PY' ...`
  Result: pass. `selection-record.json` and `round-plan-record.json` parse cleanly, match the live `roadmap_id` / `roadmap_revision` / `roadmap_dir`, select `milestone-5` / `direction-5a-witness-constructor-invariants`, and the active `roadmap-view.json` contains the `milestone-5`, `milestone-5-completion`, and `roadmap-history-completed-rounds` anchors needed for status-only closeout.

- Command: `git status --short`
  Result: pass. The live payload is limited to witness-boundary source/test/docs files plus the controller-owned `orchestrator/state.json` bookkeeping file and the reviewer-owned round artifacts written by this review. `orchestrator/state.json` is not treated as review payload.

- Command: `git diff --check`
  Result: pass. No whitespace or patch-format issues.

- Command: `git diff --name-only -- runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass. No output. `runtime/mlfp_io/target/release/libmlfp_io.d` exists in the tracked runtime tree but is unchanged in the live diff, so it is not part of the round-235 payload.

- Command: `git diff -- src/MLF/Constraint/Types/Witness.hs src/MLF/Constraint/Types/Witness/Internal.hs src/MLF/Constraint/Types/Witness/TestSupport.hs mlf2.cabal docs/architecture.md`
  Result: pass. The default `MLF.Constraint.Types.Witness` surface now exports abstract `EdgeWitness` / `InstanceWitness` types plus read-side selectors and smart constructors only; raw constructors moved to `MLF.Constraint.Types.Witness.Internal`; `MLF.Constraint.Types.Witness.TestSupport` is the explicit negative-fixture seam; `mlf2.cabal` registers the new owner/test-support modules in the private `mlf2-internal` library; and `docs/architecture.md` now describes the narrowed production boundary without claiming that all witness validity has moved to construction time.

- Command: `rg -n "MLF\\.Constraint\\.Types\\.Witness\\.TestSupport|MLF\\.Constraint\\.Types\\.Witness\\.Internal|EdgeWitness\\s*\\(\\.\\.\\)|InstanceWitness\\s*\\(\\.\\.\\)" src test`
  Result: pass. In `src/`, only `MLF.Constraint.Types.Witness` imports `Internal`, no production module imports `TestSupport`, and no production import reopens `EdgeWitness(..)` or `InstanceWitness(..)`. In `test/`, raw witness construction appears only through the explicit `TestSupport` seam.

- Command: `rg -n "mkEdgeWitness|mkInstanceWitness|EdgeWitness\\s*\\{|InstanceWitness\\s*\\{" src/MLF/Constraint/Presolution src/MLF/Elab`
  Result: pass. Production witness creation and reassembly paths now go through the smart-constructor seam. `buildEdgeWitness` uses `mkEdgeWitness`, `normalizeEdgeWitnessesM` rebuilds witnesses with `mkEdgeWitness`, and `canonicalizeWitness` rebuilds witnesses with `mkEdgeWitness`. No production `EdgeWitness { ... }` or `InstanceWitness { ... }` construction remains outside `Internal.hs`.

- Command: `cabal build mlf2-test`
  Result: pass. The focused test target rebuilt successfully after the witness-boundary changes.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "solver and solved internals stay behind owner and test-support seams"'`
  Result: pass. The repo guard ran `1 example, 0 failures`, confirming production code does not import the test-support seam.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution witness assembly guard"'`
  Result: pass. The source-guard test ran `1 example, 0 failures`, confirming the default witness module no longer exports raw constructors, the explicit test-support seam exists, and `WitnessNorm` / `Rewrite` rebuild through `mkEdgeWitness`.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 — Witness normalization"'`
  Result: pass. The focused witness-normalization suite ran `118 examples, 0 failures`, preserving the normalization and malformed-witness fail-closed behavior.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Φ translation soundness"'`
  Result: pass. The focused elaboration suite ran `54 examples, 0 failures`, preserving downstream malformed-witness rejection behavior through the explicit test seam.

- Command: `cabal build all && cabal test`
  Result: pass. The full behavior-changing gate completed locally with `2572 examples, 0 failures` in `355.7743` seconds.

- Command: `for r in 094 095 096 097 098; do sed -n '1,80p' "orchestrator/rounds/round-$r/review-record.json"; done`
  Result: pass. The inherited same-lane retained-child continuity chain remains authoritative through accepted/finalize reviewer records in `round-094` through `round-098`; round-235 does not alter those artifacts.

- Command: `rg -n "non-cyclic-graph = unknown|no-fallback = keep|no second interface is authorized|explicit-only|iso-recursive|equi-recursive|multi-SCC|cyclic search|fallback" docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
  Result: pass. The inherited recursive-inference settlement remains bounded: explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback constraints are still the accepted baseline, `non-cyclic-graph = unknown` remains pressure rather than authorization, and no cyclic-search, multi-SCC, second-interface, or fallback widening is approved. Round-235 does not touch any of those surfaces.

- Command: `python3 -m json.tool orchestrator/rounds/round-235/review-record.json >/dev/null`
  Result: pass. The reviewer machine artifact is valid JSON after authoring.

## Plan Compliance

- `Audit MLF.Constraint.Types.Witness against its real consumers and narrow the default production construction surface to the approved smart-constructor/accessor seam, so production modules stop depending on open raw constructors while still keeping the existing read-side access they need`: met. `MLF.Constraint.Types.Witness` now exports abstract witness types plus selectors and smart constructors only, while production import sites were narrowed to those read-side accessors.

- `Introduce or refine an explicit raw witness test seam for negative fixtures, then repoint the affected witness-domain and elaboration tests to that seam instead of relying on the default production export surface for malformed EdgeWitness / InstanceWitness literals`: met. `MLF.Constraint.Types.Witness.TestSupport` is the explicit negative-fixture seam, and the touched tests now import raw witness constructors only from that module.

- `Route every production witness creation or reassembly path through the approved constructor boundary, especially MLF.Constraint.Presolution.Witness.buildEdgeWitness and the normalized witness update in MLF.Constraint.Presolution.WitnessNorm, and lift only the invariants that those construction sites can actually guarantee at build time`: met. `buildEdgeWitness`, `normalizeEdgeWitnessesM`, and `canonicalizeWitness` all rebuild through `mkEdgeWitness`, and the round does not overclaim stronger construction-time guarantees than the live seam actually proves.

- `Keep MLF.Constraint.Presolution.WitnessValidation and elaboration/Phi fail-fast checks for any malformed cases that are still reachable outside the new guarantee. Remove a downstream validation only when the constructor boundary now proves the exact same invariant for the exact same input lane`: met. Downstream witness validation and Φ/elaboration fail-fast behavior remain live and covered by the focused normalization / soundness suites.

- `Add focused regression coverage for the selected boundary: source/export guards that prevent raw production construction from reappearing, preservation coverage that proves live presolution witness assembly still succeeds through the approved seam, and negative coverage showing malformed fixture paths remain explicit and fail closed rather than being weakened into smoke tests`: met. `PipelineSpec` guards the witness export surface and `mkEdgeWitness` rebuild sites, `RepoGuardSpec` blocks production imports of `TestSupport`, and the focused normalization / Φ soundness suites still exercise malformed witness failures through explicit raw fixtures.

- `Update durable guidance only where the accepted boundary changes materially, then run focused witness/elaboration checks, diff hygiene, and the full cabal build all && cabal test gate required for behavior-changing milestone-5 work`: met. Only `docs/architecture.md` changed on the durable-guidance side, and the focused plus full-gate validation all passed.

## Decision

**APPROVED**

## Evidence

- The default `MLF.Constraint.Types.Witness` production surface no longer permits raw construction of `EdgeWitness` or `InstanceWitness`. `Witness.hs` exports abstract witness types, read-side selectors (`getInstanceOps`, `ewEdgeId`, `ewLeft`, `ewRight`, `ewRoot`, `ewForallIntros`, `ewWitness`), and smart constructors only. Raw constructors now live in `Internal.hs` and are deliberately re-exposed only via `TestSupport.hs`.

- Production code does not import the explicit test-support seam. The source scan finds no `src/` import of `MLF.Constraint.Types.Witness.TestSupport`, and the repo guard passes locally. The only `src/` import of `MLF.Constraint.Types.Witness.Internal` is the owner façade `MLF.Constraint.Types.Witness`.

- The live production witness creation and reassembly paths now stay inside the smart-constructor boundary. `MLF.Constraint.Presolution.Witness.buildEdgeWitness` already constructs through `mkEdgeWitness`; this round replaces the remaining `EdgeWitness` record-update rebuilds in `MLF.Constraint.Presolution.WitnessNorm` and `MLF.Constraint.Presolution.Rewrite` with `mkEdgeWitness` reassembly, so no production bypass remains for the touched seam.

- Negative-fixture coverage is preserved rather than weakened. The affected tests still build deliberately malformed witness values, but they now do so through the explicit `TestSupport` module. The `Phase 3 — Witness normalization` and `Φ translation soundness` focused selectors remain green, which is evidence that malformed witness cases still fail closed downstream where the constructor seam does not yet prove the needed invariant.

- Scope stayed bounded. `src-public/` is untouched, no milestone-6 cleanup or broad fixture migration was pulled in, and no downstream validation was removed unless it was already subsumed by the live constructor guarantee. `docs/architecture.md` was updated only to describe the narrower production boundary accurately.

- Milestone-5 is not done yet. The selected slice freezes the production constructor boundary and makes it auditable, but the roadmap completion signal for milestone-5 also requires witness validation to cover the invariants required by consumers before downstream checks can be retired. The live boundary does not yet prove that full story, so the lawful closeout is `pending -> in-progress`, not `done`.
