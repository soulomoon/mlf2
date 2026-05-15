# Round 236 Review Attempt 1

Date: 2026-05-15
Round: `round-236`
Milestone: `milestone-5`
Direction: `direction-5a-witness-constructor-invariants`
Extracted item: `item-5a-instance-witness-validation-seam`
Base branch: `master`
Branch: `orchestrator/round-236-witness-invariant-closure`

## Findings

- No blocking findings in the selected witness-invariant slice.
- Residual operational note: rerunning `cabal build all && cabal test` in this worktree rewrote the tracked generated file `runtime/mlfp_io/target/release/libmlfp_io.d` to worktree-local paths. That file was clean before the full-gate rerun, so treat it as reviewer-induced build dirt that must be cleaned or excluded before merge rather than approved round payload.

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Checks Run

- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Result: pass. The assigned worktree is reviewing `round-236` on `orchestrator/round-236-witness-invariant-closure`, and the active bundle resolves to `roadmap_id = 2026-05-05-00-type-level-safety-singletons-roadmap`, `roadmap_revision = rev-001`, and `roadmap_dir = orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`.

- Command: `python3 - <<'PY' ...`
  Result: pass. `selection-record.json` and `round-plan-record.json` parse cleanly, match the live roadmap metadata, select `milestone-5` / `direction-5a-witness-constructor-invariants` / `item-5a-instance-witness-validation-seam`, and the active `roadmap-view.json` contains the `milestone-5`, `milestone-5-completion`, and `roadmap-history-completed-rounds` anchors needed for status-only closeout.

- Command: `roadmap_dir=$(python3 - <<'PY' ... ) && if test -f "$roadmap_dir/retry-subloop.md"; then echo present; else echo absent; fi`
  Result: pass. The file is absent, and the current family records retry policy in `verification.md` under `## Roadmap Overrides`, so this is not a blocker under the active bundle contract.

- Command: `git diff --check`
  Result: pass. No whitespace or patch-format issues.

- Command: `git diff --name-only -- runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass before the full-gate rerun. The selected round payload did not initially include the generated runtime depfile.

- Command: `rg -n "import qualified MLF\\.Constraint\\.Types\\.Witness\\.Internal|import MLF\\.Constraint\\.Types\\.Witness\\.Internal|MLF\\.Constraint\\.Types\\.Witness\\.TestSupport|EdgeWitness\\s*\\(\\.\\.\\)|InstanceWitness\\s*\\(\\.\\.\\)" src test -g '!dist-newstyle/**'`
  Result: pass. Production code keeps the default `MLF.Constraint.Types.Witness` surface abstract, raw constructors stay behind `TestSupport`, and the only production import of `Witness.Internal` outside the owner façade is the owner-local `MLF.Constraint.Presolution.Witness` module that carries the unchecked accumulation seam.

- Command: `rg -n "validatedInstanceOpsAfterNormalization|mkUncheckedInstanceWitness|mkInstanceWitness" src/MLF/Constraint src/MLF/Elab test -g '!dist-newstyle/**'`
  Result: pass. The finalized packaging helper is used only in `MLF.Constraint.Presolution.WitnessNorm`, unchecked list construction stays on the explicit owner-local `mkUncheckedInstanceWitness` seam in `MLF.Constraint.Presolution.Witness`, and tests cover the split directly.

- Command: `git diff -- src/MLF/Constraint/Types/Witness/Internal.hs src/MLF/Constraint/Types/Witness.hs src/MLF/Constraint/Presolution/Witness.hs src/MLF/Constraint/Presolution/WitnessNorm.hs test/PipelineSpec.hs test/Presolution/WitnessSpec.hs docs/architecture.md`
  Result: pass. The diff matches the selected slice: `mkInstanceWitness` now requires `ValidatedInstanceOps`, unchecked accumulation is explicit and owner-local, `WitnessNorm` validates before finalized packaging, source/behavior guards were added, and the retained downstream validation owners are documented without widening scope.

- Command: `cabal build mlf2-test`
  Result: pass. The focused test target rebuilt successfully after the witness-construction changes.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution witness assembly guard"'`
  Result: pass. The source-guard test ran `1 example, 0 failures`, confirming the split between the validated finalized seam, the unchecked owner-local seam, and the retained abstract default witness surface.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "normalizeInstanceOpsFull produces validated witnesses when it succeeds"'`
  Result: pass. The normalization/property selector ran `2 examples, 0 failures`, including the new assertion that successful normalization plus `validatedInstanceOpsAfterNormalization` feeds `mkInstanceWitness` without altering the normalized ops.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "normalization derives strict replay lane from edge semantics and maps codomain to edge-root replay binders"'`
  Result: pass. The strict replay selector ran `2 examples, 0 failures`.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "normalization prunes no-replay non-root raise wrappers before Phi"'`
  Result: pass. The no-replay selector ran `2 examples, 0 failures`.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Φ translation soundness"'`
  Result: pass. The elaboration/Phi selector ran `54 examples, 0 failures`, so the round keeps the downstream fail-closed Φ behavior green.

- Command: `cabal build all && cabal test`
  Result: pass. The full behavior-changing gate completed locally with `2572 examples, 0 failures` in `361.7127` seconds.

- Command: `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d | sed -n '1,120p'`
  Result: note. After the full-gate rerun, the tracked generated depfile now points at the round worktree path instead of `/Volumes/src/mlf4/...`. This is a reviewer-induced build side effect, not approved source payload.

- Command: `rg -n "explicit-only|non-cyclic-graph|fallback|multi-SCC|iso-recursive|equi-recursive" docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
  Result: pass. The inherited recursive-inference settlement remains unchanged: explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback boundaries still hold, `non-cyclic-graph = unknown` remains bounded pressure only, and round-236 does not touch or reopen the accepted `round-094` through `round-098` chain or earlier `round-001` through `round-093` evidence.

## Decision

**APPROVED**

## Evidence

- `src/MLF/Constraint/Types/Witness/Internal.hs` now separates finalized witness construction from unchecked accumulation: `ValidatedInstanceOps` is the proof token, `mkInstanceWitness :: ValidatedInstanceOps -> InstanceWitness` is the default finalized constructor, and `mkUncheckedInstanceWitness :: [InstanceOp] -> InstanceWitness` is the explicit owner-local raw seam.

- `src/MLF/Constraint/Types/Witness.hs` keeps the production witness surface abstract while exposing the proof-token type and finalized constructor only. `MLF.Constraint.Presolution.Witness` carries the unchecked pre-normalization seam explicitly, and `MLF.Constraint.Presolution.WitnessNorm` now calls `validateNormalizedWitness ...` before minting finalized witnesses via `validatedInstanceOpsAfterNormalization` and `mkInstanceWitness`.

- `test/PipelineSpec.hs` and `test/Presolution/WitnessSpec.hs` add both source guards and behavior guards for the new split, while the strict replay, no-replay, and Φ soundness selectors stay green.

- No downstream validation was removed, and that is the correct outcome for this slice. `docs/architecture.md` now records that the finalized token closes raw-list packaging on the post-normalization lane, while Ω normalization and Φ translation checks that are not subsumed by that token remain downstream owners.

- Milestone `5` is now complete. Round `235` froze the abstract production constructor boundary and explicit test seam; round `236` closes the remaining `InstanceWitness` gap by requiring a validation-owned token on the finalized lane, keeping unchecked accumulation owner-local, and documenting the retained non-subsumed downstream validation. Together with the passing full gate, that satisfies the milestone-5 completion signal.
