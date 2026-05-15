# Round 237 Review Attempt 1

Date: 2026-05-15
Round: `round-237`
Milestone: `milestone-6`
Direction: `direction-6a-integration-cleanup`
Extracted item: `item-6a-closeout-audit-and-guard-alignment`
Base branch: `master`
Branch: `orchestrator/round-237-integration-cleanup-closeout`

## Findings

- No blocking findings in the selected milestone-6 closeout slice.
- Residual operational note: rerunning `cabal build all && cabal test` in this worktree rewrote the tracked generated file `runtime/mlfp_io/target/release/libmlfp_io.d` to round-worktree-local paths. That file was clean before the full-gate rerun, so treat it as reviewer-induced build dirt that must be restored or excluded before merge rather than approved round payload.

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Checks Run

- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Result: pass. The assigned worktree is reviewing `round-237` on `orchestrator/round-237-integration-cleanup-closeout`, and the active bundle resolves to `roadmap_id = 2026-05-05-00-type-level-safety-singletons-roadmap`, `roadmap_revision = rev-001`, and `roadmap_dir = orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`.

- Command: `python3 - <<'PY' ...`
  Result: pass. `selection-record.json` and `round-plan-record.json` parse cleanly, match the live roadmap metadata, select `milestone-6` / `direction-6a-integration-cleanup` / `item-6a-closeout-audit-and-guard-alignment`, and the active `roadmap-view.json` contains the `milestone-6`, `milestone-6-completion`, and `roadmap-history-completed-rounds` anchors needed for status-only closeout.

- Command: `roadmap_dir=$(python3 - <<'PY' ... ) && if test -f "$roadmap_dir/retry-subloop.md"; then echo present; else echo absent; fi`
  Result: pass. The file is absent, and the current family records retry policy in `verification.md` under `## Roadmap Overrides`, so this is not a blocker under the active bundle contract.

- Command: `git diff --check`
  Result: pass. No whitespace or patch-format issues.

- Command: `git diff --name-only -- runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass before the full-gate rerun. The selected round payload did not initially include the tracked generated runtime depfile.

- Command: `git diff -- AGENTS.md CHANGELOG.md docs/architecture.md src/MLF/Elab/Run/Generalize/Prepare.hs src/MLF/Elab/Run/Pipeline.hs test/PipelineSpec.hs test/RepoGuardSpec.hs`
  Result: pass. The diff stays inside the selected milestone-6 integration/cleanup surfaces: the duplicated generalization-preparation base-constraint field is removed, docs and closeout guards are aligned, `src-public/` stays untouched, and there is no dependency or Cabal registration churn.

- Command: `rg -n "pgaAcyclicBaseConstraint|gaBaseConstraint|mkUncheckedInstanceWitness|NodeRefTag|to[A-Z][A-Za-z]*Constraint|rephaseConstraint" src/MLF AGENTS.md docs/architecture.md CHANGELOG.md test/PipelineSpec.hs test/RepoGuardSpec.hs -g '!dist-newstyle/**'`
  Result: pass. `pgaAcyclicBaseConstraint` is retired from production and guard surfaces; `GaBindParents.gaBaseConstraint` remains the owner-local base-constraint projection used by result-type recovery and generalization; `mkUncheckedInstanceWitness` remains an explicit owner-local seam; `NodeRefTag` remains the typed boundary vocabulary; and only the directional `to*Constraint` helpers are exported while `rephaseConstraint` stays private to `MLF.Constraint.Types.Graph`.

- Command: `cabal build mlf2-test`
  Result: pass. The focused test target rebuilt successfully after the closeout changes.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'`
  Result: pass. The repository guard suite ran `27 examples, 0 failures`, including the new milestone-6 guidance/owner-local seam synchronization guard.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`
  Result: pass. The presolution façade guard suite ran `8 examples, 0 failures`, keeping the milestone-3/5 inherited closeout boundaries stable while this round closes milestone-6.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "assembly helper guard: generalization preparation owns shared artifact assembly"'`
  Result: pass. The source-guard selector ran `1 example, 0 failures`, confirming the generalization-preparation assembly logic remains owner-local to `MLF.Elab.Run.Generalize.Prepare` and the retired outer-field shim stays absent.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution witness assembly guard"'`
  Result: pass. The source-guard selector ran `1 example, 0 failures`, confirming the retained `mkUncheckedInstanceWitness` seam remains explicit and owner-local.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "prepared generalization artifact drives redirecting instantiation behavior"'`
  Result: pass. The behavioral selector ran `1 example, 0 failures`, proving the pipeline still recovers copy provenance, canonical scope, root generalization, and result-type reconstruction through `pgaBindParentsGa.gaBaseConstraint` after removing the duplicated `pgaAcyclicBaseConstraint` field.

- Command: `cabal build all && cabal test`
  Result: pass. The full behavior-changing gate completed locally with `2573 examples, 0 failures` in `356.0691` seconds.

- Command: `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d | sed -n '1,160p'`
  Result: note. After the full-gate rerun, the tracked generated depfile now points at the round worktree path instead of `/Volumes/src/mlf4/...`. This is a reviewer-induced build side effect, not approved source payload.

- Command: `rg -n "explicit-only|non-cyclic-graph|fallback|multi-SCC|iso-recursive|equi-recursive" docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md"`
  Result: pass. The inherited recursive-inference settlement remains unchanged: explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback boundaries still hold, `non-cyclic-graph = unknown` remains bounded architecture-pressure only, and round-237 does not reopen the accepted `round-094` through `round-098` chain or earlier `round-001` through `round-093` evidence.

## Plan Compliance

- `Audit the live milestone-6 integration surface in src/MLF/Constraint/Types/Graph*, src/MLF/Constraint/Types/Phase*, src/MLF/Constraint/Types/Witness*, src/MLF/Constraint/Presolution/Witness*, src/MLF/Elab/Run/Generalize/Prepare.hs, AGENTS.md, docs/architecture.md, CHANGELOG.md, and the existing guard specs to classify each remaining seam as either an accepted owner-local boundary or a true broad migration shim`: met. The review audit confirms the retained seams are the accepted mixed `NodeRef` storage with typed `NodeRefTag` boundaries, directional phase rephasers, the explicit pre-normalization `mkUncheckedInstanceWitness` seam, and `GaBindParents.gaBaseConstraint` as the owner-local base-constraint projection. The only redundant broad shim in the selected surface was `PreparedGeneralizationArtifact.pgaAcyclicBaseConstraint`.

- `If that audit proves an explicitly transitional or broad shim in the selected surface is locally redundant, remove that one shim and update only the directly dependent code/tests/docs needed to keep the accepted type-level safety story coherent`: met. The round removes only `pgaAcyclicBaseConstraint`, rewires pipeline/result-type recovery through `gaBaseConstraint bindParentsGa`, and keeps the rest of the accepted owner-local surfaces intact.

- `Align durable guidance to the audited outcome by updating AGENTS.md, docs/architecture.md, and CHANGELOG.md, and update an ADR only if the audit proves a live architectural decision is currently undocumented`: met. `AGENTS.md`, `docs/architecture.md`, and `CHANGELOG.md` now describe the same accepted contract without reopening roadmap meaning or requiring an ADR update.

- `Tighten focused guard coverage ... so milestone-6 becomes mechanically reviewable: the accepted type-level safety markers stay synchronized across guidance, any retained seam stays explicitly owner-local and narrow, and any retired shim stays absent`: met. `RepoGuardSpec` guards the guidance/seam markers and retired-field absence, `PipelineSpec` guards the moved preparation owner boundary, and the behavioral prepared-artifact test proves the remaining path still works through `GaBindParents`.

- `Run the focused guard/build checks, git diff --check, and the full cabal build all && cabal test gate. Confirm the round leaves no tracked generated build dirt, including runtime/mlfp_io/target/release/libmlfp_io.d, before handing off to review`: met for the selected payload before the reviewer reran the full gate. The full gate passed locally, and the payload started with no tracked depfile dirt; the current depfile modification is reviewer-induced and must be restored or excluded before merge.

## Decision

**APPROVED**

Milestone `6` should move from `pending` to `done` through status-only closeout. Because `milestone-6` is the final unfinished milestone in `roadmap-view.json`, applying that closeout makes the active roadmap terminal; no semantic roadmap update is needed.

## Evidence

- `src/MLF/Elab/Run/Generalize/Prepare.hs` no longer duplicates the prepared base constraint on `PreparedGeneralizationArtifact`. The prepared artifact still carries `pgaBindParentsGa :: GaBindParents 'Presolved`, and the owner note now explicitly states that the phase bridge stays on `GaBindParents`.

- `src/MLF/Elab/Run/Pipeline.hs` now recovers the prepared base graph through `gaBaseConstraint bindParentsGa`. `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`, and the generalization plan code already consume `gaBaseConstraint`, so the round removes duplication instead of inventing a new authority.

- The behavioral selector `prepared generalization artifact drives redirecting instantiation behavior` proves the removal is behavior-preserving: the artifact still supplies copy provenance, canonical annotation recovery, generalization-at-root, canonical scope resolution, and Bool result-type reconstruction through the same base constraint now recovered from `GaBindParents`.

- The retained seams are narrow and explicitly owned rather than broad migration shims:
  - mixed `NodeRef` storage remains limited to binding-tree/mixed contexts, while typed call sites use `NodeRefTag`;
  - `MLF.Constraint.Types.Graph` exports only directional `toNormalizedConstraint`, `toAcyclicConstraint`, `toPresolvedConstraint`, and `toSolvedConstraint`, while the generic `rephaseConstraint` helper stays private;
  - `mkUncheckedInstanceWitness` remains in `MLF.Constraint.Types.Witness.Internal` and is consumed explicitly from `MLF.Constraint.Presolution.Witness` as the pre-normalization lane;
  - `GaBindParents.gaBaseConstraint` is the owner-local base-constraint projection for generalization and result-type recovery.

- `AGENTS.md`, `docs/architecture.md`, and `CHANGELOG.md` now describe the same accepted type-level safety contract: phase-indexed constraints with directional transitions, mixed `NodeRef` storage plus typed `NodeRefTag` boundaries, `mkUncheckedInstanceWitness` as a retained owner-local pre-normalization seam, and `gaBaseConstraint` as the preserved base-graph owner. None of the three files claims more than the evidence supports.

- The closeout remains `status-only`. The round does not change future coordination, milestone meaning, direction meaning, sequencing, parallel lanes, verification meaning, or retry policy. It closes the last pending milestone under the already-accepted roadmap contract, so the controller can finalize by marking `milestone-6` done, adding a completion pointer, and appending one compact history entry.

- `orchestrator/state.json` is controller-owned and already records the active round in `review`; it was inspected for metadata validation only and left untouched.
