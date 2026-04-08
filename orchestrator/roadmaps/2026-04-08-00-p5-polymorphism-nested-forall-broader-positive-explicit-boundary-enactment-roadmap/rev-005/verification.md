# Verification Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-005`

## Baseline Checks

Every round must satisfy all baseline checks that match its touched scope.

1. **Roadmap lineage, pointer, and preserved-history consistency**
   - Confirm `orchestrator/state.json` resolves the active roadmap bundle.
   - Confirm `selection.md` records matching
     `roadmap_id`, `roadmap_revision`, `roadmap_dir`,
     `milestone_id`, `direction_id`, and `extracted_item_id`.
   - Confirm final `review-record.json` records the same lineage fields.
   - Confirm `roadmap_item_id` is absent unless a compatibility mirror is
     explicitly required for a legacy reader.
   - Confirm `orchestrator/roadmap.md`, `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md` match the active
     `roadmap_id`, `roadmap_revision`, and `roadmap_dir`.
   - Confirm prior roadmap families and revisions remain unchanged.
   - Confirm `round-208`, `round-209`, and `round-210` artifacts remain
     immutable predecessor evidence if the round cites them.
   - For `rev-005` milestone-2 continuation, confirm the same `round-211`
     branch/worktree remains the live baseline when cited and that its
     current diff is preserved rather than discarded on a fresh round.
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Build and test gate for production/test changes**
   - If the round touches `src/`, `src-public/`, `app/`, `test/`, or
     `mlf2.cabal`, run `cabal build all && cabal test`.
4. **Thesis conformance gate for thesis-facing changes**
   - If the round touches `docs/thesis-*`, `docs/thesis-deviations.yaml`,
     `scripts/thesis-*`, `papers/these-finale-english.txt`, or any other
     thesis-conformance source, run `./scripts/thesis-conformance-gate.sh`.
5. **Broader-positive boundary discipline**
   - Confirm the round stays inside the selected milestone/direction scope and
     does not silently widen into cyclic search, multi-SCC behavior,
     equi-recursive reasoning, fallback rescue, or a second interface.
   - Confirm the retained-child clear-boundary lane remains predecessor truth
     rather than being silently upgraded into whole-frontier closure.
   - Confirm `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
     `N6 termination-pressure` remain closed unless the active milestone
     explicitly and honestly reclassifies them.
   - For milestone-2 rounds under `rev-005`, confirm the continuation stays
     limited to the preserved `round-211` baseline plus the one newly
     admitted application-builder seam:
     `src/MLF/Elab/Elaborate/Annotation.hs`,
     `src/MLF/Elab/Legacy.hs`,
     `src/MLF/Elab/Elaborate/Algebra.hs`,
     `test/ElaborationSpec.hs`,
     `test/PipelineSpec.hs`, and
     `test/Research/P5ClearBoundarySpec.hs`.
6. **Authoritative-entrypoint discipline**
   - When a round claims broader-positive support, confirm the evidence is
     visible on both `runPipelineElab` and `runPipelineElabChecked` rather
     than only on internal helpers or one entrypoint.
7. **Worker-plan integrity when fan-out is used**
   - If a round uses planner-authored worker fan-out, confirm
     `worker-plan.json` exists, worker ownership boundaries were respected,
     and approval is based on the integrated round result.

## Task-Specific Checks

Reviewers must also run checks specific to the selected milestone and
direction.

- **milestone-1**
  - Verify the artifact consumes accepted `round-203`, `round-204`, and
    `round-205` honestly and does not rewrite those predecessor artifacts.
  - Verify the artifact names the exact broader positive frontier beyond the
    one settled retained-child lane, the exact expected behavior shift away
    from controlling polymorphic-mediation `mu` absorption, the exact
    authoritative success surfaces, and the exact representative corpus.
  - Verify the artifact freezes the writable slice concretely enough for later
    code-bearing rounds and keeps excluded families and guardrails closed.
  - Verify the round stays docs/control-plane-only.
- **milestone-2**
  - Verify the diff stays inside the milestone-1 writable slice as superseded
    by `rev-005`:
    `src/MLF/Elab/Elaborate/Annotation.hs`,
    `src/MLF/Elab/Legacy.hs`,
    `src/MLF/Elab/Elaborate/Algebra.hs`,
    `test/ElaborationSpec.hs`,
    `test/PipelineSpec.hs`, and
    `test/Research/P5ClearBoundarySpec.hs`.
  - Verify the continuing round preserves the already-cleared Phase 6
    authoritative translation and the downstream
    `PhiReorder: missing binder identity` progress from the current
    `round-211` baseline, and repairs the remaining selected-packet
    Phase 7 `TCArgumentMismatch` by changing only the `AAppF`
    application-builder path and the exact helper logic around authoritative
    argument instantiation / `mu` coercion.
  - If the round carries changes in
    `src/MLF/Elab/Elaborate/Annotation.hs` or
    `src/MLF/Elab/Legacy.hs`, verify they are preserved round-211 baseline
    work or only mechanical companions to the `Algebra.hs` repair, not a
    reopened annotation-translation redesign.
  - If the round touches
    `src/MLF/Elab/Elaborate/Algebra.hs`, verify the repair stays inside
    `AAppF` and its exact helper logic; it must not broaden other algebra
    cases or add fallback/compatibility helpers.
  - Verify the authoritative packet is covered honestly on both entrypoints
    in `test/PipelineSpec.hs` and `test/Research/P5ClearBoundarySpec.hs`,
    and that `test/ElaborationSpec.hs` still proves the old Phase 6 seam and
    the post-annotation handoff stay cleared.
  - Verify
    `src/MLF/Elab/Run/Pipeline.hs`,
    `src/MLF/Elab/TermClosure.hs`,
    `src/MLF/Elab/Pipeline.hs`,
    `src-public/MLF/Pipeline.hs`,
    `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
    `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
    remain untouched in this tightened continuation.
  - Verify `cabal build all && cabal test` passed.
  - Verify no fallback rescue, second interface, cyclic widening, or
    negative-family reclassification is smuggled in.
- **milestone-3**
  - Verify the representative broader positive frontier named by milestone-1
    now passes honestly on both `runPipelineElab` and `runPipelineElabChecked`.
  - Verify the relevant research/pipeline regression surfaces were updated or
    replayed honestly, especially `test/Research/P5ClearBoundarySpec.hs`,
    `test/PipelineSpec.hs`, and any milestone-1-authorized companion specs.
  - Verify public/internal parity is real and does not depend on helper-only
    success or compatibility shims.
  - Verify preserved `P2` and representative negative-family rows remain
    closed.
- **milestone-4**
  - Verify the closeout artifact records the enacted behavior and evidence
    surface honestly.
  - Verify the closeout states that polymorphic-mediation `mu` absorption is
    no longer the controlling broader-positive read only to the extent that
    milestone-3 actually earned that claim.
  - Verify repo-facing notes, `implementation_notes.md`, `TODO.md`,
    `CHANGELOG.md`, and any thesis-deviation records were updated when the
    accepted evidence requires them.
  - Verify `cabal build all && cabal test` passed for any code/test-bearing
    round and `./scripts/thesis-conformance-gate.sh` passed when thesis-facing
    files changed.

## Approval Criteria

Approval requires all of the following:

1. Every applicable baseline check passes.
2. Every task-specific check for the selected milestone passes.
3. Reviewer evidence in `review.md` matches the observed diff and command
   output.
4. No unresolved blocking issue remains for the selected milestone.
5. Any preserved-history or pointer-refresh requirement has been satisfied.
6. Any `pending-merge` refresh or re-review requirement has been satisfied.
7. The round result stays within the enacted broader-positive boundary the
   active milestone authorizes and does not silently widen semantics,
   interfaces, or architecture.

## Reviewer Record Format

Each review must include:

- **Commands run**: exact shell commands with exit codes
- **Pass/fail result**: per-check result with output summary
- **Evidence summary**: key observations from the diff and verification output
- **Decision**: explicit **APPROVED** or **REJECTED: <reason>**

When the round finalizes, also write `review-record.json`:

```json
{
  "roadmap_id": "2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap",
  "roadmap_revision": "rev-005",
  "roadmap_dir": "orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-005",
  "milestone_id": "<milestone-id>",
  "direction_id": "<direction-id>",
  "extracted_item_id": "<extracted-item-id>",
  "decision": "approved",
  "evidence_summary": "<brief>"
}
```
