# Verification Contract

Roadmap family: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`
Revision: `rev-001`

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
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Strategy-roadmap metadata integrity**
   - Confirm the active `roadmap.md` still includes
     `## Goal`,
     `## Outcome Boundaries`,
     `## Global Sequencing Rules`,
     `## Parallel Lanes`, and
     `## Milestones`.
   - Confirm every milestone still includes
     `Milestone id:`,
     `Depends on:`,
     `Intent:`,
     `Completion signal:`,
     `Parallel lane:`, and
     `Coordination notes:`.
   - Confirm every candidate direction still includes
     `Direction id:`,
     `Summary:`,
     `Why it matters now:`,
     `Preconditions:`,
     `Parallel hints:`,
     `Boundary notes:`, and
     `Extraction notes:`.
4. **Build and test gate for production/test changes**
   - If the round touches `src/`, `src-public/`, `app/`, `test/`, or
     `mlf2.cabal`, run `cabal build all && cabal test`.
5. **Thesis conformance gate for thesis-facing changes**
   - If the round touches `docs/thesis-*`, `docs/thesis-deviations.yaml`,
     `scripts/thesis-*`, or any other thesis-conformance source, run
     `./scripts/thesis-conformance-gate.sh`.
6. **Worker-plan integrity when fan-out is used**
   - If the round uses planner-authored worker fan-out, confirm
     `worker-plan.json` exists, worker ownership boundaries were respected,
     and approval is based on the integrated round result.
7. **Preserved setup / control-plane discipline**
   - Confirm the next-family scaffold preserved prior families and revisions
     unchanged and stopped after the checkpoint commit without starting
     runtime rounds inside the scaffold change itself.

## Task-Specific Checks

Reviewers must also run checks specific to the selected milestone and
direction.

- **milestone-1**
  - Verify the artifact cites the accepted round-193 decision,
    the March 28 exact `P5` family artifacts, and the accepted round-151
    reclassification honestly.
  - Verify the artifact keeps the exact March 28
    `nestedForallContrastExpr` packet closed as predecessor truth only.
  - Verify the artifact freezes one exact post-item-7 `P5` follow-on lane,
    one authoritative-surface success bar, and one writable slice only.
  - Verify any current-architecture versus boundary-pressure gate remains
    docs-only and does not pre-authorize implementation or revision.
- **milestone-2**
  - Verify the diff stays inside the milestone-1-selected lane and writable
    slice.
  - Verify the authoritative-surface evidence is honest about whether the
    selected `P5` lane yields lawful positive support, bounded fail-closed
    behavior, or explicit boundary pressure.
  - Verify `cabal build all && cabal test` passed if the round touched
    production, public-surface, test, or Cabal files.
  - Verify the round does not reopen the settled March 28 exact `P5` packet
    as live debt.
- **milestone-3**
  - Verify the artifact distinguishes refreshed `P5` evidence from the
    accepted `P2 packet-specific folklore` ledger explicitly.
  - Verify any `P2` follow-on freeze names one exact non-local lane rather
    than promoting the existing `C1` packet into family closure.
  - Verify any routing artifact that keeps `P5` dominant does not smuggle in
    a broader readiness or boundary decision early.
- **milestone-4**
  - Verify the decision records exactly one explicit end-state outcome.
  - Verify any readiness claim, continued bounded handoff, or
    boundary-revision candidate is scoped exactly to the accumulated refreshed
    `P5` / `P2` ledger plus the preserved negative-family settlements.
  - Verify the final handoff or enablement step does not pre-authorize scope
    beyond the accepted evidence.

## Approval Criteria

Approval requires all of the following:

1. Every applicable baseline check passes.
2. Every task-specific check for the selected milestone passes.
3. Reviewer evidence in `review.md` matches the observed diff and command
   output.
4. No unresolved blocking issue remains for the selected milestone.
5. Any preserved-history or pointer-refresh requirement has been satisfied.
6. Any `pending-merge` refresh or re-review requirement has been satisfied.
7. The round result stays within the scope authorized by the active milestone
   and does not silently widen semantics, interfaces, or architecture.

## Reviewer Record Format

Each review must include:

- **Commands run**: exact shell commands with exit codes
- **Pass/fail result**: per-check result with output summary
- **Evidence summary**: key observations from the diff and verification output
- **Decision**: explicit **APPROVED** or **REJECTED: <reason>**

When the round finalizes, also write `review-record.json`:

```json
{
  "roadmap_id": "2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap",
  "roadmap_revision": "rev-001",
  "roadmap_dir": "orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001",
  "milestone_id": "<milestone-id>",
  "direction_id": "<direction-id>",
  "extracted_item_id": "<extracted-item-id>",
  "decision": "approved",
  "evidence_summary": "<brief>"
}
```
