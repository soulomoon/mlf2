# Verification Contract

Roadmap family: `2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap`
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
   - Confirm the scaffold change stopped after the checkpoint commit and did
     not start runtime rounds.
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Planning-only scope discipline for `rev-001`**
   - Confirm the diff stays inside docs/control-plane surfaces authorized by
     the selected direction.
   - Reject the round if it touches `src/`, `src-public/`, `app/`, `test/`, or
     `mlf2.cabal`.
   - Confirm the round does not introduce new implementation behavior,
     representative tests, or concrete boundary-enactment changes.
4. **Repo verification commands reserved for later code-bearing revisions**
   - Record the repo's authoritative code-bearing gates as
     `cabal build all && cabal test` and
     `./scripts/thesis-conformance-gate.sh`.
   - For this planning-only revision, confirm those commands are not required
     because the diff stays outside authorized code/test/thesis-conformance
     paths; if such paths are touched, reject the round instead of approving
     it.
5. **Worker-plan integrity when fan-out is used**
   - If a round uses planner-authored worker fan-out, confirm
     `worker-plan.json` exists, worker ownership boundaries were respected,
     and approval is based on the integrated round result.
6. **Planning-family boundary discipline**
   - Confirm the round preserves the accepted `round-200` / `round-201`
     handoff, keeps the retained-child clear-boundary settlement as
     predecessor truth only, keeps `P2` unopened, and keeps the representative
     negative-family rows closed unless an accepted active-family artifact
     explicitly changes that classification.

## Task-Specific Checks

Reviewers must also run checks specific to the selected milestone and
direction.

- **milestone-1**
  - Verify the artifact cites the accepted `round-200` / `round-201` lineage
    honestly.
  - Verify the artifact freezes the exact inherited boundary clauses under
    pressure rather than treating "boundary revision" as an undefined label.
  - Verify the artifact names the exact live broader positive `P5` subject,
    the exact lawful family outcomes, and the exact no-go claims for
    `rev-001`.
  - Verify the round stays docs-only and does not authorize implementation,
    new tests, or concrete revision enactment.
- **milestone-2**
  - Verify the artifact distinguishes the one settled retained-child lane from
    the broader positive `P5` frontier explicitly.
  - Verify preserved `P2`, absorbed-`mu`, and representative negative-family
    evidence remain predecessor truth rather than reopened live debt unless the
    accepted planning artifact explicitly says otherwise.
  - Verify any current-boundary-versus-explicit-revision comparison is grounded
    in the milestone-1 decision surface and remains planning-only.
- **milestone-3**
  - Verify the final handoff names exactly one downstream consequence.
  - Verify the artifact explains why every non-selected route remains closed.
  - Verify the round does not enact the downstream consequence inside
    `rev-001`.

## Approval Criteria

Approval requires all of the following:

1. Every applicable baseline check passes.
2. Every task-specific check for the selected milestone passes.
3. Reviewer evidence in `review.md` matches the observed diff and command
   output.
4. No unresolved blocking issue remains for the selected milestone.
5. Any preserved-history or pointer-refresh requirement has been satisfied.
6. Any `pending-merge` refresh or re-review requirement has been satisfied.
7. The round result stays within the planning-only scope of `rev-001` and does
   not silently widen semantics, interfaces, tests, or architecture.

## Reviewer Record Format

Each review must include:

- **Commands run**: exact shell commands with exit codes
- **Pass/fail result**: per-check result with output summary
- **Evidence summary**: key observations from the diff and verification output
- **Decision**: explicit **APPROVED** or **REJECTED: <reason>**

When the round finalizes, also write `review-record.json`:

```json
{
  "roadmap_id": "2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap",
  "roadmap_revision": "rev-001",
  "roadmap_dir": "orchestrator/roadmaps/2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap/rev-001",
  "milestone_id": "<milestone-id>",
  "direction_id": "<direction-id>",
  "extracted_item_id": "<extracted-item-id>",
  "decision": "approved",
  "evidence_summary": "<brief>"
}
```
