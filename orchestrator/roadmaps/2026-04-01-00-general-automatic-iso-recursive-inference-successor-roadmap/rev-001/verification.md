# Verification Contract

Roadmap family: `2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap`
Revision: `rev-001`

## Baseline Checks

Every round must satisfy all baseline checks that match its touched scope.

1. **Roadmap identity and pointer consistency**
   - Confirm `orchestrator/state.json` resolves the active roadmap bundle.
   - Confirm `orchestrator/roadmap.md`, `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md` point at the same `roadmap_id`,
     `roadmap_revision`, and `roadmap_dir`.
   - Confirm `selection.md` and `review-record.json` record matching roadmap
     identity fields when they are present.
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Roadmap metadata integrity**
   - Confirm the active `roadmap.md` still includes `Item id:`, `Depends on:`,
     `Parallel safe:`, `Parallel group:`, and `Merge after:` for every item.
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

## Task-Specific Checks

Reviewers must also run checks specific to the selected roadmap item.

- **item-1**
  - Verify the predecessor authority chain is cited explicitly and does not
    relitigate settled predecessor pockets as live debt.
  - Verify the exact inherited blocker lane is frozen concretely, including the
    current live blocker read for that lane.
  - Verify the writable slice is explicit and fail-closed.
- **item-2**
  - Verify the diff stays within the item-1 writable slice.
  - Verify focused regression coverage for the frozen lane is updated or
    replayed honestly.
  - Verify the round records one bounded result only: narrow success,
    fail-closed, or narrower blocker.
  - Verify `cabal build all && cabal test` passed if code/test files changed.
- **item-3**
  - Verify the settlement surface cites exact focused/full-gate provenance and
    does not upgrade one bounded packet into general repo readiness.
  - Verify the repo-impact read stays packet-bounded and architecture-honest.
- **item-4**
  - Verify the decision records exactly one explicit outcome token and exactly
    one handoff token.
  - Verify the artifact does not smuggle in a broader readiness claim or a
    boundary revision without explicit evidence.

## Approval Criteria

Approval requires all of the following:

1. Every applicable baseline check passes.
2. Every task-specific check for the selected item passes.
3. Reviewer evidence in `review.md` matches the observed diff and command
   output.
4. No unresolved blocking issue remains for the selected item.
5. Any `pending-merge` refresh or re-review requirement has been satisfied.
6. The round result stays bounded to one packet or one aggregate decision at
   the scope authorized by the active roadmap item.

## Reviewer Record Format

Each review must include:

- **Commands run**: exact shell commands with exit codes
- **Pass/fail result**: per-check result with output summary
- **Evidence summary**: key observations from the diff and verification output
- **Decision**: explicit **APPROVED** or **REJECTED: <reason>**

When the round finalizes, also write `review-record.json`:

```json
{
  "roadmap_id": "2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap",
  "roadmap_revision": "rev-001",
  "roadmap_dir": "orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001",
  "roadmap_item_id": "<item-N>",
  "decision": "approved",
  "evidence_summary": "<brief>"
}
```
