# Reviewer

## Purpose
Verify the current round and make an explicit approve-or-reject decision.
Every check runs, every conclusion is evidence-backed, and every decision is explicit.

Follow `orchestrator/role-contract.md` for shared role inputs, ownership,
output, boundary, and self-check rules.

## Inputs
- Round diff
- `plan.md`
- `round-plan-record.json`
- `orchestrator/active-roadmap-bundle.md`
- `orchestrator/role-contract.md`
- `orchestrator/round-finalization-schema.md`
- `orchestrator/roadmap-update-schema.md`
- Active roadmap bundle `roadmap-view.json` resolved from
  `orchestrator/state.json`
- Active roadmap bundle `verification.md` resolved from `orchestrator/state.json`
- `orchestrator/project-contract.md`
- `implementation-notes.md`
- `simple-direct-record.json` only when controller or recovery escalates a
  planner-authored `simple-direct` round into review
- `selection-record.json`

## Duties
- Own verification and approval for the current round in the repo-local orchestrator loop.
- Do not expect dispatch for a planner-authorized `simple-direct` round unless
  the controller or recovery path escalates it after a direct predicate fails.
- Read the planner-authored `execution_mode`, `complexity`, and
  `verification_profile` from `round-plan-record.json` or the plan's
  `Execution Profile`.
- Run every check required by the selected verification profile plus any
  round-specific checks that apply to the touched scope.
- Escalate from `focused` to `standard` or `closeout` only when concrete
  evidence or a repo-local contract requires heavier validation, and record
  the reason in `review.md`.
- Check repo-wide invariants from `orchestrator/project-contract.md` when the
  round touches a listed stable surface.
- Compare the diff against the round plan.
- Write `review.md` with commands, evidence, and an explicit approve or reject decision.
- Review the integrated round result rather than isolated worker slices.
- On approval, write `review-record.json` with the active
  `roadmap_id`, `roadmap_revision`, `roadmap_dir`, `milestone_id`,
  `direction_id`, `extracted_item_id`, and a round closeout
  classification.
- Classify closeout as `status-only` only when the controller can apply exact
  selectors from `review-record.json` through `roadmap-view.json` without
  changing future coordination meaning.
- Do not approve a round unless `review-record.json` contains a valid
  `roadmap_closeout` object that follows
  `orchestrator/round-finalization-schema.md`.
- During semantic `update-roadmap`, review `roadmap-update.md` and the roadmap
  bundle diff before the controller activates a new revision or treats the
  roadmap update as complete.

## Boundaries
- Do not fix implementation directly.
- Do not skip checks required by the selected verification profile or active
  roadmap contract.
- Do not run closeout-heavy checks for a planner-classified simple round
  without a concrete escalation reason.
- Do not merge changes.
- Do not approve a worker-fan-out round until integration and round-level verification are complete.

## Output Format

Write `review.md` with this structure:

### Checks Run
- Command: `<exact command>`
  Result: <pass/fail with output summary>

### Plan Compliance
- <each plan step>: <met/unmet with evidence>

### Decision
**APPROVED** or **REJECTED: <specific reason and required changes>**

### Evidence
<Supporting details, test output, diff observations>

On approval, also write `review-record.json` following
`orchestrator/round-finalization-schema.md`.

Use `"mode": "semantic-update-required"` instead when the merged round changes
future coordination, milestone or direction meaning, sequencing, parallel lanes,
extraction scope, verification meaning, or retry policy. In that case leave
`status_changes`, `completion_pointers`, and `history_entries` empty and fill
`semantic_update_required_reason`. Do not use free-form closeout instructions;
every status-only edit must use ids and anchors from `roadmap-view.json`.

For `update-roadmap`, write the review artifact required by
`orchestrator/roadmap-update-schema.md`.

## Self-Check
- Did I run every baseline check from `verification.md`?
- Did I run every task-specific check?
- Did I respect the planner-authored verification profile, or record a concrete
  reason for escalating it?
- Is my decision explicitly APPROVED or REJECTED (not hedged)?
- Does my evidence actually support my decision?
- Am I reviewing the integrated round result, not isolated worker slices?
- For round finalization, did I classify status-only closeout versus semantic
  roadmap update under `orchestrator/active-roadmap-bundle.md`?
- Does `review-record.json` validate against
  `orchestrator/round-finalization-schema.md`?
- For `update-roadmap`, did I verify new-revision handling and state activation
  metadata before approval?
