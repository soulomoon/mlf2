# Roadmap Update Schema

This file is the single machine contract for semantic roadmap updates. It owns
the `state.json.roadmap_update` shape, update branch/worktree conventions,
review artifact contract, rejection loop, and activation metadata.

Semantic roadmap updates are entered through one of two triggers:

- `merged-round`: a merged round changes future coordination, milestone or
  direction meaning, sequencing, parallel lanes, extraction scope, verification
  meaning, or retry policy.
- `planner-request`: a plan-stage `roadmap-update-request.md` shows that no
  bounded dependency-ready round can be selected without first splitting or
  resequencing the active roadmap.

Status-only round closeout stays in the round worktree under
`round-finalization-schema.md`.

## `state.json.roadmap_update`

When no semantic roadmap update is active, `roadmap_update` is `null`.

When active, it has this shape:

```json
{
  "schema_version": "roadmap-update-v1",
  "trigger": "merged-round",
  "source_round_id": "round-001-example",
  "branch": "orchestrator/roadmap-update-round-001-example-adjust-roadmap",
  "worktree_path": "orchestrator/worktrees/roadmap-update-round-001-example",
  "update_artifact": "orchestrator/roadmap-updates/round-001-example-roadmap-update.md",
  "review_artifact": "orchestrator/roadmap-updates/round-001-example-roadmap-update-review.md",
  "prior_roadmap_revision": "rev-001",
  "proposed_roadmap_revision": "rev-002",
  "status": "authoring",
  "attempt": 1,
  "last_rejection_artifact": null,
  "last_rejection_summary": null,
  "resume_error": null
}
```

Required fields:

- `schema_version`: must be `roadmap-update-v1`
- `trigger`: `merged-round` or `planner-request`
- `source_round_id`
- `branch`
- `worktree_path`
- `update_artifact`
- `review_artifact`
- `prior_roadmap_revision`
- `proposed_roadmap_revision`
- `status`: `authoring`, `review`, `rejected`, `approved`, or `blocked`
- `attempt`
- `last_rejection_artifact`
- `last_rejection_summary`
- `resume_error`

Only one semantic roadmap update may be active. Additional merged rounds that
require semantic roadmap updates remain in `finalize-round`. A
planner-requested update must cancel the unselected planning round before
entering `update-roadmap`; it uses the planning round id only as
`source_round_id`.

## Branch And Artifact Conventions

Use:

- branch `orchestrator/roadmap-update-<round-id>-<slug>`
- worktree `orchestrator/worktrees/roadmap-update-<round-id>`
- update artifact
  `orchestrator/roadmap-updates/<round-id>-roadmap-update.md`
- review artifact
  `orchestrator/roadmap-updates/<round-id>-roadmap-update-review.md`

For `planner-request`, the controller must preserve the planner-authored
`roadmap-update-request.md` verbatim in the roadmap-update worktree at
`orchestrator/rounds/<round-id>/roadmap-update-request.md` before removing the
planning round from `active_rounds[]`. The guider reads that preserved request
as evidence; it does not become an approved roadmap diff.

## `roadmap-update.md`

The guider authors this artifact and the proposed roadmap bundle revision in
the roadmap-update worktree.

Required sections:

### Source Round
- Round id:
- Trigger: merged-round | planner-request
- Merged commit: <commit hash, or `none` for planner-request>
- Evidence:

### Roadmap Change
- Roadmap id:
- Prior revision:
- Proposed revision:
- Files changed:

### Rationale

### State Activation
- Requires state.json roadmap metadata update:
- New roadmap_dir when applicable:

## `roadmap-update-review.md`

The reviewer authors this artifact.

Required sections:

### Checks Run

### Roadmap Compliance

### Decision

The decision must be exactly `APPROVED` or `REJECTED: <specific reason and
required changes>`.

## Rejection Loop

If review rejects the update:

- keep the same roadmap-update branch and worktree;
- set `roadmap_update.status` to `rejected`;
- increment `roadmap_update.attempt`;
- record `last_rejection_artifact` and `last_rejection_summary`; and
- return the update to the guider for in-place revision.

After the guider revises the update, set status back to `review` and dispatch
the reviewer again.

After three rejected attempts for the same semantic update, set status to
`blocked`, record the precise blocker in `roadmap_update.resume_error`, and do
not dispatch another same-mechanism retry until recovery chooses a different
lawful action.

## Activation

The controller may activate `proposed_roadmap_revision` only after
`roadmap-update-review.md` approves the update and the roadmap-update branch
merges.

Activation updates `state.json.roadmap_revision` and `state.json.roadmap_dir`
for the approved revision. `roadmap_id` remains the same family id unless the
approved review explicitly records a family migration.

After activation, clear `state.json.roadmap_update`.
