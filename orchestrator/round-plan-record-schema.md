# Round Plan Record Schema

`round-plan-record.json` is the planner-authored machine contract for the
current round plan and optional worker fan-out. `plan.md` remains the human
implementation plan, but runtime must not infer worker ownership or dependency
order from prose.

The lineage fields in `round-plan-record.json` must match
`selection-record.json`. They are integrity checks for the plan, not the
authority for selected round lineage.

Each planned round stores the record at:

```text
orchestrator/rounds/<round-id>/round-plan-record.json
```

While the round is live, resolve this path inside the round's recorded
`worktree_path`.

```json
{
  "schema_version": "round-plan-record-v1",
  "round_id": "round-001-example",
  "roadmap_id": "YYYY-MM-DD-00-example",
  "roadmap_revision": "rev-001",
  "roadmap_dir": "orchestrator/roadmaps/YYYY-MM-DD-00-example/rev-001",
  "milestone_id": "milestone-001-example",
  "direction_id": "direction-001-example",
  "extracted_item_id": "item-001-example",
  "plan_path": "orchestrator/rounds/round-001-example/plan.md",
  "worker_mode": "none",
  "workers": [],
  "integration": null
}
```

Required fields:

- `schema_version`: must be `round-plan-record-v1`
- `round_id`
- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- `milestone_id`
- `direction_id`
- `extracted_item_id`
- `plan_path`
- `worker_mode`: `none` or `fanout`
- `workers`
- `integration`

## Worker Fan-Out

Use `worker_mode: "fanout"` only when the plan has explicit, non-overlapping
worker ownership and a named integration pass.

Worker entries use this shape:

```json
{
  "worker_id": "worker-001",
  "summary": "Bounded worker assignment",
  "assignment_path": "orchestrator/rounds/round-001-example/workers/worker-001/assignment.md",
  "implementation_notes_path": "orchestrator/rounds/round-001-example/workers/worker-001/implementation-notes.md",
  "handoff_path": "orchestrator/rounds/round-001-example/workers/worker-001/handoff.md",
  "branch": "orchestrator/round-001-example-worker-001",
  "worktree_path": "orchestrator/worktrees/round-001-example-worker-001",
  "write_scope": ["src/example"],
  "depends_on_worker_ids": [],
  "blocks_integration": true,
  "verification_commands": ["cabal test example-test"]
}
```

`integration` uses this shape when worker fan-out is active:

```json
{
  "summary": "Integrate worker outputs in the canonical round worktree",
  "depends_on_worker_ids": ["worker-001"],
  "verification_commands": ["cabal test example-test"],
  "integration_notes_path": "orchestrator/rounds/round-001-example/implementation-notes.md"
}
```

When `worker_mode` is `none`, `workers` must be empty and `integration` must be
`null`.

## Worker Resume Observations

Worker progress is derived from `round-plan-record.json`, worker branch/worktree
state, and worker artifacts:

- `pending`: assignment exists but no worker branch/worktree has started
- `running`: worker branch/worktree exists and no handoff is complete
- `blocked`: the worker artifact records a blocker or the worktree cannot be
  observed
- `complete`: worker handoff exists and is ready for integration
- `integrated`: integration notes record that the worker output was consumed

The integration phase begins only after every `blocks_integration` worker is
`complete`. The controller derives integration state from this record, worker
artifacts, and the canonical round worktree; it does not persist worker mode in
`state.json`. After integration writes the round-level
`implementation-notes.md`, the round advances to review.
