# Selection Record Schema

`selection-record.json` is the planner-authored machine contract for selected
round lineage and merge ordering. Runtime must not infer lineage or scheduling
from prose.

Each round stores the record at:

```text
orchestrator/rounds/<round-id>/selection-record.json
```

While the round is live, resolve this path inside the round's recorded
`worktree_path`.

```json
{
  "schema_version": "selection-record-v1",
  "round_id": "round-001-example",
  "roadmap_id": "YYYY-MM-DD-00-example",
  "roadmap_revision": "rev-001",
  "roadmap_dir": "orchestrator/roadmaps/YYYY-MM-DD-00-example/rev-001",
  "milestone_id": "milestone-001-example",
  "direction_id": "direction-001-example",
  "extracted_item_id": "item-001-example",
  "summary": "Bounded round scope",
  "scheduler": {
    "depends_on_round_ids": [],
    "merge_after_item_ids": [],
    "parallel_group": null
  }
}
```

Required fields:

- `schema_version`: must be `selection-record-v1`
- `round_id`
- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- `milestone_id`
- `direction_id`
- `extracted_item_id`
- `summary`
- `scheduler`

`extracted_item_id` may be minted by the planner when the active roadmap
direction does not already name a smaller extraction id. It
must remain stable for the life of the round.

Use serial scheduler defaults unless the active roadmap metadata and selected
scope justify explicit dependency or co-scheduling fields.

Scheduler fields:

- `depends_on_round_ids`: round ids that must merge before this round may merge
- `merge_after_item_ids`: extracted item ids that must merge before this round
  may merge
- `parallel_group`: co-scheduling group, or `null`

Merge readiness is not persisted. The controller derives merge admissibility
from reviewer approval, closeout validity, scheduler fields, dependency state,
base freshness, and active semantic roadmap-update state.
