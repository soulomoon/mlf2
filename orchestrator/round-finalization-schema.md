# Round Finalization Schema

This file is the machine contract for the two artifacts that finalize a round:

- reviewer-authored `review-record.json`
- controller-authored `closeout-record.json`

The artifacts remain separate because ownership differs. `review-record.json`
owns lineage and closeout selectors; `closeout-record.json` is a compact
controller proof that those approved selectors were applied and revalidated.

## `review-record.json`

Each round stores the reviewer record at:

```text
orchestrator/rounds/<round-id>/review-record.json
```

While the round is live, resolve this path inside the round's recorded
`worktree_path`.

```json
{
  "schema_version": "review-record-v3",
  "round_id": "round-001-example",
  "roadmap_id": "YYYY-MM-DD-00-example",
  "roadmap_revision": "rev-001",
  "roadmap_dir": "orchestrator/roadmaps/YYYY-MM-DD-00-example/rev-001",
  "milestone_id": "milestone-001-example",
  "direction_id": "direction-001-example",
  "extracted_item_id": "item-001-example",
  "decision": "approved",
  "evidence_summary": "Focused tests passed and diff matches the plan.",
  "roadmap_closeout": {
    "mode": "status-only",
    "status_changes": [
      {
        "milestone_id": "milestone-001-example",
        "expected_current_status": "in-progress",
        "target_status": "done",
        "roadmap_view_anchor": "milestone-001-example"
      }
    ],
    "completion_pointers": [
      {
        "anchor_id": "milestone-001-example-completion",
        "text": "Completed by round-001-example; evidence: focused tests passed."
      }
    ],
    "history_entries": [
      {
        "anchor_id": "roadmap-history-completed-rounds",
        "text": "round-001-example completed milestone-001-example."
      }
    ],
    "semantic_update_required_reason": null
  }
}
```

Required top-level fields:

- `schema_version`: must be `review-record-v3`
- `round_id`
- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- `milestone_id`
- `direction_id`
- `extracted_item_id`
- `decision`: `approved` or `rejected`
- `evidence_summary`: non-empty reviewer evidence summary
- `roadmap_closeout`: required when `decision` is `approved`

Rejected reviews may omit `roadmap_closeout`, because no merge may follow a
rejected review.

## Closeout Modes

`roadmap_closeout.mode` is one of:

- `status-only`
- `semantic-update-required`

For `status-only`, the reviewer must provide controller-applicable selectors
that resolve through the active `roadmap-view.json`. Free-form closeout
instructions are not allowed.

For `semantic-update-required`, the reviewer must leave `status_changes`,
`completion_pointers`, and `history_entries` empty and provide
`semantic_update_required_reason`.

Use `semantic-update-required` when the merged round changes future
coordination, milestone or direction meaning, sequencing, parallel lanes,
extraction scope, verification meaning, or retry policy.

## Status-Only Selectors

Status changes use milestone ids and roadmap-view anchors instead of raw
Markdown headings:

```json
{
  "milestone_id": "<milestone id>",
  "expected_current_status": "in-progress",
  "target_status": "done",
  "roadmap_view_anchor": "<anchor id from roadmap-view.json>"
}
```

Rules:

- `milestone_id` must exist in the active `roadmap-view.json`.
- `expected_current_status` and `target_status` must be one of `pending`,
  `in-progress`, or `done`.
- `roadmap_view_anchor` must resolve through `roadmap-view.json.anchors`.
- The controller may update `roadmap-view.json` and the corresponding human
  `roadmap.md` projection only for the selected milestone status.

Completion pointers and history entries use roadmap-view anchors:

```json
{
  "anchor_id": "<anchor id from roadmap-view.json>",
  "text": "<single compact entry>"
}
```

Rules:

- `anchor_id` must resolve through `roadmap-view.json.anchors`.
- `text` must summarize already-approved completed work only.
- `text` must not change future coordination.

## `closeout-record.json`

Each status-only round stores the controller record at:

```text
orchestrator/rounds/<round-id>/closeout-record.json
```

While the round is live, resolve this path inside the round's recorded
`worktree_path`.

```json
{
  "schema_version": "closeout-record-v3",
  "round_id": "round-001-example",
  "review_record_path": "orchestrator/rounds/round-001-example/review-record.json",
  "review_record_sha256": "<sha256>",
  "base_revision": "<base commit observed before closeout>",
  "status": "applied",
  "applied_edit_summary": [
    "milestone-001-example status in-progress -> done",
    "milestone-001-example completion pointer recorded",
    "round-001-example history entry recorded"
  ],
  "verification": {
    "roadmap_view_revalidated": true,
    "active_bundle_revalidated": true,
    "diff_matches_review_record": true,
    "revalidated_after_base_refresh": false
  }
}
```

Required top-level fields:

- `schema_version`: must be `closeout-record-v3`
- `round_id`
- `review_record_path`
- `review_record_sha256`
- `base_revision`
- `status`: `applied`, `revalidated`, or `stale`
- `applied_edit_summary`
- `verification`

Lineage fields are not repeated here. The controller reads them from the
referenced `review-record.json`, verifies the hash, and confirms the referenced
review record has the same `round_id`.

The controller must not merge a status-only round unless `status` is `applied`
or `revalidated` and every verification field is true except
`revalidated_after_base_refresh`, which may be false when no refresh occurred.

Every `applied_edit_summary[]` entry must correspond to approved selectors in
`review-record.json`. Extra edits are not allowed.

If the base branch or active roadmap bundle changes while a status-only round
waits in `finalize-round`, the controller must revalidate the closeout against
the latest base before merge and update `closeout-record.json`.

If revalidation fails, set `status` to `stale`, keep the round out of merge,
and return to `review` or recovery according to the failure.
