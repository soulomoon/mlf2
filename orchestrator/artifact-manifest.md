# Artifact Manifest

This file is the canonical repo-local Interface for orchestrator file layout,
artifact keys, and path resolution. Other control-plane documents may point
here, but should not duplicate the complete file list or artifact map.

## Shared Control-Plane Files

The scaffolded `orchestrator/` directory must contain:

- `state.json`
- `state-schema.md`
- `artifact-manifest.md`
- `project-contract.md`
- `active-roadmap-bundle.md`
- `role-contract.md`
- `selection-record-schema.md`
- `round-plan-record-schema.md`
- `round-finalization-schema.md`
- `roadmap-update-schema.md`
- `roles/guider.md`
- `roles/planner.md`
- `roles/implementer.md`
- `roles/reviewer.md`
- `roles/recovery-investigator.md`
- `rounds/`
- `roadmap-updates/`
- `roadmaps/`
- `worktrees/`

## Active Roadmap Bundle Files

The active roadmap revision directory named by `state.json.roadmap_dir` must
contain:

- `roadmap.md`
- `roadmap-view.json`
- `verification.md`

The roadmap family directory must contain:

- `roadmap-history.md`

Roadmap-specific retry policy, when any exists, lives in `verification.md`
under `## Roadmap Overrides`. Shared retry mechanics live in runtime
references, not in the active bundle.

## Round Artifact Keys

Round artifact paths are derived from `round_id`, not persisted in
`state.json`. Use these keys:

| Key | Path |
|-----|------|
| `selection_record` | `orchestrator/rounds/<round-id>/selection-record.json` |
| `roadmap_update_request` | `orchestrator/rounds/<round-id>/roadmap-update-request.md` |
| `plan` | `orchestrator/rounds/<round-id>/plan.md` |
| `round_plan_record` | `orchestrator/rounds/<round-id>/round-plan-record.json` |
| `implementation_notes` | `orchestrator/rounds/<round-id>/implementation-notes.md` |
| `review` | `orchestrator/rounds/<round-id>/review.md` |
| `review_record` | `orchestrator/rounds/<round-id>/review-record.json` |
| `closeout_record` | `orchestrator/rounds/<round-id>/closeout-record.json` |

`roadmap_update_request` is written only by a plan-stage planner when no
bounded round can be selected before a semantic roadmap split. It is evidence
for `update-roadmap`, not a mergeable round output. `closeout_record` is
required only for status-only closeout rounds.

## Worker Artifact Paths

When `round-plan-record.json` authorizes worker fan-out, worker artifacts use:

- `orchestrator/rounds/<round-id>/workers/<worker-id>/assignment.md`
- `orchestrator/rounds/<round-id>/workers/<worker-id>/implementation-notes.md`
- `orchestrator/rounds/<round-id>/workers/<worker-id>/handoff.md`

Worker branch and worktree names are deterministic from the round and worker
ids:

- branch `orchestrator/<round-id>-<worker-id>`
- worktree `orchestrator/worktrees/<round-id>-<worker-id>`

## Roadmap Update Artifacts

Semantic roadmap updates use the paths defined by
`orchestrator/roadmap-update-schema.md`:

- branch `orchestrator/roadmap-update-<round-id>-<slug>`
- worktree `orchestrator/worktrees/roadmap-update-<round-id>`
- update artifact
  `orchestrator/roadmap-updates/<round-id>-roadmap-update.md`
- review artifact
  `orchestrator/roadmap-updates/<round-id>-roadmap-update-review.md`

## Path Resolution

- Treat manifest paths as repo-relative unless explicitly absolute.
- While a round is live, resolve round artifact paths inside the round record's
  `worktree_path`.
- After a successful round merge, resolve archived round artifacts from the
  parent checkout.
- For planner-requested roadmap updates, resolve
  `roadmap_update_request` in the canonical round worktree while the planning
  round is active, then in the roadmap-update worktree after the controller
  preserves it there.
- Resolve active roadmap bundle files from `state.json.roadmap_dir`.
- Resolve roadmap-update artifacts inside the recorded roadmap-update worktree
  while an update is active.
