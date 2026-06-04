# Artifact Manifest

This file owns orchestrator file layout, artifact keys, and path resolution.

## Shared Control-Plane Files

The scaffolded `orchestrator/` directory must contain:

- `state.json`
- `state-schema.md`
- `artifact-manifest.md`
- `project-contract.md`
- `active-roadmap-bundle.md`
- `role-contract.md`
- `roadmap-update-schema.md`
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
- `verification.md`

The roadmap family directory must contain:

- `roadmap-history.md`

## Round Artifact Keys

Round artifact paths are derived from `round_id`, not persisted in
`state.json`. Do not create paired JSON records for round artifacts. Each
Markdown artifact owns both its human-readable content and its structured
fields. This manifest owns live-vs-archived path resolution. Use these keys:

| Key | Path |
|-----|------|
| `roadmap_update_request` | `orchestrator/rounds/<round-id>/roadmap-update-request.md` |
| `plan` | `orchestrator/rounds/<round-id>/plan.md` |
| `implementation_notes` | `orchestrator/rounds/<round-id>/implementation-notes.md` |
| `review` | `orchestrator/rounds/<round-id>/review.md` |
| `merge` | `orchestrator/rounds/<round-id>/merge.md` |

`roadmap_update_request` is written only by a plan-stage planner when no
bounded round can be selected before a semantic roadmap split. It is evidence
for `update-roadmap`, not a mergeable round output. For `Complexity: simple`
rounds, planner-authored direct evidence lives in `implementation-notes.md`.
For non-simple rounds, reviewer approval, retry target, and closeout
classification live in `review.md`. `merge.md` is optional controller merge
bookkeeping; it must not duplicate plan or review content.

## Worker Artifact Paths

When `plan.md` authorizes worker fan-out, worker artifacts use:

- `orchestrator/rounds/<round-id>/workers/<worker-id>/assignment.md`
- `orchestrator/rounds/<round-id>/workers/<worker-id>/implementation-notes.md`
- `orchestrator/rounds/<round-id>/workers/<worker-id>/handoff.md`

Worker branch and worktree fields belong to the `Worker Fan-Out` section in
`plan.md`; this manifest owns artifact paths only.

## Roadmap Update Artifacts

Semantic roadmap-update branch, worktree, update artifact, and review artifact
paths are defined by `orchestrator/roadmap-update-schema.md`. Load that schema
instead of copying those conventions into callers.

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
