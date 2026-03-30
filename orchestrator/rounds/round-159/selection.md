roadmap_id: `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap`
roadmap_revision: `rev-001`
roadmap_dir: `orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001`
roadmap_item_id: `item-4`

# Round 159 Selection

## Selected item

`item-4` — Update repo guidance and handoff for ongoing CI maintenance.

## Rationale

Items 1-3 are done, so the remaining work is the repo-facing handoff that makes the new CI state durable and easy to maintain. This round should close the loop by documenting the bounded matrix, runner support boundaries, and which verification commands are authoritative locally versus in CI.

## Current baseline

Both gates are green on this checkout, with `1176+ examples, 0 failures`.

## What this round should deliver

- `README.md` updates for the new CI matrix and runner support notes
- `TODO.md` updates for ongoing CI maintenance priorities
- `CHANGELOG.md` updates describing the matrix rollout and maintenance handoff
- Explicit guidance on which commands are authoritative locally vs in CI
- Explicit documentation of any intentionally out-of-scope runner limitations
