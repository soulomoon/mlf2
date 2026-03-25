# Task Plan

Task: Migrate this repo-local orchestrator from the legacy top-level contract to revisioned roadmap bundles
Created: 2026-03-25
Status: complete

## Objective

- Move the live controller contract from top-level `orchestrator/roadmap.md`,
  `orchestrator/retry-subloop.md`, and `orchestrator/verification.md` to
  revisioned bundles under `orchestrator/roadmaps/<roadmap_id>/rev-###/`.
- Backfill archived rounds so their controller references resolve to stable
  roadmap bundles and per-round `state-snapshot.json` files instead of live
  top-level files or ephemeral `.worktrees/round-*` paths.
- Migrate the currently open `round-091` loop in the same change and keep it
  resumable on roadmap item `3`.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| 1. Load process skills and inspect the live repo/worktree contract surface | complete | Loaded `using-superpowers`, `executing-plans`, and `planning-with-files`; confirmed the repo and `.worktrees/round-091` still use the legacy top-level controller files and that historical rounds mix top-level and `.worktrees/round-*` references. |
| 2. Create the migration task packet and capture the execution scope | complete | Created this task folder and the paired findings/progress files for the migration. |
| 3. Implement the migration script and supporting contract rewrites | complete | Added `scripts/migrate_orchestrator_contract.py`, updated `AGENTS.md`, repo-local orchestrator role prompts, `.codex/agents` prompts, live pointer stubs, and roadmap-bundle-aware state handling. |
| 4. Run audit/write/audit migration and update the live repo plus `.worktrees/round-091` | complete | Materialized revisioned roadmap bundles, rewrote archived round packets in place, backfilled `state-snapshot.json` plus historical roadmap provenance, and migrated the live parent/worktree state to `roadmap_id` / `roadmap_revision` / `roadmap_dir`. |
| 5. Verify the migrated repo and summarize residual risks | complete | Re-ran audit after write, confirmed all archived rounds carry provenance plus `state-snapshot.json`, verified parent/worktree roadmap-dir continuity and `item-3` resumability, and passed both parent/worktree `git diff --check`. |

## Decisions

| Decision | Rationale |
| --- | --- |
| Use a script-driven migration instead of hand-editing round packets | The stale-reference surface spans most round packet types and historical epochs. |
| Rewrite old round packets in place rather than adding only sidecars | The user explicitly approved in-place repairs and wants old links fixed, not merely indexed externally. |
| Add `state-snapshot.json` to every round packet | Old `state.json` references drift for the same reason old roadmap links drift. |
| Keep top-level controller docs as pointer stubs only | This preserves discoverability while making `state.json.roadmap_dir` the only live authority. |
| Treat the active round packet `state-snapshot.json` as selection-time evidence, not live resumable state | Archived and active round packets should preserve the selection-time controller context; the still-open round continues to resume from `orchestrator/state.json` inside the active worktree. |
| Prefix every `roadmap_id` with `YYYY-MM-DD-NN-` where `NN` starts at `00` for the day | This gives each roadmap family a stable human-readable epoch prefix without replacing the descriptive slug. |

## Errors Encountered

| Error | Attempt | Resolution |
| --- | --- | --- |
| `python` is not available in this environment | 1 | Use `python3` for the migration script and script-assisted inspection. |
| `round-001` through `round-015` predate repo-local `review-record.json` | 1 | Fall back to the first `review.md` add commit for the review anchor and synthesize provenance-only `review-record.json` files during migration. |
| The first post-write audit treated newly synthesized early-round `review-record.json` files as if they had their own git-add commits | 1 | Teach the migrator to detect the missing git history and reuse the `review.md` anchor for those historical rounds. |
| The active `round-091` packet initially captured the current live plan-stage state in `state-snapshot.json` | 1 | Change the migrator to backfill the active round packet from the selection-time state while leaving live resumability in the active worktree `orchestrator/state.json`. |
