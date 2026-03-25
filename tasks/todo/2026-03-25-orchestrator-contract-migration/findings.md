# Findings

- The parent repo and the open `.worktrees/round-091` worktree both still use
  the legacy top-level controller files:
  `orchestrator/roadmap.md`, `orchestrator/retry-subloop.md`, and
  `orchestrator/verification.md`.
- `orchestrator/state.json` currently lacks `roadmap_id`,
  `roadmap_revision`, and `roadmap_dir`.
- Historical round packets use two stale controller-reference styles:
  moving top-level `orchestrator/*.md` paths and absolute
  `.worktrees/round-*` controller paths.
- `selection.md` exists for all 90 archived rounds, and `review-record.json`
  exists for all 90 archived rounds, so the round-level rewrite surface is
  complete and scriptable.
- Historical `review-record.json` files do not currently carry roadmap
  provenance, so the migration must backfill it from git history.
- `git log --diff-filter=A -- orchestrator/rounds/<round>/review-record.json`
  provides a deterministic commit anchor for each archived round.
- The current repo-local agent prompt surface under `.codex/agents/` still
  names top-level controller docs as reviewer inputs and will need contract
  updates alongside `orchestrator/roles/*.md` and `AGENTS.md`.
- `round-001` through `round-015` predate repo-local `review-record.json`, so
  the migration needs a historical fallback anchor; `review.md` first-add
  commits are sufficient for those rounds.
- Several early roadmap revisions also predate dedicated
  `orchestrator/retry-subloop.md` and `orchestrator/verification.md` files, so
  the migration must synthesize clearly labeled historical placeholder docs in
  the revisioned roadmap bundle.
- The active `round-091` packet needs two different state notions:
  `orchestrator/rounds/round-091/state-snapshot.json` should preserve the
  selection-time controller context, while `orchestrator/state.json`
  remains the live resumability surface at `current_task: "item-3"`.
- After migration, parent authoritative surfaces no longer treat top-level
  `orchestrator/roadmap.md`, `orchestrator/retry-subloop.md`, or
  `orchestrator/verification.md` as live sources; only `AGENTS.md` and
  `implementation_notes.md` mention them, and only as pointer-stub
  documentation.
- A plain slug-only `roadmap_id` is not distinctive enough for human audit; a
  `YYYY-MM-DD-NN-<slug>` prefix gives each roadmap family a stable epoch label
  while preserving the descriptive slug. The daily ordinal now starts at `00`.
