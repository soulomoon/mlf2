# Findings

## 2026-03-25

- `orchestrator/state.json` currently records `contract_version: 2`,
  `active_round_id: null`, `stage: "done"`, `last_completed_round:
  "round-088"`, and `retry: null`.
- `orchestrator/roadmap.md` still has items `1` through `5` marked
  `[pending]`, so the current `done` state is stale and non-terminal under the
  shared resume rules.
- Repo-local role definitions exist in `.codex/agents/` for guider, planner,
  implementer, reviewer, and merger, so the controller should use those
  sources rather than `orchestrator/roles/*.md`.
- The refreshed control plane uses the retry-subloop contract for roadmap
  items `1` through `4`, with `max_attempts: 100` and controller-owned
  `attempt-log.jsonl` when review requests retry.
- The worktree is currently clean (`git status --short` empty), which removes
  unrelated workspace-noise risk before round startup.
- Creating `codex/round-089` with `git worktree add ... -b ...` created the
  branch before failing on a pre-existing empty `.worktrees/round-089`
  directory. The recovery path was to delete only the empty controller-owned
  directory tree and then attach the already-created branch with
  `git worktree add .worktrees/round-089 codex/round-089`.
- The first built-in guider dispatch for `round-089` `select-task` did not
  produce `orchestrator/rounds/round-089/selection.md` in the active round
  worktree after repeated controller waits. This matches prior non-observable
  guider behavior captured in archived orchestrator-run packets, where the
  lawful recovery is to keep the round/stage fixed, record a recoverable
  `resume_error`, and relaunch a fresh guider for the same stage.
- The recovery-investigator classified the failure as incidental delegation
  failure, not corrupt state or missing-worktree metadata, and recommended the
  same round / same stage / same worktree relaunch path.
- The successful relaunch wrote
  `orchestrator/rounds/round-089/selection.md`, which
  selected roadmap `item-1` and kept the round bounded to the exact
  same-lane retained-child persistence pocket
  `boundVarTargetRoot` /
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
- The controller merged `round-089` into
  `codex/automatic-recursive-type-inference` as commit `76ac1a1`, then
  committed the roadmap-only `update-roadmap` change as `2c2cfc9`.
- The updated roadmap now marks item `1` done and keeps item `2` as the next
  bounded audit of the same frozen tuple and ledger.
- The controller merged `round-090` into
  `codex/automatic-recursive-type-inference` as commit `71edf2f`, then
  committed the roadmap-only `update-roadmap` change as `fcb2337`.
- The updated roadmap now marks item `2` done and tightens item `3` to the
  exact Phase 6 elaboration breakpoint on the same frozen packet.
