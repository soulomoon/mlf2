# Findings

## Starting Point

- The live controller was parked at accepted `rev-004` with
  `stage: "done"` and `last_completed_round: "round-116"`.
- The accepted rev-004 handoff explicitly selected
  `stop after bounded settlement` for the exact same-lane `C2` / `C5` / `C7`
  pocket and did not publish a `rev-005` bundle.
- The next honest loop is therefore a new roadmap family, not another
  same-family exact-pocket revision.
- Current repo-local roles were stale and still described the earlier
  same-lane stable-visible-persistence loop rather than the new repo-scope
  successor question.
- `.gitignore` did not yet contain a tracked ignore rule for
  `orchestrator/worktrees/`.

## Chosen Scaffold Shape

- New roadmap family:
  `2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap`
- Initial revision:
  `rev-001`
- Three-item loop:
  1. freeze the post-rev-004 repo-scope boundary,
  2. publish and validate a refreshed representative matrix,
  3. record one narrowed repo-scope successor gate and handoff.

## Parallel-Agent Contract

- One selected roadmap item remains active at a time.
- Parallelism is allowed only inside one selected round and only across
  bounded independent lanes.
- Shared authoritative files remain single-writer.
- Shared `dist-newstyle` concurrent Cabal execution is forbidden.
