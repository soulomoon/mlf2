# Post-Rev-004 Successor Loop Scaffold

## Goal

Scaffold the next repo-local orchestrator loop after accepted `rev-004`,
aimed at the narrowed repo-scope blocker set:

1. freeze a post-`rev-004` successor boundary;
2. publish an authoritative refreshed representative matrix that carries
   forward the repaired exact-pocket `C2` / `C5` / `C7` read plus bounded
   `C1` / `P5` evidence; and
3. rerun the repo-scope decision honestly on that refreshed matrix.

The new loop should be tailored for parallel-agent runtime, but this packet
stops after scaffold and checkpoint commit.

## Scope

- Scaffold only; do not start runtime rounds.
- Preserve accepted March 25 / March 26 artifacts as immutable historical
  evidence.
- Keep unrelated worktree changes untouched.
- Stage and commit only scaffold-relevant files.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| Create scaffold task packet | complete | Planning files for this scaffold turn created. |
| Survey repo, orchestrator, and skill references | complete | State, roadmap family, roles, verification, retry, and scaffold rules reviewed. |
| Dispatch parallel subagents for independent survey threads | complete | Roadmap-shape, contract/roles, and commit-scope checks returned. |
| Scaffold successor loop files | complete | New roadmap family, controller pointers/state, roles, ignore rule, and recovery-investigator role added. |
| Verify scaffold and create checkpoint commit | complete | Verified scaffold, staged scaffold-only files, and created the checkpoint commit. |

## Decisions

- Start a new roadmap family at `rev-001` rather than a `rev-005` under the
  prior family, because accepted rev-004 explicitly stopped after bounded
  exact-pocket settlement and did not publish a rev-005 bundle.
- Keep the new loop repo-scope and docs-first with three items:
  successor boundary freeze, refreshed representative matrix, and narrowed
  successor gate.
- Add parallel-agent guidance at the role/verification/retry-contract level
  rather than by planning parallel rounds.

## Errors Encountered

| Error | Attempt | Resolution |
| --- | --- | --- |
| Top-level pointer stub duplicated the old and new `roadmap_id` after the first scaffold edit. | 1 | Removed the stale rev-004 `roadmap_id` line and rechecked the pointer stub. |
