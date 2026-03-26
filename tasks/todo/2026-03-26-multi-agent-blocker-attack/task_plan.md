# Multi-Agent Blocker Attack

## Goal

Execute three bounded next steps in parallel for the current automatic
iso-recursive blocker set:

1. add a `C1` authoritative-surface harness on existing pipeline surfaces;
2. add one bounded positive-`P5` probe inside the current admissibility rules;
3. draft the repo-level successor packet needed to refresh the representative
   matrix and rerun the global `keep` vs `reopen` gate after the repaired
   exact `C2` / `C5` / `C7` pocket.

## Scope

- Keep the `C1` work exact-packet-only for the non-local
  `baseTarget -> baseC -> targetC` route.
- Keep the `P5` work bounded to one positive probe that preserves the current
  clear-boundary contract.
- Keep the repo-level work docs-only and packet-only.
- Do not widen into broad architecture redesign, multi-SCC search, second
  interfaces, or fallback behavior.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| Initialize execution packet | complete | Planning files created; prior investigation packets treated as read-only context. |
| Gather code/test/doc context | complete | Relevant `PipelineSpec.hs`, `Fallback.hs`, and accepted March 25 / March 26 docs read. |
| Dispatch bounded workers | complete | Repo-level refresh draft and `P5` probe module returned; `C1` worker provided exact context, then the main rollout completed the missing module locally. |
| Integrate worker output | complete | Added the `C1` research module, fixed the `P5` module, and wired both into `test/Main.hs` and `mlf2.cabal`. |
| Verify bounded outputs | complete | `git diff --check` passed; focused `C1` and `P5` Cabal runs both passed. |
| Final synthesis | complete | Outputs and remaining repo-level next step ready to report. |

## Decisions

- Use one new task folder because this run is multi-step, cross-file, and
  uses subagents.
- Keep worker write scopes disjoint to avoid merge conflicts:
  `test/Research/C1AuthoritativeSurfaceSpec.hs`,
  `test/Research/P5ClearBoundarySpec.hs`,
  and one new docs/task packet for the repo-level successor draft.
- Main rollout owns any shared wiring such as `test/Main.hs` and `mlf2.cabal`.
- The `C1` worker stalled after context gathering, so the main rollout
  finished the missing module locally using the exact helper and pipeline
  outputs it had already identified.

## Errors Encountered

| Error | Attempt | Resolution |
| --- | --- | --- |
| Parallel `cabal test` invocations collided in `dist-newstyle`. | 1 | Switched back to sequential focused Cabal runs. |
