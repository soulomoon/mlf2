# Findings

## 2026-03-18

- The live top-level controller is on `contract_version: 2` and uses the repo-local retry subloop in `orchestrator/retry-subloop.md`.
- The current machine state is idle at `stage: select-task` with no active round, so the controller must start a new round rather than resume an interrupted one.
- The current roadmap is the unannotated iso-recursive successor track with six pending items: `U1` through `U6`.
- The active live subject is repaired `URI-R2-C1`, and the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary remains in force unless an accepted roadmap update explicitly changes it.
- The worktree contract requires one `codex/` branch and one `.worktrees/<round-id>` worktree per round, reused across retries in the same round.
- The successor-design source explicitly says live rounds must resume from history at `round-028`.
- The delegated guider selected roadmap item `U1` for `round-028`.
- The active round is now bound to branch `codex/round-028-u1-unannotated-baseline-bind` and worktree `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-028`.
- After finalized `round-028`, the next lawful successor item is `U2` because `U1` is now done and the live subject remains fixed to repaired `URI-R2-C1`.
- After finalized `round-029`, the next lawful successor item is `U3` because `U2` is now done with bounded result `authority-narrowed` and the live subject remains fixed to repaired `URI-R2-C1`.
- After finalized `round-030`, the next lawful successor item is `U4` because `U3` is now done with bounded result `uniqueness-owner-stable-refuted` and the live subject remains fixed to repaired `URI-R2-C1`.
- After finalized `round-031`, the guider kept `U5` as the next pending item but tightened it so any bounded slice must preserve the inherited explicit-only / non-equi-recursive / non-cyclic boundary under the `U4` refuted result.
- The accepted `U5` round landed exactly one bounded production slice in `MLF.Elab.Run.ResultType.Fallback` plus focused `PipelineSpec` coverage, and the full repo gate passed on that round.
- The accepted `U6` decision gate finalized on attempt `2` with bounded next-step token `continue-bounded`; the initial successor roadmap cycle is now complete without widening beyond repaired `URI-R2-C1`.
