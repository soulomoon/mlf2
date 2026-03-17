# Findings

## 2026-03-17

- The live top-level controller is already on `contract_version: 2` and uses the repo-local retry subloop in `orchestrator/retry-subloop.md`.
- The current machine state is idle at `stage: select-task` with no active round, so the controller must start a new round rather than resume an interrupted one.
- The current roadmap is the `URI-R2-C1` replay repair track with four pending stages: `R1`, `R2`, `R3`, and `R4`.
- The active repair boundary is fixed to `URI-R2-C1`, `uri-r2-c1-only-v1`, and the localized `MLF.Elab.Inst.applyInstantiation` / `InstBot` owner area under `BUG-2026-03-16-001`.
- The worktree contract requires one `codex/` branch and one `.worktrees/<round-id>` worktree per round, reused across retries in the same round.
- The current environment did not yield an observable completion artifact or result for the delegated guider `select-task` attempt, which blocks the controller from safely advancing without violating the role-delegation contract.
- A fresh non-forked stage subagent can still satisfy the delegation contract in this environment; the second guider dispatch wrote the required `selection.md` and selected `R1`.
- `round-024` is now bound to roadmap item `R1`, branch `codex/round-024-r1-repair-boundary-reproduction`, and worktree `.worktrees/round-024`.
- The repair-track loop completed successfully through `round-027`, with `R1`, `R2`, `R3`, and `R4` all finalized as authoritative passes under the repo-local contract.
- `R2` required same-round retry handling to contract-scope inherited prototype-gate failures while keeping the bounded `InstBot` repair intact; the authoritative `R2` result is attempt `3`.
- `R3` and `R4` both finalized as docs-only rounds, preserving the locked `URI-R2-C1` scenario and consuming inherited authority rather than reopening repair work.
- The terminal `R4` decision gate recorded the single final bounded outcome `repair-accepted`, and the live controller now rests at `stage: done` with `last_completed_round: round-027`.
