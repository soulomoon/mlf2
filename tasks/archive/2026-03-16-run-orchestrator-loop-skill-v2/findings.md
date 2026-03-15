# Findings

- The shared skill currently loads `orchestrator/state.json`, `orchestrator/roadmap.md`, `orchestrator/verification.md`, and `orchestrator/roles/`, but it does not mention `orchestrator/retry-subloop.md`, so it cannot learn repo-local retry rules by contract.
- The stale controller rule is concentrated in three places:
  - `SKILL.md` says only “On review rejection, return to plan for the same round.”
  - `references/state-machine.md` only allows `review -> plan when rejected` and `review -> merge when approved`.
  - `references/resume-rules.md` only models plain rejection, not accepted-but-retryable outcomes or active retry state.
- The generic skill can stay reusable if it frames retries in terms of repo-local review fields and machine state instead of hard-coding `P1`/`P2`/`P3`.
- The shared skill should not invent retry behavior on its own. It should read repo-local retry docs/state when present, then obey them.
- The shared skill docs are now aligned with the repo-local v2 model: they load `orchestrator/retry-subloop.md` when present, treat retryable review outcomes generically, and preserve the same round/worktree across retries.
- Remaining limit: this follow-up aligned the controller documentation and references, but it did not execute a fresh delegated orchestrator round under the new v2 contract.
