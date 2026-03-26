# Round 099 Controller Recovery Note

Date: 2026-03-26
Round: `round-099`
Stage: `select-task`
Role blocked: guider

## Failure Summary

The controller opened `round-099` on branch `codex/round-099` with dedicated
worktree:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-099`

It then attempted to launch a fresh built-in guider subagent for
`round-099` `select-task` under the repo-local guider contract.

That delegation did not produce trustworthy controller-visible stage output:

- expected path:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-099/orchestrator/rounds/round-099/selection.md`
- observed result: missing after repeated controller waits
- controller-visible failure evidence: the launch returned unusable output
  instead of a qualifying guider result or a live agent handle that could be
  waited on to lawful completion

## Recovery Attempt

Under the shared recovery rules, the controller must use a qualifying
real-subagent `recovery-investigator` before recording direct blockage.

In this packet, the available built-in subagent mechanism is itself the
blocked mechanism: it did not provide controller-usable delegated output or a
qualifying live handle for the failed guider launch, so the controller could
not launch a trustworthy recovery-investigator through that same mechanism.

No qualifying recovery-investigator output was available.

## Controller Decision

The controller cannot safely continue `round-099` because the environment did
not provide:

- a trustworthy guider result for `select-task`; or
- a qualifying recovery-investigator launch through the available built-in
  subagent mechanism.

The machine state therefore remains on `round-099` / `select-task` with the
round branch and worktree preserved for later exact-stage resume after
trustworthy built-in subagent delegation is restored.
