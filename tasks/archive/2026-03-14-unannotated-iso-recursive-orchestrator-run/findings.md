# Findings

## 2026-03-14

- The live successor control plane is ready to run: `orchestrator/state.json` is reset to `select-task` with no active round and `last_completed_round: "round-005"`.
- The successor roadmap contains five pending research items for the unannotated iso-recursive track.
- `.worktrees/` already exists and is gitignored, so the controller can create per-round worktrees without additional ignore changes.
- Controller references require strict delegation: only machine-control state, branch/worktree setup, artifact-path bookkeeping, and squash-merge bookkeeping may be handled directly.
- The next legal action from the current state is to start a new round with the guider and create a new round branch/worktree for that selected item.
- The live `round-006` controller shell is now initialized on branch `codex/round-006` with worktree `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006`; `orchestrator/state.json` now records that round context while `stage` remains `select-task`.
- The current environment did not produce a usable delegated guider result for `round-006`: after the subagent handoff, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/orchestrator/rounds/round-006/selection.md` was still absent and the round worktree stayed clean.
- A second fresh guider subagent completed `select-task` successfully after receiving a stricter role-only prompt without controller context leakage.
- `round-006` selected roadmap item 1: write the `R1` gap map from `ARI-C1` to unannotated single-SCC, single-binder-family inference.
- The delegated planner kept `round-006` strictly docs-only and targeted two artifacts: the `R1` gap-map doc plus round implementation notes.
- The delegated implementer produced both planned docs artifacts without touching production code or tests.
- The delegated reviewer approved `round-006`; the only intentionally skipped baseline check was `cabal build all && cabal test`, which the contract allows for docs-only diffs.
- After the delegated `update-roadmap` stage, successor roadmap item 1 is now marked `done`; items 2 through 5 remain `pending`.
- `round-007` selected roadmap item 2 and completed it as a docs-only bounded-subset decision: the accepted `R2` artifact chose candidate subset `URI-R2-C1` plus its admissibility contract.
- After the delegated `update-roadmap` stage for `round-007`, successor roadmap items 1 and 2 are `done`; items 3 through 5 remain `pending`.
- `round-008` completed roadmap item 3 as a docs-only obligation-contract round for the fixed subset `URI-R2-C1`; the accepted `R3` artifact fixed obligation classes, rejection classes, and inherited-audit mappings without entering feasibility or handoff work.
- `round-009` completed roadmap item 4 as a docs-only bounded feasibility decision for `URI-R2-C1`, and the accepted outcome is `not-yet-go` rather than `feasible-continue`.
- `round-010` converted the accepted `R4` `not-yet-go` result into the final bounded item-5 `research-stop` decision, so the successor roadmap terminates without implementation handoff approval.
- After the delegated `update-roadmap` stage for `round-010`, all successor roadmap items are `done`.
