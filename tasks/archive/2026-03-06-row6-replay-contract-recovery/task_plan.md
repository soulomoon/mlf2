# Task Plan: 2026-03-06 Row6 Replay-Contract Recovery

## Goal
Implement the post-MAXIMUMRETRY row6 replay-contract recovery plan with Wave A/B/C/D and restore green verification.

## Scope
- Worktree: /Users/ares/.config/superpowers/worktrees/mlf4/row6-replay-contract-recovery-20260306
- Branch: codex/row6-replay-contract-recovery-20260306
- Base commit: a82d8a80fe7620c615cbb9bac0eef836ba4ff7d3
- Source plan: user-provided "Row6 Replay-Contract Recovery Plan (Post-MAXIMUMRETRY)"

## Phases
| Phase | Status | Notes |
|---|---|---|
| Wave 0 clean base + task initialization | completed | Clean recovery worktree verified with `cabal build all && cabal test` -> PASS (`951 examples, 0 failures`). |
| Wave A producer-lane authority | completed | `WitnessNorm` now derives no-replay behavior in source identity space, preserves strict no-replay `OpWeaken` only when the source-domain witness survives projection, hard-rejects residual rogue single-target no-replay grafts, and keeps producer replay-map injectivity/domain checks green. |
| Wave B consumer strictness | completed | Producer artifacts now stop leaking no-replay wrapper `OpRaise`/`OpGraft` shapes into Phi, restoring strict fail-fast only on intended bug-002 paths while preserving valid checked/unchecked elaboration. |
| Wave C checked-authoritative alignment | completed | `checked-authoritative`, `Dual-path verification`, explicit-forall, parser, and A6/BUG-2026-02-08-004/BUG-2026-02-17-002 slices all returned green under the recovered producer contract. |
| Wave D closeout/docs/archive | completed | Final verification `cabal build all && cabal test` -> PASS (`954 examples, 0 failures`); repo docs/task records updated and task ready to archive. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| Broad semantic strict replay on all non-root `OpWeaken` | Early Wave A patch | Replaced with source-domain no-replay projection: keep strict no-replay only for surviving source-domain `OpWeaken`, not copied/wrapper weakens. |
| No-replay `OpRaise` leak to Phi (`OpRaise (non-spine): missing computation context`) | Mid Wave A/B patch | Restricted no-replay `OpRaise` retention to type-tree-bound validation cases; wrapper raises under `GenRef`/no parent are pruned before Phi. |
| Rogue no-replay `OpGraft` residual check never triggered because grafts were projected away first | Mid Wave A patch | Moved residual no-replay replay-family rejection to the raw normalized op list, with the graft predicate narrowed to single-target source-interior residuals. |

## Decisions
- Implement in isolated worktree; do not touch dirty /Volumes/src/mlf4 master tree.
- Use RED->GREEN loops for each wave gate.
- Compare no-replay projection in source-domain identities (`restoreNode`) rather than rewritten canonical ids; wrapper/replay classification belongs to the trace-domain contract.
- Preserve baseline no-replay weakening heuristic (`graftTargetCount`) for success paths, then layer only the new producer-boundary fail-fast for residual rogue replay-family ops.
