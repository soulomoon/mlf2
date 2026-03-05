# Findings: 2026-03-05 TMT Improving Loop Orchestrator (Fresh Round 1 Re-Run)

## Baseline Inputs
- Prompt: `docs/prompts/improving-loop-agent.prompt.md`
- Plan: `docs/plans/2026-03-05-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-1.md`
- Table: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Thesis: `papers/these-finale-english.txt`
- Source revision: `035ec160233cbb1ed6ba88abb700f4a3e75933a2`

## Round 1 Verifier Full Sweep (attempt 0)
Ordered gates (YES/NO):
1. Elaboration input -> YES
2. Result-type context wiring -> YES
3. Ordering of transformations -> YES
4. Per-edge propagation transform -> YES
5. Graph operation execution (Graft/Merge/Weaken/Raise) -> YES
6. Replay-map producer normalization (upfront strict contract) -> NO
7. Replay-map consumer bridge in Phi -> NO
8. Translatability normalization -> NO
9. Canonicalization source used by Phi -> NO
10. Identity reconciliation mechanism -> NO
11. Non-root weaken/raise binder resolution -> NO
12. Graph mutation during solve/presolution -> NO
13. Dual-path verification mechanism -> NO
14. Campaign classification status -> YES

Target mechanism selected (first `NO`):
- Row6 `Replay-map producer normalization (upfront strict contract)`

## Attempt Loop Summary (Round 1 / row6)
| Attempt | Bugfix Feasibility | Reviewer | QA | Verifier | Key blocker/outcome |
|---|---|---|---|---|---|
| 1 | YES | YES | YES | NO | `CROSS_PHASE_COUPLING` remains (producer+consumer split contract) |
| 2 | YES | NO | NO | NO | Strict cutover regressed Phase 4/6 (`checked-authoritative` + full gate failed, 38 failures) |
| 3 | YES | NO | NO | NO | Dual-lane repair reduced but did not close row6 (full gate still failed, 16 failures) |
| 4 | NO | NO | NO | NO | Blocked-mode evidence-only attempt; no code edits |
| 5 | YES | NO | NO | NO | Replay-mode refactor caused `ReplayMapIncomplete` cascade (full gate failed, 126 failures) |
| 6 | NO | NO | NO | NO | Terminal blocked-mode attempt; no code edits |

## Key Evidence From Final Attempt State
- Final blocker class: `REPLAY_MODE_CONTRACT_REGRESSION`
- Final verifier row6 gate: `NO`
- Final earlier-YES regression status: `YES`
- Final QA baseline: `cabal build all && cabal test` failed (`126` failures), dominated by `WitnessNormalizationError ReplayMapIncomplete`

## Blocked-Mode Findings
- Bounded patching on top of Attempt-5 baseline is non-viable within the current round.
- Producer and consumer still diverge in replay/no-replay contract handling.
- Required sanity gates from prior YES mechanisms (`Phase 4 thesis-exact unification closure`, `checked-authoritative`, `Dual-path verification`) are no longer stably green in the full baseline.

## Recommended Next Scope (post-run)
- Treat follow-up as scope-expanded cross-phase redesign (row6 + row7, with row8 boundary checks only if required by failure buckets).
- Re-establish row1-5 sanity baseline first, then re-target row6 closure.

## Follow-up Execution Outcome (post Round 1)
- Contract split follow-up plan was implemented across producer and consumer surfaces.
- Replay contract metadata is now explicit (`ReplayContract` + `etReplayContract`) and consumed across presolution/phi/omega.
- Full baseline recovered to green:
  - `cabal build all && cabal test` => PASS.
- BUG-2026-03-05-003 moved to resolved state in `/Volumes/src/mlf4/Bugs.md`.
- Required targeted gates after follow-up are green:
  - `Phase 4 thesis-exact unification closure` (`11 examples, 0 failures`)
  - `checked-authoritative` (`8 examples, 0 failures`)
  - `Dual-path verification` (`4 examples, 0 failures`)
  - `replay-map` (`14 examples, 0 failures`)
