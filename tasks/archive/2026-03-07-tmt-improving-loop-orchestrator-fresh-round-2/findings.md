# Findings: 2026-03-07 TMT Improving Loop Orchestrator (Fresh Round 2)

## Baseline Provenance
- Baseline commit: `816eb2e308091506a5b1b10b385a3a0984f92209`
- Baseline timestamp (UTC): `2026-03-06T19:55:12Z`
- Thesis source of truth: `papers/these-finale-english.txt`
- Live mechanism table: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Orchestrator prompt: `docs/prompts/improving-loop-agent.prompt2.md`

## Round 1 Outcome
- The first live `NO` was row 2, `Result-type context wiring`.
- Thesis-side evidence for row2 is Definition 15.3.2 / 15.3.3 plus the `T(a)` equations in §15.3.6: result typing is phrased directly over translated presolution artifacts and witness computations, with no solved-adapter boundary.
- Code-side evidence showed the last live row2 adapter in three places:
  - `Pipeline` seeded clean/generalization views from solved values rather than finalized snapshots;
  - `ResultType.View` validated via `ChiQuery.chiSolved`;
  - `ChiQuery` still defined solved-compat shims used only by row2.
- Accepted fix:
  - `Finalize.finalizePresolutionViewFromSnapshot` now feeds the live row2 pipeline path;
  - `ResultType.View` validates directly from canonical constraint + canonical map;
  - `MLF.Elab.Run.ChiQuery` no longer defines `chiSolvedCompat` / `chiSolved`.
- Round 1 validation:
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `row2 closeout guard` — PASS (`3 examples, 0 failures`)
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS.

## Round 2 Outcome
- After row2 closed, the next live `NO` was row 8, `Translatability normalization`.
- Thesis-side evidence for row8 is Definition 15.2.10 / Theorem 15.2.11 plus §15.2.8’s stronger modular internal-language rule: weaken all inert nodes via `W`.
- Code-side evidence showed `rigidifyTranslatablePresolutionM` still weakened only inert-locked nodes before/after rigidification.
- Accepted fix:
  - `rigidifyTranslatablePresolutionM` now calls `Inert.weakenInertNodes` before and after rigidification.
- Regression side effect and recovery:
  - This intentionally changed frozen solved artifacts (`cWeakenedVars`), so `Frozen parity artifact baseline` failed on the first full-gate pass.
  - Regenerating `test/golden/legacy-replay-baseline-v1.json` with `frozen-parity-gen` re-froze the new thesis-exact artifacts and restored the full gate.
- Round 2 validation:
  - `row8 thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `Translatable presolution` — PASS (`10 examples, 0 failures`)
  - `O15-TRANS*` — PASS (`5 examples, 0 failures`)
  - `O05-*` — PASS (`3 examples, 0 failures`)
  - `Frozen parity artifact baseline` — PASS (`1 example, 0 failures`)
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS.

## Final Sweep Outcome
- Final mechanism gates on the live codebase:
  1. Elaboration input — `YES`
  2. Result-type context wiring — `YES`
  3. Ordering of transformations — `YES`
  4. Per-edge propagation transform — `YES`
  5. Graph operation execution (Graft/Merge/Weaken/Raise) — `YES`
  6. Replay-map producer normalization (upfront strict contract) — `YES`
  7. Replay-map consumer bridge in Phi — `YES`
  8. Translatability normalization — `YES`
  9. Canonicalization source used by Phi — `YES`
  10. Identity reconciliation mechanism — `YES`
  11. Non-root weaken/raise binder resolution — `YES`
  12. Graph mutation during solve/presolution — `YES`
  13. Dual-path verification mechanism — `YES`
  14. Campaign classification status — `YES`
- The campaign is back to a full 14/14 `YES` closeout on the live codebase.
