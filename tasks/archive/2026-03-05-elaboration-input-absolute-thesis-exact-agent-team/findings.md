# Findings: Elaboration Input Absolute Thesis-Exact Planning

## 2026-03-05 Audit Findings
- Thesis contract (Def. 15.3.12, Sec. 15.3.6) expects elaboration input to be translatable-presolution driven with edge translation from chosen propagation witnesses.
- Current row is marked `Thesis-exact = Yes` under strict policy including test-only paths, but remaining internal surfaces still leave room to become more thesis-direct.

## Concrete Gaps To Target
- `src/MLF/Elab/Phi/Env.hs` still carries a `Solved` handle (`peResult`, `askResult`) in PhiM environment, even though active translation flow is `PresolutionView`-driven.
- `src/MLF/Elab/Phi/Translate.hs` still imports `fromSolved` and keeps `remapSchemeInfoM` on the solved-backed env path, while core flow already accepts `PresolutionView`.
- `src/MLF/Elab/Run/Scope.hs` documents a known deviation: `preferGenScope` swallows `bindingPathToRoot` errors (`Left _ -> ref`) rather than preserving explicit failure.
- `src/MLF/Elab/Phi/TestOnly.hs` retains synthetic trace construction (`phiFromEdgeWitnessAutoTrace`) for fixtures; this is practical but not fully thesis-native witness input.
- Guard tests currently focus on solved-typed API signatures and selected marker strings; they do not comprehensively guard the residual internal solved/env + synthetic-trace surfaces above.

## Planning Direction
- Use a wave plan with one RED guard wave, one parallel implementation wave (Teams B/C/D), and one integration/docs wave.
- Keep checked-authoritative and dual-path verification as mandatory non-regression gates.

## Controller Execution Kickoff Findings
- Current workspace baseline at execution kickoff is clean (no tracked file deltas), reducing risk of mixing unrelated edits into team commits.
- Existing task folder artifacts were present and required only phase-state transition from planning -> execution.
- Wave 0 guard finalized to assert absence of these residual surfaces:
  - `peResult :: Solved`
  - `askResult ::`
  - `Left _ -> ref`
  - `phiFromEdgeWitnessAutoTrace`
- RED gate currently fails on first marker as expected, proving pre-Wave-1 baseline remains non-absolute.

## Wave 1 Findings
- Team B removed solved-backed Phi env surface area in owned files with a small API contraction footprint (23 deletions vs 5 insertions).
- Team C introduced explicit error propagation for ga-scope handling and added dedicated regression coverage for binding-tree cycle errors.
- Team D removed the synthetic auto-trace helper and rewired elaboration tests around explicit trace fixtures while preserving fail-fast no-trace behavior.
- Hspec `--match` filters with pipe-separated fragments may match zero examples in this suite depending on literal/regex interpretation; supplementary single-pattern checks were needed for meaningful local evidence in Team B/D focused runs.

## Wave 2 + Closeout Findings
- Integration cherry-picks applied without conflicts in Team E and in main workspace.
- The absolute guard now directly tracks the three residual surfaces from the
  strict all-path criterion:
  1) solved-backed Phi env fields/accessors,
  2) ga-scope error swallowing fallback,
  3) synthetic auto-trace helper surface.
- BUG-2026-03-04-002 evidence is now resolved by code state and gate proof
  (`absolute guard` + `checked-authoritative` + `Dual-path verification` +
  full suite all green).
- Final gate reruns on `/Volumes/src/mlf4` matched Team E integration results
  exactly (`1` / `8` / `4` / `934` examples, all zero failures).
