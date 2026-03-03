# Findings

## 2026-03-03
- `MLF.Elab.Run.ChiQuery` is sufficient as a thin `PresolutionView` facade
  for all chi-first runtime queries required by elaboration/result-type internals.
- `runPipelineElabWith` already provides both presolution view and solved
  compatibility handles, so migration did not require behavioral changes to the
  checked-authoritative pipeline contract.
- Remaining solved compatibility is now explicit and narrow:
  - elaboration boundary input: `eeSolvedCompat` (`MLF.Elab.Elaborate`);
  - result-type boundary input: `rtcSolvedCompat` + `rtcSolveLike`
    (`MLF.Elab.Run.ResultType.Types`);
  - bound-overlay reconstruction remains isolated in `ResultType.View`.
- Guard strategy that worked:
  - source-level anti-regression checks for `Solved.fromConstraintAndUf`
    absence in required internal targets;
  - gate-labeled matcher aliases in `PipelineSpec` to enforce exact command
    filters from the execution plan.
- Validation evidence:
  - Gate A/B/C passed on this branch after migration;
  - final closeout slice
    `--match "Phase 6 — Elaborate|ResultType|Dual-path verification"`
    passed during Task 6 closeout.
