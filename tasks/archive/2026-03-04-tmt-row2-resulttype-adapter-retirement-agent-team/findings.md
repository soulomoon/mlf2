# Findings

## 2026-03-04
- Initial reconnaissance identified row2 adapter surfaces (`rtcSolvedCompat`,
  `rtcSolveLike`, `ecSolved`) as the closeout targets for this wave.
- Final source verification after wave merges:
  - `rg -n "rtcSolvedCompat|rtcSolveLike|ecSolved" src test src-public app`
    reports these symbols only in row2 closeout guard tests
    (`test/PipelineSpec.hs`, `test/ElaborationSpec.hs`), not in `src/`.
- `ResultTypeInputs` now carries `rtcPresolutionView` directly and no longer
  exposes solved-compat adapter fields.
- `ElabConfig` exports only `ecTraceConfig` and `ecGeneralizeAtWith`; there is
  no `ecSolved` field.
- Verification gates for row2 closeout are green:
  - `row2 closeout guard`: `3 examples, 0 failures`
  - `checked-authoritative`: `8 examples, 0 failures`
  - `Dual-path verification`: `4 examples, 0 failures`
  - Full suite: `929 examples, 0 failures`
- No concrete defects were found during verifier closeout; `Bugs.md` did not
  require updates.
