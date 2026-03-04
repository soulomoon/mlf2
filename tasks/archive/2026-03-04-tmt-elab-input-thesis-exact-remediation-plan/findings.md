# Findings — 2026-03-04 TMT Elaboration Input Thesis-Exact Remediation Plan

- Wave 2 + Wave 3 closeout is complete for this remediation plan; Team E scope
  is documentation/verification evidence only.
- Current active elaboration boundary is recorded as `χp`-native with direct
  references:
  - `src/MLF/Elab/Run/Pipeline.hs:112-141` (`generalizeAtWithView`,
    `ElabConfig`, `ElabEnv` wiring)
  - `src/MLF/Elab/Elaborate.hs:64-89` (`GeneralizeAtWith`, `ElabEnv`)
  - `src/MLF/Elab/Elaborate.hs:181-203` (`elaborateWithEnv` over
    `eePresolutionView`)
  - `src/MLF/Elab/Phi/Translate.hs:246-293`
    (`GeneralizeAtWith`, `phiFromEdgeWitnessWithTrace` active callback shape)
- Guardrails used for thesis-exact closeout remain explicit and are now cited
  in the transformation table row:
  - `test/PipelineSpec.hs:176-180`
  - `test/ElaborationSpec.hs:331-344`
- Transformation Mechanism Table row `Elaboration input` is now marked
  `Thesis-exact = Yes` in
  `docs/notes/2026-02-27-transformation-mechanism-table.md`.
- Verification evidence recorded from already-run gates:
  - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
