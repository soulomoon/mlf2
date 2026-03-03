# Findings: 2026-03-03 transformation mechanism table doc audit

## Source references to collect
- Thesis: `papers/these-finale-english.txt`
- Supplement (if needed): `papers/xmlf.txt`
- Implementation: modules referenced by the note and corresponding tests

## Findings log
- Header metadata was stale: `Source revision` in the target doc did not match current `HEAD` (`cfe7e8a`).
- Rows with stale implementation claims:
  - `Elaboration input`: doc referenced `MLF.Elab.Run.PipelineBoundary`, but current code finalizes snapshots inline in `src/MLF/Elab/Run/Pipeline.hs`.
  - `Graph mutation during solve/presolution`: same stale `PipelineBoundary` claim.
  - `Result-type context wiring`: doc referenced `rtcSolved`, but current `ResultTypeInputs` fields do not include `rtcSolved`.
- Rows confirmed accurate by code audit:
  - `Per-edge propagation transform`
  - `Replay-map producer normalization`
  - `Replay-map consumer bridge in Phi`
  - `Translatability normalization`
  - `Canonicalization source used by Phi`
  - `Identity reconciliation mechanism`
  - `Non-root weaken/raise binder resolution`
  - `Campaign classification status`
- Thesis-faithfulness wording overclaims found:
  - Delayed weakenings are part of normalized propagation witnesses (§15.2.1), so treating delayed weaken as purely removable scaffolding overstates thesis intent.
  - Binder-target language should not imply uniqueness; thesis allows multiple propagation witnesses/choices.
  - “Validate translatability once at the end” is too narrow; translatability includes constructive conditions/assumptions (Def. 15.2.10, §15.2.8), not only a terminal predicate check.
  - “No bridge object in thesis” supports a presentation distinction, not a strict prohibition on equivalent runtime structure.
