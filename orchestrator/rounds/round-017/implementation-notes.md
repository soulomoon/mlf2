# Round 017 Implementation Notes

- Removed the unauthorized `src/MLF/Research/URI/R2/C1/Prototype/P1.hs` round diff and restored the approved file boundary before repairing `P2`.
- Reworked the `P2` prototype path to execute a bounded shared-entrypoint fixture through `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback -> witness replay`, then emit attempt-local evidence keyed by the inherited `P1` `subject_id` and one shared `correlation_id`.
- Replaced the fabricated `P2` trace handles and pass checks with trace refs derived from actual replay outputs, correlated `check-P2-*` JSON, a pass-gated reaffirmed `subject-token.json`, and a regenerated `P2` artifact at `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`.
- Kept the shared research entrypoint, `URI-R2-C1` / `uri-r2-c1-only-v1` isolation, `P3` rejection behavior, and default no-argument `mlf2` execution unchanged while tightening the focused prototype spec around attempt-local writes, repeated `correlation_id`, and preserved inherited token fields.
- Attempt-2 repair: `P2-W` now classifies `applyInstantiation` replay diagnostics as `semantic-negative` with rejection trigger `partial-replay`, which makes the stage verdict bounded non-pass and suppresses `subject-token.json` for `attempt-2`.
- Attempt-2 repair: the shared prototype entrypoint now accepts bounded P2 reruns for attempt ids `1` through `3`, and the P2 artifact renders the active attempt id and omits any handoff section when the stage is non-pass.
