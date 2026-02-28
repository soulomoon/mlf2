# Findings

## 2026-03-01
- Campaign initialized.
- Current transformation table already reflects major 2026-03-01 updates (single-solved elaboration wiring + strict replay bridge pass-through).
- Remaining non-aligned rows are primarily architecture/representation divergences requiring either deep refactor or explicit deviation ledger mapping.
- Added explicit deviation IDs for all remaining non-aligned row clusters in `docs/thesis-deviations.yaml`:
  - `DEV-TMT-ELAB-SOLVED-PROJECTION`
  - `DEV-TMT-RESULT-TYPE-CONTEXT`
  - `DEV-TMT-OMEGA-OPERATIONALIZATION`
  - `DEV-TMT-PHI-CANONICAL-DEPENDENCE`
  - `DEV-TMT-IDENTITY-BRIDGE`
  - `DEV-TMT-SOLVE-REWRITE-LAYER`
  - `DEV-TMT-DUAL-PATH-GUARDRAIL`
- Reclassified table rows to binary status (`Aligned` or `Deviation(<id>)`) with matching code/test evidence.
- Baseline quality is green on current master (`cabal build all`, `cabal test`, and targeted Phi/Presolution/Pipeline slices).
- No open bugs in `Bugs.md` at baseline start; no new baseline failures requiring bug entries.
