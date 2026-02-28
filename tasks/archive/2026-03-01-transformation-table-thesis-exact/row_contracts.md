# Row Contracts (Wave 0 Lock)

For each row in `docs/notes/2026-02-27-transformation-mechanism-table.md`:
- thesis anchor
- current code path
- target state (`Aligned` or `Deviation`)
- required tests/evidence

## Initial target mapping
1. Elaboration input -> Deviation (`DEV-TMT-ELAB-SOLVED-PROJECTION`)
2. Result-type context wiring -> Deviation (`DEV-TMT-RESULT-TYPE-CONTEXT`)
3. Ordering of transformations -> Aligned
4. Per-edge propagation transform -> Aligned
5. Graph operation execution -> Deviation (`DEV-TMT-OMEGA-OPERATIONALIZATION`)
6. Replay-map producer normalization -> Aligned
7. Replay-map consumer bridge in Phi -> Aligned
8. Translatability normalization -> Aligned
9. Canonicalization source used by Phi -> Deviation (`DEV-TMT-PHI-CANONICAL-DEPENDENCE`)
10. Identity reconciliation mechanism -> Deviation (`DEV-TMT-IDENTITY-BRIDGE`)
11. Non-root weaken/raise binder resolution -> Aligned
12. Graph mutation during solve/presolution -> Deviation (`DEV-TMT-SOLVE-REWRITE-LAYER`)
13. Dual-path verification mechanism -> Deviation (`DEV-TMT-DUAL-PATH-GUARDRAIL`)
14. Convergence status -> Aligned (campaign-tracking row)

## Required evidence mapping (locked)
1. Phi replay invariants:
   - `fails fast when replay-map source domain mismatches trace binder sources`
   - `fails fast when replay-map codomain target is outside replay binder domain`
   - `trace/replay binder key-space mismatch (OpRaise unresolved trace-source target)`
2. Presolution contract invariants:
   - `replay-map validation`
   - `Driver replay-map boundary validation`
   - `Witness normalization invariants`
3. Pipeline/elab boundary invariants:
   - `single-solved migration removes eeRes`
   - `single-solved migration removes split result-type solved fields`
   - `single-solved refactor keeps checked pipeline authoritative`
   - `Dual-path verification`
