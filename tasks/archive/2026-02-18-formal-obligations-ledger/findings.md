# Findings: Formal Obligations Ledger

## Baseline
- Existing gate script: `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh`.
- Existing CI workflow already runs gate.
- Existing test coverage includes significant witness/translatability matrix (`R-*`) and phase-7 checks.

## Thesis scope selected
- Chapter 14: 14.2 and 14.3 core rules.
- Chapter 15: 15.2 and 15.3 core translation/translatability rules.

## Implementation findings
- Ledger schema + deterministic ID-set checks are best enforced in the checker, not only renderer validation, to hard-fail drift in CI.
- `rootedConstraint` auto-inserts/rewrites root `GenNodeId 0`; for translatability negative fixtures with explicit `cGenNodes`, this can silently alter intended scheme-root ownership.
- `InstUnder` + `InstAbstr` succeeds only when the under-body type matches the binder bound in the typechecker environment; invalid combinations should be represented as explicit rejection anchors.
- Running matcher checks obligation-by-obligation (`--match` per ID) catches grouped-anchor brittleness quickly and is necessary to enforce “every rule has executable anchor” as an operational invariant.
