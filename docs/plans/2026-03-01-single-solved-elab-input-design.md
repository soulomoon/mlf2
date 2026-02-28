# Single-Solved Elaboration Input Design

## Summary
Reduce staging divergence in elaboration by collapsing the multi-solved environment (`eeResPhi`, `eeResReify`, `eeResGen`) into a single solved snapshot plus explicit generalization context.

Thesis anchor: elaboration from translatable presolution/witness data (`papers/these-finale-english.txt` §15.2–§15.3).

## Problem Statement
Pipeline currently wires multiple solved views into `ElabEnv`, which keeps an implementation-layer staging split not present in thesis presentation and increases identity/projection complexity.

## Goals
1. `ElabEnv` carries one authoritative solved snapshot.
2. Generalization-specific differences are explicit context data, not a second solved graph channel.
3. Preserve checked-authoritative result behavior and existing regression corpus.
4. Keep `Solved.fromPresolutionResult` path as production baseline.

## Non-Goals
1. Removing `Solved` canonicalization semantics globally.
2. Rewriting presolution/solve algorithms.
3. Changing term-level typing semantics.

## Locked Decisions
1. Single solved handle is authoritative for Φ/reify/elab queries.
2. Generalization extras stay explicit (`GaBindParents`/scope overrides/plan builder data), not hidden in additional solved slots.
3. Migration is incremental and test-gated.

## Interface/Contract Impact
### `MLF.Elab.Elaborate.ElabEnv`
1. Replace split solved fields with a single solved field.
2. Preserve edge artifacts and scope/generalization inputs.

### `MLF.Elab.Run.Pipeline`
1. Build one solved value from presolution.
2. Pass that value through elaboration and result-type context wiring.
3. Keep existing artifact checks and authoritative type-check result policy.

### Tests
1. Add/adjust shape and wiring assertions for single-solved env.
2. Preserve existing native-solved parity and checked-authoritative tests.

## High-Level Flow (Target State)
1. Constraint generation/normalize/acyclic/presolution.
2. `Solved.fromPresolutionResult` => `solved`.
3. Build canonicalized witness/trace/expansion artifacts.
4. Call elaboration with single solved handle + explicit gen/scope context.
5. Keep typechecker authoritative output rule unchanged.

## Risks and Mitigations
1. Risk: hidden coupling in call sites expecting split solved fields.
- Mitigation: compile-driven refactor + targeted spec additions.
2. Risk: regression in fallback generalization paths.
- Mitigation: keep dedicated fallback regressions and run `Phase 6` + pipeline slices.
3. Risk: accidental behavior drift in result-type diagnostics.
- Mitigation: keep diagnostics path running while checked-authoritative remains source of truth.

## Verification Strategy
1. Compile guard: no remaining `eeResPhi|eeResReify|eeResGen` references in `src/`/`test/`.
2. Targeted suites:
- `ElaborationSpec`
- `PipelineSpec`
- `DualPathSpec` / frozen parity slice.
3. Full gate:
- `cabal build all && cabal test`.

## Rollout
1. Refactor `ElabEnv` shape.
2. Migrate call sites in `Elaborate` and `Run/Pipeline`.
3. Update tests for new env shape and preserved behavior.
4. Remove dead references and sync docs.
