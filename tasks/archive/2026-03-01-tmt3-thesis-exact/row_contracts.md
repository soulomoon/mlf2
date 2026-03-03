# Row Contracts (TMT3)

## DEV-TMT-ELAB-SOLVED-PROJECTION
- Thesis anchor: `papers/these-finale-english.txt` §15.2-§15.3.
- Current path: `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Elaborate.hs`.
- Target aligned state: runtime elaboration consumes thesis-shaped presolution artifacts directly (no extra solved projection boundary semantics).
- Required tests: pipeline/elaboration invariants for checked-authoritative behavior.

## DEV-TMT-RESULT-TYPE-CONTEXT
- Thesis anchor: §15.3.
- Current path: `src/MLF/Elab/Run/ResultType/*`.
- Target aligned state: no boundary-only scaffolding required for runtime semantics.
- Required tests: annotation-heavy result typing remains stable and thesis-conformant.

## DEV-TMT-OMEGA-OPERATIONALIZATION
- Thesis anchor: Fig. 15.3.4 and §15.3.4.
- Current path: `src/MLF/Witness/OmegaExec.hs`, `src/MLF/Constraint/Presolution/WitnessCanon.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`.
- Target aligned state: omega execution and normalization follow thesis semantics without non-thesis semantic stripping paths.
- Required tests: witness normalization and replay-map validations.

## DEV-TMT-PHI-CANONICAL-DEPENDENCE
- Thesis anchor: §15.3.5-§15.3.6.
- Current path: `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi/Omega.hs`.
- Target aligned state: runtime Phi decisions are source/replay-domain first, no canonical fallback dependence.
- Required tests: replay-map mismatch fail-fast tests and Phi alignment tests.

## DEV-TMT-IDENTITY-BRIDGE
- Thesis anchor: §15.3.5.
- Current path: `src/MLF/Elab/Phi/IdentityBridge.hs`.
- Target aligned state: remove or reduce to trace-domain adapter with no canonical/class-member fallback policy.
- Required tests: identity lookup behavior in source domain.

## DEV-TMT-SOLVE-REWRITE-LAYER
- Thesis anchor: §12.1.6.
- Current path: `src/MLF/Constraint/Solve.hs`, `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Elab/Run/Pipeline.hs`.
- Target aligned state: downstream runtime consumers do not rely on explicit rewrite helper boundary semantics.
- Required tests: solve snapshot/canonical invariants and pipeline solved-artifact invariants.

## DEV-TMT-DUAL-PATH-GUARDRAIL
- Thesis anchor: §15.3.
- Current path: `test/DualPathSpec.hs`, runtime adjacency in pipeline docs/tests.
- Target aligned state: single-path runtime invariants; no dual-path architectural dependency.
- Required tests: single-path invariants replacing dual-path guardrail semantics.
