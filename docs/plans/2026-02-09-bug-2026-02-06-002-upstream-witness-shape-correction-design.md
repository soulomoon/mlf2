# BUG-2026-02-06-002 Upstream Witness-Shape Correction Design

## Scope and Decision (2026-02-09)

This design continues `tasks/todo/2026-02-08-investigate-bug-phase7` using the selected strategy:
**Option 1 — upstream witness-shape correction**.

Current known state:
- H15 naming leak (`t23`) is fixed.
- H16.1–H16.4 local probes in `MLF.Elab.Phi.Omega` were either ineffective or regressive and were reverted.
- The sentinel matrix in `test/PipelineSpec.hs` is intentionally `pendingWith` while the bug is open.

The remaining defect (`BUG-2026-02-06-002`) is the polymorphic-factory mismatch where elaboration still locks to a `TBottom`/`Int`-specialized arrow shape in cases that should remain polymorphic.

## Problem Statement

Evidence from H16 indicates the issue is not a single `OpWeaken` translation bug in isolation. The failure emerges from the shape of witness operations entering Φ translation:
- `OpGraft ... OpWeaken` pairs are present, but local translation tweaks alone do not recover `b -> a` behavior.
- Changing weaken semantics directly can create `TCExpectedArrow` regressions.
- A pre-weaken arg-source tweak in `OpGraft` had no effect on the codomain `Int` lock-in.

Therefore the correction point should move earlier: the normalized witness shape produced by presolution (`ewSteps`/`ewWitness`) and its edge-trace correspondence (`etBinderArgs`, copy mapping, interior membership).

## Design Goals / Non-Goals

### Goals
1. Make witness normalization produce a stable, thesis-faithful `OpGraft/OpRaise/OpWeaken` shape before elaboration.
2. Remove the need for heuristic/special-case behavior in Ω translation for this bug family.
3. Preserve current strict translatability invariants (`PhiTranslatabilityError` / `PhiInvariantError`) and avoid permissive fallback.
4. Graduate current pending sentinel cases into real assertions once the bug is fixed.

### Non-Goals
1. No broad redesign of the type checker or evaluator.
2. No new “silent success” fallback for malformed witnesses.
3. No unrelated cleanup outside bug path + required regression coverage.

## Proposed Architecture

### 1) Witness-shape invariants become explicit in presolution normalization

Extend witness normalization (`MLF.Constraint.Presolution.WitnessNorm` + `MLF.Constraint.Presolution.WitnessCanon`) to encode and enforce shape constraints for problematic `graft→weaken` regions.

Core invariant family:
- For a binder-targeted `OpGraft σ b` followed by `OpWeaken b`, normalization must preserve a canonical relation among:
  - canonical binder id,
  - copied/original binder identity from `etCopyMap`,
  - binder argument mapping from `etBinderArgs`,
  - interior membership / ordering keys.
- The normalized sequence must be deterministic under canonicalization and idempotent (`normalize . normalize = normalize`).

This moves correctness from ad hoc elaboration-time reconstruction into presolution’s witness contract.

### 2) Edge-trace binder/arg alignment is treated as contract input

`normalizeEdgeWitnessesM` already rewrites steps through copy-map and computes canonical binder-arg maps. The design strengthens this phase by making binder-arg pairing first-class for normalization decisions, not just merge ordering.

Concretely:
- keep operations in rewritten node space while normalizing + validating;
- use canonical binder/arg alignment to detect malformed or ambiguous weaken placement;
- reject malformed shape with `WitnessNormalizationError` instead of letting Ω translation infer intent.

### 3) Ω translation remains strict but simpler

In `MLF.Elab.Phi.Omega`, keep the current strict translation posture and avoid adding new weaken heuristics for this bug.

Expected effect:
- If upstream witness shape is corrected, current Ω logic should translate without introducing the observed `⊥ -> Int` lock-in for the remaining buggy shapes.
- If shape is still invalid, elaboration fails explicitly (translatability/invariant error) rather than drifting into wrong specialization.

## Data-Flow After Change

1. Presolution builds edge trace + initial witness steps.
2. `normalizeEdgeWitnessesM` rewrites by copy-map, canonicalizes, normalizes, validates, restores ids.
3. Persisted `ewSteps` + `ewWitness` are shape-correct and deterministic.
4. Φ translation consumes normalized steps with minimal interpretation.
5. Pipeline elaboration/typecheck produce expected polymorphic behavior for bug shapes.

## Error Handling and Observability

- Keep hard failures for malformed witness shape (`WitnessNormalizationError`, `PhiInvariantError`, `PhiTranslatabilityError`).
- Add targeted trace points in normalization (only where needed) to expose:
  - canonical binder-arg pair chosen for each relevant `OpWeaken` anchor,
  - whether reorder/coalesce changed relative order,
  - final normalized step sequence for affected edges.
- Prefer deterministic diagnostics in tests over long ad hoc runtime logs.

## Testing Strategy

### Sentinel policy (during implementation)
- Keep the 4-case sentinel matrix in `test/PipelineSpec.hs` as `pendingWith` while implementation is incomplete.

### Add/expand tests first (RED)
1. Presolution witness-shape tests (`test/Presolution/WitnessSpec.hs`):
   - canonical graft→weaken normalization behavior under copy-map rewriting,
   - idempotent normalization under binder/arg alignment,
   - malformed binder/arg ambiguity rejects early.
2. Φ translation tests (`test/ElaborationSpec.hs`):
   - graft/raise/weaken sequences from normalized witnesses preserve polymorphism where expected,
   - no `TBottom`-driven collapse for the targeted binder family.
3. Pipeline bug-target tests (`test/PipelineSpec.hs`):
   - convert pending matrix cases into explicit expected outcomes at finalization step.

### Regression gate
- Targeted during iteration:
  - BUG sentinel matrix block
  - H15 guard (`does not leak solved-node names in make let mismatch`)
  - witness/Φ focused specs
- Completion gate:
  - `cabal build all && cabal test`

## Risks and Mitigations

1. **Risk:** over-normalization changes unrelated witnesses.
   - **Mitigation:** add focused presolution tests for existing raise/merge behavior and enforce idempotence.
2. **Risk:** binder ordering regressions (`OpGraft: binder not found in quantifier spine`).
   - **Mitigation:** reuse existing order-key + binderArgs machinery; add explicit regression around non-front binder targeting.
3. **Risk:** temporary “fix” only shifts error shape.
   - **Mitigation:** require improved behavior on bug-target assertions plus no sentinel-regression gate before keeping any change.

## Exit Criteria

This design is complete when all are true:
1. The 4 known buggy shapes are no longer pending and pass as real correctness tests.
2. H15 non-leak regression remains green.
3. Witness normalization remains thesis-faithful, strict, and idempotent.
4. Full verification passes (`cabal build all && cabal test`).
