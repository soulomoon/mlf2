# Findings

## Initial
- Execution branch: `codex/phi-thesis-purity-followup-2026-02-23`
- Source plan imported to: `tasks/todo/2026-02-23-phi-thesis-purity-followup/plan_source.md`
- `docs/thesis-deviations.yaml` currently has 4 entries; checker enforces no orphan deviations and no open status.
- `CLM-PHI-CORRECTNESS` in `docs/thesis-claims.yaml` currently has `deviations: []`; must be updated when new deviations are added.

## During Task 5
- Removing the `mergeIntoApp` peephole exposed a behavior-sensitive de-fused sequence:
  - `OpGraft; OpRaise; OpWeaken` can produce a left-associated instantiation chain where final `InstElim` substitutes `⊥` if the fresh bound cannot be represented as a `BoundType`.
  - Observable regression: `\y. let id = (\x. x) in id y` inferred `∀a. a -> ⊥`.
- Root cause detail:
  - `BoundType` forbids top-level `TVar`, so some `InstInside (InstBot (TVar ...))` updates are not persisted as explicit bounds.
  - In de-fused `Raise+Weaken`, this can make a trailing elimination substitute `⊥` instead of the intended graft argument.
- Practical mitigation used here:
  - Keep operation replay de-fused, but extend `normalizeInst` to canonicalize the affected left-associated shape back to `InstApp τ` when prefix/app-arg forms agree (`InstInside (InstBot τ)` or `InstApp τ`).
  - This restores observed elaboration behavior for historical regression baselines while keeping `mergeIntoApp` removed from the operation loop.

## Final status
- Task goals achieved:
  - deviation registration and claim linking complete;
  - `mergeIntoApp` removed from the operation replay loop;
  - bottom rescue moved into reification;
  - bounded bound-match emits `InstApp boundTy` in Φ.
- Validation evidence:
  - focused regression slices green (`id y`, `polymorphic instantiation`, `bounded bound-match`, `BUG-002-V2`, full Phase 6 slice),
  - full suite green (`778 examples`),
  - thesis claims/deviation checker green.
