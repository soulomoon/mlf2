# Round 150 Plan — Documentation Correction

## Goal

Update all project documentation surfaces to accurately describe the expanded
iso-recursive type inference scope after the gap-fix campaign (rounds 146-149).

## Context

Rounds 146-149 fixed four specific gaps in automatic iso-recursive type inference:

1. **Round 146**: Phase 4 witness normalization now handles `TyMu` nodes
2. **Round 147**: Phase 6 alias-bounds resolution now works with recursive types
3. **Round 148**: ELet fixpoint reduction now handles recursive let-bindings
4. **Round 149**: Result-type fallback now opens μ-types for non-local reconstruction

The pre-existing documentation (written in round-144) claimed completeness
prematurely. This round corrects those claims.

## Known remaining limitations (must be documented)

- Nested-forall-mediated recursive types: μ absorbed during constraint solving
  through polymorphic mediation; `containsMu fallbackTy == False` but this is
  correct behavior (constraint solving legitimately removes the μ wrapper).
- Non-local proxy at pipeline entrypoints: fallback now returns μ-type
  correctly, but pipeline still fails with `PhiTranslatabilityError` at
  elaboration level (separate issue from result-type fallback).

## Files to update

All changes are in the main worktree at `orchestrator/worktrees/round-150/`.

### 1. `implementation_notes.md` (lines 1-28)

Update the top entry (2026-03-29) to:
- Mention the gap-fix campaign (rounds 146-149) that addressed witness
  normalization, alias-bounds, ELet fixpoint, and result-type fallback gaps
- Update test count from 1168+ to 1175 (current count)
- Add the known remaining limitations
- List the specific families now supported: simple self-recursion, nested
  recursive lets, μ/∀ interaction, higher-order recursion, non-local
  recursive result types (with noted limitation)

### 2. `CHANGELOG.md` (lines 5-12)

Update the existing "Unreleased" entry on automatic iso-recursive inference to:
- Mention the gap-fix campaign addressing 4 specific gaps
- Update test count to 1175
- Note the expanded scope of recursive families now supported

### 3. `TODO.md` (lines 7-27)

Update Task 105 to:
- Mark as fully completed (not just "item-3 readiness gate" pending)
- Record the gap-fix campaign completion (items 1-5 of the
  `2026-03-29-02-iso-recursive-inference-gap-fixes` roadmap)
- Note the documentation correction as the final step

### 4. `roadmap.md` (line 154)

Update the Phase 7 status paragraph to mention the gap-fix campaign and the
expanded recursive type families now supported.

### 5. `docs/thesis-deviations.yaml` (lines 91-128)

Update DEV-AUTO-ISO-RECURSIVE to:
- Expand the description to mention the gap-fix campaign
- Add the new code paths modified in rounds 146-149
- Update test evidence to include the new test matchers
- Note the known remaining limitations

## Verification

- `cabal build all && cabal test` must pass (no code changes, but verify
  the build is still green)
- All documentation changes must be factually accurate against the actual
  test results and code state

## Success criteria

Documentation accurately describes:
1. Which recursive type families are supported end-to-end
2. What gaps were fixed and when
3. What known limitations remain
4. Current test count (1175 examples, 0 failures)
