# Automatic Iso-Recursive Type Inference — Readiness Summary

## Campaign
- Roadmap: `2026-03-29-01-automatic-iso-recursive-type-inference-completion` / `rev-001`
- Rounds: 139–145
- Predecessor family: `2026-03-29-00-automatic-iso-recursive-type-inference-implementation` (rounds 139–142)

## Build Gate
- Command: `cabal build all && cabal test`
- Result: **PASS** — 1175 examples, 0 failures
- GHC: 9.12.2, build profile `-O1`

## Capability Summary

The following automatic iso-recursive type inference capabilities have been
implemented and validated across rounds 139–144:

- **Cycle detection and automatic `TyMu` introduction** — The acyclicity
  module (`MLF.Constraint.Acyclicity.breakCyclesAndCheckAcyclicity`) detects
  cycles in the constraint graph and automatically introduces `TyMu` nodes,
  enabling iso-recursive types for unannotated recursive definitions.
- **Reification producing `TMu` types** — The reification pipeline correctly
  produces `TMu` types from `TyMu` constraint nodes, with proper binder
  naming and scope handling.
- **Elaboration emitting `ERoll`/`EUnroll` coercions** — The elaboration
  phase emits explicit `ERoll` and `EUnroll` coercions for iso-recursive
  types, matching the expected xMLF term structure.
- **Phase 7 type checking acceptance** — The Phase 7 type checker accepts
  elaborated terms containing `ERoll`/`EUnroll` coercions and `TMu` types.
- **Phase 7 reduction (roll/unroll)** — The `step`/`normalize` reduction
  engine handles roll/unroll reduction steps for auto-inferred recursive
  terms, validated end-to-end in round-143.
- **Documentation and changelog recorded** — `implementation_notes.md`,
  `roadmap.md`, `TODO.md`, `CHANGELOG.md`, and `docs/thesis-deviations.yaml`
  all record automatic iso-recursive type inference as a completed capability
  (round-144).

## Prerequisite Items

| Item | Status |
|------|--------|
| item-1: End-to-end validation (Phase 7 reduction) | done (round-143) |
| item-2: Documentation update | done (round-144) |
| item-3: Final readiness gate | done (this round, round-145) |

## Open Bugs

- **BUG-2026-03-16-001** (URI-R2-C1 replay path): Open, high priority.
  The `InstBot` branch in `MLF.Elab.Inst.applyInstantiation` rejects
  non-bottom shapes carried by the bounded no-fallback replay path for
  `URI-R2-C1`. This bug is **unrelated to iso-recursive inference** — it
  concerns the provenance-preservation replay path for a specific
  research-track use case. It does not block readiness for automatic
  iso-recursive type inference.

No other open bugs exist in `Bugs.md`.

## Readiness Declaration

**Automatic iso-recursive type inference is production-ready.**

The full inference pipeline — cycle detection → `TyMu` introduction →
reification → `ERoll`/`EUnroll` elaboration → Phase 7 type checking →
Phase 7 reduction — works end-to-end for unannotated recursive definitions.
All 1175 tests pass with zero failures. Non-recursive programs continue to
produce identical results. The capability is documented as a thesis extension
(the core thesis assumes acyclic constraint graphs; this extension handles
cyclic graphs gracefully).

## Controller Post-Merge Actions

- Clean up orchestrator worktrees for this campaign
- Update `orchestrator/state.json` to reflect terminal completion
- Update roadmap item-3 status from `[pending]` to `[done]`
