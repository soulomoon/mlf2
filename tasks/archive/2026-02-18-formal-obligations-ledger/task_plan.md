# Task Plan: Formal Obligations Ledger (Ch. 14/15)

## Goal
Create a canonical, machine-enforced rule-to-code-to-test obligations ledger for thesis Chapters 14.2-14.3 and 15.2-15.3, integrated into the thesis conformance gate.

## Phases
- [completed] Phase 1: Initialize planning artifacts and baseline context
- [completed] Phase 2: Implement canonical YAML ledger + renderer
- [completed] Phase 3: Add checker script and gate integration
- [completed] Phase 4: Add/adjust test anchors with stable O* rule IDs
- [completed] Phase 5: Sync docs/trackers (paper-map, implementation_notes, TODO, CHANGELOG)
- [completed] Phase 6: Verify (`cabal build all && cabal test`, conformance gate)

## Decisions
- Scope fixed to Chapter 14 (14.2-14.3) and Chapter 15 (15.2-15.3) operational obligations.
- Source of truth is YAML ledger; Markdown is generated.
- Hard-fail enforcement is immediate.
- Reuse existing R-* tests where applicable; O15-TR obligations mapped to them via stable IDs/matchers.

## Errors Encountered
- Ruby compatibility in checker (`Array#tally` unavailable in local Ruby): replaced with explicit hash counting.
- Renderer `--check` mode wrote output files; updated to side-effect-free check mode.
- `TypeCheckSpec` compile failure: invalid `BoundType` construction (`TVar` at top level) replaced with a valid bound containing free-variable mention.
- `O15-TRANS-NON-INTERIOR-RIGID` fixture initially masked by `rootedConstraint` rewriting `cGenNodes`; switched to explicit `emptyConstraint` setup for deterministic translatability condition coverage.
- `O14-INST-*` grouped anchor originally asserted invalid `InstUnder/InstAbstr` behavior; replaced with a valid bounded outer/hyp witness path.

## Closure
- Archived to `/Volumes/src/mlf4/tasks/archive/2026-02-18-formal-obligations-ledger/` after all phases completed and full verification passed.
