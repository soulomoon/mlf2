# Merge — Round 162

## Squash Commit

**Title**: Add 14 QuickCheck property tests across 4 domains (round-162, item-3)

**Summary**: Expand property-based testing with QuickCheck for the MLF2 codebase. Adds `test/Property/QuickCheckPropertySpec.hs` with 14 properties covering:
- Reification round-trip well-formedness (4 properties: substTypeCapture free-var preservation, alphaEqType reflexivity/symmetry, non-free substitution identity)
- Canonicalization idempotency (3 properties at 200 cases: idempotent, triple-apply stable, deterministic)
- Binding tree parent-child invariant preservation (4 properties: generator validity, parent existence, cycle-freedom, rootedConstraint preservation)
- Unification symmetry (3 properties: decomposeUnifyChildren symmetry, same-head success, different-head failure)

**Infrastructure changes**: `MLF.Constraint.Unify.Decompose` promoted to exposed-modules for test access. `RepoGuardSpec` whitelist updated.

**Test count**: 1274 → 1288 (+14)

## Roadmap Identity

- `roadmap_id`: `2026-03-30-01-codebase-quality-and-coverage-improvements`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001`
- `roadmap_item_id`: `item-3`

## Follow-Up

None. Item-3 is complete. Item-4 (Decompose large modules) can proceed next.
