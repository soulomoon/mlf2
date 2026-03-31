# Selection — Round 162

## Selected Item

- **Item id**: item-3
- **Title**: Expand property-based testing (QuickCheck)
- **Depends on**: item-1 (completed in round-160)
- **Status**: [pending] → [in-progress]

## Roadmap Identity

- `roadmap_id`: `2026-03-30-01-codebase-quality-and-coverage-improvements`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001`
- `roadmap_item_id`: `item-3`

## Rationale

Item-3 is the next eligible item. Its dependency (item-1) was completed in round-160. Item-2 is now also complete (round-161). Item-3 adds QuickCheck property tests for reification round-trip, canonicalization idempotency, binding tree invariants, and unification symmetry. This builds the safety net needed before the structural refactors in items 4-9.

## Branch

- `orchestrator/round-162-quickcheck-properties`

## Worktree

- `orchestrator/worktrees/round-162`
