# Selection: Milestone 1 — NodeRef GADT and RefTag kind

Roadmap family: `2026-05-05-00-type-level-safety-singletons-roadmap`
Revision: `rev-001`
Milestone: `milestone-1`
Direction: `direction-1a-noderefgadt-reftag-kind`

## Why This Should Run Now

This is the first milestone. NodeRef GADT is the foundation for all later
milestones — the phase-indexed Constraint (milestone 3) uses `NodeRef
'TypeTag` in its field types, and Vec-indexed ForallSpec (milestone 4)
indexes over typed node references.

The change is well-scoped: `NodeRef` is defined in one file, consumed in
~15 files, and the GADT conversion is mechanical. No new dependencies are
needed — `DataKinds` and `GADTs` are already used across the codebase.

## Scope Boundaries

- Stay inside `milestone-1` / `direction-1a-noderefgadt-reftag-kind`.
- The `NodeRef` GADT and `RefTag` kind are the only type changes.
- `BindParents` container restructuring is in scope (required by the
  GADT split) but should be minimal — prefer `SomeNodeRef` existential
  over a full heterogeneous map rewrite.
- Do not add `singletons` dependency yet (milestone 2).
- Do not phase-index `Constraint` yet (milestone 3).
- Every file touched must build and test. No partial migrations.

## Key Files

- `src/MLF/Constraint/Types/Graph/NodeEdge.hs` — NodeRef definition
- `src/MLF/Util/IntMapUtils.hs` — ~17 NodeRef filter sites
- `src/MLF/Binding/GraphOps.hs` — 3 expectTypeRef calls
- `src/MLF/Reify/Type/Core.hs` — ~12 NodeRef match arms
- `src/MLF/Binding/Tree.hs` — BindParents operations
- `src/MLF/Binding/Queries.hs` — binding path traversal

## Completion Signal

- `expectTypeRef` and `expectGenRef` are deleted from the codebase.
- All NodeRef consumers use the GADT type to enforce node kind.
- `cabal build all && cabal test` passes (2524+ tests).
