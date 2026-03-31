# Round 168 — Task Selection

**Selected item**: item-9
**Item title**: {- Note -} block audit and documentation sync
**Roadmap identity**:
- roadmap_id: 2026-03-30-01-codebase-quality-and-coverage-improvements
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001
- roadmap_item_id: item-9

## Why now

item-9 is the last remaining item on the roadmap. All dependencies are satisfied:
- item-4 (module decomposition) merged in round-163
- item-8 (public API enrichment) merged in round-167

## Current baseline

- Base branch: codex/automatic-recursive-type-inference at commit dc2616a
- Test count: 1302 examples, 0 failures
- All prior items (1-8) complete

## Scope

Audit all `{- Note [...] -}` blocks across `src/` for:
- Stale function/type name references after item-4 splits
- Missing Notes for newly-created submodules from item-4
- Alignment with current `implementation_notes.md`
- Fix stale references, add Notes to new modules
- Update `implementation_notes.md` if needed
- Update `CHANGELOG.md` with documentation-hygiene entry
