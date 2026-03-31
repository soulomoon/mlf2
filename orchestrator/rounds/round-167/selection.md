# Round 167 — Task Selection

## Metadata

- roadmap_id: 2026-03-30-01-codebase-quality-and-coverage-improvements
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001
- roadmap_item_id: item-8
- round_id: round-167

## Selected Item

**item-8: Public API enrichment (error reporting + pipeline configuration)**

### Why This Item

item-8 is the lowest-numbered unfinished roadmap item. All of its dependencies are satisfied:
- Depends on: item-4 (done — merged in round-163)
- Merge after: item-6 (done — merged in round-165)

Items 1–7 are all complete. item-9 depends on item-8 (merge-after), so item-8 is the correct next step.

### Deliverable Summary

Extend `src-public/MLF/Pipeline.hs` with:
- Structured error formatting/reporting (`formatPipelineError :: PipelineError -> Text`)
- Pipeline configuration options (`PipelineConfig` with trace verbosity and optional phase selection)

Extend `src-public/MLF/API.hs` with constraint graph introspection helpers if useful types exist in the internal API. Add doc-comments for all new exports. Add tests for new public API functions.

### Verification Gate

`cabal build all && cabal test` passes. New exports documented with Haddock. At least one test per new exported function.

## Repo Baseline

- Test examples: 1296
- Test failures: 0
- Prior items complete: 7 of 9 (item-1 through item-7)
- Remaining items: item-8 (this round), item-9
