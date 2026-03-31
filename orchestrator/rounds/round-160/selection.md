# Round 160 — Task Selection

## Selected Item

- Roadmap item: Add test coverage for untested core modules (MLF.Reify.*, MLF.Util.*)
- Item id: item-1
- Roadmap id: 2026-03-30-01-codebase-quality-and-coverage-improvements
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001

## Rationale

Item-1 is the lowest-numbered unfinished item with no dependencies. It is tier-1
parallel-safe but since max_parallel_rounds=1, it runs alone. Completing item-1
first establishes the test safety net that item-3 (QuickCheck expansion) and
item-4 (module decomposition) depend on. The deliverable is well-scoped: new
spec files for MLF.Reify.Type, MLF.Reify.Core, MLF.Reify.Named, MLF.Reify.TypeOps,
MLF.Util.Graph, and MLF.Util.UnionFind, wired into test/Main.hs and mlf2.cabal.
