# Round 166 — Task Selection

## Metadata

- roadmap_id: 2026-03-30-01-codebase-quality-and-coverage-improvements
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001
- roadmap_item_id: item-7
- round_id: round-166

## Selected Item

**item-7: Golden test expansion**

## Selection Rationale

item-7 is the lowest-numbered unfinished item in the roadmap. It has no dependencies (`Depends on: none`) and is marked parallel-safe, so nothing blocks its execution. Items 1–6 are all complete; item-7 is the natural next step.

## Current Repo Baseline

- Test examples: 1288
- Test failures: 0
- Prior items complete: 6 of 9 (item-1 through item-6)
- Branch: `codex/automatic-recursive-type-inference`

## Item Summary

Add golden tests for:
- Pretty-printed xMLF output for at least 5 canonical pipeline examples (identity, church booleans, polymorphic let, rank-2 application, choose)
- Constraint graph summary dumps for at least 3 examples
- Store golden files under `test/golden/`
- Wire into the test suite with `--accept` workflow documented
