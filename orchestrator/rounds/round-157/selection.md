# Round 157 Selection

## Identity

- roadmap_id: `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001`
- roadmap_item_id: `item-2`

## Selected Item

Selected `item-2`: add a bounded GitHub Actions build-and-test matrix around the repaired baseline.

Why: it is the lowest-numbered unfinished item, and its dependency on `item-1` is satisfied.

## Current Baseline

- Both local gates are green.
- Baseline test result: `1176 examples, 0 failures`.

## Current CI

- Single workflow: `.github/workflows/thesis-conformance.yml`
- Current lane: `ubuntu-latest` with GHC `9.12.2`

## Matrix Scope

- Keep the bounded matrix from item-1 selection.
- Start with a single `ubuntu-latest` / GHC `9.12.2` lane.
- Windows remains explicitly excluded.

## This Round Should Deliver

- Update the GitHub Actions workflow to express the bounded CI matrix around the repaired baseline.
- Reuse existing repo commands; do not add CI-only verification logic.
- Keep the thesis-conformance gate present as a dedicated job or an explicitly justified equivalent.
