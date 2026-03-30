# Split thesis conformance workflow into fast gate and full gate

## Summary

This round updates `.github/workflows/thesis-conformance.yml` to replace the monolithic job with two matrix-parameterized jobs: a fast `build-and-test` lane (`cabal build all && cabal test`) and a dependent `thesis-conformance` lane (`./scripts/thesis-conformance-gate.sh`). The matrix is intentionally bounded to `ubuntu-latest` and GHC `9.12.2`, so the workflow is honest about current support while remaining easy to extend later.

## Roadmap Metadata

- roadmap_id: `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001`
- roadmap_item_id: `item-2`

## Follow-up Notes

- item-3 is a known blocker: absolute paths in `docs/thesis-obligations.yaml` still need repair before the CI thesis-conformance gate can be fully green on runners.

## Merge Readiness

The round is approved, base branch freshness is confirmed, and the approved change is ready for squash merge.
