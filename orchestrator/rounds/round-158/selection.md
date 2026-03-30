roadmap_id: 2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap
roadmap_revision: rev-001
roadmap_dir: orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001
roadmap_item_id: item-3

selected_item: Fix any matrix-exposed runner or test failures at the root cause
rationale: Item-3 is the lowest-numbered unfinished item whose dependency, item-2, is already done. The matrix is in place, so this round should focus on any failures it exposes rather than expanding scope.

current_baseline:
- `cabal build all && cabal test`: PASS (`1177 examples, 0 failures`)
- `./scripts/thesis-conformance-gate.sh`: PASS locally

known_blockers_context:
- `docs/thesis-obligations.yaml` contains 262 absolute `/Volumes/src/mlf4/...` entries in `code_anchors` and `test_anchor.file` fields.
- Those paths do not exist on CI runners such as `ubuntu-latest`, so `scripts/check-thesis-obligations-ledger.sh` can fail there via Ruby `File.exist?` checks.
- `docs/thesis-claims.yaml` may contain the same class of absolute-path `code_path` entries and should be treated as part of the same root-cause risk.

round_deliverable:
- Reproduce and fix any matrix-exposed runner or test failures at the source.
- If no new failure is exposed, record that the selected item is satisfied with explicit evidence.
- Keep the work bounded to the existing CI matrix and the absolute-path thesis ledger risk; do not write an implementation plan here.
