# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: controller state is machine-owned and must remain valid JSON.
- Command: `roadmap_dir="$(python3 -c "import json; print(json.load(open('orchestrator/state.json'))['roadmap_dir'])")" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  Why: the live controller must resolve one authoritative roadmap bundle.
- Command: `cabal build all && cabal test`
  Why: the standard local build/test gate remains authoritative for this repo.
- Command: `./scripts/thesis-conformance-gate.sh`
  Why: the matrix campaign must not proceed while the existing thesis gate is
  red.

## Task-Specific Checks

- Item 1 (Freeze scope + repair baseline):
  - Verify the round records the exact selected matrix scope and any explicit
    excluded lanes.
  - Verify the thesis gate failure was fixed at the source of truth rather than
    suppressed.
  - Verify no matrix-widening workflow changes landed yet beyond minimal
    baseline support.

- Item 2 (Add bounded matrix):
  - Verify `.github/workflows/` implements the selected matrix scope from item
    `1`.
  - Verify the workflow reuses repo commands such as `cabal build all`,
    `cabal test`, and `./scripts/thesis-conformance-gate.sh`.
  - Run:
    `rg -n "strategy:|matrix:|runs-on:|ghc-version:|cabal build all|cabal test|thesis-conformance-gate.sh" .github/workflows/*.yml`

- Item 3 (Fix matrix-exposed failures):
  - Verify every claimed fix has a bounded reproducer or focused regression
    check.
  - Verify no CI-only bypass or compatibility fallback was introduced.
  - If the item is marked done with no implementation changes, verify the
    evidence shows the matrix landed green without exposing a new failure.

- Item 4 (Docs + handoff):
  - Verify `README.md`, `TODO.md`, and `CHANGELOG.md` document the new matrix
    and any remaining runner limits.
  - Verify reviewer evidence records which commands remain the authoritative
    local gates.

## Approval Criteria

- Every baseline check passes.
- Every task-specific check required by the selected item passes.
- `selection.md` and `review-record.json` preserve `roadmap_id`,
  `roadmap_revision`, `roadmap_dir`, and `roadmap_item_id`.
- `review.md` records commands, evidence, and an explicit approve/reject
  decision.
- No unresolved blocker remains hidden behind a skipped lane or suppressed
  command.

## Reviewer Record Format

- Commands run
- Pass or fail result
- Evidence summary
- Explicit APPROVED or REJECTED decision
