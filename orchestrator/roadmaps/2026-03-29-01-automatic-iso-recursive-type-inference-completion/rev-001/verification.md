# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON.
- Command: `roadmap_dir="$(python3 -c "import json; print(json.load(open('orchestrator/state.json'))['roadmap_dir'])")" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  Why: the live control plane must resolve one authoritative roadmap bundle.
- Command: `cabal build all && cabal test`
  Why: every implementation round must pass the full repo gate. No exceptions.

## Task-Specific Checks

- Item 1: verify integration tests for recursive-type reduction exist and pass.
  Verify `step`/`normalize` can reduce recursive applications including
  roll/unroll reduction. Verify `runPipelineElabChecked` succeeds for recursive
  definitions. Verify non-recursive programs still produce identical results.
- Item 2: verify `implementation_notes.md`, `roadmap.md`, `TODO.md`, and
  `CHANGELOG.md` all record iso-recursive inference as a completed capability.
  Verify `docs/thesis-deviations.yaml` exists or is updated if applicable.
  Verify no code changes beyond documentation.
- Item 3: verify orchestrator worktrees are cleaned up, `state.json` reflects
  terminal completion, and the base branch passes the full gate. Verify a
  readiness summary exists.

## Approval Criteria

- Every baseline check passes. No skipping the Cabal gate.
- Every task-specific check required by the selected item passes.
- `review.md` records commands, evidence, and explicit approve/reject.
- `selection.md` and `review-record.json` preserve the active `roadmap_id`,
  `roadmap_revision`, and `roadmap_dir`.
- No existing test regressions. Zero failures in the full test suite.

## Reviewer Record Format

- Commands run with exact output
- Pass or fail result for each check
- Evidence summary
- Explicit APPROVED or REJECTED decision
