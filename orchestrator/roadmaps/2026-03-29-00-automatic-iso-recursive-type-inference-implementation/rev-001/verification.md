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
  Why: every implementation round must pass the full repo gate. No exceptions
  for this implementation-focused roadmap.

## Task-Specific Checks

- Item 1: verify that non-recursive test programs still produce identical
  results. Run the existing test suite to confirm zero regressions. Verify
  that a simple recursive expression (e.g., `let f = λx. f x in f`) no
  longer fails at the acyclicity check.
- Item 2: verify focused `PipelineSpec` tests for automatic recursive
  inference exist and pass. Verify `runPipelineElab` and
  `runPipelineElabChecked` handle recursive definitions.
- Item 3: verify elaborated output contains `TMu` types and `ERoll`/`EUnroll`
  terms. Verify Phase 7 type checker accepts elaborated recursive terms.
- Item 4: verify edge-case tests exist and pass for nested recursion,
  polymorphic recursion, and μ/∀ interaction.
- Item 5: verify integration tests in `TypeSoundnessSpec.hs` for recursive
  types. Verify full pipeline including reduction works.
- Item 6: verify documentation updates are accurate and complete.

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
