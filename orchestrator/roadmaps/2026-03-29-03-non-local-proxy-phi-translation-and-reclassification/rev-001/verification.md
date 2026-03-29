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

- Item 1 (Reclassify nested-forall μ absorption):
  - Verify test descriptions in `test/Research/P5ClearBoundarySpec.hs` are
    renamed to reflect correct polymorphic-mediation behavior.
  - Verify `implementation_notes.md` reclassifies this from "limitation" to
    "known correct behavior."
  - Verify test assertions remain unchanged (`containsMu == False`).
  - Verify no code changes beyond test descriptions and documentation.

- Item 2 (Fix reifyInst TyMu binder):
  - Verify `reifyInst` in `src/MLF/Reify/Type.hs` handles 0-binder TyMu
    nodes for non-local proxy wrappers without throwing
    `PhiTranslatabilityError`.
  - Verify existing local TyMu reification (1-binder path) is unchanged.
  - Verify new targeted test(s) validate non-local TyMu reification.
  - Run: `cabal test --test-option='-m' --test-option='non-local proxy'`

- Item 3 (Fix OpRaise non-spine context):
  - Verify `OpRaise` in `src/MLF/Elab/Phi/Omega/Interpret.hs` handles
    non-local bind-parent configurations.
  - If this item is marked done because item-2 resolved the upstream cause,
    verify the dependency explanation is documented.
  - Verify existing spine and local non-spine OpRaise behavior is unchanged.

- Item 4 (Upgrade pipeline test):
  - Verify `test/PipelineSpec.hs:2303` is upgraded from
    `expectStrictPipelineFailure` to a success assertion.
  - Verify the ElaborationSpec.hs survey is documented (which
    PhiTranslatabilityError tests are related vs independent).
  - Verify any related tests that now pass are upgraded.

- Item 5 (Documentation):
  - Verify `implementation_notes.md` removes the non-local proxy from
    "Known remaining limitations."
  - Verify `CHANGELOG.md` records the non-local proxy fix.
  - Verify `docs/thesis-deviations.yaml` is updated.
  - Verify no code changes (documentation only).

## Approval Criteria

- Every baseline check passes. No skipping the Cabal gate.
- Every task-specific check required by the selected item passes.
- `review.md` records commands, evidence, and explicit approve/reject.
- `selection.md` and `review-record.json` preserve the active `roadmap_id`,
  `roadmap_revision`, `roadmap_dir`, and `roadmap_item_id`.
- No existing test regressions. Zero failures in the full test suite.

## Reviewer Record Format

- Commands run with exact output
- Pass or fail result for each check
- Evidence summary
- Explicit APPROVED or REJECTED decision
