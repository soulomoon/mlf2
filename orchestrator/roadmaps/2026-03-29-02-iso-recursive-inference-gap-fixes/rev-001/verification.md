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

- Item 1 (Phase 4 witness normalization):
  - Verify `normalizeInstanceOpsCore` and `validateNormalizedWitness` handle
    TyMu nodes without error.
  - Verify the nested recursive lets test (`PipelineSpec.hs:1350`) is upgraded
    from expected Phase 4 failure to full pipeline success.
  - Verify no existing witness normalization tests regressed.
  - Run the specific test group: `cabal test --test-option='-m' --test-option='nested recursive lets'`

- Item 2 (Phase 6 alias-bounds):
  - Verify `simplifySchemeBindings` / `substBound` handles TyMu cycles.
  - Verify μ/∀ interaction test (`PipelineSpec.hs:1396`) is upgraded to success.
  - Verify higher-order recursion test (`PipelineSpec.hs:1419`) is upgraded
    to success.
  - Verify `Finalize.hs` no longer hard-stops on recursive alias bounds.
  - Run the specific test groups: `cabal test --test-option='-m' --test-option='mu-forall interaction'` and `cabal test --test-option='-m' --test-option='higher-order recursion'`

- Item 3 (ELet reduction):
  - Verify recursive `ELet` reduction uses fixpoint unfolding.
  - Verify the recursive ELet test (`PipelineSpec.hs:1477`) is upgraded from
    tolerating `TCUnboundVar` to clean success.
  - Verify `step`/`normalize` produce no unbound variables for recursive lets.
  - Verify non-recursive `ELet` behavior is unchanged.

- Item 4 (Result-type reconstruction):
  - Verify `Fallback.hs` returns μ-types for non-local recursive positions.
  - Verify fail-closed tests (`PipelineSpec.hs:2104`, `:2235`, `:2306`) are
    upgraded to assert recursive type preservation.
  - Verify local-type fallback for non-recursive types is unchanged.

- Item 5 (Documentation):
  - Verify `implementation_notes.md`, `roadmap.md`, `TODO.md`, `CHANGELOG.md`
    accurately reflect the expanded iso-recursive scope.
  - Verify `docs/thesis-deviations.yaml` is updated if new deviations exist.
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
