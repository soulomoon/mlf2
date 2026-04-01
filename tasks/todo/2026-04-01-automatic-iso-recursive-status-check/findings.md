# Findings

## Sweep 1

- `CHANGELOG.md`, `TODO.md`, and the top section of `implementation_notes.md`
  all state that automatic iso-recursive inference was completed on
  2026-03-29.
- Older strategic docs such as
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  still describe the honest repo-level baseline as explicit-only and say the
  broader repo-level claim remained unresolved at that point.
- The dedicated review packets under
  `tasks/todo/2026-03-29-automatic-iso-recursion-review/` and
  `tasks/todo/2026-03-29-remaining-iso-recursion-issues-review/` are the most
  directly relevant follow-up evidence. They show the implementation path is
  real, but they also record then-live fail-closed expectations that must be
  checked against current code.

## Sweep 2

- Current production code contains the full positive path:
  `MLF.Constraint.Acyclicity.breakCyclesAndCheckAcyclicity` introduces `TyMu`,
  reification turns `TyMu` into `TMu`, elaboration emits `ERoll`/`EUnroll`,
  and Phase 7 typechecking/reduction handle those forms.
- Current tests still encode some negative recursive expectations, but the
  names have shifted since the 2026-03-29 review:
  - nested recursive lets now fail at Phase 7 type checking, not Phase 4
    witness normalization;
  - the μ/∀ interaction case no longer expects alias-bounds failure text, but
    still expects current Phase 6 fail-closed behavior;
  - non-local proxy fallback now expects recursive result-type reconstruction
    to stay open while some pipeline/elaboration entrypoints still expect
    `PhiTranslatabilityError`.
- This means the current answer likely depends on scope:
  the core automatic iso-recursive inference mechanism exists, but broad
  repo-level completeness may still be blocked by remaining representative
  fail-closed families.

## Sweep 3

- `implementation_notes.md` and `TODO.md` now make a strong local claim that
  automatic iso-recursive inference is implemented end-to-end and that no
  further work is needed for the March 29 gap-fix campaign.
- That claim does not supersede the separate repo-level readiness lane:
  `TODO.md` Task 104 is still in progress and explicitly says broader
  readiness remains open, with representative gaps across `P3` / `P4` / `P6`
  and nested-`forall` still fail-closed.
- The accepted strategic decision doc from 2026-03-25 is explicit that broad
  general automatic iso-recursive inference was not yet justified:
  zero rows had `stable visible persistence`, positive `P5` success was not
  accepted, and the lawful outcome remained `continue within the current
  architecture` rather than a general capability claim.
- The 2026-03-29 settlement doc keeps one exact packet
  `sameLaneAliasFrameClearBoundaryExpr` as a current-architecture blocker and
  says the broader `P3` / `P4` / `P6` family remains unresolved.

## Live Verification

- Fresh focused tests under GHC 9.14.1 passed:
  - `--match "self-recursive function infers"`: `1 example, 0 failures`
  - `--match "P5 clear-boundary retained-child probes"`: `4 examples, 0 failures`
  - `--match "non-local proxy wrapper"`: `2 examples, 0 failures`
  - `--match "sameLaneAliasFrameClearBoundaryExpr"`: `2 examples, 0 failures`
  - `--match "C1 authoritative-surface harness"`: `2 examples, 0 failures`
- Those passes confirm the current intended state:
  supported positive cases work, but the suite still deliberately enforces
  unresolved representative-gap blockers and reject-side boundaries as current
  truth.

## Conclusion

- If “complete” means the concrete mechanism for automatic μ introduction and
  end-to-end iso-recursive handling exists in production code, the answer is
  yes.
- If “complete” means the repo can honestly claim broad/general automatic
  iso-recursive inference across the intended representative family matrix
  with no remaining readiness blockers, the answer is no.
- The strongest honest current status is:
  implemented for the bounded supported path, but not closed as a general
  repo-level capability claim.
