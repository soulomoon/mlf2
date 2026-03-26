# Round 115 Plan (`item-3` Same-Pocket Post-Amendment Settlement Validation)

## Objective

Execute only roadmap item `3` and prepare one docs-only exact-pocket
validation artifact at:
`docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-validation.md`.

This is the initial `item-3` plan for `attempt-1` with `retry: null`. The
round must consume accepted rev-004 items `1` and `2` as authoritative
predecessor truth, remain rev-004 docs-first and exact-pocket-only, and
prove exactly this much, and no more:

- the new rev-004 settlement ledger stays aligned with the accepted
  `round-111` current same-pocket post-amendment read;
- the new ledger preserves the exact `C2` / `C5` / `C7` packet, tuple,
  anchor, owner-local frame, route, and clear-boundary-only status;
- the new ledger preserves predecessor-artifact immutability;
- the new ledger does not silently widen into broad same-family or repo-
  level success claims; and
- the validation remains documentary and leaves item `4` for later work.

Current planning read: accepted `round-114` already wrote the one frozen
ledger path and recorded the exact current same-pocket post-amendment read.
Item `3` must now compare that ledger directly against:

- the accepted `round-111` current-result anchor;
- the accepted `round-113` item-1 freeze; and
- the immutable predecessor-artifact set named in those accepted records.

The validation target is exact and narrow:

- helper-visible/internal fallback result remains
  `TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))`;
- helper-visible/internal recursive witness remains `containsMu True`;
- both authoritative public entrypoints remain
  `Right (TForall "a" Nothing (TArrow (TVar "t31") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))))`;
- the new ledger still records only one exact-pocket settlement surface; and
- the older pre-amendment dossiers remain untouched historical evidence.

This round is docs-only. It must preserve
`iso-recursive = keep`,
`non-equi-recursive = keep`, and
`no-fallback = keep`;
keep the accepted rev-003 item-3 validation authoritative as the one exact
current-result anchor;
keep the accepted rev-004 item-1 freeze authoritative as the boundary
contract;
keep the accepted rev-004 item-2 ledger authoritative as the only new
settlement surface under validation;
keep rev-001 items `6` through `8` blocked;
and keep code changes, new settlement writing, second interfaces,
multi-SCC search, fallback widening, production rollout, hardening, and
broad capability claims blocked. It must not edit source code, tests,
Cabal, or `orchestrator/state.json` in this round.

## Locked Round Context

- Round id: `round-115`
- Roadmap item: `item-3`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-115`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-115`
- Fixed live subject: one docs-only rev-004 item-3 exact-pocket validation
  of the bounded settlement ledger already published by accepted `round-114`
- Fixed inherited boundary:
  `explicit-only / iso-recursive / non-equi-recursive / no-fallback / one-interface / no-multi-SCC`

Current worktree state is already non-pristine at the control-plane level.
Respect existing edits and do not revert unrelated work:

- controller-owned `orchestrator/state.json` is modified and must remain
  untouched by validation work; and
- existing `orchestrator/rounds/round-115/selection.md` is a round input and
  must remain untouched.

Reviewer outcome constraints for this stage remain:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-115/selection.md`
  fixes this round to roadmap item `3` only, limits writable docs work to
  the exact frozen validation path, and keeps follow-on decision work and
  all inherited blocked work blocked.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/roadmap.md`
  makes item `3` the lowest-numbered unfinished item whose dependencies are
  now satisfied by accepted `round-114`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/retry-subloop.md`
  allows retry for item `3` and requires `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`
  on every review.
- `orchestrator/rounds/round-113/review-record.json`
  is the authoritative rev-004 item-1 acceptance record. It froze the exact
  current-result anchor, the exact writable ledger/validation boundary, and
  explicit predecessor immutability.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-surface-and-successor-boundary-freeze.md`
  is the authoritative rev-004 item-1 freeze and therefore the direct scope
  contract for this validation round.
- `orchestrator/rounds/round-114/review-record.json`
  is the authoritative rev-004 item-2 acceptance record. It makes the new
  ledger surface authoritative for validation.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`
  is the authoritative rev-004 item-2 ledger and therefore the direct
  subject under validation.
- `orchestrator/rounds/round-111/review-record.json`
  remains the authoritative controller-level acceptance record for the
  current exact-pocket post-amendment read that the ledger must match.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md`
  remains the decisive accepted current-result anchor.
- the same immutable predecessor settlement artifacts named in accepted
  `round-113` remain immutable historical evidence only.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the inherited boundary contract:
  explicit-only production baseline,
  iso-recursive only,
  non-equi-recursive,
  no fallback widening,
  no second interface,
  and no equi-recursive or implicit-unfolding authorization.

## File Map

### Create

- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-validation.md`
  - Responsibility: canonical docs-only rev-004 item-3 validation artifact
    for the exact same-pocket settlement ledger only. It must compare the
    new ledger against the accepted `round-111` current-result anchor,
    verify predecessor immutability, and prove the ledger stayed
    exact-pocket-only.

### Read-Only Evidence

- `orchestrator/rounds/round-115/selection.md`
- `orchestrator/rounds/round-114/review-record.json`
- `orchestrator/rounds/round-113/review-record.json`
- `orchestrator/rounds/round-111/review-record.json`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/verification.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-surface-and-successor-boundary-freeze.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md`
- the immutable predecessor settlement artifacts named in accepted `round-113`

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/rounds/round-115/selection.md`
- roadmap files under
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/`
- `Bugs.md`
- `TODO.md`
- `implementation_notes.md`
- `CHANGELOG.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- the rev-004 item-2 ledger file
- every historical predecessor settlement artifact

No round-local `implementation-notes.md`, alternate settlement memo, or
supplemental roadmap note is authorized. The docs artifact above is the only
intended implementation output path.

## Validation Questions

The canonical artifact must answer exactly these bounded questions:

1. Does the new rev-004 settlement ledger reproduce the exact accepted
   current same-pocket post-amendment read from `round-111`?
2. Does the new ledger preserve the exact-pocket-only boundary frozen by
   accepted `round-113`?
3. Does the new ledger preserve predecessor immutability and avoid broad
   same-family or repo-level claims?

The artifact must stay documentary. It must not relitigate the accepted
`round-111` result itself and must not decide the later handoff.

## Review Checklist

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- prove the round-owned diff is docs/orchestrator-only apart from the
  controller-owned `orchestrator/state.json`
- prove the canonical artifact validates the ledger against the accepted
  current-result anchor field-by-field
- prove predecessor immutability and exact-pocket-only language are
  preserved
- prove no round-local `implementation-notes.md` was created
- because this round is docs-only, record the broader Cabal gate as
  intentionally out of scope

## Finish Condition

The round is ready for review when:

- the canonical item-3 validation artifact exists at the exact path above;
- it proves the new ledger stays aligned with the accepted `round-111`
  current-result anchor;
- it proves predecessor immutability and exact-pocket-only language are
  preserved; and
- no file outside the allowed docs/orchestrator packet has changed.
