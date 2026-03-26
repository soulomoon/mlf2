# Round 114 Plan (`item-2` Bounded Same-Pocket Post-Amendment Settlement Ledger)

## Objective

Execute only roadmap item `2` and prepare one docs-only exact-pocket
settlement artifact at:
`docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`.

This is the initial `item-2` plan for `attempt-1` with `retry: null`. The
round must consume accepted rev-004 item `1` as authoritative predecessor
truth, remain rev-004 docs-first and exact-pocket-only, and record exactly
this much, and no more:

- the same selected rows `C2`, `C5`, and `C7`;
- the same exact same-pocket boundary, exact packet, and exact tuple;
- the same exact current-result surface anchored in accepted `round-111` /
  rev-003 item `3`;
- one new bounded settlement ledger surface at the exact path frozen by
  accepted `round-113`; and
- the relation between that new ledger and the immutable pre-amendment
  predecessor artifacts, without silently rewriting any of them.

Current planning read: accepted `round-113` already froze the one exact
future writable docs boundary for rev-004 items `2` and `3`. Item `2` must
now use only the first of those two frozen docs paths to record the current
exact-pocket post-amendment read:

- helper-visible/internal fallback result remains
  `TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))`;
- helper-visible/internal recursive witness remains `containsMu True`; and
- both authoritative public entrypoints now return
  `Right (TForall "a" Nothing (TArrow (TVar "t31") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))))`
  on the same exact packet.

This round must record that exact current read on the new bounded settlement
surface only. It must not validate the new surface yet, must not change the
old dossiers in place, and must not narrate broad same-family or repo-level
success.

This round is docs-only. It must preserve
`iso-recursive = keep`,
`non-equi-recursive = keep`, and
`no-fallback = keep`;
keep the accepted rev-003 item-3 validation authoritative as the one exact
current-result anchor;
keep the accepted rev-004 item-1 freeze authoritative as the writable
boundary;
keep rev-001 items `6` through `8` blocked;
and keep code changes, validation, second interfaces, multi-SCC search,
fallback widening, production rollout, hardening, and broad capability
claims blocked. It must not edit source code, tests, Cabal, or
`orchestrator/state.json` in this round.

## Locked Round Context

- Round id: `round-114`
- Roadmap item: `item-2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-114`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-114`
- Fixed live subject: one docs-only rev-004 item-2 bounded settlement
  ledger for the exact same-pocket post-amendment read already frozen by
  accepted `round-113`
- Fixed inherited boundary:
  `explicit-only / iso-recursive / non-equi-recursive / no-fallback / one-interface / no-multi-SCC`

Current worktree state is already non-pristine at the control-plane level.
Respect existing edits and do not revert unrelated work:

- controller-owned `orchestrator/state.json` is modified and must remain
  untouched by settlement-ledger work; and
- existing `orchestrator/rounds/round-114/selection.md` is a round input and
  must remain untouched.

Reviewer outcome constraints for this stage remain:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-114/selection.md`
  fixes this round to roadmap item `2` only, limits writable docs work to
  the exact frozen settlement-ledger path, and keeps validation, follow-on
  decision work, and all inherited blocked work blocked.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/roadmap.md`
  makes item `2` the lowest-numbered unfinished item whose dependencies are
  now satisfied by accepted `round-113`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/retry-subloop.md`
  allows retry for item `2` and requires `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`
  on every review.
- `orchestrator/rounds/round-113/review-record.json`
  is the authoritative rev-004 item-1 acceptance record. It froze the same
  selected `C2` / `C5` / `C7` pocket, the exact current-result surface, the
  exact future writable docs boundary, and explicit predecessor immutability.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-surface-and-successor-boundary-freeze.md`
  is the authoritative rev-004 item-1 freeze and therefore the direct scope
  contract for this settlement-ledger round.
- `orchestrator/rounds/round-111/review-record.json`
  remains the authoritative acceptance record for the current exact-pocket
  post-amendment read that item `2` must record.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md`
  remains the decisive accepted current-result anchor. It fixes the exact
  current same-pocket post-amendment read that this ledger must carry
  forward onto new bounded settlement surfaces only.
- the still-live pre-amendment same-family docs remain immutable historical
  evidence only:
  `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`,
  `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`,
  `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`,
  and
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`.
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

- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`
  - Responsibility: canonical docs-only rev-004 item-2 bounded settlement
    ledger for the exact same-pocket post-amendment read only. It must
    consume accepted `round-113` plus the accepted `round-111` current-result
    anchor, publish that current read on a new bounded settlement surface,
    and preserve predecessor immutability explicitly.

### Read-Only Evidence

- `orchestrator/rounds/round-114/selection.md`
- `orchestrator/rounds/round-113/review-record.json`
- `orchestrator/rounds/round-111/review-record.json`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/verification.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-surface-and-successor-boundary-freeze.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-handoff-decision.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md`
- the five immutable predecessor settlement artifacts named above

### Future Validation Surface (Must Remain Absent Or Unchanged In This Round)

- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-validation.md`

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/rounds/round-114/selection.md`
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
- every historical `docs/plans/` artifact other than the one exact item-2
  output path named above

No round-local `implementation-notes.md`, alternate settlement memo, or
supplemental roadmap note is authorized. The docs artifact above is the only
intended implementation output path.

## Settlement Questions

The canonical artifact must answer exactly these bounded questions:

1. What exact accepted post-amendment read from `round-111` is now being
   carried onto a new rev-004 settlement surface?
2. Which still-live predecessor artifacts remain immutable historical
   evidence rather than writable settlement targets?
3. What exact-pocket-only claim is being recorded now, and what broader
   same-family or repo-level claims remain blocked?

The artifact must stay documentary. It must not validate the new settlement
surface yet and must not decide the later handoff.

## Review Checklist

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- prove the round-owned diff is docs/orchestrator-only apart from the
  controller-owned `orchestrator/state.json`
- prove the canonical artifact records the exact accepted post-amendment read
  on one new bounded settlement surface only
- prove predecessor immutability is explicit and that the validation surface
  remains unwritten
- prove no round-local `implementation-notes.md` was created
- because this round is docs-only, record the broader Cabal gate as
  intentionally out of scope

## Finish Condition

The round is ready for review when:

- the canonical item-2 settlement ledger exists at the exact path above;
- it records the exact accepted current same-pocket post-amendment read on
  the new bounded settlement surface only;
- it preserves predecessor immutability explicitly;
- it leaves the future validation surface for item `3` untouched; and
- no file outside the allowed docs/orchestrator packet has changed.
