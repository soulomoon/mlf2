# Round 116 Plan (`item-4` Post-Settlement Same-Family Handoff Decision)

## Objective

Execute only roadmap item `4` and prepare one docs-only aggregate decision
artifact at:
`docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md`.

This is the initial `item-4` plan for `attempt-1` with `retry: null`. The
round must consume accepted rev-004 items `1` through `3` as authoritative
predecessor truth and record exactly one lawful follow-on outcome for the
one selected same-lane `C2` / `C5` / `C7` pocket only:

- `stop after bounded settlement`; or
- `publish one later same-family successor revision for further bounded same-pocket settlement`.

Current planning read: accepted rev-004 item `1` froze the current-result
anchor, writable boundary, and predecessor immutability rule; accepted item
`2` published the new bounded settlement ledger on that exact writable
surface; and accepted item `3` validated that ledger against the accepted
`round-111` current-result anchor. The original rev-004 settlement debt is
therefore discharged. No additional exact-pocket writable surface remains
unsettled, and the older pre-amendment dossiers are now intentionally
historical evidence rather than active settlement debt. The stronger lawful
item-4 outcome is therefore `stop after bounded settlement`, not another
successor revision.

This round is docs-only and aggregate-only. It must preserve
`iso-recursive = keep`,
`non-equi-recursive = keep`, and
`no-fallback = keep`;
keep accepted rev-004 items `1`, `2`, and `3` authoritative;
keep rev-001 items `6` through `8` blocked;
and keep code changes, hardening, rollout, second interfaces, multi-SCC
search, fallback widening, and broad capability claims blocked. It must not
edit source code, tests, Cabal, or `orchestrator/state.json` in this round.

If the final decision records `publish one later same-family successor revision`,
the round may also create one bounded rev-005 roadmap bundle. Current
planning read says that bundle should not be created because no further
lawful exact-pocket settlement debt remains after accepted rev-004 item `3`.

## Locked Round Context

- Round id: `round-116`
- Roadmap item: `item-4`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-116`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-116`
- Fixed live subject: one docs-only rev-004 item-4 aggregate post-settlement
  handoff decision for the exact same-lane retained-child / public-output
  continuity pocket already frozen, settled, and validated in accepted items
  `1` through `3`
- Fixed inherited boundary:
  `explicit-only / iso-recursive / non-equi-recursive / no-fallback / one-interface / no-multi-SCC`

Current worktree state is already non-pristine at the control-plane level.
Respect existing edits and do not revert unrelated work:

- controller-owned `orchestrator/state.json` is modified and must remain
  untouched by decision-artifact work; and
- existing `orchestrator/rounds/round-116/selection.md` is a round input and
  must remain untouched.

Under the live retry contract, item `4` is aggregate-only. Review may reject
and send the same round back to `plan`, but `accepted + retry` is not lawful
for this item.

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-116/selection.md`
  fixes this round to roadmap item `4` only, requires an aggregate-only
  post-settlement handoff decision for the one selected same-lane pocket,
  preserves accepted rev-004 item `1` through item `3` truth, and blocks
  further settlement writing and scope widening.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/roadmap.md`
  makes item `4` the only remaining pending item in rev-004 and defines
  completion as exactly one lawful follow-on outcome:
  stop after the bounded settlement, or publish one later same-family
  successor revision for further bounded same-pocket settlement.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/retry-subloop.md`
  keeps item `4` aggregate-only, forbids `accepted + retry`, and preserves
  prior-attempt immutability if review rejects the round back to `plan`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/verification.md`
  requires post-settlement-decision checks proving that item `4` records
  exactly one follow-on outcome for rev-004 and does not by implication
  reopen blocked work or broad rollout.
- `orchestrator/rounds/round-113/review-record.json`
  is the authoritative acceptance record for item `1`.
- `orchestrator/rounds/round-114/review-record.json`
  is the authoritative acceptance record for item `2`.
- `orchestrator/rounds/round-115/review-record.json`
  is the authoritative acceptance record for item `3`. It validates the
  bounded settlement ledger against the accepted current-result anchor while
  preserving predecessor immutability and exact-pocket-only language.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-surface-and-successor-boundary-freeze.md`
  remains the decisive accepted item-1 boundary contract.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`
  remains the decisive accepted item-2 settlement ledger.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-validation.md`
  remains the decisive accepted item-3 validation artifact.
- accepted predecessor docs that still describe the old public collapse
  remain historical evidence only and must not be silently rewritten or
  reclassified as unresolved debt inside item `4`.

## File Map

### Create

- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md`
  - Responsibility: canonical docs-only rev-004 item-4 aggregate decision
    record for the one selected same-lane `C2` / `C5` / `C7` pocket only.
    It must consume accepted rev-004 items `1` through `3`, preserve
    inherited keep axes and blocked work, and record exactly one lawful
    follow-on outcome.

### Optional Create If The Selected Outcome Publishes A Successor Revision

- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-005/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-005/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-005/verification.md`

Current planning read does not expect these files to be created.

### Read-Only Evidence

- `orchestrator/rounds/round-116/selection.md`
- `orchestrator/rounds/round-113/review-record.json`
- `orchestrator/rounds/round-114/review-record.json`
- `orchestrator/rounds/round-115/review-record.json`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/verification.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-surface-and-successor-boundary-freeze.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-validation.md`
- immutable predecessor settlement artifacts that remain historical evidence

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/rounds/round-116/selection.md`
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
- every accepted rev-004 artifact and every accepted predecessor settlement
  artifact other than the one exact item-4 output path named above

No round-local `implementation-notes.md`, side ledger, alternate decision
memo, or supplemental roadmap note is authorized unless the selected outcome
lawfully publishes rev-005. The decision artifact above is the only intended
implementation output path in the current planning read.

## Exact Decision Questions

The canonical artifact must answer exactly these bounded questions:

1. After accepted rev-004 items `1`, `2`, and `3`, does any concrete
   same-pocket settlement debt still remain that requires another successor
   revision?
2. If yes, can that work be expressed as one later same-family successor
   revision without widening beyond the same exact pocket?
3. If not, is the stronger lawful read to stop after the bounded
   settlement?

The decision must stay aggregate-only. It must not relitigate the accepted
item-1/item-2/item-3 technical results themselves.

## Review Checklist

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- prove the round-owned diff is docs/orchestrator-only apart from the
  controller-owned `orchestrator/state.json`
- prove the canonical artifact records exactly one lawful item-4 outcome
- if rev-005 is published, prove the new bundle stays same-pocket-only and
  keeps the next unfinished item concrete
- if stop is selected, prove no further concrete exact-pocket settlement
  debt remains after accepted rev-004 item `3`
- prove no round-local `implementation-notes.md` was created
- because this round is aggregate-only and docs-only, record the broader
  Cabal gate as intentionally out of scope

## Finish Condition

The round is ready for review when:

- the canonical item-4 decision artifact exists at the exact path above;
- it records exactly one lawful follow-on outcome for this revision;
- any published rev-005 bundle is concrete and same-pocket-only, or the stop
  decision proves why no further exact-pocket settlement debt remains; and
- no file outside the allowed docs/orchestrator packet has changed.
