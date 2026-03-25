# Round 096 Plan (`item-3` Retry Delta Attempt-3)

## Objective

Execute only the same-round retry for roadmap item `3`.

This rewrite replaces the rejected `attempt-2` plan with the required
`attempt-3` retry delta only. The retry scope is exactly the current
recorded fix hypothesis from `orchestrator/state.json`,
`orchestrator/rounds/round-096/review.md`,
`orchestrator/rounds/round-096/reviews/attempt-2.md`, and
`orchestrator/rounds/round-096/attempt-log.jsonl`:

- keep the same exact item-3 docs-only blocker-proof outcome for the frozen
  same-lane retained-child pocket;
- keep the already-added current-item handoff note unchanged:
  roadmap item `4` and item `5` remain later work only;
- keep the already-added accepted split and unchanged-anchor summary
  unchanged:
  helper-visible/internal `TMu ...` plus `containsMu True` versus
  authoritative public `TForall "a" Nothing (TVar "a")`, with
  `checkedAuthoritative` still the first exact owner-local break and
  `termClosed` plus `typeCheck termClosed` still the same-pocket
  dependencies; and
- refresh only the reviewer-visible retry framing in the canonical item-3
  artifact and the round-local notes so the live packet no longer
  self-describes as `attempt-1` with `retry: null`, and instead clearly says
  this active retry is limited to repairing the rejected `attempt-2`
  reviewer-visibility gap only.

The canonical item-3 artifact remains:

`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`

The bounded item-3 conclusion also remains unchanged:

- the exact frozen same-lane retained-child pocket still preserves recursive
  structure on the helper-visible internal path as `TMu ...` plus
  `containsMu True`;
- the authoritative public output for the whole packet still collapses to
  `TForall "a" Nothing (TVar "a")`;
- `checkedAuthoritative` remains the first exact owner-local break, with
  `termClosed` and `typeCheck termClosed` as the same-pocket dependencies;
  and
- the strongest lawful result inside the approved item-3 slice remains
  `blocker debt confirmed under the unchanged current architecture`.

This retry does not authorize a new blocker theory, any code or test edit,
any roadmap or controller-state edit, any `Bugs.md` edit, or any widening
beyond the same exact item-3 docs-only blocker-proof round.

## Locked Retry Context

- Round id: `round-096`
- Roadmap item: `item-3`
- Stage: `plan`
- Active attempt: `attempt-3`
- Latest attempt verdict: `rejected`
- Latest stage action: `retry`
- Retry reason:
  attempt `2` fixed the original later-work note and round-local continuity
  gap, but it still cannot finalize because the retry-delta plan required
  the artifacts to re-anchor explicitly to the live `attempt-2`
  reviewer-visibility repair. The canonical item-3 artifact still
  self-describes as `attempt-1` with `retry: null`, and the paired
  round-local notes never say this is the retry-delta attempt.
- Recorded fix hypothesis:
  keep the same exact item-3 docs-only blocker-proof outcome, the same
  later-work note, and the same accepted split / unchanged-anchor summary,
  but refresh the canonical artifact and round-local notes so they
  explicitly frame this round as the `attempt-2` retry-delta
  reviewer-visibility repair only, remove the stale `attempt-1` /
  `retry: null` self-description, and leave every other bounded conclusion
  untouched.
- Active-attempt execution rule for this plan:
  satisfy that recorded fix hypothesis on the live `attempt-3` packet by
  making the canonical artifact and round-local notes identify themselves as
  the current retry-only repair for the rejected `attempt-2`
  reviewer-visibility miss, not as fresh blocker analysis and not as stale
  `attempt-1` / `retry: null` artifacts.

Carry forward without replanning:

- `orchestrator/rounds/round-096/selection.md` still fixes the round to
  roadmap item `3` only and preserves the exact same frozen pocket.
- `orchestrator/rounds/round-096/review.md`,
  `orchestrator/rounds/round-096/reviews/attempt-1.md`,
  `orchestrator/rounds/round-096/reviews/attempt-2.md`, and
  `orchestrator/rounds/round-096/attempt-log.jsonl` already record the prior
  retry history and must remain immutable.
- The later-work note, accepted split, unchanged-anchor summary, root-handoff
  replay, and blocker-proof conclusion are already technically correct and
  must be preserved, not re-litigated.
- The docs-only blocker-proof read is already accepted as technically bounded
  reviewer evidence; this retry exists only to close the remaining
  reviewer-visible retry-framing gap.

## Frozen Subject That Remains Binding

The round remains bounded to the same exact frozen pocket only:

- family: same-lane retained-child;
- recursive-shape anchor: `boundVarTargetRoot`;
- owner / binder frame: one owner-local retained-child frame;
- route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`;
- quantified-boundary status: clear-boundary only; and
- exact packet:

  ```haskell
  ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
  ```

  where
  `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.

The accepted split that must remain visible across the retry artifacts is:

- helper-visible/internal path: `TMu ...` plus `containsMu True`; and
- authoritative public path:
  `TForall "a" Nothing (TVar "a")`.

The unchanged exact blocker anchor that must remain visible across the retry
artifacts is:

- `checkedAuthoritative` remains the first exact owner-local break; and
- `termClosed` plus `typeCheck termClosed` remain the same-pocket
  dependencies that feed that authoritative result.

The inherited boundary also remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized;
- no second interface is authorized; and
- no compatibility, convenience, or fallback widening is authorized.

Accepted `N14`, accepted strategic items `2`, `5`, `6`, and `7`, plus
accepted rounds `089` through `095`, remain bounded predecessor evidence
only. This retry does not relitigate them.

## File Map

### Modify

- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
  - Responsibility: repair only the stale retry metadata / self-description
    at the reviewer-visible artifact surface, while preserving the same
    blocker-proof reasoning, later-work note, accepted split, unchanged
    anchor, root-handoff replay facts, and bounded conclusion.

- `orchestrator/rounds/round-096/implementation-notes.md`
  - Responsibility: add matching live retry framing so the round-local notes
    explicitly present this pass as the retry-only repair for the rejected
    `attempt-2` reviewer-visibility miss, while preserving the same
    later-work note, accepted split, unchanged anchor, and docs-only
    boundary.

### Read-Only Evidence

- `orchestrator/rounds/round-096/selection.md`
- `orchestrator/state.json`
- `orchestrator/rounds/round-096/review.md`
- `orchestrator/rounds/round-096/reviews/attempt-1.md`
- `orchestrator/rounds/round-096/reviews/attempt-2.md`
- `orchestrator/rounds/round-096/attempt-log.jsonl`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
- `orchestrator/rounds/round-096/implementation-notes.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/verification.md`
- `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-phase-6-elaboration-resolution.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`
- `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-successor-decision-gate.md`
- `orchestrator/rounds/round-089/review-record.json`
- `orchestrator/rounds/round-090/review-record.json`
- `orchestrator/rounds/round-091/review-record.json`
- `orchestrator/rounds/round-092/review-record.json`
- `orchestrator/rounds/round-093/review-record.json`
- `orchestrator/rounds/round-094/review-record.json`
- `orchestrator/rounds/round-095/review-record.json`

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/retry-subloop.md`
- `orchestrator/verification.md`
- `Bugs.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- accepted predecessor docs and review records
- reviewer-owned history under earlier round directories

## Exact Retry Delta (Exactly One)

The only retry delta is:

repair the live retry framing on the already-correct item-3 blocker-proof
surfaces.

This retry delta is allowed to:

- refresh the canonical item-3 artifact and round-local notes so they no
  longer present themselves as `attempt-1` with `retry: null`;
- make those two surfaces explicitly say the live packet is a retry-only
  repair for the rejected `attempt-2` reviewer-visibility miss;
- preserve the already-correct later-work note, accepted split,
  unchanged-anchor summary, root-handoff replay, and blocker-proof outcome;
  and
- restate that the round remains docs-only, item-3-only, and bounded to the
  same frozen pocket.

This retry delta is not allowed to:

- revise the blocker-proof outcome itself;
- rewrite the later-work note, accepted split, or unchanged-anchor
  conclusion into a different substantive claim;
- introduce a new collapse theory, a new repair slice, or a broader
  architecture argument;
- edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edit controller-owned state, roadmap contracts, retry contracts,
  verification contracts, or `Bugs.md`;
- rewrite `selection.md`, `review.md`, `reviews/attempt-1.md`,
  `reviews/attempt-2.md`, or `attempt-log.jsonl`; or
- widen into item `4`, item `5`, alias-bound, neighboring-route,
  nested-`forall`, replay / `InstBot`, broad capability, or architecture
  revision work.

## Sequential Retry Tasks

### Task 1 - Re-anchor `attempt-3` to the rejected `attempt-2` issue only

- Treat this plan as a retry delta over rejected `attempt-2`, not as a fresh
  item-3 redesign.
- Carry forward unchanged:
  - the canonical item-3 blocker-proof artifact path;
  - the exact frozen packet and tuple;
  - the exact accepted split:
    `TMu ...`, `containsMu True` versus
    `TForall "a" Nothing (TVar "a")`; and
  - the unchanged blocker anchor:
    `checkedAuthoritative` remains the first exact break, with `termClosed`
    and `typeCheck termClosed` as same-pocket dependencies.
- State explicitly that the live retry repairs reviewer-visible retry
  framing only. It does not reopen the blocker-proof reasoning itself.

### Task 2 - Refresh the canonical artifact's retry framing only

- Refresh
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
  in place.
- Replace the stale self-description
  (`Attempt: attempt-1`, `Retry state: null`, and equivalent contract-freeze
  prose) with live retry framing for this packet.
- The refreshed canonical artifact must make clear, in bounded terms:
  - this is the active retry-only repair for the rejected `attempt-2`
    reviewer-visibility miss;
  - the later-work note for current roadmap item `4` and item `5` remains
    unchanged;
  - the accepted split and unchanged-anchor summary remain unchanged; and
  - the blocker-proof result remains unchanged.
- Preserve the root-handoff replay facts, blocker-proof body, and docs-only
  scope without substantive rewrites unless a stale attempt marker must be
  removed.

### Task 3 - Refresh the round-local notes' retry framing only

- Refresh `orchestrator/rounds/round-096/implementation-notes.md` in place.
- Add one bounded lead summary that explicitly says the live notes belong to
  the active retry-only repair for the rejected `attempt-2`
  reviewer-visibility miss.
- Preserve, without changing their meaning:
  - the accepted split:
    helper-visible/internal `TMu ...` plus `containsMu True`
    versus authoritative public
    `TForall "a" Nothing (TVar "a")`;
  - the unchanged blocker anchor:
    `checkedAuthoritative` remains the first exact owner-local break, with
    `termClosed` and `typeCheck termClosed` as the same-pocket dependencies
    that feed that authoritative result; and
  - the bounded handoff:
    current roadmap item `4` and item `5` remain later work only.
- Do not add new reasoning, new evidence, or new outcome tokens to the
  notes.

### Task 4 - Run docs-only retry verification for the framing repair

Run the minimal verification needed for this docs-only retry delta:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- confirm the stale attempt-1 / retry-null framing is gone from the two
  live implementer-owned artifacts:
  `rg -n 'Attempt: `attempt-1`|retry: null|Retry state: `null`' docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md orchestrator/rounds/round-096/implementation-notes.md`
- confirm the active retry framing is now present on both surfaces:
  `rg -n 'attempt-3|attempt-2|retry-only repair|reviewer-visibility' docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md orchestrator/rounds/round-096/implementation-notes.md`
- confirm the preserved later-work note, accepted split, and unchanged
  blocker anchor still appear on the live artifacts:
  `rg -n 'TMu \\.\\.\\.|containsMu True|TForall \"a\" Nothing \\(TVar \"a\"\\)|checkedAuthoritative|termClosed|typeCheck termClosed|item `4` and item `5` remain later work only|item 4 and item 5 remain later work only' docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md orchestrator/rounds/round-096/implementation-notes.md`
- confirm the retry diff stays docs-only:
  `git diff --name-only -- src test src-public app mlf2.cabal Bugs.md orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/verification.md`

Do not rerun the focused `cabal test` commands in this retry delta unless the
diff escapes the authorized docs-only surface. Prior attempts already record
the focused exact-pocket gates as passing, and this retry does not alter the
runtime-facing subject.

## Completion Criteria

This retry-delta plan is complete only if all of the following become true:

1. `plan.md` explicitly names `attempt-3` and describes a retry delta only
   for the rejected `attempt-2` reviewer-visibility miss.
2. The canonical item-3 artifact no longer self-describes as `attempt-1`
   with `retry: null`, and instead clearly frames itself as the active
   retry-only repair while preserving the same blocker-proof outcome.
3. `implementation-notes.md` mirrors that same live retry framing while
   preserving the later-work note, accepted split, and unchanged blocker
   anchor.
4. The retry diff stays limited to the canonical item-3 artifact and the
   round-local notes on the implementer-owned docs surface.
5. The retry does not widen beyond the same exact item-3 docs-only
   blocker-proof round.

## Non-Authorization

This retry-delta plan does not authorize:

- any edit to `orchestrator/state.json`;
- any edit to the active roadmap bundle files;
- any edit to `Bugs.md`;
- any edit under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- any rewrite of `selection.md`, `review.md`, `reviews/attempt-1.md`,
  `reviews/attempt-2.md`, or `attempt-log.jsonl`;
- any change to the blocker-proof result itself;
- any new item-4 revalidation or item-5 architecture-decision work; or
- any widening into the alias-bound family, neighboring routes,
  nested-`forall` success, broad automatic recursive inference,
  equi-recursive reasoning, cyclic structural graphs, multi-SCC search,
  second interfaces, or fallback paths.

## Reviewer Checks For This Retry

1. `plan.md` explicitly names `attempt-3`, the rejected `attempt-2` retry
   reason, and the current docs-only fix hypothesis.
2. The file map modifies only the canonical item-3 artifact and
   `orchestrator/rounds/round-096/implementation-notes.md`.
3. The sequential tasks preserve the same blocker-proof outcome and already
   correct later-work / split / anchor facts while repairing only the live
   retry framing.
4. The verification section checks removal of stale `attempt-1` /
   `retry: null` text, presence of live retry framing, and preservation of
   the later-work / split / anchor strings.
5. The plan preserves the same bounded item-3 subject and does not silently
   widen into repair, revalidation, or `non-cyclic-graph` reopen work.
