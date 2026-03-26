# Round 100 Plan (`item-2` Bounded Production-Surface Settlement Evidence For `C1`, `C2`, And `C5`)

## Objective

Execute only roadmap item `2` and produce one canonical settlement-evidence
artifact at:
`docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`.

This is the initial `item-2` plan for `attempt-1` with `retry: null`. The
round must produce one bounded production-surface settlement-evidence slice for
the three already-frozen blocker-debt rows carried forward by item `1`:

- `C1`: the admitted non-local alias-bound / base-like packet on
  `baseTarget -> baseC -> targetC`;
- `C2`: the exact same-lane retained-child packet on
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`; and
- `C5`: the binder-sensitive / owner-sensitive placement lens that reuses the
  same exact `C2` packet and does not introduce a second packet.

The output must sharpen the frozen repo-level reads for `P2`, `P3`, and `P4`
only. It may use internal/public output facts as evidence, but it must not
promote any row into global `P6` success, representative-campaign success, or
repo-level `non-cyclic-graph` settlement.

Current planning read: item `1` already froze `C1`, `C2`, and `C5` as
`admitted but not reconstruction-visible / blocker debt`, and the accepted
record still contains zero `stable visible persistence` rows. The expected
strongest lawful item-2 result is therefore to preserve blocker-debt
classification for all three rows unless fresh read-only reruns contradict the
accepted predecessor evidence. If a rerun does contradict that record, fail
closed inside the same frozen rows and record the contradiction honestly rather
than widening into mechanism work, repair, representative campaigning, or the
item-5 global settlement gate.

This round is docs-first and evidence-first. It may consume existing focused
tests, read-only replay harnesses, and accepted predecessor artifacts as
evidence, but it must not edit production code, test code, `mlf2.cabal`,
controller-owned machine state, roadmap machine metadata, retry contracts,
verification contracts, or `Bugs.md`.

## Locked Round Context

- Round id: `round-100`
- Roadmap item: `item-2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-100`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-100`
- Fixed live subject: one bounded production-surface settlement-evidence slice
  for `C1`, `C2`, and `C5` only
- Current round review feedback: none yet; no `review.md`,
  `reviews/attempt-<n>.md`, or `review-record.json` exists for `round-100`, so
  this is a full first-attempt plan rather than a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `M orchestrator/state.json`
- `?? orchestrator/rounds/round-100/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-100/selection.md`
  fixes the round to roadmap item `2` only, binds the output to the bounded
  `C1` / `C2` / `C5` settlement-evidence slice, and forbids widening into item
  `3`, item `4`, item `5`, production implementation, hardening, cyclic
  search, multi-SCC search, second interfaces, fallback paths, or a broad
  capability claim.
- `orchestrator/state.json`
  fixes the live controller state at `active_round_id = round-100`,
  `stage = plan`, `current_task = item-2`, `retry = null`,
  `roadmap_id = 2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`,
  `roadmap_revision = rev-001`, and the authoritative
  `roadmap_dir = orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`
  makes item `2` the lowest-numbered unfinished dependency-satisfied item and
  defines its completion notes as reviewable production-surface evidence for
  `C1`, `C2`, and `C5` with honest classification against the frozen ledger.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/retry-subloop.md`
  confirms that item `2` may retry in principle, but `retry: null` means this
  plan is the full first-attempt plan.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/verification.md`
  requires propagation-and-placement proof-slice checks proving that item `2`
  produces representative production-surface evidence for `P2`, `P3`, and
  `P4` inside the inherited acyclic model without silent widening.
- `docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`
  is the accepted item-1 freeze. It already binds the exact item-5 settlement
  bar, the unresolved family ledger, the outcome vocabulary
  `stable visible persistence`,
  `admitted but not reconstruction-visible / blocker debt`, and
  `fail-closed rejection`, and the current repo-level reads for `C1`, `C2`,
  and `C5`.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  preserves the inherited boundary:
  explicit recursive annotations remain the production baseline;
  recursive meaning remains iso-recursive only; and non-equi-recursive,
  non-cyclic-graph, no-second-interface, and no-fallback boundaries remain
  binding.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  preserves the family-matrix contract. Item `2` may sharpen only the
  `P2` / `P3` / `P4` rows and must not silently settle the full matrix.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  keeps `non-cyclic-graph = unknown` at repo scope. Item `2` may gather
  bounded evidence under the unchanged acyclic model, but it may not consume
  the later architecture decision.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  fixes the current surface vocabulary for the admitted non-local family and
  the same-lane retained-child family, including the blocker-debt rule for
  non-recursive or non-publicly-visible output.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  preserves the accepted matrix tally:
  zero `stable visible persistence` rows;
  blocker-debt rows `C1`, `C2`, `C5`, and `C7`;
  fail-closed rows `C3`, `C4`, and `C6`.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  keeps `continue within the current architecture` as the strongest accepted
  aggregate strategic read before this refreshed roadmap family. Item `2`
  may not reinterpret that predecessor read into a present-tense item-5
  settlement.
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-verification-gate.md`,
  and
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-next-cycle-decision-gate.md`
  remain the accepted predecessor chain for the exact `C1`
  non-local alias-bound / base-like packet only. They establish one bounded
  admitted packet and verification chain, not a general non-local clearance.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`,
  and
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
  remain the accepted predecessor chain for the exact `C2` / `C5` same-lane
  retained-child pocket only. They establish one exact pocket and one
  blocker-debt classification under the unchanged architecture, not a family-
  level or repo-level success claim.
- Accepted rounds `round-072` through `round-074` and `round-094` through
  `round-098` remain bounded predecessor evidence only. They do not authorize
  widening beyond the exact non-local packet and the exact same-lane retained-
  child pocket.
- `Bugs.md`
  still lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect
  remains predecessor replay context only. It does not authorize item-2 repair
  work, replay reopen, fallback widening, or roadmap reordering.

## File Map

### Create

- `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
  - Responsibility: canonical item-2 artifact recording the exact bounded
    `C1` / `C2` / `C5` packets, the focused read-only evidence rerun for each
    row, and one honest classification per row against the frozen item-1
    ledger for `P2`, `P3`, and `P4`.

- `orchestrator/rounds/round-100/implementation-notes.md`
  - Responsibility: concise round-local mirror of the same three-case
    settlement slice, the exact commands rerun, the observed bounded surface
    facts, and the final case-by-case classifications.

### Read-Only Evidence

- `orchestrator/rounds/round-100/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/verification.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/retry-subloop.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-verification-gate.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-next-cycle-decision-gate.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
- `orchestrator/rounds/round-072/review-record.json`
- `orchestrator/rounds/round-073/review-record.json`
- `orchestrator/rounds/round-074/review-record.json`
- `orchestrator/rounds/round-094/review-record.json`
- `orchestrator/rounds/round-095/review-record.json`
- `orchestrator/rounds/round-096/review-record.json`
- `orchestrator/rounds/round-097/review-record.json`
- `orchestrator/rounds/round-098/review-record.json`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/TermClosure.hs`
- `test/PipelineSpec.hs`
- `test/ElaborationSpec.hs`
- `Bugs.md`

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

## Exact Selected Slice (Exactly One)

The only selected slice is:

produce one bounded three-row production-surface settlement-evidence dossier
for `C1`, `C2`, and `C5` under the frozen item-1 ledger.

This slice is allowed to:

- create one canonical item-2 artifact and one round-local
  `implementation-notes.md`;
- reuse the already-accepted exact non-local packet and the already-accepted
  exact same-lane retained-child packet as read-only evidence subjects;
- rerun existing focused tests and existing read-only replay harnesses to make
  the accepted surface facts reviewer-visible again; and
- record exactly one honest classification for each of `C1`, `C2`, and `C5`
  from the frozen vocabulary
  `stable visible persistence`,
  `admitted but not reconstruction-visible / blocker debt`, or
  `fail-closed rejection`.

This slice is not allowed to:

- land or plan production implementation, hardening, representative-campaign
  work, or the item-5 global settlement gate;
- edit `src/`, `src-public/`, `app/`, `test/`, `mlf2.cabal`,
  `orchestrator/state.json`, roadmap contracts, retry contracts, verification
  contracts, or `Bugs.md`;
- invent a new packet, neighboring route, or second retained-child family for
  `C5`; it must reuse the exact `C2` pocket only;
- relitigate accepted predecessor truth from the non-local or same-lane chains;
- promote any row into global `P6` success, repo-level family-matrix success,
  or `non-cyclic-graph = keep` / `reopen`; or
- authorize equi-recursive reasoning, cyclic graphs, multi-SCC search, second
  interfaces, or fallback widening.

## Sequential Tasks

### Task 1 - Freeze the exact three-case subject and family mapping

- Treat this as a fresh item-2 settlement-evidence round for three already-
  frozen blocker-debt rows, not as mechanism work, repair work, or an
  architecture-decision round.
- Carry forward unchanged:
  - `C1` as the preserved non-local alias-bound / base-like packet on
    `baseTarget -> baseC -> targetC`, exercised by
    `schemeAliasBaseLikeFallback False`;
  - `C2` as the exact same-lane retained-child packet on
    `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`; and
  - `C5` as the binder-sensitive / owner-sensitive placement lens that reuses
    the exact `C2` packet, the same owner-local retained-child frame, and the
    same clear-boundary-only status.
- Freeze the family-to-row responsibilities explicitly:
  - `C1` is the primary bounded evidence row for `P2 non-local-propagation`
    and may contribute only bounded continuity context for `P3`;
  - `C2` is the primary bounded evidence row for
    `P3 retained-child-owner-sensitive`; and
  - `C5` is the primary bounded evidence row for
    `P4 binder-sensitive-placement`, while still preserving the `N2`
    owner/binder guard context.
- State explicitly in both artifact surfaces that item `2` sharpens only the
  current `P2` / `P3` / `P4` settlement read and leaves `P5`, broad `P6`, the
  representative campaign, and the item-5 architecture gate for later items.

### Task 2 - Gather fresh read-only evidence for `C1`, `C2`, and `C5`

- Reuse existing read-only anchors and accepted predecessor docs only. Do not
  create new helper functions, new tests, or new code paths to make the
  evidence easier to collect.
- For `C1`, rerun and capture reviewer-visible evidence from the existing
  accepted non-local packet only:
  - the `Fallback.hs` source anchors that keep
    `rootNonLocalSchemeAliasBaseLike` explicit and keep the non-local
    `baseTarget -> baseC -> targetC` handoff separate from local lanes;
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane"'`;
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`; and
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`.
- For `C1`, do not invent a new public-output-specific harness if the current
  accepted packet is still only reviewer-visible through the existing
  non-local fallback/test anchors. Instead, cite the accepted item-1 and
  item-5 predecessor record that its current visible-output fact remains
  `TBase (BaseTy "Int")` with `containsMu False`.
- For `C2` and `C5`, rerun and capture reviewer-visible evidence from the
  existing same-lane retained-child packet only:
  - the read-only exact-pocket replay already used in the accepted
    same-lane predecessor chain via `cabal repl mlf2-test` with
    `:module + *PipelineSpec`, so the round records the helper-visible
    internal result, `containsMu`, and both authoritative public outputs for
    the exact frozen packet;
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`;
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`;
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`;
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`;
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`;
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`; and
  - the same focused
    `ARI-C1 feasibility characterization (bounded prototype-only)` block.
- Treat every rerun as evidence only. Even if a rerun still ends in blocker
  debt, do not patch runtime behavior or tests in this round.

### Task 3 - Write one three-row settlement matrix for `P2`, `P3`, and `P4`

- Write the canonical item-2 artifact so it contains one bounded settlement
  matrix with exactly three case rows:
  `C1`, `C2`, and `C5`.
- For each row, record at least these fields:
  - exact frozen packet / route;
  - controlling positive-family target (`P2`, `P3`, or `P4`);
  - read-only evidence sources rerun this round;
  - current surface facts that remain binding;
  - one lawful classification from the frozen item-1 vocabulary; and
  - why the case remains bounded rather than promotable to repo-level
    settlement.
- Apply the accepted predecessor evidence narrowly:
  - `C1` should record the admitted non-local packet, the preserved visible
    output fact `TBase (BaseTy "Int")` with `containsMu False`, and the reason
    that this sharpens `P2` only to
    `admitted but not reconstruction-visible / blocker debt`;
  - `C2` should record the same-lane retained-child route, the preserved
    helper-visible recursive read (`TMu ...`, `containsMu True`), the
    authoritative public collapse to `TForall "a" Nothing (TVar "a")`, and
    the reason that `P3` remains
    `admitted but not reconstruction-visible / blocker debt`; and
  - `C5` should record that the same exact `C2` packet is the only admitted
    owner-local / binder-sensitive placement story, that nested-`forall` or
    frame drift still fail closed, and that `P4` therefore remains
    `admitted but not reconstruction-visible / blocker debt` rather than a
    settled binder-sensitive success.
- State explicitly that:
  - `C5` is not a second packet or second same-lane family;
  - none of the three rows reaches `stable visible persistence` on the current
    accepted evidence unless a fresh rerun actually proves otherwise; and
  - none of the three rows by itself forces
    `reopen the non-cyclic-graph revision question`.

### Task 4 - Record the bounded settlement implications without widening scope

- Add one short synthesis section that says exactly what item `2` now sharpens
  and exactly what it still does not settle.
- The synthesis must say, in substance:
  - `P2` still has one admitted non-local packet but not a reconstruction-
    visible non-local production-surface success;
  - `P3` still has one admitted same-lane retained-child pocket but not a
    general owner-sensitive production-surface success; and
  - `P4` still has one admitted owner-local placement pocket but not a general
    binder-sensitive placement success.
- The synthesis must also say what remains later work only:
  - item `3` minimum production implementation slices;
  - item `4` representative family-matrix campaign;
  - item `5` the global `non-cyclic-graph` settlement gate; and
  - any code or test edits.

### Task 5 - Mirror the bounded result in round-local notes only

- Write `orchestrator/rounds/round-100/implementation-notes.md` as a concise
  round-local mirror of the canonical artifact.
- The notes must explicitly restate:
  - the exact three frozen rows and their exact routes;
  - the exact commands rerun this round;
  - the observed bounded surface facts for `C1`, `C2`, and `C5`; and
  - the final case-by-case classifications.
- The notes must stay round-local only. Do not create extra settlement ledgers,
  side packets, or alternate decision documents in this round.

## Verification Handoff

The implementer should leave reviewer-visible evidence for all of the
following:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- predecessor-presence checks for the item-1 artifact, the C1 predecessor
  docs, and the C2/C5 predecessor docs
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`
- the exact same-lane retained-child replay command used for this round, with
  reviewer-visible output for the helper-visible internal result,
  `containsMu`, and both authoritative public outputs
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- `rg -n 'C1|C2|C5|P2|P3|P4|stable visible persistence|admitted but not reconstruction-visible / blocker debt|fail-closed rejection' docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md orchestrator/rounds/round-100/implementation-notes.md`
- `git diff --name-only -- src test src-public app mlf2.cabal Bugs.md orchestrator/state.json`
  - expected result: no output; this round is docs-only unless the plan is
    violated

Because this plan preserves `src/`, `src-public/`, `app/`, `test/`,
`mlf2.cabal`, `Bugs.md`, and `orchestrator/state.json` unchanged,
`cabal build all && cabal test` is out of scope for this round unless the diff
escapes the authorized docs / orchestrator artifact surface.

## Failure Discipline

If fresh read-only evidence does not support the expected blocker-debt read,
fail closed inside the same frozen rows:

- record the exact contradiction in the canonical artifact and the round-local
  notes;
- choose the lawful row classification that the actual rerun now supports;
- keep the contradiction bounded to `C1`, `C2`, or `C5` rather than
  improvising a broader theory;
- do not patch code, add tests, or widen into mechanism work, representative
  campaigning, or the global settlement gate; and
- do not reinterpret one contradicted row into repo-level
  `non-cyclic-graph = keep` or `reopen`.

## Completion Criteria

This plan is complete when the implementer can land one docs-first canonical
artifact and one round-local notes file that:

- preserve the frozen item-1 settlement vocabulary and inherited boundary
  unchanged;
- classify exactly `C1`, `C2`, and `C5`, with no extra rows and no missing
  row;
- keep `C5` tied to the same exact `C2` pocket rather than inventing a second
  packet;
- sharpen only the current `P2` / `P3` / `P4` settlement read without
  consuming item `3`, item `4`, or item `5`;
- leave code, tests, controller state, roadmap machine metadata, and
  `Bugs.md` untouched; and
- hand off only to later bounded roadmap work, with item `3` as the next
  immediate consumer, without silently widening into production
  implementation or global settlement.
