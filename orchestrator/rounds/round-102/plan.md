# Round 102 Plan (`item-4` Representative End-To-End Settlement Campaign Across The Family Matrix)

## Objective

Execute only roadmap item `4` and produce one canonical settlement-campaign
artifact at:
`docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`.

This is the initial `item-4` plan for `attempt-1` with `retry: null`. The
round must run one bounded representative end-to-end settlement campaign across
the current family matrix under the frozen item-1 settlement contract and the
accepted item-2 / item-3 production-surface slices. The artifact must:

- replay the current representative read for positive families `P1` through
  `P6` on the existing solver -> elaboration -> reconstruction ->
  internal/public output surfaces;
- replay the required negative or bounded families with emphasis on `N1`,
  `N2`, and `N6`, while recording whether any claimed `N4` pressure is still
  only out-of-scope boundary pressure or is actually becoming required by an
  intended positive family;
- classify every representative row with the frozen item-1 vocabulary:
  `stable visible persistence`,
  `admitted but not reconstruction-visible / blocker debt`, or
  `fail-closed rejection`;
- keep the matrix tally honest about blocker debt, reject-side behavior, and
  any remaining architecture pressure; and
- stop short of item `5`: the artifact may sharpen the representative
  settlement read, but it must not itself record
  `non-cyclic-graph = keep` or
  `reopen the non-cyclic-graph revision question`.

Current planning read: the accepted item-1 freeze, the accepted item-2 / item-3
settlement slices, the accepted March 25 representative campaign, and the
accepted same-lane predecessor chain still point to the same conservative
starting posture:

- there are still zero accepted `stable visible persistence` rows;
- `C1`, `C2`, `C5`, and `C7` still point to
  `admitted but not reconstruction-visible / blocker debt`;
- `C3` still points to `fail-closed rejection`;
- the ambiguity and termination rows still look bounded only through
  fail-closed discipline rather than representative positive success; and
- the accepted audit still keeps `non-cyclic-graph = unknown`, so any `N4`
  pressure note must stay evidence-only in this round.

The expected strongest lawful item-4 result is therefore one honest matrix
replay that preserves those frozen truths unless fresh read-only reruns
contradict them. If a rerun does contradict the accepted record, fail closed
inside the same representative matrix and record the contradiction honestly
rather than widening into repair work, item `5`, production implementation, or
architecture revision.

This round is docs-first and evidence-first. It may consume existing focused
tests, existing exact-pocket replay harnesses, accepted item-1 / item-2 /
item-3 artifacts, the accepted representative row schema, and read-only source
anchors as evidence, but it must not edit production code, test code,
`mlf2.cabal`, controller-owned machine state, roadmap machine metadata, retry
contracts, verification contracts, or `Bugs.md`.

## Locked Round Context

- Round id: `round-102`
- Roadmap item: `item-4`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-102`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-102`
- Fixed live subject: one bounded representative end-to-end settlement
  campaign across the family matrix only
- Current round review feedback: none yet; no `review.md`,
  `reviews/attempt-<n>.md`, or `review-record.json` exists for `round-102`, so
  this is a full first-attempt plan rather than a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `M orchestrator/state.json`
- `?? orchestrator/rounds/round-102/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-102/selection.md`
  fixes this round to roadmap item `4` only, binds the output to one bounded
  representative family-matrix settlement campaign, and forbids widening into
  item `5`, production implementation, hardening, cyclic search, multi-SCC
  search, second interfaces, fallback paths, or a broad capability claim.
- `orchestrator/state.json`
  fixes the live controller state at `active_round_id = round-102`,
  `stage = plan`, `current_task = item-4`, `retry = null`,
  `roadmap_id = 2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`,
  `roadmap_revision = rev-001`, and the authoritative
  `roadmap_dir = orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`
  makes item `4` the lowest-numbered unfinished dependency-satisfied item and
  defines its completion notes as one honest representative replay across the
  family matrix without consuming item `5`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/retry-subloop.md`
  confirms that item `4` may retry in principle, but `retry: null` means this
  plan is the full first-attempt plan.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/verification.md`
  requires representative-campaign checks proving that item `4` reruns the
  family matrix honestly across positive and required negative or bounded
  families and does not hide unresolved blocker debt, ambiguity, or
  termination pressure.
- `docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`
  is the accepted item-1 freeze. It already binds:
  - the exact item-5 settlement bar;
  - the unresolved family ledger for `P1` through `P6` and `N1` through `N6`;
  - the frozen outcome vocabulary
    `stable visible persistence`,
    `admitted but not reconstruction-visible / blocker debt`, and
    `fail-closed rejection`; and
  - the current repo-level read that the matrix remains below settlement.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  preserves the inherited boundary:
  explicit recursive annotations remain the production baseline;
  recursive meaning remains iso-recursive only; and non-equi-recursive,
  non-cyclic-graph, no-second-interface, and no-fallback boundaries remain
  binding.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  preserves the positive and negative family matrix. Item `4` must consume the
  full family matrix, not a convenience subset.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  keeps `non-cyclic-graph = unknown` at repo scope. Item `4` may sharpen
  matrix evidence under the unchanged acyclic model, but it may not consume
  the later architecture decision.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  fixes the current phase-and-surface ledger vocabulary, including the rule
  that only `stable visible persistence` counts as positive `P6` success.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  remains the accepted predecessor representative row schema and matrix tally:
  it already defines `C1` through `C7`, keeps the old campaign at
  `bounded subset only`, and provides the minimum representative coverage
  surface this round must refresh against the new accepted item-2 / item-3
  evidence.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  keeps `continue within the current architecture` as the strongest accepted
  aggregate predecessor read before this refreshed roadmap family. Item `4`
  may not reinterpret that predecessor read into a present-tense item-5
  settlement result.
- `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
  is the accepted item-2 artifact. It already sharpened `C1`, `C2`, and `C5`
  and fixed their current blocker-debt classifications.
- `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
  is the accepted item-3 artifact. It already sharpened `C3` and `C7`, kept
  `P5` reject-side only, and kept `C7` below `stable visible persistence`.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`,
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`,
  and
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
  remain the accepted exact-pocket predecessor chain for the `C2` / `C5` /
  `C7` same-lane retained-child pocket only. They establish one exact pocket
  and bounded blocker-debt / current-architecture evidence only, not a family-
  level or repo-level success claim.
- Accepted rounds `round-094` through `round-101` remain bounded predecessor
  evidence only. They do not authorize widening beyond the exact same-lane
  pocket, the exact non-local packet, or the frozen item-1 settlement bar.
- `Bugs.md`
  still lists `BUG-2026-03-16-001` as predecessor replay / `InstBot` context
  only. It does not authorize item-4 repair work, replay reopen, fallback
  widening, or roadmap reordering.

## File Map

### Create

- `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`
  - Responsibility: canonical item-4 artifact recording the refreshed
    representative family-matrix replay, the current row-by-row evidence,
    the matrix tally, the positive-family ledger, the required negative /
    bounded-family ledger, and one bounded representative settlement read that
    still stops short of item `5`.

- `orchestrator/rounds/round-102/implementation-notes.md`
  - Responsibility: concise round-local mirror of the same representative
    matrix, the exact commands rerun, the observed bounded surface facts, the
    family-to-row mapping, and the final per-row classifications.

### Read-Only Evidence

- `orchestrator/rounds/round-102/selection.md`
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
- `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
- `orchestrator/rounds/round-094/review-record.json`
- `orchestrator/rounds/round-095/review-record.json`
- `orchestrator/rounds/round-096/review-record.json`
- `orchestrator/rounds/round-097/review-record.json`
- `orchestrator/rounds/round-098/review-record.json`
- `orchestrator/rounds/round-099/review-record.json`
- `orchestrator/rounds/round-100/review-record.json`
- `orchestrator/rounds/round-101/review-record.json`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
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

produce one bounded representative family-matrix end-to-end settlement-campaign
dossier under the frozen item-1 settlement ledger and the accepted item-2 /
item-3 production-surface slices.

This slice is allowed to:

- create one canonical item-4 artifact and one round-local
  `implementation-notes.md`;
- reuse the accepted representative row schema while refreshing it against the
  new authoritative item-2 / item-3 evidence;
- include one explicit local-shape row for `P1`, the inherited `C1` through
  `C7` rows, and an explicit `N4` pressure note tied to the refreshed matrix;
- rerun existing focused tests, existing exact-pocket replay harnesses, and
  read-only source-anchor inspections to make the current bounded surface facts
  reviewer-visible again; and
- record exactly one honest classification for each representative row from the
  frozen vocabulary
  `stable visible persistence`,
  `admitted but not reconstruction-visible / blocker debt`, or
  `fail-closed rejection`,
  together with one bounded representative settlement read that still stops
  short of item `5`.

This slice is not allowed to:

- decide item `5`, revise the architecture, or record
  `non-cyclic-graph = keep` / `reopen`;
- land or plan production implementation, hardening, or the final capability
  claim;
- edit `src/`, `src-public/`, `app/`, `test/`, `mlf2.cabal`,
  `orchestrator/state.json`, roadmap contracts, retry contracts, verification
  contracts, or `Bugs.md`;
- invent a new packet, neighboring route, or second same-lane family for `C2`,
  `C5`, or `C7`;
- invent a positive polymorphism packet for `C3`;
- relitigate accepted predecessor truth from item `1`, item `2`, item `3`, or
  the same-lane exact-pocket chain;
- use the matrix replay as authority for cyclic search, multi-SCC search,
  second interfaces, or fallback widening; or
- hide missing evidence by collapsing `N4` pressure into an already-decided
  architecture claim.

## Sequential Tasks

### Task 1 - Freeze the docs-only item-4 campaign frame and representative scope

- Treat this as a fresh item-4 settlement-evidence round for one representative
  family-matrix replay, not as repair work, production implementation, or the
  item-5 decision gate.
- Create the canonical item-4 artifact at the path above and state explicitly
  that it is `attempt-1` with `retry: null`.
- Reassert the inherited boundary unchanged:
  - explicit recursive annotations remain the production baseline;
  - recursive meaning remains iso-recursive only;
  - no equi-recursive equality or implicit unfolding is authorized;
  - no cyclic structural graph encoding or multi-SCC search is authorized;
  - no second interface is authorized; and
  - no compatibility / convenience / default-path fallback widening is
    authorized.
- State explicitly that item `4` owns only the representative settlement
  campaign. It may sharpen the matrix evidence and name pressure, but it does
  not authorize item `5`, item `6`, item `7`, or item `8`.
- Freeze the row-level outcome vocabulary before gathering evidence:
  `stable visible persistence`,
  `admitted but not reconstruction-visible / blocker debt`, and
  `fail-closed rejection`.

### Task 2 - Freeze the representative row inventory and family mapping before reruns

- Reuse the accepted representative campaign row schema rather than inventing a
  new matrix shape.
- Build one explicit row inventory for this round:
  - `P1-row`: local recursive-shape / local-lane support for
    `P1 local-recursive-shape`;
  - `C1`: non-local alias-bound / base-like admitted family;
  - `C2`: same-lane retained-child admitted family;
  - `C3`: nested-`forall` / quantified-crossing pressure;
  - `C4`: ambiguity / competing-candidate pressure;
  - `C5`: binder-sensitive / owner-sensitive placement pressure using the same
    exact `C2` pocket only;
  - `C6`: termination-pressure under the inherited acyclic model, with
    explicit interaction notes for `N4` and `N5`; and
  - `C7`: reconstruction-heavy output-surface pressure using the same exact
    same-lane retained-child pocket only.
- Keep the family-to-row responsibilities explicit:
  - `P1-row` is the only representative row for `P1`;
  - `C1` is the primary bounded row for `P2` and may contribute bounded
    continuity context for `P3` and `P6`;
  - `C2` is the primary bounded row for `P3`;
  - `C5` is the primary bounded row for `P4` while preserving `N2` guard
    context;
  - `C3` is reject-side only for `P5` plus `N2`;
  - `C4` is the representative row for `N1`;
  - `C6` is the representative row for `N6` and the place where any claimed
    `N4` pressure must be stated explicitly; and
  - `C7` is the primary bounded row for `P6`.
- State explicitly that `C5` and `C7` reuse the exact same `C2` same-lane
  pocket only; they are not new packets, not new routes, and not alternate
  public interfaces.
- State explicitly that `N3` and `N5` remain inherited boundary rows carried
  forward from the accepted baseline unless fresh evidence somehow forces a
  narrower representative note. They are not fresh implementation targets in
  this round.

### Task 3 - Gather fresh read-only evidence for the representative rows

- Reuse existing read-only anchors and accepted predecessor docs only. Do not
  create new helper functions, new tests, or new code paths to make the
  evidence easier to collect.
- For `P1-row`, rerun the current local-shape contrast on existing surfaces:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "does not infer recursive shape for the corresponding unannotated variant"'`
  - Use the accepted item-1 ledger and audit together with these reruns to
    determine whether the current repo record has any review-visible
    unannotated local recursive-shape success at all, or only bounded support
    below the representative success bar.
- For `C1`, rerun the exact accepted non-local packet evidence only:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - Reuse the accepted visible-output fact for the exact non-local packet
    unless a fresh rerun now shows a different production-surface result.
- For `C2`, `C5`, and `C7`, rerun and capture reviewer-visible evidence from
  the exact same-lane retained-child packet only:
  - the read-only exact-pocket replay already used in accepted rounds `097`,
    `100`, and `101` via `cabal repl mlf2-test --repl-options=-ignore-dot-ghci`
    with `:module + *PipelineSpec`, so the round records the helper-visible
    internal result, `containsMu`, and both authoritative public outputs for
    the exact frozen packet;
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- For `C3`, do not invent a new positive public-output replay harness if the
  accepted record is still reject-side only. Instead, reuse the same nested-
  `forall` fail-closed test, the accepted item-3 slice, and the accepted same-
  lane predecessor chain.
- For `C4`, rerun the current ambiguity rejection anchor on existing test
  surfaces only:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "rejects ambiguous repeated graft-weaken on the same non-front binder"'`
  - Use that rerun together with the accepted representative campaign and the
    current item-1 ledger as the representative `N1` row evidence. Do not
    widen into a new ambiguity corpus.
- For `C6`, reuse the smallest accepted bounded-search evidence only:
  - rerun the focused
    `ARI-C1 feasibility characterization (bounded prototype-only)` block; and
  - use the accepted item-1 freeze, accepted audit, and accepted predecessor
    representative campaign to state whether the current matrix still shows
    only bounded rejection inside the inherited acyclic model, or whether a
    refreshed row now makes a sharper `N4` pressure note necessary.
- Treat every rerun as evidence only. Even if a rerun still ends in blocker
  debt or reject-side classification, do not patch runtime behavior or tests
  in this round.

### Task 4 - Write one refreshed representative matrix and family ledgers

- Write the canonical item-4 artifact so it contains one representative matrix
  covering exactly the `P1-row` plus `C1` through `C7`.
- For each row, record at least these fields:
  - representative label;
  - controlling family or families;
  - exact frozen packet / route or exact fail-closed boundary;
  - read-only evidence sources rerun this round;
  - current surface facts that remain binding;
  - one lawful classification from the frozen item-1 vocabulary; and
  - why the row remains bounded rather than promotable directly into item `5`.
- Apply the accepted predecessor evidence narrowly:
  - `P1-row` must stay honest about the difference between bounded local-shape
    support and actual review-visible unannotated production-surface success;
  - `C1` must preserve the exact non-local alias-bound / base-like packet and
    its current visible-output fact unless fresh reruns prove otherwise;
  - `C2` and `C5` must preserve the exact same-lane route and the one owner-
    local frame already accepted;
  - `C3` must stay reject-side only unless fresh reruns lawfully show more on
    the same existing surfaces;
  - `C4` must remain ambiguity rejection unless fresh reruns contradict that
    accepted representative read;
  - `C6` must record bounded-search / termination behavior without converting
    that row into an architecture decision; and
  - `C7` must preserve the exact helper-visible `TMu ...` versus public
    `forall identity` split unless fresh reruns prove matching internal/public
    recursive visibility.
- Add an explicit positive-family ledger for `P1` through `P6` that maps each
  family to its strongest current representative row(s) and states whether the
  family now has any row at `stable visible persistence` or still remains below
  the item-5 settlement bar.
- Add an explicit negative / bounded-family ledger that covers at least:
  - `N1 ambiguity-reject` from `C4`;
  - `N2 unsoundness-guard` from `C3` and `C5`;
  - `N6 termination-pressure` from `C6`; and
  - one explicit `N4` pressure note saying whether the refreshed matrix still
    leaves `N4` as boundary context only or whether any current positive-family
    failure is beginning to point at that boundary.
- Keep `N3` and `N5` as inherited boundary carry-forward notes only unless the
  refreshed matrix genuinely requires a sharper bounded note. Do not invent
  fresh implementation work for them.

### Task 5 - Record the bounded representative settlement read without consuming item `5`

- Add one short synthesis section that says exactly what item `4` now sharpens
  and exactly what it still does not settle.
- The synthesis must, at minimum, say whether the refreshed matrix still
  points to:
  - zero `stable visible persistence` rows or a different tally if fresh
    evidence now proves one;
  - any positive-family row that remains only
    `admitted but not reconstruction-visible / blocker debt`;
  - `P5` as reject-side only or a stronger same-boundary read if the reruns
    truly support it;
  - `N1`, `N2`, and `N6` as bounded fail-closed discipline rather than broad
    positive support; and
  - `N4` as boundary pressure only or a sharper matrix-level concern that item
    `5` must later judge.
- It is lawful for the refreshed item-4 synthesis to say that the matrix still
  reads as `bounded subset only` if that is what the current representative
  replay shows. It is not lawful for item `4` to convert that matrix read into
  `non-cyclic-graph = keep` or `reopen`.
- State explicitly that item `5` remains the next immediate consumer and that
  items `6` through `8` remain blocked behind the item-5 decision gate.

### Task 6 - Mirror the bounded result in round-local notes only

- Write `orchestrator/rounds/round-102/implementation-notes.md` as a concise
  round-local mirror of the canonical artifact.
- The notes must explicitly restate:
  - the representative row inventory (`P1-row`, `C1` through `C7`);
  - the exact commands rerun this round;
  - the observed bounded surface facts for each refreshed row cluster; and
  - the final per-row classifications plus the bounded matrix tally.
- The notes must stay round-local only. Do not create extra settlement ledgers,
  alternate decision documents, or a shadow item-5 outcome in this round.

## Verification Handoff

The implementer should leave reviewer-visible evidence for all of the
following:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- predecessor-presence checks for:
  - the accepted item-1 freeze;
  - the accepted item-2 and item-3 artifacts;
  - the accepted March 25 representative campaign and architecture-decision
    artifacts;
  - the accepted same-lane predecessor docs; and
  - `orchestrator/rounds/round-094/review-record.json` through
    `orchestrator/rounds/round-101/review-record.json`
- the exact same-lane retained-child replay command used for this round, with
  reviewer-visible output for the helper-visible internal result,
  `containsMu`, and both authoritative public outputs
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "does not infer recursive shape for the corresponding unannotated variant"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "rejects ambiguous repeated graft-weaken on the same non-front binder"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- `rg -n 'P1|C1|C2|C3|C4|C5|C6|C7|stable visible persistence|admitted but not reconstruction-visible / blocker debt|fail-closed rejection|bounded subset only|N4|N6' docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md orchestrator/rounds/round-102/implementation-notes.md`
- `git diff --name-only -- src test src-public app mlf2.cabal Bugs.md orchestrator/state.json`
  - expected result: no output; this round is docs-only unless the plan is
    violated

Because this plan preserves `src/`, `src-public/`, `app/`, `test/`,
`mlf2.cabal`, `Bugs.md`, and `orchestrator/state.json` unchanged,
`cabal build all && cabal test` is out of scope for this round unless the diff
escapes the authorized docs / orchestrator artifact surface.

## Failure Discipline

If fresh read-only evidence does not support the expected matrix read, fail
closed inside the same representative rows:

- record the exact contradiction in the canonical artifact and the round-local
  notes;
- choose the lawful row classification that the actual rerun now supports, but
  keep the interpretation bounded to the same exact row and packet / pressure
  lane;
- do not patch code, add tests, or widen into repair work, item `5`, or
  production implementation;
- if `P1-row` cannot be made reviewer-visible on current surfaces without
  inventing a new harness or a new packet, record that gap explicitly as
  representative-campaign debt rather than substituting a different family;
- if any row seems to require cyclic structure, multi-SCC search, a second
  interface, or fallback behavior, record that only as bounded `N4` / `N5`
  pressure for item `5` to judge later; and
- do not reinterpret one contradicted row into repo-level
  `non-cyclic-graph = keep` or `reopen`.

## Completion Criteria

This plan is complete when the implementer can land one docs-first canonical
artifact and one round-local notes file that:

- preserve the frozen item-1 settlement vocabulary and inherited boundary
  unchanged;
- classify one explicit `P1-row` plus `C1` through `C7`, with no hidden row
  omissions and no silent packet widening;
- keep `C5` and `C7` tied to the same exact `C2` same-lane pocket rather than
  inventing new packets or routes;
- keep `C3` reject-side only unless fresh same-boundary evidence truly proves
  more;
- keep the ambiguity and termination rows bounded rather than broadening them
  into a new search or architecture theory;
- include one explicit `N4` pressure note without consuming item `5`;
- sharpen only the current representative settlement read without deciding
  `non-cyclic-graph = keep` / `reopen`;
- leave code, tests, controller state, roadmap machine metadata, and
  `Bugs.md` untouched; and
- hand off only to later bounded roadmap work, with item `5` as the next
  immediate consumer, without silently widening into production
  implementation or a broad capability claim.
