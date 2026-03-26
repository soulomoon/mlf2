# Round 105 Plan (`item-2` Exact Live Reopened Subject Selection)

## Objective

Execute only roadmap item `2` and prepare one docs-only selection artifact at:
`docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-reopened-subject-selection.md`.

This is the initial `item-2` plan for `attempt-1` with `retry: null`. The
round must consume accepted `round-104` item `1` as authoritative predecessor
truth, stay inside the single frozen candidate boundary, and choose exactly
one live reopened subject without widening into any later-item contract or any
implementation lane.

Current planning read: the only lawful `item-2` selection is the exact
same-lane retained-child / public-output continuity pocket already frozen in
the accepted predecessor chain for rows `C2`, `C5`, and `C7`. The selection
must stay bounded to that one exact same-family route:

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

Within rev-002, this pocket is the one lawful next live
architecture-pressure subject because the accepted representative matrix still
shows:

- `C2` as the strongest admitted same-lane retained-child pocket;
- `C5` as the same exact `C2` pocket through the owner / binder-sensitive
  placement lens, not a second packet;
- `C7` as the same exact `C2` / `C5` pocket through the public-output
  continuity lens, with helper-visible/internal recursive structure but
  authoritative public collapse; and
- non-local `C1` as admitted contrast context only, not a coequal reopened
  subject.

`item-2` is docs-only and architecture-only. It must not define the `item-3`
safety contract, the `item-4` exact audit bind, or the `item-5`
open-one-lane-versus-stop decision. It must keep accepted rev-001 truth
binding, including the rule that rev-001 items `6` through `8` remain
blocked.

## Locked Round Context

- Round id: `round-105`
- Roadmap item: `item-2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-105`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-105`
- Fixed live subject: one docs-only rev-002 subject-selection artifact that
  selects exactly one live reopened subject inside the accepted item-1
  boundary
- Current round review feedback: none yet; no `review.md`,
  `reviews/attempt-<n>.md`, or `review-record.json` exists for `round-105`,
  so this is a full `attempt-1` plan rather than a retry delta

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` (controller-owned; must remain untouched)
- `?? orchestrator/rounds/round-105/selection.md`

Under the live retry contract, item `2` is retry-capable, but this plan is
for the initial `retry: null` attempt only.

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-105/selection.md`
  fixes this round to roadmap item `2` only, binds the output to one
  docs-only subject-selection artifact, preserves accepted item-1 truth, and
  explicitly blocks implementation work, later-item contracts, multi-subject
  widening, and broad architecture search.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002/roadmap.md`
  makes item `2` the next unfinished item after accepted item `1` and defines
  completion as one accepted docs-only artifact choosing exactly one next live
  architecture-pressure subject inside the frozen boundary.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002/retry-subloop.md`
  keeps item `2` retry-capable inside the same round while preserving prior
  attempts as immutable history.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002/verification.md`
  requires reopened-subject-selection checks proving that item `2` selects
  exactly one bounded live subject grounded in accepted representative rows
  and does not widen into multiple packets, unrelated routes, or broader
  family search.
- `orchestrator/rounds/round-104/review-record.json`
  is the authoritative acceptance record for predecessor item `1`. Its
  accepted checks make the boundary freeze binding and preserve rev-001 items
  `6` through `8` as blocked.
- `docs/plans/2026-03-26-global-non-cyclic-graph-reopened-revision-authority-and-candidate-boundary-freeze.md`
  is the authoritative item-1 freeze. It preserved rev-002 as planning-only
  and architecture-only, froze exactly one candidate boundary, kept `C1`
  contrast-only, kept `C5` inside the same exact `C2` pocket, and deferred
  subject selection to item `2`.
- `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  is the authoritative rev-001 item-5 reopen record. It preserved
  `iso-recursive = keep`, `non-equi-recursive = keep`, and
  `no-fallback = keep`, reopened only the `non-cyclic-graph` question, and
  explicitly kept rev-001 items `6` through `8` blocked pending a later
  revision.
- `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
  binds `C1`, `C2`, and `C5` as blocker debt, preserves `C2` as one exact
  same-lane retained-child pocket, and preserves `C5` as the same exact
  pocket through the owner / binder-sensitive placement lens only.
- `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
  binds `C7` as the same exact `C2` / `C5` pocket through the
  reconstruction-visible / public-output continuity lens and keeps it below
  `stable visible persistence`.
- `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`
  is the accepted representative matrix. It keeps zero
  `stable visible persistence` rows, keeps `C1`, `C2`, `C5`, and `C7` as
  blocker debt, keeps `N4` as architecture-pressure context only at this
  stage, and makes the `C2` / `C5` / `C7` pocket the strongest admitted
  same-family route still below repo-level success.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
  freezes the exact same-lane pocket, route, owner frame, clear-boundary
  status, and exact packet identity carried forward here.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`
  fixes the exact-pocket item-4 classification as
  `admitted but not reconstruction-visible / blocker debt` and keeps the
  internal/public split reviewer-visible.
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
  remains bounded predecessor evidence only. It recorded that this same exact
  pocket previously stayed as blocker debt within the current architecture,
  but accepted rev-001 item `5` later promoted the global `non-cyclic-graph`
  question into the next revision-level architecture question without
  widening beyond this same-family pressure route.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the inherited boundary contract: explicit-only baseline,
  iso-recursive only, non-equi-recursive, non-cyclic-graph inherited
  boundary, no second interface, and no fallback widening.

## File Map

### Create

- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-reopened-subject-selection.md`
  - Responsibility: canonical docs-only rev-002 item-2 artifact selecting the
    one exact live reopened subject inside the accepted item-1 boundary,
    grounded in the accepted representative matrix and the accepted same-lane
    retained-child / public-output continuity predecessor chain.

### Read-Only Evidence

- `orchestrator/rounds/round-105/selection.md`
- `orchestrator/rounds/round-104/review-record.json`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002/verification.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-reopened-revision-authority-and-candidate-boundary-freeze.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/rounds/round-105/selection.md`
- roadmap files under
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-002/`
- `Bugs.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`

No round-local `implementation-notes.md` is authorized for this item-2 plan.
The docs artifact above is the only planned implementation output path. The
implementer must not create or edit any other file for item `2`.

## Exact Selected Slice (Exactly One)

The only selected slice is:

select the exact same-lane retained-child / public-output continuity pocket
already carried by accepted representative rows `C2`, `C5`, and `C7` as the
sole live reopened subject inside the frozen rev-002 boundary, with non-local
`C1` retained only as contrast context.

This slice is allowed to:

- restate the accepted rev-002 item-1 boundary and the accepted rev-001
  reopen truth as binding predecessor authority;
- identify the one exact pocket by its already-frozen family, anchor,
  owner-local frame, route, clear-boundary status, and exact packet identity;
- explain why that pocket, and not `C1`, is the strongest admitted
  same-family route still creating live architecture pressure in the accepted
  representative matrix;
- state that `C7` is the public-output continuity face of the same exact
  `C2` / `C5` pocket rather than a second packet or second interface; and
- hand off only the selected subject identity to later rev-002 items.

This slice is not allowed to:

- widen into multiple subjects, multiple packets, multiple routes, or
  multiple same-family lanes;
- select `C1` as a parallel or coequal live reopened subject;
- turn the exact same-lane pocket into multi-SCC search, general cyclic
  search, second-interface work, fallback widening, or a broad capability
  claim;
- define the `item-3` safety / acceptance contract;
- bind the `item-4` exact rows, packets, modules, commands, or output
  surfaces for future audit;
- decide the `item-5` open-one-lane-versus-stop outcome; or
- authorize source edits, tests, `mlf2.cabal` edits, `Bugs.md` edits,
  controller-state edits, roadmap edits, or commits.

If the artifact starts selecting more than one pressure subject, treats `C5`
or `C7` as separate packets, turns `C1` into a second lane, or begins naming
future audit surfaces or implementation work as live, it has already widened
beyond item `2` and must fail closed.

## Explicit Disallowed Scope

The following remain explicitly disallowed in this round:

- any change to accepted item-1 truth, accepted rev-001 truth, or the rule
  that rev-001 items `6` through `8` stay blocked;
- any widening into multiple subjects, multiple packets, unrelated routes,
  broader family search, or multiple round outputs;
- any reopening of the non-local family beyond contrast-only `C1` context;
- any attempt to define the evidence bar for
  `acyclic still sufficient`,
  `single-component cyclic-structure successor lane justified`, or
  `stop`;
- any attempt to freeze the later exact audit bind across rows, packets,
  modules, review outputs, or commands;
- any attempt to decide whether rev-002 opens one bounded
  architecture-amendment lane or stops;
- any equi-recursive semantics, implicit unfolding, cyclic implementation,
  multi-SCC search, second interface, fallback widening, production
  implementation, hardening, or repo-level capability claim;
- any edit to `orchestrator/state.json`, roadmap files, retry files,
  verification files, `Bugs.md`, source code, tests, or Cabal files; and
- any commit.

## Sequential Tasks

### Task 1 - Re-establish item-2 authority and the docs-only selection frame

- Write the future canonical item-2 artifact as `attempt-1` with
  `retry: null`.
- State explicitly that item `2` is docs-only live-subject selection work
  inside accepted item `1`; it may choose exactly one subject only and may
  not bind item `3`, item `4`, or item `5`.
- Preserve accepted rev-001 truth, accepted round-104 item-1 truth, and the
  inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary unchanged.

### Task 2 - Reconstruct the exact admissible selection domain

- Use `selection.md`, the rev-002 roadmap bundle, accepted round-104 review,
  the item-1 freeze, the accepted rev-001 reopen gate, the accepted
  representative matrix, and the accepted same-lane predecessor docs to
  restate the exact domain item `2` is allowed to narrow.
- Carry forward these fixed inputs:
  - the frozen candidate boundary is one bounded single-component
    cyclic-structure successor-lane question only;
  - the strongest admitted same-family pressure remains the one exact
    `C2` / `C5` / `C7` pocket;
  - `C5` is the same exact `C2` pocket, not a second packet;
  - `C7` is the public-output continuity face of that same pocket, not a
    second interface; and
  - `C1` remains admitted contrast context only unless the accepted
    selection explicitly says otherwise, which current planning read does not.
- Do not relitigate whether the boundary exists. Item `1` already froze that
  authority.

### Task 3 - Record the exact one-subject selection

- Create the canonical item-2 artifact at the path above.
- Select exactly one live reopened subject:
  the exact same-lane retained-child / public-output continuity pocket
  already frozen by the accepted same-lane predecessor chain and represented
  in the accepted global matrix by `C2`, `C5`, and `C7`.
- State the subject in bounded continuity terms:
  one family,
  one anchor,
  one owner-local frame,
  one route,
  one clear-boundary status,
  and one exact packet.
- State explicitly that the public-output continuity pressure carried by `C7`
  is what makes this same exact pocket the live architecture-pressure subject
  inside rev-002, while `C2` and `C5` remain continuity context on the same
  route and `C1` remains non-local contrast only.

### Task 4 - Freeze handoff ownership without stepping into later items

- State explicitly that item `3` still owns the safety / acceptance contract
  for the selected subject.
- State explicitly that item `4` still owns the exact audit bind across
  rows, packets, modules, commands, and output surfaces.
- State explicitly that item `5` still owns the one lawful decision on
  whether to open one bounded architecture-amendment lane or stop.
- Reassert that this round remains docs-only, planning-only, and
  architecture-only. The selected subject is not implementation clearance,
  not verification clearance, and not a broad architecture claim.

## Required Verification After The Plan Lands

- `git diff --check`
- `git status --short --branch`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- `rg -n 'same-lane retained-child|C2|C5|C7|C1|single-component|rev-001 items `6` through `8`|item `3`|item `4`|item `5`|multi-SCC|second interface|fallback|production implementation|commit' orchestrator/rounds/round-105/plan.md`
- `git diff --name-only -- orchestrator/state.json Bugs.md src src-public app test mlf2.cabal orchestrator/roadmaps`

Review must also confirm the active item-2-specific checks from the live
verification contract:

- exactly one bounded live reopened subject is selected;
- the selected subject is grounded in accepted representative rows and the
  accepted same-family predecessor chain;
- the round does not widen into multiple packets, unrelated routes, or broad
  family search;
- accepted item-1 truth and the blocked rev-001 items `6` through `8`
  remain binding; and
- the round remains docs-only, so the full `cabal build all && cabal test`
  gate is explicitly skipped as out of scope unless the diff escapes the
  authorized docs / round-artifact surface.
