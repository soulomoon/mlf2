# Round 069 Plan (`N2` Thesis-Backed Next Live-Subject Selection)

## Objective

Execute only roadmap item `N2` and prepare one docs-only selection artifact at:
`docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`.

This is the initial `N2` plan for `attempt-1` with `retry: null`. The round
must treat accepted `L1` fail-closed closure, accepted `L2 = stop-blocked`,
and accepted `N1 = reopen-planning-only` as binding predecessor continuity,
stay inside the reopened planning-only lane, and choose exactly one
thesis-backed next live subject without binding an implementation slice.

Current planning read: the only lawful `N2` selection is the preserved generic
scheme-alias / base-like `baseTarget` route, but only as a docs-first
alias-bound / bound-inlining / binding-structure planning subject. The thesis
anchors for that read are:

- `8.3.1 Inlining bounds`: bound inlining is only lawful when binder and
  binding-flag reconstruction stay unambiguous.
- `15.6.2 Expressivity of alias bounds`: alias bounds need special care in
  xMLF, inert/alias bounds may need inlining, and the graphic presentation
  does not directly express alias bounds.
- the recursive-types outlook near the end of the thesis: recursive types plus
  second-order polymorphism are challenging, the main difficulty is the
  binding structure, and cyclic term-graphs were not the attempted route.

`N2` should therefore select the preserved generic scheme-alias / base-like
`baseTarget` route only at planning scope and explicitly defer every other
`ResultType.Fallback` family, replay path, solver-wide recursive-inference
route, cross-family route, and code-changing slice.

`N2` is docs-only subject-selection work. It does not bind the `N3` safety
contract, the `N4` exact target, any implementation slice, any verification
slice, any roadmap/state/bug-tracker edit, or any predecessor-history rewrite.

## Locked Round Context

- Round id: `round-069`
- Roadmap item: `N2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: post-`L2` automatic iso-recursive successor planning lane
- Active branch: `codex/round-069-n2-next-live-subject-selection`
- Active worktree:
  `.worktrees/round-069`
- Stage mode: docs-only next-live-subject selection only
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `?? orchestrator/rounds/round-069/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-069/selection.md` already fixed this round to
  roadmap item `N2` only and explicitly forbids using this round to bind
  `N3` through `N7`, implementation, verification, roadmap/state edits,
  bug-tracker edits, or predecessor-history rewrites.
- `orchestrator/rounds/round-069/state-snapshot.json` in the controller root fixes the live controller
  state at `active_round_id: "round-069"`, `stage: "plan"`,
  `current_task: "N2"`, `branch:
  "codex/round-069-n2-next-live-subject-selection"`, and `retry: null`.
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/roadmap.md` makes `N2` the first pending item after accepted
  `N1`; all later items depend on the subject selection that `N2` freezes and
  therefore cannot run first.
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  still records `N2 = NO` because the next live subject has not yet been
  thesis-backed and fixed.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the inherited baseline contract: explicit recursive annotations are
  supported, automatic recursive-type inference remains unresolved and
  disabled, and the explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary remains mandatory.
- `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md` finalized the
  repaired `URI-R2-C1` queue as fail-closed inside the inherited boundary:
  no fresh lawful exact successor slice remained there.
- `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  finalized the repaired queue as `stop-blocked` and preserved the broader
  generic scheme-alias / base-like `baseTarget` route only as future context
  pending a separate roadmap amendment plus fresh selection.
- `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
  finalized the only lawful post-`L2` authority outcome as
  `reopen-planning-only`, kept the repaired `URI-R2-C1` queue closed as
  predecessor evidence only, and made the preserved generic scheme-alias /
  base-like `baseTarget` route admissible for later `N2` selection only.
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/retry-subloop.md` allows retries for `N2`, but it also makes
  prior attempts immutable. This `attempt-1` plan must therefore create one
  new bounded selection artifact without rewriting any prior attempt or review
  artifact.
- `Bugs.md` currently carries
  `BUG-2026-03-16-001` as open continuity context, but that replay /
  `InstBot` defect does not widen `N2` into replay reopen, `MLF.Elab.Inst`,
  or any implementation slice. The bug tracker stays read-only in this round.

## File Map

### Modify

- `docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
  - Responsibility: record the `N2` docs-only selection attempt, cite the
    thesis basis for the chosen live subject, name exactly one selected
    planning subject, explain why it remains planning-only, and explicitly
    defer every other alternative.

### Read-Only Evidence

- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/roadmap.md`
- `orchestrator/rounds/round-069/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/verification.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/retry-subloop.md`
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
- `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
- `Bugs.md`
- `papers/these-finale-english.txt`
- `orchestrator/rounds/round-069/selection.md`

### Preserve Unchanged

- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/roadmap.md`
- `orchestrator/rounds/round-069/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/verification.md`
- `orchestrator/rounds/round-069/selection.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
- `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
- `Bugs.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`

## Sequential Tasks

### Task 1 - Re-establish exact `N2` authority and planning-only boundary

- Write the future canonical `N2` artifact as `attempt-1` with `retry: null`.
- State explicitly that `N2` is docs-only live-subject selection work inside
  accepted `N1 = reopen-planning-only`. It may choose exactly one subject
  only; it may not bind `N3`, `N4`, `N5`, `N6`, or `N7`.
- Preserve accepted `L1` / `L2` closure and accepted `N1` planning-only
  authority as binding predecessor continuity.
- Preserve the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary unchanged.
  The current planning read does not propose any roadmap amendment here.

### Task 2 - Reconstruct the exact admissible selection domain

- Use `selection.md`, `state.json`, `roadmap.md`, `retry-subloop.md`, the
  mechanism table, the baseline contract, accepted `L1`, accepted `L2`,
  accepted `N1`, and `Bugs.md` to restate the exact post-`L2` packet that now
  constrains `N2`.
- Carry forward these fixed inputs:
  - the repaired `URI-R2-C1` queue remains closed predecessor evidence only;
  - accepted `N1` reopened only the planning lane, not an implementation lane;
  - the preserved generic scheme-alias / base-like `baseTarget` route is the
    only admissible candidate carried forward for `N2` selection;
  - another `ResultType.Fallback` family, replay reopen, the open
    `InstBot` bug, and any different solver/pipeline subject remain blocked
    unless a later accepted roadmap item explicitly reopens them.
- Do not relitigate whether `L1` or `L2` should have stayed open. `N2` must
  inherit their closure as fixed truth and select only inside the authority
  that `N1` actually created.

### Task 3 - Ground the selection in thesis-backed constraints

- Extract only the thesis evidence needed to justify why the next live subject
  must stay narrow and binder-centric:
  - `8.3.1 Inlining bounds`: inlining is only lawful when the bound can be
    rebuilt unambiguously, including its binder and binding flag.
  - `15.6.2 Expressivity of alias bounds`: alias bounds need special care in
    xMLF; inert/alias bounds may need inlining; the graphic presentation does
    not directly express alias bounds.
  - the recursive-types outlook near the end of the thesis: recursive types
    plus second-order polymorphism are challenging, the main difficulty lies
    in the binding structure, and cyclic term-graphs were not the chosen
    route.
- Use those anchors to justify the precise `N2` subject:
  - select the preserved generic scheme-alias / base-like `baseTarget` route;
  - frame it as the next docs-first alias-bound / bound-inlining /
    binding-structure planning subject; and
  - keep it outside implementation readiness, equi-recursive reasoning, and
    cyclic-graph experimentation.
- The artifact should cite the section headers explicitly and paraphrase the
  thesis points into repo-local selection language. It should not promise a
  code path or claim that the thesis already authorizes one.

### Task 4 - Write the canonical `N2` artifact as one bounded selection

- Create
  `docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`.
- The artifact must include:
  - the stage contract freeze for `round-069` / `N2` / `attempt-1` /
    `retry: null`;
  - accepted `L1` / `L2` / `N1` continuity as binding input;
  - the thesis basis from the selected sections of
    `papers/these-finale-english.txt`;
  - the exact one-subject `N2` selection; and
  - an explicit blocked/deferred-alternatives section.
- Current planning read: record exactly one selected subject:
  - the preserved generic scheme-alias / base-like `baseTarget` route;
  - treated as the next docs-first live subject because the thesis says
    alias-bound / inert-bound handling and recursive binding structure need
    explicit, careful treatment before broader inference work can proceed; and
  - kept at planning scope only, not as a bound implementation slice and not
    as verification-ready behavior.
- State explicitly that `N3` must still write the verifier-checkable safety
  contract for this selected subject, and `N4` must still bind one exact
  bounded target before any design or code slice begins.

### Task 5 - Keep deferred alternatives and non-authorization explicit

- State explicitly that `N2` does not authorize implementation, verification,
  merge action, roadmap edits, controller-state edits, bug-tracker edits, or
  predecessor-history rewrites.
- State explicitly that `N2` defers or rejects, as still blocked:
  - any other `ResultType.Fallback` family;
  - any different solver/pipeline subject;
  - replay reopen;
  - `MLF.Elab.Inst` or `InstBot`;
  - `boundVarTarget`;
  - `boundTarget`;
  - `schemeBodyTarget`;
  - `src/MLF/Elab/Run/ResultType/View.hs`;
  - equi-recursive reasoning;
  - implicit unfolding;
  - cyclic structural graph encoding;
  - multi-SCC support;
  - cross-family search;
  - a second executable interface; or
  - any compatibility, convenience, or default-path fallback.
- State explicitly that admissibility of the preserved generic scheme-alias /
  base-like `baseTarget` route is not implementation readiness, verification
  clearance, or authority to skip `N3` / `N4`.

### Task 6 - Run the docs-only verification required for `N2`

Run the baseline docs/state checks required by `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-069/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-069/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/roadmap.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
- `test -f docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
- `test -f tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `test -f orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/retry-subloop.md`

Run `N2`-specific continuity and thesis checks:

- `rg -n '^2\\. \\[pending\\] Execute the `N2` thesis-backed next live-subject selection inside the accepted planning-only lane' orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/roadmap.md`
- `rg -n 'N2 — Thesis-backed next live-subject selection|Research the thesis-backed next admissible subject and record explicit deferred alternatives' tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `rg -n 'no fresh lawful exact successor slice|generic scheme-alias / base-like `baseTarget` route' docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
- `rg -n 'stop-blocked|separate roadmap amendment|fresh selection' docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
- `rg -n 'reopen-planning-only|admissible for later `N2` selection only|implementation and verification blocked' docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
- `rg -n '8\\.3\\.1 Inlining bounds|we inline the bounds which can be rebuilt unambiguously|reconstruct the binder and the binding flag' papers/these-finale-english.txt`
- `rg -n '15\\.6\\.2 Expressivity of alias bounds|A simple solution is to entirely inline all inert bounds|forbid alias bounds entirely|graphic presentation of MLF, where they cannot be expressed at all' papers/these-finale-english.txt`
- `rg -n 'recursive types and second-order polymorphism alone is already tricky|main difficulties likely lie in the treatment of recursion in the binding structure|Allowing cyclic term-graphs' papers/these-finale-english.txt`
- `rg -n 'Attempt: `attempt-1`|Retry state: `null`|generic scheme-alias / base-like `baseTarget` route|planning scope only|defer' docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`

- `cabal build all && cabal test` is intentionally out of scope for this round
  if the diff remains docs-only and does not touch `src/`, `src-public/`,
  `app/`, `test/`, or `mlf2.cabal`. The implementation notes and reviewer
  record must state that skip reason explicitly rather than implying future
  code clearance.

## Reviewer Handoff

Reviewer should confirm that the implemented `N2` artifact:

- is explicitly framed as `round-069` / `N2` / `attempt-1` / `retry: null`;
- treats accepted `L1` / `L2` closure and accepted `N1 = reopen-planning-only`
  as binding predecessor continuity;
- selects exactly one thesis-backed next live subject;
- selects the preserved generic scheme-alias / base-like `baseTarget` route
  only as a docs-first alias-bound / binding-structure planning subject;
- explicitly defers every other alternative as still blocked;
- preserves the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary unchanged; and
- leaves `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-002/roadmap.md`, `orchestrator/rounds/round-069/state-snapshot.json`, `Bugs.md`, and
  prior attempt/review history unchanged.
