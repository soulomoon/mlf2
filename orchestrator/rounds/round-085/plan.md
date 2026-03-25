# Round 085 Plan (`item-4` Search Model)

## Objective

Execute only roadmap item `4` and produce one docs-first artifact at:
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-search-model.md`.

This is the initial `item-4` plan for `attempt-1` with `retry: null`. The
round must design one bounded search / admissibility / ambiguity /
termination model for general automatic iso-recursive inference inside the
inherited acyclic architecture. The artifact must:

- state when recursive inference is considered at all, using anchor-first
  recursive-shape discovery before downstream consumer choice;
- define the lawful candidate vocabulary in terms of anchor, owner / binder
  placement, local versus non-local propagation mode, target / consumer
  alignment, and quantified-boundary state;
- define when recursive inference is forbidden, including nested-`forall`,
  owner-crossing, neighboring-route, competing-candidate, and other unsafe
  cases that the accepted record does not justify;
- explain how ambiguity fails closed instead of being resolved by hidden
  ranking, lane-specific guessing, post-hoc repair, second interfaces, or
  fallback behavior;
- explain why the resulting search terminates inside the inherited
  non-cyclic-graph model without cyclic structural graphs, multi-SCC search,
  or search reopening loops; and
- address the named item-3 packet-guard debt
  (`rootNonLocalSchemeAliasBaseLike`,
  `sameLaneLocalRetainedChildTarget`,
  `boundHasForallFrom`,
  `not hasForall`) by either lifting it into general admissibility rules or
  recording it explicitly as unresolved blocker debt.

This round is docs-only and search-model-only. It must not edit production
code, tests, public surfaces, executables, `mlf2.cabal`,
`orchestrator/rounds/round-085/state-snapshot.json`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/roadmap.md`,
`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/verification.md`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/retry-subloop.md`, `Bugs.md`,
or any review / merge / predecessor-history artifact.

This round must not silently widen into:

- the full reconstruction / validation contract;
- the representative coverage campaign;
- the final architecture decision;
- a new exact packet-selection or implementation slice;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural graphs, multi-SCC search, or unbounded cross-family
  search;
- second interfaces; or
- compatibility / convenience / fallback paths.

Current planning read: item `4` is the `G3` search-credibility gate, not a
reconstruction, coverage, or architecture-fork gate. If the accepted evidence
does not justify a general admissibility or termination claim, the artifact
must record that limit as explicit blocker debt rather than inventing
heuristics or widening the architecture.

## Locked Round Context

- Round id: `round-085`
- Roadmap item: `item-4`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: one docs-only search / admissibility / ambiguity /
  termination model for the inherited acyclic architecture after accepted item
  `3`
- Active branch: `codex/round-085-item-4-search-model`
- Active worktree:
  `.worktrees/round-085`
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `M orchestrator/rounds/round-085/state-snapshot.json` (controller-owned; must remain untouched)
- `?? orchestrator/rounds/round-085/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-085/selection.md`
  already fixes this round to roadmap item `4` only, limits the scope to one
  docs-first search / admissibility / ambiguity / termination model, and
  forbids implementation, roadmap / state edits, bug-tracker edits, and
  predecessor-history rewrites.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/roadmap.md`
  makes item `4` the next dependency-satisfied item and defines its
  completion notes as one bounded search-model artifact only.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
  supplies the human strategic gate: general recursive inference is not
  credible until the repo can explain when recursive inference is considered,
  forbidden, rejected for ambiguity, and guaranteed to terminate.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  is the accepted item-1 contract that defines the repo-level target and the
  minimum family matrix, especially `P2`-`P5`, `N1`, `N2`, `N4`, `N5`, and
  `N6`, against which this search model must reason.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  is the accepted item-2 audit. Its binding read remains:
  `iso-recursive = keep`, `non-equi-recursive = keep`,
  `non-cyclic-graph = unknown`, and `no-fallback = keep`. Item `4` must work
  inside that bounded read rather than pre-deciding item `7`.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
  is the accepted item-3 mechanism map. It explains the accepted packet
  history through reusable mechanism families, but it leaves search policy,
  generalized admissibility rules, ambiguity rejection, and termination
  discipline as named debt owned by this round.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the inherited baseline contract: explicit recursive annotations are
  still the current baseline, automatic recursive-type inference remains
  unresolved, and the explicit-only / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains mandatory unless a later
  accepted item explicitly changes it.
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  remains the immediate predecessor decision: accepted `N14` preserves one
  exact same-lane retained-child packet as bounded evidence only and does not
  authorize a repo-level generality claim or a silent boundary revision.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/verification.md`
  requires item-4-specific checks proving that the round defines candidate
  generation, ambiguity handling, rejection conditions, and termination
  discipline without hidden heuristic widening.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/retry-subloop.md`
  confirms that `retry: null` means this plan is a full `attempt-1` plan, not
  a delta against a recorded `fix_hypothesis`.
- `Bugs.md` still
  lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
  predecessor implementation context only and does not authorize a code-path
  detour, a replay reopening, or a different live subject for this round.

## File Map

### Create

- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-search-model.md`
  - Responsibility: canonical docs-first item-4 artifact defining one bounded
    anchor-first search / admissibility / ambiguity / termination model for
    the inherited acyclic architecture, tying that model to the accepted
    mechanism map and family matrix, and naming any unresolved blocker debt
    instead of silently widening behavior.

### Read-Only Evidence

- `orchestrator/rounds/round-085/selection.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/retry-subloop.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `Bugs.md`

### Preserve Unchanged

- `orchestrator/rounds/round-085/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/retry-subloop.md`
- `Bugs.md`
- `src/`
- `test/`
- `src-public/`
- `app/`
- `mlf2.cabal`
- reviewer-owned history under earlier round directories

## Exact Selected Slice (Exactly One)

The only selected slice is:

design one bounded search / ambiguity / termination model for general
automatic iso-recursive inference inside the inherited acyclic architecture.

This slice is allowed to:

- define a search vocabulary derived from the accepted mechanism map:
  recursive-shape anchor, owner / binder frame, propagation mode,
  target / consumer route, quantified-boundary state, ambiguity class, and
  termination boundary;
- specify anchor-first candidate generation and route selection order without
  broad heuristic ranking or silent cross-family widening;
- define admissibility rules that constrain local versus non-local
  propagation by owner / binder placement and target / consumer alignment;
- define fail-closed rejection conditions for ambiguity and unsafe cases,
  especially `N1`, `N2`, and bounded `N6` pressure;
- explain termination discipline inside the inherited acyclic model; and
- record each packet-specific guard as either a generalized admissibility rule
  or unresolved blocker debt for later items.

This slice is not allowed to:

- define the full reconstruction / validation contract;
- run the representative coverage campaign;
- make the final architecture fork decision;
- select a new exact packet or implementation slice;
- authorize equi-recursive reasoning, implicit unfolding, cyclic structural
  graphs, multi-SCC search, second interfaces, or fallback paths;
- upgrade negative-only nested-`forall` evidence into a positive `P5`
  capability claim; or
- treat accepted bounded packets as proof that the repo already has general
  automatic iso-recursive inference.

If the search model starts depending on reconstruction, coverage, or
architecture-revision work to justify a claim, record that dependence
explicitly as later-item debt or blocker instead of solving it here.

## Sequential Tasks

### Task 1 - Freeze the item-4 docs-only search-model frame

- Create the canonical item-4 artifact at the path above.
- State explicitly that item `4` is docs-only, search-model-only, and
  `attempt-1` with `retry: null`.
- Reassert the inherited boundary unchanged:
  - explicit-only recursive annotations remain the current production
    baseline;
  - recursive meaning remains iso-recursive only;
  - no equi-recursive equality or implicit unfolding is authorized;
  - no cyclic structural graph encoding or multi-SCC search is authorized;
  - no second interface is authorized; and
  - no compatibility / convenience / default-path fallback widening is
    authorized.
- State explicitly that accepted `N14` and the accepted item-3 mechanism map
  contribute bounded predecessor evidence only and do not pre-clear a general
  search policy as already proven.

### Task 2 - Build the search-model rubric from the controlling docs

- Pull the relevant positive families from the accepted item-1 contract:
  `P2 non-local-propagation`, `P3 retained-child-owner-sensitive`,
  `P4 binder-sensitive-placement`, `P5 polymorphism-nested-forall`, and the
  output-facing dependency that later makes `P6` relevant but still out of
  scope for item `4`.
- Pull the relevant negative / fail-closed families:
  `N1 ambiguity-reject`, `N2 unsoundness-guard`,
  `N4 cyclic-or-multi-scc-required`,
  `N5 second-interface-or-fallback-required`, and
  `N6 termination-pressure`.
- Pull the binding item-2 audit read:
  `non-cyclic-graph = unknown` remains the highest-risk pressure point, while
  `no-fallback = keep` means the search model must stay fail-closed on the
  existing pipeline.
- Pull the item-3 mechanism debt:
  anchor-first recursive-shape discovery, owner / binder placement,
  target / consumer alignment, local versus non-local propagation, and the
  negative-only polymorphism record around nested `forall`.
- Define one consistent search-model table or equivalent rubric that records,
  for each rule:
  trigger, required evidence, admitted candidate shape, fail-closed reject
  condition, ambiguity behavior, and termination contribution.

### Task 3 - Define anchor-first candidate generation and admissibility

- Require the artifact to define a search order where recursive-shape anchors
  are discovered before downstream consumer choice.
- Require candidate generation to stay lane-bounded and evidence-driven rather
  than globally heuristic:
  - first establish the anchor and its owner / binder frame;
  - then determine whether the case is same-lane local or non-local
    propagation;
  - then admit only candidates with an explicit target / consumer route; and
  - reject neighboring routes instead of reopening them later.
- Lift the named packet guards into general admissibility rules wherever the
  accepted evidence supports it:
  - `rootNonLocalSchemeAliasBaseLike` only if it can be restated as a
    non-local alias-bound / base-like admission rule tied to anchor, owner,
    and consumer alignment;
  - `sameLaneLocalRetainedChildTarget` only if it can be restated as a
    same-lane retained-child admission rule tied to one owner frame and the
    `keepTargetFinal -> targetC` handoff;
  - `boundHasForallFrom` and `not hasForall` only if they can be restated as
    quantified-boundary admissibility rules rather than as packet folklore.
- If any guard cannot be lifted cleanly, require the artifact to record it as
  unresolved blocker debt instead of pretending a broader rule exists.

### Task 4 - Define ambiguity handling and fail-closed rejection

- Spell out the exact situations where recursive inference is forbidden even
  if an anchor exists:
  - nested-`forall` or quantified crossings without accepted positive
    polymorphism evidence;
  - owner-crossing or binder-crossing that breaks the candidate's lawful
    frame;
  - neighboring-route cases such as `schemeBodyTarget`, `rootFinal`,
    `boundTarget`, or other adjacent routes when no explicit alignment rule
    selects them;
  - multiple surviving candidates for one subject after admissibility
    filtering; and
  - any case that would need heuristic ranking, post-hoc repair, fallback, or
    a second interface.
- Tie the rejection policy explicitly to the accepted family matrix:
  ambiguity must satisfy `N1`, unsafe owner / binder violations must satisfy
  `N2`, and forbidden widening must remain `N4` / `N5` territory rather than
  being silently consumed by search.
- Keep the nested-`forall` record honest: without new positive evidence, item
  `4` may define admissibility boundaries around quantified pressure, but it
  may not upgrade that pressure into a positive `P5` success story.

### Task 5 - Define the termination discipline for the inherited acyclic model

- Explain why the proposed search remains finite inside the inherited
  non-cyclic-graph representation:
  - anchors are drawn from a finite acyclic structure;
  - owner / binder ancestry is finite;
  - candidate routes are admitted only from a finite lane-bounded set;
  - rejected routes are not reopened through fallback or alternate
    interfaces; and
  - no cyclic graphs or multi-SCC search are introduced.
- Require one explicit statement of what growth is allowed and what growth is
  forbidden, so boundedness is reviewable rather than implied.
- Tie the argument directly to `N6 termination-pressure` and the item-2
  `non-cyclic-graph = unknown` pressure point without consuming the final
  architecture decision.
- If termination depends on an unsupported assumption or hidden ranking rule,
  require the artifact to record that as blocker debt instead of presenting a
  false boundedness claim.

### Task 6 - End with one bounded item-4 outcome and the docs-only verification note

- End the artifact with one bounded item-4 result only:
  either a principled search / ambiguity / termination model is stated inside
  the inherited boundary with named unresolved blockers, or the current
  evidence is still insufficient and the blockers remain explicit. This is not
  the item-7 architecture decision.
- Cite the reviewer-facing expectations from `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/verification.md`:
  the artifact must define candidate generation, ambiguity handling, rejection
  conditions, and termination discipline; preserve the inherited boundary;
  preserve predecessor continuity; and avoid hidden heuristic widening.
- State that this round is expected to change only docs and that no full
  `cabal build all && cabal test` gate is triggered unless the work escapes
  the authorized docs-only surface.
- Wherever the accepted record does not justify a stronger claim, require the
  artifact to fail closed by naming blocker debt or later-item ownership
  rather than broadening the round.
