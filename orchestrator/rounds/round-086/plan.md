# Round 086 Plan (`item-5` Reconstruction Contract)

## Objective

Execute only roadmap item `5` and produce one docs-first artifact at:
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`.

This is the initial `item-5` plan for `attempt-1` with `retry: null`. The
round must define one bounded full-pipeline reconstruction and validation
contract for general automatic iso-recursive inference inside the inherited
acyclic architecture. The artifact must:

- state the exact evidence trail that an admitted item-4 candidate must keep
  review-visible across solver state, elaboration, reification /
  reconstruction, and internal/public output surfaces;
- define what counts as lawful full-pipeline persistence versus solver-only or
  packet-only reading, especially for `P6 reconstruction-visible-output`;
- define fail-closed conditions for recursion that disappears, changes
  mechanism family, changes owner / binder / route interpretation, crosses a
  quantified boundary, or survives only by manual interpretation,
  replay-specific reinterpretation, or fallback-like reasoning;
- define one validation rubric that reviewers can apply without inventing a
  new interface, a new search policy, or a packet-specific rescue story; and
- keep the contract bounded to the two item-4 admitted candidate families and
  the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary.

This round is docs-only and reconstruction-contract-only. It must not edit
production code, tests, public surfaces, executables, `mlf2.cabal`,
`orchestrator/rounds/round-086/state-snapshot.json`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/roadmap.md`,
`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/verification.md`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/retry-subloop.md`, `Bugs.md`,
or any review / merge / predecessor-history artifact.

This round must not silently widen into:

- a representative coverage campaign;
- the final architecture decision;
- a redesign of the item-4 search / admissibility / termination model;
- a new exact packet-selection or implementation slice;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural graphs, multi-SCC search, or cross-family reopening;
- second interfaces; or
- compatibility / convenience / fallback paths.

Current planning read: item `5` is the full-pipeline `P6` gate, not a
coverage or architecture-fork gate. If accepted evidence cannot justify how a
solver-admitted recursive candidate remains review-visible through the full
pipeline, the artifact must record that as blocker debt or fail-closed
rejection rather than counting solver-only or manually reinterpreted evidence
as success.

## Locked Round Context

- Round id: `round-086`
- Roadmap item: `item-5`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: one docs-only full-pipeline reconstruction and
  validation contract for admitted item-4 candidate families
- Active branch: `codex/round-086-item-5-reconstruction-contract`
- Active worktree:
  `.worktrees/round-086`
- Current round review feedback: none yet; no `review.md`,
  `reviews/attempt-<n>.md`, or `review-record.json` exists for `round-086`,
  so this is a full `attempt-1` plan rather than a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `M orchestrator/rounds/round-086/state-snapshot.json` (controller-owned; must remain untouched)
- `?? orchestrator/rounds/round-086/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-086/selection.md`
  already fixes this round to roadmap item `5` only, limits the scope to one
  docs-first reconstruction / validation contract, and explicitly requires a
  concrete evidence trail across solver, elaboration, reification /
  reconstruction, and internal/public output surfaces.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/roadmap.md`
  makes item `5` the next dependency-satisfied item and defines its completion
  notes as one bounded full-pipeline reconstruction and validation contract
  only.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
  supplies the human strategic gate: general recursive inference is not
  credible until inferred recursion survives solver state, elaboration,
  reification / reconstruction, internal/public output surfaces, and a
  reviewable evidence trail.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  is the accepted item-1 contract. Its binding pressure for this round is
  `P6 reconstruction-visible-output`: solver-side recursive success only
  counts if elaborated or reconstructed output still exposes the inferred
  recursive structure in a reviewable form.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
  is the accepted item-3 mechanism map. It leaves reconstruction obligations
  explicitly unresolved and preserves the bounded output facts for the two
  accepted predecessor chains without upgrading either chain into general
  full-pipeline proof.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-search-model.md`
  is the accepted item-4 search model. It fixes the lawful candidate
  vocabulary to exactly two bounded families:
  - non-local alias-bound / base-like admission through
    `baseTarget -> baseC -> targetC`; and
  - same-lane retained-child admission through
    `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
  Item `5` must inherit that vocabulary unchanged and define what it means for
  one admitted candidate from those families to persist through the pipeline.
- `orchestrator/rounds/round-085/review-record.json`
  authoritatively finalized item `4` with
  `final_outcome = search-model-established-with-bounded-admissibility-ambiguity-and-termination-read`.
  Item `5` therefore starts from an accepted search model instead of reopening
  candidate-generation law.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the inherited baseline contract: explicit recursive annotations are
  still the current production baseline, automatic recursive-type inference
  remains unresolved, and the explicit-only / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains mandatory unless a later
  accepted item explicitly changes it.
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  remains the immediate predecessor decision: accepted `N14` preserves bounded
  evidence only and does not authorize a repo-level generality claim, broad
  architectural revision, or silent reinterpretation of predecessor packets as
  full-pipeline success.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/verification.md`
  requires item-5-specific checks proving that the round states how inferred
  recursion must survive solver, elaboration, reconstruction, and output
  surfaces, while preserving the inherited boundary and predecessor
  continuity.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/retry-subloop.md`
  confirms that `retry: null` means this plan is a full `attempt-1` plan, not
  a delta against a recorded `fix_hypothesis`.
- `Bugs.md` still
  lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
  predecessor implementation context only. It does not authorize replay
  reopening, a witness-only rescue path, or a different live subject for this
  round.

## File Map

### Create

- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  - Responsibility: canonical docs-first item-5 artifact defining one bounded
    full-pipeline persistence contract for admitted item-4 candidate
    families, the validation rubric for `P6`-credible output visibility, and
    the fail-closed rules for drift, erasure, quantified crossing, or
    manual-only interpretation.

### Read-Only Evidence

- `orchestrator/rounds/round-086/selection.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/retry-subloop.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-search-model.md`
- `orchestrator/rounds/round-085/review-record.json`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `Bugs.md`

### Preserve Unchanged

- `orchestrator/rounds/round-086/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/retry-subloop.md`
- `Bugs.md`
- `src/`
- `test/`
- `src-public/`
- `app/`
- `mlf2.cabal`
- reviewer-owned history under earlier round directories

## Exact Selected Slice (Exactly One)

The only selected slice is:

define the full-pipeline reconstruction and validation contract for admitted
item-4 recursive candidate families.

This slice is allowed to:

- define a phase-by-phase evidence trail across solver state, elaboration,
  reification / reconstruction, internal output surfaces, public output
  surfaces, and reviewer-visible validation evidence;
- define exactly which candidate facts must remain stable across those phases:
  admitted family, anchor / owner / binder frame, target / consumer route,
  quantified-boundary state, and recursive-visibility status;
- distinguish stable review-visible recursive persistence from
  admitted-but-not-visible or drifted cases;
- define fail-closed rejection for family drift, owner / binder / route drift,
  quantified crossings, recursive disappearance, ambiguity resurfacing, or
  manual-only interpretation; and
- record later-item debt where accepted evidence does not justify a stronger
  full-pipeline claim.

This slice is not allowed to:

- redefine the item-4 search policy or admit new candidate families;
- run the representative coverage campaign;
- make the item-7 architecture decision;
- authorize equi-recursive reasoning, implicit unfolding, cyclic structural
  graphs, multi-SCC search, second interfaces, or fallback behavior;
- upgrade negative-only nested-`forall` evidence into a positive `P5`
  capability claim; or
- count solver-only, witness-only, or packet-manual readings as `P6`
  reconstruction-visible success.

If the contract starts depending on broader search growth, replay reopening,
coverage evidence, or architecture revision to justify a positive read, record
that dependence explicitly as blocker debt or later-item ownership instead of
solving it here.

## Sequential Tasks

### Task 1 - Freeze the item-5 docs-only reconstruction-contract frame

- Create the canonical item-5 artifact at the path above.
- State explicitly that item `5` is docs-only, reconstruction-contract-only,
  and `attempt-1` with `retry: null`.
- Reassert the inherited boundary unchanged:
  - explicit-only recursive annotations remain the current production
    baseline;
  - recursive meaning remains iso-recursive only;
  - no equi-recursive equality or implicit unfolding is authorized;
  - no cyclic structural graph encoding or multi-SCC search is authorized;
  - no second interface is authorized; and
  - no compatibility / convenience / default-path fallback widening is
    authorized.
- State explicitly that the accepted item-3 mechanism map, the accepted
  item-4 search model, and accepted `N14` predecessor continuity supply
  bounded evidence only and do not by themselves prove full-pipeline
  reconstruction.

### Task 2 - Build the bounded phase-and-surface ledger from the controlling docs

- Start from the two item-4 admitted candidate families only:
  - non-local alias-bound / base-like admission through
    `baseTarget -> baseC -> targetC`;
  - same-lane retained-child admission through
    `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
- For each family, capture only the phase-relevant facts that the contract may
  reuse:
  - the recursive-shape anchor;
  - the owner / binder frame that made the candidate lawful;
  - the explicit target / consumer route;
  - the quantified-boundary state inherited from item `4`;
  - the currently accepted output fact from predecessor evidence; and
  - whether that output fact already does or does not expose recursive
    structure in the `P6` sense.
- Define one bounded phase ledger or equivalent rubric with these surfaces
  only:
  - solver admission state;
  - elaboration handoff / result state;
  - reification / reconstruction state;
  - internal output surface;
  - public output surface; and
  - reviewer-visible evidence trail.
- Require each ledger row to state:
  - which facts must persist unchanged;
  - what output shape is acceptable at that phase;
  - what counts as drift, erasure, or reinterpretation; and
  - whether the phase can still contribute to a lawful `P6`-credible success.

### Task 3 - Define the persistence contract and the only lawful success read

- Require the artifact to say that a solver-admitted recursive candidate only
  counts as a full-pipeline success if the same admitted family and the same
  owner / binder / route interpretation survive across all listed phases.
- Require the artifact to define the minimal stability fields that may not
  change mid-pipeline:
  - admitted family identity;
  - anchor ownership / binder interpretation;
  - target / consumer alignment;
  - quantified-boundary-clear status; and
  - whether recursive structure remains review-visible on output surfaces.
- Make `P6` explicit: a positive item-5 success requires review-visible
  recursive structure on output surfaces, not merely an internal or historical
  story that a recursive candidate once existed.
- Force the artifact to treat the accepted non-local alias-bound / base-like
  output fact honestly. Its preserved `TBase (BaseTy "Int")` /
  `containsMu False` read may be bounded predecessor context, but it may not
  be silently re-labeled as reconstruction-visible recursive success unless
  the artifact can justify that classification inside the inherited boundary.
- Require one concise outcome vocabulary such as:
  - stable visible persistence;
  - admitted but not reconstruction-visible / blocker debt; or
  - fail-closed rejection.
  The exact labels may vary, but the contract must separate those outcomes
  clearly.

### Task 4 - Define fail-closed drift and invalidation rules

- Spell out the exact situations where a solver-admitted candidate is no
  longer a lawful full-pipeline recursive result:
  - recursion disappears at a later phase;
  - the candidate changes mechanism family;
  - the owner / binder frame changes;
  - the target / consumer route changes or hops to a neighboring route;
  - a nested `forall`, nested owner, or nested scheme-root crossing appears;
  - internal and public surfaces disagree about whether recursion survived;
  - multiple candidate stories survive and would need ranking or guesswork; or
  - the only remaining justification depends on manual interpretation,
    replay-specific reasoning, witness-only inspection, or fallback-like
    recovery.
- Tie these invalidation rules back to the inherited family matrix without
  widening it:
  - ambiguity remains `N1`;
  - owner / binder / scope violations remain `N2`;
  - fallback- or second-interface-dependent rescue remains `N5`;
  - quantified crossing remains reject-side only unless later accepted
    evidence changes that read.
- Make clear that review-visible validation must fail closed if a case can be
  read as recursive only by reusing packet-local folklore from predecessor
  artifacts.

### Task 5 - Define the reviewer-facing validation procedure

- Require one concrete validation checklist or matrix that reviewers can apply
  to any candidate from the two admitted families:
  - identify the admitted family and the lawful anchor from item `4`;
  - verify that the same owner / binder / route story survives each pipeline
    phase;
  - verify whether recursive structure remains visible on internal and public
    output surfaces;
  - reject cases that need manual reinterpretation or route switching; and
  - record blocker debt when evidence is incomplete rather than treating the
    case as success.
- Tie that procedure directly to `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-005/verification.md` item-5
  expectations:
  the artifact must state how recursion survives solver, elaboration,
  reconstruction, and output surfaces, and how validation distinguishes lawful
  persistence from silent drift.
- Keep the procedure docs-only and review-visible. It must not require a new
  executable, alternate interface, or code-path experiment to make the
  contract intelligible.

### Task 6 - End with one bounded item-5 outcome and the docs-only verification note

- End the artifact with one bounded item-5 result only:
  either a full-pipeline reconstruction / validation contract is stated for
  the inherited boundary with named unresolved blockers, or the current
  evidence is still insufficient and the blockers remain explicit.
- State explicitly which obligations remain later-item debt, especially:
  - representative coverage across the broader family matrix belongs to
    item `6`;
  - any architecture revision question belongs to item `7`; and
  - positive nested-`forall` / `P5` success remains unresolved unless later
    accepted evidence changes that read.
- State that this round is expected to change only docs and that no full
  `cabal build all && cabal test` gate is triggered unless the work escapes
  the authorized docs-only surface.
- Wherever the accepted record does not justify a stronger claim, require the
  artifact to fail closed by naming blocker debt or later-item ownership
  rather than broadening the round.
