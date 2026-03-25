# Round 084 Plan (`item-3` Mechanism Map)

## Objective

Execute only roadmap item `3` and produce one docs-first artifact at:
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`.

This is the initial `item-3` plan for `attempt-1` with `retry: null`. The
round must generalize accepted bounded packet history into a reusable
mechanism map. The artifact must:

- explain at least the accepted non-local `baseTarget -> baseC` packet and the
  accepted same-lane retained-child `boundVarTarget -> targetC` packet through
  a shared mechanism vocabulary rather than through packet-specific folklore;
- cover the roadmap-required mechanism families only:
  recursive-shape discovery, binder / owner placement, target / consumer
  alignment, local versus non-local propagation, interaction with polymorphism
  and instantiation, reconstruction obligations, and fail-closed ambiguity /
  unsafe-case handling;
- use those mechanisms to test the accepted item-2 pressure point: whether
  representative `P2`-`P5` family pressure appears explainable inside the
  inherited acyclic (`non-cyclic-graph`) model without packet-specific
  exceptions; and
- record which mechanism obligations remain missing or only negatively
  evidenced, failing closed wherever the accepted record does not justify a
  stronger claim.

This round is docs-only and mechanism-map-only. It must not edit production
code, tests, public surfaces, executables, `mlf2.cabal`,
`orchestrator/rounds/round-084/state-snapshot.json`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/roadmap.md`,
`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/verification.md`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/retry-subloop.md`, `Bugs.md`,
or any review / merge / predecessor-history artifact.

This round must not silently widen into:

- another exact packet-selection exercise;
- search / candidate-generation / termination design;
- a full reconstruction contract;
- a coverage campaign;
- a final architecture decision;
- equi-recursive reasoning;
- cyclic structural graphs or multi-SCC search;
- second interfaces; or
- compatibility / convenience / fallback paths.

Current planning read: item `3` is a mechanism-extraction gate, not a search,
coverage, or architecture-fork gate. If the accepted evidence does not explain
`P2`-`P5` cleanly inside the inherited acyclic model, the artifact must record
that gap as unresolved mechanism debt for later items rather than solving it
here.

## Locked Round Context

- Round id: `round-084`
- Roadmap item: `item-3`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: one docs-only reusable mechanism map for general
  automatic iso-recursive inference after accepted bounded predecessor packets
- Active branch: `codex/round-084-item-3-mechanism-map`
- Active worktree:
  `.worktrees/round-084`
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `M orchestrator/rounds/round-084/state-snapshot.json` (controller-owned; must remain untouched)
- `?? orchestrator/rounds/round-084/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-084/selection.md`
  already fixes this round to roadmap item `3` only, limits the scope to a
  docs-first mechanism map, and forbids implementation, roadmap / state edits,
  bug-tracker edits, and predecessor-history rewrites.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/roadmap.md`
  makes item `3` the next dependency-satisfied item and defines its completion
  notes as a reusable mechanism map plus an acyclic-model pressure read only.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
  supplies the human strategic gate: stop choosing another convenient lane and
  instead explain accepted packet history through reusable mechanism families.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  is the accepted item-1 contract that defines the repo-level target and the
  minimum family matrix (`P1`-`P6`, `N1`-`N6`) that this mechanism map must
  read against.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  is the accepted item-2 audit. Its controlling open pressure is unchanged:
  `non-cyclic-graph = unknown`, with the heaviest unresolved load on `P2`
  through `P5` plus bounded `N6`.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the inherited baseline contract: explicit recursive annotations are
  still the current baseline, automatic recursive-type inference remains
  unresolved, and the explicit-only / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains mandatory unless a later
  accepted item explicitly changes it.
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-verification-gate.md`,
  and
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-next-cycle-decision-gate.md`
  preserve one accepted predecessor packet chain for the exact non-local
  alias-bound / base-like `baseTarget -> baseC` route.
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-exact-target-bind.md`,
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-implementation-slice.md`,
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-verification-gate.md`,
  and
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  preserve the later accepted predecessor packet chain for the exact same-lane
  retained-child `boundVarTarget -> targetC` route.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/verification.md`
  requires item-3-specific checks proving that the round explains multiple
  accepted packets through reusable mechanism families rather than by selecting
  another narrow packet.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/retry-subloop.md`
  confirms that `retry: null` means this plan is a full `attempt-1` plan, not
  a delta against a recorded `fix_hypothesis`.
- `Bugs.md` still
  lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
  predecessor implementation context only and does not authorize a code-path
  detour or a different live subject for this round.

## File Map

### Create

- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
  - Responsibility: canonical docs-first item-3 artifact extracting a bounded
    reusable mechanism map from accepted predecessor packet chains, tying that
    map to `P2`-`P5` pressure under the inherited acyclic boundary, and
    naming unresolved mechanism obligations without consuming later-item work.

### Read-Only Evidence

- `orchestrator/rounds/round-084/selection.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/retry-subloop.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-verification-gate.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-next-cycle-decision-gate.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-exact-target-bind.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-implementation-slice.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-verification-gate.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `Bugs.md`

### Preserve Unchanged

- `orchestrator/rounds/round-084/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/retry-subloop.md`
- `Bugs.md`
- `src/`
- `test/`
- `src-public/`
- `app/`
- `mlf2.cabal`
- reviewer-owned history under earlier round directories

## Exact Selected Slice (Exactly One)

The only selected slice is:

generalize the accepted bounded packet history into a reusable mechanism map.

This slice is allowed to:

- define a shared mechanism vocabulary for the accepted predecessor packets;
- map the accepted `baseTarget -> baseC` and `boundVarTarget -> targetC`
  chains onto that vocabulary;
- tie the resulting mechanisms to the item-1 family matrix, especially
  `P2`-`P5`, while preserving the item-2 read that
  `non-cyclic-graph = unknown`;
- state which parts of the accepted packet history look reusable versus
  packet-specific;
- record mechanism-level reconstruction obligations only insofar as they are
  needed to explain the accepted packets; and
- record mechanism-level fail-closed boundaries only insofar as they explain
  why unsafe or nested crossings stayed rejected.

This slice is not allowed to:

- select a new exact bind or implementation slice;
- define candidate generation or ranking;
- define ambiguity-resolution or rejection policy beyond mechanism-level
  fail-closed observations from the accepted packets;
- define termination strategy;
- define the full reconstruction / validation contract;
- run the representative coverage campaign;
- make the final architecture fork decision; or
- treat accepted bounded packets as proof that the repo already has general
  automatic iso-recursive inference.

If the mechanism map starts depending on search, termination, full
reconstruction, or a coverage campaign to justify a claim, record that
dependence explicitly as later-item debt instead of solving it here.

## Sequential Tasks

### Task 1 - Freeze the item-3 docs-only mechanism-map frame

- Create the canonical item-3 artifact at the path above.
- State explicitly that item `3` is docs-only, mechanism-map-only, and
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
- State explicitly that the round consumes accepted bounded predecessor
  packets as evidence only and does not promote them into a repo-level general
  capability claim.

### Task 2 - Reconstruct the accepted packet inventory that the mechanism map may use

- Build one concise predecessor-evidence inventory containing exactly the
  packet chains this round may generalize:
  - the accepted non-local alias-bound / base-like
    `baseTarget -> baseC` chain (`N4` through `N7`);
  - the accepted same-lane retained-child
    `boundVarTarget -> targetC` chain (`N11` through `N14`).
- For each chain, capture only the mechanism-relevant facts:
  - where the recursive shape is anchored;
  - who owns the bound or retained child;
  - what the selected target / consumer pair is;
  - whether the packet is local or non-local;
  - what polymorphism / nested-`forall` condition is present;
  - what reviewer-visible reconstruction or output fact was preserved; and
  - what fail-closed contrast or excluded route the accepted artifact records.
- Treat open `BUG-2026-03-16-001` as predecessor implementation context only,
  not as packet evidence and not as authority to reopen replay or `InstBot`.

### Task 3 - Define the reusable mechanism-map schema before making claims

- Define one bounded table or equivalent schema with these mechanism families
  only:
  - recursive-shape discovery;
  - binder / owner placement;
  - target / consumer alignment;
  - local versus non-local propagation;
  - interaction with polymorphism and instantiation;
  - reconstruction obligations; and
  - fail-closed ambiguity / unsafe-case handling.
- For each mechanism family, require the artifact to record:
  - which accepted packet(s) witness it;
  - which item-1 families it touches (`P2`-`P5` at minimum);
  - whether the current evidence is positive, negative-only, or unresolved;
  - what aspect of the inherited acyclic model it appears to support or
    pressure; and
  - which later roadmap item owns any missing justification.
- Keep the schema mechanism-level. Do not let "reconstruction obligations"
  become the full item-5 contract, and do not let "ambiguity handling" become
  the full item-4 search policy.

### Task 4 - Map accepted packet history onto the mechanism schema

- Explain the accepted `baseTarget -> baseC` packet through the mechanism
  schema:
  - non-local propagation from a scheme-root alias-bound / base-like anchor;
  - owner / bound placement through the preserved alias-bound root;
  - target / consumer alignment through `baseTarget` and downstream `targetC`;
  - the packet's evidence for reusable non-local propagation inside the
    acyclic model;
  - any reconstruction or visibility obligation the accepted artifacts already
    make review-visible; and
  - the packet's fail-closed exclusions that keep local lanes, replay reopen,
    cyclic structure, and fallback widening out of scope.
- Explain the accepted `boundVarTarget -> targetC` packet through the same
  schema:
  - same-lane retained-child recursive-shape discovery under one owner frame;
  - binder / owner placement through `boundVarTargetRoot`,
    `boundHasForallFrom`, and the retained child;
  - target / consumer alignment through `keepTargetFinal` and downstream
    `targetC`;
  - the packet's evidence for binder-sensitive placement inside the acyclic
    model;
  - the accepted nested-`forall` fail-closed contrast as negative-only
    evidence for the polymorphism / instantiation mechanism rather than as a
    positive `P5` success claim; and
  - any reconstruction-visible obligation already implied by the accepted
    output-facing evidence.
- Require one direct comparison section that names what the two packet chains
  share, where they diverge, and whether those differences still look like a
  small reusable mechanism set or like packet-specific exceptions.

### Task 5 - Use the mechanism map to test the item-2 acyclic-model pressure point

- Read the finished mechanism map back against the accepted item-2 open
  question: whether representative `P2`-`P5` family pressure appears
  explainable inside the inherited acyclic model without packet-specific
  exceptions.
- Force the artifact to make only bounded mechanism-level reads:
  - where the accepted packets provide credible support for `P2` or `P3`;
  - where `P4` or `P5` remain only partially explained, negatively evidenced,
    or unresolved;
  - where the accepted record currently shows mechanism reuse; and
  - where the current record still depends on packet-specific guards.
- If the accepted record does not justify a clean explanation for a family,
  record that family as unresolved mechanism debt. Do not upgrade negative-only
  nested-`forall` evidence into a positive polymorphism success claim.
- End with one bounded item-3 outcome statement only:
  the mechanism map either partially explains the current packet history
  inside the acyclic model while leaving named debts, or it shows that the
  current evidence still looks packet-specific. This is not the item-7
  architecture decision.

### Task 6 - Add the docs-only verification note for this round

- State that this round is expected to change only docs and that no full
  `cabal build all && cabal test` gate is triggered unless the work escapes
  the authorized docs-only surface.
- Cite the reviewer-facing expectations from `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-003/verification.md`:
  the artifact must generalize multiple accepted packets into reusable
  mechanism families, preserve the inherited boundary, preserve predecessor
  continuity, and name later-item dependencies instead of silently widening.
- If any needed claim cannot be justified from the accepted evidence, the
  artifact must fail closed by marking the mechanism or family read unresolved
  or negative-only instead of expanding the round.
