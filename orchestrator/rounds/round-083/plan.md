# Round 083 Plan (`item-2` Architectural Constraint Audit)

## Objective

Execute only roadmap item `2` and produce one docs-first artifact at:
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`.

This is the initial `item-2` plan for `attempt-1` with `retry: null`. The
round must audit the inherited architectural constraints against the accepted
item-1 capability contract and family matrix. The artifact must:

- classify `iso-recursive`, `non-equi-recursive`, `non-cyclic-graph`, and
  `no-fallback` as `keep`, `revise`, or `unknown`;
- state which `P1`-`P6` and `N1`-`N6` families each constraint appears to
  support, block, or leave unresolved;
- state whether general automatic iso-recursive inference still appears
  plausible inside the current architecture without silently revising
  semantics, representation, interfaces, or fallback behavior; and
- cite the inherited baseline contract, the accepted item-1 capability
  contract, and accepted `N14` predecessor continuity explicitly.

This round is docs-only and constraint-audit-only. It must not edit production
code, tests, public surfaces, executables, `mlf2.cabal`,
`orchestrator/rounds/round-083/state-snapshot.json`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/roadmap.md`,
`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/verification.md`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/retry-subloop.md`, `Bugs.md`,
or any review / merge / predecessor-history artifact.

This round must not silently widen into:

- packet-to-mechanism generalization;
- search / ambiguity / termination design;
- reconstruction-contract design;
- a coverage campaign;
- a final architecture decision;
- equi-recursive reasoning;
- cyclic structural graphs or multi-SCC search;
- second interfaces; or
- compatibility / convenience / fallback paths.

Current planning read: item `2` is an audit gate, not a redesign gate. If the
audit finds pressure on the current architecture, it must record that pressure
as `revise` or `unknown` and hand the open questions to later roadmap items
instead of solving them inside this round.

## Locked Round Context

- Round id: `round-083`
- Roadmap item: `item-2`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: one docs-only audit of the inherited architectural
  constraints against the accepted repo-level capability contract
- Active branch: `codex/round-083-item-2-constraint-audit`
- Active worktree:
  `.worktrees/round-083`
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `M orchestrator/rounds/round-083/state-snapshot.json` (controller-owned; must remain untouched)
- `?? orchestrator/rounds/round-083/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-083/selection.md`
  already fixes this round to roadmap item `2` only, limits the scope to a
  docs-first constraint audit, and forbids implementation, roadmap / state
  edits, bug-tracker edits, and predecessor-history rewrites.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/roadmap.md`
  makes item `2` the next dependency-satisfied item and defines its completion
  notes as a four-constraint audit plus one bounded plausibility statement
  only.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
  supplies the human strategic gate: re-evaluate the inherited constraints,
  especially the high-risk `non-cyclic-graph` boundary, before mechanism or
  search design.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  is the accepted item-1 contract that defines the repo-level target and the
  minimum family matrix (`P1`-`P6`, `N1`-`N6`) against which this audit must
  reason.
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
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/verification.md`
  requires item-2-specific checks proving that the round classifies the
  inherited boundaries as `keep`, `revise`, or `unknown` and explicitly states
  whether the current architecture still appears plausible.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/retry-subloop.md`
  confirms that `retry: null` means this plan is a full `attempt-1` plan, not
  a delta against a recorded `fix_hypothesis`.
- `Bugs.md` still
  lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
  predecessor implementation context only and does not authorize a code-path
  detour or a different live subject for this round.

## File Map

### Create

- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  - Responsibility: canonical docs-first item-2 artifact classifying the four
    inherited architectural constraints against the accepted capability
    contract, recording per-family support / block / unresolved reads, and
    ending with one bounded plausibility statement about the current
    architecture.

### Read-Only Evidence

- `orchestrator/rounds/round-083/selection.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/retry-subloop.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `Bugs.md`

### Preserve Unchanged

- `orchestrator/rounds/round-083/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/retry-subloop.md`
- `Bugs.md`
- `src/`
- `test/`
- `src-public/`
- `app/`
- `mlf2.cabal`
- reviewer-owned history under earlier round directories

## Exact Selected Slice (Exactly One)

The only selected slice is:

audit the inherited architectural constraints against the accepted repo-level
capability contract.

This slice is allowed to:

- classify only the four named inherited constraints from item `2`;
- tie each classification to the accepted `P1`-`P6` / `N1`-`N6` family matrix;
- state one bounded architecture-plausibility read for the current model; and
- record what remains unresolved for later roadmap items.

This slice is not allowed to:

- add a new fifth audit axis or silently convert the preserved
  `no-second-interface` boundary into a separate roadmap item-2
  classification;
- define reusable mechanism families;
- design candidate generation, ambiguity resolution, or termination strategy;
- define the reconstruction contract;
- run the representative coverage campaign;
- make the final architecture fork decision; or
- treat accepted bounded packets as proof that the repo already has general
  automatic recursive inference.

If the audit starts depending on later-item work to justify a claim, record the
dependence explicitly as `unknown` or as a later-item requirement instead of
solving it here.

## Sequential Tasks

### Task 1 - Freeze the item-2 docs-only audit frame

- Create the canonical item-2 artifact at the path above.
- State explicitly that item `2` is docs-only, audit-only, and `attempt-1`
  with `retry: null`.
- Reassert the inherited boundary unchanged:
  - explicit-only recursive annotations remain the current production
    baseline;
  - recursive meaning remains iso-recursive only;
  - no equi-recursive equality or implicit unfolding is authorized;
  - no cyclic structural graph encoding or multi-SCC search is authorized;
  - no second interface is authorized; and
  - no compatibility / convenience / default-path fallback widening is
    authorized.
- State explicitly that accepted `N14` contributes bounded predecessor
  continuity only and does not pre-clear the current architecture as generally
  sufficient.

### Task 2 - Build the audit rubric from the controlling docs

- Pull the repo-level target definition, the positive families (`P1`-`P6`),
  the negative / out-of-scope families (`N1`-`N6`), and the later success /
  no-claim gates from the accepted item-1 artifact.
- Define the meaning of the three allowed classifications for this round:
  - `keep`: the current constraint appears compatible with the item-1 target
    and still looks like a necessary boundary or an acceptable fail-closed
    guard;
  - `revise`: the current constraint itself appears to block required positive
    families or makes the item-1 target implausible without explicit boundary
    change; and
  - `unknown`: the current evidence is not strong enough to classify the
    constraint as clearly compatible or clearly blocking without later
    mechanism, search, reconstruction, or coverage work.
- Establish one consistent audit table shape for all four constraints:
  current meaning, positive-family support, negative-family guard role,
  apparent pressure points, classification, and why the classification does
  not itself become an architecture-decision overreach.

### Task 3 - Audit the four inherited constraints one by one

- `iso-recursive`
  - assess whether the accepted repo-level target is explicitly defined inside
    iso-recursive meaning and which positive families appear compatible with
    that meaning;
  - tie the guard role explicitly to `N3 equi-recursive-required`; and
  - state whether any required positive family already appears to demand more
    than iso-recursive semantics.
- `non-equi-recursive`
  - assess whether preserving the non-equi-recursive boundary still looks
    compatible with the positive family matrix or whether any required family
    appears blocked without equi-recursive reasoning;
  - tie the fail-closed boundary explicitly to `N3`; and
  - distinguish semantic pressure from mere implementation difficulty.
- `non-cyclic-graph`
  - assess whether the inherited acyclic graph representation appears to
    support or threaten the positive families that stress non-local
    propagation, owner / binder sensitivity, nested structure, and
    polymorphism;
  - tie the main blocker pressure explicitly to `N4` and `N6`; and
  - treat this as the highest-risk audit axis called out by the strategic
    roadmap, without redesigning the representation in this round.
- `no-fallback`
  - assess whether the fail-closed production principle still looks compatible
    with the positive family matrix while preserving `N1`, `N2`, `N5`, and
    bounded `N6` obligations;
  - distinguish a production no-fallback principle from later search-model
    work that may still need explicit research discipline; and
  - do not design that search discipline here.

### Task 4 - Record one bounded architecture-plausibility statement

- End the artifact with one explicit current-read statement chosen by the
  audit:
  - general inference still appears plausible inside the current architecture;
  - general inference appears to require explicit architectural revision; or
  - general inference remains unresolved because one or more critical
    constraints stay `unknown`.
- Tie that statement directly to the four classifications and the family
  matrix rather than to ungrounded optimism or pessimism.
- Make clear that this is the item-2 plausibility gate only, not the final
  item-7 architecture decision.
- List the exact later roadmap items that must resolve each remaining
  `unknown` or `revise` pressure point.

### Task 5 - Add the docs-only verification note for this round

- State that this round is expected to change only docs and that no full
  `cabal build all && cabal test` gate is triggered unless the work escapes
  the authorized docs-only surface.
- Cite the reviewer-facing expectations from `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-002/verification.md`:
  the artifact must present the four classifications, the per-family support /
  block / unresolved reads, the bounded plausibility statement, and explicit
  continuity with the baseline contract, accepted item-1 capability contract,
  and accepted `N14`.
- If any needed claim cannot be justified from the read-only evidence, the
  artifact must fail closed by recording `unknown` or by naming the later-item
  dependency instead of broadening the round.
