# Round 082 Plan (`item-1` Capability Contract And Evaluation Corpus)

## Objective

Execute only roadmap item `1` and produce one docs-first artifact at:
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`.

This is the initial `item-1` plan for `attempt-1` with `retry: null`. The
round must turn the refreshed strategic roadmap into a concrete repo-level
capability contract for general automatic iso-recursive inference. The artifact
must:

- define what "general automatic iso-recursive inference" means in this repo;
- distinguish accepted bounded packet evidence from a general capability claim;
- define the required positive and negative evaluation corpus families; and
- state explicit success / failure criteria around soundness,
  thesis-faithfulness, termination, and explainability.

This round is docs-only and capability-definition-only. It must not edit
production code, tests, public surfaces, executables, `mlf2.cabal`,
`orchestrator/rounds/round-082/state-snapshot.json`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/roadmap.md`, `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/verification.md`,
`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/retry-subloop.md`, `Bugs.md`, or any review / merge /
predecessor-history artifact.

This round must not silently widen into:

- architectural constraint classification (`keep` / `revise` / `unknown`);
- packet-to-mechanism generalization;
- search / ambiguity / termination design;
- reconstruction-contract design;
- a coverage campaign;
- a final architecture decision;
- equi-recursive reasoning;
- cyclic structural graphs or multi-SCC search;
- second interfaces; or
- compatibility / convenience / fallback paths.

Current planning read: item `1` should define a testable repo-level target and
family matrix, not claim that the target has already been reached and not
pre-decide whether the current architecture is sufficient. Those questions
belong to later roadmap items.

## Locked Round Context

- Round id: `round-082`
- Roadmap item: `item-1`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: one capability-definition slice for general automatic
  iso-recursive inference after accepted `N14 = continue-bounded`
- Active branch: `codex/round-082-item-1-capability-contract`
- Active worktree:
  `.worktrees/round-082`
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `M orchestrator/rounds/round-082/state-snapshot.json` (controller-owned; must remain untouched)
- `?? orchestrator/rounds/round-082/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-082/selection.md`
  already fixes this round to roadmap item `1` only, keeps the round
  docs-first, and explicitly forbids implementation, solver experiments,
  roadmap / state edits, bug-tracker edits, and predecessor-history rewrites.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/roadmap.md`
  makes item `1` the first pending dependency-free item and defines its
  completion notes as capability contract plus evaluation corpus only.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
  supplies the human-level strategic gate: define the target precisely enough
  to test and build the corpus that can distinguish bounded packet wins from a
  credible general capability.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the inherited baseline contract: explicit recursive annotations are
  supported, automatic recursive-type inference is unresolved and disabled, and
  the explicit-only / non-equi-recursive / non-cyclic-graph / no-fallback
  boundary remains mandatory.
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  remains the immediate predecessor result: one exact same-lane retained-child
  `boundVarTarget -> targetC` packet is accepted bounded evidence only, the
  long-horizon goal remains unresolved, and no new successor lane is yet
  authorized or bound.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/verification.md`
  requires item-1-specific capability-definition checks proving that the round
  defines a concrete target, representative corpus, and explicit success /
  failure conditions rather than aspiration language only.
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/retry-subloop.md`
  confirms that `retry: null` means this plan is a full `attempt-1` plan, not
  a delta against a recorded `fix_hypothesis`.
- `Bugs.md` still
  lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
  predecessor context only and does not authorize replay reopen or a different
  live subject here.

## File Map

### Create

- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  - Responsibility: canonical docs-first item-1 artifact defining the repo's
    target capability contract, the required positive / negative evaluation
    corpus families, the explicit later success / failure criteria, and the
    inherited boundaries that remain unchanged during this strategic round.

### Read-Only Evidence

- `orchestrator/rounds/round-082/selection.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/retry-subloop.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
- `Bugs.md`

### Preserve Unchanged

- `orchestrator/rounds/round-082/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/verification.md`
- `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-001/retry-subloop.md`
- `Bugs.md`
- `src/`
- `test/`
- `src-public/`
- `app/`
- `mlf2.cabal`
- reviewer-owned history under earlier round directories

## Exact Selected Slice (Exactly One)

The only selected slice is:

define the repo-level capability contract for general automatic iso-recursive
inference, including the evaluation corpus families required to make that
contract testable.

This slice is allowed to define expected outcome families and acceptance
criteria because item `1` is not concrete unless those are named. It is not
allowed to:

- classify inherited constraints as `keep`, `revise`, or `unknown`;
- argue that the current architecture is already plausible or implausible;
- extract a reusable mechanism map from accepted packet history;
- define candidate generation, ambiguity resolution, or termination strategy;
- define the reconstruction contract in detail;
- run or design the broad coverage campaign; or
- choose the final architecture outcome.

If the artifact starts needing any of the above to stay coherent, record that
dependence as a later-item requirement instead of solving it here.

## Sequential Tasks

### Task 1 - Freeze the item-1 docs-only capability-definition contract

- Create the canonical item-1 artifact at the path above.
- State explicitly that item `1` is docs-only, capability-definition-only, and
  pre-implementation.
- Reassert the inherited boundary unchanged:
  - explicit-only baseline still current;
  - iso-recursive meaning only;
  - no equi-recursive reasoning;
  - no cyclic structural graph encoding;
  - no multi-SCC search;
  - no second interface; and
  - no fallback widening.
- State explicitly that accepted `N14` contributes bounded predecessor
  evidence only and does not itself establish general capability.

### Task 2 - Define the repo-level capability contract concretely

- Give one explicit definition of what "general automatic iso-recursive
  inference" means in this repo. Do not leave "general" ambiguous.
- Separate three states clearly:
  - honest current position (`bounded accepted evidence only`);
  - target capability claim (`what would have to be true later`); and
  - out-of-scope / still-fail-closed territory under the inherited boundary.
- Define what classes of unannotated programs the eventual claim must cover at
  the repo level.
- Define what user-visible result obligation the claim carries:
  inferred recursive structure must survive into reviewable
  elaborated / reconstructed output rather than living only in hidden solver
  state.
- State explicitly that this artifact defines the contract only; it does not
  prove feasibility, architecture fit, search correctness, or reconstruction
  correctness.

### Task 3 - Define the evaluation corpus families as a reviewable matrix

- Include a corpus section organized by named families rather than examples
  only.
- Cover, at minimum, the required positive families:
  - local recursive-shape inference families;
  - non-local propagation families;
  - retained-child and alias-bound / owner-sensitive families;
  - binder-sensitive placement families;
  - polymorphism / nested-`forall` interaction families; and
  - reconstruction-visible output families.
- Cover, at minimum, the required negative or fail-closed families:
  - ambiguity cases that must reject rather than guess;
  - unsoundness-guard cases;
  - cases that would require equi-recursive reasoning;
  - cases that would require cyclic structural graphs or multi-SCC handling;
  - cases that would need a second interface or fallback path; and
  - termination-pressure cases that must stay bounded or reject.
- For each family, record an expected disposition such as `must succeed`,
  `must fail closed`, or `out of scope under the current baseline`. The matrix
  must be concrete enough for later rounds to audit against it.
- Keep this as a family-definition matrix only. Do not run the coverage
  campaign or select convenience subsets here.

### Task 4 - State explicit later success and failure criteria

- Translate the strategic roadmap into later claim gates that are explicit and
  reviewable:
  - broad positive-corpus success;
  - negative-corpus fail-closed behavior;
  - soundness preservation;
  - thesis-faithfulness preservation or explicit deviation record;
  - bounded, explainable termination behavior; and
  - reviewable reconstructed output.
- State the corresponding stop / no-claim conditions:
  - target too vague to test;
  - required evidence would force forbidden widening;
  - ambiguity cannot remain fail-closed;
  - termination cannot be bounded or explained; or
  - inferred recursion cannot be exposed coherently on output surfaces.
- Make clear that these are claim criteria for later roadmap items, not proof
  delivered by item `1`.

### Task 5 - Keep later-roadmap ownership and non-authorization explicit

- State explicitly that item `2` owns architectural constraint audit,
  especially any `keep` / `revise` / `unknown` classification.
- State explicitly that item `3` owns packet-to-mechanism generalization.
- State explicitly that item `4` owns search, ambiguity handling, and
  termination-model design.
- State explicitly that item `5` owns the reconstruction contract.
- State explicitly that item `6` owns the representative coverage campaign.
- State explicitly that item `7` owns the architecture decision.
- Keep all implementation, prototype, solver, elaboration, and test changes
  out of scope for this round.

### Task 6 - Make reviewer-visible checks easy for a docs-only round

- Structure the artifact so the reviewer can verify, in order:
  - inherited baseline and accepted `N14` continuity;
  - honest current position versus target capability;
  - concrete capability definition;
  - representative positive / negative corpus families;
  - explicit later success / failure criteria; and
  - explicit non-authorizations preserving the live boundary.
- Include a brief docs-only verification note in the artifact: no full Cabal
  gate is triggered because this round changes only docs and does not touch
  `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not pre-approve the artifact. Review must still decide whether the
  written contract is concrete enough to satisfy item `1`.
