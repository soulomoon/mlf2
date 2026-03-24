# Capability Contract And Evaluation Corpus For General Automatic Iso-Recursive Inference

Date: 2026-03-25
Round: `round-082`
Roadmap item: `item-1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repo-level capability definition for general automatic
iso-recursive inference after accepted bounded predecessor evidence
Artifact kind: canonical docs-only capability contract and corpus-definition
artifact

## Stage Contract Freeze

This artifact implements only roadmap item `1` for `round-082`.

Item `1` is docs-only, capability-definition-only, and pre-implementation.
Its job is to define the repo-level target precisely enough for later rounds to
audit, design, and test. It does not prove feasibility, architecture fit,
search correctness, reconstruction correctness, or product posture.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- `orchestrator/state.json`, `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, or `orchestrator/retry-subloop.md` edits;
- `Bugs.md` edits;
- solver experiments, prototype interfaces, or executable changes;
- architectural `keep` / `revise` / `unknown` classification;
- packet-to-mechanism generalization;
- search / ambiguity / termination design;
- reconstruction-contract design;
- a broad coverage campaign; or
- a final architecture decision.

The inherited boundary remains fixed and unchanged:

- explicit recursive annotations remain the current baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding is authorized;
- no multi-SCC handling is authorized;
- no second executable or interface is authorized; and
- no compatibility, convenience, or default-path fallback widening is
  authorized.

## Inherited Continuity And Honest Current Position

The controlling predecessor documents remain:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  for the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph baseline; and
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  for accepted `N14 = continue-bounded`.

Accepted `N14` contributes bounded predecessor evidence only. It preserves one
exact same-lane retained-child `boundVarTarget -> targetC` packet as accepted
continuity, but it does not establish general automatic iso-recursive
inference, does not authorize silent widening from that packet to a repo-level
claim, and does not pre-decide whether the current architecture is sufficient.

The honest current repo position therefore remains:

- credible bounded packet evidence exists;
- automatic recursive-type inference for unannotated programs is still
  unresolved at repo level; and
- the current production baseline remains explicit-only.

## Repo-Level Capability Contract

For this repo, the target claim "general automatic iso-recursive inference"
means:

> On the existing repo pipeline and output surfaces, without explicit source
> recursive annotations and without a second interface or fallback path, the
> implementation can infer iso-recursive type structure for a representative
> family matrix of unannotated programs that stay inside the inherited
> non-equi-recursive and acyclic-graph boundary, and it can expose that
> inferred recursive structure in reviewable elaborated or reconstructed output
> while remaining sound, thesis-faithful, bounded, and explainable.

This definition is intentionally stronger than "another bounded packet works"
and intentionally weaker than "every recursive program is accepted." In this
repo, "general" means a credible repo-level capability across the named family
matrix below, not a claim of universal completeness and not hidden solver-only
success.

### Required separation of states

#### 1. Honest current position

- bounded accepted evidence only;
- no repo-level automatic recursive inference claim yet; and
- explicit-only recursive annotations still define current supported behavior.

#### 2. Target capability claim

A later round may claim repo-level capability only if later evidence shows all
of the following together:

- representative positive families succeed under the inherited boundary;
- representative negative families fail closed or remain explicitly out of
  scope under the inherited boundary;
- inferred recursive structure survives into reviewable output surfaces; and
- the behavior remains sound, thesis-faithful, terminating in a bounded and
  explainable way, and free of silent interface widening.

#### 3. Out-of-scope or still-fail-closed territory

The target claim does not include:

- equi-recursive reasoning or implicit unfolding;
- cyclic structural graph encoding;
- multi-SCC recursive search;
- second-interface or convenience-fallback behavior; or
- ambiguous or unsafe cases that require guessing instead of justified
  rejection.

### Required repo-level input classes

The eventual repo-level claim must cover unannotated programs whose inferred
recursive structure falls within the inherited iso-recursive boundary and whose
evidence spans at least these classes:

- local recursive-shape discovery;
- non-local propagation from recursive evidence to the consuming target;
- retained-child and alias-bound / owner-sensitive placement;
- binder-sensitive placement and nested binding structure;
- polymorphism and nested-`forall` interaction; and
- cases where the inferred recursive result must be visible after
  elaboration / reconstruction.

This artifact defines the contract only. It does not prove that the current
architecture can satisfy it, and it does not define the implementation or test
mechanism that later rounds must choose.

## Evaluation Corpus Family Matrix

The corpus below is the minimum reviewable family matrix for later roadmap
items. Item `1` defines the families and their required dispositions. Later
items own the concrete examples, mechanism design, and campaign evidence.

### Positive families that the later claim must satisfy

| Family | What the family must cover | Required disposition |
| --- | --- | --- |
| `P1 local-recursive-shape` | Unannotated programs where the recursive shape can be discovered and placed from local evidence without widening to forbidden semantics. | `must succeed` |
| `P2 non-local-propagation` | Programs where recursive evidence is discovered in one location but must reach a different consumer or target lane without becoming packet-specific folklore. | `must succeed` |
| `P3 retained-child-owner-sensitive` | Retained-child, alias-bound, or otherwise owner-sensitive cases where recursive placement depends on who owns the retained structure and where the target sits. | `must succeed` |
| `P4 binder-sensitive-placement` | Programs where binder placement, retained ancestry, or nested binding structure matters to whether the inferred recursive type is lawful and reviewable. | `must succeed` |
| `P5 polymorphism-nested-forall` | Recursive inference cases that interact with polymorphism or nested `forall` structure while still staying inside the inherited baseline. | `must succeed` |
| `P6 reconstruction-visible-output` | Programs where solver-side recursive success only counts if elaborated or reconstructed output still exposes the inferred recursive structure in a reviewable form. | `must succeed` |

### Negative and fail-closed families that the later claim must preserve

| Family | What the family must cover | Required disposition |
| --- | --- | --- |
| `N1 ambiguity-reject` | Cases with competing recursive candidates or placements where the implementation cannot justify one answer without guesswork. | `must fail closed` |
| `N2 unsoundness-guard` | Cases where recursive inference would violate ownership, binder discipline, scope discipline, soundness assumptions, or other thesis-backed invariants. | `must fail closed` |
| `N3 equi-recursive-required` | Cases that would need equi-recursive equality, implicit unfolding, or other semantics outside the inherited iso-recursive boundary. | `out of scope under current baseline` |
| `N4 cyclic-or-multi-scc-required` | Cases whose only plausible route would require cyclic structural graphs, multi-SCC search, or a richer recursion representation than the inherited baseline permits. | `out of scope under current baseline` |
| `N5 second-interface-or-fallback-required` | Cases that would succeed only by adding a new executable path, alternate interface, or default-path fallback behavior. | `out of scope under current baseline` |
| `N6 termination-pressure` | Cases that place heavy pressure on search or recursive-candidate growth; later work must show either bounded success or bounded rejection without divergence. | `must stay bounded or fail closed` |

The family matrix is representative, not exhaustive. A later claim cannot pick
only convenient examples from one family and treat that as evidence for the
whole matrix.

## Later Success And Failure Criteria

These are later claim gates, not proof delivered by item `1`.

### Later success criteria

A later roadmap item may claim repo-level general automatic iso-recursive
inference only if the accumulated evidence shows:

1. broad success across the positive family matrix rather than one narrow
   packet;
2. fail-closed or explicitly out-of-scope behavior across the negative family
   matrix;
3. soundness-preserving behavior, with no evidence that recursive inference
   violates the repo's typing or binding invariants;
4. thesis-faithful behavior, or an explicit recorded deviation explaining why
   the thesis boundary changed and under which later accepted decision;
5. bounded, explainable termination behavior rather than unexplained search
   growth; and
6. reviewable reconstructed or elaborated output that still exposes the
   inferred recursive structure.

### No-claim or stop conditions

Later rounds must stop short of a general capability claim if any of the
following remain true:

1. the target remains too vague to test as a concrete repo-level contract;
2. satisfying the matrix would require forbidden widening of semantics,
   representation, interfaces, or fallback behavior;
3. ambiguity cannot remain fail-closed and instead forces heuristic guessing;
4. termination cannot be kept bounded and explained; or
5. inferred recursion cannot be surfaced coherently on reviewable output
   surfaces.

## Later-Roadmap Ownership And Non-Authorization

This artifact does not consume the responsibilities of later roadmap items.
Ownership remains:

- item `2`: audit the inherited constraints and classify them as `keep`,
  `revise`, or `unknown`;
- item `3`: generalize accepted bounded packets into reusable mechanism
  families;
- item `4`: design candidate generation, ambiguity handling, rejection policy,
  and termination discipline;
- item `5`: define the reconstruction and full-pipeline validation contract;
- item `6`: run the representative coverage and feasibility campaign; and
- item `7`: make the architecture decision and successor-plan choice.

Until those later items complete, this round does not authorize implementation
changes, prototype solver behavior, elaboration changes, reconstruction
changes, or test-suite expansion.

## Docs-Only Verification Note

This round changes only documentation:

- this capability contract and evaluation corpus artifact; and
- the round-local `orchestrator/rounds/round-082/implementation-notes.md`.

Because the round does not touch `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`, no full `cabal build all && cabal test` gate is triggered by
item `1`. Later reviewer checks should therefore focus on docs-diff
correctness, inherited-boundary continuity, and whether this artifact makes the
capability target, family matrix, and later claim gates concrete enough to
audit.
