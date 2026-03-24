# Architectural Constraint Audit For General Automatic Iso-Recursive Inference

Date: 2026-03-25
Round: `round-083`
Roadmap item: `item-2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: docs-only audit of the inherited architectural constraints
against the accepted repo-level capability contract
Artifact kind: canonical docs-only architectural-constraint audit

## Stage Contract Freeze

This artifact implements only roadmap item `2` for `round-083`.

Item `2` is docs-only, audit-only, and `attempt-1` with `retry: null`. Its
job is to classify the inherited `iso-recursive`, `non-equi-recursive`,
`non-cyclic-graph`, and `no-fallback` constraints as `keep`, `revise`, or
`unknown` against the accepted item-1 capability contract and family matrix.
It does not authorize architecture redesign, mechanism design, search-model
design, reconstruction-contract design, a coverage campaign, or a final
architecture decision.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, or `orchestrator/retry-subloop.md`;
- edits to `Bugs.md`;
- a fifth audit axis beyond the four named inherited constraints;
- a second executable or interface;
- compatibility, convenience, or default-path fallback widening;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural graph encoding or multi-SCC search; or
- silent reinterpretation of bounded predecessor packets as repo-level
  capability.

The inherited baseline remains fixed and unchanged, as recorded in
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`:

- explicit recursive annotations remain the current production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized;
- no second interface is authorized; and
- no compatibility, convenience, or default-path fallback widening is
  authorized.

The accepted item-1 contract at
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
defines the target and family matrix for this audit. The accepted `N14`
artifact at
`docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
contributes bounded predecessor continuity only. It preserves one exact
same-lane retained-child `boundVarTarget -> targetC` packet as accepted
evidence, but it does not pre-clear the current architecture as generally
sufficient.

## Audit Inputs And Rubric

This audit is constrained by three controlling documents:

- the inherited baseline contract, which keeps the explicit-only /
  non-equi-recursive / non-cyclic-graph / no-fallback boundary live;
- the accepted item-1 capability contract, which defines the repo-level target
  and the minimum `P1`-`P6` / `N1`-`N6` family matrix; and
- accepted `N14`, which preserves bounded predecessor continuity without
  upgrading that evidence into a generality claim.

All family reads below are contract-level compatibility reads only. They do
not claim that the repo already implements the target families, only whether a
given inherited constraint currently appears to support, block, or leave them
unresolved.

### Classification meanings

| Classification | Meaning in item `2` |
| --- | --- |
| `keep` | The current constraint appears compatible with the accepted item-1 target and still looks like a necessary boundary or acceptable fail-closed guard. |
| `revise` | The current constraint itself appears to block required positive families or makes the item-1 target implausible without an explicit boundary change. |
| `unknown` | The current evidence is not yet strong enough to call the constraint clearly compatible or clearly blocking without later mechanism, search, reconstruction, or coverage work. |

### Audit summary

| Constraint | Classification | Support read | Block read | Unresolved read |
| --- | --- | --- | --- | --- |
| `iso-recursive` | `keep` | Supports the item-1 target as defined and remains compatible with `P1`-`P6`. | No required positive family currently appears to demand more than iso-recursive meaning. | None at the semantic-boundary level. |
| `non-equi-recursive` | `keep` | Preserves the accepted target semantics and keeps `N3` explicit. | No required positive family is yet shown to require equi-recursive reasoning. | Later items still owe evidence that hard cases are implementation/search problems rather than hidden semantic pressure. |
| `non-cyclic-graph` | `unknown` | Supports `P1` and bounded predecessor fragments touching `P2` / `P3`. | No positive family is conclusively blocked yet. | `P2`-`P6` remain materially unresolved, with the heaviest pressure on `P2`-`P5` plus `N6`. |
| `no-fallback` | `keep` | Keeps the repo-level target tied to the existing pipeline and output surfaces and remains compatible with `P1`-`P6` as a production principle. | No required positive family is currently shown to need fallback or a second interface. | Later search discipline remains open under item `4`, but that pressure does not yet classify the principle itself as blocking. |

## Constraint Audit

### `iso-recursive`

| Audit field | Read |
| --- | --- |
| Current meaning | Recursive meaning remains iso-recursive only. Recursive structure is mediated by explicit fold/unfold discipline rather than by equi-recursive equality or implicit unfolding. |
| Positive-family support | Supports `P1`, `P2`, `P3`, `P4`, `P5`, and `P6` at the contract level because the accepted item-1 target is explicitly defined inside iso-recursive meaning. |
| Positive-family block | No required positive family is currently shown to be blocked by iso-recursive meaning itself. |
| Positive-family unresolved | None at the semantic-boundary level. Later items still owe mechanism, search, reconstruction, and coverage evidence, but those are not currently framed as demands for stronger semantics. |
| Negative-family guard role | Keeps `N3 equi-recursive-required` explicit and out of scope under the inherited baseline. |
| Negative-family block | Deliberately blocks only the `N3` territory that would require equi-recursive equality or implicit unfolding. |
| Negative-family unresolved | None specific to this constraint. |
| Apparent pressure points | Current pressure is evidentiary, not semantic. If a later item proves that a required positive family only works with equi-recursive reasoning, that would be a future revision signal, but no such proof exists in the current controlling docs. |
| Classification | `keep` |
| Why this does not overreach | This keeps the inherited semantics because the accepted target itself is bounded to them. It does not claim that the positive families are already implemented or fully evidenced. |

### `non-equi-recursive`

| Audit field | Read |
| --- | --- |
| Current meaning | The repo still forbids equi-recursive equality and implicit unfolding. Recursive compatibility must be expressed inside the inherited iso-recursive discipline rather than by structural type equality. |
| Positive-family support | Supports `P1`, `P2`, `P3`, `P4`, `P5`, and `P6` at the contract level because the accepted item-1 capability contract already defines the target inside the inherited non-equi-recursive boundary. |
| Positive-family block | No required positive family is yet shown to require equi-recursive reasoning as opposed to better mechanism, search, or reconstruction inside the current semantics. |
| Positive-family unresolved | None at the semantic-boundary level. The hard cases remain hard, but the current docs do not justify reclassifying that difficulty as semantic insufficiency. |
| Negative-family guard role | Keeps `N3 equi-recursive-required` fail-closed and prevents silent drift from explicit iso-recursive meaning into equi-recursive equality. |
| Negative-family block | Deliberately blocks the `N3` family only. |
| Negative-family unresolved | None specific to this constraint. |
| Apparent pressure points | Later items must distinguish semantic pressure from implementation difficulty. A difficult `P2`-`P6` case does not by itself prove that the repo needs equi-recursive reasoning. |
| Classification | `keep` |
| Why this does not overreach | This says only that the current semantic boundary still matches the accepted target definition. It does not decide whether the current architecture is otherwise sufficient. |

### `non-cyclic-graph`

| Audit field | Read |
| --- | --- |
| Current meaning | The constraint graph must remain structurally acyclic. Recursive structure is represented through binders and variables rather than graph cycles, and no multi-SCC search is authorized. |
| Positive-family support | Supports `P1 local-recursive-shape` directly, and preserves bounded predecessor continuity touching `P2 non-local-propagation` and `P3 retained-child-owner-sensitive` because accepted predecessor packets, including accepted `N14`, live inside this acyclic representation. |
| Positive-family block | No required positive family is conclusively blocked on the current docs-only evidence. |
| Positive-family unresolved | Leaves `P2`, `P3`, `P4`, `P5`, and `P6` unresolved at repo level. The current docs do not yet show that non-local propagation, owner/binder-sensitive placement, nested `forall`, and reconstruction-visible output can scale inside the acyclic model without packet-specific handling. |
| Negative-family guard role | Keeps `N4 cyclic-or-multi-scc-required` explicit and out of scope under the inherited baseline. |
| Negative-family block | Deliberately blocks only the `N4` territory that would require cyclic graphs, graph-cycle exceptions, or multi-SCC search. |
| Negative-family unresolved | Leaves `N6 termination-pressure` materially unresolved because bounded search and rejection inside the acyclic model are still later-item obligations. |
| Apparent pressure points | This is the highest-risk audit axis named by the strategic roadmap. The apparent pressure sits on `P2` through `P5` and on bounded handling of `N6`: non-local propagation, retained-owner sensitivity, nested binding structure, polymorphism, and reviewable reconstruction may all become awkward if the current representation cannot express the needed recursive relations without hidden search growth or packet-specific logic. |
| Classification | `unknown` |
| Why this does not overreach | Accepted predecessor packets prove only bounded continuity, not general representational sufficiency. But the current docs also do not prove that the acyclic model is impossible. Calling `revise` now would silently consume later mechanism, search, reconstruction, and coverage work that item `2` does not own. |

### `no-fallback`

| Audit field | Read |
| --- | --- |
| Current meaning | The repo must not add a second interface, alternate executable path, convenience shim, or default-path branch that silently inserts inferred recursion. Success must live on the existing pipeline and fail closed otherwise. Item `2` still classifies only the inherited no-fallback production principle, not a new fifth interface axis. |
| Positive-family support | Supports `P1`, `P2`, `P3`, `P4`, `P5`, and `P6` as a production principle because the accepted item-1 target explicitly requires repo-level success on the existing pipeline and output surfaces without fallback or a second interface. |
| Positive-family block | No required positive family is currently shown to require a fallback path or alternate interface. |
| Positive-family unresolved | Leaves the concrete search discipline for `P1`-`P5` and the full-pipeline proof for `P6` to later items, but that is an open design obligation inside the same boundary rather than evidence that the boundary itself blocks the target. |
| Negative-family guard role | Strongly supports `N1 ambiguity-reject`, `N2 unsoundness-guard`, `N5 second-interface-or-fallback-required`, and bounded handling of `N6 termination-pressure` by forbidding guessed success, unsound widening, alternate-interface escape hatches, and silent default-path recursion. |
| Negative-family block | Deliberately blocks the `N5` territory that would require a second interface or fallback path. |
| Negative-family unresolved | Leaves the precise bounded-search discipline for `N6` to item `4`, but preserves the fail-closed obligation already required by the accepted capability contract. |
| Apparent pressure points | The strategic roadmap leaves room for a more explicit research-mode search policy, but item `2` does not authorize designing it here. If later work can only proceed by adding heuristic defaults or side-path interfaces, that would be future evidence for `revise`; the current docs do not show that yet. |
| Classification | `keep` |
| Why this does not overreach | This preserves the production fail-closed principle because the target claim itself depends on it. It does not pre-decide the later search model or the final product posture. |

## Bounded Architecture-Plausibility Read

Current item-2 read: general automatic iso-recursive inference remains
unresolved because one critical inherited constraint, `non-cyclic-graph`,
still classifies as `unknown`.

The classification set driving that read is:

- `iso-recursive = keep`
- `non-equi-recursive = keep`
- `non-cyclic-graph = unknown`
- `no-fallback = keep`

That means:

- the accepted repo-level target still fits inside the inherited
  iso-recursive / non-equi-recursive semantics;
- the fail-closed no-fallback production principle still looks compatible with
  the target contract and negative-family obligations; but
- the current docs do not yet justify a confident claim that the acyclic
  representation can carry representative `P2`-`P6` pressure without forcing
  `N4` or `N6` failure modes.

This is the item-2 plausibility gate only. It is not the final item-7
architecture decision. Item `2` therefore does not say that the current
architecture is already sufficient, and it also does not yet say that
explicit architectural revision is required.

The later roadmap items that must resolve the remaining pressure are:

1. item `3` must generalize accepted bounded packets into reusable mechanisms
   and show whether `P2`-`P5` can be explained inside the acyclic model rather
   than by packet-specific handling;
2. item `4` must define candidate generation, ambiguity rejection, and
   termination discipline so that `N1` and `N6` remain bounded without
   fallback or cyclic/multi-SCC escape hatches;
3. item `5` must define how inferred recursion survives solver,
   elaboration, and reconstructed output so that `P6` is credible inside the
   same architecture;
4. item `6` must run representative `P2`-`P6` plus `N4` / `N6` coverage and
   determine whether the current representation remains viable across the
   family matrix; and
5. item `7` owns the final architecture decision if the `non-cyclic-graph`
   pressure remains `unknown` or becomes an explicit `revise` result after the
   preceding items land.

## Docs-Only Verification Note

This round is expected to change only documentation:

- this canonical item-2 audit artifact; and
- `orchestrator/rounds/round-083/implementation-notes.md`.

Because the round does not touch `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`, no full `cabal build all && cabal test` gate is triggered
unless the diff escapes the authorized docs-only surface.

Reviewer-facing expectations from `orchestrator/verification.md` for this
artifact are:

- it classifies the four inherited constraints as `keep`, `revise`, or
  `unknown`;
- it states which `P1`-`P6` and `N1`-`N6` families each constraint appears to
  support, block, or leave unresolved;
- it records one bounded architecture-plausibility statement tied directly to
  those classifications;
- it preserves explicit continuity with the inherited baseline contract, the
  accepted item-1 capability contract, and accepted `N14`; and
- wherever the current docs do not justify a stronger claim, it fails closed
  by recording `unknown` or by naming the later roadmap item that still owns
  the open question.
