# General Automatic Iso-Recursive Inference Mechanism Map

Date: 2026-03-25
Round: `round-084`
Roadmap item: `item-3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: reusable mechanism map for general automatic iso-recursive
inference after the accepted item-2 architectural audit
Artifact kind: canonical docs-only mechanism map

## Stage Contract Freeze

This artifact implements only roadmap item `3` for `attempt-1` with
`retry: null`.

Item `3` is docs-only and mechanism-map-only. It consumes accepted bounded
predecessor evidence from exactly two packet chains:

- the accepted non-local alias-bound / base-like `baseTarget -> baseC` chain
  (`N4` through `N7`); and
- the accepted same-lane retained-child `boundVarTarget -> targetC` chain
  (`N11` through `N14`).

This artifact does not promote those chains into a repo-level claim that
general automatic iso-recursive inference already exists. They remain bounded
predecessor evidence only.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized;
- no second executable interface is authorized; and
- no compatibility, convenience, or default-path fallback widening is
  authorized.

This artifact does not authorize a new exact packet-selection exercise, search
/ candidate-generation design, a full reconstruction contract, a coverage
campaign, or the final item-7 architecture decision.

## Accepted Packet Inventory

### `N4` through `N7`: non-local alias-bound / base-like `baseTarget -> baseC`

- Recursive-shape anchor: the existing generic `baseTarget` computation for a
  scheme-root alias-bound / base-like root, with the helper packet rebinding
  the canonical `Int` base node to the extracted `AVar` body root.
- Binder / owner placement: the scheme-root alias binder still owns the root,
  and the owned bound is the canonical base-like node reattached through
  `rebindRootTo`.
- Target / consumer pair: `baseTarget` computes `baseC`, and the downstream
  `targetC` consumer selects that packet through the dedicated non-local
  `rootNonLocalSchemeAliasBaseLike` arm. `keepTargetFinal` / `rootFinal`
  remain separate local continuity only.
- Local versus non-local status: non-local propagation only
  (`not rootBindingIsLocalType`), followed by one same-lane downstream
  consumer.
- Polymorphism / instantiation condition: no accepted positive nested-`forall`
  success was recorded here. The preserved quantified local `rootFinal`
  continuity lane remains adjacent context only, not part of the selected
  non-local packet.
- Reconstruction / output fact: the bounded helper assertion still returns
  `TBase (BaseTy "Int")` with `containsMu False`. The accepted story is only
  one inverse-translation-safe alias-bound / binding-flag reconstruction read
  for one owner-binder / owned-bound pair.
- Fail-closed contrast: the accepted local empty-candidate lane, the accepted
  local `rootFinal` continuity lane, replay reopen, cyclic structure, and
  fallback widening all remain excluded.

### `N11` through `N14`: same-lane retained-child `boundVarTarget -> targetC`

- Recursive-shape anchor: `boundVarTargetRoot` is computed from
  `canonicalFinal (schemeBodyTarget targetPresolutionView rootC)`, and the
  selected packet stays inside the same-lane retained-child search.
- Binder / owner placement: one owner frame is kept review-visible through
  `boundVarTargetRoot`, `boundHasForallFrom`, and the retained child itself.
  The packet only admits candidates whose `bndRoot == boundVarTargetRoot` and
  `not hasForall`.
- Target / consumer pair: `sameLaneLocalRetainedChildTarget` feeds the
  local-only `keepTargetFinal` gate, and `targetC` returns that retained child
  through `Just v -> v`. `schemeBodyTarget`, `rootFinal`, and `boundTarget`
  remain neighboring or excluded routes only.
- Local versus non-local status: same-lane local `TypeRef` retained-child
  packet only.
- Polymorphism / instantiation condition: nested `TyForall`, nested owner, and
  nested scheme-root crossings remain fail-closed. The accepted nested-`forall`
  wrapper is negative-only evidence here, not a positive polymorphism success.
- Reconstruction / output fact: the positive example remains recursive
  (`containsMu fallbackTy` stays `True`), and the source guard keeps the
  `keepTargetFinal` / `targetC` route review-visible. A full pipeline
  reconstruction contract was not established.
- Fail-closed contrast: the nested-`forall` wrapper stays rejected
  (`containsMu False`), `schemeBodyTarget` remains neighboring context only,
  and the earlier non-local `baseTarget` packet remains separate predecessor
  evidence rather than an interchangeable consumer route.

Open `BUG-2026-03-16-001` remains predecessor implementation context only. It
does not count as mechanism evidence, and it does not reopen replay or
`InstBot`.

## Mechanism Schema

| Mechanism family | Accepted packet witness(es) | Item-1 families touched | Current evidence | Acyclic-model read | Later item owning missing justification |
| --- | --- | --- | --- | --- | --- |
| Recursive-shape discovery | `N4`-`N7` scheme-root alias-bound / base-like anchor; `N11`-`N14` same-lane retained-child root match | `P2`, `P3`, `P4` | `positive` | Both packets discover a recursive-relevant shape without graph cycles by anchoring the packet before consumer choice. | item `4` for generalized candidate-generation discipline; item `6` for broader family coverage |
| Binder / owner placement | scheme-root alias binder plus owned base-like bound; `boundVarTargetRoot` / `boundHasForallFrom` plus retained child | `P3`, `P4` | `positive` | The current acyclic model can keep owner-sensitive placement review-visible in two bounded families. | item `6` |
| Target / consumer alignment | `baseTarget -> baseC -> targetC`; `boundVarTarget -> keepTargetFinal -> targetC` | `P2`, `P3`, `P4` | `positive` | The accepted packets show one selected target can be carried into one downstream consumer without fallback. | item `4` |
| Local versus non-local propagation | non-local alias-bound packet; same-lane local retained-child packet | `P2`, `P3` | `positive` | The inherited acyclic model has bounded evidence for both a non-local lane and a local same-lane lane under one vocabulary. | item `4` for propagation/search rules; item `6` for coverage |
| Interaction with polymorphism and instantiation | bounded `rootFinal` continuity kept separate in `N4`-`N7`; nested-`forall` wrapper rejected in `N11`-`N14` | `P4`, `P5` | `negative-only / unresolved` | The record shows how the current model rejects unsafe nested-`forall` crossings, but it does not yet show a positive `P5` success inside the acyclic model. | item `4` for admissibility rules; item `6` for representative `P5` evidence |
| Reconstruction obligations | non-local packet keeps one inverse-translation-safe alias-bound / binding-flag story and a `TBase` output fact; retained-child packet keeps one recursive output fact and explicit `targetC` routing | `P2`, `P3`, `P4`, `P6` | `unresolved` | Review-visible output facts survive for the two accepted packets, but full-pipeline recursive reconstruction has not been justified. | item `5` |
| Fail-closed ambiguity / unsafe-case handling | local lanes kept separate in `N4`-`N7`; nested-`forall` / neighboring-route exclusions in `N11`-`N14` | `P4`, `P5` plus `N1`, `N2`, `N4`, `N5` | `partial positive` | The current model has bounded fail-closed exclusions, but it has not yet defined a general competing-candidate or termination policy. | item `4` |

The schema stays mechanism-level only. It does not define item-4 search
policy, item-5 full reconstruction, item-6 campaign coverage, or item-7
architecture choice.

## Mapping The Accepted Packets Onto The Schema

### Non-local alias-bound / base-like `baseTarget -> baseC`

The accepted `baseTarget -> baseC` chain fits the schema as follows:

- Recursive-shape discovery starts from the scheme-root alias-bound /
  base-like anchor, not from an already-local lane and not from a cyclic
  representation.
- Binder / owner placement stays attached to one review-visible owner-binder /
  owned-bound pair: the scheme-root alias binder plus the rebound canonical
  base-like node.
- Target / consumer alignment is explicit: `baseTarget` produces `baseC`, and
  the downstream `targetC` arm consumes exactly that packet.
- Local versus non-local propagation is the main positive read: this is one
  accepted non-local propagation packet inside the inherited acyclic model.
- Interaction with polymorphism and instantiation remains bounded. The
  preserved quantified local `rootFinal` continuity lane is evidence that the
  packet sits near quantified structure, but it is not positive `P5`
  clearance.
- Reconstruction obligations are bounded to the accepted helper-visible facts:
  one inverse-translation-safe alias-bound / binding-flag story and one
  visible output fact, `TBase (BaseTy "Int")` with `containsMu False`.
- Fail-closed handling remains explicit: local empty-candidate and local
  `rootFinal` continuity lanes stay separate, replay reopen stays out of
  scope, and no cyclic or fallback escape hatch is introduced.

This chain therefore gives bounded support for reusable non-local propagation
inside the acyclic model, but only while the owner/bound pair and consumer
ordering remain explicit and packet-guarded.

### Same-lane retained-child `boundVarTarget -> targetC`

The accepted `boundVarTarget -> targetC` chain fits the same schema:

- Recursive-shape discovery begins from `boundVarTargetRoot` and the same-lane
  retained-child search, again without graph cycles.
- Binder / owner placement stays inside one owner frame through
  `boundVarTargetRoot`, `boundHasForallFrom`, and the retained child whose
  root must match.
- Target / consumer alignment is explicit: the retained child is admitted into
  `keepTargetFinal`, then selected in `targetC` through `Just v -> v`.
- Local versus non-local propagation differs from the `baseTarget` packet in
  direction, not in vocabulary: this packet is local and same-lane, but still
  requires an explicit target / consumer handoff.
- Interaction with polymorphism and instantiation is only negative-only here.
  The accepted nested-`forall` contrast shows that the packet fails closed
  once a nested quantified wrapper intervenes. That is useful mechanism
  evidence, but it is not a positive `P5` success.
- Reconstruction obligations are partially visible only: the positive example
  stays recursive (`containsMu True`) and the source guard makes the
  `keepTargetFinal` / `targetC` route review-visible, but full-pipeline
  reconstruction remains later-item work.
- Fail-closed handling stays explicit: `schemeBodyTarget` is neighboring
  boundary context only, `rootFinal` and `boundTarget` are excluded as live
  targets for this packet, and nested quantified crossings stay rejected.

This chain therefore gives bounded support for binder-sensitive placement
inside the acyclic model, but only while nested-`forall` crossings continue to
fail closed and the same-lane owner frame stays explicit.

### Direct Comparison

The two accepted chains share a small reusable mechanism vocabulary:

- both anchor recursive-relevant shape before downstream consumer choice;
- both require an explicit owner / binder story before the packet is lawful;
- both rely on an explicit target / consumer handoff rather than an implicit
  unfolding or fallback;
- both keep adjacent lanes review-visible but separate; and
- both stay inside the inherited acyclic representation.

They diverge in bounded, mechanism-level ways rather than by forcing a new
architecture claim:

- the `baseTarget` packet is non-local, alias-bound, and terminates in a
  non-recursive visible output fact;
- the `boundVarTarget` packet is same-lane, retained-child, and terminates in
  a recursive visible output fact;
- the retained-child packet has an explicit nested-`forall` fail-closed guard,
  while the alias-bound packet contributes no positive polymorphism success;
  and
- each packet still depends on one named proof arm
  (`rootNonLocalSchemeAliasBaseLike` or
  `sameLaneLocalRetainedChildTarget`) rather than on a general search model.

The current accepted record therefore looks more like a small reusable
mechanism set than like two unrelated folklore exceptions, but the reuse is
still bounded. The record does not yet justify saying those mechanisms are
general across `P2`-`P5`.

## Item-2 Pressure Read Against The Acyclic Model

The accepted item-2 pressure point asked whether representative `P2`-`P5`
family pressure appears explainable inside the inherited acyclic
(`non-cyclic-graph`) model without packet-specific exceptions.

The current bounded read is:

| Family | Mechanism-map read | Why the read stays bounded |
| --- | --- | --- |
| `P2 non-local-propagation` | `partially explained` | The `baseTarget -> baseC` chain is credible positive evidence that one non-local packet can survive inside the acyclic model, and the retained-child chain shows the same target / consumer vocabulary on a local lane. But propagation still depends on named packet guards and explicit arm ordering. |
| `P3 retained-child-owner-sensitive` | `partially explained` | Both accepted chains need owner-sensitive placement: one through a scheme-root alias binder, one through a retained child under one owner frame. That looks reusable, but only across two bounded families so far. |
| `P4 binder-sensitive-placement` | `partially explained` | The retained-child chain gives direct binder-sensitive evidence through `boundVarTargetRoot`, `boundHasForallFrom`, and the nested-binding fail-closed contrast. The alias-bound chain gives weaker owner/binder support. No broader binder-placement rule is justified yet. |
| `P5 polymorphism-nested-forall` | `negative-only / unresolved` | The accepted record preserves fail-closed nested-`forall` rejection and keeps quantified continuity context separate, but it does not yet contain a positive recursive-polymorphism success that remains inside the inherited baseline. |

Mechanism reuse already visible in the accepted record:

- recursive-shape discovery can be described as anchor-first rather than as
  packet folklore;
- owner / binder placement is doing real explanatory work in both packets;
- target / consumer alignment explains why the accepted packets remain narrow
  but review-visible; and
- fail-closed boundaries are part of the mechanism, not merely leftover test
  noise.

Packet-specific guards that still remain debt:

- `rootNonLocalSchemeAliasBaseLike` is still a named dedicated non-local arm;
- `sameLaneLocalRetainedChildTarget` is still a named dedicated retained-child
  proof;
- `boundHasForallFrom` plus `not hasForall` still act as packet-specific
  admissibility filters rather than as a general polymorphism rule; and
- the accepted record does not yet define competing-candidate search,
  ambiguity rejection, termination discipline, or full-pipeline recursive
  reconstruction.

Bounded item-3 outcome: the current accepted packet history is partially
explainable inside the inherited acyclic model through a small reusable
mechanism vocabulary, but important obligations remain named debt. The main
unresolved debts are positive `P5` evidence, generalized search / admissibility
rules, and full-pipeline reconstruction proof. This is not the item-7
architecture decision.

## Docs-Only Verification Note

This round is expected to change only documentation:

- this canonical item-3 mechanism-map artifact; and
- `orchestrator/rounds/round-084/implementation-notes.md`.

Because the round does not touch `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`, no full `cabal build all && cabal test` gate is triggered unless
the diff escapes that docs-only surface.

Reviewer-facing expectations inherited from `orchestrator/verification.md`
that this artifact is written to satisfy:

- it generalizes multiple accepted packets into reusable mechanism families;
- it preserves the inherited explicit-only / iso-recursive /
  non-equi-recursive / non-cyclic-graph / no-fallback boundary;
- it preserves predecessor continuity by treating `N4`-`N7` and `N11`-`N14`
  as bounded evidence only;
- it tests the item-2 acyclic-model pressure point without upgrading
  negative-only nested-`forall` evidence into a positive polymorphism success;
  and
- it names later-item dependencies instead of silently widening into search,
  reconstruction, coverage, or architecture revision.

If later review cannot justify a stronger claim from the accepted record, the
stronger claim must fail closed and remain unresolved rather than being
silently imported into item `3`.
