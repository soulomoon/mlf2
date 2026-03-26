# Findings

## Scope

- Authoritative baseline documents:
  - `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  - `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
  - `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-search-model.md`
  - `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  - `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
- Newer accepted docs may be consulted only if they materially update `P5`.

## Working Notes

- Initial index pass:
  - `2026-03-25` capability contract keeps `P5 polymorphism-nested-forall` in
    the required success set (`must succeed`).
  - `2026-03-25` mechanism map records `P5` as `negative-only / unresolved`.
  - `2026-03-25` search model says positive nested-`forall` success remains
    blocker debt.
  - `2026-03-26` `C3/C7` evidence slice and `keep-vs-reopen` gate both keep
    `P5` reject-side only through `C3`.
- Candidate newer accepted docs that materially touch `P5`:
  - `2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`
  - `2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`

## Baseline Read

- Capability contract:
  - `P5 polymorphism-nested-forall` is a required positive family, not optional
    stretch scope.
- Mechanism map:
  - `P5` remains `negative-only / unresolved`.
  - The accepted mechanism record only proves fail-closed nested-`forall`
    rejection, not positive recursive-polymorphism success.
  - The mechanism-level debt is named concretely as
    `boundHasForallFrom` plus `not hasForall`, which still behave like
    packet-specific admissibility filters rather than a general polymorphism
    rule inside the inherited acyclic model.

## Search And `C3` Settlement Read

- Search model:
  - `R4` lifts `boundHasForallFrom` and `not hasForall` only into a general
    reject-side quantified-boundary rule.
  - The accepted search model explicitly says this does not justify positive
    `P5` success; positive nested-`forall` recursion remains blocker debt.
  - The blocker is therefore not yet framed as full architecture failure in
    the search-model artifact itself; it is an admissibility/search debt
    inside the inherited acyclic model.
- `2026-03-26` `C3/C7` slice:
  - `C3` is still the primary bounded row for `P5`.
  - `C3` remains reject-side only, not an admitted positive family.
  - The concrete failure is: once quantified crossing appears on the same
    retained-child search, the candidate is no longer lawfully preserved
    through `keepTargetFinal -> targetC`, and the accepted visible-output
    fact for the contrast stays non-recursive (`containsMu False`).

## Aggregate Accepted State

- `2026-03-26` representative-family matrix and unresolved-family ledger both
  keep `P5` at `reject-side only`.
- The repo-level `keep-vs-reopen` gate does not upgrade `P5` itself into an
  architecture-boundary-specific blocker. Instead, it treats `P5` as one part
  of the aggregate evidence that the inherited `non-cyclic-graph` boundary no
  longer meets the repo-level keep bar.
- Family-level read:
  - `P5` is still blocked by quantified-boundary admissibility on row `C3`.
- Repo-level read:
  - persistent `P5` failure contributes to reopening the
    `non-cyclic-graph` question, but the accepted docs stop short of saying
    that a positive `P5` packet itself has already been proven architecture-
    impossible.
- No newer accepted doc found in the current `2026-03-26` surface upgrades
  `P5` beyond the gate/matrix/ledger trio above. Later same-lane continuity
  docs still cite the nested-`forall` boundary only as fail-closed guard
  context, not as a positive `P5` change.

## Code/Test Anchors

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `boundHasForallFrom` walks ancestry and reports nested `TyForall`,
    nested scheme roots, or nested owner crossings.
  - candidate generation stores the `hasForall` bit beside each candidate.
  - candidate selection still filters on `not hasForall`, which is the direct
    guard cutting off the same-lane retained-child route for `C3`.
- `test/PipelineSpec.hs`
  - the nested-`forall` fail-closed spec asserts `containsMu False` for the
    quantified-crossing contrast, matching the accepted `C3` read.

## Synthesis

- Latest accepted `P5` status:
  - still `reject-side only` / `positive success unearned`.
- Exact blocker:
  - the only current same-lane retained-child candidate for the `C3`-style
    nested-`forall` pressure is invalidated by the quantified-boundary guard
    before it can stay on `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
- Best blocker class:
  - primarily `search/admissibility`.
  - secondary note: unresolved aggregate failure across families now reopens
    the `non-cyclic-graph` architecture question, but `P5` itself is not yet
    individually accepted as architecture-impossible.
- Narrow bounded next task:
  - construct and evaluate one bounded positive `P5` candidate packet inside
    the current search vocabulary where polymorphic context is present but the
    retained-child route stays on the clear-boundary side of `R4`, then record
    whether it can survive to a lawful recursive output read without fallback
    or architecture widening.
  - if no such packet can be made under the current rules, that result would
    sharpen the architecture-pressure case without broadening the task.
