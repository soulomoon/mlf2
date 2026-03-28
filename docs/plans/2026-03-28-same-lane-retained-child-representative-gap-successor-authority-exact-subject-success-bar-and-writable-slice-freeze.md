# Same-Lane Retained-Child Representative-Gap Successor Authority, Exact Subject, Success Bar, And Writable-Slice Freeze

Date: 2026-03-28
Round: `round-135`
Roadmap item: `item-1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one bounded current-architecture successor freeze for the
remaining same-lane retained-child representative gap across `P3` / `P4` /
`P6`
Artifact kind: canonical docs-only successor-boundary freeze

## Stage Contract Freeze

This artifact implements only roadmap item `1` for `attempt-1` with
`retry: null`.

This round is docs-only, aggregate-only, freeze-only, and pre-implementation.
Its job is to bind the direct predecessor authority chain, one exact second
same-lane retained-child packet, the exact item-2 success bar, and the exact
writable slice for one bounded current-architecture attempt.

This round does not:

- implement code or tests for the representative-gap packet;
- rerun focused domain evidence or the full Cabal gate as new authority;
- reopen the settled first same-lane pocket carried by
  `boundVarTargetRoot` /
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`;
- treat `C5` or `C7` as second packets;
- reopen the settled exact `P5` nested-`forall` packet;
- reopen the settled exact `C1` / `P2` packet or the settled exact `P1`
  packet;
- authorize cyclic search, multi-SCC search, equi-recursive reasoning,
  fallback widening, a second interface, hardening, rollout, or repo-level
  capability claims; or
- rewrite `orchestrator/state.json`, any roadmap file, `Bugs.md`, or the
  round plan.

Implementer-owned writes for this round are limited to:

- this canonical freeze artifact; and
- `orchestrator/rounds/round-135/implementation-notes.md`.

## Item-1 Authority Ledger

| Input class | Source | Binding read carried forward here |
| --- | --- | --- |
| Production baseline authority | `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` | The repo still ships explicit-only recursive support, iso-recursive meaning only, no equi-recursive semantics, inherited non-cyclic structure, and no fallback widening. |
| Repo-scope capability authority | `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md` | Broad representative-family success remains the only lawful repo-level success bar; one bounded exact packet still does not settle `P3`, `P4`, or `P6`. |
| Direct successor-boundary authority | `docs/plans/2026-03-28-post-p5-repo-scope-successor-authority-evidence-inputs-and-non-widening-boundary-freeze.md` | The post-`P5` repo-scope successor family is open only as a non-widening current-architecture continuation, with the settled same-lane pocket, exact `C1`, exact `P1`, and exact `P5` packets all closed predecessor truth only. |
| Direct refreshed-matrix authority | `docs/plans/2026-03-28-post-p5-repo-scope-refreshed-representative-family-matrix-readiness-surface-and-provenance-validation.md` | `C5` and `C7` remain rereads of the settled first same-lane pocket only; no second same-lane packet has yet been accepted for the `P3` / `P4` / `P6` cluster. |
| Direct successor-gate authority | `docs/plans/2026-03-28-post-p5-repo-scope-readiness-successor-gate-and-immediate-handoff-decision.md` | The strongest honest repo-scope read remains `narrowed unresolved / continue within the current architecture`, and the only lawful next family is one bounded same-lane retained-child representative-gap continuation across `P3` / `P4` / `P6`. |
| Settled same-lane exact-pocket chain, with named lead gate | `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`; `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`; `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-validation.md`; `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md` | The named lead gate keeps the earlier exact-pocket decision history visible, and the accepted post-amendment settlement trio closes that same exact `C2` / `C5` / `C7` packet as settled predecessor truth only. It is not the live subject here, and `C5` / `C7` still do not become second packets. |
| Non-authoritative bug context only | `Bugs.md` | `BUG-2026-03-16-001` remains predecessor replay context only; it may not reopen the older `P2` replay defect or substitute for this second-packet freeze. |

## Exact Live Subject

The live subject for this roadmap family is fixed to one bounded second
same-lane retained-child packet only:

- exact packet name:
  `sameLaneAliasFrameClearBoundaryExpr`
- intended exact harness anchor:
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- exact source expression:

```haskell
sameLaneAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))
```

- recursive annotation:
  `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`
- bounded construction rule:
  start from the settled first-pocket clear-boundary source packet, insert
  exactly one clear-boundary local alias binder `hold` between producer `k`
  and the retained-child consumer position, and do not introduce the
  quantified-crossing `id` application used by the settled exact `P5`
  contrast
- family pressure carried:
  one bounded same-lane representative packet for the remaining `P3` /
  `P4` / `P6` gap
- recursive-shape anchor:
  the same recursive lambda shape as the settled first pocket, but now
  carried through one additional clear-boundary local alias binder before the
  retained-child consumer is judged
- owner / binder frame:
  one same-lane retained-child story with two clear-boundary local binders
  (`k` then `hold`) in the same local `TypeRef` lane; no nested `TyForall`,
  nested owner, or nested scheme-root crossing is authorized
- retained-child route under audit:
  the same-lane retained-child selection culminating in
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`, now
  required to stay coherent across the extra local alias handoff
- authoritative surface under test:
  `runPipelineElab` and `runPipelineElabChecked`, implemented in
  `src/MLF/Elab/Run/Pipeline.hs` and re-exported by
  `src/MLF/Elab/Pipeline.hs` and `src-public/MLF/Pipeline.hs`

This exact subject is distinct from the settled first pocket because it adds
one extra clear-boundary alias binder `hold` and therefore asks for a second
owner/binder story rather than rereading the already-settled one-frame packet.
It is distinct from the settled exact `P5` packet because it does not route
through the quantified-crossing `nestedForallContrastExpr` path and does not
authorize a nested-`forall` success attempt.

Selection note:
no accepted predecessor artifact already names a second packet for this lane.
This exact subject is therefore a bounded inference from the allowed current
docs/code/test context only:

- the settled first same-lane packet in `test/PipelineSpec.hs` and the
  accepted same-lane exact-pocket settlement chain establish the baseline
  one-frame clear-boundary packet;
- `test/Research/P5ClearBoundarySpec.hs` establishes the nearest bounded
  reject-side contrast by adding the quantified-crossing `id` wrapper used by
  `nestedForallContrastExpr`; and
- the production anchors in `Fallback.hs`, `Scope.hs`, and `Run/Pipeline.hs`
  show that the live retained-child route and authoritative surfaces remain
  the current-architecture locus for any second-packet read.

The frozen second packet is the bounded clear-boundary midpoint between those
already-settled predecessor truths: one extra same-lane local alias binder,
no quantified crossing, no new route, and no widened interface.

## Exact Current Read Carried Forward

The carried-forward read for this exact second packet is intentionally narrow:

- no accepted artifact yet records a reviewed authoritative result for
  `sameLaneAliasFrameClearBoundaryExpr`;
- the settled first pocket remains positive predecessor truth only and may not
  be widened into second-packet evidence;
- `C5` and `C7` remain rereads of that first pocket only; and
- the settled exact `P5` packet proves only that quantified crossing remains a
  lawful fail-closed guard, not that every new binder/frame variation must
  fail.

Item `2` therefore owns the first reviewed current read for this exact second
packet and must end in exactly one honest exact-packet result.

## Exact Success Bar For Item `2`

Item `2` may claim exact-packet success only if it satisfies all of the
following together:

1. The live subject remains the exact packet
   `sameLaneAliasFrameClearBoundaryExpr`; no packet swap back to the settled
   first pocket, the settled exact `P5` packet, the settled exact `C1`
   packet, or any broader family bundle is allowed.
2. The work remains inside the inherited current architecture:
   no cyclic search,
   no multi-SCC search,
   no equi-recursive reasoning,
   no second interface,
   and no fallback widening.
3. Recursive structure for this exact packet becomes review-visible on the
   current authoritative pipeline surfaces rather than helper-only,
   replay-only, fallback-only, or diagnostic-only surfaces.
4. If that positive clear does not happen, item `2` must still end in exactly
   one honest exact-packet outcome only:
   `fail-closed` or `narrower current-architecture blocker`.
5. Any touched code or tests must satisfy the live verification contract; if
   item `2` touches `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`,
   the full gate `cabal build all && cabal test` becomes mandatory unless a
   later accepted review records a contract-allowed exception.

Even if item `2` clears this exact success bar, that would still mean:

- one exact-packet improvement only;
- not general `P3`, `P4`, or `P6` family settlement by itself; and
- not repo-level automatic recursive-inference readiness by itself.

## Writable Slice Freeze For Item `2`

The next bounded implementation round may write only inside this frozen slice:

| Path or class | Why it is included |
| --- | --- |
| `src/MLF/Elab/Run/ResultType/Fallback.hs` | The same-lane retained-child routing and retained-child target selection live here. |
| `src/MLF/Elab/Run/Scope.hs` | The current owner-local target selectors and scheme-body targeting live here. |
| `src/MLF/Elab/Run/Pipeline.hs` | The current authoritative pipeline entrypoints and `checkedAuthoritative` handoff live here. |
| `src/MLF/Elab/Pipeline.hs` | Internal pipeline re-export continuity may need synchronization if production entrypoint behavior changes. |
| `src-public/MLF/Pipeline.hs` | Public pipeline re-export continuity may need synchronization if production entrypoint behavior changes. |
| `src/MLF/Elab/TermClosure.hs` | Current authoritative-result preservation on the production path lives here. |
| `test/PipelineSpec.hs` | Production-path regression coverage may need one exact-packet anchor. |
| `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` | The exact second-packet harness belongs here if a focused spec is introduced. |
| `test/Main.hs` | Test wiring only if a new focused spec module is introduced. |
| `mlf2.cabal` | Cabal wiring only if a new focused spec module is introduced. |
| this freeze doc and `orchestrator/rounds/round-135/*` | Bound documentation and round-owned evidence for this family. |

This writable slice is permissive, not mandatory. Item `2` may narrow below
this list, but it may not widen beyond it without a later accepted change.

The following remain explicitly blocked for item `2`:

- all `src/MLF/Constraint/**` work;
- cyclic or multi-SCC machinery;
- fallback widening;
- second-interface work;
- second-family openings; and
- edits outside the frozen slice above.

## Review Focus And Round Closure

Reviewer focus for this item stays on:

- authority continuity from the March 14 baseline, the March 25 capability
  contract, the accepted post-`P5` repo-scope trio, and the closed first-
  pocket lead gate;
- whether the exact live subject is frozen as one second packet rather than a
  reread of the first pocket or a reopen of `P5`;
- whether the exact item-2 success bar stays packet-bounded and
  non-widening; and
- whether the writable slice stays exact and current-architecture only.

This round remains aggregate-only and stops at the freeze. It does not
authorize implementation inside `round-135`.

The next lawful move after this freeze is roadmap item `2` only:

`Implement and validate one bounded same-lane retained-child representative-gap slice`
