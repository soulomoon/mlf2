# General Automatic Iso-Recursive Current-Architecture Follow-On Successor Authority, Next Exact Representative-Gap Packet, Current Live Read, Success Bar, And Writable-Slice Freeze

Date: 2026-04-02
Round: `round-173`
Roadmap item: `item-1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one fresh exact same-lane retained-child representative-gap
packet only
Artifact kind: canonical docs-only successor-boundary freeze

## Stage Contract Freeze

This artifact implements only roadmap item `1` for `attempt-1` with
`retry: null`.

This round is docs-only, aggregate-only, freeze-only, and pre-implementation.
Its job is to bind the predecessor authority chain for the follow-on family,
freeze one fresh exact same-lane retained-child representative-gap packet,
record the current exact live read for that packet, freeze the exact item-2
success bar, and freeze one fail-closed writable slice for a bounded
current-architecture attempt.

This round does not:

- implement code or tests for the new packet;
- rerun focused packet checks or `cabal build all && cabal test` as new
  accepted evidence for the family;
- reopen the settled first same-lane pocket;
- reopen the settled exact `P5` packet;
- reopen the accepted one-alias packet
  `sameLaneAliasFrameClearBoundaryExpr` as if it were still the live packet;
- authorize cyclic search, multi-SCC search, equi-recursive reasoning,
  fallback widening, a second interface, hardening, rollout, or a repo-level
  readiness claim; or
- rewrite `orchestrator/state.json`, any roadmap file, or `Bugs.md`.

Implementer-owned writes for this round are limited to:

- this canonical freeze artifact; and
- `orchestrator/rounds/round-173/implementation-notes.md`.

The inherited production boundary remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- `non-equi-recursive = keep`;
- the inherited structural model remains non-cyclic; and
- `no-fallback = keep`.

## Item-1 Authority Ledger

| Input class | Source | Binding read carried forward here |
| --- | --- | --- |
| Production baseline authority | `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` | The repo still ships explicit-only recursive support, iso-recursive meaning only, no equi-recursive semantics, inherited non-cyclic structure, and no fallback widening. |
| Repo-level capability contract | `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md` | One bounded packet still may not be upgraded into general automatic iso-recursive readiness. Representative-family coverage remains the lawful repo-level success bar. |
| Narrowed current-architecture successor decision | `docs/plans/2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision.md` | The strongest accepted repo-scope read remained `narrowed unresolved / continue within the current architecture`, so follow-on work remains bounded current-architecture work rather than an implicit boundary revision. |
| Original exact representative-gap freeze | `docs/plans/2026-03-28-same-lane-retained-child-representative-gap-successor-authority-exact-subject-success-bar-and-writable-slice-freeze.md` | The retained-child representative-gap family already froze the one-alias packet `sameLaneAliasFrameClearBoundaryExpr`, its route under audit, and its packet-bounded success discipline. |
| March 29 same-packet settlement checkpoint | `docs/plans/2026-03-29-same-lane-alias-frame-representative-gap-post-item-2-settlement-surface-and-repo-impact-read.md` | The March 29 checkpoint preserved the one-alias packet as predecessor evidence only; it did not close the broader `P3` / `P4` / `P6` family. |
| April 1 inherited blocker-lane freeze | `docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md` | The next family had to preserve the inherited explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback boundary while staying exact-packet-bounded. |
| Accepted item-3 settlement surface | `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md` | `sameLaneAliasFrameClearBoundaryExpr` is now one settled `narrow success` packet on `runPipelineElab` and `runPipelineElabChecked` within the inherited current architecture. |
| Accepted item-4 decision and handoff | `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md` | The accepted handoff is `continue-bounded` plus `open one bounded current-architecture family`, while broader `P3` / `P4` / `P6`, repo-level readiness, and next-packet selection remain unresolved. |

Settled predecessor truth only:

- `sameLaneAliasFrameClearBoundaryExpr` is one accepted `narrow success`
  packet on both `runPipelineElab` and `runPipelineElabChecked`;
- that one packet does not settle the broader same-lane retained-child
  representative-gap family across `P3` / `P4` / `P6`; and
- this follow-on family therefore must freeze one fresh packet rather than
  silently reusing the accepted one-alias packet as live debt.

## Next Exact Representative-Gap Packet

The live subject for this follow-on family is fixed to one fresh exact packet
only:

- exact packet name:
  `sameLaneDoubleAliasFrameClearBoundaryExpr`
- intended exact harness anchor:
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- exact source expression:

```haskell
sameLaneDoubleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "keep")) (EVar "u"))))
```

- recursive annotation:
  `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`
- carried route under audit:
  the same-lane retained-child continuation culminating in
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`
- authoritative surfaces under audit:
  `runPipelineElab` and `runPipelineElabChecked`

This packet is distinct from the accepted predecessor packet
`sameLaneAliasFrameClearBoundaryExpr` because it adds one more clear-boundary
same-lane alias binder, `keep`, beyond the already accepted one-alias
`hold` packet. It is also distinct from the settled exact `P5` contrast
because it does not route through `nestedForallContrastExpr` and does not add
the quantified-crossing `id` mediation preserved in
`test/Research/P5ClearBoundarySpec.hs`.

Selection note:
this exact packet is a bounded inference from accepted neighboring truths plus
read-only current-architecture anchors only:

- `test/PipelineSpec.hs` and
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` preserve the
  accepted one-alias packet and its surrounding same-lane retained-child lane;
- `test/Research/P5ClearBoundarySpec.hs` preserves the nearest bounded reject
  contrast where quantified crossing is introduced; and
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` plus
  `src/MLF/Elab/TermClosure.hs` remain the current read-only production anchors
  for the retained-child route and the immediately adjacent accepted fix lane.

The frozen packet is therefore the smallest fresh follow-on subject inside the
inherited current architecture: one additional same-lane clear-boundary alias
binder, no quantified crossing, no route change, and no widened interface.

## Exact Current Live Read

The current live read for `sameLaneDoubleAliasFrameClearBoundaryExpr` is the
same shared top-level blocker on both authoritative entrypoints:

- `runPipelineElab`:

```text
Left (PipelineTypeCheckError (TCLetTypeMismatch (TForall "a" Nothing (TArrow (TMu "t6" (TArrow (TVar "t6") (TBase (BaseTy {getBaseName = "Int"})))) (TMu "t6" (TArrow (TVar "t6") (TBase (BaseTy {getBaseName = "Int"})))))) (TForall "a" Nothing (TArrow (TVar "a") (TVar "t10")))))
```

- `runPipelineElabChecked`:

```text
Left (PipelineTypeCheckError (TCLetTypeMismatch (TForall "a" Nothing (TArrow (TMu "t6" (TArrow (TVar "t6") (TBase (BaseTy {getBaseName = "Int"})))) (TMu "t6" (TArrow (TVar "t6") (TBase (BaseTy {getBaseName = "Int"})))))) (TForall "a" Nothing (TArrow (TVar "a") (TVar "t10")))))
```

This live read is exact-packet evidence only:

- it does not reopen the accepted one-alias packet as live blocker debt;
- it does not prove broad `P3` / `P4` / `P6` settlement or broad failure; and
- it does not by itself prove that the current architecture boundary must be
  revised.

Read-only adjacent-packet precedent only:
accepted round-170 evidence shows that the immediately adjacent one-alias
packet cleared by extending
`preserveRetainedChildAuthoritativeResult` in
`src/MLF/Elab/TermClosure.hs`. That adjacent accepted fix is relevant only as
bounded packet-local context for the next writable slice below; it does not
already prove the root cause for this fresh double-alias packet.

## Exact Success Bar For Item `2`

Item `2` may claim bounded success only if it satisfies all of the following
together:

1. The live subject remains the exact packet
   `sameLaneDoubleAliasFrameClearBoundaryExpr`; no packet swap back to the
   settled one-alias packet, the settled exact `P5` packet, or any broader
   family bundle is allowed.
2. The work remains inside the inherited current architecture:
   no cyclic search,
   no multi-SCC search,
   no equi-recursive reasoning,
   no second interface,
   and no fallback widening.
3. Review-visible improvement happens on the authoritative pipeline surfaces
   `runPipelineElab` and `runPipelineElabChecked`, not merely on helper-only,
   alternate-path, or fallback-only evidence.
4. Item `2` must end in exactly one honest exact-packet outcome only:
   `narrow success`,
   `fail-closed`, or
   `narrower current-architecture blocker`.
5. Any later item-2 round touching the authorized source/test files must rerun
   `cabal build all && cabal test`.

Even if item `2` clears this exact success bar, that would still mean:

- one bounded packet improvement only;
- not general `P3`, `P4`, or `P6` settlement by itself; and
- not repo-level automatic iso-recursive readiness by itself.

## Writable Slice Freeze For Item `2`

The next bounded implementation round may write only inside this frozen slice:

| Path or class | Why it is included |
| --- | --- |
| `src/MLF/Elab/TermClosure.hs` | The immediately adjacent accepted one-alias packet cleared in `preserveRetainedChildAuthoritativeResult`, so the next bounded attempt must stay centered on that same packet-local preservation lane unless a later accepted family explicitly reopens scope. |
| `test/PipelineSpec.hs` | The production-path regression for the exact fresh packet belongs here if item `2` adds or refreshes one exact packet assertion. |
| `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` | The focused retained-child representative-gap probe for this exact packet belongs here. |
| this freeze artifact and `orchestrator/rounds/round-173/*` | Bound documentation and round-owned evidence for this family. |

This writable slice is permissive, not mandatory. Item `2` may narrow below
this list, but it may not widen beyond it without a later accepted change.

The following remain explicitly read-only / blocked for item `2`:

- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`;
- `src/MLF/Elab/Run/ResultType/Fallback.hs`;
- `src/MLF/Elab/Run/Scope.hs`;
- `src/MLF/Elab/Run/Pipeline.hs`;
- `src/MLF/Elab/Pipeline.hs`;
- `src-public/MLF/Pipeline.hs`;
- `test/Main.hs`;
- `mlf2.cabal`;
- all `src/MLF/Constraint/**` work;
- cyclic search;
- multi-SCC search;
- fallback widening;
- second-interface work;
- second-family openings; and
- any edit outside the frozen slice above.

If item `2` cannot clear or honestly diagnose the packet inside this exact
slice, it must stop at `narrower current-architecture blocker` rather than
widening the slice in place.

## Next Lawful Move

The next lawful move after this freeze is roadmap item `2` only:

`Implement and validate one bounded current-architecture slice on the frozen representative-gap packet`

That next round must preserve:

- this exact predecessor authority chain;
- this exact fresh packet;
- this exact current live read;
- this exact item-2 success bar; and
- this exact fail-closed writable slice.

## Docs-Only Verification Note

This round changes only documentation and round-owned orchestrator artifacts:

- this canonical freeze artifact; and
- `orchestrator/rounds/round-173/implementation-notes.md`.

Because the round does not touch `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`, the full `cabal build all && cabal test` gate is not required
here. Reviewer verification should focus on authority continuity, exact packet
freeze discipline, exact live-read freeze discipline, exact success-bar
discipline, and whether the writable slice stays packet-bounded and
current-architecture only.
