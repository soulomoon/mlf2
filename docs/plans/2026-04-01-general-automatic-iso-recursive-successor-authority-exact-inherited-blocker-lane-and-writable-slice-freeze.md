# General Automatic Iso-Recursive Successor Authority, Exact Inherited Blocker Lane, And Writable-Slice Freeze

Date: 2026-04-01
Round: `round-169`
Roadmap item: `item-1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: inherited exact packet `sameLaneAliasFrameClearBoundaryExpr` only
Artifact kind: canonical docs-only successor-boundary freeze

## Stage Contract Freeze

This artifact implements only roadmap item `1` for `attempt-1` with
`retry: null`.

This round is docs-only, aggregate-only, freeze-only, and pre-implementation.
Its job is to bind the predecessor authority chain for the current family, the
exact inherited blocker lane, the current exact blocker read for that lane,
the exact item-2 success bar, and the exact writable slice for one bounded
current-architecture follow-on.

This round does not:

- implement code or tests for the inherited blocker lane;
- rerun focused domain evidence as new authority for the family;
- reopen the settled first same-lane pocket;
- reopen the settled exact `P5` packet;
- reopen the March 29 same-packet blocker settlement as if it were still the
  live blocker read;
- authorize cyclic search, multi-SCC search, equi-recursive reasoning,
  fallback widening, a second interface, hardening, rollout, or a repo-level
  readiness claim; or
- rewrite `orchestrator/state.json`, any roadmap file, or `Bugs.md`.

Implementer-owned writes for this round are limited to:

- this canonical freeze artifact; and
- `orchestrator/rounds/round-169/implementation-notes.md`.

The inherited production boundary remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- `non-equi-recursive = keep`;
- the inherited structural model remains non-cyclic;
- `no-fallback = keep`; and
- one-interface-only remains binding.

## Item-1 Authority Ledger

| Input class | Source | Binding read carried forward here |
| --- | --- | --- |
| Production baseline authority | `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` | The repo still ships explicit-only recursive support, iso-recursive meaning only, no equi-recursive semantics, inherited non-cyclic structure, and no fallback widening. |
| Repo-level capability contract | `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md` | One bounded packet still may not be upgraded into general automatic iso-recursive readiness. Representative-family coverage remains the lawful repo-level success bar. |
| Narrowed current-architecture successor decision | `docs/plans/2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision.md` | The strongest accepted repo-scope read remained `narrowed unresolved / continue within the current architecture`, and the live continuation stayed bounded rather than reopening the architecture boundary outright. |
| Exact inherited packet freeze | `docs/plans/2026-03-28-same-lane-retained-child-representative-gap-successor-authority-exact-subject-success-bar-and-writable-slice-freeze.md` | The exact packet `sameLaneAliasFrameClearBoundaryExpr`, its success bar, and its writable slice were already frozen for one bounded same-lane retained-child continuation. |
| March 29 blocker settlement checkpoint | `docs/plans/2026-03-29-same-lane-alias-frame-representative-gap-post-item-2-settlement-surface-and-repo-impact-read.md` | At the March 29 checkpoint, the packet remained a `narrower current-architecture blocker` on `runPipelineElab` and `runPipelineElabChecked`, with the accepted blocker text still at the `PhiTranslatabilityError` site for that revision. |

Mechanism predecessor truth only:

- roadmap families
  `2026-03-29-01-automatic-iso-recursive-type-inference-completion` and
  `2026-03-29-02-iso-recursive-inference-gap-fixes` remain predecessor truth
  only for the bounded supported mechanism: automatic `TyMu` introduction
  exists, reification produces `TMu`, elaboration emits `ERoll` / `EUnroll`,
  and the bounded gap-fix lane is closed.
- That mechanism truth does not upgrade this inherited packet into broad
  readiness and does not erase the need to freeze the current exact blocker
  read honestly.

## Exact Inherited Blocker Lane

The inherited live blocker lane for this family remains fixed to one exact
packet only:

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
- carried route under audit:
  the same-lane retained-child continuation culminating in
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`
- authoritative surfaces under audit:
  `runPipelineElab` and `runPipelineElabChecked`

This inherited lane remains distinct from:

- the settled first same-lane pocket, which remains closed predecessor truth
  only; and
- the settled exact `P5` packet, which remains a separate quantified-crossing
  predecessor packet rather than live debt here.

The lane therefore remains one bounded current-architecture successor
continuation only. It is not a reopened family bundle, and it is not a second
repo-scope readiness attempt under a new name.

## Exact Current Live Blocker Read

The March 29 checkpoint carried this packet as a `PhiTranslatabilityError`
blocker. The current focused harness now proves a later blocker on the same
exact packet:

- both `runPipelineElab` and `runPipelineElabChecked` currently fail with the
  same top-level constructor:
  `PipelineTypeCheckError (TCLetTypeMismatch ...)`
- the current blocker is therefore a later type-check/pipeline blocker for the
  same exact packet, not evidence that the packet has become a success case
  and not evidence that the family is settled

Current controller-observed rendering for both authoritative entrypoints:

```text
Left (PipelineTypeCheckError
    (TCLetTypeMismatch
        (TForall "a" Nothing
            (TArrow
                (TMu "t6" (TArrow (TVar "t6") (TBase (BaseTy {getBaseName = "Int"}))))
                (TMu "t6" (TArrow (TVar "t6") (TBase (BaseTy {getBaseName = "Int"}))))))
        (TForall "a" Nothing (TArrow (TVar "a") (TVar "t10")))))
```

This is the exact blocker read frozen by item `1` for the live repo state on
2026-04-01. The March 29 `PhiTranslatabilityError` checkpoint remains binding
predecessor evidence only; it is no longer the current exact blocker read for
this packet.

## Exact Success Bar For Item `2`

Item `2` may claim bounded success only if it satisfies all of the following
together:

1. The live subject remains the exact packet
   `sameLaneAliasFrameClearBoundaryExpr`; no packet swap back to the settled
   first same-lane pocket, the settled exact `P5` packet, or a broader family
   bundle is allowed.
2. The work remains inside the inherited current architecture:
   no cyclic search,
   no multi-SCC search,
   no equi-recursive reasoning,
   no second interface,
   and no fallback widening.
3. Review-visible improvement happens on the current authoritative pipeline
   surfaces `runPipelineElab` and `runPipelineElabChecked`, not merely on
   helper-only, alternate-path, or fallback-only evidence.
4. If that positive clear does not happen, item `2` must still end in exactly
   one honest exact-packet outcome only:
   `fail-closed` or `narrower current-architecture blocker`.
5. Any touched code or tests must satisfy the live verification contract,
   including `cabal build all && cabal test` when item `2` touches `src/`,
   `src-public/`, `app/`, `test/`, or `mlf2.cabal`.

Even if item `2` clears this exact success bar, that would still mean:

- one bounded packet improvement only;
- not general `P3`, `P4`, or `P6` settlement by itself; and
- not repo-level automatic iso-recursive readiness by itself.

## Writable Slice Freeze For Item `2`

The next bounded implementation round may write only inside this frozen slice:

| Path or class | Why it is included |
| --- | --- |
| `src/MLF/Elab/Run/ResultType/Fallback.hs` | The inherited lane and prior blocker checkpoints are anchored here. |
| `src/MLF/Elab/Run/Scope.hs` | Owner-local target selection and same-lane scope reads remain part of the bounded lane. |
| `src/MLF/Elab/Run/Pipeline.hs` | Current authoritative pipeline entrypoints live here. |
| `src/MLF/Elab/Pipeline.hs` | Internal pipeline re-export continuity may need synchronized updates. |
| `src-public/MLF/Pipeline.hs` | Public pipeline re-export continuity may need synchronized updates. |
| `src/MLF/Elab/TermClosure.hs` | Authoritative-result preservation may still be implicated by the inherited lane. |
| `test/PipelineSpec.hs` | Production-path regression coverage for bounded packet work may need refresh. |
| `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` | The exact inherited packet harness belongs here. |
| `test/Main.hs` | Test wiring only if a later focused spec change requires it. |
| `mlf2.cabal` | Cabal wiring only if a later focused spec/module change requires it. |
| this freeze artifact and `orchestrator/rounds/round-169/*` | Bound documentation and round-owned evidence for this family. |

This writable slice is permissive, not mandatory. Item `2` may narrow below
this list, but it may not widen beyond it without a later accepted change.

The following remain explicitly blocked for item `2`:

- all `src/MLF/Constraint/**` work;
- cyclic or multi-SCC machinery;
- fallback widening;
- second-interface work;
- second-family openings; and
- edits outside the frozen slice above.

## Next Lawful Move

The next lawful move after this freeze is roadmap item `2` only:

`Implement and validate one bounded current-architecture slice on the frozen blocker lane`

That next round must preserve:

- this exact predecessor authority chain;
- this exact inherited blocker lane;
- this exact current blocker read;
- this exact success bar; and
- this exact writable slice.

## Docs-Only Verification Note

This round changes only documentation and round-owned orchestrator artifacts:

- this canonical freeze artifact; and
- `orchestrator/rounds/round-169/implementation-notes.md`.

Because the round does not touch `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`, the full `cabal build all && cabal test` gate is not required
here. Reviewer verification should focus on docs-diff correctness, authority
continuity, exact inherited blocker-lane scope discipline, exact blocker-read
freeze, and whether item `2` becomes the next concrete lawful move without
silent widening.
