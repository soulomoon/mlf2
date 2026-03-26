# Global `non-cyclic-graph` Same-Lane Retained-Child Public-Output Continuity Post-Amendment Settlement Ledger

Date: 2026-03-26
Round: `round-114`
Roadmap item: `item-2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one docs-only rev-004 item-2 bounded settlement ledger for
the exact same-pocket post-amendment read already frozen by accepted
`round-113`
Artifact kind: canonical docs-only rev-004 item-2 bounded settlement ledger

## Stage Contract Freeze

This artifact implements only roadmap item `2` for `attempt-1` with
`retry: null`.

This round is docs-first and evidence-first. It records only the current
exact-pocket post-amendment settlement read for the already-selected
same-lane `C2` / `C5` / `C7` pocket on one new bounded rev-004 settlement
surface:

- the same selected rows remain in scope;
- the same exact packet and tuple remain in scope;
- the same accepted rev-003 item-3 current-result anchor remains
  authoritative;
- this one new rev-004 settlement ledger becomes the only newly written
  settlement surface in this round; and
- every still-live pre-amendment same-family dossier remains immutable
  historical evidence rather than a writable target.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, the rev-004 roadmap bundle,
  `Bugs.md`, `TODO.md`, `implementation_notes.md`, or `CHANGELOG.md`;
- validation of the new ledger surface;
- the post-settlement same-family handoff decision;
- rewriting predecessor settlement artifacts in place;
- a second packet, a second route, a second interface, or a broad same-
  family or repo-level success claim; or
- multi-SCC search, second interfaces, fallback widening, production
  rollout, hardening, or broad capability claims.

The inherited baseline remains unchanged:

- `iso-recursive = keep`;
- `non-equi-recursive = keep`;
- `no-fallback = keep`;
- explicit recursive annotations remain the production baseline;
- equi-recursive semantics and implicit unfolding remain blocked; and
- rev-001 items `6` through `8` remain blocked.

## Control-Plane Inputs

| Control-plane input | Binding read carried forward |
| --- | --- |
| `orchestrator/rounds/round-114/selection.md` | This round is item-2-only, docs-only, and limited to one bounded settlement-ledger artifact for the exact same pocket. |
| `orchestrator/rounds/round-114/plan.md` | The output must preserve the accepted current-result anchor, write only the one frozen ledger path, preserve predecessor immutability, and leave the future validation path unwritten. |
| `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/roadmap.md` | Item `2` is the current lowest-numbered unfinished item, and completion requires one accepted docs-only artifact recording the current exact-pocket post-amendment read on new bounded settlement surfaces only. |
| `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/retry-subloop.md` | Item `2` may retry, but this attempt may finalize if the bounded settlement ledger fully satisfies the stage contract. |
| `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-004/verification.md` | The stage must prove that the new settlement surface stays exact-pocket-only, preserves predecessor immutability, and does not silently widen into broad same-family or rollout claims. |

These inputs define the live item-2 contract only. They do not replace
accepted predecessor truth.

## Accepted Predecessor Authority

| Accepted predecessor artifact | Binding truth carried forward |
| --- | --- |
| `orchestrator/rounds/round-113/review-record.json` | Accepted `round-113` froze the exact current-result anchor, the exact future writable docs boundary, and the exact predecessor-immutability rule that now governs this ledger. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-surface-and-successor-boundary-freeze.md` | Accepted rev-004 item `1` fixed the only lawful writable ledger path and the only lawful validation path for this successor revision. |
| `orchestrator/rounds/round-111/review-record.json` | Accepted `round-111` remains the controller-level authority for the exact current same-pocket post-amendment read recorded here. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md` | Accepted rev-003 item `3` fixed the exact current read: helper-visible/internal recursive continuity remains present, and both authoritative public entrypoints now carry bounded recursive structure on the same exact packet. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md` | Still-live predecessor dossier that preserves the old pre-amendment public-collapse read for `C2` / `C5`. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md` | Still-live predecessor dossier that preserves the old pre-amendment public-collapse read for `C7`. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md` | Still-live predecessor matrix that preserves the old pre-amendment blocker-debt classification for the exact same pocket in its broader family context. |
| `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md` and `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md` | Still-live predecessor exact-pocket ledgers that preserve the earlier internal/public split and blocker-debt classification before the accepted amendment landed. |
| `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` | The inherited baseline remains exact-pocket-only, explicit-only in production, iso-recursive only, non-equi-recursive, and no-fallback. |

These accepted artifacts remain binding predecessor authority only. This
ledger does not validate itself and does not decide the later handoff.

## Exact Same-Pocket Boundary

This settlement ledger remains bound to exactly one same-pocket subject and
no other:

`the exact same-lane retained-child / public-output continuity pocket already carried by accepted rows C2, C5, and C7, with non-local C1 retained only as bounded contrast context`

### Exact Packet

The selected packet remains exactly this packet and no variant:

```haskell
ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
  (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
```

where
`recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.

### Exact Tuple

The same frozen tuple remains binding:

| Continuity field | Exact selected value |
| --- | --- |
| Family | same-lane retained-child |
| Recursive-shape anchor | `boundVarTargetRoot` |
| Owner / binder frame | one owner-local retained-child frame |
| Route | `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` |
| Quantified-boundary status | clear-boundary only: `boundHasForallFrom` is false and `not hasForall` holds |

`C5` and `C7` remain rereads of this same exact packet only. They are not
second packets, second routes, second interfaces, or second same-family
lanes.

## Current Exact-Pocket Post-Amendment Read Recorded On The New Ledger Surface

The exact accepted current same-pocket read now carried onto this new
bounded settlement surface is:

| Surface | Exact accepted current read |
| --- | --- |
| Helper-visible/internal fallback result | `TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))` |
| Helper-visible/internal recursive witness | `containsMu True` |
| Authoritative public output: `runPipelineElab` | `Right (TForall "a" Nothing (TArrow (TVar "t31") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))))` |
| Authoritative public output: `runPipelineElabChecked` | `Right (TForall "a" Nothing (TArrow (TVar "t31") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))))` |

This ledger records that exact read on a new rev-004 settlement surface
only. It does not retro-edit the older pre-amendment dossiers that still
describe the earlier `forall identity` public collapse.

## Exact-Pocket Settlement Ledger

| Row / surface lens | New bounded settlement read recorded here | Historical artifact preserved unchanged | Bounded meaning |
| --- | --- | --- | --- |
| `C2` same-lane retained-child | the exact same-lane packet now has bounded recursive structure reviewer-visible on both the helper-visible/internal surface and the authoritative public surface for this one packet only | `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md` remains the older pre-amendment dossier | this records a current exact-pocket post-amendment read only; it is not a broader `P3` family settlement claim |
| `C5` owner / binder-sensitive reread of the same packet | the same exact `C2` packet, owner-local frame, and clear-boundary-only status remain the only lawful owner-sensitive reread of this current post-amendment result | `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md` and the earlier case ledgers remain historical evidence only | this does not create a second packet, a second owner-local success surface, or a general `P4` settlement claim |
| `C7` public-output continuity lens on the same packet | the same exact packet no longer collapses to `Right (TForall "a" Nothing (TVar "a"))` on the authoritative public path; it now carries the bounded recursive structure shown above on both public entrypoints | `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`, the representative matrix dossier, and the earlier exact-pocket classification remain historical evidence only | this records a current exact-pocket public-output settlement read only; it is not a repo-level `P6` success claim and not a broader same-family rollout claim |

## Immutable Predecessor Surfaces Preserved

The older same-family artifacts remain untouched historical evidence, not
writable settlement targets:

| Historical artifact | Historical read it continues to preserve | Settlement relation recorded here |
| --- | --- | --- |
| `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md` | the older pre-amendment `C2` / `C5` authoritative public-collapse read | superseded only on this new rev-004 ledger surface, not rewritten in place |
| `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md` | the older pre-amendment `C7` public-collapse read | superseded only on this new rev-004 ledger surface, not rewritten in place |
| `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md` | the earlier representative-matrix blocker-debt classification | remains a predecessor matrix only; this ledger does not back-edit it into broader success |
| `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md` | the earlier exact-pocket case freeze with an internal/public split | remains historical evidence of the pre-amendment state |
| `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md` | the earlier exact-pocket blocker-debt classification | remains historical evidence of the pre-amendment classification only |

This is the key bounded settlement rule for item `2`:
the current exact-pocket post-amendment read is newly recorded here, while
the pre-amendment dossiers remain historical evidence exactly as they were.

## What This Ledger Settles, And What It Does Not

This item-2 ledger settles exactly this much, and no more:

- the accepted current exact-pocket post-amendment read now has one new
  bounded rev-004 settlement surface;
- that new settlement surface is exact-pocket-only and anchored directly to
  accepted `round-111`;
- the earlier same-family dossiers remain immutable historical evidence; and
- the repo still does not gain a broad same-family or repo-level automatic
  recursive-inference success claim by implication.

This item-2 ledger does not:

- validate the new settlement surface;
- update the representative matrix in place;
- decide whether rev-004 should stop or continue after settlement;
- broaden into code changes, hardening, rollout, second interfaces,
  multi-SCC search, or fallback widening; or
- reinterpret this one exact-pocket current read into general family-wide or
  repo-wide success.

## Later-Item Handoff Ownership

Later ownership remains exactly:

- item `3` may create only the frozen validation artifact at
  `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-validation.md`,
  and only to validate that this new bounded ledger stays aligned with the
  accepted current-result anchor and preserves predecessor immutability;
- item `4` may record only one exact-pocket post-settlement outcome:
  stop after the bounded settlement, or publish one later same-family
  successor revision for further bounded same-pocket settlement; and
- no later item may reinterpret this ledger into authorization for code
  changes, second interfaces, multi-SCC search, fallback widening,
  production rollout, hardening, or broad capability claims unless a later
  accepted revision explicitly changes the live boundary.

## Docs-Only Verification Note

This round is expected to change only documentation artifacts:

- this canonical item-2 settlement ledger; and
- the standard round packet under `orchestrator/rounds/round-114/`.

Because the round does not touch `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`, no full `cabal build all && cabal test` gate is triggered at
implement time unless the diff escapes that authorized docs-only surface.
