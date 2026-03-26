# Global `non-cyclic-graph` Same-Lane Retained-Child Public-Output Continuity Authoritative-Handoff Architecture-Amendment Contract And Writable Slice Freeze

Date: 2026-03-26
Round: `round-109`
Roadmap item: `item-1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one docs-only rev-003 item-1 aggregate freeze for the exact
same-pocket authoritative-handoff architecture-amendment contract and one
exact future writable slice inside the bounded same-family lane accepted in
`round-108`
Artifact kind: canonical docs-only rev-003 item-1 aggregate contract-freeze
record

## Stage Contract Freeze

This artifact implements only roadmap item `1` for `attempt-1` with
`retry: null`.

It consumes the live round-109 control-plane inputs plus accepted
`round-108` / rev-002 predecessor truth and freezes exactly this much, and
no more:

- rev-003 item `1` remains docs-only, planning-only, and architecture-only;
- accepted `round-108` / rev-002 item `5` remains authoritative;
- accepted rev-002 items `1` through `4` remain binding and are not
  relitigated;
- exactly the same selected rows `C2`, `C5`, and `C7` remain in scope;
- exactly the same exact packet and same exact tuple remain in scope;
- exactly one same-pocket authoritative-handoff slice is frozen:
  `runPipelineElabWith` / `checkedAuthoritative` /
  `typeCheck termClosed`;
- exactly one future writable slice is frozen for later item `2` only:
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/TermClosure.hs`, and
  `test/PipelineSpec.hs`; and
- exactly five read-only audit anchors remain frozen as read-only:
  `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  `src/MLF/Elab/Run/ResultType.hs`,
  `src/MLF/Elab/Run.hs`,
  `src/MLF/Elab/Pipeline.hs`, and
  `src-public/MLF/Pipeline.hs`.

This artifact does not:

- reopen subject selection or reinterpret the accepted lane-open result;
- treat `C5` or `C7` as second packets, second routes, second interfaces, or
  separate architecture-amendment lanes;
- widen the selected pocket beyond the same exact packet, tuple, and
  authoritative-handoff slice;
- authorize source edits, test edits, Cabal edits, `Bugs.md` edits, or
  `orchestrator/state.json` edits in this round;
- convert any read-only audit anchor into part of the future writable slice;
- or
- authorize multi-SCC search, second interfaces, fallback widening,
  production rollout, hardening, or broad capability claims.

The inherited baseline also remains unchanged:

- `iso-recursive = keep`;
- `non-equi-recursive = keep`;
- `no-fallback = keep`;
- explicit recursive annotations remain the production baseline;
- equi-recursive semantics and implicit unfolding remain blocked; and
- rev-001 items `6` through `8` remain blocked.

## Round-109 Control-Plane Inputs

| Control-plane input | Binding read carried forward |
| --- | --- |
| `orchestrator/rounds/round-109/selection.md` | This round is item-1-only, docs-only, aggregate-only, and limited to freezing one exact same-pocket architecture-amendment contract plus one exact future writable slice. |
| `orchestrator/rounds/round-109/plan.md` | The output must preserve accepted `round-108`, preserve accepted rev-002 items `1` through `4`, freeze the same exact pocket and authoritative-handoff slice, and keep all scope widening blocked. |
| `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/roadmap.md` | Item `1` is the only dependency-free unfinished item, and completion requires one accepted docs-only artifact freezing the exact selected rows, packet, tuple, authoritative-handoff slice, future writable boundary, and read-only anchors. |
| `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/retry-subloop.md` | Item `1` is aggregate-only, may not use `accepted + retry`, and must finalize or reject without rewriting prior accepted truth. |
| `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/verification.md` | Lane-freeze verification must prove the artifact narrows only to the same exact `C2` / `C5` / `C7` pocket, the same exact packet and tuple, one exact authoritative-handoff slice, one exact writable boundary, and no silent broadening. |

These inputs define the live item-1 contract only. They do not themselves
replace accepted predecessor truth.

## Accepted Predecessor Authority

| Accepted predecessor artifact | Binding truth carried forward |
| --- | --- |
| `orchestrator/rounds/round-108/review-record.json` | Accepted `round-108` made rev-002 item `5` authoritative with final outcome `global-non-cyclic-graph-selected-pocket-opens-one-bounded-same-family-architecture-amendment-lane`. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-amendment-lane-open-or-stop-decision.md` | Accepted rev-002 item `5` opened exactly one bounded same-family architecture-amendment lane for the same exact `C2` / `C5` / `C7` pocket only and preserved all inherited keep axes and blocked work. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-reopened-revision-authority-and-candidate-boundary-freeze.md` | Accepted rev-002 item `1` froze exactly one planning-only candidate boundary, kept rev-001 items `6` through `8` blocked, and kept multi-SCC search, second interfaces, fallback widening, hardening, and broad capability claims blocked. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-reopened-subject-selection.md` | Accepted rev-002 item `2` selected exactly one live reopened subject: the same exact same-lane retained-child / public-output continuity pocket already carried by `C2`, `C5`, and `C7`, with non-local `C1` retained only as contrast context. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-safety-and-acceptance-contract.md` | Accepted rev-002 item `3` froze the exact subject contract and lawful bars for that one pocket only while preserving `iso-recursive = keep`, `non-equi-recursive = keep`, and `no-fallback = keep`. |
| `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-pressure-audit-target-and-evaluation-surface-bind.md` | Accepted rev-002 item `4` froze the exact rows, exact packet, exact tuple, exact module set, exact command set, and exact six review-visible surfaces for the same selected pocket only. |
| `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md` | Accepted path audit fixed `checkedAuthoritative` as the first exact public-output break for this same pocket, with `termClosed` and `typeCheck termClosed` as same-pocket dependencies. |
| `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md` | Accepted blocker-proof fixed that the bounded `Pipeline.hs` / `TermClosure.hs` root-handoff slice contains no alternate recursive whole-packet authoritative result for this same exact pocket inside the inherited surface set. |
| `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` | The inherited repo baseline remains explicit-only in production, iso-recursive only, non-equi-recursive, no-fallback, and structurally non-cyclic by default unless a later accepted revision explicitly changes that status. |

These accepted artifacts are predecessor authority only. None of them
already lands item `2`, validates item `3`, or decides item `4`.

## Exact Same-Pocket Boundary

This freeze binds exactly one same-pocket subject and no other:

`the exact same-lane retained-child / public-output continuity pocket already carried by accepted rows C2, C5, and C7, with non-local C1 retained only as bounded contrast context`

### Exact Same-Pocket Rows

| Row | Exact same-pocket role that remains binding | What item `1` freezes for later work | Bound that must remain closed |
| --- | --- | --- | --- |
| `C2` | primary same-lane retained-child row for the selected pocket | same family / anchor / owner-local frame / route identity and same helper-visible retained-child continuity on the exact packet | not a second packet, not a broader same-family success claim, not a family-wide implementation target |
| `C5` | same exact `C2` pocket through the owner / binder-sensitive placement lens only | same local `TypeRef` lane, same owner-local frame, and same nested-`forall` fail-closed guard context | not a second owner-local packet, not a second route, not a widened writable boundary |
| `C7` | same exact `C2` / `C5` pocket through the public-output continuity lens only | exact authoritative public-output split and exact authoritative-handoff slice for the same packet | not a second interface, not a second public path, not a broad repo-level capability claim |

`C1` may appear only as bounded contrast context. It must not become a
second selected subject, a second packet, a second route, a second
interface, or a reason to widen the writable boundary.

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

## Exact Authoritative-Handoff Slice

The contract freeze centers on exactly one authoritative-handoff slice and
no other:

- `runPipelineElabWith`
- `checkedAuthoritative`
- `typeCheck termClosed`

This slice remains exact-pocket-only.

Why this is the exact slice:

1. Accepted predecessor path audit already fixed
   `checkedAuthoritative` as the first exact public-output break for the
   selected pocket.
2. The same accepted audit fixed `termClosed` and `typeCheck termClosed` as
   the exact same-pocket dependencies feeding that public result.
3. Accepted blocker-proof fixed that the bounded
   `Pipeline.hs` / `TermClosure.hs` root-handoff slice contains no alternate
   whole-packet authoritative recursive result inside the inherited surface
   set.

This artifact therefore freezes only the future architecture-amendment
contract around that same exact handoff. It does not reopen:

- helper-route ownership;
- the accepted family / anchor / owner-local frame / route identity;
- a second public interface;
- fallback behavior or a new fallback path; or
- any broader packet, lane, or family claim.

## Exact Future Writable Slice

The one exact future writable slice for later item `2` is:

| Path | Why it is the only future writable path | What remains forbidden even there |
| --- | --- | --- |
| `src/MLF/Elab/Run/Pipeline.hs` | owns `runPipelineElabWith`, `termClosed`, and `checkedAuthoritative`, so it contains the exact authoritative-handoff implementation site | no second interface, no route reselection, no broader family widening, no fallback redesign |
| `src/MLF/Elab/TermClosure.hs` | remains inside the accepted bounded root-handoff slice that must stay exact-pocket-only | no general term-closure rewrite, no broader recursive-closure search, no multi-SCC behavior |
| `test/PipelineSpec.hs` | is the exact-pocket regression surface for the same selected packet and same-pocket continuity faces only | no broad new corpus, no second packet harness, no family-wide success narration |

No other source file, test file, Cabal file, executable path, public entry
module, or helper module is part of the writable boundary.

Item `1` itself does not edit this future writable slice. It freezes the
boundary only for later item `2`.

## Exact Read-Only Audit Anchors

The following paths remain frozen as read-only audit anchors for this lane:

| Path | Why it remains read-only in rev-003 item `1` |
| --- | --- |
| `src/MLF/Elab/Run/ResultType/Fallback.hs` | owns the accepted same-lane route, guard, owner-local lookup anchors, and `boundVarTargetRoot`, and must stay audit-only rather than becoming part of the writable slice |
| `src/MLF/Elab/Run/ResultType.hs` | owns dispatch into fallback reconstruction and remains audit-only so item `1` does not widen into helper-route redesign |
| `src/MLF/Elab/Run.hs` | preserves the one public-entrypoint re-export chain and keeps second-interface work blocked |
| `src/MLF/Elab/Pipeline.hs` | preserves the same one-interface chain and remains read-only unless a later accepted revision explicitly changes that boundary |
| `src-public/MLF/Pipeline.hs` | preserves the public exposed interface and keeps second-interface and rollout widening blocked |

If any later narration turns a read-only anchor into part of the writable
slice without a later accepted revision explicitly reopening that boundary,
it has already widened beyond rev-003 item `1`.

## Blocked Scope Preserved

All of the following remain explicitly blocked by this item-1 freeze:

- any change to accepted `round-108` / rev-002 item-5 truth or accepted
  rev-002 items `1` through `4` truth;
- any reopening of the selected subject boundary or any promotion of `C1`
  into a coequal subject, packet, route, or interface;
- any treatment of `C5` or `C7` as second packets, second routes, second
  interfaces, or separate architecture-amendment lanes;
- any widening beyond the exact selected packet, exact tuple, or exact
  authoritative-handoff slice;
- any widening of the writable boundary beyond
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/TermClosure.hs`, and
  `test/PipelineSpec.hs`;
- any conversion of the read-only audit anchors into writable paths;
- any multi-SCC search, second interfaces, fallback widening,
  equi-recursive semantics, implicit unfolding, production rollout,
  hardening, or broad capability claims;
- any claim that rev-001 items `6` through `8` are unblocked by
  implication;
- any source-code, test, Cabal, or `orchestrator/state.json` edits in this
  round; and
- any commit.

If later implementation starts widening into helper-route redesign, public
interface redesign, a second packet, a second same-family lane, or any
source/test edit during item `1`, it has already exceeded the accepted
rev-003 boundary and must fail closed.

## Later-Item Handoff Ownership

This artifact is the rev-003 item-1 boundary contract for later work only.

Later ownership remains exactly:

- item `2` may change only the frozen writable slice above, and only to
  carry one bounded single-component cyclic-structure result through the
  exact `runPipelineElabWith` / `checkedAuthoritative` /
  `typeCheck termClosed` handoff path for the same selected pocket, without
  changing subject selection, helper-route ownership, public-entrypoint
  count, fallback behavior, or the inherited keep axes;
- item `3` may validate only the exact `C2` / `C5` / `C7` rows, the same
  exact packet, the exact command set, and the exact six review-visible
  surfaces already frozen by accepted rev-002 item `4`;
- item `4` may record only one exact-pocket post-amendment outcome:
  publish one later same-family successor revision for further bounded
  settlement, or stop without broader rollout; and
- no later item may reinterpret this freeze into authorization for
  multi-SCC search, second interfaces, fallback widening, production
  rollout, hardening, or broad capability claims unless a later accepted
  revision explicitly changes the live boundary.

Rev-003 item `1` therefore freezes one exact same-pocket contract, one exact
authoritative-handoff slice, one exact future writable slice, and one exact
read-only audit-anchor set. It does not itself perform implementation,
validation, or follow-on decision work.
