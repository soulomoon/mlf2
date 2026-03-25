# Same-Lane Retained-Child Stable-Visible-Persistence Breakpoint Audit

Date: 2026-03-25
Round: `round-090`
Roadmap item: `item-2`
Stage: `implement`
Attempt: `attempt-2`
Retry state: active retry from `orchestrator/state.json`
(`stage_id: "item-2"`, `attempt: 2`, `latest_attempt_verdict: "rejected"`,
`latest_stage_action: "retry"`)
Live subject: docs-only breakpoint audit of the frozen same-lane
retained-child stable-visible-persistence tuple and ledger
Artifact kind: canonical docs-only breakpoint audit

## Stage Contract Freeze

This artifact implements only roadmap item `2` for `attempt-2` under the live
retry object.

Item `2` remains docs-only and breakpoint-audit-only. Its job is to audit the
live pipeline against one exact frozen pocket only: the same-lane
retained-child family anchored at `boundVarTargetRoot`, inside one
owner-local retained-child frame, on route
`sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`, with
clear-boundary status only (`boundHasForallFrom` false and `not hasForall`
true).

This retry is a repair of rejected `attempt-1`, not a new item-2 design. The
reviewed defect in `attempt-1` was that its claimed public-output breakpoint
credited the out-of-pocket unannotated variant at
`test/PipelineSpec.hs:1693-1698` (`ELam "x" (EVar "x")`). That expression is
not the frozen same-lane retained-child packet, does not preserve the same
annotated retained-child route, and cannot lawfully stand in for the frozen
tuple. This `attempt-2` audit therefore removes that credit path entirely and
uses exact-pocket evidence only.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- edits to `orchestrator/state.json`, `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, or `orchestrator/retry-subloop.md`;
- edits to `Bugs.md`;
- the item-3 minimum bounded implementation / proof slice;
- the item-4 revalidation / outcome-classification slice;
- the item-5 successor-decision slice;
- the non-local alias-bound / base-like family;
- neighboring consumer routes;
- nested-`forall`, nested owner, or nested scheme-root positive success;
- broad automatic recursive inference claims;
- equi-recursive equality or implicit unfolding;
- cyclic structural graph encoding or multi-SCC search; or
- compatibility, convenience, or default-path fallback widening.

The inherited boundary remains fixed and unchanged:

- explicit recursive annotations remain the current production baseline;
- recursive meaning remains iso-recursive only;
- no equi-recursive equality or implicit unfolding is authorized;
- no cyclic structural graph encoding or multi-SCC search is authorized;
- no second interface is authorized; and
- no compatibility, convenience, or default-path fallback widening is
  authorized.

Accepted `N14`, accepted item `5`, accepted item `6`, and accepted item `7`
remain bounded predecessor evidence only. They preserve the frozen pocket and
the honest starting posture
`admitted but not reconstruction-visible / blocker debt`, but they do not by
themselves prove accepted `stable visible persistence`.

The lawful retry goal remains narrow: determine which ledger rows are
currently satisfied for this exact frozen pocket and localize the earliest
real breakpoint without silently widening the subject or crediting later rows
after an earlier exact-pocket failure.

## Frozen Tuple Under Audit

The only lawful subject of this audit is:

- family: same-lane retained-child;
- recursive-shape anchor: `boundVarTargetRoot`;
- owner / binder frame: one owner-local retained-child frame;
- target / consumer route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`;
- quantified-boundary state: clear-boundary only
  (`boundHasForallFrom` false and `not hasForall` true); and
- current bounded helper-visible recursive fact: `containsMu True`.

Any family swap, anchor swap, owner / binder rewrite, route hop,
quantified-crossing rescue, witness-only rescue, or packet-history-only
reinterpretation is continuity failure for this audit rather than permission
to widen scope.

## Exact-Pocket Evidence Set

This `attempt-2` audit credits only the following exact-pocket anchors:

- `test/PipelineSpec.hs:1495-1499`
  - the exact frozen packet:

    ```haskell
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
      (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
    ```

- `test/PipelineSpec.hs:1523-1570`
  - the same exact packet wired onto the same-lane local retained-child root,
    then replayed through `computeResultTypeFallback`, which still yields a
    helper-visible type with `containsMu True`.
- `src/MLF/Elab/Run/ResultType/Fallback.hs:558-735`
  - the exact solver/result-type route anchors:
    `boundVarTargetRoot`, `boundHasForallFrom`, `boundVarTarget`,
    `sameLaneLocalRetainedChildTarget`, `keepTargetFinal`, and `targetC`.
- `src/MLF/Elab/Run/Pipeline.hs:127-194`
  - elaboration environment assembly via `elaborateWithEnv` plus the
    authoritative public-output path `checkedAuthoritative`.
- exact replay through `cabal repl mlf2-test` using
  `runPipelineElab`, `runPipelineElabChecked`, and `SpecUtil.unsafeNormalizeExpr`
  on the same frozen packet only.

This retry expressly excludes `test/PipelineSpec.hs:1693-1698` from credited
evidence. That unannotated variant remains useful only as a contrast proving
why the prior attempt was out of pocket.

## Exact-Pocket Replay Evidence

The replay command used for this retry was:

```sh
printf '%s\n' \
':set -isrc -itest -isrc-public' \
'import qualified Data.Set as Set' \
'import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked, renderPipelineError)' \
'import MLF.Frontend.Syntax' \
'import SpecUtil (unsafeNormalizeExpr)' \
'let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))' \
'let expr = ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))' \
'print expr' \
'print (either renderPipelineError (const "unchecked-ok") (runPipelineElab Set.empty (unsafeNormalizeExpr expr)))' \
'print (either renderPipelineError (const "checked-ok") (runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr)))' \
':quit' \
| env HOME=/tmp/codex-home CABAL_DIR=/tmp/cabal XDG_STATE_HOME=/tmp/xdg-state XDG_CACHE_HOME=/tmp/xdg-cache XDG_CONFIG_HOME=/tmp/xdg-config cabal --store-dir=/tmp/cabal/store repl mlf2-test
```

Reviewer-visible replay output for the exact frozen packet:

```text
ELet "k" (ELamAnn "x" (STMu "a" (STArrow (STVar "a") (STBase "Int"))) (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
"Phase 6 (elaboration): PhiTranslatabilityError [\"reifyInst: missing authoritative instantiation translation for edge 3\",\"expansion args=[NodeId {getNodeId = 31}]\"]"
"Phase 6 (elaboration): PhiTranslatabilityError [\"reifyInst: missing authoritative instantiation translation for edge 3\",\"expansion args=[NodeId {getNodeId = 31}]\"]"
```

The exact-pocket replay therefore shows that both `runPipelineElab` and
`runPipelineElabChecked` fail before public output is produced, and they fail
at the same earliest phase and with the same concrete elaboration error.

## Breakpoint Audit Ledger

| Phase / surface | Frozen continuity obligation | Live anchor(s) | Row result | Continuity note |
| --- | --- | --- | --- | --- |
| Solver admission state | Preserve the same family, `boundVarTargetRoot`, one owner-local retained-child frame, the route `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`, and clear-boundary status only. | `Fallback.hs:558-735`; `PipelineSpec.hs:1495-1570` | `satisfied on current evidence` | `boundVarTargetRoot` is fixed at line `558`; `boundHasForallFrom` rejects `TyForall` and nested-scheme-root crossings at lines `563-647`; candidate filtering requires `bndRoot == boundVarTargetRoot` and `not hasForall` at lines `648-696`; and the retained-child route remains `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` at lines `697-735`. The exact packet at `PipelineSpec.hs:1495-1499`, together with the same-lane local root rewiring at `1523-1569`, is the exact-pocket test harness for that route. |
| Elaboration handoff / result state | Carry the same frozen tuple into elaboration and result-type setup without replacing the owner / binder story, route, or recursive-shape anchor, and survive Phase 6 on that same packet. | `Pipeline.hs:127-152`; exact-pocket `runPipelineElab` / `runPipelineElabChecked` replay above | `first actual continuity breakpoint` | `Pipeline.hs` assembles `scopeOverrides`, `elabConfig`, and `elabEnv` from the same presolution view, bind-parent state, edge artifacts, and scope overrides, then calls `elaborateWithEnv` at line `144`. But the exact-pocket replay of both `runPipelineElab` and `runPipelineElabChecked` fails before later phases with `Phase 6 (elaboration): PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 3","expansion args=[NodeId {getNodeId = 31}]]`. That makes elaboration, not a later output surface, the earliest exact-pocket continuity break. |
| Reification / reconstruction state | Preserve the same admitted family and route into reconstructed type terms without silent unfolding or witness-only rescue. | `Fallback.hs:758-780`; `PipelineSpec.hs:1561-1570` | `not credited after earlier breakpoint` | The helper-visible exact-pocket fallback fact (`containsMu True`) still exists at `PipelineSpec.hs:1561-1570`, and `generalizeWithPlan` / scheme-to-type reconstruction still exists at `Fallback.hs:758-780`, but item `2` may not credit reconstruction after the exact public pipeline already fails earlier in Phase 6 elaboration on the same packet. |
| Internal output surface | Keep recursive structure review-visibly present on an internal output surface for the same family / anchor / frame / route. | `PipelineSpec.hs:1561-1570` | `not credited after earlier breakpoint` | The exact-pocket helper-visible `containsMu True` fact remains bounded context only. Because the exact packet already fails earlier in Phase 6 elaboration when replayed through `runPipelineElab` / `runPipelineElabChecked`, this row cannot be credited as live pipeline continuity for item `2`. |
| Public output surface | The authoritative public pipeline output must expose the same recursive structure as the internal surface for this same frozen pocket, without a second interface or diagnostic-only escape hatch. | `Pipeline.hs:182-194`; exact-pocket replay above | `not credited after earlier breakpoint` | `checkedAuthoritative` at `Pipeline.hs:182-194` is never reached for the exact frozen packet because both replay entrypoints fail earlier in Phase 6 elaboration. The prior unannotated `PipelineSpec.hs:1693-1698` row is out of pocket and no longer receives any credit here. |
| Reviewer-visible evidence trail | Reviewers must be able to point to one stable persistence tuple across every earlier row without inventing a new route, interface, or packet-local folklore story. | item-1 frozen ledger; item-5 phase ledger; exact-pocket anchors above | `not credited after earlier breakpoint` | Once the exact frozen packet already breaks in the elaboration row, reviewers cannot lawfully claim later-row continuity by substituting the unannotated variant, the helper-only fallback surface, or any packet-history-only reinterpretation. |

## Bounded Audit Conclusion

For the frozen same-lane retained-child tuple only:

- rows already satisfied on exact-pocket evidence:
  solver admission state only;
- first actual continuity breakpoint:
  `elaboration handoff / result state`;
- exact blocker phase / surface:
  both `runPipelineElab` and `runPipelineElabChecked` fail on the exact
  frozen `let k ... let u ... in u` packet in
  `Phase 6 (elaboration)` with
  `PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 3","expansion args=[NodeId {getNodeId = 31}]]`;
- later-row status:
  reification / reconstruction, internal output surface, public output
  surface, and reviewer-visible evidence trail are all
  `not credited after earlier breakpoint`; and
- bounded helper-visible context only:
  the exact-pocket `computeResultTypeFallback` replay still produces
  `containsMu True`, but that fact cannot be used to credit later ledger rows
  after the earlier exact-pocket elaboration failure.

This retry therefore localizes the earliest exact-pocket break honestly. It
does not authorize an item-3 fix slice, item-4 revalidation, item-5 decision
work, or any widening beyond the frozen same-lane retained-child subject.
