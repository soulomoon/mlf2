# Post-Rev-004 Repo-Scope Refreshed Representative Family-Matrix Settlement Surface And Provenance Validation

Date: 2026-03-27
Round: `round-118`
Roadmap item: `item-2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one refreshed repo-scope representative family-matrix
settlement surface after the accepted post-rev-004 successor-boundary freeze
Artifact kind: canonical docs-only refreshed matrix and provenance-validation
surface

## Stage Contract Freeze

This artifact implements only roadmap item `2` for `attempt-1` with
`retry: null`.

This round republishes one refreshed repo-scope representative matrix on a
new round-owned surface only. It consumes the accepted post-rev-004 boundary
freeze, the inherited March 14 and March 25 contracts, the immutable March 26
matrix and global gate as historical evidence only, the accepted rev-004
same-lane settlement chain as carried-forward predecessor truth only, and any
fresh round-118 `C1` / `P5` provenance that can be republished lawfully.

This round does not:

- back-edit the March 26 representative matrix or the March 26 global gate in
  place;
- reopen the settled same-lane `C2` / `C5` / `C7` pocket as live repair debt;
- decide roadmap item `3`;
- claim repo-level capability success;
- authorize production implementation, hardening, rollout, cyclic search,
  multi-SCC search, a second interface, or fallback widening; or
- treat `Bugs.md`, local task drafts, or unrepublished harness context as
  coequal authority.

The inherited production boundary remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- `non-equi-recursive = keep`;
- the inherited non-cyclic structural boundary remains unchanged in this item;
- `no-fallback = keep`; and
- one-interface-only remains binding.

## Item-2 Authority And Evidence-Input Ledger

| Input class | Source | Binding read carried forward here |
| --- | --- | --- |
| Inherited baseline authority | `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` | The repo remains explicit-only in production, iso-recursive only, non-equi-recursive, structurally non-cyclic by baseline, and no-fallback. |
| Repo-scope capability authority | `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md` | The representative family matrix remains the binding repo-scope success surface, and one bounded packet still does not count as repo-level capability truth. |
| Full-pipeline visibility authority | `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md` | `stable visible persistence` remains the only lawful positive `P6` success token; solver-only, helper-only, witness-only, or internal-only reads remain non-success. |
| Strategic predecessor context only | `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md` | The March 25 posture remains predecessor context only and is not the current repo-scope answer after the rev-004 same-lane repair. |
| Historical aggregate matrix only | `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md` | The March 26 matrix remains immutable historical evidence on pre-amendment inputs only. |
| Historical aggregate gate only | `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md` | The March 26 global `keep` vs `reopen` gate remains immutable historical evidence only. |
| Accepted rev-004 same-lane settlement chain | `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`; `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-validation.md`; `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md` | The same-lane `C2` / `C5` / `C7` pocket is closed, validated, and carried forward as settled predecessor truth only. |
| Live repo-scope boundary freeze | `docs/plans/2026-03-27-post-rev-004-repo-scope-successor-authority-evidence-inputs-and-non-widening-boundary-freeze.md` | Item `2` is the next lawful move, and the refreshed matrix must carry the same-lane pocket forward as predecessor truth instead of reopening it. |
| Fresh round-118 `C1` provenance | `orchestrator/rounds/round-118/lanes/c1-provenance-summary.md`; `test/Research/C1AuthoritativeSurfaceSpec.hs` | Lawful fresh republication exists for the bounded `C1` authoritative-surface read because the harness reran successfully under an isolated build dir. |
| Fresh round-118 `P5` provenance | `orchestrator/rounds/round-118/lanes/p5-provenance-summary.md`; `test/Research/P5ClearBoundarySpec.hs` | Lawful fresh republication exists for the bounded clear-boundary versus nested-`forall` contrast because the harness reran successfully under an isolated build dir. |
| Non-authoritative bug context only | `Bugs.md` | `BUG-2026-03-16-001` remains bug context only and does not define current repo-scope row truth for this item. |

This refreshed matrix is a new round-owned settlement surface only. No older
matrix, older gate, or older same-lane dossier is edited in place.

## Refreshed Representative Matrix

| Row | Controlling family | Exact current read | Evidence source | Bounded classification |
| --- | --- | --- | --- | --- |
| `P1-row` | `P1 local-recursive-shape` | Bounded local same-lane support still exists, but the corresponding unannotated variant still does not infer recursive shape on the current pipeline surfaces. | March 26 representative matrix; round-117 boundary freeze | historical carry-forward only; below review-visible unannotated automatic success |
| `C1` | `P2 non-local-propagation` | Fallback surface: `TBase (BaseTy "Int")` with `containsMu False`.<br>Public pipeline entrypoints: both `runPipelineElab` and `runPipelineElabChecked` return `TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))` with `containsMu False`. | round-118 `C1` provenance summary; `test/Research/C1AuthoritativeSurfaceSpec.hs`; round-117 boundary freeze | fresh republished bounded non-success; the admitted non-local packet remains visibly non-recursive on current authoritative surfaces |
| `C2` | `P3 retained-child-owner-sensitive` | For the exact same packet only, helper-visible/internal fallback remains `TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))` with `containsMu True`, and both authoritative public entrypoints return `Right (TForall "a" Nothing (TArrow (TVar "t31") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))))`. | accepted rev-004 settlement ledger, validation, and handoff docs | carried-forward settled exact-pocket predecessor truth only; not broader `P3` family settlement |
| `C3` | `P5 polymorphism-nested-forall`; `N2 unsoundness-guard` | While the quantified boundary stays clear, the same-lane retained-child contrast remains recursive on the fallback surface (`containsMu True`); once the same wrapper crosses a nested `forall`, the fallback surface fails closed (`containsMu False`). | round-118 `P5` provenance summary; `test/Research/P5ClearBoundarySpec.hs`; March 26 `C3` / `C7` slice for reject-side interpretation | fresh reject-side contrast carry-forward only; positive `P5` success remains unearned |
| `C4` | `N1 ambiguity-reject` | Ambiguity still fails closed instead of being resolved by ranking or guesswork. | March 26 representative matrix | historical carry-forward only; fail-closed ambiguity guard |
| `C5` | `P4 binder-sensitive-placement`; `P3`; `N2` | The same exact `C2` packet, the same owner-local frame, and the same clear-boundary-only status remain the only lawful owner-sensitive reread of the post-amendment result; the carried-forward authoritative public read is the same bounded recursive structure recorded for `C2`, and no second packet appears. | accepted rev-004 settlement ledger, validation, and handoff docs | carried-forward settled exact-pocket predecessor truth only; not broader `P4` settlement and not a second packet |
| `C6` | `N6 termination-pressure`; `N4`; `N5` | Bounded rejection still contains forbidden growth inside the inherited acyclic model, and no current repo-scope evidence here requires cyclic search, multi-SCC search, or a second interface. | March 26 representative matrix; round-117 boundary freeze | historical carry-forward only; bounded rejection / pressure context |
| `C7` | `P6 reconstruction-visible-output` | The same exact packet no longer collapses to `Right (TForall "a" Nothing (TVar "a"))`; helper-visible/internal fallback still carries `TMu` structure with `containsMu True`, and both authoritative public entrypoints now carry `Right (TForall "a" Nothing (TArrow (TVar "t31") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))))` for that one exact packet only. | accepted rev-004 settlement ledger, validation, and handoff docs | carried-forward settled exact-pocket predecessor truth only; not repo-scope `P6` settlement |

## Provenance Validation

Fresh round-118 harness evidence is included only where lawful provenance
exists.

- `C1` fresh republication is included because
  `test/Research/C1AuthoritativeSurfaceSpec.hs` exists and reran under
  `dist-newstyle-round118-c1`.
- `P5` fresh republication is included because
  `test/Research/P5ClearBoundarySpec.hs` exists and reran under
  `dist-newstyle-round118-p5`.
- `C2` / `C5` / `C7` were not rerun in this round. They remain carried-forward
  accepted rev-004 predecessor truth only so that the same-lane pocket stays
  closed.
- `P1-row`, `C4`, and `C6` remain historical carry-forward rows only because
  no new lawful item-2 lane was authorized or required for them.
- The March 26 representative matrix and the March 26 global gate remain
  immutable historical evidence only and are not edited in place here.

If either fresh lane had failed, gone missing, or lacked isolated-build
provenance, this artifact would have left that row on historical carry-forward
only. That fail-closed fallback was not needed because both fresh lanes passed
their isolated reruns.

## Repo-Scope Read After The Refresh

The refreshed repo-scope matrix now reads as follows:

- the same-lane `C2` / `C5` / `C7` pocket survives only as settled
  predecessor truth after the accepted rev-004 repair and handoff;
- fresh republished `C1` evidence still shows a bounded admitted packet that
  remains visibly non-recursive on current authoritative surfaces;
- fresh republished `P5` evidence still shows only the clear-boundary versus
  nested-`forall` contrast, with reject-side behavior once the quantified
  boundary is crossed; and
- no item-3 successor posture is selected here.

This artifact therefore stops at refreshed matrix publication and provenance
validation only.
