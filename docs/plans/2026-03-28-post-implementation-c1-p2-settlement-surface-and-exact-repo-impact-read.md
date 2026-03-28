# Post-Implementation `C1` / `P2` Settlement Surface And Exact Repo-Impact Read

Date: 2026-03-28
Round: `round-122`
Roadmap item: `item-3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: one post-implementation settlement read for the exact bounded
`C1` / `P2 non-local-propagation authoritative-surface` lane
Artifact kind: canonical docs-only settlement surface

## Stage Contract

This artifact implements roadmap item `3` only for `attempt-1` with
`retry: null`.

Its job is to republish the exact `C1` / `P2` read after accepted item `2`,
bind the provenance for the focused and full-gate evidence cited here, and
state the exact repo-impact of that new read without widening it into a
broader capability claim.

This artifact does not:

- reopen the settled same-lane `C2` / `C5` / `C7` pocket;
- promote `P5` into a second live lane;
- rewrite the March 27 refreshed matrix in place;
- claim that general `P2` family settlement is now complete;
- claim repo-level readiness for automatic iso-recursive inference; or
- authorize a new architecture or boundary change.

The inherited production boundary therefore remains unchanged:

- explicit recursive annotations remain the production baseline;
- recursive meaning remains iso-recursive only;
- `non-equi-recursive = keep`;
- the inherited non-cyclic structural boundary remains binding;
- `no-fallback = keep`; and
- one-interface-only remains binding.

## Authority Ledger

| Input class | Source | Binding read carried here |
| --- | --- | --- |
| Production baseline authority | `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` | The repo still does not claim general automatic recursive inference readiness. |
| Capability contract authority | `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md` | `P2 non-local-propagation` remains a required positive family for eventual repo-level readiness, but exact-packet evidence alone is not enough for that global claim. |
| Full-pipeline authority | `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md` | The decisive burden remains the current authoritative output surfaces, not helper-only or diagnostic-only reads. |
| Accepted repo-scope predecessor read | `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`; `docs/plans/2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision.md` | Before this family, `C1` remained admitted but visibly non-recursive on the authoritative public pipeline entrypoints. |
| Accepted successor freeze | `docs/plans/2026-03-28-c1-p2-authoritative-surface-successor-authority-success-bar-and-writable-slice-freeze.md` | This family is still bound to the exact admitted `C1` packet on `baseTarget -> baseC -> targetC`, the exact item-2 success bar, and the exact writable slice discipline. |
| Accepted implementation round | `orchestrator/rounds/round-121/implementation-notes.md`; `orchestrator/rounds/round-121/review-record.json` | Accepted item `2` introduced one bounded pipeline-surface preservation step, updated focused and production-path regressions, and passed both focused reruns plus the full repo gate. |

## Exact Post-Implementation Read

For the exact admitted `C1` packet on `baseTarget -> baseC -> targetC`, the
accepted post-item-2 read is now:

- fallback surface:
  still `TBase (BaseTy "Int")` with `containsMu False`
- authoritative public pipeline entrypoints:
  both `runPipelineElab` and `runPipelineElabChecked` now expose recursive
  structure for the exact packet, with `containsMu True`
- exact current classification:
  `exact C1/P2 packet settled within the current architecture`

This means the exact bounded family debt carried into this roadmap has changed:
the exact admitted `C1` packet is no longer
`admitted but not reconstruction-visible / blocker debt`.

## Evidence Provenance

The exact read above is carried only from accepted item-2 evidence:

- focused harness:
  `cabal test mlf2-test --builddir=dist-newstyle-round121-c1 --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'`
  passed
- production-path regression:
  `cabal test mlf2-test --builddir=dist-newstyle-round121-pipeline --test-show-details=direct --test-options='--match "selected non-local scheme-alias/base-like packet recursive on both authoritative pipeline entrypoints"'`
  passed
- repo verification gate:
  `cabal build all && cabal test`
  passed with `1149 examples, 0 failures`
- review validation:
  `orchestrator/rounds/round-121/review-record.json`
  finalized item `2` as `accepted + finalize`

No new implementation or new test rerun is performed in this item-3 round.
This artifact republishes the accepted item-2 evidence only.

## Exact Repo-Impact Read

The exact repo-impact of the accepted item-2 result is narrower than a new
repo-scope capability claim:

- the bounded `C1` representative packet for `P2` is now settled on the
  authoritative surfaces inside the inherited current architecture;
- the settled same-lane `C2` / `C5` / `C7` pocket remains settled predecessor
  truth only;
- `P5` remains reject-side pressure only and is still out of scope for this
  family; and
- repo-level readiness for automatic iso-recursive inference is still not
  cleared by this artifact alone.

This item therefore records a real semantic improvement, but not a global
clearance.

## Non-Claims

This artifact does not claim:

- general `P2` family closure;
- repo-wide representative success across the entire positive corpus;
- automatic iso-recursive inference readiness;
- any new cyclic or multi-SCC capability;
- any fallback widening; or
- any second interface.

## Next Lawful Move

The next lawful move after this settlement surface is roadmap item `4` only:

`Record one successor gate and immediate handoff after the bounded C1/P2 lane`

That gate must convert this exact post-implementation read into exactly one
current outcome and exactly one immediate handoff, without silently widening
the claim made here.
