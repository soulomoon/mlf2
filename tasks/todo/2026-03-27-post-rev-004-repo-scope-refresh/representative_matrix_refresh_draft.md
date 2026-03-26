# Draft Representative Matrix Refresh After `rev-004`

Status: draft only. Task-local and non-authoritative. This file does not
change accepted controller truth, accepted March 25 / March 26 artifacts, or
`orchestrator/state.json`.

## Purpose

Refresh the repo-scope representative matrix by carrying forward three facts
that the older March 26 aggregate matrix did not yet contain:

- the accepted exact-pocket `rev-003` / `rev-004` repair for the same-lane
  `C2` / `C5` / `C7` packet on authoritative public surfaces;
- the new bounded `C1` authoritative-surface harness proving the admitted
  non-local packet still ends visibly non-recursive; and
- the new bounded `P5` probe proving the clear-boundary control still stays
  recursive while the nested-`forall` contrast still fails closed.

## Accepted Authority Reused Without Rewriting

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-settlement-ledger.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md`

The March 26 matrix and gate remain historical evidence exactly as written.
This draft only asks what the matrix would read if those accepted exact-pocket
repairs were carried forward honestly.

## Fresh Bounded Evidence Added

- `test/Research/C1AuthoritativeSurfaceSpec.hs`
  confirms:
  the admitted `C1` helper packet still reads `TBase (BaseTy "Int")` with
  `containsMu False`, and the exact source packet stays non-recursive on both
  current pipeline entrypoints.
- `test/Research/P5ClearBoundarySpec.hs`
  confirms:
  the same-lane clear-boundary retained-child control stays recursive, while
  the nested-`forall` contrast still fails closed.
- Focused reruns on 2026-03-27 passed:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`

## Refreshed Matrix Summary

| Row | Controlling family or families | Refreshed current read | Draft classification | Why the row is still bounded |
| --- | --- | --- | --- | --- |
| `P1-row` | `P1 local-recursive-shape` | bounded local support still exists, but the corresponding unannotated variant still does not infer recursive shape on the current pipeline surfaces | `admitted but not reconstruction-visible / blocker debt` | the row still proves only local bounded support, not review-visible unannotated automatic success |
| `C1` | `P2`, bounded continuity context for `P3` and `P6` | the admitted non-local alias-bound / base-like packet on `baseTarget -> baseC -> targetC` still ends visibly non-recursive (`TBase (BaseTy "Int")`, `containsMu False`) | `admitted but not reconstruction-visible / blocker debt` | the row still shows one admitted non-local packet only, not stable recursive persistence on current authoritative surfaces |
| `C2` | `P3`, bounded continuity context for `P6` | helper-visible/internal recursion is preserved and both authoritative public entrypoints now also carry bounded recursive structure on the same exact packet and route | `stable visible persistence` | this is one exact same-lane pocket only; it does not by itself settle the broader `P3` family at repo scope |
| `C3` | `P5`, `N2` | quantified crossing is still detected and still invalidates the same-lane retained-child candidate before a lawful recursive output survives | `fail-closed rejection` | `P5` remains reject-side only; the clear-boundary control does not convert the nested-`forall` contrast into positive `P5` success |
| `C4` | `N1 ambiguity-reject` | ambiguity still fails closed instead of being solved by ranking or guessing | `fail-closed rejection` | this remains negative-family discipline only |
| `C5` | `P4`, `P3`, `N2` | the same exact `C2` packet now also preserves recursive structure through the owner-sensitive / binder-sensitive placement lens on both internal and public surfaces | `stable visible persistence` | this is still one exact owner-local pocket only, not broad binder-sensitive placement coverage |
| `C6` | `N6`, `N4`, `N5` | bounded rejection still contains forbidden growth inside the inherited model; no row now proves cyclic structure, multi-SCC search, or a second interface is required | `fail-closed rejection` | this remains bounded pressure context, not positive support |
| `C7` | `P6 reconstruction-visible-output` | the same exact `C2` pocket now keeps recursive structure review-visible on both internal and public output surfaces | `stable visible persistence` | this establishes exact-pocket output continuity only; it does not by itself settle repo-level `P6` representativeness |

## Refreshed Positive-Family Ledger

| Family | Strongest current representative row(s) on this draft read | Draft current read | Why the family still stops short of a repo-level capability claim |
| --- | --- | --- | --- |
| `P1 local-recursive-shape` | `P1-row` | bounded local support only | the corresponding unannotated variant still does not infer recursive shape on the current surfaces |
| `P2 non-local-propagation` | `C1` | admitted packet only | non-local propagation still ends in visibly non-recursive output |
| `P3 retained-child-owner-sensitive` | `C2` primary, with bounded contrast from `C1` and `C5` | one exact stable-visible same-lane pocket now exists | one exact pocket is real evidence, but not yet representative family-wide settlement |
| `P4 binder-sensitive-placement` | `C5` primary, reusing the exact `C2` pocket | one exact stable-visible owner-local pocket now exists | broader binder-sensitive placement coverage is still not proven beyond that one exact pocket |
| `P5 polymorphism-nested-forall` | `C3` | reject-side only | quantified crossing still invalidates the candidate before a lawful recursive output survives |
| `P6 reconstruction-visible-output` | `C7` primary, with `C2` continuity support and `C1` contrast | one exact stable-visible output-continuity pocket now exists | exact-pocket success is no longer zero, but repo-level representative visibility is still not established across the broader matrix |

## Refreshed Negative And Bounded-Family Ledger

| Family | Representative row(s) | Draft current read |
| --- | --- | --- |
| `N1 ambiguity-reject` | `C4` | still fail closed |
| `N2 unsoundness-guard` | `C3`, `C5` | quantified crossing remains reject-side; the successful same-lane owner-local pocket still stays inside the clear-boundary guard |
| `N3 equi-recursive-required` | inherited boundary carry-forward only | still out of scope under the unchanged baseline |
| `N4 cyclic-or-multi-scc-required` | `C6` plus repo-scope aggregate reasoning | still pressure context only; no refreshed row yet proves cyclic or multi-SCC behavior is required |
| `N5 second-interface-or-fallback-required` | inherited boundary carry-forward, with `C6` as contrast | still out of scope |
| `N6 termination-pressure` | `C6` | still bounded or fail-closed rejection |

## Draft Tally And Repo-Scope Consequences

Draft tally on this refreshed read:

- `stable visible persistence`: `C2`, `C5`, `C7`
- `admitted but not reconstruction-visible / blocker debt`:
  `P1-row`, `C1`
- `fail-closed rejection`: `C3`, `C4`, `C6`

Immediate consequences:

1. The March 26 aggregate claim
   `zero stable visible persistence rows`
   is now stale if the accepted rev-003 / rev-004 exact-pocket repair is
   carried forward honestly.
2. The strongest same-lane `C2` / `C5` / `C7` pocket is no longer blocker debt
   on output continuity; it now contributes exact-pocket stable visible
   persistence inside the inherited acyclic model.
3. The remaining live positive-family blockers narrow to:
   `P1` local unannotated automatic success,
   `P2` non-local propagation visibility, and
   `P5` nested-`forall` interaction.
4. This refreshed read still does not justify a repo-level capability claim.
   Representative coverage across positive families remains incomplete, and
   exact-pocket success must not be silently widened into family-wide or
   repo-wide settlement.
