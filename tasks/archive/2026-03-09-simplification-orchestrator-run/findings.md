# Findings

## Session Baseline
- Orchestration run started at 2026-03-08T16:02:19Z from commit 68cb5b1007966bce282c6a6640b07e4b70a994df on `master`.
- Working rule: preserve thesis-exact behavior from `papers/these-finale-english.txt`; consult `papers/xmlf.txt` only when the thesis is silent.
- Repo context reviewed: `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, `Bugs.md`.

## Round 1
- Accepted idea: Extract the shared snapshot-canonicalization prelude between `Finalize` and `Presolution.View`.
- Thinker focus: the live duplication between `MLF.Constraint.Finalize` and `MLF.Constraint.Presolution.View` was still worth simplifying; the stale `Solved` facade TODO and the intentional result-type cycle-break helper were not.
- Initial verifier gate: YES — the duplicate snapshot-UF sanitization/canonicalization prelude was still live and bounded.
- Planner boundary: share only sanitized-UF/canonical-map/query preparation; preserve the semantic split between raw snapshot rewriting and finalized/repaired canonical constraints.
- Implementation result: `MLF.Constraint.Presolution.View` now owns `SnapshotPreparation`, `prepareSnapshotPreparation`, `prepareSnapshotPreparationFromParts`, and `buildPresolutionView`; `MLF.Constraint.Finalize` reuses that preparation for `stepSanitizeSnapshotUf`, `presolutionViewFromSnapshot`, and `finalizePresolutionViewFromSnapshot`.
- Validation: `PresolutionView mirrors solved canonical/node/bound queries` — PASS; `fromSolveOutput matches explicit pre-rewrite snapshot construction` — PASS; `cabal build all && cabal test` — PASS (`1005 examples, 0 failures`).
- Final verifier gate: YES — helper ownership changed, but the raw-vs-finalized canonical-constraint split stayed intact.
- Integration: committed on `codex/round-1-snapshot-canonicalization-prelude` and merged to `master` as 6dcc7fd100e7d8eb66c640568c1b193c8f998a4a.
- Safety adjustment: the in-flight `Presolution.Core` facade-removal candidate remains unmerged and will be reconsidered only in a later round against the new `master` baseline.

## Round 2
- Candidate accepted for planning: retire the still-live `Presolution.Core` compatibility facade and import the owner modules directly from the public presolution boundary.
- Validation result: `cabal build all && cabal test` stayed green while removing the thin presolution compatibility facade from source and Cabal.
- Round 2 result: retired the thin presolution compatibility facade from the public Phase 4 boundary, preserved the export surface, and removed the extra Cabal/module-maintenance hop.

## Round 3
- Candidate accepted for planning: retire the unused flush-all pending-weaken entrypoint and keep only owner-boundary flushing in the live presolution path.
- Round 2 verifier re-check (2026-03-08T17:02:48Z): the `MLF.Reify.Core` `solvedFromView` seam is still live on `master` (`src/MLF/Reify/Core.hs:34`, `src/MLF/Reify/Core.hs:686`, `src/MLF/Reify/Core.hs:703`, `src/MLF/Reify/Core.hs:732`, `src/MLF/Reify/Core.hs:783`, `src/MLF/Reify/Core.hs:808`, `src/MLF/Reify/Core.hs:813`, `src/MLF/Reify/Core.hs:867`).
- `PresolutionView` still carries the exact original/canonical artifacts that reify reads structurally (`pvConstraint`, `pvCanonicalMap`, `pvCanonical`, `pvCanonicalConstraint` in `src/MLF/Constraint/Presolution/View.hs:29-38`), and existing parity guards already assert view-vs-solved agreement for canonical/node/bound queries (`test/Constraint/SolvedSpec.hs:290`, `test/PipelineSpec.hs:564`).
- The remaining refactor looks bounded rather than cross-cutting: `MLF.Reify.Core` can derive order keys directly with `MLF.Util.Order.orderKeysFromConstraintWith` (`src/MLF/Util/Order.hs:50`) and can localize free-variable traversal over `pvConstraint` + `pvCanonical` without keeping the private `Solved.Internal` rebuild path.
- Thesis alignment remains structural: Chapter 15 translates types from the constraint graph, named nodes, and `<P` ordering over a rigid presolution (`papers/these-finale-english.txt:13547-13596`), not through a repository-local `Solved` wrapper.
- Caution: the rewrite must preserve the current split between original-constraint lookups (`VarStore.lookupVarBound` / bind-parent traversal) and canonical-constraint reads (`cNodes`, weakened/eliminated sets), because that split is explicit in `src/MLF/Reify/Core.hs:97-104`.
- Round 3 result: retired the dead flush-all delayed-weaken entrypoint, leaving owner-boundary flushing as the sole live presolution drain surface and guarding it in the row3 test stack.

## Round 4
- Candidate accepted for planning: retire stale internal presolution re-export routes so `Driver`, `EdgeProcessing`, and `EdgeProcessing.Solve` match their actual ownership split.
- Round 4 result: retired stale internal presolution export surfaces so internal modules now expose only what they actually own, with a focused guard keeping the public boundary intact.

## Round 5
- Candidate accepted for planning: collapse duplicated result-type bound-overlay routing into one explicit query boundary instead of rebuilding overlayed `PresolutionView`s in parallel.
- Round 5 result: single-sourced result-type overlay queries through `ResultType.View`, removing the duplicate fallback-local overlay rebuild while keeping row-2/result-type behavior green.

## Round 6
- Candidate accepted for planning: retire the one-off reader-style canonical access layer so presolution state access uses a single `PresolutionM` idiom.
- Round 6 result: retired the one-off reader-style presolution access layer so `EdgeUnify` and the rest of presolution now use a single direct `PresolutionM` access idiom.

## Round 7
- Candidate accepted for planning: collapse the single-consumer `EdgeProcessing.Witness` wrapper so edge witness/trace assembly lives under one canonical witness owner.
- Round 7 result: retired the single-consumer edge witness wrapper so interpreter witness/trace assembly now lives under one canonical witness owner.

## Round 8
- Candidate accepted for planning: trim `ResultType.View` down to overlay-aware queries and route plain context reads directly through `ResultTypeInputs`.
- Round 8 result: narrowed `ResultType.View` to the overlay-aware boundary only, routing plain result-type context reads directly through `ResultTypeInputs`/`ChiQuery`.
- Round 9 result: single-sourced annotated result-type recursion through the existing facade/annotation owner path, deleting the fallback-local circular-import workaround.
- Round 10 result: single-built and validated the base `ResultTypeView` once per computation, threading it through `Ann` and `Fallback` instead of rebuilding it in each submodule.
