# Architecture / Repo Layout

This repository implements the MLF → xMLF pipeline described in `papers/these-finale-english.txt` (see also `papers/xmlf.txt`).
Goal: keep the implementation paper-faithful to the thesis and document any deviations; use `papers/xmlf.txt` only as supplementary detail when the thesis is silent.

## Public API (downstream users)

Downstream code should import:

- `MLF.API` — umbrella module (surface syntax + pipeline entry points + xMLF result types + xMLF checking/reduction helpers)
- `MLF.Pipeline` — pipeline entry points + xMLF checking/reduction helpers (e.g. `inferConstraintGraph`, `runPipelineElab`, `typeCheck`, `step`, `normalize`)

`MyLib` remains as a legacy compatibility wrapper that re-exports `MLF.API`.

Public modules live under `src-public/` and the public Cabal library only exposes:

- `src-public/MLF/API.hs`
- `src-public/MLF/Pipeline.hs`
- `src-public/MyLib.hs`

## Internal implementation (package-private)

All implementation modules live under `src/` and are built as a private sublibrary:

- Cabal sublibrary: `library mlf2-internal` with `visibility: private`

The code is organized by domain (not by phase) under `src/MLF/`:

- `MLF.Frontend.*` — surface syntax, desugaring, constraint generation
- `MLF.Constraint.*` — constraint graph types + normalize + acyclicity + presolution + solve
- `MLF.Binding.*` — binding tree queries + executable χe ops + harmonization
- `MLF.Witness.*` — ω execution helpers (base χe operations)
- `MLF.Elab.*` — elaboration to xMLF (Φ/Σ translation, reify/generalize, plus xMLF typechecking/reduction)
- `MLF.Util.*` — shared utilities (order keys, union-find, etc.)

## `Solved` boundary and thesis-exact cleanup rule

`MLF.Constraint.Solved` should not be kept or removed for its own sake. The thesis-exact rule is:

- keep exactly the semantic boundary that the thesis needs;
- move out compatibility/convenience glue that the thesis does not require;
- remove `Solved` entirely only if its remaining semantics are represented elsewhere just as explicitly.

In the current codebase, `PresolutionView` is the primary read-only runtime/internal API, while `Solved` still carries some finalized-snapshot and original-vs-canonical semantics that are not yet pure glue.

### Current classification of the `Solved` ecosystem

| Must stay somewhere | Can move out now | Safe to retire from `Solved` surface |
|---|---|---|
| Opaque solved boundary + replay-faithful construction: `Solved`, `fromSolveOutput`, `fromPreRewriteState`. These still encode the finalized pre-rewrite → canonical solved boundary. | Compatibility/no-replay builders: `fromConstraintAndUf`, `rebuildWithConstraint`, `fromSolved`, `solvedFromView`. These should stay only in local compat owners such as `MLF.Constraint.Finalize`, `MLF.Constraint.Presolution.View`, `MLF.Constraint.Presolution.Plan`, and `MLF.Reify.Core`. | Test-only constructor alias: `mkTestSolved`. If retained, it belongs in test helpers rather than on the production `Solved` API. |
| Original ↔ canonical boundary primitives: `canonical`, `canonicalMap`, `originalConstraint`, `canonicalConstraint`. The thesis-exact phase distinction depends on these capabilities remaining explicit somewhere. | Read-query wrappers already mirrored by `PresolutionView`: `lookupNode`, `lookupBindParent`, `bindParents`, `lookupVarBound`, `genNodes`. These should migrate with the read-only solved/view boundary rather than remain core `Solved` surface area. | Pure enumeration helpers: `allNodes`, `instEdges`. These are primarily useful for tests/audits and do not justify production `Solved` surface area. |
| Finalized/view construction entrypoints: `fromPresolutionResult`, `finalizeSolvedFromSnapshot`, `finalizeSolvedForConstraint`. The system still needs explicit constructors for finalized solved/view artifacts. | Finalize-local or reify-facing helpers: `pruneBindParentsSolved`, `weakenedVars`, `isEliminatedVar`, `canonicalizedBindParents`. These are still needed, but they belong with finalization or reify-owned canonical-read helpers. | Raw canonical container accessors: `canonicalBindParents`, `canonicalGenNodes`. No meaningful live production surface depends on them. |
| Strict solved validation capability: `validateCanonicalGraphStrict`. The post-finalization solved-graph invariant must keep an explicit home even if it eventually moves out of `Solved`. | Local compatibility state that should narrow to simpler data: `geRes :: Solved` should collapse to the `canonicalMap` it actually uses. | Dead mutation hooks: `rebuildWithNodes`, `rebuildWithBindParents`, `rebuildWithGenNodes`. These no longer have live external callers. |
| A narrow solved boundary still remains justified for legacy/order/reify consumers until they gain equally explicit replacements (`MLF.Elab.Legacy`, `MLF.Util.Order`, and the remaining original/canonical reads in `Reify.*`). | Any remaining solved↔view adapters should stay local to their owning boundary modules and continue shrinking instead of expanding the shared abstraction. | Audit/test-only original-domain helpers: `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, `validateOriginalCanonicalAgreement`. These may stay in audit/test helpers, but they do not need to remain part of the production `Solved` API. |

### Practical consequence

The thesis-exact near-term direction is to shrink `Solved` down to a narrow semantic boundary built from:

- replay-faithful solved construction,
- explicit original vs canonical correspondence,
- and strict post-finalization validation.

Everything else should be:

- relocated to the owning boundary module (`MLF.Constraint.Presolution.View`, `MLF.Constraint.Finalize`, `MLF.Reify.Core`, `MLF.Constraint.Presolution.Plan`), or
- retired from the production `Solved` surface when it is only dead, diagnostic, or test-facing.

So the project goal is **not** “delete `Solved` no matter what”; it is “remove non-thesis glue first, and only delete `Solved` if no thesis-relevant semantic boundary remains.”

The first low-risk cleanup implied by this table landed on 2026-03-08 by retiring the dead `Solved` mutation hooks `rebuildWithNodes`, `rebuildWithBindParents`, and `rebuildWithGenNodes`.

The second low-risk cleanup landed on 2026-03-08 by narrowing `GeneralizeEnv` from `geRes :: Solved` to `geCanonicalMap :: IntMap.IntMap NodeId` and removing the now-obsolete `buildSolvedFromPresolutionView` helper.

The third low-risk cleanup landed on 2026-03-08 by splitting `MLF.Constraint.Solved` into a thin public facade plus `MLF.Constraint.Solved.Internal`, moving `fromConstraintAndUf` and `rebuildWithConstraint` off the public surface and into local owner-module usage.

The fourth low-risk cleanup landed on 2026-03-08 by retiring the dead raw canonical container accessors `canonicalBindParents` and `canonicalGenNodes` from both the public facade and internal implementation.

The fifth low-risk cleanup landed on 2026-03-08 by moving the remaining test/audit-only helper bundle (`mkTestSolved`, `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, `validateOriginalCanonicalAgreement`) into `test/SolvedFacadeTestUtil.hs` and removing it from the public facade.

The detailed evidence matrix for this classification lives in `tasks/archive/2026-03-08-solved-classification-table/findings.md`.

## Witness Representation (Φ/Σ)

- `EdgeWitness.ewWitness` stores the Ω-only instance operations (Graft/Merge/Raise/Weaken/RaiseMerge).
- `EdgeWitness.ewSteps` stores the interleaved step stream used for Φ translation, including `StepIntro` (O) in expansion order and `StepOmega` for Ω operations.
- `EdgeTrace` (per instantiation edge) tracks the expansion root, binder→argument pairs, interior `I(r)`, and copy-map provenance used by `phiFromEdgeWitnessWithTrace`.

## Executable

- `app/Main.hs` builds the `mlf2` binary (demo runner).

## Tests

The test suite depends on both:

- `mlf2` (public library) and
- `mlf2:mlf2-internal` (private internal library)

This keeps the downstream surface small while still allowing specs to import internal modules.
