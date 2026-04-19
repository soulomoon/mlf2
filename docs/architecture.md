# Architecture / Repo Layout

This repository implements the MLF → xMLF pipeline described in `papers/these-finale-english.txt` (see also `papers/xmlf.txt`).
Goal: keep the implementation paper-faithful to the thesis and document any deviations; use `papers/xmlf.txt` only as supplementary detail when the thesis is silent.

## Public API (downstream users)

Downstream code should import:

- `MLF.API` — umbrella frontend module (surface syntax + eMLF / `.mlfp` parse/pretty + normalization helpers)
- `MLF.Pipeline` — canonical pipeline/runtime module (e.g. `inferConstraintGraph`, `runPipelineElab`, `typeCheck`, `step`, `normalize`, `.mlfp` elaboration/checking/runtime)
- `MLF.Program` — compatibility shim re-exporting the same unified `.mlfp` surface
- `MLF.XMLF` — explicit xMLF syntax, parser, and pretty-printing helpers

Public modules live under `src-public/` and the public Cabal library only exposes:

- `src-public/MLF/API.hs`
- `src-public/MLF/Program.hs`
- `src-public/MLF/Pipeline.hs`
- `src-public/MLF/XMLF.hs`

Active implementation planning lives under `tasks/todo/YYYY-MM-DD-description/`.
Root-level `task_plan.md`, `findings.md`, and `progress.md` are historical
artifacts and are not part of the current task workflow.

## Repo layout

- `src/` builds the private implementation library `mlf2-internal`.
- `src-public/` contains the public entry modules `MLF.API`, `MLF.Program` (compatibility shim), `MLF.Pipeline`, and `MLF.XMLF`.
- `src-research/` contains `MLF.Research.*` modules for the separate internal library `mlf2-research`; `mlf2-internal` must not depend on it.
- `app/` contains the `mlf2` executable entrypoint.
- `test/` contains the Hspec suite, the manual test runner, and frozen parity tooling/artifacts.
- `papers/` contains the thesis/reference texts used for paper-faithful implementation work.

## Internal implementation (package-private)

All implementation modules live under `src/` and are built as a private sublibrary:

- Cabal sublibrary: `library mlf2-internal` with `visibility: private`

The code is organized by domain (not by phase) under `src/MLF/`:

- `MLF.Frontend.*` — surface syntax, desugaring, constraint generation
- `MLF.Frontend.Syntax.Program` / `MLF.Frontend.Parse.Program` / `MLF.Frontend.Pretty.Program` — canonical `.mlfp` syntax ownership under the main frontend boundary
- `MLF.Frontend.Program.Check` — module/import/class/data environment assembly for `.mlfp`, including static validation that may fail before the eMLF pipeline
- `MLF.Frontend.Program.Elaborate` — lowers executable `.mlfp` bindings to surface eMLF `SurfaceExpr`
- `MLF.Frontend.Program.Finalize` — normalizes lowered surface eMLF, calls the internal detailed eMLF pipeline entrypoint, and resolves `.mlfp` deferred obligations before producing checked xMLF bindings
- `MLF.Frontend.Program.Run` — runtime entrypoint that evaluates checked `.mlfp` bindings through the existing xMLF runtime
- `MLF.Constraint.*` — constraint graph types + normalize + acyclicity + presolution + solve
- `MLF.Binding.*` — binding tree queries + executable χe ops + harmonization
- `MLF.Witness.*` — ω execution helpers (base χe operations)
- `MLF.Elab.*` — elaboration to xMLF (Φ/Σ translation, reify/generalize, plus xMLF typechecking/reduction)
- `MLF.Elab.TypeCheck` — the single typing judgment owner for elaborated `.mlfp` / xMLF terms
- `MLF.XMLF.*` — explicit xMLF syntax and related helpers
- `MLF.Reify.*` — graph-to-type reification and related type operations
- `MLF.Types.*` — elaborated/runtime term and type representations
- `MLF.Util.*` — shared utilities (order keys, union-find, etc.)

Tests and executables that need `MLF.Research.*` must add `mlf2:mlf2-research`
to `build-depends`.

## Key graph and witness types

- `Expr` (`MLF.Frontend.Syntax`) — surface eMLF terms
- `Constraint` (`MLF.Constraint.Types`) — constraint graph plus binding tree
- `TyNode` — graph nodes (`TyVar`, `TyArrow`, `TyForall`, `TyBase`, `TyExp`, `TyBottom`)
- `InstEdge` — instantiation edges (`<=`)
- `BindParents` — child-to-parent binding tree map with `BindFlag`
- `Expansion` — presolution recipes (identity, forall-intro, instantiation, composition)
- `EdgeWitness` — per-edge xMLF instantiation reconstruction metadata

## Shared ownership notes

- Core graph node/edge/binding identifiers and types live in `MLF.Constraint.Types.Graph`; witness metadata lives in `MLF.Constraint.Types.Witness`; presolution-only state/types live in `MLF.Constraint.Types.Presolution`. `MLF.Constraint.Types` re-exports these for compatibility.
- Shared unification flow lives in `MLF.Constraint.Unify.Core`; shared structural decomposition lives in `MLF.Constraint.Unify.Decompose`.
- Prefer `MLF.Constraint.Canonicalizer` for redirect + union-find canonicalization instead of ad hoc chase helpers.
- Legacy expansion-to-instantiation translation lives in `MLF.Elab.Legacy` and is re-exported by `MLF.Elab.Elaborate` and `MLF.Elab.Pipeline`.
- Presolution state access should go through `MonadPresolution` plus `MLF.Constraint.Presolution.Ops` and `StateAccess`; edge processing is split across planner/interpreter passes with typed `EdgePlan`.
- Elaboration entrypoints bundle inputs as `ElabConfig`/`ElabEnv`, and tracing is explicit via `TraceConfig`.

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

The sixth low-risk cleanup landed on 2026-03-08 by moving `pruneBindParentsSolved` off the public facade and keeping it only behind `MLF.Constraint.Finalize`, its real owner.

The seventh and final table-driven facade cleanup landed on 2026-03-08 by retiring the remaining non-must-stay helper cluster (`lookupVarBound`, `genNodes`, `weakenedVars`, `isEliminatedVar`, `canonicalizedBindParents`) from the public facade and replacing their owner-local use with direct constraint/canonical logic. At this point, the public `Solved` facade is reduced to the thesis-relevant core: replay-faithful construction, explicit original↔canonical boundary primitives, and strict solved-graph validation.

The detailed evidence matrix for this classification lives in the 2026-03-08 solved classification audit notes.

## Fallback Policy

- Elaborative/runtime fallback ladders are now removed: the production path prefers explicit witness/scheme authority and surfaces structured errors when that authority is insufficient.
- Planner scheme ownership for synthesized wrappers is body-root only; there is no wrapper-root recovery path.
- Instantiation inference keeps only structurally justified argument recovery.

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

## 2026-03-08 fallback-removal architecture note

The active elaboration path is now intentionally fail-fast around the old fallback seams:

- generalization does not retry through GA-disabled or raw reify ladders;
- let elaboration no longer chooses among RHS-/env-derived alternate schemes; the authoritative generalization result is the only live let scheme source;
- `MLF.Elab.Run.Generalize` no longer passes a recursive generalization callback into `applyGeneralizePlan`, and `MLF.Elab.Generalize` now uses the existing structural scheme plan instead of recursively generalizing another scope;
- planner scheme-owner resolution is body-root authoritative;
- instantiation inference is structural and prefix-based rather than catch-all heuristic;
- `reifyInst` is witness/domain-only apart from exact source-scheme reuse for already-authoritative annotations; the live authority set is `ewLeft`/`ewRight`, `etBinderArgs`, and copied witness-domain nodes from `etCopyMap`, and if only expansion-derived recovery would make an application/annotation succeed, the pipeline now fails fast.

This keeps the runtime path closer to the thesis boundary: if witness/scheme information is insufficient, the code now fails explicitly instead of silently switching to a weaker reconstruction mode.
