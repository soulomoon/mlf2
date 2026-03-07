# Findings

## Baseline

- Baseline facts are captured in `tasks/archive/2026-03-08-solved-classification-table/baseline.txt`.
- Per-symbol usage counts are captured in `tasks/archive/2026-03-08-solved-classification-table/symbol-usage.txt` and the current static audit counts are: 32 exported surface entries, 13 direct `src/` importers, 12 direct `test/` importers, and 6 named adjacent seams.
- Direct `src/` importers are concentrated in finalize, reify, presolution view/plan, legacy elaboration, util ordering, and the runtime pipeline.
- Direct `test/` importers are broad, with `test/Constraint/SolvedSpec.hs` acting as the detailed contract surface.
- The current doc claims still constrain the table:
  - `PresolutionView` is the primary internal/runtime API and non-test/non-legacy `fromSolved` usage has been removed from runtime/reify helpers (`implementation_notes.md:52`, `implementation_notes.md:53`, `implementation_notes.md:54`).
  - `fromSolved` is intended to remain confined to `MLF.Constraint.Presolution.View`, `MLF.Elab.Legacy`, and tests (`CHANGELOG.md:33`, `CHANGELOG.md:34`, `CHANGELOG.md:35`).

## Final Classification Matrix — `MLF.Constraint.Solved` exports

| Item(s) | Column | Evidence | Reason / destination |
|---|---|---|---|
| `Solved` | **Must stay somewhere** | `src/MLF/Constraint/Solved.hs:16`, `src/MLF/Constraint/Finalize.hs:94`, `src/MLF/Reify/Core.hs:83`, `src/MLF/Elab/Legacy.hs:45` | The opaque solved handle is still the explicit finalized/original-vs-canonical boundary used by finalize, reify, and legacy elaboration; remove only after an equally explicit replacement exists. |
| `fromSolveOutput`, `fromPreRewriteState` | **Must stay somewhere** | `src/MLF/Constraint/Solved.hs:155`, `src/MLF/Constraint/Solved.hs:163`, `test/Constraint/SolvedSpec.hs:469`, `test/Constraint/SolvedSpec.hs:482` | These constructors encode the replay-faithful pre-rewrite → canonical solved boundary and are directly guarded by snapshot contract tests. |
| `fromConstraintAndUf`, `rebuildWithConstraint` | **Can move out now** | `src/MLF/Constraint/Solved.hs:128`, `src/MLF/Constraint/Solved.hs:305`, `src/MLF/Constraint/Finalize.hs:79`, `src/MLF/Constraint/Finalize.hs:100`, `src/MLF/Reify/Core.hs:861`, `src/MLF/Constraint/Presolution/Plan.hs:806` | These are no-replay/compatibility builders used to patch local constraints around a preserved canonical map; move to owning compat sites (`MLF.Constraint.Finalize`, `MLF.Reify.Core`, `MLF.Constraint.Presolution.Plan`) instead of growing the main `Solved` abstraction. |
| `mkTestSolved` | **Safe to retire from `Solved` surface** | `src/MLF/Constraint/Solved.hs:151`, `tasks/archive/2026-03-08-solved-classification-table/symbol-usage.txt` | This is explicitly a backward-compatible test alias and is only needed as test scaffolding; move to a test helper if it stays. |
| `canonical`, `canonicalMap`, `originalConstraint`, `canonicalConstraint` | **Must stay somewhere** | `src/MLF/Constraint/Solved.hs:252`, `src/MLF/Constraint/Solved.hs:257`, `src/MLF/Constraint/Solved.hs:261`, `src/MLF/Constraint/Solved.hs:266`, `src/MLF/Constraint/Presolution/View.hs:56`, `src/MLF/Constraint/Finalize.hs:107` | These are the core original↔canonical boundary primitives; the thesis-exact distinction survives only if these capabilities remain explicit somewhere. |
| `lookupNode`, `lookupBindParent`, `bindParents`, `lookupVarBound` | **Can move out now** | `src/MLF/Constraint/Solved.hs:270`, `src/MLF/Constraint/Solved.hs:279`, `src/MLF/Constraint/Solved.hs:284`, `src/MLF/Constraint/Solved.hs:296`, `src/MLF/Constraint/Presolution/View.hs:58`, `src/MLF/Constraint/Presolution/View.hs:61`, `src/MLF/Constraint/Presolution/View.hs:62` | These are read-only query wrappers already mirrored by `PresolutionView`; move to `MLF.Constraint.Presolution.View` or a shared read-view helper rather than keeping them as primary `Solved` surface area. |
| `allNodes`, `instEdges` | **Safe to retire from `Solved` surface** | `src/MLF/Constraint/Solved.hs:275`, `src/MLF/Constraint/Solved.hs:288`, `test/Constraint/SolvedSpec.hs:271`, `tasks/archive/2026-03-08-solved-classification-table/symbol-usage.txt` | These are mainly enumeration helpers for tests/diagnostics, with no meaningful production call surface; keep only in test/audit helpers if still wanted. |
| `genNodes` | **Can move out now** | `src/MLF/Constraint/Solved.hs:292`, `src/MLF/Reify/Core.hs:100`, `src/MLF/Reify/Core.hs:179` | This is still used in production, but only as canonical graph data access; move alongside the rest of the read-only solved view surface. |
| `pruneBindParentsSolved` | **Can move out now** | `src/MLF/Constraint/Solved.hs:316`, `src/MLF/Constraint/Finalize.hs:82` | This is finalize-local graph cleanup and belongs with finalization, not on the long-term `Solved` API. |
| `rebuildWithNodes`, `rebuildWithBindParents`, `rebuildWithGenNodes` | **Safe to retire from `Solved` surface** | `src/MLF/Constraint/Solved.hs:335`, `src/MLF/Constraint/Solved.hs:341`, `src/MLF/Constraint/Solved.hs:346`, `tasks/archive/2026-03-08-solved-classification-table/symbol-usage.txt` | No live external callers remain; these are obsolete mutation hooks that only enlarge the abstraction boundary. |
| `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder` | **Safe to retire from `Solved` surface** | `src/MLF/Constraint/Solved.hs:355`, `src/MLF/Constraint/Solved.hs:361`, `src/MLF/Constraint/Solved.hs:366`, `src/MLF/Constraint/Solved.hs:371`, `test/Constraint/SolvedSpec.hs:347`, `test/Constraint/SolvedSpec.hs:432` | These are useful audit/test helpers for inspecting pre-solving detail, but the thesis-relevant boundary is already preserved by `originalConstraint` + `canonical`; they do not need to remain part of the production `Solved` surface. |
| `weakenedVars`, `isEliminatedVar`, `canonicalizedBindParents` | **Can move out now** | `src/MLF/Constraint/Solved.hs:385`, `src/MLF/Constraint/Solved.hs:389`, `src/MLF/Constraint/Solved.hs:398`, `src/MLF/Reify/Core.hs:98`, `src/MLF/Reify/Core.hs:138`, `src/MLF/Reify/Core.hs:267` | These are still production-relevant canonical-domain queries, but they are reify-facing convenience helpers and should live with the canonical read surface or in reify-owned helper modules. |
| `canonicalBindParents`, `canonicalGenNodes` | **Safe to retire from `Solved` surface** | `src/MLF/Constraint/Solved.hs:394`, `src/MLF/Constraint/Solved.hs:403`, `tasks/archive/2026-03-08-solved-classification-table/symbol-usage.txt` | No live production callers depend on these raw container accessors. |
| `validateCanonicalGraphStrict` | **Must stay somewhere** | `src/MLF/Constraint/Solved.hs:407`, `src/MLF/Constraint/Finalize.hs:85`, `test/Constraint/SolvedSpec.hs:121` | The strict solved-graph validation capability is still a mandatory invariant check after finalization, even if it eventually moves to finalize/solve validation ownership. |
| `validateOriginalCanonicalAgreement` | **Safe to retire from `Solved` surface** | `src/MLF/Constraint/Solved.hs:416`, `test/Constraint/SolvedSpec.hs:294` | This is a divergence/audit helper for tests and Phase-E-style checks, not a core production surface requirement. |

## Final Classification Matrix — adjacent seams

| Item(s) | Column | Evidence | Reason / destination |
|---|---|---|---|
| `fromPresolutionResult`, `finalizeSolvedFromSnapshot`, `finalizeSolvedForConstraint` | **Must stay somewhere** | `src/MLF/Constraint/Presolution/View.hs:36`, `src/MLF/Constraint/Finalize.hs:94`, `src/MLF/Constraint/Finalize.hs:105` | The system still needs explicit constructors for finalized solved/view artifacts from presolution/snapshot state; these are boundary entrypoints, not convenience glue. |
| `fromSolved`, `buildSolvedFromPresolutionView`, `solvedFromView` | **Can move out now** | `src/MLF/Constraint/Presolution/View.hs:53`, `src/MLF/Constraint/Presolution/Plan.hs:806`, `src/MLF/Reify/Core.hs:861`, `implementation_notes.md:54` | These are compatibility adapters between `Solved` and view/native paths; keep them local to owning boundary modules and shrink them away as the remaining legacy/test call sites disappear. |
| `geRes :: Solved` and its sole `Solved.canonicalMap` use | **Safe to retire from `Solved` surface** | `src/MLF/Constraint/Presolution/Plan/Context.hs:66`, `src/MLF/Constraint/Presolution/Plan/Context.hs:341` | Planner state no longer needs a whole solved handle here; the only remaining dependency is the canonical map. |

## Caller synthesis

- **Finalize owns the remaining production construction/validation seam**: `MLF.Constraint.Finalize` is the clearest owner for replay/no-replay solved assembly, prune logic, and strict validation (`src/MLF/Constraint/Finalize.hs:74`, `src/MLF/Constraint/Finalize.hs:82`, `src/MLF/Constraint/Finalize.hs:85`, `src/MLF/Constraint/Finalize.hs:94`).
- **Runtime/read-only consumers are already largely view-shaped**: `PresolutionView` mirrors `Solved` query fields directly (`src/MLF/Constraint/Presolution/View.hs:53`), and docs already state it is the primary runtime API (`implementation_notes.md:53`).
- **Legacy/reify consumers still justify a narrow solved boundary**: `MLF.Elab.Legacy`, `MLF.Util.Order`, `MLF.Reify.Core`, and `MLF.Reify/TypeOps` still consume the original↔canonical boundary directly (`src/MLF/Elab/Legacy.hs:45`, `src/MLF/Util/Order.hs:40`, `src/MLF/Reify/Core.hs:96`, `src/MLF/Reify/TypeOps.hs:342`).

## Agent-team notes

- Built-in subagents were launched for the planned audit roles.
- One `Solved`-surface auditor returned strong, aligned evidence; one seam auditor returned only partially relevant output; the remaining agents timed out or errored when interrupted for checkpoint replies.
- Final classification therefore uses lead-side baseline evidence as the source of truth and uses the successful subagent output only as corroboration.
