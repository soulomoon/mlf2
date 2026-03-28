# Round 143 — Task Selection

## Selected Item

**Item 1: End-to-end validation: Phase 7 reduction of iso-recursive elaborated terms**

- `roadmap_item_id`: `item-1`
- Status: `[pending]` → selected for this round

## Why Now

This is the lowest-numbered pending item in the roadmap. Items 1–4 of the
predecessor roadmap (`2026-03-29-00`) are all done: cycle detection, pipeline
wiring, reification/elaboration, and edge-case hardening are complete with 1168
tests passing. Item-1 is the natural next step — it validates that the full
pipeline (inference → elaboration → type checking → **reduction**) works
end-to-end for automatically-inferred recursive types, specifically that
`step`/`normalize` correctly reduce `ERoll`/`EUnroll` terms. Items 2 and 3
depend on item-1.

## Roadmap Coordinates

- `roadmap_id`: `2026-03-29-01-automatic-iso-recursive-type-inference-completion`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-03-29-01-automatic-iso-recursive-type-inference-completion/rev-001`
- `roadmap_item_id`: `item-1`

## Completion Criteria (from roadmap)

1. `runPipelineElabChecked` succeeds for recursive definitions
2. The elaborated term type-checks via Phase 7
3. `step`/`normalize` can reduce recursive applications (including roll/unroll
   reduction steps)
4. Integration tests in `test/PipelineSpec.hs` (or a new
   `test/TypeSoundnessSpec.hs`) proving type soundness for automatically
   inferred recursive types
5. Non-recursive programs still produce identical results
6. All tests pass via `cabal build all && cabal test`

## Planner Context

### Key Source Files

| File | Role |
|------|------|
| `src/MLF/Elab/Reduce.hs` | Phase 7 reduction engine — `step`, `normalize`, `reduceInst`, `isValue`. Already handles `ERoll`/`EUnroll` structurally (lines 39–46): `ERoll` steps its body then `EUnroll (ERoll _ body)` reduces to `body`. This is the core reduction semantics to validate. |
| `src/MLF/Elab/TypeCheck.hs` | Phase 7 type checker — `typeCheck`, `typeCheckWithEnv`. Handles `ERoll` (line 83, checks body against unfolded `TMu`) and `EUnroll` (line 96, unfolds `TMu` result). |
| `src/MLF/Elab/Run/Pipeline.hs` | Pipeline entry points — `runPipelineElab` (unchecked), `runPipelineElabChecked` (checked = elab + type-check). These are the authoritative entry points. |
| `src/MLF/Types/Elab.hs` | `ElabTerm` and `ElabType` definitions including `ERoll`, `EUnroll`, `TMu` constructors. |
| `src/MLF/Elab/Elaborate/Algebra.hs` | Elaboration algebra that emits `ERoll`/`EUnroll` coercions for recursive types. |
| `src/MLF/Elab/Generalize.hs` | Generalization — produces `TMu` types during scheme construction. |
| `src/MLF/Constraint/Acyclicity.hs` | `breakCyclesAndCheckAcyclicity` — automatic `TyMu` introduction in the constraint solver. |

### Key Test Files

| File | Role |
|------|------|
| `test/PipelineSpec.hs` | Main integration test file. Already has "Automatic μ-introduction" sections for items 1–4 (lines 1281–1440+) including a test at line 1318 that validates ERoll/EUnroll presence and Phase 7 type-checking. **New reduction tests should go here or in a new `test/TypeSoundnessSpec.hs`.** |
| `test/ElaborationSpec.hs` | Phase 6 elaboration tests. |

### Key Functions to Validate

- `Reduce.step` / `Reduce.normalize` — confirm `EUnroll (ERoll ty v)` → `v` reduction fires for auto-inferred recursive terms
- `Reduce.isValue` — confirm `ERoll ty body` is a value when `body` is a value
- `TypeCheck.typeCheck` — confirm `ERoll`/`EUnroll` type rules are sound: `ERoll` checks body against unfolded μ, `EUnroll` unfolds μ result
- `Pipeline.runPipelineElabChecked` — confirm the full pipeline succeeds for recursive definitions
- Type preservation: `typeCheck term == typeCheck (step term)` for recursive elaborated terms

### Existing Test Coverage (what's already validated)

- `PipelineSpec` line 1318: "elaborates recursive uses with explicit ERoll/EUnroll and passes Phase 7" — validates that self-recursive `let f = \x. f x in f` produces `TMu` type, contains `ERoll`/`EUnroll` in the elaborated term, and type-checks. **However, this test does NOT validate reduction behavior.**
- `PipelineSpec` line 1265: non-recursive identity control — confirms μ-free
- Various edge-case tests (nested recursion, polymorphic recursion, μ/∀ interaction, explicit-μ stability)

### What's Missing (the gap this item fills)

1. **Reduction tests**: No test currently calls `step`/`normalize` on auto-inferred recursive elaborated terms
2. **Type preservation under reduction**: No test verifies `typeCheck(term) == typeCheck(step(term))` for recursive terms
3. **Application reduction**: No test verifies that applying a recursive function to an argument reduces correctly through roll/unroll
4. **Non-recursive regression**: Should confirm `step`/`normalize` behavior is unchanged for non-recursive programs
