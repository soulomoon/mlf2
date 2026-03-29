# Round 144 — Plan

## Selected Item

- **Item id**: `item-2`
- **Title**: Update documentation and record iso-recursive inference readiness

## Roadmap Coordinates

- **Active `roadmap_id`**: `2026-03-29-01-automatic-iso-recursive-type-inference-completion`
- **Active `roadmap_revision`**: `rev-001`
- **Active `roadmap_dir`**: `orchestrator/roadmaps/2026-03-29-01-automatic-iso-recursive-type-inference-completion/rev-001`

## Scope

Docs-only round. **No production code changes.** All work happens in the
worktree at `orchestrator/worktrees/round-144/`. The goal is to record
automatic iso-recursive type inference as a completed, tested capability across
all project documentation surfaces.

---

## Steps

### Step 1: Update `implementation_notes.md`

**File**: `implementation_notes.md`

**Action**: Add a new top-level dated section at the **top** of the file (before
the existing `2026-03-25` entry) with heading:

```
## 2026-03-29 - Automatic iso-recursive type inference implemented and tested
```

**Content to add** (summary, not verbatim — implementer writes final prose):

- Automatic iso-recursive type inference is now fully implemented and tested
  end-to-end across all pipeline phases.
- **Mechanism summary**:
  1. Cycle detection in `MLF.Constraint.Acyclicity`
     (`breakCyclesAndCheckAcyclicity`) detects cycles in the constraint graph
     and automatically introduces `TyMu` nodes to break them.
  2. Reification in `MLF.Reify.Type` produces `TMu` types from `TyMu` graph
     nodes.
  3. Elaboration emits `ERoll`/`EUnroll` coercions for recursive type
     boundaries.
  4. Phase 7 type checker (`MLF.Elab.TypeCheck`) accepts recursive types
     including `TMu`, `ERoll`, and `EUnroll`.
  5. Phase 7 reducer (`MLF.Elab.Reduce`) handles roll/unroll reduction steps.
- This is an **extension beyond the core thesis**, which assumes acyclic
  constraint graphs. The extension is documented in
  `docs/thesis-deviations.yaml` under `DEV-AUTO-ISO-RECURSIVE`.
- Test evidence: all 1168+ tests pass, including focused integration tests in
  `test/TypeSoundnessSpec.hs` and `test/PipelineSpec.hs` covering simple
  self-recursion, nested lets, recursive data patterns, polymorphic recursion,
  μ/∀ interaction, higher-order recursion, and explicit-μ stability.

**Verification**:
```bash
head -30 implementation_notes.md
# Confirm new section is at the top with correct date and content.
```

---

### Step 2: Update `roadmap.md`

**File**: `roadmap.md`

**Action**: In the **Phase 7** section (around line 139–156), update the status
paragraph to record iso-recursive type inference as a completed capability.
Specifically:

1. In the existing status note at the end of Phase 7 (line 154), append or
   update language indicating that Phase 7 now also handles **automatic
   iso-recursive types**: `TMu` type checking, `ERoll`/`EUnroll` reduction,
   and the full pipeline works end-to-end for automatically-inferred recursive
   types.

2. Add a brief note in the "Known deviations / proof gaps (tracked)" paragraph
   (around line 44) mentioning that `docs/thesis-deviations.yaml` now also
   records `DEV-AUTO-ISO-RECURSIVE` for the automatic μ-introduction extension.

**Verification**:
```bash
grep -n "iso-recursive\|TyMu\|TMu\|recursive type" roadmap.md
# Confirm the Phase 7 status and deviation reference are present.
```

---

### Step 3: Update `TODO.md`

**File**: `TODO.md`

**Action**: Add a new completed task entry at the **top** of the file (after the
header and `---` separator, before the current `Task 104` entry) with heading:

```
## Task 105 Automatic iso-recursive type inference completion (completed 2026-03-29)
```

**Content to add** (summary):

- Completed:
  - Completed the automatic iso-recursive type inference campaign through the
    `2026-03-29-01-automatic-iso-recursive-type-inference-completion` roadmap
    family: item-1 validated end-to-end Phase 7 reduction for auto-inferred
    recursive terms (inference → elaboration → type checking → reduction),
    item-2 recorded the completed capability across all project documentation
    surfaces.
  - The mechanism: cycle detection in the constraint graph automatically
    introduces `TyMu` nodes; reification produces `TMu` types; elaboration
    emits `ERoll`/`EUnroll` coercions; Phase 7 type checker and reducer accept
    and reduce recursive types end-to-end.
  - This is an extension beyond the core thesis (which assumes acyclic
    constraint graphs), documented in `docs/thesis-deviations.yaml` as
    `DEV-AUTO-ISO-RECURSIVE`.
- Verification:
  - `cabal build all && cabal test`: PASS (1168+ examples, 0 failures)
- Rolling priorities (next):
  1. Item-3 final readiness gate: clean up orchestrator state and declare
     readiness.

**Verification**:
```bash
head -40 TODO.md
# Confirm new Task 105 entry is at the top with correct structure.
```

---

### Step 4: Update `CHANGELOG.md`

**File**: `CHANGELOG.md`

**Action**: Under the existing `## Unreleased` / `### Changed` section, add a
new entry at the **top** of the bullet list (before the current first `- Completed
the bounded same-lane…` entry) describing the new capability:

**Content to add** (one bullet, concise):

```markdown
- Implemented automatic iso-recursive type inference end-to-end: the constraint
  solver now detects cycles and automatically introduces `TyMu` nodes,
  reification produces `TMu` types, elaboration emits `ERoll`/`EUnroll`
  coercions, and Phase 7 type-checks and reduces recursive types including
  roll/unroll steps. This is an extension beyond the core thesis (which assumes
  acyclic constraint graphs), documented in `docs/thesis-deviations.yaml` as
  `DEV-AUTO-ISO-RECURSIVE`. Validated with 1168+ tests, 0 failures.
```

**Verification**:
```bash
head -20 CHANGELOG.md
# Confirm the new entry is the first bullet under Unreleased/Changed.
```

---

### Step 5: Create/update `docs/thesis-deviations.yaml`

**File**: `docs/thesis-deviations.yaml`

**Action**: The file already exists with 4 deviation entries. Add a **new
deviation entry** in the `deviations:` list (after the existing entries, before
the `history:` section) with id `DEV-AUTO-ISO-RECURSIVE`.

**Content to add**:

```yaml
  - id: DEV-AUTO-ISO-RECURSIVE
    chapter: 9
    section: "9.3"
    thesis_ref: "Section 9.3 (acyclicity of instantiation constraints)"
    description: >
      The thesis assumes that constraint graphs are acyclic (Section 9.3,
      acyclicity check). This implementation extends the solver to detect
      cycles in the constraint graph and automatically introduce iso-recursive
      TyMu nodes to break them, enabling automatic inference of recursive
      types without explicit annotations. The mechanism: (1) cycle detection
      in breakCyclesAndCheckAcyclicity, (2) TyMu node introduction at cycle
      points, (3) reification producing TMu types, (4) elaboration emitting
      ERoll/EUnroll coercions, (5) Phase 7 type checker and reducer accepting
      and reducing recursive types. Non-recursive programs remain unaffected.
    impact: semantic-extension
    rationale: >
      Iso-recursive types are a natural extension for practical ML-family
      languages. The extension is safe because: (a) non-recursive programs
      produce identical results to the acyclic-only solver, (b) recursive
      types use iso-recursive (not equi-recursive) semantics with explicit
      roll/unroll, (c) contractiveness validation rejects non-contractive
      recursive types, and (d) the full pipeline (inference, elaboration,
      type checking, reduction) is tested end-to-end.
    code_paths:
      - src/MLF/Constraint/Acyclicity.hs
      - src/MLF/Reify/Type.hs
      - src/MLF/Elab/TypeCheck.hs
      - src/MLF/Elab/Reduce.hs
      - src/MLF/Constraint/Normalize.hs
      - src/MLF/Frontend/ConstraintGen/Translate.hs
    test_evidence:
      - matcher: "automatic recursive"
        file: test/PipelineSpec.hs
      - matcher: "iso-recursive"
        file: test/TypeSoundnessSpec.hs
      - matcher: "Phase 7"
        file: test/TypeSoundnessSpec.hs
    status: semantic-extension
```

**Verification**:
```bash
python3 -c "import yaml; yaml.safe_load(open('docs/thesis-deviations.yaml'))" && echo "VALID YAML"
# Confirm the file is valid YAML and contains the new DEV-AUTO-ISO-RECURSIVE entry.
grep "DEV-AUTO-ISO-RECURSIVE" docs/thesis-deviations.yaml
```

---

### Step 6: Final build and test gate

**Action**: Run the full verification gate to confirm zero regressions.

**Commands**:
```bash
cabal build all && cabal test
```

**Expected**: All tests pass (1168+ examples, 0 failures). No code changes
means no risk of regression, but the gate must be run and recorded per the
verification contract.

**Additional baseline checks**:
```bash
git diff --check
python3 -m json.tool orchestrator/state.json >/dev/null
```

---

## Constraints

- **No production code changes.** Only documentation files are modified:
  `implementation_notes.md`, `roadmap.md`, `TODO.md`, `CHANGELOG.md`, and
  `docs/thesis-deviations.yaml`.
- All edits happen in the worktree at `orchestrator/worktrees/round-144/`.
- The `cabal build all && cabal test` gate must pass with zero regressions.
- The new `DEV-AUTO-ISO-RECURSIVE` deviation entry uses `status:
  semantic-extension` (not `proof-gap` or `implementation-choice`) because this
  is a deliberate semantic extension beyond the thesis's acyclic assumption.
