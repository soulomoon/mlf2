# Round 155 Plan — Item 5: Update documentation to reflect completed non-local proxy support

## Overview

Documentation-only round. No code changes. Three files to update in the worktree at `orchestrator/worktrees/round-155/`.

---

## Step 1: Update `implementation_notes.md` (lines 28-34)

### Change 1a: Replace "Known remaining limitation" with "Resolved gap" (lines 28-29)

**Old text (lines 28-29):**
```
**Known remaining limitation:**
- Non-local proxy at pipeline entrypoints: the result-type fallback now correctly returns μ-types for non-local reconstruction, but the pipeline entrypoint still fails with `PhiTranslatabilityError` at the elaboration level. This is a separate elaboration-layer issue, not a result-type fallback defect.
```

**New text:**
```
**Resolved gap (non-local proxy elaboration):**
- Non-local proxy `PhiTranslatabilityError` at pipeline entrypoints has been resolved. The `reifyInst` TyMu 0-binder fallback (round 152) and `OpRaise` non-spine bind-parent guard for μ-type nodes (round 153) fixed the two `PhiTranslatabilityError` crash sites. The pipeline now reaches type checking for non-local proxy wrappers. A downstream `TCArgumentMismatch` remains for the full non-local proxy expression (`let g = (λx:μα.α→Int. x) in g g`), indicating that further fold/unfold coercion work is needed in elaboration, but the `PhiTranslatabilityError` barrier is cleared. Survey of all 13 `ElaborationSpec` `PhiTranslatabilityError` assertion sites (round 154) confirmed none match the non-local proxy pattern — all are legitimate untranslatable cases.
```

### Change 1b: Update test evidence count (lines 31-34)

**Old text (lines 31-34):**
```
**Test evidence:** all 1175 examples pass, including focused integration tests in
`test/TypeSoundnessSpec.hs` and `test/PipelineSpec.hs` covering simple
self-recursion, nested recursive lets, recursive data patterns, polymorphic recursion,
μ/∀ interaction, higher-order recursion, non-local recursive result types, and explicit-μ stability.
```

**New text:**
```
**Test evidence:** all 1176 examples pass, including focused integration tests in
`test/TypeSoundnessSpec.hs` and `test/PipelineSpec.hs` covering simple
self-recursion, nested recursive lets, recursive data patterns, polymorphic recursion,
μ/∀ interaction, higher-order recursion, non-local recursive result types, explicit-μ stability,
and non-local proxy elaboration boundary tests.
```

---

## Step 2: Update `CHANGELOG.md` (line 6, after existing bullet)

Insert a new bullet point after the existing `Changed` bullet on line 6.

**Insert after line 6** (after the line ending `...Validated with 1175 examples, 0 failures.`):

```
- Resolved non-local proxy `PhiTranslatabilityError` at pipeline entrypoints: `reifyInst` TyMu 0-binder fallback (round 152) and `OpRaise` bind-parent μ-guard (round 153) clear the two crash sites so that non-local proxy wrappers now reach type checking. Survey of `ElaborationSpec` `PhiTranslatabilityError` sites (round 154) confirmed all are legitimate untranslatable cases. Validated with 1176 examples, 0 failures.
```

Also update the example count on line 6 from `1175` to `1176`.

---

## Step 3: Update `docs/thesis-deviations.yaml` — `DEV-AUTO-ISO-RECURSIVE` (lines 91-141)

### Change 3a: Update `description` field (lines 96-107)

**Old text (lines 96-107):**
```yaml
    description: >
      The thesis assumes that constraint graphs are acyclic (Section 9.3,
      acyclicity check). This implementation extends the solver to detect
      cycles in the constraint graph and automatically introduce iso-recursive
      TyMu nodes to break them, enabling automatic inference of recursive
      types without explicit annotations. An initial implementation was
      hardened in a gap-fix campaign (rounds 146-149) that addressed witness
      normalization, alias-bounds resolution, recursive let-bindings, and
      non-local result types. The mechanism: (1) cycle detection in
      breakCyclesAndCheckAcyclicity, (2) TyMu node introduction at cycle
      points, (3) reification producing TMu types, (4) elaboration emitting
      ERoll/EUnroll coercions, (5) Phase 7 type checker and reducer accepting
      and reducing recursive types. Non-recursive programs remain unaffected.
```

**New text:**
```yaml
    description: >
      The thesis assumes that constraint graphs are acyclic (Section 9.3,
      acyclicity check). This implementation extends the solver to detect
      cycles in the constraint graph and automatically introduce iso-recursive
      TyMu nodes to break them, enabling automatic inference of recursive
      types without explicit annotations. An initial implementation was
      hardened in a gap-fix campaign (rounds 146-149) that addressed witness
      normalization, alias-bounds resolution, recursive let-bindings, and
      non-local result types. A follow-up campaign (rounds 151-154) resolved
      non-local proxy PhiTranslatabilityError at pipeline entrypoints via
      reifyInst TyMu 0-binder fallback and OpRaise bind-parent mu-guard.
      The mechanism: (1) cycle detection in breakCyclesAndCheckAcyclicity,
      (2) TyMu node introduction at cycle points, (3) reification producing
      TMu types, (4) elaboration emitting ERoll/EUnroll coercions,
      (5) Phase 7 type checker and reducer accepting and reducing recursive
      types. Non-recursive programs remain unaffected.
```

### Change 3b: Add code path (after line 129)

Insert after the `src/MLF/Elab/Run/ResultType/Fallback.hs` entry:

```yaml
      - src/MLF/Elab/Phi/Omega/Interpret.hs
```

### Change 3c: Add test evidence entry (after line 140)

Insert after the last test_evidence entry (`file: test/PipelineSpec.hs` on line 140):

```yaml
      - matcher: "non-local proxy"
        file: test/PipelineSpec.hs
```

---

## Step 4: Verification

Run in the worktree:

```bash
cd orchestrator/worktrees/round-155 && cabal build all && cabal test
```

**Expected:** 1176 examples, 0 failures. No code was changed, so this confirms the documentation edits did not break the build (e.g., no accidental file corruption).

---

## Files Modified (all in worktree `orchestrator/worktrees/round-155/`)

| File | Change Type |
|------|-------------|
| `implementation_notes.md` | Replace lines 28-29, update lines 31-34 |
| `CHANGELOG.md` | Insert bullet after line 6, update count on line 6 |
| `docs/thesis-deviations.yaml` | Update description, add code_path, add test_evidence |

## Constraints

- No code changes (`src/`, `test/`, `app/` untouched)
- All edits in the worktree only
- `cabal build all && cabal test` must pass
