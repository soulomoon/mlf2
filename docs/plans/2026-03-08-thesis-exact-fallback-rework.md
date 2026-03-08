# Thesis-Exact Fallback Rework Implementation Plan

> For Codex: execute this plan with TDD. Write the failing semantic test first for each family, verify RED, implement the minimum production change, verify GREEN, then continue.

## Goal

Remove the residual live fallback behavior identified by the audit so the elaboration/generalization path is truly thesis-exact rather than merely free of old helper names.

## Scope

In scope:
- residual let-level fallback chooser in `MLF.Elab.Elaborate`
- residual `reifyInst` secondary recovery in `MLF.Elab.Elaborate`
- recursive generalization fallback callback in `MLF.Elab.Run.Generalize`
- recursive scheme fallback in `MLF.Elab.Generalize`
- semantic guard tests, docs, and task closeout

Out of scope:
- `checkNoGenFallback`
- `NoFallback` entrypoints
- unrelated result-type / solved-facade work

## Task 1 — Lock Semantic RED Cases

### Files
- Modify `test/PipelineSpec.hs`
- Modify `test/ElaborationSpec.hs`
- Modify `test/GeneralizeSpec.hs`
- Modify `test/Phi/AlignmentSpec.hs` only if needed for a focused `reifyInst` regression

### Steps
1. Add a source guard in `test/PipelineSpec.hs` for the residual let chooser names:
   - `fallbackChoiceFromVar`
   - `fallbackChoiceFromApp`
   - `fallbackChoiceFromLam`
   - `fallbackChoice =`
   - `schChosen = maybe sch0 fst fallbackChoice`
2. Add a source guard in `test/PipelineSpec.hs` for residual `reifyInst` secondary recovery:
   - `targetArgs <|> expansionArgs`
   - `expansionArgs =`
3. Add a source guard in `test/PipelineSpec.hs` for the recursive generalization callback:
   - `let fallback scope' target' =`
   - `applyGeneralizePlan fallback`
4. Add one semantic regression in `test/ElaborationSpec.hs` for a let case that should now fail rather than succeed via chooser replacement.
5. Add one semantic regression in `test/GeneralizeSpec.hs` or a closer generalization-facing spec proving recursive scheme fallback is no longer allowed.
6. Add one semantic regression in `test/ElaborationSpec.hs` or `test/Phi/AlignmentSpec.hs` proving `reifyInst` fails when witness/domain authority is insufficient and only expansion-derived recovery would have succeeded.
7. Run the smallest matching tests and confirm RED.

## Task 2 — Remove Let-Level Fallback Chooser

### Files
- Modify `src/MLF/Elab/Elaborate.hs`
- Update tests from Task 1 as needed

### Steps
1. Delete `fallbackChoiceFromVar`, `fallbackChoiceFromApp`, `fallbackChoiceFromLam`, `fallbackChoiceRaw`, `fallbackChoice`, and related chooser state.
2. Make `ALet` use only the authoritative `(sch0, subst0)` path returned by `generalizeAtNode`.
3. Keep only normalization/closure logic that does not change which scheme was chosen.
4. Remove fallback-oriented debug output fields that mention fallback candidates.
5. Run the focused let-related RED tests and make them GREEN.

## Task 3 — Remove Recursive Generalization Fallback

### Files
- Modify `src/MLF/Elab/Run/Generalize.hs`
- Modify `src/MLF/Elab/Generalize.hs`
- Update tests from Task 1 as needed

### Steps
1. Remove the fallback callback from `generalizeAtWithBuilder`.
2. Change `applyGeneralizePlan` so it no longer accepts or invokes a fallback generalization function.
3. Remove recursive `generalizeAtForScheme schemeScope typeRootC` fallback from `MLF.Elab.Generalize`.
4. Replace that branch with explicit fail-fast behavior using the already-authoritative plan data, or surface a structured error when the authority is missing.
5. Keep rigid-bound reification only where it is direct reification support, not a scheme-recovery mechanism.
6. Run focused generalization slices and make them GREEN.

## Task 4 — Make `reifyInst` Witness/Domain-Only

### Files
- Modify `src/MLF/Elab/Elaborate.hs`
- Update tests from Task 1 as needed

### Steps
1. Remove expansion-derived recovery from `reifyInst`.
2. If the witness/domain-authoritative path cannot produce the required nontrivial instantiation, throw `PhiTranslatabilityError`.
3. Keep direct witness/domain refinement only if it is justified from the witness-owned nodes; do not reintroduce alternate sources.
4. Remove any now-dead helper code and imports.
5. Run the focused `reifyInst` / elaboration slices and make them GREEN.

## Task 5 — Run Focused Gates After Each Family

### Steps
1. Run the new fallback-removal guard matcher.
2. Run checked-authoritative slices.
3. Run dual-path verification slices.
4. Run the focused elaboration/generalization/phi regressions affected by the current family.
5. If a family breaks thesis-authoritative guards in a way that indicates a required semantic distinction, stop and document it before continuing.

## Task 6 — Full Gate and Docs Sync

### Files
- Modify `TODO.md`
- Modify `implementation_notes.md`
- Modify `docs/architecture.md`
- Modify `CHANGELOG.md`
- Update `historical task-tracker path (not retained as a live folder)`
- Update `historical task-tracker path (not retained as a live folder)`
- Update `historical task-tracker path (not retained as a live folder)`

### Steps
1. Run `cabal build all && cabal test`.
2. Update docs to describe only the behavior that is actually present after GREEN.
3. Record which fallback families were removed and how they now fail.
4. If everything is complete, move the task folder to `tasks/archive/`.

## Stop Conditions

Stop and document rather than guessing if:
- thesis-authoritative tests prove a removed path was actually semantically required;
- the only way to preserve GREEN is to add a new compatibility recovery path;
- the remaining behavior cannot be justified by the thesis text or existing witness/domain invariants.
