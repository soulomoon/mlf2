# Requirements

## Summary
Add regression tests that guard thesis-exact behavior for explicit forall annotations, especially around annotated variables and forall bounds.

## User stories
- As a maintainer, I want tests that fail if annotated variables introduce extra instantiation structure beyond the thesis κσ translation.
- As a maintainer, I want explicit forall annotations to round-trip through the pipeline without losing quantifier structure.

## Acceptance criteria (EARS)

### REQ-1 Annotated variable uses a single instantiation layer
WHEN an annotated term is a let-bound variable (e.g. `let id = \x. x in (id : ∀a. a -> a)`),
THEN the annotation instantiation edge must use a single `TyExp` over the scheme root,
AND it must not introduce a nested `TyExp` between the annotation edge and the scheme root.

### REQ-2 Explicit forall annotation round-trips
WHEN a variable is annotated with an explicit forall scheme,
THEN the elaborated type must be alpha-equivalent to the annotation scheme (no extra foralls or instantiations).

### REQ-3 Forall-in-bound is preserved
WHEN an annotation uses a bound that itself contains a forall (e.g. `∀(a ⩾ ∀b. b -> b). a -> a`),
THEN the elaborated type must preserve the forall structure in the bound (no normalization that removes it).

## Non-goals
- Changing constraint generation or generalization logic in this spec.
- Reworking the annotation translation beyond what is needed for tests.

## Constraints
- Keep tests aligned with `papers/these-finale-english.txt` and existing annotation notes in `src/MLF/Frontend/ConstraintGen/Translate.hs`.
- Use existing Hspec patterns in `test/ConstraintGenSpec.hs` and `test/ElaborationSpec.hs`.
