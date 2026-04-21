# Findings

## Requirements
- Add source-span support and user-facing `.mlfp` diagnostics without breaking
  existing unlocated parser/checker/runtime APIs.
- Document current `.mlfp` semantics as a language reference linked from
  `README.md`.
- Upgrade pattern matching from flat constructor binders to ordered nested
  patterns while preserving Church-encoded runtime behavior.
- Add explicit built-in `Prelude` support and readable ADT value rendering.
- Add a first constrained typeclass/deriving/module ergonomics slice where it
  can stay coherent and fail-closed.
- Keep public raw eMLF syntax and the shared eMLF/xMLF pipeline boundary.

## Current Observations
- `src/MLF/Frontend/Syntax/Program.hs` now keeps the old unlocated AST and adds
  `LocatedProgram` plus span indexes for parser/checker use.
- `src/MLF/Frontend/Parse/Program.hs` now exposes located and unlocated parser
  entrypoints. The unlocated API remains a projection of the located parse.
- `src/MLF/Frontend/Program/Check.hs` still returns `Either ProgramError` for
  compatibility and adds `checkLocatedProgram :: LocatedProgram -> Either
  ProgramDiagnostic CheckedProgram`.
- `src/MLF/Frontend/Program/Elaborate.hs` now lowers ordered nested patterns
  through the existing deferred case machinery.
- `src/MLF/Frontend/Program/Run.hs` now exposes `runLocatedProgram` and renders
  recovered ADT values with source constructor syntax.
- Public `.mlfp` APIs are re-exported by `src-public/MLF/Program.hs` and
  `src-public/MLF/Pipeline.hs`; the additive located/diagnostic/prelude exports
  are wired there.

## Technical Decisions
| Decision | Rationale |
|----------|-----------|
| Represent located parsing as an additive `LocatedProgram` wrapper with span indexes | Avoids forcing every existing program test to rewrite immediately |
| Map high-value diagnostics from the located wrapper to likely declaration/expression spans | Provides useful CLI errors now without changing every checker function signature in the first slice |
| Extend `Pattern` directly and update existing call sites | Nested patterns are source semantics, so the AST should own them |
| Lower nested patterns before handler generation | Existing deferred case finalization remains the authority for Church eliminators |
| Keep the Prelude explicit but useful | Users still opt in with `import Prelude ...`, while the built-in module now exposes `Nat(..)`, `Option(..)`, recursive `List(..)`, `Eq`, `eq`, `and`, and `id` |
| Repair parameterized constructor runtime spines instead of restoring direct constructor-use lowering | Source constructor uses still lower through deferred obligations, and generated Church constructor bindings keep a typecheckable runtime quantifier order |
| Derive schema instances for parameterized ADTs | `Option a` and recursive `List a` can derive `Eq a => Eq (...)`; recursive owner fields use the currently generated equality evidence |
| Reuse finalization source-type recovery in runtime rendering | Checked bindings store lowered expected types, so the runner must recover `Option Nat`/`List Nat` before decoding parameterized Church values |
| Model class constraints as hidden method evidence | Raw eMLF remains unchanged, while `.mlfp` constrained values and schema instances lower to deterministic internal evidence parameters |
| Treat qualified imports as scoped aliases, not re-exports | `import M as A` adds qualified-only names; `import M as A exposing (...)` also imports the selected exports unqualified, and hidden constructors stay hidden |

## Implemented Coverage
- Located parser/checker/runner APIs and CLI diagnostic rendering.
- `ProgramDiagnostic` rendering with spans and hints for ambiguous
  constructors, ambiguous methods, no matching instances, type mismatches,
  constructor-pattern mismatches, non-exhaustive cases, and import/export
  visibility failures.
- Ordered nested patterns, wildcards, variable patterns, and pattern
  annotations.
- Explicit CLI Prelude import target with conflict rejection.
- Source-shaped ADT value rendering for `Nat`, `Option`, and `List`.
- Constraint-aware definitions, method signatures, and instance declarations.
- Schema instance resolution with overlap rejection by unification.
- Parameterized `deriving Eq` for `Option a` and recursive `List a`.
- Qualified and aliased imports with visibility checks.
- User-facing `.mlfp` language reference linked from `README.md`.

## Remaining Follow-Up
- `Eq Bool`, blocked on a source-level conditional or primitive boolean
  eliminator for generated equality bodies.
- Re-export ergonomics and more precise qualified-import diagnostics.
- Broader typeclass ergonomics beyond single-parameter `Eq`-style classes,
  such as richer constraint solving and user-facing cycle diagnostics.
