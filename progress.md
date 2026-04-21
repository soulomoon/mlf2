# Progress

## Session: 2026-04-20

### Phase 1: Discovery and planning refresh
- **Status:** completed
- **Started:** 2026-04-20
- Actions taken:
  - Re-read `AGENTS.md`.
  - Loaded the installed `planning-with-files` skill.
  - Confirmed the branch is clean and aligned with origin before edits.
  - Confirmed `/Users/ares/.agents/skills/haskell-pro/SKILL.md` is absent.
  - Inspected current `.mlfp` AST, parser, checker, elaborator, runtime, public
    exports, and tests.
  - Replaced stale constructor-obligation planning files with the new language
    surface implementation plan.

### Phase 2: Located diagnostics
- **Status:** completed
- Actions taken:
  - Added `SourceSpan`, `LocatedProgram`, parser span indexes, and located
    parse/check/run entrypoints while keeping the existing unlocated APIs.
  - Added `ProgramDiagnostic` with rendered source locations, messages, and
    mechanically justified hints for high-value program errors.
  - Updated the CLI to parse and run through the located path.

### Phase 3: Ordered nested patterns
- **Status:** completed
- Actions taken:
  - Replaced flat constructor binders with recursive `PatCtor`, `PatVar`,
    `PatWildcard`, and `PatAnn`.
  - Updated parser, pretty-printer, deriving generation, binder collection,
    case lowering, and reachability checks.
  - Added matrix coverage for nested constructor patterns, wildcard fallthrough,
    pattern annotations, unreachable branches, and nested non-exhaustiveness.

### Phase 4: Language reference
- **Status:** completed
- Actions taken:
  - Added `docs/mlfp-language-reference.md`.
  - Linked the reference from `README.md`.
  - Updated `docs/syntax.md`, `docs/architecture.md`, `implementation_notes.md`,
    and `CHANGELOG.md`.

### Phase 5: Prelude and runtime values
- **Status:** completed
- Actions taken:
  - Added source-level `MLF.Frontend.Program.Prelude`.
  - Updated CLI/file execution to prepend the built-in Prelude for explicit
    imports and reject user-defined `Prelude` conflicts.
  - Added ADT value rendering for recovered closed Church values, including
    parameterized payloads such as `Some (Succ Zero)`.

### Phase 6: Typeclass/module ergonomics
- **Status:** completed
- Actions taken:
  - Added constrained type syntax to definitions, methods, and instances.
  - Lowered constraints to hidden typeclass evidence and resolved constrained
    calls from local evidence or schema instances.
  - Added overlapping instance rejection by unification.
  - Implemented parameterized `deriving Eq`, including recursive `List a`.
  - Added qualified and aliased imports with qualified references in
    expressions, patterns, types, constraints, constructors, classes, and
    methods.
  - Updated Prelude to export `Nat(..)`, `Option(..)`, `List(..)`, `Eq`, `eq`,
    `and`, and `id`.

### Phase 7: Validation
- **Status:** completed
- Actions taken:
  - Ran focused `MLF.Program` successfully after the typeclass/module slice.
  - Ran focused `MLF.Program eMLF` successfully after the typeclass/module slice.
  - Ran the full repository build and test gate successfully.

## Test Results
| Test | Expected | Actual | Status |
|------|----------|--------|--------|
| `cabal build mlf2:mlf2-internal lib:mlf2 exe:mlf2` | build succeeds | build succeeded | pass |
| `cabal run mlf2 -- run-program` Prelude `Nat` probe | `Succ Zero` | `Succ Zero` | pass |
| `cabal run mlf2 -- run-program` Prelude `Option` probe | `Some Zero` | `Some Zero` | pass |
| `cabal run mlf2 -- run-program` Prelude `List` probe | `Nil` | `Nil` | pass |
| `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'` | pass | 88 examples, 0 failures | pass |
| `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program eMLF"'` | pass | 51 examples, 0 failures | pass |
| `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface"'` | pass | 24 examples, 0 failures | pass |
| `cabal build all && cabal test` | pass | 1637 examples, 0 failures | pass |
| `git diff --check` | clean | clean | pass |

## Error Log
| Timestamp | Error | Attempt | Resolution |
|-----------|-------|---------|------------|
| 2026-04-20 | `haskell-pro` skill path from `AGENTS.md` missing | 1 | Continue using repo conventions |
| 2026-04-20 | `cabal build mlf2` ambiguous between executable and library | 1 | Use explicit component targets such as `lib:mlf2` and `exe:mlf2` |
| 2026-04-20 | Prelude `List` with recursive `Cons : a -> List a -> List a` failed generated constructor type comparison | 1 | Implemented constrained parameterized deriving and constructor-spine repair; Prelude now keeps `Cons` and derives `Eq (List a)` |
| 2026-04-20 | Prelude `Eq (Option Nat)` inferred a field comparison as missing `Eq a` | 1 | Implemented schema instance evidence; Prelude now derives constrained `Eq (Option a)` |
| 2026-04-20 | Recursive `Eq (List a)` initially normalized to an xMLF term instead of `true` | 1 | Generated recursive derived Eq through a monomorphic local self function and repaired parameterized constructor quantifier order |
| 2026-04-20 | Parallel `cabal run` probes collided on `package.conf.inplace` | 1 | Avoid parallel Cabal invocations and run Cabal commands sequentially |
