# Task Plan

## Summary
Goal: make `.mlfp` a more usable source language while preserving the shared
eMLF/xMLF pipeline boundary. Implement additive diagnostics, a language
reference, nested/ordered patterns, explicit prelude support, readable ADT
runtime values, and the first typeclass/module ergonomics slice
without adding a second `.mlfp -> ElabTerm` authority path.

## Current Phase
Complete.

## Phases
1. Discovery and planning refresh. - completed
2. Add located parsing and user-facing diagnostics. - completed
3. Upgrade patterns to nested/ordered lowering. - completed
4. Add language reference and readable example programs. - completed
5. Add explicit prelude support and ADT value rendering. - completed
6. Add constrained typeclass/deriving and module ergonomics slice. - completed
7. Run focused and full validation. - completed

## Decisions Made
| Decision | Rationale |
|----------|-----------|
| Keep public raw eMLF unchanged | User plan and repo invariants reserve language features for `.mlfp` |
| Keep current unlocated APIs | Additive APIs let existing equality tests continue while CLI can use richer diagnostics |
| Preserve Church runtime representation | Existing constructor/case lowering and runtime tests depend on it |
| Compile nested patterns to existing case/deferred-case machinery | Avoids adding a runtime pattern primitive or direct `.mlfp -> ElabTerm` path |
| Make the prelude explicit-import in v1 | Avoids surprising user namespace collisions |
| Keep constraints and qualified names as `.mlfp`-owned source semantics | Public raw eMLF remains thesis-facing and unchanged |
| Keep derived recursive Eq monomorphic internally | Avoids re-inferring a polymorphic schema instance from hidden evidence during recursive field comparison |

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| `/Users/ares/.agents/skills/haskell-pro/SKILL.md` missing | 1 | Follow existing repo Haskell style directly |
| Concurrent `cabal run` probes collided on `package.conf.inplace` | 1 | Avoid parallel Cabal invocations; run build/test commands sequentially |
| Recursive parameterized `List` deriving exposed constructor quantifier ordering issues | 1 | Repair parameterized constructor runtime spines and use monomorphic local self for generated recursive Eq |
