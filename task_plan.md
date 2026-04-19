# Task Plan

## Summary
Goal: implement deferred `.mlfp` program obligations, including constructor
obligations that carry constructor-local `forall` evidence, so program-owned
features resolve after eMLF inference without widening public eMLF syntax or
adding a direct `.mlfp -> ElabTerm` route.

## Current Phase
Completed: constructor-local `forall` evidence is carried in deferred constructor obligations and all source constructor uses lower through placeholders.

## Phases
1. Discovery and obligation design. - completed
2. Refactor `.mlfp` lowering/finalization for deferred program obligations. - completed
3. Move pending rows to strict matrix and add negative guards. - completed
4. Run focused validation and repair failures. - completed
5. Run full validation and update docs. - completed
6. Add constructor-local `forall` evidence and defer all source constructor uses. - completed

## Decisions Made
| Decision | Rationale |
|----------|-----------|
| Keep public eMLF parser/API unchanged | User plan and repo invariants require `.mlfp`-internal obligations only |
| Preserve existing Church ADT runtime representation | Current recursive ADT/program runtime depends on this encoding |
| Use post-eMLF finalization as the dispatch point | Matches the existing deferred overload path and lets eMLF infer terms first |
| Defer every source constructor occurrence | Constructor placeholders now carry expected-type seeds, constructor-local forall binders, and runtime instantiation order so GADT, existential, nullary, and ordinary constructors all finalize after eMLF inference |
| Preserve public pipeline APIs while adding internal external binding modes | `.mlfp` needs monomorphic constructor placeholders without exposing new public eMLF APIs |
| Use an unchecked internal detailed pipeline only for program obligations/generated constructor-forall bindings | Program placeholders may not typecheck until finalization rewrites them; public eMLF APIs still keep the normal typecheck guard |
| Skip generated runtime bindings for constructor-local foralls with no recoverable evidence | Such constructors cannot be instantiated safely; source occurrences fail later with `ProgramAmbiguousConstructorUse` |

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| Installed `haskell-pro` path from `AGENTS.md` is missing | 1 | Follow repo formatting conventions directly |
| Planning skill relative path under worktree was absent | 1 | Read installed skill at `/Users/ares/.codex/skills/planning-with-files/SKILL.md` |
| Recursive overloaded method calls hit locked-node failures when resolved through synthetic local wrappers or reannotated pattern variables | 2 | Finalize instance method bodies as their runtime binding, defer overloaded runtime resolution, and leave pattern-bound recursive fields unannotated |
| All-deferred constructor placeholders exposed recursive μ types as external schemes and hit locked-node failures | 6 | Added internal external binding modes and made constructor/case placeholders inference-local while retaining concrete types for finalization/typecheck |
| Direct deferred cases applied handlers to data-typed placeholders and clashed with μ encodings | 6 | Lower deferred cases as placeholder applications and rewrite them after constructor finalization |
| Generated constructor-local forall runtime bindings could fail before source ambiguity was checked | 6 | Route recoverable generated bindings through the unchecked internal path and skip unrecoverable generated bindings |
| Repository guard expected the older scheme-only detailed pipeline marker | 6 | Guard now requires the internal external-binding detailed/unchecked entrypoints plus the final post-rewrite typecheck |
