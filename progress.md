# Progress

## Session: 2026-04-19

### Phase 6: Constructor-local forall evidence
- **Status:** completed
- **Started:** 2026-04-19
- Actions taken:
  - Re-read `AGENTS.md` and existing planning files.
  - Confirmed branch is clean at `04631882`.
  - Confirmed `/Users/ares/.agents/skills/haskell-pro/SKILL.md` is absent.
  - Inspected current deferred constructor metadata, constructor lowering,
    finalization, and external environment injection.
  - Added internal external binding modes and an unchecked detailed pipeline
    entrypoint for program-obligation finalization.
  - Expanded deferred constructor metadata with occurrence type, source type,
    expected-type substitution seed, runtime instantiation binder order, and
    binding mode.
  - Changed source constructor variables/applications to always lower through
    deferred placeholders.
  - Made constructor and case placeholders inference-local while retaining
    concrete types in the finalization/typecheck environment.
  - Reworked deferred case lowering to avoid pre-finalization direct Church
    application and to leave ordinary ADT handlers unannotated unless
    constructor-local foralls require annotations.
  - Finalized constructor obligations by solving substitutions after eMLF
    inference and rewriting placeholders to concrete runtime constructor names.
  - Added generated-runtime handling for recoverable constructor-local forall
    constructors and skipped unrecoverable generated bindings so source uses
    fail with `ProgramAmbiguousConstructorUse`.
  - Updated the repository guardrail to recognize the internal external-binding
    detailed pipeline entrypoints and the post-rewrite typecheck guard.
  - Added strict matrix rows for ordinary nullary/nested constructors, GADT,
    existential, nullary indexed, explicit polymorphic nullary, and ambiguous
    constructor-local forall cases.
- Files modified:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
  - `src/MLF/Elab/Run/Pipeline.hs`
  - `src/MLF/Frontend/ConstraintGen.hs`
  - `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - `src/MLF/Frontend/ConstraintGen/Types.hs`
  - `src/MLF/Frontend/Program/Check.hs`
  - `src/MLF/Frontend/Program/Elaborate.hs`
  - `src/MLF/Frontend/Program/Finalize.hs`
  - `src/MLF/Frontend/Program/Types.hs`
  - `test/ProgramSpec.hs`
  - `test/RepoGuardSpec.hs`

### Phase 1-5: Deferred program obligations implementation
- **Status:** completed
- **Started:** 2026-04-19 19:04:43 CST
- Actions taken:
  - Re-read `AGENTS.md`.
  - Confirmed `git status --short --untracked-files=all` was initially clean.
  - Read installed planning skill and refreshed stale planning files.
  - Confirmed `/Users/ares/.agents/skills/haskell-pro/SKILL.md` is absent.
  - Added unified deferred program obligation types and switched lowering state
    to record obligations instead of method-only entries.
  - Lowered applied constructors and constructor-pattern cases through
    placeholders.
  - Added partial-method eta expansion for supplied overloaded method prefixes.
  - Added an internal detailed pipeline result and changed program
    finalization to call it.
  - Drafted ordered constructor, case, then method finalization passes.
  - Moved the 7 pending-success rows into the strict eMLF boundary matrix.
  - Repaired regressions in local let-polymorphism, annotated overloaded
    arguments, direct recursive ADT fixtures, and parameterized/GADT
    constructor calls.
  - Updated `RepoGuardSpec`, README, architecture notes, implementation notes,
    and changelog for the detailed pipeline/deferred-obligation path.
  - Ran full repository validation and the whitespace diff guard.
  - Removed the initial typeclass/constructor fallback choices: recursive
    explicit and derived `Eq Nat` are restored, overloaded method runtime
    selection stays deferred, and constructor placeholders rewrite in
    finalization.
- Files modified:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
  - `src/MLF/Frontend/Program/Types.hs`
  - `src/MLF/Frontend/Program/Elaborate.hs`
  - `src/MLF/Frontend/Program/Finalize.hs`
  - `src/MLF/Elab/Run/Pipeline.hs`
  - `src/MLF/Elab/TypeCheck.hs`
  - `src/MLF/Frontend/Program/Check.hs`
  - `test/ProgramSpec.hs`
  - `test/RepoGuardSpec.hs`
  - `test/programs/recursive-adt/typeclass-integration.mlfp`
  - `README.md`
  - `docs/architecture.md`
  - `implementation_notes.md`
  - `CHANGELOG.md`

## Test Results
| Test | Expected | Actual | Status |
|------|----------|--------|--------|
| Initial `git status --short --untracked-files=all` | Clean | Clean | pass |
| `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program eMLF"'` | Pass | 27 examples, 0 failures | pass |
| `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'` | Pass | 60 examples, 0 failures | pass |
| `cabal build all && cabal test` | Pass | 1609 examples, 0 failures | pass |
| `git diff --check` | Clean | Clean | pass |
| `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program eMLF"'` | Pass | 34 examples, 0 failures | pass |
| `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'` | Pass | 67 examples, 0 failures | pass |
| `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'` | Pass | 8 examples, 0 failures | pass |
| `cabal build all && cabal test` | Pass | 1616 examples, 0 failures | pass |
| `git diff --check` | Clean | Clean | pass |

## Error Log
| Timestamp | Error | Attempt | Resolution |
|-----------|-------|---------|------------|
| 2026-04-19 19:04 CST | Worktree-local planning skill path missing | 1 | Used installed skill path |
| 2026-04-19 19:04 CST | `haskell-pro` skill path missing | 1 | Continue with repo conventions |
| 2026-04-19 19:04 CST | `cabal build mlf2` ambiguous between lib and exe | 1 | Use explicit Cabal component targets |
| 2026-04-19 19:04 CST | Parallel `cabal run` probes collided in `dist-newstyle` | 1 | Use serial Cabal commands for probes and validation |
| 2026-04-19 20:xx CST | `MLF.Program` corpus regressions in GADT/existential/typeclass fixtures | 1 | Replaced early method runtime resolution with typed placeholders, finalized instance method bodies directly, deferred constructor applications, and added nullary constructor placeholder rewrite |
| 2026-04-19 21:xx CST | All-deferred constructor placeholders initially caused locked-node and stale-let-scheme failures | 1 | Made constructor/case placeholders inference-local, added unchecked internal pipeline entrypoint, refreshed let schemes after obligation rewrites, and lowered deferred cases without synthetic scrutinee lets |
| 2026-04-19 21:xx CST | Constructor-local forall runtime bindings failed before source ambiguity/finalization | 1 | Accepted recoverable generated constructor-forall bindings through the internal unchecked path and skipped unrecoverable generated bindings |
| 2026-04-19 21:xx CST | Repository guard still required the old scheme-only detailed pipeline marker | 1 | Updated the guard to require the new external-binding detailed/unchecked entrypoints and final post-rewrite typecheck |
