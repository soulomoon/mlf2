# Task Plan: Remove Remaining Live Fallback Mechanisms for Thesis-Exactness

## Metadata
- Date: 2026-03-08
- Branch: `codex/remove-live-fallbacks-thesis-exact`
- Execution mode: thesis-exact cleanup campaign
- Skills in use: using-superpowers, planning-with-files, executing-plans, using-git-worktrees, test-driven-development, haskell-pro

## Goal
Remove the remaining live fallback mechanisms from elaboration, planner owner resolution, and instantiation inference, replacing them with thesis-exact fail-fast behavior.

## Guard Targets
- `generalizeNeedsFallback`
- `fallbackToReify`
- `fallbackSchemeType`
- `allowFallbackFromTrace`
- `resolveFallbackArgNodes`
- synthesized wrapper-root fallback in planner owner resolution
- generic fallback branch in `inferInstAppArgsFromScheme`

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Guard-first fallback capture | completed | Direct guards now exist for generalization, planner-owner, and instantiation fallback markers |
| 2. Generalization ladder removal | completed | Core ladders removed and the remaining BUG-003 / explicit-forall regressions were repaired under the strict path |
| 3. `reifyInst` fallback removal | completed | Trace-search helpers are gone; remaining annotated-consumer / explicit-forall regressions were repaired with witness-/expansion-authoritative handling |
| 4. Planner fallback removal | completed | Current tree already carries strict body-root owner resolution and matching guards |
| 5. Instantiation fallback removal | completed | Current tree already carries strict inference behavior and matching guards |
| 6. Documentation and closeout | completed | Docs/task records synced and the campaign has been archived |

## Decisions
- Prefer strict failure over compatibility recovery whenever a fallback path is removed.
- Keep `checkNoGenFallback` and `NoFallback` entrypoints untouched.
- Stop a family if thesis-authoritative guards indicate the fallback is semantically required.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `zsh` command lookup missed `mkdir` during worktree setup | 1 | Retried with `/bin/bash` and `/bin/mkdir`; worktree creation succeeded |
| Direct `reifyInst` fail-fast replacement broke broad parity/alignment baselines | 1 | Reverted the family and recorded it as a provisional keeper pending a deeper witness-authoritative replacement |
| Strict generalization removal still leaves thesis-facing regressions under `cabal test` | 1 | Stopped before further speculative changes; branch currently builds, but test suite is still blocked |
| Full gate still red after restoring let-polymorphic `Phi` alignment | 1 | Narrowed the remaining cluster to BUG-004 annotated consumers, explicit-forall coercion / redirect-stability baselines, and BUG-003 `SchemeFreeVars` regressions |
| Final full gate verification after the last authority/closure fixes | 1 | `cabal build all && cabal test` now passes (`992 examples, 0 failures`) |
