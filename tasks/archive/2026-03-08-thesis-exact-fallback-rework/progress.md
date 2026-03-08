# Progress — 2026-03-08 thesis-exact fallback rework

- Loaded the required process/domain skills and reread the approved design/implementation docs plus the task artifacts.
- Added RED source guards for the residual let chooser, `reifyInst` secondary recovery, and recursive generalization callback.
- Added semantic strict regressions covering let-chooser replacement, recursive generalization fallback, and expansion-only `reifyInst` recovery.
- Removed the let-level chooser from `MLF.Elab.Elaborate` and kept only authoritative scheme normalization/closure.
- Removed the recursive callback from `MLF.Elab.Run.Generalize` and replaced recursive scheme re-entry in `MLF.Elab.Generalize` with the existing structural scheme plan.
- Removed `reifyInst` secondary recovery and then fixed the second-pass regressions by searching witness/domain-owned copied-node candidates from `etCopyMap` instead of restoring expansion fallback.
- Tightened `AAnnF` so exact-scheme polymorphic annotation subjects are reused directly rather than re-closed into duplicate top-level `forall`s.
- Updated the fallback-dependent alias/nested-let/theory-target expectations to strict fail-fast behavior and regenerated `test/golden/legacy-replay-baseline-v1.json`.
- Final verification:
  - `cabal build all && cabal test` — PASS (`998 examples, 0 failures`)
