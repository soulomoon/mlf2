### Checks Run
- Command: `git diff --check`
  Result: pass. No whitespace errors.
- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects constructor pattern fields passed where the wrapper type is expected"'`
  Result: pass. 1 matching example, 0 failures. This is the focused parent-drift regression for the `Elaborate.hs` source-type compatibility preservation.
- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "allows delayed top-level recursion through lambda closures"'`
  Result: pass. 1 matching example, 0 failures. This rechecks the round-355 delayed-recursion regression after `ProgramSpec.hs` was refreshed to preserve both regressions.
- Command: `diff -u src/MLF/Frontend/Program/Elaborate.hs /Volumes/src/mlf4/src/MLF/Frontend/Program/Elaborate.hs | sed -n '1,160p'`
  Result: pass. Empty output; the assigned worktree preserves the parent checker hunk exactly for this file.
- Command: `rg -n "allows delayed top-level recursion through lambda closures|rejects constructor pattern fields passed where the wrapper type is expected" test/ProgramSpec.hs`
  Result: pass. Both the round-355 delayed-recursion regression and the parent drift regression are present.
- Command: `rg -n "sourceTypesNeedRejection|sourceTypesCompatible|sourceTypesCompatibleInstantiatingActual|sourceTypesNeedNominalRejection|ensureSourceTypeCompatible" src/MLF/Frontend/Program/Elaborate.hs`
  Result: pass. The compatibility/rejection predicate split is present and wired through `ensureSourceTypeCompatible`.
- Command: `rg -n "parseModuleBodyRowsFirst|parseRecursiveConstructorDataRows|parseEqClassMethodRowsFirst|parseEqNatInstanceMethodRowsFirst|parseDataLedSourceDefinitionSuffixRows|parseBoundedSourceDefinitionRows|parseSixDataFourDefinition" test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
  Result: pass. Recursive module-body, recursive constructor-row, and recursive method-row helpers are present; retired exact-count helper names did not appear.
- Command: `ruby -e '<parent-drift plus round-355 phrase guard over WitnessNorm, Elaborate, Run, WitnessSpec, ProgramSpec, ParserParityParser.mlfp, and ProgramParserParitySpec.hs; retired parser helper absence checked>'`
  Result: pass. `parent-drift plus round-355 guard passed: 22 required phrases checked; 5 retired phrases absent`.
- Command: `ruby -e '<changed-line shortcut/overclaim guard over all changed source/spec surfaces>'`
  Result: pass. `changed-line guard passed: 859 added source/spec lines checked`.
- Command: `git diff --name-only -- orchestrator/roadmaps orchestrator/state.json CHANGELOG.md implementation_notes.md`
  Result: pass. Empty output; no active roadmap, state, root implementation notes, or changelog files changed.

Additional full standard gates were not rerun for this finalization-only preservation refresh. The previous full standard gates are recorded as passed in `implementation-notes.md`, and the latest delta is limited to preserving parent `Elaborate.hs` support plus the exact focused parent-drift regression that now passes.

### Plan Compliance
- Step 1 branch/worktree boundary: met. Review ran in `/Volumes/src/mlf4/orchestrator/worktrees/round-355` on `orchestrator/round-355-ergonomics-substrate`; parent checkout files were read only, and `orchestrator/state.json` was not edited.
- Step 2 blocker evidence preservation: met. `implementation-notes.md` preserves the original presolution/runtime blocker evidence and appends finalization drift-preservation evidence.
- Step 3 `WitnessNorm.hs` live-binder filtering: met from the previously approved surface. The static guard still finds `normalizationBinderArgs`, `liveNodeKeys`, and `Witness.binderArgs = normalizationBinderArgs`.
- Step 4 no-replay witness regression: met from the previously approved surface. The regression phrase remains present and prior focused evidence is recorded.
- Step 5 focused method-row reproducer: met from the previously approved surface. The broad parser-parity and method-row focused checks remain recorded as passing in `implementation-notes.md`.
- Step 6 parser-library method-row substrate: met from the previously approved surface. The refreshed static guards still find recursive module-body, recursive constructor-row, and class/instance method-row substrate, with retired exact-count helpers absent.
- Step 7 dynamic and static method-row guards: met. The implementation-notes guard is credible for the broad surface, and the reviewer reran a current parent-drift plus round-355 phrase guard after the `Elaborate.hs` refresh.
- Step 8 optional runtime surface: met. The round-355 runtime regression `allows delayed top-level recursion through lambda closures` remains present and passed in the current review.
- Step 9 prohibited shortcuts and overclaims: met. The current changed-line guard found no fixture-name shortcuts, pre-rendered projections, canonical-parser bypasses, retired aliases, or compiler-package/platform/proof/native/package/self-boot/full-parser-parity claims.
- Step 10 implementation evidence: met. `implementation-notes.md` records the finalization drift-preservation refresh, including the initial focused failure before restoring the parent `Elaborate.hs` support and the passing focused regression after it.
- Added `Elaborate.hs` drift preservation: compliant for finalization. The hunk exactly matches the parent checkout, is paired with the parent regression in `test/ProgramSpec.hs`, and keeps the latest worktree from dropping already-present parent source-type compatibility support during round finalization.

### Findings
- Blocking: no
  Problem: No blocking findings.
  Evidence: `git diff --check` passed; the required parent-drift focused regression passed; the additional delayed-recursion focused regression passed; current static drift/overclaim guards passed; `Elaborate.hs` matches the parent checkout exactly.
  Suggested fix: none

### Decision
**APPROVED**

### Retry
- Retry target: none
- Required changes: none

### Roadmap Closeout
- Mode: none
- Status changes: none
- Completion pointers: none
- History entries: none
- Semantic update reason: none

### Evidence
- `src/MLF/Frontend/Program/Elaborate.hs` now routes `ensureSourceTypeCompatible` through `sourceTypesNeedRejection`, preserving ordinary compatibility checks, forall-body compatibility for actual types, and the nominal rejection guard.
- `test/ProgramSpec.hs` preserves both regressions required by the finalization refresh: `allows delayed top-level recursion through lambda closures` and `rejects constructor pattern fields passed where the wrapper type is expected`.
- The final refresh does not alter roadmap files, controller state, root implementation notes, or changelog files.
- The selected milestone remains milestone 4, and this round still provides bounded parser/compiler-frontend ergonomics substrate plus drift preservation. It does not close full canonical parser parity and makes no compiler-package, platform/proof, native/backend, package-manager/linker, self-boot, or full-parser-parity claim, so `Roadmap Closeout` mode is `none`.
