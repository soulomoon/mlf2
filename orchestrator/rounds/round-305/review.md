### Checks Run
- Command: `git branch --show-current`
  Result: pass; current branch is `orchestrator/round-305-canonical-parser-parity-next-slice`.
- Command: `git status --short --branch`
  Result: pass; active worktree is the round-305 branch. Dirty paths are the expected round implementation/docs/control-plane artifacts; no `runtime/mlfp_io/target` path appears.
- Command: `sed -n '1,260p' orchestrator/state.json`
  Result: pass; active roadmap is `2026-05-18-00-full-self-boot-end-to-end-roadmap` `rev-004`, with active `round-305` in `review` on the assigned branch/worktree.
- Command: `sed -n '1,240p' orchestrator/rounds/round-305/selection-record.json`
  Result: pass; selected lineage is `milestone-4`, `direction-4a-canonical-parser-parity`, `item-305-parser-parity-import-exposing-spans`.
- Command: `sed -n '1,320p' orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004/roadmap-view.json`
  Result: pass; `milestone-4` remains `in-progress`, and anchors include `milestone-4-status` and `milestone-4-completion`.
- Command: `git status --porcelain=v1 -- runtime/mlfp_io/target`
  Result: pass; no output before parser rechecks and no output after parser rechecks.
- Command: `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass; no output before parser rechecks and no output after parser rechecks.
- Command: `git diff --check`
  Result: pass; no whitespace errors before parser rechecks and no whitespace errors after parser rechecks.
- Command: `git diff --stat`
  Result: pass; tracked diff is limited to `CHANGELOG.md`, `docs/mlfp-self-boot-readiness.md`, `implementation_notes.md`, `orchestrator/state.json`, `test/ProgramParserParitySpec.hs`, and `test/conformance/mlfp/README.md`.
- Command: `git ls-files --others --exclude-standard orchestrator/rounds/round-305 test/conformance/mlfp/parser-parity/import-exposing-def-bool test/programs/compiler-parser-parity/import-exposing-def-bool`
  Result: pass; untracked intended round artifacts, conformance fixture files, and parser-parity test-package files are present.
- Command: `rg -n "ProgramParserParitySpec" mlf2.cabal test/Main.hs`
  Result: pass; no new Haskell spec module was added, and existing `ProgramParserParitySpec` remains registered in `mlf2.cabal` and `test/Main.hs`.
- Command: `rg -n "full parser parity|self-boot|checker|backend|driver|platform|proof|compiler package|compiler-package|parser-complete|not a full|does not claim|remain open|bounded parser" CHANGELOG.md docs/mlfp-self-boot-readiness.md test/conformance/mlfp/README.md implementation_notes.md`
  Result: pass; docs/changelog/readiness wording stays bounded to the import parser tracer and explicitly leaves full parser parity, checker/backend/driver/platform/proof, compiler package, and self-boot out of scope.
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for a single import declaration and source spans/"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed import syntax through public run-program/"'`
  Result: pass; 1 example, 0 failures.
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  Result: pass; 4 examples, 0 failures.
- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool`
  Result: pass; output includes the expected canonical import projection, including `import Prelude span=...:2:10-2:18` and `import exposing type Bool span=...:2:28-2:32`.
- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool`
  Result: pass; round-304 basic Bool projection still prints as expected.

### Plan Compliance
- Add focused RED test in `ProgramParserParitySpec`: met for integrated review; `implementation-notes.md` records RED/GREEN evidence, and the final focused positive matcher exists and passes.
- Add parser-parity fixture and committed expected projection under `test/conformance/mlfp/parser-parity/import-exposing-def-bool/`: met; source and expected projection are present, and the direct package smoke prints the expected projection.
- Add parser-parity `.mlfp` package under `test/programs/compiler-parser-parity/import-exposing-def-bool/`: met; direct `run-program` smoke passes through the public path.
- Keep shared helpers parser-owned and test-package-owned: met; no production facade, Prelude surface, checker, backend, driver, platform, package-manager, ABI, proof, or self-hosting scope was introduced.
- Extend exactly the selected grammar family: met; the slice covers one `import Prelude exposing (Bool);` declaration before the carried Bool definition.
- Extend Haskell-side projection helper only enough to render import module and exposing-item spans: met; the spec renders the selected import module and exposing item evidence.
- Add import-specific negative evidence through public `run-program`: met; focused malformed-import matcher passes.
- Refactor after focused green: met by recorded implementation notes; integrated parser behavior remains green through the focused matchers and full parser-parity group.
- Update docs/readiness without overclaim: met; docs/changelog/readiness wording keeps full parser parity, parser combinators, checker/backend, compiler package, driver, platform, proof, and self-boot claims open.
- Behavior-changing closeout gate: met by prior review before the generated-only retry; the prior review recorded passing focused import positive matcher, malformed import matcher, full parser parity group, package smokes, `cabal build all && cabal test`, and `./scripts/thesis-conformance-gate.sh`. The retry only restored generated depfile churn, so this re-review reran generated-artifact hygiene plus focused parser/package checks.
- Generated-artifact hygiene: met; `runtime/mlfp_io/target` is clean, `libmlfp_io.d` has no diff, and `git diff --check` remains green after parser rechecks.
- Status-only closeout scope: met; milestone 4 remains `in-progress`, no semantic roadmap update is needed, and closeout should add only a compact completion pointer for this bounded tracer.

### Decision
**APPROVED**

### Evidence
The prior rejection blocker is fixed: `git status --porcelain=v1 -- runtime/mlfp_io/target` and `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d` have no output, and `git diff --check` passes after the parser rechecks.

Parser confidence remains sufficient for this retry. The focused positive import parity matcher passed, the malformed-import public-path matcher passed, the full `MLF.Program parser parity` group passed with 4 examples and 0 failures, and both direct parser-parity package smokes print the expected projections.

The implementation stays inside `item-305-parser-parity-import-exposing-spans`: it adds the import-exposing fixture/package and Haskell spec projection evidence, updates bounded progress docs, and does not claim or implement checker/backend/driver/platform/proof/compiler-package/self-boot scope. Milestone 4 is not complete; the approved `review-record.json` leaves it `in-progress` and provides only a compact completion pointer.
