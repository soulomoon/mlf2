### Checks Run
- Command: `git status --short --branch`
  Result: pass; worktree is on `orchestrator/round-309-canonical-parser-parity-next-slice`. Diff includes the expected implementation/doc/test files, untracked round-309 artifacts, untracked data-declaration parser-parity fixtures, and a controller-owned `orchestrator/state.json` diff.
- Command: `git diff --check`
  Result: pass; no whitespace or conflict-marker output.
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  Result: pass; 12 examples, 0 failures. This covered the new data-declaration positive matcher, malformed data-declaration evidence, and rounds 304-308 parser-parity regressions.
- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans`
  Result: pass; printed the committed parser projection with `Nat(..)` export, `main` export, `data Nat`, `Zero`/`Succ` constructor spans, `Nat -> Nat`, and `Succ Zero`.
- Command: `for pkg in basic-module-def-bool import-exposing-def-bool value-def-list-int-ref let-lambda-application typed-annotation-types; do printf '\n== %s ==\n' "$pkg"; cabal run mlf2 -- run-program "test/programs/compiler-parser-parity/$pkg"; done`
  Result: pass; all five prior parser-parity package smokes printed their expected projections.
- Command: `cabal build all && cabal test`
  Result: pass; full suite completed with 2624 examples, 0 failures.
- Command: `./scripts/thesis-conformance-gate.sh`
  Result: pass; obligations and claims checks were green, and final output reported `PASS: thesis conformance anchors are green`.

### Plan Compliance
- Focused parser-parity behavior: met. `ProgramParserParitySpec` now compares the Haskell canonical projection and parser-owned `.mlfp` output for `data-declaration-constructor-spans` against the committed expected projection.
- Selected fixture and expected artifact: met. Added `test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/src/Main.mlfp` and `expected/parser-program.txt`.
- Parser-owned `.mlfp` package: met. Added the selected package under `test/programs/compiler-parser-parity/data-declaration-constructor-spans/` with source/token/AST/parser modules scoped to the data-declaration tracer.
- Test-owned projection renderer: met. Extended only `test/ProgramParserParitySpec.hs` support to render ordered exports, `ExportTypeWithConstructors`, `DeclData`, constructor spans, `Nat -> Nat`, and the carried constructor application.
- Malformed data-declaration evidence: met. The public `run-program` path now checks malformed constructor-colon evidence and the parser-parity group passed it.
- Regression preservation: met. The parser-parity group and five historical package smokes passed.
- Docs and progress notes: met. `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md` describe a bounded parser-only tracer and explicitly leave full parser parity, parser combinators, checker/backend, driver, platform, proof, and self-boot completion open.
- Scope discipline: met. The diff stays within `item-309-parser-parity-data-declaration-constructor-spans`; it does not implement or claim the future shared parser-combinator/parser-monad consolidation, does not widen production facades, and does not touch checker/backend/driver/platform/proof scope. The existing controller-owned `orchestrator/state.json` diff is not treated as an implementation failure.

### Decision
**APPROVED**

### Evidence
The integrated result satisfies the selected round plan and active milestone-4 verification: parser-owned `.mlfp` modules emit the same selected parsed-program projection and spans as the canonical Haskell parser for the data-declaration fixture, negative parser evidence runs through the public `run-program` path, and prior parser-parity fixtures remain green.

Closeout classification is `status-only`: round-309 is a partial milestone-4 completion pointer only. It does not mark `milestone-4` done and does not change future coordination, extraction scope, sequencing, verification meaning, or the operator correction that the next parser round must consolidate into shared parser-combinator/parser-monad infrastructure.
