### Changes Made
- `test/ProgramParserParitySpec.hs`: added the round-309 public parser-parity matcher, malformed data-declaration run-program matcher, data-declaration fixture/package paths, negative evidence package writer, ordered export rendering, `ExportTypeWithConstructors` rendering, `DeclData` projection, constructor declaration projection, and carried constructor-application rendering.
- `test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/src/Main.mlfp`: added the canonical source fixture for `module Main export (Nat(..), main) { data Nat = Zero : Nat | Succ : Nat -> Nat; def main : Nat = Succ Zero; }`.
- `test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/expected/parser-program.txt`: committed the canonical parser projection with ordered export items, the data declaration span, constructor spans, `Nat -> Nat` type rendering, and `Succ Zero` expression rendering.
- `test/programs/compiler-parser-parity/data-declaration-constructor-spans/`: added the parser-owned `.mlfp` package (`Main.mlfp`, `ParserParitySource.mlfp`, `ParserParityToken.mlfp`, `ParserParityAst.mlfp`, `ParserParityParser.mlfp`) for exactly the selected data-declaration grammar family plus malformed constructor-colon evidence.
- `test/conformance/mlfp/README.md`: carried forward the round-304 through round-308 fixture list and added only the data-declaration Nat parser-projection fixture.
- `docs/mlfp-self-boot-readiness.md`, `CHANGELOG.md`, `implementation_notes.md`: updated progress/readiness notes for the bounded parser-only tracer without claiming checker, backend, driver, platform, proof, parser-combinator, full parser parity, or self-boot completion.
- `runtime/mlfp_io/target/release/libmlfp_io.d`: restored generated absolute-path churn after closeout validation; there is no final diff for this file.

### Tests
- RED: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for data declarations and constructor spans/"'` failed before implementation with `expected one value export, got: Just [ExportTypeWithConstructors "Nat",ExportValue "main"]`.
- GREEN: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for data declarations and constructor spans/"'` passed, 1 example, 0 failures.
- GREEN negative: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed data declarations through public run-program/"'` passed, 1 example, 0 failures.
- Regression slice: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'` passed with 12 examples and 0 failures, preserving rounds 304-308.
- Package smoke: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans` passed and printed the committed data-declaration parser projection.
- Prior parser-parity package smokes passed for `basic-module-def-bool`, `import-exposing-def-bool`, `value-def-list-int-ref`, `let-lambda-application`, and `typed-annotation-types`.
- Diff hygiene: `git diff --check` passed with no output.
- Full closeout: `cabal build all && cabal test` passed; `mlf2-test` reported 2624 examples, 0 failures.
- Thesis gate: `./scripts/thesis-conformance-gate.sh` passed; final line reported `PASS: thesis conformance anchors are green`.

### Notes
- Scope stayed inside `item-309-parser-parity-data-declaration-constructor-spans` and milestone 4 parser parity. The `.mlfp` package remains fixture-scoped and parser-owned; no Prelude, public facade, production parser API, checker, backend, package manager, driver, platform, proof, class/instance/type-family/case syntax, or parser-combinator scope was added.
- The parser-owned token stream for this slice is intentionally fixture-shaped after an initial wider token ADT caused presolution arity churn in the `.mlfp` package. The final implementation keeps the behavior bounded to the selected canonical source and malformed constructor-colon evidence.
- Prior parser-parity rounds 304-308 were preserved by rerunning the whole `MLF.Program parser parity` group and the five regression package smokes from the plan.
- `orchestrator/state.json` was already dirty when implementation began and is controller-owned; this implementer did not update it.
- No planned command was skipped or blocked.
