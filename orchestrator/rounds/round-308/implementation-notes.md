### Changes Made
- `test/ProgramParserParitySpec.hs`: added the round-308 public parser-parity matcher, malformed annotation run-program matcher, typed-annotation fixture/package paths, negative evidence package writer, and fixture-scoped rendering for `STVar`, `STArrow`, `STForall`, typed `ELet`, annotated lambda parameters, and `EAnn`.
- `test/conformance/mlfp/parser-parity/typed-annotation-types/src/Main.mlfp`: added the canonical parser fixture for `import Prelude exposing (Int);` plus `def main : Int = let id : ∀a. a -> a = λ(x : Int) x in (id 1 : Int);`.
- `test/conformance/mlfp/parser-parity/typed-annotation-types/expected/parser-program.txt`: added the committed parser-program projection with module/export/import spans, the `Int` exposing span, typed let annotation rendering, annotated lambda parameter rendering, expression annotation rendering, and arrow/forall source type rendering.
- `test/programs/compiler-parser-parity/typed-annotation-types/`: added the parser-owned `.mlfp` package (`Main.mlfp`, `ParserParitySource.mlfp`, `ParserParityToken.mlfp`, `ParserParityAst.mlfp`, `ParserParityParser.mlfp`) for exactly the selected typed-annotation grammar family plus malformed let-annotation evidence.
- `test/conformance/mlfp/README.md`: carried forward the round-307 let/lambda/application fixture and added only the new typed-annotation parser-projection fixture to the bounded parser-parity list.
- `docs/mlfp-self-boot-readiness.md`, `CHANGELOG.md`, `implementation_notes.md`: updated progress/readiness notes for the bounded parser-only tracer without claiming checker, backend, driver, platform, proof, parser-combinator, full parser parity, or self-boot completion.
- `runtime/mlfp_io/target/release/libmlfp_io.d`: restored generated absolute-path churn; there is no final diff for this file.

### Tests
- RED evidence: recoverable prior note in root `implementation_notes.md` says the focused typed-annotation matcher failed before the fixture/package existed. I did not find exact pre-implementation terminal output in the worktree.
- `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for typed let, annotated lambda, and expression annotations/"'`: passed, 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed annotation syntax through public run-program/"'`: passed, 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`: passed, 10 examples, 0 failures.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types`: passed and printed the committed typed-annotation parser projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool`: passed and printed the carried basic Bool projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool`: passed and printed the carried import-exposing Bool projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref`: passed and printed the carried value-definition-list Int/reference projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application`: passed and printed the carried let/lambda/application projection.
- `git diff --check`: passed with no output.
- `cabal build all && cabal test`: passed; `mlf2-test` reported 2622 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: passed; final line reported `PASS: thesis conformance anchors are green`.

### Notes
- Scope stayed inside `item-308-parser-parity-typed-annotation-types` and milestone 4 parser parity. The `.mlfp` package remains fixture-scoped and parser-owned; no Prelude, public facade, production parser API, checker, backend, package manager, driver, platform, proof, or parser-combinator scope was added.
- Prior parser-parity rounds 304-307 were preserved by rerunning the whole `MLF.Program parser parity` group and the four regression package smokes from the plan.
- `orchestrator/state.json` was already dirty when I started and is controller-owned; I did not edit it.
- No planned command was skipped or blocked.
