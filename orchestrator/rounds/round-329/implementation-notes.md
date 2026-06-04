### Changes Made
- `test/ProgramParserParitySpec.hs`: added the `recursive-list-tail` positive parser-parity matcher, registered `positive:recursive-list-tail` in the generated aggregate public CLI driver, added the malformed recursive-list negative case, and extended round-329 shortcut/static guards.
- `test/conformance/mlfp/parser-parity/recursive-list-tail/src/Main.mlfp`: added the canonical recursive-list tail source fixture with `Nat`, `List`, `tailOrNil`, `isNil`, and nested `isNil (tailOrNil (Cons Zero Nil))`.
- `test/conformance/mlfp/parser-parity/recursive-list-tail/expected/parser-program.txt`: added the committed Haskell canonical parser projection for module/export/data/constructor/definition spans.
- `test/programs/compiler-parser-parity/recursive-list-tail/Main.mlfp`: added a thin public parser-parity harness that calls `renderParserParityProjectionFromSourceText`.
- `test/programs/compiler-parser-parity/recursive-list-tail/ParserParityFixture.mlfp`: added only `sourceFile` and `sourceText` for the new fixture root.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: extended shared parser-owned combinators for two data declarations plus three definitions, two-constructor data rows, two-field constructor type rendering, two-argument constructor case patterns, and nested simple applications.
- `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, `test/conformance/mlfp/README.md`: updated bounded parser-parity progress wording without claiming checker/resolver/backend/platform/driver/proof/full-parser-parity/self-boot progress.
- `runtime/mlfp_io/target/release/libmlfp_io.d`: restored generated absolute-path churn; it is no longer modified in the working tree.

### Tests
- Focused RED: not reproducible in this recovery pass without reverting salvageable draft work. The recovered worktree already contained both the focused matcher and parser-library implementation, and the prior implementer left no `implementation-notes.md` or exact RED transcript.
- Focused GREEN: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive list tail/"'` passed on rerun: 1 example, 0 failures, 216.0760s.
- Malformed recursive-list diagnostic: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed recursive list tail diagnostics through public run-program/"'` passed: 1 example, 0 failures, 388.9315s.
- Parser-library shortcut/static guard: `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'` passed: 1 example, 0 failures, 0.7901s.
- Static negative-evidence guard: `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics/"'` passed: 1 example, 0 failures, 0.0839s.
- Shared-context aggregate parser-parity batch: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/runs all .mlfp parser parity fixtures through one generated public CLI driver/"'` passed: 1 example, 0 failures, 402.1352s.
- Full parser-parity group: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'` passed: 29 examples, 0 failures, 2521.5344s.
- Standalone new-fixture smoke/diff: `actual=$(mktemp); timeout 900 cabal -v0 run mlf2 -- run-program test/programs/compiler-parser-parity/recursive-list-tail --search-path test/programs/compiler-parser-parity/parser-library > "$actual"; diff -u test/conformance/mlfp/parser-parity/recursive-list-tail/expected/parser-program.txt "$actual"; rm -f "$actual"` passed with no diff output.
- New-fixture shortcut audit: `rg -n 'parseRecursiveListTail|completeModuleKey "recursive-list-tail"|moduleKey "recursive-list-tail"|programKey "recursive-list-tail"|RecursiveListTailTokens|LexerOk recursiveListTailTokens|recursive-list-tail tokens|stringIndexOf sourceText "module RecursiveList export"|stringIndexOf "module RecursiveList export" sourceText|defRows sourceFile "tailOrNil"|defRows sourceFile "isNil"|defRows sourceFile "main"|dataRows sourceFile "List"|constructorRows sourceFile "Cons"|def tailOrNil type=List -> List expr=λ\(xs : List\) case xs of|def isNil type=List -> Bool expr=λ\(xs : List\) case xs of|def main type=Bool expr=isNil \(tailOrNil \(Cons Zero Nil\)\)|recursive-list-tail parser negative expected-case-branch-arrow@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs` produced no matches.
- Diff hygiene: `git diff --check` passed.
- Build gate: `cabal build all` passed.
- Full test gate: `cabal test` passed: 2676 examples, 0 failures, 2704.2735s.
- Thesis gate: `./scripts/thesis-conformance-gate.sh` passed with `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes
- Scope stayed parser-parity/library only. No checker, resolver, backend, platform, driver, proof, package-manager, full-parser-parity, or self-boot behavior was added.
- The generated aggregate parser-parity batch now includes `positive:recursive-list-tail`; broad public parser-parity evidence stayed in one shared-context `run-program` driver with labelled sections.
- The first focused GREEN attempt was started in parallel with the shortcut guard and failed with Cabal log-file contention: `removeLink: does not exist`. It was rerun serially and passed. Subsequent Cabal validations were run serially.
- The missing focused RED transcript is a recovery limitation from the prior implementer losing observability before writing notes. I did not revert the salvaged draft solely to manufacture RED evidence.
