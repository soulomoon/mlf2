### Changes Made

- Extended the shared parser-owned source-text parser library for the bounded
  `recursive-adt-plain-nat` parser-parity fixture.
- Added the committed canonical source/projection fixture for
  `module NatPlain export (Nat(..), isZero, peel, main)` with recursive
  `Nat`, `isZero`, `peel`, and nested `main` application syntax.
- Added a thin package-root fixture under
  `test/programs/compiler-parser-parity/recursive-adt-plain-nat/` that exposes
  only `sourceFile` and `sourceText` before calling the shared parser library.
- Added a malformed recursive-ADT plain Nat negative path through the generated
  aggregate public CLI driver, expecting parser-derived
  `expected-case-branch-arrow@...` evidence.
- Extended parser shortcut/static guards to reject fixture-owned token streams,
  exact module/program success keys, whole-source recognition, pre-rendered
  `isZero`/`peel`/`main` projection rows, and static negative evidence for the
  new fixture.
- Updated `CHANGELOG.md`, root `implementation_notes.md`,
  `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`
  with bounded parser-parity-only scope.

### Tests

- Focused RED before parser implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive ADT plain Nat/"'`
  failed as expected with 1 example, 1 failure after the tracer/expected fixture
  existed but before parser support; canonical projection matched the oracle and
  shared parser output was `Right "parser-error\n"`.
- Focused GREEN after parser implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive ADT plain Nat/"'`
  passed, 1 example, 0 failures, finished in 185.6102 seconds.
- New malformed recursive-ADT plain Nat diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed recursive ADT plain Nat diagnostics through public run-program/"'`
  passed, 1 example, 0 failures, finished in 328.7428 seconds.
- Parser-library shortcut guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
  passed, 1 example, 0 failures, finished in 0.7039 seconds.
- Static negative-evidence guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics/"'`
  passed, 1 example, 0 failures, finished in 0.0715 seconds.
- New-fixture shortcut audit:
  `rg -n 'parseRecursiveAdtPlainNat|parsePlainRecursiveNat|completeModuleKey "recursive-adt-plain-nat"|moduleKey "recursive-adt-plain-nat"|programKey "recursive-adt-plain-nat"|RecursiveAdtPlainNatTokens|PlainRecursiveNatTokens|LexerOk recursiveAdtPlainNatTokens|LexerOk plainRecursiveNatTokens|recursive-adt-plain-nat tokens|plain-recursive-nat tokens|stringIndexOf sourceText "module NatPlain export"|stringIndexOf "module NatPlain export" sourceText|defRows sourceFile "isZero"|defRows sourceFile "peel"|defRows sourceFile "main"|def isZero type=Nat -> Bool expr=λ\(n : Nat\) case n of|def peel type=Nat -> Nat expr=λ\(n : Nat\) case n of|def main type=Bool expr=isZero \(peel \(Succ Zero\)\)|recursive-adt-plain-nat parser negative expected-case-branch-arrow@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  returned no matches.
- Optional standalone new-fixture smoke/diff:
  `actual=$(mktemp); timeout 900 cabal -v0 run mlf2 -- run-program test/programs/compiler-parser-parity/recursive-adt-plain-nat --search-path test/programs/compiler-parser-parity/parser-library > "$actual"; rc=$?; if [ $rc -eq 0 ]; then diff -u test/conformance/mlfp/parser-parity/recursive-adt-plain-nat/expected/parser-program.txt "$actual"; rc=$?; fi; rm -f "$actual"; exit $rc`
  passed with no diff.
- Shared-context aggregate parser-parity batch:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/runs all .mlfp parser parity fixtures through one generated public CLI driver/"'`
  passed, 1 example, 0 failures, finished in 329.4722 seconds.
- Full parser-parity group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  passed, 27 examples, 0 failures, finished in 1983.6095 seconds.
- Whitespace check:
  `git diff --check`
  passed.
- Full build:
  `timeout 3600 cabal build all`
  passed.
- Full test suite:
  `timeout 7200 cabal test`
  passed, 2674 examples, 0 failures, finished in 2323.7440 seconds.
- Thesis conformance gate:
  `timeout 7200 ./scripts/thesis-conformance-gate.sh`
  passed with `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes

- Scope stayed parser-parity/library only. No checker, resolver, backend,
  platform, driver, proof, package-manager, full parser-parity, or self-boot
  behavior was added.
- The broad parser-parity/checker-like validation used the generated aggregate
  public CLI driver with labelled per-case sections, preserving rev-005
  shared-context run discipline.
- One initial standalone smoke/diff attempt used unquiet `cabal run`, which
  captured Cabal build chatter in stdout and hit zsh's read-only `status`
  variable; the quiet rerun above is the valid standalone smoke/diff result.
- Validation rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` with
  worktree-local absolute paths; that generated path churn was restored and is
  no longer present in `git status`.
- No blockers remain and no required validation command was left unrun.
