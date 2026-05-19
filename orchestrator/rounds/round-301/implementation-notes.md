### Round 301 Implementation Notes

Role: implementer generated-artifact cleanup retry after second rejected review.

Selection: `item-301-string-equals-native-tracer`.

### Changes Made

- `src/MLF/Primitive/Inventory.hs`: added `__string_equals`, `stringEqualsPrimitiveName`, `PrimitiveNativeStringEquals`, and native-lowerable classification so the primitive inventory remains the owner of trusted primitive metadata.
- `src/MLF/Frontend/Program/Prelude.hs`: exported public Prelude `stringEquals : String -> String -> Bool` backed by `__string_equals`.
- `src/MLF/Frontend/Program/Run.hs`: added interpreter/runtime dispatch for `RuntimeStringEquals` with exact Haskell `String` equality and arity/type diagnostics.
- `src/MLF/Backend/LLVM/Lower.hs`: added raw/native declaration and call lowering for `__string_equals`. First retry computed exact UTF-8 byte lengths for known module string globals. Second retry adds owner-local native string byte-length helpers and a private append-output length registry, makes native `stringAppend` copy both inputs by exact byte length, registers the allocated result length, and makes native `stringEquals` compare by the exact helper length before byte comparison. This fixes `stringEquals (stringAppend "a" "\0b") "a"` without broadening the whole string library.
- `src/MLF/Backend/LLVM/Syntax.hs`: added a narrow mutable LLVM global form needed for the private native string-length registry head.
- `src/MLF/Backend/LLVM/Ppr.hs`: renders the new private mutable LLVM global form.
- `test/BackendLLVMSpec.hs`: extended the focused `stringEquals compares Unicode scalar strings through native execution` test with both embedded-NUL regressions: direct source literal `stringEquals "a\0b" "a"` and runtime-created `stringEquals (stringAppend "a" "\0b") "a"`, each expecting `false\n` through `check-program`, `run-program`, backend LLVM/object validation, native LLVM/object validation, and linked native execution.
- `test/PrimitiveInventorySpec.hs`: added inventory coverage for the new native-lowerable primitive constructor/name mapping.
- `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, `CHANGELOG.md`: documented the narrow exact string equality tracer and the second retry boundary: exact native evidence covers source literals plus native `stringAppend` outputs registered by the append helper; broad exact metadata for every string-producing helper remains out of scope.
- `runtime/mlfp_io/target/release/libmlfp_io.d`: generated depfile churn was restored/excluded from the implementation output.
- `orchestrator/rounds/round-301/implementation-notes.md`: updated with this second retry and exact validation results.
- `runtime/mlfp_io/target/release/libmlfp_io.d`: third retry restored the tracked generated depfile after reviewer validation rewrote it from the repository-root absolute paths to round-worktree absolute paths. No semantic code, tests, docs, review artifacts, roadmap files, or controller state were changed for this cleanup retry.
- `orchestrator/rounds/round-301/implementation-notes.md`: recorded the generated-artifact cleanup retry and final hygiene command results.

### TDD / Retry Evidence

- First retry RED checkpoint after adding the direct embedded-NUL fixture and before changing `Lower.hs`:
  - Command: `cabal test mlf2-test --test-options='--match "stringEquals compares Unicode scalar strings through native execution"'`
  - Result: failed, 1 example, 1 failure.
  - Failure: linked native execution printed `true\n` for `stringEquals "a\0b" "a"` while the expected result was `false\n`.
- First retry GREEN focused checkpoint after adding known-global exact byte lengths:
  - Command: `cabal test mlf2-test --test-options='--match "stringEquals compares Unicode scalar strings through native execution"'`
  - Result: passed, 1 example, 0 failures.
- Second retry RED checkpoint after adding the runtime-created embedded-NUL fixture and before changing native append/output length tracking:
  - Command: `cabal test mlf2-test --test-options='--match "stringEquals compares Unicode scalar strings through native execution"'`
  - Result: failed, 1 example, 1 failure.
  - Failure: linked native execution printed `true\n` for `stringEquals (stringAppend "a" "\0b") "a"` while the expected result was `false\n`.
- Second retry GREEN focused checkpoint after registering exact `stringAppend` output lengths:
  - Command: `cabal test mlf2-test --test-options='--match "stringEquals compares Unicode scalar strings through native execution"'`
  - Result: passed, 1 example, 0 failures.

### Required Validation

- Focused stringEquals matcher:
  - Command: `cabal test mlf2-test --test-options='--match "stringEquals compares Unicode scalar strings through native execution"'`
  - Result: passed, 1 example, 0 failures.
- Primitive inventory matcher:
  - Command: `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  - Result: passed, 1 example, 0 failures.
- Planned neighbor matcher:
  - Command: `cabal test mlf2-test --test-options='--match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringIndexOf indexes Unicode scalar substrings through native execution" --match "stringSplit splits Unicode scalar substrings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringCharAtOption returns optional Unicode scalar cursor lookups through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'`
  - Result: passed, 8 examples, 0 failures.
- Evidence scans:
  - Command: `rg -n -e 'stringEquals : String -> String -> Bool' -e '__string_equals' -e 'stringEqualsPrimitiveName' -e 'PrimitiveNativeStringEquals' -e 'RuntimeStringEquals' -e 'nativeStringByteLengthFunctionName' -e 'nativeStringRegisterLengthFunctionName' src test docs CHANGELOG.md`
  - Result: passed; matches found in production, tests, docs, and changelog.
  - Command: `rg -n -e 'stringEquals compares Unicode scalar strings through native execution' -e 'stringEquals "aλ" "aλ"' -e 'stringEquals "aλ" "a"' -e 'stringEquals "" ""' -e 'stringEquals "a\\0b" "a"' -e 'stringEquals \(stringAppend "a" "\\0b"\) "a"' test/BackendLLVMSpec.hs`
  - Result: passed; focused test name and all five fixture descriptions found.
- Claim audit:
  - Command: `rg -n 'Unicode scalar|stringEquals|string equality|string comparison|collation|ordering|Eq String|case conversion|Unicode normalization|locale|regex|parser parity|platform contract|compiler package|driver|self-boot proof|milestone-3 completion|milestone completion|string-producing helper|stringAppend.*\\0b|embedded-U\+0000' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`
  - Result: passed; reviewed matches preserve the round boundary: exact native evidence is literals plus registered `stringAppend` outputs, while broad exact metadata for every string-producing helper remains out of scope.
- Generated-artifact audit:
  - Command: `git diff --name-only -- '*.d' '*.rlib' 'Cargo.lock' 'dist-newstyle/**' 'target/**'`
  - Result: initially reported `runtime/mlfp_io/target/release/libmlfp_io.d` after native validation; restored that generated depfile. Final rerun passed with empty output.
- Generated-artifact cleanup retry:
  - Command: `git restore -- runtime/mlfp_io/target/release/libmlfp_io.d`
  - Result: passed with empty output; only the tracked generated depfile was restored.
  - Command: `git diff --name-only -- '*.d' '*.rlib' 'Cargo.lock' 'dist-newstyle/**' 'target/**'`
  - Result: passed with empty output.
  - Command: `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d`
  - Result: passed with empty output; no depfile diff remains.
- Whitespace audit:
  - Command: `git diff --check`
  - Result: passed.
- Full build:
  - Command: `CARGO_TARGET_DIR=/tmp/round301-fix2-cargo-target cabal build all`
  - Result: passed.
- Full test:
  - Command: `CARGO_TARGET_DIR=/tmp/round301-fix2-cargo-target cabal test`
  - Result: not run after the second retry because the controller instructed this implementer to stop further validation, update notes, and return.
- Thesis conformance gate:
  - Command: `CARGO_TARGET_DIR=/tmp/round301-fix2-cargo-target ./scripts/thesis-conformance-gate.sh`
  - Result: not run after the second retry because the controller instructed this implementer to stop further validation, update notes, and return.
- Broad validation after generated-artifact cleanup retry:
  - Result: not rerun. The latest reviewer already reran the focused matcher, primitive inventory matcher, neighbor matcher, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh` after the semantic fix. This retry only restored generated depfile churn and reran the required generated-artifact and diff hygiene audits.
- Final status audit:
  - Command: `git status --short --branch`
  - Result:

```text
## orchestrator/round-301-text-substrate-next-slice
 M CHANGELOG.md
 M docs/backend-native-pipeline.md
 M docs/mlfp-language-reference.md
 M docs/mlfp-self-boot-readiness.md
 M orchestrator/state.json
 M src/MLF/Backend/LLVM/Lower.hs
 M src/MLF/Backend/LLVM/Ppr.hs
 M src/MLF/Backend/LLVM/Syntax.hs
 M src/MLF/Frontend/Program/Prelude.hs
 M src/MLF/Frontend/Program/Run.hs
 M src/MLF/Primitive/Inventory.hs
 M test/BackendLLVMSpec.hs
 M test/PrimitiveInventorySpec.hs
?? orchestrator/rounds/round-301/
```

### Closeout Notes

- Prior semantic retries recorded loading `/Users/ares/.agents/skills/tdd/SKILL.md` and `/Users/ares/.agents/skills/haskell-pro/SKILL.md`. This generated-artifact cleanup retry loaded the requested orchestrator role, round, project, and roadmap artifacts, and did not modify Haskell code.
- Did not edit review artifacts or merge.
- Did not edit `orchestrator/state.json`; it remains controller-owned and dirty from the active-round marker.
- Final generated-artifact audit is clean; no `*.d`, `*.rlib`, `Cargo.lock`, `dist-newstyle/**`, or `target/**` paths remain in the implementation diff.
- Cleanup retry did not rerun broad validation because the reviewer already passed it after the semantic fix and the retry target was generated-artifact churn only.
- Runtime-created embedded-NUL reviewer probe is fixed for the in-scope `stringAppend` output path. This retry intentionally does not broaden exact embedded-NUL support to every native string-producing helper.
