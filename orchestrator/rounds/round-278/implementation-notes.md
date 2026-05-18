### Changes Made
- Added public Prelude `charIsAsciiLower : Char -> Bool`.
- Added reserved primitive `__char_is_ascii_lower : Char -> Bool` to the shared primitive inventory, interpreter/runtime dispatch, and native-lowerable inventory classification.
- Lowered the native primitive directly over the existing `i32` `Char` representation with ASCII range checks for `a` through `z`.
- Documented the helper as an explicit ASCII lowercase `Char` classifier, not Unicode category or locale/regex support.

### Files Changed
- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`
- `src/MLF/Primitive/Inventory.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/mlfp-language-reference.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-self-boot-readiness.md`
- `CHANGELOG.md`
- `orchestrator/rounds/round-278/implementation-notes.md`

### TDD Evidence
- Loaded `/Users/ares/.agents/skills/tdd/SKILL.md` and followed a vertical RED -> GREEN -> refactor cycle.
- RED command: `cabal test mlf2-test --test-options='--match "charIsAsciiLower classifies ASCII lowercase Char values through native execution"'`
- RED result: failed as expected at the public Prelude boundary: `module \`Prelude\` does not export \`charIsAsciiLower\``.
- GREEN command: `cabal test mlf2-test --test-options='--match "charIsAsciiLower classifies ASCII lowercase Char values through native execution"'`
- GREEN result: passed, 1 example, 0 failures. The focused test checks `charIsAsciiLower 'a'` as `true`, `charIsAsciiLower 'A'` as `false`, and `charIsAsciiLower 'λ'` as `false` through check-program, run-program, emit-backend, object validation, emit-native, native object validation, and linked native execution.

### Verification
- `cabal test mlf2-test --test-options='--match "charIsAsciiLower classifies ASCII lowercase Char values through native execution"'`: PASS, 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`: PASS, 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "charIsDigit classifies decimal Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'`: PASS, 13 examples, 0 failures.
- `rg -n 'charIsAsciiLower : Char -> Bool|__char_is_ascii_lower|charIsAsciiLowerPrimitiveName|PrimitiveNativeCharIsAsciiLower|RuntimeCharIsAsciiLower' src test docs CONTEXT.md README.md CHANGELOG.md`: PASS, expected implementation/docs references found.
- `rg -n 'charIsAsciiLower classifies ASCII lowercase Char values through native execution|charIsAsciiLower '\''a'\''|charIsAsciiLower '\''A'\''|charIsAsciiLower '\''λ'\''|NativeRunResult ExitSuccess "true\\n"|NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs`: PASS, focused test/source snippets found.
- `rg -n 'Unicode scalar|ASCII lowercase|ASCII helper|Char classification|charIsAsciiLower|charIsDigit|stringCharAt|stringSlice|stringTake|stringDrop|stringEndsWith|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|formatting|classification|cursor|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`: PASS, expected docs/changelog claim-audit references found.
- `git diff --check`: PASS before broad validation and again after final notes/depfile cleanup.
- `cabal build all`: PASS.
- `cabal test`: PASS, 2582 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: PASS, thesis conformance anchors are green.
- `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d`: clean after restoring generated depfile path churn.

### Notes
- Scope boundaries stayed limited to public `charIsAsciiLower : Char -> Bool` ASCII lowercase classification for `Char` values through check/run/backend/object/native paths.
- Uppercase helpers, combined alphabetic helpers, identifier continuation, Unicode categories, locale, regex, formatting, `String`/`List Char` conversion, parser parity, platform contracts, compiler package work, and proof records remain out of scope.
- `CONTEXT.md` was used as claim-audit input only and was not edited.
- `orchestrator/state.json` remained controller-owned and was not edited.
- Native validation regenerated `runtime/mlfp_io/target/release/libmlfp_io.d` with a worktree-local path; the depfile was restored to the checked-in parent path before closeout.
- Blockers: none.
