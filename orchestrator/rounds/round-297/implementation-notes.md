# Round 297 Implementation Notes

## Changes Made

- `src/MLF/Primitive/Inventory.hs`: added shared owner metadata for
  `__string_index_of`, including `stringIndexOfPrimitiveName`,
  `PrimitiveNativeStringIndexOf`, type `String -> String -> Option Int`, and
  native-lowerable classification.
- `src/MLF/Frontend/Program/Prelude.hs`: exported public Prelude
  `stringIndexOf` and bound it to `__string_index_of`.
- `src/MLF/Frontend/Program/Run.hs`: added run-program dispatch for
  `__string_index_of`, returning `Some` zero-based Unicode-scalar index for
  the first substring match, `None` for absence, and `Some 0` for the empty
  needle.
- `src/MLF/Backend/LLVM/Lower.hs`: added native lowering for
  `__string_index_of`, wired runtime declarations/emission, and returned the
  existing Prelude `Option Int` heap representation.
- `test/BackendLLVMSpec.hs`: added the public native tracer proving
  `stringIndexOf "aλbcλ" "λb" == Some 1`,
  `stringIndexOf "abc" "λ" == None`, and
  `stringIndexOf "λ" "" == Some 0` through check, run-program, backend
  LLVM/object validation, emit-native/native-object validation, and linked
  native execution.
- `test/PrimitiveInventorySpec.hs`: extended shared primitive inventory
  coverage to require the new primitive name and native-lowerable
  classification.
- `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`,
  `docs/mlfp-self-boot-readiness.md`, `CHANGELOG.md`: documented only the
  first native-capable substring index search tracer and kept splitting,
  substring replacement, regex, formatting completion, complete cursor APIs,
  parser parity, platform contracts, and self-boot proof out of scope.
- `orchestrator/rounds/round-297/implementation-notes.md`: updated stale RED
  notes with GREEN and closeout evidence.

## Tests

- TDD skill used: `/Users/ares/.agents/skills/tdd/SKILL.md`.
- Haskell style guide loaded: `/Users/ares/.agents/skills/haskell-pro/SKILL.md`.
- Focused RED before production changes:
  `cabal test mlf2-test --test-options='--match "stringIndexOf indexes Unicode scalar substrings through native execution"'`
  failed as expected because `Prelude` did not export `stringIndexOf`.
- Focused GREEN:
  `cabal test mlf2-test --test-options='--match "stringIndexOf indexes Unicode scalar substrings through native execution"'`
  passed, 1 example, 0 failures.
- Primitive inventory:
  `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  passed, 1 example, 0 failures.
- Neighbor matcher set:
  `cabal test mlf2-test --test-options='--match "stringIndexOfChar indexes Unicode scalar characters through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringReplaceChar replaces Unicode scalar characters through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'`
  passed, 8 examples, 0 failures.
- Evidence scans:
  `rg -n -e 'stringIndexOf : String -> String -> Option Int' -e '__string_index_of' -e 'stringIndexOfPrimitiveName' -e 'PrimitiveNativeStringIndexOf' -e 'RuntimeStringIndexOf' src test docs CHANGELOG.md`
  passed.
- Fixture evidence scan:
  `rg -n -e 'stringIndexOf indexes Unicode scalar substrings through native execution' -e 'stringIndexOf \"aλbcλ\" \"λb\"' -e 'stringIndexOf \"abc\" \"λ\"' -e 'stringIndexOf \"λ\" \"\"' test/BackendLLVMSpec.hs`
  passed.
- Claim audit:
  `rg -n 'Unicode scalar|stringIndexOf|stringIndexOfChar|Option Int|stringContains|stringReplaceChar|stringCharAt|Plain String Search|substring index|indexing|replacement|split|regex|formatting|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`
  passed; docs keep non-goals scoped as open/out-of-scope.
- `git diff --check` passed.
- `CARGO_TARGET_DIR=/tmp/round297-cargo-target cabal build all` passed.
- `CARGO_TARGET_DIR=/tmp/round297-cargo-target cabal test` passed,
  2602 examples, 0 failures.
- `CARGO_TARGET_DIR=/tmp/round297-cargo-target ./scripts/thesis-conformance-gate.sh`
  passed with final `PASS: thesis conformance anchors are green`.

## Notes

- The implementation stays within
  `item-297-string-index-of-native-tracer`; it does not add splitting,
  substring replacement, replace-all substring APIs, regex, formatting-family
  completion, Unicode normalization, locale behavior, case conversion, parser
  parity, platform contracts, proof records, roadmap status edits, controller
  state edits, or semantic roadmap updates.
- `orchestrator/state.json` and
  `runtime/mlfp_io/target/release/libmlfp_io.d` were restored to HEAD after
  accidental/generated churn and are not part of the implementation diff.
