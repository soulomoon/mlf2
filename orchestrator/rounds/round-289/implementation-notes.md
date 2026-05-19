# Round 289 Implementation Notes

## Scope

- Selected item: `item-289-string-from-list-native-tracer`.
- Added public Prelude `stringFromList : List Char -> String`.
- Preferred high-level route was kept: the Prelude definition recursively
  pattern matches `List Char` and composes existing `stringFromChar` and
  `stringAppend`.
- No new trusted text primitive was added. The primitive absence audit found no
  `__string_from_list`, `PrimitiveNativeStringFromList`, or
  `RuntimeStringFromList`.
- Scope boundaries preserved: no reverse `String -> List Char`, formatting
  completion, parser parity, platform contracts, roadmap status, or
  controller-state edits.

## TDD Evidence

- TDD skill loaded: `/Users/ares/.agents/skills/tdd/SKILL.md`.
- Focused RED command:
  `cabal test mlf2-test --test-options='--match "stringFromList converts List Char values to Unicode scalar strings through native execution"'`
- RED result: expected failure before production changes, `1 example, 1 failure`.
  The source checker rejected the import because `Prelude` did not export
  `stringFromList`.
- Focused GREEN command:
  `cabal test mlf2-test --test-options='--match "stringFromList converts List Char values to Unicode scalar strings through native execution"'`
- GREEN result: PASS, `1 example, 0 failures`.

## Implementation Notes

- Initial high-level Prelude definition exposed a `run-program` runtime gap:
  direct top-level recursion failed with
  `run-program IO runtime encountered recursive top-level binding lookup:
  Prelude__stringFromList -> Prelude__stringFromList`.
- A local recursive helper variant failed in `run-program` with an unknown local
  helper value.
- A temporary primitive route was tested and abandoned because the backend type
  shape for `List Char` did not match the recursive structural backend list
  shape. Those primitive/inventory/backend changes were removed.
- Final GREEN keeps `stringFromList` as high-level Prelude source and adds only
  a narrow `run-program` handler for the public Prelude binding
  `Prelude__stringFromList`, so interpreter evidence can execute the same public
  behavior without introducing `__string_from_list` or a primitive inventory row.
- Backend/native execution uses the high-level Prelude definition lowered
  through existing `List`, `stringFromChar`, and `stringAppend` support.

## Files Changed

- `test/BackendLLVMSpec.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `docs/mlfp-language-reference.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-self-boot-readiness.md`
- `CHANGELOG.md`
- `orchestrator/rounds/round-289/implementation-notes.md`

`orchestrator/state.json` was already controller-owned dirty state and was not
edited for this implementation.

## Validation

- Focused GREEN:
  `cabal test mlf2-test --test-options='--match "stringFromList converts List Char values to Unicode scalar strings through native execution"'`
  passed, `1 example, 0 failures`.
- Neighbor matcher set:
  `cabal test mlf2-test --test-options='--match "stringFromChar converts Unicode scalar Chars to singleton strings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'`
  passed, `10 examples, 0 failures`.
- Evidence check:
  `rg -n -e 'stringFromList : List Char -> String' -e 'def stringFromList' -e 'stringFromList converts List Char values to Unicode scalar strings through native execution' -e 'stringFromList (Cons' src/MLF/Frontend/Program/Prelude.hs test/BackendLLVMSpec.hs docs CHANGELOG.md`
  passed with matches in Prelude, test, docs, and changelog.
- Scope audit:
  `rg -n 'Unicode scalar|stringFromList|String/List Char|List Char -> String|String -> List Char|formatting|classification|cursor|parser parity|platform contract|self-boot proof' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`
  passed; matching docs preserve the round boundaries and only claim the
  `List Char -> String` direction.
- Primitive absence audit:
  `rg -n '__string_from_list|PrimitiveNativeStringFromList|RuntimeStringFromList' src test`
  returned no matches, as expected.
- `git diff --check` passed.
- `cabal build all` passed.
- `CARGO_TARGET_DIR=/tmp/round289-cargo-target cabal test` passed,
  `2593 examples, 0 failures`.
- `CARGO_TARGET_DIR=/tmp/round289-cargo-target ./scripts/thesis-conformance-gate.sh`
  passed with `PASS: thesis conformance anchors are green`.
- Generated depfile churn in `runtime/mlfp_io/target/release/libmlfp_io.d` was
  restored; final tracked diff has no depfile changes.

## Blockers

- No blocker.
