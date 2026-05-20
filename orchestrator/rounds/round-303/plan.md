### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-302-broad-string-library-completion`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Complete the rev-004 initial native-capable Broad String Library matrix as one
integrated round. This round adds `stringJoin`, `stringSplitChar`,
`stringCompare`, `charIsAsciiHexDigit`, `charIsAsciiLineBreak`,
`charIsAsciiControl`, `charToAsciiLower`, `charToAsciiUpper`,
`stringToAsciiLower`, and `stringToAsciiUpper`; proves explicit boundary
behavior for existing slicing/cursor helpers; proves `String`/`List Char`
round trips; hardens carried search/split/replace edge cases; and records exact
native metadata evidence for every string-producing helper used by the matrix.

Round classification: behavior-changing implementation. The implementer must
load and follow `/Users/ares/.agents/skills/tdd/SKILL.md`, use grouped vertical
RED -> GREEN -> refactor cycles, and keep the selected item unified. Do not
turn this into future one-function string rounds.

### Approach
Keep the round serial. Use `worker_mode: none`.

Work through the matrix in grouped public-interface slices. Each slice starts
by adding one focused Hspec example or table under the existing native text
tests, proving the RED failure, implementing only enough source/runtime/backend
support for that group, then refactoring while that focused group stays green.
Every focused test must exercise ordinary `.mlfp` source through source
checking, `run-program`, backend LLVM/object validation, `emit-native`/native
object validation, and linked native execution when the behavior is executable.

Expected write scope:

- Public behavior tests: `test/BackendLLVMSpec.hs`.
- Primitive ownership and native-lowerability coverage:
  `src/MLF/Primitive/Inventory.hs` and `test/PrimitiveInventorySpec.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive dispatch: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native lowering and exact string metadata handling:
  `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow docs/progress updates after behavior passes:
  `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`,
  `docs/mlfp-self-boot-readiness.md`, and `CHANGELOG.md`.
- Claim-audit input only unless claims are stale: `CONTEXT.md`.
- Round evidence after implementation:
  `orchestrator/rounds/round-303/implementation-notes.md`.

If a new test module is introduced unexpectedly, register it in both
`mlf2.cabal` and `test/Main.hs`; otherwise keep the work inside the existing
test modules above.

Planned public primitive names should follow the existing pattern:
`__string_join`, `__string_split_char`, `__string_compare`,
`__char_is_ascii_hex_digit`, `__char_is_ascii_line_break`,
`__char_is_ascii_control`, `__char_to_ascii_lower`,
`__char_to_ascii_upper`, `__string_to_ascii_lower`, and
`__string_to_ascii_upper`. The shared primitive inventory remains the owner for
primitive names, types, and native-lowerability classification.

### Steps
1. Confirm the round lineage in `selection-record.json`: rev-004,
   `milestone-3`, `direction-3a-broad-string-char-substrate`, and
   `item-302-broad-string-library-completion`. Do not edit
   `orchestrator/state.json`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and
   `/Users/ares/.agents/skills/haskell-pro/SKILL.md`. Record the TDD path and
   every RED/GREEN/refactor command in `implementation-notes.md`.
3. RED cycle 1, public API/import group: add a focused test named
   `broad string library exposes the rev-004 public API through native execution`.
   It should import all new public names from Prelude in ordinary `.mlfp`
   modules and include at least one executable use of `stringJoin`,
   `stringSplitChar`, `stringCompare`, ASCII classification, ASCII case, and
   string ASCII case helpers. Expected RED is missing public Prelude names or
   missing primitive support.
4. GREEN/refactor cycle 1: add the new public Prelude exports/bindings and
   primitive inventory entries with the correct types and native support. Keep
   docs untouched until behavior is actually green.
5. RED cycle 2, boundary group: add focused coverage named
   `broad string library fixes slicing and cursor boundary behavior through native execution`.
   Cover `stringDrop`, `stringTake`, `stringSlice`, and
   `stringCharAtOption` for negative offsets/counts, zero, exact end, beyond
   end, empty input, non-ASCII scalar input, and embedded U+0000 where
   applicable. Assert rev-004 semantics: negative drop/take clamp to zero;
   negative slice start clamps to zero; non-positive slice count returns `""`;
   overlarge drop returns `""`; overlarge take returns the whole string;
   overlarge slice returns the remaining suffix or `""`; negative or
   out-of-range `stringCharAtOption` returns `None`. `stringCharAt` remains
   in-range only.
6. GREEN/refactor cycle 2: adjust interpreter and native slicing/cursor paths
   only where tests expose a mismatch. Preserve Unicode scalar indexing and
   avoid byte-index or C-string assumptions.
7. RED cycle 3, search/split/replace/join group: add focused coverage named
   `broad string library covers search split replace and join edges through native execution`.
   Cover `stringJoin` for `Nil`, singleton, and multi-element separator
   insertion; `stringSplitChar` for Unicode scalar delimiters and
   leading/trailing empty segment preservation; carried `stringContainsChar`,
   `stringContains`, `stringStartsWith`, `stringEndsWith`,
   `stringIndexOfChar`, `stringIndexOf`, `stringReplaceChar`,
   `stringReplace`, and `stringSplit` edges for empty needles/delimiters,
   no-match cases, repeated matches, non-overlapping replacement, non-ASCII
   scalars, and embedded U+0000 metadata preservation.
8. GREEN/refactor cycle 3: implement `stringJoin` and `stringSplitChar` across
   runtime and native paths, then make only necessary carried-edge fixes.
   `stringJoin` is the only new `List String` helper in scope.
9. RED cycle 4, classification/case/compare group: add focused coverage named
   `broad string library covers ASCII classification case and scalar compare through native execution`.
   Cover `charIsAsciiHexDigit`, `charIsAsciiLineBreak`, and
   `charIsAsciiControl`; `charToAsciiLower`/`charToAsciiUpper`;
   `stringToAsciiLower`/`stringToAsciiUpper`; and `stringCompare`.
   `stringCompare` returns exactly `-1`, `0`, or `1` for lexicographic Unicode
   scalar value ordering. ASCII case helpers change only ASCII letters and
   preserve all other Unicode scalar values unchanged.
10. GREEN/refactor cycle 4: implement classification, ASCII case conversion,
    string ASCII case conversion, and deterministic scalar comparison in
    runtime and native paths. Do not claim locale collation, Unicode Collation
    Algorithm behavior, Unicode normalization, or Unicode default case mapping.
11. RED cycle 5, `String`/`List Char` and exact metadata group: add focused
    coverage named `broad string library preserves String List Char round trips and exact native metadata`.
    Prove `stringFromList`/`stringToList` round trips for empty, ASCII,
    non-ASCII, and embedded U+0000 scalar sequences. Prove strings produced by
    source literals and by native string-producing helpers keep exact length
    and embedded U+0000 metadata when consumed by `stringEquals`,
    `stringLength`, search, split/replace, rendering, and linked native
    execution without C-string truncation.
12. GREEN/refactor cycle 5: repair shared string allocation/metadata paths if
    any helper still loses length, embedded U+0000, or non-ASCII scalar
    metadata. Keep ownership in the existing runtime/backend string path; do
    not add a separate platform ABI or linker contract.
13. Update only narrow docs and `CHANGELOG.md` after behavior is green. Docs
    must describe the completed rev-004 initial Broad String Library matrix by
    layer and must not claim parser parity, platform contracts, compiler
    package work, driver work, proof work, Unicode normalization, Unicode
    collation, Unicode default case mapping, locale-sensitive APIs, regex,
    generic `List` library work, maps/sets, filesystem/process IO, locks,
    ABI/linker work, or proof records.
14. Run regression and closeout validation. Record in
    `implementation-notes.md`: files changed, RED/GREEN/refactor evidence for
    each group, source/interpreter/backend/object/native evidence, primitive
    inventory evidence, exact native metadata evidence, docs claim audit,
    generated-artifact audit, and the explicit out-of-scope list.

### Verification
Focused RED/GREEN commands:

```bash
cabal test mlf2-test --test-options='--match "broad string library exposes the rev-004 public API through native execution"'
```

```bash
cabal test mlf2-test --test-options='--match "broad string library fixes slicing and cursor boundary behavior through native execution"'
```

```bash
cabal test mlf2-test --test-options='--match "broad string library covers search split replace and join edges through native execution"'
```

```bash
cabal test mlf2-test --test-options='--match "broad string library covers ASCII classification case and scalar compare through native execution"'
```

```bash
cabal test mlf2-test --test-options='--match "broad string library preserves String List Char round trips and exact native metadata"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required carried-neighbor regression checks:

```bash
cabal test mlf2-test --test-options='--match "stringEquals compares Unicode scalar strings through native execution" --match "stringSplit splits Unicode scalar substrings through native execution" --match "stringReplace replaces Unicode scalar substrings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringToList converts Unicode scalar strings through native execution" --match "stringCharAtOption returns optional Unicode scalar cursor lookups through native execution" --match "charIsAsciiPrintable classifies ASCII printable scalar values through native execution"'
```

Required evidence checks:

```bash
rg -n -e 'stringJoin : String -> List String -> String' -e 'stringSplitChar : String -> Char -> List String' -e 'stringCompare : String -> String -> Int' -e 'charIsAsciiHexDigit : Char -> Bool' -e 'charIsAsciiLineBreak : Char -> Bool' -e 'charIsAsciiControl : Char -> Bool' -e 'charToAsciiLower : Char -> Char' -e 'charToAsciiUpper : Char -> Char' -e 'stringToAsciiLower : String -> String' -e 'stringToAsciiUpper : String -> String' src test docs CHANGELOG.md
```

```bash
rg -n -e '__string_join' -e '__string_split_char' -e '__string_compare' -e '__char_is_ascii_hex_digit' -e '__char_is_ascii_line_break' -e '__char_is_ascii_control' -e '__char_to_ascii_lower' -e '__char_to_ascii_upper' -e '__string_to_ascii_lower' -e '__string_to_ascii_upper' src test
```

```bash
rg -n -e 'embedded U\\+0000' -e 'C-string truncation' -e 'String/List Char' -e 'stringJoin sep Nil' -e 'stringCompare' test docs CHANGELOG.md orchestrator/rounds/round-303/implementation-notes.md
```

Required claim audit:

```bash
rg -n 'locale|regex|parser combinator|parser parity|platform contract|compiler package|driver|proof|Unicode normalization|Unicode Collation|Unicode default case|generic List|maps|sets|filesystem|process IO|lock|ABI|linker|proof record|milestone-3 completion|milestone 3 complete' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md orchestrator/rounds/round-303/implementation-notes.md
```

Required generated-artifact audit:

```bash
git diff --name-only -- '*.d' '*.rlib' 'Cargo.lock' 'dist-newstyle/**' 'target/**'
```

Expected result: no generated artifact churn remains in the final diff. If
native validation rewrites `runtime/mlfp_io/target/release/libmlfp_io.d`,
restore or otherwise remove that generated depfile churn before review.

Required closeout checks:

```bash
git diff --check
```

```bash
CARGO_TARGET_DIR=/tmp/round303-cargo-target cabal build all
```

```bash
CARGO_TARGET_DIR=/tmp/round303-cargo-target cabal test
```

```bash
CARGO_TARGET_DIR=/tmp/round303-cargo-target ./scripts/thesis-conformance-gate.sh
```

### Review Acceptance Criteria
- `selection-record.json` and `round-plan-record.json` lineage matches active
  rev-004 and `item-302-broad-string-library-completion`.
- The implementation covers every included row in the rev-004 matrix:
  `stringJoin`, `stringSplitChar`, `stringCompare`, slicing/cursor boundaries,
  `String`/`List Char` round trips, carried search/split/replace edges,
  ASCII classification and case helpers, and exact native metadata for
  string-producing helpers.
- Each grouped behavior has public `.mlfp` evidence through source checking,
  `run-program`, backend LLVM/object validation, `emit-native`/native-object
  validation, and linked native execution where applicable.
- Primitive names, types, and native-lowerability are owned by
  `MLF.Primitive.Inventory`.
- `stringCompare` is deterministic Unicode scalar ordering returning `-1`,
  `0`, or `1`; it does not claim locale collation or Unicode Collation
  Algorithm behavior.
- ASCII case helpers modify only ASCII letters and preserve other Unicode
  scalar values unchanged; they do not claim Unicode default case mapping.
- Exact scalar-sequence behavior remains explicit: no Unicode normalization is
  added, and canonically equivalent but differently encoded strings remain
  distinct.
- No generic `List` library, parser-owned cursor/combinator module, maps/sets,
  filesystem/process IO, package lock, ABI/linker, platform, compiler package,
  driver, or proof scope is introduced.
- Closeout evidence includes all focused grouped matchers, primitive inventory,
  carried-neighbor regression checks, `git diff --check`, `cabal build all`,
  `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Round Plan Record
Also see `orchestrator/rounds/round-303/selection-record.json` and
`orchestrator/rounds/round-303/round-plan-record.json`.
