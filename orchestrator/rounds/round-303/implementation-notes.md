# Round 303 Implementation Notes

## Changes Made

- Loaded and followed the round-303 implementer contract, active roadmap
  rev-004 bundle, project contract, selection record, round plan record, plan,
  and the TDD/Haskell style guides. `orchestrator/state.json` was already
  modified in the worktree and was left untouched.
- Completed the rev-004 initial Broad String Library matrix as one unified
  round, not one function per round.
- Extended the shared primitive inventory, Prelude surface, interpreter
  runtime, backend primitive validation, LLVM syntax/pretty-printer, and native
  lowerer for the remaining string helpers:
  `stringJoin`, `stringSplitChar`, `stringCompare`,
  `charIsAsciiHexDigit`, `charIsAsciiLineBreak`,
  `charIsAsciiControl`, `charToAsciiLower`, `charToAsciiUpper`,
  `stringToAsciiLower`, and `stringToAsciiUpper`.
- Repaired `stringFromList` as a real native primitive over structural
  `List Char` input rather than a recursive Prelude fallback. The primitive
  inventory now has a precise structural `mu` primitive type, and backend
  validation/conversion accepts that structural argument shape.
- Repaired native `String` metadata and rendering for embedded U+0000. Native
  `stringToList` and the renderer now scan by registered byte length instead
  of treating C NUL as terminator; embedded U+0000 renders as `\NUL`.
- Tightened metadata-light structural recursive matching so recursive self
  fields do not pollute payload-parameter matching, with a focused regression
  test.
- Added broad native backend tests for public API exposure, slicing/cursor
  boundary behavior, search/split/replace/join edges, ASCII classification and
  case helpers, scalar compare, `String`/`List Char` round trips, and exact
  native metadata.
- Updated narrow progress/docs surfaces named by the plan:
  `CHANGELOG.md`, `docs/mlfp-language-reference.md`,
  `docs/backend-native-pipeline.md`, and
  `docs/mlfp-self-boot-readiness.md`.

## TDD Log

- RED: `broad string library exposes the rev-004 public API through native
  execution` initially failed to compile because the partial lowerer used
  `i1Ty` in the native string byte-length helper without binding it.
- GREEN: added the missing binding and reran the focused public API matcher.
- RED: exact metadata coverage exposed a mismatch between `__string_from_list`
  typed as nominal `List Char` and the backend's structural recursive `List`
  lowering.
- GREEN: added `PrimitiveTypeMu`, typed `__string_from_list` with the
  structural list `mu`, taught backend type conversion about primitive `mu`
  types, and added structural-recursive matching coverage.
- RED: exact metadata coverage then showed native rendering stopped at embedded
  U+0000.
- GREEN: changed `stringToList` and native rendering to use registered byte
  lengths; the grouped focused matchers and full gates now pass.

## Tests

- PASS: `CARGO_TARGET_DIR=/tmp/round303-cargo-target cabal test mlf2-test --test-options='--match "broad string library exposes the rev-004 public API through native execution" --match "broad string library fixes slicing and cursor boundary behavior through native execution" --match "broad string library covers search split replace and join edges through native execution" --match "broad string library covers ASCII classification case and scalar compare through native execution" --match "broad string library preserves String List Char round trips and exact native metadata"'`
  - Result: 5 examples, 0 failures.
- PASS: `CARGO_TARGET_DIR=/tmp/round303-cargo-target cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  - Result: 1 example, 0 failures.
- PASS: `CARGO_TARGET_DIR=/tmp/round303-cargo-target cabal test mlf2-test --test-options='--match "stringEquals compares Unicode scalar strings through native execution" --match "stringSplit splits Unicode scalar substrings through native execution" --match "stringReplace replaces Unicode scalar substrings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringToList converts Unicode scalar strings through native execution" --match "stringCharAtOption returns optional Unicode scalar cursor lookups through native execution" --match "charIsAsciiPrintable classifies ASCII printable scalar values through native execution"'`
  - Result: 5 examples, 0 failures. Two matcher strings from the plan are
    stale names in the current test suite.
- PASS: `CARGO_TARGET_DIR=/tmp/round303-cargo-target cabal test mlf2-test --test-options='--match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "charIsAsciiPrintable classifies ASCII printable Char values through native execution"'`
  - Result: 2 examples, 0 failures. This covers the current names for the two
    stale carried-neighbor matcher strings.
- PASS: `CARGO_TARGET_DIR=/tmp/round303-cargo-target cabal test mlf2-test --test-options='--match "matches metadata-light recursive payload parameters without counting self fields"'`
  - Result: 1 example, 0 failures.
- PASS: evidence `rg` checks:
  - `rg -n -e 'stringJoin : String -> List String -> String' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md src/MLF/Frontend/Program/Prelude.hs test/BackendLLVMSpec.hs`
  - `rg -n -e '__string_join' src test docs`
  - `rg -n -e 'embedded U\+0000' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md test/BackendLLVMSpec.hs`
- PASS: claim audit command ran against the plan files. Hits are expected
  out-of-scope/roadmap terminology only; no new docs claim locale, regex,
  parser parity, platform/compiler package, driver/proof completion, Unicode
  normalization/collation/default case, generic `List`, maps/sets,
  filesystem/process IO, ABI/linker completion, or milestone-3 completion.
- PASS: generated-artifact audit:
  `git diff --name-only -- '*.d' '*.rlib' 'Cargo.lock' 'dist-newstyle/**' 'target/**'`
  - Result: no tracked generated artifacts in the diff.
- PASS: `git diff --check`
- PASS: `CARGO_TARGET_DIR=/tmp/round303-cargo-target cabal build all`
- PASS: `CARGO_TARGET_DIR=/tmp/round303-cargo-target cabal test`
  - Result: 2612 examples, 0 failures.
- PASS: `CARGO_TARGET_DIR=/tmp/round303-cargo-target ./scripts/thesis-conformance-gate.sh`
  - Result: thesis obligations, claims, Phi/Omega matrix, A6 regressions,
    theorem obligations, representative baseline, ga' redirect stability, and
    translatable-presolution/Phi/minimality gates passed.

## Docs Claim Audit

- The docs now claim only the rev-004 initial Broad String Library matrix:
  source checking, `run-program`, backend LLVM emission, object generation, and
  linked native execution for the listed string/Char helpers and exact native
  byte-length metadata.
- The docs explicitly keep locale/collation, regex, parser combinators/parser
  parity, platform contracts, compiler package implementation, driver/proof
  completion, Unicode normalization/collation/default case, generic
  collection libraries, maps/sets, filesystem/process IO, ABI, and linker
  policy out of scope.
- `CONTEXT.md` was included in the claim audit but was not edited; existing
  hits are broader roadmap vocabulary, not new round-303 claims.

## Generated-Artifact Audit

- No tracked generated files matched `*.d`, `*.rlib`, `Cargo.lock`,
  `dist-newstyle/**`, or `target/**`.

## Out Of Scope

- Locale behavior, locale collation, regex, parser combinators, parser parity,
  platform contracts, compiler package implementation, driver/proof
  completion, Unicode normalization, Unicode collation, Unicode default case
  conversion, generic `List`/map/set libraries, filesystem/process IO, lock
  files, ABI/linker policy, and milestone-3 completion.

## Remaining Blockers

- None. The round implementation is complete and the required gates are green.
