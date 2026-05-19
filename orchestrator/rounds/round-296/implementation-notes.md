# Round 296 Implementation Notes

## Scope

- Selected item: `item-296-string-index-of-char-native-tracer`.
- Target public Prelude operation:
  `stringIndexOfChar : String -> Char -> Option Int`, backed by
  `__string_index_of_char`.
- Scope is limited to first-match Unicode scalar character indexing and absent
  match reporting through `None`. This round does not add substring index APIs,
  substring replacement, splitting, regex, formatting-family completion,
  interpolation, Unicode normalization, locale behavior, case conversion,
  complete cursor APIs, parser parity, platform contracts, compiler package
  work, driver work, proof records, roadmap status edits, controller state
  edits, or semantic roadmap updates.

## RED

- TDD skill: `/Users/ares/.agents/skills/tdd/SKILL.md`.
- Command:
  `cabal test mlf2-test --test-options='--match "stringIndexOfChar indexes Unicode scalar characters through native execution"'`
- Result: failed as expected before production changes.
- Failure evidence:
  `module 'Prelude' does not export 'stringIndexOfChar'`.

## GREEN

- Command:
  `CARGO_TARGET_DIR=/tmp/round296-cargo-target cabal test mlf2-test --test-options='--match "stringIndexOfChar indexes Unicode scalar characters through native execution"'`
- Result: passed, 1 example, 0 failures.
- Evidence covered by the focused test:
  `.mlfp` source checking, `run-program`, `emit-backend`, backend object
  validation, `emit-native`, native object validation, and linked native
  execution for:
  - `stringIndexOfChar "aλbλ" 'λ'` rendering `Some 1`.
  - `stringIndexOfChar "ab" 'λ'` rendering `None`.
- Recovery note: the original implementer became non-observable after writing
  the GREEN implementation. A read-only recovery investigator classified the
  diff as `usable-needs-cleanup`; the controller restored generated depfile
  churn, reran the validations below, and updated this evidence.

## Files Changed

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
- `orchestrator/rounds/round-296/implementation-notes.md`

## Validation

- Focused RED: failed as expected; see RED.
- Focused GREEN:
  `CARGO_TARGET_DIR=/tmp/round296-cargo-target cabal test mlf2-test --test-options='--match "stringIndexOfChar indexes Unicode scalar characters through native execution"'`
  passed, 1 example, 0 failures.
- Primitive inventory matcher:
  `CARGO_TARGET_DIR=/tmp/round296-cargo-target cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  passed, 1 example, 0 failures.
- Neighbor matcher set:
  `CARGO_TARGET_DIR=/tmp/round296-cargo-target cabal test mlf2-test --test-options='--match "stringReplaceChar replaces Unicode scalar characters through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'`
  passed, 8 examples, 0 failures.
- Evidence `rg` checks:
  `rg -n -e 'stringIndexOfChar : String -> Char -> Option Int' -e '__string_index_of_char' -e 'stringIndexOfCharPrimitiveName' -e 'PrimitiveNativeStringIndexOfChar' -e 'RuntimeStringIndexOfChar' src test docs CHANGELOG.md`
  exited 0 with matches in the owned implementation, tests, docs, and
  changelog.
- Focused fixture evidence `rg` check:
  `rg -n -e 'stringIndexOfChar indexes Unicode scalar characters through native execution' -e 'stringIndexOfChar \\"aλbλ\\"' -e 'stringIndexOfChar \\"ab\\"' test/BackendLLVMSpec.hs`
  exited 0 with matches for the public test and both source fixtures.
- Claim-audit `rg` check:
  `rg -n 'Unicode scalar|stringIndexOfChar|Option Int|stringReplaceChar|stringContainsChar|stringContains|stringCharAt|Plain String Search|indexing|replacement|split|regex|formatting|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`
  exited 0 for audit review; new wording stays limited to first-match
  character index search and does not claim substring index APIs, splitting,
  replacement-family completion, regex, formatting-family completion, complete
  cursor APIs, parser parity, platform contracts, self-boot proof, roadmap
  status, or milestone completion.
- `git diff --check`: passed after implementation and after restoring generated
  depfile churn.
- `CARGO_TARGET_DIR=/tmp/round296-cargo-target cabal build all`: passed.
- `CARGO_TARGET_DIR=/tmp/round296-cargo-target cabal test`: passed, 2601
  examples, 0 failures.
- `CARGO_TARGET_DIR=/tmp/round296-cargo-target ./scripts/thesis-conformance-gate.sh`:
  passed.
- Generated validation churn: restored
  `runtime/mlfp_io/target/release/libmlfp_io.d` after validation dirtied it;
  it has no remaining diff.

## Blockers

- None.
