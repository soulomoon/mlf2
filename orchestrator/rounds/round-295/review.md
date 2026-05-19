# Round 295 Review

Decision: approved.

## Scope Review

- Lineage matches active rev-003, `milestone-3`, `direction-3a-broad-string-char-substrate`, and `item-295-string-replace-char-native-tracer`.
- The implementation adds public Prelude `stringReplaceChar : String -> Char -> Char -> String` backed by inventory-owned `__string_replace_char`.
- Runtime, primitive inventory, Prelude exposure, backend/native declaration and lowering, focused tests, docs, and changelog are limited to character replacement over Unicode scalar `Char` values.
- The backend lowerer allocates `4 * scalarCount + 1` bytes before copying replacement bytes, which is sufficient for replacing each Unicode scalar with any valid Unicode scalar. It decodes source UTF-8 scalar bytes for comparison, copies unmatched source scalar bytes unchanged, and copies replacement bytes from `__string_from_char`.
- Docs and changelog stay bounded. They do not claim substring replacement, splitting, regex, formatting-family completion, case conversion, Unicode normalization, locale behavior, parser parity, platform ABI guarantees, self-boot proof, or milestone completion.
- `runtime/mlfp_io/target/release/libmlfp_io.d` is not dirty. `orchestrator/state.json` differs only by the controller-owned active round review entry.

## Validations

- `git diff --check` passed.
- `CARGO_TARGET_DIR=/tmp/round295-review-cargo-target cabal test mlf2-test --test-options='--match "stringReplaceChar replaces Unicode scalar characters through native execution"'` passed, 1 example, 0 failures.
- `CARGO_TARGET_DIR=/tmp/round295-review-cargo-target cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'` passed, 1 example, 0 failures.
- `CARGO_TARGET_DIR=/tmp/round295-review-cargo-target cabal test mlf2-test --test-options='--match "stringContainsChar searches Unicode scalars through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringFromUnit formats Unit as a string through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively"'` passed, 8 examples, 0 failures.
- Required evidence `rg` checks passed. The literal fixture audit from the plan matched the Hspec name; the escaped Haskell-string equivalent also matched both `stringReplaceChar "aλbλ"` and `stringReplaceChar "ab"` fixtures.
- Claim-audit `rg` check passed; reviewed matches for bounded docs/changelog wording.
- `git diff master -- runtime/mlfp_io/target/release/libmlfp_io.d` was empty.
- `git diff master -- orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap.md orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap-view.json orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/verification.md` was empty.
- Extra reviewer smoke: a temporary program `stringReplaceChar "abab" 'a' 'λ'` passed `run-program`; after `emit-native`, `llc`, and linking with `runtime/mlfp_io/target/release/libmlfp_io.a`, linked native execution printed `"\\955b\\955b"`.
- Reused implementation-notes broad evidence after focused reruns and diff audit: `cabal build all` passed, `CARGO_TARGET_DIR=/tmp/round295-cargo-target cabal test` passed with 2600 examples and 0 failures, and `CARGO_TARGET_DIR=/tmp/round295-cargo-target ./scripts/thesis-conformance-gate.sh` passed.

## Findings

No blocking findings.
