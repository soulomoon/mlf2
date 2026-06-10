### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-353-parser-value-source-span-extraction-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build a reusable parser-library source-span and parser-value extraction
substrate, then migrate the existing local token/text/coordinate helpers in
`ParserParityParser.mlfp` onto it.

The round should reduce repeated `ParserValue` case plumbing around source
coordinates, token text, rendered parser text, and token-bound spans while
preserving the exact fallback behavior and aggregate parser-parity outputs
already proved by the compiler-seed/parser-parity fixtures. This is
milestone-4 parser/compiler-frontend ergonomics substrate only. It must not
claim full parser parity, compiler-package implementation, platform/proof
progress, native/backend completion, package-manager/linker work, or
self-boot completion.

### Approach
Use the rev-007 direction-4b judgment: bounded parser-parity and compiler-seed
evidence shows selected source forms are expressible, while maintainable
compiler-frontend work still needs reusable parser-library substrate. The
active rev-007 roadmap already authorizes this substrate work, so this round
does not request a semantic roadmap update.

Rounds 341-352 already centralized diagnostic expectations, bounded projection
rows, bounded case branches, bounded application arguments, nested
parenthesized application depth, annotated lambda RHS depth, source-type
arrow-tail text, constructor-row accumulation, source-definition row
sequencing, program-module row sequencing, import-row sequencing, and
module-body source-definition row sequencing. Build on those substrate
patterns without reopening them.

Target the parser-value source-span extraction owner surface in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`.
The immediate evidence is the repeated fallback case handling in helpers such
as:

- `identifierNameFromValue`, `charLiteralTextFromValue`,
  `stringLiteralTextFromValue`, `intLiteralTextFromValue`, and
  `parserTextFromValue`;
- `spanFromTokenBounds`, `spanFromTokenStartToTokenStart`,
  `spanFromStartToCoordinate`, and `spanFromStartToTokenStart`;
- `coordinateFromValue`, `tokenStartCoordinate`, and `tokenEndCoordinate`;
  and
- `constructorRowsFromValue`, `appendConstructorRow`, and projection/span
  call sites that consume the same token/text/coordinate fallbacks.

Introduce the smallest owner-local helper surface that expresses these
repeated parser-value observations. Prefer narrow helpers in
`ParserParityParser.mlfp`, such as token-text fallback, start-coordinate
fallback, end-coordinate fallback, module-key-or-token coordinate fallback,
and token-bound span construction. Keep the existing public parser-combinator
`ParserValue` constructors unchanged unless the implementation proves a tiny
owner-local helper in `ParserParityParserCombinator.mlfp` is clearer and does
not widen the generic combinator API.

Preserve all existing fallback semantics exactly: token text helpers should
still return the same `"unknown"` strings for non-token values; coordinate
helpers should still return `basicUnexpectedSpan` for non-coordinate payloads;
`ValueModuleKey` must continue to act as the current-token fallback coordinate
where existing callers use it; constructor-row helpers must keep the same empty
string fallback; and parser-returning projection helpers must still fail with
`ExpectedCompleteModule basicUnexpectedSpan` where they do today.

Migrate only the local source-span/text/coordinate helper surface in this
round. Do not redesign parser diagnostics, source-span rendering semantics,
tokenization, projection-row rendering, constructor-row payloads, case/lambda
parsing, source-definition sequencing, import/module sequencing, production
parser behavior, checker policy, package metadata, or generated batch routing.
Do not add fixture-name shortcuts, pre-rendered projections, static negative
evidence, canonical parser bypasses, retired syntax aliases, parser-private
hacks, compatibility aliases for removed helper names, or
compiler-seed/package/platform/proof hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content introduces a shared parser-library
  source-span/parser-value extraction helper surface and migrates existing
  call sites across it. That needs implementer/reviewer judgment around
  fallback behavior, `ValueModuleKey` coordinate preservation, constructor-row
  payload interactions, and removal of duplicated helper plumbing. Focused
  verification is sufficient because the work is confined to the shared
  parser-parity library/spec/docs owner surface, preserves existing behavior,
  does not touch production parser, checker, resolver, backend, package,
  platform, proof, or native code, and makes no milestone closeout or
  self-boot claim.

### Steps
1. Inspect the current parser-value extraction helpers in
   `ParserParityParser.mlfp`, especially the token text, coordinate, span, and
   constructor-row fallback helpers named above.
2. Add a narrow owner-local helper surface for token text, start coordinate,
   end coordinate, module-key-or-token coordinate, and token-bound span
   extraction. Keep helper names explicit enough that static guards can prove
   the substrate exists.
3. Route `identifierNameFromValue`, literal-text helpers,
   `parserTextFromValue`, `coordinateFromValue`, `tokenStartCoordinate`,
   `tokenEndCoordinate`, `spanFromTokenBounds`,
   `spanFromTokenStartToTokenStart`, `spanFromStartToCoordinate`, and
   `spanFromStartToTokenStart` through the new helper surface while preserving
   their current fallbacks exactly.
4. Migrate the smallest direct call sites that become clearer once the helpers
   exist, such as constructor-row append and projection/span helpers, but do
   not stretch into grammar sequencing or diagnostic redesign.
5. Remove migrated duplicated extractor definitions or case branches only when
   the new helper has replaced them directly. Do not leave compatibility
   wrappers for old helper names unless the name remains the stable public
   local entry point used by existing parser code.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` requiring
   the new parser-value extraction helper surface, representative migrated
   token/text/coordinate/span call sites, and absence of duplicated
   hand-rolled fallback case blocks in the migrated local helpers.
7. Update `CHANGELOG.md` and `implementation_notes.md` only if the
   implementation needs a durable bounded-substrate note; any note must use
   explicit non-claim language.
8. Run the focused verification below and record implementation evidence in
   `orchestrator/rounds/round-353/implementation-notes.md`.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares parser-value source-span extraction substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A static guard over `ParserParityParser.mlfp` and
  `ProgramParserParitySpec.hs` proving the parser-value extraction helpers and
  representative migrated call sites are present, and that the migrated local
  token/text/coordinate/span helpers no longer carry duplicated hand-rolled
  fallback case blocks.
- A changed-line shortcut/overclaim guard for fixture-name shortcuts,
  pre-rendered projections, canonical-parser bypasses, static negative
  evidence, retired syntax aliases, compiler-package/platform/proof hooks,
  native/backend claims, package-manager/linker claims, self-boot claims, and
  full parser parity claims.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises source-span rendering, token-derived
names/text, constructor-row accumulation, projection rows, diagnostics, public
generated `run-program` batches, negative diagnostics, and shortcut guards
through the shared parser library. Full closeout gates,
`cabal build all && cabal test`, and `./scripts/thesis-conformance-gate.sh`
are not required for this focused non-closeout slice unless the implementation
widens beyond parser-library, spec, docs, and round-artifact scope or makes
thesis-facing semantic, package/platform/proof/native/backend,
milestone-closeout, or self-boot claims.

### Scheduler
- Depends on round ids: round-340, round-341, round-342, round-343, round-344, round-345, round-346, round-347, round-348, round-349, round-350, round-351, round-352
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
