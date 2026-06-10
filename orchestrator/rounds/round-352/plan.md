### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-352-bounded-module-body-source-definition-row-sequence-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build a reusable parser-library substrate for bounded module-body
source-definition row sequencing, then migrate the existing two-definition,
three-definition, and imported three-definition body paths in
`ParserParityParser.mlfp` onto it.

The round should reduce copied source-definition continuation plumbing while
preserving module-body dispatch, exact definition counts, definition-row order,
post-import body parsing, source spans, diagnostics, package source-layout
evidence, and aggregate parser-parity outputs. This is milestone-4
parser/compiler-frontend ergonomics substrate only. It must not claim full
parser parity, compiler-package implementation, platform/proof progress,
native/backend completion, package-manager/linker work, or self-boot
completion.

### Approach
Use the rev-007 direction-4b judgment: bounded parser-parity and
compiler-seed evidence shows selected source forms are expressible, while
maintainable compiler-frontend work still needs reusable parser-library
substrate. The active rev-007 roadmap already authorizes this substrate work,
so this round does not request a semantic roadmap update.

Rounds 341-351 already centralized diagnostic expectations, bounded
projection rows, bounded case branches, bounded application arguments, nested
parenthesized application depth, annotated lambda RHS depth, source-type
arrow-tail text, constructor-row accumulation, bounded source-definition row
sequencing, bounded program-module row sequencing, and bounded import-row
sequencing. Build on those substrate patterns without reopening them.

Target the module-body source-definition sequencing owner surface in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:

- `parseTwoDefinitionBodyRows` and `parseTwoDefinitionSecondRows`;
- `parseThreeDefinitionBodyRows`, `parseSecondSourceDefinitionRows`,
  `appendFirstSecondSourceDefinitionRows`, and
  `parseThirdSourceDefinitionRows`;
- `parseThreeImportedSourceDefinitionRows`,
  `parseThreeImportedSourceDefinitionSecondRows`,
  `appendThreeImportedSourceDefinitionSecondRows`, and
  `parseThreeImportedSourceDefinitionThirdRows`; and
- existing parser-parity package/import evidence that depends on the same
  definition-row order and post-import body continuation.

Introduce the smallest owner-local helper entrypoints that express exact two-
and three-source-definition row sequences using the bounded source-definition
row substrate from round 349. Prefer explicit `parseTwoSourceDefinitionRows`
and `parseThreeSourceDefinitionRows` style entrypoints over a generic
unbounded list API or broad parser framework. The helper should parse one
`parseSourceDefinitionRows` row at a time, append rows through
`appendProjectionValues`, and return accumulated `ValueProjectionRows` to the
caller-provided body continuation.

Migrate only the selected module-body and imported-body source-definition
paths in this round. Preserve `parseDefinitionLedBodyRows` ordering and keep
`parseOneDefinitionBodyRows` plus
`parseSourceDefinitionRowsWithCurrentDefSemicolon` unchanged unless the
implementation can prove the current-token `ParserExpectDefSemicolon`
diagnostic is preserved exactly. Leave data-prefixed definition suffixes such
as `parseDataTwoDefinitionRows`, `parseDataThreeDefinitionRows`,
`parseDataFourDefinitionRows`, `parseSingleConstructorDefinitionRows`,
`parseNatDataSingleDefinitionRows`, `parseClassDerivedDefinitionRows`, and
`parseSixDataFourDefinitionRows` for later bounded rounds unless a tiny
call-site adjustment is directly necessary to route through the selected
helper.

Preserve source-definition parsing internals, import-row sequencing, module
row sequencing, data-row sequencing, source-type parsing, case/lambda parsing,
source-span rendering semantics, diagnostic payloads, production parser code,
package metadata, and generated batch routing. Do not add fixture-name
shortcuts, pre-rendered projections, static negative evidence, canonical
parser bypasses, retired syntax aliases, parser-private hacks, compatibility
aliases for removed helper names, or compiler-seed/package/platform/proof
hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content introduces reusable exact two-/three-row
  source-definition body sequencing and migrates body continuations that feed
  complete module and imported-body parsing. That needs implementer/reviewer
  judgment around exact counts, accumulated projection rows, current
  semicolon diagnostics, post-import continuation behavior, and removal of
  copied helper aliases. Focused verification is sufficient because the work
  is confined to the shared parser-parity library/spec/docs owner surface,
  preserves existing behavior, does not touch production parser, checker,
  resolver, backend, package, platform, proof, or native code, and makes no
  milestone closeout or self-boot claim.

### Steps
1. Inspect the current source-definition body call graph in
   `ParserParityParser.mlfp`, especially the selected two-definition,
   three-definition, and imported three-definition paths named above.
2. Add narrow exact two-/three-source-definition helper entrypoints in
   `ParserParityParser.mlfp`, reusing the existing bounded
   source-definition row helper family and `appendProjectionValues`.
3. Migrate `parseTwoDefinitionBodyRows` onto the two-definition helper while
   preserving the existing `finishModuleBodyRows` continuation and output row
   order.
4. Migrate `parseThreeDefinitionBodyRows` onto the three-definition helper
   while preserving `parseDefinitionLedBodyRows` dispatch ordering and module
   body close behavior.
5. Migrate `parseThreeImportedSourceDefinitionRows` onto the same
   three-definition helper while preserving `parseImportedBodyAfterImport` and
   `finishImportedBodyRows` behavior.
6. Remove the migrated second/third source-definition continuation aliases
   instead of leaving compatibility wrappers.
7. Keep unrelated parser surfaces unchanged: one-definition current-semicolon
   diagnostics, data-prefixed definition suffixes, import rows, module rows,
   data rows, source-type parsing, case/lambda parsing, spans, diagnostics,
   production parser code, package metadata, and generated batch routing.
8. Add focused static coverage in `test/ProgramParserParitySpec.hs` requiring
   the new helper surface, representative migrated call sites, and absence of
   the removed module-body source-definition sequence aliases.
9. Update `CHANGELOG.md` and `implementation_notes.md` with bounded
   ergonomics-substrate language and explicit non-claims.
10. Run the focused verification below and record implementation evidence in
    `orchestrator/rounds/round-352/implementation-notes.md`.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded module-body source-definition row sequencing"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A static guard over `ParserParityParser.mlfp` and
  `ProgramParserParitySpec.hs` proving the bounded two-/three-definition
  helper surface and migrated call sites are present and the removed
  module-body source-definition sequence aliases are absent from
  parser-library source.
- A changed-line shortcut/overclaim guard for fixture-name shortcuts,
  pre-rendered projections, canonical-parser bypasses, static negative
  evidence, retired syntax aliases, compiler-package/platform/proof hooks,
  native/backend claims, package-manager/linker claims, self-boot claims, and
  full parser parity claims.

Full closeout gates, `cabal build all && cabal test`, and
`./scripts/thesis-conformance-gate.sh` are not required for this focused
non-closeout slice unless the implementation widens beyond parser-library,
spec, docs, and round-artifact scope or makes thesis-facing semantic,
package/platform/proof/native/backend, milestone-closeout, or self-boot
claims.

### Scheduler
- Depends on round ids: round-340, round-341, round-342, round-343, round-344, round-345, round-346, round-347, round-348, round-349, round-350, round-351
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
