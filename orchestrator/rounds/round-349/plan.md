### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-349-bounded-source-definition-row-sequence-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build one reusable shared parser-library substrate for fixed source-definition
row sequencing, then use it for the existing four-, thirteen-, and
sixteen-definition parser paths in `ParserParityParser.mlfp`.

The round should reduce copied definition-row batch plumbing while preserving
the exact-count behavior and aggregate parser-parity outputs already proved by
the compiler-seed/parser-parity fixtures. This is milestone-4
parser/compiler-frontend ergonomics substrate only. It must not claim full
parser parity, compiler-package implementation, platform/proof progress,
native/backend completion, package-manager/linker work, or self-boot
completion.

### Approach
Use the rev-007 direction-4b judgment: bounded seed/parity evidence shows the
selected source forms are expressible, while maintainable compiler-frontend
work still needs reusable parser-library substrate. The active rev-007 roadmap
already encodes this, so this round does not request a semantic roadmap update.

Rounds 341-348 already centralized diagnostic expectations, bounded projection
rows, bounded case branches, bounded application arguments, nested
parenthesized applications, annotated lambda RHS depth, source-type arrow-tail
text, and constructor-row payload accumulation. Build on those substrates
without reopening them.

Target the fixed source-definition sequence owner surface in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:

- `parseFourSourceDefinitionRows` and its second/third/fourth/finish helpers;
- `parseThirteenSourceDefinitionRows` and its four-definition batch append
  helpers;
- `parseSixteenSourceDefinitionRows` and its four-definition batch append
  helpers; and
- direct callers that should keep parsing the same exact source-definition
  counts, such as `parseSixDataFourDefinitionRows` and
  `parseFourDataThirteenDefinitionDefinitionRows`.

Introduce the smallest owner-local helper family that expresses the repeated
shape: parse one source definition row, append it to accumulated
`ValueProjectionRows` through `appendProjectionValues`, and continue through an
explicit remaining-budget entry point until the exact selected count is
consumed. Keep the budget names explicit enough that failure boundaries remain
reviewable. Prefer first-order helper entry points in `ParserParityParser.mlfp`
over a broad parser framework, public Prelude list API, or generic unbounded
sequence abstraction.

Migrate only the selected source-definition batch paths in this round. Preserve
the existing parse order, exact count budgets, source-definition semicolon
behavior, `defRows` rendering, projection-row text shape, parser diagnostics,
and final module-body close behavior. Leave import sequencing, multi-module
program sequencing, data-row sequencing, declaration-led body choice ordering,
source-type parsing, case/lambda parsing, source-span rendering semantics, and
diagnostic payloads for separate rounds unless a tiny call-site adjustment is
directly necessary to route through the selected helper.

Do not add fixture-name shortcuts, pre-rendered projections, static negative
evidence, canonical-parser bypasses, retired syntax aliases, parser-private
hacks, compatibility aliases for removed helper names, or
compiler-seed/package/platform/proof hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content introduces a reusable bounded
  source-definition sequence substrate and migrates multiple existing
  parser-library batch families across it. That needs implementer/reviewer
  judgment around exact-count budgets, accumulated projection rows, removed
  helper aliases, and preservation of module-body finishing behavior. Focused
  verification is sufficient because the work is confined to the shared
  parser-parity library/spec/docs owner surface, preserves existing behavior,
  does not touch production parser, checker, resolver, backend, package,
  platform, proof, or native code, and makes no milestone closeout or self-boot
  claim.

### Steps
1. Inspect the current source-definition sequence call graph in
   `ParserParityParser.mlfp`, especially the four-, thirteen-, and
   sixteen-definition paths named above.
2. Add the narrow bounded source-definition row helper family in
   `ParserParityParser.mlfp`, using `parseSourceDefinitionRows` for each row
   and `appendProjectionValues` for accumulation.
3. Migrate `parseFourSourceDefinitionRows`,
   `parseThirteenSourceDefinitionRows`, and `parseSixteenSourceDefinitionRows`
   onto the helper while preserving exact definition counts and all existing
   callers' output shape.
4. Remove migrated second/third/fourth/batch continuation aliases instead of
   leaving compatibility wrappers.
5. Keep unrelated parser surfaces unchanged: import rows, module rows,
   data-row families, declaration-led dispatch ordering, source-type parsing,
   case/lambda parsing, source spans, diagnostics, and production parser code.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` requiring
   the new helper surface, representative migrated call sites, and absence of
   the removed source-definition batch aliases.
7. Update `CHANGELOG.md` and `implementation_notes.md` with bounded
   ergonomics-substrate language and explicit non-claims.
8. Run the focused verification below and record implementation evidence in
   `orchestrator/rounds/round-349/implementation-notes.md`.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded source-definition row sequencing"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A static guard over `ParserParityParser.mlfp` and
  `ProgramParserParitySpec.hs` proving the helper and migrated call sites are
  present and the removed source-definition batch aliases are absent from
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
package/platform/proof/native/backend, milestone-closeout, or self-boot claims.

### Scheduler
- Depends on round ids: round-340, round-341, round-342, round-343, round-344, round-345, round-346, round-347, round-348
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
