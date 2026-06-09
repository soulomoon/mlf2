### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-351-bounded-import-row-sequence-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build one reusable parser-library substrate for bounded import row sequencing
inside complete module bodies, then migrate the existing one-import and
three-import module-body paths in `ParserParityParser.mlfp` onto it.

The round should reduce copied import-row accumulation plumbing while
preserving existing module-body dispatch, import projection row order,
post-import body parsing, source spans, diagnostics, package source-layout
evidence, and aggregate parser-parity outputs. This is milestone-4
parser/compiler-frontend ergonomics substrate only. It must not claim full
parser parity, compiler-package implementation, platform/proof progress,
native/backend completion, package-manager/linker work, or self-boot
completion.

### Approach
Use the rev-007 direction-4b judgment: bounded parser-parity and compiler-seed
evidence shows selected source forms are expressible, while maintainable
compiler-frontend work still needs reusable parser-library substrate. The
active rev-007 roadmap already authorizes this substrate work, so this round
does not request a semantic roadmap update.

Rounds 341-350 already centralized diagnostic expectations, bounded projection
rows, bounded case branches, bounded application arguments, nested
parenthesized application depth, annotated lambda RHS depth, source-type
arrow-tail text, constructor-row accumulation, bounded source-definition row
sequencing, and bounded program-module row sequencing. Build on those
substrates without reopening them.

Target the import-led module body owner surface in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:

- `parseThreeImportLedBodyRows`;
- `parseThreeImportSecondRows` and `appendThreeImportSecondRows`;
- `parseThreeImportThirdRows` and `appendThreeImportThirdRows`;
- `parseImportLedBodyRows`;
- `parseImportedBodyAfterImport`; and
- existing package/import parser-parity evidence that depends on the same
  import projection row order and post-import body continuation.

Introduce the smallest owner-local helper family that expresses the repeated
shape: parse one `parseImportProjectionRows` row, append later import rows with
`appendProjectionValues`, and continue through explicit remaining-import
budget entry points until the selected one- or three-import prefix is
consumed. Keep the budget names explicit enough that the one-import and
three-import boundaries remain reviewable. Prefer first-order helper entry
points in `ParserParityParser.mlfp`; do not introduce a broad parser
framework, generic unbounded import stream, package resolver, package manager,
or public Prelude list API.

Preserve the existing import-led body behavior: after the selected import
prefix is parsed, `parseImportedBodyAfterImport` must still choose among the
same post-import body families, including `parseFourDataThirteenDefinitionRows`,
`parseImportedMainDefinition`, `parseImportedTwoDataRows`, and
`parseThreeImportedSourceDefinitionRows`. Preserve import projection parsing,
import exposing diagnostics, module body close behavior, projection row text
shape, source-span rendering semantics, and canonical parser behavior. Leave
module body dispatch ordering, source-definition sequencing internals, data-row
sequencing internals, source-type parsing, case/lambda parsing, package
metadata, and generated batch routing unchanged unless a tiny call-site
adjustment is directly necessary to route through the selected helper.

Do not add fixture-name shortcuts, pre-rendered projections, static negative
evidence, canonical-parser bypasses, retired syntax aliases,
parser-private hacks, compatibility aliases for removed helper names, or
compiler-seed/package/platform/proof hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content introduces a reusable bounded import-row
  sequencing substrate and migrates the import-led module body prefix that
  feeds post-import body parsing. That needs implementer/reviewer judgment
  around explicit remaining-import budgets, accumulated projection rows,
  body-continuation behavior, and removal of copied helper aliases. Focused
  verification is sufficient because the work is confined to the shared
  parser-parity library/spec/docs owner surface, preserves existing behavior,
  does not touch production parser, checker, resolver, backend, package,
  platform, proof, or native code, and makes no milestone closeout or
  self-boot claim.

### Steps
1. Inspect the current import-led body call graph in
   `ParserParityParser.mlfp`, especially `parseThreeImportLedBodyRows`,
   `parseThreeImportSecondRows`, `appendThreeImportSecondRows`,
   `parseThreeImportThirdRows`, `appendThreeImportThirdRows`,
   `parseImportLedBodyRows`, and `parseImportedBodyAfterImport`.
2. Add a narrow bounded import-row sequencing helper family in
   `ParserParityParser.mlfp`. It should use `parseImportProjectionRows` for
   each import row, `appendProjectionValues` for accumulation, and explicit
   one-/three-import remaining-budget entry points.
3. Migrate `parseThreeImportLedBodyRows` onto the new helper while preserving
   exactly three import rows before the post-import body continuation.
4. Migrate `parseImportLedBodyRows` onto the same helper while preserving the
   existing one-import prefix before `parseImportedBodyAfterImport`.
5. Remove the migrated second/third import continuation aliases instead of
   leaving compatibility wrappers.
6. Keep unrelated parser surfaces unchanged: import projection item/list
   parsing, module body dispatch ordering outside the selected call sites,
   source-definition rows, data rows, source-type parsing, case/lambda parsing,
   spans, diagnostics, production parser code, package metadata, and generated
   batch routing.
7. Add focused static coverage in `test/ProgramParserParitySpec.hs` requiring
   the new helper surface, representative one-/three-import call-site use, and
   absence of the removed import-row sequence aliases.
8. Update `CHANGELOG.md` and `implementation_notes.md` with bounded
   ergonomics-substrate language and explicit non-claims.
9. Run the focused verification below and record implementation evidence in
   `orchestrator/rounds/round-351/implementation-notes.md`.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded import row sequencing"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A static guard over `ParserParityParser.mlfp` and
  `ProgramParserParitySpec.hs` proving the bounded import-row helper and
  migrated one-/three-import call sites are present and the removed import-row
  sequence aliases are absent from parser-library source.
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
- Depends on round ids: round-340, round-341, round-342, round-343, round-344, round-345, round-346, round-347, round-348, round-349, round-350
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
