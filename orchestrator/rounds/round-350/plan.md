### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-350-bounded-program-module-row-sequence-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build one reusable parser-library substrate for bounded complete-program
module-row sequencing, then migrate the existing two-, three-, and four-module
complete-program tail paths in `ParserParityParser.mlfp` onto it.

The round should reduce copied multi-module accumulation plumbing while
preserving the existing complete-program parse order, end-of-input success
boundary, projection row order, parser diagnostics, package source-layout
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

Rounds 341-349 already centralized diagnostic expectations, bounded projection
rows, bounded case branches, bounded application arguments, nested
parenthesized application depth, annotated lambda RHS depth, source-type
arrow-tail text, constructor-row accumulation, and bounded source-definition
row sequencing. Build on those substrate patterns without reopening them.

Target the complete-program module sequencing owner surface in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:

- `parseCompleteProgramTail`;
- `parseProgramSecondModuleOrDone` and `appendSecondProgramModuleRows`;
- `parseProgramThirdModuleOrDone` and `appendThirdProgramModuleRows`;
- `parseProgramFourthModuleOrDone` and `appendFourthProgramModuleRows`; and
- existing public multi-module/package evidence that depends on the same
  projection row order.

Introduce the smallest owner-local helper family that expresses the repeated
shape: after the first `parseSharedProgramModule`, either return the
accumulated `ValueProjectionRows` at end of input or parse one more shared
program module, append its projection rows with `appendLine`, and continue
through an explicit remaining-module-budget entry point. Keep the budget names
explicit enough that the two-, three-, and four-module boundaries remain
reviewable. Prefer first-order helper entry points in
`ParserParityParser.mlfp`; do not introduce a broad parser framework, generic
unbounded stream abstraction, package manager, or public Prelude list API.

Preserve the existing four-module maximum and complete-syntax boundary: after
the fourth module is appended, any additional tokens must still fail through
the existing `parserReplyToResult` not-at-end check rather than being accepted.
Preserve `parseSharedProgramModule`, module export/body parsing, import
sequencing, source-definition sequencing, data-row parsing, source-type
parsing, case/lambda parsing, source-span rendering semantics, diagnostic
payloads, and canonical parser behavior unless a tiny call-site adjustment is
directly necessary to route through the selected helper.

Do not add fixture-name shortcuts, pre-rendered projections, static negative
evidence, canonical-parser bypasses, retired syntax aliases, parser-private
hacks, compatibility aliases for removed helper names, or
compiler-seed/package/platform/proof hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content introduces a reusable complete-program
  module sequencing substrate and migrates the shared parse tail that governs
  multi-module/package parser-parity inputs. That needs implementer/reviewer
  judgment around explicit remaining-module budgets, row accumulation,
  end-of-input success behavior, and removal of copied helper aliases. Focused
  verification is sufficient because the work is confined to the shared
  parser-parity library/spec/docs owner surface, preserves existing behavior,
  does not touch production parser, checker, resolver, backend, package,
  platform, proof, or native code, and makes no milestone closeout or
  self-boot claim.

### Steps
1. Inspect the current complete-program module tail call graph in
   `ParserParityParser.mlfp`, especially `parseCompleteProgramTail` and the
   second/third/fourth module append helpers.
2. Add a narrow bounded program-module sequencing helper family in
   `ParserParityParser.mlfp`. It should use `parserReturnAtEndOr`,
   `parseSharedProgramModule`, and a single helper that appends module
   projection rows and continues to the next explicit remaining-budget entry
   point.
3. Migrate `parseCompleteProgramTail` onto the new helper after the first
   module has produced `ValueProjectionRows`. Keep non-projection payloads
   fail-closed with the current `ExpectedCompleteModule basicUnexpectedSpan`
   behavior.
4. Remove the migrated second/third/fourth module continuation aliases instead
   of leaving compatibility wrappers.
5. Keep unrelated parser surfaces unchanged: module body parsing, export rows,
   import rows, source-definition rows, data rows, source-type parsing,
   case/lambda parsing, spans, diagnostics, production parser code, package
   metadata, and generated batch routing.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` requiring
   the new helper surface, representative migrated call sites, and absence of
   the removed program-module sequence aliases.
7. Update `CHANGELOG.md` and `implementation_notes.md` with bounded
   ergonomics-substrate language and explicit non-claims.
8. Run the focused verification below and record implementation evidence in
   `orchestrator/rounds/round-350/implementation-notes.md`.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded program module row sequencing"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A static guard over `ParserParityParser.mlfp` and
  `ProgramParserParitySpec.hs` proving the helper and migrated call sites are
  present and the removed second/third/fourth program-module aliases are
  absent from parser-library source.
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
- Depends on round ids: round-340, round-341, round-342, round-343, round-344, round-345, round-346, round-347, round-348, round-349
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
