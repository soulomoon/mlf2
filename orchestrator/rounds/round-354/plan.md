### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-354-diagnostic-evidence-rendering-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build a reusable parser-library diagnostic evidence rendering substrate, then
migrate the current parser-boundary diagnostic rendering in
`ParserParityParser.mlfp` onto that substrate.

The round should reduce the long duplicated diagnostic case plumbing around
negative parser/lexer evidence while preserving the exact rendered evidence
strings, source-span rendering, parser behavior, and aggregate parser-parity
outputs. This is milestone-4 parser/compiler-frontend ergonomics substrate
only. It must not claim full parser parity, compiler-package implementation,
platform/proof progress, native/backend completion, package-manager/linker
work, or self-boot completion.

### Approach
Use the rev-007 direction-4b judgment: bounded parser-parity and compiler-seed
evidence shows selected source forms are expressible, while maintainable
compiler-frontend work still needs reusable parser-library substrate. The
active roadmap already authorizes this substrate work, so this round does not
request a semantic roadmap update.

Rounds 341-353 already centralized diagnostic expectations, bounded projection
rows, bounded case branches, bounded application arguments, nested
parenthesized application depth, annotated lambda RHS depth, source-type
arrow-tail text, constructor-row accumulation, source-definition row
sequencing, program-module row sequencing, import-row sequencing, module-body
source-definition row sequencing, and parser-value source-span extraction.
Build on those substrate patterns without reopening them.

Target the diagnostic evidence owner surface in
`test/programs/compiler-parser-parity/parser-library/`. The current repeated
evidence is the `renderDiagnosticEvidence` case expression at the end of
`ParserParityParser.mlfp`; it maps every `ParserDiagnostic` constructor to a
stable evidence label and renders the diagnostic span through `renderSpan`.
That mapping belongs with `ParserParityDiagnostic.mlfp`, not with the parser
entrypoint, now that round 341 owns expectation-to-diagnostic construction and
round 353 owns reusable parser-value/span extraction.

Introduce the smallest diagnostic helper surface in
`ParserParityDiagnostic.mlfp`, such as:

- `diagnosticEvidenceLabel : ParserDiagnostic -> String`;
- `diagnosticEvidenceSpan : ParserDiagnostic -> String`; and
- `renderParserDiagnosticEvidence : String -> ParserDiagnostic -> String`.

Import only the existing string and source-span rendering helpers needed by
that module. Migrate the parser entrypoints that currently call the local
`renderDiagnosticEvidence` helper, including positive token evidence, lexer
negative evidence, parser negative evidence, and retry evidence, to call the
new diagnostic-owned renderer. Remove the parser-local long diagnostic
rendering case after the migration instead of leaving a compatibility alias.

Preserve every externally observed diagnostic evidence string exactly, such as
`unexpected-source@`, `expected-complete-module@`, `expected-equals@`, and the
existing constructor-specific labels. Preserve the existing source-file
argument threading and `renderSpan` behavior. Do not add or remove diagnostic
constructors, change expectation semantics, change parser-choice behavior,
change fallback spans, change source-span rendering, change tokenization,
change projection-row rendering, redesign diagnostics, or touch production
parser/checker/resolver/backend/package/platform/proof/native code.

Do not add fixture-name shortcuts, pre-rendered projections, static negative
evidence, canonical parser bypasses, retired syntax aliases, parser-private
hacks, compatibility aliases for removed helper names, or
compiler-seed/package/platform/proof hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content moves a shared diagnostic rendering
  responsibility from the parser entrypoint into the diagnostic owner module
  and migrates evidence call sites across that boundary. That introduces a new
  parser-library helper surface and needs implementer/reviewer judgment around
  module ownership, export shape, exact evidence labels, and span rendering.
  Focused verification is sufficient because the work is confined to the
  shared parser-parity library/spec/docs owner surface, preserves existing
  behavior, does not touch production parser, checker, resolver, backend,
  package, platform, proof, or native code, and makes no milestone closeout or
  self-boot claim.

### Steps
1. Inspect `ParserParityDiagnostic.mlfp`,
   `ParserParityParserCombinator.mlfp`, and the local
   `renderDiagnosticEvidence` implementation in `ParserParityParser.mlfp`.
   Record the exact current label-to-constructor mapping before editing.
2. Add the diagnostic-owned helper surface in `ParserParityDiagnostic.mlfp`
   with explicit label, span, and source-file rendering helpers. Keep helper
   names concrete enough that static guards can prove the substrate exists.
3. Update the `ParserParityDiagnostic.mlfp` export list and imports only as
   needed for the new helper surface. Avoid widening the parser-combinator API
   unless a compile error proves a tiny import adjustment is necessary.
4. Migrate `ParserParityParser.mlfp` evidence call sites to the new
   diagnostic-owned renderer, including `renderPositiveTokenEvidence`,
   `renderLexerNegativeEvidence`, `renderParserNegativeEvidence`, and
   `renderParserParityRetryEvidence` through those helpers.
5. Remove the parser-local `renderDiagnosticEvidence` case expression once all
   call sites use the diagnostic-owned renderer. Do not leave a compatibility
   wrapper with the old parser-local name.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` requiring
   the diagnostic evidence helper surface, representative migrated call sites,
   and absence of the parser-local diagnostic rendering case on the migrated
   path.
7. Update `CHANGELOG.md` and `implementation_notes.md` only if the
   implementation needs a durable bounded-substrate note; any note must use
   explicit non-claim language.
8. Run the focused verification below and record implementation evidence in
   `orchestrator/rounds/round-354/implementation-notes.md`.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares diagnostic evidence rendering substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A static guard over `ParserParityDiagnostic.mlfp`,
  `ParserParityParser.mlfp`, and `ProgramParserParitySpec.hs` proving the
  diagnostic evidence helpers and representative migrated call sites are
  present, and that the migrated parser-local diagnostic rendering case is not
  still present as a compatibility wrapper.
- A changed-line shortcut/overclaim guard for fixture-name shortcuts,
  pre-rendered projections, canonical-parser bypasses, static negative
  evidence, retired syntax aliases, compiler-package/platform/proof hooks,
  native/backend claims, package-manager/linker claims, self-boot claims, and
  full parser parity claims.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises positive projections, source-span
rendering, parser diagnostics, public generated `run-program` batches,
negative diagnostics, retry evidence, and shortcut guards through the shared
parser library. Full closeout gates, `cabal build all && cabal test`, and
`./scripts/thesis-conformance-gate.sh` are not required for this focused
non-closeout slice unless the implementation widens beyond parser-library,
spec, docs, and round-artifact scope or makes thesis-facing semantic,
package/platform/proof/native/backend, milestone-closeout, or self-boot
claims.

### Scheduler
- Depends on round ids: round-341, round-353
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
