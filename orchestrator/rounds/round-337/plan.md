### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4a-canonical-parser-parity
- Extracted item id: item-337-package-source-layout-parser-parity
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-006
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006

### Goal
Extend parser-owned `.mlfp` parser parity to package-source layout fixtures
whose syntax is already represented in prior parser-parity source families but
whose exact package roots are not yet covered as first-class parser-parity
inputs:

- `test/conformance/mlfp/run-program/cross-module-let/src/Core.mlfp`
- `test/conformance/mlfp/run-program/cross-module-let/src/Main.mlfp`
- `test/conformance/mlfp/run-program/search-path-package/roots/lib/SearchLib.mlfp`
- `test/conformance/mlfp/run-program/search-path-package/roots/main/Main.mlfp`

This round should prove canonical parser parity for the small same-root
`Core`/`Main` package and ordered search-path `SearchLib`/`Main` package
without treating the existing single-source
`authoritative-cross-module-let-polymorphism` fixture or Haskell package
checker tests as sufficient evidence for exact package source layout.

### Approach
Use the existing shared parser-library path and generated public CLI parser
batch. Do not add fixture-specific whole-package recognizers, concatenated
source shortcuts, pre-rendered projection rows, compatibility aliases, static
negative evidence, resolver/checker fallback behavior, or canonical-parser
bypasses.

Add parser-parity conformance fixtures for the two package layouts, preserving
the exact source-file paths inside the parser projection. The Haskell canonical
side should derive the expected parser projection by parsing each package
source file with the canonical parser and aggregating the per-file projections
in the package/source order selected for the fixture. The `.mlfp` shared parser
side should receive the same source-file path/text pairs through the generated
public CLI parser batch and render matching package-layout parser evidence.

Extend `test/programs/compiler-parser-parity/parser-library/` only as needed
to render package-layout sections from parsed source texts:

- parse `Core.mlfp` and `Main.mlfp` in the cross-module-let same-root package;
- parse `SearchLib.mlfp` and `Main.mlfp` in the ordered search-path package;
- reuse the established module header, export list, import exposing, typed
  value definition, let/lambda/application, and simple variable-reference
  parser paths;
- render each file's projection from parsed source structure and its actual
  source path rather than a fixture key or static module name; and
- add one dynamic package-layout negative case, preferably a malformed
  same-root import declaration or missing definition semicolon, through
  `renderParserNegativeEvidenceFromSourceText`.

Update repo-facing notes only with bounded parser-parity language. Respect
`orchestrator/project-contract.md` for shared invariants and do not claim full
parser parity, package checker/resolver progress, backend/native progress,
compiler-package progress, platform work, proof work, driver work, or
self-boot progress.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected source family is bounded to two existing small package
  layouts, but the task content is not only mechanical fixture registration.
  It introduces a package-layout parser-parity evidence boundary with
  multiple source files, ordered search-path roots, and source-path-preserving
  aggregate projections. Focused verification is sufficient because this is a
  non-closeout parser-parity slice with no production parser replacement, no
  checker/resolver/backend behavior claim, no compiler-package claim, and no
  milestone completion claim.

### Steps
1. Add package-layout parser-parity conformance fixtures under
   `test/conformance/mlfp/parser-parity/package-cross-module-let/` and
   `test/conformance/mlfp/parser-parity/package-search-path-import/`, copying
   the exact source files from the existing run-program conformance fixtures
   listed in the goal.
2. Commit canonical package-layout parser projections under each fixture's
   `expected/parser-program.txt`. The projections must preserve the individual
   source paths and module names `Core`, `Main`, `SearchLib`, and `Main` rather
   than collapsing them into one concatenated source fixture.
3. Add matching thin parser-owned package roots under
   `test/programs/compiler-parser-parity/package-cross-module-let/` and
   `test/programs/compiler-parser-parity/package-search-path-import/`. Each
   root should expose the selected source-file path/text pairs and call a
   shared parser-library renderer for package-layout projections.
4. Extend `ProgramParserParitySpec` with source/expected/root constants,
   direct shared-parser assertions for both package-layout fixtures, aggregate
   positive batch registration, one dynamic package-layout negative assertion,
   and shortcut/static guard phrases for whole-package, concatenated-source,
   pre-rendered row, fixture-name, and static-negative shortcuts.
5. Extend or refactor `ParserParityParser.mlfp` and related parser-library
   modules so the package-layout projections parse through shared token,
   parser-state, projection-row, diagnostic, and dynamic negative-evidence
   paths. Keep source ordering and source-path rendering explicit in the
   renderer rather than deriving it from hard-coded fixture labels.
6. Update `implementation_notes.md`, `CHANGELOG.md`, and
   `docs/mlfp-self-boot-readiness.md` with bounded evidence for the package
   source-layout parser-parity fixtures and explicit non-claims for full
   parser parity, package checker/resolver/backend, compiler-package,
   platform, driver, proof, and self-boot completion.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program shared conformance corpus"'`

The parser-parity Hspec group is the focused aggregate parser/conformance run
for this owner surface. It compiles the test suite, checks canonical
projections, runs the generated public CLI parser batch, covers the selected
dynamic package-layout negative evidence, and exercises shortcut/static guards.
The shared conformance corpus check confirms the source package fixtures copied
or referenced by this round still run through the existing public package
fixture contract.

Do not run full closeout gates for this round unless implementation widens
beyond the selected parser-layout slice. This plan does not authorize
milestone closeout, production parser replacement, package checker policy
changes, resolver policy changes, backend/native behavior, platform work,
compiler-package work, driver work, proof claims, or self-boot claims. Run
`./scripts/thesis-conformance-gate.sh` only if implementation edits thesis
obligation ledgers or makes a thesis/readiness claim beyond the bounded
parser-parity notes above; otherwise record the explicit deferral reason in
implementation evidence.

### Scheduler
- Depends on round ids: none
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
