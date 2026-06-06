### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4a-canonical-parser-parity
- Extracted item id: item-338-compiler-seed-data-model-parser-parity
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-006
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006

### Goal
Extend bounded parser-owned `.mlfp` parser parity to the existing compiler-seed
frontend data-model package modules: `SeedSource.mlfp`, `SeedToken.mlfp`,
`SeedDiagnostic.mlfp`, and `SeedAst.mlfp`.

This round should prove that the canonical parser and shared parser-parity
library agree on the selected compiler-seed source modules as ordinary package
source text. It must not start milestone 6 compiler-package implementation,
claim full parser parity, or change checker/resolver/backend/package execution
semantics.

### Approach
Use the already checked compiler-seed fixture at
`test/programs/compiler-seed/frontend-contract/` as the source family. Add a
new parser-parity conformance fixture under
`test/conformance/mlfp/parser-parity/compiler-seed-data-model/` whose selected
source files are byte-for-byte copies of:

- `test/programs/compiler-seed/frontend-contract/SeedSource.mlfp`
- `test/programs/compiler-seed/frontend-contract/SeedToken.mlfp`
- `test/programs/compiler-seed/frontend-contract/SeedDiagnostic.mlfp`
- `test/programs/compiler-seed/frontend-contract/SeedAst.mlfp`

Commit the canonical parser projection for those four files in one
`expected/parser-program.txt`. Add a thin parser-owned program under
`test/programs/compiler-parser-parity/compiler-seed-data-model/` that exposes
only source paths/text and calls the shared parser library.

Extend `test/programs/compiler-parser-parity/parser-library/` only as needed to
parse the selected modules structurally. The likely implementation boundary is
bounded package projection over four source texts plus parser support for the
selected data-model declarations, export/import rows, constructor rows, and
simple data-model source types. Do not add fixture-name shortcuts,
pre-rendered projection rows, canonical-parser bypasses, static negative
evidence, or a general package resolver.

Extend `test/ProgramParserParitySpec.hs` with direct shared-parser assertions,
aggregate public CLI positive coverage, one meaningful malformed selected
syntax negative case, source-copy checks, and shortcut/static guards for this
slice. Update `implementation_notes.md`, `CHANGELOG.md`, and
`docs/mlfp-self-boot-readiness.md` with bounded parser-parity language and
explicit non-claims.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The goal and verification boundary are bounded, but the task content
  itself needs parser-library design judgment across package projection arity
  and compiler-seed data-model module shapes. It may require extending shared
  parser behavior rather than only registering mechanical fixtures. Focused
  verification is sufficient because this is a non-closeout parser-parity slice
  with no production parser replacement, no checker/resolver/backend/package
  behavior change, no platform/proof/compiler-package work, and no milestone
  completion claim.

### Steps
1. Copy the four selected compiler-seed data-model source modules into
   `test/conformance/mlfp/parser-parity/compiler-seed-data-model/`, preserving
   byte-for-byte source text and stable package-source ordering.
2. Generate and commit one canonical `expected/parser-program.txt` projection
   for the selected source sequence using the current Haskell canonical parser.
3. Add `test/programs/compiler-parser-parity/compiler-seed-data-model/` with a
   thin `ParserParityFixture.mlfp` and `Main.mlfp` that route the four selected
   source texts through shared parser-library entrypoints.
4. Extend `ParserParityParser.mlfp` and related parser-library modules only for
   structural grammar needed by the selected seed data-model modules. Prefer a
   bounded multi-source projection helper over any package resolver or
   broad compiler-package machinery.
5. Extend `ProgramParserParitySpec` with source/expected/root constants,
   direct shared-parser equality checks, aggregate positive registration, one
   malformed selected-syntax negative case, source-copy equality checks, and
   shortcut/static guards covering seed-module names and projection text.
6. Update repo-facing notes with bounded parser-parity evidence and explicit
   non-claims for full parser parity, checker/resolver/backend behavior,
   compiler-package implementation, platform work, driver work, proof work, and
   self-boot completion.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- Source-copy check proving every selected
  `test/conformance/mlfp/parser-parity/compiler-seed-data-model/...` source is
  byte-for-byte equal to its
  `test/programs/compiler-seed/frontend-contract/...` source.
- Static guard check over
  `test/programs/compiler-parser-parity/parser-library/` and
  `test/ProgramParserParitySpec.hs` proving this slice did not add
  seed-module fixture shortcuts, pre-rendered parser-program rows, static
  negative evidence, token-stream shortcuts, or canonical-parser bypasses.
- Docs overclaim check over `implementation_notes.md`, `CHANGELOG.md`, and
  `docs/mlfp-self-boot-readiness.md`.

The aggregate parser-parity Hspec group is the focused parser/conformance gate
for this bounded slice. It exercises the selected shared parser-library path
through direct assertions and the public generated `run-program` batch. Full
closeout gates, `cabal build all && cabal test`, and
`./scripts/thesis-conformance-gate.sh` are not required unless the
implementation widens beyond parser-parity fixture/library/docs scope or claims
milestone completion.

### Scheduler
- Depends on round ids: round-337
- Merge after item ids: item-337-package-source-layout-parser-parity
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
