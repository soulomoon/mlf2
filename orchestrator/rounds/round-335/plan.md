### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4a-canonical-parser-parity
- Extracted item id: item-335-named-recursive-adt-source-module-parser-parity
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-006
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006

### Goal
Extend the shared parser-owned `.mlfp` parser parity library with one bounded
batch for the remaining exact recursive-ADT source corpus modules whose bodies
are already represented by `Main` parser-parity fixtures but whose source
files use real module names:

- `test/programs/recursive-adt/deriving-eq.mlfp`
- `test/programs/recursive-adt/recursive-gadt.mlfp`
- `test/programs/recursive-adt/recursive-existential.mlfp`

The round should prove canonical parser parity for the named modules
`DerivingEq`, `RecursiveGadt`, and `RecursiveExistential` without treating
`Main`-named fixtures as sufficient evidence for the exact recursive-ADT
corpus.

### Approach
Use the existing shared parser-library path and batch the three cases because
they share the parser owner surface, verification command, and failure mode:
exact recursive-ADT source modules whose syntax families are already covered
but whose module headers and rendered module rows must preserve the actual
module names.

Do not add fixture-specific whole-source recognizers, pre-rendered projection
rows, compatibility aliases, static negative evidence, token-stream shortcuts,
or canonical-parser bypasses.

Add parser-parity conformance fixtures under
`test/conformance/mlfp/parser-parity/deriving-eq/`,
`test/conformance/mlfp/parser-parity/recursive-gadt/`, and
`test/conformance/mlfp/parser-parity/recursive-existential/`, plus matching
thin `.mlfp` package roots under `test/programs/compiler-parser-parity/`.
Each package root should expose `sourceFile` and `sourceText`, then call
`renderParserParityProjectionFromSourceText`.

Extend `test/programs/compiler-parser-parity/parser-library/` only as needed
to parse these three named-module variants through shared grammar paths:

- parse `DerivingEq` with the same `Eq`, `Nat(..) deriving Eq`, `eq`, and
  `main` syntax as the existing typeclass deriving method fixture;
- parse `RecursiveGadt` with the same `Nat(..)`, `Expr(..)`, `doneNow`, and
  `main` syntax as the existing GADT result constructor fixture;
- parse `RecursiveExistential` with the same `Nat(..)`, `Expr(..)`,
  `SomeExpr(..)`, `unwrapSome`, and `main` syntax as the existing existential
  constructor forall fixture; and
- render module/export/data/constructor/definition rows from parsed tokens and
  source spans so the projected module names are the actual source names.

Add one dynamic negative case for the batched named-module surface, preferably
a malformed recursive-ADT case branch or malformed named-module header that
runs through `renderParserNegativeEvidenceFromSourceText`.

Update repo-facing notes only with bounded parser-parity language. Respect
`orchestrator/project-contract.md` for shared invariants and do not claim full
parser parity, checker/resolver/backend progress, compiler-package progress,
platform work, proof work, or self-boot progress.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected source batch is bounded, but its content requires
  shared parser-library behavior changes across three existing recursive-ADT
  grammar families: older paths currently expect `Main` and return static
  projection keys, while this slice must preserve named module headers and
  dynamic projection rows for exact corpus files. Focused verification is
  sufficient because this is a non-closeout parser-parity slice with no
  production parser replacement, no checker/backend behavior claim, and no
  milestone completion claim.

### Steps
1. Add the three conformance fixture sources by copying the exact recursive-ADT
   corpus files into:
   `test/conformance/mlfp/parser-parity/deriving-eq/src/Main.mlfp`,
   `test/conformance/mlfp/parser-parity/recursive-gadt/src/Main.mlfp`, and
   `test/conformance/mlfp/parser-parity/recursive-existential/src/Main.mlfp`.
2. Commit canonical parser projections for those sources under each fixture's
   `expected/parser-program.txt`; the projection must show module names
   `DerivingEq`, `RecursiveGadt`, and `RecursiveExistential`.
3. Add thin parser-owned package roots under
   `test/programs/compiler-parser-parity/deriving-eq/`,
   `test/programs/compiler-parser-parity/recursive-gadt/`, and
   `test/programs/compiler-parser-parity/recursive-existential/`.
4. Extend `ProgramParserParitySpec` with source/expected/root constants,
   direct shared-parser assertions for each fixture, aggregate positive batch
   registrations, one batched dynamic negative assertion, and shortcut/static
   guard phrases for the selected named-module surfaces.
5. Refactor or extend `ParserParityParser.mlfp` and any related parser-library
   modules so the selected deriving, GADT, and existential syntax families
   parse the source module name dynamically and render projection rows from
   parsed source structure rather than `Main`-only static projection keys.
6. Update `implementation_notes.md`, `CHANGELOG.md`, and
   `docs/mlfp-self-boot-readiness.md` with bounded evidence for the three exact
   named recursive-ADT source modules and explicit non-claims for full parser
   parity, checker/resolver/backend, compiler-package, platform, proof, and
   self-boot completion.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`

The focused Hspec group is the aggregate parser/conformance run for this owner
surface. It compiles the test suite, runs canonical projection checks, runs the
generated public CLI parser batch, covers the selected named-module negative
evidence, and exercises the shared shortcut/static guards.

Do not run full closeout gates for this round unless implementation widens
beyond the selected parser slice. This plan does not authorize milestone
closeout, production parser replacement, checker policy changes, platform
work, compiler-package work, or proof claims. Run
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
