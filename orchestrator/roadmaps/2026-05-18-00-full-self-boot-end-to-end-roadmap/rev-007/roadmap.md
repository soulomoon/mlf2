# Full Self-Boot End-to-End Roadmap

## Goal

Make `mlf2` capable of proving a small, real self-boot path: the current
Haskell implementation remains the trusted compiler while the project builds a
paper-faithful `.mlfp` compiler source package, runs it through the same
package/conformance substrate, and records comparable semantic proof evidence.

## Alignment Summary

- Source of truth remains `papers/these-finale-english.txt`.
- The roadmap follows the accepted staged order in
  `docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`.
- Compatibility with retired one-file or legacy parser behavior is not a goal.
- Shared conformance evidence must be reusable by the Haskell compiler and the
  future `.mlfp` compiler.
- Trusted substrate must be explicit, versioned, fingerprinted, and shared
  through contracts rather than hidden host behavior.
- Control-plane process follows the latest orchestrator contract: structured
  Markdown artifacts are authoritative, simple tasks are planner-completed, and
  verification profile is separate from task complexity.
- Current compiler-seed and bounded parser-parity evidence shows syntax is
  sufficient for a correctness seed, but not yet pleasant enough for a
  maintainable full self-host compiler source path.
- The immediate parser milestone bottleneck is reusable ergonomics and library
  substrate for the compiler-seed/full-parser path, not raw core syntax
  expressibility.

## Outcome Boundaries

- In scope: readiness ledger, shared conformance corpus, native-capable text
  substrate, compiler-seed/parser ergonomics substrate, canonical `.mlfp`
  parser parity, platform contract substrate, compiler source package, small
  self-boot driver, and first proof evidence.
- Out of scope: preserving legacy parser aliases, reopening settled ADR
  decisions, adding broad compatibility bridges, treating bounded
  compiler-seed/parser-parity fixtures as a full compiler package, or claiming
  full independent self-hosting before the staged proof exists.
- Every completion claim must name the layer it proves: source checking,
  interpreter/runtime, backend/native, object code, package build mode, or
  proof comparison.
- Parser and checker tasks should batch closely related simple slices when they
  share owner surface, verification commands, and failure mode.

## Global Sequencing Rules

- Milestones execute in listed dependency order unless all named dependencies
  are already `[done]`.
- A milestone may become `[in-progress]` only when all dependencies are done and
  at least one selected direction has current preconditions satisfied.
- A milestone may become `[done]` only through status-only closeout evidence
  recorded in `implementation-notes.md` for simple work or `review.md` for
  non-simple work.
- Semantic changes to future sequencing, milestone meaning, or verification
  policy require delegated `update-roadmap`; controller-owned closeout may only
  update status markers, completion pointers, and compact history.
- Planner decides `Complexity` from task content only using
  `orchestrator/active-roadmap-bundle.md` Round Execution Profiles. It chooses
  `Verification profile` separately from surrounding risk and evidence needs.
- Milestone 4 may select ergonomics/library substrate rounds when bounded
  seed/parity evidence shows the selected source forms are expressible but too
  costly to maintain. Such rounds remain milestone-4 parser-path work and do
  not unlock platform, compiler-package, driver, native/backend, proof, or
  self-boot work by themselves.

## Parallel Lanes

- `readiness`: readiness ledgers, ADR alignment, and proof taxonomy.
- `conformance`: shared file-based fixture corpus and normalization policy.
- `text-parser`: broad native-capable text substrate, parser/compiler-frontend
  ergonomics substrate, and canonical parser parity.
- `platform`: platform ABI, substrate, runtime, and package build contracts.
- `compiler-package`: `.mlfp` compiler source package and frontend driver.
- `proof`: staged self-boot command, execution, comparison, and evidence.

## Milestones

### [done] Readiness Ledger Baseline
Milestone id: milestone-1-readiness-ledger-baseline
Depends on:
Intent: Establish the initial full-self-boot readiness ledger, layer taxonomy,
and acceptance language before implementation work widens.
Completion signal: Readiness claims are layer-separated and linked to the
accepted full-self-boot ADR.
Completion pointers: round-258 completed the readiness ledger baseline and
established the layer-separated self-boot readiness vocabulary.
Parallel lane: readiness
Coordination notes: This milestone is closed. Future readiness changes should
be selected only when they support a later milestone's proof obligation.

Candidate directions:
- Direction id: direction-1a-readiness-ledger
  Summary: Keep the readiness ledger aligned with current self-boot evidence.
  Why it matters now: Later proof claims depend on stable layer terminology.
  Preconditions:
  Parallel hints: Can run beside conformance work when it does not alter
    sequencing.
  Boundary notes: Do not claim compiler-in-`.mlfp` readiness from Haskell-only
    tests.
  Extraction notes: Use only for narrow ledger or docs adjustments.

### [done] Shared File-Based Conformance Corpus
Milestone id: milestone-2-shared-file-based-conformance-corpus
Depends on: milestone-1-readiness-ledger-baseline
Intent: Move compiler-facing examples into a shared file-based conformance
corpus with explicit metadata and committed expected outputs.
Completion signal: `test/conformance/mlfp/` is usable as a shared oracle by the
current Haskell compiler and a future `.mlfp` compiler.
Completion pointers: rounds 259-264 completed the shared conformance corpus
migration, metadata policy, expected-output policy, and closeout.
Parallel lane: conformance
Coordination notes: This milestone is closed. Future fixture changes should be
selected as parser, package, platform, or proof work according to the behavior
being proved.

Candidate directions:
- Direction id: direction-2a-shared-conformance-corpus
  Summary: Maintain fixture metadata and expected-output policy as shared
    compiler oracle infrastructure.
  Why it matters now: Later parser, package, and proof stages reuse this corpus
    instead of inventing separate test sources.
  Preconditions: milestone-1-readiness-ledger-baseline
  Parallel hints: Can support focused parser or platform tests when metadata
    changes are behavior-preserving.
  Boundary notes: Fixture metadata may classify unsupported stages, but must
    not hide implementation failures behind ad hoc skips.
  Extraction notes: Use only for corpus maintenance needed by active later
    milestones.

### [done] Native-Capable Broad Text Substrate
Milestone id: milestone-3-native-capable-broad-text-substrate
Depends on: milestone-2-shared-file-based-conformance-corpus
Intent: Provide broad Unicode-scalar `Char` and `String` behavior across the
source, interpreter/runtime, backend, object, and native layers needed by
canonical parser parity.
Completion signal: Text behavior required by parser parity is proven through
shared tests across the selected layers, not parser-private helpers.
Completion pointers: rounds 265-303 completed the broad text substrate, whole
string-library work, native-facing behavior, and closeout needed before
canonical parser parity.
Parallel lane: text-parser
Coordination notes: This milestone is closed. New text work should be selected
only when parser parity, platform substrate, or proof evidence exposes a
current gap.

Candidate directions:
- Direction id: direction-3a-broad-text-substrate
  Summary: Maintain native-capable text behavior as shared language
    infrastructure.
  Why it matters now: Parser parity relies on real source strings and
    characters, not helper-only text behavior.
  Preconditions: milestone-2-shared-file-based-conformance-corpus
  Parallel hints: Can run with parser work only when the selected parser slice
    already has clear failing evidence.
  Boundary notes: Parser-private text helpers do not satisfy this milestone.
  Extraction notes: Use only for focused text regressions blocking active
    parser or proof slices.

### [in-progress] Full Canonical .mlfp Parser Parity
Milestone id: milestone-4-full-canonical-mlfp-parser-parity
Depends on: milestone-3-native-capable-broad-text-substrate
Intent: Complete the bridge from bounded parser-parity/compiler-seed syntax
evidence to a maintainable full canonical `.mlfp` parser path by building the
missing parser-facing ergonomics/library substrate and then proving required
accept/reject parity for compiler-source package inputs.
Completion signal: Parser-owned `.mlfp` source-text parsing covers the required
canonical syntax with reusable string/char/stream/list, parser
state/result/combinator, source-span, and diagnostic helpers; focused
conformance evidence includes positive projections, meaningful negative cases,
and the compiler-seed/full-parser source shapes needed before later platform
or compiler-package milestones can start.
Completion pointers: rounds 304-330 completed the parser parity tracers,
parser library consolidation, source front-door routing, shared parser-library
extensions, checker-facing single-program-run policy, and recursive tree
parser parity slices currently merged into `master`. Rounds 331-339 further
extend bounded evidence across recursive ADT/typeclass, recursive program,
package-source layout, and compiler-seed data-model/lexer sources, including
round-339 `SeedLexer.mlfp` equality, source-copy, shortcut-guard,
malformed-case negative, and aggregate parser Hspec evidence; this is bounded
evidence only, not full parser parity, compiler-package implementation,
platform/proof progress, or self-boot completion.
Parallel lane: text-parser
Coordination notes: The next lawful work may target reusable ergonomics and
library substrate for the compiler-seed/full-parser path instead of another
core syntax expressibility slice. Planner may select parser-combinator
helpers, string/char/stream/list APIs, source-span/diagnostic helpers, or
case/lambda boilerplate reduction when the slice strengthens the reusable
parser/compiler-frontend substrate. Module-body and declaration-internal
exact-count parser growth are transitional evidence patterns only; prefer
recursive declaration sequencing that parses supported module-body
declarations until the module close token, and prefer recursive data
constructor-list sequencing over adding more fixture-shaped constructor
ladders. Preserve bounded parser parity as evidence and regression guard
material; do not treat it as milestone closeout.

Candidate directions:
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
  Summary: Build the reusable ergonomics and library substrate needed to make
    the compiler-seed/full-parser path maintainable in `.mlfp`.
  Why it matters now: Current evidence says selected compiler-seed syntax can
    check, run, and pass bounded parser-parity assertions, but larger parser
    modules become painful around long application chains, nested case/lambda
    plumbing, token streams, source spans, and diagnostics. The next bottleneck
    is substrate quality, not raw core syntax expressibility.
  Preconditions: milestone-3-native-capable-broad-text-substrate
  Parallel hints: Substrate slices may be selected and batched when owner
    surface, verification command, and failure mode are shared. Planner still
    classifies `Complexity` from the task content: established helper
    extension patterns may be simple, while new abstractions or cross-owner
    behavior remain standard.
  Boundary notes: Do not add compatibility aliases, fixture-name shortcuts,
    pre-rendered projections, canonical-parser bypasses, retired syntax shims,
    or parser-private hacks that hide missing shared substrate. Do not start or
    claim compiler-package, platform, driver, native/backend, proof, package
    manager, linker, or self-boot completion work.
  Extraction notes: Prefer one strategic substrate slice at a time, such as
    parser state/result helpers, parser-combinator helpers, stronger
    string/char/stream/list APIs, source-span/diagnostic helper payloads, or a
    narrowly justified reduction of repeated case/lambda plumbing. For
    parser-owned module bodies, keep recursive declaration-row sequencing over
    already-supported `data`, `class`, `instance`, and `def` rows until `}` as
    the body-level substrate. For declaration internals, prefer focused
    recursive slices such as data constructor-list sequencing before adding any
    new exact-count wrappers; class and instance method-row internals may remain
    bounded until their own focused slice. Keep the round plan concrete; this
    roadmap direction is not itself an implementation design.

- Direction id: direction-4a-canonical-parser-parity
  Summary: Finish remaining canonical parser accept/reject parity for
    package-capable `.mlfp` sources after the reusable substrate is adequate.
  Why it matters now: Compiler-in-`.mlfp` work cannot start honestly until the
    canonical parser covers source package syntax, but current evidence shows
    the immediate blocker is maintainable parser/compiler-frontend substrate
    rather than proof that more core syntax is expressible.
  Preconditions: milestone-3-native-capable-broad-text-substrate and enough
    direction-4b substrate to avoid fixture-shaped parser-library growth.
  Parallel hints: Simple fixture/library slices may be planner-completed and
    batched, including shared parser-library changes that follow established
    parser paths and have clear verification boundaries. Parser owner changes
    go through implementer and reviewer only when the task content itself is
    non-simple under the active Round Execution Profiles.
  Boundary notes: Do not add compatibility aliases for retired syntax. Do not
    satisfy parser parity by bypassing the canonical parser. Do not close this
    milestone from bounded parser-parity fixtures alone.
  Extraction notes: Use after the selected source family can be covered through
    reusable substrate rather than one-off fixture growth; verify by one
    aggregate parser run when owner surface and failure mode are shared. Do not
    extend canonical parser parity by adding new exact-count module-body or data
    constructor-list wrappers when the same coverage can enter through recursive
    sequencing.

### [pending] Self-Boot Platform Contract Implementation
Milestone id: milestone-5-self-boot-platform-contract-implementation
Depends on: milestone-4-full-canonical-mlfp-parser-parity
Intent: Implement the trusted platform contract substrate needed by both
compiler implementations.
Completion signal: Platform ABI, substrate identity, toolchain identity, lock
validation, native link records, native execution records, and ambient-input
policy are explicit and test-backed for selected slices.
Completion pointers: none
Parallel lane: platform
Coordination notes: Do not start until parser parity is done unless a delegated
roadmap update proves a dependency split is needed.

Candidate directions:
- Direction id: direction-5a-platform-contract-substrate
  Summary: Build the explicit substrate and ABI contract required for staged
    self-boot execution.
  Why it matters now: Proof evidence must distinguish trusted capabilities from
    compiler semantics.
  Preconditions: milestone-4-full-canonical-mlfp-parser-parity
  Parallel hints: Can split by substrate identity, ABI drift checks, lock
    validation, toolchain identity, and native execution records when their
    ownership boundaries do not overlap.
  Boundary notes: Trusted substrate provides capabilities, not compiler
    semantics.
  Extraction notes: Select slices that add one contract surface plus focused
    drift or execution evidence.

### [pending] Compiler Source Package In .mlfp
Milestone id: milestone-6-compiler-source-package-in-mlfp
Depends on: milestone-5-self-boot-platform-contract-implementation
Intent: Create the small compiler source package in `.mlfp` using the shared
package, parser, and platform contracts.
Completion signal: The `.mlfp` package is source-checked and interpreter-run
through declared package roots with locked local identities and shared
conformance evidence.
Completion pointers: none
Parallel lane: compiler-package
Coordination notes: Keep unsupported backend/native behavior fail-closed until
platform and proof directions authorize it.

Candidate directions:
- Direction id: direction-6a-compiler-source-package
  Summary: Implement the minimal compiler source package as ordinary package
    mode `.mlfp` modules.
  Why it matters now: The first proof needs a real compiler source package, not
    a one-file compatibility path.
  Preconditions: milestone-5-self-boot-platform-contract-implementation
  Parallel hints: Can split frontend seed, package metadata, local package
    identity, and conformance wiring when interfaces are stable.
  Boundary notes: Do not reintroduce a separate one-file semantic mode.
  Extraction notes: Select vertical slices that make a package module checkable
    or runnable through existing package infrastructure.

### [pending] Small Real Self-Boot Driver
Milestone id: milestone-7-small-real-self-boot-driver
Depends on: milestone-6-compiler-source-package-in-mlfp
Intent: Build the staged driver that runs the `.mlfp` compiler package under
declared substrate and package identities.
Completion signal: Stage commands, output directories, generated artifacts,
link records, native execution records, and conformance outputs are stage-owned
and reproducible.
Completion pointers: none
Parallel lane: proof
Coordination notes: The driver must not share generated semantic, backend,
object, executable, link, native-execution, or conformance-output caches across
stages.

Candidate directions:
- Direction id: direction-7a-small-self-boot-driver
  Summary: Implement the small staged driver and evidence capture path.
  Why it matters now: The proof needs reproducible staged commands before it
    can compare outputs.
  Preconditions: milestone-6-compiler-source-package-in-mlfp
  Parallel hints: Can split command rendering, output isolation, link record,
    native execution record, and conformance record work when the driver
    boundary is stable.
  Boundary notes: No cross-stage cache reuse for proof artifacts.
  Extraction notes: Select slices with a clear action id and machine-readable
    evidence artifact.

### [pending] First Self-Boot Proof
Milestone id: milestone-8-first-self-boot-proof
Depends on: milestone-7-small-real-self-boot-driver
Intent: Produce the first conformance-first self-boot proof evidence for the
small compiler package.
Completion signal: Both stages pass the declared suite, normalized semantic
artifacts compare under the proof policy, native/object artifacts are
regenerated and recorded, and failures classify through the proof taxonomy.
Completion pointers: none
Parallel lane: proof
Coordination notes: Native object and executable bytes are recorded evidence,
not the primary first-proof equality oracle.

Candidate directions:
- Direction id: direction-8a-first-self-boot-proof
  Summary: Run and record the first staged self-boot proof.
  Why it matters now: This is the roadmap's terminal evidence for a small real
    self-boot path.
  Preconditions: milestone-7-small-real-self-boot-driver
  Parallel hints: Proof comparison, failure taxonomy, and evidence packaging
    can split only after the driver emits stable records.
  Boundary notes: Do not claim full independent self-hosting beyond the staged
    evidence produced.
  Extraction notes: Select slices that produce or validate one proof action
    with stable artifact paths.
