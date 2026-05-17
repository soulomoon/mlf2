# MLFP Compiler Frontend Seed Roadmap

Roadmap family: `2026-05-17-01-mlfp-compiler-frontend-seed-roadmap`
Revision: `rev-001`
Base branch: `master`
Created: 2026-05-17
Contract: `orchestrator-v2`

## Goal

Open the first compiler-in-`.mlfp` prerequisites family after the package
substrate is complete. This family should create an interpreter-runnable
frontend seed in `.mlfp`: compiler source package shape, token/span/diagnostic
data types, a minimal lexer/parser surface, and evidence that the selected
seed runs through the current `.mlfp` interpreter path.

This is not a self-hosting claim. The family deliberately stops before a
self-hosted checker, optimizer, backend, package manager, stable ABI, linker,
or full compiler driver.

## Alignment Summary

- Approved strategy: interpreter-first frontend seed.
- Approved first success boundary: `.mlfp` compiler-facing source should begin
  with lexer/parser data types plus a tiny parser/diagnostic surface.
- Approved execution boundary: the seed must typecheck and run through the
  current `.mlfp` interpreter path. Backend/native support is classified by
  layer instead of forced into the first slice.
- Gate posture: warning-free `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` are clean at scaffold time and must
  stay clean for behavior-changing closeout.
- Chosen strategy: establish a compiler source package and frontend seed
  contract first, implement the smallest meaningful token/lexer/parser proof,
  then classify the exact primitive, stdlib, runtime, backend/native, and
  tooling gaps exposed by that proof.
- Success means the repository has executable `.mlfp` compiler-source evidence
  for a frontend seed and a precise, layer-separated gap ledger for the next
  compiler-in-`.mlfp` family.
- Preserve package-mode semantics from the prior roadmap: compiler source
  modules are ordinary local `.mlfp` package modules, not a second source model
  or a compatibility path.
- Deferred alternatives: an inventory-only family, a native-constrained seed,
  and a tiny end-to-end compiler slice are out of scope for this family unless
  a semantic roadmap update explicitly changes direction.

## Outcome Boundaries

- In scope:
  - package-mode source layout for compiler seed modules;
  - a stable compiler-frontend seed contract owned by `.mlfp` source modules;
  - token, source-position/span, diagnostic, and small AST data models in
    `.mlfp`;
  - a minimal lexer/tokenization proof over the selected bounded input
    representation;
  - a minimal parser proof over the selected token stream;
  - focused interpreter-run fixtures for successful and failing frontend seed
    cases;
  - primitive and stdlib gap classification driven by the actual seed;
  - runtime, backend/native, object-code, and package-build classification for
    compiler-source modules; and
  - docs and readiness evidence that state what the seed proves and what
    remains.
- Out of scope:
  - implementing the `.mlfp` checker in `.mlfp`;
  - optimizer, backend, code generator, linker, package manager, or full driver
    implementation in `.mlfp`;
  - stable `.mlfp` ABI, persisted interface format, separate object build, or
    object-code contract;
  - broad primitive or stdlib expansion not justified by the selected frontend
    seed;
  - forcing the first frontend seed to be native-lowerable;
  - preserving old one-file semantics as a separate compiler-source model;
  - new public backend IR layers, lazy-runtime/STG machinery, or broad FFI
    scope.
- Public APIs under `src-public/` may change only when selected seed evidence
  proves the package-facing public entrypoint needs it. Prefer internal owners
  and test-support seams over widening production facades.
- Any support claim must be separated by layer: source checking,
  interpreter/runtime, backend/native, object code, package build mode, and
  compiler-in-`.mlfp` implementation.
- Status-only closeout may update milestone statuses and compact completion
  pointers only. Changes to seed scope, native policy, primitive budget,
  verification meaning, or compatibility posture require semantic roadmap
  update.

## Global Sequencing Rules

- Default sequencing is serial. The frontend seed contract must exist before
  gap inventory and native classification can be trusted.
- Milestone 1 establishes package location, ownership, and execution harness
  for compiler source modules.
- Milestone 2 defines token/span/diagnostic data and a minimal lexer proof
  against the selected bounded input representation.
- Milestone 3 defines the parser seed and AST contract after token behavior is
  stable.
- Milestone 4 converts concrete seed pain into a primitive and stdlib budget.
  Do not add general-purpose convenience APIs without seed evidence.
- Milestone 5 classifies interpreter-only, backend-native, object-code, and
  package-build behavior after the parser seed and gap budget exist.
- Milestone 6 updates fixtures, docs, readiness ledger, and next-family handoff
  only after the runnable frontend seed and layer classification are coherent.
- If a round discovers that the selected seed cannot run through the interpreter
  without broad language work, produce a semantic roadmap update instead of
  silently widening the round.

## Parallel Lanes

- `lane-compiler-seed`: compiler source package, token/span/diagnostic data,
  lexer seed, parser seed, AST contract, and interpreter fixtures.
- `lane-gap-budget`: primitive, stdlib, text/byte/collection/error, and IO
  gaps discovered by the frontend seed.
- `lane-layer-classification`: interpreter/runtime, backend/native, object
  code, and package-build classification for compiler-source modules.
- `lane-docs`: language-reference, self-boot readiness, and architecture
  wording that records produced evidence without overclaiming.

The controller keeps `max_parallel_rounds` at `1`. These lanes are ownership
hints for planners; they are not permission to run concurrent rounds unless
controller state and a selected plan explicitly allow it.

## Milestones

### [done] 1. Compiler Source Package And Seed Contract

- Milestone id: `milestone-1`
- Depends on: none
- Intent: create the package-mode home for compiler-in-`.mlfp` frontend source
  modules and define the interpreter-run proof contract for the seed.
- Completion signal: compiler seed source modules live under an intentional
  package/fixture location; focused tests or scripted fixtures prove the seed
  package is checked and run through the interpreter path; docs identify the
  seed owner, input fixture policy, and non-goals; required validation passes.
- Parallel lane: `lane-compiler-seed`
- Coordination notes: start from the package substrate owners and current
  `run-program` behavior. Do not create a parallel compiler-source loader or a
  one-file compatibility path.

#### Completion Pointers: milestone-1

- round-252 added the compiler-seed package fixture and ProgramCompilerSeedSpec evidence: discovery, check, interpreter output, CLI run-program, full gates green.

#### Candidate Direction: Compiler Source Package Contract

- Direction id: `direction-1a-compiler-source-package-contract`
- Summary: establish the `.mlfp` package, fixture, and test harness used by
  compiler frontend seed modules.
- Why it matters now: later lexer/parser code needs one ordinary package-mode
  home before the roadmap can distinguish missing language support from missing
  project scaffolding.
- Preconditions: inspect package-mode CLI/API entrypoints, current package
  fixtures, `docs/mlfp-self-boot-readiness.md`, `docs/mlfp-language-reference.md`,
  `MLF.Program.CLI`, and focused program-run tests at HEAD.
- Parallel hints: serial foundation work. Do not fan out before the seed source
  contract is selected.
- Boundary notes: no checker, optimizer, backend, package manager, ABI, linker,
  or driver scope.
- Extraction notes: a lawful first round may add only the seed package/harness
  and a tiny interpreter-run smoke if that fixes the source boundary.

### [done] 2. Token, Span, Diagnostic, And Lexer Seed

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: implement the smallest `.mlfp` token/source-span/diagnostic data
  model and lexer proof needed for a frontend seed.
- Completion signal: `.mlfp` seed code can produce tokens or diagnostics for
  bounded positive and negative fixtures through the interpreter; source
  positions/spans are represented intentionally; missing character, string,
  byte, list, or error APIs are recorded as concrete gaps rather than hidden in
  helper code; required validation passes.
- Parallel lane: `lane-compiler-seed`
- Coordination notes: the selected bounded input representation must be
  explicit. If real source-text lexing is blocked by primitives, keep the
  runnable seed honest and record the primitive gap.

#### Completion Pointers: milestone-2

- round-253 added bounded lexer seed evidence: token/span/diagnostic/result ADTs, positive token path, negative diagnostic path, full gates green.

#### Candidate Direction: Lexer Token Diagnostic Seed

- Direction id: `direction-2a-lexer-token-diagnostic-seed`
- Summary: define frontend seed ADTs and implement a minimal lexer/tokenization
  proof over the selected input representation.
- Why it matters now: parser work needs stable token and diagnostic contracts,
  and self-boot planning needs evidence from code written in `.mlfp`.
- Preconditions: inspect Prelude list/string/Nat support, primitive inventory,
  interpreter IO/value rendering behavior, fixture shape, and current parser
  diagnostics.
- Parallel hints: serial with milestone 3 until the token contract settles.
  Gap-budget documentation may split only after concrete missing APIs are
  observed.
- Boundary notes: do not add broad text/byte APIs or backend lowering work
  unless a semantic roadmap update authorizes that scope.
- Extraction notes: a lawful round can target a tiny grammar input such as
  identifiers, delimiters, and one definition form before wider syntax.

### [done] 3. Parser Seed And AST Contract

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: add the minimal parser seed and AST/result contract that can consume
  the seed token stream and return structured success or diagnostic results.
- Completion signal: `.mlfp` parser seed fixtures cover at least one accepted
  form and at least one rejected form; AST and error values are inspectable
  through interpreter-run evidence; parser tests remain real assertions rather
  than smoke checks; required validation passes.
- Parallel lane: `lane-compiler-seed`
- Coordination notes: keep the grammar deliberately tiny. Prefer a stable
  parser result contract over expanding syntax breadth.

#### Completion Pointers: milestone-3

- round-254 added bounded parser/AST seed evidence: token-stream parser, accepted AST path, rejected diagnostic path, full gates green.

#### Candidate Direction: Minimal Parser Seed

- Direction id: `direction-3a-minimal-parser-seed`
- Summary: implement a parser over the seed token stream and prove it returns
  a small AST or diagnostic result through interpreter execution.
- Why it matters now: this is the first executable compiler-facing `.mlfp`
  frontend proof, and it exposes the real support gaps for future self-boot
  work.
- Preconditions: token contract from milestone 2, package seed harness from
  milestone 1, current `.mlfp` ADT/case/typeclass behavior, and focused
  interpreter-run fixture expectations.
- Parallel hints: serial until the parser result contract is stable. Docs can
  update after runnable evidence exists.
- Boundary notes: do not attempt a full `.mlfp` grammar, checker handoff, or
  Haskell parser replacement in this milestone.
- Extraction notes: a lawful round may implement only one parseable definition
  or expression shape plus one diagnostic path.

### [done] 4. Primitive And Standard Library Gap Budget

- Milestone id: `milestone-4`
- Depends on: `milestone-2`, `milestone-3`
- Intent: turn concrete frontend seed needs into a reviewed primitive and
  standard-library budget.
- Completion signal: gaps for text/characters or bytes, collection operations,
  maps/sets, parser helpers, error accumulation, IO helpers, and diagnostics
  are classified by necessity and layer; no wishlist item is accepted without
  evidence from the runnable seed; docs identify what must be added before the
  next compiler-in-`.mlfp` family; required validation passes.
- Parallel lane: `lane-gap-budget`
- Coordination notes: this milestone should inventory and prioritize. It
  should implement only narrow APIs when the selected round proves they are
  blockers to the already-approved seed.

#### Completion Pointers: milestone-4

- round-255 added the seed-driven primitive/stdlib gap budget: all required categories classified, no broad APIs added, full gates green.

#### Candidate Direction: Seed Driven Primitive Budget

- Direction id: `direction-4a-seed-driven-primitive-budget`
- Summary: classify every primitive and stdlib gap exposed by the lexer/parser
  seed and decide which gaps must be solved before a larger compiler component.
- Why it matters now: compiler work needs text, collection, diagnostic, and IO
  support, but the project should not grow broad convenience layers without
  executable demand.
- Preconditions: runnable lexer/parser seed evidence, current Prelude source,
  primitive inventory, runtime IO support, backend/native primitive support,
  and self-boot readiness ledger.
- Parallel hints: can run after milestone 3, and can be planned independently
  from native/backend classification if ownership stays disjoint.
- Boundary notes: do not implement a package manager, persisted ABI, separate
  compilation, broad FFI, or native runtime redesign here.
- Extraction notes: a lawful round may produce only the budget ledger and
  focused no-change decisions if the seed already has enough support.

### [done] 5. Runtime And Backend Layer Classification

- Milestone id: `milestone-5`
- Depends on: `milestone-3`, `milestone-4`
- Intent: classify the compiler frontend seed by execution layer and preserve
  the interpreter-first boundary while documenting backend/native fail-closed
  behavior.
- Completion signal: each compiler seed module is classified as
  source-checked, interpreter-runnable, backend-lowerable, native-runnable, or
  explicitly not lowerable; native/backend diagnostics are tested or documented
  where the seed crosses unsupported shapes; required validation passes.
- Parallel lane: `lane-layer-classification`
- Coordination notes: native support may be extended only for selected seed
  shapes. Unsupported compiler workloads should fail closed with clear
  diagnostics rather than silently degrading.

#### Completion Pointers: milestone-5

- round-256 classified all seed modules and package root by layer; backend/native/object evidence and full gates passed.

#### Candidate Direction: Interpreter Native Boundary Ledger

- Direction id: `direction-5a-interpreter-native-boundary-ledger`
- Summary: record and test how the frontend seed behaves across source
  checking, interpreter/runtime, backend/native, object-code, and package-build
  layers.
- Why it matters now: the roadmap chose interpreter-first execution, so future
  planners need exact native/backend boundaries before selecting deeper
  compiler slices.
- Preconditions: parser seed result contract, primitive/stdlib budget, backend
  emission tests, native IO/runtime docs, and current fail-closed diagnostics.
- Parallel hints: can run after milestone 4, alongside docs work, if it does
  not change the seed contract.
- Boundary notes: do not force parser data structures into the native subset
  or add a second public backend IR.
- Extraction notes: a lawful round may add only classification tests and docs
  if the seed remains intentionally interpreter-only.

### [pending] 6. Seed Fixtures, Docs, And Next-Stage Handoff

- Milestone id: `milestone-6`
- Depends on: `milestone-4`, `milestone-5`
- Intent: close the family by preserving runnable seed fixtures, updating
  readiness docs, and handing off the next compiler-in-`.mlfp` family boundary.
- Completion signal: seed fixtures are stable and documented; README,
  language-reference, architecture, and self-boot readiness docs describe the
  frontend seed without claiming self-hosting; next-family recommendation is
  explicit; full required validation passes.
- Parallel lane: `lane-docs`
- Coordination notes: do not treat docs closeout as implementation proof. Every
  readiness claim must point to source, fixtures, tests, or recorded
  fail-closed behavior.

#### Completion Pointers: milestone-6

- None yet.

#### Candidate Direction: Frontend Seed Handoff

- Direction id: `direction-6a-frontend-seed-handoff`
- Summary: update fixture evidence, docs, and the readiness ledger so the next
  roadmap can choose a larger compiler component without re-litigating this
  seed.
- Why it matters now: the family should end with a clear executable proof and
  a precise statement of what remains before self-boot can advance.
- Preconditions: completed seed, primitive/stdlib gap budget, layer
  classification, current README/language-reference/architecture docs, and
  clean baseline gates.
- Parallel hints: docs and fixture polish may be extracted separately after
  milestones 4 and 5 are done.
- Boundary notes: no full self-boot claim, no checker/backend implementation,
  and no stable ABI/linker/separate-compilation claim.
- Extraction notes: a lawful closeout round may be docs plus fixtures only if
  all implementation evidence already exists.
