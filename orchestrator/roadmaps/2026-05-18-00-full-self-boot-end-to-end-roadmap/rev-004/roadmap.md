# Full Self-Boot End-to-End Roadmap

Roadmap family: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
Revision: `rev-004`
Base branch: `master`
Created: 2026-05-18
Contract: `orchestrator-v2`

## Goal

Drive the entire Full Self-Boot effort in one control-plane family. This
family turns the accepted staged order in
`docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md` into executable
milestones, from current-readiness evidence through shared conformance,
broad native text support, parser parity, platform contracts, the `.mlfp`
compiler source package, the first self-boot driver, and the first
conformance-first two-stage proof.

Full Self-Boot means the `.mlfp` compiler source package can rebuild itself
under shared platform contracts. It is not whole-platform self-hosting, and it
is not a compiler-only rewrite.

## Alignment Summary

- Approved strategy: one end-to-end control-plane family containing the full
  self-boot staged effort.
- Thesis: self-boot is meaningful only when compiler behavior, platform
  contracts, local packages, native execution, and proof records are staged
  together under one ordering that prevents premature self-hosting claims.
- Success criteria: the repo has a shared file-based compiler conformance
  corpus, a native-capable initial Broad String Library completion contract,
  full canonical `.mlfp` parser parity, implemented platform
  ABI/runtime/GC/FFI/package/toolchain contracts, a compiler source package in
  `.mlfp`, a bounded first compiler driver, and a first proof run that passes
  stage-shared conformance before comparing normalized semantic artifacts.
- Non-goals: do not claim whole-platform self-hosting, remote package solving,
  package publishing, REPL support, formatting, documentation generation,
  optimizer controls, or byte-for-byte native object/executable equality for
  the first proof.
- Architecture posture: compiler semantics belong to the `.mlfp` compiler
  package. Trusted substrate may provide runtime, GC, FFI, filesystem,
  subprocess, hashing, linker, and low-level primitive capabilities, but it
  must not decide parser behavior, name resolution, checking, semantic
  interface behavior, backend artifact decisions, diagnostics, package
  validation, or driver semantics.
- Compatibility posture: backwards compatibility is not a default goal.
  Existing behavior is preserved where this roadmap migrates evidence; changes
  are allowed when they fix a real bug or implement an accepted self-boot
  contract.
- Verification standard: every behavior-changing implementation round uses the
  `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` and proceeds in
  vertical RED -> GREEN -> refactor cycles. Each cycle starts with one
  public-interface behavior test, proves it fails, implements only enough code
  to pass, then refactors while the focused slice stays green. Focused tests
  still run before `cabal build all && cabal test` closeout. Rounds touching
  thesis obligations, language semantics, self-boot readiness, proof evidence,
  or platform contracts also run `./scripts/thesis-conformance-gate.sh`. Pure
  docs-only, control-plane-only, review-only, semantic roadmap-update, and
  status-only closeout rounds do not require TDD, but must still run the
  applicable diff hygiene and claim-audit checks.
- Sequencing strategy: milestone order follows the ADR. The controller may not
  skip to compiler implementation or proof work before conformance, text/parser,
  and platform prerequisites have landed or a semantic roadmap update changes
  the accepted order.
- Concurrency posture: keep controller execution serial with
  `max_parallel_rounds = 1`. Parallel lanes identify ownership boundaries for
  planners, not default concurrent execution.
- Risk posture: prevent overclaims, divergent oracles, hidden Haskell-only
  compiler behavior, host-owned proof formats, unchecked platform drift,
  native-only blind spots, cross-stage cache reuse, ambient input drift, and
  proof records that cannot localize failure.
- Deferred alternatives: whole-platform self-hosting, compiler-only rewrite,
  parser-before-text shortcut, byte-equality native proof, remote package
  solver, host-owned canonical proof formats, and proof-runner-owned compiler
  semantics are rejected by the accepted ADRs.

## Outcome Boundaries

- In scope:
  - living readiness ledger maintenance tied to current evidence;
  - Shared File-Based Compiler Conformance Corpus under `test/conformance/mlfp/`;
  - broad Unicode-scalar `Char` and `String` library with native slicing,
    classification, search, formatting, parser-needed cursor behavior, and
    exact native metadata for string-producing helpers;
  - full canonical `.mlfp` parser parity built on a parser-owned combinator
    library, explicit parser-monad sequencing, and the broad text substrate;
  - stable target-scoped public `.mlfp` ABI, managed GC contract, shared Rust
    trusted substrate path, generated binding validation, package manifests,
    checked locks, host toolchain identity, native/link records, filesystem,
    subprocess, hash, and ambient-input policy;
  - `.mlfp` compiler source package under the self-boot compiler semantics
    boundary;
  - small first compiler driver with `check`, `emit-backend`, `emit-native`,
    and `run-conformance`;
  - conformance-first stage sequence and normalized semantic artifact
    comparison.
- Out of scope:
  - whole-platform self-hosting;
  - remote package publishing or dependency solving;
  - REPL, formatter, documentation generator, and optimizer controls unless a
    semantic roadmap update admits them;
  - deterministic native object or executable byte equality as first-proof
    evidence;
  - lazy-runtime/STG machinery, duplicate public backend IR layers, or old
    one-file `.mlfp` semantics as a separate compiler-source mode.
- First proof permits trusted substrate, but the substrate must be explicit,
  versioned, fingerprinted, shared by both compiler implementations, and
  reached through stable contracts.
- Stage-inapplicable conformance fixtures must be declared in metadata. Ad hoc
  skips are not valid proof behavior.
- Status-only closeout may update milestone statuses and compact completion
  pointers only. Changes to milestone meaning, stage order, proof oracle,
  platform contract, compiler-semantics boundary, or verification meaning
  require a semantic roadmap update.

## Global Sequencing Rules

- Default execution is serial.
- Behavior-changing implementation rounds must use the `tdd` skill at
  `/Users/ares/.agents/skills/tdd/SKILL.md` and proceed behavior-first through
  vertical RED -> GREEN -> refactor cycles. Do not plan horizontal slices that
  write all tests first, implement broad layers before behavior evidence, or
  defer observable behavior until the end. Pure docs-only, control-plane-only,
  review-only, semantic roadmap-update, and status-only closeout rounds are
  exempt from the TDD workflow.
- Milestone 1 maintains the readiness ledger as current evidence and aligns
  docs with the accepted end-to-end ordering.
- Milestone 2 creates the shared file-based conformance corpus before
  self-boot implementation starts.
- Milestone 3 implements the native-capable initial Broad String Library and
  Unicode scalar `Char` contract before full parser parity. After rev-004, the
  next selected implementation round for this milestone must be the integrated
  whole-library completion item, not another one-function tracer.
- Milestone 4 implements full canonical `.mlfp` parser parity on the broad text
  substrate and does not absorb checker, backend, driver, or package-manager
  work. The parser implementation must be structured as parser-owned
  combinators over an explicit parser monad or equivalent monadic parser state
  abstraction; grammar functions should compose through that abstraction rather
  than manually pattern-matching complete fixture token streams. After the
  active round-309 recovery/closeout decision, the next normal milestone-4
  implementation selection must consolidate the carried parser-parity slices
  into one shared parser-owned `.mlfp` parser library. It must not add another
  fixture-owned parser package that recognizes one exact source file and
  constructs one hardcoded AST.
- Milestone 5 implements platform contracts needed by both compiler
  implementations before the `.mlfp` compiler source package can make a proof
  claim.
- Milestone 6 implements the compiler source package in `.mlfp`, with language
  semantics owned by that package and trusted substrate limited to capabilities.
- Milestone 7 implements the first small compiler driver over explicit local
  manifests and checked locks.
- Milestone 8 runs the first proof sequence: stage 0 produces the native
  stage-1 compiler, stage 1 rebuilds the same locked inputs, both stages pass
  the shared conformance suite, and only then normalized semantic artifacts are
  compared.
- If a round cannot preserve this order, it must request a semantic roadmap
  update instead of widening locally.
- If a dependency-ready milestone becomes too broad for a clean bounded round,
  or exposes newly independent sub-fronts with distinct dependencies,
  verification surfaces, or ownership, split it through a semantic roadmap
  update before executing the wider work. Do not force platform, compiler
  package, or proof work into oversized rounds just because the current
  milestone is coarse.

## Parallel Lanes

- `lane-evidence-conformance`: readiness ledger, conformance corpus, fixture
  metadata, expected outputs, normalization, and stage-shared conformance.
- `lane-text-parser`: broad string/`Char`, source cursor, parser combinators,
  canonical parser parity, and parser diagnostics.
- `lane-platform-substrate`: ABI, GC, FFI, substrate declarations, generated
  bindings, locks, manifests, toolchain, link/native execution records, and
  ambient-input policy.
- `lane-compiler-package`: `.mlfp` compiler source modules for parser, name
  resolution, checking, interfaces, backend decisions, diagnostics, package
  validation, and driver behavior.
- `lane-proof-driver`: first driver modes, stage command records, proof
  manifest, conformance-first gate, and normalized semantic artifact
  comparison.
- `lane-docs`: README, architecture, readiness, ADR cross-links, and closeout
  wording that prevents overclaims.

The controller keeps `max_parallel_rounds` at `1`. These lanes guide future
selection and handoff; they do not authorize concurrent rounds by themselves.

## Milestones

### [done] 1. Readiness Ledger Baseline

- Milestone id: `milestone-1`
- Depends on: none
- Intent: keep current readiness evidence, glossary, ADR links, and support
  claims aligned before implementation starts.
- Completion signal: `docs/mlfp-self-boot-readiness.md`, `CONTEXT.md`, and
  relevant user-facing docs accurately separate current evidence from future
  self-boot obligations, point to the accepted end-to-end ADR, and do not claim
  self-hosting.
- Parallel lane: `lane-docs`
- Coordination notes: this milestone is evidence alignment, not implementation.
  It may be satisfied quickly if the current docs already match the accepted
  ADRs.

#### Completion Pointers: milestone-1

- round-258 (`dcb12bdc`): completed a docs-only readiness audit and alignment
  pass for the Full Self-Boot baseline. The approved review confirmed current
  docs separate bounded compiler-seed evidence from future self-boot
  obligations, point to the accepted end-to-end ADR, do not claim
  self-hosting, and left implementation for later milestones.

#### Candidate Direction: Evidence Ledger Alignment

- Direction id: `direction-1a-evidence-ledger-alignment`
- Summary: verify and align the living readiness ledger, glossary, and docs
  against the full self-boot ADR order.
- Why it matters now: every later round needs a stable distinction between
  current evidence and future obligations.
- Preconditions: inspect `docs/mlfp-self-boot-readiness.md`, `CONTEXT.md`,
  README, accepted 2026-05-18 ADRs, and current seed/package evidence.
- Parallel hints: serial setup. Docs-only corrections can happen before
  behavior work.
- Boundary notes: no implementation scope unless docs expose a real build/test
  defect.
- Extraction notes: a lawful round may be a no-code alignment audit if docs are
  already accurate.

### [done] 2. Shared File-Based Conformance Corpus

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: migrate behavior-level `.mlfp` compiler tests into a shared corpus
  with metadata, committed expected outputs, and documented normalization.
- Completion signal: `test/conformance/mlfp/` has fixture metadata,
  per-command expected files, focused harness checks, explicit regeneration
  workflow, migrated public pass/fail fixtures, and no dynamic golden
  acceptance or ad hoc skips.
- Parallel lane: `lane-evidence-conformance`
- Coordination notes: production compiler changes are allowed only when
  migration exposes a real existing bug.

#### Completion Pointers: milestone-2

- round-259: added the first shared conformance corpus run-program tracer for
  cross-module-let with fixture metadata and committed expected stdout;
  reviewer evidence: focused matcher, cabal build all, cabal test, and thesis
  gate passed.
- round-260: completed item-260-conformance-run-program-search-path-tracer by
  adding a shared conformance corpus run-program search-path tracer with
  metadata-derived package root/search path argv and committed expected stdout;
  evidence: focused conformance tests, cabal build all, cabal test, and thesis
  gate passed.
- round-261: completed item-261-conformance-check-program-package-tracer by
  adding a shared conformance corpus check-program package tracer with
  metadata-driven checkProgramArgs dispatch and committed OK stdout; evidence:
  focused conformance tests, cabal build all, cabal test, and thesis gate
  passed.
- round-262: completed item-262-conformance-check-program-search-path-tracer
  by adding a shared conformance corpus check-program search-path tracer with
  metadata-derived package root/search-path argv and committed OK stdout;
  evidence: focused conformance tests, cabal build all, cabal test, and thesis
  gate passed.
- round-263: completed item-263-conformance-check-program-fail-missing-import-tracer
  by adding the shared conformance corpus expect-fail check-program
  missing-import tracer with committed expected stderr; evidence: focused
  conformance tests, cabal build all, cabal test, and thesis gate passed.
- round-264: completed the status-only conformance corpus closeout audit for
  milestone-2; evidence: approved rounds 259-263, five shared corpus examples,
  focused corpus check, cabal build all, cabal test, and thesis gate passed.

#### Candidate Direction: Conformance Corpus Migration

- Direction id: `direction-2a-conformance-corpus-migration`
- Summary: create the corpus contract and migrate public `.mlfp` behavior
  fixtures into it.
- Why it matters now: the future `.mlfp` compiler needs the same oracle as the
  Haskell compiler before implementation begins.
- Preconditions: milestone 1 complete; inspect current `test/programs/`,
  package-mode tests, CLI tests, expected output behavior, and diagnostic
  rendering.
- Parallel hints: serial until metadata and expected-output meaning are stable.
- Boundary notes: no broad string/parser/platform/compiler implementation.
- Extraction notes: start from representative public fixtures and expand after
  harness validation is solid.

### [done] 3. Native-Capable Broad Text Substrate

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: complete the initial Prelude-level Broad String Library and Unicode
  scalar `Char` contract as one coherent native-capable library surface through
  source checking, interpreter/runtime, backend emission, object generation,
  and native execution.
- Completion signal: `.mlfp` source supports the completed rev-004 initial
  Broad String Library matrix below: carried-forward rounds 265-301 behavior,
  remaining public string helpers selected by `item-302-broad-string-library-completion`,
  boundary behavior for slicing and cursor APIs, parser-needed ASCII
  classification/case helpers, explicit formatting/composition helpers, exact
  Unicode-scalar search/split/replace behavior, and exact native metadata for
  every native string-producing helper. The milestone is complete only after
  grouped focused validation and full validation prove the whole matrix across
  interpreter and native paths.
- Parallel lane: `lane-text-parser`
- Coordination notes: rev-004 replaces the implicit one-function-per-round
  tracer pattern with one integrated whole-library completion item. Locale,
  regex, parser combinators, full parser parity, platform contracts, compiler
  package work, driver work, and proof work remain out of scope. Parser
  combinators remain parser-owned modules, not Prelude-level generic parser
  API.

#### Completion Pointers: milestone-3

- round-303 completed item-302-broad-string-library-completion: rev-004 Broad String Library matrix satisfied across source checking, run-program, backend/object, emit-native/native-object, linked native execution, exact native metadata, carried regressions, full cabal build/test, and thesis gate; milestone 4 may start without additional Prelude-level string requirements.
- round-265: completed item-265-char-literal-native-tracer: explicit Unicode
  scalar Char literal support through check, run, backend LLVM, object
  validation, emit-native, and linked native execution for
  `def main : Char = 'λ';`; focused/full/thesis gates passed.
- round-266: completed item-266-unicode-string-literal-native-tracer:
  non-ASCII String literal support for `def main : String = "λ";` through
  check, run, backend LLVM, object validation, emit-native, and linked native
  execution; focused/native-neighbor checks passed and recorded
  full/thesis gates passed.
- round-267: completed item-267-unicode-string-length-native-tracer by adding
  public Prelude `stringLength : String -> Int` with Unicode scalar counting
  through check, run, backend LLVM, object validation, emit-native, and linked
  native execution; evidence: focused matcher, neighbor text tracers, cabal
  build all, cabal test, and thesis gate passed.
- round-268 completed item-268-string-is-empty-native-tracer:
  `stringIsEmpty : String -> Bool` now classifies `""` as true and `"λ"` as
  false through check, run-program, backend LLVM/object validation,
  emit-native, native object validation, and linked native execution;
  evidence: focused and neighbor matchers, `git diff --check`, cabal build
  all, cabal test, and thesis gate passed.
- round-269 completed item-269-string-contains-char-native-tracer:
  `stringContainsChar : String -> Char -> Bool` now classifies
  `stringContainsChar "aλb" 'λ'` as true and `stringContainsChar "ab" 'λ'` as
  false through check, run-program, backend LLVM/object validation,
  emit-native, native object validation, and linked native execution;
  evidence: focused and neighbor matchers, `git diff --check`, cabal build
  all, cabal test, and thesis gate passed.
- round-270 completed item-270-string-contains-native-tracer:
  `stringContains : String -> String -> Bool` now classifies
  `stringContains "aλb" "λ"` as true and `stringContains "ab" "λ"` as false
  through source checking, run-program, backend LLVM/object validation,
  emit-native, native object validation, and linked native execution;
  evidence: focused and neighbor matchers, rg checks, `git diff --check`,
  cabal build all, cabal test, and thesis gate passed.
- round-271 completed item-271-string-starts-with-native-tracer:
  `stringStartsWith : String -> String -> Bool` now classifies
  `stringStartsWith "λab" "λ"` as true and `stringStartsWith "aλb" "λ"` as
  false through source checking, run-program, backend LLVM/object validation,
  emit-native, native object validation, and linked native execution;
  evidence: focused and neighbor matchers, primitive inventory matcher, rg
  checks, `git diff --check`, cabal build all, cabal test, and thesis gate
  passed.
- round-272 completed item-272-string-ends-with-native-tracer:
  `stringEndsWith : String -> String -> Bool` now classifies
  `stringEndsWith "abλ" "λ"` as true and `stringEndsWith "λab" "λ"` as
  false through source checking, run-program, backend LLVM/object validation,
  emit-native, native object validation, and linked native execution;
  evidence: focused and neighbor matchers, primitive inventory matcher, rg
  checks, `git diff --check`, cabal build all, cabal test, and thesis gate
  passed.
- round-273 completed item-273-string-drop-native-tracer:
  `stringDrop : String -> Int -> String` returns `"ab"` for
  `stringDrop "λab" 1` and `"b"` for `stringDrop "aλb" 2` through source
  checking, run-program, backend LLVM/object validation, emit-native/native
  object validation, and linked native execution; evidence: focused
  stringDrop matcher, primitive inventory matcher, neighbor text/native
  matcher set, rg evidence checks, `git diff --check`, cabal build all,
  cabal test, and thesis gate passed.
- round-274 completed item-274-string-take-native-tracer:
  `stringTake : String -> Int -> String` returns `"\\955"` for
  `stringTake "λab" 1` and `"a\\955"` for `stringTake "aλb" 2` with the
  current escaped string display through source checking, run-program, backend
  LLVM/object validation, emit-native/native object validation, and linked
  native execution; evidence: focused stringTake matcher, primitive inventory
  matcher, neighbor text/native matcher set, rg evidence checks,
  `git diff --check`, cabal build all, cabal test, and thesis gate passed.
- round-275 completed item-275-string-slice-native-tracer:
  `stringSlice : String -> Int -> Int -> String` returns `"\\955b"` for
  `stringSlice "aλbc" 1 2` and `"ab"` for `stringSlice "λabc" 1 2` with the
  current escaped string display through source checking, run-program, backend
  LLVM/object validation, emit-native/native object validation, and linked
  native execution; evidence: focused stringSlice matcher, primitive inventory
  matcher, neighbor text/native matcher set, rg evidence checks,
  `git diff --check`, cabal build all, cabal test, and thesis gate passed.
- round-276 completed item-276-string-char-at-native-tracer:
  `stringCharAt : String -> Int -> Char` returns `'\\955'` for
  `stringCharAt "aλb" 1` and `'b'` for `stringCharAt "λab" 2` through source
  checking, run-program, backend LLVM/object validation, emit-native/native
  object validation, and linked native execution; evidence: focused
  stringCharAt matcher, primitive inventory matcher, neighbor text/native
  matcher set, rg evidence checks, `git diff --check`, cabal build all,
  cabal test, and thesis gate passed. Native printable ASCII Char rendering
  was aligned only to match public run-program result rendering for this
  tracer.
- round-277 completed item-277-char-is-digit-native-tracer:
  `charIsDigit : Char -> Bool` classifies `charIsDigit '7'` as true and
  `charIsDigit 'λ'` as false through source checking, run-program, backend
  LLVM/object validation, emit-native/native object validation, and linked
  native execution; evidence: focused charIsDigit matcher, primitive inventory
  matcher, neighbor text/native matcher set, rg evidence checks,
  `git diff --check`, cabal build all, cabal test, and thesis gate passed.
- round-278 completed item-278-char-is-ascii-lower-native-tracer:
  `charIsAsciiLower : Char -> Bool` classifies `charIsAsciiLower 'a'` as
  true, `charIsAsciiLower 'A'` as false, and `charIsAsciiLower 'λ'` as false
  through source checking, run-program, backend LLVM/object validation,
  emit-native/native object validation, and linked native execution; evidence:
  focused charIsAsciiLower matcher, primitive inventory matcher, neighbor
  text/native matcher set, rg evidence/claim-audit checks,
  `git diff --check`, cabal build all, cabal test, and thesis gate passed.
- round-279 completed item-279-char-is-ascii-upper-native-tracer:
  `charIsAsciiUpper : Char -> Bool` classifies `charIsAsciiUpper 'A'` as
  true, `charIsAsciiUpper 'a'` as false, and `charIsAsciiUpper 'λ'` as false
  through source checking, run-program, backend LLVM/object validation,
  emit-native/native object validation, and linked native execution; evidence:
  focused charIsAsciiUpper matcher, primitive inventory matcher, neighbor
  text/native matcher set, rg evidence/claim-audit checks,
  `git diff --check`, cabal build all, cabal test, and thesis gate passed.
- round-280 completed item-280-char-is-ascii-alpha-native-tracer:
  `charIsAsciiAlpha : Char -> Bool` classifies `charIsAsciiAlpha 'a'` and
  `charIsAsciiAlpha 'A'` as true, and `charIsAsciiAlpha '7'` and
  `charIsAsciiAlpha 'λ'` as false through source checking, run-program,
  backend LLVM/object validation, emit-native/native object validation, and
  linked native execution; evidence: focused charIsAsciiAlpha matcher,
  primitive inventory matcher, neighbor text/native matcher set, rg
  evidence/claim-audit checks, `git diff --check`, cabal build all,
  cabal test, and thesis gate passed.
- round-281 completed item-281-char-is-ascii-alphanum-native-tracer:
  `charIsAsciiAlphaNum : Char -> Bool` classifies `charIsAsciiAlphaNum 'a'`,
  `charIsAsciiAlphaNum 'A'`, and `charIsAsciiAlphaNum '7'` as true, and
  `charIsAsciiAlphaNum '_'` and `charIsAsciiAlphaNum 'λ'` as false through
  source checking, run-program, backend LLVM/object validation,
  emit-native/native object validation, and linked native execution; evidence:
  focused charIsAsciiAlphaNum matcher, primitive inventory matcher, neighbor
  text/native matcher set, rg evidence/claim-audit checks,
  `git diff --check`, cabal build all, cabal test, and thesis gate passed.
- round-282 completed item-282-char-is-ascii-identifier-start-native-tracer:
  `charIsAsciiIdentifierStart : Char -> Bool` classifies
  `charIsAsciiIdentifierStart 'a'`, `charIsAsciiIdentifierStart 'A'`, and
  `charIsAsciiIdentifierStart '_'` as true, and
  `charIsAsciiIdentifierStart '7'`, apostrophe, and
  `charIsAsciiIdentifierStart 'λ'` as false through source checking,
  run-program, backend LLVM/object validation, emit-native/native object
  validation, and linked native execution; evidence: focused
  charIsAsciiIdentifierStart matcher, primitive inventory matcher, neighbor
  text/Char native matcher set, rg evidence/claim-audit checks,
  `git diff --check`, cabal build all, cabal test, and thesis gate passed.
- round-283 completed item-283-char-is-ascii-identifier-continue-native-tracer:
  `charIsAsciiIdentifierContinue : Char -> Bool` classifies ASCII lower,
  upper, digit, underscore, and apostrophe characters as true and non-ASCII
  lambda as false through source checking, run-program, backend LLVM/object
  validation, emit-native/native object validation, and linked native
  execution; evidence: focused matcher, primitive inventory matcher, neighbor
  text/Char native matcher set, rg evidence/claim-audit checks,
  `git diff --check`, cabal build all, cabal test, and thesis gate passed.
- round-284 completed item-284-char-is-ascii-whitespace-native-tracer:
  `charIsAsciiWhitespace : Char -> Bool` classifies ASCII space, tab, newline,
  carriage return, form feed, and vertical tab as true, and ASCII `a` plus
  non-ASCII lambda as false through source checking, run-program, backend
  LLVM/object validation, emit-native/native object validation, and linked
  native execution; evidence: focused matcher, primitive inventory matcher,
  neighbor text/Char native matcher set, rg evidence/claim-audit checks,
  `git diff --check`, cabal build all, cabal test, and thesis gate passed.
- round-285 completed item-285-char-is-ascii-punctuation-native-tracer:
  `charIsAsciiPunctuation : Char -> Bool` classifies ASCII punctuation ranges
  `0x21..0x2f`, `0x3a..0x40`, `0x5b..0x60`, and `0x7b..0x7e` as true and
  letters, digits, space, and non-ASCII lambda as false through source
  checking, run-program, backend LLVM/object validation, emit-native/native
  object validation, and linked native execution; evidence: focused matcher,
  primitive inventory matcher, neighbor text/Char native matcher set, rg
  evidence/claim-audit checks, `git diff --check`, cabal build all,
  cabal test, thesis gate, and reviewer rerun of diff, focused matcher, and
  primitive inventory matcher passed.
- round-286 completed item-286-char-is-ascii-printable-native-tracer:
  `charIsAsciiPrintable : Char -> Bool` classifies ASCII scalar values
  `0x20..0x7e` as true and tab, newline, and non-ASCII lambda as false through
  source checking, run-program, backend LLVM/object validation,
  emit-native/native object validation, and linked native execution; evidence:
  focused matcher, primitive inventory matcher, neighbor text/Char native
  matcher set, rg evidence/claim-audit checks, `git diff --check`, cabal build
  all, cabal test, thesis gate, and reviewer rerun of diff, focused matcher,
  and primitive inventory matcher passed.
- round-287 completed item-287-string-append-native-tracer:
  `stringAppend : String -> String -> String` concatenates Unicode-scalar
  strings, including non-ASCII lambda and empty-side identity cases, through
  source checking, run-program, backend LLVM/object validation,
  emit-native/native object validation, and linked native execution; reviewer
  evidence: git diff --check, focused native stringAppend matcher, primitive
  inventory matcher, and docs claim audit passed.
- round-288 completed item-288-string-from-char-native-tracer:
  `stringFromChar : Char -> String` constructs singleton strings from Unicode
  scalar Char values, including lambda and ASCII A, through source checking,
  run-program, backend LLVM/object validation, emit-native/native object
  validation, and linked native execution; reviewer evidence: git diff --check,
  focused native stringFromChar matcher, primitive inventory matcher,
  orphan-fragment check, and docs scope audit passed.
- round-289 completed item-289-string-from-list-native-tracer:
  `stringFromList : List Char -> String` converts List Char values to
  Unicode-scalar strings, including the non-ASCII lambda list case and Nil
  empty case, through source checking, run-program, backend LLVM/object
  validation, emit-native/native object validation, and linked native
  execution; reviewer evidence: git diff --check, focused native
  stringFromList matcher, primitive absence audit, active-roadmap diff check,
  and docs scope audit passed.
- round-290 completed item-290-string-to-list-native-tracer: fixed the public
  PhiReorder checker regression for named functions returning `List Char`,
  then added `stringToList : String -> List Char` backed by
  `__string_to_list` and proved non-ASCII lambda plus empty string behavior
  through source checking, run-program, backend LLVM/object validation,
  emit-native/native object validation, linked native execution, primitive
  inventory, docs scope audit, git diff --check, and broad build/test/thesis
  evidence.
- round-291 completed item-291-string-from-int-native-tracer: added
  `stringFromInt : Int -> String` backed by `__string_from_int` and proved
  decimal outputs for 42 and 0 through source checking, run-program, backend
  LLVM/object validation, emit-native/native object validation, linked native
  execution, primitive inventory, neighbor text/char checks including current
  stringLength matcher, docs scope audit, git diff --check, and broad
  build/test/thesis evidence.
- round-292 completed item-292-string-from-bool-native-tracer: added
  `stringFromBool : Bool -> String` backed by `__string_from_bool` and proved
  lowercase true/false outputs through source checking, run-program, backend
  LLVM/object validation, emit-native/native object validation, linked native
  execution, primitive inventory, planned neighbor text/char checks, docs
  scope audit, git diff --check, and broad build/test/thesis evidence.
- round-293 completed item-293-string-from-nat-native-tracer: added
  `stringFromNat : Nat -> String` backed by `__string_from_nat` and proved
  canonical decimal outputs for `Zero` and `Succ (Succ Zero)` through source
  checking, run-program, backend LLVM/object validation, emit-native/native
  object validation, linked native execution, primitive inventory, planned
  neighbor text/char checks, docs scope audit, git diff --check, and broad
  build/test/thesis evidence.
- round-294 completed item-294-string-from-unit-native-tracer: added pure
  Prelude `stringFromUnit : Unit -> String` and proved `Unit -> "Unit"`
  through source checking, run-program, backend LLVM/object validation,
  emit-native/native object validation, linked native execution, primitive
  absence audit, planned neighbor text/char checks, docs scope audit, git
  diff --check, and broad build/test/thesis evidence.
- round-295 completed item-295-string-replace-char-native-tracer: added
  `stringReplaceChar : String -> Char -> Char -> String` backed by
  `__string_replace_char` and proved non-ASCII lambda replacement plus no-match
  preservation through source checking, run-program, backend LLVM/object
  validation, emit-native/native object validation, linked native execution,
  primitive inventory, planned neighbor text/char checks, docs scope audit,
  Unicode-scalar replacement allocation audit, git diff --check, and broad
  build/test/thesis evidence.
- round-296 completed item-296-string-index-of-char-native-tracer: added
  `stringIndexOfChar : String -> Char -> Option Int` backed by
  `__string_index_of_char` and proved first lambda index `Some 1` plus
  absent-match `None` through source checking, run-program, backend
  LLVM/object validation, emit-native/native object validation, linked native
  execution, primitive inventory, Option tag/layout audit, planned neighbor
  text/char checks, docs scope audit, git diff --check, and broad
  build/test/thesis evidence.
- round-297 completed item-297-string-index-of-native-tracer: added
  stringIndexOf : String -> String -> Option Int backed by __string_index_of
  and proved first Unicode-scalar substring index Some 1, absent substring
  None, and empty needle Some 0 through source checking, run-program, backend
  LLVM/object validation, emit-native/native object validation, linked native
  execution, primitive inventory, planned neighbor text/char checks, docs scope
  audit, git diff --check, cabal build all, cabal test, and thesis gate.
- round-298 completed item-298-string-replace-native-tracer: added
  stringReplace : String -> String -> String -> String backed by
  __string_replace and proved left-to-right non-overlapping Unicode-scalar
  substring replacement, no-match preservation, and empty-needle no-op through
  source checking, run-program, backend LLVM/object validation,
  emit-native/native object validation, linked native execution, primitive
  inventory, planned neighbor text/char checks, docs scope audit,
  git diff --check, cabal build all, cabal test, and thesis gate.
- round-299 completed item-299-string-split-native-tracer: added
  stringSplit : String -> String -> List String backed by __string_split and
  proved Unicode-scalar substring delimiter splitting, no-match singleton
  behavior, empty-delimiter singleton behavior, and leading/trailing empty
  segment preservation through source checking, run-program, backend
  LLVM/object validation, emit-native/native object validation, linked native
  execution, primitive inventory, planned neighbor text checks, Phi
  replay-domain audit, docs scope audit, git diff --check, cabal build all,
  cabal test, and thesis gate.
- round-300 completed item-300-string-char-at-option-native-tracer: public
  stringCharAtOption : String -> Int -> Option Char backed by
  __string_char_at_option, with Unicode scalar Some/None behavior proven
  through check, run-program, backend/object, emit-native/native-object, and
  linked native execution.
- round-301 completed item-301-string-equals-native-tracer; evidence: focused
  stringEquals, primitive inventory, neighbor, full build/test, thesis gate,
  and final generated-artifact audit passed.

#### Rev-004 Whole-Library Completion Contract

The next milestone-3 implementation selection must use this extracted item:

- Extracted item id: `item-302-broad-string-library-completion`
- Summary: complete the initial native-capable Broad String Library as one
  integrated library round, using grouped public behavior tests and preserving
  the parser-owned boundary.
- Required planner shape: one `selection-record.json`, one `plan.md`, and one
  `round-plan-record.json` for the whole-library item. The plan may contain
  multiple vertical RED -> GREEN -> refactor cycles, but it must not split the
  remaining library into separate future one-function rounds.
- Default worker mode: `none`. Worker fan-out is not expected; if proposed, the
  planner must name disjoint write scopes and a single integration owner.

Already complete and carried forward from rounds 265-301:

- Literal and sequence model: Unicode scalar `Char` literals, non-ASCII
  `String` literals, valid Unicode scalar strings, and native rendering.
- Existing public string API: `stringLength`, `stringIsEmpty`,
  `stringContainsChar`, `stringContains`, `stringEquals`,
  `stringStartsWith`, `stringEndsWith`, `stringAppend`,
  `stringReplaceChar`, `stringReplace`, `stringIndexOfChar`,
  `stringIndexOf`, `stringSplit`, `stringFromChar`, `stringFromInt`,
  `stringFromBool`, `stringFromNat`, `stringFromUnit`, `stringFromList`,
  `stringToList`, `stringDrop`, `stringTake`, `stringSlice`,
  `stringCharAt`, and `stringCharAtOption`.
- Existing public `Char` API: `charIsDigit`, `charIsAsciiLower`,
  `charIsAsciiUpper`, `charIsAsciiAlpha`, `charIsAsciiAlphaNum`,
  `charIsAsciiIdentifierStart`, `charIsAsciiIdentifierContinue`,
  `charIsAsciiWhitespace`, `charIsAsciiPunctuation`, and
  `charIsAsciiPrintable`.
- Existing proof shape: every carried-forward operation has source checking,
  `run-program`, backend LLVM/object validation, `emit-native`/native-object
  validation, linked native execution, primitive inventory coverage where
  primitive-backed, and docs claim-audit evidence.

The next whole-library item must implement and validate this remaining public
API and behavior matrix:

- Formatting and composition:
  - Include `stringJoin : String -> List String -> String`.
  - `stringJoin sep Nil` returns `""`; singleton lists return their element;
    multiple elements insert exactly one separator between adjacent elements.
  - Existing explicit conversions `stringFromInt`, `stringFromBool`,
    `stringFromNat`, `stringFromUnit`, `stringFromChar`, and
    `stringFromList` remain the formatting surface for primitive and
    first-order values.
  - Exclude printf-style formatting, interpolation, format-string parsing,
    locale-aware formatting, and generic `Show`-derived formatting expansion.
- Slicing and cursor boundaries:
  - No new public cursor type is added in milestone 3.
  - Existing `stringDrop`, `stringTake`, `stringSlice`, and
    `stringCharAtOption` must gain explicit boundary evidence: negative
    offsets/counts, zero, exact end, beyond end, empty input, non-ASCII scalar
    input, and embedded U+0000 input where applicable.
  - Boundary semantics are fixed for the initial library: negative drop/take
    counts clamp to zero; negative slice start clamps to zero; non-positive
    slice count returns `""`; overlarge drop returns `""`; overlarge take
    returns the whole string; overlarge slice returns the remaining suffix or
    `""`; negative or out-of-range `stringCharAtOption` returns `None`.
  - `stringCharAt` remains an in-range helper. New parser-facing code must use
    `stringCharAtOption` for boundary-safe cursor behavior.
- Plain search, split, replace, and comparison:
  - Include `stringSplitChar : String -> Char -> List String` as the
    single-character delimiter counterpart to `stringSplit`.
  - Include `stringCompare : String -> String -> Int`.
  - `stringSplitChar` preserves leading/trailing empty segments and uses
    Unicode scalar delimiter equality.
  - `stringCompare` returns `-1`, `0`, or `1` for lexicographic Unicode scalar
    value order. It is deterministic scalar ordering, not locale collation and
    not Unicode Collation Algorithm behavior.
  - Existing `stringContainsChar`, `stringContains`, `stringStartsWith`,
    `stringEndsWith`, `stringIndexOfChar`, `stringIndexOf`,
    `stringReplaceChar`, `stringReplace`, and `stringSplit` must receive
    grouped edge coverage for empty needles/delimiters, no-match cases,
    repeated matches, non-overlapping replacement, non-ASCII scalars, and
    embedded U+0000 metadata preservation.
  - Exclude regex, glob, pattern-language, locale-sensitive search, and
    collation search.
- `String`/`List Char` helpers:
  - No generic `List` library is added by milestone 3.
  - The whole-library round must prove `stringFromList`/`stringToList`
    round-trips for empty, ASCII, non-ASCII, and embedded U+0000 scalar
    sequences through interpreter and native execution.
  - `stringJoin` is the only new `List String` helper required by milestone 3;
    broader list append, reverse, folds, filters, non-empty streams, and token
    stream helpers belong to later parser/compiler work unless another
    semantic update selects them.
- Classification and case helpers:
  - Include `charIsAsciiHexDigit : Char -> Bool`.
  - Include `charIsAsciiLineBreak : Char -> Bool`, true exactly for LF and CR.
  - Include `charIsAsciiControl : Char -> Bool`, true exactly for ASCII
    control scalars `0x00..0x1f` and `0x7f`.
  - Include `charToAsciiLower : Char -> Char` and
    `charToAsciiUpper : Char -> Char`.
  - Include `stringToAsciiLower : String -> String` and
    `stringToAsciiUpper : String -> String`.
  - The ASCII case helpers change only ASCII letters and preserve all other
    Unicode scalar values unchanged. They are explicitly ASCII APIs, not
    Unicode default case mapping.
  - Broader Unicode property classification and Unicode default case
    conversion are deferred because they require a Unicode data-table policy
    that is not needed to unlock parser parity.
- Normalization and Unicode-default exclusions:
  - No Unicode normalization API is required for milestone 3.
  - Existing equality, search, split, replace, comparison, and metadata rules
    operate on exact Unicode scalar sequences. Canonically equivalent but
    differently encoded scalar sequences remain distinct.
  - Unicode normalization and Unicode Collation Algorithm behavior require a
    future semantic roadmap update before implementation.
- Exact native metadata:
  - The whole-library round must make exact length/metadata behavior true for
    every native string-producing helper used by the matrix, including carried
    operations and new operations.
  - Required evidence includes embedded U+0000 strings created by source
    literals and by native string-producing helpers, then consumed by
    `stringEquals`, `stringLength`, search/split/replace, rendering, and
    native execution without C-string truncation.

Grouped verification strategy for `item-302-broad-string-library-completion`:

- Public API/import group: all new names import from Prelude and typecheck in
  ordinary `.mlfp` modules.
- Boundary group: slicing and cursor boundary matrix for `stringDrop`,
  `stringTake`, `stringSlice`, and `stringCharAtOption`.
- Search/split/replace/join group: `stringSplitChar`, `stringJoin`, and
  carried search/split/replace edge cases.
- Classification/case/compare group: new ASCII classification helpers, ASCII
  case helpers, and `stringCompare`.
- Exact metadata group: embedded U+0000 and non-ASCII strings produced by
  source literals and native helpers remain exact through equality, length,
  search, split/replace, rendering, and linked native execution.
- Regression group: carried-forward focused neighbors from rounds 265-301 and
  the primitive inventory native-lowerability check stay green.
- Closeout group: `git diff --check`, focused grouped matchers,
  `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh`.

Milestone-3 closeout criteria after the whole-library round:

- The implementation, docs, and tests cover every included matrix row above.
- The reviewer confirms no locale, regex, parser combinator, full parser
  parity, platform, compiler package, driver, or proof scope was added.
- The reviewer confirms deferred Unicode normalization, Unicode collation,
  Unicode default case mapping, generic `List` library work, parser-owned
  cursor/combinator work, maps/sets, filesystem/process IO, package locks,
  ABI/linker contracts, and proof records are not claimed as complete.
- Status-only closeout may mark milestone 3 `done` only when the reviewer
  explicitly records that this rev-004 matrix is satisfied and that milestone
  4 can start without inventing additional Prelude-level string requirements.

#### Candidate Direction: Broad String And Char Substrate

- Direction id: `direction-3a-broad-string-char-substrate`
- Summary: complete the native-capable initial Broad String Library and
  Unicode scalar `Char` substrate needed before source-text parser parity.
- Why it matters now: parser parity must not hide text requirements inside
  parser-private helpers or interpreter-only support.
- Preconditions: conformance corpus is stable enough to cover public behavior;
  inspect Prelude, parser seed gaps, backend/native string lowering, runtime
  value representation, carried-forward rounds 265-301 evidence, and the
  rev-004 whole-library completion matrix.
- Parallel hints: serial with parser parity until the text substrate is native
  proven.
- Boundary notes: exclude locale, regex, parser combinators, full parser
  parity, platform contracts, compiler package work, driver work, and proof
  work. Defer Unicode normalization, Unicode collation, Unicode default case
  mapping, generic `List` library work, parser-owned source cursor/combinator
  work, maps/sets, and filesystem/process IO unless a later semantic roadmap
  update selects them.
- Extraction notes: select `item-302-broad-string-library-completion` as one
  integrated whole-library implementation item. Prove source, interpreter,
  backend, object, and native behavior together with grouped public behavior
  tests rather than one-function-per-round tracers.

### [in-progress] 4. Full Canonical `.mlfp` Parser Parity

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: build one shared parser-owned combinator, parser-monad,
  lexer/token, source cursor, and syntax parser library in `.mlfp` on top of
  the broad text substrate, then use fixtures to exercise that library.
- Completion signal: a shared `.mlfp` parser implementation produces the same
  parsed program syntax artifact and source spans as the current canonical
  parser for the selected corpus, with conformance-backed pass/fail evidence
  and no checker, backend, or driver overclaim. Fixture packages may provide
  source/evidence harnesses, but they must not each own independent grammar
  parsers or exact-source token streams as the success path.
- Parallel lane: `lane-text-parser`
- Coordination notes: this milestone is parser parity only. Name resolution,
  checking, backend lowering, driver behavior, and self-hosting belong later.
  Round-304 through round-308 established fixture-scoped parity tracers; any
  accepted round-309 data-declaration tracer is the final instance of that
  pattern. Future parser work must grow a shared parser library surface and
  route multiple canonical fixtures through it.

#### Completion Pointers: milestone-4

- round-304 completed item-304-parser-parity-basic-module-def-bool-spans: parser-owned `.mlfp` modules emit the canonical basic Bool parser projection and retry evidence covers lexer/parser mismatches; reviewer gates passed.
- round-305 completed item-305-parser-parity-import-exposing-spans: parser-owned `.mlfp` modules emit canonical import-exposing Bool parser projection and malformed-import evidence; reviewer recheck passed generated hygiene, focused parser checks, parser-parity group, and package smokes.
- round-306 completed item-306-parser-parity-value-definition-list-spans: parser-owned `.mlfp` modules emit canonical value-definition-list parser projection with `Int` literal and value-reference spans plus malformed sequencing evidence; reviewer recheck passed generated hygiene, the parser parity group, and direct package smoke.
- round-307 completed item-307-parser-parity-let-lambda-application-spans as a partial milestone-4 parser-parity tracer; evidence: focused let/lambda/application matcher, malformed-let matcher, parser-parity group, package smokes, full Cabal gate, and thesis gate passed.
- round-308 completed item-308-parser-parity-typed-annotation-types as a partial milestone-4 parser-parity tracer; evidence: focused typed-annotation and malformed-annotation checks, parser-parity group, package smokes, full Cabal gate, and thesis gate passed.
- round-309 completed item-309-parser-parity-data-declaration-constructor-spans as a partial milestone-4 parser-parity tracer; evidence: parser-parity group, package smokes, git diff --check, cabal build all && cabal test, and thesis gate passed.
- round-310 completed item-310-parser-library-consolidation: carried parser-parity fixtures now route through one shared parser-owned .mlfp parser-combinator library with complete syntax/end-state checking and shared tokenizer/parser diagnostics; evidence: static audits, focused shared-entrypoint and ADT regressions, six direct smokes, parser-parity group, full Cabal gate, and thesis gate passed.
- round-311 completed item-311-parser-source-text-front-door: carried parser-parity fixtures now expose sourceFile/sourceText and call the shared parser-owned source-text lexer/parser front door before grammar parsing; evidence: focused source-text matcher, parser-parity group, static banned-shape audits, six direct smokes, full Cabal gate, and thesis gate passed.
- round-312 completed item-312-parser-library-case-pattern-extension: the shared parser-owned source-text parser parity library covers the selected case-expression and constructor-pattern fixtures with malformed case-arrow evidence; validation passed focused/parser-parity/full Cabal gates, static shortcut audits, and thesis gate.
- round-313 completed item-313-parser-library-typeclass-instance-extension: the shared source-text parser-parity library covers bounded typeclass, deriving, and instance syntax through the parser-owned lexer/combinator path with thin fixtures and malformed instance-method evidence; validation passed focused/parser-parity/full Cabal gates, static shortcut audits, direct fixture smokes, and thesis gate.
- round-314 completed item-314-parser-library-higher-kinded-constraint-extension: the shared source-text parser-parity library covers bounded higher-kinded parameters, variable-headed type applications, superclass constraints, multi-parameter class heads, functional dependencies, and empty instance bodies through parser-combinator grammar with thin fixtures and guard coverage against the rejected fixture-key shortcut shapes; validation passed focused/parser-parity/full Cabal gates, direct fixture smokes, static shortcut audits, and thesis gate.
- round-315 completed item-315-parser-library-type-family-type-level-extension: the shared source-text parser-library path now covers bounded closed type-family declarations, kinded/plain family parameters, type-level patterns, type-level lambda/application syntax, source type-family annotations, and malformed equation diagnostics through thin parser-parity fixtures; reviewer evidence: focused matcher, all carried/new run-program smokes, static shortcut audits, git diff --check, cabal build all, cabal test, and thesis gate passed.
- round-316 completed item-316-parser-library-gadt-existential-extension: the shared source-text parser-library path now covers bounded GADT-style constructor result heads, parameterized data declarations, constructor-local Unicode forall signatures, nested constructor field/result type applications, related constructor/case pattern syntax, and malformed constructor-local forall-dot diagnostics through thin parser-parity fixtures; reviewer evidence: focused matcher, all carried/new run-program smokes, static shortcut audits, git diff --check, cabal build all, cabal test, and thesis gate passed.
- round-317 completed item-317-parser-library-qualified-import-reference-extension: the shared parser-owned parser library now covers qualified import aliases and qualified value/type/constructor/class/method references; evidence: focused matcher, direct smokes, shortcut audits, cabal build all, cabal test, and thesis gate passed.
- round-318 completed item-318-parser-library-multi-module-export-import-extension: the shared parser-owned parser now handles two multi-module export/import parser-parity fixtures with token-derived spans; reviewer evidence: focused/full parser parity, 20 direct smokes, cabal build all, cabal test, and thesis gate passed.
- round-319 completed item-319-parser-library-text-literal-extension: the shared parser-owned source-text parser library now scans and consumes Char/String literal tokens for the text-literal-char-string parser-parity fixture and reports malformed literal syntax through the public batch; reviewer evidence: focused Char/String and malformed-literal matchers, static shortcut guard, parser-parity group, 21 direct fixture smokes, cabal build all, cabal test, and thesis gate passed.
- round-320 completed item-320-parser-library-first-class-polymorphism-source-type-extension: shared parser-owned source-text parser library parses first-class polymorphic source types and public malformed source-type diagnostics; evidence: focused parser matchers, shortcut audits, 22 direct fixture smokes, parser-parity group, cabal build all, cabal test, and thesis gate passed.
- round-321 completed item-321-parser-library-higher-order-partial-application-extension: the shared parser-owned library now covers the bounded higher-order partial-application fixture and malformed close-paren diagnostic; reviewer evidence passed focused parser checks, direct fixture diffs, cabal build all, cabal test, and thesis gate.
- round-322 completed item-322-parser-library-higher-order-local-function-flow-extension: the shared parser-owned library now covers the bounded higher-order local-function-flow fixture and malformed typed-let diagnostic; reviewer evidence passed focused parser checks, direct fixture diffs, cabal build all, cabal test, and thesis gate.
- round-323 completed item-323-parser-library-higher-order-returned-function-extension: the shared parser-owned library now covers the bounded higher-order returned-function fixture and malformed returned-function diagnostic; reviewer evidence passed focused parser checks, direct fixture diffs, cabal build all, cabal test, and thesis gate.

#### Rev-004 Parser Library Consolidation Contract

The next normal milestone-4 implementation selection after the active
round-309 recovery/closeout decision must use this extracted item:

- Extracted item id: `item-310-parser-library-consolidation`
- Summary: replace the one-parser-per-fixture tracer pattern with one shared
  parser-owned `.mlfp` parser-combinator library with an explicit parser monad
  or equivalent monadic parser state abstraction that the carried parser-parity
  fixtures call through a common entrypoint.
- Required planner shape: one `selection-record.json`, one `plan.md`, and one
  `round-plan-record.json` for the shared parser-library consolidation item.
  The plan may contain grouped vertical RED -> GREEN -> refactor cycles, but it
  must not create another future round whose main artifact is a new
  fixture-specific `ParserParityParser.mlfp` plus exact-source tokenizer.
- Default worker mode: `none`. Worker fan-out is not expected; if proposed, the
  planner must name disjoint write scopes for parser library, fixture harness
  migration, and projection/diagnostic evidence, plus a single integration
  owner.

Already complete and carried forward from rounds 304-308:

- Positive parser projections for basic module/value definition, import
  exposing, multiple value definitions and value-reference spans,
  let/lambda/application expressions, typed let annotations, annotated lambda
  parameters, expression annotations, and source type rendering.
- Negative public `run-program` evidence for malformed tokenizer/parser
  mismatches, import syntax, value-definition sequencing, let syntax, and
  annotation syntax.
- The public parity pattern that compares the Haskell canonical parser
  projection and the `.mlfp` parser package projection against the same
  committed golden artifact.

The shared parser-library item must implement and validate this consolidation
matrix:

- Shared entrypoint:
  - Provide one parser-owned `.mlfp` parser entrypoint for complete module
    source text, with a single success/failure result type used by all carried
    parity fixtures.
  - Existing fixture packages should become thin harnesses that provide source
    text, call the shared parser entrypoint, and render the shared projection.
  - Do not keep a separate grammar parser per fixture family.
- Parser monad and combinators:
  - Define a parser-owned `Parser` abstraction or equivalent record/function
    shape that carries input cursor/token state, success value, remaining
    state, source span context, and parser-owned diagnostic failure.
  - Provide explicit monadic sequencing helpers such as `parserBind` or
    `andThen`, `parserMap`, `parserPure`, and failure propagation. If `.mlfp`
    typeclass support is not available, use named combinator functions rather
    than claiming a typeclass instance.
  - Provide parser combinators for token/symbol expectation, choice,
    optional/repetition where needed, span capture, and diagnostic labeling.
  - Grammar functions for modules, exports, imports, declarations, expressions,
    and source types must be expressed by composing these parser combinators,
    not by one large case split over fixture-specific token-stream
    constructors.
- Real scanning path:
  - Replace success-path tokenizers that check
    `stringIndexOf source <exactFixtureSourceText>` and return a prebuilt token
    stream with a source-scanning lexer/token stream over the broad string
    substrate.
  - Golden fixture text and expected projection files remain valid test
    oracles, but exact fixture recognition must not be the parser's normal
    success mechanism.
- Carried syntax coverage:
  - Route the round-304 through round-308 positive fixtures through the shared
    parser library in the same consolidation round.
  - If round-309 is recovered and merged before this item starts, route the
    data-declaration constructor-span fixture through the shared library too.
    If round-309 is abandoned or replanned, do not add another data-declaration
    one-off before consolidation.
  - Preserve the same source-span projection contract for module headers,
    exports, imports, definitions, let/lambda/application expressions,
    annotations, source types, and carried diagnostics.
- Diagnostics:
  - Negative evidence must call the same shared tokenizer/parser path and render
    stable parser-owned diagnostic categories and spans.
  - Do not match or re-export exact Megaparsec prose as the `.mlfp` parser
    diagnostic contract.
- Boundaries:
  - Keep parser combinators and parser cursor helpers parser-owned; do not
    widen Prelude with a generic parser API.
  - The parser monad is a parser-library implementation contract, not a new
    public Prelude abstraction and not a permission to add generic effect,
    typeclass, or application-level monad scope outside parser ownership.
  - Do not introduce checker, resolver, backend, package manager, platform,
    driver, proof, formatting, documentation generator, or REPL scope.

Grouped verification strategy for `item-310-parser-library-consolidation`:

- Shared-entrypoint group: at least two carried positive fixtures fail RED
  until they call the same shared parser entrypoint, then the consolidation
  expands to all carried positive fixtures in the same round.
- Parser-combinator group: grammar functions use the parser-owned monadic
  sequencing/combinator layer for state threading, failure propagation, span
  capture, choice, and labeling; review rejects a direct fixture-token-stream
  case tree as the main parser architecture.
- Lexer/token group: fixture exact-source token recognition is removed from the
  success path and token spans still match the committed parser projections.
- Syntax group: carried module/import/value/let/lambda/application/annotation
  syntax stays green through the shared parser library.
- Diagnostic group: carried malformed inputs fail through the shared parser
  path with stable categories and spans.
- Regression group: direct package smokes for every carried parser-parity
  fixture stay green.
- Closeout group: `git diff --check`, focused parser-parity matchers,
  `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh`.

Milestone-4 closeout remains broader than this consolidation item. Later parser
rounds may add grammar families, but they must extend the shared parser library
and add fixtures against that library instead of returning to the
one-parser-per-test package pattern.

#### Candidate Direction: Canonical Parser Parity

- Direction id: `direction-4a-canonical-parser-parity`
- Summary: implement the full canonical parser in `.mlfp` by growing one
  shared parser-owned parser-combinator library with explicit parser-monad
  sequencing and comparing its behavior against the current canonical parser.
- Why it matters now: the compiler source package cannot own language syntax
  until parser behavior is implemented in `.mlfp`.
- Preconditions: broad string/`Char` native support is complete; conformance
  corpus has parser-relevant fixtures and diagnostics; inspect the existing
  fixture-specific parser-parity packages and remove the exact-source
  token-stream success path as part of the next consolidation item.
- Parallel hints: serial with text substrate; docs may trail after evidence.
- Boundary notes: no checker, backend, package manager, or self-boot driver
  scope. Keep parser helpers parser-owned rather than adding a generic Prelude
  parser API or broad monad/effect API.
- Extraction notes: select `item-310-parser-library-consolidation` as the next
  normal implementation item after round-309 recovery/closeout. Subsequent
  syntax work must extend the shared parser library with committed
  parse/diagnostic evidence, not create one parser package per test fixture.

### [pending] 5. Self-Boot Platform Contract Implementation

- Milestone id: `milestone-5`
- Depends on: `milestone-2`, `milestone-3`
- Intent: implement the shared platform contracts required by both compiler
  implementations before a self-boot proof can be meaningful.
- Completion signal: target-scoped stable public `.mlfp` ABI, managed GC
  contract, shared Rust trusted substrate path, canonical substrate ABI
  declarations, generated binding validation, exact local package manifests and
  checked locks, Prelude package/substrate split, host toolchain identity,
  native/link/execution records, filesystem/subprocess/hash APIs, and
  ambient-input policy are implemented and validated where required.
- Parallel lane: `lane-platform-substrate`
- Coordination notes: trusted substrate provides capabilities only. Compiler
  language semantics remain in the `.mlfp` compiler package. Before selecting
  implementation rounds for this milestone, reassess the then-current
  `CONTEXT.md`, accepted ADRs, readiness ledger, architecture docs, tests, and
  codebase boundaries, then split the milestone into smaller roadmap
  milestones when those sources expose distinct ABI, runtime/GC/FFI,
  substrate, package-lock, toolchain, or proof-record work fronts.

#### Completion Pointers: milestone-5

None yet.

#### Candidate Direction: Platform Contract Substrate

- Direction id: `direction-5a-platform-contract-substrate`
- Summary: implement the stable ABI, runtime/GC/FFI, substrate declaration,
  lock, manifest, toolchain, link, native execution, and ambient-input
  contracts needed by the first proof.
- Why it matters now: first self-boot cannot compare compilers that consume
  different runtime, package, toolchain, or substrate contracts.
- Preconditions: conformance corpus exists; broad text/native behavior has
  enough coverage to protect compiler frontend workloads.
- Parallel hints: platform subareas may be planned as serial vertical slices;
  avoid concurrent edits to shared runtime or ABI contracts without explicit
  worker ownership.
- Boundary notes: no remote solver, package publishing, or host-owned canonical
  data formats.
- Extraction notes: begin with canonical declarations and drift validation
  before expanding generated bindings and runtime coverage. If the current docs
  and code show that ABI, GC/FFI, package locks, toolchain identity, or
  proof-record contracts need different verification surfaces, publish a
  semantic roadmap revision that splits this milestone before implementation.

### [pending] 6. Compiler Source Package In `.mlfp`

- Milestone id: `milestone-6`
- Depends on: `milestone-4`, `milestone-5`
- Intent: implement the `.mlfp` compiler source package under the Self-Boot
  Compiler Semantics Boundary.
- Completion signal: parser behavior, name resolution, checking,
  semantic-interface read/write, backend artifact-emission decisions,
  diagnostics, package validation, and driver-facing compiler semantics are
  implemented in `.mlfp` source modules and validated against the shared corpus
  and platform contracts.
- Parallel lane: `lane-compiler-package`
- Coordination notes: trusted substrate may provide filesystem, subprocess,
  hashing, linker, runtime, GC, and FFI primitives; it must not implement
  compiler semantics. Before implementation rounds begin, use the then-current
  docs, ADRs, `CONTEXT.md`, conformance corpus, parser/platform evidence, and
  codebase ownership boundaries to split this milestone into smaller compiler
  phase milestones when parser integration, resolution, checking, interfaces,
  backend artifact decisions, diagnostics, package validation, or driver-facing
  semantics no longer fit one bounded execution front.

#### Completion Pointers: milestone-6

None yet.

#### Candidate Direction: Compiler Package Semantics

- Direction id: `direction-6a-compiler-package-semantics`
- Summary: build the compiler source package in `.mlfp`, starting from parser
  ownership and extending through resolver/checker/interface/backend decision
  semantics.
- Why it matters now: the proof target is the compiler package, not a
  Haskell-owned wrapper or trusted-substrate implementation of compiler logic.
- Preconditions: parser parity complete; platform contracts implemented enough
  for compiler package inputs, locks, ABI, native behavior, and substrate use.
- Parallel hints: likely serial by compiler phase until interfaces between
  parser, resolver, checker, and backend decisions are stable.
- Boundary notes: no optimizer controls, formatter, docs generator, remote
  package manager, or broad REPL scope.
- Extraction notes: choose vertical compiler components that produce
  conformance-visible behavior and semantic artifacts. If phase boundaries or
  semantic artifact contracts are not clear enough for one round, require a
  semantic roadmap revision that splits the compiler package work before
  implementation continues.

### [pending] 7. Small Real Self-Boot Driver

- Milestone id: `milestone-7`
- Depends on: `milestone-6`
- Intent: implement the first compiler driver with `check`, `emit-backend`,
  `emit-native`, and `run-conformance` over explicit local manifests and
  checked locks.
- Completion signal: the `.mlfp` compiler driver consumes explicit manifests,
  validates checked locks, invokes the implemented compiler package semantics,
  emits backend/native artifacts through recorded command modes, and runs the
  shared conformance corpus without owning proof orchestration itself.
- Parallel lane: `lane-proof-driver`
- Coordination notes: `prove-self-boot` is not required in the first compiler
  driver. A host proof runner may orchestrate, but it cannot perform compiler
  semantics. If current manifest, lock, command-record, native-emission, or
  conformance-runner contracts show separate driver work fronts, split this
  milestone by command mode or proof-record boundary before executing broad
  driver work.

#### Completion Pointers: milestone-7

None yet.

#### Candidate Direction: First Driver Modes

- Direction id: `direction-7a-first-driver-modes`
- Summary: implement the bounded first driver commands and lock/manifest
  consumption required by the stage sequence.
- Why it matters now: stage 0 and stage 1 need the same driver command surface
  before proof orchestration can be trusted.
- Preconditions: compiler source package semantics complete enough for the
  selected command modes.
- Parallel hints: serial by command mode unless manifests/locks and compiler
  package boundaries are already stable.
- Boundary notes: no package publishing, remote dependency commands, REPL,
  formatting, documentation generation, or optimizer controls.
- Extraction notes: start with `check` and conformance execution, then native
  emission once record and link contracts are in place. Derive any finer split
  from the then-current manifest/lock/proof-record contracts and codebase
  driver boundaries.

### [pending] 8. First Self-Boot Proof

- Milestone id: `milestone-8`
- Depends on: `milestone-7`
- Intent: run the First Self-Boot Stage Sequence and record auditable proof
  evidence.
- Completion signal: stage 0 uses the Haskell compiler to consume checked
  locks, platform contracts, shared packages, and the `.mlfp` compiler source
  package to produce the stage-1 native compiler plus normalized semantic
  artifacts; stage 1 uses that native compiler on the same locked inputs and
  substrate fingerprint to produce stage-2 artifacts; both stages pass
  stage-shared conformance first; normalized semantic artifacts compare equal;
  proof manifest records action IDs, command/link/native execution/conformance
  records, stage-owned outputs, hashes, and classified failures if any.
- Parallel lane: `lane-proof-driver`
- Coordination notes: native object and executable bytes are regenerated and
  recorded, but not the equality oracle. Before the final proof attempt, use
  the current proof runner, platform contracts, conformance suite, normalized
  semantic artifact format, and failure taxonomy to split proof-runner
  hardening, conformance-first execution, manifest validation, and final
  stage-equivalence closeout when they need separate evidence.

#### Completion Pointers: milestone-8

None yet.

#### Candidate Direction: Conformance First Stage Sequence

- Direction id: `direction-8a-conformance-first-stage-sequence`
- Summary: run and harden the two-stage proof sequence, conformance-first gate,
  proof manifest, and normalized semantic artifact comparison.
- Why it matters now: this is the first point where the repo may make a Full
  Self-Boot claim.
- Preconditions: first driver modes and platform contracts complete; shared
  conformance corpus and substrate ABI conformance layer pass for both stages.
- Parallel hints: proof execution is serial. Failure investigation may branch
  by classified failure only after the proof manifest records the boundary.
- Boundary notes: do not compare native object or executable bytes as the
  first-proof oracle.
- Extraction notes: first runs may be proof-runner hardening rounds before the
  final stage-equivalence closeout. If proof records expose distinct failure
  classes or missing contracts, publish a semantic roadmap revision that splits
  those proof fronts instead of treating the final proof as one oversized
  round.
