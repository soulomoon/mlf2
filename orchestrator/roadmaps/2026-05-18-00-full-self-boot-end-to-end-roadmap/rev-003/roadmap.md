# Full Self-Boot End-to-End Roadmap

Roadmap family: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
Revision: `rev-003`
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
  corpus, native-capable broad string/`Char` support, full canonical `.mlfp`
  parser parity, implemented platform ABI/runtime/GC/FFI/package/toolchain
  contracts, a compiler source package in `.mlfp`, a bounded first compiler
  driver, and a first proof run that passes stage-shared conformance before
  comparing normalized semantic artifacts.
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
  - broad Unicode-scalar `Char` and `String` library with native slicing and
    classification;
  - full canonical `.mlfp` parser parity built on parser-owned combinators and
    the broad text substrate;
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
- Milestone 3 implements native-capable broad string and Unicode scalar `Char`
  support before full parser parity.
- Milestone 4 implements full canonical `.mlfp` parser parity on the broad text
  substrate and does not absorb checker, backend, driver, or package-manager
  work.
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

### [in-progress] 3. Native-Capable Broad Text Substrate

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: implement the Prelude-level Broad String Library and Unicode scalar
  `Char` contract through source checking, interpreter/runtime, backend
  emission, object generation, and native execution.
- Completion signal: `.mlfp` source supports `Char` literals, `String` as a
  Unicode-scalar sequence, `String`/`List Char` conversion, slicing,
  classification, search, formatting, and parser-needed cursor operations with
  focused and full validation across interpreter and native paths.
- Parallel lane: `lane-text-parser`
- Coordination notes: locale-sensitive APIs and regex remain out of scope.
  Parser combinators remain parser-owned modules, not Prelude-level generic
  parser API.

#### Completion Pointers: milestone-3

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

#### Candidate Direction: Broad String And Char Substrate

- Direction id: `direction-3a-broad-string-char-substrate`
- Summary: add native-capable Unicode scalar `Char` and broad string
  operations needed before source-text parser parity.
- Why it matters now: parser parity must not hide text requirements inside
  parser-private helpers or interpreter-only support.
- Preconditions: conformance corpus is stable enough to cover public behavior;
  inspect Prelude, parser seed gaps, backend/native string lowering, and
  runtime value representation.
- Parallel hints: serial with parser parity until the text substrate is native
  proven.
- Boundary notes: exclude locale and regex unless a semantic roadmap update
  selects them.
- Extraction notes: prove source,
  interpreter, backend, object, and native behavior together.

### [pending] 4. Full Canonical `.mlfp` Parser Parity

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: build the parser-owned combinator core and full canonical `.mlfp`
  parser on top of the broad text substrate.
- Completion signal: `.mlfp` parser source produces the same parsed program
  syntax artifact and source spans as the current canonical parser for the
  selected corpus, with conformance-backed pass/fail evidence and no checker,
  backend, or driver overclaim.
- Parallel lane: `lane-text-parser`
- Coordination notes: this milestone is parser parity only. Name resolution,
  checking, backend lowering, driver behavior, and self-hosting belong later.

#### Completion Pointers: milestone-4

None yet.

#### Candidate Direction: Canonical Parser Parity

- Direction id: `direction-4a-canonical-parser-parity`
- Summary: implement the full canonical parser in `.mlfp` using parser-owned
  modules and compare it against the current canonical parser behavior.
- Why it matters now: the compiler source package cannot own language syntax
  until parser behavior is implemented in `.mlfp`.
- Preconditions: broad string/`Char` native support is complete; conformance
  corpus has parser-relevant fixtures and diagnostics.
- Parallel hints: serial with text substrate; docs may trail after evidence.
- Boundary notes: no checker, backend, package manager, or self-boot driver
  scope.
- Extraction notes: expand syntax by stable grammar families with committed
  expected parse/diagnostic evidence.

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
