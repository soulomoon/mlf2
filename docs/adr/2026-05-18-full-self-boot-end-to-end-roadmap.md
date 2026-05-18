# ADR: Stage Full Self-Boot End-to-End Roadmap

Date: 2026-05-18
Status: Accepted

## Context

The self-boot discussion has produced several durable decisions and one living
readiness ledger:

- `CONTEXT.md` owns the glossary for **Full Self-Boot**, **Self-Boot Scope
  Boundary**, **Self-Boot Compiler Semantics Boundary**, **Self-Boot Total
  Native Coverage**, **Self-Boot Stage Equivalence**, and related terms.
- `docs/mlfp-self-boot-readiness.md` records current evidence, current gaps,
  seed-fixture layer classification, and next-stage readiness. It is a living
  ledger, not a decision record.
- `docs/adr/2026-05-18-file-based-conformance-before-self-boot.md` decides that
  shared file-based compiler conformance migrates before self-boot
  implementation.
- `docs/adr/2026-05-18-native-broad-string-library-before-parser-parity.md`
  decides that native-capable broad string and `Char` support precedes full
  canonical `.mlfp` parser parity.
- `docs/adr/2026-05-18-self-boot-platform-contract.md` decides the shared
  platform contract for ABI, runtime, GC, FFI, package locks, substrate,
  toolchain, proof evidence, and compiler-semantics boundaries.

Those documents are individually useful, but future implementers need one
durable end-to-end ordering. Without that ordering, work could start directly on
the `.mlfp` compiler source, parser, ABI, or proof runner while skipping the
shared evidence and platform prerequisites that make the self-boot claim
meaningful.

## Decision

Full Self-Boot will be implemented as a staged roadmap. The roadmap proves the
`.mlfp` compiler source package can rebuild itself under shared platform
contracts; it is not whole-platform self-hosting, and it is not a compiler-only
rewrite.

The roadmap order is:

1. **Readiness Ledger Baseline**

   Keep `docs/mlfp-self-boot-readiness.md` as the living current-state ledger.
   It records what the current Haskell compiler, package substrate, backend,
   native runner, and compiler frontend seed prove. It does not replace ADRs.

2. **Pre-Self-Boot Test Migration**

   Implement the **Shared File-Based Compiler Conformance Corpus** from
   `docs/adr/2026-05-18-file-based-conformance-before-self-boot.md`.
   Behavior-level `.mlfp` fixtures migrate one by one into a file-based corpus
   with fixture metadata, per-command expected output files, committed goldens,
   and explicit output normalization. This is the first implementation family.

3. **Native-Capable Broad Text Substrate**

   Implement the **Broad String Library** and **Unicode Scalar `Char`** contract
   from
   `docs/adr/2026-05-18-native-broad-string-library-before-parser-parity.md`.
   String and character operations must work through source checking,
   `run-program`, backend emission, object generation, and native execution.

4. **Full Canonical `.mlfp` Parser Parity**

   Build the parser-owned `.mlfp` combinator core and full canonical `.mlfp`
   parser on the broad text substrate. The parser produces the same parsed
   program syntax artifact and source spans as the current canonical parser. It
   does not include resolve, checking, backend lowering, driver behavior, or
   self-hosting.

5. **Self-Boot Platform Contract Implementation**

   Implement the prerequisites decided by
   `docs/adr/2026-05-18-self-boot-platform-contract.md`: stable public `.mlfp`
   ABI, managed GC contract, shared Rust trusted substrate through the shared
   stable substrate ABI path, canonical substrate ABI declarations, checked
   generated bindings, local package manifests, checked package locks, exact
   locked local identities, shared local `.mlfp` package set, Prelude
   package/substrate split, host toolchain identity, native/linker records,
   filesystem/subprocess/hash substrate APIs, and ambient-input policy.

6. **Compiler Source Package In `.mlfp`**

   Implement the `.mlfp` compiler source package under the **Self-Boot Compiler
   Semantics Boundary**. Parser behavior, name resolution, checking,
   semantic-interface reading/writing, backend artifact-emission decisions,
   diagnostics, package validation, and driver behavior belong to the compiler
   package. Trusted substrate provides capabilities but does not decide `.mlfp`
   language meaning.

7. **Small Real Self-Boot Driver**

   Implement the first **Self-Boot Compiler Driver** with `check`,
   `emit-backend`, `emit-native`, and `run-conformance` over explicit package
   manifests. Package publishing, remote dependency commands, REPL support,
   formatting, documentation generation, and optimizer controls remain outside
   first self-boot.

8. **First Self-Boot Proof**

   Run the **First Self-Boot Stage Sequence**. Stage 0 is the Haskell compiler
   consuming checked locks, platform contracts, shared local `.mlfp` packages,
   and the `.mlfp` compiler source package to produce the stage-1 native
   compiler plus normalized semantic artifacts. Stage 1 is that native compiler
   consuming the same locked inputs and substrate fingerprint to produce stage-2
   artifacts. The proof uses **Stage-Shared Self-Boot Conformance** first, then
   compares **Normalized Self-Boot Semantic Artifacts**. Native object and
   executable bytes are regenerated and recorded but are not the equality
   oracle.

## Rejected Alternatives

- Start directly with the `.mlfp` compiler implementation. That would lack a
  shared conformance oracle and would make Haskell-vs-`.mlfp` divergence hard to
  diagnose.
- Build parser parity before broad native string support. That would hide a
  compiler-front-end text substrate requirement inside parser-private helpers.
- Treat the readiness ledger as the roadmap decision. The ledger changes as
  implementation evidence changes; the staged ordering belongs in an ADR.
- Treat self-boot as whole-platform self-hosting. First self-boot permits a
  versioned trusted substrate and proves the compiler package over that
  substrate.
- Treat self-boot as a compiler-only rewrite. The compiler package proof is not
  meaningful without stable shared ABI, runtime, GC, FFI, package, lock,
  substrate, conformance, and toolchain contracts.
- Use the proof runner, trusted substrate, or Haskell helpers to perform
  compiler semantics. Those tools may provide orchestration and platform
  capabilities, but language semantics belong to the stage compiler drivers.

## Consequences

- Implementation starts with the file-based conformance corpus, not direct
  self-boot implementation.
- The readiness ledger remains useful as current-state evidence and should
  point to this ADR for end-to-end sequencing.
- Each later roadmap family can be implemented independently, but publication of
  a first self-boot claim requires all prerequisite families in this ADR.
- Future ADRs may refine a specific family, but they must preserve this ordering
  unless they explicitly replace this ADR.
