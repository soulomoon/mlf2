# ADR: Define Self-Boot Platform Contract

Date: 2026-05-18
Status: Accepted

## Context

Full Self-Boot is a compiler milestone with explicit platform prerequisites,
not whole-platform self-hosting and not a compiler-only rewrite. The proof
target is the `.mlfp` compiler source package, but the future `.mlfp` compiler
and the current Haskell compiler must compile, run, and compare programs
against the same platform contracts. Without a shared platform contract, the two
implementations could accidentally prove parity through different runtime
helpers, different package inputs, different ABI bindings, or different lock
semantics.

First self-boot still permits trusted substrate. Runtime, GC, FFI shims,
primitive/native helpers, host toolchain, and the system linker do not need to
be implemented in `.mlfp` for the first proof. They must, however, be explicit,
versioned, fingerprinted, and reached through the same stable contracts by both
compiler implementations. They provide platform capabilities, not compiler
semantics: parser behavior, name resolution, checking, semantic-interface
reading/writing, backend artifact-emission decisions, diagnostics, package
validation, and driver behavior belong to the `.mlfp` compiler source package.

## Decision

First self-boot requires a target-scoped stable public `.mlfp` ABI. The ABI is
scoped by ABI version, target triple, and substrate fingerprint. It includes
public heap layout boundaries, general user-facing FFI, stable FFI value shapes,
ownership/lifetime rules, error/unwinding rules, export/callback wrappers,
manifest-owned linker inputs, explicit FFI effects, and an explicit public-ABI
runtime context.

Native execution requires a managed GC for heap-allocated `.mlfp` values. The
first GC is a non-moving precise tracing collector with compiler-known roots and
trace metadata. Conservative stack scanning is not the correctness model.
Foreign code interacts with GC-managed values only through stable ownership,
pin/copy, or opaque-handle contracts.

The runtime/GC/FFI substrate may remain Rust trusted substrate for first
self-boot. The Rust substrate is shared by both the Haskell compiler
implementation and the future `.mlfp` compiler implementation. Proof-path
conformance, native behavior, and self-boot evidence must use the same stable
substrate ABI path. Direct Haskell calls into Rust substrate internals are
allowed only as marked dev/test scaffolding outside behavior parity, native
conformance, and self-boot equivalence.

Canonical substrate ABI declarations are the single source of truth for the
shared substrate ABI path. They live in a dedicated versioned platform
substrate contract package inside the repository, using a small `.mlfp`-owned
canonical data format rather than JSON, TOML, YAML, or a host-owned parser.
Rust exported symbols, Haskell FFI bindings, `.mlfp` foreign imports, and
substrate conformance fixture scaffolds are generated from or validated against
the same declarations.

Generated Rust/Haskell/`.mlfp` bindings and substrate fixture scaffolds are
checked in for reviewability and bootstrap ergonomics. Proof-path builds and
tests validate checked generated artifacts against the canonical declarations.
Drift is a hard failure; regeneration is an explicit reviewed command.

Every substrate ABI declaration set carries an explicit ABI version. Additive
compatible changes may advance a minor or patch-style version within policy.
Representation, ownership, calling convention, GC root/handle, error, symbol,
or layout-breaking changes require a major ABI version bump and invalidate
artifact reuse across that boundary.

High-level Prelude/library source packages belong in a shared local `.mlfp`
package set where feasible. Primitive/native operations, GC/runtime hooks, and
low-level FFI shims remain in the shared Rust trusted substrate. Both sides are
declared explicitly in manifests and substrate fingerprints.

First self-boot uses exact locked local package identities only. Checked package
locks are committed for proof fixtures and reproducibility, using a
`.mlfp`-owned canonical package-lock format. Proof-path builds recompute lock
facts from manifests, local paths, source hashes, dependency interface hashes,
required ABI version, and substrate fingerprint, then validate the checked lock.
Drift is a hard failure. The first `.mlfp` compiler driver must consume,
validate, and deterministically recompute checked locks; full solver-style lock
creation from version ranges is outside first self-boot.

A shared substrate ABI conformance layer is required before self-boot evidence.
Both compiler implementations must exercise the same stable substrate ABI
contracts for runtime init/attach, strings and buffers, GC roots, pins and
handles, FFI import/export/callback/error cases, filesystem, subprocess, hash
primitives, and linker invocation records.

Host native toolchain identity is part of the stage-consumed substrate
fingerprint. The fingerprint declares LLVM or backend tool identity, assembler,
system linker, target triple, sysroot, relevant system libraries, native codegen
settings, linker mode, and other native toolchain configuration consumed by a
bootstrap stage. Command records capture exact invocations, but they do not
replace the declared toolchain contract.

Toolchain proof identity uses resolved identities rather than version strings
alone. The substrate fingerprint records resolved tool paths plus content hashes
where practical for LLVM/backend tools, assembler, and system linker, plus
target triple, sysroot identity, and relevant system library identities. Version
strings remain useful diagnostics, but they are not sufficient proof identity.

First self-boot follows a fixed two-stage proof sequence. Stage 0 is the
Haskell compiler consuming checked package locks, the platform contract, shared
local `.mlfp` packages, and the `.mlfp` compiler source package to produce the
stage-1 native `.mlfp` compiler plus normalized semantic artifacts. Stage 1 is
that native `.mlfp` compiler consuming the same locked inputs and substrate
fingerprint to produce stage-2 artifacts. The proof compares exact conformance
outputs and normalized semantic artifacts, not native object or executable
bytes.

First self-boot does not require deterministic native object or executable bytes
for a fixed toolchain. Native artifacts must be regenerated, recorded through
normalized command and canonical link records, runnable where the proof
requires execution, and produced under the declared host toolchain contract.
Byte-level native determinism is future hardening, not first self-boot evidence.

Native link records are canonical proof-manifest entries even though native
bytes are not compared. Each link record includes linker argv, target triple,
linker mode, object inputs, resolved runtime, system, static, dynamic, and
framework library identities, library search paths, rpath or install-name data
where relevant, output artifact paths and hashes, and exit status. Link records
audit native production without making object or executable byte equality the
self-boot oracle.

Resolved linked-library identity means the actual linked file or framework
identity where practical, including path, content hash or platform package
identity, and whether the library was linked statically or dynamically. `-l`
names and search paths alone are not enough proof state.

Native execution records also capture dynamic loader resolution for executed
binaries. For dynamically linked stage compilers and conformance binaries, the
proof manifest records executable identity, loader environment, resolved dynamic
libraries and frameworks where practical, and any rpath or install-name
resolution used at execution time. Link records explain what was produced;
execution records explain what was actually loaded when the native artifact ran.

Loader-affecting environment variables such as `LD_LIBRARY_PATH`,
`DYLD_LIBRARY_PATH`, `DYLD_FALLBACK_LIBRARY_PATH`, and platform equivalents must
be explicitly declared by the manifest or host toolchain contract, or scrubbed
before native execution. Native execution records capture the normalized loader
environment. Ambient inherited loader state must not influence self-boot proof
behavior.

Undeclared or unscrubbed loader-affecting environment state is a distinct proof
failure class: loader environment violation. It is not substrate mismatch and
not stage command failure; the fix is to declare or scrub the loader-affecting
state and record the normalized loader environment.

Proof-affecting ambient inputs such as wall-clock time, random seeds, timezone,
locale, and platform equivalents are either explicitly declared with expected
normalization or scrubbed before compiler stages and conformance fixtures run.
Compiler stages and conformance fixtures must not depend on ambient time,
randomness, timezone, or locale. Inherited, undeclared, unnormalized, or
unscrubbed proof-affecting ambient state is a distinct proof failure class:
ambient input violation.

Unresolved or unrecordable dynamic loader resolution is a proof failure that
must be fixed. The target, toolchain contract, runner, or link mode must make
loader environment plus resolved dynamic library, framework, rpath, and
install-name facts recordable before the proof can pass.

The same declared conformance suite is run by stage 0 and stage 1 before
stage-2 artifacts are accepted. That suite includes both the shared file-based
compiler conformance corpus and the shared substrate ABI conformance layer,
using the same normalization and committed-golden rules for both stages. Stage 0
proves the Haskell compiler still matches the shared oracle while producing the
stage-1 compiler; stage 1 proves the generated `.mlfp` compiler matches that
same oracle before normalized stage artifacts are compared. Stage-inapplicable
fixtures must be declared in metadata, not skipped ad hoc.

The proof gate is conformance-first. A stage that fails the shared conformance
suite does not proceed to normalized semantic artifact comparison; it is
reported as a compiler behavior or substrate ABI oracle failure. Normalized
semantic artifact comparison is only meaningful after both stages pass the
declared conformance suite.

Each proof stage writes to a fresh stage-owned output directory. Stages may
share declared inputs such as checked locks, source packages, substrate
fingerprints, platform contract packages, and checked generated bindings. They
must not share semantic-interface, normalized-semantic-artifact, backend IR,
object, executable, link-record, or conformance-output caches.

Proof paths are root-bounded. Source and input paths normalize only through
manifest-declared package, fixture, platform-contract, and toolchain roots.
Generated paths normalize only through the stage-owned output root. Symlink and
case-sensitivity behavior is declared by the manifest or host toolchain
contract. A source or input path escaping declared roots is input drift; a
generated output path escaping the stage-owned output root is proof-runner
failure.

Each self-boot proof run emits a canonical structured proof manifest in the
same `.mlfp`-owned canonical data format family as manifests, package locks,
and substrate ABI declarations. The manifest records stage inputs, substrate
fingerprint, package locks, normalized command records, canonical link records,
native execution records, stage-owned output directories, conformance result
hashes, normalized semantic artifact hashes, and final comparison status. Each
command record includes a proof action ID, argv, cwd, normalized environment,
stdin source or hash, stdout/stderr hashes or artifact paths, exit status,
relevant tool identity, owning stage, stage-owned output directory, and
compiler-driver mode when the command invokes a stage compiler driver. Each
command, link, native execution, conformance, and comparison record carries a
stable proof action ID such as `stage0.check.compiler`,
`stage0.emit-native.compiler`, `stage1.run-conformance`, or
`stage1.emit-backend.compiler`; failures reference action IDs rather than list
order. First self-boot driver modes are `check`,
`emit-backend`, `emit-native`, and `run-conformance`; later modes must be
explicitly declared before they appear in proof manifests.
It also records a stable machine-readable failure taxonomy for input drift, lock
drift, substrate mismatch, ambient input violation, stage command failure,
conformance failure, normalized semantic artifact mismatch, loader environment
violation, native loader resolution failure, proof-runner failure, and
unsupported or inapplicable fixture metadata failure. Both compiler
implementations must be able to parse, emit, hash, and compare it with the same
canonical serialization rules. It is the auditable proof artifact for a run;
terminal logs are supporting diagnostics, not the proof contract.

The first proof may be orchestrated by a host-side proof runner. The runner may
invoke stage compiler drivers, run the conformance-first gate, compare
normalized semantic artifacts, manage stage-owned output directories, and emit
the proof manifest. It must not perform compiler semantics itself: parsing,
name resolution, checking, semantic-interface reading/writing, package
validation, backend artifact-emission decisions, native emission, diagnostics,
driver behavior, and conformance behavior remain owned by the stage compiler
drivers and their recorded command lines. The first `.mlfp` compiler driver
therefore does not need to own a `prove-self-boot` command.

The proof manifest records proof-runner identity and configuration: runner
source/version/hash, comparison policy, normalization policy, command templates,
and runner configuration. This is proof tooling evidence, not part of the
stage-consumed substrate fingerprint by default. It enters the substrate
fingerprint only when runner configuration changes compiler/runtime inputs
consumed by a stage.

## Rejected Alternatives

- Treat Haskell calls into Rust internals as proof evidence. That would create a
  Haskell-only substrate path and fail to prove the future `.mlfp` compiler uses
  the same runtime contract.
- Maintain Rust headers, Haskell bindings, `.mlfp` imports, and fixtures by
  hand. That would make ABI drift a review problem instead of a hard proof-path
  failure.
- Use JSON, TOML, YAML, or a host-owned parser as the canonical substrate ABI,
  package-lock, or proof-manifest format. That would add external syntax and
  parser dependencies to the self-boot proof path.
- Treat generated bindings or package locks as build-time scratch output only.
  That would reduce reviewability and make bootstrap inputs harder to audit.
- Accept version ranges or solver-selected packages in the first proof. Solver
  behavior is package-management work, not self-boot evidence.
- Compare native object or executable bytes between stages, or require
  deterministic object/executable bytes as first-proof evidence. The first proof
  is semantic: exact conformance outputs plus normalized semantic artifacts.
- Treat the whole Prelude as ambient trusted substrate. High-level Prelude and
  library source should be shared `.mlfp` package input where feasible; only the
  primitive/native hooks stay in trusted substrate.

## Consequences

- The platform substrate contract package, canonical declaration format,
  generated-binding validator, ABI version policy, checked package locks, and
  lock validator become prerequisites for first self-boot evidence.
- The Haskell and future `.mlfp` compiler implementations must consume the same
  shared local `.mlfp` package set and exact locked closure in proof runs.
- The Rust substrate remains allowed for first self-boot, but only as explicit,
  shared, versioned trusted substrate behind stable `.mlfp` ABI contracts.
- Resolved LLVM/backend tools, assembler, linker, target triple, sysroot,
  relevant system libraries, native codegen settings, and linker mode are part
  of the stage-consumed substrate fingerprint; version strings alone are not
  proof identity.
- Stage-0 and stage-1 builds must consume the same checked locks, platform
  contract, shared local packages, and substrate fingerprint; stage-local input
  drift invalidates the proof.
- Stage-0 and stage-1 generated outputs must be isolated in fresh stage-owned
  output directories; cross-stage artifact cache reuse invalidates the proof.
- Native object/executable bytes are not required to be deterministic for first
  self-boot, but native artifacts must be regenerated, recorded, runnable where
  required, and produced under the declared toolchain contract.
- Native link records are canonical proof-manifest entries and include linker
  argv, target triple, linker mode, object inputs, resolved linked-library
  identities, search paths, rpath/install-name data where relevant, output
  artifact paths and hashes, and exit status.
- Resolved linked-library identities record actual linked files or framework
  identities where practical, including path, content hash or platform package
  identity, and static or dynamic link mode.
- Native execution records capture loader environment, resolved dynamic
  libraries/frameworks where practical, and rpath/install-name resolution used
  when stage compilers or conformance binaries run.
- Loader-affecting environment variables are manifest/toolchain-declared or
  scrubbed, and normalized loader environment is recorded for native execution.
- Undeclared or unscrubbed loader-affecting environment state is a separate
  proof failure class: loader environment violation.
- Proof-affecting time, randomness, timezone, locale, and equivalents are
  declared with expected normalization or scrubbed; violations are recorded as
  ambient input violations.
- Unresolved or unrecordable dynamic loader resolution is a proof failure that
  must be fixed before the proof can pass.
- Stage-0 and stage-1 conformance runs must use the same declared suite before
  normalized semantic artifacts are accepted.
- The declared stage-shared suite includes compiler behavior corpus fixtures and
  substrate ABI conformance fixtures; fixture applicability belongs in metadata.
- Failed conformance stops the self-boot proof before normalized semantic
  artifact comparison.
- Successful or failed proof runs produce a structured proof manifest recording
  the evidence needed to audit the run.
- Proof-manifest failures use a stable machine-readable taxonomy instead of a
  generic proof-failed bucket.
- Stable proof action IDs make proof manifests diffable and make failures point
  to precise stage actions instead of list positions.
- A host-side proof runner is acceptable for first self-boot orchestration, but
  it is not a compiler-semantics implementation and does not replace recorded
  stage compiler-driver commands.
- Proof-runner identity and configuration are recorded in the proof manifest as
  tooling evidence, separately from the substrate fingerprint unless they change
  stage-consumed compiler/runtime inputs.
- First self-boot remains smaller than whole-platform self-hosting, but larger
  than a compiler-only rewrite because ABI, runtime, GC, FFI, and package-lock
  contracts must be stable enough for both implementations.
- Trusted substrate can provide runtime/system capabilities, but it cannot own
  `.mlfp` compiler-language semantics.
