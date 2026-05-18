# mlf2 Thesis Pipeline

Domain language for the paper-faithful MLF to xMLF implementation and its checked `.mlfp` pipeline.

## Language

**Acyclic Base Constraint**:
The post-acyclicity constraint graph that preserves original binding-tree ownership for thesis ga-prime recovery.
_Avoid_: raw graph, old constraint

**Presolution View**:
The canonical read model built from a presolution or solved snapshot for elaboration and reification.
_Avoid_: solved wrapper, query bag, chi-query adapter

**Snapshot Finalization**:
The construction authority that turns a constraint snapshot plus canonical equivalence data into a **Presolution View** and/or solved handle. `MLF.Constraint.Finalize` owns this construction; `MLF.Constraint.Presolution.View` owns the read-model surface.
_Avoid_: snapshot materialization, view setup, solved wrapper construction

**Legacy Surface Retirement**:
The cleanup direction that deletes outdated compatibility surfaces, including internal solved/view adapters, parser aliases, and parser support for non-canonical legacy syntax, when the paper-backed pipeline no longer needs them.
_Avoid_: compatibility preservation, syntax grandfathering, parser transition mode, ASCII alias mode

**Full Canonical `.mlfp` Parser Parity**:
The compiler-in-`.mlfp` frontend target where a `.mlfp` parser accepts the complete canonical `.mlfp` syntax and produces the same program syntax artifact as the current canonical parser, before resolve, checking, elaboration, or driver self-hosting.
_Avoid_: seed grammar growth, symbolic parser seed, legacy parser compatibility, checker self-hosting

**Full Self-Boot**:
The compiler-in-`.mlfp` success state where a `.mlfp` compiler source package compiles to a native compiler executable, and that executable compiles the same compiler source package again with equivalent outputs and diagnostics, under explicit shared platform prerequisites.
_Avoid_: parser-only self-hosting, checker-only milestone, first-stage bootstrap smoke, whole-platform self-hosting

**Self-Boot Scope Boundary**:
The planning boundary that first **Full Self-Boot** is a compiler milestone with explicit platform prerequisites, not a whole-language-platform self-hosting milestone. The proof target is the `.mlfp` compiler source package; the package model, native backend, stable ABI/substrate, runtime/GC, conformance oracle, and trusted substrate are prerequisites that must be explicit enough for both compiler implementations to share.
_Avoid_: compiler-only rewrite, whole-platform bootstrap, implicit platform contract, substrate-free self-boot proof

**Self-Boot Compiler Semantics Boundary**:
The first **Full Self-Boot** boundary that language/compiler semantics are implemented by the `.mlfp` compiler source package, while trusted substrate provides capabilities. Parser behavior, name resolution, checking, semantic-interface reading/writing, backend artifact-emission decisions, diagnostics, package validation, and driver behavior are compiler-owned semantics; runtime, GC, FFI, filesystem, subprocess, hashing, and linker primitives may be substrate-owned only when they do not decide `.mlfp` language meaning.
_Avoid_: trusted parser, substrate-owned checker, host-owned package validator, runtime deciding language semantics, proof by compiler helper

**Self-Boot Backend Continuity**:
The Full Self-Boot rule that the `.mlfp` compiler lowers through the same backend/native path and artifact contracts as the current compiler, rather than introducing a second self-boot-only backend.
_Avoid_: bootstrap-only backend, interpreter-only compiler, second native pipeline

**Self-Boot Total Native Coverage**:
The first self-boot backend requirement that every source-checked `.mlfp` program accepted by the language and every `.mlfp` language feature have backend/native support, rather than only the compiler workload or selected conformance fixtures.
_Avoid_: compiler-workload-only lowering, selected native subset, post-check native unsupported case, conformance-only backend support, narrowing the checked language to dodge native work

**Self-Boot Native Substrate Coverage**:
The **Self-Boot Total Native Coverage** requirement that trusted-substrate APIs callable from `.mlfp`, including filesystem, subprocess, hashing, broad string, primitive, and runtime helper operations, have native-execution implementations with the same checked types and behavior used by the compiler driver.
_Avoid_: interpreter-only substrate API, native-missing filesystem primitive, native-missing subprocess primitive, native-missing hash primitive, checked-but-unlowerable trusted operation

**Stable Public `.mlfp` ABI**:
The stable public artifact and runtime contract required by **Self-Boot Total Native Coverage**, covering object/module boundaries, exported symbol naming, entrypoint and linking conventions, value/closure/ADT/string/IO representations, trusted-substrate call ABI, and versioned compatibility rules for `.mlfp` native artifacts.
_Avoid_: private-only object layout, ad hoc LLVM symbol naming, unstable runtime representation, executable-only native proof, hidden ABI contract

**General Public `.mlfp` FFI**:
The user-facing foreign-function interface included in the **Stable Public `.mlfp` ABI**, allowing `.mlfp` programs to import and export foreign functions through declared ABI, symbol, library, ownership, marshalling, error, and linking contracts.
_Avoid_: runtime-only FFI, trusted-helper-only foreign calls, ad hoc foreign symbol call, hidden linker dependency, interpreter-only FFI

**Declared Stable Foreign ABI Boundary**:
The FFI rule that `.mlfp` may import or export foreign functions only through an explicitly supported stable ABI such as C/system ABI, and that Rust functions are valid FFI targets only when exported through such a stable ABI boundary rather than raw Rust ABI.
_Avoid_: raw Rust ABI target, implicit foreign calling convention, unversioned FFI boundary, undeclared symbol ABI

**Stable FFI Value Shapes**:
The value-shape rule for **General Public `.mlfp` FFI**: values cross foreign boundaries only through explicit stable representations and marshalling contracts. Direct FFI shapes include primitives, `Char`, length-carrying strings and buffers with ownership rules, opaque handles or pointers, and ABI-declared structs/enums for ADTs whose layout is part of the **Stable Public `.mlfp` ABI**. Closures, polymorphic values, higher-rank values, and runtime-owned internal structures require explicit export wrappers or marshalling and do not cross as raw implementation values.
_Avoid_: raw closure pointer FFI, implicit polymorphic ABI, unmanaged string pointer, internal ADT layout leak, runtime-owned value crossing

**Stable FFI Ownership Contract**:
The ownership and lifetime rule for non-scalar **Stable FFI Value Shapes**: every foreign parameter and result declares whether the value is borrowed, copied, transferred, or represented by an opaque handle. Borrowed values are valid only for the call; copied values cross with explicit length or representation; owned outputs name their deallocator; opaque handles name their close/free operations. Foreign code may retain `.mlfp` runtime-owned values only when they are pinned or copied through an explicit ABI API.
_Avoid_: implicit allocator ownership, retained borrowed value, missing deallocator, unclosed opaque handle, foreign-retained runtime pointer

**Stable FFI Error Boundary**:
The error and unwinding rule for **General Public `.mlfp` FFI**: foreign calls report failure through explicit result/status values or declared nullable/error-code conventions. `.mlfp` failures exported to foreign callers are converted to declared ABI errors, and foreign panics or exceptions must be caught before control re-enters `.mlfp`. Unwinding never crosses a stable foreign ABI boundary.
_Avoid_: implicit exception crossing, Rust panic over FFI, C++ exception into `.mlfp`, undeclared nullable failure, unchecked foreign status

**Stable FFI Export and Callback Wrappers**:
The export/callback rule for **General Public `.mlfp` FFI**: named `.mlfp` exports generate stable C/system-ABI entry wrappers, and callbacks use declared callback types with **Stable FFI Value Shapes**, **Stable FFI Ownership Contract**, and **Stable FFI Error Boundary** semantics. Foreign code never receives raw `.mlfp` closures or runtime-internal function values.
_Avoid_: raw closure export, implicit callback lifetime, unwrapped exported function, callback without error contract, foreign-owned `.mlfp` closure

**Manifest-Owned FFI Link Graph**:
The link-graph rule for **General Public `.mlfp` FFI**: `.mlfp` source declarations name foreign imports and exports by ABI, symbol, and type, while the explicit local package manifest owns local and system libraries, search paths, static or dynamic mode, linker inputs, and target-triple constraints.
_Avoid_: ambient linker flag, undeclared global library lookup, hidden package database, source-only link dependency, implicit target constraint

**Explicit FFI Effects**:
The effect rule for **General Public `.mlfp` FFI**: foreign imports are effectful by default. A foreign import may be declared pure only through a narrow, auditable opt-in declaration checked against **Stable FFI Value Shapes** and the **Stable FFI Error Boundary**; the compiler must not silently treat arbitrary foreign calls as referentially transparent.
_Avoid_: implicit pure foreign call, unaudited unsafe import, optimizer-visible hidden effect, arbitrary pure FFI, conformance-breaking foreign effect

**Explicit FFI Runtime Context**:
The runtime-entry rule for exported `.mlfp` functions and callbacks: generated `.mlfp` executables may initialize their default runtime before user `main`, but the public foreign ABI never relies on hidden global lazy initialization. Foreign hosts must initialize an `.mlfp` runtime context before calling exported `.mlfp` wrappers, foreign threads must attach before entering `.mlfp`, and reentrant callback entry is allowed only where declared safe.
_Avoid_: hidden global runtime init, ambient argv/env capture, unattached foreign thread, undeclared reentrant callback, runtime context inferred from process state

**Self-Boot Managed GC Requirement**:
The first self-boot runtime requirement that native `.mlfp` execution include a managed garbage collector for heap-allocated `.mlfp` values such as closures, ADTs, strings, and runtime-owned structures. The GC is accessed through the **Explicit FFI Runtime Context**, is part of the **Target-Scoped Stable `.mlfp` ABI** and **Self-Boot Substrate Fingerprint**, and foreign code interacts with GC-managed values only through the **Stable FFI Ownership Contract** by pinning, copying, or opaque handles.
_Avoid_: malloc-only self-boot runtime, raw GC pointer FFI, untracked heap object lifetime, GC outside substrate fingerprint, unmanaged closure allocation

**Precise Self-Boot GC Root Model**:
The root model required by the **Self-Boot Managed GC Requirement**: stack values, globals, closures, ADTs, strings, and FFI pins or handles are compiler-known GC roots or traceable values. The backend emits enough metadata for collection and scanning, and the **Explicit FFI Runtime Context** exposes pin/unpin or handle APIs for foreign retention. Conservative stack scanning is not the correctness model for first self-boot.
_Avoid_: conservative-scanning correctness, unregistered stack root, untraceable global, hidden FFI pin, missing layout metadata

**Non-Moving First Self-Boot GC**:
The first **Self-Boot Managed GC Requirement** algorithm boundary: use a non-moving precise tracing collector for first self-boot. This provides real managed collection with the **Precise Self-Boot GC Root Model** while keeping native pointers, FFI pins, and handles stable during the first ABI. Moving collection remains a future ABI/substrate-versioned extension if layout or pointer-stability rules change.
_Avoid_: mandatory moving-GC bootstrap, relocating raw FFI pointer, pointer-instability surprise, unversioned GC algorithm change, GC algorithm outside ABI contract

**Stable Public Heap Layout Boundary**:
The heap-layout rule for the **Target-Scoped Stable `.mlfp` ABI**: exported cross-module and FFI-visible layouts have stable ABI descriptions, including ADT constructor tags, field order and representation, string and buffer representation, closure entry-wrapper shape, and trace-metadata identity. Internal temporary objects and non-exported representation choices may remain compiler/runtime-private, but they are still versioned by the ABI/substrate fingerprint and must be reproducible enough for normalized self-boot comparison where they affect semantic artifacts.
_Avoid_: hidden exported ADT layout, unversioned string layout, FFI-visible private object, unstable trace metadata identity, public closure layout leak without wrapper

**Self-Boot ABI Version**:
The explicit ABI version or fingerprint attached to `.mlfp` native artifacts and trusted-substrate native boundaries; artifacts with the same ABI version must link and behave compatibly, while representation or linking changes require an ABI version change and invalidate artifact reuse.
_Avoid_: forever-frozen ABI, unversioned ABI change, implicit compatibility, cross-version artifact reuse

**Target-Scoped Stable `.mlfp` ABI**:
The ABI stability scope for native artifacts and FFI: compatibility is defined by **Self-Boot ABI Version**, target triple, and **Self-Boot Substrate Fingerprint**. Semantic interfaces and normalized self-boot evidence stay portable where they are representation-independent, but native artifacts, data layout, word size, endianness, calling convention, linker inputs, and FFI availability are target-specific and recorded in manifest or artifact metadata.
_Avoid_: cross-platform byte-identical ABI, implicit target triple, cross-target object reuse, hidden data-layout assumption, unrecorded FFI availability

**Self-Boot Library Artifact Coverage**:
The **Self-Boot Total Native Coverage** rule for library-only packages: every checked module must be lowerable to backend/native library or object artifacts, while executable linking requires an explicit manifest entrypoint.
_Avoid_: main-required native coverage, executable-only lowering, unchecked library module, implicit entrypoint synthesis

**Self-Boot Phase Continuity**:
The Full Self-Boot rule that the `.mlfp` compiler mirrors the current compiler phase boundaries from parse through resolve, check/elaborate, backend IR, and native emission.
_Avoid_: collapsed bootstrap pipeline, monolithic self-boot compiler, phase-skipping shortcut

**Self-Boot Separate Compilation**:
The Full Self-Boot requirement that compiler source packages compile through persisted module/interface artifacts rather than only whole-package in-memory compilation.
_Avoid_: whole-package-only self-boot, cacheless compiler smoke, interface-free bootstrap

**Self-Boot Acyclic Module Graph**:
The first self-boot module policy that compiler package imports form an acyclic graph compiled in topological order, without recursive-module knot tying or boot-interface cycles.
_Avoid_: recursive module bootstrap, cyclic imports, boot interface knot, mutually recursive module compilation

**Self-Boot Intra-Module Recursion**:
The allowance that ordinary recursive declarations inside one `.mlfp` compiler module may use the existing language recursion support even though imports between compiler modules remain acyclic.
_Avoid_: declaration-level acyclicity mandate, module-cycle allowance, recursive import inference, whole-package knot tying

**Self-Boot Semantic Interface**:
The persisted module artifact for **Self-Boot Separate Compilation**, containing checked exported semantic facts, source and dependency fingerprints, export symbol identities, backend-relevant representation facts, and stable diagnostic references rather than parser AST dumps.
_Avoid_: parser AST cache, source syntax dump, untyped module snapshot

**Self-Boot Recompilation Key**:
The correctness key for reusing a persisted **Self-Boot Semantic Interface**, derived from the module source hash, dependency interface hashes, manifest-relevant compiler settings, and **Self-Boot Substrate Fingerprint** rather than file timestamps.
_Avoid_: timestamp-based correctness, stale interface reuse, ambient build-cache validity, unchecked incremental compilation

**Self-Boot Native Artifact Regeneration**:
The first-proof rule that backend IR, object files, native executables, and link outputs are regenerated during **Full Self-Boot** rather than reused from an object/native artifact cache.
_Avoid_: object-cache proof, stale native artifact reuse, cached executable equivalence, implicit object ABI cache

**First Self-Boot Native Byte Determinism Boundary**:
The first-proof boundary that native object and executable bytes do not need to be deterministic, even under a fixed **Self-Boot Host Toolchain Contract**. Native artifacts must be regenerated, recorded through **Normalized Self-Boot Command Record** and **Canonical Self-Boot Link Record** entries, runnable where the proof requires execution, and produced under the declared toolchain contract; byte-level native determinism is future hardening, not first self-boot evidence.
_Avoid_: object-byte determinism as first proof, executable-byte oracle, byte-identical native stage comparison, unrecorded native artifact, cached executable proof

**Self-Boot Stage-Owned Outputs**:
The first-proof cache isolation rule that each bootstrap stage writes to a fresh stage-owned output directory and does not reuse generated outputs from another stage. Shared declared inputs include checked locks, source packages, substrate fingerprints, platform contract packages, and checked generated bindings; stage-owned outputs include semantic interfaces, normalized semantic artifacts, backend IR, object files, executables, link records, and conformance outputs.
_Avoid_: cross-stage artifact cache, reused Haskell-produced interface, shared backend IR directory, shared conformance-output cache, stale stage artifact

**Self-Boot Root-Bounded Path Normalization**:
The proof path policy that source and input paths normalize only through manifest-declared package, fixture, platform-contract, and toolchain roots, while generated paths normalize only through **Self-Boot Stage-Owned Outputs**. Symlink and case-sensitivity behavior is declared by the manifest or host toolchain contract. A source or input path escaping declared roots is input drift; a generated output path escaping the stage-owned output root is proof-runner failure.
_Avoid_: ambient filesystem root, path escape, symlink surprise, case-folding surprise, generated output outside stage root, temp path as proof identity

**Self-Boot Proof Manifest**:
The canonical structured artifact emitted by a self-boot proof run in the **Canonical Self-Boot Proof Manifest Format**, recording stage inputs, substrate fingerprint, package locks, **Normalized Self-Boot Command Record** entries, **Canonical Self-Boot Link Record** entries, **Canonical Self-Boot Native Execution Record** entries, stage-owned output directories, conformance result hashes, normalized semantic artifact hashes, **Self-Boot Proof Failure Taxonomy** entries, **Self-Boot Proof Action ID** references, and final comparison status.
_Avoid_: terminal-log proof, ad hoc run summary, unverifiable stage evidence, missing input fingerprint, unstructured success claim

**Canonical Self-Boot Proof Manifest Format**:
The small `.mlfp`-owned canonical data format for **Self-Boot Proof Manifest** files, in the same format family as **Canonical Self-Boot Manifest Format**, **Canonical Self-Boot Package Lock Format**, and **Canonical Substrate ABI Declaration Format** rather than JSON, TOML, YAML, or a host-owned parser. Both compiler implementations must be able to parse, emit, hash, and compare proof manifests with the same canonical serialization rules.
_Avoid_: JSON proof manifest, TOML proof manifest, YAML proof manifest, host-owned proof parser, unhashable proof log

**Normalized Self-Boot Command Record**:
The exact normalized execution record stored in the **Self-Boot Proof Manifest** for every stage command, including **Self-Boot Proof Action ID**, argv, cwd, normalized environment, stdin source or hash, stdout/stderr hashes or artifact paths, exit status, relevant tool identity, owning stage, stage-owned output directory, and **Self-Boot Driver Command Mode** when the command invokes a compiler driver.
_Avoid_: shell-string-only command, implicit cwd, ambient environment, untracked stdin, terminal-log stdout proof, unowned command output, opaque compiler driver action

**Self-Boot Proof Action ID**:
The stable action identifier recorded on each command, link, native execution, conformance, and comparison record in the **Self-Boot Proof Manifest**, independent of list order, using names such as `stage0.check.compiler`, `stage0.emit-native.compiler`, `stage1.run-conformance`, or `stage1.emit-backend.compiler`. Failure records refer to action IDs so manifest diffs and diagnostics remain stable.
_Avoid_: list-index action identity, order-dependent failure reference, opaque shell step, unstable generated action ID

**Self-Boot Driver Command Mode**:
The declared compiler-driver mode recorded for each proof-manifest command that invokes a stage compiler driver. First self-boot modes are `check`, `emit-backend`, `emit-native`, and `run-conformance`; later modes must be explicitly declared before they appear in proof manifests.
_Avoid_: opaque compiler invocation, inferred mode from argv string, hidden driver behavior, undeclared proof mode

**Canonical Self-Boot Link Record**:
The canonical proof-manifest entry for every native link step, recording **Self-Boot Proof Action ID**, linker argv, target triple, linker mode, object inputs, **Resolved Self-Boot Linked Library Identity** entries for system/runtime/static/dynamic libraries, library search paths, rpath or install-name data where relevant, output artifact paths and hashes, owning stage, stage-owned output directory, and exit status. It audits native production without making native object or executable byte equality the self-boot oracle.
_Avoid_: linker log only, `-l` name only, implicit system library, ambient search path, unrecorded rpath, missing linked output hash, native byte equality oracle

**Resolved Self-Boot Linked Library Identity**:
The resolved proof identity for each linked runtime, system, static, dynamic, or framework library in a **Canonical Self-Boot Link Record**, including the actual resolved file or framework identity where practical, path, content hash or platform package identity, and whether it was linked statically or dynamically.
_Avoid_: library name only, unresolved `-l` flag, hidden linker search result, ambient framework identity, untracked dynamic library, unknown static/dynamic mode

**Canonical Self-Boot Native Execution Record**:
The canonical proof-manifest entry for each native stage compiler or conformance binary execution, recording **Self-Boot Proof Action ID**, executable identity, argv, cwd, normalized environment, loader environment, resolved dynamic libraries or frameworks where practical, rpath or install-name resolution used at execution time, stdin source or hash, stdout/stderr hashes or artifact paths, exit status, owning stage, and stage-owned output directory. Link records explain what was produced; native execution records explain what was actually loaded when the native artifact ran.
_Avoid_: native run by terminal log, unrecorded dynamic loader environment, hidden dynamic library resolution, link record as execution proof, untracked rpath resolution

**Self-Boot Loader Environment Policy**:
The proof rule that loader-affecting environment variables such as `LD_LIBRARY_PATH`, `DYLD_LIBRARY_PATH`, `DYLD_FALLBACK_LIBRARY_PATH`, and platform equivalents are either explicitly declared by the manifest/toolchain contract or scrubbed before native stage compiler and conformance binary execution. **Canonical Self-Boot Native Execution Record** entries record the normalized loader environment; ambient inherited loader state must not influence proof behavior.
_Avoid_: ambient loader environment, inherited library path, undeclared DYLD variable, hidden runtime library override, loader search path outside manifest

**Self-Boot Loader Environment Violation**:
The proof failure when loader-affecting environment state is inherited, undeclared, or not scrubbed according to the **Self-Boot Loader Environment Policy**. It is separate from substrate mismatch and stage command failure; the fix is to declare or scrub the loader-affecting state and record the normalized loader environment.
_Avoid_: classifying undeclared loader state as substrate mismatch, treating loader environment as command failure, silent ambient loader variable, unclassified proof environment violation

**Self-Boot Loader Resolution Failure**:
The proof failure when a dynamically linked native stage compiler or conformance binary execution has unresolved or unrecordable dynamic loader resolution. The target, toolchain contract, runner, or link mode must be fixed so the **Canonical Self-Boot Native Execution Record** captures loader environment plus resolved dynamic library, framework, rpath, and install-name facts.
_Avoid_: target-specific loader limitation, silent loader omission, pretending unavailable loader state is resolved, unrecorded loader evidence, ad hoc platform exception

**Self-Boot Ambient Input Policy**:
The proof rule that proof-affecting ambient inputs such as wall-clock time, random seeds, timezone, locale, and platform equivalents are either explicitly declared with expected normalization or scrubbed before compiler stages and conformance fixtures run. Compiler stages and conformance fixtures must not depend on ambient time, randomness, timezone, or locale.
_Avoid_: wall-clock proof input, ambient random seed, inherited timezone, locale-dependent diagnostic, hidden nondeterministic fixture

**Self-Boot Ambient Input Violation**:
The proof failure when proof-affecting time, randomness, timezone, locale, or equivalent ambient state is inherited, undeclared, unnormalized, or not scrubbed according to the **Self-Boot Ambient Input Policy**.
_Avoid_: classifying ambient time as conformance drift, hidden random seed, locale-only failure, timezone-dependent golden, unclassified nondeterminism

**Self-Boot Proof Failure Taxonomy**:
The stable machine-readable failure classification stored in the **Self-Boot Proof Manifest**, covering input drift, lock drift, substrate mismatch, ambient input violation, stage command failure, conformance failure, normalized semantic artifact mismatch, loader environment violation, native loader resolution failure, proof-runner failure, and unsupported or inapplicable fixture metadata failure.
_Avoid_: generic proof failed, terminal-log-only failure reason, mixed failure class, hidden fixture skip, unstructured error bucket

**First Self-Boot Proof Runner**:
The host-side orchestration tool allowed for the first self-boot proof. It may invoke stage 0 and stage 1 compiler drivers, run the conformance-first gate, compare normalized semantic artifacts, manage stage-owned output directories, and emit the **Self-Boot Proof Manifest**. It must not perform compiler semantics itself; parsing, name resolution, checking, semantic-interface reading/writing, package validation, backend artifact-emission decisions, native emission, diagnostics, driver behavior, and conformance behavior remain owned by the real stage compiler drivers and their recorded command lines.
_Avoid_: runner-owned compiler semantics, proof by host helper, hidden package checker, bypassed stage driver, `.mlfp` driver required to own `prove-self-boot` in first proof

**Self-Boot Proof Runner Evidence**:
The proof-manifest record of **First Self-Boot Proof Runner** identity and configuration, including runner source/version/hash, comparison policy, normalization policy, command templates, and runner configuration. It is proof tooling evidence rather than part of the stage-consumed **Self-Boot Substrate Fingerprint**, unless it changes compiler/runtime inputs consumed by a stage.
_Avoid_: invisible proof-runner drift, proof runner folded into substrate by default, hidden comparison policy, unrecorded command template, stage input drift outside substrate fingerprint

**Self-Boot Backend IR Regeneration**:
The rule that backend IR for a self-boot stage is regenerated from checked semantic inputs and backend-relevant representation facts, rather than loaded from a persisted interface or treated as a serialized long-term artifact.
_Avoid_: backend IR interface dump, serialized backend cache, backend IR as semantic interface, frozen backend IR format

**Self-Boot Compiler Driver**:
The `.mlfp` command-line compiler tool required for **Full Self-Boot**, responsible for package-manifest checking, semantic interface writing, backend emission, native emission/linking, conformance execution, diagnostics, and process status.
_Avoid_: library-only compiler, in-memory harness, test-only driver

**Self-Boot Local Package Model**:
The Full Self-Boot package boundary where compiler sources are discovered from explicit local roots and ordered search paths, without remote dependency solving.
_Avoid_: remote package manager, implicit global package set, network dependency solver

**Self-Boot Package Manifest**:
The explicit local manifest for a `.mlfp` compiler source package, declaring its package identity, entrypoint, source roots, exposed modules, dependency roots, **Shared Local `.mlfp` Package Set** entries, **Canonical Self-Boot Package Lock Format** inputs for the **Self-Boot Locked Local Package Closure**, artifact directories, required compiler modes, and **Self-Boot Substrate Fingerprint**.
_Avoid_: directory-inferred compiler package, hidden entrypoint convention, ambient dependency roots, manifest-free stage proof

**Canonical Self-Boot Manifest Format**:
The small `.mlfp`-owned canonical data format for **Self-Boot Package Manifest** files, parsed and serialized by compiler-owned `.mlfp` code rather than delegated to an external JSON, TOML, YAML, or host-owned parser.
_Avoid_: JSON manifest, TOML manifest, host-owned manifest parser, external manifest syntax dependency, trusted parser shortcut

**Self-Boot Local Dependency Closure**:
The complete non-substrate package dependency set for a self-boot compiler package, declared as explicit local paths in the **Self-Boot Package Manifest** and closed without global package lookup, named package databases, remote fetching, or dependency solving.
_Avoid_: global package database, named package lookup, remote dependency, implicit installed package

**Self-Boot Locked Local Package Closure**:
The proof-time lock for the **Self-Boot Local Dependency Closure**, derived from package manifests and consumed unchanged by both compiler implementations. It records package identity, local root path, source hash, dependency interface hashes, required ABI version, and **Self-Boot Substrate Fingerprint** so package inputs cannot drift silently between Haskell and `.mlfp` compiler proof runs.
_Avoid_: ambient package discovery, per-implementation package lock, package drift during proof, unlocked shared package, source-root-only dependency identity

**Checked Self-Boot Package Locks**:
The lock-file policy for **Self-Boot Locked Local Package Closure** artifacts: lock files are checked in for proof fixtures and reproducibility, but proof-path builds recompute or regenerate the closure and validate that the checked lock exactly matches manifests, source hashes, dependency interface hashes, required ABI version, and **Self-Boot Substrate Fingerprint**. Drift is a hard failure, and lock refresh is an explicit reviewed command.
_Avoid_: silent lock refresh, unchecked package lock, generated-only lock, proof run with stale package closure, warning-only lock drift

**Canonical Self-Boot Package Lock Format**:
The small `.mlfp`-owned canonical data format for **Checked Self-Boot Package Locks**, in the same format family as **Canonical Self-Boot Manifest Format**, **Canonical Self-Boot Proof Manifest Format**, and **Canonical Substrate ABI Declaration Format** rather than JSON, TOML, YAML, or a host-owned parser. Lock entries are deterministically ordered and hash-stable, recording package identity, normalized local root, source hash, dependency interface hashes, required ABI version, and **Self-Boot Substrate Fingerprint**.
_Avoid_: JSON package lock, TOML package lock, host-owned lock parser, nondeterministic lock ordering, path-unstable lock hash

**Exact Locked Local Package Identity**:
The first self-boot dependency-selection rule that every non-substrate package is selected only by exact entries in the **Self-Boot Locked Local Package Closure**. Dependency version ranges, solver alternatives, compatible-version selection, global package lookup, and name-only dependency resolution are package-management features outside the first proof.
_Avoid_: dependency version range, package solver in proof path, compatible package alternative, name-only dependency, unlocked package version

**First Self-Boot Lock Responsibility**:
The `.mlfp` compiler-driver responsibility for package locks in the first proof: it must consume and validate **Checked Self-Boot Package Locks** and must be able to deterministically recompute the lock facts from manifests, local paths, source hashes, dependency interface hashes, required ABI version, and substrate fingerprint. Explicit lock regeneration may exist as a reviewed command; solver-style lock creation from version ranges is outside first self-boot.
_Avoid_: validation-only lock consumer, solver-created proof lock, implicit lock generation, unreviewed lock command, package-manager lock responsibility in first proof

**Shared Local `.mlfp` Package Set**:
The versioned local `.mlfp` package set used unchanged by both the Haskell compiler implementation and the future `.mlfp` compiler implementation. It includes high-level library packages, compiler-adjacent packages, and Prelude/library source packages where feasible, and is declared through the **Self-Boot Local Dependency Closure** rather than duplicated per implementation.
_Avoid_: Haskell-only `.mlfp` package copy, `.mlfp`-compiler-only package copy, divergent Prelude source package, implementation-private local package graph, ambient shared package

**Prelude Package/Substrate Split**:
The first self-boot boundary for Prelude: public Prelude contracts and high-level library code live as versioned **Shared Local `.mlfp` Package Set** packages where feasible, while primitive/native operations, GC/runtime hooks, and low-level FFI shims remain in the **Shared Rust Trusted Substrate**. Both sides are declared explicitly in manifests and substrate fingerprints rather than treated as ambient Prelude magic.
_Avoid_: whole Prelude as trusted substrate, ambient built-in Prelude, duplicated Prelude implementation, primitive runtime hook in shared library source, hidden Prelude package dependency

**Self-Boot Linker Boundary**:
The Full Self-Boot rule that the `.mlfp` compiler driver may invoke a system linker to produce native executables instead of implementing a linker in `.mlfp`.
_Avoid_: in-language linker requirement, linker-free native claim, unspecified external link step

**Self-Boot Trusted Substrate**:
The fixed platform infrastructure that the first **Full Self-Boot** proof may rely on while proving the compiler source package can compile itself, including primitive declarations, native runtime helpers, memory/runtime representation support, host toolchain, system linker, low-level Prelude/runtime hooks from the **Prelude Package/Substrate Split**, and **Full Subprocess API**.
_Avoid_: whole-platform bootstrap, runtime self-hosting, Prelude implementation self-boot, primitive-free compiler proof

**Shared Rust Trusted Substrate**:
The Rust-implemented trusted substrate used by both the Haskell compiler implementation and the future `.mlfp` compiler implementation for runtime, GC, FFI shims, primitive/native helpers, and low-level system integration. It is not part of the first `.mlfp` compiler source package proof; it is versioned and fingerprinted as **Self-Boot Trusted Substrate** and exposed through the same stable `.mlfp` ABI contracts rather than through divergent Haskell-only and `.mlfp`-only helper paths.
_Avoid_: Haskell-only runtime helper, `.mlfp`-only runtime helper, duplicated substrate semantics, unversioned Rust helper, runtime implementation counted as compiler self-boot

**Shared Stable Substrate ABI Path**:
The proof-path rule for the **Shared Rust Trusted Substrate**: conformance, native behavior, and self-boot evidence for both the Haskell compiler implementation and the future `.mlfp` compiler implementation must use the same stable substrate ABI contracts as the `.mlfp` compiler path. Direct Haskell calls into Rust substrate internals are allowed only as clearly marked dev/test scaffolding outside behavior parity, native conformance, and self-boot equivalence.
_Avoid_: Haskell-only substrate shortcut, proof-path private Rust call, duplicated helper semantics, behavior parity through internal API, `.mlfp` path mismatch

**Canonical Substrate ABI Declarations**:
The single source of truth for the **Shared Stable Substrate ABI Path**, owned by the `.mlfp` platform contract. Rust exported symbols, Haskell FFI bindings, `.mlfp` foreign imports, and **Shared Substrate ABI Conformance Layer** fixtures are generated from or validated against the same declaration set rather than maintained as duplicated headers/imports/primitive lists.
_Avoid_: hand-synced Rust header, Haskell-only FFI declaration, duplicated `.mlfp` primitive list, fixture-only ABI truth, unchecked symbol drift

**Canonical Substrate ABI Declaration Format**:
The small `.mlfp`-owned canonical data format for **Canonical Substrate ABI Declarations**, in the same format family as **Canonical Self-Boot Manifest Format** and persisted interface data rather than JSON, TOML, YAML, or a host-owned parser. It is parseable by both compiler implementations, stable for hashing and fingerprinting, and expressive enough for symbols, ABI names, target triples, value shapes, ownership, error conventions, effects, runtime-context requirements, GC root and handle rules, and linker records.
_Avoid_: JSON substrate ABI, TOML substrate ABI, YAML substrate ABI, host-owned ABI parser, non-canonical ABI hashing

**Platform Substrate Contract Package**:
The dedicated versioned repo-local package that owns **Canonical Substrate ABI Declarations** and their **Canonical Substrate ABI Declaration Format** files. It is the source referenced by package manifests and the **Self-Boot Substrate Fingerprint**; generated Rust exports, Haskell bindings, `.mlfp` imports, and conformance fixtures may live near their consumers but are not the ABI source of truth.
_Avoid_: test-owned ABI declaration, runtime-folder source of truth, generated binding as ABI oracle, scattered substrate contract, consumer-local ABI version

**Checked Generated Substrate Bindings**:
The generated-artifact policy for **Canonical Substrate ABI Declarations**: generated Rust exports, Haskell FFI bindings, `.mlfp` imports, and substrate conformance fixture scaffolds are checked in for reviewability and bootstrap ergonomics, but proof-path builds and tests validate that every checked-in generated artifact matches the canonical declarations. Drift is a hard failure, and regeneration is an explicit reviewed command rather than silent build mutation.
_Avoid_: silent generated binding update, unchecked generated artifact, proof build with stale binding, generated-only source of truth, unreviewed regeneration

**Substrate ABI Versioning Policy**:
The compatibility rule for **Canonical Substrate ABI Declarations**: every declaration set carries an explicit ABI version. Additive compatible changes may advance a minor or patch-style version within policy, while representation, ownership, calling convention, GC root/handle, error, symbol, or layout-breaking changes require a major ABI version bump and invalidate artifact reuse across that boundary.
_Avoid_: unversioned ABI declaration change, hidden breaking substrate change, cross-major artifact reuse, compatible label on ownership break, silent symbol contract change

**Self-Boot Host Toolchain Contract**:
The trusted native toolchain identity included in the **Self-Boot Substrate Fingerprint**, covering **Resolved Self-Boot Toolchain Identity**, target triple, native codegen settings, linker mode, and other native toolchain configuration consumed by a bootstrap stage. **Normalized Self-Boot Command Record** entries capture exact invocations; this contract declares the allowed toolchain inputs those invocations must match.
_Avoid_: toolchain only in command log, version-string-only proof identity, ambient linker from PATH, untracked sysroot, untracked system library, undeclared target triple, native toolchain drift

**Resolved Self-Boot Toolchain Identity**:
The exact proof identity for native tools and native platform inputs in the **Self-Boot Host Toolchain Contract**, recording resolved tool paths plus content hashes where practical for LLVM or backend tools, assembler, and system linker; target triple; sysroot identity; and relevant system library identities. Version strings are diagnostic fields, not sufficient proof identity by themselves.
_Avoid_: version-only tool identity, PATH-selected linker, unhashable backend tool, implicit sysroot, unnamed system library, diagnostic version as proof

**Self-Boot Substrate Fingerprint**:
The declared version or digest of the **Self-Boot Trusted Substrate** used by each bootstrap stage, covering the low-level Prelude/runtime hook contract from the **Prelude Package/Substrate Split**, primitive inventory, runtime helper ABI, **Shared Rust Trusted Substrate**, **Shared Stable Substrate ABI Path**, **Canonical Substrate ABI Declarations**, **Canonical Substrate ABI Declaration Format**, **Platform Substrate Contract Package**, **Checked Generated Substrate Bindings**, **Substrate ABI Versioning Policy**, **Self-Boot Host Toolchain Contract**, subprocess environment policy, and linker boundary that must remain fixed for the proof.
_Avoid_: implicit host environment, drifting runtime contract, stage-local primitive set, untracked toolchain change

**Full Subprocess API**:
The trusted `.mlfp` runtime capability to launch arbitrary local commands with explicit argv, cwd, environment, stdin, stdout/stderr capture or inheritance, exit status, and failure diagnostics.
_Avoid_: implicit shell execution, hidden process environment, remote package fetching, subprocess-free self-boot, linker-only process API

**Full Local Filesystem API**:
The trusted `.mlfp` runtime capability to perform deterministic local filesystem work needed by a compiler driver, including source reads, semantic-interface reads/writes, artifact-directory creation, declared source-root listing, existence/metadata checks, atomic temp-file writes, path normalization, and content hashing.
_Avoid_: read-write-only file API, remote filesystem dependency, global package database, network package fetching, ambient source discovery

**Self-Boot Canonical Serialization**:
The `.mlfp` compiler-owned byte/string rendering of manifests, semantic interfaces, normalized semantic artifacts, and canonical diagnostics used for hashing and stage comparison.
_Avoid_: host-owned interface serialization, map-order-dependent rendering, pretty-print-only artifact, implicit binary format

**Self-Boot Trusted Hash Primitive**:
The deterministic hash operation over compiler-owned canonical bytes that the first self-boot proof may take from the **Self-Boot Trusted Substrate**.
_Avoid_: compiler-owned cryptographic implementation requirement, host-owned serialization, timestamp hash, platform-dependent digest

**Self-Boot Subprocess Environment**:
The explicit environment passed to subprocesses during the self-boot proof, declared by the manifest or toolchain configuration and included in the **Self-Boot Substrate Fingerprint** or normalized command record rather than inherited from ambient host state.
_Avoid_: ambient environment inheritance, hidden PATH dependency, user-shell configuration, untracked toolchain variable

**Self-Boot Stage Equivalence**:
The Full Self-Boot evidence contract that stage-1 and stage-2 compiler outputs match through exact conformance-corpus outputs plus normalized semantic artifacts, rather than byte-for-byte native artifact identity or deterministic native object/executable bytes.
_Avoid_: byte-for-byte object identity, timestamp-sensitive output, unchecked executable smoke

**First Self-Boot Stage Sequence**:
The first proof sequence for **Self-Boot Stage Equivalence**. Stage 0 is the Haskell compiler consuming **Checked Self-Boot Package Locks**, the platform contract, the **Shared Local `.mlfp` Package Set**, and the `.mlfp` compiler source package to produce the stage-1 native compiler plus **Normalized Self-Boot Semantic Artifacts**. Stage 1 is that native `.mlfp` compiler consuming the same locked inputs and **Self-Boot Substrate Fingerprint** to produce stage-2 artifacts. The comparison is exact conformance outputs plus normalized semantic artifacts, not object or executable bytes.
_Avoid_: Haskell-only proof input, stage-local substrate drift, stage-2 different lock, native byte identity proof, unchecked stage-1 executable smoke

**Stage-Shared Self-Boot Conformance**:
The rule that the same declared conformance suite is run by the Haskell stage-0 compiler implementation and the native `.mlfp` stage-1 compiler implementation before stage-2 artifacts are accepted. The suite includes the **Shared File-Based Compiler Conformance Corpus** and the **Shared Substrate ABI Conformance Layer**, using the same normalization and committed-golden rules for both stages. Stage 0 proves the current Haskell compiler still matches the shared oracle while producing the stage-1 compiler; stage 1 proves the generated `.mlfp` compiler matches that same oracle before normalized stage artifacts are compared. Stage-inapplicable fixtures must be declared in metadata, not skipped ad hoc.
_Avoid_: stage-1-only conformance, Haskell-only oracle, hidden bootstrap-specific regression, artifact comparison before conformance, implementation-specific conformance suite, ad hoc stage skip

**Conformance-First Self-Boot Gate**:
The proof-gate ordering that **Stage-Shared Self-Boot Conformance** must pass before **Normalized Self-Boot Semantic Artifacts** are compared. A stage that fails the shared conformance suite is classified as a compiler behavior or substrate ABI oracle failure, not as semantic-artifact drift.
_Avoid_: artifact comparison after failed conformance, artifact-only proof gate, mixed failure classification, masking behavior failure with interface comparison

**Normalized Self-Boot Semantic Artifacts**:
The stage-equivalence artifact set produced by normalizing the compiler package's **Self-Boot Semantic Interface** outputs, including module/interface identities, exported value types, exported data/type declarations with constructors and kinds, exported classes, methods, instances, fundeps, resolved symbol identities, dependency interface hashes, and backend-relevant representation facts while removing stage-local paths, timestamps, temp names, and object filenames.
_Avoid_: native binary comparison, raw backend IR comparison, timestamped interface dump, machine-local semantic artifact

**Shared File-Based Compiler Conformance Corpus**:
The `.mlfp` file-based test corpus with stable expected compiler/run outputs used unchanged by both the Haskell compiler implementation and the `.mlfp` compiler implementation.
_Avoid_: implementation-specific tests, in-memory-only harness, divergent expected outputs

**Conformance Fixture Root**:
The dedicated `test/conformance/mlfp/` tree for migrated `.mlfp` source packages, per-command expected files, fixture metadata, and normalization policy.
_Avoid_: ad hoc test/programs layout, mixed legacy fixture roots, hidden harness-only fixtures

**Conformance Fixture Metadata**:
The machine-readable metadata for each conformance fixture, declaring package root, enabled command modes, expected pass/fail status, normalization profile, behavioral tags, and any explicit stage applicability constraints.
_Avoid_: filename inference, hidden harness defaults, implementation-specific fixture config, ad hoc stage skip

**Exact Compiler Output Parity**:
The requirement that both compiler implementations produce the same stable expected output for the **Shared File-Based Compiler Conformance Corpus**, including diagnostics where a fixture is expected to fail.
_Avoid_: approximate diagnostic parity, implementation-specific message drift, golden files for one compiler only

**Per-Command Expected Output Files**:
The conformance-corpus layout where each fixture records separate expected outputs for command modes such as check, run, backend emission, native emission, and native execution.
_Avoid_: monolithic expected output, command-mode coupling, implementation-specific golden layout

**Committed Conformance Goldens**:
The rule that conformance expected files are checked into the repository and updated only through an explicit regeneration/review workflow.
_Avoid_: dynamic expected generation during tests, unreviewed oracle drift, implementation-local goldens

**Conformance Output Normalization**:
The small documented normalization step applied before exact corpus comparison to replace environment-dependent data such as absolute paths, temp directories, object filenames, and toolchain paths with stable tokens.
_Avoid_: broad fuzzy matching, hidden diagnostic rewriting, machine-specific golden files

**Shared Substrate ABI Conformance Layer**:
The file-based conformance layer for the **Shared Stable Substrate ABI Path**, run by both the Haskell compiler implementation and the future `.mlfp` compiler implementation before self-boot evidence. It covers runtime init/attach, strings and buffers, GC roots, pins and handles, FFI import/export/callback/error cases, filesystem, subprocess, hash primitives, and linker invocation records using committed expected outputs and the same small normalization policy as compiler conformance.
_Avoid_: Haskell-only substrate tests, `.mlfp`-only substrate tests, untested runtime ABI, private Rust helper parity, substrate proof by smoke test

**Canonical Self-Boot Diagnostic Rendering**:
The stable rendered diagnostic format used for self-boot conformance and stage equivalence, containing the diagnostic code or category, phase, normalized file/module identity, span, and relevant payload fields so the text is a complete serialization of the diagnostic fact.
_Avoid_: prose-only diagnostic comparison, wording-only equivalence, hidden structured diagnostic side channel, approximate diagnostic parity

**Pre-Self-Boot Test Migration**:
The prerequisite migration that moves behavior-level `.mlfp` program expectations into the shared file-based conformance corpus before self-boot implementation begins.
_Avoid_: self-boot before corpus migration, Haskell-only behavior tests, duplicate expected-output systems

**Behavioral Invariant Projection**:
The practice of adding `.mlfp` conformance fixtures that expose internal compiler invariants through user-visible behavior when such a projection is possible.
_Avoid_: leaking private implementation details, invariant coverage by Haskell tests only, forced public fixture for non-observable internals

**Self-Boot Optimization Boundary**:
The Full Self-Boot rule that performance optimization is not required for the first self-boot compiler unless a pass is needed for correctness or the shared backend/native contract.
_Avoid_: optimization-quality success bar, optimizer-first self-boot, performance parity as semantic parity

**Parsed Program Syntax Artifact**:
The unresolved `.mlfp` program syntax shape produced by parsing source text, paired with source spans when location-aware diagnostics are in scope.
_Avoid_: compiler-seed AST, resolver input bundle, checker artifact

**Parser Parity Evidence**:
The verification evidence that a `.mlfp` parser accepts and rejects the same canonical fixture classes as the current parser and produces the same **Parsed Program Syntax Artifact** on accepted inputs.
_Avoid_: exact Megaparsec wording, smoke-only parser output, unchecked rendered strings

**String Codepoint Source Cursor**:
The parser-owned source input model that advances through `.mlfp` source as `String` codepoints while tracking file, line, and column positions.
_Avoid_: byte cursor, symbolic seed input, parser-private text substrate

**Parser-Owned Combinator Core**:
The `.mlfp` compiler-frontend module set that defines parser state, results, diagnostics, and reusable combinators for `.mlfp` parsing without making them part of the public Prelude contract.
_Avoid_: Prelude parser library, ad hoc recursive state machine, external parser runtime

**Broad String Library**:
The public Prelude-level source-text library for inspecting, comparing, slicing, classifying, constructing, formatting, Unicode-default processing, and searching `String` values as Unicode scalar sequences.
_Avoid_: parser-private string helpers, symbolic source input, byte-only API

**Unicode Default Text Operations**:
The text operations in the **Broad String Library** whose case conversion, normalization, comparison, formatting, and search behavior follow fixed Unicode/default rules without taking a `Locale` value.
_Avoid_: locale-sensitive API, hidden per-user locale behavior, process-locale dependency

**Plain String Search**:
The non-regex search surface in the **Broad String Library**, covering substring containment, prefix/suffix checks, indexing, splitting, and replacement over Unicode scalar strings.
_Avoid_: regex, pattern language, glob matching, locale collation search

**Explicit String Formatting**:
The formatting surface in the **Broad String Library** built from explicit value-to-string conversions and string composition rather than a formatting mini-language.
_Avoid_: printf, interpolation, format-string parser, implicit locale formatting

**Unicode Scalar `Char`**:
The native character value for `.mlfp`, representing exactly one Unicode Scalar Value.
_Avoid_: byte, UTF-16 code unit, surrogate code point, one-character string

**Char Literal Surface**:
The `.mlfp` source syntax for writing a **Unicode Scalar `Char`** literal directly.
_Avoid_: one-character string literal, integer codepoint literal, parser-only token spelling

**Unicode Scalar String Semantics**:
The `String` operation contract where length, indexing, slicing, and general character classification operate over **Unicode Scalar `Char`** values, with ASCII behavior exposed only through explicitly named helpers.
_Avoid_: byte length, C-string indexing, implicit ASCII classification, code-unit offsets

**Valid Text Boundary**:
The rule that `.mlfp` `String` values contain only valid Unicode Scalar `Char` sequences, with invalid external text rejected while decoding file, CLI, or native input.
_Avoid_: invalid character sentinel, lossy replacement, parser-owned recovery for malformed text

**Full Native String Slicing And Classification**:
The requirement that the **Broad String Library** has matching source-checker, `run-program`, backend, and native-execution support for slicing and classification operations used by `.mlfp` compiler frontend code.
_Avoid_: interpreter-only string library, backend-deferred string helpers, ASCII-only native shortcut

**Generalization Preparation**:
The elaboration-side alignment step that turns presolution outputs, the **Acyclic Base Constraint**, and annotated terms into the shared generalization input.
_Avoid_: pipeline glue, setup tuple

**Prepared Generalization Artifact**:
The single artifact produced by **Generalization Preparation** and consumed by elaboration, result-type reconstruction, and root-scheme generalization.
_Avoid_: constraint tuple, prep bag

**Result-Type View**:
The result-type reconstruction query adapter that owns bound overlays, no-fallback reification, base-target projection, and target generalization over the prepared result-type input.
_Avoid_: fallback patch set, presolution record surgery

**Resolved Semantic Program Artifact**:
The `.mlfp` Resolve-to-Check artifact that groups resolved syntax, local semantic symbols, full visible scope, and exports as one checker input, while reference lists stay diagnostic adapters.
_Avoid_: resolved syntax tuple, scope bag, reference side table as checker input

**Backend Structural Recursive Data Matching**:
The backend-owned decision that a structural recursive type represents the same exact canonical backend data identity and constructor payload shape as a nominal backend data type.
_Avoid_: source-local recovery, unqualified data fallback

**Backend Structural Recursive Data Match**:
The evidence produced by **Backend Structural Recursive Data Matching**, carrying the canonical data identity and recovered payload information needed by backend validation, conversion, and lowering.
_Avoid_: boolean compatibility result, ad hoc payload lookup, stored IR cache

**Backend Structural Recursive Data Mismatch**:
The structured backend-domain reason produced when **Backend Structural Recursive Data Matching** fails.
_Avoid_: diagnostic text, frontend source error, LLVM lowering error

## Relationships

- **Snapshot Finalization** produces **Presolution View** artifacts for elaboration and solved handles for finalized solved-graph validation.
- **Snapshot Finalization** is not a compatibility home for outdated raw-view adapters; stale legacy consumers should be removed rather than relocated when the paper-backed pipeline no longer needs them.
- **Legacy Surface Retirement** frames the broader cleanup; **Snapshot Finalization** is one internal sub-slice when read-model construction is part of the retired surface.
- **Legacy Surface Retirement** may broaden a cleanup beyond one internal seam when a legacy parser or surface-syntax compatibility path preserves an outdated non-canonical language shape.
- **Legacy Surface Retirement** treats ASCII aliases for canonical tokens as compatibility syntax; the target accepted syntax is the paper-aligned canonical spelling.
- **Legacy Surface Retirement** applies consistently to frontend eMLF and explicit xMLF parsers; parser families should not keep different compatibility alias policies.
- **Legacy Surface Retirement** is enforced by rejection tests for retired syntax, not only by deleting old acceptance tests.
- **Legacy Surface Retirement** rejection tests assert parse failure, not exact parser diagnostic text.
- **Full Canonical `.mlfp` Parser Parity** is bounded to parser output and source spans; it does not include the **Resolved Semantic Program Artifact**, source checking, elaboration, backend lowering, compiler driver behavior, or self-hosting.
- **Full Self-Boot** is stricter than frontend parity: parser, resolver, checker, optimizer/lowering as selected, package/build driver, backend/native emission, and repeat bootstrap evidence must all line up.
- The end-to-end ordering for **Full Self-Boot** is owned by `docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`; this glossary defines the terms, while that ADR sequences the roadmap families.
- **Full Self-Boot** requires **Self-Boot Backend Continuity**; a new backend or interpreter-only compiler is not the target proof.
- **Full Self-Boot** requires **Self-Boot Total Native Coverage**; backend/native support is measured against all source-checked `.mlfp` programs, not only the compiler package, selected fixtures, or the current native subset.
- Under **Self-Boot Total Native Coverage**, a post-check backend/native unsupported case is a first self-boot blocker; narrowing the `.mlfp` feature surface solely to avoid native implementation is not an acceptable self-boot shortcut.
- **Self-Boot Total Native Coverage** includes **Self-Boot Native Substrate Coverage**; trusted-substrate APIs callable from `.mlfp` must work in native execution, not only in the interpreter.
- **Self-Boot Total Native Coverage** includes a **Stable Public `.mlfp` ABI**; deterministic native artifact contracts must specify public `.mlfp` object/module, symbol, representation, entrypoint, linking, and trusted-substrate call boundaries.
- The **Stable Public `.mlfp` ABI** includes **General Public `.mlfp` FFI**; first self-boot does not limit foreign calls to compiler-trusted runtime helpers only.
- **General Public `.mlfp` FFI** uses a **Declared Stable Foreign ABI Boundary**; C/system ABI imports are direct FFI targets, while Rust functions must be exported through a stable ABI boundary such as `extern "C"` rather than raw Rust ABI.
- **General Public `.mlfp` FFI** uses **Stable FFI Value Shapes**; all exported/imported values cross through explicit stable representations or marshalling wrappers, not raw implementation values.
- **Stable FFI Value Shapes** require a **Stable FFI Ownership Contract** for every non-scalar foreign parameter and result, including borrowed/copy/transfer/handle lifetime and deallocation rules.
- **General Public `.mlfp` FFI** uses a **Stable FFI Error Boundary**; errors are explicit ABI results/statuses and unwinding does not cross foreign boundaries.
- **General Public `.mlfp` FFI** includes **Stable FFI Export and Callback Wrappers**; named exports and callbacks cross through generated or declared ABI wrappers, not raw `.mlfp` closures.
- **General Public `.mlfp` FFI** uses a **Manifest-Owned FFI Link Graph**; source declarations name foreign symbols and types, while the local package manifest owns libraries, search paths, link mode, linker inputs, and target constraints.
- **General Public `.mlfp` FFI** uses **Explicit FFI Effects**; foreign imports are effectful by default and pure imports require narrow auditable declarations.
- **General Public `.mlfp` FFI** uses an **Explicit FFI Runtime Context**; generated executables may initialize before `main`, but foreign hosts must initialize/attach explicitly before entering exported `.mlfp` code.
- First self-boot requires the **Self-Boot Managed GC Requirement**; native `.mlfp` runtime support includes managed heap collection, with GC behavior represented in the ABI/substrate contract and hidden from raw FFI value crossing.
- The **Self-Boot Managed GC Requirement** uses a **Precise Self-Boot GC Root Model**; roots and traceable layouts are compiler-known and emitted by the backend, not inferred by conservative stack scanning.
- First self-boot uses a **Non-Moving First Self-Boot GC**; moving GC is a later ABI/substrate-versioned extension if pointer-stability or layout rules change.
- The **Target-Scoped Stable `.mlfp` ABI** includes a **Stable Public Heap Layout Boundary** for exported cross-module and FFI-visible values, while private heap shapes remain versioned implementation details.
- A **Stable Public `.mlfp` ABI** is stable within a **Self-Boot ABI Version**; breaking representation, symbol, linking, or trusted-substrate native-boundary changes require an ABI version change.
- A **Stable Public `.mlfp` ABI** is a **Target-Scoped Stable `.mlfp` ABI**; native ABI compatibility is scoped by ABI version, target triple, and substrate fingerprint, while normalized semantic evidence remains portable where representation-independent.
- **Self-Boot Total Native Coverage** includes **Self-Boot Library Artifact Coverage**: library-only packages must emit native-capable module artifacts, but only packages with manifest entrypoints produce native executables.
- **Full Self-Boot** requires **Self-Boot Phase Continuity** so stage mismatches can be localized to the same conceptual artifacts as the current compiler.
- **Full Self-Boot** requires **Self-Boot Separate Compilation**; a whole-package-only compiler does not satisfy the final self-boot bar.
- First self-boot keeps a **Self-Boot Acyclic Module Graph**; recursive module imports require a separate design and are not part of the initial separate-compilation proof.
- **Self-Boot Acyclic Module Graph** does not impose declaration-level acyclicity inside each module; **Self-Boot Intra-Module Recursion** remains allowed where the existing `.mlfp` language supports it.
- **Self-Boot Separate Compilation** persists **Self-Boot Semantic Interface** artifacts, not parser AST dumps.
- A **Self-Boot Semantic Interface** minimally records module identity, interface format version, source hash, dependency interface hashes, exported value types, exported data/type declarations with constructors and kinds, exported classes/methods/instances/fundeps, resolved export symbol identities, backend-relevant representation facts, and source-span references only where needed for stable downstream diagnostics.
- A **Self-Boot Semantic Interface** is a checked downstream-compilation contract, not a general source cache or parsed syntax interchange format.
- A persisted **Self-Boot Semantic Interface** is reusable only when its **Self-Boot Recompilation Key** matches; timestamps may be an optimization but not the correctness rule.
- **Self-Boot Backend IR Regeneration** keeps backend IR out of **Self-Boot Semantic Interface** artifacts; interfaces carry backend-relevant facts, not backend IR dumps.
- The first **Full Self-Boot** proof follows **Self-Boot Native Artifact Regeneration**: semantic interfaces may be reused through their recompilation keys, but object/native artifacts are regenerated rather than accepted from cache.
- **Full Self-Boot** requires a **Self-Boot Compiler Driver**; compiler libraries alone do not satisfy the final self-boot bar.
- The first **Self-Boot Compiler Driver** is small but real: it supports `check`, `emit-backend`, `emit-native`, and `run-conformance` over package manifests, with stable artifact, diagnostic, and exit-status behavior.
- Package publishing, remote dependency commands, REPL support, formatting, documentation generation, and optimizer controls are outside the first **Self-Boot Compiler Driver**.
- **Full Self-Boot** uses the **Self-Boot Local Package Model**; remote package management and dependency solving are outside the final self-boot bar unless a later roadmap selects them.
- Self-boot compiler packages require a **Self-Boot Package Manifest**; directory discovery alone is fixture-friendly but too implicit for separate compilation and stage equivalence.
- A **Self-Boot Package Manifest** uses the **Canonical Self-Boot Manifest Format**; first self-boot does not add JSON, TOML, YAML, or another external manifest syntax to the trusted substrate.
- A **Self-Boot Package Manifest** declares a **Self-Boot Local Dependency Closure** plus the trusted substrate; non-substrate dependencies must not come from ambient global package state.
- Proof runs use a **Self-Boot Locked Local Package Closure** derived from manifests; both compiler implementations consume the same package identities, local roots, source hashes, dependency interface hashes, required ABI version, and substrate fingerprint.
- **Checked Self-Boot Package Locks** are checked in but validated on every proof-path build/test; lock drift from manifests, hashes, interfaces, ABI version, or substrate fingerprint is a hard failure and refresh is explicit/reviewed.
- **Checked Self-Boot Package Locks** use the **Canonical Self-Boot Package Lock Format**, a `.mlfp`-owned deterministic format with hash-stable package-lock entries.
- First self-boot uses **Exact Locked Local Package Identity** only; dependency version ranges and solver behavior are outside the proof path.
- The first `.mlfp` compiler driver has **First Self-Boot Lock Responsibility**: consume, validate, and deterministically recompute checked locks, with explicit reviewed regeneration allowed and solver-style creation deferred.
- The self-boot ABI, substrate, runtime, GC, shared-local-package, and package-lock platform contract is owned by `docs/adr/2026-05-18-self-boot-platform-contract.md`; the file-based conformance ADR only owns compiler fixture migration.
- The **Self-Boot Local Dependency Closure** includes the **Shared Local `.mlfp` Package Set**; both the Haskell compiler implementation and future `.mlfp` compiler implementation consume the same local `.mlfp` package sources rather than implementation-specific package copies.
- Prelude follows the **Prelude Package/Substrate Split**: high-level Prelude/library source is shared local `.mlfp` package code where feasible, while primitive/native operations, GC/runtime hooks, and low-level FFI shims remain declared trusted substrate.
- **Full Self-Boot** permits the **Self-Boot Linker Boundary**; the compiler must drive linking deterministically, but the linker itself may be external.
- **Full Self-Boot** follows the **Self-Boot Scope Boundary**: it is measured over the `.mlfp` compiler source package, not the entire language platform, while relying on explicit platform prerequisites shared by both compiler implementations.
- **Full Self-Boot** follows the **Self-Boot Compiler Semantics Boundary**: parser behavior, name resolution, checking, semantic-interface reading/writing, backend artifact-emission decisions, diagnostics, package validation, and driver behavior belong to the `.mlfp` compiler source package.
- The first proof may depend on the **Self-Boot Trusted Substrate**.
- The **Self-Boot Trusted Substrate** is not evidence that primitive operations, native runtime helpers, memory management, LLVM tooling, low-level Prelude/runtime hooks, or the system linker are themselves implemented in `.mlfp`; it also must not decide `.mlfp` language semantics.
- The first self-boot may use a **Shared Rust Trusted Substrate** for both the Haskell compiler implementation and the future `.mlfp` compiler implementation; the proof compares compiler behavior over the same versioned substrate rather than requiring the substrate itself to self-host.
- The **Shared Rust Trusted Substrate** is reached through a **Shared Stable Substrate ABI Path** for proof evidence; Haskell-only direct substrate internals must not be used for conformance parity, native behavior evidence, or self-boot equivalence.
- The **Shared Stable Substrate ABI Path** is specified by **Canonical Substrate ABI Declarations**; Rust exports, Haskell bindings, `.mlfp` imports, and substrate conformance fixtures must be generated from or validated against one declaration source.
- **Canonical Substrate ABI Declarations** use the **Canonical Substrate ABI Declaration Format**, a `.mlfp`-owned canonical data format parseable by both compiler implementations and stable for substrate hashing/fingerprinting.
- **Canonical Substrate ABI Declarations** live in a **Platform Substrate Contract Package**; generated Rust, Haskell, `.mlfp`, and conformance artifacts may live near consumers but are not the source of truth.
- **Checked Generated Substrate Bindings** are checked in but validated on every proof-path build/test; drift from **Canonical Substrate ABI Declarations** is a hard failure and regeneration is explicit/reviewed.
- **Canonical Substrate ABI Declarations** follow the **Substrate ABI Versioning Policy**; breaking representation, ownership, calling, GC root/handle, error, symbol, or layout changes require a major ABI version bump and invalidate reuse.
- The **Self-Boot Substrate Fingerprint** includes the **Self-Boot Host Toolchain Contract** and **Resolved Self-Boot Toolchain Identity**; resolved LLVM/backend tools, assembler, linker, target triple, sysroot, relevant system libraries, native codegen settings, and linker mode are trusted stage-consumed inputs.
- The **Full Local Filesystem API** is part of the **Self-Boot Trusted Substrate** for the first proof; self-boot records normalized paths and content hashes, but the API itself is trusted local runtime infrastructure.
- **Self-Boot Canonical Serialization** is compiler-owned `.mlfp` logic; the **Self-Boot Trusted Hash Primitive** may digest those bytes, but it does not decide what bytes represent an interface, manifest, semantic artifact, or diagnostic.
- The **Full Subprocess API** is part of the **Self-Boot Trusted Substrate**; self-boot records and normalizes the actual toolchain commands used, but the API itself is general local subprocess execution.
- **Full Subprocess API** does not imply remote package fetching, networking, or dependency solving.
- Subprocesses used by the self-boot proof run with a **Self-Boot Subprocess Environment**; ambient process-environment inheritance is allowed only as a non-proof convenience.
- Compiler stages and conformance fixtures follow the **Self-Boot Ambient Input Policy**; proof-affecting wall-clock time, random seeds, timezone, locale, and equivalents are declared with expected normalization or scrubbed.
- Inherited, undeclared, unnormalized, or unscrubbed proof-affecting ambient state is a **Self-Boot Ambient Input Violation**.
- Each bootstrap stage must use the same **Self-Boot Substrate Fingerprint**; changing the trusted substrate between stages invalidates the **Self-Boot Stage Equivalence** proof.
- Each bootstrap stage uses **Self-Boot Stage-Owned Outputs**; declared proof inputs may be shared, but generated semantic interfaces, backend IR, object/executable/link outputs, and conformance outputs are stage-local and regenerated.
- Proof records use **Self-Boot Root-Bounded Path Normalization**; source and input paths resolve inside manifest-declared roots, generated paths resolve inside stage-owned output roots, and path escapes fail as input drift or proof-runner failure.
- First self-boot observes the **First Self-Boot Native Byte Determinism Boundary**; object and executable bytes are regenerated, recorded, runnable where required, and produced under the declared toolchain contract, but deterministic native bytes are not proof evidence.
- Each proof run emits a **Self-Boot Proof Manifest** in the **Canonical Self-Boot Proof Manifest Format** so input fingerprints, **Normalized Self-Boot Command Record** entries, **Canonical Self-Boot Link Record** entries, **Canonical Self-Boot Native Execution Record** entries, stage-owned output directories, conformance result hashes, normalized semantic artifact hashes, **Self-Boot Proof Failure Taxonomy** entries, **Self-Boot Proof Action ID** references, and final comparison status are auditable without relying on terminal logs.
- Every stage command has a **Normalized Self-Boot Command Record** with **Self-Boot Proof Action ID**, argv, cwd, normalized environment, stdin source or hash, stdout/stderr hashes or artifact paths, exit status, relevant tool identity, owning stage, stage-owned output directory, and **Self-Boot Driver Command Mode** for compiler-driver invocations.
- Every proof-manifest command, link, native execution, conformance, and comparison record carries a **Self-Boot Proof Action ID** so failures and manifest diffs do not depend on list order.
- Every native link step has a **Canonical Self-Boot Link Record** with **Self-Boot Proof Action ID**, linker argv, target triple, linker mode, object inputs, **Resolved Self-Boot Linked Library Identity** entries, search paths, rpath or install-name data where relevant, output artifact paths and hashes, owning stage, stage-owned output directory, and exit status.
- **Resolved Self-Boot Linked Library Identity** records actual linked runtime/system/static/dynamic/framework libraries where practical, including path, content hash or platform package identity, and static or dynamic link mode.
- Every native execution has a **Canonical Self-Boot Native Execution Record** with **Self-Boot Proof Action ID**, loader environment, resolved dynamic library/framework identities where practical, and rpath or install-name resolution used at execution time.
- Native execution follows the **Self-Boot Loader Environment Policy**; loader-affecting variables are manifest/toolchain-declared or scrubbed, and the normalized loader environment is recorded.
- Undeclared or unscrubbed loader-affecting environment state is a **Self-Boot Loader Environment Violation**, not substrate mismatch or stage command failure.
- Unresolved or unrecordable dynamic loader resolution is a **Self-Boot Loader Resolution Failure**; the target, toolchain contract, runner, or link mode must be fixed before the proof can pass.
- **Normalized Self-Boot Command Record** entries record exact tool invocations, while the **Self-Boot Host Toolchain Contract** declares the native toolchain identity those invocations are allowed to use.
- Version strings in command records are diagnostic; **Resolved Self-Boot Toolchain Identity** uses resolved paths and content hashes where practical rather than version strings alone.
- The **Self-Boot Proof Failure Taxonomy** distinguishes input drift, lock drift, substrate mismatch, ambient input violation, stage command failure, conformance failure, normalized semantic artifact mismatch, loader environment violation, native loader resolution failure, proof-runner failure, and unsupported or inapplicable fixture metadata failure.
- The first proof may use a **First Self-Boot Proof Runner** as host-side orchestration; it invokes real stage compiler drivers and records their commands, but does not implement compiler semantics or replace the driver contract.
- The proof manifest records **Self-Boot Proof Runner Evidence** separately from the **Self-Boot Substrate Fingerprint**; runner identity/configuration enters the substrate fingerprint only when it changes compiler/runtime inputs consumed by a stage.
- **Full Self-Boot** is proven by **Self-Boot Stage Equivalence**, not by byte-for-byte object or executable identity.
- The first proof follows the **First Self-Boot Stage Sequence**: Haskell stage 0 produces the native stage-1 `.mlfp` compiler from locked inputs, and that compiler produces stage-2 artifacts from the same locked inputs and substrate fingerprint.
- The **First Self-Boot Stage Sequence** includes **Stage-Shared Self-Boot Conformance**; stage 0 and stage 1 both run the same declared conformance suite before normalized stage artifacts are accepted.
- The stage-shared suite includes both the **Shared File-Based Compiler Conformance Corpus** and **Shared Substrate ABI Conformance Layer**; any fixture that is not applicable to a stage must declare that in **Conformance Fixture Metadata**.
- The proof gate is **Conformance-First Self-Boot Gate**: failed stage-shared conformance stops the proof before normalized semantic artifact comparison.
- **Self-Boot Stage Equivalence** compares **Normalized Self-Boot Semantic Artifacts** for successful compiler builds; first self-boot does not compare native binaries, object files, executable bytes, or raw backend IR.
- **Full Self-Boot** requires a **Shared File-Based Compiler Conformance Corpus** and **Exact Compiler Output Parity** so the same `.mlfp` fixtures can expose compiler bugs in both the Haskell and `.mlfp` implementations.
- The **Shared File-Based Compiler Conformance Corpus** lives under the **Conformance Fixture Root**; existing ad hoc fixtures migrate there one by one.
- Each conformance fixture carries **Conformance Fixture Metadata** rather than relying on filename conventions or harness-only defaults.
- The **Shared File-Based Compiler Conformance Corpus** uses **Per-Command Expected Output Files** so each compiler mode has a stable oracle without coupling unrelated outputs.
- **Per-Command Expected Output Files** are **Committed Conformance Goldens**, not outputs generated dynamically during the test run.
- **Exact Compiler Output Parity** compares outputs after **Conformance Output Normalization**; the normalization list must stay small and explicit.
- **Full Self-Boot** requires a **Shared Substrate ABI Conformance Layer** before self-boot evidence; both compiler implementations must exercise the same stable substrate ABI contracts rather than implementation-private runtime helpers.
- The **Shared Substrate ABI Conformance Layer** is generated from or validated against **Canonical Substrate ABI Declarations**, not maintained as a separate ABI oracle.
- Self-boot diagnostics compare through **Canonical Self-Boot Diagnostic Rendering**; rendered text is acceptable as the proof artifact only when it serializes the stable diagnostic code/category, phase, normalized location, span, and payload.
- Failing conformance fixtures store **Canonical Self-Boot Diagnostic Rendering** as their committed expected diagnostic output; separate prose rendering tests may exist, but they are not the self-boot equivalence oracle.
- Human-friendly diagnostic prose may be derived from **Canonical Self-Boot Diagnostic Rendering**, but wording-only prose is not the stage-equivalence artifact.
- **Pre-Self-Boot Test Migration** must happen before self-boot implementation work starts: behavior-level `.mlfp` tests migrate one by one into the shared corpus.
- **Pre-Self-Boot Test Migration** is its own roadmap family before the broad string/parser/self-boot family, not merely the first implementation milestone inside it.
- **Pre-Self-Boot Test Migration** is behavior-preserving by default; production compiler changes are allowed only when a migrated fixture exposes a real existing bug.
- **Pre-Self-Boot Test Migration** starts with existing public `.mlfp` fixtures and CLI/package behavior, then adds **Behavioral Invariant Projection** fixtures after the corpus harness is stable.
- During **Pre-Self-Boot Test Migration**, internal invariants should gain **Behavioral Invariant Projection** fixtures when the invariant has an honest user-visible behavior; private invariant tests remain internal when no such projection exists.
- **Full Self-Boot** follows the **Self-Boot Optimization Boundary**; unoptimized but semantically equivalent output may satisfy the first self-boot proof.
- **Full Canonical `.mlfp` Parser Parity** follows **Legacy Surface Retirement**: retired aliases and non-canonical compatibility spellings are excluded unless an explicit thesis-faithfulness reason keeps them.
- **Full Canonical `.mlfp` Parser Parity** produces the **Parsed Program Syntax Artifact** rather than a second compiler-seed AST.
- The **Parsed Program Syntax Artifact** precedes the **Resolved Semantic Program Artifact**; parse parity must not smuggle name resolution, visibility, type checking, or import semantics into the parser target.
- **Parser Parity Evidence** uses a canonical fixture corpus, accepted artifact comparison or stable artifact rendering, pretty/parse round trips, and negative accept/reject plus diagnostic-category/span checks rather than exact parser error text.
- **Full Canonical `.mlfp` Parser Parity** depends on a Prelude-level **Broad String Library** and reads source through a parser-owned **String Codepoint Source Cursor** built on that library.
- **Unicode Scalar `Char`** is part of the source language through the **Char Literal Surface**, not only a value returned by string operations.
- Under the **Char Literal Surface**, double quotes produce `String` literals and single quotes produce **Unicode Scalar `Char`** literals.
- The **Broad String Library** follows **Unicode Scalar String Semantics**; ASCII grammar predicates must be explicitly named as ASCII helpers.
- Under **Unicode Scalar String Semantics**, `String` is conceptually a sequence of **Unicode Scalar `Char`** values; length, indexing, slicing, and classification operate on that sequence.
- The **Broad String Library** includes total conversion between `String` and `List Char`; this exposes the sequence model without requiring native strings to be represented as linked lists.
- The **Broad String Library** includes formatting, **Unicode Default Text Operations**, and search APIs as first-class scope, not as deferred unrelated text utilities.
- Locale-sensitive APIs are not part of the initial **Broad String Library** contract.
- Search APIs in the initial **Broad String Library** mean **Plain String Search**; regex is excluded from this roadmap.
- Formatting APIs in the initial **Broad String Library** mean **Explicit String Formatting**; printf-style and interpolation mini-languages are excluded from this roadmap.
- The **Valid Text Boundary** makes malformed external text fail closed before it becomes a `.mlfp` `String`; string and parser code do not carry invalid-character cases.
- The **Broad String Library** must satisfy **Full Native String Slicing And Classification** before full parser parity is complete; parser evidence cannot claim native readiness from interpreter-only string operations.
- **Full Native String Slicing And Classification** preserves **Unicode Scalar String Semantics** in native execution rather than treating strings as byte-indexed C strings.
- The next **Full Canonical `.mlfp` Parser Parity** family is ordered in two tracks: deliver the native-capable **Broad String Library** first, then build the **Parser-Owned Combinator Core** and full canonical parser on top of it.
- **Full Canonical `.mlfp` Parser Parity** is implemented with a **Parser-Owned Combinator Core**; shared parser helpers should graduate to Prelude only after the parser proves a stable public abstraction.
- **Generalization Preparation** consumes one **Acyclic Base Constraint** and one **Presolution View**.
- **Generalization Preparation** produces one **Prepared Generalization Artifact** and owns the result-type-ready adapter assembled from redirects, canonical edge artifacts, base maps, and the owner-local phase bridge.
- A **Prepared Generalization Artifact** is shared by elaboration, result-type reconstruction, and root-scheme generalization.
- **Result-Type View** is built from the prepared result-type adapter and owns overlay-aware query behavior for result-type reconstruction.
- **Resolved Semantic Program Artifact** is produced by `.mlfp` Resolve and consumed by Check; syntax and references remain available through adapters for diagnostics and audits.
- **Backend Structural Recursive Data Matching** happens after conversion has produced canonical backend data identities and uses exact identity; source-local recovery remains a conversion adapter concern.
- **Backend Structural Recursive Data Matching** produces a **Backend Structural Recursive Data Match** rather than a bare boolean.
- **Backend Structural Recursive Data Matching** has metadata-light and metadata-backed modes that share one core matcher.
- Metadata-light **Backend Structural Recursive Data Matching** proves only canonical data identity and structural recursive skeleton compatibility.
- Metadata-backed **Backend Structural Recursive Data Matching** uses nominal `BackendData` declarations to prove constructor-set equality, recovered data-parameter substitution, and aligned field evidence.
- Metadata-light **Backend Structural Recursive Data Matching** is not a fallback for missing nominal declarations when validation, conversion, or lowering needs constructor sets, substitutions, payload fields, handler validation, or lowering field types.
- Adapter operations that need metadata-backed evidence must acquire nominal declarations or fail closed.
- **Backend Structural Recursive Data Matching** supports whole-data and focused-constructor modes through the same core identity, substitution, and payload-comparison rules.
- A **Backend Structural Recursive Data Match** carries backend data-parameter and constructor-payload evidence, but not runtime tags, field offsets, closure-record layout, or LLVM/native lowering facts.
- **Backend Structural Recursive Data Matching** owns substitution-aware constructor-payload comparison; adapters provide declarations and structural shapes, and the matcher returns the recovered data-parameter substitution plus aligned field evidence.
- **Backend Structural Recursive Data Matching** recursively compares structural recursive payload fields after substitution, using a visited-pair guard keyed by canonical data identity and structural recursive binder/name so genuinely recursive fields terminate.
- Constructor identity inside **Backend Structural Recursive Data Matching** is owner-qualified by canonical data identity plus constructor name; bare constructor names are not backend identities.
- Whole-data matching proves the structural recursive data shape matches the nominal data declaration as a constructor set; focused-constructor matching proves only the constructor or handler shape needed by the current conversion or lowering operation.
- Whole-data **Backend Structural Recursive Data Matching** requires exact constructor-set equality; missing or extra structural constructors are mismatches.
- Backend IR validation requires metadata-backed whole-data **Backend Structural Recursive Data Matching** whenever a structural recursive type claims a canonical backend data identity and the nominal declaration is available.
- Focused-constructor matching does not relax whole-data validity; it is an operation-local proof for conversion or lowering after the canonical backend shape has been established.
- A **Backend Structural Recursive Data Match** is derived evidence returned by the matcher for the current adapter operation; it is not stored on `BackendProgram`, `BackendData`, or constructor metadata.
- A failed **Backend Structural Recursive Data Matching** attempt produces a **Backend Structural Recursive Data Mismatch** that adapters translate into validation, conversion, or lowering errors.
- **Backend Structural Recursive Data Mismatch** reasons stay at the backend structural-data level: data-identity mismatch, non-structural body, data-parameter mismatch, missing constructor, handler-shape mismatch, and constructor-payload mismatch.
- Conversion must normalize source-local structural recursive names to canonical backend data identities before backend validation; unresolved or ambiguous names fail in conversion rather than widening **Backend Structural Recursive Data Matching**.
- Conversion normalization rewrites backend type shapes to canonical backend names; it does not preserve source-local structural names behind a side map or later recovery hint.
- Conversion normalization also resolves representation-level nominal data aliases such as `BTCon` versus `BTBase` before **Backend Structural Recursive Data Matching**; the shared matcher compares one canonical backend representation rather than treating representation aliases as peer identity rules.
- The **Backend Structural Recursive Data Matching** implementation handoff requires private matcher tests for exact identity, recursive payload cycle guards, substitution mismatch, extra or missing constructors, and metadata-light evidence limits.
- The implementation handoff also requires adapter regression tests that validate, convert, and lower the same recursive ADT shape through public backend paths so the validation, conversion, and LLVM lowering adapters cannot drift independently.

## Example Dialogue

> **Dev:** "Should `Pipeline` build copy maps and scope overrides before elaboration?"
> **Domain expert:** "No. `Pipeline` should ask **Generalization Preparation** for a **Prepared Generalization Artifact** and pass that artifact to the consumers."

## Flagged Ambiguities

- "generalization input" can mean either individual maps or the **Prepared Generalization Artifact**; prefer the artifact name when talking about the shared Interface.
- "result-type context" should mean **Result-Type View** query behavior when overlays, base-target projection, reification, or target generalization are involved.
- "recursive ADT matching" can mean either source-local recovery during conversion or **Backend Structural Recursive Data Matching** over canonical backend data identities; keep the shared backend matcher on the canonical backend meaning.
- "constructor name" inside **Backend Structural Recursive Data Matching** means owner-qualified constructor identity, not a bare textual name.
- "metadata-light match" is not a full ADT proof; constructor-payload evidence requires metadata-backed matching.
- "`BTCon` versus `BTBase` equivalence" is a conversion-normalization concern, not a second identity rule inside **Backend Structural Recursive Data Matching**.
- "mismatch reason" means **Backend Structural Recursive Data Mismatch** until an adapter renders it into a validation, conversion, or lowering diagnostic.
- "matcher coverage" means both private matcher unit coverage and public adapter regression coverage; one does not replace the other.
- "snapshot materialization" conflicts with presolution expansion materialization; use **Snapshot Finalization** for canonical read-model and solved-handle construction.
- `ChiQuery` is an adapter name, not a domain boundary; prefer direct **Presolution View** accessors or **Snapshot Finalization** entrypoints.
- "legacy bridge" does not imply a supported compatibility contract; prefer deleting obsolete legacy code over preserving adapters for older internal shapes.
- "legacy syntax" is not protected by default; canonical parser and pretty-printer behavior should follow the paper-aligned syntax unless an explicit thesis-faithfulness reason keeps an alias.
- "alias" is not a separate exception from "legacy syntax"; parser aliases are retired with other compatibility surfaces unless explicitly justified.
- "fully parse `.mlfp`" means **Full Canonical `.mlfp` Parser Parity** unless explicitly widened; it does not mean the old symbolic frontend seed, checker implementation in `.mlfp`, or every historical parser spelling.
- "fully self boot" means **Full Self-Boot**, not a parser-only, checker-only, or first-stage compiler smoke test.
- "self-boot backend" means **Self-Boot Backend Continuity**, not a parallel backend or interpreter shortcut.
- "self-boot native coverage" means **Self-Boot Total Native Coverage**, not lowering only the compiler workload, conformance fixtures, or a narrowed subset of `.mlfp` features.
- "native substrate coverage" means **Self-Boot Native Substrate Coverage**, not interpreter-only filesystem, subprocess, hashing, string, primitive, or runtime helpers.
- "stable ABI" means **Stable Public `.mlfp` ABI**, not merely deterministic executable output for one compiler package.
- "public FFI" means **General Public `.mlfp` FFI**, not only calls from generated `.mlfp` code into trusted runtime helpers.
- "Rust FFI" means Rust code exported through a **Declared Stable Foreign ABI Boundary**, not raw Rust ABI.
- "ABI stability" means compatibility within one **Self-Boot ABI Version**, not a forever-frozen ABI across all future compiler versions.
- "library-only native coverage" means **Self-Boot Library Artifact Coverage**, not requiring every checked package to link an executable.
- "self-boot phases" means **Self-Boot Phase Continuity**, not a collapsed bootstrap-only pipeline.
- "self-boot build" means **Self-Boot Separate Compilation**, including persisted interfaces, not only whole-package compilation.
- "self-boot module graph" means **Self-Boot Acyclic Module Graph**, not recursive module imports or boot-interface knot tying.
- "self-boot recursion" means **Self-Boot Intra-Module Recursion** when it is inside one module; it does not permit recursive module imports.
- "self-boot interface" means **Self-Boot Semantic Interface**, not a parser AST cache.
- "interface contents" means checked exports, fingerprints, symbol identities, representation facts, and stable diagnostic references; parsed source trees are not interface contents.
- "interface reuse" means matching the **Self-Boot Recompilation Key**, not trusting filesystem timestamps.
- "backend IR persistence" is out of scope for first self-boot; **Self-Boot Backend IR Regeneration** rebuilds backend IR instead of storing it in interfaces.
- "object artifact reuse" is outside the first proof; **Self-Boot Native Artifact Regeneration** requires regenerating object/native/link outputs.
- "self-boot driver" means **Self-Boot Compiler Driver**, not an in-memory test harness.
- "small but real driver" means `check`, `emit-backend`, `emit-native`, and `run-conformance`; it does not include package publishing, remote dependency operations, REPL, formatter, doc generation, or optimizer controls.
- "self-boot package model" means **Self-Boot Local Package Model**, not remote package solving.
- "self-boot manifest" means **Self-Boot Package Manifest**, not directory-only source discovery.
- "manifest format" means **Canonical Self-Boot Manifest Format**, not JSON, TOML, YAML, or a host-owned parser format.
- "self-boot dependency" means a member of the **Self-Boot Local Dependency Closure** or the **Self-Boot Trusted Substrate**, not a named/global package dependency.
- "shared local `.mlfp` package" means a member of the **Shared Local `.mlfp` Package Set**, not a Haskell-only or future-compiler-only package copy.
- "Prelude in self-boot" means the **Prelude Package/Substrate Split**, not treating the whole Prelude as trusted substrate or as ambient magic.
- "self-boot linker" means **Self-Boot Linker Boundary**, not an in-language linker requirement.
- "self-boot trusted substrate" means the fixed primitive, runtime, toolchain, linker, and low-level Prelude/runtime-hook infrastructure allowed by **Self-Boot Trusted Substrate**, not part of the first `.mlfp` compiler source package proof.
- "filesystem support" means **Full Local Filesystem API**, not only `readFile` and `writeFile`.
- "canonical serialization" means **Self-Boot Canonical Serialization** owned by `.mlfp` compiler code, not a host-owned interface dump.
- "hashing" may use the **Self-Boot Trusted Hash Primitive**, but only over bytes produced by **Self-Boot Canonical Serialization**.
- "subprocess support" means **Full Subprocess API**, not only a linker-specific command hook and not implicit shell execution.
- "subprocess environment" means **Self-Boot Subprocess Environment**, not ambient shell or inherited process state.
- "substrate fingerprint" means **Self-Boot Substrate Fingerprint**, not a loose environment note or optional build log entry.
- "first full self-boot" is not whole-platform self-hosting; it proves the compiler package can compile itself while relying on the **Self-Boot Trusted Substrate**.
- "stage equivalence" means **Self-Boot Stage Equivalence**: exact conformance-corpus outputs plus normalized semantic artifacts, not byte-for-byte native artifact identity.
- "normalized semantic artifacts" means **Normalized Self-Boot Semantic Artifacts**, not native binaries, raw backend IR, or timestamped build outputs.
- "compiler test corpus" means **Shared File-Based Compiler Conformance Corpus**, not separate Haskell-only and `.mlfp`-only test suites.
- "conformance fixture root" means **Conformance Fixture Root**, not the legacy ad hoc `test/programs/` layout.
- "fixture metadata" means **Conformance Fixture Metadata**, not filename inference.
- "diagnostics parity" means **Exact Compiler Output Parity** for conformance fixtures, not approximate diagnostic-category matching.
- "self-boot diagnostic output" means **Canonical Self-Boot Diagnostic Rendering**, not prose-only rendered messages.
- "failing fixture expected diagnostics" means committed **Canonical Self-Boot Diagnostic Rendering**, not a separate structured sidecar plus prose golden.
- "expected output" means **Per-Command Expected Output Files** for the shared corpus, not one combined fixture output.
- "golden update" means updating **Committed Conformance Goldens** through an explicit regeneration/review command, not dynamic test acceptance.
- "exact output" means exact after **Conformance Output Normalization**, not raw machine-local paths or temp names.
- "test migration before self-boot" means **Pre-Self-Boot Test Migration**, not ad hoc test cleanup after implementation begins.
- "test migration roadmap" means a separate **Pre-Self-Boot Test Migration** family before broad string/parser/self-boot implementation.
- "migration behavior change" is allowed only as a bug fix exposed by **Pre-Self-Boot Test Migration**, not as opportunistic language redesign.
- "internal invariant fixture" means **Behavioral Invariant Projection** when the invariant is visible through `.mlfp` behavior; otherwise keep the internal test.
- "self-boot optimizer" means **Self-Boot Optimization Boundary**; performance optimization is not required unless correctness or backend continuity requires it.
- "parser AST" means **Parsed Program Syntax Artifact** for this roadmap; a separate seed AST would need its own justification.
- "parser parity" means **Parser Parity Evidence** over artifacts and categories, not exact parser diagnostic prose.
- "string parsing" requires the **Broad String Library** plus a parser-owned **String Codepoint Source Cursor**, not byte offsets or symbolic source constructors.
- "native string support" means **Full Native String Slicing And Classification** for this roadmap, not only C-string printing or literal lowering.
- "parser roadmap" in this context means the ordered broad-string/native track plus parser-combinator parity track, not parser code alone.
- "character" means a **Unicode Scalar `Char`** unless a helper is explicitly named ASCII or byte-oriented.
- "one-character string" is not a substitute for **Unicode Scalar `Char`**.
- "char literal" means a single-quoted **Char Literal Surface** form like `'c'`, not a double-quoted string literal of length one or an integer codepoint escape.
- "String as List Char" is an API view for conversion and reasoning, not a mandated native representation.
- "broad string library" includes formatting, **Unicode Default Text Operations**, and search APIs; do not narrow it to parser-only slicing/classification.
- "locale" is out of scope for the initial **Broad String Library**; add explicit locale values later only if a separate roadmap selects them.
- "search" means **Plain String Search** in the initial **Broad String Library**; regex requires a separate roadmap.
- "formatting" means **Explicit String Formatting** in the initial **Broad String Library**; printf or interpolation requires a separate roadmap.
- "parser combinator" means the **Parser-Owned Combinator Core** for this roadmap, not a new Prelude-level library.
