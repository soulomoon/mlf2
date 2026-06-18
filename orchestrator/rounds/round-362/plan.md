### Selected Extraction
- Milestone: Self-Boot Platform Contract Implementation
- Milestone id: milestone-5-self-boot-platform-contract-implementation
- Direction id: direction-5a-platform-contract-substrate
- Extracted item id: item-362-canonical-native-link-record-validation
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Add the next bounded M5 platform-contract slice after rounds 358, 359, 360,
and 361: pure, test-backed canonical native link record validation and
deterministic evidence rendering over explicit link-step facts.

Rounds 358-361 made substrate declarations/fingerprint material,
ambient/loader policy validation, host toolchain identity validation, and
checked local package lock validation explicit. This round must make native
link records explicit enough for later proof-runner and native-execution
record slices to consume, without running native linkers, resolving libraries
from the host filesystem, discovering toolchains, emitting proof manifests, or
claiming native execution records.

This is not M5 closeout. It must not claim native execution records, proof
manifest emission, generated binding drift closure, package-manager/linker
completion, platform/proof closeout, self-boot completion, or full self-boot.

### Approach
The active rev-007 roadmap is specific enough to select a bounded M5 slice
under `direction-5a-platform-contract-substrate`. Select canonical native link
record validation because native link records remain in the M5 completion
signal, they depend on the substrate/toolchain/package-lock surfaces now
present, and they must exist before native execution records can honestly
record what a linked artifact loaded at run time.

Keep the implementation pure over caller-provided link-step records:

- Add `src/MLF/Platform/NativeLinkRecord.hs` as the owner for canonical
  self-boot native link records, deterministic record rendering, validation,
  and violation rendering. It should reuse platform contract types such as
  `TargetTriple` and `ToolchainLinkerMode` where that keeps ownership clean,
  and introduce platform-local newtypes for link-record facts.
- Model each link record with a proof action id, linker argv as an argument
  vector, target triple, linker mode, object inputs, resolved linked library
  identities, library search paths, rpath/install-name data, output artifact
  path and hash, owning stage, stage-owned output directory, and exit status.
- Add `test/PlatformNativeLinkRecordSpec.hs`, wire it into `mlf2.cabal` and
  `test/Main.hs`, and keep assertions explicit for accepted evidence and each
  rejected drift/shape class.
- Add `test/golden/platform-contract/native-link-record.txt` as the
  deterministic canonical rendering for one valid link record.
- Update `docs/architecture.md` with the new owner entry and preserve the
  later-slice boundaries from the existing platform owners.

The native-link-record model must include at least these names:

- `SelfBootProofActionId`
- `SelfBootStageId`
- `StageOwnedOutputDirectory`
- `NativeLinkObjectInput`
- `ResolvedSelfBootLinkedLibraryIdentity`
- `ResolvedLinkedLibraryKind`
- `ResolvedLinkedLibraryLinkMode`
- `NativeLinkSearchPath`
- `NativeLinkRPath`
- `NativeLinkInstallName`
- `NativeLinkOutputArtifact`
- `NativeLinkExitStatus`
- `CanonicalSelfBootLinkRecord`
- `NativeLinkRecordEvidence`
- `NativeLinkRecordViolation`
- `validateCanonicalSelfBootLinkRecord`
- `renderCanonicalSelfBootLinkRecord`
- `renderNativeLinkRecordEvidence`
- `renderNativeLinkRecordViolation`
- `renderNativeLinkRecordViolations`

Validation must reject all of these with meaningful diagnostics:

- missing or blank proof action id;
- missing or blank owning stage;
- missing or blank stage-owned output directory;
- blank linker argv executable or empty linker argv vector;
- missing or blank target triple;
- missing or blank linker mode;
- blank object input path or object input hash;
- duplicate object input paths;
- object input path outside the declared stage-owned output directory;
- missing object inputs;
- resolved linked library identity with only an unresolved `-l`-style name and
  no resolved file, framework, or platform package identity;
- blank linked library name, kind, link mode, path/framework/package identity,
  or content hash when that field is selected for the identity;
- duplicate linked library identities by stable name/kind/link-mode key;
- blank library search path, rpath, or install-name entry;
- duplicate search paths, rpath entries, or install-name entries;
- missing or blank output artifact path or hash;
- output artifact path outside the declared stage-owned output directory; and
- unsupported or malformed exit status representation.

The validator must sort rendered object inputs, resolved library identities,
search paths, rpaths, and install-name entries by stable keys so output does
not depend on caller input order. It must not call `System.Directory`,
`System.Environment`, `System.Process`, `Data.Time`, `System.Random`,
`findExecutable`, native linkers, native runners, loader inspection tools, or
any host probing API. Real linker invocation, host library resolution,
toolchain discovery, runtime-object compilation, proof-manifest emission, and
native execution records remain later slices.

### Execution Profile
- Complexity: standard
- Verification profile: standard
- Reason: The selected slice introduces a new platform proof-record owner, a
  new canonical data/evidence/violation surface for native link steps,
  deterministic golden rendering, Cabal/test/docs wiring, and root-bounded
  stage-output validation. The task content needs design judgment and is
  therefore not a planner-direct simple task. It is bounded to one M5 contract
  surface and is not milestone closeout.

### Steps
1. Confirm the assigned worktree is
   `/Volumes/src/mlf4/orchestrator/worktrees/round-362` on branch
   `orchestrator/round-362-platform-contract-next`. Treat the parent checkout
   `orchestrator/state.json` as authoritative and do not edit any
   `orchestrator/state.json` file.
2. Re-read rounds 358, 359, 360, and 361 artifacts; current
   `src/MLF/Platform/*`; current platform specs; `docs/backend-native-pipeline.md`
   for existing test-only native link behavior; `CONTEXT.md` terms for
   Canonical Self-Boot Link Record, Resolved Self-Boot Linked Library
   Identity, Self-Boot Proof Action ID, Self-Boot Stage-Owned Outputs,
   Self-Boot Root-Bounded Path Normalization, Canonical Self-Boot Native
   Execution Record, and First Self-Boot Native Byte Determinism Boundary; and
   the accepted platform-contract ADR. Keep this round limited to pure native
   link-record validation over explicit facts.
3. Add `src/MLF/Platform/NativeLinkRecord.hs` with the link record, evidence,
   violation, validation, canonical rendering, and deterministic sorting
   functions listed above. Prefer one module for this first owner; split only
   if the file becomes materially harder to review.
4. Add `test/PlatformNativeLinkRecordSpec.hs` with positive evidence, ordering
   stability, action/stage/linker/target/linker-mode diagnostics, object input
   diagnostics, root-bounded path diagnostics, linked-library identity
   diagnostics, search/rpath/install-name diagnostics, output artifact
   diagnostics, exit-status diagnostics, and pure repeatability over explicit
   records. Wire it into both `mlf2.cabal` and `test/Main.hs`.
5. Add `test/golden/platform-contract/native-link-record.txt` for the
   canonical rendering produced by `renderCanonicalSelfBootLinkRecord` or
   `renderNativeLinkRecordEvidence` for the accepted link record. The focused
   spec must prove reordered object/library/path inputs render identically.
6. Update `docs/architecture.md` to name
   `MLF.Platform.NativeLinkRecord` as the pure canonical native link record
   owner. State that real linker invocation, host library resolution,
   generated binding drift closure, native execution records, proof-manifest
   emission, proof-runner integration, and proof closeout remain later slices.
7. Run the focused commands, static guard, full standard gate, and thesis gate
   below. If the full test suite mutates generated `runtime/mlfp_io/target/`
   artifacts, restore or remove only those generated artifacts after
   verification and preserve unrelated user changes.
8. Write `orchestrator/rounds/round-362/implementation-notes.md` with changed
   files, native link record surfaces implemented, command results, static
   guard output, the golden path and what it proves, root-bounded path evidence,
   and explicit non-claims.

### Verification
Focused commands:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform contract substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform native link record"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "renders canonical native link records deterministically"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects incomplete native link records with named diagnostics"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "keeps native link record validation pure over explicit records"'`

Static platform native-link-record guard:

```sh
ruby - <<'RUBY'
required_files = [
  "src/MLF/Platform/Contract.hs",
  "src/MLF/Platform/EnvironmentPolicy.hs",
  "src/MLF/Platform/ToolchainIdentity.hs",
  "src/MLF/Platform/PackageLock.hs",
  "src/MLF/Platform/NativeLinkRecord.hs",
  "test/PlatformContractSpec.hs",
  "test/PlatformEnvironmentPolicySpec.hs",
  "test/PlatformToolchainIdentitySpec.hs",
  "test/PlatformPackageLockSpec.hs",
  "test/PlatformNativeLinkRecordSpec.hs",
  "test/golden/platform-contract/minimal-substrate-contract.txt",
  "test/golden/platform-contract/normalized-environment-policy.txt",
  "test/golden/platform-contract/host-toolchain-identity.txt",
  "test/golden/platform-contract/checked-package-lock.txt",
  "test/golden/platform-contract/native-link-record.txt",
  "test/Main.hs",
  "mlf2.cabal",
  "docs/architecture.md"
]

missing_files = required_files.reject { |path| File.exist?(path) }
abort("missing required round-362 files: #{missing_files.inspect}") unless missing_files.empty?

changed = `git diff --name-only HEAD`.lines.map(&:chomp)
banned_paths = changed.grep(%r{\A(orchestrator/state\.json|orchestrator/roadmaps/|src-public/|src/MLF/Backend/|src/MLF/Program/|runtime/)})
abort("out-of-scope paths changed: #{banned_paths.inspect}") unless banned_paths.empty?

cabal = File.read("mlf2.cabal")
main = File.read("test/Main.hs")
contract = File.read("src/MLF/Platform/Contract.hs")
environment_policy = File.read("src/MLF/Platform/EnvironmentPolicy.hs")
toolchain = File.read("src/MLF/Platform/ToolchainIdentity.hs")
package_lock = File.read("src/MLF/Platform/PackageLock.hs")
link_record = File.read("src/MLF/Platform/NativeLinkRecord.hs")
link_record_spec = File.read("test/PlatformNativeLinkRecordSpec.hs")
arch = File.read("docs/architecture.md")

required_link_record_terms = [
  "SelfBootProofActionId",
  "SelfBootStageId",
  "StageOwnedOutputDirectory",
  "NativeLinkObjectInput",
  "ResolvedSelfBootLinkedLibraryIdentity",
  "ResolvedLinkedLibraryKind",
  "ResolvedLinkedLibraryLinkMode",
  "NativeLinkSearchPath",
  "NativeLinkRPath",
  "NativeLinkInstallName",
  "NativeLinkOutputArtifact",
  "NativeLinkExitStatus",
  "CanonicalSelfBootLinkRecord",
  "NativeLinkRecordEvidence",
  "NativeLinkRecordViolation",
  "validateCanonicalSelfBootLinkRecord",
  "renderCanonicalSelfBootLinkRecord",
  "renderNativeLinkRecordEvidence",
  "renderNativeLinkRecordViolation",
  "renderNativeLinkRecordViolations"
]

missing_link_record_terms = required_link_record_terms.reject { |term| link_record.include?(term) }
abort("missing native link record terms: #{missing_link_record_terms.inspect}") unless missing_link_record_terms.empty?

abort("mlf2.cabal missing MLF.Platform.NativeLinkRecord") unless cabal.include?("MLF.Platform.NativeLinkRecord")
abort("mlf2.cabal missing PlatformNativeLinkRecordSpec") unless cabal.include?("PlatformNativeLinkRecordSpec")
abort("test/Main.hs missing PlatformNativeLinkRecordSpec") unless main.include?("PlatformNativeLinkRecordSpec")
abort("docs/architecture.md missing native link record owner entry") unless arch.include?("MLF.Platform.NativeLinkRecord")

required_spec_phrases = [
  "renders canonical native link records deterministically",
  "rejects incomplete native link records with named diagnostics",
  "keeps native link record validation pure over explicit records",
  "object input path outside the declared stage-owned output directory",
  "output artifact path outside the declared stage-owned output directory",
  "resolved linked library identity"
]

missing_spec_phrases = required_spec_phrases.reject { |term| link_record_spec.include?(term) }
abort("missing native link record spec coverage phrases: #{missing_spec_phrases.inspect}") unless missing_spec_phrases.empty?

probe_scan = [contract, environment_policy, toolchain, package_lock, link_record, link_record_spec].join("\n")
probe_hits = probe_scan.lines.grep(/System\.Directory|System\.Environment|System\.Process|Data\.Time|System\.Random|findExecutable|readProcess|runLLVMNativeExecutable|validateLLVMAssembly|validateLLVMObjectCode/)
abort("native link record slice performs host probing or native execution: #{probe_hits.inspect}") unless probe_hits.empty?

overclaim_scan = [arch, link_record, link_record_spec].join("\n")
overclaim_patterns = [
  /M5 closeout/i,
  /milestone-5.*done/i,
  /self-boot complete/i,
  /full self-boot complete/i,
  /native execution records complete/i,
  /proof manifest complete/i,
  /proof closeout complete/i,
  /package-manager complete/i,
  /generated binding drift closure complete/i
]

overclaims = overclaim_patterns.select { |pattern| overclaim_scan.match?(pattern) }
abort("native link record slice overclaims completion: #{overclaims.map(&:source).inspect}") unless overclaims.empty?

puts "round-362 platform native-link-record static guard passed"
RUBY
```

Standard gates:

- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`
- `./scripts/thesis-conformance-gate.sh`

The selected slice touches platform substrate/proof evidence, so the standard
profile requires focused owner tests, the full Cabal gate, and the thesis
conformance gate. Full closeout gates are not selected because this round does
not claim native execution records, proof-manifest completion, M5 closeout, or
self-boot completion.

### Scheduler
- Depends on round ids: round-358, round-359, round-360, round-361
- Merge after item ids: item-358-platform-substrate-contract-fingerprint-material, item-359-ambient-loader-policy-validation, item-360-host-toolchain-identity-validation, item-361-checked-local-package-lock-validation
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: single implementer owns `src/MLF/Platform/NativeLinkRecord.hs`, `test/PlatformNativeLinkRecordSpec.hs`, `test/golden/platform-contract/native-link-record.txt`, `mlf2.cabal`, `test/Main.hs`, `docs/architecture.md`, and `orchestrator/rounds/round-362/implementation-notes.md` in this worktree.
