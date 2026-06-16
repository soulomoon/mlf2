### Selected Extraction
- Milestone: Self-Boot Platform Contract Implementation
- Milestone id: milestone-5-self-boot-platform-contract-implementation
- Direction id: direction-5a-platform-contract-substrate
- Extracted item id: item-360-host-toolchain-identity-validation
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Add the next bounded M5 platform-contract slice after rounds 358 and 359:
pure, test-backed host toolchain identity validation over explicit declared
and observed toolchain inputs.

Round 358 made host toolchain identities part of the typed substrate contract
and fingerprint material. Round 359 made ambient-input and loader-environment
policies enforceable over explicit snapshots. This round must make toolchain
identity enforceable without probing the host, discovering tools from `PATH`,
running native linkers, emitting command records, or claiming native link or
native execution records.

This is not M5 closeout. It must not claim lock validation, generated binding
drift closure, host toolchain discovery, native link records, native execution
records, package-manager/linker completion, platform/proof closeout, or
self-boot completion.

### Approach
The active rev-007 roadmap is specific enough to select a bounded M5 slice
under `direction-5a-platform-contract-substrate`. Select host toolchain
identity validation because it directly follows the host toolchain fields
introduced in round 358, does not duplicate the ambient/loader policy validator
from round 359, and advances the remaining M5 completion signal for toolchain
identity before later lock, link-record, and native-execution-record work.

Keep the implementation pure over caller-provided declarations and
observations:

- Extend `MLF.Platform.Contract` so `HostToolchainContract` records the
  target-scoped host toolchain inputs required by the platform-contract ADR:
  resolved tools, sysroot identity, relevant system library identities, native
  codegen settings, and linker mode. Keep the existing `TargetTriple` on
  `PlatformSubstrateContract`; do not duplicate it inside the host toolchain
  contract.
- Add `src/MLF/Platform/ToolchainIdentity.hs` as the owner for validating a
  declared `HostToolchainContract` and `TargetTriple` against an explicit
  toolchain observation snapshot. It should import the contract types from
  `MLF.Platform.Contract` instead of duplicating toolchain declarations.
- Add `test/PlatformToolchainIdentitySpec.hs`, wire it into `mlf2.cabal` and
  `test/Main.hs`, and keep assertions explicit for accepted evidence and each
  rejected drift class.
- Add `test/golden/platform-contract/host-toolchain-identity.txt` as the
  deterministic evidence rendering for one valid host toolchain identity
  snapshot.
- Update `test/PlatformContractSpec.hs` and
  `test/golden/platform-contract/minimal-substrate-contract.txt` so the
  expanded host toolchain fields render deterministically and remain part of
  `renderSubstrateFingerprintMaterial`.
- Update `docs/architecture.md` with the new owner entry and preserve the
  non-claims from rounds 358 and 359.

The contract/toolchain model must include at least these new or extended names:

- `ToolchainSysrootIdentity`
- `ToolchainSystemLibraryIdentity`
- `ToolchainCodegenSetting`
- `ToolchainLinkerMode`
- `ObservedToolIdentity`
- `ObservedToolchainSystemLibrary`
- `ObservedToolchainIdentity`
- `ToolchainIdentitySnapshot`
- `ToolchainIdentityEvidence`
- `ToolchainIdentityViolation`
- `validatePlatformToolchainIdentity`
- `renderPlatformToolchainIdentityEvidence`
- `renderPlatformToolchainIdentityViolation`
- `renderPlatformToolchainIdentityViolations`

Validation must reject all of these with meaningful diagnostics:

- missing or blank declared linker mode;
- missing or blank declared sysroot identity when the selected contract marks
  sysroot identity as available;
- duplicate declared tool roles, system library identities, or codegen setting
  keys;
- duplicate observed tool roles, observed system library identities, or
  observed codegen setting keys;
- observed target triple that differs from the declared `TargetTriple`;
- declared required tool role missing from the observation snapshot;
- observed tool role with no declared tool contract;
- observed tool path, digest, unavailable reason, or version conflicting with
  the declared tool identity;
- a declared or observed tool identity that supplies only a version string as
  proof identity;
- declared sysroot, system library, codegen setting, or linker mode missing
  from observations or observed with a different value.

The validator may accept explicit unavailable-tool identities only when the
declared and observed unavailable reason match and the selected evidence marks
the tool unavailable. It must sort rendered evidence by stable keys so output
does not depend on input order. It must not call `System.Environment`,
`System.Directory`, `System.Process`, `Data.Time`, `System.Random`,
`findExecutable`, native linkers, native runners, or any host probing API. Any
real host toolchain capture or discovery belongs to a later toolchain-discovery
or proof-runner slice.

### Execution Profile
- Complexity: standard
- Verification profile: standard
- Reason: The selected slice extends the platform substrate contract with
  additional ADR-backed toolchain identity fields, adds a new validation owner,
  adds a new violation/evidence surface, updates golden fingerprint material,
  and wires new focused tests. The task content needs design judgment and is
  therefore not a planner-direct simple wiring change. It is bounded to one M5
  contract surface and is not milestone closeout.

### Steps
1. Confirm the assigned worktree is
   `/Volumes/src/mlf4/orchestrator/worktrees/round-360` on branch
   `orchestrator/round-360-platform-contract-next`. Treat the parent checkout
   `orchestrator/state.json` as authoritative and do not edit any
   `orchestrator/state.json` file.
2. Re-read rounds 358 and 359 artifacts, `src/MLF/Platform/Contract.hs`,
   `src/MLF/Platform/EnvironmentPolicy.hs`, platform tests, `CONTEXT.md`
   terms for Self-Boot Host Toolchain Contract, Resolved Self-Boot Toolchain
   Identity, Self-Boot Substrate Fingerprint, Normalized Self-Boot Command
   Record, Canonical Self-Boot Link Record, and the accepted platform-contract
   ADR. Keep this round limited to pure toolchain identity validation over
   explicit declarations and observations.
3. Extend `src/MLF/Platform/Contract.hs` with sysroot, system library,
   codegen-setting, and linker-mode declarations under `HostToolchainContract`.
   Update validation, `renderPlatformSubstrateContract`, and
   `renderSubstrateFingerprintMaterial` so these fields are deterministic,
   sorted by stable keys, and part of the canonical fingerprint material.
4. Add `src/MLF/Platform/ToolchainIdentity.hs` with the snapshot, evidence,
   violation, validation, and deterministic rendering functions listed above.
   Keep it pure and free of host environment, directory, process, time,
   random, linker, native-runner, and tool discovery imports.
5. Update `test/PlatformContractSpec.hs` and
   `test/golden/platform-contract/minimal-substrate-contract.txt` for the
   expanded host toolchain contract rendering. Add focused assertions that
   changing sysroot identity, system library identity, codegen setting, or
   linker mode changes substrate fingerprint material.
6. Add `test/PlatformToolchainIdentitySpec.hs` with positive evidence,
   ordering stability, unavailable-tool matching, target triple drift, required
   tool drift, sysroot drift, system library drift, codegen setting drift,
   linker-mode drift, duplicate diagnostics, and version-string-only
   diagnostics. Wire it into both `mlf2.cabal` and `test/Main.hs`.
7. Add `test/golden/platform-contract/host-toolchain-identity.txt` for the
   canonical evidence rendering produced by
   `renderPlatformToolchainIdentityEvidence`.
8. Update `docs/architecture.md` to name `MLF.Platform.ToolchainIdentity` as
   the pure validator for declared host toolchain identity over explicit
   observations. State that real host discovery, checked package locks, native
   command records, native link records, native execution records,
   proof-manifest emission, and proof closeout remain later slices.
9. Run the focused commands, static guard, full standard gate, and thesis gate
   below. If the full test suite mutates generated `runtime/mlfp_io/target/`
   artifacts, restore or remove only those generated artifacts after
   verification and preserve unrelated user changes.
10. Write `orchestrator/rounds/round-360/implementation-notes.md` with changed
    files, toolchain identity surfaces implemented, command results, static
    guard output, and explicit non-claims.

### Verification
Focused commands:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform contract substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform toolchain identity"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "renders host toolchain identity evidence deterministically"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects toolchain identity drift with named diagnostics"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "keeps toolchain identity validation pure over explicit snapshots"'`

Static platform toolchain-identity guard:

```sh
ruby - <<'RUBY'
required_files = [
  "src/MLF/Platform/Contract.hs",
  "src/MLF/Platform/EnvironmentPolicy.hs",
  "src/MLF/Platform/ToolchainIdentity.hs",
  "test/PlatformContractSpec.hs",
  "test/PlatformEnvironmentPolicySpec.hs",
  "test/PlatformToolchainIdentitySpec.hs",
  "test/golden/platform-contract/minimal-substrate-contract.txt",
  "test/golden/platform-contract/normalized-environment-policy.txt",
  "test/golden/platform-contract/host-toolchain-identity.txt",
  "test/Main.hs",
  "mlf2.cabal",
  "docs/architecture.md"
]

missing_files = required_files.reject { |path| File.exist?(path) }
abort("missing required round-360 files: #{missing_files.inspect}") unless missing_files.empty?

changed = `git diff --name-only HEAD`.lines.map(&:chomp)
banned_paths = changed.grep(%r{\A(orchestrator/state\.json|orchestrator/roadmaps/|src-public/|src/MLF/Backend/|src/MLF/Program/|runtime/)})
abort("out-of-scope paths changed: #{banned_paths.inspect}") unless banned_paths.empty?

cabal = File.read("mlf2.cabal")
main = File.read("test/Main.hs")
contract = File.read("src/MLF/Platform/Contract.hs")
environment_policy = File.read("src/MLF/Platform/EnvironmentPolicy.hs")
toolchain = File.read("src/MLF/Platform/ToolchainIdentity.hs")
toolchain_spec = File.read("test/PlatformToolchainIdentitySpec.hs")
contract_spec = File.read("test/PlatformContractSpec.hs")
arch = File.read("docs/architecture.md")

required_contract_terms = [
  "HostToolchainContract",
  "ToolchainSysrootIdentity",
  "ToolchainSystemLibraryIdentity",
  "ToolchainCodegenSetting",
  "ToolchainLinkerMode",
  "renderSubstrateFingerprintMaterial"
]

required_toolchain_terms = [
  "ObservedToolIdentity",
  "ObservedToolchainSystemLibrary",
  "ObservedToolchainIdentity",
  "ToolchainIdentitySnapshot",
  "ToolchainIdentityEvidence",
  "ToolchainIdentityViolation",
  "validatePlatformToolchainIdentity",
  "renderPlatformToolchainIdentityEvidence",
  "renderPlatformToolchainIdentityViolation",
  "renderPlatformToolchainIdentityViolations"
]

missing_contract_terms = required_contract_terms.reject { |term| contract.include?(term) }
abort("missing host toolchain contract terms: #{missing_contract_terms.inspect}") unless missing_contract_terms.empty?

missing_toolchain_terms = required_toolchain_terms.reject { |term| toolchain.include?(term) }
abort("missing toolchain identity terms: #{missing_toolchain_terms.inspect}") unless missing_toolchain_terms.empty?

abort("mlf2.cabal missing MLF.Platform.ToolchainIdentity") unless cabal.include?("MLF.Platform.ToolchainIdentity")
abort("mlf2.cabal missing PlatformToolchainIdentitySpec") unless cabal.include?("PlatformToolchainIdentitySpec")
abort("test/Main.hs missing PlatformToolchainIdentitySpec") unless main.include?("PlatformToolchainIdentitySpec")
abort("docs/architecture.md missing toolchain identity owner entry") unless arch.include?("MLF.Platform.ToolchainIdentity")

required_spec_phrases = [
  "renders host toolchain identity evidence deterministically",
  "rejects toolchain identity drift with named diagnostics",
  "keeps toolchain identity validation pure over explicit snapshots",
  "target triple",
  "sysroot",
  "system library",
  "codegen setting",
  "linker mode",
  "version string alone"
]

missing_spec_phrases = required_spec_phrases.reject { |term| toolchain_spec.include?(term) || contract_spec.include?(term) }
abort("missing toolchain spec coverage phrases: #{missing_spec_phrases.inspect}") unless missing_spec_phrases.empty?

banned_import_or_probe_terms = [
  "System.Environment",
  "System.Directory",
  "System.Process",
  "System.Random",
  "Data.Time",
  "getCurrentTime",
  "lookupEnv",
  "getEnv",
  "findExecutable",
  "doesFileExist",
  "readProcess",
  "callProcess",
  "createProcess",
  "discoverNativeLLVMToolchain"
]

probe_scan = contract + "\n" + environment_policy + "\n" + toolchain + "\n" + toolchain_spec
probe_hits = banned_import_or_probe_terms.select { |term| probe_scan.include?(term) }
abort("toolchain identity slice performs host probing: #{probe_hits.inspect}") unless probe_hits.empty?

format_hits = ["Aeson", "Toml", "TOML", "Yaml", "YAML", "JSON"].select do |term|
  probe_scan.include?(term)
end
abort("toolchain identity slice introduced non-canonical external format terms: #{format_hits.inspect}") unless format_hits.empty?

overclaim_patterns = [
  /M5 closeout/i,
  /milestone-5.*done/i,
  /self-boot complete/i,
  /full self-boot/i,
  /compiler-package complete/i,
  /native backend complete/i,
  /package-manager complete/i,
  /linker complete/i,
  /lock validation complete/i,
  /generated binding drift closure complete/i,
  /host toolchain discovery complete/i,
  /native command records complete/i,
  /native link records complete/i,
  /native execution records complete/i,
  /proof manifest complete/i
]

scan_paths = changed.select { |path| path.match?(%r{\A(src|test|docs|README|CHANGELOG)}) && File.file?(path) }
overclaims = []
scan_paths.each do |path|
  body = File.read(path)
  overclaim_patterns.each do |pattern|
    overclaims << "#{path}:#{pattern.source}" if body.match?(pattern)
  end
end
abort("overclaim phrases found: #{overclaims.inspect}") unless overclaims.empty?

puts "round-360 platform toolchain-identity static guard passed"
RUBY
```

Standard and thesis gates:

- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`
- `./scripts/thesis-conformance-gate.sh`

Evidence required in `implementation-notes.md`:

- focused command results;
- static guard output;
- full Cabal gate result;
- thesis conformance gate result;
- the host toolchain identity golden path and what it proves;
- the host toolchain fingerprint-material drift surfaces covered by tests;
- explicit non-claims for lock validation, generated binding drift closure,
  host toolchain discovery, native command records, native link records,
  native execution records, package-manager/linker completion,
  platform/proof closeout, and self-boot completion.

### Scheduler
- Depends on round ids: round-358, round-359
- Merge after item ids: item-358-platform-substrate-contract-fingerprint-material, item-359-ambient-loader-policy-validation
- Parallel group: serial-m5-platform-contract-substrate

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: The main implementer owns the source/test/docs changes and
  `orchestrator/rounds/round-360/implementation-notes.md` in this worktree.
  Reviewer should check the static guard, focused tests, full standard gate,
  thesis gate, scope boundaries, generated-artifact cleanup, and non-claims
  before approving. No planner-stage implementation is allowed because this
  task is `Complexity: standard`.
