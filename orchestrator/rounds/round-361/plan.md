### Selected Extraction
- Milestone: Self-Boot Platform Contract Implementation
- Milestone id: milestone-5-self-boot-platform-contract-implementation
- Direction id: direction-5a-platform-contract-substrate
- Extracted item id: item-361-checked-local-package-lock-validation
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Add the next bounded M5 platform-contract slice after rounds 358, 359, and
360: pure, test-backed checked local package lock validation over explicit
package, interface, ABI, and substrate-fingerprint facts.

Round 358 added platform substrate declarations and fingerprint material.
Round 359 added pure ambient/loader policy validation. Round 360 added pure
host toolchain identity validation. This round must make checked local package
locks enforceable without discovering package roots, recomputing hashes from
the filesystem, creating a package solver, parsing a final lock-file syntax,
running native linkers, emitting command records, or claiming native link or
native execution records.

This is not M5 closeout. It must not claim generated binding drift closure,
package-manager completion, lockfile parser completion, native command
records, native link records, native execution records, platform/proof
closeout, or self-boot completion.

### Approach
The active rev-007 roadmap is specific enough to select a bounded M5 slice
under `direction-5a-platform-contract-substrate`. Select checked local package
lock validation because it is one of the remaining M5 completion-signal
surfaces, it follows the existing substrate and toolchain identity owners, and
it must be in place before later native link-record and native-execution-record
evidence can safely claim stable package inputs.

Keep the implementation pure over caller-provided current facts:

- Add `src/MLF/Platform/PackageLock.hs` as the owner for self-boot package lock
  declarations, lock validation, deterministic lock rendering, evidence
  rendering, and violation diagnostics. It may reuse package identity and
  module identity types from `MLF.Frontend.Program.Package` and source/
  interface metadata types from `MLF.Frontend.Program.BuildGraph`; do not
  duplicate package graph or interface validation policy there.
- Model checked lock entries as exact local package identities with normalized
  local roots, source metadata, dependency interface metadata, interface
  metadata, required ABI version, and required substrate fingerprint material.
  The lock should validate against an explicit current package-lock snapshot
  supplied by the caller.
- Add `test/PlatformPackageLockSpec.hs`, wire it into `mlf2.cabal` and
  `test/Main.hs`, and keep assertions explicit for accepted evidence and each
  rejected drift class.
- Add `test/golden/platform-contract/checked-package-lock.txt` as the
  deterministic rendering for one valid checked local package lock.
- Update `docs/architecture.md` with the new owner entry and preserve the
  later-slice non-claims from the existing platform owners.

The package-lock model must include at least these names:

- `SelfBootPackageLock`
- `LockedPackageIdentity`
- `LockedPackageRoot`
- `LockedPackageEntry`
- `LockedModuleEntry`
- `LockedDependencyInterface`
- `CurrentPackageLockSnapshot`
- `PackageLockEvidence`
- `PackageLockViolation`
- `validateSelfBootPackageLock`
- `renderSelfBootPackageLock`
- `renderPackageLockEvidence`
- `renderPackageLockViolation`
- `renderPackageLockViolations`

Validation must reject all of these with meaningful diagnostics:

- missing or blank package identity;
- missing or blank normalized local root;
- duplicate locked package identities;
- duplicate locked module identities inside a package lock entry;
- duplicate dependency interface entries inside a locked module;
- current package identity missing from the checked lock;
- current package identity not declared by the checked lock;
- normalized local root drift;
- required ABI version drift from the current platform contract;
- required substrate fingerprint material drift;
- source metadata drift for a locked module;
- direct dependency id drift for a locked module;
- dependency interface metadata drift for a locked module dependency;
- interface metadata drift for a locked module;
- current module identity missing from the lock;
- locked module identity missing from the current package snapshot.

The validator must sort rendered package, module, and dependency evidence by
stable package/module keys so output does not depend on caller input order. It
must not call `System.Directory`, `System.Environment`, `System.Process`,
`Data.Time`, `System.Random`, source parsers, package discovery, hashers,
native linkers, native runners, or any host probing API. Real package root
discovery, source hashing, lock regeneration, final `.mlfp`-owned package-lock
file parsing, package solving, and proof-runner integration remain later
slices.

### Execution Profile
- Complexity: standard
- Verification profile: standard
- Reason: The selected slice introduces a new platform package-lock validation
  owner, a new checked-lock data/evidence/violation surface, deterministic
  rendering, focused golden evidence, and Cabal/test/docs wiring. It must
  integrate with existing package/build-graph metadata without taking over
  package discovery or cache policy. The task content needs design judgment and
  is therefore not a planner-direct simple wiring change. It is bounded to one
  M5 contract surface and is not milestone closeout.

### Steps
1. Confirm the assigned worktree is
   `/Volumes/src/mlf4/orchestrator/worktrees/round-361` on branch
   `orchestrator/round-361-platform-contract-next`. Treat the parent checkout
   `orchestrator/state.json` as authoritative and do not edit any
   `orchestrator/state.json` file.
2. Re-read rounds 358, 359, and 360 artifacts; `src/MLF/Platform/Contract.hs`;
   `src/MLF/Platform/EnvironmentPolicy.hs`;
   `src/MLF/Platform/ToolchainIdentity.hs`; current platform specs; package
   owner docs for `MLF.Frontend.Program.Package`, `Interface`, and
   `BuildGraph`; `CONTEXT.md` terms for Checked Self-Boot Package Locks,
   Self-Boot Locked Local Package Closure, Canonical Self-Boot Package Lock
   Format, Exact Locked Local Package Identity, First Self-Boot Lock
   Responsibility, and Self-Boot Substrate Fingerprint; and the accepted
   platform-contract ADR. Keep this round limited to pure checked-lock
   validation over explicit facts.
3. Add `src/MLF/Platform/PackageLock.hs` with the lock, current-snapshot,
   evidence, violation, validation, and deterministic rendering functions
   listed above. Reuse existing package/module/build metadata types where that
   keeps ownership clean; introduce small platform-local newtypes only for
   lock-specific facts such as normalized local root and substrate fingerprint
   material.
4. Add `test/PlatformPackageLockSpec.hs` with positive evidence, ordering
   stability, ABI/substrate drift, package-root drift, source metadata drift,
   dependency-id drift, dependency-interface metadata drift, interface metadata
   drift, missing/extra package diagnostics, missing/extra module diagnostics,
   duplicate diagnostics, blank-field diagnostics, and pure repeatability over
   explicit snapshots. Wire it into both `mlf2.cabal` and `test/Main.hs`.
5. Add `test/golden/platform-contract/checked-package-lock.txt` for the
   canonical rendering produced by `renderSelfBootPackageLock` or by
   `renderPackageLockEvidence` for the accepted lock. The focused spec must
   prove reordered package/module/dependency inputs render identically.
6. Update `docs/architecture.md` to name `MLF.Platform.PackageLock` as the
   pure checked local package lock validation owner. State that package root
   discovery, source hashing/regeneration, final lock-file parsing, package
   solving, generated binding drift closure, native command/link/execution
   records, proof-manifest emission, and proof closeout remain later slices.
7. Run the focused commands, static guard, full standard gate, and thesis gate
   below. If the full test suite mutates generated `runtime/mlfp_io/target/`
   artifacts, restore or remove only those generated artifacts after
   verification and preserve unrelated user changes.
8. Write `orchestrator/rounds/round-361/implementation-notes.md` with changed
   files, package lock surfaces implemented, command results, static guard
   output, and explicit non-claims.

### Verification
Focused commands:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform contract substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform package lock"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "renders checked local package lock evidence deterministically"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects checked package lock drift with named diagnostics"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "keeps package lock validation pure over explicit snapshots"'`

Static platform package-lock guard:

```sh
ruby - <<'RUBY'
required_files = [
  "src/MLF/Platform/Contract.hs",
  "src/MLF/Platform/EnvironmentPolicy.hs",
  "src/MLF/Platform/ToolchainIdentity.hs",
  "src/MLF/Platform/PackageLock.hs",
  "test/PlatformContractSpec.hs",
  "test/PlatformEnvironmentPolicySpec.hs",
  "test/PlatformToolchainIdentitySpec.hs",
  "test/PlatformPackageLockSpec.hs",
  "test/golden/platform-contract/minimal-substrate-contract.txt",
  "test/golden/platform-contract/normalized-environment-policy.txt",
  "test/golden/platform-contract/host-toolchain-identity.txt",
  "test/golden/platform-contract/checked-package-lock.txt",
  "test/Main.hs",
  "mlf2.cabal",
  "docs/architecture.md"
]

missing_files = required_files.reject { |path| File.exist?(path) }
abort("missing required round-361 files: #{missing_files.inspect}") unless missing_files.empty?

changed = `git diff --name-only HEAD`.lines.map(&:chomp)
banned_paths = changed.grep(%r{\A(orchestrator/state\.json|orchestrator/roadmaps/|src-public/|src/MLF/Backend/|src/MLF/Program/|runtime/)})
abort("out-of-scope paths changed: #{banned_paths.inspect}") unless banned_paths.empty?

cabal = File.read("mlf2.cabal")
main = File.read("test/Main.hs")
contract = File.read("src/MLF/Platform/Contract.hs")
environment_policy = File.read("src/MLF/Platform/EnvironmentPolicy.hs")
toolchain = File.read("src/MLF/Platform/ToolchainIdentity.hs")
package_lock = File.read("src/MLF/Platform/PackageLock.hs")
package_lock_spec = File.read("test/PlatformPackageLockSpec.hs")
arch = File.read("docs/architecture.md")

required_package_lock_terms = [
  "SelfBootPackageLock",
  "LockedPackageIdentity",
  "LockedPackageRoot",
  "LockedPackageEntry",
  "LockedModuleEntry",
  "LockedDependencyInterface",
  "CurrentPackageLockSnapshot",
  "PackageLockEvidence",
  "PackageLockViolation",
  "validateSelfBootPackageLock",
  "renderSelfBootPackageLock",
  "renderPackageLockEvidence",
  "renderPackageLockViolation",
  "renderPackageLockViolations"
]

missing_package_lock_terms = required_package_lock_terms.reject { |term| package_lock.include?(term) }
abort("missing package lock terms: #{missing_package_lock_terms.inspect}") unless missing_package_lock_terms.empty?

abort("mlf2.cabal missing MLF.Platform.PackageLock") unless cabal.include?("MLF.Platform.PackageLock")
abort("mlf2.cabal missing PlatformPackageLockSpec") unless cabal.include?("PlatformPackageLockSpec")
abort("test/Main.hs missing PlatformPackageLockSpec") unless main.include?("PlatformPackageLockSpec")
abort("docs/architecture.md missing package lock owner entry") unless arch.include?("MLF.Platform.PackageLock")

required_spec_phrases = [
  "renders checked local package lock evidence deterministically",
  "rejects checked package lock drift with named diagnostics",
  "keeps package lock validation pure over explicit snapshots",
  "ABI version",
  "substrate fingerprint",
  "source metadata",
  "dependency interface metadata",
  "interface metadata",
  "normalized local root"
]

missing_spec_phrases = required_spec_phrases.reject { |term| package_lock_spec.include?(term) }
abort("missing package lock spec coverage phrases: #{missing_spec_phrases.inspect}") unless missing_spec_phrases.empty?

banned_import_or_probe_terms = [
  "System.Directory",
  "System.Environment",
  "System.Process",
  "System.Random",
  "Data.Time",
  "getCurrentTime",
  "lookupEnv",
  "getEnv",
  "findExecutable",
  "doesFileExist",
  "listDirectory",
  "readProcess",
  "callProcess",
  "createProcess",
  "discoverLocatedProgramPackage",
  "discoverLocatedProgramPackageFromSearchPath",
  "parseLocatedProgramWithFile",
  "parseRawProgram"
]

probe_scan = contract + "\n" + environment_policy + "\n" + toolchain + "\n" + package_lock + "\n" + package_lock_spec
probe_hits = banned_import_or_probe_terms.select { |term| probe_scan.include?(term) }
abort("package lock slice performs host probing or package discovery: #{probe_hits.inspect}") unless probe_hits.empty?

format_hits = ["Aeson", "Toml", "TOML", "Yaml", "YAML", "JSON"].select do |term|
  probe_scan.include?(term)
end
abort("package lock slice introduced non-canonical external format terms: #{format_hits.inspect}") unless format_hits.empty?

overclaim_patterns = [
  /M5 closeout/i,
  /milestone-5.*done/i,
  /self-boot complete/i,
  /full self-boot/i,
  /compiler-package complete/i,
  /native backend complete/i,
  /package-manager complete/i,
  /lockfile parser complete/i,
  /generated binding drift closure complete/i,
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

puts "round-361 platform package-lock static guard passed"
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
- the checked package lock golden path and what it proves;
- the package lock drift surfaces covered by tests;
- explicit non-claims for generated binding drift closure, package-manager
  completion, lockfile parser completion, native command records, native link
  records, native execution records, platform/proof closeout, and self-boot
  completion.

### Scheduler
- Depends on round ids: round-358, round-359, round-360
- Merge after item ids: item-358-platform-substrate-contract-fingerprint-material, item-359-ambient-loader-policy-validation, item-360-host-toolchain-identity-validation
- Parallel group: serial-m5-platform-contract-substrate

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: The main implementer owns the source/test/docs changes and
  `orchestrator/rounds/round-361/implementation-notes.md` in this worktree.
  Reviewer should check the static guard, focused tests, full standard gate,
  thesis gate, scope boundaries, generated-artifact cleanup, and non-claims
  before approving. No planner-stage implementation is allowed because this
  task is `Complexity: standard`.
