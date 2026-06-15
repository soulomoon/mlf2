### Selected Extraction
- Milestone: Self-Boot Platform Contract Implementation
- Milestone id: milestone-5-self-boot-platform-contract-implementation
- Direction id: direction-5a-platform-contract-substrate
- Extracted item id: item-358-platform-substrate-contract-fingerprint-material
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Introduce the first test-backed owner for the platform substrate contract
declaration and canonical substrate fingerprint material. The selected slice
must make ABI version, contract package identity, target triple, trusted
substrate component identities, host toolchain identity fields, ambient-input
policy, and loader-environment policy explicit in typed Haskell and in a
deterministic canonical rendering.

This is not M5 closeout. It must not claim completed self-boot, package-manager
lock validation, generated binding drift closure, native backend completion,
native link records, native execution records, proof-stage cache validity, or a
final cryptographic substrate digest. The intended output is the dependency
surface that later M5 slices can consume for lock validation, generated binding
drift checks, native link records, and native execution records.

### Approach
The active roadmap is specific enough to select a bounded dependency-ready M5
round. Round 357 closed the M4 parser/compiler-frontend dependency, and the
rev-007 M5 direction asks for explicit platform substrate and ABI contracts.
Select the first substrate-contract slice instead of attempting a milestone
closeout or widening into backend/native execution work.

Implement one internal owner module, one focused spec, and one golden rendering
fixture:

- Add `src/MLF/Platform/Contract.hs` under the `mlf2-internal` library. This
  module owns the typed contract model, validation errors, deterministic
  rendering, and canonical fingerprint-material rendering for the first
  platform substrate contract surface.
- Add `test/PlatformContractSpec.hs`, wire it into `mlf2.cabal` and
  `test/Main.hs`, and use explicit assertions for success and failure paths.
  Do not accept `Left _` / `Right _` catch-all success as adequate evidence.
- Add `test/golden/platform-contract/minimal-substrate-contract.txt` as the
  expected canonical rendering for a minimal valid contract package seed.
- Update `docs/architecture.md` with a short module ownership entry for the
  platform contract owner. Keep it scoped to the new substrate-contract
  declaration surface and avoid roadmap-closeout language.

The first contract model must include at least these concepts:

- `PlatformAbiVersion`
- `PlatformSubstrateContractPackageId`
- `PlatformSubstrateContractPackageVersion`
- `TargetTriple`
- `SubstrateComponentKind`
- `SubstrateComponentName`
- `SubstrateComponentDigest`
- `SubstrateComponent`
- `ToolchainToolRole`
- `ResolvedToolIdentity`
- `HostToolchainContract`
- `AmbientInputPolicy`
- `LoaderEnvironmentPolicy`
- `PlatformSubstrateContract`
- `PlatformContractError`

Validation must reject all of these with meaningful errors:

- empty or missing ABI version;
- empty or missing contract package id/version;
- empty or missing target triple;
- duplicate substrate component keys;
- duplicate host toolchain roles;
- a host toolchain identity that only supplies a version string and no resolved
  path plus digest-or-explicit-unavailable reason;
- missing ambient-input policy;
- missing loader-environment policy.

Rendering must be stable and line-oriented. Use an owner-local ASCII canonical
format such as `mlf-platform-substrate-contract-v1`, with sorted component and
toolchain sections by stable keys. Do not introduce JSON, YAML, TOML, fixture
name shortcuts, pre-rendered parser outputs, canonical-parser bypasses, or
runtime host probing in this round. The contract module may model digest text
and fingerprint material, but it must not pretend to have closed the final
cryptographic digest algorithm unless the implementation actually adds and
verifies a deliberate digest dependency and policy.

The focused spec must prove:

- canonical rendering matches the golden file;
- equivalent contracts with differently ordered components/tools render the
  same canonical fingerprint material;
- changing ABI version, target triple, substrate component digest, host
  toolchain identity, ambient-input policy, or loader-environment policy changes
  the canonical fingerprint material;
- each validation failure renders a diagnostic that names the failing contract
  surface;
- no environment, wall clock, random, linker, native execution, or host
  tool-path probing action is performed by the contract owner.

### Execution Profile
- Complexity: standard
- Verification profile: standard
- Reason: This is new source and test behavior for the first M5 platform
  substrate contract owner. It is bounded to one contract surface, but it adds
  new internal modules, Cabal wiring, focused negative diagnostics, golden
  evidence, and thesis-facing substrate identity semantics. It is not a simple
  planner-direct documentation-only task and it is not milestone closeout.

### Steps
1. Confirm the assigned worktree is
   `/Volumes/src/mlf4/orchestrator/worktrees/round-358` on branch
   `orchestrator/round-358-platform-contract-substrate`. Treat the parent
   checkout `orchestrator/state.json` as authoritative and do not edit any
   `orchestrator/state.json` file.
2. Re-read `CONTEXT.md` terms for Platform Substrate Contract Package,
   Self-Boot Substrate Fingerprint, Host Toolchain Contract, Ambient input
   policy, native link records, and native execution records, plus
   `docs/adr/2026-05-18-self-boot-platform-contract.md`. Keep this round
   limited to the declaration/fingerprint-material substrate slice.
3. Add `src/MLF/Platform/Contract.hs` with the data model, smart constructors
   or validation entrypoint, error renderer, deterministic contract renderer,
   and canonical fingerprint-material renderer. Prefer one module for this
   first owner; split only if the file becomes materially harder to review.
4. Add the golden rendering fixture under
   `test/golden/platform-contract/minimal-substrate-contract.txt`.
5. Add `test/PlatformContractSpec.hs` with the positive, drift, and negative
   tests listed above. Wire the spec into both `mlf2.cabal` and `test/Main.hs`.
6. Update the `mlf2-internal` `other-modules` stanza in `mlf2.cabal` with
   `MLF.Platform.Contract`. Update the test stanza with
   `PlatformContractSpec`.
7. Update `docs/architecture.md` with the owner entry and explicit non-claims:
   this module owns declaration/fingerprint-material substrate identity only,
   not lock validation, generated bindings, native link records, native
   execution records, or self-boot proof completion.
8. Run the focused commands, static guard, full standard gate, and thesis gate
   listed below. If the full test suite mutates generated
   `runtime/mlfp_io/target/` artifacts, restore or remove only those generated
   artifacts after verification and preserve unrelated user changes.
9. Write `orchestrator/rounds/round-358/implementation-notes.md` with changed
   files, contract surfaces implemented, command results, static guard output,
   and explicit non-claims.

### Verification
Focused commands:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform contract substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "renders deterministic substrate contract declarations"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "changes substrate fingerprint material when declared platform identity changes"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects invalid platform substrate contract declarations with named diagnostics"'`

Static platform-contract guard:

```sh
ruby - <<'RUBY'
required_files = [
  "src/MLF/Platform/Contract.hs",
  "test/PlatformContractSpec.hs",
  "test/golden/platform-contract/minimal-substrate-contract.txt",
  "test/Main.hs",
  "mlf2.cabal",
  "docs/architecture.md"
]

missing_files = required_files.reject { |path| File.exist?(path) }
abort("missing required round-358 files: #{missing_files.inspect}") unless missing_files.empty?

changed = `git diff --name-only HEAD`.lines.map(&:chomp)
banned_paths = changed.grep(%r{\A(orchestrator/state\.json|orchestrator/roadmaps/|src-public/|src/MLF/Backend/|src/MLF/Program/|runtime/)})
abort("out-of-scope paths changed: #{banned_paths.inspect}") unless banned_paths.empty?

cabal = File.read("mlf2.cabal")
main = File.read("test/Main.hs")
contract = File.read("src/MLF/Platform/Contract.hs")
spec = File.read("test/PlatformContractSpec.hs")
arch = File.read("docs/architecture.md")

required_contract_terms = [
  "PlatformAbiVersion",
  "PlatformSubstrateContractPackageId",
  "PlatformSubstrateContractPackageVersion",
  "TargetTriple",
  "SubstrateComponentKind",
  "SubstrateComponentName",
  "SubstrateComponentDigest",
  "SubstrateComponent",
  "ToolchainToolRole",
  "ResolvedToolIdentity",
  "HostToolchainContract",
  "AmbientInputPolicy",
  "LoaderEnvironmentPolicy",
  "PlatformSubstrateContract",
  "PlatformContractError",
  "validatePlatformSubstrateContract",
  "renderPlatformSubstrateContract",
  "renderSubstrateFingerprintMaterial"
]

missing_terms = required_contract_terms.reject { |term| contract.include?(term) }
abort("missing contract terms: #{missing_terms.inspect}") unless missing_terms.empty?

abort("mlf2.cabal missing MLF.Platform.Contract") unless cabal.include?("MLF.Platform.Contract")
abort("mlf2.cabal missing PlatformContractSpec") unless cabal.include?("PlatformContractSpec")
abort("test/Main.hs missing PlatformContractSpec") unless main.include?("PlatformContractSpec")
abort("docs/architecture.md missing platform contract owner entry") unless arch.include?("MLF.Platform.Contract")

banned_import_or_probe_terms = [
  "System.Environment",
  "System.Process",
  "System.Random",
  "Data.Time",
  "getCurrentTime",
  "lookupEnv",
  "getEnv",
  "readProcess",
  "callProcess",
  "createProcess"
]

probe_hits = banned_import_or_probe_terms.select { |term| contract.include?(term) || spec.include?(term) }
abort("platform contract owner/spec performs host probing: #{probe_hits.inspect}") unless probe_hits.empty?

format_hits = ["Aeson", "Toml", "TOML", "Yaml", "YAML", "JSON"].select do |term|
  contract.include?(term) || spec.include?(term)
end
abort("platform contract slice introduced non-canonical external format terms: #{format_hits.inspect}") unless format_hits.empty?

shortcut_hits = ["fixture-name shortcut", "pre-rendered parser output", "canonical-parser bypass", "parser-private hack"].select do |term|
  contract.include?(term) || spec.include?(term)
end
abort("shortcut language found in implementation: #{shortcut_hits.inspect}") unless shortcut_hits.empty?

overclaim_patterns = [
  /M5 closeout/i,
  /milestone-5.*done/i,
  /self-boot complete/i,
  /full self-boot/i,
  /compiler-package complete/i,
  /native backend complete/i,
  /package-manager complete/i,
  /linker complete/i,
  /native link records complete/i,
  /native execution records complete/i
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

puts "round-358 platform contract static guard passed"
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
- the canonical rendering fixture path and what it proves;
- the fingerprint-material drift surfaces covered by tests;
- explicit non-claims for final cryptographic digest, lock validation,
  generated binding drift closure, native link records, native execution
  records, package manager/linker completion, platform/proof closeout, and
  self-boot completion.

### Scheduler
- Depends on round ids: round-357 (satisfied M4 parser/compiler-frontend
  closeout dependency)
- Merge after item ids: item-357-m4-canonical-parser-parity-closeout-audit
- Parallel group: serial-m5-platform-contract-substrate

### Worker Fan-Out
- Worker mode: delegated single implementer
- Workers: 1
- Integration: The implementer owns the source/test/docs changes and
  `orchestrator/rounds/round-358/implementation-notes.md` in this worktree.
  No planner-stage implementation is allowed because the selected task is
  `Complexity: standard`. Reviewer should check the static guard, focused
  tests, full standard gate, thesis gate, and non-claims before approving.
