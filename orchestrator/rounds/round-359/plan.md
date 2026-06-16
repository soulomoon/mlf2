### Selected Extraction
- Milestone: Self-Boot Platform Contract Implementation
- Milestone id: milestone-5-self-boot-platform-contract-implementation
- Direction id: direction-5a-platform-contract-substrate
- Extracted item id: item-359-ambient-loader-policy-validation
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Add the next M5 platform-contract slice after round 358: pure, test-backed
validation and deterministic evidence rendering for self-boot ambient-input
and loader-environment policies.

Round 358 made ambient-input and loader-environment policies part of typed
substrate declarations and fingerprint material. This round must make those
policies enforceable against explicit snapshots without reading the host
environment, running native executables, invoking linkers, or claiming native
execution records. The result should be a reusable policy validator and
evidence renderer that later native execution record and proof-manifest slices
can consume.

This is not M5 closeout. It must not claim lock validation, generated binding
drift closure, host toolchain discovery, native link records, native execution
records, package-manager/linker completion, platform/proof closeout, or
self-boot completion.

### Approach
The active rev-007 roadmap remains specific enough for a bounded M5 slice under
`direction-5a-platform-contract-substrate`. Select ambient/loader policy
validation because it directly follows the policy declarations added in round
358 and advances one remaining M5 completion signal without duplicating the
substrate declaration/fingerprint-material work.

Keep the implementation pure over caller-provided observations:

- Extend `MLF.Platform.Contract` so `AmbientInputPolicy` and
  `LoaderEnvironmentPolicy` carry typed rules rather than opaque string lists.
  Each rule must name the input or loader variable and an explicit disposition:
  scrubbed, declared, or normalized. Update the round-358 golden and
  `PlatformContractSpec` so typed policy rules still render deterministically
  and remain part of substrate fingerprint material.
- Add `src/MLF/Platform/EnvironmentPolicy.hs` as the owner for validating
  explicit policy snapshots. It should import the policy rule types from
  `MLF.Platform.Contract` instead of duplicating policy declarations.
- Add `test/PlatformEnvironmentPolicySpec.hs`, wire it into `mlf2.cabal` and
  `test/Main.hs`, and keep assertions explicit for both accepted evidence and
  rejected violations.
- Add `test/golden/platform-contract/normalized-environment-policy.txt` as the
  deterministic evidence rendering for one valid ambient/loader snapshot.
- Update `docs/architecture.md` with the new owner entry and preserve the
  round-358 non-claims.

The policy model must include at least these names:

- `AmbientInputName`
- `AmbientInputDisposition`
- `AmbientInputRule`
- `LoaderEnvironmentVariable`
- `LoaderEnvironmentDisposition`
- `LoaderEnvironmentRule`
- `ObservedAmbientInput`
- `ObservedLoaderEnvironmentVariable`
- `EnvironmentPolicySnapshot`
- `EnvironmentPolicyEvidence`
- `EnvironmentPolicyViolation`
- `validatePlatformEnvironmentPolicies`
- `renderPlatformEnvironmentPolicyEvidence`
- `renderPlatformEnvironmentPolicyViolation`
- `renderPlatformEnvironmentPolicyViolations`

Validation must reject all of these with meaningful diagnostics:

- duplicate ambient-input rules;
- duplicate loader-environment rules;
- observed proof-affecting ambient input with no rule;
- observed loader-affecting environment variable with no rule;
- an ambient input declared as scrubbed but observed as present;
- a loader variable declared as scrubbed but observed as present;
- an input or variable declared as normalized whose observed normalized value
  does not match the policy value;
- blank input names, variable names, or normalized values.

The validator may accept explicit `declared` observations when the name and
value match a declared rule. It may accept missing observations for scrubbed
rules. It must sort output by stable names so evidence does not depend on input
order. It must not call `System.Environment`, `System.Process`, `Data.Time`,
`System.Random`, native linkers, native runners, or any host probing API. Any
real host environment capture belongs to a later native execution record slice.

### Execution Profile
- Complexity: standard
- Verification profile: standard
- Reason: The selected slice introduces a new platform policy validation owner,
  new violation taxonomy, typed policy-rule semantics that affect existing
  substrate fingerprint material, new golden evidence, Cabal/test wiring, and
  architecture ownership docs. The task content needs design judgment and is
  therefore not a planner-direct simple wiring change. It is still bounded to
  one M5 contract surface and is not milestone closeout.

### Steps
1. Confirm the assigned worktree is
   `/Volumes/src/mlf4/orchestrator/worktrees/round-359` on branch
   `orchestrator/round-359-platform-contract-next`. Treat the parent checkout
   `orchestrator/state.json` as authoritative and do not edit any
   `orchestrator/state.json` file.
2. Re-read round 358 artifacts, `src/MLF/Platform/Contract.hs`,
   `test/PlatformContractSpec.hs`, `CONTEXT.md` terms for
   Self-Boot Ambient Input Policy, Self-Boot Loader Environment Policy,
   ambient/loader violations, and the accepted platform-contract ADR. Keep this
   round limited to pure policy validation over explicit snapshots.
3. Extend `src/MLF/Platform/Contract.hs` with typed ambient and loader policy
   rule types. Preserve the existing ABI/package/target/component/toolchain
   contract owner and update `renderPlatformSubstrateContract` and
   `renderSubstrateFingerprintMaterial` so typed policy rule disposition and
   normalized values are included in sorted canonical output.
4. Add `src/MLF/Platform/EnvironmentPolicy.hs` with the snapshot, evidence,
   violation, validation, and deterministic rendering functions listed above.
   Keep it pure and free of host environment/process/time/random imports.
5. Update `test/PlatformContractSpec.hs` and
   `test/golden/platform-contract/minimal-substrate-contract.txt` for the
   typed policy rule rendering. Add a focused assertion that changing a policy
   rule disposition or normalized value changes substrate fingerprint material.
6. Add `test/PlatformEnvironmentPolicySpec.hs` with positive evidence,
   ordering stability, and each negative diagnostic listed above. Wire it into
   both `mlf2.cabal` and `test/Main.hs`.
7. Add `test/golden/platform-contract/normalized-environment-policy.txt` for
   the canonical evidence rendering produced by
   `renderPlatformEnvironmentPolicyEvidence`.
8. Update `docs/architecture.md` to name `MLF.Platform.EnvironmentPolicy` as
   the pure ambient/loader policy validator and to state that real host
   capture, native execution records, link records, and proof-manifest emission
   remain future slices.
9. Run the focused commands, static guard, full standard gate, and thesis gate
   below. If the full test suite mutates generated `runtime/mlfp_io/target/`
   artifacts, restore or remove only those generated artifacts after
   verification and preserve unrelated user changes.
10. Write `orchestrator/rounds/round-359/implementation-notes.md` with changed
   files, policy surfaces implemented, command results, static guard output,
   and explicit non-claims.

### Verification
Focused commands:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform contract substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Platform environment policy"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "renders normalized ambient and loader policy evidence deterministically"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects undeclared ambient and loader inputs with distinct diagnostics"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "keeps environment policy validation pure over explicit snapshots"'`

Static platform environment-policy guard:

```sh
ruby - <<'RUBY'
required_files = [
  "src/MLF/Platform/Contract.hs",
  "src/MLF/Platform/EnvironmentPolicy.hs",
  "test/PlatformContractSpec.hs",
  "test/PlatformEnvironmentPolicySpec.hs",
  "test/golden/platform-contract/minimal-substrate-contract.txt",
  "test/golden/platform-contract/normalized-environment-policy.txt",
  "test/Main.hs",
  "mlf2.cabal",
  "docs/architecture.md"
]

missing_files = required_files.reject { |path| File.exist?(path) }
abort("missing required round-359 files: #{missing_files.inspect}") unless missing_files.empty?

changed = `git diff --name-only HEAD`.lines.map(&:chomp)
banned_paths = changed.grep(%r{\A(orchestrator/state\.json|orchestrator/roadmaps/|src-public/|src/MLF/Backend/|src/MLF/Program/|runtime/)})
abort("out-of-scope paths changed: #{banned_paths.inspect}") unless banned_paths.empty?

cabal = File.read("mlf2.cabal")
main = File.read("test/Main.hs")
contract = File.read("src/MLF/Platform/Contract.hs")
policy = File.read("src/MLF/Platform/EnvironmentPolicy.hs")
policy_spec = File.read("test/PlatformEnvironmentPolicySpec.hs")
contract_spec = File.read("test/PlatformContractSpec.hs")
arch = File.read("docs/architecture.md")

required_contract_terms = [
  "AmbientInputName",
  "AmbientInputDisposition",
  "AmbientInputRule",
  "LoaderEnvironmentVariable",
  "LoaderEnvironmentDisposition",
  "LoaderEnvironmentRule",
  "renderSubstrateFingerprintMaterial"
]

required_policy_terms = [
  "ObservedAmbientInput",
  "ObservedLoaderEnvironmentVariable",
  "EnvironmentPolicySnapshot",
  "EnvironmentPolicyEvidence",
  "EnvironmentPolicyViolation",
  "validatePlatformEnvironmentPolicies",
  "renderPlatformEnvironmentPolicyEvidence",
  "renderPlatformEnvironmentPolicyViolation",
  "renderPlatformEnvironmentPolicyViolations"
]

missing_contract_terms = required_contract_terms.reject { |term| contract.include?(term) }
abort("missing typed policy contract terms: #{missing_contract_terms.inspect}") unless missing_contract_terms.empty?

missing_policy_terms = required_policy_terms.reject { |term| policy.include?(term) }
abort("missing environment policy terms: #{missing_policy_terms.inspect}") unless missing_policy_terms.empty?

abort("mlf2.cabal missing MLF.Platform.EnvironmentPolicy") unless cabal.include?("MLF.Platform.EnvironmentPolicy")
abort("mlf2.cabal missing PlatformEnvironmentPolicySpec") unless cabal.include?("PlatformEnvironmentPolicySpec")
abort("test/Main.hs missing PlatformEnvironmentPolicySpec") unless main.include?("PlatformEnvironmentPolicySpec")
abort("docs/architecture.md missing environment policy owner entry") unless arch.include?("MLF.Platform.EnvironmentPolicy")

required_spec_phrases = [
  "renders normalized ambient and loader policy evidence deterministically",
  "rejects undeclared ambient and loader inputs with distinct diagnostics",
  "keeps environment policy validation pure over explicit snapshots",
  "duplicate ambient-input rule",
  "duplicate loader-environment rule",
  "scrubbed",
  "normalized"
]

missing_spec_phrases = required_spec_phrases.reject { |term| policy_spec.include?(term) || contract_spec.include?(term) }
abort("missing policy spec coverage phrases: #{missing_spec_phrases.inspect}") unless missing_spec_phrases.empty?

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
  "createProcess",
  "findExecutable"
]

probe_scan = contract + "\n" + policy + "\n" + policy_spec
probe_hits = banned_import_or_probe_terms.select { |term| probe_scan.include?(term) }
abort("environment policy slice performs host probing: #{probe_hits.inspect}") unless probe_hits.empty?

format_hits = ["Aeson", "Toml", "TOML", "Yaml", "YAML", "JSON"].select do |term|
  probe_scan.include?(term)
end
abort("environment policy slice introduced non-canonical external format terms: #{format_hits.inspect}") unless format_hits.empty?

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
  /native execution records complete/i,
  /host toolchain discovery complete/i,
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

puts "round-359 platform environment-policy static guard passed"
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
- the normalized environment-policy golden path and what it proves;
- the policy-rule fingerprint-material drift surfaces covered by tests;
- explicit non-claims for lock validation, generated binding drift closure,
  host toolchain discovery, native link records, native execution records,
  package-manager/linker completion, platform/proof closeout, and self-boot
  completion.

### Scheduler
- Depends on round ids: round-358
- Merge after item ids: item-358-platform-substrate-contract-fingerprint-material
- Parallel group: serial-m5-platform-contract-substrate

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: The main implementer owns the source/test/docs changes and
  `orchestrator/rounds/round-359/implementation-notes.md` in this worktree.
  Reviewer should check the static guard, focused tests, full standard gate,
  thesis gate, scope boundaries, generated-artifact cleanup, and non-claims
  before approving. No planner-stage implementation is allowed because this
  task is `Complexity: standard`.
