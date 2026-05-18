# ADR: Migrate File-Based Conformance Before Self-Boot

Date: 2026-05-18
Status: Accepted

## Context

The current `.mlfp` compiler evidence is split across Haskell unit tests,
inline parser/checker assertions, `test/programs/` fixtures, program-matrix
expectations, and the compiler frontend seed package. That evidence is useful
for the Haskell implementation, but it is not yet a shared oracle that a future
compiler written in `.mlfp` can run unchanged.

Full self-boot needs a way to catch bugs in both implementations. The Haskell
compiler and the future `.mlfp` compiler must compile, run, and reject the same
source fixtures with the same stable expected results before stage equivalence
can be trusted.

## Decision

Before implementing Full Self-Boot, create a **Shared File-Based Compiler
Conformance Corpus** as its own prerequisite workstream. Behavior-level `.mlfp`
program tests will migrate one by one into a dedicated
`test/conformance/mlfp/` tree with machine-readable fixture metadata,
per-command expected output files, committed goldens, and a small documented
normalization pass for environment-dependent output.

The corpus is the shared oracle for both compiler implementations. The Haskell
compiler must continue to pass it while fixtures migrate, and the future
compiler written in `.mlfp` must run the same fixtures and compare against the
same expected results. Output parity is exact after documented normalization,
including expected diagnostics for failing fixtures.

The shared ABI, substrate, runtime, GC, local-package, and package-lock
contracts used by this corpus are defined separately in
`docs/adr/2026-05-18-self-boot-platform-contract.md`. This ADR depends on that
platform contract but remains focused on migrating compiler behavior fixtures
into one shared file-based corpus.

When the corpus is used in the first self-boot proof, it is one part of the
stage-shared conformance suite. The other part is the shared substrate ABI
conformance layer owned by the platform contract ADR. Both parts use committed
expected outputs and documented normalization; any stage-inapplicable fixture is
declared in metadata rather than skipped ad hoc.

The proof gate is conformance-first. A failed compiler-corpus fixture stops the
self-boot proof before normalized semantic artifact comparison, so behavior
oracle failures are not mixed with compiler-package semantic drift.

Conformance outputs used by the self-boot proof are stage-owned outputs. Stage
0 and stage 1 may read the same fixture inputs and committed expected files, but
they write fresh stage-local actual outputs and do not reuse actual-output
caches across stages.

Conformance fixture input paths normalize through manifest-declared fixture and
package roots. Actual outputs normalize through the stage-owned output root.
Path escapes are classified as input drift or proof-runner failure, not as
ambient filesystem behavior.

Conformance status and actual-output hashes from this corpus are recorded in the
self-boot proof manifest using the same `.mlfp`-owned canonical data format
family as manifests, locks, and substrate ABI declarations. Terminal output
from the conformance runner is not the proof record.
The conformance command records capture fixture runner argv, cwd, normalized
environment, stdin source or hash, stdout/stderr hashes or artifact paths, exit
status, owning stage, and stage-owned output directory.

Conformance fixtures must not depend on ambient wall-clock time, random seeds,
timezone, locale, or platform equivalents unless the fixture metadata declares
the input and expected normalization. Undeclared ambient input is recorded as an
ambient input violation rather than as ordinary conformance drift.

Compiler-corpus mismatches are recorded under the proof manifest's conformance
failure class. Missing, inconsistent, unsupported, or stage-inapplicable fixture
metadata is recorded under the fixture metadata failure class rather than hidden
as an ad hoc skip.

The migration is behavior-preserving by default. Production compiler changes are
allowed only when moving a fixture into the corpus exposes a real compiler bug.
Existing public `.mlfp` fixtures and CLI/package behavior migrate first.
Behavioral projections of internal invariants are added after the corpus
harness is stable, and only when the invariant has honest user-visible
behavior. Private implementation invariants that have no observable projection
remain Haskell tests.

## Rejected Alternatives

- Start self-boot implementation before the shared corpus. That would leave the
  future compiler without an implementation-independent oracle and make stage
  mismatches harder to localize.
- Keep Haskell-only behavior tests and later copy them into `.mlfp` compiler
  tests. That would create divergent expected-output systems instead of one
  conformance source of truth.
- Use dynamic expected-output generation during tests. That would hide oracle
  drift and make compiler regressions look like fixture updates.
- Project every internal invariant into `.mlfp` fixtures. Some invariants are
  private implementation facts and should stay in private Haskell tests unless
  they have a real user-visible behavior.
- Compare native object or executable bytes for first self-boot. Stage
  equivalence is semantic: exact conformance outputs plus normalized semantic
  artifacts, not byte-for-byte native artifacts.

## Consequences

- The next pre-self-boot step is the conformance-corpus migration, not an
  operational orchestrator roadmap and not immediate self-boot implementation.
- New or migrated fixtures must declare their package root, command modes,
  expected pass/fail status, normalization profile, and behavioral tags in
  metadata instead of relying on filename conventions or hidden harness
  defaults.
- Expected files are checked in and updated only through an explicit
  regeneration/review workflow.
- The conformance corpus becomes a prerequisite for later broad string/parser
  parity and full self-boot work because it gives both compiler implementations
  the same behavioral target.
- The substrate ABI and local-package proof inputs are governed by
  `docs/adr/2026-05-18-self-boot-platform-contract.md`; this conformance ADR
  should not duplicate that platform contract.
- Full Self-Boot remains stricter than this ADR: it still requires a `.mlfp`
  compiler driver, local package model, persisted semantic interfaces, separate
  compilation, backend continuity, target-scoped stable ABI, managed GC runtime,
  linker invocation, and stage equivalence.
