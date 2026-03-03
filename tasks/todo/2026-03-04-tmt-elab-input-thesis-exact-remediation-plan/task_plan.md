# Task Plan — 2026-03-04 TMT Elaboration Input Thesis-Exact Remediation Plan

## Goal
Execute the agent-team implementation plan and make the Transformation Mechanism Table row `Elaboration input` thesis-exact.

## Phases
- [completed] Wave 0 / Team A: Harden row guards and confirm Gate A RED.
- [in_progress] Wave 1 / Teams B+C: Build chi-native helper surfaces and migrate active Phi signatures in parallel.
- [pending] Wave 2 / Team D: Rewire Elaborate/runtime call paths to chi-native contracts.
- [pending] Wave 3 / Team E: Final verification and docs closeout.

## Decisions
- Follow wave order exactly: Wave 0 -> Wave 1 (B/C parallel) -> Wave 2 -> Wave 3.
- Keep checked-authoritative behavior and Phi missing-trace fail-fast unchanged.

## Errors Encountered
- `cabal test` commands return non-zero in sandbox after suite completion due:
  `/Users/ares/.cache/cabal/logs/build.log: withFile: permission denied`.
  Recovery: treat Hspec suite summary (`PASS`/`FAIL`) as gate signal and capture it explicitly.
