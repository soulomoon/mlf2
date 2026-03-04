# Task Plan — 2026-03-04 TMT Elaboration Input Thesis-Exact Remediation Plan

## Goal
Execute the agent-team implementation plan and make the Transformation Mechanism Table row `Elaboration input` thesis-exact.

## Phases
- [completed] Wave 0 / Team A: Harden row guards and confirm Gate A RED.
- [completed] Wave 1 / Teams B+C: Build chi-native helper surfaces and migrate active Phi signatures in parallel.
- [completed] Wave 2 / Team D: Rewire Elaborate/runtime call paths to chi-native contracts.
- [completed] Wave 3 / Team E: Final verification and docs closeout.

## Decisions
- Follow wave order exactly: Wave 0 -> Wave 1 (B/C parallel) -> Wave 2 -> Wave 3.
- Keep checked-authoritative behavior and Phi missing-trace fail-fast unchanged.
- Treat already-run gate outcomes as closeout authority for Team E docs verification
  and record evidence verbatim in owned artifacts.

## Errors Encountered
- `cabal test` commands return non-zero in sandbox after suite completion due:
  `/Users/ares/.cache/cabal/logs/build.log: withFile: permission denied`.
  Recovery: treat Hspec suite summary (`PASS`/`FAIL`) as gate signal and capture it explicitly.
- During Wave 1 integration, `git cherry-pick` was blocked with
  `Unable to create '.git/index.lock': Operation not permitted`.
  Recovery: integrated team worktree outputs by copying owned files and committing with
  exact task pathspecs.
- Planning helper bootstrap failed:
  `.../planning-with-files/scripts/session-catchup.py: [Errno 2] No such file or directory`.
  Recovery: proceeded with direct in-repo task file review/update for session state.

## Closeout Verification Evidence (already-run gates)
- `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
- `checked-authoritative`: PASS (`8 examples, 0 failures`)
- `Dual-path verification`: PASS (`4 examples, 0 failures`)
- `cabal build all && cabal test`: PASS
