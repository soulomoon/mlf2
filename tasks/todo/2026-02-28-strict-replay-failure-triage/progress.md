# Progress Log: 2026-02-28 strict replay failure triage

## Session Start
- Initialized task tracking files.
- Ran targeted failures and captured mismatch outputs.

## Reproduction
- Initial target failures matched user report across Elaboration/Pipeline cases.
- Captured witness/trace debug prints showing no-replay edges with empty replay domains.

## Root-Cause Debugging
- Diffed `WitnessNorm`, `Translate`, `Omega` and traced no-replay flow.
- Confirmed interaction among:
  - no-replay witness projection/pruning
  - source-vs-replay scheme selection in Translate
  - no-replay weaken target rescue/index elimination in Omega

## Validation
- Ran full 11-test matrix (6 target + 5 protected) with recommended code shape.
- All tests passed.
