# Round 016 Implementation Notes

- Added the internal-only `uri-r2-c1-prototype-entrypoint-v1` runner, locked to `P1-subject-discovery` and `uri-r2-c1-only-v1`, and threaded it through the existing `mlf2` executable without changing the default no-argument path.
- Implemented the bounded `P1` prototype writer under `src/MLF/Research/URI/R2/C1/Prototype/`, including deterministic attempt-local evidence JSON and the canonical stage artifact at `docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md`.
- Added focused Hspec coverage for entrypoint isolation, selector/scenario rejection, required evidence files, subject-token gating, bounded identity fields, and default-path preservation.
- Repaired the `P1` evidence contract for rerun `attempt-2` by introducing typed schema carriers for repeated invocation metadata, trace bundles, checker outputs, aggregated stage verdicts, and canonical subject-token provenance or owner-family fields before re-emitting the authoritative machine-readable outputs under `orchestrator/rounds/round-016/evidence/P1/attempt-2/`.
- Re-ran `P1` as `attempt-2`, preserving `attempt-1` as historical evidence while updating the canonical `docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md` artifact to point at the repaired attempt.
