# Round 017 Plan Delta (`P2` repair after second rejected review)

## Repair Objective

Repair only the two remaining blockers from the second rejected review:

1. make `P2-W` and the stage verdict honest: if witness replay reports a diagnostic failure, partial replay, widened-domain requirement, repair requirement, subject drift, or correlation break, the run is a bounded non-pass, not `pass`; and
2. treat `mlf2.cabal` as an admitted compile-only necessity for this round, limited to the existing one-line registration of `MLF.Research.URI.R2.C1.Prototype.P2`.

This remains a `URI-R2-C1`-only, `uri-r2-c1-only-v1`-only, single-SCC, single-binder-family, non-equi-recursive, non-cyclic-graph, shared-entrypoint-isolation repair. Do not broaden into `P3`, `P4`, replay-algorithm refactors, or generic prototype cleanup.

## Locked Round Context

- Round id stays `round-017`.
- Branch stays `codex/round-017`.
- Worktree stays `.worktrees/round-017`.
- Shared research entrypoint stays `uri-r2-c1-prototype-entrypoint-v1`.
- Stage selector stays `P2-provenance-preservation`.
- Scenario stays `uri-r2-c1-only-v1`.
- Stage input stays only `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`.
- The next repaired execution must move to `orchestrator/rounds/round-017/evidence/P2/attempt-2/`.
- `attempt-1` is rejected-history evidence and must remain preserved byte-for-byte; do not rewrite, delete, or relabel it.
- The canonical artifact path stays `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`, but the rerun narrative must identify `Attempt: 2`.

## Repair Slice

Allowed carried-forward round diff and outputs for this repair:

1. `src/MLF/Research/URI/R2/C1/Prototype/P2.hs` - required; this is the only file that should receive new logic changes unless a strictly paired renderer update is unavoidable.
2. `mlf2.cabal` - admitted compile-only necessity; keep only the existing single-line `MLF.Research.URI.R2.C1.Prototype.P2` registration and do not make any other cabal edits.
3. `src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs` - only if needed to render the corrected bounded non-pass result or suppress `P3` handoff text when `P2` does not pass.
4. `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md` - regenerated canonical `P2` artifact for `Attempt: 2`.
5. `orchestrator/rounds/round-017/evidence/P2/attempt-2/` - new attempt-local raw evidence only.

Existing carried-forward round-local edits in the current diff may remain, but they must stay byte-stable unless the implementer can point to a direct dependency from the `P2-W` classification fix:

- `src/MLF/Research/URI/R2/C1/Prototype/Entrypoint.hs`
- `src/MLF/Research/URI/R2/C1/Prototype/Types.hs`
- `test/Research/UriR2C1PrototypeP1Spec.hs`

Files that must end this repair untouched:

- `src/MLF/Research/URI/R2/C1/Prototype/P1.hs`
- `orchestrator/rounds/round-017/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap/rev-002/roadmap.md`
- every file under `orchestrator/rounds/round-016/`
- `orchestrator/rounds/round-017/evidence/P2/attempt-1/`
- `app/Main.hs`
- every file under `src-public/`

## Sequential Repair Tasks

### Task 1 - Preserve the rejected audit trail and switch the rerun to `attempt-2`

- Keep `attempt-1` as the rejected evidence set; do not overwrite any file under `orchestrator/rounds/round-017/evidence/P2/attempt-1/`.
- Rerun the bounded `P2` stage only as `--attempt-id 2`.
- Regenerated attempt-local outputs must be written only under `orchestrator/rounds/round-017/evidence/P2/attempt-2/`.

### Task 2 - Reclassify replay failure as bounded non-pass instead of fabricated success

- Keep the real bounded execution path already in place: `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback -> witness replay`.
- Do not attempt a broader replay semantic fix in this round unless the exact bounded path already succeeds without widening or refactor. The safe target is honest classification.
- `P2-W` may be `pass` only when witness replay actually succeeds for the same canonical `subject_id` and the same shared `correlation_id`.
- If replay returns a diagnostic failure such as the current `applyInstantiation` error, classify `P2-W` as bounded non-pass:
  - use `semantic-negative` when the replay executes and returns a definite rejecting diagnostic within the bounded model;
  - use `inconclusive` only when the replay evidence is missing, partial, or cannot maintain the required shared `correlation_id`.
- In either non-pass case, `rejection_trigger` must be non-`none`, the stage verdict must also be non-pass, and no `subject-token.json` may be emitted for `attempt-2`.
- `P2-G`, `P2-S`, and `P2-R` may remain individually passing if their own evidence still holds, but `P2` as a stage passes only if all four checks pass for the same canonical subject token.

### Task 3 - Regenerate only the bounded `attempt-2` outputs needed for reviewer-visible truthfulness

- Write only these raw outputs under `orchestrator/rounds/round-017/evidence/P2/attempt-2/`:
  - `trace-bundle.json`
  - `check-P2-G.json`
  - `check-P2-S.json`
  - `check-P2-R.json`
  - `check-P2-W.json`
  - `stage-verdict.json`
  - `subject-token.json` only on true `pass`
- Keep one shared `subject_id` and one shared `correlation_id` across all four subchecks, or else classify the run as bounded non-pass.
- Regenerate the canonical `P2` artifact from `attempt-2` only. If `P2` is non-pass, the artifact must say so explicitly and must not present a `P3` handoff token.

### Task 4 - Keep the diff boundary explicit and reviewer-readable

- The final diff may continue to include the existing round-local code files already in scope: `mlf2.cabal`, `P2.hs`, `Artifact.hs`, `Entrypoint.hs`, `Types.hs`, and `test/Research/UriR2C1PrototypeP1Spec.hs`.
- No additional tracked code path may enter the diff.
- The reviewer must treat `mlf2.cabal` as in-slice for this rerun, but only as the one-line compile-registration necessity for the new `P2` module.

## Acceptance Criteria

1. The next bounded execution uses `--attempt-id 2`, and `attempt-1` remains preserved as the rejected audit trail.
2. `P2-W` is no longer recorded as `pass` when replay reports a bounded diagnostic failure; the same failure now yields `semantic-negative` or `inconclusive` with a non-`none` rejection trigger.
3. `stage-verdict.json` for `attempt-2` is non-pass whenever `P2-W` is non-pass, and `subject-token.json` is absent unless replay genuinely succeeds.
4. The canonical `P2` artifact reports `Attempt: 2`, matches the `attempt-2` evidence, and does not claim `P3` handoff on non-pass.
5. `mlf2.cabal` remains limited to the existing one-line module registration and is not treated as an out-of-slice diff.
6. The round remains inside `URI-R2-C1`, `uri-r2-c1-only-v1`, single-SCC, single-binder-family, non-equi-recursive, non-cyclic-graph, and shared-entrypoint-isolation boundaries.

## Reviewer And Verification Checks

Baseline checks from `orchestrator/roadmaps/2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap/rev-002/verification.md` still apply:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-017/state-snapshot.json >/dev/null`
- `rg -n '^\\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap/rev-002/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
- `cabal build all && cabal test`

Round-specific repair checks:

- Confirm the rerun preserved the audit trail and used the new attempt id:
  - `find orchestrator/rounds/round-017/evidence/P2/attempt-1 -maxdepth 1 -type f | sort`
  - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P2-provenance-preservation --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
  - `find orchestrator/rounds/round-017/evidence/P2/attempt-2 -maxdepth 1 -type f | sort`
- Confirm the admitted diff boundary:
  - `git diff --name-only`
  - expected tracked code paths may include only `mlf2.cabal`, `src/MLF/Research/URI/R2/C1/Prototype/P2.hs`, `src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`, `src/MLF/Research/URI/R2/C1/Prototype/Entrypoint.hs`, `src/MLF/Research/URI/R2/C1/Prototype/Types.hs`, and `test/Research/UriR2C1PrototypeP1Spec.hs`
  - expected: no `P1.hs`, no `orchestrator/rounds/round-017/state-snapshot.json`, no `orchestrator/roadmaps/2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap/rev-002/roadmap.md`, no `orchestrator/rounds/round-016/`, no `app/Main.hs`, no `src-public/`
- Confirm the repaired `P2-W` classification:
  - `python3 -m json.tool orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json >/dev/null`
  - `python3 -m json.tool orchestrator/rounds/round-017/evidence/P2/attempt-2/stage-verdict.json >/dev/null`
  - reviewer must verify replay-diagnostic failure no longer maps to `verdict: "pass"`
  - reviewer must verify `subject-token.json` is absent on non-pass and present only on true `pass`
- Confirm shared-entrypoint and scenario isolation still hold:
  - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P3-safety-validation --scenario-id uri-r2-c1-only-v1 --attempt-id 2`
  - expected: reject with `UnsupportedStageSelector "P3-safety-validation"`
  - `cabal run mlf2 -- --research-entrypoint uri-r2-c1-prototype-entrypoint-v1 --stage-selector P2-provenance-preservation --scenario-id wrong-scenario --attempt-id 2`
  - expected: reject without modifying `attempt-2`
- Confirm unchanged outer behavior:
  - `cabal run mlf2`

Reviewer output remains:

- `orchestrator/rounds/round-017/review.md` with baseline checks, repair checks, explicit approve or reject decision, and continuity evidence against the accepted `P1` input
- `orchestrator/rounds/round-017/review-record.json` only if the repaired round is approved
