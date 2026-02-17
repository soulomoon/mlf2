# Findings: BUG-2026-02-16-009 OpRaise Non-Spine Context

## Investigation Log
- Initialized. Evidence pending.

## Reproduction (Phase 1)
- Deterministic repro confirmed with seed `1481579064`.
- Failure originates in non-spine `OpRaise` translation path in `MLF.Elab.Phi.Omega`.
- Error payload already includes computed context ingredients (`target=6`, `orderRoot=2`, `deps=[]`, `idsSynced=[Just 0]`) but still fails on `missing computation context`.
- This indicates context reconstruction produced `Nothing` for the insertion context in a case where the test expects a valid round-trip.

## Root Cause Evidence (Phase 1/2)
- Presolution witness for the failing expression includes:
  - `OpGraft 11 0 ; OpRaise 0 ; OpRaise 1`
  - `etRoot = 2`, `etInterior = [0,1,2]`, `etCopyMap = {0->12, 1->6, 2->6}`.
- During `OpRaise` translation in `Omega`, the source target is unconditionally adopted through `etCopyMap`:
  - `nSource = 1` becomes `nAdopted = 6`.
- Context probes show a split outcome:
  - `contextToNodeBound solved 2 1` => `Just [StepInside]` (valid non-spine context exists for source target).
  - `contextToNodeBound solved 2 6` => `Nothing` (no context for adopted target).
- The failing error payload (`missing computation context`) is therefore caused by adopting a copy-map target that is not context-reachable from the edge root in the non-spine algorithm.

## Pattern Comparison
- The same copy-map shape (`OpRaise 1` with `1 -> copied`) appears in BUG-004 paths, but those cases do not necessarily fail because they can take different branches (e.g., spine handling or successful context route).
- The failing BUG-009 path is specifically non-spine context reconstruction where context existence is the gate condition.

## Working Hypothesis
- Root cause: unconditional copy-map adoption in `OpRaise` selects a semantically equivalent node that may be outside the valid non-spine computation context domain.
- Minimal fix direction: preserve adopted target as primary, but when non-spine context computation fails on adopted target, retry context reconstruction using the source-domain target for context-bearing decisions.

## Fix Applied
- File: `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
- Change:
  - Kept adopted-target (`etCopyMap`) `OpRaise` handling as primary.
  - Added source-domain fallback for non-spine root-context insertion when the adopted-target path cannot produce candidate/root context.
  - Fallback recomputes source-target dependency index and root insertion instantiation.
- Why this is root-cause-level:
  - Failure was not due to missing witness/trace data; it was due to selecting an adopted target outside valid context reachability for non-spine reconstruction.
  - The fallback restores context reconstruction using the source-domain node where `C^r_n` exists.

## Verification Results
- Target bug repro:
  - PASS `--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Explicit forall annotation edge cases/explicit forall annotation round-trips on let-bound variables/" --seed 1481579064`
- Regression anchors:
  - PASS `--match "BUG-004" --seed 1481579064`
  - PASS `--match "BUG-002-V4" --seed 1481579064`
  - PASS `--match "BUG-2026-02-06-002 strict target matrix" --seed 1481579064`
  - PASS `--match "Explicit forall annotation edge cases" --seed 1481579064`
  - PASS `--match "contextToNodeBound does not descend through forall body fallback" --seed 1481579064`
- Full validation gate:
  - `cabal build all && cabal test` => FAIL (`672 examples, 4 failures`), all in pre-existing open buckets:
    - `BUG-2026-02-16-001`
    - `BUG-2026-02-16-002`
    - `BUG-2026-02-16-007`
    - `BUG-2026-02-16-008`
