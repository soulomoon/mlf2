# Task Plan: BUG-002 Replay-Bridge Undercoverage

## Objective
Identify and fix the root cause behind BUG-002 strict-matrix replay bridge failures (`PhiInvariantError` trace/replay binder key-space mismatch), while preserving thesis-strict guardrails.

## Scope
- Apply `systematic-debugging` end-to-end (root cause before fix).
- Use deterministic BUG-002 reproducer from `Bugs.md` (`BUG-2026-02-16-010`).
- Keep Φ/Ω fail-fast contracts strict; no permissive fallback masking.

## Phases
1. Root cause investigation: reproduce failure, collect concrete trace/replay evidence. (in progress)
2. Pattern analysis: compare failing BUG-002 path to adjacent passing strict-matrix variants. (pending)
3. Hypothesis + minimal validation test. (pending)
4. Root-cause implementation + regression coverage. (pending)
5. Verification + tracker/docs sync (`Bugs.md`, task files, notes). (pending)

## Decisions
- Track evidence and hypotheses in this task folder before implementation changes.
- Do not patch blindly; each hypothesis must be validated with smallest change and explicit result.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## 2026-02-17 status update
- Phase 1 root-cause investigation: completed.
- Phase 2 pattern analysis: completed.
- Phase 3 hypothesis testing: completed (4 minimal hypotheses tested, all reverted).
- Phase 4 implementation: blocked pending architectural decision (see errors + findings).

### Additional errors encountered
| Error | Attempt | Resolution |
|---|---:|---|
| BUG-002 matrix undercoverage (`trace/replay binder key-space mismatch` on `OpRaise`) persists after bridge candidate tweaks | 4 | Reverted each hypothesis; kept causal evidence in findings/progress. |
| Hypothesis variants shifted failure class to `ValidationFailed ["alias bounds survived scheme finalization: [\"a\"]"]` | 1 | Reverted; indicates key-space fix attempt introduced downstream scheme-finalization drift. |
| Hypothesis variants regressed to `OpRaise: binder spine / identity list length mismatch` | 1 | Reverted; indicates key-domain consistency cannot be patched locally without broader bridge/identity model alignment. |

### Architecture concern (per systematic-debugging phase 4.5)
- 3+ focused fixes failed without converging on green BUG-002 matrix.
- Evidence indicates a cross-module architectural mismatch between:
  - presolution trace/hint identity domains,
  - replay scheme substitution key-space,
  - and Ω binder lookup expectations.
- Next change should be a deliberate architecture-level normalization of replay key-space contracts before additional local fixes.

## 2026-02-17 follow-up (contract implementation pass)
- Phase 4 implementation: in progress (contract changes landed in Φ bridge + Ω lookup + witness normalization).
- Phase 5 verification: in progress.

### Contract changes implemented
- Bridge construction (`MLF.Elab.Phi.Translate`): replay candidates are now constrained to replay-binder domain derived from `siReplay` TyVar binders; non-binder replay keys are no longer considered valid bridge targets.
- Ω lookup (`MLF.Elab.Phi.Omega`): binder-target ops (`OpGraft`, `OpWeaken`, `OpMerge`, `OpRaiseMerge`) now resolve through binder-constrained normalized replay candidates before fallback paths.
- Witness normalization (`MLF.Constraint.Presolution.WitnessNorm`): restored `OpRaise` steps are normalized against source `etInterior`, and non-interior raises are dropped during restore.

### Verification status
- Deterministic BUG-002 slice (`--match "BUG-002-V" --seed 1593170056`): improved from 2 failures to 1 failure.
- Fixed:
  - `PipelineSpec BUG-002-V4` interior guard now passes.
  - `BUG-002-V2` no longer fails with `OpGraft+OpWeaken targets non-binder node`.
- Remaining:
  - `BUG-002-V2` still fails at scheme finalization: `ValidationFailed ["alias bounds survived scheme finalization: [\"a\"]"]`.

### Current assessment
- Replay-key normalization contract is now enforced across trace/hint bridge and Ω binder lookup.
- Residual V2 failure appears downstream in scheme finalization (alias-bound cleanup), not in replay key-space mismatch or non-binder replay selection.

## 2026-02-17 closure update (this session)
- Phase 4 implementation: completed for requested BUG-002 contract scope.
- Phase 5 verification: completed for BUG-002 deterministic matrix; full-gate remains red in this workspace due unrelated/open failures.

### Final root-cause and fix
- Root cause of remaining `BUG-002-V2` failure:
  - replay contract changes removed non-binder Ω failures, but `generalizeAt` could still materialize self-referential binder bounds in reified form (`∀(a ⩾ a)` or structured bounds mentioning the binder itself), causing either alias-bound validation failure or invalid `ETyAbs` bounds.
- Implemented normalization in `MLF.Constraint.Presolution.Plan.ReifyPlan`:
  - after alias substitution, self references to the current binder inside its bound are normalized to `⊥` (instead of keeping illegal self refs or dropping all structure).
  - tautological self-bounds collapse cleanly; structured bounds retain non-self structure (e.g. `b -> a` normalizes to `⊥ -> a` for binder `b`).

### Verification evidence
- PASS: `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V" --seed 1593170056'`
  - `BUG-002-V1..V4` all green.
- PASS: `cabal test mlf2-test --test-show-details=direct --test-options='--match "generalizeAt rejects alias bounds" --seed 1593170056'`
  - alias-bound strict rejection contract preserved.
- PASS: seed-pinned single-case repros from bug record:
  - `BUG-002-V2` green.
  - `BUG-002-V3` green.
- Residual cross-link (not in requested BUG-002 scope):
  - `BUG-004-V2` still red in this workspace with `PipelineTypeCheckError (TCArgumentMismatch ...)`.
- Full gate command status:
  - `cabal build all && cabal test` is red in this dirty workspace with multiple pre-existing/open failures (including planner and witness-normalization buckets).
