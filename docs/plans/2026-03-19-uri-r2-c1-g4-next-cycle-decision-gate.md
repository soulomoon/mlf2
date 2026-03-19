# `G4` Next-Cycle Decision Gate For Repaired `URI-R2-C1`

Date: 2026-03-19
Round: `round-049`
Roadmap item: `G4`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only aggregate next-cycle decision

## Stage Contract Freeze

This artifact records only roadmap item `G4` for `attempt-1`.

`G4` is aggregate-only and docs-only. It records exactly one bounded next-step
result token for the already reverified repaired `URI-R2-C1` local-binding
`rootLocalMultiInst` / `targetC -> rootFinal` lane. It does not reopen `G1`
selection, does not reopen `G2` implementation, does not reopen `G3`
verification, does not amend the roadmap, and does not itself widen the live
subject or inherited boundary.

The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

Any contradiction discovered in the accepted `G3` evidence chain would be a
blocker to record through `stop-blocked`, not permission to patch code, add
tests, reopen replay, or update `Bugs.md` in this round.

Reviewer outcomes for this stage remain limited to:

- `accepted + finalize`
- `rejected + retry`

`accepted + retry` is forbidden for `G4`.

## Accepted `G1` / `G2` / `G3` Chain Carried Forward Without Reopening

Only accepted authoritative evidence is carried forward here:

1. `G1` finalized as the authoritative bind that froze exactly one remaining
   bounded `G2` target under repaired `URI-R2-C1`: the local-binding
   `rootHasMultiInst` `keepTargetFinal` / `targetC` lane, while leaving
   `instArgRootMultiBase` explicitly unselected.
2. `G2` finalized as the authoritative implementation of only that bounded
   lane, introducing the reviewer-auditable proof
   `rootLocalMultiInst = rootBindingIsLocalType && rootHasMultiInst`, with
   `targetC -> rootFinal` admitted only for that local lane, plus one local
   success example and one matched non-local fail-closed contrast.
3. `G3` finalized authoritatively on `attempt-1`, with
   `attempt_verdict = accepted`, `stage_action = finalize`, `status =
   authoritative`, and canonical artifact path
   `docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`.
4. The accepted `G3` verification chain already reconfirmed all bounded
   decision inputs required here:
   `G3-CONTRACT`, `G3-G1-G2-AUTHORITY`, `G3-ANCHORS`, `G3-FOCUSED-RERUN`,
   `G3-FULL-GATE`, `G3-DIFF-BOUNDARY`, and `G3-CONTINUITY` all remained `pass`.
5. Accepted negative findings remain binding:
   `U2 = authority-narrowed`,
   `U3 = uniqueness-owner-stable-refuted`,
   `U4 = constructor-acyclic-termination-refuted`.
   None of them may be reinterpreted here as widening clearance.
6. `/Volumes/src/mlf4/Bugs.md` remains continuity context only. The resolved
   replay-path bug does not authorize replay reopen, `MLF.Elab.Inst`,
   `InstBot`, `instArgRootMultiBase`, `boundVarTarget` widening, or non-local
   widening in this round.

The accepted bounded lane therefore remains:

- repaired `URI-R2-C1`;
- local-binding `rootLocalMultiInst` / `targetC -> rootFinal`;
- read-only ownership anchored to `src/MLF/Elab/Run/ResultType/Fallback.hs`
  and `test/PipelineSpec.hs`; and
- explicit exclusions on `instArgRootMultiBase`, `boundVarTarget`, non-local
  widening, replay reopen, `MLF.Elab.Inst`, `InstBot`, equi-recursive
  reasoning, cyclic structural encoding, second-interface work, and fallback
  conveniences.

## Decision

Result token: `continue-bounded`

## Why `continue-bounded` Is Lawful

`continue-bounded` is the only lawful token because the accepted `G3` evidence
chain still stands as the current bounded verification baseline for the exact
same repaired `URI-R2-C1` local `rootLocalMultiInst` / `targetC -> rootFinal`
lane:

- the authoritative `round-048` review record is present, internally
  consistent, and still points at the accepted `G3` artifact;
- every required accepted `G3-*` pass key remains present in the authoritative
  review record;
- docs-level continuity checks still point at the same bounded lane and the
  same exclusions; and
- no new blocker or contradiction was found in the accepted evidence chain.

This token preserves the accepted `Fallback.hs` / `PipelineSpec.hs` ownership
anchor, preserves the repaired `URI-R2-C1` live subject, and preserves the
explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface /
no-fallback boundary.

## Why The Other Tokens Are Not Lawful

### `stop-blocked`

`stop-blocked` is not lawful because no required accepted artifact is missing,
the authoritative `G3` review record is not contradictory, the accepted
decision inputs remain parseable and coherent, and no docs-level continuity
check failed.

### `widen-approved`

`widen-approved` is not lawful because there is no already-accepted roadmap or
boundary amendment that widens the live subject, clears the still-binding
`U2` / `U3` / `U4` negative findings as widening blockers, or authorizes
replay reopen, `instArgRootMultiBase`, `boundVarTarget`, non-local widening,
or any broader recursive-inference lane.

## Continuity Preservation

This decision keeps the accepted `G3` bounded lane intact as inherited context
only:

- `Fallback.hs` and `PipelineSpec.hs` remain frozen ownership anchors for the
  accepted local `rootLocalMultiInst` / `targetC -> rootFinal` lane;
- `instArgRootMultiBase` remains explicitly unselected and out of scope in the
  accepted lane;
- `boundVarTarget` widening and non-local widening remain out of scope;
- replay reopen, `MLF.Elab.Inst`, and `InstBot` remain out of scope; and
- no second executable interface, compatibility shim, convenience fallback, or
  default-path widening is authorized here.

Any future bounded cycle would require a new accepted roadmap update. That
future work may not treat the remaining unselected `instArgRootMultiBase`
family as already cleared by this decision.

## Docs-Only Verification Notes

Verification ran from the active round worktree while reading controller-owned
state and roadmap files from the repository root.

Baseline docs/state checks passed:

- `git diff --check`
- `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `test -f /Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `test -f /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`

Authoritative continuity checks passed:

- `test -f /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`
- `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-048/review-record.json >/dev/null`
- a short `python3` assertion confirmed:
  - `stage_id == "G3"`
  - `attempt == 1`
  - `attempt_verdict == "accepted"`
  - `stage_action == "finalize"`
  - `status == "authoritative"`
  - `artifact_path == "docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md"`
  - no required `G3-*` pass keys were missing or non-pass
- `rg -n 'rootLocalMultiInst|targetC -> rootFinal|instArgRootMultiBase|boundVarTarget|non-local' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`

Docs-only diff checks passed:

- `git status --short --untracked-files=all` showed only
  `orchestrator/rounds/round-049/plan.md` and
  `orchestrator/rounds/round-049/selection.md` before authoring this round's
  docs;
- `git diff --name-only` showed no tracked code-path diff before authoring the
  `G4` docs; and
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  produced no output.

Per the accepted `G3` evidence contract, `G4` did not rerun the focused
`ARI-C1` block or `cabal build all && cabal test`. The accepted `G3` artifact
already supplies the fresh bounded verification baseline for this exact lane,
and `G4` authorizes no production or test edits.

## Non-Authorization

This round does not itself authorize:

- reopening `G1`, `G2`, or `G3`;
- replay reopen;
- `MLF.Elab.Inst` or `InstBot` work;
- `instArgRootMultiBase` as an accepted live lane;
- `boundVarTarget` widening;
- non-local widening;
- equi-recursive reasoning;
- cyclic structural graph encoding;
- a second executable interface; or
- compatibility, convenience, or default-path widening.
