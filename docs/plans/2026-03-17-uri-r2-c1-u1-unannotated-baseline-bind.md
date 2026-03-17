# `U1` Inherited Baseline And Repaired-Subject Bind

Date: 2026-03-18
Round: `round-028`
Roadmap item: `U1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: bind-only docs evidence (no implementation authority)

## Stage Contract Freeze

This artifact is `U1` `attempt-1` with `retry: null` and is bounded to a docs/evidence bind only.

Fixed inherited boundary carried forward without change:

- explicit-only baseline for recursive admission;
- non-equi-recursive semantics (no implicit unfolding/equality success path);
- non-cyclic structural graph encoding.

`U1` does not authorize production implementation work, does not clear `U2` through `U6`, and does not provide an implementation or widening verdict.

## Inherited Baseline Restatement (Authoritative Carry-Forward)

Continuity restatement from inherited accepted evidence:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`:
  explicit `TyMu` path is accepted; automatic recursive inference remains unresolved and disabled.
- `docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`:
  prior `ARI-C1` handoff was bounded and non-widening; it does not authorize broad unannotated recursive inference.
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`:
  prior unannotated `URI-R2-C1` chain finalized as research-stop because unannotated authority/uniqueness/feasibility obligations were not cleared.
- `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`:
  replay repair-track outcome finalized as `repair-accepted`, creating the repaired lane used as the successor starting subject.

Bounded continuity facts for this successor `U1` bind:

1. Explicit recursive path is implemented and accepted.
2. Broad automatic recursive inference remains unresolved and disabled.
3. Repaired `URI-R2-C1` is the only currently bound live successor subject.

`Bugs.md` is treated as continuity context only in this round; no bug-tracker edits are part of `U1`.

## Live-Subject Bind (Repaired Lane Only)

The active live subject for this successor cycle is explicitly bound to repaired `URI-R2-C1` only.

No widening is authorized from this bind to:

- broad automatic recursive inference;
- multi-SCC search;
- cross-family ownership search;
- any other unannotated recursive subject outside repaired `URI-R2-C1`.

## Reviewer-Visible Hard Stop Triggers

The following hard stops remain mandatory. If any trigger is violated, reviewer action must fail closed and keep the stage bounded/retryable rather than widen:

1. No equi-recursive equality and no implicit unfolding success path.
2. No cyclic structural graph encoding for recursion.
3. No silent widening to multi-SCC, cross-family, or broad search-wide automatic recursive inference.
4. No second executable interface for research/repair behavior.
5. No compatibility shim, convenience fallback, or default-on widening path.
6. No preemption of `U2`, `U3`, `U4`, `U5`, or `U6`.

## Retry-Aware Reviewer Handoff For `U1`

Per `orchestrator/retry-subloop.md`, reviewer records for this `U1` attempt must include:

- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`

Allowed decision combinations for `U1`:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`

This artifact is additive attempt evidence only and preserves retry immutability rules for any later `U1` attempt.

## Bounded Verification Notes

Commands executed in: `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-028`

### Baseline Commands

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json` -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `28:1. [pending] Execute the U1 inherited baseline and repaired-subject bind for unannotated iso-recursive inference`
  - `32:2. [pending] Execute the U2 provenance-stable unannotated authority clearance for the live subject`
  - `36:3. [pending] Execute the U3 uniqueness and owner-stability clearance for the live subject`
  - `40:4. [pending] Execute the U4 constructor-directed / acyclicity / termination clearance for the live subject`
  - `44:5. [pending] Execute the U5 bounded solver/pipeline implementation slice for the cleared live subject`
  - `48:6. [pending] Execute the U6 end-to-end verification and next-widening decision gate`
- `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md` -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### `U1`-Specific Verification

- `rg -n 'Live subject: repaired \`URI-R2-C1\`|Live-Subject Bind \(Repaired Lane Only\)|bound to repaired \`URI-R2-C1\` only' docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md` -> pass:
  - `9:Live subject: repaired \`URI-R2-C1\``
  - `45:## Live-Subject Bind (Repaired Lane Only)`
  - `47:The active live subject for this successor cycle is explicitly bound to repaired \`URI-R2-C1\` only.`
- `rg -n 'explicit-only|non-equi-recursive|non-cyclic structural graph|non-cyclic-graph' docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md` -> pass:
  - `18:- explicit-only baseline for recursive admission;`
  - `19:- non-equi-recursive semantics (no implicit unfolding/equality success path);`
  - `20:- non-cyclic structural graph encoding.`
  - `102:- Inherited boundary \`explicit-only / non-equi-recursive / non-cyclic-graph\` is restated and preserved.`
- `rg -n 'Hard Stop Triggers|No equi-recursive equality|No cyclic structural graph encoding|No silent widening|No second executable interface|No compatibility shim|No preemption of \`U2\`' docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md` -> pass:
  - `56:## Reviewer-Visible Hard Stop Triggers`
  - `60:1. No equi-recursive equality and no implicit unfolding success path.`
  - `61:2. No cyclic structural graph encoding for recursion.`
  - `62:3. No silent widening to multi-SCC, cross-family, or broad search-wide automatic recursive inference.`
  - `63:4. No second executable interface for research/repair behavior.`
  - `64:5. No compatibility shim, convenience fallback, or default-on widening path.`
  - `65:6. No preemption of \`U2\`, \`U3\`, \`U4\`, \`U5\`, or \`U6\`.`

### Full-Gate Note

`cabal build all && cabal test` was intentionally not run because this round is docs-only (`docs/` + round artifact notes) and does not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
