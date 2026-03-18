# `C1` Continue-Bounded Target Bind And Exact `C2` Selection

Date: 2026-03-18
Round: `round-034`
Roadmap item: `C1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bind/selection freeze

## Stage Contract Freeze

This artifact implements only roadmap item `C1` for `attempt-1` with `retry: null`.

It binds the refreshed bounded cycle to repaired `URI-R2-C1` under the accepted `U6` result `continue-bounded`.
It does not perform production edits, test edits, roadmap mutation, or bug-tracker updates.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

`C1` is a bind/selection stage only. It does not clear authority, uniqueness, owner stability, or constructor admissibility, and it does not widen broad automatic recursive inference beyond the repaired live subject.

## Accepted Carry-Forward Evidence

Only accepted bounded predecessor facts are carried forward here:

1. `U5` landed one bounded non-widening result-type hardening slice in `src/MLF/Elab/Run/ResultType/Fallback.hs` with focused coverage in `test/PipelineSpec.hs`, and exposed `rootBindingIsLocalType` as reviewer-auditable evidence rather than a new recursive success path.
2. `U6` finalized the aggregate result token `continue-bounded`, not `widen-approved`, so only one more bounded non-widening cycle is authorized.
3. `U2` remains `authority-narrowed`; provenance-stable unannotated authority is still not cleared.
4. `U3` remains `uniqueness-owner-stable-refuted`; exactly one admissible local owner/root cluster is still not established.
5. `U4` remains `constructor-acyclic-termination-refuted`; constructor-directed admissibility is still fail-closed under the inherited boundary.

`Bugs.md` and open replay-path bug `BUG-2026-03-16-001` remain repaired-lane continuity context only in this round. They do not authorize replay-lane reopen, solver widening, or selection of `MLF.Elab.Inst.applyInstantiation` / `InstBot` as the next slice.

## Cycle Bind

The new bounded cycle stays fixed to repaired `URI-R2-C1` only.

The controlling carry-forward decision from `U6` is:

- continue bounded work without widening scope;
- preserve the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary;
- keep accepted `U2` / `U3` / `U4` negative findings binding on later work.

No later `C2` work may reinterpret those negative findings as if they were already cleared unless a separate accepted roadmap update first changes the live plan.

## Selected `C2` Target (Exactly One)

The only frozen future `C2` target is:

local-binding-only result-type target-retention hardening using the existing `rootBindingIsLocalType` signal, so the retained recursive-looking target stays fail-closed unless it still resolves to a local `TypeRef` on repaired `URI-R2-C1`.

This target remains bounded to the already-landed result-type lane only:

- future production ownership is frozen to `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`;
- future focused test ownership is frozen to `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`.

Required fail-closed interpretation for that future slice:

- recursive-looking wrapper or proxy retention stays rejected unless the retained target remains rooted in a local `TypeRef`;
- wrapper/proxy retention is not evidence that authority, uniqueness, owner stability, or constructor feasibility has been cleared;
- no second `C2` family is selected by this artifact.

## Non-Authorization Statement

This `C1` bind does not authorize:

- edits to `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`;
- replay-repair reopen, including `MLF.Elab.Inst.applyInstantiation` / `InstBot`, as active `C2` work;
- prototype or research entrypoints;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural graph encoding;
- multi-SCC or cross-family widening;
- heuristic ownership ranking or manufactured authority;
- second executable interfaces;
- compatibility shims, convenience fallbacks, or default-path widening;
- roadmap, state, or bug-tracker edits as part of this bind.

`C1` therefore freezes one bounded future slice only. Any broader target family requires a separate accepted roadmap change before implementation.

## Retry-Aware Reviewer Handoff

This artifact is the immutable `attempt-1` bind/selection record for `C1`.
If `round-034` retries later, future attempts must be additive and must not rewrite this artifact.

Per `orchestrator/retry-subloop.md`, reviewer outcomes supported by this artifact remain:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`

Reviewer record fields expected for this round remain:

- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-034`

### Baseline Commands

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json` -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `34:1. [pending] Execute the `C1` continue-bounded bind and exact next-slice target selection for the still-bound live subject`
  - `38:2. [pending] Execute the `C2` bounded fail-closed solver/pipeline implementation slice for the target selected in `C1``
  - `42:3. [pending] Execute the `C3` bounded verification and evidence consolidation gate for the selected slice`
  - `46:4. [pending] Execute the `C4` next-cycle decision gate`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md` -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### `C1`-Specific Checks

- `awk 'NR<104 {print}' docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md | rg -n 'Attempt: `attempt-1`|Retry state: `null`|Live subject: repaired `URI-R2-C1`|explicit-only recursive baseline|non-equi-recursive semantics|non-cyclic structural graph encoding'` -> pass:
  - `7:Attempt: `attempt-1``
  - `8:Retry state: `null``
  - `9:Live subject: repaired `URI-R2-C1``
  - `21:- explicit-only recursive baseline;`
  - `22:- non-equi-recursive semantics;`
  - `23:- non-cyclic structural graph encoding.`
- `awk 'NR<104 {print}' docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md | rg -n 'The only frozen future `C2` target is:|rootBindingIsLocalType|/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs|/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs|no second `C2` family is selected'` -> pass:
  - `53:The only frozen future `C2` target is:`
  - `55:local-binding-only result-type target-retention hardening using the existing `rootBindingIsLocalType` signal, so the retained recursive-looking target stays fail-closed unless it still resolves to a local `TypeRef` on repaired `URI-R2-C1`.`
  - `59:- future production ownership is frozen to `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`;`
  - `60:- future focused test ownership is frozen to `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`.`
  - `66:- no second `C2` family is selected by this artifact.`
- `awk 'NR<104 {print}' docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md | rg -n '`authority-narrowed`|`uniqueness-owner-stable-refuted`|`constructor-acyclic-termination-refuted`|still not cleared|still not established|still fail-closed|does not clear authority, uniqueness, owner stability, or constructor admissibility'` -> pass:
  - `25:`C1` is a bind/selection stage only. It does not clear authority, uniqueness, owner stability, or constructor admissibility, and it does not widen broad automatic recursive inference beyond the repaired live subject.`
  - `33:3. `U2` remains `authority-narrowed`; provenance-stable unannotated authority is still not cleared.`
  - `34:4. `U3` remains `uniqueness-owner-stable-refuted`; exactly one admissible local owner/root cluster is still not established.`
  - `35:5. `U4` remains `constructor-acyclic-termination-refuted`; constructor-directed admissibility is still fail-closed under the inherited boundary.`
- `awk 'NR<104 {print}' docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md | rg -n 'MLF\.Elab\.Inst\.applyInstantiation|InstBot|equi-recursive reasoning|implicit unfolding|cyclic structural graph encoding|multi-SCC|cross-family widening|heuristic ownership ranking|second executable interfaces|compatibility shims|default-path widening|roadmap, state, or bug-tracker edits'` -> pass:
  - `37:`Bugs.md` and open replay-path bug `BUG-2026-03-16-001` remain repaired-lane continuity context only in this round. They do not authorize replay-lane reopen, solver widening, or selection of `MLF.Elab.Inst.applyInstantiation` / `InstBot` as the next slice.`
  - `73:- replay-repair reopen, including `MLF.Elab.Inst.applyInstantiation` / `InstBot`, as active `C2` work;`
  - `75:- equi-recursive reasoning or implicit unfolding;`
  - `76:- cyclic structural graph encoding;`
  - `77:- multi-SCC or cross-family widening;`
  - `78:- heuristic ownership ranking or manufactured authority;`
  - `79:- second executable interfaces;`
  - `80:- compatibility shims, convenience fallbacks, or default-path widening;`
  - `81:- roadmap, state, or bug-tracker edits as part of this bind.`

### Docs-Only Diff Evidence

- `git status --short --untracked-files=all` -> docs/orchestrator-only artifact set:
  - `?? docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`
  - `?? orchestrator/rounds/round-034/implementation-notes.md`
  - `?? orchestrator/rounds/round-034/plan.md`
  - `?? orchestrator/rounds/round-034/selection.md`

### Full-Gate Note

`cabal build all && cabal test` was intentionally not run because this round is docs-only and does not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
