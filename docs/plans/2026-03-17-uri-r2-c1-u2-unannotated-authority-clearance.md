# `U2` Provenance-Stable Unannotated Authority Clearance

Date: 2026-03-18
Round: `round-029`
Roadmap item: `U2`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only authority analysis (bounded evidence)

## Stage Contract Freeze

This artifact implements `U2` only for `attempt-1` with `retry: null`.

Scope is limited to authority-clearance evidence for repaired `URI-R2-C1` and does not authorize solver/pipeline implementation.

The inherited successor boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

No widening is authorized to multi-SCC, cross-family search, broad automatic recursive inference, second interfaces, fallback paths, or compatibility shims.

## Controlling Provenance Chain (Repaired Lane Only)

1. `R5` (`docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`) finalized `URI-R2-C1` as `research-stop` and left `URI-R3-O4` unresolved as the decisive authority blocker: no provenance-stable unannotated root/cluster authority was established without manufactured authority or late repair.
2. `R4` (`docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`) finalized `repair-accepted` for the bounded replay-lane defect at `applyInstantiation` / `InstBot`, restoring repaired-lane continuity for `URI-R2-C1` without widening scope.
3. `U1` (`docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`, finalized in `round-028` review record) bound the successor cycle to repaired `URI-R2-C1` only and restated the explicit-only / non-equi-recursive / non-cyclic-graph boundary.

Interpretation: repaired replay continuity exists, but repaired continuity alone is not equivalent to provenance-stable unannotated authority clearance.

## Reviewer-Auditable `U2` Decision Contract

`U2` must end with exactly one bounded result token:

- `authority-cleared`, or
- `authority-narrowed`.

For either result, evidence must show that no authority is manufactured by:

- fallback behavior;
- compatibility shims;
- heuristic ranking between competing roots/clusters; or
- late repair outside accepted provenance.

## Final `U2` Result

Result token: `authority-narrowed`

Precise remaining blocker for repaired `URI-R2-C1`:

- the repaired lane now has accepted replay continuity (`R4`), but this evidence still does not prove one provenance-stable unannotated root/cluster authority that survives generalization, reconstruction, reification, and witness replay without fallback, shimmed compatibility, heuristic root choice, or post-hoc repair.

Why this is a narrowing (not a reopen/widen):

- `R4` removes one localized replay defect and prevents treating that defect as the active blocker.
- The unresolved `R5` `URI-R3-O4` authority gap remains, now narrowed to a single bounded evidence requirement on the repaired live subject.
- No new broad subject is introduced; all conclusions stay inside repaired `URI-R2-C1`.

## Carry-Forward Implications For `U3` (Non-Preemptive)

- `U3` remains pending and must evaluate uniqueness/owner-stability only within the same repaired `URI-R2-C1` subject and inherited boundary.
- This `U2` artifact does not pre-clear `U3`, `U4`, `U5`, or `U6`.
- Any future attempt evidence must remain additive and preserve this `attempt-1` artifact unchanged.

## Bounded Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-029`

### Baseline Commands

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json` -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `29:1. [done] Execute the `U1` inherited baseline and repaired-subject bind for unannotated iso-recursive inference`
  - `33:2. [pending] Execute the `U2` provenance-stable unannotated authority clearance for the live subject`
  - `37:3. [pending] Execute the `U3` uniqueness and owner-stability clearance for the live subject`
  - `41:4. [pending] Execute the `U4` constructor-directed / acyclicity / termination clearance for the live subject`
  - `45:5. [pending] Execute the `U5` bounded solver/pipeline implementation slice for the cleared live subject`
  - `49:6. [pending] Execute the `U6` end-to-end verification and next-widening decision gate`
- `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md` -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### `U2`-Specific Boundary Checks

- `rg -n 'Attempt: `attempt-1`|Retry state: `null`|Live subject: repaired `URI-R2-C1`' docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md` -> pass:
  - `7:Attempt: `attempt-1``
  - `8:Retry state: `null``
  - `9:Live subject: repaired `URI-R2-C1``
- `rg -n 'explicit-only|non-equi-recursive|non-cyclic structural graph|non-cyclic-graph' docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md` -> pass:
  - `20:- explicit-only recursive baseline;`
  - `21:- non-equi-recursive semantics;`
  - `22:- non-cyclic structural graph encoding.`
  - `30:... explicit-only / non-equi-recursive / non-cyclic-graph boundary.`
- `rg -n 'Result token: `authority-narrowed`|fallback|compatibility shims|heuristic ranking|late repair' docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md` -> pass:
  - `24:... fallback paths, or compatibility shims.`
  - `28:... without manufactured authority or late repair.`
  - `43:- fallback behavior;`
  - `44:- compatibility shims;`
  - `45:- heuristic ranking between competing roots/clusters; or`
  - `46:- late repair outside accepted provenance.`
  - `50:Result token: `authority-narrowed``
- `rg -n 'Carry-Forward Implications For `U3`|does not pre-clear `U3`, `U4`, `U5`, or `U6`' docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md` -> pass:
  - `62:## Carry-Forward Implications For `U3` (Non-Preemptive)`
  - `65:- This `U2` artifact does not pre-clear `U3`, `U4`, `U5`, or `U6`.`

### Full-Gate Note

`cabal build all && cabal test` was intentionally not run because this round is docs-only and does not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
