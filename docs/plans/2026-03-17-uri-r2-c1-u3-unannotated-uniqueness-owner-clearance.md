# `U3` Uniqueness And Owner-Stability Clearance

Date: 2026-03-18
Round: `round-030`
Roadmap item: `U3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only uniqueness/owner analysis (bounded evidence)

## Stage Contract Freeze

This artifact implements only roadmap item `U3` for `attempt-1` with `retry: null`.

Scope is strictly bounded to uniqueness/owner-stability evidence for repaired `URI-R2-C1`.
It does not authorize solver/pipeline implementation and does not pre-clear `U4` through `U6`.

Inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

No widening is authorized to multi-SCC search, cross-family search, broad automatic recursive inference, second interfaces, compatibility shims, or fallback/default-path behavior.

## Inherited Evidence Inputs (`U1` + `U2` only)

Accepted predecessor evidence carried forward without reinterpretation:

1. `U1` (`docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`, `round-028` review-record `accepted + finalize`) bound the live subject to repaired `URI-R2-C1` and fixed explicit-only / non-equi-recursive / non-cyclic-graph boundaries.
2. `U2` (`docs/plans/2026-03-17-uri-r2-c1-u2-unannotated-authority-clearance.md`, `round-029` review-record `accepted + finalize`) produced result token `authority-narrowed`: repaired-lane continuity exists, but one provenance-stable unannotated local root/cluster authority is still not proven without fallback/shim/heuristic/late-repair authority manufacturing.

`U3` consumes only those bounded facts to evaluate uniqueness and owner stability for the same live subject.

## `U3` Admissibility And Decision Rubric (Bounded, Non-Heuristic)

Define the local candidate set `C` for repaired `URI-R2-C1` as roots/clusters that satisfy all criteria below:

1. Provenance criterion: candidate authority is evidenced on the repaired lane and is not manufactured by fallback, compatibility shim, heuristic choice, or post-hoc repair.
2. Subject criterion: candidate ownership is local to repaired `URI-R2-C1` only (no cross-family/multi-SCC widening).
3. Boundary criterion: candidate reasoning stays explicit-only, non-equi-recursive, and non-cyclic-graph.
4. Stability criterion: ownership remains stable under the accepted replay/reconstruction chain referenced by `U2`.

Decision rule:

- `uniqueness-owner-stable-cleared` iff `|C| = 1`.
- `uniqueness-owner-stable-refuted` iff `|C| /= 1`.

No-ranking rule (mandatory):

- If multiple competing candidates remain, the result is refuted.
- If no admissible candidate is proven, the result is refuted.
- Heuristic ranking among candidates is forbidden.

## `U3` Analysis For Repaired `URI-R2-C1`

Applying the rubric to inherited `U2` authority evidence:

1. `U2` finalized `authority-narrowed`, explicitly stating that one provenance-stable unannotated local root/cluster authority is still not proven on the repaired lane.
2. Therefore criterion 1 (provenance) is not satisfied for any candidate currently evidenced in `U3` scope.
3. With criterion 1 unsatisfied, the admissible set is `C = ∅`; thus `|C| = 0`, not exactly one.
4. Owner stability for exactly one admissible candidate cannot be established under bounded evidence; no heuristic ranking is introduced.

Explicit uniqueness/owner statement:

- Exactly one admissible local root/cluster with stable ownership and no heuristic ranking is **not established** for repaired `URI-R2-C1` in `U3` attempt-1.

## Final `U3` Result

Result token: `uniqueness-owner-stable-refuted`

Rationale (bounded):

- The inherited `U2` accepted result narrows authority but does not prove one admissible provenance-stable local root/cluster.
- Under the explicit no-ranking rule, absence of exactly one admissible candidate requires refutation rather than heuristic selection.
- The round stays fail-closed and does not widen scope or semantics.

## Carry-Forward Implications For `U4` Only (Non-Preemptive)

- `U4` remains the next pending item and must stay on repaired `URI-R2-C1` under the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.
- `U4` must not assume `U3` uniqueness/owner clearance and must not introduce heuristic ranking or subject widening to compensate for this `U3` result.
- This artifact does not pre-clear or pre-decide `U4` through `U6`; it records only the bounded `U3` outcome.

## Bounded Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-030`

### Baseline Commands

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json` -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `29:1. [done] Execute the `U1` inherited baseline and repaired-subject bind for unannotated iso-recursive inference`
  - `33:2. [done] Execute the `U2` provenance-stable unannotated authority clearance for the live subject`
  - `37:3. [pending] Execute the `U3` uniqueness and owner-stability clearance for the live subject`
  - `41:4. [pending] Execute the `U4` constructor-directed / acyclicity / termination clearance for the live subject`
  - `45:5. [pending] Execute the `U5` bounded solver/pipeline implementation slice for the cleared live subject`
  - `49:6. [pending] Execute the `U6` end-to-end verification and next-widening decision gate`
- `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md` -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### `U3`-Specific Checks

- `awk 'NR<=100 {print}' docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md | rg -n 'Attempt: `attempt-1`|Retry state: `null`|Live subject: repaired `URI-R2-C1`'` -> pass:
  - `7:Attempt: `attempt-1``
  - `8:Retry state: `null``
  - `9:Live subject: repaired `URI-R2-C1``
- `awk 'NR<=100 {print}' docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md | rg -n 'explicit-only|non-equi-recursive|non-cyclic structural graph|non-cyclic-graph'` -> pass:
  - `21:- explicit-only recursive baseline;`
  - `22:- non-equi-recursive semantics;`
  - `23:- non-cyclic structural graph encoding.`
  - `31:1. `U1` (`docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`, `round-028` review-record `accepted + finalize`) bound the live subject to repaired `URI-R2-C1` and fixed explicit-only / non-equi-recursive / non-cyclic-graph boundaries.`
  - `42:3. Boundary criterion: candidate reasoning stays explicit-only, non-equi-recursive, and non-cyclic-graph.`
  - `81:- `U4` remains the next pending item and must stay on repaired `URI-R2-C1` under the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.`
- `awk 'NR<=100 {print}' docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md | rg -n 'No-ranking rule|Heuristic ranking among candidates is forbidden|Exactly one admissible local root/cluster with stable ownership and no heuristic ranking is \*\*not established\*\*|Result token: `uniqueness-owner-stable-refuted`'` -> pass:
  - `50:No-ranking rule (mandatory):`
  - `54:- Heuristic ranking among candidates is forbidden.`
  - `67:- Exactly one admissible local root/cluster with stable ownership and no heuristic ranking is **not established** for repaired `URI-R2-C1` in `U3` attempt-1.`
  - `71:Result token: `uniqueness-owner-stable-refuted``
- `awk 'NR<=100 {print}' docs/plans/2026-03-17-uri-r2-c1-u3-unannotated-uniqueness-owner-clearance.md | rg -n 'No widening is authorized|does not pre-clear|does not pre-clear or pre-decide|fallback/default-path behavior|compatibility shims'` -> pass:
  - `17:It does not authorize solver/pipeline implementation and does not pre-clear `U4` through `U6`.`
  - `25:No widening is authorized to multi-SCC search, cross-family search, broad automatic recursive inference, second interfaces, compatibility shims, or fallback/default-path behavior.`
  - `83:- This artifact does not pre-clear or pre-decide `U4` through `U6`; it records only the bounded `U3` outcome.`

### Full-Gate Note

`cabal build all && cabal test` was intentionally not run because this round remains docs-only and does not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
