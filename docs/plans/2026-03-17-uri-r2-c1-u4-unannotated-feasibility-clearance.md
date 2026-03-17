# `U4` Constructor-Directed / Acyclicity / Termination Feasibility Clearance

Date: 2026-03-18
Round: `round-031`
Roadmap item: `U4`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only feasibility evidence (no implementation authority)

## Stage Contract Freeze

This artifact implements only roadmap item `U4` for `attempt-1` with `retry: null`.

Scope is strictly bounded to feasibility/clearance evidence for repaired `URI-R2-C1` and does not authorize solver/pipeline implementation work.

Inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics (no implicit unfolding/equi-recursive equality success path);
- non-cyclic structural graph encoding.

No widening is authorized to multi-SCC search, cross-family search, broad automatic recursive inference, second interfaces, compatibility shims, or fallback/default-path behavior.

## Inherited Carry-Forward Facts (`U1` / `U2` / `U3`)

Accepted predecessor evidence carried forward without rewrite:

1. `U1` (`round-028` review record: `accepted + finalize`) bound the successor lane to repaired `URI-R2-C1` under explicit-only / non-equi-recursive / non-cyclic-graph boundaries.
2. `U2` (`round-029` review record: `accepted + finalize`) finalized `authority-narrowed`, i.e. provenance-stable unannotated authority is still not fully cleared for the repaired live subject.
3. `U3` (`round-030` review record: `accepted + finalize`) finalized `uniqueness-owner-stable-refuted`, i.e. exactly one admissible local root/cluster with stable ownership was not established under bounded evidence.

These are inherited inputs only in `U4`; this round does not reopen or reinterpret `U1`/`U2`/`U3`.

## `U4` Admissibility Rubric (Bounded, Reviewer-Auditable)

`U4` asks whether the current solver discipline can admit repaired `URI-R2-C1` while preserving all mandatory constraints.

All of the following must hold for a cleared result:

1. Constructor-directed local reasoning only: no global search broadening beyond the bound subject.
2. Acyclic structural encoding preserved: no cyclic graph representation and no cycle exceptions.
3. Non-equi-recursive discipline preserved: no implicit unfolding and no equi-recursive equality/search.
4. Termination discipline preserved: no occurs-check weakening, no recursive chase broadening, no termination guard relaxation.
5. Ownership/scope discipline preserved: no widening beyond repaired `URI-R2-C1`, no cross-family/multi-SCC promotion, no heuristic fallback authority manufacture.

Result token contract (exactly one):

- `constructor-acyclic-termination-cleared`, or
- `constructor-acyclic-termination-refuted`.

## `U4` Bounded Analysis For Repaired `URI-R2-C1`

Applying the rubric to inherited accepted facts:

1. `U2` remains `authority-narrowed`, so provenance-stable unannotated authority is not yet fully cleared.
2. `U3` is `uniqueness-owner-stable-refuted`, so exactly one admissible local owner/root cluster is not established.
3. Under the explicit-only, non-equi-recursive, non-cyclic-graph, fail-closed discipline, constructor-directed admissibility cannot be cleared by adding fallback ranking, implicit unfolding, cyclic graph encoding, or termination weakening.
4. Therefore the current bounded evidence does not justify saying the live subject is presently admissible under constructor-directed + acyclic + terminating discipline.

## Final `U4` Result

Result token: `constructor-acyclic-termination-refuted`

Rationale (bounded):

- the repaired lane remains bound and continuity is preserved, but inherited authority/uniqueness evidence is insufficient for an admissible constructor-directed local derivation;
- mandatory boundaries forbid using equi-recursive shortcuts, cyclic graph encodings, fallback heuristics, or weakened termination to force a positive result;
- this keeps the campaign fail-closed and bounded to repaired `URI-R2-C1` only.

## Carry-Forward Implications For `U5` Only (Non-Preemptive)

- `U5` remains the next roadmap item, but no broad solver/pipeline widening is authorized from this `U4` outcome.
- Any `U5` action must remain explicitly bounded to inherited constraints and may not treat `U4` as a clearance for equi-recursive, cyclic, fallback, or cross-subject behavior.
- This artifact does not pre-decide `U6`; it records only the bounded `U4` feasibility result.

## Retry-Aware Reviewer Handoff (`U4`)

Per `orchestrator/retry-subloop.md`, reviewer records for this attempt must include:

- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`

Allowed decision combinations for `U4`:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`

This `attempt-1` artifact is immutable historical evidence for any later same-round retries.

## Bounded Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-031`

### Baseline Commands

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json` -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `29:1. [done] Execute the `U1` inherited baseline and repaired-subject bind for unannotated iso-recursive inference`
  - `33:2. [done] Execute the `U2` provenance-stable unannotated authority clearance for the live subject`
  - `37:3. [done] Execute the `U3` uniqueness and owner-stability clearance for the live subject`
  - `41:4. [pending] Execute the `U4` constructor-directed / acyclicity / termination clearance for the live subject`
  - `45:5. [pending] Execute the `U5` bounded solver/pipeline implementation slice for the cleared live subject`
  - `49:6. [pending] Execute the `U6` end-to-end verification and next-widening decision gate`
- `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md` -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### `U4`-Specific Boundary Checks

- `awk 'NR<=100 {print}' docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md | rg -n 'Attempt: `attempt-1`|Retry state: `null`|Live subject: repaired `URI-R2-C1`'` -> pass:
  - `7:Attempt: `attempt-1``
  - `8:Retry state: `null``
  - `9:Live subject: repaired `URI-R2-C1``
- `awk 'NR<=100 {print}' docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md | rg -n 'explicit-only|non-equi-recursive|non-cyclic structural graph|non-cyclic-graph'` -> pass:
  - `20:- explicit-only recursive baseline;`
  - `21:- non-equi-recursive semantics (no implicit unfolding/equi-recursive equality success path);`
  - `22:- non-cyclic structural graph encoding.`
  - `30:1. `U1` (`round-028` review record: `accepted + finalize`) bound the successor lane to repaired `URI-R2-C1` under explicit-only / non-equi-recursive / non-cyclic-graph boundaries.`
- `awk 'NR<=100 {print}' docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md | rg -n 'Result token: `constructor-acyclic-termination-refuted`|constructor-acyclic-termination-cleared|constructor-acyclic-termination-refuted|implicit unfolding|equi-recursive equality|cyclic graph|Termination discipline|fallback/default-path|second interfaces'` -> pass:
  - `43:2. Acyclic structural encoding preserved: no cyclic graph representation and no cycle exceptions.`
  - `44:3. Non-equi-recursive discipline preserved: no implicit unfolding and no equi-recursive equality/search.`
  - `45:4. Termination discipline preserved: no occurs-check weakening, no recursive chase broadening, no termination guard relaxation.`
  - `50:- `constructor-acyclic-termination-cleared`, or`
  - `51:- `constructor-acyclic-termination-refuted`.`
  - `64:Result token: `constructor-acyclic-termination-refuted``
  - `69:- mandatory boundaries forbid using equi-recursive shortcuts, cyclic graph encodings, fallback heuristics, or weakened termination to force a positive result;`
- `awk 'NR<=100 {print}' docs/plans/2026-03-17-uri-r2-c1-u4-unannotated-feasibility-clearance.md | rg -n 'Carry-Forward Implications For `U5` Only|does not pre-decide `U6`|does not authorize solver/pipeline implementation work|No widening is authorized'` -> pass:
  - `16:Scope is strictly bounded to feasibility/clearance evidence for repaired `URI-R2-C1` and does not authorize solver/pipeline implementation work.`
  - `24:No widening is authorized to multi-SCC search, cross-family search, broad automatic recursive inference, second interfaces, compatibility shims, or fallback/default-path behavior.`
  - `72:## Carry-Forward Implications For `U5` Only (Non-Preemptive)`
  - `76:- This artifact does not pre-decide `U6`; it records only the bounded `U4` feasibility result.`

### Full-Gate Note

`cabal build all && cabal test` was intentionally not run because this round is docs-only and does not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
