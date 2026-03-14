# Staged Roadmap Design: `URI-R2-C1` Re-Entry Evidence After Bounded `research-stop`

Date: 2026-03-14
Status: Proposed design

## Purpose

Define the next research-only roadmap that begins from the accepted bounded `R5` `research-stop` decision for `URI-R2-C1` and stages toward a **re-entry verdict** on whether a later implementation-handoff track may be reopened.

This design does **not** authorize implementation work, a handoff document, or broad new recursive-inference scope. It defines only the next bounded evidence track.

## Starting Point

The completed top-level successor orchestrator reached a bounded endpoint:

- the finished roadmap lives in accepted rounds `round-006` through `round-010`;
- `URI-R2-C1` is the only bounded unannotated subset selected so far;
- `URI-R3-O1` through `URI-R3-O5` are the accepted obligation classes for that subset;
- `R4` concluded `not-yet-go` for `URI-R2-C1`;
- `R5` therefore recorded an explicit bounded `research-stop`, not an implementation handoff;
- the inherited item-2 invariant audit remains authoritative for acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality-risk boundaries.

The accepted `R5` stop already names the admissible re-entry direction:

- authoritative provenance-stable evidence for the replay/reification subject;
- bounded uniqueness evidence without heuristic ranking; and
- bounded positive evidence for `URI-R3-O1` through `URI-R3-O3` without forbidden widening.

## Final Goal Of The New Roadmap

The next roadmap should end at a **research-backed re-entry verdict**, not at implementation.

That verdict must choose exactly one of:

- `reopen-handoff-track`: the bounded evidence is now strong enough that a later, separate roadmap may design a new implementation-handoff track for `URI-R2-C1`; or
- `remain-stop`: the evidence is still insufficient, so the bounded stop remains in force with updated re-entry requirements.

This roadmap does **not** itself write an implementation handoff.

## Non-Goals

- No production implementation milestone.
- No direct implementation-handoff spec.
- No widening beyond `URI-R2-C1`.
- No multi-SCC support.
- No cross-family SCC linking.
- No equi-recursive reasoning or implicit unfolding semantics.
- No cyclic structural-graph encoding.
- No silent default-on behavior from any prototype or experiment.
- No rewriting of predecessor packet history or accepted rounds `001` through `010`.

## Chosen Roadmap Shape

Use a **re-entry evidence ladder** rooted in the accepted bounded stop.

The roadmap uses:

- the accepted stop state as `RE0`;
- three evidence-contract stages (`RE1` through `RE3`);
- one bounded re-entry gate (`RE4`); and
- one final successor recommendation stage (`RE5`).

## Fixed Boundary Model

These constraints remain mandatory for the entire roadmap:

- The active subject remains **only** `URI-R2-C1`.
- The subject remains `single-SCC` and `single-binder-family` only.
- Recursion remains obligation-level rather than cyclic structural-graph encoding.
- No cross-family SCC linking is admitted.
- No equi-recursive reasoning or implicit unfolding is admitted.
- No default-on widening is admitted.
- The structural `TyNode` / constraint graph must remain non-cyclic.
- Any prototype evidence must be explicitly authorized by a roadmap stage, remain bounded and non-default, and never be treated as implementation clearance by itself.
- The authoritative inherited invariant audit remains controlling for safety boundaries.

## Roadmap Milestones

### `RE0` Existing Bounded Stop

Accepted prior result:

- `URI-R2-C1` ended at bounded `research-stop` because `R4` concluded `not-yet-go`.

This is the starting point, not a new executable milestone.

### `RE1` Provenance-Authority Evidence Contract For `URI-R3-O4`

Deliverable:

- one artifact that defines what counts as authoritative provenance-stable evidence for one unannotated root/cluster inside `URI-R2-C1`;
- one rejection list for evidence that still depends on manufactured authority, late repair, replay-domain widening, or unstable binder naming.

Purpose:

- isolate the decisive blocker from the accepted stop and make the admissible evidence shape reviewer-checkable.

Exit condition:

- a reviewer can point to a finite contract for provenance authority and say exactly what evidence would or would not clear `URI-R3-O4`.

### `RE2` Uniqueness-Evidence Contract For `URI-R3-O5`

Deliverable:

- one artifact that defines what bounded evidence is sufficient to show one uniquely admissible local root/cluster for `URI-R2-C1`;
- explicit rejection of heuristic ranking, competing roots, multi-cluster comparisons, or widening-dependent uniqueness claims.

Purpose:

- separate “one subject exists” from “one subject is uniquely admissible,” since the accepted stop says that uniqueness remains unresolved.

Exit condition:

- later stages can judge uniqueness against one explicit contract instead of reinterpreting `R2` / `R3`.

### `RE3` Positive-Evidence Contract For `URI-R3-O1` Through `URI-R3-O3`

Deliverable:

- one artifact that defines the positive evidence needed for:
  - structural acyclicity preservation,
  - deterministic single-family owner stability, and
  - constructor-directed occurs-check/termination clearance;
- one explicit statement of whether docs-only evidence is sufficient or whether bounded prototype evidence is authorized;
- if bounded prototype evidence is authorized, one explicit non-default prototype envelope:
  - touched surfaces,
  - disallowed widenings,
  - required tests/guards,
  - stop triggers,
  - and how prototype output may be cited without becoming implementation behavior.

Purpose:

- make the positive-evidence burden explicit instead of implicitly assuming the prior docs-only stop can be retried unchanged.

Exit condition:

- the repository has one approved evidence contract for `URI-R3-O1` through `URI-R3-O3`, including an explicit yes/no on bounded prototype authorization.

### `RE4` Execute The Bounded Re-Entry Gate

Deliverable:

- one artifact that evaluates the evidence gathered against `RE1`, `RE2`, and `RE3`;
- one explicit bounded decision outcome:
  - `reopen-handoff-track`, or
  - `not-yet-reopen`.

Required contents:

- the evidence actually gathered;
- which re-entry contracts were satisfied and which were not;
- no-go triggers and immediate stop conditions;
- prototype-boundary audit, if prototypes were authorized;
- reviewer-visible explanation for the decision.

Purpose:

- decide whether the bounded stop has actually been overcome for `URI-R2-C1`, rather than assuming more evidence must help.

Exit condition:

- the active subject is explicitly judged either ready to reopen a later handoff-track design or still not ready.

### `RE5` Final Successor Recommendation

Deliverable:

- one final bounded recommendation document:
  - if `RE4` concluded `reopen-handoff-track`, write the bounded recommendation for a later handoff-track roadmap; or
  - if `RE4` concluded `not-yet-reopen`, write the renewed bounded stop decision and updated re-entry requirements.

Purpose:

- close the next track cleanly without blurring research evidence into implementation authorization.

Exit condition:

- the repository has one explicit final result for the re-entry track and a clear next-step recommendation.

## Validation Model

Each stage must produce a reviewer-visible artifact with an explicit gate.

- `RE1` gate: provenance-authority requirements are concrete and bounded.
- `RE2` gate: uniqueness requirements are concrete and bounded.
- `RE3` gate: positive-evidence requirements are concrete and any prototype envelope is explicit, narrow, and non-default.
- `RE4` gate: the re-entry verdict is explicit and justified against the `RE1` through `RE3` contracts.
- `RE5` gate: the final recommendation is concrete enough to tell the next effort whether to start a new handoff-track roadmap or preserve the bounded stop.

## Fail-Closed Rules

The roadmap must stop and record `remain-stop` / `not-yet-reopen` if any stage discovers that progress requires:

- widening beyond `URI-R2-C1`;
- multi-SCC or cross-family exploration;
- equi-recursive reasoning or implicit unfolding;
- cyclic graph representation;
- default-on prototype behavior;
- unverifiable claims about replay authority, uniqueness, soundness, principality, or termination.

## Recommendation

Adopt this roadmap as the next separate successor track after the accepted bounded stop.

It is the smallest honest next step because it:

- starts from the recorded stop instead of pretending implementation is next;
- turns the accepted blockers into explicit evidence contracts;
- allows bounded experimental evidence only if a reviewed stage explicitly authorizes it;
- keeps the subject fixed to `URI-R2-C1`; and
- ends at a re-entry verdict rather than collapsing directly into implementation planning.
