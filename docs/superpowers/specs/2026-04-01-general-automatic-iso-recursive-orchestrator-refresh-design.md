# General Automatic Iso-Recursive Inference Orchestrator Refresh Design

Date: 2026-04-01

## Goal

Retarget the live repo-local `orchestrator/` control plane so the active
roadmap family once again drives progress toward general automatic
iso-recursive inference, instead of pointing at the already-completed
codebase-quality roadmap.

This change is a control-plane refresh only. It must stop after the scaffolded
roadmap bundle, role prompts, state retarget, and checkpoint commit. It must
not start implementation rounds.

## Current State

The repository already has a mature top-level `orchestrator/` contract, so
this is not a blank bootstrap.

Current observed state:

- `orchestrator/state.json` points at
  `2026-03-30-01-codebase-quality-and-coverage-improvements`, which is
  completed and unrelated to the user goal.
- `orchestrator/roadmap.md` is stale as a pointer stub and does not match the
  active family in `state.json`.
- The March automatic-iso-recursive campaigns produced two different kinds of
  evidence:
  - completed implementation/gap-fix evidence for the bounded supported
    mechanism (`2026-03-29-01-automatic-iso-recursive-type-inference-completion`
    and `2026-03-29-02-iso-recursive-inference-gap-fixes`);
  - unresolved repo-scope representative-gap evidence, especially the
    bounded current-architecture blocker family around
    `sameLaneAliasFrameClearBoundaryExpr`.

The refresh must preserve that distinction. The new live family must not
pretend that broad repo-level readiness is already settled.

## Decision

Create a fresh active roadmap family:

- `roadmap_id`: `2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap`
- `roadmap_revision`: `rev-001`
- `base_branch`: keep the current orchestrator base branch
  `codex/automatic-recursive-type-inference`

Then retarget the live control plane so `orchestrator/state.json` points at
that new bundle and the top-level pointer stubs match it.

The March 14 through March 30 automatic-iso-recursive families remain tracked
historical evidence only. They are not rewritten.

## Why A Fresh Family

This is preferable to reopening an older family revision because:

1. the March families already have accepted round histories and should remain
   immutable once used;
2. the currently active family is unrelated completed work, so a clean cutover
   is clearer than reusing its controller state;
3. the next live question is narrower than “implement automatic iso-recursive
   inference” but broader than one stale blocker snapshot, so it deserves a
   fresh successor family with explicit predecessor authority.

## Live Subject For The Refreshed Family

The refreshed family is not allowed to restart from a generic “make recursive
inference better” prompt.

Its inherited live subject is:

- push the repo from bounded mechanism support toward an honest general
  automatic iso-recursive capability claim;
- stay inside the inherited current architecture unless later accepted rounds
  explicitly revise that boundary;
- continue from the still-unresolved representative-gap lane rather than
  reopening already-closed predecessor packets; and
- treat the exact second-packet subject
  `sameLaneAliasFrameClearBoundaryExpr` as the concrete anchor for the next
  bounded follow-on unless the first new round proves a more precise exact
  breakpoint inside that same lane.

The new family must explicitly inherit:

- the March 29 implementation/gap-fix families as proof that the bounded
  automatic μ-introduction mechanism exists end to end; and
- the March 27 to March 29 representative-gap documents as proof that broad
  repo-level readiness remains open.

## Roadmap Shape

The initial roadmap should stay serial (`max_parallel_rounds: 1`). There is no
strong current evidence that safe parallel rounds would help more than they
would complicate `dist-newstyle`, repo-state, and evidence integration.

The roadmap will contain four items.

### Item 1

Title:
`Freeze successor authority, exact inherited blocker lane, and current writable slice`

Purpose:
publish one docs-only freeze that binds the predecessor evidence chain, the
current inherited boundary, the exact live representative-gap lane, and the
exact writable slice for the first bounded follow-on round.

Concrete focus:

- carry forward the March 29 blocker packet
  `sameLaneAliasFrameClearBoundaryExpr`;
- acknowledge that current live tests allow the blocker to surface as either
  the earlier `PhiTranslatabilityError` or a later exact-packet
  type-check/pipeline blocker, so the first follow-on family must freeze the
  current exact read before implementation claims;
- explicitly forbid scope widening into cyclic search, multi-SCC search,
  equi-recursive semantics, fallback widening, or repo-level readiness claims.

Parallel metadata:

- `Parallel safe: no`
- `Parallel group: none`
- `Merge after: none`

### Item 2

Title:
`Implement and validate one bounded current-architecture slice on the frozen blocker lane`

Purpose:
land exactly one bounded production/test slice within the frozen lane from
item 1 and prove its effect with focused and full verification.

Parallel metadata:

- `Parallel safe: no`
- `Parallel group: none`
- `Merge after: item-1`

### Item 3

Title:
`Publish one settlement surface and exact repo-impact read for the bounded slice`

Purpose:
record whether item 2 produced a real narrow success, a narrower blocker, or a
fail-closed result for that exact lane, without upgrading it into broad
readiness.

Parallel metadata:

- `Parallel safe: no`
- `Parallel group: none`
- `Merge after: item-2`

### Item 4

Title:
`Record the next architecture decision and successor handoff`

Purpose:
turn the item-3 result into one explicit next-step token:
continue-bounded, stop-blocked, or reopen-boundary-question, with explicit
reasoning.

Parallel metadata:

- `Parallel safe: no`
- `Parallel group: none`
- `Merge after: item-3`

## Control-Plane Files To Refresh

The scaffold/update pass should modify:

- `orchestrator/state.json`
- `orchestrator/roadmap.md` pointer stub
- `orchestrator/verification.md` pointer stub or matching live pointer text
- `orchestrator/retry-subloop.md` pointer stub or matching live pointer text
- `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/verification.md`
- `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/retry-subloop.md`
- `orchestrator/roles/guider.md`
- `orchestrator/roles/planner.md`
- other role files only where the current code-quality wording is now clearly
  stale for this family

No round execution artifacts should be created beyond the scaffold state for
the new family.

## State Retarget Rules

`orchestrator/state.json` should be refreshed to:

- the new `roadmap_id`, `roadmap_revision`, and `roadmap_dir`;
- `stage: "done"` and `controller_stage: "done"` if the scaffold ends idle;
- `max_parallel_rounds: 1`;
- no active rounds, no pending merges, no retry state;
- preserved `base_branch` unless there is strong repo evidence to change it.

The top-level pointer stubs must match `state.json` exactly after the refresh.

## Verification Contract Design

The new active bundle’s `verification.md` should require:

- `cabal build all`
- `cabal test`
- `./scripts/thesis-conformance-gate.sh`
- roadmap identity consistency across `state.json`, `selection.md`, and
  `review-record.json`
- focused checks for the inherited automatic-iso-recursive lane, including:
  - `sameLaneAliasFrameClearBoundaryExpr`
  - `C1 authoritative-surface harness`
  - `P5 clear-boundary retained-child probes`

Task-specific checks should tell reviewers to confirm that one bounded packet
is still treated as one bounded packet, not silently upgraded into general
repo-level readiness.

## Retry Contract Design

The new active bundle’s `retry-subloop.md` should allow:

- normal same-round retry when the bounded slice fails review;
- bounded replanning after reviewer evidence;
- escalation after three rejected attempts on the same round.

It should forbid:

- widening from the frozen packet to a different family without a later
  accepted roadmap update;
- rewriting already-used predecessor roadmap revisions; and
- treating documentation-only settlement as implementation clearance.

## Role Retuning

The current role prompts are still specialized for the completed quality
roadmap. They should be retuned so:

- the guider selects only from the new automatic-iso-recursive successor
  family and preserves predecessor evidence honestly;
- the planner treats the inherited boundary as explicit-only /
  iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback by
  default;
- the implementer and reviewer are reminded that the live goal is bounded
  current-architecture progress, not a broad readiness claim;
- serial execution remains the default unless a later accepted roadmap
  revision explicitly authorizes parallel rounds.

## Non-Goals

This refresh does not:

- run any new orchestrator rounds;
- implement code or tests for recursive inference directly;
- reopen already-accepted predecessor packets as if they were live debt;
- claim broad general automatic iso-recursive readiness; or
- change the inherited semantic boundary by implication.

## Expected Output Of This Turn

After implementation of this design, the repo should have:

- a new active roadmap family under `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/`;
- matching live pointers in `orchestrator/state.json` and the top-level stubs;
- role prompts and verification/retry contracts aligned to the new family; and
- one checkpoint commit containing only the orchestrator refresh scaffold and
  any required tracked docs/ignore updates.
