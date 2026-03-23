# `N8` Post-`N7` Roadmap-Amendment Authority Gate For Automatic Iso-Recursive Inference

Date: 2026-03-22
Round: `round-075`
Roadmap item: `N8`
Stage: `implement`
Attempt: `attempt-2`
Retry state: same-round retry after rejected `attempt-1`
Live subject: interpret accepted `N7 = continue-bounded` into one bounded
post-`N7` authority outcome without reopening the accepted non-local
`baseTarget -> baseC` packet as live implementation or verification work
Artifact kind: canonical docs-only roadmap-amendment authority record

## Stage Contract Freeze

This artifact implements only roadmap item `N8` for same-round retry
`attempt-2` after rejected `attempt-1`.

`N8` is docs-only roadmap-amendment authority work. It interprets accepted
`round-074` / `N7 = continue-bounded` into exactly one bounded post-`N7`
authority outcome. It does not choose a new live subject, bind a new exact
target, authorize implementation, authorize verification, edit
`orchestrator/roadmap.md`, edit `orchestrator/state.json`, edit `Bugs.md`, or
rewrite predecessor review history.

`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json` is cited
below only for stable contract continuity: valid JSON, `contract_version: 2`,
and presence of a contract-compliant `retry` field. No canonical `N8` claim in
this artifact depends on the controller remaining at any particular stage,
task, branch-position, or retry-object shape after implementer handoff.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path fallback widening.

## Accepted `N7` Authority And Binding Continuity

Only binding predecessor continuity is carried forward here:

1. `selection.md` and `plan.md` for `round-075` fix this round to roadmap
   item `N8` only, keep it docs-only and authority-only, and forbid roadmap
   edits, state edits, implementation, verification, and silent widening.
2. `orchestrator/roadmap.md` marks item `8` pending and states that item `8`
   completes only when one accepted docs-only artifact interprets accepted
   `N7 = continue-bounded` into exactly one bounded next-step authority
   outcome: either keep additional work closed, or authorize exactly one
   fresh planning-only bounded successor lane.
3. Accepted `N1` remains the authoritative post-`L2` reopening record:
   `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
   reopened work only at planning scope, preserved the inherited boundary
   unchanged, and did not authorize implementation or verification.
4. Accepted `N7` remains the authoritative bounded closure / next-cycle
   decision record:
   `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-next-cycle-decision-gate.md`
   finalized the exact accepted non-local `baseTarget -> baseC` packet as one
   bounded verified packet with final outcome `continue-bounded`, explicitly
   rejected `stop-blocked` and `completed` on that same packet, preserved the
   inherited boundary plus blocked-route set unchanged, and required a
   separate future roadmap amendment / update before any new bounded cycle can
   begin.
5. `orchestrator/rounds/round-074/review-record.json`,
   `orchestrator/rounds/round-074/review.md`, and
   `orchestrator/rounds/round-074/merge.md` remain reviewer/merger-owned
   acceptance proof that `round-074` finalized lawfully as
   `accepted + finalize` with `final_outcome = "continue-bounded"` and with
   the requirement for a separate future roadmap amendment / update preserved.
6. The mechanism-table long-horizon row remains unresolved:
   `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
   still records `N7` at `NO` and states that accepted `continue-bounded` is
   one bounded packet only, with any further work requiring a separate roadmap
   amendment / update before any new target or slice can begin.
7. Open `BUG-2026-03-16-001` remains replay-only predecessor context in
   `Bugs.md`. It does not reopen replay, `MLF.Elab.Inst`, `InstBot`, or any
   other live implementation subject in this round.

The accepted non-local `baseTarget -> baseC` packet is therefore predecessor
evidence only here. `N8` does not treat it as still-live implementation or
verification work.

## Current Docs / Continuity / No-Drift Checks

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-075`

### Baseline docs/state checks

- `git diff --check`
  -> pass (no output).
- `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json >/dev/null`
  -> pass.
- `rg -n '"contract_version": 2|"retry": null|"retry": \\{' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
  -> pass; `orchestrator/state.json` still satisfies the v2 retry-subloop
  contract with `contract_version: 2` and a contract-compliant `retry` field.
- `rg -n '^\\d+\\. \\[(pending|in-progress|done)\\]' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
  -> pass; the roadmap remains parseable and item `8` stays the active pending
  docs-only authority item.
- required artifact-presence checks
  -> pass for:
  `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
  `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`,
  `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`,
  `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`,
  `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-next-cycle-decision-gate.md`,
  `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`,
  `orchestrator/retry-subloop.md`, and
  `orchestrator/rounds/round-074/review-record.json`.

### Retry-repair / no-drift checks

- forbidden live-controller assertion scan on
  `docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`
  -> pass (no output); the artifact no longer contains explicit live
  controller-position assertions or any consistency claim that could go stale
  after handoff.
- `python3 - <<'PY'`
  `from pathlib import Path`
  `artifact = Path("docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md").read_text()`
  `required = [`
  `    "docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md",`
  `    "docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md",`
  `    "docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md",`
  `    "docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md",`
  `    "docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md",`
  `    "docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md",`
  `    "docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-next-cycle-decision-gate.md",`
  `    "tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md",`
  `    "orchestrator/retry-subloop.md",`
  `    "orchestrator/rounds/round-074/review-record.json",`
  `]`
  `missing = [path for path in required if path not in artifact]`
  `print("missing=none" if not missing else "missing=" + ",".join(missing))`
  `PY`
  -> pass (`missing=none`).
- `rg -n 'Attempt: `attempt-2`|reopen-planning-only-successor-lane|predecessor evidence only|does not choose the next live subject|does not authorize implementation|does not authorize verification' docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`
  -> pass; `attempt-2`, the selected authority outcome, and the unchanged
  scope guards all remain recorded.
- `git diff --name-only -- src test src-public app mlf2.cabal`
  -> pass (no output).
- `git diff --name-only -- orchestrator/roadmap.md orchestrator/state.json Bugs.md`
  -> pass (no output).

No read-only continuity or no-drift check found a new contradiction that would
force this round to fail-closed.

## Lawful Verification Skip Note

No fresh code-path verification was rerun in this round.

That skip is deliberate and lawful because:

1. `N8` is docs-only roadmap-amendment authority work;
2. this round authorizes no edits under `src/`, `test/`, `src-public/`,
   `app/`, or `mlf2.cabal`;
3. current no-drift checks found no tracked code/public/executable/Cabal drift
   that would stale accepted predecessor verification continuity; and
4. `N8` is not itself a verification gate and does not reinterpret the
   accepted non-local `baseTarget -> baseC` packet as still-live code work.

## One Authoritative `N8` Outcome

Authoritative `N8` outcome: `reopen-planning-only-successor-lane`.

This is the only lawful authority outcome on the current evidence.

It is lawful because all five `N8` continuity conditions hold:

1. accepted `round-074` finalized `N7` lawfully as `continue-bounded`, not
   `stop-blocked` or `completed`, on the exact accepted non-local
   `baseTarget -> baseC` packet;
2. accepted `N7` explicitly required a separate future roadmap amendment /
   update before any new bounded cycle may begin;
3. current read-only continuity and no-drift checks found no new blocker or
   contradiction that would invalidate planning-only successor authority while
   preserving accepted `round-074` truth;
4. the mechanism-table long-horizon `N7` row remains `NO`, so the
   long-horizon goal is still unresolved and any future work must remain
   gated; and
5. reopening only one fresh docs-first successor-planning lane preserves the
   accepted non-local packet as predecessor evidence only, preserves the
   inherited boundary unchanged, and avoids silently choosing a new target or
   widening into live code work.

The exact meaning of `reopen-planning-only-successor-lane` is fixed:

- one fresh docs-first successor-planning lane is authorized for later bounded
  successor-selection work only;
- the accepted non-local `baseTarget -> baseC` packet is predecessor evidence
  only and may not be treated as still-live implementation or verification
  work;
- the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary remains unchanged;
- replay reopen, `MLF.Elab.Inst`, `InstBot`, accepted local lanes,
  `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `src/MLF/Elab/Run/ResultType/View.hs`, every other fallback family, every
  different solver/pipeline subject, cross-family search, equi-recursive
  reasoning, implicit unfolding, cyclic structural graph encoding, multi-SCC
  support, second-interface work, and fallback widening remain blocked unless
  a later accepted roadmap item explicitly reopens them; and
- `N8` itself does not choose the next live subject, bind an exact next
  target, authorize implementation, authorize verification, or rewrite
  roadmap/state/Bugs/history truth.

## Why `keep-additional-work-closed` Is Not Lawful On This Evidence

`keep-additional-work-closed` is not lawful on the same evidence because no
new fail-closing contradiction was found.

Accepted `round-074` already rejected `stop-blocked` on the exact accepted
non-local `baseTarget -> baseC` packet and preserved the requirement for a
separate future roadmap amendment / update before any new bounded cycle could
begin. Current read-only checks found no missing accepted artifact, no
tracked code/public/executable/Cabal drift, and no blocker intersecting the
planning-only successor lane itself so directly that even later bounded
successor selection would be unlawful.

Fail-closing additional work here would therefore collapse accepted
`continue-bounded` into a de facto `stop-blocked` result without the concrete
new contradiction that the accepted `N7` artifact said would be required.

Absence of a chosen next subject is not such a contradiction. That ambiguity is
exactly why the lawful `N8` outcome is planning-only rather than fail-closed.

Open `BUG-2026-03-16-001` also does not make
`keep-additional-work-closed` lawful here because the bug remains replay-only
predecessor context and does not invalidate planning-only successor authority.

## Why Any Wider Reopen Is Not Lawful On This Evidence

Any wider outcome is not lawful here because it would skip the bounded
authority gate that `N8` itself is supposed to provide.

The accepted `N8` scope permits exactly one bounded authority outcome only:
either fail-close additional work on a concrete contradiction, or authorize
exactly one fresh planning-only successor lane. The current evidence supports
the second option only.

This round therefore may not lawfully:

- choose the next live subject;
- bind an exact next target;
- authorize implementation;
- authorize verification;
- reopen the accepted non-local `baseTarget -> baseC` packet as live code
  work;
- reopen replay, `MLF.Elab.Inst`, `InstBot`, accepted local lanes,
  `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `src/MLF/Elab/Run/ResultType/View.hs`, any other fallback family, any
  different solver/pipeline subject, or cross-family search; or
- widen into equi-recursive reasoning, implicit unfolding, cyclic encoding,
  multi-SCC support, a second executable interface, or any compatibility /
  convenience / default-path fallback.

Any reopening beyond the planning-only successor lane still requires a later
accepted roadmap item.

## Preserved Boundary, Remaining Blockers, And Future-Gate Need

Accepted `L1`, `L2`, `N1`, `N2`, `N3`, `N4`, `N5`, `N6`, and the accepted
non-local `baseTarget -> baseC` packet from `N7` remain predecessor evidence
only.

The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path fallback widening.

The remaining blocked state after this artifact is:

- the long-horizon automatic iso-recursive inference goal remains unresolved;
- no next live subject is yet selected for a new bounded cycle;
- no exact next target is yet bound;
- no new implementation or verification work is yet authorized; and
- every blocked route named above remains blocked unless a later accepted
  roadmap item explicitly reopens it.

The only change in authority recorded here is that one fresh docs-first
successor-planning lane may now be used to decide what, if anything, the next
bounded cycle should be.

## Reviewer Handoff

Reviewer should confirm that this artifact:

- is explicitly framed as `round-075` / `N8` / `attempt-2` / same-round retry
  after rejected `attempt-1`;
- no longer records mutable live controller-state assertions or any claim that
  later controller state remains consistent with the artifact;
- records the full ten-item predecessor artifact-presence set required by Task
  6;
- records exactly one authoritative outcome,
  `reopen-planning-only-successor-lane`;
- treats the accepted non-local `baseTarget -> baseC` packet as predecessor
  evidence only;
- preserves the inherited boundary and blocked-route set unchanged;
- explains why `keep-additional-work-closed` is not lawful on the current
  evidence;
- explains why any wider reopen, selection, implementation, or verification
  outcome is not lawful here; and
- records that any reopening beyond the planning-only successor lane still
  requires a later accepted roadmap item.
