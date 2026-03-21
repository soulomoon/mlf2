# `K3` Bounded Verification Gate For Repaired `URI-R2-C1`

Date: 2026-03-21
Round: `round-064`
Roadmap item: `K3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bounded verification and evidence consolidation gate

## Stage Contract

This artifact records only roadmap item `K3` for `attempt-1`.

`K3` is a docs-only verification gate. It rechecks the accepted repaired
`URI-R2-C1` `K2`
`rootLocalEmptyCandidateSchemeAliasBaseLike` local empty-candidate /
no-inst-arg scheme-alias / base-like `baseTarget -> baseC` / same-lane
`targetC` lane under read-only anchors plus fresh command evidence.

This round did not edit production code, tests, public API, executables,
`mlf2.cabal`, roadmap state, controller state, or `/Volumes/src/mlf4/Bugs.md`.
The pre-existing controller preparation state remained untouched:

- `M orchestrator/state.json`
- `?? orchestrator/rounds/round-064/plan.md`
- `?? orchestrator/rounds/round-064/selection.md`

The inherited boundary remains fixed:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

Accepted negative findings remain binding continuity only:

- `U2 = authority-narrowed`
- `U3 = uniqueness-owner-stable-refuted`
- `U4 = constructor-acyclic-termination-refuted`

## Accepted Authority Chain Reconfirmed

The predecessor authority chain required by `K3` still stands and remained
unchanged throughout this verification attempt.

1. `python3 -m json.tool orchestrator/rounds/round-061/review-record.json >/dev/null`
   -> pass.
2. `python3 -m json.tool orchestrator/rounds/round-062/review-record.json >/dev/null`
   -> pass.
3. `python3 -m json.tool orchestrator/rounds/round-063/review-record.json >/dev/null`
   -> pass.
4. `python3` review-record assertion over `round-061` through `round-063`
   -> pass:
   - `PASS orchestrator/rounds/round-061/review-record.json stage_id=J4 attempt=1 verdict=accepted action=finalize status=authoritative artifact=docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
   - `PASS orchestrator/rounds/round-062/review-record.json stage_id=K1 attempt=1 verdict=accepted action=finalize status=authoritative artifact=docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
   - `PASS orchestrator/rounds/round-063/review-record.json stage_id=K2 attempt=1 verdict=accepted action=finalize status=authoritative artifact=docs/plans/2026-03-21-uri-r2-c1-k2-bounded-implementation-slice.md`
5. Artifact presence checks:
   - `test -f docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
     -> pass
   - `test -f docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
     -> pass
   - `test -f docs/plans/2026-03-21-uri-r2-c1-k2-bounded-implementation-slice.md`
     -> pass

Accepted scope continuity remains the same:

- `J4` is still the authoritative accepted `continue-bounded` decision.
- `K1` still freezes exactly one bounded successor slice: the local-binding
  empty-candidate / no-inst-arg scheme-alias / base-like
  `baseTarget -> baseC` lane plus its same-lane `targetC` use.
- `K2` still records the exact bounded implementation result for that slice
  only, including the explicit
  `rootLocalEmptyCandidateSchemeAliasBaseLike` proof and its dedicated
  same-lane `targetC` arm.
- `git diff --name-only -- orchestrator/roadmap.md docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md docs/plans/2026-03-21-uri-r2-c1-k2-bounded-implementation-slice.md orchestrator/rounds/round-061/review-record.json orchestrator/rounds/round-062/review-record.json orchestrator/rounds/round-063/review-record.json docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass (no output).

## Read-Only Anchor Evidence

### `src/MLF/Elab/Run/ResultType/Fallback.hs`

Line-anchored read-only evidence confirms that the accepted `K2` lane still
exists and remains bounded to the exact selected family:

- `377-381`: the selected empty-candidate / no-inst-arg `baseTarget` branch
  still returns `Just baseC` only when `rootBoundCandidates` and
  `instArgBaseBounds` are both empty and both multi-inst guards are false.
- `531-536`: the completed adjacent `rootLocalInstArgSingleBase` proof remains
  present as inherited continuity only.
- `537-544`: the selected
  `rootLocalEmptyCandidateSchemeAliasBaseLike` proof remains present with the
  exact seven frozen ingredients:
  `rootBindingIsLocalType`,
  `rootIsSchemeAlias`,
  `rootBoundIsBaseLike`,
  `IntSet.null rootBoundCandidates`,
  `IntSet.null instArgBaseBounds`,
  `not rootHasMultiInst`,
  `not instArgRootMultiBase`.
- `545-548`: the already accepted
  `rootLocalSchemeAliasBaseLike` continuity proof remains present.
- `549-553`: the completed adjacent `rootLocalSingleBase` proof remains
  present.
- `693-699`: `keepTargetFinal` still stays limited to inherited retained-target
  families and does not include the selected `K2` proof.
- `700-710`: the final `targetC` selection order still preserves:
  - `703-704` completed `rootLocalSingleBase`;
  - `705-706` completed `rootLocalInstArgSingleBase`;
  - `707` selected `rootLocalEmptyCandidateSchemeAliasBaseLike`;
  - `708-710` preserved broader scheme-alias / base-like route.

### `test/PipelineSpec.hs`

The focused `ARI-C1 feasibility characterization (bounded prototype-only)`
block still matches the accepted bounded verification surface:

- `1244-1269`: the adjacent scheme-alias / base-like continuity helper remains
  in the existing wrapper family above the selected helper.
- `1270-1303`: the selected
  `localEmptyCandidateSchemeAliasBaseLikeFallback` helper remains present.
- `1382-1432`: the completed adjacent
  `localInstArgSingleBaseFallback` helper remains present as inherited
  continuity only.
- `1433-1459`: the completed adjacent `localSingleBaseFallback` helper remains
  present as inherited continuity only.
- `1594-1596`: the accepted positive local example for the selected `K2` lane
  remains present and still expects `TBase (BaseTy "Int")`.
- `1598-1606`: the matched local continuity contrast for the accepted
  scheme-alias / base-like `rootFinal` lane remains present.
- `1608-1620`: the completed adjacent local single-base lane remains present as
  inherited continuity only.
- `1667-1679`: the completed adjacent inst-arg-only singleton-base lane
  remains present as inherited continuity only.
- `1698-1758`: the source-guard block still names
  `rootLocalEmptyCandidateSchemeAliasBaseLike`,
  `rootLocalSingleBase`,
  `rootLocalInstArgSingleBase`,
  the dedicated selected `targetC` arm,
  preserved `keepTargetFinal`,
  preserved `rootLocalSchemeAliasBaseLike`,
  and the broader scheme-alias / base-like route.

Taken together, the read-only anchors still prove that `K3` remains bounded to
the accepted `K2` lane only and has not silently widened into retained-target
logic, replay reopen, non-local widening, or a broader trigger family.

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-064`

### Baseline Commands

- `git branch --show-current`
  -> pass (`codex/round-064-k3-verification-gate`)
- `git status --short --untracked-files=all`
  -> pass snapshot:
  - ` M orchestrator/state.json`
  - `?? orchestrator/rounds/round-064/plan.md`
  - `?? orchestrator/rounds/round-064/selection.md`
- `git diff --check`
  -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null`
  -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `16:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass:
  - ordered roadmap remains parseable
  - roadmap item `31` / `K3` remains pending at line `188` during implement stage
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f orchestrator/retry-subloop.md`
  -> pass

### Focused `ARI-C1` Rerun

- `PATH=/Users/ares/.ghcup/bin:$PATH cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass:
  - `20 examples, 0 failures`
  - all twenty bounded examples in the `ARI-C1 feasibility characterization (bounded prototype-only)` block passed, including the selected local empty-candidate / no-inst-arg scheme-alias / base-like `K2` lane, the matched local scheme-alias / base-like continuity lane, the completed local single-base lane, the completed inst-arg-only singleton-base lane, and the source-guard assertion block.

### Fresh Full Repo Gate

- `PATH=/Users/ares/.ghcup/bin:$PATH cabal build all && PATH=/Users/ares/.ghcup/bin:$PATH cabal test`
  -> pass:
  - build completed successfully under GHC `9.12.2`
  - `Finished in 2.1576 seconds`
  - `1141 examples, 0 failures`
  - `Test suite mlf2-test: PASS`

## Predecessor Continuity Checks

- `for i in $(seq 1 33); do d=$(printf 'orchestrator/rounds/round-%03d' "$i"); test -d "$d" || exit 1; done`
  -> pass
- `test -d tasks/todo/2026-03-11-recursive-types-orchestration`
  -> pass
- `/Volumes/src/mlf4/Bugs.md` continuity read:
  - `rg -n '## Open' /Volumes/src/mlf4/Bugs.md`
    -> pass (`5:## Open`)
  - read-only inspection confirms the `## Open` section is empty, so no live
    bug-tracker blocker invalidates the accepted `K2` lane.

These continuity checks confirm that `K3` still inherits the accepted `J4`,
`K1`, and `K2` packet without reopening implementation, widening the live
subject, or treating historical material as new authority.

## Docs-Only Diff Scope

Post-write docs-only diff-scope verification confirms that the finished round
remained docs-only apart from the pre-existing controller-state modification:

- `git status --short --untracked-files=all`
  -> pass snapshot:
  - ` M orchestrator/state.json`
  - `?? docs/plans/2026-03-21-uri-r2-c1-k3-bounded-verification-gate.md`
  - `?? orchestrator/rounds/round-064/implementation-notes.md`
  - `?? orchestrator/rounds/round-064/plan.md`
  - `?? orchestrator/rounds/round-064/selection.md`
- `git diff --name-only`
  -> pass:
  - `orchestrator/state.json`
- `git diff --name-only -- . ':(exclude)orchestrator/state.json'`
  -> pass (no tracked diff outside the pre-existing controller-state file)
- `git diff --name-only -- src test app src-public mlf2.cabal orchestrator/roadmap.md`
  -> pass (no output)
- `git diff --name-only -- src/MLF/Elab/Run/ResultType/Fallback.hs test/PipelineSpec.hs`
  -> pass (no output)
- `git ls-files --others --exclude-standard`
  -> pass:
  - `docs/plans/2026-03-21-uri-r2-c1-k3-bounded-verification-gate.md`
  - `orchestrator/rounds/round-064/implementation-notes.md`
  - `orchestrator/rounds/round-064/plan.md`
  - `orchestrator/rounds/round-064/selection.md`

This confirms the finished `K3` write set stayed bounded to the canonical
artifact plus `round-064` notes, while `plan.md` / `selection.md` remained the
pre-existing controller packet and `Fallback.hs` / `PipelineSpec.hs` stayed
strictly read-only.

## Result

`K3` evidence status: `pass`

The accepted repaired `URI-R2-C1` `K2`
`rootLocalEmptyCandidateSchemeAliasBaseLike` local empty-candidate /
no-inst-arg scheme-alias / base-like `baseTarget -> baseC` / same-lane
`targetC` lane remained stable under:

- read-only `Fallback.hs` / `PipelineSpec.hs` anchor rechecks;
- a fresh focused rerun of
  `ARI-C1 feasibility characterization (bounded prototype-only)`;
- a fresh full `cabal build all && cabal test` gate; and
- predecessor continuity rechecks against accepted `J4`, `K1`, `K2`, the
  inherited boundary docs, and the canonical bug tracker.

No blocker was observed in this implement-stage verification attempt.
