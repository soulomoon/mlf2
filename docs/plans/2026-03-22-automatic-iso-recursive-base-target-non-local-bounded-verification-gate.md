# `N6` Verification And Evidence Consolidation Gate For The Accepted `N5` Non-Local `baseTarget -> baseC` Proof Slice

Date: 2026-03-22
Round: `round-073`
Roadmap item: `N6`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: exact accepted `N5` non-local generic scheme-root alias-bound / base-like `baseTarget -> baseC` packet
Artifact kind: canonical docs-only verification / evidence consolidation gate

## Stage Contract Freeze

This artifact implements only roadmap item `N6` for `attempt-1` with
`retry: null`.

`N6` is a docs-only verification/evidence gate for the already-accepted `N5`
slice. It records fresh verifier-visible evidence for the exact accepted
non-local `rootNonLocalSchemeAliasBaseLike` packet only:

- the live read-only source anchors in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`;
- the live read-only focused test anchors in
  `test/PipelineSpec.hs`;
- a fresh focused rerun of
  `ARI-C1 feasibility characterization (bounded prototype-only)`;
- a fresh full repo gate via `cabal build all && cabal test`; and
- accepted-predecessor continuity against the authoritative `N3`, `N4`, and
  `N5` records.

This artifact does not authorize:

- edits under `src/`, `test/`, `src-public/`, `app/`, or `mlf2.cabal`;
- roadmap edits;
- `orchestrator/state.json` edits;
- bug-tracker edits;
- predecessor-history rewrites; or
- `N7` decision work.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic-graph structural encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path fallback widening.

If reverification had failed, that failure would have been a blocker to record
here, not permission to patch `Fallback.hs`, `PipelineSpec.hs`, or any other
code/test file during this attempt.

## Accepted Authority Chain Carried Forward Without Widening

Fresh continuity checks reconfirmed the accepted authority chain that still
binds this round:

1. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
   remains the accepted `N3` contract. Its binding axes still govern `N6`:
   alias-bound ownership must stay local to one owner-binder / owned-bound
   pair; bound inlining must stay inverse-translation-safe only; binding-flag
   reconstruction must come from structural/variance evidence only; recursive
   meaning must stay explicit-only / non-equi-recursive / non-cyclic-graph;
   and confinement must stay fixed to the one preserved non-local
   `baseTarget` packet.
2. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
   remains the accepted `N4` bind. The exact packet is still the preserved
   non-local generic scheme-root alias-bound / base-like
   `baseTarget -> baseC` packet plus its same-lane downstream `targetC`
   consumer only.
3. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`
   remains the accepted `N5` implementation artifact. It still means:
   `baseTarget` computation unchanged; explicit
   `rootNonLocalSchemeAliasBaseLike` proof present; only the same-lane generic
   `targetC` consumer routed through that proof; and focused
   `schemeAliasBaseLikeFallback False` / `True` plus source-guard anchors
   remaining the bounded test packet.
4. `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-072/review-record.json >/dev/null`
   passed, confirming the accepted `N5` review record stays valid JSON.
5. The continuity assertion over `round-066` through `round-072`
   re-confirmed authoritative accepted-finalized predecessor records for
   `L1`, `L2`, `N1`, `N2`, `N3`, `N4`, and `N5`, each pointing at the expected
   canonical artifact path.
6. The reviewer-readable summary over
   `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-072/review-record.json`
   reconfirmed `stage_id = N5`, `attempt_verdict = accepted`,
   `stage_action = finalize`, `status = authoritative`,
   `final_outcome = baseTarget-non-local-proof-slice-established`, and every
   recorded `checks` entry as `pass`.
7. `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
   still records `N6 = NO`, so this artifact is evidence-only and does not
   act as a decision token.
8. `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` remains continuity context
   only. Open `BUG-2026-03-16-001` does not reopen replay, `MLF.Elab.Inst`,
   `InstBot`, or any different live subject in this round.

## Read-Only Live Anchor Evidence

### `Fallback.hs` proof cluster and bounded consumer order

Read-only anchor command:

- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '531,719p'`

Observed live anchors in
`src/MLF/Elab/Run/ResultType/Fallback.hs`:

1. `rootLocalInstArgSingleBase` remains the earlier local singleton-base lane
   at lines `531-536`.
2. `rootLocalEmptyCandidateSchemeAliasBaseLike` remains the earlier local
   empty-candidate scheme-alias/base-like lane at lines `537-544`.
3. `rootNonLocalSchemeAliasBaseLike` remains the explicit selected non-local
   proof at lines `545-548`, defined only by
   `not rootBindingIsLocalType`, `rootIsSchemeAlias`, and
   `rootBoundIsBaseLike`.
4. `rootLocalSchemeAliasBaseLike` remains the preserved local continuity proof
   at lines `549-552`.
5. `rootLocalSingleBase` remains the separate local singleton-base lane at
   lines `553-557`.
6. `keepTargetFinal` remains local-only at lines `697-703`, because it is
   still gated by `rootBindingIsLocalType`.
7. The downstream `targetC` ordering at lines `704-719` still proves the
   selected packet is bounded and separate:
   `rootLocalSingleBase` first (`707`), then
   `rootLocalInstArgSingleBase` (`709`), then
   `rootLocalEmptyCandidateSchemeAliasBaseLike` (`711`), then the dedicated
   non-local arm `rootNonLocalSchemeAliasBaseLike` (`713`), with the
   `keepTargetFinal` / `rootFinal` cluster only after those earlier arms.

Those anchors show the accepted local lanes remain earlier and separate, while
the selected non-local proof remains one explicit named arm only.

### `PipelineSpec.hs` helper, focused packet, and source guard

Read-only anchor commands:

- `nl -ba test/PipelineSpec.hs | sed -n '1103,1135p'`
- `nl -ba test/PipelineSpec.hs | sed -n '1244,1278p'`
- `nl -ba test/PipelineSpec.hs | sed -n '1595,1768p'`

Observed live anchors in
`test/PipelineSpec.hs`:

1. The focused helper family remains in the same
   `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
   block beginning at line `1103`.
2. `schemeAliasBaseLikeFallback` remains the selected helper at lines
   `1244-1269`. It extracts the `AVar` body root, optionally localizes that
   root via `makeLocalTypeRoot`, and rebinds the canonical `Int` base node to
   `rootNid` via `rebindRootTo`.
3. `localEmptyCandidateSchemeAliasBaseLikeFallback` remains adjacent continuity
   context beginning at line `1270`; the helper family stays grouped in the
   same bounded block.
4. The preserved local continuity assertion using
   `schemeAliasBaseLikeFallback True` remains at lines `1598-1606`, and still
   expects the quantified `rootFinal` lane.
5. The selected non-local packet assertion using
   `schemeAliasBaseLikeFallback False` remains at lines `1622-1625`, and still
   requires `TBase (BaseTy "Int")` with `containsMu False`.
6. The source guard beginning at line `1698` still requires:
   - the preserved local empty-candidate arm text at `1700-1703`;
   - the preserved local singleton-base arms at `1704-1711`;
   - the explicit `rootNonLocalSchemeAliasBaseLike` proof text at
     `1712-1715`;
   - the dedicated `targetC` ordering that places
     `rootLocalEmptyCandidateSchemeAliasBaseLike` immediately before
     `rootNonLocalSchemeAliasBaseLike` at `1728-1731`;
   - rejection of the old generic
     `rootIsSchemeAlias && rootBoundIsBaseLike -> baseC` arm at `1732-1737`;
   - and local-only `keepTargetFinal` / `rootLocalSchemeAliasBaseLike` /
     `rootLocalMultiInst` / `rootLocalInstArgMultiBase` continuity at
     `1743-1768`.

These test anchors keep the selected non-local packet reviewer-visible while
preserving the local empty-candidate lane, the local continuity lane, and the
adjacent completed singleton-base lanes as inherited continuity only.

## Fresh Verification Results

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-073`

### Baseline checks

- `git branch --show-current`
  -> pass (`codex/round-073-n6-verification-evidence`)
- `git status --short --untracked-files=all`
  -> pass for bounded pre-edit state (`?? orchestrator/rounds/round-073/plan.md`,
     `?? orchestrator/rounds/round-073/selection.md`)
- `git diff --check`
  -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null`
  -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass (`2:  "contract_version": 2,`, `13:  "retry": null`)
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass (`N1` through `N5` done; `N6` and `N7` pending at lines
     `96-120`)
- Required artifact-presence checks
  -> pass for all of:
     `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
     `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`,
     `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`,
     `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`,
     `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`,
     `docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`,
     `docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`,
     `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`,
     `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`,
     and `orchestrator/retry-subloop.md`

### Focused bounded rerun

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass (`20 examples, 0 failures`)

Observed result: the focused rerun stayed green and bounded to the existing
helper/test packet. The selected non-local
`schemeAliasBaseLikeFallback False` packet remained green alongside the
preserved local contrasts and source guard; no broader subject was exercised or
reopened.

### Fresh full repo gate

- `cabal build all && cabal test`
  -> pass (`1141 examples, 0 failures`)

Observed result: the full repository gate stayed green after the focused rerun,
so the accepted `N5` slice remains currently green under both the bounded block
and the full repo verification gate.

## Accepted `N5` Continuity Recheck

Continuity commands re-ran against the accepted `N5` artifact and reviewer
record:

- `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-072/review-record.json >/dev/null`
  -> pass
- `rg -n 'rootNonLocalSchemeAliasBaseLike|schemeAliasBaseLikeFallback False|schemeAliasBaseLikeFallback True|cabal build all && cabal test|20 examples, 0 failures' docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md orchestrator/rounds/round-072/review.md`
  -> pass; hits reconfirmed:
     explicit `rootNonLocalSchemeAliasBaseLike` proof references,
     preserved `schemeAliasBaseLikeFallback True` contrast,
     selected `schemeAliasBaseLikeFallback False` packet,
     focused green `20 examples, 0 failures`,
     and full-gate green `cabal build all && cabal test` /
     `1141 examples, 0 failures`
- accepted-predecessor continuity assertion over `round-066` through
  `round-072`
  -> pass (`L1`, `L2`, `N1`, `N2`, `N3`, `N4`, `N5` all
     `accepted finalize authoritative`)
- reviewer-readable `round-072` `review-record.json` summary
  -> pass (`stage_result = pass`, `retry_reason = none`,
     `fix_hypothesis = none`, all recorded `checks` entries = `pass`)

This continuity recheck keeps the active interpretation unchanged:

- the accepted `N5` slice is still exactly one explicit non-local proof slice;
- the active `N3` contract still binds that slice;
- the preserved local lanes remain continuity context only; and
- no fresh green result here authorizes replay reopen, `MLF.Elab.Inst`,
  `InstBot`, `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `ResultType.View`, any other fallback family, any different solver/pipeline
  subject, cross-family search, implicit unfolding, equi-recursive reasoning,
  cyclic encoding, graph-cycle exceptions, multi-SCC support, second-interface
  work, or fallback widening.

## Docs-Only Diff Evidence

Post-artifact diff checks were re-run so the reviewer can confirm that this
round stayed docs-only while reverifying code:

- `git diff --name-only`
  -> pass (no tracked diffs)
- `git diff --name-only -- src test src-public app mlf2.cabal`
  -> pass (no tracked code/public/exe/Cabal diffs)
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> pass (no tracked non-doc/non-orchestrator diffs)
- `git status --short --untracked-files=all`
  -> bounded docs-only round payload:
     `?? docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-verification-gate.md`
     plus the pre-existing untracked round inputs
     `?? orchestrator/rounds/round-073/plan.md` and
     `?? orchestrator/rounds/round-073/selection.md`

No non-doc diff appeared under `src/`, `test/`, `src-public/`, `app/`, or
`mlf2.cabal`. The round therefore stayed within the authorized docs-only
boundary.

## Stability Conclusion

Fresh verifier-visible evidence shows the exact accepted `N5` slice remains
green and bounded:

- the live source packet still carries the explicit
  `rootNonLocalSchemeAliasBaseLike` proof and the dedicated same-lane `targetC`
  consumer;
- the live focused test packet still distinguishes the selected non-local
  `schemeAliasBaseLikeFallback False` lane from the preserved local
  `schemeAliasBaseLikeFallback True` and adjacent local continuity lanes;
- the focused `ARI-C1` block reran green at `20 examples, 0 failures`;
- the fresh full repo gate reran green at `1141 examples, 0 failures`; and
- the accepted `N3` / `N4` / `N5` authority chain remains intact without any
  widening inference.

No blocker was encountered in this `attempt-1`.

## Explicit Non-Authorization

This artifact does not authorize:

- `N7`;
- any code or test patch;
- any public-surface or executable change;
- any `mlf2.cabal` edit;
- roadmap/controller edits;
- bug-tracker edits;
- predecessor-history rewrites; or
- any new target, second implementation slice, or broader recursive-inference
  subject.

This artifact records only current verification/evidence for the exact
accepted `N5` non-local proof slice while preserving the active accepted `N3`
contract boundaries unchanged.
