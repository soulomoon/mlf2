# Round 090 Implementation Notes

## Change Summary

- Refreshed the canonical docs-only item-2 artifact in
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
  from `attempt-1` / `retry: null` to `attempt-2` under the active retry
  object in `orchestrator/state.json`.
- Removed the rejected `attempt-1` credit path that treated
  `test/PipelineSpec.hs:1693-1698` as public-output evidence for the frozen
  same-lane retained-child pocket. The refreshed artifact now marks that
  unannotated variant as out of pocket and not creditable for this retry.
- Added reviewer-visible exact-pocket replay evidence for the frozen
  `ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`
  packet via `runPipelineElab` and `runPipelineElabChecked` using
  `SpecUtil.unsafeNormalizeExpr`.
- Recorded that both replay entrypoints fail identically on the exact frozen
  packet with
  `Phase 6 (elaboration): PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 3","expansion args=[NodeId {getNodeId = 31}]]`.
- Rebuilt the six-row ledger in strict earliest-break order:
  solver admission remains `satisfied on current evidence`,
  elaboration handoff / result state is now the
  `first actual continuity breakpoint`, and
  reification / reconstruction, internal output surface, public output
  surface, and reviewer-visible evidence trail are all
  `not credited after earlier breakpoint`.
- Kept the exact-pocket `computeResultTypeFallback` / `containsMu True` fact
  only as bounded helper-visible context. The refreshed audit no longer uses
  that helper-visible fact to keep later rows credited after the earlier
  exact-pocket elaboration failure.

## Verification

- exact-pocket replay via `cabal repl mlf2-test`
  - Result: replayed the exact frozen `let k ... let u ... in u` packet with
    `runPipelineElab` and `runPipelineElabChecked`; both returned the same
    `Phase 6 (elaboration): PhiTranslatabilityError [...]` message quoted in
    the canonical artifact.
- `git diff --check`
  - Result: passed in
    `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-090`.
- `python3 -m json.tool orchestrator/state.json >/dev/null`
  - Result: passed.
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  - Result: matched `contract_version: 2` plus the active retry object.
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - Result: passed; the ordered roadmap remains parseable with item `2`
    still pending.
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - Result: passed.
- `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
  - Result: passed.
- `test -f orchestrator/retry-subloop.md`
  - Result: passed.
- `rg -n 'Attempt: `attempt-2`|runPipelineElab|runPipelineElabChecked|ELet "k"|Phase 6 \(elaboration\)|PhiTranslatabilityError|not credited after earlier breakpoint' docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md orchestrator/rounds/round-090/implementation-notes.md`
  - Result: passed; both refreshed docs mention `attempt-2`, the exact frozen
    packet, both replay entrypoints, the Phase 6 elaboration failure, and the
    later-row `not credited after earlier breakpoint` state.
- `rg -n '1693-1698|out of pocket|not creditable' docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
  - Result: passed; the canonical artifact now records the unannotated
    `PipelineSpec.hs:1693-1698` variant only as excluded evidence, not as a
    credited public-output row for this pocket.
- `git diff --name-only -- src test src-public app mlf2.cabal`
  - Result: passed with no output; the diff stayed out of the code / test /
    public / executable / Cabal surface.
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  - Result: passed with no output; no non-doc drift was introduced outside
    the allowed docs/orchestrator surfaces.
- `git diff --name-only -- orchestrator/roadmap.md Bugs.md orchestrator/retry-subloop.md orchestrator/verification.md orchestrator/state.json`
  - Result: reported only `orchestrator/state.json`, which remains
    pre-existing controller-owned drift. No new changes landed on the
    preserved roadmap / bug-tracker / retry-contract / verification surfaces.
- `cabal build all && cabal test`
  - Result: intentionally skipped. This retry is docs-only, and the diff
    stays out of `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`.
