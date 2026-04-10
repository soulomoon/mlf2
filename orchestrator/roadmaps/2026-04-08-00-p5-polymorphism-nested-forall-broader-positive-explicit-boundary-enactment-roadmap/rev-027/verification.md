# Verification Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-027`

## Baseline Checks

Any controller or reviewer observing `rev-027` must satisfy the checks below
before treating the family as complete.

1. **Roadmap lineage, pointer, and preserved-history consistency**
   - Confirm `orchestrator/state.json` resolves the active roadmap bundle when
     controller activation later moves to `rev-027`.
   - Confirm any stale `selection.md` / `review-record.json` observed during
     recovery matches the same
     `roadmap_id`, `roadmap_revision`, and `roadmap_dir`, or else is treated
     as predecessor evidence from an earlier accepted revision.
   - Confirm prior roadmap families and revisions remain unchanged.
   - Confirm stale `rev-022` and `rev-023` remain untouched and unactivated.
   - Confirm accepted `round-219`, merged as `7616109`, accepted
     `round-220`, merged as `ea8db76`, and accepted `round-221`, merged as
     `eaf2256`, are treated as merged lineage rather than as live unmerged
     diffs.
2. **Completed-family discipline**
   - Confirm `rev-027` marks every milestone `[done]`.
   - Confirm there is no remaining `[pending]` or `[in-progress]` milestone
     in `roadmap.md`.
   - Confirm no candidate direction, extracted item, or other unfinished work
     remains active in this revision.
   - Confirm any continuation is routed outside `rev-027` rather than
     reopening this completed family implicitly.
3. **Diff hygiene**
   - Run `git diff --check` on the revision publication diff.
4. **No fresh implementation scope**
   - Reject any attempt to use `rev-027` for new `src/`, `src-public/`,
     `app/`, `test/`, or `mlf2.cabal` work.
   - Historical code-bearing authority remains the accepted `round-220`
     verification record:
     `./scripts/thesis-conformance-gate.sh` and
     `cabal build all && cabal test` with `1365 examples, 0 failures`
     carried by `ea8db76`.
   - `eaf2256` must remain recorded honestly as a docs-only closeout on top
     of that code/test baseline, not as a fresh code-bearing verification run.
5. **Broader-positive boundary discipline**
   - Confirm `sameLaneAliasFrameClearBoundaryExpr` remains predecessor truth
     only.
   - Confirm `sameLaneClearBoundaryExpr` through
     `sameLaneNonupleAliasFrameClearBoundaryExpr` remain the exact explicit
     broader-positive frontier already earned on both authoritative
     entrypoints.
   - Confirm `sameLaneDecupleAliasFrameClearBoundaryExpr` and deeper alias
     shells remain fail-closed outside live positive support.
   - Confirm `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
     `N6 termination-pressure` remain closed.
   - Confirm no cyclic search, multi-SCC widening, equi-recursive
     reinterpretation, fallback rescue, or second interface is silently
     introduced.
6. **Closeout-record honesty**
   - Confirm the milestone-4 closeout cites the merged lineage honestly:
     `7616109` as the octuple anchor,
     `ea8db76` as the nonuple anchor and code/test evidence baseline, and
     `eaf2256` as the docs-only closeout merge.
   - Confirm the closeout records only the repo-facing note sync actually
     merged in `round-221`:
     `TODO.md`,
     `implementation_notes.md`, and
     `CHANGELOG.md`.
   - Confirm `docs/thesis-deviations.yaml` remains unchanged because the
     closeout republishes accepted evidence only.

## Task-Specific Checks

No new milestone-specific round checks remain in `rev-027`.

If a stale round is somehow observed against this revision, verification must
first prove that the dispatch is invalid for a completed roadmap family
before any review is considered lawful.

## Approval Criteria

`rev-027` is correct only when all applicable baseline checks pass and the
revision records family completion honestly without reopening merged work.

The only successful controller outcome on this revision is:

- the roadmap has no `[pending]` or `[in-progress]` milestone;
- there are no live rounds; and
- the controller can advance to terminal completion or to a separate fresh
  successor surface outside this family.

Any attempt to use `rev-027` for fresh implementation, fresh closeout
publication, or fresh boundary expansion must be rejected as out of contract.
