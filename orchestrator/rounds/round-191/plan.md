# Round 191 Plan

- Round: `round-191`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Retry: `null`
- Execution shape: serial, docs-only, one aggregate positive-family global settlement gate only, no worker fan-out, no source/test/Cabal edits, no concurrent `cabal` jobs

## Objective

Keep this round on exactly one bounded `item-5` slice: author the docs-only
aggregate artifact
`docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`.

That artifact must classify the accepted positive-family evidence into only
three buckets:

- `credible general support`
- `packet-specific folklore`
- `current-architecture blockers`

The admissible evidence inputs are fixed to the already accepted set only:

- the exact `C1` authoritative packet from
  `test/Research/C1AuthoritativeSurfaceSpec.hs`,
  `test/PipelineSpec.hs`, and
  `orchestrator/rounds/round-181/review-record.json`
- the accepted same-lane retained-child alias-through-nonuple chain from
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
  `test/PipelineSpec.hs`, and the accepted `round-182` through `round-190`
  review records
- the fresh decuple fail-closed frontier already recorded by accepted
  `round-190`, especially
  `orchestrator/rounds/round-190/review-record.json` and
  `orchestrator/rounds/round-190/implementation-notes.md`

This round must stay current-architecture-bound and non-widening. It must not
reopen `non-cyclic-graph`, authorize cyclic or multi-SCC search, infer
equi-recursive meaning, open a second interface, widen fallback behavior, move
into `item-6`, or upgrade the accepted evidence into a repo-level readiness
claim.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-191/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `orchestrator/rounds/round-191/selection.md` is the round input and must
  remain untouched.
- `orchestrator/rounds/round-191/` is the round-owned directory. Keep the
  round-local artifact set scoped to this `plan.md`, the one aggregate
  docs artifact, and an optional `implementation-notes.md` only.

The binding strategic boundary remains unchanged:

- explicit-only
- iso-recursive
- non-equi-recursive
- `non-cyclic-graph = unknown`
- no-fallback

The read-only contract chain that must stay visible in the aggregate artifact
is:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
- `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze.md`
- `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`

`Bugs.md` remains read-only predecessor context only. It must not be edited and
must not be reinterpreted as positive-family evidence for this round.

## Write Scope

Implementer-owned writes for this round are limited to:

- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`
- `orchestrator/rounds/round-191/implementation-notes.md` only if a
  round-local note is needed to summarize the exact bounded classification

Do not modify:

- `orchestrator/rounds/round-191/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-191/review.md`
- `orchestrator/rounds/round-191/merge.md`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- `src/**`
- `src-public/**`
- `app/**`
- `test/**`
- `mlf2.cabal`

No new persistent decuple test is authorized. The decuple frontier remains
read-only accepted evidence only.

## Sequential Plan

1. Freeze the exact accepted evidence ledger before drafting the aggregate
   classification; modify no files in this step.
   - Read-only inputs:
     `orchestrator/rounds/round-191/selection.md`,
     `orchestrator/state.json`,
     `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md`,
     `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md`,
     `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md`,
     the March baseline / strategic docs listed above,
     `test/Research/C1AuthoritativeSurfaceSpec.hs`,
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
     `test/PipelineSpec.hs`,
     `orchestrator/rounds/round-181/review-record.json`,
     `orchestrator/rounds/round-181/implementation-notes.md`,
     `orchestrator/rounds/round-182/review-record.json` through
     `orchestrator/rounds/round-190/review-record.json`, and
     `orchestrator/rounds/round-190/implementation-notes.md`.
   - Reconfirm only the focused read-only evidence needed to show the live
     tree still matches the accepted packet set:
     the `C1` authoritative harness, the accepted same-lane retained-child
     representative-gap harness, and the accepted round-190 decuple
     fail-closed record. Reuse the accepted round-190 frontier record instead
     of adding any new persistent decuple assertion.
   - End this step with one fixed three-row evidence ledger only:
     `C1 authoritative packet`,
     `alias-through-nonuple same-lane chain`,
     and `decuple fail-closed frontier`.
   - Verification:
     `python3 -m json.tool orchestrator/state.json >/dev/null`
     `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child representative-gap probes"'`
     `rg -n 'C1|decuple|budget 5|fails closed|authoritative' orchestrator/rounds/round-181/review-record.json orchestrator/rounds/round-181/implementation-notes.md orchestrator/rounds/round-190/review-record.json orchestrator/rounds/round-190/implementation-notes.md`

2. Draft the one aggregate classification artifact and keep it evidence-led,
   bounded, and non-widening.
   - Create
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`.
   - The document must include, in order:
     a stage-contract freeze that keeps the round docs-only and
     non-widening;
     an authority ledger pointing back to item `1` through item `4` and the
     inherited March contracts;
     one evidence-input ledger with exactly the three admissible evidence rows
     from step 1;
     one explicit classification rubric for
     `credible general support`,
     `packet-specific folklore`, and
     `current-architecture blockers`;
     one family-summary matrix covering positive-family pressure rows
     `P2`, `P3`, `P4`, `P5`, and `P6`;
     and one bounded conclusion that keeps `non-cyclic-graph` unresolved and
     keeps repo-level readiness out of scope.
   - In the family-summary matrix, each row must cite only the fixed evidence
     ledger from step 1 and must state why the strongest lawful read lands in
     exactly one of the three permitted buckets. Do not collapse one accepted
     route, one packet chain, or one frontier into broad `P2`-`P6` closure
     without an explicit evidence-backed explanation.
   - The document must keep the decuple row as a frontier read, not as a new
     writable slice, and must keep nested-`forall`, ambiguity, soundness, and
     termination material as supporting boundary context only rather than
     smuggling `item-6` into this round.
   - Verification:
     `test -f docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`
     `rg -n 'credible general support|packet-specific folklore|current-architecture blockers' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`
     `rg -n 'C1|alias-through-nonuple|sameLaneNonupleAliasFrameClearBoundaryExpr|decuple|non-cyclic-graph|repo-level readiness|item-6' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`

3. Verify docs-only scope, exact artifact closure, and round-local hygiene.
   - Keep the authored diff limited to the one aggregate docs artifact and an
     optional `orchestrator/rounds/round-191/implementation-notes.md`.
   - If `implementation-notes.md` is written, keep it to one bounded summary
     of the exact aggregate outcome only; do not restate roadmap history or
     broaden the claim.
   - Run diff hygiene and scope checks. If any `src/`, `src-public/`, `app/`,
     `test/`, `mlf2.cabal`, roadmap, controller-state, `TODO.md`,
     `implementation_notes.md`, or `Bugs.md` edit appears in the round diff,
     stop and remove it rather than widening the round.
   - Verification:
     `git diff --check`
     `python3 - <<'PY'
import subprocess
allowed = {
    'docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md',
    'orchestrator/rounds/round-191/implementation-notes.md',
    'orchestrator/rounds/round-191/plan.md',
    'orchestrator/rounds/round-191/selection.md',
}
tracked = subprocess.check_output(
    ['git', 'diff', '--name-only', '--', 'docs/plans', 'orchestrator/rounds/round-191', 'TODO.md', 'implementation_notes.md', 'Bugs.md', 'orchestrator/roadmaps', 'src', 'src-public', 'app', 'test', 'mlf2.cabal'],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    ['git', 'ls-files', '--others', '--exclude-standard', '--', 'docs/plans', 'orchestrator/rounds/round-191', 'TODO.md', 'implementation_notes.md', 'Bugs.md', 'orchestrator/roadmaps', 'src', 'src-public', 'app', 'test', 'mlf2.cabal'],
    text=True,
).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [p for p in paths if p not in allowed]
if extra:
    raise SystemExit('unexpected round-191 scope escape:\\n' + '\\n'.join(extra))
print('ROUND_191_DOCS_ONLY_SCOPE_OK')
PY`

## Review Focus

The implementer should leave the reviewer one aggregate item-5 artifact that
is easy to approve against `verification.md`:

- the diff stays docs-only
- the evidence ledger is fixed to the accepted `C1` packet, alias-through-
  nonuple same-lane chain, and accepted decuple frontier only
- the artifact distinguishes `credible general support`,
  `packet-specific folklore`, and `current-architecture blockers` explicitly
- the artifact keeps `non-cyclic-graph` unresolved and does not smuggle in a
  repo-level readiness claim or an `item-6` campaign
