# Round 193 Plan

- Round: `round-193`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-7`
- Retry: `null`
- Execution shape: serial, docs-only, aggregate-only, one repo-level readiness / architecture decision artifact only, no worker fan-out, no concurrent `cabal` jobs

## Objective

Keep this round on exactly one bounded `item-7` slice: author the docs-only
aggregate decision artifact
`docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`.

That artifact must consume only the accepted item-4 readiness contract plus
the accepted item-5 and item-6 aggregate artifacts as the direct decision
ledger, then record exactly one roadmap-authorized end-state:

- `repo-level readiness reached inside the current architecture`
- `continue-bounded`
- `explicit boundary-revision candidate`

Current planning read: the direct accepted ledger does not presently support a
repo-level readiness claim, because item `5` still leaves `P2` at
`packet-specific folklore` and `P5` at `current-architecture blockers` while
item `6` keeps `N1`, `N2`, and `N6` at `fail-closed rejection`. The artifact
must still evaluate all three lawful outcomes explicitly, but it should fail
closed unless the direct ledger honestly proves a stronger read.

This round must stay docs-only, aggregate-only, current-architecture-only,
and non-widening. It must not edit production code, tests, public facades,
executables, `mlf2.cabal`, roadmap files, controller state, `TODO.md`,
`implementation_notes.md`, or `Bugs.md`. It must not reopen `N3` through
`N5`, cyclic search, multi-SCC search, equi-recursive reasoning, fallback
widening, or a second interface. It must not blur multiple end-states
together.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-193/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `orchestrator/rounds/round-193/selection.md` is the round input and must
  remain untouched.
- `orchestrator/rounds/round-193/` is the round-owned directory. Keep the
  round-local artifact set scoped to this `plan.md`, the one aggregate
  decision artifact, and an optional `implementation-notes.md` only.

The inherited boundary remains controlling:

- explicit-only
- iso-recursive
- non-equi-recursive
- `non-cyclic-graph = unknown`
- no-fallback

Historical posture remains context only, not a controlling decision input for
this round:

- the accepted March strategic posture
  `continue within the current architecture`
- the accepted April handoff
  `continue-bounded`

This round must reevaluate the family from the fresh accepted item-4 / item-5
/ item-6 ledger instead of reusing those predecessor outcomes by inertia.

## Write Scope

Implementer-owned writes for this round are limited to:

- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`
- `orchestrator/rounds/round-193/implementation-notes.md`
  only if a round-local summary is needed

Do not modify:

- `orchestrator/rounds/round-193/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-193/review.md`
- `orchestrator/rounds/round-193/merge.md`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- any accepted predecessor `docs/plans/**` artifact
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `mlf2.cabal`

No source, test, Cabal, or roadmap edit is authorized in this round. If the
decision starts depending on fresh implementation work, fresh corpus
expansion, or a roadmap amendment to remain honest, stop at the exact
aggregate decision that the accepted ledger already supports.

## Sequential Plan

1. Freeze the exact direct decision ledger before drafting the artifact;
   modify no files in this step.
   - Read-only inputs:
     `orchestrator/rounds/round-193/selection.md`,
     `orchestrator/state.json`,
     `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md`,
     `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md`,
     `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md`,
     `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`,
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`,
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`,
     and, only if acceptance provenance needs to be rechecked,
     `orchestrator/rounds/round-180/review-record.json`,
     `orchestrator/rounds/round-191/review-record.json`, and
     `orchestrator/rounds/round-192/review-record.json`.
   - End this step with one fixed direct decision ledger only:
     the item-4 authoritative-surface success bar and lawful outcome
     vocabulary;
     the item-5 aggregate classifications
     `P3` / `P4` / `P6` = `credible general support`,
     `P2` = `packet-specific folklore`,
     `P5` = `current-architecture blockers`;
     and the item-6 aggregate classifications
     `N1` / `N2` / `N6` = `fail-closed rejection`.
   - Keep the March and April posture tokens above visible as predecessor
     context only. They may explain why this round exists, but they must not
     be reused as the decision itself.
   - The step-1 exit question is narrow: does the direct accepted ledger now
     support
     repo-level readiness,
     `continue-bounded`, or an
     explicit boundary-revision candidate?
     The current expected answer is that repo-level readiness is blocked,
     `continue-bounded` is the strongest honest read, and any
     boundary-revision candidate remains weaker unless the direct ledger
     proves that a named inherited boundary is now the strongest blocker.
   - Verification:
     `python3 -m json.tool orchestrator/state.json >/dev/null`
     `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"`
     `rg -n 'stable visible persistence|admitted but not reconstruction-visible / blocker debt|fail-closed rejection|runPipelineElab|runPipelineElabChecked' docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`
     `rg -n 'packet-specific folklore|credible general support|current-architecture blockers|P2|P3|P4|P5|P6' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`
     `rg -n 'fail-closed rejection|N1|N2|N6|bounded Conclusion|repo-level readiness' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`

2. Draft the one aggregate item-7 readiness / architecture decision artifact
   and keep it tied to the fixed step-1 ledger only.
   - Create
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`.
   - The document must stay item-7-only, docs-only, aggregate-only,
     current-architecture-only, and non-widening. It must not republish item
     `4`, item `5`, or item `6` as if they were still open work, and it must
     not mutate roadmap ordering or controller state.
   - The document must include, in order:
     a stage-contract freeze;
     one direct decision ledger grounded in item `4`, item `5`, and item `6`
     only;
     one outcome-evaluation matrix covering exactly the three lawful
     end-states;
     one section naming exactly one authoritative end-state decision;
     one section naming exactly one next lawful handoff / successor move
     consistent with that decision;
     and one non-claims section.
   - In the direct decision ledger:
     tie the item-4 readiness contract to the item-5 and item-6 aggregate
     classifications explicitly;
     keep `runPipelineElab`, `runPipelineElabChecked`, and the matching
     internal / public pipeline facades as the controlling readiness surfaces;
     and treat the March / April posture only as predecessor contrast if
     mentioned at all.
   - In the outcome-evaluation matrix:
     evaluate
     `repo-level readiness reached inside the current architecture`
     against the item-4 representative-authoritative-surface bar and reject
     it unless the accepted aggregate ledger now honestly settles `P2`
     through `P6` plus `N1`, `N2`, and `N6`;
     evaluate `continue-bounded` against the unresolved semantic families
     still left by the accepted aggregates; and
     evaluate `explicit boundary-revision candidate` only against a named
     inherited boundary that the accepted item-5 / item-6 ledger now makes
     stronger than a bounded continuation.
   - If `continue-bounded` is selected, name the unresolved semantic families
     explicitly. At minimum, preserve:
     `P2 non-local-propagation` and
     `P5 polymorphism-nested-forall`.
     Then name one precise next lawful successor move only:
     a planning-only successor gate that freezes the exact follow-on lane for
     the unresolved `P5` family and decides whether the current blocker read
     remains inside the inherited architecture or graduates into a later
     explicit boundary-revision candidate.
   - If an `explicit boundary-revision candidate` is selected instead, name
     the exact boundary under pressure and explain why the accepted aggregate
     ledger now makes revision the strongest honest read rather than a future
     risk. Do not imply that revision is selected merely because
     `non-cyclic-graph` remains `unknown`.
   - Do not authorize implementation, hardening, or roadmap-update work in
     the artifact. The handoff must stay planning-only and bounded.
   - Verification:
     `test -f docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`
     `rg -n 'repo-level readiness reached inside the current architecture|continue-bounded|explicit boundary-revision candidate' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`
     `rg -n 'P2|P5|stable visible persistence|packet-specific folklore|current-architecture blockers|fail-closed rejection|runPipelineElab|runPipelineElabChecked' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`

3. Verify docs-only scope, exact artifact closure, and round-local hygiene.
   - Keep the authored diff limited to the one aggregate decision artifact
     and an optional `orchestrator/rounds/round-193/implementation-notes.md`.
   - If `implementation-notes.md` is written, keep it to one bounded summary
     of the exact item-7 outcome and the single next lawful handoff only. Do
     not restate roadmap history or broaden the claim.
   - Run diff hygiene and scope checks. If any `src/`, `src-public/`, `app/`,
     `test/`, `mlf2.cabal`, roadmap, controller-state, `TODO.md`,
     `implementation_notes.md`, or `Bugs.md` edit appears in the round diff,
     stop and remove it rather than widening the round.
   - Verification:
     `git diff --check`
     `python3 - <<'PY'
import subprocess
allowed = {
    'docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md',
    'orchestrator/rounds/round-193/implementation-notes.md',
    'orchestrator/rounds/round-193/plan.md',
    'orchestrator/rounds/round-193/selection.md',
}
tracked = subprocess.check_output(
    ['git', 'diff', '--name-only', '--', 'docs/plans', 'orchestrator/rounds/round-193', 'TODO.md', 'implementation_notes.md', 'Bugs.md', 'orchestrator/roadmaps', 'orchestrator/state.json', 'src', 'src-public', 'app', 'test', 'mlf2.cabal'],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    ['git', 'ls-files', '--others', '--exclude-standard', '--', 'docs/plans', 'orchestrator/rounds/round-193', 'TODO.md', 'implementation_notes.md', 'Bugs.md', 'orchestrator/roadmaps', 'orchestrator/state.json', 'src', 'src-public', 'app', 'test', 'mlf2.cabal'],
    text=True,
).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [p for p in paths if p not in allowed]
if extra:
    raise SystemExit('unexpected round-193 scope escape:\\n' + '\\n'.join(extra))
print('ROUND_193_DOCS_ONLY_SCOPE_OK')
PY`

## Review Focus

The implementer should leave the reviewer one aggregate item-7 artifact that
is easy to approve against `verification.md`:

- the diff stays docs-only
- the direct decision ledger is fixed to accepted item `4`, item `5`, and
  item `6`
- the artifact records exactly one explicit end-state
- any readiness, `continue-bounded`, or boundary-revision read stays scoped
  exactly to the accumulated evidence
- the artifact names exactly one bounded next lawful move and does not
  pre-authorize implementation or roadmap edits
