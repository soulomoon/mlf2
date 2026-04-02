# Round 173 Plan

- Round: `round-173`
- Roadmap: `2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap` / `rev-001`
- Item: `item-1`
- Retry: `null`
- Execution shape: docs-only, item-1-only, serial, aggregate freeze

## Objective

Publish one docs-only item-1 freeze artifact at:

`docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md`

The artifact must do exactly four things for the new follow-on family:

- bind the predecessor authority chain from the March baseline/capability
  ledger through the accepted April 1 / April 2 settlement-and-handoff chain;
- preserve `sameLaneAliasFrameClearBoundaryExpr` as settled predecessor truth
  only, not as the live packet;
- freeze one fresh exact same-lane retained-child packet,
  `sameLaneDoubleAliasFrameClearBoundaryExpr`, together with its current exact
  live read on `runPipelineElab` and `runPipelineElabChecked`; and
- freeze the item-2 success bar plus one fail-closed writable slice for a
  bounded current-architecture follow-on.

Current planning read: the next honest packet is one extra clear-boundary
alias binder `keep` beyond the settled one-alias packet
`sameLaneAliasFrameClearBoundaryExpr`. The current live read for that
double-alias packet is still the shared top-level blocker
`PipelineTypeCheckError (TCLetTypeMismatch ...)` on both authoritative
entrypoints, so the frozen future slice should stay narrow and
`TermClosure`-centered rather than reopening broader routing, fallback, or
public-surface files.

## Write Scope

Implementer-owned writes for this round are limited to:

- `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md`
- `orchestrator/rounds/round-173/implementation-notes.md`

Do not modify `selection.md`, `review.md`, `orchestrator/state.json`,
`orchestrator/roadmaps/**`, prior accepted freeze/settlement/decision docs,
`TODO.md`, `implementation_notes.md`, `Bugs.md`, or any file under `src/`,
`src-public/`, `app/`, `test/`, or `mlf2.cabal` in this round.

## Sequential Plan

1. Author the stage contract and predecessor authority ledger in
   `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md`.
   - Mark the artifact as item `1`, `attempt-1`, `retry: null`, docs-only,
     freeze-only, and pre-implementation.
   - Carry forward these exact predecessor authorities:
     `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`,
     `docs/plans/2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision.md`,
     `docs/plans/2026-03-28-same-lane-retained-child-representative-gap-successor-authority-exact-subject-success-bar-and-writable-slice-freeze.md`,
     `docs/plans/2026-03-29-same-lane-alias-frame-representative-gap-post-item-2-settlement-surface-and-repo-impact-read.md`,
     `docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`,
     and
     `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`.
   - State explicitly that the inherited production boundary remains
     explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph /
     no-fallback, and that this round does not authorize cyclic search,
     multi-SCC search, equi-recursive reasoning, fallback widening, a second
     interface, or any repo-level readiness claim.
   - Preserve `sameLaneAliasFrameClearBoundaryExpr` as one settled
     `narrow success` predecessor packet only, while broader `P3` / `P4` /
     `P6` and repo-level readiness remain unresolved.

2. Freeze the next exact representative-gap packet in that same artifact and
   anchor it to existing read-only docs/tests/source.
   - Name the packet exactly:
     `sameLaneDoubleAliasFrameClearBoundaryExpr`.
   - Freeze this exact source expression:

     ```haskell
     sameLaneDoubleAliasFrameClearBoundaryExpr =
         ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
             (ELet "hold" (EVar "k")
                 (ELet "keep" (EVar "hold")
                     (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "keep")) (EVar "u"))))
     ```

   - Preserve the recursive annotation exactly as:
     `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.
   - Use these read-only anchors when describing why this is the right next
     packet:
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
     `test/Research/P5ClearBoundarySpec.hs`,
     `test/PipelineSpec.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`, and
     `src/MLF/Elab/TermClosure.hs`.
   - Bind the route under audit to the same same-lane retained-child route
     already carried by the current architecture:
     `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`.
   - State explicitly why this packet is distinct from
     `sameLaneAliasFrameClearBoundaryExpr`:
     it adds one more clear-boundary same-lane alias binder `keep` beyond the
     already accepted `hold` packet.
   - State explicitly why it is distinct from the settled exact `P5` contrast:
     it does not route through `nestedForallContrastExpr` and does not add the
     quantified-crossing `id` mediation carried by
     `test/Research/P5ClearBoundarySpec.hs`.

3. Record the current exact live read for the double-alias packet in that same
   artifact.
   - Reproduce the read with the authoritative public entrypoints only:
     `runPipelineElab` and `runPipelineElabChecked`.
   - Use the concrete non-writing command in the verification section below to
     print both results from `cabal repl mlf2-test`.
   - Record the exact shared read honestly as the full rendered
     `Left (PipelineTypeCheckError (TCLetTypeMismatch ...))` output for both
     entrypoints, not a paraphrase and not a broader family claim.
   - Keep the live read packet-bounded: one exact packet only, not evidence of
     broad `P3` / `P4` / `P6` settlement.
   - Use `orchestrator/rounds/round-170/implementation-notes.md` and
     `orchestrator/rounds/round-170/review.md` only as read-only packet-local
     precedent for why the immediately adjacent accepted one-alias fix lived in
     `preserveRetainedChildAuthoritativeResult` in
     `src/MLF/Elab/TermClosure.hs`; do not treat that precedent as already
     proving the new packet’s root cause.

4. Freeze the exact item-2 success bar and one fail-closed writable slice in
   that same artifact.
   - Limit lawful item-2 outcomes for
     `sameLaneDoubleAliasFrameClearBoundaryExpr` to exactly:
     `narrow success`,
     `fail-closed`, or
     `narrower current-architecture blocker`.
   - State explicitly that even a positive item-2 result for this packet still
     does not settle general `P3`, `P4`, or `P6`, and still does not justify a
     repo-level readiness claim.
   - Freeze the future production/test writable slice to exactly:
     `src/MLF/Elab/TermClosure.hs`,
     `test/PipelineSpec.hs`, and
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`.
   - Justify that narrow slice from the last accepted adjacent packet fix:
     the accepted one-alias packet cleared by changing
     `preserveRetainedChildAuthoritativeResult` in
     `src/MLF/Elab/TermClosure.hs` plus focused packet tests, so the next
     bounded attempt must stay in that same narrow lane unless a later accepted
     family explicitly reopens scope.
   - State explicitly that the following remain read-only / blocked for item-2:
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback.hs`,
     `src/MLF/Elab/Run/Scope.hs`,
     `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`,
     `src-public/MLF/Pipeline.hs`,
     `test/Main.hs`,
     `mlf2.cabal`,
     all `src/MLF/Constraint/**`,
     cyclic search,
     multi-SCC search,
     fallback widening,
     second-interface work,
     second-family openings, and any edit outside the frozen slice.
   - State explicitly that if item-2 cannot clear or diagnose the packet inside
     that slice, it must stop at `narrower current-architecture blocker`
     rather than widening the slice in place.
   - State explicitly that any later item-2 round touching the authorized
     source/test files must rerun `cabal build all && cabal test`.

5. Mirror the docs-only freeze outcome in
   `orchestrator/rounds/round-173/implementation-notes.md`.
   - Record that the round is docs-only and item-1-only.
   - Record the exact selected packet name
     `sameLaneDoubleAliasFrameClearBoundaryExpr`.
   - Record the current exact live read in substance:
     both authoritative entrypoints still render the same
     `PipelineTypeCheckError (TCLetTypeMismatch ...)` blocker.
   - Record the exact item-2 success bar and the exact narrow writable slice
     centered on `src/MLF/Elab/TermClosure.hs`.
   - State explicitly that no code/test files, roadmap files, or controller
     state were changed in this round.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `python3 - <<'PY'
import pathlib, sys
artifact = pathlib.Path(
    'docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md'
).read_text()
notes = pathlib.Path('orchestrator/rounds/round-173/implementation-notes.md').read_text()
artifact_tokens = [
    '2026-03-14-automatic-recursive-inference-baseline-contract',
    '2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus',
    '2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision',
    '2026-03-28-same-lane-retained-child-representative-gap-successor-authority-exact-subject-success-bar-and-writable-slice-freeze',
    '2026-03-29-same-lane-alias-frame-representative-gap-post-item-2-settlement-surface-and-repo-impact-read',
    '2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze',
    '2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read',
    '2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane',
    'sameLaneAliasFrameClearBoundaryExpr',
    'sameLaneDoubleAliasFrameClearBoundaryExpr',
    'sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC',
    'runPipelineElab',
    'runPipelineElabChecked',
    'PipelineTypeCheckError',
    'TCLetTypeMismatch',
    'narrow success',
    'fail-closed',
    'narrower current-architecture blocker',
    'src/MLF/Elab/TermClosure.hs',
    'test/PipelineSpec.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback',
]
for token in artifact_tokens:
    if token not in artifact:
        print(f'artifact missing token: {token}')
        sys.exit(1)
notes_tokens = [
    'docs-only',
    'item-1-only',
    'sameLaneDoubleAliasFrameClearBoundaryExpr',
    'PipelineTypeCheckError',
    'TCLetTypeMismatch',
    'src/MLF/Elab/TermClosure.hs',
    'no code/test files',
]
for token in notes_tokens:
    if token not in notes:
        print(f'implementation-notes missing token: {token}')
        sys.exit(1)
print('ROUND173_ITEM1_TOKENS_OK')
PY`
- `python3 - <<'PY'
import subprocess, sys, textwrap
script = textwrap.dedent("""\
:set -XOverloadedStrings
import qualified Data.Set as Set
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
import MLF.Frontend.Syntax
import SpecUtil (unsafeNormalizeExpr)
let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
let expr = ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "keep")) (EVar "u"))))
print (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
print (runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
:quit
""")
proc = subprocess.run(
    ['cabal', 'repl', 'mlf2-test'],
    input=script,
    text=True,
    capture_output=True,
)
sys.stdout.write(proc.stdout)
sys.stderr.write(proc.stderr)
if proc.returncode != 0:
    sys.exit(proc.returncode)
needle = 'Left (PipelineTypeCheckError (TCLetTypeMismatch'
count = proc.stdout.count(needle)
if count != 2:
    print(f'expected 2 shared blocker renders, saw {count}', file=sys.stderr)
    sys.exit(1)
print('ROUND173_LIVE_READ_OK')
PY`
- `rg -n 'sameLaneDoubleAliasFrameClearBoundaryExpr|sameLaneAliasFrameClearBoundaryExpr|PipelineTypeCheckError|TCLetTypeMismatch|src/MLF/Elab/TermClosure.hs|test/PipelineSpec.hs|test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs|explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback' docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md orchestrator/rounds/round-173/implementation-notes.md`
- `git diff --check`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md',
    'orchestrator/rounds/round-173/selection.md',
    'orchestrator/rounds/round-173/plan.md',
    'orchestrator/rounds/round-173/implementation-notes.md',
    'orchestrator/state.json',
}
tracked = subprocess.check_output(['git', 'diff', '--name-only'], text=True).splitlines()
untracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [p for p in paths if p not in allowed]
forbidden = [
    p for p in paths
    if p in {'mlf2.cabal', 'TODO.md', 'implementation_notes.md', 'Bugs.md'}
    or p.startswith('src/')
    or p.startswith('src-public/')
    or p.startswith('app/')
    or p.startswith('test/')
    or p.startswith('orchestrator/roadmaps/')
]
if forbidden or extra:
    if forbidden:
        print('FORBIDDEN_PATHS:')
        print('\n'.join(forbidden))
    if extra:
        print('OUT_OF_SCOPE_PATHS:')
        print('\n'.join(extra))
    sys.exit(1)
print('ROUND173_DOCS_ONLY_SCOPE_OK')
PY`
- `if git diff --name-only -- src src-public app test mlf2.cabal | grep -q .; then cabal build all && cabal test; else printf 'skip full cabal gate for docs-only round\n'; fi`
