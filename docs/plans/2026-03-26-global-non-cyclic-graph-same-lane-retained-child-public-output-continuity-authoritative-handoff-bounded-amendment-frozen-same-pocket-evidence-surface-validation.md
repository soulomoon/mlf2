# Global `non-cyclic-graph` Same-Lane Retained-Child Public-Output Continuity Authoritative-Handoff Bounded-Amendment Frozen Same-Pocket Evidence-Surface Validation

Date: 2026-03-26
Round: `round-111`
Roadmap item: `item-3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: exact-pocket validation for the accepted rev-003 bounded
authoritative-handoff amendment on the frozen same-lane `C2` / `C5` / `C7`
pocket only
Artifact kind: canonical docs-only exact-pocket validation record

## Stage Contract Freeze

This artifact implements only roadmap item `3` for `attempt-1` with
`retry: null`.

It reruns only the exact rev-002 item-4 same-pocket evidence surface against
the accepted rev-003 item-2 bounded handoff amendment:

- the same exact selected rows `C2`, `C5`, and `C7`;
- the same exact packet;
- the same exact tuple:
  family `same-lane retained-child`,
  anchor `boundVarTargetRoot`,
  one owner-local retained-child frame,
  route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  and clear-boundary-only status;
- the same exact command set frozen by accepted rev-002 item `4`; and
- the same exact six review-visible surfaces frozen by accepted rev-002
  item `4`.

The exact packet remains:

```haskell
ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
  (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
```

where
`recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.

This artifact does not authorize:

- edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
- roadmap edits;
- `orchestrator/state.json` edits;
- `Bugs.md`, `TODO.md`, `implementation_notes.md`, or `CHANGELOG.md` edits;
- a second implementation round;
- the post-amendment handoff decision owned by item `4`; or
- any widening into a second packet, a second interface, multi-SCC search,
  fallback widening, equi-recursive reasoning, implicit unfolding, or
  repo-level capability claims.

If the frozen reruns had failed, that failure would be a blocker to record
here, not permission to patch source or tests during this round.

## Exact Item-3 Result Summary

The bounded amendment validates successfully on the frozen same-pocket
surface, and only on that surface:

- the helper-visible/internal same-lane route still reconstructs recursive
  structure (`TMu ...`, `containsMu True`);
- the authoritative public output on both `runPipelineElab` and
  `runPipelineElabChecked` now also carries bounded recursive structure on
  the same exact packet and the same one public-entrypoint chain;
- the same clear-boundary and nested-`forall` fail-closed guards remain
  green; and
- the read remains exact-pocket-only, with no subject drift or broadened
  family claim.

This is an exact-pocket validation result only. It does not by implication
settle item `4`, authorize hardening or rollout, or upgrade the repo into a
broader automatic recursive-inference success claim.

## Accepted Authority Chain Carried Forward Without Widening

Fresh continuity checks reconfirmed the accepted chain that still binds this
round:

1. `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
   remains the inherited boundary contract:
   explicit-only production baseline,
   iso-recursive only,
   non-equi-recursive,
   no fallback widening,
   no second interface,
   and no multi-SCC search.
2. `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
   remains the authoritative accepted reopen decision for the bounded
   `non-cyclic-graph` same-family lane only.
3. `docs/plans/2026-03-26-global-non-cyclic-graph-reopened-revision-authority-and-candidate-boundary-freeze.md`
   remains the accepted rev-002 item-1 freeze: one bounded candidate lane
   only, planning-only predecessor authority, and rev-001 items `6`
   through `8` still blocked.
4. `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-reopened-subject-selection.md`
   remains the accepted rev-002 item-2 selection: exactly one live same-lane
   retained-child / public-output continuity pocket, with non-local `C1`
   contrast-only.
5. `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-safety-and-acceptance-contract.md`
   remains the accepted rev-002 item-3 safety contract: exact-pocket-only,
   clear-boundary and fail-closed guards preserved, `iso-recursive = keep`,
   `non-equi-recursive = keep`, and `no-fallback = keep`.
6. `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-pressure-audit-target-and-evaluation-surface-bind.md`
   remains the accepted rev-002 item-4 bind: the exact rows, packet,
   commands, module set, and six review-visible surfaces remain frozen.
7. `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-amendment-lane-open-or-stop-decision.md`
   remains the accepted rev-002 item-5 authority that opened exactly one
   bounded same-family amendment lane and no broader search.
8. `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-architecture-amendment-contract-and-writable-slice-freeze.md`
   remains the accepted rev-003 item-1 freeze: exact handoff slice, exact
   writable boundary, exact read-only audit anchors.
9. `orchestrator/rounds/round-109/review-record.json`
   remains the authoritative accepted item-1 finalize record.
10. `orchestrator/rounds/round-110/review-record.json`
    remains the authoritative accepted item-2 finalize record: the bounded
    handoff amendment landed only in `Pipeline.hs`, `TermClosure.hs`, and
    `PipelineSpec.hs`, and the old public collapse is no longer the expected
    exact-pocket outcome.

These records remain predecessor authority only. This item-3 validation does
not reopen the handoff amendment design question or preempt the later item-4
decision gate.

## Read-Only Live Anchor Evidence

### `Fallback.hs` same-pocket route anchors remain unchanged

Command:

- `rg -n 'boundVarTargetRoot =|boundHasForallFrom start0 =|sameLaneLocalRetainedChildTarget =|keepTargetFinal =|case sameLaneLocalRetainedChildTarget of|not hasForall' src/MLF/Elab/Run/ResultType/Fallback.hs`

Observed live anchors:

- `boundVarTargetRoot = canonicalFinal (schemeBodyTarget targetPresolutionView rootC)` at line `558`
- `boundHasForallFrom start0 =` at line `563`
- `not hasForall` guard at line `692`
- `sameLaneLocalRetainedChildTarget =` at line `697`
- `keepTargetFinal =` at line `701`
- `case sameLaneLocalRetainedChildTarget of` at line `729`

These anchors show the selected same-lane packet is still admitted through
the same exact route and clear-boundary-only rule set. Item `3` did not
change the route identity.

### `ResultType.hs` / `Pipeline.hs` handoff anchors remain on the same slice

Command:

- `rg -n 'computeResultTypeFallback|runPipelineElab =|runPipelineElabChecked =|runPipelineElabWith|termClosed|checkedAuthoritative =|typeCheck termClosed' src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/Pipeline.hs`

Observed live anchors:

- `computeResultTypeFallback` at `ResultType.hs:72`, `:77`, and `:106`
- `runPipelineElab =` at `Pipeline.hs:82`
- `runPipelineElabChecked =` at `Pipeline.hs:85`
- `runPipelineElabWith` at `Pipeline.hs:94` and `:99`
- `termClosed0 =` at `Pipeline.hs:171`
- `termClosed =` at `Pipeline.hs:186`
- `checkedAuthoritative =` at `Pipeline.hs:190`
- `typeCheck termClosed` at `Pipeline.hs:191`

This confirms the selected amendment remained on the exact frozen
`runPipelineElabWith` / `checkedAuthoritative` / `typeCheck termClosed`
authoritative-handoff slice.

### One public-entrypoint chain remains intact

Command:

- `rg -n 'runPipelineElab|runPipelineElabChecked|runPipelineElabWith' src/MLF/Elab/Run.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs`

Observed live anchors:

- `src/MLF/Elab/Run.hs:2-5`
- `src/MLF/Elab/Pipeline.hs:39-42`, `:88-91`, and comment at `:95`
- `src-public/MLF/Pipeline.hs:45-48` and `:79-82`

The selected pocket still runs through one public-entrypoint chain only. No
second public interface was introduced.

### Focused same-pocket test anchors remain exact-pocket-only

Command:

- `rg -n 'same-lane retained-child exact packet|same-lane local TypeRef root|same local TypeRef lane|nested forall boundary|edge 3 authoritative instantiation' test/PipelineSpec.hs`

Observed live anchors in `PipelineSpec.hs`:

- `keeps retained-child fallback recursive through a same-lane local TypeRef root` at line `1495`
- `same-lane retained-child exact packet clears Phase 6 elaboration` at line `1572`
- `same-lane retained-child exact packet authoritative public output stays forall identity` at line `1586`
- `keeps retained-child lookup bounded to the same local TypeRef lane` at line `1611`
- `keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary` at line `1634`

The frozen `rg` surface still points at the same exact same-pocket block.
The authoritative-instantiation test remains part of the exact command set,
but its anchor lives in `test/ElaborationSpec.hs`; the frozen rev-002 item-4
`rg` command did not include that file, so this round preserves that command
surface exactly and records the instantiation evidence through its separate
focused test rerun below.

One important exact-pocket continuity note:

- the test label
  `same-lane retained-child exact packet authoritative public output stays forall identity`
  remains a legacy string anchor in `PipelineSpec.hs`;
- accepted round `110` changed the test body, not the string anchor;
- the current test now fails on the old `TForall "a" Nothing (TVar "a")`
  collapse and requires `containsMu True` on both public entrypoints for
  this same packet only.

That preserves the frozen command surface while changing the exact-pocket
behavior under test.

## Exact-Pocket Replay Evidence

Command surface rerun:

- `cabal repl mlf2-test --repl-options=-ignore-dot-ghci`
- `:module + *PipelineSpec`

Inside that REPL, the replay reused the same exact helper-visible same-lane
rewiring anchored in `PipelineSpec.hs` for the frozen packet only and then
printed the internal fallback result, `containsMu`, and both authoritative
public output types.

Replay output:

```text
fallbackTy:
TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))
fallbackContainsMu:
True
runPipelineElab:
Right (TForall "a" Nothing (TArrow (TVar "t31") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))))
runPipelineElabChecked:
Right (TForall "a" Nothing (TArrow (TVar "t31") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))))
```

This replay proves the exact bounded change that item `3` exists to check:

- the helper-visible/internal path still exposes recursive structure
  reviewer-visibly as `TMu ...` with `containsMu True`; and
- the authoritative public output on both public entrypoints now also
  carries bounded recursive structure on the same exact packet, instead of
  the old `Right (TForall "a" Nothing (TVar "a"))` collapse.

## Frozen Focused Verification Results

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-111`

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  - Result: passed with `1 example, 0 failures`.

These frozen reruns jointly show:

- the same helper-visible/internal continuity is preserved;
- the same owner-local / same-lane bound is preserved;
- the same nested-`forall` wrapper still fails closed;
- the exact packet still clears the earlier elaboration boundary;
- the legacy-named authoritative public-output test now passes with the new
  post-amendment recursive public result; and
- the exact same-pocket authoritative instantiation evidence remains green.

## Exact Six Review-Visible Output Surfaces

| Surface | Exact current read | Validation result |
| --- | --- | --- |
| Solver admission state | The same family, `boundVarTargetRoot` anchor, owner-local frame, route, and clear-boundary-only status remain visible in `Fallback.hs` at lines `558`, `563`, `692`, `697`, `701`, and `729`. | `preserved` |
| Elaboration handoff / result state | The exact packet still clears the earlier elaboration boundary, confirmed by the focused Phase-6 test. | `preserved` |
| Reification / reconstruction state | The same helper-visible route still reconstructs a `TMu`-bearing type term on the exact packet. | `preserved` |
| Internal output surface | `fallbackTy` remains recursive and `containsMu True` still holds. | `preserved` |
| Public output surface | Both `runPipelineElab` and `runPipelineElabChecked` now return `Right (TForall "a" Nothing (TArrow (TVar "t31") (TMu ...)))` on the same exact packet. | `changed as intended inside the frozen pocket` |
| Reviewer-visible evidence trail | Reviewers can still point to one exact tuple, one exact packet, one exact route, one public-entrypoint chain, and one bounded before/after public-output shift without alternate interfaces or second packets. | `preserved and strengthened` |

## One Bounded Item-3 Validation Conclusion

Bounded item-3 validation conclusion:
`exact-pocket bounded amendment validated on the frozen same-pocket evidence surface`

Why this is the strongest lawful read:

1. The same exact pocket remains in scope and the same route identity
   remains visible.
2. The helper-visible/internal recursive fact remains present and unchanged
   in kind (`TMu ...`, `containsMu True`).
3. The authoritative public-output surface now also carries bounded
   recursive structure on the same exact packet and the same one-interface
   chain.
4. The same clear-boundary and nested-`forall` fail-closed guards remain
   green.
5. The evidence is still exact-pocket-only; it does not widen into a second
   packet, second interface, hardening claim, or repo-level capability
   claim.

Item `4` remains later work only. This artifact validates the accepted
bounded amendment on the frozen surface; it does not decide whether the
revision should continue into a later same-family successor revision or stop
without broader rollout.

## Scope Guard

This round remains docs-only. Because item `3` completion is explicitly
bound to the exact rev-002 item-4 command set, broader gates such as
`cabal build all && cabal test` remain intentionally out of scope for this
round-owned artifact. The accepted item-2 round already carried the bounded
code-change full gate; item `3` exists to rerun only the frozen exact-pocket
surface and record the result honestly.
