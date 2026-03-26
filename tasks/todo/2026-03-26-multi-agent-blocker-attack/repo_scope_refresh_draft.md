# Draft Repo-Scope Refresh Packet After `rev-004`

Status: draft only. Non-authoritative. Docs-only input for a later main-rollout
revision packet. This file does not change controller state, roadmap truth, or
accepted predecessor artifacts.

## Latest Repo-Scope Status

- The inherited repo-level baseline is still unchanged:
  explicit-only production support, iso-recursive meaning, non-equi-recursive
  semantics, `non-cyclic-graph` boundary, and `no-fallback`.
- The last accepted repo-scope aggregate gate is still
  `reopen the non-cyclic-graph revision question`
  from the March 26 global keep-vs-reopen decision gate, but that gate consumed
  the pre-amendment representative matrix and the older pre-amendment
  `C2` / `C5` / `C7` public-collapse read.
- `rev-003` repaired the exact same-lane `C2` / `C5` / `C7` pocket on the
  authoritative public path and validated that bounded result on the frozen
  exact-pocket surface.
- `rev-004` then recorded and validated that repaired exact-pocket read on new
  bounded settlement surfaces, preserved predecessor immutability, and selected
  `stop after bounded settlement` for the exact-pocket lane only.
- Repo-scope aggregate artifacts are therefore stale relative to the repaired
  exact pocket: the repair is accepted, but the representative matrix and the
  global keep-vs-reopen gate have not yet been rerun against it.

## What `rev-003` Changed

- One bounded implementation/result path changed:
  the exact same packet no longer collapses to
  `Right (TForall "a" Nothing (TVar "a"))`
  on the two authoritative public entrypoints and now carries bounded recursive
  structure on that one route.
- The change stayed exact-pocket-only:
  same packet, same owner-local retained-child frame, same
  `boundVarTargetRoot` anchor, same
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` route.

## What `rev-004` Changed

- No new implementation change was introduced.
- One new bounded post-amendment settlement ledger and one matching validation
  surface were published for the repaired exact pocket.
- The older pre-amendment matrix and settlement dossiers were kept as immutable
  historical evidence rather than rewritten in place.
- The exact-pocket handoff selected `stop after bounded settlement`, which
  closed the rev-004 same-pocket settlement lane.

## What `rev-003` / `rev-004` Did Not Change

- They did not refresh the repo-level representative family matrix.
- They did not rerun the global `keep` vs `reopen` gate.
- They did not change the repo-level baseline or convert the repaired pocket
  into a broad same-family or repo-level capability claim.
- They did not change the last accepted reads for rows outside the repaired
  pocket (`P1-row`, `C1`, `C3`, `C4`, `C6`) absent a new repo-scope refresh.

## Proposed Bounded Next Repo-Scope Sequence

1. Freeze one docs-only repo-scope successor boundary after `rev-004`.
   Preserve predecessor immutability explicitly:
   the March 26 matrix and gate stay historical evidence, and the new revision
   writes only new repo-scope refresh surfaces.
2. Publish one refreshed representative family-matrix settlement surface.
   Re-read the accepted matrix with the repaired `C2` / `C5` / `C7` pocket
   carried forward from `rev-003` / `rev-004`, while leaving unchanged rows on
   their last accepted evidence.
3. Rerun one docs-only global `keep` vs `reopen` gate on that refreshed
   matrix.
   Use the unchanged baseline/capability contracts and decide whether the
   repaired `C7` public-output result changes the repo-scope outcome or only
   narrows the remaining blocker set.
4. If needed, publish one short post-gate handoff note.
   Either stop at the refreshed repo-scope decision or name one next bounded
   blocker family. Do not rewrite the earlier matrix or gate in place.

## Guardrails

- This draft is not an authoritative controller artifact.
- Do not reinterpret `rev-004`'s `stop after bounded settlement` token as a
  repo-scope stop; it closes the exact-pocket settlement lane, not the later
  repo-scope refresh question.
- Do not widen into cyclic graphs, multi-SCC search, second interfaces,
  fallback behavior, or broad capability narration unless a later accepted
  repo-scope gate explicitly does so.
