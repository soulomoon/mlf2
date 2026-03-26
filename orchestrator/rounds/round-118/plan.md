# Round 118 Plan (`item-2` Refreshed Representative Family-Matrix Settlement Surface)

## Objective

Execute roadmap item `2` only:
publish and validate one refreshed repo-scope representative family-matrix
settlement surface after the accepted post-rev-004 successor-boundary freeze.

This round is `attempt-1` with `retry: null`. The plan stays inside one
actionable slice only:
the refreshed representative family-matrix settlement surface, including
bounded provenance validation for any fresh `C1` / `P5` evidence that the
round chooses to republish.

## Binding Boundary Carried Forward

The following accepted truth remains fixed and must be carried forward
without widening:

- `docs/plans/2026-03-27-post-rev-004-repo-scope-successor-authority-evidence-inputs-and-non-widening-boundary-freeze.md`
  is the live repo-scope authority freeze for this roadmap family.
- explicit recursive annotations remain the production baseline;
  `iso-recursive = keep`,
  `non-equi-recursive = keep`,
  the inherited non-cyclic structural boundary,
  `no-fallback = keep`,
  and one-interface-only all remain binding.
- the same-lane `C2` / `C5` / `C7` pocket remains settled predecessor truth
  only, carried forward from the accepted rev-004 settlement ledger and
  same-family handoff docs. It is not a live repair lane in this round.
- the March 26 representative matrix and the March 26 global
  `non-cyclic-graph = keep` vs `reopen` gate remain immutable historical
  evidence only. This round must not back-edit them in place.
- no equi-recursive reasoning, cyclic structural graphs, multi-SCC search,
  second interfaces, fallback widening, production implementation, hardening,
  or item-3 posture choice is authorized here.

## Authoritative Output Set

The implementer may write only the following round-owned outputs:

1. `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
   - owner: aggregate synthesis lane only;
   - role: the only authoritative item-2 artifact.
2. `orchestrator/rounds/round-118/implementation-notes.md`
   - owner: aggregate synthesis lane only;
   - role: non-authoritative companion notes, verification log, and parallel
     execution summary for the same item-2 artifact.
3. `orchestrator/rounds/round-118/lanes/c1-provenance-summary.md`
   - owner: `C1` provenance lane only;
   - role: lane-local, non-authoritative scratch summary.
4. `orchestrator/rounds/round-118/lanes/p5-provenance-summary.md`
   - owner: `P5` provenance lane only;
   - role: lane-local, non-authoritative scratch summary.

Single-writer rule:

- planner owns `orchestrator/rounds/round-118/plan.md` only;
- reviewer remains the only lawful writer for `review.md`,
  `reviews/attempt-<n>.md`, and `review-record.json`;
- no lane may edit `selection.md`, `plan.md`, `review.md`, any prior
  accepted doc, `orchestrator/state.json`, `Bugs.md`, the roadmap bundle, or
  the other lane's scratch file.

## Parallel Lane Authorization

This round uses the bounded parallel split authorized by `selection.md`.
Parallel rounds remain forbidden. The critical path is:

1. freeze inputs and write scopes;
2. run the `C1` and `P5` provenance lanes in parallel under isolated build
   outputs;
3. merge both lane results into one aggregate synthesis pass that writes the
   canonical refreshed matrix artifact and the round notes.

### Lane A: `C1` provenance

- Question:
  can the round republish a fresh authoritative-surface read for the bounded
  `C1` non-local alias-bound / base-like packet without widening past the
  inherited item-2 surface?
- Read-only inputs:
  `test/Research/C1AuthoritativeSurfaceSpec.hs`,
  the item-1 freeze artifact,
  the March 26 `C1` / `C2` / `C5` slice,
  the March 26 representative matrix,
  the baseline/capability/full-pipeline contracts, and `Bugs.md`.
- Allowed commands:
  read-only `rg`, `sed`, `test -f`, and one focused harness rerun such as
  `cabal test mlf2-test --builddir=dist-newstyle-round118-c1 --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'`.
- Writable file:
  `orchestrator/rounds/round-118/lanes/c1-provenance-summary.md` only.
- Forbidden writes:
  the canonical refreshed-matrix artifact, `implementation-notes.md`,
  any `P5` file, any March 26 doc, or any code/test file.
- Merge point:
  hand the verified current `C1` read, command record, and build-dir used to
  the aggregate synthesis owner; the lane does not edit authoritative docs.
- Build isolation:
  must use `dist-newstyle-round118-c1`, or else run fully serialized with no
  concurrent shared `dist-newstyle` access.

### Lane B: `P5` provenance

- Question:
  can the round republish a fresh clear-boundary versus nested-`forall`
  contrast from the bounded `P5` harness without reopening the settled
  same-lane pocket as live debt?
- Read-only inputs:
  `test/Research/P5ClearBoundarySpec.hs`,
  the item-1 freeze artifact,
  the March 26 `C3` / `C7` slice,
  the March 26 representative matrix,
  the rev-004 settlement ledger and post-settlement handoff docs,
  the baseline/capability/full-pipeline contracts, and `Bugs.md`.
- Allowed commands:
  read-only `rg`, `sed`, `test -f`, and one focused harness rerun such as
  `cabal test mlf2-test --builddir=dist-newstyle-round118-p5 --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`.
- Writable file:
  `orchestrator/rounds/round-118/lanes/p5-provenance-summary.md` only.
- Forbidden writes:
  the canonical refreshed-matrix artifact, `implementation-notes.md`,
  any `C1` file, any March 26 doc, or any code/test file.
- Merge point:
  hand the verified clear-boundary versus nested-`forall` read, command
  record, and build-dir used to the aggregate synthesis owner; the lane does
  not edit authoritative docs.
- Build isolation:
  must use `dist-newstyle-round118-p5`, or else run fully serialized with no
  concurrent shared `dist-newstyle` access.

### Lane C: aggregate synthesis

- Question:
  given the accepted item-1 freeze, the settled rev-004 `C2` / `C5` / `C7`
  predecessor truth, and any fresh round-118 `C1` / `P5` provenance that
  passes the lane rules above, what is the one honest refreshed repo-scope
  representative matrix surface for item `2`?
- Read-only inputs:
  everything above, plus the lane-local summaries once complete.
- Writable files:
  `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
  and `orchestrator/rounds/round-118/implementation-notes.md` only.
- Forbidden writes:
  both lane-local summaries, any March 26 canonical artifact,
  `orchestrator/state.json`, roadmap files, `Bugs.md`, or code/test files.
- Merge point:
  this lane is the only writer that may consolidate the carried-forward
  settled same-lane read plus optional fresh `C1` / `P5` provenance into the
  authoritative item-2 artifact.
- Build isolation:
  no focused Cabal rerun is needed here if both provenance lanes already
  produced admissible rerun records; any extra Cabal command must be
  serialized and must not share a live build dir with another lane.

## Sequential Task List

### Task 1: Freeze the item-2 authority and evidence-input ledger

- Re-state in the canonical artifact that item `2` consumes:
  the March 14 baseline contract, the March 25 capability and full-pipeline
  contracts, the March 25 architecture decision as predecessor context only,
  the March 26 representative matrix and global gate as immutable history,
  the accepted rev-004 settlement ledger / validation / handoff chain as
  settled same-lane predecessor truth, the round-117 freeze artifact as the
  live repo-scope boundary, and `Bugs.md` as non-authoritative bug context
  only.
- State explicitly that the refreshed matrix is a new round-owned surface and
  that no older matrix, gate, or same-lane dossier is edited in place.
- State explicitly that `C2` / `C5` / `C7` are carried forward from accepted
  rev-004 docs only and are not rerun as a live repair lane.

### Task 2: Complete the bounded `C1` provenance lane

- Verify `test/Research/C1AuthoritativeSurfaceSpec.hs` exists and remains the
  bounded harness referenced by `selection.md`.
- Run the focused `C1 authoritative-surface harness` under the isolated
  `dist-newstyle-round118-c1` build dir, or serialize it if isolation is not
  available.
- Record in `orchestrator/rounds/round-118/lanes/c1-provenance-summary.md`:
  the exact command, the build dir, the pass/fail result, and the fresh
  authoritative-surface read for fallback plus both public pipeline
  entrypoints.
- Do not write the canonical refreshed matrix from this lane.

### Task 3: Complete the bounded `P5` provenance lane

- Verify `test/Research/P5ClearBoundarySpec.hs` exists and remains the
  bounded harness referenced by `selection.md`.
- Run the focused `P5 clear-boundary retained-child probes` harness under the
  isolated `dist-newstyle-round118-p5` build dir, or serialize it if
  isolation is not available.
- Record in `orchestrator/rounds/round-118/lanes/p5-provenance-summary.md`:
  the exact command, the build dir, the pass/fail result, and the exact
  clear-boundary versus nested-`forall` read.
- Keep the lane strictly provenance-only: it may cite the clear-boundary
  retained-child read as contrast context, but it may not reopen the settled
  `C2` / `C5` / `C7` pocket as a live re-audit.

### Task 4: Write the one authoritative refreshed matrix artifact

- After both provenance lanes finish, write
  `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
  as the only authoritative item-2 artifact.
- The canonical artifact must:
  - identify item `2`, `round-118`, and the refreshed matrix as a docs-first,
    repo-scope settlement surface only;
  - carry forward the accepted rev-004 `C2` / `C5` / `C7` settled read from
    the post-amendment settlement ledger and same-family handoff docs, while
    keeping that pocket closed as predecessor truth;
  - republish any fresh `C1` / `P5` evidence only if the lane-local summary
    proves the harness reran successfully under serialized or isolated build
    conditions;
  - if a fresh lane result is missing, fails, or lacks lawful provenance,
    leave that row on historical carry-forward only and say so explicitly
    instead of silently promoting local planning context into authority;
  - present one refreshed representative matrix covering `P1-row`,
    `C1`, `C2`, `C3`, `C4`, `C5`, `C6`, and `C7`, with each row naming the
    controlling family, exact current read, evidence source, and bounded
    classification;
  - include an explicit provenance-validation section that names the
    fresh-harness lanes used, the build-isolation method, and the reasons any
    fresh lane was included or excluded;
  - preserve the March 26 representative matrix and March 26 global gate as
    immutable historical evidence only; and
  - stop at refreshed-matrix publication and validation, without choosing the
    item-3 narrowed successor posture.

### Task 5: Write round notes and verification trace

- Write `orchestrator/rounds/round-118/implementation-notes.md` after the
  canonical artifact is complete.
- The notes file must include:
  - a concise change summary;
  - a `Parallel execution summary` section naming the `C1` lane, the `P5`
    lane, the aggregate synthesis owner, their write scopes, and the merge
    discipline;
  - the baseline verification commands from `verification.md`;
  - explicit evidence that any cited `C1` / `P5` harness exists and was rerun
    under isolated or serialized build output;
  - an explicit docs-only skip note for the full
    `cabal build all && cabal test` gate unless the diff escapes the allowed
    docs/orchestrator surface.

## Failure Discipline

- If either provenance lane cannot prove lawful rerun provenance, the round
  may still finish item `2` only by excluding that fresh lane from the
  authoritative matrix and carrying forward the historical row read
  explicitly.
- If aggregate synthesis cannot produce one coherent refreshed matrix without
  reopening the settled same-lane pocket or silently treating local harness
  context as authoritative, the round must fail closed in review rather than
  widening into item `3` or implementation work.
- No lane may reinterpret build collisions or shared `dist-newstyle` races as
  domain evidence. The command must be rerun under the isolated or serialized
  rule first.
