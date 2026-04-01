# Round 169 Plan

- Round: `round-169`
- Roadmap: `2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap` / `rev-001`
- Item: `item-1`
- Retry: `null`
- Execution shape: docs-only, aggregate-only, serial

## Objective

Freeze one docs-only successor-boundary artifact for the inherited
`sameLaneAliasFrameClearBoundaryExpr` blocker lane. This round must bind the
predecessor authority chain, the current exact blocker read for that lane, the
exact item-2 success bar, and the exact writable slice for one bounded
current-architecture follow-on without widening into implementation or a
repo-level readiness claim.

## Sequential Plan

1. Create the canonical item-1 freeze artifact at
   `docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md`
   and keep the round docs-only.
   - Mark it as item `1`, `attempt-1`, `retry: null`, docs-only, freeze-only,
     and pre-implementation.
   - State explicitly that this round does not implement code or tests, rerun
     focused domain evidence as new authority, reopen settled predecessor
     packets, authorize cyclic search, multi-SCC search, equi-recursive
     reasoning, fallback widening, a second interface, or make a repo-level
     readiness claim.
   - Limit implementer-owned writes for this round to the canonical freeze
     artifact and
     `orchestrator/rounds/round-169/implementation-notes.md`.

2. Populate the authority ledger from the exact predecessor chain this family
   inherits.
   - Carry forward
     `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
     as the explicit-only / iso-recursive / non-equi-recursive /
     non-cyclic-graph / no-fallback production baseline.
   - Carry forward
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
     as the binding repo-level success contract that still forbids upgrading
     one bounded packet into broad readiness.
   - Carry forward
     `docs/plans/2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision.md`
     as the narrowed-successor authority that keeps the live question inside
     the current architecture.
   - Carry forward
     `docs/plans/2026-03-28-same-lane-retained-child-representative-gap-successor-authority-exact-subject-success-bar-and-writable-slice-freeze.md`
     as the exact representative-gap freeze that named
     `sameLaneAliasFrameClearBoundaryExpr`, its success bar, and its writable
     slice.
   - Carry forward
     `docs/plans/2026-03-29-same-lane-alias-frame-representative-gap-post-item-2-settlement-surface-and-repo-impact-read.md`
     as the accepted blocker settlement proving the lane remained a narrower
     current-architecture blocker at the March 29 checkpoint, while still
     allowing item-1 to freeze a later exact blocker read if the current
     focused harness now proves a more downstream failure on the same packet.
   - Record the March 29 implementation and gap-fix families as predecessor
     mechanism truth only: automatic `TyMu` introduction exists, reification
     produces `TMu`, elaboration emits `ERoll` / `EUnroll`, and the bounded
     gap-fix lane is closed without turning that into a broad readiness claim.

3. Freeze the exact inherited blocker lane and current blocker read.
   - Keep the live subject fixed to the exact packet
     `sameLaneAliasFrameClearBoundaryExpr`, anchored by
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`.
   - Record that the current exact live blocker read on
     `runPipelineElab` and `runPipelineElabChecked` is
     `PipelineTypeCheckError (TCLetTypeMismatch ...)`, because the current
     focused harness now proves a later type-check/pipeline blocker for the
     same exact packet than the March 29 `PhiTranslatabilityError` checkpoint.
   - Distinguish this inherited lane from the settled first same-lane pocket
     and the settled exact `P5` packet, and keep those predecessor packets
     closed rather than reopening them as live debt.
   - State that the lane remains bounded to one current-architecture successor
     continuation only.

4. Freeze the exact item-2 success bar for the next bounded attempt.
   - Item `2` may operate only on the exact inherited packet
     `sameLaneAliasFrameClearBoundaryExpr`.
   - Item `2` must stay within the inherited explicit-only / iso-recursive /
     non-equi-recursive / non-cyclic-graph / no-fallback boundary.
   - A positive item-2 outcome requires review-visible improvement on the
     current authoritative pipeline surfaces, not helper-only or alternate-path
     evidence.
   - If that positive clear does not happen, item `2` must still end in one
     honest exact-packet outcome only:
     `fail-closed` or `narrower current-architecture blocker`.
   - Record explicitly that even a positive exact-packet result still does not
     settle general `P3` / `P4` / `P6` or justify a repo-level readiness
     claim.

5. Freeze the writable slice for the next bounded current-architecture
   attempt.
   - Permit only the exact production paths already implicated by the carried
     blocker lane:
     `src/MLF/Elab/Run/ResultType/Fallback.hs`,
     `src/MLF/Elab/Run/Scope.hs`,
     `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`,
     `src-public/MLF/Pipeline.hs`,
     and `src/MLF/Elab/TermClosure.hs`.
   - Permit only the exact supporting test and round-owned surfaces:
     `test/PipelineSpec.hs`,
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
     `test/Main.hs`,
     `mlf2.cabal`,
     the canonical freeze artifact,
     and `orchestrator/rounds/round-169/*`.
   - Block all `src/MLF/Constraint/**`, cyclic or multi-SCC machinery,
     fallback widening, second-interface work, second-family openings, and any
     edits outside this frozen slice.
   - State that if a later item-2 round touches `src/`, `src-public/`, `app/`,
     `test/`, or `mlf2.cabal`, the full gate `cabal build all && cabal test`
     becomes mandatory under the live verification contract.

6. Record round-local implementation notes and explicit review expectations.
   - Update `orchestrator/rounds/round-169/implementation-notes.md` with the
     docs-only freeze summary, authority chain, inherited blocker read, success
     bar, and writable-slice freeze.
   - Keep reviewer focus on authority continuity, exact inherited blocker-lane
     freeze, exact blocker read, exact item-2 bar, exact writable-slice
     freeze, and non-widening scope.
   - Keep the round aggregate-only and stop at the freeze; do not authorize
     implementation within round-169.

## Verification Commands

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `git diff --name-only -- src src-public app test mlf2.cabal orchestrator/roadmaps Bugs.md`
- `rg -n '2026-03-14-automatic-recursive-inference-baseline-contract|2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus|2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision|2026-03-28-same-lane-retained-child-representative-gap-successor-authority-exact-subject-success-bar-and-writable-slice-freeze|2026-03-29-same-lane-alias-frame-representative-gap-post-item-2-settlement-surface-and-repo-impact-read' docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md`
- `rg -n 'sameLaneAliasFrameClearBoundaryExpr|PipelineTypeCheckError|TCLetTypeMismatch|runPipelineElab|runPipelineElabChecked|narrower current-architecture blocker' docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md`
- `rg -n 'Writable Slice|src/MLF/Elab/Run/ResultType/Fallback.hs|src/MLF/Elab/Run/Scope.hs|src/MLF/Elab/Run/Pipeline.hs|src/MLF/Elab/Pipeline.hs|src-public/MLF/Pipeline.hs|src/MLF/Elab/TermClosure.hs|test/PipelineSpec.hs|test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs|test/Main.hs|mlf2.cabal' docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md`
- `rg -n 'docs-only|freeze-only|pre-implementation|do(es)? not implement code|repo-level readiness claim|cyclic search|multi-SCC search|fallback widening|second interface' docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md`
- `if git diff --name-only -- src src-public app test mlf2.cabal | grep -q .; then cabal build all && cabal test; else printf 'skip full cabal gate for docs-only round\n'; fi`
