# Fully Automatic Unannotated Iso-Recursive Inference Completion Refresh

Date: 2026-04-13
Status: current live truth
Artifact kind: completion refresh / supersession note
Primary implementation anchor:
`.omx/plans/prd-2026-04-12-fully-general-unannotated-automatic-iso-recursive-inference-consensus.md`

## Live Claim

The repository can now honestly claim:

> fully automatic unannotated iso-recursive inference across its
> representative family matrix inside the inherited explicit-only /
> iso-recursive / non-equi-recursive / no-fallback / no-second-interface
> production boundary

This is the current live repo truth. It supersedes the April 11
fully-automatic-unannotated stop/Phase-4-closed read for present-tense claims,
while leaving those docs intact as predecessor evidence of the earlier state.

## Why The Stronger Claim Is Now Honest

The April 11 stop docs were written before the April 12 direct implementation
pass closed the shared-selection, authoritative-parity, and broader
representative-family gaps. Current-head evidence now clears the capability
contract that those docs had left open:

1. **Unannotated carrier discovery exists on authoritative surfaces.**
   - `URI-R2-C1 unannotated carrier`
   - Current result: `2 examples, 0 failures`

2. **The inferred recursive carrier survives reconstruction and authoritative
   replay.**
   - `URI-R2-C1 reconstruction`
   - `URI-R2-C1 combined wrapper`
   - Current results: `4 examples, 0 failures`; `24 examples, 0 failures`

3. **Uniqueness remains fail-closed rather than heuristic-ranked.**
   - `URI-R2-C1 uniqueness reject`
   - preserved ambiguity negatives in `PipelineSpec`
   - Current result: `1 example, 0 failures`

4. **Representative positive families now span the required contract classes.**
   - local discovery: `URI-R2-C1 unannotated carrier`
   - non-local / owner-sensitive propagation:
     `URI-R2-C1 owner-sensitive non-local transparent mediation`
   - binder-sensitive / nested-`forall` / broader unannotated output:
     `item-4 edge cases`
   - Current results: `24 examples, 0 failures`; `85 examples, 0 failures`

5. **Preserved negatives remain reject-side under the same boundary.**
   - witnessless mediation: `URI-R2-C1 uniqueness reject`
   - non-recursive contrast:
     `does not infer recursive shape for the corresponding unannotated variant`
   - Current results: `1 example, 0 failures`; `1 example, 0 failures`

## Boundary That Still Holds

This completion claim does **not** reopen any forbidden widening:

- no fallback widening
- no heuristic ranking
- no cyclic or multi-SCC search
- no equi-recursive semantics
- no second interface

The claim is therefore stronger than the old representative-family general
automatic read, but it remains bounded by the same inherited production
contract.

## Superseded Predecessor Docs

For live repo truth, the following files are now predecessor evidence only:

- `docs/plans/2026-04-11-fully-automatic-unannotated-iso-recursive-inference-capability-contract-current-state-refresh.md`
- `docs/plans/2026-04-11-fully-automatic-unannotated-iso-recursive-inference-current-architecture-plausibility-decision.md`
- `docs/plans/2026-04-11-fully-automatic-unannotated-iso-recursive-inference-march-reentry-evidence-gate.md`
- `docs/plans/2026-04-11-fully-automatic-unannotated-iso-recursive-inference-mechanism-map-refresh.md`
- `docs/plans/2026-04-11-fully-automatic-unannotated-iso-recursive-inference-phase-4-open-decision.md`

Those documents remain truthful historical records of the earlier stop state;
they are no longer the current present-tense claim surface.

## Fresh Verification

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 unannotated carrier"'`
  -> `2 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 reconstruction"'`
  -> `4 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 uniqueness reject"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "does not infer recursive shape for the corresponding unannotated variant"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 owner-sensitive non-local transparent mediation"'`
  -> `24 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 combined wrapper"'`
  -> `24 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "item-4 edge cases"'`
  -> `85 examples, 0 failures`
- `./scripts/thesis-conformance-gate.sh`
  -> `PASS`
- `cabal build all && cabal test`
  -> `1539 examples, 0 failures`
