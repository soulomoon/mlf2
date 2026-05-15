### Selected Extraction
- Milestone: `NodeRef GADT And RefTag Boundary`
- Milestone id: `milestone-1`
- Direction id: `direction-1a-noderefgadt-reftag-kind`
- Extracted item id: `binding-adjustment-typed-child-ref-boundary`
- Roadmap id: `2026-05-05-00-type-level-safety-singletons-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`

### Goal
Eliminate the reintroduced untyped child-ref boundary in `MLF.Binding.Adjustment`
so type-only harmonization and raise helpers take `NodeRefTag 'TypeTag`, while
keeping `NodeRef` only where mixed type/gen storage or ancestor targets are
genuinely required.

### Approach
Use the existing `NodeRefTag` seam that `MLF.Binding.GraphOps` already enforces.
Keep the round serial and scoped to `MLF.Binding.Adjustment`, its direct
callers, and the focused tests needed to prove that the accepted mixed
`NodeRef` seam still lives only at binding-tree storage or parent-target
boundaries. Do not broaden this round into general `NodeRef` cleanup across
unrelated query, elaboration, or presolution paths, and do not widen any
public-facing API.

### Steps
1. Inspect `MLF.Binding.Adjustment` and its direct callers to classify each
   parameter as either a type-only child ref or a genuinely mixed parent or
   ancestor target. If a caller truly needs mixed child refs, stop there and
   record the blocker instead of introducing another ad hoc runtime
   discriminator.
2. Change the type-only `Binding.Adjustment` entrypoints to accept
   `NodeRefTag 'TypeTag` child refs, delete the local `requireTypeRef`
   runtime conversion, and preserve `NodeRef` only for mixed parent or
   ancestor targets or storage-backed traversal where the mixed seam remains
   intentional.
3. Update the bounded caller set and focused tests to use the typed child-ref
   API, including the normalization, presolution, solve, and binding tests
   that currently pass `typeRef ...` into `Binding.Adjustment`.
4. Tighten or add focused coverage that the typed child boundary is enforced
   at the `Binding.Adjustment` seam while mixed ancestor targets still work
   through the retained `NodeRef` path.
5. If module comments or the narrow architecture wording drift after the API
   change, align only the directly affected note instead of widening the docs
   slice.
6. Run the focused checks first, then diff hygiene and the full build and test
   gate required by the roadmap verification contract.

### Verification
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "harmonizeBindParentsWithTrace"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "applyRaiseTo"'`
- `git diff --check`
- `cabal build all && cabal test`
