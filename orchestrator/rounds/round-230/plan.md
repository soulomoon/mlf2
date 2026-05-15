### Selected Extraction
- Milestone: `NodeRef GADT And RefTag Boundary`
- Milestone id: `milestone-1`
- Direction id: `direction-1a-noderefgadt-reftag-kind`
- Extracted item id: `milestone-1-closeout-audit-and-guard`
- Roadmap id: `2026-05-05-00-type-level-safety-singletons-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`

### Goal
Establish milestone-1 closeout evidence at HEAD by auditing the remaining
`NodeRef` / `NodeRefTag` seams, closing any last owner-local untyped
type-only boundary if one still exists, and adding the focused guards needed
for reviewer-approved `in-progress -> done` closeout.

### Approach
Round-229 already typed `MLF.Binding.Adjustment` and its bounded callers, so
the next lawful serial slice is a milestone-1 completion audit rather than a
jump to milestone 2. Keep the work inside the NodeRef boundary owners and
their direct tests/docs: `MLF.Constraint.Types.Graph`, `MLF.Binding.GraphOps`,
`MLF.Binding.Adjustment`, `MLF.Binding.NodeRefs`, and any immediately adjacent
architecture wording that describes the accepted mixed seam. Use the roadmap
completion signal as the checklist: no ambient runtime type/gen discriminator
helpers, typed refs for type-only binding operations, and one explicit
documented owner for retained mixed `NodeRef` storage or ancestor-target
boundaries. Do not broaden this round into phase, witness, public-surface, or
roadmap-semantic cleanup.

### Steps
1. Audit `MLF.Constraint.Types.Graph.NodeEdge`,
   `MLF.Constraint.Types.Graph.Binding`, `MLF.Binding.GraphOps`,
   `MLF.Binding.Adjustment`, `MLF.Binding.NodeRefs`, and their direct
   tests/docs against the milestone-1 completion signal. Classify each
   remaining `NodeRef`-typed binding entrypoint as either an accepted mixed-key
   owner (`BindParents`, ancestor-target operations, error/reporting paths) or
   a residual type-only gap.
2. If the audit finds one residual type-only gap, migrate only that owner-local
   seam to `NodeRefTag` or `SomeNodeRef`, update the bounded callers, and stop
   with a precise blocker instead of widening the round if the fix would spill
   into unrelated pipeline or public surfaces.
3. Tighten the acceptance evidence for the retained mixed seam: add or refine
   focused tests and, only if wording drift remains, the local architecture or
   note comments that identify `NodeRef` as the mixed binding-tree key and
   `NodeRefTag` as the required type-only boundary.
4. Prove the runtime discriminator story directly with a focused source scan
   and inspection of any remaining `NodeRef` pattern matches in the touched
   area. Any surviving discrimination must be an explicitly named boundary
   conversion or mixed-storage owner, not an ad hoc type-only helper.
5. Run the narrow binding test slices touched by the round, then diff hygiene,
   then the full `cabal build all && cabal test` gate so reviewer can decide
   milestone-1 closeout from complete evidence.

### Verification
- `rg -n "expectTypeRef|expectGenRef|requireTypeRef|requireGenRef" src test`
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Binding shared abstractions"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "harmonizeBindParentsWithTrace"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "applyRaiseTo"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "raiseToParentWithCount"'`
- `git diff --check`
- `cabal build all && cabal test`
