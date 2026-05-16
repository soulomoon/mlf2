### Selected Extraction
- Milestone: Primitive Inventory Module
- Milestone id: milestone-1
- Direction id: direction-1a-primitive-inventory-module
- Extracted item id: item-1a-private-owner-and-source-backend-adapters
- Roadmap id: 2026-05-16-00-architecture-deepening-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/rev-001

### Goal
Establish one private primitive inventory owner for builtin names and source/backend primitive typing, and migrate the frontend builtin registry plus backend validation/conversion adapters to consume that owner without widening public surfaces or pulling LLVM/native runtime implementation into the first slice.

### Approach
Milestone-1 is the only lawful dependency-ready milestone: it has no dependencies, while milestones 2 through 6 remain blocked behind milestone-1 or later work in `roadmap-view.json`. Current HEAD shows duplicated primitive knowledge across `MLF.Frontend.Program.Builtins`, `MLF.Backend.IR`, and `MLF.Backend.Convert`: builtin type/kind sets, opaque builtin data, reserved runtime primitive signatures, and builtin canonicalization live in separate literal tables today, while `MLF.Backend.LLVM.Lower` separately owns native wrapper/runtime implementation names. The first extraction should therefore create a private owner for the shared inventory facts and migrate only the source/backend typing adapters that currently duplicate them. Keeping lowering/native runtime implementation follow-up out of this round preserves a bounded blast radius and matches the roadmap's "owner plus one adapter family" extraction rule.

This round should stay serial. The selected files share the same primitive facts, and the acceptance evidence depends on integrated frontend/backend behavior rather than disjoint worker outputs. If the source audit shows that lowering/runtime implementation names cannot remain owner-local without leaving another unguarded typing table, the implementer may take the minimum adapter needed to keep the shared inventory authoritative, but must not broaden into CLI preparation or milestone-2 callable-shape work.

### Steps
1. Audit the live primitive inventory facts in `src/MLF/Frontend/Program/Builtins.hs`, `src/MLF/Frontend/Program/Resolve.hs`, `src/MLF/Frontend/Program/Check.hs`, `src/MLF/Backend/IR.hs`, and `src/MLF/Backend/Convert.hs`, and define the minimal private owner-module surface for this slice: builtin type names/kinds, opaque builtin type metadata, reserved runtime primitive names, source/backend primitive signatures, and builtin qualification/canonicalization helpers.
2. Introduce that private primitive inventory owner under `src/MLF/`, register it in `mlf2.cabal`, and move the canonical inventory data into it without adding any new public facade or second backend IR layer. Keep `MLF.Frontend.Program.Builtins` as a frontend adapter that derives symbol and `ValueInfo` helpers from the owner instead of continuing to own raw literal tables.
3. Rewire `MLF.Frontend.Program.Builtins`, `MLF.Frontend.Program.Resolve`, `MLF.Frontend.Program.Check`, `MLF.Backend.IR`, and `MLF.Backend.Convert` to consume the owner data instead of maintaining competing primitive tables or builtin-type sets. Remove the replaced duplicate literals, but leave `MLF.Backend.LLVM.Lower` on owner-local runtime wrapper implementation details unless a minimal adapter is required to keep the shared primitive inventory authoritative.
4. Update `docs/architecture.md` and any directly affected contract doc such as `docs/backend-native-pipeline.md` to record the new primitive inventory owner and the remaining milestone-1 follow-up boundary. Add or tighten focused regression coverage so the frontend builtin surface, backend validation/conversion, and repo guardrails all prove they agree on the same owner-managed primitive inventory.
5. Run focused primitive/frontend/backend checks first, then `git diff --check`, and then `cabal build all && cabal test` because this extraction changes production source modules.

### Verification
- `git diff --check`
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.IR"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "accepts backend conversion when pure bindings reference IO primitives"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "matches the checked backend IR snapshot for a primitive function program"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "typechecks direct IO bind primitive uses with consistent arguments"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "rejects constructor imports for opaque Prelude IO"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary"'`
- Manual source audit that no duplicate source/backend primitive typing table remains outside the selected owner and any untouched `MLF.Backend.LLVM.Lower` primitive details are implementation-only runtime adapters rather than a second inventory authority
- Manual docs audit that `docs/architecture.md` and `docs/backend-native-pipeline.md` describe the same primitive owner and do not overclaim native-policy consolidation beyond this slice
- `cabal build all && cabal test`
