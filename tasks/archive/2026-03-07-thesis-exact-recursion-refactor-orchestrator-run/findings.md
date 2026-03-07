# Findings

## Baseline

- Mechanism table exists at `docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md` with all eight rows currently marked `NO` pending a live verifier refresh.
- No prior run folder exists for this orchestrator campaign as of baseline commit `4cebc50e56d381a64367c0293cd93f3cc09d2017`.
- The campaign goal is thesis exactness under refactoring pressure, so documentation, tests, and guardrails may be valid outcomes even when broad recursion-schemes rewrites are rejected.

## Fresh verifier sweep

- Verifier-owned row states after the live sweep:
  - `YES`: `Surface Preprocessing Exactness`, `Leftmost-Lowermost Quantifier Ordering`, `Let-Scope Translation Discipline`, `Translatable Presolution Boundary`, `Computation Context Construction`
  - `NO`: `Typing Environment Construction`, `Binder-Safe Tree Recursion Coverage`, `Graph-Phase Explicitness Guardrail`
- First remaining `NO`: `Typing Environment Construction`.
- Row5 remains open because the code/test evidence is still indirect; the verifier found no direct production-path anchor proving `Definition 15.3.6` / `Property 15.3.7` on the live path.
- Row7 remains open because the recursion-schemes inventory is not exhaustive enough for a verifier-owned closeout.
- Row8 remains open because the campaign still lacks an explicit negative guardrail keeping graph-sensitive phases out of broad recursion-schemes pressure.
- Full verification gate passed on the current repository state during the sweep: `cabal build all && cabal test`.

## Round 1 closeout

- Row5 `Typing Environment Construction` now has direct production-path anchors in `docs/thesis-obligations.yaml` plus live-path regressions in `test/ElaborationSpec.hs` for both the lambda-side and let-side `Γ_{a′}` rules.
- The row5 slice passes together with the supporting `ga′` / `letScopeOverrides` / scope guard slices and the full `cabal build all && cabal test` gate.
- After row5 closes, the next fixed-order `NO` rows were `Binder-Safe Tree Recursion Coverage` and `Graph-Phase Explicitness Guardrail`.

## Round 2 closeout

- Row7 closed once `implementation_notes.md` gained the exhaustive active-campaign traversal inventory and the mechanism table treated that inventory as the authoritative recursion-refactor scope boundary.
- Row8 closed once `implementation_notes.md` gained the explicit graph-phase non-goal guardrail and the mechanism table pointed at that negative boundary.
- The final verifier pass confirmed rows 7 and 8 are both `YES`, so all mechanism rows are now `YES`.

## Verifier sweep notes (2026-03-07, pass 1)

- Surface preprocessing anchors are concrete in `src/MLF/Frontend/Normalize.hs`, `src/MLF/Frontend/Desugar.hs`, and `src/MLF/Frontend/ConstraintGen/Translate.hs`; the thesis text explicitly ties annotated-lambda desugaring to coercion functions in `§12.3.2.2` and annotation translation in `§15.3.8`.
- Reorder/context/translatability mechanisms have direct obligation IDs in `docs/thesis-obligations.yaml` (`O15-REORDER-*`, `O15-CONTEXT-*`, `O15-TRANS-*`) and corresponding anchor tests in `test/ElaborationSpec.hs` / `test/Presolution/EnforcementSpec.hs`.
- Current mechanism-table rows are intentionally conservative (`NO` everywhere), so the live verifier pass must distinguish between “already anchored and likely thesis-exact” versus “guardrail not yet evidenced enough for absolute exactness”.

## Verifier sweep notes (2026-03-07, pass 2)

- Row 1 evidence is unusually direct: `normalizeType` inlines alias bounds, `desugarSurface` performs the thesis `λ(x:τ)a ≜ λ(x) let x = cτ x in a` rewrite, and `buildCoerce` / `internalizeCoercionType` preserve rigid-domain/flexible-codomain coercion semantics with dedicated tests for codomain return and existential sharing.
- Row 3 is implemented as the thesis’ translation-friendly rightmost `let` constraint, not merely approximated: `Translate.hs` allocates `letGen`, a `trivialRoot`, records the trivial let edge, and `dropTrivialSchemeEdges` removes that edge from witness/expansion output, matching `§15.2.6.1`’s identity-computation rationale.
- Rows 2, 4, and 6 have direct obligation IDs plus row-labeled tests (`O15-REORDER-*`, `O15-TRANS-*`, `O15-CONTEXT-*`, `O15-EDGE-TRANSLATION`) and live code that still matches the thesis intent.
- Row 5 remains the weakest evidence case: the code has strong `ga′`/scope preservation notes and tests, but there is no equally direct, row-labeled proof that the live elaboration path constructs the full inductive `Γ_{a′}` of Def. 15.3.6 and thereby discharges Property 15.3.7 on the production path.
- Rows 7 and 8 are campaign guardrails rather than single thesis equations. The codebase already uses recursion-schemes selectively and keeps graph phases explicit, but there is no exhaustive verifier-owned inventory proving that all remaining manual traversals are correctly classified as safe tree refactors vs explicit graph/binder boundaries.
