# Plan: thesis-exact Elab split (module + migration)

## Goal
Realign the pipeline so MLF.Elab consumes solver artifacts rather than making solver-like decisions. Move binder selection, scheme-root planning, and ordering decisions into presolution, leaving Elab as an application/reification layer.

## Target module split
- Frontend (unchanged):
  - `MLF.Frontend.*` produces annotated term + raw constraints.
- Constraint normalization (unchanged):
  - `MLF.Constraint.Normalize`, `MLF.Binding.*` produce canonical graph metadata.
- Presolution / planning (new + expanded):
  - `MLF.Constraint.Presolution.Plan`
    - `GeneralizePlan` (scope root, scheme roots, binder candidates, ordering deps)
    - `ReifyPlan` (root choices, alias policy, subst maps)
    - `planGeneralize :: PresolutionEnv -> AnnExpr -> Either ElabError GeneralizePlan`
    - `planReify :: PresolutionEnv -> GeneralizePlan -> Either ElabError ReifyPlan`
  - `MLF.Constraint.Presolution.Witness` (already) produces Ω/Φ steps/expansions.
- Elab apply (thin):
  - `MLF.Elab.Apply` (new) applies `GeneralizePlan`/`ReifyPlan` + witnesses to terms/types.
  - `MLF.Elab.Reify` becomes purely structural.
  - `MLF.Elab.Generalize` becomes thin or removed (delegates to Plan + Apply).
- Orchestrator:
  - `MLF.Elab.Run.Pipeline` wires: normalize -> presolution -> plan -> apply.

## Migration steps
1. **Define plan records in presolution**
   - Add `GeneralizePlan`/`ReifyPlan` types in `MLF.Constraint.Presolution.Plan`.
   - Move the data currently threaded through `generalizeAtWith` (scheme roots, bind parents, ordering deps, alias policy, binder selection inputs) into these records.

2. **Move plan construction out of Elab**
   - Relocate Phase 1–6 logic (scope root, scheme-root metadata, binder selection, ordering deps) from `MLF.Elab.Generalize` into `MLF.Constraint.Presolution.Plan`.
   - Replace Elab calls with `planGeneralize` and pass the plan record forward.

3. **Move reify planning out of Elab**
   - Relocate Phase 8–10 planning (root selection, alias / subst maps) into `planReify`.
   - Keep `MLF.Elab.Reify` as a pure consumer of `ReifyPlan`.

4. **Thin Elab apply layer**
   - Create `MLF.Elab.Apply` to run: instantiation application, witness application, and reification using the plan records.
   - Remove or drastically thin `MLF.Elab.Generalize` to orchestration only.

5. **Update wiring**
   - Update `MLF.Elab.Run.Pipeline` to call `planGeneralize`/`planReify` before elaboration.
   - Thread plans to Elab.Apply functions.

6. **Validation**
   - Ensure all scheme-free-var checks and ordering invariants remain, but live in presolution planning.
   - Add a note in `implementation_notes.md` about “plan vs apply” split.

## Risks
- Must preserve exact binder ordering / alias policy semantics.
- Ensure no dependency on later Elab-only data when moving plan logic.
- Avoid circular imports between `Constraint.Presolution` and `Elab`.

## Verification
- `cabal test`
