# Findings

- `src-public/` includes `MLF.XMLF` (`src-public/MLF/XMLF.hs`, `mlf2.cabal` public `exposed-modules`), but `AGENTS.md` public module list omits it.
- Current witness module is `MLF.Constraint.Types.Witness` and `EdgeWitness` does not include `ewSteps`; doc claim referencing `MLF.Constraint.Types.EdgeWitness` + `ewSteps` is stale.
- `mlf2.cabal` defines an extra executable `frozen-parity-gen`; AGENTS build command section currently lists only `mlf2` executable flow.
- Presolution edge flow now uses typed planner/interpreter payloads (`EdgePlan` in `MLF.Constraint.Presolution.EdgeProcessing.Plan`); `AGENTS.md` referenced `EdgeCtx`, which is no longer present.
