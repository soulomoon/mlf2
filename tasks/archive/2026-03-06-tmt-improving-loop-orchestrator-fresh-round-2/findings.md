# Findings: 2026-03-06 TMT Improving Loop Orchestrator (Fresh Round 2)

## Baseline Provenance
- Baseline commit: `7808933f31f4494c22f37d1e601bd4cf3d2013d7`
- Thesis source of truth: `papers/these-finale-english.txt`
- Live mechanism table: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Orchestrator prompt: `docs/prompts/improving-loop-agent.prompt2.md`

## Round 1 Outcome
- The first live `NO` was row 1, `Elaboration input`.
- The accepted fix removed the last live `resolveContext` fallback swallow in `MLF.Constraint.Presolution.Plan.Context` so `bindingPathToRootLocal` failures now propagate explicitly.
- Regression coverage for row 1 is direct and durable:
  - `resolveContext propagates ga base binding-path failures instead of falling back`
  - `elab-input absolute thesis-exact guard`

## Round 2 Outcome
- After row 1 closed, the next live gap was the coupled row9-11 Ω runtime surface.
- The accepted runtime shape is thesis-exact because Ω now:
  - uses direct replay/source targets;
  - fails fast on unresolved non-trace `OpRaise` targets;
  - keeps only forward `etCopyMap` evidence for source-domain interior membership.
- Runtime identity repair is removed from Φ/Ω; `IdentityBridge` remains only as witness-domain utility and dedicated regression surface.

## Final Sweep Outcome
- A fresh full verification sweep re-evaluated all 14 mechanisms in order.
- Every mechanism returned `YES`.
- `docs/thesis-deviations.yaml` contains no active `DEV-TMT-*` deviations, and the campaign row is therefore `YES`.

## Final Validation Evidence
- Row 1 gates:
  - `resolveContext propagates ga base binding-path failures instead of falling back` — PASS (`1 example, 0 failures`)
  - `elab-input absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
- Coupled row9-11 gates:
  - `source-space identity replay target` — PASS (`1 example, 0 failures`)
  - `OpRaise fails fast when a trace-source target resolves to no existing replay node` — PASS (`1 example, 0 failures`)
  - `OpRaise fails fast when a non-trace target resolves to no existing replay node` — PASS (`1 example, 0 failures`)
  - `row9-11 direct-target guard` — PASS (`1 example, 0 failures`)
  - `IdentityBridge` — PASS (`23 examples, 0 failures`)
- Cross-cutting gates:
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`959 examples, 0 failures`)
