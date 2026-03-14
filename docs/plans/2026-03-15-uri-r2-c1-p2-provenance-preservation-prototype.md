# `P2` Provenance-Preservation Prototype For `URI-R2-C1`

Date: 2026-03-15
Roadmap item: 2
Stage: `P2`
Attempt: 2
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Artifact kind: provenance-preservation prototype

## Inherited Inputs

- `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
- `docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md`
- `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`
- `orchestrator/rounds/round-017/selection.md`

## Stage Input Interface

- Inherited token path: `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`.
- Shared entrypoint tuple: `{ research_entrypoint_id: uri-r2-c1-prototype-entrypoint-v1, stage_selector: P2-provenance-preservation, scenario_id: uri-r2-c1-only-v1, attempt_id: 2 }`.

## Method

- Ordered checks: `P2-G`, `P2-S`, `P2-R`, `P2-W`.
- The bounded fixture executes `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback -> witness replay` under the existing shared entrypoint.
- Shared `correlation_id`: `uri-r2-c1-only-v1-p2-attempt-2`.

## Evidence

- Attempt-local evidence directory: `orchestrator/rounds/round-017/evidence/P2/attempt-2`.
- Trace bundle: `orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`.
- `P2-G`: `pass` via `orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`.
- `P2-S`: `pass` via `orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`.
- `P2-R`: `pass` via `orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`.
- `P2-W`: `semantic-negative` via `orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`.
- Trace refs: `trace://uri-r2-c1/p2/generalize/uri-r2-c1-cluster-1/a-b-a-a-b` `trace://uri-r2-c1/p2/scheme-to-type/uri-r2-c1-cluster-1/a-b-a-a-b` `trace://uri-r2-c1/p2/reify-no-fallback/uri-r2-c1-cluster-1/t5-t5` `trace://uri-r2-c1/p2/witness-replay/uri-r2-c1-cluster-1/t9-n-a-a-n`
- Observations:
- P2 consumed only the authoritative P1 subject token from round-016.
- The bounded fixture executed generalization, reconstruction, no-fallback reification, and witness replay through the shared prototype entrypoint.
- All attempt-local check files repeat the inherited subject_id and shared correlation_id.
- P2-G: generalizeWithPlan => ∀(a ⩾ ⊥) ∀(b ⩾ a -> a) b
- P2-S: schemeToType => ∀(a ⩾ ⊥) ∀(b ⩾ a -> a) b
- P2-R: reifyTypeWithNamedSetNoFallback => t5 -> t5
- P2-W: witness replay => ∀(⩾ ⊲t9); N; (∀(⩾ ⊲(a -> a)); N) (applyInstantiation diagnostic failed: InstantiationError "InstBot expects \8869, got: t9 -> t9")

## Rejection Triggers

Observed triggers: `partial-replay`.

`P2-G`: generalizeWithPlan => ∀(a ⩾ ⊥) ∀(b ⩾ a -> a) b
`P2-S`: schemeToType => ∀(a ⩾ ⊥) ∀(b ⩾ a -> a) b
`P2-R`: reifyTypeWithNamedSetNoFallback => t5 -> t5
`P2-W`: witness replay => ∀(⩾ ⊲t9); N; (∀(⩾ ⊲(a -> a)); N) (applyInstantiation diagnostic failed: InstantiationError "InstBot expects \8869, got: t9 -> t9")

Normalized vocabulary: `none` `widened-search` `multi-scc` `cross-family` `heuristic-choice` `late-repair` `manufactured-provenance` `surrogate-substitution` `replay-domain-widening` `non-local-salvage` `structural-cycle` `implicit-unfolding` `equi-recursive-reasoning` `termination-weakening` `nondeterministic-output` `inconsistent-trace` `partial-replay` `production-path-dependence` `blocking-stop-condition`.

## Stage Result

`semantic-negative`
