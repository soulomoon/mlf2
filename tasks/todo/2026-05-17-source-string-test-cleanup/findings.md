# Findings

## Scanner Inputs

Five read-only `gpt-5.3-codex-spark` scanners inspected non-overlapping slices:

- `test/PipelineSpec.hs`
- `test/RepoGuardSpec.hs`
- presolution/solved specs
- smaller elaboration/frontend/reify/binding/generalize specs
- full-suite cross-check

## Confirmed Pattern

The suite has a large number of tests that read repository source files and assert exact strings. These are brittle when they check private helper names, implementation snippets, or prose fragments. Renaming or moving code can defeat the test without changing the behavior.

## Counts

- `test/PipelineSpec.hs`: 135 matching source/doc/config reads
- `test/RepoGuardSpec.hs`: 84 matching source/doc/config reads
- `test/PresolutionFacadeSpec.hs`: 16
- `test/Constraint/SolvedSpec.hs`: 12
- `test/ElaborationSpec.hs`: 8
- `test/Presolution/UnificationClosureSpec.hs`: 4
- `test/PresolutionSpec.hs`: 3
- `test/GeneralizeSpec.hs`: 3
- `test/FrontendNormalizeSpec.hs`: 3
- `test/FrontendParseSpec.hs`: 2
- `test/Reify/TypeSpec.hs`: 3
- single-hit files: `test/TranslatablePresolutionSpec.hs`, `test/Presolution/EdgePlannerSpec.hs`, `test/CanonicalizerSpec.hs`, `test/BindingSpec.hs`, `test/PublicSurfaceSpec.hs`

## Keep Out Of This Cleanup

The full-suite scan flagged these as not source-file grep guards:

- diagnostic substring assertions on runtime error values;
- emitted LLVM/output substring assertions;
- golden-file comparisons;
- fixture reads under `test/programs/**`.

Those may still deserve a separate typed-error or output-contract cleanup later, but they are not the source-file pattern issue.

## Immediate Delete Or Behavioral Replacement Candidates

- `test/TranslatablePresolutionSpec.hs`: first test scans `Validation.hs` for inert-normalization helper names; nearby tests already validate translatable presolution behavior.
- `test/Presolution/EdgePlannerSpec.hs`: final source guard scans `Planner.hs` for wrapper fallback text; preceding tests already assert fail-fast behavior.
- `test/Presolution/UnificationClosureSpec.hs`: final four row3 source guards scan `EdgeProcessing.hs`/`EdgeUnify.hs`; earlier tests assert drained queues, translatability, edge-boundary ordering, and witness/trace alignment.
- `test/FrontendParseSpec.hs`: parser-scaffolding source guard should be removed; parser behavior below already accepts canonical forms and rejects legacy aliases.
- `test/PipelineSpec.hs`: ARI-C1 source guards at the clear-boundary tests have same-test recursive-arrow behavioral assertions.

## Structural Rewrite Candidates

- `test/RepoGuardSpec.hs`: keep spec registration, cabal stanza checks, module-file absence, public-library exclusion, and import-offender scans. Rewrite or remove exact private-symbol/prose checks.
- `test/PipelineSpec.hs`: most source-shape guards should not live in pipeline behavior tests. If a boundary is still worth checking, it should become a structural repo guard, not a private helper substring test.
- `test/PresolutionFacadeSpec.hs`, `test/Constraint/SolvedSpec.hs`, and similar facade specs need export/API/import-aware checks before their substring guards can be removed without losing the stated boundary intent.
