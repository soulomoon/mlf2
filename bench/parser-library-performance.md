# parser-library performance table

Active fixture:

```text
test/programs/compiler-parser-parity/parser-library
```

Lower is better for every timing metric.

The baseline column is a historical parser-library snapshot captured before
module-level definition checking. Parser-library benchmark artifacts are now
captured as a single run because one run already costs about two minutes; repeat
runs should be reserved for final confirmation after a substantial change. The
current column is generated from:

```text
bench/results/parser-library-latest.tsv
```

Captured with:

```bash
./bench/run-benchmarks.sh \
  --runs 1 \
  --allow-status 1 \
  --no-build \
  --benchmark parser-library test/programs/compiler-parser-parity/parser-library \
  --output bench/results/parser-library-latest.tsv
```

The direct annotated checker was measured, then rejected as a production
optimization because it bypassed the full checker pipeline. It is retained
below only as a historical reference, not as the active implementation.

## Headline: per-definition scale

The parser target still pays about two orders of magnitude too much per
generated definition.

| System / metric | Workload | Total checked defs | Current total | Current per def | Ratio vs GHC reference |
| --- | --- | ---: | ---: | ---: | ---: |
| `mlf2` exact pipeline, `ParserParityParser.def-bindings` | generated `.mlfp` parser definitions | 914 | 97575.976ms | 106.757ms | 191.3x |
| GHC 9.12.2 local reference, `-fforce-recomp -fno-code -O0` | persisted synthetic module with 914 parser-shaped top-level defs | 914 | 510.000ms | 0.558ms | 1.0x |

The comparable real GHC artifact is not a testsuite module. It is
`GHC.Parser`, the generated Happy parser for Haskell, whose source grammar
lives at `compiler/GHC/Parser.y`. GHC's `testsuite/tests/parser/*` tree has
parser correctness fixtures (`prog001`, `should_compile`, `should_fail`,
`should_run`, and `unicode`), but those are small acceptance cases rather than
a single generated-parser scale workload.

The GHC row above is therefore a local scale reference, not an apples-to-apples
semantic comparison. It answers: how fast can GHC process 914 ordinary
parser-shaped top-level definitions when module state is shared? The important
signal is still the gap: our generated parser library is paying
per-definition graph construction, presolution edge processing, generalization
preparation, and result-type reconstruction costs that should be amortized or
indexed.

External orientation links:

```text
GHC.Parser docs: https://downloads.haskell.org/ghc/9.0.2/docs/html/libraries/ghc-9.0.2/GHC-Parser.html
GHC grammar source: https://raw.githubusercontent.com/ghc/ghc/master/compiler/GHC/Parser.y
GHC parser testsuite: https://github.com/ghc/ghc/tree/master/testsuite/tests/parser
```

Reference artifact:

```text
bench/results/ghc-per-def-reference.tsv
```

Persisted reference source:

```text
bench/ghc-reference/ParserShape914.hs
```

Reproduction command for the GHC reference:

```bash
bench/run-ghc-parser-reference.sh \
  --runs 1 \
  --output bench/results/ghc-per-def-reference.tsv
```

Most important hotspot from the detailed profile artifact:

| Exact-pipeline slice | Total (ms) | Approx per `ParserParityParser` def |
| --- | ---: | ---: |
| `presolution` | 68246.306 | 74.668ms |
| `presolution.edge_loop` | 51672.688 | 56.535ms |
| `result_type_reconstruction` | 19643.217 | 25.478ms over 771 result-typed defs |
| `prepare_generalization` | 9791.551 | 10.713ms |

This makes the next algorithmic target explicit: an indexed presolution edge
solver plus frozen/lazy external-scheme instantiation. More small read-context
caches are useful only if they reduce these totals.

| Metric | Before module def checker (ms) | Current exact module read context (ms) | Saved vs baseline (ms) | Reduction | Speedup | Rejected direct-bypass reference (ms) | Reading |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| `program.check.modules` | 161077.348 | 139511.385 | 21565.963 | 13.39% | 1.15x | 67796.970 | Direct bypass is removed; the current win comes from keeping the full pipeline while sharing exact read-only prep, carrying edge instantiation metadata, reusing result-type/source-type read context, carrying elaboration typecheck environments incrementally, sharing prepared elaboration/result-type read models, reusing result-type reification read models, avoiding duplicate edge-local interior root lookups, indexing read-model binding children for reification, reusing the base read-model var-only node map in result-type reconstruction, avoiding edge-local order-key construction for zero/one-binder edges, reading presolution copy sources from the stable snapshot, selectively materializing only referenced external schemes, seeding presolution edge owner/root indexes once per edge loop, recording guarded edge fingerprints without replaying non-idempotent processed expansions, using a sequence-backed generic indexed worklist queue with direct requeue, removing avoidable order-key queue appends, carrying known app argument/function types through the μ-use helper, preserving dirty binding-repair sets instead of invalidating the whole repair model, carrying the typed application result through μ result rolling, reusing known application result types when the function and argument types already prove the app, carrying known coerced argument types through μ argument coercion, carrying a root generalization into result-type reconstruction when the exact `(scope,target)` key matches, caching result-type fallback summaries plus scheme-root ownership/body lookup in the shared `ResultTypeView`, reusing the cached scheme-body target during root generalization, carrying checked function candidates through the μ-use helper, splitting edge-expansion timing to expose copy-bound work, reusing one binding snapshot plus duplicate canonical bound copies inside `copyBinderBounds`, adding diagnostic root-ownership indexes, replacing independent diagnostic multi-root graphs with root-local partitions, restricting module-root external environments to referenced names, keeping normal-mode batch layers out of nested detailed timing, building root partitions with one bucketed graph pass instead of one graph scan per root, and running independent root partitions concurrently only in opt-in multi-root batches. |
| `program.check.module.ParserParityParser` | 112892.114 | 97803.532 | 15088.582 | 13.37% | 1.15x | 23814.194 | Main target module remains the dominant cost; every accepted definition still runs the normal checker path. |
| `program.check.module.ParserParityParser.def-bindings` | 112683.979 | 97575.976 | 15108.003 | 13.41% | 1.15x | 23622.957 | 914 generated defs pay the full pipeline; the per-def cost is still about 106.757ms in one-root mode, so the remaining single-root win requires deeper edge execution, safe external-scheme sharing, result-type reconstruction, and generalization work. |
| `program.check.module.ParserParityParserCombinator` | 17656.853 | 14426.187 | 3230.666 | 18.30% | 1.22x | 15440.448 | Below the module read-context gate; compare as current checkout context, not direct evidence for the gate. |
| `program.check.module.ParserParityLexer` | 16997.185 | 14587.420 | 2409.765 | 14.18% | 1.17x | 15507.747 | Below the module read-context gate at 130 defs; still full per-def pipeline. |
| `program.check.module.ParserParityAst` | 10838.915 | 9887.806 | 951.109 | 8.77% | 1.10x | 9469.193 | Mostly constructor-heavy; no direct bypass and no module read-context gate. This one-run snapshot improved against the historical baseline, but it remains a small-module context metric rather than evidence for the parser-module gate. |

## Current exact-pipeline snapshot

Source:

```text
bench/results/parser-library-latest.tsv
```

| Module | Def count | Uses module read context | Module time (ms) | Def-bindings time (ms) |
| --- | ---: | --- | ---: | ---: |
| `ParserParityParser` | 914 | yes | 97803.532 | 97575.976 |
| `ParserParitySource` | 160 | yes | 328.365 | 233.725 |
| `ParserParityLexer` | 130 | no | 14587.420 | 14014.858 |
| `ParserParityParserCombinator` | 26 | no | 14426.187 | 14052.349 |
| `ParserParityAst` | 21 | no | 9887.806 | 9837.554 |

The gate is `moduleDefContextMinDefs = 150`. It avoids adding module-context
overhead to small ordinary modules such as Prelude while still covering the
large generated parser module.

## Directional detailed profile

Source:

```text
bench/results/parser-library-def-details.tsv
```

This one-run profile used `MLF_PROGRAM_TIMING_DEF_DETAILS=1`, so it is for
optimization direction only and is not comparable to the normal single-run
top-level snapshot.
For `ParserParityParser`, the largest exact-pipeline aggregates were:

| Aggregate suffix | Total (ms) |
| --- | ---: |
| `def.pipeline.elab_pipeline.presolution` | 68246.306 |
| `def.pipeline.elab_pipeline.presolution.edge_loop` | 51672.688 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.execute` | 27307.167 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.execute.expansion_unify` | 21446.057 |
| `def.pipeline.elab_pipeline.result_type_reconstruction` | 19643.217 |
| `def.pipeline.elab_pipeline.elaborate` | 16265.193 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.execute.expansion_unify.execute_omega` | 10371.963 |
| `def.pipeline.elab_pipeline.prepare_generalization` | 9791.551 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.plan` | 8405.774 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.execute.expansion_unify.apply_expansion` | 8363.146 |
| `def.pipeline.elab_pipeline.generalize_root` | 6094.144 |

The stable snapshot source-node read slice improved the detailed directional
presolution totals but did not produce a clear headline improvement. Treat it
as a small exact-pipeline local cleanup; the next large
opportunity is still algorithmic work in presolution edge execution, result-type
reconstruction, and generalization preparation.

The binding-model cache slice that reused snapshot paths in expansion binding
and raise-merge checks also did not produce a measurable parser-library
headline improvement in the single-run benchmark. The prepared typecheck-env
cache slice is in the same bucket. Keep both as exact-pipeline groundwork only;
do not count either as a speedup without a future repeated confirmation run.
A scheme-owner cache inside the binding snapshot was tested and rejected:
on `cross-module-let` it raised the one-run `program.check.modules` timing to
860.412 ms. The accepted replacement seeds owner/root indexes once when the
edge worklist is built and falls back to exact dynamic planning if the source
body root changes. This improved the normal parser-library headline, but the
opt-in detailed profile still leaves `edge_loop.execute.expansion_unify` as the
largest unsolved presolution target.

The generic indexed worklist queue change is accepted but modest: the queue is
now `Data.Sequence`-backed and requeues stale items directly from the stored
item table. Against the immediately previous accepted one-run parser-library
snapshot (`144353.242ms`), the current artifact is `141906.520ms`, a
`2446.722ms` (`1.69%`) improvement. A broader attempt to apply the same
sequence queue shape to the final unification solve worklist was measured and
rejected (`143505.206ms`) because it regressed the generated parser fixture.
Applying a similar indexed-workset shape to Phase 3 acyclicity was also tested
and rejected: both a full left-reachability index and endpoint reachability
memoization regressed the parser-library package benchmark, so acyclicity keeps
the original pairwise dependency scan.

The result-type view reuse, order-key traversal cleanup, typed app-helper
carry, dirty binding repair, typed μ result-roll carry, and known application
result-type reuse moved the one-run parser-library artifact from
`141906.520ms` to `130486.229ms`, an `11420.291ms` (`8.05%`) improvement.
The dirty binding-repair slice alone moved the immediately previous
parser-library snapshot from `136737.298ms` to `133332.860ms`, a `3404.438ms`
(`2.49%`) improvement, while `ParserParityParser.def-bindings` moved from
`92550.602ms` to `90975.131ms` (`1575.471ms`, `1.70%`). A raw shared
external-scheme graph was tested during this slice and rejected for now: it can
fail presolution with `ArityMismatch "applyExpansion" 7 2`, so production keeps
root-local external scheme copies until external scheme sharing is
copy/idempotence-aware.
The latest typed μ result-roll carry then moved the immediately previous
snapshot from `133332.860ms` to `132752.201ms`, a `580.659ms` (`0.44%`)
package improvement, while `ParserParityParser.def-bindings` moved from
`90975.131ms` to `90453.862ms` (`521.269ms`, `0.57%`).
Reusing known application result types in the same helper then moved
`132752.201ms` to `130486.229ms`, a `2265.972ms` (`1.71%`) package
improvement, while `ParserParityParser.def-bindings` moved from `90453.862ms`
to `88986.911ms` (`1466.951ms`, `1.62%`).
A follow-up root-generalization carry lets result-type reconstruction reuse a
just-computed root scheme only when the exact `(scope,target)` key matches. The
next result-type fallback index moved the current one-run artifact to
`129687.360ms` overall and `88066.676ms` for
`ParserParityParser.def-bindings`. Treat the one-root delta as small and within
one-run-noise territory, but the code keeps the exact pipeline and removes
repeated fallback summary and scheme-root-owner scans from
`ResultType.Fallback.Core`.
A narrow `ALetF` typecheck-carry experiment was tested after this and
rejected: reusing locally checked RHS variants moved the default artifact to
`130467.122ms` and batch2 to `201149.010ms`, so the code was reverted.
Adding focused edge-expansion subcounters moved the current one-run artifact to
`128672.769ms` overall and `88351.376ms` for
`ParserParityParser.def-bindings`. Treat this as measurement groundwork, not a
claimed algorithmic win: the normal `MLF_MODULE_DEF_BATCH_SIZE=2` diagnostic
artifact remains slower at `200427.987ms` overall and `158759.657ms` for
`ParserParityParser.def-bindings`.
A follow-up cached scheme-body lookup in the shared `ResultTypeView` kept the
exact pipeline and removed another graph-wide scan from result-type target
selection. The refreshed one-run artifact is noisy rather than a headline win:
`program.check.modules` is `130862.089ms`, while
`ParserParityParser.def-bindings` is `89397.987ms`. Treat this
as retained exact read-context cleanup, not proof that result-type-side caching
can solve the native multi-root regression by itself. A follow-up attempt to
reuse overlay read-model indexes instead of rebuilding them was measured and
rejected: it moved the one-root default to `130712.985ms` in that run and batch2
to `202139.784ms`, then was reverted. Root generalization now reuses the same
cached scheme-body target in `ResultTypeView`; the refreshed batch2 artifact
is `200081.198ms`, still far slower than the one-root default.

Native multi-root batching is available only as a diagnostic path through
`MLF_MODULE_DEF_BATCH_SIZE`; the production default remains `1`. Earlier
global multi-root attempts were measured and rejected because they made batch 2
roughly 1.5x-1.8x slower than one-root. Those runs showed no shared parser
edges, so the accepted follow-up changed the diagnostic path from one mutable
global graph to independent root-local partitions when ownership is exact.

The latest batch-2 capture clears the first production gate for the diagnostic
multi-root path: `program.check.modules` is `20.46%` faster than one-root and
`ParserParityParser.def-bindings` is `30.12%` faster. The win comes from
keeping independent root partitions exact but executing them concurrently in
opt-in batches; mutable graph nodes remain root-local and final validation is
unchanged. The production default remains `MLF_MODULE_DEF_BATCH_SIZE=1` until
the larger-batch setting is chosen deliberately and repeated runs confirm the
one-run shape. Batch 4, 8, and 16 below are the previous same-branch one-run
captures; the post-merge verification refreshed one-root and batch 2.

| Parser-library batch gate | One-root | Batch 2 | Batch 4 | Batch 8 | Batch 16 |
| --- | ---: | ---: | ---: | ---: | ---: |
| `program.check.modules` | 139511.385ms | 110967.252ms | 96035.435ms | 90997.719ms | 92018.259ms |
| Total improvement | 0.00% | 20.46% | 31.16% | 34.77% | 34.04% |
| `ParserParityParser.def-bindings` | 97575.976ms | 68182.797ms | 49566.815ms | 43200.135ms | 43844.366ms |
| Parser def improvement | 0.00% | 30.12% | 49.20% | 55.73% | 55.07% |
| Parser per def | 106.757ms | 74.598ms | 54.231ms | 47.265ms | 47.970ms |

Historical rejected experiments remain useful as guardrails: lazy shared
result-type fallback summaries, per-call alias-bound reify memoization,
pre-canonicalized annotation source types, module-owned external scheme graphs,
retained-child fallback indexes, seeded multi-root root generalizations,
edge-local `RaiseMerge` ancestry caches, eager `ResultTypeView` scope caches,
and equality-based elaboration term reuse all failed to beat the then-current
one-root or batch-2 artifacts. Do not repeat those shapes without a narrower
measured reason.

## Notes

- The production optimization is exact: it still runs constraint generation,
  normalization, acyclicity, presolution, generalization, elaboration, and final
  type checking.
- The useful retained pieces are module-level lowering, SCC classification,
  detailed timing labels, and benchmark artifacts.
- Per-definition nested pipeline timing is opt-in with
  `MLF_PROGRAM_TIMING_DEF_DETAILS=1`. The canonical benchmark script leaves it
  off so `bench/results/parser-library-latest.tsv` remains a lower-overhead
  top-level timing artifact.
- Bounded grouping is gated by `MLF_MODULE_DEF_BATCH_SIZE`. The production
  default remains one definition per exact pipeline until the faster one-run
  batch shape is confirmed with repeated runs and chosen deliberately.
- The direct annotated checker numbers are intentionally not counted as an
  accepted improvement because that path skipped checker semantics.
