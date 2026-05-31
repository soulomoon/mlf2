# parser-library performance table

Active fixture:

```text
test/programs/compiler-parser-parity/parser-library
```

Lower is better for every timing metric.

The baseline column is a historical parser-library snapshot captured before
module-level definition checking. Parser-library benchmark artifacts are now
captured as a single run because the end-to-end package path is still expensive
enough that repeat runs should be reserved for final confirmation after a
substantial change. The current column is generated from the accepted
worklist/timing/binding-strict artifact:

```text
bench/results/parser-library-worklist-timing-binding-strict-2026-05-30.tsv
```

Captured with:

```bash
./bench/run-benchmarks.sh \
  --runs 1 \
  --allow-status 1 \
  --no-build \
  --benchmark parser-library test/programs/compiler-parser-parity/parser-library \
  --output bench/results/parser-library-worklist-timing-binding-strict-2026-05-30.tsv
```

After the upstream master merge, module-context batching is the default for
multi-definition modules. `MLF_MODULE_DEF_BATCH_SIZE=1` no longer forces
single-root checking; values less than or equal to 1 use the default batch size
of 16. This table therefore treats the post-merge default as the current
production shape. The `one-root-post-master-merge` artifact name is historical;
the measured behavior is the post-merge default batch path.

The direct annotated checker was measured, then rejected as a production
optimization because it bypassed the full checker pipeline. It is retained
below only as a historical reference, not as the active implementation.

## Headline: per-definition scale

The parser target still pays tens of times too much per generated definition.

| System / metric | Workload | Total checked defs | Current total | Current per def | Ratio vs GHC reference |
| --- | --- | ---: | ---: | ---: | ---: |
| `mlf2` exact pipeline, `ParserParityParser.def-bindings` | generated `.mlfp` parser definitions | 914 | 15191.834ms | 16.621ms | 29.8x |
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

Most important hotspot from the historical detailed profile artifact:

| Exact-pipeline slice | Total (ms) | Approx per `ParserParityParser` def |
| --- | ---: | ---: |
| `presolution` | 68246.306 | 74.668ms |
| `presolution.edge_loop` | 51672.688 | 56.535ms |
| `result_type_reconstruction` | 19643.217 | 25.478ms over 771 result-typed defs |
| `prepare_generalization` | 9791.551 | 10.713ms |

This makes the next algorithmic target explicit: an indexed presolution edge
solver plus frozen/lazy external-scheme instantiation. More small read-context
caches are useful only if they reduce these totals.

| Metric | Before module def checker (ms) | Current default batch-16 exact path (ms) | Saved vs baseline (ms) | Reduction | Speedup | Rejected direct-bypass reference (ms) | Reading |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| `program.check.modules` | 161077.348 | 26391.237 | 134686.111 | 83.62% | 6.10x | 67796.970 | Direct bypass is removed; the current win comes from keeping the full pipeline while using module-context batching, root-local partitions, concurrent independent root finalization, cheaper binding structural queries, local typecheck env/free-var summaries, deduplicated presolution invalidation, and lower timing-label overhead. |
| `program.check.module.ParserParityParser` | 112892.114 | 15351.168 | 97540.946 | 86.40% | 7.35x | 23814.194 | Main target module remains the largest single module, but the default exact path is now faster than the rejected direct-bypass reference. |
| `program.check.module.ParserParityParser.def-bindings` | 112683.979 | 15191.834 | 97492.145 | 86.52% | 7.42x | 23622.957 | 914 generated defs still run the full checker path; the current default is about 16.621ms per generated parser definition. |
| `program.check.module.ParserParityParserCombinator` | 17656.853 | 2886.507 | 14770.346 | 83.65% | 6.12x | 15440.448 | The post-merge default also helps smaller multi-def modules because module-context batching no longer has the old high-def-count gate. |
| `program.check.module.ParserParityLexer` | 16997.185 | 2272.544 | 14724.641 | 86.63% | 7.48x | 15507.747 | Still exact checking, now using the default module-context batch path. |
| `program.check.module.ParserParityAst` | 10838.915 | 4041.654 | 6797.261 | 62.71% | 2.68x | 9469.193 | Mostly constructor-heavy; improved, but less dramatically than parser and lexer definition-heavy modules. |

## Current exact-pipeline snapshot

Source:

```text
bench/results/parser-library-worklist-timing-binding-strict-2026-05-30.tsv
```

| Module | Def count | Uses module read context | Module time (ms) | Def-bindings time (ms) |
| --- | ---: | --- | ---: | ---: |
| `ParserParityParser` | 914 | yes | 15351.168 | 15191.834 |
| `ParserParitySource` | 160 | yes | 118.828 | 52.057 |
| `ParserParityLexer` | 130 | yes | 2278.505 | 1907.081 |
| `ParserParityParserCombinator` | 26 | yes | 2893.544 | 2637.675 |
| `ParserParityAst` | 21 | yes | 3985.171 | 3958.351 |

The old `moduleDefContextMinDefs = 150` gate is gone. Multi-definition modules
use module-context batching by default, and the effective default batch size is
16 unless `MLF_MODULE_DEF_BATCH_SIZE` is set to a value greater than 1.

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
next result-type fallback index moved the then-current one-run artifact to
`129687.360ms` overall and `88066.676ms` for
`ParserParityParser.def-bindings`. Treat the one-root delta as small and within
one-run-noise territory, but the code keeps the exact pipeline and removes
repeated fallback summary and scheme-root-owner scans from
`ResultType.Fallback.Core`.
A narrow `ALetF` typecheck-carry experiment was tested after this and
rejected: reusing locally checked RHS variants moved the then-default artifact to
`130467.122ms` and batch2 to `201149.010ms`, so the code was reverted.
Adding focused edge-expansion subcounters moved the then-current one-run artifact to
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
rejected: it moved the then-one-root default to `130712.985ms` in that run and batch2
to `202139.784ms`, then was reverted. Root generalization now reuses the same
cached scheme-body target in `ResultTypeView`; the refreshed batch2 artifact
is `200081.198ms`, still far slower than the then-one-root default.

After the master merge, multi-root batching is no longer just a diagnostic
path. The default exact path uses module-context batching with effective batch
size 16, and root-local partitions run concurrently when a batch has multiple
independent roots. The temporary no-fork run below forced those same root-local
partitions through the sequential path to measure the concurrency contribution.

| Parser-library mode | Artifact | `program.check.modules` | `ParserParityParser.def-bindings` | Real time | Parser per def | Reading |
| --- | --- | ---: | ---: | ---: | ---: | --- |
| Historical one-root exact path | `bench/results/parser-library-latest.tsv` | 139511.385ms | 97575.976ms | 140930ms | 106.757ms | Pre-default-batch reference from the accepted exact-pipeline work. |
| Historical batch 16 | `bench/results/parser-library-batch16.tsv` | 92018.259ms | 43844.366ms | 92130ms | 47.970ms | Older same-branch batch-16 run before the master default-batching refresh. |
| Current default batch 16, forked root partitions | `bench/results/parser-library-worklist-timing-binding-strict-2026-05-30.tsv` | 26391.237ms | 15191.834ms | 26480ms | 16.621ms | Current production shape after the master merge plus accepted binding structural-query, typecheck env/free-var, presolution invalidation, timing-label, and binding traversal cleanup; `MLF_MODULE_DEF_BATCH_SIZE=1` falls back to default 16. |
| Batch 16, no fork | `bench/results/parser-library-batch16-no-fork.tsv` | 106012.112ms | 70580.820ms | 107390ms | 77.222ms | Temporary local patch that ran the same root partitions sequentially. |

The no-fork run is `3.54x` slower on `program.check.modules`, `4.09x` slower
on `ParserParityParser.def-bindings`, and `3.41x` slower by wall time. That
confirms the current speed is not just batching or smaller contexts; it also
depends on forking independent root-local partitions inside each batch.

A 2026-05-30 ten-worktree sweep measured each candidate sequentially to avoid
CPU contention and integrated only the repeatable winners. The same-checkpoint
control artifact was `bench/results/parser-library-base-control-2026-05-30.tsv`
at `29269.155ms` for `program.check.modules`, `16815.754ms` for
`ParserParityParser.def-bindings`, and `29850ms` real time. The accepted
combination of binding structural-query cleanup and local typecheck
env/free-var summaries moved the confirmed artifact to `26479.451ms`,
`15210.414ms`, and `26580ms`, respectively. Copy-scheme, omega/frontier,
edge-plan, result-type overlay/fallback, elaboration known-type, bounded worker
pool, and interface-accumulator candidates were not integrated because their
single-candidate or on-top confirmation runs did not beat the accepted tree
repeatably.

A second 2026-05-30 ten-worktree sweep again measured candidates sequentially.
The same-checkpoint control artifact was
`bench/results/parser-library-round2-base-control-2026-05-30.tsv` at
`26446.085ms` for `program.check.modules`, `15230.649ms` for
`ParserParityParser.def-bindings`, and `26990ms` real time. The accepted
combination of presolution invalidation de-duplication, suffix-aware timing
helpers, and stricter binding traversal moved the confirmed artifact to
`26391.237ms`, `15191.834ms`, and `26480ms`, respectively. The other round-2
candidates were not integrated because confirmation runs either regressed the
accepted tree or failed to beat the same-window parent/control baseline.

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
  off so the normal parser-library artifacts remain lower-overhead top-level
  timing captures.
- Bounded grouping is controlled by `MLF_MODULE_DEF_BATCH_SIZE`, but only values
  greater than 1 override the default. Values less than or equal to 1 use the
  default batch size of 16.
- The direct annotated checker numbers are intentionally not counted as an
  accepted improvement because that path skipped checker semantics.
