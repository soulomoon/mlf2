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
| `mlf2` exact pipeline, `ParserParityParser.def-bindings` | generated `.mlfp` parser definitions | 914 | 95814.535ms | 104.830ms | 187.9x |
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
| `program.check.modules` | 161077.348 | 141906.520 | 19170.828 | 11.90% | 1.14x | 67796.970 | Direct bypass is removed; the current win comes from keeping the full pipeline while sharing exact read-only prep, carrying edge instantiation metadata, reusing result-type/source-type read context, carrying elaboration typecheck environments incrementally, sharing prepared elaboration/result-type read models, reusing result-type reification read models, avoiding duplicate edge-local interior root lookups, indexing read-model binding children for reification, reusing the base read-model var-only node map in result-type reconstruction, avoiding edge-local order-key construction for zero/one-binder edges, reading presolution copy sources from the stable snapshot, selectively materializing only referenced external schemes, seeding presolution edge owner/root indexes once per edge loop, recording guarded edge fingerprints without replaying non-idempotent processed expansions, and using a sequence-backed generic indexed worklist queue with direct requeue. |
| `program.check.module.ParserParityParser` | 112892.114 | 96016.283 | 16875.831 | 14.95% | 1.18x | 23814.194 | Main target module remains the dominant cost; every accepted definition still runs the normal checker path. |
| `program.check.module.ParserParityParser.def-bindings` | 112683.979 | 95814.535 | 16869.444 | 14.97% | 1.18x | 23622.957 | 914 generated defs pay the full pipeline; the per-def cost is still about 104.830ms, so the remaining win requires deeper edge execution, result-type reconstruction, and generalization work. |
| `program.check.module.ParserParityParserCombinator` | 17656.853 | 15336.248 | 2320.605 | 13.14% | 1.15x | 15440.448 | Below the module read-context gate; compare as current checkout context, not direct evidence for the gate. |
| `program.check.module.ParserParityLexer` | 16997.185 | 15276.513 | 1720.672 | 10.12% | 1.11x | 15507.747 | Below the module read-context gate at 130 defs; still full per-def pipeline. |
| `program.check.module.ParserParityAst` | 10838.915 | 12316.624 | -1477.709 | -13.63% | 0.88x | 9469.193 | Mostly constructor-heavy; no direct bypass and no module read-context gate. This one-run snapshot regressed against the historical baseline and should be treated as variance until repeated. |

## Current exact-pipeline snapshot

Source:

```text
bench/results/parser-library-latest.tsv
```

| Module | Def count | Uses module read context | Module time (ms) | Def-bindings time (ms) |
| --- | ---: | --- | ---: | ---: |
| `ParserParityParser` | 914 | yes | 96016.283 | 95814.535 |
| `ParserParitySource` | 160 | yes | 358.613 | 259.145 |
| `ParserParityLexer` | 130 | no | 15276.513 | 14675.852 |
| `ParserParityParserCombinator` | 26 | no | 15336.248 | 14940.855 |
| `ParserParityAst` | 21 | no | 12316.624 | 12276.288 |

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
- Bounded grouping was tested and rejected: even 8-definition batches were
  superlinear on `ParserParityParser` and could fail before falling back.
- The direct annotated checker numbers are intentionally not counted as an
  accepted improvement because that path skipped checker semantics.
