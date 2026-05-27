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
| `mlf2` exact pipeline, `ParserParityParser.def-bindings` | generated `.mlfp` parser definitions | 914 | 96953.643ms | 106.076ms | 373.5x |
| GHC 9.12.2 local reference, `-fforce-recomp -fno-code -O0` | synthetic module with 914 parser-shaped top-level defs | 914 | 260.000ms | 0.284ms | 1.0x |

The GHC row is not an apples-to-apples semantic comparison; it is a local
scale reference for how fast a mature module checker can process many simple
top-level definitions when it shares module state. The important signal is the
gap: our generated parser library is still paying per-definition graph
construction, presolution edge processing, generalization preparation, and
result-type reconstruction costs that should be amortized or indexed.

Reference artifact:

```text
bench/results/ghc-per-def-reference.tsv
```

Reproduction command for the GHC reference:

```bash
tmpdir=$(mktemp -d)
src="$tmpdir/GhcPerDef.hs"
# Write a module with 914 parser-shaped top-level definitions, then:
/usr/bin/time -p ghc -fforce-recomp -fno-code -O0 -v0 "$src" +RTS -A64m
rm -rf "$tmpdir"
```

Most important hotspot from the detailed profile artifact:

| Exact-pipeline slice | Total (ms) | Approx per `ParserParityParser` def |
| --- | ---: | ---: |
| `presolution` | 65261.700 | 71.402ms |
| `presolution.edge_loop` | 48288.800 | 52.832ms |
| `result_type_reconstruction` | 19609.471 | 25.434ms over 771 result-typed defs |
| `prepare_generalization` | 9706.877 | 10.621ms |

This makes the next algorithmic target explicit: an indexed presolution edge
solver plus frozen/lazy external-scheme instantiation. More small read-context
caches are useful only if they reduce these totals.

| Metric | Before module def checker (ms) | Current exact module read context (ms) | Saved vs baseline (ms) | Reduction | Speedup | Rejected direct-bypass reference (ms) | Reading |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| `program.check.modules` | 161077.348 | 142332.496 | 18744.852 | 11.64% | 1.13x | 67796.970 | Direct bypass is removed; the current win comes from keeping the full pipeline while sharing exact read-only prep, carrying edge instantiation metadata, reusing result-type/source-type read context, carrying elaboration typecheck environments incrementally, sharing prepared elaboration/result-type read models, reusing result-type reification read models, avoiding duplicate edge-local interior root lookups, indexing read-model binding children for reification, reusing the base read-model var-only node map in result-type reconstruction, avoiding edge-local order-key construction for zero/one-binder edges, reading presolution copy sources from the stable snapshot, and selectively materializing only referenced external schemes. |
| `program.check.module.ParserParityParser` | 112892.114 | 97185.657 | 15706.457 | 13.91% | 1.16x | 23814.194 | Main target module remains the dominant cost; every accepted definition still runs the normal checker path. |
| `program.check.module.ParserParityParser.def-bindings` | 112683.979 | 96953.643 | 15730.336 | 13.96% | 1.16x | 23622.957 | 914 generated defs pay the full pipeline; the per-def cost is still about 106.076ms, so the remaining win requires algorithmic sharing/indexing rather than only prepared-source caches. |
| `program.check.module.ParserParityParserCombinator` | 17656.853 | 15582.880 | 2073.973 | 11.75% | 1.13x | 15440.448 | Below the module read-context gate; compare as current checkout context, not direct evidence for the gate. |
| `program.check.module.ParserParityLexer` | 16997.185 | 15709.798 | 1287.387 | 7.57% | 1.08x | 15507.747 | Below the module read-context gate at 130 defs; still full per-def pipeline. |
| `program.check.module.ParserParityAst` | 10838.915 | 10540.400 | 298.515 | 2.75% | 1.03x | 9469.193 | Mostly constructor-heavy; no direct bypass and no module read-context gate. |

## Current exact-pipeline snapshot

Source:

```text
bench/results/parser-library-latest.tsv
```

| Module | Def count | Uses module read context | Module time (ms) | Def-bindings time (ms) |
| --- | ---: | --- | ---: | ---: |
| `ParserParityParser` | 914 | yes | 97185.657 | 96953.643 |
| `ParserParitySource` | 160 | yes | 376.906 | 274.319 |
| `ParserParityLexer` | 130 | no | 15709.798 | 15088.786 |
| `ParserParityParserCombinator` | 26 | no | 15582.880 | 15188.455 |
| `ParserParityAst` | 21 | no | 10540.400 | 10500.032 |

The gate is `moduleDefContextMinDefs = 150`. It avoids adding module-context
overhead to small ordinary modules such as Prelude while still covering the
large generated parser module.

## Directional detailed profile

Source:

```text
bench/results/parser-library-def-details-latest.runs.tsv
```

This one-run profile used `MLF_PROGRAM_TIMING_DEF_DETAILS=1`, so it is for
optimization direction only and is not comparable to the normal single-run
top-level snapshot.
For `ParserParityParser`, the largest exact-pipeline aggregates were:

| Aggregate suffix | Total (ms) |
| --- | ---: |
| `def.pipeline.elab_pipeline.presolution` | 65261.700 |
| `def.pipeline.elab_pipeline.presolution.edge_loop` | 48288.800 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.execute` | 25190.934 |
| `def.pipeline.elab_pipeline.result_type_reconstruction` | 19609.471 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.execute.expansion_unify` | 19529.432 |
| `def.pipeline.elab_pipeline.elaborate` | 16114.253 |
| `def.pipeline.elab_pipeline.prepare_generalization` | 9706.877 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.execute.expansion_unify.execute_omega` | 8822.314 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.plan` | 8167.935 |
| `def.pipeline.elab_pipeline.presolution.edge_loop.execute.expansion_unify.apply_expansion` | 8028.216 |
| `def.pipeline.elab_pipeline.generalize_root` | 5958.316 |

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
A scheme-owner cache inside the binding snapshot was also tested and rejected:
on `cross-module-let` it raised the one-run `program.check.modules` timing to
860.412 ms, so owner lookup needs a cheaper index built once with the edge
worklist rather than per-edge mutable cache inserts.

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
