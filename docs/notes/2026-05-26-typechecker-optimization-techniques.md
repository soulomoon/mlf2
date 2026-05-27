# Typechecker Optimization Techniques

Date: 2026-05-26

This note records external optimization techniques relevant to the current
checker benchmarks and maps them to the MLF implementation. It is not an
accepted design yet; it is a shortlist for focused profiling and future
optimization work.

## Current Benchmark Shapes

### `cross-module-let`

Focused fixture: `test/programs/packages/cross-module-let`

Current median timings from `bench/results/latest.tsv`:

| Metric | Median |
| --- | ---: |
| `program.check.modules` | 913.937ms |
| `program.check.module.Prelude` | 890.882ms |
| `program.check.module.Prelude.instance-bindings` | 701.716ms |
| `program.check.operation.Prelude.instance_methods_group_finalize` | 664.112ms |
| `...group_1.pipeline` | 609.642ms |
| `...group_1.pipeline.elab_pipeline.presolution` | 360.117ms |
| `...group_1.pipeline.elab_pipeline.presolution.edge_loop` | 317.712ms |
| `...group_1.pipeline.elab_pipeline.elaborate` | 191.970ms |

The source-level Prelude is small, but the generated instance-method group is
large enough semantically to dominate the run. The anomaly to explain is not
ordinary module size; it is the cost of sending generated Prelude/typeclass
method bodies through the full generic MLF presolution and elaboration path.

### `parser-library`

Focused fixture: `test/programs/compiler-parser-parity/parser-library`

Command used:

```bash
./bench/run-benchmarks.sh \
  --runs 1 \
  --allow-status 1 \
  --benchmark parser-library test/programs/compiler-parser-parity/parser-library \
  --output bench/results/parser-library-latest.tsv
```

The package root is a library fixture, so the CLI exits with status `1` after
checking all modules because `main` is intentionally absent. The benchmark
allows that status and still records the complete module-check timings.

Current single-run timings from `bench/results/parser-library-latest.tsv`:

| Metric | Time |
| --- | ---: |
| `real` | 143430.000ms |
| `program.check.modules` | 142332.496ms |
| `program.check.module.ParserParityParser` | 97185.657ms |
| `program.check.module.ParserParityParser.def-bindings` | 96953.643ms |
| `program.check.module.ParserParityParserCombinator` | 15582.880ms |
| `program.check.module.ParserParityParserCombinator.def-bindings` | 15188.455ms |
| `program.check.module.ParserParityLexer` | 15709.798ms |
| `program.check.module.ParserParityLexer.def-bindings` | 15088.786ms |
| `program.check.module.ParserParityAst` | 10540.400ms |
| `program.check.module.ParserParityAst.def-bindings` | 10500.032ms |

Aggregated ordinary definition timings:

| Module | Def count | Sum | Mean | Slowest def |
| --- | ---: | ---: | ---: | --- |
| `ParserParityParser` | 914 | 96953.643ms | 106.076ms | see `bench/results/parser-library-def-details-latest.tsv` |
| `ParserParityParserCombinator` | 26 | 15188.455ms | 584.171ms | see `bench/results/parser-library-def-details-latest.tsv` |
| `ParserParityLexer` | 130 | 15088.786ms | 116.068ms | see `bench/results/parser-library-def-details-latest.tsv` |
| `ParserParityAst` | 21 | 10500.032ms | 500.002ms | see `bench/results/parser-library-def-details-latest.tsv` |

Across all direct `def_` operations, 210 definitions are at least 100ms, 21 are
at least 500ms, and 5 are at least 1000ms. This benchmark is therefore not
mostly a Prelude/typeclass-group problem. It is dominated by many independent
definition finalizations, each paying a substantial fixed pipeline cost. The
current exact-pipeline reduction came from sharing prepared module context,
reusing already-softened read-model binding parents with a child index for
reification, and selectively materializing only referenced external schemes
before constraint generation.

One attempted owner-cache variant was rejected after measurement: caching
scheme owners by mutating the per-edge binding snapshot raised
`cross-module-let` `program.check.modules` to 860.412 ms. The better direction is
to build owner/root indexes once as part of an edge worklist/inert model, not by
inserting into a cache during every `planEdge` call.

## Techniques From Other Typecheckers

### Worklist and inert-set solving

GHC's constraint solver keeps active work in a worklist and solved constraints
in an inert set. Inert constraints are indexed by shape, and new equalities only
kick out work they can rewrite.

References:

- [GHC `InertSet` API documentation](https://downloads.haskell.org/ghc/9.14.0.20251128/docs/libraries/ghc-9.14.0.20251128-1ddb/GHC-Tc-Solver-InertSet.html)
- [GHC constraint solver overview](https://sheaf.github.io/ghc-constraint-solver/)

Relevance here:

- Current `edge_loop.plan` and `edge_loop.execute` still pay repeated
  canonical-root, binder-owner, and expansion-planning costs.
- The likely optimization is not another whole-loop cache. It is an indexed
  edge-work model where canonical roots, binder parents, and expansion
  obligations are stable query artifacts for the current read phase.
- This maps most directly to `MLF.Constraint.Presolution.EdgeProcessing`,
  `Expansion`, `Copy`, and `EdgeUnify`.

Expected payoff:

- Reduce `edge_loop` from roughly 308ms toward 150-200ms on this fixture if
  the repeated edge planning and expansion queries are the dominant cost.

### Evidence and dictionary-directed typeclass solving

OutsideIn-style systems treat typeclass obligations as constraints with
evidence. GHC-style implementations solve wanted constraints against givens and
instances, producing evidence dictionaries rather than repeatedly elaborating
source-level candidates.

References:

- [OutsideIn(X): Modular type inference with local assumptions](https://simon.peytonjones.org/outsideinx/)
- [Complete and decidable type inference for GADTs](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/outsidein-icfp09.pdf)

Relevance here:

- Prelude derived and explicit instance methods are currently lowered, grouped,
  and finalized through the same generic surface pipeline used for ordinary
  source bindings.
- For generated `Eq` and builtin Prelude method bodies, we should consider a
  checked internal artifact path: method dictionary entries with typed evidence,
  accepted only after local validation, instead of a large grouped `let` body
  that re-enters full presolution.
- User-written instance methods should still use the generic path unless an
  equivalent typed evidence path is designed and tested.

Expected payoff:

- This is the largest current opportunity. It targets
  `Prelude.instance_methods_group_finalize = 678.739ms` and may bring
  Prelude-from-source toward 50-150ms if generated Prelude methods stop using
  the expensive generic group finalizer.

### Level-based generalization

Rémy/OCaml-style levels make generalization local by attaching level metadata to
type nodes. Generalization then avoids repeatedly scanning whole environments
or copying schema parts that do not mention quantified variables.

References:

- [Efficient and Insightful Generalization](https://okmij.org/ftp/ML/generalization.html)
- [OCaml type representation notes](https://ocaml.org/p/merlin-lib/5.5-503/merlin-lib.ocaml_typing/Ocaml_typing/Types/index.html)

Relevance here:

- Current elaborated typechecking and generalization paths still contain
  repeated environment-free-variable scans, type normalization, and schema/type
  copying.
- A level-style internal representation would let the checker carry
  generalization boundaries directly instead of rediscovering them from binding
  trees and free-variable traversals.
- This is a deeper data-model change than local caching. It should be measured
  after the current generated-instance hotspot is isolated.

Expected payoff:

- Most likely a broad constant-factor improvement across elaboration,
  generalization preparation, and typechecking. It is less targeted than the
  generated-instance path, but more structurally important.

### Constraint-based first-class polymorphism

Modern first-class-polymorphism systems separate constraint generation from a
deterministic solver and use annotations to control when polymorphic values are
instantiated or generalized.

Reference:

- [Constraint-based type inference for FreezeML](https://arxiv.org/abs/2207.09914)

Relevance here:

- The current elaborator uses repeated `typeCheckWithEnv` calls as a candidate
  validator while building terms. That conflates elaboration search with
  checking.
- A better design is to generate a typed elaboration artifact once, carry known
  types through local helper boundaries, and use a deterministic validation pass
  instead of repeatedly rechecking candidate terms.

Expected payoff:

- Reduce `elaborate = 224.095ms` substantially, plausibly toward 80-130ms after
  the hot application, recursive type, and method-body helper paths carry typed
  results instead of recomputing them.

### Query-based incremental compilation

Rustc and Salsa organize compiler work as keyed queries. Query results are
memoized, dependencies between query invocations are tracked, and unchanged
inputs can avoid recomputing downstream results.

References:

- [Rustc incremental compilation and red-green query algorithm](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation.html)
- [Salsa overview](https://github.com/salsa-rs/salsa)
- [Salsa algorithm reference](https://salsa-rs.github.io/salsa/reference/algorithm.html)

Relevance here:

- The `parser-library` benchmark is dominated by ordinary definition checks:
  `ParserParityParser.def-bindings = 112683.979ms` and 914 parser definitions
  average 119.324ms each.
- A package-level query model would key parse, resolve, lower, constraint,
  presolution, checked binding, and module-interface artifacts by stable
  module/definition identity plus dependency fingerprints.
- This is more useful for large generated libraries than another Prelude-only
  optimization because most parser-library work is per-definition repetition.

Expected payoff:

- For clean from-scratch runs, query boundaries mainly improve sharing and make
  hotspots visible. For repeat runs or parser-parity suites, checked artifacts
  could avoid rechecking unchanged parser-library definitions entirely.

### Separate compilation and interface fingerprints

Traditional separate compilation stores a checked module interface beside the
compiled object. GHC `.hi` files contain information needed by downstream
modules, and recompilation avoidance can stop early when needed interface
fingerprints have not changed.

Reference:

- [GHC User's Guide: filenames and separate compilation](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/separate_compilation.html)

Relevance here:

- MLF already has in-memory `ModuleInterface` and build-graph metadata, but the
  parser-library benchmark still rechecks every library module from source.
- Persisting checked module interfaces, and eventually checked binding
  artifacts, would make shared parser libraries behave like compiled
  dependencies instead of source copied into every check.
- This is also the clean answer for the earlier parser-parity slowness: the
  shared parser library should be checked once per content/interface change,
  not once per parity scenario.

Expected payoff:

- For repeated parity runs, this is the largest possible improvement:
  parser-library checking could drop from roughly 161s to interface load plus
  changed downstream fixture checking.

### Bidirectional and local type inference

Bidirectional systems split typing into synthesis and checking modes. Known
annotations guide local checking and prevent the checker from repeatedly
searching for types that are already supplied by the source or by a previous
elaboration step.

References:

- [Complete and easy bidirectional typechecking for higher-rank polymorphism](https://research.cs.queensu.ca/home/jana/papers/bidir/)
- [Practical type inference for arbitrary-rank types](https://repository.upenn.edu/server/api/core/bitstreams/7c1dc678-93c6-4516-98fd-f82b384eb75d/content)

Relevance here:

- Parser-library definitions are heavily annotated and generated. The checker
  should be able to check most bodies against expected types rather than
  infer/elaborate candidates and then re-typecheck them as validation.
- This points toward typed elaboration artifacts and helper APIs that accept
  known expected/actual types. It also supports using a cheaper checked path for
  simple annotated definitions.

Expected payoff:

- Reduce the per-definition fixed cost visible in `parser-library`, especially
  the 925 ordinary definitions currently taking at least 100ms each.

### Parallel declaration checking

Some typechecking workloads can be parallelized once dependency structure is
explicit. Independent declarations or modules can be checked concurrently when
they only read immutable interface data and produce separate checked artifacts.

Reference:

- [Parallel Type-checking with Haskell using Saturating LVars and Stream Generators](https://osa1.net/papers/type-checking-with-lvars.pdf)

Relevance here:

- `ParserParityParser` has hundreds of ordinary definitions. If dependency
  analysis can split strongly connected components and the finalization context
  becomes immutable/read-only, independent definition SCCs could be checked in
  parallel.
- This is a second-stage optimization. First the checker needs explicit
  per-definition artifacts and dependency fingerprints; otherwise parallelism
  risks multiplying the same repeated generic pipeline work.

Expected payoff:

- Potential wall-clock reduction on large generated modules, but only after the
  per-definition artifact boundary is clean enough to run safely in parallel.

## Optimization Direction

The current small Prelude result suggests the theoretical target is much lower
than the current benchmark:

| Strategy | Plausible Prelude compile impact |
| --- | ---: |
| Current generic group finalizer | ~837ms |
| Local hot-path fixes only | 450-550ms |
| Indexed presolution plus typed elaboration artifacts | 150-300ms |
| Generated Prelude/typeclass artifact path | 50-150ms |
| Cached/precompiled Prelude interface | <20ms |

The next optimization should first prove which generated bindings form
`instance_methods_group_finalize.group_1`, then compare two paths:

1. Continue optimizing the generic edge loop and elaborator for that group.
2. Add a typed generated-method artifact path for derived/builtin instance
   methods, preserving full generic checking for user-authored source.

If group 1 is mostly derived `Eq` or builtin Prelude methods, the second path is
likely the larger and cleaner improvement.

For `parser-library`, the direction is different:

1. Add finer timing inside `finalizeBindingWithContext` for ordinary
   definitions, because the current table only times each whole definition.
2. Introduce per-definition checked artifacts keyed by stable resolved symbol
   identity and dependency fingerprints.
3. Persist module interfaces and checked artifacts for library roots such as the
   parser library.
4. Move elaboration toward checking-against-known-types for annotated generated
   definitions.
5. Only after those boundaries exist, consider parallelizing independent
   definition SCCs.

The parser-library benchmark indicates that local edge-loop work alone cannot
solve large-library performance. The dominant problem is that each generated
definition pays the full generic finalization pipeline from scratch.
