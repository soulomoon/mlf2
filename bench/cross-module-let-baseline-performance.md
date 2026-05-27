# cross-module-let performance table

Active fixture:

```text
test/programs/packages/cross-module-let
```

Historical generated-baseline source records:

```text
bench/baselines/cross-module-let-baseline.before-group-finalizer.tsv
bench/baselines/cross-module-let-baseline.after-group-finalizer.tsv
```

Lower is better for every timing metric.

| Metric | Before group finalizer (ms) | After group finalizer (ms) | After edge-loop opt (ms) | After witness/rewrite opt (ms) | After elab inline-context opt (ms) | After typecheck/op opt (ms) | After presolution working-model opt (ms) | After edge metadata carry (ms) | Rejected elab+incremental-binding attempt (ms) | Group-finalizer saved (ms) | Group-finalizer reduction | Group-finalizer speedup | Next optimization focus |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| `program.check.modules` | 2127.781 | 1211.706 | 1196.398 | 1016.989 | 1024.736 | 1019.861 | 857.844 | 851.585 | 879.810 | 916.075 | 43.05% | 1.76x | Incremental binding repair was rejected; the full inline repair path is restored. Remaining work should target real local repair or elaborate cost without accepting this regression. |
| `program.check.module.Prelude.constructor-bindings` | 392.249 | 65.408 | 62.305 | 58.606 | 61.195 | 60.588 | 57.494 | 54.454 | 59.512 | 326.841 | 83.32% | 6.00x | Recheck variance before attributing to this slice. |
| `program.check.module.Prelude.instance-bindings` | 1319.084 | 1042.074 | 1019.616 | 859.942 | 852.569 | 850.402 | 690.693 | 663.637 | 706.886 | 277.010 | 21.00% | 1.27x | Continue reducing the grouped instance finalizer and the remaining elaboration phase. |
| `program.check.operation.Prelude.instance_methods_group_finalize` | n/a | 1030.465 | 1008.830 | 849.574 | 840.783 | 838.752 | 678.739 | 653.319 | 695.363 | n/a | n/a | n/a | Main post-change hot spot; do not count the incremental binding attempt as an accepted optimization. |
| `program.check.module.Prelude.def-bindings` | 372.425 | 74.193 | 76.930 | 70.571 | 79.181 | 78.364 | 77.789 | 103.435 | 84.357 | 298.232 | 80.08% | 5.02x | Recheck variance before attributing to this slice. |
| `real` | 2260.000 | 1240.000 | 1230.000 | 1050.000 | 1050.000 | 1050.000 | 900.000 | 880.000 | n/a | 1020.000 | 45.13% | 1.82x | Use as wall-clock smoke only. |

When adding later optimization rounds, append new columns rather than replacing
the before/after columns. That keeps the historical comparison visible.

## Current repeated-run medians

Source:

```text
bench/results/latest.tsv
```

Captured with:

```bash
./bench/run-benchmarks.sh --runs 1 --output bench/results/latest.tsv
```

| Benchmark | `program.check.modules` median (ms) | `Prelude.instance-bindings` median (ms) | `Prelude.instance_methods_group_finalize` median (ms) | `real` median (ms) | Reading |
| --- | ---: | ---: | ---: | ---: | --- |
| `test/programs/packages/cross-module-let` | 730.263 | n/a | n/a | 2000.000 | This one-run snapshot hit the module-level Prelude cache (`program.check.module.Prelude.cache = 702.747ms`), so the old grouped instance subphase rows are not emitted in `bench/results/latest.tsv`. Use this as the current end-to-end fixture timing, not as a fine group-finalizer profile. |

The generic indexed worklist queue now uses `Data.Sequence` and appends
requeued stale items directly from the stored item table. A matching
`Data.Sequence` change to the final unification solve queue was measured and
rejected because it regressed both this fixture and parser-library.

## Historical group-finalizer subphase medians

Historical source:

```text
earlier bench/results/latest.tsv snapshots before the module-cache timing path
```

| Benchmark | Group 1 pipeline (ms) | Group 1 deferred obligations (ms) | Group 1 extract (ms) | Group 1 binding checks total (ms) | Group 2 pipeline (ms) | Total group finalize (ms) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `test/programs/packages/cross-module-let` | 609.642 | 1.836 | 0.358 | 1.319 | 42.373 | 664.112 |

The main optimization target remains the first grouped pipeline run:
`program.check.operation.Prelude.instance_methods_group_finalize.group_1.pipeline`.
Deferred rewrites, extraction, and per-binding checks are not material in these
benchmarks.

## Historical group-1 pipeline subphase medians

Comparison source:

```text
bench/results/latest.tsv
```

The current exact module read-context gate does not target this grouped Prelude
pipeline. The latest hot subphases are `presolution` at 360.117 ms and
`elaborate` at 191.970 ms in `bench/results/latest.tsv`.

| Pipeline subphase | Previous deep profile (ms) | After edge-loop opt (ms) | After witness/rewrite opt (ms) | After elab inline-context opt (ms) | After typecheck/op opt (ms) | After presolution working-model opt (ms) | Rejected elab+incremental-binding attempt (ms) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `prepare_external_bindings` | 0.477 | 0.429 | 0.397 | 0.418 | 0.407 | 0.438 | 0.432 |
| `normalize_surface` | 0.451 | 0.401 | 0.370 | 0.378 | 0.391 | 0.401 | 0.383 |
| `elab_pipeline.validate_annotations` | 0.519 | 0.460 | 0.424 | 0.446 | 0.451 | 0.434 | 0.433 |
| `elab_pipeline.generate_constraints` | 1.410 | 1.326 | 1.155 | 1.183 | 1.255 | 1.210 | 1.207 |
| `elab_pipeline.constraint_normalize` | 0.473 | 0.425 | 0.392 | 0.423 | 0.429 | 0.418 | 0.410 |
| `elab_pipeline.acyclicity` | 2.018 | 1.841 | 1.796 | 1.874 | 1.671 | 1.991 | 1.813 |
| `elab_pipeline.presolution` | 836.418 | 626.150 | 483.816 | 505.165 | 506.035 | 340.390 | 344.632 |
| `elab_pipeline.prepare_generalization` | 42.544 | 38.312 | 37.623 | 38.671 | 36.857 | 36.816 | 36.991 |
| `elab_pipeline.elaborate` | 293.527 | 266.101 | 256.149 | 221.514 | 218.539 | 224.095 | 235.183 |
| `elab_pipeline.generalize_root` | 6.042 | 5.799 | 5.537 | 5.765 | 5.982 | 5.792 | 6.508 |
| `elab_pipeline.subst_root` | 0.442 | 0.397 | 0.370 | 0.393 | 0.411 | 0.400 | 0.398 |
| `elab_pipeline.close_term` | 3.225 | 2.842 | 4.842 | 2.324 | 2.440 | 2.266 | 1.671 |
| `elab_pipeline.freshen_type_abs` | 0.448 | 0.413 | 0.386 | 0.446 | 0.421 | 0.414 | 0.417 |

After rejecting the incremental-binding slice, `presolution` is about 345 ms
and remains the largest group-1 pipeline phase. `elaborate` is second at about
235 ms.

## Historical group-1 presolution subphase medians

Source:

```text
bench/results/latest.tsv
```

| Presolution subphase | Previous deep profile (ms) | After edge-loop opt (ms) | After witness/rewrite opt (ms) | After elab inline-context opt (ms) | After typecheck/op opt (ms) | After presolution working-model opt (ms) | Rejected elab+incremental-binding attempt (ms) | After indexed edge seeds (ms) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `init` | 0.476 | 0.419 | 0.397 | 0.403 | 0.566 | 0.457 | 0.411 | 0.506 |
| `edge_loop` | 418.294 | 298.902 | 288.862 | 298.713 | 296.882 | 308.348 | 310.075 | 329.505 |
| `edge_loop.validation` | 9.220 | 9.491 | 8.966 | 9.387 | 8.501 | 8.677 | 20.667 | 9.867 |
| `edge_loop.index` | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 0.152 |
| `edge_loop.plan` | 87.672 | 80.388 | 80.752 | 79.026 | 78.670 | 96.570 | 97.405 | 105.221 |
| `edge_loop.schedule_weakens` | 0.296 | 0.273 | 0.132 | 0.245 | 0.240 | 0.288 | 0.282 | 0.230 |
| `edge_loop.execute` | 308.184 | 198.383 | 189.121 | 198.205 | 198.665 | 181.413 | 181.284 | 201.920 |
| `edge_loop.canonicalize_trace_interiors` | 0.045 | 0.025 | 0.021 | 0.039 | 0.028 | 0.033 | 0.035 | 0.023 |
| `edge_loop.drain_unify_closure` | 0.015 | 0.022 | 0.006 | 0.011 | 0.014 | 0.014 | 0.014 | 0.015 |
| `finalize.materialize_expansions` | 86.340 | 36.000 | 35.801 | 36.408 | 37.092 | 11.534 | 11.581 | 10.785 |
| `finalize.rewrite_constraint` | 124.930 | 116.963 | 60.267 | 72.896 | 72.477 | 5.065 | 5.457 | 4.840 |
| `finalize.rigidify_validate` | 2.036 | 1.277 | 1.346 | 1.318 | 1.214 | 1.288 | 1.368 | 1.333 |
| `finalize.normalize_witnesses` | 184.035 | 160.864 | 86.291 | 87.979 | 87.145 | 4.023 | 3.997 | 3.623 |
| `post_validate` | 1.044 | 0.972 | 0.885 | 0.916 | 0.878 | 0.891 | 0.892 | 1.126 |

The incremental binding-repair split is no longer in the hot path. The old full
inline repair body was restored because the validation-only incremental path
regressed the focused benchmark instead of improving it.

## Historical group-1 edge-execute subphase medians

Source:

```text
bench/results/latest.tsv
```

| Edge execute subphase | Previous deep profile (ms) | After edge-loop opt (ms) | After witness/rewrite opt (ms) | After elab inline-context opt (ms) | After typecheck/op opt (ms) | After presolution working-model opt (ms) | Rejected elab+incremental-binding attempt (ms) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `decide_expansion` | 62.184 | 56.284 | 46.341 | 55.506 | 48.888 | 47.813 | 47.166 |
| `record_expansion` | 0.032 | 0.023 | 0.009 | 0.026 | 0.020 | 0.023 | 0.029 |
| `unify_structure` | 0.812 | 0.888 | 0.626 | 0.893 | 0.684 | 0.821 | 0.768 |
| `witness_plan` | 0.294 | 0.197 | 0.105 | 0.206 | 0.188 | 0.233 | 0.233 |
| `expansion_unify` | 215.941 | 115.402 | 116.256 | 115.346 | 125.783 | 107.195 | 106.896 |
| `record_trace` | 28.811 | 26.076 | 26.236 | 25.959 | 23.008 | 25.840 | 26.639 |
| `record_witness` | 0.091 | 0.067 | 0.034 | 0.075 | 0.075 | 0.079 | 0.089 |

Expansion unification is still the largest edge-execute subphase. Trace
recording stays near its previous cost because the narrow trace-interior
shortcut was not correctness-preserving for Phi context recovery.

## Historical group-1 expansion-unify subphase medians

Source:

```text
bench/results/latest.tsv
```

| Expansion-unify subphase | Previous deep profile (ms) | After edge-loop opt (ms) | After witness/rewrite opt (ms) | After elab inline-context opt (ms) | After typecheck/op opt (ms) | After presolution working-model opt (ms) | Rejected elab+incremental-binding attempt (ms) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `apply_expansion` | 125.205 | 30.216 | 35.863 | 30.479 | 35.985 | 31.123 | 30.392 |
| `bind_root` | 14.371 | 14.247 | 13.146 | 14.125 | 13.982 | 19.239 | 19.647 |
| `prepare_omega` | 15.348 | 14.724 | 14.139 | 14.884 | 14.679 | 15.418 | 15.350 |
| `execute_omega` | 60.869 | 55.928 | 53.152 | 56.089 | 61.226 | 41.774 | 41.655 |
| `finish` | 0.129 | 0.117 | 0.073 | 0.114 | 0.110 | 0.127 | 0.138 |

The working-model slice reduced `execute_omega`, and rejecting incremental
binding repair keeps that win. `expansion_unify` remains the largest execution
subphase; any future binding-repair work needs a real local repair model before
it should replace the full repair path again.
