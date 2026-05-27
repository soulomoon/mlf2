# cross-module-let-baseline before group finalizer

Captured on 2026-05-25 from the local worktree before the instance-method
group finalizer was introduced. The tree already included the timing
instrumentation and module-level finalization context, so this is a baseline for
the group-finalizer change rather than a pristine release baseline.

Fixture:

```text
bench/benchmarks/cross-module-let-baseline
```

Command:

```bash
MLF_PROGRAM_TIMING_DETAIL=1 MLF_PROGRAM_TIMING_OPERATIONS=1 \
  "$(cabal list-bin exe:mlf2)" check-program bench/benchmarks/cross-module-let-baseline
```

Summary:

```text
program.check.modules                               2127.781ms
program.check.module.Prelude.constructor-bindings    392.249ms
program.check.module.Prelude.instance-bindings      1319.084ms
program.check.module.Prelude.def-bindings            372.425ms
real                                                2260.000ms
user                                                1990.000ms
sys                                                  160.000ms
```

The matching machine-readable metrics are in
`bench/baselines/cross-module-let-baseline.before-group-finalizer.tsv`.
