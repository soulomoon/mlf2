# cross-module-let-baseline after group finalizer

Captured on 2026-05-25 from the local worktree after the instance-method group
finalizer was introduced.

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
program.check.modules                               1211.706ms
program.check.module.Prelude.constructor-bindings     65.408ms
program.check.module.Prelude.instance-bindings      1042.074ms
program.check.operation.Prelude.instance_methods_group_finalize
                                                    1030.465ms
program.check.module.Prelude.def-bindings             74.193ms
real                                                1240.000ms
user                                                1110.000ms
sys                                                   90.000ms
```

The matching machine-readable metrics are in
`bench/baselines/cross-module-let-baseline.after-group-finalizer.tsv`.
