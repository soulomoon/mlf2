# Findings

- `MLF.Elab.Phi.Binder` is internal-only and currently unused outside `MLF.Elab.Phi` re-exports.
- `Omega` already enforces the desired strict runtime contract with direct replay-spine binder membership checks.
- `IdentityBridge` is still valuable as a witness-domain utility/test surface, but repo notes explicitly say it is not the runtime target-repair engine.
- `PhiEnv` no longer carries the dead inverse-copy accessor path introduced only for `Phi.Binder`; the remaining environment surface is limited to live facade inputs.
- `IdentityBridge` API stays exported for tests/diagnostics, but its compatibility helpers are now documented as identity projections in strict witness-domain mode.
