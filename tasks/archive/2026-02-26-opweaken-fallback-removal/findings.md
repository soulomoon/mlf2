# Findings: OpWeaken Fallback Removal

## Key Discoveries
- `OpWeaken` replay had two silent non-root fallback exits in `MLF.Elab.Phi.Omega`:
  - non-binder alias branch with no recoverable binder member,
  - binder branch when binder key is known but absent from `vSpineIds`.
- Both branches returned identity (`InstId`) instead of erroring.
- Removing those fallbacks exposed 31 legacy tests that implicitly depended on permissive no-op behavior; all were rebaselined to assert explicit strict fail-fast semantics.
