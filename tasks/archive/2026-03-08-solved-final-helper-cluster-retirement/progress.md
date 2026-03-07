# Progress Log

## 2026-03-08
- Started the final facade-cleanup task aimed at retiring the remaining non-must-stay helper cluster from `MLF.Constraint.Solved`.
- Retired the last non-must-stay public `Solved` helper cluster by replacing owner-local use with direct constraint/canonical logic in `Reify.Core`, `Presolution.View`, and test parity helpers.
- Cleaned the new local warnings by removing the now-unused `Solved` import from `ScopeSpec` and renaming the two local helper parameters in `Reify.Core`.
