# Findings

- `freshNameLike` is duplicated verbatim between `MLF.Frontend.Normalize` and `MLF.Reify.TypeOps`.
- `mapBound` is duplicated verbatim between `MLF.Elab.Run.TypeOps` and `MLF.Constraint.Presolution.Plan.Finalize`.
- The natural shared homes are `MLF.Util.Names` and `MLF.Elab.Types`; both are already part of the internal library surface, so no Cabal/module churn is needed.
- `MLF.Util.Names` and `MLF.Elab.Types` were the right homes because both are already foundational internal utility/model modules with no new cycle pressure from the extracted helpers.
