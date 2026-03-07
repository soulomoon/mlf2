# Findings

- The lexer, identifier, token, and literal parsing helpers were duplicated almost verbatim between `MLF.Frontend.Parse` and `MLF.XMLF.Parse`.
- The type grammar was also largely duplicated, but XMLF needed one important nuance: after the first forall binder, an additional binder sequence must be dot-terminated to avoid greedily consuming the body.
- That nuance made a shared parser core viable only with a binder-list strategy hook, not with one fixed generic `forall` binder-sequence rule.
