# Syntax Specification (eMLF + xMLF)

This document is the syntax source of truth for parser and pretty-printer behavior.

## Current Implemented Syntax (As-Is, pre-canonical migration)

Legacy `MLF.Elab.Types.Pretty` output that existed before this migration:

### Legacy Type Syntax

```ebnf
Type        ::= Var
              | Base
              | Type "->" Type
              | "∀" Var "." Type
              | "∀(" Var "⩾" Type ")." Type
              | "⊥"
              | Con Type+
```

### Legacy Instantiation Syntax

```ebnf
Inst        ::= "1"
              | "⟨" Type "⟩"
              | Type                  -- legacy InstBot shape
              | "O"
              | "N"
              | "!" Var
              | "∀(" Var "⩾) " Inst
              | "∀(⩾ " Inst ")"
              | Inst "; " Inst
```

### Legacy Term Syntax

```ebnf
Term        ::= Var
              | IntLit | BoolLit | StringLit
              | "λ" Var ":" Type ". " Term
              | "(" Term ") " Term
              | "let " Var " : " Scheme " = " Term " in " Term
              | "Λ" Var ". " Term
              | "Λ(" Var "⩾" Type "). " Term
              | Term " [" Inst "]"
```

## Canonical Target Syntax (Paper-Aligned)

### eMLF (from `emlf_typeing_rules.md`)

```ebnf
Expr        ::= x
              | "λ(" x ")" Expr
              | "λ(" x " : " SrcType ")" Expr
              | Expr Expr
              | "let " x " = " Expr " in " Expr
              | "(" Expr " : " SrcType ")"

SrcType     ::= α
              | SrcType "->" SrcType
              | C SrcType+
              | "∀" Binder+ "." SrcType
              | "⊥"
Binder      ::= α | "(" α "⩾" SrcType ")"
```

### xMLF (from `xmlf_typeing_rules.md`)

```ebnf
Type        ::= α
              | Type "->" Type
              | C Type+
              | "∀(" α "⩾" Type ")" Type
              | "⊥"

Comp        ::= "ε"
              | "⊲" Type
              | α "⊳"
              | "∀(⩾" Comp ")"
              | "∀(" α "⩾)" Comp
              | "N"
              | "O"
              | Comp ";" Comp

Term        ::= x
              | "λ(" x " : " Type ")" Term
              | Term Term
              | "Λ(" α "⩾" Type ")" Term
              | Term "[" Comp "]"
              | "let " x " = " Term " in " Term
```

### Accepted ASCII Aliases

- `\` for `λ`
- `forall` for `∀`
- `>=` for `⩾`
- `_|_` or `bottom` for `⊥`
- `epsilon` or `1` for `ε` (legacy acceptance)
- legacy `⟨τ⟩` and `!a` are accepted in xMLF parser as transition syntax

## Delta Table (Legacy -> Canonical)

| Legacy | Canonical |
|---|---|
| `1` | `ε` |
| `⟨τ⟩` | `∀(⩾ ⊲τ); N` |
| bare `τ` as InstBot | `⊲τ` |
| `!a` | `a⊳` |
| `Λa. t` | `Λ(a ⩾ ⊥) t` |
| `∀a. τ` (xMLF pretty) | `∀(a ⩾ ⊥) τ` |
| `λx:τ. t` | `λ(x : τ) t` |

## Normalization Rules

- Unicode is the default pretty-print style.
- `->` is right-associative.
- Application is left-associative.
- xMLF computation composition `;` is left-associative.
- Parentheses are emitted only when required by precedence.
- Reserved words: `let`, `in`, `forall`, `true`, `false`, `epsilon`.
- Comments accepted in parsers: `-- ...` and `{- ... -}`.

## Implementation Extensions

- eMLF supports literals (`Int`, `Bool`, `String`) as expression atoms.
- eMLF parser accepts `let x : τ = e in b` and desugars to `let x = (e : τ) in b`.
- xMLF term/type syntax includes base types as uppercase 0-ary constructors (e.g. `Int`, `Bool`).
- xMLF parser accepts legacy instantiation tokens (`1`, `⟨τ⟩`, `!a`) for transition, but pretty-printing is canonical.
- `ElabTerm` carries let schemes (`ELet String ElabScheme ...`); pretty output keeps `let x : scheme = ... in ...` as a repository-specific extension for debugging fidelity.
