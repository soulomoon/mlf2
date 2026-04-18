# Syntax Specification (eMLF + xMLF)

This document is the syntax source of truth for parser and pretty-printer behavior.

## Transition Compatibility Reference

Canonical xMLF pretty-printers now emit the paper-aligned syntax described below. The legacy `MLF.Elab.Types.Pretty` output is retained here only as a parser-compatibility reference; current internal and CLI pretty-print paths no longer emit these spellings.

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

## Unified `.mlfp` Program Surface

`.mlfp` is now a unified frontend/pipeline entrypoint rather than a separate
language lane. `MLF.API` owns `.mlfp` parse/pretty, `MLF.Pipeline` elaborates
`.mlfp` through the shared eMLF/xMLF path, and `MLF.Program` survives only as
a compatibility shim. The Phase-0 freeze lives in
`docs/plans/2026-04-13-recursive-adt-syntax-freeze.md`; the implementation
currently accepts the following core shapes:

```ebnf
Program      ::= Module+
Module       ::= "module" UIdent ["export" "(" ExportItem ("," ExportItem)* ")"] "{" Import* Decl* "}"
Import       ::= "import" UIdent ["exposing" "(" ExportItem ("," ExportItem)* ")"] ";"
ExportItem   ::= lIdent | UIdent | UIdent "(..)"

Decl         ::= DataDecl | ClassDecl | InstanceDecl | DefDecl
DataDecl     ::= "data" UIdent lIdent* "=" CtorDecl ("|" CtorDecl)* ["deriving" UIdent ("," UIdent)*] ";"
CtorDecl     ::= UIdent ":" SrcType
ClassDecl    ::= "class" UIdent lIdent "{" MethodSig* "}"
MethodSig    ::= lIdent ":" SrcType ";"
InstanceDecl ::= "instance" UIdent SrcType "{" MethodDef* "}"
MethodDef    ::= lIdent "=" Expr ";"
DefDecl      ::= "def" lIdent ":" SrcType "=" Expr ";"

Expr         ::= lIdent | UIdent | Literal
               | "\" Param Expr
               | Expr Expr
               | "let" lIdent [":" SrcType] "=" Expr "in" Expr
               | "case" Expr "of" "{" Alt (";" Alt)* "}"
               | "(" Expr ":" SrcType ")"

Param        ::= lIdent | "(" lIdent [":" SrcType] ")"
Alt          ::= Pattern "->" Expr
Pattern      ::= UIdent lIdent* | lIdent | "_"
```

Current checked/evaluated recursive-ADT corpus examples live under
`test/programs/recursive-adt/`.

You can run a program file with:

```bash
cabal run mlf2 -- run-program test/programs/recursive-adt/plain-recursive-nat.mlfp
```
