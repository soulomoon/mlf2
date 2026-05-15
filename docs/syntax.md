# Syntax Specification (eMLF + xMLF)

This document is the syntax source of truth for parser and pretty-printer behavior.

## Canonical Parser Syntax (Paper-Aligned)

The raw eMLF and explicit xMLF parser APIs accept the canonical Unicode syntax
below. Legacy grammar forms and ASCII aliases are rejected; compatibility
spellings are not a standing parser contract.

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
              | α SrcType+
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

## Retired Legacy Syntax

These former compatibility forms are parse errors on the raw eMLF, explicit
xMLF, and `.mlfp` parser APIs where the token or grammar family applies:

- ASCII token aliases: `\`, `forall`, `>=`, `_|_`, `bottom`, `Lambda`,
  `epsilon`, and `1`.
- eMLF legacy lambdas such as `λx. t` and `λx:τ. t`.
- xMLF legacy computations such as `!a`, `⟨τ⟩`, and bare `τ` as a computation.
- xMLF legacy type and term forms such as `∀a. τ`, `Λa. t`, and `λx:τ. t`.

## Normalization Rules

- Unicode is the default pretty-print style.
- `->` is right-associative.
- Application is left-associative.
- xMLF computation composition `;` is left-associative.
- Parentheses are emitted only when required by precedence.
- Reserved words include active keywords and retired spellings that must fail as
  identifiers, including `let`, `in`, `mu`, `forall`, `bottom`, `true`,
  `false`, `epsilon`, `roll`, and `unroll` where applicable.
- Comments accepted in parsers: `-- ...` and `{- ... -}`.

## Implementation Extensions

- eMLF supports literals (`Int`, `Bool`, `String`) as expression atoms.
- eMLF parser accepts `let x : τ = e in b` and desugars to `let x = (e : τ) in b`.
- xMLF term/type syntax includes base types as uppercase 0-ary constructors (e.g. `Int`, `Bool`).
- xMLF parser accepts recursive term forms `roll[τ] t` and `unroll t`.
- `ElabTerm` carries let schemes (`ELet String ElabScheme ...`); pretty output keeps `let x : scheme = ... in ...` as a repository-specific extension for debugging fidelity.

## Unified `.mlfp` Program Surface

`.mlfp` is now a unified frontend/pipeline entrypoint rather than a separate
language lane. `MLF.API` owns `.mlfp` parse/pretty, `MLF.Pipeline` elaborates
`.mlfp` through the shared eMLF/xMLF path, and the old public program re-export
shim has been retired. The Phase-0 freeze lives in
`docs/plans/2026-04-13-recursive-adt-syntax-freeze.md`; the implementation
currently accepts the following core shapes:

```ebnf
Program      ::= Module+
Module       ::= "module" UIdent ["export" "(" ExportItem ("," ExportItem)* ")"] "{" Import* Decl* "}"
Import       ::= "import" UIdent ["as" UIdent] ["exposing" "(" ExportItem ("," ExportItem)* ")"] ";"
ExportItem   ::= lIdent | UIdent | UIdent "(..)"

Decl         ::= DataDecl | ClassDecl | InstanceDecl | DefDecl
DataDecl     ::= "data" UIdent TypeParam* "=" CtorDecl ("|" CtorDecl)* ["deriving" QName ("," QName)*] ";"
CtorDecl     ::= UIdent ":" SrcType
ClassDecl    ::= "class" UIdent TypeParam "{" MethodSig* "}"
MethodSig    ::= lIdent ":" ConstrainedType ";"
InstanceDecl ::= "instance" [ConstraintPrefix "=>"] QName SrcType "{" MethodDef* "}"
MethodDef    ::= lIdent "=" Expr ";"
DefDecl      ::= "def" lIdent ":" ConstrainedType "=" Expr ";"

TypeParam    ::= lIdent | "(" lIdent "::" Kind ")"
Kind         ::= KindAtom ["->" Kind]
KindAtom     ::= "*" | "(" Kind ")"

ConstrainedType ::= [ConstraintPrefix "=>"] SrcType
ConstraintPrefix ::= ClassConstraint | "(" ClassConstraint ("," ClassConstraint)* ")"
ClassConstraint ::= QName SrcType

SrcType      ::= lIdent
               | UIdent SrcTypeAtom*
               | lIdent SrcTypeAtom+
               | SrcType "->" SrcType
               | "∀" Binder+ "." SrcType
               | "μ" lIdent "." SrcType
               | "⊥"
SrcTypeAtom  ::= lIdent | UIdent | "⊥" | "(" SrcType ")"
Binder       ::= lIdent | "(" lIdent "⩾" SrcType ")"

Expr         ::= QName | Literal
               | "λ" Param Expr
               | Expr Expr
               | "let" lIdent [":" SrcType] "=" Expr "in" Expr
               | "case" Expr "of" "{" Alt (";" Alt)* "}"
               | "(" Expr ":" SrcType ")"

Param        ::= lIdent | "(" lIdent [":" SrcType] ")"
Alt          ::= Pattern "->" Expr
Pattern      ::= PatternAtom+
               | "(" Pattern ":" SrcType ")"
PatternAtom  ::= QName | "_" | "(" Pattern ")"
QName        ::= [UIdent "."] (lIdent | UIdent)
```

Current checked/evaluated recursive-ADT corpus examples live under
`test/programs/recursive-adt/`.

You can run a program file with:

```bash
cabal run mlf2 -- run-program test/programs/recursive-adt/plain-recursive-nat.mlfp
```
