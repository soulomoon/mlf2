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
              | "Λ" α+ "." SrcType
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

- eMLF supports literals (`Int`, `Bool`, `Char`, `String`) as expression atoms.
  `Char` literals use single quotes around one Unicode scalar value, for example
  `'λ'`.
- eMLF parser accepts `let x : τ = e in b` and desugars to `let x = (e : τ) in b`.
- eMLF source types accept Unicode type lambdas (`Λa. τ`) and explicit general
  type application in annotations. The source normalizer beta-reduces supported
  applications and rejects residual type lambdas or non-normalized general
  applications before the MLF core boundary.
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

Decl         ::= DataDecl | TypeFamilyDecl | ClassDecl | InstanceDecl | DefDecl
DataDecl     ::= "data" UIdent TypeParam* "=" CtorDecl ("|" CtorDecl)* ["deriving" QName ("," QName)*] ";"
CtorDecl     ::= UIdent ":" SrcType
TypeFamilyDecl ::= "type" "family" UIdent TypeFamilyParam* "::" TypeLevelKind "where" "{" TypeFamilyEquation+ "}"
TypeFamilyParam ::= lIdent | "(" lIdent "::" TypeLevelKind ")"
TypeFamilyEquation ::= UIdent TypeLevelPatternAtom* "=" TypeLevelType ";"
ClassDecl    ::= "class" [ConstraintPrefix "=>"] UIdent TypeParam+ [FundepList] "{" MethodSig* "}"
MethodSig    ::= lIdent ":" ConstrainedType ";"
InstanceDecl ::= "instance" [ConstraintPrefix "=>"] QName ClassArg+ "{" MethodDef* "}"
MethodDef    ::= lIdent "=" Expr ";"
DefDecl      ::= "def" lIdent ":" ConstrainedType "=" Expr ";"

TypeParam    ::= lIdent | "(" lIdent "::" Kind ")"
Kind         ::= KindAtom ["->" Kind]
KindAtom     ::= "*" | "(" Kind ")"
FundepList   ::= "|" Fundep ("," Fundep)*
Fundep       ::= lIdent+ "→" lIdent+
TypeLevelKind ::= "*" | KindVar | "(" TypeLevelKind ")" | TypeLevelKind "->" TypeLevelKind
KindVar      ::= lower-case identifier, including Unicode lower-case letters

ConstrainedType ::= [ConstraintPrefix "=>"] SrcType
ConstraintPrefix ::= ClassConstraint | "(" ClassConstraint ("," ClassConstraint)* ")"
ClassConstraint ::= QName ClassArg+
ClassArg     ::= lIdent | UIdent | "⊥" | "(" SrcType ")"

SrcType      ::= lIdent
               | UIdent SrcTypeAtom*
               | lIdent SrcTypeAtom+
               | SrcType "->" SrcType
               | "Λ" lIdent+ "." SrcType
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
Literal      ::= integer | "true" | "false" | string | char

Param        ::= lIdent | "(" lIdent [":" SrcType] ")"
Alt          ::= Pattern "->" Expr
Pattern      ::= PatternAtom+
               | "(" Pattern ":" SrcType ")"
PatternAtom  ::= QName | "_" | "(" Pattern ")"
QName        ::= [UIdent "."] (lIdent | UIdent)

TypeLevelType ::= lIdent | UIdent | TypeLevelType TypeLevelType | TypeLevelType "->" TypeLevelType | "Λ" TypeFamilyParam+ "." TypeLevelType | "(" TypeLevelType ")"
TypeLevelPattern ::= lIdent | UIdent TypeLevelPatternAtom*
TypeLevelPatternAtom ::= lIdent | UIdent | "(" TypeLevelPattern ")"
```

`→` is the only fundep arrow. ASCII `->` remains the type/kind arrow and is not
accepted as a functional-dependency alias.

The checked class subset includes single-parameter method classes, zero-method
multi-parameter classes, and method-bearing multi-parameter dispatch when every
class argument is fixed by the call or functional-dependency closure over
visible instances/evidence. Superclass clauses lower into flattened method
evidence and specialized instance prerequisites. Invalid fundeps, ambiguous
fundep instances, fundep-conflicting instances, and still-ambiguous
multi-parameter method uses fail closed before evidence elaboration.

Closed `type family` declarations roundtrip through the `.mlfp` parser/pretty
syntax and map into the internal pre-core type-level AST. During program
checking, reducible family applications normalize by ordered first-match
equations and family declarations are erased before the existing resolver/core
boundary. Stuck, cyclic, and fuel-exhausted family reductions fail before core
erasure.

Current checked/evaluated fixture examples live under
`test/programs/recursive-adt/`, `test/programs/unified/`, and the static local
package fixtures under `test/programs/packages/`.

You can run a local package root with:

```bash
cabal run mlf2 -- run-program test/programs/packages/cross-module-let
```

Ordered search-path roots are part of local package mode:

```bash
cabal run mlf2 -- run-program test/programs/packages/search-path-main --search-path test/programs/packages/search-path-lib
```

You can also pass one `.mlfp` file; it is loaded as a trivial package source
unit:

```bash
cabal run mlf2 -- run-program test/programs/recursive-adt/plain-recursive-nat.mlfp
```
