# Recursive ADT Phase-0 Corpus and Syntax Freeze

Date: 2026-04-13
Authoritative PRD: `.omx/plans/prd-2026-04-13-recursive-algebraic-data-types-full-language-feature-set.md`
Authoritative test spec: `.omx/plans/test-spec-2026-04-13-recursive-algebraic-data-types-full-language-feature-set.md`

## Purpose

This file freezes the initial execution corpus and the concrete surface syntax
chosen under the delegated authority granted by deep-interview/ralplan. The
implementation must target this syntax and these examples unless a later
necessity-gated ADR proves a narrower choice impossible.

## Corpus

### Canonical plain recursive ADT example
- `test/programs/recursive-adt/plain-recursive-nat.mlfp`

### Separate advanced examples
- Recursive GADT: `test/programs/recursive-adt/recursive-gadt.mlfp`
- Recursive existential constructor: `test/programs/recursive-adt/recursive-existential.mlfp`
- Deriving support: `test/programs/recursive-adt/deriving-eq.mlfp`
- Typeclass integration: `test/programs/recursive-adt/typeclass-integration.mlfp`
- Module/export behavior: `test/programs/recursive-adt/module-integrated.mlfp`

### Integrated example
- `test/programs/recursive-adt/module-integrated.mlfp`

## Frozen concrete syntax

### Modules

```text
module <UpperName> export (<export>, ...) {
  import <UpperName> exposing (<export>, ...);
  <decl>*
}
```

### Exports

```text
<lowerName>        -- value export
<TypeName>         -- abstract type export
<TypeName>(..)     -- type + constructors export
```

### Declarations

```text
class <UpperName> <lowerTyVar> {
  <methodName> : <type>;
  ...
}

instance <UpperName> <type> {
  <methodName> = <expr>;
  ...
}

data <UpperName> <lowerTyVar>* =
    <Ctor> : <type>
  | <Ctor> : <type>
  deriving <UpperName>, ...;

def <lowerName> : <type> = <expr>;
```

Notes:
- Every `def` carries an explicit type.
- Class methods are declared as signatures inside the class body and defined
  inside instance bodies.
- Constructor declarations always use explicit result types. Plain ADTs and
  GADTs share the same constructor-signature syntax.
- `deriving` is initially frozen to explicit class names; the first required
  implementation target is `Eq`.

### Expressions

The program layer extends the existing lambda/let/application/literal/annotation
surface with `case`:

```text
case <expr> of {
  <pattern> -> <expr>;
  ...
}
```

Patterns are frozen to:

```text
<Ctor> <binder>*
<lowerName>
_
```

### Widening checkpoints

Any implementation that cannot satisfy the frozen corpus under the inherited
boundary must stop and write an ADR proving necessity before widening any of:
- equi-recursive reasoning
- cyclic structural graph encoding
- multi-SCC / broader recursive search
- fallback / second interface

## Review rule

Do not silently change this file while implementing. If the syntax or corpus
must move, land the reason and the replacement examples together.
