# `.mlfp` Language Reference

`.mlfp` is the user-facing program layer on top of the eMLF/xMLF pipeline.
Raw eMLF stays thesis-facing and minimal; modules, ADTs, cases, typeclasses,
diagnostics, and runtime value rendering are owned by `.mlfp`.

## Modules

A file contains one or more modules:

```mlf
module Main export (main) {
  def main : Bool = true;
}
```

Exports are explicit when an `export (...)` list is present. Values export by
name, types export abstractly as `T`, and types export constructors as `T(..)`.
Imports expose names from another module:

```mlf
module User export (main) {
  import Core exposing (Nat(..), isZero);
  def main : Bool = isZero Zero;
}
```

Imports may refer to modules declared later in the same file. Hidden
constructors remain hidden: importing `Nat(..)` from a module that exported only
`Nat` is a visibility error.

Imports can be qualified and aliased:

```mlf
import Core as C;
import Core as C exposing (eq);
```

`import Core as C;` imports exported names as `C.name` only. `import Core as C
exposing (...)` also brings the selected exports into the unqualified scope.
Aliases must be unique within a module, and qualified access does not bypass
abstract exports or hidden constructors.

## Definitions

Definitions require a source type annotation:

```mlf
def id : forall a. a -> a = \x x;
def keep : Int = id 1;
```

Lambda parameters may also carry annotations:

```mlf
def usePoly : (forall a. a -> a) -> Bool =
  \(poly : forall a. a -> a) poly true;
```

## Data Declarations

Data declarations list explicit constructor signatures:

```mlf
data Nat =
    Zero : Nat
  | Succ : Nat -> Nat;

data Option a =
    None : Option a
  | Some : a -> Option a;
```

Runtime representation is Church-encoded. Constructor definitions are emitted
as real runtime bindings, but source constructor uses lower to deferred
obligations so eMLF inference can choose the necessary type evidence before the
constructor is rewritten to the concrete Church binding.

## Case Expressions And Patterns

Cases are ordered:

```mlf
def isTwo : Nat -> Bool = \(n : Nat) case n of {
  Succ (Succ Zero) -> true;
  Succ _ -> false;
  Zero -> false
};
```

Patterns support constructor patterns, variables, wildcards, and annotations:

```mlf
Succ n
Succ (Succ n)
_
(n : Nat)
```

Repeated top-level constructors are allowed when nested patterns make the
branches distinct. Flat duplicate branches and branches after a catch-all are
unreachable and rejected. Non-exhaustive cases fail closed.

Case lowering still targets the existing Church eliminator path. There is no
separate runtime pattern-matching primitive.

## GADT-Style Results And Existentials

Constructor results may refine the data result head:

```mlf
data Expr a =
    DoneNat : Nat -> Expr Nat
  | Step : Expr a -> Expr a;
```

Constructor-local `forall`s express existential packages:

```mlf
data SomeExpr =
    SomeExpr : forall a. Expr a -> SomeExpr;
```

Deferred constructor metadata carries the constructor-local `forall` evidence,
expected-type seeds, and runtime instantiation order. If required evidence
cannot be recovered from arguments, result type, or an explicit annotation, the
program fails with an ambiguous-constructor diagnostic.

## Typeclasses

The current typeclass lane supports single-parameter classes, class
constraints, schema instances, deriving, and overloaded method dispatch:

```mlf
class Eq a {
  eq : a -> a -> Bool;
}

data Nat =
    Zero : Nat
  | Succ : Nat -> Nat
  deriving Eq;

def main : Bool = eq (Succ Zero) (Succ Zero);
```

Constrained definitions and instances lower to hidden method evidence:

```mlf
def same : Eq a => a -> a -> Bool = \x \y eq x y;

instance Eq a => Eq (Option a) {
  eq = \left \right case left of {
    None -> case right of {
      None -> true;
      Some _ -> false
    };
    Some l -> case right of {
      None -> false;
      Some r -> eq l r
    }
  };
}
```

Method calls lower to deferred obligations and resolve after eMLF inference.
Partial overloaded method application is supported by eta-expanding the missing
arguments. Bare overloaded method names remain ambiguous and are rejected.

Instance resolution is coherent and fail-closed: duplicate and overlapping
instance heads are rejected by unification, and a missing instance remains a
`ProgramNoMatchingInstance` error.

`deriving Eq` works for nullary, recursive, and parameterized ADTs when every
field can be compared from a recursive owner field, a type-parameter
constraint, or an existing closed instance. For example, `Option a` derives
`Eq a => Eq (Option a)`, and recursive `List a` derives `Eq a => Eq (List a)`.
If deriving cannot resolve the first required field instance, it fails with a
field-specific deriving diagnostic.

## Built-In Prelude

There is a source-level `Prelude` module supplied by the program layer. It is
not imported implicitly by the checker APIs. The CLI/file runner prepends the
built-in module so user files can explicitly import it:

```mlf
module Main export (main) {
  import Prelude exposing (Nat(..), Option(..), List(..), Eq, eq, and, id);
  def main : Option Nat = Some Zero;
}
```

Current prelude contents:

- `Nat(..)` with derived `Eq Nat`
- `Option(..)` with derived `Eq a => Eq (Option a)`
- `List(..)` with `Nil`, `Cons`, and derived `Eq a => Eq (List a)`
- `Eq` and `eq`
- `and`
- `id`

The prelude is intentionally explicit-import in this version. A user-defined
module named `Prelude` conflicts with the built-in CLI prelude and is rejected.

## Runtime Values

Evaluated closed ADT values render in source constructor syntax:

```mlf
Zero
Succ Zero
None
Some true
Nil
Cons Zero Nil
```

Primitive closed values render as `true`, `false`, integers, and quoted strings.
If the runner cannot recover an ADT shape, it falls back to the existing xMLF
term pretty-printer instead of exposing a second runtime.

## When Annotations Are Required

Annotations are needed when source evidence is not recoverable from the term.
The common case is a polymorphic nullary constructor:

```mlf
def noneNat : Option Nat = None : Option Nat;
```

Without the result type evidence, `None` is ambiguous because it can inhabit
`Option a` for any `a`. Diagnostics include hints only when the compiler can
mechanically justify the suggested fix.
