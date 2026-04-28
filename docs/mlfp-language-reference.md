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

Imports may refer to modules declared later in the same file. A parsed `.mlfp`
`Program` is one compilation unit: imports resolve only against modules in that
same program. The CLI runner parses one source file and prepends the built-in
`Prelude` as an explicit module before checking. There is no filesystem module
discovery, persisted interface file, stable `.mlfp` ABI, linker, or separate
module compilation mode yet. Hidden constructors remain hidden: importing
`Nat(..)` from a module that exported only `Nat` is a visibility error.

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

## Type Parameter Kinds

Data and class type parameters default to kind `*`:

```mlf
class Eq a {
  eq : a -> a -> Bool;
}

data Option a =
    None : Option a
  | Some : a -> Option a;
```

Higher-kinded parameters can be declared with a parenthesized kind annotation.
Kind arrows associate to the right, so `* -> * -> *` means `* -> (* -> *)`.
The supported kind language is `*` plus arrows between kinds.

```mlf
class Functor (f :: * -> *) {
  map : forall a b. (a -> b) -> f a -> f b;
}

class Monad (m :: * -> *) {
  bind : forall a b. m a -> (a -> m b) -> m b;
}

class Profunctor (p :: * -> * -> *) {
  dimap : forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d;
}

data Wrap (f :: * -> *) a =
    Wrap : f a -> Wrap f a;

data WrappedP (p :: * -> * -> *) a b =
    WrappedP : p a b -> WrappedP p a b;
```

These are still single-parameter classes: `Profunctor` takes one class
parameter `p`, but that parameter itself has kind `* -> * -> *`. The checker
does not enforce class laws.

Variable-headed type applications such as `f a` and `p a b` are accepted in
source types and pretty-print back to the same structure:

```mlf
data Box a =
    Box : a -> Box a;

data MaybeF (f :: * -> *) a =
    NothingF : MaybeF f a
  | JustF : f a -> MaybeF f a;

class Boxed (f :: * -> *) {
  truthy : f Bool -> Bool;
}

instance Boxed Box {
  truthy = \box true;
}

class Uses marker {
  use : (Boxed f, Functor f) => marker -> marker;
}
```

The checker validates declaration parameter kinds, ordinary and
variable-headed type applications, class method constraints, instance heads,
and constructor signatures before lowering. Kind errors are reported as
`ProgramKindMismatch` or `ProgramTypeArityMismatch`.

Well-kinded higher-kinded classes and data declarations elaborate through the
`.mlfp` program layer, including representative runtime cases where a
higher-kinded parameter is instantiated with a concrete constructor head such
as `Box` in `MaybeF Box Bool`. Direct raw eMLF pipeline inputs that bypass the
program checker and still contain an unresolved variable-headed type
application remain fail-closed.

Unsupported forms include using a first-order parameter as a type function,
unsaturated or over-applied type constructors, mismatched higher-kinded
arguments such as passing `Bool` where `* -> *` is required, kind polymorphism,
type lambdas, and type-level computation.

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

## Backend Boundary

Successful `.mlfp` checking may be followed by conversion into the internal
typed backend IR owned by `MLF.Backend.IR`. The path is checked `.mlfp`
program -> existing eMLF/xMLF typecheck guard -> `MLF.Backend.Convert` ->
typed backend IR -> `MLF.Backend.LLVM` for repo-local LLVM lowering and
deterministic `.ll` emission.

The backend path does not change source typing, inference, checker diagnostics,
module/import visibility, runtime value rendering, or the explicit-import
Prelude contract documented here. Its validator is scoped to backend-local
invariants such as typed expression nodes, lexical/global references, and
constructor metadata consistency for construct/case nodes.

The executable exposes this boundary with:

```bash
cabal run mlf2 -- emit-backend path/to/file.mlfp
```

Like `run-program`, `emit-backend` parses one source file and prepends the
built-in Prelude as an explicit module before checking. The emitted text is
LLVM IR for the supported first-order backend subset, not a stable ABI or a
promise of final executable linking. The supported LLVM subset covers checked
first-order function bindings, saturated direct calls, literals, variables,
SSA-style lets, type abstraction/application after specialization, ADT
construction, and ADT case analysis.

The typed backend IR can also represent data metadata, constructor nodes, case
nodes, and recursive roll/unroll nodes. The LLVM lowering intentionally rejects
backend nodes that are not yet supported instead of silently dropping or
rewriting them. Diagnostics are high-level and name the backend context and
node, for example:

```text
Unsupported backend LLVM expression at binding "main": escaping lambda
```

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
