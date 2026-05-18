# `.mlfp` Language Reference

`.mlfp` is the user-facing program layer on top of the eMLF/xMLF pipeline.
Raw eMLF stays thesis-facing and minimal; modules, ADTs, cases, typeclasses,
diagnostics, and runtime value rendering are owned by `.mlfp`.
Self-boot readiness is tracked separately in
`docs/mlfp-self-boot-readiness.md`; this reference does not claim the compiler
is implemented in `.mlfp`. The accepted end-to-end order is recorded in
`docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`; implementation
starts with the shared file-based conformance corpus before broad native text,
parser parity, platform contracts, compiler package, driver, or first-proof
work.

## Modules

A `.mlfp` package source unit contains one or more modules:

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

Imports may refer to modules declared later in the same source unit or in
another `.mlfp` file discovered under an explicitly selected local package root.
The durable `.mlfp` execution model is local package mode: imports resolve only
against modules in the selected root plus ordered search-path roots, and the
CLI prepends the built-in `Prelude` as an explicit module before checking.

```bash
cabal run mlf2 -- check-program test/programs/packages/cross-module-let
cabal run mlf2 -- run-program test/programs/packages/search-path-main --search-path test/programs/packages/search-path-lib
```

Passing a single `.mlfp` file is supported as a trivial package source unit:

```bash
cabal run mlf2 -- run-program test/programs/recursive-adt/plain-recursive-nat.mlfp
```

A parsed `.mlfp` `Program` remains an in-memory package projection. There is no
package manager, remote dependency system, persisted interface file, stable
`.mlfp` ABI, linker, or separate module compilation mode yet. Hidden
constructors remain hidden: importing `Nat(..)` from a module that exported
only `Nat` is a visibility error.

Compiler-source seed fixtures use the same local package mode. The current
frontend seed lives at `test/programs/compiler-seed/frontend-contract/` and is
loaded as an ordinary package root. Its `.mlfp` modules define bounded symbolic
input, seed-owned monomorphic input/token stream ADTs, symbolic source spans,
lexer and parser diagnostics/results, and a tiny parser over the seed token
stream. The seed evidence is checked by `ProgramCompilerSeedSpec` and printed
through `run-program`; it does not provide a source-text character stream, byte
stream, substring API, parser-combinator library, separate loader, package
manager, ABI, linker, separate compilation, or self-hosting contract.

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

The method evidence lane accepts single-parameter method classes and the
method-bearing multi-parameter subset whose method use fixes every class
argument from supplied arguments, result type, local evidence, or
functional-dependency closure over visible instances/evidence. The checker also
accepts zero-method multi-parameter classes, constraints, and instances as
schema evidence. Superclass constraints are flattened into the same method
evidence lane: a local subclass constraint provides its superclass method
evidence, and subclass instances carry specialized superclass prerequisites.
It does not enforce class laws.

The parser and pretty-printer also understand the generalized class surface:
superclass constraints, multiple class parameters, and Unicode functional
dependencies:

```mlf
class Functor f => Monad (m :: * -> *) (f :: * -> *) | m → f {
  bind : forall a b. m a -> (a -> m b) -> m b;
}
```

This syntax is accepted into the frontend AST and checked before core erasure.
The current checker admits zero-method multi-parameter evidence,
vector-specialized method-bearing multi-parameter instance dispatch, superclass
evidence flattened through subclass constraints and instance prerequisites, and
functional-dependency closure for method calls whose remaining class arguments
are fixed by visible instances or local evidence. Invalid fundeps, ambiguous
fundep instances, fundep-conflicting instances, and still-ambiguous
multi-parameter method uses fail closed with structured diagnostics. The
implementation must not silently erase these forms into the older
single-parameter method evidence path. `→` is the only fundep arrow; ASCII
`->` remains the type/kind arrow and is not accepted as a fundep alias.

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

Raw frontend source types can use Unicode type lambdas in annotations when
they normalize away before the core boundary, for example `(Λa. a -> a) Int`.
The normalizer beta-reduces these applications, and any residual type lambda or
non-normalized general type application is rejected before erasure into MLF
core types.

Closed type-family declarations have parse/pretty syntax on `.mlfp`:

```mlf
type family Normalize (a :: κ) :: κ where {
  Normalize Int = Int;
  Normalize (Box a) = a;
  Normalize a = (Λx. x) a;
}
```

This declaration syntax is represented in the frontend AST, and the internal
pre-core type-level normalizer owns ordered closed-family reduction, stuck
family diagnostics, cycle detection, and fuel exhaustion. During program
checking, reducible family applications normalize and family declarations erase
before the existing resolver/core boundary sees the program. Stuck, cyclic, and
fuel-exhausted reductions fail before core erasure. Open families and
associated types are not part of the contract.

Unsupported forms include using a first-order parameter as a type function,
unsaturated or over-applied type constructors, mismatched higher-kinded
arguments such as passing `Bool` where `* -> *` is required, ordinary
data/class kind polymorphism, open type-family declarations, associated types,
unreduced type lambdas, and checker-visible stuck type-level computation.

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

The current checked typeclass lane supports single-parameter method classes,
class constraints, schema instances, deriving, overloaded method dispatch,
zero-method multi-parameter class evidence, and method-bearing
multi-parameter dispatch when every class argument is determined at the call
site or by functional-dependency closure over visible instances/evidence.
Superclass constraints are checked as flattened evidence and specialized
instance prerequisites. Invalid fundeps, ambiguous fundep instances,
fundep-conflicting instances, and ambiguous multi-parameter method uses fail
closed before evidence elaboration:

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
arguments. Overloaded methods can also resolve from an explicit or propagated
expected source type when the supplied term arguments alone do not determine the
class parameter but the method result does; for example, `(mempty : Nat)` can
select a `Monoid Nat` instance, and `pure Unit` can select `Monad IO` under an
expected result type of `IO Unit`. Bare overloaded method names or applications
without enough term arguments or expected-type evidence remain ambiguous and are
rejected.

Instance resolution is coherent and fail-closed: duplicate and overlapping
instance heads are rejected by unification, and a missing instance remains a
`ProgramNoMatchingInstance` error.

`deriving Eq` works for nullary, recursive, and parameterized ADTs when every
field can be compared from a recursive owner field, a type-parameter
constraint, or an existing closed instance. For example, `Option a` derives
`Eq a => Eq (Option a)`, and recursive `List a` derives `Eq a => Eq (List a)`.
If deriving cannot resolve the first required field instance, it fails with a
field-specific deriving diagnostic.

## IO And Monad Contract

This section freezes the initial IO contract for the #87 work. Source typing,
runner execution, and backend behavior remain separate ownership boundaries;
this contract defines the surface they implement without changing the pure
eMLF/xMLF core.

The initial standard-library surface is deliberately small:

```mlf
data Unit =
    Unit : Unit;
```

`Unit` is the unit result type for completed effects. It has one inhabitant,
`Unit`, and is the only supported result type for effectful program entrypoints
in the initial IO surface. The standard-library hierarchy is now explicit:

```mlf
class Functor (f :: * -> *) {
  map : forall a b. (a -> b) -> f a -> f b;
}

class Functor f => Applicative (f :: * -> *) {
  pure : forall a. a -> f a;
  ap : forall a b. f (a -> b) -> f a -> f b;
}

class Applicative m => Monad (m :: * -> *) {
  bind : forall a b. m a -> (a -> m b) -> m b;
}
```

`IO` is an opaque source type constructor with kind `* -> *`. It is owned by the
program/runtime boundary rather than declared as an ordinary source ADT. User
programs may mention `IO a` in types and use the standard-library operations
below, but they cannot construct, deconstruct, derive, or pattern match on an
`IO` representation. The standard library provides coherent `Functor IO`,
`Applicative IO`, and `Monad IO` instances backed by implementation-reserved
primitives such as `__io_pure` and `__io_bind`; those primitive names are not
the user-facing API.

The current source-level IO prelude exposes text, file, process, reference, and
argument operations through the opaque `IO` boundary. The initial text operation
is:

```mlf
putStrLn : String -> IO Unit
```

`putStrLn` and `putStr` use the existing source `String` base type and string
literals. Native-backed IO also includes `getLine`, `readFile`, `writeFile`,
`appendFile`, `exitWith`, `newIORef`, `readIORef`, `writeIORef`, and `getArgs`.
There is no narrower byte/character IO primitive in the first contract.

The current pure broad string operations are:

```mlf
stringLength : String -> Int
stringIsEmpty : String -> Bool
stringContainsChar : String -> Char -> Bool
```

`stringLength` counts Unicode scalar values in the source `String`, not UTF-8
bytes. `stringIsEmpty` is the first broad `String` classification operation; it
classifies the valid source text boundary where `""` is empty and a non-empty
Unicode scalar string such as `"λ"` is not. `stringContainsChar` is the first
single-character `String`/`Char` search operation; it compares Unicode scalar
values, so `stringContainsChar "aλb" 'λ'` is `true` while
`stringContainsChar "ab" 'λ'` is `false`. These operations are covered through
source checking, `run-program`, backend LLVM emission, object generation, and
linked native execution for the current native-capable tracers. `String`/`List
Char` conversion, substring search, formatting, slicing, broader
classification predicates, cursor APIs, locale, regex, and full parser parity
remain out of scope.

Pure program entrypoints remain accepted:

```mlf
module Main export (main) {
  def main : Bool = true;
}
```

The checked effectful entrypoint mode is `main : IO Unit`:

```mlf
module Main export (main) {
  import Prelude exposing (Unit(..), IO, Applicative, Monad, bind, pure, putStrLn);

  def main : IO Unit =
    bind (putStrLn "hello") (\(_done : Unit) putStrLn "world");
}
```

An `IO` action is still an opaque value while the program is checked and
normalized. `run-program` executes a checked `main : IO Unit` by interpreting
the reserved IO primitive boundary. Effects occur only when the final `main`
action is run. Sequencing is determined by `bind`, `putStrLn` appends its text
argument plus a newline to stdout, and successful `IO Unit` programs do not
render a `Unit` result value. The `run-program` output path also accepts
`main : IO a` when the action result can be converted back to a runtime value;
operations that require native process support, such as `getLine`, file IO,
`IORef`, `exitWith`, and `getArgs`, fail closed with a diagnostic that points to
native execution.

The backend/native contract is layer-specific rather than a blanket IO
rejection. `emit-backend` lowers checked programs that reference
inventory-classified native IO primitives by emitting private
closure-allocating wrappers and the runtime declarations they need; it does not
add a process entrypoint. `emit-native` starts from the same backend IR, adds
the C ABI `main` wrapper, and executes `main : IO ...` actions without rendering
their result values. The covered native wrapper paths include `__io_pure`,
`__io_bind`, `__io_map`, `__io_putStrLn`, `__io_putStr`, `__io_getLine`,
`__io_readFile`, `__io_writeFile`, `__io_appendFile`, `__io_exitWith`,
`__io_newIORef`, `__io_readIORef`, `__io_writeIORef`, and `__io_getArgs`.
`Functor IO.map` is supported through that wrapper path when the mapped action
and result stay inside the lowerable/native-renderable subset. `Applicative
IO.ap` remains a source and `run-program` operation, but its reserved
`__io_ap` primitive is not native-lowerable yet; backend/native emission must
fail closed if an `ap` use survives to emission, especially for function-valued
`IO` results. These boundaries do not change source typing, pure runtime
rendering, or the existing first-order LLVM subset.

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

The executable exposes this boundary with local package roots and ordered
search paths:

```bash
cabal run mlf2 -- emit-backend test/programs/packages/cross-module-let
cabal run mlf2 -- emit-backend test/programs/packages/search-path-main --search-path test/programs/packages/search-path-lib
```

Like `check-program`, `run-program`, and `emit-native`, `emit-backend` loads a
single file as a trivial package source unit or discovers `.mlfp` files under
the selected local roots, then prepends the built-in Prelude as an explicit
module before checking. The emitted text is LLVM IR for the supported backend
subset, not a stable ABI, compiler-seed lexer/parser native guarantee, or a
promise of final executable linking. The
source-facing surface remains primarily first-order: checked first-order
function bindings, saturated direct calls, literals, variables, SSA-style lets,
type abstraction/application after specialization, ADT construction, and ADT
case analysis.

The typed backend IR can also represent data metadata, constructor nodes, case
nodes, recursive roll/unroll nodes, and explicit closure construction plus
indirect closure calls. Closure values lower through a private backend ABI that
stores a code pointer and environment pointer in a heap closure record; this is
an internal IR-to-LLVM foundation and does not yet imply full `.mlfp` partial
application or escaping-lambda closure conversion. The LLVM lowering
intentionally rejects backend nodes that are not yet supported instead of
silently dropping or rewriting them. Diagnostics are high-level and name the
backend context and node, for example:

```text
Unsupported backend LLVM expression at binding "main": escaping lambda
```

## Built-In Prelude

There is a source-level `Prelude` module supplied by the program layer. It is
not imported implicitly by the checker APIs. The CLI package entrypoints prepend
the built-in module so user files can explicitly import it:

```mlf
module Main export (main) {
  import Prelude exposing (Nat(..), Option(..), List(..), Eq, eq, and, id);
  def main : Option Nat = Some Zero;
}
```

Current prelude contents:

- `Unit(..)` with the `Unit` value constructor
- opaque `IO` and `IORef`
- `Nat(..)` with derived `Eq Nat`
- `Option(..)` with derived `Eq a => Eq (Option a)`
- `List(..)` with `Nil`, `Cons`, and derived `Eq a => Eq (List a)`
- `Eq` and `eq`
- `Functor`, `Applicative`, and `Monad`, with built-in coherent `IO` instances
- `map`, `pure`, `ap`, and `bind`
- `putStrLn`, `putStr`, `getLine`, `readFile`, `writeFile`, `appendFile`,
  `exitWith`, `newIORef`, `readIORef`, `writeIORef`, and `getArgs`
- `stringLength`
- `stringIsEmpty`
- `stringContainsChar`
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

Primitive closed values render as `true`, `false`, integers, single-quoted
characters, and quoted strings. Unicode `Char` literals are scalar values; the
native tracer covers the non-ASCII scalar example `'λ'` rendering as `'\955'`.
The first pure broad `String` operation is `stringLength`, which counts Unicode
scalar values rather than UTF-8 bytes. The first broad `String` classification
operation is `stringIsEmpty`, which renders `true` for `""` and `false` for a
non-empty Unicode string such as `"λ"`. The first single-character
`String`/`Char` search operation is `stringContainsChar`, which compares
Unicode scalar values. Broad `String`/`List Char` conversion, substring search,
formatting, slicing, broader classification predicates, cursor APIs, and
parser-parity helpers remain outside this contract.
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
