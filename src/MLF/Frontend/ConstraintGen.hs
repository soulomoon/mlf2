module MLF.Frontend.ConstraintGen (
    ConstraintError (..),
    ConstraintResult (..),
    AnnExpr (..),
    generateConstraints
) where

import qualified MLF.Constraint.Root as ConstraintRoot
import MLF.Frontend.Syntax (Expr)
import MLF.Frontend.Desugar (desugarCoercions)
import MLF.Constraint.Types (NodeId(..))
import MLF.Frontend.ConstraintGen.Types
import MLF.Frontend.ConstraintGen.State
import MLF.Frontend.ConstraintGen.Translate (buildRootExpr)

{- Note [Phase 1: Constraint Generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module implements Phase 1 of the MLF type inference algorithm: translating
a source expression into a graphic constraint. This is the "compositional
translation" described in Rémy & Yakobowski (ICFP 2008) §1.

The translation is syntax-directed and produces:
  1. A DAG of type nodes (TyVar, TyArrow, TyBase, TyExp)
  2. A binding tree via binding edges (scope)
  3. Instantiation edges (≤) at application sites
  4. The root NodeId representing the expression's type

Key invariants maintained:
  - Binding edges encode scope (paper-style binding tree)
  - Lambda parameters are bound at the CURRENT level (monomorphic)
  - Let bindings create a CHILD level and wrap RHS in TyExp

The constraint graph is the input to subsequent phases:
  - Phase 2 normalizes via grafting/merging
  - Phase 3 checks acyclicity of instantiation dependencies
  - Phase 4 computes the principal presolution
  - Phase 5 solves remaining unification
  - Phase 6 elaborates to xMLF

Paper reference: ICFP 2008, §1 "From ML to constraints"
-}

{- Note [Lambda vs Let Polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MLF distinguishes between lambda-bound and let-bound variables in how they
handle polymorphism. This follows standard ML-style let-polymorphism.

Lambda-bound variables (monomorphic by default):
  In `λf. (f 1, f True)`, the parameter `f` gets a plain type variable `α`.
  Each use of `f` must have the SAME type, so this fails: we can't unify
  `α → Int` with `α → Bool`.

Let-bound variables (polymorphic via expansion nodes):
  In `let f = λx. x in (f 1, f True)`, the binding `f` is wrapped in an
  expansion node `s · (α → α)`. Each USE of `f` can instantiate differently,
  so `f 1 : Int` and `f True : Bool` both work.

The classic example that illustrates this difference:

    (λf. (f 1, f True)) (λx. x)     -- FAILS in ML and MLF (without annotation)
    let f = λx. x in (f 1, f True)  -- WORKS in ML and MLF

Why can't MLF infer polymorphism for lambda parameters?

To type the lambda version, you need HIGHER-RANK polymorphism:
  (λf. ...) : (∀α. α → α) → (Int, Bool)

MLF CAN express this type, but cannot INFER it without help because:
  1. The argument type (∀α. α → α) is not determined by the lambda body alone
  2. Multiple valid types exist (the type is not principal without annotation)
  3. Inference would require "guessing" where to place ∀ quantifiers

This is why our implementation:
  - ELam: allocates a plain TyVar for the parameter (monomorphic)
  - ELet: wraps the RHS in a TyExp expansion node (polymorphic)

Future extensions could support explicit type annotations like:
  ELamAnnot :: VarName -> Type -> Expr -> Expr
  -- λ(f : ∀α. α → α). (f 1, f True)

This would allow the user to request higher-rank types where needed.

Paper references:
  - ICFP 2008, §1 describes the constraint language and type syntax
  - ICFP 2008, §3 defines solved forms and expansion variables (s · τ)
  - Le Botlan & Rémy (2003) "MLF: Raising ML to the Power of System F"
    discusses the design choice of annotation-free let-polymorphism
-}

generateConstraints :: Expr -> Either ConstraintError ConstraintResult
generateConstraints expr = do
    let expr' = desugarCoercions expr
    let initialState = mkInitialState
    let topScopeRoot = NodeId (-1)
    ((_rootBinder, rootNode, annRoot), finalState) <-
        runConstraintM (buildRootExpr topScopeRoot expr') initialState
    let constraint = ConstraintRoot.ensureConstraintRoot (buildConstraint finalState)
    pure ConstraintResult
        { crConstraint = constraint
        , crRoot = rootNode
        , crAnnotated = annRoot
        }
