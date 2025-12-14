module MLF.Syntax (
    VarName,
    Lit (..),
    Expr (..),
    SrcType (..),
    SrcScheme (..),
    AnnotatedExpr (..),
    BindingSite (..)
) where

{- Note [Surface syntax and paper alignment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module defines the *surface language* accepted by the pipeline:

  - `Expr` is the core eMLF term language (partially annotated λ-calculus).
  - `SrcType` / `SrcScheme` are the user-written type annotations.

Paper reference
--------------
In `papers/xmlf.txt` (§"From λ-terms to typing constraints"), the grammar for
eMLF terms is (using the paper’s notation):

  b ::= x | λ(x) b | λ(x : σ) b | b b | let x = b in b | (b : σ)

Our `Expr` constructors correspond one-to-one to that grammar:

  - `EVar`       ↔ x
  - `ELam`       ↔ λ(x) b
  - `ELamAnn`    ↔ λ(x : σ) b
  - `EApp`       ↔ b b
  - `ELet`       ↔ let x = b in b
  - `ELetAnn`    ↔ let x : σ = b in b
  - `EAnn`       ↔ (b : σ)

The paper notes that term/type annotations can be desugared using coercion
functions κσ. This repository keeps annotations explicit in the AST and
implements their meaning directly during constraint generation.

Implementation boundary
-----------------------
These types intentionally remain simple (mostly `String`s) and do not encode
scope/levels. Binding levels and polymorphism decisions live in the constraint
representation (`MLF.Types`) produced by Phase 1.
-}

-- | Source-level term variable names.
--
-- We use plain `String`s. Uniqueness / alpha-renaming is handled by the parser
-- or assumed by construction in tests.
type VarName = String

-- | Literal subset used by the pipeline.
--
-- These map to base types in constraint generation (e.g. `Int`, `Bool`, `String`).
data Lit
    = LInt Integer
    | LBool Bool
    | LString String
    deriving (Eq, Show)

-- | Source-level type syntax for annotations.
--
-- This corresponds closely to the type language presented in `papers/xmlf.txt`:
--
--   - type variables (written !α in the paper; we use names like "a")
--   - arrows (τ → σ)
--   - bounded quantification ∀(α ⩾ τ). σ
--   - bottom type ⊥ (the default bound for unbounded quantification)
--
-- We add a small convenience notationally:
--
--   - In `STForall v Nothing body`, the missing bound means “bound = ⊥”, i.e.
--     the common unbounded form ∀(v). body.
--
-- Examples:
--   - @Int → Bool@
--   - @∀α. α → α@ (standard polymorphism)
--   - @∀(α ⩾ Int → Int). α@ (α must be at least as general as Int → Int)
--   - @∀(α ⩾ ∀β. β → β). α → α@ (α must be at least as general as polymorphic identity)
data SrcType
    = STVar String                              -- ^ Type variable: α
    | STArrow SrcType SrcType                   -- ^ Arrow type: τ → σ
    | STBase String                             -- ^ Base type: Int, Bool, ...
    | STForall String (Maybe SrcType) SrcType   -- ^ Bounded quantification: ∀(α ⩾ τ?). σ
    | STBottom                                  -- ^ Bottom type: ⊥
    deriving (Eq, Show)

-- | Source-level type scheme (multiple binders) used by `ELetAnn`.
--
-- `SrcScheme` is a syntactic convenience for user-facing let annotations:
--
--   let x : ∀(a ⩾ τa). ∀(b). body = rhs in ...
--
-- is represented as:
--
--   SrcScheme [("a", Just τa), ("b", Nothing)] body
--
-- It is equivalent to nesting `STForall` binders (right-associatively), but a
-- list is more convenient for parsing/pretty-printing and for internalization.
--
-- Example: @∀(α ⩾ Int)(β). α → β@ represents a scheme where
--   - α has bound Int
--   - β is unbounded
--   - The body is α → β
data SrcScheme = SrcScheme [(String, Maybe SrcType)] SrcType
    deriving (Eq, Show)

-- | Core term language (eMLF) supported by constraint generation.
--
-- This is intentionally small and matches the paper’s eMLF term grammar.
--
-- Downstream meaning (high level)
-- ------------------------------
-- `Expr` does not itself encode polymorphism rules. Instead:
--
--   - lambda binders (`ELam`, `ELamAnn`) behave monomorphically,
--   - let binders (`ELet`, `ELetAnn`) are generalization points,
--   - applications (`EApp`) generate instantiation constraints (≤),
--   - annotations (`ELamAnn`, `ELetAnn`, `EAnn`) are turned into constraint graph
--     structure by Phase 1.
--
-- See `MLF.ConstraintGen` for the authoritative translation.
--
-- Unannotated forms infer types; annotated forms check against provided types.
data Expr
    = EVar VarName
    | ELam VarName Expr                         -- ^ λx. e (inferred parameter type)
    | ELamAnn VarName SrcType Expr              -- ^ λ(x : τ). e (annotated parameter)
    | EApp Expr Expr
    | ELet VarName Expr Expr                    -- ^ let x = e₁ in e₂ (inferred scheme)
    | ELetAnn VarName SrcScheme Expr Expr       -- ^ let x : σ = e₁ in e₂ (annotated scheme)
    | EAnn Expr SrcType                         -- ^ (e : τ) (term annotation)
    | ELit Lit
    deriving (Eq, Show)

-- | Optional wrapper for attaching binding-site metadata to a surface expression.
--
-- This is primarily useful for tooling/debugging (e.g. reporting “this variable
-- occurrence is a lambda parameter vs a let-binding”). The main constraint
-- generator uses its own annotation structure (`MLF.ConstraintGen.AnnExpr`).
data AnnotatedExpr = AnnotatedExpr
    { annExpr :: Expr
    , annBinding :: Maybe BindingSite
    }
    deriving (Eq, Show)

-- | Distinguish between lambda parameters and let-bound values.
--
-- This mirrors the paper’s key distinction:
--   - lambda-bound variables are not generalized (monomorphic),
--   - let-bound variables may be generalized and later instantiated.
data BindingSite
    = LamParam VarName
    | LetBinding VarName
    deriving (Eq, Show)
