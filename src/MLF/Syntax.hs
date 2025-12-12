module MLF.Syntax (
    VarName,
    Lit (..),
    Expr (..),
    SrcType (..),
    SrcScheme (..),
    AnnotatedExpr (..),
    BindingSite (..)
) where

-- | Simple variable name representation; for now a plain String is enough.
type VarName = String

-- | Literal subset we care about while generating constraints.
data Lit
    = LInt Integer
    | LBool Bool
    | LString String
    deriving (Eq, Show)

-- | Source-level type syntax (eMLF annotations).
--
-- This is the surface syntax for type annotations that users can write.
-- It supports:
--   - Type variables: α, β, ...
--   - Arrow types: τ → σ
--   - Base types: Int, Bool, ...
--   - Instance-bounded quantification: ∀(α ⩾ τ). σ
--   - Bottom type: ⊥ (the minimal/most polymorphic type)
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

-- | Source-level type scheme (multiple quantifiers).
--
-- A scheme is a sequence of quantified variables with optional bounds,
-- followed by a body type.
--
-- Example: @∀(α ⩾ Int)(β). α → β@ represents a scheme where
--   - α has bound Int
--   - β is unbounded
--   - The body is α → β
data SrcScheme = SrcScheme [(String, Maybe SrcType)] SrcType
    deriving (Eq, Show)

-- | Core ML-like language supported by the constraint generator (eMLF).
--
-- This extends the basic ML expression syntax with optional type annotations:
--   - ELamAnn: Lambda with annotated parameter type
--   - ELetAnn: Let with annotated type scheme
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

-- | Annotated expression produced by constraint generation.
data AnnotatedExpr = AnnotatedExpr
    { annExpr :: Expr
    , annBinding :: Maybe BindingSite
    }
    deriving (Eq, Show)

-- | Distinguish between lambda parameters and let-bound values.
data BindingSite
    = LamParam VarName
    | LetBinding VarName
    deriving (Eq, Show)
