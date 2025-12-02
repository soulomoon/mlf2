module MLF.Syntax (
    VarName,
    Lit (..),
    Expr (..),
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

-- | Core ML-like language supported by the constraint generator.
data Expr
    = EVar VarName
    | ELam VarName Expr
    | EApp Expr Expr
    | ELet VarName Expr Expr
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
