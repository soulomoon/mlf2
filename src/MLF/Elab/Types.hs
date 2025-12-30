module MLF.Elab.Types (
    ElabType(..),
    ElabScheme(..),
    SchemeInfo(..),
    ElabTerm(..),
    Instantiation(..),
    ElabError(..),
    TypeCheckError(..),
    bindingToElab,
    Pretty(..),
    ContextStep(..),
    applyContext,
    selectMinPrecInsertionIndex,
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types (BaseTy(..), BindingError, NodeId(..), getNodeId)
import MLF.Frontend.Syntax (Lit(..), VarName)

-- | Explicitly typed types for elaboration (xMLF).
-- Corresponds to Figure 1 in "A Church-Style Intermediate Language for MLF".
--
-- xMLF extends System F with instance-bounded polymorphism (flexible quantification):
--   ∀(α ⩾ τ). σ
--
-- This restricts the variable α to range only over instances of τ.
--
-- Constructors:
--   * TVar: Type variables (α)
--   * TArrow: Function types (τ -> τ)
--   * TBase: Base types (Int, Bool, etc.) - extension of the paper's calculus
--   * TForall: Flexible quantification ∀(α ⩾ τ). σ.
--       - Nothing bound implies ⩾ ⊥ (standard System F unbounded quantification)
--       - Just bound implies explicit instance bound
--   * TBottom: The bottom type ⊥ (minimal type), used as the default bound.
data ElabType
    = TVar String
    | TArrow ElabType ElabType
    | TBase BaseTy
    | TForall String (Maybe ElabType) ElabType  -- ∀(α ⩾ τ?). σ
    | TBottom                                   -- ⊥ (minimal type)
    deriving (Eq, Show)

-- | Polymorphic schemes (multiple quantifiers).
data ElabScheme = Forall [(String, Maybe ElabType)] ElabType
    deriving (Eq, Show)

-- | Environment entry for elaboration (let-generalized schemes only).
data SchemeInfo = SchemeInfo
    { siScheme :: ElabScheme
    , siSubst :: IntMap String
    } deriving (Eq, Show)

-- | Instantiation witnesses (φ) for xMLF.
-- These explicitly record how a polymorphic type is instantiated.
--
-- From the FLOPS 2010 paper:
--   φ ::= 1        -- identity
--       | ⟨τ⟩      -- type application (substitute for outermost var)
--       | τ        -- bottom instantiation (substitute ⊥ with τ)
--       | O        -- introduce ∀ (skip outermost quantifier)
--       | φ; φ'    -- sequential composition
data Instantiation
    = InstId                                -- 1 (identity)
    | InstApp ElabType                      -- ⟨τ⟩ (type application)
    | InstBot ElabType                      -- τ (instantiate ⊥)
    | InstIntro                             -- O (introduce/skip ∀)
    | InstElim                              -- N (eliminate ∀)
    | InstAbstr String                      -- !α (abstract bound)
    | InstUnder String Instantiation        -- ∀(α ⩾) φ (under)
    | InstInside Instantiation              -- ∀(⩾ φ) (inside)
    | InstSeq Instantiation Instantiation   -- φ; φ' (composition)
    deriving (Eq, Show)

-- | Explicitly typed terms with type abstractions and instantiations (xMLF).
data ElabTerm
    = EVar String
    | ELit Lit
    | ELam String ElabType ElabTerm
    | EApp ElabTerm ElabTerm
    | ELet String ElabScheme ElabTerm ElabTerm
    | ETyAbs String (Maybe ElabType) ElabTerm  -- Λ(α ⩾ τ?). e (bounded type abstraction)
    | ETyInst ElabTerm Instantiation           -- e φ (instantiation)
    deriving (Eq, Show)

-- | Simple pretty-printing class for elaborated artifacts.
class Pretty a where
    pretty :: a -> String

instance Pretty ElabType where
    pretty =
        let go t = case t of
                TVar v -> v
                TBase (BaseTy b) -> b
                TArrow a b -> p a ++ " -> " ++ pretty b
                TForall v Nothing body -> "∀" ++ v ++ ". " ++ pretty body
                TForall v (Just bound) body ->
                    "∀(" ++ v ++ " ⩾ " ++ pretty bound ++ "). " ++ pretty body
                TBottom -> "⊥"
            p x@TArrow{} = "(" ++ pretty x ++ ")"
            p x@TForall{} = "(" ++ pretty x ++ ")"
            p x = pretty x
        in go

instance Pretty ElabScheme where
    pretty (Forall [] ty) = pretty ty
    pretty (Forall binds ty) = "∀" ++ unwords (map prettyBind binds) ++ ". " ++ pretty ty
      where
        prettyBind (v, Nothing) = v
        prettyBind (v, Just bound) = "(" ++ v ++ " ⩾ " ++ pretty bound ++ ")"

instance Pretty Instantiation where
    pretty inst = case inst of
        InstId -> "1"
        InstApp ty -> "⟨" ++ pretty ty ++ "⟩"
        InstBot ty -> pretty ty
        InstIntro -> "O"
        InstElim -> "N"
        InstAbstr v -> "!" ++ v
        InstUnder v i -> "∀(" ++ v ++ " ⩾) " ++ pretty i
        InstInside i -> "∀(⩾ " ++ pretty i ++ ")"
        InstSeq i1 i2 -> pretty i1 ++ "; " ++ pretty i2

instance Pretty ElabTerm where
    pretty term = case term of
        EVar v -> v
        ELit l -> case l of
            LInt i -> show i
            LBool b -> if b then "true" else "false"
            LString s -> show s
        ELam v ty body -> "λ" ++ v ++ ":" ++ pretty ty ++ ". " ++ pretty body
        EApp f a -> par (pretty f) ++ " " ++ parArg a
          where
            parArg x@EApp{} = par (pretty x)
            parArg x@ELam{} = par (pretty x)
            parArg x = pretty x
        ELet v sch rhs body -> "let " ++ v ++ " : " ++ pretty sch ++ " = " ++ pretty rhs ++ " in " ++ pretty body
        ETyAbs v Nothing body -> "Λ" ++ v ++ ". " ++ pretty body
        ETyAbs v (Just bound) body -> "Λ(" ++ v ++ " ⩾ " ++ pretty bound ++ "). " ++ pretty body
        ETyInst e inst -> pretty e ++ " " ++ prettyInst inst
      where
        par s = "(" ++ s ++ ")"
        prettyInst InstId = "1"
        prettyInst i = "[" ++ pretty i ++ "]"

-- | Errors that can arise during elaboration or reification.
data ElabError
    = ResidualTyExp NodeId
    | MissingNode NodeId
    | FreeVarOutOfScope NodeId
    | EnvLookup VarName
    | ValidationFailed [String]
    | BindingTreeError BindingError
    | NameConflict String
    | InstantiationError String
    deriving (Eq, Show)

data TypeCheckError
    = TCUnboundVar String
    | TCExpectedArrow ElabType
    | TCArgumentMismatch ElabType ElabType
    | TCInstantiationError Instantiation ElabType String
    | TCTypeAbsVarInScope String
    | TCTypeAbsBoundMentionsVar String
    | TCUnboundTypeVar String
    | TCLetTypeMismatch ElabType ElabType
    deriving (Eq, Show)

bindingToElab :: Either BindingError a -> Either ElabError a
bindingToElab = either (Left . BindingTreeError) Right

-- | Context steps for reaching a node in the type structure.
--
-- Paper reference: computation/instantiation contexts (Ch. 15.3, Fig. 10).
-- A context is a sequence of steps:
--   - StepUnder: go under a quantifier (∀(α ⩾) ·)
--   - StepInside: go inside a bound (∀(⩾ ·))
data ContextStep
    = StepUnder String      -- ^ Go under quantifier with given binder name
    | StepInside            -- ^ Go inside the bound of a quantifier
    deriving (Eq, Show)

-- | Apply a paper-style instantiation context to an instantiation.
--
-- This encodes Figure 10 contexts:
--   C ::= {·} | ∀(⩾ C) | ∀(α ⩾) C
applyContext :: [ContextStep] -> Instantiation -> Instantiation
applyContext steps inner = foldr step inner steps
  where
    step cs inst = case cs of
        StepUnder v -> InstUnder v inst
        StepInside -> InstInside inst

-- | Select the insertion index for the paper’s @m = min≺{…}@ choice (Figure 10).
--
-- Given a binder spine @ids@ that is already ordered by the edge-local ≺ ordering,
-- choose the first binder position whose ≺-key is strictly greater than @n@’s,
-- while respecting a minimal insertion index @minIdx@ (from dependency cutoff).
--
-- Returns an index in @[0 .. length ids]@; inserting at @length ids@ appends.
selectMinPrecInsertionIndex
    :: Int
    -> IntMap Order.OrderKey
    -> (NodeId -> NodeId)
    -> NodeId
    -> [Maybe NodeId]
    -> Int
selectMinPrecInsertionIndex minIdx orderKeys canonical n ids =
    case IntMap.lookup (getNodeId (canonical n)) orderKeys of
        Nothing -> minIdx'
        Just nk ->
            let keyAt :: Int -> Maybe Order.OrderKey
                keyAt i = do
                    mbNid <- atMaybe i ids
                    nid <- mbNid
                    IntMap.lookup (getNodeId (canonical nid)) orderKeys

                pick =
                    [ i
                    | i <- [minIdx' .. length ids - 1]
                    , Just k <- [keyAt i]
                    , Order.compareOrderKey k nk == GT
                    ]
            in case pick of
                (i : _) -> i
                [] -> length ids
  where
    minIdx' = max 0 (min minIdx (length ids))

    atMaybe :: Int -> [a] -> Maybe a
    atMaybe i xs
        | i < 0 = Nothing
        | otherwise = case drop i xs of
            (x : _) -> Just x
            [] -> Nothing
