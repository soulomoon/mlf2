{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
module MLF.Types.Elab (
    ElabType(..),
    ElabTypeF(..),
    ElabScheme(..),
    SchemeInfo(..),
    ElabTerm(..),
    ElabTermF(..),
    Instantiation(..),
    InstantiationF(..)
) where

import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.IntMap.Strict (IntMap)

import MLF.Constraint.Types (BaseTy(..))
import MLF.Frontend.Syntax (Lit(..))

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

data ElabTypeF a
    = TVarF String
    | TArrowF a a
    | TBaseF BaseTy
    | TForallF String (Maybe a) a
    | TBottomF
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base ElabType = ElabTypeF

instance Recursive ElabType where
    project ty = case ty of
        TVar v -> TVarF v
        TArrow a b -> TArrowF a b
        TBase b -> TBaseF b
        TForall v mb body -> TForallF v mb body
        TBottom -> TBottomF

instance Corecursive ElabType where
    embed ty = case ty of
        TVarF v -> TVar v
        TArrowF a b -> TArrow a b
        TBaseF b -> TBase b
        TForallF v mb body -> TForall v mb body
        TBottomF -> TBottom

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

data InstantiationF a
    = InstIdF
    | InstAppF ElabType
    | InstBotF ElabType
    | InstIntroF
    | InstElimF
    | InstAbstrF String
    | InstUnderF String a
    | InstInsideF a
    | InstSeqF a a
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base Instantiation = InstantiationF

instance Recursive Instantiation where
    project inst = case inst of
        InstId -> InstIdF
        InstApp ty -> InstAppF ty
        InstBot ty -> InstBotF ty
        InstIntro -> InstIntroF
        InstElim -> InstElimF
        InstAbstr v -> InstAbstrF v
        InstUnder v i -> InstUnderF v i
        InstInside i -> InstInsideF i
        InstSeq a b -> InstSeqF a b

instance Corecursive Instantiation where
    embed inst = case inst of
        InstIdF -> InstId
        InstAppF ty -> InstApp ty
        InstBotF ty -> InstBot ty
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstAbstrF v -> InstAbstr v
        InstUnderF v i -> InstUnder v i
        InstInsideF i -> InstInside i
        InstSeqF a b -> InstSeq a b

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

data ElabTermF a
    = EVarF String
    | ELitF Lit
    | ELamF String ElabType a
    | EAppF a a
    | ELetF String ElabScheme a a
    | ETyAbsF String (Maybe ElabType) a
    | ETyInstF a Instantiation
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base ElabTerm = ElabTermF

instance Recursive ElabTerm where
    project term = case term of
        EVar v -> EVarF v
        ELit l -> ELitF l
        ELam v ty body -> ELamF v ty body
        EApp f a -> EAppF f a
        ELet v sch rhs body -> ELetF v sch rhs body
        ETyAbs v mb body -> ETyAbsF v mb body
        ETyInst e inst -> ETyInstF e inst

instance Corecursive ElabTerm where
    embed term = case term of
        EVarF v -> EVar v
        ELitF l -> ELit l
        ELamF v ty body -> ELam v ty body
        EAppF f a -> EApp f a
        ELetF v sch rhs body -> ELet v sch rhs body
        ETyAbsF v mb body -> ETyAbs v mb body
        ETyInstF e inst -> ETyInst e inst
