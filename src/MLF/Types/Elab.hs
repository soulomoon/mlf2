{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
module MLF.Types.Elab (
    Ty(..),
    TopVar(..),
    ElabType,
    BoundType,
    TyIF(..),
    IxFix(..),
    IxFunctor(..),
    IxRecursive(..),
    IxCorecursive(..),
    IxPair(..),
    cataIx,
    cataIxConst,
    paraIx,
    zygoIx,
    K(..),
    tyToElab,
    elabToBound,
    containsForallTy,
    containsArrowTy,
    ElabScheme,
    pattern Forall,
    mkElabScheme,
    schemeBindings,
    schemeBody,
    SchemeInfo(..),
    ElabTerm(..),
    ElabTermF(..),
    Instantiation(..),
    InstantiationF(..)
) where

import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty)

import MLF.Constraint.Types.Graph (BaseTy(..), BindFlag(..))
import MLF.Frontend.Syntax (Lit(..))
import Util.IndexedRecursion
    ( IxFunctor(..)
    , IxBase
    , IxRecursive(..)
    , IxCorecursive(..)
    , IxPair(..)
    , IxFix(..)
    , K(..)
    , cataIx
    , cataIxConst
    , paraIx
    , zygoIx
    )

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
--   * TCon: Constructor application (C σ), per thesis Fig. 14.2.1.
--   * TBase: Base types (Int, Bool, etc.). This is a 0-ary constructor convenience.
--   * TForall: Flexible quantification ∀(α ⩾ τ). σ.
--       - Nothing bound implies ⩾ ⊥ (standard System F unbounded quantification)
--       - Just bound implies explicit instance bound
--   * TBottom: The bottom type ⊥ (minimal type), used as the default bound.
data TopVar = AllowVar | NoTopVar

data Ty (v :: TopVar) where
    TVar :: String -> Ty 'AllowVar
    TArrow :: Ty AllowVar -> Ty AllowVar -> Ty a
    TCon :: BaseTy -> NonEmpty (Ty AllowVar) -> Ty a
    TBase :: BaseTy -> Ty a
    TForall :: String -> Maybe (Ty 'NoTopVar) -> Ty AllowVar -> Ty a -- ∀(α ⩾ τ?). σ
    TBottom :: Ty a

deriving instance Eq (Ty v)
deriving instance Show (Ty v)

type ElabType = Ty 'AllowVar
type BoundType = Ty 'NoTopVar

-- | Indexed base functor for Ty. Recursive positions are explicitly indexed.
data TyIF (v :: TopVar) (r :: TopVar -> Type) where
    TVarIF :: String -> TyIF 'AllowVar r
    TArrowIF :: r 'AllowVar -> r 'AllowVar -> TyIF v r
    TConIF :: BaseTy -> NonEmpty (r 'AllowVar) -> TyIF v r
    TBaseIF :: BaseTy -> TyIF v r
    TForallIF :: String -> Maybe (r 'NoTopVar) -> r 'AllowVar -> TyIF v r
    TBottomIF :: TyIF v r

instance IxFunctor TyIF where
    imap f node = case node of
        TVarIF v -> TVarIF v
        TArrowIF a b -> TArrowIF (f a) (f b)
        TConIF c args -> TConIF c (fmap f args)
        TBaseIF b -> TBaseIF b
        TForallIF v mb body -> TForallIF v (fmap f mb) (f body)
        TBottomIF -> TBottomIF

type instance IxBase Ty = TyIF

instance IxRecursive Ty where
    projectIx ty = case ty of
        TVar v -> TVarIF v
        TArrow a b -> TArrowIF a b
        TCon c args -> TConIF c args
        TBase b -> TBaseIF b
        TForall v mb body -> TForallIF v mb body
        TBottom -> TBottomIF

instance IxCorecursive Ty where
    embedIx ty = case ty of
        TVarIF v -> TVar v
        TArrowIF a b -> TArrow a b
        TConIF c args -> TCon c args
        TBaseIF b -> TBase b
        TForallIF v mb body -> TForall v mb body
        TBottomIF -> TBottom

tyToElab :: Ty v -> ElabType
tyToElab ty = case ty of
    TVar v -> TVar v
    TArrow a b -> TArrow (tyToElab a) (tyToElab b)
    TCon c args -> TCon c (fmap tyToElab args)
    TBase b -> TBase b
    TBottom -> TBottom
    TForall v mb body -> TForall v mb (tyToElab body)

elabToBound :: ElabType -> Either String BoundType
elabToBound ty = case ty of
    TVar v ->
        Left ("elabToBound: unexpected variable bound " ++ show v)
    TArrow a b -> Right (TArrow a b)
    TCon c args -> Right (TCon c args)
    TBase b -> Right (TBase b)
    TForall v mb body -> Right (TForall v mb body)
    TBottom -> Right TBottom

containsForallTy :: Ty v -> Bool
containsForallTy = cataIxConst alg
  where
    alg node = case node of
        TForallIF _ _ _ -> True
        TArrowIF a b -> unK a || unK b
        TConIF _ args -> any unK args
        _ -> False

containsArrowTy :: Ty v -> Bool
containsArrowTy = cataIxConst alg
  where
    alg node = case node of
        TArrowIF _ _ -> True
        TForallIF _ mb body -> maybe False unK mb || unK body
        TConIF _ args -> any unK args
        _ -> False

data Binder (k :: BindFlag) where
    FlexBinder :: String -> Maybe BoundType -> Binder 'BindFlex

data Scheme (k :: BindFlag) where
    Scheme :: [Binder k] -> ElabType -> Scheme k

type ElabScheme = Scheme 'BindFlex

bindersToPairs :: [Binder 'BindFlex] -> [(String, Maybe BoundType)]
bindersToPairs = map (\(FlexBinder v mb) -> (v, mb))

schemeToPairs :: ElabScheme -> ([(String, Maybe BoundType)], ElabType)
schemeToPairs (Scheme binds body) = (bindersToPairs binds, body)

pattern Forall :: [(String, Maybe BoundType)] -> ElabType -> ElabScheme
pattern Forall binds body <- (schemeToPairs -> (binds, body))
  where
    Forall binds body = mkElabScheme binds body
{-# COMPLETE Forall #-}

mkElabScheme :: [(String, Maybe BoundType)] -> ElabType -> ElabScheme
mkElabScheme binds body = Scheme (map (uncurry FlexBinder) binds) body

schemeBindings :: ElabScheme -> [(String, Maybe BoundType)]
schemeBindings = fst . schemeToPairs

schemeBody :: ElabScheme -> ElabType
schemeBody = snd . schemeToPairs

instance Eq (Scheme 'BindFlex) where
    s1 == s2 =
        let (b1, t1) = schemeToPairs s1
            (b2, t2) = schemeToPairs s2
        in b1 == b2 && t1 == t2

instance Show (Scheme 'BindFlex) where
    show s =
        let (binds, body) = schemeToPairs s
        in "Forall " ++ show binds ++ " " ++ show body

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
    | ETyAbs String (Maybe BoundType) ElabTerm  -- Λ(α ⩾ τ?). e (bounded type abstraction)
    | ETyInst ElabTerm Instantiation           -- e φ (instantiation)
    deriving (Eq, Show)

data ElabTermF a
    = EVarF String
    | ELitF Lit
    | ELamF String ElabType a
    | EAppF a a
    | ELetF String ElabScheme a a
    | ETyAbsF String (Maybe BoundType) a
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
