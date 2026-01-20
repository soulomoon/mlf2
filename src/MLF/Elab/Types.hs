{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
module MLF.Elab.Types (
    ElabType(..),
    ElabTypeF(..),
    ElabScheme(..),
    SchemeInfo(..),
    ElabTerm(..),
    ElabTermF(..),
    Instantiation(..),
    InstantiationF(..),
    ElabError(..),
    TypeCheckError(..),
    bindingToElab,
    Pretty(..),
    PrettyDisplay(..),
    ContextStep(..),
    applyContext,
    selectMinPrecInsertionIndex,
) where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..), apo, cata, para, zygo)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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

-- | Simple pretty-printing class for elaborated artifacts.
class Pretty a where
    pretty :: a -> String

-- | Pretty-printing that applies display-only bound inlining (§8.3.1).
class PrettyDisplay a where
    prettyDisplay :: a -> String

instance Pretty ElabType where
    pretty = zygo complexAlg prettyAlg
      where
        complexAlg ty = case ty of
            TArrowF _ _ -> True
            TForallF _ _ _ -> True
            _ -> False

        prettyAlg ty = case ty of
            TVarF v -> v
            TBaseF (BaseTy b) -> b
            TArrowF (isComplex, l) (_, r) ->
                let left = if isComplex then "(" ++ l ++ ")" else l
                in left ++ " -> " ++ r
            TForallF v mb body ->
                case fmap snd mb of
                    Nothing -> "∀" ++ v ++ ". " ++ snd body
                    Just bound -> "∀(" ++ v ++ " ⩾ " ++ bound ++ "). " ++ snd body
            TBottomF -> "⊥"

instance Pretty ElabScheme where
    pretty (Forall [] ty) = pretty ty
    pretty (Forall binds ty) = "∀" ++ unwords (map prettyBind binds) ++ ". " ++ pretty ty
      where
        prettyBind (v, Nothing) = v
        prettyBind (v, Just bound) = "(" ++ v ++ " ⩾ " ++ pretty bound ++ ")"

instance Pretty Instantiation where
    pretty = prettyInstWith pretty

instance Pretty ElabTerm where
    pretty = prettyTermWith pretty pretty pretty

prettyInstWith :: (ElabType -> String) -> Instantiation -> String
prettyInstWith prettyTy = cata alg
  where
    alg inst = case inst of
        InstIdF -> "1"
        InstAppF ty -> "⟨" ++ prettyTy ty ++ "⟩"
        InstBotF ty -> prettyTy ty
        InstIntroF -> "O"
        InstElimF -> "N"
        InstAbstrF v -> "!" ++ v
        InstUnderF v i -> "∀(" ++ v ++ " ⩾) " ++ i
        InstInsideF i -> "∀(⩾ " ++ i ++ ")"
        InstSeqF i1 i2 -> i1 ++ "; " ++ i2

prettyTermWith
    :: (ElabType -> String)
    -> (ElabScheme -> String)
    -> (Instantiation -> String)
    -> ElabTerm
    -> String
prettyTermWith prettyTy prettyScheme prettyInst = zygo needsParensAlg prettyAlg
  where
    needsParensAlg term = case term of
        EAppF _ _ -> True
        ELamF _ _ _ -> True
        _ -> False

    prettyAlg term = case term of
        EVarF v -> v
        ELitF l -> case l of
            LInt i -> show i
            LBool b -> if b then "true" else "false"
            LString s -> show s
        ELamF v ty body -> "λ" ++ v ++ ":" ++ prettyTy ty ++ ". " ++ snd body
        EAppF f a ->
            let par s = "(" ++ s ++ ")"
                arg =
                    if fst a
                        then par (snd a)
                        else snd a
            in par (snd f) ++ " " ++ arg
        ELetF v sch rhs body ->
            "let " ++ v ++ " : " ++ prettyScheme sch
                ++ " = " ++ snd rhs
                ++ " in " ++ snd body
        ETyAbsF v Nothing body -> "Λ" ++ v ++ ". " ++ snd body
        ETyAbsF v (Just bound) body ->
            "Λ(" ++ v ++ " ⩾ " ++ prettyTy bound ++ "). " ++ snd body
        ETyInstF e inst ->
            let instStr = case inst of
                    InstId -> "1"
                    _ -> "[" ++ prettyInst inst ++ "]"
            in snd e ++ " " ++ instStr

data OccInfo = OccInfo
    { oiFreeVars :: Set.Set String
    , oiOccMap :: Map.Map String (Int, Int)
    }

-- | Display-only bound inlining for presentation (§8.3.1).
inlineBoundsForDisplay :: ElabType -> ElabType
inlineBoundsForDisplay = go
  where
    -- Conservative approximation: inline only single covariant occurrences with base/var bounds.
    go = cata alg
      where
        alg ty = case ty of
            TArrowF d c -> TArrow d c
            TForallF v mb body ->
                let mb' = fmap go mb
                    body' = go body
                in simplifyForall v mb' body'
            TVarF v -> TVar v
            TBaseF b -> TBase b
            TBottomF -> TBottom

    simplifyForall v mb body =
        case mb of
            Nothing ->
                if Set.member v (freeVarsType body)
                    then TForall v Nothing body
                    else body
            Just bound ->
                let freeInBound = Set.member v (freeVarsType bound)
                    (posCount, negCount) = occurrencesIn body
                    totalCount = posCount + negCount
                in if freeInBound
                    then TForall v (Just bound) body
                    else if totalCount == 0
                        then body
                        else if inlineableBound bound
                            then go (substType v bound body)
                            else TForall v (Just bound) body
      where
        occurrencesIn = occurrencesVar v

    inlineableBound ty = case ty of
        TBase{} -> True
        TBottom -> True
        TVar{} -> True
        TArrow{} -> True
        _ -> False

    occurrencesVar :: String -> ElabType -> (Int, Int)
    occurrencesVar name = Map.findWithDefault (0, 0) name . oiOccMap . occInfo

    freeVarsType :: ElabType -> Set.Set String
    freeVarsType = oiFreeVars . occInfo

    emptyOccInfo :: OccInfo
    emptyOccInfo = OccInfo Set.empty Map.empty

    occInfo :: ElabType -> OccInfo
    occInfo = zygo freeAlg occAlg

    freeAlg :: ElabTypeF (Set.Set String) -> Set.Set String
    freeAlg ty = case ty of
        TVarF v -> Set.singleton v
        TArrowF d c -> Set.union d c
        TBaseF _ -> Set.empty
        TBottomF -> Set.empty
        TForallF v mb body ->
            let freeBound = maybe Set.empty id mb
                freeBody = Set.delete v body
            in Set.union freeBound freeBody

    occAlg :: ElabTypeF (Set.Set String, OccInfo) -> OccInfo
    occAlg ty = case ty of
        TVarF v -> OccInfo (Set.singleton v) (Map.singleton v (1, 0))
        TArrowF d c ->
            let freeVars = Set.union (fst d) (fst c)
                occD = flipOccMap (oiOccMap (snd d))
                occC = oiOccMap (snd c)
            in OccInfo freeVars (mergeOccMaps occD occC)
        TBaseF _ -> emptyOccInfo
        TBottomF -> emptyOccInfo
        TForallF v mb body ->
            let (freeBody, occBody) = body
                (freeBound, occBound) = maybe (Set.empty, emptyOccInfo) id mb
                freeVars = Set.union freeBound (Set.delete v freeBody)
                occBody' = Map.delete v (oiOccMap occBody)
                occBound' = Map.delete v (oiOccMap occBound)
            in OccInfo freeVars (mergeOccMaps occBound' occBody')

    mergeOccMaps = Map.unionWith addCounts
    addCounts (p1, n1) (p2, n2) = (p1 + p2, n1 + n2)
    flipOccMap = Map.map (\(p, n) -> (n, p))

    substType :: String -> ElabType -> ElabType -> ElabType
    substType name replacement = goSub
      where
        freeInReplacement = freeVarsType replacement

        goSub = para alg
          where
            alg ty = case ty of
                TVarF v
                    | v == name -> replacement
                    | otherwise -> TVar v
                TArrowF d c -> TArrow (snd d) (snd c)
                TBaseF b -> TBase b
                TBottomF -> TBottom
                TForallF v mb body
                    | v == name ->
                        let mb' = fmap snd mb
                        in TForall v mb' (fst body)
                    | Set.member v freeInReplacement ->
                        let used =
                                Set.unions
                                    [ freeInReplacement
                                    , freeVarsType (fst body)
                                    , maybe Set.empty (freeVarsType . fst) mb
                                    , Set.singleton v
                                    ]
                            v' = freshNameLike v used
                            body' = renameVar v v' (fst body)
                        in TForall v' (fmap snd mb) (goSub body')
                    | otherwise ->
                        TForall v (fmap snd mb) (snd body)

    renameVar :: String -> String -> ElabType -> ElabType
    renameVar old new = para alg
      where
        alg ty = case ty of
            TVarF v
                | v == old -> TVar new
                | otherwise -> TVar v
            TArrowF d c -> TArrow (snd d) (snd c)
            TBaseF b -> TBase b
            TBottomF -> TBottom
            TForallF v mb body
                | v == old -> TForall v (fmap fst mb) (fst body)
                | otherwise -> TForall v (fmap snd mb) (snd body)

    freshNameLike :: String -> Set.Set String -> String
    freshNameLike base used =
        let goFresh n =
                let candidate = base ++ show n
                in if Set.member candidate used
                    then goFresh (n + 1)
                    else candidate
        in if Set.member base used
            then goFresh (0 :: Int)
            else base

-- | Pretty-printing with display-only bound inlining.
instance PrettyDisplay ElabType where
    prettyDisplay = pretty . inlineBoundsForDisplay

instance PrettyDisplay ElabScheme where
    prettyDisplay sch =
        let ty = inlineBoundsForDisplay (schemeToTypeLocal sch)
            (binds, body) = splitForallsLocal ty
        in case binds of
            [] -> prettyDisplay body
            _ -> "∀" ++ unwords (map prettyBind binds) ++ ". " ++ prettyDisplay body
      where
        prettyBind (v, Nothing) = v
        prettyBind (v, Just bound) = "(" ++ v ++ " ⩾ " ++ prettyDisplay bound ++ ")"

instance PrettyDisplay Instantiation where
    prettyDisplay = prettyInstWith prettyDisplay

instance PrettyDisplay ElabTerm where
    prettyDisplay = prettyTermWith prettyDisplay prettyDisplay prettyDisplay

schemeToTypeLocal :: ElabScheme -> ElabType
schemeToTypeLocal (Forall binds body) = apo coalg (binds, body)
  where
    coalg ([], ty) = fmap Left (project ty)
    coalg ((n, b) : rest, ty) =
        TForallF n (fmap Left b) (Right (rest, ty))

splitForallsLocal :: ElabType -> ([(String, Maybe ElabType)], ElabType)
splitForallsLocal = para alg
  where
    alg ty = case ty of
        TForallF v mb bodyPair ->
            let mbOrig = fmap fst mb
                (binds, body) = snd bodyPair
            in ((v, mbOrig) : binds, body)
        _ -> ([], embed (fmap fst ty))

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
    | SchemeFreeVars NodeId [String]
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
