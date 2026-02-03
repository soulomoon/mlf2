{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- |
Module      : MLF.Elab.Types
Description : Elaborated types, terms, and error types for MLF
Copyright   : (c) 2024
License     : BSD-3-Clause

This module defines the core types used in the elaborated (typed) representation
of MLF programs. After constraint generation and solving, types are elaborated
into this form for type checking and code generation.

= Key Types

* 'ElabType' - Fully elaborated types with quantifiers and bounds
* 'ElabTerm' - Typed terms with explicit type annotations
* 'ElabScheme' - Polymorphic type schemes with explicit binders
* 'Instantiation' - Witnesses for type instantiation

= Error Types

* 'ElabError' - Errors that can occur during elaboration
* 'TypeCheckError' - Specific type checking failures
-}
module MLF.Elab.Types (
    ElabType,
    Ty(..),
    TopVar(..),
    BoundType,
    TyIF(..),
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
    buildForalls,
    schemeFromType,
    selectMinPrecInsertionIndex,
) where

import Data.Functor.Foldable (cata, zygo)
import Data.List.NonEmpty (NonEmpty(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Constraint.Types.Graph (NodeId(..), getNodeId)
import MLF.Frontend.Syntax (Lit(..))
import MLF.Util.ElabError (ElabError(..), bindingToElab)
import MLF.Types.Elab
import MLF.Reify.TypeOps (splitForalls, substTypeCapture, freeTypeVarsType)

-- | Simple pretty-printing class for elaborated artifacts.
class Pretty a where
    pretty :: a -> String

-- | Pretty-printing that applies display-only bound inlining (§8.3.1).
class PrettyDisplay a where
    prettyDisplay :: a -> String

prettyBound :: BoundType -> String
prettyBound = pretty . tyToElab

prettyDisplayBound :: BoundType -> String
prettyDisplayBound = prettyDisplay . tyToElab

instance Pretty ElabType where
    pretty = unK . zygoIx complexAlg prettyAlg
      where
        complexAlg :: TyIF i (K Bool) -> K Bool i
        complexAlg ty = case ty of
            TArrowIF _ _ -> K True
            TForallIF _ _ _ -> K True
            TConIF _ _ -> K True
            _ -> K False

        prettyAlg :: TyIF i (IxPair (K Bool) (K String)) -> K String i
        prettyAlg ty = case ty of
            TVarIF v -> K v
            TBaseIF (BaseTy b) -> K b
            TArrowIF d c ->
                let (isComplex, l) = unIxPair d
                    (_, r) = unIxPair c
                    left = if unK isComplex then "(" ++ unK l ++ ")" else unK l
                in K (left ++ " -> " ++ unK r)
            TConIF (BaseTy c) args ->
                let parArg :: IxPair (K Bool) (K String) 'AllowVar -> String
                    parArg ix =
                        let (isComplex, s) = unIxPair ix
                            sStr = unK s
                        in if unK isComplex then "(" ++ sStr ++ ")" else sStr
                    argsStr = case args of
                        arg :| rest -> unwords (map parArg (arg : rest))
                in K (c ++ " " ++ argsStr)
            TForallIF v mb body ->
                let boundStr = fmap (unK . snd . unIxPair) mb
                in case boundStr of
                    Nothing -> K ("∀" ++ v ++ ". " ++ unK (snd (unIxPair body)))
                    Just bound -> K ("∀(" ++ v ++ " ⩾ " ++ bound ++ "). " ++ unK (snd (unIxPair body)))
            TBottomIF -> K "⊥"

instance Pretty ElabScheme where
    pretty (Forall [] ty) = pretty ty
    pretty (Forall binds ty) = "∀" ++ unwords (map prettyBind binds) ++ ". " ++ pretty ty
      where
        prettyBind (v, Nothing) = v
        prettyBind (v, Just bound) = "(" ++ v ++ " ⩾ " ++ prettyBound bound ++ ")"

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
            "Λ(" ++ v ++ " ⩾ " ++ prettyTy (tyToElab bound) ++ "). " ++ snd body
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
    go ty = case ty of
        TArrow d c -> TArrow (go d) (go c)
        TCon c args -> TCon c (fmap go args)
        TForall v mb body ->
            let mb' = fmap goBound mb
                body' = go body
            in simplifyForall v mb' body'
        TVar v -> TVar v
        TBase b -> TBase b
        TBottom -> TBottom

    simplifyForall v mb body =
        case mb of
            Nothing ->
                if Set.member v (freeVarsType body)
                    then TForall v Nothing body
                    else body
            Just bound ->
                let boundTy = tyToElab bound
                    freeInBound = Set.member v (freeTypeVarsType bound)
                    (posCount, negCount) = occurrencesIn body
                    totalCount = posCount + negCount
                in if freeInBound
                    then TForall v (Just bound) body
                    else if totalCount == 0
                        then body
                        else if inlineableBound boundTy
                            then go (substTypeCapture v boundTy body)
                            else TForall v (Just bound) body
      where
        occurrencesIn = occurrencesVar v

    inlineableBound :: ElabType -> Bool
    inlineableBound ty = case ty of
        TBase{} -> True
        TBottom -> True
        TVar{} -> True
        TArrow{} -> True
        TCon{} -> True
        _ -> False

    goBound :: BoundType -> BoundType
    goBound bound = case bound of
        TArrow a b -> TArrow (go a) (go b)
        TCon c args -> TCon c (fmap go args)
        TBase b -> TBase b
        TBottom -> TBottom
        TForall v mb body ->
            let mb' = fmap goBound mb
                body' = go body
            in TForall v mb' body'

    occurrencesVar :: String -> ElabType -> (Int, Int)
    occurrencesVar name = Map.findWithDefault (0, 0) name . oiOccMap . occInfo

    freeVarsType :: ElabType -> Set.Set String
    freeVarsType = oiFreeVars . occInfo

    emptyOccInfo :: OccInfo
    emptyOccInfo = OccInfo Set.empty Map.empty

    occInfo :: ElabType -> OccInfo
    occInfo = unK . paraIx occAlg
      where
        occAlg :: TyIF i (IxPair Ty (K OccInfo)) -> K OccInfo i
        occAlg ty = case ty of
            TVarIF v -> K (OccInfo (Set.singleton v) (Map.singleton v (1, 0)))
            TArrowIF d c ->
                let occD = unK (snd (unIxPair d))
                    occC = unK (snd (unIxPair c))
                    freeVars = Set.union (oiFreeVars occD) (oiFreeVars occC)
                    occD' = flipOccMap (oiOccMap occD)
                    occC' = oiOccMap occC
                in K (OccInfo freeVars (mergeOccMaps occD' occC'))
            TConIF _ args ->
                let occArg :: IxPair Ty (K OccInfo) 'AllowVar -> OccInfo
                    occArg ix = unK (snd (unIxPair ix))
                    occArgs = case args of
                        arg :| rest -> map occArg (arg : rest)
                    freeVars = Set.unions (map oiFreeVars occArgs)
                    occMaps = map oiOccMap occArgs
                in K (OccInfo freeVars (foldr mergeOccMaps Map.empty occMaps))
            TBaseIF _ -> K emptyOccInfo
            TBottomIF -> K emptyOccInfo
            TForallIF v mb body ->
                let occBody = unK (snd (unIxPair body))
                    occBound = maybe emptyOccInfo (occInfoBound . fst . unIxPair) mb
                    freeBound = oiFreeVars occBound
                    freeVars = Set.union freeBound (Set.delete v (oiFreeVars occBody))
                    occBody' = Map.delete v (oiOccMap occBody)
                    occBound' = Map.delete v (oiOccMap occBound)
                in K (OccInfo freeVars (mergeOccMaps occBound' occBody'))

    mergeOccMaps = Map.unionWith addCounts
    addCounts (p1, n1) (p2, n2) = (p1 + p2, n1 + n2)
    flipOccMap = Map.map (\(p, n) -> (n, p))

    occInfoBound :: BoundType -> OccInfo
    occInfoBound bound = case bound of
        TArrow a b ->
            let occA = occInfo a
                occB = occInfo b
                freeVars = Set.union (oiFreeVars occA) (oiFreeVars occB)
                occA' = flipOccMap (oiOccMap occA)
            in OccInfo freeVars (mergeOccMaps occA' (oiOccMap occB))
        TCon _ args ->
            let occArgs = case args of
                    arg :| rest -> map occInfo (arg : rest)
                freeVars = Set.unions (map oiFreeVars occArgs)
                occMaps = map oiOccMap occArgs
            in OccInfo freeVars (foldr mergeOccMaps Map.empty occMaps)
        TBase _ -> emptyOccInfo
        TBottom -> emptyOccInfo
        TForall v mb body ->
            let occBody = occInfo body
                occBound = maybe emptyOccInfo occInfoBound mb
                freeBound = oiFreeVars occBound
                freeVars = Set.union freeBound (Set.delete v (oiFreeVars occBody))
                occBody' = Map.delete v (oiOccMap occBody)
                occBound' = Map.delete v (oiOccMap occBound)
            in OccInfo freeVars (mergeOccMaps occBound' occBody')

-- | Pretty-printing with display-only bound inlining.
instance PrettyDisplay ElabType where
    prettyDisplay = pretty . inlineBoundsForDisplay

instance PrettyDisplay ElabScheme where
    prettyDisplay sch =
        let ty = inlineBoundsForDisplay (schemeToTypeLocal sch)
            (binds, body) = splitForalls ty
        in case binds of
            [] -> prettyDisplay body
            _ -> "∀" ++ unwords (map prettyBind binds) ++ ". " ++ prettyDisplay body
      where
        prettyBind (v, Nothing) = v
        prettyBind (v, Just bound) = "(" ++ v ++ " ⩾ " ++ prettyDisplayBound bound ++ ")"

instance PrettyDisplay Instantiation where
    prettyDisplay = prettyInstWith prettyDisplay

instance PrettyDisplay ElabTerm where
    prettyDisplay = prettyTermWith prettyDisplay prettyDisplay prettyDisplay

schemeToTypeLocal :: ElabScheme -> ElabType
schemeToTypeLocal (Forall binds body) = buildForalls binds body

buildForalls :: [(String, Maybe BoundType)] -> ElabType -> ElabType
buildForalls binds body = foldr (\(v, b) t -> TForall v b t) body binds

schemeFromType :: ElabType -> ElabScheme
schemeFromType ty =
    let (binds, body) = splitForalls ty
    in mkElabScheme binds body

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
