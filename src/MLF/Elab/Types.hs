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

import Data.List.NonEmpty (NonEmpty(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Constraint.Types.Graph (NodeId(..), getNodeId)
import MLF.Util.ElabError (ElabError(..), bindingToElab)
import MLF.Types.Elab
import MLF.Reify.TypeOps (splitForalls, substTypeCapture, freeTypeVarsType)
import qualified MLF.XMLF.Pretty as XMLFPretty
import qualified MLF.XMLF.Syntax as XMLF

-- | Simple pretty-printing class for elaborated artifacts.
class Pretty a where
    pretty :: a -> String

-- | Pretty-printing that applies display-only bound inlining (§8.3.1).
class PrettyDisplay a where
    prettyDisplay :: a -> String

instance Pretty ElabType where
    pretty = XMLFPretty.prettyXmlfType . toXmlfType

instance Pretty ElabScheme where
    pretty (Forall binds ty) = pretty (buildForalls binds ty)

instance Pretty Instantiation where
    pretty = XMLFPretty.prettyXmlfComp . toXmlfComp

instance Pretty ElabTerm where
    pretty = prettyTermCanonical

prettyTermCanonical :: ElabTerm -> String
prettyTermCanonical term = case term of
    ELet v sch rhs body ->
        "let " ++ v
            ++ " : "
            ++ pretty sch
            ++ " = "
            ++ prettyTermCanonical rhs
            ++ " in "
            ++ prettyTermCanonical body
    _ ->
        XMLFPretty.prettyXmlfTerm (toXmlfTerm term)

toXmlfType :: ElabType -> XMLF.XmlfType
toXmlfType ty = case ty of
    TVar v -> XMLF.XTVar v
    TArrow a b -> XMLF.XTArrow (toXmlfType a) (toXmlfType b)
    TCon (BaseTy c) args -> XMLF.XTCon c (fmap toXmlfType args)
    TBase (BaseTy b) -> XMLF.XTBase b
    TForall v mb body ->
        let bound = maybe XMLF.XTBottom toXmlfBound mb
        in XMLF.XTForall v bound (toXmlfType body)
    TBottom -> XMLF.XTBottom

toXmlfBound :: BoundType -> XMLF.XmlfType
toXmlfBound bound = case bound of
    TArrow a b -> XMLF.XTArrow (toXmlfType a) (toXmlfType b)
    TCon (BaseTy c) args -> XMLF.XTCon c (fmap toXmlfType args)
    TBase (BaseTy b) -> XMLF.XTBase b
    TForall v mb body ->
        let boundTy = maybe XMLF.XTBottom toXmlfBound mb
        in XMLF.XTForall v boundTy (toXmlfType body)
    TBottom -> XMLF.XTBottom

toXmlfComp :: Instantiation -> XMLF.XmlfComp
toXmlfComp inst = case inst of
    InstId -> XMLF.XCId
    InstApp ty -> compFromType ty
    InstBot ty -> XMLF.XCBot (toXmlfType ty)
    InstIntro -> XMLF.XCIntro
    InstElim -> XMLF.XCElim
    InstAbstr v -> XMLF.XCHyp v
    InstUnder v i -> XMLF.XCOuter v (toXmlfComp i)
    InstInside i -> XMLF.XCInner (toXmlfComp i)
    InstSeq i1 i2 -> XMLF.XCSeq (toXmlfComp i1) (toXmlfComp i2)
  where
    compFromType ty =
        XMLF.XCSeq
            (XMLF.XCInner (XMLF.XCBot (toXmlfType ty)))
            XMLF.XCElim

toXmlfTerm :: ElabTerm -> XMLF.XmlfTerm
toXmlfTerm term = case term of
    EVar v -> XMLF.XVar v
    ELit l -> XMLF.XLit l
    ELam v ty body -> XMLF.XLam v (toXmlfType ty) (toXmlfTerm body)
    EApp f a -> XMLF.XApp (toXmlfTerm f) (toXmlfTerm a)
    ELet v _ rhs body -> XMLF.XLet v (toXmlfTerm rhs) (toXmlfTerm body)
    ETyAbs v mb body ->
        let bound = maybe XMLF.XTBottom toXmlfBound mb
        in XMLF.XTyAbs v bound (toXmlfTerm body)
    ETyInst e inst -> XMLF.XTyInst (toXmlfTerm e) (toXmlfComp inst)

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
    prettyDisplay = prettyDisplay . inlineBoundsForDisplay . schemeToTypeLocal

instance PrettyDisplay Instantiation where
    prettyDisplay = pretty

instance PrettyDisplay ElabTerm where
    prettyDisplay = pretty

schemeToTypeLocal :: ElabScheme -> ElabType
schemeToTypeLocal (Forall binds body) = buildForalls binds body

buildForalls :: [(String, Maybe BoundType)] -> ElabType -> ElabType
buildForalls binds body = foldr (\(v, b) t -> TForall v b t) body binds

schemeFromType :: ElabType -> ElabScheme
schemeFromType ty =
    let (binds0, body0) = splitForalls ty
        binderNames = map fst binds0
        bodyFvs = Set.toList (freeTypeVarsType body0)
        bodyFvSet = Set.fromList bodyFvs
        externalFvs =
            [ v
            | v <- bodyFvs
            , v `notElem` binderNames
            ]
        unusedBinders =
            [ v
            | v <- binderNames
            , not (Set.member v bodyFvSet)
            ]
        ty' =
            if length externalFvs <= length unusedBinders && not (null externalFvs)
                then
                    foldl
                        (\acc (fromV, toV) -> substTypeCapture fromV (TVar toV) acc)
                        ty
                        (zip externalFvs unusedBinders)
                else ty
        (binds, body) = splitForalls ty'
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
