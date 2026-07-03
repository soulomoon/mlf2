{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
* 'XmlfTerm' - Typed terms with explicit type annotations
* 'ElabScheme' - Polymorphic type schemes with explicit binders
* 'Instantiation' - Witnesses for type instantiation

= Error Types

* 'ElabError' - Errors that can occur during elaboration
* 'TypeCheckError' - Specific type checking failures
-}
module MLF.Elab.Types (
    ElabType,
    Ty
        ( TVarRef,
          TArrow,
          TConWithIdentity,
          TCon,
          TVarAppRef,
          TBaseWithIdentity,
          TBase,
          TForallRef,
          TMuRef,
          TBottom
        ),
    TopVar(..),
    BoundType,
    tVarWithRef,
    tVarAppWithRef,
    tForallWithRef,
    tMuWithRef,
    TyIF
        ( TVarIFRef,
          TArrowIF,
          TConIFWithIdentity,
          TConIF,
          TVarAppIFRef,
          TBaseIFWithIdentity,
          TBaseIF,
          TForallIFRef,
          TMuIFRef,
          TBottomIF
        ),
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
    UniqueIdentity(..),
    ElabScheme,
    mkElabSchemeWithRefs,
    schemeBinderRefs,
    schemeBody,
    SchemeInfo(SchemeInfo, siScheme, siSubstRefs),
    TypeBinderIdentity,
    typeBinderIdentityFromNode,
    typeBinderIdentityNode,
    typeBinderIdentityKey,
    typeBinderIdentityFromUnique,
    TypeBinderRef,
    typeBinderRefFromIdentity,
    typeBinderRefIdentity,
    typeBinderRefNode,
    typeBinderRefName,
    typeBinderRefsSameIdentity,
    typeBinderRefsSameIdentityAndName,
    renameTypeBinderRef,
    freshTypeBinderRef,
    sourceTypeBinderRefForName,
    typeBinderRefFromIdentityOrFresh,
    sourceTypeBinderRefsFromIdentities,
    sourceTypeBinderRefOrFresh,
    sourceTypeBinderRefOrFreshInScope,
    freshTypeBinderRefFromNames,
    instAbstrWithRef,
    instUnderWithRef,
    schemeInfoBinderIdentityKeys,
    schemeInfoBinderIdentityKeySet,
    schemeInfoFromRefSubst,
    schemeInfoBinderRefSubst,
    ResolvedVar(..),
    ResolvedTermIdentityKey,
    idDetailsIdentityKey,
    resolvedVarIdentityKey,
    deferredResolvedVarFromRef,
    deferredResolvedVarRef,
    localResolvedVarFromRef,
    mkDeferredVarWithRef,
    mkLocalLamWithRef,
    mkLocalLetWithRef,
    mkLocalRecursiveLetWithRef,
    identityGeneratorAfterType,
    generatedIdentitiesInType,
    eTyAbsWithRef,
    identityGeneratorAfterTerm,
    generatedIdentitiesInTerm,
    resolvedVarName,
    resolvedVarReferenceName,
    resolvedVarConstructorRef,
    resolvedVarLocalRef,
    resolvedVarBindingSymbolIdentity,
    resolvedVarSymbolIdentity,
    resolvedVarIsLocal,
    resolvedVarIsEvidence,
    resolvedVarIsDiscard,
    resolvedVarSameIdentity,
    resolvedVarBoundBy,
    mapResolvedVarType,
    renameResolvedLocalVar,
    freshenResolvedLocalVar,
    renameResolvedDeferredVar,
    XmlfTerm(..),
    XmlfTermF
        ( EVarNodeF,
          ELitF,
          ELamF,
          EAppF,
          ELetF,
          ETyAbsFRef,
          ETyInstF,
          ERollF,
          EUnrollF
        ),
    Instantiation
        ( InstId,
          InstApp,
          InstBot,
          InstIntro,
          InstElim,
          InstInside,
          InstSeq,
          InstAbstrRef,
          InstUnderRef
        ),
    InstantiationF
        ( InstIdF,
          InstAppF,
          InstBotF,
          InstIntroF,
          InstElimF,
          InstAbstrFRef,
          InstUnderFRef,
          InstInsideF,
          InstSeqF
        ),
    ElabError(..),
    TypeCheckError(..),
    bindingToElab,
    Pretty(..),
    PrettyDisplay(..),
    ContextStep(StepUnderRef, StepInside),
    applyContext,
    schemeFromType,
    mapBoundType,
    selectMinPrecInsertionIndex,
) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types.Graph (BaseTy(..), NodeId(..), getNodeId)
import MLF.Util.ElabError (ElabError(..), bindingToElab)
import MLF.Types.Elab
import MLF.Types.Identity (UniqueIdentity(..))
import MLF.Reify.TypeOps (freeTypeVarRefsType, substTypeCaptureRef)
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
    pretty = pretty . schemeToTypeLocal

instance Pretty Instantiation where
    pretty = XMLFPretty.prettyXmlfComp . toXmlfComp

instance Pretty XmlfTerm where
    pretty = prettyTermCanonical

prettyTermCanonical :: XmlfTerm -> String
prettyTermCanonical = XMLFPretty.prettyXmlfTerm

toXmlfType :: ElabType -> XMLF.XmlfType
toXmlfType ty = case ty of
    TVarRef ref -> XMLF.XTVar (typeBinderRefName ref)
    TArrow a b -> XMLF.XTArrow (toXmlfType a) (toXmlfType b)
    TCon (BaseTy c) args -> XMLF.XTCon c (fmap toXmlfType args)
    TVarAppRef ref args -> XMLF.XTVarApp (typeBinderRefName ref) (fmap toXmlfType args)
    TBase (BaseTy b) -> XMLF.XTBase b
    TForallRef ref mb body ->
        let bound = maybe XMLF.XTBottom toXmlfBound mb
        in XMLF.XTForall (typeBinderRefName ref) bound (toXmlfType body)
    TMuRef ref body -> XMLF.XTMu (typeBinderRefName ref) (toXmlfType body)
    TBottom -> XMLF.XTBottom

toXmlfBound :: BoundType -> XMLF.XmlfType
toXmlfBound bound = case bound of
    TArrow a b -> XMLF.XTArrow (toXmlfType a) (toXmlfType b)
    TCon (BaseTy c) args -> XMLF.XTCon c (fmap toXmlfType args)
    TVarAppRef ref args -> XMLF.XTVarApp (typeBinderRefName ref) (fmap toXmlfType args)
    TBase (BaseTy b) -> XMLF.XTBase b
    TForallRef ref mb body ->
        let boundTy = maybe XMLF.XTBottom toXmlfBound mb
        in XMLF.XTForall (typeBinderRefName ref) boundTy (toXmlfType body)
    TMuRef ref body -> XMLF.XTMu (typeBinderRefName ref) (toXmlfType body)
    TBottom -> XMLF.XTBottom

toXmlfComp :: Instantiation -> XMLF.XmlfComp
toXmlfComp inst = case inst of
    InstId -> XMLF.XCId
    InstApp ty -> compFromType ty
    InstBot ty -> XMLF.XCBot (toXmlfType ty)
    InstIntro -> XMLF.XCIntro
    InstElim -> XMLF.XCElim
    InstAbstrRef ref -> XMLF.XCHyp (typeBinderRefName ref)
    InstUnderRef ref i -> XMLF.XCOuter (typeBinderRefName ref) (toXmlfComp i)
    InstInside i -> XMLF.XCInner (toXmlfComp i)
    InstSeq i1 i2 -> XMLF.XCSeq (toXmlfComp i1) (toXmlfComp i2)
  where
    compFromType ty =
        XMLF.XCSeq
            (XMLF.XCInner (XMLF.XCBot (toXmlfType ty)))
            XMLF.XCElim

data OccInfo = OccInfo
    { oiFreeVars :: Set.Set TypeBinderRef
    , oiOccMap :: Map.Map TypeBinderRef (Int, Int)
    }

-- | Display-only bound inlining for presentation (§8.3.1).
inlineBoundsForDisplay :: ElabType -> ElabType
inlineBoundsForDisplay = go
  where
    -- Conservative approximation: inline only single covariant occurrences with base/var bounds.
    go ty = case ty of
        TArrow d c -> TArrow (go d) (go c)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap go args)
        TVarAppRef ref args -> TVarAppRef ref (fmap go args)
        TForallRef ref mb body ->
            let mb' = fmap goBound mb
                body' = go body
            in simplifyForall ref mb' body'
        TMuRef ref body -> TMuRef ref (go body)
        TVarRef ref -> TVarRef ref
        TBaseWithIdentity identity b -> TBaseWithIdentity identity b
        TBottom -> TBottom

    simplifyForall ref mb body =
        case mb of
            Nothing ->
                if Set.member ref (freeVarsType body)
                    then TForallRef ref Nothing body
                    else body
            Just bound ->
                let boundTy = tyToElab bound
                    freeInBound = any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType bound)
                    (posCount, negCount) = occurrencesIn body
                    totalCount = posCount + negCount
                in if freeInBound
                    then TForallRef ref (Just bound) body
                    else if totalCount == 0
                        then body
                        else if inlineableBound boundTy
                            then go (substTypeCaptureRef ref boundTy body)
                            else TForallRef ref (Just bound) body
      where
        occurrencesIn = occurrencesVar ref

    inlineableBound :: ElabType -> Bool
    inlineableBound ty = case ty of
        TBase{} -> True
        TBottom -> True
        TVarRef{} -> True
        TArrow{} -> True
        TCon{} -> True
        TVarAppRef{} -> True
        _ -> False

    goBound :: BoundType -> BoundType
    goBound bound = case bound of
        TArrow a b -> TArrow (go a) (go b)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap go args)
        TVarAppRef ref args -> TVarAppRef ref (fmap go args)
        TBaseWithIdentity identity b -> TBaseWithIdentity identity b
        TBottom -> TBottom
        TForallRef ref mb body ->
            let mb' = fmap goBound mb
                body' = go body
            in TForallRef ref mb' body'
        TMuRef ref body -> TMuRef ref (go body)

    occurrencesVar :: TypeBinderRef -> ElabType -> (Int, Int)
    occurrencesVar ref = Map.findWithDefault (0, 0) ref . oiOccMap . occInfo

    freeVarsType :: ElabType -> Set.Set TypeBinderRef
    freeVarsType = oiFreeVars . occInfo

    emptyOccInfo :: OccInfo
    emptyOccInfo = OccInfo Set.empty Map.empty

    occInfo :: ElabType -> OccInfo
    occInfo = unK . paraIx occAlg
      where
        occAlg :: TyIF i (IxPair Ty (K OccInfo)) -> K OccInfo i
        occAlg ty = case ty of
            TVarIFRef ref ->
                K (OccInfo (Set.singleton ref) (Map.singleton ref (1, 0)))
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
            TVarAppIFRef ref args ->
                let occArg :: IxPair Ty (K OccInfo) 'AllowVar -> OccInfo
                    occArg ix = unK (snd (unIxPair ix))
                    occArgs = case args of
                        arg :| rest -> map occArg (arg : rest)
                    freeVars = Set.insert ref (Set.unions (map oiFreeVars occArgs))
                    occMaps = Map.singleton ref (1, 0) : map oiOccMap occArgs
                in K (OccInfo freeVars (foldr mergeOccMaps Map.empty occMaps))
            TBaseIF _ -> K emptyOccInfo
            TBottomIF -> K emptyOccInfo
            TForallIFRef ref mb body ->
                let occBody = unK (snd (unIxPair body))
                    occBound = maybe emptyOccInfo (occInfoBound . fst . unIxPair) mb
                    freeBound = oiFreeVars occBound
                    freeVars = Set.union freeBound (Set.delete ref (oiFreeVars occBody))
                    occBody' = Map.delete ref (oiOccMap occBody)
                    occBound' = Map.delete ref (oiOccMap occBound)
                in K (OccInfo freeVars (mergeOccMaps occBound' occBody'))
            TMuIFRef ref body ->
                let occBody = unK (snd (unIxPair body))
                    freeVars = Set.delete ref (oiFreeVars occBody)
                    occBody' = Map.delete ref (oiOccMap occBody)
                in K (OccInfo freeVars occBody')

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
        TVarAppRef ref args ->
            let occArgs = case args of
                    arg :| rest -> map occInfo (arg : rest)
                freeVars = Set.insert ref (Set.unions (map oiFreeVars occArgs))
                occMaps = Map.singleton ref (1, 0) : map oiOccMap occArgs
            in OccInfo freeVars (foldr mergeOccMaps Map.empty occMaps)
        TBase _ -> emptyOccInfo
        TBottom -> emptyOccInfo
        TForallRef ref mb body ->
            let occBody = occInfo body
                occBound = maybe emptyOccInfo occInfoBound mb
                freeBound = oiFreeVars occBound
                freeVars = Set.union freeBound (Set.delete ref (oiFreeVars occBody))
                occBody' = Map.delete ref (oiOccMap occBody)
                occBound' = Map.delete ref (oiOccMap occBound)
            in OccInfo freeVars (mergeOccMaps occBound' occBody')
        TMuRef ref body ->
            let occBody = occInfo body
                freeVars = Set.delete ref (oiFreeVars occBody)
                occBody' = Map.delete ref (oiOccMap occBody)
            in OccInfo freeVars occBody'

-- | Pretty-printing with display-only bound inlining.
instance PrettyDisplay ElabType where
    prettyDisplay = pretty . inlineBoundsForDisplay

instance PrettyDisplay ElabScheme where
    prettyDisplay = prettyDisplay . inlineBoundsForDisplay . schemeToTypeLocal

instance PrettyDisplay Instantiation where
    prettyDisplay = pretty

instance PrettyDisplay XmlfTerm where
    prettyDisplay = pretty

schemeToTypeLocal :: ElabScheme -> ElabType
schemeToTypeLocal scheme =
    foldr
        (\(ref, mbBound) body -> tForallWithRef ref mbBound body)
        (schemeBody scheme)
        (schemeBinderRefs scheme)

mapBoundType :: (ElabType -> ElabType) -> BoundType -> BoundType
mapBoundType f bound = case bound of
    TArrow a b -> TArrow (f a) (f b)
    TConWithIdentity identity c args -> TConWithIdentity identity c (fmap f args)
    TVarAppRef ref args -> TVarAppRef ref (fmap f args)
    TBaseWithIdentity identity b -> TBaseWithIdentity identity b
    TBottom -> TBottom
    TForallRef ref mb body ->
        let mb' = fmap (mapBoundType f) mb
        in TForallRef ref mb' (f body)
    TMuRef ref body -> TMuRef ref (f body)

schemeFromType :: ElabType -> ElabScheme
schemeFromType ty =
    let (binds0, body0) = splitForallRefs ty
        binderRefs = map fst binds0
        bodyFvRefs = freeTypeVarRefsType body0
        externalFvs =
            [ ref
            | ref <- bodyFvRefs
            , not (any (typeBinderRefsSameIdentity ref) binderRefs)
            ]
        unusedBinders =
            [ ref
            | (ref, _) <- binds0
            , not (any (typeBinderRefsSameIdentity ref) bodyFvRefs)
            ]
        ty' =
            if length externalFvs <= length unusedBinders && not (null externalFvs)
                then
                    foldl
                        (\acc (fromRef, toRef) -> substTypeCaptureRef fromRef (TVarRef toRef) acc)
                        ty
                        (zip externalFvs unusedBinders)
                else ty
        (binds, body) = splitForallRefs ty'
    in mkElabSchemeWithRefs binds body

splitForallRefs :: ElabType -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
splitForallRefs = go
  where
    go ty = case ty of
        TForallRef ref mb body ->
            let (binds, body') = go body
            in ((ref, mb) : binds, body')
        _ -> ([], ty)

data TypeCheckError
    = TCUnboundVar String
    | TCExpectedArrow ElabType
    | TCExpectedRecursive ElabType
    | TCNonContractiveRecursiveType ElabType
    | TCArgumentMismatch ElabType ElabType
    | TCRollBodyMismatch ElabType ElabType
    | TCInstantiationError Instantiation ElabType String
    | TCTypeAbsVarInScope String
    | TCTypeAbsBoundMentionsVar String
    | TCUnboundTypeVar String
    | TCResolvedVarTypeMismatch String ElabType ElabType
    | TCLetTypeMismatch ElabType ElabType
    deriving (Eq, Show)

-- | Context steps for reaching a node in the type structure.
--
-- Paper reference: computation/instantiation contexts (Ch. 15.3, Fig. 10).
-- A context is a sequence of steps:
--   - StepUnder: go under a quantifier (∀(α ⩾) ·)
--   - StepInside: go inside a bound (∀(⩾ ·))
data ContextStep
    = StepUnderRef TypeBinderRef -- ^ Go under quantifier with given binder ref
    | StepInside                 -- ^ Go inside the bound of a quantifier
    deriving (Show)

instance Eq ContextStep where
    StepInside == StepInside = True
    StepUnderRef left == StepUnderRef right = typeBinderRefsSameIdentity left right
    _ == _ = False

-- | Apply a paper-style instantiation context to an instantiation.
--
-- This encodes Figure 10 contexts:
--   C ::= {·} | ∀(⩾ C) | ∀(α ⩾) C
applyContext :: [ContextStep] -> Instantiation -> Instantiation
applyContext steps inner = foldr step inner steps
  where
    step cs inst = case cs of
        StepUnderRef ref -> instUnderWithRef ref inst
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
