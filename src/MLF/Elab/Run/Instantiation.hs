{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module MLF.Elab.Run.Instantiation (
    inferInstAppArgsFromSchemeRefs,
    varRefsInType,
    substTypeSelectiveRefs,
    instInsideFromArgsWithBoundsRefs,
    containsForallType
) where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map

import Data.List (find)

import MLF.Reify.TypeOps (alphaEqType, composeTypeHeadRef, matchTypeRefs, stripForallsType)
import MLF.Elab.Types

newtype SubstFun (i :: TopVar) =
    SubstFun { runSubstFun :: [TypeBinderRef] -> Ty i }

inferInstAppArgsFromSchemeRefs :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ElabType -> Maybe [ElabType]
inferInstAppArgsFromSchemeRefs binds body targetTy =
    let binderRefs = map fst binds
        targetCore = stripForallsType targetTy
        targetForallRefs =
            let alg ty = case ty of
                    TForallIFRef ref _ body' -> ref : unK body'
                    TConIFWithIdentity _ _ args -> concatMap unK args
                    _ -> []
            in cataIxConst alg targetTy
        argsAreIdentity :: [TypeBinderRef] -> [ElabType] -> Bool
        argsAreIdentity refs args =
            and
                [ case arg of
                    TVarRef argRef ->
                        typeBinderRefsSameIdentity argRef ref
                            || any (typeBinderRefsSameIdentity argRef) targetForallRefs
                    _ -> False
                | (ref, arg) <- zip refs args
                ]
        inferFromBody =
            let fromMatch =
                    case matchTypeRefs binderRefs body targetCore of
                        Left _ -> Nothing
                        Right subst ->
                            let present = map (`Map.member` subst) binderRefs
                                prefixLen = length (takeWhile id present)
                                hasOutOfOrder = or (drop prefixLen present)
                                prefixRefs = take prefixLen binderRefs
                                args = [ty | ref <- prefixRefs, Just ty <- [Map.lookup ref subst]]
                            in if hasOutOfOrder
                                then Nothing
                                else if argsAreIdentity prefixRefs args
                                    then Nothing
                                    else Just args
                fromArrowPrefix =
                    let bindDomain substAcc bodyDom targetDom =
                            case bodyDom of
                                TVarRef ref
                                    | Just binderRef <- matchBinderRef ref ->
                                        case Map.lookup binderRef substAcc of
                                            Nothing -> Just (Map.insert binderRef targetDom substAcc)
                                            Just prev
                                                | alphaEqType prev targetDom -> Just substAcc
                                                | otherwise -> Nothing
                                _
                                    | alphaEqType bodyDom targetDom -> Just substAcc
                                    | otherwise -> Nothing
                        go substAcc bodyTy targetTy' =
                            case (bodyTy, targetTy') of
                                (TArrow bodyDom bodyCod, TArrow targetDom targetCod) -> do
                                    substNext <- bindDomain substAcc bodyDom targetDom
                                    go substNext bodyCod targetCod
                                _ -> Just substAcc
                    in do
                        subst <- go Map.empty body targetCore
                        let present = map (`Map.member` subst) binderRefs
                            prefixLen = length (takeWhile id present)
                            hasOutOfOrder = or (drop prefixLen present)
                            prefixRefs = take prefixLen binderRefs
                            args = [ty | ref <- prefixRefs, Just ty <- [Map.lookup ref subst]]
                        if hasOutOfOrder
                            then Nothing
                            else if argsAreIdentity prefixRefs args
                                then Nothing
                                else Just args
            in fromMatch <|> fromArrowPrefix
        inferFromBound binderRef bound =
            let boundCore = stripForallsType bound
                matchVars = map canonicalSchemeRef (varRefsInType boundCore)
                canonicalSchemeRef ref =
                    case find (typeBinderRefsSameIdentity ref) binderRefs of
                        Just binderRef' -> binderRef'
                        Nothing -> ref
            in case matchTypeRefs matchVars boundCore targetCore of
                Left _ -> Nothing
                Right subst ->
                    let innerVars =
                            filter
                                (\ref -> not (any (typeBinderRefsSameIdentity ref) binderRefs))
                                matchVars
                        pickInnerArg =
                            case innerVars of
                                [inner] -> Map.lookup inner subst
                                _ -> Nothing
                        argFor ref =
                            if typeBinderRefsSameIdentity ref binderRef
                                then
                                    case pickInnerArg of
                                        Just innerArg -> Just innerArg
                                        Nothing -> Just (substTypeSelectiveRefs binderRefs subst boundCore)
                                else Map.lookup ref subst
                        argsMaybe = map argFor binderRefs
                        present = map (\arg -> case arg of { Just _ -> True; Nothing -> False }) argsMaybe
                        prefixLen = length (takeWhile id present)
                        hasOutOfOrder = or (drop prefixLen present)
                        prefixArgs = take prefixLen argsMaybe
                        args = [ty | Just ty <- prefixArgs]
                        prefixRefs = take prefixLen binderRefs
                    in if hasOutOfOrder
                        then Nothing
                        else if argsAreIdentity prefixRefs args
                            then Nothing
                            else Just args
    in case body of
        TVarRef ref ->
            case find (typeBinderRefsSameIdentity ref . fst) binds of
                Just (binderRef, Just bound) -> inferFromBound binderRef (tyToElab bound)
                Just (_, Nothing) -> inferFromBody
                Nothing -> Nothing
        _ -> inferFromBody
  where
    matchBinderRef ref =
        find (typeBinderRefsSameIdentity ref) (map fst binds)

varRefsInType :: ElabType -> [TypeBinderRef]
varRefsInType = cataIxConst alg
  where
    alg :: TyIF i (K [TypeBinderRef]) -> [TypeBinderRef]
    alg ty = case ty of
        TVarIFRef ref -> [ref]
        TArrowIF a b -> dedupeRefs (unK a ++ unK b)
        TConIFWithIdentity _ _ args -> dedupeRefs (concatMap unK args)
        TVarAppIFRef ref args -> dedupeRefs (ref : concatMap unK args)
        TBaseIFWithIdentity _ _ -> []
        TBottomIF -> []
        TForallIFRef _ mb body ->
            let varsBound = maybe [] unK mb
            in dedupeRefs (varsBound ++ unK body)
        TMuIFRef _ body -> unK body

dedupeRefs :: [TypeBinderRef] -> [TypeBinderRef]
dedupeRefs =
    foldr insertRef []
  where
    insertRef ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

substTypeSelectiveRefs :: [TypeBinderRef] -> Map.Map TypeBinderRef ElabType -> ElabType -> ElabType
substTypeSelectiveRefs binderRefs subst ty0 = runSubstFun (cataIx alg ty0) []
  where
    alg :: TyIF i SubstFun -> SubstFun i
    alg ty = case ty of
        TVarIFRef ref ->
            SubstFun $ \bound ->
                if refMember ref bound || refMember ref binderRefs
                    then TVarRef ref
                    else case lookupRef ref subst of
                        Just ty' -> ty'
                        Nothing -> TVarRef ref
        TArrowIF a b ->
            SubstFun $ \bound -> TArrow (runSubstFun a bound) (runSubstFun b bound)
        TConIFWithIdentity identity c args ->
            SubstFun $ \bound -> TConWithIdentity identity c (fmap (\f -> runSubstFun f bound) args)
        TVarAppIFRef ref args ->
            SubstFun $ \bound ->
                let args' = fmap (\f -> runSubstFun f bound) args
                in if refMember ref bound || refMember ref binderRefs
                    then TVarAppRef ref args'
                    else case lookupRef ref subst of
                        Just ty' -> composeTypeHeadRef ref ty' args'
                        Nothing -> TVarAppRef ref args'
        TBaseIFWithIdentity identity b -> SubstFun (const (TBaseWithIdentity identity b))
        TBottomIF -> SubstFun (const TBottom)
        TForallIFRef ref mb body ->
            SubstFun $ \bound ->
                let bound' = insertRef ref bound
                    mb' = fmap (\f -> runSubstFun f bound') mb
                    body' = runSubstFun body bound'
                in TForallRef ref mb' body'
        TMuIFRef ref body ->
            SubstFun $ \bound ->
                let bound' = insertRef ref bound
                in TMuRef ref (runSubstFun body bound')

    insertRef ref refs
        | refMember ref refs = refs
        | otherwise = ref : refs

    refMember ref =
        any (typeBinderRefsSameIdentity ref)

    lookupRef ref substMap =
        snd <$> find (typeBinderRefsSameIdentity ref . fst) (Map.toList substMap)

instInsideFromArgsWithBoundsRefs :: [(TypeBinderRef, Maybe BoundType)] -> [ElabType] -> Maybe Instantiation
instInsideFromArgsWithBoundsRefs binds args = go binds args
  where
    go [] _ = Just InstId
    go _ [] = Just InstId
    go ((ref, mbBound):ns) (t:ts) = do
        rest <- go ns ts
        inst <- instFor mbBound t
        pure $ case (inst, rest) of
            (InstId, InstId) -> InstId
            (InstId, _) -> instUnderWithRef ref rest
            (_, InstId) -> inst
            _ -> InstSeq inst (instUnderWithRef ref rest)

    instFor :: Maybe BoundType -> ElabType -> Maybe Instantiation
    instFor mbBound t = case mbBound of
        Nothing -> Just (InstInside (InstBot t))
        Just bound
            | containsForallTy bound -> Just (InstInside (InstApp t))
            | alphaEqType boundTy TBottom -> Just (InstInside (InstBot t))
            | alphaEqType boundTy t -> Just InstId
            | otherwise -> Nothing
          where
            boundTy = tyToElab bound

containsForallType :: ElabType -> Bool
containsForallType = cataIxConst alg
  where
    alg ty = case ty of
        TForallIFRef _ _ _ -> True
        TMuIFRef _ body -> unK body
        TArrowIF a b -> unK a || unK b
        TConIFWithIdentity _ _ args -> any unK args
        _ -> False
