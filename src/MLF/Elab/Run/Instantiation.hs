{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module MLF.Elab.Run.Instantiation (
    inferInstAppArgsFromSchemeRefs,
    inferInstAppArgsFromSchemeRefsExact,
    resolvedSourceApplicationArgumentEndpoint,
    sourceSchemeConstructsExactEndpoint,
    residualTopologyAgreesExact,
    varRefsInType,
    substTypeSelectiveRefs,
    instInsideFromArgsWithBoundsRefs,
    containsForallType
) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard)
import qualified Data.Map.Strict as Map

import Data.List (find)

import MLF.Elab.Inst (applyInstantiation, schemeToType)
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
import MLF.Reify.TypeOps
    ( alphaEqType
    , churchAwareEqType
    , composeTypeHeadRef
    , freeTypeVarRefsType
    , matchChurchAwareTypeRefs
    , matchTypeRefs
    , stripForallsType
    )
newtype SubstFun (i :: TopVar) =
    SubstFun { runSubstFun :: [TypeBinderRef] -> Ty i }

inferInstAppArgsFromSchemeRefs :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ElabType -> Maybe [ElabType]
inferInstAppArgsFromSchemeRefs binds body targetTy =
    inferInstAppArgsFromSchemeRefsWith
        matchTypeRefs
        alphaEqType
        (stripForallsType targetTy)
        binds
        body
        targetTy

-- | Infer source-scheme applications for an explicit xMLF endpoint.  Unlike
-- the compatibility inference above, this preserves a leading forall in the
-- target: first-class polymorphic arguments such as @id id@ instantiate the
-- function at @forall a. a -> a@, not at that scheme's stripped body.
inferInstAppArgsFromSchemeRefsExact :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ElabType -> Maybe [ElabType]
inferInstAppArgsFromSchemeRefsExact binds body targetTy =
    inferInstAppArgsFromSchemeRefsWith
        matchChurchAwareTypeRefs
        exactTypesAgree
        targetTy
        binds
        body
        targetTy
  where
    exactTypesAgree left right =
        alphaEqType left right || churchAwareEqType left right

-- | Select the exact argument endpoint for an identity-resolved source
-- application.  The function's source domain must already be closed: if it
-- still depends on one of the function scheme's binders, as in the paper's
-- @g g@ construction, root preparation must not invent a specialization
-- before the application constructor has established it.
resolvedSourceApplicationArgumentEndpoint
    :: TypeCheck.Env
    -> SchemeInfo
    -> SchemeInfo
    -> Maybe ElabType
resolvedSourceApplicationArgumentEndpoint typeEnv functionSchemeInfo argumentSchemeInfo = do
    sourceDomain <-
        case schemeBody (siScheme functionSchemeInfo) of
            TArrow domain _ -> Just domain
            _ -> Nothing
    guard (null (freeTypeVarRefsType sourceDomain))
    sourceSchemeConstructsExactEndpoint
        typeEnv
        sourceDomain
        argumentSchemeInfo

-- | Prove that one identity-resolved source occurrence constructs an exact
-- endpoint. The complete source forall spine must be consumed, and every
-- type application is checked before the endpoint is published.
sourceSchemeConstructsExactEndpoint
    :: TypeCheck.Env
    -> ElabType
    -> SchemeInfo
    -> Maybe ElabType
sourceSchemeConstructsExactEndpoint typeEnv endpoint schemeInfo = do
    let sourceScheme = siScheme schemeInfo
        sourceTy = schemeToType sourceScheme
        sourceBinders = schemeBinderRefs sourceScheme
    arguments <-
        inferInstAppArgsFromSchemeRefsExact
            sourceBinders
            (schemeBody sourceScheme)
            endpoint
    guard (length arguments == length sourceBinders)
    appliedTy <- foldM applySourceArgument sourceTy arguments
    guard (alphaEqType appliedTy endpoint || churchAwareEqType appliedTy endpoint)
    pure endpoint
  where
    applySourceArgument currentTy argumentTy =
        let instantiation =
                case currentTy of
                    TForallRef _ (Just bound) _
                        | alphaEqType argumentTy (tyToElab bound)
                            || churchAwareEqType argumentTy (tyToElab bound) ->
                            InstElim
                    _ -> InstApp argumentTy
        in either
            (const Nothing)
            Just
            (TypeCheck.checkInstantiation typeEnv currentTy instantiation)

-- | Compare a fully specialized residual function with the application
-- topology that justified its arguments.  A graph endpoint may retain a
-- leading bounded forall where the constructed endpoint has already selected
-- that bound, so equality may eliminate those explicit bounds at any residual
-- position.  Other arrow structure must agree recursively; matching only the
-- terminal result is deliberately insufficient.
residualTopologyAgreesExact :: ElabType -> ElabType -> Bool
residualTopologyAgreesExact left right
    | alphaEqType left right || churchAwareEqType left right = True
    | otherwise =
        case (left, right) of
            (TArrow leftDomain leftCodomain, TArrow rightDomain rightCodomain) ->
                residualTopologyAgreesExact leftDomain rightDomain
                    && residualTopologyAgreesExact leftCodomain rightCodomain
            (TForallRef{}, _) ->
                case applyInstantiation left InstElim of
                    Right left' -> residualTopologyAgreesExact left' right
                    Left _ -> False
            (_, TForallRef{}) ->
                case applyInstantiation right InstElim of
                    Right right' -> residualTopologyAgreesExact left right'
                    Left _ -> False
            _ -> False

inferInstAppArgsFromSchemeRefsWith
    :: ([TypeBinderRef] -> ElabType -> ElabType -> Either ElabError (Map.Map TypeBinderRef ElabType))
    -> (ElabType -> ElabType -> Bool)
    -> ElabType
    -> [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> ElabType
    -> Maybe [ElabType]
inferInstAppArgsFromSchemeRefsWith matchRefs typesAgree targetCore binds body targetTy =
    let binderRefs = map fst binds
        targetForallRefs =
            let alg ty = case ty of
                    TForallIFRef ref _ body' -> ref : unK body'
                    TConIFWithIdentity _ _ args -> concatMap unK args
                    _ -> []
            in cataIxConst alg targetTy
        argsAreIdentity :: [ElabType] -> Bool
        argsAreIdentity args =
            and
                [ case arg of
                    TVarRef argRef ->
                        any (typeBinderRefsSameIdentity argRef) targetForallRefs
                    _ -> False
                | arg <- args
                ]
        inferFromBody =
            let fromMatch =
                    case matchRefs binderRefs body targetCore of
                        Left _ -> Nothing
                        Right subst ->
                            let present = map (`Map.member` subst) binderRefs
                                prefixLen = length (takeWhile id present)
                                hasOutOfOrder = or (drop prefixLen present)
                                prefixRefs = take prefixLen binderRefs
                                args = [ty | ref <- prefixRefs, Just ty <- [Map.lookup ref subst]]
                            in if hasOutOfOrder
                                then Nothing
                                else if argsAreIdentity args
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
                                                | typesAgree prev targetDom -> Just substAcc
                                                | otherwise -> Nothing
                                _
                                    | typesAgree bodyDom targetDom -> Just substAcc
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
                            else if argsAreIdentity args
                                then Nothing
                                else Just args
            in fromMatch <|> fromArrowPrefix
        inferDefaultEliminations =
            -- A quantified variable may be absent from the body, so matching
            -- the body cannot infer an argument for it.  In that case the
            -- paper's N computation is canonical: eliminate each leading
            -- quantifier at its current lower bound, and accept the arguments
            -- only when the constructed computation reaches the exact
            -- semantic endpoint.
            let sourceTy =
                    foldr
                        (\(ref, mbBound) rest -> tForallWithRef ref mbBound rest)
                        body
                        binds
                go current remaining acc =
                    case remaining of
                        []
                            | alphaEqType current targetTy
                                || churchAwareEqType current targetTy -> Just (reverse acc)
                            | otherwise -> Nothing
                        _ : rest ->
                            case current of
                                TForallRef _ mbBound _ -> do
                                    current' <- either (const Nothing) Just (applyInstantiation current InstElim)
                                    let argument = maybe TBottom tyToElab mbBound
                                    go current' rest (argument : acc)
                                _ -> Nothing
            in go sourceTy binds []
        inferFromBound binderRef bound =
            let boundCore = stripForallsType bound
                matchVars = map canonicalSchemeRef (varRefsInType boundCore)
                canonicalSchemeRef ref =
                    case find (typeBinderRefsSameIdentity ref) binderRefs of
                        Just binderRef' -> binderRef'
                        Nothing -> ref
            in case matchRefs matchVars boundCore targetCore of
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
                    in if hasOutOfOrder
                        then Nothing
                        else if argsAreIdentity args
                            then Nothing
                            else Just args
    in case body of
        TVarRef ref ->
            case find (typeBinderRefsSameIdentity ref . fst) binds of
                Just (binderRef, Just bound) ->
                    inferFromBound binderRef (tyToElab bound)
                        <|> inferDefaultEliminations
                Just (_, Nothing) -> inferFromBody <|> inferDefaultEliminations
                Nothing -> Nothing
        _ -> inferFromBody <|> inferDefaultEliminations
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
