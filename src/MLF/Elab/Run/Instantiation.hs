{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module MLF.Elab.Run.Instantiation (
    inferInstAppArgsFromScheme,
    varsInType,
    substTypeSelective,
    instInsideFromArgsWithBounds,
    containsForallType
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Reify.TypeOps (alphaEqType, matchType, stripForallsType)
import MLF.Elab.Types

newtype SubstFun (i :: TopVar) =
    SubstFun { runSubstFun :: Set.Set String -> Ty i }

inferInstAppArgsFromScheme :: [(String, Maybe BoundType)] -> ElabType -> ElabType -> Maybe [ElabType]
inferInstAppArgsFromScheme binds body targetTy =
    let binderNames = map fst binds
        binderSet = Set.fromList binderNames
        targetCore = stripForallsType targetTy
        targetForallNames =
            let alg ty = case ty of
                    TForallIF v _ body' -> v : unK body'
                    TConIF _ args -> concatMap unK args
                    _ -> []
            in cataIxConst alg targetTy
        argsAreIdentity :: [String] -> [ElabType] -> Bool
        argsAreIdentity names args =
            and
                [ case arg of
                    TVar v -> v == name || elem v targetForallNames
                    _ -> False
                | (name, arg) <- zip names args
                ]
        fallback =
            case matchType binderSet body targetCore of
                Left _ -> Nothing
                Right subst ->
                    let present = map (\name -> Map.member name subst) binderNames
                        prefixLen = length (takeWhile id present)
                        hasOutOfOrder = or (drop prefixLen present)
                        prefixNames = take prefixLen binderNames
                        args = [ty | name <- prefixNames, Just ty <- [Map.lookup name subst]]
                    in if hasOutOfOrder
                        then Nothing
                        else if argsAreIdentity prefixNames args
                            then Nothing
                            else Just args
        inferFromBound v bound =
            let boundCore = stripForallsType bound
                matchVars = varsInType boundCore
            in case matchType matchVars boundCore targetCore of
                Left _ -> fallback
                Right subst ->
                    let innerVars = Set.difference matchVars binderSet
                        pickInnerArg =
                            case Set.toList innerVars of
                                [inner] -> Map.lookup inner subst
                                _ -> Nothing
                        argFor name =
                            if name == v
                                then
                                    case pickInnerArg of
                                        Just innerArg -> Just innerArg
                                        Nothing -> Just (substTypeSelective binderSet subst boundCore)
                                else Map.lookup name subst
                        argsMaybe = map argFor binderNames
                        present = map (\arg -> case arg of { Just _ -> True; Nothing -> False }) argsMaybe
                        prefixLen = length (takeWhile id present)
                        hasOutOfOrder = or (drop prefixLen present)
                        prefixArgs = take prefixLen argsMaybe
                        args = [ty | Just ty <- prefixArgs]
                        prefixNames = take prefixLen binderNames
                    in if hasOutOfOrder
                        then Nothing
                        else if argsAreIdentity prefixNames args
                            then Nothing
                            else Just args
    in case body of
        TVar v ->
            case lookup v binds of
                Just (Just bound) -> inferFromBound v (tyToElab bound)
                _ -> fallback
        _ -> fallback

varsInType :: ElabType -> Set.Set String
varsInType = cataIxConst alg
  where
    alg :: TyIF i (K (Set.Set String)) -> Set.Set String
    alg ty = case ty of
        TVarIF v -> Set.singleton v
        TArrowIF a b -> Set.union (unK a) (unK b)
        TConIF _ args -> foldr (Set.union . unK) Set.empty args
        TBaseIF _ -> Set.empty
        TBottomIF -> Set.empty
        TForallIF _ mb body ->
            let varsBound = maybe Set.empty unK mb
            in Set.union varsBound (unK body)

substTypeSelective :: Set.Set String -> Map.Map String ElabType -> ElabType -> ElabType
substTypeSelective binderSet subst ty0 = runSubstFun (cataIx alg ty0) Set.empty
  where
    alg :: TyIF i SubstFun -> SubstFun i
    alg ty = case ty of
        TVarIF v ->
            SubstFun $ \bound ->
                if Set.member v bound || Set.member v binderSet
                    then TVar v
                    else case Map.lookup v subst of
                        Just ty' -> ty'
                        Nothing -> TVar v
        TArrowIF a b ->
            SubstFun $ \bound -> TArrow (runSubstFun a bound) (runSubstFun b bound)
        TConIF c args ->
            SubstFun $ \bound -> TCon c (fmap (\f -> runSubstFun f bound) args)
        TBaseIF b -> SubstFun (const (TBase b))
        TBottomIF -> SubstFun (const TBottom)
        TForallIF v mb body ->
            SubstFun $ \bound ->
                let bound' = Set.insert v bound
                    mb' = fmap (\f -> runSubstFun f bound') mb
                    body' = runSubstFun body bound'
                in TForall v mb' body'

instInsideFromArgsWithBounds :: [(String, Maybe BoundType)] -> [ElabType] -> Maybe Instantiation
instInsideFromArgsWithBounds binds args = go binds args
  where
    go [] _ = Just InstId
    go _ [] = Just InstId
    go ((n, mbBound):ns) (t:ts) = do
        rest <- go ns ts
        inst <- instFor mbBound t
        pure $ case (inst, rest) of
            (InstId, InstId) -> InstId
            (InstId, _) -> InstUnder n rest
            (_, InstId) -> inst
            _ -> InstSeq inst (InstUnder n rest)

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
        TForallIF _ _ _ -> True
        TArrowIF a b -> unK a || unK b
        TConIF _ args -> any unK args
        _ -> False
