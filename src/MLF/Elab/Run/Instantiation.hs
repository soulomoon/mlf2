module MLF.Elab.Run.Instantiation (
    inferInstAppArgsFromScheme,
    varsInType,
    substTypeSelective,
    instInsideFromArgsWithBounds,
    containsForallType
) where

import Data.Functor.Foldable (cata)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Reify.TypeOps (matchType, stripForallsType)
import MLF.Elab.Types

inferInstAppArgsFromScheme :: [(String, Maybe ElabType)] -> ElabType -> ElabType -> Maybe [ElabType]
inferInstAppArgsFromScheme binds body targetTy =
    let binderNames = map fst binds
        binderSet = Set.fromList binderNames
        targetCore = stripForallsType targetTy
        targetForallNames =
            let alg ty = case ty of
                    TForallF v _ body' -> v : body'
                    _ -> []
            in cata alg targetTy
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
                Just (Just bound) -> inferFromBound v bound
                _ -> fallback
        _ -> fallback

varsInType :: ElabType -> Set.Set String
varsInType = cata alg
  where
    alg ty = case ty of
        TVarF v -> Set.singleton v
        TArrowF a b -> Set.union a b
        TBaseF _ -> Set.empty
        TBottomF -> Set.empty
        TForallF _ mb body ->
            let varsBound = maybe Set.empty id mb
            in Set.union varsBound body

substTypeSelective :: Set.Set String -> Map.Map String ElabType -> ElabType -> ElabType
substTypeSelective binderSet subst ty0 = (cata alg ty0) Set.empty
  where
    alg ty = case ty of
        TVarF v ->
            \bound ->
                if Set.member v bound || Set.member v binderSet
                    then TVar v
                    else case Map.lookup v subst of
                        Just ty' -> ty'
                        Nothing -> TVar v
        TArrowF a b -> \bound -> TArrow (a bound) (b bound)
        TBaseF b -> const (TBase b)
        TBottomF -> const TBottom
        TForallF v mb body ->
            \bound ->
                let bound' = Set.insert v bound
                    mb' = fmap ($ bound') mb
                    body' = body bound'
                in TForall v mb' body'

instInsideFromArgsWithBounds :: [(String, Maybe ElabType)] -> [ElabType] -> Instantiation
instInsideFromArgsWithBounds binds args = case (binds, args) of
    ([], _) -> InstId
    (_, []) -> InstId
    ((n, mbBound):ns, t:ts) ->
        let rest = instInsideFromArgsWithBounds ns ts
            inst =
                case mbBound of
                    Just bound | containsForallType bound -> InstInside (InstApp t)
                    _ -> InstInside (InstBot t)
        in if rest == InstId
            then inst
            else InstSeq inst (InstUnder n rest)

containsForallType :: ElabType -> Bool
containsForallType = cata alg
  where
    alg ty = case ty of
        TForallF _ _ _ -> True
        TArrowF a b -> a || b
        _ -> False
