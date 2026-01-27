{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module MLF.Constraint.Presolution.Plan.Finalize (
    FinalizeInput(..),
    finalizeScheme
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.BindingUtil
    ( bindingPathToRootLocal
    , firstGenAncestorFrom
    )
import MLF.Constraint.Presolution.Plan.Context (GaBindParents(..), GeneralizeEnv, traceGeneralize)
import MLF.Constraint.Presolution.Plan.Names (alphaName, parseNameId)
import MLF.Constraint.Presolution.Plan.Normalize
    ( simplifySchemeBindings
    , promoteArrowAlias
    , isBaseBound
    , isVarBound
    )
import MLF.Reify.TypeOps (freeTypeVarsFrom, freeTypeVarsType, stripForallsType)
import MLF.Types.Elab
    ( BoundType
    , ElabScheme
    , ElabType
    , Ty(..)
    , TyIF(..)
    , cataIx
    , mkElabScheme
    , tyToElab
    )
import MLF.Util.ElabError (ElabError(..))

mapBound :: (ElabType -> ElabType) -> BoundType -> BoundType
mapBound f bound = case bound of
    TArrow a b -> TArrow (f a) (f b)
    TBase b -> TBase b
    TBottom -> TBottom
    TForall v mb body ->
        let mb' = fmap (mapBound f) mb
        in TForall v mb' (f body)

-- | Inputs needed to finalize a generalized scheme.
data FinalizeInput = FinalizeInput
    { fiEnv :: GeneralizeEnv
    , fiConstraint :: Constraint
    , fiCanonical :: NodeId -> NodeId
    , fiBindParents :: BindParents
    , fiScopeRootC :: NodeRef
    , fiTypeRoot :: NodeId
    , fiTypeRootC :: NodeId
    , fiScopeGen :: Maybe GenNodeId
    , fiFirstGenAncestorGa :: NodeRef -> Maybe GenNodeId
    , fiBindParentsGa :: Maybe GaBindParents
    , fiSolvedToBasePref :: IntMap.IntMap NodeId
    , fiGammaAlias :: IntMap.IntMap Int
    , fiNamedUnderGaSet :: IntSet.IntSet
    , fiOrderedBinders :: [Int]
    , fiBinderNames :: [String]
    , fiBindings :: [(String, Maybe BoundType)]
    , fiSubst :: IntMap.IntMap String
    , fiTyRaw :: ElabType
    }

finalizeScheme :: FinalizeInput -> Either ElabError (ElabScheme, IntMap.IntMap String)
finalizeScheme FinalizeInput{..} =
    let env = fiEnv
        constraint = fiConstraint
        canonical = fiCanonical
        bindParents = fiBindParents
        scopeRootC = fiScopeRootC
        typeRoot = fiTypeRoot
        typeRootC = fiTypeRootC
        scopeGen = fiScopeGen
        firstGenAncestorGa = fiFirstGenAncestorGa
        mbBindParentsGa = fiBindParentsGa
        solvedToBasePrefPlan = fiSolvedToBasePref
        gammaAliasPlan = fiGammaAlias
        namedUnderGaSetPlan = fiNamedUnderGaSet
        orderedBinders = fiOrderedBinders
        binderNames = fiBinderNames
        bindings = fiBindings
        subst = fiSubst
        ty0Raw = fiTyRaw
        aliasToTypeRootNames =
            [ name
            | (nidInt, name) <- zip orderedBinders binderNames
            , let nid = NodeId nidInt
            , Just bnd <- [VarStore.lookupVarBound constraint (canonical nid)]
            , canonical bnd == canonical typeRoot
            ]
        inlineAliasBinder :: ElabType -> [(String, Maybe BoundType)] -> (ElabType, [(String, Maybe BoundType)])
        inlineAliasBinder ty binds = case ty of
            TVar v
                | v `elem` aliasToTypeRootNames ->
                    case lookup v binds of
                        Just (Just bnd)
                            | not (isVarBound bnd)
                            , not (isBaseBound bnd) ->
                                (tyToElab bnd, filter (\(n, _) -> n /= v) binds)
                        _ -> (ty, binds)
            _ -> (ty, binds)
        (ty0RawAlias, bindingsAlias) = inlineAliasBinder ty0Raw bindings
        canonAllVars ty =
            let (ty', _freeEnv, _n) = go [] [] (0 :: Int) ty
            in ty'
          where
            go boundEnv freeEnv n tyInput = case tyInput of
                TVar v ->
                    case lookup v boundEnv of
                        Just v' -> (TVar v', freeEnv, n)
                        Nothing ->
                            case lookup v freeEnv of
                                Just v' -> (TVar v', freeEnv, n)
                                Nothing ->
                                    let v' = "v" ++ show n
                                    in (TVar v', (v, v') : freeEnv, n + 1)
                TBase b -> (TBase b, freeEnv, n)
                TBottom -> (TBottom, freeEnv, n)
                TArrow a b ->
                    let (a', free1, n1) = go boundEnv freeEnv n a
                        (b', free2, n2) = go boundEnv free1 n1 b
                    in (TArrow a' b', free2, n2)
                TForall v mb body ->
                    let v' = "v" ++ show n
                        n1 = n + 1
                        (mb', free1, n2) =
                            case mb of
                                Nothing -> (Nothing, freeEnv, n1)
                                Just bnd ->
                                    let (bnd', free', n') = goBound boundEnv freeEnv n1 bnd
                                    in (Just bnd', free', n')
                        (body', free2, n3) = go ((v, v') : boundEnv) free1 n2 body
                    in (TForall v' mb' body', free2, n3)

            goBound boundEnv freeEnv n bound = case bound of
                TArrow a b ->
                    let (a', free1, n1) = go boundEnv freeEnv n a
                        (b', free2, n2) = go boundEnv free1 n1 b
                    in (TArrow a' b', free2, n2)
                TBase b -> (TBase b, freeEnv, n)
                TBottom -> (TBottom, freeEnv, n)
                TForall v mb body ->
                    let v' = "v" ++ show n
                        n1 = n + 1
                        (mb', free1, n2) =
                            case mb of
                                Nothing -> (Nothing, freeEnv, n1)
                                Just bnd ->
                                    let (bnd', free', n') = goBound boundEnv freeEnv n1 bnd
                                    in (Just bnd', free', n')
                        (body', free2, n3) = go ((v, v') : boundEnv) free1 n2 body
                    in (TForall v' mb' body', free2, n3)
        replaceAlias boundNorm v = goReplace
          where
            goReplace ty
                | canonAllVars ty == boundNorm = TVar v
                | otherwise =
                    case ty of
                        TArrow a b -> TArrow (goReplace a) (goReplace b)
                        TForall name mb body ->
                            TForall name (fmap (mapBound goReplace) mb) (goReplace body)
                        _ -> ty
        stripAliasForall ty = case ty of
            TForall v (Just bound) body
                | TVar v' <- body
                , v == v' ->
                    stripAliasForall (tyToElab bound)
                | otherwise ->
                    TForall v (Just (stripAliasForallBound bound)) (stripAliasForall body)
            TForall v Nothing body ->
                TForall v Nothing (stripAliasForall body)
            TArrow a b -> TArrow (stripAliasForall a) (stripAliasForall b)
            _ -> ty
        stripAliasForallBound bound = case bound of
            TArrow a b -> TArrow (stripAliasForall a) (stripAliasForall b)
            TBase _ -> bound
            TBottom -> bound
            TForall v mb body ->
                let mb' = fmap stripAliasForallBound mb
                    body' = stripAliasForall body
                in TForall v mb' body'
        collapseBoundAliases binds ty =
            foldr
                (\(v, mbBound) acc ->
                    case mbBound of
                        Nothing -> acc
                        Just bound ->
                            let boundCore = stripForallsType bound
                            in if isVarBound boundCore
                                then acc
                                else
                                    let boundNorm = canonAllVars boundCore
                                    in if canonAllVars acc == boundNorm
                                        then acc
                                        else replaceAlias boundNorm v acc
                )
                ty
                binds
        normalizeScheme tyRaw binds =
            let tyAdjusted =
                    case (binds, tyRaw) of
                        ((v, mb):_, TForall v' mb' body)
                            | v == v' && mb == mb' -> body
                        _ -> tyRaw
                tyAliased = stripAliasForall (collapseBoundAliases binds tyAdjusted)
            in traceGeneralize env
                ("generalizeAt: ty0Raw=" ++ show tyAliased
                    ++ " subst=" ++ show subst
                    ++ " bindings=" ++ show binds
                )
                (tyAliased, binds)
        (ty0RawAdjusted, bindingsAdjusted) = normalizeScheme ty0RawAlias bindingsAlias
        nameForId k = "t" ++ show k
        substNames =
            [ (nameForId k, name)
            | (k, name) <- IntMap.toList subst
            ]
        namedBinderNames =
            Set.union
                (Set.fromList
                    [ name
                    | (nidInt, name) <- IntMap.toList subst
                    , IntSet.member nidInt namedUnderGaSetPlan
                    ])
                (Set.fromList [ name | (name, Just _) <- bindingsAdjusted ])
        renameVars = cataIx alg
          where
            renameFromSubst v = case lookup v substNames of
                Just v' -> v'
                Nothing -> v
            alg :: TyIF i Ty -> Ty i
            alg ty = case ty of
                TVarIF v -> TVar (renameFromSubst v)
                TArrowIF a b -> TArrow a b
                TBaseIF b -> TBase b
                TBottomIF -> TBottom
                TForallIF v mb body ->
                    let v' = renameFromSubst v
                    in TForall v' mb body
        ty0 = renameVars ty0RawAdjusted
        inlineBaseBounds = False
        (bindingsNorm0, tyNorm0) =
            simplifySchemeBindings inlineBaseBounds namedBinderNames bindingsAdjusted ty0
        (bindingsNorm1, tyNorm1) = promoteArrowAlias bindingsNorm0 tyNorm0
        (bindingsNorm, tyNorm) = (bindingsNorm1, tyNorm1)
        usedNames =
            Set.unions
                ( freeTypeVarsFrom Set.empty tyNorm
                    : [freeTypeVarsType b | (_, Just b) <- bindingsNorm]
                )
        bindingsFinal =
            filter
                (\(name, _) ->
                    Set.member name usedNames || Set.member name namedBinderNames
                )
                bindingsNorm
        bindingsFinal' =
            let dropRedundant (name, mb) =
                    not (Set.member name usedNames) &&
                    case mb of
                        Nothing -> True
                        Just bnd ->
                            let freeBound = freeTypeVarsType bnd
                                boundMentionsSelf = Set.member name freeBound
                                boundIsSimple = isVarBound bnd || isBaseBound bnd
                                boundIsBody = tyToElab bnd == tyNorm
                            in not boundMentionsSelf && (boundIsSimple || boundIsBody)
            in filter (not . dropRedundant) bindingsFinal
        aliasBounds =
            [ (name, bound)
            | (name, Just bound) <- bindingsFinal'
            , isVarBound bound
            ]
        renameTypeVars :: ElabType -> ElabType
        renameTypeVars = cataIx alg
          where
            renameFromMap v = Map.findWithDefault v v renameMap
            alg :: TyIF i Ty -> Ty i
            alg ty = case ty of
                TVarIF v -> TVar (renameFromMap v)
                TArrowIF a b -> TArrow a b
                TBaseIF b -> TBase b
                TBottomIF -> TBottom
                TForallIF v mb body ->
                    let v' = renameFromMap v
                    in TForall v' mb body
        renameMap =
            Map.fromList
                [ (old, alphaName idx 0)
                | (idx, (old, _)) <- zip [0..] bindingsFinal'
                ]
        renameName name = Map.findWithDefault name name renameMap
        bindingsRenamed =
            [ (renameName name, fmap (mapBound renameTypeVars) mb)
            | (name, mb) <- bindingsFinal'
            ]
        tyRenamed = renameTypeVars tyNorm
        _ =
            traceGeneralize env
                ("generalizeAt: tyNorm=" ++ show tyNorm
                    ++ " usedNames=" ++ show (Set.toList usedNames)
                    ++ " bindingsNorm=" ++ show bindingsNorm
                    ++ " bindingsFinal=" ++ show bindingsFinal'
                    ++ " bindingsRenamed=" ++ show bindingsRenamed
                )
                ()
        usedNamesRenamed =
            Set.unions
                ( freeTypeVarsFrom Set.empty tyRenamed
                    : [freeTypeVarsType b | (_, Just b) <- bindingsRenamed]
                )
        boundNames = Set.fromList (map fst bindingsRenamed)
        allowedNames = Set.fromList (map fst bindingsRenamed)
        missingNamesRaw = Set.toList (Set.difference usedNamesRenamed boundNames)
        aliasAllowed name =
            case parseNameId name of
                Just nid ->
                    let keyC = getNodeId (canonical (NodeId nid))
                        aliasKey = case IntMap.lookup keyC gammaAliasPlan of
                            Just repKey -> repKey
                            Nothing -> keyC
                    in case IntMap.lookup aliasKey subst of
                        Just nm -> Set.member (renameName nm) boundNames
                        Nothing -> False
                Nothing -> False
        missingNamesRaw' = filter (not . aliasAllowed) missingNamesRaw
        missingNames =
            case scopeGen of
                Nothing -> missingNamesRaw'
                Just gid ->
                    let underScope name =
                            case parseNameId name of
                                Just nid ->
                                    let nidRef = NodeId nid
                                        underSolved =
                                            firstGenAncestorGa (typeRef nidRef) == Just gid
                                        underBase =
                                            case mbBindParentsGa of
                                                Just ga ->
                                                    case IntMap.lookup nid solvedToBasePrefPlan of
                                                        Just baseN ->
                                                            firstGenAncestorFrom (gaBindParentsBase ga) (TypeRef baseN) == Just gid
                                                        Nothing -> underSolved
                                                Nothing -> underSolved
                                    in underBase
                                Nothing -> True
                    in filter underScope missingNamesRaw'
        keepNames = map fst bindingsRenamed
        subst' = IntMap.filter (`elem` keepNames) (IntMap.map renameName subst)
        finalize missing =
            if null missing
                then pure (mkElabScheme bindingsRenamed tyRenamed, subst')
                else
                    traceGeneralize env
                        ("generalizeAt: SchemeFreeVars typeRoot="
                            ++ show typeRootC
                            ++ " scopeRoot="
                            ++ show scopeRootC
                            ++ " scopeGen="
                            ++ show scopeGen
                            ++ " missing="
                            ++ show missing
                            ++ " bindingsFinal="
                            ++ show bindingsFinal
                            ++ " usedNames="
                            ++ show (Set.toList usedNames)
                            ++ " boundNames="
                            ++ show (Set.toList boundNames)
                            ++ " allowedNames="
                            ++ show (Set.toList (allowedNames :: Set.Set String))
                            ++ " bindParentsMissing="
                            ++ show
                                [ (name, IntMap.lookup (nodeRefKey (typeRef (NodeId nid))) bindParents)
                                | name <- missing
                                , Just nid <- [parseNameId name]
                                ]
                            ++ " missingBasePaths="
                            ++ show
                                [ ( name
                                  , NodeId nid
                                  , mbBase
                                  , mbBasePref
                                  , case mbBase of
                                        Nothing -> []
                                        Just baseN ->
                                            case bindingPathToRootLocal (gaBindParentsBase ga) (TypeRef baseN) of
                                                Right path -> path
                                                Left _ -> []
                                  , case mbBasePref of
                                        Nothing -> []
                                        Just baseN ->
                                            case bindingPathToRootLocal (gaBindParentsBase ga) (TypeRef baseN) of
                                                Right path -> path
                                                Left _ -> []
                                  , case mbBasePref of
                                        Nothing -> Nothing
                                        Just baseN -> firstGenAncestorFrom (gaBindParentsBase ga) (TypeRef baseN)
                                  , case mbBase of
                                        Nothing -> Nothing
                                        Just baseN ->
                                            IntMap.lookup (nodeRefKey (typeRef baseN)) (gaBindParentsBase ga)
                                  )
                                | name <- missing
                                , Just nid <- [parseNameId name]
                                , Just ga <- [mbBindParentsGa]
                                , let mbBase = IntMap.lookup nid (gaSolvedToBase ga)
                                , let mbBasePref = IntMap.lookup nid solvedToBasePrefPlan
                                ]
                        )
                        (Left $ SchemeFreeVars typeRootC missing)
    in case aliasBounds of
        [] -> finalize missingNames
        _ ->
            Left $
                ValidationFailed
                    [ "alias bounds survived scheme finalization: "
                        ++ show (map fst aliasBounds)
                    ]
