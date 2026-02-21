module MLF.Constraint.Presolution.Plan.Context (
    GaBindParents(..),
    GeneralizeEnv(..),
    GeneralizeCtx(..),
    resolveContext,
    validateCrossGenMapping,
    traceGeneralizeEnabled,
    traceGeneralize,
    traceGeneralizeM
) where

import qualified Data.IntMap.Strict as IntMap
import Data.List (nub)
import Data.Maybe (listToMaybe, isNothing)
import MLF.Util.Trace (traceWhen)

import MLF.Constraint.Types hiding (lookupNode)
import MLF.Constraint.Solve (SolveResult(..))
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.BindingUtil (bindingPathToRootLocal, firstGenAncestorFrom)
import MLF.Constraint.Presolution.Plan.BinderPlan (GaBindParentsInfo(..))
import MLF.Constraint.Presolution.Plan.SchemeRoots (SchemeRootsPlan, buildSchemeRootsPlan)
import MLF.Util.ElabError (ElabError(..), bindingToElab)

data GaBindParents = GaBindParents
    { gaBindParentsBase :: BindParents
    , gaBaseConstraint :: Constraint
    , gaBaseToSolved :: IntMap.IntMap NodeId
    , gaSolvedToBase :: IntMap.IntMap NodeId
    }

data GeneralizeEnv = GeneralizeEnv
    { geConstraint :: Constraint
    , geNodes :: IntMap.IntMap TyNode
    , geCanonical :: NodeId -> NodeId
    , geCanonKey :: NodeId -> Int
    , geLookupNode :: Int -> Maybe TyNode
    , geIsTyVarKey :: Int -> Bool
    , geIsTyForallKey :: Int -> Bool
    , geIsBaseLikeKey :: Int -> Bool
    , geBindParentsGa :: Maybe GaBindParents
    , geRes :: SolveResult
    , geDebugEnabled :: Bool
    }

data GeneralizeCtx = GeneralizeCtx
    { gcTarget0 :: NodeId
    , gcTargetBase :: NodeId
    , gcScopeRootC :: NodeRef
    , gcOrderRoot :: NodeId
    , gcTypeRoot0 :: NodeId
    , gcOrderRootBase :: NodeId
    , gcScopeGen :: Maybe GenNodeId
    , gcBindParents :: BindParents
    , gcFirstGenAncestor :: NodeRef -> Maybe GenNodeId
    , gcConstraintForReify :: Constraint
    , gcResForReify :: SolveResult
    , gcBindParentsGaInfo :: Maybe GaBindParentsInfo
    , gcSchemeRootsPlan :: SchemeRootsPlan
    }

data ResolveTarget = ResolveTarget
    { rtTarget0 :: NodeId
    , rtTargetBase :: NodeId
    , rtScopeRootC :: NodeRef
    , rtOrderRoot :: NodeId
    , rtTypeRoot0 :: NodeId
    , rtOrderRootBase :: NodeId
    }

data ResolveScope = ResolveScope
    { rsScopeGen :: Maybe GenNodeId
    }

data ResolveBinds = ResolveBinds
    { rbBindParents :: BindParents
    , rbFirstGenAncestor :: NodeRef -> Maybe GenNodeId
    , rbConstraintForReify :: Constraint
    , rbResForReify :: SolveResult
    }

resolveContext
    :: GeneralizeEnv
    -> BindParents
    -> NodeRef
    -> NodeId
    -> Either ElabError GeneralizeCtx
resolveContext env bindParentsSoft scopeRootArg targetNodeArg = do
    let constraint = geConstraint env
        nodes = geNodes env
        canonical = geCanonical env
        canonKey = geCanonKey env
        lookupNode = geLookupNode env
        mbBindParentsGa' = geBindParentsGa env
        mbBindParentsGaInfo =
            fmap
                (\ga -> GaBindParentsInfo
                    { gbiBindParentsBase = gaBindParentsBase ga
                    , gbiBaseConstraint = gaBaseConstraint ga
                    , gbiBaseToSolved = gaBaseToSolved ga
                    , gbiSolvedToBase = gaSolvedToBase ga
                    })
                mbBindParentsGa'
        scopeRoot0 = case scopeRootArg of
            TypeRef nid -> TypeRef (canonical nid)
            GenRef gid -> GenRef gid
    -- Phase 1: canonicalize roots/targets and choose order roots.
    let resolveTarget node =
            case lookupNode (canonKey node) of
                Just TyExp{ tnBody = b } -> canonical b
                _ -> canonical node
        resolveTargetBase target =
            case mbBindParentsGa' of
                Nothing -> target
                Just ga ->
                    IntMap.findWithDefault target (getNodeId target) (gaSolvedToBase ga)
        resolveScopeRoot root =
            case (root, mbBindParentsGa') of
                (TypeRef nid, Just ga) ->
                    case IntMap.lookup (getNodeId nid) (gaSolvedToBase ga) of
                        Nothing -> root
                        Just baseN ->
                            case bindingPathToRootLocal (gaBindParentsBase ga) (typeRef baseN) of
                                Left _ -> root
                                Right path ->
                                    case listToMaybe [gid | GenRef gid <- drop 1 path] of
                                        Just gid -> GenRef gid
                                        Nothing -> root
                _ -> root
        resolveOrderRoots root target =
            case root of
                GenRef _ ->
                    case lookupNode (canonKey target) of
                        Just TyForall{ tnBody = b } ->
                            let bodyRoot = canonical b
                            in (bodyRoot, target)
                        _ -> (target, target)
                TypeRef _ ->
                    case lookupNode (canonKey target) of
                        Just TyForall{ tnBody = b } ->
                            let bodyRoot = canonical b
                            in (bodyRoot, target)
                        _ -> (target, target)
        resolveOrderRootBase root target =
            case mbBindParentsGa' of
                Nothing -> root
                Just ga ->
                    let baseNodes = cNodes (gaBaseConstraint ga)
                    in case lookupNodeIn baseNodes target of
                        Just TyForall{ tnBody = b } -> b
                        _ -> target
        resolveTargetPhase root node =
            let target0 = resolveTarget node
                targetBase = resolveTargetBase target0
                scopeRootC = resolveScopeRoot root
                (orderRoot, typeRoot0) = resolveOrderRoots scopeRootC target0
                orderRootBase = resolveOrderRootBase orderRoot targetBase
            in ResolveTarget
                { rtTarget0 = target0
                , rtTargetBase = targetBase
                , rtScopeRootC = scopeRootC
                , rtOrderRoot = orderRoot
                , rtTypeRoot0 = typeRoot0
                , rtOrderRootBase = orderRootBase
                }
    let targetPhase = resolveTargetPhase scopeRoot0 targetNodeArg
        ResolveTarget
            { rtTarget0 = target0
            , rtTargetBase = targetBase
            , rtScopeRootC = scopeRootC
            , rtOrderRoot = orderRoot
            , rtTypeRoot0 = typeRoot0
            , rtOrderRootBase = orderRootBase
            } = targetPhase
    traceGeneralizeM env
        ("generalizeAt: scopeRootC=" ++ show scopeRootC
            ++ " target0=" ++ show target0
            ++ " orderRoot=" ++ show orderRoot
            ++ " typeRoot0=" ++ show typeRoot0
            ++ case mbBindParentsGa' of
                Nothing -> ""
                Just _ -> " orderRootBase=" ++ show orderRootBase
        )
    -- Phase 2: discover the scope's owning gen node (if any).
    let resolveScopeGen root =
            case root of
                GenRef gid -> pure (Just gid)
                TypeRef nid ->
                    case mbBindParentsGa' of
                        Just ga ->
                            case IntMap.lookup (getNodeId nid) (gaSolvedToBase ga) of
                                Just baseNid ->
                                    pure (firstGenAncestor (gaBindParentsBase ga) (TypeRef baseNid))
                                Nothing -> do
                                    path <- bindingToElab (Binding.bindingPathToRoot constraint (TypeRef nid))
                                    pure (listToMaybe [gid | GenRef gid <- drop 1 path])
                        Nothing -> do
                            path <- bindingToElab (Binding.bindingPathToRoot constraint (TypeRef nid))
                            pure (listToMaybe [gid | GenRef gid <- drop 1 path])
        resolveScopePhase root = do
            scopeGen <- resolveScopeGen root
            pure ResolveScope { rsScopeGen = scopeGen }
    scopePhase <- resolveScopePhase scopeRootC
    let ResolveScope { rsScopeGen = scopeGen } = scopePhase
    -- Phase 3: merge binding parents (base + solved) into a single view.
    let resolveBindParents scopeGen' =
            case (mbBindParentsGa', scopeGen') of
                (Just ga, Just gidScope) ->
                    let baseParents = gaBindParentsBase ga
                        baseToSolved = gaBaseToSolved ga
                        solvedToBase = gaSolvedToBase ga
                        findSolvedKey baseKey =
                            case IntMap.lookup baseKey baseToSolved of
                                Just solvedNid -> Just (getNodeId (canonical solvedNid))
                                Nothing ->
                                    let candidates =
                                            [ solvedKey
                                            | (solvedKey, baseNid) <- IntMap.toList solvedToBase
                                            , getNodeId baseNid == baseKey
                                            ]
                                    in case candidates of
                                        (k:_) -> Just k
                                        _ ->
                                            if IntMap.member baseKey nodes
                                                then Just baseKey
                                                else Nothing
                        mapBaseRefToSolved parentRef =
                            case parentRef of
                                GenRef gid -> Just (GenRef gid)
                                TypeRef parentN ->
                                    case findSolvedKey (getNodeId parentN) of
                                        Just solvedKey -> Just (TypeRef (NodeId solvedKey))
                                        Nothing -> Nothing
                        bindParentsGaFix =
                            IntMap.foldlWithKey'
                                (\acc childKey (parentRef, flag) ->
                                    case nodeRefFromKey childKey of
                                        TypeRef baseChild ->
                                            let baseAncestor = firstGenAncestor baseParents (TypeRef baseChild)
                                            in if baseAncestor /= Just gidScope
                                                then acc
                                                else
                                                    case findSolvedKey (getNodeId baseChild) of
                                                        Nothing -> acc
                                                        Just solvedChildKey ->
                                                            case mapBaseRefToSolved parentRef of
                                                                Nothing -> acc
                                                                Just parentRef' ->
                                                                    let childKey' = nodeRefKey (TypeRef (NodeId solvedChildKey))
                                                                        existing = IntMap.lookup childKey' bindParentsSoft
                                                                        selfParent =
                                                                            case existing of
                                                                                Just (parentExisting, _) ->
                                                                                    nodeRefKey parentExisting == childKey'
                                                                                Nothing -> False
                                                                        redirected = solvedChildKey /= getNodeId baseChild
                                                                        solvedAncestor =
                                                                            firstGenAncestor
                                                                                bindParentsSoft
                                                                                (TypeRef (NodeId solvedChildKey))
                                                                        shouldOverride =
                                                                            isNothing existing
                                                                                || selfParent
                                                                                || redirected
                                                                                || solvedAncestor /= Just gidScope
                                                                    in if shouldOverride
                                                                        then
                                                                            IntMap.insertWith
                                                                                (\(parentNew, flagNew) (_parentOld, flagOld) ->
                                                                                    (parentNew, max flagNew flagOld)
                                                                                )
                                                                                childKey'
                                                                                (parentRef', flag)
                                                                                acc
                                                                        else acc
                                        _ -> acc
                                )
                                IntMap.empty
                                baseParents
                    in do
                        validateCrossGenMapping gidScope (firstGenAncestor baseParents) baseParents findSolvedKey
                        pure (IntMap.union bindParentsGaFix bindParentsSoft)
                _ -> pure bindParentsSoft
        resolveFirstGenAncestor bindParents' =
            case mbBindParentsGa' of
                Nothing -> firstGenAncestor bindParents'
                Just ga ->
                    \ref ->
                        case ref of
                            GenRef gid -> Just gid
                            TypeRef nid ->
                                let key = getNodeId (canonical nid)
                                    baseConstraint = gaBaseConstraint ga
                                in case IntMap.lookup key (gaSolvedToBase ga) of
                                    Just baseNid ->
                                        firstGenAncestor (gaBindParentsBase ga) (TypeRef baseNid)
                                    Nothing ->
                                        case lookupNodeIn (cNodes baseConstraint) (NodeId key) of
                                            Just _ ->
                                                firstGenAncestor (gaBindParentsBase ga) (TypeRef (NodeId key))
                                            Nothing -> Nothing
        resolveBindsPhase scopeGen' = do
            bindParents <- resolveBindParents scopeGen'
            let firstGenAncestorGa = resolveFirstGenAncestor bindParents
                constraintForReify = constraint { cBindParents = bindParents }
                resForReify = (geRes env) { srConstraint = constraintForReify }
            pure ResolveBinds
                { rbBindParents = bindParents
                , rbFirstGenAncestor = firstGenAncestorGa
                , rbConstraintForReify = constraintForReify
                , rbResForReify = resForReify
                }
    bindsPhase <- resolveBindsPhase scopeGen
    let ResolveBinds
            { rbBindParents = bindParents
            , rbFirstGenAncestor = firstGenAncestorGa
            , rbConstraintForReify = constraintForReify
            , rbResForReify = resForReify
            } = bindsPhase
        schemeRootsPlan =
            buildSchemeRootsPlan
                canonical
                constraint
                nodes
                mbBindParentsGaInfo
                firstGenAncestorFrom
    pure GeneralizeCtx
        { gcTarget0 = target0
        , gcTargetBase = targetBase
        , gcScopeRootC = scopeRootC
        , gcOrderRoot = orderRoot
        , gcTypeRoot0 = typeRoot0
        , gcOrderRootBase = orderRootBase
        , gcScopeGen = scopeGen
        , gcBindParents = bindParents
        , gcFirstGenAncestor = firstGenAncestorGa
        , gcConstraintForReify = constraintForReify
        , gcResForReify = resForReify
        , gcBindParentsGaInfo = mbBindParentsGaInfo
        , gcSchemeRootsPlan = schemeRootsPlan
        }

  where
    firstGenAncestor bindParents' start =
        case bindingPathToRootLocal bindParents' start of
            Left _ -> Nothing
            Right path -> listToMaybe [gid | GenRef gid <- drop 1 path]

-- | Check that base nodes mapping to the same solved key share a gen ancestor.
-- Within 'resolveContext' this is called with the gidScope-filtered
-- 'firstGenAncestor', making it a safety-net tautology.  Exported so tests
-- can exercise the logic directly with a mock ancestor function.
validateCrossGenMapping
    :: GenNodeId
    -> (NodeRef -> Maybe GenNodeId)  -- ^ firstGenAncestor for the base tree
    -> BindParents                   -- ^ base binding parents
    -> (Int -> Maybe Int)            -- ^ findSolvedKey
    -> Either ElabError ()
validateCrossGenMapping gidScope fga baseParents findSolvedKey =
    let grouped = IntMap.foldlWithKey' collect IntMap.empty baseParents
        collect acc childKey (_parentRef, _flag) =
            case nodeRefFromKey childKey of
                TypeRef baseChild ->
                    let ancestor = fga (TypeRef baseChild)
                    in if ancestor /= Just gidScope
                        then acc
                        else case findSolvedKey (getNodeId baseChild) of
                            Nothing -> acc
                            Just solvedKey ->
                                IntMap.insertWith (++) solvedKey
                                    [TypeRef baseChild] acc
                _ -> acc
        conflicts =
            [ "ga-invariant: solved key " ++ show sk
                ++ " has base nodes " ++ show bases
                ++ " with conflicting gen ancestors"
            | (sk, bases) <- IntMap.toList grouped
            , length bases > 1
            , let genAncestors = nub
                    [ g
                    | TypeRef bn <- bases
                    , Just g <- [fga (TypeRef bn)]
                    ]
            , length genAncestors > 1
            ]
    in if null conflicts
        then Right ()
        else Left (ValidationFailed conflicts)

traceGeneralizeEnabled :: Bool -> String -> a -> a
traceGeneralizeEnabled = traceWhen

traceGeneralize :: GeneralizeEnv -> String -> a -> a
traceGeneralize env = traceGeneralizeEnabled (geDebugEnabled env)

traceGeneralizeM :: GeneralizeEnv -> String -> Either ElabError ()
traceGeneralizeM env msg = traceGeneralize env msg (Right ())
