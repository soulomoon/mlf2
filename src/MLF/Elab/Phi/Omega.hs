{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Elab.Phi.Omega
Description : Omega/Step interpretation for witness translation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module hosts the Omega/Step interpretation helpers used by the
Phi translation pipeline, keeping the main Phi facade focused on orchestration.
-}
module MLF.Elab.Phi.Omega (
    OmegaContext(..),
    phiWithSchemeOmega,
    normalizeInst,
    collapseAdjacentPairs
) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when)
import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (elemIndex, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Text.Read (readMaybe)

import qualified MLF.Util.Order as Order
import qualified MLF.Util.OrderKey as OrderKey
import MLF.Constraint.Types
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (InteriorNodes(..))
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Binding.Tree as Binding
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Inst (applyInstantiation, composeInst, instMany, schemeToType, splitForalls)
import MLF.Elab.Phi.Context (contextToNodeBoundWithOrderKeys)
import qualified MLF.Elab.Phi.IdentityBridge as IB
import MLF.Elab.Phi.VSpine (VSpine(..), BodyShape(..), mkVSpine, vSpineNames, vSpineBounds, vSpineIds, vSpineLength, vSpineNull, vSpineNameAt, vSpineBoundAt, vsDeleteAt, vsInsertAt, vsUpdateBound)
import MLF.Elab.Sigma (bubbleReorderTo, bubbleReorderToFromSpine)
import MLF.Elab.Types
import MLF.Reify.Core (reifyBoundWithNames, reifyTypeWithNamedSetNoFallback)
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarsList, inlineAliasBoundsWithBy, inlineBaseBoundsType, matchType, substTypeCapture)
import MLF.Util.Graph (topoSortBy)
import MLF.Util.Trace (TraceConfig, traceGeneralize)
import MLF.Util.Names (parseNameId)

-- | Shared context for Omega/Step interpretation.
data OmegaContext = OmegaContext
    { ocTraceConfig :: TraceConfig
    , ocSolved :: Solved
    , ocCopyMap :: IntMap.IntMap NodeId
    , ocGaParents :: Maybe GaBindParents
    , ocTrace :: Maybe EdgeTrace
    , ocSchemeInfo :: Maybe SchemeInfo
    , ocTraceBinderSources :: IntSet.IntSet
    , ocTraceBinderReplayMap :: IntMap.IntMap NodeId
    , ocTraceBinderHintDomain :: IntSet.IntSet
    , ocTraceBinderSourceNames :: IntMap.IntMap String
    , ocEdgeRoot :: NodeId
    , ocEdgeLeft :: NodeId
    , ocEdgeRight :: NodeId
    }

newtype ApplyFun i =
    ApplyFun { runApplyFun :: Set.Set String -> Ty i }

phiWithSchemeOmega
    :: OmegaContext
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> SchemeInfo
    -> Int            -- ^ forall intro count (O phase)
    -> [InstanceOp]   -- ^ omega ops
    -> Either ElabError Instantiation
phiWithSchemeOmega ctx namedSet keepBinderKeys si introCount omegaOps = phiWithScheme
  where
    solved :: Solved
    solved = ocSolved ctx

    canonicalNode :: NodeId -> NodeId
    canonicalNode = Solved.canonical solved

    copyMap :: IntMap.IntMap NodeId
    copyMap = ocCopyMap ctx

    mTrace :: Maybe EdgeTrace
    mTrace = ocTrace ctx

    ib :: IB.IdentityBridge
    -- Note [gaSolvedToBase subsumption]: The bridge's source-key expansion
    -- (copy-map reverse, trace reverse, canonical alias) subsumes the old
    -- gaSolvedToBase lookup for all known cases.  If future binder-index
    -- resolution failures surface, consider integrating GaBindParents into
    -- the bridge rather than re-adding local resolution here.
    ib = IB.mkIdentityBridge solved mTrace copyMap

    mSchemeInfo :: Maybe SchemeInfo
    mSchemeInfo = ocSchemeInfo ctx

    traceBinderSources :: IntSet.IntSet
    traceBinderSources = ocTraceBinderSources ctx

    traceBinderReplayMap :: IntMap.IntMap NodeId
    traceBinderReplayMap = ocTraceBinderReplayMap ctx

    traceBinderHintDomain :: IntSet.IntSet
    traceBinderHintDomain = ocTraceBinderHintDomain ctx

    traceBinderSourceNames :: IntMap.IntMap String
    traceBinderSourceNames = ocTraceBinderSourceNames ctx

    edgeRoot :: NodeId
    edgeRoot = ocEdgeRoot ctx

    edgeLeft :: NodeId
    edgeLeft = ocEdgeLeft ctx

    edgeRight :: NodeId
    edgeRight = ocEdgeRight ctx

    debugPhi :: String -> a -> a
    debugPhi = traceGeneralize (ocTraceConfig ctx)

    interiorSet :: IntSet.IntSet
    interiorSet =
        case mTrace of
            Nothing -> IntSet.empty
            Just tr ->
                let InteriorNodes s0 = etInterior tr
                    remapKey k =
                        let nidC = canonicalNode (NodeId k)
                            keyC = getNodeId nidC
                        in case Solved.lookupNode solved nidC of
                            Just TyVar{} ->
                                case IntMap.lookup keyC copyMap of
                                    Nothing -> keyC
                                    Just nid -> getNodeId (canonicalNode nid)
                            _ -> keyC
                in IntSet.fromList (map remapKey (IntSet.toList s0))

    orderRoot :: NodeId
    -- Paper root `r` for Phi/Sigma is the expansion root (TyExp body), not the TyExp
    -- wrapper itself. When a trace is available, prefer its root to stay in
    -- the same node space as witness operations.
    orderRoot =
        case mTrace of
            Nothing -> edgeRoot
            Just tr -> etRoot tr

    nodes = cNodes (Solved.solvedConstraint solved)

    boundKids nid = case lookupNodeIn nodes nid of
        Just TyVar{ tnBound = Just bnd } -> [bnd]
        _ -> []

    schemeRootGenMap =
        IntMap.fromList
            [ (getNodeId (canonicalNode root), gnId gen)
            | gen <- NodeAccess.allGenNodes (Solved.solvedConstraint solved)
            , root <- gnSchemes gen
            ]

    genChildrenMap =
        IntMap.fromListWith (++)
            [ ( getGenNodeId gid
              , [canonicalNode child]
              )
            | (childKey, (parentRef, _flag)) <- IntMap.toList (Solved.bindParents solved)
            , GenRef gid <- [parentRef]
            , TypeRef child <- [nodeRefFromKey childKey]
            ]

    bindKids rootC nid =
        let localChildren =
                [ canonicalNode child
                | (childKey, (parentRef, _flag)) <- IntMap.toList (Solved.bindParents solved)
                , TypeRef child <- [nodeRefFromKey childKey]
                , parentRefCanon parentRef == TypeRef (canonicalNode nid)
                ]
            genChildren =
                case IntMap.lookup (getNodeId (canonicalNode nid)) schemeRootGenMap of
                    Nothing -> []
                    Just gid -> IntMap.findWithDefault [] (getGenNodeId gid) genChildrenMap
            siblingGenChildren =
                if canonicalNode nid == rootC
                    then case Solved.lookupBindParent solved (typeRef (canonicalNode nid)) of
                        Just (GenRef gid, _) -> IntMap.findWithDefault [] (getGenNodeId gid) genChildrenMap
                        _ -> []
                    else []
        in IntMap.elems $
            IntMap.fromList
                [ (getNodeId child, child)
                | child <- localChildren ++ genChildren ++ siblingGenChildren
                ]

    parentRefCanon (TypeRef parentN) = TypeRef (canonicalNode parentN)
    parentRefCanon (GenRef gid) = GenRef gid

    orderKeysFromRoot root =
        let rootC = canonicalNode root
            extraChildren nid = boundKids nid ++ bindKids rootC nid
        in OrderKey.orderKeysFromRootWithExtra canonicalNode nodes extraChildren root Nothing

    lcaRootForBinders binders =
        case map (TypeRef . canonicalNode) binders of
            [] -> orderRoot
            (r0:rs) ->
                case foldM (Binding.bindingLCA (Solved.solvedConstraint solved)) r0 rs of
                    Left _ -> orderRoot
                    Right (TypeRef nid) -> canonicalNode nid
                    Right (GenRef gid) ->
                        fromMaybe orderRoot $ do
                            gen <- NodeAccess.lookupGenNode (Solved.solvedConstraint solved) gid
                            listToMaybe (gnSchemes gen)

    orderKeys :: IntMap.IntMap Order.OrderKey
    -- Order keys are used to compare binder positions (≺) for Σ(g) / ϕR (thesis Def. 15.3.4).
    -- The natural "paper root" for Φ/Σ is the expansion root `r` (often a TyExp body), but `r`
    -- might not reach the scheme binders via the binding tree when `r` is strictly *under* a
    -- TyForall wrapper. In that situation, compute order keys from the binding-tree LCA of the
    -- scheme binders instead, so every binder identity has a key.
    orderKeys = orderKeysFromRoot orderKeysRoot

    orderKeysRoot :: NodeId
    orderKeysRoot = lcaRootForBinders [NodeId k | k <- IntMap.keys (siSubst si)]

    orderKeysForBinders binders =
        case binders of
            [] -> orderKeys
            _ -> orderKeysFromRoot (lcaRootForBinders binders)

    schemeBinderKeys :: IntSet.IntSet
    schemeBinderKeys = IntSet.fromList (IntMap.keys (siSubst si))

    isSchemeBinder :: NodeId -> Bool
    isSchemeBinder nid =
        let nidC = canonicalNode nid
        in IntSet.member (getNodeId nidC) schemeBinderKeys
            && case Solved.lookupNode solved nidC of
                Just TyVar{} -> True
                _ -> False

    substForTypes :: IntMap.IntMap String
    substForTypes =
        case mSchemeInfo of
            Just si' -> siSubst si'
            Nothing -> IntMap.empty

    traceArgMap :: IntSet.IntSet -> Map.Map String ElabType
    traceArgMap namedSet' =
        case (mTrace, mSchemeInfo) of
            (Just tr, Just si') ->
                let subst = siSubst si'
                    nameFor nid = IntMap.lookup (getNodeId (canonicalNode nid)) subst
                    reifyArg arg =
                        case Solved.lookupVarBound solved (canonicalNode arg) of
                            Just bnd -> reifyBoundWithNames (Solved.toSolveResult solved) subst bnd
                            Nothing -> reifyTypeWithNamedSetNoFallback (Solved.toSolveResult solved) subst namedSet' (canonicalNode arg)
                    entries =
                        [ (name, ty)
                        | (binder, arg) <- etBinderArgs tr
                        , Just name <- [nameFor binder]
                        , Right ty <- [reifyArg arg]
                        ]
                in Map.fromList entries
            _ -> Map.empty

    inferredArgMapFromTarget :: IntSet.IntSet -> Map.Map String ElabType
    inferredArgMapFromTarget namedSet' =
        case mSchemeInfo of
            Nothing -> Map.empty
            Just si' ->
                let inferFrom nid =
                        case reifyTargetTypeForInst namedSet' nid of
                            Left _ -> Nothing
                            Right targetTy -> inferInstAppArgs (siScheme si') targetTy
                    mbArgs =
                        inferFrom edgeRight
                            <|> inferFrom edgeLeft
                in case mbArgs of
                    Nothing -> Map.empty
                    Just args ->
                        let (binds, _) = splitForalls (schemeToType (siScheme si'))
                            names = map fst binds
                        in Map.fromList (zip names args)

    inferredArgMap :: IntSet.IntSet -> Map.Map String ElabType
    inferredArgMap namedSet' =
        -- Prefer target/scheme inference over trace args when both are present;
        -- trace reification can collapse to ⊥ too early for copied binder nodes.
        Map.union (inferredArgMapFromTarget namedSet') (traceArgMap namedSet')

    applyInferredArgs :: IntSet.IntSet -> ElabType -> ElabType
    applyInferredArgs namedSet' = applyInferredArgsWith namedSet' Set.empty

    applyInferredArgsWith :: IntSet.IntSet -> Set.Set String -> ElabType -> ElabType
    applyInferredArgsWith namedSet' bound0 ty0 = runApplyFun (cataIx alg ty0) bound0
      where
        inferredArgMap' = inferredArgMap namedSet'
        alg :: TyIF i ApplyFun -> ApplyFun i
        alg ty = case ty of
            TVarIF v ->
                ApplyFun $ \bound ->
                    if Set.member v bound
                        then TVar v
                        else case Map.lookup v inferredArgMap' of
                            Just instTy -> instTy
                            Nothing -> TVar v
            TArrowIF a b ->
                ApplyFun $ \bound ->
                    TArrow (runApplyFun a bound) (runApplyFun b bound)
            TConIF c args ->
                ApplyFun $ \bound ->
                    TCon c (fmap (\f -> runApplyFun f bound) args)
            TBaseIF b -> ApplyFun (const (TBase b))
            TBottomIF -> ApplyFun (const TBottom)
            TForallIF v mb body ->
                ApplyFun $ \bound ->
                    let bound' = Set.insert v bound
                        mb' = fmap (\f -> runApplyFun f bound) mb
                    in TForall v mb' (runApplyFun body bound')

    _binderArgType :: IntSet.IntSet -> NodeId -> Maybe ElabType
    _binderArgType namedSet' binder = do
        name <- IntMap.lookup (getNodeId (canonicalNode binder)) substForTypes
        Map.lookup name (inferredArgMap namedSet')

    reifyTypeArg :: IntSet.IntSet -> Maybe NodeId -> NodeId -> Either ElabError ElabType
    reifyTypeArg namedSet' mbBinder arg = do
        let argC = canonicalNode arg
        ty <- case Solved.lookupVarBound solved argC of
            Just bnd -> reifyTypeWithNamedSetNoFallback (Solved.toSolveResult solved) substForTypes namedSet' bnd
            Nothing -> reifyTypeWithNamedSetNoFallback (Solved.toSolveResult solved) substForTypes namedSet' argC
        let inferredSingleton =
                case Map.toList (inferredArgMapFromTarget namedSet') of
                    [(_name, inferredTy)] -> Just inferredTy
                    _ -> Nothing
            chosenTy0 =
                case (ty, inferredSingleton) of
                    (TVar _, Just inferredTy)
                        | not (containsBottomTy inferredTy) -> inferredTy
                    _ -> ty
            chosenTy1 =
                case (chosenTy0, inferredSingleton) of
                    (TVar _, _) -> chosenTy0
                    (_, Just (TVar inferredVar)) ->
                        case
                            (parseNameId inferredVar >>= (`IntMap.lookup` substForTypes))
                                <|> Just inferredVar of
                            Just binderName ->
                                case filter (/= binderName) (freeTypeVarsList chosenTy0) of
                                    [fv] -> substTypeCapture fv (TVar binderName) chosenTy0
                                    _ -> chosenTy0
                            Nothing -> chosenTy0
                    _ -> chosenTy0
            chosenTy = substSchemeNames chosenTy1
            rescuedTy =
                case (mbBinder, chosenTy) of
                    (Just binder, TBottom) ->
                        case IntMap.lookup (getNodeId (canonicalNode binder)) substForTypes of
                            Just binderName -> TVar binderName
                            Nothing -> chosenTy
                    _ -> chosenTy
        debugPhi
            ("reifyTypeArg(reify) arg=" ++ show arg
                ++ " mbBinder=" ++ show mbBinder
                ++ " inferredFromTarget=" ++ show (inferredArgMapFromTarget namedSet')
                ++ " inferredMap=" ++ show (inferredArgMap namedSet')
                ++ " freeChosen=" ++ show (freeTypeVarsList chosenTy0)
                ++ " ty=" ++ show ty
                ++ " chosenTy0=" ++ show chosenTy0
                ++ " chosenTy1=" ++ show chosenTy1
                ++ " chosenTy=" ++ show chosenTy
                ++ " rescuedTy=" ++ show rescuedTy
            )
            (pure rescuedTy)

    substSchemeNames :: ElabType -> ElabType
    substSchemeNames = cataIx alg
      where
        alg :: TyIF i Ty -> Ty i
        alg tyNode = case tyNode of
            TVarIF v ->
                case parseNameId v of
                    Just nid ->
                        case IntMap.lookup nid substForTypes of
                            Just name -> TVar name
                            Nothing -> TVar v
                    Nothing -> TVar v
            TArrowIF a b -> TArrow a b
            TConIF c args -> TCon c args
            TBaseIF b -> TBase b
            TForallIF v mb body -> TForall v mb body
            TBottomIF -> TBottom

    containsBottomTy :: Ty v -> Bool
    containsBottomTy ty = case ty of
        TVar _ -> False
        TBase _ -> False
        TBottom -> True
        TArrow a b -> containsBottomTy a || containsBottomTy b
        TCon _ args -> any containsBottomTy args
        TForall _ mb body -> maybe False containsBottomTy mb || containsBottomTy body

    reifyBoundType :: NodeId -> Either ElabError ElabType
    reifyBoundType = reifyBoundWithNames (Solved.toSolveResult solved) substForTypes

    reifyTargetTypeForInst :: IntSet.IntSet -> NodeId -> Either ElabError ElabType
    reifyTargetTypeForInst namedSet' nid = do
        let nidC = canonicalNode nid
        ty <- case Solved.lookupVarBound solved nidC of
            Just bnd -> reifyTypeWithNamedSetNoFallback (Solved.toSolveResult solved) substForTypes namedSet' bnd
            Nothing -> reifyTypeWithNamedSetNoFallback (Solved.toSolveResult solved) substForTypes namedSet' nidC
        pure (inlineBaseBounds ty)

    inlineBaseBounds :: ElabType -> ElabType
    inlineBaseBounds =
        inlineBaseBoundsType
            (Solved.solvedConstraint solved)
            canonicalNode

    inlineAliasBounds :: ElabType -> ElabType
    inlineAliasBounds = inlineAliasBoundsWith False

    inlineAliasBoundsAsBound :: ElabType -> ElabType
    inlineAliasBoundsAsBound = inlineAliasBoundsWith True

    -- See Note [Scope-aware bound/alias inlining] in
    -- docs/notes/2026-01-27-elab-changes.md.
    inlineAliasBoundsWith :: Bool -> ElabType -> ElabType
    inlineAliasBoundsWith fallbackToBottom =
        inlineAliasBoundsWithBy
            fallbackToBottom
            canonicalNode
            (cNodes (Solved.solvedConstraint solved))
            (Solved.lookupVarBound solved)
            (reifyBoundWithNames (Solved.toSolveResult solved) substForTypes)

    inferInstAppArgs :: ElabScheme -> ElabType -> Maybe [ElabType]
    inferInstAppArgs scheme targetTy =
        let (binds, body) = splitForalls (schemeToType scheme)
            binderNames = map fst binds
        in case matchType (Set.fromList binderNames) body targetTy of
            Left _ -> Nothing
            Right subst ->
                if all (`Map.member` subst) binderNames
                    then Just [ty | name <- binderNames, Just ty <- [Map.lookup name subst]]
                    else Nothing

    -- | Paper Def. 15.3.4 / Fig. 15.3.5: Φ(e) = Σ; O; Φχe(Ω).
    -- Thesis treats quantifier introduction (O) and witness replay (Ω) as
    -- separate phases. The intro count drives O as a prefix of InstIntro
    -- steps, then the omega ops are replayed via `go`.
    phiWithScheme :: Either ElabError Instantiation
    phiWithScheme = do
        let ty0 = schemeToType (siScheme si)
            subst = siSubst si
            lookupBinder (NodeId i) = IntMap.lookup i subst
            ids0 = idsForStartType si ty0
            binderKeys = IntSet.fromList (IntMap.keys subst)
        -- Always attempt Σ(g) / ϕR at the start (thesis Def. 15.3.4), even if Ω has no Raise steps.
        (sigma, ty1, ids1) <- reorderBindersByPrec ty0 ids0
        -- Phase O: apply all quantifier introductions up front.
        (ty2, ids2) <- applyIntros introCount ty1 ids1
        let phiIntro = instMany (replicate introCount InstIntro)
        -- Phase Ω: replay witness operations on the intro-extended type.
        let vs2 = mkVSpine ty2 ids2
        phiOmega <- go binderKeys keepBinderKeys namedSet vs2 [] omegaOps lookupBinder
        pure (normalizeInst (instMany [sigma, phiIntro, phiOmega]))


    -- | Apply n quantifier introductions, prepending Nothing to ids each time.
    applyIntros :: Int -> ElabType -> [Maybe NodeId] -> Either ElabError (ElabType, [Maybe NodeId])
    applyIntros 0 ty ids = Right (ty, ids)
    applyIntros n ty ids = do
        ty' <- applyInst "applyIntros" ty InstIntro
        applyIntros (n - 1) ty' (Nothing : ids)

    applyInst :: String -> ElabType -> Instantiation -> Either ElabError ElabType
    applyInst label ty0 inst = case applyInstantiation ty0 inst of
        Left (InstantiationError msg) ->
            Left $ PhiInvariantError $
                label ++ ": " ++ msg ++ " ; inst=" ++ pretty inst ++ " ; ty=" ++ pretty ty0
        other -> other


    reorderBindersByPrec :: ElabType -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderBindersByPrec ty ids = do
        let vs0 = mkVSpine ty ids
        when (vSpineLength vs0 /= length ids) $
            Left (PhiInvariantError "PhiReorder: binder spine / identity list length mismatch")
        if vSpineLength vs0 < 2
            then Right (InstId, ty, ids)
            else do
                let missingIdPositions =
                        [ i
                        | (i, Nothing) <- zip [(0::Int)..] ids
                        , i < schemeArity
                        ]
                      where
                        schemeArity = case siScheme si of
                            Forall binds _ -> length binds
                    sourceBinders = [ canonicalNode nid | Just nid <- ids, isSchemeBinder nid ]
                    orderKeysActive = orderKeysForBinders sourceBinders
                    missingKeyBinders =
                        [ nid
                        | Just nid <- ids
                        , isSchemeBinder nid
                        , not (IntMap.member (getNodeId (canonicalNode nid)) orderKeysActive)
                        ]
                unless (null missingIdPositions) $
                    Left $
                        PhiInvariantError $
                            "PhiReorder: missing binder identity at positions " ++ show missingIdPositions
                let orderKeysForSort =
                        if null missingKeyBinders
                            then orderKeysActive
                            else orderKeys
                desired <- desiredBinderOrder orderKeysForSort vs0 ids
                reorderTo vs0 ty ids desired

    desiredBinderOrder :: IntMap.IntMap Order.OrderKey -> VSpine -> [Maybe NodeId] -> Either ElabError [Maybe NodeId]
    desiredBinderOrder orderKeysActive vs0 ids = do
        let names = vSpineNames vs0
            bounds = vSpineBounds vs0
            n = vSpineLength vs0
            nameIndex nm = elemIndex nm names

            -- Bound dependencies: if a occurs free in b's bound, then a must appear before b.
            depsFor :: Int -> [Int]
            depsFor i =
                case bounds !! i of
                    Nothing -> []
                    Just bnd ->
                        [ j
                        | v <- freeTypeVarsList bnd
                        , v /= names !! i
                        , Just j <- [nameIndex v]
                        ]

            cmpIdx :: Int -> Int -> Ordering
            cmpIdx i j =
                case (ids !! i, ids !! j) of
                    (Just a, Just b)
                        | not (isSchemeBinder a) || not (isSchemeBinder b) ->
                            compare i j
                    (Just a, Just b) ->
                        let ca = canonicalNode a
                            cb = canonicalNode b
                        in case Order.compareNodesByOrderKey orderKeysActive ca cb of
                            Right ord -> ord
                            Left _ -> compare i j  -- fallback if missing key
                    (Just _, Nothing) -> LT
                    (Nothing, Just _) -> GT
                    (Nothing, Nothing) -> compare i j
            indices = [0 .. n - 1]

        idxs <-
            topoSortBy
                "PhiReorder: cycle in bound dependencies"
                cmpIdx
                depsFor
                indices
        pure [ ids !! i | i <- idxs ]

    reorderTo :: VSpine -> ElabType -> [Maybe NodeId] -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderTo _vs0 ty ids desired = bubbleReorderTo "reorderBindersByPrec" ty ids desired

    -- Interpret witness ops while tracking the current type.
    --
    -- Paper Fig. 10 uses instantiation contexts (C{.}) to reach a binder rather
    -- than swapping quantifiers. Using `InstUnder` keeps binder nesting intact,
    -- which matters for operations like Merge that reference outer binders.
    nodeExists :: NodeId -> Bool
    nodeExists nid =
        case Solved.lookupNode solved (canonicalNode nid) of
            Just _ -> True
            Nothing -> False

    nodeIsBottom :: NodeId -> Bool
    nodeIsBottom nid =
        case Solved.lookupNode solved (canonicalNode nid) of
            Just TyBottom{} -> True
            _ -> False

    isTraceBinderSource :: NodeId -> Bool
    isTraceBinderSource nid =
        IntSet.member (getNodeId nid) traceBinderSources

    replayKeysByName :: Map.Map String [NodeId]
    replayKeysByName =
        let grouped =
                IntMap.foldlWithKey'
                    (\acc key name -> Map.insertWith (++) name [NodeId key] acc)
                    Map.empty
                    (siSubst si)
        in Map.map (sortOn (IB.traceOrderRank ib . getNodeId)) grouped

    sourceNamesForNode :: NodeId -> [String]
    sourceNamesForNode nid =
        dedup $
            [ name
            | sourceKey <- IB.sourceKeysForNode ib nid
            , name <- maybeToList (IntMap.lookup sourceKey traceBinderSourceNames)
            ]
      where
        dedup =
            reverse
                . snd
                . foldl'
                    (\(seen, acc) name ->
                        if Set.member name seen
                            then (seen, acc)
                            else (Set.insert name seen, name : acc)
                    )
                    (Set.empty, [])

    nameReplayCandidatesForNode :: NodeId -> [NodeId]
    nameReplayCandidatesForNode nid =
        dedupNodeIds $
            concat
                [ Map.findWithDefault [] name replayKeysByName
                | name <- sourceNamesForNode nid
                ]

    traceHintMap :: IntMap.IntMap NodeId
    traceHintMap =
        case mTrace of
            Nothing -> IntMap.empty
            Just tr -> etBinderReplayHints tr

    normalizedReplayCandidates :: NodeId -> [NodeId]
    normalizedReplayCandidates rawTarget =
        let rawKey = getNodeId rawTarget
            mapped =
                maybe [] pure (IntMap.lookup rawKey traceBinderReplayMap)
            hinted =
                [ canonicalNode hintedN
                | hintedN <- maybeToList (IntMap.lookup rawKey traceHintMap)
                ]
            named =
                nameReplayCandidatesForNode rawTarget
            sourceEq =
                sourceCandidates rawTarget
            canonicalFallback = [canonicalNode rawTarget]
        in dedupNodeIds (mapped ++ hinted ++ named ++ sourceEq ++ canonicalFallback)

    dedupNodeIds :: [NodeId] -> [NodeId]
    dedupNodeIds =
        reverse
            . snd
            . foldl'
                (\(seen, acc) nid ->
                    let key = getNodeId nid
                    in if IntSet.member key seen
                        then (seen, acc)
                        else (IntSet.insert key seen, nid : acc)
                )
                (IntSet.empty, [])

    sourceCandidates :: NodeId -> [NodeId]
    sourceCandidates nid =
        dedupNodeIds (map NodeId (IB.sourceKeysForNode ib nid))

    pickExistingSource :: [NodeId] -> Maybe NodeId
    pickExistingSource candidates =
        listToMaybe
            [ canonicalNode candidate
            | candidate <- dedupNodeIds candidates
            , nodeExists candidate
            ]

    adoptOpNode :: NodeId -> NodeId
    adoptOpNode nid =
        fromMaybe (canonicalNode nid) (pickExistingSource (sourceCandidates nid))

    graftArgFor :: NodeId -> NodeId -> NodeId
    graftArgFor arg bv =
        fromMaybe (adoptOpNode arg) $
            pickExistingSource (sourceCandidates arg ++ sourceCandidates bv)

    normalizedBinderReplayCandidates :: NodeId -> [NodeId]
    normalizedBinderReplayCandidates rawTarget =
        filter isSchemeBinder (normalizedReplayCandidates rawTarget)

    resolveTraceBinderTarget :: Bool -> String -> NodeId -> Either ElabError NodeId
    resolveTraceBinderTarget requireBinder opName rawTarget
        | IntSet.member rawKey traceBinderSources =
            case (requireBinder, binderCandidates, replayCandidates) of
                (True, replayTarget : _, _) -> Right replayTarget
                (True, [], _) ->
                    Left $
                        PhiInvariantError $
                            unlines
                                [ "trace/replay binder key-space mismatch"
                                , "op: " ++ opName
                                , "source target: " ++ show rawTarget
                                , "trace binder sources: " ++ show (IntSet.toList traceBinderSources)
                                , "replay binder keys: " ++ show (IntMap.keys (siSubst si))
                                , "replay-map domain: " ++ show (IntMap.keys traceBinderReplayMap)
                                ]
                (False, _, replayTarget : _) -> Right replayTarget
                (False, _, []) -> Right rawTarget
        | otherwise = Right rawTarget
      where
        rawKey = getNodeId rawTarget
        replayCandidates = normalizedReplayCandidates rawTarget
        binderCandidates = normalizedBinderReplayCandidates rawTarget

    go :: IntSet.IntSet -> IntSet.IntSet -> IntSet.IntSet -> VSpine -> [Instantiation] -> [InstanceOp] -> (NodeId -> Maybe String)
       -> Either ElabError Instantiation
    go binderKeys keepBinderKeys' namedSet' vs accum ops lookupBinder = case ops of
        [] -> Right (foldl' composeInst InstId (collapseAdjacentPairs (reverse accum)))

        (OpGraft arg bv : rest) -> do
            bvReplay <- resolveTraceBinderTarget True "OpGraft" bv
            let bvC = canonicalNode bvReplay
                rootC = canonicalNode orderRoot
            if bvC == rootC
                then do
                    if vSpineNull vs
                        then do
                            argTy <- reifyTypeArg namedSet' Nothing (graftArgFor arg bv)
                            let inst =
                                    if vsBody vs == BodyBottom
                                        then InstBot argTy
                                        else InstId
                                vs' = if vsBody vs == BodyBottom then vs { vsBody = BodyNonBottom } else vs
                            go binderKeys keepBinderKeys' namedSet' vs' (inst : accum) rest lookupBinder
                        else do
                            argTy <- reifyTypeArg namedSet' Nothing (graftArgFor arg bv)
                            let inst = InstApp argTy
                                vs' = vsDeleteAt 0 vs
                            go binderKeys keepBinderKeys' namedSet' vs' (inst : accum) rest lookupBinder
                else if not (isBinderNode binderKeys bvReplay)
                    then Left $ PhiTranslatabilityError
                        [ "OpGraft targets non-binder node"
                        , "  target node: " ++ show bv
                        , "  canonical: " ++ show bvC
                        ]
                    else do
                        case lookupBinderIndex binderKeys (vSpineIds vs) bvReplay of
                            Nothing -> Left $ PhiTranslatabilityError
                                [ "OpGraft: binder not found in quantifier spine"
                                , "  target node: " ++ show bv
                                , "  canonical: " ++ show bvC
                                ]
                            Just i -> do
                                let mbBound = vSpineBoundAt vs i
                                if mbBound /= Just TBottom && mbBound /= Nothing
                                    then do
                                        argTy <- reifyTypeArg namedSet' Nothing (graftArgFor arg bv)
                                        let boundTy = maybe TBottom tyToElab mbBound
                                        if alphaEqType argTy boundTy
                                            then
                                                -- Bounded-match: bound already equals graft arg, so
                                                -- OpGraft is a no-op. The adjacent OpWeaken will emit
                                                -- InstElim which substitutes the existing bound (thesis Def. 14.2.1).
                                                go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                                            else if argTy == TBottom
                                                then
                                                    -- Bottom arg on bounded binder: no-op (OpWeaken will emit InstElim)
                                                    go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                                            else
                                                Left $
                                                    PhiTranslatabilityError
                                                        [ "OpGraft requires target binder to be unbounded/⊥-bounded or match its explicit bound"
                                                        , "  target node: " ++ show bv
                                                        , "  canonical: " ++ show bvC
                                                        , "  binder bound: " ++ show mbBound
                                                        , "  graft arg: " ++ show argTy
                                                        ]
                                    else do
                                        i' <- binderIndex binderKeys (vSpineIds vs) bvReplay
                                        argTy <- reifyTypeArg namedSet' (Just bvReplay) (graftArgFor arg bv)
                                        prefix <- prefixBinderNames vs i'
                                        let inst = underContext prefix (InstInside (InstBot argTy))
                                            newBound = either (const Nothing) Just (elabToBound argTy)
                                            vs' = vsUpdateBound i' newBound vs
                                        go binderKeys keepBinderKeys' namedSet' vs' (inst : accum) rest lookupBinder

        (OpWeaken bv : rest) -> do
            bvReplay <- resolveTraceBinderTarget False "OpWeaken" bv
            let bvC = canonicalNode bvReplay
                rootC = canonicalNode orderRoot
            if bvC == rootC
                then go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                else if not (isBinderNode binderKeys bvReplay)
                    -- Structurally-bounded binder solved away during constraint
                    -- solving: the quantifier no longer exists in the replay type,
                    -- so OpWeaken is vacuously satisfied (no-op).
                    then go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                    else do
                        let key = getNodeId bvC
                        if IntSet.member key keepBinderKeys'
                            then go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                            else do
                                case lookupBinderIndex binderKeys (vSpineIds vs) bvReplay of
                                    Nothing ->
                                        go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                                    Just idx -> do
                                        -- Thesis-exact OpWeaken: always emit InstElim.
                                        -- For graft+weaken pairs, collapseAdjacentPairs merges
                                        -- the preceding InstInside(InstBot t) with this InstElim
                                        -- into InstApp t (thesis Def. 14.2.1).
                                        (inst, vs') <- atBinderWith False binderKeys vs bvReplay (pure InstElim)
                                        go binderKeys keepBinderKeys' namedSet' vs' (inst : accum) rest lookupBinder

        (OpRaise n : rest) -> do
            nReplay <- resolveTraceBinderTarget False "OpRaise" n
            let nSource = n
                nCandidates =
                    dedupNodeIds
                        (canonicalNode nReplay : sourceCandidates nReplay)
                nExisting =
                    [ canonicalNode candidate
                    | candidate <- nCandidates
                    , nodeExists candidate
                    ]
                nNonBottom =
                    [ candidate
                    | candidate <- nExisting
                    , not (nodeIsBottom candidate)
                    ]
            nAdopt <-
                case nNonBottom of
                    chosen : _ -> pure chosen
                    [] ->
                        if isTraceBinderSource nSource
                            then
                                Left $
                                    PhiInvariantError $
                                        unlines
                                            [ "OpRaise replay target has only dead/bottom aliases"
                                            , "  edge: " ++ show edgeLeft ++ " <= " ++ show edgeRight
                                            , "  source target: " ++ show nSource
                                            , "  replay target: " ++ show nReplay
                                            , "  source candidates: " ++ show nCandidates
                                            , "  existing candidates: " ++ show nExisting
                                            , "  replay-map domain: " ++ show (IntMap.keys traceBinderReplayMap)
                                            , "  hint domain: " ++ show (IntSet.toList traceBinderHintDomain)
                                            ]
                            else pure (fromMaybe (adoptOpNode nReplay) (listToMaybe nExisting))
            let nOrig = canonicalNode nAdopt
            case debugPhi
                ("OpRaise: nSource="
                    ++ show nSource
                    ++ " nReplay="
                    ++ show nReplay
                    ++ " nAdopt="
                    ++ show nAdopt
                    ++ " nOrig="
                    ++ show nOrig
                )
                () of
                () -> pure ()
            raiseTarget <-
                case Solved.lookupNode solved nOrig of
                    Just TyForall{ tnBody = body } -> do
                        binders <- bindingToElab (Binding.orderedBinders canonicalNode (Solved.solvedConstraint solved) (typeRef nOrig))
                        let bodyC = canonicalNode body
                        pure $ case binders of
                            (b:_) -> canonicalNode b
                            [] -> bodyC
                    _ -> pure nOrig
            let nC = raiseTarget
            case debugPhi ("OpRaise: raiseTarget=" ++ show nC) () of
                () -> pure ()
            case debugPhi ("OpRaise: parent=" ++ show (Solved.lookupBindParent solved (typeRef nC))) () of
                () -> pure ()
            nContextTarget <-
                case Solved.lookupNode solved nC of
                    Just TyExp{ tnBody = body } -> pure (canonicalNode body)
                    _ -> pure nC
            let shouldRigidSkip =
                    case Solved.lookupBindParent solved (typeRef nC) of
                        Just (_, BindRigid) -> True
                        _ -> False
            if shouldRigidSkip
                then
                    case debugPhi ("OpRaise: rigid skip target=" ++ show nC) () of
                        () -> go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                else do
                    continueRaise
                        binderKeys
                        keepBinderKeys'
                        namedSet'
                        vs
                        accum
                        rest
                        lookupBinder
                        nSource
                        nAdopt
                        nOrig
                        nC
                        nContextTarget

        (OpMerge n m : rest) -> do
            nReplay <- resolveTraceBinderTarget True "OpMerge(n)" n
            mReplay <- resolveTraceBinderTarget True "OpMerge(m)" m
            if isRigidNode nReplay
                then
                    go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                else if isRigidNode mReplay
                    then Left $ PhiTranslatabilityError
                        [ "OpMerge: rigid endpoint appears only on non-operated node"
                        , "  operated node n: " ++ show n
                        , "  other endpoint m: " ++ show m
                        ]
                else if not (isBinderNode binderKeys nReplay)
                    then Left $ PhiTranslatabilityError
                        [ "OpMerge: first target is non-binder node"
                        , "  target node: " ++ show n
                        , "  canonical: " ++ show (canonicalNode nReplay)
                        ]
                else if not (isBinderNode binderKeys mReplay)
                    then Left $ PhiTranslatabilityError
                        [ "OpMerge: second target is non-binder node"
                        , "  target node: " ++ show m
                        , "  canonical: " ++ show (canonicalNode mReplay)
                        ]
                else if canonicalNode nReplay == canonicalNode mReplay
                    then go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                else do
                    mName <- binderNameFor binderKeys vs mReplay lookupBinder
                    let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                    (inst, vs') <- atBinderWith False binderKeys vs nReplay (pure hAbs)
                    go binderKeys keepBinderKeys' namedSet' vs' (inst : accum) rest lookupBinder

        (OpRaiseMerge n m : rest) -> do
            nReplay <- resolveTraceBinderTarget True "OpRaiseMerge(n)" n
            mReplay <- resolveTraceBinderTarget True "OpRaiseMerge(m)" m
            -- Paper Fig. 15.3.4: rigid-node identity is conditioned on the operated node n.
            if isRigidNode nReplay
                then go binderKeys keepBinderKeys' namedSet' vs accum rest lookupBinder
                else if isRigidNode mReplay
                    then Left $ PhiTranslatabilityError
                        [ "OpRaiseMerge: rigid endpoint appears only on non-operated node"
                        , "  operated node n: " ++ show n
                        , "  other endpoint m: " ++ show m
                        ]
                else if not (isBinderNode binderKeys nReplay)
                    then Left $ PhiTranslatabilityError
                        [ "OpRaiseMerge: first target is non-binder node"
                        , "  target node: " ++ show n
                        , "  canonical: " ++ show (canonicalNode nReplay)
                        ]
                    else if not (isBinderNode binderKeys mReplay)
                        then Left $ PhiTranslatabilityError
                            [ "OpRaiseMerge: second target is non-binder node"
                            , "  target node: " ++ show m
                            , "  canonical: " ++ show (canonicalNode mReplay)
                            ]
                        else do
                            let nC = canonicalNode nReplay
                                rC = canonicalNode orderRoot
                            if nC == rC
                                then do
                                    mName <- binderNameFor binderKeys vs mReplay lookupBinder
                                    let vs' = VSpine [] BodyNonBottom
                                    go binderKeys keepBinderKeys' namedSet' vs' (InstAbstr mName : accum) rest lookupBinder
                                else do
                                    case lookupBinderIndex binderKeys (vSpineIds vs) nReplay of
                                        Nothing ->
                                            Left (PhiTranslatabilityError ["OpRaiseMerge: binder " ++ show n ++ " not found in quantifier spine"])
                                        Just _ -> do
                                            mName <- binderNameFor binderKeys vs mReplay lookupBinder
                                            let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                                            (inst, vs') <- atBinderWith False binderKeys vs nReplay (pure hAbs)
                                            go binderKeys keepBinderKeys' namedSet' vs' (inst : accum) rest lookupBinder

    continueRaise
        :: IntSet.IntSet
        -> IntSet.IntSet
        -> IntSet.IntSet
        -> VSpine
        -> [Instantiation]
        -> [InstanceOp]
        -> (NodeId -> Maybe String)
        -> NodeId
        -> NodeId
        -> NodeId
        -> NodeId
        -> NodeId
        -> Either ElabError Instantiation
    continueRaise
        binderKeys
        keepBinderKeys'
        namedSet'
        vs
        accum
        rest
        lookupBinder
        nSource
        nAdopt
        nOrig
        nC
        nContextTarget = do
        let outsideInterior =
                not (IntSet.null interiorSet)
                    && IntSet.null
                        (IntSet.intersection interiorSet $
                            IntSet.fromList
                                [ getNodeId nSource
                                , getNodeId (canonicalNode nSource)
                                , getNodeId nAdopt
                                , getNodeId nOrig
                                , getNodeId nC
                                ])
            allowOutsideAlias =
                isTraceBinderSource nSource
                    || any
                        (\candidate ->
                            IntSet.member
                                (getNodeId (canonicalNode candidate))
                                interiorSet
                        )
                        (sourceCandidates nSource)
        if outsideInterior && not allowOutsideAlias
            then Left $ PhiTranslatabilityError
                [ "OpRaise target outside I(r)"
                , "edge: " ++ show edgeLeft ++ " <= " ++ show edgeRight
                , "op: OpRaise " ++ show nSource
                , "nSource=" ++ show nSource ++ ", nAdopt=" ++ show nAdopt ++ ", nOrig=" ++ show nOrig ++ ", nC=" ++ show nC
                , "interiorSet=" ++ show (IntSet.toList interiorSet)
                ]
            else
                -- Paper Fig. 10: Raise(n) introduces a fresh quantifier one level higher,
                -- bounds it by Txi(n), then aliases/eliminates the old binder.
                --
                -- For spine binders: use the existing logic
                -- For non-spine nodes: use binding edges + prec ordering to compute context
                let mbIndex = lookupBinderIndex binderKeys (vSpineIds vs) nC
                in case debugPhi ("OpRaise: binderIndex=" ++ show mbIndex) mbIndex of
                    Just i -> do
                        -- Spine binder case
                        when (i < 0 || i >= vSpineLength vs) $
                            Left (PhiInvariantError "OpRaise: binder index out of range")

                        let names = vSpineNames vs
                            mbBound = vSpineBoundAt vs i
                            boundName = vSpineNameAt vs i
                            inferredMap = inferredArgMap namedSet'
                            inferredBound =
                                Map.lookup boundName inferredMap
                                    <|> case Map.elems inferredMap of
                                        [singleTy] -> Just singleTy
                                        _ -> Nothing
                            boundTyRaw =
                                case mbBound of
                                    Just bnd ->
                                        let bTy = tyToElab bnd
                                        in if alphaEqType bTy TBottom
                                            then fromMaybe bTy inferredBound
                                            else bTy
                                    Nothing -> fromMaybe TBottom inferredBound
                            boundTy =
                                case (mbBound, boundTyRaw) of
                                    (Nothing, TVar{}) -> boundTyRaw
                                    (Nothing, _) -> inlineAliasBounds boundTyRaw
                                    _ -> inlineAliasBoundsAsBound boundTyRaw
                            deps = filter (/= boundName) (freeTypeVarsList boundTy)
                            depIdxs = mapMaybe (`elemIndex` names) deps
                            cutoff = if null depIdxs then (-1) else maximum depIdxs
                            insertIndex = cutoff + 1

                        when (insertIndex > i) $
                            Left (PhiInvariantError "OpRaise: computed insertion point is after binder")

                        let prefixBefore = take insertIndex names
                            between = take (i - insertIndex) (drop insertIndex names)
                            hAbsBeta = InstSeq (InstInside (InstAbstr "β")) InstElim
                            aliasOld = underContext between hAbsBeta

                            local =
                                instMany
                                    [ InstIntro
                                    , InstInside (InstBot boundTy)
                                    , InstUnder "β" aliasOld
                                    ]

                            inst = underContext prefixBefore local

                        let vsNoN = vsDeleteAt i vs
                            newBound = either (const Nothing) Just (elabToBound boundTy)
                            vs' = vsInsertAt insertIndex (vSpineNameAt vs i, newBound, Just nC) vsNoN
                        go binderKeys keepBinderKeys' namedSet' vs' (inst : accum) rest lookupBinder

                    Nothing -> do
                        -- Non-spine node case: select an insertion point `m = min-prec{...}` (Fig. 10)
                        -- using the edge-local prec ordering, then insert a fresh quantifier bounded
                        -- by `Txi(n)` at that point, and then alias/eliminate the original
                        -- (nested) binder for `n` inside the chosen `m`'s bound.
                        --
                        -- Paper Fig. 10:
                        --   Phi_xi(Raise(n)) = C^r_m { O; forall(>= Txi(n)); forall(beta_n >=) C^m_n {h!beta_n i} }
                        -- where `m = min-prec{...}`.

                        nodeTy0 <-
                            case Solved.lookupBindParent solved (typeRef nC) of
                                Just (TypeRef parent, _) ->
                                    case Solved.lookupNode solved (canonicalNode parent) of
                                        Just TyForall{} -> reifyTypeWithNamedSetNoFallback (Solved.toSolveResult solved) substForTypes namedSet' nC
                                        _ -> reifyBoundType nC
                                _ -> reifyBoundType nC
                        let nodeTy = applyInferredArgs namedSet' (inlineAliasBounds nodeTy0)
                        nodeTyBound <-
                            case Solved.lookupVarBound solved (canonicalNode nC) of
                                Just bnd -> reifyTypeWithNamedSetNoFallback (Solved.toSolveResult solved) substForTypes namedSet' bnd
                                Nothing -> pure nodeTy
                        let nodeTyBound' = inlineAliasBounds nodeTyBound

                        _ <- pure $ debugPhi ("OpRaise: nodeTy=" ++ show nodeTy) ()
                        _ <- pure $ debugPhi ("OpRaise: nodeTyBound=" ++ show nodeTyBound) ()
                        _ <- pure $ debugPhi ("OpRaise: inferredArgMap=" ++ show (inferredArgMap namedSet')) ()
                        _ <- pure $ debugPhi ("OpRaise: traceArgs=" ++ show (fmap etBinderArgs mTrace)) ()

                        let ids = vSpineIds vs
                            names = vSpineNames vs

                        -- Compute dependency cutoff: the new binder must be inserted after any
                        -- binder that appears free in `Txi(n)`.
                        let deps = freeTypeVarsList nodeTy
                            depIdxs = mapMaybe (`elemIndex` names) deps
                            cutoff = if null depIdxs then (-1) else maximum depIdxs
                            minIdx = min (cutoff + 1) (vSpineLength vs)

                            findCandidate :: [Int] -> Either ElabError (Maybe (Int, [ContextStep]))
                            findCandidate [] = Right Nothing
                            findCandidate (i : is) =
                                case ids !! i of
                                    Nothing -> findCandidate is
                                    Just mNode -> do
                                        ctxOrErr <-
                                            contextToNodeBoundWithOrderKeys
                                                canonicalNode
                                                orderKeys
                                                (Solved.solvedConstraint solved)
                                                namedSet'
                                                (canonicalNode mNode)
                                                nContextTarget
                                        case ctxOrErr of
                                            Nothing -> findCandidate is
                                            Just ctx' -> Right (Just (i, ctx'))

                        mbCandidate <- findCandidate [minIdx .. length ids - 1]
                        rootCtx <-
                            contextToNodeBoundWithOrderKeys
                                canonicalNode
                                orderKeys
                                (Solved.solvedConstraint solved)
                                namedSet'
                                (canonicalNode orderRoot)
                                nContextTarget
                        let boundTyBot = inlineAliasBoundsAsBound nodeTy
                        let mbRootInst =
                                case (rootCtx, Solved.lookupBindParent solved (typeRef nC)) of
                                    (Just _, Just (TypeRef parent, _)) ->
                                        let parentC = canonicalNode parent
                                            rootC = canonicalNode orderRoot
                                        in if parentC == rootC
                                            || case Solved.lookupNode solved parentC of
                                                Just TyForall{} -> True
                                                _ -> False
                                            then
                                                let nodeTyBoundInlined = inlineBaseBounds nodeTyBound'
                                                    numToDelete =
                                                        case mSchemeInfo of
                                                            Just si' ->
                                                                case inferInstAppArgs (siScheme si') nodeTyBoundInlined of
                                                                    Just args
                                                                        | not (null args) -> length args
                                                                    _ -> 1
                                                            Nothing -> 1
                                                    instArgInst =
                                                        case mSchemeInfo of
                                                            Just si' ->
                                                                case inferInstAppArgs (siScheme si') nodeTyBoundInlined of
                                                                    Just args
                                                                        | not (null args) ->
                                                                            instMany (map InstApp args)
                                                                    _ -> InstApp nodeTyBoundInlined
                                                            Nothing -> InstApp nodeTyBoundInlined
                                                    prefixBefore = take minIdx names
                                                    inst = underContext prefixBefore instArgInst
                                                in Just (inst, numToDelete)
                                            else Nothing
                                    _ -> Nothing
                        case mbCandidate of
                            Just (insertIdx, ctxMn) -> do
                                let prefixBefore = take insertIdx names
                                    aliasOld = applyContext ctxMn InstElim

                                    local =
                                        instMany
                                            [ InstIntro
                                            , InstInside (InstBot boundTyBot)
                                            , InstUnder "β" aliasOld
                                            ]

                                    inst = underContext prefixBefore local

                                let newBound = either (const Nothing) Just (elabToBound boundTyBot)
                                    vs' = vsInsertAt insertIdx ("β", newBound, Just nC) vs
                                go binderKeys keepBinderKeys' namedSet' vs' (inst : accum) rest lookupBinder
                            Nothing ->
                                case mbRootInst of
                                    Just (inst, numToDelete) -> do
                                        let vs' = foldl' (\v _ -> vsDeleteAt minIdx v) vs [1..numToDelete]
                                        go binderKeys keepBinderKeys' namedSet' vs' (inst : accum) rest lookupBinder
                                    Nothing ->
                                        Left $
                                            PhiTranslatabilityError
                                                [ "OpRaise (non-spine): missing computation context"
                                                , "  target node: " ++ show nOrig
                                                , "  canonical target: " ++ show nC
                                                , "  context target: " ++ show nContextTarget
                                                , "  orderRoot: " ++ show orderRoot
                                                , "  edgeRoot: " ++ show edgeRoot
                                                , "  minIdx: " ++ show minIdx
                                                , "  deps(Txi(n)): " ++ show deps
                                                , "  nodeTy: " ++ show nodeTy
                                                , "  ids: " ++ show ids
                                                , "  bindParent: " ++ show (Solved.lookupBindParent solved (typeRef nC))
                                                ]

    idsForStartType :: SchemeInfo -> ElabType -> [Maybe NodeId]
    idsForStartType si' ty =
        let nameToId =
                Map.fromList
                    [ (nm, NodeId k)
                    | (k, nm) <- IntMap.toList (siSubst si')
                    ]
            (qs, _) = splitForalls ty
        in [ case Map.lookup nm nameToId of
                Just nid -> Just nid
                Nothing -> parseBinderId nm
           | (nm, _) <- qs
           ]

    parseBinderId :: String -> Maybe NodeId
    parseBinderId ('t':rest) = NodeId <$> readMaybe rest
    parseBinderId _ = Nothing

    binderNameFor :: IntSet.IntSet -> VSpine -> NodeId -> (NodeId -> Maybe String) -> Either ElabError String
    binderNameFor binderKeys vs nid lookupBinder =
        case lookupBinderIndex binderKeys (vSpineIds vs) nid of
            Just i
                | i >= vSpineLength vs ->
                    Left (PhiInvariantError "binderNameFor: index out of range")
                | otherwise -> Right (vSpineNameAt vs i)
            Nothing ->
                Right (fromMaybe ("t" ++ show (getNodeId nid)) (lookupBinder nid))

    atBinderWith :: Bool -> IntSet.IntSet -> VSpine -> NodeId -> Either ElabError Instantiation
                 -> Either ElabError (Instantiation, VSpine)
    atBinderWith keep binderKeys vs nid mkInner = do
        i <- binderIndex binderKeys (vSpineIds vs) nid
        prefix <- prefixBinderNames vs i
        inner <- mkInner
        let vs' = if keep then vs else vsDeleteAt i vs
        pure (underContext prefix inner, vs')

    isBinderNode :: IntSet.IntSet -> NodeId -> Bool
    isBinderNode binderKeys nid = IB.isBinderNode ib binderKeys nid

    -- | Check if a node is bound rigidly. Some ω operations treat rigid targets as
    -- ε/identity (thesis Fig. 15.3.4), but not all (see the OpGraft/OpWeaken note).
    isRigidNode :: NodeId -> Bool
    isRigidNode nid =
        case Solved.lookupBindParent solved (typeRef (canonicalNode nid)) of
            Just (_, BindRigid) -> True
            _ -> False

    fallbackBinderCandidates :: NodeId -> [NodeId]
    fallbackBinderCandidates nid =
        dedupNodeIds
            ( filter isSchemeBinder (normalizedReplayCandidates nid)
                ++ nameReplayCandidatesForNode nid
                ++ filter isSchemeBinder (sourceCandidates nid)
            )

    lookupBinderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Maybe Int
    lookupBinderIndex binderKeys ids nid =
        case IB.lookupBinderIndex ib binderKeys ids nid of
            Just i -> Just i
            Nothing ->
                listToMaybe
                    [ i
                    | candidate <- fallbackBinderCandidates nid
                    , Just i <- [IB.lookupBinderIndex ib binderKeys ids candidate]
                    ]

    binderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Either ElabError Int
    binderIndex binderKeys ids nid =
        case lookupBinderIndex binderKeys ids nid of
            Just i -> Right i
            Nothing ->
                Left $
                    PhiInvariantError $
                        "binder " ++ show nid ++ " not found in identity list " ++ show ids

    prefixBinderNames :: VSpine -> Int -> Either ElabError [String]
    prefixBinderNames vs i
        | i < 0 || i > length names =
            Left (PhiInvariantError "prefixBinderNames: index out of range")
        | otherwise = Right (take i names)
      where
        names = vSpineNames vs

    underContext :: [String] -> Instantiation -> Instantiation
    underContext prefix inner = foldr InstUnder inner prefix

-- | Normalize an instantiation by collapsing redundant sequences.
-- Extracted as a top-level function for testability.
normalizeInst :: Instantiation -> Instantiation
normalizeInst = cata alg
  where
    instArgTy :: Instantiation -> Maybe ElabType
    instArgTy inst0 = case inst0 of
        InstInside (InstBot t) -> Just t
        InstApp t -> Just t
        _ -> Nothing

    alg inst = case inst of
        InstSeqF a b ->
            case (a, b) of
                -- Rule 1: Thesis 14.2.1 identity — InstApp t ≡ InstSeq (InstInside (InstBot t)) InstElim
                (InstInside (InstBot t), InstElim) -> InstApp t
                -- Rule 1b: Context-wrapped graft+weaken — same collapse under matching InstUnder
                (InstUnder v1 a, InstUnder v2 b)
                    | v1 == v2 ->
                        let inner = case (a, b) of
                                (InstInside (InstBot t), InstElim) -> InstApp t
                                _ -> InstSeq a b
                        in InstUnder v1 inner
                -- Rule 2: Structural intro-elim cancellation with matching binder names.
                -- The intro/under/abstr/elim sequence is an identity when the binder
                -- names match, so the whole sequence collapses to InstApp t.
                ( InstSeq InstIntro (InstSeq (InstInside (InstBot t)) (InstUnder beta (InstSeq (InstInside (InstAbstr beta')) InstElim)))
                    , InstElim
                    )
                        | beta == beta' ->
                            InstApp t
                -- Rule 3: Prefix-arg collapse. When a prefix instantiation carries the
                -- same arg type as the inner app, the prefix is redundant and the whole
                -- sequence reduces to InstApp tArg.
                -- REVIEW: This relies on alpha-equality of arg types as a proxy for
                -- semantic equivalence. Sound when the prefix and inner app originate
                -- from the same constraint-graph edge (guaranteed by Phi translation),
                -- but could over-collapse if two independent instantiation paths happen
                -- to share the same arg type. Audit if Phi translation changes.
                ( InstSeq
                    prefix
                    (InstSeq InstIntro (InstSeq appArg (InstUnder beta (InstSeq (InstInside (InstAbstr beta')) InstElim))))
                    , InstElim
                    )
                        | beta == beta'
                        , Just tPrefix <- instArgTy prefix
                        , Just tArg <- instArgTy appArg
                        , alphaEqType tPrefix tArg ->
                            InstApp tArg
                (InstId, x) -> x
                (x, InstId) -> x
                _ -> InstSeq a b
        InstInsideF a -> InstInside a
        InstUnderF v a -> InstUnder v a
        InstAppF t -> InstApp t
        InstBotF t -> InstBot t
        InstAbstrF v -> InstAbstr v
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstIdF -> InstId

-- | Collapse adjacent graft+weaken pairs in a flat instantiation list.
-- When an @InstInside(InstBot t)@ is immediately followed by @InstElim@
-- (possibly wrapped in matching @InstUnder@ contexts), collapse them to
-- @InstApp t@ per thesis Def. 14.2.1.
collapseAdjacentPairs :: [Instantiation] -> [Instantiation]
collapseAdjacentPairs [] = []
collapseAdjacentPairs [x] = [x]
collapseAdjacentPairs (a : b : rest) =
    case tryCollapse a b of
        Just collapsed -> collapseAdjacentPairs (collapsed : rest)
        Nothing -> a : collapseAdjacentPairs (b : rest)

tryCollapse :: Instantiation -> Instantiation -> Maybe Instantiation
tryCollapse (InstInside (InstBot t)) InstElim = Just (InstApp t)
tryCollapse (InstUnder v1 a) (InstUnder v2 b)
    | v1 == v2 = InstUnder v1 <$> tryCollapse a b
tryCollapse _ _ = Nothing
