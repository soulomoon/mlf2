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
    phiWithSchemeOmega
) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when)
import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Text.Read (readMaybe)

import qualified MLF.Util.Order as Order
import qualified MLF.Util.OrderKey as OrderKey
import MLF.Constraint.Types
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (InteriorNodes(..), lookupCopy)
import MLF.Constraint.Solve (SolveResult(..))
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Binding.Tree as Binding
import MLF.Binding.Tree (lookupBindParent)
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Inst (applyInstantiation, composeInst, instMany, schemeToType, splitForalls)
import MLF.Elab.Phi.Context (contextToNodeBoundWithOrderKeys)
import qualified MLF.Elab.Phi.IdentityBridge as IB
import MLF.Elab.Sigma (bubbleReorderTo)
import MLF.Elab.Types
import MLF.Reify.Core (reifyBoundWithNames, reifyTypeWithNamedSetNoFallback)
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarsList, inlineAliasBoundsWithBy, inlineBaseBoundsType, matchType, substTypeCapture)
import MLF.Util.Graph (topoSortBy)
import MLF.Util.Trace (TraceConfig, traceGeneralize)
import MLF.Util.Names (parseNameId)

-- | Shared context for Omega/Step interpretation.
data OmegaContext = OmegaContext
    { ocTraceConfig :: TraceConfig
    , ocResult :: SolveResult
    , ocCanonicalNode :: NodeId -> NodeId
    , ocCopyMap :: IntMap.IntMap NodeId
    , ocGaParents :: Maybe GaBindParents
    , ocTrace :: Maybe EdgeTrace
    , ocSchemeInfo :: Maybe SchemeInfo
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
    -> [InstanceStep]
    -> Either ElabError Instantiation
phiWithSchemeOmega ctx namedSet keepBinderKeys si steps = phiWithScheme
  where
    res :: SolveResult
    res = ocResult ctx

    canonicalNode :: NodeId -> NodeId
    canonicalNode = ocCanonicalNode ctx

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
    ib = IB.mkIdentityBridge canonicalNode mTrace copyMap

    mSchemeInfo :: Maybe SchemeInfo
    mSchemeInfo = ocSchemeInfo ctx

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
                        in case NodeAccess.lookupNode (srConstraint res) nidC of
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

    nodes = cNodes (srConstraint res)

    boundKids nid = case lookupNodeIn nodes nid of
        Just TyVar{ tnBound = Just bnd } -> [bnd]
        _ -> []

    schemeRootGenMap =
        IntMap.fromList
            [ (getNodeId (canonicalNode root), gnId gen)
            | gen <- NodeAccess.allGenNodes (srConstraint res)
            , root <- gnSchemes gen
            ]

    genChildrenMap =
        IntMap.fromListWith (++)
            [ ( getGenNodeId gid
              , [canonicalNode child]
              )
            | (childKey, (parentRef, _flag)) <- IntMap.toList (cBindParents (srConstraint res))
            , GenRef gid <- [parentRef]
            , TypeRef child <- [nodeRefFromKey childKey]
            ]

    bindKids rootC nid =
        let localChildren =
                [ canonicalNode child
                | (childKey, (parentRef, _flag)) <- IntMap.toList (cBindParents (srConstraint res))
                , TypeRef child <- [nodeRefFromKey childKey]
                , parentRefCanon parentRef == TypeRef (canonicalNode nid)
                ]
            genChildren =
                case IntMap.lookup (getNodeId (canonicalNode nid)) schemeRootGenMap of
                    Nothing -> []
                    Just gid -> IntMap.findWithDefault [] (getGenNodeId gid) genChildrenMap
            siblingGenChildren =
                if canonicalNode nid == rootC
                    then case lookupBindParent (srConstraint res) (typeRef (canonicalNode nid)) of
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
                case foldM (Binding.bindingLCA (srConstraint res)) r0 rs of
                    Left _ -> orderRoot
                    Right (TypeRef nid) -> canonicalNode nid
                    Right (GenRef gid) ->
                        fromMaybe orderRoot $ do
                            gen <- NodeAccess.lookupGenNode (srConstraint res) gid
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
            && case NodeAccess.lookupNode (srConstraint res) nidC of
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
                        case VarStore.lookupVarBound (srConstraint res) (canonicalNode arg) of
                            Just bnd -> reifyBoundWithNames res subst bnd
                            Nothing -> reifyTypeWithNamedSetNoFallback res subst namedSet' (canonicalNode arg)
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
        ty <- case VarStore.lookupVarBound (srConstraint res) argC of
            Just bnd -> reifyTypeWithNamedSetNoFallback res substForTypes namedSet' bnd
            Nothing -> reifyTypeWithNamedSetNoFallback res substForTypes namedSet' argC
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
            )
            (pure chosenTy)

    rescueBottomAtBinder :: NodeId -> ElabType -> ElabType
    rescueBottomAtBinder binder argTy =
        case argTy of
            TBottom ->
                case IntMap.lookup (getNodeId (canonicalNode binder)) substForTypes of
                    Just binderName -> TVar binderName
                    Nothing -> argTy
            _ -> argTy

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
    reifyBoundType = reifyBoundWithNames res substForTypes

    reifyTargetTypeForInst :: IntSet.IntSet -> NodeId -> Either ElabError ElabType
    reifyTargetTypeForInst namedSet' nid = do
        let nidC = canonicalNode nid
        ty <- case VarStore.lookupVarBound (srConstraint res) nidC of
            Just bnd -> reifyTypeWithNamedSetNoFallback res substForTypes namedSet' bnd
            Nothing -> reifyTypeWithNamedSetNoFallback res substForTypes namedSet' nidC
        pure (inlineBaseBounds ty)

    inlineBaseBounds :: ElabType -> ElabType
    inlineBaseBounds =
        inlineBaseBoundsType
            (srConstraint res)
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
            (cNodes (srConstraint res))
            (VarStore.lookupVarBound (srConstraint res))
            (reifyBoundWithNames res substForTypes)

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

    -- | Paper Def. 15.3.4 / Fig. 15.3.5: Φ(e) = Σ prefix then Φχe(Ω).
    -- We always attempt binder reordering via Σ(g), independent of whether Ω
    -- contains Raise operations. When no reorder is needed, reorderBindersByPrec
    -- returns InstId.
    phiWithScheme :: Either ElabError Instantiation
    phiWithScheme = do
        let ty0 = schemeToType (siScheme si)
            subst = siSubst si
            lookupBinder (NodeId i) = IntMap.lookup i subst
            ids0 = idsForStartType si ty0
            binderKeys = IntSet.fromList (IntMap.keys subst)
        -- Always attempt Σ(g) / ϕR at the start (thesis Def. 15.3.4), even if Ω has no Raise steps.
        (sigma, ty1, ids1) <- reorderBindersByPrec ty0 ids0
        (_, _, phiOps) <- goSteps binderKeys keepBinderKeys namedSet ty1 ids1 InstId steps lookupBinder
        pure (normalizeInst (instMany [sigma, phiOps]))

    applyInst :: String -> ElabType -> Instantiation -> Either ElabError ElabType
    applyInst label ty0 inst = case applyInstantiation ty0 inst of
        Left (InstantiationError msg) ->
            Left $ PhiInvariantError $
                label ++ ": " ++ msg ++ " ; inst=" ++ pretty inst ++ " ; ty=" ++ pretty ty0
        other -> other

    -- Keep binder identities aligned with the quantifier spine after each
    -- translated operation (thesis Fig. 10 sequencing: Φξ(ω; Ω') = Φξ(ω); Φω(ξ)(Ω')).
    syncIdsAcrossInstantiation :: ElabType -> [Maybe NodeId] -> ElabType -> [Maybe NodeId]
    syncIdsAcrossInstantiation tyBefore idsBefore tyAfter =
        let (qsBefore, _) = splitForalls tyBefore
            namesBefore = map fst qsBefore
            nameToId =
                Map.fromList
                    [ (nm, nid)
                    | (nm, Just nid) <- zip namesBefore idsBefore
                    ]
            (qsAfter, _) = splitForalls tyAfter
            namesAfter = map fst qsAfter
        in [Map.lookup nm nameToId <|> parseBinderId nm | nm <- namesAfter]

    applyInstAndSyncIds
        :: String
        -> ElabType
        -> [Maybe NodeId]
        -> Instantiation
        -> Either ElabError (ElabType, [Maybe NodeId])
    applyInstAndSyncIds label tyBefore idsBefore inst = do
        let (qsBefore, _) = splitForalls tyBefore
        when (length qsBefore /= length idsBefore) $
            Left (PhiInvariantError (label ++ ": binder spine / identity list length mismatch"))
        tyAfter <- applyInst label tyBefore inst
        let idsAfter = syncIdsAcrossInstantiation tyBefore idsBefore tyAfter
        pure (tyAfter, idsAfter)

    goSteps
        :: IntSet.IntSet
        -> IntSet.IntSet
        -> IntSet.IntSet
        -> ElabType
        -> [Maybe NodeId]
        -> Instantiation
        -> [InstanceStep]
        -> (NodeId -> Maybe String)
        -> Either ElabError (ElabType, [Maybe NodeId], Instantiation)
    goSteps binderKeys keepBinderKeys' namedSet' ty ids phi steps' lookupBinder = case steps' of
        [] -> Right (ty, ids, phi)
        StepIntro : rest -> do
            ty' <- applyInst "StepIntro" ty InstIntro
            let ids' = Nothing : ids
            goSteps binderKeys keepBinderKeys' namedSet' ty' ids' (composeInst phi InstIntro) rest lookupBinder
        _ -> do
            let (omegaSteps, rest) = span isOmegaStep steps'
                ops = [op | StepOmega op <- omegaSteps]
            (ty', ids', phi') <- go binderKeys keepBinderKeys' namedSet' ty ids phi ops lookupBinder
            goSteps binderKeys keepBinderKeys' namedSet' ty' ids' phi' rest lookupBinder

    isOmegaStep :: InstanceStep -> Bool
    isOmegaStep StepOmega{} = True
    isOmegaStep _ = False

    reorderBindersByPrec :: ElabType -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderBindersByPrec ty ids = do
        let (qs, _) = splitForalls ty
        when (length qs /= length ids) $
            Left (PhiInvariantError "PhiReorder: binder spine / identity list length mismatch")
        if length qs < 2
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
                desired <- desiredBinderOrder orderKeysForSort ty ids
                reorderTo ty ids desired

    desiredBinderOrder :: IntMap.IntMap Order.OrderKey -> ElabType -> [Maybe NodeId] -> Either ElabError [Maybe NodeId]
    desiredBinderOrder orderKeysActive ty ids = do
        let (qs, _) = splitForalls ty
            names = map fst qs
            bounds = map snd qs
            n = length qs
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

    reorderTo :: ElabType -> [Maybe NodeId] -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderTo = bubbleReorderTo "reorderBindersByPrec"

    -- Interpret witness ops while tracking the current type.
    --
    -- Paper Fig. 10 uses instantiation contexts (C{.}) to reach a binder rather
    -- than swapping quantifiers. Using `InstUnder` keeps binder nesting intact,
    -- which matters for operations like Merge that reference outer binders.
    graftArgFor :: NodeId -> NodeId -> NodeId
    graftArgFor arg bv = fromMaybe arg $ do
        tr <- mTrace
        lookupCopy bv (etCopyMap tr)

    go :: IntSet.IntSet -> IntSet.IntSet -> IntSet.IntSet -> ElabType -> [Maybe NodeId] -> Instantiation -> [InstanceOp] -> (NodeId -> Maybe String)
       -> Either ElabError (ElabType, [Maybe NodeId], Instantiation)
    go binderKeys keepBinderKeys' namedSet' ty ids phi ops lookupBinder = case ops of
        [] -> Right (ty, ids, phi)

        (OpGraft arg bv : OpWeaken bv' : rest)
            | bv == bv' -> do
                let bvC = canonicalNode bv
                    rootC = canonicalNode orderRoot
                -- Note: A binder can be rigid in the *final* presolution constraint
                -- (e.g. due to later rigidification), even though this ω step was
                -- executed while the binder was still instantiable. We must still
                -- translate the operation to an xMLF instantiation here; skipping
                -- would make Φ unsound (see ElaborationSpec “witness instantiation
                -- matches solved edge types …”).
                if bvC == rootC
                    then do
                        let (qs, _) = splitForalls ty
                        if null qs
                            then do
                                argTy <- reifyTypeArg namedSet' Nothing (graftArgFor arg bv)
                                let inst =
                                        if alphaEqType ty TBottom
                                            then InstBot argTy
                                            else InstId
                                ty' <- applyInst "OpGraft+OpWeaken(root,bot)" ty inst
                                go binderKeys keepBinderKeys' namedSet' ty' ids (composeInst phi inst) rest lookupBinder
                            else do
                                argTy <- reifyTypeArg namedSet' Nothing (graftArgFor arg bv)
                                let inst = InstApp argTy
                                (ty', ids') <- applyInstAndSyncIds "OpGraft+OpWeaken(root)" ty ids inst
                                go binderKeys keepBinderKeys' namedSet' ty' ids' (composeInst phi inst) rest lookupBinder
                    else if not (isBinderNode binderKeys bv)
                        then Left $ PhiTranslatabilityError
                            [ "OpGraft+OpWeaken targets non-binder node"
                            , "  target node: " ++ show bv
                            , "  canonical: " ++ show bvC
                            ]
                        else do
                            case lookupBinderIndex binderKeys ids bv of
                                Nothing -> Left $ PhiTranslatabilityError
                                    [ "OpGraft+OpWeaken: binder not found in quantifier spine"
                                    , "  target node: " ++ show bv
                                    , "  canonical: " ++ show bvC
                                    ]
                                Just i -> do
                                    let (qs, _) = splitForalls ty
                                    when (length qs /= length ids) $
                                        Left (PhiInvariantError "OpGraft+OpWeaken: binder spine / identity list length mismatch")
                                    let mbBound = snd (qs !! i)
                                    if mbBound /= Just TBottom && mbBound /= Nothing
                                        then do
                                            argTy <- reifyTypeArg namedSet' Nothing (graftArgFor arg bv)
                                            let boundTy = maybe TBottom tyToElab mbBound
                                            if argTy == TBottom || alphaEqType argTy boundTy
                                                then do
                                                    let chosenArg = if argTy == TBottom then boundTy else argTy
                                                    (inst, ids1) <- atBinder binderKeys ids ty bv (pure (InstApp chosenArg))
                                                    ty' <- applyInst "OpGraft+OpWeaken(bound-match)" ty inst
                                                    go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder
                                                else
                                                    Left $
                                                        PhiTranslatabilityError
                                                            [ "OpGraft+OpWeaken requires target binder to be unbounded/⊥-bounded or match its explicit bound"
                                                            , "  target node: " ++ show bv
                                                            , "  canonical: " ++ show bvC
                                                            , "  binder bound: " ++ show mbBound
                                                            , "  graft arg: " ++ show argTy
                                                            ]
                                        else do
                                            (inst, ids1) <- atBinder binderKeys ids ty bv $ do
                                                argTy0 <- reifyTypeArg namedSet' Nothing (graftArgFor arg bv)
                                                let argTy = rescueBottomAtBinder bv argTy0
                                                pure (InstApp argTy)
                                            ty' <- applyInst "OpGraft+OpWeaken" ty inst
                                            go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder
            | otherwise ->
                Left (PhiTranslatabilityError ["witness op mismatch: OpGraft/OpWeaken refer to different nodes"])

        (OpGraft arg bv : rest) -> do
            let bvC = canonicalNode bv
                rootC = canonicalNode orderRoot
            if bvC == rootC
                then do
                    let (qs, _) = splitForalls ty
                    if null qs
                        then do
                            argTy <- reifyTypeArg namedSet' Nothing (graftArgFor arg bv)
                            let inst =
                                    if alphaEqType ty TBottom
                                        then InstBot argTy
                                        else InstId
                            ty' <- applyInst "OpGraft(root,bot)" ty inst
                            go binderKeys keepBinderKeys' namedSet' ty' ids (composeInst phi inst) rest lookupBinder
                        else do
                            argTy <- reifyTypeArg namedSet' Nothing (graftArgFor arg bv)
                            let inst = InstApp argTy
                            (ty', ids') <- applyInstAndSyncIds "OpGraft(root)" ty ids inst
                            go binderKeys keepBinderKeys' namedSet' ty' ids' (composeInst phi inst) rest lookupBinder
                else if not (isBinderNode binderKeys bv)
                    then Left $ PhiTranslatabilityError
                        [ "OpGraft targets non-binder node"
                        , "  target node: " ++ show bv
                        , "  canonical: " ++ show bvC
                        ]
                    else do
                        case lookupBinderIndex binderKeys ids bv of
                            Nothing -> Left $ PhiTranslatabilityError
                                [ "OpGraft: binder not found in quantifier spine"
                                , "  target node: " ++ show bv
                                , "  canonical: " ++ show bvC
                                ]
                            Just i -> do
                                let (qs, _) = splitForalls ty
                                when (length qs /= length ids) $
                                    Left (PhiInvariantError "OpGraft: binder spine / identity list length mismatch")
                                let mbBound = snd (qs !! i)
                                if mbBound /= Just TBottom && mbBound /= Nothing
                                    then do
                                        argTy <- reifyTypeArg namedSet' Nothing arg
                                        let boundTy = maybe TBottom tyToElab mbBound
                                        if argTy == TBottom || alphaEqType argTy boundTy
                                            then
                                                go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
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
                                        (inst, ids1) <- atBinderKeep binderKeys ids ty bv $ do
                                            argTy <- reifyTypeArg namedSet' Nothing arg
                                            pure (InstInside (InstBot argTy))
                                        ty' <- applyInst "OpGraft" ty inst
                                        go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpWeaken bv : rest) -> do
            let bvC = canonicalNode bv
                rootC = canonicalNode orderRoot
            if bvC == rootC
                then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                else if not (isBinderNode binderKeys bv)
                    then Left $ PhiTranslatabilityError
                        [ "OpWeaken targets non-binder node"
                        , "  target node: " ++ show bv
                        , "  canonical: " ++ show bvC
                        ]
                    else do
                        let key = getNodeId bvC
                        if IntSet.member key keepBinderKeys'
                            then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                            else do
                                (inst, ids1) <- atBinder binderKeys ids ty bv (pure InstElim)
                                ty' <- applyInst "OpWeaken" ty inst
                                go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaise n : rest) -> do
            let nOrig = canonicalNode n
            _ <- pure $ debugPhi ("OpRaise: nOrig=" ++ show nOrig) ()
            raiseTarget <-
                case NodeAccess.lookupNode (srConstraint res) nOrig of
                    Just TyForall{ tnBody = body } -> do
                        binders <- bindingToElab (Binding.orderedBinders canonicalNode (srConstraint res) (typeRef nOrig))
                        let bodyC = canonicalNode body
                        pure $ case binders of
                            (b:_) -> canonicalNode b
                            [] -> bodyC
                    _ -> pure nOrig
            let nC = raiseTarget
            _ <- pure $ debugPhi ("OpRaise: raiseTarget=" ++ show nC) ()
            _ <- pure $ debugPhi ("OpRaise: parent=" ++ show (lookupBindParent (srConstraint res) (typeRef nC))) ()
            nContextTarget <-
                case NodeAccess.lookupNode (srConstraint res) nC of
                    Just TyExp{ tnBody = body } -> pure (canonicalNode body)
                    _ -> pure nC
            -- Thesis Fig. 15.3.4: operations on rigid nodes translate to identity.
            case lookupBindParent (srConstraint res) (typeRef nC) of
                Just (_, BindRigid) ->
                    go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                _ ->
                    if not (IntSet.null interiorSet)
                        && not (IntSet.member (getNodeId nOrig) interiorSet)
                        && not (IntSet.member (getNodeId nC) interiorSet)
                        then Left $ PhiTranslatabilityError
                            [ "OpRaise target outside I(r)"
                            , "edge: " ++ show edgeLeft ++ " <= " ++ show edgeRight
                            , "op: OpRaise " ++ show n
                            , "nOrig=" ++ show nOrig ++ ", nC=" ++ show nC
                            , "interiorSet=" ++ show (IntSet.toList interiorSet)
                            ]
                        else
                            -- Paper Fig. 10: Raise(n) introduces a fresh quantifier one level higher,
                            -- bounds it by Txi(n), then aliases/eliminates the old binder.
                            --
                            -- For spine binders: use the existing logic
                            -- For non-spine nodes: use binding edges + prec ordering to compute context
                            let mbIndex = lookupBinderIndex binderKeys ids nC
                            in case debugPhi ("OpRaise: binderIndex=" ++ show mbIndex) mbIndex of
                                Just i -> do
                                    -- Spine binder case: existing logic
                                    let (qs, _) = splitForalls ty
                                    when (length qs /= length ids) $
                                        Left (PhiInvariantError "OpRaise: binder spine / identity list length mismatch")
                                    when (i < 0 || i >= length qs) $
                                        Left (PhiInvariantError "OpRaise: binder index out of range")

                                    let names = map fst qs
                                        mbBound = snd (qs !! i)
                                        boundTy = inlineAliasBoundsAsBound (maybe TBottom tyToElab mbBound)
                                        boundName = names !! i

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
                                    ty' <- applyInst "OpRaise(spine)" ty inst

                                    idsNoN <- deleteAt i ids
                                    ids1 <- insertAt insertIndex (Just nC) idsNoN
                                    go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder

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
                                        case lookupBindParent (srConstraint res) (typeRef nC) of
                                            Just (TypeRef parent, _) ->
                                                case NodeAccess.lookupNode (srConstraint res) (canonicalNode parent) of
                                                    Just TyForall{} -> reifyTypeWithNamedSetNoFallback res substForTypes namedSet' nC
                                                    _ -> reifyBoundType nC
                                            _ -> reifyBoundType nC
                                    let nodeTy = applyInferredArgs namedSet' (inlineAliasBounds nodeTy0)
                                    nodeTyBound <-
                                        case VarStore.lookupVarBound (srConstraint res) (canonicalNode nC) of
                                            Just bnd -> reifyTypeWithNamedSetNoFallback res substForTypes namedSet' bnd
                                            Nothing -> pure nodeTy
                                    let nodeTyBound' = inlineAliasBounds nodeTyBound

                                    _ <- pure $ debugPhi ("OpRaise: nodeTy=" ++ show nodeTy ++ " ty=" ++ show ty) ()
                                    _ <- pure $ debugPhi ("OpRaise: nodeTyBound=" ++ show nodeTyBound) ()
                                    _ <- pure $ debugPhi ("OpRaise: inferredArgMap=" ++ show (inferredArgMap namedSet')) ()
                                    _ <- pure $ debugPhi ("OpRaise: traceArgs=" ++ show (fmap etBinderArgs mTrace)) ()

                                    let idsSynced = resyncIds ty ids
                                        (qs, _) = splitForalls ty
                                        names = map fst qs

                                    when (length qs /= length idsSynced) $
                                        Left (PhiInvariantError "OpRaise (non-spine): binder spine / identity list length mismatch")

                                    -- Compute dependency cutoff: the new binder must be inserted after any
                                    -- binder that appears free in `Txi(n)`.
                                    let deps = freeTypeVarsList nodeTy
                                        depIdxs = mapMaybe (`elemIndex` names) deps
                                        cutoff = if null depIdxs then (-1) else maximum depIdxs
                                        minIdx = min (cutoff + 1) (length ids)

                                        findCandidate :: [Int] -> Either ElabError (Maybe (Int, [ContextStep]))
                                        findCandidate [] = Right Nothing
                                        findCandidate (i : is) =
                                            case idsSynced !! i of
                                                Nothing -> findCandidate is
                                                Just mNode -> do
                                                    ctxOrErr <-
                                                        contextToNodeBoundWithOrderKeys
                                                            canonicalNode
                                                            orderKeys
                                                            (srConstraint res)
                                                            namedSet'
                                                            (canonicalNode mNode)
                                                            nContextTarget
                                                    case ctxOrErr of
                                                        Nothing -> findCandidate is
                                                        Just ctx' -> Right (Just (i, ctx'))

                                    mbCandidate <- findCandidate [minIdx .. length idsSynced - 1]
                                    rootCtx <-
                                        contextToNodeBoundWithOrderKeys
                                            canonicalNode
                                            orderKeys
                                            (srConstraint res)
                                            namedSet'
                                            (canonicalNode orderRoot)
                                            nContextTarget
                                    let mbRootInst =
                                            case (rootCtx, lookupBindParent (srConstraint res) (typeRef nC)) of
                                                (Just _, Just (TypeRef parent, _)) ->
                                                    let parentC = canonicalNode parent
                                                        rootC = canonicalNode orderRoot
                                                    in if parentC == rootC
                                                        || case NodeAccess.lookupNode (srConstraint res) parentC of
                                                            Just TyForall{} -> True
                                                            _ -> False
                                                        then
                                                            let nodeTyBoundInlined = inlineBaseBounds nodeTyBound'
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
                                                            in Just (inst, idsSynced)
                                                        else Nothing
                                                _ -> Nothing
                                    case mbCandidate of
                                        Just (insertIdx, ctxMn) -> do
                                            let prefixBefore = take insertIdx names
                                                aliasOld = applyContext ctxMn InstElim

                                                local =
                                                    instMany
                                                        [ InstIntro
                                                        , InstInside (InstBot (inlineAliasBoundsAsBound nodeTy))
                                                        , InstUnder "β" aliasOld
                                                        ]

                                                inst = underContext prefixBefore local

                                            ty' <- applyInst "OpRaise(non-spine)" ty inst
                                            ids1 <- insertAt insertIdx (Just nC) idsSynced
                                            let ids2 = resyncIds ty' ids1
                                            go binderKeys keepBinderKeys' namedSet' ty' ids2 (composeInst phi inst) rest lookupBinder
                                        Nothing ->
                                            case mbRootInst of
                                                Just (inst, idsNext) -> do
                                                    (ty', idsAfter) <- applyInstAndSyncIds "OpRaise(non-spine,root)" ty idsNext inst
                                                    go binderKeys keepBinderKeys' namedSet' ty' idsAfter (composeInst phi inst) rest lookupBinder
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
                                                            , "  idsSynced: " ++ show idsSynced
                                                            , "  bindParent: " ++ show (lookupBindParent (srConstraint res) (typeRef nC))
                                                            ]

        (OpMerge n m : rest)
            -- Paper Fig. 15.3.4: rigid-node identity is conditioned on the operated node n.
            | isRigidNode n ->
                go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
            | isRigidNode m ->
                Left $ PhiTranslatabilityError
                    [ "OpMerge: rigid endpoint appears only on non-operated node"
                    , "  operated node n: " ++ show n
                    , "  other endpoint m: " ++ show m
                    ]
            | not (isBinderNode binderKeys n) ->
                Left $ PhiTranslatabilityError
                    [ "OpMerge: first target is non-binder node"
                    , "  target node: " ++ show n
                    , "  canonical: " ++ show (canonicalNode n)
                    ]
            | not (isBinderNode binderKeys m) ->
                Left $ PhiTranslatabilityError
                    [ "OpMerge: second target is non-binder node"
                    , "  target node: " ++ show m
                    , "  canonical: " ++ show (canonicalNode m)
                    ]
            | canonicalNode n == canonicalNode m ->
                go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
            | otherwise -> do
                mName <- binderNameFor binderKeys ty ids m lookupBinder
                let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                (inst, ids1) <- atBinder binderKeys ids ty n (pure hAbs)
                ty' <- applyInst "OpMerge" ty inst
                go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaiseMerge n m : rest) -> do
            -- Paper Fig. 15.3.4: rigid-node identity is conditioned on the operated node n.
            if isRigidNode n
                then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                else if isRigidNode m
                    then Left $ PhiTranslatabilityError
                        [ "OpRaiseMerge: rigid endpoint appears only on non-operated node"
                        , "  operated node n: " ++ show n
                        , "  other endpoint m: " ++ show m
                        ]
                else if not (isBinderNode binderKeys n)
                    then Left $ PhiTranslatabilityError
                        [ "OpRaiseMerge: first target is non-binder node"
                        , "  target node: " ++ show n
                        , "  canonical: " ++ show (canonicalNode n)
                        ]
                    else if not (isBinderNode binderKeys m)
                        then Left $ PhiTranslatabilityError
                            [ "OpRaiseMerge: second target is non-binder node"
                            , "  target node: " ++ show m
                            , "  canonical: " ++ show (canonicalNode m)
                            ]
                        else do
                            -- Paper Fig. 10 special-cases RaiseMerge(r, m) at the (flexible) expansion
                            -- root r as !alpha m. We implement that case precisely: it applies only when
                            -- n is the expansion root (up to union-find).
                            let nC = canonicalNode n
                                rC = canonicalNode orderRoot
                            if nC == rC
                                then do
                                    mName <- binderNameFor binderKeys ty ids m lookupBinder
                                    ty' <- applyInst "OpRaiseMerge(abs)" ty (InstAbstr mName)
                                    go binderKeys keepBinderKeys' namedSet' ty' [] (composeInst phi (InstAbstr mName)) rest lookupBinder
                                else do
                                    -- Non-root RaiseMerge behaves like Merge inside the context of n.
                                    case lookupBinderIndex binderKeys ids n of
                                        Nothing ->
                                            Left (PhiTranslatabilityError ["OpRaiseMerge: binder " ++ show n ++ " not found in quantifier spine"])
                                        Just _ -> do
                                            mName <- binderNameFor binderKeys ty ids m lookupBinder
                                            let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                                            (inst, ids1) <- atBinder binderKeys ids ty n (pure hAbs)
                                            ty' <- applyInst "OpRaiseMerge" ty inst
                                            go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder

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

    resyncIds :: ElabType -> [Maybe NodeId] -> [Maybe NodeId]
    resyncIds ty idsPrev =
        let (qs, _) = splitForalls ty
            names = map fst qs
            nameMap =
                Map.fromList
                    [ (nm, nid)
                    | (Just nid, nm) <- zip idsPrev names
                    ]
            lookupName nm =
                case Map.lookup nm nameMap of
                    Just nid -> Just nid
                    Nothing -> parseBinderId nm
        in map lookupName names

    parseBinderId :: String -> Maybe NodeId
    parseBinderId ('t':rest) = NodeId <$> readMaybe rest
    parseBinderId _ = Nothing

    binderNameFor :: IntSet.IntSet -> ElabType -> [Maybe NodeId] -> NodeId -> (NodeId -> Maybe String) -> Either ElabError String
    binderNameFor binderKeys ty ids nid lookupBinder =
        case lookupBinderIndex binderKeys ids nid of
            Just i
                | length names /= length ids ->
                    Left (PhiInvariantError "binderNameFor: binder spine / identity list length mismatch")
                | i >= length names ->
                    Left (PhiInvariantError "binderNameFor: index out of range")
                | otherwise -> Right (names !! i)
            Nothing ->
                Right (fromMaybe ("t" ++ show (getNodeId nid)) (lookupBinder nid))
      where
        (qs, _) = splitForalls ty
        names = map fst qs

    atBinder :: IntSet.IntSet -> [Maybe NodeId] -> ElabType -> NodeId -> Either ElabError Instantiation
             -> Either ElabError (Instantiation, [Maybe NodeId])
    atBinder binderKeys ids ty nid mkInner = do
        i <- binderIndex binderKeys ids nid
        prefix <- prefixBinderNames ty ids i
        inner <- mkInner
        ids' <- deleteAt i ids
        pure (underContext prefix inner, ids')

    atBinderKeep :: IntSet.IntSet -> [Maybe NodeId] -> ElabType -> NodeId -> Either ElabError Instantiation
                 -> Either ElabError (Instantiation, [Maybe NodeId])
    atBinderKeep binderKeys ids ty nid mkInner = do
        i <- binderIndex binderKeys ids nid
        prefix <- prefixBinderNames ty ids i
        inner <- mkInner
        pure (underContext prefix inner, ids)

    isBinderNode :: IntSet.IntSet -> NodeId -> Bool
    isBinderNode binderKeys nid = IB.isBinderNode ib binderKeys nid

    -- | Check if a node is bound rigidly. Some ω operations treat rigid targets as
    -- ε/identity (thesis Fig. 15.3.4), but not all (see the OpGraft/OpWeaken note).
    isRigidNode :: NodeId -> Bool
    isRigidNode nid =
        case lookupBindParent (srConstraint res) (typeRef (canonicalNode nid)) of
            Just (_, BindRigid) -> True
            _ -> False

    lookupBinderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Maybe Int
    lookupBinderIndex binderKeys ids nid = IB.lookupBinderIndex ib binderKeys ids nid

    binderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Either ElabError Int
    binderIndex binderKeys ids nid =
        case lookupBinderIndex binderKeys ids nid of
            Just i -> Right i
            Nothing ->
                Left $
                    PhiInvariantError $
                        "binder " ++ show nid ++ " not found in identity list " ++ show ids

    prefixBinderNames :: ElabType -> [Maybe NodeId] -> Int -> Either ElabError [String]
    prefixBinderNames ty ids i
        | length names /= length ids =
            Left (PhiInvariantError "prefixBinderNames: binder spine / identity list length mismatch")
        | i < 0 || i > length names =
            Left (PhiInvariantError "prefixBinderNames: index out of range")
        | otherwise = Right (take i names)
      where
        (qs, _) = splitForalls ty
        names = map fst qs

    underContext :: [String] -> Instantiation -> Instantiation
    underContext prefix inner = foldr InstUnder inner prefix

    deleteAt :: Int -> [a] -> Either ElabError [a]
    deleteAt i xs
        | i < 0 = Left (PhiInvariantError "deleteAt: negative index")
        | otherwise =
            let (pre, rest) = splitAt i xs
            in case rest of
                [] -> Left (PhiInvariantError "deleteAt: index out of range")
                (_:rs) -> Right (pre ++ rs)

    insertAt :: Int -> a -> [a] -> Either ElabError [a]
    insertAt i x xs
        | i < 0 = Left (PhiInvariantError "insertAt: negative index")
        | i > length xs = Left (PhiInvariantError "insertAt: index out of range")
        | otherwise =
            let (pre, rest) = splitAt i xs
            in Right (pre ++ (x : rest))

    normalizeInst :: Instantiation -> Instantiation
    normalizeInst = cata alg
      where
        alg inst = case inst of
            InstSeqF a b ->
                case (a, b) of
                    (InstInside (InstBot t), InstElim) -> InstApp t
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
