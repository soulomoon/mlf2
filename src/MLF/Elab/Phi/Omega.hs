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
import Control.Monad (unless, when)
import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (elemIndex, findIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
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
import MLF.Elab.Sigma (bubbleReorderTo)
import MLF.Elab.Types
import MLF.Reify.Core (reifyBoundWithNames, reifyTypeWithNamedSetNoFallback)
import MLF.Reify.TypeOps (freeTypeVarsList, inlineAliasBoundsWithBy, inlineBaseBoundsType, matchType)
import MLF.Util.Graph (topoSortBy)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

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

    invCopyMap :: IntMap.IntMap NodeId
    invCopyMap =
        IntMap.fromList
            [ (getNodeId v, NodeId k)
            | (k, v) <- IntMap.toList copyMap
            ]

    mbGaParents :: Maybe GaBindParents
    mbGaParents = ocGaParents ctx

    mTrace :: Maybe EdgeTrace
    mTrace = ocTrace ctx

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
                case etInterior tr of
                    InteriorNodes s -> s

    orderRoot :: NodeId
    -- Paper root `r` for Phi/Sigma is the expansion root (TyExp body), not the TyExp
    -- wrapper itself. When a trace is available, prefer its root to stay in
    -- the same node space as witness operations.
    orderRoot =
        case mTrace of
            Nothing -> edgeRoot
            Just tr -> etRoot tr

    orderKeys :: IntMap.IntMap Order.OrderKey
    orderKeys = OrderKey.orderKeysFromRootWithExtra canonicalNode nodes extraChildren orderRoot Nothing
      where
        nodes = cNodes (srConstraint res)
        extraChildren nid = boundKids nid ++ bindKids nid
        boundKids nid = case lookupNodeIn nodes nid of
            Just TyVar{ tnBound = Just bnd } -> [bnd]
            _ -> []
        bindKids nid =
            [ canonicalNode child
            | (childKey, (parentRef, flag)) <- IntMap.toList (cBindParents (srConstraint res))
            , flag == BindFlex
            , TypeRef child <- [nodeRefFromKey childKey]
            , parentRefCanon parentRef == TypeRef (canonicalNode nid)
            ]
        parentRefCanon (TypeRef parentN) = TypeRef (canonicalNode parentN)
        parentRefCanon (GenRef gid) = GenRef gid

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

    inferredArgMap :: IntSet.IntSet -> Map.Map String ElabType
    inferredArgMap namedSet' =
        let inferred =
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
        in Map.union (traceArgMap namedSet') inferred

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
            TBaseIF b -> ApplyFun (const (TBase b))
            TBottomIF -> ApplyFun (const TBottom)
            TForallIF v mb body ->
                ApplyFun $ \bound ->
                    let bound' = Set.insert v bound
                        mb' = fmap (\f -> runApplyFun f bound) mb
                    in TForall v mb' (runApplyFun body bound')

    binderArgType :: IntSet.IntSet -> NodeId -> Maybe ElabType
    binderArgType namedSet' binder = do
        name <- IntMap.lookup (getNodeId (canonicalNode binder)) substForTypes
        Map.lookup name (inferredArgMap namedSet')

    reifyTypeArg :: IntSet.IntSet -> Maybe NodeId -> NodeId -> Either ElabError ElabType
    reifyTypeArg namedSet' mbBinder arg =
        case mbBinder >>= binderArgType namedSet' of
            Just ty -> Right ty
            Nothing ->
                case Map.toList (inferredArgMap namedSet') of
                    [(_name, ty)] -> Right (inlineAliasBounds ty)
                    _ -> inlineAliasBounds <$> reifyBoundWithNames res substForTypes arg

    reifyBoundType :: NodeId -> Either ElabError ElabType
    reifyBoundType = reifyBoundWithNames res substForTypes

    reifyTargetTypeForInst :: IntSet.IntSet -> NodeId -> Either ElabError ElabType
    reifyTargetTypeForInst namedSet' nid = do
        let nidC = canonicalNode nid
        ty <- case VarStore.lookupVarBound (srConstraint res) nidC of
            Just bnd -> reifyTypeWithNamedSetNoFallback res substForTypes namedSet' bnd
            Nothing -> reifyTypeWithNamedSetNoFallback res substForTypes namedSet' nidC
        pure (inlineBaseBounds (inlineAliasBounds ty))

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
        -- Always attempt Σ(g) reordering at the start, per thesis Def. 15.3.4
        (sigma, ty1, ids1) <- reorderBindersByPrec ty0 ids0
        (_, _, phiOps) <- goSteps binderKeys keepBinderKeys namedSet ty1 ids1 InstId steps lookupBinder
        pure (normalizeInst (instMany [sigma, phiOps]))

    applyInst :: String -> ElabType -> Instantiation -> Either ElabError ElabType
    applyInst label ty0 inst = case applyInstantiation ty0 inst of
        Left (InstantiationError msg) ->
            Left $ InstantiationError $
                label ++ ": " ++ msg ++ " ; inst=" ++ pretty inst ++ " ; ty=" ++ pretty ty0
        other -> other

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
            Left (InstantiationError "PhiReorder: binder spine / identity list length mismatch")
        if length qs < 2
            then Right (InstId, ty, ids)
            else do
                -- Require concrete binder identities for all quantifiers (fail-fast)
                let missingIdPositions = [i | (i, Nothing) <- zip [(0::Int)..] ids]
                unless (null missingIdPositions) $
                    Left (InstantiationError $ "PhiReorder: missing binder identity at positions " ++ show missingIdPositions)
                -- Require order keys for all binder identities (fail-fast)
                let missingKeyBinders = [nid | Just nid <- ids, not (IntMap.member (getNodeId (canonicalNode nid)) orderKeys)]
                unless (null missingKeyBinders) $
                    Left (InstantiationError $ "PhiReorder: missing order key for binders " ++ show missingKeyBinders)
                desired <- desiredBinderOrder ty ids
                reorderTo ty ids desired

    desiredBinderOrder :: ElabType -> [Maybe NodeId] -> Either ElabError [Maybe NodeId]
    desiredBinderOrder ty ids = do
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
                    (Just a, Just b) ->
                        let ca = canonicalNode a
                            cb = canonicalNode b
                        in case Order.compareNodesByOrderKey orderKeys ca cb of
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
                if not (isBinderNode binderKeys bv)
                    then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                    else do
                        case lookupBinderIndex binderKeys ids bv of
                            Nothing -> go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                            Just i -> do
                                let (qs, _) = splitForalls ty
                                when (length qs /= length ids) $
                                    Left (InstantiationError "OpGraft+OpWeaken: binder spine / identity list length mismatch")
                                let mbBound = snd (qs !! i)
                                if mbBound /= Just TBottom && mbBound /= Nothing
                                    then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                                    else do
                                        (inst, ids1) <- atBinder binderKeys ids ty bv $ do
                                            argTy <- reifyTypeArg namedSet' (Just bv) (graftArgFor arg bv)
                                            pure (InstApp (inlineAliasBoundsAsBound argTy))
                                        ty' <- applyInst "OpGraft+OpWeaken" ty inst
                                        go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder
            | otherwise ->
                Left (InstantiationError "witness op mismatch: OpGraft/OpWeaken refer to different nodes")

        (OpGraft arg bv : rest) -> do
            if not (isBinderNode binderKeys bv)
                then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                else do
                    case lookupBinderIndex binderKeys ids bv of
                        Nothing -> go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                        Just i -> do
                            let (qs, _) = splitForalls ty
                            when (length qs /= length ids) $
                                Left (InstantiationError "OpGraft: binder spine / identity list length mismatch")
                            let mbBound = snd (qs !! i)
                            if mbBound /= Just TBottom && mbBound /= Nothing
                                then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                                else do
                                    (inst, ids1) <- atBinderKeep binderKeys ids ty bv $ do
                                        argTy <- reifyTypeArg namedSet' (Just bv) arg
                                        pure (InstInside (InstBot (inlineAliasBoundsAsBound argTy)))
                                    ty' <- applyInst "OpGraft" ty inst
                                    go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpWeaken bv : rest) -> do
            if not (isBinderNode binderKeys bv)
                then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                else do
                    let key = getNodeId (canonicalNode bv)
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
            if not (IntSet.null interiorSet)
                && not (IntSet.member (getNodeId nOrig) interiorSet)
                && not (IntSet.member (getNodeId nC) interiorSet)
                then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                else
                        -- Paper Fig. 10: operations on rigid nodes translate to the identity
                        -- instantiation (they are inlined and not expressible as xMLF instantiation
                        -- steps). Our witnesses can still contain such ops due to normalization and
                        -- binding-tree harmonization, so treat them as no-ops here.
                        case lookupBindParent (srConstraint res) (typeRef nC) of
                            Just (_, BindRigid) ->
                                go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
                            _ ->
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
                                            Left (InstantiationError "OpRaise: binder spine / identity list length mismatch")
                                        when (i < 0 || i >= length qs) $
                                            Left (InstantiationError "OpRaise: binder index out of range")

                                        let names = map fst qs
                                            mbBound = snd (qs !! i)
                                            boundTy = inlineAliasBoundsAsBound (maybe TBottom tyToElab mbBound)
                                            boundName = names !! i

                                            deps = filter (/= boundName) (freeTypeVarsList boundTy)
                                            depIdxs = mapMaybe (`elemIndex` names) deps
                                            cutoff = if null depIdxs then (-1) else maximum depIdxs
                                            insertIndex = cutoff + 1

                                        when (insertIndex > i) $
                                            Left (InstantiationError "OpRaise: computed insertion point is after binder")

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
                                        let nodeTy = applyInferredArgs namedSet' (inlineAliasBoundsAsBound nodeTy0)
                                        nodeTyBound <-
                                            case VarStore.lookupVarBound (srConstraint res) (canonicalNode nC) of
                                                Just bnd -> reifyTypeWithNamedSetNoFallback res substForTypes namedSet' bnd
                                                Nothing -> pure nodeTy
                                        let nodeTyBound' = inlineAliasBoundsAsBound nodeTyBound

                                        _ <- pure $ debugPhi ("OpRaise: nodeTy=" ++ show nodeTy ++ " ty=" ++ show ty) ()
                                        _ <- pure $ debugPhi ("OpRaise: nodeTyBound=" ++ show nodeTyBound) ()
                                        _ <- pure $ debugPhi ("OpRaise: inferredArgMap=" ++ show (inferredArgMap namedSet')) ()
                                        _ <- pure $ debugPhi ("OpRaise: traceArgs=" ++ show (fmap etBinderArgs mTrace)) ()

                                        let idsSynced = resyncIds ty ids
                                            (qs, _) = splitForalls ty
                                            names = map fst qs

                                        when (length qs /= length idsSynced) $
                                            Left (InstantiationError "OpRaise (non-spine): binder spine / identity list length mismatch")

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
                                        let fallbackAtTop =
                                                case lookupBindParent (srConstraint res) (typeRef nC) of
                                                    Just (GenRef _, _) -> Just (minIdx, False)
                                                    Just (TypeRef parent, _) ->
                                                        case NodeAccess.lookupNode (srConstraint res) (canonicalNode parent) of
                                                            Just TyForall{} -> Just (minIdx, True)
                                                            _ -> Nothing
                                                    _ -> Nothing
                                        case (mbCandidate, fallbackAtTop) of
                                            (Nothing, Nothing) ->
                                                Left $
                                                    InstantiationError $
                                                        "OpRaise (non-spine): missing context for " ++ show nOrig
                                            (Just (insertIdx, ctxMn), _) -> do
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
                                            (Nothing, Just (insertIdx, True)) -> do
                                                let nodeTyBoundInlined = inlineBaseBounds nodeTyBound'
                                                    instArgInst =
                                                        case mSchemeInfo of
                                                            Just si' ->
                                                                case inferInstAppArgs (siScheme si') nodeTyBoundInlined of
                                                                    Just args
                                                                        | not (null args) ->
                                                                            instMany (map (InstApp . inlineAliasBoundsAsBound) args)
                                                                    _ -> InstApp nodeTyBoundInlined
                                                            Nothing -> InstApp nodeTyBoundInlined
                                                    prefixBefore = take insertIdx names
                                                    inst = underContext prefixBefore instArgInst
                                                ty' <- applyInst "OpRaise(non-spine)" ty inst
                                                go binderKeys keepBinderKeys' namedSet' ty' idsSynced (composeInst phi inst) rest lookupBinder
                                            (Nothing, Just (insertIdx, False)) -> do
                                                let prefixBefore = take insertIdx names
                                                    local =
                                                        instMany
                                                            [ InstIntro
                                                            , InstInside (InstBot (inlineAliasBoundsAsBound nodeTy))
                                                            ]
                                                    inst = underContext prefixBefore local
                                                ty' <- applyInst "OpRaise(non-spine)" ty inst
                                                ids1 <- insertAt insertIdx (Just nC) idsSynced
                                                let ids2 = resyncIds ty' ids1
                                                go binderKeys keepBinderKeys' namedSet' ty' ids2 (composeInst phi inst) rest lookupBinder

        (OpMerge n m : rest)
            | not (isBinderNode binderKeys n) || not (isBinderNode binderKeys m) ->
                go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
            | canonicalNode n == canonicalNode m ->
                go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
            | otherwise -> do
                mName <- binderNameFor binderKeys ty ids m lookupBinder
                let hAbs = InstSeq (InstInside (InstAbstr mName)) InstElim
                (inst, ids1) <- atBinder binderKeys ids ty n (pure hAbs)
                ty' <- applyInst "OpMerge" ty inst
                go binderKeys keepBinderKeys' namedSet' ty' ids1 (composeInst phi inst) rest lookupBinder

        (OpRaiseMerge n m : rest) -> do
            if not (isBinderNode binderKeys n) || not (isBinderNode binderKeys m)
                then go binderKeys keepBinderKeys' namedSet' ty ids phi rest lookupBinder
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
                                    Left (InstantiationError ("OpRaiseMerge: binder " ++ show n ++ " not found in quantifier spine"))
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
                    Left (InstantiationError "binderNameFor: binder spine / identity list length mismatch")
                | i >= length names ->
                    Left (InstantiationError "binderNameFor: index out of range")
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
    isBinderNode binderKeys nid =
        let key = getNodeId (canonicalNode nid)
        in IntSet.member key binderKeys

    lookupBinderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Maybe Int
    lookupBinderIndex binderKeys ids nid
        | not (isBinderNode binderKeys nid) = Nothing
        | otherwise = findIndex matches ids
      where
        baseKeyFor n =
            let key0 = getNodeId (canonicalNode n)
            in case mbGaParents of
                Just ga -> maybe key0 getNodeId (IntMap.lookup key0 (gaSolvedToBase ga))
                Nothing -> key0
        nC = canonicalNode nid
        key = getNodeId nC
        candidateKeys = IntSet.fromList $
            baseKeyFor nid
            : catMaybes [ baseKeyFor <$> IntMap.lookup key copyMap
                        , baseKeyFor <$> IntMap.lookup key invCopyMap
                        ]
        matches (Just nid') = IntSet.member (baseKeyFor nid') candidateKeys
        matches Nothing = False

    binderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Either ElabError Int
    binderIndex binderKeys ids nid =
        case lookupBinderIndex binderKeys ids nid of
            Just i -> Right i
            Nothing ->
                Left $
                    InstantiationError $
                        "binder " ++ show nid ++ " not found in identity list " ++ show ids

    prefixBinderNames :: ElabType -> [Maybe NodeId] -> Int -> Either ElabError [String]
    prefixBinderNames ty ids i
        | length names /= length ids =
            Left (InstantiationError "prefixBinderNames: binder spine / identity list length mismatch")
        | i < 0 || i > length names =
            Left (InstantiationError "prefixBinderNames: index out of range")
        | otherwise = Right (take i names)
      where
        (qs, _) = splitForalls ty
        names = map fst qs

    underContext :: [String] -> Instantiation -> Instantiation
    underContext prefix inner = foldr InstUnder inner prefix

    deleteAt :: Int -> [a] -> Either ElabError [a]
    deleteAt i xs
        | i < 0 = Left (InstantiationError "deleteAt: negative index")
        | otherwise =
            let (pre, rest) = splitAt i xs
            in case rest of
                [] -> Left (InstantiationError "deleteAt: index out of range")
                (_:rs) -> Right (pre ++ rs)

    insertAt :: Int -> a -> [a] -> Either ElabError [a]
    insertAt i x xs
        | i < 0 = Left (InstantiationError "insertAt: negative index")
        | i > length xs = Left (InstantiationError "insertAt: index out of range")
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
