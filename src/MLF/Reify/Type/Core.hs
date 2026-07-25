{-# LANGUAGE GADTs #-}

module MLF.Reify.Type.Core
  ( ReifyRoot (..),
    reifyWithRefs,
    reifyWithExternalRefs,
    reifyWithOuterBinderRefs,
    reifyWithReadModelRefs,
    reifyWithAsRefs,
  )
where

{- Note [Core reification algorithm — reifyWithRefs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module contains the core graph-to-type reification algorithm, implementing
the walk from constraint-graph nodes to elaborated 'ElabType' terms.

'reifyWithRefs' is the main entry point. Given a 'ReifyRoot' mode, a set of named
nodes, and a starting node, it walks the solved/canonical constraint graph and
produces an 'ElabType'.  The algorithm handles:

  * TyVar → TVar (with optional bound reification)
  * TyArrow → TArrow (recursive descent into domain/codomain)
  * TyForall → TForall (binder name assignment + body reification)
  * TyBase → TBaseWithIdentity
  * TyBottom → TBottom
  * TyExp → body reification (expansion nodes are transparent)
  * TyMu → TMu (recursive type binder)

Binder ordering follows the presolution plan's <P order (thesis §15.3) via
topological sort of flex children.  The 'RootType' vs 'RootTypeNoFallback'
dispatch controls whether missing-node fallbacks are permitted.

Paper references:
  * Yakobowski PhD thesis (2008), Chapter 15 — type reification from solved
    graphic constraints
  * Rémy & Yakobowski (FLOPS 2010) — xMLF type structure
-}

import Control.Monad (foldM, unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (partition)
import qualified Data.List.NonEmpty as NE
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Presolution.View (PresolutionView (..))
import MLF.Constraint.Types.Graph hiding (lookupNode)
import MLF.Elab.ReadModel
  ( ElabReadModel (..),
    buildElabReadModel,
  )
import MLF.Reify.Cache
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError (..))
import MLF.Util.Graph (topoSortBy)
import qualified MLF.Util.Order as Order

data ReifyRoot
  = RootType -- ^ Reify the node itself, preserving a bounded result variable.
  | RootTypeNoFallback -- ^ Reify the node without missing-node fallbacks.
  | RootBound -- ^ Reify the node's lower-bound view.

reifyWithRefs ::
  String ->
  PresolutionView p ->
  (NodeId -> TypeBinderRef) ->
  (NodeId -> Bool) ->
  ReifyRoot ->
  NodeId ->
  Either ElabError ElabType
reifyWithRefs contextLabel presolutionView refForVar isNamed rootMode nid = do
  readModel <- buildElabReadModel presolutionView
  reifyWithReadModelRefs contextLabel readModel refForVar isNamed rootMode nid

-- | Reify with a second, construction-time distinction inside the named set.
-- External names are inherited from the enclosing Gamma. They must be emitted
-- as variables by S', but must never be wrapped as binders in this packet.
reifyWithExternalRefs ::
  String ->
  PresolutionView p ->
  (NodeId -> TypeBinderRef) ->
  (NodeId -> Bool) ->
  (NodeId -> Bool) ->
  IntMap.IntMap [NodeId] ->
  ReifyRoot ->
  NodeId ->
  Either ElabError ElabType
reifyWithExternalRefs contextLabel presolutionView refForVar isNamed isExternal structuralBinders rootMode nid = do
  readModel <- buildElabReadModel presolutionView
  reifyWithReadModelExternalRefs contextLabel readModel refForVar isNamed isExternal (const False) structuralBinders rootMode nid

-- | Reify a type body whose enclosing scheme already owns some binder
-- declarations.  Those identities are emitted only as variable occurrences:
-- even a structural forall carrying the same identity must not construct a
-- second declaration inside the body.
reifyWithOuterBinderRefs ::
  String ->
  PresolutionView p ->
  (NodeId -> TypeBinderRef) ->
  (NodeId -> Bool) ->
  (NodeId -> Bool) ->
  (NodeId -> Bool) ->
  IntMap.IntMap [NodeId] ->
  ReifyRoot ->
  NodeId ->
  Either ElabError ElabType
reifyWithOuterBinderRefs contextLabel presolutionView refForVar isNamed isExternal isOuterOwned structuralBinders rootMode nid = do
  readModel <- buildElabReadModel presolutionView
  reifyWithReadModelExternalRefs contextLabel readModel refForVar isNamed isExternal isOuterOwned structuralBinders rootMode nid

reifyWithReadModelRefs ::
  String ->
  ElabReadModel p ->
  (NodeId -> TypeBinderRef) ->
  (NodeId -> Bool) ->
  ReifyRoot ->
  NodeId ->
  Either ElabError ElabType
reifyWithReadModelRefs contextLabel readModel refForVar isNamed rootMode nid =
  reifyWithReadModelExternalRefs
    contextLabel
    readModel
    refForVar
    isNamed
    -- The legacy entrypoint has only one named-node predicate.  Preserve its
    -- original contract by treating every supplied name as inherited.  The
    -- external-aware entrypoint is what permits named packet-local binders.
    isNamed
    (const False)
    IntMap.empty
    rootMode
    nid

reifyWithReadModelExternalRefs ::
  String ->
  ElabReadModel p ->
  (NodeId -> TypeBinderRef) ->
  (NodeId -> Bool) ->
  (NodeId -> Bool) ->
  (NodeId -> Bool) ->
  IntMap.IntMap [NodeId] ->
  ReifyRoot ->
  NodeId ->
  Either ElabError ElabType
reifyWithReadModelExternalRefs _contextLabel readModel refForVar isNamed isExternal isOuterOwned structuralBinders rootMode nid =
  let start = case rootMode of
        RootType -> goType
        RootTypeNoFallback -> goTypeNoFallback
        RootBound -> goBoundRoot
   in snd <$> start emptyCache (canonical nid)
  where
    presolutionView = ermPresolutionView readModel
    originalConstraint = pvConstraint presolutionView
    canonicalConstraint = pvCanonicalConstraint presolutionView
    nodes = cNodes canonicalConstraint
    canonical = pvCanonical presolutionView
    lookupVarBoundS = pvLookupVarBound presolutionView
    originalGenNodes = cGenNodes originalConstraint
    weakened = cWeakenedVars canonicalConstraint
    isEliminatedVarS queryNid = IntSet.member (getNodeId queryNid) (cEliminatedVars canonicalConstraint)
    schemeRootSet = ermSchemeRootSet readModel
    schemeGenByRoot = ermSchemeGenByRoot readModel
    schemeGenSet = ermSchemeGenSet readModel
    softChildren = ermSoftChildren readModel

    lookupNode k = maybe (Left (MissingNode k)) Right (lookupNodeIn nodes k)

    bindParentsE =
      Right (ermSoftBindParents readModel)

    boundIsSimple start =
      let go visited nid0 =
            let nidC = canonical nid0
                key = getNodeId nidC
             in if IntSet.member key visited
                  then True
                  else case lookupNodeIn nodes nidC of
                    Nothing -> True
                    Just node ->
                      let visited' = IntSet.insert key visited
                       in case node of
                            TyBase {} -> True
                            TyBottom {} -> True
                            TyCon {tnArgs = args} ->
                              all (go visited') (NE.toList args)
                            TyVarApp {tnVarHead = headNode, tnArgs = args} ->
                              all (go visited') (headNode : NE.toList args)
                            TyVar {} ->
                              case lookupVarBoundS nidC of
                                Nothing -> True
                                Just bnd -> go visited' bnd
                            TyExp {tnBody = b} -> go visited' b
                            TyArrow {} -> False
                            TyForall {} -> False
                            TyMu {} -> False
       in go IntSet.empty start

    boundIsSimpleFor n =
      case lookupVarBoundS (canonical n) of
        Nothing -> False
        Just bnd -> boundIsSimple bnd

    canonicalRef = Canonicalize.canonicalRef canonical

    nodeRefExists ref = case ref of
      TypeRef nid0 ->
        case lookupNodeIn nodes nid0 of
          Just _ -> True
          Nothing -> False
      GenRef gid ->
        IntMap.member (getGenNodeId gid) (getGenNodeMap (originalGenNodes))

    lookupBindParentUnderSoft ref0 = do
      bindParents <- bindParentsE
      let refC = canonicalRef ref0
      unless (nodeRefExists refC) $
        Left $
          BindingTreeError $
            InvalidBindingTree $
              "lookupBindParentUnderSoft: node " ++ show refC ++ " not in constraint"
      pure (IntMap.lookup (nodeRefKey refC) bindParents)

    boundFlexChildrenAllUnderSoft binder0 = do
      let binderC = canonicalRef binder0
      unless (nodeRefExists binderC) $
        Left $
          BindingTreeError $
              InvalidBindingTree $
                "boundFlexChildrenAllUnderSoft: binder " ++ show binderC ++ " not in constraint"
      reverse
        <$> foldM
          ( \acc (childKey, flag) ->
              if flag /= BindFlex
                then pure acc
                else
                  let childRef = nodeRefFromKey childKey
                   in case childRef of
                        TypeRef childN ->
                          case lookupNodeIn nodes childN of
                            Just TyExp {} -> pure acc
                            Just TyBase {} -> pure acc
                            Just TyBottom {} -> pure acc
                            Just _ -> pure (childN : acc)
                            Nothing ->
                              Left $
                                BindingTreeError $
                                  InvalidBindingTree $
                                    "boundFlexChildrenAllUnderSoft: child " ++ show childN ++ " not in cNodes"
                        GenRef gid ->
                          if IntMap.member (getGenNodeId gid) (getGenNodeMap (originalGenNodes))
                            then pure acc
                            else
                              Left $
                                BindingTreeError $
                                  InvalidBindingTree $
                                    "boundFlexChildrenAllUnderSoft: child " ++ show gid ++ " not in cGenNodes"
          )
          []
          [ (childKey, flag)
          | (childKey, flag) <- IntMap.findWithDefault [] (nodeRefKey binderC) softChildren
          ]

    varRef n = refForVar (canonical n)
    varFor n = TVarRef (varRef n)

    isNamedLocal namedExtra nodeId =
      let key = getNodeId (canonical nodeId)
       in isNamed nodeId || IntSet.member key namedExtra

    isAlreadyBoundLocal namedExtra nodeId =
      IntSet.member (getNodeId (canonical nodeId)) namedExtra

    isExternalLocal nodeId = isExternal (canonical nodeId)

    isOuterOwnedLocal nodeId = isOuterOwned (canonical nodeId)

    structuralBinderKeys =
      IntSet.fromList
        [ getNodeId (canonical binder)
        | binders <- IntMap.elems structuralBinders
        , binder <- binders
        ]

    structuralBindersForOwner owner =
      IntSet.toList $
        IntSet.fromList
          [ getNodeId (canonical binder)
          | binder <- IntMap.findWithDefault [] (getNodeId (canonical owner)) structuralBinders
          ]

    cacheLookupLocal mode cache key namedExtra =
      if IntSet.null namedExtra
        then cacheLookup mode cache key
        else Nothing

    cacheInsertLocal mode key ty cache namedExtra =
      if IntSet.null namedExtra
        then cacheInsert mode key ty cache
        else cache

    -- RootType denotes the graph node itself.  In particular, a live result
    -- variable stays a variable even when W-normalization marked it weakened;
    -- callers that want its lower bound select RootBound (or the paper-style
    -- no-fallback translation) at construction time instead.
    goType cache n0 =
      let n = canonical n0
       in case lookupNodeIn nodes n of
            Just TyVar {}
              | not (isEliminatedVarS n) ->
                  let ty = varFor n
                   in pure (cacheInsert ModeType (getNodeId n) ty cache, ty)
            _ -> goFull cache IntSet.empty ModeType n
    goTypeNoFallback cache = goFull cache IntSet.empty ModeTypeNoFallback
    goBoundRoot cache = goBound cache IntSet.empty

    goFull cache namedExtra mode n0 =
      let n = canonical n0
          key = getNodeId n
          inProgress = cacheInProgress cache
          markDone cache' = cache' {cacheInProgress = IntSet.delete key (cacheInProgress cache')}
          markStart cache' = cache' {cacheInProgress = IntSet.insert key (cacheInProgress cache')}
       in case cacheLookupLocal mode cache key namedExtra of
            Just t -> Right (cache, t)
            Nothing ->
              if IntSet.member key inProgress
                then Right (cache, varFor n)
                else do
                  node <- lookupNode n
                  let cache0 = markStart cache
                  case node of
                    TyVar {} ->
                      let cache0' = cacheInsertLocal mode key (varFor n) cache0 namedExtra
                       in if isEliminatedVarS n
                            then
                              let t = TBottom
                                  cache' = cacheInsertLocal mode key t cache0' namedExtra
                               in pure (markDone cache', t)
                            else case mode of
                              ModeBound
                                | IntSet.member key schemeRootSet ->
                                    case lookupVarBoundS n of
                                      Nothing -> do
                                        let t = varFor n
                                            cache' = cacheInsertLocal mode key t cache0' namedExtra
                                        pure (markDone cache', t)
                                      Just bnd -> do
                                        let bndC = canonical bnd
                                        (cache1, core) <- goFull cache0' namedExtra ModeBound bndC
                                        binders <- orderedFlexChildren ModeBound namedExtra n
                                        (cache2, t) <- wrapBinders cache1 namedExtra core binders
                                        let cacheFinal = cacheInsertLocal mode key t cache2 namedExtra
                                        pure (markDone cacheFinal, t)
                              ModeBound -> do
                                let t = varFor n
                                    cache' = cacheInsertLocal mode key t cache0' namedExtra
                                pure (markDone cache', t)
                              _ -> do
                                mbParent <- lookupBindParentUnderSoft (typeRef n)
                                let mbSchemeBound =
                                      case lookupVarBoundS (canonical n) of
                                        Just bnd
                                          | IntSet.member (getNodeId (canonical bnd)) schemeRootSet ->
                                              Just (canonical bnd)
                                        _ -> Nothing
                                    boundIsBaseOrBottom =
                                      case lookupVarBoundS n of
                                        Nothing -> False
                                        Just bnd ->
                                          case lookupNodeIn nodes (canonical bnd) of
                                            Just TyBase {} -> True
                                            Just TyBottom {} -> True
                                            _ -> False
                                    boundIsSchemeRootVar =
                                      case lookupVarBoundS (canonical n) of
                                        Just bnd ->
                                          IntSet.member (getNodeId (canonical bnd)) schemeRootSet
                                        Nothing -> False
                                    shouldInlineWeakened =
                                      IntSet.member key weakened
                                        && boundIsBaseOrBottom
                                        && not (isNamedLocal namedExtra (canonical n))
                                isGenBinder <- case mbParent of
                                  Just (GenRef gid, BindFlex) ->
                                    elem (canonical n) <$> boundFlexChildrenAllUnderSoft (genRef gid)
                                  _ -> pure False
                                let shouldInline =
                                      shouldInlineWeakened
                                        || case (mode, mbParent) of
                                          (ModeTypeNoFallback, _)
                                            | boundIsSchemeRootVar
                                                && not (isNamedLocal namedExtra (canonical n)) ->
                                                True
                                          (ModeTypeNoFallback, Just (GenRef _, BindFlex))
                                            | not (isNamedLocal namedExtra (canonical n))
                                                && not isGenBinder
                                                && boundIsSimpleFor n ->
                                                True
                                          (ModeTypeNoFallback, Nothing)
                                            | not (isNamedLocal namedExtra (canonical n)) ->
                                                boundIsSimpleFor n
                                          _ -> False
                                case mbSchemeBound of
                                  Just _ | not (isNamedLocal namedExtra (canonical n)) -> do
                                    (cache', t) <- goBound cache0' namedExtra n
                                    pure (markDone cache', t)
                                  _ -> case mbParent of
                                    Just (TypeRef parent, BindRigid) ->
                                      case lookupNodeIn nodes (canonical parent) of
                                        Just TyForall {} ->
                                          let t = varFor n
                                              cache' = cacheInsertLocal mode key t cache0' namedExtra
                                           in pure (markDone cache', t)
                                        _ ->
                                          if isNamedLocal namedExtra (canonical n) || boundIsPoly n
                                            then
                                              let t = varFor n
                                                  cache' = cacheInsertLocal mode key t cache0' namedExtra
                                               in pure (markDone cache', t)
                                            else do
                                              (cache', t) <- goBound cache0' namedExtra n
                                              pure (markDone cache', t)
                                    Just (GenRef gid, BindRigid) ->
                                      let isSchemeBinder =
                                            IntSet.member (getGenNodeId gid) schemeGenSet
                                       in if isNamedLocal namedExtra (canonical n)
                                            || boundIsPoly n
                                            || isSchemeBinder
                                            then
                                              let t = varFor n
                                                  cache' = cacheInsertLocal mode key t cache0' namedExtra
                                               in pure (markDone cache', t)
                                            else do
                                              (cache', t) <- goBound cache0' namedExtra n
                                              pure (markDone cache', t)
                                    _ ->
                                      if shouldInline
                                        then do
                                          (cache', t) <- goBound cache0' namedExtra n
                                          pure (markDone cache', t)
                                        else
                                          let t = varFor n
                                              cache' = cacheInsertLocal mode key t cache0' namedExtra
                                           in pure (markDone cache', t)
                    _
                      | isNamedLocal namedExtra (canonical n) ->
                          let t = varFor n
                              cache' = cacheInsertLocal mode key t cache0 namedExtra
                           in pure (markDone cache', t)
                      | TyMu {tnBody = b} <- node -> do
                          binders <- orderedFlexChildren mode namedExtra n
                          case binderIdentityGroups binders of
                            [binderGroup@(bndr : _)] -> do
                              let binder = canonical bndr
                                  namedExtra' =
                                    IntSet.union
                                      namedExtra
                                      ( IntSet.fromList
                                          [ getNodeId (canonical binderAlias)
                                          | binderAlias <- binderGroup
                                          ]
                                      )
                              (cache', bodyTy) <- vChild cache0 namedExtra' mode (canonical b)
                              let t = TMuRef (varRef binder) bodyTy
                                  cacheFinal = cacheInsertLocal mode key t cache' namedExtra
                              pure (markDone cacheFinal, t)
                            [] -> do
                              -- Non-local proxy TyMu: no binder child in binding tree.
                              -- Synthesize binder from the TyMu node itself.
                              let synthBinder = n
                                  namedExtra' = IntSet.insert (getNodeId synthBinder) namedExtra
                              (cache', bodyTy) <- vChild cache0 namedExtra' mode (canonical b)
                              let t = TMuRef (varRef synthBinder) bodyTy
                                  cacheFinal = cacheInsertLocal mode key t cache' namedExtra
                              pure (markDone cacheFinal, t)
                            _ ->
                              Left $
                                BindingTreeError $
                                  InvalidBindingTree $
                                    "reifyType: TyMu "
                                      ++ show n
                                      ++ " body="
                                      ++ show (canonical b)
                                      ++ " has multiple binder children "
                                      ++ show
                                        [ ( binder,
                                            lookupNodeIn nodes binder,
                                            IntMap.lookup (nodeRefKey (typeRef binder)) (ermSoftBindParents readModel)
                                          )
                                        | binder <- binders
                                        ]
                      | otherwise -> do
                          binders <- orderedFlexChildren mode namedExtra n
                          let binderKeys =
                                IntSet.fromList
                                  [ getNodeId (canonical b)
                                    | b <- binders
                                  ]
                              namedExtra' = IntSet.union namedExtra binderKeys
                          (cache', core) <- case node of
                            TyBase {tnBaseIdentity = identity, tnBase = b} -> pure (cache0, TBaseWithIdentity identity b)
                            TyBottom {} -> pure (cache0, TBottom)
                            TyArrow {tnDom = d, tnCod = c} -> do
                              (cache1, d') <- vChild cache0 namedExtra' mode (canonical d)
                              (cache2, c') <- vChild cache1 namedExtra' mode (canonical c)
                              pure (cache2, TArrow d' c')
                            TyCon {tnConIdentity = conIdentity, tnCon = con, tnArgs = args} -> do
                              (cache', args') <-
                                foldM
                                  ( \(cacheAcc, acc) arg -> do
                                      (cacheNext, arg') <- vChild cacheAcc namedExtra' mode (canonical arg)
                                      pure (cacheNext, arg' : acc)
                                  )
                                  (cache0, [])
                                  (NE.toList args)
                              pure (cache', TConWithIdentity conIdentity con (NE.fromList (reverse args')))
                            TyVarApp {tnVarHead = headNode, tnArgs = args} -> do
                              (cache1, headTy) <- vChild cache0 namedExtra' mode (canonical headNode)
                              (cache', args') <-
                                foldM
                                  ( \(cacheAcc, acc) arg -> do
                                      (cacheNext, arg') <- vChild cacheAcc namedExtra' mode (canonical arg)
                                      pure (cacheNext, arg' : acc)
                                  )
                                  (cache1, [])
                                  (NE.toList args)
                              let argsNE = NE.fromList (reverse args')
                              case headTy of
                                TVarRef ref -> pure (cache', TVarAppRef ref argsNE)
                                TBaseWithIdentity identity con -> pure (cache', TConWithIdentity identity con argsNE)
                                TConWithIdentity identity con existingArgs -> pure (cache', TConWithIdentity identity con (existingArgs <> argsNE))
                                TVarAppRef ref existingArgs -> pure (cache', TVarAppRef ref (existingArgs <> argsNE))
                                _ -> pure (cache', TVarAppRef (varRef (canonical headNode)) argsNE)
                            TyForall {tnBody = b} ->
                              let bodyC = canonical b
                               in vChild cache0 namedExtra' mode bodyC
                            TyExp {tnBody = b} ->
                              goFull cache0 namedExtra' mode (canonical b)
                          (cache'', t) <- wrapBinders cache' namedExtra' core binders
                          let cacheFinal = cacheInsertLocal mode key t cache'' namedExtra
                          pure (markDone cacheFinal, t)

    goBound cache namedExtra n = do
      node <- lookupNode n
      case node of
        TyVar {} ->
          if isEliminatedVarS n
            then pure (cache, TBottom)
            else case lookupVarBoundS n of
              Nothing ->
                if isNamedLocal namedExtra n
                  then pure (cache, varFor n)
                  else pure (cache, TBottom)
              Just bnd ->
                let bndC = canonical bnd
                 in if isNamedLocal namedExtra bndC
                      then pure (cache, varFor bndC)
                      else
                        if bndC == n
                          then pure (cache, TBottom)
                          else do
                            mbBoundParent <- lookupBindParentUnderSoft (typeRef bndC)
                            let bndRoot =
                                  case mbBoundParent of
                                    Just (TypeRef parent, _) ->
                                      case lookupNodeIn nodes (canonical parent) of
                                        Just TyForall {} -> canonical parent
                                        Just TyMu {} -> canonical parent
                                        _ -> bndC
                                    _ -> bndC
                                boundRootIsRigidAlias =
                                  bndRoot == bndC
                                    && case (lookupNodeIn nodes bndRoot, mbBoundParent) of
                                      (Just TyVar {}, Just (_, BindRigid)) -> True
                                      _ -> False
                            if boundRootIsRigidAlias
                              -- Rigid quantification is inlined by the thesis
                              -- translation, so follow the alias's bound.
                              then goBound cache namedExtra bndRoot
                              else goFull cache namedExtra ModeBound bndRoot
        _ -> goFull cache namedExtra ModeBound n

    boundIsPoly n =
      case lookupVarBoundS (canonical n) of
        Nothing -> False
        Just bnd -> boundHasForall IntSet.empty bnd
      where
        boundHasForall visited nid0 =
          let nidC = canonical nid0
              key = getNodeId nidC
           in if IntSet.member key visited
                then False
                else case lookupNodeIn nodes nidC of
                  Just TyForall {} -> True
                  Just TyVar {} ->
                    case lookupVarBoundS nidC of
                      Just bnd'
                        | canonical bnd' /= nidC ->
                            boundHasForall (IntSet.insert key visited) bnd'
                      _ -> False
                  Just TyExp {tnBody = b} ->
                    boundHasForall (IntSet.insert key visited) b
                  Just TyMu {tnBody = b} ->
                    boundHasForall (IntSet.insert key visited) b
                  _ -> False

    vChild cache namedExtra mode child = do
      let childC = canonical child
          childKey = getNodeId childC
          boundIsBaseOrBottom =
            case lookupVarBoundS childC of
              Nothing -> False
              Just bnd ->
                case lookupNodeIn nodes (canonical bnd) of
                  Just TyBase {} -> True
                  Just TyBottom {} -> True
                  _ -> False
          inlineWeakened =
            IntSet.member childKey weakened
              && boundIsBaseOrBottom
              && not (isNamedLocal namedExtra childC)
      mbParent <- lookupBindParentUnderSoft (typeRef child)
      case mbParent of
        Just (TypeRef parent, BindRigid) ->
          if boundIsPoly child || isNamedLocal namedExtra (canonical child)
            then pure (cache, varFor child)
            else case (mode, lookupNodeIn nodes parent) of
              (ModeTypeNoFallback, Just TyForall {}) -> goFull cache namedExtra mode child
              _ -> goBound cache namedExtra child
        Just (GenRef _, BindRigid) ->
          if boundIsPoly child || isNamedLocal namedExtra (canonical child)
            then pure (cache, varFor child)
            else goBound cache namedExtra child
        Just (_, BindFlex) ->
          case mode of
            ModeBound ->
              case lookupNodeIn nodes (canonical child) of
                Just TyVar {}
                  | isEliminatedVarS (canonical child) ->
                      goBound cache namedExtra child
                Just TyVar {} -> pure (cache, varFor child)
                _ -> goFull cache namedExtra mode child
            _
              | inlineWeakened ->
                  goBound cache namedExtra child
            _ ->
              case lookupNodeIn nodes (canonical child) of
                Just TyVar {} ->
                  let childKey' = getNodeId (canonical child)
                      isBoundHere = IntSet.member childKey' namedExtra
                   in case lookupVarBoundS (canonical child) of
                        Just bnd
                          | IntSet.member (getNodeId (canonical bnd)) schemeRootSet,
                            not isBoundHere,
                            not (isNamedLocal namedExtra (canonical child)) ->
                              goFull cache namedExtra mode (canonical bnd)
                        _ -> pure (cache, varFor child)
                _ -> goFull cache namedExtra mode child
        Nothing -> goBound cache namedExtra child

    wrapBinders cache namedExtra inner binders =
      foldrM
        ( \b (cacheAcc, acc) -> do
            let binderRef = varRef b
                -- Replay may name a copied binder with the identity of its
                -- authoritative live binder. Bounds belong to that identity,
                -- not necessarily to the traversal node.
                boundSource =
                  case typeBinderRefNode binderRef of
                    Just identityNode
                      | let identityNodeC = canonical identityNode,
                        identityNodeC /= canonical b,
                        Just TyVar {} <- lookupNodeIn nodes identityNodeC,
                        Just _ <- lookupVarBoundS identityNodeC ->
                          identityNodeC
                    _ -> b
            (cache', boundTy) <- goBound cacheAcc namedExtra boundSource
            let selfBound =
                  case boundTy of
                    TVarRef ref -> typeBinderRefsSameIdentity ref binderRef
                    _ -> False
                mbBound =
                  case boundTy of
                    TBottom -> Nothing
                    TVarRef {} -> Nothing
                    _
                      | selfBound -> Nothing
                      | otherwise -> either (const Nothing) Just (elabToBound boundTy)
            pure
              ( cache'
              , TForallRef binderRef mbBound acc
              )
        )
        (cache, inner)
        binders

    -- Solving may collapse several graph copies of one source structural
    -- binder under a single mu owner.  The graph nodes remain useful routing
    -- aliases, but the source binder identity denotes one declaration.  Keep
    -- every alias named while constructing exactly one semantic mu binder.
    binderIdentityGroups [] = []
    binderIdentityGroups (binder : rest) =
      let (aliases, remaining) =
            partition
              (typeBinderRefsSameIdentity (varRef binder) . varRef)
              rest
       in (binder : aliases) : binderIdentityGroups remaining

    orderedFlexChildren mode namedExtra n0 = do
      let n = canonical n0
      node <- lookupNode n
      let orderRoot =
            case node of
              TyForall {tnBody = body} -> canonical body
              TyMu {tnBody = body} -> canonical body
              _ -> n
          orderKeys = Order.orderKeysFromConstraintWith canonical originalConstraint orderRoot Nothing
      let includeRigid =
            isForall node
              || isMu node
              || mode == ModeBound
              || IntSet.member (getNodeId n) schemeRootSet
      let schemeOwner =
            if IntSet.member (getNodeId n) schemeRootSet
              then IntMap.lookup (getNodeId n) schemeGenByRoot
              else Nothing
      let parentRefsForBinders =
            case node of
              TyForall {} -> [typeRef n]
              -- A mu binder is created as an exact child of its TyMu owner.
              -- Scheme-level Q(g) siblings are outside that recursive scope;
              -- treating them as candidate mu binders can select unrelated
              -- rigid parameters of a higher-kinded declaration.
              TyMu {} -> [typeRef n]
              _ ->
                case schemeOwner of
                  -- A scheme root can own both Q(g) binders and binders local
                  -- to its structural packet.  Looking only under the gen node
                  -- loses the latter (for example a named variable directly
                  -- under an arrow root).
                  Just gid -> [genRef gid, typeRef n]
                  Nothing -> [typeRef n]
      bindersBase0 <-
        concat
          <$> traverse
            ( directFlexChildren
                (mode == ModeTypeNoFallback || mode == ModeBound || isMu node)
                includeRigid
            )
            parentRefsForBinders
      let structuralForOwner = structuralBindersForOwner n
          structuralForOwnerSet = IntSet.fromList structuralForOwner
          bindersBase =
            [ NodeId key
            | key <-
                IntSet.toList $
                  IntSet.union
                    structuralForOwnerSet
                    ( IntSet.fromList
                        [ getNodeId (canonical binder)
                        | binder <- bindersBase0
                        , let key = getNodeId (canonical binder)
                        , not (IntSet.member key structuralBinderKeys)
                            || IntSet.member key structuralForOwnerSet
                        ]
                    )
            ]
      let keepNamedForScheme =
            mode == ModeBound && IntSet.member (getNodeId n) schemeRootSet
      let isBinderNode candidate =
            case (node, lookupNodeIn nodes (canonical candidate)) of
              (TyMu {}, Just TyVar {}) -> True
              (TyMu {}, _) -> False
              (_, Just TyExp {}) -> False
              (_, Just TyBase {}) -> False
              (_, Just TyBottom {}) -> False
              (_, Just _) -> True
              (_, Nothing) -> False
          bindersReachable0 =
            [ canonical b
              | b <- bindersBase,
                isBinderNode b,
                let binderKey = getNodeId (canonical b),
                IntMap.member binderKey orderKeys
                  || IntSet.member binderKey structuralForOwnerSet
            ]
          bindersReachable =
            let withoutOuterOwned =
                  filter
                    (not . isOuterOwnedLocal . canonical)
                    bindersReachable0
                withoutExternal =
                  filter
                    ( \binder ->
                        let key = getNodeId (canonical binder)
                         in not (isExternalLocal (canonical binder))
                              || IntSet.member key structuralForOwnerSet
                    )
                    withoutOuterOwned
            in
            case mode of
              ModeBound ->
                let base = filter (/= n) withoutExternal
                 in if keepNamedForScheme
                      then base
                      else
                        filter
                          ( \binder ->
                              let key = getNodeId (canonical binder)
                               in IntSet.member key structuralForOwnerSet
                                    || not (isAlreadyBoundLocal namedExtra (canonical binder))
                          )
                          base
              ModeTypeNoFallback
                | isForall node -> withoutExternal
                | otherwise ->
                    filter (not . isAlreadyBoundLocal namedExtra . canonical) withoutExternal
              _ -> withoutExternal
          -- A UF merge can leave the allocation identity that used to be the
          -- recursive binder as a bounded occurrence proxy beside its lower
          -- bound.  Both nodes are then direct children of the copied TyMu,
          -- but only the lower-bound identity is the declaration: reifying
          -- the bounded child itself already follows that identity through
          -- 'goBound'.  Treating both siblings as declarations fabricates a
          -- second mu binder after an otherwise valid quotient projection.
          --
          -- This is the same structural-role distinction used by the
          -- generalization reify plan for base/live aliases.  Do it from the
          -- graph relation here as well because local RHS reification has no
          -- source-binder map to carry that plan.
          bindersForOwner =
            case node of
              TyMu {} ->
                let candidateSet =
                      IntSet.fromList
                        [ getNodeId (canonical binder)
                        | binder <- bindersReachable
                        ]
                    isBoundedOccurrenceProxy binder =
                      case lookupVarBoundS (canonical binder) of
                        Just bound ->
                          let binderKey = getNodeId (canonical binder)
                              boundKey = getNodeId (canonical bound)
                           in boundKey /= binderKey
                                && IntSet.member boundKey candidateSet
                        Nothing -> False
                 in filter (not . isBoundedOccurrenceProxy) bindersReachable
              _ -> bindersReachable
          binderKeys = map (getNodeId . canonical) bindersForOwner
          binderSet = IntSet.fromList binderKeys
          missing =
            [ NodeId k
              | k <- binderKeys,
                not (IntMap.member k orderKeys),
                not (IntSet.member k structuralForOwnerSet)
            ]
          depsFor k =
            [ d
              | d <- IntSet.toList (freeVarsInView presolutionView (NodeId k) IntSet.empty),
                IntSet.member d binderSet,
                d /= k
            ]
          cmpReady a b =
            case Order.compareNodesByOrderKey orderKeys (NodeId a) (NodeId b) of
              Right EQ -> compare a b
              Right other -> other
              Left _ -> compare a b -- fallback if missing key (validated above)
      unless (null missing) $
        Left $
          InstantiationError $
            "reifyType: missing order keys for " ++ show missing
      orderedKeys <-
        topoSortBy
          "reifyType: cycle in binder bound dependencies"
          cmpReady
          depsFor
          binderKeys
      pure [canonical (NodeId k) | k <- orderedKeys]
      where
        directFlexChildren includeAll includeRigid parentRef =
          if not includeAll
            then boundFlexChildrenAllUnderSoft parentRef
            else do
              let parentRefC = canonicalRef parentRef
              let childNode childKey =
                    case nodeRefFromKey childKey of
                      TypeRef childN -> Just childN
                      GenRef _ -> Nothing
                  isBindableNode child =
                    case lookupNodeIn nodes child of
                      Just TyVar {} -> True
                      _ -> False
                  isBindable flag child = case flag of
                    BindFlex -> isBindableNode child
                    BindRigid -> includeRigid && isBindableNode child
              pure
                [ canonical child
                  | (childKey, flag) <- IntMap.findWithDefault [] (nodeRefKey parentRefC) softChildren,
                    Just child <- [childNode childKey],
                    isBindable flag child
                ]

    isForall :: TyNode -> Bool
    isForall TyForall {} = True
    isForall _ = False

    isMu :: TyNode -> Bool
    isMu TyMu {} = True
    isMu _ = False

    foldrM :: (a -> b -> Either ElabError b) -> b -> [a] -> Either ElabError b
    foldrM _ z [] = Right z
    foldrM f z (x : xs) = do
      z' <- foldrM f z xs
      f x z'

reifyWithAsRefs ::
  String ->
  PresolutionView p ->
  (NodeId -> TypeBinderRef) ->
  (NodeId -> Bool) ->
  ReifyRoot ->
  (ElabType -> Either ElabError a) ->
  NodeId ->
  Either ElabError a
reifyWithAsRefs contextLabel presolutionView refForVar isNamed rootMode convert nid =
  convert =<< reifyWithRefs contextLabel presolutionView refForVar isNamed rootMode nid

freeVarsInView :: PresolutionView p -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVarsInView presolutionView nid visited
  | IntSet.member key visited = IntSet.empty
  | otherwise =
      let visited' = IntSet.insert key visited
       in case lookupNodeIn nodes (canonical nid) of
            Nothing -> IntSet.empty
            Just TyVar {} ->
              case pvLookupVarBound presolutionView (canonical nid) of
                Nothing -> IntSet.empty
                Just bnd -> freeVarsInView presolutionView (canonical bnd) visited'
            Just TyBase {} -> IntSet.empty
            Just TyBottom {} -> IntSet.empty
            Just TyArrow {tnDom = d, tnCod = c} ->
              freeVarsChild visited' d
                `IntSet.union` freeVarsChild visited' c
            Just TyCon {tnArgs = args} ->
              IntSet.unions (map (freeVarsChild visited') (NE.toList args))
            Just TyVarApp {tnVarHead = headNode, tnArgs = args} ->
              IntSet.unions (map (freeVarsChild visited') (headNode : NE.toList args))
            Just TyForall {tnBody = b} ->
              freeVarsChild visited' b
            Just TyMu {tnBody = b} ->
              freeVarsChild visited' b
            Just TyExp {tnBody = b} ->
              freeVarsInView presolutionView (canonical b) visited'
  where
    constraint = pvConstraint presolutionView
    nodes = cNodes constraint
    canonical = pvCanonical presolutionView
    key = getNodeId (canonical nid)

    freeVarsChild visited' child =
      let childC = canonical child
       in case lookupNodeIn nodes childC of
            Just TyVar {} ->
              case pvLookupBindParent presolutionView (typeRef childC) of
                Just (_, BindRigid) ->
                  freeVarsInView presolutionView childC visited'
                _ ->
                  IntSet.singleton (getNodeId childC)
            -- A flexible structural child is still type structure, not a
            -- binder dependency by itself.  Traverse it until the actual
            -- flexible variables are reached; otherwise a variable nested in
            -- an arrow/con application is omitted from the topological order
            -- and can be quantified after a sibling bound that mentions it.
            Just _ ->
              freeVarsInView presolutionView childC visited'
            Nothing -> IntSet.empty
