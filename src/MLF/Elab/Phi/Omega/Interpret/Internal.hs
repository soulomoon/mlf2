{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : MLF.Elab.Phi.Omega.Interpret.Internal
-- Description : Omega/Step interpretation flow for witness translation (implementation)
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- Internal implementation of the Omega\/Step interpretation flow.
-- The public facade "MLF.Elab.Phi.Omega.Interpret" re-exports
-- 'phiWithSchemeOmega' from this module.
module MLF.Elab.Phi.Omega.Interpret.Internal
  ( phiWithSchemeOmega,
  )
where

{- Note [Omega/Step witness interpretation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'phiWithSchemeOmega' is the core of the Φ translation for witness edges with
Ω (instance-operation) payloads, as described in the thesis §15.3.4 and
xMLF paper (Fig. 10).

The interpretation proceeds in three stages:

  1. **Binder reordering (Σ)**: Reorder quantifier binders from graph <P order
     to the target scheme's binder order, emitting 'InstUnder' context steps
     where the two orders differ (thesis Def. 15.3.4 / Fig. 15.3.5).

  2. **Forall introduction**: Emit 'InstInside' steps for the ∀-intro count
     recorded per edge during presolution.

  3. **Omega loop ('go')**: Walk the list of 'InstanceOp' values (OpRaise,
     OpMerge, OpWeaken, OpGraft) and translate each into xMLF instantiation
     steps.  'continueRaise' handles the multi-step OpRaise translation that
     involves context navigation and binder application.

The function captures the presolution view, edge trace, scheme info, and
constraint structure in a large closure environment.  All reification helpers
('reifyTypeArg', 'inferredOmegaInst', 'applyInferredArgs', etc.) are local
to this closure because they depend on the shared canonical/constraint/trace
context.

Paper references:
  * Yakobowski PhD thesis (2008), §15.3.4 — Φ translation and Ω execution
  * xMLF paper (Rémy & Yakobowski, FLOPS 2010), Fig. 10 — Ω operational rules
-}

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (findIndex, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution (EdgeTrace (..), PresolutionView (..))
import MLF.Constraint.Presolution.Base (InteriorNodes (..))
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution ()
import MLF.Elab.Inst (applyInstantiation, composeInst, instMany, schemeToType)
import MLF.Elab.Phi.Context (contextToNodeBoundWithOrderKeys)
import MLF.Elab.Phi.Omega.Domain
  ( OmegaContext (..),
    isBinderNode,
    isTraceBinderSource,
    isTyVarNode,
    lookupBinderIndex,
    mkOmegaDomainEnv,
    resolveNonRootGraftBinder,
    resolveNonRootWeakenBinder,
    resolveTraceBinderTarget,
  )
import MLF.Elab.Phi.Omega.Normalize (collapseAdjacentPairs, normalizeInst)
import MLF.Elab.Phi.VSpine
  ( BodyShape (..),
    VSpine (..),
    assertSpineSync,
    mkVSpine,
    vSpineBinderAt,
    vSpineBinderRefs,
    vSpineBoundAt,
    vSpineIdAt,
    vSpineIds,
    vSpineLength,
    vSpineNull,
    vsDeleteAt,
    vsInsertAt,
    vsUpdateBound
  )
import MLF.Elab.Run.Instantiation (containsForallType, inferInstAppArgsFromSchemeRefs)
import MLF.Elab.Sigma (bubbleReorderTo)
import MLF.Elab.Types
import MLF.Reify.TypeOps (alphaEqType, composeTypeHeadRef, freeTypeVarRefsList, inlineAliasBoundsWithBy, inlineBaseBoundsType, splitForallsRefs, substTypeCaptureRef)
import MLF.Util.Graph (topoSortBy)
import qualified MLF.Util.Order as Order
import qualified MLF.Util.OrderKey as OrderKey
import MLF.Util.Trace (traceGeneralize)
import Text.Read (readMaybe)

data TypeArgKey
  = TypeArgIdentity TypeBinderIdentity
  deriving (Eq, Ord, Show)

typeArgKeyForRef :: TypeBinderRef -> TypeArgKey
typeArgKeyForRef =
  TypeArgIdentity . typeBinderRefIdentity

newtype ApplyFun i = ApplyFun {runApplyFun :: Set.Set TypeArgKey -> Ty i}

phiWithSchemeOmega ::
  OmegaContext p ->
  IntSet.IntSet ->
  SchemeInfo ->
  -- | forall intro count (O phase)
  Int ->
  -- | omega ops
  [InstanceOp] ->
  Either ElabError Instantiation
phiWithSchemeOmega ctx namedSet si introCount omegaOps = phiWithScheme
  where
    presolutionView = ocPresolutionView ctx

    canonicalNode :: NodeId -> NodeId
    canonicalNode = pvCanonical presolutionView

    lookupNodePV :: NodeId -> Maybe TyNode
    lookupNodePV = pvLookupNode presolutionView

    lookupVarBound :: NodeId -> Maybe NodeId
    lookupVarBound = pvLookupVarBound presolutionView

    lookupBindParent :: NodeRef -> Maybe (NodeRef, BindFlag)
    lookupBindParent = pvLookupBindParent presolutionView

    bindParents :: BindParents
    bindParents = pvBindParents presolutionView

    constraint = pvConstraint presolutionView

    reifyBoundWithRefsAt :: IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
    reifyBoundWithRefsAt = ocReifyBoundWithRefs ctx

    reifyTypeWithNamedSetRefsNoFallbackAt ::
      IntMap.IntMap TypeBinderRef ->
      IntSet.IntSet ->
      NodeId ->
      Either ElabError ElabType
    reifyTypeWithNamedSetRefsNoFallbackAt = ocReifyTypeWithNamedSetRefsNoFallback ctx

    copyMap :: IntMap.IntMap NodeId
    copyMap = ocCopyMap ctx

    mTrace :: Maybe EdgeTrace
    mTrace = ocTrace ctx

    -- Note [Witness-domain diagnostics only]: failure messages may report raw
    -- witness-domain source matches derived from trace/copy-map artifacts, but
    -- runtime binder selection below remains direct and fail-fast on replay-
    -- spine targets. These diagnostics never participate in target recovery.

    mSchemeInfo :: Maybe SchemeInfo
    mSchemeInfo = ocSchemeInfo ctx

    traceBinderSources :: IntSet.IntSet
    traceBinderSources = ocTraceBinderSources ctx

    traceBinderReplayMap :: IntMap.IntMap NodeId
    traceBinderReplayMap = ocTraceBinderReplayMap ctx

    edgeRoot :: NodeId
    edgeRoot = ocEdgeRoot ctx

    edgeLeft :: NodeId
    edgeLeft = ocEdgeLeft ctx

    edgeRight :: NodeId
    edgeRight = ocEdgeRight ctx

    domainEnv = mkOmegaDomainEnv ctx

    replayBinderKeys :: [Int]
    replayBinderKeys = schemeInfoBinderIdentityKeys si

    resolveTraceBinderTarget' :: Bool -> String -> NodeId -> Either ElabError NodeId
    resolveTraceBinderTarget' requireBinder opName =
      resolveTraceBinderTarget domainEnv requireBinder opName replayBinderKeys isSchemeBinder

    resolveNonRootGraftBinder' ::
      IntSet.IntSet ->
      VSpine ->
      (NodeId -> Maybe String) ->
      NodeId ->
      NodeId ->
      Either ElabError NodeId
    resolveNonRootGraftBinder' binderKeys vs lookupBinder =
      resolveNonRootGraftBinder domainEnv binderKeys (vSpineIds vs) lookupBinder

    resolveNonRootWeakenBinder' ::
      IntSet.IntSet ->
      VSpine ->
      NodeId ->
      NodeId ->
      Either ElabError NodeId
    resolveNonRootWeakenBinder' binderKeys vs =
      resolveNonRootWeakenBinder domainEnv binderKeys (vSpineIds vs)

    lookupBinderIndex' :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Maybe Int
    lookupBinderIndex' = lookupBinderIndex domainEnv

    isBinderNode' :: IntSet.IntSet -> NodeId -> Bool
    isBinderNode' = isBinderNode domainEnv

    isTraceBinderSource' :: NodeId -> Bool
    isTraceBinderSource' = isTraceBinderSource domainEnv

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
                 in case lookupNodePV nidC of
                      Just TyVar {} ->
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

    nodes = cNodes constraint

    boundKids nid = case lookupNodeIn nodes nid of
      Just TyVar {tnBound = Just bnd} -> [bnd]
      _ -> []

    schemeRootGenMap =
      IntMap.fromList
        [ (getNodeId (canonicalNode root), gnId gen)
          | gen <- NodeAccess.allGenNodes constraint,
            root <- gnSchemes gen
        ]

    genChildrenMap =
      IntMap.fromListWith
        (++)
        [ ( getGenNodeId gid,
            [canonicalNode child]
          )
          | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents,
            GenRef gid <- [parentRef],
            TypeRef child <- [nodeRefFromKey childKey]
        ]

    bindKids rootC nid =
      let localChildren =
            [ canonicalNode child
              | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents,
                TypeRef child <- [nodeRefFromKey childKey],
                parentRefCanon parentRef == TypeRef (canonicalNode nid)
            ]
          genChildren =
            case IntMap.lookup (getNodeId (canonicalNode nid)) schemeRootGenMap of
              Nothing -> []
              Just gid -> IntMap.findWithDefault [] (getGenNodeId gid) genChildrenMap
          siblingGenChildren =
            if canonicalNode nid == rootC
              then case lookupBindParent (typeRef (canonicalNode nid)) of
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
        (r0 : rs) ->
          case foldM (Binding.bindingLCA constraint) r0 rs of
            Left _ -> orderRoot
            Right (TypeRef nid) -> canonicalNode nid
            Right (GenRef gid) ->
              fromMaybe orderRoot $ do
                gen <- NodeAccess.lookupGenNode constraint gid
                listToMaybe (gnSchemes gen)

    orderKeys :: IntMap.IntMap Order.OrderKey
    -- Order keys are used to compare binder positions (≺) for Σ(g) / ϕR (thesis Def. 15.3.4).
    -- The natural "paper root" for Φ/Σ is the expansion root `r` (often a TyExp body), but `r`
    -- might not reach the scheme binders via the binding tree when `r` is strictly *under* a
    -- TyForall wrapper. In that situation, compute order keys from the binding-tree LCA of the
    -- scheme binders instead, so every binder identity has a key.
    orderKeys = orderKeysFromRoot orderKeysRoot

    orderKeysRoot :: NodeId
    orderKeysRoot = lcaRootForBinders [NodeId k | k <- schemeInfoBinderIdentityKeys si]

    orderKeysForBinders binders =
      case binders of
        [] -> orderKeys
        _ -> orderKeysFromRoot (lcaRootForBinders binders)

    schemeBinderKeys :: IntSet.IntSet
    schemeBinderKeys = schemeInfoBinderIdentityKeySet si

    isSchemeBinder :: NodeId -> Bool
    isSchemeBinder nid =
      IntSet.member (getNodeId nid) schemeBinderKeys
        && isTyVarNode domainEnv nid

    substForTypes :: IntMap.IntMap TypeBinderRef
    substForTypes =
      case mSchemeInfo of
        Just si' -> schemeInfoBinderRefSubst si'
        Nothing -> IntMap.empty

    traceArgMap :: IntSet.IntSet -> Map.Map TypeArgKey ElabType
    traceArgMap namedSet' =
      case (mTrace, mSchemeInfo) of
        (Just tr, Just si') ->
          let subst = schemeInfoBinderRefSubst si'
              refFor nid = IntMap.lookup (getNodeId (canonicalNode nid)) subst
              reifyArg arg =
                let argC = canonicalNode arg
                    direct = reifyTypeWithNamedSetRefsNoFallbackAt subst namedSet' argC
                    viaBound = case lookupVarBound argC of
                      Just bnd -> reifyBoundWithRefsAt subst bnd
                      Nothing -> direct
                 in case (direct, viaBound) of
                      (Right tyDirect, Right tyBound)
                        | containsForallType tyDirect -> Right tyDirect
                        | containsBottomTy tyDirect && not (containsBottomTy tyBound) -> Right tyBound
                        | otherwise -> Right tyDirect
                      (Right tyDirect, Left _) -> Right tyDirect
                      (Left _, Right tyBound) -> Right tyBound
                      (Left err, Left _) -> Left err
              entries =
                [ (typeArgKeyForRef ref, ty)
                  | (binder, arg) <- etBinderArgs tr,
                    Just ref <- [refFor binder],
                    Right ty <- [reifyArg arg]
                ]
           in Map.fromList entries
        _ -> Map.empty

    inferredArgMapFromTarget :: IntSet.IntSet -> Map.Map TypeArgKey ElabType
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
                  let refs = map fst (schemeBinderRefs (siScheme si'))
                   in Map.fromList (zip (map typeArgKeyForRef refs) args)

    preferInferredArg :: ElabType -> ElabType -> ElabType
    preferInferredArg targetArg traceArg =
      case targetArg of
        TVarRef _
          | not (containsBottomTy traceArg) -> traceArg
        _ -> targetArg

    inferredArgMap :: IntSet.IntSet -> Map.Map TypeArgKey ElabType
    inferredArgMap namedSet' =
      Map.unionWith preferInferredArg (inferredArgMapFromTarget namedSet') (traceArgMap namedSet')

    applyInferredArgs :: IntSet.IntSet -> ElabType -> ElabType
    applyInferredArgs namedSet' = applyInferredArgsWith namedSet' Set.empty

    applyInferredArgsWith :: IntSet.IntSet -> Set.Set TypeArgKey -> ElabType -> ElabType
    applyInferredArgsWith namedSet' bound0 ty0 = runApplyFun (cataIx alg ty0) bound0
      where
        inferredArgMap' = inferredArgMap namedSet'
        alg :: TyIF i ApplyFun -> ApplyFun i
        alg ty = case ty of
          TVarIFRef ref ->
            ApplyFun $ \bound ->
              let key = typeArgKeyForRef ref
               in if Set.member key bound
                    then TVarRef ref
                    else case Map.lookup key inferredArgMap' of
                      Just instTy -> instTy
                      Nothing -> TVarRef ref
          TArrowIF a b ->
            ApplyFun $ \bound ->
              TArrow (runApplyFun a bound) (runApplyFun b bound)
          TConIFWithIdentity identity c args ->
            ApplyFun $ \bound ->
              TConWithIdentity identity c (fmap (\f -> runApplyFun f bound) args)
          TVarAppIFRef ref args ->
            ApplyFun $ \bound ->
              let args' = fmap (\f -> runApplyFun f bound) args
                  key = typeArgKeyForRef ref
               in if Set.member key bound
                    then TVarAppRef ref args'
                    else case Map.lookup key inferredArgMap' of
                      Just instTy -> composeTypeHeadRef ref instTy args'
                      Nothing -> TVarAppRef ref args'
          TBaseIFWithIdentity identity b -> ApplyFun (const (TBaseWithIdentity identity b))
          TBottomIF -> ApplyFun (const TBottom)
          TForallIFRef ref mb body ->
            ApplyFun $ \bound ->
              let bound' = Set.insert (typeArgKeyForRef ref) bound
                  mb' = fmap (\f -> runApplyFun f bound) mb
               in TForallRef ref mb' (runApplyFun body bound')
          TMuIFRef ref body ->
            ApplyFun $ \bound ->
              let bound' = Set.insert (typeArgKeyForRef ref) bound
               in TMuRef ref (runApplyFun body bound')

    _binderArgType :: IntSet.IntSet -> NodeId -> Maybe ElabType
    _binderArgType namedSet' binder = do
      ref <- IntMap.lookup (getNodeId (canonicalNode binder)) substForTypes
      Map.lookup (typeArgKeyForRef ref) (inferredArgMap namedSet')

    substRefForTypeRef :: TypeBinderRef -> Maybe TypeBinderRef
    substRefForTypeRef ref = do
      nid <- typeBinderRefNode ref
      IntMap.lookup (getNodeId nid) substForTypes

    reifyTypeArg :: IntSet.IntSet -> Maybe NodeId -> NodeId -> Either ElabError ElabType
    reifyTypeArg namedSet' mbBinder arg = do
      let argC = canonicalNode arg
      ty <- case lookupVarBound argC of
        Just bnd -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' bnd
        Nothing -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' argC
      let inferredSingleton =
            case Map.toList (inferredArgMapFromTarget namedSet') of
              [(_name, inferredTy)] -> Just inferredTy
              _ -> Nothing
          chosenTy0 =
            case (ty, inferredSingleton) of
              (TVarRef _, Just inferredTy)
                | not (containsBottomTy inferredTy) -> inferredTy
              _ -> ty
          chosenTy1 =
            case (chosenTy0, inferredSingleton) of
              (_, Just (TVarRef inferredRef)) ->
                let binderRef =
                      fromMaybe
                        inferredRef
                        (substRefForTypeRef inferredRef)
                 in case filter (not . typeBinderRefsSameIdentity binderRef) (freeTypeVarRefsList chosenTy0) of
                      [fv] -> substTypeCaptureRef fv (TVarRef binderRef) chosenTy0
                      _ -> chosenTy0
              (TVarRef _, _) -> chosenTy0
              _ -> chosenTy0
          chosenTy = substSchemeNames chosenTy1
          rescuedTy =
            case (mbBinder, chosenTy) of
              (Just binder, TBottom) ->
                case IntMap.lookup (getNodeId (canonicalNode binder)) substForTypes of
                  Just binderRef -> TVarRef binderRef
                  Nothing -> chosenTy
              _ -> chosenTy
      debugPhi
        ( "reifyTypeArg(reify) arg="
            ++ show arg
            ++ " mbBinder="
            ++ show mbBinder
            ++ " inferredFromTarget="
            ++ show (inferredArgMapFromTarget namedSet')
            ++ " inferredMap="
            ++ show (inferredArgMap namedSet')
            ++ " freeChosen="
            ++ show (freeTypeVarRefsList chosenTy0)
            ++ " ty="
            ++ show ty
            ++ " chosenTy0="
            ++ show chosenTy0
            ++ " chosenTy1="
            ++ show chosenTy1
            ++ " chosenTy="
            ++ show chosenTy
            ++ " rescuedTy="
            ++ show rescuedTy
        )
        (pure rescuedTy)

    substSchemeNames :: ElabType -> ElabType
    substSchemeNames = cataIx alg
      where
        alg :: TyIF i Ty -> Ty i
        alg tyNode = case tyNode of
          TVarIFRef ref ->
            case substRefForTypeRef ref of
              Just substRef -> TVarRef substRef
              Nothing -> TVarRef ref
          TArrowIF a b -> TArrow a b
          TConIFWithIdentity identity c args -> TConWithIdentity identity c args
          TVarAppIFRef ref args ->
            let ref' =
                  case substRefForTypeRef ref of
                    Just substRef -> substRef
                    Nothing -> ref
             in TVarAppRef ref' args
          TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
          TForallIFRef ref mb body -> TForallRef ref mb body
          TMuIFRef ref body -> TMuRef ref body
          TBottomIF -> TBottom

    containsBottomTy :: Ty v -> Bool
    containsBottomTy ty = case ty of
      TVarRef _ -> False
      TBase _ -> False
      TBottom -> True
      TArrow a b -> containsBottomTy a || containsBottomTy b
      TCon _ args -> any containsBottomTy args
      TVarAppRef _ args -> any containsBottomTy args
      TForallRef _ mb body -> maybe False containsBottomTy mb || containsBottomTy body
      TMuRef _ body -> containsBottomTy body

    reifyBoundType :: NodeId -> Either ElabError ElabType
    reifyBoundType = reifyBoundWithRefsAt substForTypes

    reifyTargetTypeForInst :: IntSet.IntSet -> NodeId -> Either ElabError ElabType
    reifyTargetTypeForInst namedSet' nid = do
      let nidC = canonicalNode nid
      ty <- case lookupVarBound nidC of
        Just bnd -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' bnd
        Nothing -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' nidC
      pure (inlineBaseBounds ty)

    inlineBaseBounds :: ElabType -> ElabType
    inlineBaseBounds =
      inlineBaseBoundsType
        constraint
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
        (cNodes constraint)
        lookupVarBound
        (reifyBoundWithRefsAt substForTypes)

    inferInstAppArgs :: ElabScheme -> ElabType -> Maybe [ElabType]
    inferInstAppArgs scheme targetTy =
      inferInstAppArgsFromSchemeRefs (schemeBinderRefs scheme) (schemeBody scheme) targetTy

    -- \| Paper Def. 15.3.4 / Fig. 15.3.5: Φ(e) = Σ; O; Φχe(Ω).
    -- Thesis treats quantifier introduction (O) and witness replay (Ω) as
    -- separate phases. The intro count drives O as a prefix of InstIntro
    -- steps, then the omega ops are replayed via `go`.
    phiWithScheme :: Either ElabError Instantiation
    phiWithScheme = do
      let ty0 = schemeToType (siScheme si)
          subst = schemeInfoBinderRefSubst si
          lookupBinder (NodeId i) = typeBinderRefName <$> IntMap.lookup i subst
          ids0 = idsForStartType si ty0
          binderKeys = IntSet.fromAscList (IntMap.keys subst)
      -- Always attempt Σ(g) / ϕR at the start (thesis Def. 15.3.4), even if Ω has no Raise steps.
      (sigma, ty1, ids1) <- reorderBindersByPrec ty0 ids0
      -- Phase O: apply all quantifier introductions up front.
      (ty2, ids2) <- applyIntros introCount ty1 ids1
      let phiIntro = instMany (replicate introCount InstIntro)
      -- Phase Ω: replay witness operations on the intro-extended type.
      let vs2 = mkVSpine ty2 ids2
      phiOmegaRaw <- go binderKeys namedSet vs2 [] omegaOps lookupBinder
      let phiOmega =
            case phiOmegaRaw of
              InstId -> inferredOmegaInst namedSet vs2
              _ -> phiOmegaRaw
      pure (normalizeInst (instMany [sigma, phiIntro, phiOmega]))

    -- \| Apply n quantifier introductions, prepending Nothing to ids each time.
    applyIntros :: Int -> ElabType -> [Maybe NodeId] -> Either ElabError (ElabType, [Maybe NodeId])
    applyIntros 0 ty ids = Right (ty, ids)
    applyIntros n ty ids = do
      ty' <- applyInst "applyIntros" ty InstIntro
      applyIntros (n - 1) ty' (Nothing : ids)

    applyInst :: String -> ElabType -> Instantiation -> Either ElabError ElabType
    applyInst label ty0 inst = case applyInstantiation ty0 inst of
      Left (InstantiationError msg) ->
        Left $
          PhiInvariantError $
            label ++ ": " ++ msg ++ " ; inst=" ++ pretty inst ++ " ; ty=" ++ pretty ty0
      other -> other

    inferredOmegaInst :: IntSet.IntSet -> VSpine -> Instantiation
    inferredOmegaInst namedSet' vs =
      let inferred = inferredArgMap namedSet'
          refs = vSpineBinderRefs vs
          isIdentityArg :: TypeBinderRef -> ElabType -> Bool
          isIdentityArg ref ty = case ty of
            TVarRef argRef -> typeBinderRefsSameIdentity ref argRef
            _ -> False
          isPresent = maybe False (const True)
          firstUseful =
            findIndex
              ( \ref ->
                  case Map.lookup (typeArgKeyForRef ref) inferred of
                    Just ty -> not (isIdentityArg ref ty)
                    Nothing -> False
              )
              refs
       in case firstUseful of
            Nothing -> InstId
            Just startIdx ->
              let suffixRefs = drop startIdx refs
                  argsMaybe = map (flip Map.lookup inferred . typeArgKeyForRef) suffixRefs
                  prefixLen = length (takeWhile isPresent argsMaybe)
                  hasOutOfOrder = any isPresent (drop prefixLen argsMaybe)
                  args = mapMaybe id (take prefixLen argsMaybe)
                  prefixBefore = take startIdx refs
               in if prefixLen == 0 || hasOutOfOrder
                    then InstId
                    else underContext prefixBefore (instMany (map InstApp args))

    reorderBindersByPrec :: ElabType -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderBindersByPrec ty ids = do
      let vs0 = mkVSpine ty ids
      assertSpineSync vs0 ty ids
      if vSpineLength vs0 < 2
        then Right (InstId, ty, ids)
        else do
          let schemeArity = length (schemeBinderRefs (siScheme si))
              missingIdPositions =
                [ i
                  | ((i, (ref, _)), Nothing) <- zip (zip [(0 :: Int) ..] schemeBinders) ids,
                    i < schemeArity,
                    binderRequiresIdentity ref
                ]
          -- Builtin type schemes (e.g. __io_bind) have synthetic binder names
          -- that never flow through the generalizer, so all identities are
          -- Nothing.  Treat this as "no reordering information available".
          if length missingIdPositions == schemeArity
            then Right (InstId, ty, ids)
            else do
              let sourceBinders = [canonicalNode nid | Just nid <- ids, isSchemeBinder nid]
                  orderKeysActive = orderKeysForBinders sourceBinders
                  missingKeyBinders =
                    [ nid
                      | Just nid <- ids,
                        isSchemeBinder nid,
                        not (IntMap.member (getNodeId (canonicalNode nid)) orderKeysActive)
                    ]
              unless (null missingIdPositions) $
                Left $
                  PhiInvariantError $
                    "PhiReorder: missing binder identity at positions " ++ show missingIdPositions
              let orderKeysForSort =
                    if null missingKeyBinders
                      then orderKeysActive
                      else orderKeys
              desired <- desiredBinderOrder orderKeysForSort vs0
              reorderTo vs0 ty ids desired
      where
        schemeBinders = schemeBinderRefs (siScheme si)
        schemeBinderRefSubst = schemeInfoBinderRefSubst si

        binderRequiresIdentity ref = case parseBinderId name of
          Just _ -> True
          Nothing ->
            any
              (\substRef ->
                 typeBinderRefsSameIdentity ref substRef
              )
              (IntMap.elems schemeBinderRefSubst)
          where
            name = typeBinderRefName ref

    desiredBinderOrder :: IntMap.IntMap Order.OrderKey -> VSpine -> Either ElabError [Maybe NodeId]
    desiredBinderOrder orderKeysActive vs0 = do
      let n = vSpineLength vs0
      binders <- mapM (vSpineBinderAt vs0) [0 .. n - 1]
      let refs = vSpineBinderRefs vs0
          binderMap = IntMap.fromList (zip [0 ..] binders)
          refIndex ref = findIndex (typeBinderRefsSameIdentity ref) refs

          -- Bound dependencies: if a occurs free in b's bound, then a must appear before b.
          depsFor :: Int -> [Int]
          depsFor i =
            case (IntMap.lookup i binderMap, listToMaybe (drop i refs)) of
              (Just (_binderName, Just bnd, _), Just binderRef) ->
                [ j
                  | ref <- freeTypeVarRefsList bnd,
                    not (typeBinderRefsSameIdentity ref binderRef),
                    Just j <- [refIndex ref]
                ]
              _ -> []

          cmpIdx :: Int -> Int -> Ordering
          cmpIdx i j =
            case (IntMap.lookup i binderMap, IntMap.lookup j binderMap) of
              (Just (_, _, Just a), Just (_, _, Just b))
                | not (isSchemeBinder a) || not (isSchemeBinder b) ->
                    compare i j
              (Just (_, _, Just a), Just (_, _, Just b)) ->
                let ca = canonicalNode a
                    cb = canonicalNode b
                 in case Order.compareNodesByOrderKey orderKeysActive ca cb of
                      Right ord -> ord
                      Left _ -> compare i j -- fallback if missing key
              (Just (_, _, Just _), _) -> LT
              (_, Just (_, _, Just _)) -> GT
              _ -> compare i j
          indices = [0 .. n - 1]

      idxs <-
        case topoSortBy
          "PhiReorder: cycle in bound dependencies"
          cmpIdx
          depsFor
          indices of
          Right ordered -> Right ordered
          Left (InstantiationError "PhiReorder: cycle in bound dependencies") ->
            Right (sortBy cmpIdx indices)
          Left err -> Left err
      mapM
        (\i -> case IntMap.lookup i binderMap of
            Just (_, _, mid) -> Right mid
            Nothing ->
              Left $
                PhiInvariantError $
                  "PhiReorder: binder index " ++ show i ++ " out of range during reorder"
        )
        idxs

    reorderTo :: VSpine -> ElabType -> [Maybe NodeId] -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderTo _vs0 ty ids desired = bubbleReorderTo "reorderBindersByPrec" ty ids desired

    -- Interpret witness ops while tracking the current type.
    --
    -- Paper Fig. 10 uses instantiation contexts (C{.}) to reach a binder rather
    -- than swapping quantifiers. Using `InstUnder` keeps binder nesting intact,
    -- which matters for operations like Merge that reference outer binders.
    nodeExists :: NodeId -> Bool
    nodeExists nid =
      case lookupNodePV (canonicalNode nid) of
        Just _ -> True
        Nothing -> False

    go ::
      IntSet.IntSet ->
      IntSet.IntSet ->
      VSpine ->
      [Instantiation] ->
      [InstanceOp] ->
      (NodeId -> Maybe String) ->
      Either ElabError Instantiation
    go binderKeys namedSet' vs accum ops lookupBinder = case ops of
      [] -> Right (foldl' composeInst InstId (collapseAdjacentPairs (reverse accum)))
      (OpGraft arg bv : rest) -> do
        bvReplay <- resolveTraceBinderTarget' True "OpGraft" bv
        let bvC = canonicalNode bvReplay
            rootC = canonicalNode orderRoot
        if bvC == rootC
          then do
            if vSpineNull vs
              then do
                argTy <- reifyTypeArg namedSet' Nothing (canonicalNode arg)
                let inst =
                      if vsBody vs == BodyBottom
                        then InstBot argTy
                        else InstId
                    vs' = if vsBody vs == BodyBottom then vs {vsBody = BodyNonBottom} else vs
                go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
              else do
                argTy <- reifyTypeArg namedSet' Nothing (canonicalNode arg)
                let inst = InstApp argTy
                    vs' = vsDeleteAt 0 vs
                go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
          else
            if not (isBinderNode' binderKeys bvReplay)
              then
                Left $
                  PhiTranslatabilityError
                    [ "OpGraft targets non-binder node",
                      "  target node: " ++ show bv,
                      "  canonical: " ++ show bvC
                    ]
              else do
                bvResolved <- resolveNonRootGraftBinder' binderKeys vs lookupBinder bv bvReplay
                case lookupBinderIndex' binderKeys (vSpineIds vs) bvResolved of
                  Nothing ->
                    Left $
                      PhiTranslatabilityError
                        [ "OpGraft: binder not found in quantifier spine",
                          "  target node: " ++ show bv,
                          "  canonical: " ++ show bvC
                        ]
                  Just i -> do
                    mbBound <- vSpineBoundAt vs i
                    if mbBound /= Just TBottom && mbBound /= Nothing
                      then do
                        argTy <- reifyTypeArg namedSet' Nothing (canonicalNode arg)
                        let boundTy = maybe TBottom tyToElab mbBound
                        if alphaEqType argTy boundTy
                          then -- Bounded-match: bound already equals graft arg, so
                          -- OpGraft is a no-op. The adjacent OpWeaken will emit
                          -- InstElim which substitutes the existing bound (thesis Def. 14.2.1).
                            go binderKeys namedSet' vs accum rest lookupBinder
                          else
                            if argTy == TBottom
                              then -- Bottom arg on bounded binder: no-op (OpWeaken will emit InstElim)
                                go binderKeys namedSet' vs accum rest lookupBinder
                              else
                                Left $
                                  PhiTranslatabilityError
                                    [ "OpGraft requires target binder to be unbounded/⊥-bounded or match its explicit bound",
                                      "  target node: " ++ show bv,
                                      "  canonical: " ++ show bvC,
                                      "  binder bound: " ++ show mbBound,
                                      "  graft arg: " ++ show argTy
                                    ]
                      else do
                        i' <- binderIndex binderKeys (vSpineIds vs) bvResolved
                        argTy <- reifyTypeArg namedSet' (Just bvResolved) (canonicalNode arg)
                        prefix <- prefixBinderRefs vs i'
                        let inst = underContext prefix (InstInside (InstBot argTy))
                            newBound = either (const Nothing) Just (elabToBound argTy)
                            vs' = vsUpdateBound i' newBound vs
                        go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
      (OpWeaken bv : rest) -> do
        bvReplay <- resolveTraceBinderTarget' False "OpWeaken" bv
        let bvC = canonicalNode bvReplay
            rootC = canonicalNode orderRoot
        if bvC == rootC
          then go binderKeys namedSet' vs accum rest lookupBinder
          else do
            -- Strict replay invariant: non-root OpWeaken must resolve to a
            -- binder in the current replay spine and emit InstElim; otherwise
            -- translation fails fast.
            weakenBinder <- resolveNonRootWeakenBinder' binderKeys vs bv bvReplay
            -- Thesis-exact OpWeaken: always emit InstElim.
            -- For graft+weaken pairs, collapseAdjacentPairs merges
            -- the preceding InstInside(InstBot t) with this InstElim
            -- into InstApp t (thesis Def. 14.2.1).
            (inst, vs') <- atBinderWith False binderKeys vs weakenBinder (pure InstElim)
            go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
      (OpRaise n : rest) -> do
        nReplay <- resolveTraceBinderTarget' False "OpRaise" n
        let nSource = n
            nAdopt = canonicalNode nReplay
        if not (nodeExists nAdopt)
          then
            if IntSet.member (getNodeId nSource) traceBinderSources
              then
                Left $
                  PhiInvariantError $
                    unlines
                      [ "trace/replay binder key-space mismatch (OpRaise unresolved trace-source target)",
                        "op: OpRaise",
                        "source target: " ++ show nSource,
                        "replay-map domain: " ++ show (IntMap.keys traceBinderReplayMap)
                      ]
              else
                Left $
                  PhiInvariantError $
                    unlines
                      [ "OpRaise unresolved target has no direct replay/source node",
                        "op: OpRaise",
                        "source target: " ++ show nSource,
                        "replay target: " ++ show nReplay
                      ]
          else do
            let nOrig = canonicalNode nAdopt
            case debugPhi
              ( "OpRaise: nSource="
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
              case lookupNodePV nOrig of
                Just TyForall {tnBody = body} -> do
                  binders <- bindingToElab (Binding.orderedBinders canonicalNode constraint (typeRef nOrig))
                  let bodyC = canonicalNode body
                  pure $ case binders of
                    (b : _) -> canonicalNode b
                    [] -> bodyC
                _ -> pure nOrig
            let nC = raiseTarget
            case debugPhi ("OpRaise: raiseTarget=" ++ show nC) () of
              () -> pure ()
            case debugPhi ("OpRaise: parent=" ++ show (lookupBindParent (typeRef nC))) () of
              () -> pure ()
            nContextTarget <-
              case lookupNodePV nC of
                Just TyExp {tnBody = body} -> pure (canonicalNode body)
                _ -> pure nC
            let shouldRigidSkip =
                  case lookupBindParent (typeRef nC) of
                    Just (_, BindRigid) -> True
                    _ -> False
            if shouldRigidSkip
              then case debugPhi ("OpRaise: rigid skip target=" ++ show nC) () of
                () -> go binderKeys namedSet' vs accum rest lookupBinder
              else do
                continueRaise
                  binderKeys
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
        nReplay <- resolveTraceBinderTarget' True "OpMerge(n)" n
        mReplay <- resolveTraceBinderTarget' True "OpMerge(m)" m
        if isRigidNode nReplay
          then go binderKeys namedSet' vs accum rest lookupBinder
          else
            if isRigidNode mReplay
              then
                Left $
                  PhiTranslatabilityError
                    [ "OpMerge: rigid endpoint appears only on non-operated node",
                      "  operated node n: " ++ show n,
                      "  other endpoint m: " ++ show m
                    ]
              else
                if not (isBinderNode' binderKeys nReplay)
                  then
                    Left $
                      PhiTranslatabilityError
                        [ "OpMerge: first target is non-binder node",
                          "  target node: " ++ show n,
                          "  canonical: " ++ show (canonicalNode nReplay)
                        ]
                  else
                    if not (isBinderNode' binderKeys mReplay)
                      then
                        Left $
                          PhiTranslatabilityError
                            [ "OpMerge: second target is non-binder node",
                              "  target node: " ++ show m,
                              "  canonical: " ++ show (canonicalNode mReplay)
                            ]
                      else
                        if nReplay == mReplay
                          then go binderKeys namedSet' vs accum rest lookupBinder
                          else do
                            mRef <- binderRefFor binderKeys vs mReplay lookupBinder
                            let hAbs = InstSeq (InstInside (instAbstrWithRef mRef)) InstElim
                            (inst, vs') <- atBinderWith False binderKeys vs nReplay (pure hAbs)
                            go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
      (OpRaiseMerge n m : rest) -> do
        nReplay <- resolveTraceBinderTarget' True "OpRaiseMerge(n)" n
        mReplay <- resolveTraceBinderTarget' True "OpRaiseMerge(m)" m
        -- Paper Fig. 15.3.4: rigid-node identity is conditioned on the operated node n.
        if isRigidNode nReplay
          then go binderKeys namedSet' vs accum rest lookupBinder
          else
            if isRigidNode mReplay
              then
                Left $
                  PhiTranslatabilityError
                    [ "OpRaiseMerge: rigid endpoint appears only on non-operated node",
                      "  operated node n: " ++ show n,
                      "  other endpoint m: " ++ show m
                    ]
              else
                if not (isBinderNode' binderKeys nReplay)
                  then
                    Left $
                      PhiTranslatabilityError
                        [ "OpRaiseMerge: first target is non-binder node",
                          "  target node: " ++ show n,
                          "  canonical: " ++ show (canonicalNode nReplay)
                        ]
                  else
                    if not (isBinderNode' binderKeys mReplay)
                      then
                        Left $
                          PhiTranslatabilityError
                            [ "OpRaiseMerge: second target is non-binder node",
                              "  target node: " ++ show m,
                              "  canonical: " ++ show (canonicalNode mReplay)
                            ]
                      else do
                        if nReplay == orderRoot
                          then do
                            mRef <- binderRefFor binderKeys vs mReplay lookupBinder
                            let vs' = VSpine [] BodyNonBottom
                            go binderKeys namedSet' vs' (instAbstrWithRef mRef : accum) rest lookupBinder
                          else do
                            case lookupBinderIndex' binderKeys (vSpineIds vs) nReplay of
                              Nothing ->
                                Left (PhiTranslatabilityError ["OpRaiseMerge: binder " ++ show n ++ " not found in quantifier spine"])
                              Just _ -> do
                                mRef <- binderRefFor binderKeys vs mReplay lookupBinder
                                let hAbs = InstSeq (InstInside (instAbstrWithRef mRef)) InstElim
                                (inst, vs') <- atBinderWith False binderKeys vs nReplay (pure hAbs)
                                go binderKeys namedSet' vs' (inst : accum) rest lookupBinder

    continueRaise ::
      IntSet.IntSet ->
      IntSet.IntSet ->
      VSpine ->
      [Instantiation] ->
      [InstanceOp] ->
      (NodeId -> Maybe String) ->
      NodeId ->
      NodeId ->
      NodeId ->
      NodeId ->
      NodeId ->
      Either ElabError Instantiation
    continueRaise
      binderKeys
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
                  ( IntSet.intersection interiorSet $
                      IntSet.fromList
                        [ getNodeId nSource,
                          getNodeId (canonicalNode nSource),
                          getNodeId nAdopt,
                          getNodeId nOrig,
                          getNodeId nC
                        ]
                  )
            allowOutsideAlias =
              isTraceBinderSource' nSource
                || maybe
                  False
                  ( \aliased ->
                      IntSet.member
                        (getNodeId (canonicalNode aliased))
                        interiorSet
                  )
                  (IntMap.lookup (getNodeId nSource) copyMap)
        if outsideInterior && not allowOutsideAlias
          then
            Left $
              PhiTranslatabilityError
                [ "OpRaise target outside I(r)",
                  "edge: " ++ show edgeLeft ++ " <= " ++ show edgeRight,
                  "op: OpRaise " ++ show nSource,
                  "nSource=" ++ show nSource ++ ", nAdopt=" ++ show nAdopt ++ ", nOrig=" ++ show nOrig ++ ", nC=" ++ show nC,
                  "interiorSet=" ++ show (IntSet.toList interiorSet)
                ]
          else -- Paper Fig. 10: Raise(n) introduces a fresh quantifier one level higher,
          -- bounds it by Txi(n), then aliases/eliminates the old binder.
          --
          -- For spine binders: use the existing logic
          -- For non-spine nodes: use binding edges + prec ordering to compute context

            let mbIndex = lookupBinderIndex' binderKeys (vSpineIds vs) nC
             in case debugPhi ("OpRaise: binderIndex=" ++ show mbIndex) mbIndex of
                  Just i -> do
                    -- Spine binder case
                    (boundRef, mbBound, _) <- vSpineBinderAt vs i
                    let refs = vSpineBinderRefs vs
                        inferredMap = inferredArgMap namedSet'
                        inferredBound =
                          Map.lookup (typeArgKeyForRef boundRef) inferredMap
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
                            (Nothing, TVarRef {}) -> boundTyRaw
                            (Nothing, _) -> inlineAliasBounds boundTyRaw
                            _ -> inlineAliasBoundsAsBound boundTyRaw
                        deps = filter (not . typeBinderRefsSameIdentity boundRef) (freeTypeVarRefsList boundTy)
                        depIdxs = mapMaybe (\ref -> findIndex (typeBinderRefsSameIdentity ref) refs) deps
                        cutoff = if null depIdxs then (-1) else maximum depIdxs
                        insertIndex = cutoff + 1

                    when (insertIndex > i) $
                      Left (PhiInvariantError "OpRaise: computed insertion point is after binder")

                    let prefixBefore = take insertIndex refs
                        between = take (i - insertIndex) (drop insertIndex refs)
                        betaRef = typeBinderRefFromIdentity (typeBinderIdentityFromNode nC) "β"
                        hAbsBeta = InstSeq (InstInside (instAbstrWithRef betaRef)) InstElim
                        aliasOld = underContext between hAbsBeta

                        local =
                          instMany
                            [ InstIntro,
                              InstInside (InstBot boundTy),
                              instUnderWithRef betaRef aliasOld
                            ]

                        inst = underContext prefixBefore local

                    let raisedRef = typeBinderRefFromIdentity (typeBinderIdentityFromNode nC) (typeBinderRefName boundRef)
                        vsNoN = vsDeleteAt i vs
                        newBound = either (const Nothing) Just (elabToBound boundTy)
                        vs' = vsInsertAt insertIndex (raisedRef, newBound, Just nC) vsNoN
                    go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
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
                      case lookupBindParent (typeRef nC) of
                        Just (TypeRef parent, _) ->
                          case lookupNodePV (canonicalNode parent) of
                            Just TyForall {} -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' nC
                            _ -> reifyBoundType nC
                        _ -> reifyBoundType nC
                    let nodeTy = applyInferredArgs namedSet' (inlineAliasBounds nodeTy0)
                    nodeTyBound <-
                      case lookupVarBound (canonicalNode nC) of
                        Just bnd -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' bnd
                        Nothing -> pure nodeTy
                    let nodeTyBound' = inlineAliasBounds nodeTyBound

                    _ <- pure $ debugPhi ("OpRaise: nodeTy=" ++ show nodeTy) ()
                    _ <- pure $ debugPhi ("OpRaise: nodeTyBound=" ++ show nodeTyBound) ()
                    _ <- pure $ debugPhi ("OpRaise: inferredArgMap=" ++ show (inferredArgMap namedSet')) ()
                    _ <- pure $ debugPhi ("OpRaise: traceArgs=" ++ show (fmap etBinderArgs mTrace)) ()

                    let ids = vSpineIds vs
                        refs = vSpineBinderRefs vs

                    -- Compute dependency cutoff: the new binder must be inserted after any
                    -- binder that appears free in `Txi(n)`.
                    let deps = freeTypeVarRefsList nodeTy
                        depIdxs = mapMaybe (\ref -> findIndex (typeBinderRefsSameIdentity ref) refs) deps
                        cutoff = if null depIdxs then (-1) else maximum depIdxs
                        minIdx = min (cutoff + 1) (vSpineLength vs)

                        findCandidate :: [Int] -> Either ElabError (Maybe (Int, [ContextStep]))
                        findCandidate [] = Right Nothing
                        findCandidate (i : is) = do
                          spineNode <- vSpineIdAt vs i
                          case spineNode of
                            Nothing -> findCandidate is
                            Just mNode -> do
                              ctxOrErr <-
                                contextToNodeBoundWithOrderKeys
                                  canonicalNode
                                  orderKeys
                                  constraint
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
                        constraint
                        (canonicalNode orderRoot)
                        nContextTarget
                    let boundTyBot = inlineAliasBoundsAsBound nodeTy
                    let mbRootInst =
                          case (rootCtx, lookupBindParent (typeRef nC)) of
                            (Just _, Just (TypeRef parent, _)) ->
                              let parentC = canonicalNode parent
                                  rootC = canonicalNode orderRoot
                               in if parentC == rootC
                                    || case lookupNodePV parentC of
                                      Just TyForall {} -> True
                                      Just TyMu {} -> True
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
                                          prefixBefore = take minIdx refs
                                          inst = underContext prefixBefore instArgInst
                                       in Just (inst, numToDelete)
                                    else Nothing
                            _ -> Nothing
                    case mbCandidate of
                      Just (insertIdx, ctxMn) -> do
                        let prefixBefore = take insertIdx refs
                            betaRef = typeBinderRefFromIdentity (typeBinderIdentityFromNode nC) "β"
                            aliasOld = applyContext ctxMn InstElim

                            local =
                              instMany
                                [ InstIntro,
                                  InstInside (InstBot boundTyBot),
                                  instUnderWithRef betaRef aliasOld
                                ]

                            inst = underContext prefixBefore local

                        let newBound = either (const Nothing) Just (elabToBound boundTyBot)
                            vs' = vsInsertAt insertIdx (betaRef, newBound, Just nC) vs
                        go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
                      Nothing ->
                        case mbRootInst of
                          Just (inst, numToDelete) -> do
                            let vs' = foldl' (\v _ -> vsDeleteAt minIdx v) vs [1 .. numToDelete]
                            go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
                          Nothing ->
                            Left $
                              PhiTranslatabilityError
                                [ "OpRaise (non-spine): missing computation context",
                                  "  target node: " ++ show nOrig,
                                  "  canonical target: " ++ show nC,
                                  "  context target: " ++ show nContextTarget,
                                  "  orderRoot: " ++ show orderRoot,
                                  "  edgeRoot: " ++ show edgeRoot,
                                  "  minIdx: " ++ show minIdx,
                                  "  deps(Txi(n)): " ++ show deps,
                                  "  nodeTy: " ++ show nodeTy,
                                  "  ids: " ++ show ids,
                                  "  bindParent: " ++ show (lookupBindParent (typeRef nC))
                                ]

    idsForStartType :: SchemeInfo -> ElabType -> [Maybe NodeId]
    idsForStartType si' ty =
      let nameToId =
            Map.fromList
              [ (typeBinderRefName ref, NodeId key)
                | (key, ref) <- IntMap.toList (schemeInfoBinderRefSubst si')
              ]
          (qs, _) = splitForallsRefs ty
       in [ case Map.lookup nm nameToId of
              Just nid -> Just nid
              Nothing -> parseBinderId nm
            | (ref, _) <- qs,
              let nm = typeBinderRefName ref
          ]

    parseBinderId :: String -> Maybe NodeId
    parseBinderId ('t' : rest) = NodeId <$> readMaybe rest
    parseBinderId _ = Nothing

    binderRefFor :: IntSet.IntSet -> VSpine -> NodeId -> (NodeId -> Maybe String) -> Either ElabError TypeBinderRef
    binderRefFor binderKeys vs nid lookupBinder =
      case lookupBinderIndex' binderKeys (vSpineIds vs) nid of
        Just i ->
          case drop i (vSpineBinderRefs vs) of
            ref : _ -> Right ref
            [] -> Left (PhiInvariantError "binderRefFor: index out of range")
        Nothing ->
          let name = fromMaybe ("t" ++ show (getNodeId nid)) (lookupBinder nid)
           in Right (typeBinderRefFromIdentity (typeBinderIdentityFromNode nid) name)

    atBinderWith ::
      Bool ->
      IntSet.IntSet ->
      VSpine ->
      NodeId ->
      Either ElabError Instantiation ->
      Either ElabError (Instantiation, VSpine)
    atBinderWith keep binderKeys vs nid mkInner = do
      i <- binderIndex binderKeys (vSpineIds vs) nid
      prefix <- prefixBinderRefs vs i
      inner <- mkInner
      let vs' = if keep then vs else vsDeleteAt i vs
      pure (underContext prefix inner, vs')

    -- \| Check if a node is bound rigidly. Some ω operations treat rigid targets as
    -- ε/identity (thesis Fig. 15.3.4), but not all (see the OpGraft/OpWeaken note).
    isRigidNode :: NodeId -> Bool
    isRigidNode nid =
      case lookupBindParent (typeRef (canonicalNode nid)) of
        Just (_, BindRigid) -> True
        _ -> False

    binderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Either ElabError Int
    binderIndex binderKeys ids nid =
      case lookupBinderIndex' binderKeys ids nid of
        Just i -> Right i
        Nothing ->
          Left $
            PhiInvariantError $
              "binder " ++ show nid ++ " not found in identity list " ++ show ids

    prefixBinderRefs :: VSpine -> Int -> Either ElabError [TypeBinderRef]
    prefixBinderRefs vs i
      | i < 0 || i > length refs =
          Left (PhiInvariantError "prefixBinderRefs: index out of range")
      | otherwise = Right (take i refs)
      where
        refs = vSpineBinderRefs vs

    underContext :: [TypeBinderRef] -> Instantiation -> Instantiation
    underContext prefix inner = foldr instUnderWithRef inner prefix
