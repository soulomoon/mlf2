{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{- Note [Scheme finalization]

This module implements the final stage of generalization planning: converting
the intermediate generalization plan into a fully-formed ElabScheme suitable
for elaboration.

'finalizeScheme' takes a FinalizeInput (populated by the generalization
planning pipeline) and performs:

1. Alias inlining — variables that bind the type root are inlined into the
   scheme body so the scheme type reflects the actual structure.
2. Canonical variable normalization — 'canonAllVars' alpha-renames all type
   variables to a canonical "v0, v1, ..." form so structural comparisons
   are order-independent.
3. Bound-alias collapsing — when a binder's bound type is structurally
   identical to the scheme body, the binder is collapsed to avoid
   redundant quantification.
4. Scheme normalization — 'simplifySchemeBindingsRefs' and 'promoteArrowAliasRefs'
   strip trivial ∀-binders and promote arrow-shaped aliases.
5. Variable renaming — canonical variables are mapped to fresh alpha names
   (a, b, c, ...) for human-readable output.
6. Binder-provenance and free-variable validation — every outer binder must
   come from the planner capability, while any remaining free identity must
   be authorized by inherited or locally constructed Gamma; no residual is
   repaired into a forall after reification.

The function returns (ElabScheme, subst') where subst' maps node IDs to
their final scheme-level binder refs, used by downstream Φ reconstruction.

Related thesis sections:
  - Section 8.2 — Reification and scheme construction (Fig 8.2.2, 8.2.3)
  - Section 7.6.1 — Generalized types and quantification
  - Section 15.3.5 — exterior binders are supplied by the typing environment
-}
{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.Finalize
  ( FinalizeBinderPlan,
    mkFinalizeBinderPlan,
    finalizeBinderPlanBinderRefs,
    FinalizeInput (..),
    finalizeScheme,
  )
where

import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import MLF.Constraint.Presolution.Plan.Context (GaBindParents (..), GeneralizeEnv (..), traceGeneralize)
import MLF.Constraint.Presolution.Plan.Normalize
  ( containsForall,
    isBaseBound,
    isVarBound,
    promoteArrowAliasRefsWhen,
    simplifySchemeBindingsRefsWhenPreserving,
  )
import MLF.Constraint.Presolution.Plan.ReifyPlan
  ( InheritedGammaPlan,
    inheritedGammaPlanAuthorizedRefs,
  )
import MLF.Constraint.Types.Graph
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Types
  ( ambientSchemeClosureAuthority,
    inheritedGammaSchemeClosureAuthority,
    locallyClosedGammaSchemeClosureAuthority,
    mapBoundType,
    requiredGammaAliasSchemeClosureAuthority,
    schemeClosureFreeRefs,
  )
import MLF.Reify.TypeOps
  ( freeTypeVarRefsType,
    splitForallsRefs,
    stripForallsType,
  )
import MLF.Types.Elab
  ( BoundType,
    ElabScheme,
    ElabType,
    TypeBinderRef,
    Ty (..),
    TyIF (..),
    cataIx,
    mkElabSchemeWithRefs,
    renameTypeBinderRef,
    typeBinderIdentityFromNode,
    typeBinderRefFromIdentity,
    typeBinderRefIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
    tyToElab,
  )
import MLF.Types.Identity (typeBinderIdentityStructural)
import MLF.Util.ElabError (ElabError (..))
import MLF.Util.Names (alphaName)

-- | The exact outer-binder spine authorized by 'BinderPlan', paired once with
-- the bounds reified for those binders. The constructor is private so scheme
-- finalization cannot turn a residual ref discovered in the reified body into
-- a new forall binder.
newtype FinalizeBinderPlan =
  FinalizeBinderPlan [(Int, TypeBinderRef, TypeBinderRef, Maybe BoundType)]

mkFinalizeBinderPlan
  :: [(Int, TypeBinderRef)]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Either ElabError FinalizeBinderPlan
mkFinalizeBinderPlan plannedRefs reifiedBindings =
  FinalizeBinderPlan <$> go (0 :: Int) plannedRefs reifiedBindings
  where
    go _ [] [] = Right []
    go index ((key, plannedRef) : planned) ((actualRef, mbBound) : bindings)
      | typeBinderRefsSameIdentity plannedRef actualRef =
          ((key, plannedRef, actualRef, mbBound) :) <$> go (index + 1) planned bindings
      | otherwise =
          Left
            ( ValidationFailed
                [ "finalize binder plan disagrees with its reified binding order"
                , "  binder index: " ++ show index
                , "  planner identity: " ++ show plannedRef
                , "  reified identity: " ++ show actualRef
                ]
            )
    go _ planned bindings =
      Left
        ( ValidationFailed
            [ "finalize binder plan and reified bindings have different lengths"
            , "  remaining planner binders: " ++ show planned
            , "  remaining reified bindings: " ++ show bindings
            ]
        )

finalizeBinderPlanBinderRefs
  :: FinalizeBinderPlan
  -> [(Int, TypeBinderRef)]
finalizeBinderPlanBinderRefs (FinalizeBinderPlan entries) =
  [(key, plannedRef) | (key, plannedRef, _, _) <- entries]

finalizeBinderPlanBindings
  :: FinalizeBinderPlan
  -> [(TypeBinderRef, Maybe BoundType)]
finalizeBinderPlanBindings (FinalizeBinderPlan entries) =
  [(actualRef, mbBound) | (_, _, actualRef, mbBound) <- entries]

-- | Inputs needed to finalize a generalized scheme.
data FinalizeInput p = FinalizeInput
  { fiEnv :: GeneralizeEnv p,
    fiConstraint :: Constraint p,
    fiCanonical :: NodeId -> NodeId,
    fiBindParents :: BindParents,
    fiScopeRootC :: NodeRef,
    fiTypeRoot :: NodeId,
    fiTypeRootC :: NodeId,
    fiScopeGen :: Maybe GenNodeId,
    fiFirstGenAncestorGa :: NodeRef -> Maybe GenNodeId,
    fiBindParentsGa :: Maybe (GaBindParents p),
    fiSolvedToBasePref :: IntMap.IntMap NodeId,
    fiGammaAlias :: IntMap.IntMap Int,
    fiNamedUnderGaSet :: IntSet.IntSet,
    fiRequiredGammaKeys :: IntSet.IntSet,
    -- | Descendant binders proved by the binder planner to survive free in
    -- the selected construction root.  A required Gamma alias can hide these
    -- identities from the final reified body, but the constructor still
    -- needs them available to complete that Gamma bound.
    fiRootBodyClosureKeys :: IntSet.IntSet,
    -- | Required Γ entries whose paper bound S(n) is an already-scoped type
    -- variable. Section 15.6.2 quotients these aliases before xMLF: the
    -- required node maps to the existing ref and contributes no new binder.
    fiRequiredGammaAliases :: IntMap.IntMap TypeBinderRef,
    -- | Exact inherited declarations proved while the planner still owns the
    -- original lexical and live/base provenance.  Finalization may consume
    -- this capability, but cannot derive authority from residual free refs.
    fiInheritedGammaPlan :: InheritedGammaPlan,
    -- | Lexical source binders proved by the caller to enclose this local
    -- construction.  They are closure authority, not inferred binders.
    fiAmbientBinderRefs :: [TypeBinderRef],
    -- | Gamma entries whose exact boundary is constructed and closed by a
    -- nested term constructor. They may occur free in the root's temporary
    -- reification, but must never be synthesized into root binders.
    fiLocallyClosedGammaRefs :: [TypeBinderRef],
    -- | Exact construction-used refs that BinderPlan has already matched to
    -- declarations in this binder spine.  These binders may be type-vacuous
    -- after erasure, but the checked term still requires their abstractions.
    fiTermUsedRootBinderRefs :: [TypeBinderRef],
    fiBinderPlan :: FinalizeBinderPlan,
    -- | The exact substitution paired with the selected reification domain.
    -- It may contain source-identity routes that are deliberately absent from
    -- the local binder substitution returned to downstream Phi construction.
    fiReifySubst :: IntMap.IntMap TypeBinderRef,
    fiSubst :: IntMap.IntMap TypeBinderRef,
    fiTyRaw :: ElabType
  }

finalizeScheme :: FinalizeInput p -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
finalizeScheme FinalizeInput {..} =
  let env = fiEnv
      constraint = fiConstraint
      canonical = fiCanonical
      typeRoot = fiTypeRoot
      gammaAliasPlan = fiGammaAlias
      namedUnderGaSetPlan = fiNamedUnderGaSet
      substRaw = fiSubst
      orderedBinderRefsRaw = finalizeBinderPlanBinderRefs fiBinderPlan
      reifiedBindings = finalizeBinderPlanBindings fiBinderPlan
      requiredGammaRefs =
        unionTypeRefs
          [ [ ref
            | (key, ref) <- orderedBinderRefsRaw
            , IntSet.member key fiRequiredGammaKeys
            , IntMap.notMember key fiRequiredGammaAliases
            ]
          , -- Ordering may replace a required exterior's raw graph key by
            -- its canonical result key.  The construction substitution is
            -- the identity authority shared by both routes, so retain the
            -- required declaration by that exact identity rather than by
            -- whichever integer key survived ordering.
            [ ref
            | key <- IntSet.toList fiRequiredGammaKeys
            , IntMap.notMember key fiRequiredGammaAliases
            , Just ref <- [IntMap.lookup key substRaw]
            ]
          ]
      isRequiredGammaRef ref = refMember ref requiredGammaRefs
      isTermUsedRootBinderRef ref =
        refMember ref fiTermUsedRootBinderRefs
      isConstructionProtectedRef ref =
        isRequiredGammaRef ref || isTermUsedRootBinderRef ref
      rootBodyClosureRefs =
        [ ref
        | (key, ref) <- orderedBinderRefsRaw,
          IntSet.member key fiRootBodyClosureKeys
        ]
      isRootBodyClosureRef ref = refMember ref rootBodyClosureRefs
      isLocallyClosedGammaRef ref =
        refMember ref fiLocallyClosedGammaRefs
      isRequiredGammaKey rawKey =
        IntSet.member rawKey fiRequiredGammaKeys
          && IntMap.notMember rawKey fiRequiredGammaAliases
      requiredGammaAlias rawKey =
        IntMap.lookup rawKey fiRequiredGammaAliases
          <|> IntMap.lookup (getNodeId (canonical (NodeId rawKey))) fiRequiredGammaAliases
      resolvedRequiredGammaAlias rawKey =
        resolveAliasTarget <$> requiredGammaAlias rawKey
      requiredGammaAliasForRef ref =
        fmap (resolveAliasTarget . snd) $
          find
            ( \(key, _) ->
                case IntMap.lookup key substRaw of
                  Just candidate -> typeBinderRefsSameIdentity candidate ref
                  Nothing -> False
            )
            (IntMap.toList fiRequiredGammaAliases)
      requiredGammaAliasBridgeKeys =
        IntSet.fromList
          [ key
          | (key, ref) <- IntMap.toList substRaw,
            any
              ( \aliasKey ->
                  case IntMap.lookup aliasKey substRaw of
                    Just requiredRef -> typeBinderRefsSameIdentity requiredRef ref
                    Nothing -> False
              )
              (IntMap.keys fiRequiredGammaAliases)
          ]
      resolveAliasTarget targetRef =
        case find (typeBinderRefsSameIdentity targetRef) (IntMap.elems substRaw) of
          Just ref -> ref
          Nothing -> targetRef
      reifiedRouteFor ref = do
        node <- typeBinderRefNode ref
        IntMap.lookup (getNodeId node) fiReifySubst
          <|> IntMap.lookup (getNodeId (canonical node)) fiReifySubst
      orderedBinderKeys =
        IntSet.fromList
          [ getNodeId (canonical (NodeId key))
          | (key, _) <- orderedBinderRefsRaw
          ]
      orderedBinderRefsByKey =
        IntMap.fromList
          [ (getNodeId (canonical (NodeId key)), ref)
          | (key, ref) <- orderedBinderRefsRaw
          ]
      binderRank :: IntMap.IntMap Int
      binderRank =
        IntMap.fromListWith min
          [ (getNodeId (canonical (NodeId key)), rank)
          | (rank, (key, _)) <- zip [0 ..] orderedBinderRefsRaw
          ]
      -- eMLF peer-variable bounds are aliases, not independent xMLF bounds.
      -- Quotient them before constructing the scheme (thesis sec. 15.6.2):
      -- @forall (b >= a). tau@ contributes no binder for @b@; every use of
      -- @b@ is represented by @a@.  The relation is functional, so chains
      -- have a unique terminal representative.  For an internal alias cycle,
      -- choose the earliest planned binder deterministically and quotient the
      -- whole component to it.
      peerAliasNext =
        IntMap.fromList
          [ (sourceKey, targetKey)
          | (rawKey, _) <- orderedBinderRefsRaw,
            let source = canonical (NodeId rawKey),
            let sourceKey = getNodeId source,
            Just target0 <- [VarStore.lookupVarBound constraint source],
            let target = canonical target0,
            let targetKey = getNodeId target,
            sourceKey /= targetKey,
            IntSet.member targetKey orderedBinderKeys,
            -- A peer-variable alias is local to one binder spine.  A bound
            -- that points from a descendant scope to an enclosing result
            -- variable is construction routing (for example a lambda-body
            -- consumer), not authority to replace the descendant
            -- declaration with that exterior identity.
            IntMap.lookup (nodeRefKey (typeRef source)) (cBindParents constraint)
              == IntMap.lookup (nodeRefKey (typeRef target)) (cBindParents constraint),
            Just TyVar {} <- [lookupNodeIn (cNodes constraint) target]
          ]
      peerAliasRepresentativeKey start = go [] IntMap.empty start
        where
          go path seen current =
            case IntMap.lookup current seen of
              Just cycleStart -> earliestBinder (drop cycleStart path)
              Nothing ->
                case IntMap.lookup current peerAliasNext of
                  Just next
                    | next /= current ->
                        go
                          (path ++ [current])
                          (IntMap.insert current (length path) seen)
                          next
                  _ -> current

          earliestBinder [] = start
          earliestBinder (key : keys) = foldl earlier key keys
          earlier left right
            | rankOf left <= rankOf right = left
            | otherwise = right
          rankOf :: Int -> Int
          rankOf key = IntMap.findWithDefault maxBound key binderRank

      isPeerAliasSource rawKey =
        let key = getNodeId (canonical (NodeId rawKey))
         in not (isRequiredGammaKey rawKey)
              && not (isTermUsedRootBinderKey rawKey)
              && peerAliasRepresentativeKey key /= key
      isTermUsedRootBinderKey rawKey =
        any
          ( \(key, ref) ->
              key == rawKey && isTermUsedRootBinderRef ref
          )
          orderedBinderRefsRaw
      aliasRepresentativeKey key = go IntSet.empty key
        where
          go seen current
            | IntSet.member current seen = current
            | otherwise =
                case IntMap.lookup current gammaAliasPlan of
                  Just next
                    | next /= current -> go (IntSet.insert current seen) next
                  _ -> current
      normalizeAliasRef ref
        | isLocallyClosedGammaRef ref = ref
        | isTermUsedRootBinderRef ref = ref
        -- A required Gamma reference names the frozen source-domain exterior,
        -- even when its live solved key canonicalizes to the operated root.
        -- Quotienting it as an ordinary live alias would replace that
        -- construction identity with the operated identity and erase the
        -- paper's exterior > S'(operated) binder.
        | Just node <- typeBinderRefNode ref,
          Just aliasRef <- resolvedRequiredGammaAlias (getNodeId node) = aliasRef
        -- The exterior source node and the edge result can carry distinct
        -- graph keys while sharing the required Gamma identity.  A variable
        -- S'(operated) aliases that whole identity class to an existing
        -- lexical ref, so normalize every such occurrence rather than only
        -- the map's planning key.
        | Just aliasRef <- requiredGammaAliasForRef ref = aliasRef
        | isRequiredGammaRef ref = ref
        -- A reification route carries occurrence identity, not declaration
        -- authority.  When BinderPlan proves that a local variable is a
        -- peer-variable alias, quotient it to the planned representative
        -- before consulting the broader reification map; otherwise S' can
        -- preserve the alias occurrence after this function has correctly
        -- removed its redundant binder.
        | Just node <- typeBinderRefNode ref,
          not (isPeerAliasSource (getNodeId node)),
          Just routedRef <- reifiedRouteFor ref = routedRef
        | otherwise =
            case typeBinderRefNode ref of
              Nothing -> ref
              Just node ->
                let rawKey = getNodeId node
                    canonicalKey = getNodeId (canonical node)
                    peerKey = peerAliasRepresentativeKey canonicalKey
                    aliasKey =
                      if isPeerAliasSource rawKey
                        then peerKey
                        else
                          if IntMap.member peerKey gammaAliasPlan
                            then aliasRepresentativeKey peerKey
                            else
                              if IntMap.member canonicalKey gammaAliasPlan
                                then aliasRepresentativeKey canonicalKey
                                else aliasRepresentativeKey rawKey
                 in case
                      IntMap.lookup aliasKey substRaw
                        <|> IntMap.lookup peerKey orderedBinderRefsByKey
                    of
                      Just aliasRef -> aliasRef
                      Nothing -> ref
      normalizeAliasRefs :: Ty v -> Ty v
      normalizeAliasRefs = cataIx alg
        where
          alg :: TyIF i Ty -> Ty i
          alg ty = case ty of
            TVarIFRef ref -> TVarRef (normalizeAliasRef ref)
            TArrowIF dom cod -> TArrow dom cod
            TConIFWithIdentity identity con args -> TConWithIdentity identity con args
            TVarAppIFRef ref args -> TVarAppRef (normalizeAliasRef ref) args
            TBaseIFWithIdentity identity base -> TBaseWithIdentity identity base
            TBottomIF -> TBottom
            TForallIFRef ref mb body -> TForallRef (normalizeAliasRef ref) mb body
            TMuIFRef ref body -> TMuRef (normalizeAliasRef ref) body
      orderedBinderRefs =
        [ (nidInt, normalizeAliasRef ref)
        | (nidInt, ref) <- orderedBinderRefsRaw,
          not (isPeerAliasSource nidInt),
          requiredGammaAlias nidInt == Nothing
        ]
      bindings =
        [ (normalizeAliasRef ref, fmap normalizeAliasRefs mb)
          | ((nidInt, _), (ref, mb)) <- zip orderedBinderRefsRaw reifiedBindings,
            not (isPeerAliasSource nidInt),
            requiredGammaAlias nidInt == Nothing
        ]
      subst = IntMap.map normalizeAliasRef substRaw
      ty0Raw = normalizeAliasRefs fiTyRaw
      originalBinderRef nidInt ref =
        case IntMap.lookup nidInt subst of
          Just substRef -> substRef
          Nothing ->
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (canonical (NodeId nidInt)))
              (typeBinderRefName ref)
      originalBinderRefs =
        [ (nidInt, originalBinderRef nidInt ref)
          | (nidInt, ref) <- orderedBinderRefs
        ]
      aliasToTypeRootRefs =
        [ ref
        | (nidInt, ref) <- originalBinderRefs,
          not (isConstructionProtectedRef ref),
            let nid = NodeId nidInt,
            Just bnd <- [VarStore.lookupVarBound constraint (canonical nid)],
            canonical bnd == canonical typeRoot
        ]
      bindingMatchesRef ref (bindingRef, _) =
        typeBinderRefsSameIdentity bindingRef ref
      lookupBindingRef ref binds =
        [ mb
          | binding@(_, mb) <- binds,
            bindingMatchesRef ref binding
        ]
      ownedTypeRootBounds =
        [ (ref, bound)
        | (ref, Just bound) <- bindings,
          not (isConstructionProtectedRef ref),
            Just binder <- [typeBinderRefNode ref],
            Just boundRoot <- [VarStore.lookupVarBound constraint (canonical binder)],
            canonical boundRoot == canonical typeRoot,
            Just (TypeRef owner, _) <-
              [IntMap.lookup (nodeRefKey (typeRef (canonical boundRoot))) fiBindParents],
            canonical owner == canonical binder
        ]
      inlineAliasBinder :: ElabType -> [(TypeBinderRef, Maybe BoundType)] -> (ElabType, [(TypeBinderRef, Maybe BoundType)])
      inlineAliasBinder ty binds = case ty of
        TVarRef ref
          | refMember ref aliasToTypeRootRefs ->
              case lookupBindingRef ref binds of
                Just bnd : _
                  | not (isVarBound bnd),
                    not (isBaseBound bnd) ->
                      (tyToElab bnd, filter (not . bindingMatchesRef ref) binds)
                _ -> (ty, binds)
        _ -> (ty, binds)
      (ty0RawAlias, bindingsAlias) =
        case ownedTypeRootBounds of
          (aliasRef, bound) : _ ->
            (tyToElab bound, filter (not . bindingMatchesRef aliasRef) bindings)
          [] -> inlineAliasBinder ty0Raw bindings
      canonAllVars ty =
        let (ty', _freeEnv, _n) = go [] [] (0 :: Int) ty
         in ty'
        where
          freshCanonRef n ref =
            renameTypeBinderRef ("v" ++ show n) ref

          lookupCanonRef ref =
            lookupRefNameRef ref

          go boundEnv freeEnv n tyInput = case tyInput of
            TVarRef ref ->
              case lookupCanonRef ref boundEnv of
                Just ref' -> (TVarRef ref', freeEnv, n)
                Nothing ->
                  case lookupCanonRef ref freeEnv of
                    Just ref' -> (TVarRef ref', freeEnv, n)
                    Nothing ->
                      let ref' = freshCanonRef n ref
                       in (TVarRef ref', (ref, ref') : freeEnv, n + 1)
            TBaseWithIdentity identity b -> (TBaseWithIdentity identity b, freeEnv, n)
            TBottom -> (TBottom, freeEnv, n)
            TArrow a b ->
              let (a', free1, n1) = go boundEnv freeEnv n a
                  (b', free2, n2) = go boundEnv free1 n1 b
               in (TArrow a' b', free2, n2)
            TConWithIdentity identity c (arg :| args) ->
              let (arg', free1, n1) = go boundEnv freeEnv n arg
                  (argsRev, free2, n2) =
                    foldl
                      ( \(acc, freeAcc, nAcc) a ->
                          let (a', free', n') = go boundEnv freeAcc nAcc a
                           in (a' : acc, free', n')
                      )
                      ([], free1, n1)
                      args
                  args' = reverse argsRev
               in (TConWithIdentity identity c (arg' :| args'), free2, n2)
            TVarAppRef ref (arg :| args) ->
              let (headTy, free1, n1) = go boundEnv freeEnv n (TVarRef ref)
                  ref' = case headTy of
                    TVarRef refHead -> refHead
                    _ -> ref
                  (arg', freeArg, nArg) = go boundEnv free1 n1 arg
                  (argsRev, free2, n2) =
                    foldl
                      ( \(acc, freeAcc, nAcc) a ->
                          let (a', free', n') = go boundEnv freeAcc nAcc a
                           in (a' : acc, free', n')
                      )
                      ([], freeArg, nArg)
                      args
                  args' = reverse argsRev
               in (TVarAppRef ref' (arg' :| args'), free2, n2)
            TForallRef ref mb body ->
              let ref' = freshCanonRef n ref
                  n1 = n + 1
                  (mb', free1, n2) =
                    case mb of
                      Nothing -> (Nothing, freeEnv, n1)
                      Just bnd ->
                        let (bnd', free', n') = goBound boundEnv freeEnv n1 bnd
                         in (Just bnd', free', n')
                  (body', free2, n3) = go ((ref, ref') : boundEnv) free1 n2 body
               in (TForallRef ref' mb' body', free2, n3)
            TMuRef ref body ->
              let ref' = freshCanonRef n ref
                  n1 = n + 1
                  (body', free1, n2) = go ((ref, ref') : boundEnv) freeEnv n1 body
               in (TMuRef ref' body', free1, n2)

          goBound boundEnv freeEnv n bound = case bound of
            TArrow a b ->
              let (a', free1, n1) = go boundEnv freeEnv n a
                  (b', free2, n2) = go boundEnv free1 n1 b
               in (TArrow a' b', free2, n2)
            TConWithIdentity identity c (arg :| args) ->
              let (arg', free1, n1) = go boundEnv freeEnv n arg
                  (argsRev, free2, n2) =
                    foldl
                      ( \(acc, freeAcc, nAcc) a ->
                          let (a', free', n') = go boundEnv freeAcc nAcc a
                           in (a' : acc, free', n')
                      )
                      ([], free1, n1)
                      args
                  args' = reverse argsRev
               in (TConWithIdentity identity c (arg' :| args'), free2, n2)
            TVarAppRef ref (arg :| args) ->
              let (headTy, free1, n1) = go boundEnv freeEnv n (TVarRef ref)
                  ref' = case headTy of
                    TVarRef refHead -> refHead
                    _ -> ref
                  (arg', freeArg, nArg) = go boundEnv free1 n1 arg
                  (argsRev, free2, n2) =
                    foldl
                      ( \(acc, freeAcc, nAcc) a ->
                          let (a', free', n') = go boundEnv freeAcc nAcc a
                           in (a' : acc, free', n')
                      )
                      ([], freeArg, nArg)
                      args
                  args' = reverse argsRev
               in (TVarAppRef ref' (arg' :| args'), free2, n2)
            TBaseWithIdentity identity b -> (TBaseWithIdentity identity b, freeEnv, n)
            TBottom -> (TBottom, freeEnv, n)
            TForallRef ref mb body ->
              let ref' = freshCanonRef n ref
                  n1 = n + 1
                  (mb', free1, n2) =
                    case mb of
                      Nothing -> (Nothing, freeEnv, n1)
                      Just bnd ->
                        let (bnd', free', n') = goBound boundEnv freeEnv n1 bnd
                         in (Just bnd', free', n')
                  (body', free2, n3) = go ((ref, ref') : boundEnv) free1 n2 body
               in (TForallRef ref' mb' body', free2, n3)
            TMuRef ref body ->
              let ref' = freshCanonRef n ref
                  n1 = n + 1
                  (body', free1, n2) = go ((ref, ref') : boundEnv) freeEnv n1 body
               in (TMuRef ref' body', free1, n2)
      replaceAlias boundNorm ref = goReplace
        where
          goReplace ty
            | canonAllVars ty == boundNorm = TVarRef ref
            | otherwise =
                case ty of
                  TArrow a b -> TArrow (goReplace a) (goReplace b)
                  TConWithIdentity identity c args -> TConWithIdentity identity c (fmap goReplace args)
                  TVarAppRef headRef args -> TVarAppRef headRef (fmap goReplace args)
                  TForallRef binderRef mb body ->
                    TForallRef binderRef (fmap (mapBoundType goReplace) mb) (goReplace body)
                  TMuRef binderRef body -> TMuRef binderRef (goReplace body)
                  _ -> ty
      stripAliasForall ty = case ty of
        TForallRef ref (Just bound) body
          | TVarRef bodyRef <- body,
            typeBinderRefsSameIdentity ref bodyRef ->
              stripAliasForall (tyToElab bound)
          | otherwise ->
              TForallRef ref (Just (stripAliasForallBound bound)) (stripAliasForall body)
        TForallRef ref Nothing body ->
          TForallRef ref Nothing (stripAliasForall body)
        TArrow a b -> TArrow (stripAliasForall a) (stripAliasForall b)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap stripAliasForall args)
        TVarAppRef ref args -> TVarAppRef ref (fmap stripAliasForall args)
        TMuRef ref body -> TMuRef ref (stripAliasForall body)
        _ -> ty
      stripAliasForallBound bound = case bound of
        TArrow a b -> TArrow (stripAliasForall a) (stripAliasForall b)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap stripAliasForall args)
        TVarAppRef ref args -> TVarAppRef ref (fmap stripAliasForall args)
        TBaseWithIdentity _ _ -> bound
        TBottom -> bound
        TForallRef ref mb body ->
          let mb' = fmap stripAliasForallBound mb
              body' = stripAliasForall body
           in TForallRef ref mb' body'
        TMuRef ref body -> TMuRef ref (stripAliasForall body)
      collapseBoundAliases binds ty =
        foldr
          ( \(v, mbBound) acc ->
              case mbBound of
                Nothing -> acc
                Just _ | isConstructionProtectedRef v -> acc
                Just bound ->
                  let boundTy = tyToElab bound
                      boundCore = stripForallsType bound
                      boundTyNorm = canonAllVars boundTy
                   in if isVarBound boundCore
                        then acc
                        else
                          let boundNorm = canonAllVars boundCore
                           in if canonAllVars acc == boundNorm
                                || canonAllVars acc == boundTyNorm
                                then acc
                                else replaceAlias boundNorm v acc
          )
          ty
          binds
      normalizeScheme tyRaw binds =
        let tyAdjusted0 =
              case (binds, tyRaw) of
                ((bindingRef, mb) : _, TForallRef ref mb' body)
                  | typeBinderRefsSameIdentity bindingRef ref && mb == mb' -> body
                _ -> tyRaw
            tyAdjusted =
              case stripForallsType tyAdjusted0 of
                TVarRef ref ->
                  case lookupBindingRef ref binds of
                    Just bound : _
                      | not (isConstructionProtectedRef ref)
                      , containsForall (tyToElab bound) -> tyToElab bound
                    _ -> tyAdjusted0
                _ -> tyAdjusted0
            tyAliased = stripAliasForall (collapseBoundAliases binds tyAdjusted)
         in traceGeneralize
              env
              ( "generalizeAt: ty0Raw="
                  ++ show tyAliased
                  ++ " subst="
                  ++ show subst
                  ++ " bindings="
                  ++ show binds
              )
              (tyAliased, binds)
      (ty0RawAdjusted, bindingsAdjusted) = normalizeScheme ty0RawAlias bindingsAlias
      isOwnedStructuredAliasBinder nidInt =
        let nid = canonical (NodeId nidInt)
         in case VarStore.lookupVarBound constraint nid of
              Just bnd0 ->
                let bnd = canonical bnd0
                    binderIsTypeRoot = nid == canonical typeRoot
                    boundOwnedByBinder =
                      case IntMap.lookup (nodeRefKey (typeRef bnd)) fiBindParents of
                        Just (TypeRef parent, _) -> canonical parent == nid
                        _ -> False
                    boundIsStructured =
                      case lookupNodeIn (cNodes constraint) bnd of
                        Just TyArrow {} -> True
                        Just TyCon {} -> True
                        Just TyVarApp {} -> True
                        Just TyForall {} -> True
                        Just TyMu {} -> True
                        Just TyExp {} -> True
                        _ -> False
                 in not binderIsTypeRoot && boundOwnedByBinder && boundIsStructured
              Nothing -> False
      namedBinderRefs =
        unionTypeRefs
          [ requiredGammaRefs
          , rootBodyClosureRefs
          , fiTermUsedRootBinderRefs
          , [ ref
            | (nidInt, ref) <- IntMap.toList subst,
              IntSet.member nidInt namedUnderGaSetPlan,
              not (isOwnedStructuredAliasBinder nidInt)
            ]
          ]
      renameVars = cataIx alg
        where
          renameRefFromSubst ref =
            case
                find
                  (typeBinderRefsSameIdentity ref)
                  (IntMap.elems subst)
                  <|> do
                    node <- typeBinderRefNode ref
                    let nodeKey = getNodeId node
                        canonicalKey =
                          getNodeId (canonical node)
                        aliasKey =
                          IntMap.findWithDefault
                            canonicalKey
                            canonicalKey
                            gammaAliasPlan
                    IntMap.lookup nodeKey subst
                      <|> IntMap.lookup aliasKey subst
                      <|> IntMap.lookup canonicalKey subst
              of
                Just routedRef -> routedRef
                Nothing -> ref
          alg :: TyIF i Ty -> Ty i
          alg ty = case ty of
            TVarIFRef ref -> TVarRef (renameRefFromSubst ref)
            TArrowIF a b -> TArrow a b
            TConIFWithIdentity identity c args -> TConWithIdentity identity c args
            TVarAppIFRef ref args -> TVarAppRef (renameRefFromSubst ref) args
            TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
            TBottomIF -> TBottom
            TForallIFRef ref mb body -> TForallRef (renameRefFromSubst ref) mb body
            TMuIFRef ref body -> TMuRef (renameRefFromSubst ref) body
      ty0 = renameVars ty0RawAdjusted
      weakenedBinderRefs =
        [ ref
        | (nidInt, ref) <- originalBinderRefs,
          IntSet.member
            (getNodeId (canonical (NodeId nidInt)))
            (cWeakenedVars constraint)
        ]
      shouldInlineBaseBound ref = refMember ref weakenedBinderRefs
      bindingsAdjustedRefs =
        bindingsAdjusted
      (bindingsNorm0Refs, tyNorm0) =
        simplifySchemeBindingsRefsWhenPreserving
          shouldInlineBaseBound
          isConstructionProtectedRef
          namedBinderRefs
          bindingsAdjustedRefs
          ty0
      (bindingsNorm1Refs, tyNorm1) =
        promoteArrowAliasRefsWhen
          (not . isConstructionProtectedRef)
          bindingsNorm0Refs
          tyNorm0
      (bindingsNormRefs, tyNorm) = (bindingsNorm1Refs, tyNorm1)
      bindingsNorm =
        [ (typeBinderRefName ref, mb)
          | (ref, mb) <- bindingsNormRefs
        ]
      quantifiedRefs = map fst (fst (splitForallsRefs tyNorm))
      usedRefs =
        unionTypeRefs
          ( quantifiedRefs
              : freeTypeVarRefsType tyNorm
              : [freeTypeVarRefsType b | (_, Just b) <- bindingsNormRefs]
          )
      usedNames = Set.fromList (map typeBinderRefName usedRefs)
      bindingsFinalRefs =
        filter
          ( \(ref, _) ->
              refMember ref usedRefs
                || refMember ref namedBinderRefs
                || isConstructionProtectedRef ref
          )
          bindingsNormRefs
      bindingsFinalRefs' =
        let dropRedundant (ref, mb) =
              not (isConstructionProtectedRef ref)
                && not (isRootBodyClosureRef ref)
                && not (refMember ref usedRefs)
                && case mb of
                  Nothing -> True
                  Just bnd ->
                    let freeBound = freeTypeVarRefsType bnd
                        boundMentionsSelf = refMember ref freeBound
                        boundIsSimple = isVarBound bnd || isBaseBound bnd
                        boundIsBody = tyToElab bnd == tyNorm
                     in not boundMentionsSelf && (boundIsSimple || boundIsBody)
         in filter (not . dropRedundant) bindingsFinalRefs
      bindingsFinal' =
        [ (typeBinderRefName ref, mb)
          | (ref, mb) <- bindingsFinalRefs'
        ]
      aliasBounds =
        [ (typeBinderRefName ref, bound)
          | (ref, Just bound) <- bindingsFinalRefs',
            isVarBound bound
        ]
      renamePairs =
        [ (ref, renameTypeBinderRef (alphaName idx 0) ref)
        | (idx, (ref, _)) <- zip [0 ..] bindingsFinalRefs'
        ]
      renameRefFromMap ref =
        case
            [ renamedRef
            | (candidateRef, renamedRef) <- renamePairs
            , typeBinderRefsSameIdentity ref candidateRef
            ]
          of
            renamedRef : _ -> renamedRef
            [] ->
              case typeBinderIdentityStructural (typeBinderRefIdentity ref) of
                Just structuralIdentity ->
                  case
                      [ renamedRef
                      | (candidateRef, renamedRef) <- renamePairs
                      , typeBinderIdentityStructural
                          (typeBinderRefIdentity candidateRef)
                          == Just structuralIdentity
                      ]
                    of
                      [renamedRef] -> renamedRef
                      _ -> ref
                Nothing -> ref
      renameTypeVars :: ElabType -> ElabType
      renameTypeVars = cataIx alg
        where
          alg :: TyIF i Ty -> Ty i
          alg ty = case ty of
            TVarIFRef ref -> TVarRef (renameRefFromMap ref)
            TArrowIF a b -> TArrow a b
            TConIFWithIdentity identity c args -> TConWithIdentity identity c args
            TVarAppIFRef ref args -> TVarAppRef (renameRefFromMap ref) args
            TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
            TBottomIF -> TBottom
            TForallIFRef ref mb body -> TForallRef (renameRefFromMap ref) mb body
            TMuIFRef ref body -> TMuRef (renameRefFromMap ref) body
      bindingsRenamedRefs =
        [ (renameRefFromMap ref, fmap (mapBoundType renameTypeVars) mb)
          | (ref, mb) <- bindingsFinalRefs'
        ]
      bindingsRenamed =
        [ (typeBinderRefName ref, mb)
          | (ref, mb) <- bindingsRenamedRefs
        ]
      tyRenamed = renameTypeVars tyNorm
      traceFinal =
        traceGeneralize
          env
          ( "generalizeAt: tyNorm="
              ++ show tyNorm
              ++ " usedNames="
              ++ show (Set.toList usedNames)
              ++ " bindingsNorm="
              ++ show bindingsNorm
              ++ " bindingsFinal="
              ++ show bindingsFinal'
              ++ " bindingsRenamed="
              ++ show bindingsRenamed
          )

      keepRefs = map fst bindingsRenamedRefs
      subst' =
        IntMap.filterWithKey
          (\key ref -> refMember ref keepRefs || IntSet.member key requiredGammaAliasBridgeKeys) $
          IntMap.map renameRefFromMap subst
      schemeFinal = mkElabSchemeWithRefs bindingsRenamedRefs tyRenamed
      plannedBinderRefs = map snd orderedBinderRefs
      unplannedFinalBinderRefs =
        [ ref
        | (ref, _) <- bindingsRenamedRefs,
          not (refMember ref plannedBinderRefs)
        ]
      closureAuthority =
        ambientSchemeClosureAuthority fiAmbientBinderRefs
          <> inheritedGammaSchemeClosureAuthority
          (inheritedGammaPlanAuthorizedRefs fiInheritedGammaPlan)
          <> locallyClosedGammaSchemeClosureAuthority fiLocallyClosedGammaRefs
          <> requiredGammaAliasSchemeClosureAuthority
            (IntMap.elems fiRequiredGammaAliases)
      missingRefs = schemeClosureFreeRefs closureAuthority schemeFinal
   in traceFinal $
        if not (null aliasBounds)
          then
            Left $
              ValidationFailed
                [ "alias bounds survived scheme finalization: "
                    ++ show (map fst aliasBounds)
                ]
          else
            if not (null unplannedFinalBinderRefs)
              then
                Left
                  ( ValidationFailed
                      [ "scheme finalization published binders without BinderPlan authority"
                      , "  unauthorized binders: " ++ show unplannedFinalBinderRefs
                      , "  planner binders: " ++ show plannedBinderRefs
                      ]
                  )
              else
                if null missingRefs
                  then pure (schemeFinal, subst')
                  else
                    Left
                      ( SchemeFreeVars
                          typeRoot
                          (map show missingRefs)
                      )

refMember :: TypeBinderRef -> [TypeBinderRef] -> Bool
refMember ref = any (typeBinderRefsSameIdentity ref)

insertTypeRef :: TypeBinderRef -> [TypeBinderRef] -> [TypeBinderRef]
insertTypeRef ref refs
  | refMember ref refs = refs
  | otherwise = ref : refs

unionTypeRefs :: [[TypeBinderRef]] -> [TypeBinderRef]
unionTypeRefs = foldr (foldr insertTypeRef) []

lookupRefNameRef :: TypeBinderRef -> [(TypeBinderRef, TypeBinderRef)] -> Maybe TypeBinderRef
lookupRefNameRef _ [] = Nothing
lookupRefNameRef ref ((candidate, renamed) : rest)
  | typeBinderRefsSameIdentity ref candidate = Just renamed
  | otherwise = lookupRefNameRef ref rest
