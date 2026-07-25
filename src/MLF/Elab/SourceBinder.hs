{-# LANGUAGE GADTs #-}

module MLF.Elab.SourceBinder
  ( resolveConstructionSourceBindersInType,
    resolveConstructionSourceBindersInTypeExcept,
    resolveConstructionSourceBindersInTypeAtExpected,
    resolveConstructionSourceBindersInPacketAtExpected,
    resolveConstructionSourceBindersInSchemeInfo,
    resolveConstructionSourceBindersInSchemeInfoExcept,
    typeBinderDeclarationRefs,
    orderSourceProjectedSchemeBinders,
    resolveSourceBinderAliasesInType,
    sourceBinderAliasSubstitution,
    sourceBinderConstructionRenames,
  )
where

import Control.Monad (foldM)
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (NodeId (..), getNodeId)
import MLF.Elab.Types
  ( ElabScheme,
    ElabType,
    SchemeInfo (..),
    Ty (..),
    TypeBinderRef,
    elabToBound,
    mkElabSchemeWithRefs,
    schemeBinderRefs,
    schemeBody,
    schemeInfoBinderRefSubst,
    schemeInfoFromRefSubst,
    typeBinderRefIdentity,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
    tyToElab,
  )
import MLF.Elab.Inst (schemeToType)
import MLF.Reify.TypeOps
  ( alphaEqType,
    churchAwareEqType,
    freeTypeVarRefsType,
    splitForallsRefs,
    substTypeCaptureRef,
    substTypeSimpleRef,
  )
import MLF.Types.Identity
  ( TypeBinderIdentity,
    typeBinderIdentityGeneratedUnique,
  )
import MLF.Util.Graph (topoSortBy)

-- | Collect identities introduced by forall and recursive-type declarations,
-- including declarations nested in bounds.  These refs are ownership
-- authority; an equal generated ref in a source sidecar is not by itself
-- evidence that the declaration is inherited from an enclosing scope.
typeBinderDeclarationRefs :: ElabType -> [TypeBinderRef]
typeBinderDeclarationRefs ty =
  case ty of
    TVarRef _ -> []
    TArrow domain codomain ->
      typeBinderDeclarationRefs domain ++ typeBinderDeclarationRefs codomain
    TConWithIdentity _ _ args ->
      concatMap typeBinderDeclarationRefs (toList args)
    TVarAppRef _ args ->
      concatMap typeBinderDeclarationRefs (toList args)
    TBaseWithIdentity _ _ -> []
    TBottom -> []
    TForallRef ref mbBound body ->
      ref
        : maybe [] (typeBinderDeclarationRefs . tyToElab) mbBound
          ++ typeBinderDeclarationRefs body
    TMuRef ref body ->
      ref : typeBinderDeclarationRefs body

-- | Compose the two identity-bearing sides of a prepared construction:
-- graph node -> source binder and graph node -> outward Gamma binder.
--
-- The source sidecar and construction Gamma are produced at different phase
-- boundaries, so comparing their generated identities directly loses the
-- common graph owner.  Return an explicit source-to-Gamma renaming and reject
-- any source identity that would be routed to two different outward binders.
sourceBinderConstructionRenames
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> Either String [(TypeBinderRef, TypeBinderRef)]
sourceBinderConstructionRenames representative sourceBinderRefs constructionAliases =
  foldM addRoute [] (IntMap.toList sourceBinderRefs)
  where
    addRoute routes (nodeKey, sourceRef)
      | not (isJust (typeBinderIdentityGeneratedUnique (typeBinderRefIdentity sourceRef))) =
          pure routes
      | otherwise = do
          mbOutwardRef <- constructionRoute sourceRef (NodeId nodeKey)
          case mbOutwardRef of
            Nothing -> pure routes
            Just outwardRef
              | typeBinderRefsSameIdentity sourceRef outwardRef -> pure routes
              | otherwise ->
                  case findSourceRoute sourceRef routes of
                    Nothing -> pure ((sourceRef, outwardRef) : routes)
                    Just existing
                      | typeBinderRefsSameIdentity existing outwardRef -> pure routes
                      | otherwise ->
                          Left
                            ( "source binder construction route is ambiguous: source="
                                ++ show sourceRef
                                ++ ", first="
                                ++ show existing
                                ++ ", second="
                                ++ show outwardRef
                            )

    constructionRoute sourceRef node =
      case IntMap.lookup (getNodeId node) constructionAliases of
        Just direct -> pure (Just direct)
        Nothing -> uniqueRepresentativeRoute sourceRef (representative node)

    uniqueRepresentativeRoute sourceRef target =
      case findSameIdentity sourceRef outwardRefs of
        Just sourceOutwardRef -> pure (Just sourceOutwardRef)
        Nothing ->
          case outwardRefs of
            [] -> pure Nothing
            [outwardRef] -> pure (Just outwardRef)
            _ ->
                Left
                ( "construction Gamma representative has multiple outward binders: source="
                    ++ show sourceRef
                    ++ ", representative="
                    ++ show target
                    ++ ", binders="
                    ++ show outwardRefs
                )
      where
        outwardRefs = foldr insertDistinct [] representativeCandidates
        representativeCandidates =
          [ outwardRef
          | (nodeKey, outwardRef) <- IntMap.toList constructionAliases,
            representative (NodeId nodeKey) == target
          ]

        -- Canonical graph representatives can contain more than one lexical
        -- construction binder.  If one of those binders already has the
        -- source identity being routed, no quotient is needed for that source;
        -- choosing a different representative peer would erase a distinction
        -- that the identity-bearing construction has already made explicit.
        findSameIdentity source = go
          where
            go [] = Nothing
            go (candidate : rest)
              | typeBinderRefsSameIdentity source candidate = Just candidate
              | otherwise = go rest

    insertDistinct ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    findSourceRoute sourceRef routes =
      case
          [ outwardRef
          | (existingSourceRef, outwardRef) <- routes,
            typeBinderRefsSameIdentity existingSourceRef sourceRef
          ]
        of
          outwardRef : _ -> Just outwardRef
          [] -> Nothing

-- | Project a prepared scheme and every graph occurrence in its substitution
-- through the same source-binder carrier.  Annotation elaboration consumes
-- both views, so changing only the body would leave its edge replay in a
-- different identity domain.
constructionSourceBinderRefsForSchemeInfo
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Either String (IntMap.IntMap TypeBinderRef)
constructionSourceBinderRefsForSchemeInfo protectedIdentities representative sourceBinderRefs schemeInfo =
  foldM
    addPacketSourceRoute
    sourceBinderRefs
    (IntMap.toList (schemeInfoBinderRefSubst schemeInfo))
  where
    -- A packet substitution is an identity-bearing bridge: the same graph key
    -- names both the packet-local reification ref and the source sidecar ref.
    -- Join those views by key before inspecting the packet type.  Solved graph
    -- representatives cannot express this instantiation/reification alias.
    addPacketSourceRoute refs (graphKey, packetRef) =
      if Set.member (typeBinderRefIdentity packetRef) protectedIdentities
        then pure refs
        else
          case sourceRefAtGraphKey graphKey of
            Nothing -> pure refs
            Just sourceRef
              | typeBinderRefsSameIdentity packetRef sourceRef -> pure refs
              | Just packetNode <- typeBinderRefNode packetRef ->
                  insertPacketSourceRef packetNode sourceRef refs
              | isJust
                  ( typeBinderIdentityGeneratedUnique
                      (typeBinderRefIdentity packetRef)
                  ) ->
                  Left
                    ( "construction packet graph key routes to two generated source identities: key="
                        ++ show (NodeId graphKey)
                        ++ ", packet="
                        ++ show packetRef
                        ++ ", source="
                        ++ show sourceRef
                    )
              | otherwise -> pure refs

    sourceRefAtGraphKey graphKey =
      case IntMap.lookup graphKey sourceBinderRefs of
        Just directRef -> generatedSourceRef directRef
        Nothing ->
          IntMap.lookup
            (getNodeId (representative (NodeId graphKey)))
            sourceBinderRefs
            >>= generatedSourceRef

    insertPacketSourceRef packetNode sourceRef refs =
      case IntMap.lookup (getNodeId packetNode) refs of
        Nothing ->
          pure (IntMap.insert (getNodeId packetNode) sourceRef refs)
        Just existing
          | typeBinderRefsSameIdentity existing sourceRef -> pure refs
          | otherwise ->
              Left
                ( "construction packet identity routes to multiple source binders: packet="
                    ++ show packetNode
                    ++ ", first="
                    ++ show existing
                    ++ ", second="
                    ++ show sourceRef
                )

    generatedSourceRef ref
      | isJust
          ( typeBinderIdentityGeneratedUnique
              (typeBinderRefIdentity ref)
          ) = Just ref
      | otherwise = Nothing

resolveConstructionSourceBindersInSchemeInfo
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Either String SchemeInfo
resolveConstructionSourceBindersInSchemeInfo =
  resolveConstructionSourceBindersInSchemeInfoExcept Set.empty

-- | Resolve a construction packet while retaining identities whose current
-- owner is itself being constructed.  Such refs are declaration authority,
-- not lexical source aliases, even when their graph representative shares a
-- source carrier.
resolveConstructionSourceBindersInSchemeInfoExcept
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Either String SchemeInfo
resolveConstructionSourceBindersInSchemeInfoExcept protectedIdentities representative sourceBinderRefs schemeInfo = do
  packetSourceBinderRefs <-
    constructionSourceBinderRefsForSchemeInfo
      protectedIdentities
      representative
      sourceBinderRefs
      schemeInfo
  resolvedType <-
    resolveConstructionSourceBindersInTypeWithExpected
      Nothing
      protectedIdentities
      representative
      packetSourceBinderRefs
      (schemeToType (siScheme schemeInfo))
  let originalOuterBinders = schemeBinderRefs (siScheme schemeInfo)
      survivingOuterRefs =
        [ ref
        | binder@(ref, _) <- originalOuterBinders
        , outerBinderSurvives packetSourceBinderRefs binder
        ]
      (resolvedSpine, resolvedBody) = splitForallsRefs resolvedType
      (resolvedOuterBinders, resolvedBodyBinders) =
        splitAt (length survivingOuterRefs) resolvedSpine
  if
      length resolvedOuterBinders /= length survivingOuterRefs
        || not
          ( and
              ( zipWith
                  typeBinderRefsSameIdentity
                  survivingOuterRefs
                  (map fst resolvedOuterBinders)
              )
          )
    then
      Left
        ( "construction source-binder projection changed the explicit scheme ownership partition: expected="
            ++ show survivingOuterRefs
            ++ ", resolved="
            ++ show (map fst resolvedOuterBinders)
        )
    else do
      let resolvedScheme =
            mkElabSchemeWithRefs
              resolvedOuterBinders
              (foldr (uncurry TForallRef) resolvedBody resolvedBodyBinders)
          survivingLocalBinders = typeBinderDeclarationRefs resolvedType
          projectRef ref
            | Set.member (typeBinderRefIdentity ref) protectedIdentities = ref
            | any (typeBinderRefsSameIdentity ref) survivingLocalBinders = ref
            | otherwise =
                maybe ref id (sourceAliasForRef packetSourceBinderRefs ref)
      orderedResolvedScheme <-
        orderSourceProjectedSchemeBinders
          "construction source-binder projection"
          resolvedScheme
      pure
        ( schemeInfoFromRefSubst
            orderedResolvedScheme
            (IntMap.map projectRef (schemeInfoBinderRefSubst schemeInfo))
        )
  where
    outerBinderSurvives refs (ref, mbBound) =
      case (mbBound, sourceAliasForRef refs ref) of
        -- An unbounded binder routed by the source sidecar belongs to the
        -- enclosing source scope even when an earlier projection has already
        -- installed that exact generated identity in the slot.  Retaining it
        -- would turn an inherited source variable into a fresh local forall.
        (Nothing, Just _) -> False
        _ -> True

    sourceAliasForRef refs ref = do
      if Set.member (typeBinderRefIdentity ref) protectedIdentities
        then Nothing
        else pure ()
      case directSourceAlias refs ref of
        Just sourceRef -> Just sourceRef
        Nothing ->
          case sourceAliasFromSubstitution refs ref of
            Just sourceRef -> Just sourceRef
            Nothing -> exactIdentitySourceAlias refs ref

    directSourceAlias refs ref = do
      node <- typeBinderRefNode ref
      case IntMap.lookup (getNodeId node) refs of
        Just directRef -> generatedSourceRef directRef
        Nothing ->
          IntMap.lookup
            (getNodeId (representative node))
            refs
            >>= generatedSourceRef

    -- Once a graph binder has already adopted its exact source identity it
    -- no longer has a graph node of its own.  The SchemeInfo key route is the
    -- remaining construction proof that the exact binder is inherited.
    sourceAliasFromSubstitution refs ref =
      listToMaybe
        [ sourceRef
        | (graphKey, packetRef) <-
            IntMap.toList (schemeInfoBinderRefSubst schemeInfo)
        , typeBinderRefsSameIdentity packetRef ref
        , Just sourceRef <- [IntMap.lookup graphKey refs >>= generatedSourceRef]
        , typeBinderRefsSameIdentity sourceRef ref
        ]

    -- The complete packet type and the outer Scheme ownership partition must
    -- consult the same already-projected source authority.  Otherwise the
    -- type resolver removes an inherited generated forall while this wrapper
    -- still counts it as a surviving local declaration.
    exactIdentitySourceAlias refs ref =
      listToMaybe
        [ sourceRef
        | sourceRef <- IntMap.elems refs
        , typeBinderRefsSameIdentity sourceRef ref
        , isJust
            ( typeBinderIdentityGeneratedUnique
                (typeBinderRefIdentity sourceRef)
            )
        ]

    generatedSourceRef ref
      | isJust
          ( typeBinderIdentityGeneratedUnique
              (typeBinderRefIdentity ref)
          ) = Just ref
      | otherwise = Nothing

-- | Restore lexical bound-dependency order after graph binders have been
-- projected to exact source identities.  Projection can make a reference
-- which was previously free become bound by a later slot in the same scheme;
-- that slot must move before every bound that consumes it.  Unrelated slots
-- retain their original order, and matching is exclusively by binder
-- identity.
orderSourceProjectedSchemeBinders
  :: String
  -> ElabScheme
  -> Either String ElabScheme
orderSourceProjectedSchemeBinders role scheme = do
  orderedIndices <-
    case topoSortBy cycleLabel compare dependenciesFor binderIndices of
      Right ordered -> Right ordered
      Left _ ->
        Left
          ( cycleLabel
              ++ ": role="
              ++ role
              ++ ", scheme="
              ++ show scheme
          )
  orderedBinders <- traverse binderAt orderedIndices
  pure (mkElabSchemeWithRefs orderedBinders (schemeBody scheme))
 where
  binders = schemeBinderRefs scheme
  binderMap = IntMap.fromList (zip [0 ..] binders)
  binderIndices = IntMap.keys binderMap
  cycleLabel = "source-identity binder projection has cyclic bound dependencies"

  binderAt index =
    case IntMap.lookup index binderMap of
      Just binder -> Right binder
      Nothing -> Left ("source-identity binder ordering lost slot " ++ show index)

  dependenciesFor index =
    case IntMap.lookup index binderMap of
      Just (_, Just bound) ->
        [ dependencyIndex
        | (dependencyIndex, (candidateRef, _)) <- IntMap.toList binderMap
        , dependencyIndex /= index
        , any
            (`typeBinderRefsSameIdentity` candidateRef)
            (freeTypeVarRefsType (tyToElab bound))
        ]
      _ -> []

-- | Resolve a prepared packet type against the source-owned identities in
-- its enclosing construction Γ.  Packet reification can close a lexical
-- source binder as a graph-local unbounded forall before the exact owner is
-- known.  Once the graph representative routes to an outward source binder,
-- remove that quantifier and substitute the outward identity immediately;
-- this is the construction proof that the binder is captured, not locally
-- generalized.  A bounded packet forall remains locally owned: its graph
-- identity is alpha-local and must not be mistaken for an outward capture.
resolveConstructionSourceBindersInType
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> Either String ElabType
resolveConstructionSourceBindersInType representative sourceBinderRefs ty = do
  resolveConstructionSourceBindersInTypeExcept
    Set.empty
    representative
    sourceBinderRefs
    ty

-- | Resolve captured source binders while retaining declarations whose exact
-- constructor is already known.  This is the type-level counterpart of
-- 'resolveConstructionSourceBindersInSchemeInfoExcept': a closed descendant
-- packet supplies positive ownership for its forall/mu declarations, so an
-- equal generated identity in the source sidecar cannot reopen them into the
-- enclosing Gamma.
resolveConstructionSourceBindersInTypeExcept
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> Either String ElabType
resolveConstructionSourceBindersInTypeExcept protectedIdentities representative sourceBinderRefs ty =
  resolveConstructionSourceBindersInTypeWithExpected
    Nothing
    protectedIdentities
    representative
    sourceBinderRefs
    ty

-- | Resolve a packet at a compiler-exact endpoint.  A bounded packet forall
-- may adopt the source binder identity only when the fully resolved packet is
-- equal to the independently supplied exact type.  The exact endpoint is the
-- missing bound-equality proof; the ordinary resolver remains fail-closed.
resolveConstructionSourceBindersInTypeAtExpected
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> ElabType
  -> Either String ElabType
resolveConstructionSourceBindersInTypeAtExpected representative sourceBinderRefs expected ty =
  resolveConstructionSourceBindersInTypeWithExpected
    (Just expected)
    Set.empty
    representative
    sourceBinderRefs
    ty

-- | Resolve an operated packet at an exact edge whose source endpoint records
-- only the packet body.  The packet's leading foralls are construction-owned:
-- they must not be discarded merely because the edge consumes the body view.
--
-- The ordinary exact resolver remains the first and preferred path.  The
-- body-view path is available only when an explicit packet binder identity is
-- free in the exact endpoint, which is the occurrence-local proof that the
-- endpoint lies underneath this packet spine.  After aligning the body, use
-- the packet refs themselves for those occurrences and rebuild the original
-- spine; no binder is invented from endpoint shape.
resolveConstructionSourceBindersInPacketAtExpected
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> ElabType
  -> Either String ElabType
resolveConstructionSourceBindersInPacketAtExpected representative sourceBinderRefs expected packet =
  case
      resolveConstructionSourceBindersInTypeAtExpected
        representative
        sourceBinderRefs
        expected
        packet
    of
      Right aligned -> Right aligned
      Left wholePacketMismatch ->
        case splitForallsRefs packet of
          ([], _) -> Left wholePacketMismatch
          (packetBinders, packetBody)
            | packetSpineOwnsEndpoint packetBinders -> do
                alignedBody <-
                  resolveConstructionSourceBindersInTypeAtExpected
                    representative
                    sourceBinderRefs
                    expected
                    packetBody
                pure
                  ( foldr
                      (uncurry TForallRef)
                      (alignBodyRefs packetBinders alignedBody)
                      packetBinders
                  )
            | otherwise -> Left wholePacketMismatch
  where
    expectedFreeRefs = freeTypeVarRefsType expected

    packetSpineOwnsEndpoint packetBinders =
      any
        ( \(packetRef, _) ->
            any (typeBinderRefsSameIdentity packetRef) expectedFreeRefs
        )
        packetBinders

    alignBodyRefs packetBinders body0 =
      foldl'
        ( \body (packetRef, _) ->
            foldl'
              ( \bodyAcc exactRef ->
                  if typeBinderRefsSameIdentity packetRef exactRef
                    then
                      substTypeCaptureRef
                        exactRef
                        (TVarRef packetRef)
                        bodyAcc
                    else bodyAcc
              )
              body
              expectedFreeRefs
        )
        body0
        packetBinders

resolveConstructionSourceBindersInTypeWithExpected
  :: Maybe ElabType
  -> Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> Either String ElabType
resolveConstructionSourceBindersInTypeWithExpected mbExpected protectedIdentities representative sourceBinderRefs ty = do
  reopened <- reopen ty
  case mbExpected of
    Just expected
      | endpointEqual reopened expected -> pure expected
    _ -> do
      let resolved =
            resolveSourceBinderAliasesInTypeExcept
              protectedIdentities
              representative
              sourceBinderRefs
              (foldl' resolveConstructionFreeRef reopened (freeTypeVarRefsType reopened))
      case mbExpected of
        Nothing -> pure resolved
        Just expected
          | endpointEqual resolved expected -> pure expected
          | otherwise ->
              Left
                ( "construction packet does not equal its exact source endpoint: packet="
                    ++ show resolved
                    ++ ", expected="
                    ++ show expected
                )
  where
    endpointEqual left right =
      (alphaEqType left right || churchAwareEqType left right)
        && exactSourceBinderIdentitiesAgree left right

    -- Alpha equivalence deliberately ignores binder identities.  That is the
    -- right equality for validating types, but it cannot by itself authorize
    -- replacing a graph binder with an exact source binder.  After 'reopen',
    -- every such replacement must already have happened through
    -- 'sourceAliasForRef'; requiring the expected generated identities to be
    -- present here turns the sidecar route into a necessary construction
    -- witness.  Church-only representation seams may have different outer
    -- shapes, so unmatched constructors remain the responsibility of
    -- 'churchAwareEqType'; every structurally paired forall is still checked.
    exactSourceBinderIdentitiesAgree left right =
      case (left, right) of
        (TArrow leftDomain leftCodomain, TArrow rightDomain rightCodomain) ->
          exactSourceBinderIdentitiesAgree leftDomain rightDomain
            && exactSourceBinderIdentitiesAgree leftCodomain rightCodomain
        (TConWithIdentity _ _ leftArgs, TConWithIdentity _ _ rightArgs) ->
          exactSourceBinderArgumentIdentitiesAgree
            (toList leftArgs)
            (toList rightArgs)
        (TVarAppRef _ leftArgs, TVarAppRef _ rightArgs) ->
          exactSourceBinderArgumentIdentitiesAgree
            (toList leftArgs)
            (toList rightArgs)
        (TForallRef leftRef leftBound leftBody, TForallRef rightRef rightBound rightBody) ->
          expectedSourceBinderIdentityAgrees leftRef rightRef
            && exactSourceBinderBoundIdentitiesAgree leftBound rightBound
            && exactSourceBinderIdentitiesAgree leftBody rightBody
        (TMuRef _ leftBody, TMuRef _ rightBody) ->
          exactSourceBinderIdentitiesAgree leftBody rightBody
        _ -> True

    exactSourceBinderArgumentIdentitiesAgree [] [] = True
    exactSourceBinderArgumentIdentitiesAgree (left : leftRest) (right : rightRest) =
      exactSourceBinderIdentitiesAgree left right
        && exactSourceBinderArgumentIdentitiesAgree leftRest rightRest
    exactSourceBinderArgumentIdentitiesAgree _ _ = False

    exactSourceBinderBoundIdentitiesAgree Nothing Nothing = True
    exactSourceBinderBoundIdentitiesAgree (Just left) (Just right) =
      exactSourceBinderIdentitiesAgree (tyToElab left) (tyToElab right)
    exactSourceBinderBoundIdentitiesAgree _ _ = False

    expectedSourceBinderIdentityAgrees actualRef expectedRef =
      case
          typeBinderIdentityGeneratedUnique
            (typeBinderRefIdentity expectedRef)
        of
          Just _ -> typeBinderRefsSameIdentity actualRef expectedRef
          Nothing -> True

    -- Structural self/result identities are reconstruction metadata for their
    -- owning mu/forall.  They are not outward Gamma aliases: substituting a
    -- free graph reference with the owner's structural identity would collapse
    -- the complete recursive type to its internally bound self variable.
    resolveConstructionFreeRef current graphRef =
      case constructionAliasForRef graphRef of
        Just sourceRef
          | not (typeBinderRefsSameIdentity graphRef sourceRef) ->
              substTypeSimpleRef graphRef (TVarRef sourceRef) current
        _ -> current

    reopen = reopenAt True

    -- Generalization can represent a captured lexical source binder only in
    -- the packet's leading forall spine.  A forall below an arrow, type
    -- constructor, or recursive owner is part of that type's own structure;
    -- its source sidecar names the declaration and is not evidence that the
    -- declaration belongs to the enclosing construction Gamma.
    reopenNested = reopenAt False

    reopenAt leadingSpine current =
      case current of
        TVarRef ref -> pure (TVarRef ref)
        TArrow domain codomain ->
          TArrow <$> reopenNested domain <*> reopenNested codomain
        TConWithIdentity identity con args ->
          TConWithIdentity identity con <$> traverse reopenNested args
        TVarAppRef ref args ->
          TVarAppRef ref <$> traverse reopenNested args
        TBaseWithIdentity identity base ->
          pure (TBaseWithIdentity identity base)
        TBottom -> pure TBottom
        TForallRef ref mbBound body
          | not leadingSpine ->
              TForallRef ref
                <$> traverse reopenBound mbBound
                <*> reopenNested body
        TForallRef ref mbBound body ->
          case sourceAliasForRef ref of
            Just sourceRef ->
              case mbBound of
                Nothing ->
                  reopenAt True
                    ( if typeBinderRefsSameIdentity ref sourceRef
                        then body
                        else
                          substTypeCaptureRef
                            ref
                            (TVarRef sourceRef)
                            body
                    )
                Just bound
                  | not (typeBinderRefsSameIdentity ref sourceRef) ->
                      case mbExpected of
                        Nothing ->
                          TForallRef ref
                            <$> (Just <$> reopenBound bound)
                            <*> reopenAt True body
                        Just _ ->
                          TForallRef sourceRef
                            <$> (Just <$> reopenBound bound)
                            <*> reopenAt True
                              ( substTypeCaptureRef
                                  ref
                                  (TVarRef sourceRef)
                                  body
                              )
                Just bound ->
                  TForallRef ref
                    <$> (Just <$> reopenBound bound)
                    <*> reopenAt True body
            Nothing
              | Nothing <- mbBound
              , exactExpectedCaptures ref ->
                  reopenAt True body
            _ ->
              TForallRef ref
                <$> traverse reopenBound mbBound
                <*> reopenAt True body
        TMuRef ref body -> TMuRef ref <$> reopenNested body

    reopenBound bound = do
      reopened <- reopenNested (tyToElab bound)
      elabToBound reopened

    sourceAliasForRef ref = do
      if Set.member (typeBinderRefIdentity ref) protectedIdentities
        then Nothing
        else pure ()
      if exactExpectedDeclares ref
        then Nothing
        else pure ()
      case graphSourceAlias ref of
        Just sourceRef -> Just sourceRef
        Nothing -> exactIdentitySourceAlias ref

    constructionAliasForRef ref = do
      if Set.member (typeBinderRefIdentity ref) protectedIdentities
        then Nothing
        else pure ()
      case graphSourceAlias ref of
        Just sourceRef -> generatedSourceRef sourceRef
        Nothing -> exactIdentitySourceAlias ref

    graphSourceAlias ref = do
      graphConstructionAlias ref >>= generatedSourceRef

    graphConstructionAlias ref = do
      graphNode <- typeBinderRefNode ref
      case IntMap.lookup (getNodeId graphNode) sourceBinderRefs of
        Just directRef -> Just directRef
        Nothing ->
          IntMap.lookup
            (getNodeId (representative graphNode))
            sourceBinderRefs

    -- Repeated construction passes may already have replaced the graph ref
    -- with the generated source identity.  The sidecar still contains that
    -- exact identity as its value, which is sufficient to keep the binder
    -- captured rather than reopening a local forall.
    exactIdentitySourceAlias ref =
      listToMaybe
        [ sourceRef
        | sourceRef <- IntMap.elems sourceBinderRefs
        , typeBinderRefsSameIdentity sourceRef ref
        , isJust
            ( typeBinderIdentityGeneratedUnique
                (typeBinderRefIdentity sourceRef)
            )
        ]

    -- An exact endpoint is independent authority that an identity used free
    -- there is owned outside this operated packet.  Generated identities
    -- carry that fact directly; graph identities additionally require an
    -- explicit construction-sidecar route.  In either case the complete
    -- endpoint equality below proves that dropping the unbounded forall did
    -- not capture an unrelated binder.
    exactExpectedCaptures ref =
      case mbExpected of
        Just expected ->
          ( isJust
              ( typeBinderIdentityGeneratedUnique
                  (typeBinderRefIdentity ref)
              )
              || isJust (graphConstructionAlias ref)
          )
            && any
              (typeBinderRefsSameIdentity ref)
              (freeTypeVarRefsType expected)
        Nothing -> False

    -- An exact endpoint distinguishes a locally declared source binder from
    -- an inherited source binder carrying the same identity.  Only the latter
    -- is free in the expected type and may be reopened away; a binder declared
    -- by the exact endpoint is part of its ABI and must remain a forall.
    exactExpectedDeclares ref =
      case mbExpected of
        Nothing -> False
        Just expected ->
          any
            (typeBinderRefsSameIdentity ref)
            (typeBinderDeclarationRefs expected)

    generatedSourceRef ref =
      if isJust (typeBinderIdentityGeneratedUnique (typeBinderRefIdentity ref))
        then Just ref
        else Nothing

-- | Resolve graph-backed free references through the source-binder identity
-- sidecar.  The direct graph node is authoritative when present; only a node
-- without a direct entry may consult its solved-to-base representative.  We
-- rewrite generated source binders only: structural self/result identities
-- are owned by their mu/forall binders and must not be confused with a graph
-- exterior introduced by construction Gamma.
resolveSourceBinderAliasesInType
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> ElabType
resolveSourceBinderAliasesInType =
  resolveSourceBinderAliasesInTypeExcept Set.empty

resolveSourceBinderAliasesInTypeExcept
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> ElabType
resolveSourceBinderAliasesInTypeExcept protectedIdentities representative sourceBinderRefs ty0 =
  foldl' resolveRef ty0 (freeTypeVarRefsType ty0)
  where
    resolveRef ty graphRef =
      case sourceAlias graphRef of
        Just sourceRef
          | not (typeBinderRefsSameIdentity graphRef sourceRef) ->
              substTypeSimpleRef graphRef (TVarRef sourceRef) ty
        _ -> ty

    sourceAlias graphRef = do
      if Set.member (typeBinderRefIdentity graphRef) protectedIdentities
        then Nothing
        else pure ()
      graphNode <- typeBinderRefNode graphRef
      case IntMap.lookup (getNodeId graphNode) sourceBinderRefs of
        Just directRef -> generatedSourceRef directRef
        Nothing ->
          IntMap.lookup (getNodeId (representative graphNode)) sourceBinderRefs
            >>= generatedSourceRef

    generatedSourceRef ref =
      if isJust (typeBinderIdentityGeneratedUnique (typeBinderRefIdentity ref))
        then Just ref
        else Nothing

-- | Build the capture-aware term substitution for free graph references that
-- have an authoritative source-binder identity.  Callers supply free refs,
-- rather than every type occurrence, so a lexical graph binder remains owned
-- by its local 'forall'/'mu' construction.
sourceBinderAliasSubstitution
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> [TypeBinderRef]
  -> IntMap.IntMap TypeBinderRef
sourceBinderAliasSubstitution representative sourceBinderRefs refs =
  IntMap.fromList
    [ (getNodeId graphNode, sourceRef)
    | graphRef <- refs
    , Just graphNode <- [typeBinderRefNode graphRef]
    , Just sourceRef <- [sourceAlias graphNode]
    , not (typeBinderRefsSameIdentity graphRef sourceRef)
    ]
  where
    sourceAlias graphNode =
      case IntMap.lookup (getNodeId graphNode) sourceBinderRefs of
        Just directRef -> generatedSourceRef directRef
        Nothing ->
          IntMap.lookup
            (getNodeId (representative graphNode))
            sourceBinderRefs
            >>= generatedSourceRef

    generatedSourceRef ref =
      if isJust (typeBinderIdentityGeneratedUnique (typeBinderRefIdentity ref))
        then Just ref
        else Nothing
