{-# LANGUAGE GADTs #-}

module MLF.Elab.Elaborate.Algebra.BodyConsumerRoute
  ( BodyConsumerRoute (..),
    selectBodyConsumerRoute,
    selectBodyConsumerRouteWithPacket,
    validateBodyConsumerRoute,
  )
where

import Control.Monad (unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import MLF.Constraint.Presolution.Plan.Requirements
  ( GeneralizationRequirements (..),
    RequiredGammaBinder (..),
    RequiredGammaPlacement (..),
  )
import MLF.Constraint.Types.Graph
  ( EdgeId,
    NodeId,
    getNodeId,
  )
import MLF.Elab.Generalize
  ( LocalGammaConstructor (..),
    LocalGammaOwner (..),
    PreparedSubtermGeneralization,
    scaConsumerIdentity,
    scaEdgeId,
    subtermGeneralizationConsumerAuthority,
    subtermGeneralizationOperatedSchemeInfo,
    subtermConsumerAuthorityEnclosingOwner,
  )
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.Types
  ( ElabError (..),
    ElabType,
    SchemeInfo (..),
    Ty (TVarRef),
  )
import MLF.Elab.SourceBinder (typeBinderDeclarationRefs)
import MLF.Reify.TypeOps (freeTypeVarRefsType, substTypeCaptureRef)
import MLF.Types.Elab
  ( TypeBinderRef,
    typeBinderIdentityFromNode,
    typeBinderRefFromIdentity,
    typeBinderRefIdentity,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
  )
import MLF.Types.Identity (typeBinderIdentityStableName)

-- | The exact semantic-to-construction route for the Gamma consumer of one
-- lambda body edge.  The semantic endpoint is always the requirement's
-- exterior graph identity; the construction endpoint must come from the
-- direct alias installed for that same exterior.  Keeping the edge, owner,
-- and exterior with both refs prevents later construction from treating a
-- quotient representative or a type-shaped peer as consumer authority.
--
-- The constructor stays in this owner-local module.  Production code obtains
-- routes only through 'selectBodyConsumerRoute'; test support may construct
-- deliberately invalid values to exercise 'validateBodyConsumerRoute'.
data BodyConsumerRoute = BodyConsumerRoute
  { bcrEdgeId :: !EdgeId
  , bcrOwner :: !LocalGammaOwner
  , bcrExteriorNode :: !NodeId
  , bcrSemanticRef :: !TypeBinderRef
  , bcrConstructionRef :: !TypeBinderRef
  -- | The operated endpoint @S'(n)@ selected by the edge requirement.  This
  -- is not necessarily the declaration bound stored in Gamma: an unbounded
  -- named node has declaration @a > bottom@ while its operated endpoint is
  -- still @a@.
  , bcrOperatedType :: !ElabType
  -- | The operated endpoint after applying only the exact graph routes
  -- published by this construction Gamma.
  , bcrConstructionOperatedType :: !ElabType
  }
  deriving (Eq, Show)

-- | Select the one local Figure 15.3.5 requirement owned by a lambda-body
-- edge.  Selection uses only the exact edge occurrence and the direct alias
-- keyed by its exterior node; quotient representatives, names, and type shape
-- are deliberately absent from this interface.
selectBodyConsumerRoute
  :: LocalGammaOwner
  -> EdgeId
  -> GeneralizationRequirements
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError (Maybe BodyConsumerRoute)
selectBodyConsumerRoute owner edgeId =
  selectBodyConsumerRouteWithPacket owner edgeId Nothing

-- | Packet-aware form used by lambda construction.  When the selected
-- packet's operated endpoint is one bare graph occurrence, its exact
-- enclosing-consumer authority supplies the missing occurrence-to-exterior
-- route.  This is deliberately narrower than extending the whole
-- construction alias map: a structured endpoint's free variables are
-- dependencies, not aliases for the consumer declaration.
selectBodyConsumerRouteWithPacket
  :: LocalGammaOwner
  -> EdgeId
  -> Maybe PreparedSubtermGeneralization
  -> GeneralizationRequirements
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError (Maybe BodyConsumerRoute)
selectBodyConsumerRouteWithPacket owner edgeId mbPacket requirements constructionAliases = do
  unless
    (lgoConstructor owner == LocalLambdaGamma)
    (routeFailure ["owner is not a lambda constructor"])
  unless
    (lgoBoundaryEdge owner == edgeId)
    (routeFailure ["owner boundary edge differs from the body edge"])
  case matchingRequirements of
    [] -> pure Nothing
    [requirement]
      | RequiredGammaAtNestedScope _ <- rgbPlacement requirement ->
          routeFailure
            [ "matching requirement is owned by a nested construction"
            , "  requirement: " ++ show requirement
            ]
      | RequiredGammaAtConstructionScope constructionScope <-
          rgbPlacement requirement
      , constructionScope /= lgoScope owner ->
          routeFailure
            [ "matching requirement names a different construction scope"
            , "  owner scope: " ++ show (lgoScope owner)
            , "  requirement: " ++ show requirement
            ]
      | otherwise ->
          case
              IntMap.lookup
                (getNodeId (rgbExteriorNode requirement))
                constructionAliases
            of
              Nothing ->
                routeFailure
                  [ "requirement exterior has no direct construction alias"
                  , "  exterior: " ++ show (rgbExteriorNode requirement)
                  , "  aliases: " ++ show constructionAliases
                  ]
              Just constructionRef ->
                let semanticIdentity =
                      typeBinderIdentityFromNode
                        (rgbExteriorNode requirement)
                    semanticRef =
                      typeBinderRefFromIdentity
                        semanticIdentity
                        (typeBinderIdentityStableName semanticIdentity)
                 in do
                      operatedAliases <-
                        either
                          ( \cause ->
                              routeFailure
                                [ "operated occurrence has no exact construction route"
                                , "  requirement: " ++ show requirement
                                , "  cause: " ++ show cause
                                ]
                          )
                          Right
                          ( constructionAliasesForOperatedPacket
                              owner
                              edgeId
                              (rgbExteriorNode requirement)
                              constructionRef
                              mbPacket
                              (rgbExactOperatedOccurrenceRef requirement)
                              (rgbOperatedType requirement)
                              constructionAliases
                          )
                      pure
                        ( Just
                            BodyConsumerRoute
                              { bcrEdgeId = edgeId
                              , bcrOwner = owner
                              , bcrExteriorNode = rgbExteriorNode requirement
                              , bcrSemanticRef = semanticRef
                              , bcrConstructionRef = constructionRef
                              , bcrOperatedType = rgbOperatedType requirement
                              , bcrConstructionOperatedType =
                                  alignOperatedType
                                    constructionRef
                                    operatedAliases
                                    (rgbOperatedType requirement)
                              }
                        )
    matches ->
      routeFailure
        [ "body edge has multiple local Gamma requirements"
        , "  requirements: " ++ show matches
        ]
  where
    matchingRequirements =
      [ requirement
      | requirement <- grRequiredGammaBinders requirements
      , edgeId `elem` NonEmpty.toList (rgbEdgeIds requirement)
      ]

    routeFailure :: [String] -> Either ElabError a
    routeFailure details =
      Left
        ( ValidationFailed
            ( [ "cannot construct exact lambda-body Gamma consumer route"
              , "  owner: " ++ show owner
              , "  edge: " ++ show edgeId
              ]
                ++ details
            )
        )

-- | Revalidate a selected route at its use site against the packet and the
-- current construction environment.  This catches stale aliases and packets
-- from a sibling edge/owner without reopening route selection heuristics.
validateBodyConsumerRoute
  :: LocalGammaOwner
  -> EdgeId
  -> PreparedSubtermGeneralization
  -> IntMap.IntMap TypeBinderRef
  -> BodyConsumerRoute
  -> Either ElabError ()
validateBodyConsumerRoute owner edgeId packet constructionAliases route = do
  authority <-
    case subtermGeneralizationConsumerAuthority packet of
      Just present -> pure present
      Nothing ->
        routeFailure
          "packet has no body-consumer authority"
          []
  unless
    (lgoConstructor owner == LocalLambdaGamma)
    (routeFailure "expected owner is not a lambda constructor" [])
  unless
    (lgoBoundaryEdge owner == edgeId)
    (routeFailure "expected owner boundary edge differs from the body edge" [])
  unless
    (bcrEdgeId route == edgeId)
    (routeFailure "route belongs to a different edge" [])
  unless
    (bcrOwner route == owner)
    (routeFailure "route belongs to a different lambda owner" [])
  unless
    (scaEdgeId authority == edgeId)
    ( routeFailure
        "packet authority belongs to a different edge"
        ["  packet authority: " ++ show authority]
    )
  unless
    (subtermConsumerAuthorityEnclosingOwner authority == Just owner)
    ( routeFailure
        "packet authority belongs to a different lambda owner"
        ["  packet authority: " ++ show authority]
    )
  unless
    ( scaConsumerIdentity authority
        == typeBinderRefIdentity (bcrSemanticRef route)
    )
    ( routeFailure
        "packet authority does not name the requirement exterior"
        ["  packet authority: " ++ show authority]
    )
  let exactExteriorIdentity =
        typeBinderIdentityFromNode
          (bcrExteriorNode route)
  unless
    ( typeBinderRefIdentity (bcrSemanticRef route)
        == exactExteriorIdentity
    )
    (routeFailure "route semantic ref is not the exact requirement exterior" [])
  case
      IntMap.lookup
        (getNodeId (bcrExteriorNode route))
        constructionAliases
    of
      Just routedRef
        | typeBinderRefsSameIdentity
            routedRef
            (bcrConstructionRef route) ->
            pure ()
      routed ->
        routeFailure
          "environment does not contain the route's direct exterior alias"
          ["  environment route: " ++ show routed]
  operatedAliases <-
    constructionAliasesForOperatedPacket
      owner
      edgeId
      (bcrExteriorNode route)
      (bcrConstructionRef route)
      (Just packet)
      Nothing
      (bcrOperatedType route)
      constructionAliases
  unless
    ( bcrConstructionOperatedType route
        == alignOperatedType
          (bcrConstructionRef route)
          operatedAliases
          (bcrOperatedType route)
    )
    ( routeFailure
        "route carries a stale packet-operated endpoint"
        []
    )
  where
    routeFailure :: String -> [String] -> Either ElabError a
    routeFailure detail context =
      Left
        ( ValidationFailed
            ( [ "invalid lambda-body Gamma consumer route"
              , "  detail: " ++ detail
              , "  owner: " ++ show owner
              , "  edge: " ++ show edgeId
              , "  route: " ++ show route
              ]
                ++ context
            )
        )

constructionAliasesForOperatedPacket
  :: LocalGammaOwner
  -> EdgeId
  -> NodeId
  -> TypeBinderRef
  -> Maybe PreparedSubtermGeneralization
  -> Maybe TypeBinderRef
  -> ElabType
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError (IntMap.IntMap TypeBinderRef)
constructionAliasesForOperatedPacket owner edgeId exterior constructionRef mbPacket mbExactOperatedOccurrence operatedType aliases =
  case mbPacket of
    Nothing ->
      case operatedType of
        TVarRef operatedRef
          | Just exactRef <- mbExactOperatedOccurrence
          , typeBinderRefsSameIdentity exactRef operatedRef ->
              -- The requirement itself carries the occurrence-level source
              -- authority. The exact source endpoint remains unchanged; the
              -- exterior alias separately identifies the Gamma declaration.
              pure aliases
          | not (operatedRefAlreadyRouted operatedRef) ->
              packetFailure
                ( "bare operated occurrence has no exact packet: "
                    ++ show operatedRef
                )
        _ -> pure aliases
    Just packet -> do
      authority <-
        case subtermGeneralizationConsumerAuthority packet of
          Just present -> pure present
          Nothing -> packetFailure "packet has no consumer authority"
      unless
        (scaEdgeId authority == edgeId)
        (packetFailure "packet belongs to a different edge")
      unless
        (subtermConsumerAuthorityEnclosingOwner authority == Just owner)
        (packetFailure "packet belongs to a different lambda owner")
      unless
        ( scaConsumerIdentity authority
            == typeBinderIdentityFromNode exterior
        )
        (packetFailure "packet names a different consumer exterior")
      case
          ( operatedType
          , schemeToType
              (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
          )
        of
          (TVarRef operatedRef, TVarRef packetRef)
            | typeBinderRefsSameIdentity operatedRef packetRef ->
                case typeBinderRefNode operatedRef of
                  Nothing -> pure aliases
                  Just operatedNode ->
                    case IntMap.lookup (getNodeId operatedNode) aliases of
                      Nothing ->
                        pure
                          ( IntMap.insert
                              (getNodeId operatedNode)
                              constructionRef
                              aliases
                          )
                      Just existingRef
                        | typeBinderRefsSameIdentity
                            existingRef
                            constructionRef ->
                            pure aliases
                      Just existingRef ->
                        packetFailure
                          ( "packet-operated occurrence has a conflicting construction route: "
                              ++ show existingRef
                          )
          (_, packetType)
            | TVarRef operatedRef <- operatedType
            , not (operatedRefAlreadyRouted operatedRef) ->
                packetFailure
                  ( "packet operated endpoint does not certify the bare requirement occurrence"
                      ++ "; requirement="
                      ++ show operatedType
                      ++ "; packet="
                      ++ show packetType
                  )
          _ -> pure aliases
  where
    operatedRefAlreadyRouted operatedRef =
      typeBinderRefsSameIdentity operatedRef constructionRef
        || case typeBinderRefNode operatedRef of
          Just node ->
            maybe
              False
              (typeBinderRefsSameIdentity constructionRef)
              (IntMap.lookup (getNodeId node) aliases)
          Nothing -> False

    packetFailure :: String -> Either ElabError a
    packetFailure detail =
      Left
        ( ValidationFailed
            [ "cannot construct packet-operated lambda-body consumer route"
            , "  detail: " ++ detail
            , "  owner: " ++ show owner
            , "  edge: " ++ show edgeId
            , "  exterior: " ++ show exterior
            ]
        )

alignOperatedType
  :: TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> ElabType
alignOperatedType consumerRef aliases ty0 =
  foldl' alignFreeRef ty0 (freeTypeVarRefsType ty0)
  where
    alignFreeRef ty ref =
      case
          typeBinderRefNode ref
            >>= \node -> IntMap.lookup (getNodeId node) aliases
        of
        -- A bare operated occurrence is the packet-selected consumer
        -- capability itself.  Inside a structured endpoint, however, a route
        -- to that same consumer would turn an ordinary dependency such as
        -- @Box a -> Bool@ into the illegal self-bound @c > Box c -> Bool@.
        -- Other independently constructed aliases remain valid.
        Just constructionRef
          | not (bareOperatedOccurrence ty0)
          , typeBinderRefsSameIdentity constructionRef consumerRef ->
              ty
        Just constructionRef
          | any
              (typeBinderRefsSameIdentity constructionRef)
              lexicalDeclarationRefs ->
              ty
        Just constructionRef ->
          substTypeCaptureRef ref (TVarRef constructionRef) ty
        Nothing -> ty

    lexicalDeclarationRefs = typeBinderDeclarationRefs ty0

    bareOperatedOccurrence ty =
      case ty of
        TVarRef _ -> True
        _ -> False
