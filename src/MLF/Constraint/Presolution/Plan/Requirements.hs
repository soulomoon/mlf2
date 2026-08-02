-- | Construction obligations that must survive from witness authority into
-- generalization planning.  This module is deliberately leaf-level so binder
-- planning can consume the obligations without depending on planning context.
module MLF.Constraint.Presolution.Plan.Requirements
  ( ExpansionConstructionPlacements,
    ExpansionConstructionPlacementConflict (..),
    expansionConstructionPlacementsFromProjectedLists,
    expansionArgumentParentsToIntMap,
    expansionSemanticMetaParentsToIntMap,
    expansionConstructionRoleKeys,
    expansionConstructionParentsToIntMap,
    emptyExpansionConstructionPlacements,
    AmbientGammaAuthority (..),
    GeneralizationRequirements (..),
    RequiredGammaBinder (..),
    RequiredGammaPlacement (..),
    placeCurrentGammaRequirementsAt,
    requiredGammaPlacementIsLocal,
    emptyGeneralizationRequirements,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty)
import MLF.Constraint.Types.Graph
  ( BindFlag,
    EdgeId,
    NodeId,
    NodeRef (..),
    getNodeId,
  )
import MLF.Types.Elab (ElabType, TypeBinderRef)

-- | The post-quotient binding placements emitted atomically while instance
-- expansions are constructed.
--
-- The exact parent chain is the certificate.  Its enclosing gen owner is not
-- stored separately: quotienting and later construction placements can change
-- the path to that owner, so consumers derive it from their authoritative
-- binding tree.  This makes an owner mismatch unrepresentable instead of
-- repairing a stale cached scope after construction.
--
-- The constructor stays private so one canonical node cannot silently acquire
-- two incompatible construction placements.
data ExpansionConstructionPlacements = ExpansionConstructionPlacements
  { ecpParents :: !(IntMap.IntMap (NodeRef, BindFlag)),
    ecpArgumentKeys :: !IntSet.IntSet,
    ecpSemanticMetaKeys :: !IntSet.IntSet
  }
  deriving (Eq, Show)

data ExpansionConstructionPlacementConflict = ExpansionConstructionPlacementConflict
  { ecpcNode :: !NodeId,
    ecpcFirstParent :: !NodeRef,
    ecpcFirstFlag :: !BindFlag,
    ecpcSecondParent :: !NodeRef,
    ecpcSecondFlag :: !BindFlag
  }
  deriving (Eq, Show)

-- | Build the post-quotient elaboration certificate from exact construction
-- placements.
-- An argument parent is not assumed to remain its original destination gen:
-- two fresh
-- arguments can later be unified, in which case their elaboration class uses
-- the quotient LCA of the original construction parents.  This certificate is
-- not the solver's complete live Rebind tree.
expansionConstructionPlacementsFromProjectedLists
  :: [(NodeId, NodeRef, BindFlag)]
  -> [(NodeId, NodeRef, BindFlag)]
  -> [(NodeId, NodeRef, BindFlag)]
  -> Either ExpansionConstructionPlacementConflict ExpansionConstructionPlacements
expansionConstructionPlacementsFromProjectedLists argumentClaims semanticMetaClaims supportClaims = do
  parentsAfterArguments <-
    go IntMap.empty argumentClaims
  parentsAfterMetas <- go parentsAfterArguments semanticMetaClaims
  parents <- go parentsAfterMetas supportClaims
  pure
    ExpansionConstructionPlacements
      { ecpParents = parents,
        ecpArgumentKeys =
          IntSet.fromList
            [getNodeId node | (node, _parent, _flag) <- argumentClaims],
        ecpSemanticMetaKeys =
          IntSet.fromList [getNodeId node | (node, _parent, _flag) <- semanticMetaClaims]
      }
  where
    go parents [] = Right parents
    go parents ((node, parent, flag) : rest) =
      let key = getNodeId node
       in case IntMap.lookup key parents of
            Nothing -> go (IntMap.insert key (parent, flag) parents) rest
            Just (parent0, flag0)
              | parent0 == parent && flag0 == flag -> go parents rest
              | otherwise ->
                  Left
                    ExpansionConstructionPlacementConflict
                      { ecpcNode = node,
                        ecpcFirstParent = parent0,
                        ecpcFirstFlag = flag0,
                        ecpcSecondParent = parent,
                        ecpcSecondFlag = flag
                      }

expansionArgumentParentsToIntMap
  :: ExpansionConstructionPlacements
  -> IntMap.IntMap (NodeRef, BindFlag)
expansionArgumentParentsToIntMap placements =
  IntMap.restrictKeys (ecpParents placements) (ecpArgumentKeys placements)

expansionSemanticMetaParentsToIntMap
  :: ExpansionConstructionPlacements
  -> IntMap.IntMap (NodeRef, BindFlag)
expansionSemanticMetaParentsToIntMap placements =
  IntMap.restrictKeys
    (ecpParents placements)
    (ecpSemanticMetaKeys placements)

expansionConstructionRoleKeys
  :: ExpansionConstructionPlacements
  -> IntSet.IntSet
expansionConstructionRoleKeys placements =
  IntSet.union (ecpArgumentKeys placements) (ecpSemanticMetaKeys placements)

expansionConstructionParentsToIntMap
  :: ExpansionConstructionPlacements
  -> IntMap.IntMap (NodeRef, BindFlag)
expansionConstructionParentsToIntMap = ecpParents

emptyExpansionConstructionPlacements :: ExpansionConstructionPlacements
emptyExpansionConstructionPlacements =
  ExpansionConstructionPlacements
    { ecpParents = IntMap.empty,
      ecpArgumentKeys = IntSet.empty,
      ecpSemanticMetaKeys = IntSet.empty
    }

-- | Paper-owned Γ entry required by a root @RaiseMerge(r, exterior)@.
-- The exterior node supplies the binder identity and the operated source root
-- identifies the paper's source term.  A bottom-up subterm packet, when one is
-- owned by this exterior, supplies the exact @S'(operated)@ bound; otherwise
-- the frozen operated source root supplies it for a leaf body.  The explicit root
-- RaiseMerge authority proves that either construction belongs to the exterior
-- required by Lemmas 15.3.10--11.  Multiple edge-local results may share that
-- one exterior; the non-empty result set makes every substitution route part
-- of the same construction obligation.
-- | Where the term translation must emit a required Gamma binder.  Most
-- requirements belong to the construction currently being planned.  Root
-- result planning can generalize a copied result at a different scope from
-- the source constructor that emits its Gamma; that exact source scope must
-- travel with the requirement instead of being reconstructed from the target.
-- A requirement may also be emitted by a proven nested Figure 15.3.5
-- constructor.  Keeping all three placements explicit prevents an enclosing
-- planner from silently reassigning or deleting the binder.
data RequiredGammaPlacement
  = RequiredGammaAtCurrentScope
  | RequiredGammaAtConstructionScope !NodeRef
  | RequiredGammaAtNestedScope !NodeRef
  deriving (Eq, Show)

data RequiredGammaBinder = RequiredGammaBinder
  { rgbEdgeIds :: !(NonEmpty EdgeId),
    rgbExteriorNode :: !NodeId,
    rgbOperatedRoot :: !NodeId,
    rgbResultRoots :: !(NonEmpty NodeId),
    rgbOperatedType :: !ElabType,
    -- | When the checked construction endpoint is one bare graph
    -- occurrence, retain that exact identity separately from the frozen
    -- operated root.  Construction may route this occurrence to the
    -- exterior binder; it must not rediscover the relation through a
    -- representative or through type shape.
    rgbExactOperatedOccurrenceRef :: !(Maybe TypeBinderRef),
    rgbPlacement :: !RequiredGammaPlacement
  }
  deriving (Eq, Show)

-- | One exact declaration already available in the ambient paper Gamma.
-- The containing map is keyed by the live graph occurrence that routes to
-- this declaration.  Producers must prove that route by a direct node-key
-- construction alias and obtain the bound from an exact-identity type
-- binding; consumers must not recreate either fact through a representative
-- or display name.
data AmbientGammaAuthority = AmbientGammaAuthority
  { agaExactRef :: !TypeBinderRef,
    agaBound :: !ElabType
  }
  deriving (Eq, Show)

data GeneralizationRequirements = GeneralizationRequirements
  { grRequiredGammaBinders :: ![RequiredGammaBinder],
    grSourceBinderRefs :: !(IntMap.IntMap TypeBinderRef),
    -- | Type binders already introduced by the enclosing source boundary.
    -- They may remain free while constructing a nested packet, but they are
    -- never candidates for this generalization's forall spine.
    grAmbientBinderRefs :: ![TypeBinderRef],
    -- | Exact graph binders that the checked owner construction proves belong
    -- to its final ETyAbs spine, including owner-emitted declarations absent
    -- from the erased result type.  Binder planning admits these identities
    -- before dependency ordering; finalization may preserve only the
    -- declarations selected there and never synthesizes a binder after
    -- reification.
    grTermUsedRootBinderRefs :: ![TypeBinderRef],
    -- | Direct live-node routes to exact declarations and bounds already in
    -- the construction Gamma.  Unlike source-binder refs, these may retain
    -- graph identities; the exact bound is the semantic authority.
    grAmbientGammaAuthorities :: !(IntMap.IntMap AmbientGammaAuthority),
    -- Exterior nodes whose Gamma is constructed at a nested lexical
    -- boundary.  Enclosing binder planning must leave these nodes to that
    -- constructor instead of quantifying an unbounded duplicate at the root.
    grLocallyClosedGammaNodes :: !IntSet.IntSet
  }
  deriving (Eq, Show)

-- | Stamp requirements generated for the current constructor with its exact
-- source scope.  This is needed when the type root is a copied or unwrapped
-- node whose own generalization scope differs from the term occurrence.  A
-- nested placement already carries another constructor's certificate and is
-- therefore left untouched.
placeCurrentGammaRequirementsAt
  :: NodeRef
  -> GeneralizationRequirements
  -> GeneralizationRequirements
placeCurrentGammaRequirementsAt constructionScope requirements =
  requirements
    { grRequiredGammaBinders =
        map place (grRequiredGammaBinders requirements)
    }
  where
    place requirement =
      requirement
        { rgbPlacement =
            case rgbPlacement requirement of
              RequiredGammaAtCurrentScope ->
                RequiredGammaAtConstructionScope constructionScope
              placement -> placement
        }

-- | Whether this requirement is emitted by the constructor being planned.
-- An exact construction scope changes only ownership validation; routing and
-- emission remain the same as an ordinary current-scope requirement.
requiredGammaPlacementIsLocal :: RequiredGammaPlacement -> Bool
requiredGammaPlacementIsLocal placement =
  case placement of
    RequiredGammaAtCurrentScope -> True
    RequiredGammaAtConstructionScope _ -> True
    RequiredGammaAtNestedScope _ -> False

emptyGeneralizationRequirements :: GeneralizationRequirements
emptyGeneralizationRequirements =
  GeneralizationRequirements
    { grRequiredGammaBinders = [],
      grSourceBinderRefs = IntMap.empty,
      grAmbientBinderRefs = [],
      grTermUsedRootBinderRefs = [],
      grAmbientGammaAuthorities = IntMap.empty,
      grLocallyClosedGammaNodes = IntSet.empty
    }
