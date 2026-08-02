module MLF.Elab.Run.Generalize.Types (
    NodeKey,
    NodeKeySet,
    NodeMap,
    GenMap,
    BindParents,
    ExpansionConstructionPlacements,
    ExpansionConstructionPlacementConflict(..),
    expansionConstructionPlacementsFromProjectedLists,
    expansionArgumentParentsToIntMap,
    expansionSemanticMetaParentsToIntMap,
    expansionConstructionRoleKeys,
    expansionConstructionParentsToIntMap,
    NodeMapping(..),
    GeneralizeEnv(..),
    Phase1Result(..),
    Phase2Result(..),
    Phase3Result(..),
    Phase4Result(..),
    InsertMode(..),
    LocalGammaConstruction(..),
    localGammaConstructionBinders,
    localGammaEmittedBinders,
    localGammaConsumedBinders,
    DirectApplicationGammaClaim(..),
    DirectApplicationAmbientGammaClaim(..),
    LocalGammaConstructionCertificate(..),
    localGammaConstructionCertificateResidualType
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import MLF.Constraint.Presolution.Plan.Requirements
    ( AmbientGammaAuthority
    , ExpansionConstructionPlacementConflict(..)
    , ExpansionConstructionPlacements
    , expansionConstructionPlacementsFromProjectedLists
    , expansionArgumentParentsToIntMap
    , expansionSemanticMetaParentsToIntMap
    , expansionConstructionRoleKeys
    , expansionConstructionParentsToIntMap
    )
import MLF.Constraint.Types.Graph
    ( BindFlag
    , Constraint
    , EdgeId
    , GenNode
    , GenNodeId
    , NodeId
    , NodeRef
    , TyNode
    )
import MLF.Elab.Generalize (LocalGammaOwner)
import MLF.Elab.Types
    ( BoundType
    , ElabType
    , Ty(..)
    , TypeBinderRef
    , tyToElab
    , typeBinderRefsSameIdentity
    )
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType)
import MLF.Util.Trace (TraceConfig)

type NodeKey = Int
type NodeKeySet = IntSet.IntSet
type NodeMap = IntMap.IntMap TyNode
type GenMap = IntMap.IntMap GenNode
type BindParents = IntMap.IntMap (NodeRef, BindFlag)

data NodeMapping = NodeMapping
    { mapBaseToSolved :: IntMap.IntMap NodeId
    , mapSolvedToBase :: IntMap.IntMap NodeId
    }

data GeneralizeEnv p = GeneralizeEnv { geBaseConstraint :: Constraint p
    , geSolvedConstraint :: Constraint p
    , geRedirects :: IntMap.IntMap NodeId
    , geInstCopyNodes :: NodeKeySet
    , geInstCopyMap :: IntMap.IntMap NodeId
    , geExpansionConstructionPlacements :: ExpansionConstructionPlacements
    , geCanonical :: NodeId -> NodeId
    , geApplyRedirectsToRef :: NodeRef -> NodeRef
    , geAdoptRef :: NodeRef -> NodeRef
    , geAdoptNodeId :: NodeId -> NodeId
    , geTraceConfig :: TraceConfig
    }

data Phase1Result = Phase1Result
    { p1NodesSolved :: NodeMap
    , p1SchemeRootsBase :: [NodeId]
    , p1SchemeRootsBaseSet :: IntSet.IntSet
    , p1SchemeRootsAllSet :: IntSet.IntSet
    -- | Alternative-let scheme roots paired with the distinct live targets
    -- proved by their typed identity redirects.
    , p1RestoredSchemeRootTargets :: IntMap.IntMap NodeId
    }

data Phase2Result = Phase2Result
    { p2GenMerged :: GenMap
    , p2NodeMapping :: NodeMapping
    , p2CopyOverrides :: IntMap.IntMap NodeId
    , p2BindParentsBase :: BindParents
    , p2BindParentsSolved :: BindParents
    , p2StickyTypeParentsBase :: IntSet.IntSet
    , p2BaseNamedKeys :: IntSet.IntSet
    }

data Phase3Result = Phase3Result
    { p3BindParentsBaseAdjusted :: BindParents
    , p3BindParentsWithCopies :: BindParents
    }

data Phase4Result = Phase4Result
    { p4BindParentsFinalAligned :: BindParents
    , p4GenMerged :: GenMap
    , p4SchemeRootsMerged :: [(GenNodeId, [NodeId])]
    , p4RootGenIdBase :: GenNodeId
    }

data InsertMode = KeepOld | Override | SelfOrEmpty

-- | Construction result for one checked application Gamma.
--
-- The producer partitions the prepared Gamma after both application-edge
-- computations have been emitted and the resulting term has typechecked:
--
-- * 'LocalGammaEmitted' records the non-empty prefix still free at the
--   application boundary, plus any prepared binders consumed internally.
-- * 'LocalGammaConsumed' records that the complete non-empty Gamma was
--   discharged by the application computations and no 'ETyAbsRef' survives.
-- * 'LocalGammaAmbient' records that a direct application requirement was
--   checked with no local binder routes.  Its exact zero-local proof lives in
--   'lgccDirectApplicationAmbientGammaClaims'.
--
-- 'LocalGammaAmbient' is not inferred from an empty binder list: it is
-- published only together with an exact direct-edge ambient claim.
data LocalGammaConstruction
    = LocalGammaEmitted
        !(NonEmpty (TypeBinderRef, Maybe BoundType))
        ![(TypeBinderRef, Maybe BoundType)]
    | LocalGammaConsumed
        !(NonEmpty (TypeBinderRef, Maybe BoundType))
    | LocalGammaAmbient
    deriving (Eq, Show)

localGammaConstructionBinders
    :: LocalGammaConstruction
    -> [(TypeBinderRef, Maybe BoundType)]
localGammaConstructionBinders construction =
    case construction of
        LocalGammaEmitted emitted consumed ->
            NonEmpty.toList emitted ++ consumed
        LocalGammaConsumed consumed ->
            NonEmpty.toList consumed
        LocalGammaAmbient -> []

localGammaEmittedBinders
    :: LocalGammaConstruction
    -> [(TypeBinderRef, Maybe BoundType)]
localGammaEmittedBinders construction =
    case construction of
        LocalGammaEmitted emitted _ -> NonEmpty.toList emitted
        LocalGammaConsumed _ -> []
        LocalGammaAmbient -> []

localGammaConsumedBinders
    :: LocalGammaConstruction
    -> [(TypeBinderRef, Maybe BoundType)]
localGammaConsumedBinders construction =
    case construction of
        LocalGammaEmitted _ consumed -> consumed
        LocalGammaConsumed consumed -> NonEmpty.toList consumed
        LocalGammaAmbient -> []

-- | Proof published by one checked application for one complete
-- edge-local Figure 15.3.5 Gamma requirement.  The edge/exterior/operated
-- triple identifies the requirement independently of a root-relative
-- placement.  Construction result roots retain the exact routes validated at
-- the application occurrence; a later root view may expose different result
-- roots, but every such route must still reach 'dagcBinderRef'.
data DirectApplicationGammaClaim = DirectApplicationGammaClaim
    { dagcEdgeIds :: !(NonEmpty EdgeId)
    , dagcExteriorNode :: !NodeId
    , dagcOperatedRoot :: !NodeId
    , dagcConstructionResultRoots :: !(NonEmpty NodeId)
    , dagcOperatedType :: !ElabType
    , dagcBinderRef :: !TypeBinderRef
    , dagcConstructedBound :: !(Maybe BoundType)
    }
    deriving (Eq, Show)

-- | Positive post-construction proof that one complete direct application
-- requirement was already ambient.  Every endpoint was absent from the
-- application's local construction routes, and the exact ambient declaration
-- and its bound supplied the operated endpoint when the application was
-- constructed, either directly or through an exact leading-binder-spine
-- construction.  When a certified descendant has completed a provisional
-- declaration, 'daagcOperatedType' is that checked endpoint; it must still
-- specialize to the graph requirement.  Keeping both occurrence and
-- declaration identity prevents a zero-local fact from being transferred to
-- another same-shaped source occurrence or a provisional declaration with a
-- different bound.
data DirectApplicationAmbientGammaClaim = DirectApplicationAmbientGammaClaim
    { daagcEdgeIds :: !(NonEmpty EdgeId)
    , daagcExteriorNode :: !NodeId
    , daagcOperatedRoot :: !NodeId
    , daagcConstructionResultRoots :: !(NonEmpty NodeId)
    , daagcOperatedType :: !ElabType
    , daagcAmbientRef :: !TypeBinderRef
    , daagcAmbientBound :: !ElabType
    }
    deriving (Eq, Show)

-- | Post-environment evidence produced by the exact application constructor.
-- A non-empty local construction covers its complete prepared Gamma,
-- including consumed binders.  An ambient construction carries no routes and
-- proves only its exact direct-edge claims.  Root preparation may use either
-- form to discharge the matching direct planning requirement, but an ambient
-- claim cannot displace an enclosing structural closure: ambientness is
-- relative to the application construction that published it.  Only
-- 'localGammaEmittedBinders' may be reconstructed in the root scheme.
data LocalGammaConstructionCertificate = LocalGammaConstructionCertificate
    { lgccOwner :: !LocalGammaOwner
    , lgccConstructedType :: !ElabType
    , lgccConstruction :: !LocalGammaConstruction
    -- | Exact direct function/argument edges of the source AApp frame.  This
    -- classifies which requirements must use a per-requirement claim; it is
    -- never sufficient by itself to discharge one.
    , lgccDirectApplicationSourceEdgeIds :: !(NonEmpty EdgeId)
    -- | Per-requirement proof for direct function/argument edges selected by
    -- the source application frame.  This is not ordinary Gamma_g scope
    -- ownership and therefore carries its exact occurrence evidence.
    , lgccDirectApplicationGammaClaims :: ![DirectApplicationGammaClaim]
    -- | Direct requirements proved ambient by the checked application.  This
    -- is explicit zero-local evidence; absence from 'lgccLocalBinderRoutes'
    -- alone never discharges a requirement.
    , lgccDirectApplicationAmbientGammaClaims
        :: ![DirectApplicationAmbientGammaClaim]
    -- | Exact declarations selected from the construction environment for
    -- the ambient claims above.  Declaration authority is distinct from
    -- free use: @alpha@ may be absent from the completed term while its bound
    -- @sigma@ still uses identities from 'lgccUsedAmbientBinderRefs'.
    , lgccAmbientDeclarationAuthorities :: ![AmbientGammaAuthority]
    -- | Graph-owned local binders, indexed by the exact graph occurrence
    -- discharged by this constructor.
    , lgccLocalBinderRoutes :: !(IntMap.IntMap TypeBinderRef)
    -- | Source-owned dependency binders, indexed by the exact source sidecar
    -- occurrence that authorized them.
    , lgccSourceBinderAuthorities :: !(IntMap.IntMap TypeBinderRef)
    -- | Exact ambient identities used freely by the checked application term,
    -- its checked result type, or an ambient declaration's bound.
    , lgccUsedAmbientBinderRefs :: ![TypeBinderRef]
    -- | Formerly ambient dependencies that an enclosing let RHS has closed
    -- with an exact leading 'ETyAbsRef'.  The let constructor records these
    -- declarations only after the finalized RHS has typechecked at its
    -- published scheme.  They remain available to validate this nested
    -- application certificate, but they are not ambient at the enclosing
    -- root and must never become root-generalization candidates.
    , lgccEnclosingTypeAbsBinders
        :: ![(TypeBinderRef, Maybe BoundType)]
    -- | Source-sidecar occurrences that prove a used ambient identity is
    -- introduced by a nested source annotation rather than by the enclosing
    -- root Gamma.  These routes are distinct from
    -- 'lgccSourceBinderAuthorities': they authorize an already-bound
    -- dependency and never authorize a local Gamma binder.
    , lgccUsedSourceBinderAuthorities :: !(IntMap.IntMap TypeBinderRef)
    }
    deriving (Eq, Show)

-- | Reveal the checked type left after a local application emitted its exact
-- Gamma prefix.  The residual is available only when the certificate's
-- constructed type starts with the same identity-bearing binders and bounds
-- recorded by 'LocalGammaEmitted'.  This keeps enclosing applications from
-- treating an arbitrary constructed body as evidence for a result route.
localGammaConstructionCertificateResidualType
    :: LocalGammaConstructionCertificate
    -> Maybe ElabType
localGammaConstructionCertificateResidualType certificate =
    stripEmittedPrefix
        (localGammaEmittedBinders (lgccConstruction certificate))
        (lgccConstructedType certificate)
  where
    stripEmittedPrefix [] ty = Just ty
    stripEmittedPrefix
        ((expectedRef, expectedBound) : rest)
        (TForallRef actualRef actualBound body)
            | typeBinderRefsSameIdentity expectedRef actualRef
            , boundsAgree expectedBound actualBound =
                stripEmittedPrefix rest body
    stripEmittedPrefix _ _ = Nothing

    boundsAgree left right =
        let leftTy = maybe TBottom tyToElab left
            rightTy = maybe TBottom tyToElab right
        in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy
