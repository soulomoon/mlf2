module MLF.Elab.Run.Scope
  ( bindingScopeRef,
    bindingScopeRefCanonical,
    generalizeTargetNode,
    schemeBodyTarget,
    canonicalizeScopeRef,
    resolveCanonicalScope,
    ConstructionScopes,
    ApplicationConstructionScopes (..),
    ConstructionScopeSelection (..),
    constructionScopes,
    constructionNodeScopeSelection,
    constructionBoundaryScopeSelection,
    resolveConstructionScopeForNode,
    resolveConstructionScopeForBoundary,
    resolveApplicationConstructionScopes,
    applicationGeneralizationScopeForRequirements,
  )
where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution (PresolutionView (..))
import MLF.Constraint.Presolution.Plan.Context
  ( GaBindParents (..),
    SolvedToBaseResolution (..),
    resolveGaSolvedToBase,
  )
import MLF.Constraint.Presolution.Plan.Requirements
  ( GeneralizationRequirements (..),
    RequiredGammaBinder (..),
    RequiredGammaPlacement (..),
  )
import MLF.Constraint.Types.Graph
  ( BindingError (..), Constraint, EdgeId (..),
    NodeId (..),
    NodeRef (..),
    TyNode (..),
    cNodes,
    getNodeId,
    gnSchemes,
    lookupNodeIn,
    typeRef,
  )
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Frontend.ConstraintGen (AnnExpr (..))
import MLF.Frontend.ConstraintGen.Types
  ( AnnExprF (..),
    instantiationSiteEdgeId,
  )
import MLF.Util.ElabError (ElabError (..))

{- Note [ga′ scope selection — Def. 15.3.2 alignment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis (Def. 15.3.2) defines ga′ as the nearest gen ancestor of the
binding root in the original constraint χ_p.  The live pipeline in
`resolveCanonicalScope` implements this as:

  1. `bindingScopeRef`: Computes `bindingPathToRoot`, takes the first GenRef
     after `drop 1` (skipping the node itself).  This is the nearest gen
     ancestor — thesis-aligned.  The `TypeRef root` fallback handles binding
     roots that have no gen ancestor (top-level nodes).

  2. `canonicalizeScopeRef`: GenRef passes through unchanged (gen nodes are
     stable identifiers not subject to redirect/UF).  TypeRef gets
     redirect-chased then UF-canonicalized.

  3. `constructionScopes`: Computes scope on the base constraint c1, compares
     with the solved constraint's scope.  Prefers the base scope when they
     diverge.  This is correct: the thesis defines ga′ on the original χ_p,
     not the solved version.

Conclusion: the pipeline is thesis-aligned and propagates binding-tree errors
consistently through original-constraint scope selection and canonical scope
resolution without a redundant second binding-tree lookup.
-}
bindingScopeRef :: Constraint p -> NodeId -> Either BindingError NodeRef
bindingScopeRef constraint root = do
  path <- Binding.bindingPathToRoot constraint (typeRef root)
  case listToMaybe [gid | GenRef gid <- drop 1 path] of
    Just gid -> Right (GenRef gid)
    Nothing -> Right (TypeRef root)

-- | Canonical-domain variant of 'bindingScopeRef' that traverses canonical bind-parents from a presolution view.
bindingScopeRefCanonical :: PresolutionView p -> NodeId -> Either BindingError NodeRef
bindingScopeRefCanonical presolutionView root =
  bindingScopeRef (pvCanonicalConstraint presolutionView) root

{- Note [S vs S' target selection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Section 15.3.1/15.3.2 distinguishes two translations of a type subpart:

  * in general the thesis uses `S'`, so named nodes stay named when computing
    subterm types;
  * the thesis also says plain `S` is still needed to compute the bounds of
    named nodes themselves.

We reflect that distinction with two owner-local selectors here. This mapping is
an implementation choice guided by the thesis text plus the checked regressions
in this codebase; it is not meant as a verbatim restatement that the thesis
names these helper boundaries explicitly.

  * `schemeBodyTarget` keeps non-scheme-root named aliases at the named node,
    matching the `S'`-style subterm translation used for reification/target
    types.

  * `generalizeTargetNode` descends through a named alias to its bound/body on
    the current path that computes the named node's own scheme/bound, matching
    the role the thesis reserves for plain `S`.
-}
data TargetUnwrapInfo = TargetUnwrapInfo
  { tuiTargetCanonical :: NodeId,
    tuiTargetNode :: Maybe TyNode,
    tuiBoundCanonical :: Maybe NodeId,
    tuiBoundNode :: Maybe TyNode
  }

targetUnwrapInfo :: PresolutionView p -> NodeId -> TargetUnwrapInfo
targetUnwrapInfo presolutionView target =
  let canonical = pvCanonical presolutionView
      targetC = canonical target
      targetNode = pvLookupNode presolutionView targetC
      boundCanonical = case targetNode of
        Just TyVar {tnBound = Just bnd} -> Just (canonical bnd)
        _ -> Nothing
      boundNode = boundCanonical >>= pvLookupNode presolutionView
   in TargetUnwrapInfo
        { tuiTargetCanonical = targetC,
          tuiTargetNode = targetNode,
          tuiBoundCanonical = boundCanonical,
          tuiBoundNode = boundNode
        }

generalizeTargetNode :: PresolutionView p -> NodeId -> NodeId
generalizeTargetNode presolutionView target =
  let canonical = pvCanonical presolutionView
      info = targetUnwrapInfo presolutionView target
   in case tuiTargetNode info of
        Just TyVar {tnBound = Just _} ->
          case (tuiBoundCanonical info, tuiBoundNode info) of
            (Just _, Just TyForall {tnBody = body}) -> canonical body
            (Just bndC, _) -> bndC
            _ -> tuiTargetCanonical info
        Just TyForall {tnBody = body} -> canonical body
        _ -> tuiTargetCanonical info

schemeBodyTarget :: PresolutionView p -> NodeId -> NodeId
schemeBodyTarget presolutionView target =
  let constraint = pvConstraint presolutionView
      canonical = pvCanonical presolutionView
      info = targetUnwrapInfo presolutionView target
      targetC = tuiTargetCanonical info
      isSchemeRoot =
        any
          (\gen -> any (\root -> canonical root == targetC) (gnSchemes gen))
          (NodeAccess.allGenNodes constraint)
      schemeRootByBody =
        IntMap.fromListWith
          (\a _ -> a)
          [ (getNodeId (canonical bnd), root)
            | gen <- NodeAccess.allGenNodes constraint,
              root <- gnSchemes gen,
              Just bnd <- [pvLookupVarBound presolutionView root],
              case pvLookupNode presolutionView (canonical bnd) of
                Just TyBase {} -> False
                Just TyBottom {} -> False
                _ -> True
          ]
   in case tuiTargetNode info of
        Just TyVar {tnBound = Just _} ->
          let boundIsSchemeBody =
                maybe False (\bndC -> IntMap.member (getNodeId bndC) schemeRootByBody) (tuiBoundCanonical info)
           in if isSchemeRoot || boundIsSchemeBody
                then case (tuiBoundCanonical info, tuiBoundNode info) of
                  (Just _, Just TyForall {tnBody = body}) -> canonical body
                  (Just bndC, _) -> bndC
                  _ -> targetC
                else targetC
        Just TyForall {tnBody = body} -> canonical body
        _ -> targetC

{- Note [ga′ preservation across redirects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Redirect chasing and UF canonicalization preserve ga′ through the pipeline:

  1. GenRef passthrough: `canonicalizeScopeRef` passes GenRef through
     unchanged (line above).  Gen nodes are stable identifiers not subject
     to redirect or union-find — they cannot be merged or redirected.

  2. TypeRef edge case: When `bindingScopeRef` returns TypeRef (no gen
     ancestor), `canonicalizeScopeRef` applies redirect+UF.  If the redirect
     changes the node to one that *does* have a gen ancestor, the scope
     remains a TypeRef (not upgraded to GenRef).  However, `constructionScopes`
     detects this divergence between base and solved scopes and records the
     base scope, so the thesis ga′ is preserved.

  3. Redirect chain stability: `chaseRedirectsStable`
     (MLF.Constraint.Canonicalizer) uses `stableChase` with cycle detection
     via IntSet.  Cycles resolve to the smallest NodeId, ensuring
     deterministic and stable canonicalization.

  4. Annotation rewriting: `applyRedirectsToAnn` (MLF.Elab.Run.Annotation)
     maps `chaseRedirects` over all node IDs in the annotated expression.
     Existing tests in PipelineSpec verify no stale (pre-redirect) nodes
     remain after annotation rewriting.
-}
canonicalizeScopeRef :: PresolutionView p -> IntMap.IntMap NodeId -> NodeRef -> NodeRef
canonicalizeScopeRef presolutionView redirects scopeRef =
  case scopeRef of
    GenRef gid -> GenRef gid
    TypeRef nid ->
      let canonical = pvCanonical presolutionView
       in TypeRef (canonical (chaseRedirects redirects nid))

resolveCanonicalScope :: Constraint p -> PresolutionView p -> IntMap.IntMap NodeId -> NodeId -> Either BindingError NodeRef
resolveCanonicalScope constraint presolutionView redirects scopeRoot = do
  scope0 <- bindingScopeRef constraint scopeRoot
  pure (canonicalizeScopeRef presolutionView redirects scope0)

-- | Occurrence-indexed construction scopes.  Canonical type nodes are not
-- source-constructor identities: union-find can merge sibling applications and
-- nested let results while their local Gammas remain distinct.  Stable edge IDs
-- retain that occurrence identity, so local constructors consult the boundary
-- map and the node map remains only a generic root/scheme fallback.
data ConstructionScopes = ConstructionScopes
  { constructionNodeScopeCandidates :: !(IntMap.IntMap (Set.Set NodeRef)),
    constructionBoundaryScopeCandidates :: !(IntMap.IntMap (Set.Set NodeRef)),
    constructionApplicationTargetScopeCandidates
      :: !(IntMap.IntMap (IntMap.IntMap (Set.Set NodeRef)))
  }
  deriving (Eq, Show)

instance Semigroup ConstructionScopes where
  left <> right =
    ConstructionScopes
      { constructionNodeScopeCandidates =
          IntMap.unionWith Set.union
            (constructionNodeScopeCandidates left)
            (constructionNodeScopeCandidates right),
        constructionBoundaryScopeCandidates =
          IntMap.unionWith Set.union
            (constructionBoundaryScopeCandidates left)
            (constructionBoundaryScopeCandidates right),
        constructionApplicationTargetScopeCandidates =
          IntMap.unionWith
            (IntMap.unionWith Set.union)
            (constructionApplicationTargetScopeCandidates left)
            (constructionApplicationTargetScopeCandidates right)
      }

instance Monoid ConstructionScopes where
  mempty = ConstructionScopes IntMap.empty IntMap.empty IntMap.empty

-- | The source occurrence and the type selected for its local scheme carry
-- distinct scope evidence.  In particular, 'generalizeTargetNode' may unwrap
-- an application result to a bound whose original ga' differs from the
-- occurrence boundary.  The occurrence scope owns the local Gamma, while the
-- target scope owns @Gen(Gamma, S(result))@.  An exact requirement placement
-- carries the former into planning at the latter, so neither authority is
-- reconstructed from the other.
data ApplicationConstructionScopes = ApplicationConstructionScopes
  { applicationOccurrenceScope :: !NodeRef,
    applicationTargetGeneralizationScope :: !NodeRef
  }
  deriving (Eq, Show)

-- | Select the scope that owns one application's @Gen(Gamma, S(result))@.
-- A raw current-scope Gamma obligation has not yet been stamped with its
-- source occurrence and therefore must stay at that occurrence.  Once
-- 'placeCurrentGammaRequirementsAt' has converted it to an exact construction
-- placement, the type can generalize at the unwrapped target's own scope while
-- Gamma ownership remains attached to the source constructor.
applicationGeneralizationScopeForRequirements
  :: ApplicationConstructionScopes
  -> GeneralizationRequirements
  -> NodeRef
applicationGeneralizationScopeForRequirements scopes requirements
  | any isCurrentScopeGamma (grRequiredGammaBinders requirements) =
      applicationOccurrenceScope scopes
  | otherwise =
      applicationTargetGeneralizationScope scopes
  where
    isCurrentScopeGamma requirement =
      rgbPlacement requirement == RequiredGammaAtCurrentScope

-- | A construction-scope lookup must distinguish absence from disagreement.
-- Collapsing both states to 'Nothing' lets an ambiguous node override silently
-- fall back to a base scope, changing which lexical Gamma owns construction.
data ConstructionScopeSelection
  = MissingConstructionScope
  | UniqueConstructionScope NodeRef
  | AmbiguousConstructionScope (NonEmpty NodeRef)
  deriving (Eq, Show)

constructionNodeScopeSelection :: ConstructionScopes -> NodeId -> ConstructionScopeSelection
constructionNodeScopeSelection scopes (NodeId nodeKey) =
  constructionScopeSelection
    nodeKey
    (constructionNodeScopeCandidates scopes)

constructionBoundaryScopeSelection :: ConstructionScopes -> EdgeId -> ConstructionScopeSelection
constructionBoundaryScopeSelection scopes (EdgeId edgeKey) =
  constructionScopeSelection
    edgeKey
    (constructionBoundaryScopeCandidates scopes)

resolveConstructionScopeForNode
  :: (NodeId -> NodeId)
  -> GaBindParents p
  -> ConstructionScopes
  -> NodeId
  -> Either ElabError NodeRef
resolveConstructionScopeForNode canonical ga scopes nodeId =
  case constructionNodeScopeSelection scopes (canonical nodeId) of
    MissingConstructionScope -> scopeRootFromGaBase canonical ga nodeId
    UniqueConstructionScope scope -> pure scope
    AmbiguousConstructionScope candidates ->
      Left
        ( ValidationFailed
            [ "one construction node has conflicting source scopes",
              "  node: " ++ show (canonical nodeId),
              "  scopes: " ++ show candidates
            ]
        )

resolveConstructionScopeForBoundary
  :: (NodeId -> NodeId)
  -> GaBindParents p
  -> ConstructionScopes
  -> EdgeId
  -> NodeId
  -> Either ElabError NodeRef
resolveConstructionScopeForBoundary canonical ga scopes edgeId fallbackNode =
  case constructionBoundaryScopeSelection scopes edgeId of
    MissingConstructionScope ->
      resolveConstructionScopeForNode canonical ga scopes fallbackNode
    UniqueConstructionScope scope -> pure scope
    AmbiguousConstructionScope candidates ->
      Left
        ( ValidationFailed
            [ "one construction boundary has conflicting source scopes",
              "  edge: " ++ show edgeId,
              "  scopes: " ++ show candidates
            ]
        )

resolveApplicationConstructionScopes
  :: (NodeId -> NodeId)
  -> GaBindParents p
  -> ConstructionScopes
  -> EdgeId
  -> NodeId
  -> NodeId
  -> Either ElabError ApplicationConstructionScopes
resolveApplicationConstructionScopes
  canonical
  ga
  scopes
  boundaryEdge
  applicationNode
  targetNode =
    ApplicationConstructionScopes
      <$> resolveConstructionScopeForBoundary
        canonical
        ga
        scopes
        boundaryEdge
        applicationNode
      <*> resolveApplicationTargetScope
        canonical
        ga
        scopes
        boundaryEdge
        targetNode

resolveApplicationTargetScope
  :: (NodeId -> NodeId)
  -> GaBindParents p
  -> ConstructionScopes
  -> EdgeId
  -> NodeId
  -> Either ElabError NodeRef
resolveApplicationTargetScope canonical ga scopes edgeId targetNode =
  case applicationTargetScopeSelection scopes edgeId (canonical targetNode) of
    MissingConstructionScope ->
      resolveConstructionScopeForNode canonical ga scopes targetNode
    UniqueConstructionScope scope -> pure scope
    AmbiguousConstructionScope candidates ->
      Left
        ( ValidationFailed
            [ "one application target has conflicting source scopes",
              "  edge: " ++ show edgeId,
              "  target: " ++ show (canonical targetNode),
              "  scopes: " ++ show candidates
            ]
        )

applicationTargetScopeSelection
  :: ConstructionScopes
  -> EdgeId
  -> NodeId
  -> ConstructionScopeSelection
applicationTargetScopeSelection scopes (EdgeId edgeKey) (NodeId targetKey) =
  case
      IntMap.lookup edgeKey
        (constructionApplicationTargetScopeCandidates scopes)
        >>= IntMap.lookup targetKey
    of
      Nothing -> MissingConstructionScope
      Just candidates ->
        case Set.toAscList candidates of
          [] -> MissingConstructionScope
          [candidate] -> UniqueConstructionScope candidate
          first : second : rest ->
            AmbiguousConstructionScope (first :| second : rest)

constructionScopeSelection
  :: Int
  -> IntMap.IntMap (Set.Set NodeRef)
  -> ConstructionScopeSelection
constructionScopeSelection key candidatesByKey =
  case maybe [] Set.toAscList (IntMap.lookup key candidatesByKey) of
    [] -> MissingConstructionScope
    [candidate] -> UniqueConstructionScope candidate
    first : second : rest ->
      AmbiguousConstructionScope (first :| second : rest)

scopeRootFromGaBase
  :: (NodeId -> NodeId)
  -> GaBindParents p
  -> NodeId
  -> Either ElabError NodeRef
scopeRootFromGaBase canonical ga root =
  case resolveGaSolvedToBase ga (canonical root) of
    SolvedToBaseMapped baseNode -> scopeFromBaseNode baseNode
    SolvedToBaseSameDomain baseNode -> scopeFromBaseNode baseNode
    SolvedToBaseMissing -> pure (typeRef root)
  where
    scopeFromBaseNode baseNode = do
      path <-
        bindingPathToRootLocal
          (gaBindParentsBase ga)
          (typeRef baseNode)
      pure $
        case listToMaybe [gid | GenRef gid <- drop 1 path] of
          Just gid -> GenRef gid
          Nothing -> typeRef root

constructionScopes
  :: Constraint p
  -> Constraint p
  -> PresolutionView p
  -> IntMap.IntMap NodeId
  -> AnnExpr
  -> Either BindingError ConstructionScopes
constructionScopes base solvedForGen presolutionView redirects ann =
  let canonical = pvCanonical presolutionView
      addNodeOverride acc schemeRootId = do
        scope0 <- bindingScopeRef base schemeRootId
        let scope = canonicalizeScopeRef presolutionView redirects scope0
            schemeRootC = canonical (chaseRedirects redirects schemeRootId)
        postScope0 <- bindingScopeRef solvedForGen schemeRootC
        let postScope =
              canonicalizeScopeRef presolutionView redirects postScope0
        pure $
          if scope == postScope
            then acc
            else
              acc
                { constructionNodeScopeCandidates =
                    insertCandidate
                      (getNodeId schemeRootC)
                      scope
                      (constructionNodeScopeCandidates acc)
                }
      addBoundaryScope acc edgeId sourceNode = do
        scope0 <- bindingScopeRef base sourceNode
        let scope = canonicalizeScopeRef presolutionView redirects scope0
        pure
          acc
            { constructionBoundaryScopeCandidates =
                insertCandidate
                  (getEdgeId edgeId)
                  scope
                  (constructionBoundaryScopeCandidates acc)
            }
      addApplicationTargetScope acc edgeId sourceResultNode = do
        let sourceTargetNode =
              sourceGeneralizationTarget base sourceResultNode
            targetNode =
              canonical
                (chaseRedirects redirects sourceTargetNode)
        scope0 <- bindingScopeRef base sourceTargetNode
        let scope =
              canonicalizeScopeRef
                presolutionView
                redirects
                scope0
            insertTarget =
              IntMap.insertWith
                Set.union
                (getNodeId targetNode)
                (Set.singleton scope)
        pure
          acc
            { constructionApplicationTargetScopeCandidates =
                IntMap.insertWith
                  (IntMap.unionWith Set.union)
                  (getEdgeId edgeId)
                  (insertTarget IntMap.empty)
                  (constructionApplicationTargetScopeCandidates acc)
            }
      alg expr = case expr of
        AResolvedVarF _ _ _ -> pure mempty
        ALitF _ _ -> pure mempty
        ALamF _ _ _ _ body _ _ -> body
        AAppF fun arg funSite _argSite resultNode -> do
          -- The function edge is the application occurrence's unique scope
          -- authority.  The argument edge contributes to the same local Gamma
          -- obligation but is never queried as a scope boundary.
          funScopes <- fun
          argScopes <- arg
          occurrenceScopes <-
            addBoundaryScope
              (funScopes <> argScopes)
              (instantiationSiteEdgeId funSite)
              resultNode
          addApplicationTargetScope
            occurrenceScopes
            (instantiationSiteEdgeId funSite)
            resultNode
        ALetF _ _ _ schemeRootId _ _ rhs body _ -> do
          rhsScopes <- rhs
          bodyScopes <- body
          addNodeOverride (rhsScopes <> bodyScopes) schemeRootId
        AAnnF inner _ _ -> inner
        ALetScopeF inner resultNode edgeId -> do
          innerScopes <- inner
          addBoundaryScope innerScopes edgeId resultNode
        AUnfoldF inner _ _ -> inner
   in cata alg ann
  where
    sourceGeneralizationTarget constraint target =
      case lookupNodeIn (cNodes constraint) target of
        Just TyVar {tnBound = Just bound} ->
          case lookupNodeIn (cNodes constraint) bound of
            Just TyForall {tnBody = body} -> body
            _ -> bound
        Just TyForall {tnBody = body} -> body
        _ -> target

    insertCandidate key candidate =
      IntMap.insertWith Set.union key (Set.singleton candidate)
