module MLF.Elab.Run.Scope
  ( bindingScopeRef,
    bindingScopeRefCanonical,
    generalizeTargetNode,
    schemeBodyTarget,
    canonicalizeScopeRef,
    resolveCanonicalScope,
    letScopeOverrides,
  )
where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (listToMaybe)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution (PresolutionView)
import MLF.Constraint.Types
  ( BindingError (..),
    Constraint,
    NodeId (..),
    NodeRef (..),
    TyNode (..),
    getNodeId,
    gnSchemes,
    typeRef,
  )
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Frontend.ConstraintGen (AnnExpr (..))
import MLF.Frontend.ConstraintGen.Types (AnnExprF (..))

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

  3. `letScopeOverrides`: Computes scope on the base constraint c1, compares
     with the solved constraint's scope.  Prefers the base scope when they
     diverge.  This is correct: the thesis defines ga′ on the original χ_p,
     not the solved version.

Conclusion: the pipeline is thesis-aligned and propagates binding-tree errors
consistently through original-constraint scope selection and canonical scope
resolution without a redundant second binding-tree lookup.
-}
bindingScopeRef :: Constraint -> NodeId -> Either BindingError NodeRef
bindingScopeRef constraint root = do
  path <- Binding.bindingPathToRoot constraint (typeRef root)
  case listToMaybe [gid | GenRef gid <- drop 1 path] of
    Just gid -> Right (GenRef gid)
    Nothing -> Right (TypeRef root)

-- | Canonical-domain variant of 'bindingScopeRef' that traverses canonical bind-parents from a presolution view.
bindingScopeRefCanonical :: PresolutionView -> NodeId -> Either BindingError NodeRef
bindingScopeRefCanonical presolutionView root =
  bindingScopeRef (ChiQuery.chiCanonicalConstraint presolutionView) root

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

targetUnwrapInfo :: PresolutionView -> NodeId -> TargetUnwrapInfo
targetUnwrapInfo presolutionView target =
  let canonical = ChiQuery.chiCanonical presolutionView
      targetC = canonical target
      targetNode = ChiQuery.chiLookupNode presolutionView targetC
      boundCanonical = case targetNode of
        Just TyVar {tnBound = Just bnd} -> Just (canonical bnd)
        _ -> Nothing
      boundNode = boundCanonical >>= ChiQuery.chiLookupNode presolutionView
   in TargetUnwrapInfo
        { tuiTargetCanonical = targetC,
          tuiTargetNode = targetNode,
          tuiBoundCanonical = boundCanonical,
          tuiBoundNode = boundNode
        }

generalizeTargetNode :: PresolutionView -> NodeId -> NodeId
generalizeTargetNode presolutionView target =
  let canonical = ChiQuery.chiCanonical presolutionView
      info = targetUnwrapInfo presolutionView target
   in case tuiTargetNode info of
        Just TyVar {tnBound = Just _} ->
          case (tuiBoundCanonical info, tuiBoundNode info) of
            (Just _, Just TyForall {tnBody = body}) -> canonical body
            (Just bndC, _) -> bndC
            _ -> tuiTargetCanonical info
        Just TyForall {tnBody = body} -> canonical body
        _ -> tuiTargetCanonical info

schemeBodyTarget :: PresolutionView -> NodeId -> NodeId
schemeBodyTarget presolutionView target =
  let constraint = ChiQuery.chiConstraint presolutionView
      canonical = ChiQuery.chiCanonical presolutionView
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
              Just bnd <- [ChiQuery.chiLookupVarBound presolutionView root],
              case ChiQuery.chiLookupNode presolutionView (canonical bnd) of
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
     remains a TypeRef (not upgraded to GenRef).  However, `letScopeOverrides`
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
canonicalizeScopeRef :: PresolutionView -> IntMap.IntMap NodeId -> NodeRef -> NodeRef
canonicalizeScopeRef presolutionView redirects scopeRef =
  case scopeRef of
    GenRef gid -> GenRef gid
    TypeRef nid ->
      let canonical = ChiQuery.chiCanonical presolutionView
       in TypeRef (canonical (chaseRedirects redirects nid))

resolveCanonicalScope :: Constraint -> PresolutionView -> IntMap.IntMap NodeId -> NodeId -> Either BindingError NodeRef
resolveCanonicalScope constraint presolutionView redirects scopeRoot = do
  scope0 <- bindingScopeRef constraint scopeRoot
  pure (canonicalizeScopeRef presolutionView redirects scope0)

letScopeOverrides :: Constraint -> Constraint -> PresolutionView -> IntMap.IntMap NodeId -> AnnExpr -> IntMap.IntMap NodeRef
letScopeOverrides base solvedForGen presolutionView redirects ann =
  let canonical = ChiQuery.chiCanonical presolutionView
      addOverride acc schemeRootId =
        case bindingScopeRef base schemeRootId of
          Right scope0 ->
            let scope = canonicalizeScopeRef presolutionView redirects scope0
                schemeRootC = canonical (chaseRedirects redirects schemeRootId)
                postScope =
                  case bindingScopeRef solvedForGen schemeRootC of
                    Right ref -> canonicalizeScopeRef presolutionView redirects ref
                    Left _ -> scope
             in if scope == postScope
                  then acc
                  else IntMap.insert (getNodeId schemeRootC) scope acc
          Left _ -> acc
      alg expr = case expr of
        AVarF _ _ -> IntMap.empty
        ALitF _ _ -> IntMap.empty
        ALamF _ _ _ body _ -> body
        AAppF fun arg _ _ _ -> IntMap.union arg fun
        ALetF _ _ schemeRootId _ _ rhs body _ ->
          let baseMap = addOverride IntMap.empty schemeRootId
           in IntMap.union body (IntMap.union rhs baseMap)
        AAnnF inner _ _ -> inner
        AUnfoldF inner _ _ -> inner
   in cata alg ann
