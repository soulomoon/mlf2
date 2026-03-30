{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.ResultType.Fallback
  ( computeResultTypeFallback,
    computeResultTypeFallbackWithView,
  )
where

import MLF.Elab.Run.ResultType.Fallback.Core (computeResultTypeFallbackCore)
import MLF.Elab.Run.ResultType.Types
  ( ResultTypeInputs (..),
  )
import MLF.Elab.Run.ResultType.Util (generalizeWithPlan)
import qualified MLF.Elab.Run.ResultType.View as View
import MLF.Elab.Run.Scope
  ( resolveCanonicalScope,
    schemeBodyTarget,
  )
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen (AnnExpr (..))

type ResultTypeRecursor =
  ResultTypeInputs ->
  View.ResultTypeView ->
  AnnExpr ->
  AnnExpr ->
  Either ElabError ElabType

-- | Compute result type when there's no direct annotation (fallback path).
-- Note: This function handles the non-AAnn case. When the root is an AAnn,
-- the facade (ResultType.hs) dispatches to computeResultTypeFromAnn instead.
computeResultTypeFallback ::
  ResultTypeRecursor ->
  ResultTypeInputs ->
  View.ResultTypeView ->
  -- | annCanon (post-redirect)
  AnnExpr ->
  -- | ann (pre-redirect)
  AnnExpr ->
  Either ElabError ElabType
computeResultTypeFallback recurse ctx view annCanon ann =
  computeResultTypeFallbackWithView recurse ctx view annCanon ann

computeResultTypeFallbackWithView ::
  ResultTypeRecursor ->
  ResultTypeInputs ->
  View.ResultTypeView ->
  AnnExpr ->
  AnnExpr ->
  Either ElabError ElabType
computeResultTypeFallbackWithView recurse ctx view annCanon ann = do
  let presolutionViewForGen = View.rtvPresolutionViewOverlay view
  -- Note [Annotated Lambda Result Type]
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- When we have an annotated lambda like `\x : τ. body`, it desugars to:
  --   λx. let x = cτ x in body
  -- The result type should be an arrow from the annotation type τ to the
  -- body's type. We detect this pattern and handle it specially to produce
  -- the correct result type with bounded quantification.
  --
  -- For thesis-exact semantics with rank-2 annotations, the result type is:
  --   ∀a ⩾ bodyTy. paramTy -> a
  -- where `a` is a fresh bounded variable bounded by the body's type.
  --
  -- For simple (non-rank-2) annotations, the result type is:
  --   paramTy -> bodyTy
  --
  -- Pattern: ALam paramName _ _ (ALet letName _ _ _ _ (AAnn _ annNode _) bodyAnn _) _
  -- where paramName == letName
  case annCanon of
    ALam
      paramName
      _paramNode
      _scopeRoot
      (ALet letName _schemeGen _schemeRoot _expVar _rhsGen rhsAnn bodyAnn _letNode)
      _lamNode
        | paramName == letName,
          AAnn _innerAnn annNodeId _eid <- rhsAnn -> do
            -- This is an annotated lambda pattern.
            -- Get the parameter type from the coercion's codomain.
            -- We need to generalize at the annotation node to get the full
            -- type with any forall wrappers.
            let bindParentsGa = rtcBindParentsGa ctx
                planBuilder = rtcPlanBuilder ctx
                c1 = rtcBaseConstraint ctx
                redirects = rtcRedirects ctx
            -- Find the scope root for the annotation node
            scopeRoot <- bindingToElab (resolveCanonicalScope c1 presolutionViewForGen redirects annNodeId)
            let targetC = schemeBodyTarget presolutionViewForGen annNodeId
            (paramSch, _subst) <-
              generalizeWithPlan
                planBuilder
                bindParentsGa
                presolutionViewForGen
                scopeRoot
                targetC
            let paramTy = case paramSch of
                  Forall binds body -> foldr (\(n, b) t -> TForall n b t) body binds
            -- Compute the result type from the body.
            -- The body may be wrapped in AAnn (from alternative let scoping),
            -- so we need to handle that case.
            bodyTy <- computeBodyResultType recurse ctx view bodyAnn
            -- Check if the parameter type is a rank-2 type (contains forall).
            -- For rank-2 annotations, wrap the result in a bounded quantifier.
            -- For simple annotations, just return the arrow type.
            let isRank2 = containsForallTy paramTy
            if isRank2
              then do
                -- For thesis-exact semantics, wrap the result in a bounded quantifier.
                -- The result type is: ∀a ⩾ bodyTy. paramTy -> a
                let resultVar = "a"
                boundTy <- case elabToBound bodyTy of
                  Left err -> Left (ValidationFailed ["elabToBound failed: " ++ err])
                  Right b -> Right b
                let boundedResultTy =
                      TForall
                        resultVar
                        (Just boundTy)
                        (TArrow paramTy (TVar resultVar))
                pure boundedResultTy
              else
                -- For simple annotations, just return the arrow type.
                pure (TArrow paramTy bodyTy)
    _ -> computeResultTypeFallbackCore ctx view annCanon ann

-- | Compute result type for the body of an annotated lambda.
-- This handles the case where the body is wrapped in AAnn.
computeBodyResultType ::
  ResultTypeRecursor ->
  ResultTypeInputs ->
  View.ResultTypeView ->
  AnnExpr ->
  Either ElabError ElabType
computeBodyResultType recurse ctx view bodyAnn =
  case bodyAnn of
    AAnn inner _ _ -> recurse ctx view inner inner
    _ ->
      computeResultTypeFallbackCore ctx view bodyAnn bodyAnn
