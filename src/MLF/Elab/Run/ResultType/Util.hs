{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.ResultType.Util
  ( CandidateSelection (..),
    candidateSelectionIsAmbiguous,
    candidateSelectionValue,
    selectUniqueCandidate,
    selectUniqueCandidateBy,
    generalizeWithPlan,
    resultTypeRoots,
    containsBoundForall,
    instHasBoundForall,
    instantiateImplicitForalls,
    stripAnn,
    collectEdges,
  )
where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Presolution (PresolutionPlanBuilder (..), PresolutionView)
import MLF.Constraint.Types.Graph
  ( Constraint,
    EdgeId (..),
    NodeId (..),
    NodeRef (..),
  )
import MLF.Elab.Generalize (GaBindParents (..))
import MLF.Elab.Inst (applyInstantiation)
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen (AnnExpr (..), instantiationSiteEdgeId)

data CandidateSelection a
  = NoCandidateSelection
  | UniqueCandidateSelection a
  | AmbiguousCandidateSelection
  deriving (Eq, Show)

candidateSelectionValue :: CandidateSelection a -> Maybe a
candidateSelectionValue selection =
  case selection of
    UniqueCandidateSelection value ->
      Just value
    _ ->
      Nothing

candidateSelectionIsAmbiguous :: CandidateSelection a -> Bool
candidateSelectionIsAmbiguous selection =
  case selection of
    AmbiguousCandidateSelection ->
      True
    _ ->
      False

selectUniqueCandidate :: (Eq a) => [a] -> CandidateSelection a
selectUniqueCandidate = selectUniqueCandidateBy (==)

selectUniqueCandidateBy :: (a -> a -> Bool) -> [a] -> CandidateSelection a
selectUniqueCandidateBy eqCandidate =
  foldl' step NoCandidateSelection
  where
    step selection candidate =
      case selection of
        NoCandidateSelection ->
          UniqueCandidateSelection candidate
        UniqueCandidateSelection existing
          | eqCandidate existing candidate ->
              UniqueCandidateSelection existing
          | otherwise ->
              AmbiguousCandidateSelection
        AmbiguousCandidateSelection ->
          AmbiguousCandidateSelection

-- | Generalize with plan helper
generalizeWithPlan ::
  PresolutionPlanBuilder ->
  GaBindParents p ->
  PresolutionView p ->
  NodeRef ->
  NodeId ->
  Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
generalizeWithPlan planBuilder bindParentsGa presolutionView scopeRoot targetNode =
  generalizeAtWithBuilder
    planBuilder
    (Just bindParentsGa)
    presolutionView
    scopeRoot
    targetNode

resultTypeRoots ::
  (NodeId -> NodeId) ->
  Constraint p ->
  Constraint p ->
  AnnExpr ->
  AnnExpr ->
  (AnnExpr, AnnExpr)
resultTypeRoots _canonical _sourceConstraint _baseConstraint annCanon ann =
  (peelGeneratedLetRoots annCanon, peelGeneratedLetRoots ann)

-- The paired annotations passed to 'resultTypeRoots' are generated from the
-- same source expression, so syntactic lets are legitimate result-root
-- wrappers here. This function does not sit behind an elaborated-term identity
-- check and therefore must not be used for authoritative root selection.
peelGeneratedLetRoots :: AnnExpr -> AnnExpr
peelGeneratedLetRoots ann0 =
  case ann0 of
    ALet _ _ _ _ _ _ _ bodyAnn _ -> peelGeneratedLetRoots bodyAnn
    ALetScope bodyAnn _ _ -> peelGeneratedLetRoots bodyAnn
    _ -> ann0

-- | Check if a type contains foralls in bounds
containsBoundForall :: ElabType -> Bool
containsBoundForall ty =
  let go t = case t of
        TForallRef _ mb body ->
          maybe False containsAnyForallBound mb || go body
        TArrow a b -> go a || go b
        TConWithIdentity _ _ args -> any go args
        TVarAppRef _ args -> any go args
        _ -> False
      containsAnyForallBound bound = case bound of
        TArrow a b -> go a || go b
        TConWithIdentity _ _ args -> any go args
        TVarAppRef _ args -> any go args
        TForallRef _ _ _ -> True
        _ -> False
   in go ty

-- | Check if an instantiation contains foralls in bounds
instHasBoundForall :: Instantiation -> Bool
instHasBoundForall inst = cata instAlg inst
  where
    instAlg inst0 = case inst0 of
      InstIdF -> False
      InstSeqF a b -> a || b
      InstAppF ty -> containsForallTy ty
      InstBotF ty -> containsForallTy ty
      InstInsideF innerInst -> innerInst
      InstUnderFRef _ innerInst -> innerInst
      InstIntroF -> False
      InstElimF -> False
      InstAbstrFRef _ -> False

-- | Instantiate implicit foralls (foralls with bounds)
instantiateImplicitForalls :: ElabType -> ElabType
instantiateImplicitForalls ty0 =
  let go ty = case ty of
        TForallRef _ (Just _) _ ->
          case applyInstantiation ty InstElim of
            Right ty' -> go ty'
            Left _ -> ty
        TForallRef ref mb body ->
          TForallRef ref (fmap goBound mb) (go body)
        TArrow a b -> TArrow (go a) (go b)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap go args)
        TVarAppRef ref args -> TVarAppRef ref (fmap go args)
        TBaseWithIdentity _ _ -> ty
        TBottom -> ty
        TVarRef _ -> ty
        TMuRef ref body -> TMuRef ref (go body)
      goBound bound = case bound of
        TArrow a b -> TArrow (go a) (go b)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap go args)
        TVarAppRef ref args -> TVarAppRef ref (fmap go args)
        TBaseWithIdentity identity b -> TBaseWithIdentity identity b
        TBottom -> TBottom
        TForallRef ref mb body ->
          TForallRef ref (fmap goBound mb) (go body)
        TMuRef ref body -> TMuRef ref (go body)
   in go ty0

-- | Strip annotations from an AnnExpr
stripAnn :: AnnExpr -> AnnExpr
stripAnn ann0 = case ann0 of
  AAnn inner _ _ -> stripAnn inner
  AExactAnn inner _ _ _ -> stripAnn inner
  ALetScope inner _ _ -> stripAnn inner
  AUnfold inner _ _ -> stripAnn inner
  _ -> ann0

-- | Collect all edge IDs from an AnnExpr
collectEdges :: AnnExpr -> [EdgeId]
collectEdges ann0 = case ann0 of
  AResolvedVar _ _ _ -> []
  ALit _ _ -> []
  ALam _ _ _ _ body bodyEid _ -> bodyEid : collectEdges body
  AApp f a funSite argSite _ ->
    instantiationSiteEdgeId funSite
      : instantiationSiteEdgeId argSite
      : collectEdges f ++ collectEdges a
  ALet _ _ _ _ _ _ rhs body _ ->
    collectEdges rhs ++ collectEdges body
  AAnn inner _ eid -> eid : collectEdges inner
  AExactAnn inner _ _ eid -> eid : collectEdges inner
  ALetScope inner _ eid -> eid : collectEdges inner
  AUnfold inner _ eid -> eid : collectEdges inner
