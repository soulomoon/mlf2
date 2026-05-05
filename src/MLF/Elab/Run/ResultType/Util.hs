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
import qualified Data.IntSet as IntSet
import MLF.Constraint.Presolution (PresolutionPlanBuilder (..), PresolutionView)
import MLF.Constraint.Types.Graph
  ( Constraint,
    EdgeId (..),
    NodeId (..),
    NodeRef (..),
    cGenNodes,
    cLetEdges,
    getNodeId,
    gnSchemes,
    toListGen,
  )
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Elab.Generalize (GaBindParents (..))
import MLF.Elab.Inst (applyInstantiation)
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen (AnnExpr (..))

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
  GaBindParents 'Raw ->
  PresolutionView 'Raw ->
  NodeRef ->
  NodeId ->
  Either ElabError (ElabScheme, IntMap.IntMap String)
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
resultTypeRoots canonical sourceConstraint baseConstraint annCanon ann =
  (peelCanonical annCanon, peelPreCanonical ann)
  where
    schemeRootSet =
      IntSet.fromList
        [ getNodeId (canonical root)
          | gen <- map snd (toListGen (cGenNodes sourceConstraint)),
            root <- gnSchemes gen
        ]
    isSchemeRoot nid =
      IntSet.member (getNodeId (canonical nid)) schemeRootSet
    letEdges = cLetEdges baseConstraint
    isLetEdge (EdgeId edgeId) = IntSet.member edgeId letEdges

    peelCanonical ann0 = case ann0 of
      ALet _ _ _ _ _ _ bodyAnn nid ->
        case bodyAnn of
          AAnn inner target eid
            | canonical target == canonical nid
                && isLetEdge eid ->
                peelCanonical inner
            | canonical target == canonical nid
                && not (isSchemeRoot target) ->
                peelCanonical inner
          AUnfold inner target eid
            | canonical target == canonical nid
                && isLetEdge eid ->
                peelCanonical inner
            | canonical target == canonical nid
                && not (isSchemeRoot target) ->
                peelCanonical inner
          _ -> bodyAnn
      _ -> ann0

    peelPreCanonical ann0 = case ann0 of
      ALet _ _ _ _ _ _ bodyAnn nid ->
        case bodyAnn of
          AAnn inner target eid
            | target == nid
                && isLetEdge eid ->
                peelPreCanonical inner
            | target == nid
                && not (isSchemeRoot target) ->
                peelPreCanonical inner
          AUnfold inner target eid
            | target == nid
                && isLetEdge eid ->
                peelPreCanonical inner
            | target == nid
                && not (isSchemeRoot target) ->
                peelPreCanonical inner
          _ -> bodyAnn
      _ -> ann0

-- | Check if a type contains foralls in bounds
containsBoundForall :: ElabType -> Bool
containsBoundForall ty =
  let go t = case t of
        TForall _ mb body ->
          maybe False containsAnyForallBound mb || go body
        TArrow a b -> go a || go b
        TCon _ args -> any go args
        _ -> False
      containsAnyForallBound bound = case bound of
        TArrow a b -> go a || go b
        TCon _ args -> any go args
        TForall _ _ _ -> True
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
      InstUnderF _ innerInst -> innerInst
      InstIntroF -> False
      InstElimF -> False
      InstAbstrF _ -> False

-- | Instantiate implicit foralls (foralls with bounds)
instantiateImplicitForalls :: ElabType -> ElabType
instantiateImplicitForalls ty0 =
  let go ty = case ty of
        TForall _ (Just _) _ ->
          case applyInstantiation ty InstElim of
            Right ty' -> go ty'
            Left _ -> ty
        TForall v mb body ->
          TForall v (fmap goBound mb) (go body)
        TArrow a b -> TArrow (go a) (go b)
        TCon c args -> TCon c (fmap go args)
        TBase _ -> ty
        TBottom -> ty
        TVar _ -> ty
        TMu v body -> TMu v (go body)
      goBound bound = case bound of
        TArrow a b -> TArrow (go a) (go b)
        TCon c args -> TCon c (fmap go args)
        TBase b -> TBase b
        TBottom -> TBottom
        TForall v mb body ->
          TForall v (fmap goBound mb) (go body)
        TMu v body -> TMu v (go body)
   in go ty0

-- | Strip annotations from an AnnExpr
stripAnn :: AnnExpr -> AnnExpr
stripAnn ann0 = case ann0 of
  AAnn inner _ _ -> stripAnn inner
  AUnfold inner _ _ -> stripAnn inner
  _ -> ann0

-- | Collect all edge IDs from an AnnExpr
collectEdges :: AnnExpr -> [EdgeId]
collectEdges ann0 = case ann0 of
  AVar _ _ -> []
  ALit _ _ -> []
  ALam _ _ _ body _ -> collectEdges body
  AApp f a funEid argEid _ ->
    funEid : argEid : collectEdges f ++ collectEdges a
  ALet _ _ _ _ _ rhs body _ ->
    collectEdges rhs ++ collectEdges body
  AAnn inner _ eid -> eid : collectEdges inner
  AUnfold inner _ eid -> eid : collectEdges inner
