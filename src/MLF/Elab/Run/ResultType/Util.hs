{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType.Util (
    generalizeWithPlan,
    containsBoundForall,
    instHasBoundForall,
    instantiateImplicitForalls,
    stripAnn,
    collectEdges,
) where

import Data.Functor.Foldable (cata)

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Presolution (PresolutionPlanBuilder(..))
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Constraint.Types.Graph
    ( NodeId(..)
    , NodeRef(..)
    , BindingError(..)
    )
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Inst (applyInstantiation)
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Constraint.Types.Graph (EdgeId(..))

-- | Generalize with plan helper
generalizeWithPlan
    :: PresolutionPlanBuilder
    -> GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeWithPlan planBuilder bindParentsGa res scopeRoot targetNode =
    case generalizeAtWithBuilder
        planBuilder
        (Just bindParentsGa)
        res
        scopeRoot
        targetNode of
        Right out -> Right out
        Left (BindingTreeError GenSchemeFreeVars{}) ->
            generalizeAtWithBuilder
                planBuilder
                Nothing
                res
                scopeRoot
                targetNode
        Left err -> Left err

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
        goBound bound = case bound of
            TArrow a b -> TArrow (go a) (go b)
            TCon c args -> TCon c (fmap go args)
            TBase b -> TBase b
            TBottom -> TBottom
            TForall v mb body ->
                TForall v (fmap goBound mb) (go body)
    in go ty0

-- | Strip annotations from an AnnExpr
stripAnn :: AnnExpr -> AnnExpr
stripAnn ann0 = case ann0 of
    AAnn inner _ _ -> stripAnn inner
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
