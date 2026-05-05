{-# LANGUAGE DataKinds #-}
module MLF.Elab.Legacy (
    expansionToInst,
    expInstantiateArgsToInstNoFallback
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Types.Graph (NodeId)
import MLF.Constraint.Types.Witness (Expansion, ExpansionF(..))
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Presolution.View (PresolutionView(..), fromSolved)
import MLF.Elab.Run.TypeOps (inlineBoundVarsTypeForBound)
import MLF.Elab.Types (ElabError, ElabType, Instantiation(..))
import MLF.Reify.Core (reifyTypeWithNamedSetNoFallback, reifyTypeWithNamesNoFallback)
import MLF.Reify.TypeOps (resolveBaseBoundForInstConstraint)
import MLF.Util.RecursionSchemes (cataM)

-- | Convert an Expansion to an Instantiation witness.
-- This translates the presolution expansion recipe into an xMLF instantiation.
--
-- Note on paper alignment:
-- Presolution's expansions (Instantiate, Forall, Compose) map roughly to
-- xMLF's instantiations (App/Elim, Intro, Seq).
-- Specifically:
--   - ExpInstantiate args: In xMLF, instantiation is N (Elim) followed by
--     substitution. Presolution does "forall elimination + substitution" in one step.
--     We map this to a sequence of (N; ⟨τ⟩) or just ⟨τ⟩ depending on context,
--     but since xMLF ⟨τ⟩ usually implies elimination in standard F, we model
--     ExpInstantiate as a sequence of type applications ⟨τ⟩ which implicitly
--     includes the elimination step N where needed, or explicit N if args are empty.
--     Actually, looking at `papers/these-finale-english.txt` (see `papers/xmlf.txt`):
--       (∀(α ⩾ τ) τ') N  --> τ'{α ← τ}  (Eliminate quantifier)
--       (∀(α ⩾ τ) τ') !α --> τ'{α ← τ}  (Abstract bound)
--     Presolution's ExpInstantiate [t1..tn] means "instantiate the outermost
--     quantifiers with t1..tn". This corresponds to N; ⟨t1⟩; ...; N; ⟨tn⟩.
--     However, standard presentation often folds N into the application.
--     For strict adherence, we should emit N for every quantifier eliminated.
--     But ExpInstantiate removes the quantifier *and* substitutes.
--     We map ExpInstantiate [t] to (N; ⟨t⟩) if it replaces a bounded var,
--     or just ⟨t⟩ if it's a standard F app.
--     Given we don't track the *source* type here easily, we generate a sequence
--     of applications ⟨t⟩, assuming the elaboration context or a later pass
--     refines this if explicit N is required.
--     For now: ExpInstantiate [t] -> ⟨t⟩.
expInstantiateArgsToInstNoFallback
    :: PresolutionView 'Raw
    -> IntSet.IntSet
    -> [NodeId]
    -> Either ElabError Instantiation
expInstantiateArgsToInstNoFallback presolutionView namedSet args = do
    tys <- mapM reifyArg args
    instAppsFromTypes presolutionView tys
  where
    constraint = pvConstraint presolutionView
    canonical = pvCanonical presolutionView
    resolveBaseBound = resolveBaseBoundForInstConstraint constraint canonical
    reifyArg arg =
        let argC = canonical arg
        in case resolveBaseBound argC of
            Just baseC ->
                reifyTypeWithNamedSetNoFallback presolutionView IntMap.empty namedSet baseC
            Nothing ->
                reifyTypeWithNamedSetNoFallback presolutionView IntMap.empty namedSet argC

expansionToInst :: Solved -> Expansion -> Either ElabError Instantiation
expansionToInst solved = cataM alg
  where
    constraint = Solved.originalConstraint solved
    canonical = Solved.canonical solved
    presolutionView = fromSolved solved
    resolveBaseBound = resolveBaseBoundForInstConstraint constraint canonical
    reifyArg arg =
        let argC = canonical arg
        in case resolveBaseBound argC of
            Just baseC -> reifyTypeWithNamesNoFallback presolutionView IntMap.empty baseC
            Nothing -> reifyTypeWithNamesNoFallback presolutionView IntMap.empty argC
    alg layer = case layer of
        ExpIdentityF -> Right InstId
        ExpInstantiateF args -> do
            tys <- mapM reifyArg args
            instAppsFromTypes presolutionView tys
        ExpForallF _ -> Right InstIntro
        ExpComposeF exps -> Right $ foldr1 InstSeq (NE.toList exps)

instAppsFromTypes :: PresolutionView p -> [ElabType] -> Either ElabError Instantiation
instAppsFromTypes presolutionView tys =
    let tys' = map (inlineBoundVarsTypeForBound presolutionView) tys
    in if null tys'
        then Right InstId
        else Right $ foldr1 InstSeq (map InstApp tys')
