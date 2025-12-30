module MLF.Elab.Run (
    runPipelineElab,
    runPipelineElabChecked,
    applyRedirectsToAnn,
    chaseRedirects
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Frontend.Syntax (Expr)
import MLF.Frontend.ConstraintGen (AnnExpr(..), ConstraintError, ConstraintResult(..), generateConstraints)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Acyclicity (checkAcyclicity)
import qualified MLF.Constraint.Root as ConstraintRoot
import MLF.Constraint.Presolution (computePresolution, PresolutionResult(..))
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import MLF.Constraint.Types (NodeId, PolySyms, getNodeId)
import MLF.Elab.Elaborate (elaborate)
import MLF.Elab.Generalize (generalizeAt)
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types

-- | Run the full pipeline (Phases 1–5) then elaborate.
runPipelineElab :: PolySyms -> Expr -> Either String (ElabTerm, ElabType)
runPipelineElab polySyms = runPipelineElabWith (generateConstraints polySyms)

runPipelineElabChecked :: PolySyms -> Expr -> Either String (ElabTerm, ElabType)
runPipelineElabChecked polySyms expr = do
    (term, _ty) <- runPipelineElab polySyms expr
    tyChecked <- firstShow (typeCheck term)
    pure (term, tyChecked)

runPipelineElabWith
    :: (Expr -> Either ConstraintError ConstraintResult)
    -> Expr
    -> Either String (ElabTerm, ElabType)
runPipelineElabWith genConstraints expr = do
    ConstraintResult { crConstraint = c0, crRoot = root, crAnnotated = ann } <- firstShow (genConstraints expr)
    let c1 = normalize c0
    acyc <- firstShow (checkAcyclicity c1)
    pres <- firstShow (computePresolution acyc c1)
    solved <- firstShow (solveUnify (prConstraint pres))
    case validateSolvedGraphStrict solved of
        [] -> do
            let ann' = applyRedirectsToAnn (prRedirects pres) ann
            term <- firstShow (elaborate solved (prEdgeWitnesses pres) (prEdgeTraces pres) ann')

            -- Also apply redirects to the root node, as it might have been a TyExp
            let root' = chaseRedirects (prRedirects pres) root

            -- Generalize at the root (top-level generalization).
            let scopeRoot =
                    case ConstraintRoot.findConstraintRoot (srConstraint solved) of
                        Just rootId -> rootId
                        Nothing -> root'
            (sch, _subst) <- firstShow (generalizeAt solved scopeRoot root')
            let ty = case sch of
                    Forall binds body -> foldr (\(n, b) t -> TForall n b t) body binds

            pure (term, ty)
        vs -> Left ("validateSolvedGraph failed:\n" ++ unlines vs)

applyRedirectsToAnn :: IntMap.IntMap NodeId -> AnnExpr -> AnnExpr
applyRedirectsToAnn redirects ann = case ann of
    ALit l nid -> ALit l (redir nid)
    AVar v nid -> AVar v (redir nid)
    ALam v pNode x bodyAnn nid ->
        ALam v (redir pNode) (redir x) (applyRedirectsToAnn redirects bodyAnn) (redir nid)
    AApp fAnn argAnn funEid argEid nid ->
        AApp
            (applyRedirectsToAnn redirects fAnn)
            (applyRedirectsToAnn redirects argAnn)
            funEid
            argEid
            (redir nid)
    ALet v schNode ev childLevel rhsAnn bodyAnn nid ->
        ALet v (redir schNode) ev (redir childLevel) (applyRedirectsToAnn redirects rhsAnn) (applyRedirectsToAnn redirects bodyAnn) (redir nid)
    AAnn exprAnn nid eid -> AAnn (applyRedirectsToAnn redirects exprAnn) (redir nid) eid
  where
    redir = chaseRedirects redirects

-- | Chase redirects through the map until stable or missing
chaseRedirects :: IntMap.IntMap NodeId -> NodeId -> NodeId
chaseRedirects redirects nid = case IntMap.lookup (getNodeId nid) redirects of
    Just n' -> if n' == nid then nid else chaseRedirects redirects n'
    Nothing -> nid

firstShow :: Show e => Either e a -> Either String a
firstShow = either (Left . show) Right
