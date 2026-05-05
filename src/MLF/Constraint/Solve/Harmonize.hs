module MLF.Constraint.Solve.Harmonize (
    batchHarmonizeConstraint
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Adjustment as BindingAdjustment
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution
import MLF.Constraint.Unify.Closure (SolveError(..))
import qualified MLF.Util.UnionFind as UnionFind

-- | Apply the thesis-style generalized admissibility repair once per final UF
-- equivalence class, rather than pairwise during worklist reduction.
batchHarmonizeConstraint :: IntMap NodeId -> Constraint p -> Either SolveError (Constraint p)
batchHarmonizeConstraint uf c0 =
    foldl' harmonizeOne (Right c0) (equivalenceClasses c0)
  where
    harmonizeOne :: Either SolveError (Constraint p) -> [NodeId] -> Either SolveError (Constraint p)
    harmonizeOne acc members = do
        cBefore <- acc
        let refs = map typeRef members
        case BindingAdjustment.harmonizeBindParentsMulti refs cBefore of
            Left err -> Left (BindingTreeError err)
            Right (cAfter, _trace) -> Right cAfter

    equivalenceClasses :: Constraint p -> [[NodeId]]
    equivalenceClasses c =
        let canonical = UnionFind.frWith uf
            allNodeIds = map fst (toListNode (cNodes c))
            grouped =
                IntMap.toList $
                    foldl'
                        (\acc nid ->
                            let rep = canonical nid
                            in IntMap.insertWith (++) (getNodeId rep) [nid] acc
                        )
                        IntMap.empty
                        allNodeIds
        in [ members | (_, members) <- grouped, length members > 1 ]
