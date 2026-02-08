module MLF.Elab.Run.Debug (
    debugGaScope,
    debugGaScopeEnabled,
    debugWhenM,
    debugWhenCondM,
    edgeOrigins
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import Data.List (intercalate)

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.ConstraintGen.Types (AnnExprF(..))
import MLF.Constraint.Types.Graph (EdgeId(..), NodeId(..), getEdgeId)
import MLF.Elab.Run.Annotation (mapAnnNodes)
import MLF.Util.Trace (TraceConfig, traceBinding, tcBinding)


debugGaScope :: TraceConfig -> String -> a -> a
debugGaScope cfg = traceBinding cfg

debugGaScopeEnabled :: TraceConfig -> Bool
debugGaScopeEnabled = tcBinding

-- | Monadic debug helper that traces a message when debugging is enabled.
-- Replaces the verbose pattern:
--   case if debugGaScopeEnabled cfg then debugGaScope cfg msg () else () of () -> pure ()
debugWhenM :: Applicative m => TraceConfig -> String -> m ()
debugWhenM cfg msg =
    if debugGaScopeEnabled cfg
        then debugGaScope cfg msg (pure ())
        else pure ()

-- | Conditional debug helper that only traces when both debugging is enabled
-- and the condition is true.
debugWhenCondM :: Applicative m => TraceConfig -> Bool -> String -> m ()
debugWhenCondM cfg cond msg =
    if debugGaScopeEnabled cfg && cond
        then debugGaScope cfg msg (pure ())
        else pure ()

edgeOrigins :: AnnExpr -> IntMap.IntMap String
edgeOrigins = fmap (intercalate " | ") . aoMap . cata alg . mapAnnNodes id
  where
    tagNode nid = "node=" ++ show nid
    insertEdge eid msg =
        IntMap.insertWith
            (\new old -> old ++ new)
            (getEdgeId eid)
            [msg]
    alg expr = case expr of
        AVarF _ nid -> OriginAcc nid IntMap.empty
        ALitF _ nid -> OriginAcc nid IntMap.empty
        ALamF _ _ _ body nid -> OriginAcc nid (aoMap body)
        AAppF fun arg funEid argEid nid ->
            let msgFun =
                    "AApp fun " ++ tagNode (aoNode fun)
                        ++ " arg=" ++ tagNode (aoNode arg)
                        ++ " app=" ++ show nid
                msgArg =
                    "AApp arg " ++ tagNode (aoNode arg)
                        ++ " fun=" ++ tagNode (aoNode fun)
                        ++ " app=" ++ show nid
                accChildren = IntMap.unionWith (++) (aoMap arg) (aoMap fun)
                acc1 = insertEdge argEid msgArg accChildren
                acc2 = insertEdge funEid msgFun acc1
            in OriginAcc nid acc2
        ALetF _ _ _ _ _ rhs body nid ->
            let accChildren = IntMap.unionWith (++) (aoMap body) (aoMap rhs)
            in OriginAcc nid accChildren
        AAnnF inner target eid ->
            let msg = "AAnn inner=" ++ tagNode (aoNode inner) ++ " target=" ++ show target
                acc1 = insertEdge eid msg (aoMap inner)
            in OriginAcc target acc1


data OriginAcc = OriginAcc
    { aoNode :: NodeId
    , aoMap :: IntMap.IntMap [String]
    }
