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
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.ConstraintGen.Types (AnnExprF(..))
import MLF.Constraint.Types (EdgeId(..), NodeId(..), getEdgeId)


debugGaScope :: String -> a -> a
debugGaScope msg value =
    if debugGaScopeEnabled
        then trace msg value
        else value

debugGaScopeEnabled :: Bool
debugGaScopeEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_BINDING"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugGaScopeEnabled #-}

-- | Monadic debug helper that traces a message when debugging is enabled.
-- Replaces the verbose pattern:
--   case if debugGaScopeEnabled then debugGaScope msg () else () of () -> pure ()
debugWhenM :: Applicative m => String -> m ()
debugWhenM msg =
    if debugGaScopeEnabled
        then debugGaScope msg (pure ())
        else pure ()

-- | Conditional debug helper that only traces when both debugging is enabled
-- and the condition is true.
debugWhenCondM :: Applicative m => Bool -> String -> m ()
debugWhenCondM cond msg =
    if debugGaScopeEnabled && cond
        then debugGaScope msg (pure ())
        else pure ()

edgeOrigins :: AnnExpr -> IntMap.IntMap String
edgeOrigins = fmap (intercalate " | ") . aoMap . cata alg
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
