module MLF.Elab.Run.Debug (
    debugGaScope,
    debugGaScopeEnabled,
    debugWhenM,
    debugWhenCondM
) where

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
