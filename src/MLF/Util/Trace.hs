{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Util.Trace
Description : Effect-based debug tracing infrastructure
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides a pure, effect-based approach to debug tracing,
replacing the previous implicit IO-based pattern. Tracing is controlled
via a 'TraceConfig' that can be initialized from environment variables
at program startup.

= Usage

At program entry point:

@
main = do
    traceConfig <- traceConfigFromEnv
    let result = runWithTrace traceConfig myComputation
    ...
@

Within pure code, use the trace functions which are no-ops when disabled:

@
traceBinding :: TraceConfig -> String -> a -> a
traceBinding cfg msg = traceWhen (tcBinding cfg) msg
@
-}
module MLF.Util.Trace (
    -- * Configuration
    TraceConfig(..),
    defaultTraceConfig,
    traceConfigFromEnv,
    -- * Pure tracing functions
    traceWhen,
    traceBinding,
    tracePresolution,
    traceSolve,
    traceElab,
    traceNormalize,
    traceGeneralize,
    -- * Monadic tracing (for StateT-based code)
    traceBindingM,
    tracePresolutionM,
    traceSolveM,
    traceElabM,
) where

import Control.Monad (when)
import Debug.Trace (trace)
import System.Environment (lookupEnv)

-- | Configuration for which trace categories are enabled.
data TraceConfig = TraceConfig
    { tcBinding :: !Bool      -- ^ Binding tree operations (MLF_DEBUG_BINDING)
    , tcPresolution :: !Bool  -- ^ Presolution phase (MLF_DEBUG_PRESOLUTION)
    , tcSolve :: !Bool        -- ^ Solve phase (MLF_DEBUG_SOLVE)
    , tcElab :: !Bool         -- ^ Elaboration phase (MLF_DEBUG_ELAB)
    , tcNormalize :: !Bool    -- ^ Normalization phase (MLF_DEBUG_NORMALIZE)
    , tcGeneralize :: !Bool   -- ^ Generalization phase (MLF_DEBUG_GENERALIZE)
    } deriving (Eq, Show)

-- | Default configuration with all tracing disabled.
defaultTraceConfig :: TraceConfig
defaultTraceConfig = TraceConfig
    { tcBinding = False
    , tcPresolution = False
    , tcSolve = False
    , tcElab = False
    , tcNormalize = False
    , tcGeneralize = False
    }

-- | Check if an environment variable is set (non-empty).
isEnvSet :: String -> IO Bool
isEnvSet name = do
    val <- lookupEnv name
    pure $ case val of
        Nothing -> False
        Just "" -> False
        Just _ -> True

-- | Initialize trace configuration from environment variables.
--
-- Recognized variables:
--   * @MLF_DEBUG_BINDING@ - Enable binding tree tracing
--   * @MLF_DEBUG_PRESOLUTION@ - Enable presolution tracing
--   * @MLF_DEBUG_SOLVE@ - Enable solve phase tracing
--   * @MLF_DEBUG_ELAB@ - Enable elaboration tracing
--   * @MLF_DEBUG_NORMALIZE@ - Enable normalization tracing
--   * @MLF_DEBUG_ALL@ - Enable all tracing
traceConfigFromEnv :: IO TraceConfig
traceConfigFromEnv = do
    allEnabled <- isEnvSet "MLF_DEBUG_ALL"
    if allEnabled
        then pure TraceConfig
            { tcBinding = True
            , tcPresolution = True
            , tcSolve = True
            , tcElab = True
            , tcNormalize = True
            , tcGeneralize = True
            }
        else TraceConfig
            <$> isEnvSet "MLF_DEBUG_BINDING"
            <*> isEnvSet "MLF_DEBUG_PRESOLUTION"
            <*> isEnvSet "MLF_DEBUG_SOLVE"
            <*> isEnvSet "MLF_DEBUG_ELAB"
            <*> isEnvSet "MLF_DEBUG_NORMALIZE"
            <*> isEnvSet "MLF_DEBUG_GENERALIZE"

-- | Conditionally trace a message.
traceWhen :: Bool -> String -> a -> a
traceWhen enabled msg value
    | enabled = trace msg value
    | otherwise = value

-- | Trace binding operations when enabled.
traceBinding :: TraceConfig -> String -> a -> a
traceBinding cfg = traceWhen (tcBinding cfg)

-- | Trace presolution operations when enabled.
tracePresolution :: TraceConfig -> String -> a -> a
tracePresolution cfg = traceWhen (tcPresolution cfg)

-- | Trace solve operations when enabled.
traceSolve :: TraceConfig -> String -> a -> a
traceSolve cfg = traceWhen (tcSolve cfg)

-- | Trace elaboration operations when enabled.
traceElab :: TraceConfig -> String -> a -> a
traceElab cfg = traceWhen (tcElab cfg)

-- | Trace normalization operations when enabled.
traceNormalize :: TraceConfig -> String -> a -> a
traceNormalize cfg = traceWhen (tcNormalize cfg)

-- | Trace generalization operations when enabled.
traceGeneralize :: TraceConfig -> String -> a -> a
traceGeneralize cfg = traceWhen (tcGeneralize cfg)

-- | Monadic binding trace.
traceBindingM :: Monad m => TraceConfig -> String -> m ()
traceBindingM cfg msg = when (tcBinding cfg) $ trace msg (pure ())

-- | Monadic presolution trace.
tracePresolutionM :: Monad m => TraceConfig -> String -> m ()
tracePresolutionM cfg msg = when (tcPresolution cfg) $ trace msg (pure ())

-- | Monadic solve trace.
traceSolveM :: Monad m => TraceConfig -> String -> m ()
traceSolveM cfg msg = when (tcSolve cfg) $ trace msg (pure ())

-- | Monadic elaboration trace.
traceElabM :: Monad m => TraceConfig -> String -> m ()
traceElabM cfg msg = when (tcElab cfg) $ trace msg (pure ())
