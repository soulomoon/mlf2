module MLF.Util.Timing
    ( TimingConfig (..)
    , defaultTimingConfig
    , timingConfigFromEnv
    , timeProgramIO
    , timeProgramDetailIO
    , timeProgramOperationIO
    , timeProgramOperationWithSuffixIO
    , measureProgramOperationIO
    , emitProgramOperationDurationIO
    , emitProgramOperationMetricIO
    , whenProgramOperationsIO
    ) where

import Control.Exception (onException)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

data TimingConfig = TimingConfig
    { timingProgram :: Bool
    , timingProgramDetail :: Bool
    , timingProgramOperations :: Bool
    , timingProgramDefDetails :: Bool
    }
    deriving (Eq, Show)

defaultTimingConfig :: TimingConfig
defaultTimingConfig =
    TimingConfig
        { timingProgram = False
        , timingProgramDetail = False
        , timingProgramOperations = False
        , timingProgramDefDetails = False
        }

timingConfigFromEnv :: IO TimingConfig
timingConfigFromEnv = do
    programTiming <- isEnvSet "MLF_PROGRAM_TIMING"
    detailTiming <- isEnvSet "MLF_PROGRAM_TIMING_DETAIL"
    operationTiming <- isEnvSet "MLF_PROGRAM_TIMING_OPERATIONS"
    defDetailTiming <- isEnvSet "MLF_PROGRAM_TIMING_DEF_DETAILS"
    pure
        TimingConfig
            { timingProgram = programTiming || detailTiming || operationTiming || defDetailTiming
            , timingProgramDetail = detailTiming || operationTiming || defDetailTiming
            , timingProgramOperations = operationTiming
            , timingProgramDefDetails = defDetailTiming
            }

timeProgramIO :: TimingConfig -> String -> IO a -> IO a
timeProgramIO config label action
    | not (timingProgram config) = action
    | otherwise = do
        start <- getMonotonicTimeNSec
        hPutStrLn stderr ("[MLF_PROGRAM_TIMING] start " ++ label)
        result <-
            action
                `onException` do
                    end <- getMonotonicTimeNSec
                    hPutStrLn stderr ("[MLF_PROGRAM_TIMING] fail  " ++ label ++ " " ++ formatMillis (end - start))
        end <- getMonotonicTimeNSec
        hPutStrLn stderr ("[MLF_PROGRAM_TIMING] done  " ++ label ++ " " ++ formatMillis (end - start))
        pure result

timeProgramDetailIO :: TimingConfig -> String -> IO a -> IO a
timeProgramDetailIO config label action
    | not (timingProgramDetail config) = action
    | otherwise = timeProgramIO config label action

timeProgramOperationIO :: TimingConfig -> String -> IO a -> IO a
timeProgramOperationIO config label action
    | not (timingProgramOperations config) = action
    | otherwise = timeProgramIO config label action

timeProgramOperationWithSuffixIO :: TimingConfig -> String -> String -> IO a -> IO a
timeProgramOperationWithSuffixIO config label suffix action
    | not (timingProgramOperations config) = action
    | otherwise = timeProgramIO config (label ++ suffix) action

whenProgramOperationsIO :: TimingConfig -> IO () -> IO ()
whenProgramOperationsIO config action
    | not (timingProgramOperations config) = pure ()
    | otherwise = action

measureProgramOperationIO :: TimingConfig -> IO a -> IO (a, Word64)
measureProgramOperationIO config action
    | not (timingProgramOperations config) = do
        result <- action
        pure (result, 0)
    | otherwise = do
        start <- getMonotonicTimeNSec
        result <- action
        end <- getMonotonicTimeNSec
        pure (result, end - start)

emitProgramOperationDurationIO :: TimingConfig -> String -> Word64 -> IO ()
emitProgramOperationDurationIO config label elapsed
    | not (timingProgramOperations config) = pure ()
    | otherwise =
        hPutStrLn stderr ("[MLF_PROGRAM_TIMING] done  " ++ label ++ " " ++ formatMillis elapsed)

emitProgramOperationMetricIO :: TimingConfig -> String -> Word64 -> IO ()
emitProgramOperationMetricIO config label value
    | not (timingProgramOperations config) = pure ()
    | otherwise =
        hPutStrLn stderr ("[MLF_PROGRAM_TIMING] metric " ++ label ++ " " ++ show value)

isEnvSet :: String -> IO Bool
isEnvSet name = do
    value <- lookupEnv name
    pure $ case value of
        Nothing -> False
        Just "" -> False
        Just "0" -> False
        Just "false" -> False
        Just "False" -> False
        Just _ -> True

formatMillis :: Word64 -> String
formatMillis ns =
    show wholeMs ++ "." ++ pad3 fractionalMicros ++ "ms"
  where
    wholeMs = ns `div` 1000000
    fractionalMicros = (ns `div` 1000) `mod` 1000

pad3 :: Word64 -> String
pad3 value =
    replicate (3 - length digits) '0' ++ digits
  where
    digits = show value
