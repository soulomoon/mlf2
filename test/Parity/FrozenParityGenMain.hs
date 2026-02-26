module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)

import Parity.FrozenArtifacts
    ( baselineFilePath
    , buildFrozenBaseline
    , mkMetadata
    , renderFrozenBaselineJson
    )

data GenConfig = GenConfig
    { gcOutput :: FilePath
    , gcGeneratedOn :: String
    , gcSourceCommit :: String
    }

main :: IO ()
main = do
    args <- getArgs
    config <-
        case parseArgs args defaultConfig of
            Left err -> die err
            Right out -> pure out
    let metadata = mkMetadata (gcGeneratedOn config) (gcSourceCommit config)
    baseline <-
        case buildFrozenBaseline metadata of
            Left err -> die err
            Right out -> pure out
    writeFile (gcOutput config) (renderFrozenBaselineJson baseline)
    putStrLn ("Wrote frozen parity baseline to " ++ gcOutput config)

parseArgs :: [String] -> GenConfig -> Either String GenConfig
parseArgs args initial = go args initial
  where
    go remaining cfg =
        case remaining of
            [] -> Right cfg
            "--output" : path : rest -> go rest cfg { gcOutput = path }
            "--generated-on" : date : rest -> go rest cfg { gcGeneratedOn = date }
            "--source-commit" : sha : rest -> go rest cfg { gcSourceCommit = sha }
            _ -> Left usage

defaultConfig :: GenConfig
defaultConfig =
    GenConfig
        { gcOutput = baselineFilePath
        , gcGeneratedOn = "UNKNOWN"
        , gcSourceCommit = "UNKNOWN"
        }

usage :: String
usage =
    unlines
        [ "Usage: cabal run frozen-parity-gen -- [OPTIONS]"
        , ""
        , "Options:"
        , "  --output PATH          Output JSON path"
        , "  --generated-on DATE    Metadata generation date (YYYY-MM-DD)"
        , "  --source-commit SHA    Metadata source commit"
        ]
