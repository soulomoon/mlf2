module Main where

import System.Environment (getArgs)
import System.Exit (die)

import MLF.Program.CLI
    ( programCliUsage
    , runProgramFile
    )
import MLF.Research.URI.R2.C1.Prototype.Entrypoint
    ( AppRun(..)
    , runAppFromCurrentDirectory
    )
import MLF.Research.URI.R2.C1.Prototype.Types (prototypeStageResult)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["run-program", path] ->
            runProgramFile path >>= either die putStrLn
        ["run-program"] ->
            die programCliUsage
        ["--help"] ->
            putStrLn programCliUsage
        _ ->
            runAppFromCurrentDirectory args >>= \result ->
                case result of
                    Left err -> die err
                    Right (AppDefaultDemo output) -> putStrLn output
                    Right (AppPrototype report) ->
                        putStrLn ("Prototype result: " ++ prototypeStageResult report)
