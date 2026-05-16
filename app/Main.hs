module Main where

import System.Environment (getArgs)
import System.Exit (die)

import MLF.API (Expr(..), normalizeExpr)
import MLF.Pipeline
    ( Pretty(..)
    , defaultPipelineConfig
    , renderPipelineError
    , runPipelineElabWithConfig
    )
import MLF.Program.CLI
    ( checkProgramArgs
    , emitBackendArgs
    , emitNativeArgs
    , programCliUsage
    , runProgramArgs
    )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->
            runDefaultDemo >>= putStrLn
        "check-program" : commandArgs ->
            checkProgramArgs commandArgs >>= either die putStr
        "run-program" : commandArgs ->
            runProgramArgs commandArgs >>= either die putStr
        "emit-backend" : commandArgs ->
            emitBackendArgs commandArgs >>= either die putStr
        "emit-native" : commandArgs ->
            emitNativeArgs commandArgs >>= either die putStr
        ["--help"] ->
            putStrLn programCliUsage
        "--research-entrypoint" : _ ->
            die "Research entrypoints have been retired; historical evidence remains under docs/plans and orchestrator/rounds."
        _ ->
            die programCliUsage

runDefaultDemo :: IO String
runDefaultDemo = do
    let expr = ELam "x" (EVar "x")
    case normalizeExpr expr of
        Left err ->
            pure ("Normalization failed: " ++ show err)
        Right normExpr ->
            case runPipelineElabWithConfig defaultPipelineConfig mempty normExpr of
                Left err ->
                    pure ("Pipeline failed: " ++ renderPipelineError err)
                Right (_term, ty) ->
                    pure ("Type: " ++ pretty ty)
