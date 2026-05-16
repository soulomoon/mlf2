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
    ( emitBackendFile
    , emitNativeFile
    , programCliUsage
    , runProgramFile
    )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->
            runDefaultDemo >>= putStrLn
        ["run-program", path] ->
            runProgramFile path >>= either die putStr
        ["run-program"] ->
            die programCliUsage
        ["emit-backend", path] ->
            emitBackendFile path >>= either die putStr
        ["emit-backend"] ->
            die programCliUsage
        ["emit-native", path] ->
            emitNativeFile path >>= either die putStr
        ["emit-native"] ->
            die programCliUsage
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
