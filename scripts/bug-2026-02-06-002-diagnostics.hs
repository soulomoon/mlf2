module Main (main) where

import qualified Data.Set as Set
import Data.List (isInfixOf)
import System.Environment (getArgs)

import MLF.API

bugExpr :: NormSurfaceExpr
bugExpr =
    ELet "make" (ELam "x" (ELam "y" (EVar "x")))
        (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
            (EApp (EVar "c1") (ELit (LBool True))))

mkConfig :: Bool -> PipelineConfig
mkConfig traceEnabled =
    defaultPipelineConfig
        { pcTraceConfig =
            defaultTraceConfig
                { tcBinding = traceEnabled
                , tcPresolution = traceEnabled
                , tcSolve = traceEnabled
                , tcElab = traceEnabled
                , tcNormalize = traceEnabled
                , tcGeneralize = traceEnabled
                }
        }

emitResult :: String -> Either PipelineError (ElabTerm, ElabType) -> IO ()
emitResult label result =
    case result of
        Right (_tm, ty) ->
            putStrLn ("DIAG|" ++ label ++ "|status=OK|type=" ++ show ty)
        Left err -> do
            let rendered = renderPipelineError err
            putStrLn ("DIAG|" ++ label ++ "|status=FAIL|" ++ rendered)
            putStrLn ("DIAG|" ++ label ++ "|phase-summary|" ++ rendered)
            if "TCLetTypeMismatch" `isInfixOf` rendered
                then putStrLn ("DIAG|" ++ label ++ "|mismatch=TCLetTypeMismatch")
                else pure ()

main :: IO ()
main = do
    args <- getArgs
    let traceEnabled = "--trace" `elem` args
        cfg = mkConfig traceEnabled

    putStrLn ("DIAG|mode=" ++ if traceEnabled then "trace" else "repro")
    emitResult "unchecked" (runPipelineElabWithConfig cfg Set.empty bugExpr)
    emitResult "checked" (runPipelineElabCheckedWithConfig cfg Set.empty bugExpr)
