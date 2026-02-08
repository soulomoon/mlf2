module Main where

import MLF.API
    ( Expr(..)
    , Pretty(..)
    , defaultPipelineConfig
    , normalizeExpr
    , renderPipelineError
    , runPipelineElabWithConfig
    )

main :: IO ()
main = do
  let expr = ELam "x" (EVar "x")
  case normalizeExpr expr of
    Left err ->
      putStrLn ("Normalization failed: " ++ show err)
    Right normExpr ->
      case runPipelineElabWithConfig defaultPipelineConfig mempty normExpr of
        Left err ->
          putStrLn ("Pipeline failed: " ++ renderPipelineError err)
        Right (_term, ty) ->
          putStrLn ("Type: " ++ pretty ty)
