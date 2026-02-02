module Main where

import MLF.API
    ( Expr(..)
    , Pretty(..)
    , defaultPipelineConfig
    , renderPipelineError
    , runPipelineElabWithConfig
    )

main :: IO ()
main = do
  let expr = ELam "x" (EVar "x")
  case runPipelineElabWithConfig defaultPipelineConfig mempty expr of
    Left err ->
      putStrLn ("Pipeline failed: " ++ renderPipelineError err)
    Right (_term, ty) ->
      putStrLn ("Type: " ++ pretty ty)
