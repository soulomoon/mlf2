module DualPathSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import qualified Data.Set as Set

import MLF.Frontend.Syntax (Expr(..), SrcTy(..))
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabProjectionFirst)
import SpecUtil (unsafeNormalizeExpr)

spec :: Spec
spec = describe "Dual-path verification" $ do
    describe "E1: canonical-heavy vs projection-first equivalence" $ do
        let corpus =
                [ ("id", ELam "x" (EVar "x"))
                , ("const", ELam "x" (ELam "y" (EVar "x")))
                , ("app-id", EApp (ELam "x" (EVar "x")) (ELam "y" (EVar "y")))
                , ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                , ("ann-id", EAnn (ELam "x" (EVar "x"))
                    (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                , ("nested-let"
                  , ELet "f" (ELam "x" (EVar "x"))
                        (ELet "g" (EVar "f")
                            (EApp (EVar "g") (EVar "g"))))
                , ("higher-rank-app"
                  , EApp
                        (ELam "f" (EVar "f"))
                        (ELam "x" (EVar "x")))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("canonical-heavy and projection-first agree for: " ++ label) $ do
                let norm = unsafeNormalizeExpr expr
                    rCanonical = runPipelineElab Set.empty norm
                    rProjection = runPipelineElabProjectionFirst Set.empty norm
                case (rCanonical, rProjection) of
                    (Right (t1, ty1), Right (t2, ty2)) -> do
                        show t1 `shouldBe` show t2
                        show ty1 `shouldBe` show ty2
                    (Left e1, Left e2) ->
                        show e1 `shouldBe` show e2
                    _ -> expectationFailure $
                        "Path disagreement for " ++ label
                        ++ ": canonical=" ++ show rCanonical
                        ++ ", projection=" ++ show rProjection

    describe "E2: projection-first validation passes" $ do
        let corpus =
                [ ("id", ELam "x" (EVar "x"))
                , ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                , ("ann-id", EAnn (ELam "x" (EVar "x"))
                    (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("original/canonical agreement holds for: " ++ label) $ do
                let norm = unsafeNormalizeExpr expr
                    result = runPipelineElabProjectionFirst Set.empty norm
                case result of
                    Left err -> expectationFailure (show err)
                    Right _ -> pure ()
