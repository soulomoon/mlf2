module ThesisFixDirectionSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Set as Set
import Test.Hspec

import MLF.API

bugExpr :: NormSurfaceExpr
bugExpr =
    ELet "make" (ELam "x" (ELam "y" (EVar "x")))
        (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
            (EApp (EVar "c1") (ELit (LBool True))))

assertPipelineFailFast
    :: String
    -> (PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType))
    -> Expectation
assertPipelineFailFast label runPipeline =
    case runPipeline Set.empty bugExpr of
        Left err ->
            renderPipelineError err
                `shouldSatisfy`
                    isStrictPhiFailFast
        Right _ ->
            expectationFailure (label ++ " unexpectedly succeeded")
  where
    isStrictPhiFailFast msg =
        ("OpWeaken: unresolved non-root binder target" `isInfixOf` msg)
            || ("trace binder replay-map target outside replay binder domain" `isInfixOf` msg)

spec :: Spec
spec = describe "BUG-2026-02-06-002 thesis target" $ do
    it "unchecked pipeline now fails fast on unresolved non-root OpWeaken" $
        assertPipelineFailFast "unchecked pipeline" runPipelineElab

    it "checked pipeline now fails fast on unresolved non-root OpWeaken" $
        assertPipelineFailFast "checked pipeline" runPipelineElabChecked
