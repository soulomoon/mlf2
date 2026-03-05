module ThesisFixDirectionSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.API

bugExpr :: NormSurfaceExpr
bugExpr =
    ELet "make" (ELam "x" (ELam "y" (EVar "x")))
        (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
            (EApp (EVar "c1") (ELit (LBool True))))

assertPipelineInt
    :: String
    -> (PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType))
    -> Expectation
assertPipelineInt label runPipeline =
    case runPipeline Set.empty bugExpr of
        Left err ->
            expectationFailure (label ++ " unexpectedly failed: " ++ renderPipelineError err)
        Right (_term, ty) ->
            ty `shouldBe` TBase (BaseTy "Int")

spec :: Spec
spec = describe "BUG-2026-02-06-002 thesis target" $ do
    it "unchecked pipeline typechecks to Int" $
        assertPipelineInt "unchecked pipeline" runPipelineElab

    it "checked pipeline typechecks to Int" $
        assertPipelineInt "checked pipeline" runPipelineElabChecked
