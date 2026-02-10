module ThesisFixDirectionSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.API

bugExpr :: NormSurfaceExpr
bugExpr =
    ELet "make" (ELam "x" (ELam "y" (EVar "x")))
        (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
            (EApp (EVar "c1") (ELit (LBool True))))

expectedIntTy :: ElabType
expectedIntTy = TBase (BaseTy "Int")

assertPipelineType
    :: String
    -> (PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType))
    -> Expectation
assertPipelineType label runPipeline =
    case runPipeline Set.empty bugExpr of
        Left err -> expectationFailure (label ++ " failed: " ++ renderPipelineError err)
        Right (_tm, ty) -> ty `shouldBe` expectedIntTy

spec :: Spec
spec = describe "BUG-2026-02-06-002 thesis target" $ do
    it "unchecked pipeline returns Int" $
        assertPipelineType "unchecked pipeline" runPipelineElab

    it "checked pipeline returns Int" $
        assertPipelineType "checked pipeline" runPipelineElabChecked
