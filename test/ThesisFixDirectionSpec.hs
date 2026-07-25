module ThesisFixDirectionSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.API
import MLF.Pipeline

bugExpr :: NormSurfaceExpr
bugExpr =
    ELet "make" (ELam "x" (ELam "y" (EVar "x")))
        (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
            (EApp (EVar "c1") (ELit (LBool True))))

assertPipelineInt
    :: String
    -> (PolySyms -> NormSurfaceExpr -> Either PipelineError (XmlfTerm, ElabType))
    -> Expectation
assertPipelineInt label runPipeline =
    case runPipeline Set.empty bugExpr of
        Left err ->
            expectationFailure (label ++ " unexpectedly failed: " ++ renderPipelineError err)
        Right (term, ty) -> do
            case ty of
                TBaseWithIdentity _ (BaseTy "Int") -> pure ()
                other -> expectationFailure (label ++ " returned non-Int type: " ++ show other)
            typeCheck term `shouldBe` Right ty

spec :: Spec
spec = describe "BUG-2026-02-06-002 thesis target" $ do
    it "canonical pipeline typechecks to Int without fallback recovery" $
        assertPipelineInt "canonical pipeline" runPipelineElab
