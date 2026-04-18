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

assertPipelineStrictFailure
    :: String
    -> (PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType))
    -> Expectation
assertPipelineStrictFailure label runPipeline =
    case runPipeline Set.empty bugExpr of
        Left err ->
            renderPipelineError err `shouldSatisfy`
                (\msg ->
                    "PhiTranslatabilityError" `elem` words msg
                        || "TCInstantiationError" `elem` words msg
                        || "TCLetTypeMismatch" `elem` words msg
                        || "TCExpectedArrow" `elem` words msg
                )
        Right (_term, ty) ->
            expectationFailure (label ++ " unexpectedly succeeded with type: " ++ show ty)

spec :: Spec
spec = describe "BUG-2026-02-06-002 thesis target" $ do
    it "unchecked pipeline now fails fast without fallback recovery" $
        assertPipelineStrictFailure "unchecked pipeline" runPipelineElab

    it "checked pipeline now fails fast without fallback recovery" $
        assertPipelineStrictFailure "checked pipeline" runPipelineElabChecked
