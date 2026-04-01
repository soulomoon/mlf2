module Research.SameLaneRetainedChildRepresentativeGapSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.Elab.Pipeline
    ( runPipelineElab
    , runPipelineElabChecked
    )
import MLF.Frontend.Syntax
import MLF.Types.Elab (ElabTerm, ElabType, Ty(..))
import SpecUtil (unsafeNormalizeExpr)

spec :: Spec
spec =
    describe "same-lane retained-child representative-gap probes" $ do
        it "sameLaneAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectRecursivePipelineSuccess
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneAliasFrameClearBoundaryExpr))

        it "sameLaneAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectRecursivePipelineSuccess
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneAliasFrameClearBoundaryExpr))

expectRecursivePipelineSuccess
    :: Show err
    => String
    -> Either err (ElabTerm, ElabType)
    -> Expectation
expectRecursivePipelineSuccess label result =
    case result of
        Left err ->
            expectationFailure (label ++ ": expected recursive success, got " ++ show err)
        Right (_term, ty) ->
            containsMu ty `shouldBe` True

containsMu :: ElabType -> Bool
containsMu ty = case ty of
    TMu _ _ -> True
    TArrow dom cod -> containsMu dom || containsMu cod
    TCon _ args -> any containsMu args
    TForall _ mb body -> maybe False containsMuBound mb || containsMu body
    _ -> False
  where
    containsMuBound bound = case bound of
        TArrow dom cod -> containsMu dom || containsMu cod
        TBase _ -> False
        TCon _ args -> any containsMu args
        TForall _ mb body -> maybe False containsMuBound mb || containsMu body
        TMu _ _ -> True
        TBottom -> False

sameLaneAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))

recursiveAnn :: SrcType
recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
