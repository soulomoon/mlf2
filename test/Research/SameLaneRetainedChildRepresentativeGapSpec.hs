module Research.SameLaneRetainedChildRepresentativeGapSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Set as Set
import Test.Hspec

import MLF.Elab.Pipeline
    ( runPipelineElab
    , runPipelineElabChecked
    )
import MLF.Frontend.Syntax
import MLF.Types.Elab (ElabTerm, ElabType)
import SpecUtil (unsafeNormalizeExpr)

spec :: Spec
spec =
    describe "same-lane retained-child representative-gap probes" $ do
        it "sameLaneAliasFrameClearBoundaryExpr surfaces a narrower current-architecture blocker on runPipelineElab" $
            expectCurrentArchitectureBlocker
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneAliasFrameClearBoundaryExpr))

        it "sameLaneAliasFrameClearBoundaryExpr surfaces a narrower current-architecture blocker on runPipelineElabChecked" $
            expectCurrentArchitectureBlocker
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneAliasFrameClearBoundaryExpr))

expectCurrentArchitectureBlocker
    :: Show err
    => String
    -> Either err (ElabTerm, ElabType)
    -> Expectation
expectCurrentArchitectureBlocker label result =
    case result of
        Left err -> do
            let rendered = show err
            rendered `shouldSatisfy` isInfixOf "PhiTranslatabilityError"
            rendered `shouldSatisfy` isInfixOf "missing authoritative instantiation translation"
            rendered `shouldSatisfy` isInfixOf "expansion args="
        Right (term, ty) ->
            expectationFailure
                ( label
                    ++ ": expected narrower current-architecture blocker, got "
                    ++ show term
                    ++ " :: "
                    ++ show ty
                )

sameLaneAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))

recursiveAnn :: SrcType
recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
