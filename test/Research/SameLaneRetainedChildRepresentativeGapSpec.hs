{-# LANGUAGE GADTs #-}

module Research.SameLaneRetainedChildRepresentativeGapSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..))
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
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneAliasFrameClearBoundaryExpr))

        it "sameLaneAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneAliasFrameClearBoundaryExpr))

        it "sameLaneDoubleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneDoubleAliasFrameClearBoundaryExpr))

        it "sameLaneDoubleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneDoubleAliasFrameClearBoundaryExpr))

        it "sameLaneTripleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneTripleAliasFrameClearBoundaryExpr))

        it "sameLaneTripleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneTripleAliasFrameClearBoundaryExpr))

        it "sameLaneQuadrupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneQuadrupleAliasFrameClearBoundaryExpr))

        it "sameLaneQuadrupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneQuadrupleAliasFrameClearBoundaryExpr))

expectExactRetainedChildAuthoritativeOutput
    :: Show err
    => String
    -> Either err (ElabTerm, ElabType)
    -> Expectation
expectExactRetainedChildAuthoritativeOutput label result =
    case result of
        Left err ->
            expectationFailure (label ++ ": expected recursive success, got " ++ show err)
        Right (_term, ty) -> do
            countLeadingUnboundedForalls ty `shouldBe` 2
            matchesRecursiveArrow (stripLeadingUnboundedForalls ty) expectedRecursiveArrow
                `shouldBe` True

countLeadingUnboundedForalls :: ElabType -> Int
countLeadingUnboundedForalls ty = case ty of
    TForall _ Nothing body -> 1 + countLeadingUnboundedForalls body
    _ -> 0

stripLeadingUnboundedForalls :: ElabType -> ElabType
stripLeadingUnboundedForalls ty = case ty of
    TForall _ Nothing body -> stripLeadingUnboundedForalls body
    _ -> ty

matchesRecursiveArrow :: ElabType -> ElabType -> Bool
matchesRecursiveArrow actual expected = case (actual, expected) of
    (TArrow domA codA, TArrow domE codE) ->
        matchesRecursiveMu domA domE && matchesRecursiveMu codA codE
    _ -> False
  where
    matchesRecursiveMu tyA tyE = case (tyA, tyE) of
        (TMu _ bodyA, TMu _ bodyE) -> stripMuNames bodyA == stripMuNames bodyE
        _ -> False

    stripMuNames ty = case ty of
        TVar _ -> TVar "_"
        TArrow dom cod -> TArrow (stripMuNames dom) (stripMuNames cod)
        TBase base -> TBase base
        TCon con args -> TCon con (fmap stripMuNames args)
        TForall _ mb body -> TForall "_" (fmap stripBoundNames mb) (stripMuNames body)
        TMu _ body -> TMu "_" (stripMuNames body)
        TBottom -> TBottom

    stripBoundNames bound = case bound of
        TArrow dom cod -> TArrow (stripMuNames dom) (stripMuNames cod)
        TBase base -> TBase base
        TCon con args -> TCon con (fmap stripMuNames args)
        TForall _ mb body -> TForall "_" (fmap stripBoundNames mb) (stripMuNames body)
        TMu _ body -> TMu "_" (stripMuNames body)
        TBottom -> TBottom

expectedRecursiveArrow :: ElabType
expectedRecursiveArrow =
    let recursiveTy = TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int")))
    in TArrow recursiveTy recursiveTy

sameLaneAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))

sameLaneDoubleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneDoubleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "keep")) (EVar "u"))))

sameLaneTripleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneTripleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "more" (EVar "keep")
                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "more")) (EVar "u")))))

sameLaneQuadrupleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneQuadrupleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "more" (EVar "keep")
                    (ELet "deep" (EVar "more")
                        (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "deep")) (EVar "u"))))))

recursiveAnn :: SrcType
recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
