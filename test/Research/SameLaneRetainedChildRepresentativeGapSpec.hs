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

        it "sameLaneQuintupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneQuintupleAliasFrameClearBoundaryExpr))

        it "sameLaneQuintupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneQuintupleAliasFrameClearBoundaryExpr))

        it "sameLaneSextupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneSextupleAliasFrameClearBoundaryExpr))

        it "sameLaneSextupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneSextupleAliasFrameClearBoundaryExpr))

        it "sameLaneSeptupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneSeptupleAliasFrameClearBoundaryExpr))

        it "sameLaneSeptupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneSeptupleAliasFrameClearBoundaryExpr))

        it "sameLaneOctupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneOctupleAliasFrameClearBoundaryExpr))

        it "sameLaneOctupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneOctupleAliasFrameClearBoundaryExpr))

        it "sameLaneNonupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneNonupleAliasFrameClearBoundaryExpr))

        it "sameLaneNonupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneNonupleAliasFrameClearBoundaryExpr))

        it "sameLaneDecupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneDecupleAliasFrameClearBoundaryExpr))

        it "sameLaneDecupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneDecupleAliasFrameClearBoundaryExpr))

        it "sameWrapperNestedForallAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallAliasFrameClearBoundaryExpr))

        it "sameWrapperNestedForallAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallAliasFrameClearBoundaryExpr))

        it "sameWrapperNestedForallDecupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallDecupleAliasFrameClearBoundaryExpr))

        it "sameWrapperNestedForallDecupleAliasFrameClearBoundaryExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallDecupleAliasFrameClearBoundaryExpr))

        it "sameWrapperNestedForallTransparentMediatorExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallTransparentMediatorExpr))

        it "sameWrapperNestedForallTransparentMediatorExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallTransparentMediatorExpr))

        it "sameWrapperNestedForallTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallTransparentMediatorDecupleAliasExpr))

        it "sameWrapperNestedForallTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallTransparentMediatorDecupleAliasExpr))

        it "sameWrapperNestedForallStackedTransparentMediatorExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallStackedTransparentMediatorExpr))

        it "sameWrapperNestedForallStackedTransparentMediatorExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallStackedTransparentMediatorExpr))

        it "sameWrapperNestedForallStackedTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallStackedTransparentMediatorDecupleAliasExpr))

        it "sameWrapperNestedForallStackedTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallStackedTransparentMediatorDecupleAliasExpr))

        it "sameWrapperNestedForallStackedLetAliasedTransparentMediatorExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallStackedLetAliasedTransparentMediatorExpr))

        it "sameWrapperNestedForallStackedLetAliasedTransparentMediatorExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallStackedLetAliasedTransparentMediatorExpr))

        it "sameWrapperNestedForallStackedLetAliasedTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallStackedLetAliasedTransparentMediatorDecupleAliasExpr))

        it "sameWrapperNestedForallStackedLetAliasedTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallStackedLetAliasedTransparentMediatorDecupleAliasExpr))

        it "sameWrapperNestedForallMixedStackedTransparentMediatorExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallMixedStackedTransparentMediatorExpr))

        it "sameWrapperNestedForallMixedStackedTransparentMediatorExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallMixedStackedTransparentMediatorExpr))

        it "sameWrapperNestedForallMixedStackedTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallMixedStackedTransparentMediatorDecupleAliasExpr))

        it "sameWrapperNestedForallMixedStackedTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallMixedStackedTransparentMediatorDecupleAliasExpr))

        it "sameWrapperNestedForallReverseMixedStackedTransparentMediatorExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallReverseMixedStackedTransparentMediatorExpr))

        it "sameWrapperNestedForallReverseMixedStackedTransparentMediatorExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallReverseMixedStackedTransparentMediatorExpr))

        it "sameWrapperNestedForallReverseMixedStackedTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElab" $
            expectExactRetainedChildAuthoritativeOutput
                "unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr sameWrapperNestedForallReverseMixedStackedTransparentMediatorDecupleAliasExpr))

        it "sameWrapperNestedForallReverseMixedStackedTransparentMediatorDecupleAliasExpr preserves recursive output on runPipelineElabChecked" $
            expectExactRetainedChildAuthoritativeOutput
                "checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameWrapperNestedForallReverseMixedStackedTransparentMediatorDecupleAliasExpr))


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

sameLaneQuintupleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneQuintupleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "more" (EVar "keep")
                    (ELet "deep" (EVar "more")
                        (ELet "tail" (EVar "deep")
                            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tail")) (EVar "u")))))))

sameLaneSextupleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneSextupleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "more" (EVar "keep")
                    (ELet "deep" (EVar "more")
                        (ELet "tail" (EVar "deep")
                            (ELet "leaf" (EVar "tail")
                                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "leaf")) (EVar "u"))))))))

sameLaneSeptupleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneSeptupleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "more" (EVar "keep")
                    (ELet "deep" (EVar "more")
                        (ELet "tail" (EVar "deep")
                            (ELet "leaf" (EVar "tail")
                                (ELet "tip" (EVar "leaf")
                                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tip")) (EVar "u")))))))))

sameLaneOctupleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneOctupleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "more" (EVar "keep")
                    (ELet "deep" (EVar "more")
                        (ELet "tail" (EVar "deep")
                            (ELet "leaf" (EVar "tail")
                                (ELet "tip" (EVar "leaf")
                                    (ELet "bud" (EVar "tip")
                                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "bud")) (EVar "u"))))))))))

sameLaneNonupleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneNonupleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "more" (EVar "keep")
                    (ELet "deep" (EVar "more")
                        (ELet "tail" (EVar "deep")
                            (ELet "leaf" (EVar "tail")
                                (ELet "tip" (EVar "leaf")
                                    (ELet "bud" (EVar "tip")
                                        (ELet "seed" (EVar "bud")
                                            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "seed")) (EVar "u")))))))))))

sameLaneDecupleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneDecupleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "more" (EVar "keep")
                    (ELet "deep" (EVar "more")
                        (ELet "tail" (EVar "deep")
                            (ELet "leaf" (EVar "tail")
                                (ELet "tip" (EVar "leaf")
                                    (ELet "bud" (EVar "tip")
                                        (ELet "seed" (EVar "bud")
                                            (ELet "grain" (EVar "seed")
                                                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "grain")) (EVar "u"))))))))))))

sameWrapperNestedForallAliasFrameClearBoundaryExpr :: SurfaceExpr
sameWrapperNestedForallAliasFrameClearBoundaryExpr =
    sameWrapperNestedForallAliasChainExpr ["hold"]

sameWrapperNestedForallDecupleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameWrapperNestedForallDecupleAliasFrameClearBoundaryExpr =
    sameWrapperNestedForallAliasChainExpr
        ["hold", "keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain"]

sameWrapperNestedForallAliasChainExpr :: [String] -> SurfaceExpr
sameWrapperNestedForallAliasChainExpr aliases =
    ELet "id" (ELam "z" (EVar "z"))
        (ELet "k" (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
            (sameWrapperAliasChain aliases "k"))

sameWrapperAliasChain :: [String] -> String -> SurfaceExpr
sameWrapperAliasChain aliases source =
    case aliases of
        [] ->
            ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
        aliasName : rest ->
            ELet aliasName (EVar source) (sameWrapperAliasChain rest aliasName)

sameWrapperNestedForallTransparentMediatorExpr :: SurfaceExpr
sameWrapperNestedForallTransparentMediatorExpr =
    ELet "id" (ELam "z" (EVar "z"))
        ( ELet
            "wrap"
            (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
            ( ELet
                "k"
                (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                ( ELet
                    "hold"
                    (EApp (EVar "wrap") (EVar "k"))
                    (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                )
            )
        )

sameWrapperNestedForallTransparentMediatorDecupleAliasExpr :: SurfaceExpr
sameWrapperNestedForallTransparentMediatorDecupleAliasExpr =
    ELet "id" (ELam "z" (EVar "z"))
        ( ELet
            "wrap"
            (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
            ( ELet
                "k"
                (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                ( ELet
                    "hold"
                    (EApp (EVar "wrap") (EVar "k"))
                    ( sameWrapperAliasChain
                        ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                        "hold"
                    )
                )
            )
        )

sameWrapperNestedForallStackedMediatorExpr :: SurfaceExpr -> SurfaceExpr -> SurfaceExpr -> SurfaceExpr
sameWrapperNestedForallStackedMediatorExpr wrap1 wrap2 finalExpr =
    ELet "id" (ELam "z" (EVar "z"))
        ( ELet
            "wrap1"
            wrap1
            ( ELet
                "wrap2"
                wrap2
                ( ELet
                    "k"
                    (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                    ( ELet
                        "hold"
                        (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "k")))
                        finalExpr
                    )
                )
            )
        )

sameWrapperNestedForallStackedTransparentMediatorExpr :: SurfaceExpr
sameWrapperNestedForallStackedTransparentMediatorExpr =
    sameWrapperNestedForallStackedMediatorExpr
        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
        (EApp (ELam "y" (EVar "y")) (EVar "hold"))

sameWrapperNestedForallStackedTransparentMediatorDecupleAliasExpr :: SurfaceExpr
sameWrapperNestedForallStackedTransparentMediatorDecupleAliasExpr =
    sameWrapperNestedForallStackedMediatorExpr
        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
        (sameWrapperAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")

sameWrapperNestedForallLetAliasedTransparentMediatorWrap :: SurfaceExpr
sameWrapperNestedForallLetAliasedTransparentMediatorWrap =
    ELam
        "h"
        (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))

sameWrapperNestedForallStackedLetAliasedTransparentMediatorExpr :: SurfaceExpr
sameWrapperNestedForallStackedLetAliasedTransparentMediatorExpr =
    sameWrapperNestedForallStackedMediatorExpr
        sameWrapperNestedForallLetAliasedTransparentMediatorWrap
        sameWrapperNestedForallLetAliasedTransparentMediatorWrap
        (EApp (ELam "y" (EVar "y")) (EVar "hold"))

sameWrapperNestedForallStackedLetAliasedTransparentMediatorDecupleAliasExpr :: SurfaceExpr
sameWrapperNestedForallStackedLetAliasedTransparentMediatorDecupleAliasExpr =
    sameWrapperNestedForallStackedMediatorExpr
        sameWrapperNestedForallLetAliasedTransparentMediatorWrap
        sameWrapperNestedForallLetAliasedTransparentMediatorWrap
        (sameWrapperAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")

sameWrapperNestedForallMixedStackedTransparentMediatorExpr :: SurfaceExpr
sameWrapperNestedForallMixedStackedTransparentMediatorExpr =
    sameWrapperNestedForallStackedMediatorExpr
        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
        sameWrapperNestedForallLetAliasedTransparentMediatorWrap
        (EApp (ELam "y" (EVar "y")) (EVar "hold"))

sameWrapperNestedForallMixedStackedTransparentMediatorDecupleAliasExpr :: SurfaceExpr
sameWrapperNestedForallMixedStackedTransparentMediatorDecupleAliasExpr =
    sameWrapperNestedForallStackedMediatorExpr
        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
        sameWrapperNestedForallLetAliasedTransparentMediatorWrap
        (sameWrapperAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")

sameWrapperNestedForallReverseMixedStackedTransparentMediatorExpr :: SurfaceExpr
sameWrapperNestedForallReverseMixedStackedTransparentMediatorExpr =
    sameWrapperNestedForallStackedMediatorExpr
        sameWrapperNestedForallLetAliasedTransparentMediatorWrap
        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
        (EApp (ELam "y" (EVar "y")) (EVar "hold"))

sameWrapperNestedForallReverseMixedStackedTransparentMediatorDecupleAliasExpr :: SurfaceExpr
sameWrapperNestedForallReverseMixedStackedTransparentMediatorDecupleAliasExpr =
    sameWrapperNestedForallStackedMediatorExpr
        sameWrapperNestedForallLetAliasedTransparentMediatorWrap
        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
        (sameWrapperAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")

recursiveAnn :: SrcType
recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
