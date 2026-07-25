{-# LANGUAGE GADTs #-}

module Research.SameLaneRetainedChildRepresentativeGapSpec (spec) where

import Control.Monad (forM_, unless)
import qualified ElabTypeTestSupport as TestElab
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Elab.Pipeline
    ( runPipelineElab
    )
import MLF.Frontend.Syntax
import MLF.Types.Elab
    ( ElabType
    , Ty(..)
    , XmlfTerm
    , tyToElab
    , typeBinderRefsSameIdentity
    )
import ElabTermTestSupport (testTForall, testTMu, testTVar, testTVarApp)
import SpecUtil (unsafeNormalizeExpr)

spec :: Spec
spec =
    describe "same-lane retained-child representative-gap probes" $ do
        forM_ retainedChildCases $ \(label, expr) ->
            it (label ++ " preserves recursive output on runPipelineElab") $
                expectExactRetainedChildAuthoritativeOutput
                    label
                    (runPipelineElab Set.empty (unsafeNormalizeExpr expr))

retainedChildCases :: [(String, SurfaceExpr)]
retainedChildCases =
    [ ("sameLaneAliasFrameClearBoundaryExpr", sameLaneAliasFrameClearBoundaryExpr)
    , ("sameLaneDoubleAliasFrameClearBoundaryExpr", sameLaneDoubleAliasFrameClearBoundaryExpr)
    , ("sameLaneTripleAliasFrameClearBoundaryExpr", sameLaneTripleAliasFrameClearBoundaryExpr)
    , ("sameLaneQuadrupleAliasFrameClearBoundaryExpr", sameLaneQuadrupleAliasFrameClearBoundaryExpr)
    , ("sameLaneQuintupleAliasFrameClearBoundaryExpr", sameLaneQuintupleAliasFrameClearBoundaryExpr)
    , ("sameLaneSextupleAliasFrameClearBoundaryExpr", sameLaneSextupleAliasFrameClearBoundaryExpr)
    , ("sameLaneSeptupleAliasFrameClearBoundaryExpr", sameLaneSeptupleAliasFrameClearBoundaryExpr)
    , ("sameLaneOctupleAliasFrameClearBoundaryExpr", sameLaneOctupleAliasFrameClearBoundaryExpr)
    , ("sameLaneNonupleAliasFrameClearBoundaryExpr", sameLaneNonupleAliasFrameClearBoundaryExpr)
    , ("sameLaneDecupleAliasFrameClearBoundaryExpr", sameLaneDecupleAliasFrameClearBoundaryExpr)
    , ("sameWrapperNestedForallAliasFrameClearBoundaryExpr", sameWrapperNestedForallAliasFrameClearBoundaryExpr)
    , ("sameWrapperNestedForallDecupleAliasFrameClearBoundaryExpr", sameWrapperNestedForallDecupleAliasFrameClearBoundaryExpr)
    , ("sameWrapperNestedForallTransparentMediatorExpr", sameWrapperNestedForallTransparentMediatorExpr)
    , ("sameWrapperNestedForallTransparentMediatorDecupleAliasExpr", sameWrapperNestedForallTransparentMediatorDecupleAliasExpr)
    , ("sameWrapperNestedForallStackedTransparentMediatorExpr", sameWrapperNestedForallStackedTransparentMediatorExpr)
    , ("sameWrapperNestedForallStackedTransparentMediatorDecupleAliasExpr", sameWrapperNestedForallStackedTransparentMediatorDecupleAliasExpr)
    , ("sameWrapperNestedForallStackedLetAliasedTransparentMediatorExpr", sameWrapperNestedForallStackedLetAliasedTransparentMediatorExpr)
    , ("sameWrapperNestedForallStackedLetAliasedTransparentMediatorDecupleAliasExpr", sameWrapperNestedForallStackedLetAliasedTransparentMediatorDecupleAliasExpr)
    , ("sameWrapperNestedForallMixedStackedTransparentMediatorExpr", sameWrapperNestedForallMixedStackedTransparentMediatorExpr)
    , ("sameWrapperNestedForallMixedStackedTransparentMediatorDecupleAliasExpr", sameWrapperNestedForallMixedStackedTransparentMediatorDecupleAliasExpr)
    , ("sameWrapperNestedForallReverseMixedStackedTransparentMediatorExpr", sameWrapperNestedForallReverseMixedStackedTransparentMediatorExpr)
    , ("sameWrapperNestedForallReverseMixedStackedTransparentMediatorDecupleAliasExpr", sameWrapperNestedForallReverseMixedStackedTransparentMediatorDecupleAliasExpr)
    ]

expectExactRetainedChildAuthoritativeOutput
    :: Show err
    => String
    -> Either err (XmlfTerm, ElabType)
    -> Expectation
expectExactRetainedChildAuthoritativeOutput label result =
    case result of
        Left err ->
            expectationFailure (label ++ ": expected recursive success, got " ++ show err)
        Right (_term, ty) -> do
            unless
                (matchesRecursiveArrow (stripLeadingUnboundedForalls ty) expectedRecursiveArrow)
                ( expectationFailure
                    (label ++ ": expected " ++ show expectedRecursiveArrow ++ ", got " ++ show ty)
                )

stripLeadingUnboundedForalls :: ElabType -> ElabType
stripLeadingUnboundedForalls ty = case ty of
    TForallRef _ Nothing body -> stripLeadingUnboundedForalls body
    _ -> ty

matchesRecursiveArrow :: ElabType -> ElabType -> Bool
matchesRecursiveArrow actual expected = case (actual, expected) of
    (TArrow domA codA, TArrow domE codE) ->
        matchesRecursiveMu domA domE && matchesRecursiveMu codA codE
    ( TForallRef resultRef (Just resultBound) (TArrow domA (TVarRef resultUseRef))
      , TArrow domE codE
      ) ->
        typeBinderRefsSameIdentity resultRef resultUseRef
            && matchesRecursiveMu domA domE
            && matchesRecursiveMu (tyToElab resultBound) codE
    _ -> False
  where
    matchesRecursiveMu tyA tyE = case (tyA, tyE) of
        (TMuRef _ bodyA, TMuRef _ bodyE) -> stripMuRefs bodyA == stripMuRefs bodyE
        _ -> False

    stripMuRefs ty = case ty of
        TVarRef _ -> testTVar "_"
        TArrow dom cod -> TArrow (stripMuRefs dom) (stripMuRefs cod)
        TBaseWithIdentity identity base -> TBaseWithIdentity identity base
        TConWithIdentity identity con args -> TConWithIdentity identity con (fmap stripMuRefs args)
        TVarAppRef _ args -> testTVarApp "_" (fmap stripMuRefs args)
        TForallRef _ mb body -> testTForall "_" (fmap stripBoundRefs mb) (stripMuRefs body)
        TMuRef _ body -> testTMu "_" (stripMuRefs body)
        TBottom -> TBottom

    stripBoundRefs bound = case bound of
        TArrow dom cod -> TArrow (stripMuRefs dom) (stripMuRefs cod)
        TBaseWithIdentity identity base -> TBaseWithIdentity identity base
        TConWithIdentity identity con args -> TConWithIdentity identity con (fmap stripMuRefs args)
        TVarAppRef _ args -> testTVarApp "_" (fmap stripMuRefs args)
        TForallRef _ mb body -> testTForall "_" (fmap stripBoundRefs mb) (stripMuRefs body)
        TMuRef _ body -> testTMu "_" (stripMuRefs body)
        TBottom -> TBottom

expectedRecursiveArrow :: ElabType
expectedRecursiveArrow =
    let recursiveTy = testTMu "a" (TArrow (testTVar "a") (TestElab.tBase (BaseTy "Int")))
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
