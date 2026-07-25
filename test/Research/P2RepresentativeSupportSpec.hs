module Research.P2RepresentativeSupportSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Set as Set
import Test.Hspec

import ElabTermTestSupport (testTForall, testTVar)
import MLF.Elab.Pipeline
    ( runPipelineElab
    )
import MLF.Frontend.Syntax
import MLF.Types.Elab
    ( BoundType
    , ElabType
    , Ty(..)
    )
import SpecUtil
    ( requireRight
    , unsafeNormalizeExpr
    )

spec :: Spec
spec =
    describe "P2 representative-support harness" $ do
        forM_ representativeSupportCases $ \(label, expr) ->
            it (label ++ " is recursive on runPipelineElab") $
                expectRecursiveCanonicalSupport expr

representativeSupportCases :: [(String, SurfaceExpr)]
representativeSupportCases =
    [ ("exact non-local C1 Int packet", c1IntExpr)
    , ("second route-pure non-local Bool packet", c1BoolExpr)
    , ("owner-sensitive non-local Int identity-consumer packet", identityWrappedExpr recursiveIntAnn)
    , ("owner-sensitive non-local Bool identity-consumer packet", identityWrappedExpr recursiveBoolAnn)
    , ("owner-sensitive non-local Int transparent-mediator packet", transparentWrappedExpr recursiveIntAnn)
    , ("owner-sensitive non-local Bool transparent-mediator packet", transparentWrappedExpr recursiveBoolAnn)
    , ("owner-sensitive non-local Int let-aliased transparent-mediator packet", aliasedTransparentWrappedExpr recursiveIntAnn)
    , ("owner-sensitive non-local Bool let-aliased transparent-mediator packet", aliasedTransparentWrappedExpr recursiveBoolAnn)
    , ("owner-sensitive non-local Int stacked transparent-mediator packet", stackedTransparentWrappedExpr recursiveIntAnn)
    , ("owner-sensitive non-local Bool stacked transparent-mediator packet", stackedTransparentWrappedExpr recursiveBoolAnn)
    , ("owner-sensitive non-local Int stacked let-aliased transparent-mediator packet", stackedTransparentWrappedExprWith aliasedTransparentWrap aliasedTransparentWrap recursiveIntAnn)
    , ("owner-sensitive non-local Bool stacked let-aliased transparent-mediator packet", stackedTransparentWrappedExprWith aliasedTransparentWrap aliasedTransparentWrap recursiveBoolAnn)
    , ("owner-sensitive non-local Int mixed direct/let-aliased stacked transparent-mediator packet", stackedTransparentWrappedExprWith transparentWrap aliasedTransparentWrap recursiveIntAnn)
    , ("owner-sensitive non-local Bool mixed direct/let-aliased stacked transparent-mediator packet", stackedTransparentWrappedExprWith transparentWrap aliasedTransparentWrap recursiveBoolAnn)
    , ("owner-sensitive non-local Int mixed let-aliased/direct stacked transparent-mediator packet", stackedTransparentWrappedExprWith aliasedTransparentWrap transparentWrap recursiveIntAnn)
    , ("owner-sensitive non-local Bool mixed let-aliased/direct stacked transparent-mediator packet", stackedTransparentWrappedExprWith aliasedTransparentWrap transparentWrap recursiveBoolAnn)
    , ("owner-sensitive non-local Int combined-wrapper transparent-mediator packet", combinedTransparentWrappedExprWith transparentWrap recursiveIntAnn)
    , ("owner-sensitive non-local Bool combined-wrapper transparent-mediator packet", combinedTransparentWrappedExprWith transparentWrap recursiveBoolAnn)
    , ("owner-sensitive non-local Int combined-wrapper let-aliased transparent-mediator packet", combinedTransparentWrappedExprWith aliasedTransparentWrap recursiveIntAnn)
    , ("owner-sensitive non-local Bool combined-wrapper let-aliased transparent-mediator packet", combinedTransparentWrappedExprWith aliasedTransparentWrap recursiveBoolAnn)
    , ("owner-sensitive non-local Int combined-wrapper stacked let-aliased transparent-mediator packet", combinedStackedTransparentWrappedExprWith aliasedTransparentWrap aliasedTransparentWrap recursiveIntAnn)
    , ("owner-sensitive non-local Bool combined-wrapper stacked let-aliased transparent-mediator packet", combinedStackedTransparentWrappedExprWith aliasedTransparentWrap aliasedTransparentWrap recursiveBoolAnn)
    , ("owner-sensitive non-local Int combined-wrapper mixed direct/let-aliased stacked transparent-mediator packet", combinedStackedTransparentWrappedExprWith transparentWrap aliasedTransparentWrap recursiveIntAnn)
    , ("owner-sensitive non-local Bool combined-wrapper mixed direct/let-aliased stacked transparent-mediator packet", combinedStackedTransparentWrappedExprWith transparentWrap aliasedTransparentWrap recursiveBoolAnn)
    ]

expectRecursiveCanonicalSupport :: SurfaceExpr -> IO ()
expectRecursiveCanonicalSupport expr = do
    let blocked = testTForall "a" Nothing (TArrow (testTVar "a") (testTVar "a"))
    (_term, ty) <-
        requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
    ty `shouldNotBe` blocked
    containsMu ty `shouldBe` True

c1IntExpr :: SurfaceExpr
c1IntExpr = ELet "k" (ELamAnn "x" recursiveIntAnn (EVar "x")) (EVar "k")

c1BoolExpr :: SurfaceExpr
c1BoolExpr = ELet "k" (ELamAnn "x" recursiveBoolAnn (EVar "x")) (EVar "k")

identityWrappedExpr :: SrcType -> SurfaceExpr
identityWrappedExpr ann =
    ELet
        "id"
        (ELam "z" (EVar "z"))
        (ELet "k" (ELamAnn "x" ann (EVar "x")) (ELet "hold" (EApp (EVar "id") (EVar "k")) (EVar "hold")))

transparentWrappedExpr :: SrcType -> SurfaceExpr
transparentWrappedExpr ann =
    ELet
        "wrap"
        transparentWrap
        (ELet "k" (ELamAnn "x" ann (EVar "x")) (ELet "hold" (EApp (EVar "wrap") (EVar "k")) (EVar "hold")))

aliasedTransparentWrappedExpr :: SrcType -> SurfaceExpr
aliasedTransparentWrappedExpr ann =
    ELet
        "wrap"
        aliasedTransparentWrap
        (ELet "k" (ELamAnn "x" ann (EVar "x")) (ELet "hold" (EApp (EVar "wrap") (EVar "k")) (EVar "hold")))

stackedTransparentWrappedExpr :: SrcType -> SurfaceExpr
stackedTransparentWrappedExpr =
    stackedTransparentWrappedExprWith transparentWrap transparentWrap

stackedTransparentWrappedExprWith :: SurfaceExpr -> SurfaceExpr -> SrcType -> SurfaceExpr
stackedTransparentWrappedExprWith wrap1 wrap2 ann =
    ELet
        "wrap1"
        wrap1
        ( ELet
            "wrap2"
            wrap2
            ( ELet
                "k"
                (ELamAnn "x" ann (EVar "x"))
                (ELet "hold" (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "k"))) (EVar "hold"))
            )
        )

combinedTransparentWrappedExprWith :: SurfaceExpr -> SrcType -> SurfaceExpr
combinedTransparentWrappedExprWith wrap ann =
    ELet
        "id"
        (ELam "z" (EVar "z"))
        ( ELet
            "wrap"
            wrap
            ( ELet
                "k"
                (EApp (EVar "id") (ELamAnn "x" ann (EVar "x")))
                (ELet "hold" (EApp (EVar "wrap") (EVar "k")) (EVar "hold"))
            )
        )

combinedStackedTransparentWrappedExprWith :: SurfaceExpr -> SurfaceExpr -> SrcType -> SurfaceExpr
combinedStackedTransparentWrappedExprWith wrap1 wrap2 ann =
    ELet
        "id"
        (ELam "z" (EVar "z"))
        ( ELet
            "wrap1"
            wrap1
            ( ELet
                "wrap2"
                wrap2
                ( ELet
                    "k"
                    (EApp (EVar "id") (ELamAnn "x" ann (EVar "x")))
                    (ELet "hold" (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "k"))) (EVar "hold"))
                )
            )
        )

transparentWrap :: SurfaceExpr
transparentWrap = ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z")))

aliasedTransparentWrap :: SurfaceExpr
aliasedTransparentWrap =
    ELam "h" (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))

recursiveIntAnn :: SrcType
recursiveIntAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))

recursiveBoolAnn :: SrcType
recursiveBoolAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))

containsMu :: ElabType -> Bool
containsMu ty = case ty of
    TMuRef _ _ -> True
    TArrow dom cod -> containsMu dom || containsMu cod
    TConWithIdentity _ _ args -> any containsMu args
    TForallRef _ mb body -> maybe False containsMuBound mb || containsMu body
    _ -> False
  where
    containsMuBound :: BoundType -> Bool
    containsMuBound bound = case bound of
        TArrow dom cod -> containsMu dom || containsMu cod
        TBaseWithIdentity _ _ -> False
        TConWithIdentity _ _ args -> any containsMu args
        TVarAppRef _ args -> any containsMu args
        TForallRef _ mb body -> maybe False containsMuBound mb || containsMu body
        TMuRef _ _ -> True
        TBottom -> False
