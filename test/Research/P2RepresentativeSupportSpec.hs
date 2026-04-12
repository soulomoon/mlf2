module Research.P2RepresentativeSupportSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.Elab.Pipeline
    ( runPipelineElab
    , runPipelineElabChecked
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
        it "keeps the exact non-local C1 Int packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport c1IntExpr

        it "keeps a second route-pure non-local Bool packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport c1BoolExpr

        it "keeps the owner-sensitive non-local Int identity-consumer packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (identityWrappedExpr recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool identity-consumer packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (identityWrappedExpr recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (transparentWrappedExpr recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (transparentWrappedExpr recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int let-aliased transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (aliasedTransparentWrappedExpr recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool let-aliased transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (aliasedTransparentWrappedExpr recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int stacked transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (stackedTransparentWrappedExpr recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool stacked transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (stackedTransparentWrappedExpr recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int stacked let-aliased transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (stackedTransparentWrappedExprWith aliasedTransparentWrap aliasedTransparentWrap recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool stacked let-aliased transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (stackedTransparentWrappedExprWith aliasedTransparentWrap aliasedTransparentWrap recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int mixed direct/let-aliased stacked transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (stackedTransparentWrappedExprWith transparentWrap aliasedTransparentWrap recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool mixed direct/let-aliased stacked transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (stackedTransparentWrappedExprWith transparentWrap aliasedTransparentWrap recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int mixed let-aliased/direct stacked transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (stackedTransparentWrappedExprWith aliasedTransparentWrap transparentWrap recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool mixed let-aliased/direct stacked transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (stackedTransparentWrappedExprWith aliasedTransparentWrap transparentWrap recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int combined-wrapper transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (combinedTransparentWrappedExprWith transparentWrap recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool combined-wrapper transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (combinedTransparentWrappedExprWith transparentWrap recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int combined-wrapper let-aliased transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (combinedTransparentWrappedExprWith aliasedTransparentWrap recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool combined-wrapper let-aliased transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (combinedTransparentWrappedExprWith aliasedTransparentWrap recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int combined-wrapper stacked let-aliased transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (combinedStackedTransparentWrappedExprWith aliasedTransparentWrap aliasedTransparentWrap recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool combined-wrapper stacked let-aliased transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (combinedStackedTransparentWrappedExprWith aliasedTransparentWrap aliasedTransparentWrap recursiveBoolAnn)

        it "keeps the owner-sensitive non-local Int combined-wrapper mixed direct/let-aliased stacked transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (combinedStackedTransparentWrappedExprWith transparentWrap aliasedTransparentWrap recursiveIntAnn)

        it "keeps the owner-sensitive non-local Bool combined-wrapper mixed direct/let-aliased stacked transparent-mediator packet recursive on both authoritative entrypoints" $
            expectRecursiveAuthoritativeSupport (combinedStackedTransparentWrappedExprWith transparentWrap aliasedTransparentWrap recursiveBoolAnn)

expectRecursiveAuthoritativeSupport :: SurfaceExpr -> IO ()
expectRecursiveAuthoritativeSupport expr = do
    let blocked = TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))
    (_uncheckedTerm, uncheckedTy) <-
        requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
    (_checkedTerm, checkedTy) <-
        requireRight (runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
    uncheckedTy `shouldNotBe` blocked
    checkedTy `shouldNotBe` blocked
    containsMu uncheckedTy `shouldBe` True
    containsMu checkedTy `shouldBe` True

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
    TMu _ _ -> True
    TArrow dom cod -> containsMu dom || containsMu cod
    TCon _ args -> any containsMu args
    TForall _ mb body -> maybe False containsMuBound mb || containsMu body
    _ -> False
  where
    containsMuBound :: BoundType -> Bool
    containsMuBound bound = case bound of
        TArrow dom cod -> containsMu dom || containsMu cod
        TBase _ -> False
        TCon _ args -> any containsMu args
        TForall _ mb body -> maybe False containsMuBound mb || containsMu body
        TMu _ _ -> True
        TBottom -> False
