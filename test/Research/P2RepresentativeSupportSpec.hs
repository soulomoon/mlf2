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
