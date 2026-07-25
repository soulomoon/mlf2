module FrontendDesugarSpec (spec) where

import Test.Hspec

import MLF.Frontend.Desugar (desugarResolvedSurface, desugarSurface)
import MLF.Frontend.Syntax
import MLF.Types.Identity
    ( IdDetails (EvidenceId, LocalId)
    , freshLocalRef
    , initialIdentityGenerator
    )

spec :: Spec
spec = describe "MLF.Frontend.Desugar" $ do
    it "desugars annotated terms to coercion application" $ do
        let ty = STBase "Int"
            expr = EAnn (EVar "x") ty
        desugarSurface expr
            `shouldBe` EApp (ECoerceConst ty) (EVar "x")

    it "desugars annotated lambdas to lambda plus let-bound coercion" $ do
        let ty = STBase "Int"
            expr = ELamAnn "x" ty (EVar "x")
        desugarSurface expr
            `shouldBe`
                ELam "x"
                    (ELet "x"
                        (EApp (ECoerceConst ty) (EVar "x"))
                        (EVar "x"))

    it "recurses structurally through let, lambda, and application" $ do
        let intTy = STBase "Int"
            boolTy = STBase "Bool"
            expr =
                ELet "f"
                    (ELamAnn "x" intTy (EVar "x"))
                    (EApp (EVar "f") (EAnn (EVar "y") boolTy))
        desugarSurface expr
            `shouldBe`
                ELet "f"
                    (ELam "x"
                        (ELet "x"
                            (EApp (ECoerceConst intTy) (EVar "x"))
                            (EVar "x")))
                    (EApp (EVar "f") (EApp (ECoerceConst boolTy) (EVar "y")))

    it "keeps typed-let sugar coercion-only on the RHS" $ do
        let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            rhs = EAnn (ELam "x" (EVar "x")) ann
            expr = ELet "id" rhs (EVar "id")
        desugarSurface expr
            `shouldBe`
                ELet "id"
                    (EApp (ECoerceConst ann) (ELam "x" (EVar "x")))
                    (EVar "id")

    it "desugars compiler-owned evidence parameters to exact core lambdas" $ do
        let ty = STArrow (STBase "Int") (STBase "Bool")
            (evidenceRef, generator) = freshLocalRef "$evidence" initialIdentityGenerator
            reference = ResolvedTermReference (EvidenceId evidenceRef) "$evidence"
            expr = ELamAnnNode reference ty (EVarNode reference)
            (core, _) = desugarResolvedSurface generator expr
        core
            `shouldBe` EExactLamNode reference ty (EVarNode reference)

    it "redirects an annotation mediator by identity, not display spelling" $ do
        let ty = STBase "Int"
            (sourceLocal, generator1) = freshLocalRef "x" initialIdentityGenerator
            (shadowLocal, generator2) = freshLocalRef "x" generator1
            (mediatorLocal, expectedGenerator) = freshLocalRef "x" generator2
            sourceRef = ResolvedTermReference (LocalId sourceLocal) "x"
            staleSourceOccurrence = ResolvedTermReference (LocalId sourceLocal) "$stale-x"
            shadowOccurrence = ResolvedTermReference (LocalId shadowLocal) "x"
            mediatorRef = ResolvedTermReference (LocalId mediatorLocal) "x"
            expr =
                ELamAnnNode sourceRef ty
                    (EApp (EVarNode staleSourceOccurrence) (EVarNode shadowOccurrence))
            (core, actualGenerator) = desugarResolvedSurface generator2 expr
        core
            `shouldBe`
                ELamNode sourceRef
                    ( ELetNode mediatorRef
                        (EApp (ECoerceConst ty) (EVarNode sourceRef))
                        (EApp (EVarNode mediatorRef) (EVarNode shadowOccurrence))
                    )
        actualGenerator `shouldBe` expectedGenerator

    it "redirects both annotated self-application occurrences to one mediator" $ do
        let sigmaId = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            (sourceLocal, generator1) = freshLocalRef "g" initialIdentityGenerator
            (mediatorLocal, expectedGenerator) = freshLocalRef "g" generator1
            sourceRef = ResolvedTermReference (LocalId sourceLocal) "g"
            mediatorRef = ResolvedTermReference (LocalId mediatorLocal) "g"
            expr =
                ELamAnnNode sourceRef sigmaId
                    (EApp (EVarNode sourceRef) (EVarNode sourceRef))
            (core, actualGenerator) = desugarResolvedSurface generator1 expr
        core
            `shouldBe`
                ELamNode sourceRef
                    ( ELetNode mediatorRef
                        (EApp (ECoerceConst sigmaId) (EVarNode sourceRef))
                        (EApp (EVarNode mediatorRef) (EVarNode mediatorRef))
                    )
        actualGenerator `shouldBe` expectedGenerator
