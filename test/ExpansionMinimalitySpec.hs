{-# LANGUAGE GADTs #-}
module ExpansionMinimalitySpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Presolution (PresolutionResult(..))
import MLF.Constraint.Types.Witness (Expansion(..))
import MLF.Frontend.Syntax (Expr(..), Lit(..))
import SpecUtil (runToPresolutionDefault)

spec :: Spec
spec = describe "Expansion minimality" $ do
    it "identity expression gets ExpIdentity" $
        case runToPresolutionDefault Set.empty (ELam "x" (EVar "x")) of
            Left err -> expectationFailure $ "pipeline: " ++ err
            Right pres ->
                all isIdentityExp (IntMap.elems (prEdgeExpansions pres))
                    `shouldBe` True

    it "let-polymorphic use gets ExpInstantiate" $ do
        let expr = ELet "id" (ELam "x" (EVar "x"))
                    (EApp (EVar "id") (ELit (LInt 1)))
        case runToPresolutionDefault Set.empty expr of
            Left err -> expectationFailure $ "pipeline: " ++ err
            Right pres ->
                any isInstantiateExp (IntMap.elems (prEdgeExpansions pres))
                    `shouldBe` True

    it "application has structurally minimal expansions" $ do
        let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 1))
        case runToPresolutionDefault Set.empty expr of
            Left err -> expectationFailure $ "pipeline: " ++ err
            Right pres ->
                let exps = IntMap.elems (prEdgeExpansions pres)
                in all isMinimal exps `shouldBe` True

    it "nested let has structurally minimal expansions" $ do
        let expr = ELet "f" (ELam "x" (EVar "x"))
                    (ELet "g" (ELam "y" (EVar "y"))
                        (EApp (EVar "f") (EApp (EVar "g") (ELit (LInt 2)))))
        case runToPresolutionDefault Set.empty expr of
            Left err -> expectationFailure $ "pipeline: " ++ err
            Right pres ->
                let exps = IntMap.elems (prEdgeExpansions pres)
                in all isMinimal exps `shouldBe` True

-- | An expansion is minimal if compose has >= 2 parts.
isMinimal :: Expansion -> Bool
isMinimal ExpIdentity = True
isMinimal (ExpInstantiate _) = True
isMinimal (ExpForall _) = True
isMinimal (ExpCompose exps) = length exps >= 2

isIdentityExp :: Expansion -> Bool
isIdentityExp ExpIdentity = True
isIdentityExp _ = False

isInstantiateExp :: Expansion -> Bool
isInstantiateExp (ExpInstantiate _) = True
isInstantiateExp _ = False
