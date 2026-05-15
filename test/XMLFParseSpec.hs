module XMLFParseSpec (spec) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec

import MLF.Frontend.Syntax (Lit (..))
import MLF.XMLF
    ( XmlfComp (..)
    , XmlfTerm (..)
    , XmlfType (..)
    , parseXmlfComp
    , parseXmlfTerm
    , parseXmlfType
    )

spec :: Spec
spec = describe "xMLF parser" $ do
    describe "types" $ do
        it "parses bounded forall" $
            parseXmlfType "∀(a ⩾ Int) a -> a"
                `shouldBe` Right (XTForall "a" (XTBase "Int") (XTArrow (XTVar "a") (XTVar "a")))

        it "parses constructor application" $
            parseXmlfType "List Int" `shouldBe` Right (XTCon "List" (XTBase "Int" :| []))

        it "parses μ types" $
            parseXmlfType "μa. a -> Int"
                `shouldBe` Right (XTMu "a" (XTArrow (XTVar "a") (XTBase "Int")))

        it "rejects ASCII forall aliases" $
            parseXmlfType "forall (a ⩾ Int) a" `shouldSatisfy` isLeft

        it "rejects ASCII bound aliases" $
            parseXmlfType "∀(a >= Int) a" `shouldSatisfy` isLeft

        it "rejects ASCII bottom aliases" $
            forM_ ["bottom", "_|_"] $ \src ->
                parseXmlfType src `shouldSatisfy` isLeft

        it "rejects ASCII mu aliases" $
            parseXmlfType "mu a. a -> Int" `shouldSatisfy` isLeft

        it "rejects legacy unbounded forall syntax" $
            parseXmlfType "∀a. a" `shouldSatisfy` isLeft

    describe "computations" $ do
        it "parses canonical computations" $ do
            parseXmlfComp "ε" `shouldBe` Right XCId
            parseXmlfComp "a⊳" `shouldBe` Right (XCHyp "a")
            parseXmlfComp "∀(⩾ ⊲Int); N"
                `shouldBe` Right (XCSeq (XCInner (XCBot (XTBase "Int"))) XCElim)

        it "rejects legacy identity computation aliases" $
            forM_ ["1", "epsilon"] $ \src ->
                parseXmlfComp src `shouldSatisfy` isLeft

        it "rejects legacy hypothesis computation syntax" $
            parseXmlfComp "!a" `shouldSatisfy` isLeft

        it "rejects legacy bracketed computation syntax" $
            parseXmlfComp "⟨Int⟩" `shouldSatisfy` isLeft

        it "rejects bare type-as-computation syntax" $
            parseXmlfComp "Int" `shouldSatisfy` isLeft

        it "rejects malformed outer computation" $
            parseXmlfComp "∀(a ⩾ Int) N" `shouldSatisfy` isLeft

    describe "terms" $ do
        it "parses term with explicit type abstraction and instantiation" $ do
            let src = "Λ(a ⩾ ⊥) (λ(x : a) x)[∀(⩾ ⊲Int); N]"
                expected =
                    XTyAbs "a" XTBottom
                        (XTyInst
                            (XLam "x" (XTVar "a") (XVar "x"))
                            (XCSeq (XCInner (XCBot (XTBase "Int"))) XCElim))
            parseXmlfTerm src `shouldBe` Right expected

        it "parses let/application precedence" $ do
            let src = "let id = λ(x : Int) x in id 1"
                expected = XLet "id" (XLam "x" (XTBase "Int") (XVar "x")) (XApp (XVar "id") (XLit (LInt 1)))
            parseXmlfTerm src `shouldBe` Right expected

        it "rejects legacy lambda syntax" $
            parseXmlfTerm "λx:Int. x" `shouldSatisfy` isLeft

        it "rejects ASCII lambda aliases" $
            parseXmlfTerm "\\(x : Int) x" `shouldSatisfy` isLeft

        it "rejects legacy type abstraction syntax" $
            parseXmlfTerm "Λa. x" `shouldSatisfy` isLeft

        it "rejects ASCII type abstraction aliases" $
            parseXmlfTerm "Lambda(a ⩾ ⊥) x" `shouldSatisfy` isLeft

        it "parses recursive roll terms with recursive types" $ do
            let src = "roll[μself. self -> Int] x"
                expected =
                    XRoll
                        (XTMu "self" (XTArrow (XTVar "self") (XTBase "Int")))
                        (XVar "x")
            parseXmlfTerm src `shouldBe` Right expected

        it "parses recursive unroll terms" $ do
            let src = "unroll (roll[μself. self -> Int] x)"
                expected =
                    XUnroll
                        (XRoll
                            (XTMu "self" (XTArrow (XTVar "self") (XTBase "Int")))
                            (XVar "x"))
            parseXmlfTerm src `shouldBe` Right expected

        it "rejects malformed lambda binder" $
            parseXmlfTerm "λ(x) x" `shouldSatisfy` isLeft

        it "rejects roll without bracketed type" $
            parseXmlfTerm "roll μself. self -> Int x" `shouldSatisfy` isLeft

        it "rejects roll without body" $
            parseXmlfTerm "roll[μself. self -> Int]" `shouldSatisfy` isLeft

        it "rejects unroll without body" $
            parseXmlfTerm "unroll" `shouldSatisfy` isLeft

        it "rejects malformed roll bracketed type" $
            parseXmlfTerm "roll[μself. self -> Int x" `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
