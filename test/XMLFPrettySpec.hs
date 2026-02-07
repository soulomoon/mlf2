module XMLFPrettySpec (spec) where

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
    , prettyXmlfComp
    , prettyXmlfTerm
    , prettyXmlfType
    )

spec :: Spec
spec = describe "xMLF pretty printer" $ do
    it "prints canonical type syntax" $ do
        let ty = XTForall "a" XTBottom (XTArrow (XTVar "a") (XTVar "a"))
        prettyXmlfType ty `shouldBe` "∀(a ⩾ ⊥) a -> a"

    it "prints canonical computation syntax" $ do
        let comp = XCSeq (XCInner (XCBot (XTBase "Int"))) XCElim
        prettyXmlfComp comp `shouldBe` "∀(⩾ ⊲Int); N"

    it "prints canonical term syntax" $ do
        let tm =
                XLet "id"
                    (XTyAbs "a" XTBottom (XLam "x" (XTVar "a") (XVar "x")))
                    (XTyInst (XVar "id") (XCSeq (XCInner (XCBot (XTBase "Int"))) XCElim))
        prettyXmlfTerm tm
            `shouldBe` "let id = Λ(a ⩾ ⊥) λ(x : a) x in id[∀(⩾ ⊲Int); N]"

    it "roundtrips type parse(pretty(type))" $ do
        let ty = XTForall "a" (XTBase "Int") (XTCon "List" (XTVar "a" :| []))
        parseXmlfType (prettyXmlfType ty) `shouldBe` Right ty

    it "roundtrips computation parse(pretty(comp))" $ do
        let comp = XCOuter "a" (XCSeq XCIntro XCElim)
        parseXmlfComp (prettyXmlfComp comp) `shouldBe` Right comp

    it "roundtrips term parse(pretty(term))" $ do
        let tm =
                XTyAbs "a" XTBottom
                    (XApp
                        (XTyInst (XVar "f") (XCInner (XCBot (XTVar "a"))))
                        (XLit (LBool True)))
        parseXmlfTerm (prettyXmlfTerm tm) `shouldBe` Right tm
