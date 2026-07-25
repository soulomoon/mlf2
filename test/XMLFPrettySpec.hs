module XMLFPrettySpec (spec) where

import qualified ElabTypeTestSupport as TestElab
import Test.Hspec

import ElabTermTestSupport
    ( mkTestDeferredVar
    , mkTestLocalLam
    , mkTestLocalLet
    , mkTestTyAbs
    , testTForall
    , testTMu
    , testTVar
    )
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId (..))
import MLF.Elab.Types (Pretty (..), schemeFromType)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Elab qualified as Elab
import MLF.XMLF
    ( XmlfComp (..)
    , XmlfType (..)
    , prettyXmlfComp
    , prettyXmlfTerm
    , prettyXmlfType
    )

spec :: Spec
spec = describe "xMLF pretty printer" $ do
    it "prints canonical type syntax" $ do
        let ty = XTForall "a" XTBottom (XTArrow (XTVar "a") (XTVar "a"))
        prettyXmlfType ty `shouldBe` "∀(a ⩾ ⊥) a -> a"

    it "prints μ types canonically" $ do
        let ty = XTMu "a" (XTArrow (XTVar "a") (XTBase "Int"))
        prettyXmlfType ty `shouldBe` "μa. a -> Int"

    it "prints canonical computation syntax" $ do
        let comp = XCSeq (XCInner (XCBot (XTBase "Int"))) XCElim
        prettyXmlfComp comp `shouldBe` "∀(⩾ ⊲Int); N"

    it "prints canonical term syntax" $ do
        let aTy = testTVar "a"
            intTy = TestElab.tBase (BaseTy "Int")
            idTy = testTForall "a" Nothing (Elab.TArrow aTy aTy)
            tm =
                mkTestLocalLet "id" (schemeFromType idTy)
                    (mkTestTyAbs "a" Nothing (mkTestLocalLam "x" aTy (mkTestDeferredVar "x")))
                    (Elab.ETyInst (mkTestDeferredVar "id") (Elab.InstSeq (Elab.InstInside (Elab.InstBot intTy)) Elab.InstElim))
        prettyXmlfTerm tm
            `shouldBe` "let id = Λ(a ⩾ ⊥) λ(x : a) x in id[∀(⩾ ⊲Int); N]"

    it "prints canonical recursive roll syntax" $ do
        let recursiveTy = testTMu "self" (Elab.TArrow (testTVar "self") (TestElab.tBase (BaseTy "Int")))
            tm = Elab.ERoll recursiveTy (mkTestDeferredVar "x")
        prettyXmlfTerm tm `shouldBe` "roll[μself. self -> Int] x"

    it "prints canonical recursive unroll syntax" $ do
        let recursiveTy = testTMu "self" (Elab.TArrow (testTVar "self") (TestElab.tBase (BaseTy "Int")))
            tm = Elab.EUnroll (Elab.ERoll recursiveTy (mkTestDeferredVar "x"))
        prettyXmlfTerm tm `shouldBe` "unroll (roll[μself. self -> Int] x)"

    it "keeps distinct checked binder identities visually distinct" $ do
        let outerRef =
                Elab.typeBinderRefFromIdentity
                    (Elab.typeBinderIdentityFromNode (NodeId 1))
                    "a"
            innerRef =
                Elab.typeBinderRefFromIdentity
                    (Elab.typeBinderIdentityFromNode (NodeId 2))
                    "a"
            ty =
                Elab.TForallRef outerRef Nothing
                    (Elab.TForallRef innerRef Nothing
                        (Elab.TArrow
                            (Elab.TVarRef innerRef)
                            (Elab.TVarRef outerRef))) :: Elab.ElabType
        pretty ty
            `shouldBe` "∀(a ⩾ ⊥) ∀(a1 ⩾ ⊥) a1 -> a"

    it "reuses the identity-aware display map throughout one checked term" $ do
        let outerRef =
                Elab.typeBinderRefFromIdentity
                    (Elab.typeBinderIdentityFromNode (NodeId 1))
                    "a"
            innerRef =
                Elab.typeBinderRefFromIdentity
                    (Elab.typeBinderIdentityFromNode (NodeId 2))
                    "a"
            tm =
                Elab.ETyAbsRef outerRef Nothing
                    (Elab.ETyAbsRef innerRef Nothing
                        (Elab.ETyInst
                            (mkTestDeferredVar "f")
                            (Elab.InstBot
                                (Elab.TArrow
                                    (Elab.TVarRef innerRef)
                                    (Elab.TVarRef outerRef)))))
        prettyXmlfTerm tm
            `shouldBe` "Λ(a ⩾ ⊥) Λ(a1 ⩾ ⊥) f[⊲(a1 -> a)]"

    it "prints type syntax without requiring a parser" $ do
        let tm =
                mkTestTyAbs "a" Nothing
                    (Elab.EApp
                        (Elab.ETyInst (mkTestDeferredVar "f") (Elab.InstInside (Elab.InstBot (testTVar "a"))))
                        (Elab.ELit (LBool True)))
        prettyXmlfTerm tm `shouldBe` "Λ(a ⩾ ⊥) f[∀(⩾ ⊲a)] true"
