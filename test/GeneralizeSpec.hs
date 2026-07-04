{-# LANGUAGE DataKinds #-}
module GeneralizeSpec (spec) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Test.Hspec

import ElabTermTestSupport (testTForall, testTVar)
import MLF.Constraint.Types.Graph (BaseTy(..), NodeId(..))
import MLF.Elab.Generalize
    ( inlineRigidTypes
    , selectSolvedOrderWithShadow
    , shadowCompareTypes
    )
import MLF.Elab.Run.TypeOps (simplifyAnnotationType)
import MLF.Elab.Run.ResultType
    ( inferInstAppArgsFromSchemeRefs
    , substTypeSelectiveRefs
    )
import MLF.Elab.Pipeline (ElabError(..))
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace(..), symbolIdentityFromParts)
import MLF.Types.Elab
    ( ElabType
    , Ty(..)
    , TypeBinderRef
    , tBase
    , tCon
    , tForallWithRef
    , tMuWithRef
    , tVarAppWithRef
    , tVarWithRef
    , typeBinderIdentityFromNode
    , typeBinderRefFromIdentity
    )
import MLF.Types.Identity (UniqueIdentity(..))

typeRef :: Int -> String -> TypeBinderRef
typeRef key name =
    typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name

typeIdentity :: Int -> SymbolIdentity
typeIdentity unique =
    symbolIdentityFromParts (UniqueIdentity unique) SymbolType "Main" "Token" Nothing

spec :: Spec
spec = do
    describe "Generalize shadow comparator" $ do
        it "accepts alpha-equivalent types" $ do
            let solvedTy = testTForall "a" Nothing (testTVar "a")
                baseTy = testTForall "b" Nothing (testTVar "b")
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "rejects same structure when solved/base free identities differ" $ do
            let solvedTy = TArrow (testTVar "t14") (testTVar "t14")
                baseTy = TArrow (testTVar "a") (testTVar "a")
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed identity mismatch, got: " ++ show other)

        it "accepts same identity when display names differ" $ do
            let solvedRef = typeRef 40 "t14"
                baseRef = typeRef 40 "a"
                solvedTy = TArrow (tVarWithRef solvedRef) (tVarWithRef solvedRef)
                baseTy = TArrow (tVarWithRef baseRef) (tVarWithRef baseRef)
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "rejects same-named free variables with different identities" $ do
            let solvedRef = typeRef 41 "a"
                baseRef = typeRef 42 "a"
                solvedTy = TArrow (tVarWithRef solvedRef) (tVarWithRef solvedRef)
                baseTy = TArrow (tVarWithRef baseRef) (tVarWithRef baseRef)
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed identity mismatch, got: " ++ show other)

        it "accepts nested forall body renaming without bounds" $ do
            let solvedTy =
                    testTForall "a" Nothing
                        (testTForall "b" Nothing (TArrow (testTVar "a") (testTVar "b")))
                baseTy =
                    testTForall "x" Nothing
                        (testTForall "y" Nothing (TArrow (testTVar "x") (testTVar "y")))
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "accepts nested forall renaming through explicit bounds and body" $ do
            let solvedFreeA = typeRef 50 "a"
                baseFreeX = typeRef 50 "x"
                solvedFreeB = typeRef 51 "b"
                baseFreeY = typeRef 51 "y"
                solvedA = typeRef 52 "a"
                baseX = typeRef 53 "x"
                solvedB = typeRef 54 "b"
                baseY = typeRef 55 "y"
                solvedTy =
                    tForallWithRef solvedA (Just (TArrow (tVarWithRef solvedFreeA) (tVarWithRef solvedFreeA)))
                        (tForallWithRef solvedB (Just (tCon (BaseTy "Box") (tVarWithRef solvedFreeA :| [tVarWithRef solvedFreeB])))
                            (TArrow (tVarWithRef solvedB) (tVarWithRef solvedA)))
                baseTy =
                    tForallWithRef baseX (Just (TArrow (tVarWithRef baseFreeX) (tVarWithRef baseFreeX)))
                        (tForallWithRef baseY (Just (tCon (BaseTy "Box") (tVarWithRef baseFreeX :| [tVarWithRef baseFreeY])))
                            (TArrow (tVarWithRef baseY) (tVarWithRef baseX)))
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "rejects inconsistent free-variable reuse under renaming" $ do
            let solvedTy = TArrow (testTVar "a") (testTVar "b")
                baseTy = TArrow (testTVar "x") (testTVar "x")
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)

        it "rejects non-bijective mapping reused across bound and body" $ do
            let solvedTy =
                    testTForall "a" (Just (TArrow (testTVar "a") (testTVar "a")))
                        (TArrow (testTVar "a") (testTVar "b"))
                baseTy =
                    testTForall "x" (Just (TArrow (testTVar "x") (testTVar "x")))
                        (TArrow (testTVar "x") (testTVar "x"))
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)

        it "accepts renamed variables through constructor arguments" $ do
            let solvedTy =
                    testTForall "a" Nothing
                        (testTForall "b" Nothing (tCon (BaseTy "Pair") (testTVar "a" :| [testTVar "b"])))
                baseTy =
                    testTForall "x" Nothing
                        (testTForall "y" Nothing (tCon (BaseTy "Pair") (testTVar "x" :| [testTVar "y"])))
            shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

        it "rejects same-named type heads with different identities" $ do
            let solvedTy = TBaseWithIdentity (Just (typeIdentity 991811)) (BaseTy "Token")
                baseTy = TBaseWithIdentity (Just (typeIdentity 991812)) (BaseTy "Token")
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed identity mismatch, got: " ++ show other)

        it "keeps same-named base bounds separate when type head identities differ" $ do
            let leftRef = typeRef 991813 "a"
                rightRef = typeRef 991814 "b"
                leftBound = TBaseWithIdentity (Just (typeIdentity 991815)) (BaseTy "Token")
                rightBound = TBaseWithIdentity (Just (typeIdentity 991816)) (BaseTy "Token")
                ty =
                    tForallWithRef leftRef (Just leftBound) $
                        tForallWithRef rightRef (Just rightBound) $
                            TArrow (tVarWithRef leftRef) (tVarWithRef rightRef)
            simplifyAnnotationType ty `shouldBe` ty

        it "rejects semantic mismatch with shadow reify mismatch diagnostics" $ do
            let solvedTy = testTForall "a" Nothing (TArrow (testTVar "a") (testTVar "a"))
                baseTy = testTForall "a" Nothing (TArrow (testTVar "a") (tBase (BaseTy "Int")))
            case shadowCompareTypes "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)

    describe "selectSolvedOrderWithShadow" $ do
        it "returns solved type when solved/base shadow comparison succeeds" $ do
            let solvedTy = testTForall "a" Nothing (testTVar "a")
                baseTy = testTForall "b" Nothing (testTVar "b")
            selectSolvedOrderWithShadow "ctx" solvedTy (Just baseTy) `shouldBe` Right solvedTy

        it "returns solved output even when base output is alpha-equivalent but syntactically different" $ do
            let solvedTy = testTForall "a" Nothing (testTVar "a")
                baseTy = testTForall "z" Nothing (testTVar "z")
            selectSolvedOrderWithShadow "ctx" solvedTy (Just baseTy) `shouldBe` Right solvedTy

        it "fails hard on solved/base shadow mismatch when base shadow is present" $ do
            let solvedTy = testTForall "a" Nothing (TArrow (testTVar "a") (testTVar "a"))
                baseTy = testTForall "a" Nothing (TArrow (testTVar "a") (tBase (BaseTy "Int")))
            case selectSolvedOrderWithShadow "ctx" solvedTy (Just baseTy) of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)

        it "reports context and normalized type diagnostics on mismatch" $ do
            let solvedTy = testTVar "a"
                baseTy = tBase (BaseTy "Int")
            case selectSolvedOrderWithShadow "generalizeAt:caseX" solvedTy (Just baseTy) of
                Left (ValidationFailed msgs) -> do
                    msgs `shouldSatisfy` any (isInfixOf "context=generalizeAt:caseX")
                    msgs `shouldSatisfy` any (isInfixOf "scopeRootC=")
                    msgs `shouldSatisfy` any (isInfixOf "typeRoot=")
                    msgs `shouldSatisfy` any (isInfixOf "binders=")
                    msgs `shouldSatisfy` any (isInfixOf "solved=")
                    msgs `shouldSatisfy` any (isInfixOf "base=")
                other ->
                    expectationFailure ("Expected ValidationFailed diagnostics, got: " ++ show other)

    describe "Instantiation inference strictness" $ do
        it "returns Nothing when a bounded body variable only matches via fallback recovery" $ do
            let refA = typeRef 10 "a"
            inferInstAppArgsFromSchemeRefs
                [(refA, Just (tBase (BaseTy "Bool")))]
                (tVarWithRef refA)
                (tBase (BaseTy "Int"))
                `shouldBe` Nothing

        it "preserves identity refs during selective substitution walks" $ do
            let refA = typeRef 20 "a"
                refF = typeRef 21 "f"
                refM = typeRef 22 "m"
                ty :: ElabType
                ty =
                    tForallWithRef
                        refA
                        (Just (tVarAppWithRef refF (tVarWithRef refA :| [])))
                        (tMuWithRef refM (tVarWithRef refA))
            substTypeSelectiveRefs [] Map.empty ty `shouldBe` ty

        it "infers instantiation args by type binder identity after display renames" $ do
            let refA = typeRef 30 "a"
                refA' = typeRef 30 "a1"
            inferInstAppArgsFromSchemeRefs
                [(refA, Nothing)]
                (tVarWithRef refA')
                (tBase (BaseTy "Int"))
                `shouldBe` Just [tBase (BaseTy "Int")]

        it "does not infer instantiation args for same-named different identities" $ do
            let refA = typeRef 31 "a"
                refB = typeRef 32 "a"
            inferInstAppArgsFromSchemeRefs
                [(refA, Nothing)]
                (tVarWithRef refB)
                (tBase (BaseTy "Int"))
                `shouldBe` Nothing

        it "does not selectively substitute same-named different identities" $ do
            let refA = typeRef 33 "a"
                refB = typeRef 34 "a"
                subst = Map.singleton refA (tBase (BaseTy "Int"))
            substTypeSelectiveRefs [] subst (tVarWithRef refB)
                `shouldBe` tVarWithRef refB

    describe "inlineRigidTypes" $ do
        it "inlines rigid bounds by identity, not display name" $ do
            let refA = typeRef 60 "a"
                refARenamed = typeRef 60 "renamed"
                refB = typeRef 61 "a"
                rigidBounds = Map.singleton refA (tBase (BaseTy "Int"))
                ty = TArrow (tVarWithRef refARenamed) (tVarWithRef refB)
            inlineRigidTypes rigidBounds ty
                `shouldBe` TArrow (tBase (BaseTy "Int")) (tVarWithRef refB)

        it "does not inline under a binder with the same identity" $ do
            let refA = typeRef 62 "a"
                rigidBounds = Map.singleton refA (tBase (BaseTy "Int"))
                ty = tForallWithRef refA Nothing (tVarWithRef refA)
            inlineRigidTypes rigidBounds ty `shouldBe` ty
