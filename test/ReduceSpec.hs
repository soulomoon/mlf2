{-# LANGUAGE GADTs #-}
-- Convention: Promote minimized QuickCheck counterexamples from
-- TypeSoundnessSpec / PipelineSpec into fixed regression tests here.
module ReduceSpec (spec) where

import qualified ElabTypeTestSupport as TestElab
import Control.Monad (forM_)
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..), NodeId(..))
import MLF.Elab.Pipeline
    ( XmlfTerm(..)
    , Ty(..)
    , Instantiation(..)
    , normalize
    , reduceLeadingTypeInstantiationRedexes
    , renderPipelineError
    , runPipelineElab
    , schemeFromType
    , step
    , typeCheck
    )
import MLF.Elab.TermClosure (preserveRetainedChildAuthoritativeResult)
import MLF.Elab.Reduce.TestSupport (collectApplicationSpineThroughHeadTypeRedexes)
import MLF.Types.Elab
    ( ResolvedVar(..)
    , TypeBinderRef
    , eTyAbsWithRef
    , instAbstrWithRef
    , instUnderWithRef
    , resolvedVarReferenceName
    , resolvedVarSameIdentity
    , tVarWithRef
    , typeBinderIdentityFromNode
    , typeBinderRefIdentity
    , typeBinderRefName
    , typeBinderRefFromIdentity
    )
import MLF.Frontend.Program.Builtins (builtinValueIdentity)
import MLF.Frontend.Syntax (Lit(..))
import qualified MLF.Frontend.Syntax as Surf (Expr(..), SrcTy(..))
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (IdDetails(..), idDetailsStableName, primitiveRefFromSymbol, freshLocalRef, initialIdentityGenerator, typeBinderIdentityStableName)
import ElabTermTestSupport
    ( generatedLocalRef
    , generatedResolvedLocalForName
    , mkTestDeferredVar
    , mkTestLocalLam
    , mkTestLocalLet
    , mkTestTyAbs
    , testTForall
    , testTMu
    , testTVar
    )
import SpecUtil (mkForalls, requireRight, unsafeNormalizeExpr)

spec :: Spec
spec = do
    let intTy = TestElab.tBase (BaseTy "Int")
        boolTy = TestElab.tBase (BaseTy "Bool")
        idLam = mkTestLocalLam "x" intTy (mkTestDeferredVar "x")
        polyLam = mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x")
        recursiveIntTy = testTMu "self" (TArrow (testTVar "self") intTy)
        recursiveBody = mkTestLocalLam "self" recursiveIntTy (ELit (LInt 1))
        resolvedLocal ref runtime ty =
            generatedResolvedLocalForName ref runtime ty
        typeRef :: Int -> String -> TypeBinderRef
        typeRef key name =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name
        boundFromType ty = case ty of
            TVarRef ref -> error ("boundFromType: unexpected variable bound " ++ show (typeBinderRefName ref))
            TArrow a b -> TArrow a b
            TConWithIdentity identity c args -> TConWithIdentity identity c args
            TVarAppRef ref args -> TVarAppRef ref args
            TBaseWithIdentity identity b -> TBaseWithIdentity identity b
            TBottom -> TBottom
            TForallRef ref mb body -> TForallRef ref mb body
            TMuRef ref body -> TMuRef ref body

        erase :: XmlfTerm -> String
        erase term = case term of
            EVarNode resolved -> "v:" ++ resolvedVarReferenceName resolved
            ELit l -> "lit:" ++ show l
            ELam resolved body -> "lam:" ++ resolvedVarReferenceName resolved ++ "(" ++ erase body ++ ")"
            EApp f a -> "app(" ++ erase f ++ "," ++ erase a ++ ")"
            ELet resolved _ rhs body -> "let:" ++ resolvedVarReferenceName resolved ++ "(" ++ erase rhs ++ "," ++ erase body ++ ")"
            ETyAbsRef _ _ body -> erase body
            ETyInst e _ -> erase e
            ERoll _ body -> "roll(" ++ erase body ++ ")"
            EUnroll body -> "unroll(" ++ erase body ++ ")"

    describe "Formal obligations ledger anchors (Chapter 14 reduction)" $ do
        it "O14-RED-BETA O14-RED-BETALET O14-RED-REFLEX O14-RED-TRANS O14-RED-QUANT-INTRO O14-RED-QUANT-ELIM O14-RED-INNER O14-RED-OUTER O14-RED-CONTEXT: reduction-rule anchors and erasure proxy" $ do
            step (EApp idLam (ELit (LInt 1))) `shouldBe` Just (ELit (LInt 1))
            step (mkTestLocalLet "x" (schemeFromType intTy) (ELit (LInt 1)) (mkTestDeferredVar "x")) `shouldBe` Just (ELit (LInt 1))

            let reflTerm = ETyInst (ELit (LInt 1)) InstId
            step reflTerm `shouldBe` Just (ELit (LInt 1))
            fmap erase (step reflTerm) `shouldBe` Just (erase reflTerm)

            step (ETyInst (ELit (LInt 1)) (InstSeq InstIntro InstElim))
                `shouldBe` Just (ETyInst (ETyInst (ELit (LInt 1)) InstIntro) InstElim)
            case step (ETyInst (ELit (LInt 1)) InstIntro) of
                Just (ETyAbsRef ref Nothing (ELit (LInt 1)))
                    | typeBinderRefName ref == "u0" -> pure ()
                other -> expectationFailure ("Expected generated InstIntro abstraction, got: " ++ show other)
            step (ETyInst (mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))) InstElim)
                `shouldBe` Just (mkTestLocalLam "x" TBottom (mkTestDeferredVar "x"))
            step (ETyInst (mkTestTyAbs "a" Nothing (mkTestDeferredVar "x")) (InstInside (InstBot intTy)))
                `shouldBe` Just (mkTestTyAbs "a" (Just (boundFromType intTy)) (mkTestDeferredVar "x"))
            step (ETyInst (mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))) (instUnderWithRef (typeRef 94 "b") (InstApp intTy)))
                `shouldBe` Just (mkTestTyAbs "a" Nothing (ETyInst (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x")) (InstApp intTy)))
            step (EApp idLam (EApp (mkTestLocalLam "y" intTy (mkTestDeferredVar "y")) (ELit (LInt 1))))
                `shouldBe` Just (EApp idLam (ELit (LInt 1)))

    describe "Phase 7 reduce" $ do
        it "reduces a dominating quantifier-elimination spine before entering its body" $ do
            let selfRef = typeRef 950 "self"
                recursiveTy = TMuRef selfRef (TArrow (TVarRef selfRef) intTy)
                recursiveBound = boundFromType recursiveTy
                sourceRef = typeRef 951 "source"
                operatedRef = typeRef 952 "operated"
                param = resolvedLocal "$x#952" "x" (TVarRef operatedRef)
                body = ELam param (EUnroll (EVarNode param))
                term =
                    ETyInst
                        ( eTyAbsWithRef
                            sourceRef
                            (Just recursiveBound)
                            (eTyAbsWithRef operatedRef (Just recursiveBound) body)
                        )
                        (InstSeq InstElim InstElim)
            case reduceLeadingTypeInstantiationRedexes term of
                Just reduced@(ELam param' (EUnroll (EVarNode occurrence'))) -> do
                    resolvedVarType param' `shouldBe` recursiveTy
                    resolvedVarType occurrence' `shouldBe` recursiveTy
                    case typeCheck reduced of
                        Right {} -> pure ()
                        Left err -> expectationFailure ("Expected reduced term to typecheck, got: " ++ show err)
                other -> expectationFailure ("Expected a concrete recursive lambda, got: " ++ show other)

        it "does not evaluate value-level lets while reducing the leading type spine" $ do
            let ref = typeRef 953 "a"
                retainedLet =
                    mkTestLocalLet
                        "answer"
                        (schemeFromType intTy)
                        (ELit (LInt 42))
                        (mkTestDeferredVar "answer")
                term = ETyInst (eTyAbsWithRef ref Nothing retainedLet) InstElim
            case reduceLeadingTypeInstantiationRedexes term of
                Just ELet {} -> pure ()
                other -> expectationFailure ("Expected the value-level let to remain, got: " ++ show other)

        it "erases identity instantiation without evaluating its non-value operand" $ do
            let retainedLet =
                    mkTestLocalLet
                        "answer"
                        (schemeFromType intTy)
                        (ELit (LInt 42))
                        (mkTestDeferredVar "answer")
            reduceLeadingTypeInstantiationRedexes (ETyInst retainedLet InstId)
                `shouldBe` Just retainedLet

        it "erases a matching Hyp after bounded elimination under a value lambda" $ do
            let ref = typeRef 954 "result"
                evidence = resolvedLocal "$evidence#954" "evidence" (TVarRef ref)
                term =
                    ETyInst
                        ( eTyAbsWithRef
                            ref
                            (Just (boundFromType intTy))
                            ( ELam
                                evidence
                                (ETyInst (EVarNode evidence) (instAbstrWithRef ref))
                            )
                        )
                        InstElim
                specializedEvidence =
                    evidence{resolvedVarType = intTy}
            reduceLeadingTypeInstantiationRedexes term
                `shouldBe` Just (ELam specializedEvidence (EVarNode specializedEvidence))

        it "reduces an alpha-equivalent forall argument at its explicit bound" $ do
            let resultRef = typeRef 956 "bounded-result"
                boundRef = typeRef 957 "bound-a"
                argumentRef = typeRef 958 "argument-a"
                boundTy =
                    TForallRef
                        boundRef
                        Nothing
                        (TArrow (TVarRef boundRef) (TVarRef boundRef))
                argumentTy =
                    TForallRef
                        argumentRef
                        Nothing
                        (TArrow (TVarRef argumentRef) (TVarRef argumentRef))
                occurrence = resolvedLocal "$x#956" "x" (TVarRef resultRef)
                term =
                    ETyInst
                        (eTyAbsWithRef resultRef (Just (boundFromType boundTy)) (EVarNode occurrence))
                        (InstApp argumentTy)
                expected = EVarNode occurrence {resolvedVarType = argumentTy}
            typeBinderRefIdentity boundRef
                `shouldNotBe` typeBinderRefIdentity argumentRef
            step term `shouldBe` Just expected

        it "keeps a non-matching bounded InstApp on the general reduction path" $ do
            let resultRef = typeRef 959 "bounded-result"
                boundRef = typeRef 960 "bound-a"
                argumentRef = typeRef 961 "argument-a"
                boundTy =
                    TForallRef
                        boundRef
                        Nothing
                        (TArrow (TVarRef boundRef) (TVarRef boundRef))
                argumentTy =
                    TForallRef
                        argumentRef
                        Nothing
                        (TArrow (TVarRef argumentRef) intTy)
                occurrence = resolvedLocal "$x#959" "x" (TVarRef resultRef)
                abstraction =
                    eTyAbsWithRef
                        resultRef
                        (Just (boundFromType boundTy))
                        (EVarNode occurrence)
                term = ETyInst abstraction (InstApp argumentTy)
                expected =
                    ETyInst
                        abstraction
                        (InstSeq (InstInside (InstBot argumentTy)) InstElim)
            step term `shouldBe` Just expected

        it "does not beta-reduce value applications while reducing the leading type spine" $ do
            let ref = typeRef 954 "a"
                retainedApplication = EApp idLam (ELit (LInt 1))
                term = ETyInst (eTyAbsWithRef ref Nothing retainedApplication) InstElim
            reduceLeadingTypeInstantiationRedexes term
                `shouldBe` Just retainedApplication

        it "collects every deferred-case argument across a head type redex" $ do
            let ref = typeRef 955 "case-result"
                placeholder = mkTestDeferredVar "$deferred_case_Nat"
                scrutinee = mkTestDeferredVar "scrutinee"
                zeroHandler = mkTestDeferredVar "zero-handler"
                succHandler = mkTestDeferredVar "succ-handler"
                partialCase = EApp (EApp placeholder scrutinee) zeroHandler
                term =
                    EApp
                        (ETyInst (eTyAbsWithRef ref Nothing partialCase) InstElim)
                        succHandler
            collectApplicationSpineThroughHeadTypeRedexes term
                `shouldBe` (placeholder, [scrutinee, zeroHandler, succHandler])

        it "beta-reduces lambda applications" $ do
            let term = EApp idLam (ELit (LInt 1))
            step term `shouldBe` Just (ELit (LInt 1))

        it "beta-reduces resolved locals by identity instead of runtime spelling" $ do
            let binder = resolvedLocal "$x#0" "runtime-x" intTy
                occurrence = resolvedLocal "$x#0" "different-runtime" intTy
                term = EApp (ELam binder (EVarNode occurrence)) (ELit (LInt 1))
            step term `shouldBe` Just (ELit (LInt 1))

        it "does not treat equal local spellings as the same generated identity" $ do
            let (outerRef, gen1) = freshLocalRef "x" initialIdentityGenerator
                (innerRef, _) = freshLocalRef "x" gen1
                resolved ref _runtime =
                    ResolvedVar
                        {
                        resolvedVarType = intTy
                        , resolvedVarDetails = LocalId ref
                        }
                outer = resolved outerRef "outer-runtime"
                inner = resolved innerRef "inner-runtime"
                occurrence = resolved outerRef "outer-use-runtime"
                term = EApp (ELam outer (ELam inner (EVarNode occurrence))) (ELit (LInt 1))
            step term `shouldBe` Just (ELam inner (ELit (LInt 1)))

        it "freshens binder identity when capture avoidance collides by identity" $ do
            let (xRef, gen1) = freshLocalRef "x" initialIdentityGenerator
                (yRef, _) = freshLocalRef "y" gen1
                resolved ref _runtime =
                    ResolvedVar
                        {
                        resolvedVarType = intTy
                        , resolvedVarDetails = LocalId ref
                        }
                x = resolved xRef "x-runtime"
                yBinder = resolved yRef "inner-y-runtime"
                yReplacement = resolved yRef "free-y-runtime"
                term = EApp (ELam x (ELam yBinder (EVarNode x))) (EVarNode yReplacement)
            case step term of
                Just (ELam binder' (EVarNode occurrence')) -> do
                    resolvedVarSameIdentity occurrence' yReplacement `shouldBe` True
                    resolvedVarSameIdentity binder' yReplacement `shouldBe` False
                other -> expectationFailure ("Expected capture-avoiding identity freshening, got: " ++ show other)

        it "freshens away from resolved identity stable aliases during capture avoidance" $ do
            let x = resolvedLocal "x" "x-runtime" intTy
                yReplacement =
                    ResolvedVar
                        {
                        resolvedVarType = intTy
                        , resolvedVarDetails = LocalId (generatedLocalRef 1 "free-y")
                        }
                stableAlias = idDetailsStableName (resolvedVarDetails yReplacement)
                yBinder =
                    ResolvedVar
                        {
                        resolvedVarType = intTy
                        , resolvedVarDetails = LocalId (generatedLocalRef 1 stableAlias)
                        }
                term = EApp (ELam x (ELam yBinder (EVarNode x))) (EVarNode yReplacement)
            case step term of
                Just (ELam binder' (EVarNode occurrence')) -> do
                    resolvedVarSameIdentity occurrence' yReplacement `shouldBe` True
                    resolvedVarSameIdentity binder' yReplacement `shouldBe` False
                    resolvedVarReferenceName binder' `shouldNotBe` stableAlias
                other -> expectationFailure ("Expected stable-alias capture freshening, got: " ++ show other)

        it "reduces primitive and by resolved identity instead of runtime spelling" $ do
            let andResolved =
                    ResolvedVar
                        {
                        resolvedVarType = TArrow boolTy (TArrow boolTy boolTy)
                        , resolvedVarDetails =
                            PrimitiveId (primitiveRefFromSymbol (builtinValueIdentity PrimitiveInventory.nativeAndPrimitiveName))
                        }
                term = EApp (EApp (EVarNode andResolved) (ELit (LBool True))) (ELit (LBool False))
            step term `shouldBe` Just (ELit (LBool False))

        it "keeps same-named sidecar type refs distinct by identity during substitution" $ do
            let outerA = typeRef 80 "a"
                innerB = typeRef 81 "b"
                freeB = typeRef 82 "b"
                freeB1 = typeRef 83 "b1"
                occurrence = resolvedLocal "$x#0" "runtime-x" (TArrow (tVarWithRef outerA) (tVarWithRef freeB1))
                term =
                    ETyInst
                        ( eTyAbsWithRef outerA
                            (Just (TArrow (tVarWithRef freeB) TBottom))
                            (eTyAbsWithRef innerB Nothing (EVarNode occurrence))
                        )
                        InstElim
                expectedOccurrence =
                    occurrence
                        { resolvedVarType =
                            TArrow
                                (TArrow (tVarWithRef freeB) TBottom)
                                (tVarWithRef freeB1)
                        }
            step term `shouldBe` Just (eTyAbsWithRef innerB Nothing (EVarNode expectedOccurrence))

        it "freshens type abstraction identity during reduction capture avoidance" $ do
            let target = typeRef 89 "target"
                replacement = typeRef 90 "free"
                replacementTy = TArrow (tVarWithRef replacement) TBottom
                stableAlias = typeBinderIdentityStableName (typeBinderRefIdentity replacement)
                binder = typeBinderRefFromIdentity (typeBinderRefIdentity replacement) stableAlias
                occurrence = resolvedLocal "$x#0" "runtime-x" (tVarWithRef target)
                term =
                    ETyInst
                        (eTyAbsWithRef target (Just replacementTy) (eTyAbsWithRef binder Nothing (EVarNode occurrence)))
                        InstElim
                expectedOccurrence = occurrence { resolvedVarType = replacementTy }
            case step term of
                Just (ETyAbsRef ref' Nothing (EVarNode occurrence')) -> do
                    typeBinderRefIdentity ref' `shouldNotBe` typeBinderRefIdentity replacement
                    typeBinderRefName ref' `shouldNotBe` stableAlias
                    occurrence' `shouldBe` expectedOccurrence
                other -> expectationFailure ("Expected fresh type abstraction identity, got: " ++ show other)

        it "does not substitute same-named type refs with different identities" $ do
            let refA = typeRef 70 "a"
                refB = typeRef 71 "a"
                occurrence = resolvedLocal "$x#0" "runtime-x" (tVarWithRef refB)
                term = ETyInst (eTyAbsWithRef refA Nothing (EVarNode occurrence)) InstElim
            step term `shouldBe` Just (EVarNode occurrence)

        it "drops vacuous type abstractions by identity instead of spelling" $ do
            let refA = typeRef 72 "a"
                refB = typeRef 73 "a"
                body = mkTestLocalLam "x" (tVarWithRef refB) (mkTestDeferredVar "x")
                arg = ELit (LInt 1)
                term = EApp (eTyAbsWithRef refA Nothing body) arg
            step term `shouldBe` Just (EApp body arg)

        it "preserves retained-child boundaries by resolved identity instead of runtime spelling" $ do
            let identityTy = testTForall "a" Nothing (testTVar "a")
                identitySch = schemeFromType identityTy
                source = resolvedLocal "$source#0" "source-runtime" recursiveIntTy
                sourceUse = resolvedLocal "$source#0" "source-use-runtime" recursiveIntTy
                alias = resolvedLocal "$alias#0" "alias-runtime" identityTy
                aliasUse = resolvedLocal "$alias#0" "alias-use-runtime" identityTy
                child = resolvedLocal "$child#0" "child-runtime" identityTy
                childUse = resolvedLocal "$child#0" "child-use-runtime" identityTy
                boundaryParam = resolvedLocal "$p#0" "p-runtime" recursiveIntTy
                boundaryUse = resolvedLocal "$p#0" "p-use-runtime" recursiveIntTy
                boundary = ELam boundaryParam (EVarNode boundaryUse)
                childRhs = EApp boundary (EVarNode aliasUse)
                childLet = ELet child identitySch childRhs (EVarNode childUse)
                aliasLet = ELet alias identitySch (EVarNode sourceUse) childLet
                sourceRhs = ERoll recursiveIntTy recursiveBody
                term =
                    ELet
                        source
                        (schemeFromType recursiveIntTy)
                        sourceRhs
                        aliasLet
                expected =
                    ELet
                        source
                        (schemeFromType recursiveIntTy)
                        sourceRhs
                        (EVarNode sourceUse)
            preserveRetainedChildAuthoritativeResult term `shouldBe` Just expected

        it "does not treat equal local spellings as identity-boundary lambdas" $ do
            let (boundaryParamRef, gen1) = freshLocalRef "$p#0" initialIdentityGenerator
                (boundaryUseRef, _) = freshLocalRef "$p#0" gen1
                resolved ref _runtime ty =
                    ResolvedVar
                        {
                        resolvedVarType = ty
                        , resolvedVarDetails = LocalId ref
                        }
                source = resolvedLocal "$source#0" "source-runtime" recursiveIntTy
                sourceUse = resolvedLocal "$source#0" "source-use-runtime" recursiveIntTy
                boundary =
                    ELam
                        (resolved boundaryParamRef "p-runtime" recursiveIntTy)
                        (EVarNode (resolved boundaryUseRef "p-use-runtime" recursiveIntTy))
                term =
                    ELet
                        source
                        (schemeFromType recursiveIntTy)
                        (ERoll recursiveIntTy recursiveBody)
                        (EApp boundary (EVarNode sourceUse))
            preserveRetainedChildAuthoritativeResult term `shouldBe` Nothing

        it "reduces let bindings when the rhs is a value" $ do
            let term = mkTestLocalLet "x" (schemeFromType intTy) (ELit (LInt 1)) (mkTestDeferredVar "x")
            step term `shouldBe` Just (ELit (LInt 1))

        it "reduces instantiation elimination" $ do
            let term = ETyInst (mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))) InstElim
            step term `shouldBe` Just (mkTestLocalLam "x" TBottom (mkTestDeferredVar "x"))

        it "reduces instantiation introduction (O) to type abstraction" $ do
            let term = ETyInst (ELit (LInt 1)) InstIntro
            case step term of
                Just (ETyAbsRef ref Nothing (ELit (LInt 1)))
                    | typeBinderRefName ref == "u0" -> pure ()
                other -> expectationFailure ("Expected generated InstIntro abstraction, got: " ++ show other)

        it "reduces instantiation trans to nested instantiations" $ do
            let inst = InstSeq InstIntro InstElim
                term = ETyInst (ELit (LInt 1)) inst
            step term `shouldBe` Just (ETyInst (ETyInst (ELit (LInt 1)) InstIntro) InstElim)

        it "reduces instantiation under (∀(α ⩾) φ) by pushing under type abstraction" $ do
            let body = mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x")
                term = ETyInst (mkTestTyAbs "a" Nothing body) (instUnderWithRef (typeRef 249 "b") (InstApp intTy))
            step term `shouldBe` Just (mkTestTyAbs "a" Nothing (ETyInst body (InstApp intTy)))

        it "preserves type binder refs while reducing instantiation under" $ do
            let refA = typeRef 60 "a"
                refB = typeRef 61 "b"
                body = mkTestLocalLam "x" (tVarWithRef refA) (mkTestDeferredVar "x")
                term = ETyInst (eTyAbsWithRef refA Nothing body) (instUnderWithRef refB (InstApp intTy))
            step term `shouldBe` Just (eTyAbsWithRef refA Nothing (ETyInst body (InstApp intTy)))

        it "renames InstUnder occurrences to the target binder identity" $ do
            let refA = typeRef 62 "a"
                refB = typeRef 63 "b"
                body = mkTestDeferredVar "x"
                inst = InstInside (instAbstrWithRef refB)
                expectedInst = InstInside (instAbstrWithRef refA)
                term = ETyInst (eTyAbsWithRef refA Nothing body) (instUnderWithRef refB inst)
            step term `shouldBe` Just (eTyAbsWithRef refA Nothing (ETyInst body expectedInst))

        it "reduces instantiation inside (∀(⩾ φ)) by rewriting the bound" $ do
            let body = mkTestDeferredVar "x"
                term = ETyInst (mkTestTyAbs "a" Nothing body) (InstInside (InstBot intTy))
            step term `shouldBe` Just (mkTestTyAbs "a" (Just (boundFromType intTy)) body)

        it "reduces instantiation application (⟨τ⟩) to inside+elim" $ do
            let body = mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x")
                term = ETyInst (mkTestTyAbs "a" Nothing body) (InstApp intTy)
                expectedInst = InstSeq (InstInside (InstBot intTy)) InstElim
            step term `shouldBe` Just (ETyInst (mkTestTyAbs "a" Nothing body) expectedInst)

        it "reduces an at-bound flexible forall use constructed with N" $ do
            let identityTy = testTForall "a" Nothing (TArrow (testTVar "a") (testTVar "a"))
                body = mkTestLocalLam "x" (testTVar "result") (mkTestDeferredVar "x")
                term = mkTestTyAbs "result" (Just (boundFromType identityTy)) body
                reduced = mkTestLocalLam "x" identityTy (mkTestDeferredVar "x")
                instantiated = ETyInst term InstElim
                reducedTy = TArrow identityTy identityTy
            typeCheck instantiated `shouldBe` Right reducedTy
            normalize instantiated `shouldBe` reduced
            typeCheck (normalize instantiated) `shouldBe` Right reducedTy

        it "consumes a vacuous leading forall before applying a later forall" $ do
            let method =
                    mkTestTyAbs "ghost" Nothing $
                        mkTestTyAbs "b" Nothing $
                            mkTestLocalLam "flag" boolTy $
                                mkTestLocalLam "value" (testTVar "b") (mkTestDeferredVar "value")
                term =
                    EApp
                        ( EApp
                            (ETyInst (ETyInst method InstElim) (InstApp boolTy))
                            (ELit (LBool True))
                        )
                        (ELit (LBool False))
            typeCheck term `shouldBe` Right boolTy
            normalize term `shouldBe` ELit (LBool False)
            typeCheck (normalize term) `shouldBe` Right boolTy

        it "opens a later forall at its Gamma identity after consuming a vacuous prefix" $ do
            let ghostRef = typeRef 962 "ghost"
                sourceRef = typeRef 963 "source-a"
                openedRef =
                    typeBinderRefFromIdentity
                        (typeBinderRefIdentity sourceRef)
                        "gamma-a"
                sourceTy = TVarRef sourceRef
                openedTy = TVarRef openedRef
                flag = resolvedLocal "$flag#963" "flag" boolTy
                value = resolvedLocal "$value#963" "value" sourceTy
                producer =
                    eTyAbsWithRef ghostRef Nothing $
                        eTyAbsWithRef sourceRef Nothing $
                            ELam flag $
                                ELam value (EVarNode value)
                term =
                    ETyInst
                        producer
                        (InstSeq InstElim (InstApp openedTy))
                openedValue = value {resolvedVarType = openedTy}
                expected =
                    ELam flag $
                        ELam openedValue (EVarNode openedValue)
            reduceLeadingTypeInstantiationRedexes term
                `shouldBe` Just expected

        it "does not erase a consuming instantiation without a leading type abstraction" $ do
            let malformed = ETyInst (ELit (LInt 1)) InstElim
            step malformed `shouldBe` Nothing
            normalize malformed `shouldBe` malformed

        it "steps inside argument position under call-by-value" $ do
            let inner = EApp (mkTestLocalLam "y" intTy (mkTestDeferredVar "y")) (ELit (LInt 1))
                term = EApp idLam inner
            step term `shouldBe` Just (EApp idLam (ELit (LInt 1)))

        it "reduces recursive unroll-roll pairs" $ do
            step (EUnroll (ERoll recursiveIntTy recursiveBody)) `shouldBe` Just recursiveBody

        it "reduces inside recursive roll bodies before the outer constructor becomes a value" $ do
            let inner = EApp idLam (ELit (LInt 1))
            step (ERoll recursiveIntTy inner) `shouldBe` Just (ERoll recursiveIntTy (ELit (LInt 1)))

    describe "Phase 7 preservation (sanity)" $ do
        it "preserves types across normalization for a fixed set of terms" $ do
            let term1 = EApp idLam (ELit (LInt 1))
                term2 = mkTestLocalLet "x" (schemeFromType intTy) (ELit (LInt 1)) (EApp idLam (mkTestDeferredVar "x"))
                term3 = ETyInst (mkTestTyAbs "a" Nothing polyLam) InstElim
                terms = [term1, term2, term3]
            forM_ terms $ \term -> do
                ty <- requireRight (typeCheck term)
                let term' = normalize term
                ty' <- requireRight (typeCheck term')
                ty' `shouldBe` ty

    describe "Phase 7 bounded/coercion-heavy parity regressions (A6)" $ do
        it "normalization preserves parity for bounded-alias coercion path" $ do
            let rhs = Surf.ELam "x" (Surf.ELam "y" (Surf.EVar "x"))
                schemeTy =
                    mkForalls
                        [ ("a", Nothing)
                        , ("b", Just (Surf.STVar "a"))
                        ]
                        (Surf.STArrow (Surf.STVar "a") (Surf.STArrow (Surf.STVar "b") (Surf.STVar "a")))
                ann =
                    Surf.STForall "a" Nothing
                        (Surf.STArrow (Surf.STVar "a") (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a")))
                expr =
                    Surf.ELet "c" (Surf.EAnn rhs schemeTy)
                        (Surf.EAnn (Surf.EVar "c") ann)
                normExpr = unsafeNormalizeExpr expr
                isPolyBinaryId ty =
                    case ty of
                        TForallRef _ Nothing (TArrow dom (TArrow dom' cod)) ->
                            dom == dom' && dom' == cod
                        _ -> False

            canonicalRes <- case runPipelineElab Set.empty normExpr of
                Left err -> expectationFailure ("Canonical pipeline failed:\n" ++ renderPipelineError err) >> fail "canonical pipeline failed"
                Right out -> pure out

            let (canonicalTerm, canonicalTy) = canonicalRes
                canonicalNorm = normalize canonicalTerm

            canonicalTy `shouldSatisfy` isPolyBinaryId
            canonicalNormTy <- requireRight (typeCheck canonicalNorm)
            canonicalNormTy `shouldSatisfy` isPolyBinaryId

        it "normalizes dual annotated coercion consumers after InstApp specialization" $ do
            let useInt =
                    Surf.ELamAnn "f" (Surf.STArrow (Surf.STBase "Int") (Surf.STBase "Int"))
                        (Surf.EApp (Surf.EVar "f") (Surf.ELit (LInt 0)))
                useBool =
                    Surf.ELamAnn "f" (Surf.STArrow (Surf.STBase "Bool") (Surf.STBase "Bool"))
                        (Surf.EApp (Surf.EVar "f") (Surf.ELit (LBool True)))
                expr =
                    Surf.ELet "id" (Surf.ELam "x" (Surf.EVar "x"))
                        (Surf.ELet "useI" useInt
                            (Surf.ELet "useB" useBool
                                (Surf.ELet "_" (Surf.EApp (Surf.EVar "useI") (Surf.EVar "id"))
                                    (Surf.EApp (Surf.EVar "useB") (Surf.EVar "id")))))
                normExpr = unsafeNormalizeExpr expr

            (term, ty) <- requireRight (runPipelineElab Set.empty normExpr)
            ty `shouldBe` boolTy
            typeCheck term `shouldBe` Right ty
            typeCheck (normalize term) `shouldBe` Right ty
