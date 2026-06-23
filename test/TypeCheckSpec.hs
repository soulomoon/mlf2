{-# LANGUAGE GADTs #-}

-- Convention: Promote minimized QuickCheck counterexamples from
-- TypeSoundnessSpec / PipelineSpec into fixed regression tests here.
module TypeCheckSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..), NodeId(..))
import MLF.Elab.Pipeline
    ( XmlfTerm(..)
    , ElabType
    , Ty(..)
    , Env(..)
    , Instantiation(..)
    , TypeCheckError(..)
    , mkTypeCheckEnvWithResolvedTerms
    , resolvedTermEnvEntries
    , freshenTypeAbsAgainstEnv
    , renderPipelineError
    , restrictResolvedTermBindings
    , runPipelineElab
    , schemeFromType
    , typeCheck
    , typeCheckWithEnv
    , unionEnvs
    )
import MLF.Elab.Run.Pipeline.TestSupport
    ( PipelineElabDetailedResult(..)
    , prepareExternalBindings
    , preparedExternalTypeCheckEnv
    , restrictPreparedExternalBindings
    , runPipelineElabDetailedWithExternalBindings
    , unionPreparedExternalBindings
    )
import MLF.Elab.TermClosure
    ( alignTermTypeVarsToScheme
    , closeTermWithSchemeSubstRefsIfNeeded
    , substInTermRefs
    )
import MLF.Types.Elab
    ( ResolvedVar(..)
    , TypeBinderRef
    , deferredResolvedVarFromRef
    , eTyAbsWithRef
    , generatedIdentitiesInType
    , identityGeneratorAfterTerm
    , instAbstrWithRef
    , instUnderWithRef
    , mkDeferredVarWithRef
    , mkElabSchemeWithRefs
    , renameResolvedLocalVar
    , resolvedVarBoundBy
    , resolvedVarConstructorRef
    , resolvedVarIsLocal
    , resolvedVarReferenceName
    , schemeBinderRefs
    , tForallWithRef
    , tVarWithRef
    , typeBinderIdentityFromNode
    , typeBinderIdentityFromUnique
    , typeBinderIdentityKey
    , typeBinderRefFromIdentity
    , typeBinderRefIdentity
    , typeBinderRefName
    )
import qualified MLF.Types.Elab as ElabTypes
import MLF.Frontend.Symbol (SymbolIdentity(..), SymbolNamespace(..), SymbolOwnerIdentity(..))
import MLF.Frontend.ConstraintGen (ExternalBinding(..), ExternalBindingIdentity(..), ExternalBindingMode(..))
import MLF.Frontend.Program.Builtins (builtinTypeIdentity, builtinValueIdentity)
import MLF.Frontend.Program.Types (LoweredBindingIdentity(..))
import MLF.Frontend.Syntax (Lit(..))
import qualified MLF.Frontend.Syntax as Surf (Expr(..), SrcTy(..))
import MLF.Primitive.Inventory (stringLengthPrimitiveName)
import qualified MLF.Reify.TypeOps as TypeOps
import MLF.Types.Identity
    ( ConstructorRef(..)
    , DeferredRef(..)
    , EnvRef(..)
    , IdDetails(..)
    , LocalIdentity(..)
    , LocalRef(..)
    , PrimitiveRef(..)
    , UniqueIdentity(..)
    , freshDeferredRef
    , freshLocalRef
    , idDetailsConstructorRef
    , idDetailsIsLocal
    , idDetailsRenameLocal
    , idDetailsReferenceName
    , idDetailsSameIdentity
    , initialIdentityGenerator
    )
import ElabTermTestSupport
    ( generatedLocalRef
    , generatedResolvedLocal
    , mkTestDeferredVar
    , mkTestLocalLam
    , mkTestLocalLet
    , mkTestTyAbs
    , testTForall
    , testTMu
    , testTVar
    )
import SpecUtil (mkForalls, unsafeNormalizeExpr)

generatedSymbolIdentity :: Int -> SymbolNamespace -> String -> String -> Maybe SymbolOwnerIdentity -> SymbolIdentity
generatedSymbolIdentity unique namespace moduleName name owner =
    SymbolIdentity
        { symbolUniqueIdentity = UniqueIdentity unique
        , symbolNamespace = namespace
        , symbolDefiningModule = moduleName
        , symbolDefiningName = name
        , symbolOwnerIdentity = owner
        }

shouldBeRightAlphaEq :: Either TypeCheckError ElabType -> ElabType -> Expectation
shouldBeRightAlphaEq actual expected =
    case actual of
        Right ty | TypeOps.alphaEqType ty expected -> pure ()
        other -> other `shouldBe` Right expected

spec :: Spec
spec = describe "Phase 7 typecheck" $ do
    let intTy = TBase (BaseTy "Int")
        builtinIntTy =
            ElabTypes.TBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int")
        listSelfTy = testTMu "self" (TCon (BaseTy "List") (testTVar "self" :| []))
        bareRecursiveTy = testTMu "self" (testTVar "self")
        forallRecursiveTy = testTMu "self" (testTForall "b" Nothing (testTVar "self"))
        recursiveIntTy = testTMu "self" (TArrow (testTVar "self") intTy)
        boolTy = TBase (BaseTy "Bool")
        recursiveBody = mkTestLocalLam "self" recursiveIntTy (ELit (LInt 1))
        resolvedLocal ref runtime ty =
            generatedResolvedLocal 0 ref runtime ty
        typeRef :: Int -> String -> TypeBinderRef
        typeRef key name =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name

    describe "Formal obligations ledger anchors (Chapter 14 typing/instance)" $ do
        it "O14-WF-EMPTY O14-WF-TVAR O14-WF-VAR: environment well-formedness proxies" $ do
            typeCheck (ELit (LInt 0)) `shouldBe` Right intTy
            case typeCheck (mkTestTyAbs "a" (Just (TArrow (testTVar "a") intTy)) (ELit (LInt 1))) of
                Left (TCTypeAbsBoundMentionsVar "a") -> pure ()
                other -> expectationFailure ("Expected bound self-reference rejection, got: " ++ show other)
            case typeCheck (mkTestDeferredVar "missing") of
                Left (TCUnboundVar "missing") -> pure ()
                other -> expectationFailure ("Expected unbound variable rejection, got: " ++ show other)

        it "O14-T-VAR O14-T-ABS O14-T-APP O14-T-TABS O14-T-TAPP O14-T-LET: typing-rule anchors" $ do
            typeCheck (mkTestLocalLam "x" intTy (mkTestDeferredVar "x")) `shouldBe` Right (TArrow intTy intTy)
            typeCheck (EApp (mkTestLocalLam "x" intTy (mkTestDeferredVar "x")) (ELit (LInt 1))) `shouldBe` Right intTy
            typeCheck
                (mkTestLocalLet "x" (schemeFromType intTy) (ELit (LInt 1)) (mkTestDeferredVar "x"))
                `shouldBe` Right intTy
            let polyId = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
            typeCheck polyId `shouldBeRightAlphaEq` testTForall "a" Nothing (TArrow (testTVar "a") (testTVar "a"))
            typeCheck (ETyInst polyId (InstApp intTy)) `shouldBe` Right (TArrow intTy intTy)

        it "O14-INST-REFLEX O14-INST-TRANS O14-INST-BOT O14-INST-HYP O14-INST-INNER O14-INST-OUTER O14-INST-QUANT-ELIM O14-INST-QUANT-INTRO: instantiation-rule anchors" $ do
            let polyId = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
                boundArrow = TArrow TBottom TBottom
                polyOuterHyp =
                    mkTestTyAbs "a" (Just boundArrow) (mkTestLocalLam "x" TBottom (mkTestDeferredVar "x"))
                refX = typeRef 162 "x"
            typeCheck (ETyInst (ELit (LInt 1)) InstId) `shouldBe` Right intTy
            typeCheck (ETyInst (ELit (LInt 1)) (InstSeq InstIntro InstElim)) `shouldBe` Right intTy
            case typeCheck (ETyInst (ELit (LInt 1)) InstIntro) of
                Right (TForallRef ref Nothing body) -> do
                    typeBinderRefName ref `shouldBe` "u0"
                    typeBinderIdentityKey (typeBinderRefIdentity ref) `shouldBe` 0
                    body `shouldBe` intTy
                other -> expectationFailure ("Expected generated InstIntro forall, got: " ++ show other)
            typeCheck (ETyInst polyId InstElim) `shouldBe` Right (TArrow TBottom TBottom)
            typeCheck (ETyInst polyId (InstInside (InstBot intTy)))
                `shouldBeRightAlphaEq` testTForall "a" (Just intTy) (TArrow (testTVar "a") (testTVar "a"))
            typeCheck (ETyInst polyOuterHyp (instUnderWithRef refX (instAbstrWithRef refX)))
                `shouldBeRightAlphaEq` testTForall "a" (Just boundArrow) (testTVar "a")
            case typeCheck (ETyInst (ELit (LInt 1)) (InstBot intTy)) of
                Left TCInstantiationError{} -> pure ()
                other -> expectationFailure ("Expected InstBot rejection on non-bottom term type, got: " ++ show other)

    it "reports unbound variables" $ do
        case typeCheck (mkTestDeferredVar "x") of
            Left (TCUnboundVar "x") -> pure ()
            other -> expectationFailure ("Expected unbound variable error, got: " ++ show other)

    it "typechecks lambdas" $ do
        let term = mkTestLocalLam "x" intTy (mkTestDeferredVar "x")
        typeCheck term `shouldBe` Right (TArrow intTy intTy)

    it "typechecks resolved locals by identity instead of runtime spelling" $ do
        let binder = resolvedLocal "$x#0" "runtime-x" intTy
            occurrence = resolvedLocal "$x#0" "different-runtime" intTy
            term = ELam binder (EVarNode occurrence)
        typeCheck term `shouldBe` Right (TArrow intTy intTy)

    it "keeps same-spelled resolved local binders distinct by identity" $ do
        let outer = generatedResolvedLocal 0 "x" "runtime-x" intTy
            inner = generatedResolvedLocal 1 "x" "runtime-x" boolTy
            term = ELam outer (ELam inner (EVarNode outer))
        typeCheck term `shouldBe` Right (TArrow intTy (TArrow boolTy intTy))

    it "preserves same-spelled resolved identities across typecheck env union" $ do
        let outer = generatedResolvedLocal 0 "x" "runtime-x" intTy
            inner = generatedResolvedLocal 1 "x" "runtime-x" boolTy
            outerEnv = mkTypeCheckEnvWithResolvedTerms [(outer, intTy)] Map.empty
            innerEnv = mkTypeCheckEnvWithResolvedTerms [(inner, boolTy)] Map.empty
            merged = unionEnvs innerEnv outerEnv
        typeCheckWithEnv merged (EVarNode outer) `shouldBe` Right intTy
        typeCheckWithEnv merged (EVarNode inner) `shouldBe` Right boolTy

    it "restricts resolved term bindings by identity when requested" $ do
        let outer = generatedResolvedLocal 0 "x" "runtime-x" intTy
            inner = generatedResolvedLocal 1 "x" "runtime-x" boolTy
            outerEnv = mkTypeCheckEnvWithResolvedTerms [(outer, intTy)] Map.empty
            innerEnv = mkTypeCheckEnvWithResolvedTerms [(inner, boolTy)] Map.empty
            merged = unionEnvs innerEnv outerEnv
            restricted = restrictResolvedTermBindings [inner] merged
        typeCheckWithEnv restricted (EVarNode outer) `shouldBe` Left (TCUnboundVar "x")
        typeCheckWithEnv restricted (EVarNode inner) `shouldBe` Right boolTy

    it "rejects same-spelled top-level references with different explicit identity" $ do
        let topLevelResolved unique moduleName ty =
                ResolvedVar
                    { resolvedVarRuntimeName = "x"
                    , resolvedVarType = ty
                    , resolvedVarDetails =
                        TopLevelId (generatedSymbolIdentity unique SymbolValue moduleName "x" Nothing)
                    }
            actual = topLevelResolved 10 "Actual" intTy
            stale = topLevelResolved 11 "Stale" intTy
            env = mkTypeCheckEnvWithResolvedTerms [(actual, intTy)] Map.empty
        typeCheckWithEnv env (EVarNode actual) `shouldBe` Right intTy
        typeCheckWithEnv env (EVarNode stale) `shouldBe` Left (TCUnboundVar "x")

    it "does not resolve top-level identity through a same-named environment identity" $ do
        let envResolved =
                ResolvedVar
                    { resolvedVarRuntimeName = "x"
                    , resolvedVarType = intTy
                    , resolvedVarDetails =
                        EnvId
                            EnvRef
                                { envRefIdentity = UniqueIdentity 12
                                , envRefName = "x"
                                }
                    }
            resolved =
                ResolvedVar
                    { resolvedVarRuntimeName = "x"
                    , resolvedVarType = intTy
                    , resolvedVarDetails =
                        TopLevelId (generatedSymbolIdentity 12 SymbolValue "Actual" "x" Nothing)
                    }
            env = mkTypeCheckEnvWithResolvedTerms [(envResolved, intTy)] Map.empty
        typeCheckWithEnv env (EVarNode resolved) `shouldBe` Left (TCUnboundVar "x")

    it "does not resolve stale environment identity through a same-named environment identity" $ do
        let envResolved identity =
                ResolvedVar
                    { resolvedVarRuntimeName = "x"
                    , resolvedVarType = intTy
                    , resolvedVarDetails =
                        EnvId
                            EnvRef
                                { envRefIdentity = UniqueIdentity identity
                                , envRefName = "x"
                                }
                    }
            actual = envResolved 0
            stale = envResolved 1
            env = mkTypeCheckEnvWithResolvedTerms [(actual, intTy)] Map.empty
        typeCheckWithEnv env (EVarNode actual) `shouldBe` Right intTy
        typeCheckWithEnv env (EVarNode stale) `shouldBe` Left (TCUnboundVar "x")

    it "seeds fresh identities after environment identities already present in terms" $ do
        let envResolved identity =
                ResolvedVar
                    { resolvedVarRuntimeName = "external"
                    , resolvedVarType = intTy
                    , resolvedVarDetails =
                        EnvId
                            EnvRef
                                { envRefIdentity = UniqueIdentity identity
                                , envRefName = "external"
                                }
                    }
            term = EVarNode (envResolved 4)
            (freshRef, _) = freshLocalRef "local" (identityGeneratorAfterTerm term)
        localRefIdentity freshRef `shouldBe` GeneratedLocalId (UniqueIdentity 5)

    it "seeds fresh identities after top-level symbol identities already present in terms" $ do
        let topLevelResolved identity =
                ResolvedVar
                    { resolvedVarRuntimeName = "external"
                    , resolvedVarType = intTy
                    , resolvedVarDetails =
                        TopLevelId
                            SymbolIdentity
                                { symbolUniqueIdentity = UniqueIdentity identity
                                , symbolNamespace = SymbolValue
                                , symbolDefiningModule = "Actual"
                                , symbolDefiningName = "external"
                                , symbolOwnerIdentity = Nothing
                                }
                    }
            term = EVarNode (topLevelResolved 4)
            (freshRef, _) = freshLocalRef "local" (identityGeneratorAfterTerm term)
        localRefIdentity freshRef `shouldBe` GeneratedLocalId (UniqueIdentity 5)

    it "seeds fresh identities after owner symbol identities already present in terms" $ do
        let classIdentity =
                SymbolIdentity
                    { symbolUniqueIdentity = UniqueIdentity 7
                    , symbolNamespace = SymbolClass
                    , symbolDefiningModule = "Actual"
                    , symbolDefiningName = "Show"
                    , symbolOwnerIdentity = Nothing
                    }
            methodIdentity =
                SymbolIdentity
                    { symbolUniqueIdentity = UniqueIdentity 4
                    , symbolNamespace = SymbolMethod
                    , symbolDefiningModule = "Actual"
                    , symbolDefiningName = "show"
                    , symbolOwnerIdentity = Just (SymbolOwnerClass classIdentity)
                    }
            term =
                EVarNode
                    ResolvedVar
                        { resolvedVarRuntimeName = "show"
                        , resolvedVarType = intTy
                        , resolvedVarDetails = MethodId methodIdentity
                        }
            (freshRef, _) = freshLocalRef "local" (identityGeneratorAfterTerm term)
        localRefIdentity freshRef `shouldBe` GeneratedLocalId (UniqueIdentity 8)

    it "seeds fresh identities after deferred identities already present in terms" $ do
        let (deferredRef, _) = freshDeferredRef "deferred" initialIdentityGenerator
            term = mkDeferredVarWithRef deferredRef
            (freshRef, _) = freshLocalRef "local" (identityGeneratorAfterTerm term)
        localRefIdentity freshRef `shouldBe` GeneratedLocalId (UniqueIdentity 1)

    it "resolves generated deferred identities by identity, not by name" $ do
        let (actualRef, gen1) = freshDeferredRef "x" initialIdentityGenerator
            (staleRef, _) = freshDeferredRef "x" gen1
            actual = deferredResolvedVarFromRef actualRef
            env = mkTypeCheckEnvWithResolvedTerms [(actual, intTy)] Map.empty
        idDetailsSameIdentity (DeferredId actualRef) (DeferredId actualRef) `shouldBe` True
        idDetailsSameIdentity (DeferredId actualRef) (DeferredId staleRef) `shouldBe` False
        typeCheckWithEnv env (mkDeferredVarWithRef actualRef) `shouldBe` Right intTy
        typeCheckWithEnv env (mkDeferredVarWithRef staleRef) `shouldBe` Left (TCUnboundVar "x")

    it "keeps generated deferred identities out of local binder identity checks" $ do
        let binder = generatedResolvedLocal 0 "x" "x" intTy
            (deferredRef, _) = freshDeferredRef "x" initialIdentityGenerator
            generatedDeferred = deferredResolvedVarFromRef deferredRef
        resolvedVarBoundBy [binder] generatedDeferred `shouldBe` False
        resolvedVarDetails generatedDeferred `shouldBe` DeferredId deferredRef

    it "preserves external binding identity in prepared typecheck environments" $ do
        let symbol unique moduleName =
                generatedSymbolIdentity unique SymbolValue moduleName "x" Nothing
            topLevelResolved unique moduleName =
                ResolvedVar
                    { resolvedVarRuntimeName = "x"
                    , resolvedVarType = builtinIntTy
                    , resolvedVarDetails = TopLevelId (symbol unique moduleName)
                    }
            externalIdentity =
                ExternalBindingIdentity
                    { externalBindingDisplayName = "x"
                    , externalBindingRuntimeName = "x"
                    , externalBindingDetails = TopLevelId (symbol 20 "Actual")
                    }
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = Just externalIdentity
                    }
        case prepareExternalBindings (Map.singleton "x" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared -> do
                let env = preparedExternalTypeCheckEnv prepared
                typeCheckWithEnv env (EVarNode (topLevelResolved 20 "Actual")) `shouldBe` Right builtinIntTy
                typeCheckWithEnv env (EVarNode (topLevelResolved 21 "Stale")) `shouldBe` Left (TCUnboundVar "x")

    it "preserves builtin type identities in prepared external binding types" $ do
        let externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = Nothing
                    }
        case prepareExternalBindings (Map.singleton "x" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "x" ] of
                    [ty@(TBase (BaseTy "Int"))] ->
                        generatedIdentitiesInType ty `shouldSatisfy` elem (symbolUniqueIdentity (builtinTypeIdentity "Int"))
                    other -> expectationFailure ("Expected Int identity in prepared external binding, got: " ++ show other)

    it "restricts prepared external typecheck bindings by identity after same-name union" $ do
        let symbol unique moduleName =
                generatedSymbolIdentity unique SymbolValue moduleName "x" Nothing
            topLevelResolved unique moduleName =
                ResolvedVar
                    { resolvedVarRuntimeName = "x"
                    , resolvedVarType = builtinIntTy
                    , resolvedVarDetails = TopLevelId (symbol unique moduleName)
                    }
            externalBinding unique moduleName =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity =
                        Just
                            ExternalBindingIdentity
                                { externalBindingDisplayName = "x"
                                , externalBindingRuntimeName = "x"
                                , externalBindingDetails = TopLevelId (symbol unique moduleName)
                                }
                    }
            prepareOne unique moduleName =
                prepareExternalBindings (Map.singleton "x" (externalBinding unique moduleName))
        case (prepareOne 22 "Preferred", prepareOne 23 "Fallback") of
            (Right preferred, Right fallback) -> do
                let restricted =
                        restrictPreparedExternalBindings
                            (Set.singleton "x")
                            (preferred `unionPreparedExternalBindings` fallback)
                    env = preparedExternalTypeCheckEnv restricted
                typeCheckWithEnv env (EVarNode (topLevelResolved 22 "Preferred")) `shouldBe` Right builtinIntTy
                typeCheckWithEnv env (EVarNode (topLevelResolved 23 "Fallback")) `shouldBe` Left (TCUnboundVar "x")
            (Left err, _) -> expectationFailure ("Expected preferred external binding preparation, got: " ++ show err)
            (_, Left err) -> expectationFailure ("Expected fallback external binding preparation, got: " ++ show err)

    it "assigns generated identities to free type variables in external binding types" $ do
        let externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STArrow (Surf.STVar "a") (Surf.STVar "a")
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = Nothing
                    }
        case prepareExternalBindings (Map.singleton "id" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "id" ] of
                    [TArrow (TVarRef argRef) (TVarRef resultRef)] -> do
                        typeBinderRefName argRef `shouldBe` "a"
                        typeBinderRefIdentity argRef `shouldBe` typeBinderIdentityFromUnique (UniqueIdentity 0)
                        typeBinderRefIdentity resultRef `shouldBe` typeBinderRefIdentity argRef
                    other -> expectationFailure ("Expected identity type with generated refs, got: " ++ show other)

    it "seeds generated external typecheck identities after provided deferred identities" $ do
        let (deferredRef, _) = freshDeferredRef "z" initialIdentityGenerator
            deferredIdentity =
                ExternalBindingIdentity
                    { externalBindingDisplayName = "z"
                    , externalBindingRuntimeName = "z"
                    , externalBindingDetails = DeferredId deferredRef
                    }
            externalBindings =
                Map.fromList
                    [ ( "a"
                      , ExternalBinding
                            { externalBindingType = Surf.STBase "Int"
                            , externalBindingMode = ExternalBindingScheme
                            , externalBindingIdentity = Nothing
                            }
                      )
                    , ( "z"
                      , ExternalBinding
                            { externalBindingType = Surf.STBase "Int"
                            , externalBindingMode = ExternalBindingScheme
                            , externalBindingIdentity = Just deferredIdentity
                            }
                      )
                    ]
        case prepareExternalBindings externalBindings of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared -> do
                let env = preparedExternalTypeCheckEnv prepared
                    generatedRefs =
                        [ ref
                        | (resolved, _) <- resolvedTermEnvEntries (resolvedTermEnv env)
                        , resolvedVarReferenceName resolved == "a"
                        , EnvId ref <- [resolvedVarDetails resolved]
                        ]
                map envRefIdentity generatedRefs `shouldBe` [UniqueIdentity 1]
                typeCheckWithEnv env (mkDeferredVarWithRef deferredRef) `shouldBe` Right builtinIntTy

    it "seeds generated external elaboration identities after provided deferred identities" $ do
        let (deferredRef, _) = freshDeferredRef "z" initialIdentityGenerator
            deferredIdentity =
                ExternalBindingIdentity
                    { externalBindingDisplayName = "z"
                    , externalBindingRuntimeName = "z"
                    , externalBindingDetails = DeferredId deferredRef
                    }
            externalBindings =
                Map.fromList
                    [ ( "a"
                      , ExternalBinding
                            { externalBindingType = Surf.STBase "Int"
                            , externalBindingMode = ExternalBindingScheme
                            , externalBindingIdentity = Nothing
                            }
                      )
                    , ( "z"
                      , ExternalBinding
                            { externalBindingType = Surf.STBase "Int"
                            , externalBindingMode = ExternalBindingScheme
                            , externalBindingIdentity = Just deferredIdentity
                            }
                      )
                    ]
        case runPipelineElabDetailedWithExternalBindings
            Set.empty
            externalBindings
            (unsafeNormalizeExpr (Surf.EVar "a")) of
            Left err -> expectationFailure ("Expected external binding elaboration, got: " ++ renderPipelineError err)
            Right PipelineElabDetailedResult {pedTerm = EVarNode resolved, pedType = ty} -> do
                ty `shouldBe` builtinIntTy
                resolvedVarDetails resolved `shouldBe` EnvId (EnvRef {envRefIdentity = UniqueIdentity 1, envRefName = "a"})
            Right other -> expectationFailure ("Expected resolved external variable term, got: " ++ show (pedTerm other))

    it "elaborates external binding references with prepared identity" $ do
        let symbol =
                generatedSymbolIdentity 30 SymbolValue "Actual" "x" Nothing
            externalIdentity =
                ExternalBindingIdentity
                    { externalBindingDisplayName = "x"
                    , externalBindingRuntimeName = "x"
                    , externalBindingDetails = TopLevelId symbol
                    }
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = Just externalIdentity
                    }
        case runPipelineElabDetailedWithExternalBindings
            Set.empty
            (Map.singleton "x" externalBinding)
            (unsafeNormalizeExpr (Surf.EVar "x")) of
            Left err -> expectationFailure ("Expected external binding elaboration, got: " ++ renderPipelineError err)
            Right PipelineElabDetailedResult {pedTerm = EVarNode resolved, pedType = ty} -> do
                ty `shouldBe` builtinIntTy
                resolvedVarDetails resolved `shouldBe` TopLevelId symbol
            Right other -> expectationFailure ("Expected resolved external variable term, got: " ++ show (pedTerm other))

    it "projects resolved identity details from the identity layer" $ do
        let typeIdentity =
                generatedSymbolIdentity 40 SymbolType "Main" "Box" Nothing
            ctorIdentity =
                generatedSymbolIdentity 41 SymbolConstructor "Main" "Box" (Just (SymbolOwnerType typeIdentity))
            ctorRef =
                ConstructorRef
                    { constructorRefSymbol = ctorIdentity
                    }
            localDetails = LocalId (generatedLocalRef 0 "$x#0")
            envDetails =
                EnvId
                    EnvRef
                        { envRefIdentity = UniqueIdentity 0
                        , envRefName = "external-x"
                        }
            primitiveDetails =
                PrimitiveId
                    PrimitiveRef
                        { primitiveRefSymbol = builtinValueIdentity stringLengthPrimitiveName
                        }
            renamedPrimitiveDetails =
                PrimitiveId
                    PrimitiveRef
                        { primitiveRefSymbol = builtinValueIdentity stringLengthPrimitiveName
                        }
            deferredDetails = DeferredId (DeferredRef (UniqueIdentity 50) "x")
            sameNamedDeferredDetails = DeferredId (DeferredRef (UniqueIdentity 51) "x")
            constructorDetails = ConstructorId ctorRef
            loweredIdentity =
                LoweredBindingIdentity
                    { loweredIdentityRuntimeName = "$Box"
                    , loweredIdentityDetails = constructorDetails
                    }
            renamedLoweredIdentity =
                loweredIdentity
                    { loweredIdentityRuntimeName = "$RenamedBox"
                    }
            localResolved =
                ResolvedVar
                    { resolvedVarRuntimeName = "runtime-x"
                    , resolvedVarType = intTy
                    , resolvedVarDetails = localDetails
                    }
            envResolved =
                ResolvedVar
                    { resolvedVarRuntimeName = "runtime-env"
                    , resolvedVarType = intTy
                    , resolvedVarDetails = envDetails
                    }
            constructorResolved =
                ResolvedVar
                    { resolvedVarRuntimeName = "$Box"
                    , resolvedVarType = intTy
                    , resolvedVarDetails = constructorDetails
                    }
            renamedLocalResolved =
                localResolved
                    { resolvedVarRuntimeName = "renamed-runtime-x"
                    }
            staleTypedLocalResolved =
                localResolved {resolvedVarType = boolTy}
        idDetailsReferenceName "runtime-x" localDetails `shouldBe` "$x#0"
        idDetailsReferenceName "runtime-env" envDetails `shouldBe` "external-x"
        idDetailsReferenceName "runtime-prim" primitiveDetails `shouldBe` "runtime-prim"
        idDetailsReferenceName "runtime-deferred" deferredDetails `shouldBe` "x"
        idDetailsIsLocal localDetails `shouldBe` True
        idDetailsIsLocal envDetails `shouldBe` False
        idDetailsIsLocal primitiveDetails `shouldBe` False
        idDetailsConstructorRef localDetails `shouldBe` Nothing
        idDetailsConstructorRef constructorDetails `shouldBe` Just ctorRef
        renamedLoweredIdentity `shouldBe` loweredIdentity
        idDetailsReferenceName "runtime-x" (idDetailsRenameLocal "$x#1" localDetails) `shouldBe` "$x#1"
        idDetailsSameIdentity localDetails (idDetailsRenameLocal "$x#1" localDetails) `shouldBe` True
        idDetailsSameIdentity localDetails (LocalId (generatedLocalRef 1 "$x#1")) `shouldBe` False
        idDetailsSameIdentity constructorDetails (ConstructorId ctorRef) `shouldBe` True
        idDetailsSameIdentity primitiveDetails renamedPrimitiveDetails `shouldBe` True
        idDetailsSameIdentity deferredDetails sameNamedDeferredDetails `shouldBe` False
        idDetailsRenameLocal "$x#1" constructorDetails `shouldBe` constructorDetails
        renamedLocalResolved `shouldBe` localResolved
        staleTypedLocalResolved `shouldNotBe` localResolved
        resolvedVarReferenceName localResolved `shouldBe` "$x#0"
        resolvedVarReferenceName envResolved `shouldBe` "external-x"
        resolvedVarReferenceName constructorResolved `shouldBe` "$Box"
        resolvedVarIsLocal localResolved `shouldBe` True
        resolvedVarIsLocal envResolved `shouldBe` False
        resolvedVarConstructorRef constructorResolved `shouldBe` Just ctorRef
        resolvedVarRuntimeName (renameResolvedLocalVar "$x#1" localResolved) `shouldBe` "$x#1"
        renameResolvedLocalVar "$x#1" localResolved
            `shouldBe` localResolved
                { resolvedVarRuntimeName = "$x#1"
                , resolvedVarDetails = idDetailsRenameLocal "$x#1" localDetails
                }
        renameResolvedLocalVar "$Box1" constructorResolved `shouldBe` constructorResolved

    it "rejects resolved locals whose occurrence type is stale" $ do
        let binder = resolvedLocal "$x#0" "runtime-x" intTy
            occurrence = resolvedLocal "$x#0" "different-runtime" boolTy
            term = ELam binder (EVarNode occurrence)
        typeCheck term `shouldBe` Left (TCResolvedVarTypeMismatch "$x#0" intTy boolTy)

    it "preserves resolved locals while substituting internal type names" $ do
        let refA = typeRef 0 "a"
            binder = resolvedLocal "$x#0" "runtime-x" (testTVar "t0")
            occurrence = resolvedLocal "$x#0" "different-runtime" (testTVar "t0")
            term = substInTermRefs (IntMap.singleton 0 refA) (ELam binder (EVarNode occurrence))
        case term of
            ELam binder' (EVarNode occurrence') -> do
                resolvedVarType binder' `shouldBe` tVarWithRef refA
                resolvedVarType occurrence' `shouldBe` tVarWithRef refA
                resolvedVarDetails binder' `shouldBe` LocalId (generatedLocalRef 0 "$x#0")
                resolvedVarRuntimeName occurrence' `shouldBe` "different-runtime"
            other -> expectationFailure ("Expected resolved term after substitution, got: " ++ show other)

    it "does not substitute identity-bearing type refs by stale parsed names" $ do
        let refA = typeRef 0 "a"
            staleRef = typeRef 1 "t0"
            binder = resolvedLocal "$x#0" "runtime-x" (tVarWithRef staleRef)
            term = substInTermRefs (IntMap.singleton 0 refA) (ELam binder (EVarNode binder))
        case term of
            ELam binder' (EVarNode occurrence') -> do
                resolvedVarType binder' `shouldBe` tVarWithRef staleRef
                resolvedVarType occurrence' `shouldBe` tVarWithRef staleRef
            other -> expectationFailure ("Expected stale identity-bearing ref to remain, got: " ++ show other)

    it "does not substitute generated identity type refs by stale parsed names" $ do
        let refA = typeRef 0 "a"
            staleRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromUnique (UniqueIdentity 991602))
                    "t0"
            binder = resolvedLocal "$x#0" "runtime-x" (tVarWithRef staleRef)
            term = substInTermRefs (IntMap.singleton 0 refA) (ELam binder (EVarNode binder))
        case term of
            ELam binder' (EVarNode occurrence') -> do
                resolvedVarType binder' `shouldBe` tVarWithRef staleRef
                resolvedVarType occurrence' `shouldBe` tVarWithRef staleRef
            other -> expectationFailure ("Expected generated identity ref to remain, got: " ++ show other)

    it "attaches graph identities while substituting internal type names" $ do
        let refA = typeRef 0 "a"
            refT0 = typeRef 0 "t0"
            binder0 = resolvedLocal "$x#0" "runtime-x" (testTForall "t0" Nothing (testTVar "t0"))
            binder1 =
                resolvedLocal
                    "$x#0"
                    "runtime-x"
                    (tForallWithRef refA Nothing (tVarWithRef refA))
            term0 =
                mkTestTyAbs "t0"
                    Nothing
                    (ETyInst (ELam binder0 (EVarNode binder0)) (instUnderWithRef refT0 (instAbstrWithRef refT0)))
            expected =
                eTyAbsWithRef
                    refA
                    Nothing
                    (ETyInst (ELam binder1 (EVarNode binder1)) (instUnderWithRef refA (instAbstrWithRef refA)))
        substInTermRefs (IntMap.singleton 0 refA) term0 `shouldBe` expected

    it "preserves type binder refs while substituting term types" $ do
        let refA = typeRef 40 "a"
            refAbs = typeRef 41 "abs"
            refInst = typeRef 42 "inst"
            binder =
                resolvedLocal
                    "$x#0"
                    "runtime-x"
                    (tForallWithRef refA Nothing (tVarWithRef refA))
            term =
                eTyAbsWithRef
                    refAbs
                    Nothing
                    (ETyInst (ELam binder (EVarNode binder)) (instUnderWithRef refInst InstId))
        substInTermRefs IntMap.empty term `shouldBe` term

    it "preserves type binder refs while aligning term type variables" $ do
        let refA = typeRef 43 "a"
            binder = resolvedLocal "$x#0" "runtime-x" (tVarWithRef refA)
            term = eTyAbsWithRef refA Nothing (ELam binder (EVarNode binder))
            scheme =
                schemeFromType
                    (tForallWithRef
                        refA
                        Nothing
                        (TArrow (tVarWithRef refA) (tVarWithRef refA))
                    )
        alignTermTypeVarsToScheme scheme term `shouldBe` Just term

    it "preserves type binder refs when deriving schemes from types" $ do
        let refA = typeRef 44 "a"
            scheme = schemeFromType (tForallWithRef refA Nothing (tVarWithRef refA))
        map fst (schemeBinderRefs scheme) `shouldBe` [refA]

    it "checks type abstraction bounds by binder identity instead of spelling" $ do
        let refAbs = typeRef 45 "a"
            refOther = typeRef 46 "a"
            bound = TArrow (tVarWithRef refOther) intTy
            term = eTyAbsWithRef refAbs (Just bound) (ELit (LInt 1))
        typeCheck term `shouldBe` Right (tForallWithRef refAbs (Just bound) intTy)

    it "freshens earlier scheme binders through later bounds by identity" $ do
        let refA = typeRef 47 "a"
            refB = typeRef 48 "b"
            reservedA = typeRef 49 "a"
            param = resolvedLocal "$x#0" "runtime-x" (tVarWithRef reservedA)
            scheme =
                schemeFromType
                    ( tForallWithRef
                        refA
                        Nothing
                        (tForallWithRef refB (Just (TArrow (tVarWithRef refA) intTy)) intTy)
                    )
            term = ELam param (ELit (LInt 1))
            closed = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term
        case closed of
            ETyAbsRef outer Nothing (ETyAbsRef inner (Just bound) (ELam param' (ELit (LInt 1)))) -> do
                typeBinderRefIdentity outer `shouldBe` typeBinderRefIdentity refA
                typeBinderRefName outer `shouldBe` "a1"
                typeBinderRefIdentity inner `shouldBe` typeBinderRefIdentity refB
                typeBinderRefName inner `shouldBe` "b"
                bound `shouldBe` TArrow (tVarWithRef outer) intTy
                resolvedVarType param' `shouldBe` tVarWithRef reservedA
                param' `shouldBe` param
            other -> expectationFailure ("Expected freshened two-binder closure, got: " ++ show other)

    it "closes scheme refs through same-named term type abstractions by identity" $ do
        let refA = typeRef 53 "a"
            inner = typeRef 54 "a"
            param = resolvedLocal "$x#0" "runtime-x" (tVarWithRef refA)
            scheme =
                mkElabSchemeWithRefs
                    [(refA, Nothing)]
                    (tForallWithRef inner Nothing (TArrow (tVarWithRef refA) intTy))
            term = eTyAbsWithRef inner Nothing (ELam param (ELit (LInt 1)))
            closed = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term
        case closed of
            ETyAbsRef outer Nothing (ETyAbsRef inner' Nothing (ELam param' (ELit (LInt 1)))) -> do
                typeBinderRefIdentity outer `shouldBe` typeBinderRefIdentity refA
                typeBinderRefName outer `shouldBe` "a1"
                inner' `shouldBe` inner
                resolvedVarType param' `shouldBe` tVarWithRef outer
            other -> expectationFailure ("Expected same-named nested closure, got: " ++ show other)

    it "freshens same-named scheme binders independently by identity" $ do
        let refA = typeRef 55 "a"
            refB = typeRef 56 "a"
            paramTy = TArrow (tVarWithRef refA) (tVarWithRef refB)
            param = resolvedLocal "$x#0" "runtime-x" paramTy
            scheme =
                mkElabSchemeWithRefs
                    [(refA, Nothing), (refB, Nothing)]
                    (TArrow paramTy paramTy)
            term = ELam param (EVarNode param)
            closed = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term
        case closed of
            ETyAbsRef outer Nothing (ETyAbsRef inner Nothing (ELam param' (EVarNode occurrence'))) -> do
                typeBinderRefIdentity outer `shouldBe` typeBinderRefIdentity refA
                typeBinderRefName outer `shouldBe` "a1"
                typeBinderRefIdentity inner `shouldBe` typeBinderRefIdentity refB
                typeBinderRefName inner `shouldBe` "a2"
                let expectedParamTy = TArrow (tVarWithRef outer) (tVarWithRef inner)
                resolvedVarType param' `shouldBe` expectedParamTy
                resolvedVarType occurrence' `shouldBe` expectedParamTy
            other -> expectationFailure ("Expected independently freshened same-named scheme binders, got: " ++ show other)

    it "freshens type abstractions through same-named distinct binders by identity" $ do
        let outer = typeRef 50 "a"
            inner = typeRef 51 "a"
            reserved = typeRef 52 "a"
            captured = resolvedLocal "$env#0" "runtime-env" (tVarWithRef reserved)
            env = mkTypeCheckEnvWithResolvedTerms [(captured, tVarWithRef reserved)] Map.empty
            param =
                resolvedLocal
                    "$x#0"
                    "runtime-x"
                    (TArrow (tVarWithRef outer) (tVarWithRef inner))
            term =
                eTyAbsWithRef
                    outer
                    Nothing
                    (eTyAbsWithRef inner Nothing (ELam param (EVarNode param)))
        case freshenTypeAbsAgainstEnv env term of
            ETyAbsRef outer' Nothing (ETyAbsRef inner' Nothing (ELam param' (EVarNode occurrence'))) -> do
                typeBinderRefIdentity outer' `shouldBe` typeBinderRefIdentity outer
                typeBinderRefName outer' `shouldBe` "a1"
                typeBinderRefIdentity inner' `shouldBe` typeBinderRefIdentity inner
                typeBinderRefName inner' `shouldBe` "a2"
                let expectedTy = TArrow (tVarWithRef outer') (tVarWithRef inner')
                resolvedVarType param' `shouldBe` expectedTy
                resolvedVarType occurrence' `shouldBe` expectedTy
            other -> expectationFailure ("Expected nested freshened type abstractions, got: " ++ show other)

    it "typechecks applications" $ do
        let term = EApp (mkTestLocalLam "x" intTy (mkTestDeferredVar "x")) (ELit (LInt 1))
        typeCheck term `shouldBe` Right intTy

    it "typechecks let bindings" $ do
        let term = mkTestLocalLet "x" (schemeFromType intTy) (ELit (LInt 1)) (mkTestDeferredVar "x")
        typeCheck term `shouldBe` Right intTy

    it "accepts let schemes instantiated by matching free type variables only" $ do
        let refA = typeRef 80 "a"
            freeA = tVarWithRef refA
            schTy = tForallWithRef refA Nothing (TArrow freeA freeA)
            scheme = schemeFromType schTy
            idBinder = generatedResolvedLocal 81 "$id" "id" schTy
            freeParam = generatedResolvedLocal 82 "$x" "x" freeA
            concreteParam = generatedResolvedLocal 83 "$x" "x" intTy
            freeRhs = ELam freeParam (EVarNode freeParam)
            concreteRhs = ELam concreteParam (EVarNode concreteParam)
        typeCheck (ELet idBinder scheme freeRhs (EVarNode idBinder)) `shouldBe` Right schTy
        typeCheck (ELet idBinder scheme concreteRhs (EVarNode idBinder))
            `shouldBe` Left (TCLetTypeMismatch (TArrow intTy intTy) schTy)

    it "typechecks type abstractions" $ do
        let term = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
        case typeCheck term of
            Right (TForallRef ref Nothing (TArrow (TVarRef argRef) (TVarRef resultRef))) -> do
                typeBinderRefName ref `shouldBe` "a"
                argRef `shouldBe` ref
                resultRef `shouldBe` ref
            other -> expectationFailure ("Expected identity-backed type abstraction type, got: " ++ show other)

    it "typechecks instantiations" $ do
        let term = ETyInst (mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))) (InstApp intTy)
        typeCheck term `shouldBe` Right (TArrow intTy intTy)

    it "typechecks internal recursive roll/unroll runtime terms" $ do
        typeCheck (ERoll recursiveIntTy recursiveBody) `shouldBe` Right recursiveIntTy
        typeCheck (EUnroll (ERoll recursiveIntTy recursiveBody))
            `shouldBe` Right (TArrow recursiveIntTy intTy)

    it "accepts guarded recursive types in annotations and instantiation arguments" $ do
        let polyId = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
        typeCheck (mkTestLocalLam "x" listSelfTy (mkTestDeferredVar "x"))
            `shouldBe` Right (TArrow listSelfTy listSelfTy)
        typeCheck (ETyInst polyId (InstApp listSelfTy))
            `shouldBe` Right (TArrow listSelfTy listSelfTy)

    it "rejects malformed recursive roll/unroll runtime terms" $ do
        case typeCheck (ERoll recursiveIntTy (ELit (LInt 1))) of
            Left TCRollBodyMismatch{} -> pure ()
            other -> expectationFailure ("Expected recursive roll body mismatch, got: " ++ show other)
        case typeCheck (EUnroll (ELit (LInt 1))) of
            Left TCExpectedRecursive{} -> pure ()
            other -> expectationFailure ("Expected recursive unroll rejection, got: " ++ show other)

    it "rejects bare recursive self-reference in lambda annotations, let schemes, bounds, instantiation, and rolls" $ do
        let polyId = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
            expectNonContractive term =
                case typeCheck term of
                    Left (TCNonContractiveRecursiveType ty) | ty == bareRecursiveTy -> pure ()
                    other ->
                        expectationFailure
                            ("Expected non-contractive recursive type rejection for "
                                ++ show bareRecursiveTy ++ ", got: " ++ show other)
        expectNonContractive (mkTestLocalLam "x" bareRecursiveTy (mkTestDeferredVar "x"))
        expectNonContractive (mkTestLocalLet "x" (schemeFromType bareRecursiveTy) (ELit (LInt 1)) (mkTestDeferredVar "x"))
        expectNonContractive (mkTestTyAbs "a" (Just bareRecursiveTy) (ELit (LInt 1)))
        expectNonContractive (ETyInst polyId (InstApp bareRecursiveTy))
        expectNonContractive (ERoll bareRecursiveTy (ELit (LInt 1)))

    it "rejects forall-only recursive types under the conservative v1 policy" $ do
        case typeCheck (mkTestLocalLam "x" forallRecursiveTy (mkTestDeferredVar "x")) of
            Left (TCNonContractiveRecursiveType ty) | ty == forallRecursiveTy -> pure ()
            other ->
                expectationFailure
                    ("Expected forall-only recursive type rejection, got: " ++ show other)

    it "reports instantiation errors" $ do
        case typeCheck (ETyInst (ELit (LInt 1)) InstElim) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected instantiation error, got: " ++ show other)

    it "rejects InstApp that violates an explicit bound" $ do
        let term =
                ETyInst
                    (mkTestTyAbs "a" (Just intTy) (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x")))
                    (InstApp boolTy)
        case typeCheck term of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected bounded instantiation error, got: " ++ show other)

    it "preserves type-variable InstApp arguments" $ do
        let idTyAbs = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
            term = mkTestTyAbs "b" Nothing (ETyInst idTyAbs (InstApp (testTVar "b")))
        typeCheck term `shouldBeRightAlphaEq` testTForall "b" Nothing (TArrow (testTVar "b") (testTVar "b"))

    it "specializes residual flexible InstApp variables at application sites" $ do
        let poly =
                mkTestTyAbs "a"
                    Nothing
                    (mkTestLocalLam "x" (testTVar "a") (mkTestLocalLam "y" (testTVar "a") (mkTestDeferredVar "x")))
            term = EApp (EApp (ETyInst poly (InstApp (testTVar "t46"))) (ELit (LInt 1))) (ELit (LInt 2))
        typeCheck term `shouldBe` Right intTy

    it "rejects InstBot on alpha-equal non-bottom type (checker strictness)" $ do
        let poly = mkTestTyAbs "a" (Just intTy) (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
            polyTy = testTForall "a" (Just intTy) (TArrow (testTVar "a") (testTVar "a"))
        case typeCheck (ETyInst poly (InstBot polyTy)) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected strict InstBot rejection, got: " ++ show other)

    -- BUG-004 strict InstBot regressions
    it "accepts InstInside(InstBot) updating unbounded forall's bound" $ do
        -- ∀(a ⩾ ⊥).a→a  with InstInside(InstBot Int) → ∀(a ⩾ Int).a→a
        let poly = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
        typeCheck (ETyInst poly (InstInside (InstBot intTy)))
            `shouldBeRightAlphaEq` testTForall "a" (Just intTy) (TArrow (testTVar "a") (testTVar "a"))

    it "rejects InstInside(InstBot) when bound is already non-bottom" $ do
        -- ∀(a ⩾ Int).a→a  with InstInside(InstBot Int) — bound is Int, not ⊥
        let poly = mkTestTyAbs "a" (Just intTy) (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
        case typeCheck (ETyInst poly (InstInside (InstBot intTy))) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected strict InstBot rejection, got: " ++ show other)

    it "rejects bare InstBot on non-bottom bound even when types match" $ do
        -- ∀(a:Int).a→a  with InstBot Int — the forall type itself is not ⊥
        let poly = mkTestTyAbs "a" (Just intTy) (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
        case typeCheck (ETyInst poly (InstBot intTy)) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected strict InstBot rejection, got: " ++ show other)

    describe "A6 parity regressions (bounded/coercion-heavy)" $ do
        it "typeCheck agrees for unchecked vs checked bounded-alias coercion path" $ do
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
                expectedTy =
                    testTForall "a" Nothing (TArrow (testTVar "a") (TArrow (testTVar "a") (testTVar "a")))
                isPolyBinaryId ty =
                    TypeOps.alphaEqType ty expectedTy

            case runPipelineElab Set.empty normExpr of
                Left err -> expectationFailure ("Canonical pipeline failed:\n" ++ renderPipelineError err)
                Right (term, ty) -> do
                    ty `shouldSatisfy` isPolyBinaryId
                    typeCheck term `shouldBe` Right ty

        it "dual annotated coercion consumers fail fast on unresolved non-root OpWeaken" $ do
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

            let expectPipelineFailure label res =
                    case res of
                        Left err ->
                            renderPipelineError err `shouldSatisfy`
                                (\msg ->
                                    "PhiTranslatabilityError" `elem` words msg
                                        || "TCInstantiationError" `elem` words msg
                                        || "TCLetTypeMismatch" `elem` words msg
                                )
                        Right (term, ty) ->
                            expectationFailure
                                (label ++ " unexpectedly succeeded with type-checked term: " ++ show (term, ty))

            expectPipelineFailure "canonical pipeline" (runPipelineElab Set.empty normExpr)
