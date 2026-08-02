{-# LANGUAGE GADTs #-}

-- Convention: Promote minimized QuickCheck counterexamples from
-- TypeSoundnessSpec / PipelineSpec into fixed regression tests here.
module TypeCheckSpec (spec) where

import qualified ElabTypeTestSupport as TestElab
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
    , PipelineError(..)
    , TypeCheckError(..)
    , insertTypeBindingRef
    , mkTypeCheckEnvWithResolvedTerms
    , resolvedTermEnvEntries
    , freshenTypeAbsAgainstEnv
    , renderPipelineError
    , restrictResolvedTermBindings
    , runPipelineElab
    , schemeFromType
    , schemeToType
    , typeCheck
    , typeCheckWithEnv
    , unionEnvs
    )
import MLF.Elab.Run.Pipeline.TestSupport
    ( PipelineElabDetailedResult(..)
    , closePipelineTerm
    , extendPreparedExternalBindingTypeIdentities
    , extendPreparedExternalBindingTypeIdentityCandidates
    , preferPreparedExternalBindingTypeIdentities
    , prepareExternalBindings
    , preparedExternalTypeCheckEnv
    , preparedSourceTypeIdentityMaps
    , freshenTypeAbsAgainstEnvFromSupply
    , restrictPreparedExternalBindings
    , runPipelineElabDetailedWithExternalBindings
    , runPipelineElabDetailedWithPreparedExternalBindings
    , unionPreparedExternalBindings
    )
import MLF.Elab.TermClosure
    ( alignTermTypeVarsToScheme
    , alignTopTyAbsToScheme
    , closeTermWithSchemeSubstRefsIfNeeded
    , constructTermWithSchemeSubstRefsAtPublication
    , constructTermWithSchemeSubstRefsByBinderRoutes
    , substInTermRefs
    )
import MLF.Types.Elab
    ( ResolvedVar(..)
    , TypeBinderRef
    , deferredResolvedVarFromRef
    , eTyAbsWithRef
    , elabToBound
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
    , resolvedVarRuntimeName
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
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace(..), SymbolOwnerIdentity(..), symbolIdentityFromParts, symbolIdentityStableName, symbolUniqueIdentity)
import MLF.Frontend.ConstraintGen (ConstraintError(..), ExternalBinding(..), ExternalBindingIdentity, ExternalBindingMode(..), ModuleConstraintResult(..), externalBindingIdentityFromResolvedVar, externalBindingRuntimeName, generateModuleConstraintsKeyedWithExternalBindingsAndTypeIdentitiesFromSupply)
import MLF.Frontend.Program.Builtins (builtinTypeIdentity, builtinValueIdentity)
import MLF.Frontend.Syntax (Lit(..), SrcBound(..))
import qualified MLF.Frontend.Syntax as Surf (Expr(..), SrcTy(..))
import MLF.Primitive.Inventory (stringLengthPrimitiveName)
import qualified MLF.Reify.TypeOps as TypeOps
import MLF.Types.Identity
    ( EnvRef
    , constructorRefFromSymbol
    , deferredRefFromIdentity
    , envRefFromIdentity
    , IdDetails(..)
    , LocalIdentity(..)
    , localRefFromIdentity
    , localRefFromNodeId
    , localRefIdentity
    , primitiveRefFromSymbol
    , UniqueIdentity(..)
    , freshDeferredRef
    , freshLocalRef
    , idDetailsAliasMapWith
    , idDetailsConstructorRef
    , idDetailsDisplayName
    , idDetailsIsLocal
    , idDetailsRenameLocal
    , idDetailsReferenceName
    , idDetailsSameIdentity
    , initialIdentityGenerator
    , StructuralTypeBinderRole(..)
    , typeBinderIdentityFromStructural
    , typeBinderIdentityStableName
    , uniqueIdentityStableName
    )
import ElabTermTestSupport
    ( generatedLocalRef
    , generatedLocalRefForName
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
    symbolIdentityFromParts (UniqueIdentity unique) namespace moduleName name owner

shouldBeRightAlphaEq :: Either TypeCheckError ElabType -> ElabType -> Expectation
shouldBeRightAlphaEq actual expected =
    case actual of
        Right ty | TypeOps.alphaEqType ty expected -> pure ()
        other -> other `shouldBe` Right expected

termInstAppTypes :: XmlfTerm -> [ElabType]
termInstAppTypes term =
    case term of
        EVarNode {} -> []
        ELit {} -> []
        ELam _ body -> termInstAppTypes body
        EApp fun arg -> termInstAppTypes fun ++ termInstAppTypes arg
        ELet _ _ rhs body -> termInstAppTypes rhs ++ termInstAppTypes body
        ETyAbsRef _ _ body -> termInstAppTypes body
        ETyInst body inst -> termInstAppTypes body ++ instAppTypes inst
        ERoll _ body -> termInstAppTypes body
        EUnroll body -> termInstAppTypes body
  where
    instAppTypes inst =
        case inst of
            InstApp ty -> [ty]
            InstUnderRef _ inner -> instAppTypes inner
            InstInside inner -> instAppTypes inner
            InstSeq left right -> instAppTypes left ++ instAppTypes right
            _ -> []

externalBindingIdentityFromDetails :: String -> IdDetails -> ExternalBindingIdentity
externalBindingIdentityFromDetails _runtimeName details =
    externalBindingIdentityFromResolvedVar
        ResolvedVar
            {
            resolvedVarType = TBottom
            , resolvedVarDetails = details
            }

fixtureExternalBindingIdentity :: String -> ExternalBindingIdentity
fixtureExternalBindingIdentity name =
    externalBindingIdentityFromDetails
        name
        (EnvId (fixtureExternalEnvRef name))

fixtureExternalEnvRef :: String -> EnvRef
fixtureExternalEnvRef name =
    envRefFromIdentity (UniqueIdentity (negate (900000 + stableNameKey name))) name
  where
    stableNameKey = foldl (\acc char -> (acc * 131 + fromEnum char) `mod` 100000) 0

spec :: Spec
spec = describe "Phase 7 typecheck" $ do
    let intTy = TestElab.tBase (BaseTy "Int")
        builtinIntTy =
            ElabTypes.TBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int")
        listSelfTy = testTMu "self" (TestElab.tCon (BaseTy "List") (testTVar "self" :| []))
        bareRecursiveTy = testTMu "self" (testTVar "self")
        forallRecursiveTy = testTMu "self" (testTForall "b" Nothing (testTVar "self"))
        recursiveIntTy = testTMu "self" (TArrow (testTVar "self") intTy)
        boolTy = TestElab.tBase (BaseTy "Bool")
        recursiveBody = mkTestLocalLam "self" recursiveIntTy (ELit (LInt 1))
        resolvedLocal ref runtime ty =
            generatedResolvedLocal 0 ref runtime ty
        typeRef :: Int -> String -> TypeBinderRef
        typeRef key name =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name

    it "promotes builtin elab type patterns to stored identities" $ do
        TestElab.tBase (BaseTy "Int")
            `shouldBe` ElabTypes.TBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int")
        TestElab.tCon (BaseTy "String") (intTy :| [])
            `shouldBe` ElabTypes.TConWithIdentity (builtinTypeIdentity "String") (BaseTy "String") (intTy :| [])

    it "compares checked type heads by identity when names are stale" $ do
        let tokenIdentity = generatedSymbolIdentity 991826 SymbolType "Main" "Token" Nothing
            otherTokenIdentity = generatedSymbolIdentity 991827 SymbolType "Main" "Token" Nothing
        ElabTypes.TBaseWithIdentity tokenIdentity (BaseTy "Token")
            `shouldBe` ElabTypes.TBaseWithIdentity tokenIdentity (BaseTy "$stale.Token")
        ElabTypes.TBaseWithIdentity tokenIdentity (BaseTy "Token")
            `shouldNotBe` ElabTypes.TBaseWithIdentity otherTokenIdentity (BaseTy "Token")
        ElabTypes.TConWithIdentity tokenIdentity (BaseTy "Token") (intTy :| [])
            `shouldBe` ElabTypes.TConWithIdentity tokenIdentity (BaseTy "$stale.Token") (intTy :| [])

    it "keeps graph and generated type-binder keys and stable names disjoint" $ do
        let graphIdentity = typeBinderIdentityFromNode (NodeId 0)
            generatedIdentity = typeBinderIdentityFromUnique (UniqueIdentity 0)
            graphNegativeCollision = typeBinderIdentityFromNode (NodeId 299999)
            generatedNegative = typeBinderIdentityFromUnique (UniqueIdentity (-300000))
        typeBinderIdentityKey graphIdentity `shouldNotBe` typeBinderIdentityKey generatedIdentity
        typeBinderIdentityStableName generatedIdentity `shouldBe` "$typevar#0"
        typeBinderIdentityStableName graphIdentity `shouldNotBe` typeBinderIdentityStableName generatedIdentity
        typeBinderIdentityStableName graphNegativeCollision `shouldNotBe` typeBinderIdentityStableName generatedNegative

    describe "Formal obligations ledger anchors (Chapter 14 typing/instance)" $ do
        it "O14-WF-EMPTY O14-WF-TVAR O14-WF-VAR: environment well-formedness proxies" $ do
            typeCheck (ELit (LInt 0)) `shouldBe` Right builtinIntTy
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
            typeCheck (ETyInst (ELit (LInt 1)) InstId) `shouldBe` Right builtinIntTy
            typeCheck (ETyInst (ELit (LInt 1)) (InstSeq InstIntro InstElim)) `shouldBe` Right builtinIntTy
            case typeCheck (ETyInst (ELit (LInt 1)) InstIntro) of
                Right (TForallRef ref Nothing body) -> do
                    typeBinderRefName ref `shouldBe` "u0"
                    typeBinderIdentityKey (typeBinderRefIdentity ref) `shouldBe` -1
                    body `shouldBe` builtinIntTy
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

    it "matches nominal data heads to structural mu by binder identity before spelling" $ do
        let tokenIdentity = generatedSymbolIdentity 991828 SymbolType "Main" "Token" Nothing
            selfRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromStructural (symbolUniqueIdentity tokenIdentity) StructuralSelfBinder)
                    "$not_the_token_self_suffix"
            resultRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromStructural (symbolUniqueIdentity tokenIdentity) StructuralResultBinder)
                    "$not_the_token_result_suffix"
            nominalTokenTy = ElabTypes.TBaseWithIdentity tokenIdentity (BaseTy "Main.Token")
            structuralTokenTy =
                ElabTypes.TMuRef
                    selfRef
                    (ElabTypes.TForallRef resultRef Nothing (TArrow (TArrow (ElabTypes.TVarRef selfRef) (ElabTypes.TVarRef resultRef)) (ElabTypes.TVarRef resultRef)))
            arg = resolvedLocal "$arg#identity" "arg" structuralTokenTy
            env = mkTypeCheckEnvWithResolvedTerms [(arg, structuralTokenTy)] Map.empty
            term = EApp (mkTestLocalLam "x" nominalTokenTy (ELit (LInt 0))) (EVarNode arg)
        typeCheckWithEnv env term `shouldBe` Right builtinIntTy

    it "propagates nominal/structural data identity through structural types" $ do
        let boxIdentity = generatedSymbolIdentity 991829 SymbolType "Core" "Box" Nothing
            boxSelfRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromStructural (symbolUniqueIdentity boxIdentity) StructuralSelfBinder)
                    "$Core.Box_self"
            boxResultRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromStructural (symbolUniqueIdentity boxIdentity) StructuralResultBinder)
                    "$Core.Box_result"
            nominalBoxTy =
                ElabTypes.TConWithIdentity
                    boxIdentity
                    (BaseTy "$stale_box_name")
                    (boolTy :| [])
            structuralBoxTy =
                ElabTypes.TMuRef
                    boxSelfRef
                    ( ElabTypes.TForallRef
                        boxResultRef
                        Nothing
                        (TArrow (TArrow boolTy (ElabTypes.TVarRef boxResultRef)) (ElabTypes.TVarRef boxResultRef))
                    )
            wrapperIdentity = generatedSymbolIdentity 991830 SymbolType "Core" "Wrapper" Nothing
            wrapperSelfRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromStructural (symbolUniqueIdentity wrapperIdentity) StructuralSelfBinder)
                    "$Core.Wrapper_self"
            wrapperResultRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromStructural (symbolUniqueIdentity wrapperIdentity) StructuralResultBinder)
                    "$Core.Wrapper_result"
            wrapperTy fieldTy =
                ElabTypes.TMuRef
                    wrapperSelfRef
                    ( ElabTypes.TForallRef
                        wrapperResultRef
                        Nothing
                        (TArrow (TArrow fieldTy (ElabTypes.TVarRef wrapperResultRef)) (ElabTypes.TVarRef wrapperResultRef))
                    )
            expectedWrapperTy = wrapperTy structuralBoxTy
            actualWrapperTy = wrapperTy nominalBoxTy
            arg = generatedResolvedLocal 991831 "$arg#nested-identity" "arg" actualWrapperTy
            env = mkTypeCheckEnvWithResolvedTerms [(arg, actualWrapperTy)] Map.empty
            term =
                EApp
                    (mkTestLocalLam "x" expectedWrapperTy (ELit (LInt 0)))
                    (EVarNode arg)
        typeCheckWithEnv env term `shouldBe` Right builtinIntTy

    it "keeps same-spelled resolved local binders distinct by identity" $ do
        let outer = generatedResolvedLocal 0 "x" "runtime-x" intTy
            inner = generatedResolvedLocal 1 "x" "runtime-x" boolTy
            term = ELam outer (ELam inner (EVarNode outer))
        typeCheck term `shouldBe` Right (TArrow intTy (TArrow boolTy intTy))

    it "does not overwrite an existing resolved local binder identity" $ do
        let outer = generatedResolvedLocal 0 "x" "runtime-x" intTy
            inner = generatedResolvedLocal 0 "x" "runtime-y" boolTy
            term = ELam outer (ELam inner (EVarNode inner))
        typeCheck term `shouldBe` Left (TCResolvedVarTypeMismatch "x" intTy boolTy)

    it "preserves same-spelled resolved identities across typecheck env union" $ do
        let outer = generatedResolvedLocal 0 "x" "runtime-x" intTy
            inner = generatedResolvedLocal 1 "x" "runtime-x" boolTy
            outerEnv = mkTypeCheckEnvWithResolvedTerms [(outer, intTy)] Map.empty
            innerEnv = mkTypeCheckEnvWithResolvedTerms [(inner, boolTy)] Map.empty
            merged = unionEnvs innerEnv outerEnv
        typeCheckWithEnv merged (EVarNode outer) `shouldBe` Right intTy
        typeCheckWithEnv merged (EVarNode inner) `shouldBe` Right boolTy

    it "does not choose an arbitrary initial resolved term binding when one identity has conflicting types" $ do
        let actual = generatedResolvedLocal 0 "x" "runtime-x" intTy
            conflicting = generatedResolvedLocal 0 "x" "runtime-x" boolTy
            env = mkTypeCheckEnvWithResolvedTerms [(actual, intTy), (conflicting, boolTy), (actual, intTy)] Map.empty
        typeCheckWithEnv env (EVarNode actual) `shouldBe` Left (TCUnboundVar "x")

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
                    {
                    resolvedVarType = ty
                    , resolvedVarDetails =
                        TopLevelId (generatedSymbolIdentity unique SymbolValue moduleName "x" Nothing)
                    }
            actual = topLevelResolved 10 "Actual" intTy
            stale = topLevelResolved 11 "Stale" intTy
            env = mkTypeCheckEnvWithResolvedTerms [(actual, intTy)] Map.empty
        typeCheckWithEnv env (EVarNode actual) `shouldBe` Right intTy
        typeCheckWithEnv env (EVarNode stale) `shouldBe` Left (TCUnboundVar "Stale__x")

    it "does not resolve top-level references through conflicting identity payloads" $ do
        let actual =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails =
                        TopLevelId (generatedSymbolIdentity 10 SymbolValue "Actual" "x" Nothing)
                    }
            stale =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails =
                        TopLevelId (generatedSymbolIdentity 10 SymbolValue "Actual" "stale-x" Nothing)
                    }
            env = mkTypeCheckEnvWithResolvedTerms [(actual, intTy)] Map.empty
        typeCheckWithEnv env (EVarNode actual) `shouldBe` Right intTy
        typeCheckWithEnv env (EVarNode stale) `shouldBe` Left (TCUnboundVar "Actual__stale-x")

    it "does not resolve top-level identity through a same-named environment identity" $ do
        let envResolved =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails =
                        EnvId (envRefFromIdentity (UniqueIdentity 12) "x")
                    }
            resolved =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails =
                        TopLevelId (generatedSymbolIdentity 12 SymbolValue "Actual" "x" Nothing)
                    }
            env = mkTypeCheckEnvWithResolvedTerms [(envResolved, intTy)] Map.empty
        typeCheckWithEnv env (EVarNode resolved) `shouldBe` Left (TCUnboundVar "Actual__x")

    it "does not resolve stale environment identity through a same-named environment identity" $ do
        let envResolved identity =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails =
                        EnvId (envRefFromIdentity (UniqueIdentity identity) "x")
                    }
            actual = envResolved 0
            stale = envResolved 1
            env = mkTypeCheckEnvWithResolvedTerms [(actual, intTy)] Map.empty
        typeCheckWithEnv env (EVarNode actual) `shouldBe` Right intTy
        typeCheckWithEnv env (EVarNode stale) `shouldBe` Left (TCUnboundVar "x")

    it "seeds fresh identities after environment identities already present in terms" $ do
        let envResolved identity =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails =
                        EnvId (envRefFromIdentity (UniqueIdentity identity) "external")
                    }
            term = EVarNode (envResolved 4)
            (freshRef, _) = freshLocalRef "local" (identityGeneratorAfterTerm term)
        localRefIdentity freshRef `shouldBe` GeneratedLocalId (UniqueIdentity 5)

    it "seeds fresh identities after top-level symbol identities already present in terms" $ do
        let topLevelResolved identity =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails =
                        TopLevelId
                            (symbolIdentityFromParts (UniqueIdentity identity) SymbolValue "Actual" "external" Nothing)
                    }
            term = EVarNode (topLevelResolved 4)
            (freshRef, _) = freshLocalRef "local" (identityGeneratorAfterTerm term)
        localRefIdentity freshRef `shouldBe` GeneratedLocalId (UniqueIdentity 5)

    it "seeds fresh identities after owner symbol identities already present in terms" $ do
        let classIdentity =
                symbolIdentityFromParts (UniqueIdentity 7) SymbolClass "Actual" "Show" Nothing
            methodIdentity =
                symbolIdentityFromParts (UniqueIdentity 4) SymbolMethod "Actual" "show" (Just (SymbolOwnerClass classIdentity))
            term =
                EVarNode
                    ResolvedVar
                        {
                        resolvedVarType = intTy
                        , resolvedVarDetails = MethodId methodIdentity
                        }
            (freshRef, _) = freshLocalRef "local" (identityGeneratorAfterTerm term)
        localRefIdentity freshRef `shouldBe` GeneratedLocalId (UniqueIdentity 8)

    it "seeds fresh identities after deferred identities already present in terms" $ do
        let (deferredRef, _) = freshDeferredRef "deferred" initialIdentityGenerator
            term = mkDeferredVarWithRef deferredRef
            (freshRef, _) = freshLocalRef "local" (identityGeneratorAfterTerm term)
        localRefIdentity freshRef `shouldBe` GeneratedLocalId (UniqueIdentity 1)

    it "seeds fresh identities after generated type identities already present in terms" $ do
        let generatedRef = typeBinderRefFromIdentity (typeBinderIdentityFromUnique (UniqueIdentity 4)) "a"
            term =
                EVarNode
                    ResolvedVar
                        {
                        resolvedVarType = tVarWithRef generatedRef
                        , resolvedVarDetails = LocalId (localRefFromNodeId "x" (NodeId 0))
                        }
            (freshRef, _) = freshLocalRef "local" (identityGeneratorAfterTerm term)
        localRefIdentity freshRef `shouldBe` GeneratedLocalId (UniqueIdentity 5)

    it "fixture local refs avoid generated type identities already present in term types" $ do
        let fixtureIdentity =
                case localRefIdentity (generatedLocalRefForName "x") of
                    GeneratedLocalId identity -> identity
                    GeneratedGraphLocalId identity _ -> identity
                    GraphLocalId {} -> UniqueIdentity 0
                    ScopedGraphLocalId {} -> UniqueIdentity 0
            nextIdentity (UniqueIdentity value) = UniqueIdentity (value + 1)
            generatedRef = typeBinderRefFromIdentity (typeBinderIdentityFromUnique fixtureIdentity) "a"
            body =
                EVarNode
                    ResolvedVar
                        {
                        resolvedVarType = tVarWithRef generatedRef
                        , resolvedVarDetails = LocalId (localRefFromNodeId "y" (NodeId 1))
                        }
        case mkTestLocalLam "x" (tVarWithRef generatedRef) body of
            ELam resolved _ ->
                resolvedVarDetails resolved `shouldBe` LocalId (localRefFromIdentity (GeneratedLocalId (nextIdentity fixtureIdentity)) "x")
            other -> expectationFailure ("expected fixture lambda, got: " ++ show other)

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

    it "compares external binding identities by identity when runtime names are stale" $ do
        let symbol unique =
                generatedSymbolIdentity unique SymbolValue "Main" "x" Nothing
            identity runtimeName unique =
                externalBindingIdentityFromDetails runtimeName (TopLevelId (symbol unique))
        externalBindingRuntimeName (identity "$stale_x" 20) `shouldBe` "Main__x"
        identity "x" 20 `shouldBe` identity "$stale_x" 20
        identity "x" 20 `shouldNotBe` identity "x" 21

    it "preserves external binding identity in prepared typecheck environments" $ do
        let symbol unique moduleName =
                generatedSymbolIdentity unique SymbolValue moduleName "x" Nothing
            topLevelResolved unique moduleName =
                ResolvedVar
                    {
                    resolvedVarType = builtinIntTy
                    , resolvedVarDetails = TopLevelId (symbol unique moduleName)
                    }
            externalIdentity =
                externalBindingIdentityFromDetails "x" (TopLevelId (symbol 20 "Actual"))
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = externalIdentity
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
        case prepareExternalBindings (Map.singleton "x" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared -> do
                let env = preparedExternalTypeCheckEnv prepared
                typeCheckWithEnv env (EVarNode (topLevelResolved 20 "Actual")) `shouldBe` Right builtinIntTy
                typeCheckWithEnv env (EVarNode (topLevelResolved 21 "Stale")) `shouldBe` Left (TCUnboundVar "Stale__x")

    it "shares scheme identities across identity-bearing external aliases" $ do
        let symbol =
                generatedSymbolIdentity 23 SymbolValue "Actual" "poly" Nothing
            stableName =
                symbolIdentityStableName symbol
            externalBinding =
                ExternalBinding
                    { externalBindingType =
                        Surf.STForall
                            "a"
                            Nothing
                            (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity =
                        externalBindingIdentityFromDetails "poly" (TopLevelId symbol)
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
            externalBindings =
                Map.fromList
                    [ ("poly", externalBinding)
                    , (stableName, externalBinding)
                    ]
        case prepareExternalBindings externalBindings of
            Left err ->
                expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared -> do
                let (_, binderIdentities) =
                        preparedSourceTypeIdentityMaps prepared
                    declarationRefs =
                        [ ref
                        | (resolved, ty) <-
                            resolvedTermEnvEntries
                                (resolvedTermEnv (preparedExternalTypeCheckEnv prepared))
                        , resolvedVarDetails resolved == TopLevelId symbol
                        , (ref, _) <- schemeBinderRefs (schemeFromType ty)
                        ]
                case declarationRefs of
                    declarationRef : _ ->
                        Map.lookup "a" binderIdentities
                            `shouldBe` Just (typeBinderRefIdentity declarationRef)
                    [] ->
                        expectationFailure
                            "Expected prepared external scheme declaration binder"
                case runPipelineElabDetailedWithPreparedExternalBindings
                    Set.empty
                    prepared
                    (unsafeNormalizeExpr (Surf.EVar "poly")) of
                    Right PipelineElabDetailedResult {pedTerm = EVarNode resolved} ->
                        resolvedVarDetails resolved `shouldBe` TopLevelId symbol
                    Right result ->
                        expectationFailure ("Expected external variable term, got: " ++ show (pedTerm result))
                    Left err ->
                        expectationFailure ("Expected shared external alias scheme, got: " ++ renderPipelineError err)

    it "uses external binding identity aliases during constraint generation" $ do
        let symbol =
                generatedSymbolIdentity 24 SymbolValue "Actual" "x" Nothing
            stableName =
                symbolIdentityStableName symbol
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity =
                        externalBindingIdentityFromDetails "$runtime_x" (TopLevelId symbol)
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
        case runPipelineElabDetailedWithExternalBindings
            Set.empty
            (Map.singleton "$runtime_x" externalBinding)
            (unsafeNormalizeExpr (Surf.EVar stableName)) of
            Right PipelineElabDetailedResult {pedTerm = EVarNode resolved} ->
                resolvedVarDetails resolved `shouldBe` TopLevelId symbol
            Right result ->
                expectationFailure ("Expected external variable term, got: " ++ show (pedTerm result))
            Left err ->
                expectationFailure ("Expected external binding alias lookup, got: " ++ renderPipelineError err)

    it "does not choose an ambiguous external binding identity alias" $ do
        let symbol unique moduleName =
                generatedSymbolIdentity unique SymbolValue moduleName "x" Nothing
            externalBinding runtimeName unique moduleName =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity =
                        externalBindingIdentityFromDetails runtimeName (TopLevelId (symbol unique moduleName))
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
            externalBindings =
                Map.fromList
                    [ ("$left_x", externalBinding "$left_x" 25 "Left")
                    , ("$right_x", externalBinding "$right_x" 26 "Right")
                    ]
        case runPipelineElabDetailedWithExternalBindings
            Set.empty
            externalBindings
            (unsafeNormalizeExpr (Surf.EVar "x")) of
            Left _ -> pure ()
            Right result ->
                expectationFailure ("Expected ambiguous alias to stay unresolved, got: " ++ show (pedTerm result))

    it "preserves builtin type identities in prepared external binding types" $ do
        let externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "x"
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
        case prepareExternalBindings (Map.singleton "x" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "x" ] of
                    [ty@(TBaseWithIdentity _ (BaseTy "Int"))] ->
                        generatedIdentitiesInType ty `shouldSatisfy` elem (symbolUniqueIdentity (builtinTypeIdentity "Int"))
                    other -> expectationFailure ("Expected Int identity in prepared external binding, got: " ++ show other)

    it "uses supplied type head identity before builtin spelling in external binding types" $ do
        let typeIdentity = generatedSymbolIdentity 9041 SymbolType "Main" "Int" Nothing
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "int"
                    , externalBindingTypeHeadIdentities = Map.singleton "Int" typeIdentity
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
        case prepareExternalBindings (Map.singleton "int" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "int" ] of
                    [ElabTypes.TBaseWithIdentity actualIdentity (BaseTy "Int")] ->
                        actualIdentity `shouldBe` typeIdentity
                    other -> expectationFailure ("Expected supplied Int identity in prepared external binding, got: " ++ show other)

    it "keeps a three-way type-head conflict ambiguous after extension" $ do
        let headA = generatedSymbolIdentity 991840 SymbolType "A" "Clash" Nothing
            headB = generatedSymbolIdentity 991841 SymbolType "B" "Clash" Nothing
            headC = generatedSymbolIdentity 991842 SymbolType "C" "Clash" Nothing
            externalBinding name identity =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Clash"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity name
                    , externalBindingTypeHeadIdentities = Map.singleton "Clash" identity
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
            expr =
                unsafeNormalizeExpr
                    ( Surf.EAnn
                        (Surf.ELam "x" (Surf.EVar "x"))
                        (Surf.STArrow (Surf.STBase "Clash") (Surf.STBase "Clash"))
                    )
        case
            ( prepareExternalBindings (Map.singleton "left" (externalBinding "left" headA))
            , prepareExternalBindings (Map.singleton "right" (externalBinding "right" headB))
            ) of
            (Right preparedA, Right preparedB) -> do
                let preparedAB = unionPreparedExternalBindings preparedA preparedB
                    preparedABC =
                        extendPreparedExternalBindingTypeIdentities
                            (Map.singleton "Clash" headC)
                            Map.empty
                            preparedAB
                case runPipelineElabDetailedWithPreparedExternalBindings Set.empty preparedABC expr of
                    Left (PipelineConstraintError (UnknownTypeHead "Clash")) -> pure ()
                    Left err -> expectationFailure ("Expected ambiguous Clash head, got: " ++ renderPipelineError err)
                    Right result -> expectationFailure ("Expected ambiguous Clash head rejection, got: " ++ show (pedType result))
            (Left err, _) -> expectationFailure ("Expected left external binding preparation, got: " ++ show err)
            (_, Left err) -> expectationFailure ("Expected right external binding preparation, got: " ++ show err)

    it "keeps every lowered binder candidate before resolving a singleton" $ do
        let binderA = typeBinderIdentityFromUnique (UniqueIdentity 991843)
            binderB = typeBinderIdentityFromUnique (UniqueIdentity 991844)
            binderC = typeBinderIdentityFromUnique (UniqueIdentity 991845)
        case prepareExternalBindings Map.empty of
            Left err -> expectationFailure ("Expected empty external binding preparation, got: " ++ show err)
            Right prepared0 -> do
                let preparedAB =
                        extendPreparedExternalBindingTypeIdentityCandidates
                            []
                            [Map.singleton "a" binderA, Map.singleton "a" binderB]
                            prepared0
                    preparedABC =
                        extendPreparedExternalBindingTypeIdentities
                            Map.empty
                            (Map.singleton "a" binderC)
                            preparedAB
                    (_, resolvedBinderIdentities) =
                        preparedSourceTypeIdentityMaps preparedABC
                Map.lookup "a" resolvedBinderIdentities `shouldBe` Nothing

    it "prefers the current root binder identity over inherited same-spelled candidates" $ do
        let inheritedA = typeBinderIdentityFromUnique (UniqueIdentity 991848)
            inheritedB = typeBinderIdentityFromUnique (UniqueIdentity 991849)
            rootBinder = typeBinderIdentityFromUnique (UniqueIdentity 991850)
        case prepareExternalBindings Map.empty of
            Left err -> expectationFailure ("Expected empty external binding preparation, got: " ++ show err)
            Right prepared0 -> do
                let inherited =
                        extendPreparedExternalBindingTypeIdentityCandidates
                            []
                            [ Map.singleton "a" inheritedA
                            , Map.singleton "a" inheritedB
                            ]
                            prepared0
                    rootPrepared =
                        preferPreparedExternalBindingTypeIdentities
                            Map.empty
                            (Map.singleton "a" rootBinder)
                            inherited
                    (_, resolvedBinderIdentities) =
                        preparedSourceTypeIdentityMaps rootPrepared
                Map.lookup "a" resolvedBinderIdentities `shouldBe` Just rootBinder

    it "preserves source binder identity per module root" $ do
        let binderA = typeBinderIdentityFromUnique (UniqueIdentity 991846)
            binderB = typeBinderIdentityFromUnique (UniqueIdentity 991847)
            expr =
                unsafeNormalizeExpr
                    ( Surf.EAnn
                        (Surf.ELam "x" (Surf.EVar "x"))
                        (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))
                    )
        case
            generateModuleConstraintsKeyedWithExternalBindingsAndTypeIdentitiesFromSupply
                initialIdentityGenerator
                Set.empty
                Map.empty
                Map.empty
                (Map.fromList [(1 :: Int, Map.singleton "a" binderA), (2, Map.singleton "a" binderB)])
                Map.empty
                [(1, "left", expr), (2, "right", expr)] of
            Right ModuleConstraintResult {mcrSourceTypeBinderIdentities = identities} ->
                Set.fromList (IntMap.elems identities)
                    `shouldBe` Set.fromList [binderA, binderB]
            Left err -> expectationFailure ("Expected per-root module constraints, got: " ++ show err)

    it "does not invent a forall to hide a root-scheme mismatch" $ do
        let freeRef = typeBinderRefFromIdentity (typeBinderIdentityFromUnique (UniqueIdentity 991848)) "a"
            freeTy = tVarWithRef freeRef
            binder = generatedResolvedLocal 991849 "x" "x" freeTy
            term = ELam binder (EVarNode binder)
            initialEnv =
                insertTypeBindingRef
                    freeRef
                    TBottom
                    (mkTypeCheckEnvWithResolvedTerms [] Map.empty)
        closePipelineTerm
            initialEnv
            IntMap.empty
            (schemeFromType intTy)
            term
            term
            `shouldBe` term

    it "retains genuinely independent root binders" $ do
        let refA = typeRef 991853 "a"
            refB = typeRef 991854 "b"
            paramA = generatedResolvedLocal 991855 "$x#root-independent" "x" (tVarWithRef refA)
            paramB = generatedResolvedLocal 991856 "$y#root-independent" "y" (tVarWithRef refB)
            term = ELam paramA (ELam paramB (EVarNode paramA))
            rootScheme =
                mkElabSchemeWithRefs
                    [(refA, Nothing), (refB, Nothing)]
                    ( TArrow
                        (tVarWithRef refA)
                        (TArrow (tVarWithRef refB) (tVarWithRef refA))
                    )
            emptyTcEnv = mkTypeCheckEnvWithResolvedTerms [] Map.empty
            closed =
                closePipelineTerm
                    emptyTcEnv
                    IntMap.empty
                    rootScheme
                    term
                    term
        case closed of
            ETyAbsRef retainedA Nothing (ETyAbsRef retainedB Nothing _) -> do
                retainedA `shouldBe` refA
                retainedB `shouldBe` refB
            other -> expectationFailure ("Expected both independent root abstractions, got: " ++ show other)
        typeCheckWithEnv emptyTcEnv closed `shouldBe` Right (schemeToType rootScheme)

    it "does not quotient root binders with incompatible bounds" $ do
        let refA = typeRef 991857 "a"
            refB = typeRef 991858 "b"
            param = generatedResolvedLocal 991859 "$x#root-bounds" "x" (tVarWithRef refA)
            term = ELam param (EVarNode param)
            rootScheme =
                mkElabSchemeWithRefs
                    [(refA, Nothing), (refB, Just builtinIntTy)]
                    (TArrow (tVarWithRef refA) (tVarWithRef refB))
            emptyTcEnv = mkTypeCheckEnvWithResolvedTerms [] Map.empty
        closePipelineTerm
            emptyTcEnv
            IntMap.empty
            rootScheme
            term
            term
            `shouldBe` term

    it "does not quotient root binders onto an externally visible type identity" $ do
        let refA = typeRef 991860 "a"
            refB = typeRef 991861 "b"
            externalRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromUnique (UniqueIdentity 991862))
                    "external"
            param = generatedResolvedLocal 991863 "$x#root-external" "x" (tVarWithRef externalRef)
            term = ELam param (EVarNode param)
            rootScheme =
                mkElabSchemeWithRefs
                    [(refA, Nothing), (refB, Nothing)]
                    (TArrow (tVarWithRef refA) (tVarWithRef refB))
            initialEnv =
                insertTypeBindingRef
                    externalRef
                    TBottom
                    (mkTypeCheckEnvWithResolvedTerms [] Map.empty)
        closePipelineTerm
            initialEnv
            IntMap.empty
            rootScheme
            term
            term
            `shouldBe` term

    it "closes a bounded result over an annotated polymorphic self-application" $ do
        let annotationRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromUnique (UniqueIdentity 0))
                    "a"
            resultRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromNode (NodeId 11))
                    "result"
            annotationTy =
                TForallRef
                    annotationRef
                    Nothing
                    (TArrow (tVarWithRef annotationRef) (tVarWithRef annotationRef))
            binder = generatedResolvedLocal 0 "g" "g" annotationTy
            openTerm =
                ELam
                    binder
                    ( EApp
                        (ETyInst (EVarNode binder) (InstApp annotationTy))
                        (EVarNode binder)
                    )
            emptyTcEnv = mkTypeCheckEnvWithResolvedTerms [] Map.empty
        case elabToBound annotationTy of
            Left err -> expectationFailure err
            Right annotationBound -> do
                let rootScheme =
                        mkElabSchemeWithRefs
                            [(resultRef, Just annotationBound)]
                            (TArrow annotationTy (tVarWithRef resultRef))
                    rootSubst = IntMap.singleton 11 resultRef
                    closed =
                        closePipelineTerm
                            emptyTcEnv
                            rootSubst
                            rootScheme
                            openTerm
                            openTerm
                closed `shouldBe` openTerm
                typeCheckWithEnv emptyTcEnv closed
                    `shouldBe` Right (TArrow annotationTy annotationTy)

    it "collects owner identities from type heads" $ do
        let ownerIdentity = generatedSymbolIdentity 6001 SymbolType "Main" "Box" Nothing
            headIdentity = generatedSymbolIdentity 6002 SymbolType "Main" "Box.Alias" (Just (SymbolOwnerType ownerIdentity))
            ty = ElabTypes.TBaseWithIdentity headIdentity (BaseTy "Box")
        generatedIdentitiesInType ty `shouldBe` [UniqueIdentity 6002, UniqueIdentity 6001]

    it "restricts prepared external typecheck bindings by identity after same-name union" $ do
        let symbol unique moduleName =
                generatedSymbolIdentity unique SymbolValue moduleName "x" Nothing
            topLevelResolved unique moduleName =
                ResolvedVar
                    {
                    resolvedVarType = builtinIntTy
                    , resolvedVarDetails = TopLevelId (symbol unique moduleName)
                    }
            externalBinding unique moduleName =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity =
                        externalBindingIdentityFromDetails "x" (TopLevelId (symbol unique moduleName))
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.empty
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
                typeCheckWithEnv env (EVarNode (topLevelResolved 23 "Fallback")) `shouldBe` Left (TCUnboundVar "Fallback__x")
            (Left err, _) -> expectationFailure ("Expected preferred external binding preparation, got: " ++ show err)
            (_, Left err) -> expectationFailure ("Expected fallback external binding preparation, got: " ++ show err)

    it "assigns generated identities to free type variables in external binding types" $ do
        let externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STArrow (Surf.STVar "a") (Surf.STVar "a")
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "id"
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
        case prepareExternalBindings (Map.singleton "id" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "id" ] of
                    [TForallRef forallRef Nothing (TArrow (TVarRef argRef) (TVarRef resultRef))] -> do
                        typeBinderRefName forallRef `shouldBe` "a"
                        typeBinderRefIdentity forallRef `shouldBe` typeBinderIdentityFromUnique (UniqueIdentity 0)
                        typeBinderRefIdentity argRef `shouldBe` typeBinderRefIdentity forallRef
                        typeBinderRefIdentity resultRef `shouldBe` typeBinderRefIdentity forallRef
                    other -> expectationFailure ("Expected identity type with generated refs, got: " ++ show other)

    it "preserves supplied type binder identities in external binding types" $ do
        let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 42)
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STArrow (Surf.STVar "a") (Surf.STVar "a")
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "id"
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.singleton "a" binderIdentity
                    }
        case prepareExternalBindings (Map.singleton "id" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "id" ] of
                    [TForallRef forallRef Nothing (TArrow (TVarRef argRef) (TVarRef resultRef))] -> do
                        typeBinderRefName forallRef `shouldBe` "a"
                        typeBinderRefIdentity forallRef `shouldBe` binderIdentity
                        typeBinderRefIdentity argRef `shouldBe` binderIdentity
                        typeBinderRefIdentity resultRef `shouldBe` binderIdentity
                    other -> expectationFailure ("Expected identity type with supplied refs, got: " ++ show other)

    it "does not reuse supplied binder identities for shadowed external binding type variables" $ do
        let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 43)
            externalBinding =
                ExternalBinding
                    { externalBindingType =
                        Surf.STForall
                            "a"
                            Nothing
                            (Surf.STForall "a" Nothing (Surf.STVar "a"))
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "id"
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.singleton "a" binderIdentity
                    }
        case prepareExternalBindings (Map.singleton "id" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "id" ] of
                    [TForallRef outerRef Nothing (TForallRef innerRef Nothing (TVarRef bodyRef))] -> do
                        typeBinderRefIdentity outerRef `shouldBe` binderIdentity
                        typeBinderRefIdentity innerRef `shouldNotBe` binderIdentity
                        typeBinderRefIdentity bodyRef `shouldBe` typeBinderRefIdentity innerRef
                    other -> expectationFailure ("Expected nested forall with distinct shadowed refs, got: " ++ show other)

    it "does not reuse supplied binder identities inside external binding bounds" $ do
        let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 49)
            typeIdentity = generatedSymbolIdentity 50 SymbolType "Main" "Box" Nothing
            externalBinding =
                ExternalBinding
                    { externalBindingType =
                        Surf.STForall
                            "a"
                            Nothing
                            ( Surf.STForall
                                "b"
                                ( Just
                                    ( SrcBound
                                        ( Surf.STCon
                                            "Box"
                                            (Surf.STForall "a" Nothing (Surf.STVar "a") :| [])
                                        )
                                    )
                                )
                                (Surf.STVar "b")
                            )
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "id"
                    , externalBindingTypeHeadIdentities = Map.singleton "Box" typeIdentity
                    , externalBindingTypeBinderIdentities = Map.singleton "a" binderIdentity
                    }
        case prepareExternalBindings (Map.singleton "id" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "id" ] of
                    [ TForallRef outerRef Nothing
                            ( TForallRef _ (Just (TConWithIdentity _ (BaseTy "Box") (TForallRef innerRef Nothing (TVarRef bodyRef) :| []))) _
                            )
                        ] -> do
                            typeBinderRefIdentity outerRef `shouldBe` binderIdentity
                            typeBinderRefIdentity innerRef `shouldNotBe` binderIdentity
                            typeBinderRefIdentity bodyRef `shouldBe` typeBinderRefIdentity innerRef
                    other -> expectationFailure ("Expected bound-local shadowed refs, got: " ++ show other)

    it "resolves external binding type variables through stable binder identity aliases" $ do
        let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 142)
            stableName = typeBinderIdentityStableName binderIdentity
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STArrow (Surf.STVar stableName) (Surf.STVar stableName)
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "id"
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.singleton "a" binderIdentity
                    }
        case prepareExternalBindings (Map.singleton "id" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "id" ] of
                    [TForallRef forallRef Nothing (TArrow (TVarRef argRef) (TVarRef resultRef))] -> do
                        typeBinderRefIdentity forallRef `shouldBe` binderIdentity
                        typeBinderRefIdentity argRef `shouldBe` binderIdentity
                        typeBinderRefIdentity resultRef `shouldBe` binderIdentity
                    other -> expectationFailure ("Expected stable binder name to resolve through supplied identity, got: " ++ show other)

    it "preserves supplied type head identities in external binding types" $ do
        let typeIdentity = generatedSymbolIdentity 43 SymbolType "Main" "Box" Nothing
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Box"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "box"
                    , externalBindingTypeHeadIdentities = Map.singleton "Box" typeIdentity
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
        case prepareExternalBindings (Map.singleton "box" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "box" ] of
                    [ElabTypes.TBaseWithIdentity actualIdentity (BaseTy "Box")] ->
                        actualIdentity `shouldBe` typeIdentity
                    other -> expectationFailure ("Expected Box identity type with supplied head ref, got: " ++ show other)

    it "resolves external binding type heads through stable symbol identity aliases" $ do
        let typeIdentity = generatedSymbolIdentity 44 SymbolType "Main" "Box" Nothing
            stableName = symbolIdentityStableName typeIdentity
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase stableName
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "box"
                    , externalBindingTypeHeadIdentities = Map.singleton "Box" typeIdentity
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
        case prepareExternalBindings (Map.singleton "box" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ty | (resolved, ty) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "box" ] of
                    [ElabTypes.TBaseWithIdentity actualIdentity (BaseTy actualName)] -> do
                        actualIdentity `shouldBe` typeIdentity
                        actualName `shouldBe` stableName
                    other -> expectationFailure ("Expected stable head name to resolve through supplied identity, got: " ++ show other)

    it "preserves supplied type head identities in source annotations" $ do
        let typeIdentity = generatedSymbolIdentity 45 SymbolType "Main" "Box" Nothing
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Box"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "box"
                    , externalBindingTypeHeadIdentities = Map.singleton "Box" typeIdentity
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
            expr =
                unsafeNormalizeExpr
                    (Surf.EAnn
                        (Surf.ELam "x" (Surf.EVar "x"))
                        (Surf.STArrow (Surf.STBase "Box") (Surf.STBase "Box")))
        case runPipelineElabDetailedWithExternalBindings Set.empty (Map.singleton "box" externalBinding) expr of
            Left err -> expectationFailure ("Expected annotated elaboration, got: " ++ renderPipelineError err)
            Right PipelineElabDetailedResult
                { pedType =
                    ElabTypes.TArrow
                        (ElabTypes.TBaseWithIdentity domIdentity (BaseTy "Box"))
                        (ElabTypes.TBaseWithIdentity codIdentity (BaseTy "Box"))
                } -> do
                    domIdentity `shouldBe` typeIdentity
                    codIdentity `shouldBe` typeIdentity
            Right other -> expectationFailure ("Expected annotated Box identity arrow, got: " ++ show (pedType other))

    it "preserves supplied type binder identities in source annotations" $ do
        let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 146)
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STArrow (Surf.STVar "a") (Surf.STVar "a")
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "id"
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.singleton "a" binderIdentity
                    }
            expr =
                unsafeNormalizeExpr
                    (Surf.EAnn
                        (Surf.ELam "x" (Surf.EVar "x"))
                        (Surf.STForall "a" Nothing (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))))
        case runPipelineElabDetailedWithExternalBindings Set.empty (Map.singleton "id" externalBinding) expr of
            Left err -> expectationFailure ("Expected annotated elaboration, got: " ++ renderPipelineError err)
            Right PipelineElabDetailedResult {pedType = ElabTypes.TForallRef ref Nothing (ElabTypes.TArrow (ElabTypes.TVarRef argRef) (ElabTypes.TVarRef resultRef))} -> do
                typeBinderRefIdentity ref `shouldBe` binderIdentity
                typeBinderRefIdentity argRef `shouldBe` binderIdentity
                typeBinderRefIdentity resultRef `shouldBe` binderIdentity
            Right other -> expectationFailure ("Expected annotated identity type, got: " ++ show (pedType other))

    it "does not reuse supplied binder identities for shadowed source annotation type variables" $ do
        let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 148)
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STArrow (Surf.STVar "a") (Surf.STVar "a")
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "id"
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.singleton "a" binderIdentity
                    }
            expr =
                unsafeNormalizeExpr
                    (Surf.EAnn
                        (Surf.ELam "x" (Surf.EVar "x"))
                        ( Surf.STForall
                            "a"
                            Nothing
                            (Surf.STForall "a" Nothing (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a")))
                        ))
        case runPipelineElabDetailedWithExternalBindings Set.empty (Map.singleton "id" externalBinding) expr of
            Left err -> expectationFailure ("Expected annotated elaboration, got: " ++ renderPipelineError err)
            Right PipelineElabDetailedResult {pedType = ElabTypes.TForallRef ref Nothing (ElabTypes.TArrow (ElabTypes.TVarRef argRef) (ElabTypes.TVarRef resultRef))} -> do
                typeBinderRefIdentity ref `shouldNotBe` binderIdentity
                typeBinderRefIdentity argRef `shouldBe` typeBinderRefIdentity ref
                typeBinderRefIdentity resultRef `shouldBe` typeBinderRefIdentity ref
            Right PipelineElabDetailedResult {pedType = ElabTypes.TForallRef outerRef Nothing (ElabTypes.TForallRef innerRef Nothing (ElabTypes.TArrow (ElabTypes.TVarRef argRef) (ElabTypes.TVarRef resultRef)))} -> do
                typeBinderRefIdentity outerRef `shouldBe` binderIdentity
                typeBinderRefIdentity innerRef `shouldNotBe` binderIdentity
                typeBinderRefIdentity argRef `shouldBe` typeBinderRefIdentity innerRef
                typeBinderRefIdentity resultRef `shouldBe` typeBinderRefIdentity innerRef
            Right other -> expectationFailure ("Expected annotated nested forall with distinct shadowed refs, got: " ++ show (pedType other))

    it "resolves source annotation type heads through stable symbol identity aliases" $ do
        let typeIdentity = generatedSymbolIdentity 46 SymbolType "Main" "Box" Nothing
            stableName = symbolIdentityStableName typeIdentity
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase stableName
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "box"
                    , externalBindingTypeHeadIdentities = Map.singleton "Box" typeIdentity
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
            expr =
                unsafeNormalizeExpr
                    (Surf.EAnn
                        (Surf.ELam "x" (Surf.EVar "x"))
                        (Surf.STArrow (Surf.STBase stableName) (Surf.STBase stableName)))
        case runPipelineElabDetailedWithExternalBindings Set.empty (Map.singleton "box" externalBinding) expr of
            Left err -> expectationFailure ("Expected annotated elaboration, got: " ++ renderPipelineError err)
            Right PipelineElabDetailedResult
                { pedType =
                    ElabTypes.TArrow
                        (ElabTypes.TBaseWithIdentity domIdentity (BaseTy domName))
                        (ElabTypes.TBaseWithIdentity codIdentity (BaseTy codName))
                } -> do
                    domIdentity `shouldBe` typeIdentity
                    codIdentity `shouldBe` typeIdentity
                    domName `shouldBe` stableName
                    codName `shouldBe` stableName
            Right other -> expectationFailure ("Expected annotated stable Box identity arrow, got: " ++ show (pedType other))

    it "resolves source annotation type variables through stable binder identity aliases" $ do
        let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 147)
            stableName = typeBinderIdentityStableName binderIdentity
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STArrow (Surf.STVar stableName) (Surf.STVar stableName)
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "id"
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.singleton "a" binderIdentity
                    }
            expr =
                unsafeNormalizeExpr
                    (Surf.EAnn
                        (Surf.ELam "x" (Surf.EVar "x"))
                        (Surf.STForall stableName Nothing (Surf.STArrow (Surf.STVar stableName) (Surf.STVar stableName))))
        case runPipelineElabDetailedWithExternalBindings Set.empty (Map.singleton "id" externalBinding) expr of
            Left err -> expectationFailure ("Expected annotated elaboration, got: " ++ renderPipelineError err)
            Right PipelineElabDetailedResult {pedType = ElabTypes.TForallRef ref Nothing (ElabTypes.TArrow (ElabTypes.TVarRef argRef) (ElabTypes.TVarRef resultRef))} -> do
                typeBinderRefIdentity ref `shouldBe` binderIdentity
                typeBinderRefIdentity argRef `shouldBe` binderIdentity
                typeBinderRefIdentity resultRef `shouldBe` binderIdentity
            Right other -> expectationFailure ("Expected annotated stable identity type, got: " ++ show (pedType other))

    it "keeps the supplied external identity when type-head identities are also present" $ do
        let typeIdentity = generatedSymbolIdentity 43 SymbolType "Main" "Box" Nothing
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Box"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = fixtureExternalBindingIdentity "box"
                    , externalBindingTypeHeadIdentities = Map.singleton "Box" typeIdentity
                    , externalBindingTypeBinderIdentities = Map.empty
                    }
        case prepareExternalBindings (Map.singleton "box" externalBinding) of
            Left err -> expectationFailure ("Expected external binding preparation, got: " ++ show err)
            Right prepared ->
                case [ ref | (resolved, _) <- resolvedTermEnvEntries (resolvedTermEnv (preparedExternalTypeCheckEnv prepared)), resolvedVarReferenceName resolved == "box", EnvId ref <- [resolvedVarDetails resolved] ] of
                    [ref] -> ref `shouldBe` fixtureExternalEnvRef "box"
                    other -> expectationFailure ("Expected generated external env ref after supplied head identity, got: " ++ show other)

    it "keeps supplied external typecheck identities alongside deferred identities" $ do
        let (deferredRef, _) = freshDeferredRef "z" initialIdentityGenerator
            deferredIdentity =
                externalBindingIdentityFromDetails "z" (DeferredId deferredRef)
            externalBindings =
                Map.fromList
                    [ ( "a"
                      , ExternalBinding
                            { externalBindingType = Surf.STBase "Int"
                            , externalBindingMode = ExternalBindingScheme
                            , externalBindingIdentity = fixtureExternalBindingIdentity "a"
                            , externalBindingTypeHeadIdentities = Map.empty
                            , externalBindingTypeBinderIdentities = Map.empty
                            }
                      )
                    , ( "z"
                      , ExternalBinding
                            { externalBindingType = Surf.STBase "Int"
                            , externalBindingMode = ExternalBindingScheme
                            , externalBindingIdentity = deferredIdentity
                            , externalBindingTypeHeadIdentities = Map.empty
                            , externalBindingTypeBinderIdentities = Map.empty
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
                generatedRefs `shouldBe` [fixtureExternalEnvRef "a"]
                typeCheckWithEnv env (mkDeferredVarWithRef deferredRef) `shouldBe` Right builtinIntTy

    it "keeps supplied external elaboration identities alongside deferred identities" $ do
        let (deferredRef, _) = freshDeferredRef "z" initialIdentityGenerator
            deferredIdentity =
                externalBindingIdentityFromDetails "z" (DeferredId deferredRef)
            externalBindings =
                Map.fromList
                    [ ( "a"
                      , ExternalBinding
                            { externalBindingType = Surf.STBase "Int"
                            , externalBindingMode = ExternalBindingScheme
                            , externalBindingIdentity = fixtureExternalBindingIdentity "a"
                            , externalBindingTypeHeadIdentities = Map.empty
                            , externalBindingTypeBinderIdentities = Map.empty
                            }
                      )
                    , ( "z"
                      , ExternalBinding
                            { externalBindingType = Surf.STBase "Int"
                            , externalBindingMode = ExternalBindingScheme
                            , externalBindingIdentity = deferredIdentity
                            , externalBindingTypeHeadIdentities = Map.empty
                            , externalBindingTypeBinderIdentities = Map.empty
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
                resolvedVarDetails resolved `shouldBe` EnvId (fixtureExternalEnvRef "a")
            Right other -> expectationFailure ("Expected resolved external variable term, got: " ++ show (pedTerm other))

    it "elaborates external binding references with prepared identity" $ do
        let symbol =
                generatedSymbolIdentity 30 SymbolValue "Actual" "x" Nothing
            externalIdentity =
                externalBindingIdentityFromDetails "x" (TopLevelId symbol)
            externalBinding =
                ExternalBinding
                    { externalBindingType = Surf.STBase "Int"
                    , externalBindingMode = ExternalBindingScheme
                    , externalBindingIdentity = externalIdentity
                    , externalBindingTypeHeadIdentities = Map.empty
                    , externalBindingTypeBinderIdentities = Map.empty
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
                constructorRefFromSymbol ctorIdentity
            localDetails = LocalId (generatedLocalRef 0 "$x#0")
            envDetails =
                EnvId (envRefFromIdentity (UniqueIdentity 0) "external-x")
            primitiveDetails =
                PrimitiveId (primitiveRefFromSymbol (builtinValueIdentity stringLengthPrimitiveName))
            renamedPrimitiveDetails =
                PrimitiveId (primitiveRefFromSymbol (builtinValueIdentity stringLengthPrimitiveName))
            deferredDetails = DeferredId (deferredRefFromIdentity (UniqueIdentity 50) "x")
            sameNamedDeferredDetails = DeferredId (deferredRefFromIdentity (UniqueIdentity 51) "x")
            constructorDetails = ConstructorId ctorRef
            topLevelDetails =
                TopLevelId (generatedSymbolIdentity 42 SymbolValue "Main" "value" Nothing)
            conflictingTopLevelDetails =
                TopLevelId (generatedSymbolIdentity 42 SymbolValue "Main" "stale-value" Nothing)
            localResolved =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails = localDetails
                    }
            envResolved =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails = envDetails
                    }
            constructorResolved =
                ResolvedVar
                    {
                    resolvedVarType = intTy
                    , resolvedVarDetails = constructorDetails
                    }
            renamedLocalResolved =
                renameResolvedLocalVar "$x#1" localResolved
            staleTypedLocalResolved =
                localResolved {resolvedVarType = boolTy}
        idDetailsReferenceName localDetails `shouldBe` "$x#0"
        idDetailsReferenceName envDetails `shouldBe` "external-x"
        idDetailsReferenceName primitiveDetails `shouldBe` stringLengthPrimitiveName
        idDetailsReferenceName deferredDetails `shouldBe` "x"
        idDetailsIsLocal localDetails `shouldBe` True
        idDetailsIsLocal envDetails `shouldBe` False
        idDetailsIsLocal primitiveDetails `shouldBe` False
        idDetailsConstructorRef localDetails `shouldBe` Nothing
        idDetailsConstructorRef constructorDetails `shouldBe` Just ctorRef
        idDetailsReferenceName (idDetailsRenameLocal "$x#1" localDetails) `shouldBe` "$x#1"
        idDetailsSameIdentity localDetails (idDetailsRenameLocal "$x#1" localDetails) `shouldBe` True
        idDetailsSameIdentity localDetails (LocalId (generatedLocalRef 1 "$x#1")) `shouldBe` False
        idDetailsSameIdentity constructorDetails (ConstructorId ctorRef) `shouldBe` True
        idDetailsSameIdentity primitiveDetails renamedPrimitiveDetails `shouldBe` True
        idDetailsSameIdentity topLevelDetails conflictingTopLevelDetails `shouldBe` False
        idDetailsSameIdentity deferredDetails sameNamedDeferredDetails `shouldBe` False
        idDetailsSameIdentity localDetails (idDetailsRenameLocal "$x#1" localDetails) `shouldBe` True
        idDetailsSameIdentity topLevelDetails conflictingTopLevelDetails `shouldBe` False
        let otherLocalDetails = LocalId (generatedLocalRef 1 "$x#0")
            renamedSameIdentityDetails = idDetailsRenameLocal "$x#renamed" localDetails
            detailsByAlias = idDetailsAliasMapWith [("runtime-x", localDetails), ("runtime-y", otherLocalDetails)]
            sameIdentityConflictAliases =
                idDetailsAliasMapWith
                    [ ("runtime-x", localDetails)
                    , ("runtime-renamed", renamedSameIdentityDetails)
                    ]
            conflictingPayloadAliases =
                idDetailsAliasMapWith
                    [ ("runtime-value", topLevelDetails)
                    , ("runtime-value", conflictingTopLevelDetails)
                    ]
        Map.lookup "$x#0" detailsByAlias `shouldBe` Nothing
        Map.lookup (uniqueIdentityStableName (UniqueIdentity 0)) detailsByAlias `shouldBe` Just localDetails
        Map.lookup (uniqueIdentityStableName (UniqueIdentity 1)) detailsByAlias `shouldBe` Just otherLocalDetails
        Map.lookup (uniqueIdentityStableName (UniqueIdentity 0)) sameIdentityConflictAliases `shouldBe` Just localDetails
        fmap idDetailsReferenceName (Map.lookup "$x#0" sameIdentityConflictAliases) `shouldBe` Just "$x#0"
        fmap idDetailsReferenceName (Map.lookup "$x#renamed" sameIdentityConflictAliases) `shouldBe` Just "$x#renamed"
        Map.lookup "runtime-value" conflictingPayloadAliases `shouldBe` Nothing
        Map.lookup (symbolIdentityStableName (generatedSymbolIdentity 42 SymbolValue "Main" "value" Nothing)) conflictingPayloadAliases `shouldBe` Nothing
        fmap idDetailsDisplayName (Map.lookup "value" conflictingPayloadAliases) `shouldBe` Just "value"
        fmap idDetailsDisplayName (Map.lookup "Main.value" conflictingPayloadAliases) `shouldBe` Just "value"
        fmap idDetailsDisplayName (Map.lookup "stale-value" conflictingPayloadAliases) `shouldBe` Just "stale-value"
        fmap idDetailsDisplayName (Map.lookup "Main.stale-value" conflictingPayloadAliases) `shouldBe` Just "stale-value"
        localDetails `shouldBe` idDetailsRenameLocal "$x#1" localDetails
        localDetails `shouldBe` EvidenceId (generatedLocalRef 0 "$x#evidence")
        deferredDetails `shouldNotBe` sameNamedDeferredDetails
        idDetailsRenameLocal "$x#1" constructorDetails `shouldBe` constructorDetails
        renamedLocalResolved `shouldBe` localResolved
        staleTypedLocalResolved `shouldNotBe` localResolved
        resolvedVarReferenceName localResolved `shouldBe` "$x#0"
        resolvedVarReferenceName envResolved `shouldBe` "external-x"
        resolvedVarReferenceName constructorResolved `shouldBe` "Main__Box"
        resolvedVarIsLocal localResolved `shouldBe` True
        resolvedVarIsLocal envResolved `shouldBe` False
        resolvedVarConstructorRef constructorResolved `shouldBe` Just ctorRef
        resolvedVarRuntimeName (renameResolvedLocalVar "$x#1" localResolved) `shouldBe` "$x#1"
        renameResolvedLocalVar "$x#1" localResolved
            `shouldBe` localResolved
                {
                resolvedVarDetails = idDetailsRenameLocal "$x#1" localDetails
                }
        renameResolvedLocalVar "$Box1" constructorResolved `shouldBe` constructorResolved

    it "rejects resolved locals whose occurrence type is stale" $ do
        let binder = resolvedLocal "$x#0" "runtime-x" intTy
            occurrence = resolvedLocal "$x#0" "different-runtime" boolTy
            term = ELam binder (EVarNode occurrence)
        typeCheck term `shouldBe` Left (TCResolvedVarTypeMismatch "$x#0" intTy boolTy)

    it "preserves resolved locals while substituting internal type names" $ do
        let refA = typeRef 0 "a"
            refT0 = typeRef 0 "t0"
            binder = resolvedLocal "$x#0" "runtime-x" (tVarWithRef refT0)
            occurrence = resolvedLocal "$x#0" "different-runtime" (tVarWithRef refT0)
            term = substInTermRefs (IntMap.singleton 0 refA) (ELam binder (EVarNode occurrence))
        case term of
            ELam binder' (EVarNode occurrence') -> do
                resolvedVarType binder' `shouldBe` tVarWithRef refA
                resolvedVarType occurrence' `shouldBe` tVarWithRef refA
                resolvedVarDetails binder' `shouldBe` LocalId (generatedLocalRef 0 "$x#0")
                resolvedVarRuntimeName occurrence' `shouldBe` "$x#0"
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
            binder0 = resolvedLocal "$x#0" "runtime-x" (tForallWithRef refT0 Nothing (tVarWithRef refT0))
            binder1 =
                resolvedLocal
                    "$x#0"
                    "runtime-x"
                    (tForallWithRef refA Nothing (tVarWithRef refA))
            term0 =
                eTyAbsWithRef refT0
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

    it "does not rewrite InstAbstr to a different binder identity" $ do
        let exteriorRef = typeRef 50 "exterior"
            aliasedRef = typeRef 51 "alias"
            term = ETyInst (ELit (LInt 1)) (instAbstrWithRef exteriorRef)
        substInTermRefs
            (IntMap.singleton 50 aliasedRef)
            term
            `shouldBe` term

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

    it "aligns an open term's existing type abstraction before closing its scheme" $ do
        let refA = typeRef 64 "a"
            external =
                ResolvedVar
                    { resolvedVarType = intTy
                    , resolvedVarDetails = EnvId (fixtureExternalEnvRef "external-int")
                    }
            ignored = generatedResolvedLocal 65 "$ignored#0" "ignored" intTy
            param = generatedResolvedLocal 66 "$x#0" "x" (tVarWithRef refA)
            term =
                eTyAbsWithRef
                    refA
                    Nothing
                    ( EApp
                        (ELam ignored (ELam param (EVarNode param)))
                        (EVarNode external)
                    )
            scheme =
                schemeFromType
                    (tForallWithRef refA Nothing (TArrow (tVarWithRef refA) (tVarWithRef refA)))
        alignTopTyAbsToScheme scheme term `shouldBe` Just term
        closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term `shouldBe` term

    it "does not duplicate an existing scheme abstraction around a deferred body" $ do
        let refA = typeRef 643 "a"
            deferredRef = deferredRefFromIdentity (UniqueIdentity 644) "$deferred"
            deferredBody = ETyInst (mkDeferredVarWithRef deferredRef) InstElim
            term = eTyAbsWithRef refA Nothing deferredBody
            scheme = mkElabSchemeWithRefs [(refA, Nothing)] TBottom
        alignTopTyAbsToScheme scheme term `shouldBe` Just term
        closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term `shouldBe` term

    it "completes only the missing suffix of a partially abstracted scheme" $ do
        let sourceOuter = typeRef 645 "source-a"
            targetOuter = typeRef 646 "a"
            targetInner = typeRef 647 "b"
            deferredRef = deferredRefFromIdentity (UniqueIdentity 648) "$deferred"
            deferredBody = ETyInst (mkDeferredVarWithRef deferredRef) InstElim
            term = eTyAbsWithRef sourceOuter Nothing deferredBody
            scheme =
                mkElabSchemeWithRefs
                    [(targetOuter, Nothing), (targetInner, Nothing)]
                    TBottom
            expected =
                eTyAbsWithRef
                    targetOuter
                    Nothing
                    (eTyAbsWithRef targetInner Nothing deferredBody)
        alignTopTyAbsToScheme scheme term `shouldBe` Just expected
        closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term `shouldBe` expected

    it "inserts a missing binder before an identity-matched abstraction" $ do
        let betaRef = typeRef 660 "beta"
            alphaRef = typeRef 661 "alpha"
            betaParam =
                generatedResolvedLocal
                    662
                    "$x#0"
                    "x"
                    (tVarWithRef betaRef)
            alphaParam =
                generatedResolvedLocal
                    663
                    "$y#0"
                    "y"
                    (tVarWithRef alphaRef)
            body =
                ELam
                    betaParam
                    (ELam alphaParam (EVarNode alphaParam))
            term = eTyAbsWithRef alphaRef Nothing body
            scheme =
                mkElabSchemeWithRefs
                    [(betaRef, Nothing), (alphaRef, Nothing)]
                    ( TArrow
                        (tVarWithRef betaRef)
                        ( TArrow
                            (tVarWithRef alphaRef)
                            (tVarWithRef alphaRef)
                        )
                    )
            expected =
                eTyAbsWithRef
                    betaRef
                    Nothing
                    (eTyAbsWithRef alphaRef Nothing body)
        alignTopTyAbsToScheme scheme term `shouldBe` Just expected
        closeTermWithSchemeSubstRefsIfNeeded
            IntMap.empty
            scheme
            term
            `shouldBe` expected
        typeCheck expected `shouldBe` Right (schemeToType scheme)

    it "publishes a vacuous prefix before rebinding a computed forall result" $ do
        let methodGhostRef = typeRef 6640 "method-ghost"
            methodValueRef = typeRef 6641 "method-value"
            localGhostRef = typeRef 6642 "local-ghost"
            localValueRef = typeRef 6643 "local-value"
            localSchemeTy =
                tForallWithRef
                    localGhostRef
                    Nothing
                    ( tForallWithRef
                        localValueRef
                        Nothing
                        ( TArrow
                            boolTy
                            ( TArrow
                                (tVarWithRef localValueRef)
                                (tVarWithRef localValueRef)
                            )
                        )
                    )
            producer =
                ResolvedVar
                    { resolvedVarType = localSchemeTy
                    , resolvedVarDetails = EnvId (fixtureExternalEnvRef "method-producer")
                    }
            computedForall =
                ETyInst
                    (EVarNode producer)
                    ( instUnderWithRef
                        localGhostRef
                        (InstApp (tVarWithRef localGhostRef))
                    )
            principal =
                eTyAbsWithRef
                    methodValueRef
                    Nothing
                    computedForall
            targetScheme =
                mkElabSchemeWithRefs
                    [(methodGhostRef, Nothing), (methodValueRef, Nothing)]
                    ( TArrow
                        boolTy
                        ( TArrow
                            (tVarWithRef methodValueRef)
                            (tVarWithRef methodValueRef)
                        )
                    )
            expected =
                eTyAbsWithRef
                    methodGhostRef
                    Nothing
                    ( eTyAbsWithRef
                        methodValueRef
                        Nothing
                        ( ETyInst
                            (EVarNode producer)
                            ( InstSeq
                                InstElim
                                (InstApp (tVarWithRef methodValueRef))
                            )
                        )
                    )
            closed =
                closeTermWithSchemeSubstRefsIfNeeded
                    IntMap.empty
                    targetScheme
                    principal
            env =
                mkTypeCheckEnvWithResolvedTerms
                    [(producer, localSchemeTy)]
                    Map.empty
        closed `shouldBe` expected
        typeCheckWithEnv env closed `shouldBe` Right (schemeToType targetScheme)

    it "keeps an unrelated local Gamma inside an exact root binder" $ do
        let rootRef = typeRef 664 "a"
            localRef = typeRef 665 "e"
            localTerm =
                eTyAbsWithRef
                    localRef
                    (Just builtinIntTy)
                    ( ETyInst
                        (ELit (LInt 1))
                        (instAbstrWithRef localRef)
                    )
            scheme =
                mkElabSchemeWithRefs
                    [(rootRef, Nothing)]
                    ( tForallWithRef
                        localRef
                        (Just builtinIntTy)
                        (tVarWithRef localRef)
                    )
            expected =
                eTyAbsWithRef rootRef Nothing localTerm
        constructTermWithSchemeSubstRefsByBinderRoutes
            []
            IntMap.empty
            scheme
            localTerm
            `shouldBe` expected
        typeCheck expected `shouldBe` Right (schemeToType scheme)

    it "reuses an exact root binder only through its construction route" $ do
        let rootRef = typeRef 666 "result"
            sourceRef = typeRef 667 "result"
            sourceTerm =
                eTyAbsWithRef
                    sourceRef
                    (Just builtinIntTy)
                    ( ETyInst
                        (ELit (LInt 1))
                        (instAbstrWithRef sourceRef)
                    )
            scheme =
                mkElabSchemeWithRefs
                    [(rootRef, Just builtinIntTy)]
                    (tVarWithRef rootRef)
            expected =
                eTyAbsWithRef
                    rootRef
                    (Just builtinIntTy)
                    ( ETyInst
                        (ELit (LInt 1))
                        (instAbstrWithRef rootRef)
                    )
        constructTermWithSchemeSubstRefsByBinderRoutes
            [(sourceRef, rootRef)]
            IntMap.empty
            scheme
            sourceTerm
            `shouldBe` expected
        typeCheck expected `shouldBe` Right (schemeToType scheme)

    it "inserts a missing scheme binder after an exact existing prefix" $ do
        let outerRef = typeRef 668 "outer"
            innerRef = typeRef 669 "inner"
            term = eTyAbsWithRef outerRef Nothing (ELit (LInt 1))
            scheme =
                mkElabSchemeWithRefs
                    [(outerRef, Nothing), (innerRef, Nothing)]
                    builtinIntTy
            expected =
                eTyAbsWithRef
                    outerRef
                    Nothing
                    (eTyAbsWithRef innerRef Nothing (ELit (LInt 1)))
        constructTermWithSchemeSubstRefsByBinderRoutes
            []
            IntMap.empty
            scheme
            term
            `shouldBe` expected
        typeCheck expected `shouldBe` Right (schemeToType scheme)

    it "publishes a computed forall without redeclaring its binder identity" $ do
        let outerRef = typeRef 670 "outer"
            innerRef = typeRef 671 "inner"
            ignored = generatedResolvedLocal 672 "$ignored#0" "ignored" builtinIntTy
            returned = generatedResolvedLocal 673 "$returned#0" "returned" (tVarWithRef outerRef)
            producer =
                EApp
                    ( ELam
                        ignored
                        ( eTyAbsWithRef
                            outerRef
                            Nothing
                            (ELam returned (EVarNode returned))
                        )
                    )
                    (ELit (LInt 0))
            scheme =
                mkElabSchemeWithRefs
                    [(outerRef, Nothing), (innerRef, Nothing)]
                    (TArrow (tVarWithRef outerRef) (tVarWithRef outerRef))
            emptyTcEnv = mkTypeCheckEnvWithResolvedTerms [] Map.empty
            published =
                constructTermWithSchemeSubstRefsAtPublication
                    emptyTcEnv
                    IntMap.empty
                    scheme
                    producer
        typeCheckWithEnv emptyTcEnv published
            `shouldBe` Right (schemeToType scheme)
        case published of
            ETyAbsRef publishedOuter _
                (ETyAbsRef publishedInner _
                    (ETyInst
                        (EApp (ELam _ (ETyAbsRef producerRef _ _)) _)
                        (InstApp (TVarRef instantiatedRef)))) -> do
                    ElabTypes.typeBinderRefsSameIdentity publishedOuter outerRef
                        `shouldBe` True
                    ElabTypes.typeBinderRefsSameIdentity publishedInner innerRef
                        `shouldBe` True
                    ElabTypes.typeBinderRefsSameIdentity instantiatedRef outerRef
                        `shouldBe` True
                    ElabTypes.typeBinderRefsSameIdentity producerRef outerRef
                        `shouldBe` False
            _ -> expectationFailure ("unexpected publication term: " ++ show published)

    it "constructs a missing bounded forall after an existing abstraction prefix" $ do
        let sourceOuter = typeRef 649 "source-a"
            targetOuter = typeRef 650 "a"
            targetInner = typeRef 651 "b"
            term = eTyAbsWithRef sourceOuter Nothing (ELit (LInt 1))
            scheme =
                mkElabSchemeWithRefs
                    [(targetOuter, Nothing), (targetInner, Just builtinIntTy)]
                    builtinIntTy
            expected =
                eTyAbsWithRef
                    targetOuter
                    Nothing
                    (eTyAbsWithRef targetInner (Just builtinIntTy) (ELit (LInt 1)))
        alignTopTyAbsToScheme scheme term `shouldBe` Just expected
        closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term `shouldBe` expected
        typeCheck expected `shouldBe` Right (schemeToType scheme)

    it "does not accept an ordinary ill-typed body as a structural abstraction spine" $ do
        let refA = typeRef 652 "a"
            badBody = EApp (ELit (LInt 1)) (ELit (LInt 2))
            term = eTyAbsWithRef refA Nothing badBody
            scheme = mkElabSchemeWithRefs [(refA, Nothing)] builtinIntTy
        alignTopTyAbsToScheme scheme term `shouldBe` Nothing

    it "does not let a deferred hole mask an ill-typed sibling" $ do
        let refA = typeRef 655 "a"
            deferredRef = deferredRefFromIdentity (UniqueIdentity 656) "$deferred"
            deferredBody = ETyInst (mkDeferredVarWithRef deferredRef) InstElim
            badFunction = EApp (ELit (LInt 1)) (ELit (LInt 2))
            term = eTyAbsWithRef refA Nothing (EApp badFunction deferredBody)
            scheme = mkElabSchemeWithRefs [(refA, Nothing)] builtinIntTy
        alignTopTyAbsToScheme scheme term `shouldBe` Nothing

    it "does not let a deferred argument mask an invalid abstraction instantiation" $ do
        let sourceRef = typeRef 657 "source-a"
            targetRef = typeRef 658 "a"
            deferredRef = deferredRefFromIdentity (UniqueIdentity 659) "$deferred"
            ignored = generatedResolvedLocal 660 "$ignored#0" "ignored" TBottom
            stableIntBody =
                EApp
                    (ELam ignored (ELit (LInt 1)))
                    (mkDeferredVarWithRef deferredRef)
            term =
                eTyAbsWithRef
                    sourceRef
                    (Just boolTy)
                    (ETyInst stableIntBody (instAbstrWithRef sourceRef))
            scheme =
                mkElabSchemeWithRefs
                    [(targetRef, Just boolTy)]
                    (tVarWithRef targetRef)
        alignTopTyAbsToScheme scheme term `shouldBe` Nothing

    it "does not add a scheme abstraction solely because its binder was freshened" $ do
        let refA = typeRef 653 "a"
            refB = typeRef 654 "b"
            term =
                eTyAbsWithRef
                    refA
                    Nothing
                    (eTyAbsWithRef refB Nothing (ELit (LInt 1)))
            scheme = mkElabSchemeWithRefs [(refA, Nothing)] builtinIntTy
            leadingTypeAbsCount :: XmlfTerm -> Int
            leadingTypeAbsCount candidate =
                case candidate of
                    ETyAbsRef _ _ body -> 1 + leadingTypeAbsCount body
                    _ -> 0
        leadingTypeAbsCount
            (closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term)
            `shouldBe` 2

    it "materializes vacuous scheme binders as operational type abstractions" $ do
        let refA = typeRef 640 "a"
            scheme = mkElabSchemeWithRefs [(refA, Nothing)] builtinIntTy
            closed = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme (ELit (LInt 1))
        closed `shouldBe` eTyAbsWithRef refA Nothing (ELit (LInt 1))
        typeCheck closed `shouldBe` Right (tForallWithRef refA Nothing builtinIntTy)

    it "rebinds a first-class forall result to the published scheme identity" $ do
        let sourceRef = typeRef 661 "source"
            targetRef = typeRef 662 "target"
            sourceTy =
                tForallWithRef
                    sourceRef
                    Nothing
                    (TArrow (tVarWithRef sourceRef) builtinIntTy)
            producer =
                ResolvedVar
                    { resolvedVarType = sourceTy
                    , resolvedVarDetails = EnvId (fixtureExternalEnvRef "polymorphic-producer")
                    }
            term = EVarNode producer
            scheme =
                mkElabSchemeWithRefs
                    [(targetRef, Nothing)]
                    (TArrow (tVarWithRef targetRef) builtinIntTy)
            expected =
                eTyAbsWithRef
                    targetRef
                    Nothing
                    (ETyInst term (InstApp (tVarWithRef targetRef)))
            closed =
                closeTermWithSchemeSubstRefsIfNeeded
                    IntMap.empty
                    scheme
                    term
            env = mkTypeCheckEnvWithResolvedTerms [(producer, sourceTy)] Map.empty
        closed `shouldBe` expected
        typeCheckWithEnv env closed `shouldBe` Right (schemeToType scheme)

    it "preserves type binder refs when deriving schemes from types" $ do
        let refA = typeRef 44 "a"
            scheme = schemeFromType (tForallWithRef refA Nothing (tVarWithRef refA))
        map fst (schemeBinderRefs scheme) `shouldBe` [refA]

    it "does not capture external free refs into vacuous scheme binders" $ do
        let quantified = typeRef 641 "a"
            external = typeRef 642 "outer"
            ty =
                tForallWithRef
                    quantified
                    Nothing
                    (TArrow (tVarWithRef external) (tVarWithRef external))
        schemeToType (schemeFromType ty) `shouldBe` ty

    it "specializes an eliminated flexible abstraction to its bound" $ do
        let refA = typeRef 67 "a"
            polymorphic =
                eTyAbsWithRef
                    refA
                    (Just builtinIntTy)
                    (ETyInst (ELit (LInt 1)) (instAbstrWithRef refA))
            scheme = mkElabSchemeWithRefs [] builtinIntTy
            closed = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme polymorphic
        case closed of
            ETyInst original (InstApp argument) -> do
                original `shouldBe` polymorphic
                argument `shouldBe` builtinIntTy
            other -> expectationFailure ("Expected bound specialization, got: " ++ show other)
        typeCheck closed `shouldBe` Right builtinIntTy

    it "checks type abstraction bounds by binder identity instead of spelling" $ do
        let refAbs = typeRef 45 "a"
            refOther = typeRef 46 "a"
            bound = TArrow (tVarWithRef refOther) intTy
            term = eTyAbsWithRef refAbs (Just bound) (ELit (LInt 1))
        typeCheck term `shouldBe` Right (tForallWithRef refAbs (Just bound) builtinIntTy)

    it "freshens earlier scheme binders through later bounds by identity" $ do
        let refA = typeRef 47 "a"
            refB = typeRef 48 "b"
            reservedA = typeRef 49 "a"
            scheme =
                schemeFromType
                    ( tForallWithRef
                        refA
                        Nothing
                        (tForallWithRef refB (Just (TArrow (tVarWithRef refA) intTy)) intTy)
                    )
            -- Keep the colliding binder inside a closed term whose result
            -- actually matches the scheme body.  The former lambda fixture
            -- had type @reservedA -> Int@ while the scheme body was @Int@,
            -- so it only exercised the retired unchecked-wrap fallback.
            term =
                ETyInst
                    (eTyAbsWithRef reservedA Nothing (ELit (LInt 1)))
                    (InstApp intTy)
            closed = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term
        case closed of
            ETyAbsRef outer Nothing (ETyAbsRef inner (Just bound) (ETyInst (ETyAbsRef reserved Nothing (ELit (LInt 1))) (InstApp instTy))) -> do
                typeBinderRefIdentity outer `shouldBe` typeBinderRefIdentity refA
                typeBinderRefName outer `shouldBe` "a1"
                typeBinderRefIdentity inner `shouldBe` typeBinderRefIdentity refB
                typeBinderRefName inner `shouldBe` "b"
                bound `shouldBe` TArrow (tVarWithRef outer) intTy
                reserved `shouldBe` reservedA
                instTy `shouldBe` intTy
            other -> expectationFailure ("Expected freshened two-binder closure, got: " ++ show other)
        case typeCheck closed of
            Right ty -> ty `shouldSatisfy` TypeOps.alphaEqType (schemeToType scheme)
            Left err -> expectationFailure ("Freshened closure failed typecheck: " ++ show err)

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

    it "aligns same-named free type variables to scheme body by identity" $ do
        let refA = typeRef 57 "a"
            refB = typeRef 58 "b"
            sourceA = typeRef 59 "a"
            sourceB = typeRef 60 "a"
            sourceParamTy = TArrow (tVarWithRef sourceA) (tVarWithRef sourceB)
            param = resolvedLocal "$x#0" "runtime-x" sourceParamTy
            scheme =
                mkElabSchemeWithRefs
                    [(refA, Nothing), (refB, Nothing)]
                    (TArrow (TArrow (tVarWithRef refB) (tVarWithRef refA)) (TArrow (tVarWithRef refB) (tVarWithRef refA)))
            term = ELam param (EVarNode param)
            closed = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term
        case closed of
            ETyAbsRef outer Nothing (ETyAbsRef inner Nothing (ELam param' (EVarNode occurrence'))) -> do
                typeBinderRefIdentity outer `shouldBe` typeBinderRefIdentity refA
                typeBinderRefName outer `shouldBe` "a1"
                typeBinderRefIdentity inner `shouldBe` typeBinderRefIdentity refB
                typeBinderRefName inner `shouldBe` "b"
                let expectedParamTy = TArrow (tVarWithRef inner) (tVarWithRef outer)
                resolvedVarType param' `shouldBe` expectedParamTy
                resolvedVarType occurrence' `shouldBe` expectedParamTy
            other -> expectationFailure ("Expected identity-aligned same-named free type variables, got: " ++ show other)

    it "freshens type abstractions through same-named distinct binders by identity" $ do
        let outer = typeRef 50 "a"
            inner = typeRef 51 "a"
            reserved = typeRef 52 "a"
            captured = generatedResolvedLocal 50 "$env#0" "runtime-env" (tVarWithRef reserved)
            env = mkTypeCheckEnvWithResolvedTerms [(captured, tVarWithRef reserved)] Map.empty
            param =
                generatedResolvedLocal
                    51
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

    it "threads one identity supply across roots that collide with a visible binder" $ do
        let capturedRef = typeRef 991870 "a"
            captured = generatedResolvedLocal 991871 "$captured#0" "captured" (tVarWithRef capturedRef)
            env = mkTypeCheckEnvWithResolvedTerms [(captured, tVarWithRef capturedRef)] Map.empty
            param = generatedResolvedLocal 991872 "$x#0" "runtime-x" (tVarWithRef capturedRef)
            root = eTyAbsWithRef capturedRef Nothing (ELam param (EVarNode param))
            runRoots =
                let (first, generator1) =
                        freshenTypeAbsAgainstEnvFromSupply initialIdentityGenerator env root
                    (second, _generator2) =
                        freshenTypeAbsAgainstEnvFromSupply generator1 env root
                 in (leadingRef first, leadingRef second)
            leadingRef term =
                case term of
                    ETyAbsRef ref Nothing _ -> Just ref
                    _ -> Nothing
            expected = runRoots
        replicate 8 runRoots `shouldBe` replicate 8 expected
        case expected of
            (Just firstRef, Just secondRef) -> do
                typeBinderRefIdentity firstRef `shouldNotBe` typeBinderRefIdentity capturedRef
                typeBinderRefIdentity secondRef `shouldNotBe` typeBinderRefIdentity capturedRef
                typeBinderRefIdentity firstRef `shouldNotBe` typeBinderRefIdentity secondRef
            other -> expectationFailure ("Expected two freshened root binders, got: " ++ show other)

    it "freshens type abstraction displays away from visible stable aliases" $ do
        let reserved = typeRef 61 "captured"
            stableAlias = typeBinderIdentityStableName (typeBinderRefIdentity reserved)
            binder = typeRef 62 stableAlias
            captured = generatedResolvedLocal 61 "$env#0" "runtime-env" (tVarWithRef reserved)
            env = mkTypeCheckEnvWithResolvedTerms [(captured, tVarWithRef reserved)] Map.empty
            param = generatedResolvedLocal 62 "$x#0" "runtime-x" (tVarWithRef binder)
            term = eTyAbsWithRef binder Nothing (ELam param (EVarNode param))
        case freshenTypeAbsAgainstEnv env term of
            ETyAbsRef binder' Nothing (ELam param' (EVarNode occurrence')) -> do
                typeBinderRefIdentity binder' `shouldBe` typeBinderRefIdentity binder
                typeBinderRefName binder' `shouldNotBe` stableAlias
                resolvedVarType param' `shouldBe` tVarWithRef binder'
                resolvedVarType occurrence' `shouldBe` tVarWithRef binder'
            other -> expectationFailure ("Expected stable-alias freshened type abstraction, got: " ++ show other)

    it "keeps captured term types authoritative while freshening colliding type binders" $ do
        let capturedRef = typeRef 63 "a"
            captured = generatedResolvedLocal 63 "$captured#0" "captured" (tVarWithRef capturedRef)
            param = generatedResolvedLocal 64 "$x#0" "runtime-x" (tVarWithRef capturedRef)
            term =
                ELam
                    captured
                    ( eTyAbsWithRef
                        capturedRef
                        Nothing
                        (ELam param (EApp (EVarNode param) (EVarNode captured)))
                    )
        case freshenTypeAbsAgainstEnv (mkTypeCheckEnvWithResolvedTerms [] Map.empty) term of
            ELam captured' (ETyAbsRef freshRef Nothing (ELam param' (EApp (EVarNode localOccurrence) (EVarNode capturedOccurrence)))) -> do
                typeBinderRefIdentity freshRef `shouldNotBe` typeBinderRefIdentity capturedRef
                resolvedVarType captured' `shouldBe` tVarWithRef capturedRef
                resolvedVarType param' `shouldBe` tVarWithRef freshRef
                resolvedVarType localOccurrence `shouldBe` tVarWithRef freshRef
                resolvedVarType capturedOccurrence `shouldBe` tVarWithRef capturedRef
            other -> expectationFailure ("Expected environment-aware freshening, got: " ++ show other)

    it "typechecks applications" $ do
        let term = EApp (mkTestLocalLam "x" intTy (mkTestDeferredVar "x")) (ELit (LInt 1))
        typeCheck term `shouldBe` Right intTy

    it "requires an explicit type computation before applying a vacuous forall value" $ do
        let ref = typeRef 991824 "a"
            function = mkTestLocalLam "x" intTy (mkTestDeferredVar "x")
            polymorphicArgument = eTyAbsWithRef ref Nothing (ELit (LInt 1))
        case typeCheck (EApp function polymorphicArgument) of
            Left TCArgumentMismatch{} -> pure ()
            other -> expectationFailure ("expected an explicit forall application boundary, got " ++ show other)
        typeCheck (EApp function (ETyInst polymorphicArgument InstElim))
            `shouldBe` Right intTy

    it "does not erase a forall nested below an application argument arrow" $ do
        let ref = typeRef 991825 "a"
            expectedArgument = TArrow (tForallWithRef ref Nothing intTy) intTy
            function = mkTestLocalLam "f" expectedArgument (ELit (LInt 1))
            monomorphicArgument = mkTestLocalLam "x" intTy (mkTestDeferredVar "x")
        case typeCheck (EApp function monomorphicArgument) of
            Left TCArgumentMismatch{} -> pure ()
            other -> expectationFailure ("expected the nested forall ABI to remain explicit, got " ++ show other)

    it "accepts a nested Church representation transition in an application argument" $ do
        let outerSourceSelf = typeRef 991851 "outer-source-self"
            outerSourceResult = typeRef 991852 "outer-source-result"
            innerSourceSelf = typeRef 991853 "inner-source-self"
            innerSourceResult = typeRef 991854 "inner-source-result"
            outerTargetSelf = typeRef 991855 "outer-target-self"
            outerTargetResult = typeRef 991856 "outer-target-result"
            innerTargetSelf = typeRef 991857 "inner-target-self"
            innerTargetResult = typeRef 991858 "inner-target-result"
            sourceType =
                TMuRef outerSourceSelf $
                    TForallRef outerSourceResult Nothing $
                        TArrow
                            ( TMuRef innerSourceSelf $
                                TForallRef innerSourceResult Nothing $
                                    TArrow
                                        (TVarRef innerSourceResult)
                                        (TVarRef innerSourceResult)
                            )
                            (TVarRef outerSourceResult)
            targetType =
                TMuRef outerTargetSelf $
                    TForallRef outerTargetResult Nothing $
                        TArrow
                            ( TMuRef innerTargetSelf $
                                TArrow
                                    (TVarRef innerTargetResult)
                                    (TVarRef innerTargetResult)
                            )
                            (TVarRef outerTargetResult)
            expectedEvidenceType = TArrow sourceType intTy
            actualEvidenceType = TArrow targetType intTy
            argument =
                generatedResolvedLocal
                    991859
                    "evidence"
                    "evidence"
                    actualEvidenceType
            env =
                mkTypeCheckEnvWithResolvedTerms
                    [(argument, actualEvidenceType)]
                    Map.empty
            function =
                mkTestLocalLam
                    "consume"
                    expectedEvidenceType
                    (ELit (LInt 1))
        typeCheckWithEnv env (EApp function (EVarNode argument))
            `shouldBe` Right intTy

    it "specializes an unbound application variable to the complete impredicative type" $ do
        let quantifiedRef = typeRef 991826 "a"
            flexibleRef = typeRef 991827 "t"
            argumentRef = typeRef 991828 "b"
            polymorphicIdentity =
                eTyAbsWithRef
                    quantifiedRef
                    Nothing
                    (mkTestLocalLam "x" (tVarWithRef quantifiedRef) (mkTestDeferredVar "x"))
            polymorphicArgument = eTyAbsWithRef argumentRef Nothing (ELit (LInt 1))
            expectedResult = tForallWithRef argumentRef Nothing intTy
            term =
                EApp
                    (ETyInst polymorphicIdentity (InstApp (tVarWithRef flexibleRef)))
                    polymorphicArgument
        typeCheck term `shouldBeRightAlphaEq` expectedResult

    it "requires an explicit Hyp computation for a bounded application variable" $ do
        let boundedRef = typeRef 991829 "a"
            parameter = generatedResolvedLocal 991830 "x" "x" (tVarWithRef boundedRef)
            env = mkTypeCheckEnvWithResolvedTerms [] (Map.singleton boundedRef intTy)
            term = EApp (ELam parameter (EVarNode parameter)) (ELit (LInt 1))
        case typeCheckWithEnv env term of
            Left TCArgumentMismatch{} -> pure ()
            other -> expectationFailure ("expected an explicit bounded-variable computation, got " ++ show other)

    it "accepts an argument after its explicit Hyp computation" $ do
        let boundedRef = typeRef 991831 "a"
            parameter = generatedResolvedLocal 991832 "x" "x" (tVarWithRef boundedRef)
            env = mkTypeCheckEnvWithResolvedTerms [] (Map.singleton boundedRef intTy)
            argument = ETyInst (ELit (LInt 1)) (InstAbstrRef boundedRef)
            term = EApp (ELam parameter (EVarNode parameter)) argument
        typeCheckWithEnv env term `shouldBe` Right (tVarWithRef boundedRef)

    it "does not treat a same-named fake IO type identity as builtin opaque IO" $ do
        let fakeIOIdentity = generatedSymbolIdentity 991821 SymbolType "Other" "IO" Nothing
            fakeIOTy = ElabTypes.TConWithIdentity fakeIOIdentity (BaseTy "IO") (intTy :| [])
            builtinIOTy = ElabTypes.TConWithIdentity (builtinTypeIdentity "IO") (BaseTy "IO") (intTy :| [])
            binder = generatedResolvedLocal 991822 "x" "x" fakeIOTy
            arg = generatedResolvedLocal 991823 "arg" "arg" builtinIOTy
            env = mkTypeCheckEnvWithResolvedTerms [(arg, builtinIOTy)] Map.empty
            term = EApp (ELam binder (ELit (LInt 1))) (EVarNode arg)
        case typeCheckWithEnv env term of
            Left (TCArgumentMismatch expected actual) -> do
                expected `shouldBe` fakeIOTy
                actual `shouldBe` builtinIOTy
            other -> expectationFailure ("Expected fake IO identity mismatch, got: " ++ show other)

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

    it "keeps discard let semantics on the local ref when names are stale" $ do
        let refA = typeRef 84 "a"
            freeA = tVarWithRef refA
            schTy = tForallWithRef refA Nothing (TArrow freeA freeA)
            scheme = schemeFromType schTy
            discardBinder =
                renameResolvedLocalVar
                    "$stale_discard"
                    (generatedResolvedLocal 85 "_" "_" schTy)
            concreteParam = generatedResolvedLocal 86 "x" "x" intTy
            concreteRhs = ELam concreteParam (EVarNode concreteParam)
        typeCheck (ELet discardBinder scheme concreteRhs (ELit (LBool True))) `shouldBe` Right boolTy

    it "typechecks type abstractions" $ do
        let term = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
        case typeCheck term of
            Right (TForallRef ref Nothing (TArrow (TVarRef argRef) (TVarRef resultRef))) -> do
                typeBinderRefName ref `shouldBe` "a"
                argRef `shouldBe` ref
                resultRef `shouldBe` ref
            other -> expectationFailure ("Expected identity-backed type abstraction type, got: " ++ show other)

    it "rejects a locally emitted binder captured through a used ambient bound" $ do
        let localRef = typeRef 991910 "local"
            ambientRef = typeRef 991911 "ambient"
            env =
                mkTypeCheckEnvWithResolvedTerms
                    []
                    (Map.singleton ambientRef (tVarWithRef localRef))
            param = generatedResolvedLocal 991912 "$x" "x" (tVarWithRef ambientRef)
            term = ETyAbsRef localRef Nothing (ELam param (EVarNode param))
        typeCheckWithEnv env term
            `shouldBe` Left (TCTypeAbsVarInScope "local")

    it "typechecks instantiations" $ do
        let term = ETyInst (mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))) (InstApp intTy)
        typeCheck term `shouldBe` Right (TArrow intTy intTy)

    it "typechecks internal recursive roll/unroll runtime terms" $ do
        typeCheck (ERoll recursiveIntTy recursiveBody) `shouldBe` Right recursiveIntTy
        typeCheck (EUnroll (ERoll recursiveIntTy recursiveBody))
            `shouldBe` Right (TArrow recursiveIntTy intTy)

    it "does not treat a recursive lower bound as an implicit downcast" $ do
        let flexibleRef = typeRef 991900 "a"
            env =
                insertTypeBindingRef
                    flexibleRef
                    recursiveIntTy
                    (mkTypeCheckEnvWithResolvedTerms [] Map.empty)
            param = generatedResolvedLocal 991901 "$x" "x" (TVarRef flexibleRef)
        case typeCheckWithEnv env (ELam param (EUnroll (EVarNode param))) of
            Left (TCExpectedRecursive (TVarRef actualRef))
                | actualRef == flexibleRef -> pure ()
            other ->
                expectationFailure
                    ("Expected the bare flexible scrutinee to remain rejected, got: " ++ show other)

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

    it "rejects InstApp when the term type has no leading forall" $ do
        case typeCheck (ETyInst (ELit (LInt 1)) (InstApp intTy)) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected non-forall InstApp rejection, got: " ++ show other)

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
        typeCheck term `shouldBe` Right builtinIntTy

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

        it "specializes dual annotated coercion consumers through normalized InstApp coercions" $ do
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

            case runPipelineElab Set.empty normExpr of
                Left err -> expectationFailure ("Canonical pipeline failed:\n" ++ renderPipelineError err)
                Right (term, ty) -> do
                    ty `shouldSatisfy` TypeOps.alphaEqType boolTy
                    typeCheck term `shouldBe` Right ty
                    let instApps = termInstAppTypes term
                    instApps `shouldSatisfy` any (TypeOps.alphaEqType intTy)
                    instApps `shouldSatisfy` any (TypeOps.alphaEqType boolTy)
