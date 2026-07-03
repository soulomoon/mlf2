{-# LANGUAGE GADTs #-}

module ProgramSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.List (isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (NodeId (..))
import MLF.API
    ( Lit (..)
    , SrcTy (..)
    , parseLocatedProgramWithFile
    , parseRawProgram
    , prettyProgram
    , renderProgramParseError
    )
import MLF.Frontend.Program.Check (checkResolvedProgram)
import MLF.Frontend.Program.Elaborate (ElaborateScope, constructorTypeView, elaborateScopeRuntimeTypeViews, lowerConstructorBinding, lowerExprBinding, lowerResolvedConstrainedExprBinding, lowerType, matchTypeViewsAgainstIdentity, mkElaborateScope, sourceTypeIdentityInScope, sourceTypeViewInScope)
import MLF.Frontend.Program.Finalize
    ( finalizeBindingAllowOpaqueWithModuleContext
    , finalizeBindingWithContext
    , finalizeBindingsAllowOpaqueWithContext
    , mkFinalizeContext
    , mkModuleFinalizeContext
    , recoverSourceType
    , resolvedForallSubst
    , sourceForallMatches
    , sourceForallMatchesInScope
    , stripVacuousForallsAndTypeAbs
    , typeViewToElabType
    )
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Resolve (resolveProgram)
import MLF.Frontend.Program.Run (runCheckedProgramOutput)
import MLF.Frontend.Program.Types
    ( ConstructorInfo (..)
    , constructorRefSymbol
    , ConstructorShape (..)
    , ClassInfo (..)
    , DataInfo (..)
    , DeferredCaseCall (..)
    , DeferredConstructorCall (..)
    , DeferredMethodEvidence (..)
    , DeferredMethodCall (..)
    , DeferredRef
    , deferredRefFromIdentity
    , deferredRefIdentity
    , deferredRefName
    , DeferredProgramObligation (..)
    , EvidenceInfo (..)
    , EvidenceMethod (..)
    , IdDetails (..)
    , InstanceInfo (..)
    , LocalRef
    , localRefFromIdentity
    , localRefIdentity
    , localRefName
    , LoweredBinding (..)
    , MethodInfo (..)
    , ResolvedLocalSymbols (..)
    , ResolvedModuleDiagnosticAdapter (..)
    , ResolvedSemanticModule (..)
    , ResolvedVar (..)
    , ValueInfo (..)
    , checkedBindingName
    , ctorName
    , constructorOwnerRuntimeTypeTrackable
    , constructorShapeFromInfo
    , dataName
    , resolvedModuleName
    , resolvedModuleReferences
    , resolvedModuleSyntax
    )
import qualified MLF.Frontend.Program.Types as ProgramTypes
import MLF.Frontend.Program.Prelude (withPrelude, withPreludeLocated)
import MLF.Frontend.Symbol (renameSymbolDefiningName, symbolIdentityStableName)
import MLF.Frontend.Syntax (ResolvedSrcTy (..), ResolvedTypeBinderRef, SrcBound (..), SrcType, mkSrcBound, resolvedTypeBinderIdentity, resolvedTypeBinderName, resolvedTypeBinderRefFromIdentity)
import qualified MLF.Frontend.Syntax as Surface
import MLF.Frontend.Syntax.Program
import MLF.Frontend.TypeLevel
    ( TypeLevelKind (..)
    , TypeLevelNormalizeError (..)
    , TypeLevelPattern (..)
    , TypeLevelTy (..)
    , familyDeclEquations
    , familyDeclName
    , familyDeclParams
    , familyDeclResultKind
    , familyEquationPatterns
    , familyEquationRhs
    )
import MLF.Pipeline
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import qualified MLF.Types.Elab as Elab
import MLF.Types.Identity
    ( LocalIdentity (..)
    , primitiveRefFromSymbol
    , localRefFromNodeId
    , localRefMatchesNodeId
    , localIdentityStableUnique
    , renameLocalRef
    , typeBinderIdentityGeneratedUnique
    , typeBinderIdentityFromNode
    , typeBinderIdentityStableName
    )
import MLF.Program.CLI (runProgramFile)
import Test.Hspec

import ElabTermTestSupport
    ( generatedLocalRefForName
    , generatedResolvedLocal
    , mkTestDeferredVar
    , mkTestTyAbs
    , testTForall
    , testTVar
    )
import Parity.ProgramMatrix

generatedSymbolIdentity :: Int -> SymbolNamespace -> String -> String -> Maybe SymbolOwnerIdentity -> SymbolIdentity
generatedSymbolIdentity unique namespace moduleName name owner =
    symbolIdentityFromParts (UniqueIdentity unique) namespace moduleName name owner

generatedSymbolOwnerType :: Int -> String -> String -> SymbolOwnerIdentity
generatedSymbolOwnerType unique moduleName name =
    SymbolOwnerType (generatedSymbolIdentity unique SymbolType moduleName name Nothing)

resolvedTypeBinderRef :: UniqueIdentity -> String -> ResolvedTypeBinderRef
resolvedTypeBinderRef identity name =
    resolvedTypeBinderRefFromIdentity (typeBinderIdentityFromUnique identity) name

poisonResolvedEqIdentityNames :: ResolvedProgram -> ResolvedProgram
poisonResolvedEqIdentityNames resolved =
    resolved {resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
  where
    poisonModule resolvedModule =
        resolvedModule
            { resolvedModuleSemantic =
                (resolvedModuleSemantic resolvedModule)
                    { resolvedSemanticModuleSyntax =
                        poisonSyntax (resolvedSemanticModuleSyntax (resolvedModuleSemantic resolvedModule))
                    }
            }

    poisonSyntax syntax =
        syntax {moduleDecls = map poisonDecl (moduleDecls syntax)}

    poisonDecl decl =
        case decl of
            DeclClass classDecl ->
                DeclClass
                    classDecl
                        { classDeclName = poisonClassSymbol (classDeclName classDecl)
                        , classDeclMethods = map poisonMethodSig (classDeclMethods classDecl)
                        }
            DeclData dataDecl ->
                DeclData
                    dataDecl
                        {dataDeclDeriving = map poisonClassSymbol (dataDeclDeriving dataDecl)}
            _ -> decl

    poisonMethodSig sig =
        sig {methodSigName = poisonMethodSymbol (methodSigName sig)}

    poisonClassSymbol =
        poisonSymbolIdentityName
            (\identity -> symbolNamespace identity == SymbolClass && symbolDefiningName identity == "Eq")
            "$stale_eq_class_identity_name"

    poisonMethodSymbol =
        poisonSymbolIdentityName
            (\identity -> symbolNamespace identity == SymbolMethod && symbolDefiningName identity == "eq")
            "$stale_eq_method_identity_name"

poisonResolvedDataParamBinderName :: String -> String -> ResolvedProgram -> ResolvedProgram
poisonResolvedDataParamBinderName targetDataName replacement resolved =
    resolved {resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
  where
    poisonModule resolvedModule =
        resolvedModule
            { resolvedModuleSemantic =
                (resolvedModuleSemantic resolvedModule)
                    { resolvedSemanticModuleSyntax =
                        poisonSyntax (resolvedSemanticModuleSyntax (resolvedModuleSemantic resolvedModule))
                    }
            }

    poisonSyntax syntax =
        syntax {moduleDecls = map poisonDecl (moduleDecls syntax)}

    poisonDecl decl =
        case decl of
            DeclData dataDecl
                | dataDeclDisplayName dataDecl == targetDataName ->
                    DeclData dataDecl {dataDeclParams = map poisonParam (dataDeclParams dataDecl)}
            _ -> decl

    poisonParam param =
        case param of
            ResolvedTypeParam ref kind0 ->
                ResolvedTypeParam
                    (resolvedTypeBinderRefFromIdentity (resolvedTypeBinderIdentity ref) replacement)
                    kind0
            _ -> param

poisonResolvedClassParamBinderName :: String -> String -> ResolvedProgram -> ResolvedProgram
poisonResolvedClassParamBinderName targetClassName replacement resolved =
    resolved {resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
  where
    poisonModule resolvedModule =
        resolvedModule
            { resolvedModuleSemantic =
                (resolvedModuleSemantic resolvedModule)
                    { resolvedSemanticModuleSyntax =
                        poisonSyntax (resolvedSemanticModuleSyntax (resolvedModuleSemantic resolvedModule))
                    }
            }

    poisonSyntax syntax =
        syntax {moduleDecls = map poisonDecl (moduleDecls syntax)}

    poisonDecl decl =
        case decl of
            DeclClass classDecl
                | classDeclDisplayName classDecl == targetClassName ->
                    DeclClass classDecl {classDeclParams = fmap poisonParam (classDeclParams classDecl)}
            _ -> decl

    poisonParam param =
        case param of
            ResolvedTypeParam ref kind0 ->
                ResolvedTypeParam
                    (resolvedTypeBinderRefFromIdentity (resolvedTypeBinderIdentity ref) replacement)
                    kind0
            _ -> param

poisonSymbolIdentityName :: (SymbolIdentity -> Bool) -> String -> ResolvedSymbol -> ResolvedSymbol
poisonSymbolIdentityName predicate replacement symbol
    | predicate identity =
        mapResolvedSymbolIdentity (renameSymbolDefiningName replacement) symbol
    | otherwise = symbol
  where
    identity = resolvedSymbolIdentity symbol

spec :: Spec
spec = do
    describe "MLF.Program source type finalization" $ do
        it "matches variable-headed applications through ∀ alpha-renaming" $ do
            let expected =
                    STForall
                        "f"
                        Nothing
                        ( STArrow
                            (STVarApp "f" (STVar "a" :| []))
                            (STVarApp "f" (STVar "a" :| []))
                        )
                actual =
                    STForall
                        "g"
                        Nothing
                        ( STArrow
                            (STVarApp "g" (STVar "a" :| []))
                            (STVarApp "g" (STVar "a" :| []))
                        )
            sourceForallMatches expected actual `shouldBe` True

        it "matches type lambdas through alpha-renaming" $ do
            let expected =
                    STTyLam
                        "f"
                        ( STArrow
                            (STVar "f")
                            (STVarApp "f" (STBase "Int" :| []))
                        )
                actual =
                    STTyLam
                        "g"
                        ( STArrow
                            (STVar "g")
                            (STVarApp "g" (STBase "Int" :| []))
                        )
            sourceForallMatches expected actual `shouldBe` True

        it "matches repeated substitutions whose ∀ bounds rename their own binder" $ do
            let bounded name =
                    STForall
                        name
                        (Just (mkSrcBound (STArrow (STVar name) (STBase "Int"))))
                        (STVar name)
                expected =
                    STForall
                        "f"
                        Nothing
                        (STArrow (STVar "f") (STVar "f"))
                actual =
                    STArrow
                        (bounded "a")
                        (bounded "b")
            sourceForallMatches expected actual `shouldBe` True

        it "rejects alpha-renamed foralls with incompatible bounds" $ do
            let bounded name bound =
                    STForall
                        name
                        (Just (mkSrcBound bound))
                        (STArrow (STVar name) (STVar name))
                expected = bounded "f" (STBase "Int")
                actual = bounded "g" (STBase "Bool")
            sourceForallMatches expected actual `shouldBe` False

        it "does not create type-view substitution keys by ordinary name" $ do
            let sourceView = ProgramTypes.mkTypeView (STVar "a") (STVar "a")
            ProgramTypes.typeViewSubstKeyFor sourceView "a" `shouldBe` Nothing

        it "builds type-view substitutions from binder identities" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991622)
                replacement = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                subst = ProgramTypes.typeViewSubstFromParamIdentities (identity :| []) (replacement :| [])
            ProgramTypes.lookupTypeViewSubst (ProgramTypes.typeViewSubstKeyForIdentity identity) subst
                `shouldBe` Just replacement

        it "hydrates type-binder substitutions from replacement identity payloads" $ do
            let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991632)
                replacementIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991633)
                replacementStableName = typeBinderIdentityStableName replacementIdentity
                replacement =
                    (ProgramTypes.mkTypeView (STVar "display") (STVar replacementStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "display" replacementIdentity
                        }
                subst =
                    ProgramTypes.typeBinderSubstFromTypeViewSubst
                        [("source", sourceIdentity)]
                        (Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity sourceIdentity) replacement)
            ProgramTypes.lookupTypeBinderSubstByIdentity sourceIdentity subst
                `shouldBe` Just (STVar replacementStableName)

        it "does not key generated stable binder names without metadata" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991601)
                stableName = typeBinderIdentityStableName identity
                sourceView = ProgramTypes.mkTypeView (STVar stableName) (STVar stableName)
            ProgramTypes.typeViewSubstKeyFor sourceView stableName
                `shouldBe` Nothing

        it "does not treat stable binder spelling as identity without metadata" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991603)
                stableName = typeBinderIdentityStableName identity
                sourceView = ProgramTypes.mkTypeView (STVar stableName) (STVar stableName)
            ProgramTypes.typeViewIsBareBinderIdentity identity sourceView
                `shouldBe` False
            ProgramTypes.typeViewMentionsFreeBinderIdentity identity sourceView
                `shouldBe` False

        it "keys generated stable binder type-view substitutions by identity without metadata" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991613)
                replacement = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                subst =
                    ProgramTypes.typeBinderSubstFromTypeViewSubst
                        []
                        (Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity identity) replacement)
                viewSubst =
                    ProgramTypes.typeBinderSubstToTypeViewSubstWith
                        (\ty -> ProgramTypes.mkTypeView ty ty)
                        subst
            ProgramTypes.lookupTypeBinderSubstByIdentity identity subst
                `shouldBe` Just (STBase "Int")
            ProgramTypes.lookupTypeViewSubst (ProgramTypes.typeViewSubstKeyForIdentity identity) viewSubst
                `shouldBe` Just replacement

        it "applies identity-bearing type-binder substitutions through stable names at the string substitution boundary" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991618)
                stableName = typeBinderIdentityStableName identity
                replacementTy = STBase "Int"
                subst =
                    ProgramTypes.insertTypeBinderSubstWithIdentity
                        identity
                        "a"
                        replacementTy
                        ProgramTypes.emptyTypeBinderSubst
            ProgramTypes.applyTypeBinderSubst subst (STVar stableName) `shouldBe` replacementTy

        it "does not apply ambiguous type-binder display substitutions at the string substitution boundary" $ do
            let firstIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991630)
                secondIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991631)
                firstStableName = typeBinderIdentityStableName firstIdentity
                secondStableName = typeBinderIdentityStableName secondIdentity
                subst =
                    ProgramTypes.insertTypeBinderSubstWithIdentity
                        secondIdentity
                        "a"
                        (STBase "Bool")
                        ( ProgramTypes.insertTypeBinderSubstWithIdentity
                            firstIdentity
                            "a"
                            (STBase "Int")
                            ProgramTypes.emptyTypeBinderSubst
                        )
            ProgramTypes.applyTypeBinderSubst subst (STVar "a")
                `shouldBe` STVar "a"
            ProgramTypes.applyTypeBinderSubst subst (STArrow (STVar firstStableName) (STVar secondStableName))
                `shouldBe` STArrow (STBase "Int") (STBase "Bool")

        it "does not apply identity-keyed type-view display substitutions through metadata-free stable names" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991619)
                stableName = typeBinderIdentityStableName identity
                replacement = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                source = ProgramTypes.mkTypeView (STVar stableName) (STVar stableName)
            ProgramTypes.typeViewDisplay
                ( ProgramTypes.applyTypeViewSubst
                    (Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity identity) replacement)
                    source
                )
                `shouldBe` STVar stableName

        it "applies identity-keyed type-view substitutions through paired metadata" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991619)
                stableName = typeBinderIdentityStableName identity
                replacement = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                source =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar stableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" identity
                        }
                actual =
                    ProgramTypes.applyTypeViewSubst
                        (Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity identity) replacement)
                        source
            ProgramTypes.typeViewDisplay actual `shouldBe` STBase "Int"
            ProgramTypes.typeViewIdentity actual `shouldBe` STBase "Int"

        it "does not apply ambiguous type-view display substitutions by arbitrary identity order" $ do
            let leftIdentity = typeBinderIdentityFromNode (NodeId 992510)
                rightIdentity = typeBinderIdentityFromNode (NodeId 992511)
                leftStableName = typeBinderIdentityStableName leftIdentity
                rightStableName = typeBinderIdentityStableName rightIdentity
                source =
                    ( ProgramTypes.mkTypeView
                        (STArrow (STVar "a") (STVar "a"))
                        (STArrow (STVar leftStableName) (STVar rightStableName))
                    )
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList
                                [ (leftStableName, leftIdentity)
                                , (rightStableName, rightIdentity)
                                ]
                        }
                subst =
                    Map.fromList
                        [ (ProgramTypes.typeViewSubstKeyForIdentity leftIdentity, ProgramTypes.mkTypeView (STBase "Int") (STBase "Int"))
                        , (ProgramTypes.typeViewSubstKeyForIdentity rightIdentity, ProgramTypes.mkTypeView (STBase "Bool") (STBase "Bool"))
                        ]
                actual = ProgramTypes.applyTypeViewSubst subst source
            ProgramTypes.typeViewDisplay actual `shouldBe` STArrow (STVar "a") (STVar "a")
            ProgramTypes.typeViewIdentity actual `shouldBe` STArrow (STBase "Int") (STBase "Bool")

        it "does not let ambiguous display pairs overwrite identity substitutions" $ do
            let leftIdentity = typeBinderIdentityFromNode (NodeId 992512)
                rightIdentity = typeBinderIdentityFromNode (NodeId 992513)
                leftStableName = typeBinderIdentityStableName leftIdentity
                rightStableName = typeBinderIdentityStableName rightIdentity
                source =
                    ( ProgramTypes.mkTypeView
                        (STArrow (STVar "a") (STVar "a"))
                        (STArrow (STVar leftStableName) (STVar rightStableName))
                    )
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList
                                [ (leftStableName, leftIdentity)
                                , ("a", rightIdentity)
                                ]
                        }
                subst =
                    Map.fromList
                        [ (ProgramTypes.typeViewSubstKeyForIdentity leftIdentity, ProgramTypes.mkTypeView (STBase "Int") (STBase "Int"))
                        , (ProgramTypes.typeViewSubstKeyForIdentity rightIdentity, ProgramTypes.mkTypeView (STBase "Bool") (STBase "Bool"))
                        ]
                actual = ProgramTypes.applyTypeViewSubst subst source
            ProgramTypes.typeViewIdentity actual `shouldBe` STArrow (STBase "Int") (STBase "Bool")

        it "does not replace ambiguous display names for a single identity substitution" $ do
            let leftIdentity = typeBinderIdentityFromNode (NodeId 992514)
                rightIdentity = typeBinderIdentityFromNode (NodeId 992515)
                leftStableName = typeBinderIdentityStableName leftIdentity
                rightStableName = typeBinderIdentityStableName rightIdentity
                source =
                    ( ProgramTypes.mkTypeView
                        (STArrow (STVar "a") (STVar "a"))
                        (STArrow (STVar leftStableName) (STVar rightStableName))
                    )
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList
                                [ (leftStableName, leftIdentity)
                                , ("a", rightIdentity)
                                ]
                        }
                subst =
                    Map.singleton
                        (ProgramTypes.typeViewSubstKeyForIdentity leftIdentity)
                        (ProgramTypes.mkTypeView (STBase "Int") (STBase "Int"))
                actual = ProgramTypes.applyTypeViewSubst subst source
            ProgramTypes.typeViewDisplay actual `shouldBe` STArrow (STVar "a") (STVar "a")
            ProgramTypes.typeViewIdentity actual `shouldBe` STArrow (STBase "Int") (STVar rightStableName)

        it "keys type-view substitutions by identity through binder metadata" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991621)
                stableName = typeBinderIdentityStableName identity
                source =
                    (ProgramTypes.mkTypeView (STVar stableName) (STVar stableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" identity
                        }
            ProgramTypes.typeViewSubstKeyFor source "a"
                `shouldBe` Just (ProgramTypes.typeViewSubstKeyForIdentity identity)

        it "keys type-view substitutions by stable identity spelling through binder metadata" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991623)
                stableName = typeBinderIdentityStableName identity
                source =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar stableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" identity
                        }
            ProgramTypes.typeViewSubstKeyFor source stableName
                `shouldBe` Just (ProgramTypes.typeViewSubstKeyForIdentity identity)

        it "drops ambiguous paired display aliases for one binder identity" $ do
            let identity = typeBinderIdentityFromNode (NodeId 992520)
                stableName = typeBinderIdentityStableName identity
                source =
                    ( ProgramTypes.mkTypeView
                        (STArrow (STVar "a") (STVar "b"))
                        (STArrow (STVar stableName) (STVar stableName))
                    )
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton stableName identity
                        }
            ProgramTypes.typeViewSubstKeyFor source "a" `shouldBe` Nothing
            ProgramTypes.typeViewSubstKeyFor source "b" `shouldBe` Nothing
            ProgramTypes.typeViewSubstKeyFor source stableName
                `shouldBe` Just (ProgramTypes.typeViewSubstKeyForIdentity identity)

        it "collects free type-view variables by binder identity through paired aliases" $ do
            let leftIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991624)
                rightIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991625)
                leftStableName = typeBinderIdentityStableName leftIdentity
                rightStableName = typeBinderIdentityStableName rightIdentity
                source =
                    ( ProgramTypes.mkTypeView
                        (STArrow (STVar "a") (STVar "b"))
                        (STArrow (STVar leftStableName) (STVar rightStableName))
                    )
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList
                                [ ("a", leftIdentity)
                                , (rightStableName, rightIdentity)
                                ]
                        }
            ProgramTypes.freeTypeBinderIdentitiesTypeView source
                `shouldBe` Right (Set.fromList [leftIdentity, rightIdentity])

        it "rejects free type-view variable collection without binder metadata" $ do
            ProgramTypes.freeTypeBinderIdentitiesTypeView (ProgramTypes.mkTypeView (STVar "a") (STVar "a"))
                `shouldBe` Left "a"

        it "keeps replacement type head identities by display key after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 991429)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                headIdentity = generatedSymbolIdentity 991430 SymbolType "Main" "Token" Nothing
                headStableName = symbolIdentityStableName headIdentity
                sourceView =
                    (ProgramTypes.mkTypeView (STVar "x") (STVar sourceStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "x" sourceIdentity
                        }
                replacement =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase headStableName))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Token" headIdentity
                        }
                subst =
                    Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity sourceIdentity) replacement
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.singleton "Token" headIdentity

        it "collects constructor metadata type-view identities for generated identity seeding" $ do
            let dataIdentity = generatedSymbolIdentity 992530 SymbolType "Main" "Phantom" Nothing
                ctorIdentity = generatedSymbolIdentity 992531 SymbolConstructor "Main" "MkPhantom" (Just (SymbolOwnerType dataIdentity))
                ctorHeadUnique = UniqueIdentity 992532
                ownerShapeHeadUnique = UniqueIdentity 992533
                ctorHeadIdentity = generatedSymbolIdentity 992532 SymbolType "Main" "CtorViewHead" Nothing
                ownerShapeHeadIdentity = generatedSymbolIdentity 992533 SymbolType "Main" "OwnerShapeHead" Nothing
                ctorHeadName = symbolIdentityStableName ctorHeadIdentity
                ownerShapeHeadName = symbolIdentityStableName ownerShapeHeadIdentity
                ctorView =
                    (ProgramTypes.mkTypeView (STBase "Phantom") (STBase ctorHeadName))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton ctorHeadName ctorHeadIdentity
                        }
                ownerShapeView =
                    (ProgramTypes.mkTypeView (STBase "Phantom") (STBase ownerShapeHeadName))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton ownerShapeHeadName ownerShapeHeadIdentity
                        }
                ownerShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity
                        , constructorShapeRuntimeName = "Main__MkPhantom"
                        , constructorShapeTypeView = ownerShapeView
                        , constructorShapeForallBinderInfo = []
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = []
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__MkPhantom"
                        , ctorTypeView = ctorView
                        , ctorForallBinderInfo = []
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = [ownerShape]
                        }
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = [ctorInfo]
                        }
                collected = ProgramTypes.dataInfoGeneratedIdentities dataInfo
            collected `shouldSatisfy` elem ctorHeadUnique
            collected `shouldSatisfy` elem ownerShapeHeadUnique

        it "collects evidence resolved-var type identities for generated identity seeding" $ do
            let classIdentity = generatedSymbolIdentity 992534 SymbolClass "Main" "C" Nothing
                methodIdentity = generatedSymbolIdentity 992535 SymbolMethod "Main" "method" (Just (SymbolOwnerClass classIdentity))
                valueIdentity = generatedSymbolIdentity 992536 SymbolValue "Main" "method" Nothing
                binderUnique = UniqueIdentity 992537
                binderRef = Elab.typeBinderRefFromIdentity (typeBinderIdentityFromUnique binderUnique) "a"
                resolved =
                    ResolvedVar
                        { resolvedVarRuntimeName = "Main__method"
                        , resolvedVarType = Elab.TVarRef binderRef
                        , resolvedVarDetails = TopLevelId valueIdentity
                        }
                method =
                    EvidenceMethod
                        { evidenceMethodRuntimeName = "Main__method"
                        , evidenceMethodSymbol = methodIdentity
                        , evidenceMethodResolvedVar = Just resolved
                        , evidenceMethodTypeView = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                        }
            ProgramTypes.evidenceMethodGeneratedIdentities method `shouldSatisfy` elem binderUnique

        it "keeps replacement type head identities by display pair after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 991650)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                headIdentity = generatedSymbolIdentity 991651 SymbolType "Main" "Token" Nothing
                headStableName = symbolIdentityStableName headIdentity
                sourceView =
                    (ProgramTypes.mkTypeView (STVar "x") (STVar sourceStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "x" sourceIdentity
                        }
                replacement =
                    (ProgramTypes.mkTypeView (STBase "DisplayToken") (STBase headStableName))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton headStableName headIdentity
                        }
                subst =
                    Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity sourceIdentity) replacement
                actual =
                    ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
            Map.lookup "DisplayToken" actual `shouldBe` Just headIdentity

        it "keeps replacement type head identities by payload stable name after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 992501)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                headIdentity = generatedSymbolIdentity 992502 SymbolType "Main" "Token" Nothing
                headStableName = symbolIdentityStableName headIdentity
                sourceView =
                    (ProgramTypes.mkTypeView (STVar "x") (STVar sourceStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "x" sourceIdentity
                        }
                replacement =
                    (ProgramTypes.mkTypeView (STBase headStableName) (STBase headStableName))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Token" headIdentity
                        }
                subst =
                    Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity sourceIdentity) replacement
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.singleton "Token" headIdentity

        it "keeps replacement type head identities by payload qualified name after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 992507)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                headIdentity = generatedSymbolIdentity 992508 SymbolType "Main" "Token" Nothing
                qualifiedHeadName = "Main.Token"
                sourceView =
                    (ProgramTypes.mkTypeView (STVar "x") (STVar sourceStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "x" sourceIdentity
                        }
                replacement =
                    (ProgramTypes.mkTypeView (STBase qualifiedHeadName) (STBase qualifiedHeadName))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Token" headIdentity
                        }
                subst =
                    Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity sourceIdentity) replacement
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.singleton "Token" headIdentity

        it "resolves type head identities through payload qualified aliases" $ do
            let headIdentity = generatedSymbolIdentity 992509 SymbolType "Main" "Token" Nothing
                view =
                    (ProgramTypes.mkTypeView (STBase "Main.Token") (STBase "Main.Token"))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Token" headIdentity
                        }
            ProgramTypes.typeViewHeadIdentityForAlias view "Main.Token"
                `shouldBe` Just headIdentity

        it "drops ambiguous paired display aliases for one type head identity" $ do
            let headIdentity = generatedSymbolIdentity 992521 SymbolType "Main" "Token" Nothing
                headStableName = symbolIdentityStableName headIdentity
                view =
                    ( ProgramTypes.mkTypeView
                        (STArrow (STBase "LeftToken") (STBase "RightToken"))
                        (STArrow (STBase headStableName) (STBase headStableName))
                    )
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton headStableName headIdentity
                        }
            ProgramTypes.typeViewHeadIdentityForAlias view "LeftToken" `shouldBe` Nothing
            ProgramTypes.typeViewHeadIdentityForAlias view "RightToken" `shouldBe` Nothing
            ProgramTypes.typeViewHeadIdentityForAlias view headStableName
                `shouldBe` Just headIdentity

        it "keeps replacement binder identities by display key after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 991431)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                replacementIdentity = typeBinderIdentityFromNode (NodeId 991432)
                replacementStableName = typeBinderIdentityStableName replacementIdentity
                sourceView =
                    (ProgramTypes.mkTypeView (STVar "x") (STVar sourceStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "x" sourceIdentity
                        }
                replacement =
                    (ProgramTypes.mkTypeView (STVar "y") (STVar replacementStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "y" replacementIdentity
                        }
                subst =
                    Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity sourceIdentity) replacement
            ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.singleton "y" replacementIdentity

        it "keeps replacement binder identities by payload stable name after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 992503)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                replacementIdentity = typeBinderIdentityFromNode (NodeId 992504)
                replacementStableName = typeBinderIdentityStableName replacementIdentity
                sourceView =
                    (ProgramTypes.mkTypeView (STVar "x") (STVar sourceStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "x" sourceIdentity
                        }
                replacement =
                    (ProgramTypes.mkTypeView (STVar replacementStableName) (STVar replacementStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "y" replacementIdentity
                        }
                subst =
                    Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity sourceIdentity) replacement
            ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.singleton "y" replacementIdentity

        it "keeps display keys for known type-binder identities" $ do
            let identity = typeBinderIdentityFromNode (NodeId 991617)
                stableName = typeBinderIdentityStableName identity
                aliases = ProgramTypes.typeBinderAliasIdentityMap [("a", identity)]
            Map.lookup "a" aliases `shouldBe` Just identity
            Map.lookup stableName aliases `shouldBe` Just identity

        it "drops ambiguous display keys from type-binder identity aliases" $ do
            let firstIdentity = typeBinderIdentityFromNode (NodeId 991618)
                secondIdentity = typeBinderIdentityFromNode (NodeId 991619)
                firstStableName = typeBinderIdentityStableName firstIdentity
                secondStableName = typeBinderIdentityStableName secondIdentity
                aliases =
                    ProgramTypes.typeBinderAliasIdentityMap
                        [ ("a", firstIdentity)
                        , ("a", secondIdentity)
                        ]
            Map.lookup "a" aliases `shouldBe` Nothing
            Map.lookup firstStableName aliases `shouldBe` Just firstIdentity
            Map.lookup secondStableName aliases `shouldBe` Just secondIdentity

        it "compares type-binder substitutions by identity targets when alias names are stale" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991616)
                replacementTy = STBase "Int"
                subst name =
                    ProgramTypes.insertTypeBinderSubstWithIdentity
                        identity
                        name
                        replacementTy
                        ProgramTypes.emptyTypeBinderSubst
            subst "a" `shouldBe` subst "$stale_a"

        it "does not hydrate identity-bearing type-binder substitutions from missing identity keys" $ do
            let identity = typeBinderIdentityFromNode (NodeId 991304)
                identitySubst =
                    ProgramTypes.typeBinderSubstFromTypeViewSubst
                        [("a", identity)]
                        Map.empty
            ProgramTypes.lookupTypeBinderSubstByIdentity identity identitySubst
                `shouldBe` Nothing

        it "does not match metadata-free type-view variables by name" $ do
            let scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template = ProgramTypes.mkTypeView (STVar "a") (STVar "a")
                actual = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldBe` Nothing

        it "skips bare type-view self-substitutions by binder identity when names are stale" $ do
            let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991628)
                stableName = typeBinderIdentityStableName binderIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar stableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" binderIdentity
                        }
                actual =
                    (ProgramTypes.mkTypeView (STVar "b") (STVar "$stale_b"))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "$stale_b" binderIdentity
                        }
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldBe` Just Map.empty

        it "rejects recursive bare type-view substitutions by binder identity when names are stale" $ do
            let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991629)
                stableName = typeBinderIdentityStableName binderIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar stableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" binderIdentity
                        }
                actual =
                    ( ProgramTypes.mkTypeView
                        (STVarApp "f" (STBase "Int" :| []))
                        (STVarApp "$stale_f" (STBase "Int" :| []))
                    )
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "$stale_f" binderIdentity
                        }
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldBe` Nothing

        it "rejects repeated type-view substitutions with same display head and different identities" $ do
            let binderIdentity = typeBinderIdentityFromNode (NodeId 992516)
                binderStableName = typeBinderIdentityStableName binderIdentity
                leftHeadIdentity = generatedSymbolIdentity 992517 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 992518 SymbolType "Right" "Token" Nothing
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar binderStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton binderStableName binderIdentity
                        }
                actual identity =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase "Token"))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Token" identity
                        }
            matchTypeViewsAgainstIdentity
                scope
                Map.empty
                (template :| [template])
                (actual leftHeadIdentity :| [actual rightHeadIdentity])
                `shouldBe` Nothing

        it "skips type-view head self-substitutions by binder identity when names are stale" $ do
            let headIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991626)
                argIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991627)
                headStableName = typeBinderIdentityStableName headIdentity
                argStableName = typeBinderIdentityStableName argIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    ( ProgramTypes.mkTypeView
                        (STVarApp "f" (STVar "a" :| []))
                        (STVarApp headStableName (STVar argStableName :| []))
                    )
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList [("f", headIdentity), ("a", argIdentity)]
                        }
                actual =
                    ( ProgramTypes.mkTypeView
                        (STVarApp "g" (STBase "Int" :| []))
                        (STVarApp "$stale_f" (STBase "Int" :| []))
                    )
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "$stale_f" headIdentity
                        }
                headKey = ProgramTypes.typeViewSubstKeyForIdentity headIdentity
                argKey = ProgramTypes.typeViewSubstKeyForIdentity argIdentity
            case matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| []) of
                Just subst -> do
                    Map.member headKey subst `shouldBe` False
                    fmap ProgramTypes.typeViewIdentity (Map.lookup argKey subst) `shouldBe` Just (STBase "Int")
                Nothing ->
                    expectationFailure "expected argument substitution"

        it "keeps replacement binder identities after applying type-view substitutions" $ do
            let originalIdentity = typeBinderIdentityFromNode (NodeId 991411)
                replacementIdentity = typeBinderIdentityFromNode (NodeId 991412)
                sourceView =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar "$a"))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "$a" originalIdentity
                        }
                replacement =
                    (ProgramTypes.mkTypeView (STVar "b") (STVar "$b"))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "$b" replacementIdentity
                        }
                subst =
                    Map.singleton
                        (ProgramTypes.typeViewSubstKeyForIdentity originalIdentity)
                        replacement
            ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.singleton "$b" replacementIdentity

        it "drops ambiguous replacement binder display identities after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991415)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991416)
                leftReplacementIdentity = typeBinderIdentityFromNode (NodeId 991417)
                rightReplacementIdentity = typeBinderIdentityFromNode (NodeId 991418)
                sourceView =
                    (ProgramTypes.mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y")))
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]
                        }
                leftReplacement =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar "a"))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" leftReplacementIdentity
                        }
                rightReplacement =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar "a"))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" rightReplacementIdentity
                        }
                subst =
                    Map.fromList
                        [ (ProgramTypes.typeViewSubstKeyForIdentity leftSourceIdentity, leftReplacement)
                        , (ProgramTypes.typeViewSubstKeyForIdentity rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.empty

        it "keeps stable binder aliases but drops ambiguous direct display keys after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991438)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991439)
                leftReplacementIdentity = typeBinderIdentityFromNode (NodeId 991440)
                rightReplacementIdentity = typeBinderIdentityFromNode (NodeId 991441)
                leftStableName = typeBinderIdentityStableName leftReplacementIdentity
                rightStableName = typeBinderIdentityStableName rightReplacementIdentity
                sourceView =
                    (ProgramTypes.mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y")))
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]
                        }
                leftReplacement =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar leftStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" leftReplacementIdentity
                        }
                rightReplacement =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar rightStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton rightStableName rightReplacementIdentity
                        }
                subst =
                    Map.fromList
                        [ (ProgramTypes.typeViewSubstKeyForIdentity leftSourceIdentity, leftReplacement)
                        , (ProgramTypes.typeViewSubstKeyForIdentity rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.fromList [(leftStableName, leftReplacementIdentity), (rightStableName, rightReplacementIdentity)]

        it "drops ambiguous replacement type head display identities after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991419)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991420)
                leftHeadIdentity = generatedSymbolIdentity 991421 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 991422 SymbolType "Right" "Token" Nothing
                sourceView =
                    (ProgramTypes.mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y")))
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]
                        }
                leftReplacement =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase "Token"))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Token" leftHeadIdentity
                        }
                rightReplacement =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase "Token"))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Token" rightHeadIdentity
                        }
                subst =
                    Map.fromList
                        [ (ProgramTypes.typeViewSubstKeyForIdentity leftSourceIdentity, leftReplacement)
                        , (ProgramTypes.typeViewSubstKeyForIdentity rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.empty

        it "keeps stable type head aliases but drops ambiguous direct display keys after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991442)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991443)
                leftHeadIdentity = generatedSymbolIdentity 991444 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 991445 SymbolType "Right" "Token" Nothing
                leftStableName = symbolIdentityStableName leftHeadIdentity
                rightStableName = symbolIdentityStableName rightHeadIdentity
                sourceView =
                    (ProgramTypes.mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y")))
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]
                        }
                leftReplacement =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase leftStableName))
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.singleton "Token" leftHeadIdentity
                        }
                rightReplacement =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase rightStableName))
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.singleton rightStableName rightHeadIdentity
                        }
                subst =
                    Map.fromList
                        [ (ProgramTypes.typeViewSubstKeyForIdentity leftSourceIdentity, leftReplacement)
                        , (ProgramTypes.typeViewSubstKeyForIdentity rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.fromList [(leftStableName, leftHeadIdentity), (rightStableName, rightHeadIdentity)]

        it "drops ambiguous replacement type head stable aliases after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991430)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991431)
                leftHeadIdentity = generatedSymbolIdentity 991432 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 991433 SymbolType "Right" "Token" Nothing
                sourceView =
                    (ProgramTypes.mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y")))
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]
                        }
                leftReplacement =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase "Token"))
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.singleton (symbolIdentityStableName leftHeadIdentity) leftHeadIdentity
                        }
                rightReplacement =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase "Token"))
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.singleton (symbolIdentityStableName rightHeadIdentity) rightHeadIdentity
                        }
                subst =
                    Map.fromList
                        [ (ProgramTypes.typeViewSubstKeyForIdentity leftSourceIdentity, leftReplacement)
                        , (ProgramTypes.typeViewSubstKeyForIdentity rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.empty

        it "drops mixed direct and stable ambiguous type head aliases after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991434)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991435)
                leftHeadIdentity = generatedSymbolIdentity 991436 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 991437 SymbolType "Right" "Token" Nothing
                sourceView =
                    (ProgramTypes.mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y")))
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]
                        }
                leftReplacement =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase "Token"))
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.singleton "Token" leftHeadIdentity
                        }
                rightReplacement =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase "Token"))
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.singleton (symbolIdentityStableName rightHeadIdentity) rightHeadIdentity
                        }
                subst =
                    Map.fromList
                        [ (ProgramTypes.typeViewSubstKeyForIdentity leftSourceIdentity, leftReplacement)
                        , (ProgramTypes.typeViewSubstKeyForIdentity rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.empty

        it "drops ambiguous constraint binder display identities from ordinary value views" $ do
            let valueIdentity = generatedSymbolIdentity 991423 SymbolValue "Main" "f" Nothing
                classIdentity = generatedSymbolIdentity 991424 SymbolClass "Main" "C" Nothing
                leftIdentity = typeBinderIdentityFromNode (NodeId 991425)
                rightIdentity = typeBinderIdentityFromNode (NodeId 991426)
                constraintView identity =
                    (ProgramTypes.mkTypeView (STVar "a") (STVar "a"))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" identity
                        }
                constraintInfo identity =
                    ProgramTypes.ConstraintInfo
                        { ProgramTypes.constraintDisplayClass = "C"
                        , ProgramTypes.constraintClassSymbol = classIdentity
                        , ProgramTypes.constraintTypeViews = constraintView identity :| []
                        }
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__f"
                        , valueTypeView = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                        , valueConstraints = []
                        , valueConstraintInfos = [constraintInfo leftIdentity, constraintInfo rightIdentity]
                        }
            ProgramTypes.typeViewBinderIdentities (ProgramTypes.ordinaryValueTypeView valueInfo)
                `shouldBe` Map.empty

        it "drops ambiguous value binder display identities from ordinary value views" $ do
            let valueIdentity = generatedSymbolIdentity 991428 SymbolValue "Main" "f" Nothing
                leftIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991429)
                rightIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991430)
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__f"
                        , valueTypeView =
                            ( ProgramTypes.mkTypeView
                                ( STForall
                                    "a"
                                    Nothing
                                    (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                                )
                                ( STForall
                                    "$typevar#991429"
                                    Nothing
                                    (STForall "$typevar#991430" Nothing (STArrow (STVar "$typevar#991429") (STVar "$typevar#991430")))
                                )
                            )
                                { ProgramTypes.typeViewBinderIdentities =
                                    Map.fromList
                                        [ ("$typevar#991429", leftIdentity)
                                        , ("$typevar#991430", rightIdentity)
                                        ]
                                }
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
            ProgramTypes.typeViewBinderIdentities (ProgramTypes.ordinaryValueTypeView valueInfo)
                `shouldBe`
                    Map.fromList
                        [ ("$typevar#991429", leftIdentity)
                        , ("$typevar#991430", rightIdentity)
                        ]

        it "carries runtime type binder identities in elaborate scope" $ do
            let valueIdentity = generatedSymbolIdentity 991431 SymbolValue "Main" "id" Nothing
                binderIdentity = typeBinderIdentityFromNode (NodeId 991432)
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__id"
                        , valueTypeView =
                            ( ProgramTypes.mkTypeView
                                (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                                (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                            )
                                { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" binderIdentity
                                }
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
                scope = mkElaborateScope (Map.singleton "id" valueInfo) Map.empty Map.empty []
            (Map.lookup "Main__id" (elaborateScopeRuntimeTypeViews scope) >>= Map.lookup "a" . ProgramTypes.typeViewBinderIdentities)
                `shouldBe` Just binderIdentity

        it "canonicalizes method evidence constraint display vars by identity" $ do
            let classIdentity = generatedSymbolIdentity 991450 SymbolClass "Main" "C" Nothing
                methodIdentity = generatedSymbolIdentity 991451 SymbolMethod "Main" "method" (Just (SymbolOwnerClass classIdentity))
                evidenceClassIdentity = generatedSymbolIdentity 991452 SymbolClass "Main" "D" Nothing
                evidenceMethodIdentity = generatedSymbolIdentity 991453 SymbolMethod "Main" "witness" (Just (SymbolOwnerClass evidenceClassIdentity))
                valueIdentity = generatedSymbolIdentity 991454 SymbolValue "Main" "use" Nothing
                classParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991455)
                localIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991456)
                evidenceParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991457)
                localStableName = typeBinderIdentityStableName localIdentity
                evidenceParamStableName = typeBinderIdentityStableName evidenceParamIdentity
                localBinderView displayName =
                    (ProgramTypes.mkTypeView (STVar displayName) (STVar localStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton localStableName localIdentity
                        }
                evidenceParamView displayName =
                    (ProgramTypes.mkTypeView (STVar displayName) (STVar evidenceParamStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton evidenceParamStableName evidenceParamIdentity
                        }
                methodConstraint =
                    ProgramTypes.ConstraintInfo
                        { ProgramTypes.constraintDisplayClass = "D"
                        , ProgramTypes.constraintClassSymbol = evidenceClassIdentity
                        , ProgramTypes.constraintTypeViews = localBinderView "c" :| []
                        }
                methodInfo =
                    MethodInfo
                        { methodInfoSymbol = methodIdentity
                        , methodDisplayName = "method"
                        , methodTypeViewRaw = localBinderView "b"
                        , methodConstraints = []
                        , methodConstraintInfos = [methodConstraint]
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                classInfo =
                    ClassInfo
                        { classInfoSymbol = classIdentity
                        , classTypeParams = ResolvedTypeParam (resolvedTypeBinderRefFromIdentity classParamIdentity "a") KType :| []
                        , classSuperclasses = []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton methodIdentity methodInfo
                        }
                evidenceMethodInfo =
                    MethodInfo
                        { methodInfoSymbol = evidenceMethodIdentity
                        , methodDisplayName = "witness"
                        , methodTypeViewRaw = evidenceParamView "d"
                        , methodConstraints = []
                        , methodConstraintInfos = []
                        , methodParamBinders = ("d", evidenceParamIdentity) :| []
                        }
                evidenceClassInfo =
                    ClassInfo
                        { classInfoSymbol = evidenceClassIdentity
                        , classTypeParams = ResolvedTypeParam (resolvedTypeBinderRefFromIdentity evidenceParamIdentity "d") KType :| []
                        , classSuperclasses = []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton evidenceMethodIdentity evidenceMethodInfo
                        }
                valueConstraint =
                    ProgramTypes.ConstraintInfo
                        { ProgramTypes.constraintDisplayClass = "C"
                        , ProgramTypes.constraintClassSymbol = classIdentity
                        , ProgramTypes.constraintTypeViews = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int") :| []
                        }
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__use"
                        , valueTypeView = ProgramTypes.mkTypeView (STBase "Bool") (STBase "Bool")
                        , valueConstraints = []
                        , valueConstraintInfos = [valueConstraint]
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "use" valueInfo)
                        Map.empty
                        (Map.fromList [("C", classInfo), ("D", evidenceClassInfo)])
                        []
                expected =
                    STArrow
                        (STForall "b" Nothing (STArrow (STVar "b") (STVar "b")))
                        (STBase "Bool")
            fmap ProgramTypes.typeViewDisplay (Map.lookup "Main__use" (elaborateScopeRuntimeTypeViews scope))
                `shouldBe` Just expected

        it "does not choose arbitrary method evidence constraint display vars" $ do
            let classIdentity = generatedSymbolIdentity 991458 SymbolClass "Main" "C2" Nothing
                methodIdentity = generatedSymbolIdentity 991459 SymbolMethod "Main" "method" (Just (SymbolOwnerClass classIdentity))
                evidenceClassIdentity = generatedSymbolIdentity 991460 SymbolClass "Main" "D2" Nothing
                evidenceMethodIdentity = generatedSymbolIdentity 991461 SymbolMethod "Main" "witness" (Just (SymbolOwnerClass evidenceClassIdentity))
                valueIdentity = generatedSymbolIdentity 991464 SymbolValue "Main" "ambiguous" Nothing
                classParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991465)
                localIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991466)
                evidenceParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991467)
                localStableName = typeBinderIdentityStableName localIdentity
                localBinderView displayName =
                    (ProgramTypes.mkTypeView (STVar displayName) (STVar localStableName))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton localStableName localIdentity
                        }
                methodConstraint displayName =
                    ProgramTypes.ConstraintInfo
                        { ProgramTypes.constraintDisplayClass = "D2"
                        , ProgramTypes.constraintClassSymbol = evidenceClassIdentity
                        , ProgramTypes.constraintTypeViews = localBinderView displayName :| []
                        }
                methodInfo =
                    MethodInfo
                        { methodInfoSymbol = methodIdentity
                        , methodDisplayName = "method"
                        , methodTypeViewRaw = ProgramTypes.mkTypeView (STBase "Bool") (STBase "Bool")
                        , methodConstraints = []
                        , methodConstraintInfos = [methodConstraint "c", methodConstraint "d"]
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                classInfo =
                    ClassInfo
                        { classInfoSymbol = classIdentity
                        , classTypeParams = ResolvedTypeParam (resolvedTypeBinderRefFromIdentity classParamIdentity "a") KType :| []
                        , classSuperclasses = []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton methodIdentity methodInfo
                        }
                evidenceParamStableName = typeBinderIdentityStableName evidenceParamIdentity
                evidenceMethodInfo =
                    MethodInfo
                        { methodInfoSymbol = evidenceMethodIdentity
                        , methodDisplayName = "witness"
                        , methodTypeViewRaw =
                            (ProgramTypes.mkTypeView (STVar "e") (STVar evidenceParamStableName))
                                { ProgramTypes.typeViewBinderIdentities = Map.singleton evidenceParamStableName evidenceParamIdentity
                                }
                        , methodConstraints = []
                        , methodConstraintInfos = []
                        , methodParamBinders = ("e", evidenceParamIdentity) :| []
                        }
                evidenceClassInfo =
                    ClassInfo
                        { classInfoSymbol = evidenceClassIdentity
                        , classTypeParams = ResolvedTypeParam (resolvedTypeBinderRefFromIdentity evidenceParamIdentity "e") KType :| []
                        , classSuperclasses = []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton evidenceMethodIdentity evidenceMethodInfo
                        }
                valueConstraint =
                    ProgramTypes.ConstraintInfo
                        { ProgramTypes.constraintDisplayClass = "C2"
                        , ProgramTypes.constraintClassSymbol = classIdentity
                        , ProgramTypes.constraintTypeViews = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int") :| []
                        }
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__ambiguous"
                        , valueTypeView = ProgramTypes.mkTypeView (STBase "Bool") (STBase "Bool")
                        , valueConstraints = []
                        , valueConstraintInfos = [valueConstraint]
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "ambiguous" valueInfo)
                        Map.empty
                        ( Map.fromList
                            [ ("C2", classInfo)
                            , ("D2", evidenceClassInfo)
                            ]
                        )
                        []
                expected =
                    STArrow
                        ( STForall
                            localStableName
                            Nothing
                            (STArrow (STVar localStableName) (STArrow (STVar localStableName) (STBase "Bool")))
                        )
                        (STBase "Bool")
            fmap ProgramTypes.typeViewDisplay (Map.lookup "Main__ambiguous" (elaborateScopeRuntimeTypeViews scope))
                `shouldBe` Just expected

        it "carries runtime type head identities in elaborate scope" $ do
            let valueIdentity = generatedSymbolIdentity 991433 SymbolValue "Main" "box" Nothing
                typeIdentity = generatedSymbolIdentity 991434 SymbolType "Main" "Box" Nothing
                staleHead = "$stale_box"
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__box"
                        , valueTypeView =
                            (ProgramTypes.mkTypeView (STBase "Box") (STBase staleHead))
                                { ProgramTypes.typeViewHeadIdentities = Map.singleton staleHead typeIdentity
                                }
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
                scope = mkElaborateScope (Map.singleton "box" valueInfo) Map.empty Map.empty []
            (Map.lookup "Main__box" (elaborateScopeRuntimeTypeViews scope) >>= Map.lookup staleHead . ProgramTypes.typeViewHeadIdentities)
                `shouldBe` Just typeIdentity

        it "does not keep an arbitrary runtime type view for duplicate runtime-name values" $ do
            let runtimeName = "Main__shared"
                leftIdentity = generatedSymbolIdentity 991438 SymbolValue "Main" "left" Nothing
                rightIdentity = generatedSymbolIdentity 991439 SymbolValue "Main" "right" Nothing
                valueInfo identity ty =
                    OrdinaryValue
                        { valueInfoSymbol = identity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = ProgramTypes.mkTypeView ty ty
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        ( Map.fromList
                            [ ("left", valueInfo leftIdentity (STBase "Int"))
                            , ("right", valueInfo rightIdentity (STBase "Bool"))
                            ]
                        )
                        Map.empty
                        Map.empty
                        []
            Map.member runtimeName (elaborateScopeRuntimeTypeViews scope) `shouldBe` False

        it "does not keep an arbitrary runtime type view when one value identity has conflicting payloads" $ do
            let runtimeName = "Main__shared"
                sharedIdentity = generatedSymbolIdentity 991652 SymbolValue "Main" "shared" Nothing
                valueInfo ty =
                    OrdinaryValue
                        { valueInfoSymbol = sharedIdentity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = ProgramTypes.mkTypeView ty ty
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        ( Map.fromList
                            [ ("left", valueInfo (STBase "Int"))
                            , ("right", valueInfo (STBase "Bool"))
                            ]
                        )
                        Map.empty
                        Map.empty
                        []
            Map.member runtimeName (elaborateScopeRuntimeTypeViews scope) `shouldBe` False

        it "does not resolve conflicting value payloads by identity in elaborate scope" $ do
            let sharedIdentity = generatedSymbolIdentity 991446 SymbolValue "Main" "shared" Nothing
                bindingIdentity = generatedSymbolIdentity 991447 SymbolValue "Main" "main" Nothing
                valueInfo runtimeName ty =
                    OrdinaryValue
                        { valueInfoSymbol = sharedIdentity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = ProgramTypes.mkTypeView ty ty
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
                leftValue = valueInfo "Main__left" (STBase "Int")
                rightValue = valueInfo "Main__right" (STBase "Bool")
                scope =
                    mkElaborateScope
                        (Map.fromList [("left", leftValue), ("right", rightValue)])
                        Map.empty
                        Map.empty
                        []
                expr =
                    EVar
                        ( ResolvedGlobalValue
                            (ProgramTypes.resolvedValueInfoSymbol (SymbolLocal "Main") "left" leftValue)
                        )
            lowerResolvedConstrainedExprBinding
                scope
                (ProgramTypes.loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                (resolvedUnconstrainedType (RSTBase (Builtins.builtinTypeSymbol "Bool")))
                False
                expr
                `shouldBe` Left (ProgramUnknownValue "left")

        it "resolves duplicate runtime-name instance methods by identity" $ do
            let runtimeName = "Main__method"
                classIdentity = generatedSymbolIdentity 991440 SymbolClass "Main" "C" Nothing
                originIdentity = generatedSymbolIdentity 991441 SymbolModule "Main" "Main" Nothing
                methodIdentity = generatedSymbolIdentity 991442 SymbolMethod "Main" "method" (Just (SymbolOwnerClass classIdentity))
                leftIdentity = generatedSymbolIdentity 991443 SymbolValue "Main" "method" Nothing
                rightIdentity = generatedSymbolIdentity 991444 SymbolValue "Other" "method" Nothing
                valueInfo identity =
                    OrdinaryValue
                        { valueInfoSymbol = identity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
                instanceInfo identity headTy =
                    InstanceInfo
                        { instanceClassSymbol = classIdentity
                        , instanceOriginModuleIdentity = originIdentity
                        , instanceConstraints = []
                        , instanceConstraintInfos = []
                        , instanceHeadTypeViews = ProgramTypes.mkTypeView headTy headTy :| []
                        , instanceMethodsByIdentity = Map.singleton methodIdentity (valueInfo identity)
                        }
                scope =
                    mkElaborateScope
                        Map.empty
                        Map.empty
                        Map.empty
                        [ instanceInfo leftIdentity (STBase "Int")
                        , instanceInfo rightIdentity (STBase "Bool")
                        ]
                bindingIdentity = generatedSymbolIdentity 991445 SymbolValue "Main" "main" Nothing
                expr =
                    EVar
                        ( ResolvedGlobalValue
                            (ProgramTypes.resolvedValueInfoSymbol (SymbolLocal "Main") "method" (valueInfo leftIdentity))
                        )
            lowered <-
                case
                    lowerResolvedConstrainedExprBinding
                        scope
                        (ProgramTypes.loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                        (resolvedUnconstrainedType (RSTBase (Builtins.builtinTypeSymbol "Int")))
                        False
                        expr
                  of
                    Left err -> expectationFailure ("resolved instance method lowering failed: " ++ show err) >> fail "resolved instance method lowering failed"
                    Right lowered0 -> pure lowered0
            loweredBindingSurfaceExpr lowered `shouldBe` Surface.EVar runtimeName

        it "does not resolve duplicate runtime-name external bindings by an arbitrary identity" $ do
            let runtimeName = "Main__shared"
                leftIdentity = generatedSymbolIdentity 991435 SymbolValue "Main" "left" Nothing
                rightIdentity = generatedSymbolIdentity 991436 SymbolValue "Main" "right" Nothing
                valueInfo identity =
                    OrdinaryValue
                        { valueInfoSymbol = identity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        (Map.fromList [("left", valueInfo leftIdentity), ("right", valueInfo rightIdentity)])
                        Map.empty
                        Map.empty
                        []
                bindingIdentity = generatedSymbolIdentity 991437 SymbolValue "Main" "main" Nothing
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.EVar runtimeName
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            finalizeBindingWithContext finalizeContext lowered `shouldBe` Left (ProgramUnknownValue runtimeName)

        it "preserves runtime external binding identity across stale runtime spellings" $ do
            let sharedIdentity = generatedSymbolIdentity 991653 SymbolValue "Main" "shared" Nothing
                bindingIdentity = generatedSymbolIdentity 991654 SymbolValue "Main" "main" Nothing
                valueInfo runtimeName =
                    OrdinaryValue
                        { valueInfoSymbol = sharedIdentity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        ( Map.fromList
                            [ ("left", valueInfo "Main__left")
                            , ("right", valueInfo "Main__right")
                            ]
                        )
                        Map.empty
                        Map.empty
                        []
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.EVar "Main__right"
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            case finalizeBindingWithContext finalizeContext lowered of
                Right binding ->
                    case checkedBindingTerm binding of
                        Elab.EVarNode resolved -> do
                            Elab.resolvedVarRuntimeName resolved `shouldBe` "Main__right"
                            Elab.resolvedVarDetails resolved `shouldBe` TopLevelId sharedIdentity
                        other ->
                            expectationFailure ("expected external variable term, got " ++ show other)
                Left err ->
                    expectationFailure ("expected resolved external identity, got " ++ show err)

        it "does not resolve conflicting runtime external payloads by an arbitrary identity" $ do
            let sharedIdentity = generatedSymbolIdentity 991650 SymbolValue "Main" "shared" Nothing
                bindingIdentity = generatedSymbolIdentity 991651 SymbolValue "Main" "main" Nothing
                valueInfo runtimeName ty =
                    OrdinaryValue
                        { valueInfoSymbol = sharedIdentity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = ProgramTypes.mkTypeView ty ty
                        , valueConstraints = []
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        ( Map.fromList
                            [ ("left", valueInfo "Main__left" (STBase "Int"))
                            , ("right", valueInfo "Main__right" (STBase "Bool"))
                            ]
                        )
                        Map.empty
                        Map.empty
                        []
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.EVar "Main__left"
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            case finalizeBindingWithContext finalizeContext lowered of
                Right binding ->
                    case checkedBindingTerm binding of
                        Elab.EVarNode resolved -> do
                            Elab.resolvedVarRuntimeName resolved `shouldBe` "Main__left"
                            case Elab.resolvedVarDetails resolved of
                                EnvId {} -> pure ()
                                other -> expectationFailure ("expected generated EnvId, got " ++ show other)
                        other ->
                            expectationFailure ("expected external variable term, got " ++ show other)
                Left err ->
                    expectationFailure ("expected generated external identity, got " ++ show err)

        it "generates source type binder identities while finalizing stable-looking names" $ do
            let stableName = "$typevar#991607"
                stableRef = Elab.typeBinderRefFromIdentity (typeBinderIdentityFromUnique (UniqueIdentity 0)) stableName
                freshRef = Elab.typeBinderRefFromIdentity (typeBinderIdentityFromUnique (UniqueIdentity 1)) "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                sourceTy =
                    STForall
                        stableName
                        Nothing
                        ( STForall
                            "a"
                            Nothing
                            (STVarApp stableName (STVar "a" :| []))
                        )
                view = ProgramTypes.mkTypeView sourceTy sourceTy
                expected =
                    Elab.TForallRef
                        stableRef
                        Nothing
                        ( Elab.TForallRef
                            freshRef
                            Nothing
                            (Elab.TVarAppRef stableRef (Elab.TVarRef freshRef :| []))
                        )
            typeViewToElabType scope view `shouldBe` Right expected

        it "does not carry stable source type binder identities without metadata" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991622)
                stableName = typeBinderIdentityStableName identity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                sourceTy = STForall stableName Nothing (STVarApp stableName (STBase "Int" :| []))
                view = sourceTypeViewInScope scope sourceTy
            Map.lookup stableName (ProgramTypes.typeViewBinderIdentities view) `shouldBe` Nothing

        it "preserves type-view binder identities while finalizing views" $ do
            let identity = typeBinderIdentityFromNode (NodeId 991608)
                stableName = typeBinderIdentityStableName identity
                ref = Elab.typeBinderRefFromIdentity identity "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                sourceTy = STForall "a" Nothing (STVar "a")
                view =
                    (ProgramTypes.mkTypeView sourceTy sourceTy)
                        { ProgramTypes.typeViewBinderIdentities =
                            Map.fromList
                                [ ("a", identity)
                                , (stableName, identity)
                                ]
                        }
                expected = Elab.TForallRef ref Nothing (Elab.TVarRef ref)
            typeViewToElabType scope view `shouldBe` Right expected

        it "preserves type-view binder identities through display metadata while finalizing views" $ do
            let identity = typeBinderIdentityFromNode (NodeId 991609)
                stableName = typeBinderIdentityStableName identity
                ref = Elab.typeBinderRefFromIdentity identity "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                displayTy = STForall "a" Nothing (STVar "a")
                identityTy = STForall stableName Nothing (STVar stableName)
                view =
                    (ProgramTypes.mkTypeView displayTy identityTy)
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" identity
                        }
                expected = Elab.TForallRef ref Nothing (Elab.TVarRef ref)
            typeViewToElabType scope view `shouldBe` Right expected

        it "does not reuse an outer display binder identity for a same-named missing inner finalization binder" $ do
            let outerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991633)
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                displayTy = STForall "a" Nothing (STForall "a" Nothing (STVar "a"))
                identityTy = STForall "$outer_a" Nothing (STForall "$missing_inner_a" Nothing (STVar "$missing_inner_a"))
                view =
                    (ProgramTypes.mkTypeView displayTy identityTy)
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" outerIdentity
                        }
            case typeViewToElabType scope view of
                Right (Elab.TForallRef outerRef Nothing (Elab.TForallRef innerRef Nothing (Elab.TVarRef bodyRef))) -> do
                    Elab.typeBinderRefIdentity outerRef `shouldBe` outerIdentity
                    Elab.typeBinderRefIdentity innerRef `shouldNotBe` outerIdentity
                    Elab.typeBinderRefIdentity bodyRef `shouldBe` Elab.typeBinderRefIdentity innerRef
                other ->
                    expectationFailure ("expected distinct finalized forall refs, got " ++ show other)

        it "does not reuse an outer display binder identity for same-named finalization binders inside bounds" $ do
            let outerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991634)
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                displayBound = STForall "a" Nothing (STVar "a")
                identityBound = STForall "$missing_bound_a" Nothing (STVar "$missing_bound_a")
                displayTy =
                    STForall "a" Nothing $
                        STForall "b" (Just (SrcBound displayBound)) (STVar "b")
                identityTy =
                    STForall "$outer_a" Nothing $
                        STForall "$b" (Just (SrcBound identityBound)) (STVar "$b")
                view =
                    (ProgramTypes.mkTypeView displayTy identityTy)
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" outerIdentity
                        }
            case typeViewToElabType scope view of
                Right (Elab.TForallRef outerRef Nothing (Elab.TForallRef bRef (Just (Elab.TForallRef boundRef Nothing (Elab.TVarRef boundBodyRef))) (Elab.TVarRef bodyRef))) -> do
                    Elab.typeBinderRefIdentity outerRef `shouldBe` outerIdentity
                    Elab.typeBinderRefIdentity boundRef `shouldNotBe` outerIdentity
                    Elab.typeBinderRefIdentity boundBodyRef `shouldBe` Elab.typeBinderRefIdentity boundRef
                    bodyRef `shouldBe` bRef
                other ->
                    expectationFailure ("expected distinct finalized bound refs, got " ++ show other)

        it "seeds fresh type-view binders after type head identities while finalizing views" $ do
            let headIdentity = generatedSymbolIdentity 43 SymbolType "Main" "Box" Nothing
                headName = symbolIdentityStableName headIdentity
                binderRef = Elab.typeBinderRefFromIdentity (typeBinderIdentityFromUnique (UniqueIdentity 44)) "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                sourceTy = STArrow (STBase "Box") (STVar "a")
                identityTy = STArrow (STBase headName) (STVar "a")
                view =
                    (ProgramTypes.mkTypeView sourceTy identityTy)
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton headName headIdentity
                        }
                expected =
                    Elab.TArrow
                        (Elab.TBaseWithIdentity (Just headIdentity) (BaseTy "Box"))
                        (Elab.TVarRef binderRef)
            typeViewToElabType scope view `shouldBe` Right expected

        it "finalizes type-view heads through payload stable aliases" $ do
            let headIdentity = generatedSymbolIdentity 45 SymbolType "Main" "Token" Nothing
                headName = symbolIdentityStableName headIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                view =
                    (ProgramTypes.mkTypeView (STBase headName) (STBase headName))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Token" headIdentity
                        }
                expected =
                    Elab.TBaseWithIdentity (Just headIdentity) (BaseTy headName)
            typeViewToElabType scope view `shouldBe` Right expected

        it "finalizes type-view heads through display identity pairs" $ do
            let headIdentity = generatedSymbolIdentity 991650 SymbolType "Main" "Token" Nothing
                headName = symbolIdentityStableName headIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                view =
                    (ProgramTypes.mkTypeView (STBase "DisplayToken") (STBase headName))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton headName headIdentity
                        }
                expected =
                    Elab.TBaseWithIdentity (Just headIdentity) (BaseTy "DisplayToken")
            typeViewToElabType scope view `shouldBe` Right expected

        it "does not finalize ambiguous type-view head aliases by display name" $ do
            let leftIdentity = generatedSymbolIdentity 991620 SymbolType "Left" "Token" Nothing
                rightIdentity = generatedSymbolIdentity 991621 SymbolType "Right" "Token" Nothing
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                view =
                    (ProgramTypes.mkTypeView (STBase "Token") (STBase "Token"))
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.fromList
                                [ (symbolIdentityStableName leftIdentity, leftIdentity)
                                , (symbolIdentityStableName rightIdentity, rightIdentity)
                                ]
                        }
                expected =
                    Elab.TBaseWithIdentity Nothing (BaseTy "Token")
            typeViewToElabType scope view `shouldBe` Right expected

        it "seeds fresh type-view binders after scoped type head identities" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box = Box : Box;"
                        , "  def main : Box = Box;"
                        , "}"
                        ]
            checked <- requireChecked program
            dataInfo <- requireCheckedData "Main" "Box" checked
            let scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
                sourceTy = STArrow (STBase "Box") (STVar "a")
                view = ProgramTypes.mkTypeView sourceTy sourceTy
                boxIdentity = ProgramTypes.dataInfoSymbolIdentity dataInfo
            case typeViewToElabType scope view of
                Right (Elab.TArrow _ (Elab.TVarRef binderRef)) ->
                    case typeBinderIdentityGeneratedUnique (Elab.typeBinderRefIdentity binderRef) of
                        Just binderIdentity ->
                            uniqueIdentityValue binderIdentity `shouldSatisfy` (> uniqueIdentityValue (symbolUniqueIdentity boxIdentity))
                        Nothing ->
                            expectationFailure "expected generated binder identity"
                other ->
                    expectationFailure ("expected scoped-head seeded type conversion, got " ++ show other)

        it "attaches builtin identities while finalizing source type views" $ do
            let scope = mkElaborateScope Map.empty Map.empty Map.empty []
                sourceTy = STArrow (STBase "Int") (STCon "IO" (STBase "Int" :| []))
                builtinIntTy =
                    Elab.TBaseWithIdentity
                        (Just (Builtins.builtinTypeIdentity "Int"))
                        (BaseTy "Int")
                expected =
                    Elab.TArrow
                        builtinIntTy
                        ( Elab.TConWithIdentity
                            (Just (Builtins.builtinTypeIdentity "IO"))
                            (BaseTy "IO")
                            (builtinIntTy :| [])
                        )
            typeViewToElabType scope (ProgramTypes.mkTypeView sourceTy sourceTy) `shouldBe` Right expected

        it "does not resolve resolved forall substitutions by stale name when binder identity differs" $ do
            let expectedIdentity = typeBinderIdentityFromNode (NodeId 991302)
                staleIdentity = typeBinderIdentityFromNode (NodeId 991303)
                ref = Elab.typeBinderRefFromIdentity expectedIdentity "a"
                sourceView =
                    (ProgramTypes.mkTypeView (STForall "a" Nothing (STVar "a")) (STForall "a" Nothing (STVar "a")))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" expectedIdentity
                        }
                replacement = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                subst = Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity staleIdentity) replacement
            resolvedForallSubst subst sourceView [(ref, Nothing)] `shouldBe` Map.empty

        it "does not degrade identity-keyed resolved forall substitutions to name lookup" $ do
            let expectedIdentity = typeBinderIdentityFromNode (NodeId 991413)
                staleIdentity = typeBinderIdentityFromNode (NodeId 991414)
                ref = Elab.typeBinderRefFromIdentity expectedIdentity "a"
                sourceView = ProgramTypes.mkTypeView (STForall "a" Nothing (STVar "a")) (STForall "a" Nothing (STVar "a"))
                replacement = ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")
                subst = Map.singleton (ProgramTypes.typeViewSubstKeyForIdentity staleIdentity) replacement
            resolvedForallSubst subst sourceView [(ref, Nothing)] `shouldBe` Map.empty

        it "matches type-view data heads by identity alias" $ do
            let typeIdentity =
                    generatedSymbolIdentity 991306 SymbolType "Main" "Box" Nothing
                stableHead = symbolIdentityStableName typeIdentity
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = typeIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
                baseTemplate = ProgramTypes.mkTypeView (STBase "Box") (STBase "Box")
                baseActual = ProgramTypes.mkTypeView (STBase "Box") (STBase "Main.Box")
                conTemplate =
                    ProgramTypes.mkTypeView
                        (STCon "Box" (STBase "Int" :| []))
                        (STCon "Box" (STBase "Int" :| []))
                conActual =
                    ProgramTypes.mkTypeView
                        (STCon "Box" (STBase "Int" :| []))
                        (STCon "Main.Box" (STBase "Int" :| []))
                metadataTemplate =
                    (ProgramTypes.mkTypeView (STBase "Box") (STBase "Box"))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Box" typeIdentity
                        }
                metadataActual =
                    (ProgramTypes.mkTypeView (STBase "Box") (STBase stableHead))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Box" typeIdentity
                        }
                emptyScope = mkElaborateScope Map.empty Map.empty Map.empty []
            matchTypeViewsAgainstIdentity scope Map.empty (baseTemplate :| []) (baseActual :| [])
                `shouldBe` Just Map.empty
            matchTypeViewsAgainstIdentity scope Map.empty (conTemplate :| []) (conActual :| [])
                `shouldBe` Just Map.empty
            matchTypeViewsAgainstIdentity emptyScope Map.empty (metadataTemplate :| []) (metadataActual :| [])
                `shouldBe` Just Map.empty

        it "canonicalizes source data heads through identity aliases" $ do
            let typeIdentity =
                    generatedSymbolIdentity 991625 SymbolType "Main" "Box" Nothing
                stableHead = symbolIdentityStableName typeIdentity
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = typeIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
            sourceTypeIdentityInScope scope (STBase "Main.Box")
                `shouldBe` STBase stableHead

        it "does not match same-named type-view data heads with different identities" $ do
            let expectedIdentity =
                    generatedSymbolIdentity 991413 SymbolType "Main" "Box" Nothing
                actualIdentity =
                    generatedSymbolIdentity 991414 SymbolType "Other" "Box" Nothing
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = expectedIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
                template =
                    (ProgramTypes.mkTypeView (STBase "Box") (STBase "Box"))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Box" expectedIdentity
                        }
                actual =
                    (ProgramTypes.mkTypeView (STBase "Box") (STBase "Box"))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Box" actualIdentity
                        }
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldBe` Nothing

        it "does not match identity-bearing type-view data heads through name-only fallback" $ do
            let expectedIdentity =
                    generatedSymbolIdentity 991415 SymbolType "Main" "Box" Nothing
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (ProgramTypes.mkTypeView (STBase "Box") (STBase "Box"))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton "Box" expectedIdentity
                        }
                actual =
                    ProgramTypes.mkTypeView
                        (STBase (symbolIdentityStableName expectedIdentity))
                        (STBase (symbolIdentityStableName expectedIdentity))
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldBe` Nothing

        it "matches type-view binders by identity when names are stale" $ do
            let binderIdentity = typeBinderIdentityFromNode (NodeId 991308)
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (ProgramTypes.mkTypeView
                        (STForall "a" Nothing (STVar "a"))
                        (STForall "$left" Nothing (STVar "$left")))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "$left" binderIdentity
                        }
                actual =
                    (ProgramTypes.mkTypeView
                        (STForall "b" Nothing (STVar "b"))
                        (STForall "$right" Nothing (STVar "$right")))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "$right" binderIdentity
                        }
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldSatisfy` isJust

        it "matches type-view variable heads by binder identity when names are stale" $ do
            let binderIdentity = typeBinderIdentityFromNode (NodeId 991622)
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (ProgramTypes.mkTypeView
                        (STVarApp "f" (STBase "Int" :| []))
                        (STVarApp "$left" (STBase "Int" :| [])))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "$left" binderIdentity
                        }
                actual =
                    (ProgramTypes.mkTypeView
                        (STVarApp "g" (STBase "Int" :| []))
                        (STVarApp "$right" (STBase "Int" :| [])))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "$right" binderIdentity
                        }
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldSatisfy` isJust

        it "matches alpha-renamed type-view binders with different identities" $ do
            let templateIdentity = typeBinderIdentityFromNode (NodeId 991309)
                actualIdentity = typeBinderIdentityFromNode (NodeId 991310)
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (ProgramTypes.mkTypeView
                        (STForall "a" Nothing (STVar "a"))
                        (STForall "a" Nothing (STVar "a")))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" templateIdentity
                        }
                actual =
                    (ProgramTypes.mkTypeView
                        (STForall "a" Nothing (STVar "a"))
                        (STForall "a" Nothing (STVar "a")))
                        { ProgramTypes.typeViewBinderIdentities = Map.singleton "a" actualIdentity
                        }
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldSatisfy` isJust

        it "matches forall source data heads by identity alias in scope" $ do
            let typeIdentity =
                    generatedSymbolIdentity 991307 SymbolType "Main" "Box" Nothing
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = typeIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
                expected =
                    STForall
                        "a"
                        Nothing
                        (STArrow (STCon "Box" (STVar "a" :| [])) (STCon "Box" (STVar "a" :| [])))
                actual =
                    STArrow
                        (STCon "Main.Box" (STBase "Int" :| []))
                        (STCon "Main.Box" (STBase "Int" :| []))
                stableHead = symbolIdentityStableName typeIdentity
                actualByStableIdentity =
                    STArrow
                        (STCon stableHead (STBase "Int" :| []))
                        (STCon stableHead (STBase "Int" :| []))
            sourceForallMatches expected actual `shouldBe` False
            sourceForallMatchesInScope scope expected actual `shouldBe` True
            sourceForallMatches expected actualByStableIdentity `shouldBe` False
            sourceForallMatchesInScope scope expected actualByStableIdentity `shouldBe` True

        it "matches rigid source type-app heads by scoped identity aliases" $ do
            let typeIdentity =
                    generatedSymbolIdentity 991308 SymbolType "Main" "Box" Nothing
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = typeIdentity
                        , dataTypeParams =
                            [ResolvedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991708) "a") KType]
                        , dataConstructors = []
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
                expected =
                    STForall
                        "a"
                        Nothing
                        (STArrow (STVarApp "Box" (STVar "a" :| [])) (STVarApp "Box" (STVar "a" :| [])))
                stableHead = symbolIdentityStableName typeIdentity
                actual =
                    STArrow
                        (STVarApp stableHead (STBase "Int" :| []))
                        (STVarApp stableHead (STBase "Int" :| []))
            sourceForallMatches expected actual `shouldBe` False
            sourceForallMatchesInScope scope expected actual `shouldBe` True

        it "matches bound variable-headed applications against instantiated constructor heads" $ do
            let expected =
                    STForall
                        "f"
                        Nothing
                        ( STArrow
                            (STVarApp "f" (STVar "a" :| []))
                            (STVarApp "f" (STVar "a" :| []))
                        )
                actual =
                    STArrow
                        (STCon "Either" (STBase "Int" :| [STVar "a"]))
                        (STCon "Either" (STBase "Int" :| [STVar "a"]))
            sourceForallMatches expected actual `shouldBe` True

        it "rejects inconsistent variable-headed application alpha-renaming" $ do
            let expected =
                    STForall
                        "f"
                        Nothing
                        ( STArrow
                            (STVarApp "f" (STVar "a" :| []))
                            (STVarApp "f" (STVar "a" :| []))
                        )
                actual =
                    STArrow
                        (STVarApp "g" (STVar "a" :| []))
                        (STVarApp "h" (STVar "a" :| []))
            sourceForallMatches expected actual `shouldBe` False

        it "rejects inconsistent instantiated constructor heads" $ do
            let expected =
                    STForall
                        "f"
                        Nothing
                        ( STArrow
                            (STVarApp "f" (STVar "a" :| []))
                            (STVarApp "f" (STVar "a" :| []))
                        )
                actual =
                    STArrow
                        (STCon "Either" (STBase "Int" :| [STVar "a"]))
                        (STCon "Maybe" (STVar "a" :| []))
            sourceForallMatches expected actual `shouldBe` False

        it "rejects bound variable applications without lowering STVarApp" $ do
            let expected =
                    STForall
                        "f"
                        Nothing
                        (STArrow (STVar "f") (STVar "f"))
                actual =
                    STForall
                        "g"
                        Nothing
                        ( STArrow
                            (STVar "g")
                            (STVarApp "g" (STVar "a" :| []))
                        )
            sourceForallMatches expected actual `shouldBe` False

        it "keeps vacuous foralls when matching type abstractions still carry instantiations" $ do
            let ty = testTForall "a" Nothing (testTVar "result")
                retainedTerm =
                    mkTestTyAbs "a"
                        Nothing
                        (Elab.ETyInst (mkTestDeferredVar "poly") (Elab.InstApp (testTVar "a")))
                strippedTerm = mkTestTyAbs "a" Nothing (mkTestDeferredVar "value")
            stripVacuousForallsAndTypeAbs ty retainedTerm `shouldBe` (ty, retainedTerm)
            stripVacuousForallsAndTypeAbs ty strippedTerm `shouldBe` (testTVar "result", mkTestDeferredVar "value")

        it "keeps vacuous foralls when resolved sidecar types still mention the binder" $ do
            let ty = testTForall "a" Nothing (testTVar "result")
                resolved =
                    ResolvedVar
                        { resolvedVarRuntimeName = "value"
                        , resolvedVarType = testTVar "a"
                        , resolvedVarDetails = LocalId (generatedLocalRefForName "$value#0")
                        }
                retainedTerm = mkTestTyAbs "a" Nothing (Elab.EVarNode resolved)
            stripVacuousForallsAndTypeAbs ty retainedTerm `shouldBe` (ty, retainedTerm)

        it "recovers higher-kinded data heads with partially applied constructor parameters" $ do
            let typeIdentity =
                    generatedSymbolIdentity 1001 SymbolType "Main" "Apply" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1002 SymbolConstructor "Main" "Apply" (Just (SymbolOwnerType typeIdentity))
                applyResult = STCon "Apply" (STVar "f" :| [STVar "a"])
                applyCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "$Apply"
                        , ctorTypeView =
                            ProgramTypes.mkTypeView
                                (STArrow (STVarApp "f" (STVar "a" :| [])) applyResult)
                                (STArrow (STVarApp "f" (STVar "a" :| [])) (STCon "Main.Apply" (STVar "f" :| [STVar "a"])))
                        , ctorForallBinderInfo = []
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                applyInfo =
                    DataInfo
                        { dataInfoSymbol = typeIdentity
                        , dataTypeParams =
                            [ ResolvedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991701) "f") (KArrow KType KType)
                            , ResolvedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991702) "a") KType
                            ]
                        , dataConstructors = [applyCtor]
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Apply" applyInfo) Map.empty []
                visible =
                    STCon
                        "Apply"
                        ( STCon "Either" (STBase "Int" :| [])
                            :| [STBase "String"]
                        )
            recoverSourceType scope (lowerType scope visible) `shouldBe` visible

        it "recovers repeated data parameters through scoped type-head identity aliases" $ do
            let boxIdentity =
                    generatedSymbolIdentity 1031 SymbolType "Main" "Box" Nothing
                boxCtorIdentity =
                    generatedSymbolIdentity 1032 SymbolConstructor "Main" "Box" (Just (SymbolOwnerType boxIdentity))
                boxCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = boxCtorIdentity
                        , ctorRuntimeName = "$Box"
                        , ctorTypeView = ProgramTypes.mkTypeView (STBase "Box") (STBase "Main.Box")
                        , ctorForallBinderInfo = []
                        , ctorOwningTypeIdentity = boxIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                boxInfo =
                    DataInfo
                        { dataInfoSymbol = boxIdentity
                        , dataTypeParams = []
                        , dataConstructors = [boxCtor]
                        }
                dupIdentity =
                    generatedSymbolIdentity 1033 SymbolType "Main" "Dup" Nothing
                dupCtorIdentity =
                    generatedSymbolIdentity 1034 SymbolConstructor "Main" "Dup" (Just (SymbolOwnerType dupIdentity))
                dupResult = STCon "Dup" (STVar "a" :| [])
                dupCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = dupCtorIdentity
                        , ctorRuntimeName = "$Dup"
                        , ctorTypeView =
                            ProgramTypes.mkTypeView
                                (STArrow (STVar "a") (STArrow (STVar "a") dupResult))
                                (STArrow (STVar "a") (STArrow (STVar "a") (STCon "Main.Dup" (STVar "a" :| []))))
                        , ctorForallBinderInfo = []
                        , ctorOwningTypeIdentity = dupIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                dupInfo =
                    DataInfo
                        { dataInfoSymbol = dupIdentity
                        , dataTypeParams =
                            [ResolvedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991707) "a") KType]
                        , dataConstructors = [dupCtor]
                        }
                scope =
                    mkElaborateScope
                        Map.empty
                        (Map.fromList [("Box", boxInfo), ("Dup", dupInfo)])
                        Map.empty
                        []
                visible = STCon "Dup" (STBase "Box" :| [])
                actual =
                    replaceFreeTypeVarsOnce
                        "a"
                        [STBase "Box", STBase (symbolIdentityStableName boxIdentity)]
                        (lowerType scope (STCon "Dup" (STVar "a" :| [])))
            recoverSourceType scope actual `shouldBe` visible

        it "lowers data encoding binders from data identity when data names are stale" $ do
            let typeIdentity =
                    generatedSymbolIdentity 1011 SymbolType "Main" "Box" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1012 SymbolConstructor "Main" "Box" (Just (SymbolOwnerType typeIdentity))
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "$Box"
                        , ctorTypeView =
                            ProgramTypes.mkTypeView
                                (STArrow (STBase "Int") (STBase "Box"))
                                (STArrow (STBase "Int") (STBase "Main.Box"))
                        , ctorForallBinderInfo = []
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = typeIdentity
                        , dataTypeParams = []
                        , dataConstructors = [ctorInfo]
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
                expected =
                    STMu
                        "$Main.Box_self"
                        ( STForall
                            "$Main.Box_result"
                            Nothing
                            ( STArrow
                                (STArrow (STBase "Int") (STVar "$Main.Box_result"))
                                (STVar "$Main.Box_result")
                            )
                        )
            lowerType scope (STBase "Box") `shouldBe` expected

        it "treats owner-shaped variable-headed constructor imports as non-trackable" $ do
            let typeIdentity =
                    generatedSymbolIdentity 1021 SymbolType "Core" "MaybeF" Nothing
                ctorIdentity unique name =
                    generatedSymbolIdentity unique SymbolConstructor "Core" name (Just (SymbolOwnerType typeIdentity))
                resultTy = STCon "MaybeF" (STVar "f" :| [STVar "a"])
                resultTyIdentity = STCon "Core.MaybeF" (STVar "f" :| [STVar "a"])
                ownerTypeParams =
                    [ ResolvedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991703) "f") (KArrow KType KType)
                    , ResolvedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991704) "a") KType
                    ]
                nothingShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity 1022 "NothingF"
                        , constructorShapeRuntimeName = "Core__NothingF"
                        , constructorShapeTypeView = ProgramTypes.mkTypeView resultTy resultTyIdentity
                        , constructorShapeForallBinderInfo = []
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = ownerTypeParams
                        }
                justShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity 1023 "JustF"
                        , constructorShapeRuntimeName = "Core__JustF"
                        , constructorShapeTypeView =
                            ProgramTypes.mkTypeView
                                (STArrow (STVarApp "f" (STVar "a" :| [])) resultTy)
                                (STArrow (STVarApp "f" (STVar "a" :| [])) resultTyIdentity)
                        , constructorShapeForallBinderInfo = []
                        , constructorShapeIndex = 1
                        , constructorShapeOwnerTypeParams = ownerTypeParams
                        }
                nothingCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity 1022 "NothingF"
                        , ctorRuntimeName = "Core__NothingF"
                        , ctorTypeView = ProgramTypes.mkTypeView resultTy resultTyIdentity
                        , ctorForallBinderInfo = []
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = [nothingShape, justShape]
                        }
            constructorOwnerRuntimeTypeTrackable Map.empty nothingCtor `shouldBe` False
            ProgramTypes.constructorShapeResultIdentity (constructorShapeFromInfo nothingCtor) `shouldBe` resultTyIdentity
            map ProgramTypes.constructorShapeResultIdentity (ProgramTypes.constructorOwnerShapes nothingCtor) `shouldBe` [resultTyIdentity, resultTyIdentity]

        it "infers constructor owner params through result head identity metadata" $ do
            let typeIdentity =
                    generatedSymbolIdentity 1024 SymbolType "Core" "MaybeF" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1025 SymbolConstructor "Core" "JustF" (Just (SymbolOwnerType typeIdentity))
                fIdentity = typeBinderIdentityFromNode (NodeId 992505)
                aIdentity = typeBinderIdentityFromNode (NodeId 992506)
                fStableName = typeBinderIdentityStableName fIdentity
                aStableName = typeBinderIdentityStableName aIdentity
                staleDisplayHead = "$stale_display_maybef"
                staleIdentityHead = "$stale_identity_maybef"
                displayResult = STCon staleDisplayHead (STVar "f" :| [STVar "a"])
                identityResult = STCon staleIdentityHead (STVar fStableName :| [STVar aStableName])
                ctorView =
                    ( ProgramTypes.mkTypeView
                        displayResult
                        identityResult
                    )
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.fromList
                                [ (staleDisplayHead, typeIdentity)
                                , (staleIdentityHead, typeIdentity)
                                ]
                        , ProgramTypes.typeViewBinderIdentities =
                            Map.fromList
                                [ (fStableName, fIdentity)
                                , (aStableName, aIdentity)
                                ]
                        }
                forallBinders =
                    [ ProgramTypes.ConstructorForallBinder "f" fIdentity
                    , ProgramTypes.ConstructorForallBinder "a" aIdentity
                    ]
                shape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity
                        , constructorShapeRuntimeName = "Core__JustF"
                        , constructorShapeTypeView = ctorView
                        , constructorShapeForallBinderInfo = forallBinders
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = []
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Core__JustF"
                        , ctorTypeView = ctorView
                        , ctorForallBinderInfo = forallBinders
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = [shape]
                        }
                ownerInfo = ProgramTypes.constructorOwnerDataInfoFromShapes ctorInfo
            dataTypeParams ownerInfo
                `shouldBe` [ ResolvedTypeParam (resolvedTypeBinderRefFromIdentity fIdentity "f") KType
                           , ResolvedTypeParam (resolvedTypeBinderRefFromIdentity aIdentity "a") KType
                           ]

        it "infers constructor owner param kinds by binder identity across stale shape aliases" $ do
            let typeIdentity =
                    generatedSymbolIdentity 1026 SymbolType "Core" "MaybeF" Nothing
                ctorIdentity unique name =
                    generatedSymbolIdentity unique SymbolConstructor "Core" name (Just (SymbolOwnerType typeIdentity))
                fIdentity = typeBinderIdentityFromNode (NodeId 992507)
                aIdentity = typeBinderIdentityFromNode (NodeId 992508)
                fStableName = typeBinderIdentityStableName fIdentity
                aStableName = typeBinderIdentityStableName aIdentity
                staleDisplayHead = "$stale_display_maybef"
                staleIdentityHead = "$stale_identity_maybef"
                leftDisplayResult = STCon staleDisplayHead (STVar "$left_f" :| [STVar "$left_a"])
                rightDisplayResult = STCon staleDisplayHead (STVar "$right_f" :| [STVar "$right_a"])
                identityResult = STCon staleIdentityHead (STVar fStableName :| [STVar aStableName])
                rightDisplayType =
                    STArrow
                        (STVarApp "$right_f" (STVar "$right_a" :| []))
                        rightDisplayResult
                rightIdentityType =
                    STArrow
                        (STVarApp fStableName (STVar aStableName :| []))
                        identityResult
                mkView displayTy identityTy =
                    (ProgramTypes.mkTypeView displayTy identityTy)
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.fromList
                                [ (staleDisplayHead, typeIdentity)
                                , (staleIdentityHead, typeIdentity)
                                ]
                        , ProgramTypes.typeViewBinderIdentities =
                            Map.fromList
                                [ (fStableName, fIdentity)
                                , (aStableName, aIdentity)
                                ]
                        }
                forallBinders =
                    [ ProgramTypes.ConstructorForallBinder "f" fIdentity
                    , ProgramTypes.ConstructorForallBinder "a" aIdentity
                    ]
                leftShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity 1027 "NothingF"
                        , constructorShapeRuntimeName = "Core__NothingF"
                        , constructorShapeTypeView = mkView leftDisplayResult identityResult
                        , constructorShapeForallBinderInfo = forallBinders
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = []
                        }
                rightShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity 1028 "JustF"
                        , constructorShapeRuntimeName = "Core__JustF"
                        , constructorShapeTypeView = mkView rightDisplayType rightIdentityType
                        , constructorShapeForallBinderInfo = forallBinders
                        , constructorShapeIndex = 1
                        , constructorShapeOwnerTypeParams = []
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity 1027 "NothingF"
                        , ctorRuntimeName = "Core__NothingF"
                        , ctorTypeView = mkView leftDisplayResult identityResult
                        , ctorForallBinderInfo = forallBinders
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = [leftShape, rightShape]
                        }
                ownerInfo = ProgramTypes.constructorOwnerDataInfoFromShapes ctorInfo
            dataTypeParams ownerInfo
                `shouldBe` [ ResolvedTypeParam (resolvedTypeBinderRefFromIdentity fIdentity "f") (KArrow KType KType)
                           , ResolvedTypeParam (resolvedTypeBinderRefFromIdentity aIdentity "a") KType
                           ]

        it "records constructor bindings with resolved constructor identity" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), main) {"
                        , "  data Option ="
                        , "      None : Option;"
                        , ""
                        , "  def main : Option = None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            binding <- requireCheckedBinding "Main__None" checked
            case checkedBindingResolvedVar binding of
                ResolvedVar
                    { resolvedVarRuntimeName = "Main__None"
                    , resolvedVarDetails = ConstructorId ctorRef
                    } -> do
                        symbolDefiningName (constructorRefSymbol ctorRef) `shouldBe` "None"
                resolvedVar ->
                    expectationFailure ("expected constructor resolved var, got: " ++ show resolvedVar)
            mainBinding <- requireCheckedBinding "Main__main" checked
            map (symbolDefiningName . constructorRefSymbol) (resolvedConstructorRefs (checkedBindingTerm mainBinding))
                `shouldContain` ["None"]

        it "keeps constructor metadata identity type separate from visible type" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), main) {"
                        , "  data Option ="
                        , "      None : Option;"
                        , ""
                        , "  def main : Option = None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Option" checked
            ctorInfo <- requireDataConstructor "None" dataInfo
            ProgramTypes.ctorType ctorInfo `shouldBe` STBase "Option"
            ProgramTypes.ctorTypeIdentity ctorInfo `shouldBe` STBase (symbolIdentityStableName (dataInfoSymbol dataInfo))

        it "uses constructor TypeView head identities when elaborating constructor applications" $ do
            let dataIdentity =
                    generatedSymbolIdentity 1021 SymbolType "Main" "Option" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1022 SymbolConstructor "Main" "Some" (Just (SymbolOwnerType dataIdentity))
                staleDisplayHead = "$stale_display_option"
                staleIdentityHead = "$stale_identity_option"
                ctorTypeView =
                    ( ProgramTypes.mkTypeView
                        (STArrow (STVar "a") (STCon staleDisplayHead (STVar "a" :| [])))
                        (STArrow (STVar "a") (STCon staleIdentityHead (STVar "a" :| [])))
                    )
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton staleIdentityHead dataIdentity
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__Some"
                        , ctorTypeView = ctorTypeView
                        , ctorForallBinderInfo = []
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = [ResolvedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991705) "a") KType]
                        , dataConstructors = [ctorInfo]
                        }
                valueInfo =
                    ConstructorValue
                        { valueInfoSymbol = ctorIdentity
                        , valueRuntimeName = "Main__Some"
                        , valueCtorInfo = ctorInfo
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "Some" valueInfo)
                        (Map.singleton "Option" dataInfo)
                        Map.empty
                        []
                expectedTy = STCon "Option" (STBase "Int" :| [])
                expr = EApp (EVar "Some") (ELit (LInt 1))
            lowered <-
                case lowerExprBinding scope (ProgramTypes.loweredBindingIdentityFromValueInfo valueInfo) expectedTy False expr of
                    Left err -> expectationFailure ("constructor application elaboration failed: " ++ show err) >> fail "constructor application elaboration failed"
                    Right binding -> pure binding
            case Map.elems (loweredBindingDeferredObligations lowered) of
                [DeferredConstructor deferred] -> do
                    deferredConstructorSourceType deferred
                        `shouldBe` STArrow (STBase "Int") (STCon staleDisplayHead (STBase "Int" :| []))
                    let placeholder = ProgramTypes.deferredConstructorPlaceholder deferred
                    (Map.lookup placeholder (loweredBindingExternalTypeViews lowered) >>= Map.lookup staleIdentityHead . ProgramTypes.typeViewHeadIdentities)
                        `shouldBe` Just dataIdentity
                obligations ->
                    expectationFailure ("expected one deferred constructor obligation, got " ++ show obligations)

        it "carries constructor placeholder type binder identities into external types" $ do
            let dataIdentity =
                    generatedSymbolIdentity 1031 SymbolType "Main" "Option" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1032 SymbolConstructor "Main" "Some" (Just (SymbolOwnerType dataIdentity))
                paramUnique = UniqueIdentity 991706
                paramIdentity = typeBinderIdentityFromUnique paramUnique
                ctorTypeView =
                    ( ProgramTypes.mkTypeView
                        (STArrow (STVar "a") (STCon "Option" (STVar "a" :| [])))
                        (STArrow (STVar "a") (STCon (symbolIdentityStableName dataIdentity) (STVar "a" :| [])))
                    )
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton (symbolIdentityStableName dataIdentity) dataIdentity
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__Some"
                        , ctorTypeView = ctorTypeView
                        , ctorForallBinderInfo = []
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = [ResolvedTypeParam (resolvedTypeBinderRef paramUnique "a") KType]
                        , dataConstructors = [ctorInfo]
                        }
                valueInfo =
                    ConstructorValue
                        { valueInfoSymbol = ctorIdentity
                        , valueRuntimeName = "Main__Some"
                        , valueCtorInfo = ctorInfo
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "Some" valueInfo)
                        (Map.singleton "Option" dataInfo)
                        Map.empty
                        []
                expectedTy = STArrow (STVar "a") (STCon "Option" (STVar "a" :| []))
            lowered <-
                case lowerExprBinding scope (ProgramTypes.loweredBindingIdentityFromValueInfo valueInfo) expectedTy False (EVar "Some") of
                    Left err -> expectationFailure ("constructor lowering failed: " ++ show err) >> fail "constructor lowering failed"
                    Right binding -> pure binding
            case Map.elems (loweredBindingDeferredObligations lowered) of
                [DeferredConstructor deferred] -> do
                    let placeholder = ProgramTypes.deferredConstructorPlaceholder deferred
                    (Map.lookup placeholder (loweredBindingExternalTypeViews lowered) >>= Map.lookup "a" . ProgramTypes.typeViewBinderIdentities)
                        `shouldBe` Just paramIdentity
                obligations ->
                    expectationFailure ("expected one deferred constructor obligation, got " ++ show obligations)

        it "seeds resolved lowering generated identities after resolved input identities" $ do
            let dataIdentity =
                    generatedSymbolIdentity 0 SymbolType "Main" "Box" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1 SymbolConstructor "Main" "Box" (Just (SymbolOwnerType dataIdentity))
                bindingIdentity =
                    generatedSymbolIdentity 2 SymbolValue "Main" "main" Nothing
                dataHead = symbolIdentityStableName dataIdentity
                dataDisplayHead = "Box"
                ctorTypeView =
                    (ProgramTypes.mkTypeView (STBase "Box") (STBase dataHead))
                        { ProgramTypes.typeViewHeadIdentities = Map.singleton dataDisplayHead dataIdentity
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__Box"
                        , ctorTypeView = ctorTypeView
                        , ctorForallBinderInfo = []
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = [ctorInfo]
                        }
                valueInfo =
                    ConstructorValue
                        { valueInfoSymbol = ctorIdentity
                        , valueRuntimeName = "Main__Box"
                        , valueCtorInfo = ctorInfo
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "Box" valueInfo)
                        (Map.singleton "Box" dataInfo)
                        Map.empty
                        []
                binding =
                    ProgramTypes.loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                expr =
                    EVar (ResolvedGlobalValue (ProgramTypes.resolvedConstructorInfoSymbol (SymbolLocal "Box") "Box" dataInfo ctorInfo))
                ty =
                    resolvedUnconstrainedType (RSTBase (ProgramTypes.resolvedDataInfoSymbol (SymbolLocal "Box") "Box" dataInfo))
            lowered <-
                case lowerResolvedConstrainedExprBinding scope binding ty False expr of
                    Left err -> expectationFailure ("resolved constructor lowering failed: " ++ show err) >> fail "resolved constructor lowering failed"
                    Right lowered0 -> pure lowered0
            case Map.elems (loweredBindingDeferredObligations lowered) of
                [DeferredConstructor deferred] ->
                    deferredRefIdentity (deferredConstructorRef deferred) `shouldBe` UniqueIdentity 3
                obligations ->
                    expectationFailure ("expected one deferred constructor obligation, got " ++ show obligations)
            (loweredBindingSourceTypeView lowered >>= Map.lookup dataDisplayHead . ProgramTypes.typeViewHeadIdentities)
                `shouldBe` Just dataIdentity
            (loweredBindingSourceTypeView lowered >>= Map.lookup dataHead . ProgramTypes.typeViewHeadIdentities)
                `shouldBe` Just dataIdentity
            (loweredBindingExpectedTypeView lowered >>= Map.lookup dataDisplayHead . ProgramTypes.typeViewHeadIdentities)
                `shouldBe` Just dataIdentity
            (loweredBindingExpectedTypeView lowered >>= Map.lookup dataHead . ProgramTypes.typeViewHeadIdentities)
                `shouldBe` Just dataIdentity

        it "carries constraint-only type binder identities through resolved lowering" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (C, main) {"
                        , "  class C a {"
                        , "  }"
                        , "  def main : C a => Int = 1;"
                        , "}"
                        ]
            checked <- requireChecked program
            binding <- requireCheckedBinding "Main__main" checked
            Map.lookup "a" (ProgramTypes.typeViewBinderIdentities (ProgramTypes.checkedBindingSourceTypeView binding))
                `shouldSatisfy` isJust

        it "uses constructor owner param identities while finalizing constructor value types" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Box Int = Box 1;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] ->
                    case typeViewToElabType scope (constructorTypeView scope ctorInfo) of
                        Right (Elab.TArrow (Elab.TVarRef argRef) resultTy) -> do
                            Elab.typeBinderRefIdentity argRef `shouldBe` paramIdentity
                            elabTypeMentionsBinder paramIdentity resultTy `shouldBe` True
                        other ->
                            expectationFailure ("expected constructor function type, got " ++ show other)
                identities ->
                    expectationFailure ("expected one data param identity, got " ++ show identities)

        it "finalizes constructor bindings from metadata without the surface pipeline" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), main) {"
                        , "  data Option ="
                        , "      None : Option;"
                        , ""
                        , "  def main : Option = None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Option" checked
            ctorInfo <- requireDataConstructor "None" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Option" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            let poisonedLowered =
                    (lowerConstructorBinding scope ctorInfo)
                        { loweredBindingSurfaceExpr = Surface.EVar "$missing_constructor_pipeline_input"
                        }
            binding <-
                case finalizeBindingWithContext finalizeContext poisonedLowered of
                    Left err -> expectationFailure ("metadata constructor finalization failed: " ++ show err) >> fail "metadata constructor finalization failed"
                    Right checkedBinding -> pure checkedBinding
            checkedBindingName binding `shouldBe` "Main__None"
            fmap (symbolDefiningName . constructorRefSymbol) (ProgramTypes.checkedBindingConstructorRef binding)
                `shouldBe` Just "None"

        it "finalizes monomorphic field constructor bindings from metadata without the surface pipeline" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box ="
                        , "      Box : Int -> Box;"
                        , ""
                        , "  def main : Box = Box 1;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            let poisonedLowered =
                    (lowerConstructorBinding scope ctorInfo)
                        { loweredBindingSurfaceExpr = Surface.EVar "$missing_constructor_pipeline_input"
                        }
            binding <-
                case finalizeBindingWithContext finalizeContext poisonedLowered of
                    Left err -> expectationFailure ("metadata constructor finalization failed: " ++ show err) >> fail "metadata constructor finalization failed"
                    Right checkedBinding -> pure checkedBinding
            checkedBindingName binding `shouldBe` "Main__Box"
            fmap (symbolDefiningName . constructorRefSymbol) (ProgramTypes.checkedBindingConstructorRef binding)
                `shouldBe` Just "Box"
            resolvedLocalBinders (checkedBindingTerm binding)
                `shouldSatisfy` any
                    ( \localRef ->
                        localRefName localRef == "$Box_arg1"
                            && isGeneratedLocalRef localRef
                    )

        it "finalizes monomorphic multi-constructor bindings from metadata without the surface pipeline" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Nat(..), main) {"
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat;"
                        , ""
                        , "  def main : Nat = Succ Zero;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Nat" checked
            let scope = mkElaborateScope Map.empty (Map.singleton "Nat" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            mapM_
                ( \ctorName0 -> do
                    ctorInfo <- requireDataConstructor ctorName0 dataInfo
                    let poisonedLowered =
                            (lowerConstructorBinding scope ctorInfo)
                                { loweredBindingSurfaceExpr = Surface.EVar "$missing_constructor_pipeline_input"
                                }
                    binding <-
                        case finalizeBindingWithContext finalizeContext poisonedLowered of
                            Left err -> expectationFailure ("metadata constructor finalization failed: " ++ show err) >> fail "metadata constructor finalization failed"
                            Right checkedBinding -> pure checkedBinding
                    fmap (symbolDefiningName . constructorRefSymbol) (ProgramTypes.checkedBindingConstructorRef binding)
                        `shouldBe` Just ctorName0
                )
                ["Zero", "Succ"]

        it "does not pick an arbitrary constructor metadata match when identities collide" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Nat(..), main) {"
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat;"
                        , ""
                        , "  def main : Nat = Zero;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Nat" checked
            zeroInfo <- requireDataConstructor "Zero" dataInfo
            succInfo <- requireDataConstructor "Succ" dataInfo
            let duplicateDataInfo =
                    dataInfo
                        { dataConstructors =
                            [ zeroInfo
                            , succInfo {ctorInfoSymbol = ctorInfoSymbol zeroInfo}
                            ]
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Nat" duplicateDataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            let poisonedLowered =
                    (lowerConstructorBinding scope zeroInfo)
                        { loweredBindingSurfaceExpr = Surface.EVar "$missing_constructor_pipeline_input"
                        }
            finalizeBindingWithContext finalizeContext poisonedLowered `shouldSatisfy` isLeft

        it "records parameterized constructor binding binders with resolved local identity" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Box Int = Box 1;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            binding <- requireCheckedBinding "Main__Box" checked
            resolvedLocalBinders (checkedBindingTerm binding) `shouldSatisfy` (not . null)
            unresolvedTermVarRefs (checkedBindingTerm binding) `shouldBe` []

        it "finalizes constructor binding types through expected type identity metadata" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Box Int = Box 1;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Box" checked
            binding <- requireCheckedBinding "Main__Box" checked
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] ->
                    case ProgramTypes.checkedBindingType binding of
                        Elab.TForallRef ref _ body -> do
                            Elab.typeBinderRefIdentity ref `shouldBe` paramIdentity
                            elabTypeMentionsBinder paramIdentity body `shouldBe` True
                        other ->
                            expectationFailure ("expected constructor binding forall type, got " ++ show other)
                identities ->
                    expectationFailure ("expected one data param identity, got " ++ show identities)

        it "finalizes non-nullary parameterized constructor bindings from metadata without the surface pipeline" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Box Int = Box 1;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            let poisonedLowered =
                    (lowerConstructorBinding scope ctorInfo)
                        { loweredBindingSurfaceExpr = Surface.EVar "$missing_constructor_pipeline_input"
                        }
            binding <-
                case finalizeBindingWithContext finalizeContext poisonedLowered of
                    Left err -> expectationFailure ("metadata constructor finalization failed: " ++ show err) >> fail "metadata constructor finalization failed"
                    Right checkedBinding -> pure checkedBinding
            checkedBindingName binding `shouldBe` "Main__Box"
            fmap (symbolDefiningName . constructorRefSymbol) (ProgramTypes.checkedBindingConstructorRef binding)
                `shouldBe` Just "Box"
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] -> do
                    let sourceBinderIdentities =
                            ProgramTypes.typeViewBinderIdentities (ProgramTypes.checkedBindingSourceTypeView binding)
                    Map.lookup "a" sourceBinderIdentities `shouldBe` Just paramIdentity
                    Map.lookup (typeBinderIdentityStableName paramIdentity) sourceBinderIdentities `shouldBe` Just paramIdentity
                    leadingTypeAbsIdentities (checkedBindingTerm binding) `shouldBe` [paramIdentity]
                identities ->
                    expectationFailure ("expected one data param identity, got " ++ show identities)
            unresolvedTermVarRefs (checkedBindingTerm binding) `shouldBe` []

        it "does not recover constructor owner params from metadata-free stable spellings" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Box Int = Box 1;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] -> do
                    let stableParamName = typeBinderIdentityStableName paramIdentity
                        metadataFreeLowered =
                            (lowerConstructorBinding scope ctorInfo)
                                { loweredBindingExpectedType =
                                    STForall
                                        stableParamName
                                        Nothing
                                        ( STArrow
                                            (STVar stableParamName)
                                            (STCon "Box" (STVar stableParamName :| []))
                                        )
                                , loweredBindingExpectedTypeView = Nothing
                                , loweredBindingSurfaceExpr = Surface.EVar "$missing_constructor_pipeline_input"
                                }
                    binding <-
                        case finalizeBindingWithContext finalizeContext metadataFreeLowered of
                            Left err -> expectationFailure ("metadata constructor finalization failed: " ++ show err) >> fail "metadata constructor finalization failed"
                            Right checkedBinding -> pure checkedBinding
                    let leadingIdentities = leadingTypeAbsIdentities (checkedBindingTerm binding)
                    leadingIdentities `shouldSatisfy` (not . null)
                    leadingIdentities `shouldSatisfy` all (/= paramIdentity)
                identities ->
                    expectationFailure ("expected one data param identity, got " ++ show identities)

        it "finalizes parameterized nullary constructor bindings from metadata without the surface pipeline" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), main) {"
                        , "  data Option a ="
                        , "      None : Option a;"
                        , ""
                        , "  def main : Option Bool = None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Option" checked
            ctorInfo <- requireDataConstructor "None" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Option" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            let poisonedLowered =
                    (lowerConstructorBinding scope ctorInfo)
                        { loweredBindingSurfaceExpr = Surface.EVar "$missing_constructor_pipeline_input"
                        }
            binding <-
                case finalizeBindingWithContext finalizeContext poisonedLowered of
                    Left err -> expectationFailure ("metadata constructor finalization failed: " ++ show err) >> fail "metadata constructor finalization failed"
                    Right checkedBinding -> pure checkedBinding
            checkedBindingName binding `shouldBe` "Main__None"
            fmap (symbolDefiningName . constructorRefSymbol) (ProgramTypes.checkedBindingConstructorRef binding)
                `shouldBe` Just "None"
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] -> do
                    let sourceBinderIdentities =
                            ProgramTypes.typeViewBinderIdentities (ProgramTypes.checkedBindingSourceTypeView binding)
                    Map.lookup "a" sourceBinderIdentities `shouldBe` Just paramIdentity
                    Map.lookup (typeBinderIdentityStableName paramIdentity) sourceBinderIdentities `shouldBe` Just paramIdentity
                identities ->
                    expectationFailure ("expected one data param identity, got " ++ show identities)
            unresolvedTermVarRefs (checkedBindingTerm binding) `shouldBe` []

        it "records lambda and let binders with resolved local identity" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (apply, main) {"
                        , "  def apply : Int -> Int = λx let y = x in y;"
                        , "  def main : Int = apply 1;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            applyBinding <- requireCheckedBinding "Main__apply" checked
            let term = checkedBindingTerm applyBinding
                binderRefs = resolvedLocalBinders term
                occurrenceRefs = resolvedLocalOccurrences term
                resolvedRefs =
                    [ ref
                    | resolvedModule <- ProgramTypes.resolvedProgramModules (ProgramTypes.checkedProgramResolved checked)
                    , resolvedModuleName resolvedModule == "Main"
                    , DeclDef defDecl <- moduleDecls (resolvedModuleSyntax resolvedModule)
                    , refDisplayName (defDeclName defDecl) == "apply"
                    , ELam param (ELet letRef _ _ _) <- [defDeclExpr defDecl]
                    , ref <- [paramName param, letRef]
                    ]
            resolvedRefs `shouldSatisfy` (not . null)
            binderRefs `shouldMatchList` resolvedRefs
            occurrenceRefs `shouldMatchList` resolvedRefs

        it "records case pattern binders with resolved local identity" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), get, main) {"
                        , "  data Box = Box : Int -> Box;"
                        , "  def get : Box -> Int = λbox case box of { Box value -> value };"
                        , "  def main : Int = get (Box 1);"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            getBinding <- requireCheckedBinding "Main__get" checked
            let term = checkedBindingTerm getBinding
                binderRefs = resolvedLocalBinders term
                occurrenceRefs = resolvedLocalOccurrences term
                resolvedPatternRefs =
                    [ valueRef
                    | resolvedModule <- ProgramTypes.resolvedProgramModules (ProgramTypes.checkedProgramResolved checked)
                    , resolvedModuleName resolvedModule == "Main"
                    , DeclDef defDecl <- moduleDecls (resolvedModuleSyntax resolvedModule)
                    , refDisplayName (defDeclName defDecl) == "get"
                    , ELam _ (ECase _ [Alt (PatCtor _ [PatVar valueRef]) _]) <- [defDeclExpr defDecl]
                    ]
            resolvedPatternRefs `shouldSatisfy` (not . null)
            binderRefs `shouldSatisfy` (\refs -> all (`elem` refs) resolvedPatternRefs)
            occurrenceRefs `shouldSatisfy` (\refs -> all (`elem` refs) resolvedPatternRefs)

        it "does not rename grouped placeholder spellings under resolved local binders" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let firstLocal = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991801)) "source_x"
                secondLocal = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991802)) "source_x"
                firstIdentity = generatedSymbolIdentity 991803 SymbolValue "Main" "first" Nothing
                secondIdentity = generatedSymbolIdentity 991804 SymbolValue "Main" "second" Nothing
                firstDeferred = deferredRefFromIdentity (UniqueIdentity 991805) "x"
                secondDeferred = deferredRefFromIdentity (UniqueIdentity 991806) "x"
                dataIdentity = generatedSymbolIdentity 991807 SymbolType "Main" "Phantom" Nothing
                phantomData =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                obligation deferredRef =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseRef = deferredRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeType = STBase "Int"
                            , deferredCaseResultType = STBase "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                intTy = STBase "Int"
                functionTy = STArrow intTy intTy
                localIdentityExpr runtimeName = Surface.ELamAnn runtimeName intTy (Surface.EVar runtimeName)
                lowered name identity localRef deferredRef =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails name (TopLevelId identity)
                        , loweredBindingSourceType = functionTy
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = functionTy
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = localIdentityExpr "x"
                        , loweredBindingResolvedLocalIdentities =
                            [ProgramTypes.LoweredResolvedLocalIdentity (renameLocalRef "x" localRef) localRef]
                        , loweredBindingDeferredObligations = Map.singleton deferredRef (obligation deferredRef)
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            checked <-
                case finalizeBindingsAllowOpaqueWithContext finalizeContext
                    [ lowered "Main__first" firstIdentity firstLocal firstDeferred
                    , lowered "Main__second" secondIdentity secondLocal secondDeferred
                    ] of
                    Right bindings -> pure bindings
                    Left err -> expectationFailure ("finalize group failed: " ++ show err) >> fail "finalize group failed"
            map (resolvedLocalBinders . checkedBindingTerm) checked `shouldBe` [[firstLocal], [secondLocal]]
            map (resolvedLocalOccurrences . checkedBindingTerm) checked `shouldBe` [[firstLocal], [secondLocal]]
            map (map localRefName . resolvedLocalBinders . checkedBindingTerm) checked `shouldBe` [["source_x"], ["source_x"]]
            map (map localRefName . resolvedLocalOccurrences . checkedBindingTerm) checked `shouldBe` [["source_x"], ["source_x"]]

        it "keeps graph local refs distinct from generated local refs" $ do
            let graphRef = localRefFromNodeId "x" (NodeId 0)
                generatedRef = localRefFromIdentity (GeneratedLocalId (UniqueIdentity (-1))) "x"
            localRefIdentity graphRef `shouldBe` GraphLocalId (NodeId 0)
            localRefMatchesNodeId graphRef (NodeId 0) `shouldBe` True
            localRefMatchesNodeId graphRef (NodeId 1) `shouldBe` False
            localRefMatchesNodeId generatedRef (NodeId 0) `shouldBe` False
            localIdentityStableUnique (localRefIdentity graphRef) `shouldNotBe` localIdentityStableUnique (localRefIdentity generatedRef)
            isGeneratedLocalRef graphRef `shouldBe` False

        it "stores checked program executable references as resolved identity terms" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), apply, main) {"
                        , "  data Option ="
                        , "      None : Option"
                        , "    | Some : Int -> Option;"
                        , ""
                        , "  def apply : Int -> Int = λx let y = x in y;"
                        , "  def main : Option = Some (apply 1);"
                        , "}"
                        ]
            checked <- requireChecked program
            checkedProgramUnresolvedTermVarNames checked `shouldBe` []

        it "records polymorphic let binders with complete scheme sidecars" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Int = let id : ∀a. a -> a = λx x in id 1;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            mainBinding <- requireCheckedBinding "Main__main" checked
            resolvedLocalLetTypes (checkedBindingTerm mainBinding)
                `shouldSatisfy` any isPolymorphicIdentityType

        it "decodes Church data using resolved local handler identity" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), main) {"
                        , "  data Option ="
                        , "      None : Option;"
                        , ""
                        , "  def main : Option = None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            let handlerType = testTVar "r"
                handlerRef = generatedLocalRefForName "$None-handler"
                binder =
                    ResolvedVar
                        { resolvedVarRuntimeName = "runtime-handler"
                        , resolvedVarType = handlerType
                        , resolvedVarDetails = LocalId handlerRef
                        }
                occurrence =
                    binder
                        { resolvedVarRuntimeName = "stale-runtime-handler"
                        , resolvedVarDetails =
                            LocalId
                                (renameLocalRef "$stale-handler-reference" handlerRef)
                        }
                churchNone =
                    mkTestTyAbs "r" Nothing $
                        Elab.ELam binder (Elab.EVarNode occurrence)
                checked' = replaceCheckedBindingTerm "Main__main" churchNone checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "None\n"

        it "does not decode Church data through duplicate handler identities" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), main) {"
                        , "  data Option ="
                        , "      None : Option"
                        , "    | Some : Int -> Option;"
                        , ""
                        , "  def main : Option = None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            let handlerType = testTVar "r"
                handlerArgType = Elab.TBase (BaseTy "Int")
                handlerRef = generatedLocalRefForName "$Option-handler"
                noneHandler =
                    ResolvedVar
                        { resolvedVarRuntimeName = "none-handler"
                        , resolvedVarType = handlerType
                        , resolvedVarDetails = LocalId handlerRef
                        }
                someHandler =
                    noneHandler
                        { resolvedVarRuntimeName = "some-handler"
                        , resolvedVarType = Elab.TArrow handlerArgType handlerType
                        , resolvedVarDetails =
                            LocalId (renameLocalRef "$same-option-handler" handlerRef)
                        }
                occurrence =
                    noneHandler
                        { resolvedVarRuntimeName = "selected-handler"
                        , resolvedVarDetails =
                            LocalId (renameLocalRef "$selected-option-handler" handlerRef)
                        }
                churchAmbiguous =
                    mkTestTyAbs "r" Nothing $
                        Elab.ELam noneHandler $
                            Elab.ELam someHandler (Elab.EVarNode occurrence)
                checked' = replaceCheckedBindingTerm "Main__main" churchAmbiguous checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldNotBe` Right "None\n"

        it "does not decode ambiguous fallback Church data by data iteration order" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (SoloA(..), SoloB(..), Option(..), main) {"
                        , "  data SoloA ="
                        , "      OnlyA : SoloA;"
                        , ""
                        , "  data SoloB ="
                        , "      OnlyB : SoloB;"
                        , ""
                        , "  data Option ="
                        , "      None : Option"
                        , "    | Some : Int -> Option;"
                        , ""
                        , "  def main : Option = None;"
                        , "}"
                        ]
            checked <- requireChecked program
            let handlerType = testTVar "r"
                handlerArgType = Elab.TBase (BaseTy "Int")
                handlerRef = generatedLocalRefForName "$Option-fallback-handler"
                noneHandler =
                    ResolvedVar
                        { resolvedVarRuntimeName = "none-handler"
                        , resolvedVarType = handlerType
                        , resolvedVarDetails = LocalId handlerRef
                        }
                someHandler =
                    noneHandler
                        { resolvedVarRuntimeName = "some-handler"
                        , resolvedVarType = Elab.TArrow handlerArgType handlerType
                        , resolvedVarDetails =
                            LocalId (renameLocalRef "$same-option-fallback-handler" handlerRef)
                        }
                occurrence =
                    noneHandler
                        { resolvedVarRuntimeName = "selected-handler"
                        , resolvedVarDetails =
                            LocalId (renameLocalRef "$selected-option-fallback-handler" handlerRef)
                        }
                churchAmbiguous =
                    mkTestTyAbs "r" Nothing $
                        Elab.ELam noneHandler $
                            Elab.ELam someHandler (Elab.EVarNode occurrence)
                checked' = replaceCheckedBindingTerm "Main__main" churchAmbiguous checked
                output = programRunOutput <$> runCheckedProgramOutput checked'
            output `shouldNotBe` Right "OnlyA\n"
            output `shouldNotBe` Right "OnlyB\n"

        it "decodes Church data using checked source type head identity metadata" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), main) {"
                        , "  data Option ="
                        , "      None : Option;"
                        , ""
                        , "  def main : Option = None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Option" checked
            let displayHead = ProgramTypes.dataInfoIdentityName dataInfo
                identityHead = symbolIdentityStableName (dataInfoSymbol dataInfo)
                checked' =
                    replaceCheckedBindingSourceTypeView
                        "Main__main"
                        ( (ProgramTypes.mkTypeView (STBase displayHead) (STBase identityHead))
                            { ProgramTypes.typeViewHeadIdentities = Map.singleton displayHead (dataInfoSymbol dataInfo)
                            }
                        )
                        checked
                checkedWithoutHeadMetadata =
                    replaceCheckedBindingSourceTypeWithHeadIdentities
                        "Main__main"
                        (STBase identityHead)
                        Map.empty
                        checked
                checkedWithDisplayHeadMetadata =
                    replaceCheckedBindingSourceTypeWithHeadIdentities
                        "Main__main"
                        (STBase identityHead)
                        (Map.singleton displayHead (dataInfoSymbol dataInfo))
                        checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "None\n"
            (programRunOutput <$> runCheckedProgramOutput checkedWithDisplayHeadMetadata) `shouldBe` Right "None\n"
            (programRunOutput <$> runCheckedProgramOutput checkedWithoutHeadMetadata) `shouldNotBe` Right "None\n"

        it "decodes parameterized Church data using checked source type head identity metadata" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), Box(..), main) {"
                        , "  data Option ="
                        , "      None : Option;"
                        , ""
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Box Option = Box None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Box" checked
            optionInfo <- requireCheckedData "Main" "Option" checked
            let displayHead = ProgramTypes.dataInfoIdentityName dataInfo
                displayOptionHead = ProgramTypes.dataInfoIdentityName optionInfo
                stableHead = symbolIdentityStableName (dataInfoSymbol dataInfo)
                stableOptionHead = symbolIdentityStableName (dataInfoSymbol optionInfo)
                checked' =
                    replaceCheckedBindingSourceTypeView
                        "Main__main"
                        ( (ProgramTypes.mkTypeView
                            (STCon displayHead (STBase displayOptionHead :| []))
                            (STCon stableHead (STBase stableOptionHead :| []))
                          )
                            { ProgramTypes.typeViewHeadIdentities =
                                Map.fromList
                                    [ (displayHead, dataInfoSymbol dataInfo)
                                    , (displayOptionHead, dataInfoSymbol optionInfo)
                                    ]
                            }
                        )
                        checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "Box None\n"

        it "decodes constructor fields using constructor head identity payloads when metadata keys are stale" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), Box(..), main) {"
                        , "  data Option ="
                        , "      None : Option;"
                        , ""
                        , "  data Box ="
                        , "      Box : Option -> Box;"
                        , ""
                        , "  def main : Box = Box None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            boxInfo <- requireCheckedData "Main" "Box" checked
            optionInfo <- requireCheckedData "Main" "Option" checked
            let boxDisplayHead = ProgramTypes.dataInfoIdentityName boxInfo
                optionDisplayHead = ProgramTypes.dataInfoIdentityName optionInfo
                boxStableHead = symbolIdentityStableName (ProgramTypes.dataInfoSymbol boxInfo)
                optionStableHead = symbolIdentityStableName (ProgramTypes.dataInfoSymbol optionInfo)
                ctorView =
                    ( ProgramTypes.mkTypeView
                        (STArrow (STBase optionStableHead) (STBase boxStableHead))
                        (STArrow (STBase optionStableHead) (STBase boxStableHead))
                    )
                        { ProgramTypes.typeViewHeadIdentities =
                            Map.fromList
                                [ (boxDisplayHead, ProgramTypes.dataInfoSymbol boxInfo)
                                , (optionDisplayHead, ProgramTypes.dataInfoSymbol optionInfo)
                                ]
                        }
                checked' = replaceCheckedConstructorTypeView "Main__Box" ctorView checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "Box None\n"

        it "decodes parameterized Church data using constructor field binder identity metadata" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), Box(..), main) {"
                        , "  data Option ="
                        , "      None : Option;"
                        , ""
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Box Option = Box None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] -> do
                    let staleBinder = "$stale_box_param"
                        oldView = ProgramTypes.ctorTypeView ctorInfo
                        (_, displayResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewDisplay oldView)))
                        (_, identityResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewIdentity oldView)))
                        poisonedView =
                            oldView
                                { ProgramTypes.typeViewDisplay = STArrow (STVar staleBinder) displayResult
                                , ProgramTypes.typeViewIdentity = STArrow (STVar staleBinder) identityResult
                                , ProgramTypes.typeViewBinderIdentities =
                                    Map.insert staleBinder paramIdentity (ProgramTypes.typeViewBinderIdentities oldView)
                                }
                        checked' = replaceCheckedConstructorTypeView "Main__Box" poisonedView checked
                    (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "Box None\n"
                identities ->
                    expectationFailure ("expected one data param identity, got " ++ show identities)

        it "runs parameterized constructors using constructor field binder identity metadata" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Box String = Box \"placeholder\";"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] -> do
                    let staleBinder = "$stale_runtime_box_param"
                        oldView = ProgramTypes.ctorTypeView ctorInfo
                        (_, displayResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewDisplay oldView)))
                        (_, identityResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewIdentity oldView)))
                        poisonedView =
                            oldView
                                { ProgramTypes.typeViewDisplay = STArrow (STVar staleBinder) displayResult
                                , ProgramTypes.typeViewIdentity = STArrow (STVar staleBinder) identityResult
                                , ProgramTypes.typeViewBinderIdentities =
                                    Map.insert staleBinder paramIdentity (ProgramTypes.typeViewBinderIdentities oldView)
                                }
                        poisonedCtor = ctorInfo {ProgramTypes.ctorTypeView = poisonedView}
                        ctorTy = ProgramTypes.ctorTypeView poisonedCtor
                        stringFromInt =
                            ResolvedVar
                                { resolvedVarRuntimeName = PrimitiveInventory.stringFromIntPrimitiveName
                                , resolvedVarType = Elab.TArrow (Elab.TBase (BaseTy "Int")) (Elab.TBase (BaseTy "String"))
                                , resolvedVarDetails =
                                    PrimitiveId (primitiveRefFromSymbol (Builtins.builtinValueIdentity PrimitiveInventory.stringFromIntPrimitiveName))
                                }
                    ctorResolvedTy <-
                        either
                            (\err -> expectationFailure ("constructor type conversion failed: " ++ show err) >> fail "constructor type conversion failed")
                            pure
                            (typeViewToElabType (checkedProgramElaborateScope checked) ctorTy)
                    let ctorValue =
                            ConstructorValue
                                { valueInfoSymbol = ProgramTypes.ctorInfoSymbol poisonedCtor
                                , valueRuntimeName = ProgramTypes.ctorRuntimeName poisonedCtor
                                , valueCtorInfo = poisonedCtor
                                }
                        ctorResolved = ProgramTypes.resolvedVarFromValueInfo ctorValue ctorResolvedTy
                        checkedTerm =
                            Elab.EApp
                                (Elab.EVarNode ctorResolved)
                                (Elab.EApp (Elab.EVarNode stringFromInt) (Elab.ELit (LInt 7)))
                        checked' =
                            replaceCheckedBindingTerm
                                "Main__main"
                                checkedTerm
                                (replaceCheckedConstructorTypeView "Main__Box" poisonedView checked)
                    (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "Box \"7\"\n"
                identities ->
                    expectationFailure ("expected one data param identity, got " ++ show identities)

        it "decodes checked main data by checked type identity before source type spelling" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module A export (Box(..)) {"
                        , "  data Box ="
                        , "      ABox : Box;"
                        , "}"
                        , ""
                        , "module B export (Box(..)) {"
                        , "  data Box ="
                        , "      BBox : Box;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import B as B;"
                        , "  def main : B.Box = B.BBox;"
                        , "}"
                        ]
            checked <- requireCheckedLocated located
            bData <- requireCheckedData "B" "Box" checked
            let checked' =
                    replaceCheckedBindingSourceType
                        "Main__main"
                        (STBase "Box")
                        checked
                checkedWithoutHeadMetadata =
                    replaceCheckedBindingSourceTypeWithHeadIdentities
                        "Main__main"
                        (STBase "Box")
                        Map.empty
                        checked
                checkedByElabIdentity =
                    replaceCheckedBindingType
                        "Main__main"
                        (Elab.TBaseWithIdentity (Just (ProgramTypes.dataInfoSymbol bData)) (BaseTy "$stale.Box"))
                        checkedWithoutHeadMetadata
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "BBox\n"
            (programRunOutput <$> runCheckedProgramOutput checkedWithoutHeadMetadata)
                `shouldNotBe` Right "BBox\n"
            (programRunOutput <$> runCheckedProgramOutput checkedByElabIdentity)
                `shouldBe` Right "BBox\n"

        it "decodes checked main data by source display head identity when identity spelling is stale" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Option(..), main) {"
                        , "  data Option ="
                        , "      None : Option;"
                        , ""
                        , "  def main : Option = None;"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            optionInfo <- requireCheckedData "Main" "Option" checked
            let displayHead = ProgramTypes.dataInfoIdentityName optionInfo
                checked' =
                    replaceCheckedBindingType
                        "Main__main"
                        (Elab.TBaseWithIdentity Nothing (BaseTy "$stale_option"))
                        ( replaceCheckedBindingSourceTypeView
                            "Main__main"
                            ( (ProgramTypes.mkTypeView (STBase displayHead) (STBase "$stale_identity_option"))
                                { ProgramTypes.typeViewHeadIdentities =
                                    Map.singleton displayHead (ProgramTypes.dataInfoSymbol optionInfo)
                                }
                            )
                            checked
                        )
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "None\n"

        it "runs pure primitive calls by resolved primitive identity instead of runtime name" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : String = \"placeholder\";"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            let intTy = Elab.TBase (BaseTy "Int")
                stringTy = Elab.TBase (BaseTy "String")
                primitiveTy = Elab.TArrow intTy stringTy
                staleStringFromInt =
                    ResolvedVar
                        { resolvedVarRuntimeName = "$stale_string_from_int"
                        , resolvedVarType = primitiveTy
                        , resolvedVarDetails =
                            PrimitiveId (primitiveRefFromSymbol (Builtins.builtinValueIdentity PrimitiveInventory.stringFromIntPrimitiveName))
                        }
                checkedTerm = Elab.EApp (Elab.EVarNode staleStringFromInt) (Elab.ELit (LInt 7))
                checked' = replaceCheckedBindingTerm "Main__main" checkedTerm checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "\"7\"\n"

        it "does not run primitive-spelled top-level refs without primitive identity" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : String = \"placeholder\";"
                        , "}"
                        ]
            checked <- requireChecked (withPrelude program)
            let intTy = Elab.TBase (BaseTy "Int")
                stringTy = Elab.TBase (BaseTy "String")
                primitiveTy = Elab.TArrow intTy stringTy
                fakePrimitiveSpelling =
                    ResolvedVar
                        { resolvedVarRuntimeName = PrimitiveInventory.stringFromIntPrimitiveName
                        , resolvedVarType = primitiveTy
                        , resolvedVarDetails =
                            TopLevelId (generatedSymbolIdentity 1031 SymbolValue "Main" "notStringFromInt" Nothing)
                        }
                checkedTerm = Elab.EApp (Elab.EVarNode fakePrimitiveSpelling) (Elab.ELit (LInt 7))
                checked' = replaceCheckedBindingTerm "Main__main" checkedTerm checked
            (programRunOutput <$> runCheckedProgramOutput checked')
                `shouldNotBe` Right "\"7\"\n"

        it "runs Prelude stringFromList by resolved identity instead of runtime name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (List(..), stringFromList);"
                        , "  def main : String = stringFromList Nil;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            mainBinding <- requireCheckedBinding "Main__main" checked
            let checked' =
                    replaceCheckedBindingTerm
                        "Main__main"
                        (poisonPrimitiveRuntimeNames "$stale_string_from_list" (checkedBindingTerm mainBinding))
                        checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "\"\"\n"

        it "indexes runtime Prelude support by module identity instead of checked module name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (List(..), stringFromList);"
                        , "  def main : String = stringFromList Nil;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' = renameCheckedModuleName "Prelude" "$stale_prelude_module_name" checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "\"\"\n"

        it "indexes runtime Prelude constructors by owner identity and index instead of constructor name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (List(..), stringFromList);"
                        , "  def main : String = stringFromList Nil;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' = renameCheckedConstructorIdentityNamesWhere (== "Nil") "$stale_nil_identity_name" checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "\"\"\n"

        it "indexes runtime Prelude constructors by exported type identity instead of data name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (List(..), stringFromList);"
                        , "  def main : String = stringFromList Nil;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' = renameCheckedDataIdentityNamesWhere (== "List") "$stale_list_data_name" checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "\"\"\n"

        it "indexes runtime Prelude constructors by owner identity instead of exported type display" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (List(..), stringFromList);"
                        , "  def main : String = stringFromList Nil;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' = renameCheckedExportedTypeDisplaysWhere (== "List") "$stale_list_export_display" checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "\"\"\n"

        it "runs checked IO terms by resolved identity instead of retained surface names" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_bind (__io_pure 1) (λ(_n : Int) __io_putStrLn \"term-runtime\");"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' =
                    replaceCheckedBindingSurfaceExpr
                        "Main__main"
                        (Surface.EVar "$stale_surface_should_not_run")
                        checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "term-runtime\n"

        it "runs checked IO main by resolved binding identity instead of checked binding name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_putStrLn \"binding-identity\";"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' = renameCheckedBindingName "Main__main" "$stale_main_binding_name" checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "binding-identity\n"

        it "classifies checked IO main by checked type identity instead of source type name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_putStrLn \"type-identity\";"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' =
                    replaceCheckedBindingSourceType
                        "Main__main"
                        (STCon "$stale_io" (STBase "$stale_unit" :| []))
                        checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "type-identity\n"

        it "keeps checked pure dependencies reachable by resolved identity instead of checked binding name" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def helper : Int = 41;"
                        , "  def main : Int = helper;"
                        , "}"
                        ]
            checked <- requireChecked program
            let checked' = renameCheckedBindingName "Main__helper" "$stale_helper_binding_name" checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "41\n"

        it "keeps runtime recursion detection keyed by resolved identity instead of checked binding name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def helper : IO Unit = __io_putStrLn \"helper\";"
                        , "  def main : IO Unit = helper;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' = renameCheckedBindingName "Main__helper" "Main__main" checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "helper\n"

        it "runs checked IO constructors by resolved constructor identity instead of constructor runtime name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, pure, putStrLn);"
                        , "  data Option ="
                        , "      None : Option;"
                        , "  def action : IO Option = pure None;"
                        , "  def after : Option -> IO Unit = λvalue case value of {"
                        , "    None -> putStrLn \"ctor-identity\""
                        , "  };"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' =
                    renameCheckedConstructorRuntimeNamesWhere
                        (== "Main__None")
                        "$stale_none_runtime_name"
                        checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "ctor-identity\n"

        it "rejects duplicate checked runtime binding identities before run context lookup" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def helper : Int = 0;"
                        , "  def main : Int = helper;"
                        , "}"
                        ]
            checked <- requireChecked program
            mainBinding <- requireCheckedBinding "Main__main" checked
            duplicateIdentity <- requireTopLevelIdentity mainBinding
            let checked' = replaceCheckedBindingTopLevelIdentity "Main__helper" duplicateIdentity checked
            case runCheckedProgramOutput checked' of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "duplicate checked binding identity"
                    message `shouldSatisfy` isInfixOf (symbolIdentityStableName duplicateIdentity)
                other ->
                    expectationFailure ("expected duplicate binding identity rejection, got " ++ show other)

        it "rejects duplicate checked runtime module identities before run context lookup" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Lib export (helper) {"
                        , "  def helper : Int = 0;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Lib exposing (helper);"
                        , "  def main : Int = helper;"
                        , "}"
                        ]
            checked <- requireChecked program
            case checkedProgramModules checked of
                firstModule : secondModule : _ -> do
                    let duplicateIdentity = checkedModuleIdentity firstModule
                        checked' = replaceCheckedModuleIdentity (checkedModuleName secondModule) duplicateIdentity checked
                    case runCheckedProgramOutput checked' of
                        Left (ProgramPipelineError message) -> do
                            message `shouldSatisfy` isInfixOf "duplicate checked module identity"
                            message `shouldSatisfy` isInfixOf (symbolIdentityStableName duplicateIdentity)
                        other ->
                            expectationFailure ("expected duplicate module identity rejection, got " ++ show other)
                _ ->
                    expectationFailure "expected two checked modules"

        it "rejects duplicate checked runtime data identities before run context lookup" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  data ABox ="
                        , "      MkABox : ABox;"
                        , "  data BBox ="
                        , "      MkBBox : BBox;"
                        , "  def main : ABox = MkABox;"
                        , "}"
                        ]
            checked <- requireChecked program
            aData <- requireCheckedData "Main" "ABox" checked
            bData <- requireCheckedData "Main" "BBox" checked
            let duplicateIdentity = dataInfoSymbol aData
                checked' = replaceCheckedDataSymbol (dataInfoSymbol bData) duplicateIdentity checked
            case runCheckedProgramOutput checked' of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "duplicate checked data identity"
                    message `shouldSatisfy` isInfixOf (symbolIdentityStableName duplicateIdentity)
                other ->
                    expectationFailure ("expected duplicate data identity rejection, got " ++ show other)

        it "rejects duplicate checked runtime constructor identities before run context lookup" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  data Option ="
                        , "      None : Option"
                        , "    | Some : Int -> Option;"
                        , "  def main : Option = None;"
                        , "}"
                        ]
            checked <- requireChecked program
            dataInfo <- requireCheckedData "Main" "Option" checked
            case dataConstructors dataInfo of
                firstCtor : secondCtor : _ -> do
                    let duplicateIdentity = ctorInfoSymbol firstCtor
                        checked' = replaceCheckedConstructorSymbol (ctorInfoSymbol secondCtor) duplicateIdentity checked
                    case runCheckedProgramOutput checked' of
                        Left (ProgramPipelineError message) -> do
                            message `shouldSatisfy` isInfixOf "duplicate checked constructor identity"
                            message `shouldSatisfy` isInfixOf (symbolIdentityStableName duplicateIdentity)
                        other ->
                            expectationFailure ("expected duplicate constructor identity rejection, got " ++ show other)
                _ ->
                    expectationFailure "expected two constructors"

        it "rejects duplicate runtime Prelude constructor keys before support lookup" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (List(..), stringFromList);"
                        , "  def main : String = stringFromList Nil;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            listData <- requireCheckedData "Prelude" "List" checked
            nilCtor <- requireDataConstructor "Nil" listData
            consCtor <- requireDataConstructor "Cons" listData
            let checked' = replaceCheckedConstructorIndex (ctorInfoSymbol consCtor) (ctorIndex nilCtor) checked
            case runCheckedProgramOutput checked' of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "duplicate checked Prelude constructor key"
                    message `shouldSatisfy` isInfixOf "List.Nil"
                other ->
                    expectationFailure ("expected duplicate Prelude constructor key rejection, got " ++ show other)

        it "runs checked IO local environments by resolved local identity instead of binder spelling" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_putStrLn \"placeholder\";"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let intElabTy = Elab.TBase (BaseTy "Int")
                outer = generatedResolvedLocal 100 "x" "runtime-x" intElabTy
                inner = generatedResolvedLocal 101 "x" "runtime-x" intElabTy
                ioBind = primitiveTerm "__io_bind"
                ioPure = primitiveTerm "__io_pure"
                ioPutStrLn = primitiveTerm "__io_putStrLn"
                stringFromInt = primitiveTerm "__string_from_int"
                checkedTerm =
                    Elab.EApp
                        (Elab.EApp ioBind (Elab.EApp ioPure (Elab.ELit (LInt 1))))
                        ( Elab.ELam outer $
                            Elab.EApp
                                ( Elab.ELam inner $
                                    Elab.EApp ioPutStrLn (Elab.EApp stringFromInt (Elab.EVarNode outer))
                                )
                                (Elab.ELit (LInt 2))
                        )
                checked' = replaceCheckedBindingTerm "Main__main" checkedTerm checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "1\n"

    describe "MLF.Program parse/pretty" $ do
        mapM_ roundtripFixture fixturePaths

        it "roundtrips first-order declaration parameters unchanged" $ do
            let programText =
                    unlines
                        [ "module Main export (Eq, Box(..)) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , "}"
                        ]
            program <- requireParsed programText
            parseRawProgram (prettyProgram program) `shouldBe` Right program

        it "roundtrips higher-kinded declaration parameter annotations" $ do
            let hk = KArrow KType KType
                hk2 = KArrow KType (KArrow KType KType)
                programText =
                    unlines
                        [ "module Main export (Functor, Higher(..)) {"
                        , "  class Functor (f :: * -> *) {"
                        , "    identity : ∀ a. a -> a;"
                        , "  }"
                        , ""
                        , "  data Higher (p :: * -> * -> *) a ="
                        , "      Higher : a -> Higher p a;"
                        , "}"
                        ]
            program <- requireParsed programText
            case program of
                Program [Module {moduleDecls = [DeclClass classDecl, DeclData dataDecl]}] -> do
                    classDeclParam classDecl `shouldBe` TypeParam "f" hk
                    dataDeclParams dataDecl `shouldBe` [TypeParam "p" hk2, firstOrderTypeParam "a"]
                other -> expectationFailure ("unexpected program shape: " ++ show other)
            parseRawProgram (prettyProgram program) `shouldBe` Right program

        it "roundtrips superclass constraints, multi-parameter classes, and Unicode fundeps" $ do
            let hk = KArrow KType KType
                programText =
                    unlines
                        [ "module Main export (Monad) {"
                        , "  class Functor f => Monad (m :: * -> *) (f :: * -> *) | m → f {"
                        , "    bind : ∀ a b. m a -> (a -> m b) -> m b;"
                        , "  }"
                        , ""
                        , "  instance Monad IO IO {"
                        , "  }"
                        , "}"
                        ]
            program <- requireParsed programText
            case program of
                Program [Module {moduleDecls = [DeclClass classDecl, DeclInstance instanceDecl]}] -> do
                    classDeclParams classDecl `shouldBe` (TypeParam "m" hk :| [TypeParam "f" hk])
                    classDeclSuperclasses classDecl
                        `shouldBe` [ ClassConstraint
                                        { constraintClassName = "Functor"
                                        , constraintTypes = STVar "f" :| []
                                        }
                                   ]
                    classDeclFundeps classDecl
                        `shouldBe` [ FunctionalDependency
                                        { fundepDeterminers = "m" :| []
                                        , fundepDetermined = "f" :| []
                                        }
                                   ]
                    instanceDeclTypes instanceDecl `shouldBe` (STBase "IO" :| [STBase "IO"])
                other -> expectationFailure ("unexpected program shape: " ++ show other)
            parseRawProgram (prettyProgram program) `shouldBe` Right program

        it "roundtrips closed type-family declarations with kind variables" $ do
            let programText =
                    unlines
                        [ "module Main {"
                        , "  type family Normalize (a :: κ) :: κ where {"
                        , "    Normalize Int = Int;"
                        , "    Normalize (Box a) = a;"
                        , "    Normalize a = (Λx. x) a;"
                        , "  }"
                        , "}"
                        ]
            program <- requireParsed programText
            case program of
                Program [Module {moduleDecls = [DeclTypeFamily familyDecl]}] -> do
                    familyDeclName familyDecl `shouldBe` "Normalize"
                    familyDeclParams familyDecl `shouldBe` [("a", TLKVar "κ")]
                    familyDeclResultKind familyDecl `shouldBe` TLKVar "κ"
                    case familyDeclEquations familyDecl of
                        [intEq, boxEq, idEq] -> do
                            familyEquationPatterns intEq `shouldBe` [TLPCon "Int" []]
                            familyEquationRhs intEq `shouldBe` TLTCon "Int"
                            familyEquationPatterns boxEq `shouldBe` [TLPCon "Box" [TLPVar "a"]]
                            familyEquationRhs boxEq `shouldBe` TLTVar "a"
                            familyEquationPatterns idEq `shouldBe` [TLPVar "a"]
                            familyEquationRhs idEq `shouldBe` TLTApp (TLTLam "x" TLKType (TLTVar "x")) (TLTVar "a")
                        other -> expectationFailure ("unexpected family equations: " ++ show other)
                other -> expectationFailure ("unexpected program shape: " ++ show other)
            parseRawProgram (prettyProgram program) `shouldBe` Right program

        it "normalizes checked type-family declarations before resolution" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family Id a :: * where {"
                        , "    Id a = a;"
                        , "  }"
                        , "  def main : Id Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "reduces nested checked type-family declarations before resolution" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family Id a :: * where {"
                        , "    Id a = a;"
                        , "  }"
                        , "  type family ToMain a :: * where {"
                        , "    ToMain Int = Id Bool;"
                        , "  }"
                        , "  def main : ToMain Int = true;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "rejects stuck checked type-family applications before resolution" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family OnlyBool a :: * where {"
                        , "    OnlyBool Bool = Int;"
                        , "  }"
                        , "  def main : OnlyBool Int = 1;"
                        , "}"
                        ]
            checkProgram program
                `shouldBe` Left (ProgramTypeFamilyReductionFailed "OnlyBool" (TypeFamilyStuck "OnlyBool" [TLTCon "Int"]))

        it "rejects cyclic checked type-family applications before resolution" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family LoopA a :: * where {"
                        , "    LoopA a = LoopB a;"
                        , "  }"
                        , "  type family LoopB a :: * where {"
                        , "    LoopB a = LoopA a;"
                        , "  }"
                        , "  def main : LoopA Int = 1;"
                        , "}"
                        ]
            case checkProgram program of
                Left (ProgramTypeFamilyReductionFailed "LoopA" (TypeFamilyCycle familyCycle)) ->
                    familyCycle `shouldSatisfy` (not . null)
                other -> expectationFailure ("expected type-family cycle, got: " ++ show other)

        it "beta-reduces checked source type-lambda applications before resolution" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : (Λa. a) Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "beta-reduces nested source type-lambda applications in constructor arguments" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , "  def main : Box ((Λa. a) Int) = Box 1;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "beta-reduces source type-lambda family arguments before erasure" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family Apply (f :: * -> *) a :: * where {"
                        , "    Apply f a = f a;"
                        , "  }"
                        , "  def main : Apply (Λx. x) Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "beta-reduces source type-lambda bodies before reducing closed families" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family OnlyInt a :: * where {"
                        , "    OnlyInt Int = Bool;"
                        , "  }"
                        , "  def main : (Λa. OnlyInt a) Int = true;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "kind-checks kind-polymorphic type-family declarations before erasure" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family IdK (a :: κ) :: κ where {"
                        , "    IdK a = a;"
                        , "  }"
                        , "  def main : IdK Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "rejects type-family equations with the wrong pattern arity" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family Id a :: * where {"
                        , "    Id a b = a;"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldBe` Left (ProgramTypeFamilyEquationArityMismatch "Id" 1 2)

        it "rejects type-family RHS kinds that do not match the result kind" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family Bad (f :: * -> *) :: * where {"
                        , "    Bad f = f;"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program
                `shouldBe` Left (ProgramTypeFamilyKindMismatch "Bad" (TLTVar "f") TLKType (TLKArrow TLKType TLKType))

        it "rejects type-family RHS variables not bound by the equation pattern" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  type family Bad a :: * where {"
                        , "    Bad Int = a;"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldBe` Left (ProgramUnboundTypeFamilyVariable "a")

        it "does not reduce closed families that are not imported" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Hidden export (HiddenId) {"
                        , "  type family HiddenId a :: * where {"
                        , "    HiddenId a = a;"
                        , "  }"
                        , "}"
                        , "module Main export (main) {"
                        , "  def main : HiddenId Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldBe` Left (ProgramUnknownType "HiddenId")

        it "normalizes imported closed families and erases their import/export surface" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Families export (Id) {"
                        , "  type family Id a :: * where {"
                        , "    Id a = a;"
                        , "  }"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Families exposing (Id);"
                        , "  def main : Id Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "rejects residual source type lambdas before resolution" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Λa. a = 1;"
                        , "}"
                        ]
            checkProgram program `shouldBe` Left (ProgramResidualTypeLambda (STTyLam "a" (STVar "a")))

        it "checks zero-method multi-parameter class constraints and instances" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Rel a b {"
                        , "  }"
                        , "  instance Rel Int Bool {"
                        , "  }"
                        , "  def needsRel : Rel Int Bool => Int = 1;"
                        , "  def main : Int = needsRel;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "checks method-bearing multi-parameter class instances when method use fixes every class argument" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Convert a b {"
                        , "    convert : a -> b;"
                        , "  }"
                        , "  instance Convert Int Bool {"
                        , "    convert = λ(_x : Int) true;"
                        , "  }"
                        , "  def main : Bool = convert 1;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "rejects ambiguous method-bearing multi-parameter method use" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Convert a b {"
                        , "    convert : a -> b;"
                        , "  }"
                        , "  instance Convert Int Bool {"
                        , "    convert = λ(_x : Int) true;"
                        , "  }"
                        , "  def main : Int -> Bool = convert;"
                        , "}"
                        ]
            checkProgram program `shouldBe` Left (ProgramAmbiguousMethodUse "convert")

        it "uses superclass constraints as flattened evidence prerequisites" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  class Eq a => Ord a {"
                        , "  }"
                        , "  instance Eq Int {"
                        , "    eq = λ(_x : Int) λ(_y : Int) true;"
                        , "  }"
                        , "  instance Ord Int {"
                        , "  }"
                        , "  def needsOrd : Ord Int => Bool = eq 1 1;"
                        , "  def main : Bool = needsOrd;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "rejects missing superclass instance prerequisites" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  class Eq a => Ord a {"
                        , "  }"
                        , "  instance Ord Int {"
                        , "  }"
                        , "  def needsOrd : Ord Int => Bool = true;"
                        , "  def main : Bool = needsOrd;"
                        , "}"
                        ]
            checkProgram program `shouldBe` Left (ProgramNoMatchingInstance "Eq" (STBase "Int"))

        it "uses functional dependencies to close multi-parameter method dispatch" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  data Box ="
                        , "      Box : Int -> Box;"
                        , "  class Collect a b | a → b {"
                        , "    collect : a -> b;"
                        , "  }"
                        , "  instance Collect Box Int {"
                        , "    collect = λ(box : Box) case box of { Box value -> value };"
                        , "  }"
                        , "  def main : Int = let value = collect (Box 1) in value;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "rejects functional dependencies over non-class parameters" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Bad a | a → b {"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldBe` Left (ProgramInvalidFunctionalDependency "Bad" "b")

        it "rejects ambiguous functional-dependency instances" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Collect a b | a → b {"
                        , "  }"
                        , "  instance Collect a b {"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldBe` Left (ProgramAmbiguousFunctionalDependencyInstance "Collect" [STVar "a", STVar "b"])

        it "rejects conflicting functional-dependency instances before generic overlap" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Collect a b | a → b {"
                        , "  }"
                        , "  instance Collect Int Bool {"
                        , "  }"
                        , "  instance Collect Int String {"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program
                `shouldBe` Left (ProgramConflictingFunctionalDependency "Collect" [STBase "Int"] [STBase "Bool"] [STBase "String"])

        it "rejects class arity mismatches with structured diagnostics" $ do
            constraintProgram <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Rel a b {"
                        , "  }"
                        , "  def main : Rel Int => Int = 1;"
                        , "}"
                        ]
            checkProgram constraintProgram
                `shouldBe` Left (ProgramClassArityMismatch "Rel" 2 1)

            instanceProgram <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Rel a b {"
                        , "  }"
                        , "  instance Rel Int {"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram instanceProgram
                `shouldBe` Left (ProgramClassArityMismatch "Rel" 2 1)

        it "rejects duplicate multi-parameter instances with structured diagnostics" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Rel a b {"
                        , "  }"
                        , "  instance Rel Int Bool {"
                        , "  }"
                        , "  instance Rel Int Bool {"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program
                `shouldBe` Left (ProgramDuplicateInstanceHead "Rel" [STBase "Int", STBase "Bool"])

        it "rejects overlapping multi-parameter instances with structured diagnostics" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  class Rel a b {"
                        , "  }"
                        , "  instance Rel a Bool {"
                        , "  }"
                        , "  instance Rel Int b {"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program
                `shouldBe` Left (ProgramOverlappingInstanceHead "Rel" [STVar "a", STBase "Bool"] [STBase "Int", STVar "b"])

        it "parses and pretty-prints variable-headed higher-kinded field types" $ do
            let programText =
                    unlines
                        [ "module Main export (Functor, Higher(..)) {"
                        , "  class Functor (f :: * -> *) {"
                        , "    map : ∀ a b. (a -> b) -> f a -> f b;"
                        , "  }"
                        , ""
                        , "  data Higher (f :: * -> *) a ="
                        , "      Higher : f a -> Higher f a;"
                        , "}"
                        ]
                expectedMethodTy =
                    STForall
                        "a"
                        Nothing
                        ( STForall
                            "b"
                            Nothing
                            ( STArrow
                                (STArrow (STVar "a") (STVar "b"))
                                ( STArrow
                                    (STVarApp "f" (STVar "a" :| []))
                                    (STVarApp "f" (STVar "b" :| []))
                                )
                            )
                        )
                expectedCtorTy =
                    STArrow
                        (STVarApp "f" (STVar "a" :| []))
                        (STCon "Higher" (STVar "f" :| [STVar "a"]))
            program <- requireParsed programText
            case program of
                Program [Module {moduleDecls = [DeclClass classDecl, DeclData dataDecl]}] -> do
                    case classDeclMethods classDecl of
                        [MethodSig {methodSigType = ConstrainedType [] methodTy}] ->
                            methodTy `shouldBe` expectedMethodTy
                        other -> expectationFailure ("unexpected method shape: " ++ show other)
                    case dataDeclConstructors dataDecl of
                        [ConstructorDecl {constructorDeclType = ctorTy}] ->
                            ctorTy `shouldBe` expectedCtorTy
                        other -> expectationFailure ("unexpected constructor shape: " ++ show other)
                other -> expectationFailure ("unexpected program shape: " ++ show other)
            parseRawProgram (prettyProgram program) `shouldBe` Right program

        it "rejects retired ASCII token aliases on the program surface" $ do
            let rejectedPrograms =
                    [ unlines
                        [ "module Main export (main) {"
                        , "  def main : forall a. a -> a = λx x;"
                        , "}"
                        ]
                    , unlines
                        [ "module Main export (main) {"
                        , "  def main : ∀(a >= Int). a -> a = λx x;"
                        , "}"
                        ]
                    , unlines
                        [ "module Main export (main) {"
                        , "  def main : Int = \\x x;"
                        , "}"
                        ]
                    , unlines
                        [ "module Main export (main) {"
                        , "  def main : mu a. a = 1;"
                        , "}"
                        ]
                    , unlines
                        [ "module Main export (main) {"
                        , "  def main : bottom = 1;"
                        , "}"
                        ]
                    , unlines
                        [ "module Main export (main) {"
                        , "  def main : _|_ = 1;"
                        , "}"
                        ]
                    , unlines
                        [ "module Main {"
                        , "  type family Id a :: * where {"
                        , "    Id a = \\x. x;"
                        , "  }"
                        , "}"
                        ]
                    , unlines
                        [ "module Main {"
                        , "  class C a b | a -> b {"
                        , "  }"
                        , "}"
                        ]
                    ]
            mapM_ (`shouldSatisfy` isLeft) (map parseRawProgram rejectedPrograms)

        it "rejects program ∀ binders without the required dot" $
            mapM_
                ( \ty ->
                    parseRawProgram
                        ( unlines
                            [ "module Main export (main) {"
                            , "  def main : " ++ ty ++ " = 1;"
                            , "}"
                            ]
                        )
                        `shouldSatisfy` isLeft
                )
                ["∀ a Int", "∀a Int"]

    describe "MLF.Program shared runtime-success parity surface" $ do
        mapM_ runProgramRuntimeCase programRuntimeSuccessCases

    describe "MLF.Program CLI helper" $ do
        it "runs a frozen sample file by path" $ do
            runProgramFile "test/programs/recursive-adt/plain-recursive-nat.mlfp"
                `shouldReturn` Right "true\n"

        it "prepends the built-in Prelude for explicit imports" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Nat(..), Option(..));"
                        , "  def main : Option Nat = Some Zero;"
                        , "}"
                        ]
            (prettyValue <$> runLocatedProgram (withPreludeLocated located)) `shouldBe` Right "Some Zero"

        it "typechecks the Prelude IO class hierarchy" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (pureUnit, main) {"
                        , "  import Prelude exposing (Unit(..), IO, Functor, Applicative, Monad, pure, bind, putStrLn);"
                        , "  def pureUnit : IO Unit = pure Unit;"
                        , "  def after : Unit -> IO Unit = λ_done putStrLn \"world\";"
                        , "  def main : IO Unit = bind (putStrLn \"hello\") after;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` isRight

        it "runs Prelude Functor and Applicative IO instances through class methods" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, Functor, Applicative, Monad, map, pure, ap, bind, putStrLn);"
                        , "  def action : IO Int = pure 1;"
                        , "  def mapped : IO Unit = map (λ(_n : Int) Unit) action;"
                        , "  def wrappedFunction : IO (Int -> Unit) = pure (λ(_n : Int) Unit);"
                        , "  def applied : IO Unit = ap wrappedFunction action;"
                        , "  def main : IO Unit = bind mapped (λ(_done : Unit) bind applied (λ(_done2 : Unit) putStrLn \"hierarchy\"));"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "hierarchy\n"

        it "typechecks direct IO bind primitive uses with consistent arguments" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_bind (__io_pure Unit) (λ(_n : Unit) __io_putStrLn \"world\");"
                        , "}"
                        ]
            checked <-
                case checkLocatedProgram (withPreludeLocated located) of
                    Left err -> expectationFailure ("check failed: " ++ show err) >> fail "check failed"
                    Right checked -> pure checked
            mainBinding <- requireCheckedBinding "Main__main" checked
            unresolvedTermVarRefs (checkedBindingTerm mainBinding) `shouldBe` []

        it "rejects inconsistent direct IO bind primitive arguments" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_bind (__io_pure 1) (λ(_n : Unit) __io_putStrLn \"world\");"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` isLeft

        it "rejects non-IO expressions for Prelude IO annotations" $ do
            intLocated <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = 1;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated intLocated) `shouldSatisfy` either
                (isInfixOf "type mismatch" . renderProgramDiagnostic)
                (const False)
            identityLocated <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = λx x;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated identityLocated) `shouldSatisfy` either
                (isInfixOf "type mismatch" . renderProgramDiagnostic)
                (const False)

        it "rejects monomorphic IO actions for polymorphic IO annotations" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (IO);"
                        , "  def main : ∀ a. IO a = __io_pure 1;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                (isInfixOf "type mismatch" . renderProgramDiagnostic)
                (const False)

        it "rejects inconsistent Prelude IO bind argument substitutions" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, putStrLn);"
                        , "  def main : IO Unit = bind (__io_pure 1) (λ(_n : Unit) putStrLn \"world\");"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                (isInfixOf "ambiguous overloaded method use `bind`" . renderProgramDiagnostic)
                (const False)

        it "rejects constructor imports for opaque Prelude IO" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (IO(..));"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ((== ProgramImportNotExported "Prelude" "IO") . diagnosticError)
                (const False)

        it "rejects case inspection of opaque Prelude IO values" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, pure);"
                        , "  def action : IO Unit = pure Unit;"
                        , "  def main : Unit = case action of {"
                        , "    _ -> Unit"
                        , "  };"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                (isInfixOf "case scrutinee is not a data type" . renderProgramDiagnostic)
                (const False)

        it "keeps overloaded pure ambiguous without an IO expected result" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), pure);"
                        , "  def main : Unit = pure Unit;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ((== ProgramAmbiguousMethodUse "pure") . diagnosticError)
                (const False)

        it "executes putStrLn for main : IO Unit" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
                        , "  def main : IO Unit = putStrLn \"hello\";"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "hello\n"

        it "sequences main IO through Prelude bind" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, putStrLn);"
                        , "  def after : Unit -> IO Unit = λ_done putStrLn \"world\";"
                        , "  def main : IO Unit = bind (putStrLn \"hello\") after;"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "hello\nworld\n"

        it "sequences direct IO primitives without rendering Unit" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_bind (__io_pure Unit) (λ(_n : Unit) __io_putStrLn \"world\");"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "world\n"

        it "runs pure IO Unit actions without stdout or Unit rendering" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, pure);"
                        , "  def main : IO Unit = pure Unit;"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right ""

        it "supports qualified non-Prelude Unit results in IO main" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module A export (Unit(..)) {"
                        , "  data Unit ="
                        , "      Unit : Unit;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import A as A;"
                        , "  def main : IO A.Unit = __io_pure A.Unit;"
                        , "}"
                        ]
            runLocatedProgramOutput located `shouldSatisfy` isRight

        it "resolves deferred non-Unit constructors passed through IO bind" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, Nat(..), bind, pure, putStrLn);"
                        , "  def after : Nat -> IO Unit = λ_n putStrLn \"nat\";"
                        , "  def action : IO Nat = pure Zero;"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            mainBinding <- requireCheckedBinding "Main__main" checked
            unresolvedTermVarRefs (checkedBindingTerm mainBinding) `shouldBe` []
            (programRunOutput <$> runCheckedProgramOutput checked) `shouldBe` Right "nat\n"

        it "seeds local binders after deferred identities in the same checked binding" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, Nat(..));"
                        , "  def main : IO Unit = __io_bind (__io_pure Zero) (λ(n : Nat) __io_putStrLn \"nat\");"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            mainBinding <- requireCheckedBinding "Main__main" checked
            resolvedLocalBinders (checkedBindingTerm mainBinding) `shouldSatisfy` any isGeneratedLocalRef
            generatedLocalIdentityValues (checkedBindingTerm mainBinding)
                `shouldSatisfy` all (`notElem` generatedDeferredIdentityValues mainBinding)
            (programRunOutput <$> runCheckedProgramOutput checked) `shouldBe` Right "nat\n"

        it "refreshes duplicate deferred refs after lowered binding identities" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let duplicateRef = deferredRefFromIdentity (UniqueIdentity 0) "$deferred"
                firstIdentity = generatedSymbolIdentity 1 SymbolValue "Main" "first" Nothing
                secondIdentity = generatedSymbolIdentity 2 SymbolValue "Main" "second" Nothing
                dataIdentity = generatedSymbolIdentity 3 SymbolType "Main" "Phantom" Nothing
                phantomData =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                obligation =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseRef = duplicateRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeType = STBase "Int"
                            , deferredCaseResultType = STBase "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered name identity value =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails name (TopLevelId identity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.ELit (LInt value)
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations = Map.singleton duplicateRef obligation
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            checked <-
                case finalizeBindingsAllowOpaqueWithContext finalizeContext
                    [ lowered "Main__first" firstIdentity 1
                    , lowered "Main__second" secondIdentity 2
                    ] of
                    Right bindings -> pure bindings
                    Left err -> expectationFailure ("finalize group failed: " ++ show err) >> fail "finalize group failed"
            let bindingIdentities = map symbolUniqueIdentity [firstIdentity, secondIdentity, dataIdentity]
            concatMap generatedDeferredIdentityValues checked
                `shouldSatisfy` all (`notElem` bindingIdentities)

        it "refreshes duplicate deferred refs after lowered local identities" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let duplicateRef = deferredRefFromIdentity (UniqueIdentity 0) "$deferred"
                firstIdentity = generatedSymbolIdentity 1 SymbolValue "Main" "first" Nothing
                secondIdentity = generatedSymbolIdentity 2 SymbolValue "Main" "second" Nothing
                dataIdentity = generatedSymbolIdentity 3 SymbolType "Main" "Phantom" Nothing
                occupiedLocal = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 4)) "x"
                phantomData =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                obligation =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseRef = duplicateRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeType = STBase "Int"
                            , deferredCaseResultType = STBase "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered name identity value locals =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails name (TopLevelId identity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.ELit (LInt value)
                        , loweredBindingResolvedLocalIdentities = locals
                        , loweredBindingDeferredObligations = Map.singleton duplicateRef obligation
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            checked <-
                case finalizeBindingsAllowOpaqueWithContext finalizeContext
                    [ lowered
                        "Main__first"
                        firstIdentity
                        1
                        [ProgramTypes.LoweredResolvedLocalIdentity occupiedLocal occupiedLocal]
                    , lowered "Main__second" secondIdentity 2 []
                    ] of
                    Right bindings -> pure bindings
                    Left err -> expectationFailure ("finalize group failed: " ++ show err) >> fail "finalize group failed"
            concatMap generatedDeferredIdentityValues checked
                `shouldSatisfy` all (/= UniqueIdentity 4)

        it "rejects duplicate lowered binding identities before caching read contexts" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let duplicateIdentity = generatedSymbolIdentity 7 SymbolValue "Main" "dup" Nothing
                lowered name value =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails name (TopLevelId duplicateIdentity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.ELit (LInt value)
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            case mkModuleFinalizeContext finalizeContext [lowered "Main__first" 1, lowered "Main__second" 2] of
                Left (ProgramPipelineError message) ->
                    message `shouldSatisfy` isInfixOf "duplicate binding identities"
                Left err ->
                    expectationFailure ("expected duplicate binding identity rejection, got " ++ show err)
                Right _ ->
                    expectationFailure "expected duplicate binding identity rejection"

        it "rejects mismatched deferred obligation identities before checked binding storage" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let keyRef = deferredRefFromIdentity (UniqueIdentity 30) "$deferred"
                payloadRef = deferredRefFromIdentity (UniqueIdentity 31) "$deferred"
                bindingIdentity = generatedSymbolIdentity 32 SymbolValue "Main" "main" Nothing
                dataIdentity = generatedSymbolIdentity 33 SymbolType "Main" "Phantom" Nothing
                phantomData =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                obligation =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseRef = payloadRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeType = STBase "Int"
                            , deferredCaseResultType = STBase "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.EVar "$deferred"
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations = Map.singleton keyRef obligation
                        , loweredBindingExternalTypeViews =
                            Map.singleton "$deferred" (ProgramTypes.mkTypeView (STBase "Int") (STBase "Int"))
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            case finalizeBindingWithContext finalizeContext lowered of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "mismatched deferred obligation identity"
                    message `shouldSatisfy` isInfixOf "Main__main"
                Left err ->
                    expectationFailure ("expected mismatched deferred identity rejection, got " ++ show err)
                Right _ ->
                    expectationFailure "expected mismatched deferred identity rejection"

        it "rejects mismatched deferred obligation identities before grouped binding storage" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let keyRef = deferredRefFromIdentity (UniqueIdentity 34) "$deferred"
                payloadRef = deferredRefFromIdentity (UniqueIdentity 35) "$deferred"
                firstIdentity = generatedSymbolIdentity 36 SymbolValue "Main" "first" Nothing
                secondIdentity = generatedSymbolIdentity 37 SymbolValue "Main" "second" Nothing
                dataIdentity = generatedSymbolIdentity 38 SymbolType "Main" "Phantom" Nothing
                phantomData =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                obligation =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseRef = payloadRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeType = STBase "Int"
                            , deferredCaseResultType = STBase "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                firstLowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails "Main__first" (TopLevelId firstIdentity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.EVar "$deferred"
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations = Map.singleton keyRef obligation
                        , loweredBindingExternalTypeViews =
                            Map.singleton "$deferred" (ProgramTypes.mkTypeView (STBase "Int") (STBase "Int"))
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
                secondLowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails "Main__second" (TopLevelId secondIdentity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.ELit (LInt 1)
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            case finalizeBindingsAllowOpaqueWithContext finalizeContext [firstLowered, secondLowered] of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "mismatched deferred obligation identity"
                    message `shouldSatisfy` isInfixOf "Main__first"
                Left err ->
                    expectationFailure ("expected grouped mismatched deferred identity rejection, got " ++ show err)
                Right _ ->
                    expectationFailure "expected grouped mismatched deferred identity rejection"

        it "rejects mismatched deferred obligation identities before module context cache lookup" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let keyRef = deferredRefFromIdentity (UniqueIdentity 39) "$deferred"
                payloadRef = deferredRefFromIdentity (UniqueIdentity 40) "$deferred"
                bindingIdentity = generatedSymbolIdentity 41 SymbolValue "Main" "main" Nothing
                dataIdentity = generatedSymbolIdentity 42 SymbolType "Main" "Phantom" Nothing
                phantomData =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                obligation =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseRef = payloadRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeType = STBase "Int"
                            , deferredCaseResultType = STBase "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered surfaceExpr obligations externalTypeViews =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = surfaceExpr
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations = obligations
                        , loweredBindingExternalTypeViews = externalTypeViews
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
                cachedLowered = lowered (Surface.ELit (LInt 1)) Map.empty Map.empty
                staleLowered =
                    lowered
                        (Surface.EVar "$deferred")
                        (Map.singleton keyRef obligation)
                        (Map.singleton "$deferred" (ProgramTypes.mkTypeView (STBase "Int") (STBase "Int")))
            moduleContext <-
                case mkModuleFinalizeContext finalizeContext [cachedLowered] of
                    Right value -> pure value
                    Left err -> expectationFailure ("module finalize context failed: " ++ show err) >> fail "module finalize context failed"
            case finalizeBindingAllowOpaqueWithModuleContext moduleContext staleLowered of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "mismatched deferred obligation identity"
                    message `shouldSatisfy` isInfixOf "Main__main"
                Left err ->
                    expectationFailure ("expected module-context mismatched deferred identity rejection, got " ++ show err)
                Right _ ->
                    expectationFailure "expected module-context mismatched deferred identity rejection"

        it "does not resolve same-named deferred external bindings by an arbitrary identity" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let placeholder = "$deferred"
                firstRef = deferredRefFromIdentity (UniqueIdentity 20) placeholder
                secondRef = deferredRefFromIdentity (UniqueIdentity 21) placeholder
                bindingIdentity = generatedSymbolIdentity 22 SymbolValue "Main" "main" Nothing
                dataIdentity = generatedSymbolIdentity 23 SymbolType "Main" "Phantom" Nothing
                phantomData =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                obligation ref =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseRef = ref
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeType = STBase "Int"
                            , deferredCaseResultType = STBase "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            ProgramTypes.loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceType = STBase "Int"
                        , loweredBindingSourceTypeView = Nothing
                        , loweredBindingExpectedType = STBase "Int"
                        , loweredBindingExpectedTypeView = Nothing
                        , loweredBindingSurfaceExpr = Surface.EVar placeholder
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingDeferredObligations =
                            Map.fromList [(firstRef, obligation firstRef), (secondRef, obligation secondRef)]
                        , loweredBindingExternalTypeViews =
                            Map.singleton placeholder (ProgramTypes.mkTypeView (STBase "Int") (STBase "Int"))
                        , loweredBindingEvidenceParamCount = 0
                        , loweredBindingExportedAsMain = False
                        }
            checked <-
                case finalizeBindingsAllowOpaqueWithContext finalizeContext [lowered] of
                    Right [binding] -> pure binding
                    Right other -> expectationFailure ("expected one checked binding, got: " ++ show (length other)) >> fail "finalize binding failed"
                    Left err -> expectationFailure ("finalize binding failed: " ++ show err) >> fail "finalize binding failed"
            unresolvedTermVarRefs (checkedBindingTerm checked) `shouldBe` []

        it "resolves deferred non-nullary constructors named Unit as functions" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (Foo(..), main) {"
                        , "  import Prelude as P exposing (Unit, IO, bind, pure, putStrLn);"
                        , "  data Foo ="
                        , "      Unit : Int -> Foo;"
                        , "  def after : Foo -> IO P.Unit = λ_foo putStrLn \"foo\";"
                        , "  def action : IO Foo = pure (Unit 1);"
                        , "  def main : IO P.Unit = bind action after;"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "foo\n"

        it "keeps deferred constructor lookup scoped to the binding that created the placeholder" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module A export (Foo, mkAction) {"
                        , "  import Prelude exposing (IO, pure);"
                        , "  data Foo ="
                        , "      Unit : Int -> Foo;"
                        , "  def mkAction : Int -> IO Foo = λn pure (Unit n);"
                        , "}"
                        , ""
                        , "module B export (Bar(..), unused) {"
                        , "  data Bar ="
                        , "      Unit : Bar;"
                        , "  def unused : Bar = Unit;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, putStrLn);"
                        , "  import A exposing (Foo, mkAction);"
                        , "  def action : IO Foo = mkAction 1;"
                        , "  def after : Foo -> IO Unit = λ_foo putStrLn \"scoped\";"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "scoped\n"

        it "resolves deferred case placeholders inside IO continuations" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, Nat(..), bind, pure, putStrLn);"
                        , "  def after : Nat -> IO Unit = λn case n of {"
                        , "    Zero -> putStrLn \"zero\";"
                        , "    Succ rest -> putStrLn \"succ\""
                        , "  };"
                        , "  def action : IO Nat = pure (Succ Zero);"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            mainBinding <- requireCheckedBinding "Main__main" checked
            unresolvedTermVarRefs (checkedBindingTerm mainBinding) `shouldBe` []
            (programRunOutput <$> runCheckedProgramOutput checked) `shouldBe` Right "succ\n"

        it "resolves deferred method placeholders inside IO continuations" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, Nat(..), bind, pure, putStrLn);"
                        , "  class Speak a {"
                        , "    speak : a -> IO Unit;"
                        , "  }"
                        , "  instance Speak Nat {"
                        , "    speak = λn case n of {"
                        , "      Zero -> putStrLn \"zero\";"
                        , "      Succ rest -> putStrLn \"succ\""
                        , "    };"
                        , "  }"
                        , "  def after : Nat -> IO Unit = λn speak n;"
                        , "  def action : IO Nat = pure (Succ Zero);"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            mainBinding <- requireCheckedBinding "Main__main" checked
            unresolvedTermVarRefs (checkedBindingTerm mainBinding) `shouldBe` []
            (programRunOutput <$> runCheckedProgramOutput checked) `shouldBe` Right "succ\n"

        it "dispatches deferred IO methods by resolved identity instead of checked binding name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, Nat(..), bind, pure, putStrLn);"
                        , "  class Speak a {"
                        , "    speak : a -> IO Unit;"
                        , "  }"
                        , "  instance Speak Nat {"
                        , "    speak = λn case n of {"
                        , "      Zero -> putStrLn \"zero\";"
                        , "      Succ rest -> putStrLn \"succ\""
                        , "    };"
                        , "  }"
                        , "  def after : Nat -> IO Unit = λn speak n;"
                        , "  def action : IO Nat = pure (Succ Zero);"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' =
                    renameInstanceMethodRuntimeNamesWhere
                        (\name -> "speak" `isInfixOf` name)
                        "$stale_speak_runtime_name"
                        checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "succ\n"

        it "preserves parameterized constructor instantiations for IO method dispatch" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, pure, putStrLn);"
                        , "  data Wrap a ="
                        , "      Wrap : a -> Wrap a;"
                        , "  class Speak a {"
                        , "    speak : a -> IO Unit;"
                        , "  }"
                        , "  instance Speak (Wrap Int) {"
                        , "    speak = λw putStrLn \"int\";"
                        , "  }"
                        , "  instance Speak (Wrap Bool) {"
                        , "    speak = λw putStrLn \"bool\";"
                        , "  }"
                        , "  def after : Wrap Int -> IO Unit = λw speak w;"
                        , "  def action : IO (Wrap Int) = pure (Wrap 1);"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "int\n"

        it "resolves constrained instance evidence for IO method dispatch" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, Nat(..), Option(..), Eq, bind, eq, pure, putStrLn);"
                        , "  def after : Option Nat -> IO Unit = λopt (λ(same : Bool) putStrLn \"eq\") (eq opt opt);"
                        , "  def action : IO (Option Nat) = pure (Some Zero);"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "eq\n"

        it "applies local evidence arguments for constrained nullary IO methods" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (Eq, Token(..), Pair(..), Pick, eq, pick, selected, main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, pure, putStrLn);"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  instance Eq Bool {"
                        , "    eq = λleft λright true;"
                        , "  }"
                        , "  data Token a ="
                        , "      Token : Token a;"
                        , "  data Pair a b ="
                        , "      Pair : Token b -> Pair a b;"
                        , "  class Pick a {"
                        , "    pick : Eq b => Pair a b;"
                        , "  }"
                        , "  instance Pick Bool {"
                        , "    pick = Pair Token;"
                        , "  }"
                        , "  def selected : (Pick Bool, Eq Bool) => Pair Bool Bool = pick;"
                        , "  def after : Pair Bool Bool -> IO Unit = λpair case pair of {"
                        , "    Pair _ -> putStrLn \"picked\""
                        , "  };"
                        , "  def action : IO (Pair Bool Bool) = pure selected;"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "picked\n"

        it "looks up local deferred evidence by resolved identity instead of evidence runtime name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (Eq, Token(..), Pair(..), Pick, eq, pick, selected, main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, pure, putStrLn);"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  instance Eq Bool {"
                        , "    eq = λleft λright true;"
                        , "  }"
                        , "  data Token a ="
                        , "      Token : Token a;"
                        , "  data Pair a b ="
                        , "      Pair : Token b -> Pair a b;"
                        , "  class Pick a {"
                        , "    pick : Eq b => Pair a b;"
                        , "  }"
                        , "  instance Pick Bool {"
                        , "    pick = Pair Token;"
                        , "  }"
                        , "  def selected : (Pick Bool, Eq Bool) => Pair Bool Bool = pick;"
                        , "  def after : Pair Bool Bool -> IO Unit = λpair case pair of {"
                        , "    Pair _ -> putStrLn \"picked\""
                        , "  };"
                        , "  def action : IO (Pair Bool Bool) = pure selected;"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            checkedProgramDeferredEvidenceMethods checked
                `shouldSatisfy` any (isJust . evidenceMethodResolvedVar)
            let checked' = poisonResolvedDeferredEvidenceRuntimeNames "$stale_evidence_runtime_name" checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "picked\n"

        it "constructs IO ADT payloads with function fields without runtime type inference" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, pure, putStrLn);"
                        , "  data Box ="
                        , "      Box : (Int -> Int) -> Box;"
                        , "  def idFn : Int -> Int = λ(n : Int) n;"
                        , "  def after : Box -> IO Unit = λ_box putStrLn \"boxed\";"
                        , "  def action : IO Box = pure (Box idFn);"
                        , "  def main : IO Unit = bind action after;"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "boxed\n"

        it "rejects recursive IO main lookup without hanging" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = main;"
                        , "}"
                        ]
            runLocatedProgramOutput (withPreludeLocated located) `shouldSatisfy` either
                ( \diagnostic ->
                    case diagnosticError diagnostic of
                        ProgramPipelineError msg ->
                            all
                                (`isInfixOf` msg)
                                [ "recursive top-level binding lookup"
                                , "Main__main -> Main__main"
                                ]
                        _ -> False
                )
                (const False)

        it "allows delayed top-level recursion through lambda closures" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, Nat(..), putStrLn);"
                        , "  def finish : Nat -> IO Unit = λ(n : Nat) case n of {"
                        , "    Zero -> putStrLn \"done\";"
                        , "    Succ _ -> finish Zero"
                        , "  };"
                        , "  def main : IO Unit = finish (Succ Zero);"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutput (withPreludeLocated located)) `shouldBe` Right "done\n"

        it "supports IO mains whose result type is not Unit" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (IO, pure);"
                        , "  def main : IO Int = pure 1;"
                        , "}"
                        ]
            let result = runLocatedProgramOutput (withPreludeLocated located)
            result `shouldSatisfy` isRight
            case result of
                Right runResult -> programRunValue runResult `shouldBe` Just (VLit (LInt 1))
                Left _ -> expectationFailure "expected Right"

        it "rejects running pure mains that depend on opaque Prelude helpers" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, pure);"
                        , "  def discard : IO Unit -> Unit = λ(_action : IO Unit) Unit;"
                        , "  def main : Unit = discard (pure Unit);"
                        , "}"
                        ]
            runLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ( \diagnostic ->
                    all
                        (`isInfixOf` renderProgramDiagnostic diagnostic)
                        [ "run-program does not support IO dependencies yet"
                        , "Main__discard"
                        ]
                )
                (const False)

        it "rejects opaque helper dependencies by checked source type identity" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, pure);"
                        , "  def discard : IO Unit -> Unit = λ(_action : IO Unit) Unit;"
                        , "  def main : Unit = discard (pure Unit);"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            let checked' =
                    replaceCheckedBindingSourceType
                        "Main__discard"
                        (STBase "Unit")
                        checked
            runCheckedProgramOutput checked' `shouldSatisfy` either
                ( \err ->
                    "run-program does not support IO dependencies yet" `isInfixOf` show err
                        && "Main__discard" `isInfixOf` show err
                )
                (const False)

        it "rejects running pure mains that directly call opaque primitives" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : Unit = (λ(_action : IO Unit) Unit) (__io_pure Unit);"
                        , "}"
                        ]
            runLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ( \diagnostic ->
                    all
                        (`isInfixOf` renderProgramDiagnostic diagnostic)
                        [ "run-program does not support IO dependencies yet"
                        , "__io_pure"
                        ]
                )
                (const False)

        it "rejects opaque primitive dependencies by resolved identity instead of runtime name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : Unit = (λ(_action : IO Unit) Unit) (__io_pure Unit);"
                        , "}"
                        ]
            checked <- requireCheckedLocated (withPreludeLocated located)
            mainBinding <- requireCheckedBinding "Main__main" checked
            let checked' =
                    replaceCheckedBindingTerm
                        "Main__main"
                        (poisonPrimitiveRuntimeNames "$stale_io_pure" (checkedBindingTerm mainBinding))
                        checked
            case runCheckedProgramOutput checked' of
                Left (ProgramPipelineError text) -> do
                    text `shouldSatisfy` isInfixOf "run-program does not support IO dependencies yet"
                    text `shouldSatisfy` isInfixOf "__io_pure"
                    text `shouldNotSatisfy` isInfixOf "$stale_io_pure"
                other ->
                    expectationFailure ("expected opaque primitive rejection, got " ++ show other)

        it "rejects a user module named Prelude when the built-in Prelude is active" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Prelude export () {"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            runLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ((== ProgramDuplicateModule "Prelude") . diagnosticError)
                (const False)

    describe "MLF.Program diagnostics" $ do
        it "reports variable-headed direct AST type mismatches as program errors" $ do
            let program =
                    Program
                        [ Module
                            { moduleName = "Main"
                            , moduleExports = Just [ExportValue "main"]
                            , moduleImports = []
                            , moduleDecls =
                                [ DeclDef
                                    DefDecl
                                        { defDeclName = "main"
                                        , defDeclType = ConstrainedType [] (STVarApp "f" (STBase "Int" :| []))
                                        , defDeclExpr = ELit (LInt 1)
                                        }
                                ]
                            }
                        ]
            checkProgram program `shouldBe` Left (ProgramTypeMismatch (STBase "Int") (STVarApp "f" (STBase "Int" :| [])))

        it "rejects duplicate data type parameter names" $ do
            let programText =
                    unlines
                        [ "module Main export (Bad(..)) {"
                        , "  data Bad a a ="
                        , "      Bad : Bad a a;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramDuplicateTypeParameter "a")

        it "checks nullary constructors from wide ADTs without leaking handler result polymorphism" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  data Many ="
                        , "      C01 : Many"
                        , "    | C02 : Many"
                        , "    | C03 : Many"
                        , "    | C04 : Many"
                        , "    | C05 : Many"
                        , "    | C06 : Many"
                        , "    | C07 : Many"
                        , "    | C08 : Many"
                        , "    | C09 : Many"
                        , "    | C10 : Many"
                        , "    | C11 : Many"
                        , "    | C12 : Many"
                        , "    | C13 : Many"
                        , "    | C14 : Many"
                        , "    | C15 : Many"
                        , "    | C16 : Many"
                        , "    | C17 : Many"
                        , "    | C18 : Many"
                        , "    | C19 : Many"
                        , "    | C20 : Many"
                        , "    | C21 : Many"
                        , "    | C22 : Many"
                        , "    | C23 : Many"
                        , "    | C24 : Many"
                        , "    | C25 : Many"
                        , "    | C26 : Many"
                        , "    | C27 : Many"
                        , "    | C28 : Many"
                        , "    | C29 : Many"
                        , "    | C30 : Many"
                        , "    | C31 : Many"
                        , "    | C32 : Many"
                        , "    | C33 : Many"
                        , "    | C34 : Many"
                        , "    | C35 : Many"
                        , "    | C36 : Many"
                        , "    | C37 : Many;"
                        , ""
                        , "  def main : Many = C01;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "checks parameterized constructor applications without leaking identity substitutions" $ do
            let programText =
                    unlines
                        [ "module Main export (Box(..), boxPure, main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def boxPure : ∀ a. a -> Box a ="
                        , "    λ(value : a) (Box value : Box a);"
                        , ""
                        , "  def main : Box String = boxPure \"ok\";"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "rejects importing constructors from an abstract type export" $ do
            let programText =
                    unlines
                        [ "module Hidden export (Nat) {"
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat;"
                        , "}"
                        , ""
                        , "module User export (main) {"
                        , "  import Hidden exposing (Nat(..));"
                        , "  def main : Nat = Zero;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramImportNotExported "Hidden" "Nat")

        it "rejects duplicate constructor branches even when a catch-all is present" $ do
            let programText =
                    unlines
                        [ "module DupCase export (Nat(..), main) {"
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat;"
                        , ""
                        , "  def main : Nat = case Zero of {"
                        , "    Zero -> Zero;"
                        , "    Zero -> Succ Zero;"
                        , "    _ -> Zero"
                        , "  };"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramDuplicateCaseBranch "Zero")

        it "rejects imports outside the same compilation unit" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  import ExternalCore;"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramUnknownImportModule "ExternalCore")

        it "rejects non-exhaustive case analysis for semantic reasons" $ do
            let programText =
                    unlines
                        [ "module Main export (Nat(..), main) {"
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat;"
                        , ""
                        , "  def main : Nat = case Succ Zero of {"
                        , "    Zero -> Zero"
                        , "  };"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramNonExhaustiveCase ["Succ"])

        it "preserves wildcard-only case scrutinee evaluation without a known source type" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Bool = case ((λx x) true) of {"
                        , "    _ -> true"
                        , "  };"
                        , "}"
                        ]
            program <- requireParsed programText
            case checkProgram program of
                Right checked ->
                    unlines
                        [ show (checkedBindingSurfaceExpr binding)
                        | checkedModule <- checkedProgramModules checked
                        , binding <- checkedModuleBindings checkedModule
                        , checkedBindingExportedAsMain binding
                        ]
                        `shouldSatisfy` isInfixOf "$case_scrutinee"
                Left err -> expectationFailure ("checkProgram failed: " ++ show err)

        it "does not treat a local value named id as identity in case scrutinees" $ do
            let programText =
                    unlines
                        [ "module Main export (B(..), main) {"
                        , "  data B ="
                        , "      BZ : B"
                        , "    | BO : B;"
                        , ""
                        , "  def main : B ="
                        , "    let id : B -> B = λx BO in"
                        , "    case (id BZ) of {"
                        , "      BZ -> BZ;"
                        , "      BO -> BO"
                        , "    };"
                        , "}"
                        ]
            program <- requireParsed programText
            (prettyValue <$> runProgram program) `shouldBe` Right "BO"

        it "rejects constructor arity mismatches as pattern errors" $ do
            let programText =
                    unlines
                        [ "module Main export (Nat(..), main) {"
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat;"
                        , ""
                        , "  def main : Nat = case Zero of {"
                        , "    Zero extra -> extra;"
                        , "    Succ inner -> inner"
                        , "  };"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramPatternConstructorMismatch "Zero" (STBase "Nat"))

        it "rejects constructor pattern fields passed where the wrapper type is expected" $ do
            let programText =
                    unlines
                        [ "module Main export (ParserValue(..), SourceSymbol(..), main) {"
                        , "  data SourceSymbol ="
                        , "      SourceSymbol : String -> SourceSymbol;"
                        , ""
                        , "  data ParserValue ="
                        , "      ValueToken : SourceSymbol -> ParserValue"
                        , "    | ValueUnit : ParserValue;"
                        , ""
                        , "  def renderValue : ParserValue -> String ="
                        , "    λ(value : ParserValue) case value of {"
                        , "      ValueToken _ -> \"token\";"
                        , "      ValueUnit -> \"unit\""
                        , "    };"
                        , ""
                        , "  def main : String ="
                        , "    case ValueToken (SourceSymbol \"identifier:a\") of {"
                        , "      ValueToken token -> renderValue token;"
                        , "      ValueUnit -> \"unit\""
                        , "    };"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramTypeMismatch (STBase "SourceSymbol") (STBase "ParserValue"))

        it "rejects missing instances instead of reviving route-specific diagnostics" $ do
            let programText =
                    unlines
                        [ "module Main export (Eq, Nat(..), eq, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat;"
                        , ""
                        , "  def main : Bool = eq Zero Zero;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramNoMatchingInstance "Eq" (STBase "Main.Nat"))

        it "rejects ordinary type mismatches directly" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Int = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramTypeMismatch (STBase "Bool") (STBase "Int"))

        it "rejects an unused constructor whose result is not its owning type" $ do
            let programText =
                    unlines
                        [ "module Main export (Nat, main) {"
                        , "  data Nat ="
                        , "      Bad : Bool;"
                        , ""
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramInvalidConstructorResult "Bad" (STBase "Bool") "Nat")

        it "rejects a parameterized constructor result with missing type arguments" $ do
            let programText =
                    unlines
                        [ "module Main export (Box, main) {"
                        , "  data Box a ="
                        , "      MkBox : Box;"
                        , ""
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramInvalidConstructorResult "MkBox" (STBase "Box") "Box")

        it "accepts GADT-style constructor results with the owning head and correct arity" $ do
            let programText =
                    unlines
                        [ "module Main export (Expr, main) {"
                        , "  data Expr a ="
                        , "      IntLit : Int -> Expr Int;"
                        , ""
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "accepts higher-kinded declarations when applications match declared kinds" $ do
            let programText =
                    unlines
                        [ "module Main export (Functor, Lifted, Higher(..), main) {"
                        , "  class Functor (f :: * -> *) {"
                        , "    map : ∀ a b. (a -> b) -> f a -> f b;"
                        , "  }"
                        , ""
                        , "  class Lifted (f :: * -> *) {"
                        , "    lift : Functor f => ∀ a. f a -> f a;"
                        , "  }"
                        , ""
                        , "  data Higher (f :: * -> *) a ="
                        , "      Higher : a -> Higher f a;"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "accepts the higher-kinded language-reference examples" $ do
            let programText =
                    unlines
                        [ "module Main export (Functor, Monad, Profunctor, Box(..), Wrap(..), WrappedP(..), MaybeF(..), main) {"
                        , "  class Functor (f :: * -> *) {"
                        , "    map : ∀ a b. (a -> b) -> f a -> f b;"
                        , "  }"
                        , ""
                        , "  class Monad (m :: * -> *) {"
                        , "    bind : ∀ a b. m a -> (a -> m b) -> m b;"
                        , "  }"
                        , ""
                        , "  class Profunctor (p :: * -> * -> *) {"
                        , "    dimap : ∀ a b c d. (a -> b) -> (c -> d) -> p b c -> p a d;"
                        , "  }"
                        , ""
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  data Wrap (f :: * -> *) a ="
                        , "      Wrap : f a -> Wrap f a;"
                        , ""
                        , "  data WrappedP (p :: * -> * -> *) a b ="
                        , "      WrappedP : p a b -> WrappedP p a b;"
                        , ""
                        , "  data MaybeF (f :: * -> *) a ="
                        , "      NothingF : MaybeF f a"
                        , "    | JustF : f a -> MaybeF f a;"
                        , ""
                        , "  class Boxed (f :: * -> *) {"
                        , "    truthy : f Bool -> Bool;"
                        , "  }"
                        , ""
                        , "  instance Boxed Box {"
                        , "    truthy = λbox true;"
                        , "  }"
                        , ""
                        , "  class Uses marker {"
                        , "    use : (Boxed f, Functor f) => marker -> marker;"
                        , "  }"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "accepts method constraints whose unknown kinds are solved out of order" $ do
            let programText =
                    unlines
                        [ "module Main export (C, Functor, Uses, main) {"
                        , "  class C a {"
                        , "  }"
                        , ""
                        , "  class Functor (f :: * -> *) {"
                        , "  }"
                        , ""
                        , "  class Uses marker {"
                        , "    use : (C (f a), Functor a) => marker -> marker;"
                        , "  }"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "accepts instance constraints whose unknown kinds are solved out of order" $ do
            let programText =
                    unlines
                        [ "module Main export (C, Functor, Higher, main) {"
                        , "  class C a {"
                        , "  }"
                        , ""
                        , "  class Functor (f :: * -> *) {"
                        , "  }"
                        , ""
                        , "  class Higher (h :: * -> *) {"
                        , "  }"
                        , ""
                        , "  instance (C (f a), Functor a) => Higher a {"
                        , "  }"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "rejects too many constructor type arguments before later lowering" $ do
            let programText =
                    unlines
                        [ "module Main export (Bad, main) {"
                        , "  data Option a ="
                        , "      None : Option a"
                        , "    | Some : a -> Option a;"
                        , ""
                        , "  data Bad ="
                        , "      Bad : Option Int Bool -> Bad;"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramTypeArityMismatch "Option" 1 2)

        it "rejects unsaturated type constructors in definition signatures" $ do
            let programText =
                    unlines
                        [ "module Main export (Option, main) {"
                        , "  data Option a ="
                        , "      None : Option a"
                        , "    | Some : a -> Option a;"
                        , ""
                        , "  def main : Option = None;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramTypeArityMismatch "Option" 1 0)

        it "rejects variable-headed applications whose parameter is first-order" $ do
            let programText =
                    unlines
                        [ "module Main export (Bad, main) {"
                        , "  data Bad (f :: *) ="
                        , "      Bad : f Int -> Bad f;"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramTypeArityMismatch "f" 0 1)

        it "rejects higher-kinded arguments with first-order types" $ do
            let programText =
                    unlines
                        [ "module Main export (Higher, main) {"
                        , "  data Higher (f :: * -> *) a ="
                        , "      Higher : a -> Higher Bool a;"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramKindMismatch (STBase "Bool") (KArrow KType KType) KType)

        it "rejects instance constraints that do not match the class parameter kind" $ do
            let programText =
                    unlines
                        [ "module Main export (Functor, Eq, main) {"
                        , "  class Functor (f :: * -> *) {"
                        , "    map : ∀ a. f a -> f a;"
                        , "  }"
                        , ""
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  instance Functor Bool => Eq Int {"
                        , "    eq = λx λy true;"
                        , "  }"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramKindMismatch (STBase "Bool") (KArrow KType KType) KType)

        it "rejects unsaturated type constructors in instance heads" $ do
            let programText =
                    unlines
                        [ "module Main export (Eq, Option, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  data Option a ="
                        , "      None : Option a"
                        , "    | Some : a -> Option a;"
                        , ""
                        , "  instance Eq Option {"
                        , "    eq = λx λy true;"
                        , "  }"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramTypeArityMismatch "Option" 1 0)

        it "rejects instance heads that do not match the class parameter kind" $ do
            let programText =
                    unlines
                        [ "module Main export (Functor, main) {"
                        , "  class Functor (f :: * -> *) {"
                        , "    map : ∀ a. f a -> f a;"
                        , "  }"
                        , ""
                        , "  instance Functor Bool {"
                        , "    map = λx x;"
                        , "  }"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramKindMismatch (STBase "Bool") (KArrow KType KType) KType)

        it "renders located diagnostics with a mechanically justified hint" $ do
            let programText =
                    unlines
                        [ "module Main export (Option(..), main) {"
                        , "  data Option a ="
                        , "      None : Option a"
                        , "    | Some : a -> Option a;"
                        , ""
                        , "  def main : Bool = let ignore = λx true in ignore None;"
                        , "}"
                        ]
            located <- requireLocatedWithFile "ambiguous.mlfp" programText
            case checkLocatedProgram located of
                Left diagnostic -> do
                    let rendered = renderProgramDiagnostic diagnostic
                    rendered `shouldSatisfy` isInfixOf "ambiguous.mlfp:3:7"
                    rendered `shouldSatisfy` isInfixOf "error: ambiguous constructor use `None`"
                    rendered `shouldSatisfy` isInfixOf "hint: add an explicit result type annotation"
                Right _ -> expectationFailure "expected ambiguous constructor diagnostic"

        it "renders unknown import diagnostics at the import site" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  import Missing;"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            located <- requireLocatedWithFile "missing-import.mlfp" programText
            case checkLocatedProgram located of
                Left diagnostic -> do
                    let rendered = renderProgramDiagnostic diagnostic
                    rendered `shouldSatisfy` isInfixOf "missing-import.mlfp:2:10"
                    rendered `shouldSatisfy` isInfixOf "error: unknown imported module `Missing`"
                Right _ -> expectationFailure "expected unknown import diagnostic"

        it "records one resolved identity for qualified and unqualified references to the same value" $ do
            let programText =
                    unlines
                        [ "module Core export (answer) {"
                        , "  def answer : Int = 1;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import Core as C exposing (answer);"
                        , "  def also : Int = C.answer;"
                        , "  def main : Int = answer;"
                        , "}"
                        ]
            program <- requireParsed programText
            case checkProgram program of
                Right checked -> do
                    let references =
                            [ ref
                            | resolvedModule <- resolvedProgramModules (checkedProgramResolved checked)
                            , resolvedModuleName resolvedModule == "Main"
                            , ref <- resolvedModuleReferences resolvedModule
                            ]
                        symbolFor name =
                            case [resolvedReferenceSymbol ref | ref <- references, resolvedReferenceName ref == name] of
                                symbol : _ -> symbol
                                [] -> error ("missing resolved reference " ++ name)
                        unqualified = symbolFor "answer"
                        qualified = symbolFor "C.answer"
                    sameResolvedSymbol unqualified qualified `shouldBe` True
                    symbolDisplayName (resolvedSymbolSpelling unqualified) `shouldBe` "answer"
                    symbolDisplayName (resolvedSymbolSpelling qualified) `shouldBe` "C.answer"
                Left err -> expectationFailure ("expected check success, got " ++ show err)

        it "stores checked binding source types by data identity" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  data Box = Box : Int -> Box;"
                        , "  def main : Box = Box 1;"
                        , "}"
                        ]
            program <- requireParsed programText
            case checkProgram program of
                Right checked -> do
                    let mainBindings =
                            [ binding
                            | checkedModule <- ProgramTypes.checkedProgramModules checked
                            , ProgramTypes.checkedModuleName checkedModule == "Main"
                            , binding <- ProgramTypes.checkedModuleBindings checkedModule
                            , ProgramTypes.checkedBindingName binding == "Main__main"
                            ]
                        boxIdentityHeads =
                            [ symbolIdentityStableName (ProgramTypes.dataInfoSymbolIdentity dataInfo)
                            | checkedModule <- ProgramTypes.checkedProgramModules checked
                            , ProgramTypes.checkedModuleName checkedModule == "Main"
                            , dataInfo <- Map.elems (ProgramTypes.checkedModuleData checkedModule)
                            , ProgramTypes.dataName dataInfo == "Box"
                            ]
                    case (mainBindings, boxIdentityHeads) of
                        ([binding], [boxIdentityHead]) ->
                            ProgramTypes.checkedBindingSourceTypeIdentity binding `shouldBe` STBase boxIdentityHead
                        _ ->
                            expectationFailure
                                ( "expected one main binding and one Box data identity, got "
                                    ++ show (length mainBindings, length boxIdentityHeads)
                                )
                Left err -> expectationFailure ("expected check success, got " ++ show err)

        it "rejects duplicate resolved class identities before choosing local metadata" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  class C a {"
                        , "  }"
                        , "  class D a {"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
                poisonResolvedClassIdentity target replacement resolved =
                    resolved {resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
                  where
                    targetIdentity = resolvedClassIdentity target resolved

                    poisonModule resolvedModule =
                        resolvedModule
                            { resolvedModuleSemantic =
                                (resolvedModuleSemantic resolvedModule)
                                    { resolvedSemanticModuleSyntax =
                                        poisonSyntax (resolvedSemanticModuleSyntax (resolvedModuleSemantic resolvedModule))
                                    }
                            }

                    poisonSyntax syntax =
                        syntax {moduleDecls = map poisonDecl (moduleDecls syntax)}

                    poisonDecl decl =
                        case decl of
                            DeclClass classDecl
                                | classDeclDisplayName classDecl == replacement ->
                                    DeclClass
                                        classDecl
                                            { classDeclName = mapResolvedSymbolIdentity (const targetIdentity) (classDeclName classDecl)
                                            }
                            _ -> decl
                resolvedClassIdentity name resolved =
                    case
                        [ resolvedSymbolIdentity (classDeclName classDecl)
                        | resolvedModule <- resolvedProgramModules resolved
                        , DeclClass classDecl <- moduleDecls (resolvedModuleSyntax resolvedModule)
                        , classDeclDisplayName classDecl == name
                        ]
                    of
                        identity : _ -> identity
                        [] -> error ("missing resolved class identity: " ++ name)
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved -> do
                    let poisoned = poisonResolvedClassIdentity "C" "D" resolved
                    case checkResolvedProgram poisoned of
                        Left (ProgramPipelineError message) -> do
                            message `shouldSatisfy` isInfixOf "duplicate resolved symbol identity"
                            message `shouldSatisfy` isInfixOf (symbolIdentityStableName (resolvedClassIdentity "C" resolved))
                        other ->
                            expectationFailure ("expected duplicate resolved class identity rejection, got " ++ show other)

        it "rejects duplicate resolved class method identities before choosing local metadata" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  class C a {"
                        , "    m : a -> a;"
                        , "    n : a -> Bool;"
                        , "  }"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
                poisonResolvedMethodIdentity target replacement resolved =
                    resolved {resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
                  where
                    targetIdentity = resolvedMethodIdentity target resolved

                    poisonModule resolvedModule =
                        resolvedModule
                            { resolvedModuleSemantic =
                                (resolvedModuleSemantic resolvedModule)
                                    { resolvedSemanticModuleSyntax =
                                        poisonSyntax (resolvedSemanticModuleSyntax (resolvedModuleSemantic resolvedModule))
                                    }
                            }

                    poisonSyntax syntax =
                        syntax {moduleDecls = map poisonDecl (moduleDecls syntax)}

                    poisonDecl decl =
                        case decl of
                            DeclClass classDecl ->
                                DeclClass classDecl {classDeclMethods = map poisonMethod (classDeclMethods classDecl)}
                            _ -> decl

                    poisonMethod sig
                        | methodSigDisplayName sig == replacement =
                            sig {methodSigName = mapResolvedSymbolIdentity (const targetIdentity) (methodSigName sig)}
                    poisonMethod sig = sig

                resolvedMethodIdentity name resolved =
                    case
                        [ resolvedSymbolIdentity (methodSigName methodSig)
                        | resolvedModule <- resolvedProgramModules resolved
                        , DeclClass classDecl <- moduleDecls (resolvedModuleSyntax resolvedModule)
                        , methodSig <- classDeclMethods classDecl
                        , methodSigDisplayName methodSig == name
                        ]
                    of
                        identity : _ -> identity
                        [] -> error ("missing resolved method identity: " ++ name)
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved -> do
                    let poisoned = poisonResolvedMethodIdentity "m" "n" resolved
                    case checkResolvedProgram poisoned of
                        Left (ProgramPipelineError message) -> do
                            message `shouldSatisfy` isInfixOf "duplicate resolved symbol identity"
                            message `shouldSatisfy` isInfixOf (symbolIdentityStableName (resolvedMethodIdentity "m" resolved))
                        other ->
                            expectationFailure ("expected duplicate resolved class method identity rejection, got " ++ show other)

        it "checks the semantic artifact independently of diagnostic reference adapters" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
                junkSymbol =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 1041 SymbolValue "Ghost" "ghost" Nothing)
                        "ghost"
                        "ghost"
                        (SymbolLocal "Ghost")
                junkReference = mkResolvedReference ResolvedValueReference "ghost" junkSymbol
                poisonDiagnostics resolved =
                    resolved
                        { resolvedProgramModules =
                            map poisonModule (resolvedProgramModules resolved)
                        }
                poisonModule resolvedModule =
                    resolvedModule
                        { resolvedModuleDiagnosticAdapter =
                            ResolvedModuleDiagnosticAdapter
                                { resolvedDiagnosticReferences =
                                    junkReference : resolvedModuleReferences resolvedModule
                                }
                        }
                checkedSemanticResult result =
                    case result of
                        Left err -> Left err
                        Right checked -> Right (checkedProgramMain checked, checkedProgramModules checked)
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved -> do
                    let poisoned = poisonDiagnostics resolved
                    checkedSemanticResult (checkResolvedProgram poisoned)
                        `shouldBe` checkedSemanticResult (checkResolvedProgram resolved)

        it "derives Eq from resolved display metadata when identity names are stale" $ do
            let programText =
                    unlines
                        [ "module Main export (Eq, eq, Box(..), main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  data Box ="
                        , "      Box : Box"
                        , "    deriving Eq;"
                        , ""
                        , "  def main : Bool = eq Box Box;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    checkResolvedProgram (poisonResolvedEqIdentityNames resolved) `shouldSatisfy` isRight

        it "derives Eq constraints by type-parameter identity when binder names are stale" $ do
            let programText =
                    unlines
                        [ "module Main export (Eq, eq, Box(..), main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  instance Eq Int {"
                        , "    eq = λx λy true;"
                        , "  }"
                        , ""
                        , "  data Box a ="
                        , "      Box : a -> Box a"
                        , "    deriving Eq;"
                        , ""
                        , "  def main : Bool = eq (Box 1) (Box 2);"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    checkResolvedProgram (poisonResolvedDataParamBinderName "Box" "$stale_box_param" resolved) `shouldSatisfy` isRight

        it "finalizes nullary method evidence by class parameter identity when binder names are stale" $ do
            let programText =
                    unlines
                        [ "module Main export (Default, default, main) {"
                        , "  class Default a {"
                        , "    default : a;"
                        , "  }"
                        , "  instance Default Int {"
                        , "    default = 7;"
                        , "  }"
                        , "  def choose : Default a => a = default;"
                        , "  def main : Int = choose;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    case checkResolvedProgram (poisonResolvedClassParamBinderName "Default" "$stale_default_param" resolved) of
                        Left err -> expectationFailure ("expected check success, got " ++ show err)
                        Right checked ->
                            programRunOutput <$> runCheckedProgramOutput checked `shouldBe` Right "7\n"

        it "records one resolved identity for mixed spellings across values, types, constructors, classes, and methods" $ do
            let programText =
                    unlines
                        [ "module Core export (Eq, Token(..), answer, eq) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  data Token ="
                        , "      Token : Token;"
                        , "  instance Eq Token {"
                        , "    eq = λx λy true;"
                        , "  }"
                        , "  def answer : Token = Token;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import Core as C exposing (Eq, Token(..), answer, eq);"
                        , "  def left : Token = answer;"
                        , "  def right : C.Token = C.answer;"
                        , "  def sameCtor : C.Token = C.Token;"
                        , "  def usesClass : Eq Token => Bool = true;"
                        , "  def usesQualifiedClass : C.Eq C.Token => Bool = true;"
                        , "  def also : Bool = eq Token Token;"
                        , "  def main : Bool = C.eq Token C.Token;"
                        , "}"
                        ]
            program <- requireParsed programText
            case checkProgram program of
                Right checked -> do
                    let references =
                            [ ref
                            | resolvedModule <- resolvedProgramModules (checkedProgramResolved checked)
                            , resolvedModuleName resolvedModule == "Main"
                            , ref <- resolvedModuleReferences resolvedModule
                            ]
                        symbolFor kind name =
                            case [resolvedReferenceSymbol ref | ref <- references, resolvedReferenceKind ref == kind, resolvedReferenceName ref == name] of
                                symbol : _ -> symbol
                                [] -> error ("missing resolved reference " ++ show (kind, name))
                    sameResolvedSymbol (symbolFor ResolvedValueReference "answer") (symbolFor ResolvedValueReference "C.answer") `shouldBe` True
                    sameResolvedSymbol (symbolFor ResolvedTypeReference "Token") (symbolFor ResolvedTypeReference "C.Token") `shouldBe` True
                    sameResolvedSymbol (symbolFor ResolvedConstructorReference "Token") (symbolFor ResolvedConstructorReference "C.Token") `shouldBe` True
                    sameResolvedSymbol (symbolFor ResolvedClassReference "Eq") (symbolFor ResolvedClassReference "C.Eq") `shouldBe` True
                    sameResolvedSymbol (symbolFor ResolvedMethodReference "eq") (symbolFor ResolvedMethodReference "C.eq") `shouldBe` True
                Left err -> expectationFailure ("expected check success, got " ++ show err)

        it "stores resolved AST global references as symbols and local references as local refs" $ do
            let programText =
                    unlines
                        [ "module Core export (Token(..), answer) {"
                        , "  data Token ="
                        , "      Token : Token;"
                        , "  def answer : Token = Token;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import Core as C exposing (Token(..), answer);"
                        , "  def main : C.Token = let local = C.answer in case local of {"
                        , "    C.Token -> local"
                        , "  };"
                        , "}"
                        ]
            program <- requireParsed programText
            case checkProgram program of
                Right checked -> do
                    let mainModules =
                            [ resolvedModuleSyntax resolvedModule
                            | resolvedModule <- resolvedProgramModules (checkedProgramResolved checked)
                            , resolvedModuleName resolvedModule == "Main"
                            ]
                    case mainModules of
                        [mainModule] ->
                            case [defDecl | DeclDef defDecl <- moduleDecls mainModule, refDisplayName (defDeclName defDecl) == "main"] of
                                [mainDef] -> do
                                    case constrainedBody (defDeclType mainDef) of
                                        RSTBase typeSymbol -> do
                                            symbolDisplayName (resolvedSymbolSpelling typeSymbol) `shouldBe` "C.Token"
                                            symbolDefiningModule (resolvedSymbolIdentity typeSymbol) `shouldBe` "Core"
                                        other -> expectationFailure ("expected resolved type symbol, got " ++ show other)
                                    case defDeclExpr mainDef of
                                        ELet localRef Nothing (EVar (ResolvedGlobalValue answerSymbol)) (ECase (EVar (ResolvedLocalValue scrutineeRef)) [Alt (PatCtor ctorSymbol []) (EVar (ResolvedLocalValue bodyRef))]) -> do
                                            localRefName localRef `shouldBe` "local"
                                            scrutineeRef `shouldBe` localRef
                                            bodyRef `shouldBe` localRef
                                            symbolDisplayName (resolvedSymbolSpelling answerSymbol) `shouldBe` "C.answer"
                                            symbolDefiningModule (resolvedSymbolIdentity answerSymbol) `shouldBe` "Core"
                                            symbolDisplayName (resolvedSymbolSpelling ctorSymbol) `shouldBe` "C.Token"
                                            symbolDefiningModule (resolvedSymbolIdentity ctorSymbol) `shouldBe` "Core"
                                        other -> expectationFailure ("expected resolved local/global expression shape, got " ++ show other)
                                other -> expectationFailure ("expected one main def, got " ++ show (length other))
                        other -> expectationFailure ("expected one Main module, got " ++ show (length other))
                Left err -> expectationFailure ("expected check success, got " ++ show err)

        it "uses globally unique resolved local identities across modules" $ do
            let programText =
                    unlines
                        [ "module A export (a) {"
                        , "  def a : Int -> Int = λx x;"
                        , "}"
                        , ""
                        , "module B export (b) {"
                        , "  def b : Bool -> Bool = λx x;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved -> do
                    let lambdaRefs =
                            [ paramName param
                            | resolvedModule <- resolvedProgramModules resolved
                            , DeclDef def <- moduleDecls (resolvedModuleSyntax resolvedModule)
                            , ELam param _ <- [defDeclExpr def]
                            ]
                        identities = map localRefIdentity lambdaRefs
                    map localRefName lambdaRefs `shouldBe` ["x", "x"]
                    length identities `shouldBe` length (nub identities)

        it "assigns generated identities to resolved forall type binders and occurrences" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : ∀ a. a -> a = λx x;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved -> do
                    let mainTypes =
                            [ defDeclType def
                            | resolvedModule <- resolvedProgramModules resolved
                            , resolvedModuleName resolvedModule == "Main"
                            , DeclDef def <- moduleDecls (resolvedModuleSyntax resolvedModule)
                            , refDisplayName (defDeclName def) == "main"
                            ]
                    case mainTypes of
                        [ConstrainedType [] (RSTForall binder Nothing (RSTArrow (RSTVar dom) (RSTVar cod)))] -> do
                            resolvedTypeBinderName binder `shouldBe` "a"
                            resolvedTypeBinderIdentity dom `shouldBe` resolvedTypeBinderIdentity binder
                            resolvedTypeBinderIdentity cod `shouldBe` resolvedTypeBinderIdentity binder
                        other -> expectationFailure ("expected resolved forall identity type, got " ++ show other)

        it "assigns generated identities to local resolved symbols" $ do
            let programText =
                    unlines
                        [ "module Main export (Box(..), Eq, eq, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  data Box ="
                        , "      Box : Box;"
                        , "  def main : Box = Box;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved -> do
                    let modules = resolvedProgramModules resolved
                        localSymbols =
                            concatMap (concat . Map.elems . ProgramTypes.resolvedModuleLocalValues) modules
                                ++ concatMap (concat . Map.elems . ProgramTypes.resolvedModuleLocalTypes) modules
                                ++ concatMap (concat . Map.elems . ProgramTypes.resolvedModuleLocalClasses) modules
                        generatedIds = map (symbolUniqueIdentity . resolvedSymbolIdentity) localSymbols
                    length generatedIds `shouldBe` length (nub generatedIds)

        it "assigns generated identities to declaration type parameter refs" $ do
            let programText =
                    unlines
                        [ "module Main export (Eq, eq, Box(..)) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    case resolvedProgramModules resolved of
                        [resolvedModule] ->
                            case moduleDecls (resolvedModuleSyntax resolvedModule) of
                                [DeclClass classDecl, DeclData dataDecl] -> do
                                    classParamRef <-
                                        case typeParamRef (classDeclParam classDecl) of
                                            Just ref -> pure ref
                                            Nothing -> expectationFailure "expected resolved class type parameter ref" >> fail "missing class param ref"
                                    (classDomRef, classCodRef) <-
                                        case classDeclMethods classDecl of
                                            [methodSig] ->
                                                case constrainedBody (methodSigType methodSig) of
                                                    RSTArrow (RSTVar domRef) (RSTArrow (RSTVar codRef) (RSTBase _)) ->
                                                        pure (domRef, codRef)
                                                    other -> expectationFailure ("expected resolved class param refs, got " ++ show other) >> fail "unexpected method type"
                                            other -> expectationFailure ("expected one method, got " ++ show other) >> fail "unexpected methods"
                                    dataParamRef <-
                                        case dataDeclParams dataDecl of
                                            [param] ->
                                                case typeParamRef param of
                                                    Just ref -> pure ref
                                                    Nothing -> expectationFailure "expected resolved data type parameter ref" >> fail "missing data param ref"
                                            other -> expectationFailure ("expected one data parameter, got " ++ show other) >> fail "unexpected data params"
                                    (dataFieldRef, dataResultRef) <-
                                        case dataDeclConstructors dataDecl of
                                            [ctorDecl] ->
                                                case constructorDeclType ctorDecl of
                                                    RSTArrow (RSTVar fieldRef) (RSTCon _ (RSTVar resultRef :| [])) ->
                                                        pure (fieldRef, resultRef)
                                                    other -> expectationFailure ("expected resolved data param refs, got " ++ show other) >> fail "unexpected constructor type"
                                            other -> expectationFailure ("expected one constructor, got " ++ show other) >> fail "unexpected constructors"
                                    resolvedTypeBinderIdentity classParamRef `shouldBe` resolvedTypeBinderIdentity classDomRef
                                    resolvedTypeBinderIdentity classDomRef `shouldBe` resolvedTypeBinderIdentity classCodRef
                                    resolvedTypeBinderIdentity dataParamRef `shouldBe` resolvedTypeBinderIdentity dataFieldRef
                                    resolvedTypeBinderIdentity dataFieldRef `shouldBe` resolvedTypeBinderIdentity dataResultRef
                                    resolvedTypeBinderIdentity classDomRef `shouldNotBe` resolvedTypeBinderIdentity dataFieldRef
                                other -> expectationFailure ("unexpected declarations: " ++ show other)
                        other -> expectationFailure ("expected one module, got " ++ show other)

        it "assigns generated identities to implicit type parameter refs" $ do
            let programText =
                    unlines
                        [ "module Main export (Eq, eq, Box(..), keep) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  instance Eq a => Eq (Box a) {"
                        , "    eq = λleft λright true;"
                        , "  }"
                        , ""
                        , "  def keep : a -> a = λvalue value;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    case resolvedProgramModules resolved of
                        [resolvedModule] ->
                            case moduleDecls (resolvedModuleSyntax resolvedModule) of
                                [DeclClass {}, DeclData {}, DeclInstance instDecl, DeclDef defDecl] -> do
                                    (constraintRef, headRef) <-
                                        case (constraintTypes <$> instanceDeclConstraints instDecl, instanceDeclTypes instDecl) of
                                            ([RSTVar constraintRef0 :| []], RSTCon _ (RSTVar headRef0 :| []) :| []) ->
                                                pure (constraintRef0, headRef0)
                                            other -> expectationFailure ("expected resolved instance param refs, got " ++ show other) >> fail "unexpected instance type"
                                    (defArgRef, defResultRef) <-
                                        case constrainedBody (defDeclType defDecl) of
                                            RSTArrow (RSTVar argRef) (RSTVar resultRef) ->
                                                pure (argRef, resultRef)
                                            other -> expectationFailure ("expected resolved def param refs, got " ++ show other) >> fail "unexpected def type"
                                    resolvedTypeBinderIdentity constraintRef `shouldBe` resolvedTypeBinderIdentity headRef
                                    resolvedTypeBinderIdentity defArgRef `shouldBe` resolvedTypeBinderIdentity defResultRef
                                    resolvedTypeBinderIdentity constraintRef `shouldNotBe` resolvedTypeBinderIdentity defArgRef
                                other -> expectationFailure ("unexpected declarations: " ++ show other)
                        other -> expectationFailure ("expected one module, got " ++ show other)

        it "reuses generated def type identities in expression annotations" $ do
            let programText =
                    unlines
                        [ "module Main export (keep) {"
                        , "  def keep : a -> a = λ(value : a) let same : a = value in (same : a);"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    case resolvedProgramModules resolved of
                        [resolvedModule] ->
                            case moduleDecls (resolvedModuleSyntax resolvedModule) of
                                [DeclDef defDecl] -> do
                                    (sigArgRef, sigResultRef) <-
                                        case constrainedBody (defDeclType defDecl) of
                                            RSTArrow (RSTVar argRef) (RSTVar resultRef) ->
                                                pure (argRef, resultRef)
                                            other -> expectationFailure ("expected resolved def signature refs, got " ++ show other) >> fail "unexpected def type"
                                    (paramRef, letRef, annRef) <-
                                        case defDeclExpr defDecl of
                                            ELam Param {paramType = Just (RSTVar paramRef0)}
                                                (ELet _ (Just (RSTVar letRef0)) _ (EAnn _ (RSTVar annRef0))) ->
                                                    pure (paramRef0, letRef0, annRef0)
                                            other -> expectationFailure ("expected resolved expression annotation refs, got " ++ show other) >> fail "unexpected def expr"
                                    let identities = map resolvedTypeBinderIdentity [sigArgRef, sigResultRef, paramRef, letRef, annRef]
                                    length (nub identities) `shouldBe` 1
                                other -> expectationFailure ("unexpected declarations: " ++ show other)
                        other -> expectationFailure ("expected one module, got " ++ show other)

        it "assigns stable generated identities to builtin symbols" $ do
            let builtinIds =
                    map (symbolUniqueIdentity . Builtins.builtinTypeIdentity) (Set.toList Builtins.builtinTypeNames)
                        ++ map (symbolUniqueIdentity . Builtins.builtinValueIdentity) (Set.toList PrimitiveInventory.primitiveValueNames)
                generatedIds = builtinIds
            length generatedIds `shouldBe` length (nub generatedIds)
            all ((< 0) . uniqueIdentityValue) generatedIds `shouldBe` True

        it "reuses generated module identity on resolved imports" $ do
            let programText =
                    unlines
                        [ "module Lib export (value) {"
                        , "  def value : Int = 1;"
                        , "}"
                        , "module Main {"
                        , "  import Lib;"
                        , "  def main : Int = value;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved -> do
                    let modules = resolvedProgramModules resolved
                        libIdentity =
                            [ ProgramTypes.resolvedModuleIdentity resolvedModule
                            | resolvedModule <- modules
                            , resolvedModuleName resolvedModule == "Lib"
                            ]
                        importIdentity =
                            [ resolvedSymbolIdentity (importModuleName imp)
                            | resolvedModule <- modules
                            , resolvedModuleName resolvedModule == "Main"
                            , imp <- moduleImports (resolvedModuleSyntax resolvedModule)
                            ]
                    map symbolUniqueIdentity importIdentity `shouldBe` map symbolUniqueIdentity libIdentity

        it "rejects duplicate resolved module identities before checking imports" $ do
            let programText =
                    unlines
                        [ "module A export (value) {"
                        , "  def value : Int = 1;"
                        , "}"
                        , "module B export (value) {"
                        , "  def value : Int = 2;"
                        , "}"
                        , "module Main export (main) {"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
                replaceModuleIdentity target replacement resolvedModule
                    | resolvedModuleName resolvedModule == target =
                        resolvedModule
                            { resolvedModuleSemantic =
                                (resolvedModuleSemantic resolvedModule)
                                    { resolvedSemanticModuleIdentity = replacement
                                    }
                            }
                replaceModuleIdentity _ _ resolvedModule = resolvedModule
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    case resolvedProgramModules resolved of
                        firstModule : _ -> do
                            let duplicateIdentity = ProgramTypes.resolvedModuleIdentity firstModule
                                poisoned =
                                    resolved
                                        { resolvedProgramModules =
                                            map
                                                (replaceModuleIdentity "B" duplicateIdentity)
                                                (resolvedProgramModules resolved)
                                        }
                            case checkResolvedProgram poisoned of
                                Left (ProgramPipelineError message) -> do
                                    message `shouldSatisfy` isInfixOf "duplicate resolved module identity"
                                    message `shouldSatisfy` isInfixOf (symbolIdentityStableName duplicateIdentity)
                                other ->
                                    expectationFailure ("expected duplicate resolved module identity rejection, got " ++ show other)
                        [] ->
                            expectationFailure "expected resolved modules"

        it "rejects duplicate resolved symbol identities before checking local metadata" $ do
            let boxType =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9401 SymbolType "Main" "Box" Nothing)
                        "Box"
                        "Box"
                        (SymbolLocal "Main")
                sharedCtorIdentity =
                    generatedSymbolIdentity
                        9402
                        SymbolConstructor
                        "Main"
                        "A"
                        (Just (generatedSymbolOwnerType 9401 "Main" "Box"))
                ctorA =
                    mkResolvedSymbol
                        sharedCtorIdentity
                        "A"
                        "A"
                        (SymbolLocal "Main")
                ctorB =
                    mkResolvedSymbol
                        sharedCtorIdentity
                        "B"
                        "B"
                        (SymbolLocal "Main")
                mainValue =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9403 SymbolValue "Main" "main" Nothing)
                        "main"
                        "main"
                        (SymbolLocal "Main")
                resolvedScope =
                    ResolvedScope
                        { resolvedScopeValues = Map.fromList [("A", ctorA), ("B", ctorB), ("main", mainValue)]
                        , resolvedScopeTypes = Map.singleton "Box" boxType
                        , resolvedScopeClasses = Map.empty
                        , resolvedScopeModules = Map.empty
                        }
                resolvedModule =
                    ResolvedModule
                        { resolvedModuleSemantic =
                            ResolvedSemanticModule
                                { resolvedSemanticModuleName = "Main"
                                , resolvedSemanticModuleIdentity = ProgramTypes.moduleSymbolIdentity (UniqueIdentity 9400) "Main"
                                , resolvedSemanticModuleSyntax =
                                    Module
                                        { moduleName = "Main"
                                        , moduleExports = Nothing
                                        , moduleImports = []
                                        , moduleDecls =
                                            [ DeclData
                                                DataDecl
                                                    { dataDeclName = boxType
                                                    , dataDeclParams = []
                                                    , dataDeclConstructors =
                                                        [ ConstructorDecl
                                                            { constructorDeclName = ctorA
                                                            , constructorDeclType = RSTBase boxType
                                                            }
                                                        , ConstructorDecl
                                                            { constructorDeclName = ctorB
                                                            , constructorDeclType = RSTBase boxType
                                                            }
                                                        ]
                                                    , dataDeclDeriving = []
                                                    }
                                            , DeclDef
                                                DefDecl
                                                    { defDeclName = mainValue
                                                    , defDeclType = ConstrainedType [] (RSTBase boxType)
                                                    , defDeclExpr = EVar (ResolvedGlobalValue ctorA)
                                                    }
                                            ]
                                        }
                                , resolvedSemanticModuleLocalSymbols =
                                    ResolvedLocalSymbols
                                        { resolvedLocalValues = Map.fromList [("A", [ctorA]), ("B", [ctorB]), ("main", [mainValue])]
                                        , resolvedLocalTypes = Map.singleton "Box" [boxType]
                                        , resolvedLocalClasses = Map.empty
                                        }
                                , resolvedSemanticModuleScope = resolvedScope
                                , resolvedSemanticModuleExports = resolvedScope
                                }
                        , resolvedModuleDiagnosticAdapter =
                            ResolvedModuleDiagnosticAdapter
                                { resolvedDiagnosticReferences = []
                                }
                        }
            case checkResolvedProgram (ResolvedProgram [resolvedModule]) of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "duplicate resolved symbol identity"
                    message `shouldSatisfy` isInfixOf (symbolIdentityStableName sharedCtorIdentity)
                other ->
                    expectationFailure ("expected duplicate resolved symbol identity rejection, got " ++ show other)

        it "assigns generated identity layer ids to checked instance method values" $ do
            let programText =
                    unlines
                        [ "module Main export (Monoid, Nat(..), mempty, append, main) {"
                        , "  class Monoid a {"
                        , "    mempty : a;"
                        , "    append : a -> a -> a;"
                        , "  }"
                        , "  data Nat ="
                        , "      Zero : Nat;"
                        , "  instance Monoid Nat {"
                        , "    mempty = Zero;"
                        , "    append = λleft λright left;"
                        , "  }"
                        , "  def main : Nat = append mempty Zero;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    case checkResolvedProgram resolved of
                        Left err -> expectationFailure ("expected check success, got " ++ show err)
                        Right checked -> do
                            let resolvedSymbols =
                                    concatMap (concat . Map.elems . ProgramTypes.resolvedModuleLocalValues) (resolvedProgramModules resolved)
                                        ++ concatMap (concat . Map.elems . ProgramTypes.resolvedModuleLocalTypes) (resolvedProgramModules resolved)
                                        ++ concatMap (concat . Map.elems . ProgramTypes.resolvedModuleLocalClasses) (resolvedProgramModules resolved)
                                resolvedIds = map (symbolUniqueIdentity . resolvedSymbolIdentity) resolvedSymbols
                                instanceMethodIds =
                                    [ identity
                                    | checkedModule <- ProgramTypes.checkedProgramModules checked
                                    , instanceInfo <- ProgramTypes.checkedModuleInstances checkedModule
                                    , valueInfo <- Map.elems (instanceMethodsByIdentity instanceInfo)
                                    , let identity = symbolUniqueIdentity (valueInfoSymbol valueInfo)
                                    ]
                                instanceMethodNames =
                                    [ symbolDefiningName (valueInfoSymbol valueInfo)
                                    | checkedModule <- ProgramTypes.checkedProgramModules checked
                                    , instanceInfo <- ProgramTypes.checkedModuleInstances checkedModule
                                    , valueInfo <- Map.elems (instanceMethodsByIdentity instanceInfo)
                                    ]
                                monoidIdentities =
                                    [ resolvedSymbolIdentity symbol
                                    | resolvedModule <- resolvedProgramModules resolved
                                    , symbol <- Map.findWithDefault [] "Monoid" (ProgramTypes.resolvedModuleLocalClasses resolvedModule)
                                    ]
                                methodIdentities =
                                    [ resolvedSymbolIdentity symbol
                                    | resolvedModule <- resolvedProgramModules resolved
                                    , methodName0 <- ["mempty", "append"]
                                    , symbol <- Map.findWithDefault [] methodName0 (ProgramTypes.resolvedModuleLocalValues resolvedModule)
                                    , symbolNamespace (resolvedSymbolIdentity symbol) == SymbolMethod
                                    ]
                                sanitizeRuntimeName = concatMap sanitizeCharForTest
                                sanitizeCharForTest char
                                    | char `elem` ['a' .. 'z'] = [char]
                                    | char `elem` ['A' .. 'Z'] = [char]
                                    | char `elem` ['0' .. '9'] = [char]
                                    | otherwise = "_u" ++ show (fromEnum char) ++ "_"
                                stableRuntimePart = sanitizeRuntimeName . symbolIdentityStableName
                            length instanceMethodIds `shouldBe` 2
                            length instanceMethodIds `shouldBe` length (nub instanceMethodIds)
                            all (`notElem` resolvedIds) instanceMethodIds `shouldBe` True
                            case monoidIdentities of
                                [monoidIdentity] ->
                                    all (isInfixOf (stableRuntimePart monoidIdentity)) instanceMethodNames `shouldBe` True
                                other -> expectationFailure ("expected one Monoid identity, got " ++ show other)
                            map stableRuntimePart methodIdentities
                                `shouldSatisfy` all (\part -> any (isInfixOf part) instanceMethodNames)
                            any (isInfixOf "Monoid") instanceMethodNames `shouldBe` False
                            any (isInfixOf "mempty") instanceMethodNames `shouldBe` False
                            any (isInfixOf "append") instanceMethodNames `shouldBe` False

        it "rejects constructor result heads with matching display but different resolved identity" $ do
            let localBox =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9101 SymbolType "Main" "Box" Nothing)
                        "Box"
                        "Box"
                        (SymbolLocal "Main")
                foreignBox =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9104 SymbolType "Other" "Box" Nothing)
                        "Box"
                        "Box"
                        (SymbolQualifiedImport "Other" "Other")
                badCtor =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9105 SymbolConstructor "Main" "Bad" (Just (generatedSymbolOwnerType 9101 "Main" "Box")))
                        "Bad"
                        "Bad"
                        (SymbolLocal "Main")
                resolvedScope =
                    ResolvedScope
                        { resolvedScopeValues = Map.singleton "Bad" badCtor
                        , resolvedScopeTypes = Map.singleton "Box" localBox
                        , resolvedScopeClasses = Map.empty
                        , resolvedScopeModules = Map.empty
                        }
                resolvedModule =
                    ResolvedModule
                        { resolvedModuleSemantic =
                            ResolvedSemanticModule
                                { resolvedSemanticModuleName = "Main"
                                , resolvedSemanticModuleIdentity = ProgramTypes.moduleSymbolIdentity (UniqueIdentity 9001) "Main"
                                , resolvedSemanticModuleSyntax =
                                    Module
                                        { moduleName = "Main"
                                        , moduleExports = Nothing
                                        , moduleImports = []
                                        , moduleDecls =
                                            [ DeclData
                                                DataDecl
                                                    { dataDeclName = localBox
                                                    , dataDeclParams = []
                                                    , dataDeclConstructors =
                                                        [ ConstructorDecl
                                                            { constructorDeclName = badCtor
                                                            , constructorDeclType = RSTBase foreignBox
                                                            }
                                                        ]
                                                    , dataDeclDeriving = []
                                                    }
                                            ]
                                        }
                                , resolvedSemanticModuleLocalSymbols =
                                    ResolvedLocalSymbols
                                        { resolvedLocalValues = Map.singleton "Bad" [badCtor]
                                        , resolvedLocalTypes = Map.singleton "Box" [localBox]
                                        , resolvedLocalClasses = Map.empty
                                        }
                                , resolvedSemanticModuleScope = resolvedScope
                                , resolvedSemanticModuleExports = resolvedScope
                                }
                        , resolvedModuleDiagnosticAdapter =
                            ResolvedModuleDiagnosticAdapter
                                { resolvedDiagnosticReferences = []
                                }
                        }
            checkResolvedProgram (ResolvedProgram [resolvedModule])
                `shouldBe` Left (ProgramInvalidConstructorResult "Bad" (STBase "Box") "Box")

        it "rejects resolved data parameters without generated identities" $ do
            let boxType =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9051 SymbolType "Main" "Box" Nothing)
                        "Box"
                        "Box"
                        (SymbolLocal "Main")
                resolvedScope =
                    ResolvedScope
                        { resolvedScopeValues = Map.empty
                        , resolvedScopeTypes = Map.singleton "Box" boxType
                        , resolvedScopeClasses = Map.empty
                        , resolvedScopeModules = Map.empty
                        }
                resolvedModule =
                    ResolvedModule
                        { resolvedModuleSemantic =
                            ResolvedSemanticModule
                                { resolvedSemanticModuleName = "Main"
                                , resolvedSemanticModuleIdentity = ProgramTypes.moduleSymbolIdentity (UniqueIdentity 9050) "Main"
                                , resolvedSemanticModuleSyntax =
                                    Module
                                        { moduleName = "Main"
                                        , moduleExports = Nothing
                                        , moduleImports = []
                                        , moduleDecls =
                                            [ DeclData
                                                DataDecl
                                                    { dataDeclName = boxType
                                                    , dataDeclParams = [TypeParam "a" KType]
                                                    , dataDeclConstructors = []
                                                    , dataDeclDeriving = []
                                                    }
                                            ]
                                        }
                                , resolvedSemanticModuleLocalSymbols =
                                    ResolvedLocalSymbols
                                        { resolvedLocalValues = Map.empty
                                        , resolvedLocalTypes = Map.singleton "Box" [boxType]
                                        , resolvedLocalClasses = Map.empty
                                        }
                                , resolvedSemanticModuleScope = resolvedScope
                                , resolvedSemanticModuleExports = resolvedScope
                                }
                        , resolvedModuleDiagnosticAdapter =
                            ResolvedModuleDiagnosticAdapter
                                { resolvedDiagnosticReferences = []
                                }
                        }
            checkResolvedProgram (ResolvedProgram [resolvedModule])
                `shouldBe` Left (ProgramPipelineError "resolved type parameter `a` is missing identity")

        it "seeds resolved declaration identities from type parameters" $ do
            let dataParamIdentity = UniqueIdentity 991901
                classParamIdentity = UniqueIdentity 991902
                boxType =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 991903 SymbolType "Main" "Box" Nothing)
                        "Box"
                        "Box"
                        (SymbolLocal "Main")
                classSymbol =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 991904 SymbolClass "Main" "Functor" Nothing)
                        "Functor"
                        "Functor"
                        (SymbolLocal "Main")
                dataDecl =
                    DeclData
                        DataDecl
                            { dataDeclName = boxType
                            , dataDeclParams = [ResolvedTypeParam (resolvedTypeBinderRef dataParamIdentity "a") KType]
                            , dataDeclConstructors = []
                            , dataDeclDeriving = []
                            }
                classDecl =
                    DeclClass
                        ClassDecl
                            { classDeclName = classSymbol
                            , classDeclSuperclasses = []
                            , classDeclParams =
                                ResolvedTypeParam (resolvedTypeBinderRef classParamIdentity "f") (KArrow KType KType) :| []
                            , classDeclFundeps = []
                            , classDeclMethods = []
                            }
            ProgramTypes.resolvedDeclGeneratedIdentities dataDecl `shouldSatisfy` elem dataParamIdentity
            ProgramTypes.resolvedDeclGeneratedIdentities classDecl `shouldSatisfy` elem classParamIdentity

        it "checks resolved syntax by identity when display spellings are stale" $ do
            let boxType =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9101 SymbolType "Main" "Box" Nothing)
                        "stale.Box"
                        "stale.Box"
                        (SymbolLocal "Main")
                boxCtor =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9102 SymbolConstructor "Main" "Box" (Just (generatedSymbolOwnerType 9101 "Main" "Box")))
                        "stale.Box"
                        "stale.Box"
                        (SymbolLocal "Main")
                mainValue =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9103 SymbolValue "Main" "main" Nothing)
                        "stale.main"
                        "stale.main"
                        (SymbolLocal "Main")
                resolvedScope =
                    ResolvedScope
                        { resolvedScopeValues = Map.fromList [("Box", boxCtor), ("main", mainValue)]
                        , resolvedScopeTypes = Map.singleton "Box" boxType
                        , resolvedScopeClasses = Map.empty
                        , resolvedScopeModules = Map.empty
                        }
                resolvedModule =
                    ResolvedModule
                        { resolvedModuleSemantic =
                            ResolvedSemanticModule
                                { resolvedSemanticModuleName = "Main"
                                , resolvedSemanticModuleIdentity = ProgramTypes.moduleSymbolIdentity (UniqueIdentity 9001) "Main"
                                , resolvedSemanticModuleSyntax =
                                    Module
                                        { moduleName = "Main"
                                        , moduleExports = Nothing
                                        , moduleImports = []
                                        , moduleDecls =
                                            [ DeclData
                                                DataDecl
                                                    { dataDeclName = boxType
                                                    , dataDeclParams = []
                                                    , dataDeclConstructors =
                                                        [ ConstructorDecl
                                                            { constructorDeclName = boxCtor
                                                            , constructorDeclType = RSTBase boxType
                                                            }
                                                        ]
                                                    , dataDeclDeriving = []
                                                    }
                                            , DeclDef
                                                DefDecl
                                                    { defDeclName = mainValue
                                                    , defDeclType = ConstrainedType [] (RSTBase boxType)
                                                    , defDeclExpr = EVar (ResolvedGlobalValue boxCtor)
                                                    }
                                            ]
                                        }
                                , resolvedSemanticModuleLocalSymbols =
                                    ResolvedLocalSymbols
                                        { resolvedLocalValues = Map.fromList [("Box", [boxCtor]), ("main", [mainValue])]
                                        , resolvedLocalTypes = Map.singleton "Box" [boxType]
                                        , resolvedLocalClasses = Map.empty
                                        }
                                , resolvedSemanticModuleScope = resolvedScope
                                , resolvedSemanticModuleExports = resolvedScope
                                }
                        , resolvedModuleDiagnosticAdapter =
                            ResolvedModuleDiagnosticAdapter
                                { resolvedDiagnosticReferences = []
                                }
                        }
            case checkResolvedProgram (ResolvedProgram [resolvedModule]) of
                Left err -> expectationFailure ("check failed: " ++ show err)
                Right checked -> do
                    mainBinding <- requireCheckedBinding "Main__main" checked
                    checkedBindingResolvedVar mainBinding
                        `shouldSatisfy` \resolvedVar@ResolvedVar {resolvedVarRuntimeName, resolvedVarDetails} ->
                            Elab.resolvedVarName resolvedVar == "main"
                                && resolvedVarRuntimeName == "Main__main"
                                && resolvedVarDetails == TopLevelId (resolvedSymbolIdentity mainValue)

        it "does not let resolved local spellings shadow global identities" $ do
            let boxType =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9201 SymbolType "Main" "Box" Nothing)
                        "Box"
                        "Box"
                        (SymbolLocal "Main")
                boxCtor =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9202 SymbolConstructor "Main" "Box" (Just (generatedSymbolOwnerType 9201 "Main" "Box")))
                        "Box"
                        "Box"
                        (SymbolLocal "Main")
                mainValue =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9203 SymbolValue "Main" "main" Nothing)
                        "main"
                        "main"
                        (SymbolLocal "Main")
                shadowRef = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 42)) "Box"
                resolvedScope =
                    ResolvedScope
                        { resolvedScopeValues = Map.fromList [("Box", boxCtor), ("main", mainValue)]
                        , resolvedScopeTypes = Map.singleton "Box" boxType
                        , resolvedScopeClasses = Map.empty
                        , resolvedScopeModules = Map.empty
                        }
                resolvedModule =
                    ResolvedModule
                        { resolvedModuleSemantic =
                            ResolvedSemanticModule
                                { resolvedSemanticModuleName = "Main"
                                , resolvedSemanticModuleIdentity = ProgramTypes.moduleSymbolIdentity (UniqueIdentity 9001) "Main"
                                , resolvedSemanticModuleSyntax =
                                    Module
                                        { moduleName = "Main"
                                        , moduleExports = Nothing
                                        , moduleImports = []
                                        , moduleDecls =
                                            [ DeclData
                                                DataDecl
                                                    { dataDeclName = boxType
                                                    , dataDeclParams = []
                                                    , dataDeclConstructors =
                                                        [ ConstructorDecl
                                                            { constructorDeclName = boxCtor
                                                            , constructorDeclType = RSTBase boxType
                                                            }
                                                        ]
                                                    , dataDeclDeriving = []
                                                    }
                                            , DeclDef
                                                DefDecl
                                                    { defDeclName = mainValue
                                                    , defDeclType = ConstrainedType [] (RSTBase boxType)
                                                    , defDeclExpr =
                                                        EApp
                                                            (ELam (Param shadowRef Nothing) (EVar (ResolvedGlobalValue boxCtor)))
                                                            (ELit (LInt 0))
                                                    }
                                            ]
                                        }
                                , resolvedSemanticModuleLocalSymbols =
                                    ResolvedLocalSymbols
                                        { resolvedLocalValues = Map.fromList [("Box", [boxCtor]), ("main", [mainValue])]
                                        , resolvedLocalTypes = Map.singleton "Box" [boxType]
                                        , resolvedLocalClasses = Map.empty
                                        }
                                , resolvedSemanticModuleScope = resolvedScope
                                , resolvedSemanticModuleExports = resolvedScope
                                }
                        , resolvedModuleDiagnosticAdapter =
                            ResolvedModuleDiagnosticAdapter
                                { resolvedDiagnosticReferences = []
                                }
                        }
            checkResolvedProgram (ResolvedProgram [resolvedModule]) `shouldSatisfy` isRight

        it "elaborates resolved annotations and patterns by identity with stale spellings" $ do
            let boxType =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9301 SymbolType "Main" "Box" Nothing)
                        "wrong.Box"
                        "wrong.Box"
                        (SymbolLocal "Main")
                boxCtor =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9302 SymbolConstructor "Main" "Box" (Just (generatedSymbolOwnerType 9301 "Main" "Box")))
                        "wrong.Box"
                        "wrong.Box"
                        (SymbolLocal "Main")
                mainValue =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9303 SymbolValue "Main" "main" Nothing)
                        "wrong.main"
                        "wrong.main"
                        (SymbolLocal "Main")
                resolvedScope =
                    ResolvedScope
                        { resolvedScopeValues = Map.fromList [("Box", boxCtor), ("main", mainValue)]
                        , resolvedScopeTypes = Map.singleton "Box" boxType
                        , resolvedScopeClasses = Map.empty
                        , resolvedScopeModules = Map.empty
                        }
                resolvedModule =
                    ResolvedModule
                        { resolvedModuleSemantic =
                            ResolvedSemanticModule
                                { resolvedSemanticModuleName = "Main"
                                , resolvedSemanticModuleIdentity = ProgramTypes.moduleSymbolIdentity (UniqueIdentity 9001) "Main"
                                , resolvedSemanticModuleSyntax =
                                    Module
                                        { moduleName = "Main"
                                        , moduleExports = Nothing
                                        , moduleImports = []
                                        , moduleDecls =
                                            [ DeclData
                                                DataDecl
                                                    { dataDeclName = boxType
                                                    , dataDeclParams = []
                                                    , dataDeclConstructors =
                                                        [ ConstructorDecl
                                                            { constructorDeclName = boxCtor
                                                            , constructorDeclType = RSTBase boxType
                                                            }
                                                        ]
                                                    , dataDeclDeriving = []
                                                    }
                                            , DeclDef
                                                DefDecl
                                                    { defDeclName = mainValue
                                                    , defDeclType = ConstrainedType [] (RSTBase boxType)
                                                    , defDeclExpr =
                                                        ECase
                                                            (EAnn (EVar (ResolvedGlobalValue boxCtor)) (RSTBase boxType))
                                                            [ Alt
                                                                (PatAnn (PatCtor boxCtor []) (RSTBase boxType))
                                                                (EVar (ResolvedGlobalValue boxCtor))
                                                            ]
                                                    }
                                            ]
                                        }
                                , resolvedSemanticModuleLocalSymbols =
                                    ResolvedLocalSymbols
                                        { resolvedLocalValues = Map.fromList [("Box", [boxCtor]), ("main", [mainValue])]
                                        , resolvedLocalTypes = Map.singleton "Box" [boxType]
                                        , resolvedLocalClasses = Map.empty
                                        }
                                , resolvedSemanticModuleScope = resolvedScope
                                , resolvedSemanticModuleExports = resolvedScope
                                }
                        , resolvedModuleDiagnosticAdapter =
                            ResolvedModuleDiagnosticAdapter
                                { resolvedDiagnosticReferences = []
                                }
                        }
            checkResolvedProgram (ResolvedProgram [resolvedModule]) `shouldSatisfy` isRight

        it "rejects unknown value references at the resolver boundary" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Int = ghost;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramUnknownValue "ghost")

        it "rejects duplicate visible imported names before downstream checking" $ do
            let programText =
                    unlines
                        [ "module A export (value) {"
                        , "  def value : Int = 1;"
                        , "}"
                        , "module B export (value) {"
                        , "  def value : Int = 2;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import A;"
                        , "  import B;"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramDuplicateVisibleName "value")

        it "exports only methods owned by the selected class" $ do
            let programText =
                    unlines
                        [ "module A export (C) {"
                        , "  class C a {"
                        , "    method : a -> Bool;"
                        , "  }"
                        , ""
                        , "  class D a {"
                        , "    method : a -> Bool;"
                        , "  }"
                        , "}"
                        , "module Main export (main) {"
                        , "  import A;"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            case checkProgram program of
                Left (ProgramDuplicateVisibleName "method") -> expectationFailure "exported a sibling class method"
                result -> result `shouldSatisfy` isRight

        it "rejects ambiguous unqualified references at the resolver boundary" $ do
            let programText =
                    unlines
                        [ "module A export (value) {"
                        , "  def value : Int = 1;"
                        , "}"
                        , "module B export (value) {"
                        , "  def value : Int = 2;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import A;"
                        , "  import B;"
                        , "  def main : Int = value;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramAmbiguousUnqualifiedReference "value")

        it "rejects duplicate case branches across mixed constructor spellings" $ do
            let programText =
                    unlines
                        [ "module Core export (Nat(..)) {"
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Core as C exposing (Nat(..));"
                        , "  def main : Bool = case Zero of {"
                        , "    Zero -> true;"
                        , "    C.Zero -> false"
                        , "  };"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramDuplicateCaseBranch "C.Zero")

        it "rejects duplicate instance heads across mixed class and type spellings" $ do
            let programText =
                    unlines
                        [ "module Core export (Eq, Token(..), eq) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  data Token ="
                        , "      Token : Token;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Core as C exposing (Eq, Token(..), eq);"
                        , "  instance Eq Token {"
                        , "    eq = λx λy true;"
                        , "  }"
                        , "  instance C.Eq C.Token {"
                        , "    eq = λx λy true;"
                        , "  }"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramDuplicateInstance "Eq" (STBase "Token"))

        it "rejects overlapping instance heads across mixed type spellings" $ do
            let programText =
                    unlines
                        [ "module Core export (Token(..)) {"
                        , "  data Token ="
                        , "      Token : Token;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Core as C exposing (Token(..));"
                        , "  class Rel a b {"
                        , "  }"
                        , "  instance Rel Token a {"
                        , "  }"
                        , "  instance Rel C.Token Bool {"
                        , "  }"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program
                `shouldBe` Left
                    ( ProgramOverlappingInstanceHead
                        "Rel"
                        [STBase "Token", STVar "a"]
                        [STBase "C.Token", STBase "Bool"]
                    )

        it "does not overlap same-named local and qualified imported instance heads" $ do
            let programText =
                    unlines
                        [ "module Core export (Token(..)) {"
                        , "  data Token ="
                        , "      RemoteToken : Token;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Core as C;"
                        , "  data Token ="
                        , "      LocalToken : Token;"
                        , "  class Rel a b {"
                        , "  }"
                        , "  instance Rel Token a {"
                        , "  }"
                        , "  instance Rel C.Token Bool {"
                        , "  }"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "keeps alias-only access valid through the resolver pass" $ do
            let programText =
                    unlines
                        [ "module Core export (Token(..)) {"
                        , "  data Token ="
                        , "      Token : Token;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Core as C;"
                        , "  def main : C.Token = C.Token;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "keeps resolved qualified type spelling in checked source display" $ do
            let programText =
                    unlines
                        [ "module Core export (Token(..)) {"
                        , "  data Token ="
                        , "      Token : Token;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Core as C exposing (Token(..));"
                        , "  def left : Token = Token;"
                        , "  def right : C.Token = C.Token;"
                        , "  def main : C.Token = right;"
                        , "}"
                        ]
            program <- requireParsed programText
            checked <- requireChecked program
            rightBinding <- requireCheckedBinding "Main__right" checked
            ProgramTypes.typeViewDisplay (ProgramTypes.checkedBindingSourceTypeView rightBinding)
                `shouldBe` STBase "C.Token"

        it "rejects hidden qualified types at the resolver boundary" $ do
            let programText =
                    unlines
                        [ "module Hidden export () {"
                        , "  data Token ="
                        , "      Token : Token;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Hidden as H;"
                        , "  def main : H.Token = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramUnknownType "H.Token")

        it "rejects hidden deriving classes at the resolver boundary" $ do
            let programText =
                    unlines
                        [ "module Hidden export () {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Hidden as H;"
                        , "  data Box ="
                        , "      Box : Bool -> Box"
                        , "    deriving H.Eq;"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramUnknownClass "H.Eq")

        it "renders duplicate import alias diagnostics at the alias site" $ do
            let programText =
                    unlines
                        [ "module A export () {"
                        , "}"
                        , "module B export () {"
                        , "}"
                        , "module Main export (main) {"
                        , "  import A as C;"
                        , "  import B as C;"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            located <- requireLocatedWithFile "duplicate-alias.mlfp" programText
            case checkLocatedProgram located of
                Left diagnostic -> do
                    let rendered = renderProgramDiagnostic diagnostic
                    rendered `shouldSatisfy` \text ->
                        "duplicate-alias.mlfp:6:15" `isInfixOf` text
                            || "duplicate-alias.mlfp:7:15" `isInfixOf` text
                    rendered `shouldSatisfy` isInfixOf "error: duplicate import alias `C`"
                Right _ -> expectationFailure "expected duplicate import alias diagnostic"

        it "renders import visibility diagnostics at the exposing item" $ do
            let programText =
                    unlines
                        [ "module Hidden export () {"
                        , "  data Nat ="
                        , "      Zero : Nat;"
                        , "}"
                        , "module Main export (main) {"
                        , "  import Hidden exposing (Nat);"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            located <- requireLocatedWithFile "hidden-import.mlfp" programText
            case checkLocatedProgram located of
                Left diagnostic -> do
                    let rendered = renderProgramDiagnostic diagnostic
                    rendered `shouldSatisfy` isInfixOf "hidden-import.mlfp:6:27"
                    rendered `shouldSatisfy` isInfixOf "error: module `Hidden` does not export `Nat`"
                Right _ -> expectationFailure "expected import visibility diagnostic"

        it "renders export visibility diagnostics at the module export item" $ do
            let programText =
                    unlines
                        [ "module Main export (missing) {"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            located <- requireLocatedWithFile "missing-export.mlfp" programText
            case checkLocatedProgram located of
                Left diagnostic -> do
                    diagnosticError diagnostic `shouldBe` ProgramExportNotLocal "missing"
                    renderProgramDiagnostic diagnostic
                        `shouldSatisfy` isInfixOf "missing-export.mlfp:1:21"
                Right _ -> expectationFailure "expected export visibility diagnostic"

        it "does not report missing-instance diagnostics at class declarations" $ do
            let programText =
                    unlines
                        [ "module Main export (Eq, Nat(..), eq, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  data Nat ="
                        , "      Zero : Nat;"
                        , ""
                        , "  def main : Bool = eq Zero Zero;"
                        , "}"
                        ]
            located <- requireLocatedWithFile "missing-instance.mlfp" programText
            case checkLocatedProgram located of
                Left diagnostic -> do
                    diagnosticError diagnostic `shouldBe` ProgramNoMatchingInstance "Eq" (STBase "Main.Nat")
                    diagnosticSpan diagnostic `shouldBe` Nothing
                    renderProgramDiagnostic diagnostic
                        `shouldSatisfy` isInfixOf "error: no matching instance for `Eq STBase \"Main.Nat\"`"
                Right _ -> expectationFailure "expected missing instance diagnostic"

        it "renders unknown instance class diagnostics at the instance head" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  instance Missing Bool {"
                        , "  }"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            located <- requireLocatedWithFile "unknown-instance-class.mlfp" programText
            case checkLocatedProgram located of
                Left diagnostic -> do
                    diagnosticError diagnostic `shouldBe` ProgramUnknownClass "Missing"
                    renderProgramDiagnostic diagnostic
                        `shouldSatisfy` isInfixOf "unknown-instance-class.mlfp:2:12"
                Right _ -> expectationFailure "expected unknown instance class diagnostic"

        it "renders unknown method constraint class diagnostics at the constraint site" $ do
            let programText =
                    unlines
                        [ "module Main export (C, main) {"
                        , "  class C a {"
                        , "    m : Missing a => a -> a;"
                        , "  }"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            located <- requireLocatedWithFile "unknown-method-constraint.mlfp" programText
            case checkLocatedProgram located of
                Left diagnostic -> do
                    diagnosticError diagnostic `shouldBe` ProgramUnknownClass "Missing"
                    renderProgramDiagnostic diagnostic
                        `shouldSatisfy` isInfixOf "unknown-method-constraint.mlfp:3:9"
                Right _ -> expectationFailure "expected unknown method constraint diagnostic"

        it "renders duplicate instance diagnostics with a class span" $ do
            let programText =
                    unlines
                        [ "module Main export (Eq, eq, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  instance Eq Bool {"
                        , "    eq = λx λy true;"
                        , "  }"
                        , ""
                        , "  instance Eq Bool {"
                        , "    eq = λx λy true;"
                        , "  }"
                        , ""
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            located <- requireLocatedWithFile "duplicate-instance.mlfp" programText
            case checkLocatedProgram located of
                Left diagnostic -> do
                    diagnosticError diagnostic `shouldBe` ProgramDuplicateInstance "Eq" (STBase "Bool")
                    renderProgramDiagnostic diagnostic
                        `shouldSatisfy` isInfixOf "duplicate-instance.mlfp:2:3"
                Right _ -> expectationFailure "expected duplicate instance diagnostic"

    describe "MLF.Program eMLF surface parity matrix" $ do
        mapM_ runProgramMatrixCase (nonRuntimeProgramMatrixCases emlfSurfaceParityMatrix)

    describe "MLF.Program eMLF boundary matrix" $ do
        mapM_ runProgramMatrixCase (nonRuntimeProgramMatrixCases emlfBoundaryMatrix)

    describe "MLF.Program eMLF-owned `.mlfp` integration" $ do
        it "fails for a real type mismatch instead of the old infer-lambda gate" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Int = let id = λx x in id true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` either
                (\err -> not ("ProgramCannotInferLambda" `isInfixOf` show err))
                (const False)

        it "does not rewrite same-shaped ADT instantiations to another nominal head" $ do
            let programText =
                    unlines
                        [ "module Main export (A(..), B(..), keep, main) {"
                        , "  data A ="
                        , "      AZ : A;"
                        , ""
                        , "  data B ="
                        , "      BZ : B;"
                        , ""
                        , "  def keep : ∀ a. a -> a = λx x;"
                        , "  def main : B = keep BZ;"
                        , "}"
                        ]
            program <- requireParsed programText
            checked <- requireChecked program
            mainBinding <- requireCheckedBinding "Main__main" checked
            show (checkedBindingTerm mainBinding) `shouldNotSatisfy` isInfixOf "Main.A"
            (prettyValue <$> runProgram program) `shouldBe` Right "BZ"
  where
    roundtripFixture path =
        it ("roundtrips " ++ path) $ do
            program <- requireParsed =<< readFile path
            parseRawProgram (prettyProgram program) `shouldBe` Right program

    runProgramRuntimeCase runtimeCase =
        it (runtimeCaseName runtimeCase) $ do
            program <- loadProgramMatrixSource (runtimeCaseSource runtimeCase)
            let result = stripNewline . programRunOutput <$> runProgramOutput program
            case runtimeCaseExpectation runtimeCase of
                ExpectRuntimeValue expectedValue ->
                    result `shouldBe` Right expectedValue
                ExpectRuntimePredicate label predicate ->
                    case result of
                        Right rendered
                            | predicate rendered -> pure ()
                            | otherwise ->
                                expectationFailure $
                                    "expected "
                                        ++ label
                                        ++ ", got: "
                                        ++ rendered
                        Left err ->
                            expectationFailure ("unexpected program failure: " ++ show err)

    nonRuntimeProgramMatrixCases =
        filter (not . isRuntimeProgramMatrixCase)

    isRuntimeProgramMatrixCase matrixCase =
        case matrixCaseExpectation matrixCase of
            ExpectRunValue _ -> True
            ExpectCheckSuccess -> False
            ExpectCheckFailureContaining _ -> False

    runProgramMatrixCase matrixCase =
        it (matrixCaseName matrixCase) $ do
            program <- loadProgramMatrixSource (matrixCaseSource matrixCase)
            case matrixCaseExpectation matrixCase of
                ExpectRunValue _ ->
                    expectationFailure "runtime-success rows are covered by programRuntimeSuccessCases"
                ExpectCheckSuccess ->
                    checkProgram program `shouldSatisfy` isRight
                ExpectCheckFailureContaining expectedFragment ->
                    checkProgram program `shouldSatisfy` either
                        (isInfixOf expectedFragment . show)
                        (const False)

    loadProgramMatrixSource source =
        withPrelude <$> case source of
            InlineProgram programText -> requireParsed programText
            ProgramFile path -> requireParsed =<< readFile path

    stripNewline s = case reverse s of
        '\n' : rest -> reverse rest
        _ -> s

    requireChecked program =
        case checkProgram program of
            Left err -> expectationFailure ("check failed: " ++ show err) >> fail "check failed"
            Right checked -> pure checked

    requireCheckedLocated located =
        case checkLocatedProgram located of
            Left err -> expectationFailure ("check failed: " ++ show err) >> fail "check failed"
            Right checked -> pure checked

    requireCheckedBinding name checked =
        case
            [ binding
            | checkedModule <- checkedProgramModules checked
            , binding <- checkedModuleBindings checkedModule
            , checkedBindingName binding == name
            ]
        of
            binding : _ -> pure binding
            [] -> expectationFailure ("missing checked binding: " ++ name) >> fail "missing checked binding"

    requireCheckedData moduleName name checked =
        case
            [ dataInfo
            | checkedModule <- checkedProgramModules checked
            , checkedModuleName checkedModule == moduleName
            , dataInfo <- Map.elems (checkedModuleData checkedModule)
            , dataName dataInfo == name
            ]
        of
            dataInfo : _ -> pure dataInfo
            [] -> expectationFailure ("missing checked data: " ++ moduleName ++ "." ++ name) >> fail "missing checked data"

    requireDataConstructor name dataInfo =
        case [ctorInfo | ctorInfo <- dataConstructors dataInfo, ctorName ctorInfo == name] of
            ctorInfo : _ -> pure ctorInfo
            [] -> expectationFailure ("missing constructor: " ++ name) >> fail "missing constructor"

    requireFinalizeContext scope =
        case mkFinalizeContext scope of
            Left err -> expectationFailure ("finalize context failed: " ++ show err) >> fail "finalize context failed"
            Right finalizeContext -> pure finalizeContext

    checkedProgramElaborateScope :: CheckedProgram -> ElaborateScope
    checkedProgramElaborateScope checked =
        mkElaborateScope
            Map.empty
            ( Map.fromList
                [ (ProgramTypes.dataInfoIdentityQualifiedName dataInfo, dataInfo)
                | checkedModule <- checkedProgramModules checked
                , dataInfo <- Map.elems (checkedModuleData checkedModule)
                ]
            )
            ( Map.fromList
                [ (ProgramTypes.classInfoIdentityQualifiedName classInfo, classInfo)
                | checkedModule <- checkedProgramModules checked
                , classInfo <- Map.elems (checkedModuleClasses checkedModule)
                ]
            )
            [ instanceInfo
            | checkedModule <- checkedProgramModules checked
            , instanceInfo <- checkedModuleInstances checkedModule
            ]

replaceCheckedBindingTerm :: String -> Elab.XmlfTerm -> CheckedProgram -> CheckedProgram
replaceCheckedBindingTerm name term checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleBindings =
                map replaceBinding (checkedModuleBindings checkedModule)
            }

    replaceBinding binding
        | checkedBindingName binding == name =
            binding {checkedBindingTerm = term}
        | otherwise = binding

replaceCheckedBindingSourceType :: String -> Surface.SrcType -> CheckedProgram -> CheckedProgram
replaceCheckedBindingSourceType name sourceType checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleBindings =
                map replaceBinding (checkedModuleBindings checkedModule)
            }

    replaceBinding binding
        | checkedBindingName binding == name =
            binding
                { ProgramTypes.checkedBindingSourceTypeView =
                    (ProgramTypes.checkedBindingSourceTypeView binding)
                        { ProgramTypes.typeViewDisplay = sourceType
                        }
                }
        | otherwise = binding

replaceCheckedBindingType :: String -> Elab.ElabType -> CheckedProgram -> CheckedProgram
replaceCheckedBindingType name bindingType checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleBindings =
                map replaceBinding (checkedModuleBindings checkedModule)
            }

    replaceBinding binding
        | checkedBindingName binding == name =
            binding {ProgramTypes.checkedBindingType = bindingType}
        | otherwise = binding

replaceCheckedConstructorTypeView :: String -> ProgramTypes.TypeView -> CheckedProgram -> CheckedProgram
replaceCheckedConstructorTypeView runtimeName view checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleData =
                fmap replaceData (checkedModuleData checkedModule)
            }

    replaceData dataInfo =
        dataInfo
            { dataConstructors =
                map replaceConstructor (dataConstructors dataInfo)
            }

    replaceConstructor ctorInfo
        | ProgramTypes.ctorRuntimeName ctorInfo == runtimeName =
            ctorInfo {ProgramTypes.ctorTypeView = view}
        | otherwise = ctorInfo

replaceCheckedBindingSourceTypeWithHeadIdentities :: String -> Surface.SrcType -> Map.Map String SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedBindingSourceTypeWithHeadIdentities name sourceType headIdentities checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleBindings =
                map replaceBinding (checkedModuleBindings checkedModule)
            }

    replaceBinding binding
        | checkedBindingName binding == name =
            binding
                { ProgramTypes.checkedBindingSourceTypeView =
                    (ProgramTypes.mkTypeView sourceType sourceType)
                        { ProgramTypes.typeViewHeadIdentities = headIdentities
                        }
                }
        | otherwise = binding

replaceCheckedBindingSourceTypeView :: String -> ProgramTypes.TypeView -> CheckedProgram -> CheckedProgram
replaceCheckedBindingSourceTypeView name sourceTypeView checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleBindings =
                map replaceBinding (checkedModuleBindings checkedModule)
            }

    replaceBinding binding
        | checkedBindingName binding == name =
            binding
                { ProgramTypes.checkedBindingSourceTypeView = sourceTypeView
                }
        | otherwise = binding

poisonPrimitiveRuntimeNames :: String -> Elab.XmlfTerm -> Elab.XmlfTerm
poisonPrimitiveRuntimeNames replacement term =
    case term of
        Elab.EVarNode resolved@ResolvedVar {resolvedVarDetails = PrimitiveId ref} ->
            Elab.EVarNode
                resolved
                    { resolvedVarRuntimeName = replacement
                    , resolvedVarDetails = PrimitiveId ref
                    }
        Elab.EVarNode resolved@ResolvedVar {resolvedVarDetails = TopLevelId _} ->
            Elab.EVarNode resolved {resolvedVarRuntimeName = replacement}
        Elab.EVarNode {} -> term
        Elab.ELit {} -> term
        Elab.ELam resolved body -> Elab.ELam resolved (go body)
        Elab.EApp fun arg -> Elab.EApp (go fun) (go arg)
        Elab.ELet resolved scheme rhs body -> Elab.ELet resolved scheme (go rhs) (go body)
        Elab.ETyAbsRef ref mbBound body -> Elab.ETyAbsRef ref mbBound (go body)
        Elab.ETyInst body inst -> Elab.ETyInst (go body) inst
        Elab.ERoll ty body -> Elab.ERoll ty (go body)
        Elab.EUnroll body -> Elab.EUnroll (go body)
  where
    go = poisonPrimitiveRuntimeNames replacement

primitiveTerm :: String -> Elab.XmlfTerm
primitiveTerm name =
    Elab.EVarNode
        ResolvedVar
            { resolvedVarRuntimeName = name
            , resolvedVarType = Elab.TBottom
            , resolvedVarDetails =
                PrimitiveId (primitiveRefFromSymbol (Builtins.builtinValueIdentity name))
            }

replaceCheckedBindingSurfaceExpr :: String -> Surface.SurfaceExpr -> CheckedProgram -> CheckedProgram
replaceCheckedBindingSurfaceExpr name expr checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleBindings =
                map replaceBinding (checkedModuleBindings checkedModule)
            }

    replaceBinding binding
        | checkedBindingName binding == name =
            binding {checkedBindingSurfaceExpr = expr}
        | otherwise = binding

renameCheckedBindingName :: String -> String -> CheckedProgram -> CheckedProgram
renameCheckedBindingName oldName newName checked =
    checked
        { checkedProgramModules =
            map renameModule (checkedProgramModules checked)
        }
  where
    renameModule checkedModule =
        checkedModule
            { checkedModuleBindings =
                map renameBinding (checkedModuleBindings checkedModule)
            }

    renameBinding binding
        | checkedBindingName binding == oldName =
            binding
                { checkedBindingResolvedVar =
                    (checkedBindingResolvedVar binding)
                        { resolvedVarRuntimeName = newName
                        }
                }
        | otherwise = binding

requireTopLevelIdentity :: CheckedBinding -> IO SymbolIdentity
requireTopLevelIdentity binding =
    case resolvedVarDetails (checkedBindingResolvedVar binding) of
        TopLevelId identity -> pure identity
        other -> expectationFailure ("expected top-level binding identity, got " ++ show other) >> fail "missing top-level identity"

replaceCheckedBindingTopLevelIdentity :: String -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedBindingTopLevelIdentity name replacement checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleBindings =
                map replaceBinding (checkedModuleBindings checkedModule)
            }

    replaceBinding binding
        | checkedBindingName binding == name =
            binding
                { checkedBindingResolvedVar =
                    (checkedBindingResolvedVar binding)
                        { resolvedVarDetails = TopLevelId replacement
                        }
                }
        | otherwise = binding

replaceCheckedModuleIdentity :: String -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedModuleIdentity moduleName replacement checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule
        | checkedModuleName checkedModule == moduleName =
            checkedModule {checkedModuleIdentity = replacement}
        | otherwise = checkedModule

replaceCheckedDataSymbol :: SymbolIdentity -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedDataSymbol target replacement checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleData =
                fmap replaceData (checkedModuleData checkedModule)
            }

    replaceData dataInfo
        | dataInfoSymbol dataInfo == target =
            dataInfo {dataInfoSymbol = replacement}
        | otherwise = dataInfo

replaceCheckedConstructorSymbol :: SymbolIdentity -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedConstructorSymbol target replacement checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleData =
                fmap replaceData (checkedModuleData checkedModule)
            }

    replaceData dataInfo =
        dataInfo
            { dataConstructors =
                map replaceConstructor (dataConstructors dataInfo)
            }

    replaceConstructor ctorInfo
        | ctorInfoSymbol ctorInfo == target =
            ctorInfo {ctorInfoSymbol = replacement}
        | otherwise = ctorInfo

replaceCheckedConstructorIndex :: SymbolIdentity -> Int -> CheckedProgram -> CheckedProgram
replaceCheckedConstructorIndex target replacement checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleData =
                fmap replaceData (checkedModuleData checkedModule)
            }

    replaceData dataInfo =
        dataInfo
            { dataConstructors =
                map replaceConstructor (dataConstructors dataInfo)
            }

    replaceConstructor ctorInfo
        | ctorInfoSymbol ctorInfo == target =
            ctorInfo {ctorIndex = replacement}
        | otherwise = ctorInfo

renameCheckedModuleName :: String -> String -> CheckedProgram -> CheckedProgram
renameCheckedModuleName oldName newName checked =
    checked
        { checkedProgramModules =
            map renameModule (checkedProgramModules checked)
        }
  where
    renameModule checkedModule
        | checkedModuleName checkedModule == oldName =
            checkedModule {checkedModuleName = newName}
        | otherwise = checkedModule

renameCheckedConstructorIdentityNamesWhere :: (String -> Bool) -> String -> CheckedProgram -> CheckedProgram
renameCheckedConstructorIdentityNamesWhere predicate replacement checked =
    checked
        { checkedProgramModules =
            map renameModule (checkedProgramModules checked)
        }
  where
    renameModule checkedModule =
        checkedModule
            { checkedModuleData =
                fmap renameDataInfo (checkedModuleData checkedModule)
            }

    renameDataInfo dataInfo =
        dataInfo
            { dataConstructors =
                map renameConstructor (dataConstructors dataInfo)
            }

    renameConstructor ctorInfo
        | predicate (symbolDefiningName (ctorInfoSymbol ctorInfo)) =
            ctorInfo
                { ctorInfoSymbol =
                    renameSymbolDefiningName replacement (ctorInfoSymbol ctorInfo)
                }
        | otherwise = ctorInfo

renameCheckedDataIdentityNamesWhere :: (String -> Bool) -> String -> CheckedProgram -> CheckedProgram
renameCheckedDataIdentityNamesWhere predicate replacement checked =
    checked
        { checkedProgramModules =
            map renameModule (checkedProgramModules checked)
        }
  where
    renameModule checkedModule =
        checkedModule
            { checkedModuleData =
                fmap renameDataInfo (checkedModuleData checkedModule)
            }

    renameDataInfo dataInfo
        | predicate (symbolDefiningName (dataInfoSymbol dataInfo)) =
            dataInfo
                { dataInfoSymbol =
                    renameSymbolDefiningName replacement (dataInfoSymbol dataInfo)
                }
        | otherwise = dataInfo

renameCheckedExportedTypeDisplaysWhere :: (String -> Bool) -> String -> CheckedProgram -> CheckedProgram
renameCheckedExportedTypeDisplaysWhere predicate replacement checked =
    checked
        { checkedProgramModules =
            map renameModule (checkedProgramModules checked)
        }
  where
    renameModule checkedModule =
        checkedModule
            { checkedModuleExports =
                (checkedModuleExports checkedModule)
                    { ProgramTypes.exportedTypeDisplaysByIdentity =
                        fmap renameDisplay (ProgramTypes.exportedTypeDisplaysByIdentity (checkedModuleExports checkedModule))
                    }
            }

    renameDisplay name
        | predicate name = replacement
        | otherwise = name

renameCheckedConstructorRuntimeNamesWhere :: (String -> Bool) -> String -> CheckedProgram -> CheckedProgram
renameCheckedConstructorRuntimeNamesWhere predicate replacement checked =
    checked
        { checkedProgramModules =
            map renameModule (checkedProgramModules checked)
        }
  where
    renameModule checkedModule =
        checkedModule
            { checkedModuleData =
                fmap renameDataInfo (checkedModuleData checkedModule)
            }

    renameDataInfo dataInfo =
        dataInfo
            { dataConstructors =
                map renameConstructor (dataConstructors dataInfo)
            }

    renameConstructor ctorInfo@ConstructorInfo {ctorRuntimeName = runtimeName}
        | predicate runtimeName =
            ctorInfo {ctorRuntimeName = replacement}
    renameConstructor ctorInfo = ctorInfo

renameInstanceMethodRuntimeNamesWhere :: (String -> Bool) -> String -> CheckedProgram -> CheckedProgram
renameInstanceMethodRuntimeNamesWhere predicate replacement checked =
    checked
        { checkedProgramModules =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule =
        checkedModule
            { checkedModuleInstances =
                map replaceInstance (checkedModuleInstances checkedModule)
            }

    replaceInstance instanceInfo =
        instanceInfo
            { instanceMethodsByIdentity =
                fmap replaceValueInfo (instanceMethodsByIdentity instanceInfo)
            }

    replaceValueInfo valueInfo@OrdinaryValue {valueRuntimeName = runtimeName}
        | predicate runtimeName =
            valueInfo {valueRuntimeName = replacement}
    replaceValueInfo valueInfo = valueInfo

checkedProgramDeferredEvidenceMethods :: CheckedProgram -> [EvidenceMethod]
checkedProgramDeferredEvidenceMethods checked =
    concatMap checkedBindingDeferredEvidenceMethods
        [ binding
        | checkedModule <- checkedProgramModules checked
        , binding <- checkedModuleBindings checkedModule
        ]

checkedBindingDeferredEvidenceMethods :: CheckedBinding -> [EvidenceMethod]
checkedBindingDeferredEvidenceMethods binding =
    concatMap obligationEvidenceMethods (Map.elems (checkedBindingDeferredObligations binding))

obligationEvidenceMethods :: DeferredProgramObligation -> [EvidenceMethod]
obligationEvidenceMethods obligation =
    case obligation of
        DeferredMethod deferred ->
            maybe [] ((: []) . deferredMethodEvidenceMethod) (deferredMethodEvidence deferred)
                ++ concatMap evidenceInfoMethods (deferredMethodLocalEvidence deferred)
        DeferredConstructor {} -> []
        DeferredCase {} -> []

evidenceInfoMethods :: EvidenceInfo -> [EvidenceMethod]
evidenceInfoMethods evidence =
    Map.elems (evidenceMethodsByIdentity evidence)

poisonResolvedDeferredEvidenceRuntimeNames :: String -> CheckedProgram -> CheckedProgram
poisonResolvedDeferredEvidenceRuntimeNames replacement =
    mapDeferredEvidenceMethods poisonEvidenceMethod
  where
    poisonEvidenceMethod method
        | Just _ <- evidenceMethodResolvedVar method =
            method {evidenceMethodRuntimeName = replacement}
        | otherwise = method

mapDeferredEvidenceMethods :: (EvidenceMethod -> EvidenceMethod) -> CheckedProgram -> CheckedProgram
mapDeferredEvidenceMethods f checked =
    checked
        { checkedProgramModules =
            map mapModule (checkedProgramModules checked)
        }
  where
    mapModule checkedModule =
        checkedModule
            { checkedModuleBindings =
                map mapBinding (checkedModuleBindings checkedModule)
            }

    mapBinding binding =
        binding
            { checkedBindingDeferredObligations =
                fmap mapObligation (checkedBindingDeferredObligations binding)
            }

    mapObligation obligation =
        case obligation of
            DeferredMethod deferred ->
                DeferredMethod
                    deferred
                        { deferredMethodEvidence = mapDeferredMethodEvidence <$> deferredMethodEvidence deferred
                        , deferredMethodLocalEvidence = map mapEvidenceInfo (deferredMethodLocalEvidence deferred)
                        }
            DeferredConstructor {} -> obligation
            DeferredCase {} -> obligation

    mapDeferredMethodEvidence evidence =
        evidence
            { deferredMethodEvidenceMethod =
                f (deferredMethodEvidenceMethod evidence)
            }

    mapEvidenceInfo evidence =
        evidence
            { evidenceMethodsByIdentity =
                fmap f (evidenceMethodsByIdentity evidence)
            }

resolvedConstructorRefs :: Elab.XmlfTerm -> [ProgramTypes.ConstructorRef]
resolvedConstructorRefs term =
    case term of
        Elab.EVarNode ResolvedVar {resolvedVarDetails = ConstructorId ctorRef} ->
            [ctorRef]
        Elab.EVarNode {} -> []
        Elab.ELit {} -> []
        Elab.ELam _ body -> resolvedConstructorRefs body
        Elab.EApp fun arg -> resolvedConstructorRefs fun ++ resolvedConstructorRefs arg
        Elab.ELet _ _ rhs body -> resolvedConstructorRefs rhs ++ resolvedConstructorRefs body
        Elab.ETyAbsRef _ _ body -> resolvedConstructorRefs body
        Elab.ETyInst body _ -> resolvedConstructorRefs body
        Elab.ERoll _ body -> resolvedConstructorRefs body
        Elab.EUnroll body -> resolvedConstructorRefs body

resolvedLocalBinders :: Elab.XmlfTerm -> [LocalRef]
resolvedLocalBinders term =
    case term of
        Elab.ELam ResolvedVar {resolvedVarDetails = LocalId localRef} body ->
            localRef : resolvedLocalBinders body
        Elab.ELam ResolvedVar {resolvedVarDetails = EvidenceId localRef} body ->
            localRef : resolvedLocalBinders body
        Elab.ELet ResolvedVar {resolvedVarDetails = LocalId localRef} _ rhs body ->
            localRef : resolvedLocalBinders rhs ++ resolvedLocalBinders body
        Elab.ELet ResolvedVar {resolvedVarDetails = EvidenceId localRef} _ rhs body ->
            localRef : resolvedLocalBinders rhs ++ resolvedLocalBinders body
        Elab.ELam _ body -> resolvedLocalBinders body
        Elab.ELet _ _ rhs body -> resolvedLocalBinders rhs ++ resolvedLocalBinders body
        Elab.EVarNode {} -> []
        Elab.ELit {} -> []
        Elab.EApp fun arg -> resolvedLocalBinders fun ++ resolvedLocalBinders arg
        Elab.ETyAbsRef _ _ body -> resolvedLocalBinders body
        Elab.ETyInst body _ -> resolvedLocalBinders body
        Elab.ERoll _ body -> resolvedLocalBinders body
        Elab.EUnroll body -> resolvedLocalBinders body

isGeneratedLocalRef :: LocalRef -> Bool
isGeneratedLocalRef localRef =
    case localRefIdentity localRef of
        GraphLocalId {} -> False
        GeneratedLocalId {} -> True

generatedLocalIdentityValues :: Elab.XmlfTerm -> [UniqueIdentity]
generatedLocalIdentityValues term =
    [ identity
    | localRef <- resolvedLocalBinders term
    , GeneratedLocalId identity <- [localRefIdentity localRef]
    ]

resolvedLocalLetTypes :: Elab.XmlfTerm -> [Elab.ElabType]
resolvedLocalLetTypes term =
    case term of
        Elab.ELet ResolvedVar {resolvedVarDetails = LocalId {}, resolvedVarType = ty} _ rhs body ->
            ty : resolvedLocalLetTypes rhs ++ resolvedLocalLetTypes body
        Elab.EVarNode {} -> []
        Elab.ELit {} -> []
        Elab.ELam _ body -> resolvedLocalLetTypes body
        Elab.EApp fun arg -> resolvedLocalLetTypes fun ++ resolvedLocalLetTypes arg
        Elab.ELet _ _ rhs body -> resolvedLocalLetTypes rhs ++ resolvedLocalLetTypes body
        Elab.ETyAbsRef _ _ body -> resolvedLocalLetTypes body
        Elab.ETyInst body _ -> resolvedLocalLetTypes body
        Elab.ERoll _ body -> resolvedLocalLetTypes body
        Elab.EUnroll body -> resolvedLocalLetTypes body

isPolymorphicIdentityType :: Elab.ElabType -> Bool
isPolymorphicIdentityType ty =
    case ty of
        Elab.TForallRef ref Nothing (Elab.TArrow (Elab.TVarRef argRef) (Elab.TVarRef resultRef)) ->
            Elab.typeBinderRefsSameIdentity argRef ref
                && Elab.typeBinderRefsSameIdentity resultRef ref
        _ -> False

leadingTypeAbsIdentities :: Elab.XmlfTerm -> [TypeBinderIdentity]
leadingTypeAbsIdentities term =
    case term of
        Elab.ETyAbsRef ref _ body ->
            Elab.typeBinderRefIdentity ref : leadingTypeAbsIdentities body
        _ ->
            []

elabTypeMentionsBinder :: TypeBinderIdentity -> Elab.Ty v -> Bool
elabTypeMentionsBinder identity ty =
    case ty of
        Elab.TVarRef ref ->
            Elab.typeBinderRefIdentity ref == identity
        Elab.TArrow dom cod ->
            elabTypeMentionsBinder identity dom || elabTypeMentionsBinder identity cod
        Elab.TBaseWithIdentity {} ->
            False
        Elab.TConWithIdentity _ _ args ->
            any (elabTypeMentionsBinder identity) args
        Elab.TVarAppRef ref args ->
            Elab.typeBinderRefIdentity ref == identity || any (elabTypeMentionsBinder identity) args
        Elab.TForallRef ref mbBound body ->
            Elab.typeBinderRefIdentity ref == identity
                || maybe False (elabTypeMentionsBinder identity) mbBound
                || elabTypeMentionsBinder identity body
        Elab.TMuRef ref body ->
            Elab.typeBinderRefIdentity ref == identity || elabTypeMentionsBinder identity body
        Elab.TBottom ->
            False

replaceFreeTypeVarsOnce :: String -> [SrcType] -> SrcType -> SrcType
replaceFreeTypeVarsOnce target replacements ty =
    snd (go replacements ty)
  where
    go [] current = ([], current)
    go remaining current =
        case current of
            STVar name
                | name == target ->
                    case remaining of
                        replacement : rest -> (rest, replacement)
                | otherwise -> (remaining, current)
            STArrow dom cod ->
                let (remaining1, dom') = go remaining dom
                    (remaining2, cod') = go remaining1 cod
                 in (remaining2, STArrow dom' cod')
            STCon name args ->
                let (remaining', args') = goNonEmpty remaining args
                 in (remaining', STCon name args')
            STVarApp name args ->
                let (remaining', args') = goNonEmpty remaining args
                 in (remaining', STVarApp name args')
            STTyLam name body
                | name == target -> (remaining, current)
                | otherwise ->
                    let (remaining', body') = go remaining body
                     in (remaining', STTyLam name body')
            STTyApp fun arg ->
                let (remaining1, fun') = go remaining fun
                    (remaining2, arg') = go remaining1 arg
                 in (remaining2, STTyApp fun' arg')
            STForall name mb body
                | name == target -> (remaining, current)
                | otherwise ->
                    let (remaining1, mb') = goBound remaining mb
                        (remaining2, body') = go remaining1 body
                     in (remaining2, STForall name mb' body')
            STMu name body
                | name == target -> (remaining, current)
                | otherwise ->
                    let (remaining', body') = go remaining body
                     in (remaining', STMu name body')
            STBase {} -> (remaining, current)
            STBottom -> (remaining, current)

    goBound remaining =
        goMaybe
      where
        goMaybe Nothing = (remaining, Nothing)
        goMaybe (Just (SrcBound bound)) =
            let (remaining', bound') = go remaining bound
             in (remaining', Just (SrcBound bound'))

    goNonEmpty remaining (arg :| args) =
        let (remaining1, arg') = go remaining arg
            (remaining2, args') = goList remaining1 args
         in (remaining2, arg' :| args')

    goList remaining =
        goArgs
      where
        goArgs [] = (remaining, [])
        goArgs (arg : args) =
            let (remaining1, arg') = go remaining arg
                (remaining2, args') = goList remaining1 args
             in (remaining2, arg' : args')

resolvedLocalOccurrences :: Elab.XmlfTerm -> [LocalRef]
resolvedLocalOccurrences term =
    case term of
        Elab.EVarNode ResolvedVar {resolvedVarDetails = LocalId localRef} ->
            [localRef]
        Elab.EVarNode ResolvedVar {resolvedVarDetails = EvidenceId localRef} ->
            [localRef]
        Elab.EVarNode {} -> []
        Elab.ELit {} -> []
        Elab.ELam _ body -> resolvedLocalOccurrences body
        Elab.EApp fun arg -> resolvedLocalOccurrences fun ++ resolvedLocalOccurrences arg
        Elab.ELet _ _ rhs body -> resolvedLocalOccurrences rhs ++ resolvedLocalOccurrences body
        Elab.ETyAbsRef _ _ body -> resolvedLocalOccurrences body
        Elab.ETyInst body _ -> resolvedLocalOccurrences body
        Elab.ERoll _ body -> resolvedLocalOccurrences body
        Elab.EUnroll body -> resolvedLocalOccurrences body

checkedBindingDeferredRefs :: CheckedBinding -> [DeferredRef]
checkedBindingDeferredRefs binding =
    map deferredObligationRef (Map.elems (checkedBindingDeferredObligations binding))
  where
    deferredObligationRef obligation =
        case obligation of
            DeferredMethod deferred -> deferredMethodRef deferred
            DeferredConstructor deferred -> deferredConstructorRef deferred
            DeferredCase deferred -> deferredCaseRef deferred

generatedDeferredIdentityValues :: CheckedBinding -> [UniqueIdentity]
generatedDeferredIdentityValues binding =
    [ identity
    | ref <- checkedBindingDeferredRefs binding
    , let identity = deferredRefIdentity ref
    ]

unresolvedTermVarRefs :: Elab.XmlfTerm -> [DeferredRef]
unresolvedTermVarRefs term =
    case term of
        Elab.EVarNode resolved ->
            maybe [] pure (Elab.deferredResolvedVarRef resolved)
        Elab.ELit {} -> []
        Elab.ELam _ body -> unresolvedTermVarRefs body
        Elab.EApp fun arg -> unresolvedTermVarRefs fun ++ unresolvedTermVarRefs arg
        Elab.ELet _ _ rhs body -> unresolvedTermVarRefs rhs ++ unresolvedTermVarRefs body
        Elab.ETyAbsRef _ _ body -> unresolvedTermVarRefs body
        Elab.ETyInst body _ -> unresolvedTermVarRefs body
        Elab.ERoll _ body -> unresolvedTermVarRefs body
        Elab.EUnroll body -> unresolvedTermVarRefs body

checkedProgramUnresolvedTermVarNames :: CheckedProgram -> [(String, [String])]
checkedProgramUnresolvedTermVarNames checked =
    [ (checkedBindingName binding, names)
    | checkedModule <- checkedProgramModules checked
    , binding <- checkedModuleBindings checkedModule
    , let names = map deferredRefName (unresolvedTermVarRefs (checkedBindingTerm binding))
    , not (null names)
    ]

requireParsed :: String -> IO Program
requireParsed input =
    case parseRawProgram input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program

requireLocated :: String -> IO LocatedProgram
requireLocated = requireLocatedWithFile "<test>"

requireLocatedWithFile :: FilePath -> String -> IO LocatedProgram
requireLocatedWithFile path input =
    case parseLocatedProgramWithFile path input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program
