{-# LANGUAGE GADTs #-}

module ProgramSpec (spec) where

import qualified ElabTypeTestSupport as TestElab
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, mapMaybe)
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
import MLF.Frontend.Program.Check.TestSupport (checkLocatedProgramPackageWithDefaultTiming)
import qualified MLF.Frontend.Program.Checked as Checked
import MLF.Frontend.Program.Checked.Internal (CheckedProgram (..))
import MLF.Frontend.Program.Elaborate (ElaborateScope, constructorTypeView, elaborateScopeRuntimeTypeViews, lowerConstructorBinding, lowerExprBinding, lowerResolvedConstrainedExprBinding, lowerType, lowerTypeView, lowerTypeViewWithIdentities, matchMethodTypeViews, matchTypeViewsAgainstIdentity, mkElaborateScope, sourceTypeIdentityInScope, sourceTypeViewInScope)
import MLF.Frontend.Program.Finalize
    ( finalizeBindingAllowOpaqueWithModuleContext
    , finalizeBindingLayerAllowOpaqueWithModuleContext
    , finalizeBindingWithContext
    , finalizeBindingsAllowOpaqueWithContext
    , mkFinalizeContext
    , mkModuleFinalizeContext
    , elabTypeToRecoveredTypeView
    , resolvedForallSubst
    , sourceForallMatchesInScope
    , srcTypeToElabTypeInScope
    , typeViewToElabType
    )
import MLF.Frontend.Program.Finalize.TestSupport
    ( allocateDeferredRewriteBinderRefs
    , constructLocalOccurrencesForSchemeForTest
    , consumeDeferredConstructorHeadInstantiationsForTest
    , consumeDeferredMethodHeadInstantiationsForTest
    , dropStaleTypeInstsForTest
    , freshenDeferredMethodTypeBinders
    , normalizeCheckedTypeRedexesForTest
    , projectDeferredConstructorConstructionRoutesForTest
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
    , DeferredMethodCall (..)
    , DeferredRef
    , deferredRefFromIdentity
    , deferredRefIdentity
    , deferredRefName
    , DeferredProgramObligation (..)
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
import MLF.Frontend.Program.Prelude
    ( withPreludeLocatedPackage
    , withPreludePackage
    )
import qualified MLF.Frontend.Symbol as Symbol
import MLF.Frontend.Symbol (renameSymbolDefiningName, symbolIdentityStableName)
import MLF.Frontend.Syntax (ResolvedSrcBound (..), ResolvedSrcTy (..), ResolvedTypeBinderRef, SrcBound (..), SrcType, mkSrcBound, resolvedTypeBinderIdentity, resolvedTypeBinderName, resolvedTypeBinderRefFromIdentity)
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
import qualified MLF.Elab.Pipeline as ElabPipeline
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarRefsType)
import qualified MLF.Types.Elab as Elab
import MLF.Types.Identity
    ( LocalIdentity (..)
    , StructuralTypeBinderRole (..)
    , freshIdentity
    , freshenLocalRef
    , identityGeneratorAfter
    , primitiveRefFromSymbol
    , localRefFromNodeId
    , localRefMatchesNodeId
    , localIdentityStableUnique
    , renameLocalRef
    , typeBinderIdentityGeneratedUnique
    , typeBinderIdentityFromNode
    , typeBinderIdentityFromStructural
    , typeBinderIdentityStableName
    )
import Test.Hspec
import TypeViewTestSupport
  ( fixtureTypeView,
    mkTypeView,
    setTypeViewBinderIdentities,
    setTypeViewDisplay,
    setTypeViewHeadIdentities,
    setTypeViewTypes,
  )

import ElabTermTestSupport
    ( generatedLocalRefForName
    , generatedResolvedLocal
    , mkTestTyAbs
    , testTVar
    )
import Parity.ProgramMatrix

generatedSymbolIdentity :: Int -> SymbolNamespace -> String -> String -> Maybe SymbolOwnerIdentity -> SymbolIdentity
generatedSymbolIdentity unique namespace moduleName name owner =
    symbolIdentityFromParts (UniqueIdentity unique) namespace moduleName name owner

generatedSymbolOwnerType :: Int -> String -> String -> SymbolOwnerIdentity
generatedSymbolOwnerType unique moduleName name =
    SymbolOwnerType (generatedSymbolIdentity unique SymbolType moduleName name Nothing)

baseTypeView :: String -> SymbolIdentity -> ProgramTypes.TypeView
baseTypeView displayName identity =
    ( setTypeViewHeadIdentities
        ( Map.fromList
            [ (displayName, identity)
            , (symbolIdentityStableName identity, identity)
            ]
        )
        ( mkTypeView
            (STBase displayName)
            (STBase (symbolIdentityStableName identity))
        )
    )

builtinBaseTypeView :: String -> ProgramTypes.TypeView
builtinBaseTypeView name =
    baseTypeView name (Builtins.builtinTypeIdentity name)

binderFunctionTypeView :: String -> [String] -> TypeBinderIdentity -> ProgramTypes.TypeView
binderFunctionTypeView displayName aliases identity =
    ( setTypeViewBinderIdentities
        ( Map.fromList
            [ (name, identity)
            | name <- displayName : stableName : aliases
            ]
        )
        ( mkTypeView
            displayType
            identityType
        )
    )
  where
    stableName = typeBinderIdentityStableName identity
    displayType = STArrow (STVar displayName) (STVar displayName)
    identityType = STArrow (STVar stableName) (STVar stableName)

resolvedTypeBinderRef :: UniqueIdentity -> String -> ResolvedTypeBinderRef
resolvedTypeBinderRef identity name =
    resolvedTypeBinderRefFromIdentity (typeBinderIdentityFromUnique identity) name

surfaceExprMentionsVar :: String -> Surface.Expr r stage ty -> Bool
surfaceExprMentionsVar needle expr =
    case expr of
        Surface.EVarNode reference -> Surface.termReferenceName reference == needle
        Surface.ELit{} -> False
        Surface.ELamNode reference body -> Surface.termReferenceName reference == needle || surfaceExprMentionsVar needle body
        Surface.EApp fun arg -> surfaceExprMentionsVar needle fun || surfaceExprMentionsVar needle arg
        Surface.ELetNode reference rhs body ->
            Surface.termReferenceName reference == needle || surfaceExprMentionsVar needle rhs || surfaceExprMentionsVar needle body
        Surface.ELamAnnNode reference _ body -> Surface.termReferenceName reference == needle || surfaceExprMentionsVar needle body
        Surface.EAnn expr0 _ -> surfaceExprMentionsVar needle expr0
        Surface.EExactAnn expr0 _ _ -> surfaceExprMentionsVar needle expr0
        Surface.EExactLamNode reference _ body -> Surface.termReferenceName reference == needle || surfaceExprMentionsVar needle body
        Surface.ECoerceConst {} -> False
        Surface.EExactCoerceConst {} -> False

resolvedSurfaceLocalOccurrenceNames :: Surface.ResolvedSurfaceExpr -> [String]
resolvedSurfaceLocalOccurrenceNames expr =
    case expr of
        Surface.EVarNode (Surface.ResolvedTermReference (LocalId _) name) -> [name]
        Surface.EVarNode _ -> []
        Surface.ELit{} -> []
        Surface.ELamNode _ body -> resolvedSurfaceLocalOccurrenceNames body
        Surface.EApp fun arg -> resolvedSurfaceLocalOccurrenceNames fun ++ resolvedSurfaceLocalOccurrenceNames arg
        Surface.ELetNode _ rhs body -> resolvedSurfaceLocalOccurrenceNames rhs ++ resolvedSurfaceLocalOccurrenceNames body
        Surface.ELamAnnNode _ _ body -> resolvedSurfaceLocalOccurrenceNames body
        Surface.EExactLamNode _ _ body -> resolvedSurfaceLocalOccurrenceNames body
        Surface.EAnn expr0 _ -> resolvedSurfaceLocalOccurrenceNames expr0
        Surface.EExactAnn expr0 _ _ -> resolvedSurfaceLocalOccurrenceNames expr0

resolvedSurfaceGeneratedLocalIdentities :: Surface.ResolvedSurfaceExpr -> [UniqueIdentity]
resolvedSurfaceGeneratedLocalIdentities expr =
    case expr of
        Surface.EVarNode reference -> generatedIdentityFor reference
        Surface.ELit{} -> []
        Surface.ELamNode reference body ->
            generatedIdentityFor reference
                ++ resolvedSurfaceGeneratedLocalIdentities body
        Surface.EApp fun arg ->
            resolvedSurfaceGeneratedLocalIdentities fun
                ++ resolvedSurfaceGeneratedLocalIdentities arg
        Surface.ELetNode reference rhs body ->
            generatedIdentityFor reference
                ++ resolvedSurfaceGeneratedLocalIdentities rhs
                ++ resolvedSurfaceGeneratedLocalIdentities body
        Surface.ELamAnnNode reference _ body ->
            generatedIdentityFor reference
                ++ resolvedSurfaceGeneratedLocalIdentities body
        Surface.EExactLamNode reference _ body ->
            generatedIdentityFor reference
                ++ resolvedSurfaceGeneratedLocalIdentities body
        Surface.EAnn expr0 _ -> resolvedSurfaceGeneratedLocalIdentities expr0
        Surface.EExactAnn expr0 _ _ -> resolvedSurfaceGeneratedLocalIdentities expr0
  where
    generatedIdentityFor reference =
        case Surface.resolvedTermReferenceDetails reference of
            LocalId ref ->
                case localRefIdentity ref of
                    GeneratedGraphLocalId identity _ -> [identity]
                    GeneratedLocalId identity -> [identity]
                    _ -> []
            _ -> []

unknownSurfaceVar :: String -> Surface.ResolvedSurfaceExpr
unknownSurfaceVar name =
    Surface.EResolvedVar
        (DeferredId (deferredRefFromIdentity (UniqueIdentity (negate (980000 + stableNameKey name))) name))
        name
  where
    stableNameKey = foldl (\acc char -> (acc * 131 + fromEnum char) `mod` 100000) 0

loweredBindingIdentityFromDetails :: String -> IdDetails -> ProgramTypes.LoweredBindingIdentity
loweredBindingIdentityFromDetails _runtimeName details =
    case details of
        TopLevelId identity -> ProgramTypes.loweredBindingIdentityFromTopLevel identity
        _ -> error "test binding identity must be top-level"

sourceForallMatches :: SrcType -> SrcType -> Bool
sourceForallMatches =
    sourceForallMatchesInScope (mkElaborateScope Map.empty Map.empty Map.empty [])

poisonResolvedEqIdentityNames :: ResolvedProgram -> ResolvedProgram
poisonResolvedEqIdentityNames resolved =
    resolved{resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
  where
    poisonModule resolvedModule =
        resolvedModule
            { resolvedModuleSemantic =
                poisonSemantic (resolvedModuleSemantic resolvedModule)
            }

    poisonSemantic semantic =
        semantic
            { resolvedSemanticModuleSyntax = poisonSyntax (resolvedSemanticModuleSyntax semantic)
            , resolvedSemanticModuleLocalSymbols = poisonLocalSymbols (resolvedSemanticModuleLocalSymbols semantic)
            , resolvedSemanticModuleScope = poisonScope (resolvedSemanticModuleScope semantic)
            , resolvedSemanticModuleExports = poisonScope (resolvedSemanticModuleExports semantic)
            }

    poisonLocalSymbols localSymbols =
        localSymbols
            { resolvedLocalValues = fmap (map poisonMethodSymbol) (resolvedLocalValues localSymbols)
            , resolvedLocalClasses = fmap (map poisonClassSymbol) (resolvedLocalClasses localSymbols)
            }

    poisonScope scope =
        scope
            { resolvedScopeValues = fmap poisonMethodSymbol (resolvedScopeValues scope)
            , resolvedScopeClasses = fmap poisonClassSymbol (resolvedScopeClasses scope)
            }

    poisonSyntax syntax =
        syntax
            { moduleExports = fmap (map poisonExport) (moduleExports syntax)
            , moduleDecls = map poisonDecl (moduleDecls syntax)
            }

    poisonExport exportItem =
        case exportItem of
            ExportValue symbol ->
                ExportValue (poisonMethodSymbol symbol)
            ExportType ref ->
                ExportType (poisonExportTypeRef ref)
            ExportTypeWithConstructors ref ->
                ExportTypeWithConstructors (poisonExportTypeRef ref)

    poisonExportTypeRef ref =
        ref{resolvedExportTypeSymbols = map poisonClassSymbol (resolvedExportTypeSymbols ref)}

    poisonDecl decl =
        case decl of
            DeclClass classDecl ->
                DeclClass
                    classDecl
                        { classDeclName = poisonClassSymbol (classDeclName classDecl)
                        , classDeclSuperclasses = map poisonClassConstraint (classDeclSuperclasses classDecl)
                        , classDeclMethods = map poisonMethodSig (classDeclMethods classDecl)
                        }
            DeclInstance instanceDecl ->
                DeclInstance
                    instanceDecl
                        { instanceDeclConstraints = map poisonClassConstraint (instanceDeclConstraints instanceDecl)
                        , instanceDeclClass = poisonClassSymbol (instanceDeclClass instanceDecl)
                        , instanceDeclTypes = fmap poisonType (instanceDeclTypes instanceDecl)
                        , instanceDeclMethods = map poisonMethodDef (instanceDeclMethods instanceDecl)
                        }
            DeclData dataDecl ->
                DeclData
                    dataDecl
                        { dataDeclConstructors = map poisonConstructorDecl (dataDeclConstructors dataDecl)
                        , dataDeclDeriving = map poisonClassSymbol (dataDeclDeriving dataDecl)
                        }
            DeclDef defDecl ->
                DeclDef
                    defDecl
                        { defDeclType = poisonConstrainedType (defDeclType defDecl)
                        , defDeclExpr = poisonExpr (defDeclExpr defDecl)
                        }
            _ -> decl

    poisonMethodSig sig =
        sig
            { methodSigName = poisonMethodSymbol (methodSigName sig)
            , methodSigType = poisonConstrainedType (methodSigType sig)
            }

    poisonMethodDef methodDef =
        methodDef
            { methodDefName = poisonMethodSymbol (methodDefName methodDef)
            , methodDefExpr = poisonExpr (methodDefExpr methodDef)
            }

    poisonConstructorDecl constructorDecl =
        constructorDecl{constructorDeclType = poisonType (constructorDeclType constructorDecl)}

    poisonConstrainedType constrained =
        constrained
            { constrainedConstraints = map poisonClassConstraint (constrainedConstraints constrained)
            , constrainedBody = poisonType (constrainedBody constrained)
            }

    poisonClassConstraint constraint =
        constraint
            { constraintClassName = poisonClassSymbol (constraintClassName constraint)
            , constraintTypes = fmap poisonType (constraintTypes constraint)
            }

    poisonExpr expr =
        case expr of
            EVar ref -> EVar (poisonValueRef ref)
            ELit lit -> ELit lit
            ELam param body -> ELam (poisonParam param) (poisonExpr body)
            EApp fun arg -> EApp (poisonExpr fun) (poisonExpr arg)
            ELet name mbTy rhs body ->
                ELet name (fmap poisonType mbTy) (poisonExpr rhs) (poisonExpr body)
            EAnn inner ty -> EAnn (poisonExpr inner) (poisonType ty)
            ECase scrutinee alts -> ECase (poisonExpr scrutinee) (map poisonAlt alts)

    poisonValueRef ref =
        case ref of
            ResolvedGlobalValue symbol -> ResolvedGlobalValue (poisonMethodSymbol symbol)
            ResolvedLocalValue localRef -> ResolvedLocalValue localRef

    poisonParam param =
        param{paramType = fmap poisonType (paramType param)}

    poisonAlt alt =
        alt{altPattern = poisonPattern (altPattern alt), altExpr = poisonExpr (altExpr alt)}

    poisonPattern pattern0 =
        case pattern0 of
            PatCtor ctor args -> PatCtor ctor (map poisonPattern args)
            PatVar localRef -> PatVar localRef
            PatWildcard -> PatWildcard
            PatAnn inner ty -> PatAnn (poisonPattern inner) (poisonType ty)

    poisonType :: ResolvedSrcTy n v -> ResolvedSrcTy n v
    poisonType ty =
        case ty of
            RSTVar ref -> RSTVar ref
            RSTArrow dom cod -> RSTArrow (poisonType dom) (poisonType cod)
            RSTBase symbol -> RSTBase symbol
            RSTCon symbol args -> RSTCon symbol (fmap poisonType args)
            RSTVarApp ref args -> RSTVarApp ref (fmap poisonType args)
            RSTTyLam ref body -> RSTTyLam ref (poisonType body)
            RSTTyApp fun arg -> RSTTyApp (poisonType fun) (poisonType arg)
            RSTForall ref mb body ->
                RSTForall ref (fmap poisonBound mb) (poisonType body)
            RSTMu ref body -> RSTMu ref (poisonType body)
            RSTBottom -> RSTBottom

    poisonBound (Surface.ResolvedSrcBound bound) =
        Surface.ResolvedSrcBound (poisonType bound)

    poisonClassSymbol =
        poisonSymbolIdentityName
            (\identity -> symbolNamespace identity == SymbolClass && symbolDefiningName identity == "Eq")
            staleEqClassIdentityName

    poisonMethodSymbol =
        mapResolvedSymbolIdentity poisonMethodIdentity

    poisonMethodIdentity identity
        | symbolNamespace identity == SymbolMethod && symbolDefiningName identity == "eq" =
            case Symbol.symbolOwnerIdentity identity of
                Just (SymbolOwnerClass classIdentity)
                    | symbolNamespace classIdentity == SymbolClass && symbolDefiningName classIdentity == "Eq" ->
                        Symbol.symbolIdentityFromParts
                            (Symbol.symbolUniqueIdentity identity)
                            (Symbol.symbolNamespace identity)
                            (Symbol.symbolDefiningModule identity)
                            staleEqMethodIdentityName
                            (Just (SymbolOwnerClass (renameEqClassIdentity classIdentity)))
                _ ->
                    renameSymbolDefiningName staleEqMethodIdentityName identity
        | otherwise = identity

    renameEqClassIdentity =
        renameSymbolDefiningName staleEqClassIdentityName

    staleEqClassIdentityName =
        "$stale_eq_class_identity_name"

    staleEqMethodIdentityName =
        "$stale_eq_method_identity_name"

poisonResolvedDataParamBinderName :: String -> String -> ResolvedProgram -> ResolvedProgram
poisonResolvedDataParamBinderName targetDataName replacement resolved =
    resolved{resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
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
        syntax{moduleDecls = map poisonDecl (moduleDecls syntax)}

    poisonDecl decl =
        case decl of
            DeclData dataDecl
                | dataDeclDisplayName dataDecl == targetDataName ->
                    DeclData dataDecl{dataDeclParams = map poisonParam (dataDeclParams dataDecl)}
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
    resolved{resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
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
        syntax{moduleDecls = map poisonDecl (moduleDecls syntax)}

    poisonDecl decl =
        case decl of
            DeclClass classDecl
                | classDeclDisplayName classDecl == targetClassName ->
                    DeclClass classDecl{classDeclParams = fmap poisonParam (classDeclParams classDecl)}
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

        it "rejects ordinary-name TypeView construction without an identity payload" $ do
            ProgramTypes.typeViewFromSourceType
                Map.empty
                Map.empty
                (STVar "a")
                `shouldSatisfy` isLeft

        it "projects TypeView children from carried identities without losing forall context" $ do
            let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992540)
                binderStableName = typeBinderIdentityStableName binderIdentity
                boxIdentity = generatedSymbolIdentity 992541 SymbolType "Main" "Box" Nothing
                boxStableName = symbolIdentityStableName boxIdentity
                source =
                    fixtureTypeView
                        ( STForall
                            "a"
                            Nothing
                            ( STArrow
                                (STBase "Alias.Box")
                                (STVar "a")
                            )
                        )
                        ( STForall
                            binderStableName
                            Nothing
                            ( STArrow
                                (STBase boxStableName)
                                (STVar binderStableName)
                            )
                        )
                        ( Map.fromList
                            [ ("Box", boxIdentity)
                            , ("Alias.Box", boxIdentity)
                            , (boxStableName, boxIdentity)
                            ]
                        )
                        ( Map.fromList
                            [ ("a", binderIdentity)
                            , (binderStableName, binderIdentity)
                            ]
                        )
            case ProgramTypes.typeViewNodeView source of
                ProgramTypes.TypeViewForallNode "a" actualBinder Nothing body -> do
                    actualBinder `shouldBe` binderIdentity
                    ProgramTypes.typeViewBinderIdentityForAlias body "a"
                        `shouldBe` Just binderIdentity
                    case ProgramTypes.typeViewNodeView body of
                        ProgramTypes.TypeViewArrowNode domain codomain -> do
                            ProgramTypes.typeViewHeadIdentityForAlias domain "Alias.Box"
                                `shouldBe` Just boxIdentity
                            ProgramTypes.typeViewNodeView domain
                                `shouldBe` ProgramTypes.TypeViewBaseNode "Alias.Box" boxIdentity
                            ProgramTypes.typeViewNodeView codomain
                                `shouldBe` ProgramTypes.TypeViewVarNode "a" binderIdentity
                        other ->
                            expectationFailure ("unexpected projected forall body " ++ show other)
                other ->
                    expectationFailure ("unexpected projected forall view " ++ show other)

        it "groups unmentioned TypeView aliases without losing exact identity contexts" $ do
            let visibleIdentity = generatedSymbolIdentity 992550 SymbolType "Main" "Visible" Nothing
                hiddenIdentity = generatedSymbolIdentity 992551 SymbolType "Imported" "Hidden" Nothing
                firstBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992552)
                secondBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992553)
                headIdentities =
                    Map.fromList
                        [ ("Visible", visibleIdentity)
                        , (symbolIdentityStableName visibleIdentity, visibleIdentity)
                        , ("Hidden", hiddenIdentity)
                        , ("Imported.Hidden", hiddenIdentity)
                        , (symbolIdentityStableName hiddenIdentity, hiddenIdentity)
                        ]
                binderIdentities =
                    Map.fromList
                        [ ("first", firstBinderIdentity)
                        , (typeBinderIdentityStableName firstBinderIdentity, firstBinderIdentity)
                        , ("second", secondBinderIdentity)
                        , (typeBinderIdentityStableName secondBinderIdentity, secondBinderIdentity)
                        ]
            case ProgramTypes.typeViewFromSourceType headIdentities binderIdentities (STBase "Visible") of
                Left err -> expectationFailure ("unexpected TypeView construction failure " ++ show err)
                Right view -> do
                    ProgramTypes.typeViewNodeView view
                        `shouldBe` ProgramTypes.TypeViewBaseNode "Visible" visibleIdentity
                    ProgramTypes.typeViewHeadIdentities view `shouldBe` headIdentities
                    ProgramTypes.typeViewBinderIdentities view `shouldBe` binderIdentities

        it "does not attach unrelated scope identities when lowering a carried TypeView" $ do
            let targetHeadIdentity = generatedSymbolIdentity 992560 SymbolType "Main" "Target" Nothing
                hiddenHeadIdentity = generatedSymbolIdentity 992561 SymbolType "Main" "Hidden" Nothing
                targetCtorIdentity = generatedSymbolIdentity 992562 SymbolConstructor "Main" "MkTarget" (Just (SymbolOwnerType targetHeadIdentity))
                unrelatedHeadIdentity = generatedSymbolIdentity 992563 SymbolType "Imported" "Other" Nothing
                unrelatedBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992564)
                targetView =
                    fixtureTypeView
                        (STBase "Target")
                        (STBase (symbolIdentityStableName targetHeadIdentity))
                        (Map.singleton "Target" targetHeadIdentity)
                        Map.empty
                targetCtorView =
                    fixtureTypeView
                        (STArrow (STBase "Hidden") (STBase "Target"))
                        ( STArrow
                            (STBase (symbolIdentityStableName hiddenHeadIdentity))
                            (STBase (symbolIdentityStableName targetHeadIdentity))
                        )
                        (Map.fromList [("Hidden", hiddenHeadIdentity), ("Target", targetHeadIdentity)])
                        Map.empty
                targetCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = targetCtorIdentity
                        , ctorRuntimeName = "Main__MkTarget"
                        , ctorTypeView = targetCtorView
                        , ctorOwningTypeIdentity = targetHeadIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                targetData = DataInfo targetHeadIdentity [] [targetCtor]
                hiddenData = DataInfo hiddenHeadIdentity [] []
                unrelatedView =
                    fixtureTypeView
                        (STForall "u" Nothing (STBase "Other"))
                        ( STForall
                            (typeBinderIdentityStableName unrelatedBinderIdentity)
                            Nothing
                            (STBase (symbolIdentityStableName unrelatedHeadIdentity))
                        )
                        (Map.singleton "Other" unrelatedHeadIdentity)
                        (Map.singleton "u" unrelatedBinderIdentity)
                targetValue =
                    OrdinaryValue
                        { valueInfoSymbol = generatedSymbolIdentity 992565 SymbolValue "Main" "target" Nothing
                        , valueRuntimeName = "Main__target"
                        , valueTypeView = targetView
                        , valueConstraintInfos = []
                        }
                unrelatedValue =
                    OrdinaryValue
                        { valueInfoSymbol = generatedSymbolIdentity 992566 SymbolValue "Imported" "other" Nothing
                        , valueRuntimeName = "Imported__other"
                        , valueTypeView = unrelatedView
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        (Map.fromList [("target", targetValue), ("other", unrelatedValue)])
                        (Map.fromList [("Target", targetData), ("Hidden", hiddenData)])
                        Map.empty
                        []
                lowered = lowerTypeViewWithIdentities scope targetView
            Map.elems (ProgramTypes.typeViewHeadIdentities lowered)
                `shouldSatisfy` any (Symbol.sameSymbolIdentity targetHeadIdentity)
            Map.elems (ProgramTypes.typeViewHeadIdentities lowered)
                `shouldSatisfy` any (Symbol.sameSymbolIdentity hiddenHeadIdentity)
            Map.elems (ProgramTypes.typeViewHeadIdentities lowered)
                `shouldSatisfy` all (not . Symbol.sameSymbolIdentity unrelatedHeadIdentity)
            Map.elems (ProgramTypes.typeViewBinderIdentities lowered)
                `shouldSatisfy` all (/= unrelatedBinderIdentity)

        it "does not let a carried context alias choose a structural binder owner" $ do
            let literalIdentity = generatedSymbolIdentity 992570 SymbolType "Main" "BoolLiteral" Nothing
                natIdentity =
                    case PrimitiveInventory.primitivePreludeTypeHeadIdentity "Nat" of
                        Just identity -> identity
                        Nothing -> error "missing primitive Prelude.Nat identity"
                literalSelfIdentity =
                    typeBinderIdentityFromStructural
                        (Symbol.symbolUniqueIdentity literalIdentity)
                        StructuralSelfBinder
                natSelfIdentity =
                    typeBinderIdentityFromStructural
                        (Symbol.symbolUniqueIdentity natIdentity)
                        StructuralSelfBinder
                pollutedView =
                    fixtureTypeView
                        (STBase "BoolLiteral")
                        (STBase (symbolIdentityStableName literalIdentity))
                        ( Map.fromList
                            [ ("BoolLiteral", literalIdentity)
                            , (symbolIdentityStableName literalIdentity, literalIdentity)
                            -- This is an earlier scope's lookup history.  The
                            -- current BoolLiteral node carries literalIdentity
                            -- and must remain the structural owner.
                            , ("Main.BoolLiteral", natIdentity)
                            ]
                        )
                        Map.empty
                scope =
                    mkElaborateScope
                        Map.empty
                        (Map.singleton "BoolLiteral" (DataInfo literalIdentity [] []))
                        Map.empty
                        []
                lowered = lowerTypeViewWithIdentities scope pollutedView
            ProgramTypes.typeViewBinderIdentityForAlias lowered "$Main.BoolLiteral_self"
                `shouldBe` Just literalSelfIdentity
            Map.elems (ProgramTypes.typeViewBinderIdentities lowered)
                `shouldSatisfy` all (/= natSelfIdentity)

        it "builds type-view substitutions from binder identities" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991622)
                replacement = mkTypeView (STBase "Int") (STBase "Int")
                subst = ProgramTypes.typeViewSubstFromParamIdentities (identity :| []) (replacement :| [])
            ProgramTypes.lookupTypeViewSubst identity subst
                `shouldBe` Just replacement

        it "preserves replacement identity payloads in type-binder substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991632)
                replacementIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991633)
                replacementStableName = typeBinderIdentityStableName replacementIdentity
                replacement =
                    (setTypeViewBinderIdentities (Map.singleton "display" replacementIdentity) (mkTypeView (STVar "display") (STVar replacementStableName)))
                subst =
                    ProgramTypes.typeBinderSubstFromTypeViewSubst
                        (Map.singleton sourceIdentity replacement)
            ProgramTypes.lookupTypeBinderSubstViewByIdentity sourceIdentity subst
                `shouldBe` Just replacement

        it "does not erase a non-identity type application at a monomorphic target" $ do
            let boolTy = TestElab.tBase (BaseTy "Bool")
                intTy = TestElab.tBase (BaseTy "Int")
                resolved = generatedResolvedLocal 992218 "value" "Main__value" boolTy
                env =
                    ElabPipeline.mkTypeCheckEnvWithResolvedTerms
                        [(resolved, boolTy)]
                        Map.empty
                term =
                    Elab.ETyInst
                        (Elab.EVarNode resolved)
                        (Elab.InstApp intTy)
            dropStaleTypeInstsForTest env term `shouldBe` term
            dropStaleTypeInstsForTest
                env
                (Elab.ETyInst (Elab.EVarNode resolved) (Elab.InstBot boolTy))
                `shouldBe` Elab.EVarNode resolved

        it "constructs monomorphic local occurrences by identity, not spelling" $ do
            let boolTy = TestElab.tBase (BaseTy "Bool")
                localTy = Elab.TArrow boolTy boolTy
                ghostRef =
                    Elab.typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 992221))
                        "ghost"
                staleOccurrenceTy = Elab.TForallRef ghostRef Nothing localTy
                binder = generatedResolvedLocal 992222 "id" "Main__id" localTy
                renamedOccurrence =
                    generatedResolvedLocal 992222 "choose" "Main__choose" staleOccurrenceTy
                sameSpellingOther =
                    generatedResolvedLocal 992223 "id" "Other__id" staleOccurrenceTy
                env =
                    ElabPipeline.mkTypeCheckEnvWithResolvedTerms
                        [(binder, localTy)]
                        Map.empty
                term =
                    Elab.EApp
                        (Elab.ETyInst (Elab.EVarNode renamedOccurrence) Elab.InstElim)
                        (Elab.ETyInst (Elab.EVarNode sameSpellingOther) Elab.InstElim)
                expected =
                    Elab.EApp
                        ( Elab.EVarNode
                            (Elab.mapResolvedVarType (const localTy) renamedOccurrence)
                        )
                        (Elab.ETyInst (Elab.EVarNode sameSpellingOther) Elab.InstElim)
            constructLocalOccurrencesForSchemeForTest env binder localTy term
                `shouldBe` expected

        it "normalizes checked type redexes only when they do not mint identities" $ do
            let binderRef =
                    Elab.typeBinderRefFromIdentity
                        (typeBinderIdentityFromUnique (UniqueIdentity 992217))
                        "a"
                literal = Elab.ELit (LInt 1)
                elimination =
                    Elab.ETyInst
                        (Elab.eTyAbsWithRef binderRef Nothing literal)
                        Elab.InstElim
                introduction =
                    Elab.ETyInst literal Elab.InstIntro
            normalizeCheckedTypeRedexesForTest elimination
                `shouldBe` literal
            normalizeCheckedTypeRedexesForTest introduction
                `shouldBe` introduction
            case Elab.elabToBound (TestElab.tBase (BaseTy "Int")) of
                Left err ->
                    expectationFailure
                        ("could not construct bounded-Hyp regression: " ++ show err)
                Right intBound -> do
                    let boundedHyp =
                            Elab.ETyInst
                                ( Elab.eTyAbsWithRef
                                    binderRef
                                    (Just intBound)
                                    ( Elab.ETyInst
                                        literal
                                        (Elab.InstAbstrRef binderRef)
                                    )
                                )
                                Elab.InstElim
                    normalizeCheckedTypeRedexesForTest boundedHyp
                        `shouldBe` boundedHyp

        it "rejects conflicting or excess deferred constructor head instantiations" $ do
            let scope = mkElaborateScope Map.empty Map.empty Map.empty []
                binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992219)
                secondBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992220)
                boolTy = TestElab.tBase (BaseTy "Bool")
                intTy = TestElab.tBase (BaseTy "Int")
                boolView = ProgramTypes.typeViewFromElabType boolTy
                sourceSubst =
                    ProgramTypes.insertTypeBinderSubstView
                        binderIdentity
                        boolView
                        ProgramTypes.emptyTypeBinderSubst
                consume =
                    consumeDeferredConstructorHeadInstantiationsForTest
                        scope
                        "Boxed"
                        [("a", binderIdentity)]
                        sourceSubst
            consume [intTy]
                `shouldBe` Left (ProgramAmbiguousConstructorUse "Boxed")
            consume [boolTy, intTy]
                `shouldBe` Left (ProgramAmbiguousConstructorUse "Boxed")
            case
                consumeDeferredConstructorHeadInstantiationsForTest
                    scope
                    "Paired"
                    [("a", binderIdentity), ("b", secondBinderIdentity)]
                    ProgramTypes.emptyTypeBinderSubst
                    [boolTy, intTy]
                of
                Left err ->
                    expectationFailure ("ordered constructor head consumption failed: " ++ show err)
                Right subst -> do
                    ProgramTypes.lookupTypeBinderSubstViewByIdentity binderIdentity subst
                        `shouldBe` Just boolView
                    ProgramTypes.lookupTypeBinderSubstViewByIdentity secondBinderIdentity subst
                        `shouldBe` Just (ProgramTypes.typeViewFromElabType intTy)

        it "maps deferred partial-method heads in order at the source-supplied arity boundary" $ do
            let scope = mkElaborateScope Map.empty Map.empty Map.empty []
                classIdentity = generatedSymbolIdentity 992228 SymbolClass "Main" "Pick" Nothing
                methodIdentity =
                    generatedSymbolIdentity
                        992229
                        SymbolMethod
                        "Main"
                        "pick"
                        (Just (SymbolOwnerClass classIdentity))
                classParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992230)
                ghostIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992231)
                valueIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992232)
                ghostRef = Elab.typeBinderRefFromIdentity ghostIdentity "ghost"
                valueRef = Elab.typeBinderRefFromIdentity valueIdentity "b"
                boolTy = TestElab.tBase (BaseTy "Bool")
                intTy = TestElab.tBase (BaseTy "Int")
                methodInfo =
                    MethodInfo
                        { methodInfoSymbol = methodIdentity
                        , methodDisplayName = "pick"
                        , methodTypeViewRaw =
                            ProgramTypes.typeViewFromElabType
                                ( Elab.TForallRef
                                    ghostRef
                                    Nothing
                                    ( Elab.TForallRef
                                        valueRef
                                        Nothing
                                        (Elab.TArrow boolTy (Elab.TArrow (Elab.TVarRef valueRef) (Elab.TVarRef valueRef)))
                                    )
                                )
                        , methodConstraintInfos = []
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                deferred =
                    DeferredMethodCall
                        { deferredMethodRef =
                            deferredRefFromIdentity (UniqueIdentity 992239) "pick"
                        , deferredMethodInfo = methodInfo
                        , deferredMethodSuppliedArgCount = 1
                        , deferredMethodRemainingArgCount = 1
                        , deferredMethodInstBinders =
                            [("ghost", ghostIdentity), ("b", valueIdentity)]
                        , deferredMethodExpectedResult = Nothing
                        , deferredMethodEvidence = Nothing
                        , deferredMethodLocalEvidence = []
                        }
                etaOnlyDeferred =
                    deferred
                        { deferredMethodSuppliedArgCount = 0
                        , deferredMethodRemainingArgCount = 2
                        }
            ProgramTypes.deferredMethodTotalArgCount deferred
                `shouldBe` 2
            ProgramTypes.deferredMethodResolutionArgCount deferred
                `shouldBe` 1
            ProgramTypes.deferredMethodResolutionArgCount etaOnlyDeferred
                `shouldBe` 2
            case consumeDeferredMethodHeadInstantiationsForTest scope deferred [intTy, boolTy] of
                Left err ->
                    expectationFailure ("ordered method head consumption failed: " ++ show err)
                Right subst -> do
                    ProgramTypes.lookupTypeViewSubst ghostIdentity subst
                        `shouldBe` Just (ProgramTypes.typeViewFromElabType intTy)
                    ProgramTypes.lookupTypeViewSubst valueIdentity subst
                        `shouldBe` Just (ProgramTypes.typeViewFromElabType boolTy)
            consumeDeferredMethodHeadInstantiationsForTest scope deferred [intTy, boolTy, intTy]
                `shouldBe` Left (ProgramAmbiguousMethodUse "pick")

        it "finalizes a partial deferred method from its checked head instantiation" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Eq, Mix, eq, mix, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  instance Eq Bool {"
                        , "    eq = λleft λright true;"
                        , "  }"
                        , "  class Mix a {"
                        , "    mix : Eq b => a -> b -> Bool;"
                        , "  }"
                        , "  instance Mix Bool {"
                        , "    mix = λx λy eq y y;"
                        , "  }"
                        , "  def applyBool : (Bool -> Bool) -> Bool = λf f true;"
                        , "  def main : Bool = applyBool (mix true);"
                        , "}"
                        ]
            checked <- requireChecked program
            case runCheckedProgramOutput checked of
                Left err ->
                    expectationFailure ("partial deferred method runtime failed: " ++ show err)
                Right result ->
                    programRunOutput result `shouldBe` "true\n"

        it "uses the recorded placeholder binder identity and strips vacuous construction wrappers" $ do
            let scope = mkElaborateScope Map.empty Map.empty Map.empty []
                classIdentity = generatedSymbolIdentity 992233 SymbolClass "Main" "Pick" Nothing
                methodIdentity =
                    generatedSymbolIdentity
                        992234
                        SymbolMethod
                        "Main"
                        "pick"
                        (Just (SymbolOwnerClass classIdentity))
                classParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992235)
                outerWrapperIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992237)
                innerWrapperIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992238)
                classParamRef = Elab.typeBinderRefFromIdentity classParamIdentity "a"
                outerWrapperRef = Elab.typeBinderRefFromIdentity outerWrapperIdentity "outer"
                innerWrapperRef = Elab.typeBinderRefFromIdentity innerWrapperIdentity "inner"
                boolTy = TestElab.tBase (BaseTy "Bool")
                wrappedBoolTy =
                    Elab.TForallRef
                        outerWrapperRef
                        (Just (TestElab.tBase (BaseTy "Bool")))
                        (Elab.TForallRef innerWrapperRef Nothing boolTy)
                methodInfo =
                    MethodInfo
                        { methodInfoSymbol = methodIdentity
                        , methodDisplayName = "pick"
                        , methodTypeViewRaw =
                            ProgramTypes.typeViewFromElabType
                                (Elab.TArrow (Elab.TVarRef classParamRef) (Elab.TVarRef classParamRef))
                        , methodConstraintInfos = []
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                deferred =
                    DeferredMethodCall
                        { deferredMethodRef =
                            deferredRefFromIdentity (UniqueIdentity 992240) "pick"
                        , deferredMethodInfo = methodInfo
                        , deferredMethodSuppliedArgCount = 1
                        , deferredMethodRemainingArgCount = 0
                        , deferredMethodInstBinders = [("a", classParamIdentity)]
                        , deferredMethodExpectedResult = Nothing
                        , deferredMethodEvidence = Nothing
                        , deferredMethodLocalEvidence = []
                        }
            case consumeDeferredMethodHeadInstantiationsForTest scope deferred [wrappedBoolTy] of
                Left err ->
                    expectationFailure ("vacuous method head wrapper consumption failed: " ++ show err)
                Right subst ->
                    ProgramTypes.lookupTypeViewSubst classParamIdentity subst
                        `shouldBe` Just (ProgramTypes.typeViewFromElabType boolTy)

        it "projects deferred constructor identities through sibling-safe common construction routes" $ do
            let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992220)
                localIdentity = typeBinderIdentityFromNode (NodeId 992221)
                conflictingLocalIdentity = typeBinderIdentityFromNode (NodeId 992227)
                constructorBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992222)
                sourceRef = Elab.typeBinderRefFromIdentity sourceIdentity "source"
                localRef = Elab.typeBinderRefFromIdentity localIdentity "local"
                conflictingLocalRef =
                    Elab.typeBinderRefFromIdentity
                        conflictingLocalIdentity
                        "conflicting-local"
                sourceView = ProgramTypes.typeViewFromElabType (Elab.TVarRef sourceRef)
                localView = ProgramTypes.typeViewFromElabType (Elab.TVarRef localRef)
                dataIdentity = generatedSymbolIdentity 992223 SymbolType "Main" "Box" Nothing
                constructorIdentity =
                    generatedSymbolIdentity
                        992224
                        SymbolConstructor
                        "Main"
                        "Boxed"
                        (Just (SymbolOwnerType dataIdentity))
                constructorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = constructorIdentity
                        , ctorRuntimeName = "Main__Boxed"
                        , ctorTypeView = sourceView
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                routedRef = deferredRefFromIdentity (UniqueIdentity 992225) "$routed_constructor"
                siblingRef = deferredRefFromIdentity (UniqueIdentity 992226) "$sibling_constructor"
                deferred ref =
                    DeferredConstructor
                        DeferredConstructorCall
                            { deferredConstructorRef = ref
                            , deferredConstructorInfo = constructorInfo
                            , deferredConstructorArgCount = 0
                            , deferredConstructorSourceTypeView = sourceView
                            , deferredConstructorOccurrenceTypeView = sourceView
                            , deferredConstructorInstBinders = [("a", constructorBinderIdentity)]
                            , deferredConstructorInitialSubst =
                                ProgramTypes.insertTypeBinderSubstView
                                    constructorBinderIdentity
                                    sourceView
                                    ProgramTypes.emptyTypeBinderSubst
                            , deferredConstructorBindingMode = ProgramTypes.DeferredBindingMonomorphic
                            }
                constructionRedex constructionRef ref =
                    Elab.ETyInst
                        ( Elab.ETyAbsRef
                            constructionRef
                            Nothing
                            (Elab.EVarNode (Elab.deferredResolvedVarFromRef ref))
                        )
                        (Elab.InstApp (Elab.TVarRef sourceRef))
                term =
                    Elab.EApp
                        (constructionRedex localRef routedRef)
                        (Elab.EVarNode (Elab.deferredResolvedVarFromRef siblingRef))
                obligations =
                    Map.fromList
                        [ (routedRef, deferred routedRef)
                        , (siblingRef, deferred siblingRef)
                        ]
                projected =
                    projectDeferredConstructorConstructionRoutesForTest term obligations
                projectedAgain =
                    projectDeferredConstructorConstructionRoutesForTest term projected
                repeatedTerm =
                    Elab.EApp
                        (constructionRedex localRef routedRef)
                        (constructionRedex conflictingLocalRef routedRef)
                repeatedProjected =
                    projectDeferredConstructorConstructionRoutesForTest
                        repeatedTerm
                        (Map.singleton routedRef (deferred routedRef))
                requireConstructor projectedObligations ref =
                    case Map.lookup ref projectedObligations of
                        Just (DeferredConstructor actual) -> actual
                        other -> error ("expected deferred constructor, got " ++ show other)
                routed = requireConstructor projected routedRef
                sibling = requireConstructor projected siblingRef
                repeated = requireConstructor repeatedProjected routedRef
            deferredConstructorSourceTypeView routed `shouldBe` localView
            deferredConstructorOccurrenceTypeView routed `shouldBe` localView
            ProgramTypes.lookupTypeBinderSubstViewByIdentity
                constructorBinderIdentity
                (deferredConstructorInitialSubst routed)
                `shouldBe` Just localView
            deferredConstructorSourceTypeView sibling `shouldBe` sourceView
            deferredConstructorOccurrenceTypeView sibling `shouldBe` sourceView
            ProgramTypes.lookupTypeBinderSubstViewByIdentity
                constructorBinderIdentity
                (deferredConstructorInitialSubst sibling)
                `shouldBe` Just sourceView
            deferredConstructorSourceTypeView repeated `shouldBe` sourceView
            deferredConstructorOccurrenceTypeView repeated `shouldBe` sourceView
            ProgramTypes.lookupTypeBinderSubstViewByIdentity
                constructorBinderIdentity
                (deferredConstructorInitialSubst repeated)
                `shouldBe` Just sourceView
            projectedAgain `shouldBe` projected

        it "rejects stable-looking binder names without identity payloads" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991601)
                stableName = typeBinderIdentityStableName identity
            ProgramTypes.typeViewFromSourceType
                Map.empty
                Map.empty
                (STVar stableName)
                `shouldSatisfy` isLeft

        it "requires a payload for a stable-looking TypeView source name" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991603)
                stableName = typeBinderIdentityStableName identity
            ProgramTypes.typeViewFromSourceType
                Map.empty
                Map.empty
                (STVar stableName)
                `shouldSatisfy` isLeft

        it "keys generated stable binder TypeView substitutions by identity" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991613)
                replacement = mkTypeView (STBase "Int") (STBase "Int")
                subst =
                    ProgramTypes.typeBinderSubstFromTypeViewSubst
                        (Map.singleton identity replacement)
                viewSubst = ProgramTypes.typeBinderSubstToTypeViewSubst subst
            ProgramTypes.lookupTypeBinderSubstViewByIdentity identity subst
                `shouldBe` Just replacement
            ProgramTypes.lookupTypeViewSubst identity viewSubst
                `shouldBe` Just replacement

        it "cannot construct a stable-name TypeView without identity payloads" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991619)
                stableName = typeBinderIdentityStableName identity
            ProgramTypes.typeViewFromSourceType
                Map.empty
                Map.empty
                (STVar stableName)
                `shouldSatisfy` isLeft

        it "applies identity-keyed type-view substitutions through carried binder payloads" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991619)
                stableName = typeBinderIdentityStableName identity
                replacement = mkTypeView (STBase "Int") (STBase "Int")
                source =
                    (setTypeViewBinderIdentities (Map.singleton "a" identity) (mkTypeView (STVar "a") (STVar stableName)))
                actual =
                    ProgramTypes.applyTypeViewSubst
                        (Map.singleton identity replacement)
                        source
            ProgramTypes.typeViewDisplay actual `shouldBe` STBase "Int"
            ProgramTypes.typeViewIdentity actual
                `shouldBe` ProgramTypes.typeViewIdentity (builtinBaseTypeView "Int")

        it "specializes quantified TypeViews without leaving identity-incomplete binders" $ do
            let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991620)
                binderStableName = typeBinderIdentityStableName binderIdentity
                boxIdentity = generatedSymbolIdentity 991621 SymbolType "Main" "Box" Nothing
                boxStableName = symbolIdentityStableName boxIdentity
                source =
                    fixtureTypeView
                        (STForall "a" Nothing (STArrow (STVar "a") (STCon "Box" (STVar "a" :| []))))
                        (STForall binderStableName Nothing (STArrow (STVar binderStableName) (STCon boxStableName (STVar binderStableName :| []))))
                        (Map.fromList [("Box", boxIdentity), (boxStableName, boxIdentity)])
                        (Map.fromList [("a", binderIdentity), (binderStableName, binderIdentity)])
                specialized =
                    ProgramTypes.specializeQuantifiedTypeView
                        ( Map.singleton
                            binderIdentity
                            (builtinBaseTypeView "Int")
                        )
                        source
            ProgramTypes.typeViewDisplay specialized
                `shouldBe` STArrow (STBase "Int") (STCon "Box" (STBase "Int" :| []))

        it "applies same-spelled type-view substitutions by node identity" $ do
            let leftIdentity = typeBinderIdentityFromNode (NodeId 992510)
                rightIdentity = typeBinderIdentityFromNode (NodeId 992511)
                leftStableName = typeBinderIdentityStableName leftIdentity
                rightStableName = typeBinderIdentityStableName rightIdentity
                source =
                    ( setTypeViewBinderIdentities
                        ( Map.fromList
                            [ (leftStableName, leftIdentity)
                            , (rightStableName, rightIdentity)
                            ]
                        )
                        ( mkTypeView
                            (STArrow (STVar "a") (STVar "a"))
                            (STArrow (STVar leftStableName) (STVar rightStableName))
                        )
                    )
                subst =
                    Map.fromList
                        [ (leftIdentity, mkTypeView (STBase "Int") (STBase "Int"))
                        , (rightIdentity, mkTypeView (STBase "Bool") (STBase "Bool"))
                        ]
                actual = ProgramTypes.applyTypeViewSubst subst source
            ProgramTypes.typeViewDisplay actual `shouldBe` STArrow (STBase "Int") (STBase "Bool")
            ProgramTypes.typeViewIdentity actual
                `shouldBe` STArrow
                    (ProgramTypes.typeViewIdentity (builtinBaseTypeView "Int"))
                    (ProgramTypes.typeViewIdentity (builtinBaseTypeView "Bool"))

        it "does not let ambiguous display aliases overwrite identity substitutions" $ do
            let leftIdentity = typeBinderIdentityFromNode (NodeId 992512)
                rightIdentity = typeBinderIdentityFromNode (NodeId 992513)
                leftStableName = typeBinderIdentityStableName leftIdentity
                rightStableName = typeBinderIdentityStableName rightIdentity
                source =
                    ( setTypeViewBinderIdentities
                        ( Map.fromList
                            [ (leftStableName, leftIdentity)
                            , (rightStableName, rightIdentity)
                            ]
                        )
                        ( mkTypeView
                            (STArrow (STVar "a") (STVar "a"))
                            (STArrow (STVar leftStableName) (STVar rightStableName))
                        )
                    )
                subst =
                    Map.fromList
                        [ (leftIdentity, mkTypeView (STBase "Int") (STBase "Int"))
                        , (rightIdentity, mkTypeView (STBase "Bool") (STBase "Bool"))
                        ]
                actual = ProgramTypes.applyTypeViewSubst subst source
            ProgramTypes.typeViewIdentity actual
                `shouldBe` STArrow
                    (ProgramTypes.typeViewIdentity (builtinBaseTypeView "Int"))
                    (ProgramTypes.typeViewIdentity (builtinBaseTypeView "Bool"))

        it "replaces only the same-spelled node selected by identity" $ do
            let leftIdentity = typeBinderIdentityFromNode (NodeId 992514)
                rightIdentity = typeBinderIdentityFromNode (NodeId 992515)
                leftStableName = typeBinderIdentityStableName leftIdentity
                rightStableName = typeBinderIdentityStableName rightIdentity
                source =
                    ( setTypeViewBinderIdentities
                        ( Map.fromList
                            [ (leftStableName, leftIdentity)
                            , (rightStableName, rightIdentity)
                            ]
                        )
                        ( mkTypeView
                            (STArrow (STVar "a") (STVar "a"))
                            (STArrow (STVar leftStableName) (STVar rightStableName))
                        )
                    )
                subst =
                    Map.singleton
                        leftIdentity
                        (mkTypeView (STBase "Int") (STBase "Int"))
                actual = ProgramTypes.applyTypeViewSubst subst source
            ProgramTypes.typeViewDisplay actual `shouldBe` STArrow (STBase "Int") (STVar "a")
            ProgramTypes.typeViewIdentity actual
                `shouldBe` STArrow
                    (ProgramTypes.typeViewIdentity (builtinBaseTypeView "Int"))
                    (STVar rightStableName)

        it "keys type-view substitutions by identity through binder metadata" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991621)
                stableName = typeBinderIdentityStableName identity
                source =
                    (setTypeViewBinderIdentities (Map.singleton "a" identity) (mkTypeView (STVar stableName) (STVar stableName)))
            ProgramTypes.typeViewSubstKeyFor source "a"
                `shouldBe` Just identity

        it "keys type-view substitutions by stable identity spelling through binder metadata" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991623)
                stableName = typeBinderIdentityStableName identity
                source =
                    (setTypeViewBinderIdentities (Map.singleton "a" identity) (mkTypeView (STVar "a") (STVar stableName)))
            ProgramTypes.typeViewSubstKeyFor source stableName
                `shouldBe` Just identity

        it "keeps multiple display aliases when they select one binder identity" $ do
            let identity = typeBinderIdentityFromNode (NodeId 992520)
                stableName = typeBinderIdentityStableName identity
                source =
                    ( setTypeViewBinderIdentities
                        (Map.singleton stableName identity)
                        ( mkTypeView
                            (STArrow (STVar "a") (STVar "b"))
                            (STArrow (STVar stableName) (STVar stableName))
                        )
                    )
            ProgramTypes.typeViewSubstKeyFor source "a" `shouldBe` Just identity
            ProgramTypes.typeViewSubstKeyFor source "b" `shouldBe` Just identity
            ProgramTypes.typeViewSubstKeyFor source stableName
                `shouldBe` Just identity

        it "collects free type-view variables directly from binder payloads" $ do
            let leftIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991624)
                rightIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991625)
                leftStableName = typeBinderIdentityStableName leftIdentity
                rightStableName = typeBinderIdentityStableName rightIdentity
                source =
                    ( setTypeViewBinderIdentities
                        ( Map.fromList
                            [ ("a", leftIdentity)
                            , (rightStableName, rightIdentity)
                            ]
                        )
                        ( mkTypeView
                            (STArrow (STVar "a") (STVar "b"))
                            (STArrow (STVar leftStableName) (STVar rightStableName))
                        )
                    )
            ProgramTypes.freeTypeBinderIdentitiesTypeView source
                `shouldBe` Set.fromList [leftIdentity, rightIdentity]

        it "rejects free type-view variable construction without binder metadata" $ do
            ProgramTypes.typeViewFromSourceType
                Map.empty
                Map.empty
                (STVar "a")
                `shouldSatisfy` isLeft

        it "keeps replacement type head identities by display key after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 991429)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                headIdentity = generatedSymbolIdentity 991430 SymbolType "Main" "Token" Nothing
                headStableName = symbolIdentityStableName headIdentity
                sourceView =
                    (setTypeViewBinderIdentities (Map.singleton "x" sourceIdentity) (mkTypeView (STVar "x") (STVar sourceStableName)))
                replacement =
                    (setTypeViewHeadIdentities (Map.singleton "Token" headIdentity) (mkTypeView (STBase "Token") (STBase headStableName)))
                subst =
                    Map.singleton sourceIdentity replacement
            let actual = ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
            Map.lookup "Token" actual `shouldBe` Just headIdentity
            Map.lookup headStableName actual `shouldBe` Just headIdentity

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
                    (setTypeViewHeadIdentities (Map.singleton ctorHeadName ctorHeadIdentity) (mkTypeView (STBase "Phantom") (STBase ctorHeadName)))
                ownerShapeView =
                    (setTypeViewHeadIdentities (Map.singleton ownerShapeHeadName ownerShapeHeadIdentity) (mkTypeView (STBase "Phantom") (STBase ownerShapeHeadName)))
                ownerShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity
                        , constructorShapeRuntimeName = "Main__MkPhantom"
                        , constructorShapeTypeView = ownerShapeView
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = []
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__MkPhantom"
                        , ctorTypeView = ctorView
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
            ProgramTypes.typeViewHeadIdentities ctorView
                `shouldSatisfy` elem ctorHeadIdentity . Map.elems
            ProgramTypes.typeViewHeadIdentities ownerShapeView
                `shouldSatisfy` elem ownerShapeHeadIdentity . Map.elems
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
                        { resolvedVarType = Elab.TVarRef binderRef
                        , resolvedVarDetails = TopLevelId valueIdentity
                        }
                method =
                    EvidenceMethod
                        { evidenceMethodSymbol = methodIdentity
                        , evidenceMethodResolvedVar = resolved
                        , evidenceMethodTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        }
            ProgramTypes.evidenceMethodGeneratedIdentities method `shouldSatisfy` elem binderUnique

        it "keeps replacement type head identities by display pair after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 991650)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                headIdentity = generatedSymbolIdentity 991651 SymbolType "Main" "Token" Nothing
                headStableName = symbolIdentityStableName headIdentity
                sourceView =
                    (setTypeViewBinderIdentities (Map.singleton "x" sourceIdentity) (mkTypeView (STVar "x") (STVar sourceStableName)))
                replacement =
                    (setTypeViewHeadIdentities (Map.singleton headStableName headIdentity) (mkTypeView (STBase "DisplayToken") (STBase headStableName)))
                subst =
                    Map.singleton sourceIdentity replacement
                actual =
                    ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
            Map.lookup "DisplayToken" actual `shouldBe` Just headIdentity

        it "projects constructor argument head identities from carried payloads" $ do
            let dataIdentity = generatedSymbolIdentity 991652 SymbolType "Main" "Box" Nothing
                ctorIdentity = generatedSymbolIdentity 991653 SymbolConstructor "Main" "MkBox" (Just (SymbolOwnerType dataIdentity))
                argIdentity = generatedSymbolIdentity 991654 SymbolType "Main" "Token" Nothing
                resultIdentity = generatedSymbolIdentity 991655 SymbolType "Main" "Box" Nothing
                argStableName = symbolIdentityStableName argIdentity
                resultStableName = symbolIdentityStableName resultIdentity
                ctorView =
                    ( setTypeViewHeadIdentities
                        ( Map.fromList
                            [ (argStableName, argIdentity)
                            , (resultStableName, resultIdentity)
                            ]
                        )
                        ( mkTypeView
                            (STArrow (STBase "DisplayToken") (STBase "DisplayBox"))
                            (STArrow (STBase argStableName) (STBase resultStableName))
                        )
                    )
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__MkBox"
                        , ctorTypeView = ctorView
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                shape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity
                        , constructorShapeRuntimeName = "Main__MkBox"
                        , constructorShapeTypeView = ctorView
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = []
                        }
            case ProgramTypes.constructorInfoArgViews ctorInfo of
                [argView] -> do
                    Map.lookup "DisplayToken" (ProgramTypes.typeViewHeadIdentities argView)
                        `shouldBe` Just argIdentity
                    ProgramTypes.typeViewHeadIdentityForAlias argView "DisplayToken"
                        `shouldBe` Just argIdentity
                views ->
                    expectationFailure ("expected one constructor arg view, got " ++ show views)
            Map.lookup "DisplayBox" (ProgramTypes.typeViewHeadIdentities (ProgramTypes.constructorInfoResultView ctorInfo))
                `shouldBe` Just resultIdentity
            case ProgramTypes.constructorShapeArgViews shape of
                [argView] -> do
                    Map.lookup "DisplayToken" (ProgramTypes.typeViewHeadIdentities argView)
                        `shouldBe` Just argIdentity
                    Map.lookup "DisplayBox" (ProgramTypes.typeViewHeadIdentities argView)
                        `shouldBe` Nothing
                views ->
                    expectationFailure ("expected one constructor shape arg view, got " ++ show views)
            Map.lookup "DisplayBox" (ProgramTypes.typeViewHeadIdentities (ProgramTypes.constructorShapeResultView shape))
                `shouldBe` Just resultIdentity

        it "projects constructor argument binder identities without retaining result binders" $ do
            let dataIdentity = generatedSymbolIdentity 992538 SymbolType "Main" "Box" Nothing
                ctorIdentity = generatedSymbolIdentity 992539 SymbolConstructor "Main" "MkBox" (Just (SymbolOwnerType dataIdentity))
                argIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992540)
                resultIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992541)
                argStableName = typeBinderIdentityStableName argIdentity
                resultStableName = typeBinderIdentityStableName resultIdentity
                ctorView =
                    ( setTypeViewBinderIdentities
                        ( Map.fromList
                            [ (argStableName, argIdentity)
                            , (resultStableName, resultIdentity)
                            ]
                        )
                        ( mkTypeView
                            (STArrow (STVar "DisplayArg") (STVar "DisplayResult"))
                            (STArrow (STVar argStableName) (STVar resultStableName))
                        )
                    )
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__MkBox"
                        , ctorTypeView = ctorView
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
            case ProgramTypes.constructorInfoArgViews ctorInfo of
                [argView] -> do
                    Map.lookup argStableName (ProgramTypes.typeViewBinderIdentities argView)
                        `shouldBe` Just argIdentity
                    ProgramTypes.typeViewBinderIdentityForAlias argView "DisplayArg"
                        `shouldBe` Just argIdentity
                    Map.lookup resultStableName (ProgramTypes.typeViewBinderIdentities argView)
                        `shouldBe` Nothing
                views ->
                    expectationFailure ("expected one constructor arg view, got " ++ show views)

        it "retains constructor existential binder identities on projected result views" $ do
            let dataIdentity = generatedSymbolIdentity 992542 SymbolType "Main" "SomeExpr" Nothing
                ctorIdentity = generatedSymbolIdentity 992543 SymbolConstructor "Main" "SomeExpr" (Just (SymbolOwnerType dataIdentity))
                existentialIdentity = typeBinderIdentityFromUnique (UniqueIdentity 992544)
                existentialStableName = typeBinderIdentityStableName existentialIdentity
                displayTy =
                    STForall
                        "a"
                        Nothing
                        (STArrow (STCon "Expr" (STVar "a" :| [])) (STBase "SomeExpr"))
                identityTy =
                    STForall
                        existentialStableName
                        Nothing
                        (STArrow (STCon "Expr" (STVar existentialStableName :| [])) (STBase "SomeExpr"))
                ctorView =
                    (setTypeViewBinderIdentities (Map.singleton existentialStableName existentialIdentity) (mkTypeView displayTy identityTy))
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__SomeExpr"
                        , ctorTypeView = ctorView
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                resultView = ProgramTypes.constructorInfoResultView ctorInfo
            Map.lookup existentialStableName (ProgramTypes.typeViewBinderIdentities resultView)
                `shouldBe` Just existentialIdentity
            ProgramTypes.typeViewBinderIdentityForAlias resultView existentialStableName
                `shouldBe` Just existentialIdentity

        it "keeps replacement type head identities by payload stable name after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 992501)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                headIdentity = generatedSymbolIdentity 992502 SymbolType "Main" "Token" Nothing
                headStableName = symbolIdentityStableName headIdentity
                sourceView =
                    (setTypeViewBinderIdentities (Map.singleton "x" sourceIdentity) (mkTypeView (STVar "x") (STVar sourceStableName)))
                replacement =
                    (setTypeViewHeadIdentities (Map.singleton "Token" headIdentity) (mkTypeView (STBase headStableName) (STBase headStableName)))
                subst =
                    Map.singleton sourceIdentity replacement
            let actual = ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
            Map.lookup "Token" actual `shouldBe` Just headIdentity
            Map.lookup headStableName actual `shouldBe` Just headIdentity

        it "keeps replacement type head identities by payload qualified name after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 992507)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                headIdentity = generatedSymbolIdentity 992508 SymbolType "Main" "Token" Nothing
                qualifiedHeadName = "Main.Token"
                sourceView =
                    (setTypeViewBinderIdentities (Map.singleton "x" sourceIdentity) (mkTypeView (STVar "x") (STVar sourceStableName)))
                replacement =
                    (setTypeViewHeadIdentities (Map.singleton "Token" headIdentity) (mkTypeView (STBase qualifiedHeadName) (STBase qualifiedHeadName)))
                subst =
                    Map.singleton sourceIdentity replacement
            let actual = ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
            Map.lookup "Token" actual `shouldBe` Just headIdentity
            Map.lookup qualifiedHeadName actual `shouldBe` Just headIdentity
            Map.lookup (symbolIdentityStableName headIdentity) actual `shouldBe` Just headIdentity

        it "resolves type head identities through payload qualified aliases" $ do
            let headIdentity = generatedSymbolIdentity 992509 SymbolType "Main" "Token" Nothing
                view =
                    (setTypeViewHeadIdentities (Map.singleton "Token" headIdentity) (mkTypeView (STBase "Main.Token") (STBase "Main.Token")))
            ProgramTypes.typeViewHeadIdentityForAlias view "Main.Token"
                `shouldBe` Just headIdentity

        it "keeps paired display aliases when they select one type head identity" $ do
            let headIdentity = generatedSymbolIdentity 992521 SymbolType "Main" "Token" Nothing
                headStableName = symbolIdentityStableName headIdentity
                view =
                    ( setTypeViewHeadIdentities
                        (Map.singleton headStableName headIdentity)
                        ( mkTypeView
                            (STArrow (STBase "LeftToken") (STBase "RightToken"))
                            (STArrow (STBase headStableName) (STBase headStableName))
                        )
                    )
            ProgramTypes.typeViewHeadIdentityForAlias view "LeftToken" `shouldBe` Just headIdentity
            ProgramTypes.typeViewHeadIdentityForAlias view "RightToken" `shouldBe` Just headIdentity
            ProgramTypes.typeViewHeadIdentityForAlias view headStableName
                `shouldBe` Just headIdentity

        it "keeps replacement binder identities by display key after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 991431)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                replacementIdentity = typeBinderIdentityFromNode (NodeId 991432)
                replacementStableName = typeBinderIdentityStableName replacementIdentity
                sourceView =
                    (setTypeViewBinderIdentities (Map.singleton "x" sourceIdentity) (mkTypeView (STVar "x") (STVar sourceStableName)))
                replacement =
                    (setTypeViewBinderIdentities (Map.singleton "y" replacementIdentity) (mkTypeView (STVar "y") (STVar replacementStableName)))
                subst =
                    Map.singleton sourceIdentity replacement
            let actual = ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
            Map.lookup "y" actual `shouldBe` Just replacementIdentity
            Map.lookup replacementStableName actual `shouldBe` Just replacementIdentity

        it "keeps replacement binder identities by payload stable name after applying type-view substitutions" $ do
            let sourceIdentity = typeBinderIdentityFromNode (NodeId 992503)
                sourceStableName = typeBinderIdentityStableName sourceIdentity
                replacementIdentity = typeBinderIdentityFromNode (NodeId 992504)
                replacementStableName = typeBinderIdentityStableName replacementIdentity
                sourceView =
                    (setTypeViewBinderIdentities (Map.singleton "x" sourceIdentity) (mkTypeView (STVar "x") (STVar sourceStableName)))
                replacement =
                    (setTypeViewBinderIdentities (Map.singleton "y" replacementIdentity) (mkTypeView (STVar replacementStableName) (STVar replacementStableName)))
                subst =
                    Map.singleton sourceIdentity replacement
            let actual = ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
            Map.lookup "y" actual `shouldBe` Just replacementIdentity
            Map.lookup replacementStableName actual `shouldBe` Just replacementIdentity

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

        it "keeps the semantic binder payload authoritative across a display overlay" $ do
            let firstIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991624)
                secondIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991625)
                firstStableName = typeBinderIdentityStableName firstIdentity
                secondStableName = typeBinderIdentityStableName secondIdentity
            case
                ProgramTypes.typeViewFromSourceType
                    Map.empty
                    (Map.fromList [("x", firstIdentity), (firstStableName, secondIdentity)])
                    (STVar firstStableName)
                of
                    Right identityView ->
                        case ProgramTypes.typeViewWithDisplay (STVar "x") identityView of
                            Right view -> do
                                ProgramTypes.typeViewIdentity view `shouldBe` STVar secondStableName
                                Map.lookup secondStableName (ProgramTypes.typeViewBinderIdentities view)
                                    `shouldBe` Just secondIdentity
                            Left err ->
                                expectationFailure ("expected shape-preserving display overlay, got " ++ show err)
                    Left err ->
                        expectationFailure ("expected source-shape identity payload, got " ++ show err)

        it "pairs display aliases through stable type-binder identity keys" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991626)
                stableName = typeBinderIdentityStableName identity
                view =
                    (setTypeViewBinderIdentities (Map.singleton stableName identity) (mkTypeView (STVar "display") (STVar stableName)))
                aliases =
                    Map.fromList (ProgramTypes.typeViewBinderIdentityAliasEntries view)
            Map.lookup "display" aliases `shouldBe` Just identity
            Map.lookup stableName aliases `shouldBe` Just identity

        it "compares type-binder substitutions by identity targets" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991616)
                replacementTy = STBase "Int"
                subst =
                    ProgramTypes.insertTypeBinderSubstView
                        identity
                        (mkTypeView replacementTy replacementTy)
                        ProgramTypes.emptyTypeBinderSubst
            ProgramTypes.lookupTypeBinderSubstViewByIdentity identity subst
                `shouldBe` Just (mkTypeView replacementTy replacementTy)

        it "does not infer identity-bearing type-binder substitutions from missing identity keys" $ do
            let identity = typeBinderIdentityFromNode (NodeId 991304)
                identitySubst =
                    ProgramTypes.typeBinderSubstFromTypeViewSubst
                        Map.empty
            ProgramTypes.lookupTypeBinderSubstViewByIdentity identity identitySubst
                `shouldBe` Nothing

        it "rejects TypeView variables without identity payloads before matching" $ do
            ProgramTypes.typeViewFromSourceType
                Map.empty
                Map.empty
                (STVar "a")
                `shouldSatisfy` isLeft

        it "does not let method display matching override conflicting head identities" $ do
            let leftIdentity = generatedSymbolIdentity 991733 SymbolType "Left" "Token" Nothing
                rightIdentity = generatedSymbolIdentity 991734 SymbolType "Right" "Token" Nothing
                leftStableName = symbolIdentityStableName leftIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (setTypeViewHeadIdentities (Map.fromList [("Token", leftIdentity), (leftStableName, leftIdentity)]) (mkTypeView (STBase "Token") (STBase leftStableName)))
                staleAlias =
                    (setTypeViewHeadIdentities (Map.fromList [("$stale_Token", leftIdentity), (leftStableName, leftIdentity)]) (mkTypeView (STBase "$stale_Token") (STBase leftStableName)))
                conflicting =
                    ( setTypeViewHeadIdentities
                        ( Map.fromList
                            [ ("Token", rightIdentity)
                            , (symbolIdentityStableName rightIdentity, rightIdentity)
                            ]
                        )
                        (mkTypeView (STBase "Token") (STBase (symbolIdentityStableName rightIdentity)))
                    )
            matchMethodTypeViews scope Map.empty (template :| []) (staleAlias :| [])
                `shouldBe` Just Map.empty
            matchMethodTypeViews scope Map.empty (template :| []) (conflicting :| [])
                `shouldBe` Nothing

        it "skips bare type-view self-substitutions by binder identity when names are stale" $ do
            let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991628)
                stableName = typeBinderIdentityStableName binderIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (setTypeViewBinderIdentities (Map.singleton "a" binderIdentity) (mkTypeView (STVar "a") (STVar stableName)))
                actual =
                    (setTypeViewBinderIdentities (Map.singleton "$stale_b" binderIdentity) (mkTypeView (STVar "b") (STVar "$stale_b")))
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldBe` Just Map.empty

        it "rejects recursive bare type-view substitutions by binder identity when names are stale" $ do
            let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991629)
                stableName = typeBinderIdentityStableName binderIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (setTypeViewBinderIdentities (Map.singleton "a" binderIdentity) (mkTypeView (STVar "a") (STVar stableName)))
                actual =
                    ( setTypeViewBinderIdentities
                        (Map.singleton "$stale_f" binderIdentity)
                        ( mkTypeView
                            (STVarApp "f" (STBase "Int" :| []))
                            (STVarApp "$stale_f" (STBase "Int" :| []))
                        )
                    )
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldBe` Nothing

        it "rejects repeated type-view substitutions with same display head and different identities" $ do
            let binderIdentity = typeBinderIdentityFromNode (NodeId 992516)
                binderStableName = typeBinderIdentityStableName binderIdentity
                leftHeadIdentity = generatedSymbolIdentity 992517 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 992518 SymbolType "Right" "Token" Nothing
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (setTypeViewBinderIdentities (Map.singleton binderStableName binderIdentity) (mkTypeView (STVar "a") (STVar binderStableName)))
                actual identity =
                    (setTypeViewHeadIdentities (Map.singleton "Token" identity) (mkTypeView (STBase "Token") (STBase "Token")))
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
                    ( setTypeViewBinderIdentities
                        (Map.fromList [("f", headIdentity), ("a", argIdentity)])
                        ( mkTypeView
                            (STVarApp "f" (STVar "a" :| []))
                            (STVarApp headStableName (STVar argStableName :| []))
                        )
                    )
                actual =
                    ( setTypeViewBinderIdentities
                        (Map.singleton "$stale_f" headIdentity)
                        ( mkTypeView
                            (STVarApp "g" (STBase "Int" :| []))
                            (STVarApp "$stale_f" (STBase "Int" :| []))
                        )
                    )
                headKey = headIdentity
                argKey = argIdentity
            case matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| []) of
                Just subst -> do
                    Map.member headKey subst `shouldBe` False
                    fmap ProgramTypes.typeViewIdentity (Map.lookup argKey subst)
                        `shouldBe` Just (ProgramTypes.typeViewIdentity (builtinBaseTypeView "Int"))
                Nothing ->
                    expectationFailure "expected argument substitution"

        it "keeps replacement binder identities after applying type-view substitutions" $ do
            let originalIdentity = typeBinderIdentityFromNode (NodeId 991411)
                replacementIdentity = typeBinderIdentityFromNode (NodeId 991412)
                sourceView =
                    (setTypeViewBinderIdentities (Map.singleton "$a" originalIdentity) (mkTypeView (STVar "a") (STVar "$a")))
                replacement =
                    (setTypeViewBinderIdentities (Map.singleton "$b" replacementIdentity) (mkTypeView (STVar "b") (STVar "$b")))
                subst =
                    Map.singleton
                        originalIdentity
                        replacement
            let actual = ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
            Map.lookup "$b" actual `shouldBe` Just replacementIdentity
            Map.lookup "b" actual `shouldBe` Just replacementIdentity
            Map.lookup (typeBinderIdentityStableName replacementIdentity) actual
                `shouldBe` Just replacementIdentity

        it "drops ambiguous replacement binder display identities after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991415)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991416)
                leftReplacementIdentity = typeBinderIdentityFromNode (NodeId 991417)
                rightReplacementIdentity = typeBinderIdentityFromNode (NodeId 991418)
                sourceView =
                    (setTypeViewBinderIdentities (Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]) (mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y"))))
                leftReplacement =
                    (setTypeViewBinderIdentities (Map.singleton "a" leftReplacementIdentity) (mkTypeView (STVar "a") (STVar "a")))
                rightReplacement =
                    (setTypeViewBinderIdentities (Map.singleton "a" rightReplacementIdentity) (mkTypeView (STVar "a") (STVar "a")))
                subst =
                    Map.fromList
                        [ (leftSourceIdentity, leftReplacement)
                        , (rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.fromList
                    [ (typeBinderIdentityStableName leftReplacementIdentity, leftReplacementIdentity)
                    , (typeBinderIdentityStableName rightReplacementIdentity, rightReplacementIdentity)
                    ]

        it "keeps stable binder aliases but drops ambiguous direct display keys after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991438)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991439)
                leftReplacementIdentity = typeBinderIdentityFromNode (NodeId 991440)
                rightReplacementIdentity = typeBinderIdentityFromNode (NodeId 991441)
                leftStableName = typeBinderIdentityStableName leftReplacementIdentity
                rightStableName = typeBinderIdentityStableName rightReplacementIdentity
                sourceView =
                    (setTypeViewBinderIdentities (Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]) (mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y"))))
                leftReplacement =
                    (setTypeViewBinderIdentities (Map.singleton "a" leftReplacementIdentity) (mkTypeView (STVar "a") (STVar leftStableName)))
                rightReplacement =
                    (setTypeViewBinderIdentities (Map.singleton rightStableName rightReplacementIdentity) (mkTypeView (STVar "a") (STVar rightStableName)))
                subst =
                    Map.fromList
                        [ (leftSourceIdentity, leftReplacement)
                        , (rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewBinderIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.fromList [(leftStableName, leftReplacementIdentity), (rightStableName, rightReplacementIdentity)]

        it "drops ambiguous replacement type head display identities after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991419)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991420)
                leftHeadIdentity = generatedSymbolIdentity 991421 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 991422 SymbolType "Right" "Token" Nothing
                sourceView =
                    (setTypeViewBinderIdentities (Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]) (mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y"))))
                leftReplacement =
                    (setTypeViewHeadIdentities (Map.singleton "Token" leftHeadIdentity) (mkTypeView (STBase "Token") (STBase "Token")))
                rightReplacement =
                    (setTypeViewHeadIdentities (Map.singleton "Token" rightHeadIdentity) (mkTypeView (STBase "Token") (STBase "Token")))
                subst =
                    Map.fromList
                        [ (leftSourceIdentity, leftReplacement)
                        , (rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.fromList
                    [ (symbolIdentityStableName leftHeadIdentity, leftHeadIdentity)
                    , (symbolIdentityStableName rightHeadIdentity, rightHeadIdentity)
                    ]

        it "keeps stable type head aliases but drops ambiguous direct display keys after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991442)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991443)
                leftHeadIdentity = generatedSymbolIdentity 991444 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 991445 SymbolType "Right" "Token" Nothing
                leftStableName = symbolIdentityStableName leftHeadIdentity
                rightStableName = symbolIdentityStableName rightHeadIdentity
                sourceView =
                    (setTypeViewBinderIdentities (Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]) (mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y"))))
                leftReplacement =
                    (setTypeViewHeadIdentities (Map.singleton "Token" leftHeadIdentity) (mkTypeView (STBase "Token") (STBase leftStableName)))
                rightReplacement =
                    (setTypeViewHeadIdentities (Map.singleton rightStableName rightHeadIdentity) (mkTypeView (STBase "Token") (STBase rightStableName)))
                subst =
                    Map.fromList
                        [ (leftSourceIdentity, leftReplacement)
                        , (rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.fromList [(leftStableName, leftHeadIdentity), (rightStableName, rightHeadIdentity)]

        it "drops ambiguous replacement type head stable aliases after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991430)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991431)
                leftHeadIdentity = generatedSymbolIdentity 991432 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 991433 SymbolType "Right" "Token" Nothing
                sourceView =
                    (setTypeViewBinderIdentities (Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]) (mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y"))))
                leftReplacement =
                    (setTypeViewHeadIdentities (Map.singleton (symbolIdentityStableName leftHeadIdentity) leftHeadIdentity) (mkTypeView (STBase "Token") (STBase "Token")))
                rightReplacement =
                    (setTypeViewHeadIdentities (Map.singleton (symbolIdentityStableName rightHeadIdentity) rightHeadIdentity) (mkTypeView (STBase "Token") (STBase "Token")))
                subst =
                    Map.fromList
                        [ (leftSourceIdentity, leftReplacement)
                        , (rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.fromList
                    [ (symbolIdentityStableName leftHeadIdentity, leftHeadIdentity)
                    , (symbolIdentityStableName rightHeadIdentity, rightHeadIdentity)
                    ]

        it "drops mixed direct and stable ambiguous type head aliases after applying type-view substitutions" $ do
            let leftSourceIdentity = typeBinderIdentityFromNode (NodeId 991434)
                rightSourceIdentity = typeBinderIdentityFromNode (NodeId 991435)
                leftHeadIdentity = generatedSymbolIdentity 991436 SymbolType "Left" "Token" Nothing
                rightHeadIdentity = generatedSymbolIdentity 991437 SymbolType "Right" "Token" Nothing
                sourceView =
                    (setTypeViewBinderIdentities (Map.fromList [("$x", leftSourceIdentity), ("$y", rightSourceIdentity)]) (mkTypeView (STArrow (STVar "x") (STVar "y")) (STArrow (STVar "$x") (STVar "$y"))))
                leftReplacement =
                    (setTypeViewHeadIdentities (Map.singleton "Token" leftHeadIdentity) (mkTypeView (STBase "Token") (STBase "Token")))
                rightReplacement =
                    (setTypeViewHeadIdentities (Map.singleton (symbolIdentityStableName rightHeadIdentity) rightHeadIdentity) (mkTypeView (STBase "Token") (STBase "Token")))
                subst =
                    Map.fromList
                        [ (leftSourceIdentity, leftReplacement)
                        , (rightSourceIdentity, rightReplacement)
                        ]
            ProgramTypes.typeViewHeadIdentities (ProgramTypes.applyTypeViewSubst subst sourceView)
                `shouldBe` Map.fromList
                    [ (symbolIdentityStableName leftHeadIdentity, leftHeadIdentity)
                    , (symbolIdentityStableName rightHeadIdentity, rightHeadIdentity)
                    ]

        it "drops conflicting merged type head payloads with the same symbol identity" $ do
            let stableName = symbolIdentityStableName originalIdentity
                originalIdentity = generatedSymbolIdentity 991629 SymbolType "Lib" "Token" Nothing
                conflictingIdentity = generatedSymbolIdentity 991629 SymbolType "Other" "StaleToken" Nothing
                aliases =
                    ProgramTypes.mergeSymbolIdentityMaps
                        [ Map.singleton stableName originalIdentity
                        , Map.singleton stableName conflictingIdentity
                        ]
            Map.lookup stableName aliases `shouldBe` Nothing

        it "drops unique info entries when one symbol identity has conflicting payloads" $ do
            let originalIdentity = generatedSymbolIdentity 991658 SymbolValue "Lib" "answer" Nothing
                conflictingIdentity = generatedSymbolIdentity 991658 SymbolValue "Other" "staleAnswer" Nothing
                infos =
                    ProgramTypes.uniqueInfoEntriesByIdentity
                        [(originalIdentity, "same"), (conflictingIdentity, "same")]
            Map.lookup originalIdentity infos `shouldBe` Nothing

        it "drops unique display names when one symbol identity has conflicting payloads" $ do
            let originalIdentity = generatedSymbolIdentity 991661 SymbolType "Lib" "Token" Nothing
                conflictingIdentity = generatedSymbolIdentity 991661 SymbolType "Other" "StaleToken" Nothing
                names =
                    ProgramTypes.uniqueDisplayNamesByIdentity
                        [(originalIdentity, "Token"), (conflictingIdentity, "Token")]
            Map.lookup originalIdentity names `shouldBe` Nothing

        it "drops exported constructor metadata when one constructor symbol payload conflicts" $ do
            let dataIdentity = generatedSymbolIdentity 991659 SymbolType "Lib" "Token" Nothing
                originalCtorIdentity =
                    generatedSymbolIdentity 991660 SymbolConstructor "Lib" "Some" (Just (SymbolOwnerType dataIdentity))
                conflictingCtorIdentity =
                    generatedSymbolIdentity 991660 SymbolConstructor "Other" "StaleSome" (Just (SymbolOwnerType dataIdentity))
                ctorInfo identity =
                    ConstructorInfo
                        { ctorInfoSymbol = identity
                        , ctorRuntimeName = "Lib__Some"
                        , ctorTypeView = mkTypeView (STBase "Token") (STBase "Token")
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                exportedType =
                    ProgramTypes.mkExportedTypeInfo
                        (DataInfo dataIdentity [] [])
                        [ ("Some", ctorInfo originalCtorIdentity)
                        , ("Some", ctorInfo conflictingCtorIdentity)
                        ]
            Map.lookup originalCtorIdentity (ProgramTypes.exportedTypeConstructorsByIdentity exportedType) `shouldBe` Nothing
            Map.lookup originalCtorIdentity (ProgramTypes.exportedTypeConstructorDisplaysByIdentity exportedType) `shouldBe` Nothing
            ProgramTypes.exportedTypeConstructorsForDisplay exportedType `shouldBe` Map.empty

        it "does not compare deferred constructors equal when type head payloads conflict" $ do
            let dataIdentity = generatedSymbolIdentity 991637 SymbolType "Lib" "Token" Nothing
                ctorIdentity = generatedSymbolIdentity 991638 SymbolConstructor "Lib" "MkToken" (Just (SymbolOwnerType dataIdentity))
                originalHeadIdentity = generatedSymbolIdentity 991639 SymbolType "Lib" "Token" Nothing
                conflictingHeadIdentity = generatedSymbolIdentity 991639 SymbolType "Other" "StaleToken" Nothing
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Lib__MkToken"
                        , ctorTypeView = mkTypeView (STBase "Token") (STBase "Token")
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                deferred identity =
                    DeferredConstructorCall
                        { deferredConstructorRef = deferredRefFromIdentity (UniqueIdentity 991640) "$ctor"
                        , deferredConstructorInfo = ctorInfo
                        , deferredConstructorArgCount = 0
                        , deferredConstructorSourceTypeView = baseTypeView "Token" identity
                        , deferredConstructorOccurrenceTypeView = baseTypeView "Token" identity
                        , deferredConstructorInstBinders = []
                        , deferredConstructorInitialSubst = ProgramTypes.emptyTypeBinderSubst
                        , deferredConstructorBindingMode = ProgramTypes.DeferredBindingMonomorphic
                        }
            deferred originalHeadIdentity `shouldNotBe` deferred conflictingHeadIdentity

        it "drops ambiguous constraint binder display identities from ordinary value views" $ do
            let valueIdentity = generatedSymbolIdentity 991423 SymbolValue "Main" "f" Nothing
                classIdentity = generatedSymbolIdentity 991424 SymbolClass "Main" "C" Nothing
                leftIdentity = typeBinderIdentityFromNode (NodeId 991425)
                rightIdentity = typeBinderIdentityFromNode (NodeId 991426)
                constraintView identity =
                    (setTypeViewBinderIdentities (Map.singleton "a" identity) (mkTypeView (STVar "a") (STVar "a")))
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
                        , valueTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , valueConstraintInfos = [constraintInfo leftIdentity, constraintInfo rightIdentity]
                        }
            let actual = ProgramTypes.typeViewBinderIdentities (ProgramTypes.ordinaryValueTypeView valueInfo)
            Map.lookup "a" actual `shouldBe` Nothing
            Map.lookup (typeBinderIdentityStableName leftIdentity) actual `shouldBe` Just leftIdentity
            Map.lookup (typeBinderIdentityStableName rightIdentity) actual `shouldBe` Just rightIdentity

        it "drops ambiguous value binder display identities from ordinary value views" $ do
            let valueIdentity = generatedSymbolIdentity 991428 SymbolValue "Main" "f" Nothing
                leftIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991429)
                rightIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991430)
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__f"
                        , valueTypeView =
                            ( setTypeViewBinderIdentities
                                ( Map.fromList
                                    [ ("$typevar#991429", leftIdentity)
                                    , ("$typevar#991430", rightIdentity)
                                    ]
                                )
                                ( mkTypeView
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
                            )
                        , valueConstraintInfos = []
                        }
            let actual = ProgramTypes.typeViewBinderIdentities (ProgramTypes.ordinaryValueTypeView valueInfo)
            Map.lookup "a" actual `shouldBe` Nothing
            Map.lookup "$typevar#991429" actual `shouldBe` Just leftIdentity
            Map.lookup "$typevar#991430" actual `shouldBe` Just rightIdentity

        it "carries runtime type binder identities in elaborate scope" $ do
            let valueIdentity = generatedSymbolIdentity 991431 SymbolValue "Main" "id" Nothing
                binderIdentity = typeBinderIdentityFromNode (NodeId 991432)
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__id"
                        , valueTypeView =
                            ( setTypeViewBinderIdentities
                                (Map.singleton "a" binderIdentity)
                                ( mkTypeView
                                    (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                                    (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                                )
                            )
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
                    (setTypeViewBinderIdentities (Map.singleton localStableName localIdentity) (mkTypeView (STVar displayName) (STVar localStableName)))
                evidenceParamView displayName =
                    (setTypeViewBinderIdentities (Map.singleton evidenceParamStableName evidenceParamIdentity) (mkTypeView (STVar displayName) (STVar evidenceParamStableName)))
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
                        , methodConstraintInfos = [methodConstraint]
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                classInfo =
                    ClassInfo
                        { classInfoSymbol = classIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity classParamIdentity "a") KType :| []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton methodIdentity methodInfo
                        }
                evidenceMethodInfo =
                    MethodInfo
                        { methodInfoSymbol = evidenceMethodIdentity
                        , methodDisplayName = "witness"
                        , methodTypeViewRaw = evidenceParamView "d"
                        , methodConstraintInfos = []
                        , methodParamBinders = ("d", evidenceParamIdentity) :| []
                        }
                evidenceClassInfo =
                    ClassInfo
                        { classInfoSymbol = evidenceClassIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity evidenceParamIdentity "d") KType :| []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton evidenceMethodIdentity evidenceMethodInfo
                        }
                valueConstraint =
                    ProgramTypes.ConstraintInfo
                        { ProgramTypes.constraintDisplayClass = "C"
                        , ProgramTypes.constraintClassSymbol = classIdentity
                        , ProgramTypes.constraintTypeViews = mkTypeView (STBase "Int") (STBase "Int") :| []
                        }
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__use"
                        , valueTypeView = mkTypeView (STBase "Bool") (STBase "Bool")
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
                    (setTypeViewBinderIdentities (Map.singleton localStableName localIdentity) (mkTypeView (STVar displayName) (STVar localStableName)))
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
                        , methodTypeViewRaw = mkTypeView (STBase "Bool") (STBase "Bool")
                        , methodConstraintInfos = [methodConstraint "c", methodConstraint "d"]
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                classInfo =
                    ClassInfo
                        { classInfoSymbol = classIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity classParamIdentity "a") KType :| []
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
                            (setTypeViewBinderIdentities (Map.singleton evidenceParamStableName evidenceParamIdentity) (mkTypeView (STVar "e") (STVar evidenceParamStableName)))
                        , methodConstraintInfos = []
                        , methodParamBinders = ("e", evidenceParamIdentity) :| []
                        }
                evidenceClassInfo =
                    ClassInfo
                        { classInfoSymbol = evidenceClassIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity evidenceParamIdentity "e") KType :| []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton evidenceMethodIdentity evidenceMethodInfo
                        }
                valueConstraint =
                    ProgramTypes.ConstraintInfo
                        { ProgramTypes.constraintDisplayClass = "C2"
                        , ProgramTypes.constraintClassSymbol = classIdentity
                        , ProgramTypes.constraintTypeViews = mkTypeView (STBase "Int") (STBase "Int") :| []
                        }
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__ambiguous"
                        , valueTypeView = mkTypeView (STBase "Bool") (STBase "Bool")
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
                            (setTypeViewHeadIdentities (Map.singleton staleHead typeIdentity) (mkTypeView (STBase "Box") (STBase staleHead)))
                        , valueConstraintInfos = []
                        }
                scope = mkElaborateScope (Map.singleton "box" valueInfo) Map.empty Map.empty []
            (Map.lookup "Main__box" (elaborateScopeRuntimeTypeViews scope) >>= Map.lookup staleHead . ProgramTypes.typeViewHeadIdentities)
                `shouldBe` Just typeIdentity

        it "does not classify runtime values as instance methods through stale identity payloads" $ do
            let valueIdentity = generatedSymbolIdentity 991662 SymbolValue "Main" "box" Nothing
                staleMethodValueIdentity = renameSymbolDefiningName "$stale_box_method" valueIdentity
                typeIdentity = generatedSymbolIdentity 991663 SymbolType "Main" "Box" Nothing
                classIdentity = generatedSymbolIdentity 991664 SymbolClass "Main" "C" Nothing
                originIdentity = generatedSymbolIdentity 991665 SymbolModule "Main" "Main" Nothing
                methodIdentity = generatedSymbolIdentity 991666 SymbolMethod "Main" "method" (Just (SymbolOwnerClass classIdentity))
                boxData =
                    DataInfo
                        { dataInfoSymbol = typeIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__box"
                        , valueTypeView = baseTypeView "Box" typeIdentity
                        , valueConstraintInfos = []
                        }
                methodValue =
                    OrdinaryValue
                        { valueInfoSymbol = staleMethodValueIdentity
                        , valueRuntimeName = "Main__method"
                        , valueTypeView = builtinBaseTypeView "Int"
                        , valueConstraintInfos = []
                        }
                instanceInfo =
                    InstanceInfo
                        { instanceClassSymbol = classIdentity
                        , instanceOriginModuleIdentity = originIdentity
                        , instanceConstraintInfos = []
                        , instanceHeadTypeViews = mkTypeView (STBase "Int") (STBase "Int") :| []
                        , instanceMethodsByIdentity = Map.singleton methodIdentity methodValue
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "box" valueInfo)
                        (Map.singleton "Box" boxData)
                        Map.empty
                        [instanceInfo]
            fmap ProgramTypes.typeViewDisplay (Map.lookup "Main__box" (elaborateScopeRuntimeTypeViews scope))
                `shouldBe` Just (lowerType scope (STBase "Box"))

        it "does not keep an arbitrary runtime type view for duplicate runtime-name values" $ do
            let runtimeName = "Main__shared"
                leftIdentity = generatedSymbolIdentity 991438 SymbolValue "Main" "left" Nothing
                rightIdentity = generatedSymbolIdentity 991439 SymbolValue "Main" "right" Nothing
                valueInfo identity ty =
                    OrdinaryValue
                        { valueInfoSymbol = identity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = mkTypeView ty ty
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
                        , valueTypeView = mkTypeView ty ty
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

        it "does not keep an arbitrary runtime type view when one value symbol payload conflicts" $ do
            let runtimeName = "Main__shared"
                sharedIdentity = generatedSymbolIdentity 991657 SymbolValue "Main" "shared" Nothing
                conflictingIdentity = generatedSymbolIdentity 991657 SymbolValue "Other" "staleShared" Nothing
                valueInfo identity =
                    OrdinaryValue
                        { valueInfoSymbol = identity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        ( Map.fromList
                            [ ("left", valueInfo sharedIdentity)
                            , ("right", valueInfo conflictingIdentity)
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
                        , valueTypeView = mkTypeView ty ty
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
                (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                (resolvedUnconstrainedType (RSTBase (Builtins.builtinTypeSymbol "Bool")))
                False
                expr
                `shouldBe` Left (ProgramUnknownValue "left")

        it "rejects same-spelled conflicting type identities through resolved lets, annotations, method arguments, and patterns" $ do
            let expectedTypeIdentity = generatedSymbolIdentity 991850 SymbolType "Expected" "Token" Nothing
                actualTypeIdentity = generatedSymbolIdentity 991851 SymbolType "Actual" "Token" Nothing
                expectedCtorIdentity =
                    generatedSymbolIdentity
                        991852
                        SymbolConstructor
                        "Expected"
                        "MkToken"
                        (Just (SymbolOwnerType expectedTypeIdentity))
                actualValueIdentity = generatedSymbolIdentity 991853 SymbolValue "Actual" "actual" Nothing
                bindingIdentity = generatedSymbolIdentity 991854 SymbolValue "Main" "main" Nothing
                classIdentity = generatedSymbolIdentity 991856 SymbolClass "Expected" "Pick" Nothing
                methodIdentity = generatedSymbolIdentity 991857 SymbolMethod "Expected" "pick" (Just (SymbolOwnerClass classIdentity))
                classParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991858)
                instanceMethodIdentity = generatedSymbolIdentity 991859 SymbolValue "Expected" "pickToken" Nothing
                instanceOriginIdentity = generatedSymbolIdentity 991860 SymbolModule "Expected" "Expected" Nothing
                expectedBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991861)
                actualBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991862)
                actualBinderValueIdentity = generatedSymbolIdentity 991863 SymbolValue "Actual" "actualBinder" Nothing
                classParamStableName = typeBinderIdentityStableName classParamIdentity
                expectedCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = expectedCtorIdentity
                        , ctorRuntimeName = "Expected__MkToken"
                        , ctorTypeView = baseTypeView "Token" expectedTypeIdentity
                        , ctorOwningTypeIdentity = expectedTypeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                expectedData =
                    DataInfo
                        { dataInfoSymbol = expectedTypeIdentity
                        , dataTypeParams = []
                        , dataConstructors = [expectedCtor]
                        }
                actualValue =
                    OrdinaryValue
                        { valueInfoSymbol = actualValueIdentity
                        , valueRuntimeName = "Actual__actual"
                        , valueTypeView = baseTypeView "Token" actualTypeIdentity
                        , valueConstraintInfos = []
                        }
                expectedView = baseTypeView "Token" expectedTypeIdentity
                instanceMethodValue =
                    OrdinaryValue
                        { valueInfoSymbol = instanceMethodIdentity
                        , valueRuntimeName = "Expected__pickToken"
                        , valueTypeView =
                            ( setTypeViewHeadIdentities
                                (ProgramTypes.typeViewHeadIdentities expectedView)
                                ( mkTypeView
                                    (STArrow (STBase "Token") (STBase "Token"))
                                    ( STArrow
                                        (STBase (symbolIdentityStableName expectedTypeIdentity))
                                        (STBase (symbolIdentityStableName expectedTypeIdentity))
                                    )
                                )
                            )
                        , valueConstraintInfos = []
                        }
                instanceInfo =
                    InstanceInfo
                        { instanceClassSymbol = classIdentity
                        , instanceOriginModuleIdentity = instanceOriginIdentity
                        , instanceConstraintInfos = []
                        , instanceHeadTypeViews = expectedView :| []
                        , instanceMethodsByIdentity = Map.singleton methodIdentity instanceMethodValue
                        }
                actualBinderStableName = typeBinderIdentityStableName actualBinderIdentity
                actualBinderValue =
                    OrdinaryValue
                        { valueInfoSymbol = actualBinderValueIdentity
                        , valueRuntimeName = "Actual__actualBinder"
                        , valueTypeView =
                            ( setTypeViewBinderIdentities
                                ( Map.fromList
                                    [ ("a", actualBinderIdentity)
                                    , (actualBinderStableName, actualBinderIdentity)
                                    ]
                                )
                                (mkTypeView (STVar "a") (STVar actualBinderStableName))
                            )
                        , valueConstraintInfos = []
                        }
                methodView =
                    ( setTypeViewBinderIdentities
                        ( Map.fromList
                            [ ("a", classParamIdentity)
                            , (classParamStableName, classParamIdentity)
                            ]
                        )
                        ( mkTypeView
                            (STArrow (STVar "a") (STVar "a"))
                            (STArrow (STVar classParamStableName) (STVar classParamStableName))
                        )
                    )
                methodInfo =
                    MethodInfo
                        { methodInfoSymbol = methodIdentity
                        , methodDisplayName = "pick"
                        , methodTypeViewRaw = methodView
                        , methodConstraintInfos = []
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                classInfo =
                    ClassInfo
                        { classInfoSymbol = classIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity classParamIdentity "a") KType :| []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton methodIdentity methodInfo
                        }
                scope =
                    mkElaborateScope
                        ( Map.fromList
                            [ ("actual", actualValue)
                            ,
                                ( "actualBinder"
                                , actualBinderValue
                                )
                            ,
                                ( "MkToken"
                                , ConstructorValue
                                    { valueInfoSymbol = expectedCtorIdentity
                                    , valueRuntimeName = "Expected__MkToken"
                                    , valueCtorInfo = expectedCtor
                                    }
                                )
                            , ("pick", OverloadedMethod methodIdentity methodInfo)
                            ]
                        )
                        (Map.singleton "Token" expectedData)
                        (Map.singleton "Pick" classInfo)
                        [instanceInfo]
                expectedTypeSymbol =
                    ProgramTypes.resolvedDataInfoSymbol
                        (SymbolLocal "Expected")
                        "Token"
                        expectedData
                expectedCtorSymbol =
                    ProgramTypes.resolvedConstructorInfoSymbol
                        (SymbolLocal "Expected")
                        "MkToken"
                        expectedData
                        expectedCtor
                actualValueSymbol =
                    ProgramTypes.resolvedValueInfoSymbol
                        (SymbolLocal "Actual")
                        "actual"
                        actualValue
                methodSymbol =
                    ProgramTypes.resolvedMethodInfoSymbol
                        (SymbolLocal "Expected")
                        "pick"
                        methodInfo
                actualBinderValueSymbol =
                    ProgramTypes.resolvedValueInfoSymbol
                        (SymbolLocal "Actual")
                        "actualBinder"
                        actualBinderValue
                expectedTy = RSTBase expectedTypeSymbol
                actualExpr = EVar (ResolvedGlobalValue actualValueSymbol)
                localRef = localRefFromNodeId "x" (NodeId 991855)
                letExpr =
                    ELet
                        localRef
                        (Just expectedTy)
                        (EAnn actualExpr expectedTy)
                        (EVar (ResolvedLocalValue localRef))
                caseExpr =
                    ECase
                        actualExpr
                        [Alt (PatCtor expectedCtorSymbol []) (ELit (LInt 1))]
                methodExpr =
                    EApp
                        (EVar (ResolvedGlobalValue methodSymbol))
                        actualExpr
                expectedBinderRef = resolvedTypeBinderRefFromIdentity expectedBinderIdentity "a"
                actualBinderExpr = EVar (ResolvedGlobalValue actualBinderValueSymbol)
                lower expected expr =
                    lowerResolvedConstrainedExprBinding
                        scope
                        (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                        (resolvedUnconstrainedType expected)
                        False
                        expr
            lower expectedTy letExpr
                `shouldBe` Left (ProgramTypeMismatch (STBase "Token") (STBase "Token"))
            lower (RSTBase (Builtins.builtinTypeSymbol "Int")) caseExpr
                `shouldBe` Left (ProgramPatternConstructorMismatch "MkToken" (STBase "Token"))
            lower expectedTy methodExpr
                `shouldBe` Left (ProgramTypeMismatch (STBase "Token") (STBase "Token"))
            lower (RSTVar expectedBinderRef) actualBinderExpr
                `shouldBe` Left (ProgramTypeMismatch (STVar "a") (STVar "a"))

        it "keeps argument-inferred type identities in resolved method placeholders" $ do
            let expectedTypeIdentity = generatedSymbolIdentity 991870 SymbolType "Expected" "Token" Nothing
                actualTypeIdentity = generatedSymbolIdentity 991871 SymbolType "Actual" "Token" Nothing
                actualValueIdentity = generatedSymbolIdentity 991872 SymbolValue "Actual" "actual" Nothing
                classIdentity = generatedSymbolIdentity 991873 SymbolClass "Main" "Inspect" Nothing
                methodIdentity = generatedSymbolIdentity 991874 SymbolMethod "Main" "inspect" (Just (SymbolOwnerClass classIdentity))
                bindingIdentity = generatedSymbolIdentity 991875 SymbolValue "Main" "main" Nothing
                classParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991876)
                classParamStableName = typeBinderIdentityStableName classParamIdentity
                intIdentity = Builtins.builtinTypeIdentity "Int"
                intStableName = symbolIdentityStableName intIdentity
                expectedData =
                    DataInfo
                        { dataInfoSymbol = expectedTypeIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                actualData =
                    DataInfo
                        { dataInfoSymbol = actualTypeIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                actualView = baseTypeView "Token" actualTypeIdentity
                actualValue =
                    OrdinaryValue
                        { valueInfoSymbol = actualValueIdentity
                        , valueRuntimeName = "Actual__actual"
                        , valueTypeView = actualView
                        , valueConstraintInfos = []
                        }
                methodView =
                    fixtureTypeView
                        (STArrow (STVar "a") (STBase "Int"))
                        (STArrow (STVar classParamStableName) (STBase intStableName))
                        ( Map.fromList
                            [ ("Int", intIdentity)
                            , (intStableName, intIdentity)
                            ]
                        )
                        ( Map.fromList
                            [ ("a", classParamIdentity)
                            , (classParamStableName, classParamIdentity)
                            ]
                        )
                methodInfo =
                    MethodInfo
                        { methodInfoSymbol = methodIdentity
                        , methodDisplayName = "inspect"
                        , methodTypeViewRaw = methodView
                        , methodConstraintInfos = []
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                classInfo =
                    ClassInfo
                        { classInfoSymbol = classIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity classParamIdentity "a") KType :| []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton methodIdentity methodInfo
                        }
                scope =
                    mkElaborateScope
                        ( Map.fromList
                            [ ("actual", actualValue)
                            , ("inspect", OverloadedMethod methodIdentity methodInfo)
                            ]
                        )
                        ( Map.fromList
                            [ ("Token", expectedData)
                            , ("Actual.Token", actualData)
                            ]
                        )
                        (Map.singleton "Inspect" classInfo)
                        []
                actualSymbol =
                    ProgramTypes.resolvedValueInfoSymbol
                        (SymbolLocal "Actual")
                        "actual"
                        actualValue
                methodSymbol =
                    ProgramTypes.resolvedMethodInfoSymbol
                        (SymbolLocal "Main")
                        "inspect"
                        methodInfo
                expr =
                    EApp
                        (EVar (ResolvedGlobalValue methodSymbol))
                        (EVar (ResolvedGlobalValue actualSymbol))
            lowered <-
                case lowerResolvedConstrainedExprBinding
                    scope
                    (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                    (resolvedUnconstrainedType (RSTBase (Builtins.builtinTypeSymbol "Int")))
                    False
                    expr of
                    Left err -> expectationFailure ("resolved method lowering failed: " ++ show err) >> fail "resolved method lowering failed"
                    Right lowered0 -> pure lowered0
            case [deferred | DeferredMethod deferred <- Map.elems (loweredBindingDeferredObligations lowered)] of
                [deferred] -> do
                    let placeholder = deferredRefName (deferredMethodRef deferred)
                    case Map.lookup placeholder (loweredBindingExternalTypeViews lowered) of
                        Just placeholderView -> do
                            Map.lookup
                                (symbolIdentityStableName actualTypeIdentity)
                                (ProgramTypes.typeViewHeadIdentities placeholderView)
                                `shouldBe` Just actualTypeIdentity
                            Map.lookup
                                (symbolIdentityStableName expectedTypeIdentity)
                                (ProgramTypes.typeViewHeadIdentities placeholderView)
                                `shouldBe` Nothing
                        Nothing ->
                            expectationFailure ("missing deferred method placeholder view " ++ placeholder)
                obligations ->
                    expectationFailure ("expected one deferred method obligation, got " ++ show obligations)

        it "specializes resolved case placeholder handlers by scrutinee type identity" $ do
            let boxIdentity = generatedSymbolIdentity 991880 SymbolType "Main" "Box" Nothing
                wrapIdentity = generatedSymbolIdentity 991881 SymbolConstructor "Main" "Wrap" (Just (SymbolOwnerType boxIdentity))
                boxValueIdentity = generatedSymbolIdentity 991882 SymbolValue "Main" "box" Nothing
                expectedTypeIdentity = generatedSymbolIdentity 991883 SymbolType "Expected" "Token" Nothing
                actualTypeIdentity = generatedSymbolIdentity 991884 SymbolType "Actual" "Token" Nothing
                bindingIdentity = generatedSymbolIdentity 991885 SymbolValue "Main" "main" Nothing
                boxParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991886)
                boxParamStableName = typeBinderIdentityStableName boxParamIdentity
                boxStableName = symbolIdentityStableName boxIdentity
                actualTypeStableName = symbolIdentityStableName actualTypeIdentity
                wrapView =
                    fixtureTypeView
                        (STArrow (STVar "a") (STCon "Box" (STVar "a" :| [])))
                        ( STArrow
                            (STVar boxParamStableName)
                            (STCon boxStableName (STVar boxParamStableName :| []))
                        )
                        ( Map.fromList
                            [ ("Box", boxIdentity)
                            , (boxStableName, boxIdentity)
                            ]
                        )
                        ( Map.fromList
                            [ ("a", boxParamIdentity)
                            , (boxParamStableName, boxParamIdentity)
                            ]
                        )
                wrapInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = wrapIdentity
                        , ctorRuntimeName = "Main__Wrap"
                        , ctorTypeView = wrapView
                        , ctorOwningTypeIdentity = boxIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                boxData =
                    DataInfo
                        { dataInfoSymbol = boxIdentity
                        , dataTypeParams =
                            [ ProgramTypes.CheckedTypeParam
                                (resolvedTypeBinderRefFromIdentity boxParamIdentity "a")
                                KType
                            ]
                        , dataConstructors = [wrapInfo]
                        }
                expectedData =
                    DataInfo
                        { dataInfoSymbol = expectedTypeIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                actualData =
                    DataInfo
                        { dataInfoSymbol = actualTypeIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                boxValueView =
                    ( setTypeViewHeadIdentities
                        ( Map.fromList
                            [ ("Box", boxIdentity)
                            , (boxStableName, boxIdentity)
                            , ("Token", actualTypeIdentity)
                            , (actualTypeStableName, actualTypeIdentity)
                            ]
                        )
                        ( mkTypeView
                            (STCon "Box" (STBase "Token" :| []))
                            (STCon boxStableName (STBase actualTypeStableName :| []))
                        )
                    )
                boxValue =
                    OrdinaryValue
                        { valueInfoSymbol = boxValueIdentity
                        , valueRuntimeName = "Main__box"
                        , valueTypeView = boxValueView
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        ( Map.fromList
                            [
                                ( "Wrap"
                                , ConstructorValue
                                    { valueInfoSymbol = wrapIdentity
                                    , valueRuntimeName = "Main__Wrap"
                                    , valueCtorInfo = wrapInfo
                                    }
                                )
                            , ("box", boxValue)
                            ]
                        )
                        ( Map.fromList
                            [ ("Box", boxData)
                            , ("Token", expectedData)
                            , ("Actual.Token", actualData)
                            ]
                        )
                        Map.empty
                        []
                boxSymbol =
                    ProgramTypes.resolvedValueInfoSymbol
                        (SymbolLocal "Main")
                        "box"
                        boxValue
                wrapSymbol =
                    ProgramTypes.resolvedConstructorInfoSymbol
                        (SymbolLocal "Main")
                        "Wrap"
                        boxData
                        wrapInfo
                actualTypeSymbol =
                    ProgramTypes.resolvedDataInfoSymbol
                        (SymbolLocal "Actual")
                        "Token"
                        actualData
                patternRef = localRefFromNodeId "value" (NodeId 991887)
                expr =
                    ECase
                        (EVar (ResolvedGlobalValue boxSymbol))
                        [ Alt
                            (PatCtor wrapSymbol [PatVar patternRef])
                            (EVar (ResolvedLocalValue patternRef))
                        ]
                expectedSelfIdentity =
                    typeBinderIdentityFromStructural
                        (Symbol.symbolUniqueIdentity expectedTypeIdentity)
                        StructuralSelfBinder
                actualSelfIdentity =
                    typeBinderIdentityFromStructural
                        (Symbol.symbolUniqueIdentity actualTypeIdentity)
                        StructuralSelfBinder
            lowered <-
                case lowerResolvedConstrainedExprBinding
                    scope
                    (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                    (resolvedUnconstrainedType (RSTBase actualTypeSymbol))
                    False
                    expr of
                    Left err -> expectationFailure ("resolved case lowering failed: " ++ show err) >> fail "resolved case lowering failed"
                    Right lowered0 -> pure lowered0
            case [deferred | DeferredCase deferred <- Map.elems (loweredBindingDeferredObligations lowered)] of
                [deferred] -> do
                    let placeholder = deferredRefName (deferredCaseRef deferred)
                    case Map.lookup placeholder (loweredBindingExternalTypeViews lowered) of
                        Just placeholderView -> do
                            ProgramTypes.typeViewBinderIdentityForAlias
                                placeholderView
                                (typeBinderIdentityStableName actualSelfIdentity)
                                `shouldBe` Just actualSelfIdentity
                            ProgramTypes.typeViewBinderIdentityForAlias
                                placeholderView
                                (typeBinderIdentityStableName expectedSelfIdentity)
                                `shouldBe` Nothing
                            case ProgramTypes.typeViewNodeView placeholderView of
                                ProgramTypes.TypeViewArrowNode _ outerCodomain ->
                                    case ProgramTypes.typeViewNodeView outerCodomain of
                                        ProgramTypes.TypeViewArrowNode handlerType _ ->
                                            case ProgramTypes.typeViewNodeView handlerType of
                                                ProgramTypes.TypeViewArrowNode handlerArgView _ -> do
                                                    let handlerArgBinderIdentities =
                                                            Map.elems (ProgramTypes.typeViewBinderIdentities handlerArgView)
                                                    handlerArgBinderIdentities `shouldSatisfy` elem actualSelfIdentity
                                                    handlerArgBinderIdentities `shouldSatisfy` not . elem expectedSelfIdentity
                                                other ->
                                                    expectationFailure ("unexpected deferred case handler type " ++ show other)
                                        other ->
                                            expectationFailure ("unexpected deferred case continuation type " ++ show other)
                                other ->
                                    expectationFailure ("unexpected deferred case placeholder type " ++ show other)
                        Nothing ->
                            expectationFailure ("missing deferred case placeholder view " ++ placeholder)
                obligations ->
                    expectationFailure ("expected one deferred case obligation, got " ++ show obligations)

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
                        , valueTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , valueConstraintInfos = []
                        }
                instanceInfo identity headTy =
                    InstanceInfo
                        { instanceClassSymbol = classIdentity
                        , instanceOriginModuleIdentity = originIdentity
                        , instanceConstraintInfos = []
                        , instanceHeadTypeViews = mkTypeView headTy headTy :| []
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
                case lowerResolvedConstrainedExprBinding
                    scope
                    (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                    (resolvedUnconstrainedType (RSTBase (Builtins.builtinTypeSymbol "Int")))
                    False
                    expr of
                    Left err -> expectationFailure ("resolved instance method lowering failed: " ++ show err) >> fail "resolved instance method lowering failed"
                    Right lowered0 -> pure lowered0
            loweredBindingSurfaceExpr lowered
                `shouldBe` Surface.EResolvedVar
                    (TopLevelId leftIdentity)
                    (symbolIdentityStableName leftIdentity)

        it "resolves a unique local nullary evidence method during resolved lowering" $ do
            let classIdentity = generatedSymbolIdentity 991800 SymbolClass "Main" "Pick" Nothing
                methodIdentity = generatedSymbolIdentity 991801 SymbolMethod "Main" "pick" (Just (SymbolOwnerClass classIdentity))
                bindingIdentity = generatedSymbolIdentity 991802 SymbolValue "Main" "main" Nothing
                classParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991803)
                classParamStableName = typeBinderIdentityStableName classParamIdentity
                classParamView =
                    (setTypeViewBinderIdentities (Map.singleton classParamStableName classParamIdentity) (mkTypeView (STVar "a") (STVar classParamStableName)))
                methodInfo =
                    MethodInfo
                        { methodInfoSymbol = methodIdentity
                        , methodDisplayName = "pick"
                        , methodTypeViewRaw = classParamView
                        , methodConstraintInfos = []
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                classInfo =
                    ClassInfo
                        { classInfoSymbol = classIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity classParamIdentity "a") KType :| []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton methodIdentity methodInfo
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "pick" (OverloadedMethod methodIdentity methodInfo))
                        Map.empty
                        (Map.singleton "Pick" classInfo)
                        []
                boolTy = RSTBase (Builtins.builtinTypeSymbol "Bool")
                bindingTy =
                    ConstrainedType
                        [ClassConstraint (ProgramTypes.resolvedClassInfoSymbol (SymbolLocal "Main") "Pick" classInfo) (boolTy :| [])]
                        boolTy
                expr =
                    EVar
                        ( ResolvedGlobalValue
                            (ProgramTypes.resolvedMethodInfoSymbol (SymbolLocal "Main") "pick" methodInfo)
                        )
            lowered <-
                case lowerResolvedConstrainedExprBinding
                    scope
                    (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                    bindingTy
                    False
                    expr of
                    Left err -> expectationFailure ("resolved evidence lowering failed: " ++ show err) >> fail "resolved evidence lowering failed"
                    Right lowered0 -> pure lowered0
            let loweredEvidenceRefs =
                    map
                        ProgramTypes.loweredResolvedLocalRef
                        (loweredBindingResolvedEvidenceIdentities lowered)
            loweredBindingDeferredObligations lowered `shouldBe` Map.empty
            case loweredBindingSurfaceExpr lowered of
                Surface.EResolvedLamAnn
                    (EvidenceId binderRef)
                    _
                    _
                    (Surface.EAnn (Surface.EResolvedVar (EvidenceId occurrenceRef) _) _) -> do
                        binderRef `shouldSatisfy` (`elem` loweredEvidenceRefs)
                        occurrenceRef `shouldBe` binderRef
                other ->
                    expectationFailure ("expected direct local evidence use, got " ++ show other)
            finalizeContext <- requireFinalizeContext scope
            binding <-
                case finalizeBindingWithContext finalizeContext lowered of
                    Right checked -> pure checked
                    Left err -> expectationFailure ("resolved evidence finalization failed: " ++ show err) >> fail "resolved evidence finalization failed"
            resolvedEvidenceBinders (checkedBindingTerm binding) `shouldMatchList` loweredEvidenceRefs

        it "specializes the full constrained nullary evidence head before applying local evidence" $ do
            let eqClassIdentity = generatedSymbolIdentity 991830 SymbolClass "Main" "Eq" Nothing
                eqMethodIdentity = generatedSymbolIdentity 991831 SymbolMethod "Main" "eq" (Just (SymbolOwnerClass eqClassIdentity))
                pickClassIdentity = generatedSymbolIdentity 991832 SymbolClass "Main" "Pick" Nothing
                pickMethodIdentity = generatedSymbolIdentity 991833 SymbolMethod "Main" "pick" (Just (SymbolOwnerClass pickClassIdentity))
                tokenIdentity = generatedSymbolIdentity 991834 SymbolType "Main" "Token" Nothing
                bindingIdentity = generatedSymbolIdentity 991835 SymbolValue "Main" "selected" Nothing
                eqParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991836)
                pickParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991837)
                methodLocalIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991838)
                tokenParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991839)
                eqParamStableName = typeBinderIdentityStableName eqParamIdentity
                methodLocalStableName = typeBinderIdentityStableName methodLocalIdentity
                tokenStableName = symbolIdentityStableName tokenIdentity
                boolIdentity = Builtins.builtinTypeIdentity "Bool"
                boolStableName = symbolIdentityStableName boolIdentity
                eqMethodView =
                    fixtureTypeView
                        (STArrow (STVar "q") (STArrow (STVar "q") (STBase "Bool")))
                        (STArrow (STVar eqParamStableName) (STArrow (STVar eqParamStableName) (STBase boolStableName)))
                        (Map.fromList [("Bool", boolIdentity), (boolStableName, boolIdentity)])
                        (Map.fromList [("q", eqParamIdentity), (eqParamStableName, eqParamIdentity)])
                eqMethodInfo =
                    MethodInfo
                        { methodInfoSymbol = eqMethodIdentity
                        , methodDisplayName = "eq"
                        , methodTypeViewRaw = eqMethodView
                        , methodConstraintInfos = []
                        , methodParamBinders = ("q", eqParamIdentity) :| []
                        }
                eqClassInfo =
                    ClassInfo
                        { classInfoSymbol = eqClassIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity eqParamIdentity "q") KType :| []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton eqMethodIdentity eqMethodInfo
                        }
                methodLocalView =
                    fixtureTypeView
                        (STVar "b")
                        (STVar methodLocalStableName)
                        Map.empty
                        (Map.fromList [("b", methodLocalIdentity), (methodLocalStableName, methodLocalIdentity)])
                pickMethodView =
                    fixtureTypeView
                        (STCon "Token" (STVar "b" :| []))
                        (STCon tokenStableName (STVar methodLocalStableName :| []))
                        (Map.fromList [("Token", tokenIdentity), (tokenStableName, tokenIdentity)])
                        (Map.fromList [("b", methodLocalIdentity), (methodLocalStableName, methodLocalIdentity)])
                pickMethodInfo =
                    MethodInfo
                        { methodInfoSymbol = pickMethodIdentity
                        , methodDisplayName = "pick"
                        , methodTypeViewRaw = pickMethodView
                        , methodConstraintInfos =
                            [ ProgramTypes.ConstraintInfo
                                { ProgramTypes.constraintDisplayClass = "Eq"
                                , ProgramTypes.constraintClassSymbol = eqClassIdentity
                                , ProgramTypes.constraintTypeViews = methodLocalView :| []
                                }
                            ]
                        , methodParamBinders = ("a", pickParamIdentity) :| []
                        }
                pickClassInfo =
                    ClassInfo
                        { classInfoSymbol = pickClassIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity pickParamIdentity "a") KType :| []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton pickMethodIdentity pickMethodInfo
                        }
                tokenInfo =
                    DataInfo
                        { dataInfoSymbol = tokenIdentity
                        , dataTypeParams = [ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity tokenParamIdentity "t") KType]
                        , dataConstructors = []
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "pick" (OverloadedMethod pickMethodIdentity pickMethodInfo))
                        (Map.singleton "Token" tokenInfo)
                        (Map.fromList [("Eq", eqClassInfo), ("Pick", pickClassInfo)])
                        []
                boolTy = RSTBase (Builtins.builtinTypeSymbol "Bool")
                tokenBoolTy =
                    RSTCon
                        (ProgramTypes.resolvedDataInfoSymbol (SymbolLocal "Main") "Token" tokenInfo)
                        (boolTy :| [])
                bindingTy =
                    ConstrainedType
                        [ ClassConstraint
                            (ProgramTypes.resolvedClassInfoSymbol (SymbolLocal "Main") "Pick" pickClassInfo)
                            (boolTy :| [])
                        , ClassConstraint
                            (ProgramTypes.resolvedClassInfoSymbol (SymbolLocal "Main") "Eq" eqClassInfo)
                            (boolTy :| [])
                        ]
                        tokenBoolTy
                expr =
                    EVar
                        ( ResolvedGlobalValue
                            (ProgramTypes.resolvedMethodInfoSymbol (SymbolLocal "Main") "pick" pickMethodInfo)
                        )
            lowered <-
                case lowerResolvedConstrainedExprBinding
                    scope
                    (loweredBindingIdentityFromDetails "Main__selected" (TopLevelId bindingIdentity))
                    bindingTy
                    False
                    expr of
                    Left err -> expectationFailure ("constrained nullary evidence lowering failed: " ++ show err) >> fail "constrained nullary evidence lowering failed"
                    Right lowered0 -> pure lowered0
            loweredBindingDeferredObligations lowered `shouldBe` Map.empty
            case loweredBindingSurfaceExpr lowered of
                Surface.EResolvedLamAnn
                    (EvidenceId pickBinderRef)
                    _
                    pickEvidenceTy
                    ( Surface.EResolvedLamAnn
                        (EvidenceId eqBinderRef)
                        _
                        eqEvidenceTy
                        ( Surface.EAnn
                            ( Surface.EApp
                                (Surface.EAnn (Surface.EResolvedVar (EvidenceId pickOccurrenceRef) _) specializedHeadTy)
                                (Surface.EResolvedVar (EvidenceId eqOccurrenceRef) _)
                              )
                            resultTy
                          )
                      ) -> do
                        pickOccurrenceRef `shouldBe` pickBinderRef
                        eqOccurrenceRef `shouldBe` eqBinderRef
                        pickEvidenceTy
                            `shouldSatisfy` ( \ty ->
                                                case ty of
                                                    STForall {} -> True
                                                    _ -> False
                                            )
                        case specializedHeadTy of
                            STArrow evidenceArgTy headResultTy -> do
                                evidenceArgTy `shouldBe` eqEvidenceTy
                                headResultTy `shouldBe` resultTy
                            other ->
                                expectationFailure ("expected specialized evidence arrow before application, got " ++ show other)
                other ->
                    expectationFailure ("expected constrained direct local evidence application, got " ++ show other)
            finalizeContext <- requireFinalizeContext scope
            binding <-
                case finalizeBindingWithContext finalizeContext lowered of
                    Right checked -> pure checked
                    Left err -> expectationFailure ("constrained nullary evidence finalization failed: " ++ show err) >> fail "constrained nullary evidence finalization failed"
            typeCheck (checkedBindingTerm binding)
                `shouldBe` Right (ProgramTypes.checkedBindingType binding)

        it "specializes a non-nullary local method-evidence head before applying local evidence" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Eq, Mix, eq, mix, callMix, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  instance Eq Bool {"
                        , "    eq = λleft λright true;"
                        , "  }"
                        , ""
                        , "  class Mix a {"
                        , "    mix : Eq b => a -> b -> Bool;"
                        , "  }"
                        , ""
                        , "  instance Mix Bool {"
                        , "    mix = λx λy eq y y;"
                        , "  }"
                        , ""
                        , "  def callMix : Mix Bool => Bool -> Bool -> Bool = λx λy mix x y;"
                        , "  def main : Bool = callMix true true;"
                        , "}"
                        ]
            checked <- requireChecked program
            callMixBinding <- requireCheckedBinding "Main__callMix" checked
            mainBinding <- requireCheckedBinding "Main__main" checked
            let evidenceBinders = resolvedEvidenceBinders (checkedBindingTerm callMixBinding)
                specializedEvidenceOccurrences =
                    [ (occurrenceRef, argumentTy)
                    | (occurrenceRef, argumentTy) <- resolvedEvidenceInstApps (checkedBindingTerm callMixBinding)
                    , occurrenceRef `elem` evidenceBinders
                    ]
            specializedEvidenceOccurrences
                `shouldSatisfy` any
                    ( \(_, argumentTy) ->
                        alphaEqType argumentTy (ProgramTypes.checkedBindingType mainBinding)
                    )

        it "does not specialize a rigid local class-evidence parameter from the expected nullary result" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Pick, pick, bad) {"
                        , "  class Pick a {"
                        , "    pick : a;"
                        , "  }"
                        , "  def bad : Pick a => Bool = pick;"
                        , "}"
                        ]
            checkProgram program
                `shouldBe` Left (ProgramNoMatchingInstance "Pick" (STBase "Bool"))

        it "does not use a polymorphic local zero-method assumption for a concrete instance prerequisite" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Marker, Choose, choose, selected, main) {"
                        , "  class Marker a {"
                        , "  }"
                        , "  class Choose a {"
                        , "    choose : a;"
                        , "  }"
                        , "  instance Marker Bool => Choose Int {"
                        , "    choose = 1;"
                        , "  }"
                        , "  def selected : Marker a => Int = choose;"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program
                `shouldBe` Left (ProgramNoMatchingInstance "Marker" (STBase "Bool"))

        it "falls back to a matching global zero-method instance instead of specializing local evidence" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Marker, Choose, choose, selected, main) {"
                        , "  class Marker a {"
                        , "  }"
                        , "  instance Marker Bool {"
                        , "  }"
                        , "  class Choose a {"
                        , "    choose : a;"
                        , "  }"
                        , "  instance Marker Bool => Choose Int {"
                        , "    choose = 1;"
                        , "  }"
                        , "  def selected : Marker a => Int = choose;"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "does not use polymorphic local method evidence for a concrete constrained nullary method" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Eq, Pick, eq, pick, selected, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  class Pick a {"
                        , "    pick : Eq Bool => a;"
                        , "  }"
                        , "  instance Pick Int {"
                        , "    pick = 1;"
                        , "  }"
                        , "  def selected : Eq a => a -> Int = λx let same = eq x x in pick;"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program
                `shouldBe` Left (ProgramNoMatchingInstance "Eq" (STBase "Bool"))

        it "falls back to a matching global method-bearing instance for a constrained nullary method" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Eq, Pick, eq, pick, selected, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  instance Eq Bool {"
                        , "    eq = λleft λright true;"
                        , "  }"
                        , "  class Pick a {"
                        , "    pick : Eq Bool => a;"
                        , "  }"
                        , "  instance Pick Int {"
                        , "    pick = 1;"
                        , "  }"
                        , "  def selected : Eq Int => Int = pick;"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isRight

        it "rejects ambiguous duplicate local evidence for a nullary method" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Pick, pick, bad) {"
                        , "  class Pick a {"
                        , "    pick : a;"
                        , "  }"
                        , "  def bad : (Pick Bool, Pick Bool) => Bool = pick;"
                        , "}"
                        ]
            checkProgram program `shouldBe` Left (ProgramAmbiguousMethodUse "pick")

        it "uses identity-derived runtime aliases for selected instance method dispatch instead of raw method runtime names" $ do
            let staleRuntimeName = "Main__stale_method_runtime"
                classIdentity = generatedSymbolIdentity 991810 SymbolClass "Main" "Apply" Nothing
                methodIdentity = generatedSymbolIdentity 991811 SymbolMethod "Main" "apply" (Just (SymbolOwnerClass classIdentity))
                instanceMethodIdentity = generatedSymbolIdentity 991812 SymbolValue "Main" "applyInt" Nothing
                bindingIdentity = generatedSymbolIdentity 991813 SymbolValue "Main" "main" Nothing
                originIdentity = generatedSymbolIdentity 991814 SymbolModule "Main" "Main" Nothing
                tokenTypeIdentity = generatedSymbolIdentity 991816 SymbolType "Main" "Token" Nothing
                tokenValueIdentity = generatedSymbolIdentity 991817 SymbolValue "Main" "token" Nothing
                classParamIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991815)
                classParamStableName = typeBinderIdentityStableName classParamIdentity
                tokenView =
                    (setTypeViewHeadIdentities (Map.singleton "Token" tokenTypeIdentity) (mkTypeView (STBase "Token") (STBase (symbolIdentityStableName tokenTypeIdentity))))
                methodView =
                    (setTypeViewBinderIdentities (Map.singleton classParamStableName classParamIdentity) (mkTypeView (STArrow (STVar "a") (STBase "Int")) (STArrow (STVar classParamStableName) (STBase "Int"))))
                methodInfo =
                    MethodInfo
                        { methodInfoSymbol = methodIdentity
                        , methodDisplayName = "apply"
                        , methodTypeViewRaw = methodView
                        , methodConstraintInfos = []
                        , methodParamBinders = ("a", classParamIdentity) :| []
                        }
                classInfo =
                    ClassInfo
                        { classInfoSymbol = classIdentity
                        , classTypeParams = ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity classParamIdentity "a") KType :| []
                        , classSuperclassInfos = []
                        , classFunctionalDependencies = []
                        , classMethodsByIdentity = Map.singleton methodIdentity methodInfo
                        }
                instanceMethodValue =
                    OrdinaryValue
                        { valueInfoSymbol = instanceMethodIdentity
                        , valueRuntimeName = staleRuntimeName
                        , valueTypeView = mkTypeView (STArrow (STBase "Token") (STBase "Int")) (STArrow (STBase (symbolIdentityStableName tokenTypeIdentity)) (STBase "Int"))
                        , valueConstraintInfos = []
                        }
                tokenValue =
                    OrdinaryValue
                        { valueInfoSymbol = tokenValueIdentity
                        , valueRuntimeName = "Main__token"
                        , valueTypeView = tokenView
                        , valueConstraintInfos = []
                        }
                instanceInfo =
                    InstanceInfo
                        { instanceClassSymbol = classIdentity
                        , instanceOriginModuleIdentity = originIdentity
                        , instanceConstraintInfos = []
                        , instanceHeadTypeViews = tokenView :| []
                        , instanceMethodsByIdentity = Map.singleton methodIdentity instanceMethodValue
                        }
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = tokenTypeIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                scope =
                    mkElaborateScope
                        (Map.fromList [("apply", OverloadedMethod methodIdentity methodInfo), ("token", tokenValue)])
                        (Map.singleton "Token" dataInfo)
                        (Map.singleton "Apply" classInfo)
                        [instanceInfo]
                expr =
                    EApp
                        ( EVar
                            ( ResolvedGlobalValue
                                (ProgramTypes.resolvedMethodInfoSymbol (SymbolLocal "Main") "apply" methodInfo)
                            )
                        )
                        ( EVar
                            ( ResolvedGlobalValue
                                (ProgramTypes.resolvedValueInfoSymbol (SymbolLocal "Main") "token" tokenValue)
                            )
                        )
            lowered <-
                case lowerResolvedConstrainedExprBinding
                    scope
                    (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                    (resolvedUnconstrainedType (RSTBase (Builtins.builtinTypeSymbol "Int")))
                    False
                    expr of
                    Left err -> expectationFailure ("selected instance method lowering failed: " ++ show err) >> fail "selected instance method lowering failed"
                    Right lowered0 -> pure lowered0
            loweredBindingSurfaceExpr lowered `shouldSatisfy` not . surfaceExprMentionsVar staleRuntimeName
            loweredBindingSurfaceExpr lowered `shouldSatisfy` surfaceExprMentionsVar (ProgramTypes.valueInfoRuntimeName instanceMethodValue)
            case [deferred | DeferredMethod deferred <- Map.elems (loweredBindingDeferredObligations lowered)] of
                [] -> pure ()
                deferred ->
                    expectationFailure ("expected no deferred method obligation, got " ++ show deferred)

        it "does not expose instance method runtime aliases as source values" $ do
            let staleRuntimeName = "Main__stale_method_runtime"
                classIdentity = generatedSymbolIdentity 991820 SymbolClass "Main" "Apply" Nothing
                methodIdentity = generatedSymbolIdentity 991821 SymbolMethod "Main" "apply" (Just (SymbolOwnerClass classIdentity))
                instanceMethodIdentity = generatedSymbolIdentity 991822 SymbolValue "Main" "applyInt" Nothing
                bindingIdentity = generatedSymbolIdentity 991823 SymbolValue "Main" "main" Nothing
                originIdentity = generatedSymbolIdentity 991824 SymbolModule "Main" "Main" Nothing
                intView = mkTypeView (STBase "Int") (STBase "Int")
                instanceMethodValue =
                    OrdinaryValue
                        { valueInfoSymbol = instanceMethodIdentity
                        , valueRuntimeName = staleRuntimeName
                        , valueTypeView = mkTypeView (STArrow (STBase "Int") (STBase "Int")) (STArrow (STBase "Int") (STBase "Int"))
                        , valueConstraintInfos = []
                        }
                instanceInfo =
                    InstanceInfo
                        { instanceClassSymbol = classIdentity
                        , instanceOriginModuleIdentity = originIdentity
                        , instanceConstraintInfos = []
                        , instanceHeadTypeViews = intView :| []
                        , instanceMethodsByIdentity = Map.singleton methodIdentity instanceMethodValue
                        }
                scope = mkElaborateScope Map.empty Map.empty Map.empty [instanceInfo]
            lowerExprBinding
                scope
                (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity))
                (STBase "Int")
                False
                (EVar staleRuntimeName)
                `shouldBe` Left (ProgramUnknownValue staleRuntimeName)

        it "does not resolve duplicate runtime-name external bindings by an arbitrary identity" $ do
            let runtimeName = "Main__shared"
                leftIdentity = generatedSymbolIdentity 991435 SymbolValue "Main" "left" Nothing
                rightIdentity = generatedSymbolIdentity 991436 SymbolValue "Main" "right" Nothing
                valueInfo identity =
                    OrdinaryValue
                        { valueInfoSymbol = identity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = mkTypeView (STBase "Int") (STBase "Int")
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
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = unknownSurfaceVar runtimeName
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            finalizeBindingWithContext finalizeContext lowered `shouldBe` Left (ProgramUnknownValue runtimeName)

        it "derives runtime external binding names from identity across stale runtime spellings" $ do
            let sharedIdentity = generatedSymbolIdentity 991653 SymbolValue "Main" "shared" Nothing
                bindingIdentity = generatedSymbolIdentity 991654 SymbolValue "Main" "main" Nothing
                valueInfo runtimeName =
                    OrdinaryValue
                        { valueInfoSymbol = sharedIdentity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = mkTypeView (STBase "Int") (STBase "Int")
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
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr =
                            Surface.EResolvedVar (TopLevelId sharedIdentity) "Main__right"
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            lowerType scope (STBase "Expected.Box") `shouldNotBe` lowerType scope (STBase "Wrong.Box")
            finalizeContext <- requireFinalizeContext scope
            case finalizeBindingWithContext finalizeContext lowered of
                Right binding ->
                    case checkedBindingTerm binding of
                        Elab.EVarNode resolved -> do
                            Elab.resolvedVarRuntimeName resolved `shouldBe` "Main__shared"
                            Elab.resolvedVarDetails resolved `shouldBe` TopLevelId sharedIdentity
                        other ->
                            expectationFailure ("expected external variable term, got " ++ show other)
                Left err ->
                    expectationFailure ("expected resolved external identity, got " ++ show err)

        it "resolves runtime external bindings through stable identity aliases" $ do
            let valueIdentity = generatedSymbolIdentity 991655 SymbolValue "Main" "actual" Nothing
                bindingIdentity = generatedSymbolIdentity 991656 SymbolValue "Main" "main" Nothing
                stableName = symbolIdentityStableName valueIdentity
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = valueIdentity
                        , valueRuntimeName = "Main__actual"
                        , valueTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "actual" valueInfo)
                        Map.empty
                        Map.empty
                        []
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr =
                            Surface.EResolvedVar (TopLevelId valueIdentity) stableName
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            case finalizeBindingWithContext finalizeContext lowered of
                Right binding ->
                    case checkedBindingTerm binding of
                        Elab.EVarNode resolved -> do
                            Elab.resolvedVarRuntimeName resolved `shouldBe` "Main__actual"
                            Elab.resolvedVarDetails resolved `shouldBe` TopLevelId valueIdentity
                        other ->
                            expectationFailure ("expected external variable term, got " ++ show other)
                Left err ->
                    expectationFailure ("expected stable runtime alias lookup, got " ++ show err)

        it "rejects recovered type compatibility for same-spelled free binders with different identities" $ do
            let binderName = "a"
                expectedBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991657)
                ambientBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991658)
                surfaceLocal = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991663)) "x"
                expectedView = binderFunctionTypeView binderName [] expectedBinderIdentity
                ambientValueIdentity = generatedSymbolIdentity 991659 SymbolValue "Main" "ambient" Nothing
                bindingIdentity = generatedSymbolIdentity 991660 SymbolValue "Main" "main" Nothing
                ambientValue =
                    OrdinaryValue
                        { valueInfoSymbol = ambientValueIdentity
                        , valueRuntimeName = "Main__ambient"
                        , valueTypeView = binderFunctionTypeView binderName [] ambientBinderIdentity
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "ambient" ambientValue)
                        Map.empty
                        Map.empty
                        []
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = expectedView
                        , loweredBindingExpectedTypeView = expectedView
                        , loweredBindingSurfaceExpr =
                            Surface.EResolvedLamAnn
                                (LocalId surfaceLocal)
                                "x"
                                (STVar binderName)
                                (Surface.EResolvedVar (LocalId surfaceLocal) "x")
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            finalizeBindingWithContext finalizeContext lowered
                `shouldSatisfy` either
                    ( \err -> case err of
                        ProgramTypeMismatch _ _ -> True
                        _ -> False
                    )
                    (const False)

        it "propagates lowered binder identities through individual and module-layer finalization" $ do
            let expectedBinderName = "$expected_a"
                surfaceBinderName = "$surface_a"
                binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991661)
                bindingIdentity = generatedSymbolIdentity 991662 SymbolValue "Main" "main" Nothing
                surfaceLocal = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991664)) "x"
                expectedView =
                    binderFunctionTypeView
                        expectedBinderName
                        [surfaceBinderName]
                        binderIdentity
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = expectedView
                        , loweredBindingExpectedTypeView = expectedView
                        , loweredBindingSurfaceExpr =
                            Surface.EResolvedLamAnn
                                (LocalId surfaceLocal)
                                "x"
                                (STVar surfaceBinderName)
                                (Surface.EResolvedVar (LocalId surfaceLocal) "x")
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
            finalizeContext <- requireFinalizeContext scope
            moduleContext <-
                case mkModuleFinalizeContext finalizeContext [lowered] of
                    Right preparedContext -> pure preparedContext
                    Left err ->
                        expectationFailure ("module finalize context failed: " ++ show err)
                            >> fail "module finalize context failed"

            finalizeBindingWithContext finalizeContext lowered `shouldSatisfy` isRight
            finalizeBindingAllowOpaqueWithModuleContext moduleContext lowered `shouldSatisfy` isRight

            moduleLayer <-
                finalizeBindingLayerAllowOpaqueWithModuleContext moduleContext [lowered]
            moduleLayer `shouldSatisfy` isRight

        it "selects resolved runtime externals by identity when the occurrence spelling names a sibling" $ do
            let leftIdentity = generatedSymbolIdentity 991653 SymbolValue "Left" "shared" Nothing
                rightIdentity = generatedSymbolIdentity 991654 SymbolValue "Right" "shared" Nothing
                bindingIdentity = generatedSymbolIdentity 991655 SymbolValue "Main" "main" Nothing
                valueInfo identity ty =
                    OrdinaryValue
                        { valueInfoSymbol = identity
                        , valueRuntimeName = symbolIdentityStableName identity
                        , valueTypeView = mkTypeView ty ty
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
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Bool") (STBase "Bool")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Bool") (STBase "Bool")
                        , loweredBindingSurfaceExpr =
                            Surface.EResolvedVar
                                (TopLevelId rightIdentity)
                                "Left__shared"
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
                expectRightIdentity result =
                    case result of
                        Right binding ->
                            case checkedBindingTerm binding of
                                Elab.EVarNode resolved ->
                                    Elab.resolvedVarDetails resolved `shouldBe` TopLevelId rightIdentity
                                other ->
                                    expectationFailure ("expected resolved external variable, got " ++ show other)
                        Left err ->
                            expectationFailure ("expected identity-keyed external selection, got " ++ show err)
            finalizeContext <- requireFinalizeContext scope
            expectRightIdentity (finalizeBindingWithContext finalizeContext lowered)
            moduleContext <-
                case mkModuleFinalizeContext finalizeContext [lowered] of
                    Right value -> pure value
                    Left err -> expectationFailure ("module finalize context failed: " ++ show err) >> fail "module finalize context failed"
            expectRightIdentity (finalizeBindingAllowOpaqueWithModuleContext moduleContext lowered)

        it "keeps a resolved free occurrence distinct from a same-spelled local binder" $ do
            let externalIdentity = generatedSymbolIdentity 991660 SymbolValue "Right" "external" Nothing
                bindingIdentity = generatedSymbolIdentity 991661 SymbolValue "Main" "main" Nothing
                binderRef = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991662)) "x"
                valueInfo =
                    OrdinaryValue
                        { valueInfoSymbol = externalIdentity
                        , valueRuntimeName = symbolIdentityStableName externalIdentity
                        , valueTypeView = mkTypeView (STBase "Bool") (STBase "Bool")
                        , valueConstraintInfos = []
                        }
                scope = mkElaborateScope (Map.singleton "external" valueInfo) Map.empty Map.empty []
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STArrow (STBase "Int") (STBase "Bool")) (STArrow (STBase "Int") (STBase "Bool"))
                        , loweredBindingExpectedTypeView = mkTypeView (STArrow (STBase "Int") (STBase "Bool")) (STArrow (STBase "Int") (STBase "Bool"))
                        , loweredBindingSurfaceExpr =
                            Surface.EResolvedLamAnn
                                (LocalId binderRef)
                                "x"
                                (STBase "Int")
                                (Surface.EResolvedVar (TopLevelId externalIdentity) "x")
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            finalizeBindingWithContext finalizeContext lowered `shouldSatisfy` isRight

        it "does not synthesize external identities for unresolved stable aliases" $ do
            let valueIdentity = generatedSymbolIdentity 991657 SymbolValue "Main" "actual" Nothing
                bindingIdentity = generatedSymbolIdentity 991658 SymbolValue "Main" "main" Nothing
                stableName = symbolIdentityStableName valueIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = unknownSurfaceVar stableName
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews =
                            Map.singleton stableName (mkTypeView (STBase "Int") (STBase "Int"))
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            finalizeBindingWithContext finalizeContext lowered
                `shouldBe` Left (ProgramUnknownValue stableName)

        it "does not accept name-only external type views without a resolved identity" $ do
            let bindingIdentity = generatedSymbolIdentity 991659 SymbolValue "Main" "main" Nothing
                externalName = "orphan"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = unknownSurfaceVar externalName
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews =
                            Map.singleton externalName (mkTypeView (STBase "Int") (STBase "Int"))
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            finalizeBindingWithContext finalizeContext lowered
                `shouldBe` Left (ProgramUnknownValue externalName)

        it "does not resolve conflicting runtime external payloads by an arbitrary identity" $ do
            let sharedIdentity = generatedSymbolIdentity 991650 SymbolValue "Main" "shared" Nothing
                bindingIdentity = generatedSymbolIdentity 991651 SymbolValue "Main" "main" Nothing
                valueInfo runtimeName ty =
                    OrdinaryValue
                        { valueInfoSymbol = sharedIdentity
                        , valueRuntimeName = runtimeName
                        , valueTypeView = mkTypeView ty ty
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
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = unknownSurfaceVar "Main__left"
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            finalizeBindingWithContext finalizeContext lowered
                `shouldBe` Left (ProgramUnknownValue "Main__left")

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
                expected =
                    Elab.TForallRef
                        stableRef
                        Nothing
                        ( Elab.TForallRef
                            freshRef
                            Nothing
                            (Elab.TVarAppRef stableRef (Elab.TVarRef freshRef :| []))
                        )
            srcTypeToElabTypeInScope scope sourceTy `shouldBe` Right expected

        it "rejects stable-looking source type binders without identity payloads" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991622)
                stableName = typeBinderIdentityStableName identity
                sourceTy = STForall stableName Nothing (STVarApp stableName (STBase "Int" :| []))
            ProgramTypes.typeViewFromSourceType
                (Map.singleton "Int" (Builtins.builtinTypeIdentity "Int"))
                Map.empty
                sourceTy
                `shouldSatisfy` isLeft

        it "preserves type-view binder identities while finalizing views" $ do
            let identity = typeBinderIdentityFromNode (NodeId 991608)
                stableName = typeBinderIdentityStableName identity
                ref = Elab.typeBinderRefFromIdentity identity "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                sourceTy = STForall "a" Nothing (STVar "a")
                view =
                    ( setTypeViewBinderIdentities
                        ( Map.fromList
                            [ ("a", identity)
                            , (stableName, identity)
                            ]
                        )
                        (mkTypeView sourceTy sourceTy)
                    )
                expected = Elab.TForallRef ref Nothing (Elab.TVarRef ref)
            typeViewToElabType scope view `shouldBe` Right expected

        it "drops ambiguous recovered elab type binder display identities" $ do
            let leftIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991635)
                rightIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991636)
                leftStableName = typeBinderIdentityStableName leftIdentity
                rightStableName = typeBinderIdentityStableName rightIdentity
                leftRef = Elab.typeBinderRefFromIdentity leftIdentity "a"
                rightRef = Elab.typeBinderRefFromIdentity rightIdentity "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                view =
                    elabTypeToRecoveredTypeView
                        scope
                        (Elab.TArrow (Elab.TVarRef leftRef) (Elab.TVarRef rightRef))
                identities = ProgramTypes.typeViewBinderIdentities view
            Map.lookup "a" identities `shouldBe` Nothing
            Map.lookup leftStableName identities `shouldBe` Just leftIdentity
            Map.lookup rightStableName identities `shouldBe` Just rightIdentity

        it "preserves type-view binder identities through display metadata while finalizing views" $ do
            let identity = typeBinderIdentityFromNode (NodeId 991609)
                stableName = typeBinderIdentityStableName identity
                ref = Elab.typeBinderRefFromIdentity identity "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                displayTy = STForall "a" Nothing (STVar "a")
                identityTy = STForall stableName Nothing (STVar stableName)
                view =
                    (setTypeViewBinderIdentities (Map.singleton "a" identity) (mkTypeView displayTy identityTy))
                expected = Elab.TForallRef ref Nothing (Elab.TVarRef ref)
            typeViewToElabType scope view `shouldBe` Right expected

        it "preserves type-view binder identities through stable metadata while finalizing display binders" $ do
            let identity = typeBinderIdentityFromNode (NodeId 991610)
                stableName = typeBinderIdentityStableName identity
                ref = Elab.typeBinderRefFromIdentity identity "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                displayTy = STForall "a" Nothing (STVar "a")
                identityTy = STForall stableName Nothing (STVar stableName)
                view =
                    (setTypeViewBinderIdentities (Map.singleton stableName identity) (mkTypeView displayTy identityTy))
                expected = Elab.TForallRef ref Nothing (Elab.TVarRef ref)
            typeViewToElabType scope view `shouldBe` Right expected

        it "preserves distinct identities for same-named nested finalization binders" $ do
            let outerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991633)
                innerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991637)
                outerSourceRef = resolvedTypeBinderRefFromIdentity outerIdentity "a"
                innerSourceRef = resolvedTypeBinderRefFromIdentity innerIdentity "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                view =
                    ProgramTypes.typeViewFromResolved
                        (RSTForall outerSourceRef Nothing (RSTForall innerSourceRef Nothing (RSTVar innerSourceRef)))
            case typeViewToElabType scope view of
                Right (Elab.TForallRef outerRef Nothing (Elab.TForallRef innerRef Nothing (Elab.TVarRef bodyRef))) -> do
                    Elab.typeBinderRefIdentity outerRef `shouldBe` outerIdentity
                    Elab.typeBinderRefIdentity innerRef `shouldBe` innerIdentity
                    Elab.typeBinderRefIdentity bodyRef `shouldBe` innerIdentity
                other ->
                    expectationFailure ("expected distinct finalized forall refs, got " ++ show other)

        it "preserves distinct identities for same-named finalization binders inside bounds" $ do
            let outerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991634)
                bodyIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991638)
                boundIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991639)
                outerSourceRef = resolvedTypeBinderRefFromIdentity outerIdentity "a"
                bodySourceRef = resolvedTypeBinderRefFromIdentity bodyIdentity "b"
                boundSourceRef = resolvedTypeBinderRefFromIdentity boundIdentity "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                view =
                    ProgramTypes.typeViewFromResolved
                        ( RSTForall outerSourceRef Nothing $
                            RSTForall
                                bodySourceRef
                                (Just (ResolvedSrcBound (RSTForall boundSourceRef Nothing (RSTVar boundSourceRef))))
                                (RSTVar bodySourceRef)
                        )
            case typeViewToElabType scope view of
                Right (Elab.TForallRef outerRef Nothing (Elab.TForallRef bRef (Just (Elab.TForallRef boundRef Nothing (Elab.TVarRef boundBodyRef))) (Elab.TVarRef bodyRef))) -> do
                    Elab.typeBinderRefIdentity outerRef `shouldBe` outerIdentity
                    Elab.typeBinderRefIdentity boundRef `shouldBe` boundIdentity
                    Elab.typeBinderRefIdentity boundBodyRef `shouldBe` boundIdentity
                    bodyRef `shouldBe` bRef
                other ->
                    expectationFailure ("expected distinct finalized bound refs, got " ++ show other)

        it "preserves construction-time type-view binder identities alongside head identities" $ do
            let headIdentity = generatedSymbolIdentity 43 SymbolType "Main" "Box" Nothing
                headName = symbolIdentityStableName headIdentity
                binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 44)
                binderName = typeBinderIdentityStableName binderIdentity
                binderRef = Elab.typeBinderRefFromIdentity binderIdentity "a"
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                sourceTy = STArrow (STBase "Box") (STVar "a")
                identityTy = STArrow (STBase headName) (STVar binderName)
                view =
                    fixtureTypeView
                        sourceTy
                        identityTy
                        (Map.singleton headName headIdentity)
                        (Map.singleton binderName binderIdentity)
                expected =
                    Elab.TArrow
                        (Elab.TBaseWithIdentity headIdentity (BaseTy "Box"))
                        (Elab.TVarRef binderRef)
            typeViewToElabType scope view `shouldBe` Right expected

        it "finalizes type-view heads through payload stable aliases" $ do
            let headIdentity = generatedSymbolIdentity 45 SymbolType "Main" "Token" Nothing
                headName = symbolIdentityStableName headIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                view =
                    (setTypeViewHeadIdentities (Map.singleton "Token" headIdentity) (mkTypeView (STBase headName) (STBase headName)))
                expected =
                    Elab.TBaseWithIdentity headIdentity (BaseTy headName)
            typeViewToElabType scope view `shouldBe` Right expected

        it "finalizes type-view heads through display identity pairs" $ do
            let headIdentity = generatedSymbolIdentity 991650 SymbolType "Main" "Token" Nothing
                headName = symbolIdentityStableName headIdentity
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                view =
                    (setTypeViewHeadIdentities (Map.singleton headName headIdentity) (mkTypeView (STBase "DisplayToken") (STBase headName)))
                expected =
                    Elab.TBaseWithIdentity headIdentity (BaseTy "DisplayToken")
            typeViewToElabType scope view `shouldBe` Right expected

        it "does not let conflicting type-view head aliases override builtin identities" $ do
            let fakeIdentity = generatedSymbolIdentity 991647 SymbolType "Fake" "Int" Nothing
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                view =
                    ProgramTypes.typeViewWithIdentityAliases
                        (Map.singleton "Int" fakeIdentity)
                        Map.empty
                        (builtinBaseTypeView "Int")
                expected =
                    Elab.TBaseWithIdentity (Builtins.builtinTypeIdentity "Int") (BaseTy "Int")
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
                view = mkTypeView sourceTy sourceTy
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
                        (Builtins.builtinTypeIdentity "Int")
                        (BaseTy "Int")
                expected =
                    Elab.TArrow
                        builtinIntTy
                        ( Elab.TConWithIdentity
                            (Builtins.builtinTypeIdentity "IO")
                            (BaseTy "IO")
                            (builtinIntTy :| [])
                        )
            typeViewToElabType scope (mkTypeView sourceTy sourceTy) `shouldBe` Right expected

        it "prefers scoped source type identities before builtin spellings during finalization" $ do
            let localIntIdentity = generatedSymbolIdentity 991667 SymbolType "Main" "Int" Nothing
                scope =
                    mkElaborateScope
                        Map.empty
                        (Map.singleton "Int" (DataInfo localIntIdentity [] []))
                        Map.empty
                        []
            srcTypeToElabTypeInScope scope (STBase "Int")
                `shouldBe` Right (Elab.TBaseWithIdentity localIntIdentity (BaseTy "Int"))

        it "does not resolve resolved forall substitutions by stale name when binder identity differs" $ do
            let expectedIdentity = typeBinderIdentityFromNode (NodeId 991302)
                staleIdentity = typeBinderIdentityFromNode (NodeId 991303)
                ref = Elab.typeBinderRefFromIdentity expectedIdentity "a"
                sourceView =
                    (setTypeViewBinderIdentities (Map.singleton "a" expectedIdentity) (mkTypeView (STForall "a" Nothing (STVar "a")) (STForall "a" Nothing (STVar "a"))))
                replacement = mkTypeView (STBase "Int") (STBase "Int")
                subst = Map.singleton staleIdentity replacement
            resolvedForallSubst subst sourceView [(ref, Nothing)] `shouldBe` Map.empty

        it "does not degrade identity-keyed resolved forall substitutions to name lookup" $ do
            let expectedIdentity = typeBinderIdentityFromNode (NodeId 991413)
                staleIdentity = typeBinderIdentityFromNode (NodeId 991414)
                ref = Elab.typeBinderRefFromIdentity expectedIdentity "a"
                sourceView = mkTypeView (STForall "a" Nothing (STVar "a")) (STForall "a" Nothing (STVar "a"))
                replacement = mkTypeView (STBase "Int") (STBase "Int")
                subst = Map.singleton staleIdentity replacement
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
                baseTemplate = baseTypeView "Box" typeIdentity
                baseActual = baseTypeView "Box" typeIdentity
                conTemplate =
                    fixtureTypeView
                        (STCon "Box" (STBase "Int" :| []))
                        (STCon stableHead (STBase (symbolIdentityStableName (Builtins.builtinTypeIdentity "Int")) :| []))
                        (Map.fromList [("Box", typeIdentity), ("Int", Builtins.builtinTypeIdentity "Int")])
                        Map.empty
                conActual =
                    fixtureTypeView
                        (STCon "Box" (STBase "Int" :| []))
                        (STCon stableHead (STBase (symbolIdentityStableName (Builtins.builtinTypeIdentity "Int")) :| []))
                        (Map.fromList [("Main.Box", typeIdentity), ("Int", Builtins.builtinTypeIdentity "Int")])
                        Map.empty
                metadataTemplate =
                    (setTypeViewHeadIdentities (Map.singleton "Box" typeIdentity) (mkTypeView (STBase "Box") (STBase "Box")))
                metadataActual =
                    (setTypeViewHeadIdentities (Map.singleton "Box" typeIdentity) (mkTypeView (STBase "Box") (STBase stableHead)))
                emptyScope = mkElaborateScope Map.empty Map.empty Map.empty []
            ProgramTypes.typeViewNodeView metadataTemplate
                `shouldBe` ProgramTypes.TypeViewBaseNode "Box" typeIdentity
            ProgramTypes.typeViewNodeView metadataActual
                `shouldBe` ProgramTypes.TypeViewBaseNode "Box" typeIdentity
            ProgramTypes.typeViewNodeView conTemplate
                `shouldBe` ProgramTypes.typeViewNodeView conActual
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

        it "lowers type-view data heads by identity before same display names" $ do
            let mainIdentity =
                    generatedSymbolIdentity 991656 SymbolType "Main" "Box" Nothing
                otherIdentity =
                    generatedSymbolIdentity 991657 SymbolType "Other" "Box" Nothing
                mainInfo =
                    DataInfo
                        { dataInfoSymbol = mainIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                otherInfo =
                    DataInfo
                        { dataInfoSymbol = otherIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                scope =
                    mkElaborateScope
                        Map.empty
                        ( Map.fromList
                            [ ("Box", otherInfo)
                            , ("Main.Box", mainInfo)
                            ]
                        )
                        Map.empty
                        []
                view =
                    baseTypeView "Box" mainIdentity
            case lowerTypeView scope view of
                STMu selfName _ ->
                    selfName `shouldBe` "$Main.Box_self"
                other ->
                    expectationFailure ("expected lowered Main.Box mu type, got " ++ show other)

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
                    (setTypeViewHeadIdentities (Map.singleton "Box" expectedIdentity) (mkTypeView (STBase "Box") (STBase "Box")))
                actual =
                    (setTypeViewHeadIdentities (Map.singleton "Box" actualIdentity) (mkTypeView (STBase "Box") (STBase "Box")))
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldBe` Nothing

        it "does not match identity-bearing type-view data heads through name-only fallback" $ do
            let expectedIdentity =
                    generatedSymbolIdentity 991415 SymbolType "Main" "Box" Nothing
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    (setTypeViewHeadIdentities (Map.singleton "Box" expectedIdentity) (mkTypeView (STBase "Box") (STBase "Box")))
                actual =
                    mkTypeView
                        (STBase (symbolIdentityStableName expectedIdentity))
                        (STBase (symbolIdentityStableName expectedIdentity))
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldBe` Nothing

        it "matches type-view binders by identity when names are stale" $ do
            let binderIdentity = typeBinderIdentityFromNode (NodeId 991308)
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    ( setTypeViewBinderIdentities
                        (Map.singleton "$left" binderIdentity)
                        ( mkTypeView
                            (STForall "a" Nothing (STVar "a"))
                            (STForall "$left" Nothing (STVar "$left"))
                        )
                    )
                actual =
                    ( setTypeViewBinderIdentities
                        (Map.singleton "$right" binderIdentity)
                        ( mkTypeView
                            (STForall "b" Nothing (STVar "b"))
                            (STForall "$right" Nothing (STVar "$right"))
                        )
                    )
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldSatisfy` isJust

        it "matches type-view variable heads by binder identity when names are stale" $ do
            let binderIdentity = typeBinderIdentityFromNode (NodeId 991622)
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    ( setTypeViewBinderIdentities
                        (Map.singleton "$left" binderIdentity)
                        ( mkTypeView
                            (STVarApp "f" (STBase "Int" :| []))
                            (STVarApp "$left" (STBase "Int" :| []))
                        )
                    )
                actual =
                    ( setTypeViewBinderIdentities
                        (Map.singleton "$right" binderIdentity)
                        ( mkTypeView
                            (STVarApp "g" (STBase "Int" :| []))
                            (STVarApp "$right" (STBase "Int" :| []))
                        )
                    )
            matchTypeViewsAgainstIdentity scope Map.empty (template :| []) (actual :| [])
                `shouldSatisfy` isJust

        it "matches alpha-renamed type-view binders with different identities" $ do
            let templateIdentity = typeBinderIdentityFromNode (NodeId 991309)
                actualIdentity = typeBinderIdentityFromNode (NodeId 991310)
                scope = mkElaborateScope Map.empty Map.empty Map.empty []
                template =
                    ( setTypeViewBinderIdentities
                        (Map.singleton "a" templateIdentity)
                        ( mkTypeView
                            (STForall "a" Nothing (STVar "a"))
                            (STForall "a" Nothing (STVar "a"))
                        )
                    )
                actual =
                    ( setTypeViewBinderIdentities
                        (Map.singleton "a" actualIdentity)
                        ( mkTypeView
                            (STForall "a" Nothing (STVar "a"))
                            (STForall "a" Nothing (STVar "a"))
                        )
                    )
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
                            [ProgramTypes.CheckedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991708) "a") KType]
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

        it "recovers structural owners from binder identity instead of the mu binder spelling" $ do
            let typeIdentity =
                    generatedSymbolIdentity 10001 SymbolType "Main" "Box" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 10002 SymbolConstructor "Main" "Box" (Just (SymbolOwnerType typeIdentity))
                wrongIntIdentity =
                    generatedSymbolIdentity 10004 SymbolType "Other" "Int" Nothing
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "$Box"
                        , ctorTypeView =
                            mkTypeView
                                (STArrow (STBase "Int") (STBase "Box"))
                                (STArrow (STBase "Int") (STBase "Main.Box"))
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
                visible = STBase "Box"
                encoded = lowerType scope visible
            case srcTypeToElabTypeInScope scope encoded of
                Left err ->
                    expectationFailure ("expected structural Box type to elaborate, got " ++ show err)
                Right encodedTy ->
                    case encodedTy of
                        Elab.TMuRef selfRef _ -> do
                            let renamedRef = Elab.renameTypeBinderRef "$not_a_box_name" selfRef
                                renamedTy = replaceElabTypeBinderRef selfRef renamedRef encodedTy
                                wrongRef =
                                    Elab.typeBinderRefFromIdentity
                                        (typeBinderIdentityFromNode (NodeId 10003))
                                        (Elab.typeBinderRefName selfRef)
                                wrongTy = replaceElabTypeBinderRef selfRef wrongRef encodedTy
                                wrongHeadTy =
                                    replaceElabTypeHeadIdentity
                                        (Builtins.builtinTypeIdentity "Int")
                                        wrongIntIdentity
                                        encodedTy
                            ProgramTypes.typeViewDisplay (elabTypeToRecoveredTypeView scope renamedTy)
                                `shouldBe` visible
                            ProgramTypes.typeViewDisplay (elabTypeToRecoveredTypeView scope wrongTy)
                                `shouldBe` encoded
                            ProgramTypes.typeViewDisplay (elabTypeToRecoveredTypeView scope wrongHeadTy)
                                `shouldBe` encoded
                        other ->
                            expectationFailure ("expected a structural Box mu type, got " ++ show other)
            case typeViewToElabType scope (sourceTypeViewInScope scope visible) of
                Left err ->
                    expectationFailure ("expected nominal Box TypeView to retain structural owner identity, got " ++ show err)
                Right encodedTy ->
                    ProgramTypes.typeViewDisplay (elabTypeToRecoveredTypeView scope encodedTy)
                        `shouldBe` visible

        it "recovers higher-kinded data heads with partially applied constructor parameters" $ do
            let typeIdentity =
                    generatedSymbolIdentity 1001 SymbolType "Main" "Apply" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1002 SymbolConstructor "Main" "Apply" (Just (SymbolOwnerType typeIdentity))
                fIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991701)
                aIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991702)
                fStableName = typeBinderIdentityStableName fIdentity
                aStableName = typeBinderIdentityStableName aIdentity
                applyStableHead = symbolIdentityStableName typeIdentity
                applyResult = STCon "Apply" (STVar "f" :| [STVar "a"])
                applyResultIdentity = STCon applyStableHead (STVar fStableName :| [STVar aStableName])
                applyCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "$Apply"
                        , ctorTypeView =
                            fixtureTypeView
                                (STArrow (STVarApp "f" (STVar "a" :| [])) applyResult)
                                (STArrow (STVarApp fStableName (STVar aStableName :| [])) applyResultIdentity)
                                (Map.singleton applyStableHead typeIdentity)
                                (Map.fromList [(fStableName, fIdentity), (aStableName, aIdentity)])
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                applyInfo =
                    DataInfo
                        { dataInfoSymbol = typeIdentity
                        , dataTypeParams =
                            [ ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity fIdentity "f") (KArrow KType KType)
                            , ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity aIdentity "a") KType
                            ]
                        , dataConstructors = [applyCtor]
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Apply" applyInfo) Map.empty []
                visible =
                    STCon
                        "Apply"
                        ( STBase "IO"
                            :| [STBase "String"]
                        )
            case srcTypeToElabTypeInScope scope (lowerType scope visible) of
                Right encodedTy ->
                    ProgramTypes.typeViewDisplay (elabTypeToRecoveredTypeView scope encodedTy)
                        `shouldBe` visible
                Left err ->
                    expectationFailure ("identity-bearing higher-kinded recovery setup failed: " ++ show err)

        it "recovers same-shaped encodings by structural owner identity" $ do
            let mkBoxInfo moduleName dataUnique ctorUnique =
                    let dataIdentity =
                            generatedSymbolIdentity dataUnique SymbolType moduleName "Box" Nothing
                        ctorIdentity =
                            generatedSymbolIdentity
                                ctorUnique
                                SymbolConstructor
                                moduleName
                                "Box"
                                (Just (SymbolOwnerType dataIdentity))
                        ctorInfo =
                            ConstructorInfo
                                { ctorInfoSymbol = ctorIdentity
                                , ctorRuntimeName = moduleName ++ "__Box"
                                , ctorTypeView =
                                    ( setTypeViewHeadIdentities
                                        (Map.singleton "Right.Box" dataIdentity)
                                        ( mkTypeView
                                            (STBase "Box")
                                            (STBase (moduleName ++ ".Box"))
                                        )
                                    )
                                , ctorOwningTypeIdentity = dataIdentity
                                , ctorIndex = 0
                                , ctorOwnerConstructors = []
                                }
                     in DataInfo
                            { dataInfoSymbol = dataIdentity
                            , dataTypeParams = []
                            , dataConstructors = [ctorInfo]
                            }
                leftInfo = mkBoxInfo "Left" 992570 992571
                rightInfo = mkBoxInfo "Right" 992572 992573
                scope =
                    mkElaborateScope
                        Map.empty
                        ( Map.fromList
                            [ ("Left.Box", leftInfo)
                            , ("Right.Box", rightInfo)
                            ]
                        )
                        Map.empty
                        []
            mapM_
                ( \visible ->
                    case srcTypeToElabTypeInScope scope (lowerType scope visible) of
                        Right encodedTy ->
                            ProgramTypes.typeViewDisplay (elabTypeToRecoveredTypeView scope encodedTy)
                                `shouldBe` visible
                        Left err ->
                            expectationFailure ("identity-bearing owner recovery setup failed: " ++ show err)
                )
                [STBase "Left.Box", STBase "Right.Box"]

        it "recovers repeated data parameters through scoped type-head identity aliases" $ do
            let boxIdentity =
                    generatedSymbolIdentity 1031 SymbolType "Main" "Box" Nothing
                boxCtorIdentity =
                    generatedSymbolIdentity 1032 SymbolConstructor "Main" "Box" (Just (SymbolOwnerType boxIdentity))
                boxCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = boxCtorIdentity
                        , ctorRuntimeName = "$Box"
                        , ctorTypeView = mkTypeView (STBase "Box") (STBase "Main.Box")
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
                            mkTypeView
                                (STArrow (STVar "a") (STArrow (STVar "a") dupResult))
                                (STArrow (STVar "a") (STArrow (STVar "a") (STCon "Main.Dup" (STVar "a" :| []))))
                        , ctorOwningTypeIdentity = dupIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                dupInfo =
                    DataInfo
                        { dataInfoSymbol = dupIdentity
                        , dataTypeParams =
                            [ProgramTypes.CheckedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991707) "a") KType]
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
            case srcTypeToElabTypeInScope scope actual of
                Right actualTy ->
                    ProgramTypes.typeViewDisplay (elabTypeToRecoveredTypeView scope actualTy)
                        `shouldBe` visible
                Left err ->
                    expectationFailure ("identity-bearing repeated-parameter recovery failed: " ++ show err)

        it "recovers repeated data parameters through constructor metadata head aliases" $ do
            let boxIdentity =
                    generatedSymbolIdentity 1035 SymbolType "Main" "Box" Nothing
                boxCtorIdentity =
                    generatedSymbolIdentity 1036 SymbolConstructor "Main" "Box" (Just (SymbolOwnerType boxIdentity))
                boxCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = boxCtorIdentity
                        , ctorRuntimeName = "$Box"
                        , ctorTypeView = mkTypeView (STBase "Box") (STBase "Main.Box")
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
                    generatedSymbolIdentity 1037 SymbolType "Main" "Dup" Nothing
                dupCtorIdentity =
                    generatedSymbolIdentity 1038 SymbolConstructor "Main" "Dup" (Just (SymbolOwnerType dupIdentity))
                staleBoxHead = "$stale_box_head"
                dupResult = STCon "Dup" (STVar "a" :| [])
                dupCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = dupCtorIdentity
                        , ctorRuntimeName = "$Dup"
                        , ctorTypeView =
                            ( setTypeViewHeadIdentities
                                (Map.singleton staleBoxHead boxIdentity)
                                ( mkTypeView
                                    (STArrow (STVar "a") (STArrow (STVar "a") dupResult))
                                    (STArrow (STVar "a") (STArrow (STVar "a") (STCon "Main.Dup" (STVar "a" :| []))))
                                )
                            )
                        , ctorOwningTypeIdentity = dupIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                dupInfo =
                    DataInfo
                        { dataInfoSymbol = dupIdentity
                        , dataTypeParams =
                            [ProgramTypes.CheckedTypeParam (resolvedTypeBinderRef (UniqueIdentity 991708) "a") KType]
                        , dataConstructors = [dupCtor]
                        }
                scope =
                    mkElaborateScope
                        Map.empty
                        (Map.fromList [("Box", boxInfo), (staleBoxHead, boxInfo), ("Dup", dupInfo)])
                        Map.empty
                        []
                visible = STCon "Dup" (STBase "Box" :| [])
                actual =
                    replaceFreeTypeVarsOnce
                        "a"
                        [STBase "Box", STBase staleBoxHead]
                        (lowerType scope (STCon "Dup" (STVar "a" :| [])))
            case srcTypeToElabTypeInScope scope actual of
                Right actualTy ->
                    ProgramTypes.typeViewDisplay (elabTypeToRecoveredTypeView scope actualTy)
                        `shouldBe` visible
                Left err ->
                    expectationFailure ("identity-bearing metadata-alias recovery failed: " ++ show err)

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
                            mkTypeView
                                (STArrow (STBase "Int") (STBase "Box"))
                                (STArrow (STBase "Int") (STBase "Main.Box"))
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
                selfVar = "$Main.Box_self"
                resultVar = "$Main.Box_result"
                expected =
                    STMu
                        selfVar
                        ( STForall
                            resultVar
                            Nothing
                            ( STArrow
                                (STArrow (STBase "Int") (STVar resultVar))
                                (STVar resultVar)
                            )
                        )
            lowerType scope (STBase "Box") `shouldBe` expected

        it "treats owner-shaped variable-headed constructor imports as non-trackable" $ do
            let typeIdentity =
                    generatedSymbolIdentity 1021 SymbolType "Core" "MaybeF" Nothing
                ctorIdentity unique name =
                    generatedSymbolIdentity unique SymbolConstructor "Core" name (Just (SymbolOwnerType typeIdentity))
                resultTy = STCon "MaybeF" (STVar "f" :| [STVar "a"])
                fIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991703)
                aIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991704)
                fStableName = typeBinderIdentityStableName fIdentity
                aStableName = typeBinderIdentityStableName aIdentity
                ownerStableHead = symbolIdentityStableName typeIdentity
                resultTyIdentity = STCon ownerStableHead (STVar fStableName :| [STVar aStableName])
                ownerTypeParams =
                    [ ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity fIdentity "f") (KArrow KType KType)
                    , ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity aIdentity "a") KType
                    ]
                resultView =
                    fixtureTypeView
                        resultTy
                        resultTyIdentity
                        (Map.singleton ownerStableHead typeIdentity)
                        (Map.fromList [(fStableName, fIdentity), (aStableName, aIdentity)])
                justView =
                    fixtureTypeView
                        (STArrow (STVarApp "f" (STVar "a" :| [])) resultTy)
                        (STArrow (STVarApp fStableName (STVar aStableName :| [])) resultTyIdentity)
                        (Map.singleton ownerStableHead typeIdentity)
                        (Map.fromList [(fStableName, fIdentity), (aStableName, aIdentity)])
                nothingShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity 1022 "NothingF"
                        , constructorShapeRuntimeName = "Core__NothingF"
                        , constructorShapeTypeView = resultView
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = ownerTypeParams
                        }
                justShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity 1023 "JustF"
                        , constructorShapeRuntimeName = "Core__JustF"
                        , constructorShapeTypeView = justView
                        , constructorShapeIndex = 1
                        , constructorShapeOwnerTypeParams = ownerTypeParams
                        }
                nothingCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity 1022 "NothingF"
                        , ctorRuntimeName = "Core__NothingF"
                        , ctorTypeView = resultView
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
                    ProgramTypes.typeViewQuantifyBinders [("f", fIdentity), ("a", aIdentity)] $
                        fixtureTypeView
                            displayResult
                            identityResult
                            ( Map.fromList
                                [ (staleDisplayHead, typeIdentity)
                                , (staleIdentityHead, typeIdentity)
                                ]
                            )
                            ( Map.fromList
                                [ (fStableName, fIdentity)
                                , (aStableName, aIdentity)
                                ]
                            )
                shape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity
                        , constructorShapeRuntimeName = "Core__JustF"
                        , constructorShapeTypeView = ctorView
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = []
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Core__JustF"
                        , ctorTypeView = ctorView
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = [shape]
                        }
                ownerInfo = ProgramTypes.constructorOwnerDataInfoFromShapes ctorInfo
            dataTypeParams ownerInfo
                `shouldBe` [ ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity fIdentity "f") KType
                           , ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity aIdentity "a") KType
                           ]

        it "does not infer constructor owner params from a same-spelled wrong owner identity" $ do
            let typeIdentity =
                    generatedSymbolIdentity 1041 SymbolType "Core" "MaybeF" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1042 SymbolConstructor "Core" "JustF" (Just (SymbolOwnerType typeIdentity))
                fIdentity = typeBinderIdentityFromNode (NodeId 992515)
                aIdentity = typeBinderIdentityFromNode (NodeId 992516)
                fStableName = typeBinderIdentityStableName fIdentity
                aStableName = typeBinderIdentityStableName aIdentity
                ownerStableHead = symbolIdentityStableName typeIdentity
                displayResult = STCon ownerStableHead (STVar "f" :| [STVar "a"])
                identityResult = STCon ownerStableHead (STVar fStableName :| [STVar aStableName])
                ctorView =
                    ProgramTypes.typeViewQuantifyBinders [("f", fIdentity), ("a", aIdentity)] $
                        ( setTypeViewBinderIdentities
                            ( Map.fromList
                                [ (fStableName, fIdentity)
                                , (aStableName, aIdentity)
                                ]
                            )
                            ( mkTypeView
                                displayResult
                                identityResult
                            )
                        )
                shape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity
                        , constructorShapeRuntimeName = "Core__JustF"
                        , constructorShapeTypeView = ctorView
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = []
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Core__JustF"
                        , ctorTypeView = ctorView
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = [shape]
                        }
                ownerInfo = ProgramTypes.constructorOwnerDataInfoFromShapes ctorInfo
            dataTypeParams ownerInfo `shouldBe` []

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
                    fixtureTypeView
                        displayTy
                        identityTy
                        ( Map.fromList
                            [ (staleDisplayHead, typeIdentity)
                            , (staleIdentityHead, typeIdentity)
                            ]
                        )
                        ( Map.fromList
                            [ (fStableName, fIdentity)
                            , (aStableName, aIdentity)
                            ]
                        )
                quantifiedView displayTy identityTy =
                    ProgramTypes.typeViewQuantifyBinders
                        [("f", fIdentity), ("a", aIdentity)]
                        (mkView displayTy identityTy)
                leftShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity 1027 "NothingF"
                        , constructorShapeRuntimeName = "Core__NothingF"
                        , constructorShapeTypeView = quantifiedView leftDisplayResult identityResult
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = []
                        }
                rightShape =
                    ConstructorShape
                        { constructorShapeSymbol = ctorIdentity 1028 "JustF"
                        , constructorShapeRuntimeName = "Core__JustF"
                        , constructorShapeTypeView = quantifiedView rightDisplayType rightIdentityType
                        , constructorShapeIndex = 1
                        , constructorShapeOwnerTypeParams = []
                        }
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity 1027 "NothingF"
                        , ctorRuntimeName = "Core__NothingF"
                        , ctorTypeView = quantifiedView leftDisplayResult identityResult
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = [leftShape, rightShape]
                        }
                ownerInfo = ProgramTypes.constructorOwnerDataInfoFromShapes ctorInfo
            dataTypeParams ownerInfo
                `shouldBe` [ ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity fIdentity "f") (KArrow KType KType)
                           , ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity aIdentity "a") KType
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
            checked <- requireCheckedWithPrelude program
            binding <- requireCheckedBinding "Main__None" checked
            case checkedBindingResolvedVar binding of
                ResolvedVar
                    { resolvedVarDetails = ConstructorId ctorRef
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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Option" checked
            ctorInfo <- requireDataConstructor "None" dataInfo
            ProgramTypes.ctorType ctorInfo `shouldBe` STBase "Option"
            ProgramTypes.ctorTypeIdentity ctorInfo `shouldBe` STBase (symbolIdentityStableName (dataInfoSymbol dataInfo))

        it "rewrites constructor visible heads by identity when display names collide" $ do
            let otherIdentity =
                    generatedSymbolIdentity 991720 SymbolType "Other" "Shared" Nothing
                ownerIdentity =
                    generatedSymbolIdentity 991721 SymbolType "Owner" "VisibleOwner" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 991722 SymbolConstructor "Owner" "Build" (Just (SymbolOwnerType ownerIdentity))
                bindingIdentity =
                    generatedSymbolIdentity 991723 SymbolValue "Main" "main" Nothing
                sharedDisplayHead = "Shared"
                ctorView =
                    fixtureTypeView
                        (STArrow (STBase sharedDisplayHead) (STBase sharedDisplayHead))
                        ( STArrow
                            (STBase (symbolIdentityStableName otherIdentity))
                            (STBase (symbolIdentityStableName ownerIdentity))
                        )
                        ( Map.fromList
                            [ (symbolIdentityStableName otherIdentity, otherIdentity)
                            , (symbolIdentityStableName ownerIdentity, ownerIdentity)
                            ]
                        )
                        Map.empty
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Owner__Build"
                        , ctorTypeView = ctorView
                        , ctorOwningTypeIdentity = ownerIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                otherInfo =
                    DataInfo
                        { dataInfoSymbol = otherIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                ownerInfo =
                    DataInfo
                        { dataInfoSymbol = ownerIdentity
                        , dataTypeParams = []
                        , dataConstructors = [ctorInfo]
                        }
                valueInfo =
                    ConstructorValue
                        { valueInfoSymbol = ctorIdentity
                        , valueRuntimeName = "Owner__Build"
                        , valueCtorInfo = ctorInfo
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "Build" valueInfo)
                        ( Map.fromList
                            [ (sharedDisplayHead, otherInfo)
                            , ("VisibleOwner", ownerInfo)
                            ]
                        )
                        Map.empty
                        []
                expectedVisibleType =
                    STArrow (STBase sharedDisplayHead) (STBase "VisibleOwner")
                expr =
                    ELet "build" Nothing (EVar "Build") (EVar "build")
            lowered <-
                case lowerExprBinding scope (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)) expectedVisibleType False expr of
                    Left err -> expectationFailure ("constructor visibility lowering failed: " ++ show err) >> fail "constructor visibility lowering failed"
                    Right binding -> pure binding
            case loweredBindingSurfaceExpr lowered of
                Surface.EResolvedLet _ _ (Surface.EAnn _ inferredType) _ ->
                    inferredType `shouldBe` lowerType scope expectedVisibleType
                surfaceExpr ->
                    expectationFailure ("expected an annotated constructor let binding, got " ++ show surfaceExpr)

        it "uses constructor TypeView head identities when elaborating constructor applications" $ do
            let dataIdentity =
                    generatedSymbolIdentity 1021 SymbolType "Main" "Option" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1022 SymbolConstructor "Main" "Some" (Just (SymbolOwnerType dataIdentity))
                staleDisplayHead = "$stale_display_option"
                staleIdentityHead = "$stale_identity_option"
                paramIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991705)
                paramStableName = typeBinderIdentityStableName paramIdentity
                ctorTypeView =
                    fixtureTypeView
                        (STArrow (STVar "a") (STCon staleDisplayHead (STVar "a" :| [])))
                        (STArrow (STVar paramStableName) (STCon staleIdentityHead (STVar paramStableName :| [])))
                        (Map.singleton staleIdentityHead dataIdentity)
                        (Map.singleton paramStableName paramIdentity)
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__Some"
                        , ctorTypeView = ctorTypeView
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = [ProgramTypes.CheckedTypeParam (resolvedTypeBinderRefFromIdentity paramIdentity "a") KType]
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
                    (ProgramTypes.typeViewDisplay . deferredConstructorSourceTypeView) deferred
                        `shouldBe` STArrow (STBase "Int") (STCon staleDisplayHead (STBase "Int" :| []))
                    let placeholder = ProgramTypes.deferredConstructorPlaceholder deferred
                    (Map.lookup placeholder (loweredBindingExternalTypeViews lowered) >>= Map.lookup staleIdentityHead . ProgramTypes.typeViewHeadIdentities)
                        `shouldBe` Just dataIdentity
                    let expectedPlaceholderIdentity =
                            ProgramTypes.typeViewIdentity
                                (sourceTypeViewInScope scope (lowerType scope (STArrow (STBase "Int") expectedTy)))
                    fmap ProgramTypes.typeViewIdentity (Map.lookup placeholder (loweredBindingExternalTypeViews lowered))
                        `shouldBe` Just expectedPlaceholderIdentity
                obligations ->
                    expectationFailure ("expected one deferred constructor obligation, got " ++ show obligations)

        it "uses data TypeView head identities when elaborating deferred case placeholders" $ do
            let dataIdentity =
                    generatedSymbolIdentity 1033 SymbolType "Main" "Option" Nothing
                noneIdentity =
                    generatedSymbolIdentity 1034 SymbolConstructor "Main" "None" (Just (SymbolOwnerType dataIdentity))
                someIdentity =
                    generatedSymbolIdentity 1035 SymbolConstructor "Main" "Some" (Just (SymbolOwnerType dataIdentity))
                bindingIdentity =
                    generatedSymbolIdentity 1036 SymbolValue "Main" "main" Nothing
                optionHead = symbolIdentityStableName dataIdentity
                intHead = symbolIdentityStableName (Builtins.builtinTypeIdentity "Int")
                noneTypeView =
                    fixtureTypeView
                        (STBase "Option")
                        (STBase optionHead)
                        (Map.singleton optionHead dataIdentity)
                        Map.empty
                someTypeView =
                    fixtureTypeView
                        (STArrow (STBase "Int") (STBase "Option"))
                        (STArrow (STBase intHead) (STBase optionHead))
                        (Map.fromList [(optionHead, dataIdentity), (intHead, Builtins.builtinTypeIdentity "Int")])
                        Map.empty
                noneInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = noneIdentity
                        , ctorRuntimeName = "Main__None"
                        , ctorTypeView = noneTypeView
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                someInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = someIdentity
                        , ctorRuntimeName = "Main__Some"
                        , ctorTypeView = someTypeView
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 1
                        , ctorOwnerConstructors = []
                        }
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = [noneInfo, someInfo]
                        }
                values =
                    Map.fromList
                        [
                            ( "None"
                            , ConstructorValue
                                { valueInfoSymbol = noneIdentity
                                , valueRuntimeName = "Main__None"
                                , valueCtorInfo = noneInfo
                                }
                            )
                        ,
                            ( "Some"
                            , ConstructorValue
                                { valueInfoSymbol = someIdentity
                                , valueRuntimeName = "Main__Some"
                                , valueCtorInfo = someInfo
                                }
                            )
                        ]
                scope =
                    mkElaborateScope values (Map.singleton "Option" dataInfo) Map.empty []
                expr =
                    ECase
                        (EVar "None")
                        [ Alt (PatCtor "None" []) (ELit (LInt 0))
                        , Alt (PatCtor "Some" [PatVar "x"]) (EVar "x")
                        ]
            lowered <-
                case lowerExprBinding scope (loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)) (STBase "Int") False expr of
                    Left err -> expectationFailure ("case elaboration failed: " ++ show err) >> fail "case elaboration failed"
                    Right binding -> pure binding
            case [deferred | DeferredCase deferred <- Map.elems (loweredBindingDeferredObligations lowered)] of
                [deferred] -> do
                    deferredCaseBindingIdentity deferred
                        `shouldBe` loweredBindingIdentity lowered
                    ProgramTypes.diagnosticMessage
                        ( ProgramTypes.diagnosticForProgramError
                            Nothing
                            (ProgramDeferredCaseArityMismatch deferred 0)
                        )
                        `shouldSatisfy` isInfixOf "binding `Main__main`: deferred case"
                    let placeholder = ProgramTypes.deferredCasePlaceholder deferred
                    case Map.lookup placeholder (loweredBindingExternalTypeViews lowered) of
                        Just view -> do
                            Map.elems (ProgramTypes.typeViewHeadIdentities view)
                                `shouldSatisfy` any (Symbol.sameSymbolIdentity dataIdentity)
                        Nothing ->
                            expectationFailure ("missing deferred case placeholder view " ++ placeholder)
                obligations ->
                    expectationFailure ("expected one deferred case obligation, got " ++ show obligations)

        it "applies residual arguments after resolving a deferred case result" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (One(..), main) {"
                        , "  data One ="
                        , "      One : One;"
                        , ""
                        , "  def main : Int = (case One of {"
                        , "    One -> λx x"
                        , "  }) 1;"
                        , "}"
                        ]
            checked <- requireChecked program
            (programRunOutput <$> runCheckedProgramOutput checked) `shouldBe` Right "1\n"

        it "consumes ordered head instantiations when resolving a deferred case result" $ do
            let dataIdentity =
                    generatedSymbolIdentity 1051 SymbolType "Main" "One" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1052 SymbolConstructor "Main" "One" (Just (SymbolOwnerType dataIdentity))
                scrutineeIdentity =
                    generatedSymbolIdentity 1053 SymbolValue "Main" "oneValue" Nothing
                handlerIdentity =
                    generatedSymbolIdentity 1054 SymbolValue "Main" "handler" Nothing
                bindingIdentity =
                    generatedSymbolIdentity 1055 SymbolValue "Main" "main" Nothing
                deferredRef =
                    deferredRefFromIdentity (UniqueIdentity 991710) "$case_one"
                firstIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991711)
                secondIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991712)
                binderView name identity =
                    let stableName = typeBinderIdentityStableName identity
                     in setTypeViewBinderIdentities
                            (Map.fromList [(name, identity), (stableName, identity)])
                            (mkTypeView (STVar name) (STVar stableName))
                firstView = binderView "first" firstIdentity
                secondView = binderView "second" secondIdentity
                resultView = ProgramTypes.typeViewArrow firstView secondView
                oneView = baseTypeView "One" dataIdentity
                specializedResultView =
                    ProgramTypes.typeViewArrow
                        (builtinBaseTypeView "Int")
                        (builtinBaseTypeView "Bool")
                placeholderView =
                    ProgramTypes.typeViewQuantifyBinders
                        [("first", firstIdentity), ("second", secondIdentity)]
                        ( ProgramTypes.typeViewArrow
                            oneView
                            (ProgramTypes.typeViewArrow resultView resultView)
                        )
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__One"
                        , ctorTypeView = oneView
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
                scrutineeValue =
                    OrdinaryValue
                        { valueInfoSymbol = scrutineeIdentity
                        , valueRuntimeName = "Main__oneValue"
                        , valueTypeView = oneView
                        , valueConstraintInfos = []
                        }
                handlerValue =
                    OrdinaryValue
                        { valueInfoSymbol = handlerIdentity
                        , valueRuntimeName = "Main__handler"
                        , valueTypeView = specializedResultView
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        (Map.fromList [("oneValue", scrutineeValue), ("handler", handlerValue)])
                        (Map.singleton "One" dataInfo)
                        Map.empty
                        []
                placeholder = deferredRefName deferredRef
                bindingOwner =
                    loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                obligation =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseBindingIdentity = bindingOwner
                            , deferredCaseRef = deferredRef
                            , deferredCaseDataInfo = dataInfo
                            , deferredCaseScrutineeTypeView = oneView
                            , deferredCaseResultTypeView = resultView
                            , deferredCaseExpectedArgCount = 2
                            }
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity = bindingOwner
                        , loweredBindingSourceTypeView = specializedResultView
                        , loweredBindingExpectedTypeView =
                            lowerTypeViewWithIdentities scope specializedResultView
                        , loweredBindingSurfaceExpr =
                            Surface.EApp
                                ( Surface.EApp
                                    (Surface.EResolvedVar (DeferredId deferredRef) placeholder)
                                    (Surface.EResolvedVar (TopLevelId scrutineeIdentity) "oneValue")
                                )
                                (Surface.EResolvedVar (TopLevelId handlerIdentity) "handler")
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.singleton deferredRef obligation
                        , loweredBindingExternalTypeViews = Map.singleton placeholder placeholderView
                        , loweredBindingExportedAsMain = False
                        }
            expectedResultTy <-
                case typeViewToElabType scope specializedResultView of
                    Right ty -> pure ty
                    Left err ->
                        expectationFailure ("failed to lower the specialized case result: " ++ show err)
                            >> fail "specialized case result lowering failed"
            finalizeContext <- requireFinalizeContext scope
            case finalizeBindingWithContext finalizeContext lowered of
                Right binding ->
                    case checkedBindingTerm binding of
                        Elab.EApp
                            (Elab.ETyInst (Elab.EUnroll _) (Elab.InstApp resultTy))
                            _ ->
                                alphaEqType resultTy expectedResultTy
                                    `shouldBe` True
                        term ->
                            expectationFailure ("expected a specialized deferred-case eliminator, got " ++ show term)
                Left err ->
                    expectationFailure ("expected ordered head instantiations to resolve, got " ++ show err)

        it "rejects deferred case scrutinees that only match by unqualified data name" $ do
            let expectedDataIdentity =
                    generatedSymbolIdentity 1041 SymbolType "Expected" "Box" Nothing
                wrongDataIdentity =
                    generatedSymbolIdentity 1042 SymbolType "Wrong" "Box" Nothing
                wrongValueIdentity =
                    generatedSymbolIdentity 1043 SymbolValue "Wrong" "box" Nothing
                bindingIdentity =
                    generatedSymbolIdentity 1044 SymbolValue "Main" "main" Nothing
                expectedCtorIdentity =
                    generatedSymbolIdentity 1045 SymbolConstructor "Expected" "Box" (Just (SymbolOwnerType expectedDataIdentity))
                deferredRef =
                    deferredRefFromIdentity (UniqueIdentity 991709) "$case_box"
                expectedCtor =
                    ConstructorInfo
                        { ctorInfoSymbol = expectedCtorIdentity
                        , ctorRuntimeName = "Expected__Box"
                        , ctorTypeView = baseTypeView "Expected.Box" expectedDataIdentity
                        , ctorOwningTypeIdentity = expectedDataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                expectedData =
                    DataInfo
                        { dataInfoSymbol = expectedDataIdentity
                        , dataTypeParams = []
                        , dataConstructors = [expectedCtor]
                        }
                wrongData =
                    DataInfo
                        { dataInfoSymbol = wrongDataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                wrongValue =
                    OrdinaryValue
                        { valueInfoSymbol = wrongValueIdentity
                        , valueRuntimeName = "Wrong__box"
                        , valueTypeView = baseTypeView "Wrong.Box" wrongDataIdentity
                        , valueConstraintInfos = []
                        }
                scope =
                    mkElaborateScope
                        (Map.singleton "box" wrongValue)
                        (Map.fromList [("Expected.Box", expectedData), ("Wrong.Box", wrongData)])
                        Map.empty
                        []
                placeholder = deferredRefName deferredRef
                placeholderTypeView =
                    fixtureTypeView
                        (STArrow (STBase "Expected.Box") (STBase "Int"))
                        (STArrow (STBase (symbolIdentityStableName expectedDataIdentity)) (STBase (symbolIdentityStableName (Builtins.builtinTypeIdentity "Int"))))
                        ( Map.fromList
                            [ ("Expected.Box", expectedDataIdentity)
                            , (symbolIdentityStableName expectedDataIdentity, expectedDataIdentity)
                            , ("Int", Builtins.builtinTypeIdentity "Int")
                            ]
                        )
                        Map.empty
                obligation =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseBindingIdentity =
                                loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                            , deferredCaseRef = deferredRef
                            , deferredCaseDataInfo = expectedData
                            , deferredCaseScrutineeTypeView = baseTypeView "Expected.Box" expectedDataIdentity
                            , deferredCaseResultTypeView = builtinBaseTypeView "Int"
                            , deferredCaseExpectedArgCount = 1
                            }
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr =
                            Surface.EApp
                                (Surface.EResolvedVar (DeferredId deferredRef) placeholder)
                                (Surface.EResolvedVar (TopLevelId wrongValueIdentity) "box")
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.singleton deferredRef obligation
                        , loweredBindingExternalTypeViews = Map.singleton placeholder placeholderTypeView
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            case finalizeBindingWithContext finalizeContext lowered of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "Phase 6 (elaboration)"
                    message `shouldSatisfy` isInfixOf "invalid constructed application"
                    message `shouldSatisfy` isInfixOf "$Expected.Box_self"
                    message `shouldSatisfy` isInfixOf "$Wrong.Box_self"
                    message `shouldSatisfy` isInfixOf "does not reach the exact endpoint"
                Left err ->
                    expectationFailure ("expected early identity-based scrutinee rejection, got " ++ show err)
                Right binding ->
                    expectationFailure ("expected scrutinee rejection, got " ++ show (checkedBindingTerm binding))

        it "carries constructor placeholder type binder identities into external types" $ do
            let dataIdentity =
                    generatedSymbolIdentity 1031 SymbolType "Main" "Option" Nothing
                ctorIdentity =
                    generatedSymbolIdentity 1032 SymbolConstructor "Main" "Some" (Just (SymbolOwnerType dataIdentity))
                paramUnique = UniqueIdentity 991706
                paramIdentity = typeBinderIdentityFromUnique paramUnique
                paramStableName = typeBinderIdentityStableName paramIdentity
                ctorTypeView =
                    fixtureTypeView
                        (STArrow (STVar "a") (STCon "Option" (STVar "a" :| [])))
                        (STArrow (STVar paramStableName) (STCon (symbolIdentityStableName dataIdentity) (STVar paramStableName :| [])))
                        (Map.singleton (symbolIdentityStableName dataIdentity) dataIdentity)
                        (Map.singleton paramStableName paramIdentity)
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__Some"
                        , ctorTypeView = ctorTypeView
                        , ctorOwningTypeIdentity = dataIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                dataInfo =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = [ProgramTypes.CheckedTypeParam (resolvedTypeBinderRef paramUnique "a") KType]
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
                    (setTypeViewHeadIdentities (Map.singleton dataDisplayHead dataIdentity) (mkTypeView (STBase "Box") (STBase dataHead)))
                ctorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "Main__Box"
                        , ctorTypeView = ctorTypeView
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
                    loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
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
                    deferredRefIdentity (deferredConstructorRef deferred) `shouldBe` UniqueIdentity 4
                obligations ->
                    expectationFailure ("expected one deferred constructor obligation, got " ++ show obligations)
            Map.lookup dataDisplayHead (ProgramTypes.typeViewHeadIdentities (loweredBindingSourceTypeView lowered))
                `shouldBe` Just dataIdentity
            Map.lookup dataHead (ProgramTypes.typeViewHeadIdentities (loweredBindingSourceTypeView lowered))
                `shouldBe` Just dataIdentity
            Map.lookup dataDisplayHead (ProgramTypes.typeViewHeadIdentities (loweredBindingExpectedTypeView lowered))
                `shouldBe` Just dataIdentity
            Map.lookup dataHead (ProgramTypes.typeViewHeadIdentities (loweredBindingExpectedTypeView lowered))
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

        it "closes polymorphic constrained bindings before checked IR acceptance" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (DefaultBox, Nat(..), Box(..), defaultBox, selected, main) {"
                        , "  class DefaultBox a {"
                        , "    defaultBox : Box a;"
                        , "  }"
                        , ""
                        , "  data Nat ="
                        , "      Zero : Nat;"
                        , ""
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  instance DefaultBox Nat {"
                        , "    defaultBox = Box Zero;"
                        , "  }"
                        , ""
                        , "  def selected : DefaultBox a => Box a = defaultBox;"
                        , "  def main : Box Nat = selected;"
                        , "}"
                        ]
            checked <- requireChecked program
            selected <- requireCheckedBinding "Main__selected" checked
            case (ProgramTypes.checkedBindingType selected, checkedBindingTerm selected) of
                ( Elab.TForallRef typeRef _ _
                    , Elab.ETyAbsRef
                        termRef
                        _
                        ( Elab.ELam
                            evidence@ResolvedVar{resolvedVarDetails = EvidenceId _}
                            (Elab.EVarNode evidenceOccurrence)
                          )
                    ) -> do
                        Elab.typeBinderRefsSameIdentity typeRef termRef `shouldBe` True
                        Elab.resolvedVarSameIdentity evidence evidenceOccurrence `shouldBe` True
                other ->
                    expectationFailure
                        ( "expected one type abstraction outside a direct evidence lambda, got "
                            ++ show other
                        )
            typeCheck (checkedBindingTerm selected)
                `shouldBe` Right (ProgramTypes.checkedBindingType selected)

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
            checked <- requireCheckedWithPrelude program
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

        it "does not move a constructor owner parameter whose name resembles the result binder" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checked <- requireChecked program
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            case dataTypeParams dataInfo of
                [ProgramTypes.CheckedTypeParam paramRef kind] -> do
                    let paramIdentity = resolvedTypeBinderIdentity paramRef
                        collidingParam =
                            ProgramTypes.CheckedTypeParam
                                (resolvedTypeBinderRefFromIdentity paramIdentity "$Main.Box_result_shadow")
                                kind
                        collidingDataInfo =
                            dataInfo{dataTypeParams = [collidingParam]}
                        scope =
                            mkElaborateScope
                                Map.empty
                                (Map.singleton "Box" collidingDataInfo)
                                Map.empty
                                []
                    finalizeContext <- requireFinalizeContext scope
                    lowered <- requireLowerConstructorBinding scope ctorInfo
                    binding <-
                        case finalizeBindingWithContext finalizeContext lowered of
                            Left err -> expectationFailure ("constructor finalization failed: " ++ show err) >> fail "constructor finalization failed"
                            Right checkedBinding -> pure checkedBinding
                    leadingTypeAbsIdentities (checkedBindingTerm binding)
                        `shouldSatisfy` elem paramIdentity
                params ->
                    expectationFailure ("expected one resolved data parameter, got " ++ show params)

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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Option" checked
            ctorInfo <- requireDataConstructor "None" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Option" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            lowered <- requireLowerConstructorBinding scope ctorInfo
            let poisonedLowered =
                    lowered
                        { loweredBindingSurfaceExpr = unknownSurfaceVar "$missing_constructor_pipeline_input"
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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            lowered <- requireLowerConstructorBinding scope ctorInfo
            let poisonedLowered =
                    lowered
                        { loweredBindingSurfaceExpr = unknownSurfaceVar "$missing_constructor_pipeline_input"
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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Nat" checked
            let scope = mkElaborateScope Map.empty (Map.singleton "Nat" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            mapM_
                ( \ctorName0 -> do
                    ctorInfo <- requireDataConstructor ctorName0 dataInfo
                    lowered <- requireLowerConstructorBinding scope ctorInfo
                    let poisonedLowered =
                            lowered
                                { loweredBindingSurfaceExpr = unknownSurfaceVar "$missing_constructor_pipeline_input"
                                }
                    binding <-
                        case finalizeBindingWithContext finalizeContext poisonedLowered of
                            Left err -> expectationFailure ("metadata constructor finalization failed: " ++ show err) >> fail "metadata constructor finalization failed"
                            Right checkedBinding -> pure checkedBinding
                    fmap (symbolDefiningName . constructorRefSymbol) (ProgramTypes.checkedBindingConstructorRef binding)
                        `shouldBe` Just ctorName0
                )
                ["Zero", "Succ"]

        it "selects constructor handlers by identity when positional metadata is stale" $ do
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
            checked <- requireChecked program
            dataInfo <- requireCheckedData "Main" "Nat" checked
            zeroInfo <- requireDataConstructor "Zero" dataInfo
            succInfo <- requireDataConstructor "Succ" dataInfo
            let staleSuccInfo = succInfo{ctorIndex = ctorIndex zeroInfo}
                scope = mkElaborateScope Map.empty (Map.singleton "Nat" dataInfo) Map.empty []
            lowered <- requireLowerConstructorBinding scope staleSuccInfo
            filter (isInfixOf "_k") (resolvedSurfaceLocalOccurrenceNames (loweredBindingSurfaceExpr lowered))
                `shouldBe` ["$Succ_k2"]

            let missingIdentityDataInfo = dataInfo{dataConstructors = [zeroInfo]}
                missingIdentityScope = mkElaborateScope Map.empty (Map.singleton "Nat" missingIdentityDataInfo) Map.empty []
            case
                lowerConstructorBinding
                    (identityGeneratorAfter (ProgramTypes.constructorInfoGeneratedIdentities staleSuccInfo))
                    missingIdentityScope
                    staleSuccInfo
              of
                Left (ProgramPipelineError message) ->
                    message `shouldSatisfy` isInfixOf "constructor handler metadata missing identity"
                Left err -> expectationFailure ("expected typed constructor metadata error, got " ++ show err)
                Right _ -> expectationFailure "expected missing constructor identity to fail during lowering"

        it "allocates constructor surface locals from the supplied identity generator" $ do
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
            checked <- requireChecked program
            dataInfo <- requireCheckedData "Main" "Nat" checked
            succInfo <- requireDataConstructor "Succ" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Nat" dataInfo) Map.empty []
                sentinel = UniqueIdentity 2000000
                suppliedGenerator = identityGeneratorAfter [sentinel]
            case lowerConstructorBinding suppliedGenerator scope succInfo of
                Left err ->
                    expectationFailure ("constructor lowering failed: " ++ show err)
                Right (lowered, returnedGenerator) -> do
                    let generatedIdentities =
                            nub
                                ( resolvedSurfaceGeneratedLocalIdentities
                                    (loweredBindingSurfaceExpr lowered)
                                )
                        (nextIdentity, _) = freshIdentity returnedGenerator
                    case generatedIdentities of
                        [] -> expectationFailure "constructor surface did not allocate local identities"
                        firstIdentity : rest -> do
                            generatedIdentities `shouldSatisfy` all (> sentinel)
                            nextIdentity `shouldSatisfy` (> foldr max firstIdentity rest)

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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Nat" checked
            zeroInfo <- requireDataConstructor "Zero" dataInfo
            succInfo <- requireDataConstructor "Succ" dataInfo
            let duplicateDataInfo =
                    dataInfo
                        { dataConstructors =
                            [ zeroInfo
                            , succInfo{ctorInfoSymbol = ctorInfoSymbol zeroInfo}
                            ]
                        }
                scope = mkElaborateScope Map.empty (Map.singleton "Nat" duplicateDataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            lowered <- requireLowerConstructorBinding scope zeroInfo
            let poisonedLowered =
                    lowered
                        { loweredBindingSurfaceExpr = unknownSurfaceVar "$missing_constructor_pipeline_input"
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
            checked <- requireCheckedWithPrelude program
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
            checked <- requireCheckedWithPrelude program
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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            lowered <- requireLowerConstructorBinding scope ctorInfo
            let poisonedLowered =
                    lowered
                        { loweredBindingSurfaceExpr = unknownSurfaceVar "$missing_constructor_pipeline_input"
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

        it "carries constructor owner param identities into finalization by construction" $ do
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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Box" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] -> do
                    lowered <- requireLowerConstructorBinding scope ctorInfo
                    let identityCompleteLowered =
                            lowered
                                { loweredBindingSurfaceExpr = unknownSurfaceVar "$missing_constructor_pipeline_input"
                                }
                    Map.elems (ProgramTypes.typeViewBinderIdentities (loweredBindingExpectedTypeView identityCompleteLowered))
                        `shouldContain` [paramIdentity]
                    binding <-
                        case finalizeBindingWithContext finalizeContext identityCompleteLowered of
                            Left err -> expectationFailure ("metadata constructor finalization failed: " ++ show err) >> fail "metadata constructor finalization failed"
                            Right checkedBinding -> pure checkedBinding
                    let leadingIdentities = leadingTypeAbsIdentities (checkedBindingTerm binding)
                    leadingIdentities `shouldSatisfy` (not . null)
                    leadingIdentities `shouldContain` [paramIdentity]
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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Option" checked
            ctorInfo <- requireDataConstructor "None" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Option" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            lowered <- requireLowerConstructorBinding scope ctorInfo
            let poisonedLowered =
                    lowered
                        { loweredBindingSurfaceExpr = unknownSurfaceVar "$missing_constructor_pipeline_input"
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

        it "finalizes constructor-forall bindings from metadata without the surface pipeline" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Pack(..), main) {"
                        , "  data Pack ="
                        , "      Pack : ∀ (a ⩾ Int). a -> Pack;"
                        , ""
                        , "  def main : Pack = Pack 1;"
                        , "}"
                        ]
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Pack" checked
            ctorInfo <- requireDataConstructor "Pack" dataInfo
            let scope = mkElaborateScope Map.empty (Map.singleton "Pack" dataInfo) Map.empty []
            finalizeContext <- requireFinalizeContext scope
            lowered <- requireLowerConstructorBinding scope ctorInfo
            let poisonedLowered =
                    lowered
                        { loweredBindingSurfaceExpr = unknownSurfaceVar "$missing_constructor_pipeline_input"
                        }
            binding <-
                case finalizeBindingWithContext finalizeContext poisonedLowered of
                    Left err -> expectationFailure ("metadata constructor finalization failed: " ++ show err) >> fail "metadata constructor finalization failed"
                    Right checkedBinding -> pure checkedBinding
            case ProgramTypes.ctorForallBinderInfo ctorInfo of
                [binder] ->
                    leadingTypeAbsIdentities (checkedBindingTerm binding)
                        `shouldSatisfy` elem (ProgramTypes.constructorForallIdentity binder)
                binders ->
                    expectationFailure ("expected one constructor forall, got " ++ show binders)
            unresolvedTermVarRefs (checkedBindingTerm binding) `shouldBe` []

        it "instantiates constructor-forall handlers in metadata constructor terms" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Expr(..), SomeExpr(..), main) {"
                        , "  data Expr a ="
                        , "      Keep : a -> Expr a;"
                        , ""
                        , "  data SomeExpr ="
                        , "      SomeExpr : ∀ a. Expr a -> SomeExpr;"
                        , ""
                        , "  def main : SomeExpr = SomeExpr (Keep 1);"
                        , "}"
                        ]
            checked <- requireChecked program
            binding <- requireCheckedBinding "Main__SomeExpr" checked
            typeCheck (checkedBindingTerm binding)
                `shouldBe` Right (ProgramTypes.checkedBindingType binding)

        it "records lambda and let binders with resolved local identity" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (apply, main) {"
                        , "  def apply : Int -> Int = λx let y = x in y;"
                        , "  def main : Int = apply 1;"
                        , "}"
                        ]
            checked <- requireCheckedWithPrelude program
            applyBinding <- requireCheckedBinding "Main__apply" checked
            let term = checkedBindingTerm applyBinding
                binderRefs = resolvedLocalBinders term
                occurrenceRefs = resolvedLocalOccurrences term
                resolvedRefs =
                    [ ref
                    | resolvedModule <- ProgramTypes.resolvedProgramModules (Checked.checkedProgramResolved checked)
                    , resolvedModuleName resolvedModule == "Main"
                    , DeclDef defDecl <- moduleDecls (resolvedModuleSyntax resolvedModule)
                    , refDisplayName (defDeclName defDecl) == "apply"
                    , ELam param (ELet letRef _ _ _) <- [defDeclExpr defDecl]
                    , ref <- [paramName param, letRef]
                    ]
            resolvedRefs `shouldSatisfy` (not . null)
            binderRefs `shouldMatchList` resolvedRefs
            occurrenceRefs `shouldMatchList` resolvedRefs

        it "preserves constrained lambda, pattern, and let binder identities" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (C, keep, main) {"
                        , "  class C a {"
                        , "    c : a -> Bool;"
                        , "  }"
                        , "  def keep : C Int => Int -> Int = λinput case input of {"
                        , "    value -> let copy = value in copy"
                        , "  };"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
            checked <- requireChecked program
            keepBinding <- requireCheckedBinding "Main__keep" checked
            let term = checkedBindingTerm keepBinding
                binderRefs = resolvedLocalBinders term
                occurrenceRefs = resolvedLocalOccurrences term
                resolvedRefs =
                    [ ref
                    | resolvedModule <- ProgramTypes.resolvedProgramModules (Checked.checkedProgramResolved checked)
                    , resolvedModuleName resolvedModule == "Main"
                    , DeclDef defDecl <- moduleDecls (resolvedModuleSyntax resolvedModule)
                    , refDisplayName (defDeclName defDecl) == "keep"
                    , ELam input (ECase _ [Alt (PatVar value) (ELet copy _ _ _)]) <- [defDeclExpr defDecl]
                    , ref <- [paramName input, value, copy]
                    ]
            resolvedRefs `shouldSatisfy` (not . null)
            binderRefs `shouldSatisfy` (\refs -> all (`elem` refs) resolvedRefs)
            occurrenceRefs `shouldSatisfy` (\refs -> all (`elem` refs) resolvedRefs)

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
            checked <- requireCheckedWithPrelude program
            getBinding <- requireCheckedBinding "Main__get" checked
            let term = checkedBindingTerm getBinding
                binderRefs = resolvedLocalBinders term
                occurrenceRefs = resolvedLocalOccurrences term
                resolvedPatternRefs =
                    [ valueRef
                    | resolvedModule <- ProgramTypes.resolvedProgramModules (Checked.checkedProgramResolved checked)
                    , resolvedModuleName resolvedModule == "Main"
                    , DeclDef defDecl <- moduleDecls (resolvedModuleSyntax resolvedModule)
                    , refDisplayName (defDeclName defDecl) == "get"
                    , ELam _ (ECase _ [Alt (PatCtor _ [PatVar valueRef]) _]) <- [defDeclExpr defDecl]
                    ]
            resolvedPatternRefs `shouldSatisfy` (not . null)
            binderRefs `shouldSatisfy` (\refs -> all (`elem` refs) resolvedPatternRefs)
            occurrenceRefs `shouldSatisfy` (\refs -> all (`elem` refs) resolvedPatternRefs)

        it "matches resolved local binders by identity when runtime spellings are stale" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let sourceLocal = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991799)) "source_x"
                bindingIdentity = generatedSymbolIdentity 991800 SymbolValue "Main" "identity" Nothing
                intTy = STBase "Int"
                functionTy = STArrow intTy intTy
                localIdentityExpr runtimeName =
                    Surface.EResolvedLamAnn
                        (LocalId sourceLocal)
                        runtimeName
                        intTy
                        (Surface.EResolvedVar (LocalId sourceLocal) "$stale_occurrence_x")
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__identity" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (functionTy) (functionTy)
                        , loweredBindingExpectedTypeView = mkTypeView (functionTy) (functionTy)
                        , loweredBindingSurfaceExpr = localIdentityExpr "$stale_x"
                        , loweredBindingResolvedLocalIdentities =
                            [ProgramTypes.LoweredResolvedLocalIdentity (renameLocalRef "$sidecar_x" sourceLocal) sourceLocal]
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            binding <-
                case finalizeBindingWithContext finalizeContext lowered of
                    Right checked -> pure checked
                    Left err -> expectationFailure ("finalize binding failed: " ++ show err) >> fail "finalize binding failed"
            resolvedLocalBinders (checkedBindingTerm binding) `shouldBe` [sourceLocal]
            resolvedLocalOccurrences (checkedBindingTerm binding) `shouldBe` [sourceLocal]
            map localRefName (resolvedLocalBinders (checkedBindingTerm binding)) `shouldBe` ["$stale_x"]
            map localRefName (resolvedLocalOccurrences (checkedBindingTerm binding)) `shouldBe` ["$stale_x"]

        it "keeps the surface binder identity authoritative over resolved-local sidecars" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let sourceLocal = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991810)) "source_x"
                surfaceLocal = localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991812)) "x"
                bindingIdentity = generatedSymbolIdentity 991811 SymbolValue "Main" "identity" Nothing
                intTy = STBase "Int"
                functionTy = STArrow intTy intTy
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__identity" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (functionTy) (functionTy)
                        , loweredBindingExpectedTypeView = mkTypeView (functionTy) (functionTy)
                        , loweredBindingSurfaceExpr =
                            Surface.EResolvedLamAnn
                                (LocalId surfaceLocal)
                                "x"
                                intTy
                                (Surface.EResolvedVar (LocalId surfaceLocal) "x")
                        , loweredBindingResolvedLocalIdentities =
                            [ProgramTypes.LoweredResolvedLocalIdentity (renameLocalRef "x" sourceLocal) sourceLocal]
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            binding <-
                case finalizeBindingWithContext finalizeContext lowered of
                    Right checked -> pure checked
                    Left err -> expectationFailure ("finalize binding failed: " ++ show err) >> fail "finalize binding failed"
            case (resolvedLocalBinders (checkedBindingTerm binding), resolvedLocalOccurrences (checkedBindingTerm binding)) of
                ([binderRef], [occurrenceRef]) -> do
                    binderRef `shouldBe` surfaceLocal
                    occurrenceRef `shouldBe` binderRef
                refs -> expectationFailure ("expected one generated local binder and occurrence, got " ++ show refs)

        it "keeps the same runtime binder alias distinct by attached LocalRef identity" $ do
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
                obligation bindingIdentity deferredRef =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseBindingIdentity = bindingIdentity
                            , deferredCaseRef = deferredRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeTypeView = builtinBaseTypeView "Int"
                            , deferredCaseResultTypeView = builtinBaseTypeView "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                intTy = STBase "Int"
                functionTy = STArrow intTy intTy
                localIdentityExpr runtimeName localRef =
                    Surface.EResolvedLamAnn
                        (LocalId localRef)
                        runtimeName
                        intTy
                        (Surface.EResolvedVar (LocalId localRef) runtimeName)
                lowered name identity localRef deferredRef =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails name (TopLevelId identity)
                        , loweredBindingSourceTypeView = mkTypeView (functionTy) (functionTy)
                        , loweredBindingExpectedTypeView = mkTypeView (functionTy) (functionTy)
                        , loweredBindingSurfaceExpr = localIdentityExpr "x" localRef
                        , loweredBindingResolvedLocalIdentities =
                            [ProgramTypes.LoweredResolvedLocalIdentity (renameLocalRef "x" localRef) localRef]
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations =
                            Map.singleton
                                deferredRef
                                (obligation (loweredBindingIdentityFromDetails name (TopLevelId identity)) deferredRef)
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
                firstLowered = lowered "Main__first" firstIdentity firstLocal firstDeferred
                secondLowered = lowered "Main__second" secondIdentity secondLocal secondDeferred
                runtimeAliases lowered0 =
                    map
                        (localRefName . ProgramTypes.loweredResolvedLocalRuntimeRef)
                        (loweredBindingResolvedLocalIdentities lowered0)
            map runtimeAliases [firstLowered, secondLowered] `shouldBe` [["x"], ["x"]]
            checked <-
                case finalizeBindingsAllowOpaqueWithContext
                    finalizeContext
                    [firstLowered, secondLowered] of
                    Right bindings -> pure bindings
                    Left err -> expectationFailure ("finalize group failed: " ++ show err) >> fail "finalize group failed"
            map (resolvedLocalBinders . checkedBindingTerm) checked `shouldBe` [[firstLocal], [secondLocal]]
            map (resolvedLocalOccurrences . checkedBindingTerm) checked `shouldBe` [[firstLocal], [secondLocal]]
            map (map localRefName . resolvedLocalBinders . checkedBindingTerm) checked `shouldBe` [["x"], ["x"]]
            map (map localRefName . resolvedLocalOccurrences . checkedBindingTerm) checked `shouldBe` [["x"], ["x"]]

        it "keeps graph local refs distinct from generated local refs" $ do
            let graphRef = localRefFromNodeId "x" (NodeId 0)
                generatedRef = localRefFromIdentity (GeneratedLocalId (UniqueIdentity (-1))) "x"
            localRefIdentity graphRef `shouldBe` GraphLocalId (NodeId 0)
            localRefMatchesNodeId graphRef (NodeId 0) `shouldBe` True
            localRefMatchesNodeId graphRef (NodeId 1) `shouldBe` False
            localRefMatchesNodeId generatedRef (NodeId 0) `shouldBe` False
            localIdentityStableUnique (localRefIdentity graphRef) `shouldNotBe` localIdentityStableUnique (localRefIdentity generatedRef)
            isGeneratedLocalRef graphRef `shouldBe` False

        it "preserves graph provenance when capture avoidance freshens a local ref" $ do
            let graphRef = localRefFromNodeId "x" (NodeId 7)
                (freshened, _) = freshenLocalRef "x1" (identityGeneratorAfter [UniqueIdentity 20]) graphRef
            localRefIdentity freshened `shouldBe` GeneratedGraphLocalId (UniqueIdentity 21) (NodeId 7)
            localRefMatchesNodeId freshened (NodeId 7) `shouldBe` True

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
            checked <- requireCheckedWithPrelude program
            mainBinding <- requireCheckedBinding "Main__main" checked
            resolvedLocalLetTypes (checkedBindingTerm mainBinding)
                `shouldSatisfy` any isPolymorphicIdentityType

        it "reuses imported polymorphic schemes for grouped direct instance-method aliases without Prelude" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Lib export (identity) {"
                        , "  def identity : ∀ a. a -> a = λx x;"
                        , "}"
                        , ""
                        , "module Main export (AliasOps, first, second, main) {"
                        , "  import Lib exposing (identity);"
                        , "  class AliasOps marker {"
                        , "    first : ∀ a. a -> a;"
                        , "    second : ∀ a. a -> a;"
                        , "  }"
                        , "  instance AliasOps Bool {"
                        , "    first = identity;"
                        , "    second = identity;"
                        , "  }"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            checked <- requireChecked program
            let methodRuntimeNames =
                    [ ProgramTypes.valueInfoRuntimeName valueInfo
                    | checkedModule <- checkedProgramModules checked
                    , instanceInfo <- checkedModuleInstances checkedModule
                    , ProgramTypes.instanceClassName instanceInfo == "AliasOps"
                    , valueInfo <- Map.elems (instanceMethodsByIdentity instanceInfo)
                    ]
            methodRuntimeNames `shouldSatisfy` ((== 2) . length)
            methodBindings <- traverse (`requireCheckedBinding` checked) methodRuntimeNames
            map ProgramTypes.checkedBindingType methodBindings
                `shouldSatisfy` all isPolymorphicIdentityType
            programRunOutput <$> runCheckedProgramOutput checked `shouldBe` Right "true\n"

        it "checks a polymorphic Foo constructor alias across modules without Prelude" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Foo export (Foo(..), mk) {"
                        , "  data Foo ="
                        , "      Foo : ∀ a. a -> Foo;"
                        , "  def mk : ∀ a. a -> Foo = Foo;"
                        , "}"
                        , ""
                        , "module Main export (action, main) {"
                        , "  import Foo exposing (Foo(..), mk);"
                        , "  def action : Foo = mk 1;"
                        , "  def main : Foo = action;"
                        , "}"
                        ]
            checked <- requireChecked program
            fooData <- requireCheckedData "Foo" "Foo" checked
            fooConstructor <- requireDataConstructor "Foo" fooData
            constructorBinding <- requireCheckedBinding "Foo__Foo" checked
            mkBinding <- requireCheckedBinding "Foo__mk" checked
            actionBinding <- requireCheckedBinding "Main__action" checked
            mainBinding <- requireCheckedBinding "Main__main" checked
            alphaEqType
                (ProgramTypes.checkedBindingType mkBinding)
                (ProgramTypes.checkedBindingType constructorBinding)
                `shouldBe` True
            leadingForallIdentities (ProgramTypes.checkedBindingType mkBinding)
                `shouldSatisfy` ((== 1) . length)
            map constructorRefSymbol (resolvedConstructorRefs (checkedBindingTerm mkBinding))
                `shouldBe` [ctorInfoSymbol fooConstructor]
            case resolvedConstructorVars (checkedBindingTerm mkBinding) of
                [resolvedConstructor] ->
                    alphaEqType
                        (Elab.resolvedVarType resolvedConstructor)
                        (ProgramTypes.checkedBindingType constructorBinding)
                        `shouldBe` True
                constructors ->
                    expectationFailure
                        ("expected one typed Foo constructor in Foo__mk, got " ++ show constructors)
            mkIdentity <- requireTopLevelIdentity mkBinding
            resolvedTopLevelIdentities (checkedBindingTerm actionBinding)
                `shouldContain` [mkIdentity]
            ProgramTypes.typeViewDisplay (ProgramTypes.checkedBindingSourceTypeView actionBinding)
                `shouldBe` STBase "Foo"
            ProgramTypes.typeViewDisplay (ProgramTypes.checkedBindingSourceTypeView mainBinding)
                `shouldBe` STBase "Foo"

        it "preserves same-module and alpha-renamed constructor schemes but specializes Int -> Foo" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Foo(..), same, renamed, specialized, main) {"
                        , "  data Foo ="
                        , "      Foo : ∀ a. a -> Foo;"
                        , "  def same : ∀ a. a -> Foo = Foo;"
                        , "  def renamed : ∀ b. b -> Foo = Foo;"
                        , "  def specialized : Int -> Foo = Foo;"
                        , "  def main : Foo = specialized 1;"
                        , "}"
                        ]
            checked <- requireChecked program
            fooData <- requireCheckedData "Main" "Foo" checked
            fooConstructor <- requireDataConstructor "Foo" fooData
            constructorBinding <- requireCheckedBinding "Main__Foo" checked
            sameBinding <- requireCheckedBinding "Main__same" checked
            renamedBinding <- requireCheckedBinding "Main__renamed" checked
            specializedBinding <- requireCheckedBinding "Main__specialized" checked
            let constructorIdentity = ctorInfoSymbol fooConstructor
                assertConstructorAlias binding = do
                    map constructorRefSymbol (resolvedConstructorRefs (checkedBindingTerm binding))
                        `shouldBe` [constructorIdentity]
                    case resolvedConstructorVars (checkedBindingTerm binding) of
                        [resolvedConstructor] ->
                            alphaEqType
                                (Elab.resolvedVarType resolvedConstructor)
                                (ProgramTypes.checkedBindingType constructorBinding)
                                `shouldBe` True
                        constructors ->
                            expectationFailure
                                (checkedBindingName binding ++ " did not retain one typed constructor: " ++ show constructors)
            alphaEqType
                (ProgramTypes.checkedBindingType sameBinding)
                (ProgramTypes.checkedBindingType constructorBinding)
                `shouldBe` True
            alphaEqType
                (ProgramTypes.checkedBindingType renamedBinding)
                (ProgramTypes.checkedBindingType constructorBinding)
                `shouldBe` True
            map (length . leadingForallIdentities . ProgramTypes.checkedBindingType) [sameBinding, renamedBinding]
                `shouldBe` [1, 1]
            ProgramTypes.typeViewDisplay (ProgramTypes.checkedBindingSourceTypeView specializedBinding)
                `shouldBe` STArrow (STBase "Int") (STBase "Foo")
            leadingForallIdentities (ProgramTypes.checkedBindingType specializedBinding)
                `shouldBe` []
            mapM_ assertConstructorAlias [sameBinding, renamedBinding, specializedBinding]

        it "threads finalized packet identities across definitions and modules in pure and timed checking" $ do
            let programText =
                    unlines
                        [ "module Lib export (first, second) {"
                        , "  def first : Int = let id : ∀ a. a -> a = λx x in id 1;"
                        , "  def second : Int = let id : ∀ a. a -> a = λx x in id 2;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import Lib exposing (first, second);"
                        , "  def main : Int = let id : ∀ a. a -> a = λx x in id first;"
                        , "}"
                        ]
                assertDistinctPacketIdentities checked = do
                    firstBinding <- requireCheckedBinding "Lib__first" checked
                    secondBinding <- requireCheckedBinding "Lib__second" checked
                    mainBinding <- requireCheckedBinding "Main__main" checked
                    let packetIdentities =
                            map
                                ( Set.fromList
                                    . mapMaybe typeBinderIdentityGeneratedUnique
                                    . localSchemeBinderIdentities
                                    . checkedBindingTerm
                                )
                                [firstBinding, secondBinding, mainBinding]
                    packetIdentities `shouldSatisfy` all (not . Set.null)
                    sequence_
                        [ Set.intersection left right `shouldBe` Set.empty
                        | (index, left) <- zip [(0 :: Int) ..] packetIdentities
                        , right <- drop (index + 1) packetIdentities
                        ]
            program <- requireParsed programText
            pureChecked <- requireChecked program
            assertDistinctPacketIdentities pureChecked
            located <- requireLocatedWithFile "<packet-supply>" programText
            timedResult <-
                checkLocatedProgramPackageWithDefaultTiming
                    (trivialLocatedProgramPackage located)
            timedChecked <-
                case timedResult of
                    Left diagnostic ->
                        expectationFailure ("timed check failed: " ++ show diagnostic)
                            >> fail "timed check failed"
                    Right checked -> pure checked
            assertDistinctPacketIdentities timedChecked

        it "threads one authoritative supply through deferred constructor and case binder allocation" $ do
            let supplyFloor = UniqueIdentity 992100
                occupied = [UniqueIdentity 7]
                generator0 = identityGeneratorAfter [supplyFloor]
                (constructorRefs, generator1) =
                    allocateDeferredRewriteBinderRefs generator0 occupied ["constructor-result"]
                (caseRefs, generator2) =
                    allocateDeferredRewriteBinderRefs generator1 occupied ["case-result"]
                generatedIdentity refs name =
                    Map.lookup name refs
                        >>= typeBinderIdentityGeneratedUnique
                            . Elab.typeBinderRefIdentity
                (nextIdentity, _) = freshIdentity generator2
            generatedIdentity constructorRefs "constructor-result"
                `shouldBe` Just (UniqueIdentity 992101)
            generatedIdentity caseRefs "case-result"
                `shouldBe` Just (UniqueIdentity 992102)
            nextIdentity `shouldBe` UniqueIdentity 992103

        it "threads the authoritative supply through deferred method capture avoidance" $ do
            let capturedIdentity = typeBinderIdentityFromUnique (UniqueIdentity 11)
                capturedRef = Elab.typeBinderRefFromIdentity capturedIdentity "a"
                capturedTy = Elab.TVarRef capturedRef
                methodTy =
                    Elab.TForallRef
                        capturedRef
                        Nothing
                        (Elab.TArrow capturedTy capturedTy)
                generator0 = identityGeneratorAfter [UniqueIdentity 992200]
                (freshened, generator1) =
                    freshenDeferredMethodTypeBinders generator0 [capturedTy] methodTy
                (nextIdentity, _) = freshIdentity generator1
            case freshened of
                Elab.TForallRef freshRef Nothing (Elab.TArrow (Elab.TVarRef leftRef) (Elab.TVarRef rightRef)) -> do
                    typeBinderIdentityGeneratedUnique (Elab.typeBinderRefIdentity freshRef)
                        `shouldBe` Just (UniqueIdentity 992201)
                    Elab.typeBinderRefsSameIdentity leftRef freshRef `shouldBe` True
                    Elab.typeBinderRefsSameIdentity rightRef freshRef `shouldBe` True
                    Elab.typeBinderRefsSameIdentity freshRef capturedRef `shouldBe` False
                other ->
                    expectationFailure ("expected freshened deferred method forall, got " ++ show other)
            nextIdentity `shouldBe` UniqueIdentity 992202

        it "freshens colliding binders inside bounds with the same authoritative supply" $ do
            let capturedIdentity = typeBinderIdentityFromUnique (UniqueIdentity 12)
                capturedRef = Elab.typeBinderRefFromIdentity capturedIdentity "a"
                resultIdentity = typeBinderIdentityFromUnique (UniqueIdentity 13)
                resultRef = Elab.typeBinderRefFromIdentity resultIdentity "result"
                capturedTy = Elab.TVarRef capturedRef
                nestedBound =
                    Elab.TForallRef
                        capturedRef
                        Nothing
                        (Elab.TArrow capturedTy capturedTy)
                methodTy =
                    Elab.TForallRef
                        resultRef
                        (Just nestedBound)
                        (Elab.TVarRef resultRef)
                generator0 = identityGeneratorAfter [UniqueIdentity 992210]
                (freshened, generator1) =
                    freshenDeferredMethodTypeBinders generator0 [capturedTy] methodTy
                (nextIdentity, _) = freshIdentity generator1
            case freshened of
                Elab.TForallRef
                    actualResultRef
                    (Just (Elab.TForallRef freshRef Nothing (Elab.TArrow (Elab.TVarRef leftRef) (Elab.TVarRef rightRef))))
                    (Elab.TVarRef resultUseRef) -> do
                        actualResultRef `shouldBe` resultRef
                        Elab.typeBinderRefsSameIdentity resultUseRef resultRef `shouldBe` True
                        typeBinderIdentityGeneratedUnique (Elab.typeBinderRefIdentity freshRef)
                            `shouldBe` Just (UniqueIdentity 992211)
                        Elab.typeBinderRefsSameIdentity leftRef freshRef `shouldBe` True
                        Elab.typeBinderRefsSameIdentity rightRef freshRef `shouldBe` True
                        Elab.typeBinderRefsSameIdentity freshRef capturedRef `shouldBe` False
                other ->
                    expectationFailure ("expected freshened deferred-method bound, got " ++ show other)
            nextIdentity `shouldBe` UniqueIdentity 992212

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
            checked <- requireCheckedWithPrelude program
            let handlerType = testTVar "r"
                handlerRef = generatedLocalRefForName "$None-handler"
                binder =
                    ResolvedVar
                        { resolvedVarType = handlerType
                        , resolvedVarDetails = LocalId handlerRef
                        }
                occurrence =
                    binder
                        { resolvedVarDetails =
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
            checked <- requireCheckedWithPrelude program
            let handlerType = testTVar "r"
                handlerArgType = TestElab.tBase (BaseTy "Int")
                handlerRef = generatedLocalRefForName "$Option-handler"
                noneHandler =
                    ResolvedVar
                        { resolvedVarType = handlerType
                        , resolvedVarDetails = LocalId handlerRef
                        }
                someHandler =
                    noneHandler
                        { resolvedVarType = Elab.TArrow handlerArgType handlerType
                        , resolvedVarDetails =
                            LocalId (renameLocalRef "$same-option-handler" handlerRef)
                        }
                occurrence =
                    noneHandler
                        { resolvedVarDetails =
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
                handlerArgType = TestElab.tBase (BaseTy "Int")
                handlerRef = generatedLocalRefForName "$Option-fallback-handler"
                noneHandler =
                    ResolvedVar
                        { resolvedVarType = handlerType
                        , resolvedVarDetails = LocalId handlerRef
                        }
                someHandler =
                    noneHandler
                        { resolvedVarType = Elab.TArrow handlerArgType handlerType
                        , resolvedVarDetails =
                            LocalId (renameLocalRef "$same-option-fallback-handler" handlerRef)
                        }
                occurrence =
                    noneHandler
                        { resolvedVarDetails =
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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Option" checked
            let displayHead = ProgramTypes.dataInfoIdentityName dataInfo
                identityHead = symbolIdentityStableName (dataInfoSymbol dataInfo)
                checked' =
                    replaceCheckedBindingSourceTypeView
                        "Main__main"
                        ( (setTypeViewHeadIdentities (Map.singleton displayHead (dataInfoSymbol dataInfo)) (mkTypeView (STBase displayHead) (STBase identityHead)))
                        )
                        checked
                checkedWithDisplayHeadMetadata =
                    replaceCheckedBindingSourceTypeWithHeadIdentities
                        "Main__main"
                        (STBase identityHead)
                        (Map.singleton displayHead (dataInfoSymbol dataInfo))
                        checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "None\n"
            (programRunOutput <$> runCheckedProgramOutput checkedWithDisplayHeadMetadata) `shouldBe` Right "None\n"

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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Box" checked
            optionInfo <- requireCheckedData "Main" "Option" checked
            let displayHead = ProgramTypes.dataInfoIdentityName dataInfo
                displayOptionHead = ProgramTypes.dataInfoIdentityName optionInfo
                stableHead = symbolIdentityStableName (dataInfoSymbol dataInfo)
                stableOptionHead = symbolIdentityStableName (dataInfoSymbol optionInfo)
                checked' =
                    replaceCheckedBindingSourceTypeView
                        "Main__main"
                        ( ( setTypeViewHeadIdentities
                                ( Map.fromList
                                    [ (displayHead, dataInfoSymbol dataInfo)
                                    , (displayOptionHead, dataInfoSymbol optionInfo)
                                    ]
                                )
                                ( mkTypeView
                                    (STCon displayHead (STBase displayOptionHead :| []))
                                    (STCon stableHead (STBase stableOptionHead :| []))
                                )
                          )
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
            checked <- requireCheckedWithPrelude program
            boxInfo <- requireCheckedData "Main" "Box" checked
            optionInfo <- requireCheckedData "Main" "Option" checked
            let boxDisplayHead = ProgramTypes.dataInfoIdentityName boxInfo
                optionDisplayHead = ProgramTypes.dataInfoIdentityName optionInfo
                boxStableHead = symbolIdentityStableName (ProgramTypes.dataInfoSymbol boxInfo)
                optionStableHead = symbolIdentityStableName (ProgramTypes.dataInfoSymbol optionInfo)
                ctorView =
                    ( setTypeViewHeadIdentities
                        ( Map.fromList
                            [ (boxDisplayHead, ProgramTypes.dataInfoSymbol boxInfo)
                            , (optionDisplayHead, ProgramTypes.dataInfoSymbol optionInfo)
                            ]
                        )
                        ( mkTypeView
                            (STArrow (STBase optionStableHead) (STBase boxStableHead))
                            (STArrow (STBase optionStableHead) (STBase boxStableHead))
                        )
                    )
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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] -> do
                    let staleBinder = "$stale_box_param"
                        oldView = ProgramTypes.ctorTypeView ctorInfo
                        (_, displayResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewDisplay oldView)))
                        (_, identityResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewIdentity oldView)))
                        poisonedView =
                            setTypeViewBinderIdentities
                                (Map.insert staleBinder paramIdentity (ProgramTypes.typeViewBinderIdentities oldView))
                                ( setTypeViewTypes
                                    (STArrow (STVar staleBinder) displayResult)
                                    (STArrow (STVar staleBinder) identityResult)
                                    oldView
                                )
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
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            case ProgramTypes.dataParamBinderIdentities dataInfo of
                [paramIdentity] -> do
                    let staleBinder = "$stale_runtime_box_param"
                        oldView = ProgramTypes.ctorTypeView ctorInfo
                        (_, displayResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewDisplay oldView)))
                        (_, identityResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewIdentity oldView)))
                        poisonedView =
                            setTypeViewBinderIdentities
                                (Map.insert staleBinder paramIdentity (ProgramTypes.typeViewBinderIdentities oldView))
                                ( setTypeViewTypes
                                    (STArrow (STVar staleBinder) displayResult)
                                    (STArrow (STVar staleBinder) identityResult)
                                    oldView
                                )
                        poisonedCtor = ctorInfo{ProgramTypes.ctorTypeView = poisonedView}
                        ctorTy = ProgramTypes.ctorTypeView poisonedCtor
                        stringFromInt =
                            ResolvedVar
                                { resolvedVarType = Elab.TArrow (TestElab.tBase (BaseTy "Int")) (TestElab.tBase (BaseTy "String"))
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

        it "runs parameterized constructors when stable metadata backs display binders" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def main : Box String = Box \"stable\";"
                        , "}"
                        ]
            checked <- requireCheckedWithPrelude program
            dataInfo <- requireCheckedData "Main" "Box" checked
            ctorInfo <- requireDataConstructor "Box" dataInfo
            case ProgramTypes.dataParamBinders dataInfo of
                [(paramName, paramIdentity)] -> do
                    let displayParam = "$runtime_display_box_param"
                        stableParam = typeBinderIdentityStableName paramIdentity
                        oldView = ProgramTypes.ctorTypeView ctorInfo
                        (_, displayResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewDisplay oldView)))
                        (_, identityResult) = ProgramTypes.splitArrows (snd (ProgramTypes.splitForalls (ProgramTypes.typeViewIdentity oldView)))
                        displayResult' = ProgramTypes.substituteTypeVar paramName (STVar displayParam) displayResult
                        identityResult' = ProgramTypes.substituteTypeVar paramName (STVar stableParam) identityResult
                        poisonedView =
                            setTypeViewBinderIdentities
                                (Map.singleton stableParam paramIdentity)
                                ( setTypeViewTypes
                                    (STArrow (STVar displayParam) displayResult')
                                    (STArrow (STVar stableParam) identityResult')
                                    oldView
                                )
                        poisonedCtor = ctorInfo{ProgramTypes.ctorTypeView = poisonedView}
                        ctorTy = ProgramTypes.ctorTypeView poisonedCtor
                        stringFromInt =
                            ResolvedVar
                                { resolvedVarType = Elab.TArrow (TestElab.tBase (BaseTy "Int")) (TestElab.tBase (BaseTy "String"))
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
                    expectationFailure ("expected one data param binder, got " ++ show identities)

        it "decodes checked main data by source identity when display heads are stale" $ do
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
            let checked' =
                    poisonCheckedBindingSourceTypeHeads
                        "Main__main"
                        (STBase "Box")
                        checked
            checkedWithStaleDisplay <-
                case reconstructCheckedProgram checked' of
                    Left err -> expectationFailure ("expected checked reconstruction, got " ++ show err) >> fail "checked reconstruction failed"
                    Right reconstructed -> pure reconstructed
            (programRunOutput <$> runCheckedProgramOutput checkedWithStaleDisplay) `shouldBe` Right "BBox\n"

        it "retains hidden constructor-field identities through imported value lowering" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Hidden export (Hidden) {"
                        , "  data Hidden ="
                        , "      HiddenValue : Hidden;"
                        , "}"
                        , ""
                        , "module Carrier export (Carrier(..), carried) {"
                        , "  import Hidden exposing (Hidden);"
                        , "  data Carrier ="
                        , "      Kept : Int -> Carrier"
                        , "    | HiddenCase : Hidden -> Carrier;"
                        , "  def carried : Carrier = Kept 7;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import Carrier exposing (Carrier(..), carried);"
                        , "  def main : Int = case carried of {"
                        , "    Kept value -> value;"
                        , "    HiddenCase _ -> 0"
                        , "  };"
                        , "}"
                        ]
            checked <- requireCheckedLocated located
            _ <- requireCheckedBinding "Main__main" checked
            (programRunOutput <$> runCheckedProgramOutput checked) `shouldBe` Right "7\n"

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
            checked <- requireCheckedWithPrelude program
            optionInfo <- requireCheckedData "Main" "Option" checked
            let displayHead = ProgramTypes.dataInfoIdentityName optionInfo
                checked' =
                    replaceCheckedBindingType
                        "Main__main"
                        (Elab.TBaseWithIdentity (ProgramTypes.dataInfoSymbol optionInfo) (BaseTy "$stale_option"))
                        ( replaceCheckedBindingSourceTypeView
                            "Main__main"
                            ( (setTypeViewHeadIdentities (Map.singleton displayHead (ProgramTypes.dataInfoSymbol optionInfo)) (mkTypeView (STBase displayHead) (STBase "$stale_identity_option")))
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
            checked <- requireCheckedWithPrelude program
            let intTy = TestElab.tBase (BaseTy "Int")
                stringTy = TestElab.tBase (BaseTy "String")
                primitiveTy = Elab.TArrow intTy stringTy
                staleStringFromInt =
                    ResolvedVar
                        { resolvedVarType = primitiveTy
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
            checked <- requireCheckedWithPrelude program
            let intTy = TestElab.tBase (BaseTy "Int")
                stringTy = TestElab.tBase (BaseTy "String")
                primitiveTy = Elab.TArrow intTy stringTy
                fakePrimitiveSpelling =
                    ResolvedVar
                        { resolvedVarType = primitiveTy
                        , resolvedVarDetails =
                            TopLevelId (generatedSymbolIdentity 1031 SymbolValue "Main" "notStringFromInt" Nothing)
                        }
                checkedTerm = Elab.EApp (Elab.EVarNode fakePrimitiveSpelling) (Elab.ELit (LInt 7))
                checked' = replaceCheckedBindingTerm "Main__main" checkedTerm checked
            (programRunOutput <$> runCheckedProgramOutput checked')
                `shouldNotBe` Right "\"7\"\n"

        it "runs Prelude stringFromList through its resolved primitive identity" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (List(..), stringFromList);"
                        , "  def main : String = stringFromList Nil;"
                        , "}"
                        ]
            checked <- requireCheckedLocatedWithPrelude located
            mainBinding <- requireCheckedBinding "Main__main" checked
            stringFromListBinding <-
                requireCheckedBinding "Prelude__stringFromList" checked
            stringFromListBindingIdentity <-
                requireTopLevelIdentity stringFromListBinding
            let stringFromListIdentity =
                    Builtins.builtinValueIdentity
                        PrimitiveInventory.stringFromListPrimitiveName
            resolvedTopLevelIdentities (checkedBindingTerm mainBinding)
                `shouldSatisfy` any (Symbol.sameSymbolIdentity stringFromListBindingIdentity)
            resolvedTopLevelIdentities (checkedBindingTerm stringFromListBinding)
                `shouldSatisfy` any (Symbol.sameSymbolIdentity stringFromListIdentity)
            (programRunOutput <$> runCheckedProgramOutput checked) `shouldBe` Right "\"\"\n"

        it "indexes runtime Prelude support by module identity instead of checked module name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (List(..), stringFromList);"
                        , "  def main : String = stringFromList Nil;"
                        , "}"
                        ]
            checked <- requireCheckedLocatedWithPrelude located
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
            checked <- requireCheckedLocatedWithPrelude located
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
            checked <- requireCheckedLocatedWithPrelude located
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
            checked <- requireCheckedLocatedWithPrelude located
            let checked' = renameCheckedExportedTypeDisplaysWhere (== "List") "$stale_list_export_display" checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "\"\"\n"

        it "does not index runtime Prelude constructors through stale owner identity payloads" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (List(..), stringFromList);"
                        , "  def main : String = stringFromList Nil;"
                        , "}"
                        ]
            checked <- requireCheckedLocatedWithPrelude located
            listData <- requireCheckedData "Prelude" "List" checked
            nilCtor <- requireDataConstructor "Nil" listData
            let staleListOwner = renameSymbolDefiningName "$stale_list_owner" (dataInfoSymbol listData)
                checked' = replaceCheckedConstructorOwner (ctorInfoSymbol nilCtor) staleListOwner checked
            case runCheckedProgramOutput checked' of
                Left (ProgramPipelineError message) ->
                    message `shouldSatisfy` isInfixOf "stringFromList expected a List Char argument"
                other ->
                    expectationFailure ("expected stale Prelude constructor owner rejection, got " ++ show other)

        it "runs checked IO main by resolved binding identity instead of checked binding name" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_putStrLn \"binding-identity\";"
                        , "}"
                        ]
            checked <- requireCheckedLocatedWithPrelude located
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
            checked <- requireCheckedLocatedWithPrelude located
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
            checked <- requireCheckedLocatedWithPrelude located
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
            checked <- requireCheckedLocatedWithPrelude located
            let checked' =
                    renameCheckedConstructorRuntimeNamesWhere
                        (== "Main__None")
                        "$stale_none_runtime_name"
                        checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "ctor-identity\n"

        it "does not resolve checked runtime constructor references through stale identity payloads" $ do
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
            checked <- requireCheckedLocatedWithPrelude located
            dataInfo <- requireCheckedData "Main" "Option" checked
            noneCtor <- requireDataConstructor "None" dataInfo
            actionBinding <- requireCheckedBinding "Main__action" checked
            let noneIdentity = ctorInfoSymbol noneCtor
                staleNoneIdentity = renameSymbolDefiningName "$stale_none" noneIdentity
                checked' =
                    replaceCheckedBindingTerm
                        "Main__action"
                        (poisonConstructorTermIdentity noneIdentity staleNoneIdentity "$stale_none" (checkedBindingTerm actionBinding))
                        checked
            case runCheckedProgramOutput checked' of
                Left (ProgramUnknownValue name) ->
                    name `shouldBe` "Main__$stale_none"
                other ->
                    expectationFailure ("expected stale constructor identity rejection, got " ++ show other)

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

        it "rejects checked runtime bindings with no symbol identity before run context lookup" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def helper : Int = 0;"
                        , "  def main : Int = helper;"
                        , "}"
                        ]
            checked <- requireChecked program
            let checked' =
                    replaceCheckedBindingDetails
                        "Main__helper"
                        (LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991754)) "helper"))
                        checked
            case runCheckedProgramOutput checked' of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "missing symbol identity"
                    message `shouldSatisfy` isInfixOf "`helper`"
                other ->
                    expectationFailure ("expected missing binding identity rejection, got " ++ show other)

        it "rejects checked runtime binding identity payload conflicts before run context lookup" $ do
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
            let conflictingIdentity = renameSymbolDefiningName "$stale_main" duplicateIdentity
                checked' = replaceCheckedBindingTopLevelIdentity "Main__helper" conflictingIdentity checked
            case runCheckedProgramOutput checked' of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "conflicting checked binding identity payload"
                    message `shouldSatisfy` isInfixOf (symbolIdentityStableName duplicateIdentity)
                other ->
                    expectationFailure ("expected binding identity payload conflict rejection, got " ++ show other)

        it "does not resolve checked runtime binding references through stale identity payloads" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, pure);"
                        , "  def helper : IO Unit = pure Unit;"
                        , "  def main : IO Unit = helper;"
                        , "}"
                        ]
            checked <- requireCheckedLocatedWithPrelude located
            helperBinding <- requireCheckedBinding "Main__helper" checked
            mainBinding <- requireCheckedBinding "Main__main" checked
            helperIdentity <- requireTopLevelIdentity helperBinding
            let staleHelperIdentity = renameSymbolDefiningName "$stale_helper" helperIdentity
                checked' =
                    replaceCheckedBindingTerm
                        "Main__main"
                        (poisonTopLevelTermIdentity helperIdentity staleHelperIdentity "$stale_helper" (checkedBindingTerm mainBinding))
                        checked
            case runCheckedProgramOutput checked' of
                Left (ProgramUnknownValue name) ->
                    name `shouldBe` "Main__$stale_helper"
                other ->
                    expectationFailure ("expected stale binding identity rejection, got " ++ show other)

        it "keeps runtime env keys exact when stale payloads share a top-level unique" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_putStrLn \"placeholder\";"
                        , "}"
                        ]
            checked <- requireCheckedLocatedWithPrelude located
            let stringTy = TestElab.tBase (BaseTy "String")
                correctIdentity = generatedSymbolIdentity 991730 SymbolValue "Synthetic" "message" Nothing
                staleIdentity = renameSymbolDefiningName "$stale_message" correctIdentity
                correctResolved =
                    ResolvedVar
                        { resolvedVarType = stringTy
                        , resolvedVarDetails = TopLevelId correctIdentity
                        }
                staleResolved =
                    ResolvedVar
                        { resolvedVarType = stringTy
                        , resolvedVarDetails = TopLevelId staleIdentity
                        }
                stringScheme = Elab.mkElabSchemeWithRefs [] stringTy
                checkedTerm =
                    Elab.ELet correctResolved stringScheme (Elab.ELit (LString "exact-env")) $
                        Elab.ELet staleResolved stringScheme (Elab.ELit (LString "stale-env")) $
                            Elab.EApp (primitiveTerm "__io_putStrLn") (Elab.EVarNode correctResolved)
                checked' = replaceCheckedBindingTerm "Main__main" checkedTerm checked
            (programRunOutput <$> runCheckedProgramOutput checked') `shouldBe` Right "exact-env\n"

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
            checked <- requireCheckedLocatedWithPrelude located
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
            checked <- requireCheckedLocatedWithPrelude located
            let intElabTy = TestElab.tBase (BaseTy "Int")
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
                Program [Module{moduleDecls = [DeclClass classDecl, DeclData dataDecl]}] -> do
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
                Program [Module{moduleDecls = [DeclClass classDecl, DeclInstance instanceDecl]}] -> do
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
                Program [Module{moduleDecls = [DeclTypeFamily familyDecl]}] -> do
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
                Program [Module{moduleDecls = [DeclClass classDecl, DeclData dataDecl]}] -> do
                    case classDeclMethods classDecl of
                        [MethodSig{methodSigType = ConstrainedType [] methodTy}] ->
                            methodTy `shouldBe` expectedMethodTy
                        other -> expectationFailure ("unexpected method shape: " ++ show other)
                    case dataDeclConstructors dataDecl of
                        [ConstructorDecl{constructorDeclType = ctorTy}] ->
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

    describe "MLF.Program CLI helper" $ do
        it "prepends the built-in Prelude for explicit imports" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Nat(..), Option(..));"
                        , "  def main : Option Nat = Some Zero;"
                        , "}"
                        ]
            (prettyValue <$> runLocatedProgramWithPrelude located) `shouldBe` Right "Some Zero"

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
            checkLocatedProgramWithPrelude located `shouldSatisfy` isRight

        it "constructs fully applied Prelude method results at the expected nominal identity" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, pure, putStrLn);"
                        , "  def action : IO Unit = pure Unit;"
                        , "  def main : IO Unit = bind action (λ(_done : Unit) putStrLn \"nominal\");"
                        , "}"
                        ]
            checked <- requireCheckedLocatedWithPrelude located
            mainBinding <- requireCheckedBinding "Main__main" checked
            let typeCheckEnv =
                    ElabPipeline.mkTypeCheckEnvWithResolvedTerms
                        [ (checkedBindingResolvedVar candidate, ProgramTypes.checkedBindingType candidate)
                        | checkedModule <- checkedProgramModules checked
                        , candidate <- checkedModuleBindings checkedModule
                        ]
                        Map.empty
            case ElabPipeline.typeCheckWithEnv typeCheckEnv (checkedBindingTerm mainBinding) of
                Left err ->
                    expectationFailure
                        ("fully applied Prelude method result failed to typecheck: " ++ show err)
                Right inferred ->
                    alphaEqType inferred (ProgramTypes.checkedBindingType mainBinding)
                        `shouldBe` True

        it "constructs generated case-handler results at the enclosing nominal identity" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Option(..), stringCharAtOption, charIsAsciiControl);"
                        , "  def main : Bool = case stringCharAtOption \"a\\0b\" 1 of {"
                        , "    None -> false;"
                        , "    Some ch -> charIsAsciiControl ch"
                        , "  };"
                        , "}"
                        ]
            checked <- requireCheckedLocatedWithPrelude located
            mainBinding <- requireCheckedBinding "Main__main" checked
            let typeCheckEnv =
                    ElabPipeline.mkTypeCheckEnvWithResolvedTerms
                        [ (checkedBindingResolvedVar candidate, ProgramTypes.checkedBindingType candidate)
                        | checkedModule <- checkedProgramModules checked
                        , candidate <- checkedModuleBindings checkedModule
                        ]
                        Map.empty
            case ElabPipeline.typeCheckWithEnv typeCheckEnv (checkedBindingTerm mainBinding) of
                Left err ->
                    expectationFailure
                        ("generated case handler failed to typecheck: " ++ show err)
                Right inferred ->
                    alphaEqType inferred (ProgramTypes.checkedBindingType mainBinding)
                        `shouldBe` True
            (programRunOutput <$> runCheckedProgramOutput checked) `shouldBe` Right "true\n"

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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "hierarchy\n"

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
                case checkLocatedProgramWithPrelude located of
                    Left err -> expectationFailure ("check failed: " ++ show err) >> fail "check failed"
                    Right checked -> pure checked
            mainBinding <- requireCheckedBinding "Main__main" checked
            unresolvedTermVarRefs (checkedBindingTerm mainBinding) `shouldBe` []
            case applicationArgumentsForRuntimeName "__io_pure" (checkedBindingTerm mainBinding) of
                [Elab.ETyInst (Elab.EVarNode unitOccurrence) (Elab.InstAbstrRef _)] ->
                    case Elab.resolvedVarDetails unitOccurrence of
                        ConstructorId constructorRef ->
                            Symbol.symbolDefiningName
                                (constructorRefSymbol constructorRef)
                                `shouldBe` "Unit"
                        details ->
                            expectationFailure
                                ("expected resolved Unit constructor, got " ++ show details)
                arguments ->
                    expectationFailure
                        ( "expected __io_pure's Unit argument to retain its explicit Hyp, got "
                            ++ show arguments
                        )

        it "rejects inconsistent direct IO bind primitive arguments" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_bind (__io_pure 1) (λ(_n : Unit) __io_putStrLn \"world\");"
                        , "}"
                        ]
            checkLocatedProgramWithPrelude located `shouldSatisfy` isLeft

        it "rejects non-IO expressions for Prelude IO annotations" $ do
            intLocated <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = 1;"
                        , "}"
                        ]
            checkLocatedProgramWithPrelude intLocated
                `shouldSatisfy` either
                    ( \diagnostic ->
                        case diagnosticError diagnostic of
                            ProgramTypeMismatch {} ->
                                "type mismatch" `isInfixOf` renderProgramDiagnostic diagnostic
                            _ -> False
                    )
                    (const False)
            identityLocated <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = λx x;"
                        , "}"
                        ]
            checkLocatedProgramWithPrelude identityLocated
                `shouldSatisfy` either
                    ( \diagnostic ->
                        case diagnosticError diagnostic of
                            ProgramTypeShapeMismatch ProgramTypes.ProgramSourceArrowShape _ ->
                                let rendered = renderProgramDiagnostic diagnostic
                                 in "type mismatch" `isInfixOf` rendered
                                      && "internal pipeline detail" `isInfixOf` rendered
                            _ -> False
                    )
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
            checkLocatedProgramWithPrelude located
                `shouldSatisfy` either
                    ( \diagnostic ->
                        case diagnosticError diagnostic of
                            ProgramTypeMismatch {} ->
                                let rendered = renderProgramDiagnostic diagnostic
                                 in "type mismatch" `isInfixOf` rendered
                                      && "internal pipeline detail" `isInfixOf` rendered
                            _ -> False
                    )
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
            checkLocatedProgramWithPrelude located
                `shouldSatisfy` either
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
            checkLocatedProgramWithPrelude located
                `shouldSatisfy` either
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
            checkLocatedProgramWithPrelude located
                `shouldSatisfy` either
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
            checkLocatedProgramWithPrelude located
                `shouldSatisfy` either
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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "hello\n"

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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "hello\nworld\n"

        it "sequences an inline bind continuation through nested application Gamma" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, putStrLn);"
                        , "  def main : IO Unit ="
                        , "    bind (putStrLn \"hello\")"
                        , "      (λ(_done : Unit) putStrLn \"world\");"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located)
                `shouldBe` Right "hello\nworld\n"

        it "sequences direct IO primitives without rendering Unit" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_bind (__io_pure Unit) (λ(_n : Unit) __io_putStrLn \"world\");"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "world\n"

        it "runs pure IO Unit actions without stdout or Unit rendering" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, pure);"
                        , "  def main : IO Unit = pure Unit;"
                        , "}"
                        ]
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right ""

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
            checked <- requireCheckedLocatedWithPrelude located
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
            checked <- requireCheckedLocatedWithPrelude located
            mainBinding <- requireCheckedBinding "Main__main" checked
            resolvedLocalBinders (checkedBindingTerm mainBinding) `shouldSatisfy` any isGeneratedLocalRef
            generatedLocalIdentityValues (checkedBindingTerm mainBinding)
                `shouldSatisfy` all (`notElem` generatedDeferredIdentityValues mainBinding)
            (programRunOutput <$> runCheckedProgramOutput checked) `shouldBe` Right "nat\n"


        it "rejects duplicate deferred identities at lowered-batch construction" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let duplicateRef = deferredRefFromIdentity (UniqueIdentity 0) "$deferred"
                dataIdentity = generatedSymbolIdentity 3 SymbolType "Main" "Phantom" Nothing
                phantomData =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                obligation name identity =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseBindingIdentity =
                                loweredBindingIdentityFromDetails name (TopLevelId identity)
                            , deferredCaseRef = duplicateRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeTypeView = builtinBaseTypeView "Int"
                            , deferredCaseResultTypeView = builtinBaseTypeView "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered name identity =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails name (TopLevelId identity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = Surface.ELit (LInt 1)
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations =
                            Map.singleton duplicateRef (obligation name identity)
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            case
                mkModuleFinalizeContext
                    finalizeContext
                    [ lowered "Main__first" (generatedSymbolIdentity 1 SymbolValue "Main" "first" Nothing)
                    , lowered "Main__second" (generatedSymbolIdentity 2 SymbolValue "Main" "second" Nothing)
                    ]
                of
                    Left (ProgramPipelineError message) ->
                        message `shouldSatisfy` isInfixOf "duplicate deferred identities"
                    Left err ->
                        expectationFailure ("expected duplicate deferred identity rejection, got " ++ show err)
                    Right _ ->
                        expectationFailure "expected duplicate deferred identity rejection"

        it "rejects duplicate lowered binding identities before caching read contexts" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let duplicateIdentity = generatedSymbolIdentity 7 SymbolValue "Main" "dup" Nothing
                lowered name value =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails name (TopLevelId duplicateIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = Surface.ELit (LInt value)
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            case mkModuleFinalizeContext finalizeContext [lowered "Main__first" 1, lowered "Main__second" 2] of
                Left (ProgramPipelineError message) ->
                    message `shouldSatisfy` isInfixOf "duplicate binding identities"
                Left err ->
                    expectationFailure ("expected duplicate binding identity rejection, got " ++ show err)
                Right _ ->
                    expectationFailure "expected duplicate binding identity rejection"

        it "rejects deferred cases owned by a different binding identity" $ do
            finalizeContext <- requireFinalizeContext (mkElaborateScope Map.empty Map.empty Map.empty [])
            let deferredRef = deferredRefFromIdentity (UniqueIdentity 24) "$deferred"
                bindingIdentity = generatedSymbolIdentity 25 SymbolValue "Main" "main" Nothing
                wrongBindingIdentity = generatedSymbolIdentity 26 SymbolValue "Main" "main" Nothing
                dataIdentity = generatedSymbolIdentity 27 SymbolType "Main" "Phantom" Nothing
                bindingOwner =
                    loweredBindingIdentityFromDetails
                        "Main__main"
                        (TopLevelId bindingIdentity)
                wrongOwner =
                    loweredBindingIdentityFromDetails
                        "Main__main"
                        (TopLevelId wrongBindingIdentity)
                phantomData =
                    DataInfo
                        { dataInfoSymbol = dataIdentity
                        , dataTypeParams = []
                        , dataConstructors = []
                        }
                obligation =
                    DeferredCase
                        DeferredCaseCall
                            { deferredCaseBindingIdentity = wrongOwner
                            , deferredCaseRef = deferredRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeTypeView = builtinBaseTypeView "Int"
                            , deferredCaseResultTypeView = builtinBaseTypeView "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity = bindingOwner
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = Surface.ELit (LInt 1)
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.singleton deferredRef obligation
                        , loweredBindingExternalTypeViews = Map.empty
                        , loweredBindingExportedAsMain = False
                        }
            ProgramTypes.loweredIdentityRuntimeName wrongOwner
                `shouldBe` ProgramTypes.loweredIdentityRuntimeName bindingOwner
            wrongOwner `shouldNotBe` bindingOwner
            case finalizeBindingWithContext finalizeContext lowered of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "deferred case owned by binding"
                    message `shouldSatisfy` isInfixOf "Main__main"
                Left err ->
                    expectationFailure ("expected deferred case owner rejection, got " ++ show err)
                Right _ ->
                    expectationFailure "expected deferred case owner rejection"

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
                            { deferredCaseBindingIdentity =
                                loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                            , deferredCaseRef = payloadRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeTypeView = builtinBaseTypeView "Int"
                            , deferredCaseResultTypeView = builtinBaseTypeView "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = unknownSurfaceVar "$deferred"
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.singleton keyRef obligation
                        , loweredBindingExternalTypeViews =
                            Map.singleton "$deferred" (mkTypeView (STBase "Int") (STBase "Int"))
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
                            { deferredCaseBindingIdentity =
                                loweredBindingIdentityFromDetails "Main__first" (TopLevelId firstIdentity)
                            , deferredCaseRef = payloadRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeTypeView = builtinBaseTypeView "Int"
                            , deferredCaseResultTypeView = builtinBaseTypeView "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                firstLowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__first" (TopLevelId firstIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = unknownSurfaceVar "$deferred"
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.singleton keyRef obligation
                        , loweredBindingExternalTypeViews =
                            Map.singleton "$deferred" (mkTypeView (STBase "Int") (STBase "Int"))
                        , loweredBindingExportedAsMain = False
                        }
                secondLowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__second" (TopLevelId secondIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = Surface.ELit (LInt 1)
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.empty
                        , loweredBindingExternalTypeViews = Map.empty
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
                            { deferredCaseBindingIdentity =
                                loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                            , deferredCaseRef = payloadRef
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeTypeView = builtinBaseTypeView "Int"
                            , deferredCaseResultTypeView = builtinBaseTypeView "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered surfaceExpr obligations externalTypeViews =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = surfaceExpr
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = obligations
                        , loweredBindingExternalTypeViews = externalTypeViews
                        , loweredBindingExportedAsMain = False
                        }
                cachedLowered = lowered (Surface.ELit (LInt 1)) Map.empty Map.empty
                staleLowered =
                    lowered
                        (unknownSurfaceVar "$deferred")
                        (Map.singleton keyRef obligation)
                        (Map.singleton "$deferred" (mkTypeView (STBase "Int") (STBase "Int")))
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

        it "rejects same-named deferred external bindings instead of choosing an arbitrary identity" $ do
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
                            { deferredCaseBindingIdentity =
                                loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                            , deferredCaseRef = ref
                            , deferredCaseDataInfo = phantomData
                            , deferredCaseScrutineeTypeView = builtinBaseTypeView "Int"
                            , deferredCaseResultTypeView = builtinBaseTypeView "Int"
                            , deferredCaseExpectedArgCount = 0
                            }
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingExpectedTypeView = mkTypeView (STBase "Int") (STBase "Int")
                        , loweredBindingSurfaceExpr = unknownSurfaceVar placeholder
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations =
                            Map.fromList [(firstRef, obligation firstRef), (secondRef, obligation secondRef)]
                        , loweredBindingExternalTypeViews =
                            Map.singleton placeholder (mkTypeView (STBase "Int") (STBase "Int"))
                        , loweredBindingExportedAsMain = False
                        }
            finalizeBindingsAllowOpaqueWithContext finalizeContext [lowered]
                `shouldBe` Left (ProgramUnknownValue placeholder)

        it "rejects unresolved constructor identities on the opaque checked-binding path" $ do
            let scope = mkElaborateScope Map.empty Map.empty Map.empty []
                intIdentity = Builtins.builtinTypeIdentity "Int"
                ioIdentity = Builtins.builtinTypeIdentity "IO"
                constructorIdentity =
                    generatedSymbolIdentity
                        991730
                        SymbolConstructor
                        "Prelude"
                        "OpaqueConstructor"
                        (Just (SymbolOwnerType ioIdentity))
                bindingIdentity = generatedSymbolIdentity 991731 SymbolValue "Main" "main" Nothing
                deferredRef = deferredRefFromIdentity (UniqueIdentity 991732) "$opaque_constructor"
                constructorTy = STArrow (STBase "Int") (STCon "IO" (STBase "Int" :| []))
                constructorIdentityTy =
                    STArrow
                        (STBase (symbolIdentityStableName intIdentity))
                        (STCon (symbolIdentityStableName ioIdentity) (STBase (symbolIdentityStableName intIdentity) :| []))
                constructorView =
                    ( setTypeViewHeadIdentities
                        ( Map.fromList
                            [ ("Int", intIdentity)
                            , (symbolIdentityStableName intIdentity, intIdentity)
                            , ("IO", ioIdentity)
                            , (symbolIdentityStableName ioIdentity, ioIdentity)
                            ]
                        )
                        (mkTypeView constructorTy constructorIdentityTy)
                    )
                constructorInfo =
                    ConstructorInfo
                        { ctorInfoSymbol = constructorIdentity
                        , ctorRuntimeName = "Prelude__OpaqueConstructor"
                        , ctorTypeView = constructorView
                        , ctorOwningTypeIdentity = ioIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                deferred =
                    DeferredConstructor
                        DeferredConstructorCall
                            { deferredConstructorRef = deferredRef
                            , deferredConstructorInfo = constructorInfo
                            , deferredConstructorArgCount = 1
                            , deferredConstructorSourceTypeView = constructorView
                            , deferredConstructorOccurrenceTypeView = constructorView
                            , deferredConstructorInstBinders = []
                            , deferredConstructorInitialSubst = ProgramTypes.emptyTypeBinderSubst
                            , deferredConstructorBindingMode = ProgramTypes.DeferredBindingMonomorphic
                            }
                lowered =
                    LoweredBinding
                        { loweredBindingIdentity =
                            loweredBindingIdentityFromDetails "Main__main" (TopLevelId bindingIdentity)
                        , loweredBindingSourceTypeView = constructorView
                        , loweredBindingExpectedTypeView = constructorView
                        , loweredBindingSurfaceExpr =
                            Surface.EResolvedVar (DeferredId deferredRef) (deferredRefName deferredRef)
                        , loweredBindingResolvedLocalIdentities = []
                        , loweredBindingResolvedEvidenceIdentities = []
                        , loweredBindingDeferredObligations = Map.singleton deferredRef deferred
                        , loweredBindingExternalTypeViews = Map.singleton (deferredRefName deferredRef) constructorView
                        , loweredBindingExportedAsMain = False
                        }
            finalizeContext <- requireFinalizeContext scope
            finalizeBindingsAllowOpaqueWithContext finalizeContext [lowered]
                `shouldSatisfy` either
                    (isInfixOf "checked XmlfTerm retained unresolved variables" . show)
                    (const False)

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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "foo\n"

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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "scoped\n"

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
            checked <- requireCheckedLocatedWithPrelude located
            actionBinding <- requireCheckedBinding "Main__action" checked
            mainBinding <- requireCheckedBinding "Main__main" checked
            let typeCheckEnv =
                    ElabPipeline.mkTypeCheckEnvWithResolvedTerms
                        [ (checkedBindingResolvedVar candidate, ProgramTypes.checkedBindingType candidate)
                        | checkedModule <- checkedProgramModules checked
                        , candidate <- checkedModuleBindings checkedModule
                        ]
                        Map.empty
                actionTerm = checkedBindingTerm actionBinding
                endpointEvidence =
                    polymorphicApplicationEndpointEvidence
                        typeCheckEnv
                        actionTerm
                validEndpoint (sourceRef, endpointTy, Right argumentTy) =
                    not
                        ( any
                            (Elab.typeBinderRefsSameIdentity sourceRef)
                            (freeTypeVarRefsType endpointTy)
                        )
                        && alphaEqType endpointTy argumentTy
                validEndpoint _ = False
            case filter validEndpoint endpointEvidence of
                _ : _ -> pure ()
                [] ->
                    expectationFailure
                        ( "expected pure's instantiation endpoint to equal its argument type without leaking the source forall binder; evidence="
                            ++ show endpointEvidence
                            ++ "; term="
                            ++ show actionTerm
                        )
            case ElabPipeline.typeCheckWithEnv typeCheckEnv actionTerm of
                Left err ->
                    expectationFailure
                        ("deferred case action failed to typecheck: " ++ show err)
                Right inferred ->
                    alphaEqType inferred (ProgramTypes.checkedBindingType actionBinding)
                        `shouldBe` True
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
            checked <- requireCheckedLocatedWithPrelude located
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
            checked <- requireCheckedLocatedWithPrelude located
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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "int\n"

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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "eq\n"

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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "picked\n"

        it "looks up local resolved evidence by identity instead of binder runtime name" $ do
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
            checked <- requireCheckedLocatedWithPrelude located
            binding <- requireCheckedBinding "Main__selected" checked
            checkedBindingDeferredObligations binding `shouldBe` Map.empty
            -- Figure 15.3.5 may construct the constrained producer under a
            -- type abstraction and immediately specialize it.  Inspect the
            -- evidence-lambda spine modulo only those explicit type redexes;
            -- keep the original construction intact for the runtime check.
            case reduceLeadingTypeInstantiationRedexesFully (checkedBindingTerm binding) of
                Elab.ELam
                    ResolvedVar{resolvedVarDetails = EvidenceId pickBinderRef}
                    pickBody ->
                        case reduceLeadingTypeInstantiationRedexesFully pickBody of
                            Elab.ELam
                                ResolvedVar{resolvedVarDetails = EvidenceId eqBinderRef}
                                body -> do
                                    resolvedEvidenceOccurrences body
                                        `shouldMatchList` [pickBinderRef, eqBinderRef]
                                    let stalePickRef = renameLocalRef "$stale_pick_binder" pickBinderRef
                                        staleEqRef = renameLocalRef "$stale_eq_binder" eqBinderRef
                                        staleTerm =
                                            renameEvidenceBinderRef eqBinderRef staleEqRef
                                                . renameEvidenceBinderRef pickBinderRef stalePickRef
                                                $ checkedBindingTerm binding
                                        checked' = replaceCheckedBindingTerm "Main__selected" staleTerm checked
                                    localRefIdentity stalePickRef `shouldBe` localRefIdentity pickBinderRef
                                    localRefIdentity staleEqRef `shouldBe` localRefIdentity eqBinderRef
                                    localRefName stalePickRef `shouldNotBe` localRefName pickBinderRef
                                    localRefName staleEqRef `shouldNotBe` localRefName eqBinderRef
                                    (programRunOutput <$> runCheckedProgramOutput checked')
                                        `shouldBe` Right "picked\n"
                            other ->
                                expectationFailure
                                    ("expected second resolved evidence binder, got " ++ show other)
                other ->
                    expectationFailure
                        ("expected two resolved evidence binders, got " ++ show other)

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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "boxed\n"

        it "rejects recursive IO main lookup without hanging" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = main;"
                        , "}"
                        ]
            runLocatedProgramOutputWithPrelude located
                `shouldSatisfy` either
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
            (programRunOutput <$> runLocatedProgramOutputWithPrelude located) `shouldBe` Right "done\n"

        it "supports IO mains whose result type is not Unit" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (IO, pure);"
                        , "  def main : IO Int = pure 1;"
                        , "}"
                        ]
            let result = runLocatedProgramOutputWithPrelude located
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
            runLocatedProgramWithPrelude located
                `shouldSatisfy` either
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
            checked <- requireCheckedLocatedWithPrelude located
            let checked' =
                    poisonCheckedBindingSourceTypeHeads
                        "Main__discard"
                        (STBase "Unit")
                        checked
            runCheckedProgramOutput checked'
                `shouldSatisfy` either
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
            runLocatedProgramWithPrelude located
                `shouldSatisfy` either
                    ( \diagnostic ->
                        all
                            (`isInfixOf` renderProgramDiagnostic diagnostic)
                            [ "run-program does not support IO dependencies yet"
                            , "__io_pure"
                            ]
                    )
                    (const False)

        it "rejects opaque primitive dependencies carried by resolved identity" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : Unit = (λ(_action : IO Unit) Unit) (__io_pure Unit);"
                        , "}"
                        ]
            checked <- requireCheckedLocatedWithPrelude located
            mainBinding <- requireCheckedBinding "Main__main" checked
            let ioPureIdentity =
                    Builtins.builtinValueIdentity
                        ( PrimitiveInventory.nativeIOPrimitiveName
                            PrimitiveInventory.PrimitiveIOPure
                        )
            resolvedTopLevelIdentities (checkedBindingTerm mainBinding)
                `shouldSatisfy` any (Symbol.sameSymbolIdentity ioPureIdentity)
            case runCheckedProgramOutput checked of
                Left (ProgramPipelineError text) -> do
                    text `shouldSatisfy` isInfixOf "run-program does not support IO dependencies yet"
                    text `shouldSatisfy` isInfixOf "__io_pure"
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
            runLocatedProgramWithPrelude located
                `shouldSatisfy` either
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

        it "checks a nested constructor through its argument-side root Gamma" $ do
            let programText =
                    unlines
                        [ "module Main export (SeedSpan(..), SeedKind(..), SeedDiagnostic(..), SeedResult(..), main) {"
                        , "  data SeedSpan ="
                        , "      SeedSpan : SeedSpan;"
                        , ""
                        , "  data SeedKind ="
                        , "      SeedUnknown : SeedKind;"
                        , ""
                        , "  data SeedDiagnostic ="
                        , "      SeedDiagnostic : SeedSpan -> SeedKind -> SeedDiagnostic;"
                        , ""
                        , "  data SeedResult ="
                        , "      SeedError : SeedDiagnostic -> SeedResult;"
                        , ""
                        , "  def main : SeedResult ="
                        , "    SeedError (SeedDiagnostic SeedSpan SeedUnknown);"
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
            checked <- requireChecked program
            binding <- requireCheckedBinding "Main__boxPure" checked
            case (ProgramTypes.checkedBindingType binding, checkedBindingTerm binding) of
                (Elab.TForallRef typeRef _ _, Elab.ETyAbsRef termRef _ _) ->
                    Elab.typeBinderRefsSameIdentity typeRef termRef `shouldBe` True
                other ->
                    expectationFailure
                        ("expected parameterized constructor producer type abstraction, got " ++ show other)
            let typeCheckEnv =
                    ElabPipeline.mkTypeCheckEnvWithResolvedTerms
                        [ (checkedBindingResolvedVar candidate, ProgramTypes.checkedBindingType candidate)
                        | checkedModule <- checkedProgramModules checked
                        , candidate <- checkedModuleBindings checkedModule
                        ]
                        Map.empty
            ElabPipeline.typeCheckWithEnv typeCheckEnv (checkedBindingTerm binding)
                `shouldBe` Right (ProgramTypes.checkedBindingType binding)

        it "checks an unannotated lambda against a leading quantified binder by identity" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : ∀ a. a -> a = λ(value) (value : a);"
                        , "}"
                        ]
            checked <- requireChecked program
            binding <- requireCheckedBinding "Main__main" checked
            case (ProgramTypes.checkedBindingType binding, checkedBindingTerm binding) of
                ( Elab.TForallRef typeRef Nothing (Elab.TArrow (Elab.TVarRef domainRef) (Elab.TVarRef codomainRef))
                    , Elab.ETyAbsRef termRef Nothing (Elab.ELam parameter _)
                    ) -> do
                        Elab.typeBinderRefsSameIdentity typeRef domainRef `shouldBe` True
                        Elab.typeBinderRefsSameIdentity typeRef codomainRef `shouldBe` True
                        Elab.typeBinderRefsSameIdentity typeRef termRef `shouldBe` True
                        alphaEqType (Elab.resolvedVarType parameter) (Elab.TVarRef typeRef) `shouldBe` True
                other ->
                    expectationFailure
                        ("expected one shared forall identity across the producer lambda, got " ++ show other)
            case typeCheck (checkedBindingTerm binding) of
                Left err -> expectationFailure ("quantified unannotated lambda failed to typecheck: " ++ show err)
                Right inferred ->
                    alphaEqType inferred (ProgramTypes.checkedBindingType binding) `shouldBe` True

        it "keeps an exact self-operated quantified lambda declaration unbounded" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : ∀ a. a -> a = λvalue value;"
                        , "}"
                        ]
            checked <- requireChecked program
            binding <- requireCheckedBinding "Main__main" checked
            case (ProgramTypes.checkedBindingType binding, checkedBindingTerm binding) of
                ( Elab.TForallRef typeRef Nothing (Elab.TArrow (Elab.TVarRef domainRef) (Elab.TVarRef codomainRef))
                    , Elab.ETyAbsRef termRef Nothing (Elab.ELam parameter _)
                    ) -> do
                        Elab.typeBinderRefsSameIdentity typeRef domainRef `shouldBe` True
                        Elab.typeBinderRefsSameIdentity typeRef codomainRef `shouldBe` True
                        Elab.typeBinderRefsSameIdentity typeRef termRef `shouldBe` True
                        alphaEqType
                            (Elab.resolvedVarType parameter)
                            (Elab.TVarRef typeRef)
                            `shouldBe` True
                other ->
                    expectationFailure
                        ("expected an unbounded shared-identity forall, got " ++ show other)
            typeCheck (checkedBindingTerm binding)
                `shouldBe` Right (ProgramTypes.checkedBindingType binding)

        it "checks an unannotated parameter in a quantified constructor producer" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Box(..), boxPure, main) {"
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  def boxPure : ∀ a. a -> Box a ="
                        , "    λ(value) (Box value : Box a);"
                        , ""
                        , "  def main : Box String = boxPure \"ok\";"
                        , "}"
                        ]
            checked <- requireChecked program
            binding <- requireCheckedBinding "Main__boxPure" checked
            case (ProgramTypes.checkedBindingType binding, checkedBindingTerm binding) of
                ( Elab.TForallRef sourceRef Nothing _
                    , Elab.ETyAbsRef outwardRef Nothing
                        ( Elab.ELam
                            parameter
                            ( Elab.EApp
                                ( Elab.ETyInst
                                    (Elab.EVarNode ResolvedVar{resolvedVarDetails = ConstructorId _})
                                    (Elab.InstApp (Elab.TVarRef constructorArgRef))
                                )
                                _
                            )
                        )
                    ) -> do
                        Elab.typeBinderRefsSameIdentity sourceRef outwardRef `shouldBe` True
                        alphaEqType
                            (Elab.resolvedVarType parameter)
                            (Elab.TVarRef sourceRef)
                            `shouldBe` True
                        Elab.typeBinderRefsSameIdentity constructorArgRef sourceRef `shouldBe` True
                other ->
                    expectationFailure
                        ("expected one identity-bearing quantified constructor producer, got " ++ show other)
            let typeCheckEnv =
                    ElabPipeline.mkTypeCheckEnvWithResolvedTerms
                        [ (checkedBindingResolvedVar candidate, ProgramTypes.checkedBindingType candidate)
                        | checkedModule <- checkedProgramModules checked
                        , candidate <- checkedModuleBindings checkedModule
                        ]
                        Map.empty
            case ElabPipeline.typeCheckWithEnv typeCheckEnv (checkedBindingTerm binding) of
                Left err ->
                    expectationFailure
                        ("scoped constructor producer failed to typecheck: " ++ show err)
                Right inferred ->
                    alphaEqType inferred (ProgramTypes.checkedBindingType binding) `shouldBe` True

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
                    (programRunOutput <$> runCheckedProgramOutput checked)
                        `shouldBe` Right "true\n"
                Left err -> expectationFailure ("checkProgram failed: " ++ show err)

        it "does not treat a local value named id as identity in case scrutinees" $ do
            let programText localName =
                    unlines
                        [ "module Main export (B(..), main) {"
                        , "  data B ="
                        , "      BZ : B"
                        , "    | BO : B;"
                        , ""
                        , "  def main : B ="
                        , "    let " ++ localName ++ " : B -> B = λx BO in"
                        , "    case (" ++ localName ++ " BZ) of {"
                        , "      BZ -> BZ;"
                        , "      BO -> BO"
                        , "    };"
                        , "}"
                        ]
                assertLocalName localName = do
                    program <- requireParsed (programText localName)
                    (prettyValue <$> runProgram program) `shouldBe` Right "BO"
            mapM_ assertLocalName ["id", "choose"]

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
                    diagnosticError diagnostic `shouldBe` ProgramAmbiguousConstructorUse "None"
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
                            | checkedModule <- Checked.checkedProgramModules checked
                            , ProgramTypes.checkedModuleName checkedModule == "Main"
                            , binding <- ProgramTypes.checkedModuleBindings checkedModule
                            , ProgramTypes.checkedBindingName binding == "Main__main"
                            ]
                        boxIdentityHeads =
                            [ symbolIdentityStableName (ProgramTypes.dataInfoSymbolIdentity dataInfo)
                            | checkedModule <- Checked.checkedProgramModules checked
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

        it "keeps imported same-name constructor fields distinct from local results" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Core export (T(..)) {"
                        , "  data T = External : T;"
                        , "}"
                        , "module Main export (T(..), main) {"
                        , "  import Core as A;"
                        , "  data T = Wrap : A.T -> T;"
                        , "  def main : Bool = case Wrap A.External of {"
                        , "    Wrap _ -> true"
                        , "  };"
                        , "}"
                        ]
            checked <- requireChecked program
            coreData <- requireCheckedData "Core" "T" checked
            mainData <- requireCheckedData "Main" "T" checked
            wrap <- requireDataConstructor "Wrap" mainData
            let coreIdentity = ProgramTypes.dataInfoSymbolIdentity coreData
                mainIdentity = ProgramTypes.dataInfoSymbolIdentity mainData
            coreIdentity `shouldNotBe` mainIdentity
            case ProgramTypes.constructorInfoArgViews wrap of
                [fieldView] ->
                    ProgramTypes.typeViewRootHeadIdentity fieldView `shouldBe` Just coreIdentity
                fieldViews ->
                    expectationFailure ("expected one Wrap field, got " ++ show fieldViews)
            ProgramTypes.typeViewRootHeadIdentity (ProgramTypes.constructorInfoResultView wrap)
                `shouldBe` Just mainIdentity

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
                    resolved{resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
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
                        syntax{moduleDecls = map poisonDecl (moduleDecls syntax)}

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
                    case [ resolvedSymbolIdentity (classDeclName classDecl)
                         | resolvedModule <- resolvedProgramModules resolved
                         , DeclClass classDecl <- moduleDecls (resolvedModuleSyntax resolvedModule)
                         , classDeclDisplayName classDecl == name
                         ] of
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
                    resolved{resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
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
                        syntax{moduleDecls = map poisonDecl (moduleDecls syntax)}

                    poisonDecl decl =
                        case decl of
                            DeclClass classDecl ->
                                DeclClass classDecl{classDeclMethods = map poisonMethod (classDeclMethods classDecl)}
                            _ -> decl

                    poisonMethod sig
                        | methodSigDisplayName sig == replacement =
                            sig{methodSigName = mapResolvedSymbolIdentity (const targetIdentity) (methodSigName sig)}
                    poisonMethod sig = sig

                resolvedMethodIdentity name resolved =
                    case [ resolvedSymbolIdentity (methodSigName methodSig)
                         | resolvedModule <- resolvedProgramModules resolved
                         , DeclClass classDecl <- moduleDecls (resolvedModuleSyntax resolvedModule)
                         , methodSig <- classDeclMethods classDecl
                         , methodSigDisplayName methodSig == name
                         ] of
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

        it "rejects stale resolved instance method identities as missing canonical methods" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  instance Eq Int {"
                        , "    eq = λleft λright true;"
                        , "  }"
                        , "  def main : Bool = eq 1 1;"
                        , "}"
                        ]
                poisonInstanceMethodIdentity resolved =
                    resolved{resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
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
                        syntax{moduleDecls = map poisonDecl (moduleDecls syntax)}

                    poisonDecl decl =
                        case decl of
                            DeclInstance instanceDecl ->
                                DeclInstance instanceDecl{instanceDeclMethods = map poisonMethodDef (instanceDeclMethods instanceDecl)}
                            _ -> decl

                    poisonMethodDef methodDef =
                        methodDef
                            { methodDefName =
                                mapResolvedSymbolIdentity
                                    (renameSymbolDefiningName "$stale_eq_method")
                                    (methodDefName methodDef)
                            }
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    checkResolvedProgram (poisonInstanceMethodIdentity resolved)
                        `shouldBe` Left (ProgramMissingInstanceMethod "Eq" "eq")

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

        it "rejects export symbols whose unique id matches but payload is stale" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Int = 1;"
                        , "}"
                        ]
                poisonExports resolved =
                    resolved{resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
                poisonModule resolvedModule =
                    resolvedModule
                        { resolvedModuleSemantic =
                            (resolvedModuleSemantic resolvedModule)
                                { resolvedSemanticModuleSyntax =
                                    poisonSyntax (resolvedModuleSyntax resolvedModule)
                                }
                        }
                poisonSyntax syntax =
                    syntax{moduleExports = fmap (map poisonExport) (moduleExports syntax)}
                poisonExport item =
                    case item of
                        ExportValue symbol ->
                            ExportValue (mapResolvedSymbolIdentity (renameSymbolDefiningName "$stale_main_export") symbol)
                        _ -> item
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    checkResolvedProgram (poisonExports resolved) `shouldBe` Left (ProgramExportNotLocal "main")

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

        it "keeps derived recursive List Eq exact-edge destinations gen-bound" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  data List a ="
                        , "      Nil : List a"
                        , "    | Cons : a -> List a -> List a"
                        , "    deriving Eq;"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

        it "checks Prelude-shaped successive Eq derivations with application-discharged intermediates" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  class Show a {"
                        , "    show : a -> String;"
                        , "  }"
                        , "  class Functor (f :: * -> *) {"
                        , "    map : ∀ a b. (a -> b) -> f a -> f b;"
                        , "  }"
                        , "  class Functor f => Applicative (f :: * -> *) {"
                        , "    pure : ∀ a. a -> f a;"
                        , "    ap : ∀ a b. f (a -> b) -> f a -> f b;"
                        , "  }"
                        , "  class Applicative m => Monad (m :: * -> *) {"
                        , "    bind : ∀ a b. m a -> (a -> m b) -> m b;"
                        , "  }"
                        , "  data Unit ="
                        , "      Unit : Unit;"
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat"
                        , "    deriving Eq;"
                        , "  data Option a ="
                        , "      None : Option a"
                        , "    | Some : a -> Option a"
                        , "    deriving Eq;"
                        , "  data List a ="
                        , "      Nil : List a"
                        , "    | Cons : a -> List a -> List a"
                        , "    deriving Eq;"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` isRight

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

        it "does not bypass visible type identities for builtin spellings" $ do
            let programText =
                    unlines
                        [ "module Main export (Int(..)) {"
                        , "  data Int ="
                        , "      LocalInt : Int;"
                        , "}"
                        ]
            program <- requireParsed programText
            resolveProgram program `shouldBe` Left (ProgramAmbiguousUnqualifiedReference "Int")

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

        it "scopes leading definition forall identities over RHS annotations" $ do
            let programText =
                    unlines
                        [ "module Main export (keep) {"
                        , "  def keep : ∀ a. a -> a = λ(value : a) (value : a);"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    case
                        [ def
                        | resolvedModule <- resolvedProgramModules resolved
                        , DeclDef def <- moduleDecls (resolvedModuleSyntax resolvedModule)
                        ] of
                        [def] ->
                            case (defDeclType def, defDeclExpr def) of
                                ( ConstrainedType [] (RSTForall binder Nothing (RSTArrow (RSTVar dom) (RSTVar cod)))
                                    , ELam Param{paramType = Just (RSTVar paramRef)} (EAnn _ (RSTVar annRef))
                                    ) -> do
                                        let identities =
                                                map
                                                    resolvedTypeBinderIdentity
                                                    [binder, dom, cod, paramRef, annRef]
                                        length (nub identities) `shouldBe` 1
                                other ->
                                    expectationFailure
                                        ("expected one forall identity across signature and RHS, got " ++ show other)
                        other -> expectationFailure ("expected one resolved definition, got " ++ show other)

        it "keeps nested forall identities local while the leading forall scopes the RHS" $ do
            let programText =
                    unlines
                        [ "module Main export (keep) {"
                        , "  def keep : ∀ a. (∀ a. a -> a) -> a -> a ="
                        , "    λ(poly) λ(value : a) (value : a);"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    case
                        [ def
                        | resolvedModule <- resolvedProgramModules resolved
                        , DeclDef def <- moduleDecls (resolvedModuleSyntax resolvedModule)
                        ] of
                        [def] ->
                            case (defDeclType def, defDeclExpr def) of
                                ( ConstrainedType
                                        []
                                        ( RSTForall
                                                outer
                                                Nothing
                                                ( RSTArrow
                                                        (RSTForall inner Nothing (RSTArrow (RSTVar innerDom) (RSTVar innerCod)))
                                                        (RSTArrow (RSTVar outerDom) (RSTVar outerCod))
                                                    )
                                            )
                                    , ELam _ (ELam Param{paramType = Just (RSTVar paramRef)} (EAnn _ (RSTVar annRef)))
                                    ) -> do
                                        let outerIdentity = resolvedTypeBinderIdentity outer
                                            innerIdentity = resolvedTypeBinderIdentity inner
                                        innerIdentity `shouldNotBe` outerIdentity
                                        map resolvedTypeBinderIdentity [innerDom, innerCod]
                                            `shouldBe` replicate 2 innerIdentity
                                        map resolvedTypeBinderIdentity [outerDom, outerCod, paramRef, annRef]
                                            `shouldBe` replicate 4 outerIdentity
                                other ->
                                    expectationFailure
                                        ("expected nested forall shadowing with an outer RHS scope, got " ++ show other)
                        other -> expectationFailure ("expected one resolved definition, got " ++ show other)

        it "scopes a local binding's leading forall identity over its RHS only" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Int ="
                        , "    let keep : ∀ a. a -> a = λ(value : a) (value : a)"
                        , "    in keep 1;"
                        , "}"
                        ]
            program <- requireParsed programText
            case resolveProgram program of
                Left err -> expectationFailure ("expected resolve success, got " ++ show err)
                Right resolved ->
                    case
                        [ defDeclExpr def
                        | resolvedModule <- resolvedProgramModules resolved
                        , DeclDef def <- moduleDecls (resolvedModuleSyntax resolvedModule)
                        ] of
                        [ ELet
                                _
                                (Just (RSTForall binder Nothing (RSTArrow (RSTVar dom) (RSTVar cod))))
                                (ELam Param{paramType = Just (RSTVar paramRef)} (EAnn _ (RSTVar annRef)))
                                _
                            ] -> do
                                let identity = resolvedTypeBinderIdentity binder
                                map resolvedTypeBinderIdentity [dom, cod, paramRef, annRef]
                                    `shouldBe` replicate 4 identity
                        other ->
                            expectationFailure
                                ("expected one local forall identity across its type and RHS, got " ++ show other)
            checkProgram program `shouldSatisfy` isRight

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
                                [DeclClass{}, DeclData{}, DeclInstance instDecl, DeclDef defDecl] -> do
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
                                            ELam
                                                Param{paramType = Just (RSTVar paramRef0)}
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

        it "rejects duplicate and conflicting resolved symbol identities before checking local metadata" $ do
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
                conflictingCtorIdentity = renameSymbolDefiningName "$stale_A" sharedCtorIdentity
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
                ctorConflict =
                    mkResolvedSymbol
                        conflictingCtorIdentity
                        "B"
                        "B"
                        (SymbolLocal "Main")
                mainValue =
                    mkResolvedSymbol
                        (generatedSymbolIdentity 9403 SymbolValue "Main" "main" Nothing)
                        "main"
                        "main"
                        (SymbolLocal "Main")
                resolvedScopeFor secondCtor =
                    ResolvedScope
                        { resolvedScopeValues = Map.fromList [("A", ctorA), ("B", secondCtor), ("main", mainValue)]
                        , resolvedScopeTypes = Map.singleton "Box" boxType
                        , resolvedScopeClasses = Map.empty
                        , resolvedScopeModules = Map.empty
                        }
                resolvedModuleFor secondCtor =
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
                                                            { constructorDeclName = secondCtor
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
                                        { resolvedLocalValues = Map.fromList [("A", [ctorA]), ("B", [secondCtor]), ("main", [mainValue])]
                                        , resolvedLocalTypes = Map.singleton "Box" [boxType]
                                        , resolvedLocalClasses = Map.empty
                                        }
                                , resolvedSemanticModuleScope = resolvedScopeFor secondCtor
                                , resolvedSemanticModuleExports = resolvedScopeFor secondCtor
                                }
                        , resolvedModuleDiagnosticAdapter =
                            ResolvedModuleDiagnosticAdapter
                                { resolvedDiagnosticReferences = []
                                }
                        }
            case checkResolvedProgram (ResolvedProgram [resolvedModuleFor ctorB]) of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "duplicate resolved symbol identity"
                    message `shouldSatisfy` isInfixOf (symbolIdentityStableName sharedCtorIdentity)
                other ->
                    expectationFailure ("expected duplicate resolved symbol identity rejection, got " ++ show other)
            case checkResolvedProgram (ResolvedProgram [resolvedModuleFor ctorConflict]) of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "conflicting resolved symbol identity payload"
                    message `shouldSatisfy` isInfixOf (symbolIdentityStableName sharedCtorIdentity)
                other ->
                    expectationFailure ("expected conflicting resolved symbol identity payload rejection, got " ++ show other)

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
                                    | checkedModule <- Checked.checkedProgramModules checked
                                    , instanceInfo <- ProgramTypes.checkedModuleInstances checkedModule
                                    , valueInfo <- Map.elems (instanceMethodsByIdentity instanceInfo)
                                    , let identity = symbolUniqueIdentity (valueInfoSymbol valueInfo)
                                    ]
                                instanceMethodNames =
                                    [ symbolDefiningName (valueInfoSymbol valueInfo)
                                    | checkedModule <- Checked.checkedProgramModules checked
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
                        `shouldSatisfy` \resolvedVar@ResolvedVar{resolvedVarDetails} ->
                            Elab.resolvedVarName resolvedVar == "main"
                                && Elab.resolvedVarRuntimeName resolvedVar == "Main__main"
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

        it "rejects resolved references whose unique id matches but payload is stale" $ do
            let valueProgramText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def helper : Int = 1;"
                        , "  def main : Int = helper;"
                        , "}"
                        ]
                typeProgramText =
                    unlines
                        [ "module Main export (main) {"
                        , "  data Box = Box : Box;"
                        , "  def main : Box = Box;"
                        , "}"
                        ]
                poisonMainDef f resolved =
                    resolved{resolvedProgramModules = map poisonModule (resolvedProgramModules resolved)}
                  where
                    poisonModule resolvedModule =
                        resolvedModule
                            { resolvedModuleSemantic =
                                (resolvedModuleSemantic resolvedModule)
                                    { resolvedSemanticModuleSyntax =
                                        poisonSyntax (resolvedModuleSyntax resolvedModule)
                                    }
                            }
                    poisonSyntax syntax =
                        syntax{moduleDecls = map poisonDecl (moduleDecls syntax)}
                    poisonDecl decl =
                        case decl of
                            DeclDef defDecl
                                | refDisplayName (defDeclName defDecl) == "main" ->
                                    DeclDef (f defDecl)
                            _ -> decl
                staleSymbol replacement sourceName symbol =
                    mkResolvedSymbol
                        (renameSymbolDefiningName replacement (Symbol.resolvedSymbolIdentity symbol))
                        sourceName
                        sourceName
                        (Symbol.symbolSpellingOrigin (Symbol.resolvedSymbolSpelling symbol))
                poisonValueRef =
                    poisonMainDef (\defDecl -> defDecl{defDeclExpr = poisonExpr (defDeclExpr defDecl)})
                  where
                    poisonExpr expr =
                        case expr of
                            EVar (ResolvedGlobalValue symbol)
                                | refDisplayName symbol == "helper" ->
                                    EVar (ResolvedGlobalValue (staleSymbol "$stale_helper" "wrong.helper" symbol))
                            _ -> expr
                poisonTypeHead =
                    poisonMainDef (\defDecl -> defDecl{defDeclType = poisonConstrainedType (defDeclType defDecl)})
                  where
                    poisonConstrainedType constrained =
                        constrained{constrainedBody = poisonType (constrainedBody constrained)}
                    poisonType ty =
                        case ty of
                            RSTBase symbol
                                | refDisplayName symbol == "Box" ->
                                    RSTBase (staleSymbol "$stale_Box" "wrong.Box" symbol)
                            _ -> ty
            valueProgram <- requireParsed valueProgramText
            typeProgram <- requireParsed typeProgramText
            case (resolveProgram valueProgram, resolveProgram typeProgram) of
                (Right valueResolved, Right typeResolved) -> do
                    checkResolvedProgram (poisonValueRef valueResolved) `shouldBe` Left (ProgramUnknownValue "wrong.helper")
                    checkResolvedProgram (poisonTypeHead typeResolved) `shouldBe` Left (ProgramUnknownType "wrong.Box")
                other -> expectationFailure ("expected resolve success, got " ++ show other)

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

    describe "MLF.Program paper bounded-instantiation construction" $ do
        it "uses N when omega is instantiated at its explicit bound" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (omega, id, main) {"
                        , "  def omega : ∀(result ⩾ ∀ a. a -> a). (∀ a. a -> a) -> result ="
                        , "    λ(g : ∀ a. a -> a) g g;"
                        , "  def id : ∀ a. a -> a = λx x;"
                        , "  def main : Bool ="
                        , "    let recovered : ∀ a. a -> a = omega id in recovered true;"
                        , "}"
                        ]
            checked <- requireChecked program
            omegaBinding <- requireCheckedBinding "Main__omega" checked
            idBinding <- requireCheckedBinding "Main__id" checked
            mainBinding <- requireCheckedBinding "Main__main" checked
            let omegaVar = checkedBindingResolvedVar omegaBinding
                idVar = checkedBindingResolvedVar idBinding
                boundedUses =
                    [ ()
                    | Elab.EApp
                        (Elab.ETyInst (Elab.EVarNode omegaOccurrence) Elab.InstElim)
                        (Elab.EVarNode idOccurrence) <-
                        xmlfSubterms (checkedBindingTerm mainBinding)
                    , Elab.resolvedVarSameIdentity omegaVar omegaOccurrence
                    , Elab.resolvedVarSameIdentity idVar idOccurrence
                    ]
            case boundedUses of
                [] ->
                    expectationFailure
                        ( "expected explicit-bound N use, got "
                            ++ show (checkedBindingTerm mainBinding)
                        )
                _ -> pure ()

        it "keeps the recursive Nat application identity-correct after bound computations normalize" $ do
            program <-
                requireParsed
                    =<< readFile "test/programs/recursive-adt/plain-recursive-nat.mlfp"
            checked <- requireChecked program
            natData <- requireCheckedData "NatPlain" "Nat" checked
            zeroCtor <- requireDataConstructor "Zero" natData
            succCtor <- requireDataConstructor "Succ" natData
            isZeroBinding <- requireCheckedBinding "NatPlain__isZero" checked
            peelBinding <- requireCheckedBinding "NatPlain__peel" checked
            mainBinding <- requireCheckedBinding "NatPlain__main" checked

            case applicationArgumentsForResolvedVar
                (checkedBindingResolvedVar isZeroBinding)
                (checkedBindingTerm mainBinding) of
                [isZeroArgument] ->
                    case applicationArgumentsForResolvedVar
                        (checkedBindingResolvedVar peelBinding)
                        isZeroArgument of
                        [peelArgument] ->
                            case applicationArgumentsForConstructor succCtor peelArgument of
                                [zeroArgument]
                                    | termHeadConstructorRef zeroArgument
                                        == Just (ProgramTypes.constructorRefFromInfo zeroCtor) ->
                                        pure ()
                                arguments ->
                                    expectationFailure
                                        ( "expected identity-linked Succ Zero construction, got "
                                            ++ show arguments
                                        )
                        arguments ->
                            expectationFailure
                                ("expected identity-linked peel application, got " ++ show arguments)
                arguments ->
                    expectationFailure
                        ( "expected identity-linked isZero application, got "
                            ++ show arguments
                        )

    describe "MLF.Program eMLF-owned `.mlfp` integration" $ do
        it "fails for a real type mismatch instead of the old infer-lambda gate" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Int = let id = λx x in id true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program
                `shouldSatisfy` either
                    (\err -> not ("ProgramCannotInferLambda" `isInfixOf` show err))
                    (const False)

        it "preserves the paper's annotated g g construction in checked IR" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : ∀(result ⩾ ∀ a. a -> a). (∀ a. a -> a) -> result ="
                        , "    λ(g : ∀ a. a -> a) g g;"
                        , "}"
                        ]
            checked <- requireChecked program
            binding <- requireCheckedBinding "Main__main" checked
            case (ProgramTypes.checkedBindingType binding, checkedBindingTerm binding) of
                ( Elab.TForallRef typeResultRef (Just typeResultBound) (Elab.TArrow parameterTy (Elab.TVarRef typeResultOccurrence))
                    , Elab.ETyAbsRef termResultRef (Just termResultBound) (Elab.ELam parameter body)
                    ) -> do
                        Elab.typeBinderRefsSameIdentity typeResultRef typeResultOccurrence `shouldBe` True
                        Elab.typeBinderRefsSameIdentity typeResultRef termResultRef `shouldBe` True
                        isPolymorphicIdentityType (Elab.tyToElab typeResultBound) `shouldBe` True
                        isPolymorphicIdentityType (Elab.tyToElab termResultBound) `shouldBe` True
                        isPolymorphicIdentityType parameterTy `shouldBe` True
                        alphaEqType (Elab.resolvedVarType parameter) parameterTy `shouldBe` True
                        case body of
                            Elab.ETyInst
                                ( Elab.EApp
                                    (Elab.ETyInst (Elab.EVarNode functionOccurrence) (Elab.InstApp functionArgumentTy))
                                    (Elab.EVarNode argumentOccurrence)
                                  )
                                (Elab.InstAbstrRef abstractedResultRef) -> do
                                    isPolymorphicIdentityType functionArgumentTy `shouldBe` True
                                    Elab.typeBinderRefsSameIdentity typeResultRef abstractedResultRef `shouldBe` True
                                    Elab.resolvedVarDetails functionOccurrence `shouldBe` Elab.resolvedVarDetails parameter
                                    Elab.resolvedVarDetails argumentOccurrence `shouldBe` Elab.resolvedVarDetails parameter
                            other ->
                                expectationFailure ("expected the paper's g g computation spine, got " ++ show other)
                other ->
                    expectationFailure ("expected the paper's bounded self-application binding, got " ++ show other)
            case typeCheck (checkedBindingTerm binding) of
                Left err -> expectationFailure ("paper self-application checked IR failed to typecheck: " ++ show err)
                Right inferred ->
                    alphaEqType inferred (ProgramTypes.checkedBindingType binding) `shouldBe` True

        it "requires an explicit annotation for a polymorphic g g lambda parameter" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : ∀(result ⩾ ∀ a. a -> a). (∀ a. a -> a) -> result ="
                        , "    λg g g;"
                        , "}"
                        ]
            case checkProgram program of
                Left (ProgramPipelineError message) -> do
                    message `shouldSatisfy` isInfixOf "Phase 4 (presolution)"
                    message `shouldSatisfy` isInfixOf "OccursCheckPresolution"
                Left err ->
                    expectationFailure ("expected the presolution occurs-check rejection, got " ++ show err)
                Right _ ->
                    expectationFailure "expected the unannotated polymorphic self-application to be rejected"

        it "does not capture an unrelated ambient type variable to satisfy a forall annotation" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (bad) {"
                        , "  def bad : ∀ x. (Bool -> x -> x) -> ∀ b. Bool -> b -> b ="
                        , "    λf (f : ∀ b. Bool -> b -> b);"
                        , "}"
                        ]
            checkProgram program `shouldSatisfy` isLeft

        it "rejects the paper's looping annotated self-application without an identity exception" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : ∀ a. a -> a ="
                        , "    ((λ(g : ∀ a. a -> a) g g) : ∀ a. a -> a)"
                        , "      (λ(g : ∀ a. a -> a) g g);"
                        , "}"
                        ]
            case checkProgram program of
                Left ProgramTypeMismatch{} -> pure ()
                Left err -> expectationFailure ("expected a typed mismatch rejection, got " ++ show err)
                Right _ -> expectationFailure "expected looping annotated self-application rejection"

        it "preserves a vacuous leading method forall as a positional producer and consumer slot" $ do
            program <-
                requireParsed $
                    unlines
                        [ "module Main export (Pick, pick, main) {"
                        , "  class Pick a {"
                        , "    pick : ∀ ghost. ∀ b. a -> b -> b;"
                        , "  }"
                        , ""
                        , "  instance Pick Bool {"
                        , "    pick = let impl : ∀ ghost. ∀ b. Bool -> b -> b = λflag λvalue value in impl;"
                        , "  }"
                        , ""
                        , "  def main : Bool = pick true false;"
                        , "}"
                        ]
            checked <- requireChecked program
            methodRuntimeName <-
                case
                    [ ProgramTypes.valueInfoRuntimeName valueInfo
                    | checkedModule <- checkedProgramModules checked
                    , instanceInfo <- checkedModuleInstances checkedModule
                    , ProgramTypes.instanceClassName instanceInfo == "Pick"
                    , valueInfo <- Map.elems (instanceMethodsByIdentity instanceInfo)
                    ]
                of
                    [runtimeName] -> pure runtimeName
                    runtimeNames ->
                        expectationFailure ("expected one Pick instance method binding, got " ++ show runtimeNames)
                            >> fail "instance method binding mismatch"
            methodBinding <- requireCheckedBinding methodRuntimeName checked
            mainBinding <- requireCheckedBinding "Main__main" checked

            case ProgramTypes.checkedBindingType methodBinding of
                Elab.TForallRef ghostRef Nothing (Elab.TForallRef bRef Nothing bodyTy) -> do
                    leadingForallIdentities (ProgramTypes.checkedBindingType methodBinding)
                        `shouldBe` [Elab.typeBinderRefIdentity ghostRef, Elab.typeBinderRefIdentity bRef]
                    elabTypeMentionsBinder (Elab.typeBinderRefIdentity ghostRef) bodyTy `shouldBe` False
                    elabTypeMentionsBinder (Elab.typeBinderRefIdentity bRef) bodyTy `shouldBe` True
                other ->
                    expectationFailure ("expected two ordered method foralls, got " ++ show other)

            case (ProgramTypes.checkedBindingType methodBinding, checkedBindingTerm methodBinding) of
                ( Elab.TForallRef methodGhostRef Nothing (Elab.TForallRef methodValueRef Nothing _)
                    , term@(
                        Elab.ETyAbsRef termGhostRef Nothing
                            ( Elab.ETyAbsRef termValueRef Nothing
                                (Elab.ELet impl scheme rhs implBody)
                              )
                        )
                    ) -> do
                    Elab.typeBinderRefsSameIdentity methodGhostRef termGhostRef `shouldBe` True
                    Elab.typeBinderRefsSameIdentity methodValueRef termValueRef `shouldBe` True
                    leadingTypeAbsIdentities term
                        `shouldBe`
                            [ Elab.typeBinderRefIdentity methodGhostRef
                            , Elab.typeBinderRefIdentity methodValueRef
                            ]
                    case Elab.schemeBinderRefs scheme of
                        [(localGhostRef, Nothing), (localValueRef, Nothing)] -> do
                            leadingTypeAbsIdentities rhs
                                `shouldBe` [Elab.typeBinderRefIdentity localGhostRef, Elab.typeBinderRefIdentity localValueRef]
                            let bodyTy = Elab.schemeBody scheme
                            elabTypeMentionsBinder (Elab.typeBinderRefIdentity localGhostRef) bodyTy `shouldBe` False
                            elabTypeMentionsBinder (Elab.typeBinderRefIdentity localValueRef) bodyTy `shouldBe` True
                            Elab.typeBinderRefsSameIdentity methodGhostRef localGhostRef `shouldBe` False
                            Elab.typeBinderRefsSameIdentity methodGhostRef localValueRef `shouldBe` False
                            -- The method producer owns both ordered ABI slots,
                            -- while the nested explicit let owns two distinct
                            -- lexical slots.  Its occurrence consumes the
                            -- local ghost with N, then applies the local value
                            -- binder to the outer method value slot.  The two
                            -- schemes remain alpha-equivalent without letting
                            -- a nested binder escape into the method ABI.
                            Elab.typeBinderRefsSameIdentity methodValueRef localGhostRef `shouldBe` False
                            case implBody of
                                Elab.ETyInst
                                    (Elab.EVarNode implOccurrence)
                                    (Elab.InstSeq Elab.InstElim (Elab.InstApp (Elab.TVarRef selectedRef))) -> do
                                        Elab.typeBinderRefsSameIdentity selectedRef methodValueRef `shouldBe` True
                                        Elab.resolvedVarSameIdentity impl implOccurrence `shouldBe` True
                                other ->
                                    expectationFailure ("expected an identity-bearing local-to-method forall bridge, got " ++ show other)
                        binders ->
                            expectationFailure ("expected two ordered local scheme binders, got " ++ show binders)
                    alphaEqType (Elab.resolvedVarType impl) (ProgramTypes.checkedBindingType methodBinding) `shouldBe` True
                    case typeCheck rhs of
                        Left err -> expectationFailure ("method producer rhs failed to typecheck: " ++ show err)
                        Right inferred -> alphaEqType inferred (Elab.resolvedVarType impl) `shouldBe` True
                    case typeCheck term of
                        Left err -> expectationFailure ("method producer failed to typecheck: " ++ show err)
                        Right inferred -> alphaEqType inferred (ProgramTypes.checkedBindingType methodBinding) `shouldBe` True
                other ->
                    expectationFailure ("expected a polymorphic local producer, got " ++ show other)

            case checkedBindingTerm mainBinding of
                Elab.EApp
                    ( Elab.EApp
                        ( Elab.ETyInst
                            (Elab.ETyInst (Elab.EVarNode methodOccurrence) Elab.InstElim)
                            (Elab.InstApp selectedTy)
                          )
                        (Elab.ELit (LBool True))
                      )
                    (Elab.ELit (LBool False)) -> do
                        Elab.resolvedVarSameIdentity (checkedBindingResolvedVar methodBinding) methodOccurrence `shouldBe` True
                        alphaEqType selectedTy (ProgramTypes.checkedBindingType mainBinding) `shouldBe` True
                other ->
                    expectationFailure ("expected N then Bool method instantiations, got " ++ show other)

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
                    expectationFailure "runtime-success rows are covered by the merged interpreter/LLVM/native parity lane"
                ExpectCheckSuccess ->
                    checkProgram program `shouldSatisfy` isRight
                ExpectCheckFailureContaining expectedFragment ->
                    checkProgram program
                        `shouldSatisfy` either
                            (isInfixOf expectedFragment . show)
                            (const False)

    loadProgramMatrixSource source =
        case source of
            InlineProgram programText -> requireParsed programText
            ProgramFile path -> requireParsed =<< readFile path

    requireChecked program =
        case checkProgram program of
            Left err -> expectationFailure ("check failed: " ++ show err) >> fail "check failed"
            Right checked -> pure checked

    requireCheckedWithPrelude program =
        case checkProgramWithPrelude program of
            Left err -> expectationFailure ("check failed: " ++ show err) >> fail "check failed"
            Right checked -> pure checked

    requireCheckedLocated located =
        case checkLocatedProgram located of
            Left err -> expectationFailure ("check failed: " ++ show err) >> fail "check failed"
            Right checked -> pure checked

    requireCheckedLocatedWithPrelude located =
        case checkLocatedProgramWithPrelude located of
            Left err -> expectationFailure ("check failed: " ++ show err) >> fail "check failed"
            Right checked -> pure checked

    checkProgramWithPrelude =
        checkProgramPackage . withPreludePackage . trivialProgramPackage

    checkLocatedProgramWithPrelude =
        checkLocatedProgramPackage
            . withPreludeLocatedPackage
            . trivialLocatedProgramPackage

    runLocatedProgramWithPrelude =
        runLocatedProgramPackage
            . withPreludeLocatedPackage
            . trivialLocatedProgramPackage

    runLocatedProgramOutputWithPrelude =
        runLocatedProgramPackageOutput
            . withPreludeLocatedPackage
            . trivialLocatedProgramPackage

    requireCheckedBinding name checked =
        case [ binding
             | checkedModule <- checkedProgramModules checked
             , binding <- checkedModuleBindings checkedModule
             , checkedBindingName binding == name
             ] of
            binding : _ -> pure binding
            [] -> expectationFailure ("missing checked binding: " ++ name) >> fail "missing checked binding"

    requireCheckedData moduleName name checked =
        case [ dataInfo
             | checkedModule <- checkedProgramModules checked
             , checkedModuleName checkedModule == moduleName
             , dataInfo <- Map.elems (checkedModuleData checkedModule)
             , dataName dataInfo == name
             ] of
            dataInfo : _ -> pure dataInfo
            [] -> expectationFailure ("missing checked data: " ++ moduleName ++ "." ++ name) >> fail "missing checked data"

    requireDataConstructor name dataInfo =
        case [ctorInfo | ctorInfo <- dataConstructors dataInfo, ctorName ctorInfo == name] of
            ctorInfo : _ -> pure ctorInfo
            [] -> expectationFailure ("missing constructor: " ++ name) >> fail "missing constructor"

    requireLowerConstructorBinding scope ctorInfo =
        case
            lowerConstructorBinding
                (identityGeneratorAfter (ProgramTypes.constructorInfoGeneratedIdentities ctorInfo))
                scope
                ctorInfo
          of
            Left err -> expectationFailure ("constructor lowering failed: " ++ show err) >> fail "constructor lowering failed"
            Right (lowered, _) -> pure lowered

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
        { checkedProgramModulesInternal =
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
            binding{checkedBindingTerm = term}
        | otherwise = binding

poisonCheckedBindingSourceTypeHeads :: String -> Surface.SrcType -> CheckedProgram -> CheckedProgram
poisonCheckedBindingSourceTypeHeads name replacement checked =
    checked
        { checkedProgramModulesInternal =
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
                    ( setTypeViewDisplay
                        ( replaceHeadNames
                            (ProgramTypes.typeViewDisplay (ProgramTypes.checkedBindingSourceTypeView binding))
                        )
                        (ProgramTypes.checkedBindingSourceTypeView binding)
                    )
                }
        | otherwise = binding

    replaceHeadNames ty =
        case ty of
            STVar{} -> ty
            STArrow dom cod -> STArrow (replaceHeadNames dom) (replaceHeadNames cod)
            STBase{} -> STBase replacementName
            STCon _ args -> STCon replacementName (fmap replaceHeadNames args)
            STVarApp headName args -> STVarApp headName (fmap replaceHeadNames args)
            STTyLam binder body -> STTyLam binder (replaceHeadNames body)
            STTyApp fun arg -> STTyApp (replaceHeadNames fun) (replaceHeadNames arg)
            STForall binder mbBound body ->
                STForall binder (fmap (SrcBound . replaceHeadNames . unSrcBound) mbBound) (replaceHeadNames body)
            STMu binder body -> STMu binder (replaceHeadNames body)
            STBottom -> STBottom

    replacementName =
        case replacement of
            STBase headName -> headName
            STCon headName _ -> headName
            _ -> "$poisoned"

replaceCheckedBindingSourceType :: String -> Surface.SrcType -> CheckedProgram -> CheckedProgram
replaceCheckedBindingSourceType name sourceType checked =
    checked
        { checkedProgramModulesInternal =
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
                    (setTypeViewDisplay sourceType (ProgramTypes.checkedBindingSourceTypeView binding))
                }
        | otherwise = binding

replaceCheckedBindingType :: String -> Elab.ElabType -> CheckedProgram -> CheckedProgram
replaceCheckedBindingType name bindingType checked =
    checked
        { checkedProgramModulesInternal =
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
            binding{ProgramTypes.checkedBindingType = bindingType}
        | otherwise = binding

replaceCheckedConstructorTypeView :: String -> ProgramTypes.TypeView -> CheckedProgram -> CheckedProgram
replaceCheckedConstructorTypeView runtimeName view checked =
    checked
        { checkedProgramModulesInternal =
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
            ctorInfo{ProgramTypes.ctorTypeView = view}
        | otherwise = ctorInfo

replaceCheckedBindingSourceTypeWithHeadIdentities :: String -> Surface.SrcType -> Map.Map String SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedBindingSourceTypeWithHeadIdentities name sourceType headIdentities checked =
    checked
        { checkedProgramModulesInternal =
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
                    (setTypeViewHeadIdentities headIdentities (mkTypeView sourceType sourceType))
                }
        | otherwise = binding

replaceCheckedBindingSourceTypeView :: String -> ProgramTypes.TypeView -> CheckedProgram -> CheckedProgram
replaceCheckedBindingSourceTypeView name sourceTypeView checked =
    checked
        { checkedProgramModulesInternal =
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

-- A Figure 15.3.5 application must specialize a polymorphic function at the
-- endpoint constructed for its argument.  The endpoint can be either a
-- Gamma-bound graph ref or an already closed structural type; requiring the
-- former would mistake one construction shape for the semantic invariant.
-- Retain the source forall ref so callers can prove that it did not leak into
-- the selected endpoint, and typecheck the argument in its exact lexical env.
polymorphicApplicationEndpointEvidence
    :: ElabPipeline.Env
    -> Elab.XmlfTerm
    -> [ ( Elab.TypeBinderRef
         , Elab.ElabType
         , Either ElabPipeline.TypeCheckError Elab.ElabType
         )
       ]
polymorphicApplicationEndpointEvidence = go
  where
    go env term =
        case term of
            Elab.EVarNode{} -> []
            Elab.ELit{} -> []
            Elab.ELam parameter body ->
                go
                    ( ElabPipeline.insertResolvedTermBinding
                        parameter
                        (Elab.resolvedVarType parameter)
                        env
                    )
                    body
            Elab.EApp fun arg ->
                endpointAt env fun arg
                    ++ go env fun
                    ++ go env arg
            Elab.ELet binder scheme rhs body ->
                let bodyEnv =
                        ElabPipeline.insertResolvedTermBinding
                            binder
                            (ElabPipeline.schemeToType scheme)
                            env
                in go bodyEnv rhs ++ go bodyEnv body
            Elab.ETyAbsRef ref mbBound body ->
                go
                    ( ElabPipeline.insertTypeBindingRef
                        ref
                        (maybe Elab.TBottom Elab.tyToElab mbBound)
                        env
                    )
                    body
            Elab.ETyInst body _ -> go env body
            Elab.ERoll _ body -> go env body
            Elab.EUnroll body -> go env body

    endpointAt env fun arg =
        case fun of
            Elab.ETyInst
                (Elab.EVarNode ResolvedVar{resolvedVarType = sourceTy})
                (Elab.InstApp endpointTy) ->
                case sourceTy of
                    Elab.TForallRef sourceRef _ (Elab.TArrow (Elab.TVarRef domainRef) _)
                        | Elab.typeBinderRefsSameIdentity sourceRef domainRef ->
                            [ ( sourceRef
                              , endpointTy
                              , ElabPipeline.typeCheckWithEnv env arg
                              )
                            ]
                    _ -> []
            _ -> []

poisonTopLevelTermIdentity :: SymbolIdentity -> SymbolIdentity -> String -> Elab.XmlfTerm -> Elab.XmlfTerm
poisonTopLevelTermIdentity target replacement _replacementName term =
    case term of
        Elab.EVarNode resolved@ResolvedVar{resolvedVarDetails = TopLevelId identity}
            | Symbol.sameSymbolIdentity identity target ->
                Elab.EVarNode
                    resolved
                        { resolvedVarDetails = TopLevelId replacement
                        }
        Elab.EVarNode{} -> term
        Elab.ELit{} -> term
        Elab.ELam resolved body -> Elab.ELam resolved (go body)
        Elab.EApp fun arg -> Elab.EApp (go fun) (go arg)
        Elab.ELet resolved scheme rhs body -> Elab.ELet resolved scheme (go rhs) (go body)
        Elab.ETyAbsRef ref mbBound body -> Elab.ETyAbsRef ref mbBound (go body)
        Elab.ETyInst body inst -> Elab.ETyInst (go body) inst
        Elab.ERoll ty body -> Elab.ERoll ty (go body)
        Elab.EUnroll body -> Elab.EUnroll (go body)
  where
    go = poisonTopLevelTermIdentity target replacement _replacementName

poisonConstructorTermIdentity :: SymbolIdentity -> SymbolIdentity -> String -> Elab.XmlfTerm -> Elab.XmlfTerm
poisonConstructorTermIdentity target replacement _replacementName term =
    case term of
        Elab.EVarNode resolved@ResolvedVar{resolvedVarDetails = ConstructorId ref}
            | Symbol.sameSymbolIdentity (constructorRefSymbol ref) target ->
                Elab.EVarNode
                    resolved
                        { resolvedVarDetails = ConstructorId (ProgramTypes.constructorRefFromSymbol replacement)
                        }
        Elab.EVarNode{} -> term
        Elab.ELit{} -> term
        Elab.ELam resolved body -> Elab.ELam resolved (go body)
        Elab.EApp fun arg -> Elab.EApp (go fun) (go arg)
        Elab.ELet resolved scheme rhs body -> Elab.ELet resolved scheme (go rhs) (go body)
        Elab.ETyAbsRef ref mbBound body -> Elab.ETyAbsRef ref mbBound (go body)
        Elab.ETyInst body inst -> Elab.ETyInst (go body) inst
        Elab.ERoll ty body -> Elab.ERoll ty (go body)
        Elab.EUnroll body -> Elab.EUnroll (go body)
  where
    go = poisonConstructorTermIdentity target replacement _replacementName

primitiveTerm :: String -> Elab.XmlfTerm
primitiveTerm name =
    Elab.EVarNode
        ResolvedVar
            { resolvedVarType = Elab.TBottom
            , resolvedVarDetails =
                PrimitiveId (primitiveRefFromSymbol (Builtins.builtinValueIdentity name))
            }

renameCheckedBindingName :: String -> String -> CheckedProgram -> CheckedProgram
renameCheckedBindingName _oldName _newName checked =
    checked

requireTopLevelIdentity :: CheckedBinding -> IO SymbolIdentity
requireTopLevelIdentity binding =
    case resolvedVarDetails (checkedBindingResolvedVar binding) of
        TopLevelId identity -> pure identity
        other -> expectationFailure ("expected top-level binding identity, got " ++ show other) >> fail "missing top-level identity"

replaceCheckedBindingTopLevelIdentity :: String -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedBindingTopLevelIdentity name replacement checked =
    replaceCheckedBindingDetails name (TopLevelId replacement) checked

replaceCheckedBindingDetails :: String -> IdDetails -> CheckedProgram -> CheckedProgram
replaceCheckedBindingDetails name replacement checked =
    checked
        { checkedProgramModulesInternal =
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
                        { resolvedVarDetails = replacement
                        }
                }
        | otherwise = binding

replaceCheckedModuleIdentity :: String -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedModuleIdentity moduleName replacement checked =
    checked
        { checkedProgramModulesInternal =
            map replaceModule (checkedProgramModules checked)
        }
  where
    replaceModule checkedModule
        | checkedModuleName checkedModule == moduleName =
            checkedModule{checkedModuleIdentity = replacement}
        | otherwise = checkedModule

replaceCheckedDataSymbol :: SymbolIdentity -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedDataSymbol target replacement checked =
    checked
        { checkedProgramModulesInternal =
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
            dataInfo{dataInfoSymbol = replacement}
        | otherwise = dataInfo

replaceCheckedConstructorSymbol :: SymbolIdentity -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedConstructorSymbol target replacement checked =
    checked
        { checkedProgramModulesInternal =
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
            ctorInfo{ctorInfoSymbol = replacement}
        | otherwise = ctorInfo

replaceCheckedConstructorIndex :: SymbolIdentity -> Int -> CheckedProgram -> CheckedProgram
replaceCheckedConstructorIndex target replacement checked =
    checked
        { checkedProgramModulesInternal =
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
            ctorInfo{ctorIndex = replacement}
        | otherwise = ctorInfo

replaceCheckedConstructorOwner :: SymbolIdentity -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedConstructorOwner target replacement checked =
    checked
        { checkedProgramModulesInternal =
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
        | Symbol.sameSymbolIdentity (ctorInfoSymbol ctorInfo) target =
            ctorInfo{ctorOwningTypeIdentity = replacement}
        | otherwise = ctorInfo

renameCheckedModuleName :: String -> String -> CheckedProgram -> CheckedProgram
renameCheckedModuleName oldName newName checked =
    checked
        { checkedProgramModulesInternal =
            map renameModule (checkedProgramModules checked)
        }
  where
    renameModule checkedModule
        | checkedModuleName checkedModule == oldName =
            checkedModule{checkedModuleName = newName}
        | otherwise = checkedModule

renameCheckedConstructorIdentityNamesWhere :: (String -> Bool) -> String -> CheckedProgram -> CheckedProgram
renameCheckedConstructorIdentityNamesWhere predicate replacement checked =
    checked
        { checkedProgramModulesInternal =
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
        { checkedProgramModulesInternal =
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
        { checkedProgramModulesInternal =
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
        { checkedProgramModulesInternal =
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

    renameConstructor ctorInfo@ConstructorInfo{ctorRuntimeName = runtimeName}
        | predicate runtimeName =
            ctorInfo{ctorRuntimeName = replacement}
    renameConstructor ctorInfo = ctorInfo

renameInstanceMethodRuntimeNamesWhere :: (String -> Bool) -> String -> CheckedProgram -> CheckedProgram
renameInstanceMethodRuntimeNamesWhere predicate replacement checked =
    checked
        { checkedProgramModulesInternal =
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

    replaceValueInfo valueInfo@OrdinaryValue{valueRuntimeName = runtimeName}
        | predicate runtimeName =
            valueInfo{valueRuntimeName = replacement}
    replaceValueInfo valueInfo = valueInfo

resolvedConstructorRefs :: Elab.XmlfTerm -> [ProgramTypes.ConstructorRef]
resolvedConstructorRefs term =
    case term of
        Elab.EVarNode ResolvedVar{resolvedVarDetails = ConstructorId ctorRef} ->
            [ctorRef]
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.ELam _ body -> resolvedConstructorRefs body
        Elab.EApp fun arg -> resolvedConstructorRefs fun ++ resolvedConstructorRefs arg
        Elab.ELet _ _ rhs body -> resolvedConstructorRefs rhs ++ resolvedConstructorRefs body
        Elab.ETyAbsRef _ _ body -> resolvedConstructorRefs body
        Elab.ETyInst body _ -> resolvedConstructorRefs body
        Elab.ERoll _ body -> resolvedConstructorRefs body
        Elab.EUnroll body -> resolvedConstructorRefs body

resolvedConstructorVars :: Elab.XmlfTerm -> [Elab.ResolvedVar]
resolvedConstructorVars term =
    case term of
        Elab.EVarNode resolved@ResolvedVar{resolvedVarDetails = ConstructorId _} ->
            [resolved]
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.ELam _ body -> resolvedConstructorVars body
        Elab.EApp fun arg -> resolvedConstructorVars fun ++ resolvedConstructorVars arg
        Elab.ELet _ _ rhs body -> resolvedConstructorVars rhs ++ resolvedConstructorVars body
        Elab.ETyAbsRef _ _ body -> resolvedConstructorVars body
        Elab.ETyInst body _ -> resolvedConstructorVars body
        Elab.ERoll _ body -> resolvedConstructorVars body
        Elab.EUnroll body -> resolvedConstructorVars body

resolvedTopLevelIdentities :: Elab.XmlfTerm -> [SymbolIdentity]
resolvedTopLevelIdentities term =
    case term of
        Elab.EVarNode ResolvedVar{resolvedVarDetails = TopLevelId identity} ->
            [identity]
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.ELam _ body -> resolvedTopLevelIdentities body
        Elab.EApp fun arg -> resolvedTopLevelIdentities fun ++ resolvedTopLevelIdentities arg
        Elab.ELet _ _ rhs body -> resolvedTopLevelIdentities rhs ++ resolvedTopLevelIdentities body
        Elab.ETyAbsRef _ _ body -> resolvedTopLevelIdentities body
        Elab.ETyInst body _ -> resolvedTopLevelIdentities body
        Elab.ERoll _ body -> resolvedTopLevelIdentities body
        Elab.EUnroll body -> resolvedTopLevelIdentities body

resolvedLocalBinders :: Elab.XmlfTerm -> [LocalRef]
resolvedLocalBinders term =
    case term of
        Elab.ELam ResolvedVar{resolvedVarDetails = LocalId localRef} body ->
            localRef : resolvedLocalBinders body
        Elab.ELam ResolvedVar{resolvedVarDetails = EvidenceId localRef} body ->
            localRef : resolvedLocalBinders body
        Elab.ELet ResolvedVar{resolvedVarDetails = LocalId localRef} _ rhs body ->
            localRef : resolvedLocalBinders rhs ++ resolvedLocalBinders body
        Elab.ELet ResolvedVar{resolvedVarDetails = EvidenceId localRef} _ rhs body ->
            localRef : resolvedLocalBinders rhs ++ resolvedLocalBinders body
        Elab.ELam _ body -> resolvedLocalBinders body
        Elab.ELet _ _ rhs body -> resolvedLocalBinders rhs ++ resolvedLocalBinders body
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.EApp fun arg -> resolvedLocalBinders fun ++ resolvedLocalBinders arg
        Elab.ETyAbsRef _ _ body -> resolvedLocalBinders body
        Elab.ETyInst body _ -> resolvedLocalBinders body
        Elab.ERoll _ body -> resolvedLocalBinders body
        Elab.EUnroll body -> resolvedLocalBinders body

resolvedEvidenceBinders :: Elab.XmlfTerm -> [LocalRef]
resolvedEvidenceBinders term =
    case term of
        Elab.ELam ResolvedVar{resolvedVarDetails = EvidenceId localRef} body ->
            localRef : resolvedEvidenceBinders body
        Elab.ELet ResolvedVar{resolvedVarDetails = EvidenceId localRef} _ rhs body ->
            localRef : resolvedEvidenceBinders rhs ++ resolvedEvidenceBinders body
        Elab.ELam _ body -> resolvedEvidenceBinders body
        Elab.ELet _ _ rhs body -> resolvedEvidenceBinders rhs ++ resolvedEvidenceBinders body
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.EApp fun arg -> resolvedEvidenceBinders fun ++ resolvedEvidenceBinders arg
        Elab.ETyAbsRef _ _ body -> resolvedEvidenceBinders body
        Elab.ETyInst body _ -> resolvedEvidenceBinders body
        Elab.ERoll _ body -> resolvedEvidenceBinders body
        Elab.EUnroll body -> resolvedEvidenceBinders body

reduceLeadingTypeInstantiationRedexesFully :: Elab.XmlfTerm -> Elab.XmlfTerm
reduceLeadingTypeInstantiationRedexesFully term =
    case ElabPipeline.reduceLeadingTypeInstantiationRedexes term of
        Just reduced -> reduceLeadingTypeInstantiationRedexesFully reduced
        Nothing -> term

renameEvidenceBinderRef :: LocalRef -> LocalRef -> Elab.XmlfTerm -> Elab.XmlfTerm
renameEvidenceBinderRef sourceRef targetRef = go
  where
    go term =
        case term of
            Elab.EVarNode{} -> term
            Elab.ELit{} -> term
            Elab.ELam resolved body ->
                Elab.ELam (renameBinder resolved) (go body)
            Elab.EApp fun arg -> Elab.EApp (go fun) (go arg)
            Elab.ELet resolved scheme rhs body ->
                Elab.ELet (renameBinder resolved) scheme (go rhs) (go body)
            Elab.ETyAbsRef ref mbBound body ->
                Elab.ETyAbsRef ref mbBound (go body)
            Elab.ETyInst body inst -> Elab.ETyInst (go body) inst
            Elab.ERoll ty body -> Elab.ERoll ty (go body)
            Elab.EUnroll body -> Elab.EUnroll (go body)

    renameBinder resolved@ResolvedVar{resolvedVarDetails = EvidenceId binderRef}
        | localRefIdentity binderRef == localRefIdentity sourceRef =
            resolved{resolvedVarDetails = EvidenceId targetRef}
    renameBinder resolved = resolved

resolvedEvidenceInstApps :: Elab.XmlfTerm -> [(LocalRef, Elab.ElabType)]
resolvedEvidenceInstApps term =
    case term of
        Elab.EVarNode {} -> []
        Elab.ELam _ body -> resolvedEvidenceInstApps body
        Elab.ELet _ _ rhs body -> resolvedEvidenceInstApps rhs ++ resolvedEvidenceInstApps body
        Elab.EApp fun arg -> resolvedEvidenceInstApps fun ++ resolvedEvidenceInstApps arg
        Elab.ETyAbsRef _ _ body -> resolvedEvidenceInstApps body
        Elab.ETyInst body inst ->
            case (body, inst) of
                (Elab.EVarNode occurrence, Elab.InstApp argumentTy) ->
                    case Elab.resolvedVarDetails occurrence of
                        EvidenceId occurrenceRef ->
                            (occurrenceRef, argumentTy) : resolvedEvidenceInstApps body
                        _ -> resolvedEvidenceInstApps body
                _ -> resolvedEvidenceInstApps body
        Elab.ELit {} -> []
        Elab.ERoll _ body -> resolvedEvidenceInstApps body
        Elab.EUnroll body -> resolvedEvidenceInstApps body

resolvedEvidenceOccurrences :: Elab.XmlfTerm -> [LocalRef]
resolvedEvidenceOccurrences term =
    case term of
        Elab.EVarNode ResolvedVar{resolvedVarDetails = EvidenceId localRef} ->
            [localRef]
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.ELam _ body -> resolvedEvidenceOccurrences body
        Elab.EApp fun arg -> resolvedEvidenceOccurrences fun ++ resolvedEvidenceOccurrences arg
        Elab.ELet _ _ rhs body -> resolvedEvidenceOccurrences rhs ++ resolvedEvidenceOccurrences body
        Elab.ETyAbsRef _ _ body -> resolvedEvidenceOccurrences body
        Elab.ETyInst body _ -> resolvedEvidenceOccurrences body
        Elab.ERoll _ body -> resolvedEvidenceOccurrences body
        Elab.EUnroll body -> resolvedEvidenceOccurrences body

isGeneratedLocalRef :: LocalRef -> Bool
isGeneratedLocalRef localRef =
    case localRefIdentity localRef of
        GraphLocalId{} -> False
        ScopedGraphLocalId{} -> False
        GeneratedGraphLocalId{} -> True
        GeneratedLocalId{} -> True

generatedLocalIdentityValues :: Elab.XmlfTerm -> [UniqueIdentity]
generatedLocalIdentityValues term =
    [ identity
    | localRef <- resolvedLocalBinders term
    , identity <-
        case localRefIdentity localRef of
            GeneratedGraphLocalId generated _ -> [generated]
            GeneratedLocalId generated -> [generated]
            _ -> []
    ]

resolvedLocalLetTypes :: Elab.XmlfTerm -> [Elab.ElabType]
resolvedLocalLetTypes term =
    case term of
        Elab.ELet ResolvedVar{resolvedVarDetails = LocalId{}, resolvedVarType = ty} _ rhs body ->
            ty : resolvedLocalLetTypes rhs ++ resolvedLocalLetTypes body
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.ELam _ body -> resolvedLocalLetTypes body
        Elab.EApp fun arg -> resolvedLocalLetTypes fun ++ resolvedLocalLetTypes arg
        Elab.ELet _ _ rhs body -> resolvedLocalLetTypes rhs ++ resolvedLocalLetTypes body
        Elab.ETyAbsRef _ _ body -> resolvedLocalLetTypes body
        Elab.ETyInst body _ -> resolvedLocalLetTypes body
        Elab.ERoll _ body -> resolvedLocalLetTypes body
        Elab.EUnroll body -> resolvedLocalLetTypes body

localSchemeBinderIdentities :: Elab.XmlfTerm -> [TypeBinderIdentity]
localSchemeBinderIdentities term =
    case term of
        Elab.ELet _ scheme rhs body ->
            map (Elab.typeBinderRefIdentity . fst) (Elab.schemeBinderRefs scheme)
                ++ localSchemeBinderIdentities rhs
                ++ localSchemeBinderIdentities body
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.ELam _ body -> localSchemeBinderIdentities body
        Elab.EApp fun arg -> localSchemeBinderIdentities fun ++ localSchemeBinderIdentities arg
        Elab.ETyAbsRef _ _ body -> localSchemeBinderIdentities body
        Elab.ETyInst body _ -> localSchemeBinderIdentities body
        Elab.ERoll _ body -> localSchemeBinderIdentities body
        Elab.EUnroll body -> localSchemeBinderIdentities body

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

leadingForallIdentities :: Elab.ElabType -> [TypeBinderIdentity]
leadingForallIdentities ty =
    case ty of
        Elab.TForallRef ref _ body ->
            Elab.typeBinderRefIdentity ref : leadingForallIdentities body
        _ ->
            []

elabTypeMentionsBinder :: TypeBinderIdentity -> Elab.Ty v -> Bool
elabTypeMentionsBinder identity ty =
    case ty of
        Elab.TVarRef ref ->
            Elab.typeBinderRefIdentity ref == identity
        Elab.TArrow dom cod ->
            elabTypeMentionsBinder identity dom || elabTypeMentionsBinder identity cod
        Elab.TBaseWithIdentity{} ->
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

replaceElabTypeBinderRef :: Elab.TypeBinderRef -> Elab.TypeBinderRef -> Elab.Ty v -> Elab.Ty v
replaceElabTypeBinderRef oldRef newRef ty =
    case ty of
        Elab.TVarRef ref ->
            Elab.TVarRef (replace ref)
        Elab.TArrow dom cod ->
            Elab.TArrow (replaceElabTypeBinderRef oldRef newRef dom) (replaceElabTypeBinderRef oldRef newRef cod)
        Elab.TBaseWithIdentity identity base ->
            Elab.TBaseWithIdentity identity base
        Elab.TConWithIdentity identity base args ->
            Elab.TConWithIdentity identity base (fmap (replaceElabTypeBinderRef oldRef newRef) args)
        Elab.TVarAppRef ref args ->
            Elab.TVarAppRef (replace ref) (fmap (replaceElabTypeBinderRef oldRef newRef) args)
        Elab.TForallRef ref mbBound body ->
            Elab.TForallRef
                (replace ref)
                (fmap (replaceElabTypeBinderRef oldRef newRef) mbBound)
                (replaceElabTypeBinderRef oldRef newRef body)
        Elab.TMuRef ref body ->
            Elab.TMuRef (replace ref) (replaceElabTypeBinderRef oldRef newRef body)
        Elab.TBottom ->
            Elab.TBottom
  where
    replace ref
        | Elab.typeBinderRefsSameIdentity ref oldRef = newRef
        | otherwise = ref

replaceElabTypeHeadIdentity :: SymbolIdentity -> SymbolIdentity -> Elab.Ty v -> Elab.Ty v
replaceElabTypeHeadIdentity oldIdentity newIdentity ty =
    case ty of
        Elab.TVarRef ref ->
            Elab.TVarRef ref
        Elab.TArrow dom cod ->
            Elab.TArrow (replaceElabTypeHeadIdentity oldIdentity newIdentity dom) (replaceElabTypeHeadIdentity oldIdentity newIdentity cod)
        Elab.TBaseWithIdentity identity base ->
            Elab.TBaseWithIdentity (replace identity) base
        Elab.TConWithIdentity identity base args ->
            Elab.TConWithIdentity (replace identity) base (fmap (replaceElabTypeHeadIdentity oldIdentity newIdentity) args)
        Elab.TVarAppRef ref args ->
            Elab.TVarAppRef ref (fmap (replaceElabTypeHeadIdentity oldIdentity newIdentity) args)
        Elab.TForallRef ref mbBound body ->
            Elab.TForallRef
                ref
                (fmap (replaceElabTypeHeadIdentity oldIdentity newIdentity) mbBound)
                (replaceElabTypeHeadIdentity oldIdentity newIdentity body)
        Elab.TMuRef ref body ->
            Elab.TMuRef ref (replaceElabTypeHeadIdentity oldIdentity newIdentity body)
        Elab.TBottom ->
            Elab.TBottom
  where
    replace identity
        | identity == oldIdentity = newIdentity
        | otherwise = identity

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
            STBase{} -> (remaining, current)
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
        Elab.EVarNode ResolvedVar{resolvedVarDetails = LocalId localRef} ->
            [localRef]
        Elab.EVarNode ResolvedVar{resolvedVarDetails = EvidenceId localRef} ->
            [localRef]
        Elab.EVarNode{} -> []
        Elab.ELit{} -> []
        Elab.ELam _ body -> resolvedLocalOccurrences body
        Elab.EApp fun arg -> resolvedLocalOccurrences fun ++ resolvedLocalOccurrences arg
        Elab.ELet _ _ rhs body -> resolvedLocalOccurrences rhs ++ resolvedLocalOccurrences body
        Elab.ETyAbsRef _ _ body -> resolvedLocalOccurrences body
        Elab.ETyInst body _ -> resolvedLocalOccurrences body
        Elab.ERoll _ body -> resolvedLocalOccurrences body
        Elab.EUnroll body -> resolvedLocalOccurrences body

applicationArgumentsForRuntimeName :: String -> Elab.XmlfTerm -> [Elab.XmlfTerm]
applicationArgumentsForRuntimeName runtimeName = go
  where
    go term =
        case term of
            Elab.EVarNode{} -> []
            Elab.ELit{} -> []
            Elab.ELam _ body -> go body
            Elab.EApp fun arg ->
                [arg | headRuntimeName fun == Just runtimeName]
                    ++ go fun
                    ++ go arg
            Elab.ELet _ _ rhs body -> go rhs ++ go body
            Elab.ETyAbsRef _ _ body -> go body
            Elab.ETyInst body _ -> go body
            Elab.ERoll _ body -> go body
            Elab.EUnroll body -> go body

    headRuntimeName term =
        case term of
            Elab.EVarNode resolved -> Just (Elab.resolvedVarRuntimeName resolved)
            Elab.ETyInst inner _ -> headRuntimeName inner
            _ -> Nothing

xmlfSubterms :: Elab.XmlfTerm -> [Elab.XmlfTerm]
xmlfSubterms term =
    term :
        case term of
            Elab.EVarNode{} -> []
            Elab.ELit{} -> []
            Elab.ELam _ body -> xmlfSubterms body
            Elab.EApp fun arg -> xmlfSubterms fun ++ xmlfSubterms arg
            Elab.ELet _ _ rhs body -> xmlfSubterms rhs ++ xmlfSubterms body
            Elab.ETyAbsRef _ _ body -> xmlfSubterms body
            Elab.ETyInst body _ -> xmlfSubterms body
            Elab.ERoll _ body -> xmlfSubterms body
            Elab.EUnroll body -> xmlfSubterms body

applicationArgumentsForResolvedVar :: ResolvedVar -> Elab.XmlfTerm -> [Elab.XmlfTerm]
applicationArgumentsForResolvedVar expected term =
    [ argument
    | Elab.EApp fun argument <- xmlfSubterms term
    , Just actual <- [termHeadResolvedVar fun]
    , Elab.resolvedVarSameIdentity expected actual
    ]

applicationArgumentsForConstructor :: ConstructorInfo -> Elab.XmlfTerm -> [Elab.XmlfTerm]
applicationArgumentsForConstructor constructor term =
    [ argument
    | Elab.EApp fun argument <- xmlfSubterms term
    , termHeadConstructorRef fun
        == Just (ProgramTypes.constructorRefFromInfo constructor)
    ]

termHeadResolvedVar :: Elab.XmlfTerm -> Maybe ResolvedVar
termHeadResolvedVar term =
    case term of
        Elab.EVarNode resolved -> Just resolved
        Elab.ETyAbsRef _ _ body -> termHeadResolvedVar body
        Elab.ETyInst body _ -> termHeadResolvedVar body
        _ -> Nothing

termHeadConstructorRef :: Elab.XmlfTerm -> Maybe ProgramTypes.ConstructorRef
termHeadConstructorRef term =
    Elab.resolvedVarConstructorRef =<< termHeadResolvedVar term

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
        Elab.ELit{} -> []
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

reconstructCheckedProgram :: CheckedProgram -> Either ProgramError CheckedProgram
reconstructCheckedProgram checked =
    Right
        ( Checked.mkCheckedProgram
            (Checked.checkedProgramResolved checked)
            (Checked.checkedProgramModules checked)
            (Checked.checkedProgramMainResolvedVar checked)
        )

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
