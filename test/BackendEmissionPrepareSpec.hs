{-# LANGUAGE LambdaCase #-}

module BackendEmissionPrepareSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import ElabTermTestSupport (generatedResolvedLocalForName)
import LLVMToolSupport (validateLLVMAssembly)
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Backend.Emission.Prepare
    ( prepareCheckedProgramForBackendEmission
    , prepareBackendEmissionFromLocatedPackage
    , prepareBackendEmissionFromSource
    )
import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , renderProgramParseError
    )
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage (..)
    , PackageId (..)
    , locatedProgramSourceUnitFromLocated
    )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackage)
import MLF.Backend.LLVM (renderCheckedProgramLLVM)
import qualified MLF.Types.Elab as Elab
import qualified MLF.Frontend.Syntax as Surface
import MLF.Frontend.Program.Types
    ( CheckedBinding (..)
    , CheckedModule (..)
    , CheckedProgram (..)
    , ConstructorInfo (..)
    , DataInfo (..)
    , IdDetails (..)
    , ResolvedLocalSymbols (..)
    , ResolvedModule (..)
    , ResolvedModuleDiagnosticAdapter (..)
    , ResolvedProgram (..)
    , ResolvedReference
    , ResolvedReferenceKind (..)
    , ResolvedScope (..)
    , ResolvedSemanticModule (..)
    , ResolvedVar (..)
    , SymbolIdentity
    , symbolDefiningModule
    , symbolDefiningName
    , symbolIdentityFromParts
    , symbolNamespace
    , SymbolNamespace (..)
    , SymbolOwnerIdentity (..)
    , SymbolOrigin (..)
    , TypeView (..)
    , checkedBindingName
    , ctorName
    , mkTypeView
    , mkResolvedSymbol
    , mkResolvedReference
    , moduleExportsFromMaps
    )
import MLF.Frontend.Symbol (symbolIdentityStableName)
import qualified MLF.Frontend.Syntax.Program as ProgramSyntax
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (UniqueIdentity (..))

spec :: Spec
spec =
    describe "BackendEmissionPrepareSpec" $ do
        it "prepares and renders backend LLVM from a source string without file IO" $ do
            checked <- requireRight (prepareBackendEmissionFromSource "inline-main.mlfp" simpleProgram)
            output <- requireRight (renderCheckedProgramLLVM checked)

            output `shouldSatisfy` isInfixOf "; mlf2 LLVM backend v0"
            output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
            validateLLVMAssembly output

        it "prepares and renders backend LLVM from a located package" $ do
            lib <- requireLocated "src/Lib.mlfp" libProgram
            main <- requireLocated "app/Main.mlfp" mainImportsLibProgram
            let package =
                    withPreludeLocatedPackage
                        LocatedProgramPackage
                            { locatedProgramPackageId = PackageId "backend-package"
                            , locatedProgramPackageSourceUnits =
                                [ locatedProgramSourceUnitFromLocated lib
                                , locatedProgramSourceUnitFromLocated main
                                ]
                            }

            checked <- requireRight (prepareBackendEmissionFromLocatedPackage package)
            output <- requireRight (renderCheckedProgramLLVM checked)

            map checkedModuleName (checkedProgramModules checked) `shouldBe` ["Prelude", "Lib", "Main"]
            output `shouldSatisfy` isInfixOf "define i64 @\"Lib__two\"()"
            output `shouldSatisfy` isInfixOf "define i64 @\"Main__main\"()"
            validateLLVMAssembly output

        it "retains referenced Prelude data and constructor bindings while pruning unreferenced Prelude bindings" $ do
            checked <- requireRight (prepareBackendEmissionFromSource "inline-unit.mlfp" unitProgram)
            preludeModule <- requirePreludeModule checked

            Set.map identityHead (Map.keysSet (checkedModuleData preludeModule))
                `shouldBe` Set.singleton (SymbolType, "Prelude", "Unit")
            map checkedBindingName (checkedModuleBindings preludeModule) `shouldBe` ["Prelude__Unit"]

        it "rejects identity-incomplete checked inputs before backend pruning" $ do
            let incomplete =
                    resolvedShadowProgram
                        { checkedProgramModules =
                            map poisonMainTypeView (checkedProgramModules resolvedShadowProgram)
                        }
                poisonMainTypeView checkedModule0 =
                    checkedModule0
                        { checkedModuleBindings =
                            map poisonMainBinding (checkedModuleBindings checkedModule0)
                        }
                poisonMainBinding binding
                    | checkedBindingName binding == "Main__main" =
                        binding
                            { checkedBindingSourceTypeView =
                                mkTypeView (Surface.STBase "Int") (Surface.STBase "Int")
                            }
                    | otherwise = binding

            prepareCheckedProgramForBackendEmission incomplete
                `shouldSatisfy` either
                    (isInfixOf "MissingTypeHeadIdentity" . show)
                    (const False)

        it "keeps resolved globals when local binders reuse their runtime spelling" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission resolvedShadowProgram)
            preludeModule <- requirePreludeModule checked

            map checkedBindingName (checkedModuleBindings preludeModule) `shouldBe` ["Prelude__keep"]

        it "retains Prelude bindings by resolved identity when binding names are stale" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission stalePreludeBindingNameProgram)
            preludeModule <- requirePreludeModule checked

            map checkedBindingName (checkedModuleBindings preludeModule) `shouldBe` ["Prelude__keep"]

        it "does not retain Prelude bindings through stale identity payloads" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission stalePreludeBindingPayloadProgram)
            preludeModule <- requirePreludeModule checked

            checkedModuleBindings preludeModule `shouldBe` []

        it "retains Prelude bindings by module identity when checked module names are stale" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission stalePreludeModuleNameProgram)
            preludeModule <- requireModuleByIdentity (moduleIdentity "Prelude") checked

            map checkedBindingName (checkedModuleBindings preludeModule) `shouldBe` ["Prelude__keep"]

        it "does not prune by an arbitrary Prelude module identity when identities conflict" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission ambiguousPreludeModuleIdentityProgram)
            let bindingsByModule =
                    Map.fromList
                        [ (checkedModuleName checkedModule0, map checkedBindingName (checkedModuleBindings checkedModule0))
                        | checkedModule0 <- checkedProgramModules checked
                        ]

            bindingsByModule
                `shouldBe` Map.fromList
                    [ ("$prelude_left", ["Prelude__keep", "Prelude__drop"])
                    , ("$prelude_right", ["OtherPrelude__drop"])
                    , ("Main", ["Main__main"])
                    ]

        it "retains Prelude data by checked type identity when data names are stale" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission stalePreludeDataNameProgram)
            preludeModule <- requirePreludeModule checked

            Set.fromList (map dataInfoSymbol (Map.elems (checkedModuleData preludeModule)))
                `shouldBe` Set.singleton (typeIdentity "Prelude" "Unit")

        it "retains Prelude data carried only by a checked source TypeView identity" $ do
            let sourceViewOnlyProgram =
                    stalePreludeDataNameProgram
                        { checkedProgramModules =
                            map stripCheckedElabType (checkedProgramModules stalePreludeDataNameProgram)
                        }
                stripCheckedElabType checkedModule0 =
                    checkedModule0
                        { checkedModuleBindings =
                            map stripBindingElabType (checkedModuleBindings checkedModule0)
                        }
                stripBindingElabType binding
                    | checkedBindingName binding == "Prelude__keep" =
                        binding {checkedBindingType = intElabType}
                    | otherwise = binding

            checked <- requireRight (prepareCheckedProgramForBackendEmission sourceViewOnlyProgram)
            preludeModule <- requirePreludeModule checked

            Set.fromList (map dataInfoSymbol (Map.elems (checkedModuleData preludeModule)))
                `shouldBe` Set.singleton (typeIdentity "Prelude" "Unit")

        it "does not retain Prelude data through stale type identity payloads" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission stalePreludeDataPayloadProgram)
            preludeModule <- requirePreludeModule checked

            checkedModuleData preludeModule `shouldBe` Map.empty

        it "retains Prelude data by constructor identity when constructor owner names are stale" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission stalePreludeConstructorOwnerProgram)
            preludeModule <- requirePreludeModule checked

            Set.fromList (map dataInfoSymbol (Map.elems (checkedModuleData preludeModule)))
                `shouldBe` Set.singleton (typeIdentity "Prelude" "Unit")

        it "retains Prelude data by resolved module identity when module names are stale" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission staleResolvedModuleNameProgram)
            preludeModule <- requirePreludeModule checked

            Set.fromList (map dataInfoSymbol (Map.elems (checkedModuleData preludeModule)))
                `shouldBe` Set.singleton (typeIdentity "Prelude" "Unit")

        it "retains Prelude data dependencies by constructor identity type when source names are stale" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission stalePreludeConstructorTypeProgram)
            preludeModule <- requirePreludeModule checked

            Set.fromList (map dataInfoSymbol (Map.elems (checkedModuleData preludeModule)))
                `shouldBe` Set.fromList [typeIdentity "Prelude" "Unit", boxTypeIdentity]

        it "does not retain arbitrary Prelude data when one constructor identity has conflicting owners" $ do
            checked <- requireRight (prepareCheckedProgramForBackendEmission conflictingPreludeConstructorOwnerProgram)
            preludeModule <- requirePreludeModule checked

            Map.keysSet (checkedModuleData preludeModule) `shouldBe` Set.empty

identityHead :: SymbolIdentity -> (SymbolNamespace, String, String)
identityHead identity =
    (symbolNamespace identity, symbolDefiningModule identity, symbolDefiningName identity)

simpleProgram :: String
simpleProgram =
    unlines
        [ "module Main export (main) {"
        , "  def main : Int = 1;"
        , "}"
        ]

unitProgram :: String
unitProgram =
    unlines
        [ "module Main export (main) {"
        , "  import Prelude exposing (Unit(..));"
        , "  def main : Unit = Unit;"
        , "}"
        ]

libProgram :: String
libProgram =
    unlines
        [ "module Lib export (two) {"
        , "  def two : Int = 2;"
        , "}"
        ]

mainImportsLibProgram :: String
mainImportsLibProgram =
    unlines
        [ "module Main export (main) {"
        , "  import Lib exposing (two);"
        , "  def main : Int = two;"
        , "}"
        ]

resolvedShadowProgram :: CheckedProgram
resolvedShadowProgram =
    CheckedProgram
        { checkedProgramModules =
            [ checkedModule
                "Prelude"
                [ testBinding "Prelude__keep" preludeKeepVar (Elab.ELit (Surface.LInt 1))
                , testBinding "Prelude__drop" preludeDropVar (Elab.ELit (Surface.LInt 0))
                ]
            , checkedModule "Main" [testBinding "Main__main" mainVar shadowedGlobalReferenceTerm]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved = ResolvedProgram []
        }
  where
    intTy = intElabType
    preludeKeepVar = topLevelVar 10 "Prelude__keep" "Prelude" "keep" intTy
    preludeDropVar = topLevelVar 11 "Prelude__drop" "Prelude" "drop" intTy
    mainVar = topLevelVar 12 "Main__main" "Main" "main" intTy
    localShadow =
        generatedResolvedLocalForName "Prelude__keep" "Prelude__keep" intTy
    shadowedGlobalReferenceTerm =
        Elab.ELam localShadow (Elab.EVarNode preludeKeepVar)

stalePreludeBindingNameProgram :: CheckedProgram
stalePreludeBindingNameProgram =
    CheckedProgram
        { checkedProgramModules =
            [ checkedModule
                "Prelude"
                [ testBinding "$stale_keep" preludeKeepVar (Elab.ELit (Surface.LInt 1))
                , testBinding "Prelude__drop" preludeDropVar (Elab.ELit (Surface.LInt 0))
                ]
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.EVarNode preludeKeepVar)]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved = ResolvedProgram []
        }
  where
    intTy = intElabType
    preludeKeepVar = topLevelVar 20 "Prelude__keep" "Prelude" "keep" intTy
    preludeDropVar = topLevelVar 21 "Prelude__drop" "Prelude" "drop" intTy
    mainVar = topLevelVar 22 "Main__main" "Main" "main" intTy

stalePreludeBindingPayloadProgram :: CheckedProgram
stalePreludeBindingPayloadProgram =
    CheckedProgram
        { checkedProgramModules =
            [ checkedModule
                "Prelude"
                [ testBinding "Prelude__keep" preludeKeepVar (Elab.ELit (Surface.LInt 1))
                , testBinding "Prelude__drop" preludeDropVar (Elab.ELit (Surface.LInt 0))
                ]
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.EVarNode stalePreludeKeepVar)]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved = ResolvedProgram []
        }
  where
    intTy = intElabType
    preludeKeepVar = topLevelVar 20 "Prelude__keep" "Prelude" "keep" intTy
    stalePreludeKeepVar = topLevelVar 20 "$stale_keep" "Prelude" "$stale_keep" intTy
    preludeDropVar = topLevelVar 21 "Prelude__drop" "Prelude" "drop" intTy
    mainVar = topLevelVar 22 "Main__main" "Main" "main" intTy

stalePreludeModuleNameProgram :: CheckedProgram
stalePreludeModuleNameProgram =
    CheckedProgram
        { checkedProgramModules =
            [ ( checkedModule
                  "$stale_prelude"
                  [ testBinding "Prelude__keep" preludeKeepVar (Elab.ELit (Surface.LInt 1))
                  , testBinding "Prelude__drop" preludeDropVar (Elab.ELit (Surface.LInt 0))
                  ]
              )
                { checkedModuleIdentity = moduleIdentity "Prelude" }
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.EVarNode preludeKeepVar)]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved = ResolvedProgram []
        }
  where
    intTy = intElabType
    preludeKeepVar = topLevelVar 23 "Prelude__keep" "Prelude" "keep" intTy
    preludeDropVar = topLevelVar 24 "Prelude__drop" "Prelude" "drop" intTy
    mainVar = topLevelVar 25 "Main__main" "Main" "main" intTy

ambiguousPreludeModuleIdentityProgram :: CheckedProgram
ambiguousPreludeModuleIdentityProgram =
    CheckedProgram
        { checkedProgramModules =
            [ ( checkedModule
                    "$prelude_left"
                    [ testBinding "Prelude__keep" preludeKeepVar (Elab.ELit (Surface.LInt 1))
                    , testBinding "Prelude__drop" preludeDropVar (Elab.ELit (Surface.LInt 0))
                    ]
              )
                { checkedModuleIdentity = generatedSymbolIdentity 210 SymbolModule "PreludeLeft" "Prelude" Nothing }
            , ( checkedModule
                    "$prelude_right"
                    [ testBinding "OtherPrelude__drop" otherPreludeDropVar (Elab.ELit (Surface.LInt 0))
                    ]
              )
                { checkedModuleIdentity = generatedSymbolIdentity 210 SymbolModule "PreludeRight" "Prelude" Nothing }
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.EVarNode preludeKeepVar)]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved = ResolvedProgram []
        }
  where
    intTy = intElabType
    preludeKeepVar = topLevelVar 26 "Prelude__keep" "Prelude" "keep" intTy
    preludeDropVar = topLevelVar 27 "Prelude__drop" "Prelude" "drop" intTy
    otherPreludeDropVar = topLevelVar 28 "OtherPrelude__drop" "OtherPrelude" "drop" intTy
    mainVar = topLevelVar 29 "Main__main" "Main" "main" intTy

stalePreludeDataPayloadProgram :: CheckedProgram
stalePreludeDataPayloadProgram =
    CheckedProgram
        { checkedProgramModules =
            [ (checkedModule "Prelude" [])
                { checkedModuleData = Map.singleton (dataInfoSymbol unitData) unitData }
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.ELit (Surface.LInt 1))]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved =
            ResolvedProgram
                [ resolvedModuleWithReferences "Main" [typeReference staleUnitIdentity]
                ]
        }
  where
    intTy = intElabType
    mainVar = topLevelVar 32 "Main__main" "Main" "main" intTy
    staleUnitIdentity = generatedSymbolIdentity 100 SymbolType "Prelude" "$stale_Unit" Nothing
    unitData =
        DataInfo
            { dataInfoSymbol = typeIdentity "Prelude" "Unit"
            , dataTypeParams = []
            , dataConstructors = []
            }

stalePreludeDataNameProgram :: CheckedProgram
stalePreludeDataNameProgram =
    CheckedProgram
        { checkedProgramModules =
            [ (checkedModule
                "Prelude"
                [ (testBinding "Prelude__keep" preludeKeepVar (Elab.ELit (Surface.LInt 1)))
                    { checkedBindingSourceTypeView =
                        withTypeHeadIdentities
                            [ ("$stale_source_name", typeIdentity "Prelude" "Unit")
                            , ("Int", builtinIntIdentity)
                            ]
                            ( mkTypeView
                                (Surface.STArrow (Surface.STBase "$stale_source_name") (Surface.STBase "Int"))
                                (Surface.STArrow (Surface.STBase "$stale_source_name") (Surface.STBase "Int"))
                            )
                    , checkedBindingType =
                        Elab.TArrow
                            (Elab.TBaseWithIdentity (Just (typeIdentity "Prelude" "Unit")) (BaseTy "$stale_elab_name"))
                            intTy
                    }
                ])
                { checkedModuleData = Map.singleton (dataInfoSymbol staleUnitData) staleUnitData }
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.EVarNode preludeKeepVar)]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved = ResolvedProgram []
        }
  where
    intTy = intElabType
    preludeKeepVar = topLevelVar 30 "Prelude__keep" "Prelude" "keep" intTy
    mainVar = topLevelVar 31 "Main__main" "Main" "main" intTy
    staleUnitData =
        DataInfo
            { dataInfoSymbol = typeIdentity "Prelude" "Unit"
            , dataTypeParams = []
            , dataConstructors = []
            }

stalePreludeConstructorOwnerProgram :: CheckedProgram
stalePreludeConstructorOwnerProgram =
    CheckedProgram
        { checkedProgramModules =
            [ (checkedModule "Prelude" [])
                { checkedModuleData = Map.singleton (dataInfoSymbol unitData) unitData }
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.ELit (Surface.LInt 1))]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved =
            ResolvedProgram
                [ resolvedModuleWithReferences "Main" [constructorReference staleUnitConstructor]
                ]
        }
  where
    intTy = intElabType
    mainVar = topLevelVar 40 "Main__main" "Main" "main" intTy
    unitData =
        DataInfo
            { dataInfoSymbol = typeIdentity "Prelude" "Unit"
            , dataTypeParams = []
            , dataConstructors = [staleUnitConstructor]
            }
    staleUnitConstructor =
        ConstructorInfo
            { ctorInfoSymbol = constructorIdentity "Prelude" "$stale_Unit_owner" "Unit"
            , ctorRuntimeName = "Prelude__Unit"
            , ctorTypeView =
                withTypeHeadIdentities
                    [("Unit", typeIdentity "Prelude" "Unit")]
                    (mkTypeView (Surface.STBase "Unit") (Surface.STBase "Prelude.Unit"))
            , ctorForallBinderInfo = []
            , ctorOwningTypeIdentity = typeIdentity "Prelude" "Unit"
            , ctorIndex = 0
            , ctorOwnerConstructors = []
            }

staleResolvedModuleNameProgram :: CheckedProgram
staleResolvedModuleNameProgram =
    CheckedProgram
        { checkedProgramModules =
            [ (checkedModule "Prelude" [])
                { checkedModuleData = Map.singleton (dataInfoSymbol unitData) unitData }
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.ELit (Surface.LInt 1))]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved =
            ResolvedProgram
                [ resolvedModuleWithIdentityAndReferences "Prelude" "Main" [constructorReference unitConstructor]
                ]
        }
  where
    intTy = intElabType
    mainVar = topLevelVar 50 "Main__main" "Main" "main" intTy
    unitData =
        DataInfo
            { dataInfoSymbol = typeIdentity "Prelude" "Unit"
            , dataTypeParams = []
            , dataConstructors = [unitConstructor]
            }
    unitConstructor =
        ConstructorInfo
            { ctorInfoSymbol = constructorIdentity "Prelude" "Unit" "Unit"
            , ctorRuntimeName = "Prelude__Unit"
            , ctorTypeView =
                withTypeHeadIdentities
                    [("Unit", typeIdentity "Prelude" "Unit")]
                    (mkTypeView (Surface.STBase "Unit") (Surface.STBase "Prelude.Unit"))
            , ctorForallBinderInfo = []
            , ctorOwningTypeIdentity = typeIdentity "Prelude" "Unit"
            , ctorIndex = 0
            , ctorOwnerConstructors = []
            }

stalePreludeConstructorTypeProgram :: CheckedProgram
stalePreludeConstructorTypeProgram =
    CheckedProgram
        { checkedProgramModules =
            [ (checkedModule "Prelude" [])
                { checkedModuleData =
                    Map.fromList
                        [ (dataInfoSymbol unitData, unitData)
                        , (dataInfoSymbol boxData, boxData)
                        ]
                }
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.ELit (Surface.LInt 1))]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved =
            ResolvedProgram
                [ resolvedModuleWithReferences "Main" [constructorReference boxConstructor]
                ]
        }
  where
    intTy = intElabType
    mainVar = topLevelVar 60 "Main__main" "Main" "main" intTy
    unitData =
        DataInfo
            { dataInfoSymbol = typeIdentity "Prelude" "Unit"
            , dataTypeParams = []
            , dataConstructors = []
            }
    boxData =
        DataInfo
            { dataInfoSymbol = boxTypeIdentity
            , dataTypeParams = []
            , dataConstructors = [boxConstructor]
            }
    boxConstructor =
        ConstructorInfo
            { ctorInfoSymbol =
                generatedSymbolIdentity
                    301
                    SymbolConstructor
                    "Prelude"
                    "Box"
                    (Just (SymbolOwnerType boxTypeIdentity))
            , ctorRuntimeName = "Prelude__Box"
            , ctorTypeView =
                withTypeHeadIdentities
                    [ ("$stale_unit", dataInfoSymbol unitData)
                    , ("$stale_box", boxTypeIdentity)
                    ]
                    ( mkTypeView
                        (Surface.STArrow (Surface.STBase "$stale_unit") (Surface.STBase "$stale_box"))
                        ( Surface.STArrow
                            (Surface.STBase (symbolIdentityStableName (dataInfoSymbol unitData)))
                            (Surface.STBase (symbolIdentityStableName boxTypeIdentity))
                        )
                    )
            , ctorForallBinderInfo = []
            , ctorOwningTypeIdentity = boxTypeIdentity
            , ctorIndex = 0
            , ctorOwnerConstructors = []
            }

conflictingPreludeConstructorOwnerProgram :: CheckedProgram
conflictingPreludeConstructorOwnerProgram =
    CheckedProgram
        { checkedProgramModules =
            [ (checkedModule "Prelude" [])
                { checkedModuleData =
                    Map.fromList
                        [ (dataInfoSymbol unitData, unitData)
                        , (dataInfoSymbol boxData, boxData)
                        ]
                }
            , checkedModule "Main" [testBinding "Main__main" mainVar (Elab.ELit (Surface.LInt 1))]
            ]
        , checkedProgramMainResolvedVar = mainVar
        , checkedProgramResolved =
            ResolvedProgram
                [ resolvedModuleWithReferences "Main" [constructorReference unitConstructor]
                ]
        }
  where
    intTy = intElabType
    mainVar = topLevelVar 61 "Main__main" "Main" "main" intTy
    unitData =
        DataInfo
            { dataInfoSymbol = typeIdentity "Prelude" "Unit"
            , dataTypeParams = []
            , dataConstructors = [unitConstructor]
            }
    boxData =
        DataInfo
            { dataInfoSymbol = boxTypeIdentity
            , dataTypeParams = []
            , dataConstructors = [boxConstructor]
            }
    unitConstructor =
        ConstructorInfo
            { ctorInfoSymbol = constructorIdentity "Prelude" "Unit" "Shared"
            , ctorRuntimeName = "Prelude__SharedUnit"
            , ctorTypeView =
                withTypeHeadIdentities
                    [("Unit", typeIdentity "Prelude" "Unit")]
                    (mkTypeView (Surface.STBase "Unit") (Surface.STBase "Prelude.Unit"))
            , ctorForallBinderInfo = []
            , ctorOwningTypeIdentity = typeIdentity "Prelude" "Unit"
            , ctorIndex = 0
            , ctorOwnerConstructors = []
            }
    boxConstructor =
        ConstructorInfo
            { ctorInfoSymbol = constructorIdentity "Prelude" "Box" "Shared"
            , ctorRuntimeName = "Prelude__SharedBox"
            , ctorTypeView =
                withTypeHeadIdentities
                    [("Box", boxTypeIdentity)]
                    (mkTypeView (Surface.STBase "Box") (Surface.STBase "Prelude.Box"))
            , ctorForallBinderInfo = []
            , ctorOwningTypeIdentity = boxTypeIdentity
            , ctorIndex = 0
            , ctorOwnerConstructors = []
            }

checkedModule :: String -> [CheckedBinding] -> CheckedModule
checkedModule name bindings =
    CheckedModule
        { checkedModuleName = name
        , checkedModuleIdentity = moduleIdentity name
        , checkedModuleBindings = bindings
        , checkedModuleData = Map.empty
        , checkedModuleClasses = Map.empty
        , checkedModuleInstances = []
        , checkedModuleExports = moduleExportsFromMaps Map.empty Map.empty Map.empty
        }

testBinding :: String -> ResolvedVar -> Elab.XmlfTerm -> CheckedBinding
testBinding name resolved term =
    CheckedBinding
        { checkedBindingResolvedVar = resolved
        , checkedBindingSourceTypeView = intTypeView
        , checkedBindingDeferredObligations = Map.empty
        , checkedBindingTerm = term
        , checkedBindingType = intElabType
        , checkedBindingExportedAsMain = name == "Main__main"
        }

builtinIntIdentity :: SymbolIdentity
builtinIntIdentity =
    PrimitiveInventory.builtinTypeIdentity "Int"

intElabType :: Elab.ElabType
intElabType =
    Elab.TBaseWithIdentity (Just builtinIntIdentity) (BaseTy "Int")

intTypeView :: TypeView
intTypeView =
    withTypeHeadIdentities
        [("Int", builtinIntIdentity)]
        (mkTypeView (Surface.STBase "Int") (Surface.STBase "Int"))

withTypeHeadIdentities :: [(String, SymbolIdentity)] -> TypeView -> TypeView
withTypeHeadIdentities identities view =
    view
        { typeViewHeadIdentities =
            Map.fromList identities `Map.union` typeViewHeadIdentities view
        }

topLevelVar :: Int -> String -> String -> String -> Elab.ElabType -> ResolvedVar
topLevelVar unique _runtimeName moduleName sourceName ty =
    ResolvedVar
        {
        resolvedVarType = ty
        , resolvedVarDetails =
            TopLevelId (generatedSymbolIdentity unique SymbolValue moduleName sourceName Nothing)
        }

generatedSymbolIdentity :: Int -> SymbolNamespace -> String -> String -> Maybe SymbolOwnerIdentity -> SymbolIdentity
generatedSymbolIdentity unique namespace moduleName sourceName owner =
    symbolIdentityFromParts (UniqueIdentity unique) namespace moduleName sourceName owner

typeIdentity :: String -> String -> SymbolIdentity
typeIdentity moduleName sourceName =
    generatedSymbolIdentity 100 SymbolType moduleName sourceName Nothing

boxTypeIdentity :: SymbolIdentity
boxTypeIdentity =
    generatedSymbolIdentity 101 SymbolType "Prelude" "Box" Nothing

moduleIdentity :: String -> SymbolIdentity
moduleIdentity name =
    generatedSymbolIdentity (moduleIdentityUnique name) SymbolModule name name Nothing
  where
    moduleIdentityUnique "Prelude" = 200
    moduleIdentityUnique "Main" = 201
    moduleIdentityUnique _ = 202

constructorIdentity :: String -> String -> String -> SymbolIdentity
constructorIdentity moduleName typeName sourceName =
    generatedSymbolIdentity
        300
        SymbolConstructor
        moduleName
        sourceName
        (Just (SymbolOwnerType (typeIdentity moduleName typeName)))

constructorReference :: ConstructorInfo -> ResolvedReference
constructorReference ctor =
    mkResolvedReference
        ResolvedConstructorReference
        (ctorName ctor)
        (mkResolvedSymbol (ctorInfoSymbol ctor) (ctorName ctor) (ctorName ctor) (SymbolUnqualifiedImport "Prelude"))

typeReference :: SymbolIdentity -> ResolvedReference
typeReference identity =
    mkResolvedReference
        ResolvedTypeReference
        (symbolDefiningName identity)
        (mkResolvedSymbol identity (symbolDefiningName identity) (symbolDefiningName identity) (SymbolUnqualifiedImport "Prelude"))

resolvedModuleWithReferences :: String -> [ResolvedReference] -> ResolvedModule
resolvedModuleWithReferences name references =
    resolvedModuleWithIdentityAndReferences name name references

resolvedModuleWithIdentityAndReferences :: String -> String -> [ResolvedReference] -> ResolvedModule
resolvedModuleWithIdentityAndReferences displayName identityName references =
    ResolvedModule
        { resolvedModuleSemantic =
            ResolvedSemanticModule
                { resolvedSemanticModuleName = displayName
                , resolvedSemanticModuleIdentity = moduleIdentity identityName
                , resolvedSemanticModuleSyntax = ProgramSyntax.Module displayName Nothing [] []
                , resolvedSemanticModuleLocalSymbols = ResolvedLocalSymbols Map.empty Map.empty Map.empty
                , resolvedSemanticModuleScope = emptyResolvedScope
                , resolvedSemanticModuleExports = emptyResolvedScope
                }
        , resolvedModuleDiagnosticAdapter = ResolvedModuleDiagnosticAdapter references
        }
  where
    emptyResolvedScope =
        ResolvedScope Map.empty Map.empty Map.empty Map.empty

requireLocated :: FilePath -> String -> IO ProgramSyntax.LocatedProgram
requireLocated path source =
    case parseLocatedProgramWithFile path source of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right located -> pure located

requirePreludeModule :: CheckedProgram -> IO CheckedModule
requirePreludeModule checked =
    case [candidate | candidate <- checkedProgramModules checked, checkedModuleName candidate == "Prelude"] of
        [preludeModule] ->
            pure preludeModule
        [] ->
            expectationFailure "expected prepared program to contain the Prelude module"
                >> fail "missing Prelude module"
        preludeModules ->
            expectationFailure ("expected one Prelude module, got " ++ show (length preludeModules))
                >> fail "duplicate Prelude modules"

requireModuleByIdentity :: SymbolIdentity -> CheckedProgram -> IO CheckedModule
requireModuleByIdentity identity checked =
    case [candidate | candidate <- checkedProgramModules checked, checkedModuleIdentity candidate == identity] of
        [matchedModule] ->
            pure matchedModule
        [] ->
            expectationFailure "expected prepared program to contain module identity"
                >> fail "missing module identity"
        modules0 ->
            expectationFailure ("expected one module identity, got " ++ show (length modules0))
                >> fail "duplicate module identity"

requireRight :: (Show err) => Either err a -> IO a
requireRight =
    \case
        Left err ->
            expectationFailure (show err) >> fail "unexpected Left"
        Right value ->
            pure value
