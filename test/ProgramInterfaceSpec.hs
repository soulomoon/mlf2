{-# LANGUAGE LambdaCase #-}

module ProgramInterfaceSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Test.Hspec

import MLF.Frontend.Parse.Program
    ( parseRawProgram
    , renderProgramParseError
    )
import MLF.Frontend.Program.Check (checkProgramPackage)
import MLF.Frontend.Program.Interface
    ( ModuleInterface (..)
    , PackageInterface (..)
    , ProgramInterfaceError (..)
    , moduleInterfaceFromCheckedModule
    , packageInterfaceFromCheckedProgram
    , packageInterfaceModuleById
    , validatePackageInterface
    )
import MLF.Frontend.Program.Package
    ( PackageId (..)
    , PackageModuleGraph (..)
    , PackageModuleGraphNode (..)
    , PackageModuleId (..)
    , ProgramPackage (..)
    , ProgramSourceUnit (..)
    , programPackageModuleGraph
    )
import MLF.Frontend.Program.Prelude (withPreludePackage)
import MLF.Frontend.Program.Types
    ( CheckedModule (..)
    , CheckedProgram (..)
    , ConstructorInfo (..)
    , DataInfo (..)
    , ExportedTypeInfo (..)
    , ModuleExports (..)
    , ProgramError (..)
    , SymbolIdentity
    , symbolIdentityFromParts
    , SymbolNamespace (..)
    , symbolUniqueIdentity
    , ValueInfo (..)
    , ctorName
    , dataInfoSymbolIdentity
    , exportedClassesForDisplay
    , exportedTypeConstructorsForDisplay
    , exportedTypesForDisplay
    , exportedValuesForDisplay
    , mkExportedTypeInfo
    , mkTypeView
    , moduleExportsFromMaps
    , valueInfoIdentityName
    )
import MLF.Frontend.Syntax (SrcTy (..))
import MLF.Frontend.Syntax.Program qualified as P
import MLF.Types.Identity (UniqueIdentity (..))

spec :: Spec
spec = do
    describe "MLF.Program interface artifacts" $ do
        it "extracts checked exports and package dependency metadata into typed module interfaces" $ do
            (graph, checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libId = PackageModuleId testPackageId "Lib"
                mainId = PackageModuleId testPackageId "Main"
            libInterface <- requireInterface libId packageInterface
            mainInterface <- requireInterface mainId packageInterface

            moduleInterfaceSourcePath libInterface `shouldBe` Just "src/Lib.mlfp"
            moduleInterfaceDependencies libInterface `shouldBe` []
            moduleInterfaceSourcePath mainInterface `shouldBe` Just "app/Main.mlfp"
            moduleInterfaceDependencies mainInterface `shouldBe` [libId]
            map moduleInterfaceIdentity (packageInterfaceModules packageInterface)
                `shouldBe` map checkedModuleIdentity (checkedProgramModules checked)

            Map.keys (exportedValuesForDisplay (moduleInterfaceExports libInterface))
                `shouldSatisfy` containsAll ["eq", "token", "Zero"]
            Map.keys (exportedClassesForDisplay (moduleInterfaceExports libInterface)) `shouldBe` ["Eq"]
            Map.keys (exportedTypesForDisplay (moduleInterfaceExports libInterface)) `shouldBe` ["Nat", "Token"]
            map ctorName
                ( Map.elems
                    ( exportedTypeConstructorsByIdentity
                        (exportedTypesForDisplay (moduleInterfaceExports libInterface) Map.! "Token")
                    )
                )
                `shouldBe` []
            map ctorName
                ( Map.elems
                    ( exportedTypeConstructorsByIdentity
                        (exportedTypesForDisplay (moduleInterfaceExports libInterface) Map.! "Nat")
                    )
                )
                `shouldBe` ["Zero"]
            length (moduleInterfaceInstances libInterface) `shouldBe` 1

            map checkedModuleName (checkedProgramModules checked) `shouldBe` ["Lib", "Main"]
            validatePackageInterface graph packageInterface `shouldBe` Right ()

        it "checks cross-file imports through the interface export boundary" $ do
            checkProgramPackage interfacePackage `shouldSatisfy` \case
                Right _ -> True
                Left err -> not ("ProgramUnknownImportModule" `isInfixOf` show err)

        it "rejects hidden constructors across files through the interface export boundary" $ do
            let package =
                    packageFromSourceUnits
                        [ ("src/Lib.mlfp", libSource)
                        ,
                            ( "app/Main.mlfp"
                            , unlines
                                [ "module Main export (main) {"
                                , "  import Lib exposing (Token(..));"
                                , "  def main : Bool = true;"
                                , "}"
                                ]
                            )
                        ]

            checkProgramPackage package
                `shouldBe` Left (ProgramImportNotExported "Lib" "Token")

        it "fails closed for malformed interface artifacts" $ do
            (graph, _checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libId = PackageModuleId testPackageId "Lib"
                mainId = PackageModuleId testPackageId "Main"
            libInterface <- requireInterface libId packageInterface
            mainInterface <- requireInterface mainId packageInterface

            let wrongModuleId =
                    packageInterface
                        { packageInterfaceModules =
                            libInterface
                                { moduleInterfaceId = PackageModuleId testPackageId "Other"
                                }
                                : [mainInterface]
                        }
                wrongDependencies =
                    packageInterface
                        { packageInterfaceModules =
                            [ libInterface
                            , mainInterface {moduleInterfaceDependencies = []}
                            ]
                        }
                wrongSourcePath =
                    packageInterface
                        { packageInterfaceModules =
                            [ libInterface {moduleInterfaceSourcePath = Just "wrong/Lib.mlfp"}
                            , mainInterface
                            ]
                        }
                wrongOwner =
                    packageInterface
                        { packageInterfaceModules =
                            [ poisonExportOwner libInterface
                            , mainInterface
                            ]
                        }
                duplicateModuleIdentity =
                    symbolIdentityFromParts
                        (symbolUniqueIdentity (moduleInterfaceIdentity libInterface))
                        SymbolModule
                        "Main"
                        "Main"
                        Nothing
                duplicateModuleInterfaceIdentity =
                    packageInterface
                        { packageInterfaceModules =
                            [ libInterface
                            , mainInterface {moduleInterfaceIdentity = duplicateModuleIdentity}
                            ]
                        }
                missingDependency =
                    packageInterface
                        { packageInterfaceModules = [mainInterface]
                        }

            validatePackageInterface graph wrongModuleId `shouldSatisfy` isLeft
            validatePackageInterface graph wrongDependencies
                `shouldBe` Left (ProgramInterfaceDependenciesMismatch mainId [libId] [])
            validatePackageInterface graph wrongSourcePath
                `shouldBe` Left (ProgramInterfaceSourcePathMismatch libId (Just "src/Lib.mlfp") (Just "wrong/Lib.mlfp"))
            validatePackageInterface graph wrongOwner `shouldSatisfy` isLeft
            validatePackageInterface graph duplicateModuleInterfaceIdentity
                `shouldBe` Left (ProgramInterfaceDuplicateMetadataIdentity mainId "module" duplicateModuleIdentity)
            validatePackageInterface graph missingDependency
                `shouldBe` Left (ProgramInterfaceModuleMissing libId)

        it "rejects extracting an interface when the checked module does not match the graph node" $ do
            (graph, checked, _packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libNode = packageModuleGraphNodes graph !! 0
                mainModule = checkedProgramModules checked !! 1

            moduleInterfaceFromCheckedModule libNode mainModule
                `shouldBe` Left (ProgramInterfaceCheckedModuleMismatch (packageModuleGraphNodeId libNode) (checkedModuleIdentity mainModule))

        it "extracts interfaces by checked module identity when module names are stale" $ do
            (graph, checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let checked' =
                    checked
                        { checkedProgramModules =
                            map staleCheckedModuleName (checkedProgramModules checked)
                        }

            packageInterfaceFromCheckedProgram graph checked' `shouldBe` Right packageInterface

        it "accepts Prelude-owned interface exports for builtin opaque types" $ do
            let package =
                    withPreludePackage
                        ( packageFromSourceUnits
                            [
                                ( "app/Main.mlfp"
                                , unlines
                                    [ "module Main export (main) {"
                                    , "  import Prelude exposing (IO);"
                                    , "  def main : Bool = true;"
                                    , "}"
                                    ]
                                )
                            ]
                        )

            checkProgramPackage package `shouldSatisfy` isRight

    describe "MLF.Program resolved symbol identities" $ do
        it "requires exported interface identities to define the exporting module" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface

            validatePackageInterface
                (singleInterfaceGraph libInterface)
                (PackageInterface [poisonExportOwner libInterface])
                `shouldSatisfy` isLeft

        it "requires identity-indexed interface maps to use payload identities" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface
            case Map.toList (moduleInterfaceDataByIdentity libInterface) of
                [] -> expectationFailure "expected interface data entries"
                (actualKey, dataInfo) : _ -> do
                    let staleKey = symbolIdentityFromParts (UniqueIdentity 900001) SymbolType "Lib" "Stale" Nothing
                        staleInterface =
                            libInterface
                                { moduleInterfaceDataByIdentity =
                                    Map.insert staleKey dataInfo (Map.delete actualKey (moduleInterfaceDataByIdentity libInterface))
                                }
                    validatePackageInterface
                        (singleInterfaceGraph libInterface)
                        (PackageInterface [staleInterface])
                        `shouldBe` Left (ProgramInterfaceIdentityKeyMismatch libId staleKey (dataInfoSymbolIdentity dataInfo))

        it "rejects duplicate constructor identities inside interface data metadata" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface constructorInterfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface
            case duplicateFirstTwoConstructorIdentities libInterface of
                Nothing ->
                    expectationFailure "expected interface data with at least two constructors"
                Just (duplicateIdentity, duplicateInterface) ->
                    validatePackageInterface
                        (singleInterfaceGraph duplicateInterface)
                        (PackageInterface [duplicateInterface])
                        `shouldBe` Left (ProgramInterfaceDuplicateMetadataIdentity libId "constructor" duplicateIdentity)

        it "requires constructor display identity keys to match exported constructors" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface
            let exports = moduleInterfaceExports libInterface
                typeInfo = exportedTypesForDisplay exports Map.! "Nat"
                typeIdentity = dataInfoSymbolIdentity (exportedTypeData typeInfo)
                staleKey = symbolIdentityFromParts (UniqueIdentity 900002) SymbolConstructor "Lib" "Stale" Nothing
                staleTypeInfo =
                    typeInfo
                        { exportedTypeConstructorDisplaysByIdentity =
                            Map.insert staleKey "Stale" (exportedTypeConstructorDisplaysByIdentity typeInfo)
                        }
                staleExports =
                    exports
                        { exportedTypesByIdentity =
                            Map.adjust
                                (const staleTypeInfo)
                                typeIdentity
                                (exportedTypesByIdentity exports)
                        }
                expectedKeys = Map.keys (exportedTypeConstructorsByIdentity typeInfo)
                actualKeys = Map.keys (exportedTypeConstructorDisplaysByIdentity staleTypeInfo)
            validatePackageInterface
                (singleInterfaceGraph libInterface)
                (PackageInterface [libInterface {moduleInterfaceExports = staleExports}])
                `shouldBe` Left (ProgramInterfaceIdentityKeySetMismatch libId expectedKeys actualKeys)

        it "requires exported value display identity keys to match exported values" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface
            let exports = moduleInterfaceExports libInterface
            case Map.toList (exportedValuesByIdentity exports) of
                [] -> expectationFailure "expected exported values"
                (actualKey, _) : _ -> do
                    let staleKey = symbolIdentityFromParts (UniqueIdentity 900004) SymbolValue "Lib" "Stale" Nothing
                        staleExports =
                            exports
                                { exportedValueDisplaysByIdentity =
                                    Map.insert staleKey "Stale" (Map.delete actualKey (exportedValueDisplaysByIdentity exports))
                                }
                        expectedKeys = Map.keys (exportedValuesByIdentity exports)
                        actualKeys = Map.keys (exportedValueDisplaysByIdentity staleExports)
                    validatePackageInterface
                        (singleInterfaceGraph libInterface)
                        (PackageInterface [libInterface {moduleInterfaceExports = staleExports}])
                        `shouldBe` Left (ProgramInterfaceIdentityKeySetMismatch libId expectedKeys actualKeys)

        it "rejects exported display names shared by multiple identities" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface
            let exports = moduleInterfaceExports libInterface
            case Map.keys (exportedValueDisplaysByIdentity exports) of
                firstKey : secondKey : _ -> do
                    let duplicateName = exportedValueDisplaysByIdentity exports Map.! firstKey
                        duplicateExports =
                            exports
                                { exportedValueDisplaysByIdentity =
                                    Map.insert secondKey duplicateName (exportedValueDisplaysByIdentity exports)
                                }
                    validatePackageInterface
                        (singleInterfaceGraph libInterface)
                        (PackageInterface [libInterface {moduleInterfaceExports = duplicateExports}])
                        `shouldSatisfy` isLeft
                    Map.member duplicateName (exportedValuesForDisplay duplicateExports) `shouldBe` False
                _ -> expectationFailure "expected at least two exported values"

        it "does not synthesize a display name when one value identity has multiple visible names" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface
            let exports = moduleInterfaceExports libInterface
            case Map.toList (exportedValuesByIdentity exports) of
                [] -> expectationFailure "expected exported values"
                (valueIdentity, valueInfo) : _ -> do
                    let ambiguousExports =
                            moduleExportsFromMaps
                                (Map.fromList [("aliasLeft", valueInfo), ("aliasRight", valueInfo)])
                                Map.empty
                                Map.empty
                    Map.keys (exportedValuesByIdentity ambiguousExports) `shouldBe` [valueIdentity]
                    Map.lookup valueIdentity (exportedValueDisplaysByIdentity ambiguousExports) `shouldBe` Nothing
                    exportedValuesForDisplay ambiguousExports `shouldBe` Map.empty
                    validatePackageInterface
                        (singleInterfaceGraph libInterface)
                        (PackageInterface [libInterface {moduleInterfaceExports = ambiguousExports}])
                        `shouldBe` Left (ProgramInterfaceIdentityKeySetMismatch libId [valueIdentity] [])

        it "does not choose an arbitrary exported payload when one value identity has conflicting metadata" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface
            let exports = moduleInterfaceExports libInterface
                ordinaryExports =
                    [ (valueIdentity, valueInfo, symbol, runtimeName, constraints, constraintInfos)
                    | ( valueIdentity,
                        valueInfo@OrdinaryValue
                            { valueInfoSymbol = symbol
                            , valueRuntimeName = runtimeName
                            , valueConstraints = constraints
                            , valueConstraintInfos = constraintInfos
                            }
                        ) <-
                        Map.toList (exportedValuesByIdentity exports)
                    ]
            case ordinaryExports of
                [] -> expectationFailure "expected an ordinary exported value"
                (valueIdentity, valueInfo, symbol, runtimeName, constraints, constraintInfos) : _ -> do
                    let conflictingValueInfo =
                            OrdinaryValue
                                { valueInfoSymbol = symbol
                                , valueRuntimeName = runtimeName
                                , valueTypeView = mkTypeView (STBase "Bool") (STBase "Bool")
                                , valueConstraints = constraints
                                , valueConstraintInfos = constraintInfos
                                }
                        ambiguousExports =
                            moduleExportsFromMaps
                                (Map.fromList [("aliasLeft", valueInfo), ("aliasRight", conflictingValueInfo)])
                                Map.empty
                                Map.empty
                    Map.lookup valueIdentity (exportedValuesByIdentity ambiguousExports) `shouldBe` Nothing
                    Map.lookup valueIdentity (exportedValueDisplaysByIdentity ambiguousExports) `shouldBe` Nothing
                    exportedValuesForDisplay ambiguousExports `shouldBe` Map.empty

        it "does not choose an arbitrary exported constructor payload when one constructor identity has conflicting metadata" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface constructorInterfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface
            case [ (dataInfo, firstCtor, secondCtor)
                 | dataInfo <- Map.elems (moduleInterfaceDataByIdentity libInterface)
                 , firstCtor : secondCtor : _ <- [dataConstructors dataInfo]
                 ] of
                [] ->
                    expectationFailure "expected interface data with at least two constructors"
                (dataInfo, firstCtor, secondCtor) : _ -> do
                    let constructorIdentity = ctorInfoSymbol firstCtor
                        conflictingCtor = secondCtor {ctorInfoSymbol = constructorIdentity}
                        ambiguousTypeInfo =
                            mkExportedTypeInfo
                                dataInfo
                                [ ("aliasLeft", firstCtor)
                                , ("aliasRight", conflictingCtor)
                                ]
                    Map.lookup constructorIdentity (exportedTypeConstructorsByIdentity ambiguousTypeInfo) `shouldBe` Nothing
                    Map.lookup constructorIdentity (exportedTypeConstructorDisplaysByIdentity ambiguousTypeInfo) `shouldBe` Nothing
                    exportedTypeConstructorsForDisplay ambiguousTypeInfo `shouldBe` Map.empty

        it "keeps an exported constructor display when duplicate entries share identity and display" $ do
            (_graph, _checked, packageInterface) <- requireCheckedPackageInterface constructorInterfacePackage
            let libId = PackageModuleId testPackageId "Lib"
            libInterface <- requireInterface libId packageInterface
            case [ (dataInfo, ctor)
                 | dataInfo <- Map.elems (moduleInterfaceDataByIdentity libInterface)
                 , ctor <- dataConstructors dataInfo
                 ] of
                [] ->
                    expectationFailure "expected interface data with constructors"
                (dataInfo, ctor) : _ -> do
                    let constructorIdentity = ctorInfoSymbol ctor
                        typeInfo =
                            mkExportedTypeInfo
                                dataInfo
                                [ ("First", ctor)
                                , ("First", ctor)
                                ]
                    Map.lookup constructorIdentity (exportedTypeConstructorDisplaysByIdentity typeInfo) `shouldBe` Just "First"
                    Map.lookup "First" (exportedTypeConstructorsForDisplay typeInfo) `shouldBe` Just ctor

interfacePackage :: ProgramPackage
interfacePackage =
    packageFromSourceUnits
        [ ("src/Lib.mlfp", libSource)
        , ("app/Main.mlfp", mainSource)
        ]

libSource :: String
libSource =
    unlines
        [ "module Lib export (Token, Nat(..), Eq, eq, token) {"
        , "  class Eq a {"
        , "    eq : a -> a -> Bool;"
        , "  }"
        , "  data Token ="
        , "      Secret : Token;"
        , "  data Nat ="
        , "      Zero : Nat;"
        , "  instance Eq Token {"
        , "    eq = λx λy true;"
        , "  }"
        , "  def token : Token = Secret;"
        , "}"
        ]

mainSource :: String
mainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Lib exposing (Token, Nat(..), Eq, eq, token);"
        , "  def main : Nat = Zero;"
        , "}"
        ]

constructorInterfacePackage :: ProgramPackage
constructorInterfacePackage =
    packageFromSourceUnits
        [ ("src/Lib.mlfp", constructorInterfaceSource)
        , ("app/Main.mlfp", constructorInterfaceMainSource)
        ]

constructorInterfaceSource :: String
constructorInterfaceSource =
    unlines
        [ "module Lib export (Choice(..)) {"
        , "  data Choice ="
        , "      First : Choice"
        , "    | Second : Choice;"
        , "}"
        ]

constructorInterfaceMainSource :: String
constructorInterfaceMainSource =
    unlines
        [ "module Main export (main) {"
        , "  import Lib exposing (Choice(..));"
        , "  def main : Choice = First;"
        , "}"
        ]

testPackageId :: PackageId
testPackageId = PackageId "test-package"

packageFromSourceUnits :: [(FilePath, String)] -> ProgramPackage
packageFromSourceUnits units =
    ProgramPackage
        { programPackageId = testPackageId
        , programPackageSourceUnits =
            [ ProgramSourceUnit
                { programSourceUnitPath = Just path
                , programSourceUnitModules = P.programModules (parseProgram source)
                }
            | (path, source) <- units
            ]
        }

requireCheckedPackageInterface ::
    ProgramPackage ->
    IO (PackageModuleGraph, CheckedProgram, PackageInterface)
requireCheckedPackageInterface package = do
    graph <- requireRight (programPackageModuleGraph package)
    checked <- requireRight (checkProgramPackage package)
    packageInterface <- requireRight (packageInterfaceFromCheckedProgram graph checked)
    pure (graph, checked, packageInterface)

requireInterface :: PackageModuleId -> PackageInterface -> IO ModuleInterface
requireInterface moduleId packageInterface =
    case packageInterfaceModuleById moduleId packageInterface of
        Just interface -> pure interface
        Nothing -> expectationFailure ("missing interface " ++ show moduleId) >> fail "missing interface"

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value

parseProgram :: String -> P.Program
parseProgram source =
    case parseRawProgram source of
        Left err -> error (renderProgramParseError err)
        Right program -> program

containsAll :: (Ord a) => [a] -> [a] -> Bool
containsAll needles haystack =
    all (`elem` haystack) needles

staleCheckedModuleName :: CheckedModule -> CheckedModule
staleCheckedModuleName checkedModule =
    checkedModule {checkedModuleName = "$stale_" ++ checkedModuleName checkedModule}

poisonExportOwner :: ModuleInterface -> ModuleInterface
poisonExportOwner interface =
    interface
        { moduleInterfaceExports = moduleExportsFromMaps values (exportedTypesForDisplay exports) (exportedClassesForDisplay exports)
        }
  where
    exports = moduleInterfaceExports interface
    values = Map.adjust poisonValueOwner "token" (exportedValuesForDisplay exports)

    poisonValueOwner valueInfo@OrdinaryValue {} =
        valueInfo
            { valueInfoSymbol =
                symbolIdentityFromParts (UniqueIdentity 900003) SymbolValue "Other" (valueInfoIdentityName valueInfo) Nothing
            }
    poisonValueOwner valueInfo = valueInfo

duplicateFirstTwoConstructorIdentities :: ModuleInterface -> Maybe (SymbolIdentity, ModuleInterface)
duplicateFirstTwoConstructorIdentities interface =
    case
        [ (dataIdentity, dataInfo, firstCtor, secondCtor, rest)
        | (dataIdentity, dataInfo) <- Map.toList (moduleInterfaceDataByIdentity interface)
        , firstCtor : secondCtor : rest <- [dataConstructors dataInfo]
        ]
    of
        (dataIdentity, dataInfo, firstCtor, secondCtor, rest) : _ ->
            let duplicateIdentity = ctorInfoSymbol firstCtor
                duplicateData =
                    dataInfo
                        { dataConstructors =
                            firstCtor : secondCtor {ctorInfoSymbol = duplicateIdentity} : rest
                        }
                duplicateInterface =
                    interface
                        { moduleInterfaceDataByIdentity =
                            Map.insert dataIdentity duplicateData (moduleInterfaceDataByIdentity interface)
                        }
             in Just (duplicateIdentity, duplicateInterface)
        [] -> Nothing

singleInterfaceGraph :: ModuleInterface -> PackageModuleGraph
singleInterfaceGraph interface =
    PackageModuleGraph
        { packageModuleGraphNodes =
            [ PackageModuleGraphNode
                { packageModuleGraphNodeId = moduleInterfaceId interface
                , packageModuleGraphNodeSourcePath = moduleInterfaceSourcePath interface
                , packageModuleGraphNodeImports = moduleInterfaceDependencies interface
                }
            ]
        , packageModuleGraphOrder = [moduleInterfaceId interface]
        }
