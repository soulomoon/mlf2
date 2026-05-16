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
    , ExportedTypeInfo (..)
    , ModuleExports (..)
    , ProgramError (..)
    , SymbolIdentity (..)
    , SymbolNamespace (..)
    , ValueInfo (..)
    )
import MLF.Frontend.Syntax.Program qualified as P

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

            Map.keys (exportedValues (moduleInterfaceExports libInterface))
                `shouldSatisfy` containsAll ["eq", "token", "Zero"]
            Map.keys (exportedClasses (moduleInterfaceExports libInterface)) `shouldBe` ["Eq"]
            Map.keys (exportedTypes (moduleInterfaceExports libInterface)) `shouldBe` ["Nat", "Token"]
            Map.keys
                ( exportedTypeConstructors
                    (exportedTypes (moduleInterfaceExports libInterface) Map.! "Token")
                )
                `shouldBe` []
            Map.keys
                ( exportedTypeConstructors
                    (exportedTypes (moduleInterfaceExports libInterface) Map.! "Nat")
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
            validatePackageInterface graph missingDependency
                `shouldBe` Left (ProgramInterfaceModuleMissing libId)

        it "rejects extracting an interface when the checked module does not match the graph node" $ do
            (graph, checked, _packageInterface) <- requireCheckedPackageInterface interfacePackage
            let libNode = packageModuleGraphNodes graph !! 0
                mainModule = checkedProgramModules checked !! 1

            moduleInterfaceFromCheckedModule libNode mainModule
                `shouldBe` Left (ProgramInterfaceCheckedModuleMismatch (packageModuleGraphNodeId libNode) "Main")

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

poisonExportOwner :: ModuleInterface -> ModuleInterface
poisonExportOwner interface =
    interface
        { moduleInterfaceExports =
            (moduleInterfaceExports interface)
                { exportedValues =
                    Map.adjust poisonValueOwner "token" (exportedValues (moduleInterfaceExports interface))
                }
        }
  where
    poisonValueOwner valueInfo@OrdinaryValue {} =
        valueInfo
            { valueInfoSymbol =
                SymbolIdentity SymbolValue "Other" (valueDisplayName valueInfo) Nothing
            }
    poisonValueOwner valueInfo = valueInfo

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
