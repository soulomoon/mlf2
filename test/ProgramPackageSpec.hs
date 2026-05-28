module ProgramPackageSpec (spec) where

import Control.Exception (bracket)
import Data.Either (isRight)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, hPutStr, openTempFile)
import Test.Hspec

import MLF.Backend.Emission.Prepare (prepareBackendEmissionFromSource)
import MLF.Frontend.Parse.Program
    ( parseLocatedProgramWithFile
    , parseRawProgram
    , renderProgramParseError
    )
import MLF.Frontend.ConstraintGen
    ( ExternalBinding (..)
    , ExternalBindingMode (..)
    , ModuleConstraintResult (..)
    , RootOwnershipIndex (..)
    , generateModuleConstraintsWithExternalBindings
    )
import MLF.Frontend.Program.Check
    ( checkLocatedProgramPackage
    , checkProgram
    , checkProgramPackage
    )
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage (..)
    , LocatedProgramSourceUnit (..)
    , PackageModuleId (..)
    , locatedProgramPackageModuleIds
    , trivialLocatedProgramPackage
    , trivialPackageId
    , trivialProgramPackage
    )
import MLF.Frontend.Program.Prelude (withPreludeLocatedPackage)
import MLF.Frontend.Program.Types
    ( CheckedModule (..)
    , CheckedProgram (..)
    )
import MLF.Frontend.Syntax
    ( Expr (..)
    , NormSrcType
    , NormSurfaceExpr
    , SrcTy (..)
    )
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Program.CLI (checkProgramArgs, runProgramFile)

spec :: Spec
spec =
    describe "MLF.Program package owner" $ do
        it "turns a located one-file program into one trivial package source unit" $ do
            located <- requireLocatedWithFile "single.mlfp" multiModuleSource
            let package = trivialLocatedProgramPackage located

            locatedProgramPackageId package `shouldBe` trivialPackageId
            map locatedProgramSourceUnitPath (locatedProgramPackageSourceUnits package)
                `shouldBe` [Just "single.mlfp"]
            map (map P.moduleName . locatedProgramSourceUnitModules) (locatedProgramPackageSourceUnits package)
                `shouldBe` [["Lib", "Main"]]
            locatedProgramPackageModuleIds package
                `shouldBe`
                    [ PackageModuleId trivialPackageId "Lib"
                    , PackageModuleId trivialPackageId "Main"
                    ]

        it "checks an existing single-file fixture through the trivial package owner" $ do
            source <- readFile "test/programs/recursive-adt/plain-recursive-nat.mlfp"
            program <- requireParsed source
            checkedProgram <- requireRight (checkProgram program)
            checkedPackage <- requireRight (checkProgramPackage (trivialProgramPackage program))

            checkedProgramMain checkedPackage `shouldBe` checkedProgramMain checkedProgram
            map checkedModuleName (checkedProgramModules checkedPackage)
                `shouldBe` map checkedModuleName (checkedProgramModules checkedProgram)

        it "prepends the Prelude source unit to a located trivial package" $ do
            located <-
                requireLocatedWithFile
                    "explicit-prelude.mlfp"
                    ( unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Nat(..), Option(..));"
                        , "  def main : Option Nat = Some Zero;"
                        , "}"
                        ]
                    )
            let package = withPreludeLocatedPackage (trivialLocatedProgramPackage located)

            locatedProgramPackageModuleIds package
                `shouldBe`
                    [ PackageModuleId trivialPackageId "Prelude"
                    , PackageModuleId trivialPackageId "Main"
                    ]
            checkLocatedProgramPackage package `shouldSatisfy` isRight

        it "keeps the CLI helper running single-file input as a trivial package" $ do
            runProgramFile "test/programs/recursive-adt/plain-recursive-nat.mlfp"
                `shouldReturn` Right "true\n"

        it "keeps backend preparation on the located trivial package path" $ do
            checked <-
                requireRight $
                    prepareBackendEmissionFromSource
                        "inline-unit.mlfp"
                        ( unlines
                            [ "module Main export (main) {"
                            , "  import Prelude exposing (Unit(..));"
                            , "  def main : Unit = Unit;"
                            , "}"
                            ]
                        )

            map checkedModuleName (checkedProgramModules checked) `shouldBe` ["Prelude", "Main"]

        it "checks independent annotated defs equivalently through CLI batch size 2" $
            withBatchEquivalenceFile $ \path -> do
                oneRoot <- checkProgramArgsWithBatchSize Nothing [path]
                batch2 <- checkProgramArgsWithBatchSize (Just "2") [path]

                batch2 `shouldBe` oneRoot
                batch2 `shouldBe` Right "OK\n"

        it "keeps external scheme instantiations root-local in module constraint batches" $ do
            ModuleConstraintResult {mcrRootOwnership = rootOwnership} <-
                requireRight $
                    generateModuleConstraintsWithExternalBindings
                        Set.empty
                        ( Map.singleton
                            "one"
                            ExternalBinding
                                { externalBindingType = intSourceType
                                , externalBindingMode = ExternalBindingScheme
                                }
                        )
                        [ ("value1", externalOneExpr)
                        , ("value2", externalOneExpr)
                        ]
            let root0Nodes = nodesOwnedByRoot 0 rootOwnership
                root1Nodes = nodesOwnedByRoot 1 rootOwnership

            root0Nodes `shouldSatisfy` (not . IntSet.null)
            root1Nodes `shouldSatisfy` (not . IntSet.null)
            root0Nodes `IntSet.intersection` root1Nodes `shouldBe` IntSet.empty
            all ((== 1) . IntSet.size) (IntMap.elems (roiNodeOwners rootOwnership))
                `shouldBe` True

multiModuleSource :: String
multiModuleSource =
    unlines
        [ "module Lib export (one) {"
        , "  def one : Int = 1;"
        , "}"
        , "module Main export (main) {"
        , "  import Lib exposing (one);"
        , "  def main : Int = one;"
        , "}"
        ]

batchEquivalenceSource :: String
batchEquivalenceSource =
    unlines $
        [ "module Helper export (one) {"
        , "  def one : Int = 1;"
        , "}"
        , "module Main export (main) {"
        , "  import Helper exposing (one);"
        ]
            ++ [ "  def value" ++ show index ++ " : Int = one;"
               | index <- [(1 :: Int) .. 150]
               ]
            ++ [ "  def main : Int = value1;"
               , "}"
               ]

intSourceType :: NormSrcType
intSourceType = STBase "Int"

externalOneExpr :: NormSurfaceExpr
externalOneExpr = EAnn (EVar "one") intSourceType

nodesOwnedByRoot :: Int -> RootOwnershipIndex -> IntSet.IntSet
nodesOwnedByRoot rootKey =
    IntMap.keysSet . IntMap.filter (IntSet.member rootKey) . roiNodeOwners

withBatchEquivalenceFile :: (FilePath -> IO a) -> IO a
withBatchEquivalenceFile =
    bracket setup removePathForcibly
  where
    setup = do
        tmpDir <- getTemporaryDirectory
        (path, handle) <- openTempFile tmpDir "batch-equivalence.mlfp"
        hPutStr handle batchEquivalenceSource
        hClose handle
        pure path

checkProgramArgsWithBatchSize ::
    Maybe String ->
    [String] ->
    IO (Either String String)
checkProgramArgsWithBatchSize mbBatchSize args =
    withEnv "MLF_MODULE_DEF_BATCH_SIZE" mbBatchSize $
        checkProgramArgs args

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action =
    bracket (lookupEnv name) restore (const (setRequested >> action))
  where
    setRequested =
        case value of
            Nothing -> unsetEnv name
            Just value0 -> setEnv name value0
    restore oldValue =
        case oldValue of
            Nothing -> unsetEnv name
            Just value0 -> setEnv name value0

requireParsed :: String -> IO P.Program
requireParsed input =
    case parseRawProgram input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program

requireLocatedWithFile :: FilePath -> String -> IO P.LocatedProgram
requireLocatedWithFile path input =
    case parseLocatedProgramWithFile path input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
    case result of
        Left err -> expectationFailure (show err) >> fail "unexpected Left"
        Right value -> pure value
