module ProgramSpec (spec) where

import Data.Either (isRight)
import Data.List (isInfixOf)
import MLF.API (SrcTy (..))
import MLF.Program
import MLF.Program.CLI (runProgramFile)
import Test.Hspec

fixturePaths :: [FilePath]
fixturePaths =
    [ "test/programs/recursive-adt/plain-recursive-nat.mlfp"
    , "test/programs/recursive-adt/recursive-list-tail.mlfp"
    , "test/programs/recursive-adt/recursive-gadt.mlfp"
    , "test/programs/recursive-adt/recursive-existential.mlfp"
    , "test/programs/recursive-adt/deriving-eq.mlfp"
    , "test/programs/recursive-adt/recursive-tree-deriving.mlfp"
    , "test/programs/recursive-adt/typeclass-integration.mlfp"
    , "test/programs/recursive-adt/abstract-module-use.mlfp"
    , "test/programs/recursive-adt/module-integrated.mlfp"
    ]

unifiedFixtureExpectations :: [(FilePath, String)]
unifiedFixtureExpectations =
    [ ("test/programs/unified/authoritative-let-polymorphism.mlfp", "1")
    , ("test/programs/unified/authoritative-cross-module-let-polymorphism.mlfp", "1")
    , ("test/programs/unified/authoritative-case-analysis.mlfp", "1")
    , ("test/programs/unified/authoritative-overloaded-method.mlfp", "true")
    , ("test/programs/unified/authoritative-recursive-let.mlfp", "true")
    ]

spec :: Spec
spec = do
    describe "MLF.Program parse/pretty" $ do
        mapM_ roundtripFixture fixturePaths

    describe "MLF.Program execution corpus" $ do
        mapM_ runFixture fixturePaths

    describe "MLF.Program CLI helper" $ do
        it "runs a frozen sample file by path" $ do
            runProgramFile "test/programs/recursive-adt/plain-recursive-nat.mlfp"
                `shouldReturn` Right "true"

    describe "MLF.Program diagnostics" $ do
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

        it "chooses the exported main instead of a hidden helper main" $ do
            let programText =
                    unlines
                        [ "module Hidden export () {"
                        , "  def main : Bool = false;"
                        , "}"
                        , ""
                        , "module Visible export (main) {"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            (prettyValue <$> runProgram program) `shouldBe` Right "true"

        it "allows importing a module declared later in the file" $ do
            let programText =
                    unlines
                        [ "module User export (main) {"
                        , "  import Core exposing (Eq, Nat(..), eq);"
                        , "  def main : Bool = eq Zero Zero;"
                        , "}"
                        , ""
                        , "module Core export (Eq, Nat(..), eq) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat"
                        , "    deriving Eq;"
                        , "}"
                        ]
            program <- requireParsed programText
            (prettyValue <$> runProgram program) `shouldBe` Right "true"

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
            checkProgram program `shouldBe` Left (ProgramNoMatchingInstance "Eq" (STBase "Nat"))

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

    describe "MLF.Program performance baseline" $ do
        it "evaluates a recursive Nat equality example at representative depth" $ do
            let depth = (24 :: Int)
                nat n = if n <= 0 then "Zero" else "Succ (" ++ nat (n - 1) ++ ")"
                programText =
                    unlines
                        [ "module Baseline export (Eq, Nat(..), eq, main) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat"
                        , "    deriving Eq;"
                        , ""
                        , "  def main : Bool = eq (" ++ nat depth ++ ") (" ++ nat depth ++ ");"
                        , "}"
                        ]
            program <- requireParsed programText
            (prettyValue <$> runProgram program) `shouldBe` Right "true"

    describe "MLF.Program eMLF-owned `.mlfp` integration" $ do
        mapM_ runUnifiedFixture unifiedFixtureExpectations

        it "fails for a real type mismatch instead of the old infer-lambda gate" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : Int = let id = \\x x in id true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldSatisfy` either
                (\err -> not ("ProgramCannotInferLambda" `isInfixOf` show err))
                (const False)
  where
    roundtripFixture path =
        it ("roundtrips " ++ path) $ do
            program <- requireParsed =<< readFile path
            parseRawProgram (prettyProgram program) `shouldBe` Right program

    runFixture path =
        it ("runs " ++ path) $ do
            program <- requireParsed =<< readFile path
            (prettyValue <$> runProgram program) `shouldBe` Right "true"

    runUnifiedFixture (path, expectedValue) =
        it ("runs " ++ path ++ " through the eMLF-owned `.mlfp` path") $ do
            program <- requireParsed =<< readFile path
            (prettyValue <$> runProgram program) `shouldBe` Right expectedValue

requireParsed :: String -> IO Program
requireParsed input =
    case parseRawProgram input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program
