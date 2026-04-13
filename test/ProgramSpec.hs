module ProgramSpec (spec) where

import MLF.Program
import Test.Hspec

fixturePaths :: [FilePath]
fixturePaths =
    [ "test/programs/recursive-adt/plain-recursive-nat.mlfp"
    , "test/programs/recursive-adt/recursive-gadt.mlfp"
    , "test/programs/recursive-adt/recursive-existential.mlfp"
    , "test/programs/recursive-adt/deriving-eq.mlfp"
    , "test/programs/recursive-adt/typeclass-integration.mlfp"
    , "test/programs/recursive-adt/module-integrated.mlfp"
    ]

spec :: Spec
spec = do
    describe "MLF.Program parse/pretty" $ do
        mapM_ roundtripFixture fixturePaths

    describe "MLF.Program execution corpus" $ do
        mapM_ runFixture fixturePaths

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
  where
    roundtripFixture path =
        it ("roundtrips " ++ path) $ do
            program <- requireParsed =<< readFile path
            parseRawProgram (prettyProgram program) `shouldBe` Right program

    runFixture path =
        it ("runs " ++ path) $ do
            program <- requireParsed =<< readFile path
            (prettyValue <$> runProgram program) `shouldBe` Right "true"

requireParsed :: String -> IO Program
requireParsed input =
    case parseRawProgram input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program
