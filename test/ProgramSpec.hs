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
    , "test/programs/recursive-adt/complex-recursive-program.mlfp"
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
    , ("test/programs/unified/first-class-polymorphism.mlfp", "true")
    ]

data ProgramMatrixSource
    = InlineProgram String
    | ProgramFile FilePath

data ProgramMatrixExpectation
    = ExpectRunValue String
    | ExpectCheckSuccess
    | ExpectCheckFailureContaining String

data ProgramMatrixCase = ProgramMatrixCase
    { matrixCaseName :: String
    , matrixCaseSource :: ProgramMatrixSource
    , matrixCaseExpectation :: ProgramMatrixExpectation
    }

emlfSurfaceParityMatrix :: [ProgramMatrixCase]
emlfSurfaceParityMatrix =
    [ ProgramMatrixCase
        "runs lambda/application"
        ( InlineProgram $
            unlines
                [ "module Main export (main) {"
                , "  def main : Int = (\\x x) 1;"
                , "}"
                ]
        )
        (ExpectRunValue "1")
    , ProgramMatrixCase
        "runs let polymorphism at Int and Bool"
        ( InlineProgram $
            unlines
                [ "module Main export (main) {"
                , "  def main : Bool = let id = \\x x in let keepInt = id 1 in id true;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs typed let annotation"
        ( InlineProgram $
            unlines
                [ "module Main export (main) {"
                , "  def main : Int = let id : forall a. a -> a = \\x x in id 1;"
                , "}"
                ]
        )
        (ExpectRunValue "1")
    , ProgramMatrixCase
        "runs term annotation"
        ( InlineProgram $
            unlines
                [ "module Main export (main) {"
                , "  def main : Int = ((\\x x) : Int -> Int) 1;"
                , "}"
                ]
        )
        (ExpectRunValue "1")
    , ProgramMatrixCase
        "checks annotated rank-2 lambda"
        ( InlineProgram $
            unlines
                [ "module Main export (main) {"
                , "  def main : (forall a. a -> a) -> Bool ="
                , "    \\(poly : forall a. a -> a) let keepInt = poly 1 in poly true;"
                , "}"
                ]
        )
        ExpectCheckSuccess
    , ProgramMatrixCase
        "runs first-class polymorphic top-level argument"
        (ProgramFile "test/programs/unified/first-class-polymorphism.mlfp")
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs first-class polymorphic local argument"
        ( InlineProgram $
            unlines
                [ "module Main export (main) {"
                , "  def main : Bool ="
                , "    let usePoly : (forall a. a -> a) -> Bool ="
                , "      \\(poly : forall a. a -> a) let keepInt = poly 1 in poly true"
                , "    in let id : forall a. a -> a = \\x x in usePoly id;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    ]

emlfBoundaryMatrix :: [ProgramMatrixCase]
emlfBoundaryMatrix =
    [ ProgramMatrixCase
        "runs overloaded method dispatch with lambda/application-inferred argument"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool = eq ((\\x x) Zero) Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs overloaded method dispatch with let-polymorphism-inferred argument"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool = let id = \\x x in eq (id Zero) Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs overloaded method dispatch with explicit argument annotation"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool = eq (((\\x x) Zero) : Nat) Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects bare overloaded method use"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  def main : Bool = eq;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramAmbiguousMethodUse \"eq\"")
    , ProgramMatrixCase
        "rejects deferred overloaded method dispatch with no matching instance"
        ( InlineProgram $
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
                , "  def main : Bool = eq ((\\x x) Zero) Zero;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramNoMatchingInstance \"Eq\" (STBase \"Nat\")")
    , ProgramMatrixCase
        "rejects duplicate instances before deferred overload resolution"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\left \\right false;"
                , "  }"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramDuplicateInstance \"Eq\" (STBase \"Bool\")")
    , ProgramMatrixCase
        "case scrutinee inferred through lambda/application should run"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool = case ((\\x x) Zero) of {"
                , "    Zero -> true;"
                , "    Succ _ -> false"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "case scrutinee inferred through let-polymorphism should run"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool = let id = \\x x in case (id Zero) of {"
                , "    Zero -> true;"
                , "    Succ _ -> false"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "constructor argument inferred as first-class polymorphic value should run"
        ( InlineProgram $
            unlines
                [ "module Main export (PolyBox(..), main) {"
                , "  data PolyBox ="
                , "      PolyBox : (forall a. a -> a) -> PolyBox;"
                , ""
                , "  def main : Bool = case PolyBox (\\x x) of {"
                , "    PolyBox poly -> let keepInt = poly 1 in poly true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "local first-class polymorphic let through constructor boundary should run"
        ( InlineProgram $
            unlines
                [ "module Main export (PolyBox(..), main) {"
                , "  data PolyBox ="
                , "      PolyBox : (forall a. a -> a) -> PolyBox;"
                , ""
                , "  def main : Bool ="
                , "    let id : forall a. a -> a = \\x x"
                , "    in case PolyBox id of {"
                , "      PolyBox poly -> let keepInt = poly 1 in poly true"
                , "    };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "pattern-bound first-class polymorphic variable should run"
        ( InlineProgram $
            unlines
                [ "module Main export (PolyBox(..), main) {"
                , "  data PolyBox ="
                , "      PolyBox : (forall a. a -> a) -> PolyBox;"
                , ""
                , "  def main : Bool ="
                , "    let id : forall a. a -> a = \\x x"
                , "    in case PolyBox id of {"
                , "      PolyBox patternPoly -> let keepInt = patternPoly 1 in patternPoly true"
                , "    };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "partial overloaded method application should run after deferred resolution"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool = let eqZero = eq Zero in eqZero Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "parameterized ADT instance recovery after eMLF inference should run"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), Box(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  instance Eq (Box Nat) {"
                , "    eq = \\(left : Box Nat) \\(right : Box Nat) true;"
                , "  }"
                , ""
                , "  def main : Bool = eq ((\\x x) (Box Zero)) (Box Zero);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
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

    describe "MLF.Program eMLF surface parity matrix" $ do
        mapM_ runProgramMatrixCase emlfSurfaceParityMatrix

    describe "MLF.Program eMLF boundary matrix" $ do
        mapM_ runProgramMatrixCase emlfBoundaryMatrix

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

    runProgramMatrixCase matrixCase =
        it (matrixCaseName matrixCase) $ do
            program <- loadProgramMatrixSource (matrixCaseSource matrixCase)
            case matrixCaseExpectation matrixCase of
                ExpectRunValue expectedValue ->
                    (prettyValue <$> runProgram program) `shouldBe` Right expectedValue
                ExpectCheckSuccess ->
                    checkProgram program `shouldSatisfy` isRight
                ExpectCheckFailureContaining expectedFragment ->
                    checkProgram program `shouldSatisfy` either
                        (isInfixOf expectedFragment . show)
                        (const False)

    loadProgramMatrixSource source =
        case source of
            InlineProgram programText -> requireParsed programText
            ProgramFile path -> requireParsed =<< readFile path

requireParsed :: String -> IO Program
requireParsed input =
    case parseRawProgram input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program
