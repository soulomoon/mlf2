module ProgramSpec (spec) where

import Data.Either (isRight)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import MLF.API (Lit (..), SrcTy (..))
import MLF.Frontend.Program.Check (checkResolvedProgram)
import MLF.Frontend.Program.Elaborate (lowerType, mkElaborateScope)
import MLF.Frontend.Program.Finalize (recoverSourceType, sourceForallMatches)
import MLF.Frontend.Program.Types
    ( ConstructorInfo (..)
    , ConstructorShape (..)
    , DataInfo (..)
    , constructorOwnerRuntimeTypeTrackable
    , mkResolvedSymbol
    )
import MLF.Frontend.Syntax (ResolvedSrcTy (..), mkSrcBound)
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
        "runs overloaded method dispatch with ordinary nullary constructors"
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
                , "  def main : Bool = eq Zero Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs overloaded method dispatch with nested ordinary constructors"
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
                , "  def main : Bool = eq (Succ Zero) (Succ Zero);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
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
        "runs overloaded method dispatch on pattern-bound variable"
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
                , "  def main : Bool = case Zero of {"
                , "    n -> eq n n"
                , "  };"
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
        "runs ordered nested constructor patterns"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool = case Succ (Succ Zero) of {"
                , "    Succ Zero -> false;"
                , "    Succ (Succ n) -> true;"
                , "    _ -> false"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs wildcard fallthrough after constructor patterns"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool = case Zero of {"
                , "    Succ _ -> false;"
                , "    _ -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs pattern annotations"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool = case Succ Zero of {"
                , "    Zero -> false;"
                , "    Succ (n : Nat) -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs exhaustive nested constructor pattern without fallback"
        ( InlineProgram $
            unlines
                [ "module Main export (Unit(..), Wrap(..), main) {"
                , "  data Unit ="
                , "      Unit : Unit;"
                , ""
                , "  data Wrap ="
                , "      Wrap : Unit -> Wrap;"
                , ""
                , "  def main : Bool = case Wrap Unit of {"
                , "    Wrap Unit -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "uses nested pattern annotations to type binders"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), Box(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    deriving Eq;"
                , ""
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  def main : Bool ="
                , "    let useBox = \\box case box of {"
                , "      Box (n : Nat) -> eq n n"
                , "    } in"
                , "    useBox (Box Zero);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects mismatched pattern annotations"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool = case Zero of {"
                , "    (_ : Bool) -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramTypeMismatch (STBase \"Bool\") (STBase \"Nat\")")
    , ProgramMatrixCase
        "rejects nested constructor patterns that violate their annotation"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), Box(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat;"
                , ""
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  def main : Bool ="
                , "    let f = \\x case x of {"
                , "      Box (Zero : Bool) -> true"
                , "    } in"
                , "    f (Box Zero);"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramTypeMismatch (STBase \"Bool\") (STBase \"Nat\")")
    , ProgramMatrixCase
        "rejects catch-all pattern annotations when scrutinee type is inferred later"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool ="
                , "    let f = \\x case x of {"
                , "      (_ : Int) -> true"
                , "    } in"
                , "    f Zero;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramTypeMismatch (STBase \"Int\") (STBase \"Nat\")")
    , ProgramMatrixCase
        "rejects branches after catch-all as unreachable"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool = case Zero of {"
                , "    _ -> true;"
                , "    Zero -> false"
                , "  };"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramDuplicateCaseBranch \"Zero\"")
    , ProgramMatrixCase
        "rejects branches after constructor-local catch-all as unreachable"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool = case Succ Zero of {"
                , "    Succ _ -> true;"
                , "    Succ Zero -> false;"
                , "    _ -> false"
                , "  };"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramDuplicateCaseBranch \"Succ\"")
    , ProgramMatrixCase
        "rejects non-exhaustive nested constructor patterns"
        ( InlineProgram $
            unlines
                [ "module Main export (Nat(..), main) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , ""
                , "  def main : Bool = case Succ Zero of {"
                , "    Zero -> false;"
                , "    Succ (Succ n) -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramNonExhaustiveCase [\"Succ\"]")
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
    , ProgramMatrixCase
        "runs higher-kinded class method over a parameterized data constructor"
        ( InlineProgram $
            unlines
                [ "module Main export (Boxed, Box(..), truthy, main) {"
                , "  class Boxed (f :: * -> *) {"
                , "    truthy : f Bool -> Bool;"
                , "  }"
                , ""
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  instance Boxed Box {"
                , "    truthy = \\box true;"
                , "  }"
                , ""
                , "  def main : Bool = truthy (Box false);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs higher-kinded data field over a concrete constructor head"
        ( InlineProgram $
            unlines
                [ "module Main export (Box(..), Wrap(..), main) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data Wrap (f :: * -> *) a ="
                , "      Wrap : f a -> Wrap f a;"
                , ""
                , "  def main : Bool = case Wrap (Box false) of {"
                , "    Wrap box -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs swapped type parameters through recursive data lowering"
        ( InlineProgram $
            unlines
                [ "module Main export (Pair(..), FlipPair(..), main) {"
                , "  data Pair a b ="
                , "      Pair : a -> b -> Pair a b;"
                , ""
                , "  data FlipPair a b ="
                , "      FlipPair : Pair b a -> FlipPair a b;"
                , ""
                , "  def main : Bool = case (FlipPair (Pair false 1) : FlipPair Int Bool) of {"
                , "    FlipPair pair -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs nullary constructor from mixed higher-kinded data type"
        ( InlineProgram $
            unlines
                [ "module Main export (Box(..), MaybeF(..), main) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data MaybeF (f :: * -> *) a ="
                , "      NothingF : MaybeF f a"
                , "    | JustF : f a -> MaybeF f a;"
                , ""
                , "  def main : Bool = case (NothingF : MaybeF Box Bool) of {"
                , "    NothingF -> true;"
                , "    JustF box -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs first-class nullary constructor from mixed higher-kinded data type"
        ( InlineProgram $
            unlines
                [ "module Main export (Box(..), MaybeF(..), mainValue, main) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data MaybeF (f :: * -> *) a ="
                , "      NothingF : MaybeF f a"
                , "    | JustF : f a -> MaybeF f a;"
                , ""
                , "  def id : forall a. a -> a = \\x x;"
                , "  def mainValue : MaybeF Box Bool = id NothingF;"
                , "  def main : Bool = case mainValue of {"
                , "    NothingF -> true;"
                , "    JustF box -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs value-imported constructor from mixed higher-kinded data type"
        ( InlineProgram $
            unlines
                [ "module Core export (Box(..), MaybeF, NothingF, accept) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data MaybeF (f :: * -> *) a ="
                , "      NothingF : MaybeF f a"
                , "    | JustF : f a -> MaybeF f a;"
                , ""
                , "  def accept : MaybeF Box Bool -> Bool = \\value case value of {"
                , "    NothingF -> true;"
                , "    JustF box -> true"
                , "  };"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core exposing (NothingF, accept);"
                , "  def id : forall a. a -> a = \\x x;"
                , "  def main : Bool = accept (id NothingF);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs value-exported constructor when owner type is not exported"
        ( InlineProgram $
            unlines
                [ "module Core export (Box(..), NothingF, accept) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data MaybeF (f :: * -> *) a ="
                , "      NothingF : MaybeF f a"
                , "    | JustF : f a -> MaybeF f a;"
                , ""
                , "  def accept : MaybeF Box Bool -> Bool = \\value case value of {"
                , "    NothingF -> true;"
                , "    JustF box -> true"
                , "  };"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core exposing (NothingF, accept);"
                , "  def id : forall a. a -> a = \\x x;"
                , "  def main : Bool = accept (id NothingF);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs value-exported constructor from bulk import when owner type is not exported"
        ( InlineProgram $
            unlines
                [ "module Core export (Box(..), NothingF, accept) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data MaybeF (f :: * -> *) a ="
                , "      NothingF : MaybeF f a"
                , "    | JustF : f a -> MaybeF f a;"
                , ""
                , "  def accept : MaybeF Box Bool -> Bool = \\value case value of {"
                , "    NothingF -> true;"
                , "    JustF box -> true"
                , "  };"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core;"
                , "  def id : forall a. a -> a = \\x x;"
                , "  def main : Bool = accept (id NothingF);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs value-exported constructor from aliased bulk import when owner type is not exported"
        ( InlineProgram $
            unlines
                [ "module Core export (Box(..), NothingF, accept) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data MaybeF (f :: * -> *) a ="
                , "      NothingF : MaybeF f a"
                , "    | JustF : f a -> MaybeF f a;"
                , ""
                , "  def accept : MaybeF Box Bool -> Bool = \\value case value of {"
                , "    NothingF -> true;"
                , "    JustF box -> true"
                , "  };"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C;"
                , "  def id : forall a. a -> a = \\x x;"
                , "  def main : Bool = C.accept (id C.NothingF);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs value-exported GADT constructor when owner type is not exported"
        ( InlineProgram $
            unlines
                [ "module Core export (Box(..), NothingBool, accept) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data MaybeF (f :: * -> *) a ="
                , "      NothingBool : MaybeF f Bool"
                , "    | JustF : f a -> MaybeF f a;"
                , ""
                , "  def accept : MaybeF Box Bool -> Bool = \\value case value of {"
                , "    NothingBool -> true;"
                , "    JustF box -> true"
                , "  };"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core exposing (NothingBool, accept);"
                , "  def id : forall a. a -> a = \\x x;"
                , "  def main : Bool = accept (id NothingBool);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs value-imported nonzero-index constructor from mixed higher-kinded data type"
        ( InlineProgram $
            unlines
                [ "module Core export (Box(..), MaybeF, JustF, accept) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data MaybeF (f :: * -> *) a ="
                , "      NothingF : MaybeF f a"
                , "    | JustF : f a -> MaybeF f a;"
                , ""
                , "  def accept : MaybeF Box Bool -> Bool = \\value case value of {"
                , "    NothingF -> false;"
                , "    JustF box -> true"
                , "  };"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core exposing (Box(..), JustF, accept);"
                , "  def id : forall a. a -> a = \\x x;"
                , "  def main : Bool = accept (id (JustF (Box true)));"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "allows local owner type name after value-only mixed constructor import"
        ( InlineProgram $
            unlines
                [ "module Core export (Box(..), MaybeF, NothingF) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  data MaybeF (f :: * -> *) a ="
                , "      NothingF : MaybeF f a"
                , "    | JustF : f a -> MaybeF f a;"
                , "}"
                , ""
                , "module Main export (MaybeF(..), main) {"
                , "  import Core exposing (NothingF);"
                , ""
                , "  data MaybeF a ="
                , "      LocalMaybeF : a -> MaybeF a;"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        ExpectCheckSuccess
    , ProgramMatrixCase
        "allows constructor imports whose hidden owners share a short type name"
        ( InlineProgram $
            unlines
                [ "module LeftCore export (MaybeF, NothingLeft) {"
                , "  data MaybeF ="
                , "      NothingLeft : MaybeF;"
                , "}"
                , ""
                , "module RightCore export (MaybeF, NothingRight) {"
                , "  data MaybeF ="
                , "      NothingRight : MaybeF;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import LeftCore exposing (NothingLeft);"
                , "  import RightCore exposing (NothingRight);"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        ExpectCheckSuccess
    , ProgramMatrixCase
        "rejects first-order parameters in higher-kinded data fields"
        ( InlineProgram $
            unlines
                [ "module Main export (Bad, main) {"
                , "  data Bad (f :: *) ="
                , "      Bad : f Bool -> Bad f;"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramTypeArityMismatch")
    , ProgramMatrixCase
        "GADT indexed constructor application should run through recursive case"
        (ProgramFile "test/programs/recursive-adt/recursive-gadt.mlfp")
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "existential constructor application should run through recursive case"
        (ProgramFile "test/programs/recursive-adt/recursive-existential.mlfp")
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "nullary indexed constructor value should run through case analysis"
        ( InlineProgram $
            unlines
                [ "module Main export (Witness(..), main) {"
                , "  data Witness a ="
                , "      WInt : Witness Int;"
                , ""
                , "  def main : Bool = case WInt of {"
                , "    WInt -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "polymorphic nullary constructor with explicit result type should run"
        ( InlineProgram $
            unlines
                [ "module Main export (Option(..), main) {"
                , "  data Option a ="
                , "      None : Option a"
                , "    | Some : a -> Option a;"
                , ""
                , "  def isNone : Option Bool -> Bool = \\opt true;"
                , "  def main : Bool = isNone (None : Option Bool);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects constructor-local forall with no recoverable evidence"
        ( InlineProgram $
            unlines
                [ "module Main export (Hidden(..), main) {"
                , "  data Hidden ="
                , "      Hidden : forall a. Hidden;"
                , ""
                , "  def main : Bool = let ignore = \\x true in ignore Hidden;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramAmbiguousConstructorUse \"Hidden\"")
    , ProgramMatrixCase
        "runs constrained helper through hidden Eq evidence"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), eq, same, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  def same : Eq a => a -> a -> Bool = \\x \\y eq x y;"
                , "  def main : Bool = same Zero Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects constrained helper alias through local hidden Eq evidence"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, eq, same, alias, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  def same : Eq a => a -> a -> Bool = \\x \\y eq x y;"
                , "  def alias : Eq a => a -> a -> Bool = same;"
                , "  def main : Bool = alias true true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramAmbiguousConstrainedValueUse \"same\"")
    , ProgramMatrixCase
        "runs ground constrained helper alias with resolved evidence"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, eq, sameBool, alias, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  def sameBool : Eq Bool => Bool -> Bool -> Bool = \\x \\y eq x y;"
                , "  def alias : Bool -> Bool -> Bool = sameBool;"
                , "  def main : Bool = alias true true;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs constrained helper after local lambda inference"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, eq, same, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  def same : Eq a => a -> a -> Bool = \\x \\y eq x y;"
                , "  def main : Bool = let f = \\x same x x in f true;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "checks constrained method use from specialized parameterized case binder"
        ( InlineProgram $
            unlines
                [ "module Main export (C, c, Box(..), f, main) {"
                , "  class C a {"
                , "    c : a -> Bool;"
                , "  }"
                , ""
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  def f : C Int => Box Int -> Bool = \\x case x of {"
                , "    Box n -> c n"
                , "  };"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs deferred method with method-level type variable constraint"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Mix, eq, mix, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  class Mix a {"
                , "    mix : Eq b => a -> b -> Bool;"
                , "  }"
                , ""
                , "  instance Mix Bool {"
                , "    mix = \\x \\y eq y y;"
                , "  }"
                , ""
                , "  def main : Bool = mix true true;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs partial deferred method after method-local evidence is fixed by application"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Mix, eq, mix, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  class Mix a {"
                , "    mix : Eq b => a -> b -> Bool;"
                , "  }"
                , ""
                , "  instance Mix Bool {"
                , "    mix = \\x \\y eq y y;"
                , "  }"
                , ""
                , "  def applyBool : (Bool -> Bool) -> Bool = \\f f true;"
                , "  def main : Bool = applyBool (mix true);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs deferred method when only a later forall binder is inferred"
        ( InlineProgram $
            unlines
                [ "module Main export (Pick, pick, main) {"
                , "  class Pick a {"
                , "    pick : forall ghost. forall b. a -> b -> b;"
                , "  }"
                , ""
                , "  instance Pick Bool {"
                , "    pick = let impl : forall ghost. forall b. Bool -> b -> b = \\flag \\value value in impl;"
                , "  }"
                , ""
                , "  def main : Bool = pick true false;"
                , "}"
                ]
        )
        (ExpectRunValue "false")
    , ProgramMatrixCase
        "runs constrained helper with method-local evidence fixed by call args"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Mix, eq, mix, callMix, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  class Mix a {"
                , "    mix : Eq b => a -> b -> Bool;"
                , "  }"
                , ""
                , "  instance Mix Bool {"
                , "    mix = \\x \\y eq y y;"
                , "  }"
                , ""
                , "  def callMix : Mix Bool => Bool -> Bool -> Bool = \\x \\y mix x y;"
                , "  def main : Bool = callMix true true;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs zero-method class constraint with matching instance"
        ( InlineProgram $
            unlines
                [ "module Main export (Marker, needsMarker, main) {"
                , "  class Marker a {"
                , "  }"
                , ""
                , "  instance Marker Bool {"
                , "  }"
                , ""
                , "  def needsMarker : Marker Bool => Bool -> Bool = \\x x;"
                , "  def main : Bool = needsMarker true;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs zero-method class instance after resolving prerequisites"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Marker, Nat(..), eq, needsMarker, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  class Marker a {"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat;"
                , ""
                , "  instance Eq Nat {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  instance Eq a => Marker a {"
                , "  }"
                , ""
                , "  def needsMarker : Marker Nat => Bool -> Bool = \\x x;"
                , "  def main : Bool = needsMarker true;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs deferred method after resolving zero-method prerequisites"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Marker, Uses, Nat(..), eq, uses, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  class Marker a {"
                , "  }"
                , ""
                , "  class Uses a {"
                , "    uses : Marker a => a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat;"
                , ""
                , "  instance Eq Nat {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  instance Eq a => Marker a {"
                , "  }"
                , ""
                , "  instance Uses Nat {"
                , "    uses = \\x true;"
                , "  }"
                , ""
                , "  def main : Bool = uses Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects zero-method class constraint without matching instance"
        ( InlineProgram $
            unlines
                [ "module Main export (Marker, needsMarker, main) {"
                , "  class Marker a {"
                , "  }"
                , ""
                , "  def needsMarker : Marker Bool => Bool -> Bool = \\x x;"
                , "  def main : Bool = needsMarker true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramNoMatchingInstance \"Marker\"")
    , ProgramMatrixCase
        "rejects method signature constraint with unknown class"
        ( InlineProgram $
            unlines
                [ "module Main export (C, main) {"
                , "  class C a {"
                , "    m : Missing a => a -> a;"
                , "  }"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramUnknownClass \"Missing\"")
    , ProgramMatrixCase
        "rejects zero-method instance constraint with unknown class"
        ( InlineProgram $
            unlines
                [ "module Main export (Marker, main) {"
                , "  class Marker a {"
                , "  }"
                , ""
                , "  instance Missing a => Marker a {"
                , "  }"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramUnknownClass \"Missing\"")
    , ProgramMatrixCase
        "rejects zero-method class instance when prerequisite is missing"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Marker, Nat(..), eq, needsMarker, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  class Marker a {"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat;"
                , ""
                , "  instance Eq a => Marker a {"
                , "  }"
                , ""
                , "  def needsMarker : Marker Nat => Bool -> Bool = \\x x;"
                , "  def main : Bool = needsMarker true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramNoMatchingInstance \"Eq\"")
    , ProgramMatrixCase
        "rejects deferred method when zero-method prerequisite is missing"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Marker, Uses, Nat(..), eq, uses, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  class Marker a {"
                , "  }"
                , ""
                , "  class Uses a {"
                , "    uses : Marker a => a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat;"
                , ""
                , "  instance Eq a => Marker a {"
                , "  }"
                , ""
                , "  instance Uses Nat {"
                , "    uses = \\x true;"
                , "  }"
                , ""
                , "  def main : Bool = uses Zero;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramNoMatchingInstance \"Eq\"")
    , ProgramMatrixCase
        "runs qualified zero-method class instance through aliased import"
        ( InlineProgram $
            unlines
                [ "module Core export (Marker) {"
                , "  class Marker a {"
                , "  }"
                , ""
                , "  instance Marker Bool {"
                , "  }"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C;"
                , ""
                , "  def needsMarker : C.Marker Bool => Bool -> Bool = \\x x;"
                , "  def main : Bool = needsMarker true;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs deferred class method with method-level Eq constraint"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, ShowEq, Nat(..), eq, showEq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  class ShowEq a {"
                , "    showEq : Eq a => a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  instance ShowEq Nat {"
                , "    showEq = \\x \\y eq x y;"
                , "  }"
                , ""
                , "  def main : Bool = showEq ((\\x x) Zero) Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs constrained helper through method-level evidence constraints"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, ShowEq, Nat(..), eq, showEq, sameShow, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  class ShowEq a {"
                , "    showEq : Eq a => a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  instance ShowEq Nat {"
                , "    showEq = \\x \\y eq x y;"
                , "  }"
                , ""
                , "  def sameShow : ShowEq a => a -> a -> Bool = \\x \\y showEq x y;"
                , "  def main : Bool = sameShow Zero Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects constrained helper call without a satisfiable instance"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), Box(..), eq, same, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  def same : Eq a => a -> a -> Bool = \\x \\y eq x y;"
                , "  def main : Bool = same (Box Zero) (Box Zero);"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramNoMatchingInstance \"Eq\" (STCon \"Box\"")
    , ProgramMatrixCase
        "runs explicit constrained parameterized Eq instance"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), Option(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  data Option a ="
                , "      None : Option a"
                , "    | Some : a -> Option a;"
                , ""
                , "  instance Eq a => Eq (Option a) {"
                , "    eq = \\left \\right case left of {"
                , "      None -> case right of {"
                , "        None -> true;"
                , "        Some _ -> false"
                , "      };"
                , "      Some l -> case right of {"
                , "        None -> false;"
                , "        Some r -> eq l r"
                , "      }"
                , "    };"
                , "  }"
                , ""
                , "  def main : Bool = eq (Some (Some Zero)) (Some (Some Zero));"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects overlapping parameterized and concrete instance heads"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), Box(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  instance Eq a => Eq (Box a) {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  instance Eq (Box Nat) {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramOverlappingInstance \"Eq\"")
    , ProgramMatrixCase
        "rejects local instance overlapping an imported schema"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, Box(..), eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  instance Eq a => Eq (Box a) {"
                , "    eq = \\left \\right true;"
                , "  }"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core exposing (Eq, Box(..), eq);"
                , ""
                , "  instance Eq (Box Bool) {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramOverlappingInstance \"Eq\"")
    , ProgramMatrixCase
        "rejects overlapping instance heads after alpha-renaming variables"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Pair(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Pair a b ="
                , "      Pair : a -> b -> Pair a b;"
                , ""
                , "  instance Eq (Pair a a) {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  instance Eq (Pair a b) {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramOverlappingInstance \"Eq\"")
    , ProgramMatrixCase
        "rejects alias-equivalent duplicate type instance heads"
        ( InlineProgram $
            unlines
                [ "module Core export (Nat(..)) {"
                , "  data Nat ="
                , "      Zero : Nat;"
                , "}"
                , ""
                , "module Main export (Eq, eq, main) {"
                , "  import Core as A exposing (Nat(..));"
                , ""
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Nat {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  instance Eq A.Nat {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramDuplicateInstance \"Eq\"")
    , ProgramMatrixCase
        "rejects alias-equivalent duplicate class instance heads"
        ( InlineProgram $
            unlines
                [ "module Classes export (Eq, eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Classes exposing (Eq, eq);"
                , "  import Classes as P;"
                , ""
                , "  data Nat ="
                , "      Zero : Nat;"
                , ""
                , "  instance Eq Nat {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  instance P.Eq Nat {"
                , "    eq = \\left \\right false;"
                , "  }"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramDuplicateInstance")
    , ProgramMatrixCase
        "runs parameterized deriving Eq for Option"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), Option(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  data Option a ="
                , "      None : Option a"
                , "    | Some : a -> Option a"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool ="
                , "    eq (Some Zero) (Some Zero);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs parameterized deriving Eq for phantom parameter without Eq constraint"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Phantom(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Phantom a ="
                , "      Phantom : Phantom a"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool ="
                , "    eq (Phantom : Phantom (Bool -> Bool)) (Phantom : Phantom (Bool -> Bool));"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs same-module deriving through pending derived field instances"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, B(..), A(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data B ="
                , "      MkB : B"
                , "    deriving Eq;"
                , ""
                , "  data A ="
                , "      MkA : B -> A"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool = eq (MkA MkB) (MkA MkB);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs deriving Eq when field instance has non-Eq prerequisite"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, Ord, BoxInt(..), eq) {"
                , "  class Eq item {"
                , "    eq : item -> item -> Bool;"
                , "  }"
                , ""
                , "  class Ord item {"
                , "  }"
                , ""
                , "  instance Ord Int {"
                , "  }"
                , ""
                , "  data BoxInt ="
                , "      MkBoxInt : Int -> BoxInt;"
                , ""
                , "  instance Ord Int => Eq BoxInt {"
                , "    eq = \\left \\right true;"
                , "  }"
                , "}"
                , ""
                , "module Main export (UsesBox(..), main) {"
                , "  import Core exposing (Eq, Ord, BoxInt(..), eq);"
                , ""
                , "  data UsesBox ="
                , "      UsesBox : BoxInt -> UsesBox"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool = eq (UsesBox (MkBoxInt 1)) (UsesBox (MkBoxInt 2));"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs parameterized deriving Eq for recursive List"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Nat(..), List(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , ""
                , "  data List a ="
                , "      Nil : List a"
                , "    | Cons : a -> List a -> List a"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool ="
                , "    eq (Cons Zero (Cons (Succ Zero) Nil)) (Cons Zero (Cons (Succ Zero) Nil));"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects parameterized deriving when a field has no Eq evidence"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, Bad(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Bad a ="
                , "      Bad : (a -> a) -> Bad a"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramDerivingMissingFieldInstance \"Eq\"")
    , ProgramMatrixCase
        "rejects parameterized deriving when transitive field constraints lack Eq evidence"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, Option(..), eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Option a ="
                , "      None : Option a"
                , "    | Some : a -> Option a"
                , "    deriving Eq;"
                , "}"
                , ""
                , "module Main export (Bad(..), main) {"
                , "  import Core exposing (Eq, Option(..), eq);"
                , ""
                , "  data Bad a ="
                , "      Bad : Option (a -> a) -> Bad a"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramDerivingMissingFieldInstance \"Eq\"")
    , ProgramMatrixCase
        "rejects non-regular recursive deriving fields"
        ( InlineProgram $
            unlines
                [ "module Main export (Eq, List(..), Weird(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data List a ="
                , "      Nil : List a"
                , "    | Cons : a -> List a -> List a;"
                , ""
                , "  data Weird a ="
                , "      Weird : Weird (List a) -> Weird a"
                , "    deriving Eq;"
                , ""
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramDerivingMissingFieldInstance \"Eq\"")
    , ProgramMatrixCase
        "runs qualified import with alias-only value and constructor access"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, Nat(..), eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C;"
                , "  def main : Bool = C.eq C.Zero C.Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs aliased import with exposed method and qualified constructors"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, Nat(..), eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C exposing (eq);"
                , "  def main : Bool = eq C.Zero C.Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs aliased import exposing a closed-type instance without duplicate matches"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Int {"
                , "    eq = \\x \\y true;"
                , "  }"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C exposing (eq);"
                , "  def main : Bool = eq 1 1;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs aliased import exposing a type without duplicate alias-head instance matches"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, Nat(..), eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C exposing (Nat(..), eq);"
                , "  def main : Bool = eq C.Zero C.Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "deduplicates equivalent instances from mixed unqualified and aliased imports"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, Nat(..), eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat"
                , "    deriving Eq;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core exposing (Eq, Nat(..), eq);"
                , "  import Core as C;"
                , "  def main : Bool = eq C.Zero C.Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "deduplicates constrained imported instances from mixed unqualified and aliased imports"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, Box(..), eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , ""
                , "  instance Eq a => Eq (Box a) {"
                , "    eq = \\left \\right true;"
                , "  }"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core exposing (Eq, Box(..), eq);"
                , "  import Core as C;"
                , "  def main : Bool = eq (C.Box true) (C.Box false);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects same-shape qualified ADTs with distinct source identities"
        ( InlineProgram $
            unlines
                [ "module A export (Token(..), accept) {"
                , "  data Token ="
                , "      Token : Token;"
                , ""
                , "  def accept : Token -> Bool = \\x true;"
                , "}"
                , ""
                , "module B export (Token(..)) {"
                , "  data Token ="
                , "      Token : Token;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import A as A;"
                , "  import B as B;"
                , "  def main : Bool = A.accept B.Token;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramTypeMismatch")
    , ProgramMatrixCase
        "rejects same-shape qualified constructor arguments after result specialization"
        ( InlineProgram $
            unlines
                [ "module A export (Box(..)) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , "}"
                , ""
                , "module B export (Box(..)) {"
                , "  data Box a ="
                , "      Box : a -> Box a;"
                , "}"
                , ""
                , "module Main export (Use(..), main) {"
                , "  import A as A;"
                , "  import B as B;"
                , ""
                , "  data Use a ="
                , "      Use : A.Box a -> Use a;"
                , ""
                , "  def bad : Use Bool = Use (B.Box true);"
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramTypeMismatch")
    , ProgramMatrixCase
        "rejects same-shape polymorphic constructor values after result specialization"
        ( InlineProgram $
            unlines
                [ "module A export (Box(..)) {"
                , "  data Box a ="
                , "      Empty : Box a;"
                , "}"
                , ""
                , "module B export (Box(..)) {"
                , "  data Box a ="
                , "      Empty : Box a;"
                , "}"
                , ""
                , "module Main export (Use(..), main) {"
                , "  import A as A;"
                , "  import B as B;"
                , ""
                , "  data Use a ="
                , "      Use : A.Box a -> Use a;"
                , ""
                , "  def bad : Use Bool = Use B.Empty;"
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramTypeMismatch")
    , ProgramMatrixCase
        "runs local class instance for alias-only imported type"
        ( InlineProgram $
            unlines
                [ "module Core export (Eq, Token(..), eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Token ="
                , "      Token : Token"
                , "    deriving Eq;"
                , "}"
                , ""
                , "module Main export (Eq, eq, main) {"
                , "  import Core as C;"
                , ""
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq C.Token {"
                , "    eq = \\left \\right false;"
                , "  }"
                , ""
                , "  def main : Bool = eq C.Token C.Token;"
                , "}"
                ]
        )
        (ExpectRunValue "false")
    , ProgramMatrixCase
        "runs alias-only qualified deriving Eq"
        ( InlineProgram $
            unlines
                [ "module Classes export (Eq, eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eq = \\x \\y true;"
                , "  }"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Classes as P;"
                , ""
                , "  data Box ="
                , "      Box : Bool -> Box"
                , "    deriving P.Eq;"
                , ""
                , "  def main : Bool = P.eq (Box true) (Box false);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs aliased instance head when class is imported from another module"
        ( InlineProgram $
            unlines
                [ "module Classes export (Eq, eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , "}"
                , ""
                , "module Core export (Foo(..)) {"
                , "  import Classes exposing (Eq, eq);"
                , ""
                , "  data Foo ="
                , "      Foo : Foo"
                , "    deriving Eq;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Classes exposing (Eq, eq);"
                , "  import Core as A;"
                , ""
                , "  def main : Bool = eq A.Foo A.Foo;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects alias-head instance for same-named private class"
        ( InlineProgram $
            unlines
                [ "module Classes export (Eq, eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , "}"
                , ""
                , "module Core export (Token(..)) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Token ="
                , "      Token : Token;"
                , ""
                , "  instance Eq Token {"
                , "    eq = \\left \\right true;"
                , "  }"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Classes exposing (Eq, eq);"
                , "  import Core as C;"
                , ""
                , "  def main : Bool = eq C.Token C.Token;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramNoMatchingInstance \"Eq\" (STBase \"C.Token\")")
    , ProgramMatrixCase
        "rejects unimported prior private instance for same-named local class"
        ( InlineProgram $
            unlines
                [ "module Core export (hidden) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Token ="
                , "      Token : Token;"
                , ""
                , "  instance Eq Token {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  def hidden : Bool = true;"
                , "}"
                , ""
                , "module Main export (Eq, Token(..), eq, main) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data Token ="
                , "      Token : Token;"
                , ""
                , "  def main : Bool = eq Token Token;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramNoMatchingInstance \"Eq\" (STBase \"Token\")")
    , ProgramMatrixCase
        "keeps hidden same-named class instances across method-only imports"
        ( InlineProgram $
            unlines
                [ "module A export (z) {"
                , "  class Eq a {"
                , "    z : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Int {"
                , "    z = \\left \\right true;"
                , "  }"
                , "}"
                , ""
                , "module B export (a) {"
                , "  class Eq a {"
                , "    a : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Int {"
                , "    a = \\left \\right false;"
                , "  }"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import A exposing (z);"
                , "  import B exposing (a);"
                , ""
                , "  def main : Bool = a 1 1;"
                , "}"
                ]
        )
        (ExpectRunValue "false")
    , ProgramMatrixCase
        "resolves method-only import by selected hidden class identity"
        ( InlineProgram $
            unlines
                [ "module A export (eqA) {"
                , "  class Eq a {"
                , "    eqA : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Int {"
                , "    eqA = \\left \\right true;"
                , "  }"
                , "}"
                , ""
                , "module B export (eqB) {"
                , "  class Eq a {"
                , "    eqB : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Bool {"
                , "    eqB = \\left \\right false;"
                , "  }"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import A exposing (eqA);"
                , "  import B exposing (eqB);"
                , ""
                , "  def main : Bool = eqB true false;"
                , "}"
                ]
        )
        (ExpectRunValue "false")
    , ProgramMatrixCase
        "runs local same-named class instance beside hidden method-only imports"
        ( InlineProgram $
            unlines
                [ "module A export (z) {"
                , "  class Eq a {"
                , "    z : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Int {"
                , "    z = \\left \\right true;"
                , "  }"
                , "}"
                , ""
                , "module B export (a) {"
                , "  class Eq a {"
                , "    a : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Int {"
                , "    a = \\left \\right false;"
                , "  }"
                , "}"
                , ""
                , "module Main export (Eq, eq, main) {"
                , "  import A exposing (z);"
                , "  import B exposing (a);"
                , ""
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  instance Eq Int {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  def main : Bool = eq 1 1;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs aliased exposing type without duplicate instance heads"
        ( InlineProgram $
            unlines
                [ "module Classes export (Eq, eq) {"
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , "}"
                , ""
                , "module Core export (Foo(..)) {"
                , "  import Classes exposing (Eq, eq);"
                , ""
                , "  data Foo ="
                , "      Foo : Foo"
                , "    deriving Eq;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Classes exposing (Eq, eq);"
                , "  import Core as A exposing (Foo(..));"
                , ""
                , "  def main : Bool = eq A.Foo A.Foo;"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs qualified type name in annotation"
        ( InlineProgram $
            unlines
                [ "module Core export (Nat(..)) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C;"
                , "  def main : C.Nat = C.Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "Zero")
    , ProgramMatrixCase
        "runs qualified constructor from implicit default export"
        ( InlineProgram $
            unlines
                [ "module Core {"
                , "  data Token ="
                , "      Token : Token;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C;"
                , "  def main : C.Token = C.Token;"
                , "}"
                ]
        )
        (ExpectRunValue "Token")
    , ProgramMatrixCase
        "runs alias-only qualified case over imported ADT"
        ( InlineProgram $
            unlines
                [ "module Core export (Box(..)) {"
                , "  data Box ="
                , "      Box : Box;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C;"
                , "  def main : Bool = case C.Box of {"
                , "    C.Box -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "keeps imported same-name ADT fields distinct from local recursion"
        ( InlineProgram $
            unlines
                [ "module Core export (T(..)) {"
                , "  data T ="
                , "      External : T;"
                , "}"
                , ""
                , "module Main export (T(..), main) {"
                , "  import Core as A;"
                , ""
                , "  data T ="
                , "      Wrap : A.T -> T;"
                , ""
                , "  def main : Bool = case Wrap A.External of {"
                , "    Wrap _ -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs exposed constructor with qualified alias type identity"
        ( InlineProgram $
            unlines
                [ "module Core export (Nat(..)) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C exposing (Nat(..));"
                , "  def main : C.Nat = Zero;"
                , "}"
                ]
        )
        (ExpectRunValue "Zero")
    , ProgramMatrixCase
        "runs mixed exposed and qualified alias constructors in one case"
        ( InlineProgram $
            unlines
                [ "module Core export (Nat(..)) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C exposing (Nat(..));"
                , "  def main : Bool = case C.Succ Zero of {"
                , "    Zero -> false;"
                , "    C.Succ _ -> true"
                , "  };"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "runs distinct instances whose qualified heads would sanitize alike"
        ( InlineProgram $
            unlines
                [ "module Core export (B(..)) {"
                , "  data B ="
                , "      B : Int -> B;"
                , "}"
                , ""
                , "module Main export (Eq, A_B(..), eq, main) {"
                , "  import Core as A;"
                , ""
                , "  class Eq a {"
                , "    eq : a -> a -> Bool;"
                , "  }"
                , ""
                , "  data A_B ="
                , "      A_B : A_B;"
                , ""
                , "  instance Eq A.B {"
                , "    eq = \\left \\right true;"
                , "  }"
                , ""
                , "  instance Eq A_B {"
                , "    eq = \\left \\right false;"
                , "  }"
                , ""
                , "  def main : Bool = eq (A.B 1) (A.B 2);"
                , "}"
                ]
        )
        (ExpectRunValue "true")
    , ProgramMatrixCase
        "rejects qualified access to hidden constructors"
        ( InlineProgram $
            unlines
                [ "module Core export (Nat) {"
                , "  data Nat ="
                , "      Zero : Nat"
                , "    | Succ : Nat -> Nat;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C;"
                , "  def main : C.Nat = C.Zero;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramUnknownValue \"C.Zero\"")
    , ProgramMatrixCase
        "rejects duplicate import aliases in one module"
        ( InlineProgram $
            unlines
                [ "module Core export (main) {"
                , "  def main : Bool = true;"
                , "}"
                , ""
                , "module Other export (main) {"
                , "  def main : Bool = false;"
                , "}"
                , ""
                , "module Main export (main) {"
                , "  import Core as C;"
                , "  import Other as C;"
                , "  def main : Bool = true;"
                , "}"
                ]
        )
        (ExpectCheckFailureContaining "ProgramDuplicateImportAlias \"C\"")
    ]

spec :: Spec
spec = do
    describe "MLF.Program source type finalization" $ do
        it "matches variable-headed applications through forall alpha-renaming" $ do
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

        it "matches repeated substitutions whose forall bounds rename their own binder" $ do
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

        it "recovers higher-kinded data heads with partially applied constructor parameters" $ do
            let typeIdentity =
                    SymbolIdentity
                        { symbolNamespace = SymbolType
                        , symbolDefiningModule = "Main"
                        , symbolDefiningName = "Apply"
                        , symbolOwnerIdentity = Nothing
                        }
                ctorIdentity =
                    SymbolIdentity
                        { symbolNamespace = SymbolConstructor
                        , symbolDefiningModule = "Main"
                        , symbolDefiningName = "Apply"
                        , symbolOwnerIdentity = Just (SymbolOwnerType "Main" "Apply")
                        }
                applyResult = STCon "Apply" (STVar "f" :| [STVar "a"])
                applyCtor =
                    ConstructorInfo
                        { ctorName = "Apply"
                        , ctorInfoSymbol = ctorIdentity
                        , ctorRuntimeName = "$Apply"
                        , ctorType = STArrow (STVarApp "f" (STVar "a" :| [])) applyResult
                        , ctorForalls = []
                        , ctorArgs = [STVarApp "f" (STVar "a" :| [])]
                        , ctorResult = applyResult
                        , ctorOwningType = "Apply"
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = []
                        }
                applyInfo =
                    DataInfo
                        { dataName = "Apply"
                        , dataInfoSymbol = typeIdentity
                        , dataModule = "Main"
                        , dataTypeParams = [TypeParam "f" (KArrow KType KType), firstOrderTypeParam "a"]
                        , dataParams = ["f", "a"]
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

        it "treats owner-shaped variable-headed constructor imports as non-trackable" $ do
            let typeIdentity =
                    SymbolIdentity
                        { symbolNamespace = SymbolType
                        , symbolDefiningModule = "Core"
                        , symbolDefiningName = "MaybeF"
                        , symbolOwnerIdentity = Nothing
                        }
                ctorIdentity name =
                    SymbolIdentity
                        { symbolNamespace = SymbolConstructor
                        , symbolDefiningModule = "Core"
                        , symbolDefiningName = name
                        , symbolOwnerIdentity = Just (SymbolOwnerType "Core" "MaybeF")
                        }
                resultTy = STCon "MaybeF" (STVar "f" :| [STVar "a"])
                ownerTypeParams = [TypeParam "f" (KArrow KType KType), firstOrderTypeParam "a"]
                nothingShape =
                    ConstructorShape
                        { constructorShapeName = "NothingF"
                        , constructorShapeForalls = []
                        , constructorShapeArgs = []
                        , constructorShapeResult = resultTy
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = ownerTypeParams
                        }
                justShape =
                    ConstructorShape
                        { constructorShapeName = "JustF"
                        , constructorShapeForalls = []
                        , constructorShapeArgs = [STVarApp "f" (STVar "a" :| [])]
                        , constructorShapeResult = resultTy
                        , constructorShapeIndex = 1
                        , constructorShapeOwnerTypeParams = ownerTypeParams
                        }
                nothingCtor =
                    ConstructorInfo
                        { ctorName = "NothingF"
                        , ctorInfoSymbol = ctorIdentity "NothingF"
                        , ctorRuntimeName = "Core__NothingF"
                        , ctorType = resultTy
                        , ctorForalls = []
                        , ctorArgs = []
                        , ctorResult = resultTy
                        , ctorOwningType = "MaybeF"
                        , ctorOwningTypeIdentity = typeIdentity
                        , ctorIndex = 0
                        , ctorOwnerConstructors = [nothingShape, justShape]
                        }
            constructorOwnerRuntimeTypeTrackable Map.empty nothingCtor `shouldBe` False

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
                        , "    identity : forall a. a -> a;"
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

        it "parses and pretty-prints variable-headed higher-kinded field types" $ do
            let programText =
                    unlines
                        [ "module Main export (Functor, Higher(..)) {"
                        , "  class Functor (f :: * -> *) {"
                        , "    map : forall a b. (a -> b) -> f a -> f b;"
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

        it "rejects malformed ascii recursive types before variable-headed application fallback" $ do
            let programText =
                    unlines
                        [ "module Main export (main) {"
                        , "  def main : mu a = 1;"
                        , "}"
                        ]
            case parseRawProgram programText of
                Left err -> renderProgramParseError err `shouldSatisfy` (not . null)
                Right program -> expectationFailure ("expected parse error, got: " ++ show program)

    describe "MLF.Program execution corpus" $ do
        mapM_ runFixture fixturePaths

    describe "MLF.Program CLI helper" $ do
        it "runs a frozen sample file by path" $ do
            runProgramFile "test/programs/recursive-adt/plain-recursive-nat.mlfp"
                `shouldReturn` Right "true"

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
        it "reports variable-headed direct AST types as program errors" $ do
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
            checkProgram program `shouldSatisfy` either
                ( \err ->
                    case err of
                        ProgramPipelineError msg ->
                            "variable-headed source type application `f`" `isInfixOf` msg
                        _ -> False
                )
                (const False)

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
                        , "  def main : Bool = case ((\\x x) true) of {"
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

        it "accepts higher-kinded declarations when applications match declared kinds" $ do
            let programText =
                    unlines
                        [ "module Main export (Functor, Lifted, Higher(..), main) {"
                        , "  class Functor (f :: * -> *) {"
                        , "    map : forall a b. (a -> b) -> f a -> f b;"
                        , "  }"
                        , ""
                        , "  class Lifted (f :: * -> *) {"
                        , "    lift : Functor f => forall a. f a -> f a;"
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
                        , "    map : forall a. f a -> f a;"
                        , "  }"
                        , ""
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , ""
                        , "  instance Functor Bool => Eq Int {"
                        , "    eq = \\x \\y true;"
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
                        , "    eq = \\x \\y true;"
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
                        , "    map : forall a. f a -> f a;"
                        , "  }"
                        , ""
                        , "  instance Functor Bool {"
                        , "    map = \\x x;"
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
                        , "  def main : Bool = let ignore = \\x true in ignore None;"
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
                        , "    eq = \\x \\y true;"
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
                            case [defDecl | DeclDef defDecl <- moduleDecls mainModule, defDeclName defDecl == "main"] of
                                [mainDef] -> do
                                    case constrainedBody (defDeclType mainDef) of
                                        RSTBase typeSymbol -> do
                                            symbolDisplayName (resolvedSymbolSpelling typeSymbol) `shouldBe` "C.Token"
                                            symbolDefiningModule (resolvedSymbolIdentity typeSymbol) `shouldBe` "Core"
                                        other -> expectationFailure ("expected resolved type symbol, got " ++ show other)
                                    case defDeclExpr mainDef of
                                        ELet "local" Nothing (EVar (ResolvedGlobalValue answerSymbol)) (ECase (EVar (ResolvedLocalValue "local")) [Alt (PatCtor ctorSymbol []) (EVar (ResolvedLocalValue "local"))]) -> do
                                            symbolDisplayName (resolvedSymbolSpelling answerSymbol) `shouldBe` "C.answer"
                                            symbolDefiningModule (resolvedSymbolIdentity answerSymbol) `shouldBe` "Core"
                                            symbolDisplayName (resolvedSymbolSpelling ctorSymbol) `shouldBe` "C.Token"
                                            symbolDefiningModule (resolvedSymbolIdentity ctorSymbol) `shouldBe` "Core"
                                        other -> expectationFailure ("expected resolved local/global expression shape, got " ++ show other)
                                other -> expectationFailure ("expected one main def, got " ++ show (length other))
                        other -> expectationFailure ("expected one Main module, got " ++ show (length other))
                Left err -> expectationFailure ("expected check success, got " ++ show err)

        it "rejects constructor result heads with matching display but different resolved identity" $ do
            let localBox =
                    mkResolvedSymbol
                        ( SymbolIdentity
                            { symbolNamespace = SymbolType
                            , symbolDefiningModule = "Main"
                            , symbolDefiningName = "Box"
                            , symbolOwnerIdentity = Nothing
                            }
                        )
                        "Box"
                        "Box"
                        (SymbolLocal "Main")
                foreignBox =
                    mkResolvedSymbol
                        ( SymbolIdentity
                            { symbolNamespace = SymbolType
                            , symbolDefiningModule = "Other"
                            , symbolDefiningName = "Box"
                            , symbolOwnerIdentity = Nothing
                            }
                        )
                        "Box"
                        "Box"
                        (SymbolQualifiedImport "Other" "Other")
                badCtor =
                    mkResolvedSymbol
                        ( SymbolIdentity
                            { symbolNamespace = SymbolConstructor
                            , symbolDefiningModule = "Main"
                            , symbolDefiningName = "Bad"
                            , symbolOwnerIdentity = Just (SymbolOwnerType "Main" "Box")
                            }
                        )
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
                        { resolvedModuleName = "Main"
                        , resolvedModuleSyntax =
                            Module
                                { moduleName = "Main"
                                , moduleExports = Nothing
                                , moduleImports = []
                                , moduleDecls =
                                    [ DeclData
                                        DataDecl
                                            { dataDeclName = "Box"
                                            , dataDeclParams = []
                                            , dataDeclConstructors =
                                                [ ConstructorDecl
                                                    { constructorDeclName = "Bad"
                                                    , constructorDeclType = RSTBase foreignBox
                                                    }
                                                ]
                                            , dataDeclDeriving = []
                                            }
                                    ]
                                }
                        , resolvedModuleLocalValues = Map.singleton "Bad" [badCtor]
                        , resolvedModuleLocalTypes = Map.singleton "Box" [localBox]
                        , resolvedModuleLocalClasses = Map.empty
                        , resolvedModuleScope = resolvedScope
                        , resolvedModuleExports = resolvedScope
                        , resolvedModuleReferences = []
                        }
            checkResolvedProgram (ResolvedProgram [resolvedModule])
                `shouldBe` Left (ProgramInvalidConstructorResult "Bad" (STBase "Box") "Box")

        it "checks resolved syntax by identity when display spellings are stale" $ do
            let boxType =
                    mkResolvedSymbol
                        ( SymbolIdentity
                            { symbolNamespace = SymbolType
                            , symbolDefiningModule = "Main"
                            , symbolDefiningName = "Box"
                            , symbolOwnerIdentity = Nothing
                            }
                        )
                        "stale.Box"
                        "stale.Box"
                        (SymbolLocal "Main")
                boxCtor =
                    mkResolvedSymbol
                        ( SymbolIdentity
                            { symbolNamespace = SymbolConstructor
                            , symbolDefiningModule = "Main"
                            , symbolDefiningName = "Box"
                            , symbolOwnerIdentity = Just (SymbolOwnerType "Main" "Box")
                            }
                        )
                        "stale.Box"
                        "stale.Box"
                        (SymbolLocal "Main")
                mainValue =
                    mkResolvedSymbol
                        ( SymbolIdentity
                            { symbolNamespace = SymbolValue
                            , symbolDefiningModule = "Main"
                            , symbolDefiningName = "main"
                            , symbolOwnerIdentity = Nothing
                            }
                        )
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
                        { resolvedModuleName = "Main"
                        , resolvedModuleSyntax =
                            Module
                                { moduleName = "Main"
                                , moduleExports = Nothing
                                , moduleImports = []
                                , moduleDecls =
                                    [ DeclData
                                        DataDecl
                                            { dataDeclName = "Box"
                                            , dataDeclParams = []
                                            , dataDeclConstructors =
                                                [ ConstructorDecl
                                                    { constructorDeclName = "Box"
                                                    , constructorDeclType = RSTBase boxType
                                                    }
                                                ]
                                            , dataDeclDeriving = []
                                            }
                                    , DeclDef
                                        DefDecl
                                            { defDeclName = "main"
                                            , defDeclType = ConstrainedType [] (RSTBase boxType)
                                            , defDeclExpr = EVar (ResolvedGlobalValue boxCtor)
                                            }
                                    ]
                                }
                        , resolvedModuleLocalValues = Map.fromList [("Box", [boxCtor]), ("main", [mainValue])]
                        , resolvedModuleLocalTypes = Map.singleton "Box" [boxType]
                        , resolvedModuleLocalClasses = Map.empty
                        , resolvedModuleScope = resolvedScope
                        , resolvedModuleExports = resolvedScope
                        , resolvedModuleReferences = []
                        }
            checkResolvedProgram (ResolvedProgram [resolvedModule]) `shouldSatisfy` isRight

        it "elaborates resolved annotations and patterns by identity with stale spellings" $ do
            let boxType =
                    mkResolvedSymbol
                        ( SymbolIdentity
                            { symbolNamespace = SymbolType
                            , symbolDefiningModule = "Main"
                            , symbolDefiningName = "Box"
                            , symbolOwnerIdentity = Nothing
                            }
                        )
                        "wrong.Box"
                        "wrong.Box"
                        (SymbolLocal "Main")
                boxCtor =
                    mkResolvedSymbol
                        ( SymbolIdentity
                            { symbolNamespace = SymbolConstructor
                            , symbolDefiningModule = "Main"
                            , symbolDefiningName = "Box"
                            , symbolOwnerIdentity = Just (SymbolOwnerType "Main" "Box")
                            }
                        )
                        "wrong.Box"
                        "wrong.Box"
                        (SymbolLocal "Main")
                mainValue =
                    mkResolvedSymbol
                        ( SymbolIdentity
                            { symbolNamespace = SymbolValue
                            , symbolDefiningModule = "Main"
                            , symbolDefiningName = "main"
                            , symbolOwnerIdentity = Nothing
                            }
                        )
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
                        { resolvedModuleName = "Main"
                        , resolvedModuleSyntax =
                            Module
                                { moduleName = "Main"
                                , moduleExports = Nothing
                                , moduleImports = []
                                , moduleDecls =
                                    [ DeclData
                                        DataDecl
                                            { dataDeclName = "Box"
                                            , dataDeclParams = []
                                            , dataDeclConstructors =
                                                [ ConstructorDecl
                                                    { constructorDeclName = "Box"
                                                    , constructorDeclType = RSTBase boxType
                                                    }
                                                ]
                                            , dataDeclDeriving = []
                                            }
                                    , DeclDef
                                        DefDecl
                                            { defDeclName = "main"
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
                        , resolvedModuleLocalValues = Map.fromList [("Box", [boxCtor]), ("main", [mainValue])]
                        , resolvedModuleLocalTypes = Map.singleton "Box" [boxType]
                        , resolvedModuleLocalClasses = Map.empty
                        , resolvedModuleScope = resolvedScope
                        , resolvedModuleExports = resolvedScope
                        , resolvedModuleReferences = []
                        }
            checkResolvedProgram (ResolvedProgram [resolvedModule]) `shouldSatisfy` isRight

        it "deduplicates mixed unqualified and aliased imports by semantic identity" $ do
            let programText =
                    unlines
                        [ "module Core export (Eq, Token(..), answer, eq) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  data Token ="
                        , "      Token : Token;"
                        , "  instance Eq Token {"
                        , "    eq = \\x \\y true;"
                        , "  }"
                        , "  def answer : Token = Token;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import Core;"
                        , "  import Core as C exposing (Eq, Token(..), answer, eq);"
                        , "  def main : Bool = eq answer C.answer;"
                        , "}"
                        ]
            program <- requireParsed programText
            (prettyValue <$> runProgram program) `shouldBe` Right "true"

        it "resolves alias-only imported instances by semantic head identity" $ do
            let programText =
                    unlines
                        [ "module Core export (Eq, Token(..), eq) {"
                        , "  class Eq a {"
                        , "    eq : a -> a -> Bool;"
                        , "  }"
                        , "  data Token ="
                        , "      Token : Token;"
                        , "  instance Eq Token {"
                        , "    eq = \\x \\y true;"
                        , "  }"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import Core as C;"
                        , "  def main : Bool = C.eq C.Token C.Token;"
                        , "}"
                        ]
            program <- requireParsed programText
            (prettyValue <$> runProgram program) `shouldBe` Right "true"

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
                        , "    eq = \\x \\y true;"
                        , "  }"
                        , "  instance C.Eq C.Token {"
                        , "    eq = \\x \\y true;"
                        , "  }"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            program <- requireParsed programText
            checkProgram program `shouldBe` Left (ProgramDuplicateInstance "Eq" (STBase "Token"))

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
                    diagnosticError diagnostic `shouldBe` ProgramNoMatchingInstance "Eq" (STBase "Nat")
                    diagnosticSpan diagnostic `shouldBe` Nothing
                    renderProgramDiagnostic diagnostic
                        `shouldSatisfy` isInfixOf "error: no matching instance for `Eq STBase \"Nat\"`"
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
                        , "    eq = \\x \\y true;"
                        , "  }"
                        , ""
                        , "  instance Eq Bool {"
                        , "    eq = \\x \\y true;"
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

    describe "MLF.Program runtime value rendering" $ do
        it "renders closed ADT values with source constructor syntax" $ do
            let programText =
                    unlines
                        [ "module Main export (Nat(..), Option(..), main) {"
                        , "  data Nat ="
                        , "      Zero : Nat"
                        , "    | Succ : Nat -> Nat;"
                        , ""
                        , "  data Option a ="
                        , "      None : Option a"
                        , "    | Some : a -> Option a;"
                        , ""
                        , "  def main : Option Nat = Some (Succ Zero);"
                        , "}"
                        ]
            program <- requireParsed programText
            (prettyValue <$> runProgram program) `shouldBe` Right "Some (Succ Zero)"

        it "renders qualified ADT main annotations with source constructor syntax" $ do
            let programText =
                    unlines
                        [ "module Core export (Nat(..), Option(..)) {"
                        , "  data Nat ="
                        , "      Zero : Nat;"
                        , ""
                        , "  data Option a ="
                        , "      None : Option a"
                        , "    | Some : a -> Option a;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import Core as A;"
                        , "  def main : A.Option A.Nat = A.Some A.Zero;"
                        , "}"
                        ]
            program <- requireParsed programText
            (prettyValue <$> runProgram program) `shouldBe` Right "Some Zero"

        it "uses qualified ADT heads to disambiguate runtime decoding" $ do
            let programText =
                    unlines
                        [ "module A export (Bit(..)) {"
                        , "  data Bit ="
                        , "      ABit : Bit;"
                        , "}"
                        , ""
                        , "module B export (Bit(..)) {"
                        , "  data Bit ="
                        , "      BBit : Bit;"
                        , "}"
                        , ""
                        , "module Main export (main) {"
                        , "  import A as L;"
                        , "  import B as R;"
                        , "  def main : R.Bit = R.BBit;"
                        , "}"
                        ]
            program <- requireParsed programText
            (prettyValue <$> runProgram program) `shouldBe` Right "BBit"

        it "does not decode non-data main values through fallback ADT decoding" $ do
            let programText =
                    unlines
                        [ "module Main export (Token(..), main) {"
                        , "  data Token ="
                        , "      Token : Token;"
                        , ""
                        , "  def main : Bool -> Bool = \\x x;"
                        , "}"
                        ]
            program <- requireParsed programText
            case prettyValue <$> runProgram program of
                Right rendered -> rendered `shouldSatisfy` (/= "Token")
                Left err -> expectationFailure ("unexpected program failure: " ++ show err)

        it "does not decode typed non-data constructor fields through fallback ADT decoding" $ do
            let programText =
                    unlines
                        [ "module Main export (Token(..), Holder(..), main) {"
                        , "  data Token ="
                        , "      Token : Token;"
                        , ""
                        , "  data Holder ="
                        , "      Holder : (Bool -> Bool) -> Holder;"
                        , ""
                        , "  def main : Holder = Holder (\\(x : Bool) x);"
                        , "}"
                        ]
            program <- requireParsed programText
            case prettyValue <$> runProgram program of
                Right rendered -> rendered `shouldSatisfy` (/= "Holder Token")
                Left err -> expectationFailure ("unexpected program failure: " ++ show err)

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

requireLocated :: String -> IO LocatedProgram
requireLocated = requireLocatedWithFile "<test>"

requireLocatedWithFile :: FilePath -> String -> IO LocatedProgram
requireLocatedWithFile path input =
    case parseLocatedProgramWithFile path input of
        Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
        Right program -> pure program
