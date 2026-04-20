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
    describe "MLF.Program parse/pretty" $ do
        mapM_ roundtripFixture fixturePaths

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
