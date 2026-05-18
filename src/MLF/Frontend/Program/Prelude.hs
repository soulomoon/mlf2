module MLF.Frontend.Program.Prelude
  ( preludeSource,
    preludeProgram,
    preludeLocatedProgram,
    withPreludePackage,
    withPreludeLocatedPackage,
    withPrelude,
    withPreludeLocated,
  )
where

import MLF.Frontend.Parse.Program (parseLocatedProgramWithFile)
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage
    , ProgramPackage
    , locatedProgramPackageProgram
    , locatedProgramSourceUnitFromLocated
    , prependLocatedProgramSourceUnit
    , prependProgramSourceUnit
    , programPackageProgram
    , programSourceUnitFromProgram
    , trivialLocatedProgramPackage
    , trivialProgramPackage
    )
import qualified MLF.Frontend.Syntax.Program as P

preludeSource :: String
preludeSource =
  unlines
    [ "module Prelude export (Unit(..), IO, Nat(..), Option(..), List(..), Eq, Show, Functor, Applicative, Monad, eq, show, map, pure, ap, bind, putStrLn, getLine, putStr, readFile, writeFile, appendFile, exitWith, newIORef, readIORef, writeIORef, getArgs, stringLength, stringIsEmpty, stringContainsChar, stringContains, stringStartsWith, stringEndsWith, stringDrop, stringTake, stringSlice, stringCharAt, charIsDigit, charIsAsciiLower, charIsAsciiUpper, charIsAsciiAlpha, and, id) {",
      "  class Eq a {",
      "    eq : a -> a -> Bool;",
      "  }",
      "",
      "  class Show a {",
      "    show : a -> String;",
      "  }",
      "",
      "  class Functor (f :: * -> *) {",
      "    map : ∀ a b. (a -> b) -> f a -> f b;",
      "  }",
      "",
      "  class Functor f => Applicative (f :: * -> *) {",
      "    pure : ∀ a. a -> f a;",
      "    ap : ∀ a b. f (a -> b) -> f a -> f b;",
      "  }",
      "",
      "  class Applicative m => Monad (m :: * -> *) {",
      "    bind : ∀ a b. m a -> (a -> m b) -> m b;",
      "  }",
      "",
      "  data Unit =",
      "      Unit : Unit;",
      "",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat",
      "    deriving Eq;",
      "",
      "  data Option a =",
      "      None : Option a",
      "    | Some : a -> Option a",
      "    deriving Eq;",
      "",
      "  data List a =",
      "      Nil : List a",
      "    | Cons : a -> List a -> List a",
      "    deriving Eq;",
      "",
      "  instance Functor IO {",
      "    map = __io_map;",
      "  }",
      "",
      "  instance Applicative IO {",
      "    pure = __io_pure;",
      "    ap = __io_ap;",
      "  }",
      "",
      "  instance Monad IO {",
      "    bind = __io_bind;",
      "  }",
      "",
      "  instance Show String {",
      "    show = λx x;",
      "  }",
      "",
      "  instance Show Unit {",
      "    show = λx \"Unit\";",
      "  }",
      "",
      "  def putStrLn : String -> IO Unit = __io_putStrLn;",
      "  def getLine : IO String = __io_getLine;",
      "  def putStr : String -> IO Unit = __io_putStr;",
      "  def readFile : String -> IO String = __io_readFile;",
      "  def writeFile : String -> String -> IO Unit = __io_writeFile;",
      "  def appendFile : String -> String -> IO Unit = __io_appendFile;",
      "  def exitWith : Int -> IO Unit = __io_exitWith;",
      "  def newIORef : ∀ a. a -> IO (IORef a) = __io_newIORef;",
      "  def readIORef : ∀ a. IORef a -> IO a = __io_readIORef;",
      "  def writeIORef : ∀ a. IORef a -> a -> IO Unit = __io_writeIORef;",
      "  def getArgs : IO (List String) = __io_getArgs;",
      "  def stringLength : String -> Int = __string_length;",
      "  def stringIsEmpty : String -> Bool = __string_is_empty;",
      "  def stringContainsChar : String -> Char -> Bool = __string_contains_char;",
      "  def stringContains : String -> String -> Bool = __string_contains;",
      "  def stringStartsWith : String -> String -> Bool = __string_starts_with;",
      "  def stringEndsWith : String -> String -> Bool = __string_ends_with;",
      "  def stringDrop : String -> Int -> String = __string_drop;",
      "  def stringTake : String -> Int -> String = __string_take;",
      "  def stringSlice : String -> Int -> Int -> String = __string_slice;",
      "  def stringCharAt : String -> Int -> Char = __string_char_at;",
      "  def charIsDigit : Char -> Bool = __char_is_digit;",
      "  def charIsAsciiLower : Char -> Bool = __char_is_ascii_lower;",
      "  def charIsAsciiUpper : Char -> Bool = __char_is_ascii_upper;",
      "  def charIsAsciiAlpha : Char -> Bool = __char_is_ascii_alpha;",
      "  def and : Bool -> Bool -> Bool = λleft λright __mlfp_and left right;",
      "  def id : ∀ a. a -> a = λx x;",
      "}"
    ]

preludeLocatedProgram :: P.LocatedProgram
preludeLocatedProgram =
  case parseLocatedProgramWithFile "<mlfp-prelude>" preludeSource of
    Right program -> program
    Left err -> error ("internal Prelude failed to parse: " ++ show err)

preludeProgram :: P.Program
preludeProgram = P.locatedProgram preludeLocatedProgram

withPreludePackage :: ProgramPackage -> ProgramPackage
withPreludePackage =
  prependProgramSourceUnit (programSourceUnitFromProgram preludeProgram)

withPreludeLocatedPackage :: LocatedProgramPackage -> LocatedProgramPackage
withPreludeLocatedPackage =
  prependLocatedProgramSourceUnit (locatedProgramSourceUnitFromLocated preludeLocatedProgram)

withPrelude :: P.Program -> P.Program
withPrelude program =
  programPackageProgram (withPreludePackage (trivialProgramPackage program))

withPreludeLocated :: P.LocatedProgram -> P.LocatedProgram
withPreludeLocated program =
  locatedProgramPackageProgram (withPreludeLocatedPackage (trivialLocatedProgramPackage program))
