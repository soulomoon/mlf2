module MLF.Frontend.Program.Prelude
  ( preludeSource,
    preludeProgram,
    preludeLocatedProgram,
    withPrelude,
    withPreludeLocated,
  )
where

import MLF.Frontend.Parse.Program (parseLocatedProgramWithFile)
import qualified MLF.Frontend.Syntax.Program as P

preludeSource :: String
preludeSource =
  unlines
    [ "module Prelude export (Unit(..), IO, Nat(..), Option(..), List(..), Eq, Show, Monad, eq, show, pure, bind, putStrLn, getLine, putStr, readFile, writeFile, appendFile, exitWith, newIORef, readIORef, writeIORef, getArgs, and, id) {",
      "  class Eq a {",
      "    eq : a -> a -> Bool;",
      "  }",
      "",
      "  class Show a {",
      "    show : a -> String;",
      "  }",
      "",
      "  class Monad (m :: * -> *) {",
      "    pure : forall a. a -> m a;",
      "    bind : forall a b. m a -> (a -> m b) -> m b;",
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
      "  instance Monad IO {",
      "    pure = \\value __io_pure value;",
      "    bind = \\action \\next __io_bind action next;",
      "  }",
      "",
      "  instance Show String {",
      "    show = \\x x;",
      "  }",
      "",
      "  instance Show Unit {",
      "    show = \\x \"Unit\";",
      "  }",
      "",
      "  def putStrLn : String -> IO Unit = __io_putStrLn;",
      "  def getLine : IO String = __io_getLine;",
      "  def putStr : String -> IO Unit = __io_putStr;",
      "  def readFile : String -> IO String = __io_readFile;",
      "  def writeFile : String -> String -> IO Unit = __io_writeFile;",
      "  def appendFile : String -> String -> IO Unit = __io_appendFile;",
      "  def exitWith : Int -> IO Unit = __io_exitWith;",
      "  def newIORef : forall a. a -> IO (IORef a) = __io_newIORef;",
      "  def readIORef : forall a. IORef a -> IO a = __io_readIORef;",
      "  def writeIORef : forall a. IORef a -> a -> IO Unit = __io_writeIORef;",
      "  def getArgs : IO (List String) = __io_getArgs;",
      "  def and : Bool -> Bool -> Bool = \\left \\right __mlfp_and left right;",
      "  def id : forall a. a -> a = \\x x;",
      "}"
    ]

preludeLocatedProgram :: P.LocatedProgram
preludeLocatedProgram =
  case parseLocatedProgramWithFile "<mlfp-prelude>" preludeSource of
    Right program -> program
    Left err -> error ("internal Prelude failed to parse: " ++ show err)

preludeProgram :: P.Program
preludeProgram = P.locatedProgram preludeLocatedProgram

withPrelude :: P.Program -> P.Program
withPrelude program =
  P.Program (P.programModules preludeProgram ++ P.programModules program)

withPreludeLocated :: P.LocatedProgram -> P.LocatedProgram
withPreludeLocated program =
  P.LocatedProgram
    { P.locatedProgram = withPrelude (P.locatedProgram program),
      P.locatedProgramSpans =
        P.locatedProgramSpans program
          `P.appendProgramSpanIndex` P.locatedProgramSpans preludeLocatedProgram
    }
