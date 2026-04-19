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
    [ "module Prelude export (Nat(..), Option(..), List(..), Eq, eq, and, id) {",
      "  class Eq a {",
      "    eq : a -> a -> Bool;",
      "  }",
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
