module ProgramSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import MLF.API (Lit (..), SrcTy (..))
import MLF.Frontend.Program.Check (checkResolvedProgram)
import MLF.Frontend.Program.Elaborate (lowerType, mkElaborateScope)
import MLF.Frontend.Program.Finalize
    ( recoverSourceType
    , sourceForallMatches
    , stripVacuousForallsAndTypeAbs
    )
import MLF.Frontend.Program.Types
    ( ConstructorInfo (..)
    , ConstructorShape (..)
    , DataInfo (..)
    , constructorOwnerRuntimeTypeTrackable
    , mkResolvedSymbol
    )
import MLF.Frontend.Syntax (ResolvedSrcTy (..), mkSrcBound)
import MLF.Program
import qualified MLF.Types.Elab as Elab
import MLF.Program.CLI (runProgramFile)
import Test.Hspec

import Parity.ProgramMatrix

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

        it "keeps vacuous foralls when matching type abstractions still carry instantiations" $ do
            let ty = Elab.TForall "a" Nothing (Elab.TVar "result")
                retainedTerm =
                    Elab.ETyAbs
                        "a"
                        Nothing
                        (Elab.ETyInst (Elab.EVar "poly") (Elab.InstApp (Elab.TVar "a")))
                strippedTerm = Elab.ETyAbs "a" Nothing (Elab.EVar "value")
            stripVacuousForallsAndTypeAbs ty retainedTerm `shouldBe` (ty, retainedTerm)
            stripVacuousForallsAndTypeAbs ty strippedTerm `shouldBe` (Elab.TVar "result", Elab.EVar "value")

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
                        , constructorShapeSymbol = ctorIdentity "NothingF"
                        , constructorShapeRuntimeName = "Core__NothingF"
                        , constructorShapeForalls = []
                        , constructorShapeArgs = []
                        , constructorShapeResult = resultTy
                        , constructorShapeIndex = 0
                        , constructorShapeOwnerTypeParams = ownerTypeParams
                        }
                justShape =
                    ConstructorShape
                        { constructorShapeName = "JustF"
                        , constructorShapeSymbol = ctorIdentity "JustF"
                        , constructorShapeRuntimeName = "Core__JustF"
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

    describe "MLF.Program shared runtime-success parity surface" $ do
        mapM_ runProgramRuntimeCase programRuntimeSuccessCases

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

        it "typechecks the initial Prelude IO and Monad surface" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (pureUnit, main) {"
                        , "  import Prelude exposing (Unit(..), IO, Monad, pure, bind, putStrLn);"
                        , "  def pureUnit : IO Unit = pure Unit;"
                        , "  def after : Unit -> IO Unit = \\_done putStrLn \"world\";"
                        , "  def main : IO Unit = bind (putStrLn \"hello\") after;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` isRight

        it "typechecks direct IO bind primitive uses with consistent arguments" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_bind (__io_pure Unit) (\\(_n : Unit) __io_putStrLn \"world\");"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` isRight

        it "rejects inconsistent direct IO bind primitive arguments" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = __io_bind (__io_pure 1) (\\(_n : Unit) __io_putStrLn \"world\");"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` isLeft

        it "rejects non-IO expressions for Prelude IO annotations" $ do
            intLocated <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = 1;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated intLocated) `shouldSatisfy` either
                (isInfixOf "type mismatch" . renderProgramDiagnostic)
                (const False)
            identityLocated <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : IO Unit = \\x x;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated identityLocated) `shouldSatisfy` either
                (isInfixOf "type mismatch" . renderProgramDiagnostic)
                (const False)

        it "rejects monomorphic IO actions for polymorphic IO annotations" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (IO);"
                        , "  def main : forall a. IO a = __io_pure 1;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                (isInfixOf "type mismatch" . renderProgramDiagnostic)
                (const False)

        it "rejects inconsistent Prelude IO bind argument substitutions" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, bind, putStrLn);"
                        , "  def main : IO Unit = bind (__io_pure 1) (\\(_n : Unit) putStrLn \"world\");"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                (isInfixOf "ambiguous overloaded method use `bind`" . renderProgramDiagnostic)
                (const False)

        it "rejects constructor imports for opaque Prelude IO" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (IO(..));"
                        , "  def main : Bool = true;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ((== ProgramImportNotExported "Prelude" "IO") . diagnosticError)
                (const False)

        it "rejects case inspection of opaque Prelude IO values" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, pure);"
                        , "  def action : IO Unit = pure Unit;"
                        , "  def main : Unit = case action of {"
                        , "    _ -> Unit"
                        , "  };"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                (isInfixOf "case scrutinee is not a data type" . renderProgramDiagnostic)
                (const False)

        it "keeps overloaded pure ambiguous without an IO expected result" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), pure);"
                        , "  def main : Unit = pure Unit;"
                        , "}"
                        ]
            checkLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ((== ProgramAmbiguousMethodUse "pure") . diagnosticError)
                (const False)

        it "rejects running IO mains until runtime support exists" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, putStrLn);"
                        , "  def main : IO Unit = putStrLn \"hello\";"
                        , "}"
                        ]
            runLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ((== ProgramPipelineError "run-program does not support IO main values yet") . diagnosticError)
                (const False)

        it "rejects running pure mains that depend on opaque Prelude helpers" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO, pure);"
                        , "  def discard : IO Unit -> Unit = \\(_action : IO Unit) Unit;"
                        , "  def main : Unit = discard (pure Unit);"
                        , "}"
                        ]
            runLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ( \diagnostic ->
                    all
                        (`isInfixOf` renderProgramDiagnostic diagnostic)
                        [ "run-program does not support IO dependencies yet"
                        , "Main__discard"
                        ]
                )
                (const False)

        it "rejects running pure mains that directly call opaque primitives" $ do
            located <-
                requireLocated $
                    unlines
                        [ "module Main export (main) {"
                        , "  import Prelude exposing (Unit(..), IO);"
                        , "  def main : Unit = (\\(_action : IO Unit) Unit) (__io_pure Unit);"
                        , "}"
                        ]
            runLocatedProgram (withPreludeLocated located) `shouldSatisfy` either
                ( \diagnostic ->
                    all
                        (`isInfixOf` renderProgramDiagnostic diagnostic)
                        [ "run-program does not support IO dependencies yet"
                        , "__io_pure"
                        ]
                )
                (const False)

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

        it "accepts the higher-kinded language-reference examples" $ do
            let programText =
                    unlines
                        [ "module Main export (Functor, Monad, Profunctor, Box(..), Wrap(..), WrappedP(..), MaybeF(..), main) {"
                        , "  class Functor (f :: * -> *) {"
                        , "    map : forall a b. (a -> b) -> f a -> f b;"
                        , "  }"
                        , ""
                        , "  class Monad (m :: * -> *) {"
                        , "    bind : forall a b. m a -> (a -> m b) -> m b;"
                        , "  }"
                        , ""
                        , "  class Profunctor (p :: * -> * -> *) {"
                        , "    dimap : forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d;"
                        , "  }"
                        , ""
                        , "  data Box a ="
                        , "      Box : a -> Box a;"
                        , ""
                        , "  data Wrap (f :: * -> *) a ="
                        , "      Wrap : f a -> Wrap f a;"
                        , ""
                        , "  data WrappedP (p :: * -> * -> *) a b ="
                        , "      WrappedP : p a b -> WrappedP p a b;"
                        , ""
                        , "  data MaybeF (f :: * -> *) a ="
                        , "      NothingF : MaybeF f a"
                        , "    | JustF : f a -> MaybeF f a;"
                        , ""
                        , "  class Boxed (f :: * -> *) {"
                        , "    truthy : f Bool -> Bool;"
                        , "  }"
                        , ""
                        , "  instance Boxed Box {"
                        , "    truthy = \\box true;"
                        , "  }"
                        , ""
                        , "  class Uses marker {"
                        , "    use : (Boxed f, Functor f) => marker -> marker;"
                        , "  }"
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

    describe "MLF.Program eMLF surface parity matrix" $ do
        mapM_ runProgramMatrixCase (nonRuntimeProgramMatrixCases emlfSurfaceParityMatrix)

    describe "MLF.Program eMLF boundary matrix" $ do
        mapM_ runProgramMatrixCase (nonRuntimeProgramMatrixCases emlfBoundaryMatrix)

    describe "MLF.Program eMLF-owned `.mlfp` integration" $ do
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

        it "does not rewrite same-shaped ADT instantiations to another nominal head" $ do
            let programText =
                    unlines
                        [ "module Main export (A(..), B(..), keep, main) {"
                        , "  data A ="
                        , "      AZ : A;"
                        , ""
                        , "  data B ="
                        , "      BZ : B;"
                        , ""
                        , "  def keep : forall a. a -> a = \\x x;"
                        , "  def main : B = keep BZ;"
                        , "}"
                        ]
            program <- requireParsed programText
            checked <- requireChecked program
            mainBinding <- requireCheckedBinding "Main__main" checked
            show (checkedBindingTerm mainBinding) `shouldNotSatisfy` isInfixOf "Main.A"
            (prettyValue <$> runProgram program) `shouldBe` Right "BZ"
  where
    roundtripFixture path =
        it ("roundtrips " ++ path) $ do
            program <- requireParsed =<< readFile path
            parseRawProgram (prettyProgram program) `shouldBe` Right program

    runProgramRuntimeCase runtimeCase =
        it (runtimeCaseName runtimeCase) $ do
            program <- loadProgramMatrixSource (runtimeCaseSource runtimeCase)
            let result = prettyValue <$> runProgram program
            case runtimeCaseExpectation runtimeCase of
                ExpectRuntimeValue expectedValue ->
                    result `shouldBe` Right expectedValue
                ExpectRuntimePredicate label predicate ->
                    case result of
                        Right rendered
                            | predicate rendered -> pure ()
                            | otherwise ->
                                expectationFailure $
                                    "expected "
                                        ++ label
                                        ++ ", got: "
                                        ++ rendered
                        Left err ->
                            expectationFailure ("unexpected program failure: " ++ show err)

    nonRuntimeProgramMatrixCases =
        filter (not . isRuntimeProgramMatrixCase)

    isRuntimeProgramMatrixCase matrixCase =
        case matrixCaseExpectation matrixCase of
            ExpectRunValue _ -> True
            ExpectCheckSuccess -> False
            ExpectCheckFailureContaining _ -> False

    runProgramMatrixCase matrixCase =
        it (matrixCaseName matrixCase) $ do
            program <- loadProgramMatrixSource (matrixCaseSource matrixCase)
            case matrixCaseExpectation matrixCase of
                ExpectRunValue _ ->
                    expectationFailure "runtime-success rows are covered by programRuntimeSuccessCases"
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

    requireChecked program =
        case checkProgram program of
            Left err -> expectationFailure ("check failed: " ++ show err) >> fail "check failed"
            Right checked -> pure checked

    requireCheckedBinding name checked =
        case
            [ binding
            | checkedModule <- checkedProgramModules checked
            , binding <- checkedModuleBindings checkedModule
            , checkedBindingName binding == name
            ]
        of
            binding : _ -> pure binding
            [] -> expectationFailure ("missing checked binding: " ++ name) >> fail "missing checked binding"

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
