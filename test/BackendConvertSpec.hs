module BackendConvertSpec (spec) where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List (find, intercalate, isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import MLF.Backend.Convert
import MLF.Backend.IR
import MLF.Constraint.Types.Graph (BaseTy (..))
import qualified MLF.Elab.Types as Elab
import MLF.Frontend.Program.Types (ConstructorInfo (..), DataInfo (..))
import MLF.Frontend.Syntax (Lit (..), SrcTy (..), SrcType)
import MLF.Program
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import Test.Hspec

spec :: Spec
spec = describe "MLF.Backend.Convert" $ do
  it "converts a checked function program to validated backend IR" $ do
    checked <- requireChecked simpleFunctionProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendProgramMain backend `shouldBe` "Main__main"

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding `shouldBe` intTy
    backendBindingExportedAsMain mainBinding `shouldBe` True
    case backendBindingExpr mainBinding of
      BackendApp
        { backendExprType = resultTy,
          backendFunction = BackendVar {backendVarName = "Main__id"},
          backendArgument = BackendLit {backendLit = LInt 1}
        } ->
          resultTy `shouldBe` intTy
      other -> expectationFailure ("expected backend application, got " ++ show other)

  it "matches the checked backend IR snapshot for a primitive function program" $ do
    checked <- requireChecked simpleFunctionProgram
    backend <- requireRight (convertCheckedProgram checked)

    backendIRGolden "test/golden/backend-ir-simple-function.golden" backend

  it "recovers explicit backend constructors and cases from checked ADT paths" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendDataNames backend `shouldSatisfy` (not . null)
    backendConstructorNames backend `shouldSatisfy` (not . null)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase
    collectConstructNames (backendBindingExpr mainBinding) `shouldSatisfy` (not . null)

  it "matches the checked backend IR snapshot for a simple ADT case program" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)

    backendIRGolden "test/golden/backend-ir-adt-case.golden" backend

  it "recovers backend cases when the result type differs from the scrutinee ADT" $ do
    checked <- requireChecked intCaseProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case findBackendCase (backendBindingExpr mainBinding) of
      Just BackendCase {backendExprType = resultTy, backendScrutinee = scrutinee} -> do
        resultTy `shouldBe` intTy
        backendExprType scrutinee `shouldSatisfy` (/= intTy)
      Just other -> expectationFailure ("expected backend case, got " ++ show other)
      Nothing -> expectationFailure "expected backend case"

  it "falls back to ordinary application for over-applied case-shaped terms" $ do
    checked0 <- requireChecked functionCaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = intElabTy,
                    checkedBindingTerm = Elab.EApp (checkedBindingTerm binding) (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingExpr mainBinding of
      BackendApp
        { backendExprType = resultTy,
          backendFunction = fun,
          backendArgument = BackendLit {backendLit = LInt 1}
        } -> do
          resultTy `shouldBe` intTy
          fun `shouldSatisfy` containsBackendCase
      other -> expectationFailure ("expected backend application of recovered case, got " ++ show other)

  it "recovers backend cases with type-wrapped handler lambdas" $ do
    checked0 <- requireChecked intCaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding {checkedBindingTerm = wrapCaseHandlersWithTypeWrappers (checkedBindingTerm binding)}
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase

  it "keeps type wrappers that belong to case handler bodies" $ do
    checked0 <- requireChecked functionCaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding {checkedBindingTerm = replaceCaseHandlerBodiesAfterLams 1 instantiatedIntIdentity (checkedBindingTerm binding)}
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendTyApp

  it "accepts alpha-equivalent case handler result types" $ do
    checked0 <- requireChecked functionCaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = polymorphicIdentityElabTy,
                    checkedBindingTerm = replaceCaseHandlerBodiesAfterLams 1 alphaEquivalentIdentityInstId (checkedBindingTerm binding)
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "preserves expected checked binding types through ordinary fallback" $ do
    checked <- requireChecked =<< readFile "test/programs/recursive-adt/abstract-module-use.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "renames expected forall bodies to actual type abstraction binders" $ do
    checked <- requireChecked =<< readFile "test/programs/unified/first-class-polymorphism.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "synthesizes constructor bindings for checked GADT and existential programs" $ do
    mapM_
      ( \path -> do
          checked <- requireChecked =<< readFile path
          backend <- requireRight (convertCheckedProgram checked)
          validateBackendProgram backend `shouldBe` Right ()
      )
      [ "test/programs/recursive-adt/recursive-gadt.mlfp",
        "test/programs/recursive-adt/recursive-existential.mlfp"
      ]

  it "instantiates parameterized constructor fields from the result type" $ do
    checked <- requireChecked parameterizedConstructorProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Some"]

  it "preserves constructor type applications when checking constructor fields" $ do
    checked <- requireChecked constructorForallApplicationProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Pack"]

  it "preserves bounded constructor foralls in backend metadata" $ do
    checked <- requireChecked boundedConstructorForallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    constructor <- requireConstructor "Main__Pack" backend
    backendConstructorForalls constructor `shouldBe` [BackendTypeBinder "a" (Just intTy)]

    let corruptedExpr =
          BackendConstruct
            { backendExprType = backendConstructorResult constructor,
              backendConstructName = backendConstructorName constructor,
              backendConstructArgs = [BackendLit boolTy (LBool True)]
            }
        corruptedBackend =
          mapBackendMainBinding
            ( \binding ->
                binding
                  { backendBindingExpr = corruptedExpr,
                    backendBindingType = backendConstructorResult constructor
                  }
            )
            backend

    validateBackendProgram corruptedBackend
      `shouldBe` Left (BackendConstructorArgumentMismatch "Main__Pack" 0 intTy boolTy)

  it "matches bounded constructor foralls against type variables with equivalent bounds" $ do
    checked0 <- requireChecked boundedConstructorForallProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = boundedWrapElabTy (checkedBindingType binding),
                    checkedBindingTerm = boundedWrapTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding
      `shouldSatisfy` containsConstructArgType "Main__Pack" (BTVar "b")

  it "matches bounded constructor foralls through dependent type-variable bounds" $ do
    checked0 <- requireChecked dependentBoundedConstructorForallProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = dependentBoundedWrapElabTy (checkedBindingType binding),
                    checkedBindingTerm = dependentBoundedWrapTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding
      `shouldSatisfy` containsConstructArgType "Main__Pack" (BTVar "b")

  it "converts nested constructor arguments under expected constructor field types" $ do
    checked <- requireChecked =<< readFile "test/programs/recursive-adt/typeclass-integration.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "converts hidden Eq evidence for constrained helpers" $ do
    checked <- requireChecked hiddenEqEvidenceProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "converts constrained parameterized Eq evidence without ambiguous ADT recovery" $ do
    checked <- requireChecked parameterizedEqEvidenceProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    map backendBindingName (backendBindings backend) `shouldSatisfy` any (isInfixOf "Eq__Option")

  it "lifts recursive parameterized deriving Eq helpers with captured evidence" $ do
    checked <- requireChecked recursiveListDerivingEqProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    map backendBindingName (backendBindings backend) `shouldSatisfy` any (isInfixOf "$letrec$")

  it "ignores stale constructor head type instantiations" $ do
    checked0 <- requireChecked constructorForallApplicationProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      addStaleConstructorHeadInstantiation
                        "Main__Pack"
                        boolElabTy
                        (checkedBindingTerm binding)
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Pack"]

  it "matches repeated constructor parameters modulo alpha-equivalence" $ do
    checked0 <- requireChecked repeatedPolymorphicParameterProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = boolElabTy,
                    checkedBindingTerm = repeatedPolymorphicParameterCaseTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "rejects mismatched vacuous recursive constructor fallback results during conversion" $ do
    checked0 <- requireChecked vacuousRecursiveConstructorFallbackProgram
    let checked =
          withConstructorResult "Main__MkBox" (STMu "a" (STBase "Int")) $
            mapMainBinding
              ( \binding ->
                  binding {checkedBindingType = Elab.TMu "b" boolElabTy}
              )
              checked0

    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) ->
        message `shouldSatisfy` isInfixOf "constructor result type does not match expected result"
      Left err ->
        expectationFailure ("expected constructor shape rejection, got " ++ show err)
      Right backend ->
        expectationFailure ("expected constructor shape rejection, got backend:\n" ++ show backend)

  it "keeps same-name data declarations module-scoped during type lowering" $ do
    checked <- requireChecked duplicateDataNameProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendDataNames backend `shouldContain` ["A.T", "B.T"]

    aBinding <- requireBinding "A__A" backend
    aConstructor <- requireConstructor "A__A" backend
    backendConstructorResult aConstructor `shouldBe` backendBindingType aBinding

  it "canonicalizes same-name data heads when module names sort after type names" $ do
    checked <- requireChecked qualifiedAliasOrderingProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendDataNames backend `shouldContain` ["Y.T", "Z.T"]

    yConstructor <- requireConstructor "Y__YValue" backend
    zConstructor <- requireConstructor "Z__ZValue" backend
    backendConstructorResult yConstructor `shouldSatisfy` (/= backendConstructorResult zConstructor)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase

  it "treats stale app-like instantiations on non-forall terms as no-ops" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm = Elab.ETyInst (Elab.ELit (LInt 1)) (Elab.InstApp intElabTy),
                    checkedBindingType = intElabTy
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldBe` BackendLit intTy (LInt 1)

  it "promotes closed recursive local lets to backend helper bindings" $ do
    checked <- requireChecked =<< readFile "test/programs/unified/authoritative-recursive-let.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    helper <-
      case filter (isInfixOf "$letrec$" . backendBindingName) (backendBindings backend) of
        [helper] -> pure helper
        helpers -> expectationFailure ("expected one lifted helper, got " ++ show (map backendBindingName helpers)) >> fail "helper mismatch"
    backendBindingType helper `shouldSatisfy` isBackendFunctionType
    backendBindingExpr helper `shouldSatisfy` containsBackendCase
    backendBindingExpr helper `shouldSatisfy` containsBackendVar (backendBindingName helper)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendVar (backendBindingName helper)

  it "renames binders that would capture recursive helper evidence" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveCaptureAvoidingElabTy,
                    checkedBindingTerm = recursiveCaptureAvoidingTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    length (filter (== "$evidence_E") (backendExprBinders (backendBindingExpr helper))) `shouldBe` 1

  it "keeps renamed let binders out of their own right-hand sides" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveLetRhsRenameElabTy,
                    checkedBindingTerm = recursiveLetRhsRenameTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingExpr helper `shouldNotSatisfy` containsSelfReferentialLetRhs

  it "captures type variables used only by recursive RHS instantiations" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveTypeCaptureElabTy,
                    checkedBindingTerm = recursiveTypeCaptureTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingType helper `shouldBe` BTForall "a" Nothing unaryIntBackendTy
    backendBindingExpr helper `shouldSatisfy` containsBackendTyAppArgument (BTVar "a")

  it "keeps type abstraction bounds outside freshened binder scope" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveTypeBoundScopeElabTy,
                    checkedBindingTerm = recursiveTypeBoundScopeTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingType helper `shouldBe` BTForall "a" Nothing unaryIntBackendTy
    backendBindingExpr helper `shouldSatisfy` containsFreshenedTypeAbsWithOuterBound

  it "leaves type abstraction bounds unchanged under shadowing type binders" $ do
    source <- readFile "src/MLF/Backend/Convert.hs"

    source `shouldSatisfy` isInfixOf "| name == old ->\n              ETyAbs name mbBound body"

  it "lifts recursive lets that shadow outer term binders" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveShadowedLetElabTy,
                    checkedBindingTerm = recursiveShadowedLetTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingType helper `shouldBe` unaryIntBackendTy
    backendBindingExpr helper `shouldSatisfy` containsBackendVar (backendBindingName helper)

  it "preserves lexical type binder order when lifting recursive helper captures" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveLexicalTypeOrderElabTy,
                    checkedBindingTerm = recursiveLexicalTypeOrderTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingType helper `shouldBe` recursiveLexicalTypeOrderHelperBackendTy

  it "rejects recursive local functions that capture lexical values" $ do
    checked <- requireChecked recursiveLetCaptureProgram

    case convertCheckedProgram checked of
      Left (BackendUnsupportedRecursiveLet detail) ->
        detail `shouldSatisfy` isInfixOf "captures lexical bindings"
      other ->
        expectationFailure ("expected recursive-let capture rejection, got " ++ show other)

  it "rejects nested recursive local functions that capture outer recursive functions" $ do
    checked <- requireChecked nestedRecursiveLetCaptureProgram

    case convertCheckedProgram checked of
      Left (BackendUnsupportedRecursiveLet detail) -> do
        detail `shouldSatisfy` isInfixOf "captures lexical bindings"
        detail `shouldSatisfy` isInfixOf "peel"
      other ->
        expectationFailure ("expected nested recursive-let capture rejection, got " ++ show other)

  it "reports unsupported source type applications instead of weakening them" $ do
    convertSourceType unsupportedVariableHeadType
      `shouldBe` Left (BackendUnsupportedSourceType unsupportedVariableHeadType)

simpleFunctionProgram :: String
simpleFunctionProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : Int -> Int = \\x x;",
      "  def main : Int = id 1;",
      "}"
    ]

recursiveLetCaptureProgram :: String
recursiveLetCaptureProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Nat -> Nat = \\(seed : Nat)",
      "    let peel : Nat -> Nat = \\(n : Nat) case n of {",
      "      Zero -> seed;",
      "      Succ inner -> peel inner",
      "    } in peel seed;",
      "}"
    ]

nestedRecursiveLetCaptureProgram :: String
nestedRecursiveLetCaptureProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Nat -> Nat =",
      "    let peel : Nat -> Nat = \\(n : Nat)",
      "      let bounce : Nat -> Nat = \\(m : Nat) case m of {",
      "        Zero -> peel Zero;",
      "        Succ inner -> bounce inner",
      "      } in case n of {",
      "        Zero -> Zero;",
      "        Succ inner -> bounce inner",
      "      }",
      "    in peel;",
      "}"
    ]

adtCaseProgram :: String
adtCaseProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Nat = case Succ Zero of {",
      "    Zero -> Zero;",
      "    Succ n -> n",
      "  };",
      "}"
    ]

intCaseProgram :: String
intCaseProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Int = case Succ Zero of {",
      "    Zero -> 0;",
      "    Succ n -> 1",
      "  };",
      "}"
    ]

functionCaseProgram :: String
functionCaseProgram =
  unlines
    [ "module Main export (Box(..), main) {",
      "  data Box =",
      "      Box : Int -> Box;",
      "",
      "  def main : Int -> Int = case Box 0 of {",
      "    Box n -> \\x x",
      "  };",
      "}"
    ]

parameterizedConstructorProgram :: String
parameterizedConstructorProgram =
  unlines
    [ "module Main export (Option(..), main) {",
      "  data Option a =",
      "      None : Option a",
      "    | Some : a -> Option a;",
      "",
      "  def main : Option Int = Some 1;",
      "}"
    ]

hiddenEqEvidenceProgram :: String
hiddenEqEvidenceProgram =
  unlines
    [ "module Main export (Eq, Nat(..), eq, same, main) {",
      "  class Eq a {",
      "    eq : a -> a -> Bool;",
      "  }",
      "",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat",
      "    deriving Eq;",
      "",
      "  def same : Eq a => a -> a -> Bool = \\x \\y eq x y;",
      "  def main : Bool = same Zero Zero;",
      "}"
    ]

parameterizedEqEvidenceProgram :: String
parameterizedEqEvidenceProgram =
  unlines
    [ "module Main export (Eq, Nat(..), Option(..), eq, main) {",
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
      "    | Some : a -> Option a;",
      "",
      "  instance Eq a => Eq (Option a) {",
      "    eq = \\left \\right case left of {",
      "      None -> case right of {",
      "        None -> true;",
      "        Some _ -> false",
      "      };",
      "      Some l -> case right of {",
      "        None -> false;",
      "        Some r -> eq l r",
      "      }",
      "    };",
      "  }",
      "",
      "  def main : Bool = eq (Some (Some Zero)) (Some (Some Zero));",
      "}"
    ]

recursiveListDerivingEqProgram :: String
recursiveListDerivingEqProgram =
  unlines
    [ "module Main export (Eq, Nat(..), List(..), eq, main) {",
      "  class Eq a {",
      "    eq : a -> a -> Bool;",
      "  }",
      "",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat",
      "    deriving Eq;",
      "",
      "  data List a =",
      "      Nil : List a",
      "    | Cons : a -> List a -> List a",
      "    deriving Eq;",
      "",
      "  def main : Bool = eq (Cons Zero Nil) (Cons Zero Nil);",
      "}"
    ]

constructorForallApplicationProgram :: String
constructorForallApplicationProgram =
  unlines
    [ "module Main export (Pack(..), main) {",
      "  data Pack =",
      "      Pack : forall a. a -> Pack;",
      "",
      "  def main : Pack = Pack 1;",
      "}"
    ]

boundedConstructorForallProgram :: String
boundedConstructorForallProgram =
  unlines
    [ "module Main export (Pack(..), main) {",
      "  data Pack =",
      "      Pack : forall (a >= Int). a -> Pack;",
      "",
      "  def main : Pack = Pack 1;",
      "}"
    ]

dependentBoundedConstructorForallProgram :: String
dependentBoundedConstructorForallProgram =
  unlines
    [ "module Main export (Pack(..), main) {",
      "  data Pack =",
      "      Pack : forall (a >= Int -> Int). a -> Pack;",
      "",
      "  def id : Int -> Int = \\x x;",
      "  def main : Pack = Pack id;",
      "}"
    ]

repeatedPolymorphicParameterProgram :: String
repeatedPolymorphicParameterProgram =
  unlines
    [ "module Main export (Pair(..), main) {",
      "  data Pair a =",
      "      Pair : a -> a -> Pair a;",
      "",
      "  def main : Bool = true;",
      "}"
    ]

duplicateDataNameProgram :: String
duplicateDataNameProgram =
  unlines
    [ "module A export (T(..), make) {",
      "  data T =",
      "      A : T;",
      "",
      "  def make : T = A;",
      "}",
      "",
      "module B export (T(..)) {",
      "  data T =",
      "      B : Int -> T;",
      "}",
      "",
      "module Main export (main) {",
      "  import A as A;",
      "",
      "  def main : Bool = case A.make of {",
      "    A.A -> true",
      "  };",
      "}"
    ]

vacuousRecursiveConstructorFallbackProgram :: String
vacuousRecursiveConstructorFallbackProgram =
  unlines
    [ "module Main export (Box(..), main) {",
      "  data Box = MkBox : Box;",
      "",
      "  def main : Box = MkBox;",
      "}"
    ]

qualifiedAliasOrderingProgram :: String
qualifiedAliasOrderingProgram =
  unlines
    [ "module Y export (T(..)) {",
      "  data T =",
      "      YValue : T;",
      "}",
      "",
      "module Z export (T(..), make) {",
      "  data T =",
      "      ZValue : T;",
      "",
      "  def make : T = ZValue;",
      "}",
      "",
      "module Main export (main) {",
      "  import Z as Z;",
      "",
      "  def main : Bool = case Z.make of {",
      "    Z.ZValue -> true",
      "  };",
      "}"
    ]

unsupportedVariableHeadType :: SrcType
unsupportedVariableHeadType =
  STVarApp "f" (STBase "Int" :| [])

requireChecked :: String -> IO CheckedProgram
requireChecked input = do
  program <- requireParsed input
  requireRight (checkProgram program)

requireParsed :: String -> IO Program
requireParsed input =
  case parseRawProgram input of
    Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
    Right program -> pure program

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
  case result of
    Left err -> expectationFailure (show err) >> fail "unexpected Left"
    Right value -> pure value

backendIRGolden :: FilePath -> BackendProgram -> Expectation
backendIRGolden goldenPath backend = do
  validateBackendProgram backend `shouldBe` Right ()
  goldenText goldenPath (renderBackendIRSnapshot backend)

goldenText :: FilePath -> String -> Expectation
goldenText goldenPath actual = do
  accept <- lookupEnv "GOLDEN_ACCEPT"
  case accept of
    Just "1" -> do
      createDirectoryIfMissing True (takeDirectory goldenPath)
      writeFile goldenPath actual
    _ -> do
      expected <- readFile goldenPath
      length expected `seq` actual `shouldBe` expected

renderBackendIRSnapshot :: BackendProgram -> String
renderBackendIRSnapshot backend =
  unlines $
    [ "backend-program",
      "  main: " ++ backendProgramMain backend,
      "  modules:"
    ]
      ++ concatMap renderBackendIRModule (backendProgramModules backend)

renderBackendIRModule :: BackendModule -> [String]
renderBackendIRModule backendModule =
  [ indent 4 ("module " ++ backendModuleName backendModule),
    indent 6 "data:"
  ]
    ++ renderListOrEmpty 8 renderBackendIRData (backendModuleData backendModule)
    ++ [indent 6 "bindings:"]
    ++ renderListOrEmpty 8 renderBackendIRBinding (backendModuleBindings backendModule)

renderBackendIRData :: BackendData -> [String]
renderBackendIRData backendData =
  [ indent 8 ("data " ++ backendDataName backendData ++ renderPlainTypeParameters (backendDataParameters backendData)),
    indent 10 "constructors:"
  ]
    ++ renderListOrEmpty 12 renderBackendIRConstructor (backendDataConstructors backendData)

renderBackendIRConstructor :: BackendConstructor -> [String]
renderBackendIRConstructor constructor =
  [ indent 12 ("ctor " ++ backendConstructorName constructor ++ renderBackendTypeBinders (backendConstructorForalls constructor)),
    indent 14 ("fields: " ++ renderTypeList (backendConstructorFields constructor)),
    indent 14 ("result: " ++ renderBackendIRType (backendConstructorResult constructor))
  ]

renderBackendIRBinding :: BackendBinding -> [String]
renderBackendIRBinding binding =
  [ indent 8 ("binding " ++ backendBindingName binding ++ " : " ++ renderBackendIRType (backendBindingType binding)),
    indent 10 ("exported-main: " ++ renderBool (backendBindingExportedAsMain binding)),
    indent 10 "expr:"
  ]
    ++ renderBackendIRExpr 12 (backendBindingExpr binding)

renderBackendIRExpr :: Int -> BackendExpr -> [String]
renderBackendIRExpr level expr =
  case expr of
    BackendVar resultTy name ->
      [indent level ("var " ++ name ++ " : " ++ renderBackendIRType resultTy)]
    BackendLit resultTy lit ->
      [indent level ("lit " ++ renderLit lit ++ " : " ++ renderBackendIRType resultTy)]
    BackendLam resultTy name paramTy body ->
      [ indent level ("lam " ++ name ++ " : " ++ renderBackendIRType paramTy ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "body:"
      ]
        ++ renderBackendIRExpr (level + 4) body
    BackendApp resultTy fun arg ->
      [ indent level ("app : " ++ renderBackendIRType resultTy),
        indent (level + 2) "function:"
      ]
        ++ renderBackendIRExpr (level + 4) fun
        ++ [indent (level + 2) "argument:"]
        ++ renderBackendIRExpr (level + 4) arg
    BackendLet resultTy name bindingTy rhs body ->
      [ indent level ("let " ++ name ++ " : " ++ renderBackendIRType bindingTy ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "rhs:"
      ]
        ++ renderBackendIRExpr (level + 4) rhs
        ++ [indent (level + 2) "body:"]
        ++ renderBackendIRExpr (level + 4) body
    BackendTyAbs resultTy name mbBound body ->
      [ indent level ("type-lam " ++ renderTypeBinder name mbBound ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "body:"
      ]
        ++ renderBackendIRExpr (level + 4) body
    BackendTyApp resultTy fun tyArg ->
      [ indent level ("type-app [" ++ renderBackendIRType tyArg ++ "] : " ++ renderBackendIRType resultTy),
        indent (level + 2) "function:"
      ]
        ++ renderBackendIRExpr (level + 4) fun
    BackendConstruct resultTy name args ->
      [ indent level ("construct " ++ name ++ " : " ++ renderBackendIRType resultTy),
        indent (level + 2) "args:"
      ]
        ++ renderExprList (level + 4) args
    BackendCase resultTy scrutinee alternatives ->
      [ indent level ("case : " ++ renderBackendIRType resultTy),
        indent (level + 2) "scrutinee:"
      ]
        ++ renderBackendIRExpr (level + 4) scrutinee
        ++ [indent (level + 2) "alternatives:"]
        ++ concatMap (renderBackendIRAlternative (level + 4)) (toList alternatives)
    BackendRoll resultTy payload ->
      [ indent level ("roll : " ++ renderBackendIRType resultTy),
        indent (level + 2) "payload:"
      ]
        ++ renderBackendIRExpr (level + 4) payload
    BackendUnroll resultTy payload ->
      [ indent level ("unroll : " ++ renderBackendIRType resultTy),
        indent (level + 2) "payload:"
      ]
        ++ renderBackendIRExpr (level + 4) payload

renderBackendIRAlternative :: Int -> BackendAlternative -> [String]
renderBackendIRAlternative level alternative =
  [ indent level ("alternative " ++ renderBackendIRPattern (backendAltPattern alternative)),
    indent (level + 2) "body:"
  ]
    ++ renderBackendIRExpr (level + 4) (backendAltBody alternative)

renderBackendIRPattern :: BackendPattern -> String
renderBackendIRPattern pattern0 =
  case pattern0 of
    BackendDefaultPattern ->
      "default"
    BackendConstructorPattern name binders ->
      name ++ "(" ++ intercalate ", " binders ++ ")"

renderExprList :: Int -> [BackendExpr] -> [String]
renderExprList level exprs =
  renderListOrEmpty level renderArg (zip [0 :: Int ..] exprs)
  where
    renderArg (ix, expr) =
      indent level ("arg " ++ show ix ++ ":") : renderBackendIRExpr (level + 2) expr

renderBackendIRType :: BackendType -> String
renderBackendIRType backendTy =
  case backendTy of
    BTVar name -> "$" ++ name
    BTArrow dom cod -> "(" ++ renderBackendIRType dom ++ " -> " ++ renderBackendIRType cod ++ ")"
    BTBase (BaseTy name) -> name
    BTCon (BaseTy name) args -> name ++ "<" ++ intercalate ", " (map renderBackendIRType (toList args)) ++ ">"
    BTForall name mbBound body -> "forall " ++ renderTypeBinder name mbBound ++ ". " ++ renderBackendIRType body
    BTMu name body -> "mu " ++ name ++ ". " ++ renderBackendIRType body
    BTBottom -> "bottom"

renderBackendTypeBinders :: [BackendTypeBinder] -> String
renderBackendTypeBinders binders =
  case binders of
    [] -> ""
    _ -> "<" ++ intercalate ", " [renderTypeBinder name mbBound | BackendTypeBinder name mbBound <- binders] ++ ">"

renderTypeBinder :: String -> Maybe BackendType -> String
renderTypeBinder name mbBound =
  "$" ++ name ++ maybe "" ((" >= " ++) . renderBackendIRType) mbBound

renderPlainTypeParameters :: [String] -> String
renderPlainTypeParameters params =
  case params of
    [] -> ""
    _ -> "<" ++ intercalate ", " (map ("$" ++) params) ++ ">"

renderTypeList :: [BackendType] -> String
renderTypeList types0 =
  "[" ++ intercalate ", " (map renderBackendIRType types0) ++ "]"

renderLit :: Lit -> String
renderLit lit =
  case lit of
    LInt n -> show n
    LBool True -> "true"
    LBool False -> "false"
    LString value -> show value

renderBool :: Bool -> String
renderBool value =
  case value of
    True -> "true"
    False -> "false"

renderListOrEmpty :: Int -> (a -> [String]) -> [a] -> [String]
renderListOrEmpty level render items =
  case items of
    [] -> [indent level "<none>"]
    _ -> concatMap render items

indent :: Int -> String -> String
indent level line =
  replicate level ' ' ++ line

requireBinding :: String -> BackendProgram -> IO BackendBinding
requireBinding name backend =
  case find ((== name) . backendBindingName) (backendBindings backend) of
    Just binding -> pure binding
    Nothing -> expectationFailure ("missing backend binding " ++ show name) >> fail "missing binding"

requireSingleLiftedHelper :: BackendProgram -> IO BackendBinding
requireSingleLiftedHelper backend =
  case filter (isInfixOf "$letrec$" . backendBindingName) (backendBindings backend) of
    [helper] -> pure helper
    helpers -> expectationFailure ("expected one lifted helper, got " ++ show (map backendBindingName helpers)) >> fail "helper mismatch"

requireConstructor :: String -> BackendProgram -> IO BackendConstructor
requireConstructor name backend =
  case find ((== name) . backendConstructorName) (backendConstructors backend) of
    Just constructor -> pure constructor
    Nothing -> expectationFailure ("missing backend constructor " ++ show name) >> fail "missing constructor"

backendBindings :: BackendProgram -> [BackendBinding]
backendBindings =
  concatMap backendModuleBindings . backendProgramModules

backendConstructors :: BackendProgram -> [BackendConstructor]
backendConstructors =
  concatMap (concatMap backendDataConstructors . backendModuleData) . backendProgramModules

backendDataNames :: BackendProgram -> [String]
backendDataNames =
  concatMap (map backendDataName . backendModuleData) . backendProgramModules

backendConstructorNames :: BackendProgram -> [String]
backendConstructorNames =
  concatMap (concatMap (map backendConstructorName . backendDataConstructors) . backendModuleData) . backendProgramModules

containsBackendCase :: BackendExpr -> Bool
containsBackendCase expr =
  case expr of
    BackendCase {} -> True
    BackendLam {backendBody = body} -> containsBackendCase body
    BackendApp {backendFunction = fun, backendArgument = arg} -> containsBackendCase fun || containsBackendCase arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} -> containsBackendCase rhs || containsBackendCase body
    BackendTyAbs {backendTyAbsBody = body} -> containsBackendCase body
    BackendTyApp {backendTyFunction = fun} -> containsBackendCase fun
    BackendConstruct {backendConstructArgs = args} -> any containsBackendCase args
    BackendRoll {backendRollPayload = body} -> containsBackendCase body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendCase body
    _ -> False

containsBackendTyApp :: BackendExpr -> Bool
containsBackendTyApp expr =
  case expr of
    BackendTyApp {} -> True
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendTyApp scrutinee || any (containsBackendTyApp . backendAltBody) (toList alternatives)
    BackendLam {backendBody = body} -> containsBackendTyApp body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendTyApp fun || containsBackendTyApp arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendTyApp rhs || containsBackendTyApp body
    BackendTyAbs {backendTyAbsBody = body} -> containsBackendTyApp body
    BackendConstruct {backendConstructArgs = args} -> any containsBackendTyApp args
    BackendRoll {backendRollPayload = body} -> containsBackendTyApp body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendTyApp body
    _ -> False

containsBackendTyAppArgument :: BackendType -> BackendExpr -> Bool
containsBackendTyAppArgument expected expr =
  case expr of
    BackendTyApp {backendTyArgument = ty, backendTyFunction = fun} ->
      ty == expected || containsBackendTyAppArgument expected fun
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendTyAppArgument expected scrutinee || any (containsBackendTyAppArgument expected . backendAltBody) (toList alternatives)
    BackendLam {backendBody = body} -> containsBackendTyAppArgument expected body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendTyAppArgument expected fun || containsBackendTyAppArgument expected arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendTyAppArgument expected rhs || containsBackendTyAppArgument expected body
    BackendTyAbs {backendTyAbsBody = body} -> containsBackendTyAppArgument expected body
    BackendConstruct {backendConstructArgs = args} -> any (containsBackendTyAppArgument expected) args
    BackendRoll {backendRollPayload = body} -> containsBackendTyAppArgument expected body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendTyAppArgument expected body
    _ -> False

containsFreshenedTypeAbsWithOuterBound :: BackendExpr -> Bool
containsFreshenedTypeAbsWithOuterBound expr =
  case expr of
    BackendTyAbs {backendTyParamName = name, backendTyParamBound = Just (BTArrow (BTVar boundDom) (BTVar boundCod)), backendTyAbsBody = body} ->
      (name /= "a" && boundDom == "a" && boundCod == "a") || containsFreshenedTypeAbsWithOuterBound body
    BackendTyAbs {backendTyAbsBody = body} ->
      containsFreshenedTypeAbsWithOuterBound body
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsFreshenedTypeAbsWithOuterBound scrutinee || any (containsFreshenedTypeAbsWithOuterBound . backendAltBody) (toList alternatives)
    BackendLam {backendBody = body} -> containsFreshenedTypeAbsWithOuterBound body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsFreshenedTypeAbsWithOuterBound fun || containsFreshenedTypeAbsWithOuterBound arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} ->
      containsFreshenedTypeAbsWithOuterBound rhs || containsFreshenedTypeAbsWithOuterBound body
    BackendTyApp {backendTyFunction = fun} -> containsFreshenedTypeAbsWithOuterBound fun
    BackendConstruct {backendConstructArgs = args} -> any containsFreshenedTypeAbsWithOuterBound args
    BackendRoll {backendRollPayload = body} -> containsFreshenedTypeAbsWithOuterBound body
    BackendUnroll {backendUnrollPayload = body} -> containsFreshenedTypeAbsWithOuterBound body
    _ -> False

containsBackendVar :: String -> BackendExpr -> Bool
containsBackendVar expected expr =
  case expr of
    BackendVar {backendVarName = name} -> name == expected
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendVar expected scrutinee || any (containsBackendVar expected . backendAltBody) (toList alternatives)
    BackendLam {backendParamName = name, backendBody = body}
      | name == expected -> False
      | otherwise -> containsBackendVar expected body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendVar expected fun || containsBackendVar expected arg
    BackendLet {backendLetName = name, backendLetRhs = rhs, backendLetBody = body}
      | name == expected -> containsBackendVar expected rhs
      | otherwise -> containsBackendVar expected rhs || containsBackendVar expected body
    BackendTyAbs {backendTyAbsBody = body} -> containsBackendVar expected body
    BackendTyApp {backendTyFunction = fun} -> containsBackendVar expected fun
    BackendConstruct {backendConstructArgs = args} -> any (containsBackendVar expected) args
    BackendRoll {backendRollPayload = body} -> containsBackendVar expected body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendVar expected body
    _ -> False

backendExprBinders :: BackendExpr -> [String]
backendExprBinders expr =
  case expr of
    BackendVar {} -> []
    BackendLit {} -> []
    BackendLam {backendParamName = name, backendBody = body} -> name : backendExprBinders body
    BackendApp {backendFunction = fun, backendArgument = arg} -> backendExprBinders fun ++ backendExprBinders arg
    BackendLet {backendLetName = name, backendLetRhs = rhs, backendLetBody = body} -> name : backendExprBinders rhs ++ backendExprBinders body
    BackendTyAbs {backendTyAbsBody = body} -> backendExprBinders body
    BackendTyApp {backendTyFunction = fun} -> backendExprBinders fun
    BackendConstruct {backendConstructArgs = args} -> concatMap backendExprBinders args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      backendExprBinders scrutinee ++ concatMap alternativeBinders (toList alternatives)
    BackendRoll {backendRollPayload = body} -> backendExprBinders body
    BackendUnroll {backendUnrollPayload = body} -> backendExprBinders body
  where
    alternativeBinders (BackendAlternative pattern0 body) =
      patternBackendBinders pattern0 ++ backendExprBinders body

    patternBackendBinders BackendDefaultPattern = []
    patternBackendBinders (BackendConstructorPattern _ names) = names

containsSelfReferentialLetRhs :: BackendExpr -> Bool
containsSelfReferentialLetRhs expr =
  case expr of
    BackendVar {} -> False
    BackendLit {} -> False
    BackendLam {backendBody = body} -> containsSelfReferentialLetRhs body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsSelfReferentialLetRhs fun || containsSelfReferentialLetRhs arg
    BackendLet {backendLetName = name, backendLetRhs = rhs, backendLetBody = body} ->
      rhsIsSelfReference name rhs || containsSelfReferentialLetRhs rhs || containsSelfReferentialLetRhs body
    BackendTyAbs {backendTyAbsBody = body} -> containsSelfReferentialLetRhs body
    BackendTyApp {backendTyFunction = fun} -> containsSelfReferentialLetRhs fun
    BackendConstruct {backendConstructArgs = args} -> any containsSelfReferentialLetRhs args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsSelfReferentialLetRhs scrutinee || any (containsSelfReferentialLetRhs . backendAltBody) (toList alternatives)
    BackendRoll {backendRollPayload = body} -> containsSelfReferentialLetRhs body
    BackendUnroll {backendUnrollPayload = body} -> containsSelfReferentialLetRhs body
  where
    rhsIsSelfReference name rhs =
      case rhs of
        BackendVar {backendVarName = rhsName} -> rhsName == name
        _ -> False

isBackendFunctionType :: BackendType -> Bool
isBackendFunctionType ty =
  case ty of
    BTArrow {} -> True
    _ -> False

containsConstructArgType :: String -> BackendType -> BackendExpr -> Bool
containsConstructArgType constructorName argTy expr =
  case expr of
    BackendConstruct {backendConstructName = name, backendConstructArgs = args} ->
      (name == constructorName && any (alphaEqBackendType argTy . backendExprType) args)
        || any (containsConstructArgType constructorName argTy) args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsConstructArgType constructorName argTy scrutinee
        || any (containsConstructArgType constructorName argTy . backendAltBody) (toList alternatives)
    BackendLam {backendBody = body} -> containsConstructArgType constructorName argTy body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsConstructArgType constructorName argTy fun || containsConstructArgType constructorName argTy arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} ->
      containsConstructArgType constructorName argTy rhs || containsConstructArgType constructorName argTy body
    BackendTyAbs {backendTyAbsBody = body} -> containsConstructArgType constructorName argTy body
    BackendTyApp {backendTyFunction = fun} -> containsConstructArgType constructorName argTy fun
    BackendRoll {backendRollPayload = body} -> containsConstructArgType constructorName argTy body
    BackendUnroll {backendUnrollPayload = body} -> containsConstructArgType constructorName argTy body
    _ -> False

findBackendCase :: BackendExpr -> Maybe BackendExpr
findBackendCase expr =
  case expr of
    BackendCase {} -> Just expr
    BackendLam {backendBody = body} -> findBackendCase body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      findBackendCase fun <|> findBackendCase arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} ->
      findBackendCase rhs <|> findBackendCase body
    BackendTyAbs {backendTyAbsBody = body} -> findBackendCase body
    BackendTyApp {backendTyFunction = fun} -> findBackendCase fun
    BackendConstruct {backendConstructArgs = args} -> firstJust (map findBackendCase args)
    BackendRoll {backendRollPayload = body} -> findBackendCase body
    BackendUnroll {backendUnrollPayload = body} -> findBackendCase body
    _ -> Nothing

collectConstructNames :: BackendExpr -> [String]
collectConstructNames expr =
  case expr of
    BackendConstruct {backendConstructName = name, backendConstructArgs = args} ->
      name : concatMap collectConstructNames args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      collectConstructNames scrutinee ++ concatMap (collectConstructNames . backendAltBody) (toList alternatives)
    BackendLam {backendBody = body} -> collectConstructNames body
    BackendApp {backendFunction = fun, backendArgument = arg} -> collectConstructNames fun ++ collectConstructNames arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} -> collectConstructNames rhs ++ collectConstructNames body
    BackendTyAbs {backendTyAbsBody = body} -> collectConstructNames body
    BackendTyApp {backendTyFunction = fun} -> collectConstructNames fun
    BackendRoll {backendRollPayload = body} -> collectConstructNames body
    BackendUnroll {backendUnrollPayload = body} -> collectConstructNames body
    _ -> []

intTy :: BackendType
intTy =
  BTBase (BaseTy "Int")

boolTy :: BackendType
boolTy =
  BTBase (BaseTy "Bool")

unaryIntBackendTy :: BackendType
unaryIntBackendTy =
  BTArrow intTy intTy

intElabTy :: Elab.ElabType
intElabTy =
  Elab.TBase (BaseTy "Int")

boolElabTy :: Elab.ElabType
boolElabTy =
  Elab.TBase (BaseTy "Bool")

unaryIntElabTy :: Elab.ElabType
unaryIntElabTy =
  Elab.TArrow intElabTy intElabTy

recursiveCaptureAvoidingElabTy :: Elab.ElabType
recursiveCaptureAvoidingElabTy =
  Elab.TArrow unaryIntElabTy intElabTy

recursiveCaptureAvoidingTerm :: Elab.ElabTerm
recursiveCaptureAvoidingTerm =
  Elab.ELam
    "$evidence_E"
    unaryIntElabTy
    ( Elab.ELet
        "loop"
        (Elab.schemeFromType unaryIntElabTy)
        recursiveCaptureAvoidingRhs
        (Elab.EApp (Elab.EVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveCaptureAvoidingRhs :: Elab.ElabTerm
recursiveCaptureAvoidingRhs =
  Elab.ELam
    "n"
    intElabTy
    ( Elab.ELet
        "before"
        (Elab.schemeFromType intElabTy)
        (Elab.EApp (Elab.EVar "$evidence_E") (Elab.EVar "n"))
        ( Elab.ELet
            "$evidence_E"
            (Elab.schemeFromType unaryIntElabTy)
            intIdentityElabTerm
            (Elab.EApp (Elab.EVar "loop") (Elab.EVar "before"))
        )
    )

recursiveLetRhsRenameElabTy :: Elab.ElabType
recursiveLetRhsRenameElabTy =
  Elab.TArrow unaryIntElabTy intElabTy

recursiveLetRhsRenameTerm :: Elab.ElabTerm
recursiveLetRhsRenameTerm =
  Elab.ELam
    "$evidence_E"
    unaryIntElabTy
    ( Elab.ELet
        "loop"
        (Elab.schemeFromType unaryIntElabTy)
        recursiveLetRhsRenameRhs
        (Elab.EApp (Elab.EVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveLetRhsRenameRhs :: Elab.ElabTerm
recursiveLetRhsRenameRhs =
  Elab.ELam
    "n"
    intElabTy
    ( Elab.ELet
        "before"
        (Elab.schemeFromType intElabTy)
        (Elab.EApp (Elab.EVar "$evidence_E") (Elab.EVar "n"))
        ( Elab.ELet
            "$evidence_E"
            (Elab.schemeFromType unaryIntElabTy)
            (Elab.EVar "$evidence_E")
            (Elab.EApp (Elab.EVar "loop") (Elab.EVar "before"))
        )
    )

recursiveTypeCaptureElabTy :: Elab.ElabType
recursiveTypeCaptureElabTy =
  Elab.TForall "a" Nothing intElabTy

recursiveTypeCaptureTerm :: Elab.ElabTerm
recursiveTypeCaptureTerm =
  Elab.ETyAbs
    "a"
    Nothing
    ( Elab.ELet
        "loop"
        (Elab.schemeFromType unaryIntElabTy)
        recursiveTypeCaptureRhs
        (Elab.EApp (Elab.EVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveTypeCaptureRhs :: Elab.ElabTerm
recursiveTypeCaptureRhs =
  Elab.ELam
    "n"
    intElabTy
    ( Elab.ELet
        "ignored"
        (Elab.schemeFromType intElabTy)
        recursiveTypeOnlyInstantiation
        (Elab.EApp (Elab.EVar "loop") (Elab.EVar "n"))
    )

recursiveTypeOnlyInstantiation :: Elab.ElabTerm
recursiveTypeOnlyInstantiation =
  Elab.ETyInst
    (Elab.ETyAbs "b" Nothing (Elab.ELit (LInt 0)))
    (Elab.InstApp (Elab.TVar "a"))

recursiveTypeBoundScopeElabTy :: Elab.ElabType
recursiveTypeBoundScopeElabTy =
  Elab.TForall "a" Nothing intElabTy

recursiveTypeBoundScopeTerm :: Elab.ElabTerm
recursiveTypeBoundScopeTerm =
  Elab.ETyAbs
    "a"
    Nothing
    ( Elab.ELet
        "loop"
        (Elab.schemeFromType unaryIntElabTy)
        recursiveTypeBoundScopeRhs
        (Elab.EApp (Elab.EVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveTypeBoundScopeRhs :: Elab.ElabTerm
recursiveTypeBoundScopeRhs =
  Elab.ELam
    "n"
    intElabTy
    ( Elab.ETyInst
        ( Elab.ETyAbs
            "a"
            (Just (dependentArrowElabBoundTy (Elab.TVar "a")))
            (Elab.EApp (Elab.EVar "loop") (Elab.EVar "n"))
        )
        (Elab.InstApp (dependentArrowElabTy (Elab.TVar "a")))
    )

recursiveShadowedLetElabTy :: Elab.ElabType
recursiveShadowedLetElabTy =
  intElabTy

recursiveShadowedLetTerm :: Elab.ElabTerm
recursiveShadowedLetTerm =
  Elab.ELet
    "f"
    (Elab.schemeFromType unaryIntElabTy)
    intIdentityElabTerm
    ( Elab.ELet
        "f"
        (Elab.schemeFromType unaryIntElabTy)
        ( Elab.ELam
            "n"
            intElabTy
            (Elab.EApp (Elab.EVar "f") (Elab.EVar "n"))
        )
        (Elab.EApp (Elab.EVar "f") (Elab.ELit (LInt 0)))
    )

recursiveLexicalTypeOrderElabTy :: Elab.ElabType
recursiveLexicalTypeOrderElabTy =
  Elab.TForall
    "z"
    Nothing
    ( Elab.TForall
        "a"
        Nothing
        intElabTy
    )

recursiveLexicalTypeOrderTerm :: Elab.ElabTerm
recursiveLexicalTypeOrderTerm =
  Elab.ETyAbs
    "z"
    Nothing
    ( Elab.ETyAbs
        "a"
        Nothing
        ( Elab.ELet
            "loop"
            (Elab.schemeFromType unaryIntElabTy)
            recursiveLexicalTypeOrderRhs
            (Elab.EApp (Elab.EVar "loop") (Elab.ELit (LInt 0)))
        )
    )

recursiveLexicalTypeOrderRhs :: Elab.ElabTerm
recursiveLexicalTypeOrderRhs =
  Elab.ELam
    "n"
    intElabTy
    ( Elab.ELet
        "captureA"
        (Elab.schemeFromType intElabTy)
        ( Elab.ETyInst
            (Elab.ETyAbs "b" Nothing (Elab.ELit (LInt 0)))
            (Elab.InstApp (Elab.TVar "a"))
        )
        ( Elab.ELet
            "captureZ"
            (Elab.schemeFromType intElabTy)
            ( Elab.ETyInst
                (Elab.ETyAbs "b" Nothing (Elab.ELit (LInt 0)))
                (Elab.InstApp (Elab.TVar "z"))
            )
            (Elab.EApp (Elab.EVar "loop") (Elab.EVar "n"))
        )
    )

recursiveLexicalTypeOrderHelperBackendTy :: BackendType
recursiveLexicalTypeOrderHelperBackendTy =
  BTForall
    "z"
    Nothing
    ( BTForall
        "a"
        Nothing
        unaryIntBackendTy
    )

intIdentityElabTerm :: Elab.ElabTerm
intIdentityElabTerm =
  Elab.ELam "m" intElabTy (Elab.EVar "m")

intElabBoundTy :: Elab.BoundType
intElabBoundTy =
  Elab.TBase (BaseTy "Int")

boundedWrapElabTy :: Elab.ElabType -> Elab.ElabType
boundedWrapElabTy resultTy =
  Elab.TForall "b" (Just intElabBoundTy) (Elab.TArrow (Elab.TVar "b") resultTy)

boundedWrapTerm :: Elab.ElabTerm
boundedWrapTerm =
  Elab.ETyAbs
    "b"
    (Just intElabBoundTy)
    ( Elab.ELam
        "x"
        (Elab.TVar "b")
        (Elab.EApp (Elab.EVar "Main__Pack") (Elab.EVar "x"))
    )

dependentBoundedWrapElabTy :: Elab.ElabType -> Elab.ElabType
dependentBoundedWrapElabTy resultTy =
  Elab.TForall
    "z"
    (Just intElabBoundTy)
    ( Elab.TForall
        "b"
        (Just (dependentArrowElabBoundTy (Elab.TVar "z")))
        (Elab.TArrow (Elab.TVar "b") resultTy)
    )

dependentBoundedWrapTerm :: Elab.ElabTerm
dependentBoundedWrapTerm =
  Elab.ETyAbs
    "z"
    (Just intElabBoundTy)
    ( Elab.ETyAbs
        "b"
        (Just (dependentArrowElabBoundTy (Elab.TVar "z")))
        ( Elab.ELam
            "x"
            (Elab.TVar "b")
            (Elab.EApp (Elab.EVar "Main__Pack") (Elab.EVar "x"))
        )
    )

dependentArrowElabBoundTy :: Elab.ElabType -> Elab.BoundType
dependentArrowElabBoundTy ty =
  Elab.TArrow ty ty

dependentArrowElabTy :: Elab.ElabType -> Elab.ElabType
dependentArrowElabTy ty =
  Elab.TArrow ty ty

polymorphicIdentityElabTy :: Elab.ElabType
polymorphicIdentityElabTy =
  Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))

alphaEquivalentIdentityElabTy :: Elab.ElabType
alphaEquivalentIdentityElabTy =
  Elab.TForall "b" Nothing (Elab.TArrow (Elab.TVar "b") (Elab.TVar "b"))

repeatedPolymorphicParameterCaseTerm :: Elab.ElabTerm
repeatedPolymorphicParameterCaseTerm =
  Elab.EApp
    (Elab.ETyInst (Elab.EUnroll pairScrutinee) (Elab.InstApp boolElabTy))
    ( Elab.ELam
        "$pair_f"
        polymorphicIdentityElabTy
        (Elab.ELam "$pair_g" alphaEquivalentIdentityElabTy (Elab.ELit (LBool True)))
    )
  where
    pairScrutinee =
      Elab.EApp
        (Elab.EApp (Elab.EVar "Main__Pair") polymorphicIdentityTerm)
        (Elab.ETyInst alphaEquivalentIdentityTerm Elab.InstId)

polymorphicIdentityTerm :: Elab.ElabTerm
polymorphicIdentityTerm =
  Elab.ETyAbs
    "a"
    Nothing
    (Elab.ELam "$poly_id_a" (Elab.TVar "a") (Elab.EVar "$poly_id_a"))

alphaEquivalentIdentityTerm :: Elab.ElabTerm
alphaEquivalentIdentityTerm =
  Elab.ETyAbs
    "b"
    Nothing
    (Elab.ELam "$poly_id_b" (Elab.TVar "b") (Elab.EVar "$poly_id_b"))

mapMainBinding :: (CheckedBinding -> CheckedBinding) -> CheckedProgram -> CheckedProgram
mapMainBinding f checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleBindings =
            map updateBinding (checkedModuleBindings checkedModule)
        }

    updateBinding binding
      | checkedBindingName binding == checkedProgramMain checked = f binding
      | otherwise = binding

withConstructorResult :: String -> SrcType -> CheckedProgram -> CheckedProgram
withConstructorResult runtimeName resultTy checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleData = fmap updateDataInfo (checkedModuleData checkedModule)
        }

    updateDataInfo dataInfo =
      dataInfo {dataConstructors = map updateConstructorInfo (dataConstructors dataInfo)}

    updateConstructorInfo constructorInfo
      | ctorRuntimeName constructorInfo == runtimeName =
          constructorInfo {ctorResult = resultTy}
      | otherwise =
          constructorInfo

mapBackendMainBinding :: (BackendBinding -> BackendBinding) -> BackendProgram -> BackendProgram
mapBackendMainBinding f backend =
  backend
    { backendProgramModules =
        map updateModule (backendProgramModules backend)
    }
  where
    updateModule backendModule =
      backendModule
        { backendModuleBindings =
            map updateBinding (backendModuleBindings backendModule)
        }

    updateBinding binding
      | backendBindingName binding == backendProgramMain backend = f binding
      | otherwise = binding

addStaleConstructorHeadInstantiation :: String -> Elab.ElabType -> Elab.ElabTerm -> Elab.ElabTerm
addStaleConstructorHeadInstantiation target staleTy =
  go
  where
    go term =
      case collectAppsElab term of
        (headTerm, args)
          | isTargetConstructorHead headTerm ->
              rebuildAppsElab (Elab.ETyInst headTerm (Elab.InstApp staleTy)) args
        _ ->
          case term of
            Elab.ELam name ty body ->
              Elab.ELam name ty (go body)
            Elab.EApp fun arg ->
              Elab.EApp (go fun) (go arg)
            Elab.ELet name scheme rhs body ->
              Elab.ELet name scheme (go rhs) (go body)
            Elab.ETyAbs name mbBound body ->
              Elab.ETyAbs name mbBound (go body)
            Elab.ETyInst inner inst ->
              Elab.ETyInst (go inner) inst
            Elab.ERoll ty body ->
              Elab.ERoll ty (go body)
            Elab.EUnroll body ->
              Elab.EUnroll (go body)
            _ ->
              term

    isTargetConstructorHead headTerm =
      case stripElabTypeInsts headTerm of
        Elab.EVar name -> name == target
        _ -> False

stripElabTypeInsts :: Elab.ElabTerm -> Elab.ElabTerm
stripElabTypeInsts term =
  case term of
    Elab.ETyInst inner _ -> stripElabTypeInsts inner
    other -> other

wrapCaseHandlersWithTypeWrappers :: Elab.ElabTerm -> Elab.ElabTerm
wrapCaseHandlersWithTypeWrappers term =
  case collectAppsElab term of
    (headTerm@(Elab.ETyInst (Elab.EUnroll _) _), handlers@(_ : _)) ->
      rebuildAppsElab headTerm (map wrapHandler handlers)
    _ ->
      case term of
        Elab.ELam name ty body ->
          Elab.ELam name ty (wrapCaseHandlersWithTypeWrappers body)
        Elab.EApp fun arg ->
          Elab.EApp (wrapCaseHandlersWithTypeWrappers fun) (wrapCaseHandlersWithTypeWrappers arg)
        Elab.ELet name scheme rhs body ->
          Elab.ELet name scheme (wrapCaseHandlersWithTypeWrappers rhs) (wrapCaseHandlersWithTypeWrappers body)
        Elab.ETyAbs name mbBound body ->
          Elab.ETyAbs name mbBound (wrapCaseHandlersWithTypeWrappers body)
        Elab.ETyInst inner inst ->
          Elab.ETyInst (wrapCaseHandlersWithTypeWrappers inner) inst
        Elab.ERoll ty body ->
          Elab.ERoll ty (wrapCaseHandlersWithTypeWrappers body)
        Elab.EUnroll body ->
          Elab.EUnroll (wrapCaseHandlersWithTypeWrappers body)
        _ ->
          term
  where
    wrapHandler handler =
      Elab.ETyAbs "$case_handler_a" Nothing (Elab.ETyInst handler (Elab.InstApp intElabTy))

replaceCaseHandlerBodiesAfterLams :: Int -> Elab.ElabTerm -> Elab.ElabTerm -> Elab.ElabTerm
replaceCaseHandlerBodiesAfterLams lamCount replacement term =
  case collectAppsElab term of
    (headTerm@(Elab.ETyInst (Elab.EUnroll _) _), handlers@(_ : _)) ->
      rebuildAppsElab headTerm (map (replaceHandlerBody lamCount) handlers)
    _ ->
      case term of
        Elab.ELam name ty body ->
          Elab.ELam name ty (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.EApp fun arg ->
          Elab.EApp (replaceCaseHandlerBodiesAfterLams lamCount replacement fun) (replaceCaseHandlerBodiesAfterLams lamCount replacement arg)
        Elab.ELet name scheme rhs body ->
          Elab.ELet name scheme (replaceCaseHandlerBodiesAfterLams lamCount replacement rhs) (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.ETyAbs name mbBound body ->
          Elab.ETyAbs name mbBound (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.ETyInst inner inst ->
          Elab.ETyInst (replaceCaseHandlerBodiesAfterLams lamCount replacement inner) inst
        Elab.ERoll ty body ->
          Elab.ERoll ty (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.EUnroll body ->
          Elab.EUnroll (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        _ ->
          term
  where
    replaceHandlerBody remaining handler
      | remaining <= 0 = replacement
      | otherwise =
          case handler of
            Elab.ELam name ty body ->
              Elab.ELam name ty (replaceHandlerBody (remaining - 1) body)
            _ ->
              handler

instantiatedIntIdentity :: Elab.ElabTerm
instantiatedIntIdentity =
  Elab.ETyInst
    ( Elab.ETyAbs
        "$case_body_a"
        Nothing
        (Elab.ELam "$case_body_x" (Elab.TVar "$case_body_a") (Elab.EVar "$case_body_x"))
    )
    (Elab.InstApp intElabTy)

alphaEquivalentIdentityInstId :: Elab.ElabTerm
alphaEquivalentIdentityInstId =
  Elab.ETyInst
    ( Elab.ETyAbs
        "$case_body_b"
        Nothing
        (Elab.ELam "$case_body_y" (Elab.TVar "$case_body_b") (Elab.EVar "$case_body_y"))
    )
    Elab.InstId

collectAppsElab :: Elab.ElabTerm -> (Elab.ElabTerm, [Elab.ElabTerm])
collectAppsElab =
  go []
  where
    go args term =
      case term of
        Elab.EApp fun arg -> go (arg : args) fun
        other -> (other, args)

rebuildAppsElab :: Elab.ElabTerm -> [Elab.ElabTerm] -> Elab.ElabTerm
rebuildAppsElab =
  foldl Elab.EApp

firstJust :: [Maybe a] -> Maybe a
firstJust [] =
  Nothing
firstJust (value : rest) =
  case value of
    Just _ -> value
    Nothing -> firstJust rest
