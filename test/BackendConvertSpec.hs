module BackendConvertSpec (spec) where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import MLF.Backend.Convert
import MLF.Backend.IR
import MLF.Constraint.Types.Graph (BaseTy (..))
import qualified MLF.Elab.Types as Elab
import MLF.Frontend.Syntax (Lit (..), SrcTy (..), SrcType)
import MLF.Program
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

  it "recovers explicit backend constructors and cases from checked ADT paths" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendDataNames backend `shouldSatisfy` (not . null)
    backendConstructorNames backend `shouldSatisfy` (not . null)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase
    collectConstructNames (backendBindingExpr mainBinding) `shouldSatisfy` (not . null)

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

  it "keeps same-name data declarations module-scoped during type lowering" $ do
    checked <- requireChecked duplicateDataNameProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendDataNames backend `shouldContain` ["A.T", "B.T"]

    aBinding <- requireBinding "A__A" backend
    aConstructor <- requireConstructor "A__A" backend
    backendConstructorResult aConstructor `shouldBe` backendBindingType aBinding

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

requireBinding :: String -> BackendProgram -> IO BackendBinding
requireBinding name backend =
  case find ((== name) . backendBindingName) (backendBindings backend) of
    Just binding -> pure binding
    Nothing -> expectationFailure ("missing backend binding " ++ show name) >> fail "missing binding"

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

intElabTy :: Elab.ElabType
intElabTy =
  Elab.TBase (BaseTy "Int")

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

firstJust :: [Maybe a] -> Maybe a
firstJust [] =
  Nothing
firstJust (value : rest) =
  case value of
    Just _ -> value
    Nothing -> firstJust rest
